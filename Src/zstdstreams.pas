{
  Copyright (c) 2026 Aleksandr Vorobev aka CynicRus, CynicRus@gmail.com

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

unit ZStdStreams;

{$IFDEF FPC}
{$MODE DELPHI}
{$PACKRECORDS C}
{$ENDIF}

interface

uses
  SysUtils, Classes, zstd;

type
  EZstdStreamError = class(EStreamError);

  ZCustomStream = class(TStream)
  protected
    FBaseStream: TStream;
    FOwnsStream: Boolean;
  public
    constructor Create(ABaseStream: TStream;
      AOwnsStream: Boolean = False); virtual;
    destructor Destroy; override;
    property BaseStream: TStream read FBaseStream;
  end;

  { Compression stream: writes a zstd stream to BaseStream (one frame) }
  TZCompressionStream = class(ZCustomStream)
  private
    FCtx: PZSTD_CCtx;
    FOutBuf: TBytes;
    FOut: TZSTD_outBuffer;
    FClosed: Boolean;
    procedure RaiseOnZstdError(Code: TZSTDSize; const Where: string);
    procedure FlushOutBuf;
    procedure CompressStep(const InPtr: Pointer; InSize: TZSTDSize;
      EndOp: ZSTD_EndDirective);
  public
    constructor Create(ABaseStream: TStream; CompressionLevel: Integer = 3;
      AOwnsStream: Boolean = False; WriteContentSize: Boolean = True;
      WriteChecksum: Boolean = False); reintroduce;
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    procedure Finish;
  end;

  { Unpack stream: reads a zstd stream from BaseStream (one or more frames in a row) }
  TZDecompressionStream = class(ZCustomStream)
  private
    FZds: PZSTD_DStream;
    FInBuf: TBytes;
    FOutBuf: TBytes;

    FIn: TZSTD_inBuffer;
    FOut: TZSTD_outBuffer;

    FOutPos: NativeUInt;
    FOutSize: NativeUInt;
    FEOF: Boolean;
    FAtFrameBoundary: Boolean;
    FPos: Int64;

    procedure RaiseOnZstdError(Code: TZSTDSize; const Where: string);
    function RefillInput: Boolean;
    function ProduceOutput: Boolean;
  public
    constructor Create(ABaseStream: TStream; AOwnsStream: Boolean = False);
      reintroduce;
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

implementation

{ ZCustomStream }

constructor ZCustomStream.Create(ABaseStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  if ABaseStream = nil then
    raise EArgumentNilException.Create('BaseStream = nil');
  FBaseStream := ABaseStream;
  FOwnsStream := AOwnsStream;
end;

destructor ZCustomStream.Destroy;
begin
  if FOwnsStream then
    FreeAndNil(FBaseStream);
  inherited Destroy;
end;

{ TZCompressionStream }

procedure TZCompressionStream.RaiseOnZstdError(Code: TZSTDSize;
  const Where: string);
begin
  if ZSTD_isError(Code) <> 0 then
    raise EZstdStreamError.CreateFmt('%s: %s',
      [Where, string(AnsiString(ZSTD_getErrorName(Code)))]);
end;

constructor TZCompressionStream.Create(ABaseStream: TStream;
  CompressionLevel: Integer; AOwnsStream: Boolean; WriteContentSize: Boolean;
  WriteChecksum: Boolean);
var
  Code: TZSTDSize;
  outSize: TZSTDSize;
begin
  inherited Create(ABaseStream, AOwnsStream);

  FCtx := ZSTD_createCCtx;
  if FCtx = nil then
    raise EZstdStreamError.Create('ZSTD_createCCtx failed');

  Code := ZSTD_CCtx_setParameter(FCtx, ZSTD_c_compressionLevel,
    CompressionLevel);
  RaiseOnZstdError(Code, 'ZSTD_CCtx_setParameter(compressionLevel)');

  Code := ZSTD_CCtx_setParameter(FCtx, ZSTD_c_contentSizeFlag,
    Ord(WriteContentSize));
  RaiseOnZstdError(Code, 'ZSTD_CCtx_setParameter(contentSizeFlag)');

  Code := ZSTD_CCtx_setParameter(FCtx, ZSTD_c_checksumFlag, Ord(WriteChecksum));
  RaiseOnZstdError(Code, 'ZSTD_CCtx_setParameter(checksumFlag)');

  Code := ZSTD_CCtx_reset(FCtx, ZSTD_reset_session_only);
  RaiseOnZstdError(Code, 'ZSTD_CCtx_reset');

  outSize := ZSTD_CStreamOutSize;
  if outSize = 0 then
    outSize := 131072;
  SetLength(FOutBuf, outSize);

  FOut.dst := @FOutBuf[0];
  FOut.size := Length(FOutBuf);
  FOut.pos := 0;

  FClosed := False;
end;

destructor TZCompressionStream.Destroy;
begin
  try
    if not FClosed then
      Finish;
  except

  end;

  if FCtx <> nil then
    ZSTD_freeCCtx(FCtx);

  inherited Destroy;
end;

procedure TZCompressionStream.FlushOutBuf;
begin
  if FOut.pos > 0 then
  begin
    FBaseStream.WriteBuffer(FOutBuf[0], FOut.pos);
    FOut.pos := 0;
  end;
end;

procedure TZCompressionStream.CompressStep(const InPtr: Pointer;
  InSize: TZSTDSize; EndOp: ZSTD_EndDirective);
var
  input: TZSTD_inBuffer;
  remaining: TZSTDSize;
begin
  input.src := InPtr;
  input.size := InSize;
  input.pos := 0;

  repeat
    FOut.dst := @FOutBuf[0];
    FOut.size := Length(FOutBuf);
    FOut.pos := 0;

    remaining := ZSTD_compressStream2(FCtx, FOut, input, EndOp);
    RaiseOnZstdError(remaining, 'ZSTD_compressStream2');

    FlushOutBuf;

  until ((EndOp = ZSTD_e_continue) and (input.pos = input.size)) or
    ((EndOp <> ZSTD_e_continue) and (remaining = 0));
end;

procedure TZCompressionStream.Finish;
begin
  if FClosed then
    Exit;
  CompressStep(nil, 0, ZSTD_e_end);
  FClosed := True;
end;

function TZCompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TZCompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  if FClosed then
    raise EZstdStreamError.Create('Cannot write: stream is finished');

  if Count <= 0 then
    Exit(0);

  CompressStep(@Buffer, TZSTDSize(Count), ZSTD_e_continue);
  Result := Count;
end;

function TZCompressionStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soCurrent:
      if Offset = 0 then
        Exit(FBaseStream.Position);
  end;
  raise EStreamError.Create('Seek is not supported for compression stream');
end;

{ TZDecompressionStream }

procedure TZDecompressionStream.RaiseOnZstdError(Code: TZSTDSize;
  const Where: string);
begin
  if ZSTD_isError(Code) <> 0 then
    raise EZstdStreamError.CreateFmt('%s: %s',
      [Where, string(AnsiString(ZSTD_getErrorName(Code)))]);
end;

constructor TZDecompressionStream.Create(ABaseStream: TStream;
  AOwnsStream: Boolean);
var
  InSize, outSize: TZSTDSize;
  Code: TZSTDSize;
begin
  inherited Create(ABaseStream, AOwnsStream);

  FZds := ZSTD_createDStream;
  if FZds = nil then
    raise EZstdStreamError.Create('ZSTD_createDStream failed');

  Code := ZSTD_initDStream(FZds);
  RaiseOnZstdError(Code, 'ZSTD_initDStream');

  InSize := ZSTD_DStreamInSize;
  if InSize = 0 then
    InSize := 131072;
  SetLength(FInBuf, InSize);

  outSize := ZSTD_DStreamOutSize;
  if outSize = 0 then
    outSize := 131072;
  SetLength(FOutBuf, outSize);

  FIn.src := nil;
  FIn.size := 0;
  FIn.pos := 0;

  FOut.dst := @FOutBuf[0];
  FOut.size := Length(FOutBuf);
  FOut.pos := 0;

  FOutPos := 0;
  FOutSize := 0;
  FEOF := False;
  FPos := 0;
  FAtFrameBoundary := true;
end;

destructor TZDecompressionStream.Destroy;
begin
  if FZds <> nil then
    ZSTD_freeDStream(FZds);
  inherited Destroy;
end;

function TZDecompressionStream.RefillInput: Boolean;
var
  n: Integer;
begin
  if (FIn.pos < FIn.size) then
    Exit(True);

  n := FBaseStream.Read(FInBuf[0], Length(FInBuf));
  if n <= 0 then
  begin
    FIn.src := nil;
    FIn.size := 0;
    FIn.pos := 0;
    Exit(False);
  end;

  FIn.src := @FInBuf[0];
  FIn.size := TZSTDSize(n);
  FIn.pos := 0;
  Result := True;
end;

function TZDecompressionStream.ProduceOutput: Boolean;
var
  remaining: TZSTDSize;
begin
  Result := False;
  if FEOF then Exit(False);

  if (FIn.pos >= FIn.size) then
  begin
    if not RefillInput then
    begin
      // There's no more input.
      // If we're at a frame boundary, that's the normal end.
      if FAtFrameBoundary then
      begin
        FEOF := True;
        Exit(False);
      end;

      // Otherwise, the stream is interrupted in the middle of the frame
      raise EZstdStreamError.Create('Unexpected end of compressed stream');
    end;
  end;

  FOut.dst := @FOutBuf[0];
  FOut.size := Length(FOutBuf);
  FOut.pos := 0;

  remaining := ZSTD_decompressStream(FZds, FOut, FIn);
  RaiseOnZstdError(remaining, 'ZSTD_decompressStream');

  // remaining=0 => end of frame (possibly further concatenated frames)
  FAtFrameBoundary := (remaining = 0);

  if FOut.pos > 0 then
  begin
    FOutPos := 0;
    FOutSize := FOut.pos;
    Exit(True);
  end;

  // output пуст: либо нужно больше input, либо мы на границе и ждём следующий фрейм
  Exit(False);
end;

function TZDecompressionStream.Read(var Buffer; Count: Longint): Longint;
var
  dst: PByte;
  toCopy: NativeUInt;
begin
  if Count <= 0 then
    Exit(0);

  dst := @Buffer;
  Result := 0;

  while Count > 0 do
  begin
    if FOutPos < FOutSize then
    begin
      toCopy := FOutSize - FOutPos;
      if toCopy > NativeUInt(Count) then
        toCopy := NativeUInt(Count);

      Move(FOutBuf[FOutPos], dst^, toCopy);

      Inc(FOutPos, toCopy);
      Inc(Result, toCopy);
      Inc(FPos, toCopy);

      Inc(dst, toCopy);
      Dec(Count, toCopy);
      Continue;
    end;

    if FEOF then
        Break;

    if not ProduceOutput then
    begin
      if FEOF then
        Break;

      Continue;
    end;
  end;
end;

function TZDecompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TZDecompressionStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soCurrent:
      if Offset = 0 then
        Exit(FPos);
  end;
  raise EStreamError.Create('Seek is not supported for decompression stream');
end;

end.
