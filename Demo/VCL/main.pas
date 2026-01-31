unit main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.IOUtils, System.Diagnostics,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  ZStdStreams, zstd;

type
  TForm1 = class(TForm)
    pnlTop: TPanel;
    lblTitle: TLabel;
    pnlMain: TPanel;

    grpCompress: TGroupBox;
    btnSelectFile: TButton;
    edtInputFile: TEdit;
    lblInputSize: TLabel;
    btnCompress: TButton;

    grpDecompress: TGroupBox;
    btnSelectCompressed: TButton;
    edtCompressedFile: TEdit;
    lblCompressedSize: TLabel;
    btnDecompress: TButton;

    pnlLog: TPanel;
    lblLog: TLabel;
    memoLog: TMemo;
    openFileDialog: TOpenDialog;
    saveFileDialog: TSaveDialog;
    pbProgress: TProgressBar;
    lblStatus: TLabel;

    cmbLevel: TComboBox;
    lblLevel: TLabel;
    chkVerify: TCheckBox;
    chkWriteContentSize: TCheckBox;
    chkWriteChecksum: TCheckBox;

    procedure FormCreate(Sender: TObject);
    procedure btnSelectFileClick(Sender: TObject);
    procedure btnSelectCompressedClick(Sender: TObject);
    procedure btnCompressClick(Sender: TObject);
    procedure btnDecompressClick(Sender: TObject);
  private
    FInputFile: string;
    FCompressedFile: string;

    procedure Log(const Msg: string);
    procedure CompressFile;
    procedure DecompressFile;
    function FormatFileSize(Size: Int64): string;

    function SelectedLevel: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  cmbLevel.Items.Clear;
  cmbLevel.Items.Add('1 - Fast');
  cmbLevel.Items.Add('3 - Default');
  cmbLevel.Items.Add('6 - Better');
  cmbLevel.Items.Add('9 - High');
  cmbLevel.Items.Add('12 - Very High');
  cmbLevel.Items.Add('15 - Ultra');
  cmbLevel.ItemIndex := 1; // 3 default-ish

  chkWriteContentSize.Checked := True;
  chkWriteChecksum.Checked := False;

  pbProgress.Position := 0;
  lblStatus.Caption := 'Ready';
  memoLog.Clear;

  Log('Zstandard (zstd) demo initialized');
  Log('zstd version: ' + string(AnsiString(ZSTD_versionString)));
end;

procedure TForm1.Log(const Msg: string);
begin
  memoLog.Lines.Add(Format('[%s] %s', [FormatDateTime('hh:nn:ss', Now), Msg]));
  Application.ProcessMessages;
end;

function TForm1.FormatFileSize(Size: Int64): string;
begin
  if Size < 1024 then
    Result := Format('%d B', [Size])
  else if Size < 1024 * 1024 then
    Result := Format('%.2f KB', [Size / 1024])
  else if Size < 1024 * 1024 * 1024 then
    Result := Format('%.2f MB', [Size / 1024 / 1024])
  else
    Result := Format('%.2f GB', [Size / 1024 / 1024 / 1024]);
end;

function TForm1.SelectedLevel: Integer;
const
  Map: array[0..5] of Integer = (1, 3, 6, 9, 12, 15);
begin
  if (cmbLevel.ItemIndex < Low(Map)) or (cmbLevel.ItemIndex > High(Map)) then
    Exit(3);
  Result := Map[cmbLevel.ItemIndex];
end;

procedure TForm1.btnSelectFileClick(Sender: TObject);
begin
  openFileDialog.Filter := 'All Files (*.*)|*.*';
  openFileDialog.Title := 'Select File to Compress';

  if openFileDialog.Execute then
  begin
    FInputFile := openFileDialog.FileName;
    edtInputFile.Text := ExtractFileName(FInputFile);
    edtInputFile.Hint := FInputFile;

    if FileExists(FInputFile) then
    begin
      lblInputSize.Caption := 'Size: ' + FormatFileSize(TFile.GetSize(FInputFile));
      Log('Selected file: ' + FInputFile);
      btnCompress.Enabled := True;
    end;
  end;
end;

procedure TForm1.btnSelectCompressedClick(Sender: TObject);
begin
  openFileDialog.Filter := 'Zstandard Files (*.zst)|*.zst|All Files (*.*)|*.*';
  openFileDialog.Title := 'Select zstd File to Decompress';

  if openFileDialog.Execute then
  begin
    FCompressedFile := openFileDialog.FileName;
    edtCompressedFile.Text := ExtractFileName(FCompressedFile);
    edtCompressedFile.Hint := FCompressedFile;

    if FileExists(FCompressedFile) then
    begin
      lblCompressedSize.Caption := 'Size: ' + FormatFileSize(TFile.GetSize(FCompressedFile));
      Log('Selected compressed file: ' + FCompressedFile);
      btnDecompress.Enabled := True;
    end;
  end;
end;

procedure TForm1.btnCompressClick(Sender: TObject);
begin
  if FInputFile.IsEmpty or not FileExists(FInputFile) then
  begin
    ShowMessage('Please select an input file first!');
    Exit;
  end;

  saveFileDialog.Filter := 'Zstandard Files (*.zst)|*.zst';
  saveFileDialog.FileName := TPath.GetFileName(FInputFile) + '.zst';
  saveFileDialog.Title := 'Save Compressed File';

  if saveFileDialog.Execute then
  begin
    FCompressedFile := saveFileDialog.FileName;
    CompressFile;
  end;
end;

procedure TForm1.btnDecompressClick(Sender: TObject);
begin
  if FCompressedFile.IsEmpty or not FileExists(FCompressedFile) then
  begin
    ShowMessage('Please select a compressed file first!');
    Exit;
  end;

  saveFileDialog.Filter := 'All Files (*.*)|*.*';
  saveFileDialog.FileName := TPath.GetFileNameWithoutExtension(FCompressedFile);
  saveFileDialog.Title := 'Save Decompressed File';

  if saveFileDialog.Execute then
    DecompressFile;
end;

procedure TForm1.CompressFile;
const
  BUFFER_SIZE = 64 * 1024;
var
  InputStream: TFileStream;
  OutFileStream: TFileStream;
  Zs: TZCompressionStream;
  Buffer: array[0..BUFFER_SIZE - 1] of Byte;
  BytesRead: Integer;
  TotalRead: Int64;
  SW: TStopwatch;
  OriginalSize, CompressedSize: Int64;
  Ratio: Double;
  Level: Integer;
  OutputFile: string;
begin
  OutputFile := FCompressedFile;
  Level := SelectedLevel;

  btnCompress.Enabled := False;
  pbProgress.Position := 0;
  lblStatus.Caption := 'Compressing...';
  Log('─────────────────────────────────');
  Log('Starting compression (zstd)...');
  Log(Format('Level: %d, content size flag: %s, checksum: %s',
    [Level,
     BoolToStr(chkWriteContentSize.Checked, True),
     BoolToStr(chkWriteChecksum.Checked, True)]));

  try
    InputStream := TFileStream.Create(FInputFile, fmOpenRead or fmShareDenyWrite);
    try
      OriginalSize := InputStream.Size;

      OutFileStream := TFileStream.Create(OutputFile, fmCreate);
      try
        Zs := TZCompressionStream.Create(
          OutFileStream,
          Level,
          True {owns OutFileStream},
          chkWriteContentSize.Checked,
          chkWriteChecksum.Checked
        );
        try
          SW := TStopwatch.StartNew;
          TotalRead := 0;

          repeat
            BytesRead := InputStream.Read(Buffer, BUFFER_SIZE);
            if BytesRead > 0 then
            begin
              Zs.WriteBuffer(Buffer, BytesRead);
              Inc(TotalRead, BytesRead);

              if OriginalSize > 0 then
                pbProgress.Position := Round((TotalRead / OriginalSize) * 100)
              else
                pbProgress.Position := 100;

              Application.ProcessMessages;
            end;
          until BytesRead = 0;

          Zs.Finish;
          SW.Stop;
        finally
          Zs.Free;
        end;
      except
        OutFileStream.Free;
        raise;
      end;

      CompressedSize := TFile.GetSize(OutputFile);
      if OriginalSize > 0 then
        Ratio := (1 - (CompressedSize / OriginalSize)) * 100
      else
        Ratio := 0;

      Log('✓ Compression successful!');
      Log('Original size: ' + FormatFileSize(OriginalSize));
      Log('Compressed size: ' + FormatFileSize(CompressedSize));
      if OriginalSize > 0 then
        Log(Format('Compression ratio: %.2f%%', [Ratio]));
      Log(Format('Time: %.2f ms', [SW.Elapsed.TotalMilliseconds]));
      if SW.Elapsed.TotalSeconds > 0 then
        Log(Format('Speed: %.2f MB/s', [OriginalSize / 1024 / 1024 / SW.Elapsed.TotalSeconds]));

      pbProgress.Position := 100;
      lblStatus.Caption := 'Compression complete!';

      // автоподстановка в секцию распаковки
      edtCompressedFile.Text := ExtractFileName(OutputFile);
      edtCompressedFile.Hint := OutputFile;
      lblCompressedSize.Caption := 'Size: ' + FormatFileSize(CompressedSize);
      btnDecompress.Enabled := True;

      if chkVerify.Checked then
      begin
        Log('Verifying compressed data...');
        // Простая верификация: попытка распаковать в "никуда"
        // (если поток битый/обрезан — получите исключение)
        try
          var InZst := TFileStream.Create(OutputFile, fmOpenRead or fmShareDenyWrite);
          try
            var Dz := TZDecompressionStream.Create(InZst, True {owns InZst});
            try
              var Sink: array[0..BUFFER_SIZE - 1] of Byte;
              while Dz.Read(Sink, SizeOf(Sink)) > 0 do ;
            finally
              Dz.Free;
            end;
          except
            InZst.Free;
            raise;
          end;
          Log('✓ Verify OK');
        except
          on E: Exception do
            Log('✗ Verify FAILED: ' + E.Message);
        end;
      end;

    finally
      InputStream.Free;
    end;
  except
    on E: Exception do
    begin
      Log('✗ ERROR: ' + E.Message);
      lblStatus.Caption := 'Compression failed!';
      ShowMessage('Compression error: ' + E.Message);
    end;
  end;

  btnCompress.Enabled := True;
  pbProgress.Position := 0;
end;

procedure TForm1.DecompressFile;
const
  BUFFER_SIZE = 64 * 1024;
var
  InputStream: TFileStream;
  OutStream: TFileStream;
  Dz: TZDecompressionStream;
  Buffer: array[0..BUFFER_SIZE - 1] of Byte;
  BytesRead: Integer;
  TotalWritten: Int64;
  SW: TStopwatch;
  CompressedSize, DecompressedSize: Int64;
  OutputFile: string;
begin
  OutputFile := saveFileDialog.FileName;

  btnDecompress.Enabled := False;
  pbProgress.Position := 0;
  lblStatus.Caption := 'Decompressing...';
  Log('─────────────────────────────────');
  Log('Starting decompression (zstd)...');

  try
    InputStream := TFileStream.Create(FCompressedFile, fmOpenRead or fmShareDenyWrite);
    try
      CompressedSize := InputStream.Size;

      Dz := TZDecompressionStream.Create(InputStream, False {do not own});
      try
        OutStream := TFileStream.Create(OutputFile, fmCreate);
        try
          SW := TStopwatch.StartNew;
          TotalWritten := 0;

          repeat
            BytesRead := Dz.Read(Buffer, BUFFER_SIZE);
            if BytesRead > 0 then
            begin
              OutStream.WriteBuffer(Buffer, BytesRead);
              Inc(TotalWritten, BytesRead);

              pbProgress.Position := (pbProgress.Position + 5) mod 100;
              Application.ProcessMessages;
            end;
          until BytesRead = 0;

          SW.Stop;
          DecompressedSize := TotalWritten;

          Log('✓ Decompression successful!');
          Log('Compressed size: ' + FormatFileSize(CompressedSize));
          Log('Decompressed size: ' + FormatFileSize(DecompressedSize));
          Log(Format('Time: %.2f ms', [SW.Elapsed.TotalMilliseconds]));
          if SW.Elapsed.TotalSeconds > 0 then
            Log(Format('Speed: %.2f MB/s', [DecompressedSize / 1024 / 1024 / SW.Elapsed.TotalSeconds]));

          pbProgress.Position := 100;
          lblStatus.Caption := 'Decompression complete!';

        finally
          OutStream.Free;
        end;
      finally
        Dz.Free;
      end;
    finally
      InputStream.Free;
    end;
  except
    on E: Exception do
    begin
      Log('✗ ERROR: ' + E.Message);
      lblStatus.Caption := 'Decompression failed!';
      ShowMessage('Decompression error: ' + E.Message);
    end;
  end;

  btnDecompress.Enabled := True;
  pbProgress.Position := 0;
end;

end.