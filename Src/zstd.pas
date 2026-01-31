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

unit zstd;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKRECORDS C}
{$ENDIF}

interface

uses
  SysUtils;
{$Z4}
{==============================================================================}
{ Platform Specific Library Loading                                             }
{==============================================================================}
const
  {$IFDEF MSWINDOWS}
    ZSTDLIB = 'libzstd.dll';
  {$ENDIF}
  {$IFDEF LINUX}
    ZSTDLIB = 'libzstd.so.1';
  {$ENDIF}
  {$IFDEF DARWIN}
    ZSTDLIB = 'libzstd.dylib';
  {$ENDIF}

{==============================================================================}
{ Types                                                                        }
{==============================================================================}
type
  // size_t is platform dependent
  TZSTDSize = NativeUInt;

  // Opaque Contexts
  PZSTD_CCtx = Pointer;
  PZSTD_DCtx = Pointer;
  PZSTD_CStream = Pointer;
  PZSTD_DStream = Pointer;
  PZSTD_CDict = Pointer;
  PZSTD_DDict = Pointer;

  // Enumerations mapped to integers for API compatibility
  ZSTD_strategy = (
    ZSTD_fast = 1,
    ZSTD_dfast = 2,
    ZSTD_greedy = 3,
    ZSTD_lazy = 4,
    ZSTD_lazy2 = 5,
    ZSTD_btlazy2 = 6,
    ZSTD_btopt = 7,
    ZSTD_btultra = 8,
    ZSTD_btultra2 = 9
  );

  ZSTD_cParameter = (
    ZSTD_c_compressionLevel = 100,
    ZSTD_c_windowLog = 101,
    ZSTD_c_hashLog = 102,
    ZSTD_c_chainLog = 103,
    ZSTD_c_searchLog = 104,
    ZSTD_c_minMatch = 105,
    ZSTD_c_targetLength = 106,
    ZSTD_c_strategy = 107,
    ZSTD_c_targetCBlockSize = 130,
    ZSTD_c_enableLongDistanceMatching = 160,
    ZSTD_c_ldmHashLog = 161,
    ZSTD_c_ldmMinMatch = 162,
    ZSTD_c_ldmBucketSizeLog = 163,
    ZSTD_c_ldmHashRateLog = 164,
    ZSTD_c_contentSizeFlag = 200,
    ZSTD_c_checksumFlag = 201,
    ZSTD_c_dictIDFlag = 202,
    ZSTD_c_nbWorkers = 400,
    ZSTD_c_jobSize = 401,
    ZSTD_c_overlapLog = 402
  );

  ZSTD_dParameter = (
    ZSTD_d_windowLogMax = 100
  );

  ZSTD_ResetDirective = (
    ZSTD_reset_session_only = 1,
    ZSTD_reset_parameters = 2,
    ZSTD_reset_session_and_parameters = 3
  );

  ZSTD_EndDirective = (
    ZSTD_e_continue = 0,
    ZSTD_e_flush = 1,
    ZSTD_e_end = 2
  );

  // Compression Parameters structures
  PPZSTD_compressionParameters = ^PZSTD_compressionParameters;
  PZSTD_compressionParameters = ^TZSTD_compressionParameters;
  TZSTD_compressionParameters = record
    windowLog: Cardinal;
    chainLog: Cardinal;
    hashLog: Cardinal;
    searchLog: Cardinal;
    minMatch: Cardinal;
    targetLength: Cardinal;
    strategy: ZSTD_strategy;
  end;

  PZSTD_frameParameters = ^TZSTD_frameParameters;
  TZSTD_frameParameters = record
    contentSizeFlag: Integer;
    checksumFlag: Integer;
    noDictIDFlag: Integer;
  end;

  PZSTD_parameters = ^TZSTD_parameters;
  TZSTD_parameters = record
    cParams: TZSTD_compressionParameters;
    fParams: TZSTD_frameParameters;
  end;

  // Streaming Buffers
  PZSTD_inBuffer = ^TZSTD_inBuffer;
  TZSTD_inBuffer = record
    src: Pointer;       // start of input buffer
    size: TZSTDSize;    // size of input buffer
    pos: TZSTDSize;     // position where reading stopped. 0 <= pos <= size
  end;

  PZSTD_outBuffer = ^TZSTD_outBuffer;
  TZSTD_outBuffer = record
    dst: Pointer;       // start of output buffer
    size: TZSTDSize;    // size of output buffer
    pos: TZSTDSize;     // position where writing stopped. 0 <= pos <= size
  end;

  // Bounds
  PZSTD_bounds = ^TZSTD_bounds;
  TZSTD_bounds = record
    error: TZSTDSize;
    lowerBound: Integer;
    upperBound: Integer;
  end;

{==============================================================================}
{ Constants                                                                    }
{==============================================================================}
const
  ZSTD_CONTENTSIZE_UNKNOWN = UInt64($FFFFFFFFFFFFFFFF);
  ZSTD_CONTENTSIZE_ERROR   = UInt64($FFFFFFFFFFFFFFFE);

  ZSTD_MAGICNUMBER            = $FD2FB528;
  ZSTD_MAGIC_DICTIONARY       = $EC30A437;
  ZSTD_MAGIC_SKIPPABLE_START  = $184D2A50;

{==============================================================================}
{ Version & Error Management                                                   }
{==============================================================================}
function ZSTD_versionNumber: Cardinal; cdecl; external ZSTDLIB;
function ZSTD_versionString: PAnsiChar; cdecl; external ZSTDLIB;

function ZSTD_isError(code: TZSTDSize): Cardinal; cdecl; external ZSTDLIB;
function ZSTD_getErrorName(code: TZSTDSize): PAnsiChar; cdecl; external ZSTDLIB;

// Note: ZSTD_getErrorCode returns ZSTD_ErrorCode enum, but Integer is safer for binding unless we map all errors
function ZSTD_getErrorCode(functionResult: TZSTDSize): Integer; cdecl; external ZSTDLIB;

function ZSTD_minCLevel: Integer; cdecl; external ZSTDLIB;
function ZSTD_maxCLevel: Integer; cdecl; external ZSTDLIB;
function ZSTD_defaultCLevel: Integer; cdecl; external ZSTDLIB;

{==============================================================================}
{ Simple API                                                                   }
{==============================================================================}
function ZSTD_compress(dst: Pointer; dstCapacity: TZSTDSize; src: Pointer; srcSize: TZSTDSize; compressionLevel: Integer): TZSTDSize; cdecl; external ZSTDLIB;
function ZSTD_decompress(dst: Pointer; dstCapacity: TZSTDSize; src: Pointer; srcSize: TZSTDSize): TZSTDSize; cdecl; external ZSTDLIB;

function ZSTD_compressBound(srcSize: TZSTDSize): TZSTDSize; cdecl; external ZSTDLIB;

{==============================================================================}
{ Explicit Context API                                                         }
{==============================================================================}
// Compression Context
function ZSTD_createCCtx: PZSTD_CCtx; cdecl; external ZSTDLIB;
function ZSTD_freeCCtx(cctx: PZSTD_CCtx): TZSTDSize; cdecl; external ZSTDLIB;
function ZSTD_compressCCtx(cctx: PZSTD_CCtx; dst: Pointer; dstCapacity: TZSTDSize; src: Pointer; srcSize: TZSTDSize; compressionLevel: Integer): TZSTDSize; cdecl; external ZSTDLIB;

// Decompression Context
function ZSTD_createDCtx: PZSTD_DCtx; cdecl; external ZSTDLIB;
function ZSTD_freeDCtx(dctx: PZSTD_DCtx): TZSTDSize; cdecl; external ZSTDLIB;
function ZSTD_decompressDCtx(dctx: PZSTD_DCtx; dst: Pointer; dstCapacity: TZSTDSize; src: Pointer; srcSize: TZSTDSize): TZSTDSize; cdecl; external ZSTDLIB;

{==============================================================================}
{ Advanced Compression API (Parameters)                                        }
{==============================================================================}
function ZSTD_compress2(cctx: PZSTD_CCtx; dst: Pointer; dstCapacity: TZSTDSize; src: Pointer; srcSize: TZSTDSize): TZSTDSize; cdecl; external ZSTDLIB;

function ZSTD_cParam_getBounds(cParam: ZSTD_cParameter): TZSTD_bounds; cdecl; external ZSTDLIB;
function ZSTD_CCtx_setParameter(cctx: PZSTD_CCtx; param: ZSTD_cParameter; value: Integer): TZSTDSize; cdecl; external ZSTDLIB;
function ZSTD_CCtx_setPledgedSrcSize(cctx: PZSTD_CCtx; pledgedSrcSize: UInt64): TZSTDSize; cdecl; external ZSTDLIB;
function ZSTD_CCtx_reset(cctx: PZSTD_CCtx; reset: ZSTD_ResetDirective): TZSTDSize; cdecl; external ZSTDLIB;

{==============================================================================}
{ Advanced Decompression API (Parameters)                                     }
{==============================================================================}
function ZSTD_dParam_getBounds(dParam: ZSTD_dParameter): TZSTD_bounds; cdecl; external ZSTDLIB;
function ZSTD_DCtx_setParameter(dctx: PZSTD_DCtx; param: ZSTD_dParameter; value: Integer): TZSTDSize; cdecl; external ZSTDLIB;
function ZSTD_DCtx_reset(dctx: PZSTD_DCtx; reset: ZSTD_ResetDirective): TZSTDSize; cdecl; external ZSTDLIB;

{==============================================================================}
{ Streaming Compression API                                                    }
{==============================================================================}
function ZSTD_createCStream: PZSTD_CStream; cdecl; external ZSTDLIB;
function ZSTD_freeCStream(zcs: PZSTD_CStream): TZSTDSize; cdecl; external ZSTDLIB;

function ZSTD_initCStream(zcs: PZSTD_CStream; compressionLevel: Integer): TZSTDSize; cdecl; external ZSTDLIB;
function ZSTD_compressStream2(cctx: PZSTD_CCtx; var output: TZSTD_outBuffer; var input: TZSTD_inBuffer; endOp: ZSTD_EndDirective): TZSTDSize; cdecl; external ZSTDLIB;

function ZSTD_flushStream(zcs: PZSTD_CStream; var output: TZSTD_outBuffer): TZSTDSize; cdecl; external ZSTDLIB;
function ZSTD_endStream(zcs: PZSTD_CStream; var output: TZSTD_outBuffer): TZSTDSize; cdecl; external ZSTDLIB;

function ZSTD_CStreamInSize: TZSTDSize; cdecl; external ZSTDLIB;
function ZSTD_CStreamOutSize: TZSTDSize; cdecl; external ZSTDLIB;

{==============================================================================}
{ Streaming Decompression API                                                  }
{==============================================================================}
function ZSTD_createDStream: PZSTD_DStream; cdecl; external ZSTDLIB;
function ZSTD_freeDStream(zds: PZSTD_DStream): TZSTDSize; cdecl; external ZSTDLIB;

function ZSTD_initDStream(zds: PZSTD_DStream): TZSTDSize; cdecl; external ZSTDLIB;
function ZSTD_decompressStream(zds: PZSTD_DStream; var output: TZSTD_outBuffer; var input: TZSTD_inBuffer): TZSTDSize; cdecl; external ZSTDLIB;

function ZSTD_DStreamInSize: TZSTDSize; cdecl; external ZSTDLIB;
function ZSTD_DStreamOutSize: TZSTDSize; cdecl; external ZSTDLIB;

{==============================================================================}
{ Dictionary Simple API                                                        }
{==============================================================================}
function ZSTD_compress_usingDict(ctx: PZSTD_CCtx; dst: Pointer; dstCapacity: TZSTDSize; src: Pointer; srcSize: TZSTDSize; dict: Pointer; dictSize: TZSTDSize; compressionLevel: Integer): TZSTDSize; cdecl; external ZSTDLIB;
function ZSTD_decompress_usingDict(dctx: PZSTD_DCtx; dst: Pointer; dstCapacity: TZSTDSize; src: Pointer; srcSize: TZSTDSize; dict: Pointer; dictSize: TZSTDSize): TZSTDSize; cdecl; external ZSTDLIB;

{==============================================================================}
{ Dictionary Bulk Processing API                                              }
{==============================================================================}
function ZSTD_createCDict(dictBuffer: Pointer; dictSize: TZSTDSize; compressionLevel: Integer): PZSTD_CDict; cdecl; external ZSTDLIB;
function ZSTD_freeCDict(CDict: PZSTD_CDict): TZSTDSize; cdecl; external ZSTDLIB;
function ZSTD_compress_usingCDict(cctx: PZSTD_CCtx; dst: Pointer; dstCapacity: TZSTDSize; src: Pointer; srcSize: TZSTDSize; cdict: PZSTD_CDict): TZSTDSize; cdecl; external ZSTDLIB;

function ZSTD_createDDict(dictBuffer: Pointer; dictSize: TZSTDSize): PZSTD_DDict; cdecl; external ZSTDLIB;
function ZSTD_freeDDict(ddict: PZSTD_DDict): TZSTDSize; cdecl; external ZSTDLIB;
function ZSTD_decompress_usingDDict(dctx: PZSTD_DCtx; dst: Pointer; dstCapacity: TZSTDSize; src: Pointer; srcSize: TZSTDSize; ddict: PZSTD_DDict): TZSTDSize; cdecl; external ZSTDLIB;

{==============================================================================}
{ Dictionary Helper Functions                                                  }
{==============================================================================}
function ZSTD_getDictID_fromDict(dict: Pointer; dictSize: TZSTDSize): Cardinal; cdecl; external ZSTDLIB;
function ZSTD_getDictID_fromDDict(ddict: PZSTD_DDict): Cardinal; cdecl; external ZSTDLIB;
function ZSTD_getDictID_fromFrame(src: Pointer; srcSize: TZSTDSize): Cardinal; cdecl; external ZSTDLIB;

{==============================================================================}
{ Frame Content Inspection                                                     }
{==============================================================================}
function ZSTD_getFrameContentSize(src: Pointer; srcSize: TZSTDSize): UInt64; cdecl; external ZSTDLIB;
function ZSTD_findFrameCompressedSize(src: Pointer; srcSize: TZSTDSize): TZSTDSize; cdecl; external ZSTDLIB;

implementation

end.
