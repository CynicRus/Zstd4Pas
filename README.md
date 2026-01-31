# Zstd4Pas

**Zstandard (zstd) bindings and streaming classes for Delphi and Free Pascal**

Zstd4Pas is a lightweight, cross-platform Pascal wrapper for the official [Zstandard](https://github.com/facebook/zstd) compression library (`libzstd`).  
It provides:

- Complete bindings to the zstd C API (`zstd.pas`)
- Convenient `TStream`-descendant classes for streaming compression and decompression (`zstdstreams.pas`)

Supported compilers and platforms:

- **Delphi** – Windows (x86 and x64)
- **Free Pascal** – Windows, Linux, macOS (Darwin)

## Features

- Full access to simple, advanced, streaming, and dictionary APIs
- Streaming classes (`TZCompressionStream`, `TZDecompressionStream`) fully compatible with Delphi/Free Pascal `TStream` hierarchy
- Support for content size flag, checksums, custom compression levels, and advanced parameters
- Multi-frame decompression (concatenated zstd frames)
- Clean error handling with Pascal exceptions
- No external dependencies other than the official `libzstd` shared library

## Requirements

You need the **libzstd** shared library in a location where the OS can find it:

| Platform   | Library name                | Typical location |
|------------|-----------------------------|------------------|
| Windows    | `libzstd.dll`               | Same folder as executable or in PATH |
| Linux      | `libzstd.so.1`              | System library path (`/usr/lib`, etc.) |
| macOS      | `libzstd.dylib`             | System library path |

You can obtain the library from:
- Official Zstd releases: https://github.com/facebook/zstd/releases
- Package managers (`apt install libzstd-dev`, `brew install zstd`, etc.)

## Installation

1. Clone or download this repository.
2. Add `zstd.pas` and (optionally) `zstdstreams.pas` to your project.
3. Ensure the appropriate `libzstd` shared library is available at runtime.

No further compilation steps are required – the units dynamically load the shared library.

## Usage Examples

### Simple One-Shot Compression / Decompression

```pascal
uses
  zstd;

procedure SimpleCompressDecompress;
var
  Src, Compressed, Decompressed: TBytes;
  CompressedSize, DecompressedSize: TZSTDSize;
begin
  Src := TEncoding.UTF8.GetBytes('Hello world! This is a test of Zstandard compression.');

  // Compress
  SetLength(Compressed, ZSTD_compressBound(Length(Src)));
  CompressedSize := ZSTD_compress(@Compressed[0], Length(Compressed),
                                @Src[0], Length(Src), 3);
  if ZSTD_isError(CompressedSize) <> 0 then
    raise Exception.Create('Compression failed: ' + ZSTD_getErrorName(CompressedSize));
  SetLength(Compressed, CompressedSize);

  // Decompress
  SetLength(Decompressed, Length(Src)); // known original size
  DecompressedSize := ZSTD_decompress(@Decompressed[0], Length(Decompressed),
                                      @Compressed[0], Length(Compressed));
  if ZSTD_isError(DecompressedSize) <> 0 then
    raise Exception.Create('Decompression failed: ' + ZSTD_getErrorName(DecompressedSize));

  Writeln('Original size: ', Length(Src));
  Writeln('Compressed size: ', Length(Compressed));
  Writeln('Decompressed matches: ', CompareMem(@Src[0], @Decompressed[0], Length(Src)));
end;
```

### Streaming Compression (using TZCompressionStream)

```pascal
uses
  Classes, zstdstreams;

procedure CompressFile(const InFile, OutFile: string);
var
  InStream, OutStream: TFileStream;
  ZStream: TZCompressionStream;
begin
  InStream := TFileStream.Create(InFile, fmOpenRead);
  try
    OutStream := TFileStream.Create(OutFile, fmCreate);
    try
      // Level 3, write content size, no checksum, owns OutStream
      ZStream := TZCompressionStream.Create(OutStream, 3, True, True, False);
      try
        ZStream.CopyFrom(InStream, 0); // copies everything
        // Destructor automatically calls Finish and flushes
      finally
        ZStream.Free;
      end;
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;
```

### Streaming Decompression (using TZDecompressionStream)

```pascal
uses
  Classes, zstdstreams;

procedure DecompressFile(const InFile, OutFile: string);
var
  InStream, OutStream: TFileStream;
  ZStream: TZDecompressionStream;
  Buffer: TBytes;
begin
  InStream := TFileStream.Create(InFile, fmOpenRead);
  try
    OutStream := TFileStream.Create(OutFile, fmCreate);
    try
      ZStream := TZDecompressionStream.Create(InStream); // does not own InStream
      try
        SetLength(Buffer, 64 * 1024);
        repeat
          Count := ZStream.Read(Buffer[0], Length(Buffer));
          if Count > 0 then
            OutStream.WriteBuffer(Buffer[0], Count);
        until Count = 0;
      finally
        ZStream.Free;
      end;
    finally
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;
```

### Using Dictionaries

```pascal
var
  CDict: PZSTD_CDict;
  DDict: PZSTD_DDict;
begin
  CDict := ZSTD_createCDict(DictBuffer, DictSize, CompressionLevel);
  try
    // Use with ZSTD_compress_usingCDict(...)
  finally
    ZSTD_freeCDict(CDict);
  end;

  DDict := ZSTD_createDDict(DictBuffer, DictSize);
  try
    // Use with ZSTD_decompress_usingDDict(...)
  finally
    ZSTD_freeDDict(DDict);
  end;
end;
```

## License

This project is licensed under the **MIT License** – see the [LICENSE](LICENSE) file for details.

```
MIT License

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
```

The underlying Zstandard library (`libzstd`) is distributed under a dual BSD/GPLv2 license. This wrapper does not modify or redistribute the library itself.
