{ ****************************************************************************** }
{ * JPEG-LS Codec https://github.com/zekiguven/pascal_jls                      * }
{ * fixed by QQ 600585@qq.com                                                  * }
{ ****************************************************************************** }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
{
  JPEG-LS Codec
  This code is based on http://www.stat.columbia.edu/~jakulin/jpeg-ls/mirror.htm
  Converted from C to Pascal. 2017

  https://github.com/zekiguven/pascal_jls

  author : Zeki Guven
}
unit JLSCodec;

{$INCLUDE zDefine.inc}

interface

uses
  CoreClasses, JLSGlobal, JLSEncoder, JLSDecoder;

type
  PJlsParameters       = JLSGlobal.PJlsParameters;
  TJlsParameters       = JLSGlobal.TJlsParameters;
  TJlsCustomParameters = JLSGlobal.TJlsCustomParameters;

function jpegls_decompress(SourceStream, OutputStream: TCoreClassStream; Info: PJlsParameters): Boolean;
function jpegls_compress(SourceStream, OutputStream: TCoreClassStream; Info: PJlsParameters): Boolean;

implementation

function jpegls_decompress(SourceStream, OutputStream: TCoreClassStream; Info: PJlsParameters): Boolean;
var
  dec: TJLSDecoder;
begin
  Result := False;
  dec := TJLSDecoder.Create;
  try
    dec.InputStream := SourceStream;
    dec.OutputStream := OutputStream;

    if dec.Execute then
      begin
        if Info <> nil then
          begin
            Info^.width := dec.width;
            Info^.height := dec.height;
            Info^.BitsPerSample := dec.bpp;
            Info^.Components := dec.Components;
            Info^.AllowedLossyError := dec._near;
            Info^.Custom.t1 := dec.t1;
            Info^.Custom.t2 := dec.t2;
            Info^.Custom.t3 := dec.t3;
            Info^.Custom.Reset := dec.Reset;
            Info^.Custom.MaxVal := dec.MaxVal;
          end;
        Result := True;
      end;

  finally
      dec.Free;
  end;
end;

function jpegls_compress(SourceStream, OutputStream: TCoreClassStream; Info: PJlsParameters): Boolean;
var
  enc: TJLSEncoder;
begin
  Result := False;
  enc := TJLSEncoder.Create;
  try
    enc.InputStream := SourceStream;
    enc.OutputStream := OutputStream;

    enc.width := Info^.width;
    enc.height := Info^.height;
    enc.bpp := Info^.BitsPerSample;
    enc.Components := Info^.Components;
    enc.t1 := Info^.Custom.t1;
    enc.t2 := Info^.Custom.t2;
    enc.t3 := Info^.Custom.t3;
    enc.Reset := Info^.Custom.Reset;
    enc._near := Info^.AllowedLossyError;
    enc.MaxVal := Info^.Custom.MaxVal;

    if enc.Execute then
      begin
        Result := True;
      end;

  finally
      enc.Free;
  end;
end;

end. 
 
 
