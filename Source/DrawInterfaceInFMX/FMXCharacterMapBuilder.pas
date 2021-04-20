{ ****************************************************************************** }
{ * FMX canvas Character to Ratermization                                      * }
{ * written by QQ 600585@qq.com                                                * }
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
unit FMXCharacterMapBuilder;

{$INCLUDE ..\zDefine.inc}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics,

  CoreClasses, ListEngine,
  ObjectData, ObjectDataManager, ItemStream, zExpression,
  MemoryStream64, MemoryRaster, Geometry2DUnit, PascalStrings, UPascalStrings,
  UnicodeMixedLib, zDrawEngine, zDrawEngineInterface_SlowFMX;

function BuildFMXCharacterAsFontRaster(AA_: Boolean; fontName_: TUPascalString; fontSize_: Single; Bold_, Italic_: Boolean; InputBuff: TUArrayChar): TFontRaster;

implementation

type
  TFMXFontToRasterFactory = class
  protected
    bmp: FMX.Graphics.TBitmap;
    dIntf: TDrawEngineInterface_FMX;
    d: TDrawEngine;
    fontSize: Integer;
  public
    constructor Create(fontName_: string; fontSize_: Integer; Bold_, Italic_: Boolean);
    destructor Destroy; override;
    function MakeCharRaster(C: string; var MinRect_: TRect): TMemoryRaster;
  end;

constructor TFMXFontToRasterFactory.Create(fontName_: string; fontSize_: Integer; Bold_, Italic_: Boolean);
begin
  inherited Create;
  fontSize := fontSize_;

  bmp := FMX.Graphics.TBitmap.Create;
  bmp.SetSize(fontSize_ * 2, fontSize_ * 2);

  bmp.Canvas.Font.Family := fontName_;
  bmp.Canvas.Font.Size := fontSize_;
  if Bold_ then
      bmp.Canvas.Font.Style := bmp.Canvas.Font.Style + [TFontStyle.fsBold]
  else
      bmp.Canvas.Font.Style := bmp.Canvas.Font.Style - [TFontStyle.fsBold];

  if Italic_ then
      bmp.Canvas.Font.Style := bmp.Canvas.Font.Style + [TFontStyle.fsItalic]
  else
      bmp.Canvas.Font.Style := bmp.Canvas.Font.Style - [TFontStyle.fsItalic];

  dIntf := TDrawEngineInterface_FMX.Create;
  dIntf.SetSurface(bmp.Canvas, bmp);
  d := TDrawEngine.Create;
  d.DrawInterface := dIntf;
  d.ViewOptions := [];
  d.SetSize;
end;

destructor TFMXFontToRasterFactory.Destroy;
begin
  disposeObject(d);
  disposeObject(dIntf);
  disposeObject(bmp);
  inherited Destroy;
end;

function TFMXFontToRasterFactory.MakeCharRaster(C: string; var MinRect_: TRect): TMemoryRaster;
var
  r4: TV2Rect4;
  raster: TMemoryRaster;
begin
  d.FillBox(d.ScreenRect, DEColor(0, 0, 0));
  d.Flush;
  r4 := d.DrawText(C, fontSize, d.ScreenRect, DEColor(1, 1, 1), True);
  d.Flush;
  raster := TMemoryRaster.Create;
  BitmapToMemoryBitmap(bmp, raster);
  MinRect_ := Rect2Rect(r4.BoundRect);
  Result := raster;
end;

function BuildFMXCharacterAsFontRaster(AA_: Boolean; fontName_: TUPascalString; fontSize_: Single; Bold_, Italic_: Boolean; InputBuff: TUArrayChar): TFontRaster;
var
  BmpFactory: TFMXFontToRasterFactory;
  fr: TFontRaster;
  i: Integer;
  C: USystemChar;
  tmp, raster: TMemoryRaster;
  R: TRect;
begin
  BmpFactory := TFMXFontToRasterFactory.Create(fontName_, round(if_(AA_, fontSize_ * 4, fontSize_)), Bold_, Italic_);
  fr := TFontRaster.Create;

  for i := 0 to length(InputBuff) - 1 do
    begin
      C := InputBuff[i];
      tmp := BmpFactory.MakeCharRaster(InputBuff[i], R);
      if AA_ then
        begin
          Antialias32(tmp);
          tmp.Scale(1 / 4);
          R := Rect2Rect(RectMul(RectV2(R), 1 / 4));
          R := CalibrationRectInRect(R, tmp.BoundsRect0);
        end;
      raster := NewRaster();
      raster.SetSize(R.Width, R.Height, RColorF(0, 0, 0));
      tmp.DrawTo(raster, 0, 0, R);

      fr.Add(C, raster);

      disposeObject(tmp);
    end;
  disposeObject(BmpFactory);
  Result := fr;
end;

end.
