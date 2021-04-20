{ ****************************************************************************** }
{ * memory Rasterization JPEG support                                          * }
{ * by QQ 600585@qq.com                                                        * }
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
unit Raster_JPEG;

{$INCLUDE zDefine.inc}

interface

uses
  CoreClasses, PascalStrings, DoStatusIO, MemoryStream64, MemoryRaster, Raster_JPEG_Image, Raster_JPEG_type, Raster_JPEG_MapIterator;

type
  // Use Performance to set the performance of the jpeg image when reading, that is,
  // for decompressing files. This property is not used for writing out files.
  // With jpBestSpeed, the DCT decompressing process uses a faster but less accurate method.
  // When loading half, quarter or 1/8 size, this performance setting is not used.
  TJpegPerformance = (jpBestQuality, jpBestSpeed);

  // TMemoryJpegRaster is a Delphi and fpc class, which can be used to load Jpeg files into TMemoryRaster.
  // It relays the Jpeg functionality in the non-Windows TJpegImage class to this TMemoryRaster
  TMemoryJpegRaster = class(TCoreClassObject)
  private
    // the temporary TMemoryRaster that can be either the full image or the tilesized bitmap when UseTiledDrawing is activated.
    FRaster: TMemoryRaster;
    FImage: TJpegImage;
    FUseTiledDrawing: boolean;
    function ImageCreateMap(var Iterator_: TMapIterator): TObject;
    procedure ImageUpdate(Sender: TObject);
{$IFDEF JPEG_Debug}
    procedure ImageDebug(Sender: TObject; WarnStyle: TWarnStyle; const Message_: TPascalString);
{$ENDIF JPEG_Debug}
    function GetPerformance: TJpegPerformance;
    procedure SetPerformance(const Value: TJpegPerformance);
    function GetGrayScale: boolean;
    procedure SetGrayScale(const Value: boolean);
    function GetCompressionQuality: TJpegQuality;
    procedure SetCompressionQuality(const Value: TJpegQuality);
    procedure SetUseTiledDrawing(const Value: boolean);
    function GetScale: TJpegScale;
    procedure SetScale(const Value: TJpegScale);
  protected
    // Assign this TJpegGraphic to Dest. The only valid type for Dest is TMemoryRaster.
    // The internal jpeg image will be loaded from the data stream at the correct scale, then assigned to the bitmap in Dest.
    function GetEmpty: boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetDataSize: int64;
    class function GetVersion: string;
  public
    constructor Create;
    destructor Destroy; override;

    // Use Assign to assign a TMemoryRaster or other TJpegGraphic to this graphic. If
    // Source is a TMemoryRaster, the TMemoryRaster is compressed to the internal jpeg image.
    // If Source is another TJpegGraphic, the data streams are copied and the internal Jpeg image is loaded from the data.
    // It is also possible to assign a TJpegGraphic to a TMemoryRaster, like this: MyBitmap.Assign(MyJpegGraphic)
    // In that case, the protected AssignTo method is called.
    procedure Assign(Source: TMemoryJpegRaster);
    procedure SetRaster(Source: PRGBArray; Source_width, Source_height: Integer); overload;
    procedure SetRaster(Source: TMemoryRaster); overload;
    procedure GetRaster(Dest: TMemoryRaster);

    // Load a Jpeg graphic from the stream in Stream. Stream can be any stream type,
    // as long as the size of the stream is known in advance. The stream should only contain *one* Jpeg graphic.
    procedure LoadFromStream(Stream: TMemoryStream64);
    procedure LoadFromFile(const FileName_: string);

    // In case of LoadOption [loTileMode] is included, after the LoadFromStream,
    // individual tile blocks can be loaded which will be put in the resulting bitmap.
    // The tile loaded will contain all the MCU blocks that fall within the specified bounds Left_/Top_/Right_/Bottom_.
    // Note that these are var parameters, after calling this procedure they will be updated to the MCU block borders.
    // Left_/Top_ can subsequently be used to draw the resulting TJpegFormat.Bitmap to a canvas.
    procedure LoadTileBlock(var Left_, Top_, Right_, Bottom_: Integer);

    // Save a Jpeg graphic to the stream in Stream. Stream can be any stream type
    // as long as the size of the stream is known in advance.
    procedure SaveToStream(Stream: TMemoryStream64);
    procedure SaveToFile(const FileName_: string);
    property Performance: TJpegPerformance read GetPerformance write SetPerformance;

    // Downsizing scale when loading. When downsizing,
    // the Jpeg compressor uses less memory and processing power to decode the DCT coefficients.
    // jsFull is the 100% scale. jsDiv2 is 50% scale, jsDiv4 is 25% scale and jsDiv8 is 12.5% scale (aka 1/8).
    property Scale: TJpegScale read GetScale write SetScale;
    property GrayScale: boolean read GetGrayScale write SetGrayScale;
    property CompressionQuality: TJpegQuality read GetCompressionQuality write SetCompressionQuality;

    // When UseTiledDrawing is activated, the Jpeg graphic gets drawn by separate small tiled bitmaps when using TJpegGraphic.Draw
    // Only baseline jpeg images can use tiled drawing, so activating this setting takes no effect in other compression methods.
    // The default tile size is 256x256 pixels.
    property UseTiledDrawing: boolean read FUseTiledDrawing write SetUseTiledDrawing;

    // Version returns the current version of the NativeJpeg library.
    property Version: string read GetVersion;
    // size in bytes of the data in the Jpeg
    property DataSize: int64 read GetDataSize;
    // Access to TJpegImage
    property Image: TJpegImage read FImage;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

implementation

function SetBitmap32FromIterator(const Iterator_: TMapIterator): TMemoryRaster;
begin
  Result := NewRaster();
  Result.SetSize(Iterator_.Width, Iterator_.Height);
end;

procedure GetBitmap32Iterator(Raster_: TMemoryRaster; Iterator_: TMapIterator); overload;
begin
  Iterator_.Width := Raster_.Width;
  Iterator_.Height := Raster_.Height;
  if Raster_.Width * Raster_.Height = 0 then
      exit;
  Iterator_.Map := PByte(Raster_.ScanLine[0]);
  if Iterator_.Height > 1 then
      Iterator_.ScanStride := NativeUInt(Raster_.ScanLine[1]) - NativeUInt(Raster_.ScanLine[0])
  else
      Iterator_.ScanStride := 0;

  Iterator_.CellStride := 4;
  Iterator_.BitCount := 32;
end;

procedure GetBitmap32Iterator(Raster_: PRGBArray; Width, Height: Integer; Iterator_: TMapIterator); overload;
begin
  Iterator_.Width := Width;
  Iterator_.Height := Height;
  if Height * Height = 0 then
      exit;
  Iterator_.Map := PByte(Raster_);
  if Iterator_.Height > 1 then
      Iterator_.ScanStride := Width * 3
  else
      Iterator_.ScanStride := 0;

  Iterator_.CellStride := 3;
  Iterator_.BitCount := 24;
end;

function TMemoryJpegRaster.ImageCreateMap(var Iterator_: TMapIterator): TObject;
begin
{$IFDEF JPEG_Debug}
  ImageDebug(Self, wsInfo, PFormat('create TMemoryRaster x=%d y=%d', [Iterator_.Width, Iterator_.Height]));
{$ENDIF JPEG_Debug}
  // create a bitmap with iterator size and pixelformat
  if (FRaster = nil) or (FRaster.Empty) or (FRaster.Width <> Iterator_.Width) or (FRaster.Height <> Iterator_.Height) then
    begin
      // create a new bitmap with iterator size and pixelformat
      DisposeObject(FRaster);
      FRaster := nil;
      FRaster := SetBitmap32FromIterator(Iterator_);
      FRaster.Clear(RColor(0, 0, 0, $FF));
    end;

  // also update the iterator with bitmap properties
  GetBitmap32Iterator(FRaster, Iterator_);

{$IFDEF JPEG_Debug}
  ImageDebug(Self, wsInfo, PFormat('Iterator_ bitmap scanstride=%d', [Iterator_.ScanStride]));
{$ENDIF JPEG_Debug}
  Result := FRaster;
end;

procedure TMemoryJpegRaster.ImageUpdate(Sender: TObject);
begin
  // this update comes from the TJpegImage subcomponent.
  // We must free the bitmap so TJpegImage can create a new one
  DisposeObject(FRaster);
  FRaster := nil;
end;

{$IFDEF JPEG_Debug}


procedure TMemoryJpegRaster.ImageDebug(Sender: TObject; WarnStyle: TWarnStyle; const Message_: TPascalString);
begin
  DoStatus('%s [%s] - %s', [cWarnStyleNames[WarnStyle], Sender.ClassName, Message_.Text]);
end;
{$ENDIF JPEG_Debug}


function TMemoryJpegRaster.GetPerformance: TJpegPerformance;
begin
  if FImage.DCTCodingMethod = dmFast then
      Result := jpBestSpeed
  else
      Result := jpBestQuality;
end;

procedure TMemoryJpegRaster.SetPerformance(const Value: TJpegPerformance);
begin
  case Value of
    jpBestSpeed: FImage.DCTCodingMethod := dmFast;
    jpBestQuality: FImage.DCTCodingMethod := dmAccurate;
  end;
end;

function TMemoryJpegRaster.GetGrayScale: boolean;
begin
  Result := FImage.BitmapCS = jcGray;
end;

procedure TMemoryJpegRaster.SetGrayScale(const Value: boolean);
begin
  if Value then
      FImage.BitmapCS := jcGray
  else
      FImage.BitmapCS := jcRGB;
end;

function TMemoryJpegRaster.GetCompressionQuality: TJpegQuality;
begin
  Result := FImage.SaveOptions.Quality;
end;

procedure TMemoryJpegRaster.SetCompressionQuality(const Value: TJpegQuality);
begin
  FImage.SaveOptions.Quality := Value;
end;

procedure TMemoryJpegRaster.SetUseTiledDrawing(const Value: boolean);
begin
  FUseTiledDrawing := Value;
  if FUseTiledDrawing then
      FImage.LoadOptions := FImage.LoadOptions + [loTileMode]
  else
      FImage.LoadOptions := FImage.LoadOptions - [loTileMode]
end;

function TMemoryJpegRaster.GetScale: TJpegScale;
begin
  Result := FImage.LoadScale;
end;

procedure TMemoryJpegRaster.SetScale(const Value: TJpegScale);
begin
  FImage.LoadScale := Value;
end;

function TMemoryJpegRaster.GetEmpty: boolean;
begin
  Result := not FImage.HasBitmap;
end;

function TMemoryJpegRaster.GetHeight: Integer;
begin
  Result := FImage.Height;
end;

function TMemoryJpegRaster.GetWidth: Integer;
begin
  Result := FImage.Width;
end;

function TMemoryJpegRaster.GetDataSize: int64;
begin
  Result := FImage.DataSize;
end;

class function TMemoryJpegRaster.GetVersion: string;
begin
  Result := cNativeJpgVersion;
end;

constructor TMemoryJpegRaster.Create;
begin
  inherited;
  FImage := TJpegImage.Create(nil);
  FImage.OnUpdate := {$IFDEF FPC}@{$ENDIF FPC}ImageUpdate;
{$IFDEF JPEG_Debug}
  FImage.OnDebugOut := {$IFDEF FPC}@{$ENDIF FPC}ImageDebug;
{$ENDIF JPEG_Debug}
  FImage.OnCreateMap := {$IFDEF FPC}@{$ENDIF FPC}ImageCreateMap;
  FImage.DCTCodingMethod := dmFast;
  FUseTiledDrawing := False;
end;

destructor TMemoryJpegRaster.Destroy;
begin
  DisposeObject(FImage);
  FImage := nil;
  DisposeObject(FRaster);
  FRaster := nil;
  inherited;
end;

procedure TMemoryJpegRaster.Assign(Source: TMemoryJpegRaster);
var
  MS: TMemoryStream64;
begin
  MS := TMemoryStream64.CustomCreate(512 * 1024);
  try
    TMemoryJpegRaster(Source).SaveToStream(MS);
    MS.Position := 0;
    FImage.LoadFromStream(MS);
  finally
      MS.Free;
  end;
  // Load the default OnCreateMap event for TJpegGraphic
  FImage.OnCreateMap := {$IFDEF FPC}@{$ENDIF FPC}ImageCreateMap;
end;

procedure TMemoryJpegRaster.SetRaster(Source: PRGBArray; Source_width, Source_height: Integer);
var
  BitmapIter: TMapIterator;
begin
  // lightweight map iterator
  BitmapIter := TMapIterator.Create;
  try
    GetBitmap32Iterator(Source, Source_width, Source_height, BitmapIter);
{$IFDEF JPEG_Debug}
    ImageDebug(Self, wsHint, PFormat('bitmap scanstride=%d', [BitmapIter.ScanStride]));
{$ENDIF JPEG_Debug}
    // Clear the image first
    FImage.Clear;

    // You can change the quality of the compression (and thus the size of the Jpeg) by changing this:
    // FImage.SaveOptions.Quality := 95;

    // compress the image
    FImage.Compress(BitmapIter);

    // Save the Jpeg image based on the bitmap iterator
    FImage.SaveJpeg;
  finally
      BitmapIter.Free;
  end;
  // Reload the image
  FImage.Reload;
end;

procedure TMemoryJpegRaster.SetRaster(Source: TMemoryRaster);
var
  BitmapIter: TMapIterator;
begin
  // lightweight map iterator
  BitmapIter := TMapIterator.Create;
  try
    GetBitmap32Iterator(Source, BitmapIter);
{$IFDEF JPEG_Debug}
    ImageDebug(Self, wsHint, PFormat('bitmap scanstride=%d', [BitmapIter.ScanStride]));
{$ENDIF JPEG_Debug}
    // Clear the image first
    FImage.Clear;

    // You can change the quality of the compression (and thus the size of the Jpeg) by changing this:
    // FImage.SaveOptions.Quality := 95;

    // compress the image
    FImage.Compress(BitmapIter);

    // Save the Jpeg image based on the bitmap iterator
    FImage.SaveJpeg;
  finally
      BitmapIter.Free;
  end;
  // Reload the image
  FImage.Reload;
end;

procedure TMemoryJpegRaster.GetRaster(Dest: TMemoryRaster);
begin
  // the LoadLJpeg method will create the FRaster thru ImageCreateMap
  Image.LoadJpeg(Scale, True);
  Dest.SetWorkMemory(True, FRaster);
  DisposeObject(FRaster);
  FRaster := nil;
end;

procedure TMemoryJpegRaster.LoadFromStream(Stream: TMemoryStream64);
begin
  FImage.LoadFromStream(Stream);
end;

procedure TMemoryJpegRaster.LoadFromFile(const FileName_: string);
begin
  FImage.LoadFromFile(FileName_);
end;

procedure TMemoryJpegRaster.LoadTileBlock(var Left_, Top_, Right_, Bottom_: Integer);
begin
  // relay to FImage
  FImage.LoadTileBlock(Left_, Top_, Right_, Bottom_);
end;

procedure TMemoryJpegRaster.SaveToStream(Stream: TMemoryStream64);
begin
  FImage.SaveToStream(Stream);
end;

procedure TMemoryJpegRaster.SaveToFile(const FileName_: string);
begin
  FImage.SaveToFile(FileName_);
end;

end.
