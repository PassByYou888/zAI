unit zDrawEngineInterface_VCL;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.ExtCtrls,
  Vcl.Imaging.jpeg, Vcl.Imaging.pngimage,

  CoreClasses, PascalStrings, MemoryStream64,
  zDrawEngine, UnicodeMixedLib, Geometry2DUnit, MemoryRaster;

type
  TDrawEngineInterface_VCL = class(TDrawEngine_Raster)
  public
    BindBitmap: TBitmap;
    constructor Create; override;
    destructor Destroy; override;
    procedure Flush; override;
  end;

procedure MemoryBitmapToBitmap(mr: TMemoryRaster; bmp: TBitmap);
procedure BitmapToMemoryBitmap(bmp: TBitmap; mr: TMemoryRaster);

implementation

constructor TDrawEngineInterface_VCL.Create;
begin
  inherited Create;
  BindBitmap := TBitmap.Create;
end;

destructor TDrawEngineInterface_VCL.Destroy;
begin
  DisposeObject(BindBitmap);
  inherited Destroy;
end;

procedure TDrawEngineInterface_VCL.Flush;
begin
  inherited Flush;
  MemoryBitmapToBitmap(Memory, BindBitmap);
end;

procedure MemoryBitmapToBitmap(mr: TMemoryRaster; bmp: TBitmap);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  mr.SaveToBmp24Stream(m64);
  m64.Position := 0;
  bmp.LoadFromStream(m64);
  DisposeObject(m64);
end;

procedure BitmapToMemoryBitmap(bmp: TBitmap; mr: TMemoryRaster);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  bmp.SaveToStream(m64);
  m64.Position := 0;
  mr.LoadFromStream(m64);
  DisposeObject(m64);
end;

function _NewRaster: TMemoryRaster;
begin
  Result := TRaster.Create;
end;

function _NewRasterFromFile(const fn: string): TMemoryRaster;
var
  pic: TPicture;
  bmp: TBitmap;
begin
  if not TMemoryRaster.CanLoadFile(fn) then
    begin
      pic := TPicture.Create;
      pic.LoadFromFile(fn);
      bmp := TBitmap.Create;
      bmp.Assign(pic.Graphic);
      Result := NewRaster();
      BitmapToMemoryBitmap(bmp, Result);
      DisposeObject([pic, bmp]);
    end
  else
    begin
      Result := NewRaster();
      Result.LoadFromFile(fn);
    end;
end;

function _NewRasterFromStream(const stream: TCoreClassStream): TMemoryRaster;
var
  pic: TPicture;
  bmp: TBitmap;
begin
  if not TMemoryRaster.CanLoadStream(stream) then
    begin
      pic := TPicture.Create;
      pic.LoadFromStream(stream);
      bmp := TBitmap.Create;
      bmp.PixelFormat := TPixelFormat.pf32bit;
      bmp.Assign(pic.Graphic);
      Result := NewRaster();
      BitmapToMemoryBitmap(bmp, Result);
      DisposeObject([pic, bmp]);
    end
  else
    begin
      Result := NewRaster();
      Result.LoadFromStream(stream);
    end;
end;

procedure _SaveRaster(b: TMemoryRaster; const f: string);
begin
  if umlMultipleMatch(['*.bmp'], f) then
      b.SaveToFile(f)
  else if umlMultipleMatch(['*.seq'], f) then
      b.SaveToZLibCompressFile(f)
  else if umlMultipleMatch(['*.yv12'], f) then
      b.SaveToYV12File(f)
  else if umlMultipleMatch(['*.jls'], f) then
      b.SaveToJpegLS3File(f)
  else
      b.SaveToFile(f);
end;

initialization

NewRaster := _NewRaster;
NewRasterFromFile := _NewRasterFromFile;
NewRasterFromStream := _NewRasterFromStream;
SaveRaster := _SaveRaster;

end.
