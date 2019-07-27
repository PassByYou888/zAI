{ ****************************************************************************** }
{ * AI Editor Common format (platform compatible)                              * }
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
unit zAI_Editor_Common;

{$INCLUDE zDefine.inc}

interface

uses Types,

{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  CoreClasses, PascalStrings, MemoryRaster, MemoryStream64, DoStatusIO, DataFrameEngine,
  Cadencer, ListEngine, TextDataEngine, NotifyObjectBase, TextParsing, zExpression, OpCode,
  UnicodeMixedLib, Geometry2DUnit, Geometry3DUnit, zDrawEngine, zAI, zAI_Common;

type
  TEditorDetectorDefine = class;
  TEditorImageData = class;
  TEditorGeometry = class;
  TEditorSegmentationMask = class;

  TEditorGeometryList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TEditorGeometry>;
  TEditorSegmentationMaskList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TEditorSegmentationMask>;
  TEditorImageDataList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TEditorImageData>;
  TEditorDetectorDefineList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TEditorDetectorDefine>;
  TEditorDetectorDefinePartList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PVec2>;

  TEditorDetectorDefine = class
  protected
    FOP_RT_RunDeleted: Boolean;
  public
    Owner: TEditorImageData;
    R: TRect;
    Token: SystemString;
    Part: TVec2List;
    PrepareRaster: TDETexture;

    constructor Create(AOwner: TEditorImageData);
    destructor Destroy; override;

    procedure SaveToStream(stream: TMemoryStream64); overload;
    procedure SaveToStream(stream: TMemoryStream64; RasterSave_: TRasterSave); overload;
    procedure LoadFromStream(stream: TMemoryStream64);
  end;

  TEditorGeometry = class(T2DPolygonGraph)
  public
    Owner: TEditorImageData;
    Token: SystemString;
    constructor Create;
    destructor Destroy; override;
  end;

  TEditorGeometryList = class(TEditorGeometryList_Decl)
  public
    Owner: TEditorImageData;
    constructor Create;
    procedure SaveToStream(stream: TMemoryStream64);
    procedure LoadFromStream(stream: TMemoryStream64);
  end;

  TEditorSegmentationMask = class
  public
    Owner: TEditorImageData;
    BGColor, FGColor: TRColor;
    Token: SystemString;
    PickedPoint: TPoint;
    Raster: TMemoryRaster;
    FromGeometry: Boolean;
    FromSegmentationMaskImage: Boolean;

    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(stream: TMemoryStream64);
    procedure LoadFromStream(stream: TMemoryStream64);
  end;

  TEditorSegmentationMaskList = class(TEditorSegmentationMaskList_Decl)
  public
    Owner: TEditorImageData;

    constructor Create;
    procedure SaveToStream(stream: TMemoryStream64);
    procedure LoadFromStream(stream: TMemoryStream64);

    procedure SaveToStream_AI(stream: TMemoryStream64);
    procedure LoadFromStream_AI(stream: TMemoryStream64);

    function BuildNewSegmentationMask(mr: TMemoryRaster; sampler_FG_Color, buildBG_color, buildFG_color: TRColor): TEditorSegmentationMask; overload;
    function BuildNewSegmentationMask(geo: TEditorGeometry; buildBG_color, buildFG_color: TRColor): TEditorSegmentationMask; overload;
    procedure RebuildGeometrySegmentationMask(buildBG_color, buildFG_color: TRColor);
  end;

  TEditorImageData = class
  private
    FOP_RT: TOpCustomRunTime;
    FOP_RT_RunDeleted: Boolean;
    // register op
    procedure CheckAndRegOPRT;
    // condition on image
    function OP_Image_GetWidth(var Param: TOpParam): Variant;
    function OP_Image_GetHeight(var Param: TOpParam): Variant;
    function OP_Image_GetDetector(var Param: TOpParam): Variant;
    function OP_Image_GetGeometry(var Param: TOpParam): Variant;
    function OP_Image_GetSegmentation(var Param: TOpParam): Variant;
    // condition on detector
    function OP_Detector_GetLabel(var Param: TOpParam): Variant;
    // process on image
    function OP_Image_Delete(var Param: TOpParam): Variant;
    function OP_Image_Scale(var Param: TOpParam): Variant;
    function OP_Image_SwapRB(var Param: TOpParam): Variant;
    function OP_Image_Gray(var Param: TOpParam): Variant;
    function OP_Image_Sharpen(var Param: TOpParam): Variant;
    function OP_Image_HistogramEqualize(var Param: TOpParam): Variant;
    function OP_Image_RemoveRedEyes(var Param: TOpParam): Variant;
    function OP_Image_Sepia(var Param: TOpParam): Variant;
    function OP_Image_Blur(var Param: TOpParam): Variant;
    function OP_Image_CalibrateRotate(var Param: TOpParam): Variant;
    // set all token
    function OP_Detector_SetLabel(var Param: TOpParam): Variant;
    // process on detector
    function OP_Detector_ClearDetector(var Param: TOpParam): Variant;
    function OP_Detector_DeleteDetector(var Param: TOpParam): Variant;
    // process on geometry
    function OP_Geometry_ClearGeometry(var Param: TOpParam): Variant;
    // process on Segmentation mask
    function OP_SegmentationMask_ClearSegmentationMask(var Param: TOpParam): Variant;
  public
    DetectorDefineList: TEditorDetectorDefineList;
    FileName: SystemString;
    Raster: TDETexture;
    RasterDrawRect: TRectV2;
    GeometryList: TEditorGeometryList;
    SegmentationMaskList: TEditorSegmentationMaskList;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function RunExpCondition(ScriptStyle: TTextStyle; exp: SystemString): Boolean;
    function RunExpProcess(ScriptStyle: TTextStyle; exp: SystemString): Boolean;
    function GetExpFunctionList: TPascalStringList;

    function AbsToLocalPt(pt: TVec2): TPoint;
    function AbsToLocal(pt: TVec2): TVec2;

    function LocalPtToAbs(pt: TPoint): TVec2;
    function LocalToAbs(pt: TVec2): TVec2;

    function GetTokenCount(Token: TPascalString): Integer;
    procedure Scale(f: TGeoFloat);

    procedure SaveToStream_AI(stream: TMemoryStream64); overload;
    procedure SaveToStream_AI(stream: TMemoryStream64; RasterSave_: TRasterSave); overload;
    procedure LoadFromStream_AI(stream: TMemoryStream64);

    procedure SaveToStream(stream: TMemoryStream64; SaveImg: Boolean);
    procedure LoadFromStream(stream: TMemoryStream64);

    procedure ProcessDetectorAlignment(Alignment: zAI.TAlignment);
    procedure ProcessSegmentationAlignment(Alignment: zAI.TAlignment_SS);
  end;

  TEditorImageDataList = class(TEditorImageDataList_Decl)
  public
    FreeImgData: Boolean;

    constructor Create(const FreeImgData_: Boolean);
    destructor Destroy; override;

    procedure RunScript(ScriptStyle: TTextStyle; condition_exp, process_exp: SystemString); overload;
    procedure RunScript(condition_exp, process_exp: SystemString); overload;

    function GetDetectorTokenList(filter: U_String): TPascalStringList;
    function GetGeometryTokenList(filter: U_String): TPascalStringList;
    function GetSegmentationMaskTokenList(filter: U_String): TPascalStringList;

    // save as .ai_set format
    procedure SaveToStream(stream: TCoreClassStream; const Scale: TGeoFloat; const pt_: TVec2; SaveImg: Boolean);
    // load from .ai_set format
    procedure LoadFromStream(stream: TCoreClassStream; var Scale: TGeoFloat; var pt_: TVec2); overload;

    procedure LoadFromStream(stream: TCoreClassStream); overload;
    procedure LoadFromFile(FileName: U_String); overload;

    // export as .imgDataset (from zAI_Common.pas) format support
    procedure SaveToStream_AI(stream: TCoreClassStream; RasterSaveMode: TRasterSave);
    // import from .imgDataset (from zAI_Common.pas) format
    procedure LoadFromStream_AI(stream: TCoreClassStream);
  end;

  TEditor_Image_Script_Register = procedure(Sender: TEditorImageData; opRT: TOpCustomRunTime) of object;

var
  On_Editor_Script_RegisterProc: TEditor_Image_Script_Register;

implementation

uses Math;

constructor TEditorDetectorDefine.Create(AOwner: TEditorImageData);
begin
  inherited Create;
  Owner := AOwner;
  R := Types.Rect(0, 0, 0, 0);
  Token := '';
  Part := TVec2List.Create;
  PrepareRaster := TDrawEngine.NewTexture;

  FOP_RT_RunDeleted := False;
end;

destructor TEditorDetectorDefine.Destroy;
begin
  DisposeObject(Part);
  DisposeObject(PrepareRaster);
  inherited Destroy;
end;

procedure TEditorDetectorDefine.SaveToStream(stream: TMemoryStream64);
begin
  SaveToStream(stream, TRasterSave.rsRGB);
end;

procedure TEditorDetectorDefine.SaveToStream(stream: TMemoryStream64; RasterSave_: TRasterSave);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
begin
  de := TDataFrameEngine.Create;
  de.WriteRect(R);
  de.WriteString(Token);

  m64 := TMemoryStream64.CustomCreate(8192);
  Part.SaveToStream(m64);
  de.WriteStream(m64);
  DisposeObject(m64);

  m64 := TMemoryStream64.CustomCreate(8192);
  if not PrepareRaster.Empty then
      PrepareRaster.SaveToStream(m64, RasterSave_);
  de.WriteStream(m64);
  DisposeObject(m64);

  de.EncodeTo(stream, True);

  DisposeObject(de);
end;

procedure TEditorDetectorDefine.LoadFromStream(stream: TMemoryStream64);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
begin
  Part.Clear;
  PrepareRaster.Reset;
  try
    de := TDataFrameEngine.Create;
    de.DecodeFrom(stream);
    R := de.Reader.ReadRect;
    Token := de.Reader.ReadString;

    m64 := TMemoryStream64.CustomCreate(8192);
    de.Reader.ReadStream(m64);
    m64.Position := 0;
    Part.Clear;
    Part.LoadFromStream(m64);
    DisposeObject(m64);

    m64 := TMemoryStream64.CustomCreate(8192);
    de.Reader.ReadStream(m64);
    if m64.Size > 0 then
      begin
        m64.Position := 0;
        PrepareRaster.LoadFromStream(m64);
      end;
    DisposeObject(m64);

    DisposeObject(de);
  except
  end;
end;

constructor TEditorGeometry.Create;
begin
  inherited Create;
  Owner := nil;
  Token := '';
end;

destructor TEditorGeometry.Destroy;
begin
  Token := '';
  inherited Destroy;
end;

constructor TEditorGeometryList.Create;
begin
  inherited Create;
  Owner := nil;
end;

procedure TEditorGeometryList.SaveToStream(stream: TMemoryStream64);
var
  d, nd: TDataFrameEngine;
  i: Integer;
  geo: TEditorGeometry;
  m64: TMemoryStream64;
begin
  d := TDataFrameEngine.Create;

  for i := 0 to count - 1 do
    begin
      nd := TDataFrameEngine.Create;
      geo := Items[i];
      nd.WriteString(geo.Token);
      m64 := TMemoryStream64.Create;
      geo.SaveToStream(m64);
      nd.WriteStream(m64);
      DisposeObject(m64);
      d.WriteDataFrame(nd);
      DisposeObject(nd);
    end;

  d.EncodeTo(stream, True);
  DisposeObject(d);
end;

procedure TEditorGeometryList.LoadFromStream(stream: TMemoryStream64);
var
  d, nd: TDataFrameEngine;
  i: Integer;
  geo: TEditorGeometry;
  m64: TMemoryStream64;
begin
  d := TDataFrameEngine.Create;
  d.DecodeFrom(stream, True);

  while d.Reader.NotEnd do
    begin
      nd := TDataFrameEngine.Create;
      d.Reader.ReadDataFrame(nd);

      geo := TEditorGeometry.Create;
      geo.Owner := Owner;
      geo.Token := nd.Reader.ReadString;

      m64 := TMemoryStream64.Create;
      nd.Reader.ReadStream(m64);
      m64.Position := 0;
      geo.LoadFromStream(m64);
      Add(geo);

      DisposeObject(m64);
      DisposeObject(nd);
    end;

  DisposeObject(d);
end;

constructor TEditorSegmentationMask.Create;
begin
  inherited Create;
  Owner := nil;
  BGColor := 0;
  FGColor := 0;
  Token := '';
  PickedPoint := Point(0, 0);
  Raster := NewRaster();
  FromGeometry := False;
  FromSegmentationMaskImage := False;
end;

destructor TEditorSegmentationMask.Destroy;
begin
  DisposeObject(Raster);
  inherited Destroy;
end;

procedure TEditorSegmentationMask.SaveToStream(stream: TMemoryStream64);
var
  d: TDataFrameEngine;
  m64: TMemoryStream64;
begin
  d := TDataFrameEngine.Create;

  d.WriteCardinal(BGColor);
  d.WriteCardinal(FGColor);
  d.WriteString(Token);
  d.WritePoint(PickedPoint);
  d.WriteBool(FromGeometry);
  d.WriteBool(FromSegmentationMaskImage);

  m64 := TMemoryStream64.CustomCreate(1024 * 128);
  Raster.SaveToBmp32Stream(m64);
  d.WriteStream(m64);
  DisposeObject(m64);

  d.EncodeAsZLib(stream, True);

  DisposeObject(d);
end;

procedure TEditorSegmentationMask.LoadFromStream(stream: TMemoryStream64);
var
  d: TDataFrameEngine;
  m64: TMemoryStream64;
begin
  d := TDataFrameEngine.Create;
  d.DecodeFrom(stream, True);

  BGColor := d.Reader.ReadCardinal;
  FGColor := d.Reader.ReadCardinal;
  Token := d.Reader.ReadString;
  PickedPoint := d.Reader.ReadPoint;
  FromGeometry := d.Reader.ReadBool;
  FromSegmentationMaskImage := d.Reader.ReadBool;

  m64 := TMemoryStream64.Create;
  d.Reader.ReadStream(m64);
  m64.Position := 0;
  Raster.LoadFromStream(m64);
  DisposeObject(m64);

  DisposeObject(d);
end;

constructor TEditorSegmentationMaskList.Create;
begin
  inherited Create;
  Owner := nil;
end;

procedure TEditorSegmentationMaskList.SaveToStream(stream: TMemoryStream64);
var
  d: TDataFrameEngine;
  i: Integer;
  SegmentationMask: TEditorSegmentationMask;
  m64: TMemoryStream64;
begin
  d := TDataFrameEngine.Create;

  for i := 0 to count - 1 do
    begin
      SegmentationMask := Items[i];
      m64 := TMemoryStream64.Create;
      SegmentationMask.SaveToStream(m64);
      d.WriteStream(m64);
      DisposeObject(m64);
    end;

  d.EncodeTo(stream, True);
  DisposeObject(d);
end;

procedure TEditorSegmentationMaskList.LoadFromStream(stream: TMemoryStream64);
var
  d: TDataFrameEngine;
  i: Integer;
  SegmentationMask: TEditorSegmentationMask;
  m64: TMemoryStream64;
begin
  d := TDataFrameEngine.Create;
  d.DecodeFrom(stream, True);

  while d.Reader.NotEnd do
    begin
      SegmentationMask := TEditorSegmentationMask.Create;
      SegmentationMask.Owner := Owner;

      m64 := TMemoryStream64.Create;
      d.Reader.ReadStream(m64);
      m64.Position := 0;
      SegmentationMask.LoadFromStream(m64);
      Add(SegmentationMask);
      DisposeObject(m64);
    end;

  DisposeObject(d);
end;

procedure TEditorSegmentationMaskList.SaveToStream_AI(stream: TMemoryStream64);
var
  d, nd: TDataFrameEngine;
  i: Integer;
  SegmentationMask: TEditorSegmentationMask;
  m64: TMemoryStream64;
begin
  d := TDataFrameEngine.Create;

  for i := 0 to count - 1 do
    begin
      // 0: bk color
      // 1: fg color
      // 2: name
      // 3: raster

      nd := TDataFrameEngine.Create;
      SegmentationMask := Items[i];

      nd.WriteCardinal(SegmentationMask.BGColor);
      nd.WriteCardinal(SegmentationMask.FGColor);
      nd.WriteString(SegmentationMask.Token);
      m64 := TMemoryStream64.CustomCreate(128 * 1024);
      SegmentationMask.Raster.SaveToBmp32Stream(m64);
      nd.WriteStream(m64);
      DisposeObject(m64);
      d.WriteDataFrameCompressed(nd);
      DisposeObject(nd);
    end;

  d.EncodeTo(stream, True);
  DisposeObject(d);
end;

procedure TEditorSegmentationMaskList.LoadFromStream_AI(stream: TMemoryStream64);
var
  d, nd: TDataFrameEngine;
  i: Integer;
  SegmentationMask: TEditorSegmentationMask;
  m64: TMemoryStream64;
begin
  d := TDataFrameEngine.Create;
  d.DecodeFrom(stream, True);

  while d.Reader.NotEnd do
    begin
      nd := TDataFrameEngine.Create;
      d.Reader.ReadDataFrame(nd);

      // 0: bk color
      // 1: fg color
      // 2: name
      // 3: raster
      SegmentationMask := TEditorSegmentationMask.Create;
      SegmentationMask.Owner := Owner;

      // base data
      SegmentationMask.BGColor := nd.Reader.ReadCardinal;
      SegmentationMask.FGColor := nd.Reader.ReadCardinal;
      SegmentationMask.Token := nd.Reader.ReadString;
      m64 := TMemoryStream64.Create;
      nd.Reader.ReadStream(m64);
      m64.Position := 0;
      SegmentationMask.Raster.LoadFromStream(m64);

      // fixed data
      SegmentationMask.FromGeometry := False;
      SegmentationMask.FromSegmentationMaskImage := True;
      SegmentationMask.PickedPoint := SegmentationMask.Raster.FindNearColor(SegmentationMask.FGColor, Owner.Raster.Centre);
      Add(SegmentationMask);

      DisposeObject(m64);
      DisposeObject(nd);
    end;

  DisposeObject(d);
end;

function TEditorSegmentationMaskList.BuildNewSegmentationMask(mr: TMemoryRaster; sampler_FG_Color, buildBG_color, buildFG_color: TRColor): TEditorSegmentationMask;
var
  i, j: Integer;
begin
  Result := nil;
  if (mr.Width <> Owner.Raster.Width) or (mr.Height <> Owner.Raster.Height) then
      exit;
  if not mr.ExistsColor(sampler_FG_Color) then
      exit;

  Result := TEditorSegmentationMask.Create;
  Result.Owner := Owner;
  Result.BGColor := buildBG_color;
  Result.FGColor := buildFG_color;
  Result.Token := '';
  Result.FromGeometry := False;
  Result.FromSegmentationMaskImage := True;
  Result.Raster.SetSize(Owner.Raster.Width, Owner.Raster.Height, buildBG_color);
  for j := 0 to mr.Height - 1 do
    for i := 0 to mr.Width - 1 do
      if (PointInRect(i, j, 0, 0, Result.Raster.Width, Result.Raster.Height)) and (mr.Pixel[i, j] = sampler_FG_Color) then
          Result.Raster.Pixel[i, j] := buildFG_color;
  Result.PickedPoint := Result.Raster.FindNearColor(buildFG_color, Owner.Raster.Centre);

  LockObject(Self);
  Add(Result);
  UnlockObject(Self);
end;

function TEditorSegmentationMaskList.BuildNewSegmentationMask(geo: TEditorGeometry; buildBG_color, buildFG_color: TRColor): TEditorSegmentationMask;
var
  i, j: Integer;
begin
  Result := TEditorSegmentationMask.Create;
  Result.Owner := Owner;
  Result.BGColor := buildBG_color;
  Result.FGColor := buildFG_color;
  Result.Token := geo.Token;
  Result.FromGeometry := True;
  Result.FromSegmentationMaskImage := False;
  Result.Raster.SetSize(Owner.Raster.Width, Owner.Raster.Height, buildBG_color);
  for j := 0 to Owner.Raster.Height - 1 do
    for i := 0 to Owner.Raster.Width - 1 do
      if geo.InHere(Vec2(i, j)) then
          Result.Raster.Pixel[i, j] := buildFG_color;
  Result.PickedPoint := Result.Raster.FindNearColor(buildFG_color, Owner.Raster.Centre);

  LockObject(Self);
  Add(Result);
  UnlockObject(Self);
end;

procedure TEditorSegmentationMaskList.RebuildGeometrySegmentationMask(buildBG_color, buildFG_color: TRColor);
var
  i: Integer;
begin
  // remove geometry data source
  i := 0;
  while i < count do
    begin
      if Items[i].FromGeometry then
        begin
          DisposeObject(Items[i]);
          Delete(i);
        end
      else
          inc(i);
    end;

  for i := 0 to Owner.GeometryList.count - 1 do
      BuildNewSegmentationMask(Owner.GeometryList[i], buildBG_color, buildFG_color);
end;

procedure TEditorImageData.CheckAndRegOPRT;
begin
  if FOP_RT <> nil then
      exit;
  FOP_RT := TOpCustomRunTime.Create;
  FOP_RT.UserObject := Self;

  // condition on image
  FOP_RT.RegOpM('Width', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetWidth);
  FOP_RT.RegOpM('Height', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetHeight);

  FOP_RT.RegOpM('Det', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetDetector);
  FOP_RT.RegOpM('Detector', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetDetector);
  FOP_RT.RegOpM('DetNum', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetDetector);

  FOP_RT.RegOpM('Geo', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetGeometry);
  FOP_RT.RegOpM('Geometry', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetGeometry);
  FOP_RT.RegOpM('GeoNum', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetGeometry);

  FOP_RT.RegOpM('Seg', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetSegmentation);
  FOP_RT.RegOpM('Segmentation', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetSegmentation);
  FOP_RT.RegOpM('SegNum', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetSegmentation);

  // condition on detector
  FOP_RT.RegOpM('Label', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_GetLabel);
  FOP_RT.RegOpM('GetLabel', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_GetLabel);

  // process on image
  FOP_RT.RegOpM('Delete', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Delete);

  FOP_RT.RegOpM('Scale', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Scale);
  FOP_RT.RegOpM('ReductMemory', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Scale);

  FOP_RT.RegOpM('SwapRB', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_SwapRB);
  FOP_RT.RegOpM('SwapBR', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_SwapRB);

  FOP_RT.RegOpM('Gray', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Gray);
  FOP_RT.RegOpM('Grayscale', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Gray);

  FOP_RT.RegOpM('Sharpen', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Sharpen);

  FOP_RT.RegOpM('HistogramEqualize', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_HistogramEqualize);
  FOP_RT.RegOpM('he', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_HistogramEqualize);
  FOP_RT.RegOpM('NiceColor', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_HistogramEqualize);

  FOP_RT.RegOpM('RemoveRedEye', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_RemoveRedEyes);
  FOP_RT.RegOpM('RemoveRedEyes', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_RemoveRedEyes);
  FOP_RT.RegOpM('RedEyes', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_RemoveRedEyes);
  FOP_RT.RegOpM('RedEye', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_RemoveRedEyes);

  FOP_RT.RegOpM('Sepia', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Sepia);
  FOP_RT.RegOpM('Blur', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Blur);

  FOP_RT.RegOpM('CalibrateRotate', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_CalibrateRotate);
  FOP_RT.RegOpM('DocumentAlignment', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_CalibrateRotate);
  FOP_RT.RegOpM('DocumentAlign', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_CalibrateRotate);
  FOP_RT.RegOpM('DocAlign', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_CalibrateRotate);
  FOP_RT.RegOpM('AlignDoc', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_CalibrateRotate);

  FOP_RT.RegOpM('SetLab', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_SetLabel);
  FOP_RT.RegOpM('SetLabel', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_SetLabel);
  FOP_RT.RegOpM('DefLab', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_SetLabel);
  FOP_RT.RegOpM('DefLabel', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_SetLabel);
  FOP_RT.RegOpM('DefineLabel', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_SetLabel);

  // process on detector
  FOP_RT.RegOpM('ClearDetector', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_ClearDetector);
  FOP_RT.RegOpM('ClearDet', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_ClearDetector);
  FOP_RT.RegOpM('KillDetector', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_ClearDetector);
  FOP_RT.RegOpM('KillDet', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_ClearDetector);
  FOP_RT.RegOpM('DeleteDetector', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_DeleteDetector);
  FOP_RT.RegOpM('DeleteRect', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_DeleteDetector);

  // process on geometry
  FOP_RT.RegOpM('ClearGeometry', {$IFDEF FPC}@{$ENDIF FPC}OP_Geometry_ClearGeometry);
  FOP_RT.RegOpM('ClearGeo', {$IFDEF FPC}@{$ENDIF FPC}OP_Geometry_ClearGeometry);
  FOP_RT.RegOpM('KillGeometry', {$IFDEF FPC}@{$ENDIF FPC}OP_Geometry_ClearGeometry);
  FOP_RT.RegOpM('KillGeo', {$IFDEF FPC}@{$ENDIF FPC}OP_Geometry_ClearGeometry);

  // process on Segmentation mask
  FOP_RT.RegOpM('ClearSegmentationMask', {$IFDEF FPC}@{$ENDIF FPC}OP_SegmentationMask_ClearSegmentationMask);
  FOP_RT.RegOpM('ClearSeg', {$IFDEF FPC}@{$ENDIF FPC}OP_SegmentationMask_ClearSegmentationMask);
  FOP_RT.RegOpM('KillSegmentationMask', {$IFDEF FPC}@{$ENDIF FPC}OP_SegmentationMask_ClearSegmentationMask);
  FOP_RT.RegOpM('KillSeg', {$IFDEF FPC}@{$ENDIF FPC}OP_SegmentationMask_ClearSegmentationMask);

  // external image processor
  if Assigned(On_Editor_Script_RegisterProc) then
      On_Editor_Script_RegisterProc(Self, FOP_RT);
end;

function TEditorImageData.OP_Image_GetWidth(var Param: TOpParam): Variant;
begin
  Result := Raster.Width;
end;

function TEditorImageData.OP_Image_GetHeight(var Param: TOpParam): Variant;
begin
  Result := Raster.Height;
end;

function TEditorImageData.OP_Image_GetDetector(var Param: TOpParam): Variant;
begin
  Result := DetectorDefineList.count;
end;

function TEditorImageData.OP_Image_GetGeometry(var Param: TOpParam): Variant;
begin
  Result := GeometryList.count;
end;

function TEditorImageData.OP_Image_GetSegmentation(var Param: TOpParam): Variant;
begin
  Result := SegmentationMaskList.count;
end;

function TEditorImageData.OP_Detector_GetLabel(var Param: TOpParam): Variant;
begin
  Result := GetTokenCount(Param[0]);
end;

function TEditorImageData.OP_Image_Delete(var Param: TOpParam): Variant;
begin
  FOP_RT_RunDeleted := True;
  Result := True;
end;

function TEditorImageData.OP_Image_Scale(var Param: TOpParam): Variant;
begin
  DoStatus('image script on scale %f', [TGeoFloat(Param[0])]);
  if not Raster.Empty then
    begin
      Scale(Param[0]);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;
  Result := True;
end;

function TEditorImageData.OP_Image_SwapRB(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on SwapRed-Blue');

  if not Raster.Empty then
    begin
      Raster.FormatBGRA;
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        DetectorDefineList[i].PrepareRaster.FormatBGRA;
  Result := True;
end;

function TEditorImageData.OP_Image_Gray(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on Grayscale');

  if not Raster.Empty then
    begin
      Raster.Grayscale;
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        DetectorDefineList[i].PrepareRaster.Grayscale;
  Result := True;
end;

function TEditorImageData.OP_Image_Sharpen(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on Sharpen');

  if not Raster.Empty then
    begin
      Sharpen(Raster, True);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        Sharpen(DetectorDefineList[i].PrepareRaster, True);
  Result := True;
end;

function TEditorImageData.OP_Image_HistogramEqualize(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on HistogramEqualize');

  if not Raster.Empty then
    begin
      HistogramEqualize(Raster);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        HistogramEqualize(DetectorDefineList[i].PrepareRaster);
  Result := True;
end;

function TEditorImageData.OP_Image_RemoveRedEyes(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on RemoveRedEyes');

  if not Raster.Empty then
    begin
      RemoveRedEyes(Raster);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        RemoveRedEyes(DetectorDefineList[i].PrepareRaster);
  Result := True;
end;

function TEditorImageData.OP_Image_Sepia(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on Sepia');

  if not Raster.Empty then
    begin
      Sepia32(Raster, Param[0]);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        Sepia32(DetectorDefineList[i].PrepareRaster, Param[0]);
  Result := True;
end;

function TEditorImageData.OP_Image_Blur(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on Sepia');

  if not Raster.Empty then
    begin
      GaussianBlur(Raster, Param[0], Raster.BoundsRect);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        GaussianBlur(DetectorDefineList[i].PrepareRaster, Param[0], DetectorDefineList[i].PrepareRaster.BoundsRect);
  Result := True;
end;

function TEditorImageData.OP_Image_CalibrateRotate(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on CalibrateRotate');

  if not Raster.Empty then
    begin
      Raster.CalibrateRotate;
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        DetectorDefineList[i].PrepareRaster.CalibrateRotate;
  Result := True;
end;

function TEditorImageData.OP_Detector_SetLabel(var Param: TOpParam): Variant;
var
  i: Integer;
  n: SystemString;
begin
  if length(Param) > 0 then
      n := Param[0]
  else
      n := '';
  DoStatus('image script on setLabel %s', [n]);
  for i := 0 to DetectorDefineList.count - 1 do
      DetectorDefineList[i].Token := n;
  for i := 0 to GeometryList.count - 1 do
      GeometryList[i].Token := n;
  for i := 0 to SegmentationMaskList.count - 1 do
      SegmentationMaskList[i].Token := n;
  Result := True;
end;

function TEditorImageData.OP_Detector_ClearDetector(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  for i := 0 to DetectorDefineList.count - 1 do
      DisposeObject(DetectorDefineList[i]);
  DetectorDefineList.Clear;
  Result := True;
end;

function TEditorImageData.OP_Detector_DeleteDetector(var Param: TOpParam): Variant;
type
  TDetArry = array of TEditorDetectorDefine;
var
  coord: TVec2;

  function ListSortCompare(Item1, Item2: TEditorDetectorDefine): TValueRelationship;
  var
    d1, d2: TGeoFloat;
  begin
    d1 := Vec2Distance(RectCentre(RectV2(Item1.R)), coord);
    d2 := Vec2Distance(RectCentre(RectV2(Item2.R)), coord);
    Result := CompareValue(d1, d2);
  end;

  procedure QuickSortList(var SortList: TDetArry; l, R: Integer);
  var
    i, j: Integer;
    p, t: TEditorDetectorDefine;
  begin
    repeat
      i := l;
      j := R;
      p := SortList[(l + R) shr 1];
      repeat
        while ListSortCompare(SortList[i], p) < 0 do
            inc(i);
        while ListSortCompare(SortList[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if l < j then
          QuickSortList(SortList, l, j);
      l := i;
    until i >= R;
  end;

var
  pt: TVec2;
  reversed_count: Integer;
  detArry: TDetArry;
  i: Integer;
  det: TEditorDetectorDefine;
begin
  if DetectorDefineList.count < 2 then
    begin
      Result := False;
      exit;
    end;

  if length(Param) <> 3 then
    begin
      DoStatus('DeleteDetector param error. exmples: DeleteDetector(1, 0.5, 0.5)');
      Result := False;
      exit;
    end;
  reversed_count := Param[0];
  pt[0] := Param[1];
  pt[1] := Param[2];
  coord := Vec2Mul(pt, Raster.Size2D);

  SetLength(detArry, DetectorDefineList.count);
  for i := 0 to DetectorDefineList.count - 1 do
      detArry[i] := DetectorDefineList[i];

  QuickSortList(detArry, 0, DetectorDefineList.count - 1);

  for i := reversed_count to length(detArry) - 1 do
    begin
      det := detArry[i];
      DetectorDefineList.Remove(det);
      DisposeObject(det);
    end;

  SetLength(detArry, 0);
  Result := True;
end;

function TEditorImageData.OP_Geometry_ClearGeometry(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  for i := 0 to GeometryList.count - 1 do
      DisposeObject(GeometryList[i]);
  GeometryList.Clear;
  Result := True;
end;

function TEditorImageData.OP_SegmentationMask_ClearSegmentationMask(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  for i := 0 to SegmentationMaskList.count - 1 do
      DisposeObject(SegmentationMaskList[i]);
  SegmentationMaskList.Clear;
  Result := True;
end;

constructor TEditorImageData.Create;
begin
  inherited Create;
  DetectorDefineList := TEditorDetectorDefineList.Create;
  FileName := '';
  Raster := TDrawEngine.NewTexture;
  RasterDrawRect := RectV2(0, 0, 0, 0);
  GeometryList := TEditorGeometryList.Create;
  GeometryList.Owner := Self;
  SegmentationMaskList := TEditorSegmentationMaskList.Create;
  SegmentationMaskList.Owner := Self;
  FOP_RT := nil;
  FOP_RT_RunDeleted := False;
end;

destructor TEditorImageData.Destroy;
begin
  Clear;
  DisposeObject(DetectorDefineList);
  DisposeObject(Raster);
  DisposeObject(GeometryList);
  DisposeObject(SegmentationMaskList);
  inherited Destroy;
end;

procedure TEditorImageData.Clear;
var
  i: Integer;
begin
  for i := 0 to DetectorDefineList.count - 1 do
      DisposeObject(DetectorDefineList[i]);
  DetectorDefineList.Clear;
  for i := 0 to GeometryList.count - 1 do
      DisposeObject(GeometryList[i]);
  GeometryList.Clear;
  for i := 0 to SegmentationMaskList.count - 1 do
      DisposeObject(SegmentationMaskList[i]);
  SegmentationMaskList.Clear;
end;

function TEditorImageData.RunExpCondition(ScriptStyle: TTextStyle; exp: SystemString): Boolean;
begin
  CheckAndRegOPRT;
  DoStatusNoLn('Image (%d * %d, detector:%d) EvaluateExpression: %s', [Raster.Width, Raster.Height, DetectorDefineList.count, exp]);

  Result := EvaluateExpressionValue(False, ScriptStyle, exp, FOP_RT);

  if Result then
      DoStatusNoLn(' = yes.')
  else
      DoStatusNoLn(' = no.');
  DoStatusNoLn;
end;

function TEditorImageData.RunExpProcess(ScriptStyle: TTextStyle; exp: SystemString): Boolean;
begin
  CheckAndRegOPRT;
  Result := EvaluateExpressionValue(False, ScriptStyle, exp, FOP_RT);
end;

function TEditorImageData.GetExpFunctionList: TPascalStringList;
begin
  CheckAndRegOPRT;
  Result := TPascalStringList.Create;
  FOP_RT.ProcList.GetNameList(Result);
end;

function TEditorImageData.AbsToLocalPt(pt: TVec2): TPoint;
var
  V: TVec2;
  X, Y: TGeoFloat;
begin
  V := Vec2Sub(pt, RasterDrawRect[0]);
  X := V[0] / RectWidth(RasterDrawRect);
  Y := V[1] / RectHeight(RasterDrawRect);
  Result.X := Round(Raster.Width * X);
  Result.Y := Round(Raster.Height * Y);
end;

function TEditorImageData.AbsToLocal(pt: TVec2): TVec2;
var
  V: TVec2;
  X, Y: TGeoFloat;
begin
  V := Vec2Sub(pt, RasterDrawRect[0]);
  X := V[0] / RectWidth(RasterDrawRect);
  Y := V[1] / RectHeight(RasterDrawRect);
  Result[0] := Raster.Width * X;
  Result[1] := Raster.Height * Y;
end;

function TEditorImageData.LocalPtToAbs(pt: TPoint): TVec2;
begin
  Result[0] := RasterDrawRect[0, 0] + pt.X / Raster.Width * RectWidth(RasterDrawRect);
  Result[1] := RasterDrawRect[0, 1] + pt.Y / Raster.Height * RectHeight(RasterDrawRect);
end;

function TEditorImageData.LocalToAbs(pt: TVec2): TVec2;
begin
  Result[0] := RasterDrawRect[0, 0] + pt[0] / Raster.Width * RectWidth(RasterDrawRect);
  Result[1] := RasterDrawRect[0, 1] + pt[1] / Raster.Height * RectHeight(RasterDrawRect);
end;

function TEditorImageData.GetTokenCount(Token: TPascalString): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to DetectorDefineList.count - 1 do
    if umlMultipleMatch(Token, DetectorDefineList[i].Token) then
        inc(Result);

  for i := 0 to GeometryList.count - 1 do
    if umlMultipleMatch(Token, GeometryList[i].Token) then
        inc(Result);

  for i := 0 to SegmentationMaskList.count - 1 do
    if umlMultipleMatch(Token, SegmentationMaskList[i].Token) then
        inc(Result);
end;

procedure TEditorImageData.Scale(f: TGeoFloat);
var
  i, j: Integer;
  DetDef: TEditorDetectorDefine;
begin
  if IsEqual(f, 1.0) then
      exit;

  Raster.Scale(f);

  for i := 0 to DetectorDefineList.count - 1 do
    begin
      DetDef := DetectorDefineList[i];
      DetDef.R := MakeRect(RectMul(RectV2(DetDef.R), f));
      DetDef.Part.Mul(f, f);
    end;

  for i := 0 to SegmentationMaskList.count - 1 do
      SegmentationMaskList[i].Raster.NoLineScale(f);

  for i := 0 to GeometryList.count - 1 do
      GeometryList[i].Scale(f);
end;

procedure TEditorImageData.SaveToStream_AI(stream: TMemoryStream64);
begin
  SaveToStream_AI(stream, TRasterSave.rsRGB);
end;

procedure TEditorImageData.SaveToStream_AI(stream: TMemoryStream64; RasterSave_: TRasterSave);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
  i: Integer;
  DetDef: TEditorDetectorDefine;
begin
  de := TDataFrameEngine.Create;

  m64 := TMemoryStream64.Create;
  Raster.SaveToStream(m64, RasterSave_);
  de.WriteStream(m64);
  DisposeObject(m64);

  de.WriteInteger(DetectorDefineList.count);

  for i := 0 to DetectorDefineList.count - 1 do
    begin
      m64 := TMemoryStream64.Create;
      DetDef := DetectorDefineList[i];
      DetDef.SaveToStream(m64);
      de.WriteStream(m64);
      DisposeObject(m64);
    end;

  m64 := TMemoryStream64.Create;
  SegmentationMaskList.SaveToStream_AI(m64);
  de.WriteStream(m64);
  DisposeObject(m64);

  de.EncodeTo(stream, True);

  DisposeObject(de);
end;

procedure TEditorImageData.LoadFromStream_AI(stream: TMemoryStream64);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
  i, c: Integer;
  DetDef: TEditorDetectorDefine;
begin
  de := TDataFrameEngine.Create;
  de.DecodeFrom(stream);

  m64 := TMemoryStream64.Create;
  de.Reader.ReadStream(m64);
  if (m64.Size > 0) then
    begin
      m64.Position := 0;
      Raster.LoadFromStream(m64);
    end;
  DisposeObject(m64);

  c := de.Reader.ReadInteger;

  for i := 0 to c - 1 do
    begin
      m64 := TMemoryStream64.Create;
      de.Reader.ReadStream(m64);
      m64.Position := 0;
      DetDef := TEditorDetectorDefine.Create(Self);
      DetDef.LoadFromStream(m64);
      DisposeObject(m64);
      DetectorDefineList.Add(DetDef);
    end;

  if de.Reader.NotEnd then
    begin
      m64 := TMemoryStream64.Create;
      de.Reader.ReadStream(m64);
      m64.Position := 0;
      SegmentationMaskList.LoadFromStream_AI(m64);
      DisposeObject(m64);
    end;

  DisposeObject(de);
end;

procedure TEditorImageData.SaveToStream(stream: TMemoryStream64; SaveImg: Boolean);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
  i: Integer;
  DetDef: TEditorDetectorDefine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(FileName);

  m64 := TMemoryStream64.Create;
  if SaveImg then
      Raster.SaveToBmp24Stream(m64);
  de.WriteStream(m64);
  DisposeObject(m64);

  de.WriteRectV2(RasterDrawRect);

  // detector define
  de.WriteInteger(DetectorDefineList.count);
  for i := 0 to DetectorDefineList.count - 1 do
    begin
      m64 := TMemoryStream64.Create;
      DetDef := DetectorDefineList[i];
      DetDef.SaveToStream(m64);
      de.WriteStream(m64);
      DisposeObject(m64);
    end;

  // geometry
  m64 := TMemoryStream64.Create;
  GeometryList.SaveToStream(m64);
  de.WriteStream(m64);
  DisposeObject(m64);

  // Segmentation mask
  m64 := TMemoryStream64.Create;
  SegmentationMaskList.SaveToStream(m64);
  de.WriteStream(m64);
  DisposeObject(m64);

  de.EncodeTo(stream, True);

  DisposeObject(de);
end;

procedure TEditorImageData.LoadFromStream(stream: TMemoryStream64);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
  i, c: Integer;
  DetDef: TEditorDetectorDefine;
begin
  de := TDataFrameEngine.Create;
  de.DecodeFrom(stream);

  FileName := de.Reader.ReadString;

  m64 := TMemoryStream64.Create;
  de.Reader.ReadStream(m64);
  if m64.Size > 0 then
    begin
      m64.Position := 0;
      Raster.LoadFromStream(m64);
    end;
  DisposeObject(m64);

  RasterDrawRect := de.Reader.ReadRectV2;

  // detector define
  c := de.Reader.ReadInteger;
  for i := 0 to c - 1 do
    begin
      m64 := TMemoryStream64.Create;
      de.Reader.ReadStream(m64);
      m64.Position := 0;
      DetDef := TEditorDetectorDefine.Create(Self);
      if m64.Size > 0 then
          DetDef.LoadFromStream(m64);
      DisposeObject(m64);
      DetectorDefineList.Add(DetDef);
    end;

  // Compatibility check zAI 1.16-1.19
  if de.Reader.NotEnd then
    begin
      // geometry
      m64 := TMemoryStream64.Create;
      de.Reader.ReadStream(m64);
      m64.Position := 0;
      GeometryList.LoadFromStream(m64);
      DisposeObject(m64);

      // Segmentation mask
      m64 := TMemoryStream64.Create;
      de.Reader.ReadStream(m64);
      m64.Position := 0;
      SegmentationMaskList.LoadFromStream(m64);
      DisposeObject(m64);
    end;

  DisposeObject(de);
end;

procedure TEditorImageData.ProcessDetectorAlignment(Alignment: zAI.TAlignment);
var
  ai_imgList: TAI_ImageList;
  ai_img: TAI_Image;
  ai_DetDef: TAI_DetectorDefine;

  editor_DetDef: TEditorDetectorDefine;

  i: Integer;
  m64: TMemoryStream64;
  needReset: Boolean;
begin
  ai_imgList := TAI_ImageList.Create;

  ai_img := TAI_Image.Create(ai_imgList);
  ai_imgList.Add(ai_img);
  ai_img.Raster.Assign(Raster);

  for i := 0 to DetectorDefineList.count - 1 do
    begin
      m64 := TMemoryStream64.CustomCreate(8192);
      DetectorDefineList[i].SaveToStream(m64);

      ai_DetDef := TAI_DetectorDefine.Create(ai_img);
      ai_img.DetectorDefineList.Add(ai_DetDef);
      m64.Position := 0;
      ai_DetDef.LoadFromStream(m64);
      DisposeObject(m64);
    end;

  Alignment.Alignment(ai_imgList);

  needReset :=
    (Alignment is TAlignment_Face)
    or (Alignment is TAlignment_FastFace)
    or (Alignment is TAlignment_OD)
    or (Alignment is TAlignment_FastOD)
    or (Alignment is TAlignment_MMOD)
    or (Alignment is TAlignment_FastMMOD)
    or (ai_img.DetectorDefineList.count <> DetectorDefineList.count);

  if needReset then
    begin
      // reset detector dataset
      for i := 0 to DetectorDefineList.count - 1 do
          DisposeObject(DetectorDefineList[i]);
      DetectorDefineList.Clear;

      // load detector dataset
      for i := 0 to ai_img.DetectorDefineList.count - 1 do
        begin
          m64 := TMemoryStream64.CustomCreate(8192);
          ai_img.DetectorDefineList[i].SaveToStream(m64);
          m64.Position := 0;

          editor_DetDef := TEditorDetectorDefine.Create(Self);
          DetectorDefineList.Add(editor_DetDef);
          editor_DetDef.LoadFromStream(m64);
          DisposeObject(m64);
        end;
    end
  else
    begin
      for i := 0 to DetectorDefineList.count - 1 do
        begin
          m64 := TMemoryStream64.CustomCreate(8192);
          ai_img.DetectorDefineList[i].SaveToStream(m64);
          m64.Position := 0;
          DetectorDefineList[i].LoadFromStream(m64);
          DisposeObject(m64);
        end;
    end;

  DisposeObject(ai_imgList);
end;

procedure TEditorImageData.ProcessSegmentationAlignment(Alignment: zAI.TAlignment_SS);
var
  ai_imgList: TAI_ImageList;
  ai_img: TAI_Image;
  i: Integer;
  m64: TMemoryStream64;
begin
  ai_imgList := TAI_ImageList.Create;

  ai_img := TAI_Image.Create(ai_imgList);
  ai_imgList.Add(ai_img);
  ai_img.Raster.Assign(Raster);

  Alignment.Alignment(ai_imgList);

  m64 := TMemoryStream64.Create;
  ai_img.SegmentationMaskList.SaveToStream(m64);
  m64.Position := 0;
  SegmentationMaskList.LoadFromStream_AI(m64);
  DisposeObject(ai_imgList);
end;

constructor TEditorImageDataList.Create(const FreeImgData_: Boolean);
begin
  inherited Create;
  FreeImgData := FreeImgData_;
end;

destructor TEditorImageDataList.Destroy;
var
  i: Integer;
begin
  if FreeImgData then
    begin
      for i := 0 to count - 1 do
          DisposeObject(Items[i]);
      Clear;
    end;
  inherited Destroy;
end;

procedure TEditorImageDataList.RunScript(ScriptStyle: TTextStyle; condition_exp, process_exp: SystemString);
var
  i, j: Integer;
  img: TEditorImageData;
  condition_img_ok, condition_det_ok: Boolean;
begin
  // reset state
  for i := 0 to count - 1 do
    begin
      img := Items[i];
      img.FOP_RT_RunDeleted := False;
      for j := 0 to img.DetectorDefineList.count - 1 do
          img.DetectorDefineList[j].FOP_RT_RunDeleted := False;
    end;

  for i := 0 to count - 1 do
    begin
      img := Items[i];

      if img.RunExpCondition(ScriptStyle, condition_exp) then
          img.RunExpProcess(ScriptStyle, process_exp);
    end;

  // process delete state
  i := 0;
  while i < count do
    begin
      img := Items[i];

      if img.FOP_RT_RunDeleted then
        begin
          DoStatus('image script on delete %d', [i]);
          Delete(i);
        end
      else
        begin
          j := 0;
          while j < img.DetectorDefineList.count do
            begin
              if img.DetectorDefineList[j].FOP_RT_RunDeleted then
                begin
                  DisposeObject(img.DetectorDefineList[j]);
                  img.DetectorDefineList.Delete(j);
                end
              else
                  inc(j);
            end;

          inc(i);
        end;
    end;
end;

procedure TEditorImageDataList.RunScript(condition_exp, process_exp: SystemString);
begin
  RunScript(tsPascal, condition_exp, process_exp);
end;

function TEditorImageDataList.GetDetectorTokenList(filter: U_String): TPascalStringList;
var
  hl: THashList;
  i, j: Integer;
  DetDef: TEditorDetectorDefine;
  n: U_String;
begin
  hl := THashList.CustomCreate(256);
  hl.AutoFreeData := False;
  hl.AccessOptimization := False;

  for i := 0 to count - 1 do
    for j := 0 to Items[i].DetectorDefineList.count - 1 do
      begin
        DetDef := Items[i].DetectorDefineList[j];

        n := DetDef.Token;
        if (n.Len = 0) or (umlSearchMatch(filter, n)) then
            hl.Add(n, nil);
      end;

  Result := TPascalStringList.Create;
  hl.GetNameList(Result);
  DisposeObject(hl);
end;

function TEditorImageDataList.GetGeometryTokenList(filter: U_String): TPascalStringList;
var
  hl: THashList;
  i, j: Integer;
  geo: TEditorGeometry;
  SegmentationMask: TEditorSegmentationMask;
  n: U_String;
begin
  hl := THashList.CustomCreate(256);
  hl.AutoFreeData := False;
  hl.AccessOptimization := False;

  for i := 0 to count - 1 do
    begin
      for j := 0 to Items[i].GeometryList.count - 1 do
        begin
          geo := Items[i].GeometryList[j];

          n := geo.Token;
          if (n.Len = 0) or (umlSearchMatch(filter, n)) then
              hl.Add(n, nil);
        end;
    end;

  Result := TPascalStringList.Create;
  hl.GetNameList(Result);
  DisposeObject(hl);
end;

function TEditorImageDataList.GetSegmentationMaskTokenList(filter: U_String): TPascalStringList;
var
  hl: THashList;
  i, j: Integer;
  geo: TEditorGeometry;
  SegmentationMask: TEditorSegmentationMask;
  n: U_String;
begin
  hl := THashList.CustomCreate(256);
  hl.AutoFreeData := False;
  hl.AccessOptimization := False;

  for i := 0 to count - 1 do
    begin
      for j := 0 to Items[i].SegmentationMaskList.count - 1 do
        begin
          SegmentationMask := Items[i].SegmentationMaskList[j];

          n := SegmentationMask.Token;
          if (n.Len = 0) or (umlSearchMatch(filter, n)) then
              hl.Add(n, nil);
        end;
    end;

  Result := TPascalStringList.Create;
  hl.GetNameList(Result);
  DisposeObject(hl);
end;

procedure TEditorImageDataList.SaveToStream(stream: TCoreClassStream; const Scale: TGeoFloat; const pt_: TVec2; SaveImg: Boolean);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
  i: Integer;
  imgData: TEditorImageData;
begin
  de := TDataFrameEngine.Create;
  de.WriteSingle(Scale);
  de.WriteString(umlFloatToStr(pt_[0]));
  de.WriteString(umlFloatToStr(pt_[1]));

  de.WriteInteger(count);

  for i := 0 to count - 1 do
    begin
      m64 := TMemoryStream64.Create;
      imgData := Items[i];
      imgData.SaveToStream(m64, SaveImg);
      de.WriteStream(m64);
      DisposeObject(m64);
    end;

  de.EncodeTo(stream);
  DisposeObject(de);
end;

procedure TEditorImageDataList.LoadFromStream(stream: TCoreClassStream; var Scale: TGeoFloat; var pt_: TVec2);
var
  de: TDataFrameEngine;
  i, c: Integer;
  m64: TMemoryStream64;
  imgData: TEditorImageData;
begin
  de := TDataFrameEngine.Create;
  de.DecodeFrom(stream);

  Scale := de.Reader.ReadSingle;
  pt_[0] := umlStrToFloat(de.Reader.ReadString, 0);
  pt_[1] := umlStrToFloat(de.Reader.ReadString, 0);

  c := de.Reader.ReadInteger;

  for i := 0 to c - 1 do
    begin
      m64 := TMemoryStream64.Create;
      de.Reader.ReadStream(m64);
      m64.Position := 0;
      imgData := TEditorImageData.Create;
      imgData.LoadFromStream(m64);
      DisposeObject(m64);
      Add(imgData);
    end;

  DisposeObject(de);
end;

procedure TEditorImageDataList.LoadFromStream(stream: TCoreClassStream);
var
  Scale: TGeoFloat;
  pt_: TVec2;
begin
  LoadFromStream(stream, Scale, pt_);
end;

procedure TEditorImageDataList.LoadFromFile(FileName: U_String);
var
  fs: TCoreClassFileStream;
begin
  try
    fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    LoadFromStream(fs);
    DisposeObject(fs);
  except
  end;
end;

procedure TEditorImageDataList.SaveToStream_AI(stream: TCoreClassStream; RasterSaveMode: TRasterSave);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
  i: Integer;
  imgData: TEditorImageData;
begin
  de := TDataFrameEngine.Create;

  de.WriteInteger(count);

  for i := 0 to count - 1 do
    begin
      m64 := TMemoryStream64.Create;
      imgData := Items[i];
      imgData.SaveToStream_AI(m64, RasterSaveMode);
      de.WriteStream(m64);
      DisposeObject(m64);
    end;

  de.EncodeTo(stream, True);
  DisposeObject(de);
end;

procedure TEditorImageDataList.LoadFromStream_AI(stream: TCoreClassStream);
var
  de: TDataFrameEngine;
  i, j, c: Integer;
  m64: TMemoryStream64;
  imgData: TEditorImageData;
begin
  de := TDataFrameEngine.Create;
  de.DecodeFrom(stream);

  c := de.Reader.ReadInteger;

  for i := 0 to c - 1 do
    begin
      m64 := TMemoryStream64.Create;
      de.Reader.ReadStream(m64);
      m64.Position := 0;
      imgData := TEditorImageData.Create;
      imgData.LoadFromStream_AI(m64);
      DisposeObject(m64);
      Add(imgData);
    end;

  DisposeObject(de);
end;

initialization

On_Editor_Script_RegisterProc := nil;

end.
