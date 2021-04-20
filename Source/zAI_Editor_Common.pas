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

uses Types, Variants,

{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  CoreClasses, PascalStrings, MemoryRaster, MemoryStream64, DoStatusIO, DataFrameEngine,
  Cadencer, ListEngine, TextDataEngine, NotifyObjectBase, TextParsing, zExpression, OpCode,
  ObjectData, ObjectDataManager, ItemStream,
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
    Token: U_String;
    Part: TVec2List;
    PrepareRaster: TDETexture;

    constructor Create(Owner_: TEditorImageData);
    destructor Destroy; override;

    procedure SaveToStream(stream: TMemoryStream64); overload;
    procedure SaveToStream(stream: TMemoryStream64; RasterSave_: TRasterSaveFormat); overload;
    procedure LoadFromStream(stream: TMemoryStream64);
  end;

  TEditorGeometry = class(T2DPolygonGraph)
  public
    Owner: TEditorImageData;
    Token: U_String;
    constructor Create;
    destructor Destroy; override;
  end;

  TEditorGeometryList = class(TEditorGeometryList_Decl)
  public
    Owner: TEditorImageData;
    constructor Create;
    procedure SaveToStream(stream: TMemoryStream64);
    procedure LoadFromStream(stream: TMemoryStream64);
    function GetNearLine(const pt_: TVec2; out output: T2DPolygon; out lb, le: Integer): TVec2;
  end;

  TEditorSegmentationMask = class
  protected
    FBoundBoxCached: Boolean;
    FBoundBoxCache: TRectV2;
    FViewerRaster: TMemoryRaster;
    FBusy: TAtomBool;
    FBorderColor, FBodyColor: TRColor;
    FBorderWidth: Integer;
    procedure BuildViewerTh();
  public
    Owner: TEditorImageData;
    BGColor, FGColor: TRColor;
    Token: U_String;
    PickedPoint: TPoint;
    Raster: TMemoryRaster;

    FromGeometry: Boolean;
    FromSegmentationMaskImage: Boolean;

    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(stream: TMemoryStream64);
    procedure LoadFromStream(stream: TMemoryStream64);
    function BoundsRectV2(): TRectV2;
    procedure WaitViewerRaster();
    function GetViewerRaster(BorderColor_, BodyColor_: TRColor; BorderWidth_: Integer): TMemoryRaster;
  end;

  TEditorSegmentationMaskList = class(TEditorSegmentationMaskList_Decl)
  public
    Owner: TEditorImageData;

    constructor Create;
    procedure SaveToStream(stream: TMemoryStream64);
    procedure LoadFromStream(stream: TMemoryStream64);

    procedure SaveToStream_AI(stream: TMemoryStream64);
    procedure LoadFromStream_AI(stream: TMemoryStream64);

    function BuildSegmentationMask(mr: TMemoryRaster; sampler_FG_Color, buildBG_color, buildFG_color: TRColor): TEditorSegmentationMask; overload;
    function BuildSegmentationMask(geo: TEditorGeometry; buildBG_color, buildFG_color: TRColor): TEditorSegmentationMask; overload;
    procedure RemoveGeometrySegmentationMask;
    procedure RebuildGeometrySegmentationMask(buildBG_color, buildFG_color: TRColor);
    function PickSegmentationMask(X, Y: Integer): TEditorSegmentationMask; overload;
    function PickSegmentationMask(R: TRect; output: TEditorSegmentationMaskList): Boolean; overload;
  end;

  TEditorImageData = class
  private
    FOP_RT: TOpCustomRunTime;
    FOP_RT_RunDeleted: Boolean;
    { register op }
    procedure CheckAndRegOPRT;
    { condition on image }
    function OP_Image_GetWidth(var Param: TOpParam): Variant;
    function OP_Image_GetHeight(var Param: TOpParam): Variant;
    function OP_Image_GetDetector(var Param: TOpParam): Variant;
    function OP_Image_GetGeometry(var Param: TOpParam): Variant;
    function OP_Image_GetSegmentation(var Param: TOpParam): Variant;
    { condition on detector }
    function OP_Detector_GetLabel(var Param: TOpParam): Variant;
    { process on image }
    function OP_Image_Delete(var Param: TOpParam): Variant;
    function OP_Image_Scale(var Param: TOpParam): Variant;
    function OP_Image_FitScale(var Param: TOpParam): Variant;
    function OP_Image_FixedScale(var Param: TOpParam): Variant;
    function OP_Image_SwapRB(var Param: TOpParam): Variant;
    function OP_Image_Gray(var Param: TOpParam): Variant;
    function OP_Image_Sharpen(var Param: TOpParam): Variant;
    function OP_Image_HistogramEqualize(var Param: TOpParam): Variant;
    function OP_Image_RemoveRedEyes(var Param: TOpParam): Variant;
    function OP_Image_Sepia(var Param: TOpParam): Variant;
    function OP_Image_Blur(var Param: TOpParam): Variant;
    function OP_Image_CalibrateRotate(var Param: TOpParam): Variant;
    function OP_Image_FlipHorz(var Param: TOpParam): Variant;
    function OP_Image_FlipVert(var Param: TOpParam): Variant;
    { set all token }
    function OP_Detector_SetLabel(var Param: TOpParam): Variant;
    { process on detector }
    function OP_Detector_ClearDetector(var Param: TOpParam): Variant;
    function OP_Detector_DeleteDetector(var Param: TOpParam): Variant;
    function OP_Detector_RemoveInvalidDetectorFromPart(var Param: TOpParam): Variant;
    function OP_Detector_RemovePart(var Param: TOpParam): Variant;
    { process on geometry }
    function OP_Geometry_ClearGeometry(var Param: TOpParam): Variant;
    { process on Segmentation mask }
    function OP_SegmentationMask_ClearSegmentationMask(var Param: TOpParam): Variant;
  public
    DetectorDefineList: TEditorDetectorDefineList;
    FileInfo: U_String;
    Raster: TDETexture;
    RasterDrawRect: TRectV2;
    GeometryList: TEditorGeometryList;
    SegmentationMaskList: TEditorSegmentationMaskList;
    CreateTime: TDateTime;
    LastModifyTime: TDateTime;

    constructor Create;
    destructor Destroy; override;

    procedure RemoveDetectorFromRect(R: TRectV2); overload;
    procedure RemoveDetectorFromRect(R: TRectV2; Token: U_String); overload;
    procedure Clear;
    function Clone: TEditorImageData;

    function RunExpCondition(ScriptStyle: TTextStyle; exp: SystemString): Boolean;
    function RunExpProcess(ScriptStyle: TTextStyle; exp: SystemString): Boolean;
    function GetExpFunctionList: TPascalStringList;

    function AbsToLocalPt(pt: TVec2): TPoint;
    function AbsToLocal(pt: TVec2): TVec2;

    function LocalPtToAbs(pt: TPoint): TVec2;
    function LocalToAbs(pt: TVec2): TVec2;

    function GetTokenCount(Token: U_String): Integer;
    procedure Scale(f: TGeoFloat);
    procedure FitScale(Width_, Height_: Integer);
    procedure FixedScale(Res: Integer);
    procedure Rotate90;
    procedure Rotate270;
    procedure Rotate180;
    procedure RemoveInvalidDetectorDefineFromPart(fixedPartNum: Integer);
    procedure FlipHorz;
    procedure FlipVert;

    procedure SaveToStream_AI(stream: TMemoryStream64); overload;
    procedure SaveToStream_AI(stream: TMemoryStream64; RasterSave_: TRasterSaveFormat); overload;
    procedure LoadFromStream_AI(stream: TMemoryStream64);

    procedure SaveToStream(stream: TMemoryStream64; SaveImg: Boolean; RasterSave_: TRasterSaveFormat); overload;
    procedure SaveToStream(stream: TMemoryStream64; SaveImg: Boolean); overload;
    procedure LoadFromStream(stream: TMemoryStream64);

    procedure ProcessDetectorAlignment(Alignment: zAI.TAlignment);
    procedure ProcessSegmentationAlignment(Alignment: zAI.TAlignment_SS);
  end;

  TEditorImageDataList = class(TEditorImageDataList_Decl)
  public
    FreeImgData: Boolean;
    LastLoad_Scale: TGeoFloat;
    LastLoad_pt: TVec2;

    constructor Create(const FreeImgData_: Boolean);
    destructor Destroy; override;

    function GetImageDataFromFileName(FileName: U_String; Width, Height: Integer): TEditorImageData;

    procedure RunScript(ScriptStyle: TTextStyle; condition_exp, process_exp: SystemString); overload;
    procedure RunScript(condition_exp, process_exp: SystemString); overload;

    function GetDetectorTokenList(filter: U_String): TPascalStringList;
    function GetGeometryTokenList(filter: U_String): TPascalStringList;
    function GetSegmentationMaskTokenList(filter: U_String): TPascalStringList;

    { save as .ai_set format }
    procedure SaveToStream(stream: TCoreClassStream; const Scale: TGeoFloat; const pt_: TVec2; SaveImg: Boolean; RasterSave_: TRasterSaveFormat); overload;
    procedure SaveToStream(stream: TCoreClassStream; const Scale: TGeoFloat; const pt_: TVec2; SaveImg: Boolean); overload;
    procedure SaveToStream(stream: TCoreClassStream); overload;
    procedure SaveToFile(FileName: U_String); overload;
    procedure SaveToFile(FileName: U_String; RasterSave_: TRasterSaveFormat); overload;
    { load from .ai_set format }
    procedure LoadFromStream(stream: TCoreClassStream; var Scale: TGeoFloat; var pt_: TVec2); overload;
    procedure LoadFromStream(stream: TCoreClassStream); overload;
    procedure LoadFromFile(FileName: U_String); overload;

    { export as .imgDataset (from zAI_Common.pas) format support }
    procedure SaveToStream_AI(stream: TCoreClassStream; RasterSaveMode: TRasterSaveFormat);
    procedure SaveToFile_AI(FileName: U_String; RasterSaveMode: TRasterSaveFormat);
    { import from .imgDataset (from zAI_Common.pas) format }
    procedure LoadFromStream_AI(stream: TCoreClassStream);
    procedure LoadFromFile_AI(FileName: U_String);

    { export as .ImgMat (from zAI_Common.pas) format support }
    procedure SaveToStream_ImgMat(stream: TCoreClassStream; RasterSaveMode: TRasterSaveFormat);
    procedure SaveToFile_ImgMat(FileName: U_String; RasterSaveMode: TRasterSaveFormat);
    { import from .ImgMat (from zAI_Common.pas) format }
    procedure LoadFromStream_ImgMat(stream: TCoreClassStream);
    procedure LoadFromFile_ImgMat(FileName: U_String);
  end;

  TEditor_Image_Script_Register = procedure(Sender: TEditorImageData; opRT: TOpCustomRunTime) of object;

var
  On_Editor_Script_RegisterProc: TEditor_Image_Script_Register;

implementation

uses Math;

constructor TEditorDetectorDefine.Create(Owner_: TEditorImageData);
begin
  inherited Create;
  Owner := Owner_;
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
  SaveToStream(stream, TRasterSaveFormat.rsRGB);
end;

procedure TEditorDetectorDefine.SaveToStream(stream: TMemoryStream64; RasterSave_: TRasterSaveFormat);
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
        PrepareRaster.Update;
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

function TEditorGeometryList.GetNearLine(const pt_: TVec2; out output: T2DPolygon; out lb, le: Integer): TVec2;
type
  TNearLineData = record
    l: T2DPolygon;
    lb, le: Integer;
    near_pt: TVec2;
  end;

  PNearLineData = ^TNearLineData;
  TNearLineDataArray = array of TNearLineData;
  TNearLineDataPtrArray = array of PNearLineData;

var
  buff_ori: TNearLineDataArray;
  buff: TNearLineDataPtrArray;
  procedure Fill_buff;
  var
    i: Integer;
  begin
    for i := 0 to length(buff) - 1 do
        buff[i] := @buff_ori[i];
  end;

  procedure extract_NearLine();
  var
    i: Integer;
  begin
    for i := 0 to count - 1 do
        buff_ori[i].near_pt := Items[i].GetNearLine(pt_, buff_ori[i].l, buff_ori[i].lb, buff_ori[i].le);
  end;

  procedure Fill_Result;
  var
    i: Integer;
  begin
    { write result }
    output := buff[0]^.l;
    lb := buff[0]^.lb;
    le := buff[0]^.le;
    Result := buff[0]^.near_pt;

    for i := 1 to length(buff) - 1 do
      begin
        if PointDistance(buff[i]^.near_pt, pt_) < PointDistance(Result, pt_) then
          begin
            output := buff[i]^.l;
            lb := buff[i]^.lb;
            le := buff[i]^.le;
            Result := buff[i]^.near_pt;
          end;
      end;
  end;

begin
  SetLength(buff_ori, count);
  SetLength(buff, count);
  Fill_buff();
  extract_NearLine();
  Fill_Result();

  { free buff }
  SetLength(buff_ori, 0);
  SetLength(buff, 0);
end;

procedure TEditorSegmentationMask.BuildViewerTh;
var
  tmp: TMemoryRaster;
  i: Integer;
begin
  tmp := NewRaster();
  tmp.Assign(Raster);
  tmp.SetSize(Raster.Width, Raster.Height);
  for i := 0 to tmp.Width * tmp.Height - 1 do
    if Raster.DirectBits^[i] = FGColor then
        tmp.DirectBits^[i] := FBodyColor
    else
        tmp.DirectBits^[i] := 0;
  Raster.FillNoneBGColorAlphaBorder(False, BGColor, FBorderColor, FBorderWidth, tmp);
  FViewerRaster := tmp;
  FBusy.V := False;
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
  FViewerRaster := nil;
  FBusy := TAtomBool.Create(False);
  FBorderColor := RColor(0, 0, 0);
  FBodyColor := RColor(0, 0, 0);
  FBorderWidth := 0;

  FromGeometry := False;
  FromSegmentationMaskImage := False;
  FBoundBoxCached := False;
  FBoundBoxCache := RectV2(0, 0, 0, 0);
end;

destructor TEditorSegmentationMask.Destroy;
begin
  WaitViewerRaster;
  DisposeObject(Raster);
  DisposeObjectAndNil(FViewerRaster);
  DisposeObject(FBusy);
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
  Raster.SaveToZLibCompressStream(m64);
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

function TEditorSegmentationMask.BoundsRectV2: TRectV2;
begin
  if not FBoundBoxCached then
    begin
      FBoundBoxCache := Raster.ColorBoundsRectV2(FGColor);
      FBoundBoxCached := True;
    end;
  Result := FBoundBoxCache;
end;

procedure TEditorSegmentationMask.WaitViewerRaster;
begin
  while FBusy.V do
      TCompute.Sleep(1);
end;

function TEditorSegmentationMask.GetViewerRaster(BorderColor_, BodyColor_: TRColor; BorderWidth_: Integer): TMemoryRaster;
begin
  Result := nil;
  if FBusy.V then
      exit;
  if FViewerRaster = nil then
    begin
      FBusy.V := True;
      FBorderColor := BorderColor_;
      FBodyColor := BodyColor_;
      FBorderWidth := BorderWidth_;
      TCompute.RunM_NP({$IFDEF FPC}@{$ENDIF FPC}BuildViewerTh);
    end;
  Result := FViewerRaster;
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
      { 0: bk color }
      { 1: fg color }
      { 2: name }
      { 3: raster }

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

      { 0: bk color }
      { 1: fg color }
      { 2: name }
      { 3: raster }
      SegmentationMask := TEditorSegmentationMask.Create;
      SegmentationMask.Owner := Owner;

      { read }
      SegmentationMask.BGColor := nd.Reader.ReadCardinal;
      SegmentationMask.FGColor := nd.Reader.ReadCardinal;
      SegmentationMask.Token := nd.Reader.ReadString;
      m64 := TMemoryStream64.Create;
      nd.Reader.ReadStream(m64);
      m64.Position := 0;
      SegmentationMask.Raster.LoadFromStream(m64);

      { calibrate }
      SegmentationMask.FromGeometry := False;
      SegmentationMask.FromSegmentationMaskImage := True;
      SegmentationMask.PickedPoint := SegmentationMask.Raster.FindNearColor(SegmentationMask.FGColor, Owner.Raster.Centre);
      Add(SegmentationMask);

      DisposeObject(m64);
      DisposeObject(nd);
    end;

  DisposeObject(d);
end;

function TEditorSegmentationMaskList.BuildSegmentationMask(mr: TMemoryRaster; sampler_FG_Color, buildBG_color, buildFG_color: TRColor): TEditorSegmentationMask;
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

function TEditorSegmentationMaskList.BuildSegmentationMask(geo: TEditorGeometry; buildBG_color, buildFG_color: TRColor): TEditorSegmentationMask;

var
  SegMask: TEditorSegmentationMask;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    i: Integer;
  begin
    for i := 0 to SegMask.Raster.Width - 1 do
      if geo.InHere(Vec2(i, pass)) then
          SegMask.Raster.Pixel[i, pass] := buildFG_color;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass, i: Integer;
  begin
    for pass := 0 to SegMask.Raster.Height - 1 do
      for i := 0 to SegMask.Raster.Width - 1 do
        if geo.InHere(Vec2(i, pass)) then
            SegMask.Raster.Pixel[i, pass] := buildFG_color;
  end;
{$ENDIF Parallel}


begin
  SegMask := TEditorSegmentationMask.Create;
  SegMask.Owner := Owner;
  SegMask.BGColor := buildBG_color;
  SegMask.FGColor := buildFG_color;
  SegMask.Token := geo.Token;
  SegMask.FromGeometry := True;
  SegMask.FromSegmentationMaskImage := False;
  SegMask.Raster.SetSize(Owner.Raster.Width, Owner.Raster.Height, buildBG_color);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(@Nested_ParallelFor, 0, Owner.GeometryList.count - 1);
{$ELSE FPC}
  DelphiParallelFor(0, SegMask.Raster.Height - 1, procedure(pass: Integer)
    var
      i: Integer;
    begin
      for i := 0 to SegMask.Raster.Width - 1 do
        if geo.InHere(Vec2(i, pass)) then
            SegMask.Raster.Pixel[i, pass] := buildFG_color;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  SegMask.PickedPoint := SegMask.Raster.FindNearColor(buildFG_color, Owner.Raster.Centre);

  LockObject(Self);
  Add(SegMask);
  UnlockObject(Self);

  Result := SegMask;
end;

procedure TEditorSegmentationMaskList.RemoveGeometrySegmentationMask;
var
  i: Integer;
begin
  LockObject(Self);
  { remove geometry data source }
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
  UnlockObject(Self);
end;

procedure TEditorSegmentationMaskList.RebuildGeometrySegmentationMask(buildBG_color, buildFG_color: TRColor);
var
  pass: Integer;
begin
  RemoveGeometrySegmentationMask;
  for pass := 0 to Owner.GeometryList.count - 1 do
      BuildSegmentationMask(Owner.GeometryList[pass], buildBG_color, buildFG_color);
end;

function TEditorSegmentationMaskList.PickSegmentationMask(X, Y: Integer): TEditorSegmentationMask;
var
  i: Integer;
  tmp: TEditorSegmentationMask;
begin
  Result := nil;
  if not Owner.Raster.InHere(X, Y) then
      exit;

  for i := 0 to count - 1 do
    begin
      tmp := Items[i];
      if (not tmp.FromGeometry) and (tmp.FromSegmentationMaskImage) and (tmp.Raster[X, Y] = tmp.FGColor) then
        begin
          Result := tmp;
          exit;
        end;
    end;
end;

function TEditorSegmentationMaskList.PickSegmentationMask(R: TRect; output: TEditorSegmentationMaskList): Boolean;
var
  i, X, Y: Integer;
  tmp: TEditorSegmentationMask;
  found: Boolean;
begin
  Result := False;
  if output <> nil then
    for i := 0 to count - 1 do
      begin
        tmp := Items[i];
        found := False;

        for Y := 0 to tmp.Raster.Height - 1 do
          if found then
              break
          else
            for X := 0 to tmp.Raster.Width - 1 do
              if tmp.Raster[X, Y] = tmp.FGColor then
                begin
                  found := True;
                  break;
                end;

        if found then
          begin
            output.Add(tmp);
            Result := True;
          end;
      end;
end;

procedure TEditorImageData.CheckAndRegOPRT;
begin
  if FOP_RT <> nil then
      exit;
  FOP_RT := TOpCustomRunTime.Create;
  FOP_RT.UserObject := Self;

  { condition on image }
  FOP_RT.RegOpM('Width', 'Width(): Image Width', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetWidth)^.Category := 'AI Editor';
  FOP_RT.RegOpM('Height', 'Height(): Image Height', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetHeight)^.Category := 'AI Editor';
  FOP_RT.RegOpM('Det', 'Det(): Detector define of Count', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetDetector)^.Category := 'AI Editor';
  FOP_RT.RegOpM('Detector', 'Detector(): Detector define of Count', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetDetector)^.Category := 'AI Editor';
  FOP_RT.RegOpM('DetNum', 'DetNum(): Detector define of Count', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetDetector)^.Category := 'AI Editor';

  FOP_RT.RegOpM('Geo', 'Geo(): geometry define of Count', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetGeometry)^.Category := 'AI Editor';
  FOP_RT.RegOpM('Geometry', 'Geometry(): geometry define of Count', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetGeometry)^.Category := 'AI Editor';
  FOP_RT.RegOpM('GeoNum', 'GeoNum(): geometry define of Count', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetGeometry)^.Category := 'AI Editor';

  FOP_RT.RegOpM('Seg', 'Seg(): segmentation define of Count', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetSegmentation)^.Category := 'AI Editor';
  FOP_RT.RegOpM('Segmentation', 'Segmentation(): segmentation define of Count', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetSegmentation)^.Category := 'AI Editor';
  FOP_RT.RegOpM('SegNum', 'SegNum(): segmentation define of Count', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetSegmentation)^.Category := 'AI Editor';

  { condition on detector }
  FOP_RT.RegOpM('Label', 'Label(): Label Name', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_GetLabel)^.Category := 'AI Editor';
  FOP_RT.RegOpM('GetLabel', 'GetLabel(): Label Name', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_GetLabel)^.Category := 'AI Editor';

  { process on image }
  FOP_RT.RegOpM('Delete', 'Delete(): Delete image', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Delete)^.Category := 'AI Editor';

  FOP_RT.RegOpM('Scale', 'Scale(k:Float): scale image', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Scale)^.Category := 'AI Editor';
  FOP_RT.RegOpM('ReductMemory', 'ReductMemory(k:Float): scale image', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Scale)^.Category := 'AI Editor';
  FOP_RT.RegOpM('FitScale', 'FitScale(Width, Height): fitscale image', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_FitScale)^.Category := 'AI Editor';
  FOP_RT.RegOpM('FixedScale', 'FixedScale(Res): fitscale image', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_FixedScale)^.Category := 'AI Editor';

  FOP_RT.RegOpM('SwapRB', 'SwapRB(): swap red blue channel', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_SwapRB)^.Category := 'AI Editor';
  FOP_RT.RegOpM('SwapBR', 'SwapRB(): swap red blue channel', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_SwapRB)^.Category := 'AI Editor';

  FOP_RT.RegOpM('Gray', 'Gray(): Convert image to grayscale', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Gray)^.Category := 'AI Editor';
  FOP_RT.RegOpM('Grayscale', 'Grayscale(): Convert image to grayscale', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Gray)^.Category := 'AI Editor';

  FOP_RT.RegOpM('Sharpen', 'Sharpen(): Convert image to Sharpen', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Sharpen)^.Category := 'AI Editor';

  FOP_RT.RegOpM('HistogramEqualize', 'HistogramEqualize(): Convert image to HistogramEqualize', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_HistogramEqualize)^.Category := 'AI Editor';
  FOP_RT.RegOpM('he', 'he(): Convert image to HistogramEqualize', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_HistogramEqualize)^.Category := 'AI Editor';
  FOP_RT.RegOpM('NiceColor', 'NiceColor(): Convert image to HistogramEqualize', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_HistogramEqualize)^.Category := 'AI Editor';

  FOP_RT.RegOpM('RemoveRedEye', 'RemoveRedEye(): Remove image red eye', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_RemoveRedEyes)^.Category := 'AI Editor';
  FOP_RT.RegOpM('RemoveRedEyes', 'RemoveRedEyes(): Remove image red eye', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_RemoveRedEyes)^.Category := 'AI Editor';
  FOP_RT.RegOpM('RedEyes', 'RedEyes(): Remove image red eye', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_RemoveRedEyes)^.Category := 'AI Editor';
  FOP_RT.RegOpM('RedEye', 'RedEye(): Remove image red eye', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_RemoveRedEyes)^.Category := 'AI Editor';

  FOP_RT.RegOpM('Sepia', 'Sepia(Depth): Convert image to Sepia', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Sepia)^.Category := 'AI Editor';
  FOP_RT.RegOpM('Blur', 'Blur(radius): Convert image to Blur', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Blur)^.Category := 'AI Editor';

  FOP_RT.RegOpM('CalibrateRotate', 'CalibrateRotate(): Using Hough transform to calibrate rotation', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_CalibrateRotate)^.Category := 'AI Editor';
  FOP_RT.RegOpM('DocumentAlignment', 'DocumentAlignment(): Using Hough transform to calibrate rotation', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_CalibrateRotate)^.Category := 'AI Editor';
  FOP_RT.RegOpM('DocumentAlign', 'DocumentAlign(): Using Hough transform to calibrate rotation', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_CalibrateRotate)^.Category := 'AI Editor';
  FOP_RT.RegOpM('DocAlign', 'DocAlign(): Using Hough transform to calibrate rotation', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_CalibrateRotate)^.Category := 'AI Editor';
  FOP_RT.RegOpM('AlignDoc', 'AlignDoc(): Using Hough transform to calibrate rotation', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_CalibrateRotate)^.Category := 'AI Editor';

  FOP_RT.RegOpM('FlipHorz', 'FlipHorz(): FlipHorz', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_FlipHorz)^.Category := 'AI Editor';
  FOP_RT.RegOpM('FlipVert', 'FlipVert(): FlipVert', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_FlipVert)^.Category := 'AI Editor';

  { process on detector }
  FOP_RT.RegOpM('SetLab', 'SetLab(newLabel name): new Label name', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_SetLabel)^.Category := 'AI Editor';
  FOP_RT.RegOpM('SetLabel', 'SetLabel(newLabel name): new Label name', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_SetLabel)^.Category := 'AI Editor';
  FOP_RT.RegOpM('DefLab', 'DefLab(newLabel name): new Label name', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_SetLabel)^.Category := 'AI Editor';
  FOP_RT.RegOpM('DefLabel', 'DefLabel(newLabel name): new Label name', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_SetLabel)^.Category := 'AI Editor';
  FOP_RT.RegOpM('DefineLabel', 'DefineLabel(newLabel name): new Label name', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_SetLabel)^.Category := 'AI Editor';

  FOP_RT.RegOpM('ClearDetector', 'ClearDetector(): clean detector box', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_ClearDetector)^.Category := 'AI Editor';
  FOP_RT.RegOpM('ClearDet', 'ClearDet(): clean detector box', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_ClearDetector)^.Category := 'AI Editor';
  FOP_RT.RegOpM('KillDetector', 'KillDetector(): clean detector box', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_ClearDetector)^.Category := 'AI Editor';
  FOP_RT.RegOpM('KillDet', 'KillDet(): clean detector box', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_ClearDetector)^.Category := 'AI Editor';

  FOP_RT.RegOpM('DeleteDetector', 'DeleteDetector(Maximum reserved box, x-scale, y-scale): delete detector box', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_DeleteDetector)^.Category := 'AI Editor';
  FOP_RT.RegOpM('DeleteRect', 'DeleteRect(Maximum reserved box, x-scale, y-scale): delete detector box', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_DeleteDetector)^.Category := 'AI Editor';

  FOP_RT.RegOpM('RemoveInvalidDetectorFromPart', 'RemoveInvalidDetectorFromPart(fixedPartNum): delete detector box from Part num', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_RemoveInvalidDetectorFromPart)^.Category := 'AI Editor';
  FOP_RT.RegOpM('RemoveInvalidDetectorFromSPNum', 'RemoveInvalidDetectorFromSPNum(fixedPartNum): delete detector box from Part num', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_RemoveInvalidDetectorFromPart)^.Category := 'AI Editor';

  FOP_RT.RegOpM('RemoveDetPart', 'RemoveDetPart(): remove detector define part', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_RemovePart)^.Category := 'AI Editor';

  { process on geometry }
  FOP_RT.RegOpM('ClearGeometry', 'ClearGeometry(): clean geometry', {$IFDEF FPC}@{$ENDIF FPC}OP_Geometry_ClearGeometry)^.Category := 'AI Editor';
  FOP_RT.RegOpM('ClearGeo', 'ClearGeo(): clean geometry', {$IFDEF FPC}@{$ENDIF FPC}OP_Geometry_ClearGeometry)^.Category := 'AI Editor';
  FOP_RT.RegOpM('KillGeometry', 'KillGeometry(): clean geometry', {$IFDEF FPC}@{$ENDIF FPC}OP_Geometry_ClearGeometry)^.Category := 'AI Editor';
  FOP_RT.RegOpM('KillGeo', 'KillGeo(): clean geometry', {$IFDEF FPC}@{$ENDIF FPC}OP_Geometry_ClearGeometry)^.Category := 'AI Editor';

  { process on Segmentation mask }
  FOP_RT.RegOpM('ClearSegmentationMask', 'ClearSegmentationMask(): clean segmentation mask', {$IFDEF FPC}@{$ENDIF FPC}OP_SegmentationMask_ClearSegmentationMask)^.Category := 'AI Editor';
  FOP_RT.RegOpM('ClearSeg', 'ClearSeg(): clean segmentation mask', {$IFDEF FPC}@{$ENDIF FPC}OP_SegmentationMask_ClearSegmentationMask)^.Category := 'AI Editor';
  FOP_RT.RegOpM('KillSegmentationMask', 'KillSegmentationMask(): clean segmentation mask', {$IFDEF FPC}@{$ENDIF FPC}OP_SegmentationMask_ClearSegmentationMask)^.Category := 'AI Editor';
  FOP_RT.RegOpM('KillSeg', 'KillSeg(): clean segmentation mask', {$IFDEF FPC}@{$ENDIF FPC}OP_SegmentationMask_ClearSegmentationMask)^.Category := 'AI Editor';

  { external image processor }
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
  if not Raster.Empty then
    begin
      Scale(Param[0]);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;
  Result := True;
end;

function TEditorImageData.OP_Image_FitScale(var Param: TOpParam): Variant;
begin
  if not Raster.Empty then
    begin
      FitScale(Param[0], Param[1]);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;
  Result := True;
end;

function TEditorImageData.OP_Image_FixedScale(var Param: TOpParam): Variant;
begin
  if not Raster.Empty then
    begin
      FixedScale(Param[0]);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;
  Result := True;
end;

function TEditorImageData.OP_Image_SwapRB(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  if not Raster.Empty then
    begin
      Raster.FormatBGRA;
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
      begin
        DetectorDefineList[i].PrepareRaster.FormatBGRA;
        DetectorDefineList[i].PrepareRaster.ReleaseGPUMemory;
      end;
  Result := True;
end;

function TEditorImageData.OP_Image_Gray(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  if not Raster.Empty then
    begin
      Raster.Grayscale;
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
      begin
        DetectorDefineList[i].PrepareRaster.Grayscale;
        DetectorDefineList[i].PrepareRaster.ReleaseGPUMemory;
      end;
  Result := True;
end;

function TEditorImageData.OP_Image_Sharpen(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  if not Raster.Empty then
    begin
      Sharpen(Raster, True);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
      begin
        Sharpen(DetectorDefineList[i].PrepareRaster, True);
        DetectorDefineList[i].PrepareRaster.ReleaseGPUMemory;
      end;
  Result := True;
end;

function TEditorImageData.OP_Image_HistogramEqualize(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  if not Raster.Empty then
    begin
      HistogramEqualize(Raster);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
      begin
        HistogramEqualize(DetectorDefineList[i].PrepareRaster);
        DetectorDefineList[i].PrepareRaster.ReleaseGPUMemory;
      end;
  Result := True;
end;

function TEditorImageData.OP_Image_RemoveRedEyes(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  if not Raster.Empty then
    begin
      RemoveRedEyes(Raster);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
      begin
        RemoveRedEyes(DetectorDefineList[i].PrepareRaster);
        DetectorDefineList[i].PrepareRaster.ReleaseGPUMemory;
      end;
  Result := True;
end;

function TEditorImageData.OP_Image_Sepia(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  if not Raster.Empty then
    begin
      Sepia32(Raster, Param[0]);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
      begin
        Sepia32(DetectorDefineList[i].PrepareRaster, Param[0]);
        DetectorDefineList[i].PrepareRaster.ReleaseGPUMemory;
      end;
  Result := True;
end;

function TEditorImageData.OP_Image_Blur(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  if not Raster.Empty then
    begin
      GaussianBlur(Raster, Param[0], Raster.BoundsRect);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
      begin
        GaussianBlur(DetectorDefineList[i].PrepareRaster, Param[0], DetectorDefineList[i].PrepareRaster.BoundsRect);
        DetectorDefineList[i].PrepareRaster.ReleaseGPUMemory;
      end;
  Result := True;
end;

function TEditorImageData.OP_Image_CalibrateRotate(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  if not Raster.Empty then
    begin
      Raster.CalibrateRotate;
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
      begin
        DetectorDefineList[i].PrepareRaster.CalibrateRotate;
        DetectorDefineList[i].PrepareRaster.ReleaseGPUMemory;
      end;
  Result := True;
end;

function TEditorImageData.OP_Image_FlipHorz(var Param: TOpParam): Variant;
begin
  FlipHorz;
  Result := True;
end;

function TEditorImageData.OP_Image_FlipVert(var Param: TOpParam): Variant;
begin
  FlipVert;
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

function TEditorImageData.OP_Detector_RemoveInvalidDetectorFromPart(var Param: TOpParam): Variant;
begin
  RemoveInvalidDetectorDefineFromPart(Param[0]);
  Result := True;
end;

function TEditorImageData.OP_Detector_RemovePart(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  for i := 0 to DetectorDefineList.count - 1 do
    begin
      DetectorDefineList[i].Part.Clear;
      DetectorDefineList[i].PrepareRaster.Reset;
    end;
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
  FileInfo := '';
  Raster := TDrawEngine.NewTexture;
  RasterDrawRect := RectV2(0, 0, 0, 0);
  GeometryList := TEditorGeometryList.Create;
  GeometryList.Owner := Self;
  SegmentationMaskList := TEditorSegmentationMaskList.Create;
  SegmentationMaskList.Owner := Self;
  FOP_RT := nil;
  FOP_RT_RunDeleted := False;
  CreateTime := umlNow();
  LastModifyTime := CreateTime;
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

procedure TEditorImageData.RemoveDetectorFromRect(R: TRectV2);
var
  i: Integer;
  det: TEditorDetectorDefine;
  r1, r2: TRectV2;
begin
  i := 0;
  clip(R, Raster.BoundsRectV2, r1);

  while i < DetectorDefineList.count do
    begin
      det := DetectorDefineList[i];
      r2 := RectV2(det.R);
      if RectWithinRect(r1, r2) or RectWithinRect(r2, r1) or RectToRectIntersect(r2, r1) or RectToRectIntersect(r1, r2) then
        begin
          DisposeObject(det);
          DetectorDefineList.Delete(i);
        end
      else
          inc(i);
    end;
end;

procedure TEditorImageData.RemoveDetectorFromRect(R: TRectV2; Token: U_String);
var
  i: Integer;
  det: TEditorDetectorDefine;
  r1, r2: TRectV2;
begin
  i := 0;
  clip(R, Raster.BoundsRectV2, r1);

  while i < DetectorDefineList.count do
    begin
      det := DetectorDefineList[i];
      r2 := RectV2(det.R);
      if (RectWithinRect(r1, r2) or RectWithinRect(r2, r1) or RectToRectIntersect(r2, r1) or RectToRectIntersect(r1, r2))
        and (Token.Same(det.Token)) then
        begin
          DisposeObject(det);
          DetectorDefineList.Delete(i);
        end
      else
          inc(i);
    end;
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

function TEditorImageData.Clone: TEditorImageData;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  SaveToStream(m64, True, TRasterSaveFormat.rsRGBA);
  Result := TEditorImageData.Create;
  m64.Position := 0;
  Result.LoadFromStream(m64);
  DisposeObject(m64);
end;

function TEditorImageData.RunExpCondition(ScriptStyle: TTextStyle; exp: SystemString): Boolean;
begin
  CheckAndRegOPRT;

  try
      Result := EvaluateExpressionValue(False, ScriptStyle, exp, FOP_RT);
  except
      Result := False;
  end;
end;

function TEditorImageData.RunExpProcess(ScriptStyle: TTextStyle; exp: SystemString): Boolean;
var
  R: Variant;
begin
  CheckAndRegOPRT;
  R := EvaluateExpressionValue(False, ScriptStyle, exp, FOP_RT);
  try
    if not VarIsNull(R) then
        Result := R
    else
        Result := False;
  except
      Result := False;
  end;
end;

function TEditorImageData.GetExpFunctionList: TPascalStringList;
begin
  CheckAndRegOPRT;
  Result := FOP_RT.GetAllProcDescription();
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

function TEditorImageData.GetTokenCount(Token: U_String): Integer;
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
      SegmentationMaskList[i].Raster.NonlinearScale(f);

  for i := 0 to GeometryList.count - 1 do
      GeometryList[i].Scale(f);

  RasterDrawRect := MakeRect(RectCentre(RasterDrawRect), Raster.Width, Raster.Height);
end;

procedure TEditorImageData.FitScale(Width_, Height_: Integer);
var
  R: TRectV2;
begin
  R := FitRect(Raster.BoundsRectV2, RectV2(0, 0, Width_, Height_));
  Scale(RectWidth(R) / Raster.Width);
end;

procedure TEditorImageData.FixedScale(Res: Integer);
begin
  // the size of the image is less than res * 0.8, todo zoom in gradiently
  if Raster.Width * Raster.Height < Round(Res * 0.8) then
    begin
      while Raster.Width * Raster.Height < Round(Res * 0.8) do
          Scale(2.0);
    end
    // he image size is higher than res * 1.2, gradient reduction (minimum aliasing)
  else if Raster.Width * Raster.Height > Round(Res * 1.2) then
    begin
      while Raster.Width * Raster.Height > Round(Res * 1.2) do
          Scale(0.5);
    end;
end;

procedure TEditorImageData.Rotate90;
var
  i, j, k: Integer;
  sour_scaleRect, dest_scaleRect, Final_Rect: TRectV2;
  DetDef: TEditorDetectorDefine;
  geo: TEditorGeometry;
  seg: TEditorSegmentationMask;
begin
  sour_scaleRect := Raster.BoundsRectV20;
  dest_scaleRect := RectV2(0, 0, sour_scaleRect[1, 1], sour_scaleRect[1, 0]);
  Final_Rect := RectAdd(sour_scaleRect, Vec2Sub(RectCentre(dest_scaleRect), RectCentre(sour_scaleRect)));

  for i := 0 to DetectorDefineList.count - 1 do
    begin
      DetDef := DetectorDefineList[i];
      DetDef.R := Rect2Rect(RectRotationProjection(sour_scaleRect, Final_Rect, 0, 90, Rect2Rect(DetDef.R)));
      for j := 0 to DetDef.Part.count - 1 do
          DetDef.Part[j]^ := RectRotationProjection(sour_scaleRect, Final_Rect, 0, 90, DetDef.Part[j]^);

      if DetDef.PrepareRaster <> nil then
        begin
          DetDef.PrepareRaster.Rotate90;
          DetDef.PrepareRaster.Update;
        end;
    end;

  for i := 0 to GeometryList.count - 1 do
    begin
      geo := GeometryList[i];
      for j := 0 to geo.Surround.count - 1 do
          geo.Surround[j]^ := RectRotationProjection(sour_scaleRect, Final_Rect, 0, 90, geo.Surround[j]^);
      for j := 0 to length(geo.Collapses) - 1 do
        for k := 0 to geo.Collapses[j].count - 1 do
            geo.Collapses[j][k]^ := RectRotationProjection(sour_scaleRect, Final_Rect, 0, 90, geo.Collapses[j][k]^);
    end;

  for i := 0 to SegmentationMaskList.count - 1 do
    begin
      seg := SegmentationMaskList[i];
      seg.FBoundBoxCached := False;
      seg.WaitViewerRaster;
      if seg.FViewerRaster <> nil then
        begin
          seg.FViewerRaster.Rotate90;
          seg.FViewerRaster.Update;
        end;
      seg.Raster.Rotate90;
      seg.Raster.Update;
    end;

  Raster.Rotate90;
  Raster.Update;
end;

procedure TEditorImageData.Rotate270;
var
  i, j, k: Integer;
  sour_scaleRect, dest_scaleRect, Final_Rect: TRectV2;
  DetDef: TEditorDetectorDefine;
  geo: TEditorGeometry;
  seg: TEditorSegmentationMask;
begin
  sour_scaleRect := Raster.BoundsRectV20;
  dest_scaleRect := RectV2(0, 0, sour_scaleRect[1, 1], sour_scaleRect[1, 0]);
  Final_Rect := RectAdd(sour_scaleRect, Vec2Sub(RectCentre(dest_scaleRect), RectCentre(sour_scaleRect)));

  for i := 0 to DetectorDefineList.count - 1 do
    begin
      DetDef := DetectorDefineList[i];
      DetDef.R := Rect2Rect(RectRotationProjection(sour_scaleRect, Final_Rect, 0, -90, Rect2Rect(DetDef.R)));
      for j := 0 to DetDef.Part.count - 1 do
          DetDef.Part[j]^ := RectRotationProjection(sour_scaleRect, Final_Rect, 0, -90, DetDef.Part[j]^);

      if DetDef.PrepareRaster <> nil then
        begin
          DetDef.PrepareRaster.Rotate270;
          DetDef.PrepareRaster.Update;
        end;
    end;

  for i := 0 to GeometryList.count - 1 do
    begin
      geo := GeometryList[i];
      for j := 0 to geo.Surround.count - 1 do
          geo.Surround[j]^ := RectRotationProjection(sour_scaleRect, Final_Rect, 0, -90, geo.Surround[j]^);
      for j := 0 to length(geo.Collapses) - 1 do
        for k := 0 to geo.Collapses[j].count - 1 do
            geo.Collapses[j][k]^ := RectRotationProjection(sour_scaleRect, Final_Rect, 0, -90, geo.Collapses[j][k]^);
    end;

  for i := 0 to SegmentationMaskList.count - 1 do
    begin
      seg := SegmentationMaskList[i];
      seg.FBoundBoxCached := False;
      seg.WaitViewerRaster;
      if seg.FViewerRaster <> nil then
        begin
          seg.FViewerRaster.Rotate270;
          seg.FViewerRaster.Update;
        end;
      seg.Raster.Rotate270;
      seg.Raster.Update;
    end;

  Raster.Rotate270;
  Raster.Update;
end;

procedure TEditorImageData.Rotate180;
var
  i, j, k: Integer;
  sour_scaleRect, Final_Rect: TRectV2;
  DetDef: TEditorDetectorDefine;
  geo: TEditorGeometry;
  seg: TEditorSegmentationMask;
begin
  sour_scaleRect := Raster.BoundsRectV20;
  Final_Rect := sour_scaleRect;

  for i := 0 to DetectorDefineList.count - 1 do
    begin
      DetDef := DetectorDefineList[i];
      DetDef.R := Rect2Rect(RectRotationProjection(sour_scaleRect, Final_Rect, 0, 180, Rect2Rect(DetDef.R)));
      for j := 0 to DetDef.Part.count - 1 do
          DetDef.Part[j]^ := RectRotationProjection(sour_scaleRect, Final_Rect, 0, 180, DetDef.Part[j]^);

      if DetDef.PrepareRaster <> nil then
        begin
          DetDef.PrepareRaster.Rotate180;
          DetDef.PrepareRaster.Update;
        end;
    end;

  for i := 0 to GeometryList.count - 1 do
    begin
      geo := GeometryList[i];
      for j := 0 to geo.Surround.count - 1 do
          geo.Surround[j]^ := RectRotationProjection(sour_scaleRect, Final_Rect, 0, 180, geo.Surround[j]^);
      for j := 0 to length(geo.Collapses) - 1 do
        for k := 0 to geo.Collapses[j].count - 1 do
            geo.Collapses[j][k]^ := RectRotationProjection(sour_scaleRect, Final_Rect, 0, 180, geo.Collapses[j][k]^);
    end;

  for i := 0 to SegmentationMaskList.count - 1 do
    begin
      seg := SegmentationMaskList[i];
      seg.FBoundBoxCached := False;
      seg.WaitViewerRaster;
      if seg.FViewerRaster <> nil then
        begin
          seg.FViewerRaster.Rotate180;
          seg.FViewerRaster.Update;
        end;
      seg.Raster.Rotate180;
      seg.Raster.Update;
    end;

  Raster.Rotate180;
  Raster.Update;
end;

procedure TEditorImageData.RemoveInvalidDetectorDefineFromPart(fixedPartNum: Integer);
var
  i: Integer;
  DetDef: TEditorDetectorDefine;
begin
  i := 0;
  while i < DetectorDefineList.count do
    begin
      DetDef := DetectorDefineList[i];
      if DetDef.Part.count <> fixedPartNum then
        begin
          DisposeObject(DetDef);
          DetectorDefineList.Delete(i);
        end
      else
          inc(i);
    end;
end;

procedure TEditorImageData.FlipHorz;
var
  i, j, k: Integer;
  w: Integer;
  DetDef: TEditorDetectorDefine;
  v_: PVec2;
  geo: TEditorGeometry;
  seg: TEditorSegmentationMask;
begin
  w := Raster.Width;
  for i := 0 to DetectorDefineList.count - 1 do
    begin
      DetDef := DetectorDefineList[i];
      DetDef.R.Left := w - DetDef.R.Left;
      DetDef.R.Right := w - DetDef.R.Right;

      if DetDef.PrepareRaster <> nil then
        begin
          DetDef.PrepareRaster.FlipHorz;
          DetDef.PrepareRaster.Update;
        end;

      for j := 0 to DetDef.Part.count - 1 do
        begin
          v_ := DetDef.Part[j];
          v_^[0] := w - v_^[0];
        end;
    end;

  for i := 0 to GeometryList.count - 1 do
    begin
      geo := GeometryList[i];
      for j := 0 to geo.Surround.count - 1 do
        begin
          v_ := geo.Surround[j];
          v_^[0] := w - v_^[0];
        end;
      for j := 0 to length(geo.Collapses) - 1 do
        for k := 0 to geo.Collapses[j].count - 1 do
          begin
            v_ := geo.Collapses[j][k];
            v_^[0] := w - v_^[0];
          end;
    end;

  for i := 0 to SegmentationMaskList.count - 1 do
    begin
      seg := SegmentationMaskList[i];
      if seg.FViewerRaster <> nil then
        begin
          seg.FViewerRaster.FlipHorz;
          seg.FViewerRaster.Update;
        end;
      seg.Raster.FlipHorz;
      seg.Raster.Update;
    end;

  Raster.FlipHorz;
  Raster.Update;
end;

procedure TEditorImageData.FlipVert;
var
  i, j, k: Integer;
  h: Integer;
  DetDef: TEditorDetectorDefine;
  v_: PVec2;
  geo: TEditorGeometry;
  seg: TEditorSegmentationMask;
begin
  h := Raster.Height;
  for i := 0 to DetectorDefineList.count - 1 do
    begin
      DetDef := DetectorDefineList[i];
      DetDef.R.Top := h - DetDef.R.Top;
      DetDef.R.Bottom := h - DetDef.R.Bottom;

      if DetDef.PrepareRaster <> nil then
        begin
          DetDef.PrepareRaster.FlipVert;
          DetDef.PrepareRaster.Update;
        end;

      for j := 0 to DetDef.Part.count - 1 do
        begin
          v_ := DetDef.Part[j];
          v_^[1] := h - v_^[1];
        end;
    end;

  for i := 0 to GeometryList.count - 1 do
    begin
      geo := GeometryList[i];
      for j := 0 to geo.Surround.count - 1 do
        begin
          v_ := geo.Surround[j];
          v_^[1] := h - v_^[1];
        end;
      for j := 0 to length(geo.Collapses) - 1 do
        for k := 0 to geo.Collapses[j].count - 1 do
          begin
            v_ := geo.Collapses[j][k];
            v_^[1] := h - v_^[1];
          end;
    end;

  for i := 0 to SegmentationMaskList.count - 1 do
    begin
      seg := SegmentationMaskList[i];
      if seg.FViewerRaster <> nil then
        begin
          seg.FViewerRaster.FlipVert;
          seg.FViewerRaster.Update;
        end;
      seg.Raster.FlipHorz;
      seg.Raster.Update;
    end;

  Raster.FlipVert;
  Raster.Update;
end;

procedure TEditorImageData.SaveToStream_AI(stream: TMemoryStream64);
begin
  SaveToStream_AI(stream, TRasterSaveFormat.rsRGB);
end;

procedure TEditorImageData.SaveToStream_AI(stream: TMemoryStream64; RasterSave_: TRasterSaveFormat);
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

  SegmentationMaskList.RebuildGeometrySegmentationMask(RColor(0, 0, 0, 0), RColor($7F, $7F, $7F, $FF));

  m64 := TMemoryStream64.Create;
  SegmentationMaskList.SaveToStream_AI(m64);
  de.WriteStream(m64);
  DisposeObject(m64);

  de.WriteString(FileInfo);
  de.WriteDouble(CreateTime);
  de.WriteDouble(LastModifyTime);

  de.FastEncodeTo(stream);

  DisposeObject(de);
end;

procedure TEditorImageData.LoadFromStream_AI(stream: TMemoryStream64);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
  i, c: Integer;
  DetDef: TEditorDetectorDefine;
  rObj: TDataFrameBase;
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

  if de.Reader.NotEnd then
    begin
      rObj := de.Reader.Read();
      if rObj is TDataFrameString then
          FileInfo := umlStringOf(TDataFrameString(rObj).Buffer);

      if de.Reader.NotEnd then
        begin
          if de.Reader.Current is TDataFrameDouble then
            begin
              CreateTime := de.Reader.ReadDouble();
              LastModifyTime := de.Reader.ReadDouble();
            end;
        end;
    end;

  DisposeObject(de);
end;

procedure TEditorImageData.SaveToStream(stream: TMemoryStream64; SaveImg: Boolean; RasterSave_: TRasterSaveFormat);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
  i: Integer;
  DetDef: TEditorDetectorDefine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(FileInfo);

  m64 := TMemoryStream64.Create;
  if SaveImg then
      Raster.SaveToStream(m64, RasterSave_);
  de.WriteStream(m64);
  DisposeObject(m64);

  de.WriteRectV2(RasterDrawRect);

  { detector define }
  de.WriteInteger(DetectorDefineList.count);
  for i := 0 to DetectorDefineList.count - 1 do
    begin
      m64 := TMemoryStream64.Create;
      DetDef := DetectorDefineList[i];
      DetDef.SaveToStream(m64);
      de.WriteStream(m64);
      DisposeObject(m64);
    end;

  { geometry }
  m64 := TMemoryStream64.Create;
  GeometryList.SaveToStream(m64);
  de.WriteStream(m64);
  DisposeObject(m64);

  { Segmentation mask }
  m64 := TMemoryStream64.Create;
  SegmentationMaskList.SaveToStream(m64);
  de.WriteStream(m64);
  DisposeObject(m64);

  de.WriteDouble(CreateTime);
  de.WriteDouble(LastModifyTime);

  de.FastEncodeTo(stream);

  DisposeObject(de);
end;

procedure TEditorImageData.SaveToStream(stream: TMemoryStream64; SaveImg: Boolean);
begin
  SaveToStream(stream, SaveImg, TRasterSaveFormat.rsJPEG_YCbCr_Qualily90);
end;

procedure TEditorImageData.LoadFromStream(stream: TMemoryStream64);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
  i, c: Integer;
  DetDef: TEditorDetectorDefine;
  rObj: TDataFrameBase;
begin
  de := TDataFrameEngine.Create;
  de.DecodeFrom(stream);

  FileInfo := de.Reader.ReadString;

  m64 := TMemoryStream64.Create;
  de.Reader.ReadStream(m64);
  if m64.Size > 0 then
    begin
      m64.Position := 0;
      Raster.LoadFromStream(m64);
      Raster.Update;
    end;
  DisposeObject(m64);

  RasterDrawRect := de.Reader.ReadRectV2;

  { detector define }
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

  { Compatibility check zAI 1.16-1.19 }
  if de.Reader.NotEnd then
    begin
      { geometry }
      m64 := TMemoryStream64.Create;
      de.Reader.ReadStream(m64);
      m64.Position := 0;
      GeometryList.LoadFromStream(m64);
      DisposeObject(m64);

      { Segmentation mask }
      m64 := TMemoryStream64.Create;
      de.Reader.ReadStream(m64);
      m64.Position := 0;
      SegmentationMaskList.LoadFromStream(m64);
      DisposeObject(m64);

      { Compatibility check zAI 1.32 last }
      if de.Reader.NotEnd then
        begin
          if de.Reader.Current is TDataFrameDouble then
            begin
              CreateTime := de.Reader.ReadDouble();
              LastModifyTime := de.Reader.ReadDouble();
            end;
        end;
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
    or (Alignment is TAlignment_OD6L)
    or (Alignment is TAlignment_FastOD6L)
    or (Alignment is TAlignment_MMOD6L)
    or (Alignment is TAlignment_FastMMOD6L)
    or (Alignment is TAlignment_MMOD3L)
    or (ai_img.DetectorDefineList.count <> DetectorDefineList.count);

  if needReset then
    begin
      { reset detector dataset }
      for i := 0 to DetectorDefineList.count - 1 do
          DisposeObject(DetectorDefineList[i]);
      DetectorDefineList.Clear;

      { load detector dataset }
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
  ai_img.Raster.Update;

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
  LastLoad_Scale := 1.0;
  LastLoad_pt := Vec2(0, 0);
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

function TEditorImageDataList.GetImageDataFromFileName(FileName: U_String; Width, Height: Integer): TEditorImageData;
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    if FileName.Same(Items[i].FileInfo) and (Items[i].Raster.Width = Width) and (Items[i].Raster.Height = Height) then
      begin
        Result := Items[i];
        exit;
      end;
  Result := nil;
end;

procedure TEditorImageDataList.RunScript(ScriptStyle: TTextStyle; condition_exp, process_exp: SystemString);
var
  i, j: Integer;
  img: TEditorImageData;
  condition_img_ok, condition_det_ok: Boolean;
begin
  { reset state }
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

  { process delete state }
  i := 0;
  while i < count do
    begin
      img := Items[i];

      if img.FOP_RT_RunDeleted then
        begin
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

procedure TEditorImageDataList.SaveToStream(stream: TCoreClassStream; const Scale: TGeoFloat; const pt_: TVec2; SaveImg: Boolean; RasterSave_: TRasterSaveFormat);
var
  de: TDataFrameEngine;
  tmpBuffer: array of TMemoryStream64;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    m64: TMemoryStream64;
    imgData: TEditorImageData;
  begin
    m64 := TMemoryStream64.Create;
    LockObject(Self);
    imgData := Items[pass];
    UnlockObject(Self);
    imgData.SaveToStream(m64, SaveImg, RasterSave_);
    tmpBuffer[pass] := m64;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass: Integer;
    m64: TMemoryStream64;
    imgData: TEditorImageData;
  begin
    for pass := 0 to count - 1 do
      begin
        m64 := TMemoryStream64.Create;
        LockObject(Self);
        imgData := Items[pass];
        UnlockObject(Self);
        imgData.SaveToStream(m64, SaveImg, RasterSave_);
        tmpBuffer[pass] := m64;
      end;
  end;
{$ENDIF Parallel}
  procedure DoFinish();
  var
    i: Integer;
  begin
    for i := 0 to length(tmpBuffer) - 1 do
      begin
        de.WriteStream(tmpBuffer[i]);
        DisposeObjectAndNil(tmpBuffer[i]);
      end;
    SetLength(tmpBuffer, 0);
  end;

begin
  de := TDataFrameEngine.Create;
  de.WriteSingle(Scale);
  de.WriteString(umlFloatToStr(pt_[0]));
  de.WriteString(umlFloatToStr(pt_[1]));
  de.WriteInteger(count);

  SetLength(tmpBuffer, count);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(@Nested_ParallelFor, 0, count - 1);
{$ELSE FPC}
  DelphiParallelFor(0, count - 1, procedure(pass: Integer)
    var
      m64: TMemoryStream64;
      imgData: TEditorImageData;
    begin
      m64 := TMemoryStream64.Create;
      LockObject(Self);
      imgData := Items[pass];
      UnlockObject(Self);
      imgData.SaveToStream(m64, SaveImg, RasterSave_);
      tmpBuffer[pass] := m64;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  DoFinish();
  de.EncodeAsSelectCompressor(TSelectCompressionMethod.scmZLIB_Fast, stream, True);
  DisposeObject(de);
end;

procedure TEditorImageDataList.SaveToStream(stream: TCoreClassStream; const Scale: TGeoFloat; const pt_: TVec2; SaveImg: Boolean);
begin
  SaveToStream(stream, Scale, pt_, SaveImg, TRasterSaveFormat.rsJPEG_YCbCr_Qualily90);
end;

procedure TEditorImageDataList.SaveToStream(stream: TCoreClassStream);
begin
  SaveToStream(stream, LastLoad_Scale, LastLoad_pt, True);
end;

procedure TEditorImageDataList.SaveToFile(FileName: U_String);
var
  stream: TCoreClassStream;
begin
  stream := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(stream);
  finally
      DisposeObject(stream);
  end;
end;

procedure TEditorImageDataList.SaveToFile(FileName: U_String; RasterSave_: TRasterSaveFormat);
var
  stream: TCoreClassStream;
begin
  stream := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(stream, LastLoad_Scale, LastLoad_pt, True, RasterSave_);
  finally
      DisposeObject(stream);
  end;
end;

procedure TEditorImageDataList.LoadFromStream(stream: TCoreClassStream; var Scale: TGeoFloat; var pt_: TVec2);
type
  TPrepareData = record
    stream: TMemoryStream64;
    imgData: TEditorImageData;
  end;
var
  tmpBuffer: array of TPrepareData;

  procedure PrepareData();
  var
    de: TDataFrameEngine;
    i, c: Integer;
  begin
    de := TDataFrameEngine.Create;
    de.DecodeFrom(stream);

    LastLoad_Scale := de.Reader.ReadSingle;
    Scale := LastLoad_Scale;

    LastLoad_pt[0] := umlStrToFloat(de.Reader.ReadString, 0);
    LastLoad_pt[1] := umlStrToFloat(de.Reader.ReadString, 0);
    pt_ := LastLoad_pt;

    c := de.Reader.ReadInteger;
    SetLength(tmpBuffer, c);

    for i := 0 to c - 1 do
      begin
        tmpBuffer[i].stream := TMemoryStream64.Create;
        de.Reader.ReadStream(tmpBuffer[i].stream);
        tmpBuffer[i].stream.Position := 0;
        tmpBuffer[i].imgData := TEditorImageData.Create;
        Add(tmpBuffer[i].imgData);
      end;
    DisposeObject(de);
  end;

  procedure FreePrepareData();
  var
    i: Integer;
  begin
    for i := 0 to length(tmpBuffer) - 1 do
        DisposeObject(tmpBuffer[i].stream);
    SetLength(tmpBuffer, 0);
  end;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    tmpBuffer[pass].imgData.LoadFromStream(tmpBuffer[pass].stream);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to length(tmpBuffer) - 1 do
      begin
        tmpBuffer[pass].imgData.LoadFromStream(tmpBuffer[pass].stream);
      end;
  end;
{$ENDIF Parallel}


begin
  PrepareData();

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(@Nested_ParallelFor, 0, length(tmpBuffer) - 1);
{$ELSE FPC}
  DelphiParallelFor(0, length(tmpBuffer) - 1, procedure(pass: Integer)
    begin
      tmpBuffer[pass].imgData.LoadFromStream(tmpBuffer[pass].stream);
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  FreePrepareData();
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
    fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    LoadFromStream(fs);
    DisposeObject(fs);
  except
  end;
end;

procedure TEditorImageDataList.SaveToStream_AI(stream: TCoreClassStream; RasterSaveMode: TRasterSaveFormat);
var
  de: TDataFrameEngine;
  tmpBuffer: array of TMemoryStream64;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    m64: TMemoryStream64;
    imgData: TEditorImageData;
  begin
    m64 := TMemoryStream64.Create;
    LockObject(Self);
    imgData := Items[pass];
    UnlockObject(Self);
    imgData.SaveToStream_AI(m64, RasterSaveMode);
    tmpBuffer[pass] := m64;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass: Integer;
    m64: TMemoryStream64;
    imgData: TEditorImageData;
  begin
    for pass := 0 to count - 1 do
      begin
        m64 := TMemoryStream64.Create;
        LockObject(Self);
        imgData := Items[pass];
        UnlockObject(Self);
        imgData.SaveToStream_AI(m64, RasterSaveMode);
        tmpBuffer[pass] := m64;
      end;
  end;
{$ENDIF Parallel}
  procedure DoFinish();
  var
    i: Integer;
  begin
    for i := 0 to length(tmpBuffer) - 1 do
      begin
        de.WriteStream(tmpBuffer[i]);
        DisposeObjectAndNil(tmpBuffer[i]);
      end;
    SetLength(tmpBuffer, 0);
  end;

begin
  de := TDataFrameEngine.Create;
  de.WriteInteger(count);

  SetLength(tmpBuffer, count);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(@Nested_ParallelFor, 0, count - 1);
{$ELSE FPC}
  DelphiParallelFor(0, count - 1, procedure(pass: Integer)
    var
      m64: TMemoryStream64;
      imgData: TEditorImageData;
    begin
      m64 := TMemoryStream64.Create;
      LockObject(Self);
      imgData := Items[pass];
      UnlockObject(Self);
      imgData.SaveToStream_AI(m64, RasterSaveMode);
      tmpBuffer[pass] := m64;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  DoFinish();
  de.EncodeAsSelectCompressor(TSelectCompressionMethod.scmZLIB, stream, True);
  DisposeObject(de);
end;

procedure TEditorImageDataList.SaveToFile_AI(FileName: U_String; RasterSaveMode: TRasterSaveFormat);
var
  stream: TCoreClassStream;
begin
  stream := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream_AI(stream, RasterSaveMode);
  finally
      DisposeObject(stream);
  end;
end;

procedure TEditorImageDataList.LoadFromStream_AI(stream: TCoreClassStream);
type
  TPrepareData = record
    stream: TMemoryStream64;
    imgData: TEditorImageData;
  end;
var
  tmpBuffer: array of TPrepareData;

  procedure PrepareData();
  var
    de: TDataFrameEngine;
    i, c: Integer;
  begin
    de := TDataFrameEngine.Create;
    de.DecodeFrom(stream);
    c := de.Reader.ReadInteger;
    SetLength(tmpBuffer, c);

    for i := 0 to c - 1 do
      begin
        tmpBuffer[i].stream := TMemoryStream64.Create;
        de.Reader.ReadStream(tmpBuffer[i].stream);
        tmpBuffer[i].stream.Position := 0;
        tmpBuffer[i].imgData := TEditorImageData.Create;
        Add(tmpBuffer[i].imgData);
      end;
    DisposeObject(de);
  end;

  procedure FreePrepareData();
  var
    i: Integer;
  begin
    for i := 0 to length(tmpBuffer) - 1 do
        DisposeObject(tmpBuffer[i].stream);
    SetLength(tmpBuffer, 0);
  end;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    tmpBuffer[pass].imgData.LoadFromStream_AI(tmpBuffer[pass].stream);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to length(tmpBuffer) - 1 do
      begin
        tmpBuffer[pass].imgData.LoadFromStream_AI(tmpBuffer[pass].stream);
      end;
  end;
{$ENDIF Parallel}


begin
  PrepareData();

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(@Nested_ParallelFor, 0, length(tmpBuffer) - 1);
{$ELSE FPC}
  DelphiParallelFor(0, length(tmpBuffer) - 1, procedure(pass: Integer)
    begin
      tmpBuffer[pass].imgData.LoadFromStream_AI(tmpBuffer[pass].stream);
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  FreePrepareData();
end;

procedure TEditorImageDataList.LoadFromFile_AI(FileName: U_String);
var
  stream: TCoreClassStream;
begin
  stream := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
      LoadFromStream_AI(stream);
  finally
      DisposeObject(stream);
  end;
end;

procedure TEditorImageDataList.SaveToStream_ImgMat(stream: TCoreClassStream; RasterSaveMode: TRasterSaveFormat);
  procedure DoSave(stream_: TCoreClassStream; index_: Integer);
  var
    de: TDataFrameEngine;
    m64: TMemoryStream64;
    imgData: TEditorImageData;
  begin
    de := TDataFrameEngine.Create;

    de.WriteInteger(1);

    m64 := TMemoryStream64.Create;
    imgData := Items[index_];
    imgData.SaveToStream_AI(m64, RasterSaveMode);
    de.WriteStream(m64);
    DisposeObject(m64);

    de.EncodeTo(stream_, True);
    DisposeObject(de);
  end;

var
  dbEng: TObjectDataManager;
  i: Integer;
  m64: TMemoryStream64;
  fn: U_String;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  dbEng := TObjectDataManagerOfCache.CreateAsStream(stream, '', DBMarshal.ID, False, True, False);

  for i := 0 to count - 1 do
    begin
      m64 := TMemoryStream64.Create;
      DoSave(m64, i);
      fn := Items[i].FileInfo;
      if fn.Len = 0 then
          fn := umlStreamMD5String(m64);

      fn.Append(C_ImageList_Ext);

      dbEng.ItemFastCreate(dbEng.RootField, fn, 'ImageMatrix', itmHnd);
      itmStream := TItemStream.Create(dbEng, itmHnd);
      m64.Position := 0;
      itmStream.CopyFrom(m64, m64.Size);
      DisposeObject(m64);
      itmStream.UpdateHandle;
      dbEng.ItemClose(itmHnd);
      DisposeObject(itmStream);
    end;
  DisposeObject(dbEng);
  DoStatus('Save Image Matrix done.');
end;

procedure TEditorImageDataList.SaveToFile_ImgMat(FileName: U_String; RasterSaveMode: TRasterSaveFormat);
var
  stream: TCoreClassStream;
begin
  stream := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream_ImgMat(stream, RasterSaveMode);
  finally
      DisposeObject(stream);
  end;
end;

procedure TEditorImageDataList.LoadFromStream_ImgMat(stream: TCoreClassStream);
var
  dbEng: TObjectDataManager;
  fPos: Int64;
  PrepareLoadBuffer: TCoreClassList;
  itmSR: TItemSearch;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  dbEng := TObjectDataManagerOfCache.CreateAsStream(stream, '', DBMarshal.ID, True, False, False);

  if dbEng.ItemFastFindFirst(dbEng.RootField, '', itmSR) then
    begin
      repeat
        if umlMultipleMatch('*' + C_ImageList_Ext, itmSR.Name) then
          begin
            dbEng.ItemFastOpen(itmSR.HeaderPOS, itmHnd);
            itmStream := TItemStream.Create(dbEng, itmHnd);
            LoadFromStream_AI(itmStream);
            DisposeObject(itmStream);
            dbEng.ItemClose(itmHnd);
          end;
      until not dbEng.ItemFindNext(itmSR);
    end;
  DisposeObject(dbEng);
end;

procedure TEditorImageDataList.LoadFromFile_ImgMat(FileName: U_String);
var
  stream: TCoreClassStream;
begin
  stream := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
      LoadFromStream_ImgMat(stream);
  finally
      DisposeObject(stream);
  end;
end;

initialization

On_Editor_Script_RegisterProc := nil;

end.
