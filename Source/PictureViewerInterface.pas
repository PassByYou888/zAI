{ ****************************************************************************** }
{ * picture viewer tool                                                        * }
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
unit PictureViewerInterface;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  PascalStrings, UnicodeMixedLib, DoStatusIO,
  zDrawEngine, Geometry2DUnit, Geometry3DUnit, TextParsing,
  MemoryRaster, MemoryStream64;

type
  TPictureViewerData = class;
  TRasterHistogramInfos = class;
  TPictureViewerInterface = class;

  TRasterHistogramInfo = class
  public
    Parent: TRasterHistogramInfos;
    info: SystemString;
    color: TRColor;
    Raster: TMemoryRaster;
    Busy: TAtomBool;
    DrawBox: TRectV2;
    MorphData: TMorphomatics;
    procedure BuildHisComputeTh(thSender: TCompute);
    procedure UpdateHisComputeTh(thSender: TCompute);
    constructor Create(Parent_: TRasterHistogramInfos; raster_: TMemoryRaster; MorphPix_: TMorphologyPixel; Height_: Integer; hColor: TRColor);
    destructor Destroy; override;
    procedure Update(MorphPix_: TMorphologyPixel; Height_: Integer; hColor: TRColor);
  end;

  TRasterHistogramInfoArray = array of TRasterHistogramInfo;

  TRasterHistogramInfos = class
  public
    Parent: TPictureViewerData;
    GrayAndYIQ: TRasterHistogramInfoArray;
    HSI: TRasterHistogramInfoArray;
    CMYK: TRasterHistogramInfoArray;
    RGBA: TRasterHistogramInfoArray;
    Approximate: TRasterHistogramInfoArray;

    function Busy(): Boolean;
    constructor Create(Parent_: TPictureViewerData; raster_: TMemoryRaster);
    destructor Destroy; override;
    procedure Update;
    procedure WaitThread();
    procedure Draw(d: TDrawEngine);
    class procedure DrawArry(Box: TRectV2; arry: TRasterHistogramInfoArray; d: TDrawEngine);
    class procedure FreeArry(var arry: TRasterHistogramInfoArray);
  end;

  TPictureViewerData = class
  public
    Owner: TPictureViewerInterface;
    Raster: TMemoryRaster;
    FreeRaster: Boolean;
    DrawBox: TRectV2;
    texInfo: U_String;
    hInfo: TRasterHistogramInfos;
    UserData: Pointer;
    UserObject: TCoreClassObject;
    constructor Create;
    destructor Destroy; override;
  end;

  TPictureViewerDataList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TPictureViewerData>;
  TRasterHistogramInfoList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TRasterHistogramInfo>;

  TPictureViewerStyle = (pvsTop2Bottom, pvsLeft2Right, pvsDynamic);

  TPictureViewerInterface = class
  private
    FPictureDataList: TPictureViewerDataList;
    FDrawEng: TDrawEngine;
    FDownScreenPT, FDownScenePT: TVec2;
    FMoveScreenPT, FMoveScenePT: TVec2;
    FUpScreenPT, FUpScenePT: TVec2;
    FDownState: Boolean;
    FShowHistogramInfo: Boolean;
    FShowPixelInfo: Boolean;
    FShowPixelInfoFontSize: TGeoFloat;
    FShowPictureInfo: Boolean;
    FShowPictureInfoFontSize: TGeoFloat;
    FShowBackground: Boolean;
    FPictureViewerStyle: TPictureViewerStyle;
    FBackgroundTex: TMemoryRaster;
  public
    constructor Create(DrawEng_: TDrawEngine);
    destructor Destroy; override;
    property DrawEng: TDrawEngine read FDrawEng write FDrawEng;

    procedure WaitThread;
    function InputPicture(Raster: TMemoryRaster; Instance_, AutoFree_: Boolean): TPictureViewerData; overload;
    function InputPicture(Raster: TMemoryRaster; Instance_: Boolean): TPictureViewerData; overload;
    function InputPicture(Raster: TMemoryRaster; texInfo: U_String; Instance_: Boolean): TPictureViewerData; overload;
    function InputPicture(Raster: TMemoryRaster; texInfo: U_String; Instance_, ShowHistogramInfo_: Boolean): TPictureViewerData; overload;
    function InputPicture(Raster: TMemoryRaster; texInfo: U_String; Instance_, ShowHistogramInfo_, AutoFree_: Boolean): TPictureViewerData; overload;
    procedure BuildMorphViewData;
    procedure Clear;
    procedure Delete(index: Integer);
    procedure Remove(obj: TPictureViewerData);
    function Count: Integer;
    function GetItems(index: Integer): TPictureViewerData;
    property Items[index: Integer]: TPictureViewerData read GetItems; default;
    function First: TPictureViewerData;
    function Last: TPictureViewerData;
    function FoundPicture(Raster: TMemoryRaster): TPictureViewerData;

    function AtPicture(pt: TVec2): TPictureViewerData;
    function AtPictureOffset(data_: TPictureViewerData; pt: TVec2): TPoint;

    // tap
    procedure TapDown(pt: TVec2);
    procedure TapMove(pt: TVec2);
    procedure TapUp(pt: TVec2);
    // tap state
    property DownScreenPT: TVec2 read FDownScreenPT;
    property DownScenePT: TVec2 read FDownScenePT;
    property MoveScreenPT: TVec2 read FMoveScreenPT;
    property MoveScenePT: TVec2 read FMoveScenePT;
    property UpScreenPT: TVec2 read FUpScreenPT;
    property UpScenePT: TVec2 read FUpScenePT;
    property DownState: Boolean read FDownState;

    // scale
    procedure ScaleCamera(f: TGeoFloat);
    procedure ScaleCameraFromWheelDelta(WheelDelta: Integer);

    // prepare
    procedure ComputeDrawBox();
    procedure Fit(Box: TRectV2); overload;
    procedure Fit(); overload;

    // renderer
    procedure Render(showPicture_, flush_: Boolean); overload;
    procedure Render(showPicture_: Boolean); overload;
    procedure Render(); overload;
    procedure Flush();

    // viewer options
    property ShowHistogramInfo: Boolean read FShowHistogramInfo write FShowHistogramInfo;
    property ShowPixelInfo: Boolean read FShowPixelInfo write FShowPixelInfo;
    property ShowPictureInfo: Boolean read FShowPictureInfo write FShowPictureInfo;
    property ShowPictureInfoFontSize: TGeoFloat read FShowPictureInfoFontSize write FShowPictureInfoFontSize;
    property ShowBackground: Boolean read FShowBackground write FShowBackground;
    property PictureViewerStyle: TPictureViewerStyle read FPictureViewerStyle write FPictureViewerStyle;
    property BackgroundTex: TMemoryRaster read FBackgroundTex;
  end;

implementation

type
  TRasterHistogramThData = record
    raster_: TMemoryRaster;
    MorphPix_: TMorphologyPixel;
    Height_: Integer;
    hColor: TRColor;
  end;

  PRasterHistogramThData = ^TRasterHistogramThData;

procedure TRasterHistogramInfo.BuildHisComputeTh(thSender: TCompute);
var
  p: PRasterHistogramThData;
  r: TMemoryRaster;
begin
  p := thSender.UserData;
  MorphData := p^.raster_.BuildMorphomatics(p^.MorphPix_);
  r := MorphData.BuildHistogram(p^.Height_, p^.hColor);
  dispose(p);
  Raster := r;
  Busy.V := False;
end;

procedure TRasterHistogramInfo.UpdateHisComputeTh(thSender: TCompute);
var
  p: PRasterHistogramThData;
begin
  p := thSender.UserData;
  p^.raster_.BuildMorphomaticsTo(p^.MorphPix_, MorphData);
  MorphData.BuildHistogramTo(p^.Height_, p^.hColor, Raster);
  dispose(p);
  if Raster is TDETexture then
      TDETexture(Raster).ReleaseGPUMemory;
  Busy.V := False;
end;

constructor TRasterHistogramInfo.Create(Parent_: TRasterHistogramInfos; raster_: TMemoryRaster; MorphPix_: TMorphologyPixel; Height_: Integer; hColor: TRColor);
{$IFDEF Parallel}
var
  p: PRasterHistogramThData;
{$ENDIF Parallel}
begin
  inherited Create;
  Parent := Parent_;
  info := PFormat('%d: %s', [Integer(MorphPix_), CMorphologyPixelInfo[MorphPix_]]);
  color := hColor;
  DrawBox := RectV2(0, 0, 0, 0);
  MorphData := nil;
  Busy := TAtomBool.Create(True);
{$IFDEF Parallel}
  Raster := nil;
  new(p);
  p^.raster_ := raster_;
  p^.MorphPix_ := MorphPix_;
  p^.Height_ := Height_;
  p^.hColor := hColor;
  TCompute.RunM(p, nil, {$IFDEF FPC}@{$ENDIF FPC}BuildHisComputeTh);

{$ELSE Parallel}
    MorphData := raster_.BuildMorphomatics(MorphPix_);
  Raster := MorphData.BuildHistogram(Height_, hColor);
  Busy.V := False;
{$ENDIF Parallel}
end;

destructor TRasterHistogramInfo.Destroy;
begin
  while Busy.V do
      TCoreClassThread.Sleep(10);
  DisposeObjectAndNil(Raster);
  DisposeObjectAndNil(MorphData);
  DisposeObject(Busy);
  inherited Destroy;
end;

procedure TRasterHistogramInfo.Update(MorphPix_: TMorphologyPixel; Height_: Integer; hColor: TRColor);
{$IFDEF Parallel}
var
  p: PRasterHistogramThData;
{$ENDIF Parallel}
begin
  while Busy.V do
      TCoreClassThread.Sleep(10);

  info := PFormat('%d: %s', [Integer(MorphPix_), CMorphologyPixelInfo[MorphPix_]]);
  color := hColor;
  DrawBox := RectV2(0, 0, 0, 0);
  Busy.V := True;
{$IFDEF Parallel}
  new(p);
  p^.raster_ := Parent.Parent.Raster;
  p^.MorphPix_ := MorphPix_;
  p^.Height_ := Height_;
  p^.hColor := hColor;
  TCompute.RunM(p, nil, {$IFDEF FPC}@{$ENDIF FPC}UpdateHisComputeTh);
{$ELSE Parallel}
    Parent.Parent.Raster.BuildMorphomaticsTo(MorphPix_, MorphData);
  MorphData.BuildHistogramTo(Height_, hColor, Raster);
  Busy.V := False;
{$ENDIF Parallel}
end;

function TRasterHistogramInfos.Busy(): Boolean;
var
  obj: TRasterHistogramInfo;
begin
  Result := True;
  for obj in GrayAndYIQ do
    if obj.Busy.V then
        exit;
  for obj in HSI do
    if obj.Busy.V then
        exit;
  for obj in CMYK do
    if obj.Busy.V then
        exit;
  for obj in RGBA do
    if obj.Busy.V then
        exit;
  for obj in Approximate do
    if obj.Busy.V then
        exit;
  Result := False;
end;

constructor TRasterHistogramInfos.Create(Parent_: TPictureViewerData; raster_: TMemoryRaster);
begin
  inherited Create;
  Parent := Parent_;
  SetLength(GrayAndYIQ, 4);
  GrayAndYIQ[0] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpGrayscale, 100, RColor($7F, $7F, $7F));
  GrayAndYIQ[1] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpYIQ_Y, 100, RColor($7F, $7F, $7F));
  GrayAndYIQ[2] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpYIQ_I, 100, RColor($A0, $20, $F0));
  GrayAndYIQ[3] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpYIQ_Q, 100, RColor($00, $FF, $FF));

  SetLength(HSI, 3);
  HSI[0] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpHSI_H, 100, RColor($FF, $7F, $7F));
  HSI[1] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpHSI_S, 100, RColor($00, $7F, $7F));
  HSI[2] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpHSI_I, 100, RColor($7F, $7F, $7F));

  SetLength(CMYK, 4);
  CMYK[0] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpCMYK_C, 100, RColor($00, $FF, $FF));
  CMYK[1] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpCMYK_M, 100, RColor($E4, $00, $7F));
  CMYK[2] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpCMYK_Y, 100, RColor($FF, $DA, $B9));
  CMYK[3] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpCMYK_K, 100, RColor($7F, $7F, $7F));

  SetLength(RGBA, 4);
  RGBA[0] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpR, 100, RColor($FF, $7F, $7F));
  RGBA[1] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpG, 100, RColor($7F, $FF, $7F));
  RGBA[2] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpB, 100, RColor($7F, $7F, $FF));
  RGBA[3] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpA, 100, RColor($7F, $7F, $7F));

  SetLength(Approximate, 5);
  Approximate[0] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpApproximateBlack, 100, RColor($7F, $7F, $7F));
  Approximate[1] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpApproximateWhite, 100, RColor($7F, $F, $7F));
  Approximate[2] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpCyan, 100, RColor($7F, $7F, $7F));
  Approximate[3] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpMagenta, 100, RColor($7F, $7F, $7F));
  Approximate[4] := TRasterHistogramInfo.Create(Self, raster_, TMorphologyPixel.mpYellow, 100, RColor($7F, $7F, $7F));
end;

destructor TRasterHistogramInfos.Destroy;
begin
  FreeArry(GrayAndYIQ);
  FreeArry(HSI);
  FreeArry(CMYK);
  FreeArry(RGBA);
  inherited Destroy;
end;

procedure TRasterHistogramInfos.Update;
begin
  GrayAndYIQ[0].Update(TMorphologyPixel.mpGrayscale, 100, RColor($7F, $7F, $7F));
  GrayAndYIQ[1].Update(TMorphologyPixel.mpYIQ_Y, 100, RColor($7F, $7F, $7F));
  GrayAndYIQ[2].Update(TMorphologyPixel.mpYIQ_I, 100, RColor($A0, $20, $F0));
  GrayAndYIQ[3].Update(TMorphologyPixel.mpYIQ_Q, 100, RColor($00, $FF, $FF));

  HSI[0].Update(TMorphologyPixel.mpHSI_H, 100, RColor($FF, $7F, $7F));
  HSI[1].Update(TMorphologyPixel.mpHSI_S, 100, RColor($00, $7F, $7F));
  HSI[2].Update(TMorphologyPixel.mpHSI_I, 100, RColor($7F, $7F, $7F));

  CMYK[0].Update(TMorphologyPixel.mpCMYK_C, 100, RColor($00, $FF, $FF));
  CMYK[1].Update(TMorphologyPixel.mpCMYK_M, 100, RColor($E4, $00, $7F));
  CMYK[2].Update(TMorphologyPixel.mpCMYK_Y, 100, RColor($FF, $DA, $B9));
  CMYK[3].Update(TMorphologyPixel.mpCMYK_K, 100, RColor($7F, $7F, $7F));

  RGBA[0].Update(TMorphologyPixel.mpR, 100, RColor($FF, $7F, $7F));
  RGBA[1].Update(TMorphologyPixel.mpG, 100, RColor($7F, $FF, $7F));
  RGBA[2].Update(TMorphologyPixel.mpB, 100, RColor($7F, $7F, $FF));
  RGBA[3].Update(TMorphologyPixel.mpA, 100, RColor($7F, $7F, $7F));

  Approximate[0].Update(TMorphologyPixel.mpApproximateBlack, 100, RColor($7F, $7F, $7F));
  Approximate[1].Update(TMorphologyPixel.mpApproximateWhite, 100, RColor($7F, $7F, $7F));
  Approximate[2].Update(TMorphologyPixel.mpCyan, 100, RColor($7F, $7F, $7F));
  Approximate[3].Update(TMorphologyPixel.mpMagenta, 100, RColor($7F, $7F, $7F));
  Approximate[4].Update(TMorphologyPixel.mpYellow, 100, RColor($7F, $7F, $7F));
end;

procedure TRasterHistogramInfos.WaitThread;
begin
  while Busy() do
      TCoreClassThread.Sleep(10);
end;

procedure TRasterHistogramInfos.Draw(d: TDrawEngine);
var
  r: TRectV2;
  arry: TRasterHistogramInfoArray;
  obj: TRasterHistogramInfo;
  i: Integer;
begin
  r := d.ScreenRect;

  SetLength(arry, Length(GrayAndYIQ) + Length(HSI) + Length(CMYK) + Length(RGBA) + Length(Approximate));
  i := 0;
  for obj in GrayAndYIQ do
    begin
      arry[i] := obj;
      inc(i);
    end;
  for obj in HSI do
    begin
      arry[i] := obj;
      inc(i);
    end;
  for obj in CMYK do
    begin
      arry[i] := obj;
      inc(i);
    end;
  for obj in RGBA do
    begin
      arry[i] := obj;
      inc(i);
    end;
  for obj in Approximate do
    begin
      arry[i] := obj;
      inc(i);
    end;

  r[0, 0] := r[0, 0] + 10;
  r[0, 1] := r[1, 1] - 80;
  DrawArry(r, arry, d);
  SetLength(arry, 0);
end;

class procedure TRasterHistogramInfos.DrawArry(Box: TRectV2; arry: TRasterHistogramInfoArray; d: TDrawEngine);
var
  w, h: TGeoFloat;
  Len_, i: Integer;
  pic_r, r: TRectV2;
  n: U_String;
  texSiz: TVec2;
  text_r: TRectV2;
begin
  Len_ := Length(arry);
  w := (RectWidth(Box) - (Len_ * 10)) / (Len_);
  h := RectHeight(Box);
  pic_r[0] := Box[0];
  for i := 0 to Len_ - 1 do
    if not arry[i].Busy.V then
      begin
        pic_r[1] := Vec2Add(pic_r[0], vec2(w, h));
        n := arry[i].info;

        r := d.FitDrawPicture(arry[i].Raster, arry[i].Raster.BoundsRectV2, pic_r, 1.0);
        d.DrawBox(RectEdge(r, 1), DEColor(1, 1, 1), 1);
        arry[i].DrawBox := r;

        texSiz := d.GetTextSize(n, 10);
        while (texSiz[0] > RectWidth(r)) and (n.L > 0) do
          begin
            texSiz := d.GetTextSize(n, 10);
            n.DeleteLast;
          end;

        text_r[0, 0] := r[0, 0];
        text_r[0, 1] := r[0, 1] - 2 - texSiz[1];
        text_r[1] := Vec2Add(text_r[0], texSiz);
        d.DrawText(n, 10, text_r, RColor2DColor(arry[i].color), False);

        pic_r[0, 0] := pic_r[0, 0] + w + 10;
      end;
end;

class procedure TRasterHistogramInfos.FreeArry(var arry: TRasterHistogramInfoArray);
var
  i: Integer;
begin
  for i := Low(arry) to high(arry) do
      DisposeObject(arry[i]);
  SetLength(arry, 0);
end;

constructor TPictureViewerData.Create;
begin
  inherited Create;
  Owner := nil;
  Raster := NewRaster();
  FreeRaster := True;
  DrawBox := RectV2(0, 0, 0, 0);
  texInfo := '';
  hInfo := nil;
  UserData := nil;
  UserObject := nil;
end;

destructor TPictureViewerData.Destroy;
begin
  while (hInfo <> nil) and (hInfo.Busy()) do
      TCoreClassThread.Sleep(10);
  DisposeObjectAndNil(hInfo);

  if FreeRaster then
      DisposeObjectAndNil(Raster);
  inherited Destroy;
end;

constructor TPictureViewerInterface.Create(DrawEng_: TDrawEngine);
begin
  inherited Create;
  FPictureDataList := TPictureViewerDataList.Create;
  FDrawEng := DrawEng_;
  FDownScreenPT := vec2(0, 0);
  FDownScenePT := vec2(0, 0);
  FMoveScreenPT := FDownScreenPT;
  FMoveScenePT := FDownScenePT;
  FUpScreenPT := FMoveScreenPT;
  FUpScenePT := FUpScreenPT;
  FDownState := False;
  FShowPixelInfo := False;
  FShowPixelInfoFontSize := 12;
  FShowPictureInfo := True;
  FShowPictureInfoFontSize := 24;
  FShowBackground := True;
  FShowHistogramInfo := False;
  FPictureViewerStyle := TPictureViewerStyle.pvsLeft2Right;
  FBackgroundTex := NewRaster();
  FBackgroundTex.SetSize($FF, $FF, RColor(0, 0, 0));
  FillBlackGrayBackgroundTexture(FBackgroundTex, 64);
end;

destructor TPictureViewerInterface.Destroy;
begin
  WaitThread();
  Clear;
  DisposeObject(FPictureDataList);
  DisposeObject(FBackgroundTex);
  inherited Destroy;
end;

procedure TPictureViewerInterface.WaitThread;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
      while (hInfo <> nil) and (hInfo.Busy()) do
          TCoreClassThread.Sleep(10);
end;

function TPictureViewerInterface.InputPicture(Raster: TMemoryRaster; Instance_, AutoFree_: Boolean): TPictureViewerData;
begin
  Result := InputPicture(Raster, '', Instance_, True);
  if Instance_ then
      Result.FreeRaster := AutoFree_;
end;

function TPictureViewerInterface.InputPicture(Raster: TMemoryRaster; Instance_: Boolean): TPictureViewerData;
begin
  Result := InputPicture(Raster, '', Instance_, True);
end;

function TPictureViewerInterface.InputPicture(Raster: TMemoryRaster; texInfo: U_String; Instance_: Boolean): TPictureViewerData;
begin
  Result := InputPicture(Raster, texInfo, Instance_, True);
end;

function TPictureViewerInterface.InputPicture(Raster: TMemoryRaster; texInfo: U_String; Instance_, ShowHistogramInfo_: Boolean): TPictureViewerData;
begin
  Result := InputPicture(Raster, texInfo, Instance_, ShowHistogramInfo_, False);
end;

function TPictureViewerInterface.InputPicture(Raster: TMemoryRaster; texInfo: U_String; Instance_, ShowHistogramInfo_, AutoFree_: Boolean): TPictureViewerData;
var
  sData: TPictureViewerData;
begin
  sData := TPictureViewerData.Create;
  FPictureDataList.Add(sData);
  sData.Owner := Self;
  if Instance_ then
    begin
      DisposeObjectAndNil(sData.Raster);
      sData.Raster := Raster;
      sData.FreeRaster := AutoFree_;
    end
  else
      sData.Raster.Assign(Raster);
  sData.texInfo := texInfo;

  if ShowHistogramInfo_ and FShowHistogramInfo then
      sData.hInfo := TRasterHistogramInfos.Create(sData, sData.Raster);

  Result := sData;
end;

procedure TPictureViewerInterface.BuildMorphViewData;
var
  i: Integer;
  sData: TPictureViewerData;
  h: TRasterHistogramInfo;
  L: TRasterHistogramInfoList;
begin
  WaitThread();
  if Count < Integer(High(TMorphologyPixel)) then
    begin
      for i := 0 to Count - 1 do
        begin
          sData := Items[i];
          if (sData.hInfo = nil) then
              sData.hInfo := TRasterHistogramInfos.Create(sData, sData.Raster);
        end;
    end;
  WaitThread();

  L := TRasterHistogramInfoList.Create;
  for i := 0 to Count - 1 do
    begin
      sData := Items[i];
      if (sData.hInfo <> nil) and (not sData.hInfo.Busy) then
        begin
          for h in sData.hInfo.GrayAndYIQ do
              L.Add(h);
          for h in sData.hInfo.HSI do
              L.Add(h);
          for h in sData.hInfo.CMYK do
              L.Add(h);
          for h in sData.hInfo.RGBA do
              L.Add(h);
          for h in sData.hInfo.Approximate do
              L.Add(h);
        end;
    end;

  for i := 0 to L.Count - 1 do
      InputPicture(L[i].MorphData.BuildViewer(), L[i].info, True, False);

  DisposeObject(L);
end;

procedure TPictureViewerInterface.Clear;
var
  i: Integer;
begin
  for i := 0 to FPictureDataList.Count - 1 do
      DisposeObject(FPictureDataList[i]);
  FPictureDataList.Clear;
end;

procedure TPictureViewerInterface.Delete(index: Integer);
begin
  DisposeObject(FPictureDataList[index]);
  FPictureDataList.Delete(index);
end;

procedure TPictureViewerInterface.Remove(obj: TPictureViewerData);
begin
  DisposeObject(obj);
  FPictureDataList.Remove(obj);
end;

function TPictureViewerInterface.Count: Integer;
begin
  Result := FPictureDataList.Count;
end;

function TPictureViewerInterface.GetItems(index: Integer): TPictureViewerData;
begin
  Result := FPictureDataList[index];
end;

function TPictureViewerInterface.First: TPictureViewerData;
begin
  Result := Items[0];
end;

function TPictureViewerInterface.Last: TPictureViewerData;
begin
  Result := Items[Count - 1];
end;

function TPictureViewerInterface.FoundPicture(Raster: TMemoryRaster): TPictureViewerData;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Raster = Raster then
      begin
        Result := Items[i];
        exit;
      end;
  Result := nil;
end;

function TPictureViewerInterface.AtPicture(pt: TVec2): TPictureViewerData;
var
  i: Integer;
  sData: TPictureViewerData;
begin
  WaitThread();
  Result := nil;
  if FDrawEng = nil then
      exit;
  for i := 0 to Count - 1 do
    begin
      sData := Items[i];
      if Vec2InRect(pt, FDrawEng.SceneToScreen(sData.DrawBox)) then
          exit(sData);
    end;
end;

function TPictureViewerInterface.AtPictureOffset(data_: TPictureViewerData; pt: TVec2): TPoint;
begin
  WaitThread();
  Result := TPoint.Create(0, 0);
  if FDrawEng = nil then
      exit;
  Result := MakePoint(RectProjection(FDrawEng.SceneToScreen(data_.DrawBox), data_.Raster.BoundsRectV2, pt));
end;

procedure TPictureViewerInterface.TapDown(pt: TVec2);
begin
  if FDrawEng = nil then
      exit;
  FDownScreenPT := pt;
  FDownScenePT := FDrawEng.ScreenToScene(pt);
  FMoveScreenPT := FDownScreenPT;
  FMoveScenePT := FDownScenePT;
  FUpScreenPT := FMoveScreenPT;
  FUpScenePT := FUpScreenPT;
  FDownState := True;
end;

procedure TPictureViewerInterface.TapMove(pt: TVec2);
begin
  if FDrawEng = nil then
      exit;

  if FDownState then
    begin
      FDrawEng.Offset := Vec2Add(FDrawEng.Offset, Vec2Sub(pt, FMoveScreenPT));
    end;

  FMoveScreenPT := pt;
  FMoveScenePT := FDrawEng.ScreenToScene(pt);
  FUpScreenPT := FMoveScreenPT;
  FUpScenePT := FUpScreenPT;
end;

procedure TPictureViewerInterface.TapUp(pt: TVec2);
begin
  if FDrawEng = nil then
      exit;
  FUpScreenPT := pt;
  FUpScenePT := FDrawEng.ScreenToScene(pt);
  FDownState := False;
end;

procedure TPictureViewerInterface.ScaleCamera(f: TGeoFloat);
begin
  if FDrawEng = nil then
      exit;

  FDrawEng.ScaleCamera(f);
end;

procedure TPictureViewerInterface.ScaleCameraFromWheelDelta(WheelDelta: Integer);
begin
  if FDrawEng = nil then
      exit;
  FDrawEng.ScaleCameraFromWheelDelta(WheelDelta);
end;

procedure TPictureViewerInterface.ComputeDrawBox;
var
  j: Integer;
  tmpRect: TRectV2;
  sData: TPictureViewerData;
  rp: TRectPacking;
begin
  if FPictureViewerStyle = pvsDynamic then
    begin
      rp := TRectPacking.Create;
      for j := 0 to Count - 1 do
        begin
          sData := Items[j];
          rp.Add(nil, sData, sData.Raster.BoundsRectV2);
        end;
      rp.Margins := 10;
      rp.Build();
      for j := 0 to rp.Count - 1 do
          TPictureViewerData(rp[j]^.Data2).DrawBox := rp[j]^.Rect;
      DisposeObject(rp);
    end
  else
    begin
      tmpRect[0] := vec2(0, 0);
      for j := 0 to Count - 1 do
        begin
          sData := Items[j];
          tmpRect[1] := Vec2Add(tmpRect[0], sData.Raster.Size2D);
          sData.DrawBox := tmpRect;
          case FPictureViewerStyle of
            pvsTop2Bottom: tmpRect[0, 1] := tmpRect[1, 1] + 10;
            pvsLeft2Right: tmpRect[0, 0] := tmpRect[1, 0] + 10;
          end;
        end;
    end;
end;

procedure TPictureViewerInterface.Fit(Box: TRectV2);
begin
  FDrawEng.CameraR := Box;
end;

procedure TPictureViewerInterface.Fit;
var
  i: Integer;
  Box: TRectV2;
begin
  if Count > 0 then
    begin
      ComputeDrawBox();
      Box := Items[0].DrawBox;
      for i := 1 to Count - 1 do
          Box := BoundRect(Box, Items[i].DrawBox);
      Fit(Box);
    end
  else
    begin
      FDrawEng.ResetCamera;
    end;
end;

procedure TPictureViewerInterface.Render(showPicture_, flush_: Boolean);
var
  i, j: Integer;
  hisRect: TRectV2;
  sData: TPictureViewerData;
  text_siz, tex_pt: TVec2;
  n: U_String;
  tex_color: TRColorEntry;
  YIQ_: TYIQ;
  HSI_: THSI;
  CMYK_: TCMYK;
  r4: TV2Rect4;
begin
  ComputeDrawBox();

  FDrawEng.FPSFontSize := 16;

  // draw tile
  if FShowBackground then
      FDrawEng.DrawTile(FBackgroundTex, FBackgroundTex.BoundsRectV2, 1.0);

  hisRect := FDrawEng.ScreenRect;
  hisRect[0, 1] := hisRect[1, 1] - 100;

  if showPicture_ then
    for j := 0 to Count - 1 do
      begin
        sData := Items[j];
        if not sData.Raster.IsEmpty then
            FDrawEng.DrawPictureInScene(sData.Raster, sData.Raster.BoundsRectV2, sData.DrawBox, 1.0);
      end;

  if showPicture_ then
    for j := 0 to Count - 1 do
      begin
        sData := Items[j];

        if not sData.Raster.IsEmpty then
          begin
            if FShowPictureInfo then
              begin
                n := PFormat('%s' + #13#10 + 'size:%d * %d', [sData.texInfo.Text, sData.Raster.width, sData.Raster.height]);
                n := FDrawEng.RebuildTextColor(n, tsText, '', '', '', '', '|color(0.5,1.0,0.5)|', '||', '', '', '', '');
                text_siz := FDrawEng.GetTextSize(n, FShowPictureInfoFontSize);
                if text_siz[0] < RectWidth(FDrawEng.SceneToScreen(sData.DrawBox)) then
                  begin
                    FDrawEng.BeginCaptureShadow(vec2(2, 2), 1.0);
                    FDrawEng.DrawText(n, FShowPictureInfoFontSize, FDrawEng.SceneToScreen(sData.DrawBox), DEColor(1, 1, 1), False);
                    FDrawEng.EndCaptureShadow;
                  end;
              end;

            if FShowPixelInfo and Vec2InRect(FMoveScenePT, sData.DrawBox) then
              begin
                tex_pt := RectProjection(sData.DrawBox, sData.Raster.BoundsRectV2, FMoveScenePT);
                tex_color.BGRA := sData.Raster.PixelVec[tex_pt];
                n := PFormat(#13#10 + 'X:%d Y:%d color:$%.8x' + #13#10 + 'R:$%.2x G:$%.2x B:$%.2x A:$%.2x' + #13#10,
                  [Round(tex_pt[0]), Round(tex_pt[1]), tex_color.BGRA, tex_color.r, tex_color.G, tex_color.B, tex_color.A]);
                YIQ_.RGB := tex_color.BGRA;
                HSI_.RGB := tex_color.BGRA;
                CMYK_.RGB := tex_color.BGRA;
                n.Append('YIQ Y:%f I:%f Q:%f' + #13#10, [YIQ_.Y, YIQ_.i, YIQ_.Q]);
                n.Append('HSI H:%f S:%f I:%f' + #13#10, [HSI_.h, HSI_.S, HSI_.i]);
                n.Append('CMYK C:%f M:%f Y:%f K:%f' + #13#10, [CMYK_.C, CMYK_.M, CMYK_.Y, CMYK_.k]);
                n.Append('RGBA R:%f G:%f B:%f, A:%f', [tex_color.r / $FF, tex_color.G / $FF, tex_color.B / $FF, tex_color.A / $FF]);
                n := FDrawEng.RebuildTextColor(n, tsText, '', '', '', '', '|color(0.5,1.0,0.5)|', '||', '', '', '', '');
                FDrawEng.BeginCaptureShadow(vec2(2, 2), 1.0);
                r4 := FDrawEng.DrawText(n, FShowPixelInfoFontSize, DEColor(1, 1, 1, 1), Vec2Add(FMoveScreenPT, 15), 5);
                FDrawEng.EndCaptureShadow;
                FDrawEng.DrawPoint(FMoveScreenPT, DEColor(1, 1, 1), 5, 1);
                FDrawEng.DrawPoint(Vec2Add(FMoveScreenPT, 1), DEColor(0, 0, 0), 5, 1);
              end;

            if (FShowHistogramInfo) and (sData.hInfo <> nil) and ((Count = 1) or PointInRect(FMoveScenePT, sData.DrawBox)) then
                sData.hInfo.Draw(FDrawEng);
          end;
      end;

  if flush_ then
      Flush();
end;

procedure TPictureViewerInterface.Render(showPicture_: Boolean);
begin
  Render(True, True);
end;

procedure TPictureViewerInterface.Render();
begin
  Render(True);
end;

procedure TPictureViewerInterface.Flush;
begin
  FDrawEng.Flush;
end;

end.
