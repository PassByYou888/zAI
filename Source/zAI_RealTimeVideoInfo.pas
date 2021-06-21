{ ****************************************************************************** }
{ * AI RealTime Video Info struct                                              * }
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

unit zAI_RealTimeVideoInfo;

{$INCLUDE zDefine.inc}

interface

uses
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  CoreClasses, PascalStrings, UnicodeMixedLib, ListEngine, Geometry2DUnit,
  DataFrameEngine, MemoryRaster;

type
  PVideoData_MetricInfo = ^TVideoData_MetricInfo;
  PVideoData_ODBoxInfo = ^TVideoData_ODBoxInfo;
  PVideoData_ODInfo = ^TVideoData_ODInfo;
  PVideoData = ^TVideoData;

  TVideoData_MetricInfo = record
    Owner: PVideoData_ODBoxInfo;
    Token: U_String;
    K: Double;
    Done: Boolean;
  end;

  TVideoData_ODBoxInfo = record
  public
    Owner: PVideoData_ODInfo;
    Box: TRectV2;
    confidence: Double;
    Token: U_String;
    Metric: array of TVideoData_MetricInfo;
  public
    procedure Init(Owner_: PVideoData_ODInfo; MetricNum: Integer);
    procedure Free;
    function MetricIsDone: Boolean;
    procedure Encode(d: TDFE);
    procedure Decode(d: TDFE);
    function CombineMetricInfo: U_String;
  end;

  TVideoData_ODInfo = record
  public
    Owner: PVideoData;
    ODBoxInfo: array of TVideoData_ODBoxInfo;
    Done: Boolean;
  public
    procedure Init(ODNum: Integer);
    procedure Free;
    function IsDone: Boolean;
    procedure Encode(d: TDFE);
    procedure Decode(d: TDFE);
  end;

  TVideoData = record
  public
    Raster: TRaster;
    VideoInfo: U_String;
    Time_: TDateTime;
    Tick_: TTimeTick;
    ID: Int64;
    Loss: Boolean;
    CorrelationTracker: Boolean;
    DNN: Boolean;
    ODInfo: array of TVideoData_ODInfo;
  public
    procedure Init(); overload;
    procedure Init(ODNum: Integer); overload;
    procedure Free;
    function AllIsDone: Boolean;
    function ODIsDone: Boolean;
    procedure Encode(d: TDFE);
    procedure Decode(d: TDFE);
  end;

  TVideoDataList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PVideoData>;

  TVideoDataList = class(TVideoDataList_Decl)
  private
    FCritical: TCritical;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
    procedure Clean;
  end;

implementation

procedure TVideoData_ODBoxInfo.Init(Owner_: PVideoData_ODInfo; MetricNum: Integer);
var
  i: Integer;
begin
  Owner := Owner_;
  Box := NullRectV2;
  confidence := 0;
  Token := '';
  SetLength(Metric, MetricNum);
  for i := 0 to length(Metric) - 1 do
    begin
      Metric[i].Owner := @Self;
      Metric[i].Token := '';
      Metric[i].K := 0;
      Metric[i].Done := False;
    end;
end;

procedure TVideoData_ODBoxInfo.Free;
var
  i: Integer;
begin
  for i := 0 to length(Metric) - 1 do
      Metric[i].Token := '';
  SetLength(Metric, 0);
end;

function TVideoData_ODBoxInfo.MetricIsDone: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to length(Metric) - 1 do
    if not Metric[i].Done then
        exit;
  Result := True;
end;

procedure TVideoData_ODBoxInfo.Encode(d: TDFE);
var
  i: Integer;
begin
  d.WriteRectV2(Box);
  d.WriteDouble(confidence);
  d.WriteString(Token);
  d.WriteInteger(length(Metric));
  for i := 0 to length(Metric) - 1 do
    begin
      d.WriteString(Metric[i].Token);
      d.WriteDouble(Metric[i].K);
    end;
end;

procedure TVideoData_ODBoxInfo.Decode(d: TDFE);
var
  i, n: Integer;
begin
  Box := d.Reader.ReadRectV2;
  confidence := d.Reader.ReadDouble;
  Token := d.Reader.ReadString;
  n := d.Reader.ReadInteger;
  SetLength(Metric, n);
  for i := 0 to n - 1 do
    begin
      Metric[i].Owner := @Self;
      Metric[i].Token := d.Reader.ReadString;
      Metric[i].K := d.Reader.ReadDouble;
      Metric[i].Done := False;
    end;
end;

function TVideoData_ODBoxInfo.CombineMetricInfo: U_String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to length(Metric) - 1 do
    begin
      if Result.L > 0 then
          Result.Append(#13#10);
      Result.Append(Metric[i].Token + ' ' + umlFloatToStr(Metric[i].K));
    end;
end;

procedure TVideoData_ODInfo.Init(ODNum: Integer);
var
  i: Integer;
begin
  SetLength(ODBoxInfo, ODNum);
  for i := 0 to length(ODBoxInfo) - 1 do
      ODBoxInfo[i].Owner := @Self;
  Done := False;
end;

procedure TVideoData_ODInfo.Free;
var
  i, j: Integer;
begin
  for i := 0 to length(ODBoxInfo) - 1 do
    begin
      ODBoxInfo[i].Token := '';
      for j := 0 to length(ODBoxInfo[i].Metric) - 1 do
        begin
          ODBoxInfo[i].Metric[j].Token := '';
        end;
      SetLength(ODBoxInfo[i].Metric, 0);
    end;
  SetLength(ODBoxInfo, 0);
end;

function TVideoData_ODInfo.IsDone: Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Done then
      exit;
  for i := 0 to length(ODBoxInfo) - 1 do
    begin
      if not ODBoxInfo[i].MetricIsDone then
          exit;
    end;
  Result := True;
end;

procedure TVideoData_ODInfo.Encode(d: TDFE);
var
  i: Integer;
begin
  d.WriteInteger(length(ODBoxInfo));
  for i := 0 to length(ODBoxInfo) - 1 do
      ODBoxInfo[i].Encode(d);
end;

procedure TVideoData_ODInfo.Decode(d: TDFE);
var
  i, n: Integer;
begin
  n := d.Reader.ReadInteger;
  SetLength(ODBoxInfo, n);
  for i := 0 to n - 1 do
    begin
      ODBoxInfo[i].Owner := @Self;
      ODBoxInfo[i].Decode(d);
    end;
  Done := False;
end;

procedure TVideoData.Init();
var
  i: Integer;
begin
  Raster := nil;
  VideoInfo := '';
  Time_ := 0;
  Tick_ := 0;
  ID := 0;
  Loss := False;
  CorrelationTracker := False;
  DNN := False;
  SetLength(ODInfo, 0);
end;

procedure TVideoData.Init(ODNum: Integer);
var
  i: Integer;
begin
  Init();
  SetLength(ODInfo, ODNum);
  for i := 0 to length(ODInfo) - 1 do
    begin
      ODInfo[i].Owner := @Self;
      ODInfo[i].Done := False;
    end;
end;

procedure TVideoData.Free;
var
  i: Integer;
begin
  DisposeObjectAndNil(Raster);
  for i := 0 to length(ODInfo) - 1 do
      ODInfo[i].Free;
  SetLength(ODInfo, 0);
  VideoInfo := '';
end;

function TVideoData.AllIsDone: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to length(ODInfo) - 1 do
    if not ODInfo[i].IsDone then
        exit;
  Result := True;
end;

function TVideoData.ODIsDone: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to length(ODInfo) - 1 do
    if not ODInfo[i].Done then
        exit;
  Result := True;
end;

procedure TVideoData.Encode(d: TDFE);
var
  i: Integer;
begin
  d.WriteString('1.0');
  d.WriteString(VideoInfo);
  d.WriteDouble(Time_);
  d.WriteUInt64(Tick_);
  d.WriteInt64(ID);
  d.WriteBool(Loss);
  d.WriteBool(CorrelationTracker);
  d.WriteBool(DNN);
  d.WriteInteger(length(ODInfo));
  for i := 0 to length(ODInfo) - 1 do
      ODInfo[i].Encode(d);
end;

procedure TVideoData.Decode(d: TDFE);
var
  i, n: Integer;
begin
  if not(TPascalString(d.Reader.ReadString()).Same('1.0')) then
      raiseInfo('data error.');
  VideoInfo := d.Reader.ReadString;
  Time_ := d.Reader.ReadDouble;
  Tick_ := d.Reader.ReadUInt64;
  ID := d.Reader.ReadInt64;
  Loss := d.Reader.ReadBool;
  CorrelationTracker := d.Reader.ReadBool;
  DNN := d.Reader.ReadBool;
  n := d.Reader.ReadInteger;
  SetLength(ODInfo, n);
  for i := 0 to n - 1 do
    begin
      ODInfo[i].Owner := @Self;
      ODInfo[i].Decode(d);
    end;
end;

constructor TVideoDataList.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
end;

destructor TVideoDataList.Destroy;
begin
  DisposeObject(FCritical);
  inherited Destroy;
end;

procedure TVideoDataList.Lock;
begin
  FCritical.Lock;
end;

procedure TVideoDataList.UnLock;
begin
  FCritical.UnLock;
end;

procedure TVideoDataList.Clean;
var
  i: Integer;
begin
  Lock;
  for i := 0 to count - 1 do
    begin
      items[i]^.Free;
      dispose(items[i]);
    end;
  inherited Clear;
  UnLock;
end;

end.
