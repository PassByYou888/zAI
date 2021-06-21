{ ****************************************************************************** }
{ * AI Rasterization Recognition ON Video                                      * }
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
unit zAI_VideoRasterDNNThreadQueue;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, UnicodeMixedLib, DoStatusIO, MemoryStream64, ListEngine, LinearAction,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  MemoryRaster, Geometry2DUnit,
  H264, FFMPEG, FFMPEG_Reader, FFMPEG_Writer,
  zAI, zAI_Common, zAI_FFMPEG, Learn, LearnTypes, KDTree;

type
  TRasterInputQueue = class;

  TRasterRecognitionData = class
  protected
    FOwner: TRasterInputQueue;
    FID: SystemString;
    FRaster: TRaster;
    FIDLE: Boolean;
    FNullRec: Boolean;
    FDNNThread: TAI_DNN_Thread;
    FInputTime, FDoneTime: TTimeTick;
    FState: SystemString;
    FNextLevel: TRasterInputQueue;
    FUserData: Pointer;
    function DoGetState(): SystemString; virtual;
    function DoGetShortName(): SystemString; virtual;
  public
    constructor Create(Owner_: TRasterInputQueue); virtual;
    destructor Destroy; override;
    property Owner: TRasterInputQueue read FOwner;
    procedure SetDone;
    function Busy(): Boolean; overload;
    function Busy(checkNextLevel_: Boolean): Boolean; overload;
    function UsageTime: TTimeTick;
    property Raster: TRaster read FRaster;
    property ID: SystemString read FID;
    property NullRec: Boolean read FNullRec;
    function GetStateInfo: SystemString;
    property StateInfo: SystemString read GetStateInfo;
    function GetNextLevel: TRasterInputQueue;
    property NextLevel: TRasterInputQueue read GetNextLevel;
    property DirectNextLevel: TRasterInputQueue read FNextLevel;
    property UserData: Pointer read FUserData write FUserData;
    property ShortName: SystemString read DoGetShortName;
  end;

  TRasterRecognitionDataClass = class of TRasterRecognitionData;

  TRasterRecognitionData_Passed = class(TRasterRecognitionData)
  public
    constructor Create(Owner_: TRasterInputQueue); override;
    destructor Destroy; override;
  end;

  TRasterRecognitionData_SP = class(TRasterRecognitionData)
  protected
    function DoGetState(): SystemString; override;
    function DoGetShortName(): SystemString; override;
  public
    Parallel_: TAI_Parallel;
    SP_Desc: TMatrixVec2;
    MMOD_Desc: TMMOD_Desc;
    FaceRasterList: TMemoryRasterList;
    constructor Create(Owner_: TRasterInputQueue); override;
    destructor Destroy; override;
  end;

  TRasterRecognitionData_Metric = class(TRasterRecognitionData)
  protected
    function DoGetState(): SystemString; override;
    function DoGetShortName(): SystemString; override;
  public
    Output: TLVec;
    L: TLearn;
    constructor Create(Owner_: TRasterInputQueue); override;
    destructor Destroy; override;
  end;

  TRasterRecognitionData_LMetric = class(TRasterRecognitionData)
  protected
    function DoGetState(): SystemString; override;
    function DoGetShortName(): SystemString; override;
  public
    Output: TLVec;
    L: TLearn;
    constructor Create(Owner_: TRasterInputQueue); override;
    destructor Destroy; override;
  end;

  TRasterRecognitionData_MMOD6L = class(TRasterRecognitionData)
  protected
    function DoGetState(): SystemString; override;
    function DoGetShortName(): SystemString; override;
  public
    Output: TMMOD_Desc;
    constructor Create(Owner_: TRasterInputQueue); override;
    destructor Destroy; override;
  end;

  TRasterRecognitionData_MMOD3L = class(TRasterRecognitionData)
  protected
    function DoGetState(): SystemString; override;
    function DoGetShortName(): SystemString; override;
  public
    Output: TMMOD_Desc;
    constructor Create(Owner_: TRasterInputQueue); override;
    destructor Destroy; override;
  end;

  TRasterRecognitionData_RNIC = class(TRasterRecognitionData)
  protected
    function DoGetState(): SystemString; override;
    function DoGetShortName(): SystemString; override;
  public
    Output: TLVec;
    ClassifierIndex: TPascalStringList;
    constructor Create(Owner_: TRasterInputQueue); override;
    destructor Destroy; override;
  end;

  TRasterRecognitionData_LRNIC = class(TRasterRecognitionData)
  protected
    function DoGetState(): SystemString; override;
    function DoGetShortName(): SystemString; override;
  public
    Output: TLVec;
    ClassifierIndex: TPascalStringList;
    constructor Create(Owner_: TRasterInputQueue); override;
    destructor Destroy; override;
  end;

  TRasterRecognitionData_GDCNIC = class(TRasterRecognitionData)
  protected
    function DoGetState(): SystemString; override;
    function DoGetShortName(): SystemString; override;
  public
    Output: TLVec;
    ClassifierIndex: TPascalStringList;
    constructor Create(Owner_: TRasterInputQueue); override;
    destructor Destroy; override;
  end;

  TRasterRecognitionData_GNIC = class(TRasterRecognitionData)
  protected
    function DoGetState(): SystemString; override;
    function DoGetShortName(): SystemString; override;
  public
    Output: TLVec;
    ClassifierIndex: TPascalStringList;
    constructor Create(Owner_: TRasterInputQueue); override;
    destructor Destroy; override;
  end;

  TRasterRecognitionData_SS = class(TRasterRecognitionData)
  protected
    function DoGetState(): SystemString; override;
    function DoGetShortName(): SystemString; override;
  public
    Output: TMemoryRaster;
    SSTokenOutput: TPascalStringList;
    ColorPool: TSegmentationColorTable;
    constructor Create(Owner_: TRasterInputQueue); override;
    destructor Destroy; override;
  end;

  TRasterRecognitionData_ZMetric = class(TRasterRecognitionData)
  protected
    function DoGetState(): SystemString; override;
    function DoGetShortName(): SystemString; override;
  public
    Output: TLVec;
    L: TLearn;
    SS_Width, SS_Height: Integer;
    constructor Create(Owner_: TRasterInputQueue); override;
    destructor Destroy; override;
  end;

  TRasterRecognitionDataList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TRasterRecognitionData>;

  TOnInput = procedure(Sender: TRasterInputQueue; Raster: TRaster) of object;
  TOnRecognitionDone = procedure(Sender: TRasterInputQueue; RD: TRasterRecognitionData) of object;
  TOnQueueDone = procedure(Sender: TRasterInputQueue) of object;
  TOnCutNullRec = procedure(Sender: TRasterInputQueue; bIndex, eIndex: Integer) of object;
  TOnCutMaxLimit = procedure(Sender: TRasterInputQueue; bIndex, eIndex: Integer) of object;

  TRasterInputQueue = class
  private
    FRunning: Integer;
    FOwner: TRasterRecognitionData;
    FQueue: TRasterRecognitionDataList;
    FCritical: TCritical;
    FCutNullQueue: Boolean;
    FMaxQueue: Integer;
    FSyncEvent: Boolean;
    FUserData: Pointer;
    FOnInput: TOnInput;
    FOnRecognitionDone: TOnRecognitionDone;
    FOnQueueDone: TOnQueueDone;
    FOnCutNullRec: TOnCutNullRec;
    FOnCutMaxLimit: TOnCutMaxLimit;

    procedure DoDelayCheckBusyAndFree;
    function BeforeInput(ID_: SystemString; UserData_: Pointer; Raster: TRaster; instance_: Boolean; dataClass: TRasterRecognitionDataClass): TRasterRecognitionData;
    procedure Sync_DoFinish(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure DoCheckDone(RD: TRasterRecognitionData);
    procedure DoFinish(RD: TRasterRecognitionData; RecSuccessed: Boolean);
    procedure DoRunSP(thSender: TCompute);
    procedure Do_Input_Metric_Result(thSender: TAI_DNN_Thread_Metric; UserData: Pointer; Input: TMemoryRaster; Output: TLVec); virtual;
    procedure Do_Input_LMetric_Result(thSender: TAI_DNN_Thread_LMetric; UserData: Pointer; Input: TMemoryRaster; Output: TLVec); virtual;
    procedure Do_Input_MMOD3L_Result(thSender: TAI_DNN_Thread_MMOD3L; UserData: Pointer; Input: TMemoryRaster; Output: TMMOD_Desc); virtual;
    procedure Do_Input_MMOD6L_Result(thSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Input: TMemoryRaster; Output: TMMOD_Desc); virtual;
    procedure Do_Input_RNIC_Result(thSender: TAI_DNN_Thread_RNIC; UserData: Pointer; Input: TMemoryRaster; Output: TLVec); virtual;
    procedure Do_Input_LRNIC_Result(thSender: TAI_DNN_Thread_LRNIC; UserData: Pointer; Input: TMemoryRaster; Output: TLVec); virtual;
    procedure Do_Input_GDCNIC_Result(thSender: TAI_DNN_Thread_GDCNIC; UserData: Pointer; Input: TMemoryRaster; Output: TLVec); virtual;
    procedure Do_Input_GNIC_Result(thSender: TAI_DNN_Thread_GNIC; UserData: Pointer; Input: TMemoryRaster; Output: TLVec); virtual;
    procedure Do_Input_SS_Result(thSender: TAI_DNN_Thread_SS; UserData: Pointer; Input: TMemoryRaster; SSTokenOutput: TPascalStringList; Output: TMemoryRaster); virtual;
    procedure Do_Input_ZMetric_Result(thSender: TAI_DNN_Thread_ZMetric; UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; Output: TLVec); virtual;
  public
    constructor Create(Owner_: TRasterRecognitionData);
    destructor Destroy; override;

    property Owner: TRasterRecognitionData read FOwner;

    function Input_Passed(ID_: SystemString; UserData_: Pointer; Raster: TRaster; instance_: Boolean): TRasterRecognitionData_Passed;
    function Input_SP(ID_: SystemString; UserData_: Pointer; Raster: TRaster; instance_: Boolean; MMOD_Desc: TMMOD_Desc; Parallel_: TAI_Parallel): TRasterRecognitionData_SP;
    function Input_Metric(ID_: SystemString; UserData_: Pointer; Raster: TRaster; L: TLearn; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_Metric): TRasterRecognitionData_Metric;
    function Input_LMetric(ID_: SystemString; UserData_: Pointer; Raster: TRaster; L: TLearn; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_LMetric): TRasterRecognitionData_LMetric;
    function Input_MMOD3L(ID_: SystemString; UserData_: Pointer; Raster: TRaster; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_MMOD3L): TRasterRecognitionData_MMOD3L;
    function Input_MMOD6L(ID_: SystemString; UserData_: Pointer; Raster: TRaster; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_MMOD6L): TRasterRecognitionData_MMOD6L;
    function Input_RNIC(ID_: SystemString; UserData_: Pointer; Raster: TRaster; ClassifierIndex: TPascalStringList; num_crops: Integer; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_RNIC): TRasterRecognitionData_RNIC;
    function Input_LRNIC(ID_: SystemString; UserData_: Pointer; Raster: TRaster; ClassifierIndex: TPascalStringList; num_crops: Integer; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_LRNIC): TRasterRecognitionData_LRNIC;
    function Input_GDCNIC(ID_: SystemString; UserData_: Pointer; Raster: TRaster; ClassifierIndex: TPascalStringList; SS_Width, SS_Height: Integer; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_GDCNIC): TRasterRecognitionData_GDCNIC;
    function Input_GNIC(ID_: SystemString; UserData_: Pointer; Raster: TRaster; ClassifierIndex: TPascalStringList; SS_Width, SS_Height: Integer; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_GNIC): TRasterRecognitionData_GNIC;
    function Input_SS(ID_: SystemString; UserData_: Pointer; Raster: TRaster; ColorPool: TSegmentationColorTable; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_SS): TRasterRecognitionData_SS;
    function Input_ZMetric(ID_: SystemString; UserData_: Pointer; Raster: TRaster; L: TLearn; SS_Width, SS_Height: Integer; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_ZMetric): TRasterRecognitionData_ZMetric;

    function FindDNNThread(DNNThread: TAI_DNN_Thread): Integer;
    function BusyNum: Integer;
    function Busy: Boolean; overload;
    function Busy(bIndex, eIndex: Integer): Boolean; overload;
    function Delete(bIndex, eIndex: Integer): Boolean;
    procedure RemoveNullOutput;
    procedure GetQueueState();
    function Count: Integer;

    function LockQueue: TRasterRecognitionDataList;
    procedure UnLockQueue;
    procedure Clean;
    procedure DelayCheckBusyAndFree;

    property CutNullQueue: Boolean read FCutNullQueue write FCutNullQueue;
    property MaxQueue: Integer read FMaxQueue write FMaxQueue;
    property SyncEvent: Boolean read FSyncEvent write FSyncEvent;
    property UserData: Pointer read FUserData write FUserData;

    property OnInput: TOnInput read FOnInput write FOnInput;
    property OnRecognitionDone: TOnRecognitionDone read FOnRecognitionDone write FOnRecognitionDone;
    property OnQueueDone: TOnQueueDone read FOnQueueDone write FOnQueueDone;
    property OnCutNullRec: TOnCutNullRec read FOnCutNullRec write FOnCutNullRec;
    property OnCutMaxLimit: TOnCutMaxLimit read FOnCutMaxLimit write FOnCutMaxLimit;
  end;

implementation

type
  TRasterRecognitionData_ = record
    Data: TRasterRecognitionData;
  end;

  TRasterRecognitionData_Ptr = ^TRasterRecognitionData_;

function TRasterRecognitionData.DoGetState: SystemString;
begin
  Result := PFormat('Done %s.', [ClassName]);
end;

function TRasterRecognitionData.DoGetShortName: SystemString;
var
  n: U_String;
begin
  n := ClassName;
  if n.Exists('_') then
      Result := umlGetLastStr(n, '_')
  else
      Result := 'Base';
end;

constructor TRasterRecognitionData.Create(Owner_: TRasterInputQueue);
begin
  inherited Create;
  FOwner := Owner_;
  FID := '';
  FRaster := nil;
  FIDLE := False;
  FNullRec := True;
  FDNNThread := nil;
  FInputTime := 0;
  FDoneTime := 0;
  FState := '';
  FNextLevel := nil;
  FUserData := nil;
end;

destructor TRasterRecognitionData.Destroy;
begin
  if FNextLevel <> nil then
      FNextLevel.DelayCheckBusyAndFree;
  DisposeObjectAndNil(FRaster);
  inherited Destroy;
end;

procedure TRasterRecognitionData.SetDone;
begin
  FIDLE := True;
  FDoneTime := GetTimeTick();
end;

function TRasterRecognitionData.Busy(): Boolean;
begin
  Result := Busy(True);
end;

function TRasterRecognitionData.Busy(checkNextLevel_: Boolean): Boolean;
begin
  Result := True;
  if not FIDLE then
      exit;
  Result := (checkNextLevel_) and (FNextLevel <> nil) and (FNextLevel.Busy());
end;

function TRasterRecognitionData.UsageTime: TTimeTick;
begin
  if Busy then
      Result := 0
  else
      Result := FDoneTime - FInputTime;
end;

function TRasterRecognitionData.GetStateInfo: SystemString;
begin
  if FIDLE then
    begin
      if FState = '' then
          FState := DoGetState;
      Result := FState;
    end
  else
      Result := 'BUSY.'
end;

function TRasterRecognitionData.GetNextLevel: TRasterInputQueue;
begin
  if FNextLevel = nil then
      FNextLevel := TRasterInputQueue.Create(Self);
  Result := FNextLevel;
end;

constructor TRasterRecognitionData_Passed.Create(Owner_: TRasterInputQueue);
begin
  inherited Create(Owner_);
end;

destructor TRasterRecognitionData_Passed.Destroy;
begin
  inherited Destroy;
end;

function TRasterRecognitionData_SP.DoGetState: SystemString;
var
  i: Integer;
begin
  Result := 'Done SP: ';
  for i := 0 to Length(SP_Desc) - 1 do
      Result := Result + PFormat('%s%d', [if_(i > 0, ',', ''), Length(SP_Desc[i])]);
end;

function TRasterRecognitionData_SP.DoGetShortName: SystemString;
begin
  Result := 'SP';
end;

constructor TRasterRecognitionData_SP.Create(Owner_: TRasterInputQueue);
begin
  inherited Create(Owner_);
  Parallel_ := nil;
  SetLength(SP_Desc, 0, 0);
  SetLength(MMOD_Desc, 0);
  FaceRasterList := TMemoryRasterList.Create;
  FaceRasterList.AutoFreeRaster := True;
end;

destructor TRasterRecognitionData_SP.Destroy;
begin
  SetLength(SP_Desc, 0, 0);
  SetLength(MMOD_Desc, 0);
  DisposeObject(FaceRasterList);
  inherited Destroy;
end;

function TRasterRecognitionData_Metric.DoGetState: SystemString;
var
  i: Integer;
  k: TLFloat;
begin
  Result := inherited DoGetState;
  if L = nil then
      exit;
  i := L.ProcessMaxIndex(Output);
  k := LDistance(Output, L[i]^.m_in);
  Result := PFormat('Done Metric %s:%f', [L[i]^.token.Text, k]);
end;

function TRasterRecognitionData_Metric.DoGetShortName: SystemString;
begin
  Result := 'Metric';
end;

constructor TRasterRecognitionData_Metric.Create(Owner_: TRasterInputQueue);
begin
  inherited Create(Owner_);
  SetLength(Output, 0);
  L := nil;
end;

destructor TRasterRecognitionData_Metric.Destroy;
begin
  SetLength(Output, 0);
  inherited Destroy;
end;

function TRasterRecognitionData_LMetric.DoGetState: SystemString;
var
  i: Integer;
  k: TLFloat;
begin
  Result := inherited DoGetState;
  if L = nil then
      exit;
  i := L.ProcessMaxIndex(Output);
  k := LDistance(Output, L[i]^.m_in);
  Result := PFormat('Done LMetric %s:%f', [L[i]^.token.Text, k]);
end;

function TRasterRecognitionData_LMetric.DoGetShortName: SystemString;
begin
  Result := 'LMetric';
end;

constructor TRasterRecognitionData_LMetric.Create(Owner_: TRasterInputQueue);
begin
  inherited Create(Owner_);
  SetLength(Output, 0);
  L := nil;
end;

destructor TRasterRecognitionData_LMetric.Destroy;
begin
  SetLength(Output, 0);
  inherited Destroy;
end;

function TRasterRecognitionData_MMOD6L.DoGetState: SystemString;
begin
  Result := PFormat('Done MMOD6L: %d', [Length(Output)]);
end;

function TRasterRecognitionData_MMOD6L.DoGetShortName: SystemString;
begin
  Result := 'MMOD6L';
end;

constructor TRasterRecognitionData_MMOD6L.Create(Owner_: TRasterInputQueue);
begin
  inherited Create(Owner_);
  SetLength(Output, 0);
end;

destructor TRasterRecognitionData_MMOD6L.Destroy;
begin
  SetLength(Output, 0);
  inherited Destroy;
end;

function TRasterRecognitionData_MMOD3L.DoGetState: SystemString;
begin
  Result := PFormat('Done MMOD3L: %d', [Length(Output)]);
end;

function TRasterRecognitionData_MMOD3L.DoGetShortName: SystemString;
begin
  Result := 'MMOD3L';
end;

constructor TRasterRecognitionData_MMOD3L.Create(Owner_: TRasterInputQueue);
begin
  inherited Create(Owner_);
  SetLength(Output, 0);
end;

destructor TRasterRecognitionData_MMOD3L.Destroy;
begin
  SetLength(Output, 0);
  inherited Destroy;
end;

function TRasterRecognitionData_RNIC.DoGetState: SystemString;
var
  i, j: Integer;
  v: TLVec;
begin
  Result := 'Done RNIC.';
  v := LVecCopy(Output);
  for i := 0 to umlMin(ClassifierIndex.Count - 1, 4) do
    begin
      j := LMaxVecIndex(v);
      if j < ClassifierIndex.Count then
          Result := Result + #13#10 + PFormat('%s=%f', [ClassifierIndex[j].Text, v[j]]);
      v[j] := 0;
    end;
end;

function TRasterRecognitionData_RNIC.DoGetShortName: SystemString;
begin
  Result := 'RNIC';
end;

constructor TRasterRecognitionData_RNIC.Create(Owner_: TRasterInputQueue);
begin
  inherited Create(Owner_);
  SetLength(Output, 0);
  ClassifierIndex := nil;
end;

destructor TRasterRecognitionData_RNIC.Destroy;
begin
  SetLength(Output, 0);
  inherited Destroy;
end;

function TRasterRecognitionData_LRNIC.DoGetState: SystemString;
var
  i, j: Integer;
  v: TLVec;
begin
  Result := 'Done LRNIC.';
  v := LVecCopy(Output);
  for i := 0 to umlMin(ClassifierIndex.Count - 1, 4) do
    begin
      j := LMaxVecIndex(v);
      if j < ClassifierIndex.Count then
          Result := Result + #13#10 + PFormat('%s=%f', [ClassifierIndex[j].Text, v[j]]);
      v[j] := 0;
    end;
end;

function TRasterRecognitionData_LRNIC.DoGetShortName: SystemString;
begin
  Result := 'LRNIC';
end;

constructor TRasterRecognitionData_LRNIC.Create(Owner_: TRasterInputQueue);
begin
  inherited Create(Owner_);
  SetLength(Output, 0);
  ClassifierIndex := nil;
end;

destructor TRasterRecognitionData_LRNIC.Destroy;
begin
  SetLength(Output, 0);
  inherited Destroy;
end;

function TRasterRecognitionData_GDCNIC.DoGetState: SystemString;
var
  i, j: Integer;
  v: TLVec;
begin
  Result := 'Done GDCNIC.';
  v := LVecCopy(Output);
  for i := 0 to umlMin(ClassifierIndex.Count - 1, 4) do
    begin
      j := LMaxVecIndex(v);
      if j < ClassifierIndex.Count then
          Result := Result + #13#10 + PFormat('%s=%f', [ClassifierIndex[j].Text, v[j]]);
      v[j] := 0;
    end;
end;

function TRasterRecognitionData_GDCNIC.DoGetShortName: SystemString;
begin
  Result := 'GDCNIC';
end;

constructor TRasterRecognitionData_GDCNIC.Create(Owner_: TRasterInputQueue);
begin
  inherited Create(Owner_);
  SetLength(Output, 0);
  ClassifierIndex := nil;
end;

destructor TRasterRecognitionData_GDCNIC.Destroy;
begin
  SetLength(Output, 0);
  inherited Destroy;
end;

function TRasterRecognitionData_GNIC.DoGetState: SystemString;
var
  i, j: Integer;
  v: TLVec;
begin
  Result := 'Done GNIC.';
  v := LVecCopy(Output);
  for i := 0 to umlMin(ClassifierIndex.Count - 1, 4) do
    begin
      j := LMaxVecIndex(v);
      if j < ClassifierIndex.Count then
          Result := Result + #13#10 + PFormat('%s=%f', [ClassifierIndex[j].Text, v[j]]);
      v[j] := 0;
    end;
end;

function TRasterRecognitionData_GNIC.DoGetShortName: SystemString;
begin
  Result := 'GNIC';
end;

constructor TRasterRecognitionData_GNIC.Create(Owner_: TRasterInputQueue);
begin
  inherited Create(Owner_);
  SetLength(Output, 0);
  ClassifierIndex := nil;
end;

destructor TRasterRecognitionData_GNIC.Destroy;
begin
  SetLength(Output, 0);
  inherited Destroy;
end;

function TRasterRecognitionData_SS.DoGetState: SystemString;
var
  i: Integer;
begin
  Result := 'Done SS.';
  for i := 0 to umlMin(SSTokenOutput.Count - 1, 4) do
      Result := Result + #13#10 + PFormat('found: %s', [SSTokenOutput[i].Text]);
end;

function TRasterRecognitionData_SS.DoGetShortName: SystemString;
begin
  Result := 'SS';
end;

constructor TRasterRecognitionData_SS.Create(Owner_: TRasterInputQueue);
begin
  inherited Create(Owner_);
  Output := NewRaster();
  SSTokenOutput := TPascalStringList.Create;
  ColorPool := nil;
end;

destructor TRasterRecognitionData_SS.Destroy;
begin
  DisposeObject(Output);
  DisposeObject(SSTokenOutput);
  inherited Destroy;
end;

function TRasterRecognitionData_ZMetric.DoGetState: SystemString;
var
  i: Integer;
  k: TLFloat;
begin
  Result := inherited DoGetState;
  if L = nil then
      exit;
  i := L.ProcessMaxIndex(Output);
  k := LDistance(Output, L[i]^.m_in);
  Result := PFormat('Done Z-Metric %s:%f', [L[i]^.token.Text, k]);
end;

function TRasterRecognitionData_ZMetric.DoGetShortName: SystemString;
begin
  Result := 'ZMetric';
end;

constructor TRasterRecognitionData_ZMetric.Create(Owner_: TRasterInputQueue);
begin
  inherited Create(Owner_);
  SetLength(Output, 0);
  L := nil;
  SS_Width := 0;
  SS_Height := 0;
end;

destructor TRasterRecognitionData_ZMetric.Destroy;
begin
  SetLength(Output, 0);
  inherited Destroy;
end;

procedure TRasterInputQueue.DoDelayCheckBusyAndFree;
begin
  while (FRunning > 0) or (Busy) do
      TCompute.Sleep(10);
  DisposeObject(Self);
end;

function TRasterInputQueue.BeforeInput(ID_: SystemString; UserData_: Pointer; Raster: TRaster; instance_: Boolean; dataClass: TRasterRecognitionDataClass): TRasterRecognitionData;
var
  RD: TRasterRecognitionData;
begin
  RD := dataClass.Create(Self);
  if instance_ then
      RD.FRaster := Raster
  else
      RD.FRaster := Raster.Clone;

  RD.FID := ID_;
  RD.FInputTime := GetTimeTick();
  RD.FDoneTime := RD.FInputTime;
  RD.UserData := UserData_;

  FCritical.Lock;
  FQueue.Add(RD);
  FCritical.UnLock;

  try
    if Assigned(FOnInput) then
        FOnInput(Self, RD.FRaster);
  except
  end;

  Result := RD;
end;

procedure TRasterInputQueue.Sync_DoFinish(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
var
  RD: TRasterRecognitionData;
  RecSuccessed: Boolean;
begin
  RD := TRasterRecognitionData(Data2);
  RecSuccessed := Data3;

  RD.FNullRec := not RecSuccessed;

  try
    if Assigned(FOnRecognitionDone) then
        FOnRecognitionDone(Self, RD);
  except
  end;

  RD.SetDone;

  DoCheckDone(RD);
  AtomDec(FRunning);
end;

procedure TRasterInputQueue.DoCheckDone(RD: TRasterRecognitionData);
var
  i: Integer;
begin
  if RD.Busy then
      exit;

  if (not Busy) then
    begin
      if FOwner <> nil then
        if FOwner.FOwner <> nil then
            FOwner.FOwner.DoCheckDone(FOwner);
      try
        if Assigned(FOnQueueDone) then
            FOnQueueDone(Self);
      except
      end;
    end;

  if (FMaxQueue > 0) and (Count > FMaxQueue) then
    begin
      if not Busy(0, Count - FMaxQueue) then
        begin
          try
            if Assigned(FOnCutMaxLimit) then
                FOnCutMaxLimit(Self, 0, Count - FMaxQueue);
          except
          end;
          Delete(0, Count - FMaxQueue);
        end;
    end;

  if (RD.FNullRec) and (FCutNullQueue) then
    begin
      FCritical.Lock;
      i := FQueue.IndexOf(RD);
      FCritical.UnLock;
      if i >= 0 then
        if not Busy(0, i) then
          begin
            try
              if Assigned(FOnCutNullRec) then
                  FOnCutNullRec(Self, 0, i);
            except
            end;
            Delete(0, i);
          end;
    end;
end;

procedure TRasterInputQueue.DoFinish(RD: TRasterRecognitionData; RecSuccessed: Boolean);
begin
  if RD.FOwner <> Self then
      exit;

  AtomInc(FRunning);

  if FSyncEvent then
      TCompute.PostM3(nil, RD, RecSuccessed, {$IFDEF FPC}@{$ENDIF FPC}Sync_DoFinish)
  else
      Sync_DoFinish(nil, RD, RecSuccessed);
end;

procedure TRasterInputQueue.DoRunSP(thSender: TCompute);
var
  p: TRasterRecognitionData_Ptr;
  spData: TRasterRecognitionData_SP;
  AI: TAI;
  rgbHnd: TRGB_Image_Handle;
  i: Integer;
  faceHnd: TFACE_Handle;
  Successed_: Boolean;
begin
  p := thSender.UserData;
  spData := TRasterRecognitionData_SP(p^.Data);

  Successed_ := False;

  AI := spData.Parallel_.GetAndLockAI();

  if spData.Parallel_.InternalFaceSP then
    begin
      faceHnd := AI.Face_Detector(p^.Data.Raster, spData.MMOD_Desc, C_Metric_Input_Size);
      Successed_ := faceHnd <> nil;
      if Successed_ then
        for i := 0 to AI.Face_chips_num(faceHnd) - 1 do
          begin
            spData.SP_Desc[i] := AI.Face_ShapeV2(faceHnd, i);
            spData.FaceRasterList.Add(AI.Face_chips(faceHnd, i));
          end;
    end
  else
    begin
      rgbHnd := AI.Prepare_RGB_Image(p^.Data.Raster);
      for i := 0 to Length(spData.MMOD_Desc) - 1 do
          spData.SP_Desc[i] := AI.SP_ProcessRGB_Vec2(AI.Parallel_SP_Hnd, rgbHnd, spData.MMOD_Desc[i].R);
      AI.Close_RGB_Image(rgbHnd);
      Successed_ := True;
    end;

  spData.Parallel_.UnLockAI(AI);

  DoFinish(p^.Data, Successed_);
  Dispose(p);
end;

procedure TRasterInputQueue.Do_Input_Metric_Result(thSender: TAI_DNN_Thread_Metric; UserData: Pointer; Input: TMemoryRaster; Output: TLVec);
var
  p: TRasterRecognitionData_Ptr;
begin
  p := UserData;
  TRasterRecognitionData_Metric(p^.Data).Output := LVecCopy(Output);
  DoFinish(p^.Data, True);
  Dispose(p);
end;

procedure TRasterInputQueue.Do_Input_LMetric_Result(thSender: TAI_DNN_Thread_LMetric; UserData: Pointer; Input: TMemoryRaster; Output: TLVec);
var
  p: TRasterRecognitionData_Ptr;
begin
  p := UserData;
  TRasterRecognitionData_LMetric(p^.Data).Output := LVecCopy(Output);
  DoFinish(p^.Data, True);
  Dispose(p);
end;

procedure TRasterInputQueue.Do_Input_MMOD3L_Result(thSender: TAI_DNN_Thread_MMOD3L; UserData: Pointer; Input: TMemoryRaster; Output: TMMOD_Desc);
var
  p: TRasterRecognitionData_Ptr;
  i: Integer;
begin
  p := UserData;
  SetLength(TRasterRecognitionData_MMOD3L(p^.Data).Output, Length(Output));
  for i := 0 to Length(Output) - 1 do
      TRasterRecognitionData_MMOD3L(p^.Data).Output[i] := Output[i];
  DoFinish(p^.Data, Length(Output) > 0);
  Dispose(p);
end;

procedure TRasterInputQueue.Do_Input_MMOD6L_Result(thSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Input: TMemoryRaster; Output: TMMOD_Desc);
var
  p: TRasterRecognitionData_Ptr;
  i: Integer;
begin
  p := UserData;
  SetLength(TRasterRecognitionData_MMOD6L(p^.Data).Output, Length(Output));
  for i := 0 to Length(Output) - 1 do
      TRasterRecognitionData_MMOD6L(p^.Data).Output[i] := Output[i];
  DoFinish(p^.Data, Length(Output) > 0);
  Dispose(p);
end;

procedure TRasterInputQueue.Do_Input_RNIC_Result(thSender: TAI_DNN_Thread_RNIC; UserData: Pointer; Input: TMemoryRaster; Output: TLVec);
var
  p: TRasterRecognitionData_Ptr;
begin
  p := UserData;
  TRasterRecognitionData_RNIC(p^.Data).Output := LVecCopy(Output);
  DoFinish(p^.Data, True);
  Dispose(p);
end;

procedure TRasterInputQueue.Do_Input_LRNIC_Result(thSender: TAI_DNN_Thread_LRNIC; UserData: Pointer; Input: TMemoryRaster; Output: TLVec);
var
  p: TRasterRecognitionData_Ptr;
begin
  p := UserData;
  TRasterRecognitionData_LRNIC(p^.Data).Output := LVecCopy(Output);
  DoFinish(p^.Data, True);
  Dispose(p);
end;

procedure TRasterInputQueue.Do_Input_GDCNIC_Result(thSender: TAI_DNN_Thread_GDCNIC; UserData: Pointer; Input: TMemoryRaster; Output: TLVec);
var
  p: TRasterRecognitionData_Ptr;
begin
  p := UserData;
  TRasterRecognitionData_GDCNIC(p^.Data).Output := LVecCopy(Output);
  DoFinish(p^.Data, True);
  Dispose(p);
end;

procedure TRasterInputQueue.Do_Input_GNIC_Result(thSender: TAI_DNN_Thread_GNIC; UserData: Pointer; Input: TMemoryRaster; Output: TLVec);
var
  p: TRasterRecognitionData_Ptr;
begin
  p := UserData;
  TRasterRecognitionData_GNIC(p^.Data).Output := LVecCopy(Output);
  DoFinish(p^.Data, True);
  Dispose(p);
end;

procedure TRasterInputQueue.Do_Input_SS_Result(thSender: TAI_DNN_Thread_SS; UserData: Pointer; Input: TMemoryRaster; SSTokenOutput: TPascalStringList; Output: TMemoryRaster);
var
  p: TRasterRecognitionData_Ptr;
begin
  p := UserData;
  TRasterRecognitionData_SS(p^.Data).Output.SwapInstance(Output);
  TRasterRecognitionData_SS(p^.Data).SSTokenOutput.Assign(SSTokenOutput);
  DoFinish(p^.Data, SSTokenOutput.Count > 0);
  Dispose(p);
end;

procedure TRasterInputQueue.Do_Input_ZMetric_Result(thSender: TAI_DNN_Thread_ZMetric; UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; Output: TLVec);
var
  p: TRasterRecognitionData_Ptr;
begin
  p := UserData;
  TRasterRecognitionData_ZMetric(p^.Data).Output := LVecCopy(Output);
  TRasterRecognitionData_ZMetric(p^.Data).SS_Width := SS_Width;
  TRasterRecognitionData_ZMetric(p^.Data).SS_Height := SS_Height;
  DoFinish(p^.Data, True);
  Dispose(p);
end;

constructor TRasterInputQueue.Create(Owner_: TRasterRecognitionData);
begin
  inherited Create;
  FRunning := 0;
  FOwner := Owner_;
  FQueue := TRasterRecognitionDataList.Create;
  FCritical := TCritical.Create;
  FCutNullQueue := False;
  FMaxQueue := 0;
  FSyncEvent := False;
  FUserData := nil;

  FOnInput := nil;
  FOnRecognitionDone := nil;
  FOnQueueDone := nil;
  FOnCutNullRec := nil;
  FOnCutMaxLimit := nil;
end;

destructor TRasterInputQueue.Destroy;
begin
  Clean;
  DisposeObject(FQueue);
  FCritical.Free;
  inherited Destroy;
end;

function TRasterInputQueue.Input_Passed(ID_: SystemString; UserData_: Pointer; Raster: TRaster; instance_: Boolean): TRasterRecognitionData_Passed;
begin
  Result := BeforeInput(ID_, UserData_, Raster, instance_, TRasterRecognitionData_Passed) as TRasterRecognitionData_Passed;
  DoFinish(Result, True);
end;

function TRasterInputQueue.Input_SP(ID_: SystemString; UserData_: Pointer; Raster: TRaster; instance_: Boolean; MMOD_Desc: TMMOD_Desc; Parallel_: TAI_Parallel): TRasterRecognitionData_SP;
var
  p: TRasterRecognitionData_Ptr;
  i: Integer;
begin
  if Parallel_ = nil then
    begin
      Result := nil;
      exit;
    end;

  Result := BeforeInput(ID_, UserData_, Raster, instance_, TRasterRecognitionData_SP) as TRasterRecognitionData_SP;
  SetLength(Result.SP_Desc, Length(MMOD_Desc));
  SetLength(Result.MMOD_Desc, Length(MMOD_Desc));
  for i := 0 to Length(MMOD_Desc) - 1 do
      Result.MMOD_Desc[i] := MMOD_Desc[i];
  Result.Parallel_ := Parallel_;

  New(p);
  p^.Data := Result;
  TCompute.RunM(p, Result, {$IFDEF FPC}@{$ENDIF FPC}DoRunSP);
end;

function TRasterInputQueue.Input_Metric(ID_: SystemString; UserData_: Pointer; Raster: TRaster; L: TLearn; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_Metric): TRasterRecognitionData_Metric;
var
  p: TRasterRecognitionData_Ptr;
begin
  if NoQueue_ and (FindDNNThread(DNNThread) > 0) then
    begin
      // skip this raster
      if instance_ then
          DisposeObject(Raster);
      Result := nil;
      exit;
    end;

  Result := BeforeInput(ID_, UserData_, Raster, instance_, TRasterRecognitionData_Metric) as TRasterRecognitionData_Metric;
  Result.L := L;

  New(p);
  p^.Data := Result;
  DNNThread.ProcessM(p, Result.Raster, False, {$IFDEF FPC}@{$ENDIF FPC}Do_Input_Metric_Result);
end;

function TRasterInputQueue.Input_LMetric(ID_: SystemString; UserData_: Pointer; Raster: TRaster; L: TLearn; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_LMetric): TRasterRecognitionData_LMetric;
var
  p: TRasterRecognitionData_Ptr;
begin
  if NoQueue_ and (FindDNNThread(DNNThread) > 0) then
    begin
      // skip this raster
      if instance_ then
          DisposeObject(Raster);
      Result := nil;
      exit;
    end;

  Result := BeforeInput(ID_, UserData_, Raster, instance_, TRasterRecognitionData_LMetric) as TRasterRecognitionData_LMetric;
  Result.L := L;

  New(p);
  p^.Data := Result;
  DNNThread.ProcessM(p, Result.Raster, False, {$IFDEF FPC}@{$ENDIF FPC}Do_Input_LMetric_Result);
end;

function TRasterInputQueue.Input_MMOD3L(ID_: SystemString; UserData_: Pointer; Raster: TRaster; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_MMOD3L): TRasterRecognitionData_MMOD3L;
var
  p: TRasterRecognitionData_Ptr;
begin
  if NoQueue_ and (FindDNNThread(DNNThread) > 0) then
    begin
      // skip this raster
      if instance_ then
          DisposeObject(Raster);
      Result := nil;
      exit;
    end;

  Result := BeforeInput(ID_, UserData_, Raster, instance_, TRasterRecognitionData_MMOD3L) as TRasterRecognitionData_MMOD3L;

  New(p);
  p^.Data := Result;
  DNNThread.ProcessM(p, Result.Raster, False, {$IFDEF FPC}@{$ENDIF FPC}Do_Input_MMOD3L_Result);
end;

function TRasterInputQueue.Input_MMOD6L(ID_: SystemString; UserData_: Pointer; Raster: TRaster; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_MMOD6L): TRasterRecognitionData_MMOD6L;
var
  p: TRasterRecognitionData_Ptr;
begin
  if NoQueue_ and (FindDNNThread(DNNThread) > 0) then
    begin
      // skip this raster
      if instance_ then
          DisposeObject(Raster);
      Result := nil;
      exit;
    end;

  Result := BeforeInput(ID_, UserData_, Raster, instance_, TRasterRecognitionData_MMOD6L) as TRasterRecognitionData_MMOD6L;

  New(p);
  p^.Data := Result;
  DNNThread.ProcessM(p, Result.Raster, False, {$IFDEF FPC}@{$ENDIF FPC}Do_Input_MMOD6L_Result);
end;

function TRasterInputQueue.Input_RNIC(ID_: SystemString; UserData_: Pointer; Raster: TRaster; ClassifierIndex: TPascalStringList; num_crops: Integer; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_RNIC): TRasterRecognitionData_RNIC;
var
  p: TRasterRecognitionData_Ptr;
begin
  if NoQueue_ and (FindDNNThread(DNNThread) > 0) then
    begin
      // skip this raster
      if instance_ then
          DisposeObject(Raster);
      Result := nil;
      exit;
    end;

  Result := BeforeInput(ID_, UserData_, Raster, instance_, TRasterRecognitionData_RNIC) as TRasterRecognitionData_RNIC;
  Result.ClassifierIndex := ClassifierIndex;

  New(p);
  p^.Data := Result;
  DNNThread.ProcessM(p, Result.Raster, num_crops, False, {$IFDEF FPC}@{$ENDIF FPC}Do_Input_RNIC_Result);
end;

function TRasterInputQueue.Input_LRNIC(ID_: SystemString; UserData_: Pointer; Raster: TRaster; ClassifierIndex: TPascalStringList; num_crops: Integer; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_LRNIC): TRasterRecognitionData_LRNIC;
var
  p: TRasterRecognitionData_Ptr;
begin
  if NoQueue_ and (FindDNNThread(DNNThread) > 0) then
    begin
      // skip this raster
      if instance_ then
          DisposeObject(Raster);
      Result := nil;
      exit;
    end;

  Result := BeforeInput(ID_, UserData_, Raster, instance_, TRasterRecognitionData_LRNIC) as TRasterRecognitionData_LRNIC;
  Result.ClassifierIndex := ClassifierIndex;

  New(p);
  p^.Data := Result;
  DNNThread.ProcessM(p, Result.Raster, num_crops, False, {$IFDEF FPC}@{$ENDIF FPC}Do_Input_LRNIC_Result);
end;

function TRasterInputQueue.Input_GDCNIC(ID_: SystemString; UserData_: Pointer; Raster: TRaster; ClassifierIndex: TPascalStringList; SS_Width, SS_Height: Integer; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_GDCNIC): TRasterRecognitionData_GDCNIC;
var
  p: TRasterRecognitionData_Ptr;
begin
  if NoQueue_ and (FindDNNThread(DNNThread) > 0) then
    begin
      // skip this raster
      if instance_ then
          DisposeObject(Raster);
      Result := nil;
      exit;
    end;

  Result := BeforeInput(ID_, UserData_, Raster, instance_, TRasterRecognitionData_GDCNIC) as TRasterRecognitionData_GDCNIC;
  Result.ClassifierIndex := ClassifierIndex;

  New(p);
  p^.Data := Result;
  DNNThread.ProcessM(p, Result.Raster, SS_Width, SS_Height, False, {$IFDEF FPC}@{$ENDIF FPC}Do_Input_GDCNIC_Result);
end;

function TRasterInputQueue.Input_GNIC(ID_: SystemString; UserData_: Pointer; Raster: TRaster; ClassifierIndex: TPascalStringList; SS_Width, SS_Height: Integer; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_GNIC): TRasterRecognitionData_GNIC;
var
  p: TRasterRecognitionData_Ptr;
begin
  if NoQueue_ and (FindDNNThread(DNNThread) > 0) then
    begin
      // skip this raster
      if instance_ then
          DisposeObject(Raster);
      Result := nil;
      exit;
    end;

  Result := BeforeInput(ID_, UserData_, Raster, instance_, TRasterRecognitionData_GNIC) as TRasterRecognitionData_GNIC;
  Result.ClassifierIndex := ClassifierIndex;

  New(p);
  p^.Data := Result;
  DNNThread.ProcessM(p, Result.Raster, SS_Width, SS_Height, False, {$IFDEF FPC}@{$ENDIF FPC}Do_Input_GNIC_Result);
end;

function TRasterInputQueue.Input_SS(ID_: SystemString; UserData_: Pointer; Raster: TRaster; ColorPool: TSegmentationColorTable; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_SS): TRasterRecognitionData_SS;
var
  p: TRasterRecognitionData_Ptr;
begin
  if NoQueue_ and (FindDNNThread(DNNThread) > 0) then
    begin
      // skip this raster
      if instance_ then
          DisposeObject(Raster);
      Result := nil;
      exit;
    end;

  Result := BeforeInput(ID_, UserData_, Raster, instance_, TRasterRecognitionData_SS) as TRasterRecognitionData_SS;
  Result.ColorPool := ColorPool;

  New(p);
  p^.Data := Result;
  DNNThread.ProcessM(p, Result.Raster, ColorPool, False, {$IFDEF FPC}@{$ENDIF FPC}Do_Input_SS_Result);
end;

function TRasterInputQueue.Input_ZMetric(ID_: SystemString; UserData_: Pointer; Raster: TRaster; L: TLearn; SS_Width, SS_Height: Integer; instance_, NoQueue_: Boolean; DNNThread: TAI_DNN_Thread_ZMetric): TRasterRecognitionData_ZMetric;
var
  p: TRasterRecognitionData_Ptr;
begin
  if NoQueue_ and (FindDNNThread(DNNThread) > 0) then
    begin
      // skip this raster
      if instance_ then
          DisposeObject(Raster);
      Result := nil;
      exit;
    end;

  Result := BeforeInput(ID_, UserData_, Raster, instance_, TRasterRecognitionData_ZMetric) as TRasterRecognitionData_ZMetric;
  Result.L := L;

  New(p);
  p^.Data := Result;
  DNNThread.ProcessM(p, Result.Raster, SS_Width, SS_Height, False, {$IFDEF FPC}@{$ENDIF FPC}Do_Input_ZMetric_Result);
end;

function TRasterInputQueue.FindDNNThread(DNNThread: TAI_DNN_Thread): Integer;
var
  i: Integer;
begin
  FCritical.Lock;
  Result := 0;
  for i := 0 to FQueue.Count - 1 do
    if FQueue[i].FDNNThread = DNNThread then
        inc(Result);
  FCritical.UnLock;
end;

function TRasterInputQueue.BusyNum: Integer;
var
  i: Integer;
begin
  Result := 0;
  FCritical.Lock;
  for i := 0 to FQueue.Count - 1 do
    if FQueue[i].Busy() then
        inc(Result);
  FCritical.UnLock;
end;

function TRasterInputQueue.Busy: Boolean;
begin
  Result := Busy(0, FQueue.Count - 1);
end;

function TRasterInputQueue.Busy(bIndex, eIndex: Integer): Boolean;
var
  i: Integer;
begin
  FCritical.Lock;
  Result := False;
  for i := umlMax(0, bIndex) to umlMin(FQueue.Count - 1, eIndex) do
      Result := Result or FQueue[i].Busy();
  FCritical.UnLock;
end;

function TRasterInputQueue.Delete(bIndex, eIndex: Integer): Boolean;
var
  i, j: Integer;
  RD: TRasterRecognitionData;
begin
  Result := False;
  if not Busy(bIndex, eIndex) then
    begin
      FCritical.Lock;
      for i := umlMax(0, bIndex) to umlMin(FQueue.Count - 1, eIndex) do
        begin
          RD := FQueue[bIndex];
          DisposeObject(RD);
          FQueue.Delete(bIndex);
        end;
      FCritical.UnLock;
      Result := True;
    end;
end;

procedure TRasterInputQueue.RemoveNullOutput;
var
  i, j: Integer;
  RD: TRasterRecognitionData;
begin
  FCritical.Lock;
  i := 0;
  while i < FQueue.Count do
    begin
      RD := FQueue[i];
      if (not RD.Busy) then
        begin
          DisposeObject(RD);
          FQueue.Delete(i);
        end
      else
          inc(i);
    end;
  FCritical.UnLock;
end;

procedure TRasterInputQueue.GetQueueState();
var
  i, j: Integer;
  RD: TRasterRecognitionData;
begin
  FCritical.Lock;
  for i := 0 to FQueue.Count - 1 do
    begin
      RD := FQueue[i];
      if RD.Busy then
        begin
        end
      else
        begin
        end;
    end;
  FCritical.UnLock;
end;

function TRasterInputQueue.Count: Integer;
begin
  Result := FQueue.Count;
end;

function TRasterInputQueue.LockQueue: TRasterRecognitionDataList;
begin
  FCritical.Lock;
  Result := FQueue;
end;

procedure TRasterInputQueue.UnLockQueue;
begin
  FCritical.UnLock;
end;

procedure TRasterInputQueue.Clean;
var
  i, j: Integer;
  RD: TRasterRecognitionData;
begin
  while Busy do
      TCompute.Sleep(1);

  FCritical.Lock;
  for i := 0 to FQueue.Count - 1 do
    begin
      RD := FQueue[i];
      DisposeObject(RD);
    end;
  FQueue.Clear;
  FCritical.UnLock;
end;

procedure TRasterInputQueue.DelayCheckBusyAndFree;
begin
  TCompute.RunM_NP({$IFDEF FPC}@{$ENDIF FPC}DoDelayCheckBusyAndFree);
end;

end.
