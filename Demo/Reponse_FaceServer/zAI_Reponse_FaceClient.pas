unit zAI_Reponse_FaceClient;

interface

uses
  CoreClasses, PascalStrings, UnicodeMixedLib, DoStatusIO,

{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  TextDataEngine, ListEngine, zDrawEngine, MemoryRaster, MemoryStream64,
  zAI_Common, zAI_TrainingTask, Geometry2DUnit,
  CommunicationFramework,
  DataFrameEngine,
  PhysicsIO;

type
  TFaceClient = class;

  TRecFace = record
    token: SystemString;
    k: Double;
    r: TRectV2;
  end;

{$IFDEF FPC}

  TRecFaceList = specialize TGenericsList<TRecFace>;
{$ELSE FPC}
  TRecFaceList = TGenericsList<TRecFace>;
{$ENDIF FPC}
  TOnRecFaceM = procedure(Sender: TFaceClient; successed: Boolean; input: TMemoryStream64; Faces: TRecFaceList) of object;
{$IFNDEF FPC}
  TOnRecFaceP = reference to procedure(Sender: TFaceClient; successed: Boolean; input: TMemoryStream64; Faces: TRecFaceList);
{$ENDIF FPC}

  TFaceClient = class(TPhysicsClient)
  private
    procedure RecFace_Result(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveFace(face_label: SystemString; Scale4x: Boolean; input: TMemoryRaster);

    procedure RecFace_M(input: TMemoryRaster; depthRec: Boolean; OnRecFace: TOnRecFaceM);
{$IFNDEF FPC}
    procedure RecFace_P(input: TMemoryRaster; depthRec: Boolean; OnRecFace: TOnRecFaceP);
{$ENDIF FPC}
    function GetFaceList: TArrayPascalString;
    function DownloadFace(token: SystemString): TAI_ImageList;
    function DeleteFace(token: SystemString): Integer;
    procedure UploadFace(token: SystemString; imgL: TAI_ImageList);
  end;

implementation

type
  TRecFaceOnResult = record
    input: TMemoryStream64;
    OnRecFaceM: TOnRecFaceM;
{$IFNDEF FPC}
    OnRecFaceP: TOnRecFaceP;
{$ENDIF FPC}
  end;

  PRecFaceOnResult = ^TRecFaceOnResult;

procedure TFaceClient.RecFace_Result(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
var
  p: PRecFaceOnResult;
  successed: Boolean;
  list: TRecFaceList;
  rf: TRecFace;
begin
  p := PRecFaceOnResult(Param1);
  successed := ResultData.Reader.ReadBool;
  list := TRecFaceList.Create;

  if successed then
    begin
      while ResultData.Reader.NotEnd do
        begin
          rf.token := ResultData.Reader.ReadString;
          rf.k := ResultData.Reader.ReadDouble;
          rf.r := ResultData.Reader.ReadRectV2;
          list.Add(rf);
        end;
    end
  else
      DoStatus(ResultData.Reader.ReadString);

  if Assigned(p^.OnRecFaceM) then
      p^.OnRecFaceM(Self, successed, p^.input, list);
{$IFNDEF FPC}
  if Assigned(p^.OnRecFaceP) then
      p^.OnRecFaceP(Self, successed, p^.input, list);
{$ENDIF FPC}
  disposeObject(list);
  disposeObject(p^.input);
  dispose(p);
end;

constructor TFaceClient.Create;
begin
  inherited Create;
  SwitchMaxPerformance;
  SendDataCompressed := True;
  CompleteBufferCompressed := True;
  SyncOnCompleteBuffer := True;
  SyncOnResult := True;
  QuietMode := True;
end;

destructor TFaceClient.Destroy;
begin
  inherited Destroy;
end;

procedure TFaceClient.SaveFace(face_label: SystemString; Scale4x: Boolean; input: TMemoryRaster);
var
  m64: TMemoryStream64;
  sendDE: TDataFrameEngine;
begin
  m64 := TMemoryStream64.Create;
  input.SaveToJpegRGBStream(m64, 80);
  SendCompleteBuffer('FaceBuffer', m64.Memory, m64.Size, True);
  m64.DiscardMemory;
  disposeObject(m64);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(face_label);
  sendDE.WriteBool(Scale4x);
  SendDirectStreamCmd('SaveFace', sendDE);
  disposeObject(sendDE);
end;

procedure TFaceClient.RecFace_M(input: TMemoryRaster; depthRec: Boolean; OnRecFace: TOnRecFaceM);
var
  sendDE: TDataFrameEngine;
  m64: TMemoryStream64;
  p: PRecFaceOnResult;
begin
  m64 := TMemoryStream64.Create;
  input.SaveToJpegRGBStream(m64, 50);
  SendCompleteBuffer('FaceBuffer', m64.Memory, m64.Size, False);
  Progress;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteBool(depthRec);

  new(p);
  FillPtrByte(p, SizeOf(p^), 0);
  p^.input := m64;
  p^.OnRecFaceM := OnRecFace;

  SendStreamCmdM('RecFace', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}RecFace_Result);
  Progress;

  disposeObject(sendDE);
end;

{$IFNDEF FPC}

procedure TFaceClient.RecFace_P(input: TMemoryRaster; depthRec: Boolean; OnRecFace: TOnRecFaceP);
var
  sendDE: TDataFrameEngine;
  m64: TMemoryStream64;
  p: PRecFaceOnResult;
begin
  m64 := TMemoryStream64.Create;
  input.SaveToJpegRGBStream(m64, 50);
  SendCompleteBuffer('FaceBuffer', m64.Memory, m64.Size, False);
  Progress;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteBool(depthRec);

  new(p);
  FillPtrByte(p, SizeOf(p^), 0);
  p^.input := m64;
  p^.OnRecFaceP := OnRecFace;

  SendStreamCmdM('RecFace', sendDE, p, nil, RecFace_Result);
  Progress;

  disposeObject(sendDE);
end;
{$ENDIF FPC}


function TFaceClient.GetFaceList: TArrayPascalString;
var
  sendDE, ResultDE: TDataFrameEngine;
  i: Integer;
begin
  sendDE := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;

  WaitSendStreamCmd('GetFaceList', sendDE, ResultDE, 5000);

  Setlength(Result, ResultDE.Count);
  for i := 0 to ResultDE.Count - 1 do
      Result[i] := ResultDE.ReadString(i);

  disposeObject(sendDE);
  disposeObject(ResultDE);
end;

function TFaceClient.DownloadFace(token: SystemString): TAI_ImageList;
var
  sendDE, ResultDE: TDataFrameEngine;
  m64: TMemoryStream64;
begin
  Result := nil;

  sendDE := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;

  sendDE.WriteString(token);

  WaitSendStreamCmd('DownloadFace', sendDE, ResultDE, C_Tick_Minute * 5);

  if ResultDE.Count > 0 then
    if ResultDE.Reader.ReadBool then
      begin
        m64 := TMemoryStream64.Create;
        ResultDE.Reader.ReadStream(m64);
        m64.Position := 0;
        Result := TAI_ImageList.Create;
        Result.LoadFromStream(m64, True);
        disposeObject(m64);
      end;

  disposeObject(sendDE);
  disposeObject(ResultDE);
end;

function TFaceClient.DeleteFace(token: SystemString): Integer;
var
  sendDE, ResultDE: TDataFrameEngine;
  i: Integer;
begin
  Result := 0;
  sendDE := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;

  sendDE.WriteString(token);
  WaitSendStreamCmd('DeleteFace', sendDE, ResultDE, 5000);

  if ResultDE.Count > 0 then
      Result := ResultDE.Reader.ReadInteger;

  disposeObject(sendDE);
  disposeObject(ResultDE);
end;

procedure TFaceClient.UploadFace(token: SystemString; imgL: TAI_ImageList);
var
  sendDE: TDataFrameEngine;
  m64: TMemoryStream64;
begin
  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(token);
  m64 := TMemoryStream64.Create;
  imgL.SaveToStream(m64, True, True, TRasterSave.rsJPEG_RGB_Qualily90);
  sendDE.WriteStream(m64);
  disposeObject(m64);

  SendDirectStreamCmd('UploadFace', sendDE);

  disposeObject(sendDE);
end;

end.
