{ ****************************************************************************** }
{ * zAI FFMPEG Support (platform: cuda+mkl+x64)                                * }
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
unit zAI_FFMPEG;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, PascalStrings, MemoryRaster, MemoryStream64, zAI, FFMPEG, FFMPEG_Reader;

type
  TAI_FFMPEG_Processor = class(TAI_IO_Processor)
  public
    PrepareDecode: integer;
  end;

  TAI_VideoStream = class(TFFMPEG_VideoStreamReader)
  protected
    UserData: Pointer;
    procedure DoVideoFillNewRaster(Raster: TMemoryRaster; var SaveToPool: Boolean); override;
    procedure DoWriteBufferAfter(p: Pointer; siz: NativeUInt; decodeFrameNum: integer); override;
  public
    DestroyDoFreeProcessor: Boolean;
    Processor: TAI_FFMPEG_Processor;
    DiscardDelayBuffer: Boolean;

    constructor Create(AI: TAI; IO_Class: TAI_IO_Class; UserData_: Pointer); overload;
    constructor Create(Processor_: TAI_FFMPEG_Processor; UserData_: Pointer); overload;
    destructor Destroy; override;
  end;

  TOnAI_OpenFFMPEGVideoProcessorDoneCall = procedure(thSender: TCompute; Processor: TAI_FFMPEG_Processor);
  TOnAI_OpenFFMPEGVideoProcessorCall = procedure(thSender: TCompute; Processor: TAI_FFMPEG_Processor; var ProcessStop: Boolean);
  TOnAI_OpenFFMPEGVideoProcessorDoneMethod = procedure(thSender: TCompute; Processor: TAI_FFMPEG_Processor) of object;
  TOnAI_OpenFFMPEGVideoProcessorMethod = procedure(thSender: TCompute; Processor: TAI_FFMPEG_Processor; var ProcessStop: Boolean) of object;

{$IFDEF FPC}
  TOnAI_OpenFFMPEGVideoProcessorDoneProc = procedure(thSender: TCompute; Processor: TAI_FFMPEG_Processor) is nested;
  TOnAI_OpenFFMPEGVideoProcessorProc = procedure(thSender: TCompute; Processor: TAI_FFMPEG_Processor; var ProcessStop: Boolean) is nested;
{$ELSE FPC}
  TOnAI_OpenFFMPEGVideoProcessorDoneProc = reference to procedure(thSender: TCompute; Processor: TAI_FFMPEG_Processor);
  TOnAI_OpenFFMPEGVideoProcessorProc = reference to procedure(thSender: TCompute; Processor: TAI_FFMPEG_Processor; var ProcessStop: Boolean);
{$ENDIF FPC}

function AI_OpenFFMPEGVideoProcessorFile(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer; VideoSource: SystemString): TAI_FFMPEG_Processor; overload;
function AI_OpenFFMPEGVideoProcessorFile(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer; VideoSource: SystemString; PrepareDecode: integer): TAI_FFMPEG_Processor; overload;
function AI_OpenFFMPEGVideoProcessorFile(AI_Parallel: TAI_Parallel; IO_Class: TAI_IO_Class; UserData: Pointer; VideoSource: SystemString): TAI_FFMPEG_Processor; overload;
function AI_OpenFFMPEGVideoProcessorFile(AI_Parallel: TAI_Parallel; IO_Class: TAI_IO_Class; UserData: Pointer; VideoSource: SystemString; PrepareDecode: integer): TAI_FFMPEG_Processor; overload;
function AI_OpenFFMPEGVideoProcessorFile(Processor_: TAI_FFMPEG_Processor; UserData: Pointer; VideoSource: SystemString): TAI_FFMPEG_Processor; overload;

function AI_OpenFFMPEGVideoProcessorFileC(IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorCall: TOnAI_OpenFFMPEGVideoProcessorCall;
  OnAI_OpenFFMPEGVideoProcessorDoneCall: TOnAI_OpenFFMPEGVideoProcessorDoneCall): TAI_FFMPEG_Processor; overload;
function AI_OpenFFMPEGVideoProcessorFileC(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorCall: TOnAI_OpenFFMPEGVideoProcessorCall;
  OnAI_OpenFFMPEGVideoProcessorDoneCall: TOnAI_OpenFFMPEGVideoProcessorDoneCall): TAI_FFMPEG_Processor; overload;
function AI_OpenFFMPEGVideoProcessorFileC(AI_Parallel: TAI_Parallel; IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorCall: TOnAI_OpenFFMPEGVideoProcessorCall;
  OnAI_OpenFFMPEGVideoProcessorDoneCall: TOnAI_OpenFFMPEGVideoProcessorDoneCall): TAI_FFMPEG_Processor; overload;

function AI_OpenFFMPEGVideoProcessorFileM(IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorMethod: TOnAI_OpenFFMPEGVideoProcessorMethod;
  OnAI_OpenFFMPEGVideoProcessorDoneMethod: TOnAI_OpenFFMPEGVideoProcessorDoneMethod): TAI_FFMPEG_Processor; overload;
function AI_OpenFFMPEGVideoProcessorFileM(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorMethod: TOnAI_OpenFFMPEGVideoProcessorMethod;
  OnAI_OpenFFMPEGVideoProcessorDoneMethod: TOnAI_OpenFFMPEGVideoProcessorDoneMethod): TAI_FFMPEG_Processor; overload;
function AI_OpenFFMPEGVideoProcessorFileM(AI_Parallel: TAI_Parallel; IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorMethod: TOnAI_OpenFFMPEGVideoProcessorMethod;
  OnAI_OpenFFMPEGVideoProcessorDoneMethod: TOnAI_OpenFFMPEGVideoProcessorDoneMethod): TAI_FFMPEG_Processor; overload;

function AI_OpenFFMPEGVideoProcessorFileP(IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorProc: TOnAI_OpenFFMPEGVideoProcessorProc;
  OnAI_OpenFFMPEGVideoProcessorDoneProc: TOnAI_OpenFFMPEGVideoProcessorDoneProc): TAI_FFMPEG_Processor; overload;
function AI_OpenFFMPEGVideoProcessorFileP(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorProc: TOnAI_OpenFFMPEGVideoProcessorProc;
  OnAI_OpenFFMPEGVideoProcessorDoneProc: TOnAI_OpenFFMPEGVideoProcessorDoneProc): TAI_FFMPEG_Processor; overload;
function AI_OpenFFMPEGVideoProcessorFileP(AI_Parallel: TAI_Parallel; IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorProc: TOnAI_OpenFFMPEGVideoProcessorProc;
  OnAI_OpenFFMPEGVideoProcessorDoneProc: TOnAI_OpenFFMPEGVideoProcessorDoneProc): TAI_FFMPEG_Processor; overload;

var
  FFMPEG_ActivtedThreadNum: integer;

implementation

procedure TAI_VideoStream.DoVideoFillNewRaster(Raster: TMemoryRaster; var SaveToPool: Boolean);
begin
  if DiscardDelayBuffer then
    begin
      SaveToPool := False;
      Processor.Input(Raster, True);
      while Processor.InputCount > Processor.PrepareDecode do
          Processor.RemoveFirstInput();
      Processor.Process(UserData);
    end;
end;

procedure TAI_VideoStream.DoWriteBufferAfter(p: Pointer; siz: NativeUInt; decodeFrameNum: integer);
var
  l: TMemoryRasterList;
  i: integer;
begin
  if DiscardDelayBuffer then
      exit;

  l := LockVideoPool;
  for i := 0 to l.Count - 1 do
      Processor.Input(l[i], True);
  l.Clear;
  Processor.Process(UserData);
  UnLockVideoPool(False);
end;

constructor TAI_VideoStream.Create(AI: TAI; IO_Class: TAI_IO_Class; UserData_: Pointer);
begin
  inherited Create;
  UserData := UserData_;
  DestroyDoFreeProcessor := True;
  Processor := TAI_FFMPEG_Processor.Create(IO_Class);
  Processor.AI := AI;
  Processor.PrepareDecode := 30;
  DiscardDelayBuffer := True;
end;

constructor TAI_VideoStream.Create(Processor_: TAI_FFMPEG_Processor; UserData_: Pointer);
begin
  inherited Create;
  UserData := UserData_;
  DestroyDoFreeProcessor := False;
  Processor := Processor_;
  DiscardDelayBuffer := True;
end;

destructor TAI_VideoStream.Destroy;
begin
  if DestroyDoFreeProcessor then
      DisposeObjectAndNil(Processor);
  inherited Destroy;
end;

type
  TAI_Video_FFMPEG_Reader = class(TFFMPEG_Reader)
  public
    DoneFreeProcessor: Boolean;
    Processor: TAI_FFMPEG_Processor;
    UserData: Pointer;

    OnAI_OpenFFMPEGVideoProcessorDoneCall: TOnAI_OpenFFMPEGVideoProcessorDoneCall;
    OnAI_OpenFFMPEGVideoProcessorCall: TOnAI_OpenFFMPEGVideoProcessorCall;
    OnAI_OpenFFMPEGVideoProcessorDoneMethod: TOnAI_OpenFFMPEGVideoProcessorDoneMethod;
    OnAI_OpenFFMPEGVideoProcessorMethod: TOnAI_OpenFFMPEGVideoProcessorMethod;
    OnAI_OpenFFMPEGVideoProcessorDoneProc: TOnAI_OpenFFMPEGVideoProcessorDoneProc;
    OnAI_OpenFFMPEGVideoProcessorProc: TOnAI_OpenFFMPEGVideoProcessorProc;
    procedure ComputeThread_Run(thSender: TCompute);
  end;

procedure TAI_Video_FFMPEG_Reader.ComputeThread_Run(thSender: TCompute);
var
  Raster: TMemoryRaster;
  ProcessStop: Boolean;
begin
  AtomInc(FFMPEG_ActivtedThreadNum);
  Raster := NewRaster();
  while True do
    begin
      ProcessStop := False;
      if Assigned(OnAI_OpenFFMPEGVideoProcessorCall) then
          OnAI_OpenFFMPEGVideoProcessorCall(thSender, Processor, ProcessStop);
      if Assigned(OnAI_OpenFFMPEGVideoProcessorMethod) then
          OnAI_OpenFFMPEGVideoProcessorMethod(thSender, Processor, ProcessStop);
      if Assigned(OnAI_OpenFFMPEGVideoProcessorProc) then
          OnAI_OpenFFMPEGVideoProcessorProc(thSender, Processor, ProcessStop);
      if ProcessStop then
          break;

      if (Processor.PrepareDecode <= 0) or (Processor.InputCount < Processor.PrepareDecode) then
        begin
          if not ReadFrame(Raster, False) then
              break;
          Processor.Input(Raster, False);
          Processor.Process(UserData);
        end
      else
          TCoreClassThread.Sleep(10);
    end;

  if Assigned(OnAI_OpenFFMPEGVideoProcessorDoneCall) then
      OnAI_OpenFFMPEGVideoProcessorDoneCall(thSender, Processor);
  if Assigned(OnAI_OpenFFMPEGVideoProcessorDoneMethod) then
      OnAI_OpenFFMPEGVideoProcessorDoneMethod(thSender, Processor);
  if Assigned(OnAI_OpenFFMPEGVideoProcessorDoneProc) then
      OnAI_OpenFFMPEGVideoProcessorDoneProc(thSender, Processor);
  if DoneFreeProcessor then
      DisposeObject(Processor);
  DisposeObject(Raster);
  DisposeObject(Self);
  AtomDec(FFMPEG_ActivtedThreadNum);
end;

function AI_OpenFFMPEGVideoProcessorFile(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer; VideoSource: SystemString): TAI_FFMPEG_Processor;
begin
  Result := AI_OpenFFMPEGVideoProcessorFile(AI, IO_Class, UserData, VideoSource, 10);
end;

function AI_OpenFFMPEGVideoProcessorFile(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer; VideoSource: SystemString; PrepareDecode: integer): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(VideoSource);
  reader.DoneFreeProcessor := False;
  reader.Processor := TAI_FFMPEG_Processor.Create(IO_Class);
  reader.Processor.AI := AI;
  reader.UserData := UserData;
  reader.Processor.PrepareDecode := PrepareDecode;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := nil;
  Result := reader.Processor;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

function AI_OpenFFMPEGVideoProcessorFile(AI_Parallel: TAI_Parallel; IO_Class: TAI_IO_Class; UserData: Pointer; VideoSource: SystemString): TAI_FFMPEG_Processor;
begin
  Result := AI_OpenFFMPEGVideoProcessorFile(AI_Parallel, IO_Class, UserData, VideoSource, 10);
end;

function AI_OpenFFMPEGVideoProcessorFile(AI_Parallel: TAI_Parallel; IO_Class: TAI_IO_Class; UserData: Pointer; VideoSource: SystemString; PrepareDecode: integer): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(VideoSource);
  reader.DoneFreeProcessor := False;
  reader.Processor := TAI_FFMPEG_Processor.Create(IO_Class);
  reader.Processor.AIPool := AI_Parallel;
  reader.UserData := UserData;
  reader.Processor.PrepareDecode := PrepareDecode;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := nil;
  Result := reader.Processor;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

function AI_OpenFFMPEGVideoProcessorFile(Processor_: TAI_FFMPEG_Processor; UserData: Pointer; VideoSource: SystemString): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(VideoSource);
  reader.DoneFreeProcessor := False;
  reader.Processor := Processor_;
  reader.UserData := UserData;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := nil;
  Result := reader.Processor;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

function AI_OpenFFMPEGVideoProcessorFileC(IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorCall: TOnAI_OpenFFMPEGVideoProcessorCall;
  OnAI_OpenFFMPEGVideoProcessorDoneCall: TOnAI_OpenFFMPEGVideoProcessorDoneCall): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(VideoSource);
  reader.DoneFreeProcessor := DoneFreeProcessor;
  reader.Processor := TAI_FFMPEG_Processor.Create(IO_Class);
  reader.Processor.ParallelProcessor := Parallel;
  reader.UserData := UserData;
  reader.Processor.PrepareDecode := PrepareDecode;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := OnAI_OpenFFMPEGVideoProcessorDoneCall;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := OnAI_OpenFFMPEGVideoProcessorCall;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := nil;
  Result := reader.Processor;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

function AI_OpenFFMPEGVideoProcessorFileC(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorCall: TOnAI_OpenFFMPEGVideoProcessorCall;
  OnAI_OpenFFMPEGVideoProcessorDoneCall: TOnAI_OpenFFMPEGVideoProcessorDoneCall): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(VideoSource);
  reader.DoneFreeProcessor := DoneFreeProcessor;
  reader.Processor := TAI_FFMPEG_Processor.Create(IO_Class);
  reader.Processor.AI := AI;
  reader.Processor.ParallelProcessor := Parallel;
  reader.UserData := UserData;
  reader.Processor.PrepareDecode := PrepareDecode;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := OnAI_OpenFFMPEGVideoProcessorDoneCall;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := OnAI_OpenFFMPEGVideoProcessorCall;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := nil;
  Result := reader.Processor;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

function AI_OpenFFMPEGVideoProcessorFileC(AI_Parallel: TAI_Parallel; IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorCall: TOnAI_OpenFFMPEGVideoProcessorCall;
  OnAI_OpenFFMPEGVideoProcessorDoneCall: TOnAI_OpenFFMPEGVideoProcessorDoneCall): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(VideoSource);
  reader.DoneFreeProcessor := DoneFreeProcessor;
  reader.Processor := TAI_FFMPEG_Processor.Create(IO_Class);
  reader.Processor.AIPool := AI_Parallel;
  reader.Processor.ParallelProcessor := Parallel;
  reader.UserData := UserData;
  reader.Processor.PrepareDecode := PrepareDecode;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := OnAI_OpenFFMPEGVideoProcessorDoneCall;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := OnAI_OpenFFMPEGVideoProcessorCall;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := nil;
  Result := reader.Processor;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

function AI_OpenFFMPEGVideoProcessorFileM(IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorMethod: TOnAI_OpenFFMPEGVideoProcessorMethod;
  OnAI_OpenFFMPEGVideoProcessorDoneMethod: TOnAI_OpenFFMPEGVideoProcessorDoneMethod): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(VideoSource);
  reader.DoneFreeProcessor := DoneFreeProcessor;
  reader.Processor := TAI_FFMPEG_Processor.Create(IO_Class);
  reader.Processor.ParallelProcessor := Parallel;
  reader.UserData := UserData;
  reader.Processor.PrepareDecode := PrepareDecode;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := OnAI_OpenFFMPEGVideoProcessorDoneMethod;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := OnAI_OpenFFMPEGVideoProcessorMethod;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := nil;
  Result := reader.Processor;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

function AI_OpenFFMPEGVideoProcessorFileM(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorMethod: TOnAI_OpenFFMPEGVideoProcessorMethod;
  OnAI_OpenFFMPEGVideoProcessorDoneMethod: TOnAI_OpenFFMPEGVideoProcessorDoneMethod): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(VideoSource);
  reader.DoneFreeProcessor := DoneFreeProcessor;
  reader.Processor := TAI_FFMPEG_Processor.Create(IO_Class);
  reader.Processor.AI := AI;
  reader.Processor.ParallelProcessor := Parallel;
  reader.UserData := UserData;
  reader.Processor.PrepareDecode := PrepareDecode;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := OnAI_OpenFFMPEGVideoProcessorDoneMethod;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := OnAI_OpenFFMPEGVideoProcessorMethod;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := nil;
  Result := reader.Processor;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

function AI_OpenFFMPEGVideoProcessorFileM(AI_Parallel: TAI_Parallel; IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorMethod: TOnAI_OpenFFMPEGVideoProcessorMethod;
  OnAI_OpenFFMPEGVideoProcessorDoneMethod: TOnAI_OpenFFMPEGVideoProcessorDoneMethod): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(VideoSource);
  reader.DoneFreeProcessor := DoneFreeProcessor;
  reader.Processor := TAI_FFMPEG_Processor.Create(IO_Class);
  reader.Processor.AIPool := AI_Parallel;
  reader.Processor.ParallelProcessor := Parallel;
  reader.UserData := UserData;
  reader.Processor.PrepareDecode := PrepareDecode;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := OnAI_OpenFFMPEGVideoProcessorDoneMethod;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := OnAI_OpenFFMPEGVideoProcessorMethod;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := nil;
  Result := reader.Processor;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

function AI_OpenFFMPEGVideoProcessorFileP(IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorProc: TOnAI_OpenFFMPEGVideoProcessorProc;
  OnAI_OpenFFMPEGVideoProcessorDoneProc: TOnAI_OpenFFMPEGVideoProcessorDoneProc): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(VideoSource);
  reader.DoneFreeProcessor := DoneFreeProcessor;
  reader.Processor := TAI_FFMPEG_Processor.Create(IO_Class);
  reader.Processor.ParallelProcessor := Parallel;
  reader.UserData := UserData;
  reader.Processor.PrepareDecode := PrepareDecode;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := OnAI_OpenFFMPEGVideoProcessorDoneProc;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := OnAI_OpenFFMPEGVideoProcessorProc;
  Result := reader.Processor;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

function AI_OpenFFMPEGVideoProcessorFileP(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorProc: TOnAI_OpenFFMPEGVideoProcessorProc;
  OnAI_OpenFFMPEGVideoProcessorDoneProc: TOnAI_OpenFFMPEGVideoProcessorDoneProc): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(VideoSource);
  reader.DoneFreeProcessor := DoneFreeProcessor;
  reader.Processor := TAI_FFMPEG_Processor.Create(IO_Class);
  reader.Processor.AI := AI;
  reader.Processor.ParallelProcessor := Parallel;
  reader.UserData := UserData;
  reader.Processor.PrepareDecode := PrepareDecode;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := OnAI_OpenFFMPEGVideoProcessorDoneProc;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := OnAI_OpenFFMPEGVideoProcessorProc;
  Result := reader.Processor;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

function AI_OpenFFMPEGVideoProcessorFileP(AI_Parallel: TAI_Parallel; IO_Class: TAI_IO_Class; UserData: Pointer;
  VideoSource: SystemString; PrepareDecode: integer; DoneFreeProcessor, Parallel: Boolean;
  OnAI_OpenFFMPEGVideoProcessorProc: TOnAI_OpenFFMPEGVideoProcessorProc;
  OnAI_OpenFFMPEGVideoProcessorDoneProc: TOnAI_OpenFFMPEGVideoProcessorDoneProc): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(VideoSource);
  reader.DoneFreeProcessor := DoneFreeProcessor;
  reader.Processor := TAI_FFMPEG_Processor.Create(IO_Class);
  reader.Processor.AIPool := AI_Parallel;
  reader.Processor.ParallelProcessor := Parallel;
  reader.UserData := UserData;
  reader.Processor.PrepareDecode := PrepareDecode;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := OnAI_OpenFFMPEGVideoProcessorDoneProc;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := OnAI_OpenFFMPEGVideoProcessorProc;
  Result := reader.Processor;
  TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

initialization

FFMPEG_ActivtedThreadNum := 0;

end.
