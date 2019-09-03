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

  TOnAI_OpenFFMPEGVideoProcessorDoneCall = procedure(thSender: TComputeThread; Processor: TAI_FFMPEG_Processor);
  TOnAI_OpenFFMPEGVideoProcessorCall = procedure(thSender: TComputeThread; Processor: TAI_FFMPEG_Processor; var ProcessStop: Boolean);
  TOnAI_OpenFFMPEGVideoProcessorDoneMethod = procedure(thSender: TComputeThread; Processor: TAI_FFMPEG_Processor) of object;
  TOnAI_OpenFFMPEGVideoProcessorMethod = procedure(thSender: TComputeThread; Processor: TAI_FFMPEG_Processor; var ProcessStop: Boolean) of object;
{$IFNDEF FPC}
  TOnAI_OpenFFMPEGVideoProcessorDoneProc = reference to procedure(thSender: TComputeThread; Processor: TAI_FFMPEG_Processor);
  TOnAI_OpenFFMPEGVideoProcessorProc = reference to procedure(thSender: TComputeThread; Processor: TAI_FFMPEG_Processor; var ProcessStop: Boolean);
{$ENDIF FPC}

function AI_OpenFFMPEGVideoProcessorFile(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer; MPEGFile: SystemString): TAI_FFMPEG_Processor; overload;
function AI_OpenFFMPEGVideoProcessorFile(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer; MPEGFile: SystemString; PrepareDecode: integer): TAI_FFMPEG_Processor; overload;
function AI_OpenFFMPEGVideoProcessorFile(Processor_: TAI_FFMPEG_Processor; UserData: Pointer; MPEGFile: SystemString): TAI_FFMPEG_Processor; overload;

function AI_OpenFFMPEGVideoProcessorFileC(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer;
  MPEGFile: SystemString; PrepareDecode: integer; DoneFreeProcessor: Boolean;
  OnAI_OpenFFMPEGVideoProcessorCall: TOnAI_OpenFFMPEGVideoProcessorCall;
  OnAI_OpenFFMPEGVideoProcessorDoneCall: TOnAI_OpenFFMPEGVideoProcessorDoneCall): TAI_FFMPEG_Processor;

function AI_OpenFFMPEGVideoProcessorFileM(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer;
  MPEGFile: SystemString; PrepareDecode: integer; DoneFreeProcessor: Boolean;
  OnAI_OpenFFMPEGVideoProcessorMethod: TOnAI_OpenFFMPEGVideoProcessorMethod;
  OnAI_OpenFFMPEGVideoProcessorDoneMethod: TOnAI_OpenFFMPEGVideoProcessorDoneMethod): TAI_FFMPEG_Processor;

{$IFNDEF FPC}
function AI_OpenFFMPEGVideoProcessorFileP(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer;
  MPEGFile: SystemString; PrepareDecode: integer; DoneFreeProcessor: Boolean;
  OnAI_OpenFFMPEGVideoProcessorProc: TOnAI_OpenFFMPEGVideoProcessorProc;
  OnAI_OpenFFMPEGVideoProcessorDoneProc: TOnAI_OpenFFMPEGVideoProcessorDoneProc): TAI_FFMPEG_Processor;
{$ENDIF FPC}


var
  FFMPEG_ActivtedThreadNum: integer;

implementation

procedure TAI_VideoStream.DoVideoFillNewRaster(Raster: TMemoryRaster; var SaveToPool: Boolean);
begin
  if DiscardDelayBuffer then
    begin
      SaveToPool := False;
      if Processor.InputCount < Processor.PrepareDecode then
        begin
          Processor.Input(Raster, True);
          Processor.Process(UserData);
        end
      else
        begin
          DisposeObject(Raster);
        end;
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
  Processor := TAI_FFMPEG_Processor.Create(AI, IO_Class);
  Processor.PrepareDecode := 150;
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
{$IFNDEF FPC}
    OnAI_OpenFFMPEGVideoProcessorDoneProc: TOnAI_OpenFFMPEGVideoProcessorDoneProc;
    OnAI_OpenFFMPEGVideoProcessorProc: TOnAI_OpenFFMPEGVideoProcessorProc;
{$ENDIF FPC}
    procedure ComputeThread_Run(thSender: TComputeThread);
  end;

procedure TAI_Video_FFMPEG_Reader.ComputeThread_Run(thSender: TComputeThread);
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
{$IFNDEF FPC}
      if Assigned(OnAI_OpenFFMPEGVideoProcessorProc) then
          OnAI_OpenFFMPEGVideoProcessorProc(thSender, Processor, ProcessStop);
{$ENDIF FPC}
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
{$IFNDEF FPC}
  if Assigned(OnAI_OpenFFMPEGVideoProcessorDoneProc) then
      OnAI_OpenFFMPEGVideoProcessorDoneProc(thSender, Processor);
{$ENDIF FPC}
  if DoneFreeProcessor then
      DisposeObject(Processor);
  DisposeObject(Raster);
  DisposeObject(Self);
  AtomDec(FFMPEG_ActivtedThreadNum);
end;

function AI_OpenFFMPEGVideoProcessorFile(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer; MPEGFile: SystemString): TAI_FFMPEG_Processor;
begin
  Result := AI_OpenFFMPEGVideoProcessorFile(AI, IO_Class, UserData, MPEGFile, 10);
end;

function AI_OpenFFMPEGVideoProcessorFile(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer; MPEGFile: SystemString; PrepareDecode: integer): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(MPEGFile);
  reader.DoneFreeProcessor := False;
  reader.Processor := TAI_FFMPEG_Processor.Create(AI, IO_Class);
  reader.UserData := UserData;
  reader.Processor.PrepareDecode := PrepareDecode;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := nil;
{$IFNDEF FPC}
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := nil;
{$ENDIF FPC}
  Result := reader.Processor;
  TComputeThread.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

function AI_OpenFFMPEGVideoProcessorFile(Processor_: TAI_FFMPEG_Processor; UserData: Pointer; MPEGFile: SystemString): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(MPEGFile);
  reader.DoneFreeProcessor := False;
  reader.Processor := Processor_;
  reader.UserData := UserData;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := nil;
{$IFNDEF FPC}
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := nil;
{$ENDIF FPC}
  Result := reader.Processor;
  TComputeThread.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

function AI_OpenFFMPEGVideoProcessorFileC(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer;
  MPEGFile: SystemString; PrepareDecode: integer; DoneFreeProcessor: Boolean;
  OnAI_OpenFFMPEGVideoProcessorCall: TOnAI_OpenFFMPEGVideoProcessorCall;
  OnAI_OpenFFMPEGVideoProcessorDoneCall: TOnAI_OpenFFMPEGVideoProcessorDoneCall): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(MPEGFile);
  reader.DoneFreeProcessor := DoneFreeProcessor;
  reader.Processor := TAI_FFMPEG_Processor.Create(AI, IO_Class);
  reader.UserData := UserData;
  reader.Processor.PrepareDecode := PrepareDecode;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := OnAI_OpenFFMPEGVideoProcessorDoneCall;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := OnAI_OpenFFMPEGVideoProcessorCall;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := nil;
{$IFNDEF FPC}
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := nil;
{$ENDIF FPC}
  Result := reader.Processor;
  TComputeThread.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

function AI_OpenFFMPEGVideoProcessorFileM(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer;
  MPEGFile: SystemString; PrepareDecode: integer; DoneFreeProcessor: Boolean;
  OnAI_OpenFFMPEGVideoProcessorMethod: TOnAI_OpenFFMPEGVideoProcessorMethod;
  OnAI_OpenFFMPEGVideoProcessorDoneMethod: TOnAI_OpenFFMPEGVideoProcessorDoneMethod): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(MPEGFile);
  reader.DoneFreeProcessor := DoneFreeProcessor;
  reader.Processor := TAI_FFMPEG_Processor.Create(AI, IO_Class);
  reader.UserData := UserData;
  reader.Processor.PrepareDecode := PrepareDecode;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := OnAI_OpenFFMPEGVideoProcessorDoneMethod;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := OnAI_OpenFFMPEGVideoProcessorMethod;
{$IFNDEF FPC}
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := nil;
{$ENDIF FPC}
  Result := reader.Processor;
  TComputeThread.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}reader.ComputeThread_Run);
end;

{$IFNDEF FPC}


function AI_OpenFFMPEGVideoProcessorFileP(AI: TAI; IO_Class: TAI_IO_Class; UserData: Pointer;
  MPEGFile: SystemString; PrepareDecode: integer; DoneFreeProcessor: Boolean;
  OnAI_OpenFFMPEGVideoProcessorProc: TOnAI_OpenFFMPEGVideoProcessorProc;
  OnAI_OpenFFMPEGVideoProcessorDoneProc: TOnAI_OpenFFMPEGVideoProcessorDoneProc): TAI_FFMPEG_Processor;
var
  reader: TAI_Video_FFMPEG_Reader;
begin
  reader := TAI_Video_FFMPEG_Reader.Create(MPEGFile);
  reader.DoneFreeProcessor := DoneFreeProcessor;
  reader.Processor := TAI_FFMPEG_Processor.Create(AI, IO_Class);
  reader.UserData := UserData;
  reader.Processor.PrepareDecode := PrepareDecode;

  reader.OnAI_OpenFFMPEGVideoProcessorDoneCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorCall := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorMethod := nil;
  reader.OnAI_OpenFFMPEGVideoProcessorDoneProc := OnAI_OpenFFMPEGVideoProcessorDoneProc;
  reader.OnAI_OpenFFMPEGVideoProcessorProc := OnAI_OpenFFMPEGVideoProcessorProc;
  Result := reader.Processor;
  TComputeThread.RunM(nil, nil, reader.ComputeThread_Run);
end;
{$ENDIF FPC}

initialization

FFMPEG_ActivtedThreadNum := 0;

end.
