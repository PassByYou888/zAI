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
type
  TLearn_thread = class(TCoreClassThread)
  public
    Source: TLearn;
    TrainDepth: TLInt;
    OnStateC: TLearnState_Call;
    OnStateM: TLearnState_Method;
    OnStateP: TLearnState_Proc;
    Successed: Boolean;

    procedure SyncResultState;
    procedure Execute; override;
    constructor Create;
    destructor Destroy; override;
  end;

procedure TLearn_thread.SyncResultState;
begin
  Source.FTrainingThreadRuning := False;
  if Assigned(OnStateC) then
      OnStateC(Source, Successed);
  if Assigned(OnStateM) then
      OnStateM(Source, Successed);
  if Assigned(OnStateP) then
      OnStateP(Source, Successed);
end;

procedure TLearn_thread.Execute;
begin
  if Source <> nil then
      Successed := Source.Training(TrainDepth)
  else
      Successed := False;

  Synchronize({$IFDEF FPC}@{$ENDIF FPC}SyncResultState);
end;

constructor TLearn_thread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Source := nil;
  TrainDepth := 2;
  OnStateC := nil;
  OnStateM := nil;
  OnStateP := nil;
  Successed := False;
end;

destructor TLearn_thread.Destroy;
begin
  inherited Destroy;
end;
