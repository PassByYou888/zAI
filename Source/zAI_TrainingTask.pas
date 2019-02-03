{ ****************************************************************************** }
{ * AI Training task Support                                                   * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit zAI_TrainingTask;

{$INCLUDE zDefine.inc}

interface

uses Types,
  CoreClasses,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  PascalStrings, MemoryStream64, UnicodeMixedLib, DataFrameEngine, CoreCipher,
  ZDBEngine, ZDBLocalManager, ObjectDataManager, ObjectData, ItemStream,
  DoStatusIO, ListEngine, Geometry2DUnit, MemoryRaster, LearnTypes, Learn, zAI;

type
  TTrainingTask = class(TCoreClassObject)
  private
    procedure On_Save_DoStatus(AText: SystemString; const ID: Integer);
  public
    DB_Stream: TMemoryStream64;
    DB_Engine: TObjectDataManager;
    LastWriteFileList: TPascalStringList;
    LastReadMD5, LastWriteMD5: TMD5;
    TaskLogStatus: TPascalStringList;

    constructor Create;
    class function OpenTask(filename: SystemString): TTrainingTask; overload;
    class function OpenTask(stream: TCoreClassStream): TTrainingTask; overload;
    class function CreateTask: TTrainingTask;
    destructor Destroy; override;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure SaveToFile(filename: SystemString);

    procedure Write(name: SystemString; m64: TMemoryStream64); overload;
    procedure Write(name: SystemString; data: THashVariantList); overload;
    procedure Write(name: SystemString; data: TPascalStringList); overload;
    procedure Write(name: SystemString; data: TCoreClassStrings); overload;
    procedure Write(name: SystemString; data: TMemoryRaster); overload;
    procedure Write(name: SystemString; data: TAI_ImageList); overload;
    procedure WriteFile(name: SystemString; fromfile: SystemString); overload;

    procedure Read(name: SystemString; m64: TMemoryStream64); overload;
    procedure Read(name: SystemString; data: THashVariantList); overload;
    procedure Read(name: SystemString; data: TPascalStringList); overload;
    procedure Read(name: SystemString; data: TCoreClassStrings); overload;
    procedure Read(name: SystemString; data: TMemoryRaster); overload;
    procedure Read(name: SystemString; data: TAI_ImageList); overload;
    procedure ReadToFile(name: SystemString; destfile: SystemString); overload;

    function Exists(name: SystemString): Boolean;
    function Delete(name: SystemString): Boolean;

    procedure CopyTo(LocalName: SystemString; dest: TTrainingTask; destName: SystemString); overload;

    // check file for training before
    function CheckTrainingBefore(const paramFile: SystemString; var report: SystemString): Boolean;
    // check file for training after
    function CheckTrainingAfter(const paramFile: SystemString; var report: SystemString): Boolean;

    // prepare again training file list
    function RebuildTrainingData(const paramFile: SystemString; var report: SystemString; dest: TTrainingTask): Boolean;

    // training on AI
    function RunTraining(const AI: TAI; const paramFile: SystemString): Boolean;

    // load output data to AI
    function LoadTrainingOutput(const paramFile: SystemString; AI: TAI; var report: SystemString): Boolean; overload;
    function LoadTrainingOutput(const paramFile: SystemString; AI_P: TAI_Parallel; var report: SystemString): Boolean; overload;

    procedure ExportLastWriteToStream(stream: TMemoryStream64);
    procedure ExportLastWriteToFile(filename: SystemString);
  end;

implementation

procedure TTrainingTask.On_Save_DoStatus(AText: SystemString; const ID: Integer);
begin
  if TaskLogStatus <> nil then
      TaskLogStatus.Add(umlDateTimeToStr(umlNow()) + #9 + AText);
end;

constructor TTrainingTask.Create;
begin
  inherited Create;
  DB_Stream := nil;
  DB_Engine := nil;
  LastWriteFileList := nil;
  LastReadMD5 := NullMD5;
  LastWriteMD5 := NullMD5;
  TaskLogStatus := nil;
  AddDoStatusHook(Self, {$IFDEF FPC}@{$ENDIF FPC}On_Save_DoStatus);
end;

class function TTrainingTask.OpenTask(filename: SystemString): TTrainingTask;
begin
  Result := TTrainingTask.Create;
  Result.DB_Stream := TMemoryStream64.CustomCreate($FFFF);
  Result.DB_Stream.LoadFromFile(filename);
  Result.DB_Engine := TObjectDataManagerOfCache.CreateAsStream(Result.DB_Stream, '', DBMarshal.ID, False, False, False);
  Result.LastWriteFileList := TPascalStringList.Create;
  Result.TaskLogStatus := TPascalStringList.Create;

end;

class function TTrainingTask.OpenTask(stream: TCoreClassStream): TTrainingTask;
begin
  Result := TTrainingTask.Create;
  Result.DB_Stream := TMemoryStream64.CustomCreate($FFFF);
  Result.DB_Stream.LoadFromStream(stream);
  Result.DB_Stream.Position := 0;
  Result.DB_Engine := TObjectDataManagerOfCache.CreateAsStream(Result.DB_Stream, '', DBMarshal.ID, False, False, False);
  Result.LastWriteFileList := TPascalStringList.Create;
  Result.TaskLogStatus := TPascalStringList.Create;
end;

class function TTrainingTask.CreateTask: TTrainingTask;
begin
  Result := TTrainingTask.Create;
  Result.DB_Stream := TMemoryStream64.CustomCreate($FFFF);
  Result.DB_Engine := TObjectDataManagerOfCache.CreateAsStream(Result.DB_Stream, '', DBMarshal.ID, False, True, False);
  Result.LastWriteFileList := TPascalStringList.Create;
  Result.TaskLogStatus := TPascalStringList.Create;
end;

destructor TTrainingTask.Destroy;
begin
  DeleteDoStatusHook(Self);
  DisposeObject(DB_Engine);
  DisposeObject(DB_Stream);
  DisposeObject(LastWriteFileList);
  DisposeObject(TaskLogStatus);
  inherited Destroy;
end;

procedure TTrainingTask.SaveToStream(stream: TCoreClassStream);
var
  temp_db: TObjectDataManager;
begin
  DB_Engine.UpdateIO;
  temp_db := TObjectDataManagerOfCache.CreateAsStream(stream, '', DB_Engine.DefaultItemID, False, True, False);
  DB_Engine.CopyTo(temp_db);
  DisposeObject(temp_db);
end;

procedure TTrainingTask.SaveToFile(filename: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(filename, fmCreate);
  SaveToStream(fs);
  DisposeObject(fs);
end;

procedure TTrainingTask.Write(name: SystemString; m64: TMemoryStream64);
begin
  if not DB_Engine.ItemWriteFromStream('/', Name, m64) then
      RaiseInfo('training task write item %s failed.', [name]);
  LastWriteMD5 := umlStreamMD5(m64);
  if LastWriteFileList.ExistsValue(name) < 0 then
      LastWriteFileList.Add(name);
end;

procedure TTrainingTask.Write(name: SystemString; data: THashVariantList);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  data.SaveToStream(m64);
  Write(Name, m64);
  DisposeObject(m64);
end;

procedure TTrainingTask.Write(name: SystemString; data: TPascalStringList);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  data.SaveToStream(m64);
  Write(Name, m64);
  DisposeObject(m64);
end;

procedure TTrainingTask.Write(name: SystemString; data: TCoreClassStrings);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  data.SaveToStream(m64);
  Write(Name, m64);
  DisposeObject(m64);
end;

procedure TTrainingTask.Write(name: SystemString; data: TMemoryRaster);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  data.SaveToBmp24Stream(m64);
  Write(Name, m64);
  DisposeObject(m64);
end;

procedure TTrainingTask.Write(name: SystemString; data: TAI_ImageList);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  data.SaveToStream(m64, True, False);
  Write(Name, m64);
  DisposeObject(m64);
end;

procedure TTrainingTask.WriteFile(name: SystemString; fromfile: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  if umlFileExists(fromfile) then
    begin
      try
          m64.LoadFromFile(fromfile);
      except
      end;
    end;
  write(name, m64);
  DisposeObject(m64);
end;

procedure TTrainingTask.Read(name: SystemString; m64: TMemoryStream64);
begin
  if not DB_Engine.ItemReadToStream('/', name, m64) then
      RaiseInfo('training task read item %s failed.', [name]);
  LastReadMD5 := umlStreamMD5(m64);
  m64.Position := 0;
end;

procedure TTrainingTask.Read(name: SystemString; data: THashVariantList);
var
  m64: TMemoryStream64;
begin
  data.Clear;
  m64 := TMemoryStream64.Create;
  read(name, m64);
  data.LoadFromStream(m64);
  DisposeObject(m64);
end;

procedure TTrainingTask.Read(name: SystemString; data: TPascalStringList);
var
  m64: TMemoryStream64;
begin
  data.Clear;
  m64 := TMemoryStream64.Create;
  read(name, m64);
  data.LoadFromStream(m64);
  DisposeObject(m64);
end;

procedure TTrainingTask.Read(name: SystemString; data: TCoreClassStrings);
var
  m64: TMemoryStream64;
begin
  data.Clear;
  m64 := TMemoryStream64.Create;
  read(name, m64);
  data.LoadFromStream(m64);
  DisposeObject(m64);
end;

procedure TTrainingTask.Read(name: SystemString; data: TMemoryRaster);
var
  m64: TMemoryStream64;
begin
  data.Clear;
  m64 := TMemoryStream64.Create;
  read(name, m64);
  data.LoadFromStream(m64);
  DisposeObject(m64);
end;

procedure TTrainingTask.Read(name: SystemString; data: TAI_ImageList);
var
  m64: TMemoryStream64;
begin
  data.Clear;
  m64 := TMemoryStream64.Create;
  read(name, m64);
  data.LoadFromStream(m64);
  DisposeObject(m64);
end;

procedure TTrainingTask.ReadToFile(name: SystemString; destfile: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  read(name, m64);
  try
      m64.SaveToFile(destfile);
  except
  end;
  DisposeObject(m64);
end;

function TTrainingTask.Exists(name: SystemString): Boolean;
begin
  Result := DB_Engine.ItemExists('/', name);
end;

function TTrainingTask.Delete(name: SystemString): Boolean;
begin
  Result := DB_Engine.ItemDelete('/', name);
  LastWriteFileList.DeletePascalString(name);
end;

procedure TTrainingTask.CopyTo(LocalName: SystemString; dest: TTrainingTask; destName: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  read(LocalName, m64);
  m64.Position := 0;
  dest.Write(destName, m64);
  DisposeObject(m64);
end;

function TTrainingTask.CheckTrainingBefore(const paramFile: SystemString; var report: SystemString): Boolean;
var
  Param: THashVariantList;
  ComputeFunc: TPascalString;
  inputfile1, inputfile2: SystemString;
begin
  Result := False;
  Param := THashVariantList.Create;

  Read(paramFile, Param);

  if Param.Exists('func') then
      ComputeFunc := Param['func']
  else if Param.Exists('compute') then
      ComputeFunc := Param['compute']
  else
      ComputeFunc := Param.GetDefaultValue('ComputeFunc', '');

  ComputeFunc := ComputeFunc.TrimChar(#32#9);

  if umlMultipleMatch(['surf', 'fastsurf'], ComputeFunc) then
    begin
      inputfile1 := Param.GetDefaultValue('source', '');
      inputfile2 := Param.GetDefaultValue('dest', '');
      Result := Exists(inputfile1) and Exists(inputfile2);
      if not Result then
          report := PFormat('error training source: %s', [inputfile1])
    end
  else if umlMultipleMatch(['TrainOD', 'TrainingOD', 'TrainObjectDetector'], ComputeFunc) then
    begin
      inputfile1 := Param.GetDefaultValue('source', '');
      Result := Exists(inputfile1);
      if not Result then
          report := PFormat('error training source: %s', [inputfile1])
    end
  else if umlMultipleMatch(['TrainOD_Marshal', 'TrainingOD_Marshal', 'TrainObjectDetectorMarshal'], ComputeFunc) then
    begin
      inputfile1 := Param.GetDefaultValue('source', '');
      Result := Exists(inputfile1);
      if not Result then
          report := PFormat('error training source: %s', [inputfile1])
    end
  else if umlMultipleMatch(['TrainSP', 'TrainingSP', 'TrainShapePredictor'], ComputeFunc) then
    begin
      inputfile1 := Param.GetDefaultValue('source', '');
      Result := Exists(inputfile1);
      if not Result then
          report := PFormat('error training source: %s', [inputfile1])
    end
  else if umlMultipleMatch(['TrainMRN', 'TrainingMRN', 'TrainMetricResNet'], ComputeFunc) then
    begin
      inputfile1 := Param.GetDefaultValue('source', '');
      Result := Exists(inputfile1);
      if not Result then
          report := PFormat('error training source: %s', [inputfile1])
    end
  else if umlMultipleMatch(['TrainMMOD', 'TrainingMMOD', 'TrainMaxMarginDNNObjectDetector'], ComputeFunc) then
    begin
      inputfile1 := Param.GetDefaultValue('source', '');
      Result := Exists(inputfile1);
      if not Result then
          report := PFormat('error training source: %s', [inputfile1])
    end
  else if umlMultipleMatch(['TrainRNIC', 'TrainingRNIC', 'TrainResNetImageClassifier'], ComputeFunc) then
    begin
      inputfile1 := Param.GetDefaultValue('source', '');
      Result := Exists(inputfile1);
      if not Result then
          report := PFormat('error training source: %s', [inputfile1])
    end
  else
    begin
      report := 'illegal ComputeFunc.';
    end;

  DisposeObject(Param);

  if Result then
      report := 'solve.';
end;

function TTrainingTask.CheckTrainingAfter(const paramFile: SystemString; var report: SystemString): Boolean;
var
  Param: THashVariantList;
  ResultValues: THashVariantList;
  ComputeFunc: TPascalString;
  outputfile: SystemString;
begin
  Result := False;
  if not Exists(paramFile) then
    begin
      report := PFormat('error param file: %s', [paramFile]);
      DisposeObject(Param);
      exit;
    end;

  Param := THashVariantList.Create;
  Read(paramFile, Param);

  outputfile := Param.GetDefaultValue('Result', 'Result.txt');
  if not Exists(outputfile) then
    begin
      report := PFormat('error result file: %s', [outputfile]);
      DisposeObject(Param);
      exit;
    end;

  ResultValues := THashVariantList.Create;
  Read(outputfile, ResultValues);

  if Param.Exists('func') then
      ComputeFunc := Param['func']
  else if Param.Exists('compute') then
      ComputeFunc := Param['compute']
  else
      ComputeFunc := Param.GetDefaultValue('ComputeFunc', '');

  ComputeFunc := ComputeFunc.TrimChar(#32#9);

  if ResultValues.GetDefaultValue('Result', False) = False then
    begin
      report := 'Training Result Error.';
    end
  else if umlMultipleMatch(['surf', 'fastsurf'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output.bmp');
      Result := Exists(outputfile);
      if not Result then
          report := PFormat('error training output: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainOD', 'TrainingOD', 'TrainObjectDetector'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output' + C_OD_Ext);
      Result := Exists(outputfile);
      if not Result then
          report := PFormat('error training output: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainOD_Marshal', 'TrainingOD_Marshal', 'TrainObjectDetectorMarshal'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output' + C_OD_Marshal_Ext);
      Result := Exists(outputfile);
      if not Result then
          report := PFormat('error training output: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainSP', 'TrainingSP', 'TrainShapePredictor'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output' + C_SP_Ext);
      Result := Exists(outputfile);
      if not Result then
          report := PFormat('error training output: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainMRN', 'TrainingMRN', 'TrainMetricResNet'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output.sync', 'output' + C_Metric_ResNet_Ext + '.sync');
      if Exists(outputfile) then
        begin
          outputfile := Param.GetDefaultValue('output', 'output' + C_Metric_ResNet_Ext);
          Result := Exists(outputfile);
          if not Result then
              report := PFormat('error training output: %s', [outputfile]);
        end
      else
          report := PFormat('error training sync file: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainMMOD', 'TrainingMMOD', 'TrainMaxMarginDNNObjectDetector'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output.sync', 'output' + C_MMOD_Ext + '.sync');
      if Exists(outputfile) then
        begin
          outputfile := Param.GetDefaultValue('output', 'output' + C_MMOD_Ext);
          Result := Exists(outputfile);
          if not Result then
              report := PFormat('error training output: %s', [outputfile]);
        end
      else
          report := PFormat('error training sync file: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainRNIC', 'TrainingRNIC', 'TrainResNetImageClassifier'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output.sync', 'output' + C_RNIC_Ext + '.sync');
      if Exists(outputfile) then
        begin
          outputfile := Param.GetDefaultValue('output', 'output' + C_RNIC_Ext);
          Result := Exists(outputfile);
          if not Result then
              report := PFormat('error training output: %s', [outputfile]);
        end
      else
          report := PFormat('error training sync file: %s', [outputfile]);
    end
  else
    begin
      report := 'illegal ComputeFunc.';
    end;

  DisposeObject([Param, ResultValues]);
  if Result then
      report := 'solve.';
end;

function TTrainingTask.RebuildTrainingData(const paramFile: SystemString; var report: SystemString; dest: TTrainingTask): Boolean;
var
  Param: THashVariantList;
  ResultValues: THashVariantList;
  ComputeFunc: TPascalString;
  inputfile1, inputfile2: SystemString;
  outputfile, syncfile: SystemString;

  m1, m2: TStream64;
begin
  Result := CheckTrainingBefore(paramFile, report);
  if not Result then
      exit;
  Result := CheckTrainingAfter(paramFile, report);
  if not Result then
      exit;

  Result := False;
  Param := THashVariantList.Create;
  Read(paramFile, Param);
  outputfile := Param.GetDefaultValue('Result', 'Result.txt');
  ResultValues := THashVariantList.Create;
  Read(outputfile, ResultValues);

  if Param.Exists('func') then
      ComputeFunc := Param['func']
  else if Param.Exists('compute') then
      ComputeFunc := Param['compute']
  else
      ComputeFunc := Param.GetDefaultValue('ComputeFunc', '');

  ComputeFunc := ComputeFunc.TrimChar(#32#9);

  if umlMultipleMatch(['surf', 'fastsurf'], ComputeFunc) then
    begin
      inputfile1 := Param.GetDefaultValue('source', '');
      inputfile2 := Param.GetDefaultValue('dest', '');
      outputfile := Param.GetDefaultValue('output', 'output.bmp');
      CopyTo(paramFile, dest, paramFile);
      CopyTo(inputfile1, dest, inputfile1);
      CopyTo(inputfile2, dest, inputfile2);
      Result := True;
    end
  else if umlMultipleMatch(['TrainOD', 'TrainingOD', 'TrainObjectDetector'], ComputeFunc) then
    begin
      inputfile1 := Param.GetDefaultValue('source', '');
      outputfile := Param.GetDefaultValue('output', 'output' + C_OD_Ext);
      CopyTo(paramFile, dest, paramFile);
      CopyTo(inputfile1, dest, inputfile1);
      Result := True;
    end
  else if umlMultipleMatch(['TrainOD_Marshal', 'TrainingOD_Marshal', 'TrainObjectDetectorMarshal'], ComputeFunc) then
    begin
      inputfile1 := Param.GetDefaultValue('source', '');
      outputfile := Param.GetDefaultValue('output', 'output' + C_OD_Marshal_Ext);
      CopyTo(paramFile, dest, paramFile);
      CopyTo(inputfile1, dest, inputfile1);
      Result := True;
    end
  else if umlMultipleMatch(['TrainSP', 'TrainingSP', 'TrainShapePredictor'], ComputeFunc) then
    begin
      inputfile1 := Param.GetDefaultValue('source', '');
      outputfile := Param.GetDefaultValue('output', 'output' + C_SP_Ext);
      CopyTo(paramFile, dest, paramFile);
      CopyTo(inputfile1, dest, inputfile1);
      Result := True;
    end
  else if umlMultipleMatch(['TrainMRN', 'TrainingMRN', 'TrainMetricResNet'], ComputeFunc) then
    begin
      inputfile1 := Param.GetDefaultValue('source', '');
      inputfile2 := Param.GetDefaultValue('syncfile', 'output' + C_Metric_ResNet_Ext + '.sync');
      syncfile := Param.GetDefaultValue('output.sync', 'output' + C_Metric_ResNet_Ext + '.sync');
      outputfile := Param.GetDefaultValue('output', 'output' + C_Metric_ResNet_Ext);
      CopyTo(paramFile, dest, paramFile);
      CopyTo(inputfile1, dest, inputfile1);
      CopyTo(syncfile, dest, inputfile2);
      Result := True;
    end
  else if umlMultipleMatch(['TrainMMOD', 'TrainingMMOD', 'TrainMaxMarginDNNObjectDetector'], ComputeFunc) then
    begin
      inputfile1 := Param.GetDefaultValue('source', '');
      inputfile2 := Param.GetDefaultValue('syncfile', 'output' + C_MMOD_Ext + '.sync');
      syncfile := Param.GetDefaultValue('output.sync', 'output' + C_MMOD_Ext + '.sync');
      outputfile := Param.GetDefaultValue('output', 'output' + C_MMOD_Ext);
      CopyTo(paramFile, dest, paramFile);
      CopyTo(inputfile1, dest, inputfile1);
      CopyTo(syncfile, dest, inputfile2);
      Result := True;
    end
  else if umlMultipleMatch(['TrainRNIC', 'TrainingRNIC', 'TrainResNetImageClassifier'], ComputeFunc) then
    begin
      inputfile1 := Param.GetDefaultValue('source', '');
      inputfile2 := Param.GetDefaultValue('syncfile', 'output' + C_RNIC_Ext + '.sync');
      syncfile := Param.GetDefaultValue('output.sync', 'output' + C_RNIC_Ext + '.sync');
      outputfile := Param.GetDefaultValue('output', 'output' + C_RNIC_Ext);
      CopyTo(paramFile, dest, paramFile);
      CopyTo(inputfile1, dest, inputfile1);
      CopyTo(syncfile, dest, inputfile2);
      Result := True;
    end
  else
    begin
      report := 'illegal ComputeFunc.';
    end;

  DisposeObject([Param, ResultValues]);
  if Result then
      report := 'solve.';
end;

function TTrainingTask.RunTraining(const AI: TAI; const paramFile: SystemString): Boolean;
var
  Param: THashVariantList;
  ComputeFunc: SystemString;

  param_md5: TMD5;

  // batch free
  inputfile1, inputfile2: SystemString;
  inputstream1, inputstream2: TMemoryStream64;
  inputraster1, inputraster2: TMemoryRaster;
  inputImgList: TAI_ImageList;
  ResultValues: THashVariantList;

  // manual free
  outputstream: TMemoryStream64;
  outputPacalStringList: TPascalStringList;
  outputraster: TMemoryRaster;
  local_sync, sync_file, output_file: SystemString;
  scale: TGeoFloat;

  metric_resnet_param: PMetric_ResNet_Train_Parameter;
  mmod_param: PMMOD_Train_Parameter;
  rnic_param: PRNIC_Train_Parameter;
begin
  Result := False;
  if not AI.Activted then
      exit;

  LastWriteFileList.Clear;

  Param := THashVariantList.Create;
  Read(paramFile, Param);
  param_md5 := LastReadMD5;

  if Param.Exists('func') then
      ComputeFunc := Param['func']
  else if Param.Exists('compute') then
      ComputeFunc := Param['compute']
  else
      ComputeFunc := Param.GetDefaultValue('ComputeFunc', '');

  DoStatus('input training parameter.');
  DoStatus(Param.AsText);

  inputfile1 := '';
  inputfile2 := '';
  inputstream1 := TMemoryStream64.Create;
  inputstream2 := TMemoryStream64.Create;
  inputraster1 := TMemoryRaster.Create;
  inputraster2 := TMemoryRaster.Create;
  inputImgList := TAI_ImageList.Create;
  ResultValues := THashVariantList.Create;

  ResultValues['Begin'] := umlNow();

  try
    if umlMultipleMatch(['surf', 'fastsurf'], ComputeFunc) then
      begin
        inputfile1 := Param.GetDefaultValue('source', '');
        inputfile2 := Param.GetDefaultValue('dest', '');

        if Exists(inputfile1) and Exists(inputfile2) then
          begin
            try
              Read(inputfile1, inputraster1);
              Read(inputfile2, inputraster2);
              inputraster1.scale(Param.GetDefaultValue('scale', 1.0));
              inputraster2.scale(Param.GetDefaultValue('scale', 1.0));
              outputraster := AI.BuildSurfMatchOutput(inputraster1, inputraster2);

              write(Param.GetDefaultValue('output', 'output.bmp'), outputraster);
              DisposeObject(outputraster);
              Result := True;
            except
            end;
          end;
      end
    else if umlMultipleMatch(['TrainOD', 'TrainingOD', 'TrainObjectDetector'], ComputeFunc) then
      begin
        inputfile1 := Param.GetDefaultValue('source', '');

        if Exists(inputfile1) then
          begin
            try
              Read(inputfile1, inputImgList);
              inputImgList.scale(Param.GetDefaultValue('scale', 1.0));
              outputstream := AI.OD_Train_Stream(
                inputImgList,
                Param.GetDefaultValue('window_width', 80),
                Param.GetDefaultValue('window_height', 80),
                Param.GetDefaultValue('thread', 2)
                );
              if outputstream <> nil then
                begin
                  write(Param.GetDefaultValue('output', 'output' + C_OD_Ext), outputstream);
                  DisposeObject(outputstream);
                  Result := True;
                end;
            except
            end;
          end;
      end
    else if umlMultipleMatch(['TrainOD_Marshal', 'TrainingOD_Marshal', 'TrainObjectDetectorMarshal'], ComputeFunc) then
      begin
        inputfile1 := Param.GetDefaultValue('source', '');

        if Exists(inputfile1) then
          begin
            try
              Read(inputfile1, inputImgList);
              inputImgList.scale(Param.GetDefaultValue('scale', 1.0));
              outputstream := AI.OD_Marshal_Train(
                inputImgList,
                Param.GetDefaultValue('window_width', 80),
                Param.GetDefaultValue('window_height', 80),
                Param.GetDefaultValue('thread', 2)
                );
              if outputstream <> nil then
                begin
                  write(Param.GetDefaultValue('output', 'output' + C_OD_Marshal_Ext), outputstream);
                  DisposeObject(outputstream);
                  Result := True;
                end;
            except
            end;
          end;
      end
    else if umlMultipleMatch(['TrainSP', 'TrainingSP', 'TrainShapePredictor'], ComputeFunc) then
      begin
        inputfile1 := Param.GetDefaultValue('source', '');

        if Exists(inputfile1) then
          begin
            try
              Read(inputfile1, inputImgList);
              inputImgList.scale(Param.GetDefaultValue('scale', 1.0));
              outputstream := AI.SP_Train_Stream(
                inputImgList,
                Param.GetDefaultValue('oversampling_amount', 300),
                Param.GetDefaultValue('tree_depth', 2),
                Param.GetDefaultValue('thread', 2)
                );
              if outputstream <> nil then
                begin
                  write(Param.GetDefaultValue('output', 'output' + C_SP_Ext), outputstream);
                  DisposeObject(outputstream);
                  Result := True;
                end;
            except
            end;
          end;
      end
    else if umlMultipleMatch(['TrainMRN', 'TrainingMRN', 'TrainMetricResNet'], ComputeFunc) then
      begin
        inputfile1 := Param.GetDefaultValue('source', '');

        if Exists(inputfile1) then
          begin
            try
              Read(inputfile1, inputImgList);

              local_sync := Param.GetDefaultValue('syncfile', 'output' + C_Metric_ResNet_Ext + '.sync');
              sync_file := umlCombineFileName(AI.RootPath, local_sync + '_' + umlMD5ToStr(umlCombineMD5(param_md5, LastReadMD5)));
              if Exists(local_sync) then
                if not umlFileExists(sync_file) then
                    ReadToFile(local_sync, sync_file);

              output_file := umlMD5ToStr(umlCombineMD5(param_md5, LastReadMD5)) + C_Metric_ResNet_Ext;

              if umlFileExists(output_file) then
                begin
                  outputstream := TMemoryStream64.Create;
                  outputstream.LoadFromFile(output_file);
                  outputstream.Position := 0;
                end
              else
                begin
                  metric_resnet_param := TAI.Init_Metric_ResNet_Parameter(sync_file, output_file);

                  metric_resnet_param^.timeout := Param.GetDefaultValue('timeout', metric_resnet_param^.timeout);

                  metric_resnet_param^.weight_decay := Param.GetDefaultValue('weight_decay', metric_resnet_param^.weight_decay);
                  metric_resnet_param^.momentum := Param.GetDefaultValue('momentum', metric_resnet_param^.momentum);
                  metric_resnet_param^.iterations_without_progress_threshold := Param.GetDefaultValue('iterations_without_progress_threshold', metric_resnet_param^.iterations_without_progress_threshold);
                  metric_resnet_param^.learning_rate := Param.GetDefaultValue('learning_rate', metric_resnet_param^.learning_rate);
                  metric_resnet_param^.completed_learning_rate := Param.GetDefaultValue('completed_learning_rate', metric_resnet_param^.completed_learning_rate);
                  metric_resnet_param^.step_mini_batch_target_num := Param.GetDefaultValue('step_mini_batch_target_num', metric_resnet_param^.step_mini_batch_target_num);
                  metric_resnet_param^.step_mini_batch_raster_num := Param.GetDefaultValue('step_mini_batch_raster_num', metric_resnet_param^.step_mini_batch_raster_num);

                  metric_resnet_param^.fullGPU_Training := Param.GetDefaultValue('fullGPU_Training', metric_resnet_param^.fullGPU_Training);

                  outputstream := AI.Metric_ResNet_Train_Stream(
                    inputImgList,
                    metric_resnet_param);

                  TAI.Free_Metric_ResNet_Parameter(metric_resnet_param);
                end;

              if outputstream <> nil then
                begin
                  write(Param.GetDefaultValue('output', 'output' + C_Metric_ResNet_Ext), outputstream);
                  WriteFile(Param.GetDefaultValue('output.sync', 'output' + C_Metric_ResNet_Ext + '.sync'), sync_file);
                  DisposeObject(outputstream);
                  ResultValues['Loss'] := AI.Last_training_average_loss;
                  ResultValues['Rate'] := AI.Last_training_learning_rate;
                  Result := True;
                end;
              umlDeleteFile(sync_file);
            except
            end;
          end;
      end
    else if umlMultipleMatch(['TrainMMOD', 'TrainingMMOD', 'TrainMaxMarginDNNObjectDetector'], ComputeFunc) then
      begin
        inputfile1 := Param.GetDefaultValue('source', '');

        if Exists(inputfile1) then
          begin
            try
              Read(inputfile1, inputImgList);
              inputImgList.scale(Param.GetDefaultValue('scale', 1.0));

              local_sync := Param.GetDefaultValue('syncfile', 'output' + C_MMOD_Ext + '.sync');
              sync_file := umlCombineFileName(AI.RootPath, local_sync + '_' + umlMD5ToStr(umlCombineMD5(param_md5, LastReadMD5)));
              if Exists(local_sync) then
                if not umlFileExists(sync_file) then
                    ReadToFile(local_sync, sync_file);

              mmod_param := AI.MMOD_DNN_PrepareTrain(inputImgList, sync_file);

              mmod_param^.timeout := Param.GetDefaultValue('timeout', mmod_param^.timeout);
              mmod_param^.target_size := Param.GetDefaultValue('target_size', mmod_param^.target_size);
              mmod_param^.min_target_size := Param.GetDefaultValue('min_target_size', mmod_param^.min_target_size);
              mmod_param^.min_detector_window_overlap_iou := Param.GetDefaultValue('min_detector_window_overlap_iou', mmod_param^.min_detector_window_overlap_iou);
              mmod_param^.iterations_without_progress_threshold := Param.GetDefaultValue('iterations_without_progress_threshold', mmod_param^.iterations_without_progress_threshold);
              mmod_param^.learning_rate := Param.GetDefaultValue('learning_rate', mmod_param^.learning_rate);
              mmod_param^.completed_learning_rate := Param.GetDefaultValue('completed_learning_rate', mmod_param^.completed_learning_rate);
              mmod_param^.num_crops := Param.GetDefaultValue('num_crops', mmod_param^.num_crops);
              mmod_param^.chip_dims_x := Param.GetDefaultValue('chip_dims_x', mmod_param^.chip_dims_x);
              mmod_param^.chip_dims_y := Param.GetDefaultValue('chip_dims_y', mmod_param^.chip_dims_y);
              mmod_param^.min_object_size_x := Param.GetDefaultValue('min_object_size_x', mmod_param^.min_object_size_x);
              mmod_param^.min_object_size_y := Param.GetDefaultValue('min_object_size_y', mmod_param^.min_object_size_y);
              mmod_param^.max_rotation_degrees := Param.GetDefaultValue('max_rotation_degrees', mmod_param^.max_rotation_degrees);
              mmod_param^.max_object_size := Param.GetDefaultValue('max_object_size', mmod_param^.max_object_size);

              outputstream := AI.MMOD_DNN_Train_Stream(mmod_param);
              AI.MMOD_DNN_FreeTrain(mmod_param);

              if outputstream <> nil then
                begin
                  write(Param.GetDefaultValue('output', 'output' + C_MMOD_Ext), outputstream);
                  WriteFile(Param.GetDefaultValue('output.sync', 'output' + C_MMOD_Ext + '.sync'), sync_file);
                  DisposeObject(outputstream);
                  ResultValues['Loss'] := AI.Last_training_average_loss;
                  ResultValues['Rate'] := AI.Last_training_learning_rate;
                  Result := True;
                end;
              umlDeleteFile(sync_file);
            except
            end;
          end;
      end
    else if umlMultipleMatch(['TrainRNIC', 'TrainingRNIC', 'TrainResNetImageClassifier'], ComputeFunc) then
      begin
        inputfile1 := Param.GetDefaultValue('source', '');

        if Exists(inputfile1) then
          begin
            outputPacalStringList := TPascalStringList.Create;
            try
              Read(inputfile1, inputImgList);
              inputImgList.scale(Param.GetDefaultValue('scale', 1.0));

              local_sync := Param.GetDefaultValue('syncfile', 'output' + C_RNIC_Ext + '.sync');
              sync_file := umlCombineFileName(AI.RootPath, local_sync + '_' + umlMD5ToStr(umlCombineMD5(param_md5, LastReadMD5)));
              if Exists(local_sync) then
                if not umlFileExists(sync_file) then
                    ReadToFile(local_sync, sync_file);

              output_file := umlMD5ToStr(umlCombineMD5(param_md5, LastReadMD5)) + C_Metric_ResNet_Ext;

              rnic_param := TAI.Init_RNIC_Train_Parameter(sync_file, output_file);

              rnic_param^.timeout := Param.GetDefaultValue('timeout', rnic_param^.timeout);
              rnic_param^.iterations_without_progress_threshold := Param.GetDefaultValue('iterations_without_progress_threshold', rnic_param^.iterations_without_progress_threshold);
              rnic_param^.learning_rate := Param.GetDefaultValue('learning_rate', rnic_param^.learning_rate);
              rnic_param^.completed_learning_rate := Param.GetDefaultValue('completed_learning_rate', rnic_param^.completed_learning_rate);
              rnic_param^.all_bn_running_stats_window_sizes := Param.GetDefaultValue('all_bn_running_stats_window_sizes', rnic_param^.all_bn_running_stats_window_sizes);
              rnic_param^.img_mini_batch := Param.GetDefaultValue('img_mini_batch', rnic_param^.img_mini_batch);

              outputstream := AI.RNIC_Train_Stream(
                inputImgList,
                rnic_param,
                outputPacalStringList
                );

              TAI.Free_RNIC_Train_Parameter(rnic_param);

              if outputstream <> nil then
                begin
                  write(Param.GetDefaultValue('output', 'output' + C_RNIC_Ext), outputstream);
                  WriteFile(Param.GetDefaultValue('output.sync', 'output' + C_RNIC_Ext + '.sync'), sync_file);
                  write(Param.GetDefaultValue('output.index', 'output' + C_RNIC_Ext + '.index'), outputPacalStringList);
                  DisposeObject(outputstream);
                  ResultValues['Loss'] := AI.Last_training_average_loss;
                  ResultValues['Rate'] := AI.Last_training_learning_rate;
                  Result := True;
                end;
              umlDeleteFile(sync_file);
            except
            end;
            DisposeObject(outputPacalStringList);
          end;
      end
    else
      begin
        DoStatus('AI Training task failed: no define ComputeFunc.');
      end;
  finally
    ResultValues['Result'] := Result;
    ResultValues['End'] := umlNow();
    Write(Param.GetDefaultValue('Result', 'Result.txt'), ResultValues);
    Write(Param.GetDefaultValue('LogFile', 'Log.txt'), TaskLogStatus);
    if Result then
      begin
        if LastWriteFileList.ExistsValue(paramFile) < 0 then
            LastWriteFileList.Add(paramFile);
        Write(Param.GetDefaultValue('LastOutput', 'LastOutput.txt'), LastWriteFileList);
      end;
  end;

  DisposeObject(Param);
  DisposeObject([inputstream1, inputstream2]);
  DisposeObject([inputraster1, inputraster2]);
  DisposeObject(inputImgList);
  DisposeObject(ResultValues);
end;

function TTrainingTask.LoadTrainingOutput(const paramFile: SystemString; AI: TAI; var report: SystemString): Boolean;
var
  Param: THashVariantList;
  ResultValues: THashVariantList;
  ComputeFunc: TPascalString;
  outputfile: SystemString;
  m64: TMemoryStream64;
begin
  Result := False;
  if not Exists(paramFile) then
    begin
      report := PFormat('error param file: %s', [paramFile]);
      DisposeObject(Param);
      exit;
    end;

  Param := THashVariantList.Create;
  Read(paramFile, Param);

  outputfile := Param.GetDefaultValue('Result', 'Result.txt');
  if not Exists(outputfile) then
    begin
      report := PFormat('error result file: %s', [outputfile]);
      DisposeObject(Param);
      exit;
    end;

  ResultValues := THashVariantList.Create;
  Read(outputfile, ResultValues);

  if Param.Exists('func') then
      ComputeFunc := Param['func']
  else if Param.Exists('compute') then
      ComputeFunc := Param['compute']
  else
      ComputeFunc := Param.GetDefaultValue('ComputeFunc', '');

  ComputeFunc := ComputeFunc.TrimChar(#32#9);

  if ResultValues.GetDefaultValue('Result', False) = False then
    begin
      report := 'Training Result Error.';
    end
  else if umlMultipleMatch(['surf', 'fastsurf'], ComputeFunc) then
    begin
      Result := False;
      report := PFormat('surf not require.', []);
    end
  else if umlMultipleMatch(['TrainOD', 'TrainingOD', 'TrainObjectDetector'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output' + C_OD_Ext);
      if Exists(outputfile) then
        begin
          if AI.Parallel_OD_Hnd <> nil then
              AI.OD_Close(AI.Parallel_OD_Hnd);

          m64 := TMemoryStream64.Create;
          Read(outputfile, m64);
          AI.Parallel_OD_Hnd := AI.OD_Open_Stream(m64);
          DisposeObject(m64);
          Result := AI.Parallel_OD_Hnd <> nil;
        end
      else
          report := PFormat('error training output: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainOD_Marshal', 'TrainingOD_Marshal', 'TrainObjectDetectorMarshal'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output' + C_OD_Marshal_Ext);
      if Exists(outputfile) then
        begin
          if AI.Parallel_OD_Marshal_Hnd <> nil then
              AI.OD_Marshal_Close(AI.Parallel_OD_Marshal_Hnd);

          m64 := TMemoryStream64.Create;
          Read(outputfile, m64);
          AI.Parallel_OD_Marshal_Hnd := AI.OD_Marshal_Open_Stream(m64);
          DisposeObject(m64);
          Result := AI.Parallel_OD_Marshal_Hnd <> nil;
        end
      else
          report := PFormat('error training output: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainSP', 'TrainingSP', 'TrainShapePredictor'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output' + C_SP_Ext);
      if Exists(outputfile) then
        begin
          if AI.Parallel_SP_Hnd <> nil then
              AI.SP_Close(AI.Parallel_SP_Hnd);

          m64 := TMemoryStream64.Create;
          Read(outputfile, m64);
          AI.Parallel_SP_Hnd := AI.SP_Open_Stream(m64);
          DisposeObject(m64);
          Result := AI.Parallel_SP_Hnd <> nil;
        end
      else
          report := PFormat('error training output: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainMRN', 'TrainingMRN', 'TrainMetricResNet'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output' + C_Metric_ResNet_Ext);
      if Exists(outputfile) then
        begin
          if AI.Parallel_MDNN_Hnd <> nil then
              AI.Metric_ResNet_Close(AI.Parallel_MDNN_Hnd);

          m64 := TMemoryStream64.Create;
          Read(outputfile, m64);
          AI.Parallel_MDNN_Hnd := AI.Metric_ResNet_Open_Stream(m64);
          DisposeObject(m64);
          Result := AI.Parallel_MDNN_Hnd <> nil;
        end
      else
          report := PFormat('error training output: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainMMOD', 'TrainingMMOD', 'TrainMaxMarginDNNObjectDetector'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output' + C_MMOD_Ext);
      if Exists(outputfile) then
        begin
          if AI.Parallel_MMOD_Hnd <> nil then
              AI.MMOD_DNN_Close(AI.Parallel_MMOD_Hnd);

          m64 := TMemoryStream64.Create;
          Read(outputfile, m64);
          AI.Parallel_MMOD_Hnd := AI.MMOD_DNN_Open_Stream(m64);
          DisposeObject(m64);
          Result := AI.Parallel_MMOD_Hnd <> nil;
        end
      else
          report := PFormat('error training output: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainRNIC', 'TrainingRNIC', 'TrainResNetImageClassifier'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output' + C_RNIC_Ext);
      if Exists(outputfile) then
        begin
          if AI.Parallel_RNIC_Hnd <> nil then
              AI.RNIC_Close(AI.Parallel_RNIC_Hnd);

          m64 := TMemoryStream64.Create;
          Read(outputfile, m64);
          AI.Parallel_RNIC_Hnd := AI.RNIC_Open_Stream(m64);
          DisposeObject(m64);
          Result := AI.Parallel_RNIC_Hnd <> nil;
        end
      else
          report := PFormat('error training output: %s', [outputfile]);
    end
  else
    begin
      report := 'illegal ComputeFunc.';
    end;

  DisposeObject([Param, ResultValues]);
  if Result then
      report := 'solve.';
end;

function TTrainingTask.LoadTrainingOutput(const paramFile: SystemString; AI_P: TAI_Parallel; var report: SystemString): Boolean;
var
  Param: THashVariantList;
  ResultValues: THashVariantList;
  ComputeFunc: TPascalString;
  outputfile: SystemString;
  m64: TMemoryStream64;
begin
  Result := False;
  if not Exists(paramFile) then
    begin
      report := PFormat('error param file: %s', [paramFile]);
      DisposeObject(Param);
      exit;
    end;

  Param := THashVariantList.Create;
  Read(paramFile, Param);

  outputfile := Param.GetDefaultValue('Result', 'Result.txt');
  if not Exists(outputfile) then
    begin
      report := PFormat('error result file: %s', [outputfile]);
      DisposeObject(Param);
      exit;
    end;

  ResultValues := THashVariantList.Create;
  Read(outputfile, ResultValues);

  if Param.Exists('func') then
      ComputeFunc := Param['func']
  else if Param.Exists('compute') then
      ComputeFunc := Param['compute']
  else
      ComputeFunc := Param.GetDefaultValue('ComputeFunc', '');

  ComputeFunc := ComputeFunc.TrimChar(#32#9);

  if ResultValues.GetDefaultValue('Result', False) = False then
    begin
      report := 'Training Result Error.';
    end
  else if umlMultipleMatch(['surf', 'fastsurf'], ComputeFunc) then
    begin
      Result := False;
      report := PFormat('surf not require.', []);
    end
  else if umlMultipleMatch(['TrainOD', 'TrainingOD', 'TrainObjectDetector'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output' + C_OD_Ext);
      if Exists(outputfile) then
        begin
          m64 := TMemoryStream64.Create;
          Read(outputfile, m64);
          AI_P.Prepare_OD(m64);
          DisposeObject(m64);
          Result := True;
        end
      else
          report := PFormat('error training output: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainOD_Marshal', 'TrainingOD_Marshal', 'TrainObjectDetectorMarshal'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output' + C_OD_Marshal_Ext);
      if Exists(outputfile) then
        begin
          m64 := TMemoryStream64.Create;
          Read(outputfile, m64);
          AI_P.Prepare_OD_Marshal(m64);
          DisposeObject(m64);
          Result := True;
        end
      else
          report := PFormat('error training output: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainSP', 'TrainingSP', 'TrainShapePredictor'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output' + C_SP_Ext);
      if Exists(outputfile) then
        begin
          m64 := TMemoryStream64.Create;
          Read(outputfile, m64);
          AI_P.Prepare_SP(m64);
          DisposeObject(m64);
          Result := True;
        end
      else
          report := PFormat('error training output: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainMRN', 'TrainingMRN', 'TrainMetricResNet'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output' + C_Metric_ResNet_Ext);
      if Exists(outputfile) then
        begin
          m64 := TMemoryStream64.Create;
          Read(outputfile, m64);
          AI_P.Prepare_MDNN(m64);
          DisposeObject(m64);
          Result := True;
        end
      else
          report := PFormat('error training output: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainMMOD', 'TrainingMMOD', 'TrainMaxMarginDNNObjectDetector'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output' + C_MMOD_Ext);
      if Exists(outputfile) then
        begin
          m64 := TMemoryStream64.Create;
          Read(outputfile, m64);
          AI_P.Prepare_MMOD(m64);
          DisposeObject(m64);
          Result := True;
        end
      else
          report := PFormat('error training output: %s', [outputfile]);
    end
  else if umlMultipleMatch(['TrainRNIC', 'TrainingRNIC', 'TrainResNetImageClassifier'], ComputeFunc) then
    begin
      outputfile := Param.GetDefaultValue('output', 'output' + C_RNIC_Ext);
      if Exists(outputfile) then
        begin
          m64 := TMemoryStream64.Create;
          Read(outputfile, m64);
          AI_P.Prepare_RNIC(m64);
          DisposeObject(m64);
          Result := True;
        end
      else
          report := PFormat('error training output: %s', [outputfile]);
    end
  else
    begin
      report := 'illegal ComputeFunc.';
    end;

  DisposeObject([Param, ResultValues]);
  if Result then
      report := 'solve.';
end;

procedure TTrainingTask.ExportLastWriteToStream(stream: TMemoryStream64);
var
  dest_db: TObjectDataManager;
  i: Integer;
  m64: TMemoryStream64;
begin
  dest_db := TObjectDataManager.CreateAsStream(stream, '', DBMarshal.ID, False, True, False);

  for i := 0 to LastWriteFileList.Count - 1 do
    begin
      m64 := TMemoryStream64.CustomCreate($FFFF);
      DB_Engine.ItemReadToStream('/', LastWriteFileList[i], m64);
      m64.Position := 0;
      dest_db.ItemWriteFromStream('/', LastWriteFileList[i], m64);
      DisposeObject(m64);
    end;

  DisposeObject(dest_db);
end;

procedure TTrainingTask.ExportLastWriteToFile(filename: SystemString);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.CustomCreate($FFFF);
  ExportLastWriteToStream(m64);
  m64.SaveToFile(filename);
  DisposeObject(m64);
end;

end.
