{ ****************************************************************************** }
{ * AI Training task Support(platform compatible)                              * }
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
  PascalStrings, MemoryStream64, UnicodeMixedLib, DataFrameEngine,
  ObjectDataManager, ObjectData, ItemStream,
  DoStatusIO, MemoryRaster, ListEngine, zAI_Common;

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

    // export training result
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
