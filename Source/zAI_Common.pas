{ ****************************************************************************** }
{ * AI Common define                                                           * }
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
unit zAI_Common;

{$INCLUDE zDefine.inc}

interface

uses {$IFNDEF FPC} System.IOUtils, {$ENDIF FPC}CoreClasses, PascalStrings, UnicodeMixedLib, TextDataEngine, DoStatusIO;

type
  TAI_Key = array [0 .. 39] of Byte;

var
  AI_Configure_Path: U_String;

  AI_CFG_FILE: SystemString;

  AI_ProductID: SystemString;
  AI_UserKey: SystemString;
  AI_Key_Server_Host: SystemString;
  AI_Key_Server_Port: Word;

  AI_Engine_Library: SystemString;
  AI_Parallel_Count: Integer;

  AI_TrainingTool: SystemString;
  AI_PackageTool: SystemString;
  AI_ModelTool: SystemString;
  AI_TrainingServer: SystemString;

procedure ReadAIConfig;
procedure WriteAIConfig;

implementation


procedure Init_AI_Common;
begin
  if IsMobile then
      AI_CFG_FILE := 'AI.conf'
  else
      AI_CFG_FILE := umlCombineFileName(AI_Configure_Path, 'AI.conf');

  AI_ProductID := '';
  AI_UserKey := '';
  AI_Key_Server_Host := 'zpascal.net';
  AI_Key_Server_Port := 7988;

  AI_Engine_Library := 'zAI.dll';
  AI_Parallel_Count := CpuCount;

  AI_TrainingTool := '';
  AI_PackageTool := '';
  AI_ModelTool := '';
  AI_TrainingServer := '127.0.0.1';
end;

procedure ReadAIConfig;
var
  ini: THashTextEngine;

  function r_ai(name, fn: U_String): U_String;
  begin
    Result := ini.GetDefaultValue('AI', Name, fn);
    if not umlExistsChar(Result, '/\') then
        Result := umlCombineFileName(AI_Configure_Path, Result);
  end;

begin
  if not umlFileExists(AI_CFG_FILE) then
      exit;

  ini := THashTextEngine.Create;
  ini.LoadFromFile(AI_CFG_FILE);

  AI_ProductID := ini.GetDefaultValue('Auth', 'ProductID', AI_ProductID);
  AI_UserKey := ini.GetDefaultValue('Auth', 'Key', AI_UserKey);
  AI_Key_Server_Host := ini.GetDefaultValue('Auth', 'Server', AI_Key_Server_Host);
  AI_Key_Server_Port := ini.GetDefaultValue('Auth', 'Port', AI_Key_Server_Port);

  AI_Engine_Library := r_ai('Engine', AI_Engine_Library);
  AI_TrainingTool := r_ai('TrainingTool', AI_TrainingTool);
  AI_PackageTool := r_ai('PackageTool', AI_PackageTool);
  AI_ModelTool := r_ai('ModelTool', AI_ModelTool);

  AI_Parallel_Count := ini.GetDefaultValue('AI', 'Parallel', AI_Parallel_Count);
  AI_TrainingServer := ini.GetDefaultValue('AI', 'TrainingServer', AI_TrainingServer);

  disposeObject(ini);
  DoStatus('read config "%s"', [AI_CFG_FILE]);
end;

procedure WriteAIConfig;
var
  ini: THashTextEngine;
  procedure w_ai(name, fn: U_String);
  begin
    if fn.Same(umlCombineFileName(AI_Configure_Path, umlGetFileName(fn))) then
        ini.SetDefaultValue('AI', Name, umlGetFileName(fn))
    else
        ini.SetDefaultValue('AI', Name, fn);
  end;

begin
  ini := THashTextEngine.Create;

  ini.SetDefaultValue('Auth', 'ProductID', AI_ProductID);
  ini.SetDefaultValue('Auth', 'Key', AI_UserKey);
  ini.SetDefaultValue('Auth', 'Server', AI_Key_Server_Host);
  ini.SetDefaultValue('Auth', 'Port', AI_Key_Server_Port);

  w_ai('Engine', AI_Engine_Library);
  w_ai('TrainingTool', AI_TrainingTool);
  w_ai('PackageTool', AI_PackageTool);
  w_ai('ModelTool', AI_ModelTool);

  ini.SetDefaultValue('AI', 'Parallel', AI_Parallel_Count);
  ini.SetDefaultValue('AI', 'TrainingServer', AI_TrainingServer);

  try
    ini.SaveToFile(AI_CFG_FILE);
    DoStatus('write config "%s"', [AI_CFG_FILE]);
  except
    TCoreClassThread.Sleep(100);
    WriteAIConfig;
  end;
  disposeObject(ini);

end;

initialization


{$IFDEF FPC}
  AI_Configure_Path := umlCurrentPath;
{$ELSE FPC}
  AI_Configure_Path := System.IOUtils.TPath.GetLibraryPath;
{$ENDIF FPC}

Init_AI_Common;

finalization

end.
