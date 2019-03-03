{ ****************************************************************************** }
{ * AI Common (platform compatible)                                            * }
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

uses Types,
  CoreClasses,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ELSE FPC}
  System.IOUtils,
{$ENDIF FPC}
  PascalStrings, MemoryStream64, UnicodeMixedLib, DataFrameEngine, ListEngine, TextDataEngine,
  ZDBEngine, ZDBLocalManager, ObjectDataManager, ObjectData, ItemStream,
  zDrawEngine, Geometry2DUnit, MemoryRaster, TextParsing, zExpression, OpCode;

type
  TAI_DetectorDefine = class;
  TAI_Image = class;
  TAI_ImageList = class;

{$IFDEF FPC}
  TImageList_Decl = specialize TGenericsList<TAI_Image>;
  TDetectorDefineList = specialize TGenericsList<TAI_DetectorDefine>;
{$ELSE FPC}
  TImageList_Decl = TGenericsObjectList<TAI_Image>;
  TDetectorDefineList = TGenericsObjectList<TAI_DetectorDefine>;
{$ENDIF FPC}

  TAI_DetectorDefine = class(TCoreClassObject)
  private
    FOP_RT_RunDeleted: Boolean;
  public
    Owner: TAI_Image;
    R: TRect;
    Token: TPascalString;
    Part: TVec2List;
    PrepareRaster: TMemoryRaster;

    constructor Create(AOwner: TAI_Image);
    destructor Destroy; override;

    procedure SaveToStream(stream: TMemoryStream64; RasterSave_: TRasterSave); overload;
    procedure SaveToStream(stream: TMemoryStream64); overload;
    procedure LoadFromStream(stream: TMemoryStream64);
  end;

  TAI_Image_Script_RegisterProc = procedure(Sender: TAI_Image; opRT: TOpCustomRunTime) of object;

  TAI_Image = class(TCoreClassObject)
  private
    FOP_RT: TOpCustomRunTime;
    FOP_RT_RunDeleted: Boolean;
    // register op
    procedure CheckAndRegOPRT;
    // condition on image
    function OP_Image_GetWidth(var Param: TOpParam): Variant;
    function OP_Image_GetHeight(var Param: TOpParam): Variant;
    function OP_Image_GetDetector(var Param: TOpParam): Variant;
    // condition on detector
    function OP_Detector_GetLabel(var Param: TOpParam): Variant;
    // process on image
    function OP_Image_Delete(var Param: TOpParam): Variant;
    function OP_Image_Scale(var Param: TOpParam): Variant;
    function OP_Image_SwapRB(var Param: TOpParam): Variant;
    function OP_Image_Gray(var Param: TOpParam): Variant;
    function OP_Image_Sharpen(var Param: TOpParam): Variant;
    function OP_Image_HistogramEqualize(var Param: TOpParam): Variant;
    function OP_Image_RemoveRedEyes(var Param: TOpParam): Variant;
    function OP_Image_Sepia(var Param: TOpParam): Variant;
    function OP_Image_CalibrateRotate(var Param: TOpParam): Variant;
    // process on detector
    function OP_Detector_SetLabel(var Param: TOpParam): Variant;
    function OP_Detector_ClearDetector(var Param: TOpParam): Variant;
    function OP_Detector_DeleteDetector(var Param: TOpParam): Variant;
  public
    Owner: TAI_ImageList;
    DetectorDefineList: TDetectorDefineList;
    Raster: TMemoryRaster;

    constructor Create(AOwner: TAI_ImageList);
    destructor Destroy; override;

    function RunExpCondition(ScriptStyle: TTextStyle; exp: SystemString): Boolean;
    function RunExpProcess(ScriptStyle: TTextStyle; exp: SystemString): Boolean;
    function GetExpFunctionList: TPascalStringList;

    procedure Clear;
    procedure ClearPrepareRaster;

    procedure DrawTo(output: TMemoryRaster);

    function FoundNoTokenDetectorDefine(output: TMemoryRaster; color: TDEColor): Boolean; overload;
    function FoundNoTokenDetectorDefine: Boolean; overload;

    procedure SaveToStream(stream: TMemoryStream64; SaveImg: Boolean; RasterSave_: TRasterSave); overload;
    procedure SaveToStream(stream: TMemoryStream64); overload;

    procedure LoadFromStream(stream: TMemoryStream64; LoadImg: Boolean); overload;
    procedure LoadFromStream(stream: TMemoryStream64); overload;

    procedure LoadPicture(stream: TMemoryStream64); overload;
    procedure LoadPicture(fileName: SystemString); overload;

    procedure Scale(f: TGeoFloat);

    function ExistsToken(Token: TPascalString): Boolean;
    function GetTokenCount(Token: TPascalString): Integer;
  end;

  TAI_ImageList = class(TImageList_Decl)
  public
    UsedJpegForXML: Boolean;
    FileInfo: TPascalString;
    UserData: TCoreClassObject;

    constructor Create;
    destructor Destroy; override;

    function Clone: TAI_ImageList;

    procedure Delete(index: Integer);

    procedure Clear;
    procedure ClearPrepareRaster;

    procedure RunScript(ScriptStyle: TTextStyle; condition_exp, process_exp: SystemString); overload;
    procedure RunScript(condition_exp, process_exp: SystemString); overload;

    procedure DrawTo(output: TMemoryRaster); overload;
    procedure DrawTo(d: TDrawEngine; Margins: TGeoFloat; destOffset: TDEVec; alpha: TDEFloat); overload;

    procedure AddPicture(stream: TCoreClassStream); overload;
    procedure AddPicture(fileName: SystemString); overload;
    procedure AddPicture(R: TMemoryRaster); overload;
    procedure AddPicture(mr: TMemoryRaster; R: TRect); overload;
    procedure AddPicture(mr: TMemoryRaster; R: TRectV2); overload;

    procedure LoadFromPictureStream(stream: TCoreClassStream);
    procedure LoadFromPictureFile(fileName: SystemString);

    function PackingRaster: TMemoryRaster;
    procedure SaveToPictureStream(stream: TCoreClassStream);
    procedure SaveToPictureFile(fileName: SystemString);

    procedure SavePrepareRasterToPictureStream(stream: TCoreClassStream);
    procedure SavePrepareRasterToPictureFile(fileName: SystemString);

    procedure SaveToStream(stream: TCoreClassStream); overload;
    procedure SaveToStream(stream: TCoreClassStream; SaveImg, Compressed: Boolean); overload;
    procedure SaveToStream(stream: TCoreClassStream; SaveImg, Compressed: Boolean; RasterSave_: TRasterSave); overload;
    procedure LoadFromStream(stream: TCoreClassStream; LoadImg: Boolean); overload;
    procedure LoadFromStream(stream: TCoreClassStream); overload;

    procedure SaveToFile(fileName: SystemString); overload;
    procedure SaveToFile(fileName: SystemString; SaveImg, Compressed: Boolean; RasterSave_: TRasterSave); overload;
    procedure LoadFromFile(fileName: SystemString; LoadImg: Boolean); overload;
    procedure LoadFromFile(fileName: SystemString); overload;

    procedure Import(imgList: TAI_ImageList);

    procedure CalibrationNullDetectorDefineToken(Token: SystemString);
    procedure CalibrationDetectorDefineTokenPrefix(Prefix: SystemString);
    procedure CalibrationDetectorDefineTokenSuffix(Suffix: SystemString);

    procedure Scale(f: TGeoFloat);

    procedure Export_PrepareRaster(outputPath: SystemString);
    procedure Export_DetectorRaster(outputPath: SystemString);
    procedure Build_XML(TokenFilter: SystemString; includeLabel, includePart, usedJpeg: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList); overload;
    procedure Build_XML(TokenFilter: SystemString; includeLabel, includePart: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList); overload;
    procedure Build_XML(includeLabel, includePart: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList); overload;
    procedure Build_XML(includeLabel, includePart: Boolean; datasetName, comment, build_output_file: SystemString); overload;

    function DetectorDefineCount: Integer;
    function DetectorDefinePartCount: Integer;

    function ExtractDetectorDefineAsSnapshot: TMemoryRaster2DArray;
    function ExtractDetectorDefineAsPrepareRaster(SS_width, SS_height: Integer): TMemoryRaster2DArray;
    function ExtractDetectorDefineAsScaleSpace(SS_width, SS_height: Integer): TMemoryRaster2DArray;

    function FoundNoTokenDetectorDefine(output: TMemoryRaster): Boolean; overload;
    function FoundNoTokenDetectorDefine: Boolean; overload;

    function Tokens: TArrayPascalString;
    function ExistsToken(Token: TPascalString): Boolean;
    function GetTokenCount(Token: TPascalString): Integer;
  end;

{$IFDEF FPC}

  TAI_ImageMatrix_Decl = specialize TGenericsList<TAI_ImageList>;
{$ELSE FPC}
  TAI_ImageMatrix_Decl = TGenericsObjectList<TAI_ImageList>;
{$ENDIF FPC}

  TAI_ImageMatrix = class(TAI_ImageMatrix_Decl)
  public
    UsedJpegForXML: Boolean;
    constructor Create;
    destructor Destroy; override;

    procedure RunScript(ScriptStyle: TTextStyle; condition_exp, process_exp: SystemString); overload;
    procedure RunScript(condition_exp, process_exp: SystemString); overload;

    function FindImageList(FileInfo: TPascalString): TAI_ImageList;

    procedure SaveToStream(stream: TCoreClassStream; SaveImg: Boolean; RasterSave_: TRasterSave); overload;
    procedure SaveToStream(stream: TCoreClassStream); overload;
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure SaveToFile(fileName: SystemString; SaveImg: Boolean; RasterSave_: TRasterSave); overload;
    procedure SaveToFile(fileName: SystemString); overload;
    procedure LoadFromFile(fileName: SystemString);

    procedure ClearPrepareRaster;

    procedure SearchAndAddImageList(rootPath, filter: SystemString; includeSubdir, LoadImg: Boolean);

    procedure Scale(f: TGeoFloat);

    procedure Export_PrepareRaster(outputPath: SystemString);
    procedure Export_DetectorRaster(outputPath: SystemString);

    procedure Build_XML(TokenFilter: SystemString; includeLabel, includePart, usedJpeg: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList); overload;
    procedure Build_XML(TokenFilter: SystemString; includeLabel, includePart: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList); overload;
    procedure Build_XML(includeLabel, includePart: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList); overload;
    procedure Build_XML(includeLabel, includePart: Boolean; datasetName, comment, build_output_file: SystemString); overload;

    function ImageCount: Integer;
    function DetectorDefineCount: Integer;
    function DetectorDefinePartCount: Integer;

    function ExtractDetectorDefineAsSnapshot: TMemoryRaster2DArray;
    function ExtractDetectorDefineAsPrepareRaster(SS_width, SS_height: Integer): TMemoryRaster2DArray;
    function ExtractDetectorDefineAsScaleSpace(SS_width, SS_height: Integer): TMemoryRaster2DArray;

    function FoundNoTokenDetectorDefine(output: TMemoryRaster): Boolean; overload;
    function FoundNoTokenDetectorDefine: Boolean; overload;

    function Tokens: TArrayPascalString;
    function ExistsToken(Token: TPascalString): Boolean;
    function GetTokenCount(Token: TPascalString): Integer;
  end;

var
  AI_Configure_Path: U_String;
  AI_CFG_FILE: U_String;

  AI_ProductID: U_String;
  AI_UserKey: U_String;
  AI_Key_Server_Host: U_String;
  AI_Key_Server_Port: Word;

  AI_Engine_Library: U_String;
  AI_Parallel_Count: Integer;

  AI_TrainingTool: U_String;
  AI_PackageTool: U_String;
  AI_ModelTool: U_String;
  AI_TrainingServer: U_String;

  On_Script_RegisterProc: TAI_Image_Script_RegisterProc;

const
  // ext define
  C_ImageMatrix_Ext: SystemString = '.imgMat';
  C_ImageList_Ext: SystemString = '.imgDataset';
  C_Image_Ext: SystemString = '.img';
  C_OD_Ext: SystemString = '.svm_od';
  C_OD_Marshal_Ext: SystemString = '.svm_od_marshal';
  C_SP_Ext: SystemString = '.shape';
  C_Metric_ResNet_Ext: SystemString = '.metric';
  C_LMetric_ResNet_Ext: SystemString = '.large_metric';
  C_Learn_Ext: SystemString = '.learn';
  C_MMOD_Ext: SystemString = '.svm_dnn_od';
  C_RNIC_Ext: SystemString = '.rnic';

procedure ReadAIConfig; overload;
procedure ReadAIConfig(ini: THashTextEngine); overload;
procedure WriteAIConfig;

procedure Build_XML_Dataset(xslFile, name, comment, body: SystemString; build_output: TMemoryStream64);
procedure Build_XML_Style(build_output: TMemoryStream64);
procedure DrawSPLine(sp_desc: TVec2List; bp, ep: Integer; closeLine: Boolean; color: TDEColor; d: TDrawEngine); overload;
procedure DrawSPLine(sp_desc: TArrayVec2; bp, ep: Integer; closeLine: Boolean; color: TDEColor; d: TDrawEngine); overload;
procedure DrawFaceSP(sp_desc: TVec2List; color: TDEColor; d: TDrawEngine); overload;
procedure DrawFaceSP(sp_desc: TArrayVec2; color: TDEColor; d: TDrawEngine); overload;

implementation

uses
{$IFDEF parallel}
{$IFDEF FPC}
  mtprocs,
{$ELSE FPC}
  Threading,
{$ENDIF FPC}
{$ENDIF parallel}
  SyncObjs, DoStatusIO, Math;

procedure Init_AI_Common;
begin
{$IFDEF FPC}
  AI_Configure_Path := umlCurrentPath;
{$ELSE FPC}
  AI_Configure_Path := System.IOUtils.TPath.GetLibraryPath;
{$ENDIF FPC}
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
  AI_TrainingServer := 'localhost';

  On_Script_RegisterProc := nil;
end;

procedure ReadAIConfig;
var
  ini: THashTextEngine;
begin
  if not umlFileExists(AI_CFG_FILE) then
    begin
      DoStatus('not found config file "%s"', [AI_CFG_FILE.Text]);
      exit;
    end;

  ini := THashTextEngine.Create;
  ini.LoadFromFile(AI_CFG_FILE);

  ReadAIConfig(ini);

  disposeObject(ini);
  DoStatus('read config "%s"', [AI_CFG_FILE.Text]);
end;

procedure ReadAIConfig(ini: THashTextEngine);
  function r_ai(name, fn: U_String): U_String;
  begin
    Result := ini.GetDefaultValue('AI', Name, fn);
    if not umlExistsChar(Result, '/\') then
        Result := umlCombineFileName(AI_Configure_Path, Result);
  end;

begin
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
    DoStatus('write config "%s"', [AI_CFG_FILE.Text]);
  except
    TCoreClassThread.Sleep(100);
    WriteAIConfig;
  end;
  disposeObject(ini);

end;

procedure Build_XML_Dataset(xslFile, name, comment, body: SystemString; build_output: TMemoryStream64);
const
  XML_Dataset =
    '<?xml version='#39'1.0'#39' encoding='#39'UTF-8'#39'?>'#13#10 +
    '<?xml-stylesheet type='#39'text/xsl'#39' href='#39'%xsl%'#39'?>'#13#10 +
    '<dataset>'#13#10 +
    '<name>%name%</name>'#13#10 +
    '<comment>%comment%</comment>'#13#10 +
    '<images>'#13#10 +
    '%body%'#13#10 +
    '</images>'#13#10 +
    '</dataset>'#13#10;

var
  vt: THashStringList;
  s_out: SystemString;
  l: TPascalStringList;
begin
  vt := THashStringList.Create;
  vt['xsl'] := xslFile;
  vt['name'] := name;
  vt['comment'] := comment;
  vt['body'] := body;
  vt.ProcessMacro(XML_Dataset, '%', '%', s_out);
  disposeObject(vt);
  l := TPascalStringList.Create;
  l.Text := s_out;
  l.SaveToStream(build_output);
  disposeObject(l);
end;

procedure Build_XML_Style(build_output: TMemoryStream64);
const
  XML_Style = '<?xml version="1.0" encoding="UTF-8" ?>'#13#10 +
    '<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">'#13#10 +
    '<xsl:output method='#39'html'#39' version='#39'1.0'#39' encoding='#39'UTF-8'#39' indent='#39'yes'#39' />'#13#10 +
    '<xsl:variable name="max_images_displayed">30</xsl:variable>'#13#10 +
    '   <xsl:template match="/dataset">'#13#10 +
    '      <html>'#13#10 +
    '         <head>'#13#10 +
    '            '#13#10 +
    '            <style type="text/css">'#13#10 +
    '               div#box{'#13#10 +
    '                  position: absolute; '#13#10 +
    '                  border-style:solid; '#13#10 +
    '                  border-width:3px; '#13#10 +
    '                  border-color:red;'#13#10 +
    '               }'#13#10 +
    '               div#circle{'#13#10 +
    '                  position: absolute; '#13#10 +
    '                  border-style:solid; '#13#10 +
    '                  border-width:1px; '#13#10 +
    '                  border-color:red;'#13#10 +
    '                  border-radius:7px;'#13#10 +
    '                  width:2px; '#13#10 +
    '                  height:2px;'#13#10 +
    '               }'#13#10 +
    '               div#label{'#13#10 +
    '                  position: absolute; '#13#10 +
    '                  color: red;'#13#10 +
    '               }'#13#10 +
    '               div#img{'#13#10 +
    '                  position: relative;'#13#10 +
    '                  margin-bottom:2em;'#13#10 +
    '               }'#13#10 +
    '               pre {'#13#10 +
    '                  color: black;'#13#10 +
    '                  margin: 1em 0.25in;'#13#10 +
    '                  padding: 0.5em;'#13#10 +
    '                  background: rgb(240,240,240);'#13#10 +
    '                  border-top: black dotted 1px;'#13#10 +
    '                  border-left: black dotted 1px;'#13#10 +
    '                  border-right: black solid 2px;'#13#10 +
    '                  border-bottom: black solid 2px;'#13#10 +
    '               }'#13#10 +
    '            </style>'#13#10 +
    '         </head>'#13#10 +
    '         <body>'#13#10 +
    '            ZAI Dataset name: <b><xsl:value-of select='#39'/dataset/name'#39'/></b> <br/>'#13#10 +
    '            ZAI comment: <b><xsl:value-of select='#39'/dataset/comment'#39'/></b> <br/> '#13#10 +
    '            include <xsl:value-of select="count(images/image)"/> of picture and <xsl:value-of select="count(images/image/box)"/> detector <br/>'#13#10 +
    '            <xsl:if test="count(images/image) &gt; $max_images_displayed">'#13#10 +
    '               <h2>max display <xsl:value-of select="$max_images_displayed"/> of picture.</h2>'#13#10 +
    '               <hr/>'#13#10 +
    '            </xsl:if>'#13#10 +
    '            <xsl:for-each select="images/image">'#13#10 +
    '               <xsl:if test="position() &lt;= $max_images_displayed">'#13#10 +
    '                  detector: <xsl:value-of select="count(box)"/>'#13#10 +
    '                  <div id="img">'#13#10 +
    '                     <img src="{@file}"/>'#13#10 +
    '                     <xsl:for-each select="box">'#13#10 +
    '                        <div id="box" style="top: {@top}px; left: {@left}px; width: {@width}px; height: {@height}px;"></div>'#13#10 +
    '                        <xsl:if test="label">'#13#10 +
    '                           <div id="label" style="top: {@top+@height}px; left: {@left+@width}px;">'#13#10 +
    '                              <xsl:value-of select="label"/>'#13#10 +
    '                           </div>'#13#10 +
    '                        </xsl:if>'#13#10 +
    '                        <xsl:for-each select="part">'#13#10 +
    '                           <div id="circle" style="top: {(@y)}px; left: {(@x)}px; "></div>'#13#10 +
    '                        </xsl:for-each>'#13#10 +
    '                     </xsl:for-each>'#13#10 +
    '                  </div>'#13#10 +
    '               </xsl:if>'#13#10 +
    '            </xsl:for-each>'#13#10 +
    '         </body>'#13#10 +
    '      </html>'#13#10 +
    '   </xsl:template>'#13#10 +
    '</xsl:stylesheet>'#13#10;

var
  l: TPascalStringList;
begin
  l := TPascalStringList.Create;
  l.Text := XML_Style;
  l.SaveToStream(build_output);
  disposeObject(l);
end;

procedure DrawSPLine(sp_desc: TVec2List; bp, ep: Integer; closeLine: Boolean; color: TDEColor; d: TDrawEngine);
var
  i: Integer;
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  for i := bp to ep do
      vl.Add(sp_desc[i]^);

  d.DrawPL(20, vl, closeLine, color, 2);
  disposeObject(vl);
end;

procedure DrawSPLine(sp_desc: TArrayVec2; bp, ep: Integer; closeLine: Boolean; color: TDEColor; d: TDrawEngine);
var
  i: Integer;
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  for i := bp to ep do
      vl.Add(sp_desc[i]);

  d.DrawPL(20, vl, closeLine, color, 2);
  disposeObject(vl);
end;

procedure DrawFaceSP(sp_desc: TVec2List; color: TDEColor; d: TDrawEngine);
begin
  if sp_desc.Count <> 68 then
      exit;
  DrawSPLine(sp_desc, 0, 16, False, color, d);
  DrawSPLine(sp_desc, 17, 21, False, color, d);
  DrawSPLine(sp_desc, 22, 26, False, color, d);
  DrawSPLine(sp_desc, 27, 30, False, color, d);
  DrawSPLine(sp_desc, 31, 35, False, color, d);
  d.DrawLine(sp_desc[31]^, sp_desc[27]^, color, 1);
  d.DrawLine(sp_desc[35]^, sp_desc[27]^, color, 1);
  d.DrawLine(sp_desc[31]^, sp_desc[30]^, color, 1);
  d.DrawLine(sp_desc[35]^, sp_desc[30]^, color, 1);
  DrawSPLine(sp_desc, 36, 41, True, color, d);
  DrawSPLine(sp_desc, 42, 47, True, color, d);
  DrawSPLine(sp_desc, 48, 59, True, color, d);
  DrawSPLine(sp_desc, 60, 67, True, color, d);
end;

procedure DrawFaceSP(sp_desc: TArrayVec2; color: TDEColor; d: TDrawEngine);
begin
  if length(sp_desc) <> 68 then
      exit;
  DrawSPLine(sp_desc, 0, 16, False, color, d);
  DrawSPLine(sp_desc, 17, 21, False, color, d);
  DrawSPLine(sp_desc, 22, 26, False, color, d);
  DrawSPLine(sp_desc, 27, 30, False, color, d);
  DrawSPLine(sp_desc, 31, 35, False, color, d);
  d.DrawLine(sp_desc[31], sp_desc[27], color, 1);
  d.DrawLine(sp_desc[35], sp_desc[27], color, 1);
  d.DrawLine(sp_desc[31], sp_desc[30], color, 1);
  d.DrawLine(sp_desc[35], sp_desc[30], color, 1);
  DrawSPLine(sp_desc, 36, 41, True, color, d);
  DrawSPLine(sp_desc, 42, 47, True, color, d);
  DrawSPLine(sp_desc, 48, 59, True, color, d);
  DrawSPLine(sp_desc, 60, 67, True, color, d);
end;

constructor TAI_DetectorDefine.Create(AOwner: TAI_Image);
begin
  inherited Create;
  Owner := AOwner;
  R.Left := 0;
  R.Top := 0;
  R.Right := 0;
  R.Bottom := 0;
  Token := '';
  Part := TVec2List.Create;
  PrepareRaster := NewRaster();
end;

destructor TAI_DetectorDefine.Destroy;
begin
  disposeObject(PrepareRaster);
  disposeObject(Part);
  inherited Destroy;
end;

procedure TAI_DetectorDefine.SaveToStream(stream: TMemoryStream64; RasterSave_: TRasterSave);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
begin
  de := TDataFrameEngine.Create;
  de.WriteRect(R);
  de.WriteString(Token);

  m64 := TMemoryStream64.Create;
  Part.SaveToStream(m64);
  de.WriteStream(m64);
  disposeObject(m64);

  m64 := TMemoryStream64.CustomCreate(8192);
  if not PrepareRaster.Empty then
      PrepareRaster.SaveToStream(m64, RasterSave_);
  de.WriteStream(m64);
  disposeObject(m64);

  de.EncodeTo(stream, True);

  disposeObject(de);
end;

procedure TAI_DetectorDefine.SaveToStream(stream: TMemoryStream64);
begin
  SaveToStream(stream, TRasterSave.rsRGB);
end;

procedure TAI_DetectorDefine.LoadFromStream(stream: TMemoryStream64);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
begin
  de := TDataFrameEngine.Create;
  de.DecodeFrom(stream);
  R := de.Reader.ReadRect;
  Token := de.Reader.ReadString;

  m64 := TMemoryStream64.CustomCreate(8192);
  de.Reader.ReadStream(m64);
  m64.Position := 0;
  Part.LoadFromStream(m64);
  disposeObject(m64);

  m64 := TMemoryStream64.CustomCreate(8192);
  de.Reader.ReadStream(m64);
  if m64.Size > 0 then
    begin
      m64.Position := 0;
      PrepareRaster.LoadFromStream(m64);
    end;
  disposeObject(m64);

  disposeObject(de);
end;

procedure TAI_Image.CheckAndRegOPRT;
begin
  if FOP_RT <> nil then
      exit;
  FOP_RT := TOpCustomRunTime.Create;
  FOP_RT.UserObject := Self;

  // condition on image
  FOP_RT.RegOpM('Width', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetWidth);
  FOP_RT.RegOpM('Height', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetHeight);
  FOP_RT.RegOpM('Det', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetDetector);
  FOP_RT.RegOpM('Detector', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetDetector);
  FOP_RT.RegOpM('DetNum', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_GetDetector);

  // condition on detector
  FOP_RT.RegOpM('Label', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_GetLabel);
  FOP_RT.RegOpM('GetLabel', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_GetLabel);

  // process on image
  FOP_RT.RegOpM('Delete', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Delete);

  FOP_RT.RegOpM('Scale', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Scale);
  FOP_RT.RegOpM('ReductMemory', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Scale);

  FOP_RT.RegOpM('SwapRB', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_SwapRB);
  FOP_RT.RegOpM('SwapBR', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_SwapRB);

  FOP_RT.RegOpM('Gray', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Gray);
  FOP_RT.RegOpM('Grayscale', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Gray);

  FOP_RT.RegOpM('Sharpen', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Sharpen);

  FOP_RT.RegOpM('HistogramEqualize', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_HistogramEqualize);
  FOP_RT.RegOpM('he', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_HistogramEqualize);
  FOP_RT.RegOpM('NiceColor', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_HistogramEqualize);

  FOP_RT.RegOpM('RemoveRedEye', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_RemoveRedEyes);
  FOP_RT.RegOpM('RemoveRedEyes', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_RemoveRedEyes);
  FOP_RT.RegOpM('RedEyes', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_RemoveRedEyes);
  FOP_RT.RegOpM('RedEye', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_RemoveRedEyes);

  FOP_RT.RegOpM('Sepia', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Sepia);

  FOP_RT.RegOpM('CalibrateRotate', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_CalibrateRotate);
  FOP_RT.RegOpM('DocumentAlignment', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_CalibrateRotate);
  FOP_RT.RegOpM('DocumentAlign', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_CalibrateRotate);
  FOP_RT.RegOpM('DocAlign', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_CalibrateRotate);
  FOP_RT.RegOpM('AlignDoc', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_CalibrateRotate);

  // process on detector
  FOP_RT.RegOpM('SetLab', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_SetLabel);
  FOP_RT.RegOpM('SetLabel', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_SetLabel);
  FOP_RT.RegOpM('DefLab', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_SetLabel);
  FOP_RT.RegOpM('DefLabel', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_SetLabel);
  FOP_RT.RegOpM('DefineLabel', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_SetLabel);
  FOP_RT.RegOpM('ClearDetector', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_ClearDetector);
  FOP_RT.RegOpM('ClearDet', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_ClearDetector);
  FOP_RT.RegOpM('KillDetector', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_ClearDetector);
  FOP_RT.RegOpM('KillDet', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_ClearDetector);
  FOP_RT.RegOpM('DeleteDetector', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_DeleteDetector);
  FOP_RT.RegOpM('DeleteRect', {$IFDEF FPC}@{$ENDIF FPC}OP_Detector_DeleteDetector);

  // external image processor
  if Assigned(On_Script_RegisterProc) then
      On_Script_RegisterProc(Self, FOP_RT);
end;

function TAI_Image.OP_Image_GetWidth(var Param: TOpParam): Variant;
begin
  Result := Raster.width;
end;

function TAI_Image.OP_Image_GetHeight(var Param: TOpParam): Variant;
begin
  Result := Raster.height;
end;

function TAI_Image.OP_Image_GetDetector(var Param: TOpParam): Variant;
begin
  Result := DetectorDefineList.Count;
end;

function TAI_Image.OP_Detector_GetLabel(var Param: TOpParam): Variant;
begin
  Result := GetTokenCount(Param[0]);
end;

function TAI_Image.OP_Image_Delete(var Param: TOpParam): Variant;
begin
  FOP_RT_RunDeleted := True;
  Result := True;
end;

function TAI_Image.OP_Image_Scale(var Param: TOpParam): Variant;
begin
  DoStatus('image script on scale %f', [TGeoFloat(Param[0])]);
  if not Raster.Empty then
    begin
      Scale(Param[0]);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseFMXResource;
    end;
  Result := True;
end;

function TAI_Image.OP_Image_SwapRB(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on SwapRed-Blue');

  if not Raster.Empty then
    begin
      Raster.FormatBGRA;
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseFMXResource;
    end;

  for i := 0 to DetectorDefineList.Count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        DetectorDefineList[i].PrepareRaster.FormatBGRA;
  Result := True;
end;

function TAI_Image.OP_Image_Gray(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on Grayscale');

  if not Raster.Empty then
    begin
      Raster.Grayscale;
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseFMXResource;
    end;

  for i := 0 to DetectorDefineList.Count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        DetectorDefineList[i].PrepareRaster.Grayscale;
  Result := True;
end;

function TAI_Image.OP_Image_Sharpen(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on Sharpen');

  if not Raster.Empty then
    begin
      Sharpen(Raster, True);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseFMXResource;
    end;

  for i := 0 to DetectorDefineList.Count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        Sharpen(DetectorDefineList[i].PrepareRaster, True);
  Result := True;
end;

function TAI_Image.OP_Image_HistogramEqualize(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on HistogramEqualize');

  if not Raster.Empty then
    begin
      HistogramEqualize(Raster);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseFMXResource;
    end;

  for i := 0 to DetectorDefineList.Count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        HistogramEqualize(DetectorDefineList[i].PrepareRaster);
  Result := True;
end;

function TAI_Image.OP_Image_RemoveRedEyes(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on RemoveRedEyes');

  if not Raster.Empty then
    begin
      RemoveRedEyes(Raster);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseFMXResource;
    end;

  for i := 0 to DetectorDefineList.Count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        RemoveRedEyes(DetectorDefineList[i].PrepareRaster);
  Result := True;
end;

function TAI_Image.OP_Image_Sepia(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on Sepia');

  if not Raster.Empty then
    begin
      Sepia32(Raster, Param[0]);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseFMXResource;
    end;

  for i := 0 to DetectorDefineList.Count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        Sepia32(DetectorDefineList[i].PrepareRaster, Param[0]);
  Result := True;
end;

function TAI_Image.OP_Image_CalibrateRotate(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on CalibrateRotate');

  if not Raster.Empty then
    begin
      Raster.CalibrateRotate;
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseFMXResource;
    end;

  for i := 0 to DetectorDefineList.Count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        DetectorDefineList[i].PrepareRaster.CalibrateRotate;
  Result := True;
end;

function TAI_Image.OP_Detector_SetLabel(var Param: TOpParam): Variant;
var
  i: Integer;
  n: SystemString;
begin
  if length(Param) > 0 then
      n := Param[0]
  else
      n := '';
  DoStatus('image script on setLabel %s', [n]);
  for i := 0 to DetectorDefineList.Count - 1 do
      DetectorDefineList[i].Token := n;
  Result := True;
end;

function TAI_Image.OP_Detector_ClearDetector(var Param: TOpParam): Variant;
begin
  Clear;
  Result := True;
end;

function TAI_Image.OP_Detector_DeleteDetector(var Param: TOpParam): Variant;
type
  TDetArry = array of TAI_DetectorDefine;
var
  coord: TVec2;

  function ListSortCompare(Item1, Item2: TAI_DetectorDefine): TValueRelationship;
  var
    d1, d2: TGeoFloat;
  begin
    d1 := Vec2Distance(RectCentre(RectV2(Item1.R)), coord);
    d2 := Vec2Distance(RectCentre(RectV2(Item2.R)), coord);
    Result := CompareValue(d1, d2);
  end;

  procedure QuickSortList(var SortList: TDetArry; l, R: Integer);
  var
    i, j: Integer;
    p, t: TAI_DetectorDefine;
  begin
    repeat
      i := l;
      j := R;
      p := SortList[(l + R) shr 1];
      repeat
        while ListSortCompare(SortList[i], p) < 0 do
            inc(i);
        while ListSortCompare(SortList[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if l < j then
          QuickSortList(SortList, l, j);
      l := i;
    until i >= R;
  end;

var
  pt: TVec2;
  reversed_count: Integer;
  detArry: TDetArry;
  i: Integer;
  det: TAI_DetectorDefine;
begin
  if DetectorDefineList.Count < 2 then
    begin
      Result := False;
      exit;
    end;

  if length(Param) <> 3 then
    begin
      DoStatus('DeleteDetector param error. exmples: DeleteDetector(1, 0.5, 0.5)');
      Result := False;
      exit;
    end;
  reversed_count := Param[0];
  pt[0] := Param[1];
  pt[1] := Param[2];
  coord := Vec2Mul(pt, Raster.Size2D);

  SetLength(detArry, DetectorDefineList.Count);
  for i := 0 to DetectorDefineList.Count - 1 do
      detArry[i] := DetectorDefineList[i];

  QuickSortList(detArry, 0, DetectorDefineList.Count - 1);

  for i := reversed_count to length(detArry) - 1 do
    begin
      det := detArry[i];
      DetectorDefineList.Remove(det);
      disposeObject(det);
    end;

  SetLength(detArry, 0);
  Result := True;
end;

constructor TAI_Image.Create(AOwner: TAI_ImageList);
begin
  inherited Create;
  Owner := AOwner;
  DetectorDefineList := TDetectorDefineList.Create;
  Raster := NewRaster();
  FOP_RT := nil;
end;

destructor TAI_Image.Destroy;
begin
  Clear;
  disposeObject(DetectorDefineList);
  disposeObject(Raster);
  if FOP_RT <> nil then
      disposeObject(FOP_RT);
  inherited Destroy;
end;

function TAI_Image.RunExpCondition(ScriptStyle: TTextStyle; exp: SystemString): Boolean;
begin
  CheckAndRegOPRT;
  DoStatusNoLn('Image (%d * %d, detector:%d) EvaluateExpression: %s', [Raster.width, Raster.height, DetectorDefineList.Count, exp]);
  Result := EvaluateExpressionValue(ScriptStyle, exp, FOP_RT);
  if Result then
      DoStatusNoLn(' = yes.')
  else
      DoStatusNoLn(' = no.');
  DoStatusNoLn;
end;

function TAI_Image.RunExpProcess(ScriptStyle: TTextStyle; exp: SystemString): Boolean;
begin
  CheckAndRegOPRT;
  Result := EvaluateExpressionValue(ScriptStyle, exp, FOP_RT);
end;

function TAI_Image.GetExpFunctionList: TPascalStringList;
begin
  CheckAndRegOPRT;
  Result := TPascalStringList.Create;
  FOP_RT.ProcList.GetNameList(Result);
end;

procedure TAI_Image.Clear;
var
  i: Integer;
begin
  for i := 0 to DetectorDefineList.Count - 1 do
      disposeObject(DetectorDefineList[i]);
  DetectorDefineList.Clear;
end;

procedure TAI_Image.ClearPrepareRaster;
var
  i: Integer;
begin
  for i := 0 to DetectorDefineList.Count - 1 do
      DetectorDefineList[i].PrepareRaster.Reset;
end;

procedure TAI_Image.DrawTo(output: TMemoryRaster);
var
  d: TDrawEngine;
  i, j: Integer;
  DetDef: TAI_DetectorDefine;
  pt_p: PVec2;
begin
  d := TDrawEngine.Create;
  d.Options := [];
  output.Assign(Raster);
  d.Rasterization.SetWorkMemory(output);

  for i := 0 to DetectorDefineList.Count - 1 do
    begin
      DetDef := DetectorDefineList[i];
      d.DrawBox(RectV2(DetDef.R), DEColor(1, 0, 0, 1), 3);

      if DetDef.Part.Count = 68 then
          DrawFaceSP(DetDef.Part, DEColor(1, 0, 0, 1), d)
      else
        for j := 0 to DetDef.Part.Count - 1 do
          begin
            pt_p := DetDef.Part.Points[j];
            d.DrawPoint(pt_p^, DEColor(1, 0, 0, 1), 2, 2);
          end;
    end;

  for i := 0 to DetectorDefineList.Count - 1 do
    begin
      DetDef := DetectorDefineList[i];
      if DetDef.Token <> '' then
        begin
          d.BeginCaptureShadow(Vec2(1, 1), 0.9);
          d.DrawText(DetDef.Token, 14, RectV2(DetDef.R), DEColor(1, 1, 1, 1), True);
          d.EndCaptureShadow;
        end;
    end;

  d.Flush;
  disposeObject(d);
end;

function TAI_Image.FoundNoTokenDetectorDefine(output: TMemoryRaster; color: TDEColor): Boolean;
var
  i: Integer;
  DetDef: TAI_DetectorDefine;
  d: TDrawEngine;
begin
  if output <> nil then
    begin
      Result := False;
      output.Assign(Raster);
      d := TDrawEngine.Create;
      d.Rasterization.SetWorkMemory(output);
      for i := 0 to DetectorDefineList.Count - 1 do
        begin
          DetDef := DetectorDefineList[i];
          if DetDef.Token = '' then
            begin
              d.FillBox(RectV2(DetDef.R), color);
              d.BeginCaptureShadow(Vec2(1, 1), 0.9);
              d.DrawText('ERROR!!' + #13#10 + 'NULL TOKEN', 12, RectV2(DetDef.R), DEColorInv(color), True);
              d.EndCaptureShadow;
              Result := True;
            end;
        end;
      d.Flush;
      disposeObject(d);
    end
  else
      Result := FoundNoTokenDetectorDefine();
end;

function TAI_Image.FoundNoTokenDetectorDefine: Boolean;
var
  i: Integer;
  DetDef: TAI_DetectorDefine;
begin
  Result := False;
  for i := 0 to DetectorDefineList.Count - 1 do
    begin
      DetDef := DetectorDefineList[i];
      if DetDef.Token = '' then
        begin
          Result := True;
          exit;
        end;
    end;
end;

procedure TAI_Image.SaveToStream(stream: TMemoryStream64; SaveImg: Boolean; RasterSave_: TRasterSave);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
  i: Integer;
  DetDef: TAI_DetectorDefine;
begin
  de := TDataFrameEngine.Create;

  m64 := TMemoryStream64.Create;
  if SaveImg then
    begin
      Raster.SaveToStream(m64, RasterSave_);
    end;
  de.WriteStream(m64);
  disposeObject(m64);

  de.WriteInteger(DetectorDefineList.Count);

  for i := 0 to DetectorDefineList.Count - 1 do
    begin
      m64 := TMemoryStream64.Create;
      DetDef := DetectorDefineList[i];
      DetDef.SaveToStream(m64, RasterSave_);
      de.WriteStream(m64);
      disposeObject(m64);
    end;

  de.EncodeTo(stream, True);

  disposeObject(de);
end;

procedure TAI_Image.SaveToStream(stream: TMemoryStream64);
begin
  SaveToStream(stream, True, TRasterSave.rsRGB);
end;

procedure TAI_Image.LoadFromStream(stream: TMemoryStream64; LoadImg: Boolean);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
  i, c: Integer;
  DetDef: TAI_DetectorDefine;
begin
  de := TDataFrameEngine.Create;
  de.DecodeFrom(stream);

  if LoadImg then
    begin
      m64 := TMemoryStream64.Create;
      de.Reader.ReadStream(m64);
      if (m64.Size > 0) then
        begin
          m64.Position := 0;
          Raster.LoadFromStream(m64);
        end;
      disposeObject(m64);
    end
  else
      de.Reader.GoNext;

  c := de.Reader.ReadInteger;

  for i := 0 to c - 1 do
    begin
      m64 := TMemoryStream64.Create;
      de.Reader.ReadStream(m64);
      m64.Position := 0;
      DetDef := TAI_DetectorDefine.Create(Self);
      DetDef.LoadFromStream(m64);
      disposeObject(m64);
      DetectorDefineList.Add(DetDef);
    end;

  disposeObject(de);
end;

procedure TAI_Image.LoadFromStream(stream: TMemoryStream64);
begin
  LoadFromStream(stream, True);
end;

procedure TAI_Image.LoadPicture(stream: TMemoryStream64);
begin
  disposeObject(Raster);
  Raster := NewRasterFromStream(stream);
  Clear;
end;

procedure TAI_Image.LoadPicture(fileName: SystemString);
begin
  disposeObject(Raster);
  Raster := NewRasterFromFile(fileName);
  Clear;
end;

procedure TAI_Image.Scale(f: TGeoFloat);
var
  i, j: Integer;
  DetDef: TAI_DetectorDefine;
begin
  if IsEqual(f, 1.0) then
      exit;

  Raster.Scale(f);

  for i := 0 to DetectorDefineList.Count - 1 do
    begin
      DetDef := DetectorDefineList[i];
      DetDef.R := MakeRect(RectMul(RectV2(DetDef.R), f));
      DetDef.Part.Mul(f, f);
    end;
end;

function TAI_Image.ExistsToken(Token: TPascalString): Boolean;
begin
  Result := GetTokenCount(Token) > 0;
end;

function TAI_Image.GetTokenCount(Token: TPascalString): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to DetectorDefineList.Count - 1 do
    if umlMultipleMatch(Token, DetectorDefineList[i].Token) then
        inc(Result);
end;

constructor TAI_ImageList.Create;
begin
  inherited Create;
  UsedJpegForXML := False;
  FileInfo := '';
  UserData := nil;
end;

destructor TAI_ImageList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TAI_ImageList.Clone: TAI_ImageList;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  SaveToStream(m64);
  m64.Position := 0;

  Result := TAI_ImageList.Create;
  Result.LoadFromStream(m64);
  disposeObject(m64);
end;

procedure TAI_ImageList.Delete(index: Integer);
begin
  if index >= 0 then
    begin
      disposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TAI_ImageList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      disposeObject(Items[i]);
  inherited Clear;
end;

procedure TAI_ImageList.ClearPrepareRaster;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].ClearPrepareRaster;
end;

procedure TAI_ImageList.RunScript(ScriptStyle: TTextStyle; condition_exp, process_exp: SystemString);
var
  i, j: Integer;
  img: TAI_Image;
  condition_img_ok, condition_det_ok: Boolean;
begin
  // reset state
  for i := 0 to Count - 1 do
    begin
      img := Items[i];
      img.FOP_RT_RunDeleted := False;
      for j := 0 to img.DetectorDefineList.Count - 1 do
          img.DetectorDefineList[j].FOP_RT_RunDeleted := False;
    end;

  for i := 0 to Count - 1 do
    begin
      img := Items[i];

      if img.RunExpCondition(ScriptStyle, condition_exp) then
          img.RunExpProcess(ScriptStyle, process_exp);
    end;

  // process delete state
  i := 0;
  while i < Count do
    begin
      img := Items[i];

      if img.FOP_RT_RunDeleted then
        begin
          DoStatus('image script on delete %d', [i]);
          Delete(i);
        end
      else
        begin
          j := 0;
          while j < img.DetectorDefineList.Count do
            begin
              if img.DetectorDefineList[j].FOP_RT_RunDeleted then
                begin
                  disposeObject(img.DetectorDefineList[j]);
                  img.DetectorDefineList.Delete(j);
                end
              else
                  inc(j);
            end;

          inc(i);
        end;
    end;
end;

procedure TAI_ImageList.RunScript(condition_exp, process_exp: SystemString);
begin
  RunScript(tsPascal, condition_exp, process_exp);
end;

procedure TAI_ImageList.DrawTo(output: TMemoryRaster);
var
  rp: TRectPacking;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; data: Pointer; Item: TMultiThreadProcItem);
  var
    mr: TMemoryRaster;
  begin
    mr := NewRaster();
    Items[pass].DrawTo(mr);
    LockObject(rp);
    rp.Add(nil, mr, mr.BoundsRectV2);
    UnLockObject(rp);
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor;
  var
    pass: Integer;
    mr: TMemoryRaster;
  begin
    for pass := 0 to Count - 1 do
      begin
        mr := NewRaster();
        Items[pass].DrawTo(mr);
        rp.Add(nil, mr, mr.BoundsRectV2);
      end;
  end;
{$ENDIF parallel}
  procedure BuildOutput_;
  var
    i: Integer;
    mr: TMemoryRaster;
    d: TDrawEngine;
  begin
    DoStatus('build output.');
    d := TDrawEngine.Create;
    d.Options := [];
    output.SetSize(Round(rp.MaxWidth), Round(rp.MaxHeight));
    FillBlackGrayBackgroundTexture(output, 32);

    d.Rasterization.SetWorkMemory(output);
    d.Rasterization.UsedAgg := False;

    for i := 0 to rp.Count - 1 do
      begin
        mr := rp[i]^.Data2 as TMemoryRaster;
        d.DrawTexture(mr, mr.BoundsRectV2, rp[i]^.Rect, 1.0);
      end;

    DoStatus('draw imageList.');
    d.Flush;
    disposeObject(d);
  end;

  procedure FreeTemp_;
  var
    i: Integer;
  begin
    for i := 0 to rp.Count - 1 do
        disposeObject(rp[i]^.Data2);
  end;

begin
  if Count = 0 then
      exit;

  if Count = 1 then
    begin
      First.DrawTo(output);
      exit;
    end;

  DoStatus('build rect packing.');
  rp := TRectPacking.Create;
  rp.Margins := 10;

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, Count - 1);
{$ELSE FPC}
  TParallel.for(0, Count - 1, procedure(pass: Integer)
    var
      mr: TMemoryRaster;
    begin
      mr := NewRaster();
      Items[pass].DrawTo(mr);
      LockObject(rp);
      rp.Add(nil, mr, mr.BoundsRectV2);
      UnLockObject(rp);
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
  rp.Build;
  BuildOutput_;
  FreeTemp_;
  disposeObject(rp);
end;

procedure TAI_ImageList.DrawTo(d: TDrawEngine; Margins: TGeoFloat; destOffset: TDEVec; alpha: TDEFloat);
var
  rList: TMemoryRasterList;
  i: Integer;
begin
  rList := TMemoryRasterList.Create;
  for i := 0 to Count - 1 do
      rList.Add(Items[i].Raster);

  d.DrawTexturePackingInScene(rList, Margins, destOffset, alpha);
  disposeObject(rList);
end;

procedure TAI_ImageList.AddPicture(stream: TCoreClassStream);
var
  img: TAI_Image;
begin
  img := TAI_Image.Create(Self);
  disposeObject(img.Raster);
  img.Raster := NewRasterFromStream(stream);
  Add(img);
end;

procedure TAI_ImageList.AddPicture(fileName: SystemString);
var
  img: TAI_Image;
begin
  img := TAI_Image.Create(Self);
  disposeObject(img.Raster);
  try
      img.Raster := NewRasterFromFile(fileName);
  except
    disposeObject(img);
    exit;
  end;
  Add(img);
end;

procedure TAI_ImageList.AddPicture(R: TMemoryRaster);
var
  img: TAI_Image;
begin
  img := TAI_Image.Create(Self);
  img.Raster.Assign(R);
  Add(img);
end;

procedure TAI_ImageList.AddPicture(mr: TMemoryRaster; R: TRect);
var
  img: TAI_Image;
  DetDef: TAI_DetectorDefine;
begin
  img := TAI_Image.Create(Self);
  img.Raster.Assign(mr);
  DetDef := TAI_DetectorDefine.Create(img);
  DetDef.R := R;
  img.DetectorDefineList.Add(DetDef);
  Add(img);
end;

procedure TAI_ImageList.AddPicture(mr: TMemoryRaster; R: TRectV2);
begin
  AddPicture(mr, MakeRect(R));
end;

procedure TAI_ImageList.LoadFromPictureStream(stream: TCoreClassStream);
begin
  Clear;
  AddPicture(stream);
end;

procedure TAI_ImageList.LoadFromPictureFile(fileName: SystemString);
begin
  Clear;
  AddPicture(fileName);
end;

function TAI_ImageList.PackingRaster: TMemoryRaster;
var
  i: Integer;
  rp: TRectPacking;
  d: TDrawEngine;
  mr: TMemoryRaster;
begin
  Result := NewRaster();
  if Count = 1 then
      Result.Assign(First.Raster)
  else
    begin
      rp := TRectPacking.Create;
      rp.Margins := 10;
      for i := 0 to Count - 1 - 1 do
          rp.Add(nil, Items[i].Raster, Items[i].Raster.BoundsRectV2);
      rp.Build;

      Result.SetSizeF(rp.MaxWidth, rp.MaxHeight, RasterColorF(0, 0, 0, 1));
      d := TDrawEngine.Create;
      d.ViewOptions := [];
      d.Rasterization.SetWorkMemory(Result);

      for i := 0 to rp.Count - 1 do
        begin
          mr := TMemoryRaster(rp[i]^.Data2);
          d.DrawTexture(mr, mr.BoundsRectV2, rp[i]^.Rect, 0, 1.0);
        end;

      d.Flush;
      disposeObject(d);
      disposeObject(rp);
    end;
end;

procedure TAI_ImageList.SaveToPictureStream(stream: TCoreClassStream);
var
  mr: TMemoryRaster;
begin
  mr := PackingRaster();
  mr.SaveToBmp24Stream(stream);
  disposeObject(mr);
end;

procedure TAI_ImageList.SaveToPictureFile(fileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fileName, fmCreate);
  SaveToPictureStream(fs);
  disposeObject(fs);
end;

procedure TAI_ImageList.SavePrepareRasterToPictureStream(stream: TCoreClassStream);
var
  i, j: Integer;
  img: TAI_Image;
  DetDef: TAI_DetectorDefine;
  rp: TRectPacking;
  mr: TMemoryRaster;
  de: TDrawEngine;
begin
  rp := TRectPacking.Create;
  rp.Margins := 10;
  for i := 0 to Count - 1 - 1 do
    begin
      img := Items[i];
      for j := 0 to img.DetectorDefineList.Count - 1 do
        begin
          DetDef := img.DetectorDefineList[i];
          if not DetDef.PrepareRaster.Empty then
              rp.Add(nil, DetDef.PrepareRaster, DetDef.PrepareRaster.BoundsRectV2);
        end;
    end;
  rp.Build;

  de := TDrawEngine.Create;
  de.SetSize(Round(rp.MaxWidth), Round(rp.MaxHeight));
  de.FillBox(de.ScreenRect, DEColor(0, 0, 0, 0));

  for i := 0 to rp.Count - 1 do
    begin
      mr := rp[i]^.Data2 as TMemoryRaster;
      de.DrawTexture(mr, mr.BoundsRectV2, rp[i]^.Rect, 0, 1.0);
    end;

  de.Flush;
  de.Rasterization.memory.SaveToBmp24Stream(stream);
  disposeObject(de);
  disposeObject(rp);
end;

procedure TAI_ImageList.SavePrepareRasterToPictureFile(fileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fileName, fmCreate);
  SavePrepareRasterToPictureStream(fs);
  disposeObject(fs);
end;

procedure TAI_ImageList.SaveToStream(stream: TCoreClassStream);
begin
  SaveToStream(stream, True, True);
end;

procedure TAI_ImageList.SaveToStream(stream: TCoreClassStream; SaveImg, Compressed: Boolean);
begin
  SaveToStream(stream, SaveImg, Compressed, TRasterSave.rsRGB);
end;

procedure TAI_ImageList.SaveToStream(stream: TCoreClassStream; SaveImg, Compressed: Boolean; RasterSave_: TRasterSave);
var
  de: TDataFrameEngine;
  m64: TMemoryStream64;
  i: Integer;
  imgData: TAI_Image;
begin
  de := TDataFrameEngine.Create;

  de.WriteInteger(Count);

  for i := 0 to Count - 1 do
    begin
      m64 := TMemoryStream64.Create;
      imgData := Items[i];
      imgData.SaveToStream(m64, SaveImg, RasterSave_);
      de.WriteStream(m64);
      disposeObject(m64);
    end;

  if Compressed then
      de.EncodeAsZLib(stream, False)
  else
      de.EncodeTo(stream, True);

  disposeObject(de);
end;

procedure TAI_ImageList.LoadFromStream(stream: TCoreClassStream; LoadImg: Boolean);
var
  de: TDataFrameEngine;
  i, j, c: Integer;
  m64: TMemoryStream64;
  imgData: TAI_Image;
begin
  Clear;
  de := TDataFrameEngine.Create;
  de.DecodeFrom(stream);

  c := de.Reader.ReadInteger;

  for i := 0 to c - 1 do
    begin
      m64 := TMemoryStream64.Create;
      de.Reader.ReadStream(m64);
      m64.Position := 0;
      imgData := TAI_Image.Create(Self);
      imgData.LoadFromStream(m64, LoadImg);
      disposeObject(m64);
      Add(imgData);
    end;

  disposeObject(de);
end;

procedure TAI_ImageList.LoadFromStream(stream: TCoreClassStream);
begin
  LoadFromStream(stream, True);
end;

procedure TAI_ImageList.SaveToFile(fileName: SystemString);
var
  fs: TReliableFileStream;
begin
  fs := TReliableFileStream.Create(fileName, True, True);
  SaveToStream(fs, True, True);
  disposeObject(fs);
end;

procedure TAI_ImageList.SaveToFile(fileName: SystemString; SaveImg, Compressed: Boolean; RasterSave_: TRasterSave);
var
  fs: TReliableFileStream;
begin
  fs := TReliableFileStream.Create(fileName, True, True);
  SaveToStream(fs, SaveImg, Compressed, RasterSave_);
  disposeObject(fs);
end;

procedure TAI_ImageList.LoadFromFile(fileName: SystemString; LoadImg: Boolean);
var
  fs: TReliableFileStream;
begin
  fs := TReliableFileStream.Create(fileName, False, False);
  LoadFromStream(fs, LoadImg);
  disposeObject(fs);
end;

procedure TAI_ImageList.LoadFromFile(fileName: SystemString);
begin
  LoadFromFile(fileName, True);
end;

procedure TAI_ImageList.Import(imgList: TAI_ImageList);
var
  i: Integer;
  m64: TMemoryStream64;
  imgData: TAI_Image;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      m64 := TMemoryStream64.Create;
      imgList[i].SaveToStream(m64, True, TRasterSave.rsRGB);

      imgData := TAI_Image.Create(Self);
      m64.Position := 0;
      imgData.LoadFromStream(m64, True);
      Add(imgData);

      disposeObject(m64);
    end;
end;

procedure TAI_ImageList.CalibrationNullDetectorDefineToken(Token: SystemString);
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
begin
  for i := 0 to Count - 1 do
    begin
      imgData := Items[i];
      for j := 0 to imgData.DetectorDefineList.Count - 1 do
        begin
          DetDef := imgData.DetectorDefineList[j];
          DetDef.Token := umlTrimSpace(DetDef.Token);
          if DetDef.Token = '' then
              DetDef.Token := Token;
          DetDef.Token := umlTrimSpace(DetDef.Token);
        end
    end;
end;

procedure TAI_ImageList.CalibrationDetectorDefineTokenPrefix(Prefix: SystemString);
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
begin
  for i := 0 to Count - 1 do
    begin
      imgData := Items[i];
      for j := 0 to imgData.DetectorDefineList.Count - 1 do
        begin
          DetDef := imgData.DetectorDefineList[j];
          DetDef.Token := umlTrimSpace(DetDef.Token);
          if DetDef.Token = '' then
              DetDef.Token := Prefix + DetDef.Token;
          DetDef.Token := umlTrimSpace(DetDef.Token);
        end
    end;
end;

procedure TAI_ImageList.CalibrationDetectorDefineTokenSuffix(Suffix: SystemString);
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
begin
  for i := 0 to Count - 1 do
    begin
      imgData := Items[i];
      for j := 0 to imgData.DetectorDefineList.Count - 1 do
        begin
          DetDef := imgData.DetectorDefineList[j];
          DetDef.Token := umlTrimSpace(DetDef.Token);
          if DetDef.Token = '' then
              DetDef.Token := DetDef.Token + Suffix;
          DetDef.Token := umlTrimSpace(DetDef.Token);
        end
    end;
end;

procedure TAI_ImageList.Scale(f: TGeoFloat);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Scale(f);
end;

procedure TAI_ImageList.Export_PrepareRaster(outputPath: SystemString);
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
  n: TPascalString;
  Raster: TMemoryRaster;
  hList: THashObjectList;
  mrList: TMemoryRasterList;
  pl: TPascalStringList;
  dn, fn: SystemString;
  m64: TMemoryStream64;
begin
  hList := THashObjectList.Create(True);
  for i := 0 to Count - 1 do
    begin
      imgData := Items[i];
      for j := 0 to imgData.DetectorDefineList.Count - 1 do
        begin
          DetDef := imgData.DetectorDefineList[j];
          if (not DetDef.PrepareRaster.Empty) then
            begin
              if (DetDef.Token <> '') then
                  n := DetDef.Token
              else
                  n := 'No_Define';

              if not hList.Exists(n) then
                  hList.FastAdd(n, TMemoryRasterList.Create);
              TMemoryRasterList(hList[n]).Add(DetDef.PrepareRaster);
            end;
        end;
    end;

  pl := TPascalStringList.Create;
  hList.GetNameList(pl);
  for i := 0 to pl.Count - 1 do
    begin
      mrList := TMemoryRasterList(hList[pl[i]]);
      dn := umlCombinePath(outputPath, pl[i]);
      umlCreateDirectory(dn);
      for j := 0 to mrList.Count - 1 do
        begin
          Raster := mrList[j];
          m64 := TMemoryStream64.Create;
          Raster.SaveToJpegRGBStream(m64, 80);
          fn := umlCombineFileName(dn, PFormat('%s.jpg', [umlStreamMD5String(m64).Text]));
          m64.SaveToFile(fn);
          disposeObject(m64);
        end;
    end;

  disposeObject(pl);
  disposeObject(hList);
end;

procedure TAI_ImageList.Export_DetectorRaster(outputPath: SystemString);
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
  n: TPascalString;
  Raster: TMemoryRaster;
  hList: THashObjectList;
  mrList: TMemoryRasterList;
  pl: TPascalStringList;
  dn, fn: SystemString;
  m64: TMemoryStream64;
begin
  hList := THashObjectList.Create(True);
  for i := 0 to Count - 1 do
    begin
      imgData := Items[i];
      for j := 0 to imgData.DetectorDefineList.Count - 1 do
        begin
          DetDef := imgData.DetectorDefineList[j];
          if (DetDef.Token <> '') then
              n := DetDef.Token
          else
              n := 'No_Define';

          if not hList.Exists(n) then
              hList.FastAdd(n, TMemoryRasterList.Create);

          TMemoryRasterList(hList[n]).Add(DetDef.Owner.Raster.BuildAreaCopy(DetDef.R));
        end;
    end;

  pl := TPascalStringList.Create;
  hList.GetNameList(pl);
  for i := 0 to pl.Count - 1 do
    begin
      mrList := TMemoryRasterList(hList[pl[i]]);
      dn := umlCombinePath(outputPath, pl[i]);
      umlCreateDirectory(dn);
      for j := 0 to mrList.Count - 1 do
        begin
          Raster := mrList[j];
          m64 := TMemoryStream64.Create;
          Raster.SaveToJpegRGBStream(m64, 80);
          fn := umlCombineFileName(dn, PFormat('%s.jpg', [umlStreamMD5String(m64).Text]));
          m64.SaveToFile(fn);
          disposeObject(m64);
          disposeObject(Raster);
        end;
    end;

  disposeObject(pl);
  disposeObject(hList);
end;

procedure TAI_ImageList.Build_XML(TokenFilter: SystemString; includeLabel, includePart, usedJpeg: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList);
  function num_2(num: Integer): SystemString;
  begin
    if num < 10 then
        Result := PFormat('0%d', [num])
    else
        Result := PFormat('%d', [num]);
  end;

  procedure SaveFileInfo(fn: TPascalString);
  begin
    if BuildFileList <> nil then
      if BuildFileList.ExistsValue(fn) < 0 then
          BuildFileList.Add(fn);
  end;

var
  body: TPascalStringList;
  output_path, n: SystemString;
  i, j, k: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
  m5: TMD5;
  m64: TMemoryStream64;
  v_p: PVec2;
  s_body: SystemString;
begin
  body := TPascalStringList.Create;
  output_path := umlGetFilePath(build_output_file);
  umlCreateDirectory(output_path);
  umlSetCurrentPath(output_path);

  for i := 0 to Count - 1 do
    begin
      imgData := Items[i];
      if (imgData.DetectorDefineList.Count = 0) or (not imgData.ExistsToken(TokenFilter)) then
          continue;

      m64 := TMemoryStream64.Create;
      if usedJpeg then
        begin
          imgData.Raster.SaveToJpegRGBStream(m64, 80);
          m5 := umlStreamMD5(m64);
          n := umlCombineFileName(output_path, Prefix + umlMD5ToStr(m5) + '.jpg');
        end
      else
        begin
          imgData.Raster.SaveToBmp24Stream(m64);
          m5 := umlStreamMD5(m64);
          n := umlCombineFileName(output_path, Prefix + umlMD5ToStr(m5) + '.bmp');
        end;

      if not umlFileExists(n) then
        begin
          m64.SaveToFile(n);
          SaveFileInfo(n);
        end;
      disposeObject(m64);

      body.Add(PFormat(' <image file='#39'%s'#39'>', [umlGetFileName(n).Text]));
      for j := 0 to imgData.DetectorDefineList.Count - 1 do
        begin
          DetDef := imgData.DetectorDefineList[j];
          if umlMultipleMatch(TokenFilter, DetDef.Token) then
            begin
              body.Add(PFormat(
                '  <box top='#39'%d'#39' left='#39'%d'#39' width='#39'%d'#39' height='#39'%d'#39'>',
                [DetDef.R.Top, DetDef.R.Left, DetDef.R.width, DetDef.R.height]));

              if includeLabel and (DetDef.Token.Len > 0) then
                  body.Add(PFormat('    <label>%s</label>', [DetDef.Token.Text]));

              if includePart then
                begin
                  for k := 0 to DetDef.Part.Count - 1 do
                    begin
                      v_p := DetDef.Part[k];
                      body.Add(PFormat(
                        '    <part name='#39'%s'#39' x='#39'%d'#39' y='#39'%d'#39'/>',
                        [num_2(k), Round(v_p^[0]), Round(v_p^[1])]));
                    end;
                end;

              body.Add('  </box>');
            end;
        end;
      body.Add(' </image>');
    end;

  s_body := body.Text;
  disposeObject(body);

  m64 := TMemoryStream64.Create;
  Build_XML_Style(m64);
  n := umlCombineFileName(output_path, umlChangeFileExt(umlGetFileName(build_output_file), '.xsl'));
  m64.SaveToFile(n);
  SaveFileInfo(n);
  disposeObject(m64);

  m64 := TMemoryStream64.Create;
  Build_XML_Dataset(umlGetFileName(n), datasetName, comment, s_body, m64);
  m64.SaveToFile(build_output_file);
  SaveFileInfo(build_output_file);
  disposeObject(m64);
end;

procedure TAI_ImageList.Build_XML(TokenFilter: SystemString; includeLabel, includePart: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList);
begin
  Build_XML(TokenFilter, includeLabel, includePart, UsedJpegForXML, datasetName, comment, build_output_file, Prefix, BuildFileList);
end;

procedure TAI_ImageList.Build_XML(includeLabel, includePart: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList);
begin
  Build_XML('', includeLabel, includePart, datasetName, comment, build_output_file, Prefix, BuildFileList);
end;

procedure TAI_ImageList.Build_XML(includeLabel, includePart: Boolean; datasetName, comment, build_output_file: SystemString);
begin
  Build_XML('', includeLabel, includePart, datasetName, comment, build_output_file, '', nil);
end;

function TAI_ImageList.DetectorDefineCount: Integer;
var
  i: Integer;
  imgData: TAI_Image;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    begin
      imgData := Items[i];
      inc(Result, imgData.DetectorDefineList.Count);
    end;
end;

function TAI_ImageList.DetectorDefinePartCount: Integer;
var
  i, j: Integer;
  imgData: TAI_Image;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    begin
      imgData := Items[i];
      for j := 0 to imgData.DetectorDefineList.Count - 1 do
          inc(Result, imgData.DetectorDefineList[i].Part.Count);
    end;
end;

function TAI_ImageList.ExtractDetectorDefineAsSnapshot: TMemoryRaster2DArray;
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
  mr: TMemoryRaster;
  hList: THashObjectList;
  mrList: TMemoryRasterList;
  pl: TPascalStringList;
begin
  hList := THashObjectList.Create(True);
  for i := 0 to Count - 1 do
    begin
      imgData := Items[i];
      for j := 0 to imgData.DetectorDefineList.Count - 1 do
        begin
          DetDef := imgData.DetectorDefineList[j];
          if DetDef.Token <> '' then
            begin
              mr := NewRaster();
              mr.UserToken := DetDef.Token;
              mr.SetWorkMemory(DetDef.Owner.Raster);
              if not hList.Exists(DetDef.Token) then
                  hList.FastAdd(DetDef.Token, TMemoryRasterList.Create);
              if TMemoryRasterList(hList[DetDef.Token]).IndexOf(mr) < 0 then
                  TMemoryRasterList(hList[DetDef.Token]).Add(mr);
            end;
        end;
    end;

  // process sequence
  SetLength(Result, hList.Count);
  pl := TPascalStringList.Create;
  hList.GetNameList(pl);
  for i := 0 to pl.Count - 1 do
    begin
      mrList := TMemoryRasterList(hList[pl[i]]);
      SetLength(Result[i], mrList.Count);
      for j := 0 to mrList.Count - 1 do
          Result[i, j] := mrList[j];
    end;

  disposeObject(pl);
  disposeObject(hList);
end;

function TAI_ImageList.ExtractDetectorDefineAsPrepareRaster(SS_width, SS_height: Integer): TMemoryRaster2DArray;
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
  mr: TMemoryRaster;
  hList: THashObjectList;
  mrList: TMemoryRasterList;
  pl: TPascalStringList;
begin
  hList := THashObjectList.Create(True);
  for i := 0 to Count - 1 do
    begin
      imgData := Items[i];
      for j := 0 to imgData.DetectorDefineList.Count - 1 do
        begin
          DetDef := imgData.DetectorDefineList[j];
          if DetDef.Token <> '' then
            if not DetDef.PrepareRaster.Empty then
              begin
                mr := NewRaster();
                mr.ZoomFrom(DetDef.PrepareRaster, SS_width, SS_height);
                mr.UserToken := DetDef.Token;
                if not hList.Exists(DetDef.Token) then
                    hList.FastAdd(DetDef.Token, TMemoryRasterList.Create);
                TMemoryRasterList(hList[DetDef.Token]).Add(mr);
              end;
        end;
    end;

  // process sequence
  SetLength(Result, hList.Count);
  pl := TPascalStringList.Create;
  hList.GetNameList(pl);
  for i := 0 to pl.Count - 1 do
    begin
      mrList := TMemoryRasterList(hList[pl[i]]);
      SetLength(Result[i], mrList.Count);
      for j := 0 to mrList.Count - 1 do
          Result[i, j] := mrList[j];
    end;

  disposeObject(pl);
  disposeObject(hList);
end;

function TAI_ImageList.ExtractDetectorDefineAsScaleSpace(SS_width, SS_height: Integer): TMemoryRaster2DArray;
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
  mr: TMemoryRaster;
  hList: THashObjectList;
  mrList: TMemoryRasterList;
  pl: TPascalStringList;
begin
  hList := THashObjectList.Create(True);
  for i := 0 to Count - 1 do
    begin
      imgData := Items[i];
      for j := 0 to imgData.DetectorDefineList.Count - 1 do
        begin
          DetDef := imgData.DetectorDefineList[j];
          if DetDef.Token <> '' then
            begin
              mr := DetDef.Owner.Raster.BuildAreaOffsetScaleSpace(DetDef.R, SS_width, SS_height);
              mr.UserToken := DetDef.Token;
              if not hList.Exists(DetDef.Token) then
                  hList.FastAdd(DetDef.Token, TMemoryRasterList.Create);
              TMemoryRasterList(hList[DetDef.Token]).Add(mr);
            end;
        end;
    end;

  // process sequence
  SetLength(Result, hList.Count);
  pl := TPascalStringList.Create;
  hList.GetNameList(pl);
  for i := 0 to pl.Count - 1 do
    begin
      mrList := TMemoryRasterList(hList[pl[i]]);
      SetLength(Result[i], mrList.Count);
      for j := 0 to mrList.Count - 1 do
          Result[i, j] := mrList[j];
    end;

  disposeObject(pl);
  disposeObject(hList);
end;

function TAI_ImageList.FoundNoTokenDetectorDefine(output: TMemoryRaster): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if Items[i].FoundNoTokenDetectorDefine(output, DEColor(1, 0, 0, 0.5)) then
        exit;
  Result := False;
end;

function TAI_ImageList.FoundNoTokenDetectorDefine: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if Items[i].FoundNoTokenDetectorDefine then
        exit;
  Result := False;
end;

function TAI_ImageList.Tokens: TArrayPascalString;
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
  hList: THashList;
begin
  hList := THashList.Create;
  for i := 0 to Count - 1 do
    begin
      imgData := Items[i];
      for j := 0 to imgData.DetectorDefineList.Count - 1 do
        begin
          DetDef := imgData.DetectorDefineList[j];
          if DetDef.Token <> '' then
            if not hList.Exists(DetDef.Token) then
                hList.Add(DetDef.Token, nil, False);
        end;
    end;

  hList.GetNameList(Result);
  disposeObject(hList);
end;

function TAI_ImageList.ExistsToken(Token: TPascalString): Boolean;
begin
  Result := GetTokenCount(Token) > 0;
end;

function TAI_ImageList.GetTokenCount(Token: TPascalString): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      inc(Result, Items[i].GetTokenCount(Token));
end;

constructor TAI_ImageMatrix.Create;
begin
  inherited Create;
  UsedJpegForXML := False;
end;

destructor TAI_ImageMatrix.Destroy;
begin
  inherited Destroy;
end;

procedure TAI_ImageMatrix.RunScript(ScriptStyle: TTextStyle; condition_exp, process_exp: SystemString);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].RunScript(ScriptStyle, condition_exp, process_exp);
end;

procedure TAI_ImageMatrix.RunScript(condition_exp, process_exp: SystemString);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].RunScript(condition_exp, process_exp);
end;

function TAI_ImageMatrix.FindImageList(FileInfo: TPascalString): TAI_ImageList;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if FileInfo.Same(@Items[i].FileInfo) then
      begin
        Result := Items[i];
        exit;
      end;
  Result := nil;
end;

procedure TAI_ImageMatrix.SaveToStream(stream: TCoreClassStream; SaveImg: Boolean; RasterSave_: TRasterSave);
type
  PSaveRec = ^TSaveRec;

  TSaveRec = record
    fn: TPascalString;
    m64: TMemoryStream64;
  end;

var
  dbEng: TObjectDataManager;
  fPos: Int64;
  PrepareSave: array of TSaveRec;
  FinishSave: array of PSaveRec;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure fpc_Prepare_Save_ParallelFor(pass: PtrInt; data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PSaveRec;
  begin
    p := @PrepareSave[pass];
    p^.m64 := TMemoryStream64.CustomCreate(1024 * 1024);
    Items[pass].SaveToStream(p^.m64, SaveImg, True, RasterSave_);
    p^.fn := Items[pass].FileInfo.TrimChar(#32#9);
    if (p^.fn.Len = 0) then
        p^.fn := umlStreamMD5String(p^.m64);
    p^.fn := p^.fn + C_ImageList_Ext;
    FinishSave[pass] := p;
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure Prepare_Save();
  var
    i: Integer;
    p: PSaveRec;
  begin
    for i := 0 to Count - 1 do
      begin
        p := @PrepareSave[i];
        p^.m64 := TMemoryStream64.CustomCreate(1024 * 1024);
        Items[i].SaveToStream(p^.m64, SaveImg, True, RasterSave_);
        p^.fn := Items[i].FileInfo.TrimChar(#32#9);
        if (p^.fn.Len = 0) then
            p^.fn := umlStreamMD5String(p^.m64);
        p^.fn := p^.fn + C_ImageList_Ext;
        FinishSave[i] := p;
      end;
  end;

{$ENDIF parallel}
  procedure Save();
  var
    i: Integer;
    p: PSaveRec;
    itmHnd: TItemHandle;
  begin
    for i := 0 to Count - 1 do
      begin
        while FinishSave[i] = nil do
            TCoreClassThread.Sleep(1);

        p := FinishSave[i];

        dbEng.ItemFastCreate(fPos, p^.fn, 'ImageMatrix', itmHnd);
        dbEng.ItemWrite(itmHnd, p^.m64.Size, p^.m64.memory^);
        dbEng.ItemClose(itmHnd);
        disposeObject(p^.m64);
        p^.fn := '';
      end;
  end;

begin
  dbEng := TObjectDataManagerOfCache.CreateAsStream(stream, '', DBMarshal.ID, False, True, False);
  fPos := dbEng.RootField;

  SetLength(PrepareSave, Count);
  SetLength(FinishSave, Count);

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@fpc_Prepare_Save_ParallelFor, 0, Count - 1);
  Save();
{$ELSE FPC}
  TParallel.for(0, Count - 1, procedure(pass: Integer)
    var
      p: PSaveRec;
    begin
      p := @PrepareSave[pass];
      p^.m64 := TMemoryStream64.CustomCreate(1024 * 1024);
      Items[pass].SaveToStream(p^.m64, SaveImg, True, RasterSave_);
      p^.fn := Items[pass].FileInfo.TrimChar(#32#9);
      if (p^.fn.Len = 0) then
          p^.fn := umlStreamMD5String(p^.m64);
      p^.fn := p^.fn + C_ImageList_Ext;
      FinishSave[pass] := p;
    end);
  Save();
{$ENDIF FPC}
{$ELSE parallel}
  Prepare_Save();
  Save();
{$ENDIF parallel}
  disposeObject(dbEng);
  DoStatus('Save Image Matrix done.');
end;

procedure TAI_ImageMatrix.SaveToStream(stream: TCoreClassStream);
begin
  SaveToStream(stream, True, TRasterSave.rsRGB);
end;

procedure TAI_ImageMatrix.LoadFromStream(stream: TCoreClassStream);
type
  PLoadRec = ^TLoadRec;

  TLoadRec = record
    fn: TPascalString;
    m64: TMemoryStream64;
    imgList: TAI_ImageList;
  end;

var
  dbEng: TObjectDataManager;
  fPos: Int64;
  PrepareLoadBuffer: TCoreClassList;
  itmSR: TItemSearch;

  procedure PrepareMemory;
  var
    itmHnd: TItemHandle;
    p: PLoadRec;
  begin
    new(p);
    p^.fn := umlChangeFileExt(itmSR.name, '');
    dbEng.ItemFastOpen(itmSR.HeaderPOS, itmHnd);
    p^.m64 := TMemoryStream64.Create;
    p^.m64.Size := itmHnd.Item.Size;
    dbEng.ItemRead(itmHnd, itmHnd.Item.Size, p^.m64.memory^);
    dbEng.ItemClose(itmHnd);

    p^.imgList := TAI_ImageList.Create;
    Add(p^.imgList);
    PrepareLoadBuffer.Add(p);
  end;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure Load_ParallelFor(pass: PtrInt; data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PLoadRec;
  begin
    p := PrepareLoadBuffer[pass];
    p^.m64.Position := 0;
    p^.imgList.LoadFromStream(p^.m64);
    p^.imgList.FileInfo := p^.fn;
    disposeObject(p^.m64);
    p^.fn := '';
    Dispose(p);
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure Load_For();
  var
    i: Integer;
    p: PLoadRec;
  begin
    for i := 0 to PrepareLoadBuffer.Count - 1 do
      begin
        p := PrepareLoadBuffer[i];
        p^.m64.Position := 0;
        p^.imgList.LoadFromStream(p^.m64);
        p^.imgList.FileInfo := p^.fn;
        disposeObject(p^.m64);
        p^.fn := '';
        Dispose(p);
      end;
  end;
{$ENDIF parallel}


begin
  dbEng := TObjectDataManagerOfCache.CreateAsStream(stream, '', DBMarshal.ID, True, False, False);
  fPos := dbEng.RootField;
  PrepareLoadBuffer := TCoreClassList.Create;

  if dbEng.ItemFastFindFirst(fPos, '', itmSR) then
    begin
      repeat
        if umlMultipleMatch('*' + C_ImageList_Ext, itmSR.name) then
            PrepareMemory;
      until not dbEng.ItemFindNext(itmSR);
    end;
  disposeObject(dbEng);

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Load_ParallelFor, 0, PrepareLoadBuffer.Count - 1);
{$ELSE FPC}
  TParallel.for(0, PrepareLoadBuffer.Count - 1, procedure(pass: Integer)
    var
      p: PLoadRec;
    begin
      p := PrepareLoadBuffer[pass];
      p^.m64.Position := 0;
      p^.imgList.LoadFromStream(p^.m64);
      p^.imgList.FileInfo := p^.fn;
      disposeObject(p^.m64);
      p^.fn := '';
      Dispose(p);
    end);
{$ENDIF FPC}
{$ELSE parallel}
  Load_For();
{$ENDIF parallel}
  disposeObject(PrepareLoadBuffer);
  DoStatus('Load Image Matrix done.');
end;

procedure TAI_ImageMatrix.SaveToFile(fileName: SystemString; SaveImg: Boolean; RasterSave_: TRasterSave);
var
  fs: TCoreClassFileStream;
begin
  DoStatus('save Image Matrix: %s', [fileName]);
  fs := TCoreClassFileStream.Create(fileName, fmCreate);
  SaveToStream(fs, SaveImg, RasterSave_);
  disposeObject(fs);
end;

procedure TAI_ImageMatrix.SaveToFile(fileName: SystemString);
begin
  SaveToFile(fileName, True, TRasterSave.rsRGB);
end;

procedure TAI_ImageMatrix.LoadFromFile(fileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  DoStatus('loading Image Matrix: %s', [fileName]);
  fs := TCoreClassFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  LoadFromStream(fs);
  disposeObject(fs);
end;

procedure TAI_ImageMatrix.ClearPrepareRaster;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].ClearPrepareRaster;
end;

procedure TAI_ImageMatrix.SearchAndAddImageList(rootPath, filter: SystemString; includeSubdir, LoadImg: Boolean);

  procedure ProcessImg(fn, Prefix: TPascalString);
  var
    imgList: TAI_ImageList;
  begin
    DoStatus('%s (%s)', [fn.Text, Prefix.Text]);
    imgList := TAI_ImageList.Create;
    imgList.LoadFromFile(fn, LoadImg);
    imgList.FileInfo := Prefix;
    Add(imgList);
  end;

  procedure ProcessPath(ph, Prefix: TPascalString);
  var
    fl, dl: TPascalStringList;
    i: Integer;
  begin
    fl := TPascalStringList.Create;
    umlGetFileList(ph, fl);

    for i := 0 to fl.Count - 1 do
      if umlMultipleMatch(filter, fl[i]) then
        begin
          if Prefix.Len > 0 then
              ProcessImg(umlCombineFileName(ph, fl[i]), Prefix + '.' + umlChangeFileExt(fl[i], ''))
          else
              ProcessImg(umlCombineFileName(ph, fl[i]), umlChangeFileExt(fl[i], ''));
        end;

    disposeObject(fl);

    if includeSubdir then
      begin
        dl := TPascalStringList.Create;
        umlGetDirList(ph, dl);
        for i := 0 to dl.Count - 1 do
          begin
            if Prefix.Len > 0 then
                ProcessPath(umlCombinePath(ph, dl[i]), Prefix + '.' + dl[i])
            else
                ProcessPath(umlCombinePath(ph, dl[i]), dl[i]);
          end;
        disposeObject(dl);
      end;
  end;

begin
  ProcessPath(rootPath, '')
end;

procedure TAI_ImageMatrix.Scale(f: TGeoFloat);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Scale(f);
end;

procedure TAI_ImageMatrix.Export_PrepareRaster(outputPath: SystemString);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Export_PrepareRaster(outputPath);
end;

procedure TAI_ImageMatrix.Export_DetectorRaster(outputPath: SystemString);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Export_DetectorRaster(outputPath);
end;

procedure TAI_ImageMatrix.Build_XML(TokenFilter: SystemString; includeLabel, includePart, usedJpeg: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList);
  function num_2(num: Integer): SystemString;
  begin
    if num < 10 then
        Result := PFormat('0%d', [num])
    else
        Result := PFormat('%d', [num]);
  end;

  procedure SaveFileInfo(fn: TPascalString);
  begin
    if BuildFileList <> nil then
        BuildFileList.Add(fn);
  end;

  procedure ProcessBody(imgList: TAI_ImageList; body: TPascalStringList; output_path: SystemString);
  var
    i, j, k: Integer;
    imgData: TAI_Image;
    DetDef: TAI_DetectorDefine;
    m64: TMemoryStream64;
    m5: TMD5;
    n: SystemString;
    v_p: PVec2;
  begin
    for i := 0 to imgList.Count - 1 do
      begin
        imgData := imgList[i];
        if (imgData.DetectorDefineList.Count = 0) or (not imgData.ExistsToken(TokenFilter)) then
            continue;

        m64 := TMemoryStream64.Create;

        if usedJpeg then
          begin
            imgData.Raster.SaveToJpegRGBStream(m64, 80);
            m5 := umlStreamMD5(m64);
            n := umlCombineFileName(output_path, Prefix + umlMD5ToStr(m5) + '.jpg');
          end
        else
          begin
            imgData.Raster.SaveToBmp24Stream(m64);
            m5 := umlStreamMD5(m64);
            n := umlCombineFileName(output_path, Prefix + umlMD5ToStr(m5) + '.bmp');
          end;

        if not umlFileExists(n) then
          begin
            m64.SaveToFile(n);
            SaveFileInfo(n);
          end;
        disposeObject(m64);

        body.Add(PFormat(' <image file='#39'%s'#39'>', [umlGetFileName(n).Text]));
        for j := 0 to imgData.DetectorDefineList.Count - 1 do
          begin
            DetDef := imgData.DetectorDefineList[j];
            if umlMultipleMatch(TokenFilter, DetDef.Token) then
              begin
                body.Add(PFormat(
                  '  <box top='#39'%d'#39' left='#39'%d'#39' width='#39'%d'#39' height='#39'%d'#39'>',
                  [DetDef.R.Top, DetDef.R.Left, DetDef.R.width, DetDef.R.height]));

                if includeLabel and (DetDef.Token.Len > 0) then
                    body.Add(PFormat('    <label>%s</label>', [DetDef.Token.Text]));

                if includePart then
                  begin
                    for k := 0 to DetDef.Part.Count - 1 do
                      begin
                        v_p := DetDef.Part[k];
                        body.Add(PFormat(
                          '    <part name='#39'%s'#39' x='#39'%d'#39' y='#39'%d'#39'/>',
                          [num_2(k), Round(v_p^[0]), Round(v_p^[1])]));
                      end;
                  end;

                body.Add('  </box>');
              end;
          end;
        body.Add(' </image>');
      end;
  end;

var
  body: TPascalStringList;
  output_path, n: SystemString;
  m64: TMemoryStream64;
  i: Integer;
  s_body: SystemString;
begin
  body := TPascalStringList.Create;
  output_path := umlGetFilePath(build_output_file);
  umlCreateDirectory(output_path);
  umlSetCurrentPath(output_path);

  for i := 0 to Count - 1 do
      ProcessBody(Items[i], body, output_path);

  s_body := body.Text;
  disposeObject(body);

  m64 := TMemoryStream64.Create;
  Build_XML_Style(m64);
  n := umlCombineFileName(output_path, Prefix + umlChangeFileExt(umlGetFileName(build_output_file), '.xsl'));
  m64.SaveToFile(n);
  SaveFileInfo(n);
  disposeObject(m64);

  m64 := TMemoryStream64.Create;
  Build_XML_Dataset(umlGetFileName(n), datasetName, comment, s_body, m64);
  m64.SaveToFile(build_output_file);
  SaveFileInfo(build_output_file);
  disposeObject(m64);
end;

procedure TAI_ImageMatrix.Build_XML(TokenFilter: SystemString; includeLabel, includePart: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList);
begin
  Build_XML(TokenFilter, includeLabel, includePart, UsedJpegForXML, datasetName, comment, build_output_file, Prefix, BuildFileList);
end;

procedure TAI_ImageMatrix.Build_XML(includeLabel, includePart: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList);
begin
  Build_XML('', includeLabel, includePart, datasetName, comment, build_output_file, Prefix, BuildFileList);
end;

procedure TAI_ImageMatrix.Build_XML(includeLabel, includePart: Boolean; datasetName, comment, build_output_file: SystemString);
begin
  Build_XML(includeLabel, includePart, datasetName, comment, build_output_file, '', nil);
end;

function TAI_ImageMatrix.ImageCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      inc(Result, Items[i].Count);
end;

function TAI_ImageMatrix.DetectorDefineCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      inc(Result, Items[i].DetectorDefineCount);
end;

function TAI_ImageMatrix.DetectorDefinePartCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      inc(Result, Items[i].DetectorDefinePartCount);
end;

function TAI_ImageMatrix.ExtractDetectorDefineAsSnapshot: TMemoryRaster2DArray;
  procedure BuildHashList(imgList: TAI_ImageList; hList: THashObjectList);
  var
    i, j: Integer;
    imgData: TAI_Image;
    DetDef: TAI_DetectorDefine;
    mr: TMemoryRaster;
  begin
    for i := 0 to imgList.Count - 1 do
      begin
        imgData := imgList[i];
        for j := 0 to imgData.DetectorDefineList.Count - 1 do
          begin
            DetDef := imgData.DetectorDefineList[j];
            if DetDef.Token <> '' then
              begin
                mr := NewRaster();
                mr.SetWorkMemory(DetDef.Owner.Raster);
                mr.UserToken := DetDef.Token;
                if not hList.Exists(DetDef.Token) then
                    hList.FastAdd(DetDef.Token, TMemoryRasterList.Create);
                if TMemoryRasterList(hList[DetDef.Token]).IndexOf(mr) < 0 then
                    TMemoryRasterList(hList[DetDef.Token]).Add(mr);
              end;
          end;
      end;
  end;

var
  i, j: Integer;
  mr: TMemoryRaster;
  hList: THashObjectList;
  mrList: TMemoryRasterList;
  pl: TPascalStringList;
begin
  hList := THashObjectList.Create(True);
  for i := 0 to Count - 1 do
      BuildHashList(Items[i], hList);

  // process sequence
  SetLength(Result, hList.Count);
  pl := TPascalStringList.Create;
  hList.GetNameList(pl);
  for i := 0 to pl.Count - 1 do
    begin
      mrList := TMemoryRasterList(hList[pl[i]]);
      SetLength(Result[i], mrList.Count);
      for j := 0 to mrList.Count - 1 do
          Result[i, j] := mrList[j];
    end;

  disposeObject(pl);
  disposeObject(hList);
end;

function TAI_ImageMatrix.ExtractDetectorDefineAsPrepareRaster(SS_width, SS_height: Integer): TMemoryRaster2DArray;
  procedure BuildHashList(imgList: TAI_ImageList; hList: THashObjectList);
  var
    i, j: Integer;
    imgData: TAI_Image;
    DetDef: TAI_DetectorDefine;
    mr: TMemoryRaster;
  begin
    for i := 0 to imgList.Count - 1 do
      begin
        imgData := imgList[i];
        for j := 0 to imgData.DetectorDefineList.Count - 1 do
          begin
            DetDef := imgData.DetectorDefineList[j];
            if DetDef.Token <> '' then
              if not DetDef.PrepareRaster.Empty then
                begin
                  mr := NewRaster();
                  mr.ZoomFrom(DetDef.PrepareRaster, SS_width, SS_height);
                  mr.UserToken := DetDef.Token;
                  if not hList.Exists(DetDef.Token) then
                      hList.FastAdd(DetDef.Token, TMemoryRasterList.Create);
                  TMemoryRasterList(hList[DetDef.Token]).Add(mr);
                end;
          end;
      end;
  end;

var
  i, j: Integer;
  mr: TMemoryRaster;
  hList: THashObjectList;
  mrList: TMemoryRasterList;
  pl: TPascalStringList;
begin
  hList := THashObjectList.Create(True);
  for i := 0 to Count - 1 do
      BuildHashList(Items[i], hList);

  // process sequence
  SetLength(Result, hList.Count);
  pl := TPascalStringList.Create;
  hList.GetNameList(pl);
  for i := 0 to pl.Count - 1 do
    begin
      mrList := TMemoryRasterList(hList[pl[i]]);
      SetLength(Result[i], mrList.Count);
      for j := 0 to mrList.Count - 1 do
          Result[i, j] := mrList[j];
    end;

  disposeObject(pl);
  disposeObject(hList);
end;

function TAI_ImageMatrix.ExtractDetectorDefineAsScaleSpace(SS_width, SS_height: Integer): TMemoryRaster2DArray;
  procedure BuildHashList(imgList: TAI_ImageList; hList: THashObjectList);
  var
    i, j: Integer;
    imgData: TAI_Image;
    DetDef: TAI_DetectorDefine;
    mr: TMemoryRaster;
  begin
    for i := 0 to imgList.Count - 1 do
      begin
        imgData := imgList[i];
        for j := 0 to imgData.DetectorDefineList.Count - 1 do
          begin
            DetDef := imgData.DetectorDefineList[j];
            if DetDef.Token <> '' then
              begin
                mr := DetDef.Owner.Raster.BuildAreaOffsetScaleSpace(DetDef.R, SS_width, SS_height);
                mr.UserToken := DetDef.Token;
                if not hList.Exists(DetDef.Token) then
                    hList.FastAdd(DetDef.Token, TMemoryRasterList.Create);
                TMemoryRasterList(hList[DetDef.Token]).Add(mr);
              end;
          end;
      end;
  end;

var
  i, j: Integer;
  mr: TMemoryRaster;
  hList: THashObjectList;
  mrList: TMemoryRasterList;
  pl: TPascalStringList;
begin
  hList := THashObjectList.Create(True);
  for i := 0 to Count - 1 do
      BuildHashList(Items[i], hList);

  // process sequence
  SetLength(Result, hList.Count);
  pl := TPascalStringList.Create;
  hList.GetNameList(pl);
  for i := 0 to pl.Count - 1 do
    begin
      mrList := TMemoryRasterList(hList[pl[i]]);
      SetLength(Result[i], mrList.Count);
      for j := 0 to mrList.Count - 1 do
          Result[i, j] := mrList[j];
    end;

  disposeObject(pl);
  disposeObject(hList);
end;

function TAI_ImageMatrix.FoundNoTokenDetectorDefine(output: TMemoryRaster): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if Items[i].FoundNoTokenDetectorDefine(output) then
        exit;
  Result := False;
end;

function TAI_ImageMatrix.FoundNoTokenDetectorDefine: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if Items[i].FoundNoTokenDetectorDefine then
        exit;
  Result := False;
end;

function TAI_ImageMatrix.Tokens: TArrayPascalString;
var
  hList: THashList;

  procedure get_img_tokens(imgL: TAI_ImageList);
  var
    i, j: Integer;
    imgData: TAI_Image;
    DetDef: TAI_DetectorDefine;
  begin
    for i := 0 to imgL.Count - 1 do
      begin
        imgData := imgL[i];
        for j := 0 to imgData.DetectorDefineList.Count - 1 do
          begin
            DetDef := imgData.DetectorDefineList[j];
            if DetDef.Token <> '' then
              if not hList.Exists(DetDef.Token) then
                  hList.Add(DetDef.Token, nil, False);
          end;
      end;

  end;

  procedure do_run;
  var
    i: Integer;
  begin
    for i := 0 to Count - 1 do
        get_img_tokens(Items[i]);
  end;

begin
  hList := THashList.Create;
  do_run;
  hList.GetNameList(Result);
  disposeObject(hList);
end;

function TAI_ImageMatrix.ExistsToken(Token: TPascalString): Boolean;
begin
  Result := GetTokenCount(Token) > 0;
end;

function TAI_ImageMatrix.GetTokenCount(Token: TPascalString): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      inc(Result, Items[i].GetTokenCount(Token));
end;

initialization

Init_AI_Common;

finalization

end.
