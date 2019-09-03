{ ****************************************************************************** }
{ * AI Common (platform compatible)                                            * }
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
{$REGION 'base define'}
  TAI_DetectorDefine = class;
  TAI_Image = class;
  TAI_ImageList = class;

  TSegmentationMask = record
    BackgroundColor, FrontColor: TRColor;
    Token: TPascalString;
    Raster: TMemoryRaster;
  end;

  PSegmentationMask = ^TSegmentationMask;

  TSegmentationColor = record
    Token: TPascalString;
    Color: TRColor;
    ID: WORD;
  end;

  PSegmentationColor = ^TSegmentationColor;

  TImageList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TAI_Image>;
  TEditorDetectorDefineList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TAI_DetectorDefine>;
  TSegmentationMasks_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PSegmentationMask>;
  TSegmentationColorList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PSegmentationColor>;
{$ENDREGION 'base define'}
{$REGION 'detector'}

  TAI_DetectorDefine = class(TCoreClassObject)
  protected
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
{$ENDREGION 'detector'}
{$REGION 'SegmentationColorPool'}

  TSegmentationColorList = class(TSegmentationColorList_Decl)
  private
    procedure DoSegColor(Color: TRColor; var Classify: TSegClassify);
  public
    constructor Create;
    destructor Destroy; override;

    procedure BuildBorderColor;
    procedure Delete(index: Integer);
    procedure Clear;
    procedure AddColor(const Token: TPascalString; const Color: TRColor);

    function IsIgnoredBorder(const c: TRColor): Boolean; overload;
    function IsIgnoredBorder(const ID: WORD): Boolean; overload;
    function IsIgnoredBorder(const Token: TPascalString): Boolean; overload;

    function ExistsColor(const c: TRColor): Boolean;
    function ExistsID(const ID: WORD): Boolean;

    function GetColorID(const c: TRColor; const def: WORD; var output: WORD): Boolean;
    function GetIDColor(const ID: WORD; const def: TRColor; var output: TRColor): Boolean;
    function GetIDColorAndToken(const ID: WORD; const def_color: TRColor; const def_token: TPascalString;
      var output_color: TRColor; var output_token: TPascalString): Boolean;

    function GetColorToken(const c: TRColor; const def: TPascalString): TPascalString; overload;
    function GetColorToken(const c: TRColor): TPascalString; overload;

    function GetTokenColor(const Token: TPascalString; const def: TRColor): TRColor; overload;
    function GetTokenColor(const Token: TPascalString): TRColor; overload;

    procedure BuildLabelViewer(input, output: TMemoryRaster; LabColor, BoxColor: TRColor);

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToFile(fileName: TPascalString);
    procedure LoadFromFile(fileName: TPascalString);
  end;

  PSegmentationColorList = ^TSegmentationColorList;
{$ENDREGION 'SegmentationColorPool'}
{$REGION 'segmentation mask'}

  TSegmentationMasks = class(TSegmentationMasks_Decl)
  private
    procedure MergePixelToRaster(segMask: PSegmentationMask; colors: TSegmentationColorList);
  public
    OwnerImage: TAI_Image;
    MaskMergeRaster: TMemoryRaster;
    constructor Create(OwnerImage_: TAI_Image);
    destructor Destroy; override;

    procedure Remove(p: PSegmentationMask);
    procedure Delete(index: Integer);
    procedure Clear;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure BuildSegmentationMask(Width, Height: Integer; polygon: T2DPolygon; buildBG_color, buildFG_color: TRColor; Token: TPascalString); overload;
    procedure BuildSegmentationMask(Width, Height: Integer; polygon: T2DPolygonGraph; buildBG_color, buildFG_color: TRColor; Token: TPascalString); overload;
    procedure BuildSegmentationMask(Width, Height: Integer; sour: TMemoryRaster; sampler_FG_Color, buildBG_color, buildFG_color: TRColor; Token: TPascalString); overload;

    procedure BuildMaskMerge(colors: TSegmentationColorList);

    procedure SegmentationTokens(output: TPascalStringList);
  end;
{$ENDREGION 'segmentation mask'}
{$REGION 'image'}

  TAI_Image_Script_Register = procedure(Sender: TAI_Image; opRT: TOpCustomRunTime) of object;

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
    function OP_Image_Blur(var Param: TOpParam): Variant;
    function OP_Image_CalibrateRotate(var Param: TOpParam): Variant;
    // process on detector
    function OP_Detector_SetLabel(var Param: TOpParam): Variant;
    function OP_Detector_ClearDetector(var Param: TOpParam): Variant;
    function OP_Detector_DeleteDetector(var Param: TOpParam): Variant;
  public
    Owner: TAI_ImageList;
    DetectorDefineList: TEditorDetectorDefineList;
    SegmentationMaskList: TSegmentationMasks;
    Raster: TMemoryRaster;

    constructor Create(AOwner: TAI_ImageList);
    destructor Destroy; override;

    function RunExpCondition(RSeri: TRasterSerialized; ScriptStyle: TTextStyle; exp: SystemString): Boolean;
    function RunExpProcess(RSeri: TRasterSerialized; ScriptStyle: TTextStyle; exp: SystemString): Boolean;
    function GetExpFunctionList: TPascalStringList;

    procedure RemoveDetectorFromRect(edge: TGeoFloat; R: TRectV2); overload;
    procedure RemoveDetectorFromRect(R: TRectV2); overload;

    procedure ClearDetector;
    procedure ClearSegmentation;
    procedure ClearPrepareRaster;

    procedure DrawTo(output: TMemoryRaster);

    function FoundNoTokenDefine(output: TMemoryRaster; Color: TDEColor): Boolean; overload;
    function FoundNoTokenDefine: Boolean; overload;

    procedure SaveToStream(stream: TMemoryStream64; SaveImg: Boolean; RasterSave_: TRasterSave); overload;
    procedure SaveToStream(stream: TMemoryStream64); overload;

    procedure LoadFromStream(stream: TMemoryStream64; LoadImg: Boolean); overload;
    procedure LoadFromStream(stream: TMemoryStream64); overload;

    procedure LoadPicture(stream: TMemoryStream64); overload;
    procedure LoadPicture(fileName: SystemString); overload;

    procedure Scale(f: TGeoFloat);

    function ExistsDetectorToken(Token: TPascalString): Boolean;
    function GetDetectorTokenCount(Token: TPascalString): Integer;

    // Serialized And Recycle Memory
    procedure SerializedAndRecycleMemory(Serializ: TRasterSerialized);
    procedure UnserializedMemory(Serializ: TRasterSerialized);
  end;
{$ENDREGION 'image'}
{$REGION 'image list'}

  TAI_ImageList = class(TImageList_Decl)
  public
    UsedJpegForXML: Boolean;
    FileInfo: TPascalString;
    UserData: TCoreClassObject;

    constructor Create;
    destructor Destroy; override;

    function Clone: TAI_ImageList;

    procedure Delete(index: Integer); overload;
    procedure Delete(index: Integer; freeObj_: Boolean); overload;

    procedure Remove(img: TAI_Image); overload;
    procedure Remove(img: TAI_Image; freeObj_: Boolean); overload;

    procedure Clear; overload;
    procedure Clear(freeObj_: Boolean); overload;
    procedure ClearDetector;
    procedure ClearSegmentation;
    procedure ClearPrepareRaster;

    procedure RunScript(RSeri: TRasterSerialized; ScriptStyle: TTextStyle; condition_exp, process_exp: SystemString); overload;
    procedure RunScript(RSeri: TRasterSerialized; condition_exp, process_exp: SystemString); overload;
    procedure RunScript(ScriptStyle: TTextStyle; condition_exp, process_exp: SystemString); overload;
    procedure RunScript(condition_exp, process_exp: SystemString); overload;

    procedure DrawTo(output: TMemoryRaster; maxSampler: Integer); overload;
    procedure DrawTo(output: TMemoryRaster); overload;
    procedure DrawToPictureList(d: TDrawEngine; Margins: TGeoFloat; destOffset: TDEVec; alpha: TDEFloat);
    function PackingRaster: TMemoryRaster;
    procedure CalibrationNullToken(Token: SystemString);
    procedure Scale(f: TGeoFloat);

    // import
    procedure Import(imgList: TAI_ImageList);
    procedure AddPicture(stream: TCoreClassStream); overload;
    procedure AddPicture(fileName: SystemString); overload;
    procedure AddPicture(R: TMemoryRaster); overload;
    procedure AddPicture(mr: TMemoryRaster; R: TRect); overload;
    procedure AddPicture(mr: TMemoryRaster; R: TRectV2); overload;

    // load
    procedure LoadFromPictureStream(stream: TCoreClassStream);
    procedure LoadFromPictureFile(fileName: SystemString);
    procedure LoadFromStream(stream: TCoreClassStream; LoadImg: Boolean); overload;
    procedure LoadFromStream(stream: TCoreClassStream); overload;
    procedure LoadFromFile(fileName: SystemString; LoadImg: Boolean); overload;
    procedure LoadFromFile(fileName: SystemString); overload;

    // save
    procedure SaveToPictureStream(stream: TCoreClassStream);
    procedure SaveToPictureFile(fileName: SystemString);
    procedure SavePrepareRasterToPictureStream(stream: TCoreClassStream);
    procedure SavePrepareRasterToPictureFile(fileName: SystemString);
    procedure SaveToStream(stream: TCoreClassStream); overload;
    procedure SaveToStream(stream: TCoreClassStream; SaveImg, Compressed: Boolean); overload;
    procedure SaveToStream(stream: TCoreClassStream; SaveImg, Compressed: Boolean; RasterSave_: TRasterSave); overload;
    procedure SaveToFile(fileName: SystemString); overload;
    procedure SaveToFile(fileName: SystemString; SaveImg, Compressed: Boolean; RasterSave_: TRasterSave); overload;

    // export
    procedure Export_PrepareRaster(outputPath: SystemString);
    procedure Export_DetectorRaster(outputPath: SystemString);
    procedure Export_Segmentation(outputPath: SystemString);
    procedure Build_XML(TokenFilter: SystemString; includeLabel, includePart, usedJpeg: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList); overload;
    procedure Build_XML(TokenFilter: SystemString; includeLabel, includePart: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList); overload;
    procedure Build_XML(includeLabel, includePart: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList); overload;
    procedure Build_XML(includeLabel, includePart: Boolean; datasetName, comment, build_output_file: SystemString); overload;

    // extract
    function ExtractDetectorDefineAsSnapshotProjection(SS_width, SS_height: Integer): TMemoryRaster2DArray;
    function ExtractDetectorDefineAsSnapshot: TMemoryRaster2DArray;
    function ExtractDetectorDefineAsPrepareRaster(SS_width, SS_height: Integer): TMemoryRaster2DArray;
    function ExtractDetectorDefineAsScaleSpace(SS_width, SS_height: Integer): TMemoryRaster2DArray;

    // statistics: image
    function DetectorDefineCount: Integer;
    function DetectorDefinePartCount: Integer;
    function SegmentationMaskCount: Integer;
    function FoundNoTokenDefine(output: TMemoryRaster): Boolean; overload;
    function FoundNoTokenDefine: Boolean; overload;
    procedure AllTokens(output: TPascalStringList);

    // statistics: detector
    function DetectorTokens: TArrayPascalString;
    function ExistsDetectorToken(Token: TPascalString): Boolean;
    function GetDetectorTokenCount(Token: TPascalString): Integer;

    // statistics: segmentation
    procedure SegmentationTokens(output: TPascalStringList);
    function BuildSegmentationColorBuffer: TSegmentationColorList;
    procedure BuildMaskMerge(colors: TSegmentationColorList); overload;
    procedure BuildMaskMerge; overload;
    procedure LargeScale_BuildMaskMerge(RSeri: TRasterSerialized; colors: TSegmentationColorList);
    procedure ClearMaskMerge;

    // Serialized And Recycle Memory
    procedure SerializedAndRecycleMemory(Serializ: TRasterSerialized);
    procedure UnserializedMemory(Serializ: TRasterSerialized);
  end;
{$ENDREGION 'image list'}
{$REGION 'image matrix'}

  TAI_ImageMatrix_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TAI_ImageList>;

  TAI_ImageMatrix = class(TAI_ImageMatrix_Decl)
  private
    procedure BuildSnapshotProjection_HashList(SS_width, SS_height: Integer; imgList: TAI_ImageList; hList: THashObjectList); overload;
    procedure BuildSnapshot_HashList(imgList: TAI_ImageList; hList: THashObjectList); overload;
    procedure BuildDefinePrepareRaster_HashList(SS_width, SS_height: Integer; imgList: TAI_ImageList; hList: THashObjectList); overload;
    procedure BuildScaleSpace_HashList(SS_width, SS_height: Integer; imgList: TAI_ImageList; hList: THashObjectList); overload;

    procedure BuildSnapshotProjection_HashList(SS_width, SS_height: Integer; imgList: TAI_ImageList; RSeri: TRasterSerialized; hList: THashObjectList); overload;
    procedure BuildSnapshot_HashList(imgList: TAI_ImageList; RSeri: TRasterSerialized; hList: THashObjectList); overload;
    procedure BuildDefinePrepareRaster_HashList(SS_width, SS_height: Integer; imgList: TAI_ImageList; RSeri: TRasterSerialized; hList: THashObjectList); overload;
    procedure BuildScaleSpace_HashList(SS_width, SS_height: Integer; imgList: TAI_ImageList; RSeri: TRasterSerialized; hList: THashObjectList); overload;
  public
    UsedJpegForXML: Boolean;
    constructor Create;
    destructor Destroy; override;

    // scipt
    procedure RunScript(RSeri: TRasterSerialized; ScriptStyle: TTextStyle; condition_exp, process_exp: SystemString); overload;
    procedure RunScript(RSeri: TRasterSerialized; condition_exp, process_exp: SystemString); overload;
    procedure RunScript(ScriptStyle: TTextStyle; condition_exp, process_exp: SystemString); overload;
    procedure RunScript(condition_exp, process_exp: SystemString); overload;

    // import
    procedure SearchAndAddImageList(RSeri: TRasterSerialized; rootPath, filter: SystemString; includeSubdir, LoadImg: Boolean); overload;
    procedure SearchAndAddImageList(rootPath, filter: SystemString; includeSubdir, LoadImg: Boolean); overload;
    procedure ImportImageListAsFragment(RSeri: TRasterSerialized; imgList: TAI_ImageList; RemoveSource: Boolean); overload;
    procedure ImportImageListAsFragment(imgList: TAI_ImageList; RemoveSource: Boolean); overload;

    // image matrix stream
    procedure SaveToStream(stream: TCoreClassStream; SaveImg: Boolean; RasterSave_: TRasterSave); overload;
    procedure SaveToStream(stream: TCoreClassStream); overload;
    procedure LoadFromStream(stream: TCoreClassStream);

    // image matrix file
    procedure SaveToFile(fileName: SystemString; SaveImg: Boolean; RasterSave_: TRasterSave); overload;
    procedure SaveToFile(fileName: SystemString); overload;
    procedure LoadFromFile(fileName: SystemString);

    // image matrix scale
    procedure Scale(f: TGeoFloat);

    // clean
    procedure ClearDetector;
    procedure ClearSegmentation;
    procedure ClearPrepareRaster;

    // export
    procedure Export_PrepareRaster(outputPath: SystemString);
    procedure Export_DetectorRaster(outputPath: SystemString);
    procedure Export_Segmentation(outputPath: SystemString);
    procedure Build_XML(TokenFilter: SystemString; includeLabel, includePart, usedJpeg: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList); overload;
    procedure Build_XML(TokenFilter: SystemString; includeLabel, includePart: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList); overload;
    procedure Build_XML(includeLabel, includePart: Boolean; datasetName, comment, build_output_file, Prefix: SystemString; BuildFileList: TPascalStringList); overload;
    procedure Build_XML(includeLabel, includePart: Boolean; datasetName, comment, build_output_file: SystemString); overload;

    // statistics: Image
    function ImageCount: Integer;
    function ImageList: TImageList_Decl;
    function FindImageList(FileInfo: TPascalString): TAI_ImageList;
    function FoundNoTokenDefine(output: TMemoryRaster): Boolean; overload;
    function FoundNoTokenDefine: Boolean; overload;
    procedure AllTokens(output: TPascalStringList);

    // statistics: detector
    function DetectorDefineCount: Integer;
    function DetectorDefinePartCount: Integer;
    function DetectorTokens: TArrayPascalString;
    function ExistsDetectorToken(Token: TPascalString): Boolean;
    function GetDetectorTokenCount(Token: TPascalString): Integer;

    // statistics: segmentation
    procedure SegmentationTokens(output: TPascalStringList);
    function BuildSegmentationColorBuffer: TSegmentationColorList;
    procedure BuildMaskMerge(colors: TSegmentationColorList); overload;
    procedure BuildMaskMerge; overload;
    procedure LargeScale_BuildMaskMerge(RSeri: TRasterSerialized; colors: TSegmentationColorList);
    procedure ClearMaskMerge;

    // parallel extract image matrix
    function ExtractDetectorDefineAsSnapshotProjection(SS_width, SS_height: Integer): TMemoryRaster2DArray;
    function ExtractDetectorDefineAsSnapshot: TMemoryRaster2DArray;
    function ExtractDetectorDefineAsPrepareRaster(SS_width, SS_height: Integer): TMemoryRaster2DArray;
    function ExtractDetectorDefineAsScaleSpace(SS_width, SS_height: Integer): TMemoryRaster2DArray;

    // large-scale image matrix stream
    procedure LargeScale_SaveToStream(RSeri: TRasterSerialized; stream: TCoreClassStream; RasterSave_: TRasterSave); overload;
    procedure LargeScale_SaveToStream(RSeri: TRasterSerialized; stream: TCoreClassStream); overload;
    procedure LargeScale_LoadFromStream(RSeri: TRasterSerialized; stream: TCoreClassStream);

    // large-scale image matrix: file
    procedure LargeScale_SaveToFile(RSeri: TRasterSerialized; fileName: SystemString; RasterSave_: TRasterSave); overload;
    procedure LargeScale_SaveToFile(RSeri: TRasterSerialized; fileName: SystemString); overload;
    procedure LargeScale_LoadFromFile(RSeri: TRasterSerialized; fileName: SystemString);

    // large-scale image matrix extract
    function LargeScale_ExtractDetectorDefineAsSnapshotProjection(RSeri: TRasterSerialized; SS_width, SS_height: Integer): TMemoryRaster2DArray;
    function LargeScale_ExtractDetectorDefineAsSnapshot(RSeri: TRasterSerialized): TMemoryRaster2DArray;
    function LargeScale_ExtractDetectorDefineAsPrepareRaster(RSeri: TRasterSerialized; SS_width, SS_height: Integer): TMemoryRaster2DArray;
    function LargeScale_ExtractDetectorDefineAsScaleSpace(RSeri: TRasterSerialized; SS_width, SS_height: Integer): TMemoryRaster2DArray;

    // large-scale image matrix: Serialized And Recycle Memory
    procedure SerializedAndRecycleMemory(Serializ: TRasterSerialized);
    procedure UnserializedMemory(Serializ: TRasterSerialized);
  end;
{$ENDREGION 'image matrix'}
{$REGION 'global'}


const
  // zAI.conf define
  C_AI_Conf: SystemString = 'Z-AI.conf';
  // ext define
  C_ImageMatrix_Ext: SystemString = '.imgMat';
  C_ImageList_Ext: SystemString = '.imgDataset';
  C_Image_Ext: SystemString = '.img';
  C_OD_Ext: SystemString = '.svm_od';
  C_OD_Marshal_Ext: SystemString = '.svm_od_marshal';
  C_SP_Ext: SystemString = '.shape';
  C_Metric_Ext: SystemString = '.metric';
  C_LMetric_Ext: SystemString = '.large_metric';
  C_Learn_Ext: SystemString = '.learn';
  C_MMOD_Ext: SystemString = '.svm_dnn_od';
  C_RNIC_Ext: SystemString = '.RNIC';
  C_LRNIC_Ext: SystemString = '.LRNIC';
  C_GDCNIC_Ext: SystemString = '.GDCNIC';
  C_GNIC_Ext: SystemString = '.GNIC';
  C_SS_Ext: SystemString = '.SS';

var
  AI_Work_Path: U_String;
  AI_CFG_FILE: U_String;
  AI_ProductID: U_String;
  AI_UserKey: U_String;
  AI_Key_Server_Host: U_String;
  AI_Key_Server_Port: WORD;
  AI_Engine_Library: U_String;
  AI_Parallel_Count: Integer;
  AI_TrainingTool: U_String;
  AI_PackageTool: U_String;
  AI_ModelTool: U_String;
  AI_TrainingServer: U_String;
  AI_Configure_ReadyDone: Boolean;
  On_Script_RegisterProc: TAI_Image_Script_Register;

function WhereFileFromConfigure(const fileName: U_String): U_String;
function FileExistsFromConfigure(const fileName: U_String): Boolean;
procedure ReadAIConfig; overload;
procedure ReadAIConfig(ini: THashTextEngine); overload;
procedure WriteAIConfig;

procedure Build_XML_Dataset(xslFile, Name, comment, body: SystemString; build_output: TMemoryStream64);
procedure Build_XML_Style(build_output: TMemoryStream64);
procedure DrawSPLine(sp_desc: TVec2List; bp, ep: Integer; closeLine: Boolean; Color: TDEColor; d: TDrawEngine); overload;
procedure DrawSPLine(sp_desc: TArrayVec2; bp, ep: Integer; closeLine: Boolean; Color: TDEColor; d: TDrawEngine); overload;
procedure DrawFaceSP(sp_desc: TVec2List; Color: TDEColor; d: TDrawEngine); overload;
procedure DrawFaceSP(sp_desc: TArrayVec2; Color: TDEColor; d: TDrawEngine); overload;

{$ENDREGION 'global'}

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

var
  AI_Configure_Path: U_String;

procedure Init_AI_Common;
begin
{$IFDEF FPC}
  AI_Configure_Path := umlCurrentPath;
  AI_Work_Path := umlCurrentPath;
{$ELSE FPC}
  AI_Configure_Path := System.IOUtils.TPath.GetDocumentsPath;
  AI_Work_Path := System.IOUtils.TPath.GetLibraryPath;
{$ENDIF FPC}
  if IsMobile then
      AI_CFG_FILE := C_AI_Conf
  else
      AI_CFG_FILE := WhereFileFromConfigure(C_AI_Conf);

  AI_ProductID := '';
  AI_UserKey := '';
  AI_Key_Server_Host := 'zpascal.net';
  AI_Key_Server_Port := 7988;

  AI_Engine_Library := 'zAI_x64.dll';
  AI_Parallel_Count := CpuCount;

  AI_TrainingTool := 'TrainingTool.exe';
  AI_PackageTool := 'FilePackageTool.exe';
  AI_ModelTool := 'Z_AI_Model.exe';
  AI_TrainingServer := '127.0.0.1';

  AI_Configure_ReadyDone := False;

  On_Script_RegisterProc := nil;
end;

function WhereFileFromConfigure(const fileName: U_String): U_String;
var
  f: U_String;
begin
  f := umlGetFileName(fileName);

  if umlFileExists(umlCombineFileName(AI_Configure_Path, f)) then
      Result := umlCombineFileName(AI_Configure_Path, f)
  else if umlFileExists(umlCombineFileName(AI_Work_Path, f)) then
      Result := umlCombineFileName(AI_Work_Path, f)
  else if umlFileExists(umlCombineFileName(umlCurrentPath, f)) then
      Result := umlCombineFileName(umlCurrentPath, f)
  else
      Result := umlCombineFileName(AI_Configure_Path, f);
end;

function FileExistsFromConfigure(const fileName: U_String): Boolean;
begin
  Result := umlFileExists(WhereFileFromConfigure(fileName));
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
  DoStatus('read Z-AI configure "%s"', [AI_CFG_FILE.Text]);
end;

procedure ReadAIConfig(ini: THashTextEngine);
  function r_ai(Name, fn: U_String): U_String;
  begin
    Result := ini.GetDefaultValue('AI', Name, fn);
    if not umlExistsChar(Result, '/\') then
        Result := WhereFileFromConfigure(Result);
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

  AI_Configure_ReadyDone := True;
end;

procedure WriteAIConfig;
var
  ini: THashTextEngine;
  procedure w_ai(Name, fn: U_String);
  begin
    if umlExistsChar(fn, '/\') then
      begin
        if WhereFileFromConfigure(fn).Same(fn) then
            ini.SetDefaultValue('AI', Name, umlGetFileName(fn))
        else
            ini.SetDefaultValue('AI', Name, fn);
      end
    else if FileExistsFromConfigure(fn) then
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
    DoStatus('write Z-AI configure "%s"', [AI_CFG_FILE.Text]);
  except
    TCoreClassThread.Sleep(100);
    WriteAIConfig;
  end;
  disposeObject(ini);
end;

procedure Build_XML_Dataset(xslFile, Name, comment, body: SystemString; build_output: TMemoryStream64);
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

procedure DrawSPLine(sp_desc: TVec2List; bp, ep: Integer; closeLine: Boolean; Color: TDEColor; d: TDrawEngine);
var
  i: Integer;
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  for i := bp to ep do
      vl.Add(sp_desc[i]^);

  d.DrawOutSideSmoothPL(False, vl, closeLine, Color, 2);
  disposeObject(vl);
end;

procedure DrawSPLine(sp_desc: TArrayVec2; bp, ep: Integer; closeLine: Boolean; Color: TDEColor; d: TDrawEngine);
var
  i: Integer;
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  for i := bp to ep do
      vl.Add(sp_desc[i]);

  d.DrawOutSideSmoothPL(False, vl, closeLine, Color, 2);
  disposeObject(vl);
end;

procedure DrawFaceSP(sp_desc: TVec2List; Color: TDEColor; d: TDrawEngine);
begin
  if sp_desc.Count <> 68 then
      exit;
  DrawSPLine(sp_desc, 0, 16, False, Color, d);
  DrawSPLine(sp_desc, 17, 21, False, Color, d);
  DrawSPLine(sp_desc, 22, 26, False, Color, d);
  DrawSPLine(sp_desc, 27, 30, False, Color, d);
  DrawSPLine(sp_desc, 31, 35, False, Color, d);
  d.DrawLine(sp_desc[31]^, sp_desc[27]^, Color, 1);
  d.DrawLine(sp_desc[35]^, sp_desc[27]^, Color, 1);
  d.DrawLine(sp_desc[31]^, sp_desc[30]^, Color, 1);
  d.DrawLine(sp_desc[35]^, sp_desc[30]^, Color, 1);
  DrawSPLine(sp_desc, 36, 41, True, Color, d);
  DrawSPLine(sp_desc, 42, 47, True, Color, d);
  DrawSPLine(sp_desc, 48, 59, True, Color, d);
  DrawSPLine(sp_desc, 60, 67, True, Color, d);
end;

procedure DrawFaceSP(sp_desc: TArrayVec2; Color: TDEColor; d: TDrawEngine);
begin
  if length(sp_desc) <> 68 then
      exit;
  DrawSPLine(sp_desc, 0, 16, False, Color, d);
  DrawSPLine(sp_desc, 17, 21, False, Color, d);
  DrawSPLine(sp_desc, 22, 26, False, Color, d);
  DrawSPLine(sp_desc, 27, 30, False, Color, d);
  DrawSPLine(sp_desc, 31, 35, False, Color, d);
  d.DrawLine(sp_desc[31], sp_desc[27], Color, 1);
  d.DrawLine(sp_desc[35], sp_desc[27], Color, 1);
  d.DrawLine(sp_desc[31], sp_desc[30], Color, 1);
  d.DrawLine(sp_desc[35], sp_desc[30], Color, 1);
  DrawSPLine(sp_desc, 36, 41, True, Color, d);
  DrawSPLine(sp_desc, 42, 47, True, Color, d);
  DrawSPLine(sp_desc, 48, 59, True, Color, d);
  DrawSPLine(sp_desc, 60, 67, True, Color, d);
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

procedure TSegmentationColorList.DoSegColor(Color: TRColor; var Classify: TSegClassify);
var
  ID: WORD;
begin
  Classify := 0;
  if IsIgnoredBorder(Color) then
      exit;
  if Color = RColor(0, 0, 0, $FF) then
      exit;
  if GetColorID(Color, 0, ID) then
      Classify := ID;
end;

constructor TSegmentationColorList.Create;
begin
  inherited Create;
  BuildBorderColor;
end;

destructor TSegmentationColorList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSegmentationColorList.BuildBorderColor;
var
  p: PSegmentationColor;
begin
  new(p);
  p^.Token := 'ignored border';
  // border color
  p^.Color := RColor($FF, $FF, $FF, $FF);
  // $FFFF index to pixel will be ignored when computing gradients.
  p^.ID := $FFFF;
  inherited Add(p);
end;

procedure TSegmentationColorList.Delete(index: Integer);
var
  p: PSegmentationColor;
begin
  p := Items[index];
  p^.Token := '';
  dispose(p);
  inherited Delete(index);
end;

procedure TSegmentationColorList.Clear;
var
  i: Integer;
  p: PSegmentationColor;
begin
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      p^.Token := '';
      dispose(p);
    end;
  inherited Clear;
end;

procedure TSegmentationColorList.AddColor(const Token: TPascalString; const Color: TRColor);
var
  p: PSegmentationColor;
begin
  if ExistsColor(Color) then
      exit;
  new(p);
  p^.Token := Token;
  p^.Color := Color;
  p^.ID := Count;
  while (p^.ID = 0) or (ExistsID(p^.ID)) do
      inc(p^.ID);
  inherited Add(p);
end;

function TSegmentationColorList.IsIgnoredBorder(const c: TRColor): Boolean;
begin
  Result := c = RColor($FF, $FF, $FF, $FF);
end;

function TSegmentationColorList.IsIgnoredBorder(const ID: WORD): Boolean;
begin
  Result := ID = $FFFF;
end;

function TSegmentationColorList.IsIgnoredBorder(const Token: TPascalString): Boolean;
begin
  Result := Token.Same('ignored border');
end;

function TSegmentationColorList.ExistsColor(const c: TRColor): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if Items[i]^.Color = c then
        exit;
  Result := False;
end;

function TSegmentationColorList.ExistsID(const ID: WORD): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if Items[i]^.ID = ID then
        exit;
  Result := False;
end;

function TSegmentationColorList.GetColorID(const c: TRColor; const def: WORD; var output: WORD): Boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i]^.Color = c then
      begin
        output := Items[i]^.ID;
        Result := True;
        exit;
      end;
  output := def;
  Result := False;
end;

function TSegmentationColorList.GetIDColor(const ID: WORD; const def: TRColor; var output: TRColor): Boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i]^.ID = ID then
      begin
        output := Items[i]^.Color;
        Result := True;
        exit;
      end;
  output := def;
  Result := False;
end;

function TSegmentationColorList.GetIDColorAndToken(const ID: WORD; const def_color: TRColor; const def_token: TPascalString;
  var output_color: TRColor; var output_token: TPascalString): Boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i]^.ID = ID then
      begin
        output_color := Items[i]^.Color;
        output_token := Items[i]^.Token;
        Result := True;
        exit;
      end;
  output_color := def_color;
  output_token := def_token;
  Result := False;
end;

function TSegmentationColorList.GetColorToken(const c: TRColor; const def: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := def;
  for i := 0 to Count - 1 do
    if Items[i]^.Color = c then
      begin
        Result := Items[i]^.Token;
        exit;
      end;
end;

function TSegmentationColorList.GetColorToken(const c: TRColor): TPascalString;
begin
  Result := GetColorToken(c, '');
end;

function TSegmentationColorList.GetTokenColor(const Token: TPascalString; const def: TRColor): TRColor;
var
  i: Integer;
begin
  Result := def;
  for i := 0 to Count - 1 do
    if Token.Same(@Items[i]^.Token) then
      begin
        Result := Items[i]^.Color;
        exit;
      end;
end;

function TSegmentationColorList.GetTokenColor(const Token: TPascalString): TRColor;
begin
  Result := GetTokenColor(Token, RColor(0, 0, 0, 0));
end;

procedure TSegmentationColorList.BuildLabelViewer(input, output: TMemoryRaster; LabColor, BoxColor: TRColor);
var
  s: TColorSegmentation;
  i: Integer;
  pool: TSegPool;
  id_color: TRColor;
  id_token: TPascalString;
  R: TRect;
  tmp: TMemoryRaster;
begin
  s := TColorSegmentation.Create(input);
  s.OnSegColor := {$IFDEF FPC}@{$ENDIF FPC}DoSegColor;
  s.BuildSegmentation;
  s.RemoveNoise(500);
  s.MergeOverlapSegmentation;

  tmp := NewRaster();
  tmp.SetSize(output.Width, output.Height, RColor(0, 0, 0, 0));
  for i := 0 to s.Count - 1 do
    begin
      pool := s[i];

      if GetIDColorAndToken(pool.Classify, RColor(0, 0, 0, $FF), '', id_color, id_token) then
        begin
          R := pool.BoundsRect;
          pool.FillTo(tmp, id_color);
        end;
    end;
  tmp.FillNoneBGColorBorder(RColor(0, 0, 0, 0), RColor($FF, 0, 0), 5);
  FastBlur(tmp, 3, tmp.BoundsRect);
  tmp.ProjectionTo(output, tmp.BoundsV2Rect4, output.BoundsV2Rect4, True, 0.8);
  disposeObject(tmp);

  for i := 0 to s.Count - 1 do
    begin
      pool := s[i];

      if GetIDColorAndToken(pool.Classify, RColor(0, 0, 0, $FF), '', id_color, id_token) then
        begin
          R := pool.BoundsRect;
          output.DrawText(id_token, R.Left + 7, R.Top + 7, 14, RColor(0, 0, 0, $7F));
          output.DrawText(id_token, R.Left + 5, R.Top + 5, 14, LabColor);
          output.DrawRect(RectAdd(RectV2(R), Vec2(2, 2)), RColor(0, 0, 0, $7F));
          output.DrawRect(R, BoxColor);
        end;
    end;

  disposeObject(s);
end;

procedure TSegmentationColorList.SaveToStream(stream: TCoreClassStream);
var
  d, nd: TDataFrameEngine;
  i: Integer;
  p: PSegmentationColor;
begin
  d := TDataFrameEngine.Create;

  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      nd := TDataFrameEngine.Create;
      nd.WriteString(p^.Token);
      nd.WriteCardinal(p^.Color);
      nd.WriteWORD(p^.ID);
      d.WriteDataFrame(nd);
      disposeObject(nd);
    end;

  d.EncodeTo(stream, False);
  disposeObject(d);
end;

procedure TSegmentationColorList.LoadFromStream(stream: TCoreClassStream);
var
  d, nd: TDataFrameEngine;
  i: Integer;
  p: PSegmentationColor;
begin
  Clear;
  d := TDataFrameEngine.Create;
  d.DecodeFrom(stream, False);

  while d.Reader.NotEnd do
    begin
      nd := TDataFrameEngine.Create;
      d.Reader.ReadDataFrame(nd);
      new(p);
      p^.Token := nd.Reader.ReadString;
      p^.Color := nd.Reader.ReadCardinal;
      p^.ID := nd.Reader.ReadWord;
      Add(p);
      disposeObject(nd);
    end;

  disposeObject(d);
end;

procedure TSegmentationColorList.SaveToFile(fileName: TPascalString);
var
  fs: TMemoryStream64;
begin
  fs := TMemoryStream64.Create;
  SaveToStream(fs);
  fs.SaveToFile(fileName);
  disposeObject(fs);
end;

procedure TSegmentationColorList.LoadFromFile(fileName: TPascalString);
var
  fs: TMemoryStream64;
begin
  fs := TMemoryStream64.Create;
  fs.LoadFromFile(fileName);
  fs.Position := 0;
  LoadFromStream(fs);
  disposeObject(fs);
end;

procedure TSegmentationMasks.MergePixelToRaster(segMask: PSegmentationMask; colors: TSegmentationColorList);
var
  fg: TRColor;
{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    i: Integer;
  begin
    for i := 0 to segMask^.Raster.Width - 1 do
      if segMask^.Raster[i, pass] = segMask^.FrontColor then
          MaskMergeRaster[i, pass] := fg;
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor;
  var
    pass: Integer;
    i: Integer;
  begin
    for pass := 0 to segMask^.Raster.Height - 1 do
      for i := 0 to segMask^.Raster.Width - 1 do
        if segMask^.Raster[i, pass] = segMask^.FrontColor then
            MaskMergeRaster[i, pass] := fg;
  end;
{$ENDIF parallel}


begin
  if (MaskMergeRaster.Width <> segMask^.Raster.Width) or (MaskMergeRaster.Height <> segMask^.Raster.Height) then
      exit;
  fg := colors.GetTokenColor(segMask^.Token, 0);
  if fg = 0 then
      exit;

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, segMask^.Raster.Height - 1);
{$ELSE FPC}
  TParallel.for(0, segMask^.Raster.Height - 1, procedure(pass: Integer)
    var
      i: Integer;
    begin
      for i := 0 to segMask^.Raster.Width - 1 do
        if segMask^.Raster[i, pass] = segMask^.FrontColor then
            MaskMergeRaster[i, pass] := fg;
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
end;

constructor TSegmentationMasks.Create(OwnerImage_: TAI_Image);
begin
  inherited Create;
  OwnerImage := OwnerImage_;
  MaskMergeRaster := NewRaster();
end;

destructor TSegmentationMasks.Destroy;
begin
  Clear();
  disposeObject(MaskMergeRaster);
  inherited Destroy;
end;

procedure TSegmentationMasks.Remove(p: PSegmentationMask);
begin
  inherited Remove(p);
  disposeObject(p^.Raster);
  dispose(p);
end;

procedure TSegmentationMasks.Delete(index: Integer);
var
  p: PSegmentationMask;
begin
  p := Items[index];
  p^.Token := '';
  disposeObject(p^.Raster);
  dispose(p);
  inherited Delete(index);
end;

procedure TSegmentationMasks.Clear;
var
  i: Integer;
  p: PSegmentationMask;
begin
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      p^.Token := '';
      disposeObject(p^.Raster);
      dispose(p);
    end;
  inherited Clear;
  MaskMergeRaster.Reset;
end;

procedure TSegmentationMasks.SaveToStream(stream: TCoreClassStream);
var
  d, nd: TDataFrameEngine;
  i: Integer;
  p: PSegmentationMask;
  m64: TMemoryStream64;
begin
  d := TDataFrameEngine.Create;

  for i := 0 to Count - 1 do
    begin
      // 0: bk color
      // 1: fg color
      // 2: name
      // 3: raster
      nd := TDataFrameEngine.Create;
      p := Items[i];
      nd.WriteCardinal(p^.BackgroundColor);
      nd.WriteCardinal(p^.FrontColor);
      nd.WriteString(p^.Token);

      m64 := TMemoryStream64.Create;
      p^.Raster.SaveToBmp32Stream(m64);
      nd.WriteStream(m64);
      disposeObject(m64);

      d.WriteDataFrame(nd);

      disposeObject(nd);
    end;

  d.EncodeAsZLib(stream, True);
  disposeObject(d);
end;

procedure TSegmentationMasks.LoadFromStream(stream: TCoreClassStream);
var
  d, nd: TDataFrameEngine;
  i: Integer;
  p: PSegmentationMask;
  m64: TMemoryStream64;
begin
  d := TDataFrameEngine.Create;
  d.DecodeFrom(stream, True);

  while d.Reader.NotEnd do
    begin
      nd := TDataFrameEngine.Create;
      d.Reader.ReadDataFrame(nd);

      // 0: bk color
      // 1: fg color
      // 2: name
      // 3: raster

      new(p);
      p^.BackgroundColor := nd.Reader.ReadCardinal;
      p^.FrontColor := nd.Reader.ReadCardinal;
      p^.Token := nd.Reader.ReadString;

      m64 := TMemoryStream64.Create;
      nd.Reader.ReadStream(m64);
      m64.Position := 0;
      p^.Raster := NewRaster();
      p^.Raster.LoadFromStream(m64);
      disposeObject(m64);

      Add(p);

      disposeObject(nd);
    end;

  disposeObject(d);
end;

procedure TSegmentationMasks.BuildSegmentationMask(Width, Height: Integer; polygon: T2DPolygon; buildBG_color, buildFG_color: TRColor; Token: TPascalString);
var
  p: PSegmentationMask;
  R: TRectV2;
{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    i: Integer;
  begin
    for i := 0 to Width - 1 do
      if PointInRect(Vec2(i, pass), R) and polygon.InHere(Vec2(i, pass)) then
          p^.Raster.Pixel[i, pass] := p^.FrontColor;
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor;
  var
    pass, i: Integer;
  begin
    for pass := 0 to Height - 1 do
      begin
        for i := 0 to Width - 1 do
          if PointInRect(Vec2(i, pass), R) and polygon.InHere(Vec2(i, pass)) then
              p^.Raster.Pixel[i, pass] := p^.FrontColor;
      end;
  end;
{$ENDIF parallel}


begin
  new(p);
  p^.BackgroundColor := buildBG_color;
  p^.FrontColor := buildFG_color;
  p^.Token := Token;
  p^.Raster := NewRaster();
  p^.Raster.SetSize(Width, Height, p^.BackgroundColor);
  R := polygon.BoundBox();

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Height - 1);
{$ELSE}
  TParallel.for(0, Height - 1, procedure(pass: Integer)
    var
      i: Integer;
    begin
      for i := 0 to Width - 1 do
        if PointInRect(Vec2(i, pass), R) and polygon.InHere(Vec2(i, pass)) then
            p^.Raster.Pixel[i, pass] := p^.FrontColor;
    end);
{$ENDIF FPC}
{$ELSE}
  DoFor();
{$ENDIF parallel}
  Add(p);
end;

procedure TSegmentationMasks.BuildSegmentationMask(Width, Height: Integer; polygon: T2DPolygonGraph; buildBG_color, buildFG_color: TRColor; Token: TPascalString);
var
  p: PSegmentationMask;
  R: TRectV2;
{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    i: Integer;
  begin
    for i := 0 to Width - 1 do
      if PointInRect(Vec2(i, pass), R) and polygon.InHere(Vec2(i, pass)) then
          p^.Raster.Pixel[i, pass] := p^.FrontColor;
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor;
  var
    pass, i: Integer;
  begin
    for pass := 0 to Height - 1 do
      begin
        for i := 0 to Width - 1 do
          if PointInRect(Vec2(i, pass), R) and polygon.InHere(Vec2(i, pass)) then
              p^.Raster.Pixel[i, pass] := p^.FrontColor;
      end;
  end;
{$ENDIF parallel}


begin
  new(p);
  p^.BackgroundColor := buildBG_color;
  p^.FrontColor := buildFG_color;
  p^.Token := Token;
  p^.Raster := NewRaster();
  p^.Raster.SetSize(Width, Height, p^.BackgroundColor);
  R := polygon.BoundBox();

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Height - 1);
{$ELSE}
  TParallel.for(0, Height - 1, procedure(pass: Integer)
    var
      i: Integer;
    begin
      for i := 0 to Width - 1 do
        if PointInRect(Vec2(i, pass), R) and polygon.InHere(Vec2(i, pass)) then
            p^.Raster.Pixel[i, pass] := p^.FrontColor;
    end);
{$ENDIF FPC}
{$ELSE}
  DoFor();
{$ENDIF parallel}
  Add(p);
end;

procedure TSegmentationMasks.BuildSegmentationMask(Width, Height: Integer; sour: TMemoryRaster; sampler_FG_Color, buildBG_color, buildFG_color: TRColor; Token: TPascalString);
var
  p: PSegmentationMask;
  i, j: Integer;
begin
  if not sour.ExistsColor(sampler_FG_Color) then
      exit;
  new(p);
  p^.BackgroundColor := buildBG_color;
  p^.FrontColor := buildFG_color;
  p^.Token := Token;
  p^.Raster := NewRaster();
  p^.Raster.SetSize(Width, Height, p^.BackgroundColor);

  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
      if PointInRect(i, j, 0, 0, sour.Width, sour.Height) then
        if sour.Pixel[i, j] = sampler_FG_Color then
            p^.Raster.Pixel[i, j] := p^.FrontColor;
  Add(p);
end;

procedure TSegmentationMasks.BuildMaskMerge(colors: TSegmentationColorList);
var
  i: Integer;
begin
  MaskMergeRaster.SetSize(OwnerImage.Raster.Width, OwnerImage.Raster.Height, RColor(0, 0, 0, $FF));
  for i := 0 to Count - 1 do
      MergePixelToRaster(Items[i], colors);
  MaskMergeRaster.FillNoneBGColorBorder(RColor(0, 0, 0, $FF), RColor($FF, $FF, $FF, $FF), 4);
end;

procedure TSegmentationMasks.SegmentationTokens(output: TPascalStringList);
var
  i: Integer;
  p: PSegmentationMask;
begin
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      if output.ExistsValue(p^.Token) < 0 then
          output.Add(p^.Token);
    end;
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
  FOP_RT.RegOpM('Blur', {$IFDEF FPC}@{$ENDIF FPC}OP_Image_Blur);

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
  Result := Raster.Width;
end;

function TAI_Image.OP_Image_GetHeight(var Param: TOpParam): Variant;
begin
  Result := Raster.Height;
end;

function TAI_Image.OP_Image_GetDetector(var Param: TOpParam): Variant;
begin
  Result := DetectorDefineList.Count;
end;

function TAI_Image.OP_Detector_GetLabel(var Param: TOpParam): Variant;
begin
  Result := GetDetectorTokenCount(Param[0]);
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
          TDETexture(Raster).ReleaseGPUMemory;
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
          TDETexture(Raster).ReleaseGPUMemory;
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
          TDETexture(Raster).ReleaseGPUMemory;
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
          TDETexture(Raster).ReleaseGPUMemory;
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
          TDETexture(Raster).ReleaseGPUMemory;
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
          TDETexture(Raster).ReleaseGPUMemory;
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
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.Count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        Sepia32(DetectorDefineList[i].PrepareRaster, Param[0]);
  Result := True;
end;

function TAI_Image.OP_Image_Blur(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  DoStatus('image script on Sepia');

  if not Raster.Empty then
    begin
      GaussianBlur(Raster, Param[0], Raster.BoundsRect);
      if Raster is TDETexture then
          TDETexture(Raster).ReleaseGPUMemory;
    end;

  for i := 0 to DetectorDefineList.Count - 1 do
    if not DetectorDefineList[i].PrepareRaster.Empty then
        GaussianBlur(DetectorDefineList[i].PrepareRaster, Param[0], DetectorDefineList[i].PrepareRaster.BoundsRect);
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
          TDETexture(Raster).ReleaseGPUMemory;
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
  ClearDetector;
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
  DetectorDefineList := TEditorDetectorDefineList.Create;
  SegmentationMaskList := TSegmentationMasks.Create(Self);
  Raster := NewRaster();
  FOP_RT := nil;
  FOP_RT_RunDeleted := False;
end;

destructor TAI_Image.Destroy;
begin
  ClearDetector;
  ClearSegmentation;
  disposeObject(DetectorDefineList);
  disposeObject(SegmentationMaskList);
  disposeObject(Raster);
  if FOP_RT <> nil then
      disposeObject(FOP_RT);
  inherited Destroy;
end;

function TAI_Image.RunExpCondition(RSeri: TRasterSerialized; ScriptStyle: TTextStyle; exp: SystemString): Boolean;
begin
  CheckAndRegOPRT;
  DoStatusNoLn('Image (%d * %d, detector:%d) EvaluateExpression: %s', [Raster.Width, Raster.Height, DetectorDefineList.Count, exp]);

  if RSeri <> nil then
      UnserializedMemory(RSeri);

  Result := EvaluateExpressionValue(False, ScriptStyle, exp, FOP_RT);

  if Result then
      DoStatusNoLn(' = yes.')
  else
      DoStatusNoLn(' = no.');
  DoStatusNoLn;

  if RSeri <> nil then
      SerializedAndRecycleMemory(RSeri);
end;

function TAI_Image.RunExpProcess(RSeri: TRasterSerialized; ScriptStyle: TTextStyle; exp: SystemString): Boolean;
begin
  CheckAndRegOPRT;

  if RSeri <> nil then
      UnserializedMemory(RSeri);

  Result := EvaluateExpressionValue(False, ScriptStyle, exp, FOP_RT);

  if RSeri <> nil then
      SerializedAndRecycleMemory(RSeri);
end;

function TAI_Image.GetExpFunctionList: TPascalStringList;
begin
  CheckAndRegOPRT;
  Result := TPascalStringList.Create;
  FOP_RT.ProcList.GetNameList(Result);
end;

procedure TAI_Image.RemoveDetectorFromRect(edge: TGeoFloat; R: TRectV2);
var
  i: Integer;
  det: TAI_DetectorDefine;
  r1, r2: TRectV2;
begin
  i := 0;
  r1 := RectEdge(ForwardRect(R), edge);
  while i < DetectorDefineList.Count do
    begin
      det := DetectorDefineList[i];
      r2 := RectV2(det.R);
      if RectWithinRect(r1, r2) or RectWithinRect(r2, r1) or RectToRectIntersect(r2, r1) or RectToRectIntersect(r1, r2) then
        begin
          disposeObject(det);
          DetectorDefineList.Delete(i);
        end
      else
          inc(i);
    end;
end;

procedure TAI_Image.RemoveDetectorFromRect(R: TRectV2);
begin
  RemoveDetectorFromRect(1, R);
end;

procedure TAI_Image.ClearDetector;
var
  i: Integer;
begin
  for i := 0 to DetectorDefineList.Count - 1 do
      disposeObject(DetectorDefineList[i]);
  DetectorDefineList.Clear;
end;

procedure TAI_Image.ClearSegmentation;
begin
  SegmentationMaskList.Clear;
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
  segMask: PSegmentationMask;
  tmp1, tmp2: TMemoryRaster;
begin
  d := TDrawEngine.Create;
  d.Options := [];
  output.Assign(Raster);

  if SegmentationMaskList.Count > 0 then
    begin
      tmp1 := NewRaster();
      tmp1.SetSize(Raster.Width, Raster.Height, RColor(0, 0, 0, 0));
      for i := 0 to SegmentationMaskList.Count - 1 do
        begin
          segMask := SegmentationMaskList[i];
          tmp2 := NewRaster();
          tmp2.Assign(segMask^.Raster);
          tmp2.ColorReplace(segMask^.BackgroundColor, RColor(0, 0, 0, 0));
          tmp2.DrawTo(tmp1);
          disposeObject(tmp2);
        end;
      tmp1.FillNoneBGColorBorder(RColor(0, 0, 0, 0), RColor($FF, 0, 0, $FF), 3);
      FastBlur(tmp1, 5, tmp1.BoundsRect);
      tmp1.ProjectionTo(output, tmp1.BoundsV2Rect4, output.BoundsV2Rect4, True, 0.8);
      disposeObject(tmp1);
    end;

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
          d.DrawText(DetDef.Token, 14, RectV2(DetDef.R), DEColor(1, 1, 1, 1), False);
          d.EndCaptureShadow;
        end;
    end;

  d.Flush;
  disposeObject(d);
end;

function TAI_Image.FoundNoTokenDefine(output: TMemoryRaster; Color: TDEColor): Boolean;
var
  i: Integer;
  d: TDrawEngine;
  DetDef: TAI_DetectorDefine;
  segMask: PSegmentationMask;
  nm: TMemoryRaster;
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
              d.FillBox(RectV2(DetDef.R), Color);
              d.BeginCaptureShadow(Vec2(1, 1), 0.9);
              d.DrawText('ERROR!!' + #13#10 + 'NULL TOKEN', 12, RectV2(DetDef.R), DEColorInv(Color), True);
              d.EndCaptureShadow;
              Result := True;
            end;
        end;
      d.Flush;
      disposeObject(d);
      for i := 0 to SegmentationMaskList.Count - 1 do
        begin
          segMask := SegmentationMaskList[i];
          if segMask^.Token = '' then
            begin
              nm.Assign(segMask^.Raster);
              nm.ColorReplace(segMask^.BackgroundColor, RColor(0, 0, 0, 0));
              nm.ColorReplace(segMask^.FrontColor, RColor(Color));
              FastBlur(nm, 3, nm.BoundsRect);
              nm.ProjectionTo(output, nm.BoundsV2Rect4, output.BoundsV2Rect4, True, 0.5);
              Result := True;
            end;
        end;
    end
  else
      Result := FoundNoTokenDefine();
end;

function TAI_Image.FoundNoTokenDefine: Boolean;
var
  i: Integer;
  DetDef: TAI_DetectorDefine;
  segMask: PSegmentationMask;
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

  for i := 0 to SegmentationMaskList.Count - 1 do
    begin
      segMask := SegmentationMaskList[i];
      if segMask^.Token = '' then
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

  m64 := TMemoryStream64.Create;
  SegmentationMaskList.SaveToStream(m64);
  de.WriteStream(m64);
  disposeObject(m64);

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

  if de.Reader.NotEnd then
    begin
      m64 := TMemoryStream64.Create;
      de.Reader.ReadStream(m64);
      m64.Position := 0;
      SegmentationMaskList.LoadFromStream(m64);
      disposeObject(m64);
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
  ClearDetector;
  ClearSegmentation;
end;

procedure TAI_Image.LoadPicture(fileName: SystemString);
begin
  disposeObject(Raster);
  Raster := NewRasterFromFile(fileName);
  ClearDetector;
  ClearSegmentation;
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

  for i := 0 to SegmentationMaskList.Count - 1 do
      SegmentationMaskList[i]^.Raster.NoLineScale(f);
end;

function TAI_Image.ExistsDetectorToken(Token: TPascalString): Boolean;
begin
  Result := GetDetectorTokenCount(Token) > 0;
end;

function TAI_Image.GetDetectorTokenCount(Token: TPascalString): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to DetectorDefineList.Count - 1 do
    if umlMultipleMatch(Token, DetectorDefineList[i].Token) then
        inc(Result);
end;

procedure TAI_Image.SerializedAndRecycleMemory(Serializ: TRasterSerialized);
var
  i: Integer;
begin
  for i := 0 to DetectorDefineList.Count - 1 do
      DetectorDefineList[i].PrepareRaster.SerializedAndRecycleMemory(Serializ);

  for i := 0 to SegmentationMaskList.Count - 1 do
      SegmentationMaskList[i]^.Raster.SerializedAndRecycleMemory(Serializ);

  SegmentationMaskList.MaskMergeRaster.SerializedAndRecycleMemory(Serializ);

  Raster.SerializedAndRecycleMemory(Serializ);
end;

procedure TAI_Image.UnserializedMemory(Serializ: TRasterSerialized);
var
  i: Integer;
begin
  for i := 0 to DetectorDefineList.Count - 1 do
    if DetectorDefineList[i].PrepareRaster.Empty then
        DetectorDefineList[i].PrepareRaster.UnserializedMemory(Serializ);

  for i := 0 to SegmentationMaskList.Count - 1 do
    if SegmentationMaskList[i]^.Raster.Empty then
        SegmentationMaskList[i]^.Raster.UnserializedMemory(Serializ);

  if SegmentationMaskList.MaskMergeRaster.Empty then
      SegmentationMaskList.MaskMergeRaster.UnserializedMemory(Serializ);

  if Raster.Empty then
      Raster.UnserializedMemory(Serializ);
end;

constructor TAI_ImageList.Create;
begin
  inherited Create;
  UsedJpegForXML := True;
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
  Delete(index, True);
end;

procedure TAI_ImageList.Delete(index: Integer; freeObj_: Boolean);
begin
  if index >= 0 then
    begin
      if freeObj_ then
          disposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TAI_ImageList.Remove(img: TAI_Image);
begin
  Remove(img, True);
end;

procedure TAI_ImageList.Remove(img: TAI_Image; freeObj_: Boolean);
var
  i: Integer;
begin
  i := 0;
  while i < Count do
    begin
      if Items[i] = img then
          Delete(i, freeObj_)
      else
          inc(i);
    end;
end;

procedure TAI_ImageList.Clear;
begin
  Clear(True);
end;

procedure TAI_ImageList.Clear(freeObj_: Boolean);
var
  i: Integer;
begin
  if freeObj_ then
    for i := 0 to Count - 1 do
        disposeObject(Items[i]);
  inherited Clear;
end;

procedure TAI_ImageList.ClearDetector;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].ClearDetector;
end;

procedure TAI_ImageList.ClearSegmentation;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].ClearSegmentation;
end;

procedure TAI_ImageList.ClearPrepareRaster;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].ClearPrepareRaster;
end;

procedure TAI_ImageList.RunScript(RSeri: TRasterSerialized; ScriptStyle: TTextStyle; condition_exp, process_exp: SystemString);
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

      if img.RunExpCondition(RSeri, ScriptStyle, condition_exp) then
          img.RunExpProcess(RSeri, ScriptStyle, process_exp);
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

procedure TAI_ImageList.RunScript(RSeri: TRasterSerialized; condition_exp, process_exp: SystemString);
begin
  RunScript(RSeri, tsPascal, condition_exp, process_exp);
end;

procedure TAI_ImageList.RunScript(ScriptStyle: TTextStyle; condition_exp, process_exp: SystemString);
begin
  RunScript(nil, ScriptStyle, condition_exp, process_exp);
end;

procedure TAI_ImageList.RunScript(condition_exp, process_exp: SystemString);
begin
  RunScript(tsPascal, condition_exp, process_exp);
end;

procedure TAI_ImageList.DrawTo(output: TMemoryRaster; maxSampler: Integer);
var
  rp: TRectPacking;
  displaySampler: Integer;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
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
    for pass := 0 to displaySampler do
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
        d.DrawPicture(mr, mr.BoundsRectV2, rp[i]^.Rect, 1.0);
      end;

    d.BeginCaptureShadow(Vec2(3, 3), 0.9);
    d.DrawText(PFormat('display pictures %d/%d ', [displaySampler, Count - 1]), 36, RectEdge(d.ScreenRect, -10), DEColor(1, 1, 1, 1), False);
    d.EndCaptureShadow;

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

  displaySampler := ifThen(maxSampler <= 0, Count - 1, Min(maxSampler, Count - 1));

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, displaySampler);
{$ELSE FPC}
  TParallel.for(0, displaySampler, procedure(pass: Integer)
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

procedure TAI_ImageList.DrawTo(output: TMemoryRaster);
begin
  DrawTo(output, 0);
end;

procedure TAI_ImageList.DrawToPictureList(d: TDrawEngine; Margins: TGeoFloat; destOffset: TDEVec; alpha: TDEFloat);
var
  rList: TMemoryRasterList;
  i: Integer;
begin
  rList := TMemoryRasterList.Create;
  for i := 0 to Count - 1 do
      rList.Add(Items[i].Raster);

  d.DrawPicturePackingInScene(rList, Margins, destOffset, alpha);
  disposeObject(rList);
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

      Result.SetSizeF(rp.MaxWidth, rp.MaxHeight, RColorF(0, 0, 0, 1));
      d := TDrawEngine.Create;
      d.ViewOptions := [];
      d.Rasterization.SetWorkMemory(Result);

      for i := 0 to rp.Count - 1 do
        begin
          mr := TMemoryRaster(rp[i]^.Data2);
          d.DrawPicture(mr, mr.BoundsRectV2, rp[i]^.Rect, 0, 1.0);
        end;

      d.Flush;
      disposeObject(d);
      disposeObject(rp);
    end;
end;

procedure TAI_ImageList.CalibrationNullToken(Token: SystemString);
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
  p: PSegmentationMask;
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
        end;

      for j := 0 to imgData.SegmentationMaskList.Count - 1 do
        begin
          p := imgData.SegmentationMaskList[j];
          p^.Token := umlTrimSpace(p^.Token);
          if p^.Token = '' then
              p^.Token := Token;
        end;
    end;
end;

procedure TAI_ImageList.Scale(f: TGeoFloat);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Scale(f);
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

procedure TAI_ImageList.AddPicture(stream: TCoreClassStream);
var
  img: TAI_Image;
begin
  img := TAI_Image.Create(Self);
  disposeObject(img.Raster);
  try
    img.Raster := NewRasterFromStream(stream);
    Add(img);
  except
    img.Raster := NewRaster();
    disposeObject(img);
  end;
end;

procedure TAI_ImageList.AddPicture(fileName: SystemString);
var
  img: TAI_Image;
begin
  img := TAI_Image.Create(Self);
  disposeObject(img.Raster);
  try
    img.Raster := NewRasterFromFile(fileName);
    Add(img);
  except
    img.Raster := NewRaster();
    disposeObject(img);
  end;
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
      de.DrawPicture(mr, mr.BoundsRectV2, rp[i]^.Rect, 0, 1.0);
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

  de.EncodeAsSelectCompressor(stream, True);
  disposeObject(de);
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

procedure TAI_ImageList.Export_Segmentation(outputPath: SystemString);
var
  i, j: Integer;
  imgData: TAI_Image;
  segMask: PSegmentationMask;

  m64: TMemoryStream64;
  Prefix: U_String;
begin
  for i := 0 to Count - 1 do
    begin
      imgData := Items[i];

      m64 := TMemoryStream64.Create;
      imgData.Raster.SaveToJpegRGBStream(m64, 90);
      Prefix := umlStreamMD5String(m64);
      m64.SaveToFile(umlCombineFileName(outputPath, Prefix + '.jpg'));
      disposeObject(m64);

      for j := 0 to imgData.SegmentationMaskList.Count - 1 do
        begin
          segMask := imgData.SegmentationMaskList[j];
          segMask^.Raster.SaveToBmp24File(umlCombineFileName(outputPath, Prefix + '_' + umlIntToStr(j) + '.bmp'));
        end;
    end;
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
      if (imgData.DetectorDefineList.Count = 0) or (not imgData.ExistsDetectorToken(TokenFilter)) then
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
                [DetDef.R.Top, DetDef.R.Left, DetDef.R.Width, DetDef.R.Height]));

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

function TAI_ImageList.ExtractDetectorDefineAsSnapshotProjection(SS_width, SS_height: Integer): TMemoryRaster2DArray;
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
  mr: TMemoryRaster;
  hList: THashObjectList;
  mrList: TMemoryRasterList;
  pl: TPascalStringList;
begin
  DoStatus('begin prepare dataset.');
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

              // projection
              if (DetDef.Owner.Raster.Width <> SS_width) or (DetDef.Owner.Raster.Height <> SS_height) then
                begin
                  mr.SetSize(SS_width, SS_height);
                  DetDef.Owner.Raster.ProjectionTo(mr,
                    TV2Rect4.Init(RectFit(SS_width, SS_height, DetDef.Owner.Raster.BoundsRectV2), 0),
                    TV2Rect4.Init(mr.BoundsRectV2, 0),
                    True, 1.0);
                end
              else // fast copy
                  mr.Assign(DetDef.Owner.Raster);

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
  DoStatus('begin prepare dataset.');
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
              mr.Assign(DetDef.Owner.Raster);
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
  DoStatus('begin prepare dataset.');
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
  DoStatus('begin prepare dataset.');
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

function TAI_ImageList.SegmentationMaskCount: Integer;
var
  i: Integer;
  imgData: TAI_Image;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    begin
      imgData := Items[i];
      inc(Result, imgData.SegmentationMaskList.Count);
    end;
end;

function TAI_ImageList.FoundNoTokenDefine(output: TMemoryRaster): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if Items[i].FoundNoTokenDefine(output, DEColor(1, 0, 0, 0.5)) then
        exit;
  Result := False;
end;

function TAI_ImageList.FoundNoTokenDefine: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if Items[i].FoundNoTokenDefine then
        exit;
  Result := False;
end;

procedure TAI_ImageList.AllTokens(output: TPascalStringList);
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
begin
  for i := 0 to Count - 1 do
    begin
      imgData := Items[i];

      imgData.SegmentationMaskList.SegmentationTokens(output);

      for j := 0 to imgData.DetectorDefineList.Count - 1 do
        begin
          DetDef := imgData.DetectorDefineList[j];
          if DetDef.Token <> '' then
            if output.ExistsValue(DetDef.Token) < 0 then
                output.Add(DetDef.Token);
        end;
    end;
end;

function TAI_ImageList.DetectorTokens: TArrayPascalString;
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

function TAI_ImageList.ExistsDetectorToken(Token: TPascalString): Boolean;
begin
  Result := GetDetectorTokenCount(Token) > 0;
end;

function TAI_ImageList.GetDetectorTokenCount(Token: TPascalString): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      inc(Result, Items[i].GetDetectorTokenCount(Token));
end;

procedure TAI_ImageList.SegmentationTokens(output: TPascalStringList);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].SegmentationMaskList.SegmentationTokens(output);
end;

function TAI_ImageList.BuildSegmentationColorBuffer: TSegmentationColorList;
var
  SegTokenList: TPascalStringList;
  i: Integer;
  c: TRColor;
begin
  Result := TSegmentationColorList.Create;
  SegTokenList := TPascalStringList.Create;
  SegmentationTokens(SegTokenList);

  for i := 0 to SegTokenList.Count - 1 do
    begin
      repeat
          c := RColor(umlRandomRange($1, $FF - 1), umlRandomRange($1, $FF - 1), umlRandomRange($1, $FF - 1), $FF);
      until not Result.ExistsColor(c);
      Result.AddColor(SegTokenList[i], c);
    end;

  disposeObject(SegTokenList);
end;

procedure TAI_ImageList.BuildMaskMerge(colors: TSegmentationColorList);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].SegmentationMaskList.BuildMaskMerge(colors);
end;

procedure TAI_ImageList.BuildMaskMerge;
var
  cl: TSegmentationColorList;
begin
  cl := BuildSegmentationColorBuffer;
  BuildMaskMerge(cl);
  disposeObject(cl);
end;

procedure TAI_ImageList.LargeScale_BuildMaskMerge(RSeri: TRasterSerialized; colors: TSegmentationColorList);
var
  i: Integer;
  img: TAI_Image;
begin
  for i := 0 to Count - 1 do
    begin
      img := Items[i];
      img.SegmentationMaskList.BuildMaskMerge(colors);
      img.SerializedAndRecycleMemory(RSeri);
    end;
end;

procedure TAI_ImageList.ClearMaskMerge;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].SegmentationMaskList.MaskMergeRaster.Reset;
end;

procedure TAI_ImageList.SerializedAndRecycleMemory(Serializ: TRasterSerialized);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].SerializedAndRecycleMemory(Serializ);
end;

procedure TAI_ImageList.UnserializedMemory(Serializ: TRasterSerialized);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].UnserializedMemory(Serializ);
end;

procedure TAI_ImageMatrix.BuildSnapshotProjection_HashList(SS_width, SS_height: Integer; imgList: TAI_ImageList; hList: THashObjectList);
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
  mr: TMemoryRaster;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      imgData := imgList[i];
      if imgData.DetectorDefineList.Count > 0 then
        begin
          for j := 0 to imgData.DetectorDefineList.Count - 1 do
            begin
              DetDef := imgData.DetectorDefineList[j];
              if DetDef.Token <> '' then
                begin
                  mr := NewRaster();
                  mr.SetSize(SS_width, SS_height);

                  // projection
                  if (DetDef.Owner.Raster.Width <> SS_width) or (DetDef.Owner.Raster.Height <> SS_height) then
                    begin
                      mr.SetSize(SS_width, SS_height);
                      DetDef.Owner.Raster.ProjectionTo(mr,
                        TV2Rect4.Init(RectFit(SS_width, SS_height, DetDef.Owner.Raster.BoundsRectV2), 0),
                        TV2Rect4.Init(mr.BoundsRectV2, 0),
                        True, 1.0);
                    end
                  else // fast assign
                      mr.Assign(DetDef.Owner.Raster);

                  mr.UserToken := DetDef.Token;

                  LockObject(hList);
                  if not hList.Exists(DetDef.Token) then
                      hList.FastAdd(DetDef.Token, TMemoryRasterList.Create);
                  if TMemoryRasterList(hList[DetDef.Token]).IndexOf(mr) < 0 then
                      TMemoryRasterList(hList[DetDef.Token]).Add(mr);
                  UnLockObject(hList);
                end;
            end;
        end;
    end;
end;

procedure TAI_ImageMatrix.BuildSnapshot_HashList(imgList: TAI_ImageList; hList: THashObjectList);
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
  mr: TMemoryRaster;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      imgData := imgList[i];
      if imgData.DetectorDefineList.Count > 0 then
        begin
          for j := 0 to imgData.DetectorDefineList.Count - 1 do
            begin
              DetDef := imgData.DetectorDefineList[j];
              if DetDef.Token <> '' then
                begin
                  mr := NewRaster();
                  mr.Assign(DetDef.Owner.Raster);
                  mr.UserToken := DetDef.Token;

                  LockObject(hList);
                  if not hList.Exists(DetDef.Token) then
                      hList.FastAdd(DetDef.Token, TMemoryRasterList.Create);
                  if TMemoryRasterList(hList[DetDef.Token]).IndexOf(mr) < 0 then
                      TMemoryRasterList(hList[DetDef.Token]).Add(mr);
                  UnLockObject(hList);
                end;
            end;
        end;
    end;
end;

procedure TAI_ImageMatrix.BuildDefinePrepareRaster_HashList(SS_width, SS_height: Integer; imgList: TAI_ImageList; hList: THashObjectList);
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
              if not DetDef.PrepareRaster.Empty then
                begin
                  mr := NewRaster();
                  mr.ZoomFrom(DetDef.PrepareRaster, SS_width, SS_height);

                  LockObject(hList);
                  mr.UserToken := DetDef.Token;
                  if not hList.Exists(DetDef.Token) then
                      hList.FastAdd(DetDef.Token, TMemoryRasterList.Create);
                  TMemoryRasterList(hList[DetDef.Token]).Add(mr);
                  UnLockObject(hList);
                end;
            end;
        end;
    end;
end;

procedure TAI_ImageMatrix.BuildScaleSpace_HashList(SS_width, SS_height: Integer; imgList: TAI_ImageList; hList: THashObjectList);
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

              LockObject(hList);
              mr.UserToken := DetDef.Token;
              if not hList.Exists(DetDef.Token) then
                  hList.FastAdd(DetDef.Token, TMemoryRasterList.Create);
              TMemoryRasterList(hList[DetDef.Token]).Add(mr);
              UnLockObject(hList);
            end;
        end;
    end;
end;

procedure TAI_ImageMatrix.BuildSnapshotProjection_HashList(SS_width, SS_height: Integer; imgList: TAI_ImageList; RSeri: TRasterSerialized; hList: THashObjectList);
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
  mr: TMemoryRaster;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      imgData := imgList[i];
      if imgData.DetectorDefineList.Count > 0 then
        begin
          for j := 0 to imgData.DetectorDefineList.Count - 1 do
            begin
              DetDef := imgData.DetectorDefineList[j];
              if DetDef.Token <> '' then
                begin
                  mr := NewRaster();

                  // projection
                  if (DetDef.Owner.Raster.Width <> SS_width) or (DetDef.Owner.Raster.Height <> SS_height) then
                    begin
                      mr.SetSize(SS_width, SS_height);
                      DetDef.Owner.Raster.ProjectionTo(mr,
                        TV2Rect4.Init(RectFit(SS_width, SS_height, DetDef.Owner.Raster.BoundsRectV2), 0),
                        TV2Rect4.Init(mr.BoundsRectV2, 0),
                        True, 1.0);
                    end
                  else // fast assign
                      mr.Assign(DetDef.Owner.Raster);

                  mr.SerializedAndRecycleMemory(RSeri);

                  mr.UserToken := DetDef.Token;

                  LockObject(hList);
                  if not hList.Exists(DetDef.Token) then
                      hList.FastAdd(DetDef.Token, TMemoryRasterList.Create);
                  if TMemoryRasterList(hList[DetDef.Token]).IndexOf(mr) < 0 then
                      TMemoryRasterList(hList[DetDef.Token]).Add(mr);
                  UnLockObject(hList);
                end;
            end;
        end;
      imgData.Raster.RecycleMemory;
    end;
end;

procedure TAI_ImageMatrix.BuildSnapshot_HashList(imgList: TAI_ImageList; RSeri: TRasterSerialized; hList: THashObjectList);
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
  mr: TMemoryRaster;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      imgData := imgList[i];
      if imgData.DetectorDefineList.Count > 0 then
        begin
          for j := 0 to imgData.DetectorDefineList.Count - 1 do
            begin
              DetDef := imgData.DetectorDefineList[j];
              if DetDef.Token <> '' then
                begin
                  mr := NewRaster();
                  mr.Assign(DetDef.Owner.Raster);
                  mr.SerializedAndRecycleMemory(RSeri);

                  mr.UserToken := DetDef.Token;

                  LockObject(hList);
                  if not hList.Exists(DetDef.Token) then
                      hList.FastAdd(DetDef.Token, TMemoryRasterList.Create);
                  if TMemoryRasterList(hList[DetDef.Token]).IndexOf(mr) < 0 then
                      TMemoryRasterList(hList[DetDef.Token]).Add(mr);
                  UnLockObject(hList);
                end;
            end;
        end;
      imgData.Raster.RecycleMemory;
    end;
end;

procedure TAI_ImageMatrix.BuildDefinePrepareRaster_HashList(SS_width, SS_height: Integer; imgList: TAI_ImageList; RSeri: TRasterSerialized; hList: THashObjectList);
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
              DetDef.PrepareRaster.UnserializedMemory(RSeri);
              if not DetDef.PrepareRaster.Empty then
                begin
                  mr := NewRaster();
                  mr.ZoomFrom(DetDef.PrepareRaster, SS_width, SS_height);
                  DetDef.PrepareRaster.RecycleMemory;
                  mr.SerializedAndRecycleMemory(RSeri);

                  LockObject(hList);
                  mr.UserToken := DetDef.Token;
                  if not hList.Exists(DetDef.Token) then
                      hList.FastAdd(DetDef.Token, TMemoryRasterList.Create);
                  TMemoryRasterList(hList[DetDef.Token]).Add(mr);
                  UnLockObject(hList);
                end;
            end;
        end;
    end;
end;

procedure TAI_ImageMatrix.BuildScaleSpace_HashList(SS_width, SS_height: Integer; imgList: TAI_ImageList; RSeri: TRasterSerialized; hList: THashObjectList);
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
              mr.SerializedAndRecycleMemory(RSeri);

              LockObject(hList);
              mr.UserToken := DetDef.Token;
              if not hList.Exists(DetDef.Token) then
                  hList.FastAdd(DetDef.Token, TMemoryRasterList.Create);
              TMemoryRasterList(hList[DetDef.Token]).Add(mr);
              UnLockObject(hList);
            end;
        end;
      imgData.Raster.RecycleMemory;
    end;
end;

constructor TAI_ImageMatrix.Create;
begin
  inherited Create;
  UsedJpegForXML := True;
end;

destructor TAI_ImageMatrix.Destroy;
begin
  inherited Destroy;
end;

procedure TAI_ImageMatrix.RunScript(RSeri: TRasterSerialized; ScriptStyle: TTextStyle; condition_exp, process_exp: SystemString);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].RunScript(RSeri, ScriptStyle, condition_exp, process_exp);
end;

procedure TAI_ImageMatrix.RunScript(RSeri: TRasterSerialized; condition_exp, process_exp: SystemString);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].RunScript(RSeri, condition_exp, process_exp);
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

procedure TAI_ImageMatrix.SearchAndAddImageList(RSeri: TRasterSerialized; rootPath, filter: SystemString; includeSubdir, LoadImg: Boolean);
  procedure ProcessImg(fn, Prefix: TPascalString);
  var
    imgList: TAI_ImageList;
  begin
    DoStatus('%s (%s)', [fn.Text, Prefix.Text]);
    imgList := TAI_ImageList.Create;
    imgList.LoadFromFile(fn, LoadImg);
    imgList.FileInfo := Prefix;
    if RSeri <> nil then
        imgList.SerializedAndRecycleMemory(RSeri);
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

procedure TAI_ImageMatrix.SearchAndAddImageList(rootPath, filter: SystemString; includeSubdir, LoadImg: Boolean);
begin
  SearchAndAddImageList(nil, rootPath, filter, includeSubdir, LoadImg);
end;

procedure TAI_ImageMatrix.ImportImageListAsFragment(RSeri: TRasterSerialized; imgList: TAI_ImageList; RemoveSource: Boolean);
var
  i: Integer;
  img: TAI_Image;
  m64: TMemoryStream64;
  nImgL: TAI_ImageList;
  nImg: TAI_Image;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      nImgL := TAI_ImageList.Create;
      nImgL.FileInfo := PFormat('%s_fragment(%d)', [imgList.FileInfo.Text, i + 1]);

      if RemoveSource then
        begin
          nImg := img;
          nImg.Owner := nImgL;
          nImgL.Add(nImg);
        end
      else
        begin
          m64 := TMemoryStream64.Create;
          img.SaveToStream(m64);
          m64.Position := 0;

          nImg := TAI_Image.Create(nImgL);
          nImg.LoadFromStream(m64);
          nImgL.Add(nImg);
          disposeObject(m64);
        end;

      if RSeri <> nil then
          imgList.SerializedAndRecycleMemory(RSeri);
      Add(nImgL);
    end;
  if RemoveSource then
      imgList.Clear(False);
end;

procedure TAI_ImageMatrix.ImportImageListAsFragment(imgList: TAI_ImageList; RemoveSource: Boolean);
begin
  ImportImageListAsFragment(nil, imgList, RemoveSource);
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
  procedure fpc_Prepare_Save_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
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
{$ENDIF FPC}
{$ELSE parallel}
  Prepare_Save();
{$ENDIF parallel}
  Save();
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
    p^.fn := umlChangeFileExt(itmSR.Name, '');
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
  procedure Load_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PLoadRec;
  begin
    p := PrepareLoadBuffer[pass];
    p^.m64.Position := 0;
    p^.imgList.LoadFromStream(p^.m64);
    p^.imgList.FileInfo := p^.fn;
    disposeObject(p^.m64);
    p^.fn := '';
    dispose(p);
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
        dispose(p);
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
        if umlMultipleMatch('*' + C_ImageList_Ext, itmSR.Name) then
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
      dispose(p);
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

procedure TAI_ImageMatrix.Scale(f: TGeoFloat);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Scale(f);
end;

procedure TAI_ImageMatrix.ClearDetector;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].ClearDetector;
end;

procedure TAI_ImageMatrix.ClearSegmentation;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].ClearSegmentation;
end;

procedure TAI_ImageMatrix.ClearPrepareRaster;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].ClearPrepareRaster;
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

procedure TAI_ImageMatrix.Export_Segmentation(outputPath: SystemString);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Export_Segmentation(outputPath);
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
        if (imgData.DetectorDefineList.Count = 0) or (not imgData.ExistsDetectorToken(TokenFilter)) then
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
                  [DetDef.R.Top, DetDef.R.Left, DetDef.R.Width, DetDef.R.Height]));

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

function TAI_ImageMatrix.ImageList: TImageList_Decl;
var
  i, j: Integer;
  imgL: TAI_ImageList;
begin
  Result := TImageList_Decl.Create;
  for i := 0 to Count - 1 do
    begin
      imgL := Items[i];
      for j := 0 to imgL.Count - 1 do
          Result.Add(imgL[j]);
    end;
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

function TAI_ImageMatrix.FoundNoTokenDefine(output: TMemoryRaster): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if Items[i].FoundNoTokenDefine(output) then
        exit;
  Result := False;
end;

function TAI_ImageMatrix.FoundNoTokenDefine: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if Items[i].FoundNoTokenDefine then
        exit;
  Result := False;
end;

procedure TAI_ImageMatrix.AllTokens(output: TPascalStringList);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].AllTokens(output);
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

function TAI_ImageMatrix.DetectorTokens: TArrayPascalString;
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

function TAI_ImageMatrix.ExistsDetectorToken(Token: TPascalString): Boolean;
begin
  Result := GetDetectorTokenCount(Token) > 0;
end;

function TAI_ImageMatrix.GetDetectorTokenCount(Token: TPascalString): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      inc(Result, Items[i].GetDetectorTokenCount(Token));
end;

procedure TAI_ImageMatrix.SegmentationTokens(output: TPascalStringList);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].SegmentationTokens(output);
end;

function TAI_ImageMatrix.BuildSegmentationColorBuffer: TSegmentationColorList;
var
  SegTokenList: TPascalStringList;
  i: Integer;
  c: TRColor;
begin
  Result := TSegmentationColorList.Create;
  SegTokenList := TPascalStringList.Create;
  SegmentationTokens(SegTokenList);

  for i := 0 to SegTokenList.Count - 1 do
    begin
      repeat
          c := RColor(umlRandomRange($1, $FF - 1), umlRandomRange($1, $FF - 1), umlRandomRange($1, $FF - 1), $FF);
      until not Result.ExistsColor(c);
      Result.AddColor(SegTokenList[i], c);
    end;

  disposeObject(SegTokenList);
end;

procedure TAI_ImageMatrix.BuildMaskMerge(colors: TSegmentationColorList);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].BuildMaskMerge(colors);
end;

procedure TAI_ImageMatrix.BuildMaskMerge;
var
  cl: TSegmentationColorList;
begin
  cl := BuildSegmentationColorBuffer;
  BuildMaskMerge(cl);
  disposeObject(cl);
end;

procedure TAI_ImageMatrix.LargeScale_BuildMaskMerge(RSeri: TRasterSerialized; colors: TSegmentationColorList);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].LargeScale_BuildMaskMerge(RSeri, colors);
end;

procedure TAI_ImageMatrix.ClearMaskMerge;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].ClearMaskMerge;
end;

function TAI_ImageMatrix.ExtractDetectorDefineAsSnapshotProjection(SS_width, SS_height: Integer): TMemoryRaster2DArray;
var
  hList: THashObjectList;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    BuildSnapshotProjection_HashList(SS_width, SS_height, Items[pass], hList);
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
        BuildSnapshotProjection_HashList(SS_width, SS_height, Items[pass], hList);
  end;
{$ENDIF parallel}
  procedure runDone;
  var
    i, j: Integer;
    mr: TMemoryRaster;
    mrList: TMemoryRasterList;
    pl: TPascalStringList;
  begin
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
  end;

begin
  DoStatus('prepare dataset.');
  hList := THashObjectList.Create(True);
{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Count - 1);
{$ELSE FPC}
  TParallel.for(0, Count - 1, procedure(pass: Integer)
    begin
      BuildSnapshotProjection_HashList(SS_width, SS_height, Items[pass], hList);
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
  runDone;
  disposeObject(hList);
end;

function TAI_ImageMatrix.ExtractDetectorDefineAsSnapshot: TMemoryRaster2DArray;
var
  hList: THashObjectList;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    BuildSnapshot_HashList(Items[pass], hList);
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
        BuildSnapshot_HashList(Items[pass], hList);
  end;
{$ENDIF parallel}
  procedure runDone;
  var
    i, j: Integer;
    mr: TMemoryRaster;
    mrList: TMemoryRasterList;
    pl: TPascalStringList;
  begin
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
  end;

begin
  DoStatus('prepare dataset.');
  hList := THashObjectList.Create(True);
{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Count - 1);
{$ELSE FPC}
  TParallel.for(0, Count - 1, procedure(pass: Integer)
    begin
      BuildSnapshot_HashList(Items[pass], hList);
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
  runDone;
  disposeObject(hList);
end;

function TAI_ImageMatrix.ExtractDetectorDefineAsPrepareRaster(SS_width, SS_height: Integer): TMemoryRaster2DArray;
var
  hList: THashObjectList;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    BuildDefinePrepareRaster_HashList(SS_width, SS_height, Items[pass], hList);
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
        BuildDefinePrepareRaster_HashList(SS_width, SS_height, Items[pass], hList);
  end;
{$ENDIF parallel}
  procedure runDone;
  var
    i, j: Integer;
    mr: TMemoryRaster;
    mrList: TMemoryRasterList;
    pl: TPascalStringList;
  begin
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
  end;

begin
  DoStatus('prepare dataset.');
  hList := THashObjectList.Create(True);
{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Count - 1);
{$ELSE FPC}
  TParallel.for(0, Count - 1, procedure(pass: Integer)
    begin
      BuildDefinePrepareRaster_HashList(SS_width, SS_height, Items[pass], hList);
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
  runDone;
  disposeObject(hList);
end;

function TAI_ImageMatrix.ExtractDetectorDefineAsScaleSpace(SS_width, SS_height: Integer): TMemoryRaster2DArray;
var
  hList: THashObjectList;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    BuildScaleSpace_HashList(SS_width, SS_height, Items[pass], hList);
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
        BuildScaleSpace_HashList(SS_width, SS_height, Items[pass], hList);
  end;
{$ENDIF parallel}
  procedure runDone;
  var
    i, j: Integer;
    mr: TMemoryRaster;
    mrList: TMemoryRasterList;
    pl: TPascalStringList;
  begin
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
  end;

begin
  DoStatus('prepare dataset.');
  hList := THashObjectList.Create(True);
{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Count - 1);
{$ELSE FPC}
  TParallel.for(0, Count - 1, procedure(pass: Integer)
    begin
      BuildScaleSpace_HashList(SS_width, SS_height, Items[pass], hList);
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
  runDone;
  disposeObject(hList);
end;

procedure TAI_ImageMatrix.LargeScale_SaveToStream(RSeri: TRasterSerialized; stream: TCoreClassStream; RasterSave_: TRasterSave);
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
  procedure fpc_Prepare_Save_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PSaveRec;
  begin
    p := @PrepareSave[pass];
    p^.m64 := TMemoryStream64.CustomCreate(1024 * 1024);
    Items[pass].SaveToStream(p^.m64, True, False, RasterSave_);
    Items[pass].SerializedAndRecycleMemory(RSeri);
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
        Items[i].SaveToStream(p^.m64, True, False, RasterSave_);
        Items[i].SerializedAndRecycleMemory(RSeri);
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
{$ELSE FPC}
  TParallel.for(0, Count - 1, procedure(pass: Integer)
    var
      p: PSaveRec;
    begin
      p := @PrepareSave[pass];
      p^.m64 := TMemoryStream64.CustomCreate(1024 * 1024);
      Items[pass].SaveToStream(p^.m64, True, False, RasterSave_);
      Items[pass].SerializedAndRecycleMemory(RSeri);
      p^.fn := Items[pass].FileInfo.TrimChar(#32#9);
      if (p^.fn.Len = 0) then
          p^.fn := umlStreamMD5String(p^.m64);
      p^.fn := p^.fn + C_ImageList_Ext;
      FinishSave[pass] := p;
    end);
{$ENDIF FPC}
{$ELSE parallel}
  Prepare_Save();
{$ENDIF parallel}
  Save();
  disposeObject(dbEng);
  DoStatus('Save Image Matrix done.');
end;

procedure TAI_ImageMatrix.LargeScale_SaveToStream(RSeri: TRasterSerialized; stream: TCoreClassStream);
begin
  LargeScale_SaveToStream(RSeri, stream, TRasterSave.rsRGB);
end;

procedure TAI_ImageMatrix.LargeScale_LoadFromStream(RSeri: TRasterSerialized; stream: TCoreClassStream);
type
  PLoadRec = ^TLoadRec;

  TLoadRec = record
    fn: TPascalString;
    itmHnd: TItemHandle;
    imgList: TAI_ImageList;
  end;

var
  dbEng: TObjectDataManager;
  fPos: Int64;
  PrepareLoadBuffer: TCoreClassList;
  itmSR: TItemSearch;
  Critical: TCritical;

  procedure PrepareMemory;
  var
    p: PLoadRec;
  begin
    new(p);
    p^.fn := umlChangeFileExt(itmSR.Name, '');
    dbEng.ItemFastOpen(itmSR.HeaderPOS, p^.itmHnd);
    p^.imgList := TAI_ImageList.Create;
    Add(p^.imgList);
    PrepareLoadBuffer.Add(p);
  end;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure Load_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  var
    p: PLoadRec;
    m64: TMemoryStream64;
  begin
    p := PrepareLoadBuffer[pass];
    m64 := TMemoryStream64.Create;
    m64.Size := p^.itmHnd.Item.Size;
    Critical.Acquire;
    dbEng.ItemRead(p^.itmHnd, p^.itmHnd.Item.Size, m64.memory^);
    Critical.Release;
    m64.Position := 0;
    p^.imgList.LoadFromStream(m64);
    p^.imgList.FileInfo := p^.fn;
    p^.imgList.SerializedAndRecycleMemory(RSeri);
    disposeObject(m64);
    p^.fn := '';
    dispose(p);
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure Load_For();
  var
    i: Integer;
    p: PLoadRec;
    m64: TMemoryStream64;
  begin
    for i := 0 to PrepareLoadBuffer.Count - 1 do
      begin
        p := PrepareLoadBuffer[i];
        m64 := TMemoryStream64.Create;
        m64.Size := p^.itmHnd.Item.Size;
        Critical.Acquire;
        dbEng.ItemRead(p^.itmHnd, p^.itmHnd.Item.Size, m64.memory^);
        Critical.Release;
        m64.Position := 0;
        p^.imgList.LoadFromStream(m64);
        p^.imgList.FileInfo := p^.fn;
        p^.imgList.SerializedAndRecycleMemory(RSeri);
        disposeObject(m64);
        p^.fn := '';
        dispose(p);
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
        if umlMultipleMatch('*' + C_ImageList_Ext, itmSR.Name) then
            PrepareMemory;
      until not dbEng.ItemFindNext(itmSR);
    end;

  Critical := TCritical.Create;
{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Load_ParallelFor, 0, PrepareLoadBuffer.Count - 1);
{$ELSE FPC}
  TParallel.for(0, PrepareLoadBuffer.Count - 1, procedure(pass: Integer)
    var
      p: PLoadRec;
      m64: TMemoryStream64;
    begin
      p := PrepareLoadBuffer[pass];
      m64 := TMemoryStream64.Create;
      m64.Size := p^.itmHnd.Item.Size;
      Critical.Acquire;
      dbEng.ItemRead(p^.itmHnd, p^.itmHnd.Item.Size, m64.memory^);
      Critical.Release;
      m64.Position := 0;
      p^.imgList.LoadFromStream(m64);
      p^.imgList.FileInfo := p^.fn;
      p^.imgList.SerializedAndRecycleMemory(RSeri);
      disposeObject(m64);
      p^.fn := '';
      dispose(p);
    end);
{$ENDIF FPC}
{$ELSE parallel}
  Load_For();
{$ENDIF parallel}
  disposeObject(Critical);
  disposeObject(PrepareLoadBuffer);
  disposeObject(dbEng);
  DoStatus('Load Image Matrix done.');
end;

procedure TAI_ImageMatrix.LargeScale_SaveToFile(RSeri: TRasterSerialized; fileName: SystemString; RasterSave_: TRasterSave);
var
  fs: TCoreClassFileStream;
begin
  DoStatus('save Image Matrix: %s', [fileName]);
  fs := TCoreClassFileStream.Create(fileName, fmCreate);
  LargeScale_SaveToStream(RSeri, fs, RasterSave_);
  disposeObject(fs);
end;

procedure TAI_ImageMatrix.LargeScale_SaveToFile(RSeri: TRasterSerialized; fileName: SystemString);
begin
  LargeScale_SaveToFile(RSeri, fileName, TRasterSave.rsRGB);
end;

procedure TAI_ImageMatrix.LargeScale_LoadFromFile(RSeri: TRasterSerialized; fileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  DoStatus('loading Image Matrix: %s', [fileName]);
  fs := TCoreClassFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  LargeScale_LoadFromStream(RSeri, fs);
  disposeObject(fs);
end;

function TAI_ImageMatrix.LargeScale_ExtractDetectorDefineAsSnapshotProjection(RSeri: TRasterSerialized; SS_width, SS_height: Integer): TMemoryRaster2DArray;
var
  hList: THashObjectList;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    BuildSnapshotProjection_HashList(SS_width, SS_height, Items[pass], RSeri, hList);
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
        BuildSnapshotProjection_HashList(SS_width, SS_height, Items[pass], RSeri, hList);
  end;
{$ENDIF parallel}
  procedure runDone;
  var
    i, j: Integer;
    mr: TMemoryRaster;
    mrList: TMemoryRasterList;
    pl: TPascalStringList;
  begin
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
  end;

begin
  DoStatus('prepare dataset.');
  hList := THashObjectList.Create(True);
{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Count - 1);
{$ELSE FPC}
  TParallel.for(0, Count - 1, procedure(pass: Integer)
    begin
      BuildSnapshotProjection_HashList(SS_width, SS_height, Items[pass], RSeri, hList);
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
  runDone;
  disposeObject(hList);
end;

function TAI_ImageMatrix.LargeScale_ExtractDetectorDefineAsSnapshot(RSeri: TRasterSerialized): TMemoryRaster2DArray;
var
  hList: THashObjectList;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    BuildSnapshot_HashList(Items[pass], RSeri, hList);
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
        BuildSnapshot_HashList(Items[pass], RSeri, hList);
  end;
{$ENDIF parallel}
  procedure runDone;
  var
    i, j: Integer;
    mr: TMemoryRaster;
    mrList: TMemoryRasterList;
    pl: TPascalStringList;
  begin
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
  end;

begin
  DoStatus('prepare dataset.');
  hList := THashObjectList.Create(True);
{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Count - 1);
{$ELSE FPC}
  TParallel.for(0, Count - 1, procedure(pass: Integer)
    begin
      BuildSnapshot_HashList(Items[pass], RSeri, hList);
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
  runDone;
  disposeObject(hList);
end;

function TAI_ImageMatrix.LargeScale_ExtractDetectorDefineAsPrepareRaster(RSeri: TRasterSerialized; SS_width, SS_height: Integer): TMemoryRaster2DArray;
var
  hList: THashObjectList;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    BuildDefinePrepareRaster_HashList(SS_width, SS_height, Items[pass], RSeri, hList);
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
        BuildDefinePrepareRaster_HashList(SS_width, SS_height, Items[pass], RSeri, hList);
  end;
{$ENDIF parallel}
  procedure runDone;
  var
    i, j: Integer;
    mr: TMemoryRaster;
    mrList: TMemoryRasterList;
    pl: TPascalStringList;
  begin
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
  end;

begin
  DoStatus('prepare dataset.');
  hList := THashObjectList.Create(True);
{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Count - 1);
{$ELSE FPC}
  TParallel.for(0, Count - 1, procedure(pass: Integer)
    begin
      BuildDefinePrepareRaster_HashList(SS_width, SS_height, Items[pass], RSeri, hList);
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
  runDone;
  disposeObject(hList);
end;

function TAI_ImageMatrix.LargeScale_ExtractDetectorDefineAsScaleSpace(RSeri: TRasterSerialized; SS_width, SS_height: Integer): TMemoryRaster2DArray;
var
  hList: THashObjectList;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; Data: Pointer; Item: TMultiThreadProcItem);
  begin
    BuildScaleSpace_HashList(SS_width, SS_height, Items[pass], RSeri, hList);
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
        BuildScaleSpace_HashList(SS_width, SS_height, Items[pass], RSeri, hList);
  end;
{$ENDIF parallel}
  procedure runDone;
  var
    i, j: Integer;
    mr: TMemoryRaster;
    mrList: TMemoryRasterList;
    pl: TPascalStringList;
  begin
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
  end;

begin
  DoStatus('prepare dataset.');
  hList := THashObjectList.Create(True);
{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, Count - 1);
{$ELSE FPC}
  TParallel.for(0, Count - 1, procedure(pass: Integer)
    begin
      BuildScaleSpace_HashList(SS_width, SS_height, Items[pass], RSeri, hList);
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor;
{$ENDIF parallel}
  runDone;
  disposeObject(hList);
end;

procedure TAI_ImageMatrix.SerializedAndRecycleMemory(Serializ: TRasterSerialized);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].SerializedAndRecycleMemory(Serializ);
end;

procedure TAI_ImageMatrix.UnserializedMemory(Serializ: TRasterSerialized);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].UnserializedMemory(Serializ);
end;

initialization

Init_AI_Common;

finalization

end.
