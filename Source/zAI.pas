{ ****************************************************************************** }
{ * AI (platform: cuda+mkl64+mkl32+win64+win32)                                * }
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
unit zAI;

{$INCLUDE zDefine.inc}

(* AI engine compatible record packing *)
{$IFDEF FPC}
{$PACKENUM 4}    (* use 4-byte enums *)
{$PACKRECORDS C}
{$ELSE}
{$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

interface

uses Types, Variants,
  CoreClasses,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ELSE FPC}
  System.IOUtils,
{$ENDIF FPC}
  PascalStrings, MemoryStream64, UnicodeMixedLib, DataFrameEngine, ListEngine, TextDataEngine, TextParsing,
  ObjectDataManager, ObjectData, ItemStream,
  zDrawEngine, Geometry2DUnit, MemoryRaster, LearnTypes, Learn, KDTree, PyramidSpace,
  zAI_Common, zAI_TrainingTask, zAI_KeyIO,
  zExpression, OpCode, MorphologyExpression;

type
{$REGION 'BaseDefine'}
  TAI = class;
  TAI_DNN_Thread = class;
  TAI_DNN_Thread_Class = class of TAI_DNN_Thread;
  TAI_DNN_ThreadPool = class;
  TAI_DNN_Thread_Metric = class;
  TAI_DNN_Thread_LMetric = class;
  TAI_DNN_Thread_MMOD6L = class;
  TAI_DNN_Thread_MMOD3L = class;
  TAI_DNN_Thread_RNIC = class;
  TAI_DNN_Thread_LRNIC = class;
  TAI_DNN_Thread_GDCNIC = class;
  TAI_DNN_Thread_GNIC = class;
  TAI_DNN_Thread_SS = class;
  TAI_DNN_Thread_ZMetric = class;

  PAI_EntryAPI = ^TAI_EntryAPI;

  TRGB_Image_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TMatrix_Image_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TOD6L_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TOD3L_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TOD6L_Marshal_Handle = THashList;
  TSP_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TFACE_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TMetric_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TLMetric_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TMMOD6L_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TMMOD3L_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TRNIC_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TLRNIC_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TGDCNIC_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TGNIC_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TSS_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TTracker_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TTracker_Handle_Array = array of TTracker_Handle;
  TTracker_Handle_ArrayOfArray = array of TTracker_Handle_Array;
  TOCR_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;
  TZMetric_Handle = {$IFDEF DELPHI} type of {$ENDIF DELPHI} Pointer;

  TCutDataType = Byte;
  PCutDataType = PByte;
  TCutData = array [0 .. MaxInt div SizeOf(TCutDataType) - 1] of TCutDataType;
  PCutData = ^TCutData;
  TCutDataLineProcessor = {$IFDEF FPC}specialize {$ENDIF FPC}TLineProcessor<TCutDataType>;

  TBGRA_Image_Buffer_ = packed record
    Bits: Pointer;
    Width, Height: Integer;
  end;

  PBGRA_Image_Buffer_ = ^TBGRA_Image_Buffer_;
  TBGRA_Buffer_Handle = PBGRA_Image_Buffer_;

  TUnmixedData_ = packed record
    output, output_a, overlay, refined, refined_a, refined_overlay: TBGRA_Image_Buffer_;
  end;

  PUnmixedData_ = ^TUnmixedData_;

  TUnmixedData = packed record
    data_num: Integer;
    data: PUnmixedData_;
    well_represented_map_num: Integer;
    well_represented_map: TBGRA_Buffer_Handle;
    temporary_weight_map_num: Integer;
    temporary_weight_map: TBGRA_Buffer_Handle;
  end;

  PUnmixedData = ^TUnmixedData;

  TImage_Handle = packed record
    image: TAI_Image;
    AccessImage: Int64;
    AccessDetectorImage: Int64;
    AccessDetectorRect: Int64;
    AccessMask: Int64;
  end;

  PImage_Handle = ^TImage_Handle;
  PPImage_Handle = ^PImage_Handle;

  TRaster_Handle = packed record
    Raster: TMemoryRaster;
  end;

  PRaster_Handle = ^TRaster_Handle;

  TSurf_Desc = packed record
    X, Y, DX, DY: Integer;
    desc: array [0 .. 63] of Single;
  end;

  PSurf_Desc = ^TSurf_Desc;

  TSurf_DescBuffer = array of TSurf_Desc;

  PSurfMatched = ^TSurfMatched;

  TSurfMatched = record
    SD1, SD2: PSurf_Desc;
    R1, R2: TMemoryRaster;
  end;

  TSurfMatchedBuffer = array of TSurfMatched;

  C_Bytes = packed record
    Size: Integer;
    Bytes: PByte;
  end;

  P_Bytes = ^C_Bytes;

  TAI_Rect = packed record
    Left, Top, Right, Bottom: Integer;
  end;

  PAI_Rect = ^TAI_Rect;

  TAI_Rect_Desc = array of TAI_Rect;

  TOD_Rect = packed record
    Left, Top, Right, Bottom: Integer;
    confidence: Double;
  end;

  POD_Rect = ^TOD_Rect;

  TOD_Desc = array of TOD_Rect;

  TOD_Marshal_Rect = record
    R: TRectV2;
    confidence: Double;
    Token: U_String;
  end;

  TOD_Marshal_Desc = array of TOD_Marshal_Rect;

  TOD_List_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TOD_Rect>;
  TOD_Marshal_List_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TOD_Marshal_Rect>;
  TOD_List = TOD_List_Decl;
  TOD_Marshal_List = TOD_Marshal_List_Decl;

  PAI_Point = ^TAI_Point;

  TAI_Point = packed record
    X, Y: Integer;
  end;

  TSP_Desc = array of TAI_Point;

  PTrainingControl = ^TTrainingControl;

  TTrainingControl = packed record
    pause, stop: Integer;
  end;

  PAI_MMOD_Rect = ^TAI_MMOD_Rect;

  TAI_MMOD_Rect = packed record
    Left, Top, Right, Bottom: Integer;
    confidence: Double;
    Token: PPascalString;
  end;

  TAI_MMOD_Desc = array of TAI_MMOD_Rect;

  TMMOD_Rect = record
    R: TRectV2;
    confidence: Double;
    Token: U_String;
  end;

  PMMOD_Rect = ^TMMOD_Rect;
  TMMOD_RectList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PMMOD_Rect>;
  TMMOD_Desc = array of TMMOD_Rect;
  PMMOD_Desc = ^TMMOD_Desc;
  TMMOD_Desc_Array = array of TMMOD_Desc;
  PMMOD_Desc_Array = ^TMMOD_Desc_Array;
  TMMOD_DescList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TMMOD_Desc>;

  TAI_Raster_Data = packed record
    raster_Hnd: PRaster_Handle;
    raster_ptr: PRColorArray;
    Width, Height, index: Integer;
  end;

  PAI_Raster_Data = ^TAI_Raster_Data;

  TAI_Raster_Data_Array = array [0 .. (MaxInt div SizeOf(PAI_Raster_Data)) - 1] of PAI_Raster_Data;
  PAI_Raster_Data_Array = ^TAI_Raster_Data_Array;

  TMetric_ResNet_Train_Parameter = packed record
    { input }
    imgArry_ptr: PAI_Raster_Data_Array;
    img_num: Integer;
    train_sync_file, train_output: P_Bytes;
    { training param }
    timeout: UInt64;
    weight_decay, momentum: Double;
    iterations_without_progress_threshold: Integer;
    learning_rate, completed_learning_rate: Double;
    step_mini_batch_target_num, step_mini_batch_raster_num: Integer;
    { progress control }
    control: PTrainingControl;
    { training result }
    training_average_loss, training_learning_rate: Double;
    { full gpu }
    fullGPU_Training: Boolean;
  end;

  PMetric_ResNet_Train_Parameter = ^TMetric_ResNet_Train_Parameter;

  TMMOD_Train_Parameter = packed record
    { input data }
    train_cfg, train_sync_file, train_output: P_Bytes;
    { training param }
    timeout: UInt64;
    weight_decay, momentum: Double;
    target_size, min_target_size: Integer;
    min_detector_window_overlap_iou: Double;
    iterations_without_progress_threshold: Integer;
    learning_rate, completed_learning_rate: Double;
    saveMemory: Integer;
    { overlap non-max suppression param }
    overlap_NMS_iou_thresh, overlap_NMS_percent_covered_thresh: Double;
    { overlap ignore param }
    overlap_ignore_iou_thresh, overlap_ignore_percent_covered_thresh: Double;
    { cropper param }
    prepare_crops_img_num: Integer;
    num_crops: Integer;
    chip_dims_x, chip_dims_y: Integer;
    min_object_size_x, min_object_size_y: Integer;
    max_rotation_degrees, max_object_size: Double;
    { progress control }
    control: PTrainingControl;
    { training result }
    training_average_loss, training_learning_rate: Double;
    { internal }
    TempFiles: TPascalStringList;
  end;

  PMMOD_Train_Parameter = ^TMMOD_Train_Parameter;

  TRNIC_Train_Parameter = packed record
    { input data }
    imgArry_ptr: PAI_Raster_Data_Array;
    img_num: Integer;
    train_sync_file, train_output: P_Bytes;
    { training param }
    timeout: UInt64;
    weight_decay, momentum: Double;
    iterations_without_progress_threshold: Integer;
    learning_rate, completed_learning_rate: Double;
    all_bn_running_stats_window_sizes: Integer;
    img_mini_batch: Integer;
    { progress control }
    control: PTrainingControl;
    { training result }
    training_average_loss, training_learning_rate: Double;
  end;

  PRNIC_Train_Parameter = ^TRNIC_Train_Parameter;

  TGDCNIC_Train_Parameter = packed record
    { input data }
    imgArry_ptr: PAI_Raster_Data_Array;
    img_num: Integer;
    train_sync_file, train_output: P_Bytes;
    { training param }
    timeout: UInt64;
    iterations_without_progress_threshold: Integer;
    learning_rate, completed_learning_rate: Double;
    img_mini_batch: Integer;
    { progress control }
    control: PTrainingControl;
    { training result }
    training_average_loss, training_learning_rate: Double;
  end;

  PGDCNIC_Train_Parameter = ^TGDCNIC_Train_Parameter;

  TGNIC_Train_Parameter = packed record
    { input data }
    imgArry_ptr: PAI_Raster_Data_Array;
    img_num: Integer;
    train_sync_file, train_output: P_Bytes;
    { training param }
    timeout: UInt64;
    iterations_without_progress_threshold: Integer;
    learning_rate, completed_learning_rate: Double;
    img_mini_batch: Integer;
    { progress control }
    control: PTrainingControl;
    { training result }
    training_average_loss, training_learning_rate: Double;
  end;

  PGNIC_Train_Parameter = ^TGNIC_Train_Parameter;

  TSS_Train_Parameter = packed record
    { input data }
    imgHnd_ptr: PPImage_Handle;
    imgHnd_num: Integer;
    color: PSegmentationColorTable;
    train_sync_file, train_output: P_Bytes;
    { training param }
    timeout: UInt64;
    weight_decay, momentum: Double;
    iterations_without_progress_threshold: Integer;
    learning_rate, completed_learning_rate: Double;
    all_bn_running_stats_window_sizes: Integer;
    img_crops_batch: Integer;
    { progress control }
    control: PTrainingControl;
    { training result }
    training_average_loss, training_learning_rate: Double;
  end;

  PSS_Train_Parameter = ^TSS_Train_Parameter;

  TSS_ProcessOnResultCall = procedure(Successed: Boolean; SSInput, SSOutput: TMemoryRaster; SSTokenOutput: TPascalStringList);
  TSS_ProcessOnResultMethod = procedure(Successed: Boolean; SSInput, SSOutput: TMemoryRaster; SSTokenOutput: TPascalStringList) of object;

{$IFDEF FPC}
  TSS_ProcessOnResultProc = procedure(Successed: Boolean; SSInput, SSOutput: TMemoryRaster; SSTokenOutput: TPascalStringList) is nested;
{$ELSE FPC}
  TSS_ProcessOnResultProc = reference to procedure(Successed: Boolean; SSInput, SSOutput: TMemoryRaster; SSTokenOutput: TPascalStringList);
{$ENDIF FPC}

  TZMetric_Train_Parameter = packed record
    { input }
    imgArry_ptr: PAI_Raster_Data_Array;
    img_num: Integer;
    train_sync_file, train_output: P_Bytes;
    { training param }
    timeout: UInt64;
    weight_decay, momentum: Double;
    iterations_without_progress_threshold: Integer;
    learning_rate, completed_learning_rate: Double;
    step_mini_batch_target_num, step_mini_batch_raster_num: Integer;
    { progress control }
    control: PTrainingControl;
    { training result }
    training_average_loss, training_learning_rate: Double;
  end;

  PZMetric_Train_Parameter = ^TZMetric_Train_Parameter;

  TOneStep = packed record
    StepTime: Double;
    one_step_calls: UInt64;
    average_loss: Double;
    learning_rate: Double;
  end;

  POneStep = ^TOneStep;

  TAI_Log = record
    LogTime: TDateTime;
    LogText: SystemString;
  end;

  TOneStepList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<POneStep>;
  TAI_LogList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TAI_Log>;

  TOnStep = procedure(Sender: POneStep) of object;

  TOneStepList = class(TOneStepList_Decl)
  private
    Critical: TCritical;
    FOnStep: TOnStep;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Delete(index: Integer);
    procedure Clear;

    procedure AddStep(one_step_calls: UInt64; average_loss, learning_rate: Double); overload;
    procedure AddStep(p_: POneStep); overload;
    property OnStep: TOnStep read FOnStep write FOnStep;

    procedure SaveToStream(stream: TMemoryStream64);
    procedure LoadFromStream(stream: TMemoryStream64);

    procedure ExportToExcelStream(stream: TMemoryStream64);
    procedure ExportToExcelFile(fileName: U_String);
  end;

  TAI_LogList = class(TAI_LogList_Decl)
  public
  end;

{$ENDREGION 'BaseDefine'}
{$REGION 'Alignment'}

  TAlignment = class(TCoreClassObject)
  public
    AI: TAI;
    constructor Create(OwnerAI: TAI); virtual;
    destructor Destroy; override;

    procedure Alignment(imgList: TAI_ImageList); virtual; abstract;
  end;

  TAlignment_Face = class(TAlignment)
  public
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_FastFace = class(TAlignment)
  public
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_Fast4VertexProjection = class(TAlignment)
  public
    SS_Width, SS_Height: Integer;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_ScaleSpace = class(TAlignment)
  public
    SS_Width, SS_Height: Integer;
    CalibrateDetectorDefine: Boolean;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_OD6L = class(TAlignment)
  public
    OD_Hnd: TOD6L_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_FastOD6L = class(TAlignment)
  public
    OD_Hnd: TOD6L_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_OD3L = class(TAlignment)
  public
    OD_Hnd: TOD3L_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_FastOD3L = class(TAlignment)
  public
    OD_Hnd: TOD3L_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_OD6L_Marshal = class(TAlignment)
  public
    OD_Hnd: TOD6L_Marshal_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_FastOD6L_Marshal = class(TAlignment)
  public
    OD_Hnd: TOD6L_Marshal_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_SP = class(TAlignment)
  public
    sp_hnd: TSP_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_Face_SP = class(TAlignment)
  public
    sp_hnd: TSP_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_MMOD6L = class(TAlignment)
  public
    MMOD_hnd: TMMOD6L_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_FastMMOD6L = class(TAlignment)
  public
    MMOD_hnd: TMMOD6L_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_MMOD3L = class(TAlignment)
  public
    MMOD_hnd: TMMOD3L_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_FastMMOD3L = class(TAlignment)
  public
    MMOD_hnd: TMMOD3L_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_SS = class(TAlignment)
  public
    SS_hnd: TSS_Handle;
    procedure DoGetPixelSegClassify(X, Y: Integer; color: TRColor; var Classify: TMorphologyClassify);
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_Metric = class(TAlignment)
  public
    MetricHnd: TMetric_Handle;
    Learn_: TLearn;
    MinK: TLFloat;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_ZMetric = class(TAlignment)
  public
    MetricHnd: TZMetric_Handle;
    Learn_: TLearn;
    SS_Width, SS_Height: Integer;
    MinK: TLFloat;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

{$ENDREGION 'Alignment'}
{$REGION 'Entry'}

  TMorphExpIntf = class
  private
    FAI_EntryAPI: PAI_EntryAPI;
    MorphologyExpression_RegData: Pointer;
    function MorphExp_HotMap(OpRunTime: TOpCustomRunTime; var param: TOpParam): Variant;
    function MorphExp_JetMap(OpRunTime: TOpCustomRunTime; var param: TOpParam): Variant;
    function MorphExp_Salient(OpRunTime: TOpCustomRunTime; var param: TOpParam): Variant;
    procedure RegMorphExpExternalAPI(Exp: TMorphExpRunTime);
  public
    constructor Create(AI_EntryAPI_: PAI_EntryAPI);
    destructor Destroy; override;
  end;

  TAI_EntryAPI = packed record
    { engine support }
    Authentication, Training, CUDA, MKL: Integer;

    { prepare image }
    Prepare_RGB_Image: function(const raster_ptr: PRColorArray; const Width, Height: Integer): TRGB_Image_Handle; stdcall;
    Prepare_Matrix_Image: function(const raster_ptr: PRColorArray; const Width, Height: Integer): TMatrix_Image_Handle; stdcall;
    Close_RGB_Image: procedure(img: TRGB_Image_Handle); stdcall;
    Close_Matrix_Image: procedure(img: TMatrix_Image_Handle); stdcall;

    { image buffer }
    OpenImageBuffer_RGB: function(hnd: TRGB_Image_Handle): TBGRA_Buffer_Handle; stdcall;
    OpenImageBuffer_Matrix: function(hnd: TMatrix_Image_Handle): TBGRA_Buffer_Handle; stdcall;
    OpenImageBuffer_Hot: function(const raster_ptr: PRColorArray; const Width, Height: Integer): TBGRA_Buffer_Handle; stdcall;
    OpenImageBuffer_Jet: function(const raster_ptr: PRColorArray; const Width, Height: Integer): TBGRA_Buffer_Handle; stdcall;
    CloseImageBuffer: procedure(hnd: TBGRA_Buffer_Handle); stdcall;

    { image segment }
    Segment: function(const raster_ptr: PRColorArray; const Width, Height: Integer; const k: Double; const min_siz: Integer): TBGRA_Buffer_Handle; stdcall;

    { image salient }
    Salient: function(const raster_ptr: PRColorArray; const Width, Height: Integer; const iterations: Integer): TBGRA_Buffer_Handle; stdcall;

    { search Candidate Object from image }
    CandidateObject: function(const raster_ptr: PRColorArray; const Width, Height: Integer;
      const min_size, max_merging_iterations: Integer; const AI_Rect: PAI_Rect; const max_AI_Rect: Integer): Integer; stdcall;

    { Rasterize Unmixing }
    RasterizeUnmixing: function(const raster_ptr: PRColorArray; const Width, Height: Integer): PUnmixedData; stdcall;
    FreeUnmixingData: procedure(data: PUnmixedData); stdcall;

    { poisson blend }
    poisson_blend: procedure(const GAMMA: Double;
      const sour_ptr: PRColorArray; const sour_Width, sour_Height: Integer;
      const dest_ptr: PRColorArray; const dest_Width, dest_Height: Integer;
      const projection_x, projection_y, paper: Integer); stdcall;

    { grabcut }
    CutRaster: procedure(const raster_ptr: PRColorArray; maskIO: PByte; const Width, Height: Integer; box: TAI_Rect; iterCount, mode: Integer); stdcall;

    { surf detector }
    fast_surf: function(const raster_ptr: PRColorArray; const Width, Height: Integer; const max_points: Integer; const detection_threshold: Double; const output: PSurf_Desc): Integer; stdcall;

    { object detector 6 layer }
    OD6L_Train: function(train_cfg, train_output: P_Bytes; window_w, window_h, thread_num: Integer): Integer; stdcall;
    LargeScale_OD6L_Train: function(img_: PPImage_Handle; num_: Integer; train_output: P_Bytes; window_w, window_h, thread_num: Integer): Integer; stdcall;
    OD6L_Init: function(train_data: P_Bytes): TOD6L_Handle; stdcall;
    OD6L_Init_Memory: function(memory: Pointer; Size: Integer): TOD6L_Handle; stdcall;
    OD6L_Free: function(hnd: TOD6L_Handle): Integer; stdcall;
    OD6L_Process: function(hnd: TOD6L_Handle; const raster_ptr: PRColorArray; const Width, Height: Integer; const OD_Rect: POD_Rect; const max_OD_Rect: Integer; var OD_Rect_num: Integer): Integer; stdcall;
    OD6L_Process_Image: function(hnd: TOD6L_Handle; rgb_img: TRGB_Image_Handle; const OD_Rect: POD_Rect; const max_OD_Rect: Integer; var OD_Rect_num: Integer): Integer; stdcall;

    { object detector 3 layer }
    OD3L_Train: function(train_cfg, train_output: P_Bytes; window_w, window_h, thread_num: Integer): Integer; stdcall;
    LargeScale_OD3L_Train: function(img_: PPImage_Handle; num_: Integer; train_output: P_Bytes; window_w, window_h, thread_num: Integer): Integer; stdcall;
    OD3L_Init: function(train_data: P_Bytes): TOD6L_Handle; stdcall;
    OD3L_Init_Memory: function(memory: Pointer; Size: Integer): TOD6L_Handle; stdcall;
    OD3L_Free: function(hnd: TOD6L_Handle): Integer; stdcall;
    OD3L_Process: function(hnd: TOD6L_Handle; const raster_ptr: PRColorArray; const Width, Height: Integer; const OD_Rect: POD_Rect; const max_OD_Rect: Integer; var OD_Rect_num: Integer): Integer; stdcall;
    OD3L_Process_Image: function(hnd: TOD6L_Handle; rgb_img: TRGB_Image_Handle; const OD_Rect: POD_Rect; const max_OD_Rect: Integer; var OD_Rect_num: Integer): Integer; stdcall;

    { shape predictor and shape detector }
    SP_Train: function(train_cfg, train_output: P_Bytes; oversampling_amount, tree_depth, thread_num: Integer): Integer; stdcall;
    LargeScale_SP_Train: function(img_: PPImage_Handle; num_: Integer; train_output: P_Bytes; oversampling_amount, tree_depth, thread_num: Integer): Integer; stdcall;
    SP_Init: function(train_data: P_Bytes): TSP_Handle; stdcall;
    SP_Init_Memory: function(memory: Pointer; Size: Integer): TSP_Handle; stdcall;
    SP_Free: function(hnd: TSP_Handle): Integer; stdcall;
    SP_Process: function(hnd: TSP_Handle; const raster_ptr: PRColorArray; const Width, Height: Integer; const AI_Rect: PAI_Rect; const AI_Point: PAI_Point; const max_AI_Point: Integer; var AI_Point_num: Integer): Integer; stdcall;
    SP_Process_Image: function(hnd: TSP_Handle; rgb_img: TRGB_Image_Handle; const AI_Rect: PAI_Rect; const AI_Point: PAI_Point; const max_AI_Point: Integer; var AI_Point_num: Integer): Integer; stdcall;

    { face recognition shape predictor }
    SP_extract_face_rect_desc_chips: function(hnd: TSP_Handle; const raster_ptr: PRColorArray; const Width, Height, extract_face_size: Integer; rect_desc_: PAI_Rect; rect_num: Integer): TFACE_Handle; stdcall;
    SP_extract_face_rect_chips: function(hnd: TSP_Handle; const raster_ptr: PRColorArray; const Width, Height, extract_face_size: Integer): TFACE_Handle; stdcall;
    SP_extract_face_rect: function(const raster_ptr: PRColorArray; const Width, Height: Integer): TFACE_Handle; stdcall;
    SP_close_face_chips_handle: procedure(hnd: TFACE_Handle); stdcall;
    SP_get_face_chips_num: function(hnd: TFACE_Handle): Integer; stdcall;
    SP_get_face_chips_size: procedure(hnd: TFACE_Handle; const index: Integer; var Width, Height: Integer); stdcall;
    SP_get_face_chips_bits: procedure(hnd: TFACE_Handle; const index: Integer; const raster_ptr: PRColorArray); stdcall;
    SP_get_face_rect_num: function(hnd: TFACE_Handle): Integer; stdcall;
    SP_get_face_rect: procedure(hnd: TFACE_Handle; const index: Integer; var AI_Rect: TAI_Rect); stdcall;
    SP_get_num: function(hnd: TFACE_Handle): Integer; stdcall;
    SP_get: function(hnd: TFACE_Handle; const index: Integer; const AI_Point: PAI_Point; const max_AI_Point: Integer): Integer; stdcall;

    { MDNN-ResNet(ResNet metric DNN input net size 150*150, full resnet jitter) }
    MDNN_ResNet_Train: function(param: PMetric_ResNet_Train_Parameter): Integer; stdcall;
    MDNN_ResNet_Full_GPU_Train: function(param: PMetric_ResNet_Train_Parameter): Integer; stdcall;
    MDNN_ResNet_Init: function(train_data: P_Bytes): TMetric_Handle; stdcall;
    MDNN_ResNet_Init_Memory: function(memory: Pointer; Size: Integer): TMetric_Handle; stdcall;
    MDNN_ResNet_Free: function(hnd: TMetric_Handle): Integer; stdcall;
    MDNN_ResNet_Process: function(hnd: TMetric_Handle; imgArry_ptr: PAI_Raster_Data_Array; img_num: Integer; output: PDouble): Integer; stdcall;
    MDNN_DebugInfo: procedure(hnd: TMetric_Handle; var p: PPascalString); stdcall;

    { LMDNN-ResNet(ResNet metric DNN input net size 200*200, resnet no jitter) }
    LMDNN_ResNet_Train: function(param: PMetric_ResNet_Train_Parameter): Integer; stdcall;
    LMDNN_ResNet_Full_GPU_Train: function(param: PMetric_ResNet_Train_Parameter): Integer; stdcall;
    LMDNN_ResNet_Init: function(train_data: P_Bytes): TLMetric_Handle; stdcall;
    LMDNN_ResNet_Init_Memory: function(memory: Pointer; Size: Integer): TLMetric_Handle; stdcall;
    LMDNN_ResNet_Free: function(hnd: TLMetric_Handle): Integer; stdcall;
    LMDNN_ResNet_Process: function(hnd: TLMetric_Handle; imgArry_ptr: PAI_Raster_Data_Array; img_num: Integer; output: PDouble): Integer; stdcall;
    LMDNN_DebugInfo: procedure(hnd: TLMetric_Handle; var p: PPascalString); stdcall;

    { MMOD-DNN(max-margin DNN object detector) 6 Layer }
    MMOD6L_DNN_Train: function(param: PMMOD_Train_Parameter): Integer; stdcall;
    LargeScale_MMOD6L_Train: function(param: PMMOD_Train_Parameter; img_: PPImage_Handle; num_: Integer): Integer; stdcall;
    MMOD6L_DNN_Init: function(train_data: P_Bytes): TMMOD6L_Handle; stdcall;
    MMOD6L_DNN_Init_Memory: function(memory: Pointer; Size: Integer): TMMOD6L_Handle; stdcall;
    MMOD6L_DNN_Free: function(hnd: TMMOD6L_Handle): Integer; stdcall;
    MMOD6L_DNN_Process: function(hnd: TMMOD6L_Handle; const raster_ptr: PRColorArray; const Width, Height: Integer; const MMOD6L_AI_Rect: PAI_MMOD_Rect; const max_AI_Rect: Integer): Integer; stdcall;
    MMOD6L_DNN_Process_Image: function(hnd: TMMOD6L_Handle; Matrix_IMG: TMatrix_Image_Handle; const MMOD6L_AI_Rect: PAI_MMOD_Rect; const max_AI_Rect: Integer): Integer; stdcall;
    MMOD6L_DebugInfo: procedure(hnd: TMMOD6L_Handle; var p: PPascalString); stdcall;

    { MMOD-DNN(max-margin DNN object detector) 3 Layer }
    MMOD3L_DNN_Train: function(param: PMMOD_Train_Parameter): Integer; stdcall;
    LargeScale_MMOD3L_Train: function(param: PMMOD_Train_Parameter; img_: PPImage_Handle; num_: Integer): Integer; stdcall;
    MMOD3L_DNN_Init: function(train_data: P_Bytes): TMMOD3L_Handle; stdcall;
    MMOD3L_DNN_Init_Memory: function(memory: Pointer; Size: Integer): TMMOD3L_Handle; stdcall;
    MMOD3L_DNN_Free: function(hnd: TMMOD3L_Handle): Integer; stdcall;
    MMOD3L_DNN_Process: function(hnd: TMMOD3L_Handle; const raster_ptr: PRColorArray; const Width, Height: Integer; const MMOD3L_AI_Rect: PAI_MMOD_Rect; const max_AI_Rect: Integer): Integer; stdcall;
    MMOD3L_DNN_Process_Image: function(hnd: TMMOD3L_Handle; Matrix_IMG: TMatrix_Image_Handle; const MMOD3L_AI_Rect: PAI_MMOD_Rect; const max_AI_Rect: Integer): Integer; stdcall;
    MMOD3L_DebugInfo: procedure(hnd: TMMOD3L_Handle; var p: PPascalString); stdcall;

    { ResNet-Image-Classifier }
    RNIC_Train: function(param: PRNIC_Train_Parameter): Integer; stdcall;
    RNIC_Init: function(train_data: P_Bytes): TRNIC_Handle; stdcall;
    RNIC_Init_Memory: function(memory: Pointer; Size: Integer): TRNIC_Handle; stdcall;
    RNIC_Free: function(hnd: TRNIC_Handle): Integer; stdcall;
    RNIC_Process: function(hnd: TRNIC_Handle; num_crops: Integer; const raster_ptr: PRColorArray; const Width, Height: Integer; output: PDouble): Integer; stdcall;
    RNIC_Process_Image: function(hnd: TRNIC_Handle; num_crops: Integer; Matrix_IMG: TMatrix_Image_Handle; output: PDouble): Integer; stdcall;
    RNIC_DebugInfo: procedure(hnd: TRNIC_Handle; var p: PPascalString); stdcall;

    { Large-ResNet-Image-Classifier }
    LRNIC_Train: function(param: PRNIC_Train_Parameter): Integer; stdcall;
    LRNIC_Init: function(train_data: P_Bytes): TLRNIC_Handle; stdcall;
    LRNIC_Init_Memory: function(memory: Pointer; Size: Integer): TLRNIC_Handle; stdcall;
    LRNIC_Free: function(hnd: TLRNIC_Handle): Integer; stdcall;
    LRNIC_Process: function(hnd: TLRNIC_Handle; num_crops: Integer; const raster_ptr: PRColorArray; const Width, Height: Integer; output: PDouble): Integer; stdcall;
    LRNIC_Process_Image: function(hnd: TLRNIC_Handle; num_crops: Integer; Matrix_IMG: TMatrix_Image_Handle; output: PDouble): Integer; stdcall;
    LRNIC_DebugInfo: procedure(hnd: TLRNIC_Handle; var p: PPascalString); stdcall;

    { Going Deeper with Convolutions net-Image-Classifier }
    GDCNIC_Train: function(param: PGDCNIC_Train_Parameter): Integer; stdcall;
    GDCNIC_Init: function(train_data: P_Bytes): TGDCNIC_Handle; stdcall;
    GDCNIC_Init_Memory: function(memory: Pointer; Size: Integer): TGDCNIC_Handle; stdcall;
    GDCNIC_Free: function(hnd: TGDCNIC_Handle): Integer; stdcall;
    GDCNIC_Process: function(hnd: TGDCNIC_Handle; const raster_ptr: PRColorArray; const Width, Height: Integer; output: PDouble): Integer; stdcall;
    GDCNIC_DebugInfo: procedure(hnd: TGDCNIC_Handle; var p: PPascalString); stdcall;

    { Gradient-based net-Image-Classifier }
    GNIC_Train: function(param: PGNIC_Train_Parameter): Integer; stdcall;
    GNIC_Init: function(train_data: P_Bytes): TGNIC_Handle; stdcall;
    GNIC_Init_Memory: function(memory: Pointer; Size: Integer): TGNIC_Handle; stdcall;
    GNIC_Free: function(hnd: TGNIC_Handle): Integer; stdcall;
    GNIC_Process: function(hnd: TGNIC_Handle; const raster_ptr: PRColorArray; const Width, Height: Integer; output: PDouble): Integer; stdcall;
    GNIC_DebugInfo: procedure(hnd: TGNIC_Handle; var p: PPascalString); stdcall;

    { segmantic segmentation }
    SS_Train: function(param: PSS_Train_Parameter): Integer; stdcall;
    SS_Init: function(train_data: P_Bytes): TSS_Handle; stdcall;
    SS_Init_Memory: function(memory: Pointer; Size: Integer): TSS_Handle; stdcall;
    SS_Free: function(hnd: TSS_Handle): Integer; stdcall;
    SS_Process: function(hnd: TSS_Handle; const raster_ptr: PRColorArray; const Width, Height: Integer; output: PWORD): Integer; stdcall;
    SS_Process_Image: function(hnd: TSS_Handle; Matrix_IMG: TMatrix_Image_Handle; output: PWORD): Integer; stdcall;
    SS_DebugInfo: procedure(hnd: TSS_Handle; var p: PPascalString); stdcall;

    { correlation video tracker }
    Start_Tracker: function(rgb_img: TRGB_Image_Handle; AI_Rect: PAI_Rect): TTracker_Handle; stdcall;
    Update_Tracker: function(hnd: TTracker_Handle; rgb_img: TRGB_Image_Handle; var AI_Rect: TAI_Rect): Double; stdcall;
    Update_Tracker_NoScale: function(hnd: TTracker_Handle; rgb_img: TRGB_Image_Handle; var AI_Rect: TAI_Rect): Double; stdcall;
    Start_Tracker_matrix: function(mat_img: TMatrix_Image_Handle; AI_Rect: PAI_Rect): TTracker_Handle; stdcall;
    Update_Tracker_matrix: function(hnd: TTracker_Handle; mat_img: TMatrix_Image_Handle; var AI_Rect: TAI_Rect): Double; stdcall;
    Update_Tracker_NoScale_matrix: function(hnd: TTracker_Handle; mat_img: TMatrix_Image_Handle; var AI_Rect: TAI_Rect): Double; stdcall;
    Stop_Tracker: function(hnd: TTracker_Handle): Integer; stdcall;

    { ocr }
    OpenOCREngine: function(ocrData, ocrLang: P_Bytes): TOCR_Handle; stdcall;
    CloseOCREngine: procedure(hnd: TOCR_Handle); stdcall;
    SetOCRParameter: procedure(hnd: TOCR_Handle; paramKey, paramValue: P_Bytes); stdcall;
    PrintOCRParameter: procedure(hnd: TOCR_Handle); stdcall;
    ProcessOCR: function(hnd: TOCR_Handle; data: Pointer; dataSize: Cardinal; mode: Integer): Integer; stdcall;
    GetOCR_ResultText: function(hnd: TOCR_Handle): Pointer; stdcall;
    GetOCR_ResultHTML: function(hnd: TOCR_Handle): Pointer; stdcall;
    GetOCR_ResultXML: function(hnd: TOCR_Handle): Pointer; stdcall;
    GetOCR_ResultTSV: function(hnd: TOCR_Handle): Pointer; stdcall;
    GetOCR_ResultLSTMBoxText: function(hnd: TOCR_Handle): Pointer; stdcall;
    GetOCR_ResultBoxText: function(hnd: TOCR_Handle): Pointer; stdcall;
    GetOCR_ResultWordStrBoxText: function(hnd: TOCR_Handle): Pointer; stdcall;
    GetOCR_ResultOSDText: function(hnd: TOCR_Handle): Pointer; stdcall;

    { Z-Metric }
    ZMetric_Full_GPU_Train: function(param: PZMetric_Train_Parameter): Integer; stdcall;
    ZMetric_Init: function(train_data: P_Bytes): TZMetric_Handle; stdcall;
    ZMetric_Init_Memory: function(memory: Pointer; Size: Integer): TZMetric_Handle; stdcall;
    ZMetric_Free: function(hnd: TZMetric_Handle): Integer; stdcall;
    ZMetric_Process: function(hnd: TZMetric_Handle; imgArry_ptr: PAI_Raster_Data_Array; img_num: Integer; output: PDouble): Integer; stdcall;
    ZMetric_DebugInfo: procedure(hnd: TZMetric_Handle; var p: PPascalString); stdcall;

    { check key }
    CheckKey: function(): Integer; stdcall;
    { print key state }
    printKeyState: procedure(); stdcall;
    { close ai entry }
    CloseAI: procedure(); stdcall;
    SetComputeDeviceOfProcess: function(device_id: Integer): Integer; stdcall;
    GetComputeDeviceOfProcess: function(): Integer; stdcall;
    GetComputeDeviceNumOfProcess: function(): Integer; stdcall;
    GetComputeDeviceNameOfProcess: function(device_id: Integer): Pointer; stdcall;

    { backcall api }
    API_OnOneStep: procedure(Sender: PAI_EntryAPI; one_step_calls: UInt64; average_loss, learning_rate: Double); stdcall;
    API_OnPause: procedure(); stdcall;
    API_Status_Out: procedure(Sender: PAI_EntryAPI; i_char: Integer); stdcall;
    API_GetTimeTick64: function(): UInt64; stdcall;
    API_BuildString: function(p: Pointer; Size: Integer): Pointer; stdcall;
    API_FreeString: procedure(p: Pointer); stdcall;
    API_GetRaster: function(hnd: PRaster_Handle; var Bits: Pointer; var Width, Height: Integer): Byte; stdcall;
    API_GetImage: function(hnd: PImage_Handle; var Bits: Pointer; var Width, Height: Integer): Byte; stdcall;
    API_RecycleImage: function(Sender: PAI_EntryAPI; hnd: PImage_Handle): Byte; stdcall;
    API_GetDetectorDefineNum: function(hnd: PImage_Handle): Integer; stdcall;
    API_GetDetectorDefineImage: function(hnd: PImage_Handle; detIndex: Integer; var Bits: Pointer; var Width, Height: Integer): Byte; stdcall;
    API_GetDetectorDefineRect: function(hnd: PImage_Handle; detIndex: Integer; var rect_: TAI_Rect): Byte; stdcall;
    API_GetDetectorDefineLabel: function(hnd: PImage_Handle; detIndex: Integer; var p: P_Bytes): Byte; stdcall;
    API_FreeDetectorDefineLabel: procedure(var p: P_Bytes); stdcall;
    API_GetDetectorDefinePartNum: function(hnd: PImage_Handle; detIndex: Integer): Integer; stdcall;
    API_GetDetectorDefinePart: function(hnd: PImage_Handle; detIndex, partIndex: Integer; var part_: TAI_Point): Byte; stdcall;
    API_GetSegmentationMaskMergeImage: function(hnd: PImage_Handle; var Bits: Pointer; var Width, Height: Integer): Byte; stdcall;
    API_QuerySegmentationMaskColorID: function(cl: PSegmentationColorTable; color: TRColor; def: WORD): WORD; stdcall;

    { version information }
    (*
      1£¬snapshot
      2£¬alpha
      3£¬beta
      4£¬pre
      5£¬RC(Release Candidate)
      6£¬GA(General Availability)
      7£¬release
      8£¬stable
      9£¬current
      10£¬eval
      11£¬Patch
    *)
    MajorVer, MinorVer, VerMode, VerID: Integer;
    { Key information }
    Key: TAI_Key;
    { ComputeDeviceOfTraining (cuda/MKL support) }
    ComputeDeviceOfTraining: array [0 .. 64 - 1] of Integer;

    { internal usage }
    LibraryFile: SystemString;
    LoadLibraryTime: TDateTime;
    OneStepList: TOneStepList;
    Log: TAI_LogList;
    RasterSerialized: TRasterSerialized;
    SerializedTime: TTimeTick;
    Critical: TCritical;

    { morph expression api }
    MorphExpIntf: TMorphExpIntf;

    procedure Lock;
    procedure UnLock;
    function GetVersionName: TPascalString;
    function GetVersionTitle: TPascalString;
    function GetVersionInfo: TPascalString;
  end;
{$ENDREGION 'Entry'}
{$REGION 'AI Core'}

  TAI = class(TCoreClassObject)
{$REGION 'general'}
  protected
    { internal }
    FAI_EntryAPI: PAI_EntryAPI;
    FFace_SP_Hnd: TSP_Handle;
    TrainingControl: TTrainingControl;
    Critical: TCritical;
  public
  var
    { Parallel handle }
    Parallel_OD6L_Hnd: TOD6L_Handle;
    Parallel_OD3L_Hnd: TOD3L_Handle;
    Parallel_OD_Marshal_Hnd: TOD6L_Marshal_Handle;
    Parallel_SP_Hnd: TSP_Handle;

    { root path }
    RootPath: SystemString;

    { deep neural network training state }
    Last_training_average_loss, Last_training_learning_rate, completed_learning_rate: Double;
  public
    // face shape handle
    property Face_SP_Hnd: TSP_Handle read FFace_SP_Hnd;
    { API entry }
    property API: PAI_EntryAPI read FAI_EntryAPI;

    constructor Create;
    class function OpenEngine(libFile: SystemString): TAI; overload;
    class function OpenEngine(lib_p: PAI_EntryAPI): TAI; overload;
    class function OpenEngine: TAI; overload;
    destructor Destroy; override;

    { engine activted }
    function Activted: Boolean;

    { GPU supported }
    function isGPU: Boolean;
    { Intel-MKL supported }
    function isMKL: Boolean;
    { trainer supported }
    function isTrainer: Boolean;

    { set GPU/MKL compute device for Training }
    procedure SetComputeDeviceOfTraining(const Device_: TLIVec);
    procedure GetComputeDeviceOfTraining(var Device_: TLIVec);
    { set GPU/MKL compute device for process }
    function SetComputeDeviceOfProcess(device_id: Integer): Boolean;
    { get current GPU/MKL compute device for process }
    function GetComputeDeviceOfProcess(): Integer;
    { get GPU/MKL compute device number for process }
    function GetComputeDeviceNumOfProcess(): Integer;
    { get GPU/MKL compute device name for process }
    function GetComputeDeviceNameOfProcess(device_id: Integer): U_String;
    function GetComputeDeviceNames(): U_StringArray; overload;
    procedure GetComputeDeviceNames(output: TCoreClassStrings); overload;

    { MemoryRasterSerialized }
    function MakeSerializedFileName: U_String;

    { atomic ctrl }
    procedure Lock;
    procedure UnLock;
    function Busy: Boolean;

    { training control }
    procedure Training_Stop;
    procedure Training_Pause;
    procedure Training_Continue;
    function Training_IsPause: Boolean;
{$ENDREGION 'general'}
{$REGION 'graphics'}
    { structor draw }
    procedure DrawOD6L(OD_Hnd: TOD6L_Handle; Raster: TMemoryRaster; color: TDEColor);
    procedure DrawOD3L(OD_Hnd: TOD3L_Handle; Raster: TMemoryRaster; color: TDEColor);
    procedure DrawOD(OD_Desc: TOD_Desc; Raster: TMemoryRaster; color: TDEColor);
    procedure DrawODM(odm_hnd: TOD6L_Marshal_Handle; Raster: TMemoryRaster; color: TDEColor);
    procedure DrawSP(OD_Hnd: TOD6L_Handle; sp_hnd: TSP_Handle; Raster: TMemoryRaster);
    function DrawMMOD(MMOD_hnd: TMMOD6L_Handle; Raster: TMemoryRaster; color: TDEColor; fontSiz: TGeoFloat): TMMOD_Desc; overload;
    function DrawMMOD(MMOD_hnd: TMMOD6L_Handle; Raster: TMemoryRaster; color: TDEColor): TMMOD_Desc; overload;
    function DrawMMOD(MMOD_hnd: TMMOD6L_Handle; confidence: Double; Raster: TMemoryRaster; color: TDEColor; fontSiz: TGeoFloat): Integer; overload;
    function DrawMMOD(MMOD_hnd: TMMOD6L_Handle; confidence: Double; Raster: TMemoryRaster; color: TDEColor): Integer; overload;
    function DrawMMOD(MMOD_Desc: TMMOD_Desc; Raster: TMemoryRaster; color: TDEColor): Integer; overload;
    procedure DrawFace(Raster: TMemoryRaster); overload;
    procedure DrawFace(face_hnd: TFACE_Handle; d: TDrawEngine); overload;
    procedure DrawFace(face_hnd: TFACE_Handle; d: TDrawEngine; sourBox, destBox: TRectV2); overload;
    procedure DrawFace(Raster: TMemoryRaster; Metric_hnd: TMetric_Handle; Face_Learn: TLearn; faceAccuracy: TGeoFloat; lineColor, TextColor: TDEColor); overload;
    procedure PrintFace(prefix: SystemString; Raster: TMemoryRaster; Metric_hnd: TMetric_Handle; Face_Learn: TLearn; faceAccuracy: TGeoFloat);
    function DrawExtractFace(Raster: TMemoryRaster): TMemoryRaster;

    { prepare image }
    function Prepare_RGB_Image(Raster: TMemoryRaster): TRGB_Image_Handle;
    procedure Close_RGB_Image(hnd: TRGB_Image_Handle);
    function Prepare_Matrix_Image(Raster: TMemoryRaster): TMatrix_Image_Handle;
    procedure Close_Matrix_Image(hnd: TMatrix_Image_Handle);

    { Build-in RGB to Raster }
    procedure BuildRGBRaster(hnd_RGB: TRGB_Image_Handle; output: TMemoryRaster); overload;
    function BuildRGBRaster(hnd_RGB: TRGB_Image_Handle): TMemoryRaster; overload;

    { Build-in Matrix to Raster }
    procedure BuildMatrixRaster(hnd_Matrix: TMatrix_Image_Handle; output: TMemoryRaster); overload;
    function BuildMatrixRaster(hnd_Matrix: TMatrix_Image_Handle): TMemoryRaster; overload;

    { Medical graphic support }
    procedure HotMap(Raster: TMemoryRaster);
    procedure JetMap(Raster: TMemoryRaster);
    function BuildHotMap(Raster: TMemoryRaster): TMemoryRaster;
    function BuildJetMap(Raster: TMemoryRaster): TMemoryRaster;
{$ENDREGION 'graphics'}
{$REGION 'Efficient Graph-Based Image Segmentation'}
    (*
      paper: Efficient Graph-Based Image Segmentation
      url https://wenku.baidu.com/view/f21203d726fff705cc170a33.html
      post by 2004

      author:
      Pedro F. Felzenszwalb
      Artificial Intelligence Lab, Massachusetts Institute  of Technology
      pff@ai.mit.edu

      Daniel P. Huttenlocher
      Computer Science Department, Cornell University
      dph@cs.cornell.edu

      Abstract:
      This paper addresses the problem of segmenting an image into regions.
      We define a predicate for measuring the evidence for a boundary between two regions using a graph-based representation of the image.
      We then develop an efficient segmentation algorithm based on this predicate,
      and show that although this algorithm makes greedy decisions it produces segmentations that satisfy global properties.
      We apply the al- gorithm to image segmentation using two different kinds of local neighborhoods in constructing the graph,
      and illustrate the results with both real and synthetic images.
      The algorithm runs in time nearly linear in the number of graph edges and is also fast in practice.
      An important characteristic of the method is its ability to preserve detail in low-variability image regions while ignoring detail in high-variability regions.
      Keywords: image segmentation, clustering, perceptual organization, graph algorithm.
    *)
    function Segment(Raster: TMemoryRaster; const k: Double; const min_siz: Integer): TMemoryRaster; overload;
    function Segment(Raster: TMemoryRaster): TMemoryRaster; overload;

{$ENDREGION 'Efficient Graph-Based Image Segmentation'}
{$REGION 'Minimum Barrier Salient Object Detection at 80 FPS'}
    (*
      paper: Minimum Barrier Salient Object Detection at 80 FPS
      url http://openaccess.thecvf.com/content_iccv_2015/papers/Zhang_Minimum_Barrier_Salient_ICCV_2015_paper.pdf
      post by 2015

      author
      Boston University: Jianming Zhang1
      Boston University: Stan Sclaroff1
      Adobe Research:Zhe Lin2
      Adobe Research:Xiaohui Shen2
      Adobe Research:Brian Price2
      Adobe Research:Radomir Mech2

      Abstract
      We propose a highly efficient, yet powerful, salient object detection method based on the Minimum Barrier Distance (MBD) Transform.
      The MBD transform is robust to pixel- value fluctuation, and thus can be effectively applied on raw pixels without region abstraction.
      We present an approx- imate MBD transform algorithm with 100X speedup over the exact algorithm.
      An error bound analysis is also pro- vided. Powered by this fast MBD transform algorithm,
      the proposed salient object detection method runs at 80 FPS, and significantly outperforms previous methods with similar speed on four large benchmark datasets,
      and achieves com-parable or better performance than state-of-the-art meth- ods.
      Furthermore, a technique based on color whitening is proposed to extend our method to leverage the appearance- based backgroundness cue.
      This extended version further improves the performance, while still being one order of
      magnitude faster than all the other leading methods.
    *)
    function Salient(Raster: TMemoryRaster; const iterations: Integer): TMemoryRaster; overload;
    function Salient(Raster: TMemoryRaster): TMemoryRaster; overload;

{$ENDREGION 'Minimum Barrier Salient Object Detection at 80 FPS'}
{$REGION 'Segmentation as Selective Search for Object Recognition'}
    (*
      paper: Segmentation as Selective Search for Object Recognition
      url https://www.koen.me/research/selectivesearch/
      post by 2011

      Abstract
      For object recognition, the current state-of-the-art is based on exhaustive search. However,
      to enable the use of more expensive features and classifiers and thereby progress beyond the state-of-the-art,
      a selective search strategy is needed. Therefore,

      we adapt segmentation as a selective search by reconsidering segmentation:
      We propose to generate many approximate locations over few and precise object delineations because
      (1) an object whose location is never generated can not be recognised and
      (2) appearance and immediate nearby context are most effective for object recognition.

      Our method is class-independent and is shown to cover 96.7% of all objects in the Pascal VOC 2007 test set using only 1,536 locations per image.

      Our selective search enables the use of the more expensive bag-of-words method
      which we use to substantially improve the state-of-the-art by up to 8.5% for 8 out of 20 classes on the Pascal VOC 2010 detection challenge.
    *)

    function CandidateObject(Raster: TMemoryRaster; const min_size, max_merging_iterations: Integer): TAI_Rect_Desc; overload;
    function CandidateObject(Raster: TMemoryRaster): TAI_Rect_Desc; overload;
{$ENDREGION 'Segmentation as Selective Search for Object Recognition'}
{$REGION 'Unmixing-Based Soft Color Segmentation for Image Manipulation'}
    (*
      paper: Unmixing-Based Soft Color Segmentation for Image Manipulation
      url https://cvg.ethz.ch/research/soft-color-segmentation/
      post by 2017
      reference material http://staff.ustc.edu.cn/~zhuang/acg/SIGGRAPH-2017-papers.pdf

      abstract
      We present a new method for decomposing an image into a set of soft color segments that are analogous to color layers with alpha channels that have been commonly utilized in modern image manipulation software.
      We show that the resulting decomposition serves as an effective intermediate image representation, which can be utilized for performing various, seemingly unrelated,
      image manipulation tasks. We identify a set of requirements that soft color segmentation methods have to fulfill,
      and present an in-depth theoretical analysis of prior work. We propose an energy formulation for producing compact layers of homogeneous colors and a color refinement procedure,
      as well as a method for automatically estimating a statistical color model from an image.

      This results in a novel framework for automatic and high-quality soft color segmentation that is efficient, parallelizable, and scalable.
      We show that our technique is superior in quality compared to previous methods through quantitative analysis as well as visually through an extensive set of examples.

      We demonstrate that our soft color segments can easily be exported to familiar image manipulation software packages and used to produce compelling results for numerous image manipulation,
      applications without forcing the user to learn new tools and workflows.
    *)
    function Unmixing_Raster(Raster: TMemoryRaster): PUnmixedData;
    procedure Unmixing_Free(var data: PUnmixedData);
{$ENDREGION 'Unmixing-Based Soft Color Segmentation for Image Manipulation'}
{$REGION 'Poisson Image Editing'}
    (*
      paper: Poisson Image Editing
      url http://www.cs.virginia.edu/~connelly/class/2014/comp_photo/proj2/poisson.pdf
      post by 2014

      author
      Patrick Perez, Michel Gangnet, Andrew Blake
      Microsoft Research UK

      Abstract
      Using generic interpolation machinery based on solving Poisson equations,
      a variety of novel tools are introduced for seamless edit- ing of image regions.
      The first set of tools permits the seamless importation of both opaque and transparent source image regions into a destination region.
      The second set is based on similar math- ematical ideas and allows the user to modify the appearance of the image seamlessly, within a selected region.
      These changes can be arranged to affect the texture, the illumination,
      and the color of ob-jects lying in the region, or to make tileable a rectangular selection.
    *)
    procedure PoissonBlend(GAMMA: Double; sour, dest: TMemoryRaster; dest_x, dest_y: Integer; PaperMethod: Boolean);

{$ENDREGION 'Poisson Image Editing'}
{$REGION 'GrabCut ¡ª Interactive Foreground Extraction using Iterated Graph Cuts'}
    (*
      paper: GrabCut ¡ª Interactive Foreground Extraction using Iterated Graph Cuts
      url http://www.cvg.ethz.ch/teaching/cvl/2012/grabcut-siggraph04.pdf
      post by 2004

      Published in: international conference on computer graphics and interactive techniques ¡¤ 2004
      Authors: Carsten Rother ¡¤ Vladimir Kolmogorov ¡¤ Andrew Blake
      Affiliation: Microsoft

      Abstract
      The problem of efficient, interactive foreground/background seg-mentation in still images is of great practical importance in im-age editing.
      Classical image segmentation tools use either texture(colour) information, e.g.  Magic Wand, or edge (contrast) infor-mation,
      e.g. Intelligent Scissors. Recently, an approach based onoptimization by graph-cut has been developed which successfullycombines both types of information.
      In this paper we extend thegraph-cut approach in three respects. First, we have developed amore powerful, iterative version of the optimisation.
      Secondly, thepower of the iterative algorithm is used to simplify substantially theuser interaction needed for a given quality of result.

      We show that for moderately difficultexamples the proposed method outperforms competitive tools.CR Categories:
      I.3.3 [Computer Graphics]:  Picture/ImageGeneration¡ªDisplay  algorithms;
      I.3.6  [Computer  Graphics]:Methodology and Techniques¡ªInteraction techniques;
      I.4.6 [Im-age Processing and Computer Vision]: Segmentation¡ªPixel clas-sification;


      data input
      0: an obvious background pixels
      1: an obvious foreground (object) pixel
      2: a possible background pixel
      3: a possible foreground pixel

      data output
      0:  background
      FF: forgeground

      mode defined

      C_CUT_MODE_INIT_WITH_RECT = 0;
      The function initializes the state and the mask using the provided rectangle.
      After that it runs iterCount iterations of the algorithm.

      C_CUT_MODE_INIT_WITH_MASK = 1;
      The function initializes the state using the provided mask.
      Note that GC_INIT_WITH_RECT and C_CUT_MODE_INIT_WITH_RECT can be combined.
      Then, all the pixels outside of the ROI are automatically initialized with GC_BGD .

      C_CUT_MODE_GC_EVAL = 2;
      The value means that the algorithm should just resume.

      C_CUT_MODE_EVAL_FREEZE_MODEL = 3;
      The value means that the algorithm should just run the grabCut algorithm (a single iteration) with the fixed model
    *)
    function CutRaster(Raster: TMemoryRaster; inputData: PCutData; box: TRect; iterCount, mode: Integer): TMorphologyBinaryzation;
    function BuildCutConvolutionGeometry(Raster: TMemoryRaster; box: TRect;
      Remove_Noise, Convolutionsiz: Integer; binOperation: TBinaryzationOperation; vetex_reduce: TGeoFloat): T2DPolygonGraph;
    function BuildCutGeometry(Raster: TMemoryRaster; box: TRect; Remove_Noise: Integer; vetex_reduce: TGeoFloat): T2DPolygonGraph;

{$ENDREGION 'GrabCut ¡ª Interactive Foreground Extraction using Iterated Graph Cuts'}
{$REGION 'Speeded Up Robust Features'}
    { fast surf(cpu) }
    function fast_surf(Raster: TMemoryRaster; const max_points: Integer; const detection_threshold: Double): TSurf_DescBuffer;
    function surf_sqr(const sour, dest: PSurf_Desc): Single; inline;
    function Surf_Matched(reject_ratio_sqr: Single; r1_, r2_: TMemoryRaster; sd1_, sd2_: TSurf_DescBuffer): TSurfMatchedBuffer;
    procedure BuildFeatureView(Raster: TMemoryRaster; descbuff: TSurf_DescBuffer);
    function BuildMatchInfoView(var MatchInfo: TSurfMatchedBuffer): TMemoryRaster;
    function BuildSurfMatchOutput(raster1, raster2: TMemoryRaster): TMemoryRaster;
{$ENDREGION 'Speeded Up Robust Features'}
{$REGION 'object detector 6 layer'}
    { object detector 6 layer training(cpu), usage XML swap dataset. }
    function OD6L_Train(train_cfg, train_output: U_String; window_w, window_h, thread_num: Integer): Boolean; overload;
    function OD6L_Train(imgList: TAI_ImageList; TokenFilter, train_output: U_String; window_w, window_h, thread_num: Integer): Boolean; overload;
    function OD6L_Train(imgMat: TAI_ImageMatrix; TokenFilter, train_output: U_String; window_w, window_h, thread_num: Integer): Boolean; overload;
    function OD6L_Train_Stream(imgList: TAI_ImageList; window_w, window_h, thread_num: Integer): TMemoryStream64; overload;
    function OD6L_Train_Stream(imgMat: TAI_ImageMatrix; window_w, window_h, thread_num: Integer): TMemoryStream64; overload;
    { large-scale object detector 6 layer training(cpu), direct input without XML swap dataset. }
    function LargeScale_OD6L_Train(imgList: TAI_ImageList; train_output: U_String; window_w, window_h, thread_num: Integer): Boolean; overload;
    function LargeScale_OD6L_Train(imgMat: TAI_ImageMatrix; train_output: U_String; window_w, window_h, thread_num: Integer): Boolean; overload;
    function LargeScale_OD6L_Train_Stream(imgList: TAI_ImageList; window_w, window_h, thread_num: Integer): TMemoryStream64; overload;
    function LargeScale_OD6L_Train_Stream(imgMat: TAI_ImageMatrix; window_w, window_h, thread_num: Integer): TMemoryStream64; overload;
    { object detector 6 layer api(cpu) }
    function OD6L_Open(train_file: SystemString): TOD6L_Handle;
    function OD6L_Open_Stream(stream: TMemoryStream64): TOD6L_Handle; overload;
    function OD6L_Open_Stream(train_file: SystemString): TOD6L_Handle; overload;
    function OD6L_Close(var hnd: TOD6L_Handle): Boolean;
    function OD6L_Process(hnd: TOD6L_Handle; Raster: TMemoryRaster; const max_AI_Rect: Integer): TOD_Desc; overload;
    function OD6L_Process(hnd: TOD6L_Handle; Raster: TMemoryRaster): TOD_List; overload;
    procedure OD6L_Process(hnd: TOD6L_Handle; Raster: TMemoryRaster; output: TOD_List); overload;
    function OD6L_ProcessRGB(hnd: TOD6L_Handle; rgb_img: TRGB_Image_Handle; const max_AI_Rect: Integer): TOD_Desc; overload;
    function OD6L_ProcessScaleSpace(hnd: TOD6L_Handle; Raster: TMemoryRaster; scale: TGeoFloat): TOD_Desc; overload;
{$ENDREGION 'object detector 6 layer'}
{$REGION 'object detector 3 layer'}
    { object detector 3 layer training(cpu), usage XML swap dataset. }
    function OD3L_Train(train_cfg, train_output: U_String; window_w, window_h, thread_num: Integer): Boolean; overload;
    function OD3L_Train(imgList: TAI_ImageList; TokenFilter, train_output: U_String; window_w, window_h, thread_num: Integer): Boolean; overload;
    function OD3L_Train(imgMat: TAI_ImageMatrix; TokenFilter, train_output: U_String; window_w, window_h, thread_num: Integer): Boolean; overload;
    function OD3L_Train_Stream(imgList: TAI_ImageList; window_w, window_h, thread_num: Integer): TMemoryStream64; overload;
    function OD3L_Train_Stream(imgMat: TAI_ImageMatrix; window_w, window_h, thread_num: Integer): TMemoryStream64; overload;
    { large-scale object detector 3 layer training(cpu), direct input without XML swap dataset. }
    function LargeScale_OD3L_Train(imgList: TAI_ImageList; train_output: U_String; window_w, window_h, thread_num: Integer): Boolean; overload;
    function LargeScale_OD3L_Train(imgMat: TAI_ImageMatrix; train_output: U_String; window_w, window_h, thread_num: Integer): Boolean; overload;
    function LargeScale_OD3L_Train_Stream(imgList: TAI_ImageList; window_w, window_h, thread_num: Integer): TMemoryStream64; overload;
    function LargeScale_OD3L_Train_Stream(imgMat: TAI_ImageMatrix; window_w, window_h, thread_num: Integer): TMemoryStream64; overload;
    { object detector 3 layer api(cpu) }
    function OD3L_Open(train_file: SystemString): TOD3L_Handle;
    function OD3L_Open_Stream(stream: TMemoryStream64): TOD3L_Handle; overload;
    function OD3L_Open_Stream(train_file: SystemString): TOD3L_Handle; overload;
    function OD3L_Close(var hnd: TOD3L_Handle): Boolean;
    function OD3L_Process(hnd: TOD3L_Handle; Raster: TMemoryRaster; const max_AI_Rect: Integer): TOD_Desc; overload;
    function OD3L_Process(hnd: TOD3L_Handle; Raster: TMemoryRaster): TOD_List; overload;
    procedure OD3L_Process(hnd: TOD3L_Handle; Raster: TMemoryRaster; output: TOD_List); overload;
    function OD3L_ProcessRGB(hnd: TOD3L_Handle; rgb_img: TRGB_Image_Handle; const max_AI_Rect: Integer): TOD_Desc; overload;
    function OD3L_ProcessScaleSpace(hnd: TOD3L_Handle; Raster: TMemoryRaster; scale: TGeoFloat): TOD_Desc; overload;
{$ENDREGION 'object detector 3 layer'}
{$REGION 'object marshal(6 layer) detector'}
    { object marshal detector(6 layer) training(cpu), usage XML swap dataset. }
    function OD6L_Marshal_Train(imgList: TAI_ImageList; window_w, window_h, thread_num: Integer): TMemoryStream64; overload;
    function OD6L_Marshal_Train(imgMat: TAI_ImageMatrix; window_w, window_h, thread_num: Integer): TMemoryStream64; overload;
    { object marshal detector(6 layer) api(cpu) }
    function OD6L_Marshal_Open_Stream(stream: TMemoryStream64): TOD6L_Marshal_Handle; overload;
    function OD6L_Marshal_Open_Stream(train_file: SystemString): TOD6L_Marshal_Handle; overload;
    function OD6L_Marshal_Close(var hnd: TOD6L_Marshal_Handle): Boolean;
    function OD6L_Marshal_Process(hnd: TOD6L_Marshal_Handle; Raster: TMemoryRaster): TOD_Marshal_Desc;
    function OD6L_Marshal_ProcessScaleSpace(hnd: TOD6L_Marshal_Handle; Raster: TMemoryRaster; scale: TGeoFloat): TOD_Marshal_Desc;
{$ENDREGION 'object marshal(6 layer) detector'}
{$REGION 'shape predictor and shape detector'}
    { shape predictor and shape detector training(cpu), usage XML swap dataset. }
    function SP_Train(train_cfg, train_output: U_String; oversampling_amount, tree_depth, thread_num: Integer): Boolean; overload;
    function SP_Train(imgList: TAI_ImageList; train_output: U_String; oversampling_amount, tree_depth, thread_num: Integer): Boolean; overload;
    function SP_Train(imgMat: TAI_ImageMatrix; train_output: U_String; oversampling_amount, tree_depth, thread_num: Integer): Boolean; overload;
    function SP_Train_Stream(imgList: TAI_ImageList; oversampling_amount, tree_depth, thread_num: Integer): TMemoryStream64; overload;
    function SP_Train_Stream(imgMat: TAI_ImageMatrix; oversampling_amount, tree_depth, thread_num: Integer): TMemoryStream64; overload;
    { large-scale shape predictor and shape detector training(cpu), direct input without XML swap dataset. }
    function LargeScale_SP_Train(imgList: TAI_ImageList; train_output: U_String; oversampling_amount, tree_depth, thread_num: Integer): Boolean; overload;
    function LargeScale_SP_Train(imgMat: TAI_ImageMatrix; train_output: U_String; oversampling_amount, tree_depth, thread_num: Integer): Boolean; overload;
    function LargeScale_SP_Train_Stream(imgList: TAI_ImageList; oversampling_amount, tree_depth, thread_num: Integer): TMemoryStream64; overload;
    function LargeScale_SP_Train_Stream(imgMat: TAI_ImageMatrix; oversampling_amount, tree_depth, thread_num: Integer): TMemoryStream64; overload;
    { shape predictor and shape detector api(cpu) }
    function SP_Open(train_file: SystemString): TSP_Handle;
    function SP_Open_Stream(stream: TMemoryStream64): TSP_Handle; overload;
    function SP_Open_Stream(train_file: SystemString): TSP_Handle; overload;
    function SP_Close(var hnd: TSP_Handle): Boolean;
    function SP_Process(hnd: TSP_Handle; Raster: TMemoryRaster; const AI_Rect: TAI_Rect; const max_AI_Point: Integer): TSP_Desc; overload;
    function SP_Process_Vec2List(hnd: TSP_Handle; Raster: TMemoryRaster; const R: TRectV2): TVec2List; overload;
    function SP_Process_Vec2(hnd: TSP_Handle; Raster: TMemoryRaster; const R: TRectV2): TArrayVec2; overload;
    function SP_Process_Vec2(hnd: TSP_Handle; Raster: TMemoryRaster; const R: TAI_Rect): TArrayVec2; overload;
    function SP_Process_Vec2(hnd: TSP_Handle; Raster: TMemoryRaster; const R: TOD_Rect): TArrayVec2; overload;
    function SP_Process_Face(Raster: TMemoryRaster; const R: TRectV2): TArrayVec2;
    function SP_ProcessRGB(hnd: TSP_Handle; rgb_img: TRGB_Image_Handle; const AI_Rect: TAI_Rect; const max_AI_Point: Integer): TSP_Desc; overload;
    function SP_ProcessRGB_Vec2List(hnd: TSP_Handle; rgb_img: TRGB_Image_Handle; const R: TRectV2): TVec2List; overload;
    function SP_ProcessRGB_Vec2(hnd: TSP_Handle; rgb_img: TRGB_Image_Handle; const R: TRectV2): TArrayVec2; overload;
    function SP_ProcessRGB_Vec2(hnd: TSP_Handle; rgb_img: TRGB_Image_Handle; const R: TAI_Rect): TArrayVec2; overload;
    function SP_ProcessRGB_Vec2(hnd: TSP_Handle; rgb_img: TRGB_Image_Handle; const R: TOD_Rect): TArrayVec2; overload;
{$ENDREGION 'shape predictor and shape detector'}
{$REGION 'face shape predictor'}
    { face shape predictor(cpu) }
    procedure PrepareFaceDataSource;
    function Face_Detector(Raster: TMemoryRaster; R: TRect; extract_face_size: Integer): TFACE_Handle; overload;
    function Face_Detector(Raster: TMemoryRaster; desc: TAI_Rect_Desc; extract_face_size: Integer): TFACE_Handle; overload;
    function Face_Detector(Raster: TMemoryRaster; MMOD_Desc: TMMOD_Desc; extract_face_size: Integer): TFACE_Handle; overload;
    function Face_Detector(Raster: TMemoryRaster; OD_Desc: TOD_Desc; extract_face_size: Integer): TFACE_Handle; overload;
    function Face_DetectorAsChips(Raster: TMemoryRaster; desc: TAI_Rect; extract_face_size: Integer): TMemoryRaster;
    function Face_Detector_All(Raster: TMemoryRaster): TFACE_Handle; overload;
    function Face_Detector_All(Raster: TMemoryRaster; extract_face_size: Integer): TFACE_Handle; overload;
    function Face_Detector_Rect(Raster: TMemoryRaster): TFACE_Handle;
    function Face_Detector_AllRect(Raster: TMemoryRaster): TAI_Rect_Desc;
    function Face_chips_num(hnd: TFACE_Handle): Integer;
    function Face_chips(hnd: TFACE_Handle; index: Integer): TMemoryRaster;
    function Face_GetCentreRectIndex(Raster: TMemoryRaster; hnd: TFACE_Handle): Integer;
    function Face_Rect_Num(hnd: TFACE_Handle): Integer;
    function Face_Rect(hnd: TFACE_Handle; index: Integer): TAI_Rect;
    function Face_RectV2(hnd: TFACE_Handle; index: Integer): TRectV2;
    function Face_Shape_num(hnd: TFACE_Handle): Integer;
    function Face_Shape(hnd: TFACE_Handle; index: Integer): TSP_Desc;
    function Face_ShapeV2(hnd: TFACE_Handle; index: Integer): TArrayVec2;
    function Face_Shape_rect(hnd: TFACE_Handle; index: Integer): TRectV2;
    procedure Face_Close(var hnd: TFACE_Handle);
{$ENDREGION 'face shape predictor'}
{$REGION 'MDNN-ResNet(ResNet metric DNN)'}
    { MDNN-ResNet(ResNet metric DNN) training(gpu), extract dim 32, input size 150*150, full resnet jitter, include bias, direct input without XML swap dataset. }
    class function Init_Metric_ResNet_Parameter(train_sync_file, train_output: U_String): PMetric_ResNet_Train_Parameter;
    class procedure Free_Metric_ResNet_Parameter(param: PMetric_ResNet_Train_Parameter);
    { data prototype }
    function Metric_ResNet_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TMemoryRaster2DArray; param: PMetric_ResNet_Train_Parameter): Boolean; overload;
    function Metric_ResNet_Train(Snapshot_: Boolean; imgList: TAI_ImageList; param: PMetric_ResNet_Train_Parameter): Boolean; overload;
    function Metric_ResNet_Train_Stream(Snapshot_: Boolean; imgList: TAI_ImageList; param: PMetric_ResNet_Train_Parameter): TMemoryStream64; overload;
    function Metric_ResNet_Train(Snapshot_: Boolean; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): Boolean; overload;
    function Metric_ResNet_Train_Stream(Snapshot_: Boolean; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): TMemoryStream64; overload;
    { Large-Scale-MDNN-ResNet(ResNet metric DNN) training(gpu), extract dim 32, input size 150*150, full resnet jitter, include bias, direct input without XML swap dataset. }
    function Metric_ResNet_Train(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): Boolean; overload;
    function Metric_ResNet_Train_Stream(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): TMemoryStream64; overload;
    { MDNN-ResNet(ResNet metric DNN) api(gpu), extract dim 256, input size 150*150, full resnet jitter, include bias }
    class function BuildShareFaceLearn(): TLearn;
    class function Build_Metric_ResNet_Learn(): TLearn;
    function Metric_ResNet_Open_ShareFace(): TMetric_Handle;
    function Metric_ResNet_Open(train_file: SystemString): TMetric_Handle;
    function Metric_ResNet_Open_Stream(stream: TMemoryStream64): TMetric_Handle; overload;
    function Metric_ResNet_Open_Stream(train_file: SystemString): TMetric_Handle; overload;
    function Metric_ResNet_Close(var hnd: TMetric_Handle): Boolean;
    function Metric_ResNet_Process(hnd: TMetric_Handle; RasterArray: TMemoryRasterArray; output: PDouble): Integer; overload;
    function Metric_ResNet_Process(hnd: TMetric_Handle; RasterArray: TMemoryRasterArray): TLMatrix; overload;
    function Metric_ResNet_Process(hnd: TMetric_Handle; Raster: TMemoryRaster): TLVec; overload;
    { DNN Thread tech }
    procedure Metric_ResNet_SaveToLearnEngine_DT(pool_: TAI_DNN_ThreadPool; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; lr: TLearn); overload;
    procedure Metric_ResNet_SaveToLearnEngine_DT(Metric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; lr: TLearn); overload;
    procedure Metric_ResNet_SaveToLearnEngine_DT(Metric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; lr: TLearn); overload;
    procedure Metric_ResNet_SaveToLearnEngine_DT(Metric_stream: TMemoryStream64; Snapshot_: Boolean; imgList: TAI_ImageList; lr: TLearn); overload;
    procedure Metric_ResNet_SaveToLearnEngine_DT(Metric_stream: TMemoryStream64; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; lr: TLearn); overload;
    { normal }
    procedure Metric_ResNet_SaveToLearnEngine(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; lr: TLearn); overload;
    procedure Metric_ResNet_SaveToLearnEngine(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; lr: TLearn); overload;
    procedure Metric_ResNet_SaveToLearnEngine(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; imgList: TAI_ImageList; lr: TLearn); overload;
    procedure Metric_ResNet_SaveToLearnEngine(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; lr: TLearn); overload;
    procedure Metric_ResNet_SaveToKDTree(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; kd: TKDTreeDataList); overload;
    procedure Metric_ResNet_SaveToKDTree(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; kd: TKDTreeDataList); overload;
    procedure Metric_ResNet_SaveToKDTree(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; imgList: TAI_ImageList; kd: TKDTreeDataList); overload;
    procedure Metric_ResNet_SaveToKDTree(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; kd: TKDTreeDataList); overload;
    function Metric_ResNet_DebugInfo(hnd: TMetric_Handle): U_String;
{$ENDREGION 'MDNN-ResNet(ResNet metric DNN)'}
{$REGION 'Large-Scale-LMDNN-ResNet(ResNet LMetric DNN)'}
    { LMDNN-ResNet(ResNet LMetric DNN) training(gpu), extract dim 24, input size 200*200, no resnet jitter, no bias, direct input without XML swap dataset. }
    class function Init_LMetric_ResNet_Parameter(train_sync_file, train_output: U_String): PMetric_ResNet_Train_Parameter;
    class procedure Free_LMetric_ResNet_Parameter(param: PMetric_ResNet_Train_Parameter);
    { data prototype }
    function LMetric_ResNet_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TMemoryRaster2DArray; param: PMetric_ResNet_Train_Parameter): Boolean; overload;
    function LMetric_ResNet_Train(Snapshot_: Boolean; imgList: TAI_ImageList; param: PMetric_ResNet_Train_Parameter): Boolean; overload;
    function LMetric_ResNet_Train_Stream(Snapshot_: Boolean; imgList: TAI_ImageList; param: PMetric_ResNet_Train_Parameter): TMemoryStream64; overload;
    function LMetric_ResNet_Train(Snapshot_: Boolean; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): Boolean; overload;
    function LMetric_ResNet_Train_Stream(Snapshot_: Boolean; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): TMemoryStream64; overload;
    { Large-Scale-LMDNN-ResNet(ResNet LMetric DNN) training(gpu), extract dim 24, input size 200*200, no resnet jitter, no bias, direct input without XML swap dataset. }
    function LMetric_ResNet_Train(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): Boolean; overload;
    function LMetric_ResNet_Train_Stream(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): TMemoryStream64; overload;
    { LMDNN-ResNet(ResNet LMetric DNN) api(gpu), extract dim 24, input size 200*200, no resnet jitter }
    class function Build_LMetric_ResNet_Learn(): TLearn;
    function LMetric_ResNet_Open(train_file: SystemString): TLMetric_Handle;
    function LMetric_ResNet_Open_Stream(stream: TMemoryStream64): TLMetric_Handle; overload;
    function LMetric_ResNet_Open_Stream(train_file: SystemString): TLMetric_Handle; overload;
    function LMetric_ResNet_Close(var hnd: TLMetric_Handle): Boolean;
    function LMetric_ResNet_Process(hnd: TLMetric_Handle; RasterArray: TMemoryRasterArray; output: PDouble): Integer; overload;
    function LMetric_ResNet_Process(hnd: TLMetric_Handle; RasterArray: TMemoryRasterArray): TLMatrix; overload;
    function LMetric_ResNet_Process(hnd: TLMetric_Handle; Raster: TMemoryRaster): TLVec; overload;
    { DNN Thread tech }
    procedure LMetric_ResNet_SaveToLearnEngine_DT(pool_: TAI_DNN_ThreadPool; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; lr: TLearn); overload;
    procedure LMetric_ResNet_SaveToLearnEngine_DT(LMetric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; lr: TLearn); overload;
    procedure LMetric_ResNet_SaveToLearnEngine_DT(LMetric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; lr: TLearn); overload;
    procedure LMetric_ResNet_SaveToLearnEngine_DT(LMetric_stream: TMemoryStream64; Snapshot_: Boolean; imgList: TAI_ImageList; lr: TLearn); overload;
    procedure LMetric_ResNet_SaveToLearnEngine_DT(LMetric_stream: TMemoryStream64; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; lr: TLearn); overload;
    { normal }
    procedure LMetric_ResNet_SaveToLearnEngine(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; lr: TLearn); overload;
    procedure LMetric_ResNet_SaveToLearnEngine(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; lr: TLearn); overload;
    procedure LMetric_ResNet_SaveToLearnEngine(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; imgList: TAI_ImageList; lr: TLearn); overload;
    procedure LMetric_ResNet_SaveToLearnEngine(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; lr: TLearn); overload;
    procedure LMetric_ResNet_SaveToKDTree(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; kd: TKDTreeDataList); overload;
    procedure LMetric_ResNet_SaveToKDTree(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; kd: TKDTreeDataList); overload;
    procedure LMetric_ResNet_SaveToKDTree(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; imgList: TAI_ImageList; kd: TKDTreeDataList); overload;
    procedure LMetric_ResNet_SaveToKDTree(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; kd: TKDTreeDataList); overload;
    function LMetric_ResNet_DebugInfo(hnd: TLMetric_Handle): U_String;
{$ENDREGION 'Large-Scale-LMDNN-ResNet(ResNet LMetric DNN)'}
{$REGION 'MMOD-DNN(DNN+SVM:max-margin object detector 6 layer)'}
    { MMOD-DNN(DNN+SVM:max-margin object detector 6 layer) training(gpu), usage XML swap dataset. }
    class function Init_MMOD6L_DNN_TrainParam(train_cfg, train_sync_file, train_output: U_String): PMMOD_Train_Parameter;
    class procedure Free_MMOD6L_DNN_TrainParam(param: PMMOD_Train_Parameter);
    function MMOD6L_DNN_PrepareTrain(imgList: TAI_ImageList; train_sync_file: U_String): PMMOD_Train_Parameter; overload;
    function MMOD6L_DNN_PrepareTrain(imgMat: TAI_ImageMatrix; train_sync_file: U_String): PMMOD_Train_Parameter; overload;
    function MMOD6L_DNN_Train(param: PMMOD_Train_Parameter): Integer;
    function MMOD6L_DNN_Train_Stream(param: PMMOD_Train_Parameter): TMemoryStream64;
    procedure MMOD6L_DNN_FreeTrain(param: PMMOD_Train_Parameter);
    { Large-Scale MMOD-DNN(DNN+SVM:max-margin object detector 6 layer) training(gpu), direct input without XML swap dataset. }
    function LargeScale_MMOD6L_DNN_PrepareTrain(train_sync_file, train_output: U_String): PMMOD_Train_Parameter;
    function LargeScale_MMOD6L_DNN_Train(param: PMMOD_Train_Parameter; imgList: TAI_ImageList): Integer; overload;
    function LargeScale_MMOD6L_DNN_Train(param: PMMOD_Train_Parameter; imgMat: TAI_ImageMatrix): Integer; overload;
    function LargeScale_MMOD6L_DNN_Train(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgList: TAI_ImageList): Integer; overload;
    function LargeScale_MMOD6L_DNN_Train(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix): Integer; overload;
    function LargeScale_MMOD6L_DNN_Train_Stream(param: PMMOD_Train_Parameter; imgList: TAI_ImageList): TMemoryStream64; overload;
    function LargeScale_MMOD6L_DNN_Train_Stream(param: PMMOD_Train_Parameter; imgMat: TAI_ImageMatrix): TMemoryStream64; overload;
    function LargeScale_MMOD6L_DNN_Train_Stream(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgList: TAI_ImageList): TMemoryStream64; overload;
    function LargeScale_MMOD6L_DNN_Train_Stream(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix): TMemoryStream64; overload;
    procedure LargeScale_MMOD6L_DNN_FreeTrain(param: PMMOD_Train_Parameter);
    { MMOD-DNN(DNN+SVM:max-margin object detector 6 layer) api(gpu) }
    function MMOD6L_DNN_Open_Face(): TMMOD6L_Handle;
    function MMOD6L_DNN_Open(train_file: SystemString): TMMOD6L_Handle;
    function MMOD6L_DNN_Open_Stream(stream: TMemoryStream64): TMMOD6L_Handle; overload;
    function MMOD6L_DNN_Open_Stream(train_file: SystemString): TMMOD6L_Handle; overload;
    function MMOD6L_DNN_Close(var hnd: TMMOD6L_Handle): Boolean;
    function MMOD6L_DNN_Process(hnd: TMMOD6L_Handle; Raster: TMemoryRaster): TMMOD_Desc;
    function MMOD6L_DNN_Process_Matrix(hnd: TMMOD6L_Handle; Matrix_IMG: TMatrix_Image_Handle): TMMOD_Desc;
    function MMOD6L_DNN_DebugInfo(hnd: TMMOD6L_Handle): U_String;
{$ENDREGION 'MMOD-DNN(DNN+SVM:max-margin object detector 6 layer)'}
{$REGION 'MMOD-DNN(DNN+SVM:max-margin object detector 3 layer)'}
    { MMOD-DNN(DNN+SVM:max-margin object detector 3 layer) training(gpu), usage XML swap dataset. }
    class function Init_MMOD3L_DNN_TrainParam(train_cfg, train_sync_file, train_output: U_String): PMMOD_Train_Parameter;
    class procedure Free_MMOD3L_DNN_TrainParam(param: PMMOD_Train_Parameter);
    function MMOD3L_DNN_PrepareTrain(imgList: TAI_ImageList; train_sync_file: U_String): PMMOD_Train_Parameter; overload;
    function MMOD3L_DNN_PrepareTrain(imgMat: TAI_ImageMatrix; train_sync_file: U_String): PMMOD_Train_Parameter; overload;
    function MMOD3L_DNN_Train(param: PMMOD_Train_Parameter): Integer;
    function MMOD3L_DNN_Train_Stream(param: PMMOD_Train_Parameter): TMemoryStream64;
    procedure MMOD3L_DNN_FreeTrain(param: PMMOD_Train_Parameter);
    { Large-Scale MMOD-DNN(DNN+SVM:max-margin object detector 3 layer) training(gpu), direct input without XML swap dataset. }
    function LargeScale_MMOD3L_DNN_PrepareTrain(train_sync_file, train_output: U_String): PMMOD_Train_Parameter;
    function LargeScale_MMOD3L_DNN_Train(param: PMMOD_Train_Parameter; imgList: TAI_ImageList): Integer; overload;
    function LargeScale_MMOD3L_DNN_Train(param: PMMOD_Train_Parameter; imgMat: TAI_ImageMatrix): Integer; overload;
    function LargeScale_MMOD3L_DNN_Train(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgList: TAI_ImageList): Integer; overload;
    function LargeScale_MMOD3L_DNN_Train(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix): Integer; overload;
    function LargeScale_MMOD3L_DNN_Train_Stream(param: PMMOD_Train_Parameter; imgList: TAI_ImageList): TMemoryStream64; overload;
    function LargeScale_MMOD3L_DNN_Train_Stream(param: PMMOD_Train_Parameter; imgMat: TAI_ImageMatrix): TMemoryStream64; overload;
    function LargeScale_MMOD3L_DNN_Train_Stream(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgList: TAI_ImageList): TMemoryStream64; overload;
    function LargeScale_MMOD3L_DNN_Train_Stream(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix): TMemoryStream64; overload;
    procedure LargeScale_MMOD3L_DNN_FreeTrain(param: PMMOD_Train_Parameter);
    { MMOD-DNN(DNN+SVM:max-margin object detector 3 layer) api(gpu) }
    function MMOD3L_DNN_Open(train_file: SystemString): TMMOD3L_Handle;
    function MMOD3L_DNN_Open_Stream(stream: TMemoryStream64): TMMOD3L_Handle; overload;
    function MMOD3L_DNN_Open_Stream(train_file: SystemString): TMMOD3L_Handle; overload;
    function MMOD3L_DNN_Close(var hnd: TMMOD3L_Handle): Boolean;
    function MMOD3L_DNN_Process(hnd: TMMOD3L_Handle; Raster: TMemoryRaster): TMMOD_Desc;
    function MMOD3L_DNN_Process_Matrix(hnd: TMMOD3L_Handle; Matrix_IMG: TMatrix_Image_Handle): TMMOD_Desc;
    function MMOD3L_DNN_DebugInfo(hnd: TMMOD3L_Handle): U_String;
{$ENDREGION 'MMOD-DNN(DNN+SVM:max-margin object detector 3 layer)'}
{$REGION 'ResNet-Image-Classifier'}
    { ResNet-Image-Classifier training(gpu), crop size 227, max classifier 1000, direct input without XML swap dataset. }
    class function Init_RNIC_Train_Parameter(train_sync_file, train_output: U_String): PRNIC_Train_Parameter;
    class procedure Free_RNIC_Train_Parameter(param: PRNIC_Train_Parameter);
    { data prototype }
    function RNIC_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TMemoryRaster2DArray; param: PRNIC_Train_Parameter; Train_OutputIndex: TMemoryRasterList): Boolean; overload;
    { ImageList data source }
    function RNIC_Train(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean; overload;
    function RNIC_Train(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; train_index_output: U_String): Boolean; overload;
    function RNIC_Train_Stream(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64; overload;
    { ImageMatrix data source }
    function RNIC_Train(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean; overload;
    function RNIC_Train(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; train_index_output: U_String): Boolean; overload;
    function RNIC_Train_Stream(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64; overload;
    { LargeScale-ResNet-Image-Classifier training(gpu), crop size 227, max classifier 1000, direct input without XML swap dataset. }
    function RNIC_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean; overload;
    function RNIC_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; train_index_output: U_String): Boolean; overload;
    function RNIC_Train_Stream(LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64; overload;
    { ResNet-Image-Classifier api(gpu), crop size 227, max classifier 1000 }
    function RNIC_Open(train_file: SystemString): TRNIC_Handle;
    function RNIC_Open_Stream(stream: TMemoryStream64): TRNIC_Handle; overload;
    function RNIC_Open_Stream(train_file: SystemString): TRNIC_Handle; overload;
    function RNIC_Close(var hnd: TRNIC_Handle): Boolean;
    function RNIC_Process(hnd: TRNIC_Handle; Raster: TMemoryRaster; num_crops: Integer): TLVec; overload;
    function RNIC_Process(hnd: TRNIC_Handle; Raster: TMemoryRaster): TLVec; overload;
    function RNIC_ProcessMatrix(hnd: TRNIC_Handle; mat_hnd: TMatrix_Image_Handle; num_crops: Integer): TLVec; overload;
    function RNIC_DebugInfo(hnd: TRNIC_Handle): U_String;
{$ENDREGION 'ResNet-Image-Classifier'}
{$REGION 'Large-ResNet-Image-Classifier'}
    { Large-ResNet-Image-Classifier training(gpu), crop size 227, max classifier 10000, direct input without XML swap dataset. }
    class function Init_LRNIC_Train_Parameter(train_sync_file, train_output: U_String): PRNIC_Train_Parameter;
    class procedure Free_LRNIC_Train_Parameter(param: PRNIC_Train_Parameter);
    { data prototype }
    function LRNIC_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TMemoryRaster2DArray; param: PRNIC_Train_Parameter; Train_OutputIndex: TMemoryRasterList): Boolean; overload;
    { ImageList data source }
    function LRNIC_Train(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean; overload;
    function LRNIC_Train(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; train_index_output: U_String): Boolean; overload;
    function LRNIC_Train_Stream(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64; overload;
    { ImageMatrix data source }
    function LRNIC_Train(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean; overload;
    function LRNIC_Train(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; train_index_output: U_String): Boolean; overload;
    function LRNIC_Train_Stream(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64; overload;
    { LargeScale-Large-ResNet-Image-Classifier training(gpu), crop size 227, max classifier 1000, direct input without XML swap dataset. }
    function LRNIC_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean; overload;
    function LRNIC_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; train_index_output: U_String): Boolean; overload;
    function LRNIC_Train_Stream(LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64; overload;
    { Large-ResNet-Image-Classifier api(gpu), crop size 227, max classifier 10000 }
    function LRNIC_Open(train_file: SystemString): TLRNIC_Handle;
    function LRNIC_Open_Stream(stream: TMemoryStream64): TLRNIC_Handle; overload;
    function LRNIC_Open_Stream(train_file: SystemString): TLRNIC_Handle; overload;
    function LRNIC_Close(var hnd: TLRNIC_Handle): Boolean;
    function LRNIC_Process(hnd: TLRNIC_Handle; Raster: TMemoryRaster; num_crops: Integer): TLVec; overload;
    function LRNIC_Process(hnd: TLRNIC_Handle; Raster: TMemoryRaster): TLVec; overload;
    function LRNIC_ProcessMatrix(hnd: TLRNIC_Handle; mat_hnd: TMatrix_Image_Handle; num_crops: Integer): TLVec; overload;
    function LRNIC_DebugInfo(hnd: TLRNIC_Handle): U_String;
{$ENDREGION 'Large-ResNet-Image-Classifier'}
{$REGION 'Going Deeper with Convolutions'}
    (*
      CVPR-2015 "Going Deeper with Convolutions"

      Christian Szegedy
      Wei Liu
      Yangqing Jia
      Pierre Sermanet

      Scott Reed
      Dragomir Anguelov
      Dumitru Erhan
      Vincent Vanhoucke
      Andrew Rabinovich

      Google Inc
      University of North Carolina
      Chapel Hill
      University of Michigan
      Ann Arbor 4Magic Leap Inc.

      paper author
      fszegedy@google.com
      jiayq@google.com
      sermanet@google.com
      dragomir@google.com
      dumitru@google.com
      vanhouckeg@google.com
      wliu@cs.unc.edu
      3reedscott@umich.edu
      4arabinovich@magicleap.com

      create by qq600585, test passed. 2019/4
    *)
    { Going Deeper with Convolutions net Image Classifier training(gpu), max classifier 10000, direct input without XML swap dataset. }
    class function Init_GDCNIC_Train_Parameter(train_sync_file, train_output: U_String): PGDCNIC_Train_Parameter;
    class procedure Free_GDCNIC_Train_Parameter(param: PGDCNIC_Train_Parameter);
    { data prototype }
    function GDCNIC_Train_(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TMemoryRaster2DArray; param: PGDCNIC_Train_Parameter; Train_OutputIndex: TMemoryRasterList): Boolean; overload;
    { ImageList data source }
    function GDCNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgList: TAI_ImageList; param: PGDCNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean; overload;
    function GDCNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgList: TAI_ImageList; param: PGDCNIC_Train_Parameter; train_index_output: U_String): Boolean; overload;
    function GDCNIC_Train_Stream(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgList: TAI_ImageList; param: PGDCNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64; overload;
    { ImageMatrix data source }
    function GDCNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGDCNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean; overload;
    function GDCNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGDCNIC_Train_Parameter; train_index_output: U_String): Boolean; overload;
    function GDCNIC_Train_Stream(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGDCNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64; overload;
    { LargeScale Going Deeper with Convolutions net Image Classifier training(gpu), max classifier 10000, direct input without XML swap dataset. }
    function GDCNIC_Train(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGDCNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean; overload;
    function GDCNIC_Train(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGDCNIC_Train_Parameter; train_index_output: U_String): Boolean; overload;
    function GDCNIC_Train_Stream(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGDCNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64; overload;
    { Going Deeper with Convolutions net Image Classifier api(gpu), max classifier 10000 }
    function GDCNIC_Open(train_file: SystemString): TGDCNIC_Handle;
    function GDCNIC_Open_Stream(stream: TMemoryStream64): TGDCNIC_Handle; overload;
    function GDCNIC_Open_Stream(train_file: SystemString): TGDCNIC_Handle; overload;
    function GDCNIC_Close(var hnd: TGDCNIC_Handle): Boolean;
    function GDCNIC_Process(hnd: TGDCNIC_Handle; SS_Width, SS_Height: Integer; Raster: TMemoryRaster): TLVec;
    function GDCNIC_DebugInfo(hnd: TGDCNIC_Handle): U_String;
{$ENDREGION 'Going Deeper with Convolutions'}
{$REGION 'Gradient-based learning applied to document recognition.'}
    (*
      LeCun, Yann, et al. "Gradient-based learning applied to document recognition."
      Proceedings of the IEEE 86.11 (1998): 2278-2324.

      im extracting CNN net struct part, not text recognition!!
      create by qq600585, test passed. 2019/4
    *)
    { Gradient-based net Image Classifier training(gpu), max classifier 10000, direct input without XML swap dataset. }
    class function Init_GNIC_Train_Parameter(train_sync_file, train_output: U_String): PGNIC_Train_Parameter;
    class procedure Free_GNIC_Train_Parameter(param: PGNIC_Train_Parameter);
    { data prototype }
    function GNIC_Train_(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TMemoryRaster2DArray; param: PGNIC_Train_Parameter; Train_OutputIndex: TMemoryRasterList): Boolean; overload;
    { ImageList data source }
    function GNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgList: TAI_ImageList; param: PGNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean; overload;
    function GNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgList: TAI_ImageList; param: PGNIC_Train_Parameter; train_index_output: U_String): Boolean; overload;
    function GNIC_Train_Stream(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgList: TAI_ImageList; param: PGNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64; overload;
    { ImageMatrix data source }
    function GNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean; overload;
    function GNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGNIC_Train_Parameter; train_index_output: U_String): Boolean; overload;
    function GNIC_Train_Stream(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64; overload;
    { LargeScale Gradient-based net Image Classifier training(gpu), max classifier 10000, direct input without XML swap dataset. }
    function GNIC_Train(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean; overload;
    function GNIC_Train(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGNIC_Train_Parameter; train_index_output: U_String): Boolean; overload;
    function GNIC_Train_Stream(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64; overload;
    { Gradient-based net Image Classifier api(gpu), max classifier 10000 }
    function GNIC_Open(train_file: SystemString): TGNIC_Handle;
    function GNIC_Open_Stream(stream: TMemoryStream64): TGNIC_Handle; overload;
    function GNIC_Open_Stream(train_file: SystemString): TGNIC_Handle; overload;
    function GNIC_Close(var hnd: TGNIC_Handle): Boolean;
    function GNIC_Process(hnd: TGNIC_Handle; SS_Width, SS_Height: Integer; Raster: TMemoryRaster): TLVec;
    function GNIC_DebugInfo(hnd: TGNIC_Handle): U_String;
{$ENDREGION 'Gradient-based learning applied to document recognition.'}
{$REGION 'Convolutional Networks for Biomedical Image Segmentation'}
    (*
      U-Net: Convolutional Networks for Biomedical Image Segmentation
      Olaf Ronneberger, Philipp Fischer, and Thomas Brox
      Computer Science Department and BIOSS Centre for Biological Signalling Studies,
      University of Freiburg, Germany
      ronneber@informatik.uni-freiburg.de
      WWW home page: http://lmb.informatik.uni-freiburg.de/

      hint: Max Classifier num is 50
    *)
    { segmantic segmentation }
    class function Init_SS_Train_Parameter(train_sync_file, train_output: U_String): PSS_Train_Parameter;
    class procedure Free_SS_Train_Parameter(param: PSS_Train_Parameter);
    function SS_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; param: PSS_Train_Parameter; const colorPool: TSegmentationColorTable): Boolean; overload;
    function SS_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PSS_Train_Parameter; const colorPool: TSegmentationColorTable): Boolean; overload;
    function SS_Train_Stream(imgList: TAI_ImageList; param: PSS_Train_Parameter; const colorPool: TSegmentationColorTable): TMemoryStream64; overload;
    function SS_Train_Stream(imgMat: TAI_ImageMatrix; param: PSS_Train_Parameter; const colorPool: TSegmentationColorTable): TMemoryStream64; overload;
    function SS_Open(train_file: SystemString): TSS_Handle;
    function SS_Open_Stream(stream: TMemoryStream64): TSS_Handle; overload;
    function SS_Open_Stream(train_file: SystemString): TSS_Handle; overload;
    function SS_Close(var hnd: TSS_Handle): Boolean;
    class function SS_TranslateColor(const c: WORD): TRColorEntry;
    function SS_Process(parallel_: Boolean; hnd: TSS_Handle; InputRaster: TMemoryRaster; colorPool: TSegmentationColorTable; SSTokenOutput: TPascalStringList): TMemoryRaster; overload;
    function SS_Process(hnd: TSS_Handle; InputRaster: TMemoryRaster; colorPool: TSegmentationColorTable; SSTokenOutput: TPascalStringList): TMemoryRaster; overload;
    function SS_ProcessMatrix(hnd: TSS_Handle; mat_hnd: TMatrix_Image_Handle; Width, Height: Integer; colorPool: TSegmentationColorTable; SSTokenOutput: TPascalStringList): TMemoryRaster; overload;
    procedure SS_ProcessAsync(hnd: TSS_Handle; SSInput: TMemoryRaster; colorPool: TSegmentationColorTable;
      OnResultC: TSS_ProcessOnResultCall; OnResultM: TSS_ProcessOnResultMethod; OnResultP: TSS_ProcessOnResultProc); overload;
    function SS_DebugInfo(hnd: TSS_Handle): U_String;
{$ENDREGION 'Convolutional Networks for Biomedical Image Segmentation'}
{$REGION 'correlation tracker'}
    { video tracker(cpu) from Matrix, tracker from TRectV2 }
    function Tracker_Open_Matrix(mat_hnd: TMatrix_Image_Handle; const track_rect: TRectV2): TTracker_Handle;
    function Tracker_Update_Matrix(hnd: TTracker_Handle; mat_hnd: TMatrix_Image_Handle; var track_rect: TRectV2): Double;
    function Tracker_Update_NoScale_Matrix(hnd: TTracker_Handle; mat_hnd: TMatrix_Image_Handle; var track_rect: TRectV2): Double;
    { video tracker(cpu) from RGBHnd, tracker from TRectV2 }
    function Tracker_Open_RGB(RGB_Hnd: TRGB_Image_Handle; const track_rect: TRectV2): TTracker_Handle;
    function Tracker_Update_RGB(hnd: TTracker_Handle; RGB_Hnd: TRGB_Image_Handle; var track_rect: TRectV2): Double;
    function Tracker_Update_NoScale_RGB(hnd: TTracker_Handle; RGB_Hnd: TRGB_Image_Handle; var track_rect: TRectV2): Double;
    { video tracker(cpu) from Matrix, multi tracker from TRectV2/TMMOD_Desc/TOD_Desc }
    function Tracker_Open_Matrix_Multi(parallel_: Boolean; mat_hnd: TMatrix_Image_Handle; const track_rect: TArrayRectV2): TTracker_Handle_Array; overload;
    function Tracker_Open_Matrix_Multi(parallel_: Boolean; mat_hnd: TMatrix_Image_Handle; const MMOD_Desc: TMMOD_Desc): TTracker_Handle_Array; overload;
    function Tracker_Open_Matrix_Multi(parallel_: Boolean; mat_hnd: TMatrix_Image_Handle; const OD_Desc: TOD_Desc): TTracker_Handle_Array; overload;
    function Tracker_Update_Matrix_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; mat_hnd: TMatrix_Image_Handle; var track_rect: TArrayRectV2): TLVec; overload;
    procedure Tracker_Update_Matrix_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; mat_hnd: TMatrix_Image_Handle; var MMOD_Desc: TMMOD_Desc); overload;
    procedure Tracker_Update_Matrix_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; mat_hnd: TMatrix_Image_Handle; var OD_Desc: TOD_Desc); overload;
    { video tracker(cpu) from RGB, multi tracker from TRectV2/TMMOD_Desc/TOD_Desc }
    function Tracker_Open_RGB_Multi(parallel_: Boolean; RGB_Hnd: TRGB_Image_Handle; const track_rect: TArrayRectV2): TTracker_Handle_Array; overload;
    function Tracker_Open_RGB_Multi(parallel_: Boolean; RGB_Hnd: TRGB_Image_Handle; const MMOD_Desc: TMMOD_Desc): TTracker_Handle_Array; overload;
    function Tracker_Open_RGB_Multi(parallel_: Boolean; RGB_Hnd: TRGB_Image_Handle; const OD_Desc: TOD_Desc): TTracker_Handle_Array; overload;
    function Tracker_Update_RGB_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; RGB_Hnd: TRGB_Image_Handle; var track_rect: TArrayRectV2): TLVec; overload;
    procedure Tracker_Update_RGB_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; RGB_Hnd: TRGB_Image_Handle; var MMOD_Desc: TMMOD_Desc); overload;
    procedure Tracker_Update_RGB_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; RGB_Hnd: TRGB_Image_Handle; var OD_Desc: TOD_Desc); overload;
    { video tracker(cpu) from raster, multi tracker from TRectV2/TMMOD_Desc/TOD_Desc }
    function Tracker_Open_Multi(parallel_: Boolean; Raster: TMemoryRaster; const track_rect: TArrayRectV2): TTracker_Handle_Array; overload;
    function Tracker_Open_Multi(parallel_: Boolean; Raster: TMemoryRaster; const MMOD_Desc: TMMOD_Desc): TTracker_Handle_Array; overload;
    function Tracker_Open_Multi(parallel_: Boolean; Raster: TMemoryRaster; const MMOD_DescArray: TMMOD_Desc_Array): TTracker_Handle_ArrayOfArray; overload;
    function Tracker_Open_Multi(parallel_: Boolean; Raster: TMemoryRaster; const OD_Desc: TOD_Desc): TTracker_Handle_Array; overload;
    function Tracker_Update_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; Raster: TMemoryRaster; var track_rect: TArrayRectV2): TLVec; overload;
    procedure Tracker_Update_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; Raster: TMemoryRaster; var MMOD_Desc: TMMOD_Desc); overload;
    procedure Tracker_Update_Multi(parallel_: Boolean; hnd: TTracker_Handle_ArrayOfArray; Raster: TMemoryRaster; var MMOD_DescArray: TMMOD_Desc_Array); overload;
    procedure Tracker_Update_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; Raster: TMemoryRaster; var OD_Desc: TOD_Desc); overload;
    { video tracker(cpu),single raster tracker from TRect }
    function Tracker_Open(Raster: TMemoryRaster; const track_rect: TRect): TTracker_Handle; overload;
    function Tracker_Update(hnd: TTracker_Handle; Raster: TMemoryRaster; var track_rect: TRect): Double; overload;
    function Tracker_Update_NoScale(hnd: TTracker_Handle; Raster: TMemoryRaster; var track_rect: TRect): Double; overload;
    { video tracker(cpu),single raster tracker from TRectV2 }
    function Tracker_Open(Raster: TMemoryRaster; const track_rect: TRectV2): TTracker_Handle; overload;
    function Tracker_Update(hnd: TTracker_Handle; Raster: TMemoryRaster; var track_rect: TRectV2): Double; overload;
    function Tracker_Update_NoScale(hnd: TTracker_Handle; Raster: TMemoryRaster; var track_rect: TRectV2): Double; overload;
    { close tracker handle }
    function Tracker_Close(var hnd: TTracker_Handle): Boolean; overload;
    function Tracker_Close(var hnd: TTracker_Handle_Array): Boolean; overload;
    function Tracker_Close(var hnd: TTracker_Handle_ArrayOfArray): Boolean; overload;
{$ENDREGION 'correlation tracker'}
{$REGION 'OCR'}
    (*
      OCR Model manager
    *)
    class procedure RebuildOCREngineMD5Label();
    class function PrepareOCRLanguageModelToCustomPath(DBLangPath_, DBLangFile_, DestPath_: SystemString; ExtractConfigure_: Boolean): Boolean; overload;
    class function PrepareOCRLanguageModelToCustomPath(DBLangPath_, DestPath_: SystemString): Boolean; overload;
    class function PrepareOCRLanguageModel(DBLangPath_, DBLangFile_: SystemString): Boolean; overload;
    class function PrepareOCRLanguageModel(DBLangPath_: SystemString): Boolean; overload;
    class procedure CleanOCRLanguageModel();
    class function PrepareOCRFastLanguageModel(): Boolean;
    class function PrepareOCRBestLanguageModel(): Boolean;
    class function PrepareOCRDefaultLanguageModel(): Boolean;
    class function PrepareOCRLanguageModel(): Boolean; overload;

    (*
      Init OCR Engine
    *)
    function OpenOCREngine(ocrData, ocrLang: SystemString): TOCR_Handle; overload;
    function OpenOCREngine(ocrLang: SystemString): TOCR_Handle; overload;

    (*
      daily ussage parameters
    *)
    procedure SetOCRDPI(hnd: TOCR_Handle; v_: Integer);
    procedure SetOCRWhiteChar(hnd: TOCR_Handle; v_: TPascalString);

    (*
      close OCR Engine and free memory.
    *)
    procedure CloseOCREngine(var hnd: TOCR_Handle);

    (*
      set and print ocr parameter.
    *)
    procedure SetOCRParameter(hnd: TOCR_Handle; ocrKey, ocrValue: U_String);
    procedure PrintOCRParameter(hnd: TOCR_Handle);

    (*
      mode defined
      0 Orientation and script detection only.
      1 Automatic page segmentation with orientation and OSD.
      2 Automatic page segmentation, but no OSD, or OCR.
      3 Fully automatic page segmentation, but no OSD.
      4 Assume a single column of text of variable sizes.
      5 Assume a single uniform block of vertically.
      6 Assume a single uniform block of text. (Default.)
      7 Treat the image as a single text line.
      8 Treat the image as a single word.
      9 Treat the image as a single word in a circle.
      10 Treat the image as a single character.
      11 Find as much text as possible in no particular order.
      12 Sparse text with orientation and script det.
      13 Treat the image as a single text line.
    *)
    function ProcessOCR(hnd: TOCR_Handle; Raster: TMemoryRaster; mode: Integer): Boolean; overload;
    (*
      default mode = 3 Fully automatic page segmentation, but no OSD.
    *)
    function ProcessOCR(hnd: TOCR_Handle; Raster: TMemoryRaster): Boolean; overload;

    (*
      result processor
    *)
    function GetOCR_Text(hnd: TOCR_Handle): U_String;
    function GetOCR_HTML(hnd: TOCR_Handle): U_String;
    function GetOCR_XML(hnd: TOCR_Handle): U_String;
    function GetOCR_TSV(hnd: TOCR_Handle): U_String;
    function GetOCR_LSTMBox(hnd: TOCR_Handle): U_String;
    function GetOCR_Box(hnd: TOCR_Handle): U_String;
    function GetOCR_WordStrBox(hnd: TOCR_Handle): U_String;
    function GetOCR_OSD(hnd: TOCR_Handle): U_String;
{$ENDREGION 'OCR'}
{$REGION 'ZMetric'}
    { Z-Metric training(gpu), extract dim 24, input size is user custom, full resnet jitter, include bias, direct input without XML swap dataset. }
    class function Init_ZMetric_Parameter(train_sync_file, train_output: U_String): PZMetric_Train_Parameter;
    class procedure Free_ZMetric_Parameter(param: PZMetric_Train_Parameter);
    { Z-Metric data prototype }
    function ZMetric_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TMemoryRaster2DArray; param: PZMetric_Train_Parameter): Boolean; overload;
    function ZMetric_Train(Snapshot_: Boolean; imgList: TAI_ImageList; SS_Width, SS_Height: Integer; param: PZMetric_Train_Parameter): Boolean; overload;
    function ZMetric_Train_Stream(Snapshot_: Boolean; imgList: TAI_ImageList; SS_Width, SS_Height: Integer; param: PZMetric_Train_Parameter): TMemoryStream64; overload;
    function ZMetric_Train(Snapshot_: Boolean; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; param: PZMetric_Train_Parameter): Boolean; overload;
    function ZMetric_Train_Stream(Snapshot_: Boolean; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; param: PZMetric_Train_Parameter): TMemoryStream64; overload;
    { Z-Metric training(gpu), extract dim 24, input size is user custom, full resnet jitter, include bias, direct input without XML swap dataset. }
    function ZMetric_Train(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; param: PZMetric_Train_Parameter): Boolean; overload;
    function ZMetric_Train_Stream(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; param: PZMetric_Train_Parameter): TMemoryStream64; overload;
    { Z-Metric api(gpu), extract dim 24, input size is user custom, full resnet jitter, include bias }
    class function Build_ZMetric_Learn(): TLearn;
    function ZMetric_Open(train_file: SystemString): TZMetric_Handle;
    function ZMetric_Open_Stream(stream: TMemoryStream64): TZMetric_Handle; overload;
    function ZMetric_Open_Stream(train_file: SystemString): TZMetric_Handle; overload;
    function ZMetric_Close(var hnd: TZMetric_Handle): Boolean;
    function ZMetric_Process(hnd: TZMetric_Handle; RasterArray: TMemoryRasterArray; SS_Width, SS_Height: Integer; output: PDouble): Integer; overload;
    function ZMetric_Process(hnd: TZMetric_Handle; RasterArray: TMemoryRasterArray; SS_Width, SS_Height: Integer): TLMatrix; overload;
    function ZMetric_Process(hnd: TZMetric_Handle; Raster: TMemoryRaster; SS_Width, SS_Height: Integer): TLVec; overload;
    { Z-Metric DNN Thread tech }
    procedure ZMetric_SaveToLearnEngine_DT(pool_: TAI_DNN_ThreadPool; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; SS_Width, SS_Height: Integer; lr: TLearn); overload;
    procedure ZMetric_SaveToLearnEngine_DT(ThNum: Integer; ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; SS_Width, SS_Height: Integer; lr: TLearn); overload;
    procedure ZMetric_SaveToLearnEngine_DT(ThNum: Integer; ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; lr: TLearn); overload;
    procedure ZMetric_SaveToLearnEngine_DT(ThNum: Integer; ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; imgList: TAI_ImageList; SS_Width, SS_Height: Integer; lr: TLearn); overload;
    procedure ZMetric_SaveToLearnEngine_DT(ThNum: Integer; ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; lr: TLearn); overload;
    procedure ZMetric_SaveToLearnEngine_DT(ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; SS_Width, SS_Height: Integer; lr: TLearn); overload;
    procedure ZMetric_SaveToLearnEngine_DT(ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; lr: TLearn); overload;
    procedure ZMetric_SaveToLearnEngine_DT(ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; imgList: TAI_ImageList; SS_Width, SS_Height: Integer; lr: TLearn); overload;
    procedure ZMetric_SaveToLearnEngine_DT(ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; lr: TLearn); overload;
{$ENDREGION 'ZMetric'}
  end;

  TSS_ResultProcessor = class
  private
    SSMatrix: array of WORD;
    colorPool: TSegmentationColorTable;

    SSInput: TMemoryRaster;
    SSOutput: TMemoryRaster;
    SSTokenOutput: TPascalStringList;

    OnResultC: TSS_ProcessOnResultCall;
    OnResultM: TSS_ProcessOnResultMethod;
    OnResultP: TSS_ProcessOnResultProc;

    procedure DoFailed;
    procedure DoSuccessed;
    procedure ThRun(ThSender: TCompute);
  public
    constructor Create;
  end;
{$ENDREGION 'AI Core'}
{$REGION 'TAI_DNN_Thread'}

  TAI_DNN_Thread_Trigger = record
    p: Pointer;
    Ev: TRunWithThreadMethod;
    class function Init(p_: Pointer; Ev_: TRunWithThreadMethod): TAI_DNN_Thread_Trigger; static;
  end;

  // event trigger queue
  TAI_DNN_Thread_Event_Trigger_Order = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalOrderPtrStruct<TAI_DNN_Thread_Trigger>;

  // AI parallel for GPU/MKL Platform
  TAI_DNN_Thread = class(TCoreClassObject)
  protected
    FID: Integer;
    FAI: TAI;
    FThread: TCompute;
    FDevice: Integer;
    FThreadPost: TThreadPost;
    FActivted: TAtomBool;
    FDNNThreadRuning: TAtomBool;
    FThreadInfo: SystemString;
    FPSP: TGeoFloat;
    FMaxPSP: TGeoFloat;
    FCPUThreadCritical: Integer;
    FGPUPerformanceCritical: Integer;
    FName: SystemString;
    // event thread
    FEventThreadNum: Integer;
    FEventQueue: TAI_DNN_Thread_Event_Trigger_Order;
    // process raster state
    FEnabledLastProcessRaster: Boolean;
    FLastProcessRasterCritical: TCritical;
    FLastProcessRaster: TRaster;
    FCustomObject: TCoreClassObject;
    FCustomData: Pointer;
    procedure Run_DNN_Thread(Sender: TCompute);
    procedure ThreadFree; virtual;
    function GetTaskNum: Integer;
    procedure UpdateLastProcessRaster(raster_: TRaster);
    procedure UpdateLastProcessMatrixRaster(Matrix_IMG: TMatrix_Image_Handle);
    // event thread
    procedure DoEventDone(ThSender: TCompute);
    procedure DoRunEvent(p: Pointer; Ev: TRunWithThreadMethod);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function Build(Owner: TAI_DNN_ThreadPool; AI_LIB_P: PAI_EntryAPI; Device_: Integer; class_: TAI_DNN_Thread_Class): TAI_DNN_Thread; overload;
    class function Build(Owner: TAI_DNN_ThreadPool; Device_: Integer; class_: TAI_DNN_Thread_Class): TAI_DNN_Thread; overload;
    procedure CheckGPUPerformanceCritical; overload;
    function CheckGPUPerformanceCritical(m: TTimeTick): Boolean; overload;
    function GetCPUAsyncThreadNum: Integer;
    property TaskNum: Integer read GetTaskNum;
    function Busy: Boolean;
    property Device: Integer read FDevice;
    property ID: Integer read FID;
    property AI: TAI read FAI;
    property ThreadInfo: SystemString read FThreadInfo;
    property PSP: TGeoFloat read FPSP;
    property MaxPSP: TGeoFloat read FMaxPSP;
    property CPUThreadCritical: Integer read FCPUThreadCritical write FCPUThreadCritical;
    property GPUPerformanceCritical: Integer read FGPUPerformanceCritical write FGPUPerformanceCritical;
    property Name: SystemString read FName write FName;
    function GetAndLockLastProcessRaster: TRaster;
    procedure UnLockLastProcessRaster;
    property CustomObject: TCoreClassObject read FCustomObject write FCustomObject;
    property CustomData: Pointer read FCustomData write FCustomData;
  end;

  TAI_DNN_ThreadPool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TAI_DNN_Thread>;

  TAI_DNN_ThreadPool = class(TAI_DNN_ThreadPool_Decl)
  private
    FName: U_String;
    FCritical: TCritical;
    FNext_DNNThreadID: Integer;
    FQueueOptimized: Boolean;
    FLastRasterList: TMemoryRasterList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Remove(Obj: TAI_DNN_Thread);
    procedure Delete(index: Integer);
    procedure Clear;

    // dnn thread for device
    procedure BuildDeviceThread(AI_LIB_P: PAI_EntryAPI; Device_, ThNum_: Integer; class_: TAI_DNN_Thread_Class); overload;
    procedure BuildDeviceThread(Device_, ThNum_: Integer; class_: TAI_DNN_Thread_Class); overload;
    // custom device
    procedure BuildPerDeviceThread(AI_LIB_P: PAI_EntryAPI; Device_: TLIVec; ThNum_: Integer; class_: TAI_DNN_Thread_Class); overload;
    procedure BuildPerDeviceThread(Device_: TLIVec; ThNum_: Integer; class_: TAI_DNN_Thread_Class); overload;
    procedure BuildPerDeviceThread(Device_: TLIVec; class_: TAI_DNN_Thread_Class); overload;
    // per device
    procedure BuildPerDeviceThread(AI_LIB_P: PAI_EntryAPI; ThNum_: Integer; class_: TAI_DNN_Thread_Class); overload;
    procedure BuildPerDeviceThread(ThNum_: Integer; class_: TAI_DNN_Thread_Class); overload;
    procedure BuildPerDeviceThread(class_: TAI_DNN_Thread_Class); overload;
    // performance and state for DNN thread
    function Next_DNN_Thread: TAI_DNN_Thread;
    function MinLoad_DNN_Thread: TAI_DNN_Thread;
    function IDLE_DNN_Thread: TAI_DNN_Thread;
    function GetMinLoad_DNN_Thread_TaskNum: Integer;
    function GetTaskNum: Integer;
    property TaskNum: Integer read GetTaskNum;
    function Busy: Boolean;
    function PSP: TGeoFloat;
    function MaxPSP: TGeoFloat;
    procedure Wait;
    function StateInfo: U_String; overload;
    function StateInfo(const Separator: Boolean): U_String; overload;
    procedure EnabledLastProcessRaster(value_: Boolean);
    function LockLastRasterList: TMemoryRasterList;
    procedure UnLockLastRasterList;
    property QueueOptimized: Boolean read FQueueOptimized write FQueueOptimized;
    property Name: U_String read FName write FName;
  end;
{$ENDREGION 'TAI_DNN_Thread'}
{$REGION 'TAI_DNN_Thread_Metric'}

  TAI_DNN_Thread_Metric_AsyncProcess_C = procedure(ThSender: TAI_DNN_Thread_Metric; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
  TAI_DNN_Thread_Metric_AsyncProcess_M = procedure(ThSender: TAI_DNN_Thread_Metric; UserData: Pointer; Input: TMemoryRaster; output: TLVec) of object;
{$IFDEF FPC}
  TAI_DNN_Thread_Metric_AsyncProcess_P = procedure(ThSender: TAI_DNN_Thread_Metric; UserData: Pointer; Input: TMemoryRaster; output: TLVec) is nested;
{$ELSE FPC}
  TAI_DNN_Thread_Metric_AsyncProcess_P = reference to procedure(ThSender: TAI_DNN_Thread_Metric; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
{$ENDIF FPC}

  TAI_DNN_Thread_Metric = class(TAI_DNN_Thread)
  private type
    TCMD_SyncProcess = record
      Done: TAtomBool;
      Input: TMemoryRaster;
      output: TLVec;
    end;

    TCMD_AsyncProcess = record
      UserData: Pointer;
      Input: TMemoryRaster;
      FreeInput: Boolean;
      OnResult_C: TAI_DNN_Thread_Metric_AsyncProcess_C;
      OnResult_M: TAI_DNN_Thread_Metric_AsyncProcess_M;
      OnResult_P: TAI_DNN_Thread_Metric_AsyncProcess_P;
      output: TLVec;
    end;

    PCMD_SyncProcess = ^TCMD_SyncProcess;
    PCMD_AsyncProcess = ^TCMD_AsyncProcess;
  private
    MetricHnd: TMetric_Handle;
    procedure ThreadFree; override;
    procedure CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_OpenShareFace();
    procedure CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_SyncProcess(data: Pointer);
    procedure OnComputeThreadResult(ThSender: TCompute);
    procedure CMD_AsyncProcess(data: Pointer);
  public
    constructor Create; override;
    procedure Open(train_file: SystemString);
    procedure Open_ShareFace();
    procedure Open_Stream(stream: TMemoryStream64);
    function Process(Input: TMemoryRaster): TLVec;
    procedure ProcessC(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_Metric_AsyncProcess_C);
    procedure ProcessM(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_Metric_AsyncProcess_M);
    procedure ProcessP(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_Metric_AsyncProcess_P);
  end;
{$ENDREGION 'TAI_DNN_Thread_Metric'}
{$REGION 'TAI_DNN_Thread_LMetric'}

  TAI_DNN_Thread_LMetric_AsyncProcess_C = procedure(ThSender: TAI_DNN_Thread_LMetric; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
  TAI_DNN_Thread_LMetric_AsyncProcess_M = procedure(ThSender: TAI_DNN_Thread_LMetric; UserData: Pointer; Input: TMemoryRaster; output: TLVec) of object;
{$IFDEF FPC}
  TAI_DNN_Thread_LMetric_AsyncProcess_P = procedure(ThSender: TAI_DNN_Thread_LMetric; UserData: Pointer; Input: TMemoryRaster; output: TLVec) is nested;
{$ELSE FPC}
  TAI_DNN_Thread_LMetric_AsyncProcess_P = reference to procedure(ThSender: TAI_DNN_Thread_LMetric; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
{$ENDIF FPC}

  TAI_DNN_Thread_LMetric = class(TAI_DNN_Thread)
  private type
    TCMD_SyncProcess = record
      Done: TAtomBool;
      Input: TMemoryRaster;
      output: TLVec;
    end;

    TCMD_AsyncProcess = record
      UserData: Pointer;
      Input: TMemoryRaster;
      FreeInput: Boolean;
      OnResult_C: TAI_DNN_Thread_LMetric_AsyncProcess_C;
      OnResult_M: TAI_DNN_Thread_LMetric_AsyncProcess_M;
      OnResult_P: TAI_DNN_Thread_LMetric_AsyncProcess_P;
      output: TLVec;
    end;

    PCMD_SyncProcess = ^TCMD_SyncProcess;
    PCMD_AsyncProcess = ^TCMD_AsyncProcess;
  private
    LMetricHnd: TLMetric_Handle;
    procedure ThreadFree; override;
    procedure CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_SyncProcess(data: Pointer);
    procedure OnComputeThreadResult(ThSender: TCompute);
    procedure CMD_AsyncProcess(data: Pointer);
  public
    constructor Create; override;
    procedure Open(train_file: SystemString);
    procedure Open_Stream(stream: TMemoryStream64);
    function Process(Input: TMemoryRaster): TLVec;
    procedure ProcessC(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_LMetric_AsyncProcess_C);
    procedure ProcessM(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_LMetric_AsyncProcess_M);
    procedure ProcessP(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_LMetric_AsyncProcess_P);
  end;
{$ENDREGION 'TAI_DNN_Thread_LMetric'}
{$REGION 'TAI_DNN_Thread_MMOD6L'}

  TAI_DNN_Thread_MMOD6L_AsyncProcess_C = procedure(ThSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Input: TMemoryRaster; output: TMMOD_Desc);
  TAI_DNN_Thread_MMOD6L_AsyncProcess_M = procedure(ThSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Input: TMemoryRaster; output: TMMOD_Desc) of object;
  TAI_DNN_Thread_MMOD6L_AsyncProcessMatrix_C = procedure(ThSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; output: TMMOD_Desc);
  TAI_DNN_Thread_MMOD6L_AsyncProcessMatrix_M = procedure(ThSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; output: TMMOD_Desc) of object;
{$IFDEF FPC}
  TAI_DNN_Thread_MMOD6L_AsyncProcess_P = procedure(ThSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Input: TMemoryRaster; output: TMMOD_Desc) is nested;
  TAI_DNN_Thread_MMOD6L_AsyncProcessMatrix_P = procedure(ThSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; output: TMMOD_Desc) is nested;
{$ELSE FPC}
  TAI_DNN_Thread_MMOD6L_AsyncProcess_P = reference to procedure(ThSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Input: TMemoryRaster; output: TMMOD_Desc);
  TAI_DNN_Thread_MMOD6L_AsyncProcessMatrix_P = reference to procedure(ThSender: TAI_DNN_Thread_MMOD6L; UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; output: TMMOD_Desc);
{$ENDIF FPC}

  TAI_DNN_Thread_MMOD6L = class(TAI_DNN_Thread)
  private type
    TCMD_SyncProcess = record
      Done: TAtomBool;
      Input: TMemoryRaster;
      output: TMMOD_Desc;
    end;

    TCMD_AsyncProcess = record
      UserData: Pointer;
      Input: TMemoryRaster;
      FreeInput: Boolean;
      OnResult_C: TAI_DNN_Thread_MMOD6L_AsyncProcess_C;
      OnResult_M: TAI_DNN_Thread_MMOD6L_AsyncProcess_M;
      OnResult_P: TAI_DNN_Thread_MMOD6L_AsyncProcess_P;
      output: TMMOD_Desc;
    end;

    TCMD_AsyncProcessMatrix = record
      UserData: Pointer;
      Matrix_IMG: TMatrix_Image_Handle;
      FreeInput: Boolean;
      OnResult_C: TAI_DNN_Thread_MMOD6L_AsyncProcessMatrix_C;
      OnResult_M: TAI_DNN_Thread_MMOD6L_AsyncProcessMatrix_M;
      OnResult_P: TAI_DNN_Thread_MMOD6L_AsyncProcessMatrix_P;
      output: TMMOD_Desc;
    end;

    PCMD_SyncProcess = ^TCMD_SyncProcess;
    PCMD_AsyncProcess = ^TCMD_AsyncProcess;
    PCMD_AsyncProcessMatrix = ^TCMD_AsyncProcessMatrix;
  private
    MMOD6LHnd: TMMOD6L_Handle;
    procedure ThreadFree; override;
    procedure CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_OpenFace();
    procedure CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_SyncProcess(data: Pointer);
    procedure CMD_AsyncProcess_Result(ThSender: TCompute);
    procedure CMD_AsyncProcess(data: Pointer);
    procedure CMD_AsyncProcessMatrix_Result(ThSender: TCompute);
    procedure CMD_AsyncProcessMatrix(data: Pointer);
  public
    constructor Create; override;
    procedure Open(train_file: SystemString);
    procedure Open_Face();
    procedure Open_Stream(stream: TMemoryStream64);
    function Process(Input: TMemoryRaster): TMMOD_Desc;
    procedure ProcessC(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD6L_AsyncProcess_C);
    procedure ProcessM(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD6L_AsyncProcess_M);
    procedure ProcessP(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD6L_AsyncProcess_P);
    procedure ProcessMatrixC(UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD6L_AsyncProcessMatrix_C);
    procedure ProcessMatrixM(UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD6L_AsyncProcessMatrix_M);
    procedure ProcessMatrixP(UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD6L_AsyncProcessMatrix_P);
  end;
{$ENDREGION 'TAI_DNN_Thread_MMOD6L'}
{$REGION 'TAI_DNN_Thread_MMOD3L'}

  TAI_DNN_Thread_MMOD3L_AsyncProcess_C = procedure(ThSender: TAI_DNN_Thread_MMOD3L; UserData: Pointer; Input: TMemoryRaster; output: TMMOD_Desc);
  TAI_DNN_Thread_MMOD3L_AsyncProcess_M = procedure(ThSender: TAI_DNN_Thread_MMOD3L; UserData: Pointer; Input: TMemoryRaster; output: TMMOD_Desc) of object;
  TAI_DNN_Thread_MMOD3L_AsyncProcessMatrix_C = procedure(ThSender: TAI_DNN_Thread_MMOD3L; UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; output: TMMOD_Desc);
  TAI_DNN_Thread_MMOD3L_AsyncProcessMatrix_M = procedure(ThSender: TAI_DNN_Thread_MMOD3L; UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; output: TMMOD_Desc) of object;
{$IFDEF FPC}
  TAI_DNN_Thread_MMOD3L_AsyncProcess_P = procedure(ThSender: TAI_DNN_Thread_MMOD3L; UserData: Pointer; Input: TMemoryRaster; output: TMMOD_Desc) is nested;
  TAI_DNN_Thread_MMOD3L_AsyncProcessMatrix_P = procedure(ThSender: TAI_DNN_Thread_MMOD3L; UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; output: TMMOD_Desc) is nested;
{$ELSE FPC}
  TAI_DNN_Thread_MMOD3L_AsyncProcess_P = reference to procedure(ThSender: TAI_DNN_Thread_MMOD3L; UserData: Pointer; Input: TMemoryRaster; output: TMMOD_Desc);
  TAI_DNN_Thread_MMOD3L_AsyncProcessMatrix_P = reference to procedure(ThSender: TAI_DNN_Thread_MMOD3L; UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; output: TMMOD_Desc);
{$ENDIF FPC}

  TAI_DNN_Thread_MMOD3L = class(TAI_DNN_Thread)
  private type
    TCMD_SyncProcess = record
      Done: TAtomBool;
      Input: TMemoryRaster;
      output: TMMOD_Desc;
    end;

    TCMD_AsyncProcess = record
      UserData: Pointer;
      Input: TMemoryRaster;
      FreeInput: Boolean;
      OnResult_C: TAI_DNN_Thread_MMOD3L_AsyncProcess_C;
      OnResult_M: TAI_DNN_Thread_MMOD3L_AsyncProcess_M;
      OnResult_P: TAI_DNN_Thread_MMOD3L_AsyncProcess_P;
      output: TMMOD_Desc;
    end;

    TCMD_AsyncProcessMatrix = record
      UserData: Pointer;
      Matrix_IMG: TMatrix_Image_Handle;
      FreeInput: Boolean;
      OnResult_C: TAI_DNN_Thread_MMOD3L_AsyncProcessMatrix_C;
      OnResult_M: TAI_DNN_Thread_MMOD3L_AsyncProcessMatrix_M;
      OnResult_P: TAI_DNN_Thread_MMOD3L_AsyncProcessMatrix_P;
      output: TMMOD_Desc;
    end;

    PCMD_SyncProcess = ^TCMD_SyncProcess;
    PCMD_AsyncProcess = ^TCMD_AsyncProcess;
    PCMD_AsyncProcessMatrix = ^TCMD_AsyncProcessMatrix;
  private
    MMOD3LHnd: TMMOD3L_Handle;
    procedure ThreadFree; override;
    procedure CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_SyncProcess(data: Pointer);
    procedure CMD_AsyncProcess_Result(ThSender: TCompute);
    procedure CMD_AsyncProcess(data: Pointer);
    procedure CMD_AsyncProcessMatrix_Result(ThSender: TCompute);
    procedure CMD_AsyncProcessMatrix(data: Pointer);
  public
    constructor Create; override;
    procedure Open(train_file: SystemString);
    procedure Open_Stream(stream: TMemoryStream64);
    function Process(Input: TMemoryRaster): TMMOD_Desc;
    procedure ProcessC(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD3L_AsyncProcess_C);
    procedure ProcessM(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD3L_AsyncProcess_M);
    procedure ProcessP(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD3L_AsyncProcess_P);
    procedure ProcessMatrixC(UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD3L_AsyncProcessMatrix_C);
    procedure ProcessMatrixM(UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD3L_AsyncProcessMatrix_M);
    procedure ProcessMatrixP(UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD3L_AsyncProcessMatrix_P);
  end;
{$ENDREGION 'TAI_DNN_Thread_MMOD3L'}
{$REGION 'TAI_DNN_Thread_RNIC'}

  TAI_DNN_Thread_RNIC_AsyncProcess_C = procedure(ThSender: TAI_DNN_Thread_RNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
  TAI_DNN_Thread_RNIC_AsyncProcess_M = procedure(ThSender: TAI_DNN_Thread_RNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec) of object;
{$IFDEF FPC}
  TAI_DNN_Thread_RNIC_AsyncProcess_P = procedure(ThSender: TAI_DNN_Thread_RNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec) is nested;
{$ELSE FPC}
  TAI_DNN_Thread_RNIC_AsyncProcess_P = reference to procedure(ThSender: TAI_DNN_Thread_RNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
{$ENDIF FPC}

  TAI_DNN_Thread_RNIC = class(TAI_DNN_Thread)
  private type
    TCMD_SyncProcess = record
      Done: TAtomBool;
      Input: TMemoryRaster;
      num_crops: Integer;
      output: TLVec;
    end;

    TCMD_AsyncProcess = record
      UserData: Pointer;
      Input: TMemoryRaster;
      num_crops: Integer;
      FreeInput: Boolean;
      OnResult_C: TAI_DNN_Thread_RNIC_AsyncProcess_C;
      OnResult_M: TAI_DNN_Thread_RNIC_AsyncProcess_M;
      OnResult_P: TAI_DNN_Thread_RNIC_AsyncProcess_P;
      output: TLVec;
    end;

    PCMD_SyncProcess = ^TCMD_SyncProcess;
    PCMD_AsyncProcess = ^TCMD_AsyncProcess;
  private
    RNICHnd: TRNIC_Handle;
    procedure ThreadFree; override;
    procedure CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_SyncProcess(data: Pointer);
    procedure OnComputeThreadResult(ThSender: TCompute);
    procedure CMD_AsyncProcess(data: Pointer);
  public
    constructor Create; override;
    procedure Open(train_file: SystemString);
    procedure Open_Stream(stream: TMemoryStream64);
    function Process(Input: TMemoryRaster; num_crops: Integer): TLVec;
    procedure ProcessC(UserData: Pointer; Input: TMemoryRaster; num_crops: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_RNIC_AsyncProcess_C);
    procedure ProcessM(UserData: Pointer; Input: TMemoryRaster; num_crops: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_RNIC_AsyncProcess_M);
    procedure ProcessP(UserData: Pointer; Input: TMemoryRaster; num_crops: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_RNIC_AsyncProcess_P);
  end;
{$ENDREGION 'TAI_DNN_Thread_RNIC'}
{$REGION 'TAI_DNN_Thread_LRNIC'}

  TAI_DNN_Thread_LRNIC_AsyncProcess_C = procedure(ThSender: TAI_DNN_Thread_LRNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
  TAI_DNN_Thread_LRNIC_AsyncProcess_M = procedure(ThSender: TAI_DNN_Thread_LRNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec) of object;
{$IFDEF FPC}
  TAI_DNN_Thread_LRNIC_AsyncProcess_P = procedure(ThSender: TAI_DNN_Thread_LRNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec) is nested;
{$ELSE FPC}
  TAI_DNN_Thread_LRNIC_AsyncProcess_P = reference to procedure(ThSender: TAI_DNN_Thread_LRNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
{$ENDIF FPC}

  TAI_DNN_Thread_LRNIC = class(TAI_DNN_Thread)
  private type
    TCMD_SyncProcess = record
      Done: TAtomBool;
      Input: TMemoryRaster;
      num_crops: Integer;
      output: TLVec;
    end;

    TCMD_AsyncProcess = record
      UserData: Pointer;
      Input: TMemoryRaster;
      num_crops: Integer;
      FreeInput: Boolean;
      OnResult_C: TAI_DNN_Thread_LRNIC_AsyncProcess_C;
      OnResult_M: TAI_DNN_Thread_LRNIC_AsyncProcess_M;
      OnResult_P: TAI_DNN_Thread_LRNIC_AsyncProcess_P;
      output: TLVec;
    end;

    PCMD_SyncProcess = ^TCMD_SyncProcess;
    PCMD_AsyncProcess = ^TCMD_AsyncProcess;
  private
    LRNICHnd: TLRNIC_Handle;
    procedure ThreadFree; override;
    procedure CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_SyncProcess(data: Pointer);
    procedure OnComputeThreadResult(ThSender: TCompute);
    procedure CMD_AsyncProcess(data: Pointer);
  public
    constructor Create; override;
    procedure Open(train_file: SystemString);
    procedure Open_Stream(stream: TMemoryStream64);
    function Process(Input: TMemoryRaster; num_crops: Integer): TLVec;
    procedure ProcessC(UserData: Pointer; Input: TMemoryRaster; num_crops: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_LRNIC_AsyncProcess_C);
    procedure ProcessM(UserData: Pointer; Input: TMemoryRaster; num_crops: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_LRNIC_AsyncProcess_M);
    procedure ProcessP(UserData: Pointer; Input: TMemoryRaster; num_crops: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_LRNIC_AsyncProcess_P);
  end;
{$ENDREGION 'TAI_DNN_Thread_LRNIC'}
{$REGION 'TAI_DNN_Thread_GDCNIC'}

  TAI_DNN_Thread_GDCNIC_AsyncProcess_C = procedure(ThSender: TAI_DNN_Thread_GDCNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
  TAI_DNN_Thread_GDCNIC_AsyncProcess_M = procedure(ThSender: TAI_DNN_Thread_GDCNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec) of object;
{$IFDEF FPC}
  TAI_DNN_Thread_GDCNIC_AsyncProcess_P = procedure(ThSender: TAI_DNN_Thread_GDCNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec) is nested;
{$ELSE FPC}
  TAI_DNN_Thread_GDCNIC_AsyncProcess_P = reference to procedure(ThSender: TAI_DNN_Thread_GDCNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
{$ENDIF FPC}

  TAI_DNN_Thread_GDCNIC = class(TAI_DNN_Thread)
  private type
    TCMD_SyncProcess = record
      Done: TAtomBool;
      Input: TMemoryRaster;
      SS_Width, SS_Height: Integer;
      output: TLVec;
    end;

    TCMD_AsyncProcess = record
      UserData: Pointer;
      Input: TMemoryRaster;
      SS_Width, SS_Height: Integer;
      FreeInput: Boolean;
      OnResult_C: TAI_DNN_Thread_GDCNIC_AsyncProcess_C;
      OnResult_M: TAI_DNN_Thread_GDCNIC_AsyncProcess_M;
      OnResult_P: TAI_DNN_Thread_GDCNIC_AsyncProcess_P;
      output: TLVec;
    end;

    PCMD_SyncProcess = ^TCMD_SyncProcess;
    PCMD_AsyncProcess = ^TCMD_AsyncProcess;
  private
    GDCNICHnd: TGDCNIC_Handle;
    procedure ThreadFree; override;
    procedure CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_SyncProcess(data: Pointer);
    procedure OnComputeThreadResult(ThSender: TCompute);
    procedure CMD_AsyncProcess(data: Pointer);
  public
    constructor Create; override;
    procedure Open(train_file: SystemString);
    procedure Open_Stream(stream: TMemoryStream64);
    function Process(Input: TMemoryRaster; SS_Width, SS_Height: Integer): TLVec;
    procedure ProcessC(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_GDCNIC_AsyncProcess_C);
    procedure ProcessM(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_GDCNIC_AsyncProcess_M);
    procedure ProcessP(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_GDCNIC_AsyncProcess_P);
  end;
{$ENDREGION 'TAI_DNN_Thread_GDCNIC'}
{$REGION 'TAI_DNN_Thread_GNIC'}

  TAI_DNN_Thread_GNIC_AsyncProcess_C = procedure(ThSender: TAI_DNN_Thread_GNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
  TAI_DNN_Thread_GNIC_AsyncProcess_M = procedure(ThSender: TAI_DNN_Thread_GNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec) of object;
{$IFDEF FPC}
  TAI_DNN_Thread_GNIC_AsyncProcess_P = procedure(ThSender: TAI_DNN_Thread_GNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec) is nested;
{$ELSE FPC}
  TAI_DNN_Thread_GNIC_AsyncProcess_P = reference to procedure(ThSender: TAI_DNN_Thread_GNIC; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
{$ENDIF FPC}

  TAI_DNN_Thread_GNIC = class(TAI_DNN_Thread)
  private type
    TCMD_SyncProcess = record
      Done: TAtomBool;
      Input: TMemoryRaster;
      SS_Width, SS_Height: Integer;
      output: TLVec;
    end;

    TCMD_AsyncProcess = record
      UserData: Pointer;
      Input: TMemoryRaster;
      SS_Width, SS_Height: Integer;
      FreeInput: Boolean;
      OnResult_C: TAI_DNN_Thread_GNIC_AsyncProcess_C;
      OnResult_M: TAI_DNN_Thread_GNIC_AsyncProcess_M;
      OnResult_P: TAI_DNN_Thread_GNIC_AsyncProcess_P;
      output: TLVec;
    end;

    PCMD_SyncProcess = ^TCMD_SyncProcess;
    PCMD_AsyncProcess = ^TCMD_AsyncProcess;
  private
    GNICHnd: TGNIC_Handle;
    procedure ThreadFree; override;
    procedure CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_SyncProcess(data: Pointer);
    procedure OnComputeThreadResult(ThSender: TCompute);
    procedure CMD_AsyncProcess(data: Pointer);
  public
    constructor Create; override;
    procedure Open(train_file: SystemString);
    procedure Open_Stream(stream: TMemoryStream64);
    function Process(Input: TMemoryRaster; SS_Width, SS_Height: Integer): TLVec;
    procedure ProcessC(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_GNIC_AsyncProcess_C);
    procedure ProcessM(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_GNIC_AsyncProcess_M);
    procedure ProcessP(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_GNIC_AsyncProcess_P);
  end;
{$ENDREGION 'TAI_DNN_Thread_GNIC'}
{$REGION 'TAI_DNN_Thread_SS'}

  TAI_DNN_Thread_SS_AsyncProcess_C = procedure(ThSender: TAI_DNN_Thread_SS; UserData: Pointer; Input: TMemoryRaster; SSTokenOutput: TPascalStringList; output: TMemoryRaster);
  TAI_DNN_Thread_SS_AsyncProcess_M = procedure(ThSender: TAI_DNN_Thread_SS; UserData: Pointer; Input: TMemoryRaster; SSTokenOutput: TPascalStringList; output: TMemoryRaster) of object;
{$IFDEF FPC}
  TAI_DNN_Thread_SS_AsyncProcess_P = procedure(ThSender: TAI_DNN_Thread_SS; UserData: Pointer; Input: TMemoryRaster; SSTokenOutput: TPascalStringList; output: TMemoryRaster) is nested;
{$ELSE FPC}
  TAI_DNN_Thread_SS_AsyncProcess_P = reference to procedure(ThSender: TAI_DNN_Thread_SS; UserData: Pointer; Input: TMemoryRaster; SSTokenOutput: TPascalStringList; output: TMemoryRaster);
{$ENDIF FPC}

  TAI_DNN_Thread_SS = class(TAI_DNN_Thread)
  private type
    TCMD_SyncProcess = record
      Done: TAtomBool;
      Input: TMemoryRaster;
      colorPool: TSegmentationColorTable;
      SSTokenOutput: TPascalStringList;
      output: TMemoryRaster;
    end;

    TCMD_AsyncProcess = record
      UserData: Pointer;
      Input: TMemoryRaster;
      colorPool: TSegmentationColorTable;
      FreeInput: Boolean;
      OnResult_C: TAI_DNN_Thread_SS_AsyncProcess_C;
      OnResult_M: TAI_DNN_Thread_SS_AsyncProcess_M;
      OnResult_P: TAI_DNN_Thread_SS_AsyncProcess_P;
      output: TMemoryRaster;
      SSTokenOutput: TPascalStringList;
    end;

    PCMD_SyncProcess = ^TCMD_SyncProcess;
    PCMD_AsyncProcess = ^TCMD_AsyncProcess;
  private
    SSHnd: TSS_Handle;
    procedure ThreadFree; override;
    procedure CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_SyncProcess(data: Pointer);
    procedure OnComputeThreadResult(ThSender: TCompute);
    procedure CMD_AsyncProcess(data: Pointer);
  public
    constructor Create; override;
    procedure Open(train_file: SystemString);
    procedure Open_Stream(stream: TMemoryStream64);
    function Process(Input: TMemoryRaster; colorPool: TSegmentationColorTable; SSTokenOutput: TPascalStringList): TMemoryRaster;
    procedure ProcessC(UserData: Pointer; Input: TMemoryRaster; colorPool: TSegmentationColorTable; FreeInput: Boolean; OnResult: TAI_DNN_Thread_SS_AsyncProcess_C);
    procedure ProcessM(UserData: Pointer; Input: TMemoryRaster; colorPool: TSegmentationColorTable; FreeInput: Boolean; OnResult: TAI_DNN_Thread_SS_AsyncProcess_M);
    procedure ProcessP(UserData: Pointer; Input: TMemoryRaster; colorPool: TSegmentationColorTable; FreeInput: Boolean; OnResult: TAI_DNN_Thread_SS_AsyncProcess_P);
  end;
{$ENDREGION 'TAI_DNN_Thread_SS'}
{$REGION 'TAI_DNN_Thread_ZMetric'}

  TAI_DNN_Thread_ZMetric_AsyncProcess_C = procedure(ThSender: TAI_DNN_Thread_ZMetric; UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; output: TLVec);
  TAI_DNN_Thread_ZMetric_AsyncProcess_M = procedure(ThSender: TAI_DNN_Thread_ZMetric; UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; output: TLVec) of object;
  TAI_DNN_Thread_ZMetric_AsyncProcess_List_C = procedure(ThSender: TAI_DNN_Thread_ZMetric; UserData: Pointer; Input: TMemoryRasterList; output: TLMatrix);
  TAI_DNN_Thread_ZMetric_AsyncProcess_List_M = procedure(ThSender: TAI_DNN_Thread_ZMetric; UserData: Pointer; Input: TMemoryRasterList; output: TLMatrix) of object;
{$IFDEF FPC}
  TAI_DNN_Thread_ZMetric_AsyncProcess_P = procedure(ThSender: TAI_DNN_Thread_ZMetric; UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; output: TLVec) is nested;
  TAI_DNN_Thread_ZMetric_AsyncProcess_List_P = procedure(ThSender: TAI_DNN_Thread_ZMetric; UserData: Pointer; Input: TMemoryRasterList; output: TLMatrix) is nested;
{$ELSE FPC}
  TAI_DNN_Thread_ZMetric_AsyncProcess_P = reference to procedure(ThSender: TAI_DNN_Thread_ZMetric; UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; output: TLVec);
  TAI_DNN_Thread_ZMetric_AsyncProcess_List_P = reference to procedure(ThSender: TAI_DNN_Thread_ZMetric; UserData: Pointer; Input: TMemoryRasterList; output: TLMatrix);
{$ENDIF FPC}

  TAI_DNN_Thread_ZMetric = class(TAI_DNN_Thread)
  private type
    TCMD_SyncProcess = record
      Done: TAtomBool;
      Input: TMemoryRaster;
      SS_Width, SS_Height: Integer;
      output: TLVec;
    end;

    TCMD_AsyncProcess = record
      UserData: Pointer;
      Input: TMemoryRaster;
      SS_Width, SS_Height: Integer;
      FreeInput: Boolean;
      OnResult_C: TAI_DNN_Thread_ZMetric_AsyncProcess_C;
      OnResult_M: TAI_DNN_Thread_ZMetric_AsyncProcess_M;
      OnResult_P: TAI_DNN_Thread_ZMetric_AsyncProcess_P;
      output: TLVec;
    end;

    TCMD_AsyncProcess_List = record
      UserData: Pointer;
      Input: TMemoryRasterList;
      SS_Width, SS_Height: Integer;
      FreeInput: Boolean;
      OnResult_C: TAI_DNN_Thread_ZMetric_AsyncProcess_List_C;
      OnResult_M: TAI_DNN_Thread_ZMetric_AsyncProcess_List_M;
      OnResult_P: TAI_DNN_Thread_ZMetric_AsyncProcess_List_P;
      output: TLMatrix;
    end;

    PCMD_SyncProcess = ^TCMD_SyncProcess;
    PCMD_AsyncProcess = ^TCMD_AsyncProcess;
    PCMD_AsyncProcess_List = ^TCMD_AsyncProcess_List;
  private
    ZMetricHnd: TZMetric_Handle;
    procedure ThreadFree; override;
    procedure CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
    procedure CMD_SyncProcess(data: Pointer);
    procedure CMD_AsyncProcess_Result(ThSender: TCompute);
    procedure CMD_AsyncProcess(data: Pointer);
    procedure CMD_AsyncProcess_List_Result(ThSender: TCompute);
    procedure CMD_AsyncProcess_List(data: Pointer);
  public
    constructor Create; override;
    procedure Open(train_file: SystemString);
    procedure Open_Stream(stream: TMemoryStream64);
    function Process(Input: TMemoryRaster; SS_Width, SS_Height: Integer): TLVec;
    procedure ProcessC(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_ZMetric_AsyncProcess_C);
    procedure ProcessM(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_ZMetric_AsyncProcess_M);
    procedure ProcessP(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_ZMetric_AsyncProcess_P);
    procedure ProcessListC(UserData: Pointer; Input: TMemoryRasterList; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_ZMetric_AsyncProcess_List_C);
    procedure ProcessListM(UserData: Pointer; Input: TMemoryRasterList; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_ZMetric_AsyncProcess_List_M);
    procedure ProcessListP(UserData: Pointer; Input: TMemoryRasterList; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_ZMetric_AsyncProcess_List_P);
  end;
{$ENDREGION 'TAI_DNN_Thread_ZMetric'}
{$REGION 'Parallel'}

  // AI parallel for CPU Platform
  TAI_Parallel_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TAI>;

  TAI_Parallel = class(TAI_Parallel_Decl)
  private
    Critical: TCritical;
    FInternalFaceSP: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Remove(AI_: TAI);
    procedure Clear;
    procedure Delete(index: Integer);

    procedure Prepare_Parallel(eng: SystemString; poolSiz: Integer); overload;
    procedure Prepare_Parallel(lib_p: PAI_EntryAPI; poolSiz: Integer); overload;
    procedure Prepare_Parallel(poolSiz: Integer); overload;
    procedure Prepare_Parallel; overload;

    procedure Prepare_FaceSP;                                // Prepare OD6L Face Model
    procedure Prepare_OD6L(stream: TMemoryStream64);         // Prepare OD6L Model
    procedure Prepare_OD3L(stream: TMemoryStream64);         // Prepare OD3L Model
    procedure Prepare_OD6L_Marshal(stream: TMemoryStream64); // Prepare OD6L Marshal Model
    procedure Prepare_SP(stream: TMemoryStream64);           // Prepare ShapePredictor Model
    function GetAndLockAI: TAI;
    procedure UnLockAI(AI: TAI);
    function Busy: Integer;
    property InternalFaceSP: Boolean read FInternalFaceSP;
  end;
{$ENDREGION 'Parallel'}
{$REGION 'IOProcessor'}

  TAI_IO_Processor = class;

  { async IO input define }
  TAI_IO = class(TCoreClassInterfacedObject)
  public
    Owner: TAI_IO_Processor;
    InputRaster: TMemoryRaster;
    OutputRaster: TMemoryRaster;
    IndexNumber: UInt64;

    function GetAI: TAI; virtual;
    function GetAIPool: TAI_Parallel; virtual;
    constructor Create(Owner_: TAI_IO_Processor); virtual;
    destructor Destroy; override;
    procedure ProcessBefore(UserData: Pointer); virtual;
    function Process(UserData: Pointer): Boolean; virtual; // return is True then append IO to OutputBuffer
    procedure ProcessAfter(UserData: Pointer); virtual;
  end;

  TAI_IO_Buffer = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TAI_IO>;

  TAI_IO_Class = class of TAI_IO;

  { async IO processor }
  TAI_IO_Processor = class(TCoreClassInterfacedObject)
  private
    FAI: TAI;
    FAIPool: TAI_Parallel;
    FIO_Class: TAI_IO_Class;
    FInputBuffer, FOutputBuffer: TAI_IO_Buffer;
    FIOThreadRuning: TAtomBool;
    FParallelProcessor: Boolean;
    FIndexNumber: UInt64;
    procedure LockInputBuffer;
    procedure UnLockInputBuffer;
    procedure IOProcessorThreadRun(ThSender: TCompute);
  public
    constructor Create(IO_Class_: TAI_IO_Class);
    destructor Destroy; override;

    procedure Clear;

    { input and process }
    procedure InputPicture(fileName: U_String); overload;
    procedure InputPicture(stream: TCoreClassStream); overload;
    procedure Input(Raster: TMemoryRaster; RasterInstance_: Boolean);
    function InputCount: Integer;
    procedure Process(UserData: Pointer);
    function Finished: Boolean;
    procedure WaitProcessDone;
    procedure RemoveFirstInput();

    { output }
    function LockOutputBuffer: TAI_IO_Buffer;
    procedure UnLockOutputBuffer(freeObj_: Boolean); overload;
    procedure UnLockOutputBuffer; overload;

    { AI Engine }
    property AI: TAI read FAI write FAI;
    property AIPool: TAI_Parallel read FAIPool write FAIPool;

    { IO Class }
    property IO_Class: TAI_IO_Class read FIO_Class write FIO_Class;

    property ParallelProcessor: Boolean read FParallelProcessor write FParallelProcessor;
  end;

{$ENDREGION 'IOProcessor'}
{$REGION 'backcall API'}


procedure API_OnOneStep(Sender: PAI_EntryAPI; one_step_calls: UInt64; average_loss, learning_rate: Double); stdcall;
procedure API_OnPause(); stdcall;
procedure API_StatusIO_Out(Sender: PAI_EntryAPI; i_char: Integer); stdcall;
function API_GetTimeTick64(): UInt64; stdcall;
function API_BuildString(p: Pointer; Size: Integer): Pointer; stdcall;
procedure API_FreeString(p: Pointer); stdcall;
function API_GetRaster(hnd: PRaster_Handle; var Bits: Pointer; var Width, Height: Integer): Byte; stdcall;
function API_GetImage(hnd: PImage_Handle; var Bits: Pointer; var Width, Height: Integer): Byte; stdcall;
function API_RecycleImage(Sender: PAI_EntryAPI; hnd: PImage_Handle): Byte; stdcall;
function API_GetDetectorDefineNum(hnd: PImage_Handle): Integer; stdcall;
function API_GetDetectorDefineImage(hnd: PImage_Handle; detIndex: Integer; var Bits: Pointer; var Width, Height: Integer): Byte; stdcall;
function API_GetDetectorDefineRect(hnd: PImage_Handle; detIndex: Integer; var rect_: TAI_Rect): Byte; stdcall;
function API_GetDetectorDefineLabel(hnd: PImage_Handle; detIndex: Integer; var p: P_Bytes): Byte; stdcall;
procedure API_FreeDetectorDefineLabel(var p: P_Bytes); stdcall;
function API_GetDetectorDefinePartNum(hnd: PImage_Handle; detIndex: Integer): Integer; stdcall;
function API_GetDetectorDefinePart(hnd: PImage_Handle; detIndex, partIndex: Integer; var part_: TAI_Point): Byte; stdcall;
function API_GetSegmentationMaskMergeImage(hnd: PImage_Handle; var Bits: Pointer; var Width, Height: Integer): Byte; stdcall;
function API_QuerySegmentationMaskColorID(cl: PSegmentationColorTable; color: TRColor; def: WORD): WORD; stdcall;

type
  TMetric_ResNet_SaveToLearnEngine_DT_UserData_ = record
    lr: TLearn;
    Snapshot: Boolean;
    imgData: TAI_Image;
    detDef: TAI_DetectorDefine;
  end;

  PMetric_ResNet_SaveToLearnEngine_DT_UserData_ = ^TMetric_ResNet_SaveToLearnEngine_DT_UserData_;

procedure Metric_ResNet_SaveToLearnEngine_DT_Backcall(ThSender: TAI_DNN_Thread_Metric; UserData: Pointer; Input: TMemoryRaster; output: TLVec);

type
  TLMetric_ResNet_SaveToLearnEngine_DT_UserData_ = record
    lr: TLearn;
    Snapshot: Boolean;
    imgData: TAI_Image;
    detDef: TAI_DetectorDefine;
  end;

  PLMetric_ResNet_SaveToLearnEngine_DT_UserData_ = ^TLMetric_ResNet_SaveToLearnEngine_DT_UserData_;

procedure LMetric_ResNet_SaveToLearnEngine_DT_Backcall(ThSender: TAI_DNN_Thread_LMetric; UserData: Pointer; Input: TMemoryRaster; output: TLVec);

type
  TZMetric_SaveToLearnEngine_DT_UserData_ = record
    lr: TLearn;
    Snapshot: Boolean;
    imgData: TAI_Image;
    detDef: TAI_DetectorDefine;
  end;

  PZMetric_SaveToLearnEngine_DT_UserData_ = ^TZMetric_SaveToLearnEngine_DT_UserData_;

procedure ZMetric_SaveToLearnEngine_DT_Backcall(ThSender: TAI_DNN_Thread_ZMetric; UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; output: TLVec);

{$ENDREGION 'backcall API'}
{$REGION 'API'}
procedure Wait_AI_Init;

function CheckZAI(libFile: SystemString): Boolean;
function Load_ZAI(libFile: SystemString): PAI_EntryAPI; { thread instance support. }
function Prepare_AI_Engine(eng: SystemString): PAI_EntryAPI; overload;
function Prepare_AI_Engine: PAI_EntryAPI; overload;
procedure Close_AI_Engine;

function Alloc_P_Bytes(const buff: U_String): P_Bytes; overload;
function Alloc_P_Bytes_FromBuff(const buff: TBytes): P_Bytes; overload;
procedure Free_P_Bytes(const buff: P_Bytes);
function Get_P_Bytes_String(const buff: P_Bytes): U_String;

function Rect(const V: TAI_Rect): TRect; overload;
function Rect(const V: TOD_Rect): TRect; overload;

function AIRect(const V: TRect): TAI_Rect; overload;
function AIRect(const V: TRectV2): TAI_Rect; overload;
function AIRect(const V: TOD_Rect): TAI_Rect; overload;
function AIRect(const V: TAI_MMOD_Rect): TAI_Rect; overload;

function RectV2(const V: TAI_Rect): TRectV2; overload;
function RectV2(const V: TOD_Rect): TRectV2; overload;
function RectV2(const V: TAI_MMOD_Rect): TRectV2; overload;

function AI_Point(const V: TVec2): TAI_Point;
function Point(const V: TAI_Point): TPoint; overload;
function Vec2(const V: TAI_Point): TVec2; overload;

function InRect(V: TAI_Point; R: TAI_Rect): Boolean; overload;
function InRect(V: TSP_Desc; R: TAI_Rect): Boolean; overload;
function InRect(V: TSP_Desc; R: TRectV2): Boolean; overload;

procedure FilterOD_Desc(var desc: TOD_Desc);
procedure FilterMMOD_Desc(var desc: TMMOD_Desc);

{ sp vector }
procedure SPToVec(V: TSP_Desc; L: TVec2List); overload;
function GetSPBound(desc: TSP_Desc; endge_threshold: TGeoFloat): TRectV2;
procedure DrawSPLine(sp_desc: TSP_Desc; bp, ep: Integer; closeLine: Boolean; color: TDEColor; d: TDrawEngine); overload;
procedure DrawFaceSP(sp_desc: TSP_Desc; color: TDEColor; d: TDrawEngine); overload;

{ normal training task }
function RunTrainingTask(Task: TAI_TrainingTask; const AI: TAI; const paramFile: SystemString): Boolean;

{ large-scale training task }
function RunLargeScaleTrainingTask(
  ImgMatDatasetFile, RasterSerializedFile, Training_RasterSerializedFile, SyncFile, LogFile, StepFile, OutputModel: U_String;
  AI: TAI;
  param: THashVariantList): Boolean;

{$ENDREGION 'API'}
{$REGION 'core parameter'}


const
  { core parameter }
  C_Metric_Input_Size: Integer = 150;
  C_Metric_Dim: Integer = 32;
  C_LMetric_Input_Size: Integer = 200;
  C_LMetric_Dim: Integer = 24;
  C_RNIC_Dim: Integer = 1000;
  C_LRNIC_Dim: Integer = 10000;
  C_GDCNIC_Dim: Integer = 10000;
  C_GNIC_Dim: Integer = 10000;
  C_ZMetric_Dim: Integer = 24;
{$ENDREGION 'core parameter'}
{$REGION 'var and const'}


var
  KeepPerformanceOnTraining: TTimeTick;
  LargeScaleTrainingMemoryRecycleTime: TTimeTick;
  IOProcessorActivtedThreadNum: Integer;
  { build-in database }
  AI_BuildIn_DBEngine: TObjectDataManager;
  { build-in face shape }
  build_in_face_shape_memory: Pointer;
  build_in_face_shape_memory_siz: Int64;
  { build-in mmod for face }
  build_in_face_detector_memory: Pointer;
  build_in_face_detector_memory_siz: Int64;
  { build-in metric for face }
  build_in_face_metric_memory: Pointer;
  build_in_face_metric_memory_siz: Int64;
  { build-in metric learn (KDTree) for face }
  build_in_face_metric_learn_memory: Pointer;
  build_in_face_metric_learn_memory_siz: Int64;

const
  { The function initializes the state and the mask using the provided rectangle. }
  { After that it runs iterCount iterations of the algorithm. }
  C_CUT_MODE_INIT_WITH_RECT = 0;

  { The function initializes the state using the provided mask. }
  { Note that GC_INIT_WITH_RECT and C_CUT_MODE_INIT_WITH_RECT can be combined. }
  { Then, all the pixels outside of the ROI are automatically initialized with GC_BGD . }
  C_CUT_MODE_INIT_WITH_MASK = 1;

  { The value means that the algorithm should just resume. }
  C_CUT_MODE_GC_EVAL = 2;

  { The value means that the algorithm should just run the grabCut algorithm (a single iteration) with the fixed model }
  C_CUT_MODE_EVAL_FREEZE_MODEL = 3;

  { cut raster data type }
  C_CUT_BGD = 0;    { an obvious background pixels }
  C_CUT_FGD = 1;    { an obvious foreground (object) pixel }
  C_CUT_PR_BGD = 2; { possible background pixel }
  C_CUT_PR_FGD = 3; { a possible foreground pixel }

{$ENDREGION 'var and const'}
{$REGION 'TEST'}
procedure test_imageProcessing(imgfile: U_String);
procedure test_poissonBlend(sourfile, sourMask, destFile: U_String);
procedure test_Unmixed(sourfile: U_String);
procedure test_mmod_largescale_training(dataset_file, sync_file, output_file: U_String);
{$ENDREGION 'TEST'}

implementation

uses DoStatusIO, Math;

{$IFDEF Z_AI_Dataset_Build_In}
{$RESOURCE zAI_BuildIn.RES}
{$ENDIF Z_AI_Dataset_Build_In}


var
  AI_Entry_Cache: THashList;
  AI_Status_Critical: TCritical;
  AI_Status_Buffer: TMemoryStream64;

{$REGION 'back caller'}


procedure API_OnOneStep(Sender: PAI_EntryAPI; one_step_calls: UInt64; average_loss, learning_rate: Double); stdcall;
var
  L: TMemoryRasterList;
  i: Integer;
  recycle_mem: Int64;
begin
  try
    Sender^.OneStepList.AddStep(one_step_calls, average_loss, learning_rate);

    if Sender^.RasterSerialized <> nil then
      if GetTimeTick() - Sender^.SerializedTime > 100 then
        begin
          Sender^.RasterSerialized.Critical.Acquire;
          L := Sender^.RasterSerialized.ReadHistory;
          recycle_mem := 0;
          i := 0;
          while i < L.Count do
            begin
              if GetTimeTick() - L[i].ActiveTimeTick() > LargeScaleTrainingMemoryRecycleTime then
                begin
                  inc(recycle_mem, L[i].RecycleMemory());
                  L.Delete(i);
                end
              else
                  inc(i);
            end;
          Sender^.RasterSerialized.Critical.Release;
          Sender^.SerializedTime := GetTimeTick();
        end;

    if KeepPerformanceOnTraining > 0 then
        TCoreClassThread.Sleep(KeepPerformanceOnTraining);
  except
  end;
end;

procedure API_OnPause(); stdcall;
begin
  TCompute.Sleep(1);
end;

{ i_char = unicode encoded char }
procedure API_StatusIO_Out(Sender: PAI_EntryAPI; i_char: Integer); stdcall;
var
  buff: TBytes;
  al: TAI_Log;
begin
  AI_Status_Critical.Acquire;
  try
    if (i_char in [10, 13]) then
      begin
        if (AI_Status_Buffer.Size > 0) then
          begin
            SetLength(buff, AI_Status_Buffer.Size);
            CopyPtr(AI_Status_Buffer.memory, @buff[0], AI_Status_Buffer.Size);
            AI_Status_Buffer.Clear;

            al.LogTime := umlNow();
            al.LogText := umlStringOf(buff);
            Sender^.Log.Add(al);

            DoStatus(al.LogText, i_char);

            SetLength(buff, 0);
          end
        else if i_char = 10 then
            DoStatus('');
      end
    else
      begin
        AI_Status_Buffer.WriteUInt8(i_char);
      end;
  except
  end;
  AI_Status_Critical.Release;
end;

function API_GetTimeTick64(): UInt64; stdcall;
begin
  Result := GetTimeTick();
end;

function API_BuildString(p: Pointer; Size: Integer): Pointer; stdcall;
var
  b: TBytes;
  pp: PPascalString;
begin
  new(pp);
  pp^ := '';
  Result := pp;
  if Size > 0 then
    begin
      SetLength(b, Size);
      CopyPtr(p, @b[0], Size);
      pp^.Bytes := b;
      SetLength(b, 0);
    end;
end;

procedure API_FreeString(p: Pointer); stdcall;
begin
  Dispose(PPascalString(p));
end;

function API_GetRaster(hnd: PRaster_Handle; var Bits: Pointer; var Width, Height: Integer): Byte; stdcall;
begin
  Result := 0;
  Bits := hnd^.Raster.Bits;
  Width := hnd^.Raster.Width;
  Height := hnd^.Raster.Height;
  Result := 1;
end;

function API_GetImage(hnd: PImage_Handle; var Bits: Pointer; var Width, Height: Integer): Byte; stdcall;
begin
  Result := 0;
  Bits := hnd^.image.Raster.Bits;
  Width := hnd^.image.Raster.Width;
  Height := hnd^.image.Raster.Height;
  Result := 1;
  AtomInc(hnd^.AccessImage);
end;

function API_RecycleImage(Sender: PAI_EntryAPI; hnd: PImage_Handle): Byte; stdcall;
begin
  Result := 0;
  if Sender^.RasterSerialized <> nil then
    begin
      Sender^.RasterSerialized.Critical.Acquire;
      hnd^.image.RecycleMemory();
      Sender^.RasterSerialized.Critical.Release;
    end;
end;

function API_GetDetectorDefineNum(hnd: PImage_Handle): Integer; stdcall;
begin
  Result := hnd^.image.DetectorDefineList.Count;
end;

function API_GetDetectorDefineImage(hnd: PImage_Handle; detIndex: Integer; var Bits: Pointer; var Width, Height: Integer): Byte; stdcall;
var
  img: TMemoryRaster;
begin
  Result := 0;
  if hnd = nil then
      exit;
  if (detIndex < 0) or (detIndex >= hnd^.image.DetectorDefineList.Count) then
      exit;
  img := hnd^.image.DetectorDefineList[detIndex].PrepareRaster;
  if img.Empty then
      exit;

  Bits := img.Bits;
  Width := img.Width;
  Height := img.Height;
  Result := 1;
  AtomInc(hnd^.AccessDetectorImage);
end;

function API_GetDetectorDefineRect(hnd: PImage_Handle; detIndex: Integer; var rect_: TAI_Rect): Byte; stdcall;
var
  detDef: TAI_DetectorDefine;
begin
  Result := 0;
  if hnd = nil then
      exit;
  if (detIndex < 0) or (detIndex >= hnd^.image.DetectorDefineList.Count) then
      exit;
  detDef := hnd^.image.DetectorDefineList[detIndex];
  rect_.Left := detDef.R.Left;
  rect_.Top := detDef.R.Top;
  rect_.Right := detDef.R.Right;
  rect_.Bottom := detDef.R.Bottom;
  Result := 1;
  AtomInc(hnd^.AccessDetectorRect);
end;

function API_GetDetectorDefineLabel(hnd: PImage_Handle; detIndex: Integer; var p: P_Bytes): Byte; stdcall;
var
  detDef: TAI_DetectorDefine;
begin
  Result := 0;
  if hnd = nil then
      exit;
  if (detIndex < 0) or (detIndex >= hnd^.image.DetectorDefineList.Count) then
      exit;
  detDef := hnd^.image.DetectorDefineList[detIndex];
  { using UTF8 format }
  p := Alloc_P_Bytes_FromBuff(detDef.Token.Bytes);
  Result := 1;
end;

procedure API_FreeDetectorDefineLabel(var p: P_Bytes); stdcall;
begin
  Free_P_Bytes(p);
  p := nil;
end;

function API_GetDetectorDefinePartNum(hnd: PImage_Handle; detIndex: Integer): Integer; stdcall;
var
  detDef: TAI_DetectorDefine;
begin
  Result := -1;
  if hnd = nil then
      exit;
  if (detIndex < 0) or (detIndex >= hnd^.image.DetectorDefineList.Count) then
      exit;
  detDef := hnd^.image.DetectorDefineList[detIndex];
  Result := detDef.Part.Count;
end;

function API_GetDetectorDefinePart(hnd: PImage_Handle; detIndex, partIndex: Integer; var part_: TAI_Point): Byte; stdcall;
var
  detDef: TAI_DetectorDefine;
begin
  Result := 0;
  if hnd = nil then
      exit;
  if (detIndex < 0) or (detIndex >= hnd^.image.DetectorDefineList.Count) then
      exit;
  detDef := hnd^.image.DetectorDefineList[detIndex];
  if (partIndex < 0) or (partIndex >= detDef.Part.Count) then
      exit;

  part_ := AI_Point(detDef.Part[partIndex]^);
  Result := 1;
end;

function API_GetSegmentationMaskMergeImage(hnd: PImage_Handle; var Bits: Pointer; var Width, Height: Integer): Byte; stdcall;
var
  img: TMemoryRaster;
begin
  Result := 0;
  if hnd = nil then
      exit;
  img := hnd^.image.SegmentationMaskList.MaskMergeRaster;

  Bits := img.Bits;
  Width := img.Width;
  Height := img.Height;
  Result := 1;
  AtomInc(hnd^.AccessMask);
end;

function API_QuerySegmentationMaskColorID(cl: PSegmentationColorTable; color: TRColor; def: WORD): WORD; stdcall;
begin
  cl^.GetColorID(color, def, Result);
end;

procedure Metric_ResNet_SaveToLearnEngine_DT_Backcall(ThSender: TAI_DNN_Thread_Metric; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
var
  p: PMetric_ResNet_SaveToLearnEngine_DT_UserData_;
  j: Integer;
  detDef: TAI_DetectorDefine;
begin
  p := UserData;
  if Length(output) <> C_Metric_Dim then
    begin
      Dispose(p);
      DoStatus('Metric-ResNet vector error!');
      exit;
    end;
  if p^.Snapshot then
    begin
      for j := 0 to p^.imgData.DetectorDefineList.Count - 1 do
        begin
          detDef := p^.imgData.DetectorDefineList[j];
          if detDef.Token.Len > 0 then
            begin
              LockObject(p^.lr);
              try
                  p^.lr.AddMemory(output, detDef.Token);
              except
              end;
              UnLockObject(p^.lr);
            end;
        end;
    end
  else
    begin
      detDef := p^.detDef;
      LockObject(p^.lr);
      try
          p^.lr.AddMemory(output, detDef.Token);
      except
      end;
      UnLockObject(p^.lr);
    end;
  Dispose(p);
end;

procedure LMetric_ResNet_SaveToLearnEngine_DT_Backcall(ThSender: TAI_DNN_Thread_LMetric; UserData: Pointer; Input: TMemoryRaster; output: TLVec);
var
  p: PLMetric_ResNet_SaveToLearnEngine_DT_UserData_;
  j: Integer;
  detDef: TAI_DetectorDefine;
begin
  p := UserData;
  if Length(output) <> C_LMetric_Dim then
    begin
      DoStatus('LMetric-ResNet vector error!');
      exit;
    end;
  if p^.Snapshot then
    begin
      for j := 0 to p^.imgData.DetectorDefineList.Count - 1 do
        begin
          detDef := p^.imgData.DetectorDefineList[j];
          if detDef.Token.Len > 0 then
            begin
              LockObject(p^.lr);
              p^.lr.AddMemory(output, detDef.Token);
              UnLockObject(p^.lr);
            end;
        end;
    end
  else
    begin
      detDef := p^.detDef;
      LockObject(p^.lr);
      p^.lr.AddMemory(output, detDef.Token);
      UnLockObject(p^.lr);
    end;
  Dispose(p);
end;

procedure ZMetric_SaveToLearnEngine_DT_Backcall(ThSender: TAI_DNN_Thread_ZMetric; UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; output: TLVec);
var
  p: PZMetric_SaveToLearnEngine_DT_UserData_;
  j: Integer;
  detDef: TAI_DetectorDefine;
begin
  p := UserData;
  if Length(output) <> C_ZMetric_Dim then
    begin
      DoStatus('Z-Metric vector error!');
      exit;
    end;
  if p^.Snapshot then
    begin
      for j := 0 to p^.imgData.DetectorDefineList.Count - 1 do
        begin
          detDef := p^.imgData.DetectorDefineList[j];
          if detDef.Token.Len > 0 then
            begin
              LockObject(p^.lr);
              p^.lr.AddMemory(output, detDef.Token);
              UnLockObject(p^.lr);
            end;
        end;
    end
  else
    begin
      detDef := p^.detDef;
      LockObject(p^.lr);
      p^.lr.AddMemory(output, detDef.Token);
      UnLockObject(p^.lr);
    end;
  Dispose(p);
end;

{$ENDREGION 'back caller'}


function CheckZAI(libFile: SystemString): Boolean;
var
  currDir: U_String;
  hnd: HMODULE;
begin
  Result := AI_Entry_Cache.Exists(libFile);
  if (not Result) and (CurrentPlatform in [epWin64, epWin32]) then
    begin
      currDir := umlGetCurrentPath;
      try
        umlSetCurrentPath(umlGetFilePath(libFile));
        hnd := GetExtLib(libFile);
        Result := hnd <> 0;
      except
      end;
      FreeExtLib(libFile);
      umlSetCurrentPath(currDir);
    end;
end;

function Load_ZAI_(libFile: SystemString): PAI_EntryAPI;
type
  TProc_Init_ai = procedure(var AI: TAI_EntryAPI); stdcall;
var
  proc_init_ai_: TProc_Init_ai;
  FAI_EntryAPI: PAI_EntryAPI;
  currDir: U_String;
  i: Integer;
begin
  Result := nil;

  if CurrentPlatform in [epWin64, epWin32] then
    begin
      LockObject(AI_Entry_Cache);
      try
        FAI_EntryAPI := PAI_EntryAPI(AI_Entry_Cache[libFile]);
        if FAI_EntryAPI <> nil then
          begin
            Result := FAI_EntryAPI;
          end
        else
          begin
            currDir := umlGetCurrentPath;
            try
              umlSetCurrentPath(umlGetFilePath(libFile));
              proc_init_ai_ := TProc_Init_ai(GetExtProc(libFile, 'init_api_entry'));
            except
              proc_init_ai_ := nil;
              FreeExtLib(libFile);
            end;
            umlSetCurrentPath(currDir);
            if Assigned(proc_init_ai_) then
              begin
                new(FAI_EntryAPI);
                FillPtrByte(FAI_EntryAPI, SizeOf(TAI_EntryAPI), 0);
                FAI_EntryAPI^.API_OnOneStep := {$IFDEF FPC}@{$ENDIF FPC}API_OnOneStep;
                FAI_EntryAPI^.API_OnPause := {$IFDEF FPC}@{$ENDIF FPC}API_OnPause;
                FAI_EntryAPI^.API_Status_Out := {$IFDEF FPC}@{$ENDIF FPC}API_StatusIO_Out;
                FAI_EntryAPI^.API_GetTimeTick64 := {$IFDEF FPC}@{$ENDIF FPC}API_GetTimeTick64;
                FAI_EntryAPI^.API_BuildString := {$IFDEF FPC}@{$ENDIF FPC}API_BuildString;
                FAI_EntryAPI^.API_FreeString := {$IFDEF FPC}@{$ENDIF FPC}API_FreeString;
                FAI_EntryAPI^.API_GetRaster := {$IFDEF FPC}@{$ENDIF FPC}API_GetRaster;
                FAI_EntryAPI^.API_GetImage := {$IFDEF FPC}@{$ENDIF FPC}API_GetImage;
                FAI_EntryAPI^.API_RecycleImage := {$IFDEF FPC}@{$ENDIF FPC}API_RecycleImage;
                FAI_EntryAPI^.API_GetDetectorDefineNum := {$IFDEF FPC}@{$ENDIF FPC}API_GetDetectorDefineNum;
                FAI_EntryAPI^.API_GetDetectorDefineImage := {$IFDEF FPC}@{$ENDIF FPC}API_GetDetectorDefineImage;
                FAI_EntryAPI^.API_GetDetectorDefineRect := {$IFDEF FPC}@{$ENDIF FPC}API_GetDetectorDefineRect;
                FAI_EntryAPI^.API_GetDetectorDefineLabel := {$IFDEF FPC}@{$ENDIF FPC}API_GetDetectorDefineLabel;
                FAI_EntryAPI^.API_FreeDetectorDefineLabel := {$IFDEF FPC}@{$ENDIF FPC}API_FreeDetectorDefineLabel;
                FAI_EntryAPI^.API_GetDetectorDefinePartNum := {$IFDEF FPC}@{$ENDIF FPC}API_GetDetectorDefinePartNum;
                FAI_EntryAPI^.API_GetDetectorDefinePart := {$IFDEF FPC}@{$ENDIF FPC}API_GetDetectorDefinePart;
                FAI_EntryAPI^.API_GetSegmentationMaskMergeImage := {$IFDEF FPC}@{$ENDIF FPC}API_GetSegmentationMaskMergeImage;
                FAI_EntryAPI^.API_QuerySegmentationMaskColorID := {$IFDEF FPC}@{$ENDIF FPC}API_QuerySegmentationMaskColorID;

                for i := Low(FAI_EntryAPI^.ComputeDeviceOfTraining) to High(FAI_EntryAPI^.ComputeDeviceOfTraining) do
                    FAI_EntryAPI^.ComputeDeviceOfTraining[i] := -1;
                FAI_EntryAPI^.ComputeDeviceOfTraining[0] := 0;

                FAI_EntryAPI^.LibraryFile := libFile;
                FAI_EntryAPI^.LoadLibraryTime := umlNow();
                FAI_EntryAPI^.OneStepList := TOneStepList.Create;
                FAI_EntryAPI^.Log := TAI_LogList.Create;
                FAI_EntryAPI^.RasterSerialized := nil;
                FAI_EntryAPI^.SerializedTime := GetTimeTick();
                FAI_EntryAPI^.Critical := TCritical.Create;

                FAI_EntryAPI^.MorphExpIntf := TMorphExpIntf.Create(FAI_EntryAPI);
                FAI_EntryAPI^.MorphExpIntf.MorphologyExpression_RegData := nil;
                try
                  proc_init_ai_(FAI_EntryAPI^);

                  if (FAI_EntryAPI^.MajorVer = 1) and (FAI_EntryAPI^.MinorVer = 34) then
                    begin
                      if FAI_EntryAPI^.Authentication = 1 then
                          FAI_EntryAPI^.Key := AIKey(FAI_EntryAPI^.Key);
                      if (FAI_EntryAPI^.CheckKey() = 0) then
                        begin
                          DoStatus('illegal License key for %s', [libFile]);
                          FAI_EntryAPI^.LibraryFile := '';
                          DisposeObject(FAI_EntryAPI^.OneStepList);
                          DisposeObject(FAI_EntryAPI^.Log);
                          DisposeObject(FAI_EntryAPI^.Critical);
                          Dispose(FAI_EntryAPI);
                          FreeExtLib(libFile);
                        end
                      else
                        begin
                          AI_Entry_Cache.Add(libFile, FAI_EntryAPI, False);
                          FAI_EntryAPI^.MorphExpIntf.MorphologyExpression_RegData := RegMorphExpExternalAPI(nil, {$IFDEF FPC}@{$ENDIF FPC}FAI_EntryAPI^.MorphExpIntf.RegMorphExpExternalAPI, nil);
                          DoStatus(FAI_EntryAPI^.GetVersionInfo());
                          Result := FAI_EntryAPI;
                        end;
                    end
                  else
                    begin
                      DoStatus('edition not supported. AI engine: %s', [umlGetFileName(libFile).Text]);
                      FAI_EntryAPI^.LibraryFile := '';
                      DisposeObject(FAI_EntryAPI^.OneStepList);
                      DisposeObject(FAI_EntryAPI^.Log);
                      DisposeObject(FAI_EntryAPI^.Critical);
                      Dispose(FAI_EntryAPI);
                      FreeExtLib(libFile);
                    end;
                except
                  DoStatus('AI engine init failed: "%s"', [umlGetFileName(libFile).Text]);
                  FAI_EntryAPI^.LibraryFile := '';
                  DisposeObject(FAI_EntryAPI^.OneStepList);
                  DisposeObject(FAI_EntryAPI^.Log);
                  DisposeObject(FAI_EntryAPI^.Critical);
                  Dispose(FAI_EntryAPI);
                  FreeExtLib(libFile);
                end;
              end
            else
              begin
                DoStatus('AI engine without support this platform: %s', [libFile]);
              end;
          end;
      finally
          UnLockObject(AI_Entry_Cache);
      end;
    end;
end;

type
  TSync_Load_ZAI_ = class
  public
    libFile: SystemString;
    APIEntry: PAI_EntryAPI;
    procedure Sync_Load();
  end;

procedure TSync_Load_ZAI_.Sync_Load();
begin
  APIEntry := Load_ZAI_(libFile);
end;

function Load_ZAI(libFile: SystemString): PAI_EntryAPI;
var
  th: TCoreClassThread;
  sync: TSync_Load_ZAI_;
begin
  th := TCompute.CurrentThread;
  if th.ThreadID = MainThreadProgress.ThreadID then
    begin
      Result := Load_ZAI_(libFile);
    end
  else
    begin
      sync := TSync_Load_ZAI_.Create;
      sync.libFile := libFile;
      TCompute.Synchronize(th, {$IFDEF FPC}@{$ENDIF FPC}sync.Sync_Load);
      Result := sync.APIEntry;
      DisposeObject(sync);
    end;
end;

var
  // build-in state
  found_build_in: TAtomBool;
  done_build_in: TAtomBool;

procedure BuildIn_Thread_Run(Sender: TCompute);
var
  fn: U_String;
  stream: TCoreClassStream;
  m64: TMemoryStream64;
  itmHnd: TItemHandle;
  p: Pointer;
begin
{$IFDEF Z_AI_Dataset_Build_In}
  try
      stream := TCoreClassResourceStream.Create(HInstance, 'zAI_BuildIn', RT_RCDATA);
  except
    found_build_in.V := False;
{$IFDEF initializationStatus}
    DoStatus('warning: no found resource "zAI_BuildIn"');
{$ENDIF initializationStatus}
    done_build_in.V := True;
    exit;
  end;
{$ELSE Z_AI_Dataset_Build_In}
  fn := WhereFileFromConfigure('zAI_BuildIn.OXC');
  if not umlFileExists(fn) then
    begin
      found_build_in.V := False;
{$IFDEF initializationStatus}
      DoStatus('warning: no found resource file: %s', [umlGetFileName(fn).Text]);
{$ENDIF initializationStatus}
      done_build_in.V := True;
      exit;
    end;
  stream := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
{$ENDIF Z_AI_Dataset_Build_In}
  found_build_in.V := True;
  stream.Position := 0;
  m64 := TMemoryStream64.Create;
  DecompressStream(stream, m64);
  DisposeObject(stream);
{$IFDEF initializationStatus}
  DoStatus('AI_BuildIn_DBEngine initialization done.');
{$ENDIF initializationStatus}
  m64.Position := 0;
  AI_BuildIn_DBEngine := TObjectDataManagerOfCache.CreateAsStream(m64, '', DBMarshal.ID, True, False, True);

  if AI_BuildIn_DBEngine.ItemOpen('/', 'build_in_face_shape.dat', itmHnd) then
    begin
      build_in_face_shape_memory_siz := itmHnd.Item.Size;
      p := GetMemory(build_in_face_shape_memory_siz);
      AI_BuildIn_DBEngine.ItemRead(itmHnd, build_in_face_shape_memory_siz, p^);
      AI_BuildIn_DBEngine.ItemClose(itmHnd);
      build_in_face_shape_memory := p;
{$IFDEF initializationStatus}
      DoStatus('Z-AI "build_in_face_shape.dat" initialization done.');
{$ENDIF initializationStatus}
    end
  else
    begin
{$IFDEF initializationStatus}
      DoStatus('Z-AI buildIn "build_in_face_shape.dat" error.');
{$ENDIF initializationStatus}
    end;

  if AI_BuildIn_DBEngine.ItemOpen('/', 'human_face_detector.svm_dnn_od', itmHnd) then
    begin
      build_in_face_detector_memory_siz := itmHnd.Item.Size;
      p := GetMemory(build_in_face_detector_memory_siz);
      AI_BuildIn_DBEngine.ItemRead(itmHnd, build_in_face_detector_memory_siz, p^);
      AI_BuildIn_DBEngine.ItemClose(itmHnd);
      build_in_face_detector_memory := p;
{$IFDEF initializationStatus}
      DoStatus('Z-AI "human_face_detector.svm_dnn_od" initialization done.');
{$ENDIF initializationStatus}
    end
  else
    begin
{$IFDEF initializationStatus}
      DoStatus('Z-AI buildIn "human_face_detector.svm_dnn_od" error.');
{$ENDIF initializationStatus}
    end;

  if AI_BuildIn_DBEngine.ItemOpen('/', 'share_face.metric', itmHnd) then
    begin
      build_in_face_metric_memory_siz := itmHnd.Item.Size;
      p := GetMemory(build_in_face_metric_memory_siz);
      AI_BuildIn_DBEngine.ItemRead(itmHnd, build_in_face_metric_memory_siz, p^);
      AI_BuildIn_DBEngine.ItemClose(itmHnd);
      build_in_face_metric_memory := p;
{$IFDEF initializationStatus}
      DoStatus('Z-AI "share_face.metric" initialization done.');
{$ENDIF initializationStatus}
    end
  else
    begin
{$IFDEF initializationStatus}
      DoStatus('Z-AI buildIn "share_face.metric" error.');
{$ENDIF initializationStatus}
    end;

  if AI_BuildIn_DBEngine.ItemOpen('/', 'share_face.learn', itmHnd) then
    begin
      build_in_face_metric_learn_memory_siz := itmHnd.Item.Size;
      p := GetMemory(build_in_face_metric_learn_memory_siz);
      AI_BuildIn_DBEngine.ItemRead(itmHnd, build_in_face_metric_learn_memory_siz, p^);
      AI_BuildIn_DBEngine.ItemClose(itmHnd);
      build_in_face_metric_learn_memory := p;
{$IFDEF initializationStatus}
      DoStatus('Z-AI "share_face.learn" initialization done.');
{$ENDIF initializationStatus}
    end
  else
    begin
{$IFDEF initializationStatus}
      DoStatus('Z-AI buildIn "share_face.learn" error.');
{$ENDIF initializationStatus}
    end;

  done_build_in.V := True;
end;

procedure Init_AI_BuildIn;
begin
  AI_Status_Critical := TCritical.Create;
  AI_Entry_Cache := THashList.Create;
  AI_Entry_Cache.AutoFreeData := False;
  AI_Entry_Cache.AccessOptimization := False;
  AI_Status_Buffer := TMemoryStream64.CustomCreate(8192);

  AI_BuildIn_DBEngine := nil;
  build_in_face_shape_memory := nil;
  build_in_face_shape_memory_siz := 0;
  build_in_face_detector_memory := nil;
  build_in_face_detector_memory_siz := 0;
  build_in_face_metric_memory := nil;
  build_in_face_metric_memory_siz := 0;
  build_in_face_metric_learn_memory := nil;
  build_in_face_metric_learn_memory_siz := 0;
  found_build_in := TAtomBool.Create(True);
  done_build_in := TAtomBool.Create(False);

  TCompute.RunC(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}BuildIn_Thread_Run);
end;

procedure Wait_AI_Init;
begin
  while not done_build_in.V do
      TCompute.Sleep(1);
end;

procedure Free_AI_BuildIn;
begin
  Close_AI_Engine;
  DisposeObject(AI_Entry_Cache);
  DisposeObject(AI_Status_Buffer);
  DisposeObject(AI_Status_Critical);

  if build_in_face_shape_memory <> nil then
      FreeMemory(build_in_face_shape_memory);

  if build_in_face_detector_memory <> nil then
      FreeMemory(build_in_face_detector_memory);

  if build_in_face_metric_memory <> nil then
      FreeMemory(build_in_face_metric_memory);

  if build_in_face_metric_learn_memory <> nil then
      FreeMemory(build_in_face_metric_learn_memory);

  DisposeObjectAndNil(found_build_in);
  DisposeObjectAndNil(done_build_in);
  DisposeObjectAndNil(AI_BuildIn_DBEngine);
end;

function Prepare_AI_Engine(eng: SystemString): PAI_EntryAPI;
begin
  Result := Load_ZAI(eng);
end;

function Prepare_AI_Engine: PAI_EntryAPI;
begin
  Result := Prepare_AI_Engine(AI_Engine_Library);
end;

procedure Close_AI_Engine;
  procedure Free_ZAI(FAI_EntryAPI: PAI_EntryAPI);
  begin
    try
      if FAI_EntryAPI <> nil then
        begin
          RemoveMorphExpExternalAPI(FAI_EntryAPI^.MorphExpIntf.MorphologyExpression_RegData);
          FAI_EntryAPI^.CloseAI();
          DisposeObject(FAI_EntryAPI^.OneStepList);
          DisposeObject(FAI_EntryAPI^.Log);
          DisposeObject(FAI_EntryAPI^.Critical);
          DisposeObject(FAI_EntryAPI^.MorphExpIntf);
          Dispose(FAI_EntryAPI);
        end;
    except
    end;
  end;

var
  i: Integer;
  p: PHashListData;
begin
  Wait_AI_Init;

  if AI_Entry_Cache.Count > 0 then
    begin
      i := 0;
      p := AI_Entry_Cache.FirstPtr;
      while i < AI_Entry_Cache.Count do
        begin
          Free_ZAI(PAI_EntryAPI(p^.data));
          p^.data := nil;
          FreeExtLib(p^.OriginName);
          inc(i);
          p := p^.Next;
        end;
    end;
  AI_Entry_Cache.Clear;
end;

function Alloc_P_Bytes(const buff: U_String): P_Bytes;
begin
  Result := Alloc_P_Bytes_FromBuff(buff.PlatformBytes);
end;

function Alloc_P_Bytes_FromBuff(const buff: TBytes): P_Bytes;
begin
  new(Result);
  Result^.Size := Length(buff);
  if Result^.Size > 0 then
    begin
      Result^.Bytes := GetMemory(Result^.Size + 1);
      CopyPtr(@buff[0], Result^.Bytes, Result^.Size);
    end
  else
      Result^.Bytes := nil;
end;

procedure Free_P_Bytes(const buff: P_Bytes);
begin
  if (buff = nil) then
      exit;
  if (buff^.Size > 0) and (buff^.Bytes <> nil) then
      FreeMemory(buff^.Bytes);
  FillPtrByte(buff, SizeOf(C_Bytes), 0);
  Dispose(buff);
end;

function Get_P_Bytes_String(const buff: P_Bytes): U_String;
var
  tmp: TBytes;
begin
  SetLength(tmp, buff^.Size);
  if buff^.Size > 0 then
      CopyPtr(buff^.Bytes, @tmp[0], buff^.Size);
  Result.PlatformBytes := tmp;
  SetLength(tmp, 0);
end;

function Rect(const V: TAI_Rect): TRect;
begin
  Result.Left := V.Left;
  Result.Top := V.Top;
  Result.Right := V.Right;
  Result.Bottom := V.Bottom;
end;

function Rect(const V: TOD_Rect): TRect;
begin
  Result.Left := V.Left;
  Result.Top := V.Top;
  Result.Right := V.Right;
  Result.Bottom := V.Bottom;
end;

function AIRect(const V: TRect): TAI_Rect;
begin
  Result.Left := V.Left;
  Result.Top := V.Top;
  Result.Right := V.Right;
  Result.Bottom := V.Bottom;
end;

function AIRect(const V: TRectV2): TAI_Rect;
begin
  Result.Left := Round(V[0, 0]);
  Result.Top := Round(V[0, 1]);
  Result.Right := Round(V[1, 0]);
  Result.Bottom := Round(V[1, 1]);
end;

function AIRect(const V: TOD_Rect): TAI_Rect;
begin
  Result.Left := V.Left;
  Result.Top := V.Top;
  Result.Right := V.Right;
  Result.Bottom := V.Bottom;
end;

function AIRect(const V: TAI_MMOD_Rect): TAI_Rect;
begin
  Result.Left := V.Left;
  Result.Top := V.Top;
  Result.Right := V.Right;
  Result.Bottom := V.Bottom;
end;

function RectV2(const V: TAI_Rect): TRectV2;
begin
  Result[0, 0] := V.Left;
  Result[0, 1] := V.Top;
  Result[1, 0] := V.Right;
  Result[1, 1] := V.Bottom;
end;

function RectV2(const V: TOD_Rect): TRectV2;
begin
  Result[0, 0] := V.Left;
  Result[0, 1] := V.Top;
  Result[1, 0] := V.Right;
  Result[1, 1] := V.Bottom;
end;

function RectV2(const V: TAI_MMOD_Rect): TRectV2;
begin
  Result[0, 0] := V.Left;
  Result[0, 1] := V.Top;
  Result[1, 0] := V.Right;
  Result[1, 1] := V.Bottom;
end;

function AI_Point(const V: TVec2): TAI_Point;
begin
  Result.X := Round(V[0]);
  Result.Y := Round(V[1]);
end;

function Point(const V: TAI_Point): TPoint;
begin
  Result.X := V.X;
  Result.Y := V.Y;
end;

function Vec2(const V: TAI_Point): TVec2;
begin
  Result[0] := V.X;
  Result[1] := V.Y;
end;

function InRect(V: TAI_Point; R: TAI_Rect): Boolean;
begin
  Result := PointInRect(Vec2(V), RectV2(R));
end;

function InRect(V: TSP_Desc; R: TAI_Rect): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(V) - 1 do
    if not InRect(V[i], R) then
        exit;
  Result := True;
end;

function InRect(V: TSP_Desc; R: TRectV2): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(V) - 1 do
    if not PointInRect(Vec2(V[i]), R) then
        exit;
  Result := True;
end;

procedure FilterOD_Desc(var desc: TOD_Desc);
var
  L: TOD_List;

  function IsOverlap(const p: TRectV2): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to L.Count - 1 do
      if RectInRect(p, RectV2(L[i])) or RectInRect(RectV2(L[i]), p) then
        begin
          Result := True;
          exit;
        end;
    Result := False;
  end;

var
  i: Integer;
begin
  if Length(desc) <= 1 then
      exit;

  L := TOD_List.Create;
  for i := 0 to Length(desc) - 1 do
    begin
      if not IsOverlap(RectV2(desc[i])) then
          L.Add(desc[i]);
    end;
  SetLength(desc, L.Count);
  for i := 0 to L.Count - 1 do
      desc[i] := L[i];
  DisposeObject(L);
end;

procedure FilterMMOD_Desc(var desc: TMMOD_Desc);
var
  L: TMMOD_RectList;

  function IsOverlap(const p: PMMOD_Rect): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to L.Count - 1 do
      if p^.Token.Same(L[i]^.Token) then
        if RectInRect(p^.R, L[i]^.R) or RectInRect(L[i]^.R, p^.R) then
          begin
            Result := True;
            exit;
          end;
    Result := False;
  end;

var
  i: Integer;
  NewDesc: TMMOD_Desc;
begin
  if Length(desc) <= 1 then
      exit;

  L := TMMOD_RectList.Create;
  for i := 0 to Length(desc) - 1 do
    begin
      if not IsOverlap(@desc[i]) then
          L.Add(@desc[i]);
    end;
  SetLength(NewDesc, L.Count);
  for i := 0 to L.Count - 1 do
      NewDesc[i] := L[i]^;
  DisposeObject(L);
  desc := NewDesc;
end;

procedure SPToVec(V: TSP_Desc; L: TVec2List);
var
  i: Integer;
begin
  for i := 0 to Length(V) - 1 do
      L.Add(Vec2(V[i]));
end;

function GetSPBound(desc: TSP_Desc; endge_threshold: TGeoFloat): TRectV2;
var
  vbuff: TArrayVec2;
  i: Integer;
  siz: TVec2;
begin
  if Length(desc) = 0 then
    begin
      Result := NullRect;
      exit;
    end;
  SetLength(vbuff, Length(desc));
  for i := 0 to Length(desc) - 1 do
      vbuff[i] := Vec2(desc[i]);

  Result := FixRect(BoundRect(vbuff));
  SetLength(vbuff, 0);

  if IsEqual(endge_threshold, 0) then
      exit;

  siz := Vec2Mul(RectSize(Result), endge_threshold);
  Result[0] := Vec2Sub(Result[0], siz);
  Result[1] := Vec2Add(Result[1], siz);
end;

procedure DrawSPLine(sp_desc: TSP_Desc; bp, ep: Integer; closeLine: Boolean; color: TDEColor; d: TDrawEngine);
var
  i: Integer;
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  for i := bp to ep do
      vl.Add(Vec2(sp_desc[i]));

  d.DrawOutSideSmoothPL(False, vl, closeLine, color, 2);
  DisposeObject(vl);
end;

procedure DrawFaceSP(sp_desc: TSP_Desc; color: TDEColor; d: TDrawEngine);
begin
  if Length(sp_desc) <> 68 then
      exit;
  DrawSPLine(sp_desc, 0, 16, False, color, d);
  DrawSPLine(sp_desc, 17, 21, False, color, d);
  DrawSPLine(sp_desc, 22, 26, False, color, d);
  DrawSPLine(sp_desc, 27, 30, False, color, d);
  DrawSPLine(sp_desc, 31, 35, False, color, d);
  d.DrawLine(Vec2(sp_desc[31]), Vec2(sp_desc[27]), color, 1);
  d.DrawLine(Vec2(sp_desc[35]), Vec2(sp_desc[27]), color, 1);
  d.DrawLine(Vec2(sp_desc[31]), Vec2(sp_desc[30]), color, 1);
  d.DrawLine(Vec2(sp_desc[35]), Vec2(sp_desc[30]), color, 1);
  DrawSPLine(sp_desc, 36, 41, True, color, d);
  DrawSPLine(sp_desc, 42, 47, True, color, d);
  DrawSPLine(sp_desc, 48, 59, True, color, d);
  DrawSPLine(sp_desc, 60, 67, True, color, d);
end;

function RunTrainingTask(Task: TAI_TrainingTask; const AI: TAI; const paramFile: SystemString): Boolean;
var
  i, j: Integer;

  startTick: TTimeTick;

  param: THashVariantList;
  ComputeFunc: SystemString;

  param_md5: TMD5;

  { batch free }
  inputfile1, inputfile2: SystemString;
  inputstream1, inputstream2: TMemoryStream64;
  inputraster1, inputraster2: TMemoryRaster;
  detDef: TAI_DetectorDefine;
  inputImgList, imgL: TAI_ImageList;
  inputImgMatrix: TAI_ImageMatrix;
  ResultValues: THashVariantList;

  { manual free }
  outputstream: TMemoryStream64;
  outputPacalStringList: TPascalStringList;
  OutputRaster: TMemoryRaster;
  local_sync1, local_sync2, sync_file1, sync_file2, output_file: SystemString;
  scale: TGeoFloat;

  metric_resnet_param: PMetric_ResNet_Train_Parameter;
  LMetric_resnet_param: PMetric_ResNet_Train_Parameter;
  mmod_param: PMMOD_Train_Parameter;
  rnic_param: PRNIC_Train_Parameter;
  GDCNIC_param: PGDCNIC_Train_Parameter;
  GNIC_param: PGNIC_Train_Parameter;
  ss_colorPool: TSegmentationColorTable;
  SS_param: PSS_Train_Parameter;
  zmetric_param: PZMetric_Train_Parameter;

  tmpPSL: TPascalStringList;
  tmpM64: TMemoryStream64;
  output_learn_file: SystemString;
  learnEng: TLearn;
  Metric_hnd: TMetric_Handle;
  LMetric_hnd: TLMetric_Handle;
begin
  Result := False;
  if Task = nil then
      exit;
  if not AI.Activted then
      exit;

  Task.LastWriteFileList.Clear;

  param := THashVariantList.Create;
  Task.Read(paramFile, param);
  param_md5 := Task.LastReadMD5;

  if param.Exists('func') then
      ComputeFunc := param['func']
  else if param.Exists('compute') then
      ComputeFunc := param['compute']
  else
      ComputeFunc := param.GetDefaultValue('ComputeFunc', '');

  inputfile1 := '';
  inputfile2 := '';
  inputstream1 := TMemoryStream64.Create;
  inputstream2 := TMemoryStream64.Create;
  inputraster1 := NewRaster();
  inputraster2 := NewRaster();
  inputImgList := TAI_ImageList.Create;
  inputImgMatrix := TAI_ImageMatrix.Create;
  ResultValues := THashVariantList.Create;

  ResultValues['Begin'] := umlNow();
  startTick := GetTimeTick();

  try
    if umlMultipleMatch(['surf', 'fastsurf'], ComputeFunc) then
      begin
{$REGION 'surf'}
        inputfile1 := param.GetDefaultValue('source', '');
        inputfile2 := param.GetDefaultValue('dest', '');

        if Task.Exists(inputfile1) and Task.Exists(inputfile2) then
          begin
            try
              Task.Read(inputfile1, inputraster1);
              Task.Read(inputfile2, inputraster2);
              inputraster1.scale(param.GetDefaultValue('scale', 1.0));
              inputraster2.scale(param.GetDefaultValue('scale', 1.0));
              OutputRaster := AI.BuildSurfMatchOutput(inputraster1, inputraster2);

              Task.write(param.GetDefaultValue('output', 'output.bmp'), OutputRaster);
              DisposeObject(OutputRaster);
              Result := True;
            except
            end;
          end;
{$ENDREGION 'surf'}
      end
    else if umlMultipleMatch(['TrainOD', 'TrainingOD', 'TrainObjectDetector', 'TrainOD6L', 'TrainingOD6L', 'TrainObjectDetector6L'], ComputeFunc) then
      begin
{$REGION 'OD'}
        inputfile1 := param.GetDefaultValue('source', '');

        if Task.Exists(inputfile1) then
          begin
            try
              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                begin
                  Task.Read(inputfile1, inputImgMatrix);
                  inputImgMatrix.scale(param.GetDefaultValue('scale', 1.0));
                end
              else
                begin
                  Task.Read(inputfile1, inputImgList);
                  inputImgList.scale(param.GetDefaultValue('scale', 1.0));
                end;

              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  outputstream := AI.LargeScale_OD6L_Train_Stream(
                  inputImgMatrix,
                  param.GetDefaultValue('window_width', 80),
                  param.GetDefaultValue('window_height', 80),
                  param.GetDefaultValue('thread', 2)
                  )
              else
                  outputstream := AI.LargeScale_OD6L_Train_Stream(
                  inputImgList,
                  param.GetDefaultValue('window_width', 80),
                  param.GetDefaultValue('window_height', 80),
                  param.GetDefaultValue('thread', 2)
                  );

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_OD6L_Ext), outputstream);
                  DisposeObject(outputstream);
                  Result := True;
                end;
            except
            end;
          end;
{$ENDREGION 'OD'}
      end
    else if umlMultipleMatch(['TrainOD3L', 'TrainingOD3L', 'TrainObjectDetector3L'], ComputeFunc) then
      begin
{$REGION 'OD3L'}
        inputfile1 := param.GetDefaultValue('source', '');

        if Task.Exists(inputfile1) then
          begin
            try
              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                begin
                  Task.Read(inputfile1, inputImgMatrix);
                  inputImgMatrix.scale(param.GetDefaultValue('scale', 1.0));
                end
              else
                begin
                  Task.Read(inputfile1, inputImgList);
                  inputImgList.scale(param.GetDefaultValue('scale', 1.0));
                end;

              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  outputstream := AI.LargeScale_OD3L_Train_Stream(
                  inputImgMatrix,
                  param.GetDefaultValue('window_width', 80),
                  param.GetDefaultValue('window_height', 80),
                  param.GetDefaultValue('thread', 2)
                  )
              else
                  outputstream := AI.LargeScale_OD3L_Train_Stream(
                  inputImgList,
                  param.GetDefaultValue('window_width', 80),
                  param.GetDefaultValue('window_height', 80),
                  param.GetDefaultValue('thread', 2)
                  );

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_OD3L_Ext), outputstream);
                  DisposeObject(outputstream);
                  Result := True;
                end;
            except
            end;
          end;
{$ENDREGION 'OD3L'}
      end
    else if umlMultipleMatch(['TrainOD_Marshal', 'TrainingOD_Marshal', 'TrainObjectDetectorMarshal', 'TrainOD6L_Marshal', 'TrainingOD6L_Marshal', 'TrainObjectDetector6LMarshal'], ComputeFunc) then
      begin
{$REGION 'OD Marshal'}
        inputfile1 := param.GetDefaultValue('source', '');

        if Task.Exists(inputfile1) then
          begin
            try
              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                begin
                  Task.Read(inputfile1, inputImgMatrix);
                  inputImgMatrix.scale(param.GetDefaultValue('scale', 1.0));
                end
              else
                begin
                  Task.Read(inputfile1, inputImgList);
                  inputImgList.scale(param.GetDefaultValue('scale', 1.0));
                end;

              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  outputstream := AI.OD6L_Marshal_Train(
                  inputImgMatrix,
                  param.GetDefaultValue('window_width', 80),
                  param.GetDefaultValue('window_height', 80),
                  param.GetDefaultValue('thread', 2)
                  )
              else
                  outputstream := AI.OD6L_Marshal_Train(
                  inputImgList,
                  param.GetDefaultValue('window_width', 80),
                  param.GetDefaultValue('window_height', 80),
                  param.GetDefaultValue('thread', 2)
                  );

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_OD6L_Marshal_Ext), outputstream);
                  DisposeObject(outputstream);
                  Result := True;
                end;
            except
            end;
          end;
{$ENDREGION 'OD Marshal'}
      end
    else if umlMultipleMatch(['TrainSP', 'TrainingSP', 'TrainShapePredictor'], ComputeFunc) then
      begin
{$REGION 'sp'}
        inputfile1 := param.GetDefaultValue('source', '');

        if Task.Exists(inputfile1) then
          begin
            try
              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                begin
                  Task.Read(inputfile1, inputImgMatrix);
                  inputImgMatrix.scale(param.GetDefaultValue('scale', 1.0));
                end
              else
                begin
                  Task.Read(inputfile1, inputImgList);
                  inputImgList.scale(param.GetDefaultValue('scale', 1.0));
                end;

              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  outputstream := AI.LargeScale_SP_Train_Stream(
                  inputImgMatrix,
                  param.GetDefaultValue('oversampling_amount', 300),
                  param.GetDefaultValue('tree_depth', 2),
                  param.GetDefaultValue('thread', 2)
                  )
              else
                  outputstream := AI.LargeScale_SP_Train_Stream(
                  inputImgList,
                  param.GetDefaultValue('oversampling_amount', 300),
                  param.GetDefaultValue('tree_depth', 2),
                  param.GetDefaultValue('thread', 2)
                  );

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_SP_Ext), outputstream);
                  DisposeObject(outputstream);
                  Result := True;
                end;
            except
            end;
          end;
{$ENDREGION 'sp'}
      end
    else if umlMultipleMatch(['TrainMRN', 'TrainingMRN', 'TrainMetricResNet'], ComputeFunc) then
      begin
{$REGION 'metric'}
        inputfile1 := param.GetDefaultValue('source', '');

        if Task.Exists(inputfile1) then
          begin
            try
              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  Task.Read(inputfile1, inputImgMatrix)
              else
                  Task.Read(inputfile1, inputImgList);

              { init sync file1. }
              local_sync1 := param.GetDefaultValue('syncfile', 'output' + C_Sync_Ext);
              sync_file1 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext);
              umlDeleteFile(sync_file1);
              if Task.Exists(local_sync1) then
                  Task.ReadToFile(local_sync1, sync_file1);
              { init sync file2. }
              local_sync2 := param.GetDefaultValue('syncfile2', 'output' + C_Sync_Ext2);
              sync_file2 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext2);
              umlDeleteFile(sync_file2);
              if Task.Exists(local_sync2) then
                  Task.ReadToFile(local_sync2, sync_file2);

              output_file := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5))) + C_Metric_Ext;

              metric_resnet_param := TAI.Init_Metric_ResNet_Parameter(sync_file1, output_file);

              metric_resnet_param^.timeout := param.GetDefaultValue('timeout', metric_resnet_param^.timeout);

              metric_resnet_param^.weight_decay := param.GetDefaultValue('weight_decay', metric_resnet_param^.weight_decay);
              metric_resnet_param^.momentum := param.GetDefaultValue('momentum', metric_resnet_param^.momentum);
              metric_resnet_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', metric_resnet_param^.iterations_without_progress_threshold);
              metric_resnet_param^.learning_rate := param.GetDefaultValue('learning_rate', metric_resnet_param^.learning_rate);
              metric_resnet_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', metric_resnet_param^.completed_learning_rate);
              metric_resnet_param^.step_mini_batch_target_num := param.GetDefaultValue('step_mini_batch_target_num', metric_resnet_param^.step_mini_batch_target_num);
              metric_resnet_param^.step_mini_batch_raster_num := param.GetDefaultValue('step_mini_batch_raster_num', metric_resnet_param^.step_mini_batch_raster_num);

              metric_resnet_param^.fullGPU_Training := param.GetDefaultValue('fullGPU_Training', metric_resnet_param^.fullGPU_Training);

              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  outputstream := AI.Metric_ResNet_Train_Stream(
                  param.GetDefaultValue('Snapshot', False),
                  inputImgMatrix,
                  metric_resnet_param)
              else
                  outputstream := AI.Metric_ResNet_Train_Stream(
                  param.GetDefaultValue('Snapshot', False),
                  inputImgList,
                  metric_resnet_param);

              TAI.Free_Metric_ResNet_Parameter(metric_resnet_param);

              { write sync1 to task }
              if umlFileExists(sync_file1) then
                  Task.WriteFile(local_sync1, sync_file1);
              { write sync2 to task }
              if umlFileExists(sync_file2) then
                  Task.WriteFile(local_sync2, sync_file2);

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_Metric_Ext), outputstream);

                  if (param.GetDefaultValue('LearnVec', False) = True) and (AI.completed_learning_rate > AI.Last_training_learning_rate) then
                    begin
                      learnEng := TLearn.CreateClassifier(ltKDT, zAI.C_Metric_Dim);
                      outputstream.Position := 0;

                      DoStatus('build metric to learn-KDTree.');
                      if param.GetDefaultValue('DNNThread', False) = True then
                        begin
                          if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                              AI.Metric_ResNet_SaveToLearnEngine_DT(outputstream, param.GetDefaultValue('Snapshot', False), inputImgMatrix, learnEng)
                          else
                              AI.Metric_ResNet_SaveToLearnEngine_DT(outputstream, param.GetDefaultValue('Snapshot', False), inputImgList, learnEng);
                        end
                      else
                        begin
                          Metric_hnd := AI.Metric_ResNet_Open_Stream(outputstream);
                          if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                              AI.Metric_ResNet_SaveToLearnEngine(Metric_hnd, param.GetDefaultValue('Snapshot', False), inputImgMatrix, learnEng)
                          else
                              AI.Metric_ResNet_SaveToLearnEngine(Metric_hnd, param.GetDefaultValue('Snapshot', False), inputImgList, learnEng);
                          AI.Metric_ResNet_Close(Metric_hnd);
                        end;
                      DoStatus('process metric to learn-KDTree done.');

                      tmpM64 := TMemoryStream64.Create;
                      learnEng.SaveToStream(tmpM64);
                      output_learn_file := umlChangeFileExt(param.GetDefaultValue('output', 'output' + C_Metric_Ext), C_Learn_Ext);
                      Task.write(param.GetDefaultValue('output' + C_Learn_Ext, output_learn_file), tmpM64);
                      DisposeObject(tmpM64);
                      DisposeObject(learnEng);
                    end;

                  DisposeObject(outputstream);
                  ResultValues['Loss'] := AI.Last_training_average_loss;
                  ResultValues['Rate'] := AI.Last_training_learning_rate;
                  ResultValues['TargetRate'] := AI.completed_learning_rate;
                  Result := True;
                end;
            except
            end;
          end;
{$ENDREGION 'metric'}
      end
    else if umlMultipleMatch(['TrainLMRN', 'TrainingLMRN', 'TrainLMetricResNet'], ComputeFunc) then
      begin
{$REGION 'LMetric'}
        inputfile1 := param.GetDefaultValue('source', '');

        if Task.Exists(inputfile1) then
          begin
            try
              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  Task.Read(inputfile1, inputImgMatrix)
              else
                  Task.Read(inputfile1, inputImgList);

              { init sync file1. }
              local_sync1 := param.GetDefaultValue('syncfile', 'output' + C_Sync_Ext);
              sync_file1 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext);
              umlDeleteFile(sync_file1);
              if Task.Exists(local_sync1) then
                  Task.ReadToFile(local_sync1, sync_file1);
              { init sync file2. }
              local_sync2 := param.GetDefaultValue('syncfile2', 'output' + C_Sync_Ext2);
              sync_file2 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext2);
              umlDeleteFile(sync_file2);
              if Task.Exists(local_sync2) then
                  Task.ReadToFile(local_sync2, sync_file2);

              output_file := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5))) + C_LMetric_Ext;

              LMetric_resnet_param := TAI.Init_LMetric_ResNet_Parameter(sync_file1, output_file);

              LMetric_resnet_param^.timeout := param.GetDefaultValue('timeout', LMetric_resnet_param^.timeout);

              LMetric_resnet_param^.weight_decay := param.GetDefaultValue('weight_decay', LMetric_resnet_param^.weight_decay);
              LMetric_resnet_param^.momentum := param.GetDefaultValue('momentum', LMetric_resnet_param^.momentum);
              LMetric_resnet_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', LMetric_resnet_param^.iterations_without_progress_threshold);
              LMetric_resnet_param^.learning_rate := param.GetDefaultValue('learning_rate', LMetric_resnet_param^.learning_rate);
              LMetric_resnet_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', LMetric_resnet_param^.completed_learning_rate);
              LMetric_resnet_param^.step_mini_batch_target_num := param.GetDefaultValue('step_mini_batch_target_num', LMetric_resnet_param^.step_mini_batch_target_num);
              LMetric_resnet_param^.step_mini_batch_raster_num := param.GetDefaultValue('step_mini_batch_raster_num', LMetric_resnet_param^.step_mini_batch_raster_num);

              LMetric_resnet_param^.fullGPU_Training := param.GetDefaultValue('fullGPU_Training', LMetric_resnet_param^.fullGPU_Training);

              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  outputstream := AI.LMetric_ResNet_Train_Stream(
                  param.GetDefaultValue('Snapshot', False),
                  inputImgMatrix,
                  LMetric_resnet_param)
              else
                  outputstream := AI.LMetric_ResNet_Train_Stream(
                  param.GetDefaultValue('Snapshot', False),
                  inputImgList,
                  LMetric_resnet_param);

              TAI.Free_LMetric_ResNet_Parameter(LMetric_resnet_param);

              { write sync1 to task }
              if umlFileExists(sync_file1) then
                  Task.WriteFile(local_sync1, sync_file1);
              { write sync2 to task }
              if umlFileExists(sync_file2) then
                  Task.WriteFile(local_sync2, sync_file2);

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_LMetric_Ext), outputstream);

                  if (param.GetDefaultValue('LearnVec', False) = True) and (AI.completed_learning_rate > AI.Last_training_learning_rate) then
                    begin
                      learnEng := TLearn.CreateClassifier(ltKDT, zAI.C_LMetric_Dim);
                      outputstream.Position := 0;

                      DoStatus('build LMetric to learn-KDTree.');
                      if param.GetDefaultValue('DNNThread', False) = True then
                        begin
                          if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                              AI.LMetric_ResNet_SaveToLearnEngine_DT(outputstream, param.GetDefaultValue('Snapshot', False), inputImgMatrix, learnEng)
                          else
                              AI.LMetric_ResNet_SaveToLearnEngine_DT(outputstream, param.GetDefaultValue('Snapshot', False), inputImgList, learnEng);
                        end
                      else
                        begin
                          LMetric_hnd := AI.LMetric_ResNet_Open_Stream(outputstream);
                          if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                              AI.LMetric_ResNet_SaveToLearnEngine(LMetric_hnd, param.GetDefaultValue('Snapshot', False), inputImgMatrix, learnEng)
                          else
                              AI.LMetric_ResNet_SaveToLearnEngine(LMetric_hnd, param.GetDefaultValue('Snapshot', False), inputImgList, learnEng);
                          AI.LMetric_ResNet_Close(LMetric_hnd);
                        end;
                      DoStatus('process LMetric to learn-KDTree done.');

                      tmpM64 := TMemoryStream64.Create;
                      learnEng.SaveToStream(tmpM64);
                      output_learn_file := umlChangeFileExt(param.GetDefaultValue('output', 'output' + C_LMetric_Ext), C_Learn_Ext);
                      Task.write(param.GetDefaultValue('output' + C_Learn_Ext, output_learn_file), tmpM64);
                      DisposeObject(tmpM64);
                      DisposeObject(learnEng);
                    end;

                  DisposeObject(outputstream);
                  ResultValues['Loss'] := AI.Last_training_average_loss;
                  ResultValues['Rate'] := AI.Last_training_learning_rate;
                  ResultValues['TargetRate'] := AI.completed_learning_rate;
                  Result := True;
                end;
            except
            end;
          end;
{$ENDREGION 'LMetric'}
      end
    else if umlMultipleMatch(['TrainMMOD', 'TrainingMMOD', 'TrainMaxMarginDNNObjectDetector', 'TrainMMOD6L', 'TrainingMMOD6L', 'TrainMaxMarginDNNObjectDetector6L'], ComputeFunc) then
      begin
{$REGION 'MMOD6L'}
        inputfile1 := param.GetDefaultValue('source', '');

        if Task.Exists(inputfile1) then
          begin
            try
              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                begin
                  Task.Read(inputfile1, inputImgMatrix);
                  inputImgMatrix.scale(param.GetDefaultValue('scale', 1.0));
                  if param.GetDefaultValue('NoLabel', True) = True then
                      inputImgMatrix.RunScript('True', 'SetLabel(' + #39#39 + ')');
                end
              else
                begin
                  Task.Read(inputfile1, inputImgList);
                  inputImgList.scale(param.GetDefaultValue('scale', 1.0));
                  if param.GetDefaultValue('NoLabel', True) = True then
                      inputImgList.RunScript('True', 'SetLabel(' + #39#39 + ')');
                end;

              { init sync file1. }
              local_sync1 := param.GetDefaultValue('syncfile', 'output' + C_Sync_Ext);
              sync_file1 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext);
              umlDeleteFile(sync_file1);
              if Task.Exists(local_sync1) then
                  Task.ReadToFile(local_sync1, sync_file1);
              { init sync file2. }
              local_sync2 := param.GetDefaultValue('syncfile2', 'output' + C_Sync_Ext2);
              sync_file2 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext2);
              umlDeleteFile(sync_file2);
              if Task.Exists(local_sync2) then
                  Task.ReadToFile(local_sync2, sync_file2);

              TCoreClassThread.Sleep(1);

              output_file := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5))) + C_MMOD6L_Ext;
              mmod_param := AI.LargeScale_MMOD6L_DNN_PrepareTrain(sync_file1, output_file);

              mmod_param^.timeout := param.GetDefaultValue('timeout', mmod_param^.timeout);
              mmod_param^.weight_decay := param.GetDefaultValue('weight_decay', mmod_param^.weight_decay);
              mmod_param^.momentum := param.GetDefaultValue('momentum', mmod_param^.momentum);
              mmod_param^.target_size := param.GetDefaultValue('target_size', mmod_param^.target_size);
              mmod_param^.min_target_size := param.GetDefaultValue('min_target_size', mmod_param^.min_target_size);
              mmod_param^.min_detector_window_overlap_iou := param.GetDefaultValue('min_detector_window_overlap_iou', mmod_param^.min_detector_window_overlap_iou);
              mmod_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', mmod_param^.iterations_without_progress_threshold);
              mmod_param^.learning_rate := param.GetDefaultValue('learning_rate', mmod_param^.learning_rate);
              mmod_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', mmod_param^.completed_learning_rate);

              mmod_param^.overlap_NMS_iou_thresh := param.GetDefaultValue('overlap_NMS_iou_thresh', mmod_param^.overlap_NMS_iou_thresh);
              mmod_param^.overlap_NMS_percent_covered_thresh := param.GetDefaultValue('overlap_NMS_percent_covered_thresh', mmod_param^.overlap_NMS_percent_covered_thresh);
              mmod_param^.overlap_ignore_iou_thresh := param.GetDefaultValue('overlap_ignore_iou_thresh', mmod_param^.overlap_ignore_iou_thresh);
              mmod_param^.overlap_ignore_percent_covered_thresh := param.GetDefaultValue('overlap_ignore_percent_covered_thresh', mmod_param^.overlap_ignore_percent_covered_thresh);

              mmod_param^.num_crops := param.GetDefaultValue('num_crops', mmod_param^.num_crops);
              mmod_param^.chip_dims_x := param.GetDefaultValue('chip_dims_x', mmod_param^.chip_dims_x);
              mmod_param^.chip_dims_y := param.GetDefaultValue('chip_dims_y', mmod_param^.chip_dims_y);
              mmod_param^.min_object_size_x := param.GetDefaultValue('min_object_size_x', mmod_param^.min_object_size_x);
              mmod_param^.min_object_size_y := param.GetDefaultValue('min_object_size_y', mmod_param^.min_object_size_y);
              mmod_param^.max_rotation_degrees := param.GetDefaultValue('max_rotation_degrees', mmod_param^.max_rotation_degrees);
              mmod_param^.max_object_size := param.GetDefaultValue('max_object_size', mmod_param^.max_object_size);

              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  outputstream := AI.LargeScale_MMOD6L_DNN_Train_Stream(mmod_param, inputImgMatrix)
              else
                  outputstream := AI.LargeScale_MMOD6L_DNN_Train_Stream(mmod_param, inputImgList);

              AI.LargeScale_MMOD6L_DNN_FreeTrain(mmod_param);

              { write sync1 to task }
              if umlFileExists(sync_file1) then
                  Task.WriteFile(local_sync1, sync_file1);
              { write sync2 to task }
              if umlFileExists(sync_file2) then
                  Task.WriteFile(local_sync2, sync_file2);

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_MMOD6L_Ext), outputstream);
                  DisposeObject(outputstream);
                  ResultValues['Loss'] := AI.Last_training_average_loss;
                  ResultValues['Rate'] := AI.Last_training_learning_rate;
                  ResultValues['TargetRate'] := AI.completed_learning_rate;
                  Result := True;
                end;
            except
            end;
          end;
{$ENDREGION 'MMOD6L'}
      end
    else if umlMultipleMatch(['TrainMMOD3L', 'TrainingMMOD3L', 'TrainMaxMarginDNNObjectDetector3L'], ComputeFunc) then
      begin
{$REGION 'MMOD3L'}
        inputfile1 := param.GetDefaultValue('source', '');

        if Task.Exists(inputfile1) then
          begin
            try
              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                begin
                  Task.Read(inputfile1, inputImgMatrix);
                  inputImgMatrix.scale(param.GetDefaultValue('scale', 1.0));
                  if param.GetDefaultValue('NoLabel', True) = True then
                      inputImgMatrix.RunScript('True', 'SetLabel(' + #39#39 + ')');
                end
              else
                begin
                  Task.Read(inputfile1, inputImgList);
                  inputImgList.scale(param.GetDefaultValue('scale', 1.0));
                  if param.GetDefaultValue('NoLabel', True) = True then
                      inputImgList.RunScript('True', 'SetLabel(' + #39#39 + ')');
                end;

              { init sync file1. }
              local_sync1 := param.GetDefaultValue('syncfile', 'output' + C_Sync_Ext);
              sync_file1 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext);
              umlDeleteFile(sync_file1);
              if Task.Exists(local_sync1) then
                  Task.ReadToFile(local_sync1, sync_file1);
              { init sync file2. }
              local_sync2 := param.GetDefaultValue('syncfile2', 'output' + C_Sync_Ext2);
              sync_file2 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext2);
              umlDeleteFile(sync_file2);
              if Task.Exists(local_sync2) then
                  Task.ReadToFile(local_sync2, sync_file2);

              TCoreClassThread.Sleep(1);

              output_file := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5))) + C_MMOD3L_Ext;
              mmod_param := AI.LargeScale_MMOD3L_DNN_PrepareTrain(sync_file1, output_file);

              mmod_param^.timeout := param.GetDefaultValue('timeout', mmod_param^.timeout);
              mmod_param^.weight_decay := param.GetDefaultValue('weight_decay', mmod_param^.weight_decay);
              mmod_param^.momentum := param.GetDefaultValue('momentum', mmod_param^.momentum);
              mmod_param^.target_size := param.GetDefaultValue('target_size', mmod_param^.target_size);
              mmod_param^.min_target_size := param.GetDefaultValue('min_target_size', mmod_param^.min_target_size);
              mmod_param^.min_detector_window_overlap_iou := param.GetDefaultValue('min_detector_window_overlap_iou', mmod_param^.min_detector_window_overlap_iou);
              mmod_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', mmod_param^.iterations_without_progress_threshold);
              mmod_param^.learning_rate := param.GetDefaultValue('learning_rate', mmod_param^.learning_rate);
              mmod_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', mmod_param^.completed_learning_rate);

              mmod_param^.overlap_NMS_iou_thresh := param.GetDefaultValue('overlap_NMS_iou_thresh', mmod_param^.overlap_NMS_iou_thresh);
              mmod_param^.overlap_NMS_percent_covered_thresh := param.GetDefaultValue('overlap_NMS_percent_covered_thresh', mmod_param^.overlap_NMS_percent_covered_thresh);
              mmod_param^.overlap_ignore_iou_thresh := param.GetDefaultValue('overlap_ignore_iou_thresh', mmod_param^.overlap_ignore_iou_thresh);
              mmod_param^.overlap_ignore_percent_covered_thresh := param.GetDefaultValue('overlap_ignore_percent_covered_thresh', mmod_param^.overlap_ignore_percent_covered_thresh);

              mmod_param^.num_crops := param.GetDefaultValue('num_crops', mmod_param^.num_crops);
              mmod_param^.chip_dims_x := param.GetDefaultValue('chip_dims_x', mmod_param^.chip_dims_x);
              mmod_param^.chip_dims_y := param.GetDefaultValue('chip_dims_y', mmod_param^.chip_dims_y);
              mmod_param^.min_object_size_x := param.GetDefaultValue('min_object_size_x', mmod_param^.min_object_size_x);
              mmod_param^.min_object_size_y := param.GetDefaultValue('min_object_size_y', mmod_param^.min_object_size_y);
              mmod_param^.max_rotation_degrees := param.GetDefaultValue('max_rotation_degrees', mmod_param^.max_rotation_degrees);
              mmod_param^.max_object_size := param.GetDefaultValue('max_object_size', mmod_param^.max_object_size);

              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  outputstream := AI.LargeScale_MMOD3L_DNN_Train_Stream(mmod_param, inputImgMatrix)
              else
                  outputstream := AI.LargeScale_MMOD3L_DNN_Train_Stream(mmod_param, inputImgList);

              AI.LargeScale_MMOD3L_DNN_FreeTrain(mmod_param);

              { write sync1 to task }
              if umlFileExists(sync_file1) then
                  Task.WriteFile(local_sync1, sync_file1);
              { write sync2 to task }
              if umlFileExists(sync_file2) then
                  Task.WriteFile(local_sync2, sync_file2);

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_MMOD3L_Ext), outputstream);
                  DisposeObject(outputstream);
                  ResultValues['Loss'] := AI.Last_training_average_loss;
                  ResultValues['Rate'] := AI.Last_training_learning_rate;
                  ResultValues['TargetRate'] := AI.completed_learning_rate;
                  Result := True;
                end;
            except
            end;
          end;
{$ENDREGION 'MMOD3L'}
      end
    else if umlMultipleMatch(['TrainRNIC', 'TrainingRNIC', 'TrainResNetImageClassifier'], ComputeFunc) then
      begin
{$REGION 'RNIC'}
        inputfile1 := param.GetDefaultValue('source', '');

        if Task.Exists(inputfile1) then
          begin
            outputPacalStringList := TPascalStringList.Create;
            try
              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                begin
                  Task.Read(inputfile1, inputImgMatrix);
                  inputImgMatrix.scale(param.GetDefaultValue('scale', 1.0));
                end
              else
                begin
                  Task.Read(inputfile1, inputImgList);
                  inputImgList.scale(param.GetDefaultValue('scale', 1.0));
                end;

              { init sync file1. }
              local_sync1 := param.GetDefaultValue('syncfile', 'output' + C_Sync_Ext);
              sync_file1 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext);
              umlDeleteFile(sync_file1);
              if Task.Exists(local_sync1) then
                  Task.ReadToFile(local_sync1, sync_file1);
              { init sync file2. }
              local_sync2 := param.GetDefaultValue('syncfile2', 'output' + C_Sync_Ext2);
              sync_file2 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext2);
              umlDeleteFile(sync_file2);
              if Task.Exists(local_sync2) then
                  Task.ReadToFile(local_sync2, sync_file2);

              output_file := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5))) + C_RNIC_Ext;

              rnic_param := TAI.Init_RNIC_Train_Parameter(sync_file1, output_file);

              rnic_param^.timeout := param.GetDefaultValue('timeout', rnic_param^.timeout);
              rnic_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', rnic_param^.iterations_without_progress_threshold);
              rnic_param^.learning_rate := param.GetDefaultValue('learning_rate', rnic_param^.learning_rate);
              rnic_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', rnic_param^.completed_learning_rate);
              rnic_param^.all_bn_running_stats_window_sizes := param.GetDefaultValue('all_bn_running_stats_window_sizes', rnic_param^.all_bn_running_stats_window_sizes);
              rnic_param^.img_mini_batch := param.GetDefaultValue('img_mini_batch', rnic_param^.img_mini_batch);

              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  outputstream := AI.RNIC_Train_Stream(
                  inputImgMatrix,
                  rnic_param,
                  outputPacalStringList
                  )
              else
                  outputstream := AI.RNIC_Train_Stream(
                  inputImgList,
                  rnic_param,
                  outputPacalStringList
                  );

              TAI.Free_RNIC_Train_Parameter(rnic_param);

              { write sync1 to task }
              if umlFileExists(sync_file1) then
                  Task.WriteFile(local_sync1, sync_file1);
              { write sync2 to task }
              if umlFileExists(sync_file2) then
                  Task.WriteFile(local_sync2, sync_file2);

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_RNIC_Ext), outputstream);
                  Task.write(param.GetDefaultValue('output.index', 'output' + C_RNIC_Ext + '.index'), outputPacalStringList);
                  DisposeObject(outputstream);
                  ResultValues['Loss'] := AI.Last_training_average_loss;
                  ResultValues['Rate'] := AI.Last_training_learning_rate;
                  ResultValues['TargetRate'] := AI.completed_learning_rate;
                  Result := True;
                end;
            except
            end;
            DisposeObject(outputPacalStringList);
          end;
{$ENDREGION 'RNIC'}
      end
    else if umlMultipleMatch(['TrainLRNIC', 'TrainingLRNIC', 'TrainLResNetImageClassifier'], ComputeFunc) then
      begin
{$REGION 'LRNIC'}
        inputfile1 := param.GetDefaultValue('source', '');

        if Task.Exists(inputfile1) then
          begin
            outputPacalStringList := TPascalStringList.Create;
            try
              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                begin
                  Task.Read(inputfile1, inputImgMatrix);
                  inputImgMatrix.scale(param.GetDefaultValue('scale', 1.0));
                end
              else
                begin
                  Task.Read(inputfile1, inputImgList);
                  inputImgList.scale(param.GetDefaultValue('scale', 1.0));
                end;

              { init sync file1. }
              local_sync1 := param.GetDefaultValue('syncfile', 'output' + C_Sync_Ext);
              sync_file1 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext);
              umlDeleteFile(sync_file1);
              if Task.Exists(local_sync1) then
                  Task.ReadToFile(local_sync1, sync_file1);
              { init sync file2. }
              local_sync2 := param.GetDefaultValue('syncfile2', 'output' + C_Sync_Ext2);
              sync_file2 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext2);
              umlDeleteFile(sync_file2);
              if Task.Exists(local_sync2) then
                  Task.ReadToFile(local_sync2, sync_file2);

              output_file := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5))) + C_LRNIC_Ext;

              rnic_param := TAI.Init_LRNIC_Train_Parameter(sync_file1, output_file);

              rnic_param^.timeout := param.GetDefaultValue('timeout', rnic_param^.timeout);
              rnic_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', rnic_param^.iterations_without_progress_threshold);
              rnic_param^.learning_rate := param.GetDefaultValue('learning_rate', rnic_param^.learning_rate);
              rnic_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', rnic_param^.completed_learning_rate);
              rnic_param^.all_bn_running_stats_window_sizes := param.GetDefaultValue('all_bn_running_stats_window_sizes', rnic_param^.all_bn_running_stats_window_sizes);
              rnic_param^.img_mini_batch := param.GetDefaultValue('img_mini_batch', rnic_param^.img_mini_batch);

              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  outputstream := AI.LRNIC_Train_Stream(
                  inputImgMatrix,
                  rnic_param,
                  outputPacalStringList
                  )
              else
                  outputstream := AI.LRNIC_Train_Stream(
                  inputImgList,
                  rnic_param,
                  outputPacalStringList
                  );

              TAI.Free_LRNIC_Train_Parameter(rnic_param);

              { write sync1 to task }
              if umlFileExists(sync_file1) then
                  Task.WriteFile(local_sync1, sync_file1);
              { write sync2 to task }
              if umlFileExists(sync_file2) then
                  Task.WriteFile(local_sync2, sync_file2);

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_LRNIC_Ext), outputstream);
                  Task.write(param.GetDefaultValue('output.index', 'output' + C_LRNIC_Ext + '.index'), outputPacalStringList);
                  DisposeObject(outputstream);
                  ResultValues['Loss'] := AI.Last_training_average_loss;
                  ResultValues['Rate'] := AI.Last_training_learning_rate;
                  ResultValues['TargetRate'] := AI.completed_learning_rate;
                  Result := True;
                end;
            except
            end;
            DisposeObject(outputPacalStringList);
          end;
{$ENDREGION 'LRNIC'}
      end
    else if umlMultipleMatch(['TrainGDCNIC', 'TrainingGDCNIC'], ComputeFunc) then
      begin
{$REGION 'GDCNIC'}
        inputfile1 := param.GetDefaultValue('source', '');

        if Task.Exists(inputfile1) then
          begin
            outputPacalStringList := TPascalStringList.Create;
            try
              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                begin
                  Task.Read(inputfile1, inputImgMatrix);
                  inputImgMatrix.scale(param.GetDefaultValue('scale', 1.0));
                end
              else
                begin
                  Task.Read(inputfile1, inputImgList);
                  inputImgList.scale(param.GetDefaultValue('scale', 1.0));
                end;

              { init sync file1. }
              local_sync1 := param.GetDefaultValue('syncfile', 'output' + C_Sync_Ext);
              sync_file1 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext);
              umlDeleteFile(sync_file1);
              if Task.Exists(local_sync1) then
                  Task.ReadToFile(local_sync1, sync_file1);
              { init sync file2. }
              local_sync2 := param.GetDefaultValue('syncfile2', 'output' + C_Sync_Ext2);
              sync_file2 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext2);
              umlDeleteFile(sync_file2);
              if Task.Exists(local_sync2) then
                  Task.ReadToFile(local_sync2, sync_file2);

              output_file := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5))) + C_GDCNIC_Ext;

              GDCNIC_param := TAI.Init_GDCNIC_Train_Parameter(sync_file1, output_file);

              GDCNIC_param^.timeout := param.GetDefaultValue('timeout', GDCNIC_param^.timeout);
              GDCNIC_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', GDCNIC_param^.iterations_without_progress_threshold);
              GDCNIC_param^.learning_rate := param.GetDefaultValue('learning_rate', GDCNIC_param^.learning_rate);
              GDCNIC_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', GDCNIC_param^.completed_learning_rate);
              GDCNIC_param^.img_mini_batch := param.GetDefaultValue('img_mini_batch', GDCNIC_param^.img_mini_batch);

              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  outputstream := AI.GDCNIC_Train_Stream(param.GetDefaultValue('Snapshot', True),
                  param.GetDefaultValue('SS_Width', 32),
                  param.GetDefaultValue('SS_Height', 32),
                  inputImgMatrix,
                  GDCNIC_param,
                  outputPacalStringList
                  )
              else
                  outputstream := AI.GDCNIC_Train_Stream(param.GetDefaultValue('Snapshot', True),
                  param.GetDefaultValue('SS_Width', 32),
                  param.GetDefaultValue('SS_Height', 32),
                  inputImgList,
                  GDCNIC_param,
                  outputPacalStringList
                  );

              TAI.Free_GDCNIC_Train_Parameter(GDCNIC_param);

              { write sync1 to task }
              if umlFileExists(sync_file1) then
                  Task.WriteFile(local_sync1, sync_file1);
              { write sync2 to task }
              if umlFileExists(sync_file2) then
                  Task.WriteFile(local_sync2, sync_file2);

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_GDCNIC_Ext), outputstream);
                  Task.write(param.GetDefaultValue('output.index', 'output' + C_GDCNIC_Ext + '.index'), outputPacalStringList);
                  DisposeObject(outputstream);
                  ResultValues['Loss'] := AI.Last_training_average_loss;
                  ResultValues['Rate'] := AI.Last_training_learning_rate;
                  ResultValues['TargetRate'] := AI.completed_learning_rate;
                  Result := True;
                end;
            except
            end;
            DisposeObject(outputPacalStringList);
          end;
{$ENDREGION 'GDCNIC'}
      end
    else if umlMultipleMatch(['TrainGNIC', 'TrainingGNIC'], ComputeFunc) then
      begin
{$REGION 'GNIC'}
        inputfile1 := param.GetDefaultValue('source', '');

        if Task.Exists(inputfile1) then
          begin
            outputPacalStringList := TPascalStringList.Create;
            try
              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                begin
                  Task.Read(inputfile1, inputImgMatrix);
                  inputImgMatrix.scale(param.GetDefaultValue('scale', 1.0));
                end
              else
                begin
                  Task.Read(inputfile1, inputImgList);
                  inputImgList.scale(param.GetDefaultValue('scale', 1.0));
                end;

              { init sync file1. }
              local_sync1 := param.GetDefaultValue('syncfile', 'output' + C_Sync_Ext);
              sync_file1 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext);
              umlDeleteFile(sync_file1);
              if Task.Exists(local_sync1) then
                  Task.ReadToFile(local_sync1, sync_file1);
              { init sync file2. }
              local_sync2 := param.GetDefaultValue('syncfile2', 'output' + C_Sync_Ext2);
              sync_file2 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext2);
              umlDeleteFile(sync_file2);
              if Task.Exists(local_sync2) then
                  Task.ReadToFile(local_sync2, sync_file2);

              output_file := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5))) + C_GNIC_Ext;

              GNIC_param := TAI.Init_GNIC_Train_Parameter(sync_file1, output_file);

              GNIC_param^.timeout := param.GetDefaultValue('timeout', GNIC_param^.timeout);
              GNIC_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', GNIC_param^.iterations_without_progress_threshold);
              GNIC_param^.learning_rate := param.GetDefaultValue('learning_rate', GNIC_param^.learning_rate);
              GNIC_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', GNIC_param^.completed_learning_rate);
              GNIC_param^.img_mini_batch := param.GetDefaultValue('img_mini_batch', GNIC_param^.img_mini_batch);

              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  outputstream := AI.GNIC_Train_Stream(param.GetDefaultValue('Snapshot', True),
                  param.GetDefaultValue('SS_Width', 32),
                  param.GetDefaultValue('SS_Height', 32),
                  inputImgMatrix,
                  GNIC_param,
                  outputPacalStringList
                  )
              else
                  outputstream := AI.GNIC_Train_Stream(param.GetDefaultValue('Snapshot', True),
                  param.GetDefaultValue('SS_Width', 32),
                  param.GetDefaultValue('SS_Height', 32),
                  inputImgList,
                  GNIC_param,
                  outputPacalStringList
                  );

              TAI.Free_GNIC_Train_Parameter(GNIC_param);

              { write sync1 to task }
              if umlFileExists(sync_file1) then
                  Task.WriteFile(local_sync1, sync_file1);
              { write sync2 to task }
              if umlFileExists(sync_file2) then
                  Task.WriteFile(local_sync2, sync_file2);

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_GNIC_Ext), outputstream);
                  Task.write(param.GetDefaultValue('output.index', 'output' + C_GNIC_Ext + '.index'), outputPacalStringList);
                  DisposeObject(outputstream);
                  ResultValues['Loss'] := AI.Last_training_average_loss;
                  ResultValues['Rate'] := AI.Last_training_learning_rate;
                  ResultValues['TargetRate'] := AI.completed_learning_rate;
                  Result := True;
                end;
            except
            end;
            DisposeObject(outputPacalStringList);
          end;
{$ENDREGION 'GNIC'}
      end
    else if umlMultipleMatch(['TrainSS', 'TrainingSS'], ComputeFunc) then
      begin
{$REGION 'SS'}
        inputfile1 := param.GetDefaultValue('source', '');

        if Task.Exists(inputfile1) then
          begin
            outputPacalStringList := TPascalStringList.Create;
            try
              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                begin
                  Task.Read(inputfile1, inputImgMatrix);
                  inputImgMatrix.scale(param.GetDefaultValue('scale', 1.0));
                  ss_colorPool := inputImgMatrix.BuildSegmentationColorBuffer;
                end
              else
                begin
                  Task.Read(inputfile1, inputImgList);
                  inputImgList.scale(param.GetDefaultValue('scale', 1.0));
                  ss_colorPool := inputImgList.BuildSegmentationColorBuffer;
                end;

              { init sync file1. }
              local_sync1 := param.GetDefaultValue('syncfile', 'output' + C_Sync_Ext);
              sync_file1 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext);
              umlDeleteFile(sync_file1);
              if Task.Exists(local_sync1) then
                  Task.ReadToFile(local_sync1, sync_file1);
              { init sync file2. }
              local_sync2 := param.GetDefaultValue('syncfile2', 'output' + C_Sync_Ext2);
              sync_file2 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext2);
              umlDeleteFile(sync_file2);
              if Task.Exists(local_sync2) then
                  Task.ReadToFile(local_sync2, sync_file2);

              output_file := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5))) + C_SS_Ext;

              SS_param := TAI.Init_SS_Train_Parameter(sync_file1, output_file);

              SS_param^.timeout := param.GetDefaultValue('timeout', SS_param^.timeout);
              SS_param^.weight_decay := param.GetDefaultValue('weight_decay', SS_param^.weight_decay);
              SS_param^.momentum := param.GetDefaultValue('momentum', SS_param^.momentum);
              SS_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', SS_param^.iterations_without_progress_threshold);
              SS_param^.learning_rate := param.GetDefaultValue('learning_rate', SS_param^.learning_rate);
              SS_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', SS_param^.completed_learning_rate);
              SS_param^.img_crops_batch := param.GetDefaultValue('img_crops_batch', SS_param^.img_crops_batch);

              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  outputstream := AI.SS_Train_Stream(
                  inputImgMatrix,
                  SS_param,
                  ss_colorPool
                  )
              else
                  outputstream := AI.SS_Train_Stream(
                  inputImgList,
                  SS_param,
                  ss_colorPool
                  );

              TAI.Free_SS_Train_Parameter(SS_param);

              { write sync1 to task }
              if umlFileExists(sync_file1) then
                  Task.WriteFile(local_sync1, sync_file1);
              { write sync2 to task }
              if umlFileExists(sync_file2) then
                  Task.WriteFile(local_sync2, sync_file2);

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_SS_Ext), outputstream);
                  Task.write(param.GetDefaultValue('output.colorPool', 'output' + C_SS_Ext + '.colorPool'), ss_colorPool);
                  DisposeObject(outputstream);
                  ResultValues['Loss'] := AI.Last_training_average_loss;
                  ResultValues['Rate'] := AI.Last_training_learning_rate;
                  ResultValues['TargetRate'] := AI.completed_learning_rate;
                  Result := True;
                end;
            except
            end;
            DisposeObject(outputPacalStringList);
          end;
{$ENDREGION 'SS'}
      end
    else if umlMultipleMatch(['TrainZMetric', 'TrainingZMetric'], ComputeFunc) then
      begin
{$REGION 'ZMetric'}
        inputfile1 := param.GetDefaultValue('source', '');

        if Task.Exists(inputfile1) then
          begin
            try
              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  Task.Read(inputfile1, inputImgMatrix)
              else
                  Task.Read(inputfile1, inputImgList);

              { init sync file1. }
              local_sync1 := param.GetDefaultValue('syncfile', 'output' + C_Sync_Ext);
              sync_file1 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext);
              umlDeleteFile(sync_file1);
              if Task.Exists(local_sync1) then
                  Task.ReadToFile(local_sync1, sync_file1);
              { init sync file2. }
              local_sync2 := param.GetDefaultValue('syncfile2', 'output' + C_Sync_Ext2);
              sync_file2 := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Sync_Ext2);
              umlDeleteFile(sync_file2);
              if Task.Exists(local_sync2) then
                  Task.ReadToFile(local_sync2, sync_file2);

              output_file := umlCombineFileName(AI.RootPath, umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5))) + C_ZMetric_Ext;

              zmetric_param := TAI.Init_ZMetric_Parameter(sync_file1, output_file);

              zmetric_param^.timeout := param.GetDefaultValue('timeout', zmetric_param^.timeout);

              zmetric_param^.weight_decay := param.GetDefaultValue('weight_decay', zmetric_param^.weight_decay);
              zmetric_param^.momentum := param.GetDefaultValue('momentum', zmetric_param^.momentum);
              zmetric_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', zmetric_param^.iterations_without_progress_threshold);
              zmetric_param^.learning_rate := param.GetDefaultValue('learning_rate', zmetric_param^.learning_rate);
              zmetric_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', zmetric_param^.completed_learning_rate);
              zmetric_param^.step_mini_batch_target_num := param.GetDefaultValue('step_mini_batch_target_num', zmetric_param^.step_mini_batch_target_num);
              zmetric_param^.step_mini_batch_raster_num := param.GetDefaultValue('step_mini_batch_raster_num', zmetric_param^.step_mini_batch_raster_num);

              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  outputstream := AI.ZMetric_Train_Stream(
                  param.GetDefaultValue('Snapshot', False),
                  inputImgMatrix,
                  param.GetDefaultValue('SS_Width', 150),
                  param.GetDefaultValue('SS_Height', 150),
                  zmetric_param)
              else
                  outputstream := AI.ZMetric_Train_Stream(
                  param.GetDefaultValue('Snapshot', False),
                  inputImgList,
                  param.GetDefaultValue('SS_Width', 150),
                  param.GetDefaultValue('SS_Height', 150),
                  zmetric_param);

              TAI.Free_ZMetric_Parameter(zmetric_param);

              { write sync1 to task }
              if umlFileExists(sync_file1) then
                  Task.WriteFile(local_sync1, sync_file1);
              { write sync2 to task }
              if umlFileExists(sync_file2) then
                  Task.WriteFile(local_sync2, sync_file2);

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_ZMetric_Ext), outputstream);

                  if (param.GetDefaultValue('LearnVec', False) = True) and (AI.completed_learning_rate > AI.Last_training_learning_rate) then
                    begin
                      learnEng := TAI.Build_ZMetric_Learn;
                      outputstream.Position := 0;

                      DoStatus('build Z-Metric to learn-KDTree.');

                      if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                          AI.ZMetric_SaveToLearnEngine_DT(
                          param.GetDefaultValue('LearnThreadNum', 2),
                          outputstream,
                          param.GetDefaultValue('Snapshot', False),
                          inputImgMatrix,
                          param.GetDefaultValue('SS_Width', 150),
                          param.GetDefaultValue('SS_Height', 150),
                          learnEng)
                      else
                          AI.ZMetric_SaveToLearnEngine_DT(
                          param.GetDefaultValue('LearnThreadNum', 2),
                          outputstream,
                          param.GetDefaultValue('Snapshot', False),
                          inputImgList,
                          param.GetDefaultValue('SS_Width', 150),
                          param.GetDefaultValue('SS_Height', 150),
                          learnEng);

                      DoStatus('process Z-Metric to learn-KDTree done.');

                      tmpM64 := TMemoryStream64.Create;
                      learnEng.SaveToStream(tmpM64);
                      output_learn_file := umlChangeFileExt(param.GetDefaultValue('output', 'output' + C_ZMetric_Ext), C_Learn_Ext);
                      Task.write(param.GetDefaultValue('output' + C_Learn_Ext, output_learn_file), tmpM64);
                      DisposeObject(tmpM64);
                      DisposeObject(learnEng);
                    end;

                  DisposeObject(outputstream);
                  ResultValues['Loss'] := AI.Last_training_average_loss;
                  ResultValues['Rate'] := AI.Last_training_learning_rate;
                  ResultValues['TargetRate'] := AI.completed_learning_rate;
                  Result := True;
                end;
            except
            end;
          end;
{$ENDREGION 'ZMetric'}
      end
    else
      begin
        DoStatus('AI Training task failed: no define ComputeFunc.');
      end;
  finally
    ResultValues['Result'] := Result;
    ResultValues['End'] := umlNow();
    DoStatus('usage time: %s', [umlTimeTickToStr(GetTimeTick() - startTick).Text]);

    Task.write(param.GetDefaultValue('result', 'result.txt'), ResultValues);
    Task.write(param.GetDefaultValue('log', 'log.txt'), Task.TaskLogStatus);

    if AI.FAI_EntryAPI^.Log.Count > 0 then
      begin
        tmpPSL := TPascalStringList.Create;
        for i := 0 to AI.FAI_EntryAPI^.Log.Count - 1 do
            tmpPSL.Add(AI.FAI_EntryAPI^.Log[i].LogText);
        Task.write(param.GetDefaultValue('engine_log', 'engine_log.txt'), tmpPSL);
        DisposeObject(tmpPSL);
      end;

    if AI.FAI_EntryAPI^.OneStepList.Count > 0 then
      begin
        tmpM64 := TMemoryStream64.Create;
        AI.FAI_EntryAPI^.OneStepList.SaveToStream(tmpM64);
        Task.write(param.GetDefaultValue('training_steps', 'training_steps.dat'), tmpM64);
        DisposeObject(tmpM64);

        tmpM64 := TMemoryStream64.Create;
        AI.FAI_EntryAPI^.OneStepList.ExportToExcelStream(tmpM64);
        Task.write(param.GetDefaultValue('training_steps_excel', 'training_steps_excel.csv'), tmpM64);
        DisposeObject(tmpM64);
      end;

    if Result then
      begin
        if Task.LastWriteFileList.ExistsValue(paramFile) < 0 then
            Task.LastWriteFileList.Add(paramFile);
        Task.write(param.GetDefaultValue('LastOutput', 'LastOutput.txt'), Task.LastWriteFileList);
      end;
  end;

  DisposeObject(param);
  DisposeObject([inputstream1, inputstream2]);
  DisposeObject([inputraster1, inputraster2]);
  DisposeObject(inputImgList);
  DisposeObject(inputImgMatrix);
  DisposeObject(ResultValues);
end;

function RunLargeScaleTrainingTask(
  ImgMatDatasetFile, RasterSerializedFile, Training_RasterSerializedFile, SyncFile, LogFile, StepFile, OutputModel: U_String;
  AI: TAI;
  param: THashVariantList): Boolean;
var
  ComputeFunc: SystemString;
  ImgMatrix: TAI_ImageMatrix;

  { Image Matrix Serialized }
  RSeriStream: TCoreClassFileStream;
  RSeri: TRasterSerialized;

  { AI Engine Serialized }
  Training_RSeriStream: TCoreClassFileStream;
  Training_RSeri: TRasterSerialized;

  { log }
  LogData: TPascalStringList;

  { step }
  StepData: TOneStepList;

  { temp stream }
  m64: TMemoryStream64;
  i: Integer;

  { ai build-in param }
  metric_resnet_param: PMetric_ResNet_Train_Parameter;
  LMetric_resnet_param: PMetric_ResNet_Train_Parameter;
  mmod_param: PMMOD_Train_Parameter;
  rnic_param: PRNIC_Train_Parameter;
  GDCNIC_param: PGDCNIC_Train_Parameter;
  GNIC_param: PGNIC_Train_Parameter;
  ss_colorPool: TSegmentationColorTable;
  SS_param: PSS_Train_Parameter;
  zmetric_param: PZMetric_Train_Parameter;

  { data support }
  output_learn_file: SystemString;
  learnEng: TLearn;
  kd: TKDTree;
  KD_Data: TKDTreeDataList;
  Metric_hnd: TMetric_Handle;
  LMetric_hnd: TLMetric_Handle;
begin
  Result := False;
  if not AI.Activted then
    begin
      DoStatus('AI engine error.');
      exit;
    end;
  if not umlFileExists(ImgMatDatasetFile) then
    begin
      DoStatus('no exists %s', [ImgMatDatasetFile.Text]);
      exit;
    end;

  DoStatus('init Serialized temp file: %s', [RasterSerializedFile.Text]);
  RSeriStream := TCoreClassFileStream.Create(RasterSerializedFile, fmCreate);
  RSeri := TRasterSerialized.Create(RSeriStream);

  DoStatus('init training serialized temp file: %s', [Training_RasterSerializedFile.Text]);
  Training_RSeriStream := TCoreClassFileStream.Create(Training_RasterSerializedFile, fmCreate);
  Training_RSeri := TRasterSerialized.Create(Training_RSeriStream);

  DoStatus('init log file: %s', [LogFile.Text]);
  LogData := TPascalStringList.Create;

  if umlFileExists(LogFile) then
    begin
      LogData.LoadFromFile(LogFile);
      DoStatus('undo log file state: %s', [LogFile.Text]);
    end;

  StepData := TOneStepList.Create;
  DoStatus('init step file: %s', [StepFile.Text]);
  if umlFileExists(StepFile) then
    begin
      m64 := TMemoryStream64.Create;
      m64.LoadFromFile(StepFile);
      m64.Position := 0;
      StepData.LoadFromStream(m64);
      DisposeObject(m64);
      DoStatus('undo step file state: %s', [StepFile.Text]);
    end;
  ImgMatrix := TAI_ImageMatrix.Create;
  ImgMatrix.LargeScale_LoadFromFile(RSeri, ImgMatDatasetFile);

  if param.Exists('func') then
      ComputeFunc := param['func']
  else if param.Exists('compute') then
      ComputeFunc := param['compute']
  else
      ComputeFunc := param.GetDefaultValue('ComputeFunc', '');

  DoStatus('run large-scale training.');
  if umlMultipleMatch(['TrainMRN', 'TrainingMRN', 'TrainMetricResNet'], ComputeFunc) then
    begin
      metric_resnet_param := TAI.Init_Metric_ResNet_Parameter(SyncFile, OutputModel);
      metric_resnet_param^.timeout := param.GetDefaultValue('timeout', metric_resnet_param^.timeout);
      metric_resnet_param^.weight_decay := param.GetDefaultValue('weight_decay', metric_resnet_param^.weight_decay);
      metric_resnet_param^.momentum := param.GetDefaultValue('momentum', metric_resnet_param^.momentum);
      metric_resnet_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', metric_resnet_param^.iterations_without_progress_threshold);
      metric_resnet_param^.learning_rate := param.GetDefaultValue('learning_rate', metric_resnet_param^.learning_rate);
      metric_resnet_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', metric_resnet_param^.completed_learning_rate);
      metric_resnet_param^.step_mini_batch_target_num := param.GetDefaultValue('step_mini_batch_target_num', metric_resnet_param^.step_mini_batch_target_num);
      metric_resnet_param^.step_mini_batch_raster_num := param.GetDefaultValue('step_mini_batch_raster_num', metric_resnet_param^.step_mini_batch_raster_num);
      metric_resnet_param^.fullGPU_Training := param.GetDefaultValue('fullGPU_Training', metric_resnet_param^.fullGPU_Training);
      Result := AI.Metric_ResNet_Train(param.GetDefaultValue('Snapshot', False), True, Training_RSeri, ImgMatrix, metric_resnet_param);
      TAI.Free_Metric_ResNet_Parameter(metric_resnet_param);
      if (Result) and (param.GetDefaultValue('LearnVec', False) = True) and (AI.completed_learning_rate > AI.Last_training_learning_rate) then
        begin
          DoStatus('build metric to learn-KDTree.');
          learnEng := TLearn.CreateClassifier(ltKDT, zAI.C_Metric_Dim);
          if param.GetDefaultValue('DNNThread', False) = True then
            begin
              m64 := TMemoryStream64.Create;
              m64.LoadFromFile(OutputModel);
              AI.Metric_ResNet_SaveToLearnEngine_DT(m64, param.GetDefaultValue('Snapshot', False), RSeri, ImgMatrix, learnEng);
              DisposeObject(m64);
            end
          else
            begin
              Metric_hnd := AI.Metric_ResNet_Open(OutputModel);
              AI.Metric_ResNet_SaveToLearnEngine(Metric_hnd, param.GetDefaultValue('Snapshot', False), RSeri, ImgMatrix, learnEng);
              AI.Metric_ResNet_Close(Metric_hnd);
            end;
          learnEng.SaveToFile(umlChangeFileExt(OutputModel, C_Learn_Ext));
          DisposeObject(learnEng);
          DoStatus('process metric to learn-KDTree done.');
        end;
      if (Result) and (param.GetDefaultValue('KDTreeVec', False) = True) and (AI.completed_learning_rate > AI.Last_training_learning_rate) then
        begin
          Metric_hnd := AI.Metric_ResNet_Open(OutputModel);
          DoStatus('build metric to KDTree.');
          KD_Data := TKDTreeDataList.Create;
          AI.Metric_ResNet_SaveToKDTree(Metric_hnd, param.GetDefaultValue('Snapshot', False), RSeri, ImgMatrix, KD_Data);
          AI.Metric_ResNet_Close(Metric_hnd);
          DoStatus('process metric to KDTree done.');
          kd := TKDTree.Create(zAI.C_Metric_Dim);
          KD_Data.Build(kd);
          DisposeObject(KD_Data);
          kd.SaveToFile(umlChangeFileExt(OutputModel, C_KDtree_Ext));
          DisposeObject(kd);
        end;
    end
  else if umlMultipleMatch(['TrainLMRN', 'TrainingLMRN', 'TrainLMetricResNet'], ComputeFunc) then
    begin
      LMetric_resnet_param := TAI.Init_LMetric_ResNet_Parameter(SyncFile, OutputModel);
      LMetric_resnet_param^.timeout := param.GetDefaultValue('timeout', LMetric_resnet_param^.timeout);
      LMetric_resnet_param^.weight_decay := param.GetDefaultValue('weight_decay', LMetric_resnet_param^.weight_decay);
      LMetric_resnet_param^.momentum := param.GetDefaultValue('momentum', LMetric_resnet_param^.momentum);
      LMetric_resnet_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', LMetric_resnet_param^.iterations_without_progress_threshold);
      LMetric_resnet_param^.learning_rate := param.GetDefaultValue('learning_rate', LMetric_resnet_param^.learning_rate);
      LMetric_resnet_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', LMetric_resnet_param^.completed_learning_rate);
      LMetric_resnet_param^.step_mini_batch_target_num := param.GetDefaultValue('step_mini_batch_target_num', LMetric_resnet_param^.step_mini_batch_target_num);
      LMetric_resnet_param^.step_mini_batch_raster_num := param.GetDefaultValue('step_mini_batch_raster_num', LMetric_resnet_param^.step_mini_batch_raster_num);
      LMetric_resnet_param^.fullGPU_Training := param.GetDefaultValue('fullGPU_Training', LMetric_resnet_param^.fullGPU_Training);
      Result := AI.LMetric_ResNet_Train(param.GetDefaultValue('Snapshot', False), True, Training_RSeri, ImgMatrix, LMetric_resnet_param);
      TAI.Free_LMetric_ResNet_Parameter(LMetric_resnet_param);
      if (Result) and (param.GetDefaultValue('LearnVec', False) = True) and (AI.completed_learning_rate > AI.Last_training_learning_rate) then
        begin
          learnEng := TLearn.CreateClassifier(ltKDT, zAI.C_LMetric_Dim);
          DoStatus('build LMetric to learn-KDTree.');
          if param.GetDefaultValue('DNNThread', False) = True then
            begin
              m64 := TMemoryStream64.Create;
              m64.LoadFromFile(OutputModel);
              AI.LMetric_ResNet_SaveToLearnEngine_DT(m64, param.GetDefaultValue('Snapshot', False), RSeri, ImgMatrix, learnEng);
              DisposeObject(m64);
            end
          else
            begin
              LMetric_hnd := AI.LMetric_ResNet_Open(OutputModel);
              AI.LMetric_ResNet_SaveToLearnEngine(LMetric_hnd, param.GetDefaultValue('Snapshot', False), RSeri, ImgMatrix, learnEng);
              AI.LMetric_ResNet_Close(LMetric_hnd);
            end;
          DoStatus('process LMetric to learn-KDTree done.');
          learnEng.SaveToFile(umlChangeFileExt(OutputModel, C_Learn_Ext));
          DisposeObject(learnEng);
        end;
      if (Result) and (param.GetDefaultValue('KDTreeVec', False) = True) and (AI.completed_learning_rate > AI.Last_training_learning_rate) then
        begin
          LMetric_hnd := AI.LMetric_ResNet_Open(OutputModel);
          DoStatus('build LMetric to KDTree.');
          KD_Data := TKDTreeDataList.Create;
          AI.LMetric_ResNet_SaveToKDTree(LMetric_hnd, param.GetDefaultValue('Snapshot', False), RSeri, ImgMatrix, KD_Data);
          AI.LMetric_ResNet_Close(LMetric_hnd);
          DoStatus('process LMetric to KDTree done.');
          kd := TKDTree.Create(zAI.C_LMetric_Dim);
          KD_Data.Build(kd);
          DisposeObject(KD_Data);
          kd.SaveToFile(umlChangeFileExt(OutputModel, C_KDtree_Ext));
          DisposeObject(kd);
        end;
    end
  else if umlMultipleMatch(['TrainMMOD', 'TrainingMMOD', 'TrainMaxMarginDNNObjectDetector', 'TrainMMOD6L', 'TrainingMMOD6L', 'TrainMaxMarginDNNObjectDetector6L'], ComputeFunc) then
    begin
      if param.GetDefaultValue('NoLabel', True) = True then
          ImgMatrix.RunScript('True', 'SetLabel(' + #39#39 + ')');
      mmod_param := AI.LargeScale_MMOD6L_DNN_PrepareTrain(SyncFile, OutputModel);
      mmod_param^.timeout := param.GetDefaultValue('timeout', mmod_param^.timeout);
      mmod_param^.weight_decay := param.GetDefaultValue('weight_decay', mmod_param^.weight_decay);
      mmod_param^.momentum := param.GetDefaultValue('momentum', mmod_param^.momentum);
      mmod_param^.target_size := param.GetDefaultValue('target_size', mmod_param^.target_size);
      mmod_param^.min_target_size := param.GetDefaultValue('min_target_size', mmod_param^.min_target_size);
      mmod_param^.min_detector_window_overlap_iou := param.GetDefaultValue('min_detector_window_overlap_iou', mmod_param^.min_detector_window_overlap_iou);
      mmod_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', mmod_param^.iterations_without_progress_threshold);
      mmod_param^.learning_rate := param.GetDefaultValue('learning_rate', mmod_param^.learning_rate);
      mmod_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', mmod_param^.completed_learning_rate);
      mmod_param^.overlap_NMS_iou_thresh := param.GetDefaultValue('overlap_NMS_iou_thresh', mmod_param^.overlap_NMS_iou_thresh);
      mmod_param^.overlap_NMS_percent_covered_thresh := param.GetDefaultValue('overlap_NMS_percent_covered_thresh', mmod_param^.overlap_NMS_percent_covered_thresh);
      mmod_param^.overlap_ignore_iou_thresh := param.GetDefaultValue('overlap_ignore_iou_thresh', mmod_param^.overlap_ignore_iou_thresh);
      mmod_param^.overlap_ignore_percent_covered_thresh := param.GetDefaultValue('overlap_ignore_percent_covered_thresh', mmod_param^.overlap_ignore_percent_covered_thresh);
      mmod_param^.num_crops := param.GetDefaultValue('num_crops', mmod_param^.num_crops);
      mmod_param^.chip_dims_x := param.GetDefaultValue('chip_dims_x', mmod_param^.chip_dims_x);
      mmod_param^.chip_dims_y := param.GetDefaultValue('chip_dims_y', mmod_param^.chip_dims_y);
      mmod_param^.min_object_size_x := param.GetDefaultValue('min_object_size_x', mmod_param^.min_object_size_x);
      mmod_param^.min_object_size_y := param.GetDefaultValue('min_object_size_y', mmod_param^.min_object_size_y);
      mmod_param^.max_rotation_degrees := param.GetDefaultValue('max_rotation_degrees', mmod_param^.max_rotation_degrees);
      mmod_param^.max_object_size := param.GetDefaultValue('max_object_size', mmod_param^.max_object_size);
      Result := AI.LargeScale_MMOD6L_DNN_Train(mmod_param, RSeri, ImgMatrix) > 0;
      AI.LargeScale_MMOD6L_DNN_FreeTrain(mmod_param);
    end
  else if umlMultipleMatch(['TrainMMOD3L', 'TrainingMMOD3L', 'TrainMaxMarginDNNObjectDetector3L'], ComputeFunc) then
    begin
      if param.GetDefaultValue('NoLabel', True) = True then
          ImgMatrix.RunScript('True', 'SetLabel(' + #39#39 + ')');
      mmod_param := AI.LargeScale_MMOD3L_DNN_PrepareTrain(SyncFile, OutputModel);
      mmod_param^.timeout := param.GetDefaultValue('timeout', mmod_param^.timeout);
      mmod_param^.weight_decay := param.GetDefaultValue('weight_decay', mmod_param^.weight_decay);
      mmod_param^.momentum := param.GetDefaultValue('momentum', mmod_param^.momentum);
      mmod_param^.target_size := param.GetDefaultValue('target_size', mmod_param^.target_size);
      mmod_param^.min_target_size := param.GetDefaultValue('min_target_size', mmod_param^.min_target_size);
      mmod_param^.min_detector_window_overlap_iou := param.GetDefaultValue('min_detector_window_overlap_iou', mmod_param^.min_detector_window_overlap_iou);
      mmod_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', mmod_param^.iterations_without_progress_threshold);
      mmod_param^.learning_rate := param.GetDefaultValue('learning_rate', mmod_param^.learning_rate);
      mmod_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', mmod_param^.completed_learning_rate);
      mmod_param^.overlap_NMS_iou_thresh := param.GetDefaultValue('overlap_NMS_iou_thresh', mmod_param^.overlap_NMS_iou_thresh);
      mmod_param^.overlap_NMS_percent_covered_thresh := param.GetDefaultValue('overlap_NMS_percent_covered_thresh', mmod_param^.overlap_NMS_percent_covered_thresh);
      mmod_param^.overlap_ignore_iou_thresh := param.GetDefaultValue('overlap_ignore_iou_thresh', mmod_param^.overlap_ignore_iou_thresh);
      mmod_param^.overlap_ignore_percent_covered_thresh := param.GetDefaultValue('overlap_ignore_percent_covered_thresh', mmod_param^.overlap_ignore_percent_covered_thresh);
      mmod_param^.num_crops := param.GetDefaultValue('num_crops', mmod_param^.num_crops);
      mmod_param^.chip_dims_x := param.GetDefaultValue('chip_dims_x', mmod_param^.chip_dims_x);
      mmod_param^.chip_dims_y := param.GetDefaultValue('chip_dims_y', mmod_param^.chip_dims_y);
      mmod_param^.min_object_size_x := param.GetDefaultValue('min_object_size_x', mmod_param^.min_object_size_x);
      mmod_param^.min_object_size_y := param.GetDefaultValue('min_object_size_y', mmod_param^.min_object_size_y);
      mmod_param^.max_rotation_degrees := param.GetDefaultValue('max_rotation_degrees', mmod_param^.max_rotation_degrees);
      mmod_param^.max_object_size := param.GetDefaultValue('max_object_size', mmod_param^.max_object_size);
      Result := AI.LargeScale_MMOD3L_DNN_Train(mmod_param, RSeri, ImgMatrix) > 0;
      AI.LargeScale_MMOD3L_DNN_FreeTrain(mmod_param);
    end
  else if umlMultipleMatch(['TrainRNIC', 'TrainingRNIC', 'TrainResNetImageClassifier'], ComputeFunc) then
    begin
      rnic_param := TAI.Init_RNIC_Train_Parameter(SyncFile, OutputModel);
      rnic_param^.timeout := param.GetDefaultValue('timeout', rnic_param^.timeout);
      rnic_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', rnic_param^.iterations_without_progress_threshold);
      rnic_param^.learning_rate := param.GetDefaultValue('learning_rate', rnic_param^.learning_rate);
      rnic_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', rnic_param^.completed_learning_rate);
      rnic_param^.all_bn_running_stats_window_sizes := param.GetDefaultValue('all_bn_running_stats_window_sizes', rnic_param^.all_bn_running_stats_window_sizes);
      rnic_param^.img_mini_batch := param.GetDefaultValue('img_mini_batch', rnic_param^.img_mini_batch);
      Result := AI.RNIC_Train(True, Training_RSeri, ImgMatrix, rnic_param, umlChangeFileExt(OutputModel, '.index'));
      TAI.Free_RNIC_Train_Parameter(rnic_param);
    end
  else if umlMultipleMatch(['TrainLRNIC', 'TrainingLRNIC', 'TrainLResNetImageClassifier'], ComputeFunc) then
    begin
      rnic_param := TAI.Init_LRNIC_Train_Parameter(SyncFile, OutputModel);
      rnic_param^.timeout := param.GetDefaultValue('timeout', rnic_param^.timeout);
      rnic_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', rnic_param^.iterations_without_progress_threshold);
      rnic_param^.learning_rate := param.GetDefaultValue('learning_rate', rnic_param^.learning_rate);
      rnic_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', rnic_param^.completed_learning_rate);
      rnic_param^.all_bn_running_stats_window_sizes := param.GetDefaultValue('all_bn_running_stats_window_sizes', rnic_param^.all_bn_running_stats_window_sizes);
      rnic_param^.img_mini_batch := param.GetDefaultValue('img_mini_batch', rnic_param^.img_mini_batch);
      Result := AI.LRNIC_Train(True, Training_RSeri, ImgMatrix, rnic_param, umlChangeFileExt(OutputModel, '.index'));
      TAI.Free_LRNIC_Train_Parameter(rnic_param);
    end
  else if umlMultipleMatch(['TrainGDCNIC', 'TrainingGDCNIC'], ComputeFunc) then
    begin
      GDCNIC_param := TAI.Init_GDCNIC_Train_Parameter(SyncFile, OutputModel);
      GDCNIC_param^.timeout := param.GetDefaultValue('timeout', GDCNIC_param^.timeout);
      GDCNIC_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', GDCNIC_param^.iterations_without_progress_threshold);
      GDCNIC_param^.learning_rate := param.GetDefaultValue('learning_rate', GDCNIC_param^.learning_rate);
      GDCNIC_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', GDCNIC_param^.completed_learning_rate);
      GDCNIC_param^.img_mini_batch := param.GetDefaultValue('img_mini_batch', GDCNIC_param^.img_mini_batch);
      Result := AI.GDCNIC_Train(param.GetDefaultValue('Snapshot', True), True, Training_RSeri, param.GetDefaultValue('SS_Width', 32), param.GetDefaultValue('SS_Height', 32), ImgMatrix, GDCNIC_param, umlChangeFileExt(OutputModel, '.index'));
      TAI.Free_GDCNIC_Train_Parameter(GDCNIC_param);
    end
  else if umlMultipleMatch(['TrainGNIC', 'TrainingGNIC'], ComputeFunc) then
    begin
      GNIC_param := TAI.Init_GNIC_Train_Parameter(SyncFile, OutputModel);
      GNIC_param^.timeout := param.GetDefaultValue('timeout', GNIC_param^.timeout);
      GNIC_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', GNIC_param^.iterations_without_progress_threshold);
      GNIC_param^.learning_rate := param.GetDefaultValue('learning_rate', GNIC_param^.learning_rate);
      GNIC_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', GNIC_param^.completed_learning_rate);
      GNIC_param^.img_mini_batch := param.GetDefaultValue('img_mini_batch', GNIC_param^.img_mini_batch);
      Result := AI.GNIC_Train(param.GetDefaultValue('Snapshot', True), True, Training_RSeri, param.GetDefaultValue('SS_Width', 32), param.GetDefaultValue('SS_Height', 32), ImgMatrix, GNIC_param, umlChangeFileExt(OutputModel, '.index'));
      TAI.Free_GNIC_Train_Parameter(GNIC_param);
    end
  else if umlMultipleMatch(['TrainSS', 'TrainingSS'], ComputeFunc) then
    begin
      SS_param := TAI.Init_SS_Train_Parameter(SyncFile, OutputModel);
      SS_param^.timeout := param.GetDefaultValue('timeout', SS_param^.timeout);
      SS_param^.weight_decay := param.GetDefaultValue('weight_decay', SS_param^.weight_decay);
      SS_param^.momentum := param.GetDefaultValue('momentum', SS_param^.momentum);
      SS_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', SS_param^.iterations_without_progress_threshold);
      SS_param^.learning_rate := param.GetDefaultValue('learning_rate', SS_param^.learning_rate);
      SS_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', SS_param^.completed_learning_rate);
      SS_param^.img_crops_batch := param.GetDefaultValue('img_crops_batch', SS_param^.img_crops_batch);
      ss_colorPool := ImgMatrix.BuildSegmentationColorBuffer;
      Result := AI.SS_Train(True, Training_RSeri, ImgMatrix, SS_param, ss_colorPool);
      TAI.Free_SS_Train_Parameter(SS_param);
      if Result then
          ss_colorPool.SaveToFile(umlChangeFileExt(OutputModel, '.colorPool'));
      DisposeObject(ss_colorPool);
    end
  else if umlMultipleMatch(['TrainZMetric', 'TrainingZMetric'], ComputeFunc) then
    begin
      zmetric_param := TAI.Init_ZMetric_Parameter(SyncFile, OutputModel);
      zmetric_param^.timeout := param.GetDefaultValue('timeout', zmetric_param^.timeout);
      zmetric_param^.weight_decay := param.GetDefaultValue('weight_decay', zmetric_param^.weight_decay);
      zmetric_param^.momentum := param.GetDefaultValue('momentum', zmetric_param^.momentum);
      zmetric_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', zmetric_param^.iterations_without_progress_threshold);
      zmetric_param^.learning_rate := param.GetDefaultValue('learning_rate', zmetric_param^.learning_rate);
      zmetric_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', zmetric_param^.completed_learning_rate);
      zmetric_param^.step_mini_batch_target_num := param.GetDefaultValue('step_mini_batch_target_num', zmetric_param^.step_mini_batch_target_num);
      zmetric_param^.step_mini_batch_raster_num := param.GetDefaultValue('step_mini_batch_raster_num', zmetric_param^.step_mini_batch_raster_num);
      Result := AI.ZMetric_Train(
        param.GetDefaultValue('Snapshot', False),
        True,
        Training_RSeri,
        ImgMatrix,
        param.GetDefaultValue('SS_Width', 150),
        param.GetDefaultValue('SS_Height', 150),
        zmetric_param);
      TAI.Free_ZMetric_Parameter(zmetric_param);
      if (Result) and (param.GetDefaultValue('LearnVec', False) = True) and (AI.completed_learning_rate > AI.Last_training_learning_rate) then
        begin
          DoStatus('build Z-Metric to learn-KDTree.');
          learnEng := TAI.Build_ZMetric_Learn;

          m64 := TMemoryStream64.Create;
          m64.LoadFromFile(OutputModel);
          AI.ZMetric_SaveToLearnEngine_DT(
            param.GetDefaultValue('LearnThreadNum', 2),
            m64,
            param.GetDefaultValue('Snapshot', False),
            RSeri,
            ImgMatrix,
            param.GetDefaultValue('SS_Width', 150),
            param.GetDefaultValue('SS_Height', 150),
            learnEng);
          DisposeObject(m64);

          learnEng.SaveToFile(umlChangeFileExt(OutputModel, C_Learn_Ext));
          DisposeObject(learnEng);
          DoStatus('process Z-Metric to learn-KDTree done.');
        end;
    end
  else
    begin
      DoStatus('AI Training task failed: no define ComputeFunc.');
    end;

  { save log }
  try
    for i := 0 to AI.API^.Log.Count - 1 do
        LogData.Add(umlDateTimeToStr(AI.API^.Log[i].LogTime) + #9 + AI.API^.Log[i].LogText);
    LogData.SaveToFile(LogFile);
    DoStatus('save log file %s', [LogFile.Text]);
  except
  end;

  { save step }
  try
    for i := 0 to AI.API^.OneStepList.Count - 1 do
        StepData.AddStep(AI.API^.OneStepList[i]);
    m64 := TMemoryStream64.Create;
    StepData.SaveToStream(m64);
    m64.SaveToFile(StepFile);
    DisposeObject(m64);
    DoStatus('save step file %s', [StepFile.Text]);
    if not umlMultipleMatch('*.csv', StepFile) then
        StepData.ExportToExcelFile(umlChangeFileExt(StepFile, '.csv'));
    DoStatus('save csv file %s', [umlChangeFileExt(StepFile, '.csv').Text]);
  except
  end;

  try
    { free ImgMatrix }
    DoStatus('free ImageMatrix.');
    DisposeObject(ImgMatrix);

    { free Rastermization Serialized }
    DoStatus('free Rastermization Serialized.');
    DisposeObject(RSeri);
    DoStatus('free Rastermization Serialized of stream.');
    DisposeObject(RSeriStream);
    DoStatus('remove Serialized temp file %s', [RasterSerializedFile.Text]);
    umlDeleteFile(RasterSerializedFile);

    { free AI Engine Serialized }
    DoStatus('free training Serialized.');
    DisposeObject(Training_RSeri);
    DoStatus('free training Serialized of stream.');
    DisposeObject(Training_RSeriStream);
    DoStatus('remove training temp file %s', [Training_RasterSerializedFile.Text]);
    umlDeleteFile(Training_RasterSerializedFile);

    { free log }
    DoStatus('free log');
    DisposeObject(LogData);

    { free step }
    DoStatus('free step');
    DisposeObject(StepData);
  except
  end;
end;

procedure test_imageProcessing(imgfile: U_String);
var
  AI: TAI;
  ph: U_String;
  rIn, rOut: TMemoryRaster;
  rDesc: TAI_Rect_Desc;
  i: Integer;
begin
  AI := TAI.OpenEngine;
  rIn := NewRasterFromFile(imgfile);
  ph := umlGetFilePath(imgfile);

  rOut := AI.Salient(rIn, 100);
  rOut.SaveToBmp24File(umlCombineFileName(ph, 'Salient_out.bmp'));
  DisposeObject(rOut);

  rOut := AI.Segment(rIn, 5000, 50);
  rOut.SaveToBmp24File(umlCombineFileName(ph, 'Segment_out.bmp'));
  DisposeObject(rOut);

  rDesc := AI.CandidateObject(rIn, 50, 20000);
  for i := 0 to Length(rDesc) - 1 do
      rIn.DrawRect(Rect(rDesc[i]), RColorF(0.5, 0.1, 0.1, 0.5));
  rIn.SaveToBmp24File(umlCombineFileName(ph, 'CandidateObject_out.bmp'));

  DisposeObject(rIn);
  DisposeObject(AI);
end;

procedure test_poissonBlend(sourfile, sourMask, destFile: U_String);
var
  AI: TAI;
  ph: U_String;
  sourIn, sourMaskIn, destIn: TMemoryRaster;
  i, j: Integer;
begin
  AI := TAI.OpenEngine;
  sourIn := NewRasterFromFile(sourfile);
  sourMaskIn := NewRasterFromFile(sourMask);
  destIn := NewRasterFromFile(destFile);

  for j := 0 to sourMaskIn.Height - 1 do
    for i := 0 to sourMaskIn.Width - 1 do
      if sourMaskIn.PixelRed[i, j] > 50 then
          sourIn.PixelAlpha[i, j] := $FF
      else
          sourIn.PixelAlpha[i, j] := 0;

  ph := umlGetFilePath(destFile);
  AI.PoissonBlend(2.2, sourIn, destIn, 450, 380, False);
  destIn.SaveToFile(umlCombineFileName(ph, 'poisson_blend_out.bmp'));

  DisposeObject(sourIn);
  DisposeObject(sourMaskIn);
  DisposeObject(destIn);
  DisposeObject(AI);
end;

procedure test_Unmixed(sourfile: U_String);
var
  AI: TAI;
  ph, fp: U_String;
  sourIn: TMemoryRaster;
  p: PUnmixedData;
  i, j: Integer;
  m: TMemoryRaster;
  p_: PUnmixedData_;
begin
  AI := TAI.OpenEngine;
  sourIn := NewRasterFromFile(sourfile);
  ph := umlGetFilePath(sourfile);
  fp := umlChangeFileExt(umlGetFileName(sourfile), '');

  sourIn.FitScale(512, 512);
  p := AI.Unmixing_Raster(sourIn);
  sourIn.SaveToFile(umlCombineFileName(ph, fp + PFormat('_Unmixing_origin.bmp', [])));
  p_ := p^.data;
  for j := 0 to p^.data_num - 1 do
    begin
      m := NewRaster();
      m.SetWorkMemory(p_^.output.Bits, p_^.output.Width, p_^.output.Height);
      m.SaveToFile(umlCombineFileName(ph, fp + PFormat('_unmixed(%d)_output.bmp', [j + 1])));
      DisposeObject(m);

      m := NewRaster();
      m.SetWorkMemory(p_^.output_a.Bits, p_^.output_a.Width, p_^.output_a.Height);
      m.SaveToFile(umlCombineFileName(ph, fp + PFormat('_unmixed(%d)_output_alpha.bmp', [j + 1])));
      DisposeObject(m);

      m := NewRaster();
      m.SetWorkMemory(p_^.overlay.Bits, p_^.overlay.Width, p_^.overlay.Height);
      m.SaveToFile(umlCombineFileName(ph, fp + PFormat('_unmixed(%d)_output_overlay.bmp', [j + 1])));
      DisposeObject(m);

      m := NewRaster();
      m.SetWorkMemory(p_^.refined.Bits, p_^.refined.Width, p_^.refined.Height);
      m.SaveToFile(umlCombineFileName(ph, fp + PFormat('_unmixed(%d)_output_refined.bmp', [j + 1])));
      DisposeObject(m);

      m := NewRaster();
      m.SetWorkMemory(p_^.refined_a.Bits, p_^.refined_a.Width, p_^.refined_a.Height);
      m.SaveToFile(umlCombineFileName(ph, fp + PFormat('_unmixed(%d)_output_refined_alpha.bmp', [j + 1])));
      DisposeObject(m);

      m := NewRaster();
      m.SetWorkMemory(p_^.refined_overlay.Bits, p_^.refined_overlay.Width, p_^.refined_overlay.Height);
      m.SaveToFile(umlCombineFileName(ph, fp + PFormat('_unmixed(%d)_output_refined_overlay.bmp', [j + 1])));
      DisposeObject(m);
      inc(p_);
    end;
  AI.Unmixing_Free(p);

  DisposeObject(sourIn);
  DisposeObject(AI);
end;

procedure test_mmod_largescale_training(dataset_file, sync_file, output_file: U_String);
var
  AI: TAI;
  imgL: TAI_ImageList;
  RSeri: TRasterSerialized;
  param: PMMOD_Train_Parameter;
begin
  AI := TAI.OpenEngine();
  RSeri := TRasterSerialized.Create(TCoreClassFileStream.Create(AI.MakeSerializedFileName, fmCreate));
  imgL := TAI_ImageList.Create;
  imgL.LoadFromFile(dataset_file);
  imgL.SerializedAndRecycleMemory(RSeri);
  param := AI.LargeScale_MMOD6L_DNN_PrepareTrain(sync_file, output_file);
  param^.prepare_crops_img_num := 20;
  param^.num_crops := 50;
  AI.LargeScale_MMOD6L_DNN_Train(param, RSeri, imgL);
end;

constructor TOneStepList.Create;
begin
  inherited Create;
  Critical := TCritical.Create;
  FOnStep := nil;
end;

destructor TOneStepList.Destroy;
begin
  Clear;
  DisposeObject(Critical);
  inherited Destroy;
end;

procedure TOneStepList.Delete(index: Integer);
begin
  Critical.Acquire;
  try
    Dispose(Items[index]);
    inherited Delete(index);
  finally
      Critical.Release;
  end;
end;

procedure TOneStepList.Clear;
var
  i: Integer;
begin
  Critical.Acquire;
  try
    for i := 0 to Count - 1 do
        Dispose(Items[i]);
    inherited Clear;
  finally
      Critical.Release;
  end;
end;

procedure TOneStepList.AddStep(one_step_calls: UInt64; average_loss, learning_rate: Double);
var
  p: POneStep;
begin
  new(p);
  p^.StepTime := umlNow();
  p^.one_step_calls := one_step_calls;
  p^.average_loss := average_loss;
  p^.learning_rate := learning_rate;
  Critical.Acquire;
  Add(p);
  Critical.Release;
  if Assigned(FOnStep) then
    begin
      try
          FOnStep(p);
      except
      end;
    end;
end;

procedure TOneStepList.AddStep(p_: POneStep);
var
  p: POneStep;
begin
  new(p);
  p^ := p_^;
  Critical.Acquire;
  Add(p);
  Critical.Release;
  if Assigned(FOnStep) then
    begin
      try
          FOnStep(p);
      except
      end;
    end;
end;

procedure TOneStepList.SaveToStream(stream: TMemoryStream64);
var
  i: Integer;
  p: POneStep;
begin
  Critical.Acquire;
  try
    stream.WriteInt32(Count);
    for i := 0 to Count - 1 do
      begin
        p := Items[i];
        stream.WritePtr(p, SizeOf(TOneStep));
      end;
  finally
      Critical.Release;
  end;
end;

procedure TOneStepList.LoadFromStream(stream: TMemoryStream64);
var
  c, i: Integer;
  p: POneStep;
begin
  Clear;
  Critical.Acquire;
  try
    c := stream.ReadInt32;
    for i := 0 to c - 1 do
      begin
        new(p);
        stream.ReadPtr(p, SizeOf(TOneStep));
        Add(p);
      end;
  finally
      Critical.Release;
  end;
end;

procedure TOneStepList.ExportToExcelStream(stream: TMemoryStream64);
var
  i: Integer;
  p: POneStep;
begin
  stream.WriteANSI('time,step,loss,rate' + #13#10);
  Critical.Acquire;
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      stream.WriteANSI(PFormat('%s,%d,%g,%g' + #13#10, [umlDateTimeToStr(p^.StepTime).Text, p^.one_step_calls, p^.average_loss, p^.learning_rate]));
    end;
  Critical.Release;
end;

procedure TOneStepList.ExportToExcelFile(fileName: U_String);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  ExportToExcelStream(m64);
  m64.SaveToFile(fileName);
  DisposeObject(m64);
end;

constructor TAlignment.Create(OwnerAI: TAI);
begin
  inherited Create;
  AI := OwnerAI;
end;

destructor TAlignment.Destroy;
begin
  inherited Destroy;
end;

procedure TAlignment_Face.Alignment(imgList: TAI_ImageList);
var
  mr: TMemoryRaster;
  face_hnd: TFACE_Handle;
  i, j, k: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  sp_desc: TSP_Desc;
  R1, R2: TRectV2;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];

      { full detector do scale 4x size }
      mr := NewRaster();
      mr.ZoomFrom(img.Raster, img.Raster.Width * 4, img.Raster.Height * 4);
      { extract face }
      face_hnd := AI.Face_Detector_All(mr);
      { dispose raster }
      DisposeObject(mr);

      if face_hnd <> nil then
        begin
          { remove overlap detector }
          for j := 0 to AI.Face_chips_num(face_hnd) - 1 do
            begin
              sp_desc := AI.Face_Shape(face_hnd, j);
              if (Length(sp_desc) > 0) then
                begin
                  for k := 0 to Length(sp_desc) - 1 do
                    begin
                      sp_desc[k].X := Round(sp_desc[k].X * 0.25);
                      sp_desc[k].Y := Round(sp_desc[k].Y * 0.25);
                    end;
                  R1 := RectV2(AI.Face_Rect(face_hnd, j));
                  R1 := ForwardRect(RectMul(R1, 0.25));
                  R2 := ForwardRect(GetSPBound(sp_desc, 0.1));

                  if InRect(sp_desc, img.Raster.BoundsRectV2) then
                    begin
                      img.RemoveDetectorFromRect(BoundRect(R1, R2));
                    end;
                  SetLength(sp_desc, 0);
                end;
            end;

          { make detector }
          for j := 0 to AI.Face_chips_num(face_hnd) - 1 do
            begin
              sp_desc := AI.Face_Shape(face_hnd, j);
              if (Length(sp_desc) > 0) then
                begin
                  for k := 0 to Length(sp_desc) - 1 do
                    begin
                      sp_desc[k].X := Round(sp_desc[k].X * 0.25);
                      sp_desc[k].Y := Round(sp_desc[k].Y * 0.25);
                    end;
                  R1 := RectV2(AI.Face_Rect(face_hnd, j));
                  R1 := ForwardRect(RectMul(R1, 0.25));
                  R2 := ForwardRect(GetSPBound(sp_desc, 0.1));

                  if InRect(sp_desc, img.Raster.BoundsRectV2) then
                    begin
                      detDef := TAI_DetectorDefine.Create(img);
                      img.DetectorDefineList.Add(detDef);
                      DisposeObject(detDef.PrepareRaster);
                      detDef.PrepareRaster := AI.Face_chips(face_hnd, j);
                      SPToVec(sp_desc, detDef.Part);

                      detDef.R := MakeRect(BoundRect(R1, R2));
                    end;
                  SetLength(sp_desc, 0);
                end;
            end;

          AI.Face_Close(face_hnd);
        end;
    end;
end;

procedure TAlignment_FastFace.Alignment(imgList: TAI_ImageList);
var
  face_hnd: TFACE_Handle;
  i, j, k: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  sp_desc: TSP_Desc;
  R1, R2: TRectV2;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];

      { extract face }
      face_hnd := AI.Face_Detector_All(img.Raster);

      if face_hnd <> nil then
        begin
          { remove overlap detector }
          for j := 0 to AI.Face_chips_num(face_hnd) - 1 do
            begin
              sp_desc := AI.Face_Shape(face_hnd, j);
              if (Length(sp_desc) > 0) then
                begin
                  R1 := RectV2(AI.Face_Rect(face_hnd, j));
                  R2 := ForwardRect(GetSPBound(sp_desc, 0.1));

                  if InRect(sp_desc, img.Raster.BoundsRectV2) then
                    begin
                      img.RemoveDetectorFromRect(BoundRect(R1, R2));
                    end;
                  SetLength(sp_desc, 0);
                end;
            end;

          { make detector }
          for j := 0 to AI.Face_chips_num(face_hnd) - 1 do
            begin
              sp_desc := AI.Face_Shape(face_hnd, j);
              if (Length(sp_desc) > 0) then
                begin
                  R1 := RectV2(AI.Face_Rect(face_hnd, j));
                  R2 := ForwardRect(GetSPBound(sp_desc, 0.1));

                  if InRect(sp_desc, img.Raster.BoundsRectV2) then
                    begin
                      detDef := TAI_DetectorDefine.Create(img);
                      img.DetectorDefineList.Add(detDef);
                      DisposeObject(detDef.PrepareRaster);
                      detDef.PrepareRaster := AI.Face_chips(face_hnd, j);
                      SPToVec(sp_desc, detDef.Part);

                      detDef.R := MakeRect(BoundRect(R1, R2));
                    end;
                  SetLength(sp_desc, 0);
                end;
            end;

          AI.Face_Close(face_hnd);
        end;
    end;
end;

procedure TAlignment_Fast4VertexProjection.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  r4: TV2R4;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      for j := 0 to img.DetectorDefineList.Count - 1 do
        begin
          detDef := img.DetectorDefineList[j];
          if detDef.Part.Count = 4 then
            begin
              detDef.PrepareRaster.SetSize(SS_Width, SS_Height, RColor(0, 0, 0));
              r4 := TV2R4.RebuildVertex(detDef.Part.BuildArray);
              detDef.Owner.Raster.ProjectionTo(detDef.PrepareRaster, r4, detDef.PrepareRaster.BoundsV2Rect40, True, 1.0);
            end;
        end;
    end;
end;

procedure TAlignment_ScaleSpace.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  R: TRect;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      for j := 0 to img.DetectorDefineList.Count - 1 do
        begin
          detDef := img.DetectorDefineList[j];
          R := CalibrationRectInRect(RectScaleSpace(detDef.R, SS_Width, SS_Height), detDef.Owner.Raster.BoundsRect);
          if CalibrateDetectorDefine then
              detDef.R := R;
          DisposeObject(detDef.PrepareRaster);
          detDef.PrepareRaster := detDef.Owner.Raster.BuildAreaOffsetScaleSpace(R, SS_Width, SS_Height);
        end;
    end;
end;

procedure TAlignment_OD6L.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  OD_Desc: TOD_Desc;
  mr: TMemoryRaster;
begin
  if OD_Hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      mr := NewRaster();
      mr.ZoomFrom(img.Raster, img.Raster.Width * 4, img.Raster.Height * 4);
      OD_Desc := AI.OD6L_Process(OD_Hnd, mr, 8192);
      DisposeObject(mr);

      { remove overlap detector }
      for j := 0 to Length(OD_Desc) - 1 do
          img.RemoveDetectorFromRect(RectMul(RectV2(OD_Desc[j]), 0.25));

      { make detector }
      for j := 0 to Length(OD_Desc) - 1 do
        begin
          detDef := TAI_DetectorDefine.Create(img);
          detDef.R := MakeRect(RectMul(RectV2(OD_Desc[j]), 0.25));
          img.DetectorDefineList.Add(detDef);
        end;
    end;
end;

procedure TAlignment_FastOD6L.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  OD_Desc: TOD_Desc;
begin
  if OD_Hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      OD_Desc := AI.OD6L_Process(OD_Hnd, img.Raster, 8192);

      { remove overlap detector }
      for j := 0 to Length(OD_Desc) - 1 do
          img.RemoveDetectorFromRect(RectV2(OD_Desc[j]));

      { make detector }
      for j := 0 to Length(OD_Desc) - 1 do
        begin
          detDef := TAI_DetectorDefine.Create(img);
          detDef.R := Rect(OD_Desc[j]);
          img.DetectorDefineList.Add(detDef);
        end;
    end;
end;

procedure TAlignment_OD3L.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  OD_Desc: TOD_Desc;
  mr: TMemoryRaster;
begin
  if OD_Hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      mr := NewRaster();
      mr.ZoomFrom(img.Raster, img.Raster.Width * 4, img.Raster.Height * 4);
      OD_Desc := AI.OD3L_Process(OD_Hnd, mr, 8192);
      DisposeObject(mr);

      { remove overlap detector }
      for j := 0 to Length(OD_Desc) - 1 do
          img.RemoveDetectorFromRect(RectMul(RectV2(OD_Desc[j]), 0.25));

      { make detector }
      for j := 0 to Length(OD_Desc) - 1 do
        begin
          detDef := TAI_DetectorDefine.Create(img);
          detDef.R := MakeRect(RectMul(RectV2(OD_Desc[j]), 0.25));
          img.DetectorDefineList.Add(detDef);
        end;
    end;
end;

procedure TAlignment_FastOD3L.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  OD_Desc: TOD_Desc;
begin
  if OD_Hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      OD_Desc := AI.OD3L_Process(OD_Hnd, img.Raster, 8192);

      { remove overlap detector }
      for j := 0 to Length(OD_Desc) - 1 do
          img.RemoveDetectorFromRect(RectV2(OD_Desc[j]));

      { make detector }
      for j := 0 to Length(OD_Desc) - 1 do
        begin
          detDef := TAI_DetectorDefine.Create(img);
          detDef.R := Rect(OD_Desc[j]);
          img.DetectorDefineList.Add(detDef);
        end;
    end;
end;

procedure TAlignment_OD6L_Marshal.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  OD_Desc: TOD_Marshal_Desc;
  mr: TMemoryRaster;
begin
  if OD_Hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      mr := NewRaster();
      mr.ZoomFrom(img.Raster, img.Raster.Width * 4, img.Raster.Height * 4);
      OD_Desc := AI.OD6L_Marshal_Process(OD_Hnd, mr);
      DisposeObject(mr);

      { remove overlap detector }
      for j := 0 to Length(OD_Desc) - 1 do
          img.RemoveDetectorFromRect(RectMul(OD_Desc[j].R, 0.25));

      { make detector }
      for j := 0 to Length(OD_Desc) - 1 do
        begin
          detDef := TAI_DetectorDefine.Create(img);
          detDef.R := MakeRect(RectMul(OD_Desc[j].R, 0.25));
          detDef.Token := OD_Desc[j].Token;
          img.DetectorDefineList.Add(detDef);
        end;
    end;
end;

procedure TAlignment_FastOD6L_Marshal.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  OD_Desc: TOD_Marshal_Desc;
begin
  if OD_Hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      OD_Desc := AI.OD6L_Marshal_Process(OD_Hnd, img.Raster);

      { remove overlap detector }
      for j := 0 to Length(OD_Desc) - 1 do
          img.RemoveDetectorFromRect(OD_Desc[j].R);

      { make detector }
      for j := 0 to Length(OD_Desc) - 1 do
        begin
          detDef := TAI_DetectorDefine.Create(img);
          detDef.R := MakeRect(OD_Desc[j].R);
          detDef.Token := OD_Desc[j].Token;
          img.DetectorDefineList.Add(detDef);
        end;
    end;
end;

procedure TAlignment_SP.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  sp_desc: TSP_Desc;
begin
  if sp_hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      for j := 0 to img.DetectorDefineList.Count - 1 do
        begin
          detDef := img.DetectorDefineList[j];

          sp_desc := AI.SP_Process(sp_hnd, detDef.Owner.Raster, AIRect(detDef.R), 8192);
          if Length(sp_desc) > 0 then
            begin
              detDef.Part.Clear;
              SPToVec(sp_desc, detDef.Part);
              detDef.PrepareRaster.Reset;
              SetLength(sp_desc, 0);
            end;
        end;
    end;
end;

procedure TAlignment_Face_SP.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  r_Desc: TAI_Rect_Desc;
  sp_desc: TSP_Desc;
  faceHnd: TFACE_Handle;
begin
  if sp_hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];

      SetLength(r_Desc, img.DetectorDefineList.Count);
      for j := 0 to img.DetectorDefineList.Count - 1 do
        begin
          detDef := img.DetectorDefineList[j];
          r_Desc[j] := AIRect(detDef.R);
        end;
      faceHnd := AI.Face_Detector(img.Raster, r_Desc, zAI.C_Metric_Input_Size);
      SetLength(r_Desc, 0);
      for j := 0 to img.DetectorDefineList.Count - 1 do
        begin
          detDef := img.DetectorDefineList[j];
          sp_desc := AI.Face_Shape(faceHnd, j);
          detDef.Part.Clear;
          SPToVec(sp_desc, detDef.Part);
          DisposeObject(detDef.PrepareRaster);
          detDef.PrepareRaster := AI.Face_chips(faceHnd, j);
          SetLength(sp_desc, 0);
        end;
      AI.Face_Close(faceHnd);
    end;
end;

procedure TAlignment_MMOD6L.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  MMOD_Desc: TMMOD_Desc;
  mr: TMemoryRaster;
begin
  if MMOD_hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      mr := NewRaster();
      mr.ZoomFrom(img.Raster, img.Raster.Width * 4, img.Raster.Height * 4);
      MMOD_Desc := AI.MMOD6L_DNN_Process(MMOD_hnd, mr);
      DisposeObject(mr);

      { remove overlap detector }
      for j := 0 to Length(MMOD_Desc) - 1 do
          img.RemoveDetectorFromRect(RectMul(MMOD_Desc[j].R, 0.25));

      { make detector }
      for j := 0 to Length(MMOD_Desc) - 1 do
        begin
          detDef := TAI_DetectorDefine.Create(img);
          detDef.R := MakeRect(RectMul(MMOD_Desc[j].R, 0.25));
          detDef.Token := MMOD_Desc[j].Token;
          img.DetectorDefineList.Add(detDef);
        end;
    end;
end;

procedure TAlignment_FastMMOD6L.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  MMOD_Desc: TMMOD_Desc;
begin
  if MMOD_hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      MMOD_Desc := AI.MMOD6L_DNN_Process(MMOD_hnd, img.Raster);

      { remove overlap detector }
      for j := 0 to Length(MMOD_Desc) - 1 do
          img.RemoveDetectorFromRect(MMOD_Desc[j].R);

      { make detector }
      for j := 0 to Length(MMOD_Desc) - 1 do
        begin
          detDef := TAI_DetectorDefine.Create(img);
          detDef.R := MakeRect(MMOD_Desc[j].R);
          detDef.Token := MMOD_Desc[j].Token;
          img.DetectorDefineList.Add(detDef);
        end;
    end;
end;

procedure TAlignment_MMOD3L.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  MMOD_Desc: TMMOD_Desc;
  mr: TMemoryRaster;
begin
  if MMOD_hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      mr := NewRaster();
      mr.ZoomFrom(img.Raster, img.Raster.Width * 4, img.Raster.Height * 4);
      MMOD_Desc := AI.MMOD3L_DNN_Process(MMOD_hnd, mr);
      DisposeObject(mr);

      { remove overlap detector }
      for j := 0 to Length(MMOD_Desc) - 1 do
          img.RemoveDetectorFromRect(RectMul(MMOD_Desc[j].R, 0.25));

      { make detector }
      for j := 0 to Length(MMOD_Desc) - 1 do
        begin
          detDef := TAI_DetectorDefine.Create(img);
          detDef.R := MakeRect(RectMul(MMOD_Desc[j].R, 0.25));
          detDef.Token := MMOD_Desc[j].Token;
          img.DetectorDefineList.Add(detDef);
        end;
    end;
end;

procedure TAlignment_FastMMOD3L.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  MMOD_Desc: TMMOD_Desc;
begin
  if MMOD_hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      MMOD_Desc := AI.MMOD3L_DNN_Process(MMOD_hnd, img.Raster);

      { remove overlap detector }
      for j := 0 to Length(MMOD_Desc) - 1 do
          img.RemoveDetectorFromRect(MMOD_Desc[j].R);

      { make detector }
      for j := 0 to Length(MMOD_Desc) - 1 do
        begin
          detDef := TAI_DetectorDefine.Create(img);
          detDef.R := MakeRect(MMOD_Desc[j].R);
          detDef.Token := MMOD_Desc[j].Token;
          img.DetectorDefineList.Add(detDef);
        end;
    end;
end;

procedure TAlignment_SS.DoGetPixelSegClassify(X, Y: Integer; color: TRColor; var Classify: TMorphologyClassify);
begin
  Classify := color;
end;

procedure TAlignment_SS.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  mr, nm: TMemoryRaster;
  s: TMorphologySegmentation;
  sp: TMorphologyPool;
begin
  if SS_hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      img.ClearSegmentation;

      mr := AI.SS_Process(SS_hnd, img.Raster, nil, nil);
      if mr <> nil then
        begin
          s := TMorphologySegmentation.Create;
          s.OnGetPixelSegClassify := {$IFDEF FPC}@{$ENDIF FPC}DoGetPixelSegClassify;
          s.BuildSegmentation(mr);
          s.RemoveNoise(500);

          for j := 0 to s.Count - 1 do
            begin
              sp := s[j];
              nm := sp.BuildDatamap(RColor(0, 0, 0), RColor($FF, $FF, $FF));
              img.SegmentationMaskList.BuildSegmentationMask(nm.Width, nm.Height,
                nm, RColor($FF, $FF, $FF), RColor(0, 0, 0, 0), RColor($1F, $7F, $1F), '');
              DisposeObject(nm);
            end;

          DisposeObject(s);
          DisposeObject(mr);
        end;
    end;
end;

procedure TAlignment_Metric.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;

  tmp: TRaster;
  vec: TLVec;
  idx: Integer;
  d: TLFloat;
begin
  if MetricHnd = nil then
      exit;
  if Learn_ = nil then
      exit;

  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      for j := 0 to img.DetectorDefineList.Count - 1 do
        begin
          detDef := img.DetectorDefineList[j];

          if (detDef.PrepareRaster <> nil) and (not detDef.PrepareRaster.Empty) then
            begin
              tmp := NewRaster();
              tmp.SetWorkMemory(detDef.PrepareRaster);
            end
          else
              tmp := detDef.Owner.Raster.BuildAreaOffsetScaleSpace(detDef.R, C_Metric_Input_Size, C_Metric_Input_Size);

          vec := AI.Metric_ResNet_Process(MetricHnd, tmp);
          DisposeObject(tmp);

          if Length(vec) = Learn_.InSize then
            begin
              idx := Learn_.ProcessMaxIndex(vec);
              d := LDistance(Learn_.MemorySource[idx]^.m_in, vec);
              if d <= MinK then
                  detDef.Token := Learn_.MemorySource[idx]^.Token;
            end;
          SetLength(vec, 0);
        end;
    end;
end;

procedure TAlignment_ZMetric.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  detDef: TAI_DetectorDefine;
  tmp: TRaster;
  vec: TLVec;
  idx: Integer;
  d: TLFloat;
begin
  if MetricHnd = nil then
      exit;
  if Learn_ = nil then
      exit;

  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      for j := 0 to img.DetectorDefineList.Count - 1 do
        begin
          detDef := img.DetectorDefineList[j];

          if (detDef.PrepareRaster <> nil) and (not detDef.PrepareRaster.Empty) then
            begin
              tmp := NewRaster();
              tmp.SetWorkMemory(detDef.PrepareRaster);
            end
          else
              tmp := detDef.Owner.Raster.BuildAreaOffsetScaleSpace(detDef.R, SS_Width, SS_Height);

          vec := AI.ZMetric_Process(MetricHnd, tmp, SS_Width, SS_Height);
          DisposeObject(tmp);
          if Length(vec) = Learn_.InSize then
            begin
              idx := Learn_.ProcessMaxIndex(vec);
              d := LDistance(Learn_.MemorySource[idx]^.m_in, vec);
              if d <= MinK then
                  detDef.Token := Learn_.MemorySource[idx]^.Token;
            end;
          SetLength(vec, 0);
        end;
    end;
end;

function TMorphExpIntf.MorphExp_HotMap(OpRunTime: TOpCustomRunTime; var param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster input]';
      exit;
    end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  with TAI.OpenEngine(FAI_EntryAPI) do
    begin
      HotMap(meRT.Step.OutData.Raster);
      Free;
    end;

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TMorphExpIntf.MorphExp_JetMap(OpRunTime: TOpCustomRunTime; var param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster input]';
      exit;
    end;

  meRT.Step.OutData.Assign(meRT.Step.InData);
  with TAI.OpenEngine(FAI_EntryAPI) do
    begin
      JetMap(meRT.Step.OutData.Raster);
      Free;
    end;

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

function TMorphExpIntf.MorphExp_Salient(OpRunTime: TOpCustomRunTime; var param: TOpParam): Variant;
var
  meRT: TMorphExpRunTime;
begin
  meRT := OpRunTime as TMorphExpRunTime;
  if (meRT.Step.InData = nil) or (meRT.Step.InData.FoundData = 0) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: data is nil]';
      exit;
    end;

  if (meRT.Step.InData.Raster = nil) then
    begin
      Result := OpRunTime.Trigger^.Name + '[error: must be raster input]';
      exit;
    end;

  with TAI.OpenEngine(FAI_EntryAPI) do
    begin
      meRT.Step.OutData.Raster := Salient(meRT.Step.InData.Raster);
      Free;
    end;

  Result := OpRunTime.Trigger^.Name + '[ok]';
end;

procedure TMorphExpIntf.RegMorphExpExternalAPI(Exp: TMorphExpRunTime);
var
  prefix: U_String;
begin
  Exp.RegObjectOpM('HotMap', 'HotMap(): build Hot rastermization', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_HotMap)^.Category := 'Z-AI Engine';
  Exp.RegObjectOpM('JetMap', 'JetMap(): build Hot rastermization', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_JetMap)^.Category := 'Z-AI Engine';
  Exp.RegObjectOpM('Salient', 'Salient(): build Salient rastermization', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Salient)^.Category := 'Z-AI Engine';

  prefix := umlChangeFileExt(umlGetFileName(FAI_EntryAPI^.LibraryFile), '') + '_';
  prefix := prefix.ReplaceChar('.', '_');
  Exp.RegObjectOpM(prefix + 'HotMap', prefix + 'HotMap(): build Hot rastermization', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_HotMap)^.Category := 'Z-AI Engine';
  Exp.RegObjectOpM(prefix + 'JetMap', prefix + 'JetMap(): build Hot rastermization', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_JetMap)^.Category := 'Z-AI Engine';
  Exp.RegObjectOpM(prefix + 'Salient', prefix + 'Salient(): build Salient rastermization', {$IFDEF FPC}@{$ENDIF FPC}MorphExp_Salient)^.Category := 'Z-AI Engine';
end;

constructor TMorphExpIntf.Create(AI_EntryAPI_: PAI_EntryAPI);
begin
  inherited Create;;
  FAI_EntryAPI := AI_EntryAPI_;
end;

destructor TMorphExpIntf.Destroy;
begin
  inherited Destroy;
end;

procedure TAI_EntryAPI.Lock;
begin
  Critical.Lock;
end;

procedure TAI_EntryAPI.UnLock;
begin
  Critical.UnLock;
end;

function TAI_EntryAPI.GetVersionName: TPascalString;
begin
  case VerMode of
    1: Result := 'Snapshot';
    2: Result := 'Alpha';
    3: Result := 'Beta';
    4: Result := 'Pre';
    5: Result := 'RC';
    6: Result := 'GA';
    7: Result := 'Release';
    8: Result := 'Stable';
    9: Result := 'Current';
    10: Result := 'Eval';
    11: Result := 'Patch';
    else Result := '';
  end;
end;

function TAI_EntryAPI.GetVersionTitle: TPascalString;
begin
  if VerMode in [7, 8] then
      Result := PFormat('%d.%d %s', [MajorVer, MinorVer, GetVersionName().Text])
  else
      Result := PFormat('%d.%d %s%d', [MajorVer, MinorVer, GetVersionName().Text, VerID]);
end;

function TAI_EntryAPI.GetVersionInfo: TPascalString;
begin
  Result := 'Version: ' + GetVersionTitle() + #13#10;
  Result.Append('Activted Engine: %s' + #13#10, [LibraryFile]);
  Result.Append('CUDA: ' + if_(CUDA = 1, 'YES', 'NO') + #13#10);
  Result.Append('Intel-MKL: ' + if_(MKL = 1, 'YES', 'NO') + #13#10);
  Result.Append('Trainer: ' + if_(Training = 1, 'YES', 'NO') + #13#10);
  Result.Append('Licensed: ' + if_(Authentication = 1, 'Authorized', 'Free') + #13#10);
end;

constructor TAI.Create;
begin
  inherited Create;
  FAI_EntryAPI := nil;
  FFace_SP_Hnd := nil;
  TrainingControl.pause := 0;
  TrainingControl.stop := 0;
  Critical := TCritical.Create;

  Parallel_OD6L_Hnd := nil;
  Parallel_OD3L_Hnd := nil;
  Parallel_OD_Marshal_Hnd := nil;
  Parallel_SP_Hnd := nil;

  RootPath := GetAITempDirectory();

  Last_training_average_loss := 0;
  Last_training_learning_rate := 0;
  completed_learning_rate := 0;
end;

class function TAI.OpenEngine(libFile: SystemString): TAI;
begin
  Result := TAI.Create;
  Result.FAI_EntryAPI := Load_ZAI(libFile);
  if Result.FAI_EntryAPI = nil then
      Result.FAI_EntryAPI := Load_ZAI(AI_Engine_Library);

  if Result.FAI_EntryAPI <> nil then
    if Result.FAI_EntryAPI^.CheckKey() = 0 then
        Result.FAI_EntryAPI := nil;
end;

class function TAI.OpenEngine(lib_p: PAI_EntryAPI): TAI;
begin
  Result := TAI.Create;
  Result.FAI_EntryAPI := lib_p;

  if Result.FAI_EntryAPI <> nil then
    if Result.FAI_EntryAPI^.CheckKey() = 0 then
        Result.FAI_EntryAPI := nil;
end;

class function TAI.OpenEngine: TAI;
begin
  Result := TAI.Create;
  Result.FAI_EntryAPI := Load_ZAI(AI_Engine_Library);

  if Result.FAI_EntryAPI <> nil then
    if Result.FAI_EntryAPI^.CheckKey() = 0 then
        Result.FAI_EntryAPI := nil;
end;

destructor TAI.Destroy;
begin
  if FFace_SP_Hnd <> nil then
    begin
      if Pointer(FFace_SP_Hnd) = Pointer(Parallel_SP_Hnd) then
          Parallel_SP_Hnd := nil;
      SP_Close(FFace_SP_Hnd);
    end;
  if Parallel_OD6L_Hnd <> nil then
      OD6L_Close(Parallel_OD6L_Hnd);
  if Parallel_OD3L_Hnd <> nil then
      OD3L_Close(Parallel_OD3L_Hnd);
  if Parallel_OD_Marshal_Hnd <> nil then
      OD6L_Marshal_Close(Parallel_OD_Marshal_Hnd);
  if Parallel_SP_Hnd <> nil then
      SP_Close(Parallel_SP_Hnd);

  DisposeObject(Critical);
  inherited Destroy;
end;

function TAI.Activted: Boolean;
begin
  Result := FAI_EntryAPI <> nil;
end;

function TAI.isGPU: Boolean;
begin
  Result := (FAI_EntryAPI <> nil) and (FAI_EntryAPI^.CUDA = 1);
end;

function TAI.isMKL: Boolean;
begin
  Result := (FAI_EntryAPI <> nil) and (FAI_EntryAPI^.MKL = 1);
end;

function TAI.isTrainer: Boolean;
begin
  Result := (FAI_EntryAPI <> nil) and (FAI_EntryAPI^.Training = 1);
end;

procedure TAI.SetComputeDeviceOfTraining(const Device_: TLIVec);
var
  i: Integer;
begin
  if (FAI_EntryAPI = nil) then
      exit;
  for i := Low(FAI_EntryAPI^.ComputeDeviceOfTraining) to High(FAI_EntryAPI^.ComputeDeviceOfTraining) do
      FAI_EntryAPI^.ComputeDeviceOfTraining[i] := -1;
  try
    for i := Low(FAI_EntryAPI^.ComputeDeviceOfTraining) to umlMin(High(Device_), High(FAI_EntryAPI^.ComputeDeviceOfTraining)) do
      begin
        FAI_EntryAPI^.ComputeDeviceOfTraining[i] := Device_[i];
        if isGPU then
            DoStatus('Activted GPU Device: %d - "%s"', [Device_[i], GetComputeDeviceNameOfProcess(Device_[i]).Text])
        else
            DoStatus('Activted Compute Device [%d]', [Device_[i]]);
      end;
  except
  end;
end;

procedure TAI.GetComputeDeviceOfTraining(var Device_: TLIVec);
var
  L: TListInteger;
  i: Integer;
begin
  L := TIntegerList.Create;
  for i := Low(FAI_EntryAPI^.ComputeDeviceOfTraining) to High(FAI_EntryAPI^.ComputeDeviceOfTraining) do
    if FAI_EntryAPI^.ComputeDeviceOfTraining[i] >= 0 then
        L.Add(FAI_EntryAPI^.ComputeDeviceOfTraining[i]);
  SetLength(Device_, L.Count);
  for i := 0 to L.Count - 1 do
      Device_[i] := L[i];
  DisposeObject(L);
end;

function TAI.SetComputeDeviceOfProcess(device_id: Integer): Boolean;
begin
  Result := False;
  if (FAI_EntryAPI = nil) then
      exit;
  Result := FAI_EntryAPI^.SetComputeDeviceOfProcess(device_id) = 0;
  if Result then
    begin
      if isGPU then
          DoStatus('Current GPU Device [%d] - "%s"', [device_id, GetComputeDeviceNameOfProcess(device_id).Text])
      else
          DoStatus('Current Compute Device [%d]', [device_id]);
    end;
end;

function TAI.GetComputeDeviceOfProcess: Integer;
begin
  Result := -1;
  if (FAI_EntryAPI = nil) then
      exit;
  Result := FAI_EntryAPI^.GetComputeDeviceOfProcess();
end;

function TAI.GetComputeDeviceNumOfProcess: Integer;
begin
  Result := -1;
  if (FAI_EntryAPI = nil) then
      exit;
  Result := FAI_EntryAPI^.GetComputeDeviceNumOfProcess();
end;

function TAI.GetComputeDeviceNameOfProcess(device_id: Integer): U_String;
var
  p: Pointer;
begin
  Result := '';
  if (FAI_EntryAPI = nil) then
      exit;
  p := FAI_EntryAPI^.GetComputeDeviceNameOfProcess(device_id);
  if p = nil then
      exit;
  Result := PPascalString(p)^;
  API_FreeString(p);
end;

function TAI.GetComputeDeviceNames(): U_StringArray;
var
  i, Num: Integer;
begin
  SetLength(Result, 0);
  Num := GetComputeDeviceNumOfProcess;
  if Num > 0 then
    begin
      SetLength(Result, Num);
      for i := 0 to Num - 1 do
          Result[i] := GetComputeDeviceNameOfProcess(i);
    end;
end;

procedure TAI.GetComputeDeviceNames(output: TCoreClassStrings);
var
  i, Num: Integer;
begin
  output.Clear;
  Num := GetComputeDeviceNumOfProcess;
  if Num > 0 then
    begin
      for i := 0 to Num - 1 do
          output.Add(GetComputeDeviceNameOfProcess(i));
    end;
end;

function TAI.MakeSerializedFileName: U_String;
begin
  repeat
      Result := umlCombineFileName(RootPath, umlMakeRanName + '.dat');
  until not umlFileExists(Result);
end;

procedure TAI.Lock;
begin
  Critical.Acquire;
end;

procedure TAI.UnLock;
begin
  Critical.Release;
end;

function TAI.Busy: Boolean;
begin
  Result := Critical.Busy;
end;

procedure TAI.Training_Stop;
begin
  TrainingControl.stop := MaxInt;
end;

procedure TAI.Training_Pause;
begin
  TrainingControl.pause := MaxInt;
end;

procedure TAI.Training_Continue;
begin
  TrainingControl.pause := 0;
end;

function TAI.Training_IsPause: Boolean;
begin
  Result := TrainingControl.pause <> 0;
end;

procedure TAI.DrawOD6L(OD_Hnd: TOD6L_Handle; Raster: TMemoryRaster; color: TDEColor);
var
  OD_Desc: TOD_Desc;
  i: Integer;
  d: TDrawEngine;
  dt: TTimeTick;
begin
  dt := GetTimeTick();
  OD_Desc := OD6L_Process(OD_Hnd, Raster, 8192);
  dt := GetTimeTick() - dt;
  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);
  d.Rasterization.UsedAgg := True;
  for i := 0 to Length(OD_Desc) - 1 do
      d.DrawLabelBox(PFormat('%f', [OD_Desc[i].confidence]), 20, DEColor(1, 1, 1, 1), RectV2(OD_Desc[i]), color, 2);
  d.Flush;
  DisposeObject(d);
end;

procedure TAI.DrawOD3L(OD_Hnd: TOD3L_Handle; Raster: TMemoryRaster; color: TDEColor);
var
  OD_Desc: TOD_Desc;
  i: Integer;
  d: TDrawEngine;
  dt: TTimeTick;
begin
  dt := GetTimeTick();
  OD_Desc := OD3L_Process(OD_Hnd, Raster, 8192);
  dt := GetTimeTick() - dt;
  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);
  d.Rasterization.UsedAgg := True;
  for i := 0 to Length(OD_Desc) - 1 do
      d.DrawLabelBox(PFormat('%f', [OD_Desc[i].confidence]), 20, DEColor(1, 1, 1, 1), RectV2(OD_Desc[i]), color, 2);
  d.Flush;
  DisposeObject(d);
end;

procedure TAI.DrawOD(OD_Desc: TOD_Desc; Raster: TMemoryRaster; color: TDEColor);
var
  i: Integer;
  d: TDrawEngine;
begin
  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);
  d.Rasterization.UsedAgg := True;
  for i := 0 to Length(OD_Desc) - 1 do
      d.DrawLabelBox(PFormat('%f', [OD_Desc[i].confidence]), 20, DEColor(1, 1, 1, 1), RectV2(OD_Desc[i]), color, 2);
  d.Flush;
  DisposeObject(d);
end;

procedure TAI.DrawODM(odm_hnd: TOD6L_Marshal_Handle; Raster: TMemoryRaster; color: TDEColor);
var
  odm_desc: TOD_Marshal_Desc;
  i: Integer;
  d: TDrawEngine;
  dt: TTimeTick;
begin
  dt := GetTimeTick();
  odm_desc := OD6L_Marshal_Process(odm_hnd, Raster);
  dt := GetTimeTick() - dt;
  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);
  d.Rasterization.UsedAgg := True;
  for i := 0 to Length(odm_desc) - 1 do
    begin
      d.DrawLabelBox(PFormat('%s-%f', [odm_desc[i].Token.Text, odm_desc[i].confidence]), 20, DEColor(1, 1, 1, 1), odm_desc[i].R, color, 2);
    end;
  d.Flush;
  DisposeObject(d);
end;

procedure TAI.DrawSP(OD_Hnd: TOD6L_Handle; sp_hnd: TSP_Handle; Raster: TMemoryRaster);
var
  OD_Desc: TOD_Desc;
  sp_desc: TSP_Desc;
  i, j: Integer;
  d: TDrawEngine;
begin
  OD_Desc := OD6L_Process(OD_Hnd, Raster, 8192);
  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);
  d.Rasterization.UsedAgg := True;
  for i := 0 to Length(OD_Desc) - 1 do
    begin
      d.DrawBox(RectV2(OD_Desc[i]), DEColor(1, 0, 0, 0.9), 2);
      sp_desc := SP_Process(sp_hnd, Raster, AIRect(OD_Desc[i]), 1024);
      for j := 0 to Length(sp_desc) - 1 do
          d.DrawPoint(Vec2(sp_desc[j]), DEColor(1, 0, 0, 0.9), 4, 2);
    end;
  d.Flush;
  DisposeObject(d);
end;

function TAI.DrawMMOD(MMOD_hnd: TMMOD6L_Handle; Raster: TMemoryRaster; color: TDEColor; fontSiz: TGeoFloat): TMMOD_Desc;
var
  MMOD_Desc: TMMOD_Desc;
  i: Integer;
  d: TDrawEngine;
  dt: TTimeTick;
  n: U_String;
begin
  dt := GetTimeTick();
  MMOD_Desc := MMOD6L_DNN_Process(MMOD_hnd, Raster);
  dt := GetTimeTick() - dt;
  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);
  d.Rasterization.UsedAgg := True;
  for i := 0 to Length(MMOD_Desc) - 1 do
    begin
      if MMOD_Desc[i].Token.Len > 0 then
          n := PFormat('%s|alpha:0.5| %f', [MMOD_Desc[i].Token.Text, MMOD_Desc[i].confidence])
      else
          n := PFormat('%f', [MMOD_Desc[i].confidence]);

      d.DrawLabelBox(n, fontSiz, DEColor(1, 1, 1, 1), MMOD_Desc[i].R, color, 4);
    end;
  d.Flush;
  DisposeObject(d);
  Result := MMOD_Desc;
end;

function TAI.DrawMMOD(MMOD_hnd: TMMOD6L_Handle; Raster: TMemoryRaster; color: TDEColor): TMMOD_Desc;
begin
  Result := DrawMMOD(MMOD_hnd, Raster, color, 14);
end;

function TAI.DrawMMOD(MMOD_hnd: TMMOD6L_Handle; confidence: Double; Raster: TMemoryRaster; color: TDEColor; fontSiz: TGeoFloat): Integer;
var
  MMOD_Desc: TMMOD_Desc;
  i: Integer;
  d: TDrawEngine;
  dt: TTimeTick;
  n: U_String;
begin
  dt := GetTimeTick();
  MMOD_Desc := MMOD6L_DNN_Process(MMOD_hnd, Raster);
  dt := GetTimeTick() - dt;
  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);
  d.Rasterization.UsedAgg := True;
  Result := 0;
  for i := 0 to Length(MMOD_Desc) - 1 do
    begin
      if confidence < abs(MMOD_Desc[i].confidence) then
        begin
          if MMOD_Desc[i].Token.Len > 0 then
              n := PFormat('%s-%f', [MMOD_Desc[i].Token.Text, MMOD_Desc[i].confidence])
          else
              n := PFormat('%f', [MMOD_Desc[i].confidence]);

          d.DrawLabelBox(n, fontSiz, DEColor(1, 1, 1, 1), MMOD_Desc[i].R, color, 4);
          inc(Result);
        end;
    end;
  d.Flush;
  DisposeObject(d);
end;

function TAI.DrawMMOD(MMOD_hnd: TMMOD6L_Handle; confidence: Double; Raster: TMemoryRaster; color: TDEColor): Integer;
begin
  Result := DrawMMOD(MMOD_hnd, confidence, Raster, color, 16);
end;

function TAI.DrawMMOD(MMOD_Desc: TMMOD_Desc; Raster: TMemoryRaster; color: TDEColor): Integer;
var
  i: Integer;
  d: TDrawEngine;
  n: U_String;
begin
  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);
  d.Rasterization.UsedAgg := True;
  Result := 0;
  for i := 0 to Length(MMOD_Desc) - 1 do
    begin
      n := PFormat('%f', [MMOD_Desc[i].confidence]);
      d.DrawLabelBox(n, 20, DEColor(RColorInv(RColor(color))), MMOD_Desc[i].R, color, 4);
      inc(Result);
    end;
  d.Flush;
  DisposeObject(d);
end;

procedure TAI.DrawFace(Raster: TMemoryRaster);
var
  face_hnd: TFACE_Handle;
  d: TDrawEngine;
  i: Integer;
  sp_desc: TSP_Desc;
begin
  face_hnd := Face_Detector_All(Raster, 0);
  if face_hnd = nil then
      exit;

  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);
  d.Rasterization.UsedAgg := True;

  for i := 0 to Face_Shape_num(face_hnd) - 1 do
    begin
      sp_desc := Face_Shape(face_hnd, i);
      DrawFaceSP(sp_desc, DEColor(1, 0, 0, 0.5), d);
      d.DrawBox(GetSPBound(sp_desc, 0.01), DEColor(1, 0, 0, 0.9), 2);
    end;

  d.Flush;
  DisposeObject(d);
  Face_Close(face_hnd);
end;

procedure TAI.DrawFace(face_hnd: TFACE_Handle; d: TDrawEngine);
var
  i: Integer;
  sp_desc: TSP_Desc;
begin
  for i := 0 to Face_Shape_num(face_hnd) - 1 do
    begin
      sp_desc := Face_Shape(face_hnd, i);
      DrawFaceSP(sp_desc, DEColor(1, 0, 0, 0.5), d);
      d.DrawBox(GetSPBound(sp_desc, 0.01), DEColor(1, 0, 0, 0.9), 2);
    end;
end;

procedure TAI.DrawFace(face_hnd: TFACE_Handle; d: TDrawEngine; sourBox, destBox: TRectV2);
var
  i, j: Integer;
  sp_desc: TSP_Desc;
begin
  for i := 0 to Face_Shape_num(face_hnd) - 1 do
    begin
      sp_desc := Face_Shape(face_hnd, i);
      for j := Low(sp_desc) to high(sp_desc) do
          sp_desc[j] := AI_Point(RectProjection(sourBox, destBox, Vec2(sp_desc[j])));
      DrawFaceSP(sp_desc, DEColor(1, 0, 0, 0.5), d);
      d.DrawBox(GetSPBound(sp_desc, 0.01), DEColor(1, 0, 0, 0.9), 2);
    end;
end;

procedure TAI.DrawFace(Raster: TMemoryRaster; Metric_hnd: TMetric_Handle; Face_Learn: TLearn; faceAccuracy: TGeoFloat; lineColor, TextColor: TDEColor);
var
  face_hnd: TFACE_Handle;
  d: TDrawEngine;
  i: Integer;
  sp_desc: TSP_Desc;
  chip_img: TMemoryRaster;
  face_vec: TLVec;
  LIndex: TLInt;
  p: PLearnMemory;
  k: TLFloat;
  face_lab: SystemString;
  n: U_String;
begin
  face_hnd := Face_Detector_All(Raster);
  if face_hnd = nil then
      exit;

  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);
  d.Rasterization.UsedAgg := True;

  for i := 0 to Face_Shape_num(face_hnd) - 1 do
    begin
      sp_desc := Face_Shape(face_hnd, i);

      chip_img := Face_chips(face_hnd, i);
      face_vec := Metric_ResNet_Process(Metric_hnd, chip_img);
      DisposeObject(chip_img);

      LIndex := Face_Learn.ProcessMaxIndex(face_vec);
      p := Face_Learn.MemorySource[LIndex];
      k := LDistance(face_vec, p^.m_in);
      face_lab := p^.Token;
      SetLength(face_vec, 0);

      if k <= faceAccuracy then
        begin
          DrawFaceSP(sp_desc, DEColor(lineColor, 0.5), d);
          n := PFormat('%s-%f', [face_lab, 1.0 - k]);
          DoStatus(PFormat('%s-%f', [face_lab, 1.0 - k]));
        end
      else
        begin
          n := 'no face defined.';
          DoStatus('no face defined.');
        end;

      d.DrawLabelBox(n, 20, TextColor, GetSPBound(sp_desc, 0.01), lineColor, 4);
    end;

  d.Flush;
  DisposeObject(d);
  Face_Close(face_hnd);
end;

procedure TAI.PrintFace(prefix: SystemString; Raster: TMemoryRaster; Metric_hnd: TMetric_Handle; Face_Learn: TLearn; faceAccuracy: TGeoFloat);
var
  face_hnd: TFACE_Handle;
  i: Integer;
  sp_desc: TSP_Desc;
  chip_img: TMemoryRaster;
  face_vec: TLVec;
  LIndex: TLInt;
  p: PLearnMemory;
  k: TLFloat;
  face_lab: SystemString;
begin
  face_hnd := Face_Detector_All(Raster);
  if face_hnd = nil then
      exit;

  for i := 0 to Face_Shape_num(face_hnd) - 1 do
    begin
      sp_desc := Face_Shape(face_hnd, i);

      chip_img := Face_chips(face_hnd, i);
      face_vec := Metric_ResNet_Process(Metric_hnd, chip_img);
      DisposeObject(chip_img);

      LIndex := Face_Learn.ProcessMaxIndex(face_vec);
      p := Face_Learn.MemorySource[LIndex];
      k := LDistance(face_vec, p^.m_in);
      face_lab := p^.Token;
      SetLength(face_vec, 0);

      if k <= faceAccuracy then
        begin
          DoStatus(prefix + ' ' + PFormat('%s-%f', [face_lab, 1.0 - k]));
        end
      else
        begin
          DoStatus(prefix + ' ' + 'no face defined.');
        end;
    end;
  Face_Close(face_hnd);
end;

function TAI.DrawExtractFace(Raster: TMemoryRaster): TMemoryRaster;
var
  face_hnd: TFACE_Handle;
  i: Integer;
  rp: TRectPacking;
  mr: TMemoryRaster;
  d: TDrawEngine;
begin
  Result := nil;
  face_hnd := Face_Detector_All(Raster);
  if face_hnd = nil then
      exit;

  rp := TRectPacking.Create;
  rp.Margins := 10;
  for i := 0 to Face_chips_num(face_hnd) - 1 do
    begin
      mr := Face_chips(face_hnd, i);
      rp.Add(nil, mr, mr.BoundsRectV2);
    end;
  Face_Close(face_hnd);
  rp.Build;

  d := TDrawEngine.Create;
  d.ViewOptions := [];
  Result := NewRaster();
  Result.SetSize(Round(rp.MaxWidth), Round(rp.MaxHeight));

  d.Rasterization.SetWorkMemory(Result);
  d.FillBox(d.ScreenRect, DEColor(1, 1, 1, 1));

  for i := 0 to rp.Count - 1 do
    begin
      mr := rp[i]^.Data2 as TMemoryRaster;
      d.DrawPicture(mr, mr.BoundsRectV2, rp[i]^.Rect, 0, 1.0);
    end;

  d.Flush;
  DisposeObject(d);

  for i := 0 to rp.Count - 1 do
      DisposeObject(rp[i]^.Data2);
  DisposeObject(rp);
end;

function TAI.Prepare_RGB_Image(Raster: TMemoryRaster): TRGB_Image_Handle;
begin
  Raster.ReadyBits();
  Result := nil;
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.Prepare_RGB_Image) then
      Result := FAI_EntryAPI^.Prepare_RGB_Image(Raster.Bits, Raster.Width, Raster.Height);
end;

procedure TAI.Close_RGB_Image(hnd: TRGB_Image_Handle);
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.Close_RGB_Image) then
      FAI_EntryAPI^.Close_RGB_Image(hnd);
end;

function TAI.Prepare_Matrix_Image(Raster: TMemoryRaster): TMatrix_Image_Handle;
begin
  Raster.ReadyBits();
  Result := nil;
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.Prepare_Matrix_Image) then
      Result := FAI_EntryAPI^.Prepare_Matrix_Image(Raster.Bits, Raster.Width, Raster.Height);
end;

procedure TAI.Close_Matrix_Image(hnd: TMatrix_Image_Handle);
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.Close_Matrix_Image) then
      FAI_EntryAPI^.Close_Matrix_Image(hnd);
end;

procedure TAI.BuildRGBRaster(hnd_RGB: TRGB_Image_Handle; output: TMemoryRaster);
var
  hnd: TBGRA_Buffer_Handle;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OpenImageBuffer_RGB) then
    begin
      hnd := FAI_EntryAPI^.OpenImageBuffer_RGB(hnd_RGB);
      if hnd <> nil then
        begin
          output.SetSize(hnd^.Width, hnd^.Height);
          CopyPtr(hnd^.Bits, output.Bits, (hnd^.Width * hnd^.Height) shl 2);
          FAI_EntryAPI^.CloseImageBuffer(hnd);
        end;
    end;
end;

function TAI.BuildRGBRaster(hnd_RGB: TRGB_Image_Handle): TMemoryRaster;
var
  hnd: TBGRA_Buffer_Handle;
begin
  Result := nil;
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OpenImageBuffer_RGB) then
    begin
      hnd := FAI_EntryAPI^.OpenImageBuffer_RGB(hnd_RGB);
      if hnd <> nil then
        begin
          Result := NewRaster();
          Result.SetSize(hnd^.Width, hnd^.Height);
          CopyPtr(hnd^.Bits, Result.Bits, (hnd^.Width * hnd^.Height) shl 2);
          FAI_EntryAPI^.CloseImageBuffer(hnd);
        end;
    end;
end;

procedure TAI.BuildMatrixRaster(hnd_Matrix: TMatrix_Image_Handle; output: TMemoryRaster);
var
  hnd: TBGRA_Buffer_Handle;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OpenImageBuffer_Matrix) then
    begin
      hnd := FAI_EntryAPI^.OpenImageBuffer_Matrix(hnd_Matrix);
      if hnd <> nil then
        begin
          output.SetSize(hnd^.Width, hnd^.Height);
          CopyPtr(hnd^.Bits, output.Bits, (hnd^.Width * hnd^.Height) shl 2);
          FAI_EntryAPI^.CloseImageBuffer(hnd);
        end;
    end;
end;

function TAI.BuildMatrixRaster(hnd_Matrix: TMatrix_Image_Handle): TMemoryRaster;
var
  hnd: TBGRA_Buffer_Handle;
begin
  Result := nil;
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OpenImageBuffer_Matrix) then
    begin
      hnd := FAI_EntryAPI^.OpenImageBuffer_Matrix(hnd_Matrix);
      if hnd <> nil then
        begin
          Result := NewRaster();
          Result.SetSize(hnd^.Width, hnd^.Height);
          CopyPtr(hnd^.Bits, Result.Bits, (hnd^.Width * hnd^.Height) shl 2);
          FAI_EntryAPI^.CloseImageBuffer(hnd);
        end;
    end;
end;

procedure TAI.HotMap(Raster: TMemoryRaster);
var
  hnd: TBGRA_Buffer_Handle;
begin
  Raster.ReadyBits();
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OpenImageBuffer_Hot) then
    begin
      hnd := FAI_EntryAPI^.OpenImageBuffer_Hot(Raster.Bits, Raster.Width, Raster.Height);
      if hnd <> nil then
        begin
          CopyPtr(hnd^.Bits, Raster.Bits, (hnd^.Width * hnd^.Height) shl 2);
          FAI_EntryAPI^.CloseImageBuffer(hnd);
        end;
    end;
end;

procedure TAI.JetMap(Raster: TMemoryRaster);
var
  hnd: TBGRA_Buffer_Handle;
begin
  Raster.ReadyBits();
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OpenImageBuffer_Jet) then
    begin
      hnd := FAI_EntryAPI^.OpenImageBuffer_Jet(Raster.Bits, Raster.Width, Raster.Height);
      if hnd <> nil then
        begin
          CopyPtr(hnd^.Bits, Raster.Bits, (hnd^.Width * hnd^.Height) shl 2);
          FAI_EntryAPI^.CloseImageBuffer(hnd);
        end;
    end;
end;

function TAI.BuildHotMap(Raster: TMemoryRaster): TMemoryRaster;
var
  hnd: TBGRA_Buffer_Handle;
begin
  Raster.ReadyBits();
  Result := nil;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OpenImageBuffer_Hot) then
    begin
      hnd := FAI_EntryAPI^.OpenImageBuffer_Hot(Raster.Bits, Raster.Width, Raster.Height);
      if hnd <> nil then
        begin
          Result := NewRaster();
          Result.SetSize(hnd^.Width, hnd^.Height);
          CopyPtr(hnd^.Bits, Result.Bits, (hnd^.Width * hnd^.Height) shl 2);
          FAI_EntryAPI^.CloseImageBuffer(hnd);
        end;
    end;
end;

function TAI.BuildJetMap(Raster: TMemoryRaster): TMemoryRaster;
var
  hnd: TBGRA_Buffer_Handle;
begin
  Raster.ReadyBits();
  Result := nil;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OpenImageBuffer_Jet) then
    begin
      hnd := FAI_EntryAPI^.OpenImageBuffer_Jet(Raster.Bits, Raster.Width, Raster.Height);
      if hnd <> nil then
        begin
          Result := NewRaster();
          Result.SetSize(hnd^.Width, hnd^.Height);
          CopyPtr(hnd^.Bits, Result.Bits, (hnd^.Width * hnd^.Height) shl 2);
          FAI_EntryAPI^.CloseImageBuffer(hnd);
        end;
    end;
end;

function TAI.Segment(Raster: TMemoryRaster; const k: Double; const min_siz: Integer): TMemoryRaster;
var
  hnd: TBGRA_Buffer_Handle;
begin
  Raster.ReadyBits();
  Result := nil;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.Segment) then
    begin
      hnd := FAI_EntryAPI^.Segment(Raster.Bits, Raster.Width, Raster.Height, k, min_siz);
      if hnd <> nil then
        begin
          Result := NewRaster();
          Result.SetSize(hnd^.Width, hnd^.Height);
          CopyPtr(hnd^.Bits, Result.Bits, (hnd^.Width * hnd^.Height) shl 2);
          FAI_EntryAPI^.CloseImageBuffer(hnd);
        end;
    end;
end;

function TAI.Segment(Raster: TMemoryRaster): TMemoryRaster;
begin
  Result := Segment(Raster, 5000, 50);
end;

function TAI.Salient(Raster: TMemoryRaster; const iterations: Integer): TMemoryRaster;
var
  hnd: TBGRA_Buffer_Handle;
begin
  Raster.ReadyBits();
  Result := nil;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.Salient) then
    begin
      hnd := FAI_EntryAPI^.Salient(Raster.Bits, Raster.Width, Raster.Height, iterations);
      if hnd <> nil then
        begin
          Result := NewRaster();
          Result.SetSize(hnd^.Width, hnd^.Height);
          CopyPtr(hnd^.Bits, Result.Bits, (hnd^.Width * hnd^.Height) shl 2);
          FAI_EntryAPI^.CloseImageBuffer(hnd);
        end;
    end;
end;

function TAI.Salient(Raster: TMemoryRaster): TMemoryRaster;
begin
  Result := Salient(Raster, 100);
end;

function TAI.CandidateObject(Raster: TMemoryRaster; const min_size, max_merging_iterations: Integer): TAI_Rect_Desc;
var
  n: Integer;
begin
  Raster.ReadyBits();
  SetLength(Result, 0);

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.CandidateObject) then
    begin
      SetLength(Result, $FFFF);
      n := FAI_EntryAPI^.CandidateObject(Raster.Bits, Raster.Width, Raster.Height, min_size, max_merging_iterations, @Result[0], Length(Result));
      SetLength(Result, n);
    end;
end;

function TAI.CandidateObject(Raster: TMemoryRaster): TAI_Rect_Desc;
begin
  Result := CandidateObject(Raster, 50, 5000);
end;

function TAI.Unmixing_Raster(Raster: TMemoryRaster): PUnmixedData;
begin
  Raster.ReadyBits();
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.RasterizeUnmixing) then
    begin
      Result := FAI_EntryAPI^.RasterizeUnmixing(Raster.Bits, Raster.Width, Raster.Height);
    end
  else
      Result := nil;
end;

procedure TAI.Unmixing_Free(var data: PUnmixedData);
begin
  if (data <> nil) and (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.FreeUnmixingData) then
    begin
      FAI_EntryAPI^.FreeUnmixingData(data);
      data := nil;
    end;
end;

procedure TAI.PoissonBlend(GAMMA: Double; sour, dest: TMemoryRaster; dest_x, dest_y: Integer; PaperMethod: Boolean);
var
  paper: Integer;
  mm: TMorphMath;
  bin: TMorphBin;
begin
  sour.ReadyBits();
  dest.ReadyBits();
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.poisson_blend) then
    begin
      if PaperMethod then
          paper := 1
      else
          paper := -1;

      mm := sour.BuildMorphomatics(TMorphPixel.mpA);
      bin := mm.Binarization(0.1);
      DisposeObject(mm);
      bin.DrawTo(TMorphPixel.mpA, sour);
      DisposeObject(bin);

      try
          FAI_EntryAPI^.poisson_blend(GAMMA,
          sour.Bits, sour.Width, sour.Height,
          dest.Bits, dest.Width, dest.Height,
          dest_x, dest_y, paper);
      except
      end;
    end;
end;

function TAI.CutRaster(Raster: TMemoryRaster; inputData: PCutData; box: TRect; iterCount, mode: Integer): TMorphologyBinaryzation;
var
  L, i: Integer;
  dataIO: array of TCutDataType;
  R: TAI_Rect;
  bin: TMorphologyBinaryzation;
  p: PBinaryzationBits;
begin
  Raster.ReadyBits();
  L := Raster.Width * Raster.Height;
  SetLength(dataIO, L);
  CopyPtr(@inputData^[0], @dataIO[0], L);

  { calibrate box }
  R := AIRect(ForwardRect(box));
  R.Left := umlClamp(R.Left, 0, Raster.Width0i);
  R.Top := umlClamp(R.Top, 0, Raster.Height0i);
  R.Right := umlClamp(R.Right, 0, Raster.Width0i);
  R.Bottom := umlClamp(R.Bottom, 0, Raster.Height0i);

  try
      FAI_EntryAPI^.CutRaster(Raster.Bits, @dataIO[0], Raster.Width, Raster.Height, R, umlMax(1, iterCount), mode);
  except
  end;

  bin := TMorphologyBinaryzation.Create;
  bin.SetSize(Raster.Width, Raster.Height);
  p := bin.Bits;
  for i := 0 to L - 1 do
      p^[i] := dataIO[i] = C_CUT_PR_FGD;
  SetLength(dataIO, 0);
  Result := bin;
end;

function TAI.BuildCutConvolutionGeometry(Raster: TMemoryRaster; box: TRect;
  Remove_Noise, Convolutionsiz: Integer; binOperation: TBinaryzationOperation; vetex_reduce: TGeoFloat): T2DPolygonGraph;
var
  L: NativeInt;
  i, j, w: Integer;
  cutData: PCutData;
  tmpFiller: TCutDataLineProcessor;

  bin: TMorphBin;
  seg: TMorphSeg;
  convBin: TMorphBin;
begin
  L := SizeOf(TCutDataType) * Raster.Width * Raster.Height;

  cutData := GetMemory(L);
  FillPtr(cutData, L, zAI.C_CUT_BGD);

  tmpFiller := TCutDataLineProcessor.Create(cutData, Raster.Width, Raster.Height, C_CUT_PR_FGD, True);
  tmpFiller.FillBox(box.Left, box.Top, box.Right, box.Bottom);
  DisposeObject(tmpFiller);

  bin := CutRaster(Raster, cutData, box, 1, zAI.C_CUT_MODE_INIT_WITH_RECT);
  seg := bin.BuildMorphologySegmentation();
  DisposeObject(bin);
  FreeMemory(cutData);

  seg.RemoveNoise(Remove_Noise);
  convBin := TMorphBin.Create;
  convBin.SetSize(Convolutionsiz, Convolutionsiz, True);
  if seg.Count > 0 then
      Result := seg[0].BuildConvolutionGeometry(vetex_reduce, binOperation, convBin)
  else
      Result := nil;

  DisposeObject(seg);
  DisposeObject(convBin);
end;

function TAI.BuildCutGeometry(Raster: TMemoryRaster; box: TRect; Remove_Noise: Integer; vetex_reduce: TGeoFloat): T2DPolygonGraph;
var
  L: NativeInt;
  i, j, w: Integer;
  cutData: PCutData;
  tmpFiller: TCutDataLineProcessor;

  bin: TMorphBin;
  seg: TMorphSeg;
begin
  L := SizeOf(TCutDataType) * Raster.Width * Raster.Height;

  cutData := GetMemory(L);
  FillPtr(cutData, L, zAI.C_CUT_BGD);

  tmpFiller := TCutDataLineProcessor.Create(cutData, Raster.Width, Raster.Height, C_CUT_PR_FGD, True);
  tmpFiller.FillBox(box.Left, box.Top, box.Right, box.Bottom);
  DisposeObject(tmpFiller);

  bin := CutRaster(Raster, cutData, box, 1, zAI.C_CUT_MODE_INIT_WITH_RECT);
  seg := bin.BuildMorphologySegmentation();
  DisposeObject(bin);
  FreeMemory(cutData);

  seg.RemoveNoise(Remove_Noise);
  if seg.Count > 0 then
      Result := seg[0].BuildGeometry(vetex_reduce)
  else
      Result := nil;

  DisposeObject(seg);
end;

function TAI.fast_surf(Raster: TMemoryRaster; const max_points: Integer; const detection_threshold: Double): TSurf_DescBuffer;
begin
  Raster.ReadyBits();
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.fast_surf) then
    begin
      SetLength(Result, max_points);
      SetLength(Result, FAI_EntryAPI^.fast_surf(Raster.Bits, Raster.Width, Raster.Height, max_points, detection_threshold, @Result[0]));
    end
  else
      SetLength(Result, 0);
end;

function TAI.surf_sqr(const sour, dest: PSurf_Desc): Single;
var
  f128: TGFloat_4x;
begin
  f128 := sqr_128(@sour^.desc[0], @dest^.desc[0]);
  Result := f128[0] + f128[1] + f128[2] + f128[3];
  f128 := sqr_128(@sour^.desc[32], @dest^.desc[32]);
  Result := Result + f128[0] + f128[1] + f128[2] + f128[3];
end;

function TAI.Surf_Matched(reject_ratio_sqr: Single; r1_, r2_: TMemoryRaster; sd1_, sd2_: TSurf_DescBuffer): TSurfMatchedBuffer;
var
  sd1_len, sd2_len: Integer;
  SD1, SD2: TSurf_DescBuffer;
  R1, R2: TMemoryRaster;
  L: TCoreClassList;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    m_idx, j: Integer;
    minf, next_minf: Single;
    d: Single;
    p: PSurfMatched;
  begin
    m_idx := -1;
    minf := 3.4E+38;
    next_minf := minf;

    { find dsc1 from feat2 }
    for j := 0 to sd2_len - 1 do
      begin
        d := Min(surf_sqr(@SD1[pass], @SD2[j]), next_minf);
        if (d < minf) then
          begin
            next_minf := minf;
            minf := d;
            m_idx := j;
          end
        else
            next_minf := Min(next_minf, d);
      end;

    { bidirectional rejection }
    if (minf > reject_ratio_sqr * next_minf) then
        exit;

    { fix m_idx }
    for j := 0 to sd1_len - 1 do
      if j <> pass then
        begin
          d := Min(surf_sqr(@SD1[j], @SD2[m_idx]), next_minf);
          next_minf := Min(next_minf, d);
        end;

    { bidirectional rejection }
    if (minf > reject_ratio_sqr * next_minf) then
        exit;

    new(p);
    p^.SD1 := @SD1[pass];
    p^.SD2 := @SD2[m_idx];
    p^.R1 := R1;
    p^.R2 := R2;
    LockObject(L);
    L.Add(p);
    UnLockObject(L);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass: Integer;
    m_idx, j: Integer;
    minf, next_minf: Single;
    d: Single;
    p: PSurfMatched;
  begin
    for pass := 0 to sd1_len - 1 do
      begin
        m_idx := -1;
        minf := 3.4E+38;
        next_minf := minf;

        { find dsc1 from feat2 }
        for j := 0 to sd2_len - 1 do
          begin
            d := Min(surf_sqr(@SD1[pass], @SD2[j]), next_minf);
            if (d < minf) then
              begin
                next_minf := minf;
                minf := d;
                m_idx := j;
              end
            else
                next_minf := Min(next_minf, d);
          end;

        { bidirectional rejection }
        if (minf > reject_ratio_sqr * next_minf) then
            continue;

        { fix m_idx }
        for j := 0 to sd1_len - 1 do
          if j <> pass then
            begin
              d := Min(surf_sqr(@SD1[j], @SD2[m_idx]), next_minf);
              next_minf := Min(next_minf, d);
            end;

        { bidirectional rejection }
        if (minf > reject_ratio_sqr * next_minf) then
            continue;

        new(p);
        p^.SD1 := @SD1[pass];
        p^.SD2 := @SD2[m_idx];
        p^.R1 := R1;
        p^.R2 := R2;
        L.Add(p);
      end;
  end;
{$ENDIF Parallel}
  procedure FillMatchInfoAndFreeTemp;
  var
    i: Integer;
    p: PSurfMatched;
  begin
    SetLength(Result, L.Count);
    for i := 0 to L.Count - 1 do
      begin
        p := PSurfMatched(L[i]);
        Result[i] := p^;
        Dispose(p);
      end;
  end;

begin
  SetLength(Result, 0);
  sd1_len := Length(sd1_);
  sd2_len := Length(sd2_);

  if (sd1_len = 0) or (sd2_len = 0) then
      exit;

  if sd1_len > sd2_len then
    begin
      Swap(sd1_len, sd2_len);
      SD1 := sd2_;
      R1 := r2_;
      SD2 := sd1_;
      R2 := r1_;
    end
  else
    begin
      SD1 := sd1_;
      R1 := r1_;
      SD2 := sd2_;
      R2 := r2_;
    end;

  L := TCoreClassList.Create;
  L.Capacity := sd1_len;

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(@Nested_ParallelFor, 0, sd1_len - 1);
{$ELSE FPC}
  DelphiParallelFor(0, sd1_len - 1, procedure(pass: Integer)
    var
      m_idx, j: Integer;
      minf, next_minf: Single;
      d: Single;
      p: PSurfMatched;
    begin
      m_idx := -1;
      minf := MaxRealNumber;
      next_minf := minf;

      { find dsc1 from feat2 }
      for j := 0 to sd2_len - 1 do
        begin
          d := Min(surf_sqr(@SD1[pass], @SD2[j]), next_minf);
          if (d < minf) then
            begin
              next_minf := minf;
              minf := d;
              m_idx := j;
            end
          else
              next_minf := Min(next_minf, d);
        end;

      { bidirectional rejection }
      if (minf > reject_ratio_sqr * next_minf) then
          exit;

      { fix m_idx }
      for j := 0 to sd1_len - 1 do
        if j <> pass then
          begin
            d := Min(surf_sqr(@SD1[j], @SD2[m_idx]), next_minf);
            next_minf := Min(next_minf, d);
          end;

      { bidirectional rejection }
      if (minf > reject_ratio_sqr * next_minf) then
          exit;

      new(p);
      p^.SD1 := @SD1[pass];
      p^.SD2 := @SD2[m_idx];
      p^.R1 := R1;
      p^.R2 := R2;
      LockObject(L);
      L.Add(p);
      UnLockObject(L);
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor();
{$ENDIF Parallel}
  FillMatchInfoAndFreeTemp;
  DisposeObject(L);
end;

procedure TAI.BuildFeatureView(Raster: TMemoryRaster; descbuff: TSurf_DescBuffer);
var
  i: Integer;
  p: PSurf_Desc;
begin
  Raster.OpenAgg;
  Raster.Agg.LineWidth := 1.0;
  for i := 0 to Length(descbuff) - 1 do
    begin
      p := @descbuff[i];

      Raster.DrawCrossF(p^.X, p^.Y, 4, RColorF(1, 0, 0, 1));
      Raster.LineF(p^.X, p^.Y, p^.DX, p^.DY, RColorF(0, 1, 0, 0.5), True);
    end;
end;

function TAI.BuildMatchInfoView(var MatchInfo: TSurfMatchedBuffer): TMemoryRaster;
var
  mr1, mr2: TMemoryRaster;
  c: Byte;
  i, j: Integer;

  p: PSurfMatched;
  RC: TRColor;
  v1, v2: TVec2;
begin
  if Length(MatchInfo) = 0 then
    begin
      Result := nil;
      exit;
    end;
  Result := NewRaster();

  mr1 := MatchInfo[0].R1;
  mr2 := MatchInfo[0].R2;

  Result.SetSize(mr1.Width + mr2.Width, Max(mr1.Height, mr2.Height), RColor(0, 0, 0, 0));
  Result.Draw(0, 0, mr1);
  Result.Draw(mr1.Width, 0, mr2);
  Result.OpenAgg;

  for i := 0 to Length(MatchInfo) - 1 do
    begin
      p := @MatchInfo[i];
      RC := RColor(RandomRange(0, 255), RandomRange(0, 255), RandomRange(0, 255), $7F);
      v1 := Geometry2DUnit.Vec2(p^.SD1^.X, p^.SD1^.Y);
      v2 := Geometry2DUnit.Vec2(p^.SD2^.X, p^.SD2^.Y);
      v2 := Geometry2DUnit.Vec2Add(v2, Geometry2DUnit.Vec2(mr1.Width, 0));

      Result.LineF(v1, v2, RC, True);
    end;
end;

function TAI.BuildSurfMatchOutput(raster1, raster2: TMemoryRaster): TMemoryRaster;
var
  R1, R2: TMemoryRaster;
  d1, d2: TSurf_DescBuffer;
  matched: TSurfMatchedBuffer;
begin
  R1 := NewRaster();
  R2 := NewRaster();
  R1.Assign(raster1);
  R2.Assign(raster2);
  d1 := fast_surf(R1, 20000, 1.0);
  d2 := fast_surf(R2, 20000, 1.0);
  BuildFeatureView(R1, d1);
  BuildFeatureView(R2, d2);
  matched := Surf_Matched(0.4, R1, R2, d1, d2);
  Result := BuildMatchInfoView(matched);
  DisposeObject([R1, R2]);
  SetLength(matched, 0);
  SetLength(d1, 0);
  SetLength(d2, 0);
end;

function TAI.OD6L_Train(train_cfg, train_output: U_String; window_w, window_h, thread_num: Integer): Boolean;
var
  train_cfg_buff, train_output_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OD6L_Train) and (umlFileExists(train_cfg)) and (train_output.Len > 0) then
    begin
      train_cfg_buff := Alloc_P_Bytes(train_cfg);
      train_output_buff := Alloc_P_Bytes(train_output);

      FAI_EntryAPI^.RasterSerialized := nil;
      FAI_EntryAPI^.SerializedTime := GetTimeTick();

      try
          Result := FAI_EntryAPI^.OD6L_Train(train_cfg_buff, train_output_buff, window_w, window_h, thread_num) = 0;
      except
          Result := False;
      end;

      Free_P_Bytes(train_cfg_buff);
      Free_P_Bytes(train_output_buff);

      if not Result then
          DoStatus('ZAI: Object Detector training failed.');
    end
  else
      Result := False;
end;

function TAI.OD6L_Train(imgList: TAI_ImageList; TokenFilter, train_output: U_String; window_w, window_h, thread_num: Integer): Boolean;
var
  ph, fn, prefix: U_String;
  tmpFileList: TPascalStringList;
  i: Integer;
begin
  ph := RootPath;
  tmpFileList := TPascalStringList.Create;

  TCoreClassThread.Sleep(1);
  prefix := 'temp_OD6L_' + umlMakeRanName + '_';

  fn := umlCombineFileName(ph, prefix + 'temp.xml');
  imgList.Build_XML(TokenFilter, False, False, 'ZAI dataset', 'object detector training dataset', fn, prefix, tmpFileList);

  Result := OD6L_Train(fn, train_output, window_w, window_h, thread_num);

  for i := 0 to tmpFileList.Count - 1 do
      umlDeleteFile(tmpFileList[i]);

  DisposeObject(tmpFileList);
end;

function TAI.OD6L_Train(imgMat: TAI_ImageMatrix; TokenFilter, train_output: U_String; window_w, window_h, thread_num: Integer): Boolean;
var
  ph, fn, prefix: U_String;
  tmpFileList: TPascalStringList;
  i: Integer;
begin
  ph := RootPath;
  tmpFileList := TPascalStringList.Create;

  TCoreClassThread.Sleep(1);
  prefix := 'temp_OD6L_' + umlMakeRanName + '_';

  fn := umlCombineFileName(ph, prefix + 'temp.xml');
  imgMat.Build_XML(TokenFilter, False, False, 'ZAI dataset', 'object detector training dataset', fn, prefix, tmpFileList);

  Result := OD6L_Train(fn, train_output, window_w, window_h, thread_num);

  for i := 0 to tmpFileList.Count - 1 do
      umlDeleteFile(tmpFileList[i]);

  DisposeObject(tmpFileList);
end;

function TAI.OD6L_Train_Stream(imgList: TAI_ImageList; window_w, window_h, thread_num: Integer): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(RootPath, PFormat('temp_OD6L_%s' + C_OD6L_Ext, [umlMakeRanName.Text]));

  if OD6L_Train(imgList, '', fn, window_w, window_h, thread_num) then
    if umlFileExists(fn) then
      begin
        Result := TMemoryStream64.Create;
        Result.LoadFromFile(fn);
        Result.Position := 0;
      end;
  umlDeleteFile(fn);
end;

function TAI.OD6L_Train_Stream(imgMat: TAI_ImageMatrix; window_w, window_h, thread_num: Integer): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(RootPath, PFormat('temp_OD6L_%s' + C_OD6L_Ext, [umlMakeRanName.Text]));

  if OD6L_Train(imgMat, '', fn, window_w, window_h, thread_num) then
    if umlFileExists(fn) then
      begin
        Result := TMemoryStream64.Create;
        Result.LoadFromFile(fn);
        Result.Position := 0;
      end;
  umlDeleteFile(fn);
end;

function TAI.LargeScale_OD6L_Train(imgList: TAI_ImageList; train_output: U_String; window_w, window_h, thread_num: Integer): Boolean;
var
  imgArry: array of TImage_Handle;
  imgArry_P: array of PImage_Handle;
  i: Integer;
  train_output_buff: P_Bytes;
begin
  Result := False;
  SetLength(imgArry, imgList.Count);
  SetLength(imgArry_P, imgList.Count);

  for i := 0 to imgList.Count - 1 do
    begin
      imgArry[i].image := imgList[i];
      imgArry[i].AccessImage := 0;
      imgArry[i].AccessDetectorImage := 0;
      imgArry[i].AccessDetectorRect := 0;
      imgArry[i].AccessMask := 0;
      imgArry_P[i] := @imgArry[i];
    end;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LargeScale_OD6L_Train) then
    begin
      train_output_buff := Alloc_P_Bytes(train_output);
      FAI_EntryAPI^.RasterSerialized := nil;
      FAI_EntryAPI^.SerializedTime := GetTimeTick();
      try
          Result := FAI_EntryAPI^.LargeScale_OD6L_Train(@imgArry_P[0], imgList.Count, train_output_buff, window_w, window_h, thread_num) = 0;
      except
          Result := False;
      end;
      Free_P_Bytes(train_output_buff);
    end;

  SetLength(imgArry, 0);
  SetLength(imgArry_P, 0);
end;

function TAI.LargeScale_OD6L_Train(imgMat: TAI_ImageMatrix; train_output: U_String; window_w, window_h, thread_num: Integer): Boolean;
var
  imgL: TImageList_Decl;
  imgArry: array of TImage_Handle;
  imgArry_P: array of PImage_Handle;
  i: Integer;
  train_output_buff: P_Bytes;
begin
  Result := False;
  imgL := imgMat.ImageList();
  SetLength(imgArry, imgL.Count);
  SetLength(imgArry_P, imgL.Count);

  for i := 0 to imgL.Count - 1 do
    begin
      imgArry[i].image := imgL[i];
      imgArry[i].AccessImage := 0;
      imgArry[i].AccessDetectorImage := 0;
      imgArry[i].AccessDetectorRect := 0;
      imgArry[i].AccessMask := 0;
      imgArry_P[i] := @imgArry[i];
    end;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LargeScale_OD6L_Train) then
    begin
      train_output_buff := Alloc_P_Bytes(train_output);
      try
          Result := FAI_EntryAPI^.LargeScale_OD6L_Train(@imgArry_P[0], imgL.Count, train_output_buff, window_w, window_h, thread_num) = 0;
      except
          Result := False;
      end;
      Free_P_Bytes(train_output_buff);
    end;

  SetLength(imgArry, 0);
  SetLength(imgArry_P, 0);
  DisposeObject(imgL);
end;

function TAI.LargeScale_OD6L_Train_Stream(imgList: TAI_ImageList; window_w, window_h, thread_num: Integer): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(RootPath, PFormat('temp_OD6L_%s' + C_OD6L_Ext, [umlMakeRanName.Text]));

  if LargeScale_OD6L_Train(imgList, fn, window_w, window_h, thread_num) then
    if umlFileExists(fn) then
      begin
        Result := TMemoryStream64.Create;
        Result.LoadFromFile(fn);
        Result.Position := 0;
      end;
  umlDeleteFile(fn);
end;

function TAI.LargeScale_OD6L_Train_Stream(imgMat: TAI_ImageMatrix; window_w, window_h, thread_num: Integer): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(RootPath, PFormat('temp_OD6L_%s' + C_OD6L_Ext, [umlMakeRanName.Text]));

  if LargeScale_OD6L_Train(imgMat, fn, window_w, window_h, thread_num) then
    if umlFileExists(fn) then
      begin
        Result := TMemoryStream64.Create;
        Result.LoadFromFile(fn);
        Result.Position := 0;
      end;
  umlDeleteFile(fn);
end;

function TAI.OD6L_Open(train_file: SystemString): TOD6L_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OD6L_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      try
          Result := FAI_EntryAPI^.OD6L_Init(train_file_buff);
      finally
          Free_P_Bytes(train_file_buff);
      end;
      if Result <> nil then
          DoStatus('Object detector open: %s', [train_file]);
    end
  else
      Result := nil;
end;

function TAI.OD6L_Open_Stream(stream: TMemoryStream64): TOD6L_Handle;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OD6L_Init_Memory) then
    begin
      Result := FAI_EntryAPI^.OD6L_Init_Memory(stream.memory, stream.Size);
      if Result <> nil then
          DoStatus('Object Detector open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.OD6L_Open_Stream(train_file: SystemString): TOD6L_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := OD6L_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('Object detector open: %s', [train_file]);
end;

function TAI.OD6L_Close(var hnd: TOD6L_Handle): Boolean;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OD6L_Free) and (hnd <> nil) then
    begin
      Result := FAI_EntryAPI^.OD6L_Free(hnd) = 0;
      DoStatus('Object detector Close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

function TAI.OD6L_Process(hnd: TOD6L_Handle; Raster: TMemoryRaster; const max_AI_Rect: Integer): TOD_Desc;
var
  rect_num: Integer;
begin
  Raster.ReadyBits();
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.OD6L_Process) then
      exit;
  SetLength(Result, max_AI_Rect);

  try
    if FAI_EntryAPI^.OD6L_Process(hnd, Raster.Bits, Raster.Width, Raster.Height, @Result[0], max_AI_Rect, rect_num) > 0 then
      begin
        SetLength(Result, rect_num);
        FilterOD_Desc(Result);
      end
    else
        SetLength(Result, 0);
  except
      SetLength(Result, 0);
  end;
end;

function TAI.OD6L_Process(hnd: TOD6L_Handle; Raster: TMemoryRaster): TOD_List;
var
  OD_Desc: TOD_Desc;
  i: Integer;
begin
  Result := TOD_List.Create;
  OD_Desc := OD6L_Process(hnd, Raster, 1024);
  for i := Low(OD_Desc) to High(OD_Desc) do
      Result.Add(OD_Desc[i]);
end;

procedure TAI.OD6L_Process(hnd: TOD6L_Handle; Raster: TMemoryRaster; output: TOD_List);
var
  OD_Desc: TOD_Desc;
  i: Integer;
begin
  OD_Desc := OD6L_Process(hnd, Raster, 1024);
  for i := Low(OD_Desc) to High(OD_Desc) do
      output.Add(OD_Desc[i]);
end;

function TAI.OD6L_ProcessRGB(hnd: TOD6L_Handle; rgb_img: TRGB_Image_Handle; const max_AI_Rect: Integer): TOD_Desc;
var
  rect_num: Integer;
begin
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.OD6L_Process_Image) then
      exit;
  SetLength(Result, max_AI_Rect);

  try
    if FAI_EntryAPI^.OD6L_Process_Image(hnd, rgb_img, @Result[0], max_AI_Rect, rect_num) > 0 then
      begin
        SetLength(Result, rect_num);
        FilterOD_Desc(Result);
      end
    else
        SetLength(Result, 0);
  except
      SetLength(Result, 0);
  end;
end;

function TAI.OD6L_ProcessScaleSpace(hnd: TOD6L_Handle; Raster: TMemoryRaster; scale: TGeoFloat): TOD_Desc;
var
  nr: TMemoryRaster;
  buff: TOD_Desc;
  i: Integer;
begin
  Raster.ReadyBits();
  nr := NewRaster();
  nr.ZoomFrom(Raster, scale);

  buff := OD6L_Process(hnd, nr, 1024);

  SetLength(Result, Length(buff));

  for i := 0 to Length(buff) - 1 do
    begin
      Result[i].Left := Round(buff[i].Left / scale);
      Result[i].Top := Round(buff[i].Top / scale);
      Result[i].Right := Round(buff[i].Right / scale);
      Result[i].Bottom := Round(buff[i].Bottom / scale);
      Result[i].confidence := buff[i].confidence;
    end;

  SetLength(buff, 0);
  DisposeObject(nr);
end;

function TAI.OD3L_Train(train_cfg, train_output: U_String; window_w, window_h, thread_num: Integer): Boolean;
var
  train_cfg_buff, train_output_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OD3L_Train) and (umlFileExists(train_cfg)) and (train_output.Len > 0) then
    begin
      train_cfg_buff := Alloc_P_Bytes(train_cfg);
      train_output_buff := Alloc_P_Bytes(train_output);

      FAI_EntryAPI^.RasterSerialized := nil;
      FAI_EntryAPI^.SerializedTime := GetTimeTick();

      try
          Result := FAI_EntryAPI^.OD3L_Train(train_cfg_buff, train_output_buff, window_w, window_h, thread_num) = 0;
      except
          Result := False;
      end;

      Free_P_Bytes(train_cfg_buff);
      Free_P_Bytes(train_output_buff);

      if not Result then
          DoStatus('ZAI: Object Detector training failed.');
    end
  else
      Result := False;
end;

function TAI.OD3L_Train(imgList: TAI_ImageList; TokenFilter, train_output: U_String; window_w, window_h, thread_num: Integer): Boolean;
var
  ph, fn, prefix: U_String;
  tmpFileList: TPascalStringList;
  i: Integer;
begin
  ph := RootPath;
  tmpFileList := TPascalStringList.Create;

  TCoreClassThread.Sleep(1);
  prefix := 'temp_OD3L_' + umlMakeRanName + '_';

  fn := umlCombineFileName(ph, prefix + 'temp.xml');
  imgList.Build_XML(TokenFilter, False, False, 'ZAI dataset', 'object detector training dataset', fn, prefix, tmpFileList);

  Result := OD3L_Train(fn, train_output, window_w, window_h, thread_num);

  for i := 0 to tmpFileList.Count - 1 do
      umlDeleteFile(tmpFileList[i]);

  DisposeObject(tmpFileList);
end;

function TAI.OD3L_Train(imgMat: TAI_ImageMatrix; TokenFilter, train_output: U_String; window_w, window_h, thread_num: Integer): Boolean;
var
  ph, fn, prefix: U_String;
  tmpFileList: TPascalStringList;
  i: Integer;
begin
  ph := RootPath;
  tmpFileList := TPascalStringList.Create;

  TCoreClassThread.Sleep(1);
  prefix := 'temp_OD3L_' + umlMakeRanName + '_';

  fn := umlCombineFileName(ph, prefix + 'temp.xml');
  imgMat.Build_XML(TokenFilter, False, False, 'ZAI dataset', 'object detector training dataset', fn, prefix, tmpFileList);

  Result := OD3L_Train(fn, train_output, window_w, window_h, thread_num);

  for i := 0 to tmpFileList.Count - 1 do
      umlDeleteFile(tmpFileList[i]);

  DisposeObject(tmpFileList);
end;

function TAI.OD3L_Train_Stream(imgList: TAI_ImageList; window_w, window_h, thread_num: Integer): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(RootPath, PFormat('temp_OD3L_%s' + C_OD3L_Ext, [umlMakeRanName.Text]));

  if OD3L_Train(imgList, '', fn, window_w, window_h, thread_num) then
    if umlFileExists(fn) then
      begin
        Result := TMemoryStream64.Create;
        Result.LoadFromFile(fn);
        Result.Position := 0;
      end;
  umlDeleteFile(fn);
end;

function TAI.OD3L_Train_Stream(imgMat: TAI_ImageMatrix; window_w, window_h, thread_num: Integer): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(RootPath, PFormat('temp_OD3L_%s' + C_OD3L_Ext, [umlMakeRanName.Text]));

  if OD3L_Train(imgMat, '', fn, window_w, window_h, thread_num) then
    if umlFileExists(fn) then
      begin
        Result := TMemoryStream64.Create;
        Result.LoadFromFile(fn);
        Result.Position := 0;
      end;
  umlDeleteFile(fn);
end;

function TAI.LargeScale_OD3L_Train(imgList: TAI_ImageList; train_output: U_String; window_w, window_h, thread_num: Integer): Boolean;
var
  imgArry: array of TImage_Handle;
  imgArry_P: array of PImage_Handle;
  i: Integer;
  train_output_buff: P_Bytes;
begin
  Result := False;
  SetLength(imgArry, imgList.Count);
  SetLength(imgArry_P, imgList.Count);

  for i := 0 to imgList.Count - 1 do
    begin
      imgArry[i].image := imgList[i];
      imgArry[i].AccessImage := 0;
      imgArry[i].AccessDetectorImage := 0;
      imgArry[i].AccessDetectorRect := 0;
      imgArry[i].AccessMask := 0;
      imgArry_P[i] := @imgArry[i];
    end;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LargeScale_OD3L_Train) then
    begin
      train_output_buff := Alloc_P_Bytes(train_output);
      FAI_EntryAPI^.RasterSerialized := nil;
      FAI_EntryAPI^.SerializedTime := GetTimeTick();
      try
          Result := FAI_EntryAPI^.LargeScale_OD3L_Train(@imgArry_P[0], imgList.Count, train_output_buff, window_w, window_h, thread_num) = 0;
      except
          Result := False;
      end;
      Free_P_Bytes(train_output_buff);
    end;

  SetLength(imgArry, 0);
  SetLength(imgArry_P, 0);
end;

function TAI.LargeScale_OD3L_Train(imgMat: TAI_ImageMatrix; train_output: U_String; window_w, window_h, thread_num: Integer): Boolean;
var
  imgL: TImageList_Decl;
  imgArry: array of TImage_Handle;
  imgArry_P: array of PImage_Handle;
  i: Integer;
  train_output_buff: P_Bytes;
begin
  Result := False;
  imgL := imgMat.ImageList();
  SetLength(imgArry, imgL.Count);
  SetLength(imgArry_P, imgL.Count);

  for i := 0 to imgL.Count - 1 do
    begin
      imgArry[i].image := imgL[i];
      imgArry[i].AccessImage := 0;
      imgArry[i].AccessDetectorImage := 0;
      imgArry[i].AccessDetectorRect := 0;
      imgArry[i].AccessMask := 0;
      imgArry_P[i] := @imgArry[i];
    end;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LargeScale_OD3L_Train) then
    begin
      train_output_buff := Alloc_P_Bytes(train_output);
      try
          Result := FAI_EntryAPI^.LargeScale_OD3L_Train(@imgArry_P[0], imgL.Count, train_output_buff, window_w, window_h, thread_num) = 0;
      except
          Result := False;
      end;
      Free_P_Bytes(train_output_buff);
    end;

  SetLength(imgArry, 0);
  SetLength(imgArry_P, 0);
  DisposeObject(imgL);
end;

function TAI.LargeScale_OD3L_Train_Stream(imgList: TAI_ImageList; window_w, window_h, thread_num: Integer): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(RootPath, PFormat('temp_OD3L_%s' + C_OD3L_Ext, [umlMakeRanName.Text]));

  if LargeScale_OD3L_Train(imgList, fn, window_w, window_h, thread_num) then
    if umlFileExists(fn) then
      begin
        Result := TMemoryStream64.Create;
        Result.LoadFromFile(fn);
        Result.Position := 0;
      end;
  umlDeleteFile(fn);
end;

function TAI.LargeScale_OD3L_Train_Stream(imgMat: TAI_ImageMatrix; window_w, window_h, thread_num: Integer): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(RootPath, PFormat('temp_OD3L_%s' + C_OD3L_Ext, [umlMakeRanName.Text]));

  if LargeScale_OD3L_Train(imgMat, fn, window_w, window_h, thread_num) then
    if umlFileExists(fn) then
      begin
        Result := TMemoryStream64.Create;
        Result.LoadFromFile(fn);
        Result.Position := 0;
      end;
  umlDeleteFile(fn);
end;

function TAI.OD3L_Open(train_file: SystemString): TOD3L_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OD3L_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      try
          Result := FAI_EntryAPI^.OD3L_Init(train_file_buff);
      finally
          Free_P_Bytes(train_file_buff);
      end;
      if Result <> nil then
          DoStatus('Object detector open: %s', [train_file]);
    end
  else
      Result := nil;
end;

function TAI.OD3L_Open_Stream(stream: TMemoryStream64): TOD3L_Handle;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OD3L_Init_Memory) then
    begin
      Result := FAI_EntryAPI^.OD3L_Init_Memory(stream.memory, stream.Size);
      if Result <> nil then
          DoStatus('Object Detector open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.OD3L_Open_Stream(train_file: SystemString): TOD3L_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := OD3L_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('Object detector open: %s', [train_file]);
end;

function TAI.OD3L_Close(var hnd: TOD3L_Handle): Boolean;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.OD3L_Free) and (hnd <> nil) then
    begin
      Result := FAI_EntryAPI^.OD3L_Free(hnd) = 0;
      DoStatus('Object detector Close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

function TAI.OD3L_Process(hnd: TOD3L_Handle; Raster: TMemoryRaster; const max_AI_Rect: Integer): TOD_Desc;
var
  rect_num: Integer;
begin
  Raster.ReadyBits();
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.OD3L_Process) then
      exit;
  SetLength(Result, max_AI_Rect);

  try
    if FAI_EntryAPI^.OD3L_Process(hnd, Raster.Bits, Raster.Width, Raster.Height, @Result[0], max_AI_Rect, rect_num) > 0 then
      begin
        SetLength(Result, rect_num);
        FilterOD_Desc(Result);
      end
    else
        SetLength(Result, 0);
  except
      SetLength(Result, 0);
  end;
end;

function TAI.OD3L_Process(hnd: TOD3L_Handle; Raster: TMemoryRaster): TOD_List;
var
  OD_Desc: TOD_Desc;
  i: Integer;
begin
  Result := TOD_List.Create;
  OD_Desc := OD3L_Process(hnd, Raster, 1024);
  for i := Low(OD_Desc) to High(OD_Desc) do
      Result.Add(OD_Desc[i]);
end;

procedure TAI.OD3L_Process(hnd: TOD3L_Handle; Raster: TMemoryRaster; output: TOD_List);
var
  OD_Desc: TOD_Desc;
  i: Integer;
begin
  OD_Desc := OD3L_Process(hnd, Raster, 1024);
  for i := Low(OD_Desc) to High(OD_Desc) do
      output.Add(OD_Desc[i]);
end;

function TAI.OD3L_ProcessRGB(hnd: TOD3L_Handle; rgb_img: TRGB_Image_Handle; const max_AI_Rect: Integer): TOD_Desc;
var
  rect_num: Integer;
begin
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.OD3L_Process_Image) then
      exit;
  SetLength(Result, max_AI_Rect);

  try
    if FAI_EntryAPI^.OD3L_Process_Image(hnd, rgb_img, @Result[0], max_AI_Rect, rect_num) > 0 then
      begin
        SetLength(Result, rect_num);
        FilterOD_Desc(Result);
      end
    else
        SetLength(Result, 0);
  except
      SetLength(Result, 0);
  end;
end;

function TAI.OD3L_ProcessScaleSpace(hnd: TOD3L_Handle; Raster: TMemoryRaster; scale: TGeoFloat): TOD_Desc;
var
  nr: TMemoryRaster;
  buff: TOD_Desc;
  i: Integer;
begin
  nr := NewRaster();
  nr.ZoomFrom(Raster, scale);

  buff := OD3L_Process(hnd, nr, 1024);

  SetLength(Result, Length(buff));

  for i := 0 to Length(buff) - 1 do
    begin
      Result[i].Left := Round(buff[i].Left / scale);
      Result[i].Top := Round(buff[i].Top / scale);
      Result[i].Right := Round(buff[i].Right / scale);
      Result[i].Bottom := Round(buff[i].Bottom / scale);
      Result[i].confidence := buff[i].confidence;
    end;

  SetLength(buff, 0);
  DisposeObject(nr);
end;

function TAI.OD6L_Marshal_Train(imgList: TAI_ImageList; window_w, window_h, thread_num: Integer): TMemoryStream64;
var
  dbEng: TObjectDataManager;
  token_arry: TArrayPascalString;
  m64: TMemoryStream64;
  itmHnd: TItemHandle;
  Token: U_String;
  fn: U_String;
begin
  Result := TMemoryStream64.Create;
  dbEng := TObjectDataManagerOfCache.CreateAsStream(Result, '', DBMarshal.ID, False, True, False);

  imgList.CalibrationNullToken('null');

  token_arry := imgList.DetectorTokens;
  for Token in token_arry do
    begin
      TCoreClassThread.Sleep(1);
      fn := umlCombineFileName(RootPath, PFormat('temp_OD_%s' + C_OD6L_Ext, [umlMakeRanName.Text]));

      if OD6L_Train(imgList, Token, fn, window_w, window_h, thread_num) then
        begin
          if umlFileExists(fn) then
            begin
              m64 := TMemoryStream64.Create;
              m64.LoadFromFile(fn);
              dbEng.ItemFastCreate(dbEng.RootField, Token, Token, itmHnd);
              dbEng.ItemWrite(itmHnd, m64.Size, m64.memory^);
              dbEng.ItemClose(itmHnd);
              DisposeObject(m64);
              umlDeleteFile(fn);
            end;
        end
      else
        begin
          DoStatus('Training "%s" failed.', [Token.Text]);
          DisposeObject(dbEng);
          DisposeObject(Result);
          Result := nil;
          exit;
        end;
    end;

  DisposeObject(dbEng);
end;

function TAI.OD6L_Marshal_Train(imgMat: TAI_ImageMatrix; window_w, window_h, thread_num: Integer): TMemoryStream64;
var
  i: Integer;
  dbEng: TObjectDataManager;
  token_arry: TArrayPascalString;
  m64: TMemoryStream64;
  itmHnd: TItemHandle;
  Token: U_String;
  fn: U_String;
begin
  Result := TMemoryStream64.Create;
  dbEng := TObjectDataManagerOfCache.CreateAsStream(Result, '', DBMarshal.ID, False, True, False);

  for i := 0 to imgMat.Count - 1 do
      imgMat[i].CalibrationNullToken('null');

  token_arry := imgMat.DetectorTokens;
  for Token in token_arry do
    begin
      TCoreClassThread.Sleep(1);
      fn := umlCombineFileName(RootPath, PFormat('temp_OD_%s' + C_OD6L_Ext, [umlMakeRanName.Text]));

      if OD6L_Train(imgMat, Token, fn, window_w, window_h, thread_num) then
        begin
          if umlFileExists(fn) then
            begin
              m64 := TMemoryStream64.Create;
              m64.LoadFromFile(fn);
              dbEng.ItemFastCreate(dbEng.RootField, Token, Token, itmHnd);
              dbEng.ItemWrite(itmHnd, m64.Size, m64.memory^);
              dbEng.ItemClose(itmHnd);
              DisposeObject(m64);
              umlDeleteFile(fn);
            end;
        end
      else
        begin
          DoStatus('Training "%s" failed.', [Token.Text]);
          DisposeObject(dbEng);
          DisposeObject(Result);
          Result := nil;
          exit;
        end;
    end;

  DisposeObject(dbEng);
end;

function TAI.OD6L_Marshal_Open_Stream(stream: TMemoryStream64): TOD6L_Marshal_Handle;
var
  m64: TMemoryStream64;
  dbEng: TObjectDataManager;
  itmSR: TItemSearch;
  itmHnd: TItemHandle;
  OD_Hnd: TOD6L_Handle;
begin
  m64 := TMemoryStream64.Create;
  m64.SetPointerWithProtectedMode(stream.memory, stream.Size);
  dbEng := TObjectDataManagerOfCache.CreateAsStream(m64, '', DBMarshal.ID, True, False, True);

  Result := TOD6L_Marshal_Handle.Create;
  Result.AutoFreeData := False;

  if dbEng.ItemFastFindFirst(dbEng.RootField, '', itmSR) then
    begin
      repeat
        dbEng.ItemFastOpen(itmSR.HeaderPOS, itmHnd);
        m64 := TMemoryStream64.Create;
        m64.SetSize(itmHnd.Item.Size);
        dbEng.ItemRead(itmHnd, itmHnd.Item.Size, m64.memory^);
        dbEng.ItemClose(itmHnd);

        OD_Hnd := Result[itmHnd.Name];
        if OD_Hnd <> nil then
            OD6L_Close(OD_Hnd);
        OD_Hnd := OD6L_Open_Stream(m64);
        Result.Add(itmHnd.Name, OD_Hnd, False);
        DisposeObject(m64);

      until not dbEng.ItemFastFindNext(itmSR);
    end;

  DisposeObject(dbEng);

  DoStatus('Object Detector marshal open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
end;

function TAI.OD6L_Marshal_Open_Stream(train_file: SystemString): TOD6L_Marshal_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := OD6L_Marshal_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('Object marshal detector open: %s', [train_file]);
end;

function TAI.OD6L_Marshal_Close(var hnd: TOD6L_Marshal_Handle): Boolean;
var
  i: Integer;
  p: PHashListData;
begin
  if hnd.Count > 0 then
    begin
      i := 0;
      p := hnd.FirstPtr;
      while i < hnd.Count do
        begin
          OD6L_Close(TOD6L_Handle(p^.data));
          inc(i);
          p := p^.Next;
        end;
    end;
  DisposeObject(hnd);
  Result := True;
end;

function TAI.OD6L_Marshal_Process(hnd: TOD6L_Marshal_Handle; Raster: TMemoryRaster): TOD_Marshal_Desc;
var
  lst: TCoreClassList;
  output: TOD_Marshal_List;
  rgb_img: TRGB_Image_Handle;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: Integer);
  var
    j: Integer;
    p: PHashListData;
    OD_Desc: TOD_Desc;
    omr: TOD_Marshal_Rect;
  begin
    p := PHashListData(lst[pass]);
    OD_Desc := OD6L_ProcessRGB(p^.data, rgb_img, 1024);
    for j := low(OD_Desc) to high(OD_Desc) do
      begin
        omr.R := RectV2(OD_Desc[j]);
        omr.confidence := OD_Desc[j].confidence;
        omr.Token := p^.OriginName;
        LockObject(output);
        output.Add(omr);
        UnLockObject(output);
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor_;
  var
    pass, j: Integer;
    p: PHashListData;
    OD_Desc: TOD_Desc;
    omr: TOD_Marshal_Rect;
  begin
    for pass := 0 to lst.Count - 1 do
      begin
        p := PHashListData(lst[pass]);
        OD_Desc := OD6L_ProcessRGB(p^.data, rgb_img, 1024);
        for j := low(OD_Desc) to high(OD_Desc) do
          begin
            omr.R := RectV2(OD_Desc[j]);
            omr.confidence := OD_Desc[j].confidence;
            omr.Token := p^.OriginName;
            output.Add(omr);
          end;
      end;
  end;
{$ENDIF Parallel}
  procedure FillResult_;
  var
    i: Integer;
  begin
    SetLength(Result, output.Count);
    for i := 0 to output.Count - 1 do
        Result[i] := output[i];
  end;

begin
  Raster.ReadyBits();
  output := TOD_Marshal_List.Create;
  lst := TCoreClassList.Create;
  hnd.GetListData(lst);

  rgb_img := FAI_EntryAPI^.Prepare_RGB_Image(Raster.Bits, Raster.Width, Raster.Height);

{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(@FPC_ParallelFor, 0, lst.Count - 1);
{$ELSE FPC}
  DelphiParallelFor(0, lst.Count - 1, procedure(pass: Integer)
    var
      j: Integer;
      p: PHashListData;
      OD_Desc: TOD_Desc;
      omr: TOD_Marshal_Rect;
    begin
      p := PHashListData(lst[pass]);
      OD_Desc := OD6L_ProcessRGB(p^.data, rgb_img, 1024);
      for j := low(OD_Desc) to high(OD_Desc) do
        begin
          omr.R := RectV2(OD_Desc[j]);
          omr.confidence := OD_Desc[j].confidence;
          omr.Token := p^.OriginName;
          LockObject(output);
          output.Add(omr);
          UnLockObject(output);
        end;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor_;
{$ENDIF Parallel}
  FillResult_;

  FAI_EntryAPI^.Close_RGB_Image(rgb_img);

  DisposeObject(output);
  DisposeObject(lst);
end;

function TAI.OD6L_Marshal_ProcessScaleSpace(hnd: TOD6L_Marshal_Handle; Raster: TMemoryRaster; scale: TGeoFloat): TOD_Marshal_Desc;
var
  nr: TMemoryRaster;
  buff: TOD_Marshal_Desc;
  i: Integer;
begin
  nr := NewRaster();
  nr.ZoomFrom(Raster, scale);

  buff := OD6L_Marshal_Process(hnd, nr);

  SetLength(Result, Length(buff));

  for i := 0 to Length(buff) - 1 do
    begin
      Result[i].R := RectDiv(buff[i].R, scale);
      Result[i].Token := buff[i].Token;
    end;

  SetLength(buff, 0);
  DisposeObject(nr);
end;

function TAI.SP_Train(train_cfg, train_output: U_String; oversampling_amount, tree_depth, thread_num: Integer): Boolean;
var
  train_cfg_buff, train_output_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.SP_Train) and (umlFileExists(train_cfg)) and (train_output.Len > 0) then
    begin
      train_cfg_buff := Alloc_P_Bytes(train_cfg);
      train_output_buff := Alloc_P_Bytes(train_output);

      FAI_EntryAPI^.RasterSerialized := nil;
      FAI_EntryAPI^.SerializedTime := GetTimeTick();
      try
          Result := FAI_EntryAPI^.SP_Train(train_cfg_buff, train_output_buff, oversampling_amount, tree_depth, thread_num) = 0;
      except
          Result := False;
      end;

      Free_P_Bytes(train_cfg_buff);
      Free_P_Bytes(train_output_buff);

      if not Result then
          DoStatus('ZAI: Shape Predictor training failed.');
    end
  else
      Result := False;
end;

function TAI.SP_Train(imgList: TAI_ImageList; train_output: U_String; oversampling_amount, tree_depth, thread_num: Integer): Boolean;
var
  ph, fn, prefix: U_String;
  tmpFileList: TPascalStringList;
  i: Integer;
begin
  ph := RootPath;
  tmpFileList := TPascalStringList.Create;

  TCoreClassThread.Sleep(1);
  prefix := 'temp_SP_' + umlMakeRanName + '_';

  fn := umlCombineFileName(ph, prefix + 'temp.xml');
  imgList.Build_XML(True, True, 'ZAI dataset', 'Shape predictor dataset', fn, prefix, tmpFileList);

  Result := SP_Train(fn, train_output, oversampling_amount, tree_depth, thread_num);

  for i := 0 to tmpFileList.Count - 1 do
      umlDeleteFile(tmpFileList[i]);

  DisposeObject(tmpFileList);
end;

function TAI.SP_Train(imgMat: TAI_ImageMatrix; train_output: U_String; oversampling_amount, tree_depth, thread_num: Integer): Boolean;
var
  ph, fn, prefix: U_String;
  tmpFileList: TPascalStringList;
  i: Integer;
begin
  ph := RootPath;
  tmpFileList := TPascalStringList.Create;

  TCoreClassThread.Sleep(1);
  prefix := 'temp_SP_' + umlMakeRanName + '_';

  fn := umlCombineFileName(ph, prefix + 'temp.xml');
  imgMat.Build_XML(True, True, 'ZAI dataset', 'Shape predictor dataset', fn, prefix, tmpFileList);

  Result := SP_Train(fn, train_output, oversampling_amount, tree_depth, thread_num);

  for i := 0 to tmpFileList.Count - 1 do
      umlDeleteFile(tmpFileList[i]);

  DisposeObject(tmpFileList);
end;

function TAI.SP_Train_Stream(imgList: TAI_ImageList; oversampling_amount, tree_depth, thread_num: Integer): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(RootPath, PFormat('temp_SP_%s' + C_SP_Ext, [umlMakeRanName.Text]));

  if SP_Train(imgList, fn, oversampling_amount, tree_depth, thread_num) then
    if umlFileExists(fn) then
      begin
        Result := TMemoryStream64.Create;
        Result.LoadFromFile(fn);
        Result.Position := 0;
      end;
  umlDeleteFile(fn);
end;

function TAI.SP_Train_Stream(imgMat: TAI_ImageMatrix; oversampling_amount, tree_depth, thread_num: Integer): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(RootPath, PFormat('temp_SP_%s' + C_SP_Ext, [umlMakeRanName.Text]));

  if SP_Train(imgMat, fn, oversampling_amount, tree_depth, thread_num) then
    if umlFileExists(fn) then
      begin
        Result := TMemoryStream64.Create;
        Result.LoadFromFile(fn);
        Result.Position := 0;
      end;
  umlDeleteFile(fn);
end;

function TAI.LargeScale_SP_Train(imgList: TAI_ImageList; train_output: U_String; oversampling_amount, tree_depth, thread_num: Integer): Boolean;
var
  imgArry: array of TImage_Handle;
  imgArry_P: array of PImage_Handle;
  i: Integer;
  train_output_buff: P_Bytes;
begin
  Result := False;
  SetLength(imgArry, imgList.Count);
  SetLength(imgArry_P, imgList.Count);

  for i := 0 to imgList.Count - 1 do
    begin
      imgArry[i].image := imgList[i];
      imgArry[i].AccessImage := 0;
      imgArry[i].AccessDetectorImage := 0;
      imgArry[i].AccessDetectorRect := 0;
      imgArry[i].AccessMask := 0;
      imgArry_P[i] := @imgArry[i];
    end;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LargeScale_SP_Train) then
    begin
      train_output_buff := Alloc_P_Bytes(train_output);
      FAI_EntryAPI^.RasterSerialized := nil;
      FAI_EntryAPI^.SerializedTime := GetTimeTick();
      try
          Result := FAI_EntryAPI^.LargeScale_SP_Train(@imgArry_P[0], imgList.Count, train_output_buff, oversampling_amount, tree_depth, thread_num) = 0;
      except
          Result := False;
      end;
      Free_P_Bytes(train_output_buff);
    end;

  SetLength(imgArry, 0);
  SetLength(imgArry_P, 0);
end;

function TAI.LargeScale_SP_Train(imgMat: TAI_ImageMatrix; train_output: U_String; oversampling_amount, tree_depth, thread_num: Integer): Boolean;
var
  imgL: TImageList_Decl;
  imgArry: array of TImage_Handle;
  imgArry_P: array of PImage_Handle;
  i: Integer;
  train_output_buff: P_Bytes;
begin
  Result := False;
  imgL := imgMat.ImageList();
  SetLength(imgArry, imgL.Count);
  SetLength(imgArry_P, imgL.Count);

  for i := 0 to imgL.Count - 1 do
    begin
      imgArry[i].image := imgL[i];
      imgArry[i].AccessImage := 0;
      imgArry[i].AccessDetectorImage := 0;
      imgArry[i].AccessDetectorRect := 0;
      imgArry[i].AccessMask := 0;
      imgArry_P[i] := @imgArry[i];
    end;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LargeScale_SP_Train) then
    begin
      train_output_buff := Alloc_P_Bytes(train_output);
      try
          Result := FAI_EntryAPI^.LargeScale_SP_Train(@imgArry_P[0], imgL.Count, train_output_buff, oversampling_amount, tree_depth, thread_num) = 0;
      except
          Result := False;
      end;
      Free_P_Bytes(train_output_buff);
    end;

  SetLength(imgArry, 0);
  SetLength(imgArry_P, 0);
  DisposeObject(imgL);
end;

function TAI.LargeScale_SP_Train_Stream(imgList: TAI_ImageList; oversampling_amount, tree_depth, thread_num: Integer): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(RootPath, PFormat('temp_SP_%s' + C_SP_Ext, [umlMakeRanName.Text]));

  if LargeScale_SP_Train(imgList, fn, oversampling_amount, tree_depth, thread_num) then
    if umlFileExists(fn) then
      begin
        Result := TMemoryStream64.Create;
        Result.LoadFromFile(fn);
        Result.Position := 0;
      end;
  umlDeleteFile(fn);
end;

function TAI.LargeScale_SP_Train_Stream(imgMat: TAI_ImageMatrix; oversampling_amount, tree_depth, thread_num: Integer): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(RootPath, PFormat('temp_SP_%s' + C_SP_Ext, [umlMakeRanName.Text]));

  if LargeScale_SP_Train(imgMat, fn, oversampling_amount, tree_depth, thread_num) then
    if umlFileExists(fn) then
      begin
        Result := TMemoryStream64.Create;
        Result.LoadFromFile(fn);
        Result.Position := 0;
      end;
  umlDeleteFile(fn);
end;

function TAI.SP_Open(train_file: SystemString): TSP_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.SP_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      Result := FAI_EntryAPI^.SP_Init(train_file_buff);
      Free_P_Bytes(train_file_buff);
      if Result <> nil then
          DoStatus('shape predictor open: %s', [train_file]);
    end
  else
      Result := nil;
end;

function TAI.SP_Open_Stream(stream: TMemoryStream64): TSP_Handle;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.SP_Init_Memory) then
    begin
      Result := FAI_EntryAPI^.SP_Init_Memory(stream.memory, stream.Size);
      if Result <> nil then
          DoStatus('shape predictor open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.SP_Open_Stream(train_file: SystemString): TSP_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := SP_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('shape predictor open: %s', [train_file]);
end;

function TAI.SP_Close(var hnd: TSP_Handle): Boolean;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.SP_Free) and (hnd <> nil) then
    begin
      Result := FAI_EntryAPI^.SP_Free(hnd) = 0;
      DoStatus('shape predictor close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

function TAI.SP_Process(hnd: TSP_Handle; Raster: TMemoryRaster; const AI_Rect: TAI_Rect; const max_AI_Point: Integer): TSP_Desc;
var
  point_num: Integer;
begin
  Raster.ReadyBits();
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SP_Process) then
      exit;
  SetLength(Result, max_AI_Point);

  try
    if FAI_EntryAPI^.SP_Process(hnd, Raster.Bits, Raster.Width, Raster.Height, @AI_Rect, @Result[0], max_AI_Point, point_num) > 0 then
        SetLength(Result, point_num)
    else
        SetLength(Result, 0);
  except
      SetLength(Result, 0);
  end;
end;

function TAI.SP_Process_Vec2List(hnd: TSP_Handle; Raster: TMemoryRaster; const R: TRectV2): TVec2List;
var
  desc: TSP_Desc;
  i: Integer;
begin
  desc := SP_Process(hnd, Raster, AIRect(R), 8192);
  Result := TVec2List.Create;
  for i := 0 to Length(desc) - 1 do
      Result.Add(Vec2(desc[i]));
end;

function TAI.SP_Process_Vec2(hnd: TSP_Handle; Raster: TMemoryRaster; const R: TRectV2): TArrayVec2;
var
  desc: TSP_Desc;
  i: Integer;
begin
  desc := SP_Process(hnd, Raster, AIRect(R), 8192);
  SetLength(Result, Length(desc));
  for i := 0 to Length(desc) - 1 do
      Result[i] := Vec2(desc[i]);
end;

function TAI.SP_Process_Vec2(hnd: TSP_Handle; Raster: TMemoryRaster; const R: TAI_Rect): TArrayVec2;
var
  desc: TSP_Desc;
  i: Integer;
begin
  desc := SP_Process(hnd, Raster, R, 8192);
  SetLength(Result, Length(desc));
  for i := 0 to Length(desc) - 1 do
      Result[i] := Vec2(desc[i]);
end;

function TAI.SP_Process_Vec2(hnd: TSP_Handle; Raster: TMemoryRaster; const R: TOD_Rect): TArrayVec2;
var
  desc: TSP_Desc;
  i: Integer;
begin
  desc := SP_Process(hnd, Raster, AIRect(R), 8192);
  SetLength(Result, Length(desc));
  for i := 0 to Length(desc) - 1 do
      Result[i] := Vec2(desc[i]);
end;

function TAI.SP_Process_Face(Raster: TMemoryRaster; const R: TRectV2): TArrayVec2;
begin
  PrepareFaceDataSource;
  Result := SP_Process_Vec2(FFace_SP_Hnd, Raster, R);
end;

function TAI.SP_ProcessRGB(hnd: TSP_Handle; rgb_img: TRGB_Image_Handle; const AI_Rect: TAI_Rect; const max_AI_Point: Integer): TSP_Desc;
var
  point_num: Integer;
begin
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SP_Process) then
      exit;
  SetLength(Result, max_AI_Point);

  try
    if FAI_EntryAPI^.SP_Process_Image(hnd, rgb_img, @AI_Rect, @Result[0], max_AI_Point, point_num) > 0 then
        SetLength(Result, point_num)
    else
        SetLength(Result, 0);
  except
      SetLength(Result, 0);
  end;
end;

function TAI.SP_ProcessRGB_Vec2List(hnd: TSP_Handle; rgb_img: TRGB_Image_Handle; const R: TRectV2): TVec2List;
var
  desc: TSP_Desc;
  i: Integer;
begin
  desc := SP_ProcessRGB(hnd, rgb_img, AIRect(R), 8192);
  Result := TVec2List.Create;
  for i := 0 to Length(desc) - 1 do
      Result.Add(Vec2(desc[i]));
end;

function TAI.SP_ProcessRGB_Vec2(hnd: TSP_Handle; rgb_img: TRGB_Image_Handle; const R: TRectV2): TArrayVec2;
var
  desc: TSP_Desc;
  i: Integer;
begin
  desc := SP_ProcessRGB(hnd, rgb_img, AIRect(R), 8192);
  SetLength(Result, Length(desc));
  for i := 0 to Length(desc) - 1 do
      Result[i] := Vec2(desc[i]);
end;

function TAI.SP_ProcessRGB_Vec2(hnd: TSP_Handle; rgb_img: TRGB_Image_Handle; const R: TAI_Rect): TArrayVec2;
var
  desc: TSP_Desc;
  i: Integer;
begin
  desc := SP_ProcessRGB(hnd, rgb_img, R, 8192);
  SetLength(Result, Length(desc));
  for i := 0 to Length(desc) - 1 do
      Result[i] := Vec2(desc[i]);
end;

function TAI.SP_ProcessRGB_Vec2(hnd: TSP_Handle; rgb_img: TRGB_Image_Handle; const R: TOD_Rect): TArrayVec2;
var
  desc: TSP_Desc;
  i: Integer;
begin
  desc := SP_ProcessRGB(hnd, rgb_img, AIRect(R), 8192);
  SetLength(Result, Length(desc));
  for i := 0 to Length(desc) - 1 do
      Result[i] := Vec2(desc[i]);
end;

procedure TAI.PrepareFaceDataSource;
var
  m64: TMemoryStream64;
begin
  Wait_AI_Init;
  try
    if (FFace_SP_Hnd = nil) then
      begin
        m64 := TMemoryStream64.Create;
        m64.SetPointerWithProtectedMode(build_in_face_shape_memory, build_in_face_shape_memory_siz);
        m64.Position := 0;
        FFace_SP_Hnd := SP_Open_Stream(m64);
        SP_Close(Parallel_SP_Hnd);
        Parallel_SP_Hnd := FFace_SP_Hnd;
        DisposeObject(m64);
      end;
  except
      FFace_SP_Hnd := nil;
  end;
end;

function TAI.Face_Detector(Raster: TMemoryRaster; R: TRect; extract_face_size: Integer): TFACE_Handle;
var
  desc: TAI_Rect_Desc;
begin
  SetLength(desc, 1);
  desc[0] := AIRect(R);
  Result := Face_Detector(Raster, desc, extract_face_size);
  SetLength(desc, 0);
end;

function TAI.Face_Detector(Raster: TMemoryRaster; desc: TAI_Rect_Desc; extract_face_size: Integer): TFACE_Handle;
var
  i: Integer;
  fixed_desc: TAI_Rect_Desc;
begin
  Raster.ReadyBits();
  Result := nil;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SP_extract_face_rect_desc_chips) then
      exit;
  if Length(desc) = 0 then
      exit;

  PrepareFaceDataSource;
  SetLength(fixed_desc, Length(desc));
  for i := 0 to Length(desc) - 1 do
      fixed_desc[i] := AIRect(RectScaleSpace(RectV2(desc[i]), extract_face_size, extract_face_size));

  try
      Result := FAI_EntryAPI^.SP_extract_face_rect_desc_chips(FFace_SP_Hnd, Raster.Bits, Raster.Width, Raster.Height, extract_face_size, @fixed_desc[0], Length(desc));
  except
      Result := nil;
  end;
  SetLength(fixed_desc, 0);
end;

function TAI.Face_Detector(Raster: TMemoryRaster; MMOD_Desc: TMMOD_Desc; extract_face_size: Integer): TFACE_Handle;
var
  i: Integer;
  ai_rect_desc: TAI_Rect_Desc;
begin
  SetLength(ai_rect_desc, Length(MMOD_Desc));
  for i := 0 to Length(MMOD_Desc) - 1 do
      ai_rect_desc[i] := AIRect(MMOD_Desc[i].R);
  Result := Face_Detector(Raster, ai_rect_desc, extract_face_size);
  SetLength(ai_rect_desc, 0);
end;

function TAI.Face_Detector(Raster: TMemoryRaster; OD_Desc: TOD_Desc; extract_face_size: Integer): TFACE_Handle;
var
  i: Integer;
  ai_rect_desc: TAI_Rect_Desc;
begin
  SetLength(ai_rect_desc, Length(OD_Desc));
  for i := 0 to Length(OD_Desc) - 1 do
      ai_rect_desc[i] := AIRect(OD_Desc[i]);
  Result := Face_Detector(Raster, ai_rect_desc, extract_face_size);
  SetLength(ai_rect_desc, 0);
end;

function TAI.Face_DetectorAsChips(Raster: TMemoryRaster; desc: TAI_Rect; extract_face_size: Integer): TMemoryRaster;
var
  face_hnd: TFACE_Handle;
begin
  Result := nil;
  if not Activted then
      exit;
  face_hnd := Face_Detector(Raster, Rect(desc), extract_face_size);
  if face_hnd = nil then
      exit;
  if Face_chips_num(face_hnd) > 0 then
      Result := Face_chips(face_hnd, 0);
  Face_Close(face_hnd);
end;

function TAI.Face_Detector_All(Raster: TMemoryRaster): TFACE_Handle;
begin
  Result := Face_Detector_All(Raster, C_Metric_Input_Size);
end;

function TAI.Face_Detector_All(Raster: TMemoryRaster; extract_face_size: Integer): TFACE_Handle;
begin
  Raster.ReadyBits();
  Result := nil;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SP_extract_face_rect_chips) then
      exit;
  PrepareFaceDataSource;
  try
      Result := FAI_EntryAPI^.SP_extract_face_rect_chips(FFace_SP_Hnd, Raster.Bits, Raster.Width, Raster.Height, extract_face_size);
  except
      Result := nil;
  end;
end;

function TAI.Face_Detector_Rect(Raster: TMemoryRaster): TFACE_Handle;
begin
  Raster.ReadyBits();
  Result := nil;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SP_extract_face_rect) then
      exit;

  try
      Result := FAI_EntryAPI^.SP_extract_face_rect(Raster.Bits, Raster.Width, Raster.Height);
  except
      Result := nil;
  end;
end;

function TAI.Face_Detector_AllRect(Raster: TMemoryRaster): TAI_Rect_Desc;
var
  face_hnd: TFACE_Handle;
  i: Integer;
begin
  SetLength(Result, 0);
  face_hnd := Face_Detector_Rect(Raster);
  if face_hnd = nil then
      exit;
  SetLength(Result, Face_Rect_Num(face_hnd));
  for i := 0 to Length(Result) - 1 do
      Result[i] := Face_Rect(face_hnd, i);
  Face_Close(face_hnd);
end;

function TAI.Face_chips_num(hnd: TFACE_Handle): Integer;
begin
  Result := 0;
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SP_get_face_chips_num) then
      exit;

  Result := FAI_EntryAPI^.SP_get_face_chips_num(hnd);
end;

function TAI.Face_chips(hnd: TFACE_Handle; index: Integer): TMemoryRaster;
var
  w, h: Integer;
begin
  Result := nil;
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SP_get_face_chips_size) then
      exit;
  if not Assigned(FAI_EntryAPI^.SP_get_face_chips_bits) then
      exit;

  Result := NewRaster();
  FAI_EntryAPI^.SP_get_face_chips_size(hnd, index, w, h);
  Result.SetSize(w, h);
  FAI_EntryAPI^.SP_get_face_chips_bits(hnd, index, Result.Bits);
end;

function TAI.Face_GetCentreRectIndex(Raster: TMemoryRaster; hnd: TFACE_Handle): Integer;
var
  i: Integer;
  axis, tmp: TVec2;
begin
  Result := -1;
  if Face_Rect_Num(hnd) <= 0 then
      exit;
  Result := 0;
  axis := RectCentre(Face_RectV2(hnd, Result));
  for i := 1 to Face_Rect_Num(hnd) - 1 do
    begin
      tmp := RectCentre(Face_RectV2(hnd, i));
      if Vec2Distance(axis, Raster.Centre) > Vec2Distance(tmp, Raster.Centre) then
        begin
          Result := i;
          axis := tmp;
        end;
    end;
end;

function TAI.Face_Rect_Num(hnd: TFACE_Handle): Integer;
begin
  Result := 0;
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SP_get_face_rect_num) then
      exit;

  Result := FAI_EntryAPI^.SP_get_face_rect_num(hnd);
end;

function TAI.Face_Rect(hnd: TFACE_Handle; index: Integer): TAI_Rect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := 0;
  Result.Bottom := 0;
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SP_get_face_rect) then
      exit;

  FAI_EntryAPI^.SP_get_face_rect(hnd, index, Result);
end;

function TAI.Face_RectV2(hnd: TFACE_Handle; index: Integer): TRectV2;
begin
  Result := RectV2(Face_Rect(hnd, index));
end;

function TAI.Face_Shape_num(hnd: TFACE_Handle): Integer;
begin
  Result := 0;
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SP_get_num) then
      exit;

  Result := FAI_EntryAPI^.SP_get_num(hnd);
end;

function TAI.Face_Shape(hnd: TFACE_Handle; index: Integer): TSP_Desc;
var
  sp_num: Integer;
begin
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SP_get) then
      exit;

  SetLength(Result, 8192);
  sp_num := FAI_EntryAPI^.SP_get(hnd, index, @Result[0], Length(Result));
  SetLength(Result, sp_num);
end;

function TAI.Face_ShapeV2(hnd: TFACE_Handle; index: Integer): TArrayVec2;
var
  sp_num: Integer;
  buff: TSP_Desc;
  i: Integer;
begin
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SP_get) then
      exit;

  SetLength(buff, 8192);
  sp_num := FAI_EntryAPI^.SP_get(hnd, index, @buff[0], Length(buff));
  SetLength(buff, sp_num);
  SetLength(Result, sp_num);
  for i := Low(buff) to high(buff) do
      Result[i] := Vec2(buff[i]);
end;

function TAI.Face_Shape_rect(hnd: TFACE_Handle; index: Integer): TRectV2;
var
  sp_desc: TSP_Desc;
begin
  sp_desc := Face_Shape(hnd, index);
  Result := GetSPBound(sp_desc, 0);
  SetLength(sp_desc, 0);
end;

procedure TAI.Face_Close(var hnd: TFACE_Handle);
begin
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SP_close_face_chips_handle) then
      exit;

  FAI_EntryAPI^.SP_close_face_chips_handle(hnd);
  hnd := nil;
end;

class function TAI.Init_Metric_ResNet_Parameter(train_sync_file, train_output: U_String): PMetric_ResNet_Train_Parameter;
begin
  new(Result);
  FillPtrByte(Result, SizeOf(TMetric_ResNet_Train_Parameter), 0);

  Result^.imgArry_ptr := nil;
  Result^.img_num := 0;
  Result^.train_sync_file := Alloc_P_Bytes(train_sync_file);
  Result^.train_output := Alloc_P_Bytes(train_output);

  Result^.timeout := C_Tick_Hour;
  Result^.weight_decay := 0.0001;
  Result^.momentum := 0.9;
  Result^.iterations_without_progress_threshold := 500;
  Result^.learning_rate := 0.1;
  Result^.completed_learning_rate := 0.0001;
  Result^.step_mini_batch_target_num := 5;
  Result^.step_mini_batch_raster_num := 5;

  Result^.control := nil;
  Result^.training_average_loss := 0;
  Result^.training_learning_rate := 0;

  Result^.fullGPU_Training := True;
end;

class procedure TAI.Free_Metric_ResNet_Parameter(param: PMetric_ResNet_Train_Parameter);
begin
  Free_P_Bytes(param^.train_sync_file);
  Free_P_Bytes(param^.train_output);
  Dispose(param);
end;

function TAI.Metric_ResNet_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TMemoryRaster2DArray; param: PMetric_ResNet_Train_Parameter): Boolean;
var
  i, j, imgSum, ri: Integer;
  imgArry: TMemoryRasterArray;
  rArry: array of TAI_Raster_Data;
begin
  Result := False;

  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.MDNN_ResNet_Train) then
      exit;
  if not Assigned(FAI_EntryAPI^.MDNN_ResNet_Full_GPU_Train) then
      exit;

  imgSum := 0;
  for i := 0 to Length(imgList) - 1 do
      inc(imgSum, Length(imgList[i]));

  if imgSum = 0 then
      exit;

  { process sequence }
  SetLength(rArry, imgSum);
  ri := 0;
  for i := 0 to Length(imgList) - 1 do
    begin
      imgArry := imgList[i];
      for j := 0 to Length(imgArry) - 1 do
        begin
          new(rArry[ri].raster_Hnd);
          rArry[ri].raster_Hnd^.Raster := imgArry[j];
          if LargeScale_ then
            begin
              rArry[ri].raster_ptr := nil;
              imgArry[j].SerializedAndRecycleMemory(RSeri);
            end
          else
              rArry[ri].raster_ptr := imgArry[j].Bits;

          rArry[ri].Width := imgArry[j].Width;
          rArry[ri].Height := imgArry[j].Height;
          rArry[ri].index := i;
          inc(ri);
        end;
    end;

  { set arry }
  param^.imgArry_ptr := PAI_Raster_Data_Array(@rArry[0]);
  param^.img_num := Length(rArry);
  param^.control := @TrainingControl;

  { execute training }
  TrainingControl.pause := 0;
  TrainingControl.stop := 0;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := True;
      FAI_EntryAPI^.RasterSerialized := RSeri;
      RSeri.EnabledReadHistory := True;
    end
  else
      FAI_EntryAPI^.RasterSerialized := nil;

  FAI_EntryAPI^.SerializedTime := GetTimeTick();

  { run training }
  try
    if param^.fullGPU_Training then
        Result := FAI_EntryAPI^.MDNN_ResNet_Full_GPU_Train(param) >= 0
    else
        Result := FAI_EntryAPI^.MDNN_ResNet_Train(param) >= 0;
  except
      Result := False;
  end;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := False;
      FAI_EntryAPI^.RasterSerialized := nil;
    end;

  Last_training_average_loss := param^.training_average_loss;
  Last_training_learning_rate := param^.training_learning_rate;
  completed_learning_rate := param^.completed_learning_rate;

  { reset arry }
  param^.imgArry_ptr := nil;
  param^.img_num := 0;

  { free }
  for i := 0 to Length(rArry) - 1 do
      Dispose(rArry[i].raster_Hnd);
  SetLength(rArry, 0);
end;

function TAI.Metric_ResNet_Train(Snapshot_: Boolean; imgList: TAI_ImageList; param: PMetric_ResNet_Train_Parameter): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.MDNN_ResNet_Train) then
      exit;

  if Snapshot_ then
    begin
      imgList.CalibrationNoDetectorDefine('');
      imgBuff := imgList.ExtractDetectorDefineAsSnapshotProjection(C_Metric_Input_Size, C_Metric_Input_Size);
    end
  else
    begin
      imgBuff := imgList.ExtractDetectorDefineAsPrepareRaster(C_Metric_Input_Size, C_Metric_Input_Size);
    end;

  if Length(imgBuff) = 0 then
      exit;

  Result := Metric_ResNet_Train(False, nil, imgBuff, param);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.Metric_ResNet_Train_Stream(Snapshot_: Boolean; imgList: TAI_ImageList; param: PMetric_ResNet_Train_Parameter): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if Metric_ResNet_Train(Snapshot_, imgList, param) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.Metric_ResNet_Train(Snapshot_: Boolean; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  imgL: TAI_ImageList;
  detDef: TAI_DetectorDefine;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.MDNN_ResNet_Train) then
      exit;

  if Snapshot_ then
    begin
      DoStatus('Calibration Metric dataset.');
      for i := 0 to imgMat.Count - 1 do
        begin
          imgL := imgMat[i];
          imgL.CalibrationNoDetectorDefine(imgL.FileInfo);
          imgL.CalibrationNullToken(imgL.FileInfo);
          for j := 0 to imgL.Count - 1 do
            if imgL[j].DetectorDefineList.Count = 0 then
              begin
                detDef := TAI_DetectorDefine.Create(imgL[j]);
                detDef.R := imgL[j].Raster.BoundsRect;
                detDef.Token := imgL.FileInfo;
                imgL[j].DetectorDefineList.Add(detDef);
              end;
        end;
      imgBuff := imgMat.ExtractDetectorDefineAsSnapshotProjection(C_Metric_Input_Size, C_Metric_Input_Size);
    end
  else
      imgBuff := imgMat.ExtractDetectorDefineAsPrepareRaster(C_Metric_Input_Size, C_Metric_Input_Size);

  if Length(imgBuff) = 0 then
      exit;

  Result := Metric_ResNet_Train(False, nil, imgBuff, param);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.Metric_ResNet_Train_Stream(Snapshot_: Boolean; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if Metric_ResNet_Train(Snapshot_, imgMat, param) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.Metric_ResNet_Train(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  imgL: TAI_ImageList;
  detDef: TAI_DetectorDefine;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.MDNN_ResNet_Train) then
      exit;

  if Snapshot_ then
    begin
      DoStatus('Calibration Metric dataset.');
      for i := 0 to imgMat.Count - 1 do
        begin
          imgL := imgMat[i];
          imgL.CalibrationNoDetectorDefine(imgL.FileInfo);
          imgL.CalibrationNullToken(imgL.FileInfo);
          for j := 0 to imgL.Count - 1 do
            if imgL[j].DetectorDefineList.Count = 0 then
              begin
                detDef := TAI_DetectorDefine.Create(imgL[j]);
                detDef.R := imgL[j].Raster.BoundsRect;
                detDef.Token := imgL.FileInfo;
                imgL[j].DetectorDefineList.Add(detDef);
              end;
        end;

      if LargeScale_ then
          imgBuff := imgMat.LargeScale_ExtractDetectorDefineAsSnapshotProjection(RSeri, C_Metric_Input_Size, C_Metric_Input_Size)
      else
          imgBuff := imgMat.ExtractDetectorDefineAsSnapshotProjection(C_Metric_Input_Size, C_Metric_Input_Size);
    end
  else
    begin
      if LargeScale_ then
          imgBuff := imgMat.LargeScale_ExtractDetectorDefineAsPrepareRaster(RSeri, C_Metric_Input_Size, C_Metric_Input_Size)
      else
          imgBuff := imgMat.ExtractDetectorDefineAsPrepareRaster(C_Metric_Input_Size, C_Metric_Input_Size);
    end;

  if Length(imgBuff) = 0 then
      exit;

  Result := Metric_ResNet_Train(LargeScale_, RSeri, imgBuff, param);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.Metric_ResNet_Train_Stream(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if Metric_ResNet_Train(Snapshot_, LargeScale_, RSeri, imgMat, param) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

class function TAI.BuildShareFaceLearn(): TLearn;
var
  m64: TMemoryStream64;
  L: TLearn;
begin
  Wait_AI_Init;
  m64 := TMemoryStream64.Create;
  m64.SetPointerWithProtectedMode(build_in_face_metric_learn_memory, build_in_face_metric_learn_memory_siz);
  L := Build_Metric_ResNet_Learn();
  L.LoadFromStream(m64);
  DisposeObject(m64);
  Result := L;
end;

class function TAI.Build_Metric_ResNet_Learn(): TLearn;
var
  L: TLearn;
begin
  L := TLearn.CreateClassifier(ltKDT, zAI.C_Metric_Dim);
  Result := L;
end;

function TAI.Metric_ResNet_Open_ShareFace(): TMetric_Handle;
var
  m64: TMemoryStream64;
begin
  Wait_AI_Init;
  m64 := TMemoryStream64.Create;
  m64.SetPointerWithProtectedMode(build_in_face_metric_memory, build_in_face_metric_memory_siz);
  Result := Metric_ResNet_Open_Stream(m64);
  DisposeObject(m64);
end;

function TAI.Metric_ResNet_Open(train_file: SystemString): TMetric_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MDNN_ResNet_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      Result := FAI_EntryAPI^.MDNN_ResNet_Init(train_file_buff);
      Free_P_Bytes(train_file_buff);
      if Result <> nil then
          DoStatus('MDNN-ResNet(ResNet metric DNN) open: %s', [train_file]);
    end
  else
      Result := nil;
end;

function TAI.Metric_ResNet_Open_Stream(stream: TMemoryStream64): TMetric_Handle;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MDNN_ResNet_Init_Memory) then
    begin
      Result := FAI_EntryAPI^.MDNN_ResNet_Init_Memory(stream.memory, stream.Size);
      if Result <> nil then
          DoStatus('MDNN-ResNet(ResNet metric DNN) open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.Metric_ResNet_Open_Stream(train_file: SystemString): TMetric_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := Metric_ResNet_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('MDNN-ResNet(ResNet metric DNN) open: %s', [train_file]);
end;

function TAI.Metric_ResNet_Close(var hnd: TMetric_Handle): Boolean;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MDNN_ResNet_Free) and (hnd <> nil) then
    begin
      Result := FAI_EntryAPI^.MDNN_ResNet_Free(hnd) = 0;
      DoStatus('MDNN-ResNet(ResNet metric DNN) close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

function TAI.Metric_ResNet_Process(hnd: TMetric_Handle; RasterArray: TMemoryRasterArray; output: PDouble): Integer;
var
  rArry: array of TAI_Raster_Data;
  i: Integer;
  nr: TMemoryRaster;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MDNN_ResNet_Process) then
    begin
      SetLength(rArry, Length(RasterArray));
      for i := 0 to Length(RasterArray) - 1 do
        begin
          new(rArry[i].raster_Hnd);

          nr := NewRaster();

          { projection }
          if (RasterArray[i].Width <> C_Metric_Input_Size) or (RasterArray[i].Height <> C_Metric_Input_Size) then
            begin
              nr.SetSize(C_Metric_Input_Size, C_Metric_Input_Size);
              RasterArray[i].ProjectionTo(nr,
                TV2Rect4.Init(RectFit(C_Metric_Input_Size, C_Metric_Input_Size, RasterArray[i].BoundsRectV2), 0),
                TV2Rect4.Init(nr.BoundsRectV2, 0),
                True, 1.0);
            end
          else { fast assign }
              nr.SetWorkMemory(RasterArray[i]);

          rArry[i].raster_Hnd^.Raster := nr;

          rArry[i].raster_ptr := nr.Bits;
          rArry[i].Width := nr.Width;
          rArry[i].Height := nr.Height;
          rArry[i].index := i;
        end;

      FAI_EntryAPI^.RasterSerialized := nil;
      FAI_EntryAPI^.SerializedTime := GetTimeTick();

      Result := FAI_EntryAPI^.MDNN_ResNet_Process(hnd, PAI_Raster_Data_Array(@rArry[0]), Length(rArry), output);

      for i := 0 to Length(rArry) - 1 do
        begin
          DisposeObject(rArry[i].raster_Hnd^.Raster);
          Dispose(rArry[i].raster_Hnd);
        end;
      SetLength(rArry, 0);
    end
  else
      Result := -2;
end;

function TAI.Metric_ResNet_Process(hnd: TMetric_Handle; RasterArray: TMemoryRasterArray): TLMatrix;
var
  L: TLVec;
  i: TLInt;
begin
  Result := LMatrix(0, 0);
  if Length(RasterArray) > 0 then
    begin
      SetLength(L, Length(RasterArray) * C_Metric_Dim);
      if Metric_ResNet_Process(hnd, RasterArray, @L[0]) > 0 then
        begin
          Result := LMatrix(Length(RasterArray), 0);
          for i := Low(Result) to high(Result) do
              Result[i] := LVecCopy(L, i * C_Metric_Dim, C_Metric_Dim);
        end;
      SetLength(L, 0);
    end;
end;

function TAI.Metric_ResNet_Process(hnd: TMetric_Handle; Raster: TMemoryRaster): TLVec;
var
  rArry: TMemoryRasterArray;
begin
  Raster.ReadyBits();
  SetLength(Result, C_Metric_Dim);
  SetLength(rArry, 1);
  rArry[0] := Raster;
  if Metric_ResNet_Process(hnd, rArry, @Result[0]) <= 0 then
      SetLength(Result, 0);
end;

procedure TAI.Metric_ResNet_SaveToLearnEngine_DT(pool_: TAI_DNN_ThreadPool; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; lr: TLearn);
var
  i, j: Integer;
  imgData: TAI_Image;
  detDef: TAI_DetectorDefine;
  p: PMetric_ResNet_SaveToLearnEngine_DT_UserData_;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      imgData := imgList[i];
      if RSeri <> nil then
          imgData.UnserializedMemory(RSeri);
      if Snapshot_ then
        begin
          new(p);
          p^.lr := lr;
          p^.Snapshot := True;
          p^.imgData := imgData;
          p^.detDef := nil;
          TAI_DNN_Thread_Metric(pool_.MinLoad_DNN_Thread).ProcessC(p, imgData.Raster.Clone, True, {$IFDEF FPC}@{$ENDIF FPC}Metric_ResNet_SaveToLearnEngine_DT_Backcall);
        end
      else
        for j := 0 to imgData.DetectorDefineList.Count - 1 do
          begin
            detDef := imgData.DetectorDefineList[j];
            if detDef.Token.Len > 0 then
              begin
                new(p);
                p^.lr := lr;
                p^.Snapshot := False;
                p^.imgData := nil;
                p^.detDef := detDef;

                if detDef.PrepareRaster.Empty then
                    TAI_DNN_Thread_Metric(pool_.MinLoad_DNN_Thread).ProcessC(p,
                    detDef.Owner.Raster.BuildAreaOffsetScaleSpace(detDef.R, C_Metric_Input_Size, C_Metric_Input_Size),
                    True, {$IFDEF FPC}@{$ENDIF FPC}Metric_ResNet_SaveToLearnEngine_DT_Backcall)
                else
                    TAI_DNN_Thread_Metric(pool_.MinLoad_DNN_Thread).ProcessC(p,
                    detDef.PrepareRaster.Clone,
                    True, {$IFDEF FPC}@{$ENDIF FPC}Metric_ResNet_SaveToLearnEngine_DT_Backcall);
              end;
          end;
      if RSeri <> nil then
          imgData.SerializedAndRecycleMemory(RSeri);
    end;
end;

procedure TAI.Metric_ResNet_SaveToLearnEngine_DT(Metric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; lr: TLearn);
var
  pool_: TAI_DNN_ThreadPool;
  i: Integer;
  Device_: TLIVec;
begin
  if lr.InSize <> C_Metric_Dim then
      RaiseInfo('Learn Engine Insize illegal');
  pool_ := TAI_DNN_ThreadPool.Create;

  GetComputeDeviceOfTraining(Device_);
  for i in Device_ do
      pool_.BuildDeviceThread(FAI_EntryAPI, i, 2, TAI_DNN_Thread_Metric);
  for i := 0 to pool_.Count - 1 do
      TAI_DNN_Thread_Metric(pool_[i]).Open_Stream(Metric_stream);

  Metric_ResNet_SaveToLearnEngine_DT(pool_, Snapshot_, RSeri, imgList, lr);
  pool_.Wait();
  DisposeObject(pool_);
end;

procedure TAI.Metric_ResNet_SaveToLearnEngine_DT(Metric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; lr: TLearn);
var
  pool_: TAI_DNN_ThreadPool;
  i: Integer;
  Device_: TLIVec;
begin
  if lr.InSize <> C_Metric_Dim then
      RaiseInfo('Learn Engine Insize illegal');
  pool_ := TAI_DNN_ThreadPool.Create;

  GetComputeDeviceOfTraining(Device_);
  for i in Device_ do
      pool_.BuildDeviceThread(FAI_EntryAPI, i, 2, TAI_DNN_Thread_Metric);
  for i := 0 to pool_.Count - 1 do
      TAI_DNN_Thread_Metric(pool_[i]).Open_Stream(Metric_stream);

  for i := 0 to imgMat.Count - 1 do
      Metric_ResNet_SaveToLearnEngine_DT(pool_, Snapshot_, RSeri, imgMat[i], lr);
  pool_.Wait();
  DisposeObject(pool_);
end;

procedure TAI.Metric_ResNet_SaveToLearnEngine_DT(Metric_stream: TMemoryStream64; Snapshot_: Boolean; imgList: TAI_ImageList; lr: TLearn);
begin
  Metric_ResNet_SaveToLearnEngine_DT(Metric_stream, Snapshot_, nil, imgList, lr);
end;

procedure TAI.Metric_ResNet_SaveToLearnEngine_DT(Metric_stream: TMemoryStream64; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; lr: TLearn);
begin
  Metric_ResNet_SaveToLearnEngine_DT(Metric_stream, Snapshot_, nil, imgMat, lr);
end;

procedure TAI.Metric_ResNet_SaveToLearnEngine(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; lr: TLearn);
var
  i, j: Integer;
  imgData: TAI_Image;
  detDef: TAI_DetectorDefine;
  mr: TMemoryRaster;
  V: TLVec;
begin
  if lr.InSize <> C_Metric_Dim then
      RaiseInfo('Learn Engine Insize illegal');
  for i := 0 to imgList.Count - 1 do
    begin
      imgData := imgList[i];
      if RSeri <> nil then
          imgData.UnserializedMemory(RSeri);
      if Snapshot_ then
        begin
          mr := imgData.Raster;
          V := Metric_ResNet_Process(Metric_hnd, mr);
          if Length(V) <> C_Metric_Dim then
              DoStatus('Metric-ResNet vector error!')
          else
            begin
              for j := 0 to imgData.DetectorDefineList.Count - 1 do
                begin
                  detDef := imgData.DetectorDefineList[j];
                  if detDef.Token.Len > 0 then
                      lr.AddMemory(V, detDef.Token);
                end;
            end;
        end
      else
        for j := 0 to imgData.DetectorDefineList.Count - 1 do
          begin
            detDef := imgData.DetectorDefineList[j];
            if detDef.Token.Len > 0 then
              begin
                if detDef.PrepareRaster.Empty then
                  begin
                    mr := detDef.Owner.Raster.BuildAreaOffsetScaleSpace(detDef.R, C_Metric_Input_Size, C_Metric_Input_Size);
                    V := Metric_ResNet_Process(Metric_hnd, mr);
                    DisposeObject(mr);
                  end
                else
                  begin
                    mr := detDef.PrepareRaster;
                    V := Metric_ResNet_Process(Metric_hnd, mr);
                  end;
                if Length(V) <> C_Metric_Dim then
                    DoStatus('Metric-ResNet vector error!')
                else
                    lr.AddMemory(V, detDef.Token);
              end;
          end;
      if RSeri <> nil then
          imgData.SerializedAndRecycleMemory(RSeri);
    end;
end;

procedure TAI.Metric_ResNet_SaveToLearnEngine(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; lr: TLearn);
var
  i: Integer;
begin
  for i := 0 to imgMat.Count - 1 do
      Metric_ResNet_SaveToLearnEngine(Metric_hnd, Snapshot_, RSeri, imgMat[i], lr);
end;

procedure TAI.Metric_ResNet_SaveToLearnEngine(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; imgList: TAI_ImageList; lr: TLearn);
begin
  Metric_ResNet_SaveToLearnEngine(Metric_hnd, Snapshot_, nil, imgList, lr);
end;

procedure TAI.Metric_ResNet_SaveToLearnEngine(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; lr: TLearn);
begin
  Metric_ResNet_SaveToLearnEngine(Metric_hnd, Snapshot_, nil, imgMat, lr);
end;

procedure TAI.Metric_ResNet_SaveToKDTree(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; kd: TKDTreeDataList);
var
  i, j: Integer;
  imgData: TAI_Image;
  detDef: TAI_DetectorDefine;
  mr: TMemoryRaster;
  V: TLVec;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      imgData := imgList[i];
      if RSeri <> nil then
          imgData.UnserializedMemory(RSeri);
      if Snapshot_ then
        begin
          mr := imgData.Raster;
          V := Metric_ResNet_Process(Metric_hnd, mr);
          if Length(V) <> C_Metric_Dim then
              DoStatus('Metric-ResNet vector error!')
          else
            begin
              for j := 0 to imgData.DetectorDefineList.Count - 1 do
                begin
                  detDef := imgData.DetectorDefineList[j];
                  if detDef.Token.Len > 0 then
                      kd.Add(V, detDef.Token);
                end;
            end;
        end
      else
        for j := 0 to imgData.DetectorDefineList.Count - 1 do
          begin
            detDef := imgData.DetectorDefineList[j];
            if detDef.Token.Len > 0 then
              begin
                if detDef.PrepareRaster.Empty then
                  begin
                    mr := detDef.Owner.Raster.BuildAreaOffsetScaleSpace(detDef.R, C_Metric_Input_Size, C_Metric_Input_Size);
                    V := Metric_ResNet_Process(Metric_hnd, mr);
                    DisposeObject(mr);
                  end
                else
                  begin
                    mr := detDef.PrepareRaster;
                    V := Metric_ResNet_Process(Metric_hnd, mr);
                  end;

                if Length(V) <> C_Metric_Dim then
                    DoStatus('Metric-ResNet vector error!')
                else
                    kd.Add(V, detDef.Token);
              end;
          end;
      if RSeri <> nil then
          imgData.SerializedAndRecycleMemory(RSeri);
    end;
end;

procedure TAI.Metric_ResNet_SaveToKDTree(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; kd: TKDTreeDataList);
var
  i: Integer;
begin
  for i := 0 to imgMat.Count - 1 do
    begin
      Metric_ResNet_SaveToKDTree(Metric_hnd, Snapshot_, RSeri, imgMat[i], kd);
    end;
end;

procedure TAI.Metric_ResNet_SaveToKDTree(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; imgList: TAI_ImageList; kd: TKDTreeDataList);
begin
  Metric_ResNet_SaveToKDTree(Metric_hnd, Snapshot_, nil, imgList, kd);
end;

procedure TAI.Metric_ResNet_SaveToKDTree(Metric_hnd: TMetric_Handle; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; kd: TKDTreeDataList);
begin
  Metric_ResNet_SaveToKDTree(Metric_hnd, Snapshot_, nil, imgMat, kd);
end;

function TAI.Metric_ResNet_DebugInfo(hnd: TMetric_Handle): U_String;
var
  p: PPascalString;
begin
  Result := '';
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MDNN_DebugInfo) and (hnd <> nil) then
    begin
      FAI_EntryAPI^.MDNN_DebugInfo(hnd, p);
      Result := p^;
      Dispose(p);
    end;
end;

class function TAI.Init_LMetric_ResNet_Parameter(train_sync_file, train_output: U_String): PMetric_ResNet_Train_Parameter;
begin
  new(Result);
  FillPtrByte(Result, SizeOf(TMetric_ResNet_Train_Parameter), 0);

  Result^.imgArry_ptr := nil;
  Result^.img_num := 0;
  Result^.train_sync_file := Alloc_P_Bytes(train_sync_file);
  Result^.train_output := Alloc_P_Bytes(train_output);

  Result^.timeout := C_Tick_Hour;
  Result^.weight_decay := 0.0001;
  Result^.momentum := 0.9;
  Result^.iterations_without_progress_threshold := 500;
  Result^.learning_rate := 0.1;
  Result^.completed_learning_rate := 0.0001;
  Result^.step_mini_batch_target_num := 5;
  Result^.step_mini_batch_raster_num := 5;

  Result^.control := nil;
  Result^.training_average_loss := 0;
  Result^.training_learning_rate := 0;

  Result^.fullGPU_Training := True;
end;

class procedure TAI.Free_LMetric_ResNet_Parameter(param: PMetric_ResNet_Train_Parameter);
begin
  Free_P_Bytes(param^.train_sync_file);
  Free_P_Bytes(param^.train_output);
  Dispose(param);
end;

function TAI.LMetric_ResNet_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TMemoryRaster2DArray; param: PMetric_ResNet_Train_Parameter): Boolean;
var
  i, j, imgSum, ri: Integer;
  imgArry: TMemoryRasterArray;
  rArry: array of TAI_Raster_Data;
begin
  Result := False;

  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.LMDNN_ResNet_Train) then
      exit;
  if not Assigned(FAI_EntryAPI^.LMDNN_ResNet_Full_GPU_Train) then
      exit;

  imgSum := 0;
  for i := 0 to Length(imgList) - 1 do
      inc(imgSum, Length(imgList[i]));

  if imgSum = 0 then
      exit;

  { process sequence }
  SetLength(rArry, imgSum);
  ri := 0;
  for i := 0 to Length(imgList) - 1 do
    begin
      imgArry := imgList[i];
      for j := 0 to Length(imgArry) - 1 do
        begin
          new(rArry[ri].raster_Hnd);
          rArry[ri].raster_Hnd^.Raster := imgArry[j];
          if LargeScale_ then
            begin
              rArry[ri].raster_ptr := nil;
              imgArry[j].SerializedAndRecycleMemory(RSeri);
            end
          else
              rArry[ri].raster_ptr := imgArry[j].Bits;

          rArry[ri].Width := imgArry[j].Width;
          rArry[ri].Height := imgArry[j].Height;
          rArry[ri].index := i;
          inc(ri);
        end;
    end;

  { set arry }
  param^.imgArry_ptr := PAI_Raster_Data_Array(@rArry[0]);
  param^.img_num := Length(rArry);
  param^.control := @TrainingControl;

  { execute training }
  TrainingControl.pause := 0;
  TrainingControl.stop := 0;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := True;
      FAI_EntryAPI^.RasterSerialized := RSeri;
    end
  else
      FAI_EntryAPI^.RasterSerialized := nil;

  FAI_EntryAPI^.SerializedTime := GetTimeTick();

  try
    { run training }
    if param^.fullGPU_Training then
        Result := FAI_EntryAPI^.LMDNN_ResNet_Full_GPU_Train(param) >= 0
    else
        Result := FAI_EntryAPI^.LMDNN_ResNet_Train(param) >= 0;
  except
      Result := False;
  end;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := False;
      FAI_EntryAPI^.RasterSerialized := nil;
    end;

  Last_training_average_loss := param^.training_average_loss;
  Last_training_learning_rate := param^.training_learning_rate;
  completed_learning_rate := param^.completed_learning_rate;

  { reset arry }
  param^.imgArry_ptr := nil;
  param^.img_num := 0;

  { free }
  for i := 0 to Length(rArry) - 1 do
      Dispose(rArry[i].raster_Hnd);
  SetLength(rArry, 0);
end;

function TAI.LMetric_ResNet_Train(Snapshot_: Boolean; imgList: TAI_ImageList; param: PMetric_ResNet_Train_Parameter): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.LMDNN_ResNet_Train) then
      exit;

  if Snapshot_ then
    begin
      imgList.CalibrationNoDetectorDefine('');
      imgBuff := imgList.ExtractDetectorDefineAsSnapshotProjection(C_LMetric_Input_Size, C_LMetric_Input_Size);
    end
  else
    begin
      imgBuff := imgList.ExtractDetectorDefineAsPrepareRaster(C_LMetric_Input_Size, C_LMetric_Input_Size);
    end;

  if Length(imgBuff) = 0 then
      exit;

  Result := LMetric_ResNet_Train(False, nil, imgBuff, param);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.LMetric_ResNet_Train_Stream(Snapshot_: Boolean; imgList: TAI_ImageList; param: PMetric_ResNet_Train_Parameter): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if LMetric_ResNet_Train(Snapshot_, imgList, param) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.LMetric_ResNet_Train(Snapshot_: Boolean; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  imgL: TAI_ImageList;
  detDef: TAI_DetectorDefine;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.LMDNN_ResNet_Train) then
      exit;

  if Snapshot_ then
    begin
      DoStatus('Calibration LMetric dataset.');
      for i := 0 to imgMat.Count - 1 do
        begin
          imgL := imgMat[i];
          imgL.CalibrationNoDetectorDefine(imgL.FileInfo);
          imgL.CalibrationNullToken(imgL.FileInfo);
          for j := 0 to imgL.Count - 1 do
            if imgL[j].DetectorDefineList.Count = 0 then
              begin
                detDef := TAI_DetectorDefine.Create(imgL[j]);
                detDef.R := imgL[j].Raster.BoundsRect;
                detDef.Token := imgL.FileInfo;
                imgL[j].DetectorDefineList.Add(detDef);
              end;
        end;
      imgBuff := imgMat.ExtractDetectorDefineAsSnapshotProjection(C_LMetric_Input_Size, C_LMetric_Input_Size)
    end
  else
      imgBuff := imgMat.ExtractDetectorDefineAsPrepareRaster(C_LMetric_Input_Size, C_LMetric_Input_Size);

  if Length(imgBuff) = 0 then
      exit;

  Result := LMetric_ResNet_Train(False, nil, imgBuff, param);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.LMetric_ResNet_Train_Stream(Snapshot_: Boolean; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if LMetric_ResNet_Train(Snapshot_, imgMat, param) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.LMetric_ResNet_Train(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  imgL: TAI_ImageList;
  detDef: TAI_DetectorDefine;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.LMDNN_ResNet_Train) then
      exit;

  if Snapshot_ then
    begin
      DoStatus('Calibration LMetric dataset.');
      for i := 0 to imgMat.Count - 1 do
        begin
          imgL := imgMat[i];
          imgL.CalibrationNoDetectorDefine(imgL.FileInfo);
          imgL.CalibrationNullToken(imgL.FileInfo);
          for j := 0 to imgL.Count - 1 do
            if imgL[j].DetectorDefineList.Count = 0 then
              begin
                detDef := TAI_DetectorDefine.Create(imgL[j]);
                detDef.R := imgL[j].Raster.BoundsRect;
                detDef.Token := imgL.FileInfo;
                imgL[j].DetectorDefineList.Add(detDef);
              end;
        end;

      if LargeScale_ then
          imgBuff := imgMat.LargeScale_ExtractDetectorDefineAsSnapshotProjection(RSeri, C_LMetric_Input_Size, C_LMetric_Input_Size)
      else
          imgBuff := imgMat.ExtractDetectorDefineAsSnapshotProjection(C_LMetric_Input_Size, C_LMetric_Input_Size);
    end
  else
    begin
      if LargeScale_ then
          imgBuff := imgMat.LargeScale_ExtractDetectorDefineAsPrepareRaster(RSeri, C_LMetric_Input_Size, C_LMetric_Input_Size)
      else
          imgBuff := imgMat.ExtractDetectorDefineAsPrepareRaster(C_LMetric_Input_Size, C_LMetric_Input_Size);
    end;

  if Length(imgBuff) = 0 then
      exit;

  Result := LMetric_ResNet_Train(LargeScale_, RSeri, imgBuff, param);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.LMetric_ResNet_Train_Stream(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if LMetric_ResNet_Train(Snapshot_, LargeScale_, RSeri, imgMat, param) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

class function TAI.Build_LMetric_ResNet_Learn(): TLearn;
var
  L: TLearn;
begin
  L := TLearn.CreateClassifier(ltKDT, zAI.C_LMetric_Dim);
  Result := L;
end;

function TAI.LMetric_ResNet_Open(train_file: SystemString): TLMetric_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LMDNN_ResNet_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      Result := FAI_EntryAPI^.LMDNN_ResNet_Init(train_file_buff);
      Free_P_Bytes(train_file_buff);
      if Result <> nil then
          DoStatus('Large-MDNN-ResNet(ResNet metric DNN) open: %s', [train_file]);
    end
  else
      Result := nil;
end;

function TAI.LMetric_ResNet_Open_Stream(stream: TMemoryStream64): TLMetric_Handle;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LMDNN_ResNet_Init_Memory) then
    begin
      Result := FAI_EntryAPI^.LMDNN_ResNet_Init_Memory(stream.memory, stream.Size);
      if Result <> nil then
          DoStatus('Large-MDNN-ResNet(ResNet metric DNN) open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.LMetric_ResNet_Open_Stream(train_file: SystemString): TLMetric_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := LMetric_ResNet_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('Large-MDNN-ResNet(ResNet metric DNN) open: %s', [train_file]);
end;

function TAI.LMetric_ResNet_Close(var hnd: TLMetric_Handle): Boolean;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LMDNN_ResNet_Free) and (hnd <> nil) then
    begin
      Result := FAI_EntryAPI^.LMDNN_ResNet_Free(hnd) = 0;
      DoStatus('Large-MDNN-ResNet(ResNet metric DNN) close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

function TAI.LMetric_ResNet_Process(hnd: TLMetric_Handle; RasterArray: TMemoryRasterArray; output: PDouble): Integer;
var
  rArry: array of TAI_Raster_Data;
  nr: TMemoryRaster;
  i: Integer;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LMDNN_ResNet_Process) then
    begin
      SetLength(rArry, Length(RasterArray));
      for i := 0 to Length(RasterArray) - 1 do
        begin
          new(rArry[i].raster_Hnd);

          nr := NewRaster();

          { projection }
          if (RasterArray[i].Width <> C_LMetric_Input_Size) or (RasterArray[i].Height <> C_LMetric_Input_Size) then
            begin
              nr.SetSize(C_LMetric_Input_Size, C_LMetric_Input_Size);
              RasterArray[i].ProjectionTo(nr,
                TV2Rect4.Init(RectFit(C_LMetric_Input_Size, C_LMetric_Input_Size, RasterArray[i].BoundsRectV2), 0),
                TV2Rect4.Init(nr.BoundsRectV2, 0),
                True, 1.0);
            end
          else { fast assign }
              nr.SetWorkMemory(RasterArray[i]);

          rArry[i].raster_Hnd^.Raster := nr;

          rArry[i].raster_ptr := nr.Bits;
          rArry[i].Width := nr.Width;
          rArry[i].Height := nr.Height;
          rArry[i].index := i;
        end;

      FAI_EntryAPI^.RasterSerialized := nil;
      FAI_EntryAPI^.SerializedTime := GetTimeTick();

      Result := FAI_EntryAPI^.LMDNN_ResNet_Process(hnd, PAI_Raster_Data_Array(@rArry[0]), Length(rArry), output);

      for i := 0 to Length(rArry) - 1 do
        begin
          DisposeObject(rArry[i].raster_Hnd^.Raster);
          Dispose(rArry[i].raster_Hnd);
        end;
      SetLength(rArry, 0);
    end
  else
      Result := -2;
end;

function TAI.LMetric_ResNet_Process(hnd: TLMetric_Handle; RasterArray: TMemoryRasterArray): TLMatrix;
var
  L: TLVec;
  i: TLInt;
begin
  Result := LMatrix(0, 0);
  SetLength(L, Length(RasterArray) * C_LMetric_Dim);
  if LMetric_ResNet_Process(hnd, RasterArray, @L[0]) > 0 then
    begin
      Result := LMatrix(Length(RasterArray), 0);
      for i := Low(Result) to high(Result) do
          Result[i] := LVecCopy(L, i * C_LMetric_Dim, C_LMetric_Dim);
    end;
  SetLength(L, 0);
end;

function TAI.LMetric_ResNet_Process(hnd: TLMetric_Handle; Raster: TMemoryRaster): TLVec;
var
  rArry: TMemoryRasterArray;
begin
  Raster.ReadyBits();
  SetLength(Result, C_LMetric_Dim);
  SetLength(rArry, 1);
  rArry[0] := Raster;
  if LMetric_ResNet_Process(hnd, rArry, @Result[0]) <= 0 then
      SetLength(Result, 0);
end;

procedure TAI.LMetric_ResNet_SaveToLearnEngine_DT(pool_: TAI_DNN_ThreadPool; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; lr: TLearn);
var
  i, j: Integer;
  imgData: TAI_Image;
  detDef: TAI_DetectorDefine;
  p: PLMetric_ResNet_SaveToLearnEngine_DT_UserData_;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      imgData := imgList[i];
      if RSeri <> nil then
          imgData.UnserializedMemory(RSeri);
      if Snapshot_ then
        begin
          new(p);
          p^.lr := lr;
          p^.Snapshot := True;
          p^.imgData := imgData;
          p^.detDef := nil;
          TAI_DNN_Thread_LMetric(pool_.MinLoad_DNN_Thread).ProcessC(p, imgData.Raster.Clone, True, {$IFDEF FPC}@{$ENDIF FPC}LMetric_ResNet_SaveToLearnEngine_DT_Backcall);
        end
      else
        for j := 0 to imgData.DetectorDefineList.Count - 1 do
          begin
            detDef := imgData.DetectorDefineList[j];
            if detDef.Token.Len > 0 then
              begin
                new(p);
                p^.lr := lr;
                p^.Snapshot := False;
                p^.imgData := nil;
                p^.detDef := detDef;

                if detDef.PrepareRaster.Empty then
                    TAI_DNN_Thread_LMetric(pool_.MinLoad_DNN_Thread).ProcessC(p, detDef.Owner.Raster.BuildAreaOffsetScaleSpace(detDef.R, C_LMetric_Input_Size, C_LMetric_Input_Size),
                    True, {$IFDEF FPC}@{$ENDIF FPC}LMetric_ResNet_SaveToLearnEngine_DT_Backcall)
                else
                    TAI_DNN_Thread_LMetric(pool_.MinLoad_DNN_Thread).ProcessC(p, detDef.PrepareRaster.Clone, True, {$IFDEF FPC}@{$ENDIF FPC}LMetric_ResNet_SaveToLearnEngine_DT_Backcall);
              end;
          end;
      if RSeri <> nil then
          imgData.SerializedAndRecycleMemory(RSeri);
    end;
end;

procedure TAI.LMetric_ResNet_SaveToLearnEngine_DT(LMetric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; lr: TLearn);
var
  pool_: TAI_DNN_ThreadPool;
  i: Integer;
  Device_: TLIVec;
begin
  if lr.InSize <> C_LMetric_Dim then
      RaiseInfo('Learn Engine Insize illegal');
  pool_ := TAI_DNN_ThreadPool.Create;

  GetComputeDeviceOfTraining(Device_);
  for i in Device_ do
      pool_.BuildDeviceThread(FAI_EntryAPI, i, 2, TAI_DNN_Thread_LMetric);

  for i := 0 to pool_.Count - 1 do
      TAI_DNN_Thread_LMetric(pool_[i]).Open_Stream(LMetric_stream);
  LMetric_ResNet_SaveToLearnEngine_DT(pool_, Snapshot_, RSeri, imgList, lr);
  pool_.Wait();
  DisposeObject(pool_);
end;

procedure TAI.LMetric_ResNet_SaveToLearnEngine_DT(LMetric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; lr: TLearn);
var
  pool_: TAI_DNN_ThreadPool;
  i: Integer;
  Device_: TLIVec;
begin
  if lr.InSize <> C_LMetric_Dim then
      RaiseInfo('Learn Engine Insize illegal');
  pool_ := TAI_DNN_ThreadPool.Create;

  GetComputeDeviceOfTraining(Device_);
  for i in Device_ do
      pool_.BuildDeviceThread(FAI_EntryAPI, i, 2, TAI_DNN_Thread_LMetric);

  for i := 0 to pool_.Count - 1 do
      TAI_DNN_Thread_LMetric(pool_[i]).Open_Stream(LMetric_stream);
  for i := 0 to imgMat.Count - 1 do
      LMetric_ResNet_SaveToLearnEngine_DT(pool_, Snapshot_, RSeri, imgMat[i], lr);
  pool_.Wait();
  DisposeObject(pool_);
end;

procedure TAI.LMetric_ResNet_SaveToLearnEngine_DT(LMetric_stream: TMemoryStream64; Snapshot_: Boolean; imgList: TAI_ImageList; lr: TLearn);
begin
  LMetric_ResNet_SaveToLearnEngine_DT(LMetric_stream, Snapshot_, nil, imgList, lr);
end;

procedure TAI.LMetric_ResNet_SaveToLearnEngine_DT(LMetric_stream: TMemoryStream64; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; lr: TLearn);
begin
  LMetric_ResNet_SaveToLearnEngine_DT(LMetric_stream, Snapshot_, nil, imgMat, lr);
end;

procedure TAI.LMetric_ResNet_SaveToLearnEngine(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; lr: TLearn);
var
  i, j: Integer;
  imgData: TAI_Image;
  detDef: TAI_DetectorDefine;
  mr: TMemoryRaster;
  V: TLVec;
begin
  if lr.InSize <> C_LMetric_Dim then
      RaiseInfo('Learn Engine Insize illegal');
  for i := 0 to imgList.Count - 1 do
    begin
      imgData := imgList[i];
      if RSeri <> nil then
          imgData.UnserializedMemory(RSeri);
      if Snapshot_ then
        begin
          mr := imgData.Raster;
          V := LMetric_ResNet_Process(LMetric_hnd, mr);
          if Length(V) <> C_LMetric_Dim then
              DoStatus('LMetric-ResNet vector error!')
          else
            begin
              for j := 0 to imgData.DetectorDefineList.Count - 1 do
                begin
                  detDef := imgData.DetectorDefineList[j];
                  if detDef.Token.Len > 0 then
                      lr.AddMemory(V, detDef.Token);
                end;
            end;
        end
      else
        for j := 0 to imgData.DetectorDefineList.Count - 1 do
          begin
            detDef := imgData.DetectorDefineList[j];
            if detDef.Token.Len > 0 then
              begin
                if detDef.PrepareRaster.Empty then
                  begin
                    mr := detDef.Owner.Raster.BuildAreaOffsetScaleSpace(detDef.R, C_LMetric_Input_Size, C_LMetric_Input_Size);
                    V := LMetric_ResNet_Process(LMetric_hnd, mr);
                    DisposeObject(mr);
                  end
                else
                  begin
                    mr := detDef.PrepareRaster;
                    V := LMetric_ResNet_Process(LMetric_hnd, mr);
                  end;
                if Length(V) <> C_LMetric_Dim then
                    DoStatus('LMetric-ResNet vector error!')
                else
                    lr.AddMemory(V, detDef.Token);
              end;
          end;
      if RSeri <> nil then
          imgData.SerializedAndRecycleMemory(RSeri);
    end;
end;

procedure TAI.LMetric_ResNet_SaveToLearnEngine(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; lr: TLearn);
var
  i: Integer;
begin
  for i := 0 to imgMat.Count - 1 do
      LMetric_ResNet_SaveToLearnEngine(LMetric_hnd, Snapshot_, RSeri, imgMat[i], lr);
end;

procedure TAI.LMetric_ResNet_SaveToLearnEngine(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; imgList: TAI_ImageList; lr: TLearn);
begin
  LMetric_ResNet_SaveToLearnEngine(LMetric_hnd, Snapshot_, nil, imgList, lr);
end;

procedure TAI.LMetric_ResNet_SaveToLearnEngine(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; lr: TLearn);
begin
  LMetric_ResNet_SaveToLearnEngine(LMetric_hnd, Snapshot_, nil, imgMat, lr);
end;

procedure TAI.LMetric_ResNet_SaveToKDTree(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; kd: TKDTreeDataList);
var
  i, j: Integer;
  imgData: TAI_Image;
  detDef: TAI_DetectorDefine;
  mr: TMemoryRaster;
  V: TLVec;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      imgData := imgList[i];
      if RSeri <> nil then
          imgData.UnserializedMemory(RSeri);
      if Snapshot_ then
        begin
          mr := imgData.Raster;
          V := LMetric_ResNet_Process(LMetric_hnd, mr);
          if Length(V) <> C_LMetric_Dim then
              DoStatus('LMetric-ResNet vector error!')
          else
            begin
              for j := 0 to imgData.DetectorDefineList.Count - 1 do
                begin
                  detDef := imgData.DetectorDefineList[j];
                  if detDef.Token.Len > 0 then
                      kd.Add(V, detDef.Token);
                end;
            end;
        end
      else
        for j := 0 to imgData.DetectorDefineList.Count - 1 do
          begin
            detDef := imgData.DetectorDefineList[j];
            if detDef.Token.Len > 0 then
              begin
                if detDef.PrepareRaster.Empty then
                  begin
                    mr := detDef.Owner.Raster.BuildAreaOffsetScaleSpace(detDef.R, C_LMetric_Input_Size, C_LMetric_Input_Size);
                    V := LMetric_ResNet_Process(LMetric_hnd, mr);
                    DisposeObject(mr);
                  end
                else
                  begin
                    mr := detDef.PrepareRaster;
                    V := LMetric_ResNet_Process(LMetric_hnd, mr);
                  end;

                if Length(V) <> C_LMetric_Dim then
                    DoStatus('LMetric-ResNet vector error!')
                else
                    kd.Add(V, detDef.Token);
              end;
          end;
      if RSeri <> nil then
          imgData.SerializedAndRecycleMemory(RSeri);
    end;
end;

procedure TAI.LMetric_ResNet_SaveToKDTree(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; kd: TKDTreeDataList);
var
  i: Integer;
begin
  for i := 0 to imgMat.Count - 1 do
    begin
      LMetric_ResNet_SaveToKDTree(LMetric_hnd, Snapshot_, RSeri, imgMat[i], kd);
    end;
end;

procedure TAI.LMetric_ResNet_SaveToKDTree(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; imgList: TAI_ImageList; kd: TKDTreeDataList);
begin
  LMetric_ResNet_SaveToKDTree(LMetric_hnd, Snapshot_, nil, imgList, kd);
end;

procedure TAI.LMetric_ResNet_SaveToKDTree(LMetric_hnd: TLMetric_Handle; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; kd: TKDTreeDataList);
begin
  LMetric_ResNet_SaveToKDTree(LMetric_hnd, Snapshot_, nil, imgMat, kd);
end;

function TAI.LMetric_ResNet_DebugInfo(hnd: TLMetric_Handle): U_String;
var
  p: PPascalString;
begin
  Result := '';
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MDNN_DebugInfo) and (hnd <> nil) then
    begin
      FAI_EntryAPI^.MDNN_DebugInfo(hnd, p);
      Result := p^;
      Dispose(p);
    end;
end;

class function TAI.Init_MMOD6L_DNN_TrainParam(train_cfg, train_sync_file, train_output: U_String): PMMOD_Train_Parameter;
begin
  new(Result);
  FillPtrByte(Result, SizeOf(TMMOD_Train_Parameter), 0);

  Result^.train_cfg := Alloc_P_Bytes(train_cfg);
  Result^.train_sync_file := Alloc_P_Bytes(train_sync_file);
  Result^.train_output := Alloc_P_Bytes(train_output);

  Result^.timeout := C_Tick_Hour;
  Result^.weight_decay := 0.0005;
  Result^.momentum := 0.9;
  Result^.target_size := 80;
  Result^.min_target_size := 30;
  Result^.min_detector_window_overlap_iou := 0.75;
  Result^.iterations_without_progress_threshold := 800;
  Result^.learning_rate := 0.1;
  Result^.completed_learning_rate := 0.0001;
  Result^.saveMemory := 0;
  Result^.overlap_NMS_iou_thresh := 0.4;
  Result^.overlap_NMS_percent_covered_thresh := 1.0;
  Result^.overlap_ignore_iou_thresh := 0.5;
  Result^.overlap_ignore_percent_covered_thresh := 0.95;
  Result^.prepare_crops_img_num := 5;
  Result^.num_crops := 20;
  Result^.chip_dims_x := 300;
  Result^.chip_dims_y := 300;
  Result^.min_object_size_x := 75;
  Result^.min_object_size_y := 25;
  Result^.max_rotation_degrees := 10.0;
  Result^.max_object_size := 0.7;

  Result^.control := nil;
  Result^.training_average_loss := 0;
  Result^.training_learning_rate := 0;

  { internal }
  Result^.TempFiles := nil;
end;

class procedure TAI.Free_MMOD6L_DNN_TrainParam(param: PMMOD_Train_Parameter);
begin
  Free_P_Bytes(param^.train_cfg);
  Free_P_Bytes(param^.train_sync_file);
  Free_P_Bytes(param^.train_output);
  Dispose(param);
end;

function TAI.MMOD6L_DNN_PrepareTrain(imgList: TAI_ImageList; train_sync_file: U_String): PMMOD_Train_Parameter;
var
  ph, fn, prefix, train_out: U_String;
  tmpFileList: TPascalStringList;
begin
  ph := RootPath;
  tmpFileList := TPascalStringList.Create;
  TCoreClassThread.Sleep(1);
  prefix := 'MMOD6L_DNN_' + umlMakeRanName + '_';
  fn := umlCombineFileName(ph, prefix + 'temp.xml');
  imgList.Build_XML(True, False, 'ZAI dataset', 'dnn resnet max-margin dataset', fn, prefix, tmpFileList);
  train_out := prefix + 'output' + C_MMOD6L_Ext;
  Result := Init_MMOD6L_DNN_TrainParam(fn, train_sync_file, train_out);
  Result^.control := @TrainingControl;
  Result^.TempFiles := tmpFileList;
end;

function TAI.MMOD6L_DNN_PrepareTrain(imgMat: TAI_ImageMatrix; train_sync_file: U_String): PMMOD_Train_Parameter;
var
  ph, fn, prefix, train_out: U_String;
  tmpFileList: TPascalStringList;
begin
  ph := RootPath;
  tmpFileList := TPascalStringList.Create;
  TCoreClassThread.Sleep(1);
  prefix := 'MMOD6L_DNN_' + umlMakeRanName + '_';
  fn := umlCombineFileName(ph, prefix + 'temp.xml');
  imgMat.Build_XML(True, False, 'ZAI dataset', 'build-in', fn, prefix, tmpFileList);
  train_out := prefix + 'output' + C_MMOD6L_Ext;
  Result := Init_MMOD6L_DNN_TrainParam(fn, train_sync_file, train_out);
  Result^.control := @TrainingControl;
  Result^.TempFiles := tmpFileList;
end;

function TAI.MMOD6L_DNN_Train(param: PMMOD_Train_Parameter): Integer;
begin
  Result := -1;
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MMOD6L_DNN_Train) then
    begin
      TrainingControl.pause := 0;
      TrainingControl.stop := 0;
      FAI_EntryAPI^.RasterSerialized := nil;
      FAI_EntryAPI^.SerializedTime := GetTimeTick();
      param^.saveMemory := 0; { normal MMOD trainer. }
      Result := FAI_EntryAPI^.MMOD6L_DNN_Train(param);
      Last_training_average_loss := param^.training_average_loss;
      Last_training_learning_rate := param^.training_learning_rate;
      completed_learning_rate := param^.completed_learning_rate;
      if Result > 0 then
          param^.TempFiles.Add(Get_P_Bytes_String(param^.train_output));
    end;
end;

function TAI.MMOD6L_DNN_Train_Stream(param: PMMOD_Train_Parameter): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  fn := Get_P_Bytes_String(param^.train_output);
  if (MMOD6L_DNN_Train(param) > 0) and (umlFileExists(fn)) then
    begin
      Result := TMemoryStream64.Create;
      Result.LoadFromFile(fn);
      Result.Position := 0;
    end;
end;

procedure TAI.MMOD6L_DNN_FreeTrain(param: PMMOD_Train_Parameter);
var
  i: Integer;
begin
  if param^.TempFiles <> nil then
    begin
      for i := 0 to param^.TempFiles.Count - 1 do
          umlDeleteFile(param^.TempFiles[i]);
      DisposeObject(param^.TempFiles);
      param^.TempFiles := nil;
    end;
  Free_MMOD6L_DNN_TrainParam(param);
end;

function TAI.LargeScale_MMOD6L_DNN_PrepareTrain(train_sync_file, train_output: U_String): PMMOD_Train_Parameter;
begin
  Result := Init_MMOD6L_DNN_TrainParam('', train_sync_file, train_output);
  Result^.control := @TrainingControl;
end;

function TAI.LargeScale_MMOD6L_DNN_Train(param: PMMOD_Train_Parameter; imgList: TAI_ImageList): Integer;
var
  imgArry: array of TImage_Handle;
  imgArry_P: array of PImage_Handle;
  i: Integer;
begin
  Result := -1;
  SetLength(imgArry, imgList.Count);
  SetLength(imgArry_P, imgList.Count);

  for i := 0 to imgList.Count - 1 do
    begin
      imgArry[i].image := imgList[i];
      imgArry[i].AccessImage := 0;
      imgArry[i].AccessDetectorImage := 0;
      imgArry[i].AccessDetectorRect := 0;
      imgArry[i].AccessMask := 0;
      imgArry_P[i] := @imgArry[i];
    end;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LargeScale_MMOD6L_Train) then
    begin
      param^.control := @TrainingControl;
      param^.saveMemory := 0; { normal MMOD trainer. }
      try
          Result := FAI_EntryAPI^.LargeScale_MMOD6L_Train(param, @imgArry_P[0], imgList.Count);
      except
          Result := -1;
      end;
      Last_training_average_loss := param^.training_average_loss;
      Last_training_learning_rate := param^.training_learning_rate;
      completed_learning_rate := param^.completed_learning_rate;
    end;

  SetLength(imgArry, 0);
  SetLength(imgArry_P, 0);
end;

function TAI.LargeScale_MMOD6L_DNN_Train(param: PMMOD_Train_Parameter; imgMat: TAI_ImageMatrix): Integer;
var
  imgL: TImageList_Decl;
  imgArry: array of TImage_Handle;
  imgArry_P: array of PImage_Handle;
  i: Integer;
begin
  Result := -1;
  imgL := imgMat.ImageList();

  SetLength(imgArry, imgL.Count);
  SetLength(imgArry_P, imgL.Count);

  for i := 0 to imgL.Count - 1 do
    begin
      imgArry[i].image := imgL[i];
      imgArry[i].AccessImage := 0;
      imgArry[i].AccessDetectorImage := 0;
      imgArry[i].AccessDetectorRect := 0;
      imgArry[i].AccessMask := 0;
      imgArry_P[i] := @imgArry[i];
    end;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LargeScale_MMOD6L_Train) then
    begin
      param^.control := @TrainingControl;
      param^.saveMemory := 0; { normal MMOD trainer. }
      try
          Result := FAI_EntryAPI^.LargeScale_MMOD6L_Train(param, @imgArry_P[0], imgL.Count);
      except
          Result := -1;
      end;
      Last_training_average_loss := param^.training_average_loss;
      Last_training_learning_rate := param^.training_learning_rate;
      completed_learning_rate := param^.completed_learning_rate;
    end;

  SetLength(imgArry, 0);
  SetLength(imgArry_P, 0);
  DisposeObject(imgL);
end;

function TAI.LargeScale_MMOD6L_DNN_Train(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgList: TAI_ImageList): Integer;
var
  imgArry: array of TImage_Handle;
  imgArry_P: array of PImage_Handle;
  i: Integer;
begin
  Result := -1;
  SetLength(imgArry, imgList.Count);
  SetLength(imgArry_P, imgList.Count);

  for i := 0 to imgList.Count - 1 do
    begin
      imgArry[i].image := imgList[i];
      imgArry[i].AccessImage := 0;
      imgArry[i].AccessDetectorImage := 0;
      imgArry[i].AccessDetectorRect := 0;
      imgArry[i].AccessMask := 0;
      imgArry_P[i] := @imgArry[i];
    end;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LargeScale_MMOD6L_Train) then
    begin
      param^.control := @TrainingControl;

      param^.saveMemory := 1; { large-scale MMOD trainer. }
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := True;
      FAI_EntryAPI^.RasterSerialized := RSeri;
      imgList.SerializedAndRecycleMemory(RSeri);

      try
          Result := FAI_EntryAPI^.LargeScale_MMOD6L_Train(param, @imgArry_P[0], imgList.Count);
      except
          Result := -1;
      end;

      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := False;
      FAI_EntryAPI^.RasterSerialized := nil;

      Last_training_average_loss := param^.training_average_loss;
      Last_training_learning_rate := param^.training_learning_rate;
      completed_learning_rate := param^.completed_learning_rate;
    end;

  SetLength(imgArry, 0);
  SetLength(imgArry_P, 0);
end;

function TAI.LargeScale_MMOD6L_DNN_Train(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix): Integer;
var
  imgL: TImageList_Decl;
  imgArry: array of TImage_Handle;
  imgArry_P: array of PImage_Handle;
  i: Integer;
begin
  Result := -1;
  imgL := imgMat.ImageList();

  SetLength(imgArry, imgL.Count);
  SetLength(imgArry_P, imgL.Count);

  for i := 0 to imgL.Count - 1 do
    begin
      imgArry[i].image := imgL[i];
      imgArry[i].AccessImage := 0;
      imgArry[i].AccessDetectorImage := 0;
      imgArry[i].AccessDetectorRect := 0;
      imgArry[i].AccessMask := 0;
      imgArry_P[i] := @imgArry[i];
    end;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LargeScale_MMOD6L_Train) then
    begin
      param^.control := @TrainingControl;

      param^.saveMemory := 1; { large-scale MMOD trainer. }
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := True;
      FAI_EntryAPI^.RasterSerialized := RSeri;
      imgMat.SerializedAndRecycleMemory(RSeri);

      try
          Result := FAI_EntryAPI^.LargeScale_MMOD6L_Train(param, @imgArry_P[0], imgL.Count);
      except
          Result := -1;
      end;

      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := False;
      FAI_EntryAPI^.RasterSerialized := nil;

      Last_training_average_loss := param^.training_average_loss;
      Last_training_learning_rate := param^.training_learning_rate;
      completed_learning_rate := param^.completed_learning_rate;
    end;

  SetLength(imgArry, 0);
  SetLength(imgArry_P, 0);
  DisposeObject(imgL);
end;

function TAI.LargeScale_MMOD6L_DNN_Train_Stream(param: PMMOD_Train_Parameter; imgList: TAI_ImageList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  fn := Get_P_Bytes_String(param^.train_output);
  if (LargeScale_MMOD6L_DNN_Train(param, imgList) > 0) and (umlFileExists(fn)) then
    begin
      Result := TMemoryStream64.Create;
      Result.LoadFromFile(fn);
      Result.Position := 0;
    end;
end;

function TAI.LargeScale_MMOD6L_DNN_Train_Stream(param: PMMOD_Train_Parameter; imgMat: TAI_ImageMatrix): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  fn := Get_P_Bytes_String(param^.train_output);
  if (LargeScale_MMOD6L_DNN_Train(param, imgMat) > 0) and (umlFileExists(fn)) then
    begin
      Result := TMemoryStream64.Create;
      Result.LoadFromFile(fn);
      Result.Position := 0;
    end;
end;

function TAI.LargeScale_MMOD6L_DNN_Train_Stream(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgList: TAI_ImageList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  fn := Get_P_Bytes_String(param^.train_output);
  if (LargeScale_MMOD6L_DNN_Train(param, RSeri, imgList) > 0) and (umlFileExists(fn)) then
    begin
      Result := TMemoryStream64.Create;
      Result.LoadFromFile(fn);
      Result.Position := 0;
    end;
end;

function TAI.LargeScale_MMOD6L_DNN_Train_Stream(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  fn := Get_P_Bytes_String(param^.train_output);
  if (LargeScale_MMOD6L_DNN_Train(param, RSeri, imgMat) > 0) and (umlFileExists(fn)) then
    begin
      Result := TMemoryStream64.Create;
      Result.LoadFromFile(fn);
      Result.Position := 0;
    end;
end;

procedure TAI.LargeScale_MMOD6L_DNN_FreeTrain(param: PMMOD_Train_Parameter);
begin
  Free_MMOD6L_DNN_TrainParam(param);
end;

function TAI.MMOD6L_DNN_Open_Face(): TMMOD6L_Handle;
var
  m64: TMemoryStream64;
begin
  Wait_AI_Init;
  m64 := TMemoryStream64.Create;
  m64.SetPointerWithProtectedMode(build_in_face_detector_memory, build_in_face_detector_memory_siz);
  Result := MMOD6L_DNN_Open_Stream(m64);
  DisposeObject(m64);
end;

function TAI.MMOD6L_DNN_Open(train_file: SystemString): TMMOD6L_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MMOD6L_DNN_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      Result := FAI_EntryAPI^.MMOD6L_DNN_Init(train_file_buff);
      Free_P_Bytes(train_file_buff);
      if Result <> nil then
          DoStatus('MMOD-DNN(DNN+SVM:max-margin object detector) open: %s', [train_file]);
    end
  else
      Result := nil;
end;

function TAI.MMOD6L_DNN_Open_Stream(stream: TMemoryStream64): TMMOD6L_Handle;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MMOD6L_DNN_Init_Memory) then
    begin
      Result := FAI_EntryAPI^.MMOD6L_DNN_Init_Memory(stream.memory, stream.Size);
      if Result <> nil then
          DoStatus('MMOD-DNN(DNN+SVM:max-margin object detector) open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.MMOD6L_DNN_Open_Stream(train_file: SystemString): TMMOD6L_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := MMOD6L_DNN_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('MMOD-DNN(DNN+SVM:max-margin object detector) open: %s', [train_file]);
end;

function TAI.MMOD6L_DNN_Close(var hnd: TMMOD6L_Handle): Boolean;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MMOD6L_DNN_Free) and (hnd <> nil) then
    begin
      Result := FAI_EntryAPI^.MMOD6L_DNN_Free(hnd) = 0;
      DoStatus('MMOD-DNN(DNN+SVM:max-margin object detector) close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

function TAI.MMOD6L_DNN_Process(hnd: TMMOD6L_Handle; Raster: TMemoryRaster): TMMOD_Desc;
var
  rect_num: Integer;
  buff: TAI_MMOD_Desc;
  i: Integer;
begin
  Raster.ReadyBits();
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.MMOD6L_DNN_Process) then
      exit;
  SetLength(buff, 8192);

  rect_num := FAI_EntryAPI^.MMOD6L_DNN_Process(hnd, Raster.Bits, Raster.Width, Raster.Height, @buff[0], Length(buff));

  if rect_num >= 0 then
    begin
      SetLength(Result, rect_num);
      for i := 0 to rect_num - 1 do
        begin
          Result[i].R := RectV2(buff[i]);
          Result[i].confidence := buff[i].confidence;
          Result[i].Token := buff[i].Token^;
          API_FreeString(buff[i].Token);
        end;
    end;
  SetLength(buff, 0);

  FilterMMOD_Desc(Result);
end;

function TAI.MMOD6L_DNN_Process_Matrix(hnd: TMMOD6L_Handle; Matrix_IMG: TMatrix_Image_Handle): TMMOD_Desc;
var
  rect_num: Integer;
  buff: TAI_MMOD_Desc;
  i: Integer;
begin
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.MMOD6L_DNN_Process) then
      exit;
  SetLength(buff, 8192);

  rect_num := FAI_EntryAPI^.MMOD6L_DNN_Process_Image(hnd, Matrix_IMG, @buff[0], Length(buff));
  if rect_num >= 0 then
    begin
      SetLength(Result, rect_num);
      for i := 0 to rect_num - 1 do
        begin
          Result[i].R := RectV2(buff[i]);
          Result[i].confidence := buff[i].confidence;
          Result[i].Token := buff[i].Token^;
          API_FreeString(buff[i].Token);
        end;
    end;
  SetLength(buff, 0);

  FilterMMOD_Desc(Result);
end;

function TAI.MMOD6L_DNN_DebugInfo(hnd: TMMOD6L_Handle): U_String;
var
  p: PPascalString;
begin
  Result := '';
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MMOD6L_DebugInfo) and (hnd <> nil) then
    begin
      FAI_EntryAPI^.MMOD6L_DebugInfo(hnd, p);
      Result := p^;
      Dispose(p);
    end;
end;

class function TAI.Init_MMOD3L_DNN_TrainParam(train_cfg, train_sync_file, train_output: U_String): PMMOD_Train_Parameter;
begin
  new(Result);
  FillPtrByte(Result, SizeOf(TMMOD_Train_Parameter), 0);

  Result^.train_cfg := Alloc_P_Bytes(train_cfg);
  Result^.train_sync_file := Alloc_P_Bytes(train_sync_file);
  Result^.train_output := Alloc_P_Bytes(train_output);

  Result^.timeout := C_Tick_Hour;
  Result^.weight_decay := 0.0005;
  Result^.momentum := 0.9;
  Result^.target_size := 80;
  Result^.min_target_size := 30;
  Result^.min_detector_window_overlap_iou := 0.75;
  Result^.iterations_without_progress_threshold := 800;
  Result^.learning_rate := 0.1;
  Result^.completed_learning_rate := 0.0001;
  Result^.saveMemory := 0;
  Result^.overlap_NMS_iou_thresh := 0.4;
  Result^.overlap_NMS_percent_covered_thresh := 1.0;
  Result^.overlap_ignore_iou_thresh := 0.5;
  Result^.overlap_ignore_percent_covered_thresh := 0.95;
  Result^.prepare_crops_img_num := 5;
  Result^.num_crops := 20;
  Result^.chip_dims_x := 300;
  Result^.chip_dims_y := 300;
  Result^.min_object_size_x := 75;
  Result^.min_object_size_y := 25;
  Result^.max_rotation_degrees := 10.0;
  Result^.max_object_size := 0.7;

  Result^.control := nil;
  Result^.training_average_loss := 0;
  Result^.training_learning_rate := 0;

  { internal }
  Result^.TempFiles := nil;
end;

class procedure TAI.Free_MMOD3L_DNN_TrainParam(param: PMMOD_Train_Parameter);
begin
  Free_P_Bytes(param^.train_cfg);
  Free_P_Bytes(param^.train_sync_file);
  Free_P_Bytes(param^.train_output);
  Dispose(param);
end;

function TAI.MMOD3L_DNN_PrepareTrain(imgList: TAI_ImageList; train_sync_file: U_String): PMMOD_Train_Parameter;
var
  ph, fn, prefix, train_out: U_String;
  tmpFileList: TPascalStringList;
begin
  ph := RootPath;
  tmpFileList := TPascalStringList.Create;
  TCoreClassThread.Sleep(1);
  prefix := 'MMOD3L_DNN_' + umlMakeRanName + '_';
  fn := umlCombineFileName(ph, prefix + 'temp.xml');
  imgList.Build_XML(True, False, 'ZAI dataset', 'dnn resnet max-margin dataset', fn, prefix, tmpFileList);
  train_out := prefix + 'output' + C_MMOD3L_Ext;
  Result := Init_MMOD3L_DNN_TrainParam(fn, train_sync_file, train_out);
  Result^.control := @TrainingControl;
  Result^.TempFiles := tmpFileList;
end;

function TAI.MMOD3L_DNN_PrepareTrain(imgMat: TAI_ImageMatrix; train_sync_file: U_String): PMMOD_Train_Parameter;
var
  ph, fn, prefix, train_out: U_String;
  tmpFileList: TPascalStringList;
begin
  ph := RootPath;
  tmpFileList := TPascalStringList.Create;
  TCoreClassThread.Sleep(1);
  prefix := 'MMOD3L_DNN_' + umlMakeRanName + '_';
  fn := umlCombineFileName(ph, prefix + 'temp.xml');
  imgMat.Build_XML(True, False, 'ZAI dataset', 'build-in', fn, prefix, tmpFileList);
  train_out := prefix + 'output' + C_MMOD3L_Ext;
  Result := Init_MMOD3L_DNN_TrainParam(fn, train_sync_file, train_out);
  Result^.control := @TrainingControl;
  Result^.TempFiles := tmpFileList;
end;

function TAI.MMOD3L_DNN_Train(param: PMMOD_Train_Parameter): Integer;
begin
  Result := -1;
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MMOD3L_DNN_Train) then
    begin
      TrainingControl.pause := 0;
      TrainingControl.stop := 0;
      FAI_EntryAPI^.RasterSerialized := nil;
      FAI_EntryAPI^.SerializedTime := GetTimeTick();
      param^.saveMemory := 0; { normal MMOD trainer. }
      Result := FAI_EntryAPI^.MMOD3L_DNN_Train(param);
      Last_training_average_loss := param^.training_average_loss;
      Last_training_learning_rate := param^.training_learning_rate;
      completed_learning_rate := param^.completed_learning_rate;
      if Result > 0 then
          param^.TempFiles.Add(Get_P_Bytes_String(param^.train_output));
    end;
end;

function TAI.MMOD3L_DNN_Train_Stream(param: PMMOD_Train_Parameter): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  fn := Get_P_Bytes_String(param^.train_output);
  if (MMOD3L_DNN_Train(param) > 0) and (umlFileExists(fn)) then
    begin
      Result := TMemoryStream64.Create;
      Result.LoadFromFile(fn);
      Result.Position := 0;
    end;
end;

procedure TAI.MMOD3L_DNN_FreeTrain(param: PMMOD_Train_Parameter);
var
  i: Integer;
begin
  if param^.TempFiles <> nil then
    begin
      for i := 0 to param^.TempFiles.Count - 1 do
          umlDeleteFile(param^.TempFiles[i]);
      DisposeObject(param^.TempFiles);
      param^.TempFiles := nil;
    end;
  Free_MMOD3L_DNN_TrainParam(param);
end;

function TAI.LargeScale_MMOD3L_DNN_PrepareTrain(train_sync_file, train_output: U_String): PMMOD_Train_Parameter;
begin
  Result := Init_MMOD3L_DNN_TrainParam('', train_sync_file, train_output);
  Result^.control := @TrainingControl;
end;

function TAI.LargeScale_MMOD3L_DNN_Train(param: PMMOD_Train_Parameter; imgList: TAI_ImageList): Integer;
var
  imgArry: array of TImage_Handle;
  imgArry_P: array of PImage_Handle;
  i: Integer;
begin
  Result := -1;
  SetLength(imgArry, imgList.Count);
  SetLength(imgArry_P, imgList.Count);

  for i := 0 to imgList.Count - 1 do
    begin
      imgArry[i].image := imgList[i];
      imgArry[i].AccessImage := 0;
      imgArry[i].AccessDetectorImage := 0;
      imgArry[i].AccessDetectorRect := 0;
      imgArry[i].AccessMask := 0;
      imgArry_P[i] := @imgArry[i];
    end;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LargeScale_MMOD3L_Train) then
    begin
      param^.control := @TrainingControl;
      param^.saveMemory := 0; { normal MMOD trainer. }
      try
          Result := FAI_EntryAPI^.LargeScale_MMOD3L_Train(param, @imgArry_P[0], imgList.Count);
      except
          Result := -1;
      end;
      Last_training_average_loss := param^.training_average_loss;
      Last_training_learning_rate := param^.training_learning_rate;
      completed_learning_rate := param^.completed_learning_rate;
    end;

  SetLength(imgArry, 0);
  SetLength(imgArry_P, 0);
end;

function TAI.LargeScale_MMOD3L_DNN_Train(param: PMMOD_Train_Parameter; imgMat: TAI_ImageMatrix): Integer;
var
  imgL: TImageList_Decl;
  imgArry: array of TImage_Handle;
  imgArry_P: array of PImage_Handle;
  i: Integer;
begin
  Result := -1;
  imgL := imgMat.ImageList();

  SetLength(imgArry, imgL.Count);
  SetLength(imgArry_P, imgL.Count);

  for i := 0 to imgL.Count - 1 do
    begin
      imgArry[i].image := imgL[i];
      imgArry[i].AccessImage := 0;
      imgArry[i].AccessDetectorImage := 0;
      imgArry[i].AccessDetectorRect := 0;
      imgArry[i].AccessMask := 0;
      imgArry_P[i] := @imgArry[i];
    end;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LargeScale_MMOD3L_Train) then
    begin
      param^.control := @TrainingControl;
      param^.saveMemory := 0; { normal MMOD trainer. }
      try
          Result := FAI_EntryAPI^.LargeScale_MMOD3L_Train(param, @imgArry_P[0], imgL.Count);
      except
          Result := -1;
      end;
      Last_training_average_loss := param^.training_average_loss;
      Last_training_learning_rate := param^.training_learning_rate;
      completed_learning_rate := param^.completed_learning_rate;
    end;

  SetLength(imgArry, 0);
  SetLength(imgArry_P, 0);
  DisposeObject(imgL);
end;

function TAI.LargeScale_MMOD3L_DNN_Train(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgList: TAI_ImageList): Integer;
var
  imgArry: array of TImage_Handle;
  imgArry_P: array of PImage_Handle;
  i: Integer;
begin
  Result := -1;
  SetLength(imgArry, imgList.Count);
  SetLength(imgArry_P, imgList.Count);

  for i := 0 to imgList.Count - 1 do
    begin
      imgArry[i].image := imgList[i];
      imgArry[i].AccessImage := 0;
      imgArry[i].AccessDetectorImage := 0;
      imgArry[i].AccessDetectorRect := 0;
      imgArry[i].AccessMask := 0;
      imgArry_P[i] := @imgArry[i];
    end;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LargeScale_MMOD3L_Train) then
    begin
      param^.control := @TrainingControl;

      param^.saveMemory := 1; { large-scale MMOD trainer. }
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := True;
      FAI_EntryAPI^.RasterSerialized := RSeri;
      imgList.SerializedAndRecycleMemory(RSeri);

      try
          Result := FAI_EntryAPI^.LargeScale_MMOD3L_Train(param, @imgArry_P[0], imgList.Count);
      except
          Result := -1;
      end;

      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := False;
      FAI_EntryAPI^.RasterSerialized := nil;

      Last_training_average_loss := param^.training_average_loss;
      Last_training_learning_rate := param^.training_learning_rate;
      completed_learning_rate := param^.completed_learning_rate;
    end;

  SetLength(imgArry, 0);
  SetLength(imgArry_P, 0);
end;

function TAI.LargeScale_MMOD3L_DNN_Train(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix): Integer;
var
  imgL: TImageList_Decl;
  imgArry: array of TImage_Handle;
  imgArry_P: array of PImage_Handle;
  i: Integer;
begin
  Result := -1;
  imgL := imgMat.ImageList();

  SetLength(imgArry, imgL.Count);
  SetLength(imgArry_P, imgL.Count);

  for i := 0 to imgL.Count - 1 do
    begin
      imgArry[i].image := imgL[i];
      imgArry[i].AccessImage := 0;
      imgArry[i].AccessDetectorImage := 0;
      imgArry[i].AccessDetectorRect := 0;
      imgArry[i].AccessMask := 0;
      imgArry_P[i] := @imgArry[i];
    end;

  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LargeScale_MMOD3L_Train) then
    begin
      param^.control := @TrainingControl;

      param^.saveMemory := 1; { large-scale MMOD trainer. }
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := True;
      FAI_EntryAPI^.RasterSerialized := RSeri;
      imgMat.SerializedAndRecycleMemory(RSeri);

      try
          Result := FAI_EntryAPI^.LargeScale_MMOD3L_Train(param, @imgArry_P[0], imgL.Count);
      except
          Result := -1;
      end;

      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := False;
      FAI_EntryAPI^.RasterSerialized := nil;

      Last_training_average_loss := param^.training_average_loss;
      Last_training_learning_rate := param^.training_learning_rate;
      completed_learning_rate := param^.completed_learning_rate;
    end;

  SetLength(imgArry, 0);
  SetLength(imgArry_P, 0);
  DisposeObject(imgL);
end;

function TAI.LargeScale_MMOD3L_DNN_Train_Stream(param: PMMOD_Train_Parameter; imgList: TAI_ImageList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  fn := Get_P_Bytes_String(param^.train_output);
  if (LargeScale_MMOD3L_DNN_Train(param, imgList) > 0) and (umlFileExists(fn)) then
    begin
      Result := TMemoryStream64.Create;
      Result.LoadFromFile(fn);
      Result.Position := 0;
    end;
end;

function TAI.LargeScale_MMOD3L_DNN_Train_Stream(param: PMMOD_Train_Parameter; imgMat: TAI_ImageMatrix): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  fn := Get_P_Bytes_String(param^.train_output);
  if (LargeScale_MMOD3L_DNN_Train(param, imgMat) > 0) and (umlFileExists(fn)) then
    begin
      Result := TMemoryStream64.Create;
      Result.LoadFromFile(fn);
      Result.Position := 0;
    end;
end;

function TAI.LargeScale_MMOD3L_DNN_Train_Stream(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgList: TAI_ImageList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  fn := Get_P_Bytes_String(param^.train_output);
  if (LargeScale_MMOD3L_DNN_Train(param, RSeri, imgList) > 0) and (umlFileExists(fn)) then
    begin
      Result := TMemoryStream64.Create;
      Result.LoadFromFile(fn);
      Result.Position := 0;
    end;
end;

function TAI.LargeScale_MMOD3L_DNN_Train_Stream(param: PMMOD_Train_Parameter; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;
  fn := Get_P_Bytes_String(param^.train_output);
  if (LargeScale_MMOD3L_DNN_Train(param, RSeri, imgMat) > 0) and (umlFileExists(fn)) then
    begin
      Result := TMemoryStream64.Create;
      Result.LoadFromFile(fn);
      Result.Position := 0;
    end;
end;

procedure TAI.LargeScale_MMOD3L_DNN_FreeTrain(param: PMMOD_Train_Parameter);
begin
  Free_MMOD3L_DNN_TrainParam(param);
end;

function TAI.MMOD3L_DNN_Open(train_file: SystemString): TMMOD3L_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MMOD3L_DNN_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      Result := FAI_EntryAPI^.MMOD3L_DNN_Init(train_file_buff);
      Free_P_Bytes(train_file_buff);
      if Result <> nil then
          DoStatus('MMOD-DNN(DNN+SVM:max-margin object detector) open: %s', [train_file]);
    end
  else
      Result := nil;
end;

function TAI.MMOD3L_DNN_Open_Stream(stream: TMemoryStream64): TMMOD3L_Handle;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MMOD3L_DNN_Init_Memory) then
    begin
      Result := FAI_EntryAPI^.MMOD3L_DNN_Init_Memory(stream.memory, stream.Size);
      if Result <> nil then
          DoStatus('MMOD-DNN(DNN+SVM:max-margin object detector) open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.MMOD3L_DNN_Open_Stream(train_file: SystemString): TMMOD3L_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := MMOD3L_DNN_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('MMOD-DNN(DNN+SVM:max-margin object detector) open: %s', [train_file]);
end;

function TAI.MMOD3L_DNN_Close(var hnd: TMMOD3L_Handle): Boolean;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MMOD3L_DNN_Free) and (hnd <> nil) then
    begin
      Result := FAI_EntryAPI^.MMOD3L_DNN_Free(hnd) = 0;
      DoStatus('MMOD-DNN(DNN+SVM:max-margin object detector) close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

function TAI.MMOD3L_DNN_Process(hnd: TMMOD3L_Handle; Raster: TMemoryRaster): TMMOD_Desc;
var
  rect_num: Integer;
  buff: TAI_MMOD_Desc;
  i: Integer;
begin
  Raster.ReadyBits();
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.MMOD3L_DNN_Process) then
      exit;
  SetLength(buff, 8192);

  rect_num := FAI_EntryAPI^.MMOD3L_DNN_Process(hnd, Raster.Bits, Raster.Width, Raster.Height, @buff[0], Length(buff));

  if rect_num >= 0 then
    begin
      SetLength(Result, rect_num);
      for i := 0 to rect_num - 1 do
        begin
          Result[i].R := RectV2(buff[i]);
          Result[i].confidence := buff[i].confidence;
          Result[i].Token := buff[i].Token^;
          API_FreeString(buff[i].Token);
        end;
    end;
  SetLength(buff, 0);
  FilterMMOD_Desc(Result);
end;

function TAI.MMOD3L_DNN_Process_Matrix(hnd: TMMOD3L_Handle; Matrix_IMG: TMatrix_Image_Handle): TMMOD_Desc;
var
  rect_num: Integer;
  buff: TAI_MMOD_Desc;
  i: Integer;
begin
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.MMOD3L_DNN_Process) then
      exit;
  SetLength(buff, 8192);

  rect_num := FAI_EntryAPI^.MMOD3L_DNN_Process_Image(hnd, Matrix_IMG, @buff[0], Length(buff));
  if rect_num >= 0 then
    begin
      SetLength(Result, rect_num);
      for i := 0 to rect_num - 1 do
        begin
          Result[i].R := RectV2(buff[i]);
          Result[i].confidence := buff[i].confidence;
          Result[i].Token := buff[i].Token^;
          API_FreeString(buff[i].Token);
        end;
    end;
  SetLength(buff, 0);
  FilterMMOD_Desc(Result);
end;

function TAI.MMOD3L_DNN_DebugInfo(hnd: TMMOD3L_Handle): U_String;
var
  p: PPascalString;
begin
  Result := '';
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.MMOD3L_DebugInfo) and (hnd <> nil) then
    begin
      FAI_EntryAPI^.MMOD3L_DebugInfo(hnd, p);
      Result := p^;
      Dispose(p);
    end;
end;

class function TAI.Init_RNIC_Train_Parameter(train_sync_file, train_output: U_String): PRNIC_Train_Parameter;
begin
  new(Result);
  FillPtrByte(Result, SizeOf(TRNIC_Train_Parameter), 0);

  Result^.imgArry_ptr := nil;
  Result^.img_num := 0;
  Result^.train_sync_file := Alloc_P_Bytes(train_sync_file);
  Result^.train_output := Alloc_P_Bytes(train_output);

  Result^.timeout := C_Tick_Hour;
  Result^.weight_decay := 0.0001;
  Result^.momentum := 0.9;
  Result^.iterations_without_progress_threshold := 500;
  Result^.learning_rate := 0.1;
  Result^.completed_learning_rate := 0.0001;
  Result^.all_bn_running_stats_window_sizes := 1000;
  Result^.img_mini_batch := 10;

  Result^.control := nil;
  Result^.training_average_loss := 0;
  Result^.training_learning_rate := 0;
end;

class procedure TAI.Free_RNIC_Train_Parameter(param: PRNIC_Train_Parameter);
begin
  Free_P_Bytes(param^.train_sync_file);
  Free_P_Bytes(param^.train_output);
  Dispose(param);
end;

function TAI.RNIC_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TMemoryRaster2DArray; param: PRNIC_Train_Parameter; Train_OutputIndex: TMemoryRasterList): Boolean;
var
  i, j, imgSum, ri: Integer;
  imgArry: TMemoryRasterArray;
  imgInfo_arry: array of TAI_Raster_Data;
begin
  Result := False;

  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.RNIC_Train) then
      exit;

  if Length(imgList) > C_RNIC_Dim then
    begin
      DoStatus('RNIC classifier out the max limit. %d > %d', [Length(imgList), C_RNIC_Dim]);
      exit;
    end;

  imgSum := 0;
  for i := 0 to Length(imgList) - 1 do
      inc(imgSum, Length(imgList[i]));

  if Train_OutputIndex <> nil then
      Train_OutputIndex.Clear;
  SetLength(imgInfo_arry, imgSum);
  ri := 0;

  for i := 0 to Length(imgList) - 1 do
    begin
      imgArry := imgList[i];
      for j := 0 to Length(imgArry) - 1 do
        begin
          new(imgInfo_arry[ri].raster_Hnd);
          imgInfo_arry[ri].raster_Hnd^.Raster := imgArry[j];
          if LargeScale_ then
            begin
              imgInfo_arry[ri].raster_ptr := nil;
              imgArry[j].SerializedAndRecycleMemory(RSeri);
            end
          else
              imgInfo_arry[ri].raster_ptr := imgArry[j].Bits;

          imgInfo_arry[ri].Width := imgArry[j].Width;
          imgInfo_arry[ri].Height := imgArry[j].Height;
          imgInfo_arry[ri].index := i;
          imgArry[j].UserVariant := i;

          if Train_OutputIndex <> nil then
              Train_OutputIndex.Add(imgArry[j]);
          inc(ri);
        end;
    end;

  TrainingControl.pause := 0;
  TrainingControl.stop := 0;

  param^.imgArry_ptr := @imgInfo_arry[0];
  param^.img_num := Length(imgInfo_arry);
  param^.control := @TrainingControl;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := True;
      FAI_EntryAPI^.RasterSerialized := RSeri;
    end
  else
      FAI_EntryAPI^.RasterSerialized := nil;

  FAI_EntryAPI^.SerializedTime := GetTimeTick();

  try
      Result := FAI_EntryAPI^.RNIC_Train(param) > 0;
  except
      Result := False;
  end;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := False;
      FAI_EntryAPI^.RasterSerialized := nil;
    end;

  Last_training_average_loss := param^.training_average_loss;
  Last_training_learning_rate := param^.training_learning_rate;
  completed_learning_rate := param^.completed_learning_rate;

  param^.imgArry_ptr := nil;
  param^.img_num := 0;
  param^.control := nil;

  { free }
  for i := 0 to Length(imgInfo_arry) - 1 do
      Dispose(imgInfo_arry[i].raster_Hnd);
  SetLength(imgInfo_arry, 0);
end;

function TAI.RNIC_Train(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  out_index: TMemoryRasterList;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.RNIC_Train) then
      exit;

  Train_OutputIndex.Clear;
  imgList.CalibrationNoDetectorDefine('');
  imgBuff := imgList.ExtractDetectorDefineAsSnapshot();
  out_index := TMemoryRasterList.Create;
  Result := RNIC_Train(False, nil, imgBuff, param, out_index);
  if Result then
    for i := 0 to out_index.Count - 1 do
      if Train_OutputIndex.ExistsValue(out_index[i].UserToken) < 0 then
          Train_OutputIndex.Add(out_index[i].UserToken);
  DisposeObject(out_index);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.RNIC_Train(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; train_index_output: U_String): Boolean;
var
  TrainIndex: TPascalStringList;
begin
  TrainIndex := TPascalStringList.Create;
  Result := RNIC_Train(imgList, param, TrainIndex);
  if Result then
      TrainIndex.SaveToFile(train_index_output);
  DisposeObject(TrainIndex);
end;

function TAI.RNIC_Train_Stream(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if RNIC_Train(imgList, param, Train_OutputIndex) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.RNIC_Train(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  out_index: TMemoryRasterList;
  imgL: TAI_ImageList;
  detDef: TAI_DetectorDefine;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.RNIC_Train) then
      exit;

  DoStatus('Calibration RNIC dataset.');
  for i := 0 to imgMat.Count - 1 do
    begin
      imgL := imgMat[i];
      imgL.CalibrationNoDetectorDefine(imgL.FileInfo);
      imgL.CalibrationNullToken(imgL.FileInfo);
      for j := 0 to imgL.Count - 1 do
        if imgL[j].DetectorDefineList.Count = 0 then
          begin
            detDef := TAI_DetectorDefine.Create(imgL[j]);
            detDef.R := imgL[j].Raster.BoundsRect;
            detDef.Token := imgL.FileInfo;
            imgL[j].DetectorDefineList.Add(detDef);
          end;
    end;

  Train_OutputIndex.Clear;
  imgBuff := imgMat.ExtractDetectorDefineAsSnapshot();
  out_index := TMemoryRasterList.Create;
  Result := RNIC_Train(False, nil, imgBuff, param, out_index);
  if Result then
    for i := 0 to out_index.Count - 1 do
      if Train_OutputIndex.ExistsValue(out_index[i].UserToken) < 0 then
          Train_OutputIndex.Add(out_index[i].UserToken);
  DisposeObject(out_index);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.RNIC_Train(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; train_index_output: U_String): Boolean;
var
  TrainIndex: TPascalStringList;
begin
  TrainIndex := TPascalStringList.Create;
  Result := RNIC_Train(imgMat, param, TrainIndex);
  if Result then
      TrainIndex.SaveToFile(train_index_output);
  DisposeObject(TrainIndex);
end;

function TAI.RNIC_Train_Stream(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if RNIC_Train(imgMat, param, Train_OutputIndex) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.RNIC_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  out_index: TMemoryRasterList;
  imgL: TAI_ImageList;
  detDef: TAI_DetectorDefine;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.RNIC_Train) then
      exit;

  DoStatus('Calibration RNIC dataset.');
  for i := 0 to imgMat.Count - 1 do
    begin
      imgL := imgMat[i];
      imgL.CalibrationNoDetectorDefine(imgL.FileInfo);
      imgL.CalibrationNullToken(imgL.FileInfo);
      for j := 0 to imgL.Count - 1 do
        if imgL[j].DetectorDefineList.Count = 0 then
          begin
            detDef := TAI_DetectorDefine.Create(imgL[j]);
            detDef.R := imgL[j].Raster.BoundsRect;
            detDef.Token := imgL.FileInfo;
            imgL[j].DetectorDefineList.Add(detDef);
          end;
    end;
  Train_OutputIndex.Clear;

  if LargeScale_ then
      imgBuff := imgMat.LargeScale_ExtractDetectorDefineAsSnapshot(RSeri)
  else
      imgBuff := imgMat.ExtractDetectorDefineAsSnapshot();

  out_index := TMemoryRasterList.Create;

  Result := RNIC_Train(LargeScale_, RSeri, imgBuff, param, out_index);

  if Result then
    for i := 0 to out_index.Count - 1 do
      if Train_OutputIndex.ExistsValue(out_index[i].UserToken) < 0 then
          Train_OutputIndex.Add(out_index[i].UserToken);

  DisposeObject(out_index);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.RNIC_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; train_index_output: U_String): Boolean;
var
  TrainIndex: TPascalStringList;
begin
  TrainIndex := TPascalStringList.Create;
  Result := RNIC_Train(LargeScale_, RSeri, imgMat, param, TrainIndex);
  if Result then
      TrainIndex.SaveToFile(train_index_output);
  DisposeObject(TrainIndex);
end;

function TAI.RNIC_Train_Stream(LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if RNIC_Train(LargeScale_, RSeri, imgMat, param, Train_OutputIndex) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.RNIC_Open(train_file: SystemString): TRNIC_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.RNIC_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      Result := FAI_EntryAPI^.RNIC_Init(train_file_buff);
      Free_P_Bytes(train_file_buff);
      if Result <> nil then
          DoStatus('ResNet-Image-Classifier open: %s', [train_file]);
    end
  else
      Result := nil;
end;

function TAI.RNIC_Open_Stream(stream: TMemoryStream64): TRNIC_Handle;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.RNIC_Init_Memory) then
    begin
      Result := FAI_EntryAPI^.RNIC_Init_Memory(stream.memory, stream.Size);
      DoStatus('ResNet-Image-Classifier open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.RNIC_Open_Stream(train_file: SystemString): TRNIC_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := RNIC_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('ResNet-Image-Classifier open: %s', [train_file]);
end;

function TAI.RNIC_Close(var hnd: TRNIC_Handle): Boolean;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.RNIC_Free) and (hnd <> nil) then
    begin
      Result := FAI_EntryAPI^.RNIC_Free(hnd) = 0;
      DoStatus('ResNet-Image-Classifier close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

function TAI.RNIC_Process(hnd: TRNIC_Handle; Raster: TMemoryRaster; num_crops: Integer): TLVec;
var
  R: Integer;
begin
  Raster.ReadyBits();
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.RNIC_Process) then
      exit;
  SetLength(Result, C_RNIC_Dim);

  R := FAI_EntryAPI^.RNIC_Process(hnd, num_crops, Raster.Bits, Raster.Width, Raster.Height, @Result[0]);

  if R <> C_RNIC_Dim then
      SetLength(Result, 0);
end;

function TAI.RNIC_Process(hnd: TRNIC_Handle; Raster: TMemoryRaster): TLVec;
begin
  Result := RNIC_Process(hnd, Raster, 16);
end;

function TAI.RNIC_ProcessMatrix(hnd: TRNIC_Handle; mat_hnd: TMatrix_Image_Handle; num_crops: Integer): TLVec;
var
  R: Integer;
begin
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.RNIC_Process) then
      exit;
  SetLength(Result, C_RNIC_Dim);

  R := FAI_EntryAPI^.RNIC_Process_Image(hnd, num_crops, mat_hnd, @Result[0]);

  if R <> C_RNIC_Dim then
      SetLength(Result, 0);
end;

function TAI.RNIC_DebugInfo(hnd: TRNIC_Handle): U_String;
var
  p: PPascalString;
begin
  Result := '';
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.RNIC_DebugInfo) and (hnd <> nil) then
    begin
      FAI_EntryAPI^.RNIC_DebugInfo(hnd, p);
      Result := p^;
      Dispose(p);
    end;
end;

class function TAI.Init_LRNIC_Train_Parameter(train_sync_file, train_output: U_String): PRNIC_Train_Parameter;
begin
  new(Result);
  FillPtrByte(Result, SizeOf(TRNIC_Train_Parameter), 0);

  Result^.imgArry_ptr := nil;
  Result^.img_num := 0;
  Result^.train_sync_file := Alloc_P_Bytes(train_sync_file);
  Result^.train_output := Alloc_P_Bytes(train_output);

  Result^.timeout := C_Tick_Hour;
  Result^.weight_decay := 0.0001;
  Result^.momentum := 0.9;
  Result^.iterations_without_progress_threshold := 500;
  Result^.learning_rate := 0.1;
  Result^.completed_learning_rate := 0.0001;
  Result^.all_bn_running_stats_window_sizes := 1000;
  Result^.img_mini_batch := 10;

  Result^.control := nil;
  Result^.training_average_loss := 0;
  Result^.training_learning_rate := 0;
end;

class procedure TAI.Free_LRNIC_Train_Parameter(param: PRNIC_Train_Parameter);
begin
  Free_P_Bytes(param^.train_sync_file);
  Free_P_Bytes(param^.train_output);
  Dispose(param);
end;

function TAI.LRNIC_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TMemoryRaster2DArray; param: PRNIC_Train_Parameter; Train_OutputIndex: TMemoryRasterList): Boolean;
var
  i, j, imgSum, ri: Integer;
  imgArry: TMemoryRasterArray;
  imgInfo_arry: array of TAI_Raster_Data;
begin
  Result := False;

  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.LRNIC_Train) then
      exit;

  if Length(imgList) > C_LRNIC_Dim then
    begin
      DoStatus('LRNIC classifier out the max limit. %d > %d', [Length(imgList), C_LRNIC_Dim]);
      exit;
    end;

  imgSum := 0;
  for i := 0 to Length(imgList) - 1 do
      inc(imgSum, Length(imgList[i]));

  if Train_OutputIndex <> nil then
      Train_OutputIndex.Clear;
  SetLength(imgInfo_arry, imgSum);
  ri := 0;

  for i := 0 to Length(imgList) - 1 do
    begin
      imgArry := imgList[i];
      for j := 0 to Length(imgArry) - 1 do
        begin
          new(imgInfo_arry[ri].raster_Hnd);
          imgInfo_arry[ri].raster_Hnd^.Raster := imgArry[j];
          if LargeScale_ then
            begin
              imgInfo_arry[ri].raster_ptr := nil;
              imgArry[j].SerializedAndRecycleMemory(RSeri);
            end
          else
              imgInfo_arry[ri].raster_ptr := imgArry[j].Bits;

          imgInfo_arry[ri].Width := imgArry[j].Width;
          imgInfo_arry[ri].Height := imgArry[j].Height;
          imgInfo_arry[ri].index := i;
          imgArry[j].UserVariant := i;

          if Train_OutputIndex <> nil then
              Train_OutputIndex.Add(imgArry[j]);
          inc(ri);
        end;
    end;

  TrainingControl.pause := 0;
  TrainingControl.stop := 0;

  param^.imgArry_ptr := @imgInfo_arry[0];
  param^.img_num := Length(imgInfo_arry);
  param^.control := @TrainingControl;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := True;
      FAI_EntryAPI^.RasterSerialized := RSeri;
    end
  else
      FAI_EntryAPI^.RasterSerialized := nil;

  FAI_EntryAPI^.SerializedTime := GetTimeTick();

  try
      Result := FAI_EntryAPI^.LRNIC_Train(param) > 0;
  except
      Result := False;
  end;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := False;
      FAI_EntryAPI^.RasterSerialized := nil;
    end;

  Last_training_average_loss := param^.training_average_loss;
  Last_training_learning_rate := param^.training_learning_rate;
  completed_learning_rate := param^.completed_learning_rate;

  param^.imgArry_ptr := nil;
  param^.img_num := 0;
  param^.control := nil;

  for i := 0 to Length(imgInfo_arry) - 1 do
      Dispose(imgInfo_arry[i].raster_Hnd);
  SetLength(imgInfo_arry, 0);
end;

function TAI.LRNIC_Train(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  out_index: TMemoryRasterList;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.LRNIC_Train) then
      exit;

  Train_OutputIndex.Clear;
  imgList.CalibrationNoDetectorDefine('');
  imgBuff := imgList.ExtractDetectorDefineAsSnapshot();
  out_index := TMemoryRasterList.Create;
  Result := LRNIC_Train(False, nil, imgBuff, param, out_index);
  if Result then
    for i := 0 to out_index.Count - 1 do
      if Train_OutputIndex.ExistsValue(out_index[i].UserToken) < 0 then
          Train_OutputIndex.Add(out_index[i].UserToken);
  DisposeObject(out_index);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.LRNIC_Train(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; train_index_output: U_String): Boolean;
var
  TrainIndex: TPascalStringList;
begin
  TrainIndex := TPascalStringList.Create;
  Result := LRNIC_Train(imgList, param, TrainIndex);
  if Result then
      TrainIndex.SaveToFile(train_index_output);
  DisposeObject(TrainIndex);
end;

function TAI.LRNIC_Train_Stream(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if LRNIC_Train(imgList, param, Train_OutputIndex) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.LRNIC_Train(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  out_index: TMemoryRasterList;
  imgL: TAI_ImageList;
  detDef: TAI_DetectorDefine;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.LRNIC_Train) then
      exit;

  DoStatus('Calibration LRNIC dataset.');
  for i := 0 to imgMat.Count - 1 do
    begin
      imgL := imgMat[i];
      imgL.CalibrationNoDetectorDefine(imgL.FileInfo);
      imgL.CalibrationNullToken(imgL.FileInfo);
      for j := 0 to imgL.Count - 1 do
        if imgL[j].DetectorDefineList.Count = 0 then
          begin
            detDef := TAI_DetectorDefine.Create(imgL[j]);
            detDef.R := imgL[j].Raster.BoundsRect;
            detDef.Token := imgL.FileInfo;
            imgL[j].DetectorDefineList.Add(detDef);
          end;
    end;

  Train_OutputIndex.Clear;
  imgBuff := imgMat.ExtractDetectorDefineAsSnapshot();
  out_index := TMemoryRasterList.Create;
  Result := LRNIC_Train(False, nil, imgBuff, param, out_index);
  if Result then
    for i := 0 to out_index.Count - 1 do
      if Train_OutputIndex.ExistsValue(out_index[i].UserToken) < 0 then
          Train_OutputIndex.Add(out_index[i].UserToken);
  DisposeObject(out_index);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.LRNIC_Train(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; train_index_output: U_String): Boolean;
var
  TrainIndex: TPascalStringList;
begin
  TrainIndex := TPascalStringList.Create;
  Result := LRNIC_Train(imgMat, param, TrainIndex);
  if Result then
      TrainIndex.SaveToFile(train_index_output);
  DisposeObject(TrainIndex);
end;

function TAI.LRNIC_Train_Stream(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if LRNIC_Train(imgMat, param, Train_OutputIndex) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.LRNIC_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  out_index: TMemoryRasterList;
  imgL: TAI_ImageList;
  detDef: TAI_DetectorDefine;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.LRNIC_Train) then
      exit;

  DoStatus('Calibration RNIC dataset.');
  for i := 0 to imgMat.Count - 1 do
    begin
      imgL := imgMat[i];
      imgL.CalibrationNoDetectorDefine(imgL.FileInfo);
      imgL.CalibrationNullToken(imgL.FileInfo);
      for j := 0 to imgL.Count - 1 do
        if imgL[j].DetectorDefineList.Count = 0 then
          begin
            detDef := TAI_DetectorDefine.Create(imgL[j]);
            detDef.R := imgL[j].Raster.BoundsRect;
            detDef.Token := imgL.FileInfo;
            imgL[j].DetectorDefineList.Add(detDef);
          end;
    end;
  Train_OutputIndex.Clear;

  if LargeScale_ then
      imgBuff := imgMat.LargeScale_ExtractDetectorDefineAsSnapshot(RSeri)
  else
      imgBuff := imgMat.ExtractDetectorDefineAsSnapshot();

  out_index := TMemoryRasterList.Create;

  Result := LRNIC_Train(LargeScale_, RSeri, imgBuff, param, out_index);

  if Result then
    for i := 0 to out_index.Count - 1 do
      if Train_OutputIndex.ExistsValue(out_index[i].UserToken) < 0 then
          Train_OutputIndex.Add(out_index[i].UserToken);

  DisposeObject(out_index);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.LRNIC_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; train_index_output: U_String): Boolean;
var
  TrainIndex: TPascalStringList;
begin
  TrainIndex := TPascalStringList.Create;
  Result := LRNIC_Train(LargeScale_, RSeri, imgMat, param, TrainIndex);
  if Result then
      TrainIndex.SaveToFile(train_index_output);
  DisposeObject(TrainIndex);
end;

function TAI.LRNIC_Train_Stream(LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if LRNIC_Train(LargeScale_, RSeri, imgMat, param, Train_OutputIndex) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.LRNIC_Open(train_file: SystemString): TLRNIC_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LRNIC_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      Result := FAI_EntryAPI^.LRNIC_Init(train_file_buff);
      Free_P_Bytes(train_file_buff);
      if Result <> nil then
          DoStatus('ResNet-Image-Classifier open: %s', [train_file]);
    end
  else
      Result := nil;
end;

function TAI.LRNIC_Open_Stream(stream: TMemoryStream64): TLRNIC_Handle;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LRNIC_Init_Memory) then
    begin
      Result := FAI_EntryAPI^.LRNIC_Init_Memory(stream.memory, stream.Size);
      DoStatus('ResNet-Image-Classifier open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.LRNIC_Open_Stream(train_file: SystemString): TLRNIC_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := LRNIC_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('ResNet-Image-Classifier open: %s', [train_file]);
end;

function TAI.LRNIC_Close(var hnd: TLRNIC_Handle): Boolean;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LRNIC_Free) and (hnd <> nil) then
    begin
      Result := FAI_EntryAPI^.LRNIC_Free(hnd) = 0;
      DoStatus('ResNet-Image-Classifier close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

function TAI.LRNIC_Process(hnd: TLRNIC_Handle; Raster: TMemoryRaster; num_crops: Integer): TLVec;
var
  R: Integer;
begin
  Raster.ReadyBits();
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.LRNIC_Process) then
      exit;
  SetLength(Result, C_LRNIC_Dim);

  R := FAI_EntryAPI^.LRNIC_Process(hnd, num_crops, Raster.Bits, Raster.Width, Raster.Height, @Result[0]);

  if R <> C_LRNIC_Dim then
      SetLength(Result, 0);
end;

function TAI.LRNIC_Process(hnd: TLRNIC_Handle; Raster: TMemoryRaster): TLVec;
begin
  Result := LRNIC_Process(hnd, Raster, 24);
end;

function TAI.LRNIC_ProcessMatrix(hnd: TLRNIC_Handle; mat_hnd: TMatrix_Image_Handle; num_crops: Integer): TLVec;
var
  R: Integer;
begin
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.LRNIC_Process) then
      exit;
  SetLength(Result, C_LRNIC_Dim);

  R := FAI_EntryAPI^.LRNIC_Process_Image(hnd, num_crops, mat_hnd, @Result[0]);

  if R <> C_LRNIC_Dim then
      SetLength(Result, 0);
end;

function TAI.LRNIC_DebugInfo(hnd: TLRNIC_Handle): U_String;
var
  p: PPascalString;
begin
  Result := '';
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.LRNIC_DebugInfo) and (hnd <> nil) then
    begin
      FAI_EntryAPI^.LRNIC_DebugInfo(hnd, p);
      Result := p^;
      Dispose(p);
    end;
end;

class function TAI.Init_GDCNIC_Train_Parameter(train_sync_file, train_output: U_String): PGDCNIC_Train_Parameter;
begin
  new(Result);
  FillPtrByte(Result, SizeOf(TGDCNIC_Train_Parameter), 0);

  Result^.imgArry_ptr := nil;
  Result^.img_num := 0;
  Result^.train_sync_file := Alloc_P_Bytes(train_sync_file);
  Result^.train_output := Alloc_P_Bytes(train_output);

  Result^.timeout := C_Tick_Hour;
  Result^.iterations_without_progress_threshold := 2000;
  Result^.learning_rate := 0.01;
  Result^.completed_learning_rate := 0.00001;
  Result^.img_mini_batch := 128;

  Result^.control := nil;
  Result^.training_average_loss := 0;
  Result^.training_learning_rate := 0;
end;

class procedure TAI.Free_GDCNIC_Train_Parameter(param: PGDCNIC_Train_Parameter);
begin
  Free_P_Bytes(param^.train_sync_file);
  Free_P_Bytes(param^.train_output);
  Dispose(param);
end;

function TAI.GDCNIC_Train_(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TMemoryRaster2DArray; param: PGDCNIC_Train_Parameter; Train_OutputIndex: TMemoryRasterList): Boolean;
var
  i, j, imgSum, ri: Integer;
  imgArry: TMemoryRasterArray;
  imgInfo_arry: array of TAI_Raster_Data;
begin
  Result := False;

  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.GDCNIC_Train) then
      exit;

  if Length(imgList) > C_GDCNIC_Dim then
    begin
      DoStatus('GDCNIC classifier out the max limit. %d > %d', [Length(imgList), C_GDCNIC_Dim]);
      exit;
    end;

  imgSum := 0;
  for i := 0 to Length(imgList) - 1 do
      inc(imgSum, Length(imgList[i]));

  if Train_OutputIndex <> nil then
      Train_OutputIndex.Clear;
  SetLength(imgInfo_arry, imgSum);
  ri := 0;

  for i := 0 to Length(imgList) - 1 do
    begin
      imgArry := imgList[i];
      for j := 0 to Length(imgArry) - 1 do
        begin
          new(imgInfo_arry[ri].raster_Hnd);
          imgInfo_arry[ri].raster_Hnd^.Raster := imgArry[j];
          if LargeScale_ then
            begin
              imgInfo_arry[ri].raster_ptr := nil;
              imgArry[j].SerializedAndRecycleMemory(RSeri);
            end
          else
              imgInfo_arry[ri].raster_ptr := imgArry[j].Bits;

          imgInfo_arry[ri].Width := imgArry[j].Width;
          imgInfo_arry[ri].Height := imgArry[j].Height;
          imgInfo_arry[ri].index := i;
          imgArry[j].UserVariant := i;

          if Train_OutputIndex <> nil then
              Train_OutputIndex.Add(imgArry[j]);
          inc(ri);
        end;
    end;

  TrainingControl.pause := 0;
  TrainingControl.stop := 0;

  param^.imgArry_ptr := @imgInfo_arry[0];
  param^.img_num := Length(imgInfo_arry);
  param^.control := @TrainingControl;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := True;
      FAI_EntryAPI^.RasterSerialized := RSeri;
    end
  else
      FAI_EntryAPI^.RasterSerialized := nil;

  FAI_EntryAPI^.SerializedTime := GetTimeTick();

  try
      Result := FAI_EntryAPI^.GDCNIC_Train(param) > 0;
  except
      Result := False;
  end;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := False;
      FAI_EntryAPI^.RasterSerialized := nil;
    end;

  Last_training_average_loss := param^.training_average_loss;
  Last_training_learning_rate := param^.training_learning_rate;
  completed_learning_rate := param^.completed_learning_rate;

  param^.imgArry_ptr := nil;
  param^.img_num := 0;
  param^.control := nil;

  { free }
  for i := 0 to Length(imgInfo_arry) - 1 do
      Dispose(imgInfo_arry[i].raster_Hnd);
  SetLength(imgInfo_arry, 0);
end;

function TAI.GDCNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgList: TAI_ImageList; param: PGDCNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  out_index: TMemoryRasterList;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.GDCNIC_Train) then
      exit;

  Train_OutputIndex.Clear;

  imgList.CalibrationNoDetectorDefine('');

  if Snapshot_ then
      imgBuff := imgList.ExtractDetectorDefineAsSnapshotProjection(SS_Width, SS_Height)
  else
      imgBuff := imgList.ExtractDetectorDefineAsPrepareRaster(SS_Width, SS_Height);

  out_index := TMemoryRasterList.Create;
  Result := GDCNIC_Train_(False, nil, imgBuff, param, out_index);
  if Result then
    for i := 0 to out_index.Count - 1 do
      if Train_OutputIndex.ExistsValue(out_index[i].UserToken) < 0 then
          Train_OutputIndex.Add(out_index[i].UserToken);
  DisposeObject(out_index);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.GDCNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgList: TAI_ImageList; param: PGDCNIC_Train_Parameter; train_index_output: U_String): Boolean;
var
  TrainIndex: TPascalStringList;
begin
  TrainIndex := TPascalStringList.Create;
  Result := GDCNIC_Train(Snapshot_, SS_Width, SS_Height, imgList, param, TrainIndex);
  if Result then
      TrainIndex.SaveToFile(train_index_output);
  DisposeObject(TrainIndex);
end;

function TAI.GDCNIC_Train_Stream(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgList: TAI_ImageList; param: PGDCNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if GDCNIC_Train(Snapshot_, SS_Width, SS_Height, imgList, param, Train_OutputIndex) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.GDCNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGDCNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  out_index: TMemoryRasterList;
  imgL: TAI_ImageList;
  detDef: TAI_DetectorDefine;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.GDCNIC_Train) then
      exit;

  DoStatus('Calibration GDCNIC dataset.');
  for i := 0 to imgMat.Count - 1 do
    begin
      imgL := imgMat[i];
      imgL.CalibrationNoDetectorDefine(imgL.FileInfo);
      imgL.CalibrationNullToken(imgL.FileInfo);
      for j := 0 to imgL.Count - 1 do
        if imgL[j].DetectorDefineList.Count = 0 then
          begin
            detDef := TAI_DetectorDefine.Create(imgL[j]);
            detDef.R := imgL[j].Raster.BoundsRect;
            detDef.Token := imgL.FileInfo;
            imgL[j].DetectorDefineList.Add(detDef);
          end;
    end;

  Train_OutputIndex.Clear;
  if Snapshot_ then
      imgBuff := imgMat.ExtractDetectorDefineAsSnapshotProjection(SS_Width, SS_Height)
  else
      imgBuff := imgMat.ExtractDetectorDefineAsPrepareRaster(SS_Width, SS_Height);
  out_index := TMemoryRasterList.Create;
  Result := GDCNIC_Train_(False, nil, imgBuff, param, out_index);
  if Result then
    for i := 0 to out_index.Count - 1 do
      if Train_OutputIndex.ExistsValue(out_index[i].UserToken) < 0 then
          Train_OutputIndex.Add(out_index[i].UserToken);
  DisposeObject(out_index);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.GDCNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGDCNIC_Train_Parameter; train_index_output: U_String): Boolean;
var
  TrainIndex: TPascalStringList;
begin
  TrainIndex := TPascalStringList.Create;
  Result := GDCNIC_Train(Snapshot_, SS_Width, SS_Height, imgMat, param, TrainIndex);
  if Result then
      TrainIndex.SaveToFile(train_index_output);
  DisposeObject(TrainIndex);
end;

function TAI.GDCNIC_Train_Stream(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGDCNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if GDCNIC_Train(Snapshot_, SS_Width, SS_Height, imgMat, param, Train_OutputIndex) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.GDCNIC_Train(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGDCNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  out_index: TMemoryRasterList;
  imgL: TAI_ImageList;
  detDef: TAI_DetectorDefine;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.GDCNIC_Train) then
      exit;

  DoStatus('Calibration GDCNIC dataset.');
  for i := 0 to imgMat.Count - 1 do
    begin
      imgL := imgMat[i];
      imgL.CalibrationNoDetectorDefine(imgL.FileInfo);
      imgL.CalibrationNullToken(imgL.FileInfo);
      for j := 0 to imgL.Count - 1 do
        if imgL[j].DetectorDefineList.Count = 0 then
          begin
            detDef := TAI_DetectorDefine.Create(imgL[j]);
            detDef.R := imgL[j].Raster.BoundsRect;
            detDef.Token := imgL.FileInfo;
            imgL[j].DetectorDefineList.Add(detDef);
          end;
    end;
  Train_OutputIndex.Clear;

  if Snapshot_ then
    begin
      if LargeScale_ then
          imgBuff := imgMat.LargeScale_ExtractDetectorDefineAsSnapshotProjection(RSeri, SS_Width, SS_Height)
      else
          imgBuff := imgMat.ExtractDetectorDefineAsSnapshotProjection(SS_Width, SS_Height);
    end
  else
    begin
      if LargeScale_ then
          imgBuff := imgMat.LargeScale_ExtractDetectorDefineAsPrepareRaster(RSeri, SS_Width, SS_Height)
      else
          imgBuff := imgMat.ExtractDetectorDefineAsPrepareRaster(SS_Width, SS_Height);
    end;

  out_index := TMemoryRasterList.Create;

  Result := GDCNIC_Train_(LargeScale_, RSeri, imgBuff, param, out_index);

  if Result then
    for i := 0 to out_index.Count - 1 do
      if Train_OutputIndex.ExistsValue(out_index[i].UserToken) < 0 then
          Train_OutputIndex.Add(out_index[i].UserToken);

  DisposeObject(out_index);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.GDCNIC_Train(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGDCNIC_Train_Parameter; train_index_output: U_String): Boolean;
var
  TrainIndex: TPascalStringList;
begin
  TrainIndex := TPascalStringList.Create;
  Result := GDCNIC_Train(Snapshot_, LargeScale_, RSeri, SS_Width, SS_Height, imgMat, param, TrainIndex);
  if Result then
      TrainIndex.SaveToFile(train_index_output);
  DisposeObject(TrainIndex);
end;

function TAI.GDCNIC_Train_Stream(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGDCNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if GDCNIC_Train(Snapshot_, LargeScale_, RSeri, SS_Width, SS_Height, imgMat, param, Train_OutputIndex) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.GDCNIC_Open(train_file: SystemString): TGDCNIC_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.GDCNIC_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      Result := FAI_EntryAPI^.GDCNIC_Init(train_file_buff);
      Free_P_Bytes(train_file_buff);
      if Result <> nil then
          DoStatus('ResNet-Image-Classifier open: %s', [train_file]);
    end
  else
      Result := nil;
end;

function TAI.GDCNIC_Open_Stream(stream: TMemoryStream64): TGDCNIC_Handle;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.GDCNIC_Init_Memory) then
    begin
      Result := FAI_EntryAPI^.GDCNIC_Init_Memory(stream.memory, stream.Size);
      DoStatus('ResNet-Image-Classifier open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.GDCNIC_Open_Stream(train_file: SystemString): TGDCNIC_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := GDCNIC_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('ResNet-Image-Classifier open: %s', [train_file]);
end;

function TAI.GDCNIC_Close(var hnd: TGDCNIC_Handle): Boolean;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.GDCNIC_Free) and (hnd <> nil) then
    begin
      Result := FAI_EntryAPI^.GDCNIC_Free(hnd) = 0;
      DoStatus('ResNet-Image-Classifier close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

function TAI.GDCNIC_Process(hnd: TGDCNIC_Handle; SS_Width, SS_Height: Integer; Raster: TMemoryRaster): TLVec;
var
  R: Integer;
  nr: TMemoryRaster;
begin
  Raster.ReadyBits();
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.GDCNIC_Process) then
      exit;
  SetLength(Result, C_GDCNIC_Dim);

  { projection }
  nr := nil;
  if (Raster.Width <> SS_Width) or (Raster.Height <> SS_Height) then
    begin
      nr := NewRaster();
      nr.SetSize(SS_Width, SS_Height);
      Raster.ReadyBits();
      Raster.ProjectionTo(nr,
        TV2Rect4.Init(RectFit(SS_Width, SS_Height, Raster.BoundsRectV2), 0),
        TV2Rect4.Init(nr.BoundsRectV2, 0),
        True, 1.0);
    end
  else
      nr := Raster;

  R := FAI_EntryAPI^.GDCNIC_Process(hnd, nr.Bits, nr.Width, nr.Height, @Result[0]);
  if nr <> Raster then
      DisposeObject(nr);

  if R <> C_GDCNIC_Dim then
      SetLength(Result, 0);
end;

function TAI.GDCNIC_DebugInfo(hnd: TGDCNIC_Handle): U_String;
var
  p: PPascalString;
begin
  Result := '';
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.GDCNIC_DebugInfo) and (hnd <> nil) then
    begin
      FAI_EntryAPI^.GDCNIC_DebugInfo(hnd, p);
      Result := p^;
      Dispose(p);
    end;
end;

class function TAI.Init_GNIC_Train_Parameter(train_sync_file, train_output: U_String): PGNIC_Train_Parameter;
begin
  new(Result);
  FillPtrByte(Result, SizeOf(TGNIC_Train_Parameter), 0);

  Result^.imgArry_ptr := nil;
  Result^.img_num := 0;
  Result^.train_sync_file := Alloc_P_Bytes(train_sync_file);
  Result^.train_output := Alloc_P_Bytes(train_output);

  Result^.timeout := C_Tick_Hour;
  Result^.iterations_without_progress_threshold := 2000;
  Result^.learning_rate := 0.01;
  Result^.completed_learning_rate := 0.00001;
  Result^.img_mini_batch := 128;

  Result^.control := nil;
  Result^.training_average_loss := 0;
  Result^.training_learning_rate := 0;
end;

class procedure TAI.Free_GNIC_Train_Parameter(param: PGNIC_Train_Parameter);
begin
  Free_P_Bytes(param^.train_sync_file);
  Free_P_Bytes(param^.train_output);
  Dispose(param);
end;

function TAI.GNIC_Train_(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TMemoryRaster2DArray; param: PGNIC_Train_Parameter; Train_OutputIndex: TMemoryRasterList): Boolean;
var
  i, j, imgSum, ri: Integer;
  imgArry: TMemoryRasterArray;
  imgInfo_arry: array of TAI_Raster_Data;
begin
  Result := False;

  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.GNIC_Train) then
      exit;

  if Length(imgList) > C_GNIC_Dim then
    begin
      DoStatus('GNIC classifier out the max limit. %d > %d', [Length(imgList), C_GNIC_Dim]);
      exit;
    end;

  imgSum := 0;
  for i := 0 to Length(imgList) - 1 do
      inc(imgSum, Length(imgList[i]));

  if Train_OutputIndex <> nil then
      Train_OutputIndex.Clear;
  SetLength(imgInfo_arry, imgSum);
  ri := 0;

  for i := 0 to Length(imgList) - 1 do
    begin
      imgArry := imgList[i];
      for j := 0 to Length(imgArry) - 1 do
        begin
          new(imgInfo_arry[ri].raster_Hnd);
          imgInfo_arry[ri].raster_Hnd^.Raster := imgArry[j];
          if LargeScale_ then
            begin
              imgInfo_arry[ri].raster_ptr := nil;
              imgArry[j].SerializedAndRecycleMemory(RSeri);
            end
          else
              imgInfo_arry[ri].raster_ptr := imgArry[j].Bits;

          imgInfo_arry[ri].Width := imgArry[j].Width;
          imgInfo_arry[ri].Height := imgArry[j].Height;
          imgInfo_arry[ri].index := i;
          imgArry[j].UserVariant := i;

          if Train_OutputIndex <> nil then
              Train_OutputIndex.Add(imgArry[j]);
          inc(ri);
        end;
    end;

  TrainingControl.pause := 0;
  TrainingControl.stop := 0;

  param^.imgArry_ptr := @imgInfo_arry[0];
  param^.img_num := Length(imgInfo_arry);
  param^.control := @TrainingControl;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := True;
      FAI_EntryAPI^.RasterSerialized := RSeri;
    end
  else
      FAI_EntryAPI^.RasterSerialized := nil;

  FAI_EntryAPI^.SerializedTime := GetTimeTick();

  try
      Result := FAI_EntryAPI^.GNIC_Train(param) > 0;
  except
      Result := False;
  end;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := False;
      FAI_EntryAPI^.RasterSerialized := nil;
    end;

  Last_training_average_loss := param^.training_average_loss;
  Last_training_learning_rate := param^.training_learning_rate;
  completed_learning_rate := param^.completed_learning_rate;

  param^.imgArry_ptr := nil;
  param^.img_num := 0;
  param^.control := nil;

  { free }
  for i := 0 to Length(imgInfo_arry) - 1 do
      Dispose(imgInfo_arry[i].raster_Hnd);
  SetLength(imgInfo_arry, 0);
end;

function TAI.GNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgList: TAI_ImageList; param: PGNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  out_index: TMemoryRasterList;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.GNIC_Train) then
      exit;

  Train_OutputIndex.Clear;
  imgList.CalibrationNoDetectorDefine('');

  if Snapshot_ then
      imgBuff := imgList.ExtractDetectorDefineAsSnapshotProjection(SS_Width, SS_Height)
  else
      imgBuff := imgList.ExtractDetectorDefineAsPrepareRaster(SS_Width, SS_Height);

  out_index := TMemoryRasterList.Create;
  Result := GNIC_Train_(False, nil, imgBuff, param, out_index);
  if Result then
    for i := 0 to out_index.Count - 1 do
      if Train_OutputIndex.ExistsValue(out_index[i].UserToken) < 0 then
          Train_OutputIndex.Add(out_index[i].UserToken);
  DisposeObject(out_index);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.GNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgList: TAI_ImageList; param: PGNIC_Train_Parameter; train_index_output: U_String): Boolean;
var
  TrainIndex: TPascalStringList;
begin
  TrainIndex := TPascalStringList.Create;
  Result := GNIC_Train(Snapshot_, SS_Width, SS_Height, imgList, param, TrainIndex);
  if Result then
      TrainIndex.SaveToFile(train_index_output);
  DisposeObject(TrainIndex);
end;

function TAI.GNIC_Train_Stream(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgList: TAI_ImageList; param: PGNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if GNIC_Train(Snapshot_, SS_Width, SS_Height, imgList, param, Train_OutputIndex) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.GNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  out_index: TMemoryRasterList;
  imgL: TAI_ImageList;
  detDef: TAI_DetectorDefine;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.GNIC_Train) then
      exit;

  DoStatus('Calibration GNIC dataset.');
  for i := 0 to imgMat.Count - 1 do
    begin
      imgL := imgMat[i];
      imgL.CalibrationNoDetectorDefine(imgL.FileInfo);
      imgL.CalibrationNullToken(imgL.FileInfo);
      for j := 0 to imgL.Count - 1 do
        if imgL[j].DetectorDefineList.Count = 0 then
          begin
            detDef := TAI_DetectorDefine.Create(imgL[j]);
            detDef.R := imgL[j].Raster.BoundsRect;
            detDef.Token := imgL.FileInfo;
            imgL[j].DetectorDefineList.Add(detDef);
          end;
    end;

  Train_OutputIndex.Clear;
  if Snapshot_ then
      imgBuff := imgMat.ExtractDetectorDefineAsSnapshotProjection(SS_Width, SS_Height)
  else
      imgBuff := imgMat.ExtractDetectorDefineAsPrepareRaster(SS_Width, SS_Height);

  out_index := TMemoryRasterList.Create;
  Result := GNIC_Train_(False, nil, imgBuff, param, out_index);
  if Result then
    for i := 0 to out_index.Count - 1 do
      if Train_OutputIndex.ExistsValue(out_index[i].UserToken) < 0 then
          Train_OutputIndex.Add(out_index[i].UserToken);
  DisposeObject(out_index);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.GNIC_Train(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGNIC_Train_Parameter; train_index_output: U_String): Boolean;
var
  TrainIndex: TPascalStringList;
begin
  TrainIndex := TPascalStringList.Create;
  Result := GNIC_Train(Snapshot_, SS_Width, SS_Height, imgMat, param, TrainIndex);
  if Result then
      TrainIndex.SaveToFile(train_index_output);
  DisposeObject(TrainIndex);
end;

function TAI.GNIC_Train_Stream(Snapshot_: Boolean; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if GNIC_Train(Snapshot_, SS_Width, SS_Height, imgMat, param, Train_OutputIndex) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.GNIC_Train(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  out_index: TMemoryRasterList;
  imgL: TAI_ImageList;
  detDef: TAI_DetectorDefine;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.GNIC_Train) then
      exit;

  DoStatus('Calibration GNIC dataset.');
  for i := 0 to imgMat.Count - 1 do
    begin
      imgL := imgMat[i];
      imgL.CalibrationNoDetectorDefine(imgL.FileInfo);
      imgL.CalibrationNullToken(imgL.FileInfo);
      for j := 0 to imgL.Count - 1 do
        if imgL[j].DetectorDefineList.Count = 0 then
          begin
            detDef := TAI_DetectorDefine.Create(imgL[j]);
            detDef.R := imgL[j].Raster.BoundsRect;
            detDef.Token := imgL.FileInfo;
            imgL[j].DetectorDefineList.Add(detDef);
          end;
    end;
  Train_OutputIndex.Clear;

  if Snapshot_ then
    begin
      if LargeScale_ then
          imgBuff := imgMat.LargeScale_ExtractDetectorDefineAsSnapshotProjection(RSeri, SS_Width, SS_Height)
      else
          imgBuff := imgMat.ExtractDetectorDefineAsSnapshotProjection(SS_Width, SS_Height);
    end
  else
    begin
      if LargeScale_ then
          imgBuff := imgMat.LargeScale_ExtractDetectorDefineAsPrepareRaster(RSeri, SS_Width, SS_Height)
      else
          imgBuff := imgMat.ExtractDetectorDefineAsPrepareRaster(SS_Width, SS_Height);
    end;

  out_index := TMemoryRasterList.Create;

  Result := GNIC_Train_(LargeScale_, RSeri, imgBuff, param, out_index);

  if Result then
    for i := 0 to out_index.Count - 1 do
      if Train_OutputIndex.ExistsValue(out_index[i].UserToken) < 0 then
          Train_OutputIndex.Add(out_index[i].UserToken);

  DisposeObject(out_index);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.GNIC_Train(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGNIC_Train_Parameter; train_index_output: U_String): Boolean;
var
  TrainIndex: TPascalStringList;
begin
  TrainIndex := TPascalStringList.Create;
  Result := GNIC_Train(Snapshot_, LargeScale_, RSeri, SS_Width, SS_Height, imgMat, param, TrainIndex);
  if Result then
      TrainIndex.SaveToFile(train_index_output);
  DisposeObject(TrainIndex);
end;

function TAI.GNIC_Train_Stream(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; SS_Width, SS_Height: Integer; imgMat: TAI_ImageMatrix; param: PGNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if GNIC_Train(Snapshot_, LargeScale_, RSeri, SS_Width, SS_Height, imgMat, param, Train_OutputIndex) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.GNIC_Open(train_file: SystemString): TGNIC_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.GNIC_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      Result := FAI_EntryAPI^.GNIC_Init(train_file_buff);
      Free_P_Bytes(train_file_buff);
      if Result <> nil then
          DoStatus('ResNet-Image-Classifier open: %s', [train_file]);
    end
  else
      Result := nil;
end;

function TAI.GNIC_Open_Stream(stream: TMemoryStream64): TGNIC_Handle;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.GNIC_Init_Memory) then
    begin
      Result := FAI_EntryAPI^.GNIC_Init_Memory(stream.memory, stream.Size);
      DoStatus('ResNet-Image-Classifier open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.GNIC_Open_Stream(train_file: SystemString): TGNIC_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := GNIC_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('ResNet-Image-Classifier open: %s', [train_file]);
end;

function TAI.GNIC_Close(var hnd: TGNIC_Handle): Boolean;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.GNIC_Free) and (hnd <> nil) then
    begin
      Result := FAI_EntryAPI^.GNIC_Free(hnd) = 0;
      DoStatus('ResNet-Image-Classifier close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

function TAI.GNIC_Process(hnd: TGNIC_Handle; SS_Width, SS_Height: Integer; Raster: TMemoryRaster): TLVec;
var
  R: Integer;
  nr: TMemoryRaster;
begin
  Raster.ReadyBits();
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.GNIC_Process) then
      exit;
  SetLength(Result, C_GNIC_Dim);

  { projection }
  nr := nil;
  if (Raster.Width <> SS_Width) or (Raster.Height <> SS_Height) then
    begin
      nr := NewRaster();
      nr.SetSize(SS_Width, SS_Height);
      Raster.ProjectionTo(nr,
        TV2Rect4.Init(RectFit(SS_Width, SS_Height, Raster.BoundsRectV2), 0),
        TV2Rect4.Init(nr.BoundsRectV2, 0),
        True, 1.0);
    end
  else
      nr := Raster;

  R := FAI_EntryAPI^.GNIC_Process(hnd, nr.Bits, nr.Width, nr.Height, @Result[0]);
  if nr <> Raster then
      DisposeObject(nr);

  if R <> C_GNIC_Dim then
      SetLength(Result, 0);
end;

function TAI.GNIC_DebugInfo(hnd: TGNIC_Handle): U_String;
var
  p: PPascalString;
begin
  Result := '';
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.GNIC_DebugInfo) and (hnd <> nil) then
    begin
      FAI_EntryAPI^.GNIC_DebugInfo(hnd, p);
      Result := p^;
      Dispose(p);
    end;
end;

class function TAI.Init_SS_Train_Parameter(train_sync_file, train_output: U_String): PSS_Train_Parameter;
begin
  new(Result);
  FillPtrByte(Result, SizeOf(TSS_Train_Parameter), 0);

  Result^.imgHnd_ptr := nil;
  Result^.imgHnd_num := 0;
  Result^.color := nil;
  Result^.train_sync_file := Alloc_P_Bytes(train_sync_file);
  Result^.train_output := Alloc_P_Bytes(train_output);

  Result^.timeout := C_Tick_Hour;
  Result^.weight_decay := 0.0001;
  Result^.momentum := 0.9;
  Result^.iterations_without_progress_threshold := 2000;
  Result^.learning_rate := 0.01;
  Result^.completed_learning_rate := 0.00001;
  Result^.all_bn_running_stats_window_sizes := 1000;
  Result^.img_crops_batch := 16;

  Result^.control := nil;
  Result^.training_average_loss := 0;
  Result^.training_learning_rate := 0;
end;

class procedure TAI.Free_SS_Train_Parameter(param: PSS_Train_Parameter);
begin
  Free_P_Bytes(param^.train_sync_file);
  Free_P_Bytes(param^.train_output);
  Dispose(param);
end;

function TAI.SS_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; param: PSS_Train_Parameter; const colorPool: TSegmentationColorTable): Boolean;
var
  tk: TTimeTick;
  i: Integer;
  list: TImageList_Decl;
  imgBuff: array of TImage_Handle;
  imgBuff_p: array of PImage_Handle;
begin
  Result := False;

  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SS_Train) then
      exit;

  list := TImageList_Decl.Create;
  for i := 0 to imgList.Count - 1 do
    begin
      if imgList[i].SegmentationMaskList.Count > 0 then
          list.Add(imgList[i]);
    end;
  SetLength(imgBuff, list.Count);
  SetLength(imgBuff_p, list.Count);
  for i := 0 to list.Count - 1 do
    begin
      imgBuff[i].image := list[i];
      imgBuff[i].AccessImage := 0;
      imgBuff[i].AccessDetectorImage := 0;
      imgBuff[i].AccessDetectorRect := 0;
      imgBuff[i].AccessMask := 0;
      imgBuff_p[i] := @imgBuff[i];
    end;

  DoStatus('build segmentation merge space.');
  tk := GetTimeTick();
  if LargeScale_ then
      imgList.LargeScale_BuildMaskMerge(RSeri, colorPool)
  else
      imgList.BuildMaskMerge(colorPool);
  DoStatus('done segmentation merge time %dms', [GetTimeTick - tk]);
  DisposeObject(list);

  TrainingControl.pause := 0;
  TrainingControl.stop := 0;

  param^.imgHnd_ptr := @imgBuff_p[0];
  param^.imgHnd_num := Length(imgBuff_p);
  param^.color := @colorPool;
  param^.control := @TrainingControl;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := True;
      FAI_EntryAPI^.RasterSerialized := RSeri;
      imgList.SerializedAndRecycleMemory(RSeri);
    end
  else
      FAI_EntryAPI^.RasterSerialized := nil;

  FAI_EntryAPI^.SerializedTime := GetTimeTick();

  try
      Result := FAI_EntryAPI^.SS_Train(param) > 0;
  except
      Result := False;
  end;

  if LargeScale_ then
    begin
      RSeri.EnabledReadHistory := False;
      imgList.SerializedAndRecycleMemory(RSeri);
      RSeri.ClearHistory;
      FAI_EntryAPI^.RasterSerialized := nil;
    end;

  Last_training_average_loss := param^.training_average_loss;
  Last_training_learning_rate := param^.training_learning_rate;
  completed_learning_rate := param^.completed_learning_rate;

  param^.imgHnd_ptr := nil;
  param^.imgHnd_num := 0;
  param^.color := nil;
  param^.control := nil;

  SetLength(imgBuff, 0);
  SetLength(imgBuff_p, 0);
end;

function TAI.SS_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; param: PSS_Train_Parameter; const colorPool: TSegmentationColorTable): Boolean;
var
  tk: TTimeTick;
  i, j: Integer;
  list: TImageList_Decl;
  imgBuff: array of TImage_Handle;
  imgBuff_p: array of PImage_Handle;
begin
  Result := False;

  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SS_Train) then
      exit;

  list := TImageList_Decl.Create;
  for j := 0 to imgMat.Count - 1 do
    for i := 0 to imgMat[j].Count - 1 do
      begin
        if imgMat[j][i].SegmentationMaskList.Count > 0 then
            list.Add(imgMat[j][i]);
      end;
  SetLength(imgBuff, list.Count);
  SetLength(imgBuff_p, list.Count);
  for i := 0 to list.Count - 1 do
    begin
      imgBuff[i].image := list[i];
      imgBuff[i].AccessImage := 0;
      imgBuff[i].AccessDetectorImage := 0;
      imgBuff[i].AccessDetectorRect := 0;
      imgBuff[i].AccessMask := 0;
      imgBuff_p[i] := @imgBuff[i];
    end;

  DoStatus('build segmentation merge space.');
  tk := GetTimeTick();

  if LargeScale_ then
      imgMat.LargeScale_BuildMaskMerge(RSeri, colorPool)
  else
      imgMat.BuildMaskMerge(colorPool);

  DoStatus('done segmentation merge time %dms', [GetTimeTick - tk]);
  DisposeObject(list);

  TrainingControl.pause := 0;
  TrainingControl.stop := 0;

  param^.imgHnd_ptr := @imgBuff_p[0];
  param^.imgHnd_num := Length(imgBuff_p);
  param^.color := @colorPool;
  param^.control := @TrainingControl;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := True;
      FAI_EntryAPI^.RasterSerialized := RSeri;
      imgMat.SerializedAndRecycleMemory(RSeri);
    end
  else
      FAI_EntryAPI^.RasterSerialized := nil;

  FAI_EntryAPI^.SerializedTime := GetTimeTick();

  try
      Result := FAI_EntryAPI^.SS_Train(param) > 0;
  except
      Result := False;
  end;

  if LargeScale_ then
    begin
      RSeri.EnabledReadHistory := False;
      imgMat.SerializedAndRecycleMemory(RSeri);
      RSeri.ClearHistory;
      FAI_EntryAPI^.RasterSerialized := nil;
    end;

  Last_training_average_loss := param^.training_average_loss;
  Last_training_learning_rate := param^.training_learning_rate;
  completed_learning_rate := param^.completed_learning_rate;

  param^.imgHnd_ptr := nil;
  param^.imgHnd_num := 0;
  param^.color := nil;
  param^.control := nil;

  SetLength(imgBuff, 0);
  SetLength(imgBuff_p, 0);
end;

function TAI.SS_Train_Stream(imgList: TAI_ImageList; param: PSS_Train_Parameter; const colorPool: TSegmentationColorTable): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if SS_Train(False, nil, imgList, param, colorPool) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.SS_Train_Stream(imgMat: TAI_ImageMatrix; param: PSS_Train_Parameter; const colorPool: TSegmentationColorTable): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if SS_Train(False, nil, imgMat, param, colorPool) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.SS_Open(train_file: SystemString): TSS_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.SS_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      Result := FAI_EntryAPI^.SS_Init(train_file_buff);
      Free_P_Bytes(train_file_buff);
      if Result <> nil then
          DoStatus('segmantic segmentation open: %s', [train_file]);
    end
  else
      Result := nil;
end;

function TAI.SS_Open_Stream(stream: TMemoryStream64): TSS_Handle;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.SS_Init_Memory) then
    begin
      Result := FAI_EntryAPI^.SS_Init_Memory(stream.memory, stream.Size);
      DoStatus('segmantic segmentation open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.SS_Open_Stream(train_file: SystemString): TSS_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := SS_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('segmantic segmentation open: %s', [train_file]);
end;

function TAI.SS_Close(var hnd: TSS_Handle): Boolean;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.SS_Free) and (hnd <> nil) then
    begin
      Result := FAI_EntryAPI^.SS_Free(hnd) = 0;
      DoStatus('segmantic segmentation close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

class function TAI.SS_TranslateColor(const c: WORD): TRColorEntry;
begin
  Result.BGRA := TRColor(c);
end;

function TAI.SS_Process(parallel_: Boolean; hnd: TSS_Handle; InputRaster: TMemoryRaster; colorPool: TSegmentationColorTable; SSTokenOutput: TPascalStringList): TMemoryRaster;
var
  R: Integer;
  dr: TMemoryRaster;
  SSMatrix: array of WORD;
  TokenHash: THashList;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    i: Integer;
    p: PWORD;
    tk: U_String;
  begin
    p := @SSMatrix[pass * InputRaster.Width];
    for i := 0 to dr.Width - 1 do
      begin
        if colorPool = nil then
            dr.Pixel[i, pass] := SS_TranslateColor(p^).BGRA
        else if colorPool.GetIDColorAndToken(p^, RColor(0, 0, 0, $FF), '', dr.PixelPtr[i, pass]^, tk) then
          begin
            if not TokenHash.Exists(tk) then
              begin
                LockObject(TokenHash);
                TokenHash.Add(tk, nil, False);
                UnLockObject(TokenHash);
              end;
          end;
        inc(p);
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass, i: Integer;
    p: PWORD;
    tk: U_String;
  begin
    for pass := 0 to dr.Height - 1 do
      begin
        p := @SSMatrix[pass * InputRaster.Width];
        for i := 0 to dr.Width - 1 do
          begin
            if colorPool = nil then
                dr.Pixel[i, pass] := SS_TranslateColor(p^).BGRA
            else if colorPool.GetIDColorAndToken(p^, RColor(0, 0, 0, $FF), '', dr.PixelPtr[i, pass]^, tk) then
              begin
                if not TokenHash.Exists(tk) then
                  begin
                    LockObject(TokenHash);
                    TokenHash.Add(tk, nil, False);
                    UnLockObject(TokenHash);
                  end;
              end;
            inc(p);
          end;
      end;
  end;
{$ENDIF Parallel}


begin
  InputRaster.ReadyBits();
  Result := nil;
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SS_Process) then
      exit;

  SetLength(SSMatrix, InputRaster.Width * InputRaster.Height);

  if FAI_EntryAPI^.SS_Process(hnd, InputRaster.Bits, InputRaster.Width, InputRaster.Height, @SSMatrix[0]) >= 0 then
    begin
      dr := NewRaster();
      dr.SetSize(InputRaster.Width, InputRaster.Height);
      TokenHash := THashList.CustomCreate($FFFF);

{$IFDEF Parallel}
{$IFDEF FPC}
      FPCParallelFor(parallel_, @Nested_ParallelFor, 0, dr.Height - 1);
{$ELSE FPC}
      DelphiParallelFor(parallel_, 0, dr.Height - 1, procedure(pass: Integer)
        var
          i: Integer;
          p: PWORD;
          tk: U_String;
        begin
          p := @SSMatrix[pass * InputRaster.Width];
          for i := 0 to dr.Width - 1 do
            begin
              if colorPool = nil then
                  dr.Pixel[i, pass] := SS_TranslateColor(p^).BGRA
              else if colorPool.GetIDColorAndToken(p^, RColor(0, 0, 0, $FF), '', dr.PixelPtr[i, pass]^, tk) then
                begin
                  if not TokenHash.Exists(tk) then
                    begin
                      LockObject(TokenHash);
                      TokenHash.Add(tk, nil, False);
                      UnLockObject(TokenHash);
                    end;
                end;
              inc(p);
            end;
        end);
{$ENDIF FPC}
{$ELSE Parallel}
      DoFor;
{$ENDIF Parallel}
      if SSTokenOutput <> nil then
          TokenHash.GetNameList(SSTokenOutput);
      DisposeObject(TokenHash);
      Result := dr;
    end;

  SetLength(SSMatrix, 0);
end;

function TAI.SS_Process(hnd: TSS_Handle; InputRaster: TMemoryRaster; colorPool: TSegmentationColorTable; SSTokenOutput: TPascalStringList): TMemoryRaster;
begin
  Result := SS_Process(True, hnd, InputRaster, colorPool, SSTokenOutput);
end;

function TAI.SS_ProcessMatrix(hnd: TSS_Handle; mat_hnd: TMatrix_Image_Handle; Width, Height: Integer; colorPool: TSegmentationColorTable; SSTokenOutput: TPascalStringList): TMemoryRaster;
var
  R: Integer;
  dr: TMemoryRaster;
  SSMatrix: array of WORD;
  TokenHash: THashList;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    i: Integer;
    p: PWORD;
    tk: U_String;
  begin
    p := @SSMatrix[pass * Width];
    for i := 0 to dr.Width - 1 do
      begin
        if colorPool = nil then
            dr.Pixel[i, pass] := SS_TranslateColor(p^).BGRA
        else if colorPool.GetIDColorAndToken(p^, RColor(0, 0, 0, $FF), '', dr.PixelPtr[i, pass]^, tk) then
          begin
            if not TokenHash.Exists(tk) then
              begin
                LockObject(TokenHash);
                TokenHash.Add(tk, nil, False);
                UnLockObject(TokenHash);
              end;
          end;
        inc(p);
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass, i: Integer;
    p: PWORD;
    tk: U_String;
  begin
    for pass := 0 to dr.Height - 1 do
      begin
        p := @SSMatrix[pass * Width];
        for i := 0 to dr.Width - 1 do
          begin
            if colorPool = nil then
                dr.Pixel[i, pass] := SS_TranslateColor(p^).BGRA
            else if colorPool.GetIDColorAndToken(p^, RColor(0, 0, 0, $FF), '', dr.PixelPtr[i, pass]^, tk) then
              begin
                if not TokenHash.Exists(tk) then
                  begin
                    LockObject(TokenHash);
                    TokenHash.Add(tk, nil, False);
                    UnLockObject(TokenHash);
                  end;
              end;
            inc(p);
          end;
      end;
  end;
{$ENDIF Parallel}


begin
  Result := nil;
  if hnd = nil then
      exit;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.SS_Process) then
      exit;

  SetLength(SSMatrix, Width * Height);

  if FAI_EntryAPI^.SS_Process_Image(hnd, mat_hnd, @SSMatrix[0]) >= 0 then
    begin
      dr := NewRaster();
      dr.SetSize(Width, Height);
      TokenHash := THashList.CustomCreate($FFFF);

{$IFDEF Parallel}
{$IFDEF FPC}
      FPCParallelFor(@Nested_ParallelFor, 0, dr.Height - 1);
{$ELSE FPC}
      DelphiParallelFor(0, dr.Height - 1, procedure(pass: Integer)
        var
          i: Integer;
          p: PWORD;
          tk: U_String;
        begin
          p := @SSMatrix[pass * Width];
          for i := 0 to dr.Width - 1 do
            begin
              if colorPool = nil then
                  dr.Pixel[i, pass] := SS_TranslateColor(p^).BGRA
              else if colorPool.GetIDColorAndToken(p^, RColor(0, 0, 0, $FF), '', dr.PixelPtr[i, pass]^, tk) then
                begin
                  if not TokenHash.Exists(tk) then
                    begin
                      LockObject(TokenHash);
                      TokenHash.Add(tk, nil, False);
                      UnLockObject(TokenHash);
                    end;
                end;
              inc(p);
            end;
        end);
{$ENDIF FPC}
{$ELSE Parallel}
      DoFor;
{$ENDIF Parallel}
      if SSTokenOutput <> nil then
          TokenHash.GetNameList(SSTokenOutput);
      DisposeObject(TokenHash);
      Result := dr;
    end;

  SetLength(SSMatrix, 0);
end;

procedure TAI.SS_ProcessAsync(hnd: TSS_Handle; SSInput: TMemoryRaster; colorPool: TSegmentationColorTable;
OnResultC: TSS_ProcessOnResultCall; OnResultM: TSS_ProcessOnResultMethod; OnResultP: TSS_ProcessOnResultProc);
var
  ResultData: TSS_ResultProcessor;
  R: Integer;
  dr: TMemoryRaster;
begin
  SSInput.ReadyBits();
  ResultData := TSS_ResultProcessor.Create;
  ResultData.SSInput := SSInput;
  ResultData.colorPool := colorPool;
  ResultData.OnResultC := OnResultC;
  ResultData.OnResultM := OnResultM;
  ResultData.OnResultP := OnResultP;

  if (hnd = nil) or (FAI_EntryAPI = nil) or (not Assigned(FAI_EntryAPI^.SS_Process)) then
    begin
      ResultData.DoFailed();
      DisposeObject(ResultData);
      exit;
    end;

  SetLength(ResultData.SSMatrix, SSInput.Width * SSInput.Height);

  if FAI_EntryAPI^.SS_Process(hnd, SSInput.Bits, SSInput.Width, SSInput.Height, @ResultData.SSMatrix[0]) >= 0 then
    begin
      ResultData.SSInput := NewRaster();
      ResultData.SSInput.Assign(SSInput);
      TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}ResultData.ThRun);
    end
  else
    begin
      ResultData.DoFailed();
      SetLength(ResultData.SSMatrix, 0);
      DisposeObject(ResultData);
    end;
end;

function TAI.SS_DebugInfo(hnd: TSS_Handle): U_String;
var
  p: PPascalString;
begin
  Result := '';
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.SS_DebugInfo) and (hnd <> nil) then
    begin
      FAI_EntryAPI^.SS_DebugInfo(hnd, p);
      Result := p^;
      Dispose(p);
    end;
end;

function TAI.Tracker_Open_Matrix(mat_hnd: TMatrix_Image_Handle; const track_rect: TRectV2): TTracker_Handle;
var
  A: TAI_Rect;
begin
  Result := nil;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.Start_Tracker_matrix) then
      exit;
  A := AIRect(ForwardRect(track_rect));

  try
    Result := FAI_EntryAPI^.Start_Tracker_matrix(mat_hnd, @A);
    FAI_EntryAPI^.Update_Tracker_matrix(Result, mat_hnd, A);
  except
  end;
end;

function TAI.Tracker_Update_Matrix(hnd: TTracker_Handle; mat_hnd: TMatrix_Image_Handle; var track_rect: TRectV2): Double;
var
  A: TAI_Rect;
begin
  Result := 0;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.Update_Tracker_matrix) then
      exit;
  try
      Result := FAI_EntryAPI^.Update_Tracker_matrix(hnd, mat_hnd, A);
  except
  end;
  track_rect := RectV2(A);
end;

function TAI.Tracker_Update_NoScale_Matrix(hnd: TTracker_Handle; mat_hnd: TMatrix_Image_Handle; var track_rect: TRectV2): Double;
var
  A: TAI_Rect;
begin
  Result := 0;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.Update_Tracker_NoScale_matrix) then
      exit;

  try
      Result := FAI_EntryAPI^.Update_Tracker_NoScale_matrix(hnd, mat_hnd, A);
  except
  end;
  track_rect := RectV2(A);
end;

function TAI.Tracker_Open_RGB(RGB_Hnd: TRGB_Image_Handle; const track_rect: TRectV2): TTracker_Handle;
var
  A: TAI_Rect;
begin
  Result := nil;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.Start_Tracker) then
      exit;
  A := AIRect(ForwardRect(track_rect));
  try
    Result := FAI_EntryAPI^.Start_Tracker(RGB_Hnd, @A);
    FAI_EntryAPI^.Update_Tracker(Result, RGB_Hnd, A);
  except
  end;
end;

function TAI.Tracker_Update_RGB(hnd: TTracker_Handle; RGB_Hnd: TRGB_Image_Handle; var track_rect: TRectV2): Double;
var
  A: TAI_Rect;
begin
  Result := 0;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.Update_Tracker_matrix) then
      exit;
  try
    Result := FAI_EntryAPI^.Update_Tracker(hnd, RGB_Hnd, A);
    track_rect := RectV2(A);
  except
  end;
end;

function TAI.Tracker_Update_NoScale_RGB(hnd: TTracker_Handle; RGB_Hnd: TRGB_Image_Handle; var track_rect: TRectV2): Double;
var
  A: TAI_Rect;
begin
  Result := 0;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.Update_Tracker_NoScale) then
      exit;
  try
    Result := FAI_EntryAPI^.Update_Tracker_NoScale(hnd, RGB_Hnd, A);
    track_rect := RectV2(A);
  except
  end;
end;

function TAI.Tracker_Open_Matrix_Multi(parallel_: Boolean; mat_hnd: TMatrix_Image_Handle; const track_rect: TArrayRectV2): TTracker_Handle_Array;
var
  buff: TTracker_Handle_Array;

{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    buff[pass] := Tracker_Open_Matrix(mat_hnd, track_rect[pass]);
  end;
{$ENDIF FPC}


begin
  SetLength(buff, Length(track_rect));
{$IFDEF FPC}
  FPCParallelFor(parallel_, @Nested_ParallelFor, 0, Length(track_rect) - 1);
{$ELSE FPC}
  DelphiParallelFor(parallel_, 0, Length(track_rect) - 1, procedure(pass: Integer)
    begin
      buff[pass] := Tracker_Open_Matrix(mat_hnd, track_rect[pass]);
    end);
{$ENDIF FPC}
  Result := buff;
end;

function TAI.Tracker_Open_Matrix_Multi(parallel_: Boolean; mat_hnd: TMatrix_Image_Handle; const MMOD_Desc: TMMOD_Desc): TTracker_Handle_Array;
var
  buff: TTracker_Handle_Array;

{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    buff[pass] := Tracker_Open_Matrix(mat_hnd, MMOD_Desc[pass].R);
  end;
{$ENDIF FPC}


begin
  SetLength(buff, Length(MMOD_Desc));
{$IFDEF FPC}
  FPCParallelFor(parallel_, @Nested_ParallelFor, 0, Length(MMOD_Desc) - 1);
{$ELSE FPC}
  DelphiParallelFor(parallel_, 0, Length(MMOD_Desc) - 1, procedure(pass: Integer)
    begin
      buff[pass] := Tracker_Open_Matrix(mat_hnd, MMOD_Desc[pass].R);
    end);
{$ENDIF FPC}
  Result := buff;
end;

function TAI.Tracker_Open_Matrix_Multi(parallel_: Boolean; mat_hnd: TMatrix_Image_Handle; const OD_Desc: TOD_Desc): TTracker_Handle_Array;
var
  buff: TTracker_Handle_Array;

{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    buff[pass] := Tracker_Open_Matrix(mat_hnd, RectV2(OD_Desc[pass]));
  end;
{$ENDIF FPC}


begin
  SetLength(buff, Length(OD_Desc));
{$IFDEF FPC}
  FPCParallelFor(parallel_, @Nested_ParallelFor, 0, Length(OD_Desc) - 1);
{$ELSE FPC}
  DelphiParallelFor(parallel_, 0, Length(OD_Desc) - 1, procedure(pass: Integer)
    begin
      buff[pass] := Tracker_Open_Matrix(mat_hnd, RectV2(OD_Desc[pass]));
    end);
{$ENDIF FPC}
  Result := buff;
end;

function TAI.Tracker_Update_Matrix_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; mat_hnd: TMatrix_Image_Handle; var track_rect: TArrayRectV2): TLVec;
var
  buff: TArrayRectV2;
  R_: TLVec;
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    R_[pass] := Tracker_Update_Matrix(hnd[pass], mat_hnd, buff[pass]);
  end;
{$ENDIF FPC}


begin
  SetLength(buff, Length(hnd));
  SetLength(R_, Length(hnd));

{$IFDEF FPC}
  FPCParallelFor(parallel_, @Nested_ParallelFor, 0, Length(hnd) - 1);
{$ELSE FPC}
  DelphiParallelFor(parallel_, 0, Length(hnd) - 1, procedure(pass: Integer)
    begin
      R_[pass] := Tracker_Update_Matrix(hnd[pass], mat_hnd, buff[pass]);
    end);
{$ENDIF FPC}
  track_rect := buff;
  Result := R_;
end;

procedure TAI.Tracker_Update_Matrix_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; mat_hnd: TMatrix_Image_Handle; var MMOD_Desc: TMMOD_Desc);
var
  buff: TMMOD_Desc;
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    buff[pass].confidence := Tracker_Update_Matrix(hnd[pass], mat_hnd, buff[pass].R);
  end;
{$ENDIF FPC}


begin
  SetLength(buff, Length(hnd));

{$IFDEF FPC}
  FPCParallelFor(parallel_, @Nested_ParallelFor, 0, Length(hnd) - 1);
{$ELSE FPC}
  DelphiParallelFor(parallel_, 0, Length(hnd) - 1, procedure(pass: Integer)
    begin
      buff[pass].confidence := Tracker_Update_Matrix(hnd[pass], mat_hnd, buff[pass].R);
    end);
{$ENDIF FPC}
  MMOD_Desc := buff;
end;

procedure TAI.Tracker_Update_Matrix_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; mat_hnd: TMatrix_Image_Handle; var OD_Desc: TOD_Desc);
var
  buff: TOD_Desc;
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    R_: TRectV2;
  begin
    buff[pass].confidence := Tracker_Update_Matrix(hnd[pass], mat_hnd, R_);
    buff[pass].Left := Round(R_[0, 0]);
    buff[pass].Top := Round(R_[0, 1]);
    buff[pass].Right := Round(R_[1, 0]);
    buff[pass].Bottom := Round(R_[1, 1]);
  end;
{$ENDIF FPC}


begin
  SetLength(buff, Length(hnd));

{$IFDEF FPC}
  FPCParallelFor(parallel_, @Nested_ParallelFor, 0, Length(hnd) - 1);
{$ELSE FPC}
  DelphiParallelFor(parallel_, 0, Length(hnd) - 1, procedure(pass: Integer)
    var
      R_: TRectV2;
    begin
      buff[pass].confidence := Tracker_Update_Matrix(hnd[pass], mat_hnd, R_);
      buff[pass].Left := Round(R_[0, 0]);
      buff[pass].Top := Round(R_[0, 1]);
      buff[pass].Right := Round(R_[1, 0]);
      buff[pass].Bottom := Round(R_[1, 1]);
    end);
{$ENDIF FPC}
  OD_Desc := buff;
end;

function TAI.Tracker_Open_RGB_Multi(parallel_: Boolean; RGB_Hnd: TRGB_Image_Handle; const track_rect: TArrayRectV2): TTracker_Handle_Array;
var
  buff: TTracker_Handle_Array;

{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    buff[pass] := Tracker_Open_RGB(RGB_Hnd, track_rect[pass]);
  end;
{$ENDIF FPC}


begin
  SetLength(buff, Length(track_rect));
{$IFDEF FPC}
  FPCParallelFor(parallel_, @Nested_ParallelFor, 0, Length(track_rect) - 1);
{$ELSE FPC}
  DelphiParallelFor(parallel_, 0, Length(track_rect) - 1, procedure(pass: Integer)
    begin
      buff[pass] := Tracker_Open_RGB(RGB_Hnd, track_rect[pass]);
    end);
{$ENDIF FPC}
  Result := buff;
end;

function TAI.Tracker_Open_RGB_Multi(parallel_: Boolean; RGB_Hnd: TRGB_Image_Handle; const MMOD_Desc: TMMOD_Desc): TTracker_Handle_Array;
var
  buff: TTracker_Handle_Array;

{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    buff[pass] := Tracker_Open_RGB(RGB_Hnd, MMOD_Desc[pass].R);
  end;
{$ENDIF FPC}


begin
  SetLength(buff, Length(MMOD_Desc));
{$IFDEF FPC}
  FPCParallelFor(parallel_, @Nested_ParallelFor, 0, Length(MMOD_Desc) - 1);
{$ELSE FPC}
  DelphiParallelFor(parallel_, 0, Length(MMOD_Desc) - 1, procedure(pass: Integer)
    begin
      buff[pass] := Tracker_Open_RGB(RGB_Hnd, MMOD_Desc[pass].R);
    end);
{$ENDIF FPC}
  Result := buff;
end;

function TAI.Tracker_Open_RGB_Multi(parallel_: Boolean; RGB_Hnd: TRGB_Image_Handle; const OD_Desc: TOD_Desc): TTracker_Handle_Array;
var
  buff: TTracker_Handle_Array;

{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    R_: TRectV2;
  begin
    R_ := RectV2(OD_Desc[pass]);
    buff[pass] := Tracker_Open_RGB(RGB_Hnd, R_);
  end;
{$ENDIF FPC}


begin
  SetLength(buff, Length(OD_Desc));
{$IFDEF FPC}
  FPCParallelFor(parallel_, @Nested_ParallelFor, 0, Length(OD_Desc) - 1);
{$ELSE FPC}
  DelphiParallelFor(parallel_, 0, Length(OD_Desc) - 1, procedure(pass: Integer)
    var
      R_: TRectV2;
    begin
      R_ := RectV2(OD_Desc[pass]);
      buff[pass] := Tracker_Open_RGB(RGB_Hnd, R_);
    end);
{$ENDIF FPC}
  Result := buff;
end;

function TAI.Tracker_Update_RGB_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; RGB_Hnd: TRGB_Image_Handle; var track_rect: TArrayRectV2): TLVec;
var
  buff: TArrayRectV2;
  R_: TLVec;
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    R_[pass] := Tracker_Update_RGB(hnd[pass], RGB_Hnd, buff[pass]);
  end;
{$ENDIF FPC}


begin
  SetLength(buff, Length(hnd));
  SetLength(R_, Length(hnd));

{$IFDEF FPC}
  FPCParallelFor(parallel_, @Nested_ParallelFor, 0, Length(hnd) - 1);
{$ELSE FPC}
  DelphiParallelFor(parallel_, 0, Length(hnd) - 1, procedure(pass: Integer)
    begin
      R_[pass] := Tracker_Update_RGB(hnd[pass], RGB_Hnd, buff[pass]);
    end);
{$ENDIF FPC}
  track_rect := buff;
  Result := R_;
end;

procedure TAI.Tracker_Update_RGB_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; RGB_Hnd: TRGB_Image_Handle; var MMOD_Desc: TMMOD_Desc);
var
  buff: TMMOD_Desc;
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    buff[pass].confidence := Tracker_Update_RGB(hnd[pass], RGB_Hnd, buff[pass].R);
  end;
{$ENDIF FPC}


begin
  SetLength(buff, Length(hnd));

{$IFDEF FPC}
  FPCParallelFor(parallel_, @Nested_ParallelFor, 0, Length(hnd) - 1);
{$ELSE FPC}
  DelphiParallelFor(parallel_, 0, Length(hnd) - 1, procedure(pass: Integer)
    begin
      buff[pass].confidence := Tracker_Update_RGB(hnd[pass], RGB_Hnd, buff[pass].R);
    end);
{$ENDIF FPC}
  MMOD_Desc := buff;
end;

procedure TAI.Tracker_Update_RGB_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; RGB_Hnd: TRGB_Image_Handle; var OD_Desc: TOD_Desc);
var
  buff: TOD_Desc;
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    R_: TRectV2;
  begin
    buff[pass].confidence := Tracker_Update_RGB(hnd[pass], RGB_Hnd, R_);
    buff[pass].Left := Round(R_[0, 0]);
    buff[pass].Top := Round(R_[0, 1]);
    buff[pass].Right := Round(R_[1, 0]);
    buff[pass].Bottom := Round(R_[1, 1]);
  end;
{$ENDIF FPC}


begin
  SetLength(buff, Length(hnd));

{$IFDEF FPC}
  FPCParallelFor(parallel_, @Nested_ParallelFor, 0, Length(hnd) - 1);
{$ELSE FPC}
  DelphiParallelFor(parallel_, 0, Length(hnd) - 1, procedure(pass: Integer)
    var
      R_: TRectV2;
    begin
      buff[pass].confidence := Tracker_Update_RGB(hnd[pass], RGB_Hnd, R_);
      buff[pass].Left := Round(R_[0, 0]);
      buff[pass].Top := Round(R_[0, 1]);
      buff[pass].Right := Round(R_[1, 0]);
      buff[pass].Bottom := Round(R_[1, 1]);
    end);
{$ENDIF FPC}
  OD_Desc := buff;
end;

function TAI.Tracker_Open_Multi(parallel_: Boolean; Raster: TMemoryRaster; const track_rect: TArrayRectV2): TTracker_Handle_Array;
var
  RGB_Hnd: TRGB_Image_Handle;
begin
  SetLength(Result, 0);
  if (FAI_EntryAPI = nil) then
      exit;

  RGB_Hnd := Prepare_RGB_Image(Raster);
  if RGB_Hnd = nil then
      exit;
  Result := Tracker_Open_RGB_Multi(parallel_, RGB_Hnd, track_rect);
  Close_RGB_Image(RGB_Hnd);
end;

function TAI.Tracker_Open_Multi(parallel_: Boolean; Raster: TMemoryRaster; const MMOD_Desc: TMMOD_Desc): TTracker_Handle_Array;
var
  RGB_Hnd: TRGB_Image_Handle;
begin
  SetLength(Result, 0);
  if (FAI_EntryAPI = nil) then
      exit;

  RGB_Hnd := Prepare_RGB_Image(Raster);
  if RGB_Hnd = nil then
      exit;
  Result := Tracker_Open_RGB_Multi(parallel_, RGB_Hnd, MMOD_Desc);
  Close_RGB_Image(RGB_Hnd);
end;

function TAI.Tracker_Open_Multi(parallel_: Boolean; Raster: TMemoryRaster; const MMOD_DescArray: TMMOD_Desc_Array): TTracker_Handle_ArrayOfArray;
var
  RGB_Hnd: TRGB_Image_Handle;
  buff: TTracker_Handle_ArrayOfArray;
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    buff[pass] := Tracker_Open_RGB_Multi(parallel_, RGB_Hnd, MMOD_DescArray[pass]);
  end;
{$ENDIF FPC}


begin
  SetLength(Result, 0);
  if (FAI_EntryAPI = nil) then
      exit;

  RGB_Hnd := Prepare_RGB_Image(Raster);
  if RGB_Hnd = nil then
      exit;
  SetLength(buff, Length(MMOD_DescArray));

{$IFDEF FPC}
  FPCParallelFor(parallel_, @Nested_ParallelFor, 0, Length(MMOD_DescArray) - 1);
{$ELSE FPC}
  DelphiParallelFor(parallel_, 0, Length(MMOD_DescArray) - 1, procedure(pass: Integer)
    begin
      buff[pass] := Tracker_Open_RGB_Multi(parallel_, RGB_Hnd, MMOD_DescArray[pass]);
    end);
{$ENDIF FPC}
  Close_RGB_Image(RGB_Hnd);
  Result := buff;
end;

function TAI.Tracker_Open_Multi(parallel_: Boolean; Raster: TMemoryRaster; const OD_Desc: TOD_Desc): TTracker_Handle_Array;
var
  RGB_Hnd: TRGB_Image_Handle;
begin
  SetLength(Result, 0);
  if (FAI_EntryAPI = nil) then
      exit;

  RGB_Hnd := Prepare_RGB_Image(Raster);
  if RGB_Hnd = nil then
      exit;
  Result := Tracker_Open_RGB_Multi(parallel_, RGB_Hnd, OD_Desc);
  Close_RGB_Image(RGB_Hnd);
end;

function TAI.Tracker_Update_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; Raster: TMemoryRaster; var track_rect: TArrayRectV2): TLVec;
var
  RGB_Hnd: TRGB_Image_Handle;
begin
  SetLength(Result, 0);
  if (FAI_EntryAPI = nil) then
      exit;

  RGB_Hnd := Prepare_RGB_Image(Raster);
  if RGB_Hnd = nil then
      exit;
  Result := Tracker_Update_RGB_Multi(parallel_, hnd, RGB_Hnd, track_rect);
  Close_RGB_Image(RGB_Hnd);
end;

procedure TAI.Tracker_Update_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; Raster: TMemoryRaster; var MMOD_Desc: TMMOD_Desc);
var
  RGB_Hnd: TRGB_Image_Handle;
begin
  if (FAI_EntryAPI = nil) then
      exit;

  RGB_Hnd := Prepare_RGB_Image(Raster);
  if RGB_Hnd = nil then
      exit;
  Tracker_Update_RGB_Multi(parallel_, hnd, RGB_Hnd, MMOD_Desc);
  Close_RGB_Image(RGB_Hnd);
end;

procedure TAI.Tracker_Update_Multi(parallel_: Boolean; hnd: TTracker_Handle_ArrayOfArray; Raster: TMemoryRaster; var MMOD_DescArray: TMMOD_Desc_Array);
var
  RGB_Hnd: TRGB_Image_Handle;
  buff: TMMOD_Desc_Array;
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  begin
    Tracker_Update_RGB_Multi(parallel_, hnd[pass], RGB_Hnd, buff[pass]);
  end;
{$ENDIF FPC}


begin
  if (FAI_EntryAPI = nil) then
      exit;

  RGB_Hnd := Prepare_RGB_Image(Raster);
  if RGB_Hnd = nil then
      exit;
  SetLength(buff, Length(hnd));

{$IFDEF FPC}
  FPCParallelFor(parallel_, @Nested_ParallelFor, 0, Length(MMOD_DescArray) - 1);
{$ELSE FPC}
  DelphiParallelFor(parallel_, 0, Length(hnd) - 1, procedure(pass: Integer)
    begin
      Tracker_Update_RGB_Multi(parallel_, hnd[pass], RGB_Hnd, buff[pass]);
    end);
{$ENDIF FPC}
  Close_RGB_Image(RGB_Hnd);
  MMOD_DescArray := buff;
end;

procedure TAI.Tracker_Update_Multi(parallel_: Boolean; hnd: TTracker_Handle_Array; Raster: TMemoryRaster; var OD_Desc: TOD_Desc);
var
  RGB_Hnd: TRGB_Image_Handle;
begin
  if (FAI_EntryAPI = nil) then
      exit;

  RGB_Hnd := Prepare_RGB_Image(Raster);
  if RGB_Hnd = nil then
      exit;
  Tracker_Update_RGB_Multi(parallel_, hnd, RGB_Hnd, OD_Desc);
  Close_RGB_Image(RGB_Hnd);
end;

function TAI.Tracker_Open(Raster: TMemoryRaster; const track_rect: TRect): TTracker_Handle;
var
  RGB_Hnd: TRGB_Image_Handle;
  A: TAI_Rect;
begin
  Result := nil;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.Start_Tracker) then
      exit;
  RGB_Hnd := Prepare_RGB_Image(Raster);
  if RGB_Hnd = nil then
      exit;
  A := AIRect(ForwardRect(track_rect));
  try
    Result := FAI_EntryAPI^.Start_Tracker(RGB_Hnd, @A);
    FAI_EntryAPI^.Update_Tracker(Result, RGB_Hnd, A);
    Close_RGB_Image(RGB_Hnd);
  except
  end;
end;

function TAI.Tracker_Update(hnd: TTracker_Handle; Raster: TMemoryRaster; var track_rect: TRect): Double;
var
  RGB_Hnd: TRGB_Image_Handle;
  A: TAI_Rect;
begin
  Result := 0;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.Update_Tracker) then
      exit;

  RGB_Hnd := Prepare_RGB_Image(Raster);
  if RGB_Hnd = nil then
      exit;
  try
    Result := FAI_EntryAPI^.Update_Tracker(hnd, RGB_Hnd, A);
    track_rect := Rect(A);
    Close_RGB_Image(RGB_Hnd);
  except
  end;
end;

function TAI.Tracker_Update_NoScale(hnd: TTracker_Handle; Raster: TMemoryRaster; var track_rect: TRect): Double;
var
  RGB_Hnd: TRGB_Image_Handle;
  A: TAI_Rect;
begin
  Result := 0;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.Update_Tracker_NoScale) then
      exit;

  RGB_Hnd := Prepare_RGB_Image(Raster);
  if RGB_Hnd = nil then
      exit;
  try
    Result := FAI_EntryAPI^.Update_Tracker_NoScale(hnd, RGB_Hnd, A);
    track_rect := Rect(A);
    Close_RGB_Image(RGB_Hnd);
  except
  end;
end;

function TAI.Tracker_Open(Raster: TMemoryRaster; const track_rect: TRectV2): TTracker_Handle;
var
  RGB_Hnd: TRGB_Image_Handle;
  A: TAI_Rect;
begin
  Result := nil;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.Start_Tracker) then
      exit;
  RGB_Hnd := Prepare_RGB_Image(Raster);
  if RGB_Hnd = nil then
      exit;
  A := AIRect(ForwardRect(track_rect));
  try
    Result := FAI_EntryAPI^.Start_Tracker(RGB_Hnd, @A);
    FAI_EntryAPI^.Update_Tracker(Result, RGB_Hnd, A);
    Close_RGB_Image(RGB_Hnd);
  except
  end;
end;

function TAI.Tracker_Update(hnd: TTracker_Handle; Raster: TMemoryRaster; var track_rect: TRectV2): Double;
var
  RGB_Hnd: TRGB_Image_Handle;
  A: TAI_Rect;
begin
  Result := 0;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.Update_Tracker) then
      exit;

  RGB_Hnd := Prepare_RGB_Image(Raster);
  if RGB_Hnd = nil then
      exit;
  try
    Result := FAI_EntryAPI^.Update_Tracker(hnd, RGB_Hnd, A);
    track_rect := RectV2(A);
    Close_RGB_Image(RGB_Hnd);
  except
  end;
end;

function TAI.Tracker_Update_NoScale(hnd: TTracker_Handle; Raster: TMemoryRaster; var track_rect: TRectV2): Double;
var
  RGB_Hnd: TRGB_Image_Handle;
  A: TAI_Rect;
begin
  Result := 0;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.Update_Tracker_NoScale) then
      exit;

  RGB_Hnd := Prepare_RGB_Image(Raster);
  if RGB_Hnd = nil then
      exit;
  try
    Result := FAI_EntryAPI^.Update_Tracker_NoScale(hnd, RGB_Hnd, A);
    track_rect := RectV2(A);
    Close_RGB_Image(RGB_Hnd);
  except
  end;
end;

function TAI.Tracker_Close(var hnd: TTracker_Handle): Boolean;
begin
  Result := False;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.Stop_Tracker) then
      exit;
  try
      FAI_EntryAPI^.Stop_Tracker(hnd);
  except
  end;
  hnd := nil;
  Result := True;
end;

function TAI.Tracker_Close(var hnd: TTracker_Handle_Array): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Length(hnd) - 1 do
      Result := Tracker_Close(hnd[i]) and Result;
  SetLength(hnd, 0);
end;

function TAI.Tracker_Close(var hnd: TTracker_Handle_ArrayOfArray): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Length(hnd) - 1 do
      Result := Tracker_Close(hnd[i]) and Result;
  SetLength(hnd, 0);
end;

class procedure TAI.RebuildOCREngineMD5Label();
var
  dbFile, tmpFile: U_String;
  dbEng, tmpDB: TObjectDataManager;
  md5Info: THashTextEngine;
  rs: TItemRecursionSearch;
  fieldPh: U_String;
  md5Key, md5Value: U_String;
  itmHnd: TItemHandle;
  m64: TMemoryStream64;
begin
  if not FileExistsFromConfigure(C_OCR_Model_Package) then
      RaiseInfo('No exists OCR model package ''%s', [C_OCR_Model_Package]);

  dbFile := WhereFileFromConfigure(C_OCR_Model_Package);
  dbEng := TObjectDataManagerOfCache.Open(dbFile, DBMarshal.ID, True);
  if dbEng.isAbort then
      RaiseInfo('Unable to access file %s', [dbFile.Text]);

  md5Info := THashTextEngine.Create(1024, 1024);

  if dbEng.RecursionSearchFirst('/', '*', rs) then
    begin
      repeat
        if rs.ReturnHeader.ID = DB_Header_Item_ID then
          begin
            fieldPh := dbEng.GetFieldPath(rs.CurrentField.RHeader.CurrentHeader);
            md5Key := umlCombineUnixFileName(fieldPh, rs.ReturnHeader.Name);
            dbEng.ItemFastOpen(rs.ReturnHeader.CurrentHeader, itmHnd);
            m64 := TMemoryStream64.Create;
            dbEng.ItemReadToStream(itmHnd, m64);
            md5Value := umlStreamMD5String(m64);
            DisposeObject(m64);
            dbEng.ItemClose(itmHnd);
            md5Info.SetDefaultText('FileKey', md5Key, md5Value);
            DoStatus('%s md5: %s', [md5Key.Text, md5Value.Text]);
          end;
      until not dbEng.RecursionSearchNext(rs);
    end;

  tmpFile := umlChangeFileExt(dbFile, '.tmp');
  DoStatus('reconstruct %s -> %s', [C_OCR_Model_Package, umlGetFileName(tmpFile).Text]);
  tmpDB := TObjectDataManager.CreateNew(dbEng.Handle^.FixedStringL, tmpFile, dbEng.ID);
  dbEng.CopyTo(tmpDB);
  DoStatus('build md5.txt');
  m64 := TMemoryStream64.Create;
  md5Info.SaveToStream(m64);
  DisposeObject(md5Info);
  tmpDB.ItemWriteFromStream('/', 'md5.txt', m64);
  DisposeObject(m64);
  DisposeObject(dbEng);
  DisposeObject(tmpDB);
  umlDeleteFile(dbFile);
  DoStatus('rename %s -> %s', [umlGetFileName(tmpFile).Text, umlGetFileName(dbFile).Text]);
  umlRenameFile(tmpFile, dbFile);
  DoStatus('done.');
end;

class function TAI.PrepareOCRLanguageModelToCustomPath(DBLangPath_, DBLangFile_, DestPath_: SystemString; ExtractConfigure_: Boolean): Boolean;
var
  DBLangPath, DBLangFile: U_String;
  dbFile: U_String;
  dbEng: TObjectDataManager;
  ocrModelPh: U_String;
  tmpFile: U_String;
  i: Integer;
  fs: TCoreClassFileStream;
  md5Info: THashTextEngine;
  m64: TMemoryStream64;
  fieldPos: Int64;
  md5Key, md5Value: U_String;
begin
  Result := False;
  if not FileExistsFromConfigure(C_OCR_Model_Package) then
    begin
      DoStatus('No exists OCR model package ''%s', [C_OCR_Model_Package]);
      exit;
    end;

  dbFile := WhereFileFromConfigure(C_OCR_Model_Package);
  dbEng := TObjectDataManagerOfCache.Open(dbFile, DBMarshal.ID, True);

  ocrModelPh := DestPath_;
  umlCreateDirectory(ocrModelPh);

  DBLangPath := umlTrimSpace(DBLangPath_);
  if DBLangPath.L = 0 then
      DBLangPath := '/avg';

  DBLangFile := DBLangFile_;

  if not umlMultipleMatch(True, '*.TrainedData', DBLangFile) then
      DBLangFile := umlChangeFileExt(DBLangFile, '.TrainedData');

  if ExtractConfigure_ then
    begin
      if not dbEng.FieldExists(DBLangPath, 'tessconfigs') then
        begin
          if dbEng.FieldExists(umlCombineUnixPath(umlGetFirstStr(DBLangPath, '/\'), 'tessconfigs')) then
              dbEng.ExpPathToDisk(umlCombineUnixPath(umlGetFirstStr(DBLangPath, '/\'), 'tessconfigs'), umlCombinePath(ocrModelPh, 'tessconfigs'), True);
        end
      else
          dbEng.ExpPathToDisk(umlCombineUnixPath(DBLangPath, 'tessconfigs'), umlCombinePath(ocrModelPh, 'tessconfigs'), True);
    end;

  tmpFile := umlCombineFileName(ocrModelPh, DBLangFile);

  if umlFileExists(tmpFile) then
    begin
      md5Info := THashTextEngine.Create(1024, 1024);
      m64 := TMemoryStream64.Create;
      dbEng.ItemReadToStream('/', 'md5.txt', m64);
      m64.Position := 0;
      md5Info.LoadFromStream(m64);
      DisposeObject(m64);

      if not dbEng.GetPathField(DBLangPath, fieldPos) then
        begin
          DisposeObject(dbEng);
          DisposeObject(md5Info);
          exit;
        end;

      md5Key := umlCombineUnixFileName(dbEng.GetFieldPath(fieldPos), DBLangFile);
      if (md5Key.L > 0) and (md5Key.First <> '/') then
          md5Key := '/' + md5Key;
      md5Value := md5Info.GetDefaultText('FileKey', md5Key, '');
      DisposeObject(md5Info);

      if not md5Value.Same(umlMD5ToString(umlFileMD5(tmpFile))) then
        begin
          if dbEng.ItemExists(DBLangPath, DBLangFile) then
            begin
              { overwrite language model }
              try
                fs := TCoreClassFileStream.Create(tmpFile, fmCreate);
                dbEng.ItemReadToStream(DBLangPath, DBLangFile, fs);
                DisposeObject(fs);
                Result := True;
              except
              end;
            end
          else
              DoStatus('no exists language model %s', [umlCombineUnixFileName(DBLangPath, DBLangFile).Text]);
        end
      else
          Result := True;
    end
  else if dbEng.ItemExists(DBLangPath, DBLangFile) then
    begin
      { export language model }
      fs := TCoreClassFileStream.Create(tmpFile, fmCreate);
      dbEng.ItemReadToStream(DBLangPath, DBLangFile, fs);
      DisposeObject(fs);
      Result := True;
    end
  else
    begin
      DoStatus('no exists language model %s', [umlCombineUnixFileName(DBLangPath, DBLangFile).Text]);
    end;
  DisposeObject(dbEng);
end;

class function TAI.PrepareOCRLanguageModelToCustomPath(DBLangPath_, DestPath_: SystemString): Boolean;
var
  DBLangPath: U_String;
  dbFile: U_String;
  dbEng: TObjectDataManager;
  ocrModelPh: U_String;
begin
  Result := False;
  if not FileExistsFromConfigure(C_OCR_Model_Package) then
    begin
      DoStatus('No exists OCR model package ''%s', [C_OCR_Model_Package]);
      exit;
    end;

  dbFile := WhereFileFromConfigure(C_OCR_Model_Package);
  dbEng := TObjectDataManagerOfCache.Open(dbFile, DBMarshal.ID, True);

  ocrModelPh := DestPath_;
  umlCreateDirectory(ocrModelPh);

  DBLangPath := umlTrimSpace(DBLangPath_);
  if DBLangPath.L = 0 then
      DBLangPath := '/avg';

  dbEng.ExpPathToDisk(DBLangPath, ocrModelPh, True);
  Result := True;
end;

class function TAI.PrepareOCRLanguageModel(DBLangPath_, DBLangFile_: SystemString): Boolean;
begin
  Result := PrepareOCRLanguageModelToCustomPath(DBLangPath_, DBLangFile_, umlCombinePath(AI_Work_Path, 'OCRModel'), True);
end;

class function TAI.PrepareOCRLanguageModel(DBLangPath_: SystemString): Boolean;
begin
  Result := PrepareOCRLanguageModelToCustomPath(DBLangPath_, umlCombinePath(AI_Work_Path, 'OCRModel'));
end;

class procedure TAI.CleanOCRLanguageModel;
var
  ocrModelPh: U_String;
begin
  ocrModelPh := umlCombinePath(AI_Work_Path, 'OCRModel');
  umlDeleteFile(umlCombineFileName(ocrModelPh, '*.*'));
end;

class function TAI.PrepareOCRFastLanguageModel: Boolean;
begin
  Result := PrepareOCRLanguageModel('/fast');
end;

class function TAI.PrepareOCRBestLanguageModel: Boolean;
begin
  Result := PrepareOCRLanguageModel('/best');
end;

class function TAI.PrepareOCRDefaultLanguageModel: Boolean;
begin
  Result := PrepareOCRLanguageModel('/avg');
end;

class function TAI.PrepareOCRLanguageModel: Boolean;
begin
  Result := PrepareOCRLanguageModel('/avg');
end;

function TAI.OpenOCREngine(ocrData, ocrLang: SystemString): TOCR_Handle;
var
  ocrDataP, ocrLangP: P_Bytes;
begin
  Result := nil;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.OpenOCREngine) then
      exit;
  ocrDataP := Alloc_P_Bytes(ocrData);
  ocrLangP := Alloc_P_Bytes(ocrLang);
  try
      Result := FAI_EntryAPI^.OpenOCREngine(ocrDataP, ocrLangP);
  except
      Result := nil;
  end;
  Free_P_Bytes(ocrDataP);
  Free_P_Bytes(ocrLangP);
end;

function TAI.OpenOCREngine(ocrLang: SystemString): TOCR_Handle;
var
  p: U_String;
begin
  p := umlCombinePath(AI_Work_Path, 'OCRModel');
  p.DeleteLast;
  Result := OpenOCREngine(p, ocrLang);
end;

procedure TAI.SetOCRDPI(hnd: TOCR_Handle; v_: Integer);
begin
  SetOCRParameter(hnd, 'user_defined_dpi', umlIntToStr(v_));
end;

procedure TAI.SetOCRWhiteChar(hnd: TOCR_Handle; v_: TPascalString);
begin
  SetOCRParameter(hnd, 'tessedit_char_whitelist', v_);
end;

procedure TAI.CloseOCREngine(var hnd: TOCR_Handle);
begin
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.CloseOCREngine) then
      exit;
  FAI_EntryAPI^.CloseOCREngine(hnd);
  hnd := nil;
end;

procedure TAI.SetOCRParameter(hnd: TOCR_Handle; ocrKey, ocrValue: U_String);
var
  ocrKeyP, ocrValueP: P_Bytes;
begin
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.SetOCRParameter) then
      exit;
  ocrKeyP := Alloc_P_Bytes(ocrKey);
  ocrValueP := Alloc_P_Bytes(ocrValue);
  FAI_EntryAPI^.SetOCRParameter(hnd, ocrKeyP, ocrValueP);
  Free_P_Bytes(ocrKeyP);
  Free_P_Bytes(ocrValueP);
end;

procedure TAI.PrintOCRParameter(hnd: TOCR_Handle);
begin
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.PrintOCRParameter) then
      exit;
  FAI_EntryAPI^.PrintOCRParameter(hnd);
end;

function TAI.ProcessOCR(hnd: TOCR_Handle; Raster: TMemoryRaster; mode: Integer): Boolean;
var
  tmp: TRaster;
  m64: TMemoryStream64;
begin
  Result := False;
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.ProcessOCR) then
      exit;
  m64 := TMemoryStream64.Create;
  tmp := Raster.FitScaleAsNew(4096, 4096);
  tmp.SaveToBmp32Stream(m64);
  DisposeObject(tmp);
  try
      Result := FAI_EntryAPI^.ProcessOCR(hnd, m64.memory, m64.Size, mode) = 0;
  except
      Result := False;
  end;
  DisposeObject(m64);
end;

function TAI.ProcessOCR(hnd: TOCR_Handle; Raster: TMemoryRaster): Boolean;
begin
  Result := ProcessOCR(hnd, Raster, 3);
end;

function TAI.GetOCR_Text(hnd: TOCR_Handle): U_String;
var
  p: Pointer;
begin
  Result := '';
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.GetOCR_ResultText) then
      exit;
  try
      p := FAI_EntryAPI^.GetOCR_ResultText(hnd);
  except
      exit;
  end;
  Result := PPascalString(p)^;
  Result.DeleteChar(#13);
  Result := umlStringReplace(Result, #10, #13#10, False);
  API_FreeString(p);
end;

function TAI.GetOCR_HTML(hnd: TOCR_Handle): U_String;
var
  p: Pointer;
begin
  Result := '';
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.GetOCR_ResultHTML) then
      exit;
  try
      p := FAI_EntryAPI^.GetOCR_ResultHTML(hnd);
  except
      exit;
  end;
  Result := PPascalString(p)^;
  Result := umlStringReplace(Result, #10, #13#10, False);
  API_FreeString(p);
end;

function TAI.GetOCR_XML(hnd: TOCR_Handle): U_String;
var
  p: Pointer;
begin
  Result := '';
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.GetOCR_ResultXML) then
      exit;
  try
      p := FAI_EntryAPI^.GetOCR_ResultXML(hnd);
  except
      exit;
  end;
  Result := PPascalString(p)^;
  Result := umlStringReplace(Result, #10, #13#10, False);
  API_FreeString(p);
end;

function TAI.GetOCR_TSV(hnd: TOCR_Handle): U_String;
var
  p: Pointer;
begin
  Result := '';
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.GetOCR_ResultTSV) then
      exit;
  try
      p := FAI_EntryAPI^.GetOCR_ResultTSV(hnd);
  except
      exit;
  end;
  Result := PPascalString(p)^;
  Result := umlStringReplace(Result, #10, #13#10, False);
  API_FreeString(p);
end;

function TAI.GetOCR_LSTMBox(hnd: TOCR_Handle): U_String;
var
  p: Pointer;
begin
  Result := '';
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.GetOCR_ResultLSTMBoxText) then
      exit;
  try
      p := FAI_EntryAPI^.GetOCR_ResultLSTMBoxText(hnd);
  except
      exit;
  end;
  Result := PPascalString(p)^;
  Result := umlStringReplace(Result, #10, #13#10, False);
  API_FreeString(p);
end;

function TAI.GetOCR_Box(hnd: TOCR_Handle): U_String;
var
  p: Pointer;
begin
  Result := '';
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.GetOCR_ResultBoxText) then
      exit;
  try
      p := FAI_EntryAPI^.GetOCR_ResultBoxText(hnd);
  except
      exit;
  end;
  Result := PPascalString(p)^;
  Result := umlStringReplace(Result, #10, #13#10, False);
  API_FreeString(p);
end;

function TAI.GetOCR_WordStrBox(hnd: TOCR_Handle): U_String;
var
  p: Pointer;
begin
  Result := '';
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.GetOCR_ResultWordStrBoxText) then
      exit;
  try
      p := FAI_EntryAPI^.GetOCR_ResultWordStrBoxText(hnd);
  except
      exit;
  end;
  Result := PPascalString(p)^;
  Result := umlStringReplace(Result, #10, #13#10, False);
  API_FreeString(p);
end;

function TAI.GetOCR_OSD(hnd: TOCR_Handle): U_String;
var
  p: Pointer;
begin
  Result := '';
  if hnd = nil then
      exit;
  if (FAI_EntryAPI = nil) then
      exit;
  if not Assigned(FAI_EntryAPI^.GetOCR_ResultOSDText) then
      exit;
  try
      p := FAI_EntryAPI^.GetOCR_ResultOSDText(hnd);
  except
      exit;
  end;
  Result := PPascalString(p)^;
  Result := umlStringReplace(Result, #10, #13#10, False);
  API_FreeString(p);
end;

class function TAI.Init_ZMetric_Parameter(train_sync_file, train_output: U_String): PZMetric_Train_Parameter;
begin
  new(Result);
  FillPtrByte(Result, SizeOf(TZMetric_Train_Parameter), 0);

  Result^.imgArry_ptr := nil;
  Result^.img_num := 0;
  Result^.train_sync_file := Alloc_P_Bytes(train_sync_file);
  Result^.train_output := Alloc_P_Bytes(train_output);

  Result^.timeout := C_Tick_Hour;
  Result^.weight_decay := 0.0001;
  Result^.momentum := 0.9;
  Result^.iterations_without_progress_threshold := 500;
  Result^.learning_rate := 0.1;
  Result^.completed_learning_rate := 0.0001;
  Result^.step_mini_batch_target_num := 5;
  Result^.step_mini_batch_raster_num := 5;

  Result^.control := nil;
  Result^.training_average_loss := 0;
  Result^.training_learning_rate := 0;
end;

class procedure TAI.Free_ZMetric_Parameter(param: PZMetric_Train_Parameter);
begin
  Free_P_Bytes(param^.train_sync_file);
  Free_P_Bytes(param^.train_output);
  Dispose(param);
end;

function TAI.ZMetric_Train(LargeScale_: Boolean; RSeri: TRasterSerialized; imgList: TMemoryRaster2DArray; param: PZMetric_Train_Parameter): Boolean;
var
  i, j, imgSum, ri: Integer;
  imgArry: TMemoryRasterArray;
  rArry: array of TAI_Raster_Data;
begin
  Result := False;

  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.ZMetric_Full_GPU_Train) then
      exit;

  imgSum := 0;
  for i := 0 to Length(imgList) - 1 do
      inc(imgSum, Length(imgList[i]));

  if imgSum = 0 then
      exit;

  { process sequence }
  SetLength(rArry, imgSum);
  ri := 0;
  for i := 0 to Length(imgList) - 1 do
    begin
      imgArry := imgList[i];
      for j := 0 to Length(imgArry) - 1 do
        begin
          new(rArry[ri].raster_Hnd);
          rArry[ri].raster_Hnd^.Raster := imgArry[j];
          if LargeScale_ then
            begin
              rArry[ri].raster_ptr := nil;
              imgArry[j].SerializedAndRecycleMemory(RSeri);
            end
          else
              rArry[ri].raster_ptr := imgArry[j].Bits;

          rArry[ri].Width := imgArry[j].Width;
          rArry[ri].Height := imgArry[j].Height;
          rArry[ri].index := i;
          inc(ri);
        end;
    end;

  { set arry }
  param^.imgArry_ptr := PAI_Raster_Data_Array(@rArry[0]);
  param^.img_num := Length(rArry);
  param^.control := @TrainingControl;

  { execute training }
  TrainingControl.pause := 0;
  TrainingControl.stop := 0;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := True;
      FAI_EntryAPI^.RasterSerialized := RSeri;
      RSeri.EnabledReadHistory := True;
    end
  else
      FAI_EntryAPI^.RasterSerialized := nil;

  FAI_EntryAPI^.SerializedTime := GetTimeTick();

  { run training }
  try
      Result := FAI_EntryAPI^.ZMetric_Full_GPU_Train(param) >= 0
  except
      Result := False;
  end;

  if LargeScale_ then
    begin
      RSeri.ClearHistory;
      RSeri.EnabledReadHistory := False;
      FAI_EntryAPI^.RasterSerialized := nil;
    end;

  Last_training_average_loss := param^.training_average_loss;
  Last_training_learning_rate := param^.training_learning_rate;
  completed_learning_rate := param^.completed_learning_rate;

  { reset arry }
  param^.imgArry_ptr := nil;
  param^.img_num := 0;

  { free }
  for i := 0 to Length(rArry) - 1 do
      Dispose(rArry[i].raster_Hnd);
  SetLength(rArry, 0);
end;

function TAI.ZMetric_Train(Snapshot_: Boolean; imgList: TAI_ImageList; SS_Width, SS_Height: Integer; param: PZMetric_Train_Parameter): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.ZMetric_Full_GPU_Train) then
      exit;

  if Snapshot_ then
    begin
      imgList.CalibrationNoDetectorDefine('');
      imgBuff := imgList.ExtractDetectorDefineAsSnapshotProjection(SS_Width, SS_Height);
    end
  else
    begin
      imgBuff := imgList.ExtractDetectorDefineAsPrepareRaster(SS_Width, SS_Height);
    end;

  if Length(imgBuff) = 0 then
      exit;

  Result := ZMetric_Train(False, nil, imgBuff, param);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.ZMetric_Train_Stream(Snapshot_: Boolean; imgList: TAI_ImageList; SS_Width, SS_Height: Integer; param: PZMetric_Train_Parameter): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if ZMetric_Train(Snapshot_, imgList, SS_Width, SS_Height, param) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.ZMetric_Train(Snapshot_: Boolean; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; param: PZMetric_Train_Parameter): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  imgL: TAI_ImageList;
  detDef: TAI_DetectorDefine;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.ZMetric_Full_GPU_Train) then
      exit;

  if Snapshot_ then
    begin
      DoStatus('Calibration Z-Metric dataset.');
      for i := 0 to imgMat.Count - 1 do
        begin
          imgL := imgMat[i];
          imgL.CalibrationNoDetectorDefine(imgL.FileInfo);
          imgL.CalibrationNullToken(imgL.FileInfo);
          for j := 0 to imgL.Count - 1 do
            if imgL[j].DetectorDefineList.Count = 0 then
              begin
                detDef := TAI_DetectorDefine.Create(imgL[j]);
                detDef.R := imgL[j].Raster.BoundsRect;
                detDef.Token := imgL.FileInfo;
                imgL[j].DetectorDefineList.Add(detDef);
              end;
        end;
      imgBuff := imgMat.ExtractDetectorDefineAsSnapshotProjection(SS_Width, SS_Height);
    end
  else
      imgBuff := imgMat.ExtractDetectorDefineAsPrepareRaster(SS_Width, SS_Height);

  if Length(imgBuff) = 0 then
      exit;

  Result := ZMetric_Train(False, nil, imgBuff, param);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.ZMetric_Train_Stream(Snapshot_: Boolean; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; param: PZMetric_Train_Parameter): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if ZMetric_Train(Snapshot_, imgMat, SS_Width, SS_Height, param) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

function TAI.ZMetric_Train(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; param: PZMetric_Train_Parameter): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  imgL: TAI_ImageList;
  detDef: TAI_DetectorDefine;
begin
  Result := False;
  if FAI_EntryAPI = nil then
      exit;
  if not Assigned(FAI_EntryAPI^.ZMetric_Full_GPU_Train) then
      exit;

  if Snapshot_ then
    begin
      DoStatus('Calibration Z-Metric dataset.');
      for i := 0 to imgMat.Count - 1 do
        begin
          imgL := imgMat[i];
          imgL.CalibrationNoDetectorDefine(imgL.FileInfo);
          imgL.CalibrationNullToken(imgL.FileInfo);
          for j := 0 to imgL.Count - 1 do
            if imgL[j].DetectorDefineList.Count = 0 then
              begin
                detDef := TAI_DetectorDefine.Create(imgL[j]);
                detDef.R := imgL[j].Raster.BoundsRect;
                detDef.Token := imgL.FileInfo;
                imgL[j].DetectorDefineList.Add(detDef);
              end;
        end;

      if LargeScale_ then
          imgBuff := imgMat.LargeScale_ExtractDetectorDefineAsSnapshotProjection(RSeri, SS_Width, SS_Height)
      else
          imgBuff := imgMat.ExtractDetectorDefineAsSnapshotProjection(SS_Width, SS_Height);
    end
  else
    begin
      if LargeScale_ then
          imgBuff := imgMat.LargeScale_ExtractDetectorDefineAsPrepareRaster(RSeri, SS_Width, SS_Height)
      else
          imgBuff := imgMat.ExtractDetectorDefineAsPrepareRaster(SS_Width, SS_Height);
    end;

  if Length(imgBuff) = 0 then
      exit;

  Result := ZMetric_Train(LargeScale_, RSeri, imgBuff, param);

  for i := 0 to Length(imgBuff) - 1 do
    for j := 0 to Length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.ZMetric_Train_Stream(Snapshot_, LargeScale_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; param: PZMetric_Train_Parameter): TMemoryStream64;
var
  fn: U_String;
begin
  Result := nil;

  if ZMetric_Train(Snapshot_, LargeScale_, RSeri, imgMat, SS_Width, SS_Height, param) then
    begin
      fn := Get_P_Bytes_String(param^.train_output);
      if umlFileExists(fn) then
        begin
          Result := TMemoryStream64.Create;
          Result.LoadFromFile(fn);
          Result.Position := 0;
        end;
    end;
end;

class function TAI.Build_ZMetric_Learn: TLearn;
var
  L: TLearn;
begin
  L := TLearn.CreateClassifier(ltKDT, zAI.C_ZMetric_Dim);
  Result := L;
end;

function TAI.ZMetric_Open(train_file: SystemString): TZMetric_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.ZMetric_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      Result := FAI_EntryAPI^.ZMetric_Init(train_file_buff);
      Free_P_Bytes(train_file_buff);
      if Result <> nil then
          DoStatus('Z-Metric open: %s', [train_file]);
    end
  else
      Result := nil;
end;

function TAI.ZMetric_Open_Stream(stream: TMemoryStream64): TZMetric_Handle;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.ZMetric_Init_Memory) then
    begin
      Result := FAI_EntryAPI^.ZMetric_Init_Memory(stream.memory, stream.Size);
      if Result <> nil then
          DoStatus('Z-Metric open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.ZMetric_Open_Stream(train_file: SystemString): TZMetric_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := ZMetric_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('Z-Metric open: %s', [train_file]);
end;

function TAI.ZMetric_Close(var hnd: TZMetric_Handle): Boolean;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.ZMetric_Free) and (hnd <> nil) then
    begin
      Result := FAI_EntryAPI^.ZMetric_Free(hnd) = 0;
      DoStatus('Z-Metric close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

function TAI.ZMetric_Process(hnd: TZMetric_Handle; RasterArray: TMemoryRasterArray; SS_Width, SS_Height: Integer; output: PDouble): Integer;
var
  rArry: array of TAI_Raster_Data;
  i: Integer;
  nr: TMemoryRaster;
begin
  if (FAI_EntryAPI <> nil) and Assigned(FAI_EntryAPI^.ZMetric_Process) then
    begin
      SetLength(rArry, Length(RasterArray));
      for i := 0 to Length(RasterArray) - 1 do
        begin
          new(rArry[i].raster_Hnd);

          nr := NewRaster();

          { projection }
          if (RasterArray[i].Width <> SS_Width) or (RasterArray[i].Height <> SS_Height) then
            begin
              nr.SetSize(SS_Width, SS_Height);
              RasterArray[i].ProjectionTo(nr,
                TV2Rect4.Init(RectFit(SS_Width, SS_Height, RasterArray[i].BoundsRectV2), 0),
                TV2Rect4.Init(nr.BoundsRectV2, 0),
                True, 1.0);
            end
          else { fast assign }
              nr.SetWorkMemory(RasterArray[i]);

          rArry[i].raster_Hnd^.Raster := nr;

          rArry[i].raster_ptr := nr.Bits;
          rArry[i].Width := nr.Width;
          rArry[i].Height := nr.Height;
          rArry[i].index := i;
        end;

      FAI_EntryAPI^.RasterSerialized := nil;
      FAI_EntryAPI^.SerializedTime := GetTimeTick();

      try
          Result := FAI_EntryAPI^.ZMetric_Process(hnd, PAI_Raster_Data_Array(@rArry[0]), Length(rArry), output);
      except
      end;

      for i := 0 to Length(rArry) - 1 do
        begin
          DisposeObject(rArry[i].raster_Hnd^.Raster);
          Dispose(rArry[i].raster_Hnd);
        end;
      SetLength(rArry, 0);
    end
  else
      Result := -2;
end;

function TAI.ZMetric_Process(hnd: TZMetric_Handle; RasterArray: TMemoryRasterArray; SS_Width, SS_Height: Integer): TLMatrix;
var
  L: TLVec;
  i: TLInt;
begin
  Result := LMatrix(0, 0);
  if Length(RasterArray) > 0 then
    begin
      SetLength(L, Length(RasterArray) * C_ZMetric_Dim);
      if ZMetric_Process(hnd, RasterArray, SS_Width, SS_Height, @L[0]) > 0 then
        begin
          Result := LMatrix(Length(RasterArray), 0);
          for i := Low(Result) to high(Result) do
              Result[i] := LVecCopy(L, i * C_ZMetric_Dim, C_ZMetric_Dim);
        end;
      SetLength(L, 0);
    end;
end;

function TAI.ZMetric_Process(hnd: TZMetric_Handle; Raster: TMemoryRaster; SS_Width, SS_Height: Integer): TLVec;
var
  rArry: TMemoryRasterArray;
begin
  Raster.ReadyBits();
  SetLength(Result, C_ZMetric_Dim);
  SetLength(rArry, 1);
  rArry[0] := Raster;
  if ZMetric_Process(hnd, rArry, SS_Width, SS_Height, @Result[0]) <= 0 then
      SetLength(Result, 0);
end;

procedure TAI.ZMetric_SaveToLearnEngine_DT(pool_: TAI_DNN_ThreadPool; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; SS_Width, SS_Height: Integer; lr: TLearn);
var
  i, j: Integer;
  imgData: TAI_Image;
  detDef: TAI_DetectorDefine;
  p: PZMetric_SaveToLearnEngine_DT_UserData_;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      imgData := imgList[i];
      if RSeri <> nil then
          imgData.UnserializedMemory(RSeri);
      if Snapshot_ then
        begin
          new(p);
          p^.lr := lr;
          p^.Snapshot := True;
          p^.imgData := imgData;
          p^.detDef := nil;
          TAI_DNN_Thread_ZMetric(pool_.MinLoad_DNN_Thread).ProcessC(p, imgData.Raster.Clone, SS_Width, SS_Height, True, {$IFDEF FPC}@{$ENDIF FPC}ZMetric_SaveToLearnEngine_DT_Backcall);
        end
      else
        for j := 0 to imgData.DetectorDefineList.Count - 1 do
          begin
            detDef := imgData.DetectorDefineList[j];
            if detDef.Token.Len > 0 then
              begin
                new(p);
                p^.lr := lr;
                p^.Snapshot := False;
                p^.imgData := nil;
                p^.detDef := detDef;

                if detDef.PrepareRaster.Empty then
                    TAI_DNN_Thread_ZMetric(pool_.MinLoad_DNN_Thread).ProcessC(p,
                    detDef.Owner.Raster.BuildAreaOffsetScaleSpace(detDef.R, C_Metric_Input_Size, C_Metric_Input_Size),
                    SS_Width, SS_Height, True, {$IFDEF FPC}@{$ENDIF FPC}ZMetric_SaveToLearnEngine_DT_Backcall)
                else
                    TAI_DNN_Thread_ZMetric(pool_.MinLoad_DNN_Thread).ProcessC(p,
                    detDef.PrepareRaster.Clone,
                    SS_Width, SS_Height, True, {$IFDEF FPC}@{$ENDIF FPC}ZMetric_SaveToLearnEngine_DT_Backcall);
              end;
          end;
      if RSeri <> nil then
          imgData.SerializedAndRecycleMemory(RSeri);
    end;
end;

procedure TAI.ZMetric_SaveToLearnEngine_DT(ThNum: Integer; ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; SS_Width, SS_Height: Integer; lr: TLearn);
var
  pool_: TAI_DNN_ThreadPool;
  i: Integer;
  Device_: TLIVec;
begin
  if lr.InSize <> C_ZMetric_Dim then
      RaiseInfo('Learn Engine Insize illegal');
  pool_ := TAI_DNN_ThreadPool.Create;

  GetComputeDeviceOfTraining(Device_);
  for i in Device_ do
      pool_.BuildDeviceThread(FAI_EntryAPI, i, ThNum, TAI_DNN_Thread_ZMetric);
  for i := 0 to pool_.Count - 1 do
      TAI_DNN_Thread_ZMetric(pool_[i]).Open_Stream(ZMetric_stream);

  ZMetric_SaveToLearnEngine_DT(pool_, Snapshot_, RSeri, imgList, SS_Width, SS_Height, lr);
  pool_.Wait();
  DisposeObject(pool_);
end;

procedure TAI.ZMetric_SaveToLearnEngine_DT(ThNum: Integer; ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; lr: TLearn);
var
  pool_: TAI_DNN_ThreadPool;
  i: Integer;
  Device_: TLIVec;
begin
  if lr.InSize <> C_ZMetric_Dim then
      RaiseInfo('Learn Engine Insize illegal');
  pool_ := TAI_DNN_ThreadPool.Create;

  GetComputeDeviceOfTraining(Device_);
  for i in Device_ do
      pool_.BuildDeviceThread(FAI_EntryAPI, i, ThNum, TAI_DNN_Thread_ZMetric);
  for i := 0 to pool_.Count - 1 do
      TAI_DNN_Thread_ZMetric(pool_[i]).Open_Stream(ZMetric_stream);

  for i := 0 to imgMat.Count - 1 do
      ZMetric_SaveToLearnEngine_DT(pool_, Snapshot_, RSeri, imgMat[i], SS_Width, SS_Height, lr);
  pool_.Wait();
  DisposeObject(pool_);
end;

procedure TAI.ZMetric_SaveToLearnEngine_DT(ThNum: Integer; ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; imgList: TAI_ImageList; SS_Width, SS_Height: Integer; lr: TLearn);
begin
  ZMetric_SaveToLearnEngine_DT(ThNum, ZMetric_stream, Snapshot_, nil, imgList, SS_Width, SS_Height, lr);
end;

procedure TAI.ZMetric_SaveToLearnEngine_DT(ThNum: Integer; ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; lr: TLearn);
begin
  ZMetric_SaveToLearnEngine_DT(ThNum, ZMetric_stream, Snapshot_, nil, imgMat, SS_Width, SS_Height, lr);
end;

procedure TAI.ZMetric_SaveToLearnEngine_DT(ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgList: TAI_ImageList; SS_Width, SS_Height: Integer; lr: TLearn);
begin
  ZMetric_SaveToLearnEngine_DT(2, ZMetric_stream, Snapshot_, RSeri, imgList, SS_Width, SS_Height, lr);
end;

procedure TAI.ZMetric_SaveToLearnEngine_DT(ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; RSeri: TRasterSerialized; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; lr: TLearn);
begin
  ZMetric_SaveToLearnEngine_DT(2, ZMetric_stream, Snapshot_, RSeri, imgMat, SS_Width, SS_Height, lr);
end;

procedure TAI.ZMetric_SaveToLearnEngine_DT(ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; imgList: TAI_ImageList; SS_Width, SS_Height: Integer; lr: TLearn);
begin
  ZMetric_SaveToLearnEngine_DT(ZMetric_stream, Snapshot_, nil, imgList, SS_Width, SS_Height, lr);
end;

procedure TAI.ZMetric_SaveToLearnEngine_DT(ZMetric_stream: TMemoryStream64; Snapshot_: Boolean; imgMat: TAI_ImageMatrix; SS_Width, SS_Height: Integer; lr: TLearn);
begin
  ZMetric_SaveToLearnEngine_DT(ZMetric_stream, Snapshot_, nil, imgMat, SS_Width, SS_Height, lr);
end;

procedure TSS_ResultProcessor.DoFailed;
begin
  if Assigned(OnResultC) then
      OnResultC(False, nil, nil, nil);
  if Assigned(OnResultM) then
      OnResultM(False, nil, nil, nil);
  if Assigned(OnResultP) then
      OnResultP(False, nil, nil, nil);
end;

procedure TSS_ResultProcessor.DoSuccessed;
begin
  if Assigned(OnResultC) then
      OnResultC(True, SSInput, SSOutput, SSTokenOutput);
  if Assigned(OnResultM) then
      OnResultM(True, SSInput, SSOutput, SSTokenOutput);
  if Assigned(OnResultP) then
      OnResultP(True, SSInput, SSOutput, SSTokenOutput);
end;

procedure TSS_ResultProcessor.ThRun(ThSender: TCompute);
var
  TokenHash: THashList;

{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: Integer);
  var
    i: Integer;
    p: ^WORD;
    tk: U_String;
  begin
    p := @SSMatrix[pass * SSOutput.Width];
    for i := 0 to SSOutput.Width - 1 do
      begin
        if colorPool = nil then
            SSOutput.Pixel[i, pass] := TAI.SS_TranslateColor(p^).BGRA
        else if colorPool.GetIDColorAndToken(p^, RColor(0, 0, 0, $FF), '', SSOutput.PixelPtr[i, pass]^, tk) then
          begin
            if not TokenHash.Exists(tk) then
              begin
                LockObject(TokenHash);
                TokenHash.Add(tk, nil, False);
                UnLockObject(TokenHash);
              end;
          end;
        inc(p);
      end;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure DoFor;
  var
    pass, i: Integer;
    p: ^WORD;
    tk: U_String;
  begin
    for pass := 0 to SSOutput.Height - 1 do
      begin
        p := @SSMatrix[pass * SSOutput.Width];
        for i := 0 to SSOutput.Width - 1 do
          begin
            if colorPool = nil then
                SSOutput.Pixel[i, pass] := TAI.SS_TranslateColor(p^).BGRA
            else if colorPool.GetIDColorAndToken(p^, RColor(0, 0, 0, $FF), '', SSOutput.PixelPtr[i, pass]^, tk) then
              begin
                if not TokenHash.Exists(tk) then
                  begin
                    LockObject(TokenHash);
                    TokenHash.Add(tk, nil, False);
                    UnLockObject(TokenHash);
                  end;
              end;
            inc(p);
          end;
      end;
  end;
{$ENDIF Parallel}


begin
  SSOutput := NewRaster();
  SSOutput.SetSize(SSInput.Width, SSInput.Height);
  TokenHash := THashList.CustomCreate($FFFF);
  TokenHash.AccessOptimization := False;
  SSTokenOutput := TPascalStringList.Create;

  { fill output }
{$IFDEF Parallel}
{$IFDEF FPC}
  FPCParallelFor(@Nested_ParallelFor, 0, SSOutput.Height - 1);
{$ELSE FPC}
  DelphiParallelFor(0, SSOutput.Height - 1, procedure(pass: Integer)
    var
      i: Integer;
      p: ^WORD;
      tk: U_String;
    begin
      p := @SSMatrix[pass * SSOutput.Width];
      for i := 0 to SSOutput.Width - 1 do
        begin
          if colorPool = nil then
              SSOutput.Pixel[i, pass] := TAI.SS_TranslateColor(p^).BGRA
          else if colorPool.GetIDColorAndToken(p^, RColor(0, 0, 0, $FF), '', SSOutput.PixelPtr[i, pass]^, tk) then
            begin
              if not TokenHash.Exists(tk) then
                begin
                  LockObject(TokenHash);
                  TokenHash.Add(tk, nil, False);
                  UnLockObject(TokenHash);
                end;
            end;
          inc(p);
        end;
    end);
{$ENDIF FPC}
{$ELSE Parallel}
  DoFor;
{$ENDIF Parallel}
  TokenHash.GetNameList(SSTokenOutput);

  { trigger event }
  DoSuccessed();

  DisposeObject(TokenHash);
  DisposeObject(SSInput);
  DisposeObject(SSOutput);
  DisposeObject(SSTokenOutput);
  SetLength(SSMatrix, 0);
  DisposeObject(Self);
end;

constructor TSS_ResultProcessor.Create;
begin
  inherited Create;
  SetLength(SSMatrix, 0);
  colorPool := nil;

  SSInput := nil;
  SSOutput := nil;
  SSTokenOutput := nil;

  OnResultC := nil;
  OnResultM := nil;
  OnResultP := nil;
end;

class function TAI_DNN_Thread_Trigger.Init(p_: Pointer; Ev_: TRunWithThreadMethod): TAI_DNN_Thread_Trigger;
begin
  Result.p := p_;
  Result.Ev := Ev_;
end;

procedure TAI_DNN_Thread.Run_DNN_Thread(Sender: TCompute);
var
  tk: TTimeTick;
  R, L: Integer;
begin
  FThread := Sender;
  FThreadPost.ThreadID := FThread.ThreadID;

  FAI.SetComputeDeviceOfProcess(FDevice);

  if FAI.isGPU then
      FThreadInfo := PFormat('%s GPU[%d] %s thread:%d', [
      if_(FAI.GetComputeDeviceOfProcess = FDevice, 'OK', 'Error'),
      FAI.GetComputeDeviceOfProcess,
      FAI.GetComputeDeviceNameOfProcess(FAI.GetComputeDeviceOfProcess).Text,
      Sender.ThreadID])
  else if FAI.isMKL then
      FThreadInfo := PFormat('%s INTEL-MKL[%d] %s thread:%d', ['OK', 0, 'X86/X64', Sender.ThreadID])
  else
      FThreadInfo := PFormat('%s CPU[%d] %s thread:%d', ['OK', 0, 'X86/X64', Sender.ThreadID]);

  tk := GetTimeTick();
  R := 0;
  while FActivted.V or Busy() do
    begin
      L := FThreadPost.Progress(FThreadPost.ThreadID);
      inc(R, L);
      if GetTimeTick() - tk > 2000 then
        begin
          FPSP := R * 0.5;
          FMaxPSP := umlMax(FPSP, FMaxPSP);
          tk := GetTimeTick();
          R := 0;
        end;

      while ((FCPUThreadCritical <= 0) or (FEventThreadNum < FCPUThreadCritical)) and (FEventQueue.Num > 0) and (FEventQueue.Current <> nil) do
        begin
          AtomInc(FEventThreadNum);
          try
              TCompute.RunM(FEventQueue.Current^.data^.p, nil, FEventQueue.Current^.data^.Ev);
          except
              AtomDec(FEventThreadNum);
          end;
          FEventQueue.Next;
          inc(L, 1);
        end;

      if L <= 0 then
          TCompute.Sleep(1);
    end;

  try
      ThreadFree();
  except
  end;

  FDNNThreadRuning.V := False;
  FThread := nil;
end;

procedure TAI_DNN_Thread.ThreadFree;
begin

end;

function TAI_DNN_Thread.GetTaskNum: Integer;
begin
  Result := FThreadPost.Count;
end;

procedure TAI_DNN_Thread.UpdateLastProcessRaster(raster_: TRaster);
begin
  if not FEnabledLastProcessRaster then
      exit;
  FLastProcessRasterCritical.Lock;
  FLastProcessRaster.Assign(raster_);
  FLastProcessRaster.UserToken := Name;
  FLastProcessRaster.Update;
  FLastProcessRasterCritical.UnLock;
end;

procedure TAI_DNN_Thread.UpdateLastProcessMatrixRaster(Matrix_IMG: TMatrix_Image_Handle);
begin
  if not FEnabledLastProcessRaster then
      exit;
  FLastProcessRasterCritical.Lock;
  FAI.BuildMatrixRaster(Matrix_IMG, FLastProcessRaster);
  FLastProcessRaster.UserToken := Name;
  FLastProcessRaster.Update;
  FLastProcessRasterCritical.UnLock;
end;

procedure TAI_DNN_Thread.DoEventDone(ThSender: TCompute);
begin
  AtomDec(FEventThreadNum);
end;

procedure TAI_DNN_Thread.DoRunEvent(p: Pointer; Ev: TRunWithThreadMethod);
begin
  FEventQueue.Push(TAI_DNN_Thread_Trigger.Init(p, Ev));
end;

constructor TAI_DNN_Thread.Create;
begin
  inherited Create;
  FID := 0;
  FAI := nil;
  FThread := nil;
  FThreadPost := nil;
  FActivted := nil;
  FDNNThreadRuning := nil;

  FThreadInfo := '';
  FPSP := 0;
  FMaxPSP := 0;
  FCPUThreadCritical := 50;
  FGPUPerformanceCritical := 20;
  FName := '';

  FEventThreadNum := 0;
  FEventQueue := nil;

  FEnabledLastProcessRaster := False;
  FLastProcessRasterCritical := TCritical.Create;
  FLastProcessRaster := NewRaster();

  FCustomObject := nil;
  FCustomData := nil;
end;

destructor TAI_DNN_Thread.Destroy;
begin
  FActivted.V := False;
  while FDNNThreadRuning.V or Busy() do
      TCompute.Sleep(1);

  DisposeObjectAndNil(FEventQueue);
  DisposeObjectAndNil(FLastProcessRasterCritical);
  DisposeObjectAndNil(FLastProcessRaster);
  DisposeObjectAndNil(FActivted);
  DisposeObjectAndNil(FDNNThreadRuning);
  DisposeObjectAndNil(FThreadPost);
  DisposeObjectAndNil(FAI);
  inherited Destroy;
end;

class function TAI_DNN_Thread.Build(Owner: TAI_DNN_ThreadPool; AI_LIB_P: PAI_EntryAPI; Device_: Integer; class_: TAI_DNN_Thread_Class): TAI_DNN_Thread;
var
  TH_: TAI_DNN_Thread;
begin
  TH_ := class_.Create;
  with TH_ do
    begin
      FAI := TAI.OpenEngine(AI_LIB_P);
      FDevice := Device_;
      FThreadPost := TThreadPost.Create(0);
      FThreadPost.OneStep := True;
      FThreadPost.ResetRandomSeed := False;
      FActivted := TAtomBool.Create(True);
      FDNNThreadRuning := TAtomBool.Create(True);
      FEventQueue := TAI_DNN_Thread_Event_Trigger_Order.Create;
      TCompute.RunM(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}Run_DNN_Thread);
    end;
  Owner.FCritical.Acquire;
  TH_.FID := Owner.Count;
  Owner.Add(TH_);
  Owner.FCritical.Release;
  Result := TH_;
end;

class function TAI_DNN_Thread.Build(Owner: TAI_DNN_ThreadPool; Device_: Integer; class_: TAI_DNN_Thread_Class): TAI_DNN_Thread;
begin
  Result := TAI_DNN_Thread.Build(Owner, Prepare_AI_Engine(), Device_, class_);
end;

procedure TAI_DNN_Thread.CheckGPUPerformanceCritical;
begin
  while (FGPUPerformanceCritical > 0) and (TaskNum >= FGPUPerformanceCritical) do
      TCompute.Sleep(1);
end;

function TAI_DNN_Thread.CheckGPUPerformanceCritical(m: TTimeTick): Boolean;
var
  tk: TTimeTick;
begin
  tk := GetTimeTick();
  while (FGPUPerformanceCritical > 0) and (TaskNum >= FGPUPerformanceCritical) and (GetTimeTick - tk < m) do
      TCompute.Sleep(1);
  Result := (FGPUPerformanceCritical = 0) or (TaskNum < FGPUPerformanceCritical);
end;

function TAI_DNN_Thread.GetCPUAsyncThreadNum: Integer;
begin
  Result := FEventThreadNum;
end;

function TAI_DNN_Thread.Busy: Boolean;
begin
  Result := FThreadPost.Busy or (FEventThreadNum > 0) or (FEventQueue.Num > 0);
end;

function TAI_DNN_Thread.GetAndLockLastProcessRaster: TRaster;
begin
  FLastProcessRasterCritical.Lock;
  FLastProcessRaster.UserText := ThreadInfo;
  Result := FLastProcessRaster;
end;

procedure TAI_DNN_Thread.UnLockLastProcessRaster;
begin
  FLastProcessRasterCritical.UnLock;
end;

constructor TAI_DNN_ThreadPool.Create;
begin
  inherited Create;
  FName := '';
  FCritical := TCritical.Create;
  FNext_DNNThreadID := 0;
  FQueueOptimized := True;
  FLastRasterList := TMemoryRasterList.Create;
  FLastRasterList.AutoFreeRaster := False;
end;

destructor TAI_DNN_ThreadPool.Destroy;
begin
  Wait;
  Clear;
  DisposeObject(FLastRasterList);
  DisposeObject(FCritical);
  inherited Destroy;
end;

procedure TAI_DNN_ThreadPool.Remove(Obj: TAI_DNN_Thread);
begin
  FCritical.Acquire;
  try
    DisposeObject(Obj);
    inherited Remove(Obj);
  finally
      FCritical.Release;
  end;
end;

procedure TAI_DNN_ThreadPool.Delete(index: Integer);
begin
  FCritical.Acquire;
  try
    if (index >= 0) and (index < Count) then
      begin
        DisposeObject(Items[index]);
        inherited Delete(index);
      end;
  finally
      FCritical.Release;
  end;
end;

procedure TAI_DNN_ThreadPool.Clear;
var
  i: Integer;
begin
  FCritical.Acquire;
  FLastRasterList.Clear;
  try
    for i := 0 to Count - 1 do
        DisposeObject(Items[i]);
    inherited Clear;
  finally
      FCritical.Release;
  end;
end;

procedure TAI_DNN_ThreadPool.BuildDeviceThread(AI_LIB_P: PAI_EntryAPI; Device_, ThNum_: Integer; class_: TAI_DNN_Thread_Class);
var
  i: Integer;
begin
  for i := 0 to ThNum_ - 1 do
      TAI_DNN_Thread.Build(Self, AI_LIB_P, Device_, class_);
end;

procedure TAI_DNN_ThreadPool.BuildDeviceThread(Device_, ThNum_: Integer; class_: TAI_DNN_Thread_Class);
var
  i: Integer;
begin
  for i := 0 to ThNum_ - 1 do
      TAI_DNN_Thread.Build(Self, Device_, class_);
end;

procedure TAI_DNN_ThreadPool.BuildPerDeviceThread(AI_LIB_P: PAI_EntryAPI; Device_: TLIVec; ThNum_: Integer; class_: TAI_DNN_Thread_Class);
var
  num_: Integer;
  AI_: TAI;
  i, j: Integer;
begin
  if Length(Device_) = 0 then
    begin
      BuildPerDeviceThread(AI_LIB_P, ThNum_, class_);
      exit;
    end;
  num_ := if_(CurrentPlatform = epWin32, 1, ThNum_);
  AI_ := TAI.OpenEngine(AI_LIB_P);
  for i := 0 to num_ - 1 do
    begin
      if AI_.isGPU then
        begin
          for j in Device_ do
              BuildDeviceThread(AI_LIB_P, j, 1, class_);
        end
      else
          BuildDeviceThread(AI_LIB_P, 0, 1, class_);
    end;
  DisposeObject(AI_);
end;

procedure TAI_DNN_ThreadPool.BuildPerDeviceThread(Device_: TLIVec; ThNum_: Integer; class_: TAI_DNN_Thread_Class);
begin
  BuildPerDeviceThread(Prepare_AI_Engine(), Device_, ThNum_, class_);
end;

procedure TAI_DNN_ThreadPool.BuildPerDeviceThread(Device_: TLIVec; class_: TAI_DNN_Thread_Class);
begin
  BuildPerDeviceThread(Device_, 1, class_);
end;

procedure TAI_DNN_ThreadPool.BuildPerDeviceThread(AI_LIB_P: PAI_EntryAPI; ThNum_: Integer; class_: TAI_DNN_Thread_Class);
var
  num_: Integer;
  AI_: TAI;
  i, j: Integer;
begin
  num_ := if_(CurrentPlatform = epWin32, 1, ThNum_);
  AI_ := TAI.OpenEngine(AI_LIB_P);
  for i := 0 to num_ - 1 do
    begin
      if AI_.isGPU then
        begin
          for j := 0 to AI_.GetComputeDeviceNumOfProcess - 1 do
              BuildDeviceThread(AI_LIB_P, j, 1, class_);
        end
      else
          BuildDeviceThread(AI_LIB_P, 0, 1, class_);
    end;
  DisposeObject(AI_);
end;

procedure TAI_DNN_ThreadPool.BuildPerDeviceThread(ThNum_: Integer; class_: TAI_DNN_Thread_Class);
begin
  BuildPerDeviceThread(Prepare_AI_Engine(), ThNum_, class_);
end;

procedure TAI_DNN_ThreadPool.BuildPerDeviceThread(class_: TAI_DNN_Thread_Class);
begin
  BuildPerDeviceThread(1, class_);
end;

function TAI_DNN_ThreadPool.Next_DNN_Thread: TAI_DNN_Thread;
begin
  if Count = 0 then
      RaiseInfo('DNN FThread pool is empty.');
  FCritical.Acquire;
  try
    if FNext_DNNThreadID >= Count then
        FNext_DNNThreadID := 0;
    Result := Items[FNext_DNNThreadID];
    inc(FNext_DNNThreadID);
  finally
      FCritical.Release;
  end;
end;

function TAI_DNN_ThreadPool.MinLoad_DNN_Thread: TAI_DNN_Thread;
var
  i, id_: Integer;
  th: TAI_DNN_Thread;
begin
  if Count = 0 then
      RaiseInfo('DNN FThread pool is empty.');
  FCritical.Acquire;
  try
    for i := 0 to Count - 1 do
      if (not Items[i].Busy) then
        begin
          if (FQueueOptimized) and (i < Count - 1) then
              move(i, Count - 1);
          Result := Items[i];
          exit;
        end;

    th := Items[0];
    id_ := 0;
    for i := 1 to Count - 1 do
      if Items[i].TaskNum < th.TaskNum then
        begin
          th := Items[i];
          id_ := i;
        end;
    if (FQueueOptimized) and (id_ < Count - 1) then
        move(id_, Count - 1);
    Result := th;
  finally
      FCritical.Release;
  end;
end;

function TAI_DNN_ThreadPool.IDLE_DNN_Thread: TAI_DNN_Thread;
var
  i: Integer;
begin
  if Count = 0 then
      RaiseInfo('DNN FThread pool is empty.');
  FCritical.Acquire;
  Result := nil;
  try
    for i := 0 to Count - 1 do
      if not Items[i].Busy then
          exit(Items[i]);
  finally
      FCritical.Release;
  end;
end;

function TAI_DNN_ThreadPool.GetMinLoad_DNN_Thread_TaskNum: Integer;
var
  th: TAI_DNN_Thread;
begin
  th := MinLoad_DNN_Thread();
  if th <> nil then
      Result := th.TaskNum
  else
      Result := 0;
end;

function TAI_DNN_ThreadPool.GetTaskNum: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      inc(Result, Items[i].TaskNum);
end;

function TAI_DNN_ThreadPool.Busy: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
      Result := Result or Items[i].Busy;
end;

function TAI_DNN_ThreadPool.PSP: TGeoFloat;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      Result := Result + Items[i].PSP;
end;

function TAI_DNN_ThreadPool.MaxPSP: TGeoFloat;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      Result := Result + Items[i].MaxPSP;
end;

procedure TAI_DNN_ThreadPool.Wait;
var
  thID: TThreadID;
begin
  thID := TCoreClassThread.CurrentThread.ThreadID;
  while Busy do
    if thID = MainThreadProgress.ThreadID then
        TCompute.Sleep(1);
end;

function TAI_DNN_ThreadPool.StateInfo: U_String;
begin
  Result := StateInfo(True);
end;

function TAI_DNN_ThreadPool.StateInfo(const Separator: Boolean): U_String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
    with Items[i] do
        Result.Append('%s %s DNN(%d): %d/%d - %s Event: %d/%d/%d'#13#10,
        [if_(Name = '', ClassName, Name), ThreadInfo, ID, TaskNum, GPUPerformanceCritical, if_(Busy, 'Busy', 'IDLE'), GetCPUAsyncThreadNum(), FEventQueue.Num, CPUThreadCritical]);
  Result.Append('per second avg/max: %d/%d'#13#10, [Round(PSP), Round(MaxPSP)]);
  if Separator then
      Result.Append('----'#13#10);
end;

procedure TAI_DNN_ThreadPool.EnabledLastProcessRaster(value_: Boolean);
var
  i: Integer;
begin
  FCritical.Acquire;
  try
    FLastRasterList.Clear;
    FLastRasterList.UserToken := '';
    for i := 0 to Count - 1 do
        Items[i].FEnabledLastProcessRaster := value_;
  finally
      FCritical.Release;
  end;
end;

function TAI_DNN_ThreadPool.LockLastRasterList: TMemoryRasterList;
var
  i: Integer;
  raster_: TRaster;
begin
  FCritical.Acquire;
  try
    FLastRasterList.Clear;
    for i := 0 to Count - 1 do
      begin
        raster_ := Items[i].GetAndLockLastProcessRaster;
        FLastRasterList.Add(raster_);
      end;
  finally
      FCritical.Release;
  end;
  FLastRasterList.UserToken := FName;
  Result := FLastRasterList;
end;

procedure TAI_DNN_ThreadPool.UnLockLastRasterList;
var
  i: Integer;
begin
  FCritical.Acquire;
  try
    FLastRasterList.Clear;
    FLastRasterList.UserToken := '';
    for i := 0 to Count - 1 do
        Items[i].UnLockLastProcessRaster;
  finally
      FCritical.Release;
  end;
end;

procedure TAI_DNN_Thread_Metric.ThreadFree;
begin
  FAI.Metric_ResNet_Close(MetricHnd);
end;

procedure TAI_DNN_Thread_Metric.CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  MetricHnd := FAI.Metric_ResNet_Open(VarToStr(Data3));
end;

procedure TAI_DNN_Thread_Metric.CMD_OpenShareFace();
begin
  MetricHnd := FAI.Metric_ResNet_Open_ShareFace();
end;

procedure TAI_DNN_Thread_Metric.CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  MetricHnd := FAI.Metric_ResNet_Open_Stream(Data2 as TMemoryStream64);
end;

procedure TAI_DNN_Thread_Metric.CMD_SyncProcess(data: Pointer);
var
  p: PCMD_SyncProcess;
begin
  p := data;
  p^.output := LVecCopy(FAI.Metric_ResNet_Process(MetricHnd, p^.Input));
  UpdateLastProcessRaster(p^.Input);
  p^.Done.V := True;
end;

procedure TAI_DNN_Thread_Metric.OnComputeThreadResult(ThSender: TCompute);
var
  p: PCMD_AsyncProcess;
begin
  p := ThSender.UserData;
  try
    if Assigned(p^.OnResult_C) then
        p^.OnResult_C(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_M) then
        p^.OnResult_M(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_P) then
        p^.OnResult_P(Self, p^.UserData, p^.Input, p^.output);
  except
  end;
  if p^.FreeInput then
      DisposeObject(p^.Input);
  SetLength(p^.output, 0);
  Dispose(p);
  DoEventDone(ThSender);
end;

procedure TAI_DNN_Thread_Metric.CMD_AsyncProcess(data: Pointer);
var
  p: PCMD_AsyncProcess;
begin
  p := data;
  p^.output := LVecCopy(FAI.Metric_ResNet_Process(MetricHnd, p^.Input));
  UpdateLastProcessRaster(p^.Input);
  DoRunEvent(p, {$IFDEF FPC}@{$ENDIF FPC}OnComputeThreadResult);
end;

constructor TAI_DNN_Thread_Metric.Create;
begin
  inherited Create;
  MetricHnd := nil;
end;

procedure TAI_DNN_Thread_Metric.Open(train_file: SystemString);
begin
  FThreadPost.PostM3(nil, nil, train_file, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open);
end;

procedure TAI_DNN_Thread_Metric.Open_ShareFace();
begin
  FThreadPost.PostM1({$IFDEF FPC}@{$ENDIF FPC}CMD_OpenShareFace);
end;

procedure TAI_DNN_Thread_Metric.Open_Stream(stream: TMemoryStream64);
begin
  FThreadPost.PostM3(nil, stream, NULL, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open_Stream);
end;

function TAI_DNN_Thread_Metric.Process(Input: TMemoryRaster): TLVec;
var
  CMD_: TCMD_SyncProcess;
begin
  CheckGPUPerformanceCritical;
  CMD_.Done := TAtomBool.Create(False);
  CMD_.Input := Input;
  FThreadPost.PostM2(@CMD_, {$IFDEF FPC}@{$ENDIF FPC}CMD_SyncProcess);
  while not CMD_.Done.V do
      TCompute.Sleep(1);
  Result := CMD_.output;
  DisposeObject(CMD_.Done);
end;

procedure TAI_DNN_Thread_Metric.ProcessC(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_Metric_AsyncProcess_C);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := OnResult;
  p^.OnResult_M := nil;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_Metric.ProcessM(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_Metric_AsyncProcess_M);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := OnResult;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_Metric.ProcessP(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_Metric_AsyncProcess_P);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := nil;
  p^.OnResult_P := OnResult;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_LMetric.ThreadFree;
begin
  FAI.LMetric_ResNet_Close(LMetricHnd);
end;

procedure TAI_DNN_Thread_LMetric.CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  LMetricHnd := FAI.LMetric_ResNet_Open(VarToStr(Data3));
end;

procedure TAI_DNN_Thread_LMetric.CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  LMetricHnd := FAI.LMetric_ResNet_Open_Stream(Data2 as TMemoryStream64);
end;

procedure TAI_DNN_Thread_LMetric.CMD_SyncProcess(data: Pointer);
var
  p: PCMD_SyncProcess;
begin
  p := data;
  p^.output := LVecCopy(FAI.LMetric_ResNet_Process(LMetricHnd, p^.Input));
  UpdateLastProcessRaster(p^.Input);
  p^.Done.V := True;
end;

procedure TAI_DNN_Thread_LMetric.OnComputeThreadResult(ThSender: TCompute);
var
  p: PCMD_AsyncProcess;
begin
  p := ThSender.UserData;
  try
    if Assigned(p^.OnResult_C) then
        p^.OnResult_C(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_M) then
        p^.OnResult_M(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_P) then
        p^.OnResult_P(Self, p^.UserData, p^.Input, p^.output);
  except
  end;
  if p^.FreeInput then
      DisposeObject(p^.Input);
  SetLength(p^.output, 0);
  Dispose(p);
  DoEventDone(ThSender);
end;

procedure TAI_DNN_Thread_LMetric.CMD_AsyncProcess(data: Pointer);
var
  p: PCMD_AsyncProcess;
begin
  p := data;
  p^.output := LVecCopy(FAI.LMetric_ResNet_Process(LMetricHnd, p^.Input));
  UpdateLastProcessRaster(p^.Input);
  DoRunEvent(p, {$IFDEF FPC}@{$ENDIF FPC}OnComputeThreadResult);
end;

constructor TAI_DNN_Thread_LMetric.Create;
begin
  inherited Create;
  LMetricHnd := nil;
end;

procedure TAI_DNN_Thread_LMetric.Open(train_file: SystemString);
begin
  FThreadPost.PostM3(nil, nil, train_file, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open);
end;

procedure TAI_DNN_Thread_LMetric.Open_Stream(stream: TMemoryStream64);
begin
  FThreadPost.PostM3(nil, stream, NULL, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open_Stream);
end;

function TAI_DNN_Thread_LMetric.Process(Input: TMemoryRaster): TLVec;
var
  CMD_: TCMD_SyncProcess;
begin
  CheckGPUPerformanceCritical;
  CMD_.Done := TAtomBool.Create(False);
  CMD_.Input := Input;
  FThreadPost.PostM2(@CMD_, {$IFDEF FPC}@{$ENDIF FPC}CMD_SyncProcess);
  while not CMD_.Done.V do
      TCompute.Sleep(1);
  Result := CMD_.output;
  DisposeObject(CMD_.Done);
end;

procedure TAI_DNN_Thread_LMetric.ProcessC(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_LMetric_AsyncProcess_C);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := OnResult;
  p^.OnResult_M := nil;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_LMetric.ProcessM(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_LMetric_AsyncProcess_M);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := OnResult;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_LMetric.ProcessP(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_LMetric_AsyncProcess_P);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := nil;
  p^.OnResult_P := OnResult;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_MMOD6L.ThreadFree;
begin
  FAI.MMOD6L_DNN_Close(MMOD6LHnd);
end;

procedure TAI_DNN_Thread_MMOD6L.CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  MMOD6LHnd := FAI.MMOD6L_DNN_Open(VarToStr(Data3));
end;

procedure TAI_DNN_Thread_MMOD6L.CMD_OpenFace();
begin
  MMOD6LHnd := FAI.MMOD6L_DNN_Open_Face();
end;

procedure TAI_DNN_Thread_MMOD6L.CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  MMOD6LHnd := FAI.MMOD6L_DNN_Open_Stream(Data2 as TMemoryStream64);
end;

procedure TAI_DNN_Thread_MMOD6L.CMD_SyncProcess(data: Pointer);
var
  p: PCMD_SyncProcess;
begin
  p := data;
  p^.output := FAI.MMOD6L_DNN_Process(MMOD6LHnd, p^.Input);
  UpdateLastProcessRaster(p^.Input);
  p^.Done.V := True;
end;

procedure TAI_DNN_Thread_MMOD6L.CMD_AsyncProcess_Result(ThSender: TCompute);
var
  p: PCMD_AsyncProcess;
begin
  p := ThSender.UserData;
  try
    if Assigned(p^.OnResult_C) then
        p^.OnResult_C(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_M) then
        p^.OnResult_M(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_P) then
        p^.OnResult_P(Self, p^.UserData, p^.Input, p^.output);
  except
  end;
  if p^.FreeInput then
      DisposeObject(p^.Input);
  SetLength(p^.output, 0);
  Dispose(p);
  DoEventDone(ThSender);
end;

procedure TAI_DNN_Thread_MMOD6L.CMD_AsyncProcess(data: Pointer);
var
  p: PCMD_AsyncProcess;
begin
  p := data;
  p^.output := FAI.MMOD6L_DNN_Process(MMOD6LHnd, p^.Input);
  UpdateLastProcessRaster(p^.Input);
  DoRunEvent(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess_Result);
end;

procedure TAI_DNN_Thread_MMOD6L.CMD_AsyncProcessMatrix_Result(ThSender: TCompute);
var
  p: PCMD_AsyncProcessMatrix;
begin
  p := ThSender.UserData;
  try
    if Assigned(p^.OnResult_C) then
        p^.OnResult_C(Self, p^.UserData, p^.Matrix_IMG, p^.output);
    if Assigned(p^.OnResult_M) then
        p^.OnResult_M(Self, p^.UserData, p^.Matrix_IMG, p^.output);
    if Assigned(p^.OnResult_P) then
        p^.OnResult_P(Self, p^.UserData, p^.Matrix_IMG, p^.output);
  except
  end;
  if p^.FreeInput then
      FAI.Close_Matrix_Image(p^.Matrix_IMG);
  SetLength(p^.output, 0);
  Dispose(p);
  DoEventDone(ThSender);
end;

procedure TAI_DNN_Thread_MMOD6L.CMD_AsyncProcessMatrix(data: Pointer);
var
  p: PCMD_AsyncProcessMatrix;
begin
  p := data;
  p^.output := FAI.MMOD6L_DNN_Process_Matrix(MMOD6LHnd, p^.Matrix_IMG);
  UpdateLastProcessMatrixRaster(p^.Matrix_IMG);
  Self.DoRunEvent(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcessMatrix_Result);
end;

constructor TAI_DNN_Thread_MMOD6L.Create;
begin
  inherited Create;
  MMOD6LHnd := nil;
end;

procedure TAI_DNN_Thread_MMOD6L.Open(train_file: SystemString);
begin
  FThreadPost.PostM3(nil, nil, train_file, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open);
end;

procedure TAI_DNN_Thread_MMOD6L.Open_Face;
begin
  FThreadPost.PostM1({$IFDEF FPC}@{$ENDIF FPC}CMD_OpenFace);
end;

procedure TAI_DNN_Thread_MMOD6L.Open_Stream(stream: TMemoryStream64);
begin
  FThreadPost.PostM3(nil, stream, NULL, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open_Stream);
end;

function TAI_DNN_Thread_MMOD6L.Process(Input: TMemoryRaster): TMMOD_Desc;
var
  CMD_: TCMD_SyncProcess;
begin
  CheckGPUPerformanceCritical;
  CMD_.Done := TAtomBool.Create(False);
  CMD_.Input := Input;
  FThreadPost.PostM2(@CMD_, {$IFDEF FPC}@{$ENDIF FPC}CMD_SyncProcess);
  while not CMD_.Done.V do
      TCompute.Sleep(1);
  Result := CMD_.output;
  DisposeObject(CMD_.Done);
end;

procedure TAI_DNN_Thread_MMOD6L.ProcessC(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD6L_AsyncProcess_C);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := OnResult;
  p^.OnResult_M := nil;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_MMOD6L.ProcessM(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD6L_AsyncProcess_M);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := OnResult;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_MMOD6L.ProcessP(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD6L_AsyncProcess_P);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := nil;
  p^.OnResult_P := OnResult;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_MMOD6L.ProcessMatrixC(UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD6L_AsyncProcessMatrix_C);
var
  p: PCMD_AsyncProcessMatrix;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Matrix_IMG := Matrix_IMG;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := OnResult;
  p^.OnResult_M := nil;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcessMatrix);
end;

procedure TAI_DNN_Thread_MMOD6L.ProcessMatrixM(UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD6L_AsyncProcessMatrix_M);
var
  p: PCMD_AsyncProcessMatrix;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Matrix_IMG := Matrix_IMG;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := OnResult;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcessMatrix);
end;

procedure TAI_DNN_Thread_MMOD6L.ProcessMatrixP(UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD6L_AsyncProcessMatrix_P);
var
  p: PCMD_AsyncProcessMatrix;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Matrix_IMG := Matrix_IMG;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := nil;
  p^.OnResult_P := OnResult;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcessMatrix);
end;

procedure TAI_DNN_Thread_MMOD3L.ThreadFree;
begin
  FAI.MMOD3L_DNN_Close(MMOD3LHnd);
end;

procedure TAI_DNN_Thread_MMOD3L.CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  MMOD3LHnd := FAI.MMOD3L_DNN_Open(VarToStr(Data3));
end;

procedure TAI_DNN_Thread_MMOD3L.CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  MMOD3LHnd := FAI.MMOD3L_DNN_Open_Stream(Data2 as TMemoryStream64);
end;

procedure TAI_DNN_Thread_MMOD3L.CMD_SyncProcess(data: Pointer);
var
  p: PCMD_SyncProcess;
begin
  p := data;
  p^.output := FAI.MMOD3L_DNN_Process(MMOD3LHnd, p^.Input);
  UpdateLastProcessRaster(p^.Input);
  p^.Done.V := True;
end;

procedure TAI_DNN_Thread_MMOD3L.CMD_AsyncProcess_Result(ThSender: TCompute);
var
  p: PCMD_AsyncProcess;
begin
  p := ThSender.UserData;
  try
    if Assigned(p^.OnResult_C) then
        p^.OnResult_C(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_M) then
        p^.OnResult_M(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_P) then
        p^.OnResult_P(Self, p^.UserData, p^.Input, p^.output);
  except
  end;
  if p^.FreeInput then
      DisposeObject(p^.Input);
  SetLength(p^.output, 0);
  Dispose(p);
  DoEventDone(ThSender);
end;

procedure TAI_DNN_Thread_MMOD3L.CMD_AsyncProcess(data: Pointer);
var
  p: PCMD_AsyncProcess;
begin
  p := data;
  p^.output := FAI.MMOD3L_DNN_Process(MMOD3LHnd, p^.Input);
  UpdateLastProcessRaster(p^.Input);
  DoRunEvent(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess_Result);
end;

procedure TAI_DNN_Thread_MMOD3L.CMD_AsyncProcessMatrix_Result(ThSender: TCompute);
var
  p: PCMD_AsyncProcessMatrix;
begin
  p := ThSender.UserData;
  try
    if Assigned(p^.OnResult_C) then
        p^.OnResult_C(Self, p^.UserData, p^.Matrix_IMG, p^.output);
    if Assigned(p^.OnResult_M) then
        p^.OnResult_M(Self, p^.UserData, p^.Matrix_IMG, p^.output);
    if Assigned(p^.OnResult_P) then
        p^.OnResult_P(Self, p^.UserData, p^.Matrix_IMG, p^.output);
  except
  end;
  if p^.FreeInput then
      FAI.Close_Matrix_Image(p^.Matrix_IMG);
  SetLength(p^.output, 0);
  Dispose(p);
  DoEventDone(ThSender);
end;

procedure TAI_DNN_Thread_MMOD3L.CMD_AsyncProcessMatrix(data: Pointer);
var
  p: PCMD_AsyncProcessMatrix;
begin
  p := data;
  p^.output := FAI.MMOD3L_DNN_Process_Matrix(MMOD3LHnd, p^.Matrix_IMG);
  UpdateLastProcessMatrixRaster(p^.Matrix_IMG);
  DoRunEvent(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcessMatrix_Result);
end;

constructor TAI_DNN_Thread_MMOD3L.Create;
begin
  inherited Create;
  MMOD3LHnd := nil;
end;

procedure TAI_DNN_Thread_MMOD3L.Open(train_file: SystemString);
begin
  FThreadPost.PostM3(nil, nil, train_file, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open);
end;

procedure TAI_DNN_Thread_MMOD3L.Open_Stream(stream: TMemoryStream64);
begin
  FThreadPost.PostM3(nil, stream, NULL, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open_Stream);
end;

function TAI_DNN_Thread_MMOD3L.Process(Input: TMemoryRaster): TMMOD_Desc;
var
  CMD_: TCMD_SyncProcess;
begin
  CheckGPUPerformanceCritical;
  CMD_.Done := TAtomBool.Create(False);
  CMD_.Input := Input;
  FThreadPost.PostM2(@CMD_, {$IFDEF FPC}@{$ENDIF FPC}CMD_SyncProcess);
  while not CMD_.Done.V do
      TCompute.Sleep(1);
  Result := CMD_.output;
  DisposeObject(CMD_.Done);
end;

procedure TAI_DNN_Thread_MMOD3L.ProcessC(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD3L_AsyncProcess_C);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := OnResult;
  p^.OnResult_M := nil;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_MMOD3L.ProcessM(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD3L_AsyncProcess_M);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := OnResult;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_MMOD3L.ProcessP(UserData: Pointer; Input: TMemoryRaster; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD3L_AsyncProcess_P);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := nil;
  p^.OnResult_P := OnResult;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_MMOD3L.ProcessMatrixC(UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD3L_AsyncProcessMatrix_C);
var
  p: PCMD_AsyncProcessMatrix;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Matrix_IMG := Matrix_IMG;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := OnResult;
  p^.OnResult_M := nil;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcessMatrix);
end;

procedure TAI_DNN_Thread_MMOD3L.ProcessMatrixM(UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD3L_AsyncProcessMatrix_M);
var
  p: PCMD_AsyncProcessMatrix;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Matrix_IMG := Matrix_IMG;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := OnResult;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcessMatrix);
end;

procedure TAI_DNN_Thread_MMOD3L.ProcessMatrixP(UserData: Pointer; Matrix_IMG: TMatrix_Image_Handle; FreeInput: Boolean; OnResult: TAI_DNN_Thread_MMOD3L_AsyncProcessMatrix_P);
var
  p: PCMD_AsyncProcessMatrix;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Matrix_IMG := Matrix_IMG;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := nil;
  p^.OnResult_P := OnResult;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcessMatrix);
end;

procedure TAI_DNN_Thread_RNIC.ThreadFree;
begin
  FAI.RNIC_Close(RNICHnd);
end;

procedure TAI_DNN_Thread_RNIC.CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  RNICHnd := FAI.RNIC_Open(VarToStr(Data3));
end;

procedure TAI_DNN_Thread_RNIC.CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  RNICHnd := FAI.RNIC_Open_Stream(Data2 as TMemoryStream64);
end;

procedure TAI_DNN_Thread_RNIC.CMD_SyncProcess(data: Pointer);
var
  p: PCMD_SyncProcess;
begin
  p := data;
  p^.output := FAI.RNIC_Process(RNICHnd, p^.Input, p^.num_crops);
  UpdateLastProcessRaster(p^.Input);
  p^.Done.V := True;
end;

procedure TAI_DNN_Thread_RNIC.OnComputeThreadResult(ThSender: TCompute);
var
  p: PCMD_AsyncProcess;
begin
  p := ThSender.UserData;
  try
    if Assigned(p^.OnResult_C) then
        p^.OnResult_C(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_M) then
        p^.OnResult_M(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_P) then
        p^.OnResult_P(Self, p^.UserData, p^.Input, p^.output);
  except
  end;
  if p^.FreeInput then
      DisposeObject(p^.Input);
  SetLength(p^.output, 0);
  Dispose(p);
  DoEventDone(ThSender);
end;

procedure TAI_DNN_Thread_RNIC.CMD_AsyncProcess(data: Pointer);
var
  p: PCMD_AsyncProcess;
begin
  p := data;
  p^.output := FAI.RNIC_Process(RNICHnd, p^.Input, p^.num_crops);
  UpdateLastProcessRaster(p^.Input);
  DoRunEvent(p, {$IFDEF FPC}@{$ENDIF FPC}OnComputeThreadResult);
end;

constructor TAI_DNN_Thread_RNIC.Create;
begin
  inherited Create;
  RNICHnd := nil;
end;

procedure TAI_DNN_Thread_RNIC.Open(train_file: SystemString);
begin
  FThreadPost.PostM3(nil, nil, train_file, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open);
end;

procedure TAI_DNN_Thread_RNIC.Open_Stream(stream: TMemoryStream64);
begin
  FThreadPost.PostM3(nil, stream, NULL, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open_Stream);
end;

function TAI_DNN_Thread_RNIC.Process(Input: TMemoryRaster; num_crops: Integer): TLVec;
var
  CMD_: TCMD_SyncProcess;
begin
  CheckGPUPerformanceCritical;
  CMD_.Done := TAtomBool.Create(False);
  CMD_.Input := Input;
  CMD_.num_crops := num_crops;
  FThreadPost.PostM2(@CMD_, {$IFDEF FPC}@{$ENDIF FPC}CMD_SyncProcess);
  while not CMD_.Done.V do
      TCompute.Sleep(1);
  Result := CMD_.output;
  DisposeObject(CMD_.Done);
end;

procedure TAI_DNN_Thread_RNIC.ProcessC(UserData: Pointer; Input: TMemoryRaster; num_crops: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_RNIC_AsyncProcess_C);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.num_crops := num_crops;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := OnResult;
  p^.OnResult_M := nil;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_RNIC.ProcessM(UserData: Pointer; Input: TMemoryRaster; num_crops: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_RNIC_AsyncProcess_M);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.num_crops := num_crops;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := OnResult;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_RNIC.ProcessP(UserData: Pointer; Input: TMemoryRaster; num_crops: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_RNIC_AsyncProcess_P);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.num_crops := num_crops;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := nil;
  p^.OnResult_P := OnResult;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_LRNIC.ThreadFree;
begin
  FAI.LRNIC_Close(LRNICHnd);
end;

procedure TAI_DNN_Thread_LRNIC.CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  LRNICHnd := FAI.LRNIC_Open(VarToStr(Data3));
end;

procedure TAI_DNN_Thread_LRNIC.CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  LRNICHnd := FAI.LRNIC_Open_Stream(Data2 as TMemoryStream64);
end;

procedure TAI_DNN_Thread_LRNIC.CMD_SyncProcess(data: Pointer);
var
  p: PCMD_SyncProcess;
begin
  p := data;
  p^.output := FAI.LRNIC_Process(LRNICHnd, p^.Input, p^.num_crops);
  UpdateLastProcessRaster(p^.Input);
  p^.Done.V := True;
end;

procedure TAI_DNN_Thread_LRNIC.OnComputeThreadResult(ThSender: TCompute);
var
  p: PCMD_AsyncProcess;
begin
  p := ThSender.UserData;
  try
    if Assigned(p^.OnResult_C) then
        p^.OnResult_C(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_M) then
        p^.OnResult_M(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_P) then
        p^.OnResult_P(Self, p^.UserData, p^.Input, p^.output);
  except
  end;
  if p^.FreeInput then
      DisposeObject(p^.Input);
  SetLength(p^.output, 0);
  Dispose(p);
  DoEventDone(ThSender);
end;

procedure TAI_DNN_Thread_LRNIC.CMD_AsyncProcess(data: Pointer);
var
  p: PCMD_AsyncProcess;
begin
  p := data;
  p^.output := FAI.LRNIC_Process(LRNICHnd, p^.Input, p^.num_crops);
  UpdateLastProcessRaster(p^.Input);
  DoRunEvent(p, {$IFDEF FPC}@{$ENDIF FPC}OnComputeThreadResult);
end;

constructor TAI_DNN_Thread_LRNIC.Create;
begin
  inherited Create;
  LRNICHnd := nil;
end;

procedure TAI_DNN_Thread_LRNIC.Open(train_file: SystemString);
begin
  FThreadPost.PostM3(nil, nil, train_file, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open);
end;

procedure TAI_DNN_Thread_LRNIC.Open_Stream(stream: TMemoryStream64);
begin
  FThreadPost.PostM3(nil, stream, NULL, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open_Stream);
end;

function TAI_DNN_Thread_LRNIC.Process(Input: TMemoryRaster; num_crops: Integer): TLVec;
var
  CMD_: TCMD_SyncProcess;
begin
  CheckGPUPerformanceCritical;
  CMD_.Done := TAtomBool.Create(False);
  CMD_.Input := Input;
  CMD_.num_crops := num_crops;
  FThreadPost.PostM2(@CMD_, {$IFDEF FPC}@{$ENDIF FPC}CMD_SyncProcess);
  while not CMD_.Done.V do
      TCompute.Sleep(1);
  Result := CMD_.output;
  DisposeObject(CMD_.Done);
end;

procedure TAI_DNN_Thread_LRNIC.ProcessC(UserData: Pointer; Input: TMemoryRaster; num_crops: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_LRNIC_AsyncProcess_C);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.num_crops := num_crops;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := OnResult;
  p^.OnResult_M := nil;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_LRNIC.ProcessM(UserData: Pointer; Input: TMemoryRaster; num_crops: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_LRNIC_AsyncProcess_M);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.num_crops := num_crops;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := OnResult;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_LRNIC.ProcessP(UserData: Pointer; Input: TMemoryRaster; num_crops: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_LRNIC_AsyncProcess_P);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.num_crops := num_crops;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := nil;
  p^.OnResult_P := OnResult;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_GDCNIC.ThreadFree;
begin
  FAI.GDCNIC_Close(GDCNICHnd);
end;

procedure TAI_DNN_Thread_GDCNIC.CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  GDCNICHnd := FAI.GDCNIC_Open(VarToStr(Data3));
end;

procedure TAI_DNN_Thread_GDCNIC.CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  GDCNICHnd := FAI.GDCNIC_Open_Stream(Data2 as TMemoryStream64);
end;

procedure TAI_DNN_Thread_GDCNIC.CMD_SyncProcess(data: Pointer);
var
  p: PCMD_SyncProcess;
begin
  p := data;
  p^.output := FAI.GDCNIC_Process(GDCNICHnd, p^.SS_Width, p^.SS_Height, p^.Input);
  UpdateLastProcessRaster(p^.Input);
  p^.Done.V := True;
end;

procedure TAI_DNN_Thread_GDCNIC.OnComputeThreadResult(ThSender: TCompute);
var
  p: PCMD_AsyncProcess;
begin
  p := ThSender.UserData;
  try
    if Assigned(p^.OnResult_C) then
        p^.OnResult_C(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_M) then
        p^.OnResult_M(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_P) then
        p^.OnResult_P(Self, p^.UserData, p^.Input, p^.output);
  except
  end;
  if p^.FreeInput then
      DisposeObject(p^.Input);
  SetLength(p^.output, 0);
  Dispose(p);
  DoEventDone(ThSender);
end;

procedure TAI_DNN_Thread_GDCNIC.CMD_AsyncProcess(data: Pointer);
var
  p: PCMD_AsyncProcess;
begin
  p := data;
  p^.output := FAI.GDCNIC_Process(GDCNICHnd, p^.SS_Width, p^.SS_Height, p^.Input);
  UpdateLastProcessRaster(p^.Input);
  DoRunEvent(p, {$IFDEF FPC}@{$ENDIF FPC}OnComputeThreadResult);
end;

constructor TAI_DNN_Thread_GDCNIC.Create;
begin
  inherited Create;
  GDCNICHnd := nil;
end;

procedure TAI_DNN_Thread_GDCNIC.Open(train_file: SystemString);
begin
  FThreadPost.PostM3(nil, nil, train_file, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open);
end;

procedure TAI_DNN_Thread_GDCNIC.Open_Stream(stream: TMemoryStream64);
begin
  FThreadPost.PostM3(nil, stream, NULL, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open_Stream);
end;

function TAI_DNN_Thread_GDCNIC.Process(Input: TMemoryRaster; SS_Width, SS_Height: Integer): TLVec;
var
  CMD_: TCMD_SyncProcess;
begin
  CheckGPUPerformanceCritical;
  CMD_.Done := TAtomBool.Create(False);
  CMD_.Input := Input;
  CMD_.SS_Width := SS_Width;
  CMD_.SS_Height := SS_Height;
  FThreadPost.PostM2(@CMD_, {$IFDEF FPC}@{$ENDIF FPC}CMD_SyncProcess);
  while not CMD_.Done.V do
      TCompute.Sleep(1);
  Result := CMD_.output;
  DisposeObject(CMD_.Done);
end;

procedure TAI_DNN_Thread_GDCNIC.ProcessC(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_GDCNIC_AsyncProcess_C);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.SS_Width := SS_Width;
  p^.SS_Height := SS_Height;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := OnResult;
  p^.OnResult_M := nil;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_GDCNIC.ProcessM(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_GDCNIC_AsyncProcess_M);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.SS_Width := SS_Width;
  p^.SS_Height := SS_Height;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := OnResult;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_GDCNIC.ProcessP(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_GDCNIC_AsyncProcess_P);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.SS_Width := SS_Width;
  p^.SS_Height := SS_Height;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := nil;
  p^.OnResult_P := OnResult;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_GNIC.ThreadFree;
begin
  FAI.GNIC_Close(GNICHnd);
end;

procedure TAI_DNN_Thread_GNIC.CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  GNICHnd := FAI.GNIC_Open(VarToStr(Data3));
end;

procedure TAI_DNN_Thread_GNIC.CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  GNICHnd := FAI.GNIC_Open_Stream(Data2 as TMemoryStream64);
end;

procedure TAI_DNN_Thread_GNIC.CMD_SyncProcess(data: Pointer);
var
  p: PCMD_SyncProcess;
begin
  p := data;
  p^.output := FAI.GNIC_Process(GNICHnd, p^.SS_Width, p^.SS_Height, p^.Input);
  UpdateLastProcessRaster(p^.Input);
  p^.Done.V := True;
end;

procedure TAI_DNN_Thread_GNIC.OnComputeThreadResult(ThSender: TCompute);
var
  p: PCMD_AsyncProcess;
begin
  p := ThSender.UserData;
  try
    if Assigned(p^.OnResult_C) then
        p^.OnResult_C(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_M) then
        p^.OnResult_M(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_P) then
        p^.OnResult_P(Self, p^.UserData, p^.Input, p^.output);
  except
  end;
  if p^.FreeInput then
      DisposeObject(p^.Input);
  SetLength(p^.output, 0);
  Dispose(p);
  DoEventDone(ThSender);
end;

procedure TAI_DNN_Thread_GNIC.CMD_AsyncProcess(data: Pointer);
var
  p: PCMD_AsyncProcess;
begin
  p := data;
  p^.output := FAI.GNIC_Process(GNICHnd, p^.SS_Width, p^.SS_Height, p^.Input);
  UpdateLastProcessRaster(p^.Input);
  DoRunEvent(p, {$IFDEF FPC}@{$ENDIF FPC}OnComputeThreadResult);
end;

constructor TAI_DNN_Thread_GNIC.Create;
begin
  inherited Create;
  GNICHnd := nil;
end;

procedure TAI_DNN_Thread_GNIC.Open(train_file: SystemString);
begin
  FThreadPost.PostM3(nil, nil, train_file, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open);
end;

procedure TAI_DNN_Thread_GNIC.Open_Stream(stream: TMemoryStream64);
begin
  FThreadPost.PostM3(nil, stream, NULL, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open_Stream);
end;

function TAI_DNN_Thread_GNIC.Process(Input: TMemoryRaster; SS_Width, SS_Height: Integer): TLVec;
var
  CMD_: TCMD_SyncProcess;
begin
  CheckGPUPerformanceCritical;
  CMD_.Done := TAtomBool.Create(False);
  CMD_.Input := Input;
  CMD_.SS_Width := SS_Width;
  CMD_.SS_Height := SS_Height;
  FThreadPost.PostM2(@CMD_, {$IFDEF FPC}@{$ENDIF FPC}CMD_SyncProcess);
  while not CMD_.Done.V do
      TCompute.Sleep(1);
  Result := CMD_.output;
  DisposeObject(CMD_.Done);
end;

procedure TAI_DNN_Thread_GNIC.ProcessC(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_GNIC_AsyncProcess_C);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.SS_Width := SS_Width;
  p^.SS_Height := SS_Height;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := OnResult;
  p^.OnResult_M := nil;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_GNIC.ProcessM(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_GNIC_AsyncProcess_M);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.SS_Width := SS_Width;
  p^.SS_Height := SS_Height;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := OnResult;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_GNIC.ProcessP(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_GNIC_AsyncProcess_P);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.SS_Width := SS_Width;
  p^.SS_Height := SS_Height;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := nil;
  p^.OnResult_P := OnResult;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_SS.ThreadFree;
begin
  FAI.SS_Close(SSHnd);
end;

procedure TAI_DNN_Thread_SS.CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  SSHnd := FAI.SS_Open(VarToStr(Data3));
end;

procedure TAI_DNN_Thread_SS.CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  SSHnd := FAI.SS_Open_Stream(Data2 as TMemoryStream64);
end;

procedure TAI_DNN_Thread_SS.CMD_SyncProcess(data: Pointer);
var
  p: PCMD_SyncProcess;
begin
  p := data;
  p^.output := FAI.SS_Process(SSHnd, p^.Input, p^.colorPool, p^.SSTokenOutput);
  UpdateLastProcessRaster(p^.Input);
  p^.Done.V := True;
end;

procedure TAI_DNN_Thread_SS.OnComputeThreadResult(ThSender: TCompute);
var
  p: PCMD_AsyncProcess;
begin
  p := ThSender.UserData;
  try
    if Assigned(p^.OnResult_C) then
        p^.OnResult_C(Self, p^.UserData, p^.Input, p^.SSTokenOutput, p^.output);
    if Assigned(p^.OnResult_M) then
        p^.OnResult_M(Self, p^.UserData, p^.Input, p^.SSTokenOutput, p^.output);
    if Assigned(p^.OnResult_P) then
        p^.OnResult_P(Self, p^.UserData, p^.Input, p^.SSTokenOutput, p^.output);
  except
  end;
  if p^.FreeInput then
      DisposeObject(p^.Input);
  DisposeObject(p^.SSTokenOutput);
  if p^.output <> nil then
      DisposeObject(p^.output);
  Dispose(p);
  DoEventDone(ThSender);
end;

procedure TAI_DNN_Thread_SS.CMD_AsyncProcess(data: Pointer);
var
  p: PCMD_AsyncProcess;
begin
  p := data;
  p^.SSTokenOutput := TPascalStringList.Create;
  p^.output := FAI.SS_Process(SSHnd, p^.Input, p^.colorPool, p^.SSTokenOutput);
  UpdateLastProcessRaster(p^.Input);
  DoRunEvent(p, {$IFDEF FPC}@{$ENDIF FPC}OnComputeThreadResult);
end;

constructor TAI_DNN_Thread_SS.Create;
begin
  inherited Create;
  SSHnd := nil;
end;

procedure TAI_DNN_Thread_SS.Open(train_file: SystemString);
begin
  FThreadPost.PostM3(nil, nil, train_file, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open);
end;

procedure TAI_DNN_Thread_SS.Open_Stream(stream: TMemoryStream64);
begin
  FThreadPost.PostM3(nil, stream, NULL, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open_Stream);
end;

function TAI_DNN_Thread_SS.Process(Input: TMemoryRaster; colorPool: TSegmentationColorTable; SSTokenOutput: TPascalStringList): TMemoryRaster;
var
  CMD_: TCMD_SyncProcess;
begin
  CMD_.Done := TAtomBool.Create(False);
  CMD_.Input := Input;
  CMD_.colorPool := colorPool;
  CMD_.SSTokenOutput := SSTokenOutput;
  FThreadPost.PostM2(@CMD_, {$IFDEF FPC}@{$ENDIF FPC}CMD_SyncProcess);
  while not CMD_.Done.V do
      TCompute.Sleep(1);
  Result := CMD_.output;
  DisposeObject(CMD_.Done);
end;

procedure TAI_DNN_Thread_SS.ProcessC(UserData: Pointer; Input: TMemoryRaster; colorPool: TSegmentationColorTable; FreeInput: Boolean; OnResult: TAI_DNN_Thread_SS_AsyncProcess_C);
var
  p: PCMD_AsyncProcess;
begin
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.colorPool := colorPool;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := OnResult;
  p^.OnResult_M := nil;
  p^.OnResult_P := nil;
  p^.output := nil;
  p^.SSTokenOutput := nil;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_SS.ProcessM(UserData: Pointer; Input: TMemoryRaster; colorPool: TSegmentationColorTable; FreeInput: Boolean; OnResult: TAI_DNN_Thread_SS_AsyncProcess_M);
var
  p: PCMD_AsyncProcess;
begin
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.colorPool := colorPool;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := OnResult;
  p^.OnResult_P := nil;
  p^.output := nil;
  p^.SSTokenOutput := nil;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_SS.ProcessP(UserData: Pointer; Input: TMemoryRaster; colorPool: TSegmentationColorTable; FreeInput: Boolean; OnResult: TAI_DNN_Thread_SS_AsyncProcess_P);
var
  p: PCMD_AsyncProcess;
begin
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.colorPool := colorPool;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := nil;
  p^.OnResult_P := OnResult;
  p^.output := nil;
  p^.SSTokenOutput := nil;
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_ZMetric.ThreadFree;
begin
  FAI.ZMetric_Close(ZMetricHnd);
end;

procedure TAI_DNN_Thread_ZMetric.CMD_Open(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  ZMetricHnd := FAI.ZMetric_Open(VarToStr(Data3));
end;

procedure TAI_DNN_Thread_ZMetric.CMD_Open_Stream(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant);
begin
  ZMetricHnd := FAI.ZMetric_Open_Stream(Data2 as TMemoryStream64);
end;

procedure TAI_DNN_Thread_ZMetric.CMD_SyncProcess(data: Pointer);
var
  p: PCMD_SyncProcess;
begin
  p := data;
  p^.output := LVecCopy(FAI.ZMetric_Process(ZMetricHnd, p^.Input, p^.SS_Width, p^.SS_Height));
  UpdateLastProcessRaster(p^.Input);
  p^.Done.V := True;
end;

procedure TAI_DNN_Thread_ZMetric.CMD_AsyncProcess_Result(ThSender: TCompute);
var
  p: PCMD_AsyncProcess;
begin
  p := ThSender.UserData;
  try
    if Assigned(p^.OnResult_C) then
        p^.OnResult_C(Self, p^.UserData, p^.Input, p^.SS_Width, p^.SS_Height, p^.output);
    if Assigned(p^.OnResult_M) then
        p^.OnResult_M(Self, p^.UserData, p^.Input, p^.SS_Width, p^.SS_Height, p^.output);
    if Assigned(p^.OnResult_P) then
        p^.OnResult_P(Self, p^.UserData, p^.Input, p^.SS_Width, p^.SS_Height, p^.output);
  except
  end;
  if p^.FreeInput then
      DisposeObject(p^.Input);
  SetLength(p^.output, 0);
  Dispose(p);
  DoEventDone(ThSender);
end;

procedure TAI_DNN_Thread_ZMetric.CMD_AsyncProcess(data: Pointer);
var
  p: PCMD_AsyncProcess;
begin
  p := data;
  p^.output := LVecCopy(FAI.ZMetric_Process(ZMetricHnd, p^.Input, p^.SS_Width, p^.SS_Height));
  UpdateLastProcessRaster(p^.Input);
  DoRunEvent(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess_Result);
end;

procedure TAI_DNN_Thread_ZMetric.CMD_AsyncProcess_List_Result(ThSender: TCompute);
var
  p: PCMD_AsyncProcess_List;
begin
  p := ThSender.UserData;
  try
    if Assigned(p^.OnResult_C) then
        p^.OnResult_C(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_M) then
        p^.OnResult_M(Self, p^.UserData, p^.Input, p^.output);
    if Assigned(p^.OnResult_P) then
        p^.OnResult_P(Self, p^.UserData, p^.Input, p^.output);
  except
  end;
  if p^.FreeInput then
      DisposeObject(p^.Input);
  SetLength(p^.output, 0, 0);
  Dispose(p);
  DoEventDone(ThSender);
end;

procedure TAI_DNN_Thread_ZMetric.CMD_AsyncProcess_List(data: Pointer);
var
  arry: TMemoryRasterArray;
  p: PCMD_AsyncProcess_List;
begin
  p := data;
  arry := p^.Input.BuildArray;
  p^.output := LMatrixCopy(FAI.ZMetric_Process(ZMetricHnd, arry, p^.SS_Width, p^.SS_Height));
  SetLength(arry, 0);
  DoRunEvent(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess_List_Result);
end;

constructor TAI_DNN_Thread_ZMetric.Create;
begin
  inherited Create;
  ZMetricHnd := nil;
end;

procedure TAI_DNN_Thread_ZMetric.Open(train_file: SystemString);
begin
  FThreadPost.PostM3(nil, nil, train_file, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open);
end;

procedure TAI_DNN_Thread_ZMetric.Open_Stream(stream: TMemoryStream64);
begin
  FThreadPost.PostM3(nil, stream, NULL, {$IFDEF FPC}@{$ENDIF FPC}CMD_Open_Stream);
end;

function TAI_DNN_Thread_ZMetric.Process(Input: TMemoryRaster; SS_Width, SS_Height: Integer): TLVec;
var
  CMD_: TCMD_SyncProcess;
begin
  CheckGPUPerformanceCritical;
  CMD_.Done := TAtomBool.Create(False);
  CMD_.Input := Input;
  CMD_.SS_Width := SS_Width;
  CMD_.SS_Height := SS_Height;
  FThreadPost.PostM2(@CMD_, {$IFDEF FPC}@{$ENDIF FPC}CMD_SyncProcess);
  while not CMD_.Done.V do
      TCompute.Sleep(1);
  Result := CMD_.output;
  DisposeObject(CMD_.Done);
end;

procedure TAI_DNN_Thread_ZMetric.ProcessC(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_ZMetric_AsyncProcess_C);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.SS_Width := SS_Width;
  p^.SS_Height := SS_Height;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := OnResult;
  p^.OnResult_M := nil;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_ZMetric.ProcessM(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_ZMetric_AsyncProcess_M);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.SS_Width := SS_Width;
  p^.SS_Height := SS_Height;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := OnResult;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_ZMetric.ProcessP(UserData: Pointer; Input: TMemoryRaster; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_ZMetric_AsyncProcess_P);
var
  p: PCMD_AsyncProcess;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.SS_Width := SS_Width;
  p^.SS_Height := SS_Height;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := nil;
  p^.OnResult_P := OnResult;
  SetLength(p^.output, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess);
end;

procedure TAI_DNN_Thread_ZMetric.ProcessListC(UserData: Pointer; Input: TMemoryRasterList; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_ZMetric_AsyncProcess_List_C);
var
  p: PCMD_AsyncProcess_List;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.SS_Width := SS_Width;
  p^.SS_Height := SS_Height;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := OnResult;
  p^.OnResult_M := nil;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess_List);
end;

procedure TAI_DNN_Thread_ZMetric.ProcessListM(UserData: Pointer; Input: TMemoryRasterList; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_ZMetric_AsyncProcess_List_M);
var
  p: PCMD_AsyncProcess_List;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.SS_Width := SS_Width;
  p^.SS_Height := SS_Height;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := OnResult;
  p^.OnResult_P := nil;
  SetLength(p^.output, 0, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess_List);
end;

procedure TAI_DNN_Thread_ZMetric.ProcessListP(UserData: Pointer; Input: TMemoryRasterList; SS_Width, SS_Height: Integer; FreeInput: Boolean; OnResult: TAI_DNN_Thread_ZMetric_AsyncProcess_List_P);
var
  p: PCMD_AsyncProcess_List;
begin
  CheckGPUPerformanceCritical;
  new(p);
  p^.UserData := UserData;
  p^.Input := Input;
  p^.SS_Width := SS_Width;
  p^.SS_Height := SS_Height;
  p^.FreeInput := FreeInput;
  p^.OnResult_C := nil;
  p^.OnResult_M := nil;
  p^.OnResult_P := OnResult;
  SetLength(p^.output, 0, 0);
  FThreadPost.PostM2(p, {$IFDEF FPC}@{$ENDIF FPC}CMD_AsyncProcess_List);
end;

constructor TAI_Parallel.Create;
begin
  inherited Create;
  Critical := TCritical.Create;
  FInternalFaceSP := False;
  Wait_AI_Init;
end;

destructor TAI_Parallel.Destroy;
begin
  Clear;
  DisposeObject(Critical);
  inherited Destroy;
end;

procedure TAI_Parallel.Remove(AI_: TAI);
begin
  inherited Remove(AI_);
  DisposeObject(AI_);
end;

procedure TAI_Parallel.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DisposeObject(Items[i]);
  inherited Clear;
end;

procedure TAI_Parallel.Delete(index: Integer);
begin
  DisposeObject(Items[index]);
  inherited Delete(index);
end;

procedure TAI_Parallel.Prepare_Parallel(eng: SystemString; poolSiz: Integer);
begin
  Prepare_Parallel(Prepare_AI_Engine(eng), poolSiz);
end;

procedure TAI_Parallel.Prepare_Parallel(lib_p: PAI_EntryAPI; poolSiz: Integer);
var
  i: Integer;
  AI: TAI;
begin
  if lib_p = nil then
      RaiseInfo('engine library failed!');
  Critical.Acquire;
  try
    if poolSiz > Count then
      begin
        for i := Count to poolSiz - 1 do
          begin
            AI := TAI.OpenEngine(lib_p);
            Add(AI);
          end;
      end;
  finally
      Critical.Release;
  end;
end;

procedure TAI_Parallel.Prepare_Parallel(poolSiz: Integer);
begin
  Prepare_Parallel(zAI_Common.AI_Engine_Library, poolSiz);
end;

procedure TAI_Parallel.Prepare_Parallel;
begin
  Prepare_Parallel(zAI_Common.AI_Engine_Library, zAI_Common.AI_Parallel_Count);
end;

procedure TAI_Parallel.Prepare_FaceSP;
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Do_ParallelFor(pass: Integer);
  begin
    Items[pass].PrepareFaceDataSource;
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure Do_For;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
        Items[pass].PrepareFaceDataSource;
  end;
{$ENDIF Parallel}


begin
  Critical.Acquire;
  try
{$IFDEF Parallel}
{$IFDEF FPC}
    FPCParallelFor(@Do_ParallelFor, 0, Count - 1);
{$ELSE FPC}
    DelphiParallelFor(0, Count - 1, procedure(pass: Integer)
      begin
        Items[pass].PrepareFaceDataSource;
      end);
{$ENDIF FPC}
{$ELSE Parallel}
    Do_For;
{$ENDIF Parallel}
  finally
    Critical.Release;
    FInternalFaceSP := True;
  end;
end;

procedure TAI_Parallel.Prepare_OD6L(stream: TMemoryStream64);
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Do_ParallelFor(pass: Integer);
  begin
    with Items[pass] do
        Parallel_OD6L_Hnd := OD6L_Open_Stream(stream);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure Do_For;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
      with Items[pass] do
          Parallel_OD6L_Hnd := OD6L_Open_Stream(stream);
  end;
{$ENDIF Parallel}


begin
  Critical.Acquire;
  try
{$IFDEF Parallel}
{$IFDEF FPC}
    FPCParallelFor(@Do_ParallelFor, 0, Count - 1);
{$ELSE FPC}
    DelphiParallelFor(0, Count - 1, procedure(pass: Integer)
      begin
        with Items[pass] do
            Parallel_OD6L_Hnd := OD6L_Open_Stream(stream);
      end);
{$ENDIF FPC}
{$ELSE Parallel}
    Do_For;
{$ENDIF Parallel}
  finally
      Critical.Release;
  end;
end;

procedure TAI_Parallel.Prepare_OD3L(stream: TMemoryStream64);
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Do_ParallelFor(pass: Integer);
  begin
    with Items[pass] do
        Parallel_OD3L_Hnd := OD3L_Open_Stream(stream);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure Do_For;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
      with Items[pass] do
          Parallel_OD3L_Hnd := OD3L_Open_Stream(stream);
  end;
{$ENDIF Parallel}


begin
  Critical.Acquire;
  try
{$IFDEF Parallel}
{$IFDEF FPC}
    FPCParallelFor(@Do_ParallelFor, 0, Count - 1);
{$ELSE FPC}
    DelphiParallelFor(0, Count - 1, procedure(pass: Integer)
      begin
        with Items[pass] do
            Parallel_OD3L_Hnd := OD3L_Open_Stream(stream);
      end);
{$ENDIF FPC}
{$ELSE Parallel}
    Do_For;
{$ENDIF Parallel}
  finally
      Critical.Release;
  end;
end;

procedure TAI_Parallel.Prepare_OD6L_Marshal(stream: TMemoryStream64);
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Do_ParallelFor(pass: Integer);
  begin
    with Items[pass] do
        Parallel_OD_Marshal_Hnd := OD6L_Marshal_Open_Stream(stream);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure Do_For;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
      with Items[pass] do
          Parallel_OD_Marshal_Hnd := OD6L_Marshal_Open_Stream(stream);
  end;
{$ENDIF Parallel}


begin
  Critical.Acquire;
  try
{$IFDEF Parallel}
{$IFDEF FPC}
    FPCParallelFor(@Do_ParallelFor, 0, Count - 1);
{$ELSE FPC}
    DelphiParallelFor(0, Count - 1, procedure(pass: Integer)
      begin
        with Items[pass] do
            Parallel_OD_Marshal_Hnd := OD6L_Marshal_Open_Stream(stream);
      end);
{$ENDIF FPC}
{$ELSE Parallel}
    Do_For;
{$ENDIF Parallel}
  finally
      Critical.Release;
  end;
end;

procedure TAI_Parallel.Prepare_SP(stream: TMemoryStream64);
{$IFDEF Parallel}
{$IFDEF FPC}
  procedure Do_ParallelFor(pass: Integer);
  begin
    with Items[pass] do
        Parallel_SP_Hnd := SP_Open_Stream(stream);
  end;
{$ENDIF FPC}
{$ELSE Parallel}
  procedure Do_For;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
      with Items[pass] do
          Parallel_SP_Hnd := SP_Open_Stream(stream);
  end;
{$ENDIF Parallel}


begin
  Critical.Acquire;
  try
{$IFDEF Parallel}
{$IFDEF FPC}
    FPCParallelFor(@Do_ParallelFor, 0, Count - 1);
{$ELSE FPC}
    DelphiParallelFor(0, Count - 1, procedure(pass: Integer)
      begin
        with Items[pass] do
            Parallel_SP_Hnd := SP_Open_Stream(stream);
      end);
{$ENDIF FPC}
{$ELSE Parallel}
    Do_For;
{$ENDIF Parallel}
  finally
      Critical.Release;
  end;
end;

function TAI_Parallel.GetAndLockAI: TAI;
var
  i: Integer;
begin
  Critical.Acquire;
  Result := nil;
  while Result = nil do
    begin
      i := 0;
      while i < Count do
        begin
          if not Items[i].Critical.Busy then
            begin
              Result := Items[i];
              Result.Lock;
              break;
            end;
          inc(i);
        end;
      if Result = nil then
          TCompute.Sleep(1);
    end;
  Critical.Release;
end;

procedure TAI_Parallel.UnLockAI(AI: TAI);
begin
  AI.UnLock;
end;

function TAI_Parallel.Busy: Integer;
var
  i: Integer;
begin
  Critical.Acquire;
  Result := 0;
  for i := 0 to Count - 1 do
    if Items[i].Critical.Busy then
        inc(Result);
  Critical.Release;
end;

function TAI_IO.GetAI: TAI;
begin
  Result := Owner.FAI;
end;

function TAI_IO.GetAIPool: TAI_Parallel;
begin
  Result := Owner.FAIPool;
end;

constructor TAI_IO.Create(Owner_: TAI_IO_Processor);
begin
  inherited Create;
  Owner := Owner_;
  InputRaster := NewRaster();
  OutputRaster := NewRaster();
  IndexNumber := 0;
end;

destructor TAI_IO.Destroy;
begin
  if InputRaster <> nil then
      DisposeObjectAndNil(InputRaster);
  if OutputRaster <> nil then
      DisposeObjectAndNil(OutputRaster);
  inherited Destroy;
end;

procedure TAI_IO.ProcessBefore(UserData: Pointer);
begin

end;

function TAI_IO.Process(UserData: Pointer): Boolean;
begin
  Result := True;
end;

procedure TAI_IO.ProcessAfter(UserData: Pointer);
begin

end;

procedure TAI_IO_Processor.LockInputBuffer;
begin
  LockObject(FInputBuffer);
end;

procedure TAI_IO_Processor.UnLockInputBuffer;
begin
  UnLockObject(FInputBuffer);
end;

procedure TAI_IO_Processor.IOProcessorThreadRun(ThSender: TCompute);

  function DoPickBuff(): TAI_IO_Buffer;
  var
    i: Integer;
  begin
    Result := TAI_IO_Buffer.Create;

    LockInputBuffer;
    for i := 0 to FInputBuffer.Count - 1 do
        Result.Add(FInputBuffer[i]);
    FInputBuffer.Clear;
    UnLockInputBuffer;
  end;

  procedure DoProcessPick(pickBuff: TAI_IO_Buffer);
  var
    pass: Integer;
    IO_: TAI_IO;
    processed_ok: Boolean;
  begin
    for pass := 0 to pickBuff.Count - 1 do
      begin
        IO_ := pickBuff[pass];

        try
          IO_.ProcessBefore(ThSender.UserData);
          processed_ok := IO_.Process(ThSender.UserData);
          IO_.ProcessAfter(ThSender.UserData);
        except
            processed_ok := False;
        end;

        if processed_ok then
          begin
            LockObject(FOutputBuffer);
            FOutputBuffer.Add(IO_);
            UnLockObject(FOutputBuffer);
          end
        else
          begin
            DisposeObject(IO_);
          end;
      end;
  end;

{$IFDEF Parallel}
  procedure DoParallelProcessPick(pickBuff: TAI_IO_Buffer);
  var
    tmp_buff: TAI_IO_Buffer;
    tmp_buff_state: array of Boolean;
{$IFDEF FPC}
    procedure Nested_ParallelFor(pass: Integer);
    var
      IO_: TAI_IO;
    begin
      LockObject(tmp_buff);
      IO_ := tmp_buff[pass];
      UnLockObject(tmp_buff);

      try
        IO_.ProcessBefore(ThSender.UserData);
        tmp_buff_state[pass] := IO_.Process(ThSender.UserData);
        IO_.ProcessAfter(ThSender.UserData);
      except
          tmp_buff_state[pass] := False;
      end;
    end;
{$ENDIF FPC}

  var
    i: Integer;
  begin
    tmp_buff := pickBuff;
    SetLength(tmp_buff_state, tmp_buff.Count);

{$IFDEF FPC}
    FPCParallelFor(@Nested_ParallelFor, 0, tmp_buff.Count - 1);
{$ELSE FPC}
    DelphiParallelFor(0, tmp_buff.Count - 1, procedure(pass: Integer)
      var
        IO_: TAI_IO;
      begin
        LockObject(tmp_buff);
        IO_ := tmp_buff[pass];
        UnLockObject(tmp_buff);

        try
          IO_.ProcessBefore(ThSender.UserData);
          tmp_buff_state[pass] := IO_.Process(ThSender.UserData);
          IO_.ProcessAfter(ThSender.UserData);
        except
            tmp_buff_state[pass] := False;
        end;
      end);
{$ENDIF FPC}
    LockObject(FOutputBuffer);
    for i := 0 to tmp_buff.Count - 1 do
      begin
        if tmp_buff_state[i] then
            FOutputBuffer.Add(tmp_buff[i])
        else
            DisposeObject(tmp_buff[i]);
      end;
    UnLockObject(FOutputBuffer);
    SetLength(tmp_buff_state, 0);
  end;
{$ENDIF Parallel}


var
  pickList: TAI_IO_Buffer;
begin
  AtomInc(IOProcessorActivtedThreadNum);
  while (InputCount > 0) do
    begin
      pickList := DoPickBuff();
{$IFDEF Parallel}
      if FParallelProcessor then
          DoParallelProcessPick(pickList)
      else
{$ENDIF Parallel}
          DoProcessPick(pickList);
      DisposeObject(pickList);
    end;
  FIOThreadRuning.V := False;
  AtomDec(IOProcessorActivtedThreadNum);
end;

constructor TAI_IO_Processor.Create(IO_Class_: TAI_IO_Class);
begin
  inherited Create;
  FIO_Class := IO_Class_;
  FInputBuffer := TAI_IO_Buffer.Create;
  FOutputBuffer := TAI_IO_Buffer.Create;
  FIOThreadRuning := TAtomBool.Create(False);
  FParallelProcessor := False;
  FIndexNumber := 0;

  FAI := nil;
  FAIPool := nil;
end;

destructor TAI_IO_Processor.Destroy;
begin
  while FIOThreadRuning.V do
      TCompute.Sleep(1);
  DisposeObject(FIOThreadRuning);

  Clear;
  DisposeObject(FInputBuffer);
  DisposeObject(FOutputBuffer);
  inherited Destroy;
end;

procedure TAI_IO_Processor.Clear;
var
  i: Integer;
begin
  LockInputBuffer;
  LockObject(FOutputBuffer);

  for i := 0 to FInputBuffer.Count - 1 do
      DisposeObject(FInputBuffer[i]);
  FInputBuffer.Clear;

  for i := 0 to FOutputBuffer.Count - 1 do
      DisposeObject(FOutputBuffer[i]);
  FOutputBuffer.Clear;

  UnLockInputBuffer;
  UnLockObject(FOutputBuffer);
end;

procedure TAI_IO_Processor.InputPicture(fileName: U_String);
begin
  Input(NewRasterFromFile(fileName), True);
end;

procedure TAI_IO_Processor.InputPicture(stream: TCoreClassStream);
begin
  Input(NewRasterFromStream(stream), True);
end;

procedure TAI_IO_Processor.Input(Raster: TMemoryRaster; RasterInstance_: Boolean);
var
  vio: TAI_IO;
begin
  vio := FIO_Class.Create(Self);

  if RasterInstance_ then
    begin
      if vio.InputRaster <> nil then
          DisposeObjectAndNil(vio.InputRaster);
      vio.InputRaster := Raster;
    end
  else
      vio.InputRaster.Assign(Raster);

  LockInputBuffer;
  inc(FIndexNumber);
  vio.IndexNumber := FIndexNumber;
  FInputBuffer.Add(vio);
  UnLockInputBuffer;
end;

function TAI_IO_Processor.InputCount: Integer;
begin
  LockInputBuffer;
  Result := FInputBuffer.Count;
  UnLockInputBuffer;
end;

procedure TAI_IO_Processor.Process(UserData: Pointer);
begin
  if (FIOThreadRuning.V) or (InputCount = 0) then
      exit;

  FIOThreadRuning.V := True;
  TCompute.RunM(UserData, Self, {$IFDEF FPC}@{$ENDIF FPC}IOProcessorThreadRun);
end;

function TAI_IO_Processor.Finished: Boolean;
begin
  Result := (InputCount = 0) and (not FIOThreadRuning.V);
end;

procedure TAI_IO_Processor.WaitProcessDone;
begin
  while not Finished do
    begin
      DoStatus();
      TCompute.Sleep(1);
    end;
end;

procedure TAI_IO_Processor.RemoveFirstInput;
begin
  LockInputBuffer;
  if FInputBuffer.Count > 0 then
    begin
      DisposeObject(FInputBuffer[0]);
      FInputBuffer.Delete(0);
    end;
  UnLockInputBuffer;
end;

function TAI_IO_Processor.LockOutputBuffer: TAI_IO_Buffer;
begin
  LockObject(FOutputBuffer);
  Result := FOutputBuffer;
end;

procedure TAI_IO_Processor.UnLockOutputBuffer(freeObj_: Boolean);
var
  i: Integer;
begin
  if freeObj_ then
    begin
      for i := 0 to FOutputBuffer.Count - 1 do
          DisposeObject(FOutputBuffer[i]);
      FOutputBuffer.Clear;
    end;
  UnLockObject(FOutputBuffer);
end;

procedure TAI_IO_Processor.UnLockOutputBuffer;
begin
  UnLockOutputBuffer(False);
end;

initialization

Init_AI_BuildIn;
KeepPerformanceOnTraining := 0;
LargeScaleTrainingMemoryRecycleTime := C_Tick_Second * 5;
IOProcessorActivtedThreadNum := 0;

finalization

Free_AI_BuildIn;

end.
