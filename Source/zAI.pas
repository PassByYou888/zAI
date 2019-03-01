{ ****************************************************************************** }
{ * AI Support(platform: cuda)                                                 * }
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
unit zAI;

{$INCLUDE zDefine.inc}

(* cuda-nvcc compatible record packing *)
{$IFDEF FPC}
{$PACKENUM 4}    (* use 4-byte enums *)
{$PACKRECORDS C}
{$ELSE}
{$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

interface

uses Types,
  CoreClasses,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ELSE FPC}
  System.IOUtils,
{$ENDIF FPC}
  PascalStrings, MemoryStream64, UnicodeMixedLib, DataFrameEngine, ListEngine,
  ZDBEngine, ZDBLocalManager, ObjectDataManager, ObjectData, ItemStream,
  zDrawEngine, Geometry2DUnit, MemoryRaster, LearnTypes, Learn, KDTree, PyramidSpace,
  zAI_Common, zAI_TrainingTask, zAI_KeyIO;

type
{$REGION 'BaseDefine'}
  PAI_Entry = ^TAI_Entry;

  TRGB_Image_Handle = Pointer;
  TMatrix_Image_Handle = Pointer;
  TOD_Handle = Pointer;
  TOD_Marshal_Handle = THashList;
  TSP_Handle = Pointer;
  TFACE_Handle = Pointer;
  TMDNN_Handle = Pointer;
  TMMOD_Handle = Pointer;
  TRNIC_Handle = Pointer;
  TTracker_Handle = Pointer;

  TBGRA_Image_Buffer_ = packed record
    bits: Pointer;
    width, height: Integer;
  end;

  TBGRA_Buffer_Handle = ^TBGRA_Image_Buffer_;

  TSurf_Desc = packed record
    x, y, dx, dy: Integer;
    desc: array [0 .. 63] of Single;
  end;

  PSurf_Desc = ^TSurf_Desc;

  TSurf_DescBuffer = packed array of TSurf_Desc;

  PSurfMatched = ^TSurfMatched;

  TSurfMatched = record
    sd1, sd2: PSurf_Desc;
    r1, r2: TMemoryRaster;
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

  TAI_Rect_Desc = packed array of TAI_Rect;

  TOD_Rect = packed record
    Left, Top, Right, Bottom: Integer;
    confidence: Double;
  end;

  POD_Rect = ^TOD_Rect;

  TOD_Desc = packed array of TOD_Rect;

  TOD_Marshal_Rect = record
    R: TRectV2;
    confidence: Double;
    Token: TPascalString;
  end;

  TOD_Marshal_Desc = packed array of TOD_Marshal_Rect;

{$IFDEF FPC}
  TOD_List_Decl = specialize TGenericsList<TOD_Rect>;
  TOD_Marshal_List_Decl = specialize TGenericsList<TOD_Marshal_Rect>;
{$ELSE FPC}
  TOD_List_Decl = TGenericsList<TOD_Rect>;
  TOD_Marshal_List_Decl = TGenericsList<TOD_Marshal_Rect>;
{$ENDIF FPC}
  TOD_List = TOD_List_Decl;

  TOD_Marshal_List = TOD_Marshal_List_Decl;

  PAI_Point = ^TAI_Point;

  TAI_Point = packed record
    x, y: Integer;
  end;

  TSP_Desc = packed array of TAI_Point;

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

  TAI_MMOD_Desc = packed array of TAI_MMOD_Rect;

  TMMOD_Rect = record
    R: TRectV2;
    confidence: Double;
    Token: TPascalString;
  end;

  TMMOD_Desc = packed array of TMMOD_Rect;

  TAI_Raster_Data = packed record
    raster_ptr: PRasterColorArray;
    width, height, index: Integer;
  end;

  PAI_Raster_Data = ^TAI_Raster_Data;

  TAI_Raster_Data_Array = array [0 .. (MaxInt div SizeOf(PAI_Raster_Data)) - 1] of PAI_Raster_Data;
  PAI_Raster_Data_Array = ^TAI_Raster_Data_Array;

  TMetric_ResNet_Train_Parameter = packed record
    // input
    imgArry_ptr: PAI_Raster_Data_Array;
    img_num: Integer;
    train_sync_file, train_output: P_Bytes;
    // train param
    timeout: UInt64;
    weight_decay, momentum: Single;
    iterations_without_progress_threshold: Integer;
    learning_rate, completed_learning_rate: Double;
    step_mini_batch_target_num, step_mini_batch_raster_num: Integer;
    // progress control
    control: PTrainingControl;
    // training result
    training_average_loss, training_learning_rate: Double;
    // full gpu
    fullGPU_Training: Boolean;
  end;

  PMetric_ResNet_Train_Parameter = ^TMetric_ResNet_Train_Parameter;

  TMMOD_Train_Parameter = packed record
    // input data
    train_cfg, train_sync_file, train_output: P_Bytes;
    // train param
    timeout: UInt64;
    target_size, min_target_size: Integer;
    min_detector_window_overlap_iou: Double;
    iterations_without_progress_threshold: Integer;
    learning_rate, completed_learning_rate: Double;
    // cropper param
    num_crops: Integer;
    chip_dims_x, chip_dims_y: Integer;
    min_object_size_x, min_object_size_y: Integer;
    max_rotation_degrees, max_object_size: Double;
    // progress control
    control: PTrainingControl;
    // training result
    training_average_loss, training_learning_rate: Double;
    // internal
    TempFiles: TPascalStringList;
  end;

  PMMOD_Train_Parameter = ^TMMOD_Train_Parameter;

  TRNIC_Train_Parameter = packed record
    // input data
    imgArry_ptr: PAI_Raster_Data_Array;
    img_num: Integer;
    train_sync_file, train_output: P_Bytes;
    // train param
    timeout: UInt64;
    weight_decay, momentum: Double;
    iterations_without_progress_threshold: Integer;
    learning_rate, completed_learning_rate: Double;
    all_bn_running_stats_window_sizes: Integer;
    img_mini_batch: Integer;
    // progress control
    control: PTrainingControl;
    // training result
    training_average_loss, training_learning_rate: Double;
  end;

  PRNIC_Train_Parameter = ^TRNIC_Train_Parameter;

  TOneStep = packed record
    StepTime: TDateTime;
    one_step_calls: UInt64;
    average_loss: Double;
    learning_rate: Double;
  end;

  POneStep = ^TOneStep;

  TAI_Log = record
    LogTime: TDateTime;
    LogText: SystemString;
  end;

{$IFDEF FPC}

  TOneStepList_Decl = specialize TGenericsList<POneStep>;
  TAI_LogList_Decl = specialize TGenericsList<TAI_Log>;
{$ELSE FPC}
  TOneStepList_Decl = TGenericsList<POneStep>;
  TAI_LogList_Decl = TGenericsList<TAI_Log>;
{$ENDIF FPC}

  TOneStepList = class(TOneStepList_Decl)
  private
    Critical: TSoftCritical;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Delete(index: Integer);
    procedure Clear;
    procedure AddStep(one_step_calls: UInt64; average_loss, learning_rate: Double);

    procedure SaveToStream(stream: TMemoryStream64);
    procedure LoadFromStream(stream: TMemoryStream64);
  end;

  TAI_LogList = class(TAI_LogList_Decl)
  end;

  TAI = class;

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

  TAlignment_ScaleSpace = class(TAlignment)
  public
    SS_width, SS_height: Integer;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_OD = class(TAlignment)
  public
    od_hnd: TOD_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_FastOD = class(TAlignment)
  public
    od_hnd: TOD_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_OD_Marshal = class(TAlignment)
  public
    od_hnd: TOD_Marshal_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_FastOD_Marshal = class(TAlignment)
  public
    od_hnd: TOD_Marshal_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_SP = class(TAlignment)
  public
    sp_hnd: TSP_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_MMOD = class(TAlignment)
  public
    MMOD_hnd: TMMOD_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAlignment_FastMMOD = class(TAlignment)
  public
    MMOD_hnd: TMMOD_Handle;
    procedure Alignment(imgList: TAI_ImageList); override;
  end;

  TAI_Parallel = class;

{$ENDREGION 'BaseDefine'}

  TAI_Entry = packed record
    // prepare image
    Prepare_RGB_Image: function(const raster_ptr: PRasterColorArray; const width, height: Integer): TRGB_Image_Handle; stdcall;
    Prepare_Matrix_Image: function(const raster_ptr: PRasterColorArray; const width, height: Integer): TMatrix_Image_Handle; stdcall;
    Close_RGB_Image: procedure(img: TRGB_Image_Handle); stdcall;
    Close_Matrix_Image: procedure(img: TMatrix_Image_Handle); stdcall;

    // image buffer
    OpenImageBuffer_RGB: function(hnd: TRGB_Image_Handle): TBGRA_Buffer_Handle; stdcall;
    OpenImageBuffer_MatrixRGB: function(hnd: TMatrix_Image_Handle): TBGRA_Buffer_Handle; stdcall;
    OpenImageBuffer_Hot: function(const raster_ptr: PRasterColorArray; const width, height: Integer): TBGRA_Buffer_Handle; stdcall;
    OpenImageBuffer_Jet: function(const raster_ptr: PRasterColorArray; const width, height: Integer): TBGRA_Buffer_Handle; stdcall;
    CloseImageBuffer: procedure(hnd: TBGRA_Buffer_Handle); stdcall;

    // surf detector
    fast_surf: function(const raster_ptr: PRasterColorArray; const width, height: Integer;
      const max_points: Integer; const detection_threshold: Double; const output: PSurf_Desc): Integer; stdcall;

    // object detector
    OD_Train: function(train_cfg, train_output: P_Bytes; window_w, window_h, thread_num: Integer): Integer; stdcall;
    OD_Init: function(train_data: P_Bytes): TOD_Handle; stdcall;
    OD_Init_Memory: function(memory: Pointer; Size: Integer): TOD_Handle; stdcall;
    OD_Free: function(hnd: TOD_Handle): Integer; stdcall;
    OD_Process: function(hnd: TOD_Handle; const raster_ptr: PRasterColorArray; const width, height: Integer;
      const OD_Rect: POD_Rect; const max_OD_Rect: Integer; var OD_Rect_num: Integer): Integer; stdcall;
    OD_Process_Image: function(hnd: TOD_Handle; rgb_img: TRGB_Image_Handle;
      const OD_Rect: POD_Rect; const max_OD_Rect: Integer; var OD_Rect_num: Integer): Integer; stdcall;

    // shape predictor and shape detector
    SP_Train: function(train_cfg, train_output: P_Bytes; oversampling_amount, tree_depth, thread_num: Integer): Integer; stdcall;
    SP_Init: function(train_data: P_Bytes): TSP_Handle; stdcall;
    SP_Init_Memory: function(memory: Pointer; Size: Integer): TSP_Handle; stdcall;
    SP_Free: function(hnd: TSP_Handle): Integer; stdcall;
    SP_Process: function(hnd: TSP_Handle; const raster_ptr: PRasterColorArray; const width, height: Integer;
      const AI_Rect: PAI_Rect; const AI_Point: PAI_Point; const max_AI_Point: Integer; var AI_Point_num: Integer): Integer; stdcall;
    SP_Process_Image: function(hnd: TSP_Handle; rgb_img: TRGB_Image_Handle;
      const AI_Rect: PAI_Rect; const AI_Point: PAI_Point; const max_AI_Point: Integer; var AI_Point_num: Integer): Integer; stdcall;

    // face recognition shape predictor
    SP_extract_face_rect_desc_chips: function(hnd: TSP_Handle; const raster_ptr: PRasterColorArray; const width, height, extract_face_size: Integer; rect_desc_: PAI_Rect; rect_num: Integer): TFACE_Handle; stdcall;
    SP_extract_face_rect_chips: function(hnd: TSP_Handle; const raster_ptr: PRasterColorArray; const width, height, extract_face_size: Integer): TFACE_Handle; stdcall;
    SP_extract_face_rect: function(const raster_ptr: PRasterColorArray; const width, height: Integer): TFACE_Handle; stdcall;
    SP_close_face_chips_handle: procedure(hnd: TFACE_Handle); stdcall;
    SP_get_face_chips_num: function(hnd: TFACE_Handle): Integer; stdcall;
    SP_get_face_chips_size: procedure(hnd: TFACE_Handle; const index: Integer; var width, height: Integer); stdcall;
    SP_get_face_chips_bits: procedure(hnd: TFACE_Handle; const index: Integer; const raster_ptr: PRasterColorArray); stdcall;
    SP_get_face_rect_num: function(hnd: TFACE_Handle): Integer; stdcall;
    SP_get_face_rect: procedure(hnd: TFACE_Handle; const index: Integer; var AI_Rect: TAI_Rect); stdcall;
    SP_get_num: function(hnd: TFACE_Handle): Integer; stdcall;
    SP_get: function(hnd: TFACE_Handle; const index: Integer; const AI_Point: PAI_Point; const max_AI_Point: Integer): Integer; stdcall;

    // MDNN-ResNet(ResNet matric DNN)
    MDNN_ResNet_Train: function(param: PMetric_ResNet_Train_Parameter): Integer; stdcall;
    MDNN_ResNet_Full_GPU_Train: function(param: PMetric_ResNet_Train_Parameter): Integer; stdcall;
    MDNN_ResNet_Init: function(train_data: P_Bytes): TMDNN_Handle; stdcall;
    MDNN_ResNet_Init_Memory: function(memory: Pointer; Size: Integer): TMDNN_Handle; stdcall;
    MDNN_ResNet_Free: function(hnd: TMDNN_Handle): Integer; stdcall;
    MDNN_ResNet_Process: function(hnd: TMDNN_Handle; imgArry_ptr: PAI_Raster_Data_Array; img_num: Integer; output: PDouble): Integer; stdcall;
    MDNN_DebugInfo: procedure(hnd: TMDNN_Handle; var p: PPascalString); stdcall;

    // MMOD-DNN(max-margin DNN object detector)
    MMOD_DNN_Train: function(param: PMMOD_Train_Parameter): Integer; stdcall;
    MMOD_DNN_Init: function(train_data: P_Bytes): TMMOD_Handle; stdcall;
    MMOD_DNN_Init_Memory: function(memory: Pointer; Size: Integer): TMMOD_Handle; stdcall;
    MMOD_DNN_Free: function(hnd: TMMOD_Handle): Integer; stdcall;
    MMOD_DNN_Process: function(hnd: TMMOD_Handle; const raster_ptr: PRasterColorArray; const width, height: Integer;
      const MMOD_AI_Rect: PAI_MMOD_Rect; const max_AI_Rect: Integer): Integer; stdcall;
    MMOD_DNN_Process_Image: function(hnd: TMMOD_Handle; matrix_img: TMatrix_Image_Handle;
      const MMOD_AI_Rect: PAI_MMOD_Rect; const max_AI_Rect: Integer): Integer; stdcall;
    MMOD_DebugInfo: procedure(hnd: TMMOD_Handle; var p: PPascalString); stdcall;

    // ResNet-Image-Classifier
    RNIC_Train: function(param: PRNIC_Train_Parameter): Integer; stdcall;
    RNIC_Init: function(train_data: P_Bytes): TRNIC_Handle; stdcall;
    RNIC_Init_Memory: function(memory: Pointer; Size: Integer): TRNIC_Handle; stdcall;
    RNIC_Free: function(hnd: TRNIC_Handle): Integer; stdcall;
    RNIC_Process: function(hnd: TRNIC_Handle; num_crops: Integer; const raster_ptr: PRasterColorArray; const width, height: Integer; output: PDouble): Integer; stdcall;
    RNIC_DebugInfo: procedure(hnd: TRNIC_Handle; var p: PPascalString); stdcall;

    // video tracker
    Start_Tracker: function(rgb_img: TRGB_Image_Handle; AI_Rect: PAI_Rect): TTracker_Handle; stdcall;
    Update_Tracker: function(hnd: TTracker_Handle; rgb_img: TRGB_Image_Handle; var AI_Rect: TAI_Rect): Double; stdcall;
    Stop_Tracker: function(hnd: TTracker_Handle): Integer; stdcall;

    // check key
    CheckKey: function(): Integer; stdcall;
    // close ai entry
    CloseAI: procedure(); stdcall;

    // backcall api
    API_OnOneStep: procedure(Sender: PAI_Entry; one_step_calls: UInt64; average_loss, learning_rate: Double); stdcall;
    API_OnPause: procedure(); stdcall;
    API_Status_Out: procedure(Sender: PAI_Entry; i_char: Integer); stdcall;
    API_GetTimeTick64: function(): UInt64; stdcall;
    API_BuildString: function(p: Pointer; Size: Integer): Pointer; stdcall;
    API_FreeString: procedure(p: Pointer); stdcall;

    // version information
    MajorVer, MinorVer: Integer;
    // Key information
    Key: TAI_Key;

    // internal
    LibraryFile: TPascalString;
    LoadLibraryTime: TDateTime;
    OneStepList: TOneStepList;
    Log: TAI_LogList;
  end;

  TAI = class(TCoreClassObject)
  protected
    // internal
    AI_Ptr: PAI_Entry;
    face_sp_hnd: TSP_Handle;
    TrainingControl: TTrainingControl;
    Critical: TSoftCritical;
  public
    // parallel handle
    Parallel_OD_Hnd: TOD_Handle;
    Parallel_OD_Marshal_Hnd: TOD_Marshal_Handle;
    Parallel_SP_Hnd: TSP_Handle;

    // root path
    rootPath: SystemString;

    // deep neural network training state
    Last_training_average_loss, Last_training_learning_rate: Double;

    constructor Create;
    class function OpenEngine(libFile: SystemString): TAI; overload;
    class function OpenEngine(lib_p: PAI_Entry): TAI; overload;
    class function OpenEngine: TAI; overload;
    destructor Destroy; override;

    // structor draw
    procedure DrawOD(od_hnd: TOD_Handle; Raster: TMemoryRaster; color: TDEColor); overload;
    procedure DrawOD(od_desc: TOD_Desc; Raster: TMemoryRaster; color: TDEColor); overload;
    procedure DrawODM(odm_hnd: TOD_Marshal_Handle; Raster: TMemoryRaster; color: TDEColor);
    procedure DrawSP(od_hnd: TOD_Handle; sp_hnd: TSP_Handle; Raster: TMemoryRaster);
    function DrawMMOD(MMOD_hnd: TMMOD_Handle; Raster: TMemoryRaster; color: TDEColor): TMMOD_Desc;
    procedure DrawFace(Raster: TMemoryRaster); overload;
    procedure DrawFace(face_hnd: TFACE_Handle; d: TDrawEngine); overload;
    procedure DrawFace(Raster: TMemoryRaster; mdnn_hnd: TMDNN_Handle; Face_Learn: TLearn; faceAccuracy: TGeoFloat; lineColor, TextColor: TDEColor); overload;
    procedure PrintFace(prefix: SystemString; Raster: TMemoryRaster; mdnn_hnd: TMDNN_Handle; Face_Learn: TLearn; faceAccuracy: TGeoFloat);
    function DrawExtractFace(Raster: TMemoryRaster): TMemoryRaster;

    // atomic lock ctrl
    procedure Lock;
    procedure Unlock;
    function Busy: Boolean;

    // training control
    procedure Training_Stop;
    procedure Training_Pause;
    procedure Training_Continue;

    // prepare image
    function Prepare_RGB_Image(Raster: TMemoryRaster): TRGB_Image_Handle;
    procedure Close_RGB_Image(hnd: TRGB_Image_Handle);
    function Prepare_Matrix_Image(Raster: TMemoryRaster): TMatrix_Image_Handle;
    procedure Close_Matrix_Image(hnd: TMatrix_Image_Handle);

    // Medical graphic support
    procedure HotMap(Raster: TMemoryRaster);
    procedure JetMap(Raster: TMemoryRaster);
    function BuildHotMap(Raster: TMemoryRaster): TMemoryRaster;
    function BuildJetMap(Raster: TMemoryRaster): TMemoryRaster;

    // fast surf(cpu)
    function fast_surf(Raster: TMemoryRaster; const max_points: Integer; const detection_threshold: Double): TSurf_DescBuffer;
    function surf_sqr(const sour, dest: PSurf_Desc): Single; inline;
    function Surf_Matched(reject_ratio_sqr: Single; r1_, r2_: TMemoryRaster; sd1_, sd2_: TSurf_DescBuffer): TSurfMatchedBuffer;
    procedure BuildFeatureView(Raster: TMemoryRaster; descbuff: TSurf_DescBuffer);
    function BuildMatchInfoView(var MatchInfo: TSurfMatchedBuffer): TMemoryRaster;
    function BuildSurfMatchOutput(raster1, raster2: TMemoryRaster): TMemoryRaster;

    // object detector training(cpu)
    function OD_Train(train_cfg, train_output: TPascalString; window_w, window_h, thread_num: Integer): Boolean; overload;
    function OD_Train(imgList: TAI_ImageList; TokenFilter, train_output: TPascalString; window_w, window_h, thread_num: Integer): Boolean; overload;
    function OD_Train(imgMat: TAI_ImageMatrix; TokenFilter, train_output: TPascalString; window_w, window_h, thread_num: Integer): Boolean; overload;
    function OD_Train_Stream(imgList: TAI_ImageList; window_w, window_h, thread_num: Integer): TMemoryStream64; overload;
    function OD_Train_Stream(imgMat: TAI_ImageMatrix; window_w, window_h, thread_num: Integer): TMemoryStream64; overload;
    // object detector api(cpu)
    function OD_Open(train_file: TPascalString): TOD_Handle;
    function OD_Open_Stream(stream: TMemoryStream64): TOD_Handle; overload;
    function OD_Open_Stream(train_file: TPascalString): TOD_Handle; overload;
    function OD_Close(var hnd: TOD_Handle): Boolean;
    function OD_Process(hnd: TOD_Handle; Raster: TMemoryRaster; const max_AI_Rect: Integer): TOD_Desc; overload;
    function OD_Process(hnd: TOD_Handle; Raster: TMemoryRaster): TOD_List; overload;
    procedure OD_Process(hnd: TOD_Handle; Raster: TMemoryRaster; output: TOD_List); overload;
    function OD_Process(hnd: TOD_Handle; rgb_img: TRGB_Image_Handle; const max_AI_Rect: Integer): TOD_Desc; overload;
    function OD_ProcessScaleSpace(hnd: TOD_Handle; Raster: TMemoryRaster; scale: TGeoFloat): TOD_Desc; overload;

    // object marshal detector training(cpu)
    function OD_Marshal_Train(imgList: TAI_ImageList; window_w, window_h, thread_num: Integer): TMemoryStream64; overload;
    function OD_Marshal_Train(imgMat: TAI_ImageMatrix; window_w, window_h, thread_num: Integer): TMemoryStream64; overload;
    // object marshal detector api(cpu)
    function OD_Marshal_Open_Stream(stream: TMemoryStream64): TOD_Marshal_Handle; overload;
    function OD_Marshal_Open_Stream(train_file: TPascalString): TOD_Marshal_Handle; overload;
    function OD_Marshal_Close(var hnd: TOD_Marshal_Handle): Boolean;
    function OD_Marshal_Process(hnd: TOD_Marshal_Handle; Raster: TMemoryRaster): TOD_Marshal_Desc;
    function OD_Marshal_ProcessScaleSpace(hnd: TOD_Marshal_Handle; Raster: TMemoryRaster; scale: TGeoFloat): TOD_Marshal_Desc;

    // shape predictor and shape detector training(cpu)
    function SP_Train(train_cfg, train_output: TPascalString; oversampling_amount, tree_depth, thread_num: Integer): Boolean; overload;
    function SP_Train(imgList: TAI_ImageList; train_output: TPascalString; oversampling_amount, tree_depth, thread_num: Integer): Boolean; overload;
    function SP_Train(imgMat: TAI_ImageMatrix; train_output: TPascalString; oversampling_amount, tree_depth, thread_num: Integer): Boolean; overload;
    function SP_Train_Stream(imgList: TAI_ImageList; oversampling_amount, tree_depth, thread_num: Integer): TMemoryStream64; overload;
    function SP_Train_Stream(imgMat: TAI_ImageMatrix; oversampling_amount, tree_depth, thread_num: Integer): TMemoryStream64; overload;
    // shape predictor and shape detector api(cpu)
    function SP_Open(train_file: TPascalString): TSP_Handle;
    function SP_Open_Stream(stream: TMemoryStream64): TSP_Handle; overload;
    function SP_Open_Stream(train_file: TPascalString): TSP_Handle; overload;
    function SP_Close(var hnd: TSP_Handle): Boolean;
    function SP_Process(hnd: TSP_Handle; Raster: TMemoryRaster; const AI_Rect: TAI_Rect; const max_AI_Point: Integer): TSP_Desc;
    function SP_Process_Vec2List(hnd: TSP_Handle; Raster: TMemoryRaster; const R: TRectV2): TVec2List;
    function SP_Process_Vec2(hnd: TSP_Handle; Raster: TMemoryRaster; const R: TRectV2): TArrayVec2; overload;
    function SP_Process_Vec2(hnd: TSP_Handle; Raster: TMemoryRaster; const R: TAI_Rect): TArrayVec2; overload;
    function SP_Process_Vec2(hnd: TSP_Handle; Raster: TMemoryRaster; const R: TOD_Rect): TArrayVec2; overload;

    // face shape predictor(cpu)
    procedure PrepareFaceDataSource;
    function Face_Detector(Raster: TMemoryRaster; R: TRect; extract_face_size: Integer): TFACE_Handle; overload;
    function Face_Detector(Raster: TMemoryRaster; desc: TAI_Rect_Desc; extract_face_size: Integer): TFACE_Handle; overload;
    function Face_Detector(Raster: TMemoryRaster; mmod_desc: TMMOD_Desc; extract_face_size: Integer): TFACE_Handle; overload;
    function Face_Detector(Raster: TMemoryRaster; od_desc: TOD_Desc; extract_face_size: Integer): TFACE_Handle; overload;
    function Face_DetectorAsChips(Raster: TMemoryRaster; desc: TAI_Rect; extract_face_size: Integer): TMemoryRaster;
    function Face_Detector_All(Raster: TMemoryRaster): TFACE_Handle; overload;
    function Face_Detector_All(Raster: TMemoryRaster; extract_face_size: Integer): TFACE_Handle; overload;
    function Face_Detector_Rect(Raster: TMemoryRaster): TFACE_Handle;
    function Face_Detector_AllRect(Raster: TMemoryRaster): TAI_Rect_Desc;
    function Face_chips_num(hnd: TFACE_Handle): Integer;
    function Face_chips(hnd: TFACE_Handle; index: Integer): TMemoryRaster;
    function Face_Rect_Num(hnd: TFACE_Handle): Integer;
    function Face_Rect(hnd: TFACE_Handle; index: Integer): TAI_Rect;
    function Face_RectV2(hnd: TFACE_Handle; index: Integer): TRectV2;
    function Face_Shape_num(hnd: TFACE_Handle): Integer;
    function Face_Shape(hnd: TFACE_Handle; index: Integer): TSP_Desc;
    function Face_ShapeV2(hnd: TFACE_Handle; index: Integer): TArrayVec2;
    function Face_Shape_rect(hnd: TFACE_Handle; index: Integer): TRectV2;
    procedure Face_Close(var hnd: TFACE_Handle);

    // MDNN-ResNet(ResNet metric DNN) training(gpu)
    class function Init_Metric_ResNet_Parameter(train_sync_file, train_output: TPascalString): PMetric_ResNet_Train_Parameter;
    class procedure Free_Metric_ResNet_Parameter(param: PMetric_ResNet_Train_Parameter);
    function Metric_ResNet_Train(imgList: TMemoryRaster2DArray; param: PMetric_ResNet_Train_Parameter): Boolean; overload;
    function Metric_ResNet_Train(imgList: TAI_ImageList; param: PMetric_ResNet_Train_Parameter): Boolean; overload;
    function Metric_ResNet_Train_Stream(imgList: TAI_ImageList; param: PMetric_ResNet_Train_Parameter): TMemoryStream64; overload;
    function Metric_ResNet_Train(imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): Boolean; overload;
    function Metric_ResNet_Train_Stream(imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): TMemoryStream64; overload;
    // MDNN-ResNet(ResNet metric DNN) api(gpu)
    function Metric_ResNet_Open(train_file: TPascalString): TMDNN_Handle;
    function Metric_ResNet_Open_Stream(stream: TMemoryStream64): TMDNN_Handle; overload;
    function Metric_ResNet_Open_Stream(train_file: TPascalString): TMDNN_Handle; overload;
    function Metric_ResNet_Close(var hnd: TMDNN_Handle): Boolean;
    function Metric_ResNet_Process(hnd: TMDNN_Handle; RasterArray: TMemoryRasterArray; output: PDouble): Integer; overload;
    function Metric_ResNet_Process(hnd: TMDNN_Handle; RasterArray: TMemoryRasterArray): TLMatrix; overload;
    function Metric_ResNet_Process(hnd: TMDNN_Handle; Raster: TMemoryRaster): TLVec; overload;
    procedure Metric_ResNet_SaveDetectorDefineToLearnEngine(mdnn_hnd: TMDNN_Handle; imgList: TAI_ImageList; lr: TLearn); overload;
    procedure Metric_ResNet_SaveDetectorDefineToLearnEngine(mdnn_hnd: TMDNN_Handle; imgMat: TAI_ImageMatrix; lr: TLearn); overload;
    function Metric_ResNet_DebugInfo(hnd: TMDNN_Handle): TPascalString;

    // MMOD-DNN(DNN+SVM:max-margin object detector) training(gpu)
    class function Init_MMOD_DNN_TrainParam(train_cfg, train_sync_file, train_output: TPascalString): PMMOD_Train_Parameter;
    class procedure Free_MMOD_DNN_TrainParam(param: PMMOD_Train_Parameter);
    function MMOD_DNN_PrepareTrain(imgList: TAI_ImageList; train_sync_file: TPascalString): PMMOD_Train_Parameter; overload;
    function MMOD_DNN_PrepareTrain(imgMat: TAI_ImageMatrix; train_sync_file: TPascalString): PMMOD_Train_Parameter; overload;
    function MMOD_DNN_Train(param: PMMOD_Train_Parameter): Integer;
    function MMOD_DNN_Train_Stream(param: PMMOD_Train_Parameter): TMemoryStream64;
    procedure MMOD_DNN_FreeTrain(param: PMMOD_Train_Parameter);
    // MMOD-DNN(DNN+SVM:max-margin object detector) api(gpu)
    function MMOD_DNN_Open(train_file: TPascalString): TMMOD_Handle;
    function MMOD_DNN_Open_Stream(stream: TMemoryStream64): TMMOD_Handle; overload;
    function MMOD_DNN_Open_Stream(train_file: TPascalString): TMMOD_Handle; overload;
    function MMOD_DNN_Close(var hnd: TMMOD_Handle): Boolean;
    function MMOD_DNN_Process(hnd: TMMOD_Handle; Raster: TMemoryRaster): TMMOD_Desc; overload;
    function MMOD_DNN_Process_Matrix(hnd: TMMOD_Handle; matrix_img: TMatrix_Image_Handle): TMMOD_Desc; overload;
    function MMOD_DNN_DebugInfo(hnd: TMMOD_Handle): TPascalString;

    // ResNet-Image-Classifier training(gpu)
    class function Init_RNIC_Train_Parameter(train_sync_file, train_output: TPascalString): PRNIC_Train_Parameter;
    class procedure Free_RNIC_Train_Parameter(param: PRNIC_Train_Parameter);
    function RNIC_Train(imgList: TMemoryRaster2DArray; param: PRNIC_Train_Parameter; Train_OutputIndex: TMemoryRasterList): Boolean; overload;
    function RNIC_Train(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean; overload;
    function RNIC_Train(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; train_index_output: TPascalString): Boolean; overload;
    function RNIC_Train_Stream(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64; overload;
    function RNIC_Train(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean; overload;
    function RNIC_Train(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; train_index_output: TPascalString): Boolean; overload;
    function RNIC_Train_Stream(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): TMemoryStream64; overload;
    // ResNet-Image-Classifier api(gpu)
    function RNIC_Open(train_file: TPascalString): TRNIC_Handle;
    function RNIC_Open_Stream(stream: TMemoryStream64): TRNIC_Handle; overload;
    function RNIC_Open_Stream(train_file: TPascalString): TRNIC_Handle; overload;
    function RNIC_Close(var hnd: TRNIC_Handle): Boolean;
    function RNIC_Process(hnd: TRNIC_Handle; Raster: TMemoryRaster; num_crops: Integer): TLVec; overload;
    function RNIC_Process(hnd: TRNIC_Handle; Raster: TMemoryRaster): TLVec; overload;
    function RNIC_DebugInfo(hnd: TRNIC_Handle): TPascalString;

    // video tracker(cpu)
    function Tracker_Open(Raster: TMemoryRaster; const track_rect: TRect): TTracker_Handle; overload;
    function Tracker_Open(Raster: TMemoryRaster; const track_rect: TRectV2): TTracker_Handle; overload;
    function Tracker_Update(hnd: TTracker_Handle; Raster: TMemoryRaster; var track_rect: TRect): Double; overload;
    function Tracker_Update(hnd: TTracker_Handle; Raster: TMemoryRaster; var track_rect: TRectV2): Double; overload;
    function Tracker_Close(var hnd: TTracker_Handle): Boolean;

    // engine activted
    function Activted: Boolean;
  end;

{$IFDEF FPC}

  TAI_Parallel_Decl = specialize TGenericsList<TAI>;
{$ELSE FPC}
  TAI_Parallel_Decl = TGenericsObjectList<TAI>;
{$ENDIF FPC}

  TAI_Parallel = class(TAI_Parallel_Decl)
  private
    Critical: TSoftCritical;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(index: Integer);

    procedure Prepare_Parallel(eng: SystemString; poolSiz: Integer); overload;
    procedure Prepare_Parallel(lib_p: PAI_Entry; poolSiz: Integer); overload;
    procedure Prepare_Parallel(poolSiz: Integer); overload;
    procedure Prepare_Parallel; overload;

    procedure Prepare_Face;
    procedure Prepare_OD(stream: TMemoryStream64);
    procedure Prepare_OD_Marshal(stream: TMemoryStream64);
    procedure Prepare_SP(stream: TMemoryStream64);
    function GetAndLockAI: TAI;
    procedure UnLockAI(AI: TAI);
    function Busy: Integer;
  end;

const
  // core parameter
  C_Metric_ResNet_Image_Size: Integer = 150;
  C_Metric_ResNet_Dim: Integer = 256;
  C_ResNet_Image_Classifier_Dim: Integer = 1000;

procedure Wait_AI_Init;

function Load_ZAI(libFile: SystemString): PAI_Entry;
function Prepare_AI_Engine(eng: SystemString): PAI_Entry; overload;
function Prepare_AI_Engine: PAI_Entry; overload;
procedure Close_AI_Engine;

function Alloc_P_Bytes(const buff: TPascalString): P_Bytes; overload;
function Alloc_P_Bytes(const buff: TBytes): P_Bytes; overload;
procedure Free_P_Bytes(const buff: P_Bytes);
function Get_P_Bytes_String(const buff: P_Bytes): TPascalString;

function Rect(const v: TAI_Rect): TRect; overload;
function Rect(const v: TOD_Rect): TRect; overload;

function AIRect(const v: TRect): TAI_Rect; overload;
function AIRect(const v: TRectV2): TAI_Rect; overload;
function AIRect(const v: TOD_Rect): TAI_Rect; overload;
function AIRect(const v: TAI_MMOD_Rect): TAI_Rect; overload;

function RectV2(const v: TAI_Rect): TRectV2; overload;
function RectV2(const v: TOD_Rect): TRectV2; overload;
function RectV2(const v: TAI_MMOD_Rect): TRectV2; overload;

function point(const v: TAI_Point): TPoint; overload;
function Vec2(const v: TAI_Point): TVec2; overload;

function InRect(v: TAI_Point; R: TAI_Rect): Boolean; overload;
function InRect(v: TSP_Desc; R: TAI_Rect): Boolean; overload;
function InRect(v: TSP_Desc; R: TRectV2): Boolean; overload;

procedure SPToVec(v: TSP_Desc; l: TVec2List); overload;

function GetSPBound(desc: TSP_Desc; endge_threshold: TGeoFloat): TRectV2;
procedure DrawSPLine(sp_desc: TSP_Desc; bp, ep: Integer; closeLine: Boolean; color: TDEColor; d: TDrawEngine); overload;
procedure DrawFaceSP(sp_desc: TSP_Desc; color: TDEColor; d: TDrawEngine); overload;

// training task
function RunTrainingTask(Task: TTrainingTask; const AI: TAI; const paramFile: SystemString): Boolean;

var
  KeepPerformanceOnTraining: TTimeTick = 0;

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

{$IFDEF Z_AI_Dataset_Build_In}
{$RESOURCE zAI_BuildIn.RES}
{$ENDIF Z_AI_Dataset_Build_In}


var
  AI_Entry_Cache: THashList;
  AI_Status_Critical: TCritical;
  AI_Status_Buffer: TMemoryStream64;
  build_in_face_shape_memory: Pointer;
  build_in_face_shape_memory_siz: Int64;
  found_build_in: Boolean;

procedure AI_OnOneStep(Sender: PAI_Entry; one_step_calls: UInt64; average_loss, learning_rate: Double); stdcall;
var
  tk: TTimeTick;
begin
  try
    Sender^.OneStepList.AddStep(one_step_calls, average_loss, learning_rate);

    if KeepPerformanceOnTraining > 0 then
      begin
        tk := GetTimeTick() + KeepPerformanceOnTraining;
        while GetTimeTick() < tk do
            nop;
      end;
  except
  end;
end;

procedure AI_OnPause(); stdcall;
begin
end;

// i_char = unicode encoded char
procedure AI_StatusIO_Out(Sender: PAI_Entry; i_char: Integer); stdcall;
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

function AI_GetTimeTick64(): UInt64; stdcall;
begin
  Result := GetTimeTick();
end;

function AI_BuildString(p: Pointer; Size: Integer): Pointer; stdcall;
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

procedure AI_FreeString(p: Pointer); stdcall;
begin
  Dispose(PPascalString(p));
end;

function Load_ZAI(libFile: SystemString): PAI_Entry;
type
  TProc_Init_ai = procedure(var AI: TAI_Entry); stdcall;
var
  proc_init_ai_: TProc_Init_ai;
  AI_Ptr: PAI_Entry;
begin
  Result := nil;

  LockObject(AI_Entry_Cache);
  try

    if AI_Entry_Cache.Exists(libFile) then
      begin
        AI_Ptr := PAI_Entry(AI_Entry_Cache[libFile]);
        Result := AI_Ptr;
      end
    else
      begin
        try
            proc_init_ai_ := TProc_Init_ai(GetExtProc(libFile, 'init_api_entry'));
        except
          proc_init_ai_ := nil;
          FreeExtLib(libFile);
        end;
        if Assigned(proc_init_ai_) then
          begin
            new(AI_Ptr);
            FillPtrByte(AI_Ptr, SizeOf(TAI_Entry), 0);
            AI_Ptr^.API_OnOneStep := {$IFDEF FPC}@{$ENDIF FPC}AI_OnOneStep;
            AI_Ptr^.API_OnPause := {$IFDEF FPC}@{$ENDIF FPC}AI_OnPause;
            AI_Ptr^.API_Status_Out := {$IFDEF FPC}@{$ENDIF FPC}AI_StatusIO_Out;
            AI_Ptr^.API_GetTimeTick64 := {$IFDEF FPC}@{$ENDIF FPC}AI_GetTimeTick64;
            AI_Ptr^.API_BuildString := {$IFDEF FPC}@{$ENDIF FPC}AI_BuildString;
            AI_Ptr^.API_FreeString := {$IFDEF FPC}@{$ENDIF FPC}AI_FreeString;

            AI_Ptr^.LibraryFile := libFile;
            AI_Ptr^.LoadLibraryTime := umlNow();
            AI_Ptr^.OneStepList := TOneStepList.Create;
            AI_Ptr^.Log := TAI_LogList.Create;
            try
              proc_init_ai_(AI_Ptr^);

              if (AI_Ptr^.MajorVer = 1) and (AI_Ptr^.MinorVer = 15) then
                begin
                  AI_Ptr^.Key := AIKey(AI_Ptr^.Key);
                  if AI_Ptr^.CheckKey() > 0 then
                    begin
                      AI_Entry_Cache.Add(libFile, AI_Ptr, False);
                      Result := AI_Ptr;
                      DoStatus('prepare AI engine: %s', [libFile]);
                    end
                  else
                    begin
                      AI_Ptr^.LibraryFile := '';
                      DisposeObject(AI_Ptr^.OneStepList);
                      DisposeObject(AI_Ptr^.Log);
                      Dispose(AI_Ptr);
                      FreeExtLib(libFile);
                      DoStatus('illegal AI engine: %s', [libFile]);
                    end;
                end
              else
                begin
                  DoStatus('nonsupport AI engine edition: %d.%d ', [AI_Ptr^.MajorVer, AI_Ptr^.MinorVer]);
                  AI_Ptr^.LibraryFile := '';
                  DisposeObject(AI_Ptr^.OneStepList);
                  DisposeObject(AI_Ptr^.Log);
                  Dispose(AI_Ptr);
                  FreeExtLib(libFile);
                end;
            except
            end;
          end;
      end;
  finally
      UnLockObject(AI_Entry_Cache);
  end;
end;

procedure BuildIn_Thread_Run(Sender: TComputeThread);
var
  fn: U_String;
  stream: TCoreClassStream;
  m64: TMemoryStream64;
  dbEng: TObjectDataManager;
  itmHnd: TItemHandle;
  p: Pointer;
begin
{$IFDEF Z_AI_Dataset_Build_In}
  try
      stream := TCoreClassResourceStream.Create(HInstance, 'zAI_BuildIn', RT_RCDATA);
  except
    found_build_in := False;
    DoStatus('not found AI resource "zAI_BuildIn"');
    exit;
  end;
{$ELSE Z_AI_Dataset_Build_In}
  fn := umlCombineFileName(AI_Configure_Path, 'zAI_BuildIn.OXC');
  if not umlFileExists(fn) then
    begin
      found_build_in := False;
      DoStatus('not found AI dataset %s', [fn.Text]);
      exit;
    end;
  stream := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
{$ENDIF Z_AI_Dataset_Build_In}
  found_build_in := True;
  stream.Position := 0;
  m64 := TMemoryStream64.Create;
  DecompressStream(stream, m64);
  DisposeObject(stream);

  m64.Position := 0;
  dbEng := TObjectDataManagerOfCache.CreateAsStream(m64, '', DBMarshal.ID, True, False, True);

  if dbEng.ItemOpen('/', 'build_in_face_shape.dat', itmHnd) then
    begin
      build_in_face_shape_memory_siz := itmHnd.Item.Size;
      p := GetMemory(build_in_face_shape_memory_siz);
      dbEng.ItemRead(itmHnd, build_in_face_shape_memory_siz, p^);
      dbEng.ItemClose(itmHnd);
      build_in_face_shape_memory := p;
    end
  else
      RaiseInfo('zAI buildIn DB error.');

  DisposeObject(dbEng);
end;

procedure Init_AI_BuildIn;
begin
  AI_Status_Critical := TCritical.Create;
  AI_Entry_Cache := THashList.Create;
  AI_Entry_Cache.AutoFreeData := False;
  AI_Entry_Cache.AccessOptimization := False;
  AI_Status_Buffer := TMemoryStream64.CustomCreate(8192);

  build_in_face_shape_memory := nil;
  build_in_face_shape_memory_siz := 0;
  found_build_in := True;

  TComputeThread.RunC(nil, nil, {$IFDEF FPC}@{$ENDIF FPC}BuildIn_Thread_Run);
end;

procedure Wait_AI_Init;
begin
  while (found_build_in) and ((build_in_face_shape_memory = nil) or (build_in_face_shape_memory_siz = 0)) do
      CheckThreadSynchronize(10);
end;

procedure Free_AI_BuildIn;
begin
  Close_AI_Engine;
  DisposeObject(AI_Entry_Cache);
  DisposeObject(AI_Status_Buffer);
  DisposeObject(AI_Status_Critical);
  FreeMemory(build_in_face_shape_memory);
end;

function Prepare_AI_Engine(eng: SystemString): PAI_Entry;
begin
  Result := Load_ZAI(eng);
end;

function Prepare_AI_Engine: PAI_Entry;
begin
  Result := Prepare_AI_Engine(AI_Engine_Library);
end;

procedure Close_AI_Engine;
  procedure Free_ZAI(AI_Ptr: PAI_Entry);
  begin
    try
      AI_Ptr^.CloseAI();
      DisposeObject(AI_Ptr^.OneStepList);
      DisposeObject(AI_Ptr^.Log);
      Dispose(AI_Ptr);
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
          Free_ZAI(PAI_Entry(p^.data));
          p^.data := nil;
          FreeExtLib(p^.OriginName);
          inc(i);
          p := p^.Next;
        end;
    end;
  AI_Entry_Cache.Clear;
end;

function Alloc_P_Bytes(const buff: TPascalString): P_Bytes;
begin
  Result := Alloc_P_Bytes(buff.SysBytes);
end;

function Alloc_P_Bytes(const buff: TBytes): P_Bytes;
begin
  new(Result);
  Result^.Size := length(buff);
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
  Dispose(buff);
end;

function Get_P_Bytes_String(const buff: P_Bytes): TPascalString;
var
  tmp: TBytes;
begin
  SetLength(tmp, buff^.Size);
  if buff^.Size > 0 then
      CopyPtr(buff^.Bytes, @tmp[0], buff^.Size);
  Result.SysBytes := tmp;
  SetLength(tmp, 0);
end;

function Rect(const v: TAI_Rect): TRect;
begin
  Result.Left := v.Left;
  Result.Top := v.Top;
  Result.Right := v.Right;
  Result.Bottom := v.Bottom;
end;

function Rect(const v: TOD_Rect): TRect;
begin
  Result.Left := v.Left;
  Result.Top := v.Top;
  Result.Right := v.Right;
  Result.Bottom := v.Bottom;
end;

function AIRect(const v: TRect): TAI_Rect;
begin
  Result.Left := v.Left;
  Result.Top := v.Top;
  Result.Right := v.Right;
  Result.Bottom := v.Bottom;
end;

function AIRect(const v: TRectV2): TAI_Rect;
begin
  Result.Left := Round(v[0, 0]);
  Result.Top := Round(v[0, 1]);
  Result.Right := Round(v[1, 0]);
  Result.Bottom := Round(v[1, 1]);
end;

function AIRect(const v: TOD_Rect): TAI_Rect;
begin
  Result.Left := v.Left;
  Result.Top := v.Top;
  Result.Right := v.Right;
  Result.Bottom := v.Bottom;
end;

function AIRect(const v: TAI_MMOD_Rect): TAI_Rect;
begin
  Result.Left := v.Left;
  Result.Top := v.Top;
  Result.Right := v.Right;
  Result.Bottom := v.Bottom;
end;

function RectV2(const v: TAI_Rect): TRectV2;
begin
  Result[0, 0] := v.Left;
  Result[0, 1] := v.Top;
  Result[1, 0] := v.Right;
  Result[1, 1] := v.Bottom;
end;

function RectV2(const v: TOD_Rect): TRectV2;
begin
  Result[0, 0] := v.Left;
  Result[0, 1] := v.Top;
  Result[1, 0] := v.Right;
  Result[1, 1] := v.Bottom;
end;

function RectV2(const v: TAI_MMOD_Rect): TRectV2;
begin
  Result[0, 0] := v.Left;
  Result[0, 1] := v.Top;
  Result[1, 0] := v.Right;
  Result[1, 1] := v.Bottom;
end;

function point(const v: TAI_Point): TPoint;
begin
  Result.x := v.x;
  Result.y := v.y;
end;

function Vec2(const v: TAI_Point): TVec2;
begin
  Result[0] := v.x;
  Result[1] := v.y;
end;

function InRect(v: TAI_Point; R: TAI_Rect): Boolean;
begin
  Result := PointInRect(Vec2(v), RectV2(R));
end;

function InRect(v: TSP_Desc; R: TAI_Rect): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to length(v) - 1 do
    if not InRect(v[i], R) then
        exit;
  Result := True;
end;

function InRect(v: TSP_Desc; R: TRectV2): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to length(v) - 1 do
    if not PointInRect(Vec2(v[i]), R) then
        exit;
  Result := True;
end;

procedure SPToVec(v: TSP_Desc; l: TVec2List);
var
  i: Integer;
begin
  for i := 0 to length(v) - 1 do
      l.Add(Vec2(v[i]));
end;

function GetSPBound(desc: TSP_Desc; endge_threshold: TGeoFloat): TRectV2;
var
  vbuff: TArrayVec2;
  i: Integer;
  siz: TVec2;
begin
  if length(desc) = 0 then
    begin
      Result := NullRect;
      exit;
    end;
  SetLength(vbuff, length(desc));
  for i := 0 to length(desc) - 1 do
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

  d.DrawPL(20, vl, closeLine, color, 2);
  DisposeObject(vl);
end;

procedure DrawFaceSP(sp_desc: TSP_Desc; color: TDEColor; d: TDrawEngine);
begin
  if length(sp_desc) <> 68 then
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

function RunTrainingTask(Task: TTrainingTask; const AI: TAI; const paramFile: SystemString): Boolean;
var
  i: Integer;

  param: THashVariantList;
  ComputeFunc: SystemString;

  param_md5: TMD5;

  // batch free
  inputfile1, inputfile2: SystemString;
  inputstream1, inputstream2: TMemoryStream64;
  inputraster1, inputraster2: TMemoryRaster;
  inputImgList: TAI_ImageList;
  inputImgMatrix: TAI_ImageMatrix;
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
  tmpPSL: TPascalStringList;
  tmpM64: TMemoryStream64;
  output_learn_file: SystemString;
  learnEng: TLearn;
  mdnn_hnd: TMDNN_Handle;
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

  DoStatus('input training parameter.');
  DoStatus(param.AsText);

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

  try
    if umlMultipleMatch(['surf', 'fastsurf'], ComputeFunc) then
      begin
        inputfile1 := param.GetDefaultValue('source', '');
        inputfile2 := param.GetDefaultValue('dest', '');

        if Task.Exists(inputfile1) and Task.Exists(inputfile2) then
          begin
            try
              Task.Read(inputfile1, inputraster1);
              Task.Read(inputfile2, inputraster2);
              inputraster1.scale(param.GetDefaultValue('scale', 1.0));
              inputraster2.scale(param.GetDefaultValue('scale', 1.0));
              outputraster := AI.BuildSurfMatchOutput(inputraster1, inputraster2);

              Task.write(param.GetDefaultValue('output', 'output.bmp'), outputraster);
              DisposeObject(outputraster);
              Result := True;
            except
            end;
          end;
      end
    else if umlMultipleMatch(['TrainOD', 'TrainingOD', 'TrainObjectDetector'], ComputeFunc) then
      begin
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
                  outputstream := AI.OD_Train_Stream(
                  inputImgMatrix,
                  param.GetDefaultValue('window_width', 80),
                  param.GetDefaultValue('window_height', 80),
                  param.GetDefaultValue('thread', 2)
                  )
              else
                  outputstream := AI.OD_Train_Stream(
                  inputImgList,
                  param.GetDefaultValue('window_width', 80),
                  param.GetDefaultValue('window_height', 80),
                  param.GetDefaultValue('thread', 2)
                  );

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_OD_Ext), outputstream);
                  DisposeObject(outputstream);
                  Result := True;
                end;
            except
            end;
          end;
      end
    else if umlMultipleMatch(['TrainOD_Marshal', 'TrainingOD_Marshal', 'TrainObjectDetectorMarshal'], ComputeFunc) then
      begin
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
                  outputstream := AI.OD_Marshal_Train(
                  inputImgMatrix,
                  param.GetDefaultValue('window_width', 80),
                  param.GetDefaultValue('window_height', 80),
                  param.GetDefaultValue('thread', 2)
                  )
              else
                  outputstream := AI.OD_Marshal_Train(
                  inputImgList,
                  param.GetDefaultValue('window_width', 80),
                  param.GetDefaultValue('window_height', 80),
                  param.GetDefaultValue('thread', 2)
                  );

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_OD_Marshal_Ext), outputstream);
                  DisposeObject(outputstream);
                  Result := True;
                end;
            except
            end;
          end;
      end
    else if umlMultipleMatch(['TrainSP', 'TrainingSP', 'TrainShapePredictor'], ComputeFunc) then
      begin
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
                  outputstream := AI.SP_Train_Stream(
                  inputImgMatrix,
                  param.GetDefaultValue('oversampling_amount', 300),
                  param.GetDefaultValue('tree_depth', 2),
                  param.GetDefaultValue('thread', 2)
                  )
              else
                  outputstream := AI.SP_Train_Stream(
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
      end
    else if umlMultipleMatch(['TrainMRN', 'TrainingMRN', 'TrainMetricResNet'], ComputeFunc) then
      begin
        inputfile1 := param.GetDefaultValue('source', '');

        if Task.Exists(inputfile1) then
          begin
            try
              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  Task.Read(inputfile1, inputImgMatrix)
              else
                  Task.Read(inputfile1, inputImgList);

              local_sync := param.GetDefaultValue('syncfile', 'output' + C_Metric_ResNet_Ext + '.sync');
              sync_file := umlCombineFileName(AI.rootPath, local_sync + '_' + umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)));
              if Task.Exists(local_sync) then
                if not umlFileExists(sync_file) then
                    Task.ReadToFile(local_sync, sync_file);

              output_file := umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Metric_ResNet_Ext;

              if umlFileExists(output_file) then
                begin
                  outputstream := TMemoryStream64.Create;
                  outputstream.LoadFromFile(output_file);
                  outputstream.Position := 0;
                end
              else
                begin
                  metric_resnet_param := TAI.Init_Metric_ResNet_Parameter(sync_file, output_file);

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
                      inputImgMatrix,
                      metric_resnet_param)
                  else
                      outputstream := AI.Metric_ResNet_Train_Stream(
                      inputImgList,
                      metric_resnet_param);

                  TAI.Free_Metric_ResNet_Parameter(metric_resnet_param);
                end;

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_Metric_ResNet_Ext), outputstream);
                  Task.WriteFile(param.GetDefaultValue('output.sync', 'output' + C_Metric_ResNet_Ext + '.sync'), sync_file);

                  if param.GetDefaultValue('LearnVec', False) = True then
                    begin
                      learnEng := TLearn.CreateClassifier(ltKDT, zAI.C_Metric_ResNet_Dim);
                      outputstream.Position := 0;
                      mdnn_hnd := AI.Metric_ResNet_Open_Stream(outputstream);

                      DoStatus('build metric to learn-KDTree.');
                      if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                          AI.Metric_ResNet_SaveDetectorDefineToLearnEngine(mdnn_hnd, inputImgMatrix, learnEng)
                      else
                          AI.Metric_ResNet_SaveDetectorDefineToLearnEngine(mdnn_hnd, inputImgList, learnEng);
                      DoStatus('process metric to learn-KDTree done.');
                      AI.Metric_ResNet_Close(mdnn_hnd);

                      tmpM64 := TMemoryStream64.Create;
                      learnEng.SaveToStream(tmpM64);
                      output_learn_file := umlChangeFileExt(param.GetDefaultValue('output', 'output' + C_Metric_ResNet_Ext), C_Learn_Ext);
                      Task.write(param.GetDefaultValue('output.learn', output_learn_file), tmpM64);
                      DisposeObject(tmpM64);
                      DisposeObject(learnEng);
                    end;

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

              local_sync := param.GetDefaultValue('syncfile', 'output' + C_MMOD_Ext + '.sync');
              sync_file := umlCombineFileName(AI.rootPath, local_sync + '_' + umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)));
              if Task.Exists(local_sync) then
                if not umlFileExists(sync_file) then
                    Task.ReadToFile(local_sync, sync_file);

              if umlMultipleMatch('*' + C_ImageMatrix_Ext, inputfile1) then
                  mmod_param := AI.MMOD_DNN_PrepareTrain(inputImgMatrix, sync_file)
              else
                  mmod_param := AI.MMOD_DNN_PrepareTrain(inputImgList, sync_file);

              mmod_param^.timeout := param.GetDefaultValue('timeout', mmod_param^.timeout);
              mmod_param^.target_size := param.GetDefaultValue('target_size', mmod_param^.target_size);
              mmod_param^.min_target_size := param.GetDefaultValue('min_target_size', mmod_param^.min_target_size);
              mmod_param^.min_detector_window_overlap_iou := param.GetDefaultValue('min_detector_window_overlap_iou', mmod_param^.min_detector_window_overlap_iou);
              mmod_param^.iterations_without_progress_threshold := param.GetDefaultValue('iterations_without_progress_threshold', mmod_param^.iterations_without_progress_threshold);
              mmod_param^.learning_rate := param.GetDefaultValue('learning_rate', mmod_param^.learning_rate);
              mmod_param^.completed_learning_rate := param.GetDefaultValue('completed_learning_rate', mmod_param^.completed_learning_rate);
              mmod_param^.num_crops := param.GetDefaultValue('num_crops', mmod_param^.num_crops);
              mmod_param^.chip_dims_x := param.GetDefaultValue('chip_dims_x', mmod_param^.chip_dims_x);
              mmod_param^.chip_dims_y := param.GetDefaultValue('chip_dims_y', mmod_param^.chip_dims_y);
              mmod_param^.min_object_size_x := param.GetDefaultValue('min_object_size_x', mmod_param^.min_object_size_x);
              mmod_param^.min_object_size_y := param.GetDefaultValue('min_object_size_y', mmod_param^.min_object_size_y);
              mmod_param^.max_rotation_degrees := param.GetDefaultValue('max_rotation_degrees', mmod_param^.max_rotation_degrees);
              mmod_param^.max_object_size := param.GetDefaultValue('max_object_size', mmod_param^.max_object_size);

              outputstream := AI.MMOD_DNN_Train_Stream(mmod_param);
              AI.MMOD_DNN_FreeTrain(mmod_param);

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_MMOD_Ext), outputstream);
                  Task.WriteFile(param.GetDefaultValue('output.sync', 'output' + C_MMOD_Ext + '.sync'), sync_file);
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

              local_sync := param.GetDefaultValue('syncfile', 'output' + C_RNIC_Ext + '.sync');
              sync_file := umlCombineFileName(AI.rootPath, local_sync + '_' + umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)));
              if Task.Exists(local_sync) then
                if not umlFileExists(sync_file) then
                    Task.ReadToFile(local_sync, sync_file);

              output_file := umlMD5ToStr(umlCombineMD5(param_md5, Task.LastReadMD5)) + C_Metric_ResNet_Ext;

              rnic_param := TAI.Init_RNIC_Train_Parameter(sync_file, output_file);

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

              if outputstream <> nil then
                begin
                  Task.write(param.GetDefaultValue('output', 'output' + C_RNIC_Ext), outputstream);
                  Task.WriteFile(param.GetDefaultValue('output.sync', 'output' + C_RNIC_Ext + '.sync'), sync_file);
                  Task.write(param.GetDefaultValue('output.index', 'output' + C_RNIC_Ext + '.index'), outputPacalStringList);
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
    Task.write(param.GetDefaultValue('result', 'result.txt'), ResultValues);
    Task.write(param.GetDefaultValue('log', 'log.txt'), Task.TaskLogStatus);

    if AI.AI_Ptr^.Log.Count > 0 then
      begin
        tmpPSL := TPascalStringList.Create;
        for i := 0 to AI.AI_Ptr^.Log.Count - 1 do
            tmpPSL.Add(AI.AI_Ptr^.Log[i].LogText);
        Task.write(param.GetDefaultValue('engine_log', 'engine_log.txt'), tmpPSL);
        DisposeObject(tmpPSL);
      end;

    if AI.AI_Ptr^.OneStepList.Count > 0 then
      begin
        tmpM64 := TMemoryStream64.Create;
        AI.AI_Ptr^.OneStepList.SaveToStream(tmpM64);
        Task.write(param.GetDefaultValue('training_steps', 'training_steps.dat'), tmpM64);
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

constructor TOneStepList.Create;
begin
  inherited Create;
  Critical := TSoftCritical.Create;
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
  try
      Add(p);
  finally
      Critical.Release;
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
  DetDef: TAI_DetectorDefine;
  sp_desc: TSP_Desc;
  r1, r2: TRectV2;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      img.Clear;

      // full detector do scale 4x size
      mr := NewRaster();
      mr.ZoomFrom(img.Raster, img.Raster.width * 4, img.Raster.height * 4);
      // extract face
      face_hnd := AI.Face_Detector_All(mr);
      // dispose raster
      DisposeObject(mr);

      if face_hnd <> nil then
        begin
          for j := 0 to AI.Face_chips_num(face_hnd) - 1 do
            begin
              sp_desc := AI.Face_Shape(face_hnd, j);
              if (length(sp_desc) > 0) then
                begin
                  for k := 0 to length(sp_desc) - 1 do
                    begin
                      sp_desc[k].x := Round(sp_desc[k].x * 0.25);
                      sp_desc[k].y := Round(sp_desc[k].y * 0.25);
                    end;
                  r1 := RectV2(AI.Face_Rect(face_hnd, j));
                  r1 := ForwardRect(RectMul(r1, 0.25));
                  r2 := ForwardRect(GetSPBound(sp_desc, 0.1));

                  if InRect(sp_desc, img.Raster.BoundsRectV2) then
                    begin
                      DetDef := TAI_DetectorDefine.Create(img);
                      img.DetectorDefineList.Add(DetDef);
                      DisposeObject(DetDef.PrepareRaster);
                      DetDef.PrepareRaster := AI.Face_chips(face_hnd, j);
                      SPToVec(sp_desc, DetDef.Part);

                      DetDef.R := MakeRect(BoundRect(r1, r2));
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
  DetDef: TAI_DetectorDefine;
  sp_desc: TSP_Desc;
  r1, r2: TRectV2;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      img.Clear;

      // extract face
      face_hnd := AI.Face_Detector_All(img.Raster);

      if face_hnd <> nil then
        begin
          for j := 0 to AI.Face_chips_num(face_hnd) - 1 do
            begin
              sp_desc := AI.Face_Shape(face_hnd, j);
              if (length(sp_desc) > 0) then
                begin
                  r1 := RectV2(AI.Face_Rect(face_hnd, j));
                  r2 := ForwardRect(GetSPBound(sp_desc, 0.1));

                  if InRect(sp_desc, img.Raster.BoundsRectV2) then
                    begin
                      DetDef := TAI_DetectorDefine.Create(img);
                      img.DetectorDefineList.Add(DetDef);
                      DisposeObject(DetDef.PrepareRaster);
                      DetDef.PrepareRaster := AI.Face_chips(face_hnd, j);
                      SPToVec(sp_desc, DetDef.Part);

                      DetDef.R := MakeRect(BoundRect(r1, r2));
                    end;
                  SetLength(sp_desc, 0);
                end;
            end;

          AI.Face_Close(face_hnd);
        end;
    end;
end;

procedure TAlignment_ScaleSpace.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  DetDef: TAI_DetectorDefine;
begin
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      for j := 0 to img.DetectorDefineList.Count - 1 do
        begin
          DetDef := img.DetectorDefineList[j];
          DetDef.R := RectScaleSpace(DetDef.R, SS_width, SS_height);
          DetDef.R := CalibrationRectInRect(DetDef.R, DetDef.Owner.Raster.BoundsRect);

          DetDef := img.DetectorDefineList[j];
          DisposeObject(DetDef.PrepareRaster);
          DetDef.PrepareRaster := DetDef.Owner.Raster.BuildAreaOffsetScaleSpace(DetDef.R, SS_width, SS_height);
        end;
    end;
end;

procedure TAlignment_OD.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  DetDef: TAI_DetectorDefine;
  od_desc: TOD_Desc;
  mr: TMemoryRaster;
begin
  if od_hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      img.Clear;
      mr := NewRaster();
      mr.ZoomFrom(img.Raster, img.Raster.width * 4, img.Raster.height * 4);
      od_desc := AI.OD_Process(od_hnd, mr, 1024);
      DisposeObject(mr);
      for j := 0 to length(od_desc) - 1 do
        begin
          DetDef := TAI_DetectorDefine.Create(img);
          DetDef.R := MakeRect(RectMul(RectV2(od_desc[j]), 0.25));
          img.DetectorDefineList.Add(DetDef);
        end;
    end;
end;

procedure TAlignment_FastOD.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  DetDef: TAI_DetectorDefine;
  od_desc: TOD_Desc;
begin
  if od_hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      img.Clear;
      od_desc := AI.OD_Process(od_hnd, img.Raster, 1024);
      for j := 0 to length(od_desc) - 1 do
        begin
          DetDef := TAI_DetectorDefine.Create(img);
          DetDef.R := Rect(od_desc[j]);
          img.DetectorDefineList.Add(DetDef);
        end;
    end;
end;

procedure TAlignment_OD_Marshal.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  DetDef: TAI_DetectorDefine;
  od_desc: TOD_Marshal_Desc;
  mr: TMemoryRaster;
begin
  if od_hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      img.Clear;
      mr := NewRaster();
      mr.ZoomFrom(img.Raster, img.Raster.width * 4, img.Raster.height * 4);
      od_desc := AI.OD_Marshal_Process(od_hnd, mr);
      DisposeObject(mr);
      for j := 0 to length(od_desc) - 1 do
        begin
          DetDef := TAI_DetectorDefine.Create(img);
          DetDef.R := MakeRect(RectMul(od_desc[j].R, 0.25));
          DetDef.Token := od_desc[j].Token;
          img.DetectorDefineList.Add(DetDef);
        end;
    end;
end;

procedure TAlignment_FastOD_Marshal.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  DetDef: TAI_DetectorDefine;
  od_desc: TOD_Marshal_Desc;
begin
  if od_hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      img.Clear;
      od_desc := AI.OD_Marshal_Process(od_hnd, img.Raster);
      for j := 0 to length(od_desc) - 1 do
        begin
          DetDef := TAI_DetectorDefine.Create(img);
          DetDef.R := MakeRect(od_desc[j].R);
          DetDef.Token := od_desc[j].Token;
          img.DetectorDefineList.Add(DetDef);
        end;
    end;
end;

procedure TAlignment_SP.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  DetDef: TAI_DetectorDefine;
  sp_desc: TSP_Desc;
begin
  if sp_hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      for j := 0 to img.DetectorDefineList.Count - 1 do
        begin
          DetDef := img.DetectorDefineList[j];

          sp_desc := AI.SP_Process(sp_hnd, DetDef.Owner.Raster, AIRect(DetDef.R), 1024);
          if length(sp_desc) > 0 then
            begin
              DetDef.Part.Clear;
              SPToVec(sp_desc, DetDef.Part);
              DetDef.PrepareRaster.Reset;
              SetLength(sp_desc, 0);
            end;
        end;
    end;
end;

procedure TAlignment_MMOD.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  DetDef: TAI_DetectorDefine;
  mmod_desc: TMMOD_Desc;
  mr: TMemoryRaster;
begin
  if MMOD_hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      img.Clear;
      mr := NewRaster();
      mr.ZoomFrom(img.Raster, img.Raster.width * 4, img.Raster.height * 4);
      mmod_desc := AI.MMOD_DNN_Process(MMOD_hnd, mr);
      DisposeObject(mr);
      for j := 0 to length(mmod_desc) - 1 do
        begin
          DetDef := TAI_DetectorDefine.Create(img);
          DetDef.R := MakeRect(RectMul(mmod_desc[j].R, 0.25));
          DetDef.Token := mmod_desc[j].Token;
          img.DetectorDefineList.Add(DetDef);
        end;
    end;
end;

procedure TAlignment_FastMMOD.Alignment(imgList: TAI_ImageList);
var
  i, j: Integer;
  img: TAI_Image;
  DetDef: TAI_DetectorDefine;
  mmod_desc: TMMOD_Desc;
begin
  if MMOD_hnd = nil then
      exit;
  for i := 0 to imgList.Count - 1 do
    begin
      img := imgList[i];
      img.Clear;
      mmod_desc := AI.MMOD_DNN_Process(MMOD_hnd, img.Raster);
      for j := 0 to length(mmod_desc) - 1 do
        begin
          DetDef := TAI_DetectorDefine.Create(img);
          DetDef.R := MakeRect(mmod_desc[j].R);
          DetDef.Token := mmod_desc[j].Token;
          img.DetectorDefineList.Add(DetDef);
        end;
    end;
end;

constructor TAI.Create;
begin
  inherited Create;
  AI_Ptr := nil;
  face_sp_hnd := nil;
  TrainingControl.pause := 0;
  TrainingControl.stop := 0;
  Critical := TSoftCritical.Create;

  Parallel_OD_Hnd := nil;
  Parallel_OD_Marshal_Hnd := nil;
  Parallel_SP_Hnd := nil;

{$IFDEF FPC}
  rootPath := AI_Configure_Path;
{$ELSE FPC}
  rootPath := System.IOUtils.TPath.GetTempPath;
{$ENDIF FPC}
  rootPath := umlCombinePath(rootPath, 'Z_AI_Temp');
  umlCreateDirectory(rootPath);

  Last_training_average_loss := 0;
  Last_training_learning_rate := 0;
end;

class function TAI.OpenEngine(libFile: SystemString): TAI;
begin
  Result := TAI.Create;
  Result.AI_Ptr := Load_ZAI(libFile);
  if Result.AI_Ptr = nil then
      Result.AI_Ptr := Load_ZAI(AI_Engine_Library);
end;

class function TAI.OpenEngine(lib_p: PAI_Entry): TAI;
begin
  Result := TAI.Create;
  Result.AI_Ptr := lib_p;
end;

class function TAI.OpenEngine: TAI;
begin
  Result := TAI.Create;
  Result.AI_Ptr := Load_ZAI(AI_Engine_Library);
end;

destructor TAI.Destroy;
begin
  if face_sp_hnd <> nil then
      SP_Close(face_sp_hnd);
  if Parallel_OD_Hnd <> nil then
      OD_Close(Parallel_OD_Hnd);
  if Parallel_OD_Marshal_Hnd <> nil then
      OD_Marshal_Close(Parallel_OD_Marshal_Hnd);
  if Parallel_SP_Hnd <> nil then
      SP_Close(Parallel_SP_Hnd);

  DisposeObject(Critical);
  inherited Destroy;
end;

procedure TAI.DrawOD(od_hnd: TOD_Handle; Raster: TMemoryRaster; color: TDEColor);
var
  od_desc: TOD_Desc;
  i: Integer;
  d: TDrawEngine;
  dt: TTimeTick;
begin
  dt := GetTimeTick();
  od_desc := OD_Process(od_hnd, Raster, 1024);
  dt := GetTimeTick() - dt;
  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);
  for i := 0 to length(od_desc) - 1 do
    begin
      d.DrawBox(RectV2(od_desc[i]), color, 2);

      d.BeginCaptureShadow(Vec2(1, 1), 0.9);
      d.DrawText(PFormat('%f', [od_desc[i].confidence]), 16, RectV2(od_desc[i]), color, False);
      d.EndCaptureShadow;
    end;
  d.Flush;
  DisposeObject(d);
end;

procedure TAI.DrawOD(od_desc: TOD_Desc; Raster: TMemoryRaster; color: TDEColor);
var
  i: Integer;
  d: TDrawEngine;
begin
  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);
  for i := 0 to length(od_desc) - 1 do
    begin
      d.DrawBox(RectV2(od_desc[i]), color, 2);

      d.BeginCaptureShadow(Vec2(1, 1), 0.9);
      d.DrawText(PFormat('%f', [od_desc[i].confidence]), 16, RectV2(od_desc[i]), color, False);
      d.EndCaptureShadow;
    end;
  d.Flush;
  DisposeObject(d);
end;

procedure TAI.DrawODM(odm_hnd: TOD_Marshal_Handle; Raster: TMemoryRaster; color: TDEColor);
var
  odm_desc: TOD_Marshal_Desc;
  i: Integer;
  d: TDrawEngine;
  dt: TTimeTick;
begin
  dt := GetTimeTick();
  odm_desc := OD_Marshal_Process(odm_hnd, Raster);
  dt := GetTimeTick() - dt;
  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);
  for i := 0 to length(odm_desc) - 1 do
    begin
      d.DrawBox(odm_desc[i].R, color, 2);
      d.BeginCaptureShadow(Vec2(1, 1), 0.9);
      d.DrawText(PFormat('%s-%f', [odm_desc[i].Token.Text, odm_desc[i].confidence]), 16, odm_desc[i].R, DEColor(1, 1, 1, 1), False);
      d.EndCaptureShadow;
    end;
  d.Flush;
  DisposeObject(d);
end;

procedure TAI.DrawSP(od_hnd: TOD_Handle; sp_hnd: TSP_Handle; Raster: TMemoryRaster);
var
  od_desc: TOD_Desc;
  sp_desc: TSP_Desc;
  i, j: Integer;
  d: TDrawEngine;
begin
  od_desc := OD_Process(od_hnd, Raster, 1024);
  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);
  for i := 0 to length(od_desc) - 1 do
    begin
      d.DrawBox(RectV2(od_desc[i]), DEColor(1, 0, 0, 0.9), 2);
      sp_desc := SP_Process(sp_hnd, Raster, AIRect(od_desc[i]), 1024);
      for j := 0 to length(sp_desc) - 1 do
          d.DrawPoint(Vec2(sp_desc[j]), DEColor(1, 0, 0, 0.9), 4, 2);
    end;
  d.Flush;
  DisposeObject(d);
end;

function TAI.DrawMMOD(MMOD_hnd: TMMOD_Handle; Raster: TMemoryRaster; color: TDEColor): TMMOD_Desc;
var
  mmod_desc: TMMOD_Desc;
  i: Integer;
  d: TDrawEngine;
  dt: TTimeTick;
begin
  dt := GetTimeTick();
  mmod_desc := MMOD_DNN_Process(MMOD_hnd, Raster);
  dt := GetTimeTick() - dt;
  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);
  for i := 0 to length(mmod_desc) - 1 do
    begin
      d.DrawBox(mmod_desc[i].R, color, 2);
      d.BeginCaptureShadow(Vec2(1, 1), 0.9);
      d.DrawText(PFormat('%s-%f', [mmod_desc[i].Token.Text, mmod_desc[i].confidence]), 16, mmod_desc[i].R, DEColor(1, 1, 1, 1), False);
      d.EndCaptureShadow;
    end;
  d.Flush;
  DisposeObject(d);
  Result := mmod_desc;
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

procedure TAI.DrawFace(Raster: TMemoryRaster; mdnn_hnd: TMDNN_Handle; Face_Learn: TLearn; faceAccuracy: TGeoFloat; lineColor, TextColor: TDEColor);
var
  face_hnd: TFACE_Handle;
  d: TDrawEngine;
  i: Integer;
  sp_desc: TSP_Desc;
  chip_img: TMemoryRaster;
  face_vec: TLVec;
  LIndex: TLInt;
  p: TLearn.PLearnMemory;
  k: TLFloat;
  face_lab: SystemString;
begin
  face_hnd := Face_Detector_All(Raster);
  if face_hnd = nil then
      exit;

  d := TDrawEngine.Create;
  d.ViewOptions := [];
  d.Rasterization.SetWorkMemory(Raster);

  for i := 0 to Face_Shape_num(face_hnd) - 1 do
    begin
      sp_desc := Face_Shape(face_hnd, i);
      d.DrawCorner(TV2Rect4.Init(GetSPBound(sp_desc, 0.01), 0), DEColor(lineColor, 0.9), 40, 4);

      chip_img := Face_chips(face_hnd, i);
      face_vec := Metric_ResNet_Process(mdnn_hnd, chip_img);
      DisposeObject(chip_img);

      LIndex := Face_Learn.ProcessMaxIndex(face_vec);
      p := Face_Learn.MemorySource[LIndex];
      k := LDistance(face_vec, p^.m_in);
      face_lab := p^.Token;
      SetLength(face_vec, 0);

      if k <= faceAccuracy then
        begin
          DrawFaceSP(sp_desc, DEColor(lineColor, 0.5), d);
          d.BeginCaptureShadow(Vec2(2, 2), 0.9);
          d.DrawText(PFormat('%s-%f', [face_lab, 1.0 - k]), 16, GetSPBound(sp_desc, 0.01), DEColor(TextColor, 0.9), True);
          d.EndCaptureShadow;
          DoStatus(PFormat('%s-%f', [face_lab, 1.0 - k]));
        end
      else
        begin
          d.BeginCaptureShadow(Vec2(2, 2), 0.9);
          d.DrawText('no face defined.', 16, GetSPBound(sp_desc, 0.01), DEColor(lineColor, 0.9), True);
          d.EndCaptureShadow;
          DoStatus('no face defined.');
        end;
    end;

  d.Flush;
  DisposeObject(d);
  Face_Close(face_hnd);
end;

procedure TAI.PrintFace(prefix: SystemString; Raster: TMemoryRaster; mdnn_hnd: TMDNN_Handle; Face_Learn: TLearn; faceAccuracy: TGeoFloat);
var
  face_hnd: TFACE_Handle;
  i: Integer;
  sp_desc: TSP_Desc;
  chip_img: TMemoryRaster;
  face_vec: TLVec;
  LIndex: TLInt;
  p: TLearn.PLearnMemory;
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
      face_vec := Metric_ResNet_Process(mdnn_hnd, chip_img);
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
      d.DrawTexture(mr, mr.BoundsRectV2, rp[i]^.Rect, 0, 1.0);
    end;

  d.Flush;
  DisposeObject(d);

  for i := 0 to rp.Count - 1 do
      DisposeObject(rp[i]^.Data2);
  DisposeObject(rp);
end;

procedure TAI.Lock;
begin
  Critical.Acquire;
end;

procedure TAI.Unlock;
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

function TAI.Prepare_RGB_Image(Raster: TMemoryRaster): TRGB_Image_Handle;
begin
  Result := nil;
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.Prepare_RGB_Image) then
      Result := AI_Ptr^.Prepare_RGB_Image(Raster.bits, Raster.width, Raster.height);
end;

procedure TAI.Close_RGB_Image(hnd: TRGB_Image_Handle);
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.Close_RGB_Image) then
      AI_Ptr^.Close_RGB_Image(hnd);
end;

function TAI.Prepare_Matrix_Image(Raster: TMemoryRaster): TMatrix_Image_Handle;
begin
  Result := nil;
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.Prepare_Matrix_Image) then
      Result := AI_Ptr^.Prepare_Matrix_Image(Raster.bits, Raster.width, Raster.height);
end;

procedure TAI.Close_Matrix_Image(hnd: TMatrix_Image_Handle);
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.Close_Matrix_Image) then
      AI_Ptr^.Close_Matrix_Image(hnd);
end;

procedure TAI.HotMap(Raster: TMemoryRaster);
var
  hnd: TBGRA_Buffer_Handle;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.OpenImageBuffer_Hot) then
    begin
      hnd := AI_Ptr^.OpenImageBuffer_Hot(Raster.bits, Raster.width, Raster.height);
      if hnd <> nil then
        begin
          CopyPtr(hnd^.bits, Raster.bits, (hnd^.width * hnd^.height) shl 2);
          AI_Ptr^.CloseImageBuffer(hnd);
        end;
    end;
end;

procedure TAI.JetMap(Raster: TMemoryRaster);
var
  hnd: TBGRA_Buffer_Handle;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.OpenImageBuffer_Jet) then
    begin
      hnd := AI_Ptr^.OpenImageBuffer_Jet(Raster.bits, Raster.width, Raster.height);
      if hnd <> nil then
        begin
          CopyPtr(hnd^.bits, Raster.bits, (hnd^.width * hnd^.height) shl 2);
          AI_Ptr^.CloseImageBuffer(hnd);
        end;
    end;
end;

function TAI.BuildHotMap(Raster: TMemoryRaster): TMemoryRaster;
var
  hnd: TBGRA_Buffer_Handle;
begin
  Result := nil;

  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.OpenImageBuffer_Hot) then
    begin
      hnd := AI_Ptr^.OpenImageBuffer_Hot(Raster.bits, Raster.width, Raster.height);
      if hnd <> nil then
        begin
          Result := NewRaster();
          Result.SetSize(hnd^.width, hnd^.height);
          CopyPtr(hnd^.bits, Result.bits, (hnd^.width * hnd^.height) shl 2);
          AI_Ptr^.CloseImageBuffer(hnd);
        end;
    end;
end;

function TAI.BuildJetMap(Raster: TMemoryRaster): TMemoryRaster;
var
  hnd: TBGRA_Buffer_Handle;
begin
  Result := nil;

  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.OpenImageBuffer_Jet) then
    begin
      hnd := AI_Ptr^.OpenImageBuffer_Jet(Raster.bits, Raster.width, Raster.height);
      if hnd <> nil then
        begin
          Result := NewRaster();
          Result.SetSize(hnd^.width, hnd^.height);
          CopyPtr(hnd^.bits, Result.bits, (hnd^.width * hnd^.height) shl 2);
          AI_Ptr^.CloseImageBuffer(hnd);
        end;
    end;
end;

function TAI.fast_surf(Raster: TMemoryRaster; const max_points: Integer; const detection_threshold: Double): TSurf_DescBuffer;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.fast_surf) then
    begin
      SetLength(Result, max_points);
      SetLength(Result, AI_Ptr^.fast_surf(Raster.bits, Raster.width, Raster.height, max_points, detection_threshold, @Result[0]));
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
  sd1, sd2: TSurf_DescBuffer;
  r1, r2: TMemoryRaster;
  l: TCoreClassList;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure Nested_ParallelFor(pass: PtrInt; data: Pointer; Item: TMultiThreadProcItem);
  var
    m_idx, j: Integer;
    minf, next_minf: Single;
    d: Single;
    p: PSurfMatched;
  begin
    m_idx := -1;
    minf := MaxRealNumber;
    next_minf := minf;

    // find dsc1 from feat2
    for j := 0 to sd2_len - 1 do
      begin
        d := Min(surf_sqr(@sd1[pass], @sd2[j]), next_minf);
        if (d < minf) then
          begin
            next_minf := minf;
            minf := d;
            m_idx := j;
          end
        else
            next_minf := Min(next_minf, d);
      end;

    // bidirectional rejection
    if (minf > reject_ratio_sqr * next_minf) then
        exit;

    // fix m_idx
    for j := 0 to sd1_len - 1 do
      if j <> pass then
        begin
          d := Min(surf_sqr(@sd1[j], @sd2[m_idx]), next_minf);
          next_minf := Min(next_minf, d);
        end;

    // bidirectional rejection
    if (minf > reject_ratio_sqr * next_minf) then
        exit;

    new(p);
    p^.sd1 := @sd1[pass];
    p^.sd2 := @sd2[m_idx];
    p^.r1 := r1;
    p^.r2 := r2;
    LockObject(l);
    l.Add(p);
    UnLockObject(l);
  end;
{$ENDIF FPC}
{$ELSE parallel}
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

        // find dsc1 from feat2
        for j := 0 to sd2_len - 1 do
          begin
            d := Min(surf_sqr(@sd1[pass], @sd2[j]), next_minf);
            if (d < minf) then
              begin
                next_minf := minf;
                minf := d;
                m_idx := j;
              end
            else
                next_minf := Min(next_minf, d);
          end;

        // bidirectional rejection
        if (minf > reject_ratio_sqr * next_minf) then
            continue;

        // fix m_idx
        for j := 0 to sd1_len - 1 do
          if j <> pass then
            begin
              d := Min(surf_sqr(@sd1[j], @sd2[m_idx]), next_minf);
              next_minf := Min(next_minf, d);
            end;

        // bidirectional rejection
        if (minf > reject_ratio_sqr * next_minf) then
            continue;

        new(p);
        p^.sd1 := @sd1[pass];
        p^.sd2 := @sd2[m_idx];
        p^.r1 := r1;
        p^.r2 := r2;
        l.Add(p);
      end;
  end;
{$ENDIF parallel}
  procedure FillMatchInfoAndFreeTemp;
  var
    i: Integer;
    p: PSurfMatched;
  begin
    SetLength(Result, l.Count);
    for i := 0 to l.Count - 1 do
      begin
        p := PSurfMatched(l[i]);
        Result[i] := p^;
        Dispose(p);
      end;
  end;

begin
  SetLength(Result, 0);
  sd1_len := length(sd1_);
  sd2_len := length(sd2_);

  if (sd1_len = 0) or (sd2_len = 0) then
      exit;

  if sd1_len > sd2_len then
    begin
      Swap(sd1_len, sd2_len);
      sd1 := sd2_;
      r1 := r2_;
      sd2 := sd1_;
      r2 := r1_;
    end
  else
    begin
      sd1 := sd1_;
      r1 := r1_;
      sd2 := sd2_;
      r2 := r2_;
    end;

  l := TCoreClassList.Create;
  l.Capacity := sd1_len;

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@Nested_ParallelFor, 0, sd1_len - 1);
{$ELSE FPC}
  TParallel.for(0, sd1_len - 1, procedure(pass: Integer)
    var
      m_idx, j: Integer;
      minf, next_minf: Single;
      d: Single;
      p: PSurfMatched;
    begin
      m_idx := -1;
      minf := MaxRealNumber;
      next_minf := minf;

      // find dsc1 from feat2
      for j := 0 to sd2_len - 1 do
        begin
          d := Min(surf_sqr(@sd1[pass], @sd2[j]), next_minf);
          if (d < minf) then
            begin
              next_minf := minf;
              minf := d;
              m_idx := j;
            end
          else
              next_minf := Min(next_minf, d);
        end;

      // bidirectional rejection
      if (minf > reject_ratio_sqr * next_minf) then
          exit;

      // fix m_idx
      for j := 0 to sd1_len - 1 do
        if j <> pass then
          begin
            d := Min(surf_sqr(@sd1[j], @sd2[m_idx]), next_minf);
            next_minf := Min(next_minf, d);
          end;

      // bidirectional rejection
      if (minf > reject_ratio_sqr * next_minf) then
          exit;

      new(p);
      p^.sd1 := @sd1[pass];
      p^.sd2 := @sd2[m_idx];
      p^.r1 := r1;
      p^.r2 := r2;
      LockObject(l);
      l.Add(p);
      UnLockObject(l);
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor();
{$ENDIF parallel}
  FillMatchInfoAndFreeTemp;
  DisposeObject(l);
end;

procedure TAI.BuildFeatureView(Raster: TMemoryRaster; descbuff: TSurf_DescBuffer);
var
  i: Integer;
  p: PSurf_Desc;
begin
  Raster.OpenAgg;
  Raster.Agg.LineWidth := 1.0;
  for i := 0 to length(descbuff) - 1 do
    begin
      p := @descbuff[i];

      Raster.DrawCrossF(p^.x, p^.y, 4, RasterColorF(1, 0, 0, 1));
      Raster.LineF(p^.x, p^.y, p^.dx, p^.dy, RasterColorF(0, 1, 0, 0.5), True);
    end;
end;

function TAI.BuildMatchInfoView(var MatchInfo: TSurfMatchedBuffer): TMemoryRaster;
var
  mr1, mr2: TMemoryRaster;
  c: Byte;
  i, j: Integer;

  p: PSurfMatched;
  RC: TRasterColor;
  v1, v2: TVec2;
begin
  if length(MatchInfo) = 0 then
    begin
      Result := nil;
      exit;
    end;
  Result := NewRaster();

  mr1 := MatchInfo[0].r1;
  mr2 := MatchInfo[0].r2;

  Result.SetSize(mr1.width + mr2.width, Max(mr1.height, mr2.height), RasterColor(0, 0, 0, 0));
  Result.Draw(0, 0, mr1);
  Result.Draw(mr1.width, 0, mr2);
  Result.OpenAgg;

  for i := 0 to length(MatchInfo) - 1 do
    begin
      p := @MatchInfo[i];
      RC := RasterColor(RandomRange(0, 255), RandomRange(0, 255), RandomRange(0, 255), $7F);
      v1 := Geometry2DUnit.Vec2(p^.sd1^.x, p^.sd1^.y);
      v2 := Geometry2DUnit.Vec2(p^.sd2^.x, p^.sd2^.y);
      v2 := Geometry2DUnit.Vec2Add(v2, Geometry2DUnit.Vec2(mr1.width, 0));

      Result.LineF(v1, v2, RC, True);
    end;
end;

function TAI.BuildSurfMatchOutput(raster1, raster2: TMemoryRaster): TMemoryRaster;
var
  r1, r2: TMemoryRaster;
  d1, d2: TSurf_DescBuffer;
  matched: TSurfMatchedBuffer;
begin
  r1 := NewRaster();
  r2 := NewRaster();
  r1.Assign(raster1);
  r2.Assign(raster2);
  d1 := fast_surf(r1, 20000, 1.0);
  d2 := fast_surf(r2, 20000, 1.0);
  BuildFeatureView(r1, d1);
  BuildFeatureView(r2, d2);
  matched := Surf_Matched(0.4, r1, r2, d1, d2);
  Result := BuildMatchInfoView(matched);
  DisposeObject([r1, r2]);
  SetLength(matched, 0);
  SetLength(d1, 0);
  SetLength(d2, 0);
end;

function TAI.OD_Train(train_cfg, train_output: TPascalString; window_w, window_h, thread_num: Integer): Boolean;
var
  train_cfg_buff, train_output_buff: P_Bytes;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.OD_Train) and (umlFileExists(train_cfg)) and (train_output.Len > 0) then
    begin
      train_cfg_buff := Alloc_P_Bytes(train_cfg);
      train_output_buff := Alloc_P_Bytes(train_output);

      try
          Result := AI_Ptr^.OD_Train(train_cfg_buff, train_output_buff, window_w, window_h, thread_num) = 0;
      except
          Result := False;
      end;

      Free_P_Bytes(train_cfg_buff);
      Free_P_Bytes(train_output_buff);

      if not Result then
          DoStatus('ZAI: Object Detector Train failed.');
    end
  else
      Result := False;
end;

function TAI.OD_Train(imgList: TAI_ImageList; TokenFilter, train_output: TPascalString; window_w, window_h, thread_num: Integer): Boolean;
var
  ph, fn, prefix: TPascalString;
  tmpFileList: TPascalStringList;
  i: Integer;
begin
  ph := rootPath;
  tmpFileList := TPascalStringList.Create;

  TCoreClassThread.Sleep(1);
  prefix := 'temp_OD_' + umlMakeRanName + '_';

  fn := umlCombineFileName(ph, prefix + 'temp.xml');
  imgList.Build_XML(TokenFilter, False, False, 'ZAI dataset', 'object detector training dataset', fn, prefix, tmpFileList);

  Result := OD_Train(fn, train_output, window_w, window_h, thread_num);

  for i := 0 to tmpFileList.Count - 1 do
      umlDeleteFile(tmpFileList[i]);

  DisposeObject(tmpFileList);
end;

function TAI.OD_Train(imgMat: TAI_ImageMatrix; TokenFilter, train_output: TPascalString; window_w, window_h, thread_num: Integer): Boolean;
var
  ph, fn, prefix: TPascalString;
  tmpFileList: TPascalStringList;
  i: Integer;
begin
  ph := rootPath;
  tmpFileList := TPascalStringList.Create;

  TCoreClassThread.Sleep(1);
  prefix := 'temp_OD_' + umlMakeRanName + '_';

  fn := umlCombineFileName(ph, prefix + 'temp.xml');
  imgMat.Build_XML(TokenFilter, False, False, 'ZAI dataset', 'object detector training dataset', fn, prefix, tmpFileList);

  Result := OD_Train(fn, train_output, window_w, window_h, thread_num);

  for i := 0 to tmpFileList.Count - 1 do
      umlDeleteFile(tmpFileList[i]);

  DisposeObject(tmpFileList);
end;

function TAI.OD_Train_Stream(imgList: TAI_ImageList; window_w, window_h, thread_num: Integer): TMemoryStream64;
var
  fn: TPascalString;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(rootPath, PFormat('temp_OD_%s' + C_OD_Ext, [umlMakeRanName.Text]));

  if OD_Train(imgList, '', fn, window_w, window_h, thread_num) then
    if umlFileExists(fn) then
      begin
        Result := TMemoryStream64.Create;
        Result.LoadFromFile(fn);
        Result.Position := 0;
      end;
  umlDeleteFile(fn);
end;

function TAI.OD_Train_Stream(imgMat: TAI_ImageMatrix; window_w, window_h, thread_num: Integer): TMemoryStream64;
var
  fn: TPascalString;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(rootPath, PFormat('temp_OD_%s' + C_OD_Ext, [umlMakeRanName.Text]));

  if OD_Train(imgMat, '', fn, window_w, window_h, thread_num) then
    if umlFileExists(fn) then
      begin
        Result := TMemoryStream64.Create;
        Result.LoadFromFile(fn);
        Result.Position := 0;
      end;
  umlDeleteFile(fn);
end;

function TAI.OD_Open(train_file: TPascalString): TOD_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.OD_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      try
          Result := AI_Ptr^.OD_Init(train_file_buff);
      finally
          Free_P_Bytes(train_file_buff);
      end;
      if Result <> nil then
          DoStatus('Object detector open: %s', [train_file.Text]);
    end
  else
      Result := nil;
end;

function TAI.OD_Open_Stream(stream: TMemoryStream64): TOD_Handle;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.OD_Init_Memory) then
    begin
      Result := AI_Ptr^.OD_Init_Memory(stream.memory, stream.Size);
      if Result <> nil then
          DoStatus('Object Detector open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.OD_Open_Stream(train_file: TPascalString): TOD_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := OD_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('Object detector open: %s', [train_file.Text]);
end;

function TAI.OD_Close(var hnd: TOD_Handle): Boolean;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.OD_Free) and (hnd <> nil) then
    begin
      Result := AI_Ptr^.OD_Free(hnd) = 0;
      DoStatus('Object detector Close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

function TAI.OD_Process(hnd: TOD_Handle; Raster: TMemoryRaster; const max_AI_Rect: Integer): TOD_Desc;
var
  rect_num: Integer;
begin
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if (AI_Ptr = nil) then
      exit;
  if not Assigned(AI_Ptr^.OD_Process) then
      exit;
  SetLength(Result, max_AI_Rect);

  try
    if AI_Ptr^.OD_Process(hnd, Raster.bits, Raster.width, Raster.height,
      @Result[0], max_AI_Rect, rect_num) > 0 then
        SetLength(Result, rect_num)
    else
        SetLength(Result, 0);
  except
      SetLength(Result, 0);
  end;
end;

function TAI.OD_Process(hnd: TOD_Handle; Raster: TMemoryRaster): TOD_List;
var
  od_desc: TOD_Desc;
  i: Integer;
begin
  Result := TOD_List.Create;
  od_desc := OD_Process(hnd, Raster, 1024);
  for i := Low(od_desc) to High(od_desc) do
      Result.Add(od_desc[i]);
end;

procedure TAI.OD_Process(hnd: TOD_Handle; Raster: TMemoryRaster; output: TOD_List);
var
  od_desc: TOD_Desc;
  i: Integer;
begin
  od_desc := OD_Process(hnd, Raster, 1024);
  for i := Low(od_desc) to High(od_desc) do
      output.Add(od_desc[i]);
end;

function TAI.OD_Process(hnd: TOD_Handle; rgb_img: TRGB_Image_Handle; const max_AI_Rect: Integer): TOD_Desc;
var
  rect_num: Integer;
begin
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if (AI_Ptr = nil) then
      exit;
  if not Assigned(AI_Ptr^.OD_Process_Image) then
      exit;
  SetLength(Result, max_AI_Rect);

  try
    if AI_Ptr^.OD_Process_Image(hnd, rgb_img, @Result[0], max_AI_Rect, rect_num) > 0 then
        SetLength(Result, rect_num)
    else
        SetLength(Result, 0);
  except
      SetLength(Result, 0);
  end;
end;

function TAI.OD_ProcessScaleSpace(hnd: TOD_Handle; Raster: TMemoryRaster; scale: TGeoFloat): TOD_Desc;
var
  nr: TMemoryRaster;
  buff: TOD_Desc;
  i: Integer;
begin
  nr := NewRaster();
  nr.ZoomFrom(Raster, scale);

  buff := OD_Process(hnd, nr, 1024);

  SetLength(Result, length(buff));

  for i := 0 to length(buff) - 1 do
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

function TAI.OD_Marshal_Train(imgList: TAI_ImageList; window_w, window_h, thread_num: Integer): TMemoryStream64;
var
  dbEng: TObjectDataManager;
  token_arry: TArrayPascalString;
  m64: TMemoryStream64;
  itmHnd: TItemHandle;
  Token: TPascalString;
  fn: TPascalString;
begin
  Result := TMemoryStream64.Create;
  dbEng := TObjectDataManagerOfCache.CreateAsStream(Result, '', DBMarshal.ID, False, True, False);

  imgList.CalibrationNullDetectorDefineToken('null');

  token_arry := imgList.Tokens;
  for Token in token_arry do
    begin
      TCoreClassThread.Sleep(1);
      fn := umlCombineFileName(rootPath, PFormat('temp_OD_%s' + C_OD_Ext, [umlMakeRanName.Text]));

      if OD_Train(imgList, Token, fn, window_w, window_h, thread_num) then
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

function TAI.OD_Marshal_Train(imgMat: TAI_ImageMatrix; window_w, window_h, thread_num: Integer): TMemoryStream64;
var
  i: Integer;
  dbEng: TObjectDataManager;
  token_arry: TArrayPascalString;
  m64: TMemoryStream64;
  itmHnd: TItemHandle;
  Token: TPascalString;
  fn: TPascalString;
begin
  Result := TMemoryStream64.Create;
  dbEng := TObjectDataManagerOfCache.CreateAsStream(Result, '', DBMarshal.ID, False, True, False);

  for i := 0 to imgMat.Count - 1 do
      imgMat[i].CalibrationNullDetectorDefineToken('null');

  token_arry := imgMat.Tokens;
  for Token in token_arry do
    begin
      TCoreClassThread.Sleep(1);
      fn := umlCombineFileName(rootPath, PFormat('temp_OD_%s' + C_OD_Ext, [umlMakeRanName.Text]));

      if OD_Train(imgMat, Token, fn, window_w, window_h, thread_num) then
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

function TAI.OD_Marshal_Open_Stream(stream: TMemoryStream64): TOD_Marshal_Handle;
var
  m64: TMemoryStream64;
  dbEng: TObjectDataManager;
  itmSR: TItemSearch;
  itmHnd: TItemHandle;
  od_hnd: TOD_Handle;
begin
  m64 := TMemoryStream64.Create;
  m64.SetPointerWithProtectedMode(stream.memory, stream.Size);
  dbEng := TObjectDataManagerOfCache.CreateAsStream(m64, '', DBMarshal.ID, True, False, True);

  Result := TOD_Marshal_Handle.Create;
  Result.AutoFreeData := False;

  if dbEng.ItemFastFindFirst(dbEng.RootField, '', itmSR) then
    begin
      repeat
        dbEng.ItemFastOpen(itmSR.HeaderPOS, itmHnd);
        m64 := TMemoryStream64.Create;
        m64.SetSize(itmHnd.Item.Size);
        dbEng.ItemRead(itmHnd, itmHnd.Item.Size, m64.memory^);
        dbEng.ItemClose(itmHnd);

        od_hnd := Result[itmHnd.name];
        if od_hnd <> nil then
            OD_Close(od_hnd);
        od_hnd := OD_Open_Stream(m64);
        Result.Add(itmHnd.name, od_hnd, False);
        DisposeObject(m64);

      until not dbEng.ItemFastFindNext(itmSR);
    end;

  DisposeObject(dbEng);

  DoStatus('Object Detector marshal open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
end;

function TAI.OD_Marshal_Open_Stream(train_file: TPascalString): TOD_Marshal_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := OD_Marshal_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('Object marshal detector open: %s', [train_file.Text]);
end;

function TAI.OD_Marshal_Close(var hnd: TOD_Marshal_Handle): Boolean;
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
          OD_Close(p^.data);
          inc(i);
          p := p^.Next;
        end;
    end;
  DisposeObject(hnd);
  Result := True;
end;

function TAI.OD_Marshal_Process(hnd: TOD_Marshal_Handle; Raster: TMemoryRaster): TOD_Marshal_Desc;
var
  lst: TCoreClassList;
  output: TOD_Marshal_List;
  rgb_img: TRGB_Image_Handle;

{$IFDEF parallel}
{$IFDEF FPC}
  procedure FPC_ParallelFor(pass: PtrInt; data: Pointer; Item: TMultiThreadProcItem);
  var
    j: Integer;
    p: PHashListData;
    od_desc: TOD_Desc;
    omr: TOD_Marshal_Rect;
  begin
    p := PHashListData(lst[pass]);
    od_desc := OD_Process(p^.data, rgb_img, 1024);
    for j := low(od_desc) to high(od_desc) do
      begin
        omr.R := RectV2(od_desc[j]);
        omr.confidence := od_desc[j].confidence;
        omr.Token := p^.OriginName;
        LockObject(output);
        output.Add(omr);
        UnLockObject(output);
      end;
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure DoFor_;
  var
    pass, j: Integer;
    p: PHashListData;
    od_desc: TOD_Desc;
    omr: TOD_Marshal_Rect;
  begin
    for pass := 0 to lst.Count - 1 do
      begin
        p := PHashListData(lst[pass]);
        od_desc := OD_Process(p^.data, rgb_img, 1024);
        for j := low(od_desc) to high(od_desc) do
          begin
            omr.R := RectV2(od_desc[j]);
            omr.confidence := od_desc[j].confidence;
            omr.Token := p^.OriginName;
            output.Add(omr);
          end;
      end;
  end;
{$ENDIF parallel}
  procedure FillResult_;
  var
    i: Integer;
  begin
    SetLength(Result, output.Count);
    for i := 0 to output.Count - 1 do
        Result[i] := output[i];
  end;

begin
  output := TOD_Marshal_List.Create;
  lst := TCoreClassList.Create;
  hnd.GetListData(lst);

  rgb_img := AI_Ptr^.Prepare_RGB_Image(Raster.bits, Raster.width, Raster.height);

{$IFDEF parallel}
{$IFDEF FPC}
  ProcThreadPool.DoParallelLocalProc(@FPC_ParallelFor, 0, lst.Count - 1);
{$ELSE FPC}
  TParallel.for(0, lst.Count - 1, procedure(pass: Integer)
    var
      j: Integer;
      p: PHashListData;
      od_desc: TOD_Desc;
      omr: TOD_Marshal_Rect;
    begin
      p := PHashListData(lst[pass]);
      od_desc := OD_Process(p^.data, rgb_img, 1024);
      for j := low(od_desc) to high(od_desc) do
        begin
          omr.R := RectV2(od_desc[j]);
          omr.confidence := od_desc[j].confidence;
          omr.Token := p^.OriginName;
          LockObject(output);
          output.Add(omr);
          UnLockObject(output);
        end;
    end);
{$ENDIF FPC}
{$ELSE parallel}
  DoFor_;
{$ENDIF parallel}
  FillResult_;

  AI_Ptr^.Close_RGB_Image(rgb_img);

  DisposeObject(output);
  DisposeObject(lst);
end;

function TAI.OD_Marshal_ProcessScaleSpace(hnd: TOD_Marshal_Handle; Raster: TMemoryRaster; scale: TGeoFloat): TOD_Marshal_Desc;
var
  nr: TMemoryRaster;
  buff: TOD_Marshal_Desc;
  i: Integer;
begin
  nr := NewRaster();
  nr.ZoomFrom(Raster, scale);

  buff := OD_Marshal_Process(hnd, nr);

  SetLength(Result, length(buff));

  for i := 0 to length(buff) - 1 do
    begin
      Result[i].R := RectDiv(buff[i].R, scale);
      Result[i].Token := buff[i].Token;
    end;

  SetLength(buff, 0);
  DisposeObject(nr);
end;

function TAI.SP_Train(train_cfg, train_output: TPascalString; oversampling_amount, tree_depth, thread_num: Integer): Boolean;
var
  train_cfg_buff, train_output_buff: P_Bytes;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.SP_Train) and (umlFileExists(train_cfg)) and (train_output.Len > 0) then
    begin
      train_cfg_buff := Alloc_P_Bytes(train_cfg);
      train_output_buff := Alloc_P_Bytes(train_output);

      try
          Result := AI_Ptr^.SP_Train(train_cfg_buff, train_output_buff, oversampling_amount, tree_depth, thread_num) = 0;
      except
          Result := False;
      end;

      Free_P_Bytes(train_cfg_buff);
      Free_P_Bytes(train_output_buff);

      if not Result then
          DoStatus('ZAI: Shape Predictor Train failed.');
    end
  else
      Result := False;
end;

function TAI.SP_Train(imgList: TAI_ImageList; train_output: TPascalString; oversampling_amount, tree_depth, thread_num: Integer): Boolean;
var
  ph, fn, prefix: TPascalString;
  tmpFileList: TPascalStringList;
  i: Integer;
begin
  ph := rootPath;
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

function TAI.SP_Train(imgMat: TAI_ImageMatrix; train_output: TPascalString; oversampling_amount, tree_depth, thread_num: Integer): Boolean;
var
  ph, fn, prefix: TPascalString;
  tmpFileList: TPascalStringList;
  i: Integer;
begin
  ph := rootPath;
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
  fn: TPascalString;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(rootPath, PFormat('temp_SP_%s' + C_SP_Ext, [umlMakeRanName.Text]));

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
  fn: TPascalString;
begin
  Result := nil;
  TCoreClassThread.Sleep(1);
  fn := umlCombineFileName(rootPath, PFormat('temp_SP_%s' + C_SP_Ext, [umlMakeRanName.Text]));

  if SP_Train(imgMat, fn, oversampling_amount, tree_depth, thread_num) then
    if umlFileExists(fn) then
      begin
        Result := TMemoryStream64.Create;
        Result.LoadFromFile(fn);
        Result.Position := 0;
      end;
  umlDeleteFile(fn);
end;

function TAI.SP_Open(train_file: TPascalString): TSP_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.SP_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      Result := AI_Ptr^.SP_Init(train_file_buff);
      Free_P_Bytes(train_file_buff);
      if Result <> nil then
          DoStatus('shape predictor open: %s', [train_file.Text]);
    end
  else
      Result := nil;
end;

function TAI.SP_Open_Stream(stream: TMemoryStream64): TSP_Handle;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.SP_Init_Memory) then
    begin
      Result := AI_Ptr^.SP_Init_Memory(stream.memory, stream.Size);
      if Result <> nil then
          DoStatus('shape predictor open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.SP_Open_Stream(train_file: TPascalString): TSP_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := SP_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('shape predictor open: %s', [train_file.Text]);
end;

function TAI.SP_Close(var hnd: TSP_Handle): Boolean;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.SP_Free) and (hnd <> nil) then
    begin
      Result := AI_Ptr^.SP_Free(hnd) = 0;
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
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.SP_Process) then
      exit;
  SetLength(Result, max_AI_Point);

  try
    if AI_Ptr^.SP_Process(hnd, Raster.bits, Raster.width, Raster.height,
      @AI_Rect, @Result[0], max_AI_Point, point_num) > 0 then
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
  desc := SP_Process(hnd, Raster, AIRect(R), 1024);
  Result := TVec2List.Create;
  for i := 0 to length(desc) - 1 do
      Result.Add(Vec2(desc[i]));
end;

function TAI.SP_Process_Vec2(hnd: TSP_Handle; Raster: TMemoryRaster; const R: TRectV2): TArrayVec2;
var
  desc: TSP_Desc;
  i: Integer;
begin
  desc := SP_Process(hnd, Raster, AIRect(R), 1024);
  SetLength(Result, length(desc));
  for i := 0 to length(desc) - 1 do
      Result[i] := Vec2(desc[i]);
end;

function TAI.SP_Process_Vec2(hnd: TSP_Handle; Raster: TMemoryRaster; const R: TAI_Rect): TArrayVec2;
var
  desc: TSP_Desc;
  i: Integer;
begin
  desc := SP_Process(hnd, Raster, R, 1024);
  SetLength(Result, length(desc));
  for i := 0 to length(desc) - 1 do
      Result[i] := Vec2(desc[i]);
end;

function TAI.SP_Process_Vec2(hnd: TSP_Handle; Raster: TMemoryRaster; const R: TOD_Rect): TArrayVec2;
var
  desc: TSP_Desc;
  i: Integer;
begin
  desc := SP_Process(hnd, Raster, AIRect(R), 1024);
  SetLength(Result, length(desc));
  for i := 0 to length(desc) - 1 do
      Result[i] := Vec2(desc[i]);
end;

procedure TAI.PrepareFaceDataSource;
var
  m64: TMemoryStream64;
begin
  Wait_AI_Init;
  try
    if (face_sp_hnd = nil) then
      begin
        m64 := TMemoryStream64.Create;
        m64.SetPointerWithProtectedMode(build_in_face_shape_memory, build_in_face_shape_memory_siz);
        m64.Position := 0;
        face_sp_hnd := SP_Open_Stream(m64);
        DisposeObject(m64);
      end;
  except
      face_sp_hnd := nil;
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
  Result := nil;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.SP_extract_face_rect_desc_chips) then
      exit;
  if length(desc) = 0 then
      exit;

  PrepareFaceDataSource;
  SetLength(fixed_desc, length(desc));
  for i := 0 to length(desc) - 1 do
      fixed_desc[i] := AIRect(RectScaleSpace(RectV2(desc[i]), extract_face_size, extract_face_size));

  try
      Result := AI_Ptr^.SP_extract_face_rect_desc_chips(face_sp_hnd, Raster.bits, Raster.width, Raster.height, extract_face_size, @fixed_desc[0], length(desc));
  except
      Result := nil;
  end;
  SetLength(fixed_desc, 0);
end;

function TAI.Face_Detector(Raster: TMemoryRaster; mmod_desc: TMMOD_Desc; extract_face_size: Integer): TFACE_Handle;
var
  i: Integer;
  ai_rect_desc: TAI_Rect_Desc;
begin
  SetLength(ai_rect_desc, length(mmod_desc));
  for i := 0 to length(mmod_desc) - 1 do
      ai_rect_desc[i] := AIRect(mmod_desc[i].R);
  Result := Face_Detector(Raster, ai_rect_desc, extract_face_size);
  SetLength(ai_rect_desc, 0);
end;

function TAI.Face_Detector(Raster: TMemoryRaster; od_desc: TOD_Desc; extract_face_size: Integer): TFACE_Handle;
var
  i: Integer;
  ai_rect_desc: TAI_Rect_Desc;
begin
  SetLength(ai_rect_desc, length(od_desc));
  for i := 0 to length(od_desc) - 1 do
      ai_rect_desc[i] := AIRect(od_desc[i]);
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
  Result := Face_Detector_All(Raster, C_Metric_ResNet_Image_Size);
end;

function TAI.Face_Detector_All(Raster: TMemoryRaster; extract_face_size: Integer): TFACE_Handle;
begin
  Result := nil;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.SP_extract_face_rect_chips) then
      exit;
  PrepareFaceDataSource;
  try
      Result := AI_Ptr^.SP_extract_face_rect_chips(face_sp_hnd, Raster.bits, Raster.width, Raster.height, extract_face_size);
  except
      Result := nil;
  end;
end;

function TAI.Face_Detector_Rect(Raster: TMemoryRaster): TFACE_Handle;
begin
  Result := nil;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.SP_extract_face_rect) then
      exit;

  try
      Result := AI_Ptr^.SP_extract_face_rect(Raster.bits, Raster.width, Raster.height);
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
  for i := 0 to length(Result) - 1 do
      Result[i] := Face_Rect(face_hnd, i);
  Face_Close(face_hnd);
end;

function TAI.Face_chips_num(hnd: TFACE_Handle): Integer;
begin
  Result := 0;
  if hnd = nil then
      exit;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.SP_get_face_chips_num) then
      exit;

  Result := AI_Ptr^.SP_get_face_chips_num(hnd);
end;

function TAI.Face_chips(hnd: TFACE_Handle; index: Integer): TMemoryRaster;
var
  w, h: Integer;
begin
  Result := nil;
  if hnd = nil then
      exit;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.SP_get_face_chips_size) then
      exit;
  if not Assigned(AI_Ptr^.SP_get_face_chips_bits) then
      exit;

  Result := NewRaster();
  AI_Ptr^.SP_get_face_chips_size(hnd, index, w, h);
  Result.SetSize(w, h);
  AI_Ptr^.SP_get_face_chips_bits(hnd, index, Result.bits);
end;

function TAI.Face_Rect_Num(hnd: TFACE_Handle): Integer;
begin
  Result := 0;
  if hnd = nil then
      exit;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.SP_get_face_rect_num) then
      exit;

  Result := AI_Ptr^.SP_get_face_rect_num(hnd);
end;

function TAI.Face_Rect(hnd: TFACE_Handle; index: Integer): TAI_Rect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := 0;
  Result.Bottom := 0;
  if hnd = nil then
      exit;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.SP_get_face_rect) then
      exit;

  AI_Ptr^.SP_get_face_rect(hnd, index, Result);
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
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.SP_get_num) then
      exit;

  Result := AI_Ptr^.SP_get_num(hnd);
end;

function TAI.Face_Shape(hnd: TFACE_Handle; index: Integer): TSP_Desc;
var
  sp_num: Integer;
begin
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.SP_get) then
      exit;

  SetLength(Result, 1024);
  sp_num := AI_Ptr^.SP_get(hnd, index, @Result[0], length(Result));
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
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.SP_get) then
      exit;

  SetLength(buff, 1024);
  sp_num := AI_Ptr^.SP_get(hnd, index, @buff[0], length(buff));
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
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.SP_close_face_chips_handle) then
      exit;

  AI_Ptr^.SP_close_face_chips_handle(hnd);
  hnd := nil;
end;

class function TAI.Init_Metric_ResNet_Parameter(train_sync_file, train_output: TPascalString): PMetric_ResNet_Train_Parameter;
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

function TAI.Metric_ResNet_Train(imgList: TMemoryRaster2DArray; param: PMetric_ResNet_Train_Parameter): Boolean;
var
  i, j, imgSum, ri: Integer;
  imgArry: TMemoryRasterArray;
  rArry: array of TAI_Raster_Data;
begin
  Result := False;

  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.MDNN_ResNet_Train) then
      exit;

  imgSum := 0;
  for i := 0 to length(imgList) - 1 do
      inc(imgSum, length(imgList[i]));

  if imgSum = 0 then
      exit;

  // process sequence
  SetLength(rArry, imgSum);
  ri := 0;
  for i := 0 to length(imgList) - 1 do
    begin
      imgArry := imgList[i];
      for j := 0 to length(imgArry) - 1 do
        begin
          rArry[ri].raster_ptr := imgArry[j].bits;
          rArry[ri].width := imgArry[j].width;
          rArry[ri].height := imgArry[j].height;
          rArry[ri].index := i;
          inc(ri);
        end;
    end;

  // set arry
  param^.imgArry_ptr := PAI_Raster_Data_Array(@rArry[0]);
  param^.img_num := length(rArry);
  param^.control := @TrainingControl;

  // execute train
  TrainingControl.pause := 0;
  TrainingControl.stop := 0;

  // run train
  if param^.fullGPU_Training then
      Result := AI_Ptr^.MDNN_ResNet_Full_GPU_Train(param) >= 0
  else
      Result := AI_Ptr^.MDNN_ResNet_Train(param) >= 0;

  Last_training_average_loss := param^.training_average_loss;
  Last_training_learning_rate := param^.training_learning_rate;

  // reset arry
  param^.imgArry_ptr := nil;
  param^.img_num := 0;

  // free res
  SetLength(rArry, 0);
end;

function TAI.Metric_ResNet_Train(imgList: TAI_ImageList; param: PMetric_ResNet_Train_Parameter): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
begin
  Result := False;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.MDNN_ResNet_Train) then
      exit;

  imgBuff := imgList.ExtractDetectorDefineAsPrepareRaster(C_Metric_ResNet_Image_Size, C_Metric_ResNet_Image_Size);

  if length(imgBuff) = 0 then
      exit;

  Result := Metric_ResNet_Train(imgBuff, param);

  for i := 0 to length(imgBuff) - 1 do
    for j := 0 to length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.Metric_ResNet_Train_Stream(imgList: TAI_ImageList; param: PMetric_ResNet_Train_Parameter): TMemoryStream64;
var
  fn: TPascalString;
begin
  Result := nil;

  if Metric_ResNet_Train(imgList, param) then
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

function TAI.Metric_ResNet_Train(imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
begin
  Result := False;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.MDNN_ResNet_Train) then
      exit;

  imgBuff := imgMat.ExtractDetectorDefineAsPrepareRaster(C_Metric_ResNet_Image_Size, C_Metric_ResNet_Image_Size);

  if length(imgBuff) = 0 then
      exit;

  Result := Metric_ResNet_Train(imgBuff, param);

  for i := 0 to length(imgBuff) - 1 do
    for j := 0 to length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.Metric_ResNet_Train_Stream(imgMat: TAI_ImageMatrix; param: PMetric_ResNet_Train_Parameter): TMemoryStream64;
var
  fn: TPascalString;
begin
  Result := nil;

  if Metric_ResNet_Train(imgMat, param) then
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

function TAI.Metric_ResNet_Open(train_file: TPascalString): TMDNN_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.MDNN_ResNet_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      Result := AI_Ptr^.MDNN_ResNet_Init(train_file_buff);
      Free_P_Bytes(train_file_buff);
      if Result <> nil then
          DoStatus('MDNN-ResNet(ResNet matric DNN) open: %s', [train_file.Text]);
    end
  else
      Result := nil;
end;

function TAI.Metric_ResNet_Open_Stream(stream: TMemoryStream64): TMDNN_Handle;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.MDNN_ResNet_Init_Memory) then
    begin
      Result := AI_Ptr^.MDNN_ResNet_Init_Memory(stream.memory, stream.Size);
      if Result <> nil then
          DoStatus('MDNN-ResNet(ResNet matric DNN) open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.Metric_ResNet_Open_Stream(train_file: TPascalString): TMDNN_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := Metric_ResNet_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('MDNN-ResNet(ResNet matric DNN) open: %s', [train_file.Text]);
end;

function TAI.Metric_ResNet_Close(var hnd: TMDNN_Handle): Boolean;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.MDNN_ResNet_Free) and (hnd <> nil) then
    begin
      Result := AI_Ptr^.MDNN_ResNet_Free(hnd) = 0;
      DoStatus('MDNN-ResNet(ResNet matric DNN) close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

function TAI.Metric_ResNet_Process(hnd: TMDNN_Handle; RasterArray: TMemoryRasterArray; output: PDouble): Integer;
var
  rArry: array of TAI_Raster_Data;
  i: Integer;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.MDNN_ResNet_Process) then
    begin
      SetLength(rArry, length(RasterArray));
      for i := 0 to length(RasterArray) - 1 do
        begin
          rArry[i].raster_ptr := RasterArray[i].bits;
          rArry[i].width := RasterArray[i].width;
          rArry[i].height := RasterArray[i].height;
          rArry[i].index := i;
        end;

      Result := AI_Ptr^.MDNN_ResNet_Process(hnd, PAI_Raster_Data_Array(@rArry[0]), length(rArry), output);
    end
  else
      Result := -2;
end;

function TAI.Metric_ResNet_Process(hnd: TMDNN_Handle; RasterArray: TMemoryRasterArray): TLMatrix;
var
  l: TLVec;
  i: TLInt;
begin
  Result := LMatrix(0, 0);
  SetLength(l, length(RasterArray) * C_Metric_ResNet_Dim);
  if Metric_ResNet_Process(hnd, RasterArray, @l[0]) > 0 then
    begin
      Result := LMatrix(length(RasterArray), 0);
      for i := Low(Result) to high(Result) do
          Result[i] := LVecCopy(l, i * C_Metric_ResNet_Dim, C_Metric_ResNet_Dim);
    end;
  SetLength(l, 0);
end;

function TAI.Metric_ResNet_Process(hnd: TMDNN_Handle; Raster: TMemoryRaster): TLVec;
var
  rArry: TMemoryRasterArray;
begin
  SetLength(Result, C_Metric_ResNet_Dim);
  SetLength(rArry, 1);
  rArry[0] := Raster;
  if Metric_ResNet_Process(hnd, rArry, @Result[0]) <= 0 then
      SetLength(Result, 0);
end;

procedure TAI.Metric_ResNet_SaveDetectorDefineToLearnEngine(mdnn_hnd: TMDNN_Handle; imgList: TAI_ImageList; lr: TLearn);
var
  i, j: Integer;
  imgData: TAI_Image;
  DetDef: TAI_DetectorDefine;
  mr: TMemoryRaster;
  v: TLVec;
begin
  if lr.InLen <> C_Metric_ResNet_Dim then
      RaiseInfo('Learn InLen illegal');
  for i := 0 to imgList.Count - 1 do
    begin
      imgData := imgList[i];
      for j := 0 to imgData.DetectorDefineList.Count - 1 do
        begin
          DetDef := imgData.DetectorDefineList[j];
          if DetDef.Token.Len > 0 then
            if not DetDef.PrepareRaster.Empty then
              begin
                mr := NewRaster();
                mr.ZoomFrom(DetDef.PrepareRaster, C_Metric_ResNet_Image_Size, C_Metric_ResNet_Image_Size);
                v := Metric_ResNet_Process(mdnn_hnd, mr);
                DisposeObject(mr);
                if length(v) <> C_Metric_ResNet_Dim then
                    DoStatus('Metric-ResNet vector error!')
                else
                    lr.AddMemory(v, DetDef.Token);
              end;
        end;
    end;
end;

procedure TAI.Metric_ResNet_SaveDetectorDefineToLearnEngine(mdnn_hnd: TMDNN_Handle; imgMat: TAI_ImageMatrix; lr: TLearn);
var
  i: Integer;
begin
  for i := 0 to imgMat.Count - 1 do
      Metric_ResNet_SaveDetectorDefineToLearnEngine(mdnn_hnd, imgMat[i], lr);
end;

function TAI.Metric_ResNet_DebugInfo(hnd: TMDNN_Handle): TPascalString;
var
  p: PPascalString;
begin
  Result := '';
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.MDNN_DebugInfo) and (hnd <> nil) then
    begin
      AI_Ptr^.MDNN_DebugInfo(hnd, p);
      Result := p^;
      Dispose(p);
    end;
end;

class function TAI.Init_MMOD_DNN_TrainParam(train_cfg, train_sync_file, train_output: TPascalString): PMMOD_Train_Parameter;
begin
  new(Result);
  FillPtrByte(Result, SizeOf(TMMOD_Train_Parameter), 0);

  Result^.train_cfg := Alloc_P_Bytes(train_cfg);
  Result^.train_sync_file := Alloc_P_Bytes(train_sync_file);
  Result^.train_output := Alloc_P_Bytes(train_output);

  Result^.timeout := C_Tick_Hour;
  Result^.target_size := 100;
  Result^.min_target_size := 20;
  Result^.min_detector_window_overlap_iou := 0.75;
  Result^.iterations_without_progress_threshold := 500;
  Result^.learning_rate := 0.1;
  Result^.completed_learning_rate := 0.0001;
  Result^.num_crops := 10;
  Result^.chip_dims_x := 150;
  Result^.chip_dims_y := 150;
  Result^.min_object_size_x := 95;
  Result^.min_object_size_y := 19;
  Result^.max_rotation_degrees := 50.0;
  Result^.max_object_size := 0.7;

  Result^.control := nil;
  Result^.training_average_loss := 0;
  Result^.training_learning_rate := 0;

  // internal
  Result^.TempFiles := nil;
end;

class procedure TAI.Free_MMOD_DNN_TrainParam(param: PMMOD_Train_Parameter);
begin
  Free_P_Bytes(param^.train_cfg);
  Free_P_Bytes(param^.train_sync_file);
  Free_P_Bytes(param^.train_output);
  Dispose(param);
end;

function TAI.MMOD_DNN_PrepareTrain(imgList: TAI_ImageList; train_sync_file: TPascalString): PMMOD_Train_Parameter;
var
  ph, fn, prefix, train_out: TPascalString;
  tmpFileList: TPascalStringList;
begin
  ph := rootPath;
  tmpFileList := TPascalStringList.Create;
  TCoreClassThread.Sleep(1);
  prefix := 'MMOD_DNN_' + umlMakeRanName + '_';
  fn := umlCombineFileName(ph, prefix + 'temp.xml');
  imgList.Build_XML(True, False, 'ZAI dataset', 'dnn resnet max-margin dataset', fn, prefix, tmpFileList);
  train_out := prefix + 'output' + C_MMOD_Ext;
  Result := Init_MMOD_DNN_TrainParam(fn, train_sync_file, train_out);
  Result^.control := @TrainingControl;
  Result^.TempFiles := tmpFileList;
end;

function TAI.MMOD_DNN_PrepareTrain(imgMat: TAI_ImageMatrix; train_sync_file: TPascalString): PMMOD_Train_Parameter;
var
  ph, fn, prefix, train_out: TPascalString;
  tmpFileList: TPascalStringList;
begin
  ph := rootPath;
  tmpFileList := TPascalStringList.Create;
  TCoreClassThread.Sleep(1);
  prefix := 'MMOD_DNN_' + umlMakeRanName + '_';
  fn := umlCombineFileName(ph, prefix + 'temp.xml');
  imgMat.Build_XML(True, False, 'ZAI dataset', 'build-in', fn, prefix, tmpFileList);
  train_out := prefix + 'output' + C_MMOD_Ext;
  Result := Init_MMOD_DNN_TrainParam(fn, train_sync_file, train_out);
  Result^.control := @TrainingControl;
  Result^.TempFiles := tmpFileList;
end;

function TAI.MMOD_DNN_Train(param: PMMOD_Train_Parameter): Integer;
begin
  Result := -1;
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.MMOD_DNN_Train) then
    begin
      TrainingControl.pause := 0;
      TrainingControl.stop := 0;
      Result := AI_Ptr^.MMOD_DNN_Train(param);
      Last_training_average_loss := param^.training_average_loss;
      Last_training_learning_rate := param^.training_learning_rate;
      if Result > 0 then
          param^.TempFiles.Add(Get_P_Bytes_String(param^.train_output));
    end;
end;

function TAI.MMOD_DNN_Train_Stream(param: PMMOD_Train_Parameter): TMemoryStream64;
var
  fn: TPascalString;
begin
  Result := nil;
  fn := Get_P_Bytes_String(param^.train_output);
  if (MMOD_DNN_Train(param) > 0) and (umlFileExists(fn)) then
    begin
      Result := TMemoryStream64.Create;
      Result.LoadFromFile(fn);
      Result.Position := 0;
    end;
end;

procedure TAI.MMOD_DNN_FreeTrain(param: PMMOD_Train_Parameter);
var
  i: Integer;
begin
  for i := 0 to param^.TempFiles.Count - 1 do
      umlDeleteFile(param^.TempFiles[i]);
  DisposeObject(param^.TempFiles);
  param^.TempFiles := nil;

  Free_MMOD_DNN_TrainParam(param);
end;

function TAI.MMOD_DNN_Open(train_file: TPascalString): TMMOD_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.MMOD_DNN_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      Result := AI_Ptr^.MMOD_DNN_Init(train_file_buff);
      Free_P_Bytes(train_file_buff);
      if Result <> nil then
          DoStatus('MMOD-DNN(DNN+SVM:max-margin object detector) open: %s', [train_file.Text]);
    end
  else
      Result := nil;
end;

function TAI.MMOD_DNN_Open_Stream(stream: TMemoryStream64): TMMOD_Handle;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.MMOD_DNN_Init_Memory) then
    begin
      Result := AI_Ptr^.MMOD_DNN_Init_Memory(stream.memory, stream.Size);
      if Result <> nil then
          DoStatus('MMOD-DNN(DNN+SVM:max-margin object detector) open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.MMOD_DNN_Open_Stream(train_file: TPascalString): TMMOD_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := MMOD_DNN_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('MMOD-DNN(DNN+SVM:max-margin object detector) open: %s', [train_file.Text]);
end;

function TAI.MMOD_DNN_Close(var hnd: TMMOD_Handle): Boolean;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.MMOD_DNN_Free) and (hnd <> nil) then
    begin
      Result := AI_Ptr^.MMOD_DNN_Free(hnd) = 0;
      DoStatus('MMOD-DNN(DNN+SVM:max-margin object detector) close.', []);
    end
  else
      Result := False;

  hnd := nil;
end;

function TAI.MMOD_DNN_Process(hnd: TMMOD_Handle; Raster: TMemoryRaster): TMMOD_Desc;
var
  rect_num: Integer;
  buff: TAI_MMOD_Desc;
  i: Integer;
begin
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.MMOD_DNN_Process) then
      exit;
  SetLength(buff, 1024);

  rect_num := AI_Ptr^.MMOD_DNN_Process(hnd, Raster.bits, Raster.width, Raster.height, @buff[0], 1024);

  if rect_num >= 0 then
    begin
      SetLength(Result, rect_num);
      for i := 0 to rect_num - 1 do
        begin
          Result[i].R := RectV2(buff[i]);
          Result[i].confidence := buff[i].confidence;
          Result[i].Token := buff[i].Token^;
          AI_FreeString(buff[i].Token);
        end;
    end;
  SetLength(buff, 0);
end;

function TAI.MMOD_DNN_Process_Matrix(hnd: TMMOD_Handle; matrix_img: TMatrix_Image_Handle): TMMOD_Desc;
var
  rect_num: Integer;
  buff: TAI_MMOD_Desc;
  i: Integer;
begin
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.MMOD_DNN_Process) then
      exit;
  SetLength(buff, 1024);

  rect_num := AI_Ptr^.MMOD_DNN_Process_Image(hnd, matrix_img, @buff[0], 1024);
  if rect_num >= 0 then
    begin
      SetLength(Result, rect_num);
      for i := 0 to rect_num - 1 do
        begin
          Result[i].R := RectV2(buff[i]);
          Result[i].confidence := buff[i].confidence;
          Result[i].Token := buff[i].Token^;
          AI_FreeString(buff[i].Token);
        end;
    end;
  SetLength(buff, 0);
end;

function TAI.MMOD_DNN_DebugInfo(hnd: TMMOD_Handle): TPascalString;
var
  p: PPascalString;
begin
  Result := '';
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.MMOD_DebugInfo) and (hnd <> nil) then
    begin
      AI_Ptr^.MMOD_DebugInfo(hnd, p);
      Result := p^;
      Dispose(p);
    end;
end;

class function TAI.Init_RNIC_Train_Parameter(train_sync_file, train_output: TPascalString): PRNIC_Train_Parameter;
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

function TAI.RNIC_Train(imgList: TMemoryRaster2DArray; param: PRNIC_Train_Parameter; Train_OutputIndex: TMemoryRasterList): Boolean;
var
  i, j, imgSum, ri: Integer;
  imgArry: TMemoryRasterArray;
  imgInfo_arry: array of TAI_Raster_Data;
begin
  Result := False;

  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.RNIC_Train) then
      exit;

  imgSum := 0;
  for i := 0 to length(imgList) - 1 do
      inc(imgSum, length(imgList[i]));

  if Train_OutputIndex <> nil then
      Train_OutputIndex.Clear;
  SetLength(imgInfo_arry, imgSum);
  ri := 0;

  for i := 0 to length(imgList) - 1 do
    begin
      imgArry := imgList[i];
      for j := 0 to length(imgArry) - 1 do
        begin
          imgInfo_arry[ri].raster_ptr := imgArry[j].bits;
          imgInfo_arry[ri].width := imgArry[j].width;
          imgInfo_arry[ri].height := imgArry[j].height;
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
  param^.img_num := length(imgInfo_arry);
  param^.control := @TrainingControl;

  Result := AI_Ptr^.RNIC_Train(param) > 0;

  Last_training_average_loss := param^.training_average_loss;
  Last_training_learning_rate := param^.training_learning_rate;

  param^.imgArry_ptr := nil;
  param^.img_num := 0;
  param^.control := nil;

  SetLength(imgInfo_arry, 0);
end;

function TAI.RNIC_Train(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; Train_OutputIndex: TPascalStringList): Boolean;
var
  imgBuff: TMemoryRaster2DArray;
  i, j: Integer;
  out_index: TMemoryRasterList;
begin
  Result := False;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.RNIC_Train) then
      exit;

  Train_OutputIndex.Clear;
  imgBuff := imgList.ExtractDetectorDefineAsSnapshot();
  out_index := TMemoryRasterList.Create;
  Result := RNIC_Train(imgBuff, param, out_index);
  if Result then
    for i := 0 to out_index.Count - 1 do
      if Train_OutputIndex.ExistsValue(out_index[i].UserToken) < 0 then
          Train_OutputIndex.Add(out_index[i].UserToken);
  DisposeObject(out_index);

  for i := 0 to length(imgBuff) - 1 do
    for j := 0 to length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.RNIC_Train(imgList: TAI_ImageList; param: PRNIC_Train_Parameter; train_index_output: TPascalString): Boolean;
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
  fn: TPascalString;
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
begin
  Result := False;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.RNIC_Train) then
      exit;

  Train_OutputIndex.Clear;
  imgBuff := imgMat.ExtractDetectorDefineAsSnapshot();
  out_index := TMemoryRasterList.Create;
  Result := RNIC_Train(imgBuff, param, out_index);
  if Result then
    for i := 0 to out_index.Count - 1 do
      if Train_OutputIndex.ExistsValue(out_index[i].UserToken) < 0 then
          Train_OutputIndex.Add(out_index[i].UserToken);
  DisposeObject(out_index);

  for i := 0 to length(imgBuff) - 1 do
    for j := 0 to length(imgBuff[i]) - 1 do
        DisposeObject(imgBuff[i, j]);
  SetLength(imgBuff, 0, 0);
end;

function TAI.RNIC_Train(imgMat: TAI_ImageMatrix; param: PRNIC_Train_Parameter; train_index_output: TPascalString): Boolean;
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
  fn: TPascalString;
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

function TAI.RNIC_Open(train_file: TPascalString): TRNIC_Handle;
var
  train_file_buff: P_Bytes;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.RNIC_Init) then
    begin
      train_file_buff := Alloc_P_Bytes(train_file);
      Result := AI_Ptr^.RNIC_Init(train_file_buff);
      Free_P_Bytes(train_file_buff);
      if Result <> nil then
          DoStatus('ResNet-Image-Classifier open: %s', [train_file.Text]);
    end
  else
      Result := nil;
end;

function TAI.RNIC_Open_Stream(stream: TMemoryStream64): TRNIC_Handle;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.RNIC_Init_Memory) then
    begin
      Result := AI_Ptr^.RNIC_Init_Memory(stream.memory, stream.Size);
      DoStatus('ResNet-Image-Classifier open memory %s size:%s', [umlPointerToStr(stream.memory).Text, umlSizeToStr(stream.Size).Text]);
    end
  else
      Result := nil;
end;

function TAI.RNIC_Open_Stream(train_file: TPascalString): TRNIC_Handle;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.LoadFromFile(train_file);
  Result := RNIC_Open_Stream(m64);
  DisposeObject(m64);
  if Result <> nil then
      DoStatus('ResNet-Image-Classifier open: %s', [train_file.Text]);
end;

function TAI.RNIC_Close(var hnd: TRNIC_Handle): Boolean;
begin
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.RNIC_Free) and (hnd <> nil) then
    begin
      Result := AI_Ptr^.RNIC_Free(hnd) = 0;
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
  SetLength(Result, 0);
  if hnd = nil then
      exit;
  if AI_Ptr = nil then
      exit;
  if not Assigned(AI_Ptr^.RNIC_Process) then
      exit;
  SetLength(Result, C_ResNet_Image_Classifier_Dim);

  R := AI_Ptr^.RNIC_Process(hnd, num_crops, Raster.bits, Raster.width, Raster.height, @Result[0]);

  if R <> C_ResNet_Image_Classifier_Dim then
      SetLength(Result, 0);
end;

function TAI.RNIC_Process(hnd: TRNIC_Handle; Raster: TMemoryRaster): TLVec;
begin
  Result := RNIC_Process(hnd, Raster, 16);
end;

function TAI.RNIC_DebugInfo(hnd: TRNIC_Handle): TPascalString;
var
  p: PPascalString;
begin
  Result := '';
  if (AI_Ptr <> nil) and Assigned(AI_Ptr^.RNIC_DebugInfo) and (hnd <> nil) then
    begin
      AI_Ptr^.RNIC_DebugInfo(hnd, p);
      Result := p^;
      Dispose(p);
    end;
end;

function TAI.Tracker_Open(Raster: TMemoryRaster; const track_rect: TRect): TTracker_Handle;
var
  rgb_hnd: TRGB_Image_Handle;
  a: TAI_Rect;
begin
  Result := nil;
  if (AI_Ptr = nil) then
      exit;
  if not Assigned(AI_Ptr^.Start_Tracker) then
      exit;
  rgb_hnd := Prepare_RGB_Image(Raster);
  if rgb_hnd = nil then
      exit;
  a := AIRect(track_rect);
  Result := AI_Ptr^.Start_Tracker(rgb_hnd, @a);
  Close_RGB_Image(rgb_hnd);
end;

function TAI.Tracker_Open(Raster: TMemoryRaster; const track_rect: TRectV2): TTracker_Handle;
var
  rgb_hnd: TRGB_Image_Handle;
  a: TAI_Rect;
begin
  Result := nil;
  if (AI_Ptr = nil) then
      exit;
  if not Assigned(AI_Ptr^.Start_Tracker) then
      exit;
  rgb_hnd := Prepare_RGB_Image(Raster);
  if rgb_hnd = nil then
      exit;
  a := AIRect(track_rect);
  Result := AI_Ptr^.Start_Tracker(rgb_hnd, @a);
  Close_RGB_Image(rgb_hnd);
end;

function TAI.Tracker_Update(hnd: TTracker_Handle; Raster: TMemoryRaster; var track_rect: TRect): Double;
var
  rgb_hnd: TRGB_Image_Handle;
  a: TAI_Rect;
begin
  Result := 0;
  if (AI_Ptr = nil) then
      exit;
  if not Assigned(AI_Ptr^.Update_Tracker) then
      exit;

  rgb_hnd := Prepare_RGB_Image(Raster);
  if rgb_hnd = nil then
      exit;
  Result := AI_Ptr^.Update_Tracker(hnd, rgb_hnd, a);
  track_rect := Rect(a);
  Close_RGB_Image(rgb_hnd);
end;

function TAI.Tracker_Update(hnd: TTracker_Handle; Raster: TMemoryRaster; var track_rect: TRectV2): Double;
var
  rgb_hnd: TRGB_Image_Handle;
  a: TAI_Rect;
begin
  Result := 0;
  if (AI_Ptr = nil) then
      exit;
  if not Assigned(AI_Ptr^.Update_Tracker) then
      exit;

  rgb_hnd := Prepare_RGB_Image(Raster);
  if rgb_hnd = nil then
      exit;
  Result := AI_Ptr^.Update_Tracker(hnd, rgb_hnd, a);
  track_rect := RectV2(a);
  Close_RGB_Image(rgb_hnd);
end;

function TAI.Tracker_Close(var hnd: TTracker_Handle): Boolean;
begin
  Result := False;
  if (AI_Ptr = nil) then
      exit;
  if not Assigned(AI_Ptr^.Stop_Tracker) then
      exit;

  AI_Ptr^.Stop_Tracker(hnd);
  hnd := nil;
  Result := True;
end;

function TAI.Activted: Boolean;
begin
  Result := AI_Ptr <> nil;
end;

constructor TAI_Parallel.Create;
begin
  inherited Create;
  Critical := TSoftCritical.Create;
  Wait_AI_Init;
end;

destructor TAI_Parallel.Destroy;
begin
  Clear;
  DisposeObject(Critical);
  inherited Destroy;
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

procedure TAI_Parallel.Prepare_Parallel(lib_p: PAI_Entry; poolSiz: Integer);
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

procedure TAI_Parallel.Prepare_Parallel(eng: SystemString; poolSiz: Integer);
begin
  Prepare_Parallel(Prepare_AI_Engine(eng), poolSiz);
end;

procedure TAI_Parallel.Prepare_Parallel(poolSiz: Integer);
begin
  Prepare_Parallel(zAI_Common.AI_Engine_Library, poolSiz);
end;

procedure TAI_Parallel.Prepare_Parallel;
begin
  Prepare_Parallel(zAI_Common.AI_Engine_Library, zAI_Common.AI_Parallel_Count);
end;

procedure TAI_Parallel.Prepare_Face;
{$IFDEF parallel}
{$IFDEF FPC}
  procedure Do_ParallelFor(pass: PtrInt; data: Pointer; Item: TMultiThreadProcItem);
  begin
    Items[pass].PrepareFaceDataSource;
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure Do_For;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
        Items[pass].PrepareFaceDataSource;
  end;
{$ENDIF parallel}


begin
  Critical.Acquire;
  try
{$IFDEF parallel}
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@Do_ParallelFor, 0, Count - 1);
{$ELSE FPC}
    TParallel.for(0, Count - 1, procedure(pass: Integer)
      begin
        Items[pass].PrepareFaceDataSource;
      end);
{$ENDIF FPC}
{$ELSE parallel}
    Do_For;
{$ENDIF parallel}
  finally
      Critical.Release;
  end;
end;

procedure TAI_Parallel.Prepare_OD(stream: TMemoryStream64);
{$IFDEF parallel}
{$IFDEF FPC}
  procedure Do_ParallelFor(pass: PtrInt; data: Pointer; Item: TMultiThreadProcItem);
  begin
    with Items[pass] do
        Parallel_OD_Hnd := OD_Open_Stream(stream);
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure Do_For;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
      with Items[pass] do
          Parallel_OD_Hnd := OD_Open_Stream(stream);
  end;
{$ENDIF parallel}


begin
  Critical.Acquire;
  try
{$IFDEF parallel}
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@Do_ParallelFor, 0, Count - 1);
{$ELSE FPC}
    TParallel.for(0, Count - 1, procedure(pass: Integer)
      begin
        with Items[pass] do
            Parallel_OD_Hnd := OD_Open_Stream(stream);
      end);
{$ENDIF FPC}
{$ELSE parallel}
    Do_For;
{$ENDIF parallel}
  finally
      Critical.Release;
  end;
end;

procedure TAI_Parallel.Prepare_OD_Marshal(stream: TMemoryStream64);
{$IFDEF parallel}
{$IFDEF FPC}
  procedure Do_ParallelFor(pass: PtrInt; data: Pointer; Item: TMultiThreadProcItem);
  begin
    with Items[pass] do
        Parallel_OD_Marshal_Hnd := OD_Marshal_Open_Stream(stream);
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure Do_For;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
      with Items[pass] do
          Parallel_OD_Marshal_Hnd := OD_Marshal_Open_Stream(stream);
  end;
{$ENDIF parallel}


begin
  Critical.Acquire;
  try
{$IFDEF parallel}
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@Do_ParallelFor, 0, Count - 1);
{$ELSE FPC}
    TParallel.for(0, Count - 1, procedure(pass: Integer)
      begin
        with Items[pass] do
            Parallel_OD_Marshal_Hnd := OD_Marshal_Open_Stream(stream);
      end);
{$ENDIF FPC}
{$ELSE parallel}
    Do_For;
{$ENDIF parallel}
  finally
      Critical.Release;
  end;
end;

procedure TAI_Parallel.Prepare_SP(stream: TMemoryStream64);
{$IFDEF parallel}
{$IFDEF FPC}
  procedure Do_ParallelFor(pass: PtrInt; data: Pointer; Item: TMultiThreadProcItem);
  begin
    with Items[pass] do
        Parallel_SP_Hnd := SP_Open_Stream(stream);
  end;
{$ENDIF FPC}
{$ELSE parallel}
  procedure Do_For;
  var
    pass: Integer;
  begin
    for pass := 0 to Count - 1 do
      with Items[pass] do
          Parallel_SP_Hnd := SP_Open_Stream(stream);
  end;
{$ENDIF parallel}


begin
  Critical.Acquire;
  try
{$IFDEF parallel}
{$IFDEF FPC}
    ProcThreadPool.DoParallelLocalProc(@Do_ParallelFor, 0, Count - 1);
{$ELSE FPC}
    TParallel.for(0, Count - 1, procedure(pass: Integer)
      begin
        with Items[pass] do
            Parallel_SP_Hnd := SP_Open_Stream(stream);
      end);
{$ENDIF FPC}
{$ELSE parallel}
    Do_For;
{$ENDIF parallel}
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
    end;
  Critical.Release;
end;

procedure TAI_Parallel.UnLockAI(AI: TAI);
begin
  AI.Unlock;
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

initialization

Init_AI_BuildIn;
KeepPerformanceOnTraining := 0;

finalization

Free_AI_BuildIn;

end.
