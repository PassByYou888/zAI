unit FaceRec_GPU_DemoFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.ScrollBox, FMX.Memo,

  System.IOUtils,

  CoreClasses,
  Learn, LearnTypes,
  zAI, zAI_Common,
  zDrawEngineInterface_SlowFMX, zDrawEngine, Geometry2DUnit, MemoryRaster,
  MemoryStream64, PascalStrings, UnicodeMixedLib, DoStatusIO, FMX.Layouts, FMX.ExtCtrls;

type
  TFaceRecForm = class(TForm)
    FaceRecButton: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    ResetButton: TButton;
    Image1: TImageViewer;
    procedure ResetButtonClick(Sender: TObject);
    procedure FaceRecButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  public
    imgL: TAI_ImageList;
    AI: TAI;
    face_tile: TMemoryRaster;
    L_Engine: TLearn;
  end;

var
  FaceRecForm: TFaceRecForm;

implementation

{$R *.fmx}

procedure TFaceRecForm.ResetButtonClick(Sender: TObject);
var
  fn: U_String;

  procedure d(filename: U_String);
  begin
    DoStatus('删除文件 %s', [filename.Text]);
    umlDeleteFile(filename);
  end;

begin
  fn := umlCombineFileName(TPath.GetLibraryPath, 'lady_face' + C_Metric_ResNet_Ext);
  d(fn);
  d(fn + '.sync');
  d(fn + '.sync_');
  d(umlchangeFileExt(fn, '.learn'));
  MemoryBitmapToBitmap(face_tile, Image1.Bitmap);
end;

procedure TFaceRecForm.FaceRecButtonClick(Sender: TObject);
begin
  TComputeThread.RunP(nil, nil, procedure(Sender: TComputeThread)
    var
      fn, L_fn: U_String;
      param: PMetric_ResNet_Train_Parameter;
      training_successed: Boolean;
      mdnn_hnd: TMDNN_Handle;
      mmod_hnd: TMMOD_Handle;
      face_hnd: TFACE_Handle;
      mmod_desc: TMMOD_Desc;
      tk: TTimeTick;
      new_face_tile: TMemoryRaster;
      i: Integer;
      d: TDrawEngine;
      face_raster: TMemoryRaster;
      face_vec: TLVec;
      face_token: SystemString;
      face_rect: TRectV2;
    begin
      TThread.Synchronize(Sender, procedure
        begin
          FaceRecButton.Enabled := False;
          ResetButton.Enabled := False;
        end);
      try
        DoStatus('检查度量化神经网络库:%s', ['lady_face' + C_Metric_ResNet_Ext]);
        fn := umlCombineFileName(TPath.GetLibraryPath, 'lady_face' + C_Metric_ResNet_Ext);
        if not umlFileExists(fn) then
          begin
            // 这里我们用api方法来训练面部度量化的神经网络
            // 同样的训练也可以使用 TTrainingTask 方式
            DoStatus('开始训练度量化神经网络库:%s', ['lady_face' + C_Metric_ResNet_Ext]);
            param := TAI.Init_Metric_ResNet_Parameter(fn + '.sync', fn);

            // 在深度学习训练中，学习率是个不固定的东西，需要收敛
            // 收敛条件就是根据无效迭代器发生的次数来
            // 无效次数越小，学习速度就会越快，但是太小就会错过最佳收敛，最后得到模型将会失去准确度
            // 一般来说使用默认的值就可以
            // 处于快速demo，我将收敛值定义成了300，当人脸库很大，比如5000人的面部库，这个数值应该设置成500以上
            param^.iterations_without_progress_threshold := 300;
            param^.step_mini_batch_target_num := 4;
            param^.step_mini_batch_raster_num := 5;
            training_successed := AI.Metric_ResNet_Train(imgL, param);
            TAI.Free_Metric_ResNet_Parameter(param);

            if training_successed then
              begin
                DoStatus('训练成功');
              end
            else
              begin
                DoStatus('训练失败');
                exit;
              end;
          end;

        DoStatus('载入度量化神经网络 "%s"', [fn.Text]);
        mdnn_hnd := AI.Metric_ResNet_Open_Stream(fn);

        // learn学习这一步可以保存成文件，不必每次学习
        L_fn := umlchangeFileExt(fn, '.learn');
        DoStatus('检查度量化记忆库');
        if umlFileExists(L_fn) then
          begin
            DoStatus('读取度量化记忆库 "%s"', [L_fn.Text]);
            L_Engine.LoadFromFile(L_fn);
          end
        else
          begin
            DoStatus('Learn引擎正在学习Face度量', []);
            L_Engine.Clear;
            tk := GetTimeTick();
            AI.Metric_ResNet_SaveDetectorDefineToLearnEngine(mdnn_hnd, imgL, L_Engine);
            L_Engine.Train;
            DoStatus('学习Face度量，Learn记忆了 %d 张面部度量，耗时:%dms', [L_Engine.Count, GetTimeTick() - tk]);
            DoStatus('保存度量化记忆库 "%s"', [L_fn.Text]);
            L_Engine.SaveToFile(L_fn);
          end;

        // 因为zai的内置人脸数据集都采用高清图片训练，我们在实际应用中，这一步可以省却
        // 直接选用720p,1080p这类高清图像的数据源即可
        // 没有缩放后，性能将会得到提升
        DoStatus('对人脸做并行化高斯预处理.', []);
        new_face_tile := NewRaster();
        tk := GetTimeTick();
        new_face_tile.ZoomFrom(face_tile, face_tile.width * 2, face_tile.height * 2);
        DoStatus('并行化高斯预处理耗时:%dms', [GetTimeTick() - tk]);

        DoStatus('读取DNN-OD文件', []);
        mmod_hnd := AI.MMOD_DNN_Open_Stream(umlCombineFileName(TPath.GetLibraryPath, 'human_face_detector.svm_dnn_od'));

        // face检测使用gpu方式，检测完成后，输出mmod_desc，这是一个rect数组
        // 首次gpu检测需要展开cuda内存，face会比较慢，第二次gpu检测face将会得到提速
        // 上层api的只管调用，不需要关心底层
        DoStatus('正在检测人脸. demo图片分辨率 %d*%d', [new_face_tile.width, new_face_tile.height]);
        tk := GetTimeTick();
        mmod_desc := AI.MMOD_DNN_Process(mmod_hnd, new_face_tile);
        DoStatus('检测人脸完成. 发现 %d 张人脸，耗时:%dms', [length(mmod_desc), GetTimeTick() - tk]);

        // 基于mmod_desc数据，直接做sp对齐
        // 首次sp对齐需要展开stl的临时内存，第二次sp对齐就会得到提速
        // 上层api的只管调用，不需要关心底层
        DoStatus('正在对齐人脸. demo图片分辨率 %d*%d', [new_face_tile.width, new_face_tile.height]);
        tk := GetTimeTick();
        face_hnd := AI.Face_Detector(new_face_tile, mmod_desc, C_Metric_ResNet_Image_Size);
        DoStatus('对齐人脸完成. 耗时:%dms', [GetTimeTick() - tk]);

        d := TDrawEngine.Create;
        d.Rasterization.Memory.Assign(face_tile);
        d.SetSize(face_tile);
        for i := 0 to AI.Face_chips_num(face_hnd) - 1 do
          begin
            // 从照片获取对齐face
            face_raster := AI.Face_chips(face_hnd, i);

            tk := GetTimeTick();
            // 使用残差网络处理这张对齐face
            // 输出Learn引擎欧模型向量到face_vec
            // AI.Metric_ResNet_Process是个api，第一调用时，它会将DNN展开到gpu，这一部分涉及到了大量copy，会消耗比较多的时间
            // 当第二次或则高频率调用时，AI.Metric_ResNet_Process几乎都是实时的
            face_vec := AI.Metric_ResNet_Process(mdnn_hnd, face_raster);
            disposeObject(face_raster);

            // 使用Learn引擎分析这张度量人脸，返回人脸标签
            // 因为delphi和freepascal使用了label关键字，label无法被定义，label都以token来代替
            // 在Learn引擎的ProcessMaxIndexToken是分类器方法，它会遍历全部的K模型，Learn引擎有很多方法可以处理欧模型
            // 用Learn对付万人级的向量没有问题
            // 了解更多Learn的技术细节，可以访问我的开源工程，https://github.com/PassByYou888/zAnalysis
            face_token := L_Engine.ProcessMaxIndexToken(face_vec);
            DoStatus('度量化 "%s" 耗时:%dms', [face_token, GetTimeTick() - tk]);

            // 现在我们可以把标签画出来了

            // 由于我们是放大两倍做人脸检测，这里的坐标系要还原一下
            face_rect := RectMul(AI.Face_RectV2(face_hnd, i), 0.5);

            // 画框体
            d.DrawCorner(TV2Rect4.Init(face_rect, 0), DEColor(1, 0.5, 0.5, 1), 20, 5);

            // 画标签
            d.BeginCaptureShadow(Vec2(1, 1), 0.9);
            d.DrawText(PFormat('%s', [face_token]), d.Rasterization.Memory.Font.FontSize, face_rect, DEColor(1, 0, 0, 1), False);
            d.EndCaptureShadow;
          end;
        d.Flush;

        DoStatus('将drawEngine光栅转换成fmx显示');
        TThread.Synchronize(Sender, procedure
          begin
            MemoryBitmapToBitmap(d.Rasterization.Memory, Image1.Bitmap);
          end);
        disposeObject(d);

      finally
          TThread.Synchronize(Sender, procedure
          begin
            FaceRecButton.Enabled := True;
            ResetButton.Enabled := True;
          end);
      end;

      AI.Face_Close(face_hnd);
      AI.MMOD_DNN_Close(mmod_hnd);
      AI.Metric_ResNet_Close(mdnn_hnd);
    end);
end;

procedure TFaceRecForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TFaceRecForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
  // 读取zAI的配置
  ReadAIConfig;
  // 这一步会连接Key服务器，验证ZAI的Key
  // 连接服务器验证Key是在启动引擎时一次性的验证，只会当程序启动时才会验证，假如验证不能通过，zAI将会拒绝工作
  // 在程序运行中，反复创建TAI，不会发生远程验证
  // 验证需要一个userKey，通过userkey推算出ZAI在启动时生成的随机Key，userkey可以通过web申请，也可以联系作者发放
  // 验证key都是抗量子级，无法被破解
  zAI.Prepare_AI_Engine();

  FaceRecButton.Enabled := False;
  ResetButton.Enabled := False;

  TComputeThread.RunP(nil, nil, procedure(Sender: TComputeThread)
    var
      fn: U_String;
      m64: TMemoryStream64;
    begin
      AI := TAI.OpenEngine();

      DoStatus('读取数据集.');
      imgL := TAI_ImageList.Create;
      fn := umlCombineFileName(TPath.GetLibraryPath, 'lady_face.ImgDataSet');
      imgL.LoadFromFile(fn);

      DoStatus('将数据集展开成平铺光栅.');
      m64 := TMemoryStream64.Create;
      imgL.SaveToPictureStream(m64);
      m64.Position := 0;
      face_tile := NewRasterFromStream(m64);
      disposeObject(m64);
      DoStatus('将光栅转换成FMX位图显示');
      TThread.Synchronize(Sender, procedure
        begin
          MemoryBitmapToBitmap(face_tile, Image1.Bitmap);
          FaceRecButton.Enabled := True;
          ResetButton.Enabled := True;
        end);

      DoStatus('初始化Learn引擎分类器');
      DoStatus('Learn引擎K维：%d', [zAI.C_Metric_ResNet_Dim]);
      L_Engine := TLearn.CreateClassifier(TLearnType.ltKDT, zAI.C_Metric_ResNet_Dim);
    end);
end;

procedure TFaceRecForm.Image1Click(Sender: TObject);
begin
  MemoryBitmapToBitmap(face_tile, Image1.Bitmap);
end;

procedure TFaceRecForm.Timer1Timer(Sender: TObject);
begin
  DoStatus;
end;

end.
