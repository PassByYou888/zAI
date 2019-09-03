unit GradientBasedNetImageClassifierFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.ExtCtrls,
  System.Threading,

  System.IOUtils,

  CoreClasses, ListEngine,
  Learn, LearnTypes,
  zAI, zAI_Common, zAI_TrainingTask,
  zDrawEngineInterface_SlowFMX, zDrawEngine, Geometry2DUnit, MemoryRaster,
  MemoryStream64, PascalStrings, UnicodeMixedLib, DoStatusIO;

type
  TGradientBasedNetImageClassifierForm = class(TForm)
    Training_IMGClassifier_Button: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    ResetButton: TButton;
    TestClassifierButton: TButton;
    procedure TestClassifierButtonClick(Sender: TObject);
    procedure Training_IMGClassifier_ButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  public
    ai: TAI;
    imgMat: TAI_ImageMatrix;

    // 大规模训练会直接绕过内存使用，让数据以序列化方式通过Stream来工作
    // TRasterSerialized应该构建在ssd,m2,raid这类拥有高速存储能力的设备中
    RSeri: TRasterSerialized;
  end;

var
  GradientBasedNetImageClassifierForm: TGradientBasedNetImageClassifierForm;

implementation

{$R *.fmx}


procedure TGradientBasedNetImageClassifierForm.TestClassifierButtonClick(Sender: TObject);
begin
  TComputeThread.RunP(nil, nil, procedure(Sender: TComputeThread)
    var
      i, j: Integer;
      pick_raster: Integer;
      imgL: TAI_ImageList;
      img: TAI_Image;
      rasterList: TMemoryRasterList;

      output_fn, gnic_index_fn: U_String;
      Train_OutputIndex: TPascalStringList;
      hnd: TGNIC_Handle;
      vec: TLVec;
      index: TLInt;
      wrong: Integer;
    begin
      output_fn := umlCombineFileName(TPath.GetLibraryPath, 'GNIC_mnist_number_0_9' + C_GNIC_Ext);
      gnic_index_fn := umlCombineFileName(TPath.GetLibraryPath, 'GNIC_mnist_number_0_9.index');

      if (not umlFileExists(output_fn)) or (not umlFileExists(gnic_index_fn)) then
        begin
          DoStatus('必须训练');
          exit;
        end;

      TThread.Synchronize(Sender, procedure
        begin
          Training_IMGClassifier_Button.Enabled := False;
          TestClassifierButton.Enabled := False;
          ResetButton.Enabled := False;
        end);

      hnd := ai.GNIC_Open_Stream(output_fn);
      Train_OutputIndex := TPascalStringList.Create;
      Train_OutputIndex.LoadFromFile(gnic_index_fn);

      // 在每个分类中，随机采集出来测试的数量
      pick_raster := 100;

      // 从训练数据集中随机构建测试数据集
      rasterList := TMemoryRasterList.Create;
      for i := 0 to imgMat.Count - 1 do
        begin
          imgL := imgMat[i];
          for j := 0 to pick_raster - 1 do
            begin
              repeat
                  img := imgL[umlRandomRange(0, imgL.Count - 1)];
              until rasterList.IndexOf(img.Raster) < 0;
              rasterList.Add(img.Raster);
              rasterList.Last.UserToken := imgL.FileInfo;
            end;
        end;

      wrong := 0;
      for i := 0 to rasterList.Count - 1 do
        begin
          vec := ai.GNIC_Process(hnd, 32, 32, rasterList[i]);
          index := LMaxVecIndex(vec);
          if (index >= 0) and (index < Train_OutputIndex.Count) then
            begin
              if not Train_OutputIndex[index].Same(rasterList[i].UserToken) then
                  inc(wrong);
            end
          else
              inc(wrong);

          DoStatus('test %d/%d', [i + 1, rasterList.Count]);
        end;

      DoStatus('测试总数: %d', [rasterList.Count]);
      DoStatus('测试错误: %d', [wrong]);
      DoStatus('模型准确率: %f%%', [(1.0 - (wrong / rasterList.Count)) * 100]);

      DisposeObject(Train_OutputIndex);
      DisposeObject(rasterList);
      ai.GNIC_Close(hnd);

      DoStatus('正在回收内存');
      imgMat.SerializedAndRecycleMemory(RSeri);

      TThread.Synchronize(Sender, procedure
        begin
          Training_IMGClassifier_Button.Enabled := True;
          TestClassifierButton.Enabled := True;
          ResetButton.Enabled := True;
        end);
      DoStatus('测试完成.');
    end);
end;

procedure TGradientBasedNetImageClassifierForm.Training_IMGClassifier_ButtonClick(Sender: TObject);
begin
  TComputeThread.RunP(nil, nil, procedure(Sender: TComputeThread)
    var
      param: PGNIC_Train_Parameter;
      sync_fn, output_fn, gnic_index_fn: U_String;
      hnd: TGNIC_Handle;
      Train_OutputIndex: TPascalStringList;
    begin
      TThread.Synchronize(Sender, procedure
        begin
          Training_IMGClassifier_Button.Enabled := False;
          TestClassifierButton.Enabled := False;
          ResetButton.Enabled := False;
        end);

      sync_fn := umlCombineFileName(TPath.GetLibraryPath, 'GNIC_mnist_number_0_9.imgMat.sync');
      output_fn := umlCombineFileName(TPath.GetLibraryPath, 'GNIC_mnist_number_0_9' + C_GNIC_Ext);
      gnic_index_fn := umlCombineFileName(TPath.GetLibraryPath, 'GNIC_mnist_number_0_9.index');

      if (not umlFileExists(output_fn)) or (not umlFileExists(gnic_index_fn)) then
        begin
          param := TAI.Init_GNIC_Train_Parameter(sync_fn, output_fn);

          // 本次训练计划使用8小时
          param^.timeout := C_Tick_Hour * 8;

          // 通过通过调整学习率也可以达到epoch(初级建模方式)的收敛流程
          param^.learning_rate := 0.01;
          param^.completed_learning_rate := 0.00001;

          // 收敛梯度的处理条件
          // 在收敛梯度中，只要失效步数高于该数值，梯度就会开始收敛
          param^.iterations_without_progress_threshold := 4000;

          // 对每个步数输入的图片
          param^.img_mini_batch := 128;

          // gpu每做一次批次运算会暂停的时间单位是ms
          // 这项参数是在1.15新增的呼吸参数，它可以让我们在工作的同时，后台进行无感觉训练
          // zAI.KeepPerformanceOnTraining := 10;

          // 在大规模训练中，使用频率不高的光栅化数据数据都会在硬盘(m2,ssd,raid)暂存，使用才会被调用出来
          // LargeScaleTrainingMemoryRecycleTime表示这些光栅化数据可以在系统内存中暂存多久，单位是毫秒，数值越大，越吃内存
          // 如果在机械硬盘使用光栅序列化交换，更大的数值可能带来更好的训练性能
          // 大规模训练注意给光栅序列化交换文件腾挪足够的磁盘空间
          // 大数据消耗到数百G甚至若干TB，因为某些jpg这类数据原太多，展开以后，存储空间会在原尺度基础上*10倍左右
          LargeScaleTrainingMemoryRecycleTime := C_Tick_Second * 5;

          Train_OutputIndex := TPascalStringList.Create;
          if ai.GNIC_Train(True, RSeri, 32, 32, imgMat, param, Train_OutputIndex) then
            begin
              Train_OutputIndex.SaveToFile(gnic_index_fn);
              DoStatus('训练成功.');
            end
          else
            begin
              DoStatus('训练失败.');
            end;

          TAI.Free_GNIC_Train_Parameter(param);
          DisposeObject(Train_OutputIndex);
        end
      else
          DoStatus('图片分类器已经训练过了.');

      TThread.Synchronize(Sender, procedure
        begin
          Training_IMGClassifier_Button.Enabled := True;
          TestClassifierButton.Enabled := True;
          ResetButton.Enabled := True;
        end);
    end);
end;

procedure TGradientBasedNetImageClassifierForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TGradientBasedNetImageClassifierForm.FormCreate(Sender: TObject);
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

  TComputeThread.RunP(nil, nil, procedure(Sender: TComputeThread)
    var
      tokens: TArrayPascalString;
      i, j: Integer;
      imgL: TAI_ImageList;
      detDef: TAI_DetectorDefine;
      n: TPascalString;
    begin
      TThread.Synchronize(Sender, procedure
        begin
          Training_IMGClassifier_Button.Enabled := False;
          TestClassifierButton.Enabled := False;
          ResetButton.Enabled := False;
        end);
      ai := TAI.OpenEngine();
      imgMat := TAI_ImageMatrix.Create;
      DoStatus('正在读取分类图片矩阵库.');
      // TRasterSerialized 创建时需要指定一个临时文件名，ai.MakeSerializedFileName指向了一个临时目录temp，它一般位于c:盘
      // 如果c:盘空间不够，训练大数据将会出错，解决办法，重新指定TRasterSerialized构建的临时文件名
      RSeri := TRasterSerialized.Create(TFileStream.Create(ai.MakeSerializedFileName, fmCreate));
      imgMat.LargeScale_LoadFromFile(RSeri, umlCombineFileName(TPath.GetLibraryPath, 'mnist_number_0_9.imgMat'));

      DoStatus('矫正分类标签.');
      for i := 0 to imgMat.Count - 1 do
        begin
          imgL := imgMat[i];
          imgL.CalibrationNullToken(imgL.FileInfo);
          for j := 0 to imgL.Count - 1 do
            if imgL[j].DetectorDefineList.Count = 0 then
              begin
                detDef := TAI_DetectorDefine.Create(imgL[j]);
                detDef.R := imgL[j].Raster.BoundsRect;
                detDef.token := imgL.FileInfo;
                imgL[j].DetectorDefineList.Add(detDef);
              end;
        end;

      tokens := imgMat.DetectorTokens;
      DoStatus('总共有 %d 个分类', [length(tokens)]);
      for n in tokens do
          DoStatus('"%s" 有 %d 张图片', [n.Text, imgMat.GetDetectorTokenCount(n)]);

      TThread.Synchronize(Sender, procedure
        begin
          Training_IMGClassifier_Button.Enabled := True;
          TestClassifierButton.Enabled := True;
          ResetButton.Enabled := True;
        end);
    end);
end;

procedure TGradientBasedNetImageClassifierForm.ResetButtonClick(Sender: TObject);
  procedure d(FileName: U_String);
  begin
    DoStatus('删除文件 %s', [FileName.Text]);
    umlDeleteFile(FileName);
  end;

begin
  d(umlCombineFileName(TPath.GetLibraryPath, 'GNIC_mnist_number_0_9.imgMat.sync'));
  d(umlCombineFileName(TPath.GetLibraryPath, 'GNIC_mnist_number_0_9.imgMat.sync_'));
  d(umlCombineFileName(TPath.GetLibraryPath, 'GNIC_mnist_number_0_9' + C_GNIC_Ext));
  d(umlCombineFileName(TPath.GetLibraryPath, 'GNIC_mnist_number_0_9.index'));
end;

procedure TGradientBasedNetImageClassifierForm.Timer1Timer(Sender: TObject);
begin
  DoStatus;
end;

end.
