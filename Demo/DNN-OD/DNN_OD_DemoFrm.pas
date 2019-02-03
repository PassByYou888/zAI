unit DNN_OD_DemoFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.ScrollBox, FMX.Memo,

  System.IOUtils,

  CoreClasses,
  Learn, LearnTypes,
  zAI, zAI_Common, zAI_TrainingTask,
  zDrawEngineInterface_SlowFMX, zDrawEngine, Geometry2DUnit, MemoryRaster,
  MemoryStream64, PascalStrings, UnicodeMixedLib, DoStatusIO, FMX.Layouts, FMX.ExtCtrls;

type
  TDNN_OD_Form = class(TForm)
    DNN_OD_Button: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    Image1: TImageViewer;
    ResetButton: TButton;
    procedure DNN_OD_ButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  public
  end;

var
  DNN_OD_Form: TDNN_OD_Form;

implementation

{$R *.fmx}


procedure TDNN_OD_Form.DNN_OD_ButtonClick(Sender: TObject);
begin
  TComputeThread.RunP(nil, nil, procedure(Sender: TComputeThread)
    var
      bear_ImgL: TAI_ImageList;
      bear_dataset_file, bear_od_file: U_String;
      i, j: Integer;
      detDef: TAI_DetectorDefine;

      ai: TAI;
      param: PMMOD_Train_Parameter;

      m64: TMemoryStream64;

      mmod_hnd: TMMOD_Handle;
      matrix_img: TMatrix_Image_Handle;
      detTarget: TMemoryRaster;
      tk: TTimeTick;
      mmod_processcounter: Integer;
    begin
      TThread.Synchronize(Sender, procedure
        begin
          DNN_OD_Button.Enabled := False;
        end);
      try
        bear_dataset_file := umlCombineFileName(TPath.GetLibraryPath, 'bear.ImgDataSet');
        bear_od_file := umlChangeFileExt(bear_dataset_file, zAI.C_MMOD_Ext);

        bear_ImgL := TAI_ImageList.Create;
        bear_ImgL.LoadFromFile(bear_dataset_file);

        ai := TAI.OpenEngine();

        DoStatus('检查熊本熊的OD检测器 : %s', [bear_od_file.Text]);
        if not umlFileExists(bear_od_file) then
          begin
            DoStatus('开始训练熊本熊的OD检测器.');

            for i := 0 to bear_ImgL.Count - 1 do
              for j := 0 to bear_ImgL[i].DetectorDefineList.Count - 1 do
                begin
                  detDef := bear_ImgL[i].DetectorDefineList[j];
                  // 检测框尺度矫正
                  // 该尺度是等比尺度，100:100是宽高比，并不是真实的100像素，100:100也等同于1:1
                  // 这里给100:100，是告诉矫正函数，我们需要是方框样本，你给我把检测器框体全部改成方正的

                  // MMOD也可以叫做DNN-OD
                  // MMOD相比SVM-OD，会多出标签
                  // 我们在detDef.Token中定义的标签将会被DNN记忆和学习(分类)
                  detDef.R := detDef.Owner.Raster.ComputeAreaScaleSpace(detDef.R, 100, 100);
                end;

            param := ai.MMOD_DNN_PrepareTrain(bear_ImgL, umlChangeFileExt(bear_dataset_file, '.sync'));

            // 这里的OD训练参数叫做超参数，简称超参

            // 下面罗列出来的是最重要的几个DNN OD参数
            // 假如不太明白，请自行查找相关资料，关键字
            // "resnet mini batch"
            // "resnet object detector"

            // 本次训练的计划时间为2小时
            param^.timeout := C_Tick_Hour * 2;

            // 我们希望最后训练出的检测器窗口最长尺寸100
            param^.target_size := 100;
            // 我们希望最后训练出的检测器窗口的最小尺寸至少是50
            param^.min_target_size := 50;
            // 限制resnet mini batch生成的目标对象尺度,裁剪后的对象沿其最长边至少有98像素
            // min_object_size_x 不应该超过 target_size，这一步参数如果给错，ZAI内核会自动校正，但是会输出很多提示
            param^.min_object_size_x := 98;
            // 限制resnet mini batch生成的目标对象尺度,被裁剪的对象将沿着其最短边至少有49像素
            // min_object_size_y 不应该超过 min_target_size，这一步参数如果给错，ZAI内核会自动校正，但是会输出很多提示
            param^.min_object_size_y := 49;

            // resnet mini batch 尺度
            // 训练中，每次step input net都会使用renet mini batch重新生成一次数据样本
            // 这里的参数是新数据样本的尺度
            param^.chip_dims_x := 300;
            param^.chip_dims_y := 300;

            // 错误进度，值越小训练速度越快，值越大训练结果越精确，但是需要更长的时间来训练
            // 我取的值很大，因此，训练完成这个熊本熊的数据集，需要30分钟左右
            param^.iterations_without_progress_threshold := 500;

            // resnet在每个step做mini batch的次数
            // 一般来说，你的样本图片总和值之间就可以了，如果样本图片很多，大小可以根据GPU+内存的配置来定
            param^.num_crops := 10;

            // resnet在做mini batch时的随机旋转系数
            param^.max_rotation_degrees := 90;

            // 现在，我们已经将超参都设置完成了，让我们开始执行训练吧
            // MMOD也可以叫做DNN-OD
            // MMOD相比SVM-OD，会多出标签
            // 我们在detDef.Token中定义的标签将会被DNN记忆和学习(分类)
            m64 := ai.MMOD_DNN_Train_Stream(param);

            if m64 <> nil then
              begin
                DoStatus('训练完成');
                m64.SaveToFile(bear_od_file);
                disposeObject(m64);
              end
            else
                DoStatus('训练失败');

            ai.Free_MMOD_DNN_TrainParam(param);
          end;

        if umlFileExists(bear_od_file) then
          begin
            DoStatus('载入熊本熊的OD检测器 : %s', [bear_od_file.Text]);
            mmod_hnd := ai.MMOD_DNN_Open_Stream(bear_od_file);

            // 使用texture atlas技术体系做纹理光栅的排序，输出排序后的新光栅
            detTarget := bear_ImgL.PackingRaster;

            // 处于简便演示，这里都不做具体的OD检测实现了，直接使用DrawMMOD来输出
            ai.DrawMMOD(mmod_hnd, detTarget, DEColor(0.5, 0.5, 1, 1));

            TThread.Synchronize(Sender, procedure
              begin
                MemoryBitmapToBitmap(detTarget, Image1.Bitmap);
              end);

            // 现在我们测试一下gpu的od能力
            // 在实际应用中，我们也会高频率使用这个api
            DoStatus('干扰GPU-性能测试：光栅在排序后的分辨率为 %d * %d', [detTarget.width, detTarget.height]);
            DoStatus('干扰GPU-性能测试：正在做GPU-OD性能测试,5秒后将会报告测试结果.');
            tk := GetTimeTick();
            mmod_processcounter := 0;
            while GetTimeTick() - tk < 5000 do
              begin
                // 在MMOD_DNN_Process的返回数组，TMMOD_Desc中，会有token标签，它表示，检测到的这个框体是什么
                // 注意：MMOD_DNN_Process在每次处理前，会将内存的光栅，传递给gpu，这个过程是等待，不能并行，这是主要性能瓶颈
                // 注意：当我们要求非常高的实时性，我们就需要将分辨率调低一点，或则使用Tracker技术来做快速跟踪定位
                // 注意：当我们要求非常高的实时性，我们也可以使用TAI_Parallel功能来并行化的处理多个并发的OD检测，以此来提升效率
                ai.MMOD_DNN_Process(mmod_hnd, detTarget);
                inc(mmod_processcounter);
              end;
            DoStatus('干扰GPU-性能测试：GPU-OD在5秒内总共算成了 %d 次OD检测，大约每秒完成 %d 次检测', [mmod_processcounter, Round(mmod_processcounter / 5.0)]);

            matrix_img := ai.Prepare_Matrix_Image(detTarget);
            DoStatus('纯净GPU-性能测试：光栅在排序后的分辨率为 %d * %d', [detTarget.width, detTarget.height]);
            DoStatus('纯净GPU-性能测试：正在做GPU-OD性能测试,5秒后将会报告测试结果.');
            tk := GetTimeTick();
            mmod_processcounter := 0;
            while GetTimeTick() - tk < 5000 do
              begin
                // 在MMOD_DNN_Process的返回数组，TMMOD_Desc中，会有token标签，它表示，检测到的这个框体是什么
                // 注意：MMOD_DNN_Process在每次处理前，会将内存的光栅，传递给gpu，这个过程是等待，不能并行，这是主要性能瓶颈
                // 注意：当我们要求非常高的实时性，我们就需要将分辨率调低一点，或则使用Tracker技术来做快速跟踪定位
                // 注意：当我们要求非常高的实时性，我们也可以使用TAI_Parallel功能来并行化的处理多个并发的OD检测，以此来提升效率
                ai.MMOD_DNN_Process_Matrix(mmod_hnd, matrix_img);
                inc(mmod_processcounter);
              end;
            DoStatus('纯净GPU-性能测试：GPU-OD在5秒内总共算成了 %d 次OD检测，大约每秒完成 %d 次检测', [mmod_processcounter, Round(mmod_processcounter / 5.0)]);
            ai.Close_Matrix_Image(matrix_img);

            // 现在，我们将分辨率降低，再做一个测试
            detTarget.Scale(0.5);

            // 现在我们测试一下gpu的od能力
            // 在实际应用中，我们也会高频率使用这个api
            DoStatus('干扰GPU-性能测试：光栅在排序后的分辨率为 %d * %d', [detTarget.width, detTarget.height]);
            DoStatus('干扰GPU-性能测试：正在做GPU-OD性能测试,5秒后将会报告测试结果.');
            tk := GetTimeTick();
            mmod_processcounter := 0;
            while GetTimeTick() - tk < 5000 do
              begin
                // 在MMOD_DNN_Process的返回数组，TMMOD_Desc中，会有token标签，它表示，检测到的这个框体是什么
                // 注意：MMOD_DNN_Process在每次处理前，会将内存的光栅，传递给gpu，这个过程是等待，不能并行，这是主要性能瓶颈
                // 注意：当我们要求非常高的实时性，我们就需要将分辨率调低一点，或则使用Tracker技术来做快速跟踪定位
                // 注意：当我们要求非常高的实时性，我们也可以使用TAI_Parallel功能来并行化的处理多个并发的OD检测，以此来提升效率
                ai.MMOD_DNN_Process(mmod_hnd, detTarget);
                inc(mmod_processcounter);
              end;
            DoStatus('干扰GPU-性能测试：GPU-OD在5秒内总共算成了 %d 次OD检测，大约每秒完成 %d 次检测', [mmod_processcounter, Round(mmod_processcounter / 5.0)]);

            matrix_img := ai.Prepare_Matrix_Image(detTarget);
            DoStatus('纯净GPU-性能测试：光栅在排序后的分辨率为 %d * %d', [detTarget.width, detTarget.height]);
            DoStatus('纯净GPU-性能测试：正在做GPU-OD性能测试,5秒后将会报告测试结果.');
            tk := GetTimeTick();
            mmod_processcounter := 0;
            while GetTimeTick() - tk < 5000 do
              begin
                // 在MMOD_DNN_Process的返回数组，TMMOD_Desc中，会有token标签，它表示，检测到的这个框体是什么
                // 注意：MMOD_DNN_Process在每次处理前，会将内存的光栅，传递给gpu，这个过程是等待，不能并行，这是主要性能瓶颈
                // 注意：当我们要求非常高的实时性，我们就需要将分辨率调低一点，或则使用Tracker技术来做快速跟踪定位
                // 注意：当我们要求非常高的实时性，我们也可以使用TAI_Parallel功能来并行化的处理多个并发的OD检测，以此来提升效率
                ai.MMOD_DNN_Process_Matrix(mmod_hnd, matrix_img);
                inc(mmod_processcounter);
              end;
            DoStatus('纯净GPU-性能测试：GPU-OD在5秒内总共算成了 %d 次OD检测，大约每秒完成 %d 次检测', [mmod_processcounter, Round(mmod_processcounter / 5.0)]);
            ai.Close_Matrix_Image(matrix_img);

            ai.MMOD_DNN_Close(mmod_hnd);
          end;

        disposeObject(bear_ImgL);
        disposeObject(ai);

      finally
          TThread.Synchronize(Sender, procedure
          begin
            DNN_OD_Button.Enabled := True;
          end);
      end;
    end);
end;

procedure TDNN_OD_Form.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TDNN_OD_Form.FormCreate(Sender: TObject);
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
end;

procedure TDNN_OD_Form.ResetButtonClick(Sender: TObject);
var
  fn: U_String;

  procedure d(filename: U_String);
  begin
    DoStatus('删除文件 %s', [filename.Text]);
    umlDeleteFile(filename);
  end;

begin
  fn := umlCombineFileName(TPath.GetLibraryPath, 'bear' + zAI.C_MMOD_Ext);
  d(fn);
  d(umlChangeFileExt(fn, '.sync'));
  d(umlChangeFileExt(fn, '.sync_'));
  Image1.Bitmap.FreeHandle;
end;

procedure TDNN_OD_Form.Timer1Timer(Sender: TObject);
begin
  DoStatus;
end;

end.
