unit Face_Metric_ResNet_TrainAPIDemoFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  System.IOUtils, Vcl.ExtCtrls,

  CoreClasses, PascalStrings, UnicodeMixedLib, zAI, zAI_Common, zAI_TrainingTask,
  ListEngine, zDrawEngineInterface_SlowFMX, MemoryRaster, DoStatusIO, MemoryStream64;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    FileEdit: TLabeledEdit;
    trainingButton: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure trainingButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}


procedure TForm2.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TForm2.FormCreate(Sender: TObject);
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

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  // dostatus不给参数，是刷新在线程中的StatusIO状态，可以刷新parallel线程中的status
  DoStatus;
end;

procedure TForm2.trainingButtonClick(Sender: TObject);
begin
  TComputeThread.RunP(nil, nil,
    procedure(Sender: TComputeThread)
    var
      fn: U_String;
      // AI引擎
      ai: TAI;
      param: PMetric_ResNet_Train_Parameter;
      imgMat: TAI_ImageMatrix;
      successed: Boolean;
    begin
      TThread.Synchronize(Sender, procedure
        begin
          fn := umlCombineFileName(TPath.GetLibraryPath, FileEdit.Text);
        end);

      // imgMat是图片矩阵，用于处理大规模图片数据集的训练
      imgMat := TAI_ImageMatrix.Create;

      // 由于图片矩阵在读取和保存大型图片集非常慢，一般来说，一次读取和保存都是数十万张，这里的操作要谨慎
      // 图片矩阵的保存和读取，都是并行化的，会将cpu吃满，然后让磁盘IO满负荷工作，以减少等待时间
      imgMat.LoadFromFile(fn);

      // 构建zAI的引擎
      // zAI引擎可以在线程中直接构建，不用Sync
      ai := TAI.OpenEngine();

      DoStatus('开始训练');
      // 开始训练图片库矩阵
      // 我们训练大规模样本时，都应该选择图片矩阵方式来训练
      param := TAI.Init_Metric_ResNet_Parameter(umlChangeFileExt(fn, '.sync'), umlChangeFileExt(fn, C_Metric_ResNet_Ext));

      // 本次计划训练3小时
      // 在3小时条件满足时候，如果拟合度没有达到要求将会强行退出
      // 强行退出的程序，它的拟合度相关数值，都会放在下列容器中
      // AI.Last_training_average_loss, AI.Last_training_learning_rate: Double;
      // 如果在3小时内程序退出的，就表示满足了拟合要求
      param^.timeout := C_Tick_Hour * 3;

      // 错误收敛次数100
      param^.iterations_without_progress_threshold := 100;

      // resnet深度学习技术
      // resnet是在每次做深度学习的step中，做mini batch（小批次），大意就是，重组数据，让每次step都会会有不一样但是又可以拟合的face样本
      // step_mini_batch_target_num等同于真实的人脸分类数量时，拟合度学习质量将会是最好的
      // 但是由于gpu和物理内存限制，step_mini_batch_target_num很难达到非常大的数量，因此，我们需要step做对齐人脸的裁剪迭代工作
      // 这里，我们每次step迭代做100个分类的face批次，这个数值根据已配置好的内存体积而定（包括gpu内存和物理内存）
      // 如果内存配置到128G+4张sli，这个数值可以写500
      // 作者的配置为12G显存，16G高速物理内存，这个数值我写的100，在学习过程中，它几乎逼近硬件极限
      // 假如step_mini_batch_target_num的值过大，或则过小，zAI内核会自动调整该值，大多数时候，不需要专门对其设定
      // 当我们训练很大的数据集时，也许我们不会一次完成，我们需要修改数据集以后反复训练，所以这些值才会需要固定下来
      param^.step_mini_batch_target_num := 100;

      // resnet深度学习技术，每次step迭代时，在每个face里面做的光栅生成数量，这个数值根据已配置好的内存体积而定（包括gpu内存和物理内存）
      // 如果内存配置到128G+4张sli，这个数值可以写20以上，这个数值我写的5，在学习过程中，它几乎逼近硬件极限
      // 假如step_mini_batch_raster_num的值过大，或则过小，zAI内核会自动调整该值，大多数时候，不需要专门对其设定
      // 当我们训练很大的数据集时，也许我们不会一次完成，我们需要修改数据集以后反复训练，所以这些值才会需要固定下来
      param^.step_mini_batch_raster_num := 5;

      // 深度学习在运行过程中的特别说明：
      // zAI在深度学习的过程中，每隔5分钟会保存一次状态文件
      // 当发生蓝屏宕机，重启以后，可以从最近的一次状态文件中恢复
      // 恢复只需要重新点开程序，只要保证训练参数不变就能正常运行

      // 深度学习在性能优化方面的特别说明：
      // 因为专业学习卡非常昂贵，起价4万起，配置一台深度学习服务器，设备投资数十万以上都是很平常的
      // 而民用显卡一般配置比较低，容易发热，内存错误率也很高，最关键的还是我们在训练中，我们还会工作，不能让训练影响太多工作
      // 处于以上考虑，满负荷的cuda计算机制并不是最好的cuda机制，新的Metric_ResNet_Train学习机制可以采用呼吸式学习
      // 呼吸式学习：学习效率相比满负荷会低20%，它每次迭代，会让gpu短暂呼吸一下，并且做一次显存和内存验证工作
      // 呼吸式学习机制适合大规模和长时间的数据集训练，它更适合民用，但是你需要花更多时间训练模型

      // 满负荷GPU训练，如果关闭，就是呼吸式训练
      // 如果机器配置不好，或则需要正常工作+训练同步进行，建议关闭
      // 假如配置了sli or fire，建议打开
      // 因为我的配置偏中高，所以这里是打开状态
      param^.fullGPU_Training := True;

      // 好了，我们开始执行度量化的残差网络训练吧
      successed := ai.Metric_ResNet_Train(imgMat, param);

      TAI.Free_Metric_ResNet_Parameter(param);

      if successed then
        begin
          DoStatus('训练成功.');
        end
      else
          DoStatus('训练失败.');

      // 释放训练使用的数据
      DisposeObject(ai);
      DisposeObject(imgMat);
    end);
end;

end.
