program Analysis_Decision;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  CoreClasses,
  PascalStrings,
  DoStatusIO,
  LearnTypes,
  Learn;

var
  lr: TLearn;
  n: TPascalString;
  lt: TLearnType;

begin
  // 这里演示了怎样使用神经网络进行数据分类
  for lt in [
    ltKDT,            // KDTree, fast space operation, this not Neurons network
  ltKM,               // k-means++ clusterization, this not Neurons network
  ltForest,           // random decision forest
  ltLogit,            // Logistic regression
  ltLM,               // Levenberg-Marquardt
  ltLM_MT,            // Levenberg-Marquardt with parallel
  ltLBFGS,            // L-BFGS
  ltLBFGS_MT,         // L-BFGS with parallel
  ltLBFGS_MT_Mod,     // L-BFGS with parallel and optimization
  ltLM_Ensemble,      // Levenberg-Marquardt Ensemble
  ltLM_Ensemble_MT,   // Levenberg-Marquardt Ensemble with parallel
  ltLBFGS_Ensemble,   // L-BFGS Ensemble
  ltLBFGS_Ensemble_MT // L-BFGS Ensemble with parallel
    ] do
    begin
      // CreateClassifier2 是创建一个具有两层感知能力的神经网络学习层
      // CreateClassifier方法在任何时候，都是一个近整型输出值
      lr := TLearn.CreateClassifier2(lt, 5);

      // 1 1 1 1 1，这里的一堆1表示n种数据维度，这里可以说任意数值
      // k1表示决代号，决策号是线性的
      lr.AddMemory('1 1 1 1 1=k1');      // 决策索引值0
      lr.AddMemory('1 2 1 2 1=k2');      // 决策索引值1
      lr.AddMemory('10 10 21 12 21=k3'); // 决策索引值2
      lr.AddMemory('2 2 2 2 2=k4');      // 决策索引值3
      lr.AddMemory('1 2 3 4 5=k5');      // 决策索引值4
      lr.AddMemory('2 2 3 4 7=k6');      // 决策索引值5
      lr.AddMemory('3 4 5 6 7=k7');      // 决策索引值6
      lr.AddMemory('9 3 2 3 7=k1');      // 决策索引值7
      lr.AddMemory('3 3 9 1 1=k3');      // 决策索引值7

      // 10是训练深度，在内核反复迭代训练的次数
      // 部分算法因为样本数量，在这一步会有不同延迟
      lr.Train(10);

      // 这里的输出是7个返回值，因为，我们只有k1,k2,k3,k4,k5,k6,k7的决策，依据顺序，对n变量的输入值进行分别评估
      // 数值最大的值，表示最接近的决策id，当我们取得了决策id后，即可进行相应的子程序处理
      // 不同的训练方法会产生不同的决策优先级
      n := '2 2 5 4 6.5';
      // lr.process(n)返回的是按索引排列的决策权重
      DoStatus('(%s) %s = %s', [LearnTypes.CLearnString[lr.LearnType], n.Text, lr.process(n)]);
      DoStatus('(%s) 最优决策:%s', [LearnTypes.CLearnString[lr.LearnType], lr.ProcessMaxToken(LVec(n, lr.InLen))]);

      disposeObject(lr);
    end;

  DoStatus('NN决策演示结束');
  readln;

end.
