program LDA;

{$APPTYPE CONSOLE}

{$R *.res}


uses SysUtils, PascalStrings, UnicodeMixedLib, DoStatusIO, Learn, LearnTypes;

// 线性判别式分析(Linear Discriminant Analysis, LDA)，也叫做Fisher线性判别(Fisher Linear Discriminant ,FLD)，是模式识别的经典算法
// 它是在1996年由Belhumeur引入模式识别和人工智能领域的。

// 线性鉴别分析的基本思想是将高维的模式样本投影到最佳鉴别矢量空间，以达到抽取分类信息和压缩特征空间维数的效果.
// 投影后保证模式样本在新的子空间有最大的类间距离和最小的类内距离，即模式在该空间中有最佳的可分离性。
// 因此，它是一种有效的特征抽取方法。
// 使用这种方法能够使投影后模式样本的类间散布矩阵最大，并且同时类内散布矩阵最小。
// 就是说，它能够保证投影后模式样本在新的空间中有最小的类内距离和最大的类间距离，即模式在该空间中有最佳的可分离性。

procedure LDA_Demo;
const
  sampler_num = 5;
  Classifier_num = 50;
var
  M: TLMatrix;
  cv, v: TLVec;
  i, J: TLInt;
  Info: SystemString;
begin
  // 5个数据样本
  M := LMatrix(Classifier_num, sampler_num);
  // cv是向量形式的分类标签，由于需要指定分类标签，所以LDA也被定位成为一种有监督的数据分类方法
  cv := LVec(Classifier_num);

  for J := 0 to length(M) - 1 do
    begin
      for i := 0 to length(M[J]) - 1 do
          M[J, i] := Random;
      // 分类标签输入类型为整数，最大值不能超过 Classifier_num
      cv[J] := J + 1;
    end;

  DoStatus('input');
  DoStatus(M);

  // v是降维后的输出向量，长度=数据样本
  // 在同纬度空间中v符合线性规律，既同类标签
  Learn.LDA(M, cv, Classifier_num, Info, v);
  DoStatus(Info);
  DoStatus(v);
end;

begin
  LDA_Demo;
  readln;

end.
