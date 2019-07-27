program PCA;

{$APPTYPE CONSOLE}

{$R *.res}


uses SysUtils, DoStatusIO, Learn, LearnTypes;

procedure PCA_Demo;
var
  m1, m2: TLMatrix;
  v1: TLVec;
  i, j, k, l: Integer;
  t: TLFloat;
begin
  m1 := Learn.ExpressionToLMatrix(3, 6,
    // 便于理解，我们也可以在这里使用3d数据来输入，注释去掉即可
    '1.0,2.0,3.0,' +
    '1.0,2.0,3.0,' +
    '1.0,2.0,3.0,' +
    '1.0,2.0,3.0,' +
    '1.0,2.0,3.0,' +
    '1.0,2.0,3.0'

    // RandomF是输入随机浮点数，类型为Extended
    // 'RandomF,RandomF,RandomF,' +
    // 'RandomF,RandomF,RandomF,' +
    // 'RandomF,RandomF,RandomF,' +
    // 'RandomF,RandomF,RandomF,' +
    // 'RandomF,RandomF,RandomF,' +
    // 'RandomF,RandomF,RandomF'
    );

  DoStatus('input');
  DoStatus(m1);
  DoStatus('');
  // Learn中的PCA，将会提取input中的基底向量作为空间特征使用
  Learn.PCA(m1, 6, 3, v1, m2);
  // 基底
  DoStatus('output');
  DoStatus('basis0_x = %f', [m2[0, 0]]);
  DoStatus('basis0_y = %f', [m2[1, 0]]);
  DoStatus('basis0_z = %f', [m2[2, 0]]);
  DoStatus('basis1_x = %f', [m2[0, 1]]);
  DoStatus('basis1_y = %f', [m2[1, 1]]);
  DoStatus('basis1_z = %f', [m2[2, 1]]);
  DoStatus('basis2_x = %f', [m2[0, 2]]);
  DoStatus('basis2_y = %f', [m2[1, 2]]);
  DoStatus('basis2_z = %f', [m2[2, 2]]);

  // 在线性代数中，向量的基(也称为基底）是描述、刻画向量空间的基本工具。
  // 向量空间的基是它的一个特殊的子集，基的元素称为基向量。
  // 向量空间中任意一个元素，都可以唯一地表示成基向量的线性组合。
  // 如果基中元素个数有限，就称向量空间为有限维向量空间，将元素的个数称作向量空间的维数。
  // 不是所有空间都拥有由有限个元素构成的基底。这样的空间称为无限维空间。某些无限维空间上可以定义由无限个元素构成的基。
  // 如果承认选择公理，那么可以证明任何向量空间都拥有一组基。
  // 一个向量空间的基不止一组，但同一个空间的两组不同的基，它们的元素个数或势（当元素个数是无限的时候）是相等的。
  // 一组基里面的任意一部分向量都是线性无关的；反之，如果向量空间拥有一组基，那么在向量空间中取一组线性无关的向量，一定能将它扩充为一组基。
  // 在内积向量空间中，可以定义正交的概念。通过特别的方法，可以将任意的一组基变换成正交基乃至标准正交基。
  // 现在，我们将对PCA产生的基底向量做正交性测试
  DoStatus('');
  DoStatus('正在对基底向量做正交性测试,by qq600585');
  l := 3;
  for i := 0 to l - 1 do
    for j := 0 to l - 1 do
      begin
        t := 0.0;
        for k := 0 to l - 1 do
            t := t + m2[k, i] * m2[k, j];
        if i = j then
            t := t - 1; // 正交轴
        if Abs(t) > MachineEpsilon then
            DoStatus('基底错误')
        else
            DoStatus('基底测试通过');
      end;
  DoStatus('PCA Demo完毕');
end;

begin
  PCA_Demo;
  readln;

end.
