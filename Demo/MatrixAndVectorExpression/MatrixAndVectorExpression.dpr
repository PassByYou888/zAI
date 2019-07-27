program MatrixAndVectorExpression;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  CoreClasses,
  PascalStrings,
  DoStatusIO,
  LearnTypes,
  Learn,
  TextParsing,
  zExpression;

// 构建3*3的variant矩阵，使用c语法表达式
procedure MatrixExp;
var
  m: TExpressionValueMatrix;
begin
  DoStatus('');
  m := EvaluateExpressionMatrix(3, 3,
    '"hello"+"-baby"/*备注：字符串联合*/,true,false,' +
    '1+1,2+2,3+3,' +
    '4*4,4*5,4*6', tsC);
  DoStatus(m);
end;

// 构建variant向量数组，使用pascal语法表达式
procedure MatrixVec;
var
  v: TExpressionValueVector;
begin
  DoStatus('');
  v := EvaluateExpressionVector('0.1*(0.1+max(0.15,0.11)){备注内容},1,2,3,4,5,6,7,8,9', tsPascal);
  DoStatus(v);
end;

// 构建3*4的TLMatrix矩阵，浮点矩阵，默认使用pascal语法表达式
procedure LearnMatrixExp;
var
  m: TLMatrix;
begin
  DoStatus('');
  m := ExpressionToLMatrix(3, 4,
    '1*1,1*2,1*3,' +
    '2*1,2*2,2*3,' +
    '3*1,3*2,3*3,' +
    '4*4,4*5,4*6');
  DoStatus(m);
end;

// 构建TLVec向量数组，浮点数组，默认使用pascal语法表达式
procedure LearnVecExp;
var
  v: TLVec;
begin
  DoStatus('');
  v := ExpressionToLVec('1,2,3,4,5,6,7,8,9');
  DoStatus(v);
end;

begin
  MatrixExp;
  MatrixVec;
  LearnMatrixExp;
  LearnVecExp;
  DoStatus('preee exter key to exit.');
  readln;
end.
