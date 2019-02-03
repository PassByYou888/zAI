{ ****************************************************************************** }
{ * neural network support, by QQ 600585@qq.com                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
type
  TMultiLayerPerceptron = record
    StructInfo: TLIVec;
    Weights: TLVec;
    ColumnMeans: TLVec;
    ColumnSigmas: TLVec;
    Neurons: TLVec;
    DFDNET: TLVec;
    DError: TLVec;
    x: TLVec;
    y: TLVec;
    Chunks: TLMatrix;
    NWBuf: TLVec;
  end;

  PMultiLayerPerceptron = ^TMultiLayerPerceptron;

procedure MLPCreate0(NIn, NOut: TLInt; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreate1(NIn, NHid, NOut: TLInt; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreate2(NIn, NHid1, NHid2, NOut: TLInt; var Network: TMultiLayerPerceptron); forward;

procedure MLPCreateB0(NIn, NOut: TLInt; b, d: TLFloat; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreateB1(NIn, NHid, NOut: TLInt; b, d: TLFloat; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreateB2(NIn, NHid1, NHid2, NOut: TLInt; b, d: TLFloat; var Network: TMultiLayerPerceptron); forward;

procedure MLPCreateR0(NIn, NOut: TLInt; a, b: TLFloat; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreateR1(NIn, NHid, NOut: TLInt; a, b: TLFloat; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreateR2(NIn, NHid1, NHid2, NOut: TLInt; a, b: TLFloat; var Network: TMultiLayerPerceptron); forward;

procedure MLPCreateC0(NIn, NOut: TLInt; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreateC1(NIn, NHid, NOut: TLInt; var Network: TMultiLayerPerceptron); forward;
procedure MLPCreateC2(NIn, NHid1, NHid2, NOut: TLInt; var Network: TMultiLayerPerceptron); forward;

procedure MLPFree(var Network: TMultiLayerPerceptron); forward;
procedure MLPCopy(const Network1: TMultiLayerPerceptron; var Network2: TMultiLayerPerceptron); forward;

procedure MLPSerialize(const Network: TMultiLayerPerceptron; var RA: TLVec; var RLen: TLInt); forward;
procedure MLPUNSerialize(const RA: TLVec; var Network: TMultiLayerPerceptron); forward;

procedure MLPRandomize(var Network: TMultiLayerPerceptron); overload; forward;
procedure MLPRandomize(var Network: TMultiLayerPerceptron; const Diameter: TLFloat); overload; forward;
procedure MLPRandomize(var Network: TMultiLayerPerceptron; const WBest: TLVec; const Diameter: TLFloat); overload; forward;
procedure MLPRandomizeFull(var Network: TMultiLayerPerceptron); forward;

procedure MLPInitPreprocessor(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt); forward;
procedure MLPProperties(const Network: TMultiLayerPerceptron; var NIn: TLInt; var NOut: TLInt; var WCount: TLInt); forward;
function MLPIsSoftmax(const Network: TMultiLayerPerceptron): Boolean; forward;

procedure MLPProcess(var Network: TMultiLayerPerceptron; const x: TLVec; var y: TLVec); forward;

function MLPError(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt): TLFloat; forward;
function MLPErrorN(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt): TLFloat; forward;
function MLPClsError(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt): TLInt; forward;
function MLPRelClsError(var Network: TMultiLayerPerceptron; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function MLPAvgCE(var Network: TMultiLayerPerceptron; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function MLPRMSError(var Network: TMultiLayerPerceptron; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function MLPAvgError(var Network: TMultiLayerPerceptron; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function MLPAvgRelError(var Network: TMultiLayerPerceptron; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;

procedure MLPGrad(var Network: TMultiLayerPerceptron; const x: TLVec; const DesiredY: TLVec; var E: TLFloat; var Grad: TLVec); forward;
procedure MLPGradN(var Network: TMultiLayerPerceptron; const x: TLVec; const DesiredY: TLVec; var E: TLFloat; var Grad: TLVec); forward;
procedure MLPGradBatch(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt; var E: TLFloat; var Grad: TLVec); forward;
procedure MLPGradNBatch(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt; var E: TLFloat; var Grad: TLVec); forward;

procedure MLPHessianNBatch(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt; var E: TLFloat; var Grad: TLVec; var h: TLMatrix); forward;
procedure MLPHessianBatch(var Network: TMultiLayerPerceptron; const xy: TLMatrix; SSize: TLInt; var E: TLFloat; var Grad: TLVec; var h: TLMatrix); forward;

procedure MLPInternalProcessVector(const StructInfo: TLIVec;
  const Weights: TLVec; const ColumnMeans: TLVec;
  const ColumnSigmas: TLVec; var Neurons: TLVec;
  var DFDNET: TLVec; const x: TLVec; var y: TLVec); forward;

const
  MLPVNum = 7;
  NFieldWidth = 4;
  ChunkSize = 32;

procedure AddInputLayer(NCount: TLInt; var LSizes: TLIVec;
  var LTypes: TLIVec; var LConnFirst: TLIVec;
  var LConnLast: TLIVec; var LastProc: TLInt); forward;

procedure AddBiasedSummatorLayer(NCount: TLInt;
  var LSizes: TLIVec; var LTypes: TLIVec;
  var LConnFirst: TLIVec; var LConnLast: TLIVec;
  var LastProc: TLInt); forward;

procedure AddActivationLayer(FuncType: TLInt;
  var LSizes: TLIVec; var LTypes: TLIVec;
  var LConnFirst: TLIVec; var LConnLast: TLIVec;
  var LastProc: TLInt); forward;

procedure AddZeroLayer(var LSizes: TLIVec; var LTypes: TLIVec;
  var LConnFirst: TLIVec; var LConnLast: TLIVec;
  var LastProc: TLInt); forward;

procedure MLPCreate(NIn, NOut: TLInt;
  const LSizes: TLIVec; const LTypes: TLIVec;
  const LConnFirst: TLIVec; const LConnLast: TLIVec;
  LayersCount: TLInt; IsClsNet: Boolean;
  var Network: TMultiLayerPerceptron); forward;

procedure MLPActivationFunction(NET: TLFloat; k: TLInt;
  var f: TLFloat; var df: TLFloat; var D2F: TLFloat); forward;

procedure MLPHessianBatchInternal(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; SSize: TLInt; NaturalErr: Boolean;
  var E: TLFloat; var Grad: TLVec; var h: TLMatrix); forward;

procedure MLPInternalCalculateGradient(var Network: TMultiLayerPerceptron;
  const Neurons: TLVec; const Weights: TLVec;
  var DError: TLVec; var Grad: TLVec;
  NaturalErrorFunc: Boolean); forward;

procedure MLPChunkedGradient(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; CStart: TLInt; CSize: TLInt;
  var E: TLFloat; var Grad: TLVec;
  NaturalErrorFunc: Boolean); forward;

function SafeCrossEntropy(t: TLFloat; z: TLFloat): TLFloat; forward;

(* ************************************************************************
  Creates neural network with NIn inputs, NOut outputs, without hidden layers, with linear output layer.
  Network weights are filled with small random values.
  ************************************************************************ *)
procedure MLPCreate0(NIn, NOut: TLInt; var Network: TMultiLayerPerceptron);
var
  LSizes: TLIVec;
  LTypes: TLIVec;
  LConnFirst: TLIVec;
  LConnLast: TLIVec;
  LayersCount: TLInt;
  LastProc: TLInt;
begin
  LayersCount := 1 + 2;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount  );
  SetLength(LTypes, LayersCount  );
  SetLength(LConnFirst, LayersCount  );
  SetLength(LConnLast, LayersCount  );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount, False, Network);
end;

(* ************************************************************************
  Same as MLPCreate0, but with one hidden layer (NHid neurons) with non-linear activation function. Output layer is linear.
  ************************************************************************ *)
procedure MLPCreate1(NIn, NHid, NOut: TLInt; var Network: TMultiLayerPerceptron);
var
  LSizes: TLIVec;
  LTypes: TLIVec;
  LConnFirst: TLIVec;
  LConnLast: TLIVec;
  LayersCount: TLInt;
  LastProc: TLInt;
begin
  LayersCount := 1 + 3 + 2;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount  );
  SetLength(LTypes, LayersCount  );
  SetLength(LConnFirst, LayersCount  );
  SetLength(LConnLast, LayersCount  );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount, False, Network);
end;

(* ************************************************************************
  Same as MLPCreate0, but with two hidden layers (NHid1 and  NHid2  neurons)
  with non-linear activation function. Output layer is linear.
  ************************************************************************ *)
procedure MLPCreate2(NIn, NHid1, NHid2, NOut: TLInt; var Network: TMultiLayerPerceptron);
var
  LSizes: TLIVec;
  LTypes: TLIVec;
  LConnFirst: TLIVec;
  LConnLast: TLIVec;
  LayersCount: TLInt;
  LastProc: TLInt;
begin
  LayersCount := 1 + 3 + 3 + 2;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount  );
  SetLength(LTypes, LayersCount  );
  SetLength(LConnFirst, LayersCount  );
  SetLength(LConnLast, LayersCount  );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid2, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount, False, Network);
end;

(* ************************************************************************
  Creates neural network with NIn inputs, NOut outputs, without hidden layers with non-linear output layer.
  Network weights are filled with small random values.

  Activation function of the output layer takes values:

  (B, +INF), if D>=0

  or

  (-INF, B), if D<0.
  ************************************************************************ *)
procedure MLPCreateB0(NIn, NOut: TLInt; b, d: TLFloat; var Network: TMultiLayerPerceptron);
var
  LSizes: TLIVec;
  LTypes: TLIVec;
  LConnFirst: TLIVec;
  LConnLast: TLIVec;
  LayersCount: TLInt;
  LastProc: TLInt;
  i: TLInt;
begin
  LayersCount := 1 + 3;
  if AP_FP_Greater_Eq(d, 0) then
    begin
      d := 1;
    end
  else
    begin
      d := -1;
    end;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount  );
  SetLength(LTypes, LayersCount  );
  SetLength(LConnFirst, LayersCount  );
  SetLength(LConnLast, LayersCount  );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(3, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount, False, Network);

  //
  // Turn on ouputs shift/scaling.
  //
  i := NIn;
  while i <= NIn + NOut - 1 do
    begin
      Network.ColumnMeans[i] := b;
      Network.ColumnSigmas[i] := d;
      inc(i);
    end;
end;

(* ************************************************************************
  Same as MLPCreateB0 but with non-linear hidden layer.
  ************************************************************************ *)
procedure MLPCreateB1(NIn, NHid, NOut: TLInt; b, d: TLFloat; var Network: TMultiLayerPerceptron);
var
  LSizes: TLIVec;
  LTypes: TLIVec;
  LConnFirst: TLIVec;
  LConnLast: TLIVec;
  LayersCount: TLInt;
  LastProc: TLInt;
  i: TLInt;
begin
  LayersCount := 1 + 3 + 3;
  if AP_FP_Greater_Eq(d, 0) then
    begin
      d := 1;
    end
  else
    begin
      d := -1;
    end;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount  );
  SetLength(LTypes, LayersCount  );
  SetLength(LConnFirst, LayersCount  );
  SetLength(LConnLast, LayersCount  );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(3, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount, False, Network);

  //
  // Turn on ouputs shift/scaling.
  //
  i := NIn;
  while i <= NIn + NOut - 1 do
    begin
      Network.ColumnMeans[i] := b;
      Network.ColumnSigmas[i] := d;
      inc(i);
    end;
end;

(* ************************************************************************
  Same as MLPCreateB0 but with two non-linear hidden layers.
  ************************************************************************ *)
procedure MLPCreateB2(NIn, NHid1, NHid2, NOut: TLInt; b, d: TLFloat; var Network: TMultiLayerPerceptron);
var
  LSizes: TLIVec;
  LTypes: TLIVec;
  LConnFirst: TLIVec;
  LConnLast: TLIVec;
  LayersCount: TLInt;
  LastProc: TLInt;
  i: TLInt;
begin
  LayersCount := 1 + 3 + 3 + 3;
  if AP_FP_Greater_Eq(d, 0) then
    begin
      d := 1;
    end
  else
    begin
      d := -1;
    end;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount  );
  SetLength(LTypes, LayersCount  );
  SetLength(LConnFirst, LayersCount  );
  SetLength(LConnLast, LayersCount  );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid2, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(3, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount, False, Network);

  //
  // Turn on ouputs shift/scaling.
  //
  i := NIn;
  while i <= NIn + NOut - 1 do
    begin
      Network.ColumnMeans[i] := b;
      Network.ColumnSigmas[i] := d;
      inc(i);
    end;
end;

(* ************************************************************************
  Creates  neural  network  with  NIn  inputs,  NOut outputs, without hidden
  layers with non-linear output layer. Network weights are filled with small
  random values. Activation function of the output layer takes values [A,B].
  ************************************************************************ *)
procedure MLPCreateR0(NIn, NOut: TLInt; a, b: TLFloat; var Network: TMultiLayerPerceptron);
var
  LSizes: TLIVec;
  LTypes: TLIVec;
  LConnFirst: TLIVec;
  LConnLast: TLIVec;
  LayersCount: TLInt;
  LastProc: TLInt;
  i: TLInt;
begin
  LayersCount := 1 + 3;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount  );
  SetLength(LTypes, LayersCount  );
  SetLength(LConnFirst, LayersCount  );
  SetLength(LConnLast, LayersCount  );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount, False, Network);

  //
  // Turn on outputs shift/scaling.
  //
  i := NIn;
  while i <= NIn + NOut - 1 do
    begin
      Network.ColumnMeans[i] := 0.5 * (a + b);
      Network.ColumnSigmas[i] := 0.5 * (a - b);
      inc(i);
    end;
end;

(* ************************************************************************
  Same as MLPCreateR0, but with non-linear hidden layer.
  ************************************************************************ *)
procedure MLPCreateR1(NIn, NHid, NOut: TLInt; a, b: TLFloat; var Network: TMultiLayerPerceptron);
var
  LSizes: TLIVec;
  LTypes: TLIVec;
  LConnFirst: TLIVec;
  LConnLast: TLIVec;
  LayersCount: TLInt;
  LastProc: TLInt;
  i: TLInt;
begin
  LayersCount := 1 + 3 + 3;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount  );
  SetLength(LTypes, LayersCount  );
  SetLength(LConnFirst, LayersCount  );
  SetLength(LConnLast, LayersCount  );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount, False, Network);

  //
  // Turn on outputs shift/scaling.
  //
  i := NIn;
  while i <= NIn + NOut - 1 do
    begin
      Network.ColumnMeans[i] := 0.5 * (a + b);
      Network.ColumnSigmas[i] := 0.5 * (a - b);
      inc(i);
    end;
end;

(* ************************************************************************
  Same as MLPCreateR0, but with two non-linear hidden layers.
  ************************************************************************ *)
procedure MLPCreateR2(NIn, NHid1, NHid2, NOut: TLInt; a, b: TLFloat; var Network: TMultiLayerPerceptron);
var
  LSizes: TLIVec;
  LTypes: TLIVec;
  LConnFirst: TLIVec;
  LConnLast: TLIVec;
  LayersCount: TLInt;
  LastProc: TLInt;
  i: TLInt;
begin
  LayersCount := 1 + 3 + 3 + 3;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount  );
  SetLength(LTypes, LayersCount  );
  SetLength(LConnFirst, LayersCount  );
  SetLength(LConnLast, LayersCount  );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid2, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount,
    False, Network);

  //
  // Turn on outputs shift/scaling.
  //
  i := NIn;
  while i <= NIn + NOut - 1 do
    begin
      Network.ColumnMeans[i] := 0.5 * (a + b);
      Network.ColumnSigmas[i] := 0.5 * (a - b);
      inc(i);
    end;
end;

(* ************************************************************************
  Creates classifier network with NIn inputs and NOut possible classes.
  Network contains no hidden layers and linear output layer with SOFTMAX-
  normalization (so outputs sums up to 1.0 and converge to posterior probabilities).
  ************************************************************************ *)
procedure MLPCreateC0(NIn, NOut: TLInt; var Network: TMultiLayerPerceptron);
var
  LSizes: TLIVec;
  LTypes: TLIVec;
  LConnFirst: TLIVec;
  LConnLast: TLIVec;
  LayersCount: TLInt;
  LastProc: TLInt;
begin
  Assert(NOut >= 2, 'MLPCreateC0: NOut<2!');
  LayersCount := 1 + 2 + 1;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount  );
  SetLength(LTypes, LayersCount  );
  SetLength(LConnFirst, LayersCount  );
  SetLength(LConnLast, LayersCount  );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut - 1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddZeroLayer(LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount, True, Network);
end;

(* ************************************************************************
  Same as MLPCreateC0, but with one non-linear hidden layer.
  ************************************************************************ *)
procedure MLPCreateC1(NIn, NHid, NOut: TLInt; var Network: TMultiLayerPerceptron);
var
  LSizes: TLIVec;
  LTypes: TLIVec;
  LConnFirst: TLIVec;
  LConnLast: TLIVec;
  LayersCount: TLInt;
  LastProc: TLInt;
begin
  Assert(NOut >= 2, 'MLPCreateC1: NOut<2!');
  LayersCount := 1 + 3 + 2 + 1;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount  );
  SetLength(LTypes, LayersCount  );
  SetLength(LConnFirst, LayersCount  );
  SetLength(LConnLast, LayersCount  );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut - 1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddZeroLayer(LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount, True, Network);
end;

(* ************************************************************************
  Same as MLPCreateC0, but with two non-linear hidden layers.
  ************************************************************************ *)
procedure MLPCreateC2(NIn, NHid1, NHid2, NOut: TLInt; var Network: TMultiLayerPerceptron);
var
  LSizes: TLIVec;
  LTypes: TLIVec;
  LConnFirst: TLIVec;
  LConnLast: TLIVec;
  LayersCount: TLInt;
  LastProc: TLInt;
begin
  Assert(NOut >= 2, 'MLPCreateC2: NOut<2!');
  LayersCount := 1 + 3 + 3 + 2 + 1;

  //
  // Allocate arrays
  //
  SetLength(LSizes, LayersCount  );
  SetLength(LTypes, LayersCount  );
  SetLength(LConnFirst, LayersCount  );
  SetLength(LConnLast, LayersCount  );

  //
  // Layers
  //
  AddInputLayer(NIn, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NHid2, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddActivationLayer(1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddBiasedSummatorLayer(NOut - 1, LSizes, LTypes, LConnFirst, LConnLast, LastProc);
  AddZeroLayer(LSizes, LTypes, LConnFirst, LConnLast, LastProc);

  //
  // Create
  //
  MLPCreate(NIn, NOut, LSizes, LTypes, LConnFirst, LConnLast, LayersCount, True, Network);
end;

procedure MLPFree(var Network: TMultiLayerPerceptron);
begin
  SetLength(Network.StructInfo, 0);
  SetLength(Network.Weights, 0);
  SetLength(Network.ColumnMeans, 0);
  SetLength(Network.ColumnSigmas, 0);
  SetLength(Network.Neurons, 0);
  SetLength(Network.DFDNET, 0);
  SetLength(Network.DError, 0);
  SetLength(Network.x, 0);
  SetLength(Network.y, 0);
  SetLength(Network.Chunks, 0);
  SetLength(Network.NWBuf, 0);
end;

(* ************************************************************************
  Copying of neural network

  INPUT PARAMETERS:
  Network1 -   original

  OUTPUT PARAMETERS:
  Network2 -   copy
  ************************************************************************ *)
procedure MLPCopy(const Network1: TMultiLayerPerceptron;
  var Network2: TMultiLayerPerceptron);
var
  i: TLInt;
  SSize: TLInt;
  NTotal: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
begin

  //
  // Unload info
  //
  SSize := Network1.StructInfo[0];
  NIn := Network1.StructInfo[1];
  NOut := Network1.StructInfo[2];
  NTotal := Network1.StructInfo[3];
  WCount := Network1.StructInfo[4];

  //
  // Allocate space
  //
  SetLength(Network2.StructInfo, SSize  );
  SetLength(Network2.Weights, WCount  );
  if MLPIsSoftmax(Network1) then
    begin
      SetLength(Network2.ColumnMeans, NIn  );
      SetLength(Network2.ColumnSigmas, NIn  );
    end
  else
    begin
      SetLength(Network2.ColumnMeans, NIn + NOut  );
      SetLength(Network2.ColumnSigmas, NIn + NOut  );
    end;
  SetLength(Network2.Neurons, NTotal  );
  SetLength(Network2.Chunks, 3 * NTotal + 1, ChunkSize  );
  SetLength(Network2.NWBuf, Max(WCount, 2 * NOut)  );
  SetLength(Network2.DFDNET, NTotal  );
  SetLength(Network2.x, NIn  );
  SetLength(Network2.y, NOut  );
  SetLength(Network2.DError, NTotal  );

  //
  // Copy
  //
  i := 0;
  while i <= SSize - 1 do
    begin
      Network2.StructInfo[i] := Network1.StructInfo[i];
      inc(i);
    end;
  APVMove(@Network2.Weights[0], 0, WCount - 1, @Network1.Weights[0], 0,
    WCount - 1);
  if MLPIsSoftmax(Network1) then
    begin
      APVMove(@Network2.ColumnMeans[0], 0, NIn - 1, @Network1.ColumnMeans[0], 0, NIn - 1);
      APVMove(@Network2.ColumnSigmas[0], 0, NIn - 1, @Network1.ColumnSigmas[0], 0, NIn - 1);
    end
  else
    begin
      APVMove(@Network2.ColumnMeans[0], 0, NIn + NOut - 1, @Network1.ColumnMeans[0], 0, NIn + NOut - 1);
      APVMove(@Network2.ColumnSigmas[0], 0, NIn + NOut - 1, @Network1.ColumnSigmas[0], 0, NIn + NOut - 1);
    end;
  APVMove(@Network2.Neurons[0], 0, NTotal - 1, @Network1.Neurons[0], 0, NTotal - 1);
  APVMove(@Network2.DFDNET[0], 0, NTotal - 1, @Network1.DFDNET[0], 0, NTotal - 1);
  APVMove(@Network2.x[0], 0, NIn - 1, @Network1.x[0], 0, NIn - 1);
  APVMove(@Network2.y[0], 0, NOut - 1, @Network1.y[0], 0, NOut - 1);
  APVMove(@Network2.DError[0], 0, NTotal - 1, @Network1.DError[0], 0, NTotal - 1);
end;

(* ************************************************************************
  Serialization of TMultiLayerPerceptron strucure

  INPUT PARAMETERS:
  Network -   original

  OUTPUT PARAMETERS:
  RA      -   array of real numbers which stores network,
  array[0..RLen-1]
  RLen    -   RA lenght
  ************************************************************************ *)
procedure MLPSerialize(const Network: TMultiLayerPerceptron;
  var RA: TLVec; var RLen: TLInt);
var
  i: TLInt;
  SSize: TLInt;
  NTotal: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
  SigmaLen: TLInt;
  Offs: TLInt;
begin

  //
  // Unload info
  //
  SSize := Network.StructInfo[0];
  NIn := Network.StructInfo[1];
  NOut := Network.StructInfo[2];
  NTotal := Network.StructInfo[3];
  WCount := Network.StructInfo[4];
  if MLPIsSoftmax(Network) then
    begin
      SigmaLen := NIn;
    end
  else
    begin
      SigmaLen := NIn + NOut;
    end;

  //
  // RA format:
  // LEN         DESRC.
  // 1           RLen
  // 1           version (MLPVNum)
  // 1           StructInfo size
  // SSize       StructInfo
  // WCount      Weights
  // SigmaLen    ColumnMeans
  // SigmaLen    ColumnSigmas
  //
  RLen := 3 + SSize + WCount + 2 * SigmaLen;
  SetLength(RA, RLen  );
  RA[0] := RLen;
  RA[1] := MLPVNum;
  RA[2] := SSize;
  Offs := 3;
  i := 0;
  while i <= SSize - 1 do
    begin
      RA[Offs + i] := Network.StructInfo[i];
      inc(i);
    end;
  Offs := Offs + SSize;
  APVMove(@RA[0], Offs, Offs + WCount - 1, @Network.Weights[0], 0, WCount - 1);
  Offs := Offs + WCount;
  APVMove(@RA[0], Offs, Offs + SigmaLen - 1, @Network.ColumnMeans[0], 0, SigmaLen - 1);
  Offs := Offs + SigmaLen;
  APVMove(@RA[0], Offs, Offs + SigmaLen - 1, @Network.ColumnSigmas[0], 0, SigmaLen - 1);
  Offs := Offs + SigmaLen;
end;

(* ************************************************************************
  Unserialization of TMultiLayerPerceptron strucure

  INPUT PARAMETERS:
  RA      -   real array which stores network

  OUTPUT PARAMETERS:
  Network -   restored network
  ************************************************************************ *)
procedure MLPUNSerialize(const RA: TLVec;
  var Network: TMultiLayerPerceptron);
var
  i: TLInt;
  SSize: TLInt;
  NTotal: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
  SigmaLen: TLInt;
  Offs: TLInt;
begin
  Assert(Round(RA[1]) = MLPVNum, 'MLPUnserialize: incorrect array!');

  //
  // Unload StructInfo from IA
  //
  Offs := 3;
  SSize := Round(RA[2]);
  SetLength(Network.StructInfo, SSize  );
  i := 0;
  while i <= SSize - 1 do
    begin
      Network.StructInfo[i] := Round(RA[Offs + i]);
      inc(i);
    end;
  Offs := Offs + SSize;

  //
  // Unload info from StructInfo
  //
  SSize := Network.StructInfo[0];
  NIn := Network.StructInfo[1];
  NOut := Network.StructInfo[2];
  NTotal := Network.StructInfo[3];
  WCount := Network.StructInfo[4];
  if Network.StructInfo[6] = 0 then
    begin
      SigmaLen := NIn + NOut;
    end
  else
    begin
      SigmaLen := NIn;
    end;

  //
  // Allocate space for other fields
  //
  SetLength(Network.Weights, WCount  );
  SetLength(Network.ColumnMeans, SigmaLen  );
  SetLength(Network.ColumnSigmas, SigmaLen  );
  SetLength(Network.Neurons, NTotal  );
  SetLength(Network.Chunks, 3 * NTotal + 1, ChunkSize  );
  SetLength(Network.NWBuf, Max(WCount, 2 * NOut)  );
  SetLength(Network.DFDNET, NTotal  );
  SetLength(Network.x, NIn  );
  SetLength(Network.y, NOut  );
  SetLength(Network.DError, NTotal  );

  //
  // Copy parameters from RA
  //
  APVMove(@Network.Weights[0], 0, WCount - 1, @RA[0], Offs, Offs + WCount - 1);
  Offs := Offs + WCount;
  APVMove(@Network.ColumnMeans[0], 0, SigmaLen - 1, @RA[0], Offs, Offs + SigmaLen - 1);
  Offs := Offs + SigmaLen;
  APVMove(@Network.ColumnSigmas[0], 0, SigmaLen - 1, @RA[0], Offs, Offs + SigmaLen - 1);
  Offs := Offs + SigmaLen;
end;

(* ************************************************************************
  Randomization of neural network weights
  ************************************************************************ *)
procedure MLPRandomize(var Network: TMultiLayerPerceptron); overload;
var
  i: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  for i := 0 to WCount - 1 do
    begin
      Network.Weights[i] := RandomReal - 0.5;
    end;
end;

(* ************************************************************************
  Randomization of neural network weights
  ************************************************************************ *)
procedure MLPRandomize(var Network: TMultiLayerPerceptron;
  const Diameter: TLFloat); overload;
var
  i: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  for i := 0 to WCount - 1 do
    begin
      Network.Weights[i] := (RandomReal - 0.5) * Diameter;
    end;
end;

(* ************************************************************************
  Randomization of neural network weights
  ************************************************************************ *)
procedure MLPRandomize(var Network: TMultiLayerPerceptron;
  const WBest: TLVec; const Diameter: TLFloat); overload;
var
  i: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  for i := 0 to WCount - 1 do
    begin
      Network.Weights[i] := WBest[i] + (RandomReal - 0.5) * Diameter;
    end;
end;

(* ************************************************************************
  Randomization of neural network weights and standartisator
  ************************************************************************ *)
procedure MLPRandomizeFull(var Network: TMultiLayerPerceptron);
var
  i: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
  NTotal: TLInt;
  IStart: TLInt;
  Offs: TLInt;
  NType: TLInt;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  NTotal := Network.StructInfo[3];
  IStart := Network.StructInfo[5];

  //
  // Process network
  //
  i := 0;
  while i <= WCount - 1 do
    begin
      Network.Weights[i] := RandomReal - 0.5;
      inc(i);
    end;
  i := 0;
  while i <= NIn - 1 do
    begin
      Network.ColumnMeans[i] := 2 * RandomReal - 1;
      Network.ColumnSigmas[i] := 1.5 * RandomReal + 0.5;
      inc(i);
    end;
  if not MLPIsSoftmax(Network) then
    begin
      i := 0;
      while i <= NOut - 1 do
        begin
          Offs := IStart + (NTotal - NOut + i) * NFieldWidth;
          NType := Network.StructInfo[Offs + 0];
          if NType = 0 then
            begin

              //
              // Shifts are changed only for linear outputs neurons
              //
              Network.ColumnMeans[NIn + i] := 2 * RandomReal - 1;
            end;
          if (NType = 0) or (NType = 3) then
            begin

              //
              // Scales are changed only for linear or bounded outputs neurons.
              // Note that scale randomization preserves sign.
              //
              Network.ColumnSigmas[NIn + i] := Sign(Network.ColumnSigmas[NIn + i]) * (1.5 * RandomReal + 0.5);
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Internal subroutine.
  ************************************************************************ *)
procedure MLPInitPreprocessor(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; SSize: TLInt);
var
  i: TLInt;
  j: TLInt;
  JMAX: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
  NTotal: TLInt;
  IStart: TLInt;
  Offs: TLInt;
  NType: TLInt;
  Means: TLVec;
  Sigmas: TLVec;
  s: TLFloat;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  NTotal := Network.StructInfo[3];
  IStart := Network.StructInfo[5];

  //
  // Means/Sigmas
  //
  if MLPIsSoftmax(Network) then
    begin
      JMAX := NIn - 1;
    end
  else
    begin
      JMAX := NIn + NOut - 1;
    end;
  SetLength(Means, JMAX + 1);
  SetLength(Sigmas, JMAX + 1);
  j := 0;
  while j <= JMAX do
    begin
      Means[j] := 0;
      i := 0;
      while i <= SSize - 1 do
        begin
          Means[j] := Means[j] + xy[i, j];
          inc(i);
        end;
      Means[j] := Means[j] / SSize;
      Sigmas[j] := 0;
      i := 0;
      while i <= SSize - 1 do
        begin
          Sigmas[j] := Sigmas[j] + AP_Sqr(xy[i, j] - Means[j]);
          inc(i);
        end;
      Sigmas[j] := Sqrt(Sigmas[j] / SSize);
      inc(j);
    end;

  //
  // Inputs
  //
  i := 0;
  while i <= NIn - 1 do
    begin
      Network.ColumnMeans[i] := Means[i];
      Network.ColumnSigmas[i] := Sigmas[i];
      if AP_FP_Eq(Network.ColumnSigmas[i], 0) then
        begin
          Network.ColumnSigmas[i] := 1;
        end;
      inc(i);
    end;

  //
  // Outputs
  //
  if not MLPIsSoftmax(Network) then
    begin
      i := 0;
      while i <= NOut - 1 do
        begin
          Offs := IStart + (NTotal - NOut + i) * NFieldWidth;
          NType := Network.StructInfo[Offs + 0];

          //
          // Linear outputs
          //
          if NType = 0 then
            begin
              Network.ColumnMeans[NIn + i] := Means[NIn + i];
              Network.ColumnSigmas[NIn + i] := Sigmas[NIn + i];
              if AP_FP_Eq(Network.ColumnSigmas[NIn + i], 0) then
                begin
                  Network.ColumnSigmas[NIn + i] := 1;
                end;
            end;

          //
          // Bounded outputs (half-interval)
          //
          if NType = 3 then
            begin
              s := Means[NIn + i] - Network.ColumnMeans[NIn + i];
              if AP_FP_Eq(s, 0) then
                begin
                  s := Sign(Network.ColumnSigmas[NIn + i]);
                end;
              if AP_FP_Eq(s, 0) then
                begin
                  s := 1.0;
                end;
              Network.ColumnSigmas[NIn + i] := Sign(Network.ColumnSigmas[NIn + i]) * AbsReal(s);
              if AP_FP_Eq(Network.ColumnSigmas[NIn + i], 0) then
                begin
                  Network.ColumnSigmas[NIn + i] := 1;
                end;
            end;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Returns information about initialized network: number of inputs, outputs, weights.
  ************************************************************************ *)
procedure MLPProperties(const Network: TMultiLayerPerceptron;
  var NIn: TLInt; var NOut: TLInt; var WCount: TLInt);
begin
  NIn := Network.StructInfo[1];
  NOut := Network.StructInfo[2];
  WCount := Network.StructInfo[4];
end;

(* ************************************************************************
  Tells whether network is SOFTMAX-normalized (i.e. classifier) or not.
  ************************************************************************ *)
function MLPIsSoftmax(const Network: TMultiLayerPerceptron): Boolean;
begin
  Result := Network.StructInfo[6] = 1;
end;

(* ************************************************************************
  Procesing

  INPUT PARAMETERS:
  Network -   neural network
  X       -   input vector,  array[0..NIn-1].

  OUTPUT PARAMETERS:
  Y       -   result. Regression estimate when solving regression  task,
  vector of posterior probabilities for classification task.
  Subroutine does not allocate memory for this vector, it is
  responsibility of a caller to allocate it. Array must be at least [0..NOut-1].
  ************************************************************************ *)
procedure MLPProcess(var Network: TMultiLayerPerceptron; const x: TLVec; var y: TLVec);
begin
  MLPInternalProcessVector(Network.StructInfo, Network.Weights,
    Network.ColumnMeans, Network.ColumnSigmas, Network.Neurons,
    Network.DFDNET, x, y);
end;

(* ************************************************************************
  Error function for neural network, internal subroutine.
  ************************************************************************ *)
function MLPError(var Network: TMultiLayerPerceptron; const xy: TLMatrix;
  SSize: TLInt): TLFloat;
var
  i: TLInt;
  k: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
  E: TLFloat;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  Result := 0;
  i := 0;
  while i <= SSize - 1 do
    begin
      APVMove(@Network.x[0], 0, NIn - 1, @xy[i][0], 0, NIn - 1);
      MLPProcess(Network, Network.x, Network.y);
      if MLPIsSoftmax(Network) then
        begin

          //
          // class labels outputs
          //
          k := Round(xy[i, NIn]);
          if (k >= 0) and (k < NOut) then
            begin
              Network.y[k] := Network.y[k] - 1;
            end;
        end
      else
        begin

          //
          // real outputs
          //
          APVSub(@Network.y[0], 0, NOut - 1, @xy[i][0], NIn, NIn + NOut - 1);
        end;
      E := APVDotProduct(@Network.y[0], 0, NOut - 1, @Network.y[0], 0, NOut - 1);
      Result := Result + E / 2;
      inc(i);
    end;
end;

(* ************************************************************************
  Natural error function for neural network, internal subroutine.
  ************************************************************************ *)
function MLPErrorN(var Network: TMultiLayerPerceptron; const xy: TLMatrix;
  SSize: TLInt): TLFloat;
var
  i: TLInt;
  k: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
  E: TLFloat;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  Result := 0;
  for i := 0 to SSize - 1 do
    begin

      //
      // Process vector
      //
      APVMove(@Network.x[0], 0, NIn - 1, @xy[i][0], 0, NIn - 1);
      MLPProcess(Network, Network.x, Network.y);

      //
      // Update error function
      //
      if Network.StructInfo[6] = 0 then
        begin

          //
          // Least squares error function
          //
          APVSub(@Network.y[0], 0, NOut - 1, @xy[i][0], NIn, NIn + NOut - 1);
          E := APVDotProduct(@Network.y[0], 0, NOut - 1, @Network.y[0], 0, NOut - 1);
          Result := Result + E * 0.5;
        end
      else
        begin

          //
          // Cross-entropy error function
          //
          k := Round(xy[i, NIn]);
          if (k >= 0) and (k < NOut) then
            begin
              Result := Result + SafeCrossEntropy(1, Network.y[k]);
            end;
        end;
    end;
end;

(* ************************************************************************
  Classification error
  ************************************************************************ *)
function MLPClsError(var Network: TMultiLayerPerceptron; const xy: TLMatrix;
  SSize: TLInt): TLInt;
var
  i: TLInt;
  j: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
  WorkX: TLVec;
  WorkY: TLVec;
  NN: TLInt;
  ns: TLInt;
  NMAX: TLInt;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  SetLength(WorkX, NIn  );
  SetLength(WorkY, NOut  );
  Result := 0;
  i := 0;
  while i <= SSize - 1 do
    begin

      //
      // Process
      //
      APVMove(@WorkX[0], 0, NIn - 1, @xy[i][0], 0, NIn - 1);
      MLPProcess(Network, WorkX, WorkY);

      //
      // Network version of the answer
      //
      NMAX := 0;
      j := 0;
      while j <= NOut - 1 do
        begin
          if AP_FP_Greater(WorkY[j], WorkY[NMAX]) then
            begin
              NMAX := j;
            end;
          inc(j);
        end;
      NN := NMAX;

      //
      // Right answer
      //
      if MLPIsSoftmax(Network) then
        begin
          ns := Round(xy[i, NIn]);
        end
      else
        begin
          NMAX := 0;
          j := 0;
          while j <= NOut - 1 do
            begin
              if AP_FP_Greater(xy[i, NIn + j], xy[i, NIn + NMAX]) then
                begin
                  NMAX := j;
                end;
              inc(j);
            end;
          ns := NMAX;
        end;

      //
      // compare
      //
      if NN <> ns then
        begin
          Result := Result + 1;
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Relative classification error on the test set

  INPUT PARAMETERS:
  Network -   network
  XY      -   test set
  NPoints -   test set size

  RESULT:
  percent of incorrectly classified cases. Works both for
  classifier networks and general purpose networks used as
  classifiers.
  ************************************************************************ *)
function MLPRelClsError(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; NPoints: TLInt): TLFloat;
begin
  Result := AP_Float(MLPClsError(Network, xy, NPoints)) / NPoints;
end;

(* ************************************************************************
  Average cross-entropy (in bits per element) on the test set

  INPUT PARAMETERS:
  Network -   neural network
  XY      -   test set
  NPoints -   test set size

  RESULT:
  CrossEntropy/(NPoints*LN(2)).
  Zero if network solves regression task.
  ************************************************************************ *)
function MLPAvgCE(var Network: TMultiLayerPerceptron; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
begin
  if MLPIsSoftmax(Network) then
    begin
      MLPProperties(Network, NIn, NOut, WCount);
      Result := MLPErrorN(Network, xy, NPoints) / (NPoints * ln(2));
    end
  else
    begin
      Result := 0;
    end;
end;

(* ************************************************************************
  RMS error on the test set

  INPUT PARAMETERS:
  Network -   neural network
  XY      -   test set
  NPoints -   test set size

  RESULT:
  root mean square error.
  Its meaning for regression task is obvious. As for
  classification task, RMS error means error when estimating posterior
  probabilities.
  ************************************************************************ *)
function MLPRMSError(var Network: TMultiLayerPerceptron; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  Result := Sqrt(2 * MLPError(Network, xy, NPoints) / (NPoints * NOut));
end;

(* ************************************************************************
  Average error on the test set

  INPUT PARAMETERS:
  Network -   neural network
  XY      -   test set
  NPoints -   test set size

  RESULT:
  Its meaning for regression task is obvious. As for
  classification task, it means average error when estimating posterior
  probabilities.
  ************************************************************************ *)
function MLPAvgError(var Network: TMultiLayerPerceptron; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  i: TLInt;
  j: TLInt;
  k: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  Result := 0;
  i := 0;
  while i <= NPoints - 1 do
    begin
      APVMove(@Network.x[0], 0, NIn - 1, @xy[i][0], 0, NIn - 1);
      MLPProcess(Network, Network.x, Network.y);
      if MLPIsSoftmax(Network) then
        begin

          //
          // class labels
          //
          k := Round(xy[i, NIn]);
          j := 0;
          while j <= NOut - 1 do
            begin
              if j = k then
                begin
                  Result := Result + AbsReal(1 - Network.y[j]);
                end
              else
                begin
                  Result := Result + AbsReal(Network.y[j]);
                end;
              inc(j);
            end;
        end
      else
        begin

          //
          // real outputs
          //
          j := 0;
          while j <= NOut - 1 do
            begin
              Result := Result + AbsReal(xy[i, NIn + j] - Network.y[j]);
              inc(j);
            end;
        end;
      inc(i);
    end;
  Result := Result / (NPoints * NOut);
end;

(* ************************************************************************
  Average relative error on the test set

  INPUT PARAMETERS:
  Network -   neural network
  XY      -   test set
  NPoints -   test set size

  RESULT:
  Its meaning for regression task is obvious. As for
  classification task, it means average relative error when estimating
  posterior probability of belonging to the correct class.

  ************************************************************************ *)
function MLPAvgRelError(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; NPoints: TLInt): TLFloat;
var
  i: TLInt;
  j: TLInt;
  k: TLInt;
  LK: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  Result := 0;
  k := 0;
  i := 0;
  while i <= NPoints - 1 do
    begin
      APVMove(@Network.x[0], 0, NIn - 1, @xy[i][0], 0, NIn - 1);
      MLPProcess(Network, Network.x, Network.y);
      if MLPIsSoftmax(Network) then
        begin

          //
          // class labels
          //
          LK := Round(xy[i, NIn]);
          j := 0;
          while j <= NOut - 1 do
            begin
              if j = LK then
                begin
                  Result := Result + AbsReal(1 - Network.y[j]);
                  k := k + 1;
                end;
              inc(j);
            end;
        end
      else
        begin

          //
          // real outputs
          //
          j := 0;
          while j <= NOut - 1 do
            begin
              if AP_FP_NEq(xy[i, NIn + j], 0) then
                begin
                  Result := Result + AbsReal(xy[i, NIn + j] - Network.y[j]) / AbsReal(xy[i, NIn + j]);
                  k := k + 1;
                end;
              inc(j);
            end;
        end;
      inc(i);
    end;
  if k <> 0 then
    begin
      Result := Result / k;
    end;
end;

(* ************************************************************************
  Gradient calculation. Internal subroutine.
  ************************************************************************ *)
procedure MLPGrad(var Network: TMultiLayerPerceptron; const x: TLVec;
  const DesiredY: TLVec; var E: TLFloat; var Grad: TLVec);
var
  i: TLInt;
  NOut: TLInt;
  NTotal: TLInt;
begin

  //
  // Prepare dError/dOut, internal structures
  //
  MLPProcess(Network, x, Network.y);
  NOut := Network.StructInfo[2];
  NTotal := Network.StructInfo[3];
  E := 0;
  i := 0;
  while i <= NTotal - 1 do
    begin
      Network.DError[i] := 0;
      inc(i);
    end;
  i := 0;
  while i <= NOut - 1 do
    begin
      Network.DError[NTotal - NOut + i] := Network.y[i] - DesiredY[i];
      E := E + AP_Sqr(Network.y[i] - DesiredY[i]) / 2;
      inc(i);
    end;

  //
  // gradient
  //
  MLPInternalCalculateGradient(Network, Network.Neurons, Network.Weights, Network.DError, Grad, False);
end;

(* ************************************************************************
  Gradient calculation (natural error function). Internal subroutine.
  ************************************************************************ *)
procedure MLPGradN(var Network: TMultiLayerPerceptron; const x: TLVec;
  const DesiredY: TLVec; var E: TLFloat; var Grad: TLVec);
var
  s: TLFloat;
  i: TLInt;
  NOut: TLInt;
  NTotal: TLInt;
begin

  //
  // Prepare dError/dOut, internal structures
  //
  MLPProcess(Network, x, Network.y);
  NOut := Network.StructInfo[2];
  NTotal := Network.StructInfo[3];
  i := 0;
  while i <= NTotal - 1 do
    begin
      Network.DError[i] := 0;
      inc(i);
    end;
  E := 0;
  if Network.StructInfo[6] = 0 then
    begin

      //
      // Regression network, least squares
      //
      i := 0;
      while i <= NOut - 1 do
        begin
          Network.DError[NTotal - NOut + i] := Network.y[i] - DesiredY[i];
          E := E + AP_Sqr(Network.y[i] - DesiredY[i]) / 2;
          inc(i);
        end;
    end
  else
    begin

      //
      // Classification network, cross-entropy
      //
      s := 0;
      i := 0;
      while i <= NOut - 1 do
        begin
          s := s + DesiredY[i];
          inc(i);
        end;
      i := 0;
      while i <= NOut - 1 do
        begin
          Network.DError[NTotal - NOut + i] := s * Network.y[i] - DesiredY[i];
          E := E + SafeCrossEntropy(DesiredY[i], Network.y[i]);
          inc(i);
        end;
    end;

  //
  // gradient
  //
  MLPInternalCalculateGradient(Network, Network.Neurons, Network.Weights, Network.DError, Grad, True);
end;

(* ************************************************************************
  Batch gradient calculation. Internal subroutine.
  ************************************************************************ *)
procedure MLPGradBatch(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; SSize: TLInt; var E: TLFloat;
  var Grad: TLVec);
var
  i: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  i := 0;
  while i <= WCount - 1 do
    begin
      Grad[i] := 0;
      inc(i);
    end;
  E := 0;
  i := 0;
  while i <= SSize - 1 do
    begin
      MLPChunkedGradient(Network, xy, i, Min(SSize, i + ChunkSize) - i, E, Grad, False);
      i := i + ChunkSize;
    end;
end;

(* ************************************************************************
  Batch gradient calculation (natural error function). Internal subroutine.
  ************************************************************************ *)
procedure MLPGradNBatch(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; SSize: TLInt; var E: TLFloat;
  var Grad: TLVec);
var
  i: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  i := 0;
  while i <= WCount - 1 do
    begin
      Grad[i] := 0;
      inc(i);
    end;
  E := 0;
  i := 0;
  while i <= SSize - 1 do
    begin
      MLPChunkedGradient(Network, xy, i, Min(SSize, i + ChunkSize) - i, E, Grad, True);
      i := i + ChunkSize;
    end;
end;

(* ************************************************************************
  Batch Hessian calculation (natural error function) using R-algorithm.
  Internal subroutine.

  Hessian calculation based on R-algorithm described in
  "Fast Exact Multiplication by the Hessian",
  B. A. Pearlmutter,
  Neural Computation, 1994.
  ************************************************************************ *)
procedure MLPHessianNBatch(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; SSize: TLInt; var E: TLFloat;
  var Grad: TLVec; var h: TLMatrix);
begin
  MLPHessianBatchInternal(Network, xy, SSize, True, E, Grad, h);
end;

(* ************************************************************************
  Batch Hessian calculation using R-algorithm.
  Internal subroutine.

  Hessian calculation based on R-algorithm described in
  "Fast Exact Multiplication by the Hessian",
  B. A. Pearlmutter,
  Neural Computation, 1994.
  ************************************************************************ *)
procedure MLPHessianBatch(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; SSize: TLInt; var E: TLFloat;
  var Grad: TLVec; var h: TLMatrix);
begin
  MLPHessianBatchInternal(Network, xy, SSize, False, E, Grad, h);
end;

(* ************************************************************************
  Internal subroutine, shouldn't be called by user.
  ************************************************************************ *)
procedure MLPInternalProcessVector(const StructInfo: TLIVec;
  const Weights: TLVec; const ColumnMeans: TLVec;
  const ColumnSigmas: TLVec; var Neurons: TLVec;
  var DFDNET: TLVec; const x: TLVec; var y: TLVec);
var
  i: TLInt;
  n1: TLInt;
  n2: TLInt;
  W1: TLInt;
  W2: TLInt;
  NTotal: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  IStart: TLInt;
  Offs: TLInt;
  NET: TLFloat;
  f: TLFloat;
  df: TLFloat;
  D2F: TLFloat;
  mx: TLFloat;
  PErr: Boolean;
begin

  //
  // Read network geometry
  //
  NIn := StructInfo[1];
  NOut := StructInfo[2];
  NTotal := StructInfo[3];
  IStart := StructInfo[5];

  //
  // Inputs standartisation and putting in the network
  //
  for i := 0 to NIn - 1 do
    begin
      if AP_FP_NEq(ColumnSigmas[i], 0) then
        begin
          Neurons[i] := (x[i] - ColumnMeans[i]) / ColumnSigmas[i];
        end
      else
        begin
          Neurons[i] := x[i] - ColumnMeans[i];
        end;
    end;

  //
  // Process network
  //
  i := 0;
  for i := 0 to NTotal - 1 do
    begin
      Offs := IStart + i * NFieldWidth;
      if StructInfo[Offs + 0] > 0 then
        begin

          //
          // Activation function
          //
          MLPActivationFunction(Neurons[StructInfo[Offs + 2]], StructInfo[Offs + 0], f, df, D2F);
          Neurons[i] := f;
          DFDNET[i] := df;
        end;
      if StructInfo[Offs + 0] = 0 then
        begin

          //
          // Adaptive summator
          //
          n1 := StructInfo[Offs + 2];
          n2 := n1 + StructInfo[Offs + 1] - 1;
          W1 := StructInfo[Offs + 3];
          W2 := W1 + StructInfo[Offs + 1] - 1;
          NET := APVDotProduct(@Weights[0], W1, W2, @Neurons[0], n1, n2);
          Neurons[i] := NET;
          DFDNET[i] := 1.0;
        end;
      if StructInfo[Offs + 0] < 0 then
        begin
          PErr := True;
          if StructInfo[Offs + 0] = -2 then
            begin

              //
              // input neuron, left unchanged
              //
              PErr := False;
            end;
          if StructInfo[Offs + 0] = -3 then
            begin

              //
              // "-1" neuron
              //
              Neurons[i] := -1;
              PErr := False;
            end;
          if StructInfo[Offs + 0] = -4 then
            begin

              //
              // "0" neuron
              //
              Neurons[i] := 0;
              PErr := False;
            end;
          Assert(not PErr, 'MLPInternalProcessVector: internal error - unknown neuron type!');
        end;
    end;

  //
  // Extract result
  //
  APVMove(@y[0], 0, NOut - 1, @Neurons[0], NTotal - NOut, NTotal - 1);

  //
  // Softmax post-processing or standardisation if needed
  //
  Assert((StructInfo[6] = 0) or (StructInfo[6] = 1), 'MLPInternalProcessVector: unknown normalization type!');
  if StructInfo[6] = 1 then
    begin

      //
      // Softmax
      //
      mx := y[0];
      for i := 1 to NOut - 1 do
        begin
          mx := Max(mx, y[i]);
        end;
      NET := 0;
      for i := 0 to NOut - 1 do
        begin
          y[i] := Exp(y[i] - mx);
          NET := NET + y[i];
        end;
      for i := 0 to NOut - 1 do
        begin
          y[i] := y[i] / NET;
        end;
    end
  else
    begin

      //
      // Standardisation
      //
      for i := 0 to NOut - 1 do
        begin
          y[i] := y[i] * ColumnSigmas[NIn + i] + ColumnMeans[NIn + i];
        end;
    end;
end;

(* ************************************************************************
  Internal subroutine: adding new input layer to network
  ************************************************************************ *)
procedure AddInputLayer(NCount: TLInt; var LSizes: TLIVec;
  var LTypes: TLIVec; var LConnFirst: TLIVec;
  var LConnLast: TLIVec; var LastProc: TLInt);
begin
  LSizes[0] := NCount;
  LTypes[0] := -2;
  LConnFirst[0] := 0;
  LConnLast[0] := 0;
  LastProc := 0;
end;

(* ************************************************************************
  Internal subroutine: adding new summator layer to network
  ************************************************************************ *)
procedure AddBiasedSummatorLayer(NCount: TLInt;
  var LSizes: TLIVec; var LTypes: TLIVec;
  var LConnFirst: TLIVec; var LConnLast: TLIVec;
  var LastProc: TLInt);
begin
  LSizes[LastProc + 1] := 1;
  LTypes[LastProc + 1] := -3;
  LConnFirst[LastProc + 1] := 0;
  LConnLast[LastProc + 1] := 0;
  LSizes[LastProc + 2] := NCount;
  LTypes[LastProc + 2] := 0;
  LConnFirst[LastProc + 2] := LastProc;
  LConnLast[LastProc + 2] := LastProc + 1;
  LastProc := LastProc + 2;
end;

(* ************************************************************************
  Internal subroutine: adding new summator layer to network
  ************************************************************************ *)
procedure AddActivationLayer(FuncType: TLInt;
  var LSizes: TLIVec; var LTypes: TLIVec;
  var LConnFirst: TLIVec; var LConnLast: TLIVec;
  var LastProc: TLInt);
begin
  Assert(FuncType > 0, 'AddActivationLayer: incorrect function type');
  LSizes[LastProc + 1] := LSizes[LastProc];
  LTypes[LastProc + 1] := FuncType;
  LConnFirst[LastProc + 1] := LastProc;
  LConnLast[LastProc + 1] := LastProc;
  LastProc := LastProc + 1;
end;

(* ************************************************************************
  Internal subroutine: adding new zero layer to network
  ************************************************************************ *)
procedure AddZeroLayer(var LSizes: TLIVec; var LTypes: TLIVec;
  var LConnFirst: TLIVec; var LConnLast: TLIVec;
  var LastProc: TLInt);
begin
  LSizes[LastProc + 1] := 1;
  LTypes[LastProc + 1] := -4;
  LConnFirst[LastProc + 1] := 0;
  LConnLast[LastProc + 1] := 0;
  LastProc := LastProc + 1;
end;

(* ************************************************************************
  Internal subroutine.
  ************************************************************************ *)
procedure MLPCreate(NIn, NOut: TLInt;
  const LSizes: TLIVec; const LTypes: TLIVec;
  const LConnFirst: TLIVec; const LConnLast: TLIVec;
  LayersCount: TLInt; IsClsNet: Boolean;
  var Network: TMultiLayerPerceptron);
var
  i: TLInt;
  j: TLInt;
  SSize: TLInt;
  NTotal: TLInt;
  WCount: TLInt;
  Offs: TLInt;
  NProcessed: TLInt;
  WAllocated: TLInt;
  LocalTemp: TLIVec;
  LNFirst: TLIVec;
  LNSyn: TLIVec;
begin

  //
  // Check
  //
  Assert(LayersCount > 0, 'MLPCreate: wrong parameters!');
  Assert(LTypes[0] = -2, 'MLPCreate: wrong LTypes[0] (must be -2)!');
  i := 0;
  while i <= LayersCount - 1 do
    begin
      Assert(LSizes[i] > 0, 'MLPCreate: wrong LSizes!');
      Assert((LConnFirst[i] >= 0) and ((LConnFirst[i] < i) or (i = 0)), 'MLPCreate: wrong LConnFirst!');
      Assert((LConnLast[i] >= LConnFirst[i]) and ((LConnLast[i] < i) or (i = 0)), 'MLPCreate: wrong LConnLast!');
      inc(i);
    end;

  //
  // Build network geometry
  //
  SetLength(LNFirst, LayersCount  );
  SetLength(LNSyn, LayersCount  );
  NTotal := 0;
  WCount := 0;
  i := 0;
  while i <= LayersCount - 1 do
    begin

      //
      // Analyze connections.
      // This code must throw an assertion in case of unknown LTypes[I]
      //
      LNSyn[i] := -1;
      if LTypes[i] >= 0 then
        begin
          LNSyn[i] := 0;
          j := LConnFirst[i];
          while j <= LConnLast[i] do
            begin
              LNSyn[i] := LNSyn[i] + LSizes[j];
              inc(j);
            end;
        end
      else
        begin
          if (LTypes[i] = -2) or (LTypes[i] = -3) or (LTypes[i] = -4) then
            begin
              LNSyn[i] := 0;
            end;
        end;
      Assert(LNSyn[i] >= 0, 'MLPCreate: internal error #0!');

      //
      // Other info
      //
      LNFirst[i] := NTotal;
      NTotal := NTotal + LSizes[i];
      if LTypes[i] = 0 then
        begin
          WCount := WCount + LNSyn[i] * LSizes[i];
        end;
      inc(i);
    end;
  SSize := 7 + NTotal * NFieldWidth;

  //
  // Allocate
  //
  SetLength(Network.StructInfo, SSize  );
  SetLength(Network.Weights, WCount  );
  if IsClsNet then
    begin
      SetLength(Network.ColumnMeans, NIn  );
      SetLength(Network.ColumnSigmas, NIn  );
    end
  else
    begin
      SetLength(Network.ColumnMeans, NIn + NOut  );
      SetLength(Network.ColumnSigmas, NIn + NOut  );
    end;
  SetLength(Network.Neurons, NTotal  );
  SetLength(Network.Chunks, 3 * NTotal + 1, ChunkSize  );
  SetLength(Network.NWBuf, Max(WCount, 2 * NOut)  );
  SetLength(Network.DFDNET, NTotal  );
  SetLength(Network.x, NIn  );
  SetLength(Network.y, NOut  );
  SetLength(Network.DError, NTotal  );

  //
  // Fill structure: global info
  //
  Network.StructInfo[0] := SSize;
  Network.StructInfo[1] := NIn;
  Network.StructInfo[2] := NOut;
  Network.StructInfo[3] := NTotal;
  Network.StructInfo[4] := WCount;
  Network.StructInfo[5] := 7;
  if IsClsNet then
    begin
      Network.StructInfo[6] := 1;
    end
  else
    begin
      Network.StructInfo[6] := 0;
    end;

  //
  // Fill structure: neuron connections
  //
  NProcessed := 0;
  WAllocated := 0;
  i := 0;
  while i <= LayersCount - 1 do
    begin
      j := 0;
      while j <= LSizes[i] - 1 do
        begin
          Offs := Network.StructInfo[5] + NProcessed * NFieldWidth;
          Network.StructInfo[Offs + 0] := LTypes[i];
          if LTypes[i] = 0 then
            begin

              //
              // Adaptive summator:
              // * connections with weights to previous neurons
              //
              Network.StructInfo[Offs + 1] := LNSyn[i];
              Network.StructInfo[Offs + 2] := LNFirst[LConnFirst[i]];
              Network.StructInfo[Offs + 3] := WAllocated;
              WAllocated := WAllocated + LNSyn[i];
              NProcessed := NProcessed + 1;
            end;
          if LTypes[i] > 0 then
            begin

              //
              // Activation layer:
              // * each neuron connected to one (only one) of previous neurons.
              // * no weights
              //
              Network.StructInfo[Offs + 1] := 1;
              Network.StructInfo[Offs + 2] := LNFirst[LConnFirst[i]] + j;
              Network.StructInfo[Offs + 3] := -1;
              NProcessed := NProcessed + 1;
            end;
          if (LTypes[i] = -2) or (LTypes[i] = -3) or (LTypes[i] = -4) then
            begin
              NProcessed := NProcessed + 1;
            end;
          inc(j);
        end;
      inc(i);
    end;
  Assert(WAllocated = WCount, 'MLPCreate: internal error #1!');
  Assert(NProcessed = NTotal, 'MLPCreate: internal error #2!');

  //
  // Fill weights by small random values
  // Initialize means and sigmas
  //
  i := 0;
  while i <= WCount - 1 do
    begin
      Network.Weights[i] := RandomReal - 0.5;
      inc(i);
    end;
  i := 0;
  while i <= NIn - 1 do
    begin
      Network.ColumnMeans[i] := 0;
      Network.ColumnSigmas[i] := 1;
      inc(i);
    end;
  if not IsClsNet then
    begin
      i := 0;
      while i <= NOut - 1 do
        begin
          Network.ColumnMeans[NIn + i] := 0;
          Network.ColumnSigmas[NIn + i] := 1;
          inc(i);
        end;
    end;
end;

(* ************************************************************************
  Internal subroutine
  ************************************************************************ *)
procedure MLPActivationFunction(NET: TLFloat; k: TLInt; var f: TLFloat; var df: TLFloat; var D2F: TLFloat);
var
  NET2: TLFloat;
  ARG: TLFloat;
  ROOT: TLFloat;
  r: TLFloat;
begin
  f := 0;
  df := 0;
  if k = 1 then
    begin

      //
      // TanH activation function
      //
      if AP_FP_Less(AbsReal(NET), 100) then
        begin
          f := TanH(NET);
        end
      else
        begin
          f := Sign(NET);
        end;
      df := 1 - AP_Sqr(f);
      D2F := -2 * f * df;
      Exit;
    end;
  if k = 3 then
    begin

      //
      // EX activation function
      //
      if AP_FP_Greater_Eq(NET, 0) then
        begin
          NET2 := NET * NET;
          ARG := NET2 + 1;
          ROOT := Sqrt(ARG);
          f := NET + ROOT;
          r := NET / ROOT;
          df := 1 + r;
          D2F := (ROOT - NET * r) / ARG;
        end
      else
        begin
          f := Exp(NET);
          df := f;
          D2F := f;
        end;
      Exit;
    end;
  if k = 2 then
    begin
      f := Exp(-AP_Sqr(NET));
      df := -2 * NET * f;
      D2F := -2 * (f + df * NET);
      Exit;
    end;
end;

(* ************************************************************************
  Internal subroutine for Hessian calculation.

  WARNING!!! Unspeakable math far beyong human capabilities :)
  ************************************************************************ *)
procedure MLPHessianBatchInternal(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; SSize: TLInt; NaturalErr: Boolean;
  var E: TLFloat; var Grad: TLVec; var h: TLMatrix);
var
  NIn: TLInt;
  NOut: TLInt;
  WCount: TLInt;
  NTotal: TLInt;
  IStart: TLInt;
  i: TLInt;
  j: TLInt;
  k: TLInt;
  KL: TLInt;
  Offs: TLInt;
  n1: TLInt;
  n2: TLInt;
  W1: TLInt;
  W2: TLInt;
  s: TLFloat;
  t: TLFloat;
  v: TLFloat;
  ET: TLFloat;
  BFlag: Boolean;
  f: TLFloat;
  df: TLFloat;
  D2F: TLFloat;
  dEIdYJ: TLFloat;
  mx: TLFloat;
  q: TLFloat;
  z: TLFloat;
  s2: TLFloat;
  ExpI: TLFloat;
  ExpJ: TLFloat;
  x: TLVec;
  DesiredY: TLVec;
  GT: TLVec;
  Zeros: TLVec;
  RX: TLMatrix;
  RY: TLMatrix;
  rdx: TLMatrix;
  RDY: TLMatrix;
begin
  MLPProperties(Network, NIn, NOut, WCount);
  NTotal := Network.StructInfo[3];
  IStart := Network.StructInfo[5];

  //
  // Prepare
  //
  SetLength(x, NIn  );
  SetLength(DesiredY, NOut  );
  SetLength(Zeros, WCount  );
  SetLength(GT, WCount  );
  SetLength(RX, NTotal + NOut  , WCount  );
  SetLength(RY, NTotal + NOut  , WCount  );
  SetLength(rdx, NTotal + NOut  , WCount  );
  SetLength(RDY, NTotal + NOut  , WCount  );
  E := 0;
  i := 0;
  while i <= WCount - 1 do
    begin
      Zeros[i] := 0;
      inc(i);
    end;
  APVMove(@Grad[0], 0, WCount - 1, @Zeros[0], 0, WCount - 1);
  i := 0;
  while i <= WCount - 1 do
    begin
      APVMove(@h[i][0], 0, WCount - 1, @Zeros[0], 0, WCount - 1);
      inc(i);
    end;

  //
  // Process
  //
  k := 0;
  while k <= SSize - 1 do
    begin

      //
      // Process vector with MLPGradN.
      // Now Neurons, DFDNET and DError contains results of the last run.
      //
      APVMove(@x[0], 0, NIn - 1, @xy[k][0], 0, NIn - 1);
      if MLPIsSoftmax(Network) then
        begin

          //
          // class labels outputs
          //
          KL := Round(xy[k, NIn]);
          i := 0;
          while i <= NOut - 1 do
            begin
              if i = KL then
                begin
                  DesiredY[i] := 1;
                end
              else
                begin
                  DesiredY[i] := 0;
                end;
              inc(i);
            end;
        end
      else
        begin

          //
          // real outputs
          //
          APVMove(@DesiredY[0], 0, NOut - 1, @xy[k][0], NIn, NIn + NOut - 1);
        end;
      if NaturalErr then
        begin
          MLPGradN(Network, x, DesiredY, ET, GT);
        end
      else
        begin
          MLPGrad(Network, x, DesiredY, ET, GT);
        end;

      //
      // grad, error
      //
      E := E + ET;
      APVAdd(@Grad[0], 0, WCount - 1, @GT[0], 0, WCount - 1);

      //
      // Hessian.
      // Forward pass of the R-algorithm
      //
      i := 0;
      while i <= NTotal - 1 do
        begin
          Offs := IStart + i * NFieldWidth;
          APVMove(@RX[i][0], 0, WCount - 1, @Zeros[0], 0, WCount - 1);
          APVMove(@RY[i][0], 0, WCount - 1, @Zeros[0], 0, WCount - 1);
          if Network.StructInfo[Offs + 0] > 0 then
            begin

              //
              // Activation function
              //
              n1 := Network.StructInfo[Offs + 2];
              APVMove(@RX[i][0], 0, WCount - 1, @RY[n1][0], 0, WCount - 1);
              v := Network.DFDNET[i];
              APVMove(@RY[i][0], 0, WCount - 1, @RX[i][0], 0, WCount - 1, v);
            end;
          if Network.StructInfo[Offs + 0] = 0 then
            begin

              //
              // Adaptive summator
              //
              n1 := Network.StructInfo[Offs + 2];
              n2 := n1 + Network.StructInfo[Offs + 1] - 1;
              W1 := Network.StructInfo[Offs + 3];
              W2 := W1 + Network.StructInfo[Offs + 1] - 1;
              j := n1;
              while j <= n2 do
                begin
                  v := Network.Weights[W1 + j - n1];
                  APVAdd(@RX[i][0], 0, WCount - 1, @RY[j][0], 0, WCount - 1, v);
                  RX[i, W1 + j - n1] := RX[i, W1 + j - n1] + Network.Neurons[j];
                  inc(j);
                end;
              APVMove(@RY[i][0], 0, WCount - 1, @RX[i][0], 0, WCount - 1);
            end;
          if Network.StructInfo[Offs + 0] < 0 then
            begin
              BFlag := True;
              if Network.StructInfo[Offs + 0] = -2 then
                begin

                  //
                  // input neuron, left unchanged
                  //
                  BFlag := False;
                end;
              if Network.StructInfo[Offs + 0] = -3 then
                begin

                  //
                  // "-1" neuron, left unchanged
                  //
                  BFlag := False;
                end;
              if Network.StructInfo[Offs + 0] = -4 then
                begin

                  //
                  // "0" neuron, left unchanged
                  //
                  BFlag := False;
                end;
              Assert(not BFlag, 'MLPHessianNBatch: internal error - unknown neuron type!');
            end;
          inc(i);
        end;

      //
      // Hessian. Backward pass of the R-algorithm.
      //
      // Stage 1. Initialize RDY
      //
      i := 0;
      while i <= NTotal + NOut - 1 do
        begin
          APVMove(@RDY[i][0], 0, WCount - 1, @Zeros[0], 0, WCount - 1);
          inc(i);
        end;
      if Network.StructInfo[6] = 0 then
        begin

          //
          // Standardisation.
          //
          // In context of the Hessian calculation standardisation
          // is considered as additional layer with weightless
          // activation function:
          //
          // F(NET) := Sigma*NET
          //
          // So we add one more layer to forward pass, and
          // make forward/backward pass through this layer.
          //
          i := 0;
          while i <= NOut - 1 do
            begin
              n1 := NTotal - NOut + i;
              n2 := NTotal + i;

              //
              // Forward pass from N1 to N2
              //
              APVMove(@RX[n2][0], 0, WCount - 1, @RY[n1][0], 0, WCount - 1);
              v := Network.ColumnSigmas[NIn + i];
              APVMove(@RY[n2][0], 0, WCount - 1, @RX[n2][0], 0, WCount - 1, v);

              //
              // Initialization of RDY
              //
              APVMove(@RDY[n2][0], 0, WCount - 1, @RY[n2][0], 0, WCount - 1);

              //
              // Backward pass from N2 to N1:
              // 1. Calculate R(dE/dX).
              // 2. No R(dE/dWij) is needed since weight of activation neuron
              // is fixed to 1. So we can update R(dE/dY) for
              // the connected neuron (note that Vij=0, Wij=1)
              //
              df := Network.ColumnSigmas[NIn + i];
              APVMove(@rdx[n2][0], 0, WCount - 1, @RDY[n2][0], 0, WCount - 1, df);
              APVAdd(@RDY[n1][0], 0, WCount - 1, @rdx[n2][0], 0, WCount - 1);
              inc(i);
            end;
        end
      else
        begin

          //
          // Softmax.
          //
          // Initialize RDY using generalized expression for ei'(yi)
          // (see expression (9) from p. 5 of "Fast Exact Multiplication by the Hessian").
          //
          // When we are working with softmax network, generalized
          // expression for ei'(yi) is used because softmax
          // normalization leads to ei, which depends on all y's
          //
          if NaturalErr then
            begin

              //
              // softmax + cross-entropy.
              // We have:
              //
              // S = sum(exp(yk)),
              // ei = sum(trn)*exp(yi)/S-trn_i
              //
              // j=i:   d(ei)/d(yj) = T*exp(yi)*(S-exp(yi))/S^2
              // j<>i:  d(ei)/d(yj) = -T*exp(yi)*exp(yj)/S^2
              //
              t := 0;
              i := 0;
              while i <= NOut - 1 do
                begin
                  t := t + DesiredY[i];
                  inc(i);
                end;
              mx := Network.Neurons[NTotal - NOut];
              i := 0;
              while i <= NOut - 1 do
                begin
                  mx := Max(mx, Network.Neurons[NTotal - NOut + i]);
                  inc(i);
                end;
              s := 0;
              i := 0;
              while i <= NOut - 1 do
                begin
                  Network.NWBuf[i] := Exp(Network.Neurons[NTotal - NOut + i] - mx);
                  s := s + Network.NWBuf[i];
                  inc(i);
                end;
              i := 0;
              while i <= NOut - 1 do
                begin
                  j := 0;
                  while j <= NOut - 1 do
                    begin
                      if j = i then
                        begin
                          dEIdYJ := t * Network.NWBuf[i] * (s - Network.NWBuf[i]) / AP_Sqr(s);
                          APVAdd(@RDY[NTotal - NOut + i][0], 0, WCount - 1, @RY[NTotal - NOut + i][0], 0, WCount - 1, dEIdYJ);
                        end
                      else
                        begin
                          dEIdYJ := -t * Network.NWBuf[i] * Network.NWBuf[j] / AP_Sqr(s);
                          APVAdd(@RDY[NTotal - NOut + i][0], 0, WCount - 1, @RY[NTotal - NOut + j][0], 0, WCount - 1, dEIdYJ);
                        end;
                      inc(j);
                    end;
                  inc(i);
                end;
            end
          else
            begin

              //
              // For a softmax + squared error we have expression
              // far beyond human imagination so we dont even try
              // to comment on it. Just enjoy the code...
              //
              // P.S. That's why "natural error" is called "natural" -
              // compact beatiful expressions, fast code....
              //
              mx := Network.Neurons[NTotal - NOut];
              i := 0;
              while i <= NOut - 1 do
                begin
                  mx := Max(mx, Network.Neurons[NTotal - NOut + i]);
                  inc(i);
                end;
              s := 0;
              s2 := 0;
              i := 0;
              while i <= NOut - 1 do
                begin
                  Network.NWBuf[i] := Exp(Network.Neurons[NTotal - NOut + i] - mx);
                  s := s + Network.NWBuf[i];
                  s2 := s2 + AP_Sqr(Network.NWBuf[i]);
                  inc(i);
                end;
              q := 0;
              i := 0;
              while i <= NOut - 1 do
                begin
                  q := q + (Network.y[i] - DesiredY[i]) * Network.NWBuf[i];
                  inc(i);
                end;
              i := 0;
              while i <= NOut - 1 do
                begin
                  z := -q + (Network.y[i] - DesiredY[i]) * s;
                  ExpI := Network.NWBuf[i];
                  j := 0;
                  while j <= NOut - 1 do
                    begin
                      ExpJ := Network.NWBuf[j];
                      if j = i then
                        begin
                          dEIdYJ := ExpI / AP_Sqr(s) * ((z + ExpI) * (s - 2 * ExpI) / s + ExpI * s2 / AP_Sqr(s));
                        end
                      else
                        begin
                          dEIdYJ := ExpI * ExpJ / AP_Sqr(s) *
                            (s2 / AP_Sqr(s) - 2 * z / s - (ExpI + ExpJ) / s +
                            (Network.y[i] - DesiredY[i]) - (Network.y[j] - DesiredY[j]));
                        end;
                      APVAdd(@RDY[NTotal - NOut + i][0], 0, WCount - 1, @RY[NTotal - NOut + j][0], 0, WCount - 1, dEIdYJ);
                      inc(j);
                    end;
                  inc(i);
                end;
            end;
        end;

      //
      // Hessian. Backward pass of the R-algorithm
      //
      // Stage 2. Process.
      //
      i := NTotal - 1;
      while i >= 0 do
        begin

          //
          // Possible variants:
          // 1. Activation function
          // 2. Adaptive summator
          // 3. Special neuron
          //
          Offs := IStart + i * NFieldWidth;
          if Network.StructInfo[Offs + 0] > 0 then
            begin
              n1 := Network.StructInfo[Offs + 2];

              //
              // First, calculate R(dE/dX).
              //
              MLPActivationFunction(Network.Neurons[n1], Network.StructInfo[Offs + 0], f, df, D2F);
              v := D2F * Network.DError[i];
              APVMove(@rdx[i][0], 0, WCount - 1, @RDY[i][0], 0, WCount - 1, df);
              APVAdd(@rdx[i][0], 0, WCount - 1, @RX[i][0], 0, WCount - 1, v);

              //
              // No R(dE/dWij) is needed since weight of activation neuron
              // is fixed to 1.
              //
              // So we can update R(dE/dY) for the connected neuron.
              // (note that Vij=0, Wij=1)
              //
              APVAdd(@RDY[n1][0], 0, WCount - 1, @rdx[i][0], 0, WCount - 1);
            end;
          if Network.StructInfo[Offs + 0] = 0 then
            begin

              //
              // Adaptive summator
              //
              n1 := Network.StructInfo[Offs + 2];
              n2 := n1 + Network.StructInfo[Offs + 1] - 1;
              W1 := Network.StructInfo[Offs + 3];
              W2 := W1 + Network.StructInfo[Offs + 1] - 1;

              //
              // First, calculate R(dE/dX).
              //
              APVMove(@rdx[i][0], 0, WCount - 1, @RDY[i][0], 0, WCount - 1);

              //
              // Then, calculate R(dE/dWij)
              //
              j := W1;
              while j <= W2 do
                begin
                  v := Network.Neurons[n1 + j - W1];
                  APVAdd(@h[j][0], 0, WCount - 1, @rdx[i][0], 0, WCount - 1, v);
                  v := Network.DError[i];
                  APVAdd(@h[j][0], 0, WCount - 1, @RY[n1 + j - W1][0], 0, WCount - 1, v);
                  inc(j);
                end;

              //
              // And finally, update R(dE/dY) for connected neurons.
              //
              j := W1;
              while j <= W2 do
                begin
                  v := Network.Weights[j];
                  APVAdd(@RDY[n1 + j - W1][0], 0, WCount - 1, @rdx[i][0], 0, WCount - 1, v);
                  RDY[n1 + j - W1, j] := RDY[n1 + j - W1, j] + Network.DError[i];
                  inc(j);
                end;
            end;
          if Network.StructInfo[Offs + 0] < 0 then
            begin
              BFlag := False;
              if (Network.StructInfo[Offs + 0] = -2) or
                (Network.StructInfo[Offs + 0] = -3) or
                (Network.StructInfo[Offs + 0] = -4) then
                begin

                  //
                  // Special neuron type, no back-propagation required
                  //
                  BFlag := True;
                end;
              Assert(BFlag, 'MLPHessianNBatch: unknown neuron type!');
            end;
          dec(i);
        end;
      inc(k);
    end;
end;

(* ************************************************************************
  Internal subroutine

  Network must be processed by MLPProcess on X
  ************************************************************************ *)
procedure MLPInternalCalculateGradient(var Network: TMultiLayerPerceptron;
  const Neurons: TLVec; const Weights: TLVec;
  var DError: TLVec; var Grad: TLVec; NaturalErrorFunc: Boolean);
var
  i: TLInt;
  n1: TLInt;
  n2: TLInt;
  W1: TLInt;
  W2: TLInt;
  NTotal: TLInt;
  IStart: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  Offs: TLInt;
  dEdF: TLFloat;
  DFDNET: TLFloat;
  v: TLFloat;
  FOwn: TLFloat;
  DEOwn: TLFloat;
  NET: TLFloat;
  mx: TLFloat;
  BFlag: Boolean;
begin

  //
  // Read network geometry
  //
  NIn := Network.StructInfo[1];
  NOut := Network.StructInfo[2];
  NTotal := Network.StructInfo[3];
  IStart := Network.StructInfo[5];

  //
  // Pre-processing of dError/dOut:
  // from dError/dOut(normalized) to dError/dOut(non-normalized)
  //
  Assert((Network.StructInfo[6] = 0) or (Network.StructInfo[6] = 1), 'MLPInternalCalculateGradient: unknown normalization type!');

  if Network.StructInfo[6] = 1 then
    begin

      //
      // Softmax
      //
      if not NaturalErrorFunc then
        begin
          mx := Network.Neurons[NTotal - NOut];
          i := 0;
          while i <= NOut - 1 do
            begin
              mx := Max(mx, Network.Neurons[NTotal - NOut + i]);
              inc(i);
            end;
          NET := 0;
          i := 0;
          while i <= NOut - 1 do
            begin
              Network.NWBuf[i] := Exp(Network.Neurons[NTotal - NOut + i] - mx);
              NET := NET + Network.NWBuf[i];
              inc(i);
            end;
          v := APVDotProduct(@Network.DError[0], NTotal - NOut, NTotal - 1,
            @Network.NWBuf[0], 0, NOut - 1);
          i := 0;
          while i <= NOut - 1 do
            begin
              FOwn := Network.NWBuf[i];
              DEOwn := Network.DError[NTotal - NOut + i];
              Network.NWBuf[NOut + i] := (-v + DEOwn * FOwn + DEOwn * (NET - FOwn)) * FOwn / AP_Sqr(NET);
              inc(i);
            end;
          i := 0;
          while i <= NOut - 1 do
            begin
              Network.DError[NTotal - NOut + i] := Network.NWBuf[NOut + i];
              inc(i);
            end;
        end;
    end
  else
    begin

      //
      // Un-standardisation
      //
      i := 0;
      while i <= NOut - 1 do
        begin
          Network.DError[NTotal - NOut + i] := Network.DError[NTotal - NOut + i] * Network.ColumnSigmas[NIn + i];
          inc(i);
        end;
    end;

  //
  // Backpropagation
  //
  i := NTotal - 1;
  while i >= 0 do
    begin

      //
      // Extract info
      //
      Offs := IStart + i * NFieldWidth;
      if Network.StructInfo[Offs + 0] > 0 then
        begin

          //
          // Activation function
          //
          dEdF := Network.DError[i];
          DFDNET := Network.DFDNET[i];
          DError[Network.StructInfo[Offs + 2]] := DError[Network.StructInfo[Offs + 2]] + dEdF * DFDNET;
        end;
      if Network.StructInfo[Offs + 0] = 0 then
        begin

          //
          // Adaptive summator
          //
          n1 := Network.StructInfo[Offs + 2];
          n2 := n1 + Network.StructInfo[Offs + 1] - 1;
          W1 := Network.StructInfo[Offs + 3];
          W2 := W1 + Network.StructInfo[Offs + 1] - 1;
          dEdF := Network.DError[i];
          DFDNET := 1.0;
          v := dEdF * DFDNET;
          APVMove(@Grad[0], W1, W2, @Neurons[0], n1, n2, v);
          APVAdd(@DError[0], n1, n2, @Weights[0], W1, W2, v);
        end;
      if Network.StructInfo[Offs + 0] < 0 then
        begin
          BFlag := False;
          if (Network.StructInfo[Offs + 0] = -2) or
            (Network.StructInfo[Offs + 0] = -3) or
            (Network.StructInfo[Offs + 0] = -4) then
            begin

              //
              // Special neuron type, no back-propagation required
              //
              BFlag := True;
            end;
          Assert(BFlag, 'MLPInternalCalculateGradient: unknown neuron type!');
        end;
      dec(i);
    end;
end;

(* ************************************************************************
  Internal subroutine, chunked gradient
  ************************************************************************ *)
procedure MLPChunkedGradient(var Network: TMultiLayerPerceptron;
  const xy: TLMatrix; CStart: TLInt; CSize: TLInt;
  var E: TLFloat; var Grad: TLVec; NaturalErrorFunc: Boolean);
var
  i: TLInt;
  j: TLInt;
  k: TLInt;
  KL: TLInt;
  n1: TLInt;
  n2: TLInt;
  W1: TLInt;
  W2: TLInt;
  c1: TLInt;
  c2: TLInt;
  NTotal: TLInt;
  NIn: TLInt;
  NOut: TLInt;
  Offs: TLInt;
  f: TLFloat;
  df: TLFloat;
  D2F: TLFloat;
  v: TLFloat;
  s: TLFloat;
  FOwn: TLFloat;
  DEOwn: TLFloat;
  NET: TLFloat;
  LnNET: TLFloat;
  mx: TLFloat;
  BFlag: Boolean;
  IStart: TLInt;
  INeurons: TLInt;
  IDFDNET: TLInt;
  IDError: TLInt;
  IZeros: TLInt;
begin

  //
  // Read network geometry, prepare data
  //
  NIn := Network.StructInfo[1];
  NOut := Network.StructInfo[2];
  NTotal := Network.StructInfo[3];
  IStart := Network.StructInfo[5];
  c1 := CStart;
  c2 := CStart + CSize - 1;
  INeurons := 0;
  IDFDNET := NTotal;
  IDError := 2 * NTotal;
  IZeros := 3 * NTotal;
  j := 0;
  while j <= CSize - 1 do
    begin
      Network.Chunks[IZeros, j] := 0;
      inc(j);
    end;

  //
  // Forward pass:
  // 1. Load inputs from XY to Chunks[0:NIn-1,0:CSize-1]
  // 2. Forward pass
  //
  i := 0;
  while i <= NIn - 1 do
    begin
      j := 0;
      while j <= CSize - 1 do
        begin
          if AP_FP_NEq(Network.ColumnSigmas[i], 0) then
            begin
              Network.Chunks[i, j] := (xy[c1 + j, i] - Network.ColumnMeans[i]) / Network.ColumnSigmas[i];
            end
          else
            begin
              Network.Chunks[i, j] := xy[c1 + j, i] - Network.ColumnMeans[i];
            end;
          inc(j);
        end;
      inc(i);
    end;
  i := 0;
  while i <= NTotal - 1 do
    begin
      Offs := IStart + i * NFieldWidth;
      if Network.StructInfo[Offs + 0] > 0 then
        begin

          //
          // Activation function:
          // * calculate F vector, F(i) = F(NET(i))
          //
          n1 := Network.StructInfo[Offs + 2];
          APVMove(@Network.Chunks[i][0], 0, CSize - 1, @Network.Chunks[n1][0], 0, CSize - 1);
          j := 0;
          while j <= CSize - 1 do
            begin
              MLPActivationFunction(Network.Chunks[i, j], Network.StructInfo[Offs + 0], f, df, D2F);
              Network.Chunks[i, j] := f;
              Network.Chunks[IDFDNET + i, j] := df;
              inc(j);
            end;
        end;
      if Network.StructInfo[Offs + 0] = 0 then
        begin

          //
          // Adaptive summator:
          // * calculate NET vector, NET(i) = SUM(W(j,i)*Neurons(j),j=N1..N2)
          //
          n1 := Network.StructInfo[Offs + 2];
          n2 := n1 + Network.StructInfo[Offs + 1] - 1;
          W1 := Network.StructInfo[Offs + 3];
          W2 := W1 + Network.StructInfo[Offs + 1] - 1;
          APVMove(@Network.Chunks[i][0], 0, CSize - 1, @Network.Chunks[IZeros][0], 0, CSize - 1);
          j := n1;
          while j <= n2 do
            begin
              v := Network.Weights[W1 + j - n1];
              APVAdd(@Network.Chunks[i][0], 0, CSize - 1, @Network.Chunks[j][0], 0, CSize - 1, v);
              inc(j);
            end;
        end;
      if Network.StructInfo[Offs + 0] < 0 then
        begin
          BFlag := False;
          if Network.StructInfo[Offs + 0] = -2 then
            begin

              //
              // input neuron, left unchanged
              //
              BFlag := True;
            end;
          if Network.StructInfo[Offs + 0] = -3 then
            begin

              //
              // "-1" neuron
              //
              k := 0;
              while k <= CSize - 1 do
                begin
                  Network.Chunks[i, k] := -1;
                  inc(k);
                end;
              BFlag := True;
            end;
          if Network.StructInfo[Offs + 0] = -4 then
            begin

              //
              // "0" neuron
              //
              k := 0;
              while k <= CSize - 1 do
                begin
                  Network.Chunks[i, k] := 0;
                  inc(k);
                end;
              BFlag := True;
            end;
          Assert(BFlag, 'MLPChunkedGradient: internal error - unknown neuron type!');
        end;
      inc(i);
    end;

  //
  // Post-processing, error, dError/dOut
  //
  i := 0;
  while i <= NTotal - 1 do
    begin
      APVMove(@Network.Chunks[IDError + i][0], 0, CSize - 1, @Network.Chunks[IZeros][0], 0, CSize - 1);
      inc(i);
    end;
  Assert((Network.StructInfo[6] = 0) or (Network.StructInfo[6] = 1), 'MLPChunkedGradient: unknown normalization type!');

  if Network.StructInfo[6] = 1 then
    begin

      //
      // Softmax output, classification network.
      //
      // For each K = 0..CSize-1 do:
      // 1. place exp(outputs[k]) to NWBuf[0:NOut-1]
      // 2. place sum(exp(..)) to NET
      // 3. calculate dError/dOut and place it to the second block of Chunks
      //
      k := 0;
      while k <= CSize - 1 do
        begin

          //
          // Normalize
          //
          mx := Network.Chunks[NTotal - NOut, k];
          i := 1;
          while i <= NOut - 1 do
            begin
              mx := Max(mx, Network.Chunks[NTotal - NOut + i, k]);
              inc(i);
            end;
          NET := 0;
          i := 0;
          while i <= NOut - 1 do
            begin
              Network.NWBuf[i] := Exp(Network.Chunks[NTotal - NOut + i, k] - mx);
              NET := NET + Network.NWBuf[i];
              inc(i);
            end;

          //
          // Calculate error function and dError/dOut
          //
          if NaturalErrorFunc then
            begin

              //
              // Natural error func.
              //
              //
              s := 1;
              LnNET := ln(NET);
              KL := Round(xy[CStart + k, NIn]);
              i := 0;
              while i <= NOut - 1 do
                begin
                  if i = KL then
                    begin
                      v := 1;
                    end
                  else
                    begin
                      v := 0;
                    end;
                  Network.Chunks[IDError + NTotal - NOut + i, k] := s * Network.NWBuf[i] / NET - v;
                  E := E + SafeCrossEntropy(v, Network.NWBuf[i] / NET);
                  inc(i);
                end;
            end
          else
            begin

              //
              // Least squares error func
              // Error, dError/dOut(normalized)
              //
              KL := Round(xy[CStart + k, NIn]);
              i := 0;
              while i <= NOut - 1 do
                begin
                  if i = KL then
                    begin
                      v := Network.NWBuf[i] / NET - 1;
                    end
                  else
                    begin
                      v := Network.NWBuf[i] / NET;
                    end;
                  Network.NWBuf[NOut + i] := v;
                  E := E + AP_Sqr(v) / 2;
                  inc(i);
                end;

              //
              // From dError/dOut(normalized) to dError/dOut(non-normalized)
              //
              v := APVDotProduct(@Network.NWBuf[0], NOut, 2 * NOut - 1, @Network.NWBuf[0], 0, NOut - 1);
              i := 0;
              while i <= NOut - 1 do
                begin
                  FOwn := Network.NWBuf[i];
                  DEOwn := Network.NWBuf[NOut + i];
                  Network.Chunks[IDError + NTotal - NOut + i, k] := (-v + DEOwn * FOwn + DEOwn * (NET - FOwn)) * FOwn / AP_Sqr(NET);
                  inc(i);
                end;
            end;
          inc(k);
        end;
    end
  else
    begin

      //
      // Normal output, regression network
      //
      // For each K = 0..CSize-1 do:
      // 1. calculate dError/dOut and place it to the second block of Chunks
      //
      i := 0;
      while i <= NOut - 1 do
        begin
          j := 0;
          while j <= CSize - 1 do
            begin
              v := Network.Chunks[NTotal - NOut + i, j] * Network.ColumnSigmas[NIn + i] + Network.ColumnMeans[NIn + i] - xy[CStart + j, NIn + i];
              Network.Chunks[IDError + NTotal - NOut + i, j] := v * Network.ColumnSigmas[NIn + i];
              E := E + AP_Sqr(v) / 2;
              inc(j);
            end;
          inc(i);
        end;
    end;

  //
  // Backpropagation
  //
  i := NTotal - 1;
  while i >= 0 do
    begin

      //
      // Extract info
      //
      Offs := IStart + i * NFieldWidth;
      if Network.StructInfo[Offs + 0] > 0 then
        begin

          //
          // Activation function
          //
          n1 := Network.StructInfo[Offs + 2];
          k := 0;
          while k <= CSize - 1 do
            begin
              Network.Chunks[IDError + i, k] := Network.Chunks[IDError + i, k] * Network.Chunks[IDFDNET + i, k];
              inc(k);
            end;
          APVAdd(@Network.Chunks[IDError + n1][0], 0, CSize - 1, @Network.Chunks[IDError + i][0], 0, CSize - 1);
        end;
      if Network.StructInfo[Offs + 0] = 0 then
        begin

          //
          // "Normal" activation function
          //
          n1 := Network.StructInfo[Offs + 2];
          n2 := n1 + Network.StructInfo[Offs + 1] - 1;
          W1 := Network.StructInfo[Offs + 3];
          W2 := W1 + Network.StructInfo[Offs + 1] - 1;
          j := W1;
          while j <= W2 do
            begin
              v := APVDotProduct(@Network.Chunks[n1 + j - W1][0], 0, CSize - 1, @Network.Chunks[IDError + i][0], 0, CSize - 1);
              Grad[j] := Grad[j] + v;
              inc(j);
            end;
          j := n1;
          while j <= n2 do
            begin
              v := Network.Weights[W1 + j - n1];
              APVAdd(@Network.Chunks[IDError + j][0], 0, CSize - 1, @Network.Chunks[IDError + i][0], 0, CSize - 1, v);
              inc(j);
            end;
        end;
      if Network.StructInfo[Offs + 0] < 0 then
        begin
          BFlag := False;
          if (Network.StructInfo[Offs + 0] = -2) or
            (Network.StructInfo[Offs + 0] = -3) or
            (Network.StructInfo[Offs + 0] = -4) then
            begin

              //
              // Special neuron type, no back-propagation required
              //
              BFlag := True;
            end;
          Assert(BFlag, 'MLPInternalCalculateGradient: unknown neuron type!');
        end;
      dec(i);
    end;
end;

(* ************************************************************************
  Returns T*Ln(T/Z), guarded against overflow/underflow.
  Internal subroutine.
  ************************************************************************ *)
function SafeCrossEntropy(t: TLFloat; z: TLFloat): TLFloat;
var
  r: TLFloat;
begin
  if AP_FP_Eq(t, 0) then
    begin
      Result := 0;
    end
  else
    begin
      if AP_FP_Greater(AbsReal(z), 1) then
        begin

          //
          // Shouldn't be the case with softmax,
          // but we just want to be sure.
          //
          if AP_FP_Eq(t / z, 0) then
            begin
              r := MinRealNumber;
            end
          else
            begin
              r := t / z;
            end;
        end
      else
        begin

          //
          // Normal case
          //
          if AP_FP_Eq(z, 0) or AP_FP_Greater_Eq(AbsReal(t), MaxRealNumber * AbsReal(z)) then
            begin
              r := MaxRealNumber;
            end
          else
            begin
              r := t / z;
            end;
        end;
      Result := t * ln(r);
    end;
end;
