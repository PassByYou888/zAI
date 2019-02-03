{ ****************************************************************************** }
{ * Neural networks ensemble support, by QQ 600585@qq.com                      * }
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
  (* ************************************************************************
    Neural networks ensemble
    ************************************************************************ *)
  TMLPEnsemble = record
    StructInfo: TLIVec;
    EnsembleSize: TLInt;
    NIn: TLInt;
    NOut: TLInt;
    WCount: TLInt;
    IsSoftmax: Boolean;
    PostProcessing: Boolean;
    Weights: TLVec;
    ColumnMeans: TLVec;
    ColumnSigmas: TLVec;
    SerializedLen: TLInt;
    SerializedMLP: TLVec;
    TmpWeights: TLVec;
    TmpMeans: TLVec;
    TmpSigmas: TLVec;
    Neurons: TLVec;
    DFDNET: TLVec;
    y: TLVec;
  end;

  PMLPEnsemble = ^TMLPEnsemble;

procedure MLPECreate0(NIn, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward;
procedure MLPECreate1(NIn, NHid, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward;
procedure MLPECreate2(NIn, NHid1, NHid2, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward;

procedure MLPECreateB0(NIn, NOut: TLInt; b, d: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward;
procedure MLPECreateB1(NIn, NHid, NOut: TLInt; b, d: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward;
procedure MLPECreateB2(NIn, NHid1, NHid2, NOut: TLInt; b, d: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward;

procedure MLPECreateR0(NIn, NOut: TLInt; a, b: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward;
procedure MLPECreateR1(NIn, NHid, NOut: TLInt; a, b: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward;
procedure MLPECreateR2(NIn, NHid1, NHid2, NOut: TLInt; a, b: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward;

procedure MLPECreateC0(NIn, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward;
procedure MLPECreateC1(NIn, NHid, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward;
procedure MLPECreateC2(NIn, NHid1, NHid2, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward;

procedure MLPECreateFromNetwork(const Network: TMultiLayerPerceptron; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble); forward;

procedure MLPECopy(const Ensemble1: TMLPEnsemble; var Ensemble2: TMLPEnsemble); forward;
procedure MLPESerialize(var Ensemble: TMLPEnsemble; var RA: TLVec; var RLen: TLInt); forward;
procedure MLPEUNSerialize(const RA: TLVec; var Ensemble: TMLPEnsemble); forward;

procedure MLPERandomize(var Ensemble: TMLPEnsemble); forward;

procedure MLPEProperties(const Ensemble: TMLPEnsemble; var NIn: TLInt; var NOut: TLInt); forward;

function MLPEIsSoftmax(const Ensemble: TMLPEnsemble): Boolean; forward;

procedure MLPEProcess(var Ensemble: TMLPEnsemble; const x: TLVec; var y: TLVec); forward;

function MLPERelClsError(var Ensemble: TMLPEnsemble; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function MLPEAvgCE(var Ensemble: TMLPEnsemble; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function MLPERMSError(var Ensemble: TMLPEnsemble; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function MLPEAvgError(var Ensemble: TMLPEnsemble; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function MLPEAvgRelError(var Ensemble: TMLPEnsemble; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;

procedure MLPEBaggingLM(const MultiThread: Boolean; var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  var Info: TLInt; var Rep: TMLPReport; var OOBErrors: TMLPCVReport); forward;

procedure MLPEBaggingLBFGS(const MultiThread: Boolean; var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  WStep: TLFloat; MAXITS: TLInt; var Info: TLInt;
  var Rep: TMLPReport; var OOBErrors: TMLPCVReport); forward;


const
  MLPNTotalOffset = 3;
  MLPEVNum        = 9;

procedure MLPEAllErrors(var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt; var RelCls: TLFloat; var AvgCE: TLFloat;
  var RMS: TLFloat; var Avg: TLFloat; var AvgRel: TLFloat); forward;
procedure MLPEBaggingInternal(const MultiThread: Boolean; var Ensemble: TMLPEnsemble;
  const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; WStep: TLFloat; MAXITS: TLInt;
  LMAlgorithm: Boolean; var Info: TLInt; var Rep: TMLPReport;
  var OOBErrors: TMLPCVReport); forward;


procedure DSErrAllocate(NClasses: TLInt; var Buf: TLVec);
begin
  SetLength(Buf, 7 + 1);
  Buf[0] := 0;
  Buf[1] := 0;
  Buf[2] := 0;
  Buf[3] := 0;
  Buf[4] := 0;
  Buf[5] := NClasses;
  Buf[6] := 0;
  Buf[7] := 0;
end;

procedure DSErrAccumulate(var Buf: TLVec; const y: TLVec; const DesiredY: TLVec);
var
  NClasses: TLInt;
  NOut    : TLInt;
  Offs    : TLInt;
  MMAX    : TLInt;
  rmax    : TLInt;
  j       : TLInt;
  v       : TLFloat;
  EV      : TLFloat;
begin
  Offs := 5;
  NClasses := Round(Buf[Offs]);
  if NClasses > 0 then
    begin

      //
      // Classification
      //
      rmax := Round(DesiredY[0]);
      MMAX := 0;
      j := 1;
      while j <= NClasses - 1 do
        begin
          if AP_FP_Greater(y[j], y[MMAX]) then
            begin
              MMAX := j;
            end;
          inc(j);
        end;
      if MMAX <> rmax then
        begin
          Buf[0] := Buf[0] + 1;
        end;
      if AP_FP_Greater(y[rmax], 0) then
        begin
          Buf[1] := Buf[1] - ln(y[rmax]);
        end
      else
        begin
          Buf[1] := Buf[1] + ln(MaxRealNumber);
        end;
      j := 0;
      while j <= NClasses - 1 do
        begin
          v := y[j];
          if j = rmax then
            begin
              EV := 1;
            end
          else
            begin
              EV := 0;
            end;
          Buf[2] := Buf[2] + AP_Sqr(v - EV);
          Buf[3] := Buf[3] + AbsReal(v - EV);
          if AP_FP_NEq(EV, 0) then
            begin
              Buf[4] := Buf[4] + AbsReal((v - EV) / EV);
              Buf[Offs + 2] := Buf[Offs + 2] + 1;
            end;
          inc(j);
        end;
      Buf[Offs + 1] := Buf[Offs + 1] + 1;
    end
  else
    begin

      //
      // Regression
      //
      NOut := -NClasses;
      rmax := 0;
      j := 1;
      while j <= NOut - 1 do
        begin
          if AP_FP_Greater(DesiredY[j], DesiredY[rmax]) then
            begin
              rmax := j;
            end;
          inc(j);
        end;
      MMAX := 0;
      j := 1;
      while j <= NOut - 1 do
        begin
          if AP_FP_Greater(y[j], y[MMAX]) then
            begin
              MMAX := j;
            end;
          inc(j);
        end;
      if MMAX <> rmax then
        begin
          Buf[0] := Buf[0] + 1;
        end;
      j := 0;
      while j <= NOut - 1 do
        begin
          v := y[j];
          EV := DesiredY[j];
          Buf[2] := Buf[2] + AP_Sqr(v - EV);
          Buf[3] := Buf[3] + AbsReal(v - EV);
          if AP_FP_NEq(EV, 0) then
            begin
              Buf[4] := Buf[4] + AbsReal((v - EV) / EV);
              Buf[Offs + 2] := Buf[Offs + 2] + 1;
            end;
          inc(j);
        end;
      Buf[Offs + 1] := Buf[Offs + 1] + 1;
    end;
end;

procedure DSErrFinish(var Buf: TLVec);
var
  NOut: TLInt;
  Offs: TLInt;
begin
  Offs := 5;
  NOut := AbsInt(Round(Buf[Offs]));
  if AP_FP_NEq(Buf[Offs + 1], 0) then
    begin
      Buf[0] := Buf[0] / Buf[Offs + 1];
      Buf[1] := Buf[1] / Buf[Offs + 1];
      Buf[2] := Sqrt(Buf[2] / (NOut * Buf[Offs + 1]));
      Buf[3] := Buf[3] / (NOut * Buf[Offs + 1]);
    end;
  if AP_FP_NEq(Buf[Offs + 2], 0) then
    begin
      Buf[4] := Buf[4] / Buf[Offs + 2];
    end;
end;

(* ************************************************************************
  Like MLPCreate0, but for ensembles.
  ************************************************************************ *)
procedure MLPECreate0(NIn, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreate0(NIn, NOut, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreate1, but for ensembles.
  ************************************************************************ *)
procedure MLPECreate1(NIn, NHid, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreate1(NIn, NHid, NOut, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreate2, but for ensembles.
  ************************************************************************ *)
procedure MLPECreate2(NIn, NHid1, NHid2, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreate2(NIn, NHid1, NHid2, NOut, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateB0, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateB0(NIn, NOut: TLInt; b, d: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateB0(NIn, NOut, b, d, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateB1, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateB1(NIn, NHid, NOut: TLInt; b, d: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateB1(NIn, NHid, NOut, b, d, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateB2, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateB2(NIn, NHid1, NHid2, NOut: TLInt; b, d: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateB2(NIn, NHid1, NHid2, NOut, b, d, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateR0, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateR0(NIn, NOut: TLInt; a, b: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateR0(NIn, NOut, a, b, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateR1, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateR1(NIn, NHid, NOut: TLInt; a, b: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateR1(NIn, NHid, NOut, a, b, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateR2, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateR2(NIn, NHid1, NHid2, NOut: TLInt; a, b: TLFloat; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateR2(NIn, NHid1, NHid2, NOut, a, b, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateC0, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateC0(NIn, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateC0(NIn, NOut, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateC1, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateC1(NIn, NHid, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateC1(NIn, NHid, NOut, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Like MLPCreateC2, but for ensembles.
  ************************************************************************ *)
procedure MLPECreateC2(NIn, NHid1, NHid2, NOut, EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  NET: TMultiLayerPerceptron;
begin
  MLPCreateC2(NIn, NHid1, NHid2, NOut, NET);
  MLPECreateFromNetwork(NET, EnsembleSize, Ensemble);
end;

(* ************************************************************************
  Creates ensemble from network. Only network geometry is copied.
  ************************************************************************ *)
procedure MLPECreateFromNetwork(const Network: TMultiLayerPerceptron; EnsembleSize: TLInt; var Ensemble: TMLPEnsemble);
var
  i     : TLInt;
  CCount: TLInt;
begin
  Assert(EnsembleSize > 0, 'MLPECreate: incorrect ensemble size!');

  //
  // network properties
  //
  MLPProperties(Network, Ensemble.NIn, Ensemble.NOut, Ensemble.WCount);
  if MLPIsSoftmax(Network) then
    begin
      CCount := Ensemble.NIn;
    end
  else
    begin
      CCount := Ensemble.NIn + Ensemble.NOut;
    end;
  Ensemble.PostProcessing := False;
  Ensemble.IsSoftmax := MLPIsSoftmax(Network);
  Ensemble.EnsembleSize := EnsembleSize;

  //
  // structure information
  //
  SetLength(Ensemble.StructInfo, Network.StructInfo[0]  );
  i := 0;
  while i <= Network.StructInfo[0] - 1 do
    begin
      Ensemble.StructInfo[i] := Network.StructInfo[i];
      inc(i);
    end;

  //
  // weights, means, sigmas
  //
  SetLength(Ensemble.Weights, EnsembleSize * Ensemble.WCount  );
  SetLength(Ensemble.ColumnMeans, EnsembleSize * CCount  );
  SetLength(Ensemble.ColumnSigmas, EnsembleSize * CCount  );
  i := 0;
  while i <= EnsembleSize * Ensemble.WCount - 1 do
    begin
      Ensemble.Weights[i] := RandomReal - 0.5;
      inc(i);
    end;
  i := 0;
  while i <= EnsembleSize - 1 do
    begin
      APVMove(@Ensemble.ColumnMeans[0], i * CCount, (i + 1) * CCount - 1,
        @Network.ColumnMeans[0], 0, CCount - 1);
      APVMove(@Ensemble.ColumnSigmas[0], i * CCount, (i + 1) * CCount - 1,
        @Network.ColumnSigmas[0], 0, CCount - 1);
      inc(i);
    end;

  //
  // serialized part
  //
  MLPSerialize(Network, Ensemble.SerializedMLP, Ensemble.SerializedLen);

  //
  // temporaries, internal buffers
  //
  SetLength(Ensemble.TmpWeights, Ensemble.WCount  );
  SetLength(Ensemble.TmpMeans, CCount  );
  SetLength(Ensemble.TmpSigmas, CCount  );
  SetLength(Ensemble.Neurons, Ensemble.StructInfo[MLPNTotalOffset]  );
  SetLength(Ensemble.DFDNET, Ensemble.StructInfo[MLPNTotalOffset]  );
  SetLength(Ensemble.y, Ensemble.NOut  );
end;

(* ************************************************************************
  Copying of TMLPEnsemble strucure

  INPUT PARAMETERS:
  Ensemble1 -   original

  OUTPUT PARAMETERS:
  Ensemble2 -   copy
  ************************************************************************ *)
procedure MLPECopy(const Ensemble1: TMLPEnsemble; var Ensemble2: TMLPEnsemble);
var
  i     : TLInt;
  SSize : TLInt;
  CCount: TLInt;
  NTotal: TLInt;
begin

  //
  // Unload info
  //
  SSize := Ensemble1.StructInfo[0];
  if Ensemble1.IsSoftmax then
    begin
      CCount := Ensemble1.NIn;
    end
  else
    begin
      CCount := Ensemble1.NIn + Ensemble1.NOut;
    end;
  NTotal := Ensemble1.StructInfo[MLPNTotalOffset];

  //
  // Allocate space
  //
  SetLength(Ensemble2.StructInfo, SSize  );
  SetLength(Ensemble2.Weights, Ensemble1.EnsembleSize *
    Ensemble1.WCount  );
  SetLength(Ensemble2.ColumnMeans, Ensemble1.EnsembleSize * CCount  );
  SetLength(Ensemble2.ColumnSigmas, Ensemble1.EnsembleSize * CCount  );
  SetLength(Ensemble2.TmpWeights, Ensemble1.WCount  );
  SetLength(Ensemble2.TmpMeans, CCount  );
  SetLength(Ensemble2.TmpSigmas, CCount  );
  SetLength(Ensemble2.SerializedMLP, Ensemble1.SerializedLen  );
  SetLength(Ensemble2.Neurons, NTotal  );
  SetLength(Ensemble2.DFDNET, NTotal  );
  SetLength(Ensemble2.y, Ensemble1.NOut  );

  //
  // Copy
  //
  Ensemble2.NIn := Ensemble1.NIn;
  Ensemble2.NOut := Ensemble1.NOut;
  Ensemble2.WCount := Ensemble1.WCount;
  Ensemble2.EnsembleSize := Ensemble1.EnsembleSize;
  Ensemble2.IsSoftmax := Ensemble1.IsSoftmax;
  Ensemble2.PostProcessing := Ensemble1.PostProcessing;
  Ensemble2.SerializedLen := Ensemble1.SerializedLen;
  i := 0;
  while i <= SSize - 1 do
    begin
      Ensemble2.StructInfo[i] := Ensemble1.StructInfo[i];
      inc(i);
    end;
  APVMove(@Ensemble2.Weights[0], 0, Ensemble1.EnsembleSize * Ensemble1.WCount -
    1, @Ensemble1.Weights[0], 0, Ensemble1.EnsembleSize * Ensemble1.WCount - 1);
  APVMove(@Ensemble2.ColumnMeans[0], 0, Ensemble1.EnsembleSize * CCount - 1,
    @Ensemble1.ColumnMeans[0], 0, Ensemble1.EnsembleSize * CCount - 1);
  APVMove(@Ensemble2.ColumnSigmas[0], 0, Ensemble1.EnsembleSize * CCount - 1,
    @Ensemble1.ColumnSigmas[0], 0, Ensemble1.EnsembleSize * CCount - 1);
  APVMove(@Ensemble2.SerializedMLP[0], 0, Ensemble1.SerializedLen - 1,
    @Ensemble1.SerializedMLP[0], 0, Ensemble1.SerializedLen - 1);
end;

(* ************************************************************************
  Serialization of TMLPEnsemble strucure

  INPUT PARAMETERS:
  Ensemble-   original

  OUTPUT PARAMETERS:
  RA      -   array of real numbers which stores ensemble,
  array[0..RLen-1]
  RLen    -   RA lenght
  ************************************************************************ *)
procedure MLPESerialize(var Ensemble: TMLPEnsemble; var RA: TLVec;
  var RLen: TLInt);
var
  i     : TLInt;
  SSize : TLInt;
  NTotal: TLInt;
  CCount: TLInt;
  HSize : TLInt;
  Offs  : TLInt;
begin
  HSize := 13;
  SSize := Ensemble.StructInfo[0];
  if Ensemble.IsSoftmax then
    begin
      CCount := Ensemble.NIn;
    end
  else
    begin
      CCount := Ensemble.NIn + Ensemble.NOut;
    end;
  NTotal := Ensemble.StructInfo[MLPNTotalOffset];
  RLen := HSize + SSize + Ensemble.EnsembleSize * Ensemble.WCount + 2 * CCount *
    Ensemble.EnsembleSize + Ensemble.SerializedLen;

  //
  // RA format:
  // [0]     RLen
  // [1]     Version (MLPEVNum)
  // [2]     EnsembleSize
  // [3]     NIn
  // [4]     NOut
  // [5]     WCount
  // [6]     IsSoftmax 0/1
  // [7]     PostProcessing 0/1
  // [8]     sizeof(StructInfo)
  // [9]     NTotal (sizeof(Neurons), sizeof(DFDNET))
  // [10]    CCount (sizeof(ColumnMeans), sizeof(ColumnSigmas))
  // [11]    data offset
  // [12]    SerializedLen
  //
  // [..]    StructInfo
  // [..]    Weights
  // [..]    ColumnMeans
  // [..]    ColumnSigmas
  //
  SetLength(RA, RLen  );
  RA[0] := RLen;
  RA[1] := MLPEVNum;
  RA[2] := Ensemble.EnsembleSize;
  RA[3] := Ensemble.NIn;
  RA[4] := Ensemble.NOut;
  RA[5] := Ensemble.WCount;
  if Ensemble.IsSoftmax then
    begin
      RA[6] := 1;
    end
  else
    begin
      RA[6] := 0;
    end;
  if Ensemble.PostProcessing then
    begin
      RA[7] := 1;
    end
  else
    begin
      RA[7] := 9;
    end;
  RA[8] := SSize;
  RA[9] := NTotal;
  RA[10] := CCount;
  RA[11] := HSize;
  RA[12] := Ensemble.SerializedLen;
  Offs := HSize;
  i := Offs;
  while i <= Offs + SSize - 1 do
    begin
      RA[i] := Ensemble.StructInfo[i - Offs];
      inc(i);
    end;
  Offs := Offs + SSize;
  APVMove(@RA[0], Offs, Offs + Ensemble.EnsembleSize * Ensemble.WCount - 1,
    @Ensemble.Weights[0], 0, Ensemble.EnsembleSize * Ensemble.WCount - 1);
  Offs := Offs + Ensemble.EnsembleSize * Ensemble.WCount;
  APVMove(@RA[0], Offs, Offs + Ensemble.EnsembleSize * CCount - 1,
    @Ensemble.ColumnMeans[0], 0, Ensemble.EnsembleSize * CCount - 1);
  Offs := Offs + Ensemble.EnsembleSize * CCount;
  APVMove(@RA[0], Offs, Offs + Ensemble.EnsembleSize * CCount - 1,
    @Ensemble.ColumnSigmas[0], 0, Ensemble.EnsembleSize * CCount - 1);
  Offs := Offs + Ensemble.EnsembleSize * CCount;
  APVMove(@RA[0], Offs, Offs + Ensemble.SerializedLen - 1,
    @Ensemble.SerializedMLP[0], 0, Ensemble.SerializedLen - 1);
  Offs := Offs + Ensemble.SerializedLen;
end;

(* ************************************************************************
  Unserialization of TMLPEnsemble strucure

  INPUT PARAMETERS:
  RA      -   real array which stores ensemble

  OUTPUT PARAMETERS:
  Ensemble-   restored structure
  ************************************************************************ *)
procedure MLPEUNSerialize(const RA: TLVec; var Ensemble: TMLPEnsemble);
var
  i     : TLInt;
  SSize : TLInt;
  NTotal: TLInt;
  CCount: TLInt;
  HSize : TLInt;
  Offs  : TLInt;
begin
  Assert(Round(RA[1]) = MLPEVNum, 'MLPEUnserialize: incorrect array!');

  //
  // load info
  //
  HSize := 13;
  Ensemble.EnsembleSize := Round(RA[2]);
  Ensemble.NIn := Round(RA[3]);
  Ensemble.NOut := Round(RA[4]);
  Ensemble.WCount := Round(RA[5]);
  Ensemble.IsSoftmax := Round(RA[6]) = 1;
  Ensemble.PostProcessing := Round(RA[7]) = 1;
  SSize := Round(RA[8]);
  NTotal := Round(RA[9]);
  CCount := Round(RA[10]);
  Offs := Round(RA[11]);
  Ensemble.SerializedLen := Round(RA[12]);

  //
  // Allocate arrays
  //
  SetLength(Ensemble.StructInfo, SSize  );
  SetLength(Ensemble.Weights, Ensemble.EnsembleSize * Ensemble.WCount  );
  SetLength(Ensemble.ColumnMeans, Ensemble.EnsembleSize * CCount  );
  SetLength(Ensemble.ColumnSigmas, Ensemble.EnsembleSize * CCount  );
  SetLength(Ensemble.TmpWeights, Ensemble.WCount  );
  SetLength(Ensemble.TmpMeans, CCount  );
  SetLength(Ensemble.TmpSigmas, CCount  );
  SetLength(Ensemble.Neurons, NTotal  );
  SetLength(Ensemble.DFDNET, NTotal  );
  SetLength(Ensemble.SerializedMLP, Ensemble.SerializedLen  );
  SetLength(Ensemble.y, Ensemble.NOut  );

  //
  // load data
  //
  i := Offs;
  while i <= Offs + SSize - 1 do
    begin
      Ensemble.StructInfo[i - Offs] := Round(RA[i]);
      inc(i);
    end;
  Offs := Offs + SSize;
  APVMove(@Ensemble.Weights[0], 0, Ensemble.EnsembleSize * Ensemble.WCount - 1,
    @RA[0], Offs, Offs + Ensemble.EnsembleSize * Ensemble.WCount - 1);
  Offs := Offs + Ensemble.EnsembleSize * Ensemble.WCount;
  APVMove(@Ensemble.ColumnMeans[0], 0, Ensemble.EnsembleSize * CCount - 1,
    @RA[0], Offs, Offs + Ensemble.EnsembleSize * CCount - 1);
  Offs := Offs + Ensemble.EnsembleSize * CCount;
  APVMove(@Ensemble.ColumnSigmas[0], 0, Ensemble.EnsembleSize * CCount - 1,
    @RA[0], Offs, Offs + Ensemble.EnsembleSize * CCount - 1);
  Offs := Offs + Ensemble.EnsembleSize * CCount;
  APVMove(@Ensemble.SerializedMLP[0], 0, Ensemble.SerializedLen - 1, @RA[0],
    Offs, Offs + Ensemble.SerializedLen - 1);
  Offs := Offs + Ensemble.SerializedLen;
end;

(* ************************************************************************
  Randomization of MLP ensemble
  ************************************************************************ *)
procedure MLPERandomize(var Ensemble: TMLPEnsemble);
var
  i: TLInt;
begin
  i := 0;
  while i <= Ensemble.EnsembleSize * Ensemble.WCount - 1 do
    begin
      Ensemble.Weights[i] := RandomReal - 0.5;
      inc(i);
    end;
end;

(* ************************************************************************
  Return ensemble properties (number of inputs and outputs).
  ************************************************************************ *)
procedure MLPEProperties(const Ensemble: TMLPEnsemble; var NIn: TLInt;
  var NOut: TLInt);
begin
  NIn := Ensemble.NIn;
  NOut := Ensemble.NOut;
end;

(* ************************************************************************
  Return normalization type (whether ensemble is SOFTMAX-normalized or not).
  ************************************************************************ *)
function MLPEIsSoftmax(const Ensemble: TMLPEnsemble): Boolean;
begin
  Result := Ensemble.IsSoftmax;
end;

(* ************************************************************************
  Procesing

  INPUT PARAMETERS:
  Ensemble-   neural networks ensemble
  X       -   input vector,  array[0..NIn-1].

  OUTPUT PARAMETERS:
  Y       -   result. Regression estimate when solving regression  task,
  vector of posterior probabilities for classification task.
  Subroutine does not allocate memory for this vector, it is
  responsibility of a caller to allocate it. Array  must  be
  at least [0..NOut-1].
  ************************************************************************ *)
procedure MLPEProcess(var Ensemble: TMLPEnsemble; const x: TLVec;
  var y: TLVec);
var
  i : TLInt;
  ES: TLInt;
  WC: TLInt;
  CC: TLInt;
  v : TLFloat;
begin
  ES := Ensemble.EnsembleSize;
  WC := Ensemble.WCount;
  if Ensemble.IsSoftmax then
    begin
      CC := Ensemble.NIn;
    end
  else
    begin
      CC := Ensemble.NIn + Ensemble.NOut;
    end;
  v := AP_Float(1) / ES;
  i := 0;
  while i <= Ensemble.NOut - 1 do
    begin
      y[i] := 0;
      inc(i);
    end;
  i := 0;
  while i <= ES - 1 do
    begin
      APVMove(@Ensemble.TmpWeights[0], 0, WC - 1, @Ensemble.Weights[0], i * WC,
        (i + 1) * WC - 1);
      APVMove(@Ensemble.TmpMeans[0], 0, CC - 1, @Ensemble.ColumnMeans[0], i * CC,
        (i + 1) * CC - 1);
      APVMove(@Ensemble.TmpSigmas[0], 0, CC - 1, @Ensemble.ColumnSigmas[0],
        i * CC, (i + 1) * CC - 1);
      MLPInternalProcessVector(Ensemble.StructInfo, Ensemble.TmpWeights,
        Ensemble.TmpMeans, Ensemble.TmpSigmas, Ensemble.Neurons, Ensemble.DFDNET,
        x, Ensemble.y);
      APVAdd(@y[0], 0, Ensemble.NOut - 1, @Ensemble.y[0], 0,
        Ensemble.NOut - 1, v);
      inc(i);
    end;
end;

(* ************************************************************************
  Relative classification error on the test set

  INPUT PARAMETERS:
  Ensemble-   ensemble
  XY      -   test set
  NPoints -   test set size

  RESULT:
  percent of incorrectly classified cases.
  Works both for classifier betwork and for regression networks which
  are used as classifiers.
  ************************************************************************ *)
function MLPERelClsError(var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  MLPEAllErrors(Ensemble, xy, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := RelCls;
end;

(* ************************************************************************
  Average cross-entropy (in bits per element) on the test set

  INPUT PARAMETERS:
  Ensemble-   ensemble
  XY      -   test set
  NPoints -   test set size

  RESULT:
  CrossEntropy/(NPoints*LN(2)).
  Zero if ensemble solves regression task.
  ************************************************************************ *)
function MLPEAvgCE(var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  MLPEAllErrors(Ensemble, xy, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := AvgCE;
end;

(* ************************************************************************
  RMS error on the test set

  INPUT PARAMETERS:
  Ensemble-   ensemble
  XY      -   test set
  NPoints -   test set size

  RESULT:
  root mean square error.
  Its meaning for regression task is obvious. As for classification task
  RMS error means error when estimating posterior probabilities.
  ************************************************************************ *)
function MLPERMSError(var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  MLPEAllErrors(Ensemble, xy, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := RMS;
end;

(* ************************************************************************
  Average error on the test set

  INPUT PARAMETERS:
  Ensemble-   ensemble
  XY      -   test set
  NPoints -   test set size

  RESULT:
  Its meaning for regression task is obvious. As for classification task
  it means average error when estimating posterior probabilities.
  ************************************************************************ *)
function MLPEAvgError(var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  MLPEAllErrors(Ensemble, xy, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := Avg;
end;

(* ************************************************************************
  Average relative error on the test set

  INPUT PARAMETERS:
  Ensemble-   ensemble
  XY      -   test set
  NPoints -   test set size

  RESULT:
  Its meaning for regression task is obvious. As for classification task
  it means average relative error when estimating posterior probabilities.
  ************************************************************************ *)
function MLPEAvgRelError(var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  RelCls: TLFloat;
  AvgCE : TLFloat;
  RMS   : TLFloat;
  Avg   : TLFloat;
  AvgRel: TLFloat;
begin
  MLPEAllErrors(Ensemble, xy, NPoints, RelCls, AvgCE, RMS, Avg, AvgRel);
  Result := AvgRel;
end;

(* ************************************************************************
  Training neural networks ensemble using  bootstrap  aggregating (bagging).
  Modified Levenberg-Marquardt algorithm is used as base training method.

  INPUT PARAMETERS:
  MultiThread -   parallel train
  Ensemble    -   model with initialized geometry
  XY          -   training set
  NPoints     -   training set size
  Decay       -   weight decay coefficient, >=0.001
  Restarts    -   restarts, >0.

  OUTPUT PARAMETERS:
  Ensemble    -   trained model
  Info        -   return code:
  * -2, if there is a point with class number outside of [0..NClasses-1].
  * -1, if incorrect parameters was passed (NPoints<0, Restarts<1).
  *  2, if task has been solved.
  Rep         -   training report.
  OOBErrors   -   out-of-bag generalization error estimate
  ************************************************************************ *)
procedure MLPEBaggingLM(const MultiThread: Boolean; var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  var Info: TLInt; var Rep: TMLPReport; var OOBErrors: TMLPCVReport);
begin
  MLPEBaggingInternal(MultiThread, Ensemble, xy, NPoints, Decay, Restarts, 0.0, 0, True,
    Info, Rep, OOBErrors);
end;

(* ************************************************************************
  Training neural networks ensemble using  bootstrap  aggregating (bagging).
  L-BFGS algorithm is used as base training method.

  INPUT PARAMETERS:
  MultiThread -   parallel train
  Ensemble    -   model with initialized geometry
  XY          -   training set
  NPoints     -   training set size
  Decay       -   weight decay coefficient, >=0.001
  Restarts    -   restarts, >0.
  WStep       -   stopping criterion, same as in MLPTrainLBFGS
  MaxIts      -   stopping criterion, same as in MLPTrainLBFGS

  OUTPUT PARAMETERS:
  Ensemble    -   trained model
  Info        -   return code:
  * -8, if both WStep=0 and MaxIts=0
  * -2, if there is a point with class number outside of [0..NClasses-1].
  * -1, if incorrect parameters was passed (NPoints<0, Restarts<1).
  *  2, if task has been solved.
  Rep         -   training report.
  OOBErrors   -   out-of-bag generalization error estimate
  ************************************************************************ *)
procedure MLPEBaggingLBFGS(const MultiThread: Boolean; var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt; Decay: TLFloat; Restarts: TLInt;
  WStep: TLFloat; MAXITS: TLInt; var Info: TLInt;
  var Rep: TMLPReport; var OOBErrors: TMLPCVReport);
begin
  MLPEBaggingInternal(MultiThread, Ensemble, xy, NPoints, Decay, Restarts, WStep, MAXITS,
    False, Info, Rep, OOBErrors);
end;

(* ************************************************************************
  Calculation of all types of errors
  ************************************************************************ *)
procedure MLPEAllErrors(var Ensemble: TMLPEnsemble; const xy: TLMatrix;
  NPoints: TLInt; var RelCls: TLFloat; var AvgCE: TLFloat;
  var RMS: TLFloat; var Avg: TLFloat; var AvgRel: TLFloat);
var
  i    : TLInt;
  Buf  : TLVec;
  WorkX: TLVec;
  y    : TLVec;
  dy   : TLVec;
begin
  SetLength(WorkX, Ensemble.NIn  );
  SetLength(y, Ensemble.NOut  );
  if Ensemble.IsSoftmax then
    begin
      SetLength(dy, 0 + 1);
      DSErrAllocate(Ensemble.NOut, Buf);
    end
  else
    begin
      SetLength(dy, Ensemble.NOut  );
      DSErrAllocate(-Ensemble.NOut, Buf);
    end;
  i := 0;
  while i <= NPoints - 1 do
    begin
      APVMove(@WorkX[0], 0, Ensemble.NIn - 1, @xy[i][0], 0, Ensemble.NIn - 1);
      MLPEProcess(Ensemble, WorkX, y);
      if Ensemble.IsSoftmax then
        begin
          dy[0] := xy[i, Ensemble.NIn];
        end
      else
        begin
          APVMove(@dy[0], 0, Ensemble.NOut - 1, @xy[i][0], Ensemble.NIn,
            Ensemble.NIn + Ensemble.NOut - 1);
        end;
      DSErrAccumulate(Buf, y, dy);
      inc(i);
    end;
  DSErrFinish(Buf);
  RelCls := Buf[0];
  AvgCE := Buf[1];
  RMS := Buf[2];
  Avg := Buf[3];
  AvgRel := Buf[4];
end;

(* ************************************************************************
  Internal bagging subroutine.
  ************************************************************************ *)
procedure MLPEBaggingInternal(const MultiThread: Boolean; var Ensemble: TMLPEnsemble;
  const xy: TLMatrix; NPoints: TLInt; Decay: TLFloat;
  Restarts: TLInt; WStep: TLFloat; MAXITS: TLInt;
  LMAlgorithm: Boolean; var Info: TLInt; var Rep: TMLPReport;
  var OOBErrors: TMLPCVReport);
var
  XYS         : TLMatrix;
  s           : TLBVec;
  OOBBuf      : TLMatrix;
  OOBCntBuf   : TLIVec;
  x           : TLVec;
  y           : TLVec;
  dy          : TLVec;
  DSBuf       : TLVec;
  NIn         : TLInt;
  NOut        : TLInt;
  CCnt        : TLInt;
  PCnt        : TLInt;
  i           : TLInt;
  j           : TLInt;
  k           : TLInt;
  v           : TLFloat;
  TmpRep      : TMLPReport;
  Network     : TMultiLayerPerceptron;
  IsTerminated: Boolean;
  EBest       : TLFloat;
begin

  //
  // Test for inputs
  //
  if not LMAlgorithm and AP_FP_Eq(WStep, 0) and (MAXITS = 0) then
    begin
      Info := -8;
      Exit;
    end;
  if (NPoints <= 0) or (Restarts < 1) or AP_FP_Less(WStep, 0) or (MAXITS < 0)
  then
    begin
      Info := -1;
      Exit;
    end;
  if Ensemble.IsSoftmax then
    begin
      i := 0;
      while i <= NPoints - 1 do
        begin
          if (Round(xy[i, Ensemble.NIn]) < 0) or
            (Round(xy[i, Ensemble.NIn]) >= Ensemble.NOut) then
            begin
              Info := -2;
              Exit;
            end;
          inc(i);
        end;
    end;

  //
  // allocate temporaries
  //
  Info := 2;
  Rep.NGrad := 0;
  Rep.NHess := 0;
  Rep.NCholesky := 0;
  OOBErrors.RelClsError := 0;
  OOBErrors.AvgCE := 0;
  OOBErrors.RMSError := 0;
  OOBErrors.AvgError := 0;
  OOBErrors.AvgRelError := 0;
  NIn := Ensemble.NIn;
  NOut := Ensemble.NOut;
  if Ensemble.IsSoftmax then
    begin
      CCnt := NIn + 1;
      PCnt := NIn;
    end
  else
    begin
      CCnt := NIn + NOut;
      PCnt := NIn + NOut;
    end;
  SetLength(XYS, NPoints  , CCnt  );
  SetLength(s, NPoints  );
  SetLength(OOBBuf, NPoints  , NOut  );
  SetLength(OOBCntBuf, NPoints  );
  SetLength(x, NIn  );
  SetLength(y, NOut  );
  if Ensemble.IsSoftmax then
    begin
      SetLength(dy, 0 + 1);
    end
  else
    begin
      SetLength(dy, NOut  );
    end;
  i := 0;
  while i <= NPoints - 1 do
    begin
      j := 0;
      while j <= NOut - 1 do
        begin
          OOBBuf[i, j] := 0;
          inc(j);
        end;
      inc(i);
    end;
  i := 0;
  while i <= NPoints - 1 do
    begin
      OOBCntBuf[i] := 0;
      inc(i);
    end;
  MLPUNSerialize(Ensemble.SerializedMLP, Network);

  //
  // main bagging cycle
  //
  k := 0;
  while k <= Ensemble.EnsembleSize - 1 do
    begin

      //
      // prepare dataset
      //
      i := 0;
      while i <= NPoints - 1 do
        begin
          s[i] := False;
          inc(i);
        end;
      i := 0;
      while i <= NPoints - 1 do
        begin
          j := RandomInteger(NPoints);
          s[j] := True;
          APVMove(@XYS[i][0], 0, CCnt - 1, @xy[j][0], 0, CCnt - 1);
          inc(i);
        end;

      //
      // train
      //
      if LMAlgorithm then
        begin
          if MultiThread then
              MLPTrainLM_MT(Network, XYS, NPoints, Decay, Restarts, Info, TmpRep)
          else
              MLPTrainLM(Network, XYS, NPoints, Decay, Restarts, Info, TmpRep);
        end
      else
        begin
          IsTerminated := False;

          if MultiThread then
              MLPTrainLBFGS_MT(Network, XYS, NPoints, Decay, Restarts, WStep, MAXITS, Info, TmpRep)
          else
              MLPTrainLBFGS(Network, XYS, NPoints, Decay, Restarts, WStep, MAXITS, Info, TmpRep, @IsTerminated, EBest);
        end;
      if Info < 0 then
        begin
          Exit;
        end;

      //
      // save results
      //
      Rep.NGrad := Rep.NGrad + TmpRep.NGrad;
      Rep.NHess := Rep.NHess + TmpRep.NHess;
      Rep.NCholesky := Rep.NCholesky + TmpRep.NCholesky;
      APVMove(@Ensemble.Weights[0], k * Ensemble.WCount, (k + 1) * Ensemble.WCount
        - 1, @Network.Weights[0], 0, Ensemble.WCount - 1);
      APVMove(@Ensemble.ColumnMeans[0], k * PCnt, (k + 1) * PCnt - 1,
        @Network.ColumnMeans[0], 0, PCnt - 1);
      APVMove(@Ensemble.ColumnSigmas[0], k * PCnt, (k + 1) * PCnt - 1,
        @Network.ColumnSigmas[0], 0, PCnt - 1);

      //
      // OOB estimates
      //
      i := 0;
      while i <= NPoints - 1 do
        begin
          if not s[i] then
            begin
              APVMove(@x[0], 0, NIn - 1, @xy[i][0], 0, NIn - 1);
              MLPProcess(Network, x, y);
              APVAdd(@OOBBuf[i][0], 0, NOut - 1, @y[0], 0, NOut - 1);
              OOBCntBuf[i] := OOBCntBuf[i] + 1;
            end;
          inc(i);
        end;
      inc(k);
    end;

  //
  // OOB estimates
  //
  if Ensemble.IsSoftmax then
    begin
      DSErrAllocate(NOut, DSBuf);
    end
  else
    begin
      DSErrAllocate(-NOut, DSBuf);
    end;
  i := 0;
  while i <= NPoints - 1 do
    begin
      if OOBCntBuf[i] <> 0 then
        begin
          v := AP_Float(1) / OOBCntBuf[i];
          APVMove(@y[0], 0, NOut - 1, @OOBBuf[i][0], 0, NOut - 1, v);
          if Ensemble.IsSoftmax then
            begin
              dy[0] := xy[i, NIn];
            end
          else
            begin
              APVMove(@dy[0], 0, NOut - 1, @xy[i][0], NIn, NIn + NOut - 1, v);
            end;
          DSErrAccumulate(DSBuf, y, dy);
        end;
      inc(i);
    end;
  DSErrFinish(DSBuf);
  OOBErrors.RelClsError := DSBuf[0];
  OOBErrors.AvgCE := DSBuf[1];
  OOBErrors.RMSError := DSBuf[2];
  OOBErrors.AvgError := DSBuf[3];
  OOBErrors.AvgRelError := DSBuf[4];
end; 
 
 
