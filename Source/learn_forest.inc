{ ****************************************************************************** }
{ * Random Decision Forest, by QQ 600585@qq.com                                * }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
type
  TDecisionForest = record
    NVars: TLInt;
    NClasses: TLInt;
    NTrees: TLInt;
    BufSize: TLInt;
    Trees: TLVec;
  end;

  PDecisionForest = ^TDecisionForest;

  TDFReport = record
    RelClsError: TLFloat;
    AvgCE: TLFloat;
    RMSError: TLFloat;
    AvgError: TLFloat;
    AvgRelError: TLFloat;
    OOBRelClsError: TLFloat;
    OOBAvgCE: TLFloat;
    OOBRMSError: TLFloat;
    OOBAvgError: TLFloat;
    OOBAvgRelError: TLFloat;
  end;

procedure DFBuildRandomDecisionForest(const xy: TLMatrix; NPoints, NVars, NClasses, NTrees: TLInt; r: TLFloat;
  var Info: TLInt; var df: TDecisionForest; var Rep: TDFReport); forward;

procedure DFBuildInternal(const xy: TLMatrix; NPoints: TLInt;
  NVars: TLInt; NClasses: TLInt; NTrees: TLInt;
  SampleSize: TLInt; NFeatures: TLInt; Flags: TLInt;
  var Info: TLInt; var df: TDecisionForest; var Rep: TDFReport); forward;

procedure DFProcess(const df: TDecisionForest; const x: TLVec; var y: TLVec); forward;
function DFRelClsError(const df: TDecisionForest; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function DFAvgCE(const df: TDecisionForest; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function DFRMSError(const df: TDecisionForest; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function DFAvgError(const df: TDecisionForest; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
function DFAvgRelError(const df: TDecisionForest; const xy: TLMatrix; NPoints: TLInt): TLFloat; forward;
procedure DFCopy(const DF1: TDecisionForest; var DF2: TDecisionForest); forward;
procedure DFSerialize(const df: TDecisionForest; var RA: TLVec; var RLen: TLInt); forward;
procedure DFUnserialize(const RA: TLVec; var df: TDecisionForest); forward;

const
  DFVNum = 8;
  InnerNodeWidth = 3;
  LeafNodeWidth = 2;
  DFUseStrongSplits = 1;
  DFUseEVS = 2;

type
  TDFInternalBuffers = record
    TreeBuf: TLVec;
    IdxBuf: TLIVec;
    TmpBufR: TLVec;
    TmpBufR2: TLVec;
    TmpBufI: TLIVec;
    ClassIBuf: TLIVec;
    VarPool: TLIVec;
    EVSBin: TLBVec;
    EVSSplits: TLVec;
  end;

function DFClsError(const df: TDecisionForest; const xy: TLMatrix; NPoints: TLInt): TLInt; forward;
procedure DFProcessInternal(const df: TDecisionForest; Offs: TLInt; const x: TLVec; var y: TLVec); forward;

procedure DFBuildTree(const xy: TLMatrix; NPoints: TLInt;
  NVars: TLInt; NClasses: TLInt; NFeatures: TLInt;
  NVarsInPool: TLInt; Flags: TLInt;
  var Bufs: TDFInternalBuffers); forward;

procedure DFBuildTreeRec(const xy: TLMatrix; NPoints: TLInt;
  NVars: TLInt; NClasses: TLInt; NFeatures: TLInt;
  NVarsInPool: TLInt; Flags: TLInt;
  var NumProcessed: TLInt; idx1: TLInt; idx2: TLInt;
  var Bufs: TDFInternalBuffers); forward;

procedure DFWeakSplitI(var x: TLVec; var y: TLIVec;
  n: TLInt; NClasses: TLInt; var Info: TLInt;
  var Threshold: TLFloat; var E: TLFloat); forward;

procedure DFSplitC(var x: TLVec; var c: TLIVec;
  var CntBuf: TLIVec; n: TLInt; NC: TLInt; Flags: TLInt; var Info: TLInt; var Threshold: TLFloat; var E: TLFloat); forward;

procedure DFSplitR(var x: TLVec; var y: TLVec; n: TLInt; Flags: TLInt; var Info: TLInt; var Threshold: TLFloat; var E: TLFloat); forward;

(* ************************************************************************
  //  This subroutine builds random decision forest.
  //
  //  INPUT PARAMETERS:
  //  XY          -   training set
  //  NPoints     -   training set size, NPoints>=1
  //  NVars       -   number of independent variables, NVars>=1
  //  NClasses    -   task type:
  //                  * NClasses=1 - regression task with one dependent variable
  //                  * NClasses>1 - classification task with NClasses classes.
  //  NTrees      -   number of trees in a forest, NTrees>=1.
  //                  recommended values: 50-100.
  //  R           -   percent of a training set used to build individual trees. 0<R<=1.
  //                  recommended values: 0.1 <= R <= 0.66.
  //
  //  OUTPUT PARAMETERS:
  //  Info        -   return code:
  //                  * -2, if there is a point with class number outside of [0..NClasses-1].
  //                  * -1, if incorrect parameters was passed (NPoints<1, NVars<1, NClasses<1, NTrees<1, R<=0 or R>1).
  //                  *  1, if task has been solved
  //  DF          -   model built
  //  Rep         -   training report, contains error on a training set and out-of-bag estimates of generalization error.
  ************************************************************************ *)
procedure DFBuildRandomDecisionForest(const xy: TLMatrix; NPoints, NVars, NClasses, NTrees: TLInt; r: TLFloat;
  var Info: TLInt; var df: TDecisionForest; var Rep: TDFReport);
var
  SampleSize: TLInt;

begin
  if AP_FP_Less_Eq(r, 0) or AP_FP_Greater(r, 1) then
    begin
      Info := -1;
      Exit;
    end;
  SampleSize := Max(Round(r * NPoints), 1);
  DFBuildInternal(xy, NPoints, NVars, NClasses, NTrees, SampleSize,
    Max(NVars div 2, 1), DFUseStrongSplits + DFUseEVS, Info, df, Rep);
end;

procedure DFBuildInternal(const xy: TLMatrix; NPoints: TLInt;
  NVars: TLInt; NClasses: TLInt; NTrees: TLInt;
  SampleSize: TLInt; NFeatures: TLInt; Flags: TLInt;
  var Info: TLInt; var df: TDecisionForest; var Rep: TDFReport);
var
  i: TLInt;
  j: TLInt;
  k: TLInt;
  TmpI: TLInt;
  LastTreeOffs: TLInt;
  Offs: TLInt;
  OOBOffs: TLInt;
  TreeSize: TLInt;
  NVarsInPool: TLInt;
  UseEVS: Boolean;
  Bufs: TDFInternalBuffers;
  PermBuf: TLIVec;
  OOBBuf: TLVec;
  OOBCntBuf: TLIVec;
  XYS: TLMatrix;
  x: TLVec;
  y: TLVec;
  OOBCnt: TLInt;
  OOBRelCnt: TLInt;
  v: TLFloat;
  VMin: TLFloat;
  VMAX: TLFloat;
  BFlag: Boolean;
begin

  //
  // Test for inputs
  //
  if (NPoints < 1) or (SampleSize < 1) or (SampleSize > NPoints) or (NVars < 1)
    or (NClasses < 1) or (NTrees < 1) or (NFeatures < 1) then
    begin
      Info := -1;
      Exit;
    end;
  if NClasses > 1 then
    begin
      i := 0;
      while i <= NPoints - 1 do
        begin
          if (Round(xy[i, NVars]) < 0) or (Round(xy[i, NVars]) >= NClasses) then
            begin
              Info := -2;
              Exit;
            end;
          inc(i);
        end;
    end;
  Info := 1;

  //
  // Flags
  //
  UseEVS := Flags div DFUseEVS mod 2 <> 0;

  //
  // Allocate data, prepare header
  //
  TreeSize := 1 + InnerNodeWidth * (SampleSize - 1) + LeafNodeWidth *
    SampleSize;
  SetLength(PermBuf, NPoints  );
  SetLength(Bufs.TreeBuf, TreeSize  );
  SetLength(Bufs.IdxBuf, NPoints  );
  SetLength(Bufs.TmpBufR, NPoints  );
  SetLength(Bufs.TmpBufR2, NPoints  );
  SetLength(Bufs.TmpBufI, NPoints  );
  SetLength(Bufs.VarPool, NVars  );
  SetLength(Bufs.EVSBin, NVars  );
  SetLength(Bufs.EVSSplits, NVars  );
  SetLength(Bufs.ClassIBuf, 2 * NClasses  );
  SetLength(OOBBuf, NClasses * NPoints  );
  SetLength(OOBCntBuf, NPoints  );
  SetLength(df.Trees, NTrees * TreeSize  );
  SetLength(XYS, SampleSize  , NVars + 1);
  SetLength(x, NVars  );
  SetLength(y, NClasses  );
  i := 0;
  while i <= NPoints - 1 do
    begin
      PermBuf[i] := i;
      inc(i);
    end;
  i := 0;
  while i <= NPoints * NClasses - 1 do
    begin
      OOBBuf[i] := 0;
      inc(i);
    end;
  i := 0;
  while i <= NPoints - 1 do
    begin
      OOBCntBuf[i] := 0;
      inc(i);
    end;

  //
  // Prepare variable pool and EVS (extended variable selection/splitting) buffers
  // (whether EVS is turned on or not):
  // 1. detect binary variables and pre-calculate splits for them
  // 2. detect variables with non-distinct values and exclude them from pool
  //
  i := 0;
  while i <= NVars - 1 do
    begin
      Bufs.VarPool[i] := i;
      inc(i);
    end;
  NVarsInPool := NVars;
  if UseEVS then
    begin
      j := 0;
      while j <= NVars - 1 do
        begin
          VMin := xy[0, j];
          VMAX := VMin;
          i := 0;
          while i <= NPoints - 1 do
            begin
              v := xy[i, j];
              VMin := Min(VMin, v);
              VMAX := Max(VMAX, v);
              inc(i);
            end;
          if AP_FP_Eq(VMin, VMAX) then
            begin

              //
              // exclude variable from pool
              //
              Bufs.VarPool[j] := Bufs.VarPool[NVarsInPool - 1];
              Bufs.VarPool[NVarsInPool - 1] := -1;
              NVarsInPool := NVarsInPool - 1;
              inc(j);
              Continue;
            end;
          BFlag := False;
          i := 0;
          while i <= NPoints - 1 do
            begin
              v := xy[i, j];
              if AP_FP_NEq(v, VMin) and AP_FP_NEq(v, VMAX) then
                begin
                  BFlag := True;
                  Break;
                end;
              inc(i);
            end;
          if BFlag then
            begin

              //
              // non-binary variable
              //
              Bufs.EVSBin[j] := False;
            end
          else
            begin

              //
              // Prepare
              //
              Bufs.EVSBin[j] := True;
              Bufs.EVSSplits[j] := 0.5 * (VMin + VMAX);
              if AP_FP_Less_Eq(Bufs.EVSSplits[j], VMin) then
                begin
                  Bufs.EVSSplits[j] := VMAX;
                end;
            end;
          inc(j);
        end;
    end;

  //
  // RANDOM FOREST FORMAT
  // W[0]         -   size of array
  // W[1]         -   version number
  // W[2]         -   NVars
  // W[3]         -   NClasses (1 for regression)
  // W[4]         -   NTrees
  // W[5]         -   trees offset
  //
  //
  // TREE FORMAT
  // W[Offs]      -   size of sub-array
  // node info:
  // W[K+0]       -   variable number        (-1 for leaf mode)
  // W[K+1]       -   threshold              (class/value for leaf node)
  // W[K+2]       -   ">=" branch index      (absent for leaf node)
  //
  //
  df.NVars := NVars;
  df.NClasses := NClasses;
  df.NTrees := NTrees;

  //
  // Build forest
  //
  Offs := 0;
  i := 0;
  while i <= NTrees - 1 do
    begin

      //
      // Prepare sample
      //
      k := 0;
      while k <= SampleSize - 1 do
        begin
          j := k + RandomInteger(NPoints - k);
          TmpI := PermBuf[k];
          PermBuf[k] := PermBuf[j];
          PermBuf[j] := TmpI;
          j := PermBuf[k];
          APVMove(@XYS[k][0], 0, NVars, @xy[j][0], 0, NVars);
          inc(k);
        end;

      //
      // build tree, copy
      //
      DFBuildTree(XYS, SampleSize, NVars, NClasses, NFeatures, NVarsInPool,
        Flags, Bufs);
      j := Round(Bufs.TreeBuf[0]);
      APVMove(@df.Trees[0], Offs, Offs + j - 1, @Bufs.TreeBuf[0], 0, j - 1);
      LastTreeOffs := Offs;
      Offs := Offs + j;

      //
      // OOB estimates
      //
      k := SampleSize;
      while k <= NPoints - 1 do
        begin
          j := 0;
          while j <= NClasses - 1 do
            begin
              y[j] := 0;
              inc(j);
            end;
          j := PermBuf[k];
          APVMove(@x[0], 0, NVars - 1, @xy[j][0], 0, NVars - 1);
          DFProcessInternal(df, LastTreeOffs, x, y);
          APVAdd(@OOBBuf[0], j * NClasses, (j + 1) * NClasses - 1, @y[0], 0,
            NClasses - 1);
          OOBCntBuf[j] := OOBCntBuf[j] + 1;
          inc(k);
        end;
      inc(i);
    end;
  df.BufSize := Offs;

  //
  // Normalize OOB results
  //
  i := 0;
  while i <= NPoints - 1 do
    begin
      if OOBCntBuf[i] <> 0 then
        begin
          v := AP_Float(1) / OOBCntBuf[i];
          APVMul(@OOBBuf[0], i * NClasses, i * NClasses + NClasses - 1, v);
        end;
      inc(i);
    end;

  //
  // Calculate training set estimates
  //
  Rep.RelClsError := DFRelClsError(df, xy, NPoints);
  Rep.AvgCE := DFAvgCE(df, xy, NPoints);
  Rep.RMSError := DFRMSError(df, xy, NPoints);
  Rep.AvgError := DFAvgError(df, xy, NPoints);
  Rep.AvgRelError := DFAvgRelError(df, xy, NPoints);

  //
  // Calculate OOB estimates.
  //
  Rep.OOBRelClsError := 0;
  Rep.OOBAvgCE := 0;
  Rep.OOBRMSError := 0;
  Rep.OOBAvgError := 0;
  Rep.OOBAvgRelError := 0;
  OOBCnt := 0;
  OOBRelCnt := 0;
  i := 0;
  while i <= NPoints - 1 do
    begin
      if OOBCntBuf[i] <> 0 then
        begin
          OOBOffs := i * NClasses;
          if NClasses > 1 then
            begin

              //
              // classification-specific code
              //
              k := Round(xy[i, NVars]);
              TmpI := 0;
              j := 1;
              while j <= NClasses - 1 do
                begin
                  if AP_FP_Greater(OOBBuf[OOBOffs + j], OOBBuf[OOBOffs + TmpI]) then
                    begin
                      TmpI := j;
                    end;
                  inc(j);
                end;
              if TmpI <> k then
                begin
                  Rep.OOBRelClsError := Rep.OOBRelClsError + 1;
                end;
              if AP_FP_NEq(OOBBuf[OOBOffs + k], 0) then
                begin
                  Rep.OOBAvgCE := Rep.OOBAvgCE - ln(OOBBuf[OOBOffs + k]);
                end
              else
                begin
                  Rep.OOBAvgCE := Rep.OOBAvgCE - ln(MinRealNumber);
                end;
              j := 0;
              while j <= NClasses - 1 do
                begin
                  if j = k then
                    begin
                      Rep.OOBRMSError := Rep.OOBRMSError +
                        AP_Sqr(OOBBuf[OOBOffs + j] - 1);
                      Rep.OOBAvgError := Rep.OOBAvgError +
                        AbsReal(OOBBuf[OOBOffs + j] - 1);
                      Rep.OOBAvgRelError := Rep.OOBAvgRelError +
                        AbsReal(OOBBuf[OOBOffs + j] - 1);
                      OOBRelCnt := OOBRelCnt + 1;
                    end
                  else
                    begin
                      Rep.OOBRMSError := Rep.OOBRMSError + AP_Sqr(OOBBuf[OOBOffs + j]);
                      Rep.OOBAvgError := Rep.OOBAvgError + AbsReal(OOBBuf[OOBOffs + j]);
                    end;
                  inc(j);
                end;
            end
          else
            begin

              //
              // regression-specific code
              //
              Rep.OOBRMSError := Rep.OOBRMSError +
                AP_Sqr(OOBBuf[OOBOffs] - xy[i, NVars]);
              Rep.OOBAvgError := Rep.OOBAvgError +
                AbsReal(OOBBuf[OOBOffs] - xy[i, NVars]);
              if AP_FP_NEq(xy[i, NVars], 0) then
                begin
                  Rep.OOBAvgRelError := Rep.OOBAvgRelError +
                    AbsReal((OOBBuf[OOBOffs] - xy[i, NVars]) / xy[i, NVars]);
                  OOBRelCnt := OOBRelCnt + 1;
                end;
            end;

          //
          // update OOB estimates count.
          //
          OOBCnt := OOBCnt + 1;
        end;
      inc(i);
    end;
  if OOBCnt > 0 then
    begin
      Rep.OOBRelClsError := Rep.OOBRelClsError / OOBCnt;
      Rep.OOBAvgCE := Rep.OOBAvgCE / OOBCnt;
      Rep.OOBRMSError := Sqrt(Rep.OOBRMSError / (OOBCnt * NClasses));
      Rep.OOBAvgError := Rep.OOBAvgError / (OOBCnt * NClasses);
      if OOBRelCnt > 0 then
        begin
          Rep.OOBAvgRelError := Rep.OOBAvgRelError / OOBRelCnt;
        end;
    end;
end;

(* ************************************************************************
  Procesing

  INPUT PARAMETERS:
  DF      -   decision forest model
  X       -   input vector,  array[0..NVars-1].

  OUTPUT PARAMETERS:
  Y       -   result. Regression estimate when solving regression  task,
  vector of posterior probabilities for classification task. Subroutine does not allocate memory for this vector,
  it is responsibility of a caller to allocate it. Array must be at least [0..NClasses-1].
  ************************************************************************ *)
procedure DFProcess(const df: TDecisionForest; const x: TLVec; var y: TLVec);
var
  Offs: TLInt;
  i: TLInt;
  v: TLFloat;
begin

  //
  // Proceed
  //
  Offs := 0;
  i := 0;
  while i <= df.NClasses - 1 do
    begin
      y[i] := 0;
      inc(i);
    end;
  i := 0;
  while i <= df.NTrees - 1 do
    begin

      //
      // Process basic tree
      //
      DFProcessInternal(df, Offs, x, y);

      //
      // Next tree
      //
      Offs := Offs + Round(df.Trees[Offs]);
      inc(i);
    end;
  v := AP_Float(1) / df.NTrees;
  APVMul(@y[0], 0, df.NClasses - 1, v);
end;

(* ************************************************************************
  Relative classification error on the test set

  INPUT PARAMETERS:
  DF      -   decision forest model
  XY      -   test set
  NPoints -   test set size

  RESULT:
  percent of incorrectly classified cases.
  Zero if model solves regression task.
  ************************************************************************ *)
function DFRelClsError(const df: TDecisionForest; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
begin
  Result := AP_Float(DFClsError(df, xy, NPoints)) / NPoints;
end;

(* ************************************************************************
  Average cross-entropy (in bits per element) on the test set

  INPUT PARAMETERS:
  DF      -   decision forest model
  XY      -   test set
  NPoints -   test set size

  RESULT:
  CrossEntropy/(NPoints*LN(2)).
  Zero if model solves regression task.
  ************************************************************************ *)
function DFAvgCE(const df: TDecisionForest; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  x: TLVec;
  y: TLVec;
  i: TLInt;
  j: TLInt;
  k: TLInt;
  TmpI: TLInt;
begin
  SetLength(x, df.NVars  );
  SetLength(y, df.NClasses  );
  Result := 0;
  i := 0;
  while i <= NPoints - 1 do
    begin
      APVMove(@x[0], 0, df.NVars - 1, @xy[i][0], 0, df.NVars - 1);
      DFProcess(df, x, y);
      if df.NClasses > 1 then
        begin

          //
          // classification-specific code
          //
          k := Round(xy[i, df.NVars]);
          TmpI := 0;
          j := 1;
          while j <= df.NClasses - 1 do
            begin
              if AP_FP_Greater(y[j], y[TmpI]) then
                begin
                  TmpI := j;
                end;
              inc(j);
            end;
          if AP_FP_NEq(y[k], 0) then
            begin
              Result := Result - ln(y[k]);
            end
          else
            begin
              Result := Result - ln(MinRealNumber);
            end;
        end;
      inc(i);
    end;
  Result := Result / NPoints;
end;

(* ************************************************************************
  RMS error on the test set

  INPUT PARAMETERS:
  DF      -   decision forest model
  XY      -   test set
  NPoints -   test set size

  RESULT:
  root mean square error.
  Its meaning for regression task is obvious. As for
  classification task, RMS error means error when estimating posterior
  probabilities.
  ************************************************************************ *)
function DFRMSError(const df: TDecisionForest; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  x: TLVec;
  y: TLVec;
  i: TLInt;
  j: TLInt;
  k: TLInt;
  TmpI: TLInt;
begin
  SetLength(x, df.NVars  );
  SetLength(y, df.NClasses  );
  Result := 0;
  i := 0;
  while i <= NPoints - 1 do
    begin
      APVMove(@x[0], 0, df.NVars - 1, @xy[i][0], 0, df.NVars - 1);
      DFProcess(df, x, y);
      if df.NClasses > 1 then
        begin

          //
          // classification-specific code
          //
          k := Round(xy[i, df.NVars]);
          TmpI := 0;
          j := 1;
          while j <= df.NClasses - 1 do
            begin
              if AP_FP_Greater(y[j], y[TmpI]) then
                begin
                  TmpI := j;
                end;
              inc(j);
            end;
          j := 0;
          while j <= df.NClasses - 1 do
            begin
              if j = k then
                begin
                  Result := Result + AP_Sqr(y[j] - 1);
                end
              else
                begin
                  Result := Result + AP_Sqr(y[j]);
                end;
              inc(j);
            end;
        end
      else
        begin

          //
          // regression-specific code
          //
          Result := Result + AP_Sqr(y[0] - xy[i, df.NVars]);
        end;
      inc(i);
    end;
  Result := Sqrt(Result / (NPoints * df.NClasses));
end;

(* ************************************************************************
  Average error on the test set

  INPUT PARAMETERS:
  DF      -   decision forest model
  XY      -   test set
  NPoints -   test set size

  RESULT:
  Its meaning for regression task is obvious. As for
  classification task, it means average error when estimating posterior
  probabilities.
  ************************************************************************ *)
function DFAvgError(const df: TDecisionForest; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  x: TLVec;
  y: TLVec;
  i: TLInt;
  j: TLInt;
  k: TLInt;
begin
  SetLength(x, df.NVars  );
  SetLength(y, df.NClasses  );
  Result := 0;
  i := 0;
  while i <= NPoints - 1 do
    begin
      APVMove(@x[0], 0, df.NVars - 1, @xy[i][0], 0, df.NVars - 1);
      DFProcess(df, x, y);
      if df.NClasses > 1 then
        begin

          //
          // classification-specific code
          //
          k := Round(xy[i, df.NVars]);
          j := 0;
          while j <= df.NClasses - 1 do
            begin
              if j = k then
                begin
                  Result := Result + AbsReal(y[j] - 1);
                end
              else
                begin
                  Result := Result + AbsReal(y[j]);
                end;
              inc(j);
            end;
        end
      else
        begin

          //
          // regression-specific code
          //
          Result := Result + AbsReal(y[0] - xy[i, df.NVars]);
        end;
      inc(i);
    end;
  Result := Result / (NPoints * df.NClasses);
end;

(* ************************************************************************
  Average relative error on the test set

  INPUT PARAMETERS:
  DF      -   decision forest model
  XY      -   test set
  NPoints -   test set size

  RESULT:
  Its meaning for regression task is obvious. As for
  classification task, it means average relative error when estimating
  posterior probability of belonging to the correct class.
  ************************************************************************ *)
function DFAvgRelError(const df: TDecisionForest; const xy: TLMatrix;
  NPoints: TLInt): TLFloat;
var
  x: TLVec;
  y: TLVec;
  RelCnt: TLInt;
  i: TLInt;
  j: TLInt;
  k: TLInt;
begin
  SetLength(x, df.NVars  );
  SetLength(y, df.NClasses  );
  Result := 0;
  RelCnt := 0;
  i := 0;
  while i <= NPoints - 1 do
    begin
      APVMove(@x[0], 0, df.NVars - 1, @xy[i][0], 0, df.NVars - 1);
      DFProcess(df, x, y);
      if df.NClasses > 1 then
        begin

          //
          // classification-specific code
          //
          k := Round(xy[i, df.NVars]);
          j := 0;
          while j <= df.NClasses - 1 do
            begin
              if j = k then
                begin
                  Result := Result + AbsReal(y[j] - 1);
                  RelCnt := RelCnt + 1;
                end;
              inc(j);
            end;
        end
      else
        begin

          //
          // regression-specific code
          //
          if AP_FP_NEq(xy[i, df.NVars], 0) then
            begin
              Result := Result + AbsReal((y[0] - xy[i, df.NVars]) / xy[i, df.NVars]);
              RelCnt := RelCnt + 1;
            end;
        end;
      inc(i);
    end;
  if RelCnt > 0 then
    begin
      Result := Result / RelCnt;
    end;
end;

(* ************************************************************************
  Copying of TDecisionForest strucure

  INPUT PARAMETERS:
  DF1 -   original

  OUTPUT PARAMETERS:
  DF2 -   copy
  ************************************************************************ *)
procedure DFCopy(const DF1: TDecisionForest; var DF2: TDecisionForest);
begin
  DF2.NVars := DF1.NVars;
  DF2.NClasses := DF1.NClasses;
  DF2.NTrees := DF1.NTrees;
  DF2.BufSize := DF1.BufSize;
  SetLength(DF2.Trees, DF1.BufSize  );
  APVMove(@DF2.Trees[0], 0, DF1.BufSize - 1, @DF1.Trees[0], 0, DF1.BufSize - 1);
end;

(* ************************************************************************
  Serialization of TDecisionForest strucure

  INPUT PARAMETERS:
  DF      -   original

  OUTPUT PARAMETERS:
  RA      -   array of real numbers which stores decision forest,
  array[0..RLen-1]
  RLen    -   RA lenght
  ************************************************************************ *)
procedure DFSerialize(const df: TDecisionForest; var RA: TLVec; var RLen: TLInt);
begin
  SetLength(RA, df.BufSize + 5  );
  RA[0] := DFVNum;
  RA[1] := df.NVars;
  RA[2] := df.NClasses;
  RA[3] := df.NTrees;
  RA[4] := df.BufSize;
  APVMove(@RA[0], 5, 5 + df.BufSize - 1, @df.Trees[0], 0, df.BufSize - 1);
  RLen := 5 + df.BufSize;
end;

(* ************************************************************************
  Unserialization of TDecisionForest strucure

  INPUT PARAMETERS:
  RA      -   real array which stores decision forest

  OUTPUT PARAMETERS:
  DF      -   restored structure
  ************************************************************************ *)
procedure DFUnserialize(const RA: TLVec; var df: TDecisionForest);
begin
  Assert(Round(RA[0]) = DFVNum, 'DFUnserialize: incorrect array!');
  df.NVars := Round(RA[1]);
  df.NClasses := Round(RA[2]);
  df.NTrees := Round(RA[3]);
  df.BufSize := Round(RA[4]);
  SetLength(df.Trees, df.BufSize  );
  APVMove(@df.Trees[0], 0, df.BufSize - 1, @RA[0], 5, 5 + df.BufSize - 1);
end;

(* ************************************************************************
  Classification error
  ************************************************************************ *)
function DFClsError(const df: TDecisionForest; const xy: TLMatrix;
  NPoints: TLInt): TLInt;
var
  x: TLVec;
  y: TLVec;
  i: TLInt;
  j: TLInt;
  k: TLInt;
  TmpI: TLInt;
begin
  if df.NClasses <= 1 then
    begin
      Result := 0;
      Exit;
    end;
  SetLength(x, df.NVars  );
  SetLength(y, df.NClasses  );
  Result := 0;
  i := 0;
  while i <= NPoints - 1 do
    begin
      APVMove(@x[0], 0, df.NVars - 1, @xy[i][0], 0, df.NVars - 1);
      DFProcess(df, x, y);
      k := Round(xy[i, df.NVars]);
      TmpI := 0;
      j := 1;
      while j <= df.NClasses - 1 do
        begin
          if AP_FP_Greater(y[j], y[TmpI]) then
            begin
              TmpI := j;
            end;
          inc(j);
        end;
      if TmpI <> k then
        begin
          Result := Result + 1;
        end;
      inc(i);
    end;
end;

(* ************************************************************************
  Internal subroutine for processing one decision tree starting at Offs
  ************************************************************************ *)
procedure DFProcessInternal(const df: TDecisionForest; Offs: TLInt;
  const x: TLVec; var y: TLVec);
var
  k: TLInt;
  idx: TLInt;
begin

  //
  // Set pointer to the root
  //
  k := Offs + 1;

  //
  // Navigate through the tree
  //
  while True do
    begin
      if AP_FP_Eq(df.Trees[k], -1) then
        begin
          if df.NClasses = 1 then
            begin
              y[0] := y[0] + df.Trees[k + 1];
            end
          else
            begin
              idx := Round(df.Trees[k + 1]);
              y[idx] := y[idx] + 1;
            end;
          Break;
        end;
      if AP_FP_Less(x[Round(df.Trees[k])], df.Trees[k + 1]) then
        begin
          k := k + InnerNodeWidth;
        end
      else
        begin
          k := Offs + Round(df.Trees[k + 2]);
        end;
    end;
end;

(* ************************************************************************
  Builds one decision tree. Just a wrapper for the DFBuildTreeRec.
  ************************************************************************ *)
procedure DFBuildTree(const xy: TLMatrix; NPoints: TLInt;
  NVars: TLInt; NClasses: TLInt; NFeatures: TLInt;
  NVarsInPool: TLInt; Flags: TLInt;
  var Bufs: TDFInternalBuffers);
var
  NumProcessed: TLInt;
  i: TLInt;
begin
  Assert(NPoints > 0);

  //
  // Prepare IdxBuf. It stores indices of the training set elements.
  // When training set is being split, contents of IdxBuf is
  // correspondingly reordered so we can know which elements belong
  // to which branch of decision tree.
  //
  i := 0;
  while i <= NPoints - 1 do
    begin
      Bufs.IdxBuf[i] := i;
      inc(i);
    end;

  //
  // Recursive procedure
  //
  NumProcessed := 1;
  DFBuildTreeRec(xy, NPoints, NVars, NClasses, NFeatures, NVarsInPool, Flags,
    NumProcessed, 0, NPoints - 1, Bufs);
  Bufs.TreeBuf[0] := NumProcessed;
end;

(* ************************************************************************
  Builds one decision tree (internal recursive subroutine)

  Parameters:
  TreeBuf     -   large enough array, at least TreeSize
  IdxBuf      -   at least NPoints elements
  TmpBufR     -   at least NPoints
  TmpBufR2    -   at least NPoints
  TmpBufI     -   at least NPoints
  TmpBufI2    -   at least NPoints+1
  ************************************************************************ *)
procedure DFBuildTreeRec(const xy: TLMatrix; NPoints: TLInt;
  NVars: TLInt; NClasses: TLInt; NFeatures: TLInt;
  NVarsInPool: TLInt; Flags: TLInt;
  var NumProcessed: TLInt; idx1: TLInt; idx2: TLInt;
  var Bufs: TDFInternalBuffers);
var
  i: TLInt;
  j: TLInt;
  k: TLInt;
  BFlag: Boolean;
  i1: TLInt;
  i2: TLInt;
  Info: TLInt;
  SL: TLFloat;
  SR: TLFloat;
  w: TLFloat;
  IdxBest: TLInt;
  EBest: TLFloat;
  TBest: TLFloat;
  VarCur: TLInt;
  s: TLFloat;
  v: TLFloat;
  v1: TLFloat;
  v2: TLFloat;
  Threshold: TLFloat;
  OldNP: TLInt;
  CurRMS: TLFloat;
  UseEVS: Boolean;
begin
  Assert(NPoints > 0);
  Assert(idx2 >= idx1);
  UseEVS := Flags div DFUseEVS mod 2 <> 0;

  //
  // Leaf node
  //
  if idx2 = idx1 then
    begin
      Bufs.TreeBuf[NumProcessed] := -1;
      Bufs.TreeBuf[NumProcessed + 1] := xy[Bufs.IdxBuf[idx1], NVars];
      NumProcessed := NumProcessed + LeafNodeWidth;
      Exit;
    end;

  //
  // Non-leaf node.
  // Select random variable, prepare split:
  // 1. prepare default solution - no splitting, class at random
  // 2. investigate possible splits, compare with default/best
  //
  IdxBest := -1;
  if NClasses > 1 then
    begin

      //
      // default solution for classification
      //
      i := 0;
      while i <= NClasses - 1 do
        begin
          Bufs.ClassIBuf[i] := 0;
          inc(i);
        end;
      s := idx2 - idx1 + 1;
      i := idx1;
      while i <= idx2 do
        begin
          j := Round(xy[Bufs.IdxBuf[i], NVars]);
          Bufs.ClassIBuf[j] := Bufs.ClassIBuf[j] + 1;
          inc(i);
        end;
      EBest := 0;
      i := 0;
      while i <= NClasses - 1 do
        begin
          EBest := EBest + Bufs.ClassIBuf[i] * AP_Sqr(1 - Bufs.ClassIBuf[i] / s) +
            (s - Bufs.ClassIBuf[i]) * AP_Sqr(Bufs.ClassIBuf[i] / s);
          inc(i);
        end;
      EBest := Sqrt(EBest / (NClasses * (idx2 - idx1 + 1)));
    end
  else
    begin

      //
      // default solution for regression
      //
      v := 0;
      i := idx1;
      while i <= idx2 do
        begin
          v := v + xy[Bufs.IdxBuf[i], NVars];
          inc(i);
        end;
      v := v / (idx2 - idx1 + 1);
      EBest := 0;
      i := idx1;
      while i <= idx2 do
        begin
          EBest := EBest + AP_Sqr(xy[Bufs.IdxBuf[i], NVars] - v);
          inc(i);
        end;
      EBest := Sqrt(EBest / (idx2 - idx1 + 1));
    end;
  i := 0;
  while i <= Min(NFeatures, NVarsInPool) - 1 do
    begin

      //
      // select variables from pool
      //
      j := i + RandomInteger(NVarsInPool - i);
      k := Bufs.VarPool[i];
      Bufs.VarPool[i] := Bufs.VarPool[j];
      Bufs.VarPool[j] := k;
      VarCur := Bufs.VarPool[i];

      //
      // load variable values to working array
      //
      // apply EVS preprocessing: if all variable values are same,
      // variable is excluded from pool.
      //
      // This is necessary for binary pre-splits (see later) to work.
      //
      j := idx1;
      while j <= idx2 do
        begin
          Bufs.TmpBufR[j - idx1] := xy[Bufs.IdxBuf[j], VarCur];
          inc(j);
        end;
      if UseEVS then
        begin
          BFlag := False;
          v := Bufs.TmpBufR[0];
          j := 0;
          while j <= idx2 - idx1 do
            begin
              if AP_FP_NEq(Bufs.TmpBufR[j], v) then
                begin
                  BFlag := True;
                  Break;
                end;
              inc(j);
            end;
          if not BFlag then
            begin

              //
              // exclude variable from pool,
              // go to the next iteration.
              // I is not increased.
              //
              k := Bufs.VarPool[i];
              Bufs.VarPool[i] := Bufs.VarPool[NVarsInPool - 1];
              Bufs.VarPool[NVarsInPool - 1] := k;
              NVarsInPool := NVarsInPool - 1;
              Continue;
            end;
        end;

      //
      // load labels to working array
      //
      if NClasses > 1 then
        begin
          j := idx1;
          while j <= idx2 do
            begin
              Bufs.TmpBufI[j - idx1] := Round(xy[Bufs.IdxBuf[j], NVars]);
              inc(j);
            end;
        end
      else
        begin
          j := idx1;
          while j <= idx2 do
            begin
              Bufs.TmpBufR2[j - idx1] := xy[Bufs.IdxBuf[j], NVars];
              inc(j);
            end;
        end;

      //
      // calculate split
      //
      if UseEVS and Bufs.EVSBin[VarCur] then
        begin

          //
          // Pre-calculated splits for binary variables.
          // Threshold is already known, just calculate RMS error
          //
          Threshold := Bufs.EVSSplits[VarCur];
          if NClasses > 1 then
            begin

              //
              // classification-specific code
              //
              j := 0;
              while j <= 2 * NClasses - 1 do
                begin
                  Bufs.ClassIBuf[j] := 0;
                  inc(j);
                end;
              SL := 0;
              SR := 0;
              j := 0;
              while j <= idx2 - idx1 do
                begin
                  k := Bufs.TmpBufI[j];
                  if AP_FP_Less(Bufs.TmpBufR[j], Threshold) then
                    begin
                      Bufs.ClassIBuf[k] := Bufs.ClassIBuf[k] + 1;
                      SL := SL + 1;
                    end
                  else
                    begin
                      Bufs.ClassIBuf[k + NClasses] := Bufs.ClassIBuf[k + NClasses] + 1;
                      SR := SR + 1;
                    end;
                  inc(j);
                end;
              Assert(AP_FP_NEq(SL, 0) and AP_FP_NEq(SR, 0),
                'DFBuildTreeRec: something strange!');
              CurRMS := 0;
              j := 0;
              while j <= NClasses - 1 do
                begin
                  w := Bufs.ClassIBuf[j];
                  CurRMS := CurRMS + w * AP_Sqr(w / SL - 1);
                  CurRMS := CurRMS + (SL - w) * AP_Sqr(w / SL);
                  w := Bufs.ClassIBuf[NClasses + j];
                  CurRMS := CurRMS + w * AP_Sqr(w / SR - 1);
                  CurRMS := CurRMS + (SR - w) * AP_Sqr(w / SR);
                  inc(j);
                end;
              CurRMS := Sqrt(CurRMS / (NClasses * (idx2 - idx1 + 1)));
            end
          else
            begin

              //
              // regression-specific code
              //
              SL := 0;
              SR := 0;
              v1 := 0;
              v2 := 0;
              j := 0;
              while j <= idx2 - idx1 do
                begin
                  if AP_FP_Less(Bufs.TmpBufR[j], Threshold) then
                    begin
                      v1 := v1 + Bufs.TmpBufR2[j];
                      SL := SL + 1;
                    end
                  else
                    begin
                      v2 := v2 + Bufs.TmpBufR2[j];
                      SR := SR + 1;
                    end;
                  inc(j);
                end;
              Assert(AP_FP_NEq(SL, 0) and AP_FP_NEq(SR, 0),
                'DFBuildTreeRec: something strange!');
              v1 := v1 / SL;
              v2 := v2 / SR;
              CurRMS := 0;
              j := 0;
              while j <= idx2 - idx1 do
                begin
                  if AP_FP_Less(Bufs.TmpBufR[j], Threshold) then
                    begin
                      CurRMS := CurRMS + AP_Sqr(v1 - Bufs.TmpBufR2[j]);
                    end
                  else
                    begin
                      CurRMS := CurRMS + AP_Sqr(v2 - Bufs.TmpBufR2[j]);
                    end;
                  inc(j);
                end;
              CurRMS := Sqrt(CurRMS / (idx2 - idx1 + 1));
            end;
          Info := 1;
        end
      else
        begin

          //
          // Generic splits
          //
          if NClasses > 1 then
            begin
              DFSplitC(Bufs.TmpBufR, Bufs.TmpBufI, Bufs.ClassIBuf, idx2 - idx1 + 1,
                NClasses, DFUseStrongSplits, Info, Threshold, CurRMS);
            end
          else
            begin
              DFSplitR(Bufs.TmpBufR, Bufs.TmpBufR2, idx2 - idx1 + 1,
                DFUseStrongSplits, Info, Threshold, CurRMS);
            end;
        end;
      if Info > 0 then
        begin
          if AP_FP_Less_Eq(CurRMS, EBest) then
            begin
              EBest := CurRMS;
              IdxBest := VarCur;
              TBest := Threshold;
            end;
        end;

      //
      // Next iteration
      //
      i := i + 1;
    end;

  //
  // to split or not to split
  //
  if IdxBest < 0 then
    begin

      //
      // All values are same, cannot split.
      //
      Bufs.TreeBuf[NumProcessed] := -1;
      if NClasses > 1 then
        begin

          //
          // Select random class label (randomness allows us to
          // approximate distribution of the classes)
          //
          Bufs.TreeBuf[NumProcessed + 1] :=
            Round(xy[Bufs.IdxBuf[idx1 + RandomInteger(idx2 - idx1 + 1)], NVars]);
        end
      else
        begin

          //
          // Select average (for regression task).
          //
          v := 0;
          i := idx1;
          while i <= idx2 do
            begin
              v := v + xy[Bufs.IdxBuf[i], NVars] / (idx2 - idx1 + 1);
              inc(i);
            end;
          Bufs.TreeBuf[NumProcessed + 1] := v;
        end;
      NumProcessed := NumProcessed + LeafNodeWidth;
    end
  else
    begin

      //
      // we can split
      //
      Bufs.TreeBuf[NumProcessed] := IdxBest;
      Bufs.TreeBuf[NumProcessed + 1] := TBest;
      i1 := idx1;
      i2 := idx2;
      while i1 <= i2 do
        begin

          //
          // Reorder indices so that left partition is in [Idx1..I1-1],
          // and right partition is in [I2+1..Idx2]
          //
          if AP_FP_Less(xy[Bufs.IdxBuf[i1], IdxBest], TBest) then
            begin
              i1 := i1 + 1;
              Continue;
            end;
          if AP_FP_Greater_Eq(xy[Bufs.IdxBuf[i2], IdxBest], TBest) then
            begin
              i2 := i2 - 1;
              Continue;
            end;
          j := Bufs.IdxBuf[i1];
          Bufs.IdxBuf[i1] := Bufs.IdxBuf[i2];
          Bufs.IdxBuf[i2] := j;
          i1 := i1 + 1;
          i2 := i2 - 1;
        end;
      OldNP := NumProcessed;
      NumProcessed := NumProcessed + InnerNodeWidth;
      DFBuildTreeRec(xy, NPoints, NVars, NClasses, NFeatures, NVarsInPool, Flags,
        NumProcessed, idx1, i1 - 1, Bufs);
      Bufs.TreeBuf[OldNP + 2] := NumProcessed;
      DFBuildTreeRec(xy, NPoints, NVars, NClasses, NFeatures, NVarsInPool, Flags,
        NumProcessed, i2 + 1, idx2, Bufs);
    end;
end;

(* ************************************************************************
  Makes weak split on attribute
  ************************************************************************ *)
procedure DFWeakSplitI(var x: TLVec; var y: TLIVec;
  n: TLInt; NClasses: TLInt; var Info: TLInt;
  var Threshold: TLFloat; var E: TLFloat);
var
  i: TLInt;
  NEq: TLInt;
  NLess: TLInt;
  NGreater: TLInt;
begin
  TagSortFastI(x, y, n);
  if n mod 2 = 1 then
    begin

      //
      // odd number of elements
      //
      Threshold := x[n div 2];
    end
  else
    begin

      //
      // even number of elements.
      //
      // if two closest to the middle of the array are equal,
      // we will select one of them (to avoid possible problems with
      // floating point errors).
      // we will select halfsum otherwise.
      //
      if AP_FP_Eq(x[n div 2 - 1], x[n div 2]) then
        begin
          Threshold := x[n div 2 - 1];
        end
      else
        begin
          Threshold := 0.5 * (x[n div 2 - 1] + x[n div 2]);
        end;
    end;
  NEq := 0;
  NLess := 0;
  NGreater := 0;
  i := 0;
  while i <= n - 1 do
    begin
      if AP_FP_Less(x[i], Threshold) then
        begin
          NLess := NLess + 1;
        end;
      if AP_FP_Eq(x[i], Threshold) then
        begin
          NEq := NEq + 1;
        end;
      if AP_FP_Greater(x[i], Threshold) then
        begin
          NGreater := NGreater + 1;
        end;
      inc(i);
    end;
  if (NLess = 0) and (NGreater = 0) then
    begin
      Info := -3;
    end
  else
    begin
      if NEq <> 0 then
        begin
          if NLess < NGreater then
            begin
              Threshold := 0.5 * (x[NLess + NEq - 1] + x[NLess + NEq]);
            end
          else
            begin
              Threshold := 0.5 * (x[NLess - 1] + x[NLess]);
            end;
        end;
      Info := 1;
      E := 0;
    end;
end;

(* ************************************************************************
  Makes split on attribute
  ************************************************************************ *)
procedure DFSplitC(var x: TLVec; var c: TLIVec;
  var CntBuf: TLIVec; n: TLInt; NC: TLInt;
  Flags: TLInt; var Info: TLInt; var Threshold: TLFloat;
  var E: TLFloat);
var
  i: TLInt;
  NEq: TLInt;
  NLess: TLInt;
  NGreater: TLInt;
  q: TLInt;
  QMin: TLInt;
  QMax: TLInt;
  QCnt: TLInt;
  CurSplit: TLFloat;
  NLeft: TLInt;
  v: TLFloat;
  CurE: TLFloat;
  w: TLFloat;
  SL: TLFloat;
  SR: TLFloat;
begin
  TagSortFastI(x, c, n);
  E := MaxRealNumber;
  Threshold := 0.5 * (x[0] + x[n - 1]);
  Info := -3;
  if Flags div DFUseStrongSplits mod 2 = 0 then
    begin

      //
      // weak splits, split at half
      //
      QCnt := 2;
      QMin := 1;
      QMax := 1;
    end
  else
    begin

      //
      // strong splits: choose best quartile
      //
      QCnt := 4;
      QMin := 1;
      QMax := 3;
    end;
  q := QMin;
  while q <= QMax do
    begin
      CurSplit := x[n * q div QCnt];
      NEq := 0;
      NLess := 0;
      NGreater := 0;
      i := 0;
      while i <= n - 1 do
        begin
          if AP_FP_Less(x[i], CurSplit) then
            begin
              NLess := NLess + 1;
            end;
          if AP_FP_Eq(x[i], CurSplit) then
            begin
              NEq := NEq + 1;
            end;
          if AP_FP_Greater(x[i], CurSplit) then
            begin
              NGreater := NGreater + 1;
            end;
          inc(i);
        end;
      Assert(NEq <> 0, 'DFSplitR: NEq=0, something strange!!!');
      if (NLess <> 0) or (NGreater <> 0) then
        begin

          //
          // set threshold between two partitions, with
          // some tweaking to avoid problems with floating point
          // arithmetics.
          //
          // The problem is that when you calculates C = 0.5*(A+B) there
          // can be no C which lies strictly between A and B (for example,
          // there is no floating point number which is
          // greater than 1 and less than 1+eps). In such situations
          // we choose right side as theshold (remember that
          // points which lie on threshold falls to the right side).
          //
          if NLess < NGreater then
            begin
              CurSplit := 0.5 * (x[NLess + NEq - 1] + x[NLess + NEq]);
              NLeft := NLess + NEq;
              if AP_FP_Less_Eq(CurSplit, x[NLess + NEq - 1]) then
                begin
                  CurSplit := x[NLess + NEq];
                end;
            end
          else
            begin
              CurSplit := 0.5 * (x[NLess - 1] + x[NLess]);
              NLeft := NLess;
              if AP_FP_Less_Eq(CurSplit, x[NLess - 1]) then
                begin
                  CurSplit := x[NLess];
                end;
            end;
          Info := 1;
          CurE := 0;
          i := 0;
          while i <= 2 * NC - 1 do
            begin
              CntBuf[i] := 0;
              inc(i);
            end;
          i := 0;
          while i <= NLeft - 1 do
            begin
              CntBuf[c[i]] := CntBuf[c[i]] + 1;
              inc(i);
            end;
          i := NLeft;
          while i <= n - 1 do
            begin
              CntBuf[NC + c[i]] := CntBuf[NC + c[i]] + 1;
              inc(i);
            end;
          SL := NLeft;
          SR := n - NLeft;
          v := 0;
          i := 0;
          while i <= NC - 1 do
            begin
              w := CntBuf[i];
              v := v + w * AP_Sqr(w / SL - 1);
              v := v + (SL - w) * AP_Sqr(w / SL);
              w := CntBuf[NC + i];
              v := v + w * AP_Sqr(w / SR - 1);
              v := v + (SR - w) * AP_Sqr(w / SR);
              inc(i);
            end;
          CurE := Sqrt(v / (NC * n));
          if AP_FP_Less(CurE, E) then
            begin
              Threshold := CurSplit;
              E := CurE;
            end;
        end;
      inc(q);
    end;
end;

(* ************************************************************************
  Makes split on attribute
  ************************************************************************ *)
procedure DFSplitR(var x: TLVec; var y: TLVec; n: TLInt;
  Flags: TLInt; var Info: TLInt; var Threshold: TLFloat;
  var E: TLFloat);
var
  i: TLInt;
  NEq: TLInt;
  NLess: TLInt;
  NGreater: TLInt;
  q: TLInt;
  QMin: TLInt;
  QMax: TLInt;
  QCnt: TLInt;
  CurSplit: TLFloat;
  NLeft: TLInt;
  v: TLFloat;
  CurE: TLFloat;
begin
  TagSortFastR(x, y, n);
  E := MaxRealNumber;
  Threshold := 0.5 * (x[0] + x[n - 1]);
  Info := -3;
  if Flags div DFUseStrongSplits mod 2 = 0 then
    begin

      //
      // weak splits, split at half
      //
      QCnt := 2;
      QMin := 1;
      QMax := 1;
    end
  else
    begin

      //
      // strong splits: choose best quartile
      //
      QCnt := 4;
      QMin := 1;
      QMax := 3;
    end;
  q := QMin;
  while q <= QMax do
    begin
      CurSplit := x[n * q div QCnt];
      NEq := 0;
      NLess := 0;
      NGreater := 0;
      i := 0;
      while i <= n - 1 do
        begin
          if AP_FP_Less(x[i], CurSplit) then
            begin
              NLess := NLess + 1;
            end;
          if AP_FP_Eq(x[i], CurSplit) then
            begin
              NEq := NEq + 1;
            end;
          if AP_FP_Greater(x[i], CurSplit) then
            begin
              NGreater := NGreater + 1;
            end;
          inc(i);
        end;
      Assert(NEq <> 0, 'DFSplitR: NEq=0, something strange!!!');
      if (NLess <> 0) or (NGreater <> 0) then
        begin

          //
          // set threshold between two partitions, with
          // some tweaking to avoid problems with floating point
          // arithmetics.
          //
          // The problem is that when you calculates C = 0.5*(A+B) there
          // can be no C which lies strictly between A and B (for example,
          // there is no floating point number which is
          // greater than 1 and less than 1+eps). In such situations
          // we choose right side as theshold (remember that
          // points which lie on threshold falls to the right side).
          //
          if NLess < NGreater then
            begin
              CurSplit := 0.5 * (x[NLess + NEq - 1] + x[NLess + NEq]);
              NLeft := NLess + NEq;
              if AP_FP_Less_Eq(CurSplit, x[NLess + NEq - 1]) then
                begin
                  CurSplit := x[NLess + NEq];
                end;
            end
          else
            begin
              CurSplit := 0.5 * (x[NLess - 1] + x[NLess]);
              NLeft := NLess;
              if AP_FP_Less_Eq(CurSplit, x[NLess - 1]) then
                begin
                  CurSplit := x[NLess];
                end;
            end;
          Info := 1;
          CurE := 0;
          v := 0;
          i := 0;
          while i <= NLeft - 1 do
            begin
              v := v + y[i];
              inc(i);
            end;
          v := v / NLeft;
          i := 0;
          while i <= NLeft - 1 do
            begin
              CurE := CurE + AP_Sqr(y[i] - v);
              inc(i);
            end;
          v := 0;
          i := NLeft;
          while i <= n - 1 do
            begin
              v := v + y[i];
              inc(i);
            end;
          v := v / (n - NLeft);
          i := NLeft;
          while i <= n - 1 do
            begin
              CurE := CurE + AP_Sqr(y[i] - v);
              inc(i);
            end;
          CurE := Sqrt(CurE / n);
          if AP_FP_Less(CurE, E) then
            begin
              Threshold := CurSplit;
              E := CurE;
            end;
        end;
      inc(q);
    end;
end;
