{ ****************************************************************************** }
{ * Normalizes direction/step pair support, by QQ 600585@qq.com                * }
{ ****************************************************************************** }
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
  TLinMinState = record
    BRACKT: Boolean;
    STAGE1: Boolean;
    INFOC: TLInt;
    DG: TLFloat;
    DGM: TLFloat;
    DGINIT: TLFloat;
    DGTEST: TLFloat;
    DGX: TLFloat;
    DGXM: TLFloat;
    DGY: TLFloat;
    DGYM: TLFloat;
    FINIT: TLFloat;
    FTEST1: TLFloat;
    FM: TLFloat;
    fx: TLFloat;
    FXM: TLFloat;
    fy: TLFloat;
    FYM: TLFloat;
    STX: TLFloat;
    STY: TLFloat;
    STMIN: TLFloat;
    STMAX: TLFloat;
    width: TLFloat;
    WIDTH1: TLFloat;
    XTRAPF: TLFloat;
  end;

procedure LinMinNormalizeD(var d: TLVec; var Stp: TLFloat; n: TLInt); forward;

procedure MCSRCH(const n: TLInt; var x: TLVec;
  var f: TLFloat; var g: TLVec; const s: TLVec;
  var Stp: TLFloat; StpMax: TLFloat; var Info: TLInt;
  var NFEV: TLInt; var WA: TLVec; var State: TLinMinState; var Stage: TLInt); forward;


const
  LM_FTOL      = 0.001;
  LM_XTOL      = 100 * MachineEpsilon;
  LM_GTOL      = 0.3;
  LM_MAXFEV    = 20;
  LM_STPMIN    = 1.0E-50;
  LM_DefSTPMAX = 1.0E+50;

procedure MCSTEP(var STX: TLFloat; var fx: TLFloat; var dx: TLFloat;
  var STY: TLFloat; var fy: TLFloat; var dy: TLFloat;
  var Stp: TLFloat; const FP: TLFloat; const DP: TLFloat;
  var BRACKT: Boolean; const STMIN: TLFloat; const STMAX: TLFloat;
  var Info: TLInt); forward;


(* ************************************************************************
  Normalizes direction/step pair: makes |D|=1, scales Stp.
  If |D|=0, it returns, leavind D/Stp unchanged.
  ************************************************************************ *)
procedure LinMinNormalizeD(var d: TLVec; var Stp: TLFloat;
  n: TLInt);
var
  mx: TLFloat;
  s : TLFloat;
  i : TLInt;
begin

  //
  // first, scale D to avoid underflow/overflow durng squaring
  //
  mx := 0;
  i := 0;
  while i <= n - 1 do
    begin
      mx := Max(mx, AbsReal(d[i]));
      inc(i);
    end;
  if AP_FP_Eq(mx, 0) then
    begin
      Exit;
    end;
  s := 1 / mx;
  APVMul(@d[0], 0, n - 1, s);
  Stp := Stp / s;

  //
  // normalize D
  //
  s := APVDotProduct(@d[0], 0, n - 1, @d[0], 0, n - 1);
  s := 1 / Sqrt(s);
  APVMul(@d[0], 0, n - 1, s);
  Stp := Stp / s;
end;

procedure MCSRCH(const n: TLInt; var x: TLVec;
  var f: TLFloat; var g: TLVec; const s: TLVec;
  var Stp: TLFloat; StpMax: TLFloat; var Info: TLInt;
  var NFEV: TLInt; var WA: TLVec; var State: TLinMinState;
  var Stage: TLInt);
var
  v   : TLFloat;
  P5  : TLFloat;
  P66 : TLFloat;
  Zero: TLFloat;
begin

  //
  // init
  //
  P5 := 0.5;
  P66 := 0.66;
  State.XTRAPF := 4.0;
  Zero := 0;
  if AP_FP_Eq(StpMax, 0) then
    begin
      StpMax := LM_DefSTPMAX;
    end;
  if AP_FP_Less(Stp, LM_STPMIN) then
    begin
      Stp := LM_STPMIN;
    end;
  if AP_FP_Greater(Stp, StpMax) then
    begin
      Stp := StpMax;
    end;

  //
  // Main cycle
  //
  while True do
    begin
      if Stage = 0 then
        begin

          //
          // NEXT
          //
          Stage := 2;
          Continue;
        end;
      if Stage = 2 then
        begin
          State.INFOC := 1;
          Info := 0;

          //
          // CHECK THE INPUT PARAMETERS FOR ERRORS.
          //
          if (n <= 0) or AP_FP_Less_Eq(Stp, 0) or AP_FP_Less(LM_FTOL, 0) or
            AP_FP_Less(LM_GTOL, Zero) or AP_FP_Less(LM_XTOL, Zero) or
            AP_FP_Less(LM_STPMIN, Zero) or AP_FP_Less(StpMax, LM_STPMIN) or (LM_MAXFEV <= 0)
          then
            begin
              Stage := 0;
              Exit;
            end;

          //
          // COMPUTE THE INITIAL GRADIENT IN THE SEARCH DIRECTION
          // AND CHECK THAT S IS A DESCENT DIRECTION.
          //
          v := APVDotProduct(@g[0], 0, n - 1, @s[0], 0, n - 1);
          State.DGINIT := v;
          if AP_FP_Greater_Eq(State.DGINIT, 0) then
            begin
              Stage := 0;
              Exit;
            end;

          //
          // INITIALIZE LOCAL VARIABLES.
          //
          State.BRACKT := False;
          State.STAGE1 := True;
          NFEV := 0;
          State.FINIT := f;
          State.DGTEST := LM_FTOL * State.DGINIT;
          State.width := StpMax - LM_STPMIN;
          State.WIDTH1 := State.width / P5;
          APVMove(@WA[0], 0, n - 1, @x[0], 0, n - 1);

          //
          // THE VARIABLES STX, FX, DGX CONTAIN THE VALUES OF THE STEP,
          // FUNCTION, AND DIRECTIONAL DERIVATIVE AT THE BEST STEP.
          // THE VARIABLES STY, FY, DGY CONTAIN THE VALUE OF THE STEP,
          // FUNCTION, AND DERIVATIVE AT THE OTHER ENDPOINT OF
          // THE INTERVAL OF UNCERTAINTY.
          // THE VARIABLES STP, F, DG CONTAIN THE VALUES OF THE STEP,
          // FUNCTION, AND DERIVATIVE AT THE CURRENT STEP.
          //
          State.STX := 0;
          State.fx := State.FINIT;
          State.DGX := State.DGINIT;
          State.STY := 0;
          State.fy := State.FINIT;
          State.DGY := State.DGINIT;

          //
          // NEXT
          //
          Stage := 3;
          Continue;
        end;
      if Stage = 3 then
        begin

          //
          // START OF ITERATION.
          //
          // SET THE MINIMUM AND MAXIMUM STEPS TO CORRESPOND
          // TO THE PRESENT INTERVAL OF UNCERTAINTY.
          //
          if State.BRACKT then
            begin
              if AP_FP_Less(State.STX, State.STY) then
                begin
                  State.STMIN := State.STX;
                  State.STMAX := State.STY;
                end
              else
                begin
                  State.STMIN := State.STY;
                  State.STMAX := State.STX;
                end;
            end
          else
            begin
              State.STMIN := State.STX;
              State.STMAX := Stp + State.XTRAPF * (Stp - State.STX);
            end;

          //
          // FORCE THE STEP TO BE WITHIN THE BOUNDS STPMAX AND LM_STPMIN.
          //
          if AP_FP_Greater(Stp, StpMax) then
            begin
              Stp := StpMax;
            end;
          if AP_FP_Less(Stp, LM_STPMIN) then
            begin
              Stp := LM_STPMIN;
            end;

          //
          // IF AN UNUSUAL TERMINATION IS TO OCCUR THEN LET
          // STP BE THE LOWEST POINT OBTAINED SO FAR.
          //
          if State.BRACKT and (AP_FP_Less_Eq(Stp, State.STMIN) or
            AP_FP_Greater_Eq(Stp, State.STMAX)) or (NFEV >= LM_MAXFEV - 1) or
            (State.INFOC = 0) or State.BRACKT and
            AP_FP_Less_Eq(State.STMAX - State.STMIN, LM_XTOL * State.STMAX) then
            begin
              Stp := State.STX;
            end;

          //
          // EVALUATE THE FUNCTION AND GRADIENT AT STP
          // AND COMPUTE THE DIRECTIONAL DERIVATIVE.
          //
          APVMove(@x[0], 0, n - 1, @WA[0], 0, n - 1);
          APVAdd(@x[0], 0, n - 1, @s[0], 0, n - 1, Stp);

          //
          // NEXT
          //
          Stage := 4;
          Exit;
        end;
      if Stage = 4 then
        begin
          Info := 0;
          NFEV := NFEV + 1;
          v := APVDotProduct(@g[0], 0, n - 1, @s[0], 0, n - 1);
          State.DG := v;
          State.FTEST1 := State.FINIT + Stp * State.DGTEST;

          //
          // TEST FOR CONVERGENCE.
          //
          if State.BRACKT and (AP_FP_Less_Eq(Stp, State.STMIN) or
            AP_FP_Greater_Eq(Stp, State.STMAX)) or (State.INFOC = 0) then
            begin
              Info := 6;
            end;
          if AP_FP_Eq(Stp, StpMax) and AP_FP_Less_Eq(f, State.FTEST1) and
            AP_FP_Less_Eq(State.DG, State.DGTEST) then
            begin
              Info := 5;
            end;
          if AP_FP_Eq(Stp, LM_STPMIN) and (AP_FP_Greater(f, State.FTEST1) or
            AP_FP_Greater_Eq(State.DG, State.DGTEST)) then
            begin
              Info := 4;
            end;
          if NFEV >= LM_MAXFEV then
            begin
              Info := 3;
            end;
          if State.BRACKT and AP_FP_Less_Eq(State.STMAX - State.STMIN,
            LM_XTOL * State.STMAX) then
            begin
              Info := 2;
            end;
          if AP_FP_Less_Eq(f, State.FTEST1) and AP_FP_Less_Eq(AbsReal(State.DG),
            -LM_GTOL * State.DGINIT) then
            begin
              Info := 1;
            end;

          //
          // CHECK FOR TERMINATION.
          //
          if Info <> 0 then
            begin
              Stage := 0;
              Exit;
            end;

          //
          // IN THE FIRST STAGE WE SEEK A STEP FOR WHICH THE MODIFIED
          // FUNCTION HAS A NONPOSITIVE VALUE AND NONNEGATIVE DERIVATIVE.
          //
          if State.STAGE1 and AP_FP_Less_Eq(f, State.FTEST1) and
            AP_FP_Greater_Eq(State.DG, Min(LM_FTOL, LM_GTOL) * State.DGINIT) then
            begin
              State.STAGE1 := False;
            end;

          //
          // A MODIFIED FUNCTION IS USED TO PREDICT THE STEP ONLY IF
          // WE HAVE NOT OBTAINED A STEP FOR WHICH THE MODIFIED
          // FUNCTION HAS A NONPOSITIVE FUNCTION VALUE AND NONNEGATIVE
          // DERIVATIVE, AND IF A LOWER FUNCTION VALUE HAS BEEN
          // OBTAINED BUT THE DECREASE IS NOT SUFFICIENT.
          //
          if State.STAGE1 and AP_FP_Less_Eq(f, State.fx) and
            AP_FP_Greater(f, State.FTEST1) then
            begin

              //
              // DEFINE THE MODIFIED FUNCTION AND DERIVATIVE VALUES.
              //
              State.FM := f - Stp * State.DGTEST;
              State.FXM := State.fx - State.STX * State.DGTEST;
              State.FYM := State.fy - State.STY * State.DGTEST;
              State.DGM := State.DG - State.DGTEST;
              State.DGXM := State.DGX - State.DGTEST;
              State.DGYM := State.DGY - State.DGTEST;

              //
              // CALL CSTEP TO UPDATE THE INTERVAL OF UNCERTAINTY
              // AND TO COMPUTE THE NEW STEP.
              //
              MCSTEP(State.STX, State.FXM, State.DGXM, State.STY, State.FYM,
                State.DGYM, Stp, State.FM, State.DGM, State.BRACKT, State.STMIN,
                State.STMAX, State.INFOC);

              //
              // RESET THE FUNCTION AND GRADIENT VALUES FOR F.
              //
              State.fx := State.FXM + State.STX * State.DGTEST;
              State.fy := State.FYM + State.STY * State.DGTEST;
              State.DGX := State.DGXM + State.DGTEST;
              State.DGY := State.DGYM + State.DGTEST;
            end
          else
            begin

              //
              // CALL MCSTEP TO UPDATE THE INTERVAL OF UNCERTAINTY
              // AND TO COMPUTE THE NEW STEP.
              //
              MCSTEP(State.STX, State.fx, State.DGX, State.STY, State.fy, State.DGY,
                Stp, f, State.DG, State.BRACKT, State.STMIN, State.STMAX,
                State.INFOC);
            end;

          //
          // FORCE A SUFFICIENT DECREASE IN THE SIZE OF THE
          // INTERVAL OF UNCERTAINTY.
          //
          if State.BRACKT then
            begin
              if AP_FP_Greater_Eq(AbsReal(State.STY - State.STX), P66 * State.WIDTH1)
              then
                begin
                  Stp := State.STX + P5 * (State.STY - State.STX);
                end;
              State.WIDTH1 := State.width;
              State.width := AbsReal(State.STY - State.STX);
            end;

          //
          // NEXT.
          //
          Stage := 3;
          Continue;
        end;
    end;
end;

procedure MCSTEP(var STX: TLFloat; var fx: TLFloat; var dx: TLFloat;
  var STY: TLFloat; var fy: TLFloat; var dy: TLFloat;
  var Stp: TLFloat; const FP: TLFloat; const DP: TLFloat;
  var BRACKT: Boolean; const STMIN: TLFloat; const STMAX: TLFloat;
  var Info: TLInt);
var
  Bound: Boolean;
  Gamma: TLFloat;
  p    : TLFloat;
  q    : TLFloat;
  r    : TLFloat;
  s    : TLFloat;
  SGND : TLFloat;
  STPC : TLFloat;
  STPF : TLFloat;
  STPQ : TLFloat;
  Theta: TLFloat;
begin
  Info := 0;

  //
  // CHECK THE INPUT PARAMETERS FOR ERRORS.
  //
  if BRACKT and (AP_FP_Less_Eq(Stp, Min(STX, STY)) or AP_FP_Greater_Eq(Stp,
    Max(STX, STY))) or AP_FP_Greater_Eq(dx * (Stp - STX), 0) or
    AP_FP_Less(STMAX, STMIN) then
    begin
      Exit;
    end;

  //
  // DETERMINE IF THE DERIVATIVES HAVE OPPOSITE SIGN.
  //
  SGND := DP * (dx / AbsReal(dx));

  //
  // FIRST CASE. A HIGHER FUNCTION VALUE.
  // THE MINIMUM IS BRACKETED. IF THE CUBIC STEP IS CLOSER
  // TO STX THAN THE QUADRATIC STEP, THE CUBIC STEP IS TAKEN,
  // ELSE THE AVERAGE OF THE CUBIC AND QUADRATIC STEPS IS TAKEN.
  //
  if AP_FP_Greater(FP, fx) then
    begin
      Info := 1;
      Bound := True;
      Theta := 3 * (fx - FP) / (Stp - STX) + dx + DP;
      s := Max(AbsReal(Theta), Max(AbsReal(dx), AbsReal(DP)));
      Gamma := s * Sqrt(AP_Sqr(Theta / s) - dx / s * (DP / s));
      if AP_FP_Less(Stp, STX) then
        begin
          Gamma := -Gamma;
        end;
      p := Gamma - dx + Theta;
      q := Gamma - dx + Gamma + DP;
      r := p / q;
      STPC := STX + r * (Stp - STX);
      STPQ := STX + dx / ((fx - FP) / (Stp - STX) + dx) / 2 * (Stp - STX);
      if AP_FP_Less(AbsReal(STPC - STX), AbsReal(STPQ - STX)) then
        begin
          STPF := STPC;
        end
      else
        begin
          STPF := STPC + (STPQ - STPC) / 2;
        end;
      BRACKT := True;
    end
  else
    begin
      if AP_FP_Less(SGND, 0) then
        begin

          //
          // SECOND CASE. A LOWER FUNCTION VALUE AND DERIVATIVES OF
          // OPPOSITE SIGN. THE MINIMUM IS BRACKETED. IF THE CUBIC
          // STEP IS CLOSER TO STX THAN THE QUADRATIC (SECANT) STEP,
          // THE CUBIC STEP IS TAKEN, ELSE THE QUADRATIC STEP IS TAKEN.
          //
          Info := 2;
          Bound := False;
          Theta := 3 * (fx - FP) / (Stp - STX) + dx + DP;
          s := Max(AbsReal(Theta), Max(AbsReal(dx), AbsReal(DP)));
          Gamma := s * Sqrt(AP_Sqr(Theta / s) - dx / s * (DP / s));
          if AP_FP_Greater(Stp, STX) then
            begin
              Gamma := -Gamma;
            end;
          p := Gamma - DP + Theta;
          q := Gamma - DP + Gamma + dx;
          r := p / q;
          STPC := Stp + r * (STX - Stp);
          STPQ := Stp + DP / (DP - dx) * (STX - Stp);
          if AP_FP_Greater(AbsReal(STPC - Stp), AbsReal(STPQ - Stp)) then
            begin
              STPF := STPC;
            end
          else
            begin
              STPF := STPQ;
            end;
          BRACKT := True;
        end
      else
        begin
          if AP_FP_Less(AbsReal(DP), AbsReal(dx)) then
            begin

              //
              // THIRD CASE. A LOWER FUNCTION VALUE, DERIVATIVES OF THE
              // SAME SIGN, AND THE MAGNITUDE OF THE DERIVATIVE DECREASES.
              // THE CUBIC STEP IS ONLY USED IF THE CUBIC TENDS TO INFINITY
              // IN THE DIRECTION OF THE STEP OR IF THE MINIMUM OF THE CUBIC
              // IS BEYOND STP. OTHERWISE THE CUBIC STEP IS DEFINED TO BE
              // EITHER LM_STPMIN OR STPMAX. THE QUADRATIC (SECANT) STEP IS ALSO
              // COMPUTED AND IF THE MINIMUM IS BRACKETED THEN THE THE STEP
              // CLOSEST TO STX IS TAKEN, ELSE THE STEP FARTHEST AWAY IS TAKEN.
              //
              Info := 3;
              Bound := True;
              Theta := 3 * (fx - FP) / (Stp - STX) + dx + DP;
              s := Max(AbsReal(Theta), Max(AbsReal(dx), AbsReal(DP)));

              //
              // THE CASE GAMMA = 0 ONLY ARISES IF THE CUBIC DOES NOT TEND
              // TO INFINITY IN THE DIRECTION OF THE STEP.
              //
              Gamma := s * Sqrt(Max(0, AP_Sqr(Theta / s) - dx / s * (DP / s)));
              if AP_FP_Greater(Stp, STX) then
                begin
                  Gamma := -Gamma;
                end;
              p := Gamma - DP + Theta;
              q := Gamma + (dx - DP) + Gamma;
              r := p / q;
              if AP_FP_Less(r, 0) and AP_FP_NEq(Gamma, 0) then
                begin
                  STPC := Stp + r * (STX - Stp);
                end
              else
                begin
                  if AP_FP_Greater(Stp, STX) then
                    begin
                      STPC := STMAX;
                    end
                  else
                    begin
                      STPC := STMIN;
                    end;
                end;
              STPQ := Stp + DP / (DP - dx) * (STX - Stp);
              if BRACKT then
                begin
                  if AP_FP_Less(AbsReal(Stp - STPC), AbsReal(Stp - STPQ)) then
                    begin
                      STPF := STPC;
                    end
                  else
                    begin
                      STPF := STPQ;
                    end;
                end
              else
                begin
                  if AP_FP_Greater(AbsReal(Stp - STPC), AbsReal(Stp - STPQ)) then
                    begin
                      STPF := STPC;
                    end
                  else
                    begin
                      STPF := STPQ;
                    end;
                end;
            end
          else
            begin

              //
              // FOURTH CASE. A LOWER FUNCTION VALUE, DERIVATIVES OF THE
              // SAME SIGN, AND THE MAGNITUDE OF THE DERIVATIVE DOES
              // NOT DECREASE. IF THE MINIMUM IS NOT BRACKETED, THE STEP
              // IS EITHER LM_STPMIN OR STPMAX, ELSE THE CUBIC STEP IS TAKEN.
              //
              Info := 4;
              Bound := False;
              if BRACKT then
                begin
                  Theta := 3 * (FP - fy) / (STY - Stp) + dy + DP;
                  s := Max(AbsReal(Theta), Max(AbsReal(dy), AbsReal(DP)));
                  Gamma := s * Sqrt(AP_Sqr(Theta / s) - dy / s * (DP / s));
                  if AP_FP_Greater(Stp, STY) then
                    begin
                      Gamma := -Gamma;
                    end;
                  p := Gamma - DP + Theta;
                  q := Gamma - DP + Gamma + dy;
                  r := p / q;
                  STPC := Stp + r * (STY - Stp);
                  STPF := STPC;
                end
              else
                begin
                  if AP_FP_Greater(Stp, STX) then
                    begin
                      STPF := STMAX;
                    end
                  else
                    begin
                      STPF := STMIN;
                    end;
                end;
            end;
        end;
    end;

  //
  // UPDATE THE INTERVAL OF UNCERTAINTY. THIS UPDATE DOES NOT
  // DEPEND ON THE NEW STEP OR THE CASE ANALYSIS ABOVE.
  //
  if AP_FP_Greater(FP, fx) then
    begin
      STY := Stp;
      fy := FP;
      dy := DP;
    end
  else
    begin
      if AP_FP_Less(SGND, 0.0) then
        begin
          STY := STX;
          fy := fx;
          dy := dx;
        end;
      STX := Stp;
      fx := FP;
      dx := DP;
    end;

  //
  // COMPUTE THE NEW STEP AND SAFEGUARD IT.
  //
  STPF := Min(STMAX, STPF);
  STPF := Max(STMIN, STPF);
  Stp := STPF;
  if BRACKT and Bound then
    begin
      if AP_FP_Greater(STY, STX) then
        begin
          Stp := Min(STX + 0.66 * (STY - STX), Stp);
        end
      else
        begin
          Stp := Max(STX + 0.66 * (STY - STX), Stp);
        end;
    end;
end; 
 
 
