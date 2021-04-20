{ ****************************************************************************** }
{ * Portable high quality random number, by QQ 600585@qq.com                   * }
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
const
  HQRNDMax = 2147483563;
  HQRNDM1 = 2147483563;
  HQRNDM2 = 2147483399;
  HQRNDMagic = 1634357784;

function HQRNDIntegerBase(var State: THQRNDState): TLInt;
var
  k: TLInt;
begin
  Assert(State.MagicV = HQRNDMagic, 'HQRNDIntegerBase: State is not correctly initialized!');
  k := State.s1 div 53668;
  State.s1 := 40014 * (State.s1 - k * 53668) - k * 12211;
  if State.s1 < 0 then
    begin
      State.s1 := State.s1 + 2147483563;
    end;
  k := State.s2 div 52774;
  State.s2 := 40692 * (State.s2 - k * 52774) - k * 3791;
  if State.s2 < 0 then
    begin
      State.s2 := State.s2 + 2147483399;
    end;

  //
  // Result
  //
  Result := State.s1 - State.s2;
  if Result < 1 then
    begin
      Result := Result + 2147483562;
    end;
end;

(* ************************************************************************
  THQRNDState initialization  with  random  values  which come from standard RNG.
  ************************************************************************ *)
procedure HQRNDRandomize(var State: THQRNDState);
begin
  HQRNDSeed(RandomInteger(HQRNDM1), RandomInteger(HQRNDM2), State);
end;

(* ************************************************************************
  THQRNDState initialization with seed values
  ************************************************************************ *)
procedure HQRNDSeed(const s1, s2: TLInt; var State: THQRNDState);
begin
  State.s1 := s1 mod (HQRNDM1 - 1) + 1;
  State.s2 := s2 mod (HQRNDM2 - 1) + 1;
  State.v := AP_Float(1) / HQRNDMax;
  State.MagicV := HQRNDMagic;
end;

(* ************************************************************************
  This function generates random real number in (0,1),
  not including interval boundaries

  State structure must be initialized with HQRNDRandomize() or HQRNDSeed().
  ************************************************************************ *)
function HQRNDUniformR(var State: THQRNDState): TLFloat;
begin
  Result := State.v * HQRNDIntegerBase(State);
end;

(* ************************************************************************
  This function generates random integer number in [0, N)

  1. N must be less than HQRNDMax-1.
  2. State structure must be initialized with HQRNDRandomize() or HQRNDSeed()
  ************************************************************************ *)
function HQRNDUniformI(const n: TLInt; var State: THQRNDState): TLInt;
var
  mx: TLInt;
begin

  //
  // Correct handling of N's close to RNDBaseMax
  // (avoiding skewed distributions for RNDBaseMax<>K*N)
  //
  Assert(n > 0, 'HQRNDUniformI: N<=0!');
  Assert(n < HQRNDMax - 1, 'HQRNDUniformI: N>=RNDBaseMax-1!');
  mx := HQRNDMax - 1 - (HQRNDMax - 1) mod n;
  repeat
      Result := HQRNDIntegerBase(State) - 1;
  until Result < mx;
  Result := Result mod n;
end;

(* ************************************************************************
  Random number generator: normal numbers

  This function generates one random number from normal distribution.
  Its performance is equal to that of HQRNDNormal2()

  State structure must be initialized with HQRNDRandomize() or HQRNDSeed().
  ************************************************************************ *)
function HQRNDNormal(var State: THQRNDState): TLFloat;
var
  v1: TLFloat;
  v2: TLFloat;
begin
  HQRNDNormal2(State, v1, v2);
  Result := v1;
end;

(* ************************************************************************
  Random number generator: random X and Y such that X^2+Y^2=1

  State structure must be initialized with HQRNDRandomize() or HQRNDSeed().
  ************************************************************************ *)
procedure HQRNDUnit2(var State: THQRNDState; var x: TLFloat; var y: TLFloat);
var
  v: TLFloat;
  mx: TLFloat;
  mn: TLFloat;
begin
  repeat
      HQRNDNormal2(State, x, y);
  until AP_FP_NEq(x, 0) or AP_FP_NEq(y, 0);
  mx := Max(AbsReal(x), AbsReal(y));
  mn := Min(AbsReal(x), AbsReal(y));
  v := mx * Sqrt(1 + AP_Sqr(mn / mx));
  x := x / v;
  y := y / v;
end;

(* ************************************************************************
  Random number generator: normal numbers

  This function generates two independent random numbers from normal
  distribution. Its performance is equal to that of HQRNDNormal()

  State structure must be initialized with HQRNDRandomize() or HQRNDSeed().
  ************************************************************************ *)
procedure HQRNDNormal2(var State: THQRNDState; var x1: TLFloat; var x2: TLFloat);
var
  u: TLFloat;
  v: TLFloat;
  s: TLFloat;
begin
  while True do
    begin
      u := 2 * HQRNDUniformR(State) - 1;
      v := 2 * HQRNDUniformR(State) - 1;
      s := AP_Sqr(u) + AP_Sqr(v);
      if AP_FP_Greater(s, 0) and AP_FP_Less(s, 1) then
        begin

          //
          // two Sqrt's instead of one to
          // avoid overflow when S is too small
          //
          s := Sqrt(-2 * ln(s)) / Sqrt(s);
          x1 := u * s;
          x2 := v * s;
          Exit;
        end;
    end;
end;

(* ************************************************************************
  Random number generator: exponential distribution

  State structure must be initialized with HQRNDRandomize() or HQRNDSeed().
  ************************************************************************ *)
function HQRNDExponential(const LAMBDA: TLFloat; var State: THQRNDState): TLFloat;
begin
  Assert(AP_FP_Greater(LAMBDA, 0), 'HQRNDExponential: Lambda<=0!');
  Result := -ln(HQRNDUniformR(State)) / LAMBDA;
end;
