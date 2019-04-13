{ ****************************************************************************** }
{ * memory Rasterization with AGG support                                      * }
{ * by QQ 600585@qq.com                                                        * }
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

(*
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  //  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
  //    Maintained by Christian-W. Budde (Christian@pcjv.de)                    //
  //    Copyright (c) 2012-2017                                                 //
  //                                                                            //
  //  Based on:                                                                 //
  //    Pascal port by Milan Marusinec alias Milano (milan@marusinec.sk)        //
  //    Copyright (c) 2005-2006, see http://www.aggpas.org                      //
  //                                                                            //
  //  Original License:                                                         //
  //    Anti-Grain Geometry - Version 2.4 (Public License)                      //
  //    Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)     //
  //    Contact: McSeem@antigrain.com / McSeemAgg@yahoo.com                     //
  //                                                                            //
  //  Permission to copy, use, modify, sell and distribute this software        //
  //  is granted provided this copyright notice appears in all copies.          //
  //  This software is provided "as is" without express or implied              //
  //  warranty, and with no claim as to its suitability for any purpose.        //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
*)
unit AggColor32;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}

interface

uses
  Math,
  AggBasics;

type
  PAggOrderRgb = ^TAggOrderRGB;

  TAggOrderRGB = record
    r, g, b: Int8u;
  end;

  PAggOrderBgr = ^TAggOrderBGR;

  TAggOrderBGR = record
    b, g, r: Int8u;
  end;

  PAggOrderRgba = ^TAggOrderRGBA;

  TAggOrderRGBA = record
    r, g, b, a: Int8u;
  end;

  PAggOrderBgra = ^TAggOrderBGRA;

  TAggOrderBGRA = record
    b, g, r, a: Int8u;
  end;

  PAggOrderArgb = ^TAggOrderARGB;

  TAggOrderARGB = record
    a, r, g, b: Int8u;
  end;

  PAggOrderAbgr = ^TAggOrderABGR;

  TAggOrderABGR = record
    a, b, g, r: Int8u;
  end;

  TAggOrder = TAggOrderRGBA;

const
  CAggBaseShift = 8;
  CAggBaseSize = 1 shl CAggBaseShift;
  CAggBaseMask = CAggBaseSize - 1;

  CAggOrderRgb: TAggOrder = (r: 0; g: 1; b: 2; a: 3);
  CAggOrderBgr: TAggOrder = (r: 2; g: 1; b: 0; a: 3);
  CAggOrderRgba: TAggOrder = (r: 0; g: 1; b: 2; a: 3);
  CAggOrderBgra: TAggOrder = (r: 2; g: 1; b: 0; a: 3);
  CAggOrderArgb: TAggOrder = (r: 1; g: 2; b: 3; a: 0);
  CAggOrderAbgr: TAggOrder = (r: 3; g: 2; b: 1; a: 0);

type
  TAggPackedRgba8 = Cardinal;

  PPAggRgba8 = ^PAggRgba8;
  PAggRgba8 = ^TAggRgba8;

  TAggRgba8 = record
  public
    procedure Initialize(r, g, b: Int8u; a: Cardinal = CAggBaseMask);

    procedure NoColor;
    procedure Random;
    procedure Black;
    procedure White;

    function Gradient(c: TAggRgba8; k: Double): TAggRgba8;
    case Integer of
      0: (ABGR: TAggPackedRgba8);
      1: (r, g, b, a: Byte);
      2: (Bytes: array [0 .. 3] of Byte);
  end;

  PAggColor = ^TAggColor;

  { TAggColor }

  TAggColor = record
    Rgba8: TAggRgba8;
    v: Int8u;
  private
    function GetOpacity: Double;
    function GetBlue: Double;
    function GetGreen: Double;
    function GetRed: Double;
    procedure SetOpacity(Value: Double);
    procedure SetBlue(Value: Double);
    procedure SetGreen(Value: Double);
    procedure SetRed(Value: Double);
    procedure CalculateValue;
  public
    procedure FromRgba8(RGBA: TAggRgba8);
    procedure FromValueInteger(Value: Cardinal; alpha: Cardinal = CAggBaseMask);
    procedure FromRgbaInteger(r, g, b: Cardinal; a: Cardinal = CAggBaseMask);
    procedure FromRgbInteger(r, g, b: Cardinal; a: Double = 1.0);
    procedure FromRgbaDouble(r, g, b: Double; a: Double = 1.0);
    procedure FromWaveLength(WaveLength, Gamma: Double);

    function Gradient8(const c: TAggColor; k: Double): TAggRgba8;

    procedure Add(c: PAggColor; Cover: Cardinal);

    procedure Clear;
    procedure Black;
    procedure White;
    procedure PreMultiply;
    procedure ApplyGammaDir(Gamma: TAggGamma);

    property Opacity: Double read GetOpacity write SetOpacity;
    property Red: Double read GetRed write SetRed;
    property Green: Double read GetGreen write SetGreen;
    property Blue: Double read GetBlue write SetBlue;
  end;

const
  // Some predefined color constants
  CRgba8Black: TAggRgba8 = (ABGR: $FF000000);
  CRgba8DarkGray: TAggRgba8 = (ABGR: $FF3F3F3F);
  CRgba8Gray: TAggRgba8 = (ABGR: $FF7F7F7F);
  CRgba8LightGray: TAggRgba8 = (ABGR: $FFBFBFBF);
  CRgba8White: TAggRgba8 = (ABGR: $FFFFFFFF);
  CRgba8Maroon: TAggRgba8 = (ABGR: $FF00007F);
  CRgba8Green: TAggRgba8 = (ABGR: $FF007F00);
  CRgba8Olive: TAggRgba8 = (ABGR: $FF007F7F);
  CRgba8Navy: TAggRgba8 = (ABGR: $FF7F0000);
  CRgba8Purple: TAggRgba8 = (ABGR: $FF7F007F);
  CRgba8Teal: TAggRgba8 = (ABGR: $FF7F7F00);
  CRgba8Red: TAggRgba8 = (ABGR: $FF0000FF);
  CRgba8Lime: TAggRgba8 = (ABGR: $FF00FF00);
  CRgba8Yellow: TAggRgba8 = (ABGR: $FF00FFFF);
  CRgba8Blue: TAggRgba8 = (ABGR: $FFFF0000);
  CRgba8Fuchsia: TAggRgba8 = (ABGR: $FFFF00FF);
  CRgba8Aqua: TAggRgba8 = (ABGR: $FFFFFF00);

  CRgba8SemiWhite: TAggRgba8 = (ABGR: $7FFFFFFF);
  CRgba8SemiBlack: TAggRgba8 = (ABGR: $7F000000);
  CRgba8SemiRed: TAggRgba8 = (ABGR: $7F0000FF);
  CRgba8SemiGreen: TAggRgba8 = (ABGR: $7F00FF00);
  CRgba8SemiBlue: TAggRgba8 = (ABGR: $7FFF0000);
  CRgba8SemiMaroon: TAggRgba8 = (ABGR: $FF00007F);
  CRgba8SemiOlive: TAggRgba8 = (ABGR: $FF007F7F);
  CRgba8SemiNavy: TAggRgba8 = (ABGR: $FF7F0000);
  CRgba8SemiPurple: TAggRgba8 = (ABGR: $FF7F007F);
  CRgba8SemiTeal: TAggRgba8 = (ABGR: $FF7F7F00);
  CRgba8SemiLime: TAggRgba8 = (ABGR: $FF00FF00);
  CRgba8SemiFuchsia: TAggRgba8 = (ABGR: $FFFF00FF);
  CRgba8SemiAqua: TAggRgba8 = (ABGR: $FFFFFF00);

function Rgb8Packed(v: TAggPackedRgba8): TAggRgba8;
function HueSaturationLuminanceToRgb8(h, s, L: Double): TAggRgba8;
function RandomRgba8: TAggRgba8; overload;
function RandomRgba8(alpha: Int8u): TAggRgba8; overload;
function Gradient(const c1, c2: TAggColor; k: Double): TAggColor;

implementation


function HueSaturationLuminanceToRgb8(h, s, L: Double): TAggRgba8;
const
  COneOverThree = 1 / 3;
var
  Temp: array [0 .. 1] of Double;

  function HueToColor(Hue: Single): Byte;
  var
    v: Double;
  begin
    Hue := Hue - Floor(Hue);
    if 6 * Hue < 1 then
        v := Temp[0] + (Temp[1] - Temp[0]) * Hue * 6
    else
      if 2 * Hue < 1 then
        v := Temp[1]
    else
      if 3 * Hue < 2 then
        v := Temp[0] + (Temp[1] - Temp[0]) * (2 * COneOverThree - Hue) * 6
    else
        v := Temp[0];
    Result := Round(255 * v);
  end;

begin
  if s = 0 then
    begin
      Result.r := Round(255 * L);
      Result.g := Result.r;
      Result.b := Result.r;
    end
  else
    begin
      if L <= 0.5 then
          Temp[1] := L * (1 + s)
      else
          Temp[1] := L + s - L * s;

      Temp[0] := 2 * L - Temp[1];

      Result.r := HueToColor(h + COneOverThree);
      Result.g := HueToColor(h);
      Result.b := HueToColor(h - COneOverThree)
    end;
  Result.a := $FF;
end;

function RandomRgba8: TAggRgba8;
begin
  Result.ABGR := System.Random($FFFFFFFF);
end;

function RandomRgba8(alpha: Int8u): TAggRgba8; overload;
begin
  Result.ABGR := System.Random($FFFFFF) + alpha shl 24;
end;

function Gradient(const c1, c2: TAggColor; k: Double): TAggColor;
var
  Ik: Cardinal;
begin
  Ik := Trunc(k * CAggBaseSize);

  Result.Rgba8.r := Int8u(c1.Rgba8.r + (((c2.Rgba8.r - c1.Rgba8.r) * Ik) shr CAggBaseShift));
  Result.Rgba8.g := Int8u(c1.Rgba8.g + (((c2.Rgba8.g - c1.Rgba8.g) * Ik) shr CAggBaseShift));
  Result.Rgba8.b := Int8u(c1.Rgba8.b + (((c2.Rgba8.b - c1.Rgba8.b) * Ik) shr CAggBaseShift));
  Result.Rgba8.a := Int8u(c1.Rgba8.a + (((c2.Rgba8.a - c1.Rgba8.a) * Ik) shr CAggBaseShift));
end;

function Rgba8ToAggColor(RGBA: TAggRgba8): TAggColor;
begin
  with Result do
    begin
      Rgba8 := RGBA;
      v := (Rgba8.r * 77 + Rgba8.g * 150 + Rgba8.b * 29) shr 8;
    end;
end;

function RgbaIntegerToAggColor(r, g, b: Cardinal; a: Cardinal = CAggBaseMask):
  TAggColor;
begin
  with Result do
    begin
      Rgba8.r := Int8u(r);
      Rgba8.g := Int8u(g);
      Rgba8.b := Int8u(b);
      Rgba8.a := a;
      v := (Rgba8.r * 77 + Rgba8.g * 150 + Rgba8.b * 29) shr 8;
    end;
end;

function RgbIntegerToAggColor(r, g, b: Cardinal; a: Double = 1.0): TAggColor;
begin
  with Result do
    begin
      Rgba8.r := Int8u(r);
      Rgba8.g := Int8u(g);
      Rgba8.b := Int8u(b);
      Rgba8.a := Trunc(a * CAggBaseMask + 0.5);
      v := (Rgba8.r * 77 + Rgba8.g * 150 + Rgba8.b * 29) shr 8;
    end;
end;

function RgbaDoubleToAggColor(r, g, b, a: Double): TAggColor;
begin
  with Result do
    begin
      v := Trunc((0.299 * r + 0.587 * g + 0.114 * b) * CAggBaseMask + 0.5);
      Rgba8.r := Trunc(r * CAggBaseMask + 0.5);
      Rgba8.g := Trunc(g * CAggBaseMask + 0.5);
      Rgba8.b := Trunc(b * CAggBaseMask + 0.5);
      Rgba8.a := Trunc(a * CAggBaseMask + 0.5);
    end;
end;

{ TAggRgba8 }

procedure TAggRgba8.Initialize(r, g, b: Int8u; a: Cardinal = CAggBaseMask);
begin
  Self.b := Int8u(b);
  Self.g := Int8u(g);
  Self.r := Int8u(r);
  Self.a := Int8u(a);
end;

procedure TAggRgba8.NoColor;
begin
  r := 0;
  g := 0;
  b := 0;
  a := 0;
end;

procedure TAggRgba8.Black;
begin
  r := 0;
  g := 0;
  b := 0;
  a := $FF;
end;

procedure TAggRgba8.Random;
begin
  ABGR := System.Random($FFFFFFFF);
end;

procedure TAggRgba8.White;
begin
  r := $FF;
  g := $FF;
  b := $FF;
  a := $FF;
end;

function TAggRgba8.Gradient(c: TAggRgba8; k: Double): TAggRgba8;
var
  Ik: Int32u; // calc_type
begin
  Ik := Trunc(k * CAggBaseSize);

  Result.r := Int8u(Int32u(r) + (((Int32u(c.r) - r) * Ik) shr CAggBaseShift));
  Result.g := Int8u(Int32u(g) + (((Int32u(c.g) - g) * Ik) shr CAggBaseShift));
  Result.b := Int8u(Int32u(b) + (((Int32u(c.b) - b) * Ik) shr CAggBaseShift));
  Result.a := Int8u(Int32u(a) + (((Int32u(c.a) - a) * Ik) shr CAggBaseShift));
end;

{ TAggColor }

procedure TAggColor.FromRgba8(RGBA: TAggRgba8);
begin
  Rgba8 := RGBA;
  CalculateValue;
end;

procedure TAggColor.FromRgbaInteger(r, g, b: Cardinal; a: Cardinal = CAggBaseMask);
begin
  Rgba8.r := Int8u(r);
  Rgba8.g := Int8u(g);
  Rgba8.b := Int8u(b);
  Rgba8.a := a;
  CalculateValue;
end;

procedure TAggColor.FromRgbInteger(r, g, b: Cardinal; a: Double = 1.0);
begin
  Rgba8.r := Int8u(r);
  Rgba8.g := Int8u(g);
  Rgba8.b := Int8u(b);
  Rgba8.a := Trunc(a * CAggBaseMask + 0.5);
  CalculateValue;
end;

procedure TAggColor.FromValueInteger(Value: Cardinal; alpha: Cardinal);
begin
  v := Value;
  Rgba8.r := 0;
  Rgba8.g := 0;
  Rgba8.b := 0;
  Rgba8.a := alpha;
end;

procedure TAggColor.FromRgbaDouble(r, g, b: Double; a: Double);
begin
  v := Trunc((0.299 * r + 0.587 * g + 0.114 * b) * CAggBaseMask + 0.5);
  Rgba8.r := Trunc(r * CAggBaseMask + 0.5);
  Rgba8.g := Trunc(g * CAggBaseMask + 0.5);
  Rgba8.b := Trunc(b * CAggBaseMask + 0.5);
  Rgba8.a := Trunc(a * CAggBaseMask + 0.5);
end;

procedure TAggColor.FromWaveLength(WaveLength, Gamma: Double);
var
  tr, Tg, TB, s: Double;
begin
  tr := 0;
  Tg := 0;
  TB := 0;

  if (WaveLength >= 380.0) and (WaveLength <= 440.0) then
    begin
      tr := (440.0 - WaveLength) / 60;
      TB := 1.0;
    end
  else if (WaveLength >= 440.0) and (WaveLength <= 490.0) then
    begin
      Tg := (WaveLength - 440.0) * 0.02;
      TB := 1.0;
    end
  else if (WaveLength >= 490.0) and (WaveLength <= 510.0) then
    begin
      Tg := 1.0;
      TB := (510 - WaveLength) * 0.05;
    end
  else if (WaveLength >= 510.0) and (WaveLength <= 580.0) then
    begin
      tr := (WaveLength - 510.0) / 70;
      Tg := 1.0;
    end
  else if (WaveLength >= 580.0) and (WaveLength <= 645.0) then
    begin
      tr := 1.0;
      Tg := (645.0 - WaveLength) / 65;
    end
  else if (WaveLength >= 645.0) and (WaveLength <= 780.0) then
      tr := 1.0;

  s := 1.0;

  if WaveLength > 700.0 then
      s := 0.3 + 0.7 * (780.0 - WaveLength) / 80
  else if WaveLength < 420.0 then
      s := 0.3 + 0.7 * (WaveLength - 380.0) / 40;

  tr := Power(tr * s, Gamma);
  Tg := Power(Tg * s, Gamma);
  TB := Power(TB * s, Gamma);

  v := Trunc((0.299 * tr + 0.587 * Tg + 0.114 * TB) * CAggBaseMask + 0.5);
  Rgba8.r := Trunc(tr * CAggBaseMask + 0.5);
  Rgba8.g := Trunc(Tg * CAggBaseMask + 0.5);
  Rgba8.b := Trunc(TB * CAggBaseMask + 0.5);
  Rgba8.a := $FF;
end;

function TAggColor.Gradient8(const c: TAggColor; k: Double): TAggRgba8;
var
  Ik: Cardinal;
begin
  Ik := Trunc(k * CAggBaseSize);

  Result.r := Int8u(Rgba8.r + (((c.Rgba8.r - Rgba8.r) * Ik) shr CAggBaseShift));
  Result.g := Int8u(Rgba8.g + (((c.Rgba8.g - Rgba8.g) * Ik) shr CAggBaseShift));
  Result.b := Int8u(Rgba8.b + (((c.Rgba8.b - Rgba8.b) * Ik) shr CAggBaseShift));
  Result.a := Int8u(Rgba8.a + (((c.Rgba8.a - Rgba8.a) * Ik) shr CAggBaseShift));
end;

procedure TAggColor.Add(c: PAggColor; Cover: Cardinal);
var
  cv, CR, Cg, CB, ca: Int32u;
begin
  if Cover = CAggCoverMask then
    if c.Rgba8.a = CAggBaseMask then
      begin
        v := c^.v;
        Rgba8 := c^.Rgba8;
      end
    else
      begin
        cv := v + c.v;

        if cv > Int32u(CAggBaseMask) then
            v := Int8u(CAggBaseMask)
        else
            v := Int8u(cv);

        CR := Rgba8.r + c.Rgba8.r;

        if CR > Int32u(CAggBaseMask) then
            Rgba8.r := Int8u(CAggBaseMask)
        else
            Rgba8.r := Int8u(CR);

        Cg := Rgba8.g + c.Rgba8.g;

        if Cg > Int32u(CAggBaseMask) then
            Rgba8.g := Int8u(CAggBaseMask)
        else
            Rgba8.g := Int8u(Cg);

        CB := Rgba8.b + c.Rgba8.b;

        if CB > Int32u(CAggBaseMask) then
            Rgba8.b := Int8u(CAggBaseMask)
        else
            Rgba8.b := Int8u(CB);

        ca := Rgba8.a + c.Rgba8.a;

        if ca > Int32u(CAggBaseMask) then
            Rgba8.a := Int8u(CAggBaseMask)
        else
            Rgba8.a := Int8u(ca);
      end
  else
    begin
      cv := v + ((c.v * Cover + CAggCoverMask div 2) shr CAggCoverShift);
      CR := Rgba8.r + ((c.Rgba8.r * Cover + CAggCoverMask div 2) shr CAggCoverShift);
      Cg := Rgba8.g + ((c.Rgba8.g * Cover + CAggCoverMask div 2) shr CAggCoverShift);
      CB := Rgba8.b + ((c.Rgba8.b * Cover + CAggCoverMask div 2) shr CAggCoverShift);
      ca := Rgba8.a + ((c.Rgba8.a * Cover + CAggCoverMask div 2) shr CAggCoverShift);

      if cv > Int32u(CAggBaseMask) then
          v := Int8u(CAggBaseMask)
      else
          v := Int8u(cv);

      if CR > Int32u(CAggBaseMask) then
          Rgba8.r := Int8u(CAggBaseMask)
      else
          Rgba8.r := Int8u(CR);

      if Cg > Int32u(CAggBaseMask) then
          Rgba8.g := Int8u(CAggBaseMask)
      else
          Rgba8.g := Int8u(Cg);

      if CB > Int32u(CAggBaseMask) then
          Rgba8.b := Int8u(CAggBaseMask)
      else
          Rgba8.b := Int8u(CB);

      if ca > Int32u(CAggBaseMask) then
          Rgba8.a := Int8u(CAggBaseMask)
      else
          Rgba8.a := Int8u(ca);
    end;
end;

procedure TAggColor.SetBlue(Value: Double);
begin
  if Value < 0.0 then
      Value := 0.0;

  if Value > 1.0 then
      Value := 1.0;

  Rgba8.b := Trunc(Value * CAggBaseMask + 0.5);
end;

procedure TAggColor.SetGreen(Value: Double);
begin
  if Value < 0.0 then
      Value := 0.0;

  if Value > 1.0 then
      Value := 1.0;

  Rgba8.g := Trunc(Value * CAggBaseMask + 0.5);
end;

procedure TAggColor.SetRed(Value: Double);
begin
  if Value < 0.0 then
      Value := 0.0;

  if Value > 1.0 then
      Value := 1.0;

  Rgba8.r := Trunc(Value * CAggBaseMask + 0.5);
end;

procedure TAggColor.SetOpacity(Value: Double);
begin
  if Value < 0.0 then
      Value := 0.0;

  if Value > 1.0 then
      Value := 1.0;

  Rgba8.a := Trunc(Value * CAggBaseMask + 0.5);
end;

function TAggColor.GetBlue: Double;
begin
  Result := Rgba8.b / CAggBaseMask;
end;

function TAggColor.GetGreen: Double;
begin
  Result := Rgba8.g / CAggBaseMask;
end;

function TAggColor.GetOpacity: Double;
begin
  Result := Rgba8.a / CAggBaseMask;
end;

function TAggColor.GetRed: Double;
begin
  Result := Rgba8.r / CAggBaseMask;
end;

procedure TAggColor.CalculateValue;
begin
  v := (Rgba8.r * 77 + Rgba8.g * 150 + Rgba8.b * 29) shr 8;
end;

procedure TAggColor.Clear;
begin
  v := 0;
  Rgba8.r := 0;
  Rgba8.g := 0;
  Rgba8.b := 0;
  Rgba8.a := 0;
end;

procedure TAggColor.PreMultiply;
begin
  if Rgba8.a = CAggBaseMask then
      Exit;

  if Rgba8.a = 0 then
    begin
      v := 0;
      Rgba8.r := 0;
      Rgba8.g := 0;
      Rgba8.b := 0;
      Exit;
    end;

  v := Int8u((v * Rgba8.a) shr CAggBaseShift);
  Rgba8.r := Int8u((Rgba8.r * Rgba8.a) shr CAggBaseShift);
  Rgba8.g := Int8u((Rgba8.g * Rgba8.a) shr CAggBaseShift);
  Rgba8.b := Int8u((Rgba8.b * Rgba8.a) shr CAggBaseShift);
end;

procedure TAggColor.ApplyGammaDir(Gamma: TAggGamma);
begin
  v := Int8u(Gamma.dir[v]);
  Rgba8.r := Int8u(Gamma.dir[Rgba8.r]);
  Rgba8.g := Int8u(Gamma.dir[Rgba8.g]);
  Rgba8.b := Int8u(Gamma.dir[Rgba8.b]);
end;

procedure TAggColor.Black;
begin
  v := 0;
  Rgba8 := CRgba8Black;
end;

procedure TAggColor.White;
begin
  v := $FF;
  Rgba8 := CRgba8White;
end;

function Rgb8Packed(v: TAggPackedRgba8): TAggRgba8;
begin
  Result.r := (v shr 16) and $FF;
  Result.g := (v shr 8) and $FF;
  Result.b := v and $FF;
  Result.a := CAggBaseMask;
end;

end.
