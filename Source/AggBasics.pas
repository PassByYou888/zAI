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
unit AggBasics;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}

interface

type
  Int8 = ShortInt;
  Int8u = Byte;
  Int16 = SmallInt;
  Int16u = Word;
  Int32 = Integer;
  Int32u = Cardinal;
  Int64u = UInt64;
  PInt8 = ^Int8;
  PInt8u = ^Int8u;
  PInt16 = ^Int16;
  PInt16u = ^Int16u;
  PInt32 = ^Int32;
  PInt32u = ^Int32u;
  PInt64 = ^Int64;
  PInt64u = ^Int64u;
  PPInt8u = ^PInt8u;

  TCover = Byte;
  PCover = ^TCover;

  PInt8uArray2 = ^TInt8uArray2;
  TInt8uArray2 = array [0 .. 1] of Int8u;

  PAggBytes = ^TAggBytes;
  TAggBytes = array of Byte;

  TInt16uAccess = record
    low, high: Int8u;
  end;

  TInt32Access = record
    low, high: Int16;
  end;

  TInt32Int8uAccess = record
    values: array [0 .. 3] of Int8u;
  end;

  TInt32uAccess = record
    low, high: Int16u;
  end;

  TInt64uAccess = record
    low, high: TInt32uAccess;
  end;

  { To achive maximum compatiblity with older code, FPC doesn't change the size
    of predefined data types like integer, longint or Word when changing from
    32 to 64 Bit. However, the size of a pointer is 8 bytes on a 64 bit
    architecture so constructs like longint(pointer(p)) are doomed to crash on
    64 bit architectures. However, to alLow you to write portable code, the
    FPC system unit introduces the types PtrInt and PtrUInt which are signed
    and Cardinal integer data types with the same size as a pointer.

    Keep in mind that the size change of the "pointer" type also affects record
    sizes. If you allocate records with fixed sizes, and not with new or with
    getmem (<x>,SizeOf(<x>)), this will have to be fixed. }
  // Pascal Pointer Computation Type
  PtrComp = nativeUInt;

  // Pascal's pointer-in-an-array-access helper structures
  PNativePointer = ^TNativePointer;

  TNativePointer = record
    case Integer of
      1: (PTR: Pointer);
      2: (Int: nativeUInt);
  end;

  PPDouble = ^PDouble;

  PDoubleArray2 = ^TDoubleArray2;
  TDoubleArray2 = array [0 .. 1] of Double;

  PDoubleArray4 = ^TDoubleArray4;
  TDoubleArray4 = array [0 .. 3] of Double;

  PDoubleArray8 = ^TDoubleArray8;
  TDoubleArray8 = array [0 .. 7] of Double;

  PDoubleArray42 = ^TDoubleArray42;
  TDoubleArray42 = array [0 .. 3, 0 .. 1] of Double;

  PDoubleMatrix4x4 = ^TDoubleMatrix4x4;
  TDoubleMatrix4x4 = array [0 .. 3, 0 .. 3] of Double;

  PDoubleMatrix8x1 = ^TDoubleMatrix8x1;
  TDoubleMatrix8x1 = array [0 .. 7, 0 .. 0] of Double;

  PDoubleMatrix8x8 = ^TDoubleMatrix8x8;
  TDoubleMatrix8x8 = array [0 .. 7, 0 .. 7] of Double;

  PDoubleMatrix2x6 = ^TDoubleMatrix2x6;
  TDoubleMatrix2x6 = array [0 .. 25] of Double;

  TAggGamma = class
  protected
    function GetDir(Value: Cardinal): Cardinal; virtual; abstract;
    function GetInv(Value: Cardinal): Cardinal; virtual; abstract;
  public
    property dir[Value: Cardinal]: Cardinal read GetDir;
    property Inv[Value: Cardinal]: Cardinal read GetInv;
  end;

  TAggFillingRule = (frNonZero, frEvenOdd);
  TAggLineCap = (lcButt, lcSquare, lcRound);
  TAggLineJoin = (ljMiter, ljMiterRevert, ljMiterRound, ljRound, ljBevel);
  TAggInnerJoin = (ijBevel, ijMiter, ijJag, ijRound);

const
  CAggCoverShift = 8;
  CAggCoverSize = 1 shl CAggCoverShift;
  CAggCoverMask = CAggCoverSize - 1;
  CAggCoverNone = 0;
  CAggCoverFull = CAggCoverMask;

  // These constants determine the subpixel accuracy, to be more precise,
  // the number of bits of the fractional part of the coordinates.
  // The possible coordinate capacity in bits can be calculated by formula:
  // SizeOf(Integer) * 8 - CAggPolySubpixelShift, i.ECX, for 32-bit integers and
  // 8-bits fractional part the capacity is 24 bits.
  CAggPolySubpixelShift = 8;
  CAggPolySubpixelScale = 1 shl CAggPolySubpixelShift;
  CAggPolySubpixelMask = CAggPolySubpixelScale - 1;

  // CAggPathCmd enumeration (see flags below)
  CAggPathCmdStop = 0;
  CAggPathCmdMoveTo = 1;
  CAggPathCmdLineTo = 2;
  CAggPathCmdCurve3 = 3;
  CAggPathCmdCurve4 = 4;
  CAggPathCmdCurveN = 5;
  CAggPathCmdCatrom = 6;
  CAggPathCmdUbSpline = 7;
  CAggPathCmdEndPoly = $0F;
  CAggPathCmdMask = $0F;

  // CAggPathFlags
  CAggPathFlagsNone = 0;
  CAggPathFlagsCcw = $10;
  CAggPathFlagsCw = $20;
  CAggPathFlagsClose = $40;
  CAggPathFlagsMask = $F0;

  CDeg2Rad: Double = pi / 180;
  CRad2Deg: Double = 180 / pi;

type
  PPointDouble = ^TPointDouble;

  TPointDouble = record
    x, y: Double;
  public
    class operator Equal(const Lhs, Rhs: TPointDouble): Boolean;
    class operator NotEqual(const Lhs, Rhs: TPointDouble): Boolean;
  end;

  PPointInteger = ^TPointInteger;

  TPointInteger = record
    x, y: Integer;
  public
    class operator Equal(const Lhs, Rhs: TPointInteger): Boolean;
    class operator NotEqual(const Lhs, Rhs: TPointInteger): Boolean;
  end;

  PRectInteger = ^TRectInteger;

  TRectInteger = record
  private
    function GetWidth: Integer;
    function GetHeight: Integer;
  public
{$IFNDEF FPC}
    constructor Create(x1, y1, x2, y2: Integer); overload;
    constructor Create(Rect: TRectInteger); overload;
{$ENDIF}
    class operator Equal(const Lhs, Rhs: TRectInteger): Boolean;
    class operator NotEqual(const Lhs, Rhs: TRectInteger): Boolean;
    class operator Add(const Lhs, Rhs: TRectInteger): TRectInteger;
    class operator Subtract(const Lhs, Rhs: TRectInteger): TRectInteger;
    class function Zero: TRectInteger; static;

    procedure Normalize;
    function Clip(var Rect: TRectInteger): Boolean;
    function IsValid: Boolean;

    property width: Integer read GetWidth;
    property height: Integer read GetHeight;

    case Integer of
      0: (x1, y1, x2, y2: Integer);
      1: (values: array [0 .. 3] of Integer);
      2: (Point1, Point2: TPointInteger);
      3: (Points: array [0 .. 1] of TPointInteger);
  end;

  PRectDouble = ^TRectDouble;

  TRectDouble = record
  private
    function GetCenterX: Double;
    function GetCenterY: Double;
  public
{$IFNDEF FPC}
    constructor Create(x1, y1, x2, y2: Double); overload;
    constructor Create(Rect: TRectDouble); overload;
{$ENDIF}
    // operator overloads
    class operator Equal(const Lhs, Rhs: TRectDouble): Boolean;
    class operator NotEqual(const Lhs, Rhs: TRectDouble): Boolean;
    class operator Add(const Lhs, Rhs: TRectDouble): TRectDouble;
    class operator Subtract(const Lhs, Rhs: TRectDouble): TRectDouble;

    class function Zero: TRectDouble; static;

    class operator Implicit(const Value: TRectInteger): TRectDouble;

    procedure Normalize;
    function Clip(r: PRectDouble): Boolean; overload;
    function Clip(var r: TRectDouble): Boolean; overload;
    function IsValid: Boolean;

    property CenterX: Double read GetCenterX;
    property CenterY: Double read GetCenterY;
    case Integer of
      0: (x1, y1, x2, y2: Double);
      1: (values: array [0 .. 3] of Double);
      2: (Point1, Point2: TPointDouble);
      3: (Points: array [0 .. 1] of TPointDouble);
  end;

  PQuadDouble = ^TQuadDouble;

  TQuadDouble = record
    case Integer of
      0: (values: array [0 .. 7] of Double);
      1: (Points: array [0 .. 3] of TPointDouble);
  end;

  TVertex = record
    x, y: Double;
    Cmd: Byte;
  end;

  TCardinalList = class
  protected
    function GetItem(index: Cardinal): Cardinal; virtual; abstract;
  public
    property Item[index: Cardinal]: Cardinal read GetItem; default;
  end;

function AggGetMem(out Buf: Pointer; SZ: Cardinal): Boolean;
function AggFreeMem(var Buf: Pointer; SZ: Cardinal): Boolean;

function Deg2Rad(Deg: Double): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Rad2Deg(Rad: Double): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function IntersectRectangles(const r1, r2: TRectInteger): TRectInteger;
function IntersectRectanglesDouble(const r1, r2: TRectDouble): TRectDouble;

function UniteRectangles(const r1, r2: TRectInteger): TRectInteger;
function UniteRectanglesDouble(const r1, r2: TRectDouble): TRectDouble;

function IsVertex(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsDrawing(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsStop(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsMove(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsLineTo(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsMoveTo(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsCurve(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsCurve3(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsCurve4(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsEndPoly(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsClose(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsNextPoly(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsClockwise(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsCounterClockwise(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsOriented(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsClosed(Cx: Cardinal): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function GetCloseFlag(Cx: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ClearOrientation(Cx: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function GetOrientation(Cx: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SetOrientation(Cx, O: Cardinal): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SwapPointers(a, b: Pointer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IntToDouble(i: Integer): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RandomMinMax(Min, Max: Double): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectInteger(x1, y1, x2, y2: Integer): TRectInteger; overload;
function RectInteger(Point1, Point2: TPointInteger): TRectInteger; overload;
function RectDouble(x1, y1, x2, y2: Double): TRectDouble; overload;
function RectDouble(Point1, Point2: TPointDouble): TRectDouble; overload;

function PointInteger(x, y: Integer): TPointInteger; overload;
function PointInteger(Value: Integer): TPointInteger; overload;
function PointDouble(x, y: Double): TPointDouble; overload;
function PointDouble(Value: Double): TPointDouble; overload;

function PointIntegerOffset(Point: TPointInteger; Value: Integer): TPointInteger;
function PointDoubleOffset(Point: TPointDouble; Value: Double): TPointDouble;
function PointIntegerScale(Point: TPointInteger; Value: Integer): TPointInteger;
function PointDoubleScale(Point: TPointDouble; Value: Double): TPointDouble;

function QuadDouble(RectDouble: TRectDouble): TQuadDouble;

(*
  procedure Srand_(Seed: Integer);
  function Rand_: Integer;

  procedure Srand(Seed: Integer);
  function Rand: Integer;
*)

function EnsureRange(const Value, Min, Max: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function EnsureRange(const Value, Min, Max: Double): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF} overload;
function UnsignedRound(v: Double): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IntegerRound(v: Double): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function SaturationIntegerRound(Limit: Integer; v: Double): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}

// SHR for signed integers is differently implemented in pascal compilers
// than in C++ compilers. On the assembler level, C++ is using the SAR and
// pascal is using SHR. That gives completely different Result, when the
// number is negative. We have to be compatible with C++ implementation,
// thus instead of directly using SHR we emulate C++ solution.
function ShrInt8(i, Shift: Int8): Int8; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ShrInt16(i, Shift: Int16): Int16; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ShrInt32(i, Shift: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure Fill32Bit(var x; Count: Cardinal; var Value);

implementation

function AggGetMem(out Buf: Pointer; SZ: Cardinal): Boolean;
begin
  Result := False;
  try
    GetMem(Buf, SZ);
    Result := True;
  except
      Buf := nil;
  end;
end;

function AggFreeMem(var Buf: Pointer; SZ: Cardinal): Boolean;
begin
  if Buf = nil then
      Result := True
  else
    try
      FreeMem(Buf, SZ);
      Buf := nil;
      Result := True;
    except
        Result := False;
    end;
end;

function Deg2Rad(Deg: Double): Double;
begin
  Result := Deg * CDeg2Rad;
end;

function Rad2Deg(Rad: Double): Double;
begin
  Result := Rad * CRad2Deg;
end;

{ TPointDouble }

class operator TPointDouble.Equal(const Lhs, Rhs: TPointDouble): Boolean;
begin
  Result := (Lhs.x = Rhs.x) and (Lhs.y = Rhs.y);
end;

class operator TPointDouble.NotEqual(const Lhs, Rhs: TPointDouble): Boolean;
begin
  Result := not(Lhs = Rhs);
end;

{ TPointInteger }

class operator TPointInteger.Equal(const Lhs, Rhs: TPointInteger): Boolean;
begin
  Result := (Lhs.x = Rhs.x) and (Lhs.y = Rhs.y);
end;

class operator TPointInteger.NotEqual(const Lhs, Rhs: TPointInteger): Boolean;
begin
  Result := not(Lhs = Rhs);
end;

{ TRectInteger }

{$IFNDEF FPC}


constructor TRectInteger.Create(x1, y1, x2, y2: Integer);
begin
  Self.x1 := x1;
  Self.y1 := y1;
  Self.x2 := x2;
  Self.y2 := y2;
end;

constructor TRectInteger.Create(Rect: TRectInteger);
begin
  Self.x1 := Rect.x1;
  Self.y1 := Rect.y1;
  Self.x2 := Rect.x2;
  Self.y2 := Rect.y2;
end;
{$ENDIF}


class operator TRectInteger.Equal(const Lhs, Rhs: TRectInteger): Boolean;
begin
  Result := (Lhs.Point1 = Rhs.Point1) and (Lhs.Point2 = Rhs.Point2)
end;

class operator TRectInteger.NotEqual(const Lhs, Rhs: TRectInteger): Boolean;
begin
  Result := not(Lhs = Rhs);
end;

class operator TRectInteger.Add(const Lhs, Rhs: TRectInteger): TRectInteger;
begin
  Result.x1 := Lhs.x1 + Rhs.x1;
  Result.y1 := Lhs.y1 + Rhs.y1;
  Result.x2 := Lhs.x2 + Rhs.x2;
  Result.y2 := Lhs.y2 + Rhs.y2;
end;

class operator TRectInteger.Subtract(const Lhs, Rhs: TRectInteger): TRectInteger;
begin
  Result.x1 := Lhs.x1 - Rhs.x1;
  Result.y1 := Lhs.y1 - Rhs.y1;
  Result.x2 := Lhs.x2 - Rhs.x2;
  Result.y2 := Lhs.y2 - Rhs.y2;
end;

class function TRectInteger.Zero: TRectInteger;
begin
  FillChar(Result, 16, 0);
end;

function TRectInteger.Clip(var Rect: TRectInteger): Boolean;
begin
  if x2 > Rect.x2 then
      x2 := Rect.x2;

  if y2 > Rect.y2 then
      y2 := Rect.y2;

  if x1 < Rect.x1 then
      x1 := Rect.x1;

  if y1 < Rect.y1 then
      y1 := Rect.y1;

  Result := (x1 <= x2) and (y1 <= y2);
end;

function TRectInteger.IsValid: Boolean;
begin
  Result := (x1 <= x2) and (y1 <= y2);
end;

procedure TRectInteger.Normalize;
var
  t: Integer;
begin
  if x1 > x2 then
    begin
      t := x1;
      x1 := x2;
      x2 := t;
    end;

  if y1 > y2 then
    begin
      t := y1;
      y1 := y2;
      y2 := t;
    end;
end;

{ TRectDouble }

{$IFNDEF FPC}


constructor TRectDouble.Create(x1, y1, x2, y2: Double);
begin
  Self.x1 := x1;
  Self.y1 := y1;
  Self.x2 := x2;
  Self.y2 := y2;
end;

constructor TRectDouble.Create(Rect: TRectDouble);
begin
  Self.x1 := Rect.x1;
  Self.y1 := Rect.y1;
  Self.x2 := Rect.x2;
  Self.y2 := Rect.y2;
end;
{$ENDIF}


function TRectDouble.GetCenterX: Double;
begin
  Result := 0.5 * (x1 + x2);
end;

function TRectDouble.GetCenterY: Double;
begin
  Result := 0.5 * (y1 + y2);
end;

function TRectInteger.GetHeight: Integer;
begin
  Result := Abs(x2 - x1);
end;

function TRectInteger.GetWidth: Integer;
begin
  Result := Abs(y2 - y1);
end;

procedure TRectDouble.Normalize;
var
  t: Double;
begin
  if x1 > x2 then
    begin
      t := x1;
      x1 := x2;
      x2 := t;
    end;

  if y1 > y2 then
    begin
      t := y1;
      y1 := y2;
      y2 := t;
    end;
end;

class operator TRectDouble.Equal(const Lhs, Rhs: TRectDouble): Boolean;
begin
  Result := (Lhs.Point1 = Rhs.Point1) and (Lhs.Point2 = Rhs.Point2)
end;

class operator TRectDouble.NotEqual(const Lhs, Rhs: TRectDouble): Boolean;
begin
  Result := (Lhs.Point1 <> Rhs.Point1) or (Lhs.Point2 <> Rhs.Point2)
end;

class operator TRectDouble.Subtract(const Lhs, Rhs: TRectDouble): TRectDouble;
begin
  Result.x1 := Lhs.x1 - Rhs.x1;
  Result.y1 := Lhs.y1 - Rhs.y1;
  Result.x2 := Lhs.x2 - Rhs.x2;
  Result.y2 := Lhs.y2 - Rhs.y2;
end;

class function TRectDouble.Zero: TRectDouble;
begin
  FillChar(Result, 32, 4);
end;

function TRectDouble.Clip(r: PRectDouble): Boolean;
begin
  if x2 > r.x2 then
      x2 := r.x2;

  if y2 > r.y2 then
      y2 := r.y2;

  if x1 < r.x1 then
      x1 := r.x1;

  if y1 < r.y1 then
      y1 := r.y1;

  Result := (x1 <= x2) and (y1 <= y2);
end;

class operator TRectDouble.Add(const Lhs, Rhs: TRectDouble): TRectDouble;
begin
  Result.x1 := Lhs.x1 + Rhs.x1;
  Result.y1 := Lhs.y1 + Rhs.y1;
  Result.x2 := Lhs.x2 + Rhs.x2;
  Result.y2 := Lhs.y2 + Rhs.y2;
end;

function TRectDouble.Clip(var r: TRectDouble): Boolean;
begin
  Result := Clip(@r)
end;

class operator TRectDouble.Implicit(const Value: TRectInteger): TRectDouble;
begin
  Result.x1 := Value.x1;
  Result.y1 := Value.y1;
  Result.x2 := Value.x2;
  Result.y2 := Value.y2;
end;

function TRectDouble.IsValid;
begin
  Result := (x1 <= x2) and (y1 <= y2);
end;

{ TVertex }

procedure NormalizeRect(var This: TRectInteger);
var
  t: Integer;
begin
  if This.x1 > This.x2 then
    begin
      t := This.x1;
      This.x1 := This.x2;
      This.x2 := t;
    end;

  if This.y1 > This.y2 then
    begin
      t := This.y1;
      This.y1 := This.y2;
      This.y2 := t;
    end;
end;

procedure NormalizeRectDouble(var This: TRectDouble);
var
  t: Double;
begin
  if This.x1 > This.x2 then
    begin
      t := This.x1;
      This.x1 := This.x2;
      This.x2 := t;
    end;

  if This.y1 > This.y2 then
    begin
      t := This.y1;
      This.y1 := This.y2;
      This.y2 := t;
    end;
end;

function ClipRect(var This: TRectInteger; r: PRectInteger): Boolean;
begin
  if This.x2 > r.x2 then
      This.x2 := r.x2;

  if This.y2 > r.y2 then
      This.y2 := r.y2;

  if This.x1 < r.x1 then
      This.x1 := r.x1;

  if This.y1 < r.y1 then
      This.y1 := r.y1;

  Result := (This.x1 <= This.x2) and (This.y1 <= This.y2);
end;

function ClipRectDouble(var This: TRectDouble; r: PRectDouble): Boolean;
begin
  if This.x2 > r.x2 then
      This.x2 := r.x2;

  if This.y2 > r.y2 then
      This.y2 := r.y2;

  if This.x1 < r.x1 then
      This.x1 := r.x1;

  if This.y1 < r.y1 then
      This.y1 := r.y1;

  Result := (This.x1 <= This.x2) and (This.y1 <= This.y2);
end;

function IsValidRect(var This: TRectInteger): Boolean;
begin
  Result := (This.x1 <= This.x2) and (This.y1 <= This.y2);
end;

function IsValidRectDouble(var This: TRectDouble): Boolean;
begin
  Result := (This.x1 <= This.x2) and (This.y1 <= This.y2);
end;

function IntersectRectangles(const r1, r2: TRectInteger): TRectInteger;
begin
  Result := r1;

  if Result.x2 > r2.x2 then
      Result.x2 := r2.x2;

  if Result.y2 > r2.y2 then
      Result.y2 := r2.y2;

  if Result.x1 < r2.x1 then
      Result.x1 := r2.x1;

  if Result.y1 < r2.y1 then
      Result.y1 := r2.y1;
end;

function IntersectRectanglesDouble(const r1, r2: TRectDouble): TRectDouble;
begin
  Result := r1;

  if Result.x2 > r2.x2 then
      Result.x2 := r2.x2;

  if Result.y2 > r2.y2 then
      Result.y2 := r2.y2;

  if Result.x1 < r2.x1 then
      Result.x1 := r2.x1;

  if Result.y1 < r2.y1 then
      Result.y1 := r2.y1;
end;

function UniteRectangles(const r1, r2: TRectInteger): TRectInteger;
begin
  Result := r1;

  if Result.x2 < r2.x2 then
      Result.x2 := r2.x2;

  if Result.y2 < r2.y2 then
      Result.y2 := r2.y2;

  if Result.x1 > r2.x1 then
      Result.x1 := r2.x1;

  if Result.y1 > r2.y1 then
      Result.y1 := r2.y1;
end;

function UniteRectanglesDouble(const r1, r2: TRectDouble): TRectDouble;
begin
  Result := r1;

  if Result.x2 < r2.x2 then
      Result.x2 := r2.x2;

  if Result.y2 < r2.y2 then
      Result.y2 := r2.y2;

  if Result.x1 > r2.x1 then
      Result.x1 := r2.x1;

  if Result.y1 > r2.y1 then
      Result.y1 := r2.y1;
end;

function IsVertex(Cx: Cardinal): Boolean;
begin
  Result := (Cx >= CAggPathCmdMoveTo) and (Cx < CAggPathCmdEndPoly);
end;

function IsDrawing(Cx: Cardinal): Boolean;
begin
  Result := (Cx >= CAggPathCmdLineTo) and (Cx < CAggPathCmdEndPoly);
end;

function IsStop(Cx: Cardinal): Boolean;
begin
  Result := (Cx = CAggPathCmdStop);
end;

function IsMove(Cx: Cardinal): Boolean;
begin
  Result := (Cx = CAggPathCmdMoveTo);
end;

function IsLineTo(Cx: Cardinal): Boolean;
begin
  Result := (Cx = CAggPathCmdLineTo);
end;

function IsMoveTo(Cx: Cardinal): Boolean;
begin
  Result := (Cx = CAggPathCmdMoveTo);
end;

function IsCurve(Cx: Cardinal): Boolean;
begin
  Result := (Cx = CAggPathCmdCurve3) or (Cx = CAggPathCmdCurve4);
end;

function IsCurve3(Cx: Cardinal): Boolean;
begin
  Result := (Cx = CAggPathCmdCurve3);
end;

function IsCurve4(Cx: Cardinal): Boolean;
begin
  Result := (Cx = CAggPathCmdCurve4);
end;

function IsEndPoly(Cx: Cardinal): Boolean;
begin
  Result := ((Cx and CAggPathCmdMask) = CAggPathCmdEndPoly);
end;

function IsClose(Cx: Cardinal): Boolean;
begin
  Result := (Cx and not(CAggPathFlagsCw or CAggPathFlagsCcw))
    = (CAggPathCmdEndPoly or CAggPathFlagsClose)
end;

function IsNextPoly(Cx: Cardinal): Boolean;
begin
  Result := IsStop(Cx) or IsMoveTo(Cx) or IsEndPoly(Cx);
end;

function IsClockwise(Cx: Cardinal): Boolean;
begin
  Result := not((Cx and CAggPathFlagsCw) = 0);
end;

function IsCounterClockwise(Cx: Cardinal): Boolean;
begin
  Result := not((Cx and CAggPathFlagsCcw) = 0);
end;

function IsOriented(Cx: Cardinal): Boolean;
begin
  Result := not((Cx and (CAggPathFlagsCw or CAggPathFlagsCcw)) = 0);
end;

function IsClosed(Cx: Cardinal): Boolean;
begin
  Result := not((Cx and CAggPathFlagsClose) = 0);
end;

function GetCloseFlag(Cx: Cardinal): Cardinal;
begin
  Result := Cx and CAggPathFlagsClose;
end;

function ClearOrientation(Cx: Cardinal): Cardinal;
begin
  Result := Cx and not(CAggPathFlagsCw or CAggPathFlagsCcw);
end;

function GetOrientation(Cx: Cardinal): Cardinal;
begin
  Result := Cx and (CAggPathFlagsCw or CAggPathFlagsCcw);
end;

function SetOrientation(Cx, O: Cardinal): Cardinal;
begin
  Result := ClearOrientation(Cx) or O;
end;

procedure SwapPointers(a, b: Pointer);
var
  Temp: Pointer;
begin
  Temp := PPointer(a)^;
  PPointer(a)^ := PPointer(b)^;
  PPointer(b)^ := Temp;
end;

function IntToDouble(i: Integer): Double;
begin
  Result := i;
end;

function RandomMinMax(Min, Max: Double): Double;
begin
  Result := (Max - Min) * Random + Min;
end;

function RectInteger(x1, y1, x2, y2: Integer): TRectInteger;
begin
  Result.x1 := x1;
  Result.y1 := y1;
  Result.x2 := x2;
  Result.y2 := y2;
end;

function RectInteger(Point1, Point2: TPointInteger): TRectInteger;
begin
  Result.Point1 := Point1;
  Result.Point2 := Point2;
end;

function RectDouble(x1, y1, x2, y2: Double): TRectDouble;
begin
  Result.x1 := x1;
  Result.y1 := y1;
  Result.x2 := x2;
  Result.y2 := y2;
end;

function RectDouble(Point1, Point2: TPointDouble): TRectDouble;
begin
  Result.Point1 := Point1;
  Result.Point2 := Point2;
end;

function PointInteger(x, y: Integer): TPointInteger;
begin
  Result.x := x;
  Result.y := y;
end;

function PointInteger(Value: Integer): TPointInteger;
begin
  Result.x := Value;
  Result.y := Value;
end;

function PointDouble(x, y: Double): TPointDouble;
begin
  Result.x := x;
  Result.y := y;
end;

function PointDouble(Value: Double): TPointDouble;
begin
  Result.x := Value;
  Result.y := Value;
end;

function PointIntegerOffset(Point: TPointInteger; Value: Integer)
  : TPointInteger;
begin
  Result.x := Point.x + Value;
  Result.y := Point.y + Value;
end;

function PointDoubleOffset(Point: TPointDouble; Value: Double): TPointDouble;
begin
  Result.x := Point.x + Value;
  Result.y := Point.y + Value;
end;

function PointIntegerScale(Point: TPointInteger; Value: Integer): TPointInteger;
begin
  Result.x := Point.x * Value;
  Result.y := Point.y * Value;
end;

function PointDoubleScale(Point: TPointDouble; Value: Double): TPointDouble;
begin
  Result.x := Point.x * Value;
  Result.y := Point.y * Value;
end;

function QuadDouble(RectDouble: TRectDouble): TQuadDouble;
begin
  Result.Points[0] := RectDouble.Point1;
  Result.values[2] := RectDouble.x2;
  Result.values[3] := RectDouble.y1;
  Result.Points[2] := RectDouble.Point2;
  Result.values[5] := RectDouble.y2;
  Result.values[6] := RectDouble.x1;
  Result.values[7] := RectDouble.y2;
end;

function EnsureRange(const Value, Min, Max: Integer): Integer;
begin
  Result := Value;
  if Result < Min then
      Result := Min;
  if Result > Max then
      Result := Max;
end;

function EnsureRange(const Value, Min, Max: Double): Double;
begin
  Result := Value;
  if Result < Min then
      Result := Min;
  if Result > Max then
      Result := Max;
end;

function UnsignedRound(v: Double): Cardinal;
begin
  Result := Cardinal(Trunc(v + 0.5));
end;

function IntegerRound(v: Double): Integer;
begin
  if v < 0.0 then
      Result := Integer(Trunc(v - 0.5))
  else
      Result := Integer(Trunc(v + 0.5));
end;

function SaturationIntegerRound(Limit: Integer; v: Double): Integer;
begin
  if v < -Limit then
      Result := -Limit
  else if v > Limit then
      Result := Limit
  else
      Result := IntegerRound(v);
end;

function ShrInt8(i, Shift: Int8): Int8;
begin
  Result := i div (1 shl Shift);
end;

function ShrInt16(i, Shift: Int16): Int16;
begin
  Result := i div (1 shl Shift);
end;

function ShrInt32(i, Shift: Integer): Integer;
begin
  Result := i div (1 shl Shift);
end;

procedure Fill32Bit(var x; Count: Cardinal; var Value);
var
  i: Integer;
  p: PIntegerArray;
begin
  p := PIntegerArray(@x);
  for i := Count - 1 downto 0 do
      p[i] := Integer(Value);
end;

end.
