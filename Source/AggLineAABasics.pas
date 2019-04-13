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
unit AggLineAABasics;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics;

const
  CAggLineSubpixelShift = 8;
  CAggLineSubpixelSize  = 1 shl CAggLineSubpixelShift;
  CAggLineSubpixelMask  = CAggLineSubpixelSize - 1;

  CAggLineMrSubpixelShift = 4;
  CAggLineMrSubpixelSize  = 1 shl CAggLineMrSubpixelShift;
  CAggLineMrSubpixelMask  = CAggLineMrSubpixelSize - 1;

type
  PAggLineParameters = ^TAggLineParameters;

  TAggLineParameters = record
    x1, y1, x2, y2, SX, SY: Integer;
    Delta: TPointInteger;
    Vertical: Boolean;
    IncValue, Len, Octant: Integer;
  public
    procedure Initialize(x1, y1, x2, y2, Len: Integer); overload;

    function OrthogonalQuadrant: Cardinal;
    function DiagonalQuadrant: Cardinal;

    function SameOrthogonalQuadrant(LP: PAggLineParameters): Boolean;
    function SameDiagonalQuadrant(LP: PAggLineParameters): Boolean;
  end;

function LineMedResolution(x: Integer): Integer;
function LineHighResolution(x: Integer): Integer;

function LineDoubleHighResolution(x: Integer): Integer;
function LineCoord(x: Double): Integer;

procedure Bisectrix(L1, l2: PAggLineParameters; x, y: PInteger);

procedure FixDegenerateBisectrixStart(LP: PAggLineParameters; x, y: PInteger);
procedure FixDegenerateBisectrixEnd(LP: PAggLineParameters; x, y: PInteger);

implementation


const
  // The number of the octant is determined as a 3-bit value as follows:
  // bit 0 = vertical flag
  // bit 1 = sx < 0
  // bit 2 = sy < 0
  //
  // [N] shows the number of the orthogonal quadrant
  // <M> shows the number of the diagonal quadrant
  // <1>
  // [1]          |          [0]
  // . (3)011 | 001(1) .
  // .      |      .
  // .    |    .
  // .  |  .
  // (2)010     .|.     000(0)
  // <2> ----------.+.----------- <0>
  // (6)110   .  |  .   100(4)
  // .    |    .
  // .      |      .
  // .        |        .
  // (7)111 | 101(5)
  // [2]          |          [3]
  // <3>
  // 0 ,1 ,2 ,3 ,4 ,5 ,6 ,7
  COrthogonalQuadrant: array [0 .. 7] of Int8u = (0, 0, 1, 1, 3, 3, 2, 2);
  CDiagonalQuadrant: array [0 .. 7] of Int8u   = (0, 1, 2, 1, 0, 3, 2, 3);

  { TAggLineParameters }

procedure TAggLineParameters.Initialize(x1, y1, x2, y2, Len: Integer);
begin
  Self.x1 := x1;
  Self.y1 := y1;
  Self.x2 := x2;
  Self.y2 := y2;
  Self.Delta.x := Abs(x2 - x1);
  Self.Delta.y := Abs(y2 - y1);

  if x2 > x1 then
      SX := 1
  else
      SX := -1;

  if y2 > y1 then
      SY := 1
  else
      SY := -1;

  Vertical := Self.Delta.y >= Self.Delta.x;

  if Vertical then
      IncValue := SY
  else
      IncValue := SX;

  Self.Len := Len;

  Octant := (SY and 4) or (SX and 2) or Integer(Vertical);
end;

function TAggLineParameters.OrthogonalQuadrant;
begin
  Result := COrthogonalQuadrant[Octant];
end;

function TAggLineParameters.DiagonalQuadrant;
begin
  Result := CDiagonalQuadrant[Octant];
end;

function TAggLineParameters.SameOrthogonalQuadrant;
begin
  Result := COrthogonalQuadrant[Octant] = COrthogonalQuadrant[LP.Octant];
end;

function TAggLineParameters.SameDiagonalQuadrant;
begin
  Result := CDiagonalQuadrant[Octant] = CDiagonalQuadrant[LP.Octant];
end;

function LineMedResolution;
begin
  Result := ShrInt32(x, CAggLineSubpixelShift - CAggLineMrSubpixelShift);
end;

function LineHighResolution;
begin
  Result := x shl (CAggLineSubpixelShift - CAggLineMrSubpixelShift);
end;

function LineDoubleHighResolution;
begin
  Result := x shl CAggLineSubpixelShift;
end;

function LineCoord;
begin
  Result := Trunc(x * CAggLineSubpixelSize);
end;

procedure Bisectrix(L1, l2: PAggLineParameters; x, y: PInteger);
var
  k, TX, TY, dx, dy: Double;
begin
  k := l2.Len / L1.Len;
  TX := l2.x2 - (l2.x1 - L1.x1) * k;
  TY := l2.y2 - (l2.y1 - L1.y1) * k;

  // All bisectrices must be on the right of the line
  // If the next point is on the left (l1 => l2.2)
  // then the bisectix should be rotated by 180 degrees.
  dx := l2.x2 - l2.x1;
  dy := l2.y2 - l2.y1;
  if dx * (l2.y1 - L1.y1) < dy * (l2.x1 - L1.x1) + 100.0 then
    begin
      TX := TX - 2 * (TX - l2.x1);
      TY := TY - 2 * (TY - l2.y1);
    end;

  // Check if the bisectrix is too short
  dx := TX - l2.x1;
  dy := TY - l2.y1;

  if Trunc(Sqrt(Sqr(dx) + Sqr(dy))) < CAggLineSubpixelSize then
    begin
      x^ := ShrInt32(l2.x1 + l2.x1 + (l2.y1 - L1.y1) + (l2.y2 - l2.y1), 1);
      y^ := ShrInt32(l2.y1 + l2.y1 - (l2.x1 - L1.x1) - (l2.x2 - l2.x1), 1);

      Exit;
    end;

  x^ := Trunc(TX);
  y^ := Trunc(TY);
end;

procedure FixDegenerateBisectrixStart(LP: PAggLineParameters;
  x, y: PInteger);
var
  d: Integer;
begin
  Assert(Assigned(LP));
  d := Trunc((IntToDouble(x^ - LP.x2) * IntToDouble(LP.y2 - LP.y1) -
    IntToDouble(y^ - LP.y2) * IntToDouble(LP.x2 - LP.x1)) / LP.Len);

  if d < CAggLineSubpixelSize then
    begin
      x^ := LP.x1 + (LP.y2 - LP.y1);
      y^ := LP.y1 - (LP.x2 - LP.x1);
    end;
end;

procedure FixDegenerateBisectrixEnd(LP: PAggLineParameters; x, y: PInteger);
var
  d: Integer;
begin
  Assert(Assigned(LP));
  d := Trunc((IntToDouble(x^ - LP.x2) * IntToDouble(LP.y2 - LP.y1) -
    IntToDouble(y^ - LP.y2) * IntToDouble(LP.x2 - LP.x1)) / LP.Len);

  if d < CAggLineSubpixelSize then
    begin
      x^ := LP.x2 + (LP.y2 - LP.y1);
      y^ := LP.y2 - (LP.x2 - LP.x1);
    end;
end;

end. 
 
 
