{ ****************************************************************************** }
{ * memory Rasterization with AGG support                                      * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
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
unit AggGradientLut;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggArray,
  AggDdaLine,
  AggColor32;

type
  TAggGradientLut = class(TAggCustomArray)
  private
    FColorLutSize: Cardinal;
    FColorProfile: TAggPodBVector;
    FColorLut: TAggPodArray;
  protected
    function GetSize: Cardinal; override;
    function GetEntry: Cardinal; override;

    // Size-index Interface. This class can be used directly as the
    // ColorF in SpanGradient. All it needs is two access methods
    // size() and operator [].
    function ArrayOperator(index: Cardinal): Pointer; override;
    property ItemPointer[index: Cardinal]: Pointer read ArrayOperator; default;
  public
    constructor Create(ASize: Cardinal = 256);
    destructor Destroy; override;

    // Build Gradient Lut
    // First, call RemoveAll(), then addColor() at least twice,
    // then BuildLut(). Argument "offset" in addColor must be
    // in range [0...1] and defines a color stop as it is described
    // in SVG specification, section Gradients and Patterns.
    // The simplest linear Gradient is:
    // TAggGradientLut.addColor(0.0, startColor);
    // TAggGradientLut.addColor(1.0, endColor);
    procedure RemoveAll;
    procedure AddColor(Offset: Double; COLOR: PAggColor);

    procedure BuildLut;
  end;

implementation

type
  PAggColorPoint = ^TAggColorPoint;

  TAggColorPoint = record
    Offset: Double;
    COLOR: TAggColor;
  end;

  TAggColorInterpolator = record
    FC1, FC2: TAggColor;
    FLength, FCount: Cardinal;
    v, r, g, b, a: TAggDdaLineInterpolator;
    FIsGray: Boolean;
  public
    procedure Initialize(c1, c2: PAggColor; Len: Cardinal;
      IsGray: Boolean = False);

    procedure OperatorInc;

    function COLOR: TAggColor;
  end;

  { TAggColorInterpolator }

procedure TAggColorInterpolator.Initialize(c1, c2: PAggColor; Len: Cardinal;
  IsGray: Boolean = False);
begin
  FC1 := c1^;
  FC2 := c2^;

  FLength := Len;
  FCount := 0;

  FIsGray := IsGray;

  if FIsGray then
      v.Initialize(c1.v, c2.v, Len, 14)

  else
    begin
      r.Initialize(c1.Rgba8.r, c2.Rgba8.r, Len, 14);
      g.Initialize(c1.Rgba8.g, c2.Rgba8.g, Len, 14);
      b.Initialize(c1.Rgba8.b, c2.Rgba8.b, Len, 14);
    end;

  a.Initialize(c1.Rgba8.a, c2.Rgba8.a, Len, 14);
end;

procedure TAggColorInterpolator.OperatorInc;
begin
  inc(FCount);

  if FIsGray then
      v.PlusOperator
  else
    begin
      r.PlusOperator;
      g.PlusOperator;
      b.PlusOperator;
    end;

  a.PlusOperator;
end;

function TAggColorInterpolator.COLOR: TAggColor;
begin
  if FIsGray then
      Result.FromValueInteger(r.y, a.y)
  else
      Result.FromRgbaInteger(r.y, g.y, b.y, a.y)
end;

{ TAggGradientLut }

constructor TAggGradientLut.Create(ASize: Cardinal = 256);
begin
  FColorLutSize := ASize;

  FColorProfile := TAggPodBVector.Create(SizeOf(TAggColorPoint), 4);
  FColorLut := TAggPodArray.Create(SizeOf(TAggColor), FColorLutSize);
end;

destructor TAggGradientLut.Destroy;
begin
  FColorProfile.Free;
  FColorLut.Free;
  inherited
end;

procedure TAggGradientLut.RemoveAll;
begin
  FColorProfile.RemoveAll;
end;

procedure TAggGradientLut.AddColor(Offset: Double; COLOR: PAggColor);
var
  cp: TAggColorPoint;
begin
  if Offset < 0.0 then
      Offset := 0.0;

  if Offset > 1.0 then
      Offset := 1.0;

  cp.COLOR := COLOR^;
  cp.Offset := Offset;

  FColorProfile.Add(@cp);
end;

function OffsetLess(a, b: PAggColorPoint): Boolean;
begin
  Result := a.Offset < b.Offset;
end;

function OffsetEqual(a, b: PAggColorPoint): Boolean;
begin
  Result := a.Offset = b.Offset;
end;

procedure TAggGradientLut.BuildLut;
var
  i, Start, stop: Cardinal;
  c: TAggColor;
  CI: TAggColorInterpolator;
begin
  QuickSort(FColorProfile, @OffsetLess);
  FColorProfile.CutAt(RemoveDuplicates(FColorProfile, @OffsetEqual));

  if FColorProfile.Size >= 2 then
    begin
      Start := UnsignedRound(PAggColorPoint(FColorProfile[0]).Offset *
        FColorLutSize);

      c := PAggColorPoint(FColorProfile[0]).COLOR;
      i := 0;

      while i < Start do
        begin
          PAggColor(FColorLut[i])^ := c;

          inc(i);
        end;

      i := 1;

      while i < FColorProfile.Size do
        begin
          stop := UnsignedRound(PAggColorPoint(FColorProfile[i]).Offset *
            FColorLutSize);

          CI.Initialize(@PAggColorPoint(FColorProfile[i - 1]).COLOR,
            @PAggColorPoint(FColorProfile[i]).COLOR, stop - Start + 1);

          while Start < stop do
            begin
              PAggColor(FColorLut[Start])^ := CI.COLOR;

              CI.OperatorInc;

              inc(Start);
            end;

          inc(i);
        end;

      c := PAggColorPoint(FColorProfile.Last).COLOR;

      while stop < FColorLut.Size do
        begin
          PAggColor(FColorLut[stop])^ := c;

          inc(stop);
        end;
    end;
end;

function TAggGradientLut.GetSize: Cardinal;
begin
  Result := FColorLutSize;
end;

function TAggGradientLut.GetEntry: Cardinal;
begin
  Result := FColorLut.EntrySize;
end;

function TAggGradientLut.ArrayOperator(index: Cardinal): Pointer;
begin
  Result := FColorLut[index];
end;

end. 
 
 
 
