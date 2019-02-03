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
unit AggSpanInterpolatorLinear;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggDdaLine,
  AggTransAffine;

type
  TAggSpanInterpolator = class
  protected
    FSubpixelShift, FSubpixelSize: Cardinal;
    function GetTransformer: TAggTransAffine; virtual; abstract;
    procedure SetTransformer(Trans: TAggTransAffine); virtual; abstract;
  public
    constructor Create(SS: Cardinal = 8); virtual;

    procedure SetBegin(x, y: Double; Len: Cardinal); virtual; abstract;

    procedure Resynchronize(XE, Ye: Double; Len: Cardinal); virtual; abstract;

    procedure IncOperator; virtual; abstract;
    procedure Coordinates(x, y: PInteger); overload; virtual; abstract;
    procedure Coordinates(var x, y: Integer); overload; virtual;

    procedure LocalScale(x, y: PInteger); virtual;

    property SubpixelShift: Cardinal read FSubpixelShift;
    property Transformer: TAggTransAffine read GetTransformer write SetTransformer;
  end;

  TAggSpanInterpolatorLinear = class(TAggSpanInterpolator)
  private
    FTrans: TAggTransAffine;
    FLineInterpolatorX, FLineInterpolatorY: TAggDda2LineInterpolator;
  protected
    function GetTransformer: TAggTransAffine; override;
    procedure SetTransformer(Trans: TAggTransAffine); override;
  public
    constructor Create(SS: Cardinal = 8); overload; override;
    constructor Create(Trans: TAggTransAffine; SS: Cardinal = 8); overload;
    constructor Create(Trans: TAggTransAffine; x, y: Double; Len: Cardinal; SS: Cardinal = 8); overload;

    procedure SetBegin(x, y: Double; Len: Cardinal); override;

    procedure Resynchronize(XE, Ye: Double; Len: Cardinal); override;

    procedure IncOperator; override;
    procedure Coordinates(x, y: PInteger); override;
    procedure Coordinates(var x, y: Integer); override;
  end;

  TAggSpanInterpolatorLinearSubdiv = class(TAggSpanInterpolator)
  private
    FSubdivShift, FSubdivSize, FSubdivMask: Cardinal;

    FTrans: TAggTransAffine;
    FLineInterpolatorX, FLineInterpolatorY: TAggDda2LineInterpolator;

    FSourceX: Integer;
    FSourceY: Double;
    fPos, FLength: Cardinal;

    procedure SetSubdivShift(Shift: Cardinal);
  protected
    function GetTransformer: TAggTransAffine; override;
    procedure SetTransformer(Trans: TAggTransAffine); override;
  public
    constructor Create(SS: Cardinal = 8); overload; override;
    constructor Create(Trans: TAggTransAffine; ASubdivShift: Cardinal = 4; SS: Cardinal = 8); overload;
    constructor Create(Trans: TAggTransAffine; x, y: Double; Len: Cardinal; ASubdivShift: Cardinal = 4; SS: Cardinal = 8); overload;

    procedure SetBegin(x, y: Double; Len: Cardinal); override;

    procedure IncOperator; override;
    procedure Coordinates(x, y: PInteger); override;
    procedure Coordinates(var x, y: Integer); override;

    property SubdivShift: Cardinal read FSubdivShift write SetSubdivShift;
  end;

implementation


{ TAggSpanInterpolator }

constructor TAggSpanInterpolator.Create(SS: Cardinal = 8);
begin
  FSubpixelShift := SS;
  FSubpixelSize := 1 shl FSubpixelShift;
end;

procedure TAggSpanInterpolator.Coordinates(var x, y: Integer);
begin
  Coordinates(@x, @y);
end;

procedure TAggSpanInterpolator.LocalScale;
begin
end;

{ TAggSpanInterpolatorLinear }

constructor TAggSpanInterpolatorLinear.Create(SS: Cardinal = 8);
begin
  inherited Create(SS);
end;

constructor TAggSpanInterpolatorLinear.Create(Trans: TAggTransAffine;
  SS: Cardinal = 8);
begin
  Create(SS);

  FTrans := Trans;
end;

constructor TAggSpanInterpolatorLinear.Create(Trans: TAggTransAffine;
  x, y: Double; Len: Cardinal; SS: Cardinal = 8);
begin
  Create(Trans, SS);

  SetBegin(x, y, Len);
end;

function TAggSpanInterpolatorLinear.GetTransformer;
begin
  Result := FTrans;
end;

procedure TAggSpanInterpolatorLinear.SetTransformer;
begin
  FTrans := Trans;
end;

procedure TAggSpanInterpolatorLinear.SetBegin(x, y: Double; Len: Cardinal);
var
  TX, TY: Double;
  x1, y1, x2, y2: Integer;
begin
  TX := x;
  TY := y;

  FTrans.Transform(FTrans, @TX, @TY);

  x1 := Trunc(TX * FSubpixelSize);
  y1 := Trunc(TY * FSubpixelSize);

  TX := x + Len;
  TY := y;

  FTrans.Transform(FTrans, @TX, @TY);

  x2 := Trunc(TX * FSubpixelSize);
  y2 := Trunc(TY * FSubpixelSize);

  FLineInterpolatorX.Initialize(x1, x2, Len);
  FLineInterpolatorY.Initialize(y1, y2, Len);
end;

procedure TAggSpanInterpolatorLinear.Resynchronize;
begin
  FTrans.Transform(FTrans, @XE, @Ye);

  FLineInterpolatorX.Initialize(FLineInterpolatorX.y, Trunc(XE * FSubpixelSize), Len);
  FLineInterpolatorY.Initialize(FLineInterpolatorY.y, Trunc(Ye * FSubpixelSize), Len);
end;

procedure TAggSpanInterpolatorLinear.IncOperator;
begin
  FLineInterpolatorX.PlusOperator;
  FLineInterpolatorY.PlusOperator;
end;

procedure TAggSpanInterpolatorLinear.Coordinates(x, y: PInteger);
begin
  x^ := FLineInterpolatorX.y;
  y^ := FLineInterpolatorY.y;
end;

procedure TAggSpanInterpolatorLinear.Coordinates(var x, y: Integer);
begin
  x := FLineInterpolatorX.y;
  y := FLineInterpolatorY.y;
end;

{ TAggSpanInterpolatorLinearSubdiv }

constructor TAggSpanInterpolatorLinearSubdiv.Create(SS: Cardinal = 8);
begin
  inherited Create(SS);

  FSubdivShift := 4;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;
end;

constructor TAggSpanInterpolatorLinearSubdiv.Create(Trans: TAggTransAffine;
  ASubdivShift: Cardinal = 4; SS: Cardinal = 8);
begin
  inherited Create(SS);

  FSubdivShift := ASubdivShift;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;

  FTrans := Trans;
end;

constructor TAggSpanInterpolatorLinearSubdiv.Create(Trans: TAggTransAffine;
  x, y: Double; Len: Cardinal; ASubdivShift: Cardinal = 4; SS: Cardinal = 8);
begin
  Create(Trans, ASubdivShift, SS);

  SetBegin(x, y, Len);
end;

function TAggSpanInterpolatorLinearSubdiv.GetTransformer;
begin
  Result := FTrans;
end;

procedure TAggSpanInterpolatorLinearSubdiv.SetTransformer;
begin
  FTrans := Trans;
end;

procedure TAggSpanInterpolatorLinearSubdiv.SetSubdivShift;
begin
  FSubdivShift := Shift;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;
end;

procedure TAggSpanInterpolatorLinearSubdiv.SetBegin;
var
  TX, TY: Double;
  x1, y1: Integer;
begin
  fPos := 1;
  FSourceX := Trunc(x * FSubpixelSize) + FSubpixelSize;
  FSourceY := y;
  FLength := Len;

  if Len > FSubdivSize then
      Len := FSubdivSize;

  TX := x;
  TY := y;

  FTrans.Transform(FTrans, @TX, @TY);

  x1 := Trunc(TX * FSubpixelSize);
  y1 := Trunc(TY * FSubpixelSize);

  TX := x + Len;
  TY := y;

  FTrans.Transform(FTrans, @TX, @TY);

  FLineInterpolatorX.Initialize(x1, Trunc(TX * FSubpixelSize), Len);
  FLineInterpolatorY.Initialize(y1, Trunc(TY * FSubpixelSize), Len);
end;

procedure TAggSpanInterpolatorLinearSubdiv.IncOperator;
var
  TX, TY: Double;
  Len: Cardinal;
begin
  FLineInterpolatorX.PlusOperator;
  FLineInterpolatorY.PlusOperator;

  if fPos >= FSubdivSize then
    begin
      Len := FLength;

      if Len > FSubdivSize then
          Len := FSubdivSize;

      TX := FSourceX / FSubpixelSize + Len;
      TY := FSourceY;

      FTrans.Transform(FTrans, @TX, @TY);

      FLineInterpolatorX.Initialize(FLineInterpolatorX.y, Trunc(TX * FSubpixelSize), Len);
      FLineInterpolatorY.Initialize(FLineInterpolatorY.y, Trunc(TY * FSubpixelSize), Len);

      fPos := 0;
    end;

  inc(FSourceX, FSubpixelSize);
  inc(fPos);
  dec(FLength);
end;

procedure TAggSpanInterpolatorLinearSubdiv.Coordinates(x, y: PInteger);
begin
  x^ := FLineInterpolatorX.y;
  y^ := FLineInterpolatorY.y;
end;

procedure TAggSpanInterpolatorLinearSubdiv.Coordinates(var x, y: Integer);
begin
  x := FLineInterpolatorX.y;
  y := FLineInterpolatorY.y;
end;

end. 
 
 
