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
unit AggRasterizerScanlineClip;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggClipLiangBarsky,
  AggRasterizerCellsAA;

const
  CAggPolyMaxCoord = (1 shl 30) - 1;
  CMaxStack        = 4;

type
  TAggRasterizerConv = class
  public
    function MulDiv(a, b, c: Double): Pointer; virtual; abstract;

    function XI(v: Pointer): Integer; virtual; abstract;
    function Yi(v: Pointer): Integer; virtual; abstract;

    function Upscale(v: Double): Pointer; virtual; abstract;
    function Downscale(v: Integer): Pointer; virtual; abstract;
  end;

  TAggRasConvInt = class(TAggRasterizerConv)
  private
    FResult: array [1 .. CMaxStack] of Integer;
    FStack: Integer;
  public
    constructor Create;

    function MulDiv(a, b, c: Double): Pointer; override;

    function XI(v: Pointer): Integer; override;
    function Yi(v: Pointer): Integer; override;

    function Upscale(v: Double): Pointer; override;
    function Downscale(v: Integer): Pointer; override;
  end;

  TAggRasConvIntSat = class(TAggRasterizerConv)
  private
    FResult: array [1 .. CMaxStack] of Integer;
    FStack: Integer;
  public
    constructor Create;

    function MulDiv(a, b, c: Double): Pointer; override;

    function XI(v: Pointer): Integer; override;
    function Yi(v: Pointer): Integer; override;

    function Upscale(v: Double): Pointer; override;
    function Downscale(v: Integer): Pointer; override;
  end;

  TAggRasConvInt3x = class(TAggRasterizerConv)
  private
    FResult: array [1 .. CMaxStack] of Integer;
    FStack: Integer;
  public
    constructor Create;

    function MulDiv(a, b, c: Double): Pointer; override;

    function XI(v: Pointer): Integer; override;
    function Yi(v: Pointer): Integer; override;

    function Upscale(v: Double): Pointer; override;
    function Downscale(v: Integer): Pointer; override;
  end;

  TRasConvDouble = class(TAggRasterizerConv)
  private
    FResult: array [1 .. CMaxStack] of Double;
    FStack: Integer;
  public
    constructor Create;

    function MulDiv(a, b, c: Double): Pointer; override;

    function XI(v: Pointer): Integer; override;
    function Yi(v: Pointer): Integer; override;

    function Upscale(v: Double): Pointer; override;
    function Downscale(v: Integer): Pointer; override;
  end;

  TRasConvDouble3x = class(TAggRasterizerConv)
  private
    FResult: array [1 .. CMaxStack] of Double;
    FStack: Integer;
  public
    constructor Create;

    function MulDiv(a, b, c: Double): Pointer; override;

    function XI(v: Pointer): Integer; override;
    function Yi(v: Pointer): Integer; override;

    function Upscale(v: Double): Pointer; override;
    function Downscale(v: Integer): Pointer; override;
  end;

  TAggRasterizerScanLineClip = class
  protected
    function GetConverterType: TAggRasterizerConv; virtual; abstract;
  public
    procedure ResetClipping; virtual; abstract;
    procedure SetClipBox(x1, y1, x2, y2: Pointer); overload; virtual; abstract;
    procedure SetClipBox(Bounds: Pointer); overload; virtual; abstract;
    procedure MoveTo(x1, y1: Pointer); virtual; abstract;
    procedure LineTo(Ras: TAggRasterizerCellsAA; x2, y2: Pointer); virtual; abstract;

    property ConverterType: TAggRasterizerConv read GetConverterType;
  end;

  TAggRasterizerScanLineClipInteger = class(TAggRasterizerScanLineClip)
  private
    RasterizerConverter: TAggRasterizerConv;
    FClipBox: TRectInteger;
    FX1, FY1: Integer;
    FF1: Cardinal;
    FClipping: Boolean;
    procedure LineClipY(Ras: TAggRasterizerCellsAA; x1, y1, x2, y2: Integer;
      f1, f2: Cardinal);
  protected
    function GetConverterType: TAggRasterizerConv; override;
  public
    constructor Create(conv: TAggRasterizerConv);

    procedure ResetClipping; override;
    procedure SetClipBox(x1, y1, x2, y2: Pointer); override;
    procedure SetClipBox(Bounds: Pointer); override;
    procedure MoveTo(x1, y1: Pointer); override;
    procedure LineTo(Ras: TAggRasterizerCellsAA; x2, y2: Pointer); override;
  end;

  TAggRasterizerScanLineClipDouble = class(TAggRasterizerScanLineClip)
  private
    RasterizerConverter: TAggRasterizerConv;
    FClipBox: TRectDouble;
    FX1, FY1: Double;
    FF1: Cardinal;
    FClipping: Boolean;
    procedure LineClipY(Ras: TAggRasterizerCellsAA; x1, y1, x2, y2: Double;
      f1, f2: Cardinal);
  protected
    function GetConverterType: TAggRasterizerConv; override;
  public
    constructor Create(conv: TAggRasterizerConv);

    procedure ResetClipping; override;
    procedure SetClipBox(x1, y1, x2, y2: Pointer); override;
    procedure SetClipBox(Bounds: Pointer); override;
    procedure MoveTo(x1, y1: Pointer); override;
    procedure LineTo(Ras: TAggRasterizerCellsAA; x2, y2: Pointer); override;
  end;

  TAggRasterizerScanLineNoClip = class(TAggRasterizerScanLineClip)
  private
    FX1, FY1: Integer;
    FConv: TAggRasConvInt;
  protected
    function GetConverterType: TAggRasterizerConv; override;
  public
    constructor Create;

    procedure ResetClipping; override;
    procedure SetClipBox(x1, y1, x2, y2: Pointer); override;
    procedure SetClipBox(Bounds: Pointer); override;
    procedure MoveTo(x1, y1: Pointer); override;
    procedure LineTo(Ras: TAggRasterizerCellsAA; x2, y2: Pointer); override;
  end;

  TAggRasterizerScanLineClipInt = class(TAggRasterizerScanLineClipInteger)
  private
    FConv: TAggRasConvInt;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAggRasterizerScanLineClipIntegerSat = class(TAggRasterizerScanLineClipInteger)
  private
    FConv: TAggRasConvIntSat;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAggRasterizerScanLineClipInteger3x = class(TAggRasterizerScanLineClipInteger)
  private
    FConv: TAggRasConvInt3x;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAggRasterizerScanLineDoubleClip = class(TAggRasterizerScanLineClipDouble)
  private
    FConv: TRasConvDouble;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAggRasterizerScanLineClipDouble3x = class(TAggRasterizerScanLineClipDouble)
  private
    FConv: TRasConvDouble3x;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation


{ TAggRasConvInt }

constructor TAggRasConvInt.Create;
begin
  FStack := 1;
  inherited;
end;

function TAggRasConvInt.MulDiv(a, b, c: Double): Pointer;
begin
  FResult[FStack] := IntegerRound(a * b / c);

  Result := @FResult[FStack];

  inc(FStack);

  if FStack > CMaxStack then
      FStack := 1;
end;

function TAggRasConvInt.XI(v: Pointer): Integer;
begin
  Result := PInteger(v)^;
end;

function TAggRasConvInt.Yi(v: Pointer): Integer;
begin
  Result := PInteger(v)^;
end;

function TAggRasConvInt.Upscale(v: Double): Pointer;
begin
  FResult[FStack] := IntegerRound(v * CAggPolySubpixelScale);

  Result := @FResult[FStack];

  inc(FStack);

  if FStack > CMaxStack then
      FStack := 1;
end;

function TAggRasConvInt.Downscale(v: Integer): Pointer;
begin
  FResult[FStack] := v;

  Result := @FResult[FStack];

  inc(FStack);

  if FStack > CMaxStack then
      FStack := 1;
end;

{ TAggRasConvIntSat }

constructor TAggRasConvIntSat.Create;
begin
  FStack := 1;
end;

function TAggRasConvIntSat.MulDiv(a, b, c: Double): Pointer;
begin
  FResult[FStack] := SaturationIntegerRound(CAggPolyMaxCoord, a * b / c);

  Result := @FResult[FStack];

  inc(FStack);

  if FStack > CMaxStack then
      FStack := 1;
end;

function TAggRasConvIntSat.XI(v: Pointer): Integer;
begin
  Result := PInteger(v)^;
end;

function TAggRasConvIntSat.Yi(v: Pointer): Integer;
begin
  Result := PInteger(v)^;
end;

function TAggRasConvIntSat.Upscale(v: Double): Pointer;
begin
  FResult[FStack] := SaturationIntegerRound(CAggPolyMaxCoord,
    v * CAggPolySubpixelScale);

  Result := @FResult[FStack];

  inc(FStack);

  if FStack > CMaxStack then
      FStack := 1;
end;

function TAggRasConvIntSat.Downscale(v: Integer): Pointer;
begin
  FResult[FStack] := v;

  Result := @FResult[FStack];

  inc(FStack);

  if FStack > CMaxStack then
      FStack := 1;
end;

{ TAggRasConvInt3x }

constructor TAggRasConvInt3x.Create;
begin
  FStack := 1;
end;

function TAggRasConvInt3x.MulDiv(a, b, c: Double): Pointer;
begin
  FResult[FStack] := IntegerRound(a * b / c);

  Result := @FResult[FStack];

  inc(FStack);

  if FStack > CMaxStack then
      FStack := 1;
end;

function TAggRasConvInt3x.XI(v: Pointer): Integer;
begin
  Result := PInteger(v)^ * 3;
end;

function TAggRasConvInt3x.Yi(v: Pointer): Integer;
begin
  Result := PInteger(v)^;
end;

function TAggRasConvInt3x.Upscale(v: Double): Pointer;
begin
  FResult[FStack] := IntegerRound(v * CAggPolySubpixelScale);

  Result := @FResult[FStack];

  inc(FStack);

  if FStack > CMaxStack then
      FStack := 1;
end;

function TAggRasConvInt3x.Downscale(v: Integer): Pointer;
begin
  FResult[FStack] := v;

  Result := @FResult[FStack];

  inc(FStack);

  if FStack > CMaxStack then
      FStack := 1;
end;

{ TRasConvDouble }

constructor TRasConvDouble.Create;
begin
  FStack := 1;
end;

function TRasConvDouble.MulDiv(a, b, c: Double): Pointer;
begin
  FResult[FStack] := a * b / c;

  Result := @FResult[FStack];

  inc(FStack);

  if FStack > CMaxStack then
      FStack := 1;
end;

function TRasConvDouble.XI(v: Pointer): Integer;
begin
  Result := IntegerRound(PDouble(v)^ * CAggPolySubpixelScale);
end;

function TRasConvDouble.Yi(v: Pointer): Integer;
begin
  Result := IntegerRound(PDouble(v)^ * CAggPolySubpixelScale);
end;

function TRasConvDouble.Upscale(v: Double): Pointer;
begin
  FResult[FStack] := v;

  Result := @FResult[FStack];

  inc(FStack);

  if FStack > CMaxStack then
      FStack := 1;
end;

function TRasConvDouble.Downscale(v: Integer): Pointer;
begin
  FResult[FStack] := v / CAggPolySubpixelScale;

  Result := @FResult[FStack];

  inc(FStack);

  if FStack > CMaxStack then
      FStack := 1;
end;

{ TRasConvDouble3x }

constructor TRasConvDouble3x.Create;
begin
  FStack := 1;
end;

function TRasConvDouble3x.MulDiv(a, b, c: Double): Pointer;
begin
  FResult[FStack] := a * b / c;

  Result := @FResult[FStack];

  inc(FStack);

  if FStack > CMaxStack then
      FStack := 1;
end;

function TRasConvDouble3x.XI(v: Pointer): Integer;
begin
  Result := IntegerRound(PDouble(v)^ * CAggPolySubpixelScale * 3);
end;

function TRasConvDouble3x.Yi(v: Pointer): Integer;
begin
  Result := IntegerRound(PDouble(v)^ * CAggPolySubpixelScale);
end;

function TRasConvDouble3x.Upscale(v: Double): Pointer;
begin
  FResult[FStack] := v;

  Result := @FResult[FStack];

  inc(FStack);

  if FStack > CMaxStack then
      FStack := 1;
end;

function TRasConvDouble3x.Downscale(v: Integer): Pointer;
begin
  FResult[FStack] := v / CAggPolySubpixelScale;

  Result := @FResult[FStack];

  inc(FStack);

  if FStack > CMaxStack then
      FStack := 1;
end;

{ TAggRasterizerScanLineClipInteger }

constructor TAggRasterizerScanLineClipInteger.Create(conv: TAggRasterizerConv);
begin
  RasterizerConverter := conv;

  FClipBox := RectInteger(0, 0, 0, 0);

  FX1 := 0;
  FY1 := 0;
  FF1 := 0;

  FClipping := False;
end;

procedure TAggRasterizerScanLineClipInteger.ResetClipping;
begin
  FClipping := False;
end;

procedure TAggRasterizerScanLineClipInteger.SetClipBox(Bounds: Pointer);
begin
  FClipBox := PRectInteger(Bounds)^;
  FClipBox.Normalize;

  FClipping := True;
end;

procedure TAggRasterizerScanLineClipInteger.SetClipBox(x1, y1, x2, y2: Pointer);
begin
  FClipBox := RectInteger(PInteger(x1)^, PInteger(y1)^, PInteger(x2)^,
    PInteger(y2)^);
  FClipBox.Normalize;

  FClipping := True;
end;

procedure TAggRasterizerScanLineClipInteger.MoveTo(x1, y1: Pointer);
begin
  FX1 := PInteger(x1)^;
  FY1 := PInteger(y1)^;

  if FClipping then
      FF1 := ClippingFlagsInteger(PInteger(x1)^, PInteger(y1)^, FClipBox);
end;

procedure TAggRasterizerScanLineClipInteger.LineTo(Ras: TAggRasterizerCellsAA;
  x2, y2: Pointer);
var
  f1, f2, F3, F4: Cardinal;
  x1, y1, y3, y4: Integer;

begin
  if FClipping then
    begin
      f2 := ClippingFlagsInteger(PInteger(x2)^, PInteger(y2)^, FClipBox);

      // Invisible by Y
      if ((FF1 and 10) = (f2 and 10)) and (FF1 and 10 <> 0) then
        begin
          FX1 := PInteger(x2)^;
          FY1 := PInteger(y2)^;
          FF1 := f2;

          Exit;
        end;

      x1 := FX1;
      y1 := FY1;
      f1 := FF1;

      case ((f1 and 5) shl 1) or (f2 and 5) of
        // Visible by X
        0:
          LineClipY(Ras, x1, y1, PInteger(x2)^, PInteger(y2)^, f1, f2);

        // x2 > clip.x2
        1:
          begin
            y3 := y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.x2 - x1,
              PInteger(y2)^ - y1, PInteger(x2)^ - x1))^;
            F3 := ClippingFlagsYInteger(y3, FClipBox);

            LineClipY(Ras, x1, y1, FClipBox.x2, y3, f1, F3);
            LineClipY(Ras, FClipBox.x2, y3, FClipBox.x2,
              PInteger(y2)^, F3, f2);
          end;

        // x1 > clip.x2
        2:
          begin
            y3 := y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.x2 - x1,
              PInteger(y2)^ - y1, PInteger(x2)^ - x1))^;
            F3 := ClippingFlagsYInteger(y3, FClipBox);

            LineClipY(Ras, FClipBox.x2, y1, FClipBox.x2, y3, f1, F3);
            LineClipY(Ras, FClipBox.x2, y3, PInteger(x2)^,
              PInteger(y2)^, F3, f2);
          end;

        // x1 > clip.x2 && x2 > clip.x2
        3:
          LineClipY(Ras, FClipBox.x2, y1, FClipBox.x2,
            PInteger(y2)^, f1, f2);

        // x2 < clip.x1
        4:
          begin
            y3 := y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.x1 - x1,
              PInteger(y2)^ - y1, PInteger(x2)^ - x1))^;
            F3 := ClippingFlagsYInteger(y3, FClipBox);

            LineClipY(Ras, x1, y1, FClipBox.x1, y3, f1, F3);
            LineClipY(Ras, FClipBox.x1, y3, FClipBox.x1,
              PInteger(y2)^, F3, f2);
          end;

        // x1 > clip.x2 && x2 < clip.x1
        6:
          begin
            y3 := y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.x2 - x1,
              PInteger(y2)^ - y1, PInteger(x2)^ - x1))^;
            y4 := y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.x1 - x1,
              PInteger(y2)^ - y1, PInteger(x2)^ - x1))^;

            F3 := ClippingFlagsYInteger(y3, FClipBox);
            F4 := ClippingFlagsYInteger(y4, FClipBox);

            LineClipY(Ras, FClipBox.x2, y1, FClipBox.x2, y3, f1, F3);
            LineClipY(Ras, FClipBox.x2, y3, FClipBox.x1, y4, F3, F4);
            LineClipY(Ras, FClipBox.x1, y4, FClipBox.x1,
              PInteger(y2)^, F4, f2);
          end;

        // x1 < clip.x1
        8:
          begin
            y3 := y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.x1 - x1,
              PInteger(y2)^ - y1, PInteger(x2)^ - x1))^;
            F3 := ClippingFlagsYInteger(y3, FClipBox);

            LineClipY(Ras, FClipBox.x1, y1, FClipBox.x1, y3, f1, F3);
            LineClipY(Ras, FClipBox.x1, y3, PInteger(x2)^,
              PInteger(y2)^, F3, f2);
          end;

        // x1 < clip.x1 && x2 > clip.x2
        9:
          begin
            y3 := y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.x1 - x1,
              PInteger(y2)^ - y1, PInteger(x2)^ - x1))^;
            y4 := y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.x2 - x1,
              PInteger(y2)^ - y1, PInteger(x2)^ - x1))^;
            F3 := ClippingFlagsYInteger(y3, FClipBox);
            F4 := ClippingFlagsYInteger(y4, FClipBox);

            LineClipY(Ras, FClipBox.x1, y1, FClipBox.x1, y3, f1, F3);
            LineClipY(Ras, FClipBox.x1, y3, FClipBox.x2, y4, F3, F4);
            LineClipY(Ras, FClipBox.x2, y4, FClipBox.x2,
              PInteger(y2)^, F4, f2);
          end;

        // x1 < clip.x1 && x2 < clip.x1
        12:
          LineClipY(Ras, FClipBox.x1, y1, FClipBox.x1,
            PInteger(y2)^, f1, f2);
      end;

      FF1 := f2;

    end
  else
      Ras.Line(RasterizerConverter.XI(@FX1), RasterizerConverter.Yi(@FY1), RasterizerConverter.XI(x2), RasterizerConverter.Yi(y2));

  FX1 := PInteger(x2)^;
  FY1 := PInteger(y2)^;
end;

function TAggRasterizerScanLineClipInteger.GetConverterType: TAggRasterizerConv;
begin
  Result := RasterizerConverter;
end;

procedure TAggRasterizerScanLineClipInteger.LineClipY(Ras: TAggRasterizerCellsAA;
  x1, y1, x2, y2: Integer; f1, f2: Cardinal);
var
  Tx1, Ty1, Tx2, Ty2: Integer;
begin
  f1 := f1 and 10;
  f2 := f2 and 10;

  if f1 or f2 = 0 then
    // Fully visible
      Ras.Line(RasterizerConverter.XI(@x1), RasterizerConverter.Yi(@y1), RasterizerConverter.XI(@x2), RasterizerConverter.Yi(@y2))

  else
    begin
      // Invisible by Y
      if f1 = f2 then
          Exit;

      Tx1 := x1;
      Ty1 := y1;
      Tx2 := x2;
      Ty2 := y2;

      // y1 < clip.y1
      if f1 and 8 <> 0 then
        begin
          Tx1 := x1 + PInteger(RasterizerConverter.MulDiv(FClipBox.y1 - y1, x2 - x1,
            y2 - y1))^;
          Ty1 := FClipBox.y1;
        end;

      // y1 > clip.y2
      if f1 and 2 <> 0 then
        begin
          Tx1 := x1 + PInteger(RasterizerConverter.MulDiv(FClipBox.y2 - y1, x2 - x1,
            y2 - y1))^;
          Ty1 := FClipBox.y2;
        end;

      // y2 < clip.y1
      if f2 and 8 <> 0 then
        begin
          Tx2 := x1 + PInteger(RasterizerConverter.MulDiv(FClipBox.y1 - y1, x2 - x1,
            y2 - y1))^;
          Ty2 := FClipBox.y1;
        end;

      // y2 > clip.y2
      if f2 and 2 <> 0 then
        begin
          Tx2 := x1 + PInteger(RasterizerConverter.MulDiv(FClipBox.y2 - y1, x2 - x1,
            y2 - y1))^;
          Ty2 := FClipBox.y2;
        end;

      Ras.Line(RasterizerConverter.XI(@Tx1), RasterizerConverter.Yi(@Ty1), RasterizerConverter.XI(@Tx2),
        RasterizerConverter.Yi(@Ty2));
    end;
end;

{ TAggRasterizerScanLineClipDouble }

constructor TAggRasterizerScanLineClipDouble.Create(conv: TAggRasterizerConv);
begin
  RasterizerConverter := conv;

  FClipBox := RectDouble(0, 0, 0, 0);

  FX1 := 0;
  FY1 := 0;
  FF1 := 0;

  FClipping := False;
end;

procedure TAggRasterizerScanLineClipDouble.ResetClipping;
begin
  FClipping := False;
end;

procedure TAggRasterizerScanLineClipDouble.SetClipBox(Bounds: Pointer);
begin
  FClipBox := PRectDouble(Bounds)^;
  FClipBox.Normalize;

  FClipping := True;
end;

procedure TAggRasterizerScanLineClipDouble.SetClipBox(x1, y1, x2, y2: Pointer);
begin
  FClipBox := RectDouble(PDouble(x1)^, PDouble(y1)^, PDouble(x2)^,
    PDouble(y2)^);
  FClipBox.Normalize;

  FClipping := True;
end;

procedure TAggRasterizerScanLineClipDouble.MoveTo(x1, y1: Pointer);
begin
  FX1 := PDouble(x1)^;
  FY1 := PDouble(y1)^;

  if FClipping then
      FF1 := ClippingFlagsDouble(PDouble(x1)^, PDouble(y1)^, @FClipBox);
end;

procedure TAggRasterizerScanLineClipDouble.LineTo(Ras: TAggRasterizerCellsAA;
  x2, y2: Pointer);
var
  f1, f2, F3, F4: Cardinal;
  x1, y1, y3, y4: Double;

begin
  if FClipping then
    begin
      f2 := ClippingFlagsDouble(PDouble(x2)^, PDouble(y2)^, @FClipBox);

      // Invisible by Y
      if ((FF1 and 10) = (f2 and 10)) and (FF1 and 10 <> 0) then
        begin
          FX1 := PDouble(x2)^;
          FY1 := PDouble(y2)^;
          FF1 := f2;

          Exit;
        end;

      x1 := FX1;
      y1 := FY1;
      f1 := FF1;

      case ((f1 and 5) shl 1) or (f2 and 5) of
        // Visible by X
        0:
          LineClipY(Ras, x1, y1, PDouble(x2)^, PDouble(y2)^, f1, f2);

        // x2 > clip.x2
        1:
          begin
            y3 := y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.x2 - x1,
              PDouble(y2)^ - y1, PDouble(x2)^ - x1))^;
            F3 := ClippingFlagsYDouble(y3, @FClipBox);

            LineClipY(Ras, x1, y1, FClipBox.x2, y3, f1, F3);
            LineClipY(Ras, FClipBox.x2, y3, FClipBox.x2,
              PDouble(y2)^, F3, f2);
          end;

        // x1 > clip.x2
        2:
          begin
            y3 := y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.x2 - x1,
              PDouble(y2)^ - y1, PDouble(x2)^ - x1))^;
            F3 := ClippingFlagsYDouble(y3, @FClipBox);

            LineClipY(Ras, FClipBox.x2, y1, FClipBox.x2, y3, f1, F3);
            LineClipY(Ras, FClipBox.x2, y3, PDouble(x2)^,
              PDouble(y2)^, F3, f2);
          end;

        // x1 > clip.x2 && x2 > clip.x2
        3:
          LineClipY(Ras, FClipBox.x2, y1, FClipBox.x2,
            PDouble(y2)^, f1, f2);

        // x2 < clip.x1
        4:
          begin
            y3 := y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.x1 - x1,
              PDouble(y2)^ - y1, PDouble(x2)^ - x1))^;
            F3 := ClippingFlagsYDouble(y3, @FClipBox);

            LineClipY(Ras, x1, y1, FClipBox.x1, y3, f1, F3);
            LineClipY(Ras, FClipBox.x1, y3, FClipBox.x1,
              PDouble(y2)^, F3, f2);
          end;

        // x1 > clip.x2 && x2 < clip.x1
        6:
          begin
            y3 := y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.x2 - x1,
              PDouble(y2)^ - y1, PDouble(x2)^ - x1))^;
            y4 := y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.x1 - x1,
              PDouble(y2)^ - y1, PDouble(x2)^ - x1))^;

            F3 := ClippingFlagsYDouble(y3, @FClipBox);
            F4 := ClippingFlagsYDouble(y4, @FClipBox);

            LineClipY(Ras, FClipBox.x2, y1, FClipBox.x2, y3, f1, F3);
            LineClipY(Ras, FClipBox.x2, y3, FClipBox.x1, y4, F3, F4);
            LineClipY(Ras, FClipBox.x1, y4, FClipBox.x1,
              PDouble(y2)^, F4, f2);
          end;

        // x1 < clip.x1
        8:
          begin
            y3 := y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.x1 - x1,
              PDouble(y2)^ - y1, PDouble(x2)^ - x1))^;
            F3 := ClippingFlagsYDouble(y3, @FClipBox);

            LineClipY(Ras, FClipBox.x1, y1, FClipBox.x1, y3, f1, F3);
            LineClipY(Ras, FClipBox.x1, y3, PDouble(x2)^,
              PDouble(y2)^, F3, f2);
          end;

        // x1 < clip.x1 && x2 > clip.x2
        9:
          begin
            y3 := y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.x1 - x1,
              PDouble(y2)^ - y1, PDouble(x2)^ - x1))^;
            y4 := y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.x2 - x1,
              PDouble(y2)^ - y1, PDouble(x2)^ - x1))^;
            F3 := ClippingFlagsYDouble(y3, @FClipBox);
            F4 := ClippingFlagsYDouble(y4, @FClipBox);

            LineClipY(Ras, FClipBox.x1, y1, FClipBox.x1, y3, f1, F3);
            LineClipY(Ras, FClipBox.x1, y3, FClipBox.x2, y4, F3, F4);
            LineClipY(Ras, FClipBox.x2, y4, FClipBox.x2,
              PDouble(y2)^, F4, f2);
          end;

        // x1 < clip.x1 && x2 < clip.x1
        12:
          LineClipY(Ras, FClipBox.x1, y1, FClipBox.x1,
            PDouble(y2)^, f1, f2);
      end;

      FF1 := f2;

    end
  else
      Ras.Line(RasterizerConverter.XI(@FX1), RasterizerConverter.Yi(@FY1), RasterizerConverter.XI(x2), RasterizerConverter.Yi(y2));

  FX1 := PDouble(x2)^;
  FY1 := PDouble(y2)^;
end;

function TAggRasterizerScanLineClipDouble.GetConverterType: TAggRasterizerConv;
begin
  Result := RasterizerConverter;
end;

procedure TAggRasterizerScanLineClipDouble.LineClipY(Ras: TAggRasterizerCellsAA;
  x1, y1, x2, y2: Double; f1, f2: Cardinal);
var
  Tx1, Ty1, Tx2, Ty2: Double;

begin
  f1 := f1 and 10;
  f2 := f2 and 10;

  if f1 or f2 = 0 then
    // Fully visible
      Ras.Line(RasterizerConverter.XI(@x1), RasterizerConverter.Yi(@y1), RasterizerConverter.XI(@x2), RasterizerConverter.Yi(@y2))

  else
    begin
      // Invisible by Y
      if f1 = f2 then
          Exit;

      Tx1 := x1;
      Ty1 := y1;
      Tx2 := x2;
      Ty2 := y2;

      // y1 < clip.y1
      if f1 and 8 <> 0 then
        begin
          Tx1 := x1 + PDouble(RasterizerConverter.MulDiv(FClipBox.y1 - y1, x2 - x1,
            y2 - y1))^;
          Ty1 := FClipBox.y1;
        end;

      // y1 > clip.y2
      if f1 and 2 <> 0 then
        begin
          Tx1 := x1 + PDouble(RasterizerConverter.MulDiv(FClipBox.y2 - y1, x2 - x1,
            y2 - y1))^;
          Ty1 := FClipBox.y2;
        end;

      // y2 < clip.y1
      if f2 and 8 <> 0 then
        begin
          Tx2 := x1 + PDouble(RasterizerConverter.MulDiv(FClipBox.y1 - y1, x2 - x1,
            y2 - y1))^;
          Ty2 := FClipBox.y1;
        end;

      // y2 > clip.y2
      if f2 and 2 <> 0 then
        begin
          Tx2 := x1 + PDouble(RasterizerConverter.MulDiv(FClipBox.y2 - y1, x2 - x1,
            y2 - y1))^;
          Ty2 := FClipBox.y2;
        end;

      Ras.Line(RasterizerConverter.XI(@Tx1), RasterizerConverter.Yi(@Ty1), RasterizerConverter.XI(@Tx2),
        RasterizerConverter.Yi(@Ty2));
    end;
end;

{ TAggRasterizerScanLineNoClip }

constructor TAggRasterizerScanLineNoClip.Create;
begin
  FX1 := 0;
  FY1 := 0;

  FConv := TAggRasConvInt.Create;
end;

procedure TAggRasterizerScanLineNoClip.ResetClipping;
begin
end;

procedure TAggRasterizerScanLineNoClip.SetClipBox(x1, y1, x2, y2: Pointer);
begin
end;

procedure TAggRasterizerScanLineNoClip.SetClipBox(Bounds: Pointer);
begin
end;

procedure TAggRasterizerScanLineNoClip.MoveTo(x1, y1: Pointer);
begin
  FX1 := PInteger(x1)^;
  FY1 := PInteger(y1)^;
end;

procedure TAggRasterizerScanLineNoClip.LineTo(Ras: TAggRasterizerCellsAA;
  x2, y2: Pointer);
begin
  Ras.Line(FX1, FY1, PInteger(x2)^, PInteger(y2)^);

  FX1 := PInteger(x2)^;
  FY1 := PInteger(y2)^;
end;

function TAggRasterizerScanLineNoClip.GetConverterType: TAggRasterizerConv;
begin
  Result := FConv;
end;

{ TAggRasterizerScanLineClipInt }

constructor TAggRasterizerScanLineClipInt.Create;
begin
  FConv := TAggRasConvInt.Create;

  inherited Create(FConv);
end;

destructor TAggRasterizerScanLineClipInt.Destroy;
begin
  FConv.Free;
  inherited;
end;

{ TAggRasterizerScanLineClipIntegerSat }

constructor TAggRasterizerScanLineClipIntegerSat.Create;
begin
  FConv := TAggRasConvIntSat.Create;

  inherited Create(FConv);
end;

destructor TAggRasterizerScanLineClipIntegerSat.Destroy;
begin
  FConv.Free;
  inherited;
end;

{ TAggRasterizerScanLineClipInteger3x }

constructor TAggRasterizerScanLineClipInteger3x.Create;
begin
  FConv := TAggRasConvInt3x.Create;

  inherited Create(FConv);
end;

destructor TAggRasterizerScanLineClipInteger3x.Destroy;
begin
  FConv.Free;
  inherited;
end;

{ TAggRasterizerScanLineDoubleClip }

constructor TAggRasterizerScanLineDoubleClip.Create;
begin
  FConv := TRasConvDouble.Create;

  inherited Create(FConv);
end;

destructor TAggRasterizerScanLineDoubleClip.Destroy;
begin
  FConv.Free;
  inherited;
end;

{ TAggRasterizerScanLineClipDouble3x }

constructor TAggRasterizerScanLineClipDouble3x.Create;
begin
  FConv := TRasConvDouble3x.Create;

  inherited Create(FConv);
end;

destructor TAggRasterizerScanLineClipDouble3x.Destroy;
begin
  FConv.Free;
  inherited;
end;

end. 
 
 
