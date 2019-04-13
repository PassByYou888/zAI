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
unit AggArc;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface

uses
  Math,
  AggMath,
  AggBasics,
  AggVertexSource;

type
  TAggArc = class(TAggCustomVertexSource)
  private
    FRadius: TPointDouble;
    fx, fy, FAngle, FStart, FEnd, FScale, FDeltaAngle: Double;
    FCounterClockWise, FInitialized: Boolean;
    FPathCmd: Cardinal;

    procedure SetApproximationScale(Value: Double);
  protected
    procedure Normalize(a1, a2: Double; Ccw: Boolean);
  public
    constructor Create; overload;
    constructor Create(x, y, RX, RY, a1, a2: Double;
      Ccw: Boolean = True); overload;

    procedure Init(x, y, RX, RY, a1, a2: Double; Ccw: Boolean = True);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;

    property ApproximationScale: Double read FScale write SetApproximationScale;
  end;

implementation


{ TAggArc }

constructor TAggArc.Create;
begin
  fx := 0;
  fy := 0;
  FRadius.x := 0;
  FRadius.y := 0;
  FAngle := 0;
  FStart := 0;
  FEnd := 0;
  FDeltaAngle := 0;

  FCounterClockWise := False;
  FPathCmd := 0;

  FScale := 1;

  FInitialized := False;
end;

constructor TAggArc.Create(x, y, RX, RY, a1, a2: Double; Ccw: Boolean = True);
begin
  Create;

  fx := x;
  fy := y;
  FRadius.x := RX;
  FRadius.y := RY;

  FScale := 1;

  Normalize(a1, a2, Ccw);
end;

procedure TAggArc.Init(x, y, RX, RY, a1, a2: Double; Ccw: Boolean = True);
begin
  fx := x;
  fy := y;
  FRadius.x := RX;
  FRadius.y := RY;

  Normalize(a1, a2, Ccw);
end;

procedure TAggArc.SetApproximationScale(Value: Double);
begin
  FScale := Value;

  if FInitialized then
    Normalize(FStart, FEnd, FCounterClockWise);
end;

procedure TAggArc.Rewind(PathID: Cardinal);
begin
  FPathCmd := CAggPathCmdMoveTo;
  FAngle := FStart;
end;

function TAggArc.Vertex(x, y: PDouble): Cardinal;
var
  pf: Cardinal;
  Pnt : TPointDouble;
begin
  if IsStop(FPathCmd) then
    Result := CAggPathCmdStop

  else if (FAngle < FEnd - FDeltaAngle / 4) <> FCounterClockWise then
  begin
    SinCos(FEnd, Pnt.y, Pnt.x);
    x^ := fx + Pnt.x * FRadius.x;
    y^ := fy + Pnt.y * FRadius.y;

    FPathCmd := CAggPathCmdStop;

    Result := CAggPathCmdLineTo;
  end
  else
  begin
    SinCos(FAngle, Pnt.y, Pnt.x);
    x^ := fx + Pnt.x * FRadius.x;
    y^ := fy + Pnt.y * FRadius.y;

    FAngle := FAngle + FDeltaAngle;

    pf := FPathCmd;
    FPathCmd := CAggPathCmdLineTo;

    Result := pf;
  end;
end;

procedure TAggArc.Normalize(a1, a2: Double; Ccw: Boolean);
var
  RA: Double;
begin
  RA := (Abs(FRadius.x) + Abs(FRadius.y)) * 0.5;
  FDeltaAngle := ArcCos(RA / (RA + 0.125 / FScale)) * 2;

  if Ccw then
    while a2 < a1 do
      a2 := a2 + (pi * 2.0)
  else
  begin
    while a1 < a2 do
      a1 := a1 + (pi * 2.0);

    FDeltaAngle := -FDeltaAngle;
  end;

  FCounterClockWise := Ccw;
  FStart := a1;
  FEnd := a2;

  FInitialized := True;
end;

end. 
 
 
