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
unit AggVpGenSegmentator;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggVpGen,
  AggVertexSource;

type
  TAggVpgenSegmentator = class(TAggCustomVpgen)
  private
    FApproximationScale, FX1, FY1: Double;
    FDeltaL, FDeltaDeltaL: Double;
    FDelta: TPointDouble;
    FCmd: Cardinal;
    procedure SetApproximationScale(s: Double);
    function GetAutoClose: Boolean;
    function GetAutoUnclose: Boolean;
  public
    constructor Create; override;

    procedure Reset; override;
    procedure MoveTo(x, y: Double); override;
    procedure LineTo(x, y: Double); override;

    function Vertex(x, y: PDouble): Cardinal; override;

    property ApproximationScale: Double read FApproximationScale write SetApproximationScale;
    property AutoClose: Boolean read GetAutoClose;
    property AutoUnclose: Boolean read GetAutoUnclose;
  end;

implementation


{ TAggVpgenSegmentator }

constructor TAggVpgenSegmentator.Create;
begin
  FApproximationScale := 1.0;

  FX1 := 0;
  FY1 := 0;
  FDelta.x := 0;
  FDelta.y := 0;
  FDeltaL := 0;
  FDeltaDeltaL := 0;
  FCmd := 0;
end;

procedure TAggVpgenSegmentator.SetApproximationScale;
begin
  FApproximationScale := s;
end;

function TAggVpgenSegmentator.GetAutoClose;
begin
  Result := False
end;

function TAggVpgenSegmentator.GetAutoUnclose;
begin
  Result := False
end;

procedure TAggVpgenSegmentator.Reset;
begin
  FCmd := CAggPathCmdStop;
end;

procedure TAggVpgenSegmentator.MoveTo;
begin
  FX1 := x;
  FY1 := y;
  FDelta.x := 0.0;
  FDelta.y := 0.0;
  FDeltaL := 2.0;
  FDeltaDeltaL := 2.0;
  FCmd := CAggPathCmdMoveTo;
end;

procedure TAggVpgenSegmentator.LineTo;
var
  Len: Double;
begin
  FX1 := FX1 + FDelta.x;
  FY1 := FY1 + FDelta.y;
  FDelta.x := x - FX1;
  FDelta.y := y - FY1;

  Len := Sqrt(FDelta.x * FDelta.x + FDelta.y * FDelta.y) * FApproximationScale;

  if Len < 1E-30 then
      Len := 1E-30;

  FDeltaDeltaL := 1.0 / Len;

  if FCmd = CAggPathCmdMoveTo then
      FDeltaL := 0.0
  else
      FDeltaL := FDeltaDeltaL;

  if FCmd = CAggPathCmdStop then
      FCmd := CAggPathCmdLineTo;
end;

function TAggVpgenSegmentator.Vertex(x, y: PDouble): Cardinal;
var
  Cmd: Cardinal;
begin
  if FCmd = CAggPathCmdStop then
      Result := CAggPathCmdStop

  else
    begin
      Cmd := FCmd;
      FCmd := CAggPathCmdLineTo;

      if FDeltaL >= 1.0 - FDeltaDeltaL then
        begin
          FDeltaL := 1.0;
          FCmd := CAggPathCmdStop;

          x^ := FX1 + FDelta.x;
          y^ := FY1 + FDelta.y;

          Result := Cmd;

        end
      else
        begin
          x^ := FX1 + FDelta.x * FDeltaL;
          y^ := FY1 + FDelta.y * FDeltaL;

          FDeltaL := FDeltaL + FDeltaDeltaL;

          Result := Cmd;
        end;
    end;
end;

end. 
 
 
