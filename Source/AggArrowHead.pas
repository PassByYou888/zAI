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
unit AggArrowHead;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface

uses
  AggBasics,
  AggVertexSource;

type
  TAggArrowHead = class(TAggVertexSource)
  private
    FHead: array [0..3] of Double;
    FTail: array [0..3] of Double;

    FHeadFlag, FTailFlag: Boolean;

    FCoord: array [0..15] of Double;
    FCmd: array [0..7] of Cardinal;

    FCurrentID, FCurrentCoord: Cardinal;
  public
    constructor Create;

    procedure Head;
    procedure NoHead;

    procedure Tail;
    procedure NoTail;

    procedure SetHead(d1, d2, d3, d4: Double);
    procedure SetTail(d1, d2, d3, d4: Double);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;

    property HasHead: Boolean read FHeadFlag write FHeadFlag;
    property HasTail: Boolean read FTailFlag write FTailFlag;
  end;

implementation


{ TAggArrowHead }

constructor TAggArrowHead.Create;
begin
  FHead[0] := 1.0;
  FHead[1] := 1.0;
  FHead[2] := 1.0;
  FHead[3] := 0.0;
  FTail[0] := 1.0;
  FTail[1] := 1.0;
  FTail[2] := 1.0;
  FTail[3] := 0.0;

  FHeadFlag := False;
  FTailFlag := False;

  FCurrentID := 0;
  FCurrentCoord := 0;
end;

procedure TAggArrowHead.SetHead(d1, d2, d3, d4: Double);
begin
  FHead[0] := d1;
  FHead[1] := d2;
  FHead[2] := d3;
  FHead[3] := d4;

  FHeadFlag := True;
end;

procedure TAggArrowHead.Head;
begin
  FHeadFlag := True;
end;

procedure TAggArrowHead.NoHead;
begin
  FHeadFlag := False;
end;

procedure TAggArrowHead.SetTail(d1, d2, d3, d4: Double);
begin
  FTail[0] := d1;
  FTail[1] := d2;
  FTail[2] := d3;
  FTail[3] := d4;

  FTailFlag := True;
end;

procedure TAggArrowHead.Tail;
begin
  FTailFlag := True;
end;

procedure TAggArrowHead.NoTail;
begin
  FTailFlag := False;
end;

procedure TAggArrowHead.Rewind(PathID: Cardinal);
begin
  FCurrentID := PathID;
  FCurrentCoord := 0;

  case PathID of
    0:
      begin
        if not FTailFlag then
        begin
          FCmd[0] := CAggPathCmdStop;
          Exit;
        end;

        FCoord[0] := FTail[0];
        FCoord[1] := 0.0;
        FCoord[2] := FTail[0] - FTail[3];
        FCoord[3] := FTail[2];
        FCoord[4] := -FTail[1] - FTail[3];
        FCoord[5] := FTail[2];
        FCoord[6] := -FTail[1];
        FCoord[7] := 0.0;
        FCoord[8] := -FTail[1] - FTail[3];
        FCoord[9] := -FTail[2];
        FCoord[10] := FTail[0] - FTail[3];
        FCoord[11] := -FTail[2];

        FCmd[0] := CAggPathCmdMoveTo;
        FCmd[1] := CAggPathCmdLineTo;
        FCmd[2] := CAggPathCmdLineTo;
        FCmd[3] := CAggPathCmdLineTo;
        FCmd[4] := CAggPathCmdLineTo;
        FCmd[5] := CAggPathCmdLineTo;
        FCmd[7] := CAggPathCmdEndPoly or CAggPathFlagsClose or CAggPathFlagsCcw;
        FCmd[6] := CAggPathCmdStop;
      end;
    1:
      begin
        if not FHeadFlag then
        begin
          FCmd[0] := CAggPathCmdStop;

          Exit;
        end;

        FCoord[0] := -FHead[0];
        FCoord[1] := 0.0;
        FCoord[2] := FHead[1] + FHead[3];
        FCoord[3] := -FHead[2];
        FCoord[4] := FHead[1];
        FCoord[5] := 0.0;
        FCoord[6] := FHead[1] + FHead[3];
        FCoord[7] := FHead[2];

        FCmd[0] := CAggPathCmdMoveTo;
        FCmd[1] := CAggPathCmdLineTo;
        FCmd[2] := CAggPathCmdLineTo;
        FCmd[3] := CAggPathCmdLineTo;
        FCmd[4] := CAggPathCmdEndPoly or CAggPathFlagsClose or CAggPathFlagsCcw;
        FCmd[5] := CAggPathCmdStop;
      end;
  end;
end;

function TAggArrowHead.Vertex(x, y: PDouble): Cardinal;
var
  CurrentIndex: Cardinal;
begin
  if FCurrentID < 2 then
  begin
    CurrentIndex := FCurrentCoord * 2;

    x^ := FCoord[CurrentIndex];
    y^ := FCoord[CurrentIndex + 1];

    Result := FCmd[FCurrentCoord];

    inc(FCurrentCoord);
  end
  else
    Result := CAggPathCmdStop;
end;

end. 
 
 
