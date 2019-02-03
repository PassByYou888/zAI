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
unit AggVcgenBSpline;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggBSpline,
  AggArray,
  AggVertexSource;

type
  TAggInternalStatus = (siInitial, siReady, siPolygon, siEndPoly, siStop);

  TAggVcgenBSpline = class(TAggVertexSource)
  private
    FSourceVertices: TAggPodDeque;
    FSplineX, FSplineY: TAggBSpline;
    FInterpolationStep: Double;
    FClosed: Cardinal;
    FStatus: TAggInternalStatus;
    FSourceVertex: Cardinal;
    FCurrentAbscissa, FMaxAbscissa: Double;
    procedure SetInterpolationStep(Value: Double);
  public
    constructor Create;
    destructor Destroy; override;

    // Vertex Generator Interface
    procedure RemoveAll; override;
    procedure AddVertex(x, y: Double; Cmd: Cardinal); override;

    // Vertex Source Interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;

    property InterpolationStep: Double read FInterpolationStep write SetInterpolationStep;
  end;

implementation


{ TAggVcgenBSpline }

constructor TAggVcgenBSpline.Create;
begin
  inherited Create;

  FSourceVertices := TAggPodDeque.Create(SizeOf(TPointDouble), 6);
  FSplineX := TAggBSpline.Create;
  FSplineY := TAggBSpline.Create;

  FInterpolationStep := 1.0 / 50.0;

  FClosed := 0;
  FStatus := siInitial;

  FSourceVertex := 0;
end;

destructor TAggVcgenBSpline.Destroy;
begin
  FSourceVertices.Free;
  FSplineX.Free;
  FSplineY.Free;

  inherited;
end;

procedure TAggVcgenBSpline.SetInterpolationStep(Value: Double);
begin
  FInterpolationStep := Value;
end;

procedure TAggVcgenBSpline.RemoveAll;
begin
  FSourceVertices.RemoveAll;

  FClosed := 0;
  FStatus := siInitial;

  FSourceVertex := 0;
end;

procedure TAggVcgenBSpline.AddVertex(x, y: Double; Cmd: Cardinal);
var
  pt: TPointDouble;
begin
  FStatus := siInitial;

  if IsMoveTo(Cmd) then
    begin
      pt.x := x;
      pt.y := y;

      FSourceVertices.ModifyLast(@pt);
    end
  else if IsVertex(Cmd) then
    begin
      pt.x := x;
      pt.y := y;

      FSourceVertices.Add(@pt);
    end
  else
      FClosed := GetCloseFlag(Cmd);
end;

procedure TAggVcgenBSpline.Rewind(PathID: Cardinal);
var
  i: Cardinal;
  x: Double;
begin
  FCurrentAbscissa := 0.0;
  FMaxAbscissa := 0.0;

  FSourceVertex := 0;

  if (FStatus = siInitial) and (FSourceVertices.Size > 2) then
    begin
      if FClosed <> 0 then
        begin
          FSplineX.Init(FSourceVertices.Size + 8);
          FSplineY.Init(FSourceVertices.Size + 8);

          FSplineX.AddPoint(0.0,
            PPointDouble(FSourceVertices.Prev(FSourceVertices.Size - 3))^.x);
          FSplineY.AddPoint(0.0,
            PPointDouble(FSourceVertices.Prev(FSourceVertices.Size - 3))^.y);
          FSplineX.AddPoint(1.0,
            PPointDouble(FSourceVertices[FSourceVertices.Size - 3])^.x);
          FSplineY.AddPoint(1.0,
            PPointDouble(FSourceVertices[FSourceVertices.Size - 3])^.y);
          FSplineX.AddPoint(2.0,
            PPointDouble(FSourceVertices[FSourceVertices.Size - 2])^.x);
          FSplineY.AddPoint(2.0,
            PPointDouble(FSourceVertices[FSourceVertices.Size - 2])^.y);
          FSplineX.AddPoint(3.0,
            PPointDouble(FSourceVertices[FSourceVertices.Size - 1])^.x);
          FSplineY.AddPoint(3.0,
            PPointDouble(FSourceVertices[FSourceVertices.Size - 1])^.y);
        end
      else
        begin
          FSplineX.Init(FSourceVertices.Size);
          FSplineY.Init(FSourceVertices.Size);
        end;

      for i := 0 to FSourceVertices.Size - 1 do
        begin
          if FClosed <> 0 then
              x := i + 4
          else
              x := i;

          FSplineX.AddPoint(x,
            PPointDouble(FSourceVertices[i])^.x);
          FSplineY.AddPoint(x,
            PPointDouble(FSourceVertices[i])^.y);
        end;

      FCurrentAbscissa := 0.0;
      FMaxAbscissa := FSourceVertices.Size - 1;

      if FClosed <> 0 then
        begin
          FCurrentAbscissa := 4.0;
          FMaxAbscissa := FMaxAbscissa + 5.0;

          FSplineX.AddPoint(FSourceVertices.Size + 4,
            PPointDouble(FSourceVertices[0])^.x);

          FSplineY.AddPoint(FSourceVertices.Size + 4,
            PPointDouble(FSourceVertices[0])^.y);

          FSplineX.AddPoint(FSourceVertices.Size + 5,
            PPointDouble(FSourceVertices[1])^.x);

          FSplineY.AddPoint(FSourceVertices.Size + 5,
            PPointDouble(FSourceVertices[1])^.y);

          FSplineX.AddPoint(FSourceVertices.Size + 6,
            PPointDouble(FSourceVertices[2])^.x);

          FSplineY.AddPoint(FSourceVertices.Size + 6,
            PPointDouble(FSourceVertices[2])^.y);

          FSplineX.AddPoint(FSourceVertices.Size + 7,
            PPointDouble(FSourceVertices.Next(2))^.x);

          FSplineY.AddPoint(FSourceVertices.Size + 7,
            PPointDouble(FSourceVertices.Next(2))^.y);
        end;

      FSplineX.Prepare;
      FSplineY.Prepare;
    end;

  FStatus := siReady;
end;

function TAggVcgenBSpline.Vertex(x, y: PDouble): Cardinal;
var
  Cmd: Cardinal;

label
  _next, _ready, _polygon;

begin
  Cmd := CAggPathCmdLineTo;

_next:
  while not IsStop(Cmd) do
    case FStatus of
      siInitial:
        begin
          Rewind(0);

          goto _ready;
        end;

      siReady:
      _ready:
        begin
          if FSourceVertices.Size < 2 then
            begin
              Cmd := CAggPathCmdStop;

              goto _next;
            end;

          if FSourceVertices.Size = 2 then
            begin
              x^ := PPointDouble
                (FSourceVertices[FSourceVertex])^.x;
              y^ := PPointDouble
                (FSourceVertices[FSourceVertex])^.y;

              inc(FSourceVertex);

              if FSourceVertex = 1 then
                begin
                  Result := CAggPathCmdMoveTo;

                  Exit;
                end;

              if FSourceVertex = 2 then
                begin
                  Result := CAggPathCmdLineTo;

                  Exit;
                end;

              Cmd := CAggPathCmdStop;

              goto _next;
            end;

          Cmd := CAggPathCmdMoveTo;

          FStatus := siPolygon;

          FSourceVertex := 0;

          goto _polygon;
        end;

      siPolygon:
      _polygon:
        begin
          if FCurrentAbscissa >= FMaxAbscissa then
            if FClosed <> 0 then
              begin
                FStatus := siEndPoly;

                goto _next;
              end
            else
              begin
                x^ := PPointDouble
                  (FSourceVertices[FSourceVertices.Size - 1])^.x;
                y^ := PPointDouble
                  (FSourceVertices[FSourceVertices.Size - 1])^.y;

                FStatus := siEndPoly;
                Result := CAggPathCmdLineTo;

                Exit;
              end;

          x^ := FSplineX.GetStateful(FCurrentAbscissa);
          y^ := FSplineY.GetStateful(FCurrentAbscissa);

          inc(FSourceVertex);

          FCurrentAbscissa := FCurrentAbscissa + FInterpolationStep;

          if FSourceVertex = 1 then
              Result := CAggPathCmdMoveTo
          else
              Result := CAggPathCmdLineTo;
          Exit;
        end;

      siEndPoly:
        begin
          FStatus := siStop;
          Result := CAggPathCmdEndPoly or FClosed;
          Exit;
        end;

      siStop:
        begin
          Result := CAggPathCmdStop;
          Exit;
        end;
    end;

  Result := Cmd;
end;

end. 
 
 
