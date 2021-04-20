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
unit AggVertexSequence;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggArray;

type
  TAggFuncVertexSequence = function(This, val: Pointer): Boolean;

  // Modified TAggPodDeque. The data is interpreted as a sequence of vertices.
  TAggVertexSequence = class(TAggPodDeque)
  public
    FuncOperatorVertexSequence: TAggFuncVertexSequence;

    constructor Create(EntrySize: Cardinal; Shift: Cardinal = 6; Fovs: TAggFuncVertexSequence = nil);

    procedure Add(val: Pointer); override;
    procedure ModifyLast(val: Pointer); override;
    procedure Close(RemoveFlag: Boolean);
  end;

const
  // Coinciding points maximal distance (Epsilon)
  CAggVertexDistEpsilon: Double = 1E-14;

type
  // Vertex (x, y) with the distance to the next one. The last vertex has
  // distance between the last and the first points if the polygon is closed
  // and 0.0 if it's a polyline.
  PAggVertexDistance = ^TAggVertexDistance;

  TAggVertexDistance = record
    Pos: TPointDouble;
    Dist: Double;
  end;

  PAggVertexDistCmd = ^TAggVertexDistCmd;

  TAggVertexDistCmd = record
    Pos: TPointDouble;
    Dist: Double;
    Cmd: Cardinal;
  end;

function VertexDisTAggFuncOperator(This, val: PAggVertexDistance): Boolean;

implementation

uses
  AggMath;

function VertexDisTAggFuncOperator(This, val: PAggVertexDistance): Boolean;
var
  ret: Boolean;
begin
  This.Dist := CalculateDistance(This.Pos, val.Pos);

  ret := This.Dist > CAggVertexDistEpsilon;

  if not ret then
      This.Dist := 1 / CAggVertexDistEpsilon;

  Result := ret;
end;

{ TAggVertexSequence }

constructor TAggVertexSequence.Create(EntrySize: Cardinal; Shift: Cardinal = 6;
  Fovs: TAggFuncVertexSequence = nil);
begin
  inherited Create(EntrySize, Shift);

  if @Fovs = nil then
      FuncOperatorVertexSequence := @VertexDisTAggFuncOperator
  else
      FuncOperatorVertexSequence := Fovs;
end;

procedure TAggVertexSequence.Add(val: Pointer);
begin
  if Size > 1 then
    if not FuncOperatorVertexSequence(ArrayOperator(Size - 2),
      ArrayOperator(Size - 1)) then
        RemoveLast;

  inherited Add(val);
end;

procedure TAggVertexSequence.ModifyLast(val: Pointer);
begin
  RemoveLast;

  Add(val);
end;

procedure TAggVertexSequence.Close(RemoveFlag: Boolean);
var
  t: Pointer;
begin
  while Size > 1 do
    begin
      if FuncOperatorVertexSequence(ArrayOperator(Size - 2),
        ArrayOperator(Size - 1)) then
          Break;

      t := ArrayOperator(Size - 1);

      RemoveLast;
      ModifyLast(t);
    end;

  if RemoveFlag then
    while Size > 1 do
      begin
        if FuncOperatorVertexSequence(ArrayOperator(Size - 1),
          ArrayOperator(0)) then
            Break;

        RemoveLast;
      end;
end;

end. 
 
 
 
