{ ****************************************************************************** }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
unit zDrawControlBoxForFMX;

{$INCLUDE ..\zDefine.inc}

interface

uses System.Types, FMX.Controls, zDrawEngine, Geometry2DUnit, Geometry3DUnit;

procedure DrawChildrenControl(WorkCtrl: TControl; DrawEng: TDrawEngine; ctrl: TControl; COLOR: TDEColor; LineWidth:TDEFloat);

implementation

procedure DrawChildrenControl(WorkCtrl: TControl; DrawEng: TDrawEngine; ctrl: TControl; COLOR: TDEColor; LineWidth:TDEFloat);
  procedure DrawControlRect(c: TControl);
  var
    r4: TRectf;
    r: TDERect;
  begin
    r4 := c.AbsoluteRect;
    r := MakeRectV2(Make2DPoint(WorkCtrl.AbsoluteToLocal(r4.TopLeft)), Make2DPoint(WorkCtrl.AbsoluteToLocal(r4.BottomRight)));
    DrawEng.DrawBoxInScene(r, COLOR, LineWidth);
  end;

var
  i: Integer;
begin
  for i := 0 to ctrl.ChildrenCount - 1 do
    begin
      if (ctrl.Children[i] is TControl) and (TControl(ctrl.Children[i]).Visible) then
        begin
          DrawChildrenControl(WorkCtrl, DrawEng, TControl(ctrl.Children[i]), COLOR, LineWidth);
          DrawControlRect(TControl(ctrl.Children[i]));
        end;
    end;
end;

end. 
 
 
