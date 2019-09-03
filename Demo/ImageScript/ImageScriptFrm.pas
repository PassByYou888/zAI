unit ImageScriptFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  System.IOUtils,

  CoreClasses, PascalStrings, UnicodeMixedLib, DoStatusIO,
  Geometry2DUnit, zDrawEngine, MemoryRaster, zDrawEngineInterface_SlowFMX, zAI_Common, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Layouts,
  FMX.ScrollBox, FMX.Memo;

type
  TImageScriptForm = class(TForm)
    Timer1: TTimer;
    Layout2: TLayout;
    Label2: TLabel;
    ProcessEdit: TEdit;
    Layout1: TLayout;
    Label1: TLabel;
    conditionEdit: TEdit;
    RunButton: TButton;
    Memo1: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure RunButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  public
    { Public declarations }
    drawIntf: TDrawEngineInterface_FMX;
    background: TMemoryRaster;
    sourL: TAI_ImageList;
  end;

var
  ImageScriptForm: TImageScriptForm;

implementation

{$R *.fmx}


procedure TImageScriptForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TImageScriptForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  EnginePool.Clear;
  DisposeObject([drawIntf, background]);
  DisposeObject(sourL);
end;

procedure TImageScriptForm.FormCreate(Sender: TObject);
begin
  drawIntf := TDrawEngineInterface_FMX.Create;
  background := NewRaster();
  background.SetSize(256, 256);
  FillBlackGrayBackgroundTexture(background, 32);

  sourL := TAI_ImageList.Create;
  sourL.LoadFromFile(WhereFileFromConfigure('lady_face.ImgDataSet'));

  AddDoStatusHook(Self, DoStatusMethod);
end;

procedure TImageScriptForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  d: TDrawEngine;
  fi, fj: TGeoFloat;
begin
  drawIntf.SetSurface(Canvas, Sender);
  d := DrawPool(Sender, drawIntf);
  d.ViewOptions := [voFPS, voEdge];

  // draw background
  fi := 0;
  while fi < d.width do
    begin
      fj := 0;
      while fj < d.height do
        begin
          d.DrawPicture(background, background.BoundsRectV2,
            RectAdd(background.BoundsRectV2, Vec2(fi, fj)), 0, 1.0);
          fj := fj + background.height - 1;
        end;
      fi := fi + background.width - 1;
    end;

  sourL.DrawToPictureList(d, 10, Vec2(0, 20), 1.0);

  d.Flush;
end;

procedure TImageScriptForm.RunButtonClick(Sender: TObject);
begin
  sourL.RunScript(conditionEdit.Text, ProcessEdit.Text);
end;

procedure TImageScriptForm.Timer1Timer(Sender: TObject);
begin
  EnginePool.Progress(Interval2Delta(Timer1.Interval));
  Invalidate;
end;

end.
