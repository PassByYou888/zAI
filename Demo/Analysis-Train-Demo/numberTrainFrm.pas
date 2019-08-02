unit numberTrainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,

  FMX.Surfaces,

  CoreClasses, DoStatusIO, MemoryRaster, PascalStrings, ObjectDataManager, ItemStream,
  Geometry2DUnit, UnicodeMixedLib, Learn, LearnTypes;

type
  TnumberTrainForm = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    lr: TLearn;
    procedure UpdateOutput;
    procedure DoStatusM(AText: SystemString; const ID: Integer);
  end;

var
  numberTrainForm: TnumberTrainForm;

implementation

{$R *.fmx}

{ TForm1 }

procedure TnumberTrainForm.DoStatusM(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TnumberTrainForm.FormCreate(Sender: TObject);
var
  i     : Integer;
  d1, d2: Double;
begin
  AddDoStatusHook(Self, DoStatusM);

  lr := TLearn.CreateRegression(TLearnType.ltLBFGS_MT, 2, 1);
  for i := 0 to 20000 - 1 do
    begin
      d1 := umlRandomRange(-5000, 5000);
      d2 := umlRandomRange(-5000, 5000);
      lr.AddMemory([d1, d2], [d1 - d2], IntToStr(i));
    end;
  lr.TrainP(1000, procedure(const LSender: TLearn; const state: Boolean)
    begin
      if state then
        begin
          DoStatus('ÑµÁ·Íê³É');
          UpdateOutput;
        end;
    end);
end;

procedure TnumberTrainForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(Self);
  DisposeObject(lr);
end;

procedure TnumberTrainForm.UpdateOutput;
var
  i     : Integer;
  d1, d2: Double;
  v:TLFloat;
begin
  for i := 1 to 1000 do
    begin
      d1 := umlRandomRange(1, 100);
      d2 := umlRandomRange(1, 100);
      v:=lr.processFV([d1, d2]);
      DoStatus('%f - %f=%d (%s)', [d1, d2, Round(v), lr.SearchToken([v])]);
    end;
end;

end.
