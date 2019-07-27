unit ShowImageFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.ExtCtrls,
  zDrawEngineInterface_SlowFMX, MemoryRaster, FMX.Objects;

type
  TShowImageForm = class(TForm)
    ImageViewer: TImage;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure ShowImage(img: TMemoryRaster); overload;
procedure ShowImage(img: TMemoryRaster; title: string); overload;

implementation

{$R *.fmx}


procedure ShowImage(img: TMemoryRaster);
var
  ShowImageForm: TShowImageForm;
begin
  ShowImageForm := TShowImageForm.Create(Application);
  ShowImageForm.Show;
  MemoryBitmapToBitmap(img, ShowImageForm.ImageViewer.Bitmap);
end;

procedure ShowImage(img: TMemoryRaster; title: string);
var
  ShowImageForm: TShowImageForm;
begin
  ShowImageForm := TShowImageForm.Create(Application);
  ShowImageForm.Show;
  MemoryBitmapToBitmap(img, ShowImageForm.ImageViewer.Bitmap);
  ShowImageForm.Caption := title;
end;

constructor TShowImageForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TShowImageForm.Destroy;
begin
  inherited Destroy;
end;

procedure TShowImageForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TShowImageForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar:
  Char; Shift: TShiftState);
begin
  if Key = VKESCAPE then
      close;
end;

end.
