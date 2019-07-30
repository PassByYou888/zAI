program TextDraw;

uses
  System.StartUpCopy,
  FMX.Forms,
  TextDrawMainFrm in 'TextDrawMainFrm.pas' {TextDrawMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTextDrawMainForm, TextDrawMainForm);
  Application.Run;
end.
