program FillColor;

uses
  System.StartUpCopy,
  FMX.Forms,
  FillColorFrm in 'FillColorFrm.pas' {FillColorForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFillColorForm, FillColorForm);
  Application.Run;
end.
