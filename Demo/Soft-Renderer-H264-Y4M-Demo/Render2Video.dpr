program Render2Video;

uses
  System.StartUpCopy,
  FMX.Forms,
  Render2VideoFrm in 'Render2VideoFrm.pas' {Render2VideoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRender2VideoForm, Render2VideoForm);
  Application.Run;
end.
