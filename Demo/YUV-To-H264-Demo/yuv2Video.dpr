program yuv2Video;

uses
  System.StartUpCopy,
  FMX.Forms,
  VideoFrm in 'VideoFrm.pas' {VideoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TVideoForm, VideoForm);
  Application.Run;
end.
