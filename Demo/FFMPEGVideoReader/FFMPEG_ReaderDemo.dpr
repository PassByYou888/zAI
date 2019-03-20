program FFMPEG_ReaderDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  FFMPEGReaderDemoFrm in 'FFMPEGReaderDemoFrm.pas' {FFMPEGReaderDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFFMPEGReaderDemoForm, FFMPEGReaderDemoForm);
  Application.Run;
end.
