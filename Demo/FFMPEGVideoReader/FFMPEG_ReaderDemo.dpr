program FFMPEG_ReaderDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  FFMPEGReaderDemoFrm in 'FFMPEGReaderDemoFrm.pas' {FFMPEGReaderDemoForm},
  FFMPEG in 'C:\CoreLibrary\ffmpeg\FFMPEG.pas',
  FFMPEG_Reader in 'C:\CoreLibrary\ffmpeg\FFMPEG_Reader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFFMPEGReaderDemoForm, FFMPEGReaderDemoForm);
  Application.Run;
end.
