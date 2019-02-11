program VideoTrackerAndSoftDecode_CPU;

uses
  System.StartUpCopy,
  FMX.Forms,
  VideoTrackerFrm_CPU in 'VideoTrackerFrm_CPU.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
