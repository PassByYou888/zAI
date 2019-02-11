program VideoTrackerAndSoftDecode_GPU;

uses
  System.StartUpCopy,
  FMX.Forms,
  VideoTrackerFrm_GPU in 'VideoTrackerFrm_GPU.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
