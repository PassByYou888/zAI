program VideoTracker_Lady;

uses
  System.StartUpCopy,
  FMX.Forms,
  VideoTrackerFrm in 'VideoTrackerFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
