program VideoTracker_Camera;

uses
  System.StartUpCopy,
  FMX.Forms,
  VideoTrackerFrm in 'VideoTrackerFrm.pas' {VideoTrackerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TVideoTrackerForm, VideoTrackerForm);
  Application.Run;
end.
