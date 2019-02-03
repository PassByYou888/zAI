program VideoTrackerAndSoftDecode;

uses
  System.StartUpCopy,
  FMX.Forms,
  VideoTrackerFrm in 'VideoTrackerFrm.pas' {Form1},
  ShowImageFrm in 'ShowImageFrm.pas' {ShowImageForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
