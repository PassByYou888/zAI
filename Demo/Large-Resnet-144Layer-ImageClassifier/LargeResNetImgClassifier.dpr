program LargeResNetImgClassifier;

uses
  System.StartUpCopy,
  FMX.Forms,
  LargeResNetImgClassifierFrm in 'LargeResNetImgClassifierFrm.pas' {LargeResNetImgClassifierForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLargeResNetImgClassifierForm, LargeResNetImgClassifierForm);
  Application.Run;
end.
