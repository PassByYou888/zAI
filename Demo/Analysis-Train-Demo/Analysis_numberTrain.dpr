program Analysis_numberTrain;

uses
  System.StartUpCopy,
  FMX.Forms,
  numberTrainFrm in 'numberTrainFrm.pas' {numberTrainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TnumberTrainForm, numberTrainForm);
  Application.Run;
end.
