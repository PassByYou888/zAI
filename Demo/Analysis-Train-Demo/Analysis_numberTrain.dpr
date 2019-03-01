program Analysis_numberTrain;

uses
  System.StartUpCopy,
  FMX.Forms,
  numberTrainFrm in 'numberTrainFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
