program GoingDeeperWithConvolutionsNetImageClassifier;

uses
  System.StartUpCopy,
  FMX.Forms,
  GoingDeeperWithConvolutionsNetImageClassifierFrm in 'GoingDeeperWithConvolutionsNetImageClassifierFrm.pas' {GoingDeeperWithConvolutionsNetImageClassifierForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGoingDeeperWithConvolutionsNetImageClassifierForm, GoingDeeperWithConvolutionsNetImageClassifierForm);
  Application.Run;
end.
