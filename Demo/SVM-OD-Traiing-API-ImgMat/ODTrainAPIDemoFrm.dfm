object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Training .ImgMat'
  ClientHeight = 577
  ClientWidth = 1059
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 24
    Top = 16
    Width = 1009
    Height = 425
    Lines.Strings = (
      'OD'#21644'ODM'#30340#25216#26415#25351#26631
      #25805#20316#31995#32479#65306'ios,android,windows,linux'
      #22788#29702#22120#26550#26500#65306'intel x86,x64,arm32,arm64'
      'IOT'#65306#25903#25345
      #20869#23384#38656#27714#65306#20302
      'GPU'#38656#27714#65306#26080
      #24182#34892#65306#25903#25345
      #25968#25454#20860#23481#65306#20840#20860#23481
      #23454#26102#24615#65306#20013#65292#19981#36866#21512#39640#24103#29575#35270#39057#65292'OD'#21644'ODM'#36890#36807#38477#20302#20998#36776#29575#21487#20197#20248#21270#21040#27599#31186'10'#24103
      ''
      #35813'Demo'#28436#31034#20102#65292#19981#20351#29992'ZModelBuilder'#24037#20855#65292#25163#21160#35757#32451'.svm_od'#21644'.svm_od_marshal'#25968#25454
      ''
      #31532#19968#27493#65292#25105#20204#20351#29992'ImgMatrix_Tool'#23558'dog+bear'#25968#25454#38598#25910#38598#22312#19968#36215#65292#24182#19988#20445#23384#25104'.imgMat'#26684#24335
      #31532#20108#27493#65292#24320#22987#35757#32451'.imgMat'#20026'.svm_od_marshal'
      #31532#19977#27493#65292#30417#25511#36807#31243#65288'5'#20998#38047#23601#20250#23436#25104#65289#65292#24471#21040#32467#26524#65292#20351#29992#35757#32451#32467#26524
      '')
    TabOrder = 0
  end
  object FileEdit: TLabeledEdit
    Left = 24
    Top = 480
    Width = 193
    Height = 21
    EditLabel.Width = 60
    EditLabel.Height = 13
    EditLabel.Caption = #26679#26412#25968#25454#38598
    TabOrder = 1
    Text = 'dog_bear.imgMat'
  end
  object trainingButton: TButton
    Left = 24
    Top = 507
    Width = 121
    Height = 25
    Caption = #35757#32451
    TabOrder = 2
    OnClick = trainingButtonClick
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.svm_od_marshal'
    Filter = '.svm_od_marshal|*.svm_od_marshal'
    Left = 464
    Top = 432
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 464
    Top = 368
  end
end
