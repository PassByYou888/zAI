unit ResNetImgClassifierFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.ScrollBox, FMX.Memo,

  System.IOUtils,

  CoreClasses, ListEngine,
  Learn, LearnTypes,
  zAI, zAI_Common, zAI_TrainingTask,
  zDrawEngineInterface_SlowFMX, zDrawEngine, Geometry2DUnit, MemoryRaster,
  MemoryStream64, PascalStrings, UnicodeMixedLib, DoStatusIO, FMX.Layouts, FMX.ExtCtrls;

type
  TResNetImgClassifierForm = class(TForm)
    Training_IMGClassifier_Button: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    ResetButton: TButton;
    ImgClassifierDetectorButton: TButton;
    OpenDialog1: TOpenDialog;
    procedure ImgClassifierDetectorButtonClick(Sender: TObject);
    procedure Training_IMGClassifier_ButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  public
    ai: TAI;
    imgMat: TAI_ImageMatrix;
  end;

var
  ResNetImgClassifierForm: TResNetImgClassifierForm;

implementation

{$R *.fmx}


procedure TResNetImgClassifierForm.ImgClassifierDetectorButtonClick(Sender: TObject);
begin
  OpenDialog1.Filter := TBitmapCodecManager.GetFilterString;
  if not OpenDialog1.Execute then
      exit;

  TComputeThread.RunP(nil, nil, procedure(Sender: TComputeThread)
    var
      sync_fn, output_fn, index_fn: U_String;
      mr: TMemoryRaster;
      rnic_hnd: TRNIC_Handle;
      rnic_index: TPascalStringList;
      rnic_vec: TLVec;
      i, index: Integer;
    begin
      output_fn := umlCombineFileName(TPath.GetLibraryPath, 'MiniImgClassifier' + C_RNIC_Ext);
      index_fn := umlCombineFileName(TPath.GetLibraryPath, 'MiniImgClassifier.index');

      if (not umlFileExists(output_fn)) or (not umlFileExists(index_fn)) then
        begin
          DoStatus('û��ͼƬ��������ѵ������.');
          exit;
        end;

      mr := NewRasterFromFile(OpenDialog1.FileName);
      rnic_hnd := ai.RNIC_Open_Stream(output_fn);
      rnic_index := TPascalStringList.Create;
      rnic_index.LoadFromFile(index_fn);

      rnic_vec := ai.RNIC_Process(rnic_hnd, mr, 64);

      for i := 0 to rnic_index.Count - 1 do
        begin
          index := LMaxVecIndex(rnic_vec);
          if index < rnic_index.Count then
              DoStatus('%d - %s - %f', [i, rnic_index[index].Text, rnic_vec[index]])
          else
              DoStatus('������RNIC�����ƥ��.��Ҫ����ѵ��');
          rnic_vec[index] := 0;
        end;

      ai.RNIC_Close(rnic_hnd);
      disposeObject(rnic_index);
      disposeObject(mr);
    end);
end;

procedure TResNetImgClassifierForm.Training_IMGClassifier_ButtonClick(Sender: TObject);
begin
  TComputeThread.RunP(nil, nil, procedure(Sender: TComputeThread)
    var
      param: PRNIC_Train_Parameter;
      sync_fn, output_fn, index_fn: U_String;
    begin
      TThread.Synchronize(Sender, procedure
        begin
          Training_IMGClassifier_Button.Enabled := False;
          ResetButton.Enabled := False;
        end);
      try
        sync_fn := umlCombineFileName(TPath.GetLibraryPath, 'MiniImgClassifier.imgMat.sync');
        output_fn := umlCombineFileName(TPath.GetLibraryPath, 'MiniImgClassifier' + C_RNIC_Ext);
        index_fn := umlCombineFileName(TPath.GetLibraryPath, 'MiniImgClassifier.index');

        if (not umlFileExists(output_fn)) or (not umlFileExists(index_fn)) then
          begin
            param := TAI.Init_RNIC_Train_Parameter(sync_fn, output_fn);

            // ����ѵ���ƻ�ʹ��8Сʱ
            param^.timeout := C_Tick_Hour * 8;

            // �����ݶȵĴ�������
            // �������ݶ��У�ֻҪʧЧ�������ȴﵽ���ڸ���ֵ���ݶȾͻῪʼ����
            param^.iterations_without_progress_threshold := 3000;

            // �����ֵ��������netʱʹ�õģ��������ͣ����ǿ��Ի���ͳ�ƵĲο��߶�
            // ��Ϊ��ͼƬ��������ѵ����iterations_without_progress_threshold��ܴ�
            // all_bn_running_stats_window_sizes���������ںܴ�ĵ��������У�����resnet��ÿ��step mini batch�Ļ���size
            // all_bn_running_stats_window_sizes�ǽ���ѵ��ʱ�����Ƶĳ���
            param^.all_bn_running_stats_window_sizes := 1000;

            // ��ο�od˼·
            // resnetÿ����stepʱ�Ĺ�դ��������
            // ����gpu���ڴ���������趨����
            param^.img_mini_batch := 4;

            // gpuÿ��һ�������������ͣ��ʱ�䵥λ��ms
            // �����������1.15�����ĺ����������������������ڹ�����ͬʱ����̨�����޸о�ѵ��
            zAI.KeepPerformanceOnTraining := 5;

            if ai.RNIC_Train(imgMat, param, index_fn) then
              begin
                DoStatus('ѵ���ɹ�.');
              end
            else
              begin
                DoStatus('ѵ��ʧ��.');
              end;

            TAI.Free_RNIC_Train_Parameter(param);
          end
        else
            DoStatus('ͼƬ�������Ѿ�ѵ������.');
      finally
          TThread.Synchronize(Sender, procedure
          begin
            Training_IMGClassifier_Button.Enabled := True;
            ResetButton.Enabled := True;
          end);
      end;
    end);
end;

procedure TResNetImgClassifierForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TResNetImgClassifierForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
  // ��ȡzAI������
  ReadAIConfig;
  // ��һ��������Key����������֤ZAI��Key
  // ���ӷ�������֤Key������������ʱһ���Ե���֤��ֻ�ᵱ��������ʱ�Ż���֤��������֤����ͨ����zAI����ܾ�����
  // �ڳ��������У���������TAI�����ᷢ��Զ����֤
  // ��֤��Ҫһ��userKey��ͨ��userkey�����ZAI������ʱ���ɵ����Key��userkey����ͨ��web���룬Ҳ������ϵ���߷���
  // ��֤key���ǿ����Ӽ����޷����ƽ�
  zAI.Prepare_AI_Engine();

  TComputeThread.RunP(nil, nil, procedure(Sender: TComputeThread)
    var
      i, j: Integer;
      imgL: TAI_ImageList;
      detDef: TAI_DetectorDefine;
      tokens: TArrayPascalString;
      n: TPascalString;
    begin
      TThread.Synchronize(Sender, procedure
        begin
          Training_IMGClassifier_Button.Enabled := False;
          ResetButton.Enabled := False;
        end);
      ai := TAI.OpenEngine();
      imgMat := TAI_ImageMatrix.Create;
      DoStatus('���ڶ�ȡ����ͼƬ�����.');
      imgMat.LoadFromFile(umlCombineFileName(TPath.GetLibraryPath, 'MiniImgClassifier.imgMat'));

      DoStatus('���������ǩ.');
      for i := 0 to imgMat.Count - 1 do
        begin
          imgL := imgMat[i];
          imgL.CalibrationNullDetectorDefineToken(imgL.FileInfo);
          for j := 0 to imgL.Count - 1 do
            if imgL[j].DetectorDefineList.Count = 0 then
              begin
                detDef := TAI_DetectorDefine.Create(imgL[j]);
                detDef.R := imgL[j].Raster.BoundsRect;
                detDef.Token := imgL.FileInfo;
                imgL[j].DetectorDefineList.Add(detDef);
              end;
        end;

      tokens := imgMat.tokens;
      DoStatus('�ܹ��� %d ������', [length(tokens)]);
      for n in tokens do
          DoStatus('"%s" �� %d ��ͼƬ', [n.Text, imgMat.GetTokenCount(n)]);

      TThread.Synchronize(Sender, procedure
        begin
          Training_IMGClassifier_Button.Enabled := True;
          ResetButton.Enabled := True;
        end);
    end);
end;

procedure TResNetImgClassifierForm.ResetButtonClick(Sender: TObject);
  procedure d(FileName: U_String);
  begin
    DoStatus('ɾ���ļ� %s', [FileName.Text]);
    umlDeleteFile(FileName);
  end;

begin
  d(umlCombineFileName(TPath.GetLibraryPath, 'MiniImgClassifier.imgMat.sync'));
  d(umlCombineFileName(TPath.GetLibraryPath, 'MiniImgClassifier.imgMat.sync_'));
  d(umlCombineFileName(TPath.GetLibraryPath, 'MiniImgClassifier' + C_RNIC_Ext));
  d(umlCombineFileName(TPath.GetLibraryPath, 'MiniImgClassifier.index'));
end;

procedure TResNetImgClassifierForm.Timer1Timer(Sender: TObject);
begin
  DoStatus;
end;

end.
