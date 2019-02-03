procedure TLearn.KDInput(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer);
var
  p: PLearnMemory;
  i: TLInt;
begin
  p := PLearnMemory(FMemorySource[IndexFor]);
  Source.index := IndexFor;
  for i := 0 to FInLen - 1 do
      Source.buff[i] := p^.m_in[i];
  Source.token := p^.token;
end;

procedure TLearn.TokenInput(const IndexFor: NativeInt; var Source: TKDTree_Source; const Data: Pointer);
var
  p: PLearnMemory;
  i: TLInt;
begin
  p := PLearnMemory(FMemorySource[IndexFor]);
  Source.index := IndexFor;
  for i := 0 to FOutLen - 1 do
      Source.buff[i] := p^.m_out[i];
  Source.token := p^.token;
end;

procedure TLearn.FreeLearnData;
begin
  if FLearnData <> nil then
    begin
      case FLearnType of
        ltKDT, ltKM:
          begin
            DisposeObject(PLearnKDT(FLearnData)^.K);
            Dispose(PLearnKDT(FLearnData));
            FLearnData := nil;
          end;
        ltForest:
          begin
            Dispose(PDecisionForest(FLearnData));
            FLearnData := nil;
          end;
        ltLogit:
          begin
            Dispose(PLogitModel(FLearnData));
            FLearnData := nil;
          end;
        ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo:
          begin
            MLPFree(PMultiLayerPerceptron(FLearnData)^);
            Dispose(PMultiLayerPerceptron(FLearnData));
            FLearnData := nil;
          end;
        ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT:
          begin
            Dispose(PMLPEnsemble(FLearnData));
            FLearnData := nil;
          end;
      end;
    end;
end;

procedure TLearn.CreateLearnData(const isTrainTime: Boolean);
var
  p_k: PLearnKDT;
  p_f: PDecisionForest;
  p_logit: PLogitModel;
  p_n: PMultiLayerPerceptron;
  p_e: PMLPEnsemble;
begin
  if not isTrainTime then
      FreeLearnData;

  case FLearnType of
    ltKDT, ltKM:
      begin
        if not isTrainTime then
          begin
            new(p_k);
            p_k^.K := TKDTree.Create(FInLen);
            FLearnData := p_k;
          end;
      end;
    ltForest:
      begin
        if not isTrainTime then
          begin
            new(p_f);
            FLearnData := p_f;
          end;
      end;
    ltLogit:
      begin
        if not isTrainTime then
          begin
            new(p_logit);
            FLearnData := p_logit;
          end;
      end;
    ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo:
      begin
        if not isTrainTime then
          begin
            new(p_n);
            FLearnData := p_n;
          end
        else
            p_n := PMultiLayerPerceptron(FLearnData);

        if FClassifier then
          begin
            if isTrainTime then
              begin
                case FHideLayerDepth of
                  hld0: MLPCreateC0(FInLen, Round(FLastTrainMaxOutValue) + 1, p_n^);
                  hld1: MLPCreateC1(FInLen, FInLen, Round(FLastTrainMaxOutValue) + 1, p_n^);
                  else MLPCreateC2(FInLen, FInLen, FInLen, Round(FLastTrainMaxOutValue) + 1, p_n^);
                end;
              end;
          end
        else
          begin
            case FHideLayerDepth of
              hld0: MLPCreate0(FInLen, FOutLen, p_n^);
              hld1: MLPCreate1(FInLen, FInLen, FOutLen, p_n^);
              else MLPCreate2(FInLen, FInLen, FInLen, FOutLen, p_n^);
            end;
          end;

      end;
    ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT:
      begin
        if not isTrainTime then
          begin
            new(p_e);
            FLearnData := p_e;
          end
        else
            p_e := PMLPEnsemble(FLearnData);

        if FClassifier then
          begin
            if isTrainTime then
              begin
                case FHideLayerDepth of
                  hld0: MLPECreateC0(FInLen, Round(FLastTrainMaxOutValue) + 1, 10, p_e^);
                  hld1: MLPECreateC1(FInLen, FInLen, Round(FLastTrainMaxOutValue) + 1, 10, p_e^);
                  else MLPECreateC2(FInLen, FInLen, FInLen, Round(FLastTrainMaxOutValue) + 1, 10, p_e^);
                end;
              end;
          end
        else
          begin
            case FHideLayerDepth of
              hld0: MLPECreate0(FInLen, FOutLen, 10, p_e^);
              hld1: MLPECreate1(FInLen, FInLen, FOutLen, 10, p_e^);
              else MLPECreate2(FInLen, FInLen, FInLen, FOutLen, 10, p_e^);
            end;
          end;
      end;
  end;
end;

class function TLearn.CreateRegression(const lt: TLearnType; const InDataLen, OutDataLen: TLInt): TLearn;
begin
  if InDataLen <= 0 then
      RaiseInfo('input need > 0');
  if OutDataLen <= 0 then
      RaiseInfo('output need > 0');

  Result := TLearn.Create;
  with Result do
    begin
      FEnabledRandomNumber := False;

      FInLen := InDataLen;
      FOutLen := OutDataLen;

      if (FOutLen <> 1) then
        begin
          if (lt = ltForest) then
              FOutLen := 1
          else if (lt = ltLogit) then
              FOutLen := 1;
        end;

      FMemorySource := TCoreClassList.Create;
      FKDToken := TKDTree.Create(FOutLen);
      FLearnType := lt;

      FLearnData := nil;
      FClassifier := False;
      FHideLayerDepth := hld0;
      CreateLearnData(False);

      FLastTrainMaxInValue := 0;
      FLastTrainMaxOutValue := 0;
      FInfo := '';
      FIsTraining := False;
      FTrainThreadRuning := False;

      FUserData := nil;
      FUserObject := nil;
    end;
end;

class function TLearn.CreateRegression1(const lt: TLearnType; const InDataLen, OutDataLen: TLInt): TLearn;
begin
  if InDataLen <= 0 then
      RaiseInfo('input need > 0');
  if OutDataLen <= 0 then
      RaiseInfo('output need > 0');

  Result := TLearn.Create;
  with Result do
    begin
      FEnabledRandomNumber := False;

      FInLen := InDataLen;
      FOutLen := OutDataLen;

      if (FOutLen <> 1) then
        begin
          if (lt = ltForest) then
              FOutLen := 1
          else if (lt = ltLogit) then
              FOutLen := 1;
        end;

      FMemorySource := TCoreClassList.Create;
      FKDToken := TKDTree.Create(FOutLen);
      FLearnType := lt;

      FLearnData := nil;
      FClassifier := False;
      FHideLayerDepth := hld1;
      CreateLearnData(False);

      FLastTrainMaxInValue := 0;
      FLastTrainMaxOutValue := 0;
      FInfo := '';
      FIsTraining := False;
      FTrainThreadRuning := False;

      FUserData := nil;
      FUserObject := nil;
    end;
end;

class function TLearn.CreateRegression2(const lt: TLearnType; const InDataLen, OutDataLen: TLInt): TLearn;
begin
  if InDataLen <= 0 then
      RaiseInfo('input need > 0');
  if OutDataLen <= 0 then
      RaiseInfo('output need > 0');

  Result := TLearn.Create;
  with Result do
    begin
      FEnabledRandomNumber := False;

      FInLen := InDataLen;
      FOutLen := OutDataLen;

      if (FOutLen <> 1) then
        begin
          if (lt = ltForest) then
              FOutLen := 1
          else if (lt = ltLogit) then
              FOutLen := 1;
        end;

      FMemorySource := TCoreClassList.Create;
      FKDToken := TKDTree.Create(FOutLen);
      FLearnType := lt;

      FLearnData := nil;
      FClassifier := False;
      FHideLayerDepth := hld2;
      CreateLearnData(False);

      FLastTrainMaxInValue := 0;
      FLastTrainMaxOutValue := 0;
      FInfo := '';
      FIsTraining := False;
      FTrainThreadRuning := False;

      FUserData := nil;
      FUserObject := nil;
    end;
end;

class function TLearn.CreateClassifier(const lt: TLearnType; const InDataLen: TLInt): TLearn;
begin
  if InDataLen <= 0 then
      RaiseInfo('input need > 0');

  Result := TLearn.Create;
  with Result do
    begin
      FEnabledRandomNumber := False;

      FInLen := InDataLen;
      FOutLen := 1;
      FMemorySource := TCoreClassList.Create;
      FKDToken := TKDTree.Create(FOutLen);
      FLearnType := lt;

      FLearnData := nil;
      FClassifier := True;
      FHideLayerDepth := hld0;
      CreateLearnData(False);

      FLastTrainMaxInValue := 0;
      FLastTrainMaxOutValue := 0;
      FInfo := '';
      FIsTraining := False;
      FTrainThreadRuning := False;

      FUserData := nil;
      FUserObject := nil;
    end;
end;

class function TLearn.CreateClassifier1(const lt: TLearnType; const InDataLen: TLInt): TLearn;
begin
  if InDataLen <= 0 then
      RaiseInfo('input need > 0');

  Result := TLearn.Create;
  with Result do
    begin
      FEnabledRandomNumber := False;

      FInLen := InDataLen;
      FOutLen := 1;
      FMemorySource := TCoreClassList.Create;
      FKDToken := TKDTree.Create(FOutLen);
      FLearnType := lt;

      FLearnData := nil;
      FClassifier := True;
      FHideLayerDepth := hld1;
      CreateLearnData(False);

      FLastTrainMaxInValue := 0;
      FLastTrainMaxOutValue := 0;
      FInfo := '';
      FIsTraining := False;
      FTrainThreadRuning := False;

      FUserData := nil;
      FUserObject := nil;
    end;
end;

class function TLearn.CreateClassifier2(const lt: TLearnType; const InDataLen: TLInt): TLearn;
begin
  if InDataLen <= 0 then
      RaiseInfo('input need > 0');

  Result := TLearn.Create;
  with Result do
    begin
      FEnabledRandomNumber := False;

      FInLen := InDataLen;
      FOutLen := 1;
      FMemorySource := TCoreClassList.Create;
      FKDToken := TKDTree.Create(FOutLen);
      FLearnType := lt;

      FLearnData := nil;
      FClassifier := True;
      FHideLayerDepth := hld2;
      CreateLearnData(False);

      FLastTrainMaxInValue := 0;
      FLastTrainMaxOutValue := 0;
      FInfo := '';
      FIsTraining := False;
      FTrainThreadRuning := False;

      FUserData := nil;
      FUserObject := nil;
    end;
end;

constructor TLearn.Create;
begin
  inherited Create;
  FEnabledRandomNumber := False;
  FInLen := 1;
  FOutLen := 1;
  FMemorySource := nil;
  FKDToken := nil;
  FLearnType := ltKDT;
  FLearnData := nil;
  FClassifier := True;
  FHideLayerDepth := hld0;
  FLastTrainMaxInValue := 0;
  FLastTrainMaxOutValue := 0;
  FInfo := '';
  FIsTraining := False;
  FTrainThreadRuning := False;
  FUserData := nil;
  FUserObject := nil;
end;

destructor TLearn.Destroy;
var
  i: TLInt;
begin
  WaitTrain;

  if FMemorySource <> nil then
    begin
      for i := 0 to FMemorySource.Count - 1 do
        begin
          SetLength(PLearnMemory(FMemorySource[i])^.m_in, 0);
          SetLength(PLearnMemory(FMemorySource[i])^.m_out, 0);
          Dispose(PLearnMemory(FMemorySource[i]));
        end;
      DisposeObject(FMemorySource);
      FMemorySource := nil;
    end;

  if FKDToken <> nil then
    begin
      DisposeObject(FKDToken);
      FKDToken := nil;
    end;

  FreeLearnData;

  FInLen := 0;
  FOutLen := 0;
  FInfo := '';
  inherited Destroy;
end;

procedure TLearn.Clear;
var
  i: TLInt;
  p: PLearnMemory;
  p_k: PLearnKDT;
  p_f: PDecisionForest;
  p_logit: PLogitModel;
  p_n: PMultiLayerPerceptron;
  p_e: PMLPEnsemble;
begin
  WaitTrain;

  if FMemorySource <> nil then
    begin
      for i := 0 to FMemorySource.Count - 1 do
        begin
          p := PLearnMemory(FMemorySource[i]);
          SetLength(p^.m_in, 0);
          SetLength(p^.m_out, 0);
          p^.token := '';
          Dispose(p);
        end;
      DisposeObject(FMemorySource);
      FMemorySource := nil;
    end;

  FMemorySource := TCoreClassList.Create;

  CreateLearnData(False);

  FLastTrainMaxInValue := 0;
  FLastTrainMaxOutValue := 0;
  FInfo := '';
end;

function TLearn.Count: TLInt;
begin
  Result := FMemorySource.Count;
end;

function TLearn.GetMemorySource(const index: TLInt): PLearnMemory;
begin
  Result := PLearnMemory(FMemorySource[index]);
end;

procedure TLearn.AddMemory(const f_In, f_Out: TLVec; f_token: SystemString);
var
  p: PLearnMemory;
  i: TLInt;
begin
  if FIsTraining or FTrainThreadRuning then
      RaiseInfo('wait Training');
  if length(f_In) <> FInLen then
      RaiseInfo('input length need = %d', [FInLen]);
  if FClassifier then
    begin
      if (length(f_Out) <> 1) then
          RaiseInfo('Classifier output length need >= 1', []);
    end
  else
    begin
      if (length(f_Out) <> FOutLen) then
          RaiseInfo('Regression output length need = %d', [FOutLen]);
    end;

  new(p);
  SetLength(p^.m_in, FInLen);
  CopyPtr(@f_In[0], @(p^.m_in[0]), FInLen * SizeOf(TLFloat));
  SetLength(p^.m_out, FOutLen);
  CopyPtr(@f_Out[0], @(p^.m_out[0]), FOutLen * SizeOf(TLFloat));
  p^.token := f_token;

  FMemorySource.Add(p);
end;

procedure TLearn.AddMemory(const f_In: TLVec; f_token: SystemString);
var
  f_Out: TLVec;
begin
  f_Out := LVec(FOutLen);
  LSetVec(f_Out, Count);
  AddMemory(f_In, f_Out, f_token);
  SetLength(f_Out, 0);
end;

procedure TLearn.AddMemory(const f_In, f_Out: TLVec);
begin
  AddMemory(f_In, f_Out, LVec(f_Out));
end;

procedure TLearn.AddMemory(const s_In, s_Out, s_token: SystemString);
var
  f_In, f_Out: TLVec;
begin
  f_In := LVec(s_In, FInLen);
  f_Out := LVec(s_Out, FOutLen);
  AddMemory(f_In, f_Out, s_token);
  SetLength(f_In, 0);
  SetLength(f_Out, 0);
end;

procedure TLearn.AddMemory(const s_In, s_Out: SystemString);
begin
  AddMemory(s_In, s_Out, s_Out);
end;

procedure TLearn.AddMemory(const s: TPascalString);
var
  s_In, s_Out: TPascalString;
  v: TLVec;
begin
  s_In := umlGetFirstStr(s, '=');
  s_Out := umlGetLastStr(s, '=');
  if s_Out.Exists(',') or umlIsNumber(s_Out) then
      AddMemory(s_In.Text, s_Out.Text)
  else
    begin
      v := LVec(FOutLen);
      LSetVec(v, Count);
      AddMemory(s_In.Text, LVec(v), s_Out.Text);
      SetLength(v, 0);
    end;
end;

procedure TLearn.AddSampler(const f_In, f_Out: TLVec);
begin
  AddMemory(f_In, f_Out);
end;

procedure TLearn.AddSampler(const s_In, s_Out: SystemString);
begin
  AddMemory(s_In, s_Out);
end;

procedure TLearn.AddSampler(const s: TPascalString);
begin
  AddMemory(s);
end;

procedure TLearn.AddMatrix(const m_in: TLMatrix; const f_Out: TLVec);
var
  f_In: TLVec;
begin
  f_In := LVec(m_in, FInLen);
  AddMemory(f_In, f_Out);
  SetLength(f_In, 0);
end;

procedure TLearn.AddMatrix(const m_in: TLMatrix; const f_Out: TLVec; const f_token: SystemString);
var
  f_In: TLVec;
begin
  f_In := LVec(m_in, FInLen);
  AddMemory(f_In, f_Out, f_token);
  SetLength(f_In, 0);
end;

function TLearn.Train(const TrainDepth: TLInt): Boolean;
var
  p_k: PLearnKDT;
  p_f: PDecisionForest;
  p_logit: PLogitModel;
  p_n: PMultiLayerPerceptron;
  p_e: PMLPEnsemble;
  kmIndexOut: TDynamicIndexArray;
  buff: TLMatrix;
  rInfo: TLInt;
  mlReport: TMLPReport;
  IsTerminated: Boolean;
  EBest: TLFloat;
  CVRep: TMLPCVReport;
  DFRep: TDFReport;
  logitRep: TMNLReport;

  procedure BuildInternalData;
  var
    i, j: TLInt;
    v: TLFloat;
  begin
    FLastTrainMaxInValue := PLearnMemory(FMemorySource[0])^.m_in[0];
    FLastTrainMaxOutValue := PLearnMemory(FMemorySource[0])^.m_out[0];

    if FClassifier then
      begin
        SetLength(buff, FMemorySource.Count, FInLen + 1);
        for i := 0 to FMemorySource.Count - 1 do
          begin
            for j := 0 to FInLen - 1 do
              begin
                v := PLearnMemory(FMemorySource[i])^.m_in[j];
                if v > FLastTrainMaxInValue then
                    FLastTrainMaxInValue := v;
                buff[i][j] := v;
              end;

            v := PLearnMemory(FMemorySource[i])^.m_out[0];;
            if v > FLastTrainMaxOutValue then
                FLastTrainMaxOutValue := v;
            buff[i][FInLen] := v;
          end;
        CreateLearnData(True);
      end
    else
      begin
        SetLength(buff, FMemorySource.Count, FInLen + FOutLen);
        for i := 0 to FMemorySource.Count - 1 do
          begin
            for j := 0 to FInLen - 1 do
              begin
                v := PLearnMemory(FMemorySource[i])^.m_in[j];
                if v > FLastTrainMaxInValue then
                    FLastTrainMaxInValue := v;
                buff[i][j] := v;
              end;

            for j := 0 to FOutLen - 1 do
              begin
                v := PLearnMemory(FMemorySource[i])^.m_out[j];
                if v > FLastTrainMaxOutValue then
                    FLastTrainMaxOutValue := v;
                buff[i][FInLen + j] := v;
              end;
          end;
      end;
  end;

  procedure FreeInternalData;
  begin
    SetLength(buff, 0, 0);
  end;

begin
  Result := False;

  if FIsTraining then
    begin
      FInfo := 'wait Training';
      Exit;
    end;

  if FMemorySource.Count <= 0 then
    begin
      FInfo := 'Out Training set invailed';
      Exit;
    end;

  FIsTraining := True;

  if not FEnabledRandomNumber then
      RandSeed := 0;

  FKDToken.BuildKDTreeM(FMemorySource.Count, nil, {$IFDEF FPC}@{$ENDIF FPC}TokenInput);

  try
    case FLearnType of
      ltKDT:
        begin
          CreateLearnData(True);
          p_k := PLearnKDT(FLearnData);
          p_k^.K.Clear;
          p_k^.K.BuildKDTreeM(FMemorySource.Count, nil, {$IFDEF FPC}@{$ENDIF FPC}KDInput);
          FInfo := 'task has been solved';
          Result := True;
        end;
      ltKM:
        begin
          CreateLearnData(True);
          p_k := PLearnKDT(FLearnData);
          p_k^.K.Clear;
          if (TrainDepth > 1) and (not FClassifier) and (TrainDepth < FMemorySource.Count) then
            begin
              p_k^.K.BuildKDTreeWithClusterM(FMemorySource.Count, TrainDepth, 1, kmIndexOut, nil, {$IFDEF FPC}@{$ENDIF FPC}KDInput);
            end
          else
            begin
              p_k^.K.BuildKDTreeM(FMemorySource.Count, nil, {$IFDEF FPC}@{$ENDIF FPC}KDInput);
            end;
          FInfo := 'task has been solved';
          Result := True;
        end;
      ltForest:
        begin
          BuildInternalData;
          p_f := PDecisionForest(FLearnData);
          if FClassifier then
              DFBuildRandomDecisionForest(buff, length(buff), FInLen, Max(1, Round(FLastTrainMaxOutValue) + 1), 100, 1, rInfo, p_f^, DFRep)
          else
              DFBuildRandomDecisionForest(buff, length(buff), FInLen, 1, 100, 1, rInfo, p_f^, DFRep);

          FreeInternalData;
          case rInfo of
            1: FInfo := 'task has been solved';
            -2: FInfo := 'there is a point with class number outside of [0..NClasses-1]';
            -1: FInfo := 'incorrect parameters was passed (NPoints<1, NVars<1, NClasses<1, NTrees<1, R<=0 or R>1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 1);
        end;
      ltLogit:
        begin
          BuildInternalData;
          p_logit := PLogitModel(FLearnData);
          MNLTrainH(buff, length(buff), FInLen, Max(2, Round(FLastTrainMaxOutValue) + 1), rInfo, p_logit^, logitRep);

          FreeInternalData;
          case rInfo of
            1: FInfo := 'task has been solved';
            -2: FInfo := 'there is a point with class number outside of [0..NClasses-1]';
            -1: FInfo := 'incorrect parameters was passed (NPoints<NVars+2, NVars<1, NClasses<2)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 1);
        end;
      ltLM:
        begin
          BuildInternalData;
          p_n := PMultiLayerPerceptron(FLearnData);
          MLPTrainLM(p_n^, buff, length(buff), 0.01, TrainDepth, rInfo, mlReport);

          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -9: FInfo := 'internal matrix inverse subroutine failed';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLM_MT:
        begin
          BuildInternalData;
          p_n := PMultiLayerPerceptron(FLearnData);
          MLPTrainLM_MT(p_n^, buff, length(buff), 0.01, TrainDepth, rInfo, mlReport);

          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -9: FInfo := 'internal matrix inverse subroutine failed';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLBFGS:
        begin
          BuildInternalData;
          p_n := PMultiLayerPerceptron(FLearnData);
          IsTerminated := False;
          MLPTrainLBFGS(p_n^, buff, length(buff), 0.01, TrainDepth, 0.01, 500, rInfo, mlReport, @IsTerminated, EBest);

          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -8: FInfo := 'if both WStep=0 and MaxIts=0';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLBFGS_MT:
        begin
          BuildInternalData;
          p_n := PMultiLayerPerceptron(FLearnData);
          IsTerminated := False;
          MLPTrainLBFGS_MT(p_n^, buff, length(buff), 0.01, TrainDepth, 0.01, 500, rInfo, mlReport);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -8: FInfo := 'if both WStep=0 and MaxIts=0';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLBFGS_MT_Mod:
        begin
          BuildInternalData;
          p_n := PMultiLayerPerceptron(FLearnData);
          IsTerminated := False;
          MLPTrainLBFGS_MT_Mod(p_n^, buff, length(buff), TrainDepth, 0.01, 2.0, 500, rInfo, mlReport);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -8: FInfo := 'if both WStep=0 and MaxIts=0';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltMonteCarlo:
        begin
          BuildInternalData;
          p_n := PMultiLayerPerceptron(FLearnData);
          IsTerminated := False;
          MLPTrainMonteCarlo(p_n^, buff, length(buff), 10, TrainDepth, 0, 1, rInfo, mlReport);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -8: FInfo := 'if both WStep=0 and MaxIts=0';
            -2: FInfo := 'there is a point with class number outside of [0..NOut-1]';
            -1: FInfo := 'wrong parameters specified (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLM_Ensemble, ltLM_Ensemble_MT:
        begin
          BuildInternalData;
          p_e := PMLPEnsemble(FLearnData);
          MLPEBaggingLM(FLearnType = ltLM_Ensemble_MT, p_e^, buff, length(buff), 0.01, TrainDepth, rInfo, mlReport, CVRep);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -2: FInfo := 'there is a point with class number outside of [0..NClasses-1]';
            -1: FInfo := 'incorrect parameters was passed (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
      ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT:
        begin
          BuildInternalData;
          p_e := PMLPEnsemble(FLearnData);
          MLPEBaggingLBFGS(FLearnType = ltLBFGS_Ensemble_MT, p_e^, buff, length(buff), 0.01, TrainDepth, 0.01, 500, rInfo, mlReport, CVRep);
          FreeInternalData;
          case rInfo of
            2: FInfo := 'task has been solved';
            -8: FInfo := 'both WStep=0 and MaxIts=0';
            -2: FInfo := 'there is a point with class number outside of [0..NClasses-1]';
            -1: FInfo := 'incorrect parameters was passed (NPoints<0, Restarts<1)';
            else FInfo := 'unknow state';
          end;
          Result := (rInfo = 2);
        end;
    end;
  finally
    FIsTraining := False;
    if not Result then
        DoStatus(FInfo);
  end;
end;

function TLearn.Train: Boolean;
begin
  Result := Train(1);
end;

procedure TLearn.Train_MT;
var
  th: TLearn_th;
begin
  WaitTrain;
  FTrainThreadRuning := True;
  th := TLearn_th.Create;
  th.Source := Self;
  th.TrainDepth := 1;
  th.Suspended := False;
end;

procedure TLearn.Train_MT(const TrainDepth: TLInt);
var
  th: TLearn_th;
begin
  WaitTrain;
  FTrainThreadRuning := True;
  th := TLearn_th.Create;
  th.Source := Self;
  th.TrainDepth := TrainDepth;
  th.Suspended := False;
end;

procedure TLearn.TrainC(const TrainDepth: TLInt; const OnResult: TLearnState_Call);
var
  th: TLearn_th;
begin
  WaitTrain;
  FTrainThreadRuning := True;
  th := TLearn_th.Create;
  th.Source := Self;
  th.OnStateC := OnResult;
  th.TrainDepth := TrainDepth;
  th.Suspended := False;
end;

procedure TLearn.TrainM(const TrainDepth: TLInt; const OnResult: TLearnState_Method);
var
  th: TLearn_th;
begin
  WaitTrain;
  FTrainThreadRuning := True;
  th := TLearn_th.Create;
  th.Source := Self;
  th.OnStateM := OnResult;
  th.TrainDepth := TrainDepth;
  th.Suspended := False;
end;

{$IFNDEF FPC}


procedure TLearn.TrainP(const TrainDepth: TLInt; const OnResult: TLearnState_Proc);
var
  th: TLearn_th;
begin
  WaitTrain;
  FTrainThreadRuning := True;
  th := TLearn_th.Create;
  th.Source := Self;
  th.OnStateP := OnResult;
  th.TrainDepth := TrainDepth;
  th.Suspended := False;
end;
{$ENDIF FPC}


procedure TLearn.WaitTrain;
begin
  while FTrainThreadRuning do
      CheckThreadSynchronize(1);
end;

function TLearn.SearchToken(const v: TLVec): SystemString;
begin
  Result := FKDToken.SearchToken(v);
end;

function TLearn.Process(const p_in, p_out: PLVec): Boolean;
var
  p_kd_node: PKDTree_Node;
  i: TLInt;
  r, rmax: TLFloat;
  List: TLIVec;
begin
  Result := False;
  try
    if FIsTraining or FTrainThreadRuning then
      begin
        FInfo := 'wait training';
        Exit;
      end;
    if length(p_in^) <> FInLen then
      begin
        FInfo := 'input length error';
        Exit;
      end;

    case FLearnType of
      ltKDT, ltKM:
        begin
          if PLearnKDT(FLearnData)^.K.Count > 0 then
            begin
              if FClassifier then
                begin
                  SearchMemoryDistance(p_in^, List);
                  SetLength(p_out^, length(List));

                  for i := 0 to length(List) - 1 do
                      p_out^[List[i]] := (length(List) - 1) - i;
                  SetLength(List, 0);
                end
              else
                begin
                  p_kd_node := PLearnKDT(FLearnData)^.K.Search(p_in^);
                  SetLength(p_out^, FOutLen);
                  if p_kd_node <> nil then
                      CopyPtr(@(PLearnMemory(FMemorySource[p_kd_node^.vec^.index])^.m_out[0]), @p_out^[0], FOutLen * SizeOf(TLFloat));
                end;
              FInfo := 'successed';
              Result := True;
            end;
        end;
      ltForest:
        begin
          if length(PDecisionForest(FLearnData)^.Trees) > 0 then
            begin
              if FClassifier then
                  SetLength(p_out^, Max(1, Round(FLastTrainMaxOutValue) + 1))
              else
                  SetLength(p_out^, 1);

              DFProcess(PDecisionForest(FLearnData)^, p_in^, p_out^);
              FInfo := 'successed';
              Result := True;
            end;
        end;
      ltLogit:
        begin
          if length(PLogitModel(FLearnData)^.w) > 0 then
            begin
              SetLength(p_out^, Max(2, Round(FLastTrainMaxOutValue) + 1));

              MNLProcess(PLogitModel(FLearnData)^, p_in^, p_out^);
              FInfo := 'successed';
              Result := True;
            end;
        end;
      ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo:
        begin
          if FClassifier then
              SetLength(p_out^, Max(2, Round(FLastTrainMaxOutValue) + 1))
          else
              SetLength(p_out^, FOutLen);

          MLPProcess(PMultiLayerPerceptron(FLearnData)^, p_in^, p_out^);
          FInfo := 'successed';
          Result := True;
        end;
      ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT:
        begin
          if FClassifier then
              SetLength(p_out^, Max(2, Round(FLastTrainMaxOutValue) + 1))
          else
              SetLength(p_out^, FOutLen);

          MLPEProcess(PMLPEnsemble(FLearnData)^, p_in^, p_out^);
          FInfo := 'successed';
          Result := True;
        end;
    end;
  finally
    if not Result then
        DoStatus(FInfo);
  end;
end;

function TLearn.Process(const ProcessIn: PLVec): SystemString;
var
  ProcessOut: TLVec;
begin
  Result := '';
  if not Process(ProcessIn, @ProcessOut) then
      Exit;
  Result := LVec(ProcessOut, True);
end;

function TLearn.Process(const ProcessIn: TLVec): SystemString;
begin
  Result := Process(PLVec(@ProcessIn));
end;

function TLearn.Process(const ProcessIn: TPascalString): SystemString;
begin
  Result := Process(TKDTree.KDTreeVec(ProcessIn.Text));
end;

function TLearn.ProcessMatrix(const p_in: PLMatrix; const p_out: PLVec): Boolean;
var
  f_In: TLVec;
begin
  f_In := LVec(p_in^, FInLen);
  Result := Process(@f_In, p_out);
  SetLength(f_In, 0);
end;

function TLearn.ProcessToken(const ProcessIn: PLVec): SystemString;
var
  ProcessOut: TLVec;
begin
  Result := '';
  if not Process(ProcessIn, @ProcessOut) then
      Exit;
  Result := SearchToken(ProcessOut);
  if Result = '' then
      Result := LVec(ProcessOut, True);
end;

function TLearn.ProcessMax(const ProcessIn: TLVec): TLFloat;
var
  ProcessOut: TLVec;
  i: TLInt;
begin
  Result := 0;
  if not Process(@ProcessIn, @ProcessOut) then
      Exit;

  Result := ProcessOut[0];

  if length(ProcessOut) > 1 then
    for i := 1 to length(ProcessOut) - 1 do
      if ProcessOut[i] > Result then
          Result := ProcessOut[i];

  SetLength(ProcessOut, 0);
end;

function TLearn.ProcessMax(const ProcessIn: TLMatrix): TLFloat;
var
  f_In: TLVec;
begin
  f_In := LVec(ProcessIn, FInLen);
  Result := ProcessMax(f_In);
  SetLength(f_In, 0);
end;

function TLearn.ProcessMaxToken(const ProcessIn: TLVec): SystemString;
begin
  if FClassifier then
      Result := ProcessMaxIndexToken(ProcessIn)
  else
      Result := '';
end;

function TLearn.ProcessMaxToken(const ProcessIn: TLMatrix): SystemString;
begin
  if FClassifier then
      Result := ProcessMaxIndexToken(ProcessIn)
  else
      Result := '';
end;

function TLearn.ProcessMaxIndex(const ProcessIn: TLVec): TLInt;
var
  ProcessOut: TLVec;
  K: TLFloat;
  i: TLInt;
begin
  Result := -1;
  if not Process(@ProcessIn, @ProcessOut) then
      Exit;

  K := ProcessOut[0];
  Result := 0;

  if length(ProcessOut) > 1 then
    for i := 1 to length(ProcessOut) - 1 do
      if ProcessOut[i] > K then
        begin
          Result := i;
          K := ProcessOut[i];
        end;

  SetLength(ProcessOut, 0);
end;

function TLearn.ProcessMaxIndex(const ProcessIn: TLMatrix): TLInt;
var
  f_In: TLVec;
begin
  f_In := LVec(ProcessIn, FInLen);
  Result := ProcessMaxIndex(f_In);
  SetLength(f_In, 0);
end;

function TLearn.ProcessMaxIndexToken(const ProcessIn: TLVec): SystemString;
var
  i: TLInt;
  p: PKDTree_Source;
begin
  Result := '';
  if not FClassifier then
      Exit;
  i := ProcessMaxIndex(ProcessIn);
  if (i >= 0) and (i < FKDToken.Count) then
    begin
      p := FKDToken.SourceP[i];
      Result := p^.token;
    end;
end;

function TLearn.ProcessMaxIndexToken(const ProcessIn: TLMatrix): SystemString;
var
  f_In: TLVec;
begin
  f_In := LVec(ProcessIn, FInLen);
  Result := ProcessMaxIndexToken(f_In);
  SetLength(f_In, 0);
end;

function TLearn.ProcessMin(const ProcessIn: TLVec): TLFloat;
var
  ProcessOut: TLVec;
  i: TLInt;
begin
  Result := 0;
  if not Process(@ProcessIn, @ProcessOut) then
      Exit;

  Result := ProcessOut[0];

  if length(ProcessOut) > 1 then
    for i := 1 to length(ProcessOut) - 1 do
      if ProcessOut[i] < Result then
          Result := ProcessOut[i];

  SetLength(ProcessOut, 0);
end;

function TLearn.ProcessMin(const ProcessIn: TLMatrix): TLFloat;
var
  f_In: TLVec;
begin
  f_In := LVec(ProcessIn, FInLen);
  Result := ProcessMin(f_In);
  SetLength(f_In, 0);
end;

function TLearn.ProcessMinToken(const ProcessIn: TLVec): SystemString;
begin
  if FClassifier then
      Result := ProcessMinIndexToken(ProcessIn)
  else
      Result := '';
end;

function TLearn.ProcessMinToken(const ProcessIn: TLMatrix): SystemString;
begin
  if FClassifier then
      Result := ProcessMinIndexToken(ProcessIn)
  else
      Result := '';
end;

function TLearn.ProcessMinIndex(const ProcessIn: TLVec): TLInt;
var
  ProcessOut: TLVec;
  K: TLFloat;
  i: TLInt;
begin
  Result := -1;
  if not Process(@ProcessIn, @ProcessOut) then
      Exit;

  K := ProcessOut[0];
  Result := 0;

  if length(ProcessOut) > 1 then
    for i := 1 to length(ProcessOut) - 1 do
      if ProcessOut[i] < K then
        begin
          Result := i;
          K := ProcessOut[i];
        end;

  SetLength(ProcessOut, 0);
end;

function TLearn.ProcessMinIndex(const ProcessIn: TLMatrix): TLInt;
var
  f_In: TLVec;
begin
  f_In := LVec(ProcessIn, FInLen);
  Result := ProcessMinIndex(f_In);
  SetLength(f_In, 0);
end;

function TLearn.ProcessMinIndexToken(const ProcessIn: TLVec): SystemString;
var
  i: TLInt;
  p: PKDTree_Source;
begin
  Result := '';
  if not FClassifier then
      Exit;
  i := ProcessMinIndex(ProcessIn);
  if (i >= 0) and (i < FKDToken.Count) then
    begin
      p := FKDToken.SourceP[i];
      Result := p^.token;
    end;
end;

function TLearn.ProcessMinIndexToken(const ProcessIn: TLMatrix): SystemString;
var
  f_In: TLVec;
begin
  f_In := LVec(ProcessIn, FInLen);
  Result := ProcessMinIndexToken(f_In);
  SetLength(f_In, 0);
end;

function TLearn.ProcessFV(const ProcessIn: TLVec): TLFloat;
var
  ProcessOut: TLVec;
begin
  Result := 0;
  if not Process(@ProcessIn, @ProcessOut) then
      Exit;

  Result := ProcessOut[0];

  SetLength(ProcessOut, 0);
end;

function TLearn.ProcessFV(const ProcessIn: TLMatrix): TLFloat;
var
  f_In: TLVec;
begin
  f_In := LVec(ProcessIn, FInLen);
  Result := ProcessFV(f_In);
  SetLength(f_In, 0);
end;

function TLearn.ProcessFV(const ProcessIn: TPascalString): TLFloat;
begin
  Result := ProcessFV(TKDTree.KDTreeVec(ProcessIn.Text));
end;

function TLearn.ProcessLV(const ProcessIn: TLVec): TLFloat;
var
  ProcessOut: TLVec;
begin
  Result := 0;
  if not Process(@ProcessIn, @ProcessOut) then
      Exit;

  Result := ProcessOut[length(ProcessOut) - 1];

  SetLength(ProcessOut, 0);
end;

function TLearn.ProcessLV(const ProcessIn: TLMatrix): TLFloat;
var
  f_In: TLVec;
begin
  f_In := LVec(ProcessIn, FInLen);
  Result := ProcessLV(f_In);
  SetLength(f_In, 0);
end;

function TLearn.ProcessLV(const ProcessIn: TPascalString): TLFloat;
begin
  Result := ProcessLV(TKDTree.KDTreeVec(ProcessIn.Text));
end;

function TLearn.SearchMemoryPearson(const ProcessIn: TLVec): TLInt;
var
  K, r: TLFloat;
  i: TLInt;
begin
  if Count <= 0 then
    begin
      Result := -1;
      Exit;
    end;

  K := PearsonCorrelation(ProcessIn, GetMemorySource(0)^.m_in, FInLen);
  Result := 0;

  for i := 1 to Count - 1 do
    begin
      r := PearsonCorrelation(ProcessIn, GetMemorySource(i)^.m_in, FInLen);
      if (r <> 0) and (r > K) then
        begin
          K := r;
          Result := i;
        end;
    end;
end;

procedure TLearn.SearchMemoryPearson(const ProcessIn: TLVec; out List: TLIVec);
{$REGION 'Imp'}

type
  TState = record
    K: TLFloat;
    index: TLInt;
  end;

  PState = ^TState;

  TStatePtrArray = array of PState;
  TStateArray = array of TState;

  function SortCompare(const p1, p2: PState): ShortInt; inline;
  begin
    if p1^.K < p2^.K then
        Result := -1
    else if p1^.K > p2^.K then
        Result := 1
    else
      begin
        Result := 0;
        if p1^.index = p2^.index then
            Result := 0
        else if p1^.index < p2^.index then
            Result := -1
        else
            Result := 1;
      end;
  end;
  procedure InternalSort(var SortBuffer: TStatePtrArray; L, r: TLInt);
  var
    i, j: TLInt;
    p, t: PState;
  begin
    repeat
      i := L;
      j := r;
      p := SortBuffer[(L + r) shr 1];
      repeat
        while SortCompare(SortBuffer[i], p) < 0 do
            inc(i);
        while SortCompare(SortBuffer[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j);
      L := i;
    until i >= r;
  end;

var
  buff: TStateArray;
  buffPtr: TStatePtrArray;
  i: TLInt;
begin
  if Count <= 0 then
      Exit;
  if Count = 1 then
    begin
      SetLength(List, 1);
      List[0] := 0;
      Exit;
    end;
  SetLength(buff, Count);
  SetLength(buffPtr, Count);

  for i := 0 to Count - 1 do
    begin
      buff[i].K := PearsonCorrelation(ProcessIn, GetMemorySource(i)^.m_in, FInLen);
      buff[i].index := i;
      buffPtr[i] := @buff[i];
    end;

  // complete sort
  InternalSort(buffPtr, 0, length(buffPtr) - 1);

  SetLength(List, Count);
  for i := 0 to Count - 1 do
      List[i] := buffPtr[i]^.index;

  SetLength(buff, 0);
  SetLength(buffPtr, 0);
end;
{$ENDREGION 'Imp'}


function TLearn.SearchMemorySpearman(const ProcessIn: TLVec): TLInt;
var
  K, r: TLFloat;
  i: TLInt;
begin
  if Count <= 0 then
    begin
      Result := -1;
      Exit;
    end;

  K := SpearmanRankCorrelation(ProcessIn, GetMemorySource(0)^.m_in, FInLen);
  Result := 0;

  for i := 1 to Count - 1 do
    begin
      r := SpearmanRankCorrelation(ProcessIn, GetMemorySource(i)^.m_in, FInLen);
      if (r <> 0) and (r > K) then
        begin
          K := r;
          Result := i;
        end;
    end;
end;

procedure TLearn.SearchMemorySpearman(const ProcessIn: TLVec; out List: TLIVec);
{$REGION 'Imp'}

type
  TState = record
    K: TLFloat;
    index: TLInt;
  end;

  PState = ^TState;

  TStatePtrArray = array of PState;
  TStateArray = array of TState;

  function SortCompare(const p1, p2: PState): ShortInt; inline;
  begin
    if p1^.K < p2^.K then
        Result := -1
    else if p1^.K > p2^.K then
        Result := 1
    else
      begin
        Result := 0;
        if p1^.index = p2^.index then
            Result := 0
        else if p1^.index < p2^.index then
            Result := -1
        else
            Result := 1;
      end;
  end;
  procedure InternalSort(var SortBuffer: TStatePtrArray; L, r: TLInt);
  var
    i, j: TLInt;
    p, t: PState;
  begin
    repeat
      i := L;
      j := r;
      p := SortBuffer[(L + r) shr 1];
      repeat
        while SortCompare(SortBuffer[i], p) < 0 do
            inc(i);
        while SortCompare(SortBuffer[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j);
      L := i;
    until i >= r;
  end;

var
  buff: TStateArray;
  buffPtr: TStatePtrArray;
  i: TLInt;
begin
  if Count <= 0 then
      Exit;
  if Count = 1 then
    begin
      SetLength(List, 1);
      List[0] := 0;
      Exit;
    end;
  SetLength(buff, Count);
  SetLength(buffPtr, Count);

  for i := 0 to Count - 1 do
    begin
      buff[i].K := SpearmanRankCorrelation(ProcessIn, GetMemorySource(i)^.m_in, FInLen);
      buff[i].index := i;
      buffPtr[i] := @buff[i];
    end;

  // complete sort
  InternalSort(buffPtr, 0, length(buffPtr) - 1);

  SetLength(List, Count);
  for i := 0 to Count - 1 do
      List[i] := buffPtr[i]^.index;

  SetLength(buff, 0);
  SetLength(buffPtr, 0);
end;
{$ENDREGION 'Imp'}


function TLearn.SearchMemoryDistance(const ProcessIn: TLVec): TLInt;
var
  K, r: Double;
  i: TLInt;
begin
  if Count <= 0 then
    begin
      Result := -1;
      Exit;
    end;

  if length(ProcessIn) <> FInLen then
      RaiseInfo('processIn need Length=%d', [FInLen]);
  K := TKDTree.KDTreeDistance(ProcessIn, GetMemorySource(0)^.m_in);
  Result := 0;

  for i := 1 to Count - 1 do
    begin
      r := TKDTree.KDTreeDistance(ProcessIn, GetMemorySource(i)^.m_in);
      if (r < K) then
        begin
          K := r;
          Result := i;
        end;
    end;
end;

procedure TLearn.SearchMemoryDistance(const ProcessIn: TLVec; out List: TLIVec);
type
  TState = record
    K: Double;
    index: TLInt;
  end;

  PState = ^TState;

  TStatePtrArray = array of PState;
  TStateArray = array of TState;

  function SortCompare(const p1, p2: PState): ShortInt; inline;
  begin
    if p1^.K < p2^.K then
        Result := -1
    else if p1^.K > p2^.K then
        Result := 1
    else
      begin
        Result := 0;
        if p1^.index = p2^.index then
            Result := 0
        else if p1^.index < p2^.index then
            Result := -1
        else
            Result := 1;
      end;
  end;
  procedure InternalSort(var SortBuffer: TStatePtrArray; L, r: TLInt);
  var
    i, j: TLInt;
    p, t: PState;
  begin
    repeat
      i := L;
      j := r;
      p := SortBuffer[(L + r) shr 1];
      repeat
        while SortCompare(SortBuffer[i], p) < 0 do
            inc(i);
        while SortCompare(SortBuffer[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortBuffer[i];
                SortBuffer[i] := SortBuffer[j];
                SortBuffer[j] := t;
              end;
            inc(i);
            dec(j);
          end;
      until i > j;
      if L < j then
          InternalSort(SortBuffer, L, j);
      L := i;
    until i >= r;
  end;

var
  buff: TStateArray;
  buffPtr: TStatePtrArray;
  i: TLInt;
begin
  if Count <= 0 then
    begin
      Exit;
    end;

  if Count < 2 then
    begin
      SetLength(List, 1);
      List[0] := 0;
      Exit;
    end;

  SetLength(buff, Count);
  SetLength(buffPtr, Count);

  for i := 0 to Count - 1 do
    begin
      buff[i].K := TKDTree.KDTreeDistance(ProcessIn, GetMemorySource(i)^.m_in);
      buff[i].index := i;
      buffPtr[i] := @buff[i];
    end;

  // complete sort
  InternalSort(buffPtr, 0, length(buffPtr) - 1);

  SetLength(List, Count);
  for i := 0 to Count - 1 do
      List[i] := buffPtr[i]^.index;

  SetLength(buff, 0);
  SetLength(buffPtr, 0);
end;

procedure TLearn.SaveToDF(df: TDataFrameEngine);
var
  p: PLearnMemory;
  ar: TDataFrameArrayDouble;
  nd: TDataFrameEngine;
  i, j: TLInt;
  buff: TLVec;
  buffLen: TLInt;
  m64: TMemoryStream64;
begin
  df.WriteInt64(FInLen);
  df.WriteInt64(FOutLen);
  df.WriteByte(Byte(FLearnType));
  df.WriteBool(FEnabledRandomNumber);
  df.WriteBool(FClassifier);
  df.WriteByte(Byte(FHideLayerDepth));
  df.WriteDouble(FLastTrainMaxInValue);
  df.WriteDouble(FLastTrainMaxOutValue);

  ar := df.WriteArrayDouble;
  nd := TDataFrameEngine.Create;
  for i := 0 to FMemorySource.Count - 1 do
    begin
      p := PLearnMemory(FMemorySource[i]);
      for j := 0 to FInLen - 1 do
          ar.Add(p^.m_in[j]);
      for j := 0 to FOutLen - 1 do
          ar.Add(p^.m_out[j]);

      nd.WriteString(p^.token);
    end;
  df.WriteDataFrameCompressed(nd);
  DisposeObject(nd);

  m64 := TMemoryStream64.Create;
  FKDToken.SaveToStream(m64);
  df.WriteStream(m64);
  DisposeObject(m64);

  case FLearnType of
    ltKDT, ltKM:
      begin
        if PLearnKDT(FLearnData)^.K.Count > 0 then
          begin
            m64 := TMemoryStream64.Create;
            PLearnKDT(FLearnData)^.K.SaveToStream(m64);
            df.WriteStream(m64);
            DisposeObject(m64);
          end;
      end;
    ltForest:
      begin
        if length(PDecisionForest(FLearnData)^.Trees) > 0 then
          begin
            DFSerialize(PDecisionForest(FLearnData)^, buff, buffLen);
            ar := df.WriteArrayDouble;
            for i := 0 to buffLen - 1 do
                ar.Add(buff[i]);
          end;
      end;
    ltLogit:
      begin
        if length(PLogitModel(FLearnData)^.w) > 0 then
          begin
            MNLSerialize(PLogitModel(FLearnData)^, buff, buffLen);
            ar := df.WriteArrayDouble;
            for i := 0 to buffLen - 1 do
                ar.Add(buff[i]);
          end;
      end;
    ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo:
      begin
        if length(PMultiLayerPerceptron(FLearnData)^.Neurons) > 0 then
          begin
            MLPSerialize(PMultiLayerPerceptron(FLearnData)^, buff, buffLen);
            ar := df.WriteArrayDouble;
            for i := 0 to buffLen - 1 do
                ar.Add(buff[i]);
          end;
      end;
    ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT:
      begin
        if length(PMLPEnsemble(FLearnData)^.DFDNET) > 0 then
          begin
            MLPESerialize(PMLPEnsemble(FLearnData)^, buff, buffLen);
            ar := df.WriteArrayDouble;
            for i := 0 to buffLen - 1 do
                ar.Add(buff[i]);
          end;
      end;
  end;
end;

procedure TLearn.LoadFromDF(df: TDataFrameEngine);
var
  ar: TDataFrameArrayDouble;
  nd: TDataFrameEngine;
  i, j: TLInt;
  plm: PLearnMemory;
  buff: TLVec;
  m64: TMemoryStream64;
begin
  Clear;

  FInLen := df.Reader.ReadInt64;
  FOutLen := df.Reader.ReadInt64;
  FLearnType := TLearnType(df.Reader.ReadByte);
  FEnabledRandomNumber := df.Reader.ReadBool;
  FClassifier := df.Reader.ReadBool;
  FHideLayerDepth := THideLayerDepth(df.Reader.ReadByte);
  FLastTrainMaxInValue := df.Reader.ReadDouble;
  FLastTrainMaxOutValue := df.Reader.ReadDouble;

  ar := df.Reader.ReadArrayDouble;
  nd := TDataFrameEngine.Create;
  df.Reader.ReadDataFrame(nd);

  i := 0;
  while i < ar.Count do
    begin
      new(plm);
      SetLength(plm^.m_in, FInLen);
      SetLength(plm^.m_out, FOutLen);
      plm^.token := nd.Reader.ReadString;
      FMemorySource.Add(plm);

      j := 0;
      while j < FInLen do
        begin
          plm^.m_in[j] := ar[i];
          inc(j);
          inc(i);
        end;

      j := 0;
      while j < FOutLen do
        begin
          plm^.m_out[j] := ar[i];
          inc(j);
          inc(i);
        end;
    end;
  DisposeObject(nd);

  if df.Reader.IsEnd then
    begin
      Train;
      Exit;
    end;

  m64 := TMemoryStream64.Create;
  df.Reader.ReadStream(m64);
  m64.Position := 0;
  FKDToken.LoadFromStream(m64);
  DisposeObject(m64);

  if df.Reader.IsEnd then
    begin
      Train;
      Exit;
    end;

  case FLearnType of
    ltKDT, ltKM:
      begin
        m64 := TMemoryStream64.Create;
        df.Reader.ReadStream(m64);
        m64.Position := 0;
        try
            PLearnKDT(FLearnData)^.K.LoadFromStream(m64);
        except
            Train;
        end;
        DisposeObject(m64);
      end;
    ltForest:
      begin
        ar := df.Reader.ReadArrayDouble;
        SetLength(buff, ar.Count);
        for i := 0 to ar.Count - 1 do
            buff[i] := ar[i];

        try
            DFUnserialize(buff, PDecisionForest(FLearnData)^);
        except
            Train;
        end;
        SetLength(buff, 0);
      end;
    ltLogit:
      begin
        ar := df.Reader.ReadArrayDouble;
        SetLength(buff, ar.Count);
        for i := 0 to ar.Count - 1 do
            buff[i] := ar[i];

        try
            MNLUnserialize(buff, PLogitModel(FLearnData)^);
        except
            Train;
        end;
        SetLength(buff, 0);
      end;
    ltLM, ltLM_MT, ltLBFGS, ltLBFGS_MT, ltLBFGS_MT_Mod, ltMonteCarlo:
      begin
        ar := df.Reader.ReadArrayDouble;
        SetLength(buff, ar.Count);
        for i := 0 to ar.Count - 1 do
            buff[i] := ar[i];

        try
            MLPUNSerialize(buff, PMultiLayerPerceptron(FLearnData)^);
        except
            Train;
        end;
        SetLength(buff, 0);
      end;
    ltLM_Ensemble, ltLM_Ensemble_MT, ltLBFGS_Ensemble, ltLBFGS_Ensemble_MT:
      begin
        ar := df.Reader.ReadArrayDouble;
        SetLength(buff, ar.Count);
        for i := 0 to ar.Count - 1 do
            buff[i] := ar[i];

        try
            MLPEUNSerialize(buff, PMLPEnsemble(FLearnData)^);
        except
            Train;
        end;
        SetLength(buff, 0);
      end;
  end;
end;

procedure TLearn.SaveToStream(stream: TCoreClassStream);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  SaveToDF(de);

  de.EncodeTo(stream, True);
  DisposeObject(de);
end;

procedure TLearn.LoadFromStream(stream: TCoreClassStream);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.DecodeFrom(stream, True);

  LoadFromDF(de);

  DisposeObject(de);
end;

procedure TLearn.SaveToFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TLearn.LoadFromFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

{$IFNDEF FPC}


procedure TLearn.SaveToJsonStream(stream: TCoreClassStream);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  SaveToDF(de);

  de.EncodeAsJson(stream);
  DisposeObject(de);
end;

procedure TLearn.LoadFromJsonStream(stream: TCoreClassStream);
var
  de: TDataFrameEngine;
begin
  Clear;

  de := TDataFrameEngine.Create;
  de.DecodeFromJson(stream);

  LoadFromDF(de);

  DisposeObject(de);
end;

procedure TLearn.SaveToJsonFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToJsonStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TLearn.LoadFromJsonFile(FileName: SystemString);
var
  fs: TCoreClassFileStream;
begin
  try
      fs := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  try
      LoadFromJsonStream(fs);
  finally
      DisposeObject(fs);
  end;
end;
{$ENDIF FPC}
