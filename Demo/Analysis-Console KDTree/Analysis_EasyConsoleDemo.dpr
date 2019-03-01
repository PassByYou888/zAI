program Analysis_EasyConsoleDemo;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  CoreClasses,
  DoStatusIO,
  PascalStrings,
  UnicodeMixedLib,
  MemoryStream64,
  FastKDTreeD,
  KM,
  MH_1;

procedure Demo;
var
  i, j           : Integer;
  buff           : TKDT1DD.TKDT1DD_DynamicVecBuffer;
  outIndex       : TDynamicIndexArray;
  k1t            : TKDT1DD;
  k1r            : TKDT1DD.PKDT1DD_Node;
  d              : Double;
  SearchedCounter: NativeInt;
  n              : string;
  NearestNodes   : TCoreClassList;
begin
  k1t := TKDT1DD.Create;

  NearestNodes := TCoreClassList.Create;

  SetLength(buff, 10000);
  for i := 0 to Length(buff) - 1 do
    for j := 0 to KDT1DD_Axis - 1 do
        buff[i][j] := umlRandomRangeD(-3000, 3000);

  k1t.BuildKDTreeWithCluster(buff, 100, 1, outIndex);

  repeat
    DoStatus('wait input (cmd:buff,tree,origin,exit, number:[x,x,x,x])');
    n := '';
    readln(n);
    if n <> '' then
      begin
        if umlMultipleMatch(['buff'], n) then
            k1t.PrintBuffer
        else if umlMultipleMatch(['origin'], n) then
          begin
            for i := 0 to Length(buff) - 1 do
                DoStatus('%d. %s ', [i, TKDT1DD.KDT1DDVec(buff[i])]);
          end
        else if umlMultipleMatch(['tree'], n) then
            k1t.PrintNodeTree(k1t.RootNode)
        else if SmithWatermanCompare(['exit'], n) > 0.5 then
            break
        else
          begin
            NearestNodes.Clear;
            k1r := k1t.Search(TKDT1DD.KDT1DDVec(n), d, SearchedCounter, NearestNodes);

            DoStatus(Format('finded total:%d Nearest:%d dist:%f', [SearchedCounter, k1r^.vec^.index,
              sqrt(TKDT1DD.KDT1DDDistance(TKDT1DD.KDT1DDVec(n), k1r^.vec^.buff))]));
            for i := 0 to NearestNodes.Count - 1 do
                DoStatus(Format('index:%d dist:%f',
                [TKDT1DD.PKDT1DD_Node(NearestNodes[i])^.vec^.index, sqrt(TKDT1DD.KDT1DDDistance(TKDT1DD.PKDT1DD_Node(NearestNodes[i])^.vec^.buff, k1r^.vec^.buff))]));
          end;
      end
    else
        k1t.PrintBuffer;
  until False;

  disposeObject([k1t, NearestNodes]);
end;

begin
  MH_1.BeginMemoryHook($FFFFF);
  FastKDTreeD.Test_All();
  Demo;
  MH_1.EndMemoryHook;
  MH_1.HookPtrList.ProgressP(procedure(NPtr: Pointer; uData: NativeUInt)
    begin
      DoStatus('leak memory:0x%s', [IntToHex(NativeUInt(NPtr), sizeof(Pointer) * 2)]);
      DoStatus(NPtr, uData, 80);
    end);
  DoStatus('memory leak count: %s', [umlSizeToStr(MH_1.HookPtrList.Total).Text]);
  if MH_1.HookPtrList.Total > 0 then
      readln;
end.
