(*
 *	 Unit owner: D10.Mofen
 *         homePage: http://www.diocp.org
 *	       blog: http://www.cnblogs.com/dksoft

 *   1. ��չ������TDiocpExTcpServer, ���Զ��忪ʼ��־�ͽ�����־(Ҳ����ֻ�趨������־)��Ȼ���Զ����н������OnContextDataAction�¼���
 *   2. �ַ���������TDiocpStringTcpServer, �����趨��ʼ�ַ����ͽ����ַ���(Ҳ����ֻ�趨�����ַ���)��Ȼ���Զ����н������OnContextStringAction�¼���
 *      2015-07-15 09:00:09
 *
 *   3. �޸�ex.server�������⣬���ʹ�����ʱ���޷������bug
 *      2015-08-17 14:25:56

*)

unit diocp_ex_server;

interface

{$DEFINE StartEndCut}

uses
  diocp_tcp_server, utils_rawPackage, utils_safeLogger, SysUtils, Classes,
  utils_strings, diocp_res;

type
  TDiocpExContext = class;
  TDiocpStringContext = class;
  
  TContextDataActionEvent = procedure(pvContext:TDiocpExContext; pvData: Pointer;
      pvDataLen: Integer) of object;

  TContextStringActionEvent = procedure(pvContext:TDiocpStringContext;
      pvDataString:String) of object;


  TDiocpExContext = class(TIocpClientContext)
  private
    FCacheBuffer: TRawPackage;
    FRecvData: array of Byte;
  protected
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;
    procedure OnDataAction(pvData: Pointer; pvDataLen: Integer);
    procedure DoCleanUp;override;
  public
    constructor Create; override;
    destructor Destroy; override;

    /// <summary>
    ///   �Զ����ǰ���־
    /// </summary>
    procedure WriteData(pvData: Pointer; pvDataLen: Integer);
  end;



  TDiocpExTcpServer = class(TDiocpTcpServer)
  private
    FStartData: array [0..254] of Byte;
    FStartDataLen:Byte;

    FEndData:array [0..254] of Byte;
    FEndDataLen: Byte;

    /// �����������ݰ�����
    FMaxDataLen:Integer;
    FOnContextDataAction: TContextDataActionEvent;
  protected
    procedure DoDataAction(pvContext: TDiocpExContext; pvData: Pointer; pvDataLen:
        Integer);virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetStart(pvData:Pointer; pvDataLen:Byte);
    procedure SetEnd(pvData:Pointer; pvDataLen:Byte);

    /// <summary>
    ///  �����������ݰ�����
    ///  ��������С��0������
    ///   10M (1024 * 1024 * 10)
    /// </summary>
    procedure SetMaxDataLen(pvDataLen:Integer);

    property OnContextDataAction: TContextDataActionEvent read FOnContextDataAction write FOnContextDataAction; 
  end;


  TDiocpStringContext = class(TDiocpExContext)
  public
    procedure WriteAnsiString(pvData:AnsiString);
  end;

  TDiocpStringTcpServer = class(TDiocpExTcpServer)
  private
    FOnContextStringAction: TContextStringActionEvent;
  protected
    procedure DoDataAction(pvContext: TDiocpExContext; pvData: Pointer; pvDataLen:
        Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetPackEndStr(pvEndStr:AnsiString);
    procedure SetPackStartStr(pvStartStr:AnsiString);
    property OnContextStringAction: TContextStringActionEvent read FOnContextStringAction write FOnContextStringAction;
  end;

implementation


constructor TDiocpExContext.Create;
begin
  inherited Create;
end;

destructor TDiocpExContext.Destroy;
begin
  inherited Destroy;
end;



procedure TDiocpExContext.DoCleanUp;
begin
  inherited DoCleanUp;
  FCacheBuffer.FEndBytesLength := 0;
  FCacheBuffer.FStartBytesLength := 0;
end;

procedure TDiocpExContext.OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD);
var
  j, i, x, r:Integer;
  str:AnsiString;
  pstr, pbuf, prsearch:PAnsiChar;

  lvStartData:Pointer;
  lvStartDataLen:Byte;
  
  lvEndData:Pointer;
  lvEndDataLen:Byte;

  lvOwner:TDiocpExTcpServer;

  lvPtr:PByte;

begin
  lvOwner := TDiocpExTcpServer(Owner);

  lvStartDataLen := lvOwner.FStartDataLen;
  lvEndDataLen := lvOwner.FEndDataLen;

  if (FCacheBuffer.FEndBytesLength = 0) and (FCacheBuffer.FStartBytesLength = 0)  then
  begin
    SetPackageMaxLength(@FCacheBuffer, lvOwner.FMaxDataLen);
    lvStartData := @lvOwner.FStartData[0];
    lvEndData := @lvOwner.FEndData[0];

    if lvStartDataLen > 0 then
    begin
      SetLength(FCacheBuffer.FStartBytes, lvStartDataLen);
      Move(lvStartData^, FCacheBuffer.FStartBytes[0], lvStartDataLen);
      FCacheBuffer.FStartBytesLength := lvStartDataLen;
    end;
    if lvEndDataLen > 0 then
    begin
      SetLength(FCacheBuffer.FEndBytes, lvEndDataLen);
      Move(lvEndData^, FCacheBuffer.FEndBytes[0], lvEndDataLen);
      FCacheBuffer.FEndBytesLength := lvEndDataLen;
    end;
    ResetPacakge(@FCacheBuffer);
  end;

  lvPtr := PByte(buf);
  for i := 0 to len -1 do
  begin
    r := InputBuffer(@FCacheBuffer, lvPtr^);
    inc(lvPtr);
    if r = 1 then
    begin
{$IFDEF StartEndCut}
      // ȥ��ͷβ
      OnDataAction(@FCacheBuffer.FRawBytes[lvStartDataLen], FCacheBuffer.FRawLength - lvStartDataLen - lvEndDataLen);
{$ELSE}
      OnDataAction(@FCacheBuffer.FRawBytes[0], FCacheBuffer.FRawLength );
{$ENDIF}
      ResetPacakge(@FCacheBuffer);
    end;

  end;




//  pbuf := PAnsiChar(buf);
//  r := len;
//
//
//
//  // �Ѿ���������
//  if (FCacheBuffer.validCount > 0) then
//  begin
//    // ��������
//    if FCacheBuffer.validCount < lvEndDataLen then Exit;
//    
//    // ���ҽ����ַ���   
//    prsearch := SearchPointer(pbuf, len, 0, lvEndData, lvEndDataLen);
//    if prsearch = nil then
//    begin  // û�н�����־
//      FCacheBuffer.AddBuffer(buf, len);
//      Exit;
//    end else
//    begin   // �н�����־�ˣ�ƴ��
//      j := prsearch-pbuf;
//      i := self.FCacheBuffer.validCount;
//      if i > 0 then
//      begin
//        SetLength(FRecvData, i + j);
//        pstr := PAnsiChar(@FRecvData[0]);
//        FCacheBuffer.readBuffer(pstr, i);
//        pstr := pstr + i;
//        Move(pbuf^, pstr^, j);
//        Inc(pbuf, j);
//        Dec(r, j);
//
//        FCacheBuffer.clearBuffer();
//        OnDataAction(@FRecvData[0], i + j);
//      end;
//    end;
//  end;  
//  
//  while r > 0 do
//  begin
//    if lvStartDataLen > 0 then
//    begin
//      prsearch := SearchPointer(pbuf, r, 0, lvStartData, lvStartDataLen);
//      if prsearch = nil then
//      begin  // û�п�ʼ��־buf��Ч
//        Break;
//      end else
//      begin
//        j := prsearch - pbuf;
//        // ��������ʼ��־֮ǰ������
//        Inc(pbuf, j + lvStartDataLen);   // ������ʼ��־
//        Dec(r, j + lvStartDataLen);
//      end;
//    end;
//
//    prsearch := SearchPointer(pbuf, r, 0, lvEndData, lvEndDataLen);//(pbuf, r, 0);
//    if prsearch <> nil then
//    begin
//      j := prsearch - pbuf;
//      if j = 0 then
//      begin  // ֻ��һ��������־
//
//      end else
//      begin
//        SetLength(FRecvData, j);
//        pstr := PAnsiChar(@FRecvData[0]);
//        Move(pbuf^, pstr^, j);
//        Inc(pbuf, j);
//        Dec(r, j);
//        OnDataAction(pstr, j);
//      end;
//      Inc(pbuf, lvEndDataLen);   // ����������־
//      Dec(r, lvEndDataLen); 
//    end else
//    begin     // ʣ�����ݴ���
//      if r > 0 then FCacheBuffer.AddBuffer(pbuf, r);
//      if FCacheBuffer.validCount > lvOwner.FMaxDataLen then
//      begin                      // ����������ݰ���С
//        FCacheBuffer.clearBuffer();
//      end;
//
//      Break;
//    end;
//  end;
end;

procedure TDiocpExContext.OnDataAction(pvData: Pointer; pvDataLen: Integer);
var
  lvOwner:TDiocpExTcpServer;
begin
  try
    lvOwner := TDiocpExTcpServer(Owner);
    lvOwner.DoDataAction(self, pvData, pvDataLen);
  except
    on E:Exception do
    begin
      if Owner <> nil then
      begin
        Owner.LogMessage(strOnResponseException, [SocketHandle, 'OnDataAction', e.Message], '�쳣', lgvError);

        DoOwnerClientContext(-1);
      end else
      begin
        __svrLogger.logMessage(strOnResponseException, [SocketHandle, 'OnDataAction', e.Message], '�쳣', lgvError);
      end;
    end;
  end;
end;

procedure TDiocpExContext.WriteData(pvData: Pointer; pvDataLen: Integer);
var
  j, i, x, r:Integer;
  str:AnsiString;
  pstr, pbuf, prsearch:PAnsiChar;

  lvStartData:Pointer;
  lvStartDataLen:Byte;
  
  lvEndData:Pointer;
  lvEndDataLen:Byte;

  lvOwner:TDiocpExTcpServer;

  lvSendBuffer:array of byte;  
begin
  lvOwner := TDiocpExTcpServer(Owner);
{$IFDEF StartEndCut}
      // ȥ��ͷβ
  lvStartData := @lvOwner.FStartData[0];
  lvStartDataLen := lvOwner.FStartDataLen;
  lvEndData := @lvOwner.FEndData[0];
  lvEndDataLen := lvOwner.FEndDataLen;
  j := lvStartDataLen + pvDataLen + lvEndDataLen;
{$ELSE}
  lvStartDataLen:=0;
  lvEndDataLen:=0;
  j := pvDataLen;
{$ENDIF}



  SetLength(lvSendBuffer, j);
  if lvStartDataLen > 0 then
  begin
    Move(lvStartData^, lvSendBuffer[0], lvStartDataLen);
  end;

  Move(pvData^, lvSendBuffer[lvStartDataLen], pvDataLen);

  if lvEndDataLen > 0 then
  begin
    Move(lvEndData^, lvSendBuffer[lvStartDataLen + pvDatalen], lvEndDataLen);
  end;

  PostWSASendRequest(@lvSendBuffer[0], j);
end;

{ TDiocpExTcpServer }

constructor TDiocpExTcpServer.Create(AOwner: TComponent);
begin
  inherited;
  RegisterContextClass(TDiocpExContext);
  FMaxDataLen := 1024 * 1024 * 10;  // 10M
end;

procedure TDiocpExTcpServer.DoDataAction(pvContext: TDiocpExContext; pvData:
    Pointer; pvDataLen: Integer);
begin
  if Assigned(FOnContextDataAction) then
  begin
    FOnContextDataAction(pvContext, pvData, pvDataLen);
  end;  
end;

procedure TDiocpExTcpServer.SetEnd(pvData:Pointer; pvDataLen:Byte);
begin
  Move(pvData^, FEndData[0], pvDataLen);
  FEndDataLen := pvDataLen;
end;

procedure TDiocpExTcpServer.SetMaxDataLen(pvDataLen:Integer);
begin
  FMaxDataLen := pvDataLen;
  Assert(FMaxDataLen > 0);
end;

procedure TDiocpExTcpServer.SetStart(pvData:Pointer; pvDataLen:Byte);
begin
  Move(pvData^, FStartData[0], pvDataLen);
  FStartDataLen := pvDataLen;
end;

constructor TDiocpStringTcpServer.Create(AOwner: TComponent);
begin
  inherited;
  RegisterContextClass(TDiocpStringContext);
end;

procedure TDiocpStringTcpServer.DoDataAction(pvContext: TDiocpExContext; pvData:
    Pointer; pvDataLen: Integer);
var
  lvStr:String;
begin
  inherited;
  lvStr := ByteBufferToString(PByte(pvData), pvDataLen);
  if Assigned(FOnContextStringAction) then
  begin
    FOnContextStringAction(TDiocpStringContext(pvContext), lvStr);
  end;    
end;

procedure TDiocpStringTcpServer.SetPackEndStr(pvEndStr:AnsiString);
begin
  SetEnd(PAnsiChar(pvEndStr), Length(pvEndStr));
end;

procedure TDiocpStringTcpServer.SetPackStartStr(pvStartStr:AnsiString);
begin
  SetStart(PAnsiChar(pvStartStr), Length(pvStartStr));
end;

procedure TDiocpStringContext.WriteAnsiString(pvData:AnsiString);
begin
  WriteData(PAnsiChar(pvData), Length(pvData));
end;

end.
