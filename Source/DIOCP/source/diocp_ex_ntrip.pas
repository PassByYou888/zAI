(*
  *	 Unit owner: D10.Mofen, delphi iocp framework author
  *         homePage: http://www.Diocp.org
  *	       blog: http://www.cnblogs.com/dksoft

  *   2015-02-22 08:29:43
  *     DIOCP-V5 ����

  *    HttpЭ�鴦��Ԫ
  *    ���д󲿷�˼·������delphi iocp framework�е�iocp.HttpServer
  *

// ��֤
GET /1 HTTP/1.1
Host: 127.0.0.1
Ntrip-Version: Ntrip/2.0
User-Agent: NTRIP NtripClientPOSIX/1.49
Connection: close
Authorization: Basic dXNlcjpwYXNzd29yZA==


*)
unit diocp_ex_ntrip;

interface

/// �������뿪�أ�ֻ�ܿ���һ��
{$DEFINE INNER_IOCP}     // iocp�̴߳����¼�
{.$DEFINE  QDAC_QWorker} // ��qworker���е��ȴ����¼�
{.$DEFINE DIOCP_Task}    // ��diocp_task���е��ȴ����¼�


uses
  Classes, StrUtils, SysUtils, utils_buffer, utils_strings

  {$IFDEF QDAC_QWorker}, qworker{$ENDIF}
  {$IFDEF DIOCP_Task}, diocp_task{$ENDIF}
  , diocp_tcp_server, utils_queues, utils_hashs, diocp_ex_http_common;



const
  HTTPLineBreak = #13#10;

type
  TDiocpNtripState = (hsCompleted, hsRecevingBodyData, hsRequest { �������� }, hsRecvingSource { ����NtripSource���� } );
  TDiocpNtripContextMode = (ncmNtripNone, ncmNtripSource, ncmNtripClient);
  TDiocpNtripResponse = class;
  TDiocpNtripClientContext = class;
  TDiocpNtripServer = class;
  TDiocpNtripRequest = class;

  TOnRequestAcceptEvent = procedure(pvRequest:TDiocpNtripRequest; var vIsNMEA:Boolean) of object;

  TDiocpNtripRequest = class(TObject)
  private
    /// <summary>
    ///   ������Closeʱ�黹�ض����
    /// </summary>
    FDiocpNtripServer:TDiocpNtripServer;

    FDiocpContext: TDiocpNtripClientContext;

    /// ͷ��Ϣ
    FHttpVersion: Word; // 10, 11

    FRequestVersionStr: String;

    FRequestMethod: String;

    FMountPoint: String;

    /// <summary>
    ///  ԭʼ�����е�URL��������(û�о���URLDecode����Ϊ��DecodeRequestHeader��Ҫƴ��RequestURLʱ��ʱ������URLDecode)
    ///  û�о���URLDecode�ǿ��ǵ�����ֵ�б������&�ַ�������DecodeURLParam���ֲ������쳣
    /// </summary>
    FRequestURLParamData: string;


    FRequestParamsList: TStringList; // TODO:���http������StringList

    FContextType: string;
    FContextLength: Int64;
    FKeepAlive: Boolean;
    FRequestAccept: String;
    FRequestAcceptLanguage: string;
    FRequestAcceptEncoding: string;
    FRequestUserAgent: string;
    FRequestAuth: string;
    FRequestHostName: string;
    FRequestHostPort: string;

    FRawHeaderData: TMemoryStream;

    FRequestHeader: TStringList;

    FResponse: TDiocpNtripResponse;
    FSourceRequestPass: String;

    /// <summary>
    ///   ����ʹ���ˣ��黹�ض����
    /// </summary>
    procedure Close;
    /// <summary>
    /// ����Http���������Ϣ
    /// </summary>
    /// <returns>
    /// 1: ��Ч��Http��������
    /// </returns>
    function DecodeRequestHeader: Integer;

    /// <summary>
    /// ���յ���Buffer,д������
    /// </summary>
    procedure WriteRawBuffer(const buffer: Pointer; len: Integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;


    /// <summary>
    ///   ����
    /// </summary>
    procedure Clear;

    property ContextLength: Int64 read FContextLength;


    /// <summary>
    ///   ��ͻ��˽���������
    /// </summary>
    property Connection: TDiocpNtripClientContext read FDiocpContext;

    property HttpVersion: Word read FHttpVersion;
    property RequestAccept: String read FRequestAccept;
    property RequestAcceptEncoding: string read FRequestAcceptEncoding;
    property RequestAcceptLanguage: string read FRequestAcceptLanguage;
    /// <summary>
    ///   �����ͷ��Ϣ
    /// </summary>
    property RequestHeader: TStringList read FRequestHeader;

    /// <summary>
    ///   �ҽڵ�
    /// </summary>
    property MountPoint: String read FMountPoint;

    /// <summary>
    ///   Source���������е�Password
    /// </summary>
    property SourceRequestPass: String read FSourceRequestPass write  FSourceRequestPass;

    /// <summary>
    ///  ��ͷ��Ϣ�ж�ȡ���������������ʽ
    /// </summary>
    property RequestMethod: string read FRequestMethod;

    /// <summary>
    ///   ��ͷ��Ϣ�ж�ȡ�����������IP��ַ
    /// </summary>
    property RequestHostName: string read FRequestHostName;

    /// <summary>
    ///   ��ͷ��Ϣ�ж�ȡ������������˿�
    /// </summary>
    property RequestHostPort: string read FRequestHostPort;

    /// <summary>
    /// Http��Ӧ���󣬻�д����
    /// </summary>
    property Response: TDiocpNtripResponse read FResponse;

    /// <summary>
    ///   ��Url��Post�����еõ��Ĳ�����Ϣ: key = value
    /// </summary>
    property RequestParamsList: TStringList read FRequestParamsList;


    /// <summary>
    ///   ��ȡͷ��Ϣ�е��û�����������Ϣ
    /// </summary>
    /// <returns>
    ///   ��ȡ�ɹ�����true
    /// </returns>
    /// <param name="vUser"> (string) </param>
    /// <param name="vPass"> (string) </param>
    function ExtractBasicAuthenticationInfo(var vUser, vPass:string): Boolean;


    /// <summary>
    ///  �ر�����
    /// </summary>
    procedure CloseContext;

    /// <summary>
    /// �õ�http�������
    /// </summary>
    /// <params>
    /// <param name="ParamsKey">http���������key</param>
    /// </params>
    /// <returns>
    /// 1: http���������ֵ
    /// </returns>
    function GetRequestParam(ParamsKey: string): string;

    /// <summary>
    /// ����POST��GET����
    /// </summary>
    /// <pvParamText>
    /// <param name="pvParamText">Ҫ������ȫ������</param>
    /// </pvParamText>
    procedure ParseParams(pvParamText: string);


  end;

  TDiocpNtripResponse = class(TObject)
  private
    FResponseHeader: string;
    FData: TMemoryStream;
    FDiocpContext : TDiocpNtripClientContext;
  public
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    procedure WriteBuf(pvBuf: Pointer; len: Cardinal);
    procedure WriteString(pvString: string; pvUtf8Convert: Boolean = true);

    /// <summary>
    ///  ����ICY200OK��Ϣ
    /// </summary>
    procedure ICY200OK();

    /// <summary>
    ///   NtripSource��֤ʱ�������Ļظ����ظ��󣬹ر�����
    /// </summary>
    procedure BadPassword();

    /// <summary>
    ///   ����SourceTableOK��Ϣ
    /// </summary>
    procedure SourceTableOK();

    /// <summary>
    ///   ����SourceTableOK��SourceTable����
    /// </summary>
    procedure SourceTableOKAndData(pvSourceTable:AnsiString);

    /// <summary>
    ///   NtripClient��֤ʧ��
    /// </summary>
    procedure Unauthorized();

    /// <summary>
    ///   ��Ч���û���֤��Ϣ
    /// </summary>
    /// <param name="pvMountpoint"> �ҽڵ� </param>
    procedure InvalidPasswordMsg(pvMountpoint: string);


    /// <summary>
    ///   ��ͻ��˽���������
    /// </summary>
    property Connection: TDiocpNtripClientContext read FDiocpContext;

  end;

  /// <summary>
  /// Http �ͻ�������
  /// </summary>
  TDiocpNtripClientContext = class(TIocpClientContext)
  private
    // NtripSource����Ϊת��ʹ��
    FNtripClients:TList;

    // ���ַ�����ʱ��ʱʹ��
    FTempNtripClients:TList;

    // ��������
    FRecvBuffer: TBufferLink;

    FContextMode: TDiocpNtripContextMode;
    FNtripState: TDiocpNtripState;
    FNtripRequest: TDiocpNtripRequest;
    FMountPoint: String;
    FTag: Integer;
    FTagStr: String;

    // ִ���¼�
    procedure DoRequest(pvRequest:TDiocpNtripRequest);

  protected

    /// <summary>
    ///   ���������Request����
    /// </summary>
    procedure OnRequest(pvRequest:TDiocpNtripRequest); virtual;

    procedure OnDisconnected; override;

  public
    constructor Create; override;
    destructor Destroy; override;
  protected
    /// <summary>
    /// �黹������أ�����������
    /// </summary>
    procedure DoCleanUp; override;

    /// <summary>
    /// ���յ��ͻ��˵�HttpЭ������, ���н����TDiocpNtripRequest����ӦHttp����
    /// </summary>
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: Word); override;
  public
    /// <summary>
    ///   ��ӵ�NtripSource�ķַ��б�
    /// </summary>
    procedure AddNtripClient(pvContext:TDiocpNtripClientContext);

    /// <summary>
    ///   �Ƴ���NtripSource�ķַ��б�
    ///   �����ַ�����ʱ����ִ���Ƴ�����(��Ӧ��Context���������mountpoint�����ݣ����߶Ͽ�)
    /// </summary>
    procedure RemoveNtripClient(pvContext:TDiocpNtripClientContext);

    /// <summary>
    ///   �ַ�GNSSData
    /// </summary>
    procedure DispatchGNSSDATA(buf: Pointer; len: Cardinal);

    property ContextMode: TDiocpNtripContextMode read FContextMode write FContextMode;

    property MountPoint: String read FMountPoint write FMountPoint;



    property Tag: Integer read FTag write FTag;

    property TagStr: String read FTagStr write FTagStr;


  end;

{$IFDEF UNICODE}
  /// <summary>
  /// Request�¼�����
  /// </summary>
  TOnDiocpNtripRequestEvent = reference to procedure(pvRequest: TDiocpNtripRequest);

  /// <summary>
  /// ���յ�NtripSource����
  /// </summary>
  TDiocpRecvBufferEvent = reference to procedure(pvContext:TDiocpNtripClientContext; buf: Pointer; len: Cardinal);
{$ELSE}
  /// <summary>
  /// Request�¼�����
  /// </summary>
  TOnDiocpNtripRequestEvent = procedure(pvRequest: TDiocpNtripRequest) of object;

  /// <summary>
  /// ���յ�NtripSource����
  /// </summary>
  TDiocpRecvBufferEvent = procedure(pvContext:TDiocpNtripClientContext;buf: Pointer; len: Cardinal) of object;
{$ENDIF}

  /// <summary>
  /// Http ��������
  /// </summary>
  TDiocpNtripServer = class(TDiocpTcpServer)
  private
    FDebugState: Integer;
    FNtripSourcePassword: String;
    FRequestPool: TBaseQueue;

    /// <summary>
    ///  ���Source�б�
    /// </summary>
    FNtripSources: TDHashTableSafe;

    FOnDiocpNtripRequest: TOnDiocpNtripRequestEvent;
    FOnDiocpRecvNtripSourceBuffer: TDiocpRecvBufferEvent;
    FOnRequestAcceptEvent: TOnRequestAcceptEvent;
    FOnDiocpRecvNtripClientBuffer: TDiocpRecvBufferEvent;

    /// <summary>
    /// ��ӦHttp���� ִ����Ӧ�¼�
    /// </summary>
    procedure DoRequest(pvRequest: TDiocpNtripRequest);

    /// <summary>
    ///   �ӳ��л�ȡһ������
    /// </summary>
    function GetRequest: TDiocpNtripRequest;

    /// <summary>
    ///   ����һ������
    /// </summary>
    procedure GiveBackRequest(pvRequest:TDiocpNtripRequest);

    procedure RemoveNtripSource(pvMountPoint:String; pvContext:TIocpClientContext);

    procedure AddNtripSource(pvMountPoint:String; pvContext:TIocpClientContext);

  public
    constructor Create(AOwner: TComponent); override;

    destructor Destroy; override;

    function GetNtripSourceList(pvStrings:TStrings): Integer; overload;

    function GetNtripSourceList: String; overload;

    



    /// <summary>
    ///   ����mountPoint����NtripSource
    /// </summary>
    function FindNtripSource(pvMountPoint:string):TDiocpNtripClientContext;


    /// <summary>
    ///   ����״̬, 0:������ 1:����
    /// </summary>
    property DebugState: Integer read FDebugState write FDebugState;

    /// <summary>
    ///   NtripSourcePassword, ����NtripSource����ʱ����֤
    /// </summary>
    property NtripSourcePassword: String read FNtripSourcePassword write FNtripSourcePassword;


    /// <summary>
    ///  �������
    /// </summary>
    property OnRequestAcceptEvent: TOnRequestAcceptEvent read FOnRequestAcceptEvent write FOnRequestAcceptEvent;

    /// <summary>
    ///   ���յ�NtripSource����
    /// </summary>
    property OnDiocpRecvNtripSourceBuffer: TDiocpRecvBufferEvent read
        FOnDiocpRecvNtripSourceBuffer write FOnDiocpRecvNtripSourceBuffer;

    /// <summary>
    ///   ���յ�Client Buffer
    /// </summary>
    property OnDiocpRecvNtripClientBuffer: TDiocpRecvBufferEvent read
        FOnDiocpRecvNtripClientBuffer write FOnDiocpRecvNtripClientBuffer;
        
    /// <summary>
    ///   ��ӦHttp�����¼�
    /// </summary>
    property OnDiocpNtripRequest: TOnDiocpNtripRequestEvent read FOnDiocpNtripRequest
        write FOnDiocpNtripRequest; 
  end;

procedure WriteHttpResponse(pvContent: TIocpClientContext; pvResponse: String;
    pvContextType: String = 'text/html');

var
  // double linebreak;
  __HEAD_END :array[0..3] of Byte;



implementation

uses
  utils_base64;

procedure WriteHttpResponse(pvContent: TIocpClientContext; pvResponse: String;
    pvContextType: String = 'text/html');
var
  lvResponse:THttpResponse;
  s:RAWString;
begin
  s := pvResponse;
  //s := StringReplace(s, sLineBreak, '<BR>', [rfReplaceAll]);
  lvResponse := THttpResponse.Create;
  try
    lvResponse.ResponseCode := 200;
    lvResponse.ContentType := pvContextType;
    lvResponse.ContentBuffer.AppendRawStr(s);
    lvResponse.EncodeHeader(lvResponse.ContentBuffer.Length);
    pvContent.PostWSASendRequest(lvResponse.HeaderBuilder.Memory, lvResponse.HeaderBuilder.Length);
    pvContent.PostWSASendRequest(lvResponse.ContentBuffer.Memory, lvResponse.ContentBuffer.Length);
  finally
    lvResponse.Free;
  end;
  // TODO -cMM: WriteHttpResponse default body inserted
end;

procedure TDiocpNtripRequest.Clear;
begin
  FRawHeaderData.Clear;
  FMountPoint := '';
  FSourceRequestPass := '';
  FRequestVersionStr := '';
  FRequestMethod := '';
  FRequestParamsList.Clear;
  FContextLength := 0;
  FResponse.Clear;  
end;

procedure TDiocpNtripRequest.Close;
begin
  if FDiocpNtripServer = nil then exit;
  FDiocpNtripServer.GiveBackRequest(Self);
end;

procedure TDiocpNtripRequest.CloseContext;
begin
  FDiocpContext.PostWSACloseRequest();
end;

function TDiocpNtripRequest.GetRequestParam(ParamsKey: string): string;
var
  lvTemp: string; // ���صĲ���ֵ
  lvParamsCount: Integer; // ��������
  I: Integer;
begin
  Result := '';

  lvTemp := ''; // ���صĲ���ֵĬ��ֵΪ��

  // �õ��ύ�����Ĳ���������
  lvParamsCount := self.FRequestParamsList.Count;

  // �ж��Ƿ����ύ�����Ĳ�������
  if lvParamsCount = 0 then exit;

  // ѭ���Ƚ�ÿһ�������key���Ƿ�͵�ǰ����һ��
  for I := 0 to lvParamsCount - 1 do
  begin 
    if Trim(self.FRequestParamsList.Names[I]) = Trim(ParamsKey) then
    begin
      lvTemp := Trim(self.FRequestParamsList.ValueFromIndex[I]);
      Break;
    end;
  end; 

  Result := lvTemp;
end;

constructor TDiocpNtripRequest.Create;
begin
  inherited Create;
  FRawHeaderData := TMemoryStream.Create();
  FRequestHeader := TStringList.Create();
  FResponse := TDiocpNtripResponse.Create();

  FRequestParamsList := TStringList.Create; // TODO:�������http������StringList
end;

destructor TDiocpNtripRequest.Destroy;
begin
  FreeAndNil(FResponse);
  FRawHeaderData.Free;
  FRequestHeader.Free;
  FreeAndNil(FRequestParamsList); // TODO:�ͷŴ��http������StringList
  inherited Destroy;
end;

function TDiocpNtripRequest.DecodeRequestHeader: Integer;
var
  lvRawString: AnsiString;
  lvMethod: AnsiString;
  lvRequestCmdLine, lvTempStr, lvRemainStr: String;
  p : PChar;
begin
  Result := 1;
  SetLength(lvRawString, FRawHeaderData.Size + 1);
  FillChar(PAnsiChar(lvRawString)^, FRawHeaderData.Size + 1, 0);
  FRawHeaderData.Position := 0;
  FRawHeaderData.Read(PAnsiChar(lvRawString)^, FRawHeaderData.Size);
  FRequestHeader.Text := lvRawString;

  // GET /test?v=abc HTTP/1.1
  // SOURCE letmein /Mountpoint
  lvRequestCmdLine := FRequestHeader[0];
  P := PChar(lvRequestCmdLine);
  FRequestHeader.Delete(0);

  // Method
  lvTempStr := LeftUntil(P, [' ']);
  if lvTempStr = '' then Exit;
  lvTempStr := UpperCase(lvTempStr);

  // �����ո�
  SkipChars(P, [' ']);
  if lvTempStr = 'GET' then
  begin
    FRequestMethod := 'GET';
    
    FMountPoint := LeftUntil(P, [' ']);

    if Length(FMountPoint) > 0 then
    begin
      if PChar(FMountPoint)^ in ['/', '\'] then
      begin
        Delete(FMountPoint, 1, 1);
      end;
    end;


    // �����ո�
    SkipChars(P, [' ']);

    // �����HTTP�汾
    lvTempStr := P;
    FRequestVersionStr := UpperCase(lvTempStr);
  end else
  begin    // SOURCE
    FRequestMethod := 'SOURCE';
    if P^=' ' then
    begin
      FSourceRequestPass := '';
    end else
    begin
      FSourceRequestPass := LeftUntil(P, [' ']);
    end;
    // �����ո�
    SkipChars(P, [' ']);
    if P^='/' then inc(p);
    
    FMountPoint := P;

  end;
end;

function TDiocpNtripRequest.ExtractBasicAuthenticationInfo(var vUser,
    vPass:string): Boolean;
var
  lvAuth, lvValue:string;
  p:PChar;
begin
  Result := False;
  // Authorization: Basic aHVnb2JlbjpodWdvYmVuMTIz
  lvAuth := Trim(StringsValueOfName(FRequestHeader, 'Authorization', [':'], true));
  if lvAuth <> '' then
  begin  // ��֤��Ϣ
    p := PChar(lvAuth);    //Basic aHVnb2JlbjpodWdvYmVuMTIz

    // ����Basic
    SkipUntil(P, [' ']);
    SkipChars(P, [' ']);


    // Base64
    lvValue := P;
    lvValue := Base64Decode(lvValue);

    /// userid:pasword
    P := PChar(lvValue);

    // ȡ�û�ID
    vUser := LeftUntil(P, [':']);
    SkipChars(P, [':']);
    // ȡ����
    vPass := P;

    Result := true;
  end;

end;


/// <summary>
///  ����POST��GET����
/// </summary>
/// <pvParamText>
/// <param name="pvParamText">Ҫ������ȫ������</param>
/// </pvParamText>
procedure TDiocpNtripRequest.ParseParams(pvParamText: string);
begin
  SplitStrings(pvParamText, FRequestParamsList, ['&']);
end;

procedure TDiocpNtripRequest.WriteRawBuffer(const buffer: Pointer; len: Integer);
begin
  FRawHeaderData.WriteBuffer(buffer^, len);
end;

procedure TDiocpNtripResponse.BadPassword;
var
  lvData:AnsiString;
  len: Integer;
begin
  lvData := 'ERROR - Bad Password' + sLineBreak;
  len := Length(lvData);
  FDiocpContext.PostWSASendRequest(PAnsiChar(lvData), len);

end;

procedure TDiocpNtripResponse.Clear;
begin
  FData.Clear;
  FResponseHeader := '';
end;

constructor TDiocpNtripResponse.Create;
begin
  inherited Create;
  FData := TMemoryStream.Create();
end;

destructor TDiocpNtripResponse.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

procedure TDiocpNtripResponse.ICY200OK;
var
  lvData:AnsiString;
  len: Integer;
begin
  lvData := 'ICY 200 OK' + sLineBreak;
  len := Length(lvData);
  FDiocpContext.PostWSASendRequest(PAnsiChar(lvData), len);
end;

procedure TDiocpNtripResponse.InvalidPasswordMsg(pvMountpoint: string);
var
  lvData:AnsiString;
  len: Integer;
begin
  lvData := 'Server: NtripCaster/1.0' + sLineBreak
          + 'WWW-Authenticate: Basic realm="/' +pvMountpoint + '"' + sLineBreak
          + 'Content-Type: text/html' + sLineBreak
          + 'Connection: close' + sLineBreak
          + '<html><head><title>401 Unauthorized</title></head><body bgcolor=black text=white link=blue alink=red>' + sLineBreak
          + '<h1><center>The server does not recognize your privileges to the requested entity stream</center></h1>' + sLineBreak
          + '</body></html>' + sLineBreak;
  len := Length(lvData);
  FDiocpContext.PostWSASendRequest(PAnsiChar(lvData), len);
end;

procedure TDiocpNtripResponse.SourceTableOKAndData(pvSourceTable:AnsiString);
var
  lvData:AnsiString;
  len: Integer;
begin
//SOURCETABLE 200 OK
//Content-Type: text/plain
//Content-Length: n
//CAS;129.217.182.51;80;EUREF;BKG;0;DEU;51.5;7.5;http://igs.ifag.de/index_ntrip_cast.htm
//CAS;62.159.109.248;8080;Trimble GPSNet;Trimble Terrasat;1;DEU;48.03;11.72;http://www.virtualrtk.com
//NET;EUREF;EUREF;B;N;http://www.epncb.oma.be/euref_IP;http://www.epncb.oma.be/euref_IP;http
//ENDSOURCETABLE

  lvData := 'SOURCETABLE 200 OK' + sLineBreak +
            'Content-Type: text/plain' + sLineBreak +
            'Content-Length: ' + IntToStr(length(pvSourceTable)) + sLineBreak + sLineBreak +
            pvSourceTable + sLineBreak +
           'ENDSOURCETABLE' + sLineBreak + sLineBreak;


  len := Length(lvData);
  FDiocpContext.PostWSASendRequest(PAnsiChar(lvData), len);
end;

procedure TDiocpNtripResponse.SourceTableOK;
var
  lvData:AnsiString;
  len: Integer;
begin
  lvData := 'SOURCETABLE 200 OK' + sLineBreak;
  len := Length(lvData);
  FDiocpContext.PostWSASendRequest(PAnsiChar(lvData), len);
end;

procedure TDiocpNtripResponse.Unauthorized;
var
  lvData:AnsiString;
  len: Integer;
begin
  lvData := 'HTTP/1.0 401 Unauthorized' + sLineBreak;
  len := Length(lvData);
  FDiocpContext.PostWSASendRequest(PAnsiChar(lvData), len);
end;

procedure TDiocpNtripResponse.WriteBuf(pvBuf: Pointer; len: Cardinal);
begin
  FData.Write(pvBuf^, len);
end;

procedure TDiocpNtripResponse.WriteString(pvString: string; pvUtf8Convert:
    Boolean = true);
var
  lvRawString: AnsiString;
begin
  if pvUtf8Convert then
  begin     // ����Utf8ת��
    lvRawString := UTF8Encode(pvString);
  end else
  begin
    lvRawString := AnsiString(pvString);
  end;
  FData.WriteBuffer(PAnsiChar(lvRawString)^, Length(lvRawString));
end;

procedure TDiocpNtripClientContext.AddNtripClient(
  pvContext: TDiocpNtripClientContext);
begin
  // ��ǰ�Ƿ�NtripSource
  if FContextMode <> ncmNtripSource then Exit;

  self.Lock;
  try
    FNtripClients.Add(pvContext);
  finally
    self.UnLock;
  end;
end;

constructor TDiocpNtripClientContext.Create;
begin
  inherited Create;
  FRecvBuffer := TBufferLink.Create;
  FNtripClients := TList.Create;
  FTempNtripClients := TList.Create;
end;

destructor TDiocpNtripClientContext.Destroy;
begin
  FNtripClients.Free;
  FTempNtripClients.Free;
  FRecvBuffer.Free;
  inherited Destroy;
end;

procedure TDiocpNtripClientContext.DispatchGNSSDATA(buf: Pointer;
  len: Cardinal);
var
  i:Integer;
  lvContext:TDiocpNtripClientContext;
begin
  FTempNtripClients.Clear;
  // copy����ʱ�б���
  Self.Lock;
  FTempNtripClients.Assign(FNtripClients);
  Self.UnLock;

  for i := 0 to FTempNtripClients.Count -1 do
  begin
    lvContext :=TDiocpNtripClientContext(FTempNtripClients[i]);
    if lvContext.LockContext('�ַ�GNSS����', Self) then
    begin
      try
        if not SameText(lvContext.FMountPoint, self.FMountPoint) then  // ��������Ĺҽڵ�
        begin
          RemoveNtripClient(lvContext);
        end else
        begin
          // �ַ�����
          lvContext.PostWSASendRequest(buf, len);
        end;
      finally
        lvContext.UnLockContext('�ַ�GNSS����', Self);
      end;
    end else
    begin
      RemoveNtripClient(lvContext);
    end;
  end;
end;

procedure TDiocpNtripClientContext.DoCleanUp;
begin
  inherited;
  FTag := 0;
  FTagStr := '';
  FNtripState := hsCompleted;
  FContextMode := ncmNtripNone;
  FMountPoint := '';

  // ����б�
  FNtripClients.Clear;
  if FNtripRequest <> nil then
  begin
    FNtripRequest.Close;
    FNtripRequest := nil;
  end;
end;

procedure TDiocpNtripClientContext.DoRequest(pvRequest: TDiocpNtripRequest);
begin
   OnRequest(pvRequest);
   TDiocpNtripServer(FOwner).DoRequest(pvRequest);
end;


procedure TDiocpNtripClientContext.OnDisconnected;
begin
  if ContextMode = ncmNtripSource then
  begin

    // �Ƴ�
    TDiocpNtripServer(FOwner).RemoveNtripSource(FMountPoint, Self);

//    .FNtripSources.Lock;
//    TDiocpNtripServer(FOwner).FNtripSources.ValueMap[FMountPoint] := nil;
//    TDiocpNtripServer(FOwner).FNtripSources.unLock;
  end;

  inherited;
end;

procedure TDiocpNtripClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
  ErrCode: Word);
var
  lvTmpBuf: PAnsiChar;
  j: Integer;
  lvRemain: Cardinal;
  lvTempRequest: TDiocpNtripRequest;
  lvIsNMEA:Boolean;
  lvBuffer:TBytes;
  
begin
  RecordWorkerStartTick;
  try
    if FNtripRequest = nil then
    begin // ��ɺ����ã����´�����һ����
      FNtripRequest := TDiocpNtripServer(Owner).GetRequest;
      FNtripRequest.FDiocpContext := self;
      FNtripRequest.Response.FDiocpContext := self;
      FNtripRequest.Clear;
      FNtripState := hsRequest;
    end;


    if self.FNtripState = hsRecvingSource then
    begin   // ֱ�ӽ���NtripSource����
      if Assigned(TDiocpNtripServer(FOwner).FOnDiocpRecvNtripSourceBuffer) then
      begin
        TDiocpNtripServer(FOwner).FOnDiocpRecvNtripSourceBuffer(Self, buf, len);
      end;
    end else if FNtripState = hsRecevingBodyData then
    begin
      if Assigned(TDiocpNtripServer(FOwner).FOnDiocpRecvNtripClientBuffer) then
      begin
        TDiocpNtripServer(FOwner).FOnDiocpRecvNtripClientBuffer(Self, buf, len);
      end;
    end else
    begin
      FRecvBuffer.AddBuffer(buf, len);

      j := FRecvBuffer.SearchBuffer(@__HEAD_END[0], 4);
      if j = -1 then
      begin      // û�н��յ�ͷ
        Exit;
      end;

      FNtripRequest.FRawHeaderData.Clear;
      FNtripRequest.FRawHeaderData.SetSize(j + 4);
      /// д�뵽ԭʼͷ�����ڽ���
      FRecvBuffer.readBuffer(FNtripRequest.FRawHeaderData.Memory, j + 4);

      if FNtripRequest.DecodeRequestHeader = 0 then
      begin
        self.RequestDisconnect('��Ч��HttpЭ������', self);
        Exit;
      end;



      // ����Context�Ĺһ���
      Self.FMountPoint := FNtripRequest.FMountPoint;

      if SameText(FNtripRequest.FRequestMethod, 'SOURCE') then
      begin    // NtripSource������֤
        if FNtripRequest.SourceRequestPass <> TDiocpNtripServer(FOwner).FNtripSourcePassword then
        begin
          FNtripRequest.Response.BadPassword;
          self.RequestDisconnect('NtripSource ������Կ ��֤ʧ��', self);
          Exit;
        end else
        begin
          Self.FContextMode := ncmNtripSource;

          // �ı�װ������������ģʽ
          FNtripState := hsRecvingSource;


          TDiocpNtripServer(FOwner).AddNtripSource(FMountPoint, Self);

//          // ��ӵ�NtripSource��Ӧ����
//          TDiocpNtripServer(FOwner).FNtripSources.Lock;
//          TDiocpNtripServer(FOwner).FNtripSources.ValueMap[FMountPoint] := Self;
//          TDiocpNtripServer(FOwner).FNtripSources.unLock;

          // ��Ӧ����
          FNtripRequest.Response.ICY200OK;


          j := FRecvBuffer.validCount;
          if j > 0 then
          begin

            SetLength(lvBuffer, j);
            FRecvBuffer.readBuffer(@lvBuffer[0], j);


            if Assigned(TDiocpNtripServer(FOwner).FOnDiocpRecvNtripSourceBuffer) then
            begin
              TDiocpNtripServer(FOwner).FOnDiocpRecvNtripSourceBuffer(Self, buf, len);
            end;
          end;
        end;
        // �������
        FRecvBuffer.clearBuffer;
      end else
      begin   // client����ģʽ

        FContextMode := ncmNtripClient;

        lvIsNMEA := false;
        if Assigned(TDiocpNtripServer(FOwner).OnRequestAcceptEvent) then
        begin
          TDiocpNtripServer(FOwner).OnRequestAcceptEvent(FNtripRequest, lvIsNMEA);
        end;
        // �����¼�
        DoRequest(FNtripRequest);

        FNtripState := hsRecevingBodyData;

        j := FRecvBuffer.validCount;
        if j > 0 then
        begin

          SetLength(lvBuffer, j);
          FRecvBuffer.readBuffer(@lvBuffer[0], j);

          if Assigned(TDiocpNtripServer(FOwner).FOnDiocpRecvNtripClientBuffer) then
          begin
            TDiocpNtripServer(FOwner).FOnDiocpRecvNtripClientBuffer(Self, @lvBuffer[0], j);
          end;
        end;

        // �������
        FRecvBuffer.clearBuffer;
      end;
    end;
  finally
    RecordWorkerEndTick;
  end;
end;

procedure TDiocpNtripClientContext.OnRequest(pvRequest:TDiocpNtripRequest);
begin

end;

procedure TDiocpNtripClientContext.RemoveNtripClient(
  pvContext: TDiocpNtripClientContext);
begin
  self.Lock;
  try
    FNtripClients.Remove(pvContext);
  finally
    self.UnLock;
  end;
end;

{ TDiocpNtripServer }

constructor TDiocpNtripServer.Create(AOwner: TComponent);
begin
  inherited;
  FRequestPool := TBaseQueue.Create;
  FNtripSources := TDHashTableSafe.Create();

  KeepAlive := false;
  RegisterContextClass(TDiocpNtripClientContext);
end;

destructor TDiocpNtripServer.Destroy;
begin
  SafeStop;
  
  FRequestPool.FreeDataObject;
  FRequestPool.Free;
  
  FNtripSources.Free;
  inherited Destroy;
end;



procedure TDiocpNtripServer.DoRequest(pvRequest: TDiocpNtripRequest);
begin
  if Assigned(FOnDiocpNtripRequest) then
  begin
    FOnDiocpNtripRequest(pvRequest);
  end;
end;

function TDiocpNtripServer.FindNtripSource(
  pvMountPoint: string): TDiocpNtripClientContext;
begin
  FNtripSources.Lock;
  Result := TDiocpNtripClientContext(FNtripSources.ValueMap[pvMountPoint]);
  FNtripSources.unLock();
end;

function TDiocpNtripServer.GetNtripSourceList(pvStrings:TStrings): Integer;
var
  lvList:TList;
  i: Integer;
begin
  lvList := TList.Create;
  try
    FNtripSources.Lock;
    try
      FNtripSources.GetDatas(lvList);

      Result := lvList.Count;
      for i := 0 to lvList.Count - 1 do
      begin
        pvStrings.Add(TDiocpNtripClientContext(lvList[i]).FMountPoint + ',' + IntToStr(IntPtr(lvList[i])));
      end;
    finally
      FNtripSources.unLock();
    end; 
  finally
    lvList.Free;
  end;
end;

function TDiocpNtripServer.GetNtripSourceList: String;
var
  lvStrs:TStrings;
begin
  lvStrs := TStringList.Create;
  try
    GetNtripSourceList(lvStrs);
    Result := lvStrs.Text;
  finally
    lvStrs.Free;
  end;
end;

function TDiocpNtripServer.GetRequest: TDiocpNtripRequest;
begin
  Result := TDiocpNtripRequest(FRequestPool.DeQueue);
  if Result = nil then
  begin
    Result := TDiocpNtripRequest.Create;
  end;
  Result.FDiocpNtripServer := Self;
end;

procedure TDiocpNtripServer.GiveBackRequest(pvRequest: TDiocpNtripRequest);
begin
  FRequestPool.EnQueue(pvRequest);
end;

procedure TDiocpNtripServer.AddNtripSource(pvMountPoint:String;
    pvContext:TIocpClientContext);
var
  lvContext:TIocpClientContext;
  s:string;
begin
  // ��ӵ�NtripSource��Ӧ����
  FNtripSources.Lock;
  try
    lvContext := TIocpClientContext(FNtripSources.ValueMap[pvMountPoint]);
    FNtripSources.ValueMap[pvMountPoint] := pvContext;
  finally
    FNtripSources.unLock;
  end;

  s := Format('+ NtripSource Bind, %s, %d, %s:%d',
    [pvMountPoint, IntPtr(pvContext), pvContext.RemoteAddr, pvContext.RemotePort]);
  AddDebugStrings(s);

  try
    if (lvContext <> nil) and (lvContext <> pvContext) then
    begin
      s := Format('= NtripSource kick out, %s, %d, now:%d',
        [pvMountPoint, IntPtr(lvContext), IntPtr(pvContext)]);
      AddDebugStrings(s);
      lvContext.RequestDisconnect(s);
    end;
  except
  end;
end;

procedure TDiocpNtripServer.RemoveNtripSource(pvMountPoint:String;
    pvContext:TIocpClientContext);
var
  lvRemove:Boolean;
  lvContext:TIocpClientContext;
begin
  FNtripSources.Lock;
  try
    lvContext :=TIocpClientContext(FNtripSources.ValueMap[pvMountPoint]);
    lvRemove := (lvContext = pvContext);
    if lvRemove then
       FNtripSources.ValueMap[pvMountPoint] := nil;
  finally
    FNtripSources.unLock;
  end;

  if FDebugState = 1 then
  begin
    if lvRemove then
    begin
      AddDebugStrings(Format('- NtripSource, %s, %d, %s:%d',
      [pvMountPoint, IntPtr(pvContext), pvContext.RemoteAddr, pvContext.RemotePort]));
    end else
    begin
      AddDebugStrings(Format('= NtripSource remove fail, maybe is kick out, %s, %d, %d',
        [pvMountPoint, IntPtr(pvContext), IntPtr(lvContext)]));
    end;
  end;
end;


initialization
  // double linebreak;
  __HEAD_END[0] := 13;
  __HEAD_END[1] := 10;
  __HEAD_END[2] := 13;
  __HEAD_END[3] := 10;

end.
