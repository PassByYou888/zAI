unit utils_URL;

interface

uses
  utils_strings;


type
  TURL = class(TObject)
  private
    FRawHostStr:String;

    FHost: string;
    FUser: String;
    FPassword: String;
    FParamStr: String;
    FPort: string;
    FProtocol: string;
    FURI: String;
    /// <summary>
    ///   ����·������
    ///   127.0.0.1:9983
    ///   user:password@127.0.0.1:9983/diocp/a.html
    /// </summary>
    procedure InnerParseUrlPath(const pvPath:String);
  public
    /// <summary>
    ///  ����URL
    ///  http://user:password@127.0.0.1:9983/diocp/a.html?qry=abcd&dd=xl
    /// </summary>
    procedure SetURL(pvURL:String);

    /// <summary>
    ///   Э��, http, https, ftp, ws
    /// </summary>
    property Protocol: string read FProtocol write FProtocol;

    /// <summary>
    ///   ������ַ
    /// </summary>
    property Host: string read FHost write FHost;

    /// <summary>
    ///   �˿�
    /// </summary>
    property Port: string read FPort write FPort;



    /// <summary>
    ///   ����
    /// </summary>
    property ParamStr: String read FParamStr write FParamStr;
    property RawHostStr: String read FRawHostStr;

    /// <summary>
    ///   URI����
    /// </summary>
    property URI: String read FURI;
  end;



implementation

{ TURL }

procedure TURL.InnerParseUrlPath(const pvPath: String);
var
  lvP, lvTempP:PChar;
  lvTempStr:String;
begin
  if length(pvPath) = 0 then Exit;

  lvP := PChar(pvPath);
  /// user:password@
  lvTempStr := LeftUntil(lvP, ['@']);

  if lvTempStr <> '' then
  begin  // �����û���������
    lvTempP := PChar(lvTempStr);

    FUser := LeftUntil(lvTempP, [':']);
    if FUser <> '' then
    begin
      SkipChars(lvTempP, [':']);
      FPassword := lvTempP;
    end else
    begin
      // ������
      FUser := lvTempStr;
    end;
    SkipChars(lvP, ['@']);
  end;
  
  /// 127.0.0.1:9983/
  lvTempStr := LeftUntil(lvP, ['/']);
  if lvTempStr = '' then  // û��URI
  begin
    lvTempStr := lvP;
    lvP := nil;
  end;
  FRawHostStr := lvTempStr;
  lvTempP := PChar(lvTempStr);
  FHost := LeftUntil(lvTempP, [':']);
  if FHost <> '' then
  begin
    SkipChars(lvTempP, [':']);
    FPort := lvTempP;
  end else
  begin  // û��ָ��Port
    FHost := lvTempStr;
    FPort := '80';
  end;
  
  if lvP = nil then FURI := '/' else FURI := lvP;          
end;

procedure TURL.SetURL(pvURL: String);
var
  lvPSave, lvPUrl:PChar;
  lvTempStr, lvPath:String;
begin
  FProtocol := '';
  FHost := '';
  FRawHostStr := '';
  FPort := '';
  FPassword := '';
  FUser := '';
  FURI := '';

  lvPUrl := PChar(pvURL);

  if (lvPUrl = nil) or (lvPUrl^ = #0) then Exit;

  // http, ftp... or none
  FProtocol := LeftUntilStr(lvPUrl, '://');
  if FProtocol <> '' then lvPUrl := lvPUrl + 3; // ���� ://

  lvPSave := lvPUrl;  // ����λ��

  ///  ·���Ͳ���
  ///  www.diocp.org/image/xxx.asp
  lvTempStr := LeftUntil(lvPUrl, ['?']);

  // ���û�в���
  if lvTempStr = '' then
  begin
    /// ·������ǩ
    lvTempStr := LeftUntil(lvPUrl, ['#']);
    if lvTempStr <> '' then
    begin
      InnerParseUrlPath(lvTempStr);

      // lvPUrl = ʣ�±�ǩ����
    end else
    begin  // û����ǩ��û�в���,ȫ����·��
      lvTempStr := lvPUrl;
      InnerParseUrlPath(lvTempStr);
    end;
  end else
  begin  // �в���
    InnerParseUrlPath(lvTempStr);

    SkipChars(lvPUrl, ['?']);
    // lvPUrl = ʣ�²�������ǩ����
    FParamStr := lvPUrl;
  end;




  





  


end;

end.
