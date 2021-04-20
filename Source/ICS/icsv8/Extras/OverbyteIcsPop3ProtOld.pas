{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
###############################################################################
                       I M P O R T A N T  R E A D
###############################################################################

This is an old version of the POP3 component which is 99% compatible with
old, legacy code. For Unicode enabled applications, you are urged to use the
new unit which performs much better with Unicode but it may break some existing
code. You are invited to update your own code and use the new unit. If you do
so the required changes in your code depend very much on the compiler version.
With legacy Ansi compiler no or just a few changes are required.

The following public properties of the new version return an AnsiString result
instead of string:
property LastResponse
property MsgUidl
property HeaderKeyword
property HeaderData
property HeaderFrom
property HeaderTo
property HeaderSubject
property HeaderReplyTo
property HeaderInReplyTo
property HeaderMessageId
property HeaderDate
property HeaderReturnPath
property HeaderCc

Signature of event OnResponse changed to:
procedure(Sender: TObject; const Msg : AnsiString);
and Signature of event OnDisplay changed to:
procedure(Sender: TObject; const Msg : String);
so your event handler must be changed accordingly.

If you derived your own components some more changes are required since some
protected methods and fields use AnsiString instead of string as well, so
make sure the signatures of your overriden methods are changed accordingly.
Do not disable string cast warnings and resolve all implicit string cast warnings
in a Unicode enabled application. Useful in order to avoid too many string casts
is to include AnsiStrings.pas in the uses clause as well as
OverbyteIcsLibrary.pas (i.e. for IcsIntToStrA()) .

################################################################################

Author:       Fran�ois PIETTE
Object:       TPop3Cli class implements the POP3 protocol
              (RFC-1225, RFC-1939)
Creation:     03 october 1997
Version:      6.06

EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2010 by Fran�ois PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>
              
              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

Updates:
Sept 09, 1997 Modified TOP to be able to request 0 lines (bug reported by
              damien@jetman.demon.co.uk)
Oct 10, 1997  V1.10. Published ProtocolState property, made TOP command
              complies with RFC-1939 as suggested by damien@jetman.demon.co.uk
              Implemented the UIDL command.
Oct 11, 1997  V1.11 Implemented the APOP command, but not tested because no
              server available to test it.
              Made internal error message look like POP3 error messages (-ERR)
Oct 28, 1997  V1.12 Modified TWSocket to handle line buffer overflow and
              TPop3Client to handle that in GetMultiLine.
Jan 10, 1998  V1.13 Made FWSocket accessible with a read only property. This
              eases DNSLookup without a supplementary TWSocket.
              Added a Port property.
Apr 01, 1998  V1.14 Adapted for BCB V3
May 05, 1998  V1.15 Changed GetMultiLine to correctly handle double dots at
              line start.
Jun 01, 1998  V1.16 Ben Robinson <zeppelin@wwa.com> found that Last did'nt
              update MsgNum and MsgSize.
Aug 05, 1998  V2.00 New asynchronous version.
Sep 19, 1998  V2.01 Corrected WSocketDataAvailable to count for the added
              nul byte at the end of buffer.
Nov 28, 1998  V2.02 Corrected exception triggered using highlevel function
              when connection or DNS lookup failed (for example using Open).
Dec 03, 1998  V2.03 Added SetErrorMessage in WSocketSessionConnected.
Dec 22, 1998  V2.04 Handle exception when connecting (will be triggered when
              an invalid port has been given).
Feb 27, 1999  V2.05 Adde State property.
Mar 07, 1999  V2.06 Made public property Connected.
Aug 20, 1999  V2.07 Revised conditional compilation, adapted for BCB4, set
              compile options same as TWSocket.
Dec 26, 1999  V2.08 Makes OnRequestDone properly called after a QUIT command.
              Special thanks to roger.morton@dial.pipex.com for his work
              about that problem.
Jul 22, 2000  V2.09 Checked for buffer overflow in WSocketDataAvailable
              as suggested by Jeroen Stolting <stolting.em@ilco.nl>
Nov 11, 2000  V2.10 Made ClearErrorMessage public. Cleared ErrorMessage when
              connecting. Thanks to Jeroen Nijk <Nijk.em@ilco.nl> for pointing
              to problem.
Nov 25, 2000  V2.11 Converted MD5 digest to lower case before sending to the
              server. Thanks to Poessler Thomas <Thomas.Poessler@uta.at> who
              found the problem and fix.
Jul 30, 2001  V2.12 Jake Traynham <jake@comm-unity.net> found a problem in
              end of line logic in WSocketDataAvailable when random CR are
              inside a message line. Changed logic to take only CRLF pair as
              end of line.
Aug 18, 2001  V2.13  Angus Robertson <angus@magsys.co.uk> found a problem when
              using the RetrSync and TopSync methods that it's not possible to
              retrieve a body that takes longer than the timeout in
              WaitUntilReady. Timeout has to be reevaluated in TriggerResponse.
Sep 09, 2001  V2.14 Beat Boegli <leeloo999@bluewin.ch> added LocalAddr property
              for multihomed hosts.
Jul 06, 2002  V2.15 Added header decoding for RETR command. Added corresponding
              properties such as HeaderFrom, HeaderTo, HeaderSubject,...
Apr 29, 2003  V2.16 Use continuation lines in header.
              Remove trailing space in header lines (such as subject) values.
              Thanks to Christophe Thiaux <tophet@free.fr> for his help.
              Fixed 'reply-to' (dash was missing).
              Added HeaderInReplyTo property.
May 08, 2003  V2.17 Allow not only TAB but also SPACE for continuation lines.
              Thanks to Christophe Thiaux <tophet@free.fr> for his help.
Sep 15, 2003  V2.18 Added ICSDEF feature to the source code. Thanks to Marco
              van de Voort <marcov@stack.nl> for his help.
Jan 11, 2004  V2.19 "Piotr Hellrayzer Dalek" <enigmatical@interia.pl> added
              AuthLogin, CRAM-MD5 and CRAM-SHA1 authentication.
              Fixed a problem when an invalid hostname was given for connect.
Feb 23, 2004  V2.20 Daniel <freedani@free.fr> added IsServerAPOP method. He
              also cleared FTimeStamp in connect method.
Mar 07, 2004  V2.21 Revised WSocketDataAvailable so that LF alone is allowed
              instead of CRLF pair. This is not RFC compliant but some mail
              server send such lines. It should not hurt any RFC compliant
              server so let's do it... By Holger Lembke.
May 31, 2004  V2.22 Used ICSDEFS.INC the same way as in other units
Aug 23, 2004  V2.23 Use MsgWaitForMultipleObjects in WaitUntilReady to avoid
              consumming 100% CPU while waiting.
Sep 08, 2004  V2.24 Fixed a number of compilation problems when using NOFORMS
              MD5 has been renamed to IcsMD5
Jan 13, 2005  V2.25 In WaitUntilReady, moved MsgWaitForMultipleObjects before
              the call to the message pump and added a few flags for
              MsgWaitForMultipleObjects. This make the loop much efficient.
Feb 21, 2005  V2.26 Fixed WSocketDataAvailable to repeat in case of line
              too long. Thanks to Piotr Hellrayzer Dalek.
May 24, 2005  V2.27 Bjornar Nielsen <bjornar@sentinel.no> added HeaderCc
              property.
Mar 23, 2006  V6.00  New version started from ICS-V5
Dec 29, 2007  V6.01  A.Garrels made WSocketSessionConnected virtual (SSL).
Feb 11, 2008  V6.02  Angus SSL version now supports sync methods
Mar 24, 2008  V6.03 Francois Piette made some changes to prepare code
                    for Unicode.
Jun 28, 2008  V6.04 **Breaking Change** enum items "pop3TlsImplicite", "pop3TlsExplicite"
              renamed to "pop3TlsImplicit", "pop3TlsExplicit"
Dec 21, 2008  V6.05 F.Piette added two string cast in WSocketDataAvailable to
              avoid warning with Delphi 2009.
Oct 08, 2009  V6.06 Faust added NTLM support (untested by the team).

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsPop3Prot;

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

uses
    Messages,
{$IFDEF USEWINDOWS}
    Windows,
{$ELSE}
    WinTypes, WinProcs,
{$ENDIF}
{$IFNDEF NOFORMS}
    Forms,
{$ENDIF}
    SysUtils, Classes, OverbyteIcsMimeUtils,
    OverbyteIcsWSocket, OverbyteIcsWndControl,OverbyteIcsWinSock,
{$IFDEF DELPHI5_UP}
    OverbyteIcsSha1,          { SHA1 code require Delphi 5 and up  }
{$ENDIF}
    OverbyteIcsMD5,
    OverbyteIcsNtlmMsgs;      {V6.06}

const
    Pop3CliVersion     = 606;
    CopyRight : String = ' POP3 component (c) 1997-2010 F. Piette V6.06 ';
{$IFDEF VER80}
    { Delphi 1 has a 255 characters string limitation }
    POP3_RCV_BUF_SIZE = 255;
{$ELSE}
    POP3_RCV_BUF_SIZE = 4096;
{$ENDIF}

type
    Pop3Exception = class(Exception);
    TPop3Display  = procedure(Sender: TObject; Msg : String) of object;
    TPop3ProtocolState  = (pop3Disconnected,  pop3WaitingUser,
                           pop3WaitingPass,   pop3Transaction);
    TPop3State    = (pop3Ready,         pop3DnsLookup,       pop3Connecting,
                     pop3Connected,     pop3InternalReady,
                     pop3WaitingBanner, pop3WaitingResponse, pop3Abort);
    TPop3Request  = (pop3Connect, pop3User, pop3Pass, pop3RPop, pop3Quit,
                     pop3Stat,    pop3List, pop3Retr, pop3Top,  pop3Dele,
                     pop3Noop,    pop3Last, pop3RSet, pop3Uidl, pop3APop,
                     pop3Open,    pop3Auth,
                 {$IFDEF USE_SSL}
                     pop3StartTls,
                 {$ENDIF}    
                     pop3Custom);
    TPop3Fct      = (pop3FctNone, pop3FctConnect, pop3FctUser, pop3FctPass,
                     pop3FctRPop, pop3FctQuit,    pop3FctAPop, pop3FctStat,
                     pop3FctList, pop3FctUidl,    pop3FctRetr, pop3FctTop,
                     pop3FctDele, pop3FctNoop,    pop3FctRSet, pop3FctLast
                 {$IFDEF USE_SSL}
                     , pop3FctStartTls
                 {$ENDIF}
                     );
    TPop3AuthType = (popAuthNone,    popAuthLogin,
                     popAuthCramMD5, popAuthCramSHA1, popAuthNTLM); {V6.06}

    TPop3FctSet   = set of TPop3Fct;
    TPop3NextProc = procedure of object;
    TPop3RequestDone        = procedure(Sender    : TObject;
                                        RqType    : TPop3Request;
                                        Error     : Word) of object;
    TPop3Method   = function : boolean of object;

    TCustomPop3Cli = class(TIcsWndControl)
    private
        FWSocket            : TWSocket;
        FState              : TPop3State;
        FNextProtocolState  : TPop3ProtocolState;
        FProtocolState      : TPop3ProtocolState;
        FConnected          : Boolean;
        FRequestType        : TPop3Request;
        FRequestDoneFlag    : Boolean;
        FReceiveLen         : Integer;
        FRequestResult      : Integer;
        FStatusCode         : Integer;
        FReceiveBuffer      : array [0..POP3_RCV_BUF_SIZE - 1] of AnsiChar;
        FNext               : TPop3NextProc;
        FWhenConnected      : TPop3NextProc;
        FFctSet             : TPop3FctSet;
        FFctPrv             : TPop3Fct;
        FHighLevelResult    : Integer;
        FHighLevelFlag      : Boolean;
        FNextRequest        : TPop3NextProc;
        FLastResponseSave   : String;
        FStatusCodeSave     : Integer;
        FRestartFlag        : Boolean;
        FDoneAsync          : TPop3NextProc;
        FMultiLineLine      : TNotifyEvent;
        FMultiLineEnd       : TNotifyEvent;
        FMultiLineProcess   : TNotifyEvent;
        FHost               : String;
        FLocalAddr          : String; {bb}
        FPort               : String;
        FUserName           : String;
        FPassWord           : String;
        FAuthType           : TPop3AuthType;{HLX}
        FLastResponse       : String;
        FErrorMessage       : String;
        FTimeStamp          : String;
        FMsgCount           : Integer;
        FMsgSize            : Integer;
        FMsgNum             : Integer;
        FMsgUidl            : String;
        FMsgLines           : Integer;
        FTag                : LongInt;
        // FWaitingOnQuit      : Boolean; Bad code, see TriggerRequestDone
        FHeaderPart         : Boolean;
        FHeaderKeyword      : String;
        FHeaderData         : String;
        FHeaderFrom         : String;
        FHeaderTo           : String;
        FHeaderSubject      : String;
        FHeaderReplyTo      : String;
        FHeaderInReplyTo    : String;
        FHeaderMessageId    : String;
        FHeaderDate         : String;
        FHeaderReturnPath   : String;
        FHeaderCc           : String;
        FMsg_WM_POP3_REQUEST_DONE : UINT;

        FOnDisplay          : TPop3Display;
        FOnMessageBegin     : TNotifyEvent;
        FOnMessageEnd       : TNotifyEvent;
        FOnMessageLine      : TNotifyEvent;
        FOnListBegin        : TNotifyEvent;
        FOnListEnd          : TNotifyEvent;
        FOnListLine         : TNotifyEvent;
        FOnUidlBegin        : TNotifyEvent;
        FOnUidlEnd          : TNotifyEvent;
        FOnUidlLine         : TNotifyEvent;
        FOnStateChange      : TNotifyEvent;
        FOnRequestDone      : TPop3RequestDone;
        FOnResponse         : TPop3Display;
        FOnSessionConnected : TSessionConnected;
        FOnSessionClosed    : TSessionClosed;
        FOnHeaderEnd        : TNotifyEvent;
    protected
        procedure   ExecAsync(RqType      : TPop3Request;
                              Cmd         : String;
                              NextState   : TPop3ProtocolState;
                              DoneAsync   : TPop3NextProc);
        procedure   NextExecAsync;
        procedure   StartTransaction(OpCode      : String;
                                     Params      : String;
                                     RqType      : TPop3Request;
                                     NextState   : TPop3ProtocolState;
                                     DoneTrans   : TPop3NextProc);
        procedure   StartMultiLine(aOnBegin : TNotifyEvent;
                                   aOnLine  : TNotifyEvent;
                                   aOnEnd   : TNotifyEvent;
                                   aProcess : TNotifyEvent);
        procedure   CreateCtrlSocket; virtual;
        procedure   GetALine;
        procedure   StatDone;
        procedure   ListAllDone;
        procedure   ListSingleDone;
        procedure   UidlAllDone;
        procedure   UidlSingleDone;
        procedure   RetrDone;
        procedure   LastDone;
        procedure   WndProc(var MsgRec: TMessage); override;
        procedure   AllocateMsgHandlers; override;
        procedure   FreeMsgHandlers; override;
        function    MsgHandlersCount: Integer; override;
        procedure   HandleBackGroundException(E: Exception); override;
        procedure   WMPop3RequestDone(var msg: TMessage); virtual;
        procedure   WSocketDnsLookupDone(Sender: TObject; Error: Word);
        procedure   WSocketSessionConnected(Sender: TObject; Error: Word); virtual;
        procedure   WSocketDataAvailable(Sender: TObject; Error: Word);
        procedure   WSocketSessionClosed(Sender : TObject; Error : WORD);
        procedure   DisplayLastResponse;
        procedure   TriggerDisplay(Msg : String);
        procedure   TriggerSessionConnected(Error : Word); virtual;
        procedure   TriggerSessionClosed(Error : Word); virtual;
        procedure   TriggerResponse(Msg : String); virtual;
        procedure   TriggerStateChange; virtual;
        procedure   TriggerRequestDone(Error: Word); virtual;
        function    OkResponse : Boolean;
        procedure   StateChange(NewState : TPop3State);
        procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
        procedure   SetErrorMessage;
        procedure   Display(Msg : String);
        procedure   SendCommand(Cmd : String);
        function    ExtractNumbers(var N1 : Integer; var N2 : Integer) : Boolean;
        function    ExtractUidl(var N1 : Integer; var N2 : String) : Boolean;
        procedure   ProcessUidl(Sender : TObject);
        procedure   ProcessList(Sender : TObject);
        procedure   CheckReady;
        procedure   DoHighLevelAsync; virtual;
        procedure   AuthLoginNext;
        procedure   AuthLoginPass;
{$IFDEF DELPHI5_UP}
        procedure   AuthCramSha1;
{$ENDIF}
        procedure   AuthCramMd5;
        procedure   AuthNextNtlm;        {V6.06}
        procedure   AuthNextNtlmNext;    {V6.06}
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;
        procedure   Connect; virtual;
        procedure   Open; virtual;
        procedure   Auth; virtual; {HLX}
        procedure   User; virtual;
        procedure   Pass; virtual;
        procedure   RPop; virtual;
        procedure   APop; virtual;
        procedure   Quit; virtual;
        procedure   Stat; virtual;
        procedure   List; virtual;
        procedure   Retr; virtual;
        procedure   Top;  virtual;
        procedure   Dele; virtual;
        procedure   Noop; virtual;
        procedure   Last; virtual;
        procedure   RSet; virtual;
        procedure   Uidl; virtual;
        procedure   Abort; virtual;
        function    IsServerAPOP: Boolean;
        procedure   ClearErrorMessage;
        procedure   HighLevelAsync(RqType : TPop3Request; Fcts : TPop3FctSet);

        property CtrlSocket    : TWSocket            read  FWSocket;
        property Host          : String              read  FHost
                                                     write FHost;
        property LocalAddr     : String              read  FLocalAddr   {bb}
                                                     write FLocalAddr;  {bb}
        property Port          : String              read  FPort
                                                     write FPort;
        property UserName      : String              read  FUserName
                                                     write FUserName;
        property PassWord      : String              read  FPassWord
                                                     write FPassWord;
        property AuthType      : TPop3AuthType       read  FAuthType
                                                     write FAuthType; {HLX}
        property ErrorMessage  : String              read  FErrorMessage;
        property LastResponse  : String              read  FLastResponse;
        property State         : TPop3State          read  FState;
        property Connected     : Boolean             read  FConnected;
        property ProtocolState : TPop3ProtocolState  read  FProtocolState;
        {:Updated by the Stat method with the number of
          messages in the maildrop }
        property MsgCount : Integer                  read  FMsgCount;
        {:Updated by the Stat method with the total size
          in byte for the messages in the maildrop }
        property MsgSize : Integer                   read  FMsgSize;
        {:This is the number of lines to display in the TOP command
          Set to zero if you wants the default value }
        property MsgLines : Integer                  read  FMsgLines
                                                     write FMsgLines;
        {:This is the message number which must be returned by the Retr
          method. It is also updated by the Last method }
        property MsgNum : Integer                    read  FMsgNum
                                                     write FMsgNum;
        property MsgUidl : String                    read  FMsgUidl;
        {:The following properties are decoded by RETR command }
        property HeaderKeyword      : String         read  FHeaderKeyword;
        property HeaderData         : String         read  FHeaderData;
        property HeaderFrom         : String         read  FHeaderFrom;
        property HeaderTo           : String         read  FHeaderTo;
        property HeaderSubject      : String         read  FHeaderSubject;
        property HeaderReplyTo      : String         read  FHeaderReplyTo;
        property HeaderInReplyTo    : String         read  FHeaderInReplyTo;
        property HeaderMessageId    : String         read  FHeaderMessageId;
        property HeaderDate         : String         read  FHeaderDate;
        property HeaderReturnPath   : String         read  FHeaderReturnPath;
        property HeaderCc           : String         read  FHeaderCc;
        {:General purpose property, not used by component }
        property Tag : LongInt                       read  FTag
                                                     write FTag;
        property OnDisplay : TPop3Display            read  FOnDisplay
                                                     write FOnDisplay;
        property OnMessageBegin : TNotifyEvent       read  FOnMessageBegin
                                                     write FOnMessageBegin;
        property OnMessageEnd : TNotifyEvent         read  FOnMessageEnd
                                                     write FOnMessageEnd;
        property OnMessageLine : TNotifyEvent        read  FOnMessageLine
                                                     write FOnMessageLine;
        property OnListBegin : TNotifyEvent          read  FOnListBegin
                                                     write FOnListBegin;
        property OnListEnd : TNotifyEvent            read  FOnListEnd
                                                     write FOnListEnd;
        property OnListLine : TNotifyEvent           read  FOnListLine
                                                     write FOnListLine;
        property OnUidlBegin : TNotifyEvent          read  FOnUidlBegin
                                                     write FOnUidlBegin;
        property OnUidlEnd : TNotifyEvent            read  FOnUidlEnd
                                                     write FOnUidlEnd;
        property OnUidlLine : TNotifyEvent           read  FOnUidlLine
                                                     write FOnUidlLine;
        property OnHeaderEnd : TNotifyEvent          read  FOnHeaderEnd
                                                     write FOnHeaderEnd;
        property OnStateChange : TNotifyEvent        read  FOnStateChange
                                                     write FOnStateChange;
        property OnRequestDone : TPop3RequestDone    read  FOnRequestDone
                                                     write FOnRequestDone;
        property OnResponse: TPop3Display            read  FOnResponse
                                                     write FOnResponse;
        property OnSessionConnected : TSessionConnected
                                                     read  FOnSessionConnected
                                                     write FOnSessionConnected;
        property OnSessionClosed : TSessionClosed
                                                     read  FOnSessionClosed
                                                     write FOnSessionClosed;
    end;

    TPop3Cli = class(TCustomPop3Cli)
    published
        property Host;
        property LocalAddr; {bb}
        property Port;
        property UserName;
        property PassWord;
        property AuthType;
        property ErrorMessage;
        property LastResponse;
        property ProtocolState;
        property MsgCount;
        property MsgSize;
        property MsgLines;
        property MsgNum;
        property MsgUidl;
        property Tag;
        property OnDisplay;
        property OnMessageBegin;
        property OnMessageEnd;
        property OnMessageLine;
        property OnListBegin;
        property OnListEnd;
        property OnListLine;
        property OnUidlBegin;
        property OnUidlEnd;
        property OnUidlLine;
        property OnHeaderEnd;
        property OnStateChange;
        property OnRequestDone;
        property OnResponse;
        property OnSessionConnected;
        property OnSessionClosed;
    end;


    { TSyncPop3Cli add synchronous functions. You should avoid using this   }
    { component because synchronous function, apart from being easy, result }
    { in lower performance programs.                                        }

    TSyncPop3Cli = class(TPop3Cli)
    protected
        FTimeout       : Integer;                 { Given in seconds }
        FTimeStop      : LongInt;                 { Milli-seconds    }
        FMultiThreaded : Boolean;
        function WaitUntilReady : Boolean; virtual;
        function Synchronize(Proc : TPop3NextProc) : Boolean;
        procedure TriggerResponse(Msg : String); override;   { Angus }
    public
        constructor Create(AOwner : TComponent); override;
        function    ConnectSync  : Boolean; virtual;
        function    OpenSync     : Boolean; virtual;
        function    UserSync     : Boolean; virtual;
        function    PassSync     : Boolean; virtual;
        function    RPopSync     : Boolean; virtual;
        function    APopSync     : Boolean; virtual;
        function    QuitSync     : Boolean; virtual;
        function    StatSync     : Boolean; virtual;
        function    ListSync     : Boolean; virtual;
        function    RetrSync     : Boolean; virtual;
        function    TopSync      : Boolean; virtual;
        function    DeleSync     : Boolean; virtual;
        function    NoopSync     : Boolean; virtual;
        function    LastSync     : Boolean; virtual;
        function    RSetSync     : Boolean; virtual;
        function    UidlSync     : Boolean; virtual;
        function    AbortSync    : Boolean; virtual;
    published
        property Timeout : Integer       read  FTimeout
                                         write FTimeout;
        property MultiThreaded : Boolean read  FMultiThreaded
                                         write FMultiThreaded;
    end;

{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Creation:     30 December 2007
Version:      1.0 Beta
Updated:      11 February 2008
Description:  A component adding SSL/TLS support to TPop3Cli.
              This unit contains the interface section of the component.
              It is included in Pop3Prot.pas unit when USE_SSL is defined.
              Make use of OpenSSL (http://www.openssl.org).
              Make use of freeware TWSocket component from ICS
              Source code available from http://www.overbyte.be

Features:     - Explicit SSL/TLS thru command STLS (abbrevation of STARTTLS).
              - Implicit SSL/TLS connections on a dedicated port (default 995).

Testing:      A nice made test server represents the free SSL/TLS capable
              mail server 'Hamster' including full Delphi source,
              available at: http://tglsoft.de/
              Also googlemail/gmail POP3 servers support explicite TLS on port 995.

ToDo:

Updates:
11 Feb 2008  V6.02  Angus SSL version now supports sync methods

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
    TPop3SslType  = (pop3TlsNone, pop3TlsImplicit, pop3TlsExplicit);
    TSslPop3Cli = class(TSyncPop3Cli)
    protected
        FSslType            : TPop3SslType; 
        FOnSslHandshakeDone : TSslHandshakeDoneEvent;
        function    GetSslAcceptableHosts: TStrings;
        procedure   SetSslAcceptableHosts(const Value: TStrings);
        function    GetSslCliGetSession: TSslCliGetSession;
        function    GetSslCliNewSession: TSslCliNewSession;
        function    GetSslVerifyPeer: TSslVerifyPeerEvent;
        procedure   SetSslCliGetSession(const Value: TSslCliGetSession);
        procedure   SetSslCliNewSession(const Value: TSslCliNewSession);
        procedure   SetSslVerifyPeer(const Value: TSslVerifyPeerEvent);
        function    GetSslCliCertRequest: TSslCliCertRequest;
        procedure   SetSslCliCertRequest(const Value: TSslCliCertRequest);
        procedure   TransferSslHandshakeDone(Sender         : TObject;
                                             ErrCode        : Word;
                                             PeerCert       : TX509Base;
                                             var Disconnect : Boolean); virtual;
        procedure   WSocketSessionConnected(Sender : TObject;
                                            Error  : Word); override;
        procedure   TlsNext;
        procedure   DoHighLevelAsync; override;
        procedure   CreateCtrlSocket; override;
        procedure   SetSslContext(Value: TSslContext);
        function    GetSslContext: TSslContext;
    public
        procedure   Stls; virtual;
        //procedure   Abort; override;
        procedure   Open; override;
        procedure   Connect; override;
        property    SslAcceptableHosts   : TStrings
                                                  read  GetSslAcceptableHosts
                                                  write SetSslAcceptableHosts;
    published
        property    SslType            : TPop3SslType
                                                  read  FSslType
                                                  write FSslType;
        property    SslContext         : TSslContext
                                                  read  GetSslContext
                                                  write SetSslContext;
        property    OnSslVerifyPeer    : TSslVerifyPeerEvent
                                                  read  GetSslVerifyPeer
                                                  write SetSslVerifyPeer;
        property    OnSslCliGetSession : TSslCliGetSession
                                                  read  GetSslCliGetSession
                                                  write SetSslCliGetSession;
        property    OnSslCliNewSession : TSslCliNewSession
                                                  read  GetSslCliNewSession
                                                  write SetSslCliNewSession;
        property    OnSslHandshakeDone : TSslHandshakeDoneEvent
                                                  read  FOnSslHandshakeDone
                                                  write FOnSslHandshakeDone;
        property    OnSslCliCertRequest : TSslCliCertRequest
                                                  read  GetSslCliCertRequest
                                                  write SetSslCliCertRequest;
    end;
{$ENDIF} // USE_SSL

implementation

const
    HexDigits : array [0..15] of char = ('0','1','2','3','4','5','6','7','8',
                                         '9','a','b','c','d','e','f');

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
procedure SetLength(var S: string; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function RTrim(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LTrim(Str : String) : String;
var
    I : Integer;
begin
    if Str[1] <> ' ' then             { Petite optimisation: pas d'espace   }
        Result := Str
    else begin
        I := 1;
        while (i <= Length(Str)) and (Str[I] = ' ') do
            I := I + 1;
        Result := Copy(Str, I, Length(Str) - I + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := LTrim(Rtrim(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsDigit(Ch : Char) : Boolean;
begin
    Result := (Ch >= '0') and (Ch <= '9');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsSpace(Ch : Char) : Boolean;
begin
    Result := (Ch = ' ') or (Ch = #9);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsSpaceOrCRLF(Ch : Char) : Boolean;
begin
    Result := (Ch = ' ') or (Ch = #9) or (Ch = #10) or (Ch = #13);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function stpblk(PValue : PChar) : PChar;
begin
    Result := PValue;
    while IsSpaceOrCRLF(Result^) do
        Inc(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function atoi(PValue : PChar) : Integer;
begin
    Result := 0;
    PValue := stpblk(PValue);
    while IsDigit(PValue^) do begin
        Result := Result * 10 + ord(PValue^) - ord('0');
        Inc(PValue);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF NOFORMS}
{ This function is a callback function. It means that it is called by       }
{ windows. This is the very low level message handler procedure setup to    }
{ handle the message sent by windows (winsock) to handle messages.          }
function POP3CliWindowProc(
    ahWnd   : HWND;
    auMsg   : Integer;
    awParam : WPARAM;
    alParam : LPARAM): Integer; stdcall;
var
    Obj    : TObject;
    MsgRec : TMessage;
begin
    { At window creation asked windows to store a pointer to our object     }
    Obj := TObject(GetWindowLong(ahWnd, 0));

    { If the pointer doesn't represent a TCustomFtpCli, just call the default procedure}
    if not (Obj is TPOP3Cli) then
        Result := DefWindowProc(ahWnd, auMsg, awParam, alParam)
    else begin
        { Delphi use a TMessage type to pass parameter to his own kind of   }
        { windows procedure. So we are doing the same...                    }
        MsgRec.Msg    := auMsg;
        MsgRec.wParam := awParam;
        MsgRec.lParam := alParam;
        { May be a try/except around next line is needed. Not sure ! }
        TPOP3Cli(Obj).WndProc(MsgRec);
        Result := MsgRec.Result;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.CreateCtrlSocket;
begin
    FWSocket := TWSocket.Create(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomPop3Cli.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    AllocateHWnd;
    //FWSocket                 := TWSocket.Create(nil);
    CreateCtrlSocket;
    FWSocket.OnSessionClosed := WSocketSessionClosed;
    FProtocolState           := pop3Disconnected;
    FState                   := pop3Ready;
    FLocalAddr               := '0.0.0.0'; {bb}    
    FPort                    := 'pop3';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomPop3Cli.Destroy;
begin
    if Assigned(FWSocket) then begin
        FWSocket.Destroy;
        FWSocket := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomPop3Cli.MsgHandlersCount : Integer;
begin
    Result := 1 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_POP3_REQUEST_DONE := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then begin
        FWndHandler.UnregisterMessage(FMsg_WM_POP3_REQUEST_DONE);
    end;
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.WndProc(var MsgRec: TMessage);
begin
    try
         with MsgRec do begin
             if Msg = FMsg_WM_POP3_REQUEST_DONE then
                 WMPop3RequestDone(MsgRec)
             else
                 inherited WndProc(MsgRec);
        end;
    except
        on E:Exception do
            HandleBackGroundException(E);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ All exceptions *MUST* be handled. If an exception is not handled, the     }
{ application will be shut down !                                           }
procedure TCustomPop3Cli.HandleBackGroundException(E: Exception);
var
    CanAbort : Boolean;
begin
    CanAbort := TRUE;
    { First call the error event handler, if any }
    if Assigned(FOnBgException) then begin
        try
            FOnBgException(Self, E, CanAbort);
        except
        end;
    end;
    { Then abort the component }
    if CanAbort then begin
        try
            Abort;
        except
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.WMPop3RequestDone(var msg: TMessage);
begin
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Self, FRequestType, Msg.LParam);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if AComponent = FWSocket then
            FWSocket := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.WSocketDnsLookupDone(Sender: TObject; Error: Word);
begin
    if Error <> 0 then begin
        FLastResponse  := '-ERR ' + WSocketErrorDesc(Error) +
                          ' (Winsock error #' + IntToStr(Error) + ')';
        FStatusCode    := 500;
        FRequestResult := Error;      { V2.02 }
        SetErrorMessage;
        TriggerRequestDone(Error);
    end
    else begin
        FWSocket.Addr               := FWSocket.DnsResult;
        FWSocket.Proto              := 'tcp';
        FWSocket.Port               := FPort;
        FWSocket.LocalAddr          := FLocalAddr; {bb}
        FWSocket.OnSessionConnected := WSocketSessionConnected;
        FWSocket.OnDataAvailable    := WSocketDataAvailable;
        StateChange(pop3Connecting);
        try
            FWSocket.Connect;
        except
            on E:Exception do begin
                FLastResponse  := '-ERR ' + E.ClassName + ': ' + E.Message;
                FStatusCode    := 500;
                FRequestResult := FStatusCode;
                SetErrorMessage;
                TriggerRequestDone(FStatusCode);
            end;
        end
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.WSocketSessionConnected(Sender: TObject; Error: Word);
begin
    { Do not trigger the client SessionConnected from here. We must wait }
    { to have received the server banner.                                }
    if Error <> 0 then begin
        FLastResponse  := '-ERR ' + WSocketErrorDesc(Error) +
                          ' (Winsock error #' + IntToStr(Error) + ')';
        FStatusCode    := 500;
        FConnected     := FALSE;
        FRequestResult := Error;      { V2.02 }
        SetErrorMessage;              { V2.03 }
        TriggerRequestDone(Error);
        FWSocket.Close;
        StateChange(pop3Ready);
    end
    else begin
        FConnected := TRUE;
        StateChange(pop3WaitingBanner);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.WSocketSessionClosed(Sender : TObject; Error : WORD);
begin
    FConnected := FALSE;
    TriggerSessionClosed(Error);
    TriggerRequestDone(WSAEINTR);
    FProtocolState := pop3Disconnected;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.WSocketDataAvailable(Sender: TObject; Error: Word);
var
    Len       : Integer;
    I, J      : Integer;
    Remaining : Integer;
begin
    repeat
        { Compute remaining space in our buffer. Preserve 3 bytes for CR/LF   }
        { and nul terminating byte.                                           }
        Remaining := SizeOf(FReceiveBuffer) - FReceiveLen - 3;
        if Remaining <= 0 then begin
            { Received message has a line longer than our buffer. This is not }
            { acceptable ! We will add a CR/LF to enable processing, but this }
            { will ALTER received message and could cause strange results.    }
            { May be it is better to raise an exception ?                     }
            FReceiveBuffer[SizeOf(FReceiveBuffer) - 3] := #13;
            FReceiveBuffer[SizeOf(FReceiveBuffer) - 2] := #10;
            Len := 2;
        end
        else begin
            Len := FWSocket.Receive(@FReceiveBuffer[FReceiveLen], Remaining);
            if Len <= 0 then
                Exit;
        end;

        FReceiveBuffer[FReceiveLen + Len] := #0;
        FReceiveLen := FReceiveLen + Len;

        while FReceiveLen > 0 do begin
            { Search LF. We can't use Pos because it stops at first #0 }
            I := 1;
            while (I < FReceiveLen) and                        {07/03/2004}
                  (FReceiveBuffer[I] <> #10) do
                    Inc(I);
            if I >= FReceiveLen then
                break;                   { LF not found }

            { Found a LF. Extract data from buffer, ignoring CR if any }
            if (I > 0) and (FReceiveBuffer[I - 1] = #13) then      {07/03/2004}
                FLastResponse := String(Copy(FReceiveBuffer, 1, I - 1))
            else
                FLastResponse := String(Copy(FReceiveBuffer, 1, I));

            TriggerResponse(FLastResponse);

    {$IFDEF DUMP}
            FDumpBuf := '>|';
            FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
            FDumpStream.WriteBuffer(FLastResponse[1], Length(FLastResponse));
            FDumpBuf := '|' + #13#10;
            FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
    {$ENDIF}
    {$IFDEF VER80}
            { Add a nul byte at the end of string for Delphi 1 }
            FLastResponse[Length(FLastResponse) + 1] := #0;
    {$ENDIF}
            FReceiveLen := FReceiveLen - I - 1;
            if FReceiveLen > 0 then
                Move(FReceiveBuffer[I + 1], FReceiveBuffer[0], FReceiveLen + 1);

            if FState = pop3WaitingBanner then begin
                DisplayLastResponse;
                if not OkResponse then begin
                    SetErrorMessage;
                    FRequestResult := FStatusCode;
                    FWSocket.Close;
                    Exit;
                end;
                I := Pos('<', FLastResponse);
                J := Pos('>', Copy(FLastResponse, I, Length(FLastREsponse)));
                if (I > 0) and (J > 0) then
                    FTimeStamp := Copy(FLastResponse, I, J);

                FProtocolState := pop3WaitingUser;
                StateChange(pop3Connected);
                TriggerSessionConnected(Error);

                if Assigned(FWhenConnected) then
                    FWhenConnected
                else begin
                    TriggerRequestDone(0);
                end;
            end
            else if FState = pop3WaitingResponse then begin
                if Assigned(FNext) then
                    FNext
                else
                    raise Pop3Exception.Create('Program error: FNext is nil');
            end
            else begin
                { Unexpected data received }
                DisplayLastResponse;
            end;
        end;
    until (Remaining > 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.TriggerResponse(Msg : String);
begin
    if Assigned(FOnResponse) then
        FOnResponse(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.TriggerStateChange;
begin
    if Assigned(FOnStateChange) then
        FOnStateChange(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.TriggerRequestDone(Error: Word);
begin
    { Special processing for Quit (Roger Morton 24-12-99) }
(*  This code is bad!
    if FRequestType = pop3Quit then begin
        if FWaitingOnQuit then
            { When the second RqDone arrives (from WSocketSessionClosed),   }
            { treat it as a normal event by setting a zero Error code       }
            Error := 0
        else begin
            { When the first RqDone arrives, set the FWaitingOnQuit flag so }
            { we're ready to handle a second RqDone.                        }
            { Take no other action (in particular, we don't advise the user }
            { that the first RqDone has happened)                           }
            FWaitingOnQuit := True;
            Exit;
        end;
        { Fall down here for all normal RqDone, and after the second RqDone }
        { following a Quit                                                  }
        FWaitingOnQuit := False;
    end;
*)
    if not FRequestDoneFlag then begin
        FRequestDoneFlag := TRUE;
        if Assigned(FNextRequest) then begin
            if FState <> pop3Abort then
                StateChange(pop3InternalReady);
            FNextRequest;
        end
        else begin
            StateChange(pop3Ready);
            { Restore the lastresponse saved before quit command }
            if FHighLevelFlag and (FStatusCodeSave >= 0) then begin
                 FLastResponse := FLastResponseSave;
                 FStatusCode   := FStatusCodeSave;
            end;
            FHighLevelFlag := FALSE;
            PostMessage(Handle, FMsg_WM_POP3_REQUEST_DONE, 0, Error);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.TriggerDisplay(Msg : String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.TriggerSessionConnected(Error : Word);
begin
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.TriggerSessionClosed(Error : Word);
begin
    if Assigned(FOnSessionClosed) then
        FOnSessionClosed(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.DoHighLevelAsync;
begin
{$IFDEF TRACE} TriggerDisplay('! HighLevelAsync ' + IntToStr(FRequestResult)); {$ENDIF}
    if FState = pop3Abort then begin
        {$IFDEF TRACE} TriggerDisplay('! Abort detected'); {$ENDIF}
        FFctSet := [];
        FHighLevelResult := 426;
        FErrorMessage    := '426 Operation aborted.';
    end;

    FNextRequest := DoHighLevelAsync;

    if FRequestResult <> 0 then begin
        { Previous command had errors }
        FHighLevelResult := FRequestResult;
        if (FFctPrv = pop3FctQuit) or (not (pop3FctQuit in FFctSet)) then
            FFctSet := []
        else
            FFctSet := [pop3FctQuit];
    end;

    if pop3FctConnect in FFctSet then begin
        FFctPrv := pop3FctConnect;
        FFctSet := FFctSet - [FFctPrv];
        Connect;
        Exit;
    end;

    if pop3FctUser in FFctSet then begin
        FFctPrv := pop3FctUser;
        FFctSet := FFctSet - [FFctPrv];
        User;
        Exit;
    end;

    if pop3FctPass in FFctSet then begin
        FFctPrv := pop3FctPass;
        FFctSet := FFctSet - [FFctPrv];
        Pass;
        Exit;
    end;

    if pop3FctRPop in FFctSet then begin
        FFctPrv := pop3FctRPop;
        FFctSet := FFctSet - [FFctPrv];
        RPop;
        Exit;
    end;

    if pop3FctDele in FFctSet then begin
        FFctPrv := pop3FctDele;
        FFctSet := FFctSet - [FFctPrv];
        Dele;
        Exit;
    end;

    if pop3FctNoop in FFctSet then begin
        FFctPrv := pop3FctNoop;
        FFctSet := FFctSet - [FFctPrv];
        Noop;
        Exit;
    end;

    if pop3FctList in FFctSet then begin
        FFctPrv := pop3FctList;
        FFctSet := FFctSet - [FFctPrv];
        List;
        Exit;
    end;

    if pop3FctRSet in FFctSet then begin
        FFctPrv := pop3FctRSet;
        FFctSet := FFctSet - [FFctPrv];
        RSet;
        Exit;
    end;

    if pop3FctAPop in FFctSet then begin
        FFctPrv := pop3FctAPop;
        FFctSet := FFctSet - [FFctPrv];
        APop;
        Exit;
    end;

    if pop3FctRetr in FFctSet then begin
        FFctPrv := pop3FctRetr;
        FFctSet := FFctSet - [FFctPrv];
        Retr;
        Exit;
    end;

    if pop3FctTop in FFctSet then begin
        FFctPrv := pop3FctTop;
        FFctSet := FFctSet - [FFctPrv];
        Top;
        Exit;
    end;

    if pop3FctStat in FFctSet then begin
        FFctPrv := pop3FctStat;
        FFctSet := FFctSet - [FFctPrv];
        Stat;
        Exit;
    end;

    if pop3FctUidl in FFctSet then begin
        FFctPrv := pop3FctUidl;
        FFctSet := FFctSet - [FFctPrv];
        Uidl;
        Exit;
    end;

    if pop3FctLast in FFctSet then begin
        FFctPrv := pop3FctLast;
        FFctSet := FFctSet - [FFctPrv];
        Last;
        Exit;
    end;

    if pop3FctQuit in FFctSet then begin
        FFctPrv := pop3FctQuit;
        FFctSet := FFctSet - [FFctPrv];
        Quit;
        Exit;
    end;

    {$IFDEF TRACE} TriggerDisplay('! HighLevelAsync done'); {$ENDIF}
    FFctSet          := [];
    FNextRequest     := nil;
    FRequestDoneFlag := FALSE;
    TriggerRequestDone(FHighLevelResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.HighLevelAsync(
    RqType : Tpop3Request; Fcts : Tpop3FctSet);
begin
    if FConnected and (pop3FctConnect in Fcts) then
        raise pop3Exception.Create('pop3 component already connected');
    CheckReady;
    FLastResponseSave := FLastResponse;
    FStatusCodeSave   := -1;
    FRequestType      := RqType;
    FRequestResult    := 0;
    FFctSet           := Fcts;
    FFctPrv           := pop3FctNone;
    FHighLevelResult  := 0;
    FHighLevelFlag    := TRUE;
    FLastResponse     := '';
    FRestartFlag      := FALSE;
    ClearErrorMessage;
    DoHighLevelAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.ProcessUidl(Sender : TObject);
begin
    ExtractUidl(FMsgNum, FMsgUidl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.ProcessList(Sender : TObject);
begin
    ExtractNumbers(FMsgNum, FMsgSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomPop3Cli.ExtractUidl(var N1 : Integer; var N2 : String) : Boolean;
var
    p : PChar;
begin
    Result := FALSE;
    N1     := 0;
    N2     := '';

{$IFDEF VER80}
    { Delphi 1 do not automatically nul terminate strings }
    FLastResponse := FLastResponse + #0;
{$ENDIF}

    { Search for first digit in response }
    p := @FLastResponse[1];
    while (p^ <> #0) and (not IsDigit(p^)) do
        Inc(p);
    if p^ = #0 then { Invalid response, need a number }
        Exit;

    { Convert first number }
    N1 := atoi(p);

    { Search end of number }
    while (p^ <> #0) and IsDigit(p^) do
        Inc(p);

    { Search Uidl }
    while (p^ = ' ') do
        Inc(p);

    { Copy UIDL }
    while (p^ <> #0) and ((p^ >= #33) and (p^ <= #126)) do begin
        N2 := N2 + p^;
        Inc(p);
    end;

    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomPop3Cli.ExtractNumbers(var N1 : Integer; var N2 : Integer) : Boolean;
var
    p : PChar;
begin
    Result := FALSE;

{$IFDEF VER80}
    { Delphi 1 do not automatically nul terminate strings }
    FLastResponse := FLastResponse + #0;
{$ENDIF}

    { Search for first digit in response }
    p := @FLastResponse[1];
    while (p^ <> #0) and (not IsDigit(p^)) do
        Inc(p);
    if p^ = #0 then begin
        { Invalid response, need a number }
        N1 := 0;
        N2 := 0;
        Exit;
    end;

    { Convert first number }
    N1 := atoi(p);

    { Search end of number }
    while (p^ <> #0) and IsDigit(p^) do
        Inc(p);

    { Search next number }
    p := stpblk(p);

    if p^ = #0 then begin
        { Invalid response, need a number }
        N1 := 0;
        N2 := 0;
        Exit;
    end;

    N2     := atoi(p);
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.SendCommand(Cmd : String);
begin
    Display('> ' + Cmd);
    { Application.ProcessMessages;        //FP Should it be removed ?! }
    FWSocket.SendStr(Cmd + #13 + #10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomPop3Cli.OkResponse : Boolean;
begin
    Result := ((Length(FLastResponse) > 0) and (FLastResponse[1] = '+'));
    if Result then
        FStatusCode := 0
    else
        FStatusCode := 500;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Display(Msg : String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.ClearErrorMessage;
begin
    FErrorMessage := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.SetErrorMessage;
begin
    if FErrorMessage = '' then
        FErrorMessage := FLastResponse;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.CheckReady;
begin
    if not (FState in [pop3Ready, pop3InternalReady]) then
        raise pop3Exception.Create('POP3 component not ready');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.StateChange(NewState : TPop3State);
begin
    if FState <> NewState then begin
        FState := NewState;
        TriggerStateChange;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.DisplayLastResponse;
begin
     TriggerDisplay('< ' + FLastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.ExecAsync(
    RqType      : TPop3Request;
    Cmd         : String;             { Command to execute                     }
    NextState   : TPop3ProtocolState; { Next protocol state in case of success }
    DoneAsync   : TPop3NextProc);     { What to do when done                   }
begin
    CheckReady;

    if not FConnected then
        raise Pop3Exception.Create('POP3 component not connected');

    if not FHighLevelFlag then
        FRequestType := RqType;

    FRequestDoneFlag   := FALSE;
    FNext              := NextExecAsync;
    FNextProtocolState := NextState;
    FDoneAsync         := DoneAsync;
    StateChange(pop3WaitingResponse);
    SendCommand(Cmd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.NextExecAsync;
begin
    DisplayLastResponse;

    if not OkResponse then begin
        FRequestResult := FStatusCode;
        SetErrorMessage;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    FRequestResult := 0;
    FProtocolState := FNextProtocolState;

    if Assigned(FDoneAsync) then
        FDoneAsync
    else
        TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.AuthLoginNext; {HLX}
begin
    if FRequestResult <> 0 then begin
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    FState := pop3InternalReady;
    ExecAsync(pop3User, Base64Encode(FUsername), pop3WaitingUser, AuthLoginPass);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.AuthLoginPass; {HLX}
begin
    if FRequestResult <> 0 then begin
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    FState := pop3InternalReady;
    ExecAsync(pop3Pass, Base64Encode(FPassword), pop3Transaction, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF DELPHI5_UP}
procedure TCustomPop3Cli.AuthCramSha1; {HLX}
var
    Challenge  : String;
    Response   : String;
    Digest     : SHA1Digest;
    count      : Integer;
begin
    if FRequestResult <> 0 then begin
        TriggerRequestDone(FRequestResult);
        Exit;
    end;
    if Length(FLastResponse) < 3 then begin
        FLastResponse := '-ERR Malformed SHA1 Challenge: ' + FLastResponse;
        SetErrorMessage;
        TriggerRequestDone(500);
        Exit;
    end;
    Challenge := Copy(FLastResponse, 3, Length(FLastResponse) - 2);
    Challenge := Base64Decode(Challenge);
    HMAC_SHA1(Challenge[1], Length(Challenge),
              FPassword[1], Length(FPassword), Digest);
    Response := FUsername + ' ';
    for Count := 0 to SHA1HashSize-1 do begin
        Response := Response + HexDigits[((Byte(Digest[Count]) and $F0) shr 4)];
        Response := Response + HexDigits[(Byte(Digest[Count]) and $0F)];
    end;
    FState := pop3InternalReady;
    ExecAsync(pop3Pass, Base64Encode(Response), pop3Transaction, nil);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.AuthCramMd5; {HLX}
var
    Challenge  : String;
    Response   : String;
    MD5Digest  : TMD5Digest;
    MD5Context : TMD5Context;
    Count      : Integer;
    IPAD       : array [0..63] of Byte;
    OPAD       : array [0..63] of Byte;
begin
    if FRequestResult <> 0 then begin
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    if Length(FLastResponse) < 3 then begin
        FLastResponse := '500 Malformed MD5 Challenge: ' + FLastResponse;
        SetErrorMessage;
        TriggerRequestDone(500);
        Exit;
    end;

    Challenge := Copy(FLastResponse, 3, Length(FLastResponse) - 2);
    Challenge := Base64Decode(Challenge);

    {See RFC2104 }
    for Count := 0 to 63 do begin
        if (Count + 1) <= Length(FPassword) then begin
            IPAD[Count] := Byte(FPassword[Count+1]) xor $36;
            OPAD[Count] := Byte(FPassword[Count+1]) xor $5C;
        end
        else begin
            IPAD[Count] := 0 xor $36;
            OPAD[Count] := 0 xor $5C;
        end;
    end;

    MD5Init(MD5Context);
    MD5Update(MD5Context, IPAD, 64);
    MD5UpdateBuffer(MD5Context, @Challenge[1], Length(Challenge));
    MD5Final(MD5Digest, MD5Context);
    MD5Init(MD5Context);
    MD5Update(MD5Context, OPAD, 64);
    MD5Update(MD5Context, MD5Digest, 16);
    MD5Final(MD5Digest, MD5Context);

    Response := FUsername;
    Response := Response + ' ';
    for Count := 0 to 15 do begin
        Response := Response + HexDigits[((Byte(MD5Digest[Count]) and $F0) shr 4)];
        Response := Response + HexDigits[(Byte(MD5Digest[Count]) and $0F)];
    end;

    FState := pop3InternalReady;
    ExecAsync(pop3Pass, Base64encode(Response), pop3Transaction, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.AuthNextNtlm; {V6.06}
begin
    if FRequestResult <> 0 then begin
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    FState := pop3InternalReady;
    ExecAsync(pop3Auth, NtlmGetMessage1('', ''), pop3Transaction, AuthNextNtlmNext);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.AuthNextNtlmNext; {V6.06}
var
    NtlmMsg2Info : TNTLM_Msg2_Info;
    NtlmMsg3     : String;
begin
    if FRequestResult <> 0 then begin
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    if (Length(FLastResponse) < 3) then begin
        FLastResponse := '-ERR Malformed NtlmMsg2: ' + FLastResponse;
        SetErrorMessage;
        TriggerRequestDone(500);
        Exit;
    end;

    NtlmMsg2Info := NtlmGetMessage2(Copy(FLastResponse, 3, Length(FLastResponse) - 2));
    NtlmMsg3 := NtlmGetMessage3('',
                                '',  // the Host param seems to be ignored
                                FUsername, FPassword,
                                NtlmMsg2Info.Challenge);
    FState := pop3InternalReady;
    ExecAsync(pop3Auth, NtlmMsg3, pop3Transaction, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Auth; {HLX}
begin
    if FProtocolState > pop3WaitingUser then begin
        FErrorMessage := '-ERR AUTH command invalid now';
        Display(FErrorMessage);
        raise Pop3Exception.Create(FErrorMessage);
    end;

    case FAuthType of
    popAuthLogin    : ExecAsync(pop3Auth, 'AUTH LOGIN',
                                pop3WaitingUser, AuthLoginNext);
    popAuthCramSHA1 :
{$IFDEF DELPHI5_UP}
                      ExecAsync(pop3Auth, 'AUTH CRAM-SHA1',
                                pop3WaitingUser, AuthCramSha1);
{$ELSE}
                      raise Exception.Create('SHA1 require Delphi 5 or later');
{$ENDIF}
    popAuthCramMD5  : ExecAsync(pop3Auth, 'AUTH CRAM-MD5',
                                pop3WaitingUser, AuthCramMD5);
    popAuthNTLM     : ExecAsync(pop3Auth, 'AUTH NTLM',
                                pop3WaitingUser, AuthNextNtlm);    {V6.06}
    else
        User;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.User;
begin
    if FProtocolState > pop3WaitingUser then begin
        FErrorMessage := '-ERR USER command invalid now';
        Display(FErrorMessage);
        raise Pop3Exception.Create(FErrorMessage);
    end;

    FFctPrv := pop3FctUser;
    ExecAsync(pop3User, 'USER ' + Trim(FUserName), pop3WaitingPass, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Connect;
begin
    CheckReady;
    if FConnected then
        raise Pop3Exception.Create('POP3 component already connected');

    if not FHighLevelFlag then
        FRequestType  := pop3Connect;

    FRequestDoneFlag  := FALSE;
    FReceiveLen       := 0;
    FRequestResult    := 0;
    FTimeStamp        := '';
    ClearErrorMessage;
    FWSocket.OnDataSent      := nil;
    FWSocket.OnDnsLookupDone := WSocketDnsLookupDone;
    StateChange(pop3DnsLookup);
    try
        FWSocket.DnsLookup(FHost);
    except
        Abort;
        raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Abort;
begin
    StateChange(pop3Abort);
    FWSocket.CancelDnsLookup;
    FWSocket.Abort;
    StateChange(pop3Ready);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Pass;
begin
    if FProtocolState > pop3WaitingPass then begin
        FErrorMessage := '-ERR PASS command invalid now';
        Display(FErrorMessage);
        raise Pop3Exception.Create(FErrorMessage);
    end;

    FFctPrv := pop3FctPass;
    ExecAsync(pop3Pass, 'PASS ' + Trim(FPassWord), pop3Transaction, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.RPop;
begin
    if FProtocolState > pop3WaitingPass then begin
        FErrorMessage := '-ERR RPOP command invalid now';
        Display(FErrorMessage);
        raise Pop3Exception.Create(FErrorMessage);
    end;

    FFctPrv := pop3FctRPop;
    ExecAsync(pop3RPop, 'RPOP ' + Trim(FPassWord), pop3Transaction, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.APop;
begin
    if FProtocolState <> pop3WaitingUser then begin
        FErrorMessage := '-ERR APOP command invalid now';
        Display(FErrorMessage);
        raise Pop3Exception.Create(FErrorMessage);
    end;

    if FTimeStamp = '' then begin
        FErrorMessage := '-ERR Server does not support APOP (no timestamp)'; {HLX ;PP}
        Display(FErrorMessage);
        raise Pop3Exception.Create(FErrorMessage);
    end;

    FFctPrv := pop3FctAPop;
    ExecAsync(pop3APop, 'APOP ' + Trim(FUserName) + ' ' +
                        LowerCase(StrMD5(FTimeStamp + FPassWord)),
                        pop3Transaction, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Quit;
begin
    CheckReady;
    FFctPrv := pop3FctQuit;
    if not FConnected then begin
        { We are not connected, it's ok... }
        FRequestType     := pop3Quit;
        FRequestDoneFlag := FALSE;
        TriggerRequestDone(0);
        Exit;
    end;
    ExecAsync(pop3Quit, 'QUIT', pop3Disconnected, nil); { Should I force a FWSocket.Close }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Stat;
begin
    FFctPrv := pop3FctStat;
    StartTransaction('STAT', '', pop3Stat, pop3Transaction, StatDone);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.StatDone;
begin
    ExtractNumbers(FMsgCount, FMsgSize);
    TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.List;
begin
    FFctPrv := pop3FctList;
    if FMsgNum <= 0 then
        { Scan LIST command (all messages) }
        StartTransaction('LIST', '', pop3List, pop3Transaction, ListAllDone)
    else
        { Single message LIST command }
        StartTransaction('LIST', IntToStr(FMsgNum), pop3List,
                         pop3Transaction, ListSingleDone);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Uidl;
begin
    FFctPrv := pop3FctUidl;
    if FMsgNum <= 0 then
        { UIDL command (all messages) }
        StartTransaction('UIDL', '', pop3Uidl, pop3Transaction, UidlAllDone)
    else
        { Single message UIDL command }
        StartTransaction('UIDL', IntToStr(FMsgNum), pop3Uidl,
                         pop3Transaction, UidlSingleDone);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.UidlAllDone;
begin
    StartMultiLine(FOnUidlBegin, FOnUidlLine, FOnUidlEnd, ProcessUidl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.UidlSingleDone;
begin
    ExtractUidl(FMsgNum, FMsgUidl);
    TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.ListSingleDone;
begin
    ExtractNumbers(FMsgNum, FMsgSize);
    TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.ListAllDone;
begin
    StartMultiLine(FOnListBegin, FOnListLine, FOnListEnd, ProcessList);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Retr;
begin
    FFctPrv := pop3FctRetr;
    StartTransaction('RETR',   IntToStr(FMsgNum),
                     pop3Retr, pop3Transaction, RetrDone);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Top;
begin
    if FMsgLines < 0 then
        raise Pop3Exception.Create('Invalid MsgLines for TOP command');
    FFctPrv := pop3FctTop;
    StartTransaction('TOP', IntToStr(FMsgNum) + ' ' + IntToStr(FMsgLines),
                     pop3Top, pop3Transaction, RetrDone);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.RetrDone;
begin
    StartMultiLine(FOnMessageBegin, FOnMessageLine, FOnMessageEnd, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Dele;
begin
    FFctPrv := pop3FctDele;
    StartTransaction('DELE', IntToStr(FMsgNum),
                     pop3Dele, pop3Transaction, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Noop;
begin
    FFctPrv := pop3FctNoop;
    StartTransaction('NOOP', '', pop3Noop, pop3Transaction, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.RSet;
begin
    FFctPrv := pop3FctRSet;
    StartTransaction('RSET', '', pop3RSet, pop3Transaction, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Last;
begin
    FFctPrv := pop3FctLast;
    StartTransaction('LAST', '', pop3Last, pop3Transaction, LastDone);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.LastDone;
begin
    ExtractNumbers(FMsgNum, FMsgSize);
    TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.Open;
begin
    HighLevelAsync(pop3Open, [pop3FctConnect, pop3FctUser, pop3FctPass]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.StartTransaction(
    OpCode      : String;
    Params      : String;
    RqType      : TPop3Request;
    NextState   : TPop3ProtocolState;  { Next protocol state in case of success}
    DoneTrans   : TPop3NextProc);      { What to do when done                  }
var
    Cmd : String;
begin
    if FProtocolState <> pop3Transaction then begin
        FErrorMessage := '-ERR ' + OpCode + ' command invalid now';
        Display(FErrorMessage);
        raise Pop3Exception.Create(FErrorMessage);
    end;

    FHeaderPart         := TRUE;
    FHeaderKeyword      := '';
    FHeaderData         := '';
    FHeaderFrom         := '';
    FHeaderTo           := '';
    FHeaderSubject      := '';
    FHeaderReplyTo      := '';
    FHeaderInReplyTo    := '';
    FHeaderMessageId    := '';
    FHeaderReturnPath   := '';
    FHeaderDate         := '';
    FHeaderCc           := '';

    Cmd := OpCode;
    if Params <> '' then
        Cmd := Cmd + ' ' + Params;
    ExecAsync(RqType, Cmd, NextState, DoneTrans);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.StartMultiLine(
    aOnBegin : TNotifyEvent;
    aOnLine  : TNotifyEvent;
    aOnEnd   : TNotifyEvent;
    aProcess : TNotifyEvent);
begin
    FMultiLineLine    := aOnLine;
    FMultiLineEnd     := aOnEnd;
    FMultiLineProcess := aProcess;

    { Let the application know that the message is beginning }
    if Assigned(aOnBegin) then
        aOnBegin(Self);

    FNext := GetALine;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomPop3Cli.GetALine;
var
    I : Integer;
begin
    { Check if we are still connected }
    if not FConnected then begin
        FErrorMessage  := '-ERR Disconneced unexpectedly';
        FRequestResult := 500;
        Display(FErrorMessage);
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    { Check if end of message }
    if FLastResponse = '.' then begin
        { Let the application know that the message is finished }
        if Assigned(FMultiLineEnd) then
            FMultiLineEnd(Self);
        FLastResponse := '';
        FNext         := nil;
        TriggerRequestDone(0);
        Exit;
    end;

    { Check if message contains end-of-message mark }
    if (Length(FLastResponse) >= 2) and
       (FLastResponse[1] = '.') and (FLastResponse[2] = '.') then
        { Remove byte-stuff }
        FLastResponse := Copy(FLastResponse, 2, Length(FLastResponse));

    { Check if end of header }
    if FHeaderPart then begin
        if FLastResponse = '' then begin
            { Last header line }
            FHeaderPart := FALSE;
            if Assigned(FOnHeaderEnd) then
                FOnHeaderEnd(Self);
        end
        else if IsSpace(FLastResponse[1]) then begin
            { Continuation line }
            { Remove TAB or space. Should I insert a CRLF ? }
            FHeaderData := Copy(FLastResponse, 2, 10000);
            if FHeaderKeyword      = 'from'        then
                FHeaderFrom       := FHeaderFrom + FHeaderData
            else if FHeaderKeyword = 'to'          then
                FHeaderTo         := FHeaderTo + FHeaderData
            else if FHeaderKeyword = 'subject'     then
                FHeaderSubject    := FHeaderSubject + FHeaderData
            else if FHeaderKeyword = 'date'        then
                FHeaderDate       := FHeaderDate + FHeaderData
            else if FHeaderKeyword = 'message-id'  then
                FHeaderMessageId  := FHeaderMessageId + FHeaderData
            else if FHeaderKeyword = 'reply-to'     then
                FHeaderReplyTo    := FHeaderReplyTo + FHeaderData
            else if FHeaderKeyword = 'in-reply-to'     then
                FHeaderInReplyTo  := FHeaderInReplyTo + FHeaderData
            else if FHeaderKeyword = 'return-path' then
                FHeaderReturnPath := FHeaderReturnPath + FHeaderData
            else if (FHeaderKeyword = 'Cc') or (FHeaderKeyword = 'CC') or (FHeaderKeyword = 'cc') then
                FHeaderCc := FHeaderCc + FHeaderData;
        end
        else begin
            I := Pos(':', FLastResponse);
            if I > 0 then begin
                FHeaderKeyword := LowerCase(Trim(Copy(FLastResponse, 1, I - 1)));
                { Remove space commonly found at start of header data }
                if (I < Length(FLastResponse)) and
                   (FLastResponse[I + 1] = ' ') then
                    Inc(I);
                FHeaderData    := Copy(FLastResponse, I + 1, 10000);
                if FHeaderKeyword      = 'from'        then
                    FHeaderFrom       := FHeaderData
                else if FHeaderKeyword = 'to'          then
                    FHeaderTo         := FHeaderData
                else if FHeaderKeyword = 'subject'     then
                    FHeaderSubject    := FHeaderData
                else if FHeaderKeyword = 'date'        then
                    FHeaderDate       := FHeaderData
                else if FHeaderKeyword = 'message-id'  then
                    FHeaderMessageId  := FHeaderData
                else if FHeaderKeyword = 'reply-to'     then
                    FHeaderReplyTo    := FHeaderData
                else if FHeaderKeyword = 'in-reply-to'     then
                    FHeaderInReplyTo  := FHeaderData
                else if FHeaderKeyword = 'return-path' then
                    FHeaderReturnPath := FHeaderData
                else if (FHeaderKeyword = 'Cc') or (FHeaderKeyword = 'CC') or (FHeaderKeyword = 'cc') then
                    FHeaderCc := FHeaderCc + FHeaderData;
            end;
        end;
    end;

    { Additional process }
    if Assigned(FMultiLineProcess) then
        FMultiLineProcess(Self);

    { Let the application process the message line }
    if Assigned(FMultiLineLine) then
        FMultiLineLine(Self);

    { To process next line }
    FNext := GetALine;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomPop3Cli.IsServerAPOP: boolean;
begin
    Result := (FTimeStamp <> '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSyncPop3Cli.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FTimeout := 15;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSyncPop3Cli.TriggerResponse(Msg : String);  { angus }
begin
    inherited TriggerResponse(Msg);
    { Evaluate the timeout period again }
    if FTimeout > 0 then
        FTimeStop := Integer(GetTickCount) + FTimeout * 1000;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.WaitUntilReady : Boolean;
var
    DummyHandle     : THandle;
begin
    Result := TRUE;           { Suppose success }
    FTimeStop := Integer(GetTickCount) + FTimeout * 1000;
    while TRUE do begin
        if FState = pop3Ready then begin
            { Back to ready state, the command is finiched }
            Result := (FRequestResult = 0);
            break;
        end;

        if  {$IFNDEF NOFORMS}Application.Terminated or{$ENDIF}
            ((FTimeout > 0) and (Integer(GetTickCount) > FTimeStop)) then begin
            { Application is terminated or timeout occured }
            inherited Abort;
            FErrorMessage := '426 Timeout';
            FStatusCode   := 426;
            Result        := FALSE; { Command failed }
            break;
        end;
{$IFNDEF VER80}
        { Do not use 100% CPU }
        DummyHandle := INVALID_HANDLE_VALUE;                                           //FP
        MsgWaitForMultipleObjects(0, DummyHandle, FALSE, 1000,
                                  QS_ALLINPUT + QS_ALLEVENTS +
                                  QS_KEY + QS_MOUSE);
{$ENDIF}
{$IFDEF NOFORMS}
        FWSocket.ProcessMessages;
{$ELSE}
{$IFNDEF VER80}
        if FMultiThreaded then
            FWSocket.ProcessMessages
        else
{$ENDIF}
            Application.ProcessMessages;
{$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.Synchronize(Proc : TPop3NextProc) : Boolean;
begin
    try
        Proc;
        Result := WaitUntilReady;
    except
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.ConnectSync : Boolean;
begin
    Result := Synchronize(Connect);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.OpenSync : Boolean;
begin
    Result := Synchronize(Open);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.UserSync : Boolean;
begin
    Result := Synchronize(User);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.PassSync : Boolean;
begin
    Result := Synchronize(Pass);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.RetrSync : Boolean;
begin
    Result := Synchronize(Retr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.DeleSync : Boolean;
begin
    Result := Synchronize(Dele);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.UidlSync : Boolean;
begin
    Result := Synchronize(Uidl);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.LastSync : Boolean;
begin
    Result := Synchronize(Last);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.RPopSync : Boolean;
begin
    Result := Synchronize(RPop);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.TopSync : Boolean;
begin
    Result := Synchronize(Top);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.ListSync : Boolean;
begin
    Result := Synchronize(List);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.StatSync : Boolean;
begin
    Result := Synchronize(Stat);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.QuitSync : Boolean;
begin
    Result := Synchronize(Quit);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.APopSync : Boolean;
begin
    Result := Synchronize(APop);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.AbortSync : Boolean;
begin
    Result := Synchronize(Abort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.RSetSync : Boolean;
begin
    Result := Synchronize(RSet);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSyncPop3Cli.NoopSync : Boolean;
begin
    Result := Synchronize(Noop);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure TSslPop3Cli.Connect;
begin
    FWSocket.SslEnable := FALSE; // We handle everything in code
    inherited Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslPop3Cli.CreateCtrlSocket;
begin
    FWSocket := TSslWSocket.Create(nil);
    FWSocket.SslMode := sslModeClient;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslPop3Cli.GetSslContext: TSslContext;
begin
    if Assigned(FWSocket) then
        Result := FWSocket.SslContext
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslPop3Cli.GetSslCliGetSession: TSslCliGetSession;
begin
    if Assigned(FWSocket) then
        Result := FWSocket.OnSslCliGetSession
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslPop3Cli.GetSslCliNewSession: TSslCliNewSession;
begin
    if Assigned(FWSocket) then
        Result := FWSocket.OnSslCliNewSession
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslPop3Cli.GetSslVerifyPeer: TSslVerifyPeerEvent;
begin
    if Assigned(FWSocket) then
        Result := FWSocket.OnSslVerifyPeer
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslPop3Cli.SetSslCliGetSession(const Value: TSslCliGetSession);
begin
    if Assigned(FWSocket) then
        FWSocket.OnSslCliGetSession := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslPop3Cli.SetSslCliNewSession(const Value: TSslCliNewSession);
begin
    if Assigned(FWSocket) then
        FWSocket.OnSslCliNewSession := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslPop3Cli.SetSslVerifyPeer(const Value: TSslVerifyPeerEvent);
begin
    if Assigned(FWSocket) then
        FWSocket.OnSslVerifyPeer := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslPop3Cli.GetSslAcceptableHosts: TStrings;
begin
    if Assigned(FWSocket) then
        Result := FWSocket.SslAcceptableHosts
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslPop3Cli.SetSslAcceptableHosts(const Value: TStrings);
begin
    if Assigned(FWSocket) then
        FWSocket.SslAcceptableHosts.Assign(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslPop3Cli.GetSslCliCertRequest: TSslCliCertRequest;
begin
    if Assigned(FWSocket) then
        Result := FWSocket.OnSslCliCertRequest
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslPop3Cli.SetSslCliCertRequest(
  const Value: TSslCliCertRequest);
begin
    if Assigned(FWSocket) then
        FWSocket.OnSslCliCertRequest := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslPop3Cli.SetSslContext(Value: TSslContext);
begin
    if Assigned(FWSocket) then
        FWSocket.SslContext := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslPop3Cli.Open;
begin
    if FSslType = pop3TlsExplicit then
        HighLevelAsync(pop3Open, [pop3FctConnect, pop3FctStartTls,
                                  pop3FctUser, pop3FctPass])
    else
        inherited Open;    
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslPop3Cli.TransferSslHandshakeDone(Sender: TObject; ErrCode: Word;
  PeerCert: TX509Base; var Disconnect: Boolean);
begin
    if Assigned(FOnSslHandShakeDone) then
        FOnSslHandShakeDone(Sender, ErrCode, PeerCert, Disconnect);
    if (ErrCode = 0) and (not Disconnect) and
       (FWSocket.State = wsConnected) then begin
        with (Sender as TSslWSocket) do
            TriggerDisplay(
                           Format('! Secure connection with %s, cipher %s, ' +
                                  '%d secret bits (%d total)',
                                  [SslVersion, SslCipher, SslSecretBits,
                                   SslTotalBits])
                           ); 
        if FSslType = pop3TlsImplicit then
            StateChange(pop3WaitingBanner)
        else begin
            FProtocolState := pop3WaitingUser;
            TriggerRequestDone(0);
        end;
    end
    else begin
        if (FWSocket.State = wsConnected) then begin
            { Temporarily disable RequestDone }
            FRequestDoneFlag := TRUE;
            FWSocket.Abort;
            FRequestDoneFlag := FALSE;
        end;
        FErrorMessage  := '-ERR SSL Handshake';
        FStatusCode    := 500;
        FRequestResult := FStatusCode;
        FNextRequest   := nil;
        TriggerRequestDone(FRequestResult);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslPop3Cli.Stls;
begin
    if (FSslType = pop3TlsImplicit) or (FProtocolState = pop3Transaction) then
    begin
        FErrorMessage := '-ERR STLS command invalid now';
        Display(FErrorMessage);
        raise Pop3Exception.Create(FErrorMessage);
    end;
    FFctPrv := pop3FctStartTls;
    ExecAsync(pop3StartTls, 'STLS', pop3WaitingUser, TlsNext);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslPop3Cli.TlsNext;
begin
    if FRequestResult <> 0 then begin
        TriggerRequestDone(FRequestResult);
        Exit;
    end;
    TriggerDisplay('! Starting SSL handshake');
    FWSocket.OnSslHandshakeDone := TransferSslHandShakeDone;
    FWSocket.SslEnable := TRUE;
    try
        //raise Exception.Create('Test');
        FWSocket.StartSslHandshake;
    except
        on E: Exception do begin
            FWSocket.SslEnable := FALSE;
            FErrorMessage  := '-ERR SslHandshake ' + E.Classname + ' ' +
                              E.Message;
            FStatusCode    := 500;
            FRequestResult := FStatusCode;
            { Temporarily disable RequestDone }
            FRequestDoneFlag := TRUE;
            FWSocket.Abort; { here we must not abort, however it's more secure }
            FRequestDoneFlag := FALSE;
            FNextRequest := nil;
            TriggerRequestDone(FRequestResult);
        end
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslPop3Cli.WSocketSessionConnected(Sender: TObject; Error: Word);
begin
    if (FSslType <> pop3TlsImplicit) or (Error <> 0) then
        inherited WSocketSessionConnected(Sender, Error)
    else begin
        FConnected := TRUE;
        TriggerDisplay('! Starting SSL handshake');
        FWSocket.OnSslHandshakeDone := TransferSslHandShakeDone;
        FWSocket.SslEnable := TRUE;
        try
            //raise Exception.Create('Test');
            FWSocket.StartSslHandshake;
        except
            on E: Exception do begin
                FWSocket.SslEnable := FALSE;
                FErrorMessage  := '-ERR SslHandshake ' + E.Classname + ' ' +
                                  E.Message; 
                FStatusCode    := 500;
                FRequestResult := FStatusCode;
                { Temporarily disable RequestDone }
                FRequestDoneFlag := TRUE;
                FWSocket.Abort;
                FRequestDoneFlag := FALSE;
                FNextRequest := nil;
                TriggerRequestDone(FRequestResult);
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslPop3Cli.DoHighLevelAsync;
begin
    {$IFDEF TRACE} TriggerDisplay('! HighLevelAsync ' + IntToStr(FRequestResult)); {$ENDIF}
    if FState = pop3Abort then begin
        {$IFDEF TRACE} TriggerDisplay('! Abort detected'); {$ENDIF}
        FFctSet := [];
        FHighLevelResult := 426;
        FErrorMessage    := '426 Operation aborted.';
    end;

    FNextRequest := DoHighLevelAsync;

    if FRequestResult <> 0 then begin
        { Previous command had errors }
        FHighLevelResult := FRequestResult;
        if (FFctPrv = pop3FctQuit) or (not (pop3FctQuit in FFctSet)) then
            FFctSet := []
        else
            FFctSet := [pop3FctQuit];
    end;

    if pop3FctConnect in FFctSet then begin
        FFctPrv := pop3FctConnect;
        FFctSet := FFctSet - [FFctPrv];
        Connect;
        Exit;
    end;

    if pop3FctStartTls in FFctSet then begin
        FFctPrv := pop3FctStartTls;
        FFctSet := FFctSet - [FFctPrv];
        Stls;
        Exit;
    end;

    if pop3FctUser in FFctSet then begin
        FFctPrv := pop3FctUser;
        FFctSet := FFctSet - [FFctPrv];
        User;
        Exit;
    end;

    if pop3FctPass in FFctSet then begin
        FFctPrv := pop3FctPass;
        FFctSet := FFctSet - [FFctPrv];
        Pass;
        Exit;
    end;

    if pop3FctRPop in FFctSet then begin
        FFctPrv := pop3FctRPop;
        FFctSet := FFctSet - [FFctPrv];
        RPop;
        Exit;
    end;

    if pop3FctDele in FFctSet then begin
        FFctPrv := pop3FctDele;
        FFctSet := FFctSet - [FFctPrv];
        Dele;
        Exit;
    end;

    if pop3FctNoop in FFctSet then begin
        FFctPrv := pop3FctNoop;
        FFctSet := FFctSet - [FFctPrv];
        Noop;
        Exit;
    end;

    if pop3FctList in FFctSet then begin
        FFctPrv := pop3FctList;
        FFctSet := FFctSet - [FFctPrv];
        List;
        Exit;
    end;

    if pop3FctRSet in FFctSet then begin
        FFctPrv := pop3FctRSet;
        FFctSet := FFctSet - [FFctPrv];
        RSet;
        Exit;
    end;

    if pop3FctAPop in FFctSet then begin
        FFctPrv := pop3FctAPop;
        FFctSet := FFctSet - [FFctPrv];
        APop;
        Exit;
    end;

    if pop3FctRetr in FFctSet then begin
        FFctPrv := pop3FctRetr;
        FFctSet := FFctSet - [FFctPrv];
        Retr;
        Exit;
    end;

    if pop3FctTop in FFctSet then begin
        FFctPrv := pop3FctTop;
        FFctSet := FFctSet - [FFctPrv];
        Top;
        Exit;
    end;

    if pop3FctStat in FFctSet then begin
        FFctPrv := pop3FctStat;
        FFctSet := FFctSet - [FFctPrv];
        Stat;
        Exit;
    end;

    if pop3FctUidl in FFctSet then begin
        FFctPrv := pop3FctUidl;
        FFctSet := FFctSet - [FFctPrv];
        Uidl;
        Exit;
    end;

    if pop3FctLast in FFctSet then begin
        FFctPrv := pop3FctLast;
        FFctSet := FFctSet - [FFctPrv];
        Last;
        Exit;
    end;

    if pop3FctQuit in FFctSet then begin
        FFctPrv := pop3FctQuit;
        FFctSet := FFctSet - [FFctPrv];
        Quit;
        Exit;
    end;

    {$IFDEF TRACE} TriggerDisplay('! HighLevelAsync done'); {$ENDIF}
    FFctSet          := [];
    FNextRequest     := nil;
    FRequestDoneFlag := FALSE;
    TriggerRequestDone(FHighLevelResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF} // USE_SSL

end.

