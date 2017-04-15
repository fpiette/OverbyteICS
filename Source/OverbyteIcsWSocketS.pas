{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  A TWSocket that has server functions: it listen to connections
              an create other TWSocket to handle connection for each client.
Creation:     Aug 29, 1999
Version:      8.46
EMail:        francois.piette@overbyte.be     http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1999-2017 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
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

Quick reference guide:
TWSocketServer will normally be used to listen on a given tcp port. When a
client connect, it will instanciate a new TWSocketClient component to handle
communication with client. Normally you will derive your own component from
TWSocketClient to add private data and methods to handle it. You tell
TWSocketServer which component it has to instanciate using ClientClass
property. You have to initialize instances from OnClientConnect event handler.
TWSocketServer maintain a list of connected clients. You can access it using
Client[] indexed property and ClientCount property.

History:
Sep 05, 1999 V1.01 Adpted for Delphi 1
Oct 09, 1999 V1.02 Added intermediate class TCustomWSocket
Nov 12, 1999 V1.03 Added OnClientCreate event just after client component has
                   been created.
Apr 02, 2000 V1.04 Added FSessionClosedFlag to avoid double SessionClosed
                   event triggering
Apr 13, 2002 V1.05 When sending banner to client, add LineEnd instead of CR/LF
                   as suggested by David Aguirre Grazio <djagra@xaire.com>
Sep 13, 2002 V1.06 Check if Assigned(Server) in TriggerSessionClosed.
                   Reported by Matthew Meadows <matthew.meadows@inquisite.com>
Sep 16, 2002 V1.07 Fixed a Delphi 1 issue in TriggerSessionClosed where
                   property was used in place of field variable.
Jan 04, 2003 V1.08 Renamed BannerToBusy to BannerTooBusy. This will cause
                   trouble in applications already using this property. You
                   have to rename the property in your app !
Jan 24, 2003 V5.00 Skipped to version 5 because of SSL code
Jan 26, 2004 V5.01 Introduced ICSDEFS.INC and reordered uses for FPC
                   compatibility.
May 01, 2004 V5.02 WMClientClosed was incorrectly referencing global Error
                   variable instead of the real winsock error code. Now pass
                   the errcode in WParam at the time of PostMessage.
                   Removed Forms and Graphics units from the uses clause.
May 23, 2005 V5.03 Added intermediate variable NewHSocket in procedure
                   TriggerSessionAvailable
Dec 30, 2005 V6.00b A.Garrels added IcsLogger
Jan 06, 2008 V6.01 Angus added Disconnect(Client) and DisconnectAll
May 01, 2008 V6.02 A. Garrels - Function names adjusted according to changes in
                   OverbyteIcsLibrary.pas.
May 14, 2008 V6.03 A. Garrels - Type change from String to AnsiString in
                   TWSocketClient (FPeerPort and FPeerAddr).
Aug 11, 2008 V6.04 A. Garrels - Type AnsiString rolled back String.
Nov 6,  2008 V7.00 Angus added CliId property used to ensure correct client freed
                    (did not call it ID to avoid conflicts with existing clients)
Aug 8,  2010 V7.01 FPiette enhanced TriggerSessionAvailable so catch exception
                   in client class constructor and ClientCreate, and close the
                   remote socket in that case.
Feb 4,  2011 V7.02 Angus added bandwidth throttling using TCustomThrottledWSocket.
                   Client sockets inherit server settings for BandwidthLimit and
                   BandwidthSampling, but these can be changed ideally in
                   OnClientCreate event, but also in OnClientConnect but note a
                   timer may have been started by then so better to default to
                   BandwidthLimit=0 and set it, than to disable it.
Apr 15, 2011 V7.03 Arno prepared for 64-bit.
May 13, 2011 V7.04 Anton S. found a small issue with CliId.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Jul 21, 2012 V8.01 Fix in TCustomMultiListenWSocketServer.TriggerClientConnect.
Jun 03, 2013 V8.02 FPiette added unit "Types" so that some inlines are
                   expanded.
Jun 09, 2013 V8.03 FPiette WMClientClosed clear CliId before freeing.
Aug 18, 2013 V8.04 Arno - It was not possible to clear both string properties
                   Banner and BannerTooBusy in OI since empty strings were not
                   stored in the .dfm.
Aug 18, 2013 V8.05 Arno added some default property specifiers.
Mar 10, 2015 V8.06 Angus CloseDelayed when too many clients so closes cleanly
Mar 23, 2015 V8.07 Angus onSslServerName and OnBgException events set for clients
Oct 26, 2016 V8.36 Angus TWSocketMultiListenItem now has SslEnable moved from SSL class
                   Added extended exception information
Nov 9,  2016  V8.37 Client inherits server SocketErrs
Apr 11, 2017  V8.45 Added multiple SSL host support to TSslWSocketServer.
                    There is a new IcsHosts property which allows multiple hosts
                      to be specified, each with one or two IP addresses and
                      non-SSL and SSL port bindings, SSL certificates and private
                      key, SSL context and security level, and other web server
                      host related properties (not used here).
                    If IcsHosts is specified, TSslWSocketServer ignores existing
                      bindings and SSLContext, and creates new bindings and
                      initialises an SSL context for each host checking and
                      reporting all server certificate chains.
                    IcsHosts includes SslSrvSecurity SSL server security
                       level, that sets protocol, cipher and SslSecLevel according
                       to eight levels from sslSrvSecNone to sslSrvSecHigh192 to
                       avoid needing to set protocols and ciphers in SSL context.
                    IcsHosts includes SslCert, SslKey, SslPassword and SslInter.
                      SslCert may be a file name or a ASCII PEM string containing
                      an Ssl server certificate. The file may be PEM/CER/DER/PFX/P12/etc,
                      either a single certificate or bundle with private key and
                      any intermediates (PEM/PFX/P12 only), SslPassword is the private
                      key password if encrypted.  If a bundle is not found, SslKey is
                      a PEM file or ASCII PEM private key, and SslInters is any
                      intermediate SSL certificates used to sign the server SSL cert
                      as a file name in PEM/CER/DER format or ASCII PEM.
                    TSslWSocketServer has new RootCA property which should be loaded
                      with CA Root certificates to validates the server chain and
                      new DHParams property for a context DHParams file or string.
                    The ValidateHosts property checks all host properties and
                      attempts to validate SSL certificate chains and hosts,
                      it keeps certificate chain information in the IcsHosts CertInfo
                      property and validation warnings or error in ErrInfo.
                    TWSocketClient includes new properties IcsHostIdx, MultiListenIdx
                      and HostTag to identify which IcsHost and MultiListen socket
                      accepted the connection.  Published client server and remote peer
                      address and port as CServerAddr, CServerPort, CPeerAddr and
                      CPeerPort since many clients need this information.
Apr 12, 2017  V8.46 New RootCA property is now a String (filename or base84 string)


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsWSocketS;
{$ENDIF}

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  OFF}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
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
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Types{$ELSE}Types{$ENDIF},
    OverbyteIcsWinsock,
{$ENDIF}
{$IFDEF POSIX}
    Posix.Errno,
    Posix.NetinetIn,
    Posix.SysSocket,
    Ics.Posix.WinTypes,
    Ics.Posix.Messages,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
{$IFDEF USE_SSL}
    OverbyteIcsSSLEAY, OverbyteIcsLIBEAY,
{$ENDIF}
{$IFNDEF NO_DEBUG_LOG}
    OverbyteIcsLogger,
{$ENDIF}
{$IFDEF FMX}
    Ics.Fmx.OverbyteIcsWSocket,
{$ELSE}
    OverbyteIcsWSocket,
{$ENDIF}
    OverbyteIcsUtils, OverbyteIcsTypes;

const
    WSocketServerVersion     = 846;
    CopyRight : String       = ' TWSocketServer (c) 1999-2017 F. Piette V8.46 ';

type
    TCustomWSocketServer       = class;
    TWSocketClient             = class;
    TWSocketClientClass        = class of TWSocketClient;
    TWSocketClientCreateEvent  = procedure (Sender : TObject;
                                            Client : TWSocketClient) of object;
    TWSocketClientConnectEvent = procedure (Sender : TObject;
                                            Client : TWSocketClient;
                                            Error  : Word) of object;

    TClientIdRec = record    { angus V7.00 }
        PClient : Pointer;
        CliId   : LongInt;
    end;
    PClientIdRec = ^TClientIdRec;

    { TWSocketClient is used to handle all client connections.           }
    { Altough you may use it directly, you'll probably wants to use your }
    { own derived component to add data and methods suited to your       }
    { application.                                                       }
    { If you use a derived component, then assign it's class to          }
    { TWSocketServer ClientClass property.                               }
    TWSocketClient = class(TWSocket)
    protected
        FBanner            : String;
        FServer            : TCustomWSocketServer;
        FPeerAddr          : String;
        FPeerPort          : String;
        FSessionClosedFlag : Boolean;
        FCliId             : LongInt;          { angus V7.00 }
{$IFDEF USE_SSL}
        FIcsHostIdx        : Integer;          { V8.45 }
        FMultiListenIdx    : Integer;          { V8.45 }
        FHostTag           : String;           { V8.45 }
        FServerAddr        : String;           { V8.45 }
        FServerPort        : String;           { V8.45 }
{$ENDIF} // USE_SSL
    public
        procedure   StartConnection; virtual;
        procedure   TriggerSessionClosed(ErrCode : Word); override;
        procedure   Dup(newHSocket : TSocket); override;
        function    GetPeerAddr: String; override;
        function    GetPeerPort: String; override;
        property    Server : TCustomWSocketServer read  FServer
                                                  write FServer;
        property    CliId : LongInt               read  FCliId              { angus V7.00 }
                                                  write FCliId;
        property    CPeerAddr : String            read  FPeerAddr;        { V8.45 }
        property    CPeerPort : String            read  FPeerPort;        { V8.45 }
{$IFDEF USE_SSL}
        property    IcsHostIdx : Integer          read FIcsHostIdx;       { V8.45 }
        property    MultiListenIdx : Integer      read FMultiListenIdx;   { V8.45 }
        property    HostTag : String              read FHostTag;          { V8.45 }
        property    CServerAddr : String          read FServerAddr;      { V8.45 }
        property    CServerPort : String          read FServerPort;      { V8.45 }
{$ENDIF} // USE_SSL
    published
        property    Banner : String               read  FBanner
                                                  write FBanner;
    end;

    { TWSocketServer is made for listening for tcp client connections.      }
    { For each connection, it instanciate a new TWSocketClient (or derived) }
    { to handle connection. Use ClientClass to specify your derived.        }
    TCustomWSocketServer = class(TWSocket)
    private
        procedure ReadBannerValue(Reader: TReader);
        procedure WriteBannerValue(Writer: TWriter);
        procedure WriteBannerTooBusyValue(Writer: TWriter);
        procedure ReadBannerTooBusyValue(Reader: TReader);
        function  IsBannerStored: Boolean;
        function  IsBannerTooBusyStored: Boolean;
    protected
        FBanner                 : String;
        FBannerTooBusy          : String;
        FClientClass            : TWSocketClientClass;
        FClientList             : TList;
        FClientNum              : LongInt;
        FMaxClients             : LongInt;
        FMsg_WM_CLIENT_CLOSED   : UINT;
        FOnClientCreate         : TWSocketClientCreateEvent;
        FOnClientConnect        : TWSocketClientConnectEvent;
        FOnClientDisconnect     : TWSocketClientConnectEvent;
        procedure DefineProperties(Filer: TFiler); override;
        procedure WndProc(var MsgRec: TMessage); override;
        procedure Notification(AComponent: TComponent; operation: TOperation); override;
        procedure TriggerSessionAvailable(Error : Word); override;
        procedure TriggerClientCreate(Client : TWSocketClient); virtual;
        procedure TriggerClientConnect(Client : TWSocketClient; Error : Word); virtual;
        procedure TriggerClientDisconnect(Client : TWSocketClient; Error : Word); virtual;
        function  GetClientCount : Integer; virtual;
        function  GetClient(nIndex : Integer) : TWSocketClient; virtual;
        procedure WMClientClosed(var msg: TMessage); virtual;
        function  MsgHandlersCount: Integer; override;
        procedure AllocateMsgHandlers; override;
        procedure FreeMsgHandlers; override;
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        { Check  if a given object is one of our clients }
        function  IsClient(SomeThing : TObject) : Boolean;
        procedure Disconnect(Client: TWSocketClient); virtual;        { angus V6.01 }
        procedure DisconnectAll; virtual;                             { angus V6.01 }
    protected
        { TWSocketClient derived class to instanciate for each client }
        property  ClientClass            : TWSocketClientClass
                                                      read  FClientClass
                                                      write FClientClass;
        { How many active clients we currently have }
        property  ClientCount   : Integer             read  GetClientCount;
        { Client[] give direct access to anyone of our clients }
        property  Client[nIndex : Integer] : TWSocketClient
                                                      read  GetClient;
    published
        { Banner sent to client as welcome message. Can be empty. }
        property  Banner                 : String     read  FBanner
                                                      write FBanner
                                                      stored IsBannerStored;
        property  BannerTooBusy          : String     read  FBannerTooBusy
                                                      write FBannerTooBusy
                                                      stored IsBannerTooBusyStored;
        property  MaxClients             : LongInt    read  FMaxClients
                                                      write FMaxClients
                                                      default 0;
        { Triggered when a client disconnect }
        property  OnClientDisconnect     : TWSocketClientConnectEvent
                                                      read  FOnClientDisconnect
                                                      write FOnClientDisconnect;
        { Triggered when a new client is connecting }
        property  OnClientConnect        : TWSocketClientConnectEvent
                                                      read  FOnClientConnect
                                                      write FOnClientConnect;
        { Triggered when a new client component has been created }
        property  OnClientCreate         : TWSocketClientCreateEvent
                                                      read  FOnClientCreate
                                                      write FOnClientCreate;
    end;

    TWSocketServer = class;

    TCustomMultiListenWSocketServer = class;

    TWSocketMultiListenItem = class(TCollectionItem {$IFDEF POSIX}, IIcsEventSource{$ENDIF})
    private
      FAddr: string;
      FHSocket: TSocket;
      FListenBacklog: Integer;
      FPort: string;
      FSocketFamily: TSocketFamily;
      FOldSocketFamily: TSocketFamily;
      FState: TSocketState;
      FPortNum: Integer;
      FLastError: Integer;
      FCloseInvoked: Boolean;
      FPaused: Boolean;
      FSelectEvent: LongWord;
{$IFDEF USE_SSL}
      FSslEnable : Boolean;         { V8.36 moved from SSL class }
{$ENDIF} // USE_SSL
      procedure SetAddr(const Value: string);
      procedure SetSocketFamily(const Value: TSocketFamily);
      function GetAddrResolved: string;
  {$IFDEF POSIX} { IIcsEventSource }
    strict private
      FPxEventMask        : LongWord;
      FPxFileDescriptor   : Integer;
      FPxEventState       : TIcsAsyncEventState;
      FPxEventMessageID   : UINT;
      FPxEventWindow      : HWND;
      FPxObjectID         : NativeInt;
      function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
      function  GetEventMask: LongWord;
      procedure SetEventMask(const AValue: LongWord);
      function  GetNotifyMessageID: UINT;
      procedure SetNotifyMessageID(const AValue: UINT);
      function  GetNotifyWindow: HWND;
      function  GetEventState: TIcsAsyncEventState;
      function  GetFileDescriptor: Integer;
      procedure SetFileDescriptor(const AValue: Integer);
      function  GetObject: TObject;
      procedure SetEventState(const AValue: TIcsAsyncEventState);
      procedure SetNotifyWindow(const AValue: HWND);
      function  GetObjectID: NativeInt;
  {$ENDIF POSIX IIcsEventSource}
    protected
      procedure AssignDefaults; virtual;
      procedure SetCloseInvoked(const AValue: Boolean);
      function  GetCloseInvoked: Boolean;
      property  CloseInvoked: Boolean read GetCloseInvoked write SetCloseInvoked;
    public
      Fsin: TSockAddrIn6;
      constructor Create(Collection: TCollection); override;
      destructor Destroy; override;
      procedure Close;
      procedure Listen;
      function  OwnerServer: TCustomMultiListenWSocketServer;
      function  Pause: Boolean;
      function  Resume: Boolean;
      property  AddrResolved: string read GetAddrResolved;
      property  HSocket: TSocket read FHSocket write FHSocket;
      property  LastError: Integer read FLastError write FLastError;
      property  Paused: Boolean read FPaused;
      property  PortNum: Integer read FPortNum write FPortNum;
      property  State: TSocketState read FState write FState;
      function  SetAddressListChangeNotification: Boolean;
      function  SetRoutingInterfaceChangeNotification: Boolean;
      property  SelectEvent: LongWord read FSelectEvent;
    published
      property Addr: string read FAddr write SetAddr;
      property ListenBacklog: Integer           read  FListenBacklog
                                                write FListenBacklog default 5;
      property Port: string read FPort write FPort;
      property SocketFamily: TSocketFamily      read  FSocketFamily
                                                write SetSocketFamily
                                                default DefaultSocketFamily;
 {$IFDEF USE_SSL}
      property SslEnable : Boolean read FSslEnable write FSslEnable;   { V8.36 moved from SSL class }
{$ENDIF} // USE_SSL
   end;

    TWSocketMultiListenItemClass = class of TWSocketMultiListenItem;

    TWSocketMultiListenCollection = class(TOwnedCollection)
    protected
      function GetItem(Index: Integer): TWSocketMultiListenItem;
        {$IFDEF USE_INLINE} inline; {$ENDIF}
      procedure SetItem(Index: Integer; Value: TWSocketMultiListenItem);
        {$IFDEF USE_INLINE} inline; {$ENDIF}
    public
      constructor Create(AOwner     : TPersistent;
                         AItemClass : TWSocketMultiListenItemClass);
      function Add: TWSocketMultiListenItem;
          {$IFDEF USE_INLINE} inline; {$ENDIF}
      function FindItemIndex(const AHSocket: TSocket): Integer;
          {$IFDEF USE_INLINE} inline; {$ENDIF}
      function FindItemHandle(const AHSocket: TSocket): TWSocketMultiListenItem;
      function FindItemID(ID: Integer): TWSocketMultiListenItem;
          {$IFDEF USE_INLINE} inline; {$ENDIF}
      function Insert(Index: Integer): TWSocketMultiListenItem;
          {$IFDEF USE_INLINE} inline; {$ENDIF}
      function Owner: TCustomMultiListenWSocketServer;
      property Items[Index: Integer]: TWSocketMultiListenItem
                                                        read  GetItem
                                                        write SetItem; default;
    end;

    TCustomMultiListenWSocketServer = class(TCustomWSocketServer)
    private
        FMultiListenSockets: TWSocketMultiListenCollection;
        FMultiListenIndex: Integer;
    protected
        procedure Ml_Do_FD_CLOSE(AItem: TWSocketMultiListenItem;
                                  AMsg: TMessage); virtual;
        procedure MlListen(AItem: TWSocketMultiListenItem); virtual;
        procedure MlClose(AItem: TWSocketMultiListenItem); virtual;
        procedure MlSocketError(AItem: TWSocketMultiListenItem;
            const ASockFunc: String; ALastError: Integer = 0;
                FriendlyMsg: String = ''); virtual;  { V8.36 added FriendlyMsg }
        procedure MlPause(AItem: TWSocketMultiListenItem); virtual;
        procedure MlResume(AItem: TWSocketMultiListenItem); virtual;
        procedure MlSetAddr(var FldAddr              : string;
                            var FldSocketFamily      : TSocketfamily;
                            const FldOldSocketFamily : TSocketfamily;
                            const NewValue           : string); virtual;
        procedure MlSetSocketFamily(var FldSocketFamily    : TSocketfamily;
                                    var FldOldSocketFamily : TSocketfamily;
                                    const NewValue         : TSocketFamily);
        function  MultiListenItemClass: TWSocketMultiListenItemClass; virtual;
        procedure SetMultiListenIndex(const Value: Integer);
        procedure TriggerClientConnect(Client: TWSocketClient; Error: Word); override;
        procedure WMASyncSelect(var msg: TMessage); override;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        function  Accept: TSocket; override;
        procedure Close; override;
        procedure Listen; override;
        procedure MultiListen; virtual;
        procedure MultiClose; virtual;
        procedure ThreadAttach; override;
        procedure ThreadDetach; override;
        property  MultiListenIndex: Integer read  FMultiListenIndex;

        property  MultiListenSockets: TWSocketMultiListenCollection
                                                      read  FMultiListenSockets
                                                      write FMultiListenSockets;
    end;

    TWSocketServer = class(TCustomMultiListenWSocketServer)
    public
        property  ClientClass;
        property  ClientCount;
        property  Client;
    published
    {$IFNDEF NO_DEBUG_LOG}
        property  IcsLogger;                                 { V5.04 }
    {$ENDIF}
        property  Banner;
        property  BannerTooBusy;
        property  MaxClients;
        property  MultiListenSockets;
        property  OnClientDisconnect;
        property  OnClientConnect;
    end;

{ You must define USE_SSL so that SSL code is included in the component.    }
{ Either in OverbyteIcsDefs.inc or in the project/package options.          }
{$IFDEF USE_SSL}
{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  A component adding SSL support to TWSocketServer.
              Requires OpenSSL (http://www.openssl.org).
              More details in ReadMeIcsSsl.txt and IcsSslHowTo.txt.
              SSL demo applications can be found in /Delphi/SslInternet.
              If you use Delphi 7 and later, you may want to disable warnings
              for unsage type, unsafe code and unsafe typecast in the project
              options. Those warning are intended for .NET programs. You may
              also want to turn off deprecated symbol and platform symbol
              warnings.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{const
     SslWSocketServerVersion            = 100;
     SslWSocketServerDate               = 'Feb 02, 2003';
     SslWSocketServerCopyRight : String = ' TSslWSocket (c) 2003 Francois Piette V1.00.3 ';   }

type
{ TIcsHost defines the bindings for an SSL TCP/IP server, usually a web server,
   such as which address and port to listen, which host names to accept using
   which SSL certificate, and server related set-up such as directories }

  TIcsHost = class(TCollectionItem)    { V8.45 }
  private
    FHostNames: TStrings;
    FHostEnabled: boolean;
    FBindIpAddr: String;
    FBindIpAddr2: String;
    FBindSslPort: integer;
    FBindNonPort: integer;
    FHostTag: String;
    FDescr: String;
    FWebDocDir: string;
    FWebTemplDir: string;
    FWebDefDoc: string;
    FWebLogDir: string;
    FSslCert: string;
    FSslKey: string;
    FSslInter: string;
    FSslPassword: string;
    FSslSrvSecurity: TSslSrvSecurity;
    FCertDomains: string;
    FCertInfo: string;
    FCertExiry: TDateTime;
    FCertErrs: string;
    FCertValRes: TChainResult;
    FBindIdxNone: Integer;
    FBindIdxSsl: Integer;
    FBindIdx2None: Integer;
    FBindIdx2Ssl: Integer;
  protected
    function GetDisplayName: string; override;
    function GetHostNameTot: integer;
    procedure SetHostNames(Value : TStrings);
  public
    SslCtx: TSslContext;
    property HostNameTot : Integer               read  GetHostNameTot;
    property DisplayName : String                read  GetDisplayName;
    property CertDomains : String                read  FCertDomains;
    property CertInfo : String                   read  FCertInfo;
    property CertExiry : TDateTime               read  FCertExiry;
    property CertErrs : String                   read  FCertErrs;
    property CertValRes : TChainResult           read  FCertValRes;
    property BindIdxNone : Integer               read  FBindIdxNone;
    property BindIdxSsl : Integer                read  FBindIdxSsl;
    property BindIdx2None : Integer              read  FBindIdx2None;
    property BindIdx2Ssl : Integer               read  FBindIdx2Ssl;
  published
    constructor Create (Collection: TCollection); Override ;
    destructor Destroy; override;
    property HostNames : TStrings                read  FHostNames
                                                 write SetHostNames;
    property HostEnabled : boolean               read  FHostEnabled
                                                 write FHostEnabled;
    property BindIpAddr : String                 read  FBindIpAddr
                                                 write FBindIpAddr;
    property BindIpAddr2 : String                read  FBindIpAddr2
                                                 write FBindIpAddr2;
    property BindSslPort : Integer               read  FBindSslPort
                                                 write FBindSslPort;
    property BindNonPort : Integer               read  FBindNonPort
                                                 write FBindNonPort;
    property HostTag: String                     read  FHostTag
                                                 write FHostTag;
    property Descr: String                       read  FDescr
                                                 write FDescr;
    property WebDocDir : String                  read  FWebDocDir
                                                 write FWebDocDir;
    property WebTemplDir : String                read  FWebTemplDir
                                                 write FWebTemplDir;
    property WebDefDoc : String                  read  FWebDefDoc
                                                 write FWebDefDoc;
    property WebLogDir : String                  read  FWebLogDir
                                                 write FWebLogDir;
    property SslCert : String                    read  FSslCert
                                                 write FSslCert;
    property SslKey : String                     read  FSslKey
                                                 write FSslKey;
    property SslInter : String                   read  FSslInter
                                                 write FSslInter;
    property SslPassword : String                read  FSslPassword
                                                 write FSslPassword;
    property SslSrvSecurity : TSslSrvSecurity    read  FSslSrvSecurity
                                                 write FSslSrvSecurity;
  end;

  { TIcsHosts defines a collection of TIcsHost }

  TIcsHostCollection = class(TCollection)     { V8.45 }
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TIcsHost;
    procedure SetItem(Index: Integer; Value: TIcsHost);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent);
    property Items[Index: Integer]: TIcsHost read GetItem write SetItem; default;
  end;

type
    TSslWSocketMultiListenItem = class(TWSocketMultiListenItem)
    private
 {     FSslEnable : Boolean;      V8.36 moved to base class }
    public
      constructor Create(Collection: TCollection); override;
    published
 {     property SslEnable : Boolean read FSslEnable write FSslEnable;     V8.36 moved to base class }
    end;

    TSslWSocketClient = class(TWSocketClient)
    public
        constructor Create(AOwner : TComponent); override;
        procedure   StartConnection; override;
        procedure   TriggerSslServerName(var Ctx: TSslContext; var ErrCode: TTlsExtError); override; { V8.45 }
    end;

    TSslWSocketServer = class(TWSocketServer)
    protected
        FIcsHosts: TIcsHostCollection;            { V8.45 }
        FRootCAX509: TX509Base;                   { V8.46 }
        FRootCA: String;                          { V8.46 }
        FDHParams: String;                        { V8.45 }
        procedure TriggerClientConnect(Client : TWSocketClient; Error : Word); override;
        function  MultiListenItemClass: TWSocketMultiListenItemClass; override;
    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
        property  ClientClass;
        property  ClientCount;
        property  Client;
        property  SslMode;
        procedure Listen; override;
        function  GetIcsHosts: TIcsHostCollection;                   { V8.45 }
        procedure SetIcsHosts(const Value: TIcsHostCollection);      { V8.45 }
        function  FindBinding(const MAddr: String; MPort: Integer;
                                 var MIndex: Integer): boolean;      { V8.45 }
        procedure ValidateHosts; virtual;                            { V8.45 }
    published
        property  SslContext;
        property  Banner;
        property  BannerTooBusy;
        property  MaxClients;
        property  OnClientDisconnect;
        property  OnClientConnect;
        property  SslEnable;
        property  SslAcceptableHosts;
        property  IcsHosts: TIcsHostCollection           read  GetIcsHosts
                                                         write SetIcsHosts;   { V8.45 }
        property  RootCA: String                         read  FRootCA
                                                         write FRootCA;       { V8.46 }
        property  DHParams: String                       read  FDHParams
                                                         write FDHParams;     { V8.45 }
        property  OnSslVerifyPeer;
        property  OnSslSetSessionIDContext;
        property  OnSslSvrNewSession;
        property  OnSslSvrGetSession;
        property  OnSslHandshakeDone;
        property  OnSslServerName;    { V8.07 }
  end;
{$ENDIF} // USE_SSL

implementation

const
    DefaultBanner            = 'Welcome to OverByte ICS TcpSrv';
    DefaultBannerTooBusy     = 'Sorry, too many clients';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.DefineProperties(Filer: TFiler);
begin
    inherited DefineProperties(Filer);
    Filer.DefineProperty('Banner', ReadBannerValue, WriteBannerValue, (Banner = ''));
    Filer.DefineProperty('BannerTooBusy', ReadBannerTooBusyValue,
      WriteBannerTooBusyValue, (BannerTooBusy = ''));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocketServer.IsBannerStored: Boolean;
begin
    Result := Banner <> DefaultBanner;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocketServer.IsBannerTooBusyStored: Boolean;
begin
  Result := BannerTooBusy <> DefaultBannerTooBusy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.WriteBannerValue(Writer: TWriter);
begin
    Writer.WriteString(Banner);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.WriteBannerTooBusyValue(Writer: TWriter);
begin
    Writer.WriteString(BannerTooBusy);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.ReadBannerValue(Reader: TReader);
begin
    Banner := Reader.ReadString;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.ReadBannerTooBusyValue(Reader: TReader);
begin
    BannerTooBusy := Reader.ReadString;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomWSocketServer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FClientList      := TList.Create;
    FClientClass     := TWSocketClient;
    FBanner          := DefaultBanner;
    FBannerTooBusy   := DefaultBannerTooBusy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomWSocketServer.Destroy;
var
    I : Integer;
begin
    if Assigned(FClientList) then begin
        { We need to destroy all clients }
        for I := FClientList.Count - 1 downto 0 do begin
            try
                TWSocketClient(FClientList.Items[I]).Free;
            except
                { Ignore any exception here }
            end;
        end;
        { Then we can destroy client list }
        FClientList.Free;
        FClientList := nil;
    end;
    { And finally destroy ourself }
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocketServer.MsgHandlersCount : Integer;
begin
    Result := 1 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_CLIENT_CLOSED := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then
        FWndHandler.UnregisterMessage(FMsg_WM_CLIENT_CLOSED);
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Message handler                                                           }
procedure TCustomWSocketServer.WndProc(var MsgRec: TMessage);
begin
    with MsgRec do begin
        if Msg = FMsg_WM_CLIENT_CLOSED then begin
            { We *MUST* handle all exception to avoid application shutdown }
            try
                WMClientClosed(MsgRec)
            except
                on E:Exception do
                    HandleBackGroundException(E);
            end;
        end
        else
            inherited WndProc(MsgRec);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Called by destructor when child component (a clients) is create or        }
{ destroyed.                                                                }
procedure TCustomWSocketServer.Notification(
    AComponent : TComponent;
    Operation  : TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Assigned(FClientList) and (AComponent is TWSocketClient) then begin
        if Operation = opInsert then
            { A new client has been created, add it to our list }
            FClientList.Add(AComponent)
        else if Operation = opRemove then
            { If one of our client has been destroyed, remove it from our list }
            FClientList.Remove(AComponent);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Called when a session is available, that is when a client is connecting   }
procedure TCustomWSocketServer.TriggerSessionAvailable(Error : Word);
var
    Client     : TWSocketClient;
    TempHandle : TSocket;
begin
{$IFDEF DEBUG_OUTPUT}
    OutputDebugString('OnSessionAvailable');
{$ENDIF}
    { Call parent event handler }
    inherited TriggerSessionAvailable(Error);
    { In case of error, do nothing }
    if Error <> 0 then
        Exit;

    if Cardinal(FClientNum) >= Cardinal(MaxInt) then    { V7.04 }
        FClientNum := 0;                                { angus V7.00 }
    Inc(FClientNum);
    Client := nil;
    try                                                 { FPiette V7.01 }
        Client                 := FClientClass.Create(Self);
        Client.FCliId          := FClientNum;           { angus V7.00 }
        Client.OnBgException   := FOnBgException;       { angus V8.07 }
        Client.SocketErrs      := FSocketErrs;          { V8.37 }
{$IFDEF BUILTIN_THROTTLE}
        Client.BandwidthLimit    := Self.BandwidthLimit;     { angus V7.02 may be changed in event for different limit }
        Client.BandwidthSampling := Self.BandwidthSampling;  { angus V7.02 }
{$ENDIF}
        TriggerClientCreate(Client);
    except                                               { FPiette V7.01 }
        try                                              { FPiette V7.01 }
            TempHandle := Accept;                        { FPiette V7.01 }
            if TempHandle <> INVALID_SOCKET then         { FPiette V7.01 }
                WSocket_closesocket(TempHandle);         { FPiette V7.01 }
            if Assigned(Client) then                     { FPiette V7.01 }
                Client.Free;                             { FPiette V7.01 }
        except                                           { FPiette V7.01 }
            // safely ignore any exception here. Component user may already
            // have accepted and closed the connection.
        end;                                             { FPiette V7.01 }
        raise;                                           { FPiette V7.01 }
    end;                                                 { FPiette V7.01 }
    Client.Name            := Name + 'Client' + IntToStr(FClientNum);
    Client.Banner          := FBanner;
    Client.Server          := Self;
{$IFNDEF NO_DEBUG_LOG}
    Client.IcsLogger       := IcsLogger;                           { V5.04 }
{$ENDIF}
{$IFDEF MSWINDOWS}
    Client.HSocket         := Accept;
{$ENDIF}
{$IFDEF POSIX}
    TempHandle := Accept;
    { Accept() doesn't raise a socket error for WSAEWOULDBLOCK in POSIX. }
    { IMO Accept() should never raise a socket error here but we should  }
    { call Dup() only if Accept() returned a valid socket handle,        }
    { otherwise pass the error code to TriggerClientConnect() and free   }
    { the client object afterwards, so this is just a workaround.  AG    }
    if (TempHandle = INVALID_SOCKET) and (LastError = WSAEWOULDBLOCK) then
        Error := LastError
    else
        Client.HSocket := TempHandle;
{$ENDIF}
    TriggerClientConnect(Client, Error);
    { The event handler may have destroyed the client ! }
    if FClientList.IndexOf(Client) < 0 then
        Exit;
{$IFDEF POSIX}
    if Error <> 0 then begin
        Client.Free;
        Exit;
    end;
{$ENDIF}
    { The event handler may have closed the connection }
    if Client.State <> wsConnected then
        Exit;
    { Ok, the client is still there, process with the connection }
    if (FMaxClients > 0) and (FMaxClients < ClientCount) then begin
        { Sorry, toomuch clients }
        Client.Banner := FBannerTooBusy;
        Client.StartConnection;
        Client.CloseDelayed;  { V8.06 was Close but too quick }
    end
    else
        Client.StartConnection;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.TriggerClientConnect(
    Client : TWSocketClient; Error : Word);
begin
    if Assigned(FOnClientConnect) then
        FOnClientConnect(Self, Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.TriggerClientCreate(Client : TWSocketClient);
begin
    if Assigned(FOnClientCreate) then
        FOnClientCreate(Self, Client);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.TriggerClientDisconnect(
    Client : TWSocketClient; Error : Word);
begin
    if Assigned(FOnClientDisconnect) then
        FOnClientDisconnect(Self, Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ get number of connect clients                                               }
function TCustomWSocketServer.GetClientCount : Integer;
begin
    if Assigned(FClientList) then
        Result := FClientList.Count
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Acces method to return a client by index.                                   }
{ Return nil if index is out of range.                                        }
function TCustomWSocketServer.GetClient(nIndex : Integer) : TWSocketClient;
begin
    if not Assigned(FClientList) then begin
        Result := nil;
        Exit;
    end;
    if (nIndex < 0) or (nIndex >= FClientList.Count) then begin
        Result := nil;
        Exit;
    end;
    Result := TWSocketClient(FClientList.Items[nIndex]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Client has closed. Remove it from client list and destroy component.        }
procedure TCustomWSocketServer.WMClientClosed(var msg: TMessage);
var
    Client : TWSocketClient;
    PIdRec : PClientIdRec;
begin
    PIdRec := PClientIdRec(Msg.LParam);  { angus V7.00 }
    try
        Client := TWSocketClient(PIdRec^.PClient);
        { angus V7.00 ensure client not freed already }
        if IsClient(Client) and (Client.CliId = PIdRec^.CliId) then begin
            try
                TriggerClientDisconnect(Client, Msg.WParam);
            finally
                { Calling Free will automatically remove client from list     }
                { because we installed a notification handler.                }
                Client.CliId := 0;  { V8.03 }
                Client.Free;
            end;
        end;
    finally
        System.Dispose(PIdRec);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Check if a given object is one of our clients.                              }
function TCustomWSocketServer.IsClient(SomeThing : TObject) : Boolean;
begin
    if not Assigned(FClientList) then
        Result := FALSE
    else
        Result := (FClientList.IndexOf(SomeThing) >= 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.Disconnect(Client: TWSocketClient);        { angus V6.01 }
var
    Msg : TMessage;
    PIdRec : PClientIdRec;
begin
    FillChar(Msg, SizeOf(Msg), 0);
{ angus V7.00 pass CliId to WMClientClosed so correct client is closed  }
    New(PIdRec);
    PIdRec^.PClient := Client;
    PIdRec^.CliId   := Client.CliId;
    Msg.WParam      := WSAECONNABORTED;
    Msg.LParam      := LPARAM(PIdRec);
    WMClientClosed(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocketServer.DisconnectAll;                             { angus V6.01 }
begin
    while ClientCount > 0 do
        Disconnect(Client[0]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{*                                                                           *}
{*                   TCustomMultiListenWSocketServer                         *}
{*                                                                           *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SizeOfAddr(const AAddr: TSockAddrIn6): Integer;
    {$IFDEF USE_INLINE} inline; {$ENDIF}
begin
    if AAddr.sin6_family = AF_INET6 then
        Result := SizeOf(TSockAddrIn6)
    else
        Result := SizeOf(TSockAddrIn);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomMultiListenWSocketServer.Accept: TSocket;
var
    Len     : Integer;
    AItem   : TWSocketMultiListenItem;
  {$IFDEF POSIX}
    LastErr : Integer;
  {$ENDIF}
begin
    if FMultiListenIndex = -1 then
    begin
        Result := inherited Accept;
    end
    else begin
      {$IFDEF POSIX}
        AItem := nil;
        try
      {$ENDIF}
            AItem := FMultiListenSockets[FMultiListenIndex];
            if AItem.State <> wsListening then begin
                WSocket_WSASetLastError(WSAEINVAL);
                MlSocketError(AItem, 'not a listening socket');
                Result := INVALID_SOCKET;
                Exit;
            end;
            Len := SizeOf(AItem.Fsin);
            FASocket := WSocket_Accept(AItem.HSocket, @AItem.Fsin, @Len);
            Result := FASocket;
            if FASocket = INVALID_SOCKET then begin
              {$IFDEF MSWINDOWS}
                MlSocketError(AItem, 'Accept');
              {$ENDIF}
              {$IFDEF POSIX}
                LastErr := WSocket_WSAGetLastError;
                if LastErr <> WSAEWOULDBLOCK then
                    MlSocketError(AItem, 'Accept', LastErr);
              {$ENDIF}
                Exit;
            end;
      {$IFDEF POSIX}
        finally
            if (AItem <> nil) and (AItem.State = wsListening) then
                WSocketSynchronizedEnableAcceptEvent(AItem);
        end;
      {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.Close;
begin
    FMultiListenIndex := -1;
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomMultiListenWSocketServer.Create(AOwner: TComponent);
begin
    inherited;
    FMultiListenIndex := -1;
    FMultiListenSockets := TWSocketMultiListenCollection.Create(
                              Self, MultiListenItemClass);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomMultiListenWSocketServer.Destroy;
begin
  FMultiListenSockets.Free;
  inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.Listen;
begin
    FMultiListenIndex := -1;
    inherited Listen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MlClose(
  AItem: TWSocketMultiListenItem);
var
    iStatus : Integer;
begin
    FMultiListenIndex := AItem.Index;
    if AItem.HSocket = INVALID_SOCKET then
    begin
        AItem.AssignDefaults;
        Exit;
    end;

    if AItem.State = wsClosed then
        Exit;

    if AItem.HSocket <> INVALID_SOCKET then begin
        repeat
            { Close the socket }
            iStatus := WSocket_closesocket(AItem.HSocket);
            if iStatus <> 0 then begin
                AItem.LastError := WSocket_WSAGetLastError;
                if AItem.LastError <> WSAEWOULDBLOCK then begin
                  {$IFDEF POSIX}
                    WSocketSynchronizedRemoveEvents(AItem, False);
                    IcsClearMessages(Handle, FMsg_WM_ASYNCSELECT, WPARAM(AItem.HSocket));
                  {$ENDIF}
                    AItem.HSocket := INVALID_SOCKET;
                  {$IFDEF MSWINDOWS}
                    { Ignore the error occuring when winsock DLL not      }
                    { initialized (occurs when using TWSocket from a DLL) }
                    if AItem.LastError = WSANOTINITIALISED then
                        Break;
                  {$ENDIF}
                    MlSocketError(AItem, 'Disconnect (closesocket)');
                    Exit;
                end;
                MessagePump;
            end;
        until iStatus = 0;
      {$IFDEF POSIX}
        WSocketSynchronizedRemoveEvents(AItem, True);
        IcsClearMessages(Handle, FMsg_WM_ASYNCSELECT, WPARAM(AItem.HSocket));
      {$ENDIF}
        AItem.HSocket := INVALID_SOCKET;
    end;
    AItem.State := wsClosed;
    if (not (csDestroying in ComponentState)) and
       (not AItem.CloseInvoked) {and Assigned(FOnSessionClosed)} then begin
        AItem.CloseInvoked := TRUE;
        TriggerSessionClosed(0);
    end;
    { 29/09/98 Protect AssignDefaultValue because SessionClosed event handler }
    { may have destroyed the component.                                       }
    try
        AItem.AssignDefaults;
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MlSocketError(
    AItem: TWSocketMultiListenItem; const ASockFunc: String;
    ALastError: Integer = 0; FriendlyMsg: String = '');   { V8.36 added FriendlyMsg }
var
    ErrCode  : Integer;
    Line : String;
begin
    FMultiListenIndex := AItem.Index;
    try
        if ALastError = 0 then
            ErrCode := WSocket_WSAGetLastError
        else
            ErrCode := ALastError;
        Line  := 'Listening socket index #' + IntToStr(FMultiListenIndex) + ' ' +
                  WSocketErrorDesc(ErrCode) + ' (#' + IntToStr(ErrCode) +
                  ' in ' + ASockFunc + ')' ;
        if (ErrCode = WSAECONNRESET) or
           (ErrCode = WSAENOTCONN) then begin
            WSocket_closesocket(AItem.HSocket);
            AItem.HSocket := INVALID_SOCKET;
            if AItem.State <> wsClosed then
               TriggerSessionClosed(ErrCode);
            AItem.State := wsClosed;
        end;

        AItem.LastError := ErrCode;
        LastError := ErrCode;
        RaiseException(Line, ErrCode, SocketErrorDesc(ErrCode), FriendlyMsg,
                                   ASockfunc, AItem.Addr, AItem.Port, FProtoStr);  { V8.36 }
    finally
        FMultiListenIndex := -1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.Ml_Do_FD_CLOSE(
    AItem : TWSocketMultiListenItem;
    AMsg  : TMessage);
begin
    if (AItem.HSocket <> INVALID_SOCKET) then begin
        if not AItem.CloseInvoked then
        begin
            AItem.CloseInvoked := TRUE;
            TriggerSessionClosed(IcsHiWord(AMsg.LParam));
        end;
        if AItem.State <> wsClosed then
            MlClose(AItem);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MlListen(
    AItem: TWSocketMultiListenItem);
var
    iStatus : Integer;
    ErrorCode : Integer;
    FriendlyMsg : string;
    optval : Integer;
begin
    FMultiListenIndex := AItem.Index;
    FriendlyMsg := '';
    try
        if (AItem.State <> wsClosed) then begin
            WSocket_WSASetLastError(WSAEINVAL);
            MlSocketError(AItem, 'listen: socket is already listening');
            Exit;
        end;

        if IcsLowerCase(FProtoStr) <> 'tcp' then begin
            WSocket_WSASetLastError(WSAEINVAL);
            MlSocketError(AItem, 'listen: protocol unsupported');
            Exit;
        end;

        if AItem.Port = '' then begin
            WSocket_WSASetLastError(WSAEINVAL);
            MlSocketError(AItem, 'listen: port not assigned');
            Exit;
        end;

        if AItem.Addr = '' then begin
            //WSocket_Synchronized_WSASetLastError(WSAEINVAL);
            WSocket_WSASetLastError(WSAEINVAL);
            MlSocketError(AItem, 'listen: address not assigned');
            Exit;
        end;

        try
            { The next line will trigger an exception in case of failure }
            AItem.PortNum := WSocketResolvePort(
                                  AnsiString(AItem.Port), AnsiString('tcp'));
            AItem.Fsin.sin6_port := WSocket_htons(AItem.PortNum);

            { The next line will trigger an exception in case of failure }
            if AItem.SocketFamily = sfIPv4 then
            begin
                AItem.Fsin.sin6_family := AF_INET;
                PSockAddrIn(@AItem.Fsin).sin_addr.s_addr :=
                    WSocketResolveHost(AnsiString(AItem.Addr)).s_addr;
            end
            else
                WSocketResolveHost(AItem.Addr, AItem.Fsin, AItem.SocketFamily,
                                   IPPROTO_TCP);
        except
            on E: Exception do begin
                AItem.AssignDefaults;
                raise ESocketException.Create('listen: ' + E.Message);
            end;
        end;

        { Remove any data from the internal output buffer }
        { (should already be empty !)                     }
        DeleteBufferedData;

        AItem.HSocket := WSocket_socket(AItem.Fsin.sin6_family, SOCK_STREAM, IPPROTO_TCP);
        if AItem.HSocket = INVALID_SOCKET then begin
            MlSocketError(AItem, 'listen: socket');
            Exit;
        end;

        if FExclusiveAddr then begin
        { V8.36 Prevent other applications accessing this address and port }
            optval  := -1;
            iStatus := WSocket_SetSockOpt(AItem.HSocket, SOL_SOCKET,
                                                       SO_EXCLUSIVEADDRUSE,
                                                       @optval, SizeOf(optval));

            if iStatus <> 0 then begin
                MlSocketError(AItem, 'setsockopt(SO_EXCLUSIVEADDRUSE)');
                MlClose(AItem);
                Exit;
            end;
        end;

        iStatus := WSocket_bind(AItem.HSocket, PSockAddr(@AItem.Fsin)^,
                                           SizeOfAddr(AItem.Fsin));
        if iStatus = 0 then
            AItem.State := wsBound
        else begin
            ErrorCode := WSocket_WSAGetLastError;
            if (ErrorCode = WSAEADDRINUSE) or (ErrorCode = WSAEACCES)  then   { V8.36 more friendly message for common error }
                FriendlyMsg := 'Another server is already listening on ' +
                                                 AItem.Addr + ':' + AItem.Port;
            MlSocketError(AItem, 'listen: Bind', ErrorCode, FriendlyMsg);
            MlClose(AItem);
            Exit;
        end;

        iStatus := WSocket_listen(AItem.HSocket, AItem.ListenBacklog);
        if iStatus = 0 then
            AItem.State := wsListening
        else begin
            MlSocketError(AItem, 'listen: Listen');
            Exit;
        end;

        AItem.FSelectEvent := FD_ACCEPT or FD_CLOSE;

      {$IFDEF MSWINDOWS}
        if wsoNotifyAddressListChange in ComponentOptions then
            AItem.FSelectEvent := AItem.FSelectEvent or FD_ADDRESS_LIST_CHANGE;
        if wsoNotifyRoutingInterfaceChange in ComponentOptions then
            AItem.FSelectEvent := AItem.FSelectEvent or FD_ROUTING_INTERFACE_CHANGE;
      {$ENDIF}

        iStatus := WSocket_WSAASyncSelect(
                                        {$IFDEF POSIX}
                                          AItem,
                                        {$ENDIF}
                                          AItem.HSocket,
                                          Handle,
                                          FMsg_WM_ASYNCSELECT,
                                          AItem.FSelectEvent);
        if iStatus <> 0 then begin
            MlSocketError(AItem, 'listen: WSAASyncSelect');
            Exit;
        end;

      {$IFDEF MSWINDOWS}
        if (wsoNotifyAddressListChange in ComponentOptions) and
           (not AItem.SetAddressListChangeNotification) then begin
            MlSocketError(AItem, 'listen: SetAddressListChangeNotification');
            Exit;
        end;

        if (wsoNotifyRoutingInterfaceChange in ComponentOptions) and
           (not AItem.SetRoutingInterfaceChangeNotification)then begin
            MlSocketError(AItem, 'listen: SetRoutingInterfaceChangeNotification');
            Exit;
        end;
      {$ENDIF}
    finally
        FMultiListenIndex := -1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MlPause(
    AItem: TWSocketMultiListenItem);
begin
    if not AItem.Paused then
        AItem.FPaused := WSocket_WSAASyncSelect(
                                              {$IFDEF POSIX}
                                                AItem,
                                              {$ENDIF}
                                                AItem.HSocket,
                                                Handle, 0, 0) = 0;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MLResume(
    AItem: TWSocketMultiListenItem);
begin
    if AItem.Paused then
        AItem.FPaused := not (WSocket_WSAASyncSelect(
                                                {$IFDEF POSIX}
                                                  AItem,
                                                {$ENDIF}
                                                  AItem.HSocket,
                                                  Handle,
                                                  FMsg_WM_ASYNCSELECT,
                                                  AItem.SelectEvent) = 0);

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MlSetAddr(
  var FldAddr: string; var FldSocketFamily: TSocketfamily;
  const FldOldSocketFamily: TSocketfamily;
  const NewValue: string);
var
    LSocketFamily: TSocketFamily;
begin
    FldAddr := IcsTrim(NewValue);
    if FldAddr = '' then
        Exit;
    { If the address is either a valid IPv4 or IPv6 address }
    { change current SocketFamily.                          }
    if WSocketIsIP(FldAddr, LSocketFamily) then
    begin
        if (LSocketFamily = sfIPv4) or (IsIPv6APIAvailable) then
            FldSocketFamily := LSocketFamily
        else
            FldSocketFamily := FldOldSocketFamily;
    end
    else
        FldSocketFamily := FldOldSocketFamily;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MlSetSocketFamily(
  var FldSocketFamily: TSocketfamily;
  var FldOldSocketFamily: TSocketfamily;
  const NewValue: TSocketFamily);
begin
    if NewValue <> FldSocketFamily then begin
        if NewValue <> sfIPv4 then begin
            try
                if not IsIPv6APIAvailable then
                    raise ESocketException.Create(
                     'SetSocketFamily: New API requires winsock 2.2 ' +
                     'and Windows XP, property "SocketFamily" reset to "sfIPv4"');
            except
                FldSocketFamily := sfIPv4;
                FldOldSocketFamily := FldSocketFamily;
                Exit;
            end;
        end;
        FldSocketFamily := NewValue;
        FldOldSocketFamily :=FldSocketFamily;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MultiClose;
var
    I: Integer;
begin
    if State <> wsClosed then
        Close;
    if Assigned(FMultiListenSockets) then begin
        for I := 0 to FMultiListenSockets.Count - 1 do
            if FMultiListenSockets[I].State <> wsClosed then
                MlClose(FMultiListenSockets[I]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.MultiListen;
var
    I: Integer;
begin
    if State <> wsListening then
        Listen;
    if Assigned(FMultiListenSockets) then
        for I := 0 to FMultiListenSockets.Count - 1 do
            if FMultiListenSockets[I].State <> wsListening then
            MlListen(FMultiListenSockets[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomMultiListenWSocketServer.MultiListenItemClass: TWSocketMultiListenItemClass;
begin
    Result := TWSocketMultiListenItem;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.SetMultiListenIndex(
  const Value: Integer);
begin
    FMultiListenIndex := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.ThreadAttach;
var
    I : Integer;
    LItem : TWSocketMultiListenItem;
begin
    FMultiListenIndex := -1;
    inherited ThreadAttach;
    for I := 0 to FMultiListenSockets.Count -1 do begin
        LItem := FMultiListenSockets[I];
        if (LItem.HSocket <> INVALID_SOCKET) then
            WSocket_WSAASyncSelect(
                                  {$IFDEF POSIX}
                                    LItem,
                                  {$ENDIF}
                                    LItem.HSocket,
                                    Handle, FMsg_WM_ASYNCSELECT,
                                    LItem.SelectEvent);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.ThreadDetach;
var
    I : Integer;
    LItem : TWSocketMultiListenItem;
begin
    FMultiListenIndex := -1;
    if ThreadID = IcsGetCurrentThreadID then begin // not thread-safe
        for I := 0 to FMultiListenSockets.Count -1 do begin
            LItem := FMultiListenSockets[I];
            if (LItem.HSocket <> INVALID_SOCKET) then
                WSocket_WSAASyncSelect(
                                      {$IFDEF POSIX}
                                        LItem,
                                      {$ENDIF}
                                        LItem.HSocket,
                                        Handle, 0, 0);
        end;
    end;
    inherited ThreadDetach;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.TriggerClientConnect(
    Client : TWSocketClient; Error : Word);
begin
    inherited TriggerClientConnect(Client, Error);
    { Finally reset the MultiListenIndex just to avoid bad component use }
    //FMultiListenIndex := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomMultiListenWSocketServer.WMASyncSelect(var msg: TMessage);
var
    Check   : Word;
    ParamLo : Word;
    AItem   : TWSocketMultiListenItem;
begin
    if msg.wParam = WPARAM(FHSocket) then begin
        FMultiListenIndex := -1;

        if FPaused then
          Exit;

        ParamLo := LoWord(msg.lParam);
        Check := ParamLo and FD_ACCEPT;
        if Check <> 0 then begin
            FSelectMessage := FD_ACCEPT;
            Do_FD_ACCEPT(msg);
        end;

        Check := ParamLo and FD_CLOSE;
        if Check <> 0 then begin
            FSelectMessage := FD_CLOSE;
            Do_FD_CLOSE(msg);
        end;

      {$IFDEF MSWINDOWS}
        if ParamLo and FD_ROUTING_INTERFACE_CHANGE <> 0 then begin
            FSelectMessage := FD_ROUTING_INTERFACE_CHANGE;
            Do_FD_ROUTING_INTERFACE_CHANGE(msg);
        end;

        if ParamLo and FD_ADDRESS_LIST_CHANGE <> 0 then begin
            FSelectMessage := FD_ADDRESS_LIST_CHANGE;
            Do_FD_ADDRESS_LIST_CHANGE(msg);
        end;
      {$ENDIF}

        FSelectMessage := 0;

    end
    else begin
        FMultiListenIndex := FMultiListenSockets.FindItemIndex(msg.wParam);
        if FMultiListenIndex = -1 then
            Exit;
        AItem := FMultiListenSockets[FMultiListenIndex];

        if AItem.Paused then
          Exit;

        ParamLo := LoWord(msg.lParam);

        Check := ParamLo and FD_ACCEPT;
        if Check <> 0 then
            Do_FD_ACCEPT(msg);

        Check := ParamLo and FD_CLOSE;
        if Check <> 0 then
            Ml_Do_FD_CLOSE(AItem, msg);

      {$IFDEF MSWINDOWS}
        if ParamLo and FD_ROUTING_INTERFACE_CHANGE <> 0 then begin
            FSelectMessage := FD_ROUTING_INTERFACE_CHANGE;
            Do_FD_ROUTING_INTERFACE_CHANGE(msg);
        end;

        if ParamLo and FD_ADDRESS_LIST_CHANGE <> 0 then begin
            FSelectMessage := FD_ADDRESS_LIST_CHANGE;
            Do_FD_ADDRESS_LIST_CHANGE(msg);
        end;
      {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TWSocketMultiListenItem }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$IFDEF POSIX}
{ Impl. IIcsEventSource }
function TWSocketMultiListenItem.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem._AddRef: Integer;
begin
  Result := -1;  // no ref count
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem._Release: Integer;
begin
  Result := -1; // no ref count
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetEventMask: LongWord;
begin
    Result := FPxEventMask;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetEventMask(const AValue: LongWord);
begin
    FPxEventMask := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetNotifyMessageID: UINT;
begin
    Result := FPxEventMessageID;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetNotifyMessageID(const AValue: UINT);
begin
    FPxEventMessageID := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetNotifyWindow: HWND;
begin
    Result := FPxEventWindow;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetNotifyWindow(const AValue: HWND);
begin
    FPxEventWindow := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetEventState: TIcsAsyncEventState;
begin
    Result := FPxEventState;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetEventState(const AValue: TIcsAsyncEventState);
begin
    FPxEventState := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetFileDescriptor: Integer;
begin
    Result := FPxFileDescriptor;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetFileDescriptor(const AValue: Integer);
begin
    FPxFileDescriptor := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetObject: TObject;
begin
    Result := Self;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetObjectID: NativeInt;
begin
    Result := FPxObjectID;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF POSIX IIcsEventSource}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure TWSocketMultiListenItem.AssignDefaults;
begin
    FHSocket            := INVALID_SOCKET;
    FPortNum            := 0;
    FState              := wsClosed;
    FPaused             := FALSE;
    FCloseInvoked       := FALSE;
    FillChar(Fsin, SizeOf(Fsin), 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.Close;
begin
    OwnerServer.MlClose(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TWSocketMultiListenItem.Create(Collection: TCollection);
begin
    inherited Create(Collection);
    FListenBackLog := 5;
    FSocketFamily := DefaultSocketFamily;
    FOldSocketFamily := FSocketFamily;
    AssignDefaults;
{$IFDEF POSIX}
    FPxObjectID := WSocketGenerateObjectID;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TWSocketMultiListenItem.Destroy;
begin
    if (FState <> wsInvalidState) and (FState <> wsClosed) then
        OwnerServer.MlClose(Self);
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetAddrResolved: string;
begin
    if Fsin.sin6_family = AF_INET6 then
        Result := WSocketIPv6ToStr(@Fsin)
    else
        Result := WSocketIPv4ToStr(PInteger(@PSockAddr(@Fsin)^.sin_addr)^);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.GetCloseInvoked: Boolean;
begin
    Result := FCloseInvoked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.Listen;
begin
    OwnerServer.MlListen(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.OwnerServer: TCustomMultiListenWSocketServer;
begin
    Result := TWSocketMultiListenCollection(Collection).Owner;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.Pause: Boolean;
begin
    OwnerServer.MlPause(Self);
    Result := FPaused;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.Resume: Boolean;
begin
    OwnerServer.MlResume(Self);
    Result := not FPaused;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetAddr(const Value: string);
begin
    OwnerServer.MlSetAddr(FAddr, FSocketFamily, FOldSocketFamily, Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetCloseInvoked(const AValue: Boolean);
begin
    FCloseInvoked := AValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenItem.SetSocketFamily(const Value: TSocketFamily);
begin
    OwnerServer.MlSetSocketFamily(FSocketFamily, FOldSocketFamily, Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.SetAddressListChangeNotification: Boolean;
{$IFDEF MSWINDOWS}
var
    LBytesRcvd : LongWord;
begin
    if FHSocket <> INVALID_SOCKET then begin
        if FSelectEvent and FD_ADDRESS_LIST_CHANGE = 0 then begin
            FSelectEvent := FSelectEvent or FD_ADDRESS_LIST_CHANGE;
            Result := WSocket_WSAASyncSelect(FHSocket,
                                             OwnerServer.Handle,
                                             OwnerServer.FMsg_WM_ASYNCSELECT,
                                             FSelectEvent) <> SOCKET_ERROR;
        end
        else
            Result := True;
        if Result then
            Result := (WSocket_WSAIoctl(FHSocket, SIO_ADDRESS_LIST_CHANGE, nil, 0,
                                        nil, 0, LBytesRcvd, nil, nil) <> SOCKET_ERROR) or
                      (WSocket_WSAGetLastError = WSAEWOULDBLOCK);
    end
    else
        Result := False;
{$ENDIF}
{$IFDEF POSIX}
begin
    Result := False;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenItem.SetRoutingInterfaceChangeNotification: Boolean;
{$IFDEF MSWINDOWS}
var
    LBytesRcvd : LongWord;
begin
    if FHSocket <> INVALID_SOCKET then begin
        if FSelectEvent and FD_ROUTING_INTERFACE_CHANGE = 0 then begin
            FSelectEvent := FSelectEvent or FD_ROUTING_INTERFACE_CHANGE;
            Result := WSocket_WSAASyncSelect(FHSocket,
                                             OwnerServer.Handle,
                                             OwnerServer.FMsg_WM_ASYNCSELECT,
                                             FSelectEvent) <> SOCKET_ERROR;
        end
        else
            Result := True;
        if Result then
            Result := (WSocket_WSAIoctl(FHSocket, SIO_ROUTING_INTERFACE_CHANGE,
                                        @Fsin, SizeOfAddr(Fsin), nil, 0, LBytesRcvd,
                                        nil, nil) <> SOCKET_ERROR) or
                      (WSocket_WSAGetLastError = WSAEWOULDBLOCK);
    end
    else
        Result := False;
{$ENDIF}
{$IFDEF POSIX}
begin
    Result := False;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TWSocketMultiListenCollection }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function TWSocketMultiListenCollection.Add: TWSocketMultiListenItem;
begin
    Result := TWSocketMultiListenItem(inherited Add);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TWSocketMultiListenCollection.Create(AOwner: TPersistent;
    AItemClass: TWSocketMultiListenItemClass);
begin
    inherited Create(AOwner, AItemClass);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenCollection.FindItemHandle(
    const AHSocket: TSocket): TWSocketMultiListenItem;
var
    I: Integer;
begin
    for I := 0 to Count -1 do
    begin
      Result := Items[I];
      if Result.FHSocket = AHSocket then
          Exit;
    end;
    Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenCollection.FindItemIndex(
    const AHSocket: TSocket): Integer;
begin
    for Result := 0 to Count -1 do
    begin
      if Items[Result].FHSocket = AHSocket then
          Exit;
    end;
    Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenCollection.FindItemID(ID: Integer): TWSocketMultiListenItem;
begin
    Result := TWSocketMultiListenItem(inherited FindItemID(ID));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenCollection.GetItem(Index: Integer): TWSocketMultiListenItem;
begin
    Result := TWSocketMultiListenItem(inherited GetItem(Index));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenCollection.Insert(
  Index: Integer): TWSocketMultiListenItem;
begin
    Result := TWSocketMultiListenItem(inherited Insert(Index));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketMultiListenCollection.Owner: TCustomMultiListenWSocketServer;
begin
    Result := TCustomMultiListenWSocketServer(GetOwner);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketMultiListenCollection.SetItem(Index: Integer;
  Value: TWSocketMultiListenItem);
begin
    inherited SetItem(Index, Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{*                                                                           *}
{*                            TWSocketClient                                 *}
{*                                                                           *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketClient.StartConnection;
begin
    if Length(FBanner) > 0 then
        SendStr(FBanner + FLineEnd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Triggered when socket is closed. Need to inform server socket to update   }
{ client list and trigger client disconnect event.                          }
procedure TWSocketClient.TriggerSessionClosed(ErrCode : Word);
var
    PIdRec : PClientIdRec;
begin
    if not FSessionClosedFlag then begin
        FSessionClosedFlag := TRUE;
        if Assigned(FServer) then begin
            New(PIdRec);
            PIdRec^.PClient := Self;
            PIdRec^.CliId   := FCliId;
            if NOT PostMessage(Server.Handle, Server.FMsg_WM_CLIENT_CLOSED,
                               WPARAM(ErrCode), LPARAM(PIdRec))
            then
                System.Dispose(PIdRec);
        end;
        inherited TriggerSessionClosed(ErrCode);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This override base class GetPeerAddr. It return cached value.             }
function TWSocketClient.GetPeerAddr: String;
begin
    Result := FPeerAddr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This override base class GetPeerPort. It return cached value.             }
function TWSocketClient.GetPeerPort: String;
begin
    Result := FPeerPort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Override base class. Dup is called when a client is assigned to a         }
{ TWSocket. Assigning HSocket property will call Dup.                       }
procedure TWSocketClient.Dup(newHSocket : TSocket);
begin
    inherited Dup(newHSocket);
    { Cache PeerAddr value }
    FPeerAddr := inherited GetPeerAddr;
    FPeerPort := inherited GetPeerPort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF USE_SSL}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslWSocketServer.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    // Server socket doesn't use SSL to listen for clients
    FSslEnable       := TRUE;
    Port             := '443';
    Proto            := 'tcp';
    Addr             := '0.0.0.0';
    SslMode          := sslModeServer;
    FIcsHosts        := TIcsHostCollection.Create(self);            { V8.45 }
    FRootCAX509      := TX509Base.Create(self);                     { V8.46 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslWSocketServer.Destroy;                                 { V8.45 }
begin
    if Assigned(FIcsHosts) then begin
        FIcsHosts.Free;
        FIcsHosts := nil;
    end;
    if Assigned(FRootCAX509) then begin
        FRootCAX509.Free;
        FRootCAX509 := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslWSocketServer.MultiListenItemClass: TWSocketMultiListenItemClass;
begin
    Result := TSslWSocketMultiListenItem;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWSocketServer.TriggerClientConnect(
    Client : TWSocketClient; Error : Word);
var
    I: Integer;
begin
   { V8.45 set SslEnable before event handler, so it can be used there }
    if FMultiListenIndex = -1 then
        Client.SslEnable := FSslEnable
    else begin
        Assert(MultiListenIndex < MultiListenSockets.Count);
        Client.SslEnable := TSslWSocketMultiListenItem(
                  MultiListenSockets[FMultiListenIndex]).SslEnable;
    end;

 { V8.45 keep server binding information for client }
    Client.FMultiListenIdx := FMultiListenIndex;
    if FMultiListenIndex = -1 then begin
        Client.FServerAddr := GetXAddr;
        Client.FServerPort := GetXPort;
    end
    else begin
        with MultiListenSockets [FMultiListenIndex] do begin
            Client.FServerAddr := Addr ;
            Client.FServerPort := Port ;
        end;
    end ;

  { V8.45 check binding for IcsHost - may be changed later if SNI or Host: header checked }
  { beware multiple hosts may have the same binding }
    if FIcsHosts.Count > 0 then begin
        for I := 0 to FIcsHosts.Count - 1 do begin
            if NOT (FIcsHosts [I].HostEnabled) then continue;
            if (FIcsHosts [I].FBindIdxNone = FMultiListenIndex) or
               (FIcsHosts [I].FBindIdxSsl = FMultiListenIndex) or
                 (FIcsHosts [I].FBindIdx2None = FMultiListenIndex) or
                   (FIcsHosts [I].FBindIdx2Ssl = FMultiListenIndex) then begin
                Client.FIcsHostIdx := I;
                Client.FHostTag := FIcsHosts [I].HostTag;
                Break;
            end;
        end;
    end;

    inherited TriggerClientConnect(Client, Error);
    { The event handler may have closed the connection }
    { The event handler may also have started the SSL }
    if (Error <> 0) or (Client.State <> wsConnected) or
       (Client.SslState > sslNone) then
        Exit;

    if Client.SslEnable then begin
        Client.SslMode                  := FSslMode;
        Client.SslAcceptableHosts       := FSslAcceptableHosts;
        Client.SslContext               := FSslContext;
        Client.OnSslVerifyPeer          := OnSslVerifyPeer;
        Client.OnSslSetSessionIDContext := OnSslSetSessionIDContext;
        Client.OnSslSvrNewSession       := OnSslSvrNewSession;
        Client.OnSslSvrGetSession       := OnSslSvrGetSession;
        Client.OnSslHandshakeDone       := OnSslHandshakeDone;
        Client.OnSslServerName          := OnSslServerName;   { V8.07 }
        try
            if Client.SslMode = sslModeClient then
                Client.StartSslHandshake
            else
                Client.AcceptSslHandshake;
        except
            on E: Exception do begin                            // AG 12/18/05
                Client.SslEnable := False;
                Client.Abort;
                { Don't abort silently }
                Client.HandleBackGroundException(E);            // AG 12/18/05
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslWSocketServer.GetIcsHosts: TIcsHostCollection;                   { V8.45 }
begin
    Result := FIcsHosts;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWSocketServer.SetIcsHosts(const Value: TIcsHostCollection);   { V8.45 }
begin
    FIcsHosts := Value;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ are we listening on this binding already }
function TSslWSocketServer.FindBinding(const MAddr: String;
                                MPort: Integer; var MIndex: Integer): boolean;      { V8.45 }
var
    J: integer;
begin
    Result := False;
    if (Addr = MAddr) and (Port = IntToStr(MPort)) then begin
        Result := True;
        MIndex := -1;
        Exit;
    end;
    if MultiListenSockets.Count = 0 then Exit;
    for J := 0 to MultiListenSockets.Count -1 do begin
        if (MultiListenSockets[J].Addr = MAddr) and
                     (MultiListenSockets[J].Port = IntToStr(MPort)) then begin
            Result := True;
            MIndex := J;
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
 { V8.45 if IcsHostCollection has been specified, use it to define all
   bindings and SSL contexts, including SSL certificates and security,
   with SSL Server Name Indication (SNI) used to select the correct host.
   HTTP descendants of SocketServer might check the Host: header for
   non=SSL connections }

procedure TSslWSocketServer.ValidateHosts;                            { V8.45 }
var
    I, FirstSsl: integer;
    FirstHost: Boolean;

    procedure AddBinding(const MAddr: String; MPort: Integer;
                                    SslFlag: Boolean; var MIndex: Integer);
    var
        SockFamily: TSocketFamily;
        ListenItem: TSslWSocketMultiListenItem;
    begin
        if (MAddr = '') OR (NOT WSocketIsIPEx (MAddr, SockFamily)) then begin
            raise ESocketException.Create('Host #' + IntToStr(I) +
                            ', Invalid host listen IP address: ' + MAddr);
            exit ;
        end ;
        if FirstHost then begin    { first is not a multi-listen }
            FirstHost := False;
            Addr := MAddr;
            Port := IntToStr(MPort);
            SocketFamily := SockFamily;
            SslEnable := SslFlag;
            MIndex := -1;
        end
        else begin
            { ignore duplicates, but keep binding index }
            if NOT FindBinding(MAddr, MPort, MIndex) then begin
                MultiListenSockets.Add;
                MIndex := MultiListenSockets.Count - 1;
                ListenItem := MultiListenSockets [MIndex] as TSslWSocketMultiListenItem;
                ListenItem.Addr := MAddr;
                ListenItem.Port := IntToStr(MPort);
                ListenItem.SocketFamily := SockFamily;
                ListenItem.SslEnable := SslFlag;
            end;
        end;
    end;

begin
    if FIcsHosts.Count = 0 then Exit;
    FirstSsl := -1;
    FirstHost := True;

 { keep Root CA to validate SSL certificate chains }
    if FRootCA <> '' then begin    { V8.46 }
        if (Pos(PEM_STRING_HDR_BEGIN, FRootCA) > 0) then
            FRootCAX509.LoadCATrustFromString(FRootCA)
        else
            FRootCAX509.LoadCATrustFromPemFile(FRootCA);
    end;
    MultiListenSockets.Clear;
    for I := 0 to FIcsHosts.Count - 1 do begin
         with FIcsHosts [I] do begin
            FCertInfo := '';
            FCertErrs := '';
            FBindIdxNone := -2;  { bindings are -1 to +x }
            FBindIdx2None := -2;
            FBindIdxSsl := -2;
            FBindIdx2Ssl := -2;
            if NOT (FHostEnabled) then continue;

         { create up to four bindings for host, IPv4, IPv6, non-SSL, SSL }
            if (FBindNonPort = 0) and (BindSslPort = 0) then continue;
            if (FBindNonPort > 0) then begin
                AddBinding(FBindIpAddr, FBindNonPort, False, FBindIdxNone);
                if FBindIpAddr2 <> '' then
                    AddBinding(FBindIpAddr2, FBindNonPort, False, FBindIdx2None);
            end;
            if (FBindSslPort > 0) then begin
                if (FSslCert = '') then begin
                    raise ESocketException.Create('Host #' + IntToStr(I) +
                         ', SSL certificate can not be blank');
                    exit ;
                end;
                AddBinding(FBindIpAddr, FBindSslPort, True, FBindIdxSsl);
                if FBindIpAddr2 <> '' then
                    AddBinding(FBindIpAddr2, FBindSslPort, True, FBindIdx2Ssl);
            end;

     { if using SSL, set-up context with server certificates }
            if FBindSslPort > 0 then begin
                if NOT Assigned(SslCtx) then
                    SslCtx := TSslContext.Create(Self);
                if FirstSsl < 0 then FirstSsl := I;
                SslCtx.SslVersionMethod := sslBestVer_SERVER;
                SslCtx.SslMinVersion := sslVerTLS1;
                SslCtx.SslMaxVersion := sslVerMax;
                SslCtx.SslOptions := [sslOpt_CIPHER_SERVER_PREFERENCE,
                        sslOpt_NO_SESSION_RESUMPTION_ON_RENEGOTIATION,
                        sslOpt_NO_COMPRESSION,
                        sslOpt_SINGLE_DH_USE, SslOpt_SINGLE_ECDH_USE ];
                SslCtx.SslCipherList := sslCiphersMozillaSrvInter ;  // excludes MSIE6/XP no SSLv3

             { set SSL security choices before loading certificates, which may then fail }
                case FSslSrvSecurity of
                  sslSrvSecNone: begin                { all protocols and ciphers, any key lenghts }
                      SslCtx.SslMinVersion := sslVerSSL3;
                      SslCtx.SslCipherList := 'ALL';
                      SslCtx.SslSecLevel := sslSecLevelAny;
                  end;
                  sslSrvSecSsl3: begin                { SSL3 only, all ciphers, any key lenghts, MD5 }
                      SslCtx.SslMinVersion := sslVerSSL3;
                      SslCtx.SslMaxVersion := sslVerSSL3;
                      SslCtx.SslCipherList := 'ALL';
                      SslCtx.SslSecLevel := sslSecLevelAny;
                  end;
                  sslSrvSecBack: begin                { TLS1 or later, backward ciphers, RSA/DH keys=>1024, ECC=>160, no MD5, SHA1 }
                      SslCtx.SslCipherList := sslCiphersMozillaSrvBack;
                      SslCtx.SslSecLevel := sslSecLevel80bits;
                  end;
                  sslSrvSecInter: begin               { TLS1 or later, intermediate ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                      SslCtx.SslCipherList := sslCiphersMozillaSrvInter;
                      SslCtx.SslSecLevel := sslSecLevel112bits;  // Dec 2016  keys=>2048, ECC=>224, no RC4, no SSL3, no SHA1 certs
                  end;
                  sslSrvSecInterFS: begin             { TLS1 or later, intermediate FS ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                      SslCtx.SslCipherList := sslCiphersMozillaSrvInterFS;
                      SslCtx.SslSecLevel := sslSecLevel112bits;
                  end;
                  sslSrvSecHigh: begin                 { TLS1.2 or later, high ciphers, RSA/DH keys=>2048, ECC=>224, no RC4, no SHA1 certs }
                      SslCtx.SslMinVersion := sslVerTLS1_2;
                      SslCtx.SslCipherList := sslCiphersMozillaSrvHigh;
                      SslCtx.SslSecLevel := sslSecLevel112bits;
                  end;
                  sslSrvSecHigh128: begin               { TLS1.2 or later, high ciphers, RSA/DH keys=>3072, ECC=>256, FS forced }
                      SslCtx.SslMinVersion := sslVerTLS1_2;
                      SslCtx.SslCipherList := sslCiphersMozillaSrvHigh;
                      SslCtx.SslSecLevel :=sslSecLevel128bits;
                  end;
                  sslSrvSecHigh192: begin                { TLS1.2 or later, high ciphers, RSA/DH keys=>7680, ECC=>384, FS forced }
                      SslCtx.SslMinVersion := sslVerTLS1_2;
                      SslCtx.SslCipherList := sslCiphersMozillaSrvHigh;
                      SslCtx.SslSecLevel := sslSecLevel192bits;
                  end;
                end;
                SslCtx.SslECDHMethod := sslECDHAuto;

            { Enables OpenSsl's internal session caching }
                SslCtx.SslSessionCacheModes := [sslSESS_CACHE_SERVER];

            { load certificate, private key and optional intermediates, that may all be
              in the same PEM or PFX bundle file or seperate files, or may be base64 text }
                if (Pos(PEM_STRING_HDR_BEGIN, FSslCert) > 0) then
                    SslCtx.SslCertX509.LoadFromText(FSslCert, croTry, croTry, FSslPassword)
                else
                    SslCtx.SslCertX509.LoadFromFile(FSslCert, croTry, croTry, FSslPassword);
                if NOT SslCtx.SslCertX509.IsPKeyLoaded then begin
                    if (FSslKey = '') then begin
                        raise ESocketException.Create('Host #' + IntToStr(I) +
                                      ', SSL private key can not be blank for ' + FSslCert);
                        exit ;
                    end;
                    if (Pos(PEM_STRING_HDR_BEGIN, FSslKey) > 0) then
                       SslCtx.SslCertX509.PrivateKeyLoadFromText(FSslKey, FSslPassword)
                    else
                       SslCtx.SslCertX509.PrivateKeyLoadFromPemFile(FSslKey, FSslPassword);
                end ;
                if (NOT SslCtx.SslCertX509.IsInterLoaded) and (FSslInter <> '') then begin
                    if (Pos(PEM_STRING_HDR_BEGIN, FSslInter) > 0) then
                        SslCtx.SslCertX509.LoadIntersFromString(FSslInter)
                    else
                        SslCtx.SslCertX509.LoadIntersFromPemFile(FSslInter);
                end ;
                SslCtx.SslVerifyPeer := false;  // don't expect remote client to send us certificate
                SslCtx.SslSessionTimeout := 300; //sec
                SslCtx.SslDefaultSessionIDContext := 'AnyStringForSessionCaching';
                if FDHParams <> '' then begin
                    if (Pos(PEM_STRING_HDR_BEGIN, FDHParams) > 0) then
                        SslCtx.SslDHParamLines.Text := FDHParams
                    else
                        SslCtx.SslDHParamFile := FDHParams;
                end;

             { validate SSL certificate chain, helps to ensure server will work! }
                if NOT SslCtx.SslCertX509.IsCertLoaded then
                begin
                    raise ESocketException.Create('Host #' + IntToStr(I) +
                         ', SSL certificate not loaded - ' + FSslCert);
                    exit ;
                end;
                SslCtx.SslCertX509.X509CATrust := FRootCAX509.X509CATrust;
                FCertDomains := IcsUnwrapNames (SslCtx.SslCertX509.SubAltNameDNS);
                FCertExiry := SslCtx.SslCertX509.ValidNotAfter;
                FCertValRes := SslCtx.SslCertX509.ValidateCertChain(HostNames.Text, FCertInfo, FCertErrs);
                if FCertValRes = chainOK then
                    FCertErrs := 'Chain Validated OK'
                else begin
                    if FCertValRes = chainWarn then
                        FCertErrs := 'Chain Warning - ' + FCertErrs
                    else begin
                        FCertErrs := 'Chain Failed - ' + FCertErrs;
                        raise ESocketException.Create('Host #' + IntToStr(I) +
                            ', SSL certificate errors - ' + FCertErrs);
                        exit ;
                    end;
                end;
                SslCtx.InitContext;
            end;
        end;
    end;

  { set server context as first host with SSL }
    if FirstSsl >= 0 then FSslContext := FIcsHosts [FirstSsl].SslCtx;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWSocketServer.Listen;                               { V8.45 }
begin
    ValidateHosts;
    inherited Listen;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslWSocketClient.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FIcsHostIdx := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslWSocketClient.StartConnection;
begin
    inherited StartConnection;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ SSL client has sent a host name using SNI, look up IcsHost }
procedure TSslWSocketClient.TriggerSslServerName(var Ctx: TSslContext; var ErrCode: TTlsExtError);  { V8.45 }
var
    I: Integer;
begin
    inherited TriggerSslServerName(Ctx, ErrCode);

  { if event has not set an SslContext, look for IcsHost instead }
    if NOT Assigned(Ctx) then begin
         with FServer as TSslWSocketServer do begin
            if FIcsHosts.Count > 0 then begin
               for I := 0 to FIcsHosts.Count - 1 do begin
                    if NOT (FIcsHosts [I].HostEnabled) then continue;
                    if (FIcsHosts [I].FBindIdxSsl <> Self.FMultiListenIdx) and
                         (FIcsHosts [I].FBindIdx2Ssl <> Self.FMultiListenIdx) then continue;
                    if FIcsHosts [I].SslCtx.SslCertX509.PostConnectionCheck(Self.FSslServerName) then begin
                        Self.FIcsHostIdx := I;
                        Self.FHostTag := FIcsHosts [I].HostTag;
                        Ctx := FIcsHosts [I].SslCtx;
                        {$IFNDEF NO_DEBUG_LOG}
                            if CheckLogOptions(loSslInfo) then
                                DebugLog(loSslInfo, 'SNI Found "' + Self.FSslServerName +
                                                '" for IcsHost #' + IntToStr(I));
                        {$ENDIF}
                        break;
                    end;
                end;
            end;
        end;
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSslWSocketMultiListenItem }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslWSocketMultiListenItem.Create(Collection: TCollection);
begin
    inherited Create(Collection);
    FSslEnable := TRUE;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsHost }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsHost.Create(Collection: TCollection);
begin
    inherited;
    FHostNames := TStringList.Create;
    FHostNames.Add ('*');
    FHostTag := 'HostTag';
    FBindIpAddr := ICS_ANY_HOST_V4;
    FBindNonPort := 0;
    FBindSslPort := 0;
    FSslSrvSecurity := sslSrvSecBack;
    FHostEnabled := True;
    SslCtx := Nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsHost.Destroy;
begin
    FreeAndNil (SslCtx);
    FreeAndNil (FHostNames);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsHost.GetDisplayName: string;
begin
    Result := FHostTag + ' - ' + FBindIpAddr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsHost.SetHostNames(Value : TStrings);
begin
    FHostNames.Assign(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsHost.GetHostNameTot: integer;
begin
    if Assigned (FHostNames) then
        Result := FHostNames.Count
     else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TIcsHosts }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsHostCollection.Create(Owner: TPersistent);
begin
  FOwner := Owner;
  inherited Create(TIcsHost);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsHostCollection.GetItem(Index: Integer): TIcsHost;
begin
  Result := TIcsHost(inherited GetItem(Index));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsHostCollection.SetItem(Index: Integer; Value: TIcsHost);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsHostCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$ENDIF} // USE_SSL

end.

