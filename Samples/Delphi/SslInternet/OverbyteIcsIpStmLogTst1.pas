{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  IP Log Streaming Component - Test Application
Creation:     Aug 2007
Updated:      Jan 2019
Version:      8.60
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list ics-ssl@elists.org
Legal issues: Copyright (C) 2019 by Angus Robertson, Magenta Systems Ltd,
              Croydon, England. delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/

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


A client and server test application for the TIcsIpStrmLog IP Log component.
Note the components are created dynamically to avoid needing to install
them in the component pallete.


22nd Nov 2006 - baseline

18th August 2007 - 1.1 - using OverbyteIcsFtpSrvT instead of OverbyteIcsLibrary
UDP receive packets may be from multiple hosts, always keep IP

5th August 2008 - 1.2 - made compatible with ICS V7 and Delphi 2009
Note - only sends and receives ANSI text

20th August 2009 - 1.3 - fixed problem with MaxSockets being reported as closed
in the event when only one was open, tested with Delphi 2010

9th August 2010 - 1.4 - removed cast warnings with Delphi 2009 and later

7th July 2014 - 2.0 - ICS 8 and later, using new ICS ping
                      added IPv6 and SSL support, including server certificate checking
                      added host name support for UDP and TCP client with DNS lookup
                      save all settings in local INI file
                      added send a file stream

13th July 2015 - 2.2 - added better SSL handshake error reporting
                       added lineendCRLF, only support FF as lineend if using CR
                       added Debug Info button for ICS info level logging
                       added SSL Server DH Params, set ECDHCurves, both for ECDH ciphers
                       Note OpenSSL no longer support dhparam512, minimum is 768 bits.

23rd Oct 2015  - 2.3 - better SSL client and server certificate reporting

8th July 2016  - 2.4 - removed TBufferedFileStream
                       added SrvTimeoutSecs to close idle server sessions
                       Report session length and data xmit/recv before closing

23rd Nov 2016  - 2.5 - fixed bug reporting data sent after close
                       use sslRootCACertsBundle if no file
                       check not overloading send buffer
                       increased default MaxSendBuffer size to 64K
                       only works with latest digitally signed OpenSSL DLLs

7th March 2017 - 2.6 - check server private key and certificate match
                       better validation of list of remote hosts
                       SSL contenxt logging enabled
                       new way to load SSL certificate bundles supports
                         PFX files as well as PEM, validate chain before
                         initialising context rather than after, report
                         chain here

5th May 2017   - 2.7 - Added localhost to local IP address list
                       Ignore hosts suppressed with *

22nd June 2018 - 2.7 - support TLSv1.3
                       Added SslCliSecurity to set client security, pending
                          server version (needs IcsHosts)

22 Feb 2019 - V8.60 - Adapted for ICS, separate tab for settings, allow to
                        order X509 SSL certificates.

                     WARNING NOT FINISHED YET!!!   


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsIpStmLogTst1;

{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{ If you use Delphi 7, you may wants to disable warnings for unsage type,   }
{ unsafe code and unsafe typecast in the project options. Those warning are }
{ intended for .NET programs. You may also want to turn off deprecated      }
{ symbol and platform symbol warnings.                                      }

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}



interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, OverbyteIcsIniFiles, Buttons, Dialogs, TypInfo, ComCtrls,
  OverbyteIcsWSocket, OverbyteIcsWinsock, OverbyteIcsLIBEAY,
  OverbyteIcsSSLEAY, OverbyteIcsSslX509Utils, OverbyteIcsSslSessionCache,
  OverbyteIcsUtils, OverbyteIcsLogger, OverbyteIcsStreams,
  OverbyteIcsIpStreamLog, OverbyteIcsSslX509Certs, OverbyteIcsWndControl;

type
  TIpLogForm = class(TForm)
    DataTimer: TTimer;
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    SheetSettings: TTabSheet;
    BoxClient: TGroupBox;
    Label5: TLabel;
    Label2: TLabel;
    RemoteHosts: TMemo;
    RemotePort: TComboBox;
    BoxServer: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label11: TLabel;
    Label10: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    ServerPort: TComboBox;
    MaxSockets: TEdit;
    SslServCert: TEdit;
    SslCertKey: TEdit;
    SslCACerts: TEdit;
    SslDHParams: TEdit;
    BoxLocalAddr: TGroupBox;
    Label9: TLabel;
    Protocol: TRadioGroup;
    LocalAddr: TComboBox;
    UseSSL: TCheckBox;
    LogErrors: TCheckBox;
    LogInfo: TCheckBox;
    SheetOperation: TTabSheet;
    PanelBottom: TPanel;
    doStop: TButton;
    doLocal: TButton;
    doClient: TButton;
    doExit: TButton;
    doServer: TButton;
    doClear: TButton;
    doCliSendFile: TButton;
    SendFileName: TEdit;
    SelectFile: TBitBtn;
    doSrvSendFile: TButton;
    Label1: TLabel;
    PanelSplitter: TPanel;
    LabelSendClient: TLabel;
    LabelSendServer: TLabel;
    LogWin: TMemo;
    DataWin: TMemo;
    VerifyCertMode: TRadioGroup;
    RevokeCheck: TCheckBox;
    Label19: TLabel;
    SslCertAuth: TEdit;
    PingRemote: TCheckBox;
    Label16: TLabel;
    SrvTimeout: TEdit;
    SslX509Certs1: TSslX509Certs;
    SslAvlSessionCache: TSslAvlSessionCache;
    SslCliSec: TRadioGroup;
    ReportChain: TCheckBox;
    SslSrvSec: TRadioGroup;
    Label4: TLabel;
    LocalPort: TEdit;
    SocketFamily: TRadioGroup;
    BoxSampleData: TGroupBox;
    DataServer: TCheckBox;
    DataClient: TCheckBox;
    HeavyTraffic: TCheckBox;
    RawData: TCheckBox;
    Label3: TLabel;
    DataGap: TEdit;
    ServerHost: TEdit;
    Label8: TLabel;
    Label12: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure DataTimerTimer(Sender: TObject);
    procedure doLocalClick(Sender: TObject);
    procedure doClientClick(Sender: TObject);
    procedure doServerClick(Sender: TObject);
    procedure doStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LocalAddrChange(Sender: TObject);
    procedure SocketFamilyChange(Sender: TObject);
    procedure doClearClick(Sender: TObject);
    procedure doCliSendFileClick(Sender: TObject);
    procedure SelectFileClick(Sender: TObject);
    procedure doSrvSendFileClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    procedure SetButtons (Started: boolean) ;
    procedure LogRecvEvent (Sender: TObject; Socnr: integer; const Line: string) ;
    procedure LogProgEvent (Sender: TObject; Socnr: integer;
                              LogOption: TLogOption; const Msg: string);
    procedure LogChangeEvent (Sender: TObject; Socnr: integer;
                                                 LogState: TStrmLogState);
//    function SetSsl (client, server: boolean): boolean ;
  public
    { Public declarations }
  end;

const
    ProtoUdp = 0 ; ProtoTcp = 1 ;
    MyLogOptions: TLogOptions = [loDestEvent, loWsockErr, loSslErr] ;
    MyLogOptions2: TLogOptions = [loDestEvent, loWsockErr, loWsockInfo, loSslErr , loSslInfo] ;

var
    IpLogForm: TIpLogForm;
    IpLogClient: TIcsIpStrmLog ;
    IpLogServer: TIcsIpStrmLog ;
    CSerialNr: integer = 1 ;
    SSerialNr: integer = 1 ;
    FIniFileName: string;
    FCertificateDir: string ;
    FLocalFileStream: TFileStream ;

implementation

{$R *.dfm}

procedure TIpLogForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
    section, temp: string ;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do
    begin
        section := 'Main' ;
        if DataClient.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'DataClient_Checked', temp) ;
        WriteString (section, 'DataGap_Text', DataGap.Text) ;
        if DataServer.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'DataServer_Checked', temp) ;
        WriteString (section, 'FileName_Text', SendFileName.Text) ;
        if HeavyTraffic.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'HeavyTraffic_Checked', temp) ;
        WriteString (section, 'LocalAddr_Text', LocalAddr.Text) ;
        WriteString (section, 'LocalPort_Text', LocalPort.Text) ;
        WriteString (section, 'MaxSockets_Text', MaxSockets.Text) ;
        if PingRemote.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'PingRemote_Checked', temp) ;
        WriteInteger (section, 'Protocol_ItemIndex', Protocol.ItemIndex) ;
        WriteString (section, 'RemoteHosts_Lines', RemoteHosts.Lines.CommaText) ;
        WriteString (section, 'RemotePort_Text', RemotePort.Text) ;
        WriteString (section, 'ServerPort_Text', ServerPort.Text) ;
        WriteInteger (section, 'SocketFamily_ItemIndex', SocketFamily.ItemIndex) ;
        WriteString (section, 'SslServCert_Text', SslServCert.Text) ;
        WriteString (section, 'SslCertAuth_Text', SslCertAuth.Text) ;
        WriteString (section, 'SslCertKey_Text', SslCertKey.Text) ;
        WriteString (section, 'SslCACerts_Text', SslCACerts.Text) ;
        if UseSSL.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'UseSSL_Checked', temp) ;
        if RawData.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'RawData_Checked', temp) ;
        WriteInteger (section, 'VerifyCertMode_ItemIndex', VerifyCertMode.ItemIndex) ;
        if RevokeCheck.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'RevokeCheck_Checked', temp) ;
        if ReportChain.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'ReportChain_Checked', temp) ;
        if LogErrors.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'LogErrors_Checked', temp) ;
        if LogInfo.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'LogInfo_Checked', temp) ;
        WriteString (section, 'SslDHParams_Text', SslDHParams.Text) ;
        WriteString (section, 'SrvTimeout_Text', SrvTimeout.Text) ;
        WriteInteger (section, 'SslSrvSec_ItemIndex', SslSrvSec.ItemIndex) ;
        WriteInteger (section, 'SslCliSec_ItemIndex', SslCliSec.ItemIndex) ;

        WriteInteger ('Window', 'Top', Top);
        WriteInteger ('Window', 'Left', Left);
        WriteInteger ('Window', 'Width', Width);
        WriteInteger ('Window', 'Height', Height);
    end ;
    IniFile.UpdateFile;
    IniFile.Free;
end;

procedure TIpLogForm.FormCreate(Sender: TObject);
var
    SF: TSocketFamily;
    SL: TSslSrvSecurity;
    CL: TSslCliSecurity;
    IniFile : TIcsIniFile;
    section: string ;
begin
// see socket families and security levels
    SocketFamily.Items.Clear;
    for SF := Low (TSocketFamily) to High (TSocketFamily) do
        SocketFamily.Items.Add (SocketFamilyNames [SF]) ;
    SslSrvSec.Items.Clear;
    for SL := Low (TSslSrvSecurity) to High (TSslSrvSecurity) do
        SslSrvSec.Items.Add (SslSrvSecurityNames[SL]);
    SslCliSec.Items.Clear;
    for CL := Low(TSslCliSecurity) to High(TSslCliSecurity) do
         SslCliSec.Items.Add (SslCliSecurityNames[CL]);

// set local IPs
    try
        LocalAddr.Items.Assign (LocalIPList (sfIPv4, IPPROTO_TCP)) ;  // Wsocket function
    except
    end ;
    if IsIPv6ApiAvailable then
    begin
        try
            LocalAddr.Items.Assign (LocalIPList (sfIPv4, IPPROTO_TCP)) ;  // Wsocket function
            LocalAddr.Items.AddStrings (LocalIPList (sfIPv6, IPPROTO_TCP)) ;    // Wsocket function
        except
        end ;
        LocalAddr.Items.Insert (0, ICS_ANY_HOST_V6);
    end;
    LocalAddr.Items.Insert (0, ICS_ANY_HOST_V4);
    LocalAddr.Items.Insert (1, ICS_LOCAL_HOST_V4);  // May 2017

// get old settings
    FIniFileName := GetIcsIniFileName;
    FCertificateDir := ExtractFileDir (FIniFileName) + '\';
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do
    begin
        section := 'Main' ;
        if ReadString (section, 'DataClient_Checked', 'True') = 'True' then DataClient.Checked := true else DataClient.Checked := false ;
        DataGap.Text := ReadString (section, 'DataGap_Text', '1000') ;
        if ReadString (section, 'DataServer_Checked', 'False') = 'True' then DataServer.Checked := true else DataServer.Checked := false ;
        SendFileName.Text := ReadString (section, 'FileName_Text', '') ;
        if ReadString (section, 'HeavyTraffic_Checked', 'False') = 'True' then HeavyTraffic.Checked := true else HeavyTraffic.Checked := false ;
        LocalAddr.Text := ReadString (section, 'LocalAddr_Text', '0.0.0.0') ;
        LocalPort.Text := ReadString (section, 'LocalPort_Text', '25678') ;
        MaxSockets.Text := ReadString (section, 'MaxSockets_Text', '4') ;
        if ReadString (section, 'PingRemote_Checked', 'False') = 'True' then PingRemote.Checked := true else PingRemote.Checked := false ;
        Protocol.ItemIndex := ReadInteger (section, 'Protocol_ItemIndex', 0) ;
        RemoteHosts.Lines.CommaText := ReadString (section, 'RemoteHosts_Lines', '192.168.1.120') ;
        RemotePort.Text := ReadString (section, 'RemotePort_Text', '514') ;
        ServerPort.Text := ReadString (section, 'ServerPort_Text', '514') ;
        SocketFamily.ItemIndex := ReadInteger (section, 'SocketFamily_ItemIndex', 0) ;
        SslServCert.Text := ReadString (section, 'SslServCert_Text', 'iplog-cert.pem') ;
        SslCertAuth.Text := ReadString (section, 'SslCertAuth_Text', 'RootCaCertsBundle.pem') ;
        SslCertKey.Text := ReadString (section, 'SslCertKey_Text', 'iplog-prvkey.pem') ;
        SslCACerts.Text := ReadString (section, 'SslCACerts_Text', '') ;
        if ReadString (section, 'UseSSL_Checked', 'False') = 'True' then UseSSL.Checked := true else UseSSL.Checked := false ;
        if ReadString (section, 'RawData_Checked', 'False') = 'True' then RawData.Checked := true else RawData.Checked := false ;
        VerifyCertMode.ItemIndex := ReadInteger (section, 'VerifyCertMode_ItemIndex', 0) ;
        if ReadString (section, 'RevokeCheck_Checked', 'False') = 'True' then RevokeCheck.Checked := true else RevokeCheck.Checked := false ;
        if ReadString (section, 'ReportChain_Checked', 'False') = 'True' then ReportChain.Checked := true else ReportChain.Checked := false ;
        if ReadString (section, 'LogErrors_Checked', 'False') = 'True' then LogErrors.Checked := true else LogErrors.Checked := false ;
        if ReadString (section, 'LogInfo_Checked', 'False') = 'True' then LogInfo.Checked := true else LogInfo.Checked := false ;
        SslDHParams.Text := ReadString (section, 'SslDHParams_Text', 'dhparam2048.pem') ;
        SrvTimeout.Text := ReadString (section, 'SrvTimeout_Text', '300') ;
        SslSrvSec.ItemIndex := ReadInteger (section, 'SslSrvSec_ItemIndex', 0) ;
        SslCliSec.ItemIndex := ReadInteger (section, 'SslCliSec_ItemIndex', 0) ;

        Top := ReadInteger ('Window', 'Top', (Screen.Height - Height) div 2);
        Left := ReadInteger ('Window', 'Left', (Screen.Width - Width) div 2);
        Width := ReadInteger ('Window', 'Width', Width);
        Height := ReadInteger ('Window', 'Height', Height);
    end;
    IniFile.Free;

    SetButtons (false) ;
    LabelSendClient.Caption := 'Total Lines Sent to Server 0' ;
    LabelSendServer.Caption := 'Total Lines Sent by Server 0' ;

    IpLogServer := TIcsIpStrmLog.Create (Self) ;
    IpLogServer.Tag := 1 ;
    IpLogServer.onLogRecvEvent := LogRecvEvent ;
    IpLogServer.onLogProgEvent := LogProgEvent ;
    IpLogServer.onLogChangeEvent := LogChangeEvent ;
    IpLogServer.MaxSockets := 1 ;

    IpLogClient := TIcsIpStrmLog.Create (Self) ;
    IpLogClient.Tag := 2 ;
    IpLogClient.onLogRecvEvent := LogRecvEvent ;
    IpLogClient.onLogProgEvent := LogProgEvent ;
    IpLogClient.onLogChangeEvent := LogChangeEvent ;
    IpLogClient.RetryWaitSecs := 10 ;
    IpLogServer.MaxSockets := 1 ;

  // ignore OpenSSL 1.1.0 and later, or earlier
//    GSSLEAY_DLL_IgnoreNew := True;
//    GSSLEAY_DLL_IgnoreOld := True;    // Nov 2016 use latest OpenSSL
    GSSL_DLL_DIR := ExtractFilePath (ParamStr (0)) ;   // Nov 2016 from our directory
    GSSL_SignTest_Check := True;         // Nov 2016 check OpenSSL digitall signed
 //   GSSL_SignTest_Certificate := True;
   if SslSrvSec.ItemIndex <= 0 then SslSrvSec.ItemIndex := Ord(sslSrvSecDefault);
   if SslCliSec.ItemIndex <= 0 then SslCliSec.ItemIndex := Ord(sslCliSecDefault);
end;

procedure TIpLogForm.FormDestroy(Sender: TObject);
begin
    DataTimer.Enabled := false ;
    FreeAndNil (IpLogClient) ;
    FreeAndNil (IpLogServer) ;
end;

procedure TIpLogForm.FormResize(Sender: TObject);
begin
    DataWin.Width := (PanelSplitter.Width div 3);
    LogWin.Width := PanelSplitter.Width - DataWin.Width;
    DataWin.Left := LogWin.Width;
end;

procedure TIpLogForm.SetButtons (Started: boolean) ;
begin
    doStop.Enabled := Started ;
    doLocal.Enabled := NOT Started ;
    doClient.Enabled := NOT Started ;
    doServer.Enabled := NOT Started ;
    doSrvSendFile.Enabled := Started ;
    doCliSendFile.Enabled := Started ;
end ;

procedure TIpLogForm.SocketFamilyChange(Sender: TObject);
begin
    if SocketFamily.ItemIndex = Ord (sfAnyIPv4) then
        LocalAddr.Text := ICS_ANY_HOST_V4
    else if SocketFamily.ItemIndex = Ord (sfAnyIPv6) then
        LocalAddr.Text := ICS_ANY_HOST_V6 ;
end;

procedure TIpLogForm.LocalAddrChange(Sender: TObject);
var
    SF: TSocketFamily;
begin
    if NOT WSocketIsIPEx (LocalAddr.Text, SF) then exit;
    SocketFamily.ItemIndex := Ord (SF);
end;

procedure TIpLogForm.LogChangeEvent (Sender: TObject; Socnr: integer;
                                                 LogState: TStrmLogState);
var
    S, S2: string ;
begin
    case LogState of
        logstateNone: S2 := 'None' ;
        logstateStart: S2 := 'Starting' ;
        logstateHandshake: S2 := 'SSL Handshake' ;
        logstateOK: S2 := 'OK' ;
        logstateOKStream: S2 := 'OK Sending Stream' ;
        logstateStopping: S2 := 'Stopping' ;
    end ;
    // close file stream when state changes from logstateOKStream
    if (LogState <> logstateOKStream) and (Assigned (FLocalFileStream)) then
                    FreeAndNil (FLocalFileStream) ;
    S := TimeToStr (Time) ;
    if (Sender as TIcsIpStrmLog).Tag = 1 then
        S := S + ' S['
    else
        S := S + ' C[' ;
    S := S + IntToStr (Socnr) + '] State: ' + S2 ;
    LogWin.Lines.Add (S) ;
end ;

procedure TIpLogForm.LogProgEvent (Sender: TObject; Socnr: integer;
                                LogOption: TLogOption; const Msg: string);
var
    S: string ;
begin
    S := TimeToStr (Time) ;
    if (Sender as TIcsIpStrmLog).Tag = 1 then
        S := S + ' S['
    else
        S := S + ' C[' ;
    S := S + IntToStr (Socnr) + '] ' ;
    case LogOption of
        loWsockErr: S := S + 'WsockErr ' ;
        loWsockInfo: S := S + 'WsockInfo ' ;
        loSslErr: S := S + 'SslErr ' ;
        loSslInfo: S := S + 'SslInfo ' ;
    end;
    S := S  + Msg ;
    LogWin.Lines.Add (S) ;
end ;

procedure TIpLogForm.LogRecvEvent (Sender: TObject; Socnr: integer;
                                                        const Line: string) ;
var
    S: string ;
begin
    if (Sender as TIcsIpStrmLog).Tag = 1 then
        S := 'S['
    else
        S := 'C[' ;
     S := S + IntToStr (Socnr) + '] ' + Line ;
    DataWin.Lines.Add (S) ;
end ;

procedure TIpLogForm.doExitClick(Sender: TObject);
begin
    doStopClick (Self) ;
    Close ;
end;

procedure TIpLogForm.DataTimerTimer(Sender: TObject);
var
    Line: AnsiString ;    // 8 Aug 2010
    I, tot, old: integer ;
begin
    DataTimer.Enabled := false ;
    try
        DataTimer.Interval := atoi (DataGap.Text) ;
        if DataClient.Checked then
        begin
            tot := 1 ;
            if HeavyTraffic.Checked then tot := 50 ;
            if IpLogClient.AnyStateOK then
            begin
                old := IpLogClient.GetSendWaiting (0) ;
                if old > IpLogClient.MaxSendBuffer - 1000 then
                    LogWin.Lines.Add ('Client already waiting to send (bytes) ' + IcsIntToCStr (old)) ;
                for I := 1 to tot do
                begin
                    Line := AnsiString (TimeToStr (Time) +
                       ' Test Line of Data to Server, Serial ' + IntToStr (CSerialNr)) ;
                    if IpLogClient.SendLogLine (Line) then
                    begin
                        inc (CSerialNr) ;
                    end ;
                end ;
                LabelSendClient.Caption := 'Total Lines Sent to Server ' + IcsIntToCStr (CSerialNr) ;
            end ;
        end ;
        if DataServer.Checked then
        begin
            if IpLogServer.AnyStateOK then
            begin
                Line := AnsiString (TimeToStr (Time) +
                  ' Test Line of Data from Server, Serial ' + IcsIntToCStr (SSerialNr)) ;
                if IpLogServer.SendLogLine (Line) then
                begin
                   LabelSendServer.Caption := 'Total Lines Sent by Server ' + IcsIntToCStr (SSerialNr) ;
                   inc (SSerialNr) ;
                end ;
            end ;
        end ;
    finally
        DataTimer.Enabled := true ;
    end;
end;

(*
function TIpLogForm.SetSsl (client, server: boolean): boolean ;
var
    fname: string ;
    CertStr, ErrStr: string;
    valres: TChainResult;
begin
    Result := false ;
    IpLogClient.ForceSsl := false ;
    IpLogServer.ForceSsl := false ;
    if (Protocol.ItemIndex <> ProtoTcp) or (NOT UseSsl.Checked) then
    begin
        Result := true ;
        exit ;
    end ;
    try
        // Feb 2016, before cert functions
   {     if NOT SslContextSrv.IsSslInitialized then begin
            SslContextSrv.InitializeSsl;
            LogWin.Lines.Add ('SSL Version: ' + OpenSslVersion + ', Dir: ' + GLIBEAY_DLL_FileName) ;
        end;   }

        // SSL server must have a certificate and private key to work
        // but generally we don't verify the client talking to the server
        if server then
        begin
            fname := SslServCert.Text;
            if (Pos (':', fname) = 0) then fname := FCertificateDir + fname ;
            if NOT FileExists (fname) then
            begin
                LogWin.Lines.Add ('Can Not Find SSL Server Certificate File - ' + fname) ;
                exit ;
            end;

         // Feb 2017 try and load bundle, certificate, private key and intermediates
            with SslContextSrv.SslCertX509 do
            begin
                LogWin.Lines.Add ('Loading SSL Server Certificate File - ' + fname) ;
                LoadFromFile (fname, croTry, croTry, 'password');
                if NOT IsPkeyLoaded then  // no private password, try from separate file
                begin
                    fname := SslCertKey.Text;
                    if (Pos (':', fname) = 0) then fname := FCertificateDir + fname ;
                    if NOT FileExists (fname) then
                    begin
                        LogWin.Lines.Add ('Can Not Find SSL Server Private Key File - ' + fname) ;
                        exit ;
                    end;
                    PrivateKeyLoadFromPemFile (fname, 'password');
                end;
                if NOT IsInterLoaded then  // no intermediate certificates, try from separate file
                begin
                    fname := SslCACerts.Text;
                    if fname <> '' then
                    begin
                        if (Pos (':', fname) = 0) then fname := FCertificateDir + fname ;
                        if NOT FileExists (fname) then
                        begin
                            LogWin.Lines.Add ('Can Not Find SSL Server CA Bundle File - ' + fname) ;
                            exit ;
                        end;
                        LoadIntersFromPemFile (fname);
                    end;
                end;

             // Feb 2017 validate server certificate chain
                fname := SslCertAuth.Text;
                if fname <> '' then
                begin
                    if (Pos (':', fname) = 0) then fname := FCertificateDir + fname ;
                    if FileExists (fname) then
                        LoadCATrustFromPemFile(fname);
                end
                else
                    LoadCATrustFromString(sslRootCACertsBundle);  /? trusted root

                valres := ValidateCertChain('', CertStr, ErrStr);   // really need host name
                if ReportChain.Checked and (CertStr <> '') then begin
                    LogWin.Lines.Add (CertStr);
                    IpLogServer.LogSslReportChain := false;  // not a second time
                end
                else
                    IpLogServer.LogSslReportChain := ReportChain.Checked ;  // Nov 2016
                if valres = chainOK then
                    ErrStr := 'Chain Validated OK'
                else if valres = chainWarn then
                    ErrStr := 'Chain Warning - ' + ErrStr
                else
                    ErrStr := 'Chain Failed - ' + ErrStr;
                LogWin.Lines.Add (ErrStr + #13#10);
            end;

            fname := SslDHParams.Text;  // May 2015
            if (Pos (':', fname) = 0) then fname := FCertificateDir + fname ;
            if NOT FileExists (fname) then
            begin
                LogWin.Lines.Add ('Can Not Find SSL Server DH Params File - ' + fname) ;
                exit ;
            end;
            SslContextSrv.SslDHParamFile := fname;
            SslContextSrv.SslOptions := { SslContextSrv.SslOptions + } [sslOpt_NO_SSLv2, sslOpt_NO_SSLv3,
                sslOpt_CIPHER_SERVER_PREFERENCE, sslOpt_NO_SESSION_RESUMPTION_ON_RENEGOTIATION,
                sslOpt_SINGLE_DH_USE, SslOpt_SINGLE_ECDH_USE] ; //SSLv2/3 are unsecure
            SslContextSrv.SslCipherList := sslCiphersNormal ;
         //   SslContextSrv.SslCipherList := sslCiphersMozillaSrvInter ;  // Oct 2014, no SSLv3
//            SslContextSrv.SslVersionMethod := sslBestVer_SERVER ;
            SslContextSrv.SslMinVersion := sslVerTLS1;               // Nov 2017
            SslContextSrv.SslMaxVersion  := sslVerMax;               // Nov 2017
            SslContextSrv.SslSecLevel := TSslSecLevel (SslSecLevel.ItemIndex);   // Dec 2016
        // June 2018, pending use IcsHosts so we can use
            SslContextSrv.InitContext;
            if NOT SslContextSrv.CheckPrivateKey then   // Dec 2016
            begin
                LogWin.Lines.Add ('Mismatch for Certificate and Private Key') ;
                exit ;
            end;
            IpLogServer.ForceSsl := true ;
         //   IpLogServer.LogSslContext := SslContextSrv ;
            IpLogServer.LogSslVerMethod := logSslVerNone ;
            IpLogServer.LogSslRevocation := false ;
            if IpLogServer.LogSslVerMethod <> logSslVerBundle then SslContextCli.SslCAFile := '' ;
            IpLogServer.ExternalSslSessCache := SslAvlSessionCache ;
            IpLogServer.SrvTimeoutSecs := AscToInt(SrvTimeout.Text) ; // 5 July 2016
        end;

        // SSL client needs certificate authority root certificates if the server
        // certificate is to be fully verified, but we also have a list of allowed
        // certificates that fail verfication such as our own self signed certificate
        // listed in SslAllowNames.Text
        // not needed if Microsoft Certificate Store is used instead of PEM bundle
        if client then
        begin
       {     fname := SslCertAuth.Text;
            if fname <> '' then
            begin
                if (Pos (':', fname) = 0) then fname := FCertificateDir+ fname ;
                if NOT FileExists (fname) then
                begin
                    LogWin.Lines.Add ('Can Not Find SSL Client CA Bundle File - ' + fname) ;
                    exit ;
                end ;
                SslContextCli.SslCAFile := fname;
            end
            else
                SslContextCli.SslCALines.Text := sslRootCACertsBundle ;  }
      //      SslContextCli.SslOptions := [] ;              // Nov 2017 kill old stuff
       //     SslContextCli.SslCipherList := sslCiphersNormal;
 //         SslContextCli.SslVersionMethod := sslBestVer_CLIENT ;
        //    SslContextCli.SslMinVersion := sslVerTLS1;               // Nov 2017 modern stuff
        //    SslContextCli.SslMaxVersion  := sslVerMax;               // Nov 2017
        //    SslContextCli.SslSecLevel := TSslSecLevel (SslSecLevel.ItemIndex);   // Dec 2016
       //     SslContextCli.SslCliSecurity := TSslCliSecurity(SslCliSecurity.ItemIndex);  // June 2018 replaces prior stuff
       //     SslContextCli.InitContext;

        if client then
        begin
            IpLogClient.ForceSsl := true ;
       //     IpLogClient.LogSslContext := SslContextCli ;
            IpLogClient.LogTrustedList := SslAllowNames.Text ;
            IpLogClient.LogSslVerMethod := TVerifyMethod (VerifyCertMode.ItemIndex) ;
            IpLogClient.LogSslRevocation := RevokeCheck.Checked ;
            IpLogClient.LogSslReportChain := ReportChain.Checked ;
            IpLogClient.ExternalSslSessCache := SslAvlSessionCache ;
            IpLogClient.LogSslCliSecurity := TSslCliSecurity(SslCliSecurity.ItemIndex);
            IpLogClient.LogSslRootFile := SslCertAuth.Text;
        end;
        Result := true ;
    except
        LogWin.Lines.Add ('Failed to Initialise SSL - ' + GetExceptMess (ExceptObject)) ;
    end ;
end;   *)

// local needed both client and server to send stuff to each other

procedure TIpLogForm.doLocalClick(Sender: TObject);
var
    fname: string ;
//    CertStr
    ErrStr: string;
//    valres: TChainResult;
begin

/// server stuff
    IpLogServer.MaxSockets := 1 ;
    IpLogServer.RawData := RawData.Checked ;
    IpLogServer.ForceSsl := UseSsl.Checked;
    if (Protocol.ItemIndex = ProtoUdp) then
    begin
        IpLogClient.LogProtocol := logprotUdpClient ;
        IpLogServer.LogProtocol := logprotUdpServer ;
        IpLogServer.SocFamily := TSocketFamily (SocketFamily.ItemIndex) ;
        IpLogServer.LocalIpAddr := LocalAddr.Text ;
        IpLogServer.LocalIpPort := LocalPort.Text ;
    end
    else if (Protocol.ItemIndex = ProtoTcp) then
    begin
        IpLogClient.LogProtocol := logprotTcpClient ;
        IpLogServer.LogProtocol := logprotTcpServer ;
        IpLogServer.LogSslVerMethod := logSslVerNone ;
        IpLogServer.LogSslRevocation := false ;
        IpLogServer.ExternalSslSessCache := SslAvlSessionCache ;
        IpLogServer.SrvTimeoutSecs := atoi(SrvTimeout.Text) ; // 5 July 2016
        IpLogServer.SrvIcsHosts.Clear;
        IpLogServer.SrvIcsHosts.Add;  // only need one host
        with IpLogServer.SrvIcsHosts [0] do
        begin
            HostEnabled := True;
            HostNames.Text := 'MyHost';
            BindIpAddr := LocalAddr.Text ;
        //    BindIpAddr2 :=
            if NOT IpLogServer.ForceSsl then
                BindNonPort := atoi(LocalPort.Text)
            else
                BindSslPort := atoi(LocalPort.Text) ;
            HostTag := 'LocalServer' ;
            Descr := HostTag;
       //     WellKnownPath :=
            CertSupplierProto := SuppProtoNone;

            if BindSslPort <> 0 then begin
                IpLogClient.LogSslRootFile := SslCertAuth.Text;
                fname := SslDHParams.Text;  // May 2015
                if (Pos (':', fname) = 0) then fname := FCertificateDir + fname ;
                if NOT FileExists (fname) then
                begin
                    LogWin.Lines.Add ('Can Not Find SSL Server DH Params File - ' + fname) ;
                    exit ;
                end;
                IpLogServer.SrvDHParams := fname;
                SslSrvSecurity := TSslSrvSecurity(SslSrvSec.ItemIndex);
                SslCert := IcsTrim(SslServCert.Text);
                SslKey := IcsTrim(SslCertKey.Text);
                SslPassword := IcsTrim('password');
                SslInter := IcsTrim(SslCACerts.Text);
              { following are for automatic ordering and installation of SSL certificates }
                CertSupplierProto := SuppProtoNone;                          { V8.59 sanity test }
             //   CertSupplierProto := TSupplierProto(x);
             //   CertDirWork := IcsTrim(x);
             //   CertChallenge := TChallengeType(x);
             //   CertPKeyType := TSslPrivKeyType(x);
             //   CertProduct := IcsTrim(x);
             //   CertSignDigest := TEvpDigest(x);
            end;
        end;

    // set-up binding and SSL contexts, check certificates
   // validate hosts and keep site certificiate information
        try
            ErrStr := IpLogServer.SrvValidateHosts(False, True); // don't stop on first error, no exceptions
            if ErrStr <> '' then begin
                LogWin.Lines.Add('Server Host Validation Errors:' + icsCRLF + ErrStr);
                Exit;
            end;
      //    ReportHosts;
      //    Display('Required Listen Bindings:' + icsCRLF + SslHttpAppSrv1.ListenStates);
        except
            on E:Exception do begin
                LogWin.Lines.Add('Server Host Validation Failed - ' + E.Message);
                Exit;
            end;
        end;
    end
    else
        exit ;

    SetButtons (true) ;

 // client stuff
    IpLogClient.MaxSockets := 1 ;
    IpLogClient.SocFamily := TSocketFamily (SocketFamily.ItemIndex) ;
    if IpLogClient.SocFamily = sfAnyIPv6 then
        IpLogClient.RemoteHost := ICS_LOCAL_HOST_V6
    else if IpLogClient.SocFamily = sfAnyIPv4 then
        IpLogClient.RemoteHost := ICS_LOCAL_HOST_V4
    else
        IpLogClient.RemoteHost := LocalAddr.Text ;
    IpLogClient.RemoteIpPort := LocalPort.Text ;
    IpLogClient.CheckPing := PingRemote.Checked ;
    IpLogClient.RawData := RawData.Checked ;
    IpLogClient.ForceSsl := UseSsl.Checked;
    if (Protocol.ItemIndex = ProtoTcp) and IpLogClient.ForceSsl then
    begin
        IpLogClient.LogSslVerMethod := TStrmVerifyMethod (VerifyCertMode.ItemIndex) ;
        IpLogClient.LogSslRevocation := RevokeCheck.Checked ;
        IpLogClient.LogSslReportChain := ReportChain.Checked ;
        IpLogClient.ExternalSslSessCache := SslAvlSessionCache ;
        IpLogClient.LogSslCliSecurity := TSslCliSecurity(SslCliSec.ItemIndex);
        IpLogClient.LogSslRootFile := SslCertAuth.Text;
    end;

// diagnostic stuff
    if LogInfo.Checked then
    begin
        IpLogClient.IpIcsLogger.LogOptions := MyLogOptions2 ;
        IpLogServer.IpIcsLogger.LogOptions := MyLogOptions2 ;
    end
    else if LogErrors.Checked then
    begin
        IpLogClient.IpIcsLogger.LogOptions := MyLogOptions ;
        IpLogServer.IpIcsLogger.LogOptions := MyLogOptions ;
    end
    else
    begin
        IpLogClient.IpIcsLogger.LogOptions := [] ;
        IpLogServer.IpIcsLogger.LogOptions := [] ;
    end;

// start logging
    IpLogServer.StartLogging ;
    IpLogClient.StartLogging ;
    DataTimerTimer (Self) ;
end;

procedure TIpLogForm.doClearClick(Sender: TObject);
begin
    DataWin.Lines.Clear ;
    LogWin.Lines.Clear ;
end;

procedure TIpLogForm.doClientClick(Sender: TObject);
var
    I, tot: integer ;
    Host: String ;
begin
    if (Protocol.ItemIndex = ProtoUdp) then
         IpLogClient.LogProtocol := logprotUdpClient
    else if (Protocol.ItemIndex = ProtoTcp) then
         IpLogClient.LogProtocol := logprotTcpClient
    else
        exit ;

    SetButtons (true) ;
    if RemoteHosts.Lines.Count = 0 then exit ;
    IpLogClient.SocFamily := TSocketFamily (SocketFamily.ItemIndex) ;
    IpLogClient.RemoteIpPort := RemotePort.Text ;
    if RemoteHosts.Lines.Count > 1 then
    begin
        IpLogClient.MaxSockets := RemoteHosts.Lines.Count ;
        tot := 0 ;
        for I := 0 to Pred (RemoteHosts.Lines.Count) do begin
            Host := Trim (RemoteHosts.Lines [I]) ;  // Dec 2016 better check for hosts
            if (Host <> '') and (Pos ('*', Host) <> 1) then begin  // May 2017 ignore lines with *
                IpLogClient.SetRemotes (tot, Host, RemotePort.Text) ;
                inc (tot) ;
            end;
        end;
        if IpLogClient.MaxSockets <> tot then IpLogClient.MaxSockets := tot;
    end
    else
    begin
        IpLogClient.RemoteHost := RemoteHosts.Lines [0] ;
    end ;
    if LogInfo.Checked then
        IpLogClient.IpIcsLogger.LogOptions := MyLogOptions2
    else if LogErrors.Checked then
        IpLogClient.IpIcsLogger.LogOptions := MyLogOptions
    else
        IpLogClient.IpIcsLogger.LogOptions := [] ;
    IpLogClient.LocalIpAddr := LocalAddr.Text ;
    IpLogClient.CheckPing := PingRemote.Checked ;
    IpLogClient.RawData := RawData.Checked ;
    if (Protocol.ItemIndex = ProtoTcp) then
    begin
     {   if NOT SetSsl (true, false) then
        begin
            SetButtons (false) ;
            exit ;
        end ;    }
    end;
    IpLogClient.StartLogging ;
    DataTimerTimer (Self) ;
end;

procedure TIpLogForm.doServerClick(Sender: TObject);
begin
    SetButtons (true) ;
    IpLogServer.MaxSockets := atoi (MaxSockets.Text) ;
    if (Protocol.ItemIndex = ProtoUdp) then
         IpLogServer.LogProtocol := logprotUdpServer
    else if (Protocol.ItemIndex = ProtoTcp) then
         IpLogServer.LogProtocol := logprotTcpServer
    else
        exit ;

    IpLogServer.SocFamily := TSocketFamily (SocketFamily.ItemIndex) ;
    IpLogServer.LocalIpAddr := LocalAddr.Text ;
    IpLogServer.LocalIpPort := ServerPort.Text ;
    IpLogServer.RawData := RawData.Checked ;
    if (Protocol.ItemIndex = ProtoTcp) then
    begin
    {    if NOT SetSsl (false, true) then
        begin
            SetButtons (false) ;
            exit ;
        end ;   }
    end;
    if LogInfo.Checked then
        IpLogServer.IpIcsLogger.LogOptions := MyLogOptions2
     else if LogErrors.Checked then
        IpLogServer.IpIcsLogger.LogOptions := MyLogOptions
    else
        IpLogServer.IpIcsLogger.LogOptions := [] ;
    IpLogServer.StartLogging ;
    DataTimerTimer (Self) ;
end;

procedure TIpLogForm.doStopClick(Sender: TObject);
var
    endtick: longword ;
    stopflag: boolean ;
    MySocket: TWSocket ;
begin
    DataTimer.Enabled := false ;

 // 7 July 2016 report traffic
    if IpLogServer.LogActive then
    begin
        MySocket := IpLogServer.Socket [0] ;
        if Assigned (MySocket) and Assigned (MySocket.Counter) then
            LogWin.Lines.Add ('Server session lasted ' + IntToStr (IcsCalcTickDiff
                 (MySocket.Counter.ConnectTick, IcsGetTickCount) div 1000) + ' secs, Xmit ' +
                    IntToStr (MySocket.WriteCount) + ', Recv ' + IntToStr (MySocket.ReadCount)) ;
    end;
    if IpLogClient.LogActive then
    begin
        MySocket := IpLogClient.Socket [0] ;
        if Assigned (MySocket) and Assigned (MySocket.Counter) then
            LogWin.Lines.Add ('Client session lasted ' + IntToStr (IcsCalcTickDiff
                 (MySocket.Counter.ConnectTick, IcsGetTickCount) div 1000) + ' secs, Xmit ' +
                    IntToStr (MySocket.WriteCount) + ', Recv ' + IntToStr (MySocket.ReadCount)) ;
    end;

    IpLogServer.StopLogging ;
    IpLogClient.StopLogging ;

 // TCP does not close instantly, we should really wait until it's all done
    endtick := GetTickCount + 5000 ;  // wait five seconds
    while endtick > GetTickCount do
    begin
        stopflag := true ;
        if NOT IpLogServer.CheckStopped then stopflag := false ;
        if NOT IpLogClient.CheckStopped then stopflag := false ;
        if stopflag then break ;
        LogWin.Lines.Add ('Waiting for Streams to Stop') ; // TEMP !!!
        Application.ProcessMessages ;
        Sleep (250) ;
        Application.ProcessMessages ;
    end ;
    SetButtons (false) ;
    FreeAndNil (FLocalFileStream) ;
end;

procedure TIpLogForm.SelectFileClick(Sender: TObject);
begin
    OpenDialog.FileName := SendFileName.Text ;
    OpenDialog.InitialDir := ExtractFileDir(SendFileName.Text) ;
    if OpenDialog.Execute then
        SendFileName.Text := OpenDialog.FileName;
end;

procedure TIpLogForm.doCliSendFileClick(Sender: TObject);
begin
    if (SendFileName.Text = '') or (NOT FileExists (SendFileName.Text)) then
    begin
        LogWin.Lines.Add ('Must Specify a File Name to Send') ;
        exit ;
    end;
    try
        if Assigned (FLocalFileStream) then
        begin
            LogWin.Lines.Add ('Last File Still Being Sent') ;
            exit ;
        end;
        FLocalFileStream := TFileStream.Create (SendFileName.Text,
                                                fmOpenRead + fmShareDenyWrite, MAX_BUFSIZE);
        if NOT IpLogClient.SendStream (FLocalFileStream) then
        begin
            FreeAndNil (FLocalFileStream) ;
            LogWin.Lines.Add ('Failed to Send File') ;
        end
        else
        begin
            LogWin.Lines.Add ('Client Sending File: ' + SendFileName.Text +
                                                ', Size ' + IcsIntToCStr(FLocalFileStream.Size)) ;
        end;
    except
        LogWin.Lines.Add ('Failed to Open Send File - ' + IcsGetExceptMess (ExceptObject)) ;
    end;
end;

procedure TIpLogForm.doSrvSendFileClick(Sender: TObject);
begin
    if (SendFileName.Text = '') or (NOT FileExists (SendFileName.Text)) then
    begin
        LogWin.Lines.Add ('Must Specify a File Name to Send') ;
        exit ;
    end;
    try
        if Assigned (FLocalFileStream) then
        begin
            LogWin.Lines.Add ('Last File Still Being Sent') ;
            exit ;
        end;
        FLocalFileStream := TFileStream.Create (SendFileName.Text,
                                                fmOpenRead + fmShareDenyWrite, MAX_BUFSIZE);
        if NOT IpLogServer.SendStream (FLocalFileStream) then
        begin
            FreeAndNil (FLocalFileStream) ;
            LogWin.Lines.Add ('Failed to Send File') ;
        end
        else
        begin
            LogWin.Lines.Add ('Server Sending File: ' + SendFileName.Text +
                                                ', Size ' + IcsIntToCStr(FLocalFileStream.Size)) ;
        end;
    except
        LogWin.Lines.Add ('Failed to Open Send File - ' + IcsGetExceptMess (ExceptObject)) ;
    end;
end;

end.
