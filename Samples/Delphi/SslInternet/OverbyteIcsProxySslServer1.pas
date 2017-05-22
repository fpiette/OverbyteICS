{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Forward and Reverse SSL HTTP Proxy
Creation:     May 2017
Updated:      May 2017
Version:      8.48
Support:      Use the mailing list ics-ssl@elists.org
Legal issues: Copyright (C) 2003-2017 by François PIETTE
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

History:
22 May 2017 - 8.48 baseline



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsProxySslServer1;

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

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, ComCtrls, TypInfo,
  OverbyteIcsWSocket, OverbyteIcsWSocketS, OverbyteIcsWndControl,
  OverbyteIcsUtils, OverbyteIcsSSLEAY, OverbyteIcsLIBEAY,
  OverbyteIcsLogger, OverbyteIcsSslX509Utils, OverbyteIcsProxy;

const
    ProxyCopyRight : String = ' OverbyteIcsProxySslServer (c) 2017 Francois Piette V8.47 ';
    MaxWinChars = 800000;
    WM_STARTUP = WM_USER + 711 ;

type
  TProxySslServerForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    StartButton: TButton;
    StopButton: TButton;
    IcsHttpProxy1: TIcsHttpProxy;
    Timer1: TTimer;
    RecheckCertsButton: TButton;
    procedure WMCMSTARTUP (var Msg : TMessage); message WM_STARTUP ;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IcsHttpProxy1DataRecvTar(Sender: TObject;
      ProxyClient: TProxyClient; DataPtr: Pointer; var DataLen: Integer);
    procedure IcsHttpProxy1DataSendTar(Sender: TObject;
      ProxyClient: TProxyClient; DataPtr: Pointer; var DataLen: Integer);
    procedure IcsHttpProxy1ProxyProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure IcsHttpProxy1SetTarget(Sender: TObject;
      ProxyClient: TProxyClient);
    procedure Timer1Timer(Sender: TObject);
    procedure RecheckCertsButtonClick(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
  public
    procedure Display(Msg : String);
    procedure ReportHosts;
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
    ProxySslServerForm: TProxySslServerForm;
    WinLinesBuff: string ;
    WinDispCur: Integer;
    ProgDirectory: String;
    LogDate: Integer;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ we update DisplayMemo once a second in the timer }
procedure TProxySslServerForm.Display(Msg : String);
begin
    WinLinesBuff := WinLinesBuff + Msg + cCRLF ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.Timer1Timer(Sender: TObject);
var
    displen, removelen, newstart: integer ;
    S1: String ;
    NewFlag: Boolean;
begin

  // rotate logs at midnight, recheck SSL certificates, log configuration again
    if LogDate <> Trunc(Date) then begin
        LogDate := Trunc(Date);

    // revalidate SSL certs which might expire at midnight
        try
          // don't stop on first error, no exceptions
            Display('Nightly Server Recheck Starting');
            NewFlag := IcsHttpProxy1.RecheckSslCerts(S1, False, True);
            if NewFlag or (S1 <> '') then  begin
                if NewFlag then Display('Server Recheck Loaded New SSL Certificate(s)');
                Display('Proxy Recheck SSL Certificate Errors:' + cCRLF + S1);
            end;
        except
            on E:Exception do begin
               Display('Proxy Recheck SSL Certificate Failed - ' + E.Message);
            end;
        end;
        ReportHosts;    // new log file, report everything again
        Display('Listen Bindings:' + cCRLF + IcsHttpProxy1.SourceServer.ListenStates);
    end;

  // see if updating the log window with multiple lines
    displen := Length (WinLinesBuff) ;
    if displen > 0 then begin
        try
            if WinDispCur + displen > MaxWinChars then begin
                S1 := DisplayMemo.Lines.Text ;
                removelen := MaxWinChars - displen - 20000 ;
                if removelen > 20000 then begin
                    S1 := copy (S1, removelen, 9999999) ;
                    newstart := Pos(cCR, S1) ; // find start of next line
                    DisplayMemo.Text := copy (S1, newstart, 9999999) + WinLinesBuff ;
                    WinDispCur := Length (S1) - newstart + displen ;
                end
                else begin
                    DisplayMemo.Lines.Text := WinLinesBuff ;
                    WinDispCur := displen ;
                end;
            end
            else begin
                SetLength (WinLinesBuff, displen - 2) ;  // remove CRLF
                DisplayMemo.Lines.Add (WinLinesBuff) ;
                WinDispCur := WinDispCur + displen ;
                SendMessage (DisplayMemo.Handle, WM_VSCROLL, SB_BOTTOM, 0);
            end;
        except
        end ;
        WinLinesBuff := '' ;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.FormCreate(Sender: TObject);
begin
    FIniFileName := GetIcsIniFileName;
  // ensure SSL DLLs come from program directory, and exist, else die
  // GSSLEAY_DLL_IgnoreNew := true ; // !!! TEMP TESTING
    GSSLEAY_DLL_IgnoreOld := true ;
    ProgDirectory := ExtractFileDir (Lowercase (ParamStr(0))) ;
    GSSL_DLL_DIR := ProgDirectory + '\' ;
    GSSL_SignTest_Check := True;
    GSSL_SignTest_Certificate := True;
    OverbyteIcsWSocket.LoadSsl;
    LogDate := Trunc(Date);
    PostMessage (Handle, WM_STARTUP, 0, 0) ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.FormDestroy(Sender: TObject);
begin
    OverbyteIcsWSocket.UnLoadSsl;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIcsIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        IniFile.Free;
        DisplayMemo.Clear;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.IcsHttpProxy1DataRecvTar(Sender: TObject;
  ProxyClient: TProxyClient; DataPtr: Pointer; var DataLen: Integer);
begin
 //   Display('Data received from target, bytes ' + IntToStr(DataLen));
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.IcsHttpProxy1DataSendTar(Sender: TObject;
  ProxyClient: TProxyClient; DataPtr: Pointer; var DataLen: Integer);
begin
//    Display('Data being sent to target, bytes ' + IntToStr(DataLen));
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.IcsHttpProxy1ProxyProg(Sender: TObject;
  LogOption: TLogOption; const Msg: string);
begin
    Display(Msg);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.IcsHttpProxy1SetTarget(Sender: TObject;
  ProxyClient: TProxyClient);
begin
    Display('Do we want to change target??? - ' + ProxyClient.TarHost);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.RecheckCertsButtonClick(Sender: TObject);
begin
    LogDate := 0;  // force timer midnight event
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.FormClose(
    Sender     : TObject;
    var Action : TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    StopButtonClick(Self);
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.UpdateFile;
    IniFile.Free;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.WMCMSTARTUP (var Msg : TMessage);
begin
    StartButtonClick(Self);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.ReportHosts;
var
    J: Integer;
    S: String;
begin
    Display('Total source server hosts ' + IntToStr(IcsHttpProxy1.IcsHosts.Count)) ;
    for J := 0 to IcsHttpProxy1.IcsHosts.Count - 1 do begin
        with IcsHttpProxy1.IcsHosts[J] do begin
            if CertValRes > chainOK then begin
                Display('Server SSL Certificate Errors: Server: ' + DisplayName +
                    cCRLF + 'SSL Certificate chain: ' + cCRLF + CertInfo + cCRLF + CertErrs) ;
            end;
            if CertValRes = chainFail then begin
                Display('Server SSL Certificate Errors') ;
                Exit;
            end;
            S := 'Source #' + IntToStr(J) + ' - ' + HostTag + ': ' + Descr + cCRLF +
              'Host: ' + HostNames [0] + ', Bindings: ' + BindInfo + cCRLF;
            if BindSslPort <> 0 then S := S + 'SSL Security Level ' +
                  GetEnumName(TypeInfo(TSslSrvSecurity), Ord(SslSrvSecurity)) +
                    ' - ' + CertErrs + cCRLF + CertInfo + cCRLF
            else
                S := S + 'No SSL' + cCRLF;
            if ForwardProxy then S := S + 'Forward Proxy' + cCRLF;
            Display(S) ;
        end;
    end;
    Display('Total remote targets ' + IntToStr(IcsHttpProxy1.ProxyTargets.Count)) ;
    for J := 0 to IcsHttpProxy1.ProxyTargets.Count - 1 do begin
        with IcsHttpProxy1.ProxyTargets[J] do begin
            S := 'Target #' + IntToStr(J) + ' - ' + HostTag + ': ' + Descr + cCRLF +
                    'Remote ' + TarHost + ':' + IntToStr (TarPort) ;
            if TarSsl then S := S + ' with SSL' ;
            if SrcPath <> '' then S := S + ', Path ' + SrcPath ;
            S := S + cCRLF ;
           Display(S) ;
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.StartButtonClick(Sender: TObject);
var
    IniFile : TIcsIniFile;
    List: TStringList;
    J: Integer;
    S: String;
begin
    try
      { try and open INI file in C:\Users\(me)\AppData\Local\ICS, if missing copy
        default from sample directory  }
        if NOT FileExists(FIniFileName) then begin
            S := ChangeFileExt(IcsLowercase(ParamStr(0)), '.ini');
            if FileExists(S) then begin
                List := TStringList.Create;
                try
                    List.LoadFromFile(S);
                    List.SaveToFile(FIniFileName);
                    Display('Copied default INI file to: ' + FIniFileName + ', where it should be edited');
                except
                    List.Free;
                end;
            end;
        end;
        IniFile := TIcsIniFile.Create(FIniFileName);
        Display('INI file: ' + FIniFileName);

      // tell them who we are
        Display(OverbyteIcsProxy.CopyRight + cCRLF +
           'SSL Version: ' + OpenSslVersion + ', Dir: ' + GLIBEAY_DLL_FileName + cCRLF);

      // main proxy settings from INI file, built in CA if no file found
        IcsLoadTIcsHttpProxyFromIni(IniFile, IcsHttpProxy1, 'Proxy');
        if (IcsHttpProxy1.RootCA = '') or (NOT FileExists(IcsHttpProxy1.RootCA)) then
                                         IcsHttpProxy1.RootCA := sslRootCACertsBundle;

      // read the source server hosts from INI file and check SSL files exist
        IcsLoadIcsHostsFromIni(IniFile, IcsHttpProxy1.IcsHosts, 'Source');
        if IcsHttpProxy1.IcsHosts.Count <= 0 then
         begin
            Display('Can Not Start Server - No Source Server Hosts Configured') ;
            exit ;
        end;
        for J := 0 to IcsHttpProxy1.IcsHosts.Count - 1 do begin
            with IcsHttpProxy1.IcsHosts [J] do begin
                if BindSslPort <> 0 then begin

               // check if file names need paths and they exist
                     if (NOT FileExists (SslCert)) then begin
                        HostEnabled := false ;
                        Display('SSL HTTP Server disabled, certificate file not found for: ' +
                                                                        Descr + ', Cert: ' + SslCert) ;
                     end;
                     if (SslKey <> '') and (NOT FileExists (SslKey)) then begin
                        HostEnabled := false ;
                        Display('SSL HTTP Server disabled, private key file not found for: ' +
                                                                        Descr + ', Cert: ' + SslKey) ;
                     end;
                     if (SslInter <> '') and (NOT FileExists (SslInter)) then begin
                        HostEnabled := false ;
                        Display('SSL HTTP Server disabled, intermediate certificate file not found for: ' +
                                                                        Descr + ', Cert: ' + SslInter) ;
                     end;
                end;
            end;
        end;

    // read the remote proxy targets
        IcsLoadProxyTargetsFromIni(IniFile, IcsHttpProxy1.ProxyTargets, 'Target');
        if IcsHttpProxy1.ProxyTargets.Count <= 0 then  begin
            Display('Can Not Start Server - No Proxy Targets Configured');
            exit ;
        end;

    // validate hosts and keep site certificiate information
        try
            S := IcsHttpProxy1.ValidateHosts(False, True); // don't stop on first error, no exceptions }
            if S <> '' then begin
                Display('Proxy Validation Errors:' + cCRLF + S);
            end;
            ReportHosts;
            Display('Required Listen Bindings:' + cCRLF + IcsHttpProxy1.SourceServer.ListenStates);
        except
            on E:Exception do begin
                Display('Host Validation Failed, Server Stopped - ' + E.Message);
                Exit;
            end;
        end;
        IcsHttpProxy1.Start;
        if NOT IcsHttpProxy1.SourceServer.ListenAllOK then
            Display('Failed to Start, Listen Bindings:' +
                                cCRLF + IcsHttpProxy1.SourceServer.ListenStates);
        StartButton.Enabled := false;
        StopButton.Enabled := true;
    except
        on E:Exception do begin
           Display('Failed to start proxy - ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TProxySslServerForm.StopButtonClick(Sender: TObject);
begin
    if NOT StopButton.Enabled then Exit;
    StartButton.Enabled := true;
    StopButton.Enabled := false;
    IcsHttpProxy1.Stop;
    Display('Server stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
