{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  ICS SSL Json Object Signing (Jose) Demos
Creation:     May 2018
Updated:      May 2018
Version:      8.54
Support:      Use the mailing list ics-ssl@elists.org
Legal issues: Copyright (C) 2003-2018 by François PIETTE
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
22 May 2018 - 8.54 baseline

For docunentation on how to use this sample, please see a length Overview in
the OverbyteIcsSslX509Certs.pas unit.  


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsX509CertsTst1;

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids,
  OverbyteIcsWSocket,
  OverbyteIcsWndControl,
  OverbyteIcsHttpProt,
  OverbyteIcsIniFiles,
  OverbyteIcsUtils,
  OverbyteIcsMimeUtils,
  OverbyteIcsURL,
  OverbyteIcsLogger,     { for TLogOption }
  OverbyteIcsSSLEAY,
  OverbyteIcsLibeay,
  OverbyteIcsSslHttpRest,
  OverbyteIcsSuperObject,
  OverbyteIcsSslJose,
  OverbyteIcsSslX509Utils,
  OverbyteIcsSslX509Certs;

type
  TX509CertsForm = class(TForm)
 // properties saved
    AccAcmeKeyV1: TComboBox;
    AccAcmeKeyV2: TComboBox;
    AcmeServerV1: TComboBox;
    AcmeServerV2: TComboBox;
    CertAddress: TEdit;
    CertCentreApprovEmail: TComboBox;
    CertCentreOrderId: TEdit;
    CertCentreOrderRef: TEdit;
    CertCentreProducts: TListBox;
    CertCentreServer: TComboBox;
    CertContactEmail: TEdit;
    CertContactFirst: TEdit;
    CertContactLast: TEdit;
    CertContactTitle: TEdit;
    CertCountry: TEdit;
    CertLocality: TEdit;
    CertOrganization: TEdit;
    CertOrganizationalUnit: TEdit;
    CertPhone: TEdit;
    CertPostCode: TEdit;
    CertSignDigestType: TRadioGroup;
    CertState: TEdit;
    CertValidity: TEdit;
    DebugLogging: TComboBox;
    DirAcmeConfV1: TEdit;
    DirAcmeConfV2: TEdit;
    DirCertCenConf: TEdit;
    DirLogs: TEdit;
    DirPubWebCert: TEdit;
    DirWellKnown: TEdit;
    DomWebSrvIP: TComboBox;
    LogJson: TCheckBox;
    LogPkeys: TCheckBox;
    OAuthAccToken: TEdit;
    OAuthAppUrl: TEdit;
    OAuthAutoRefresh: TCheckBox;
    OAuthClientId: TEdit;
    OAuthClientSecret: TEdit;
    OAuthExpire: TEdit;
    OAuthRedirectUrl: TEdit;
    OAuthRefToken: TEdit;
    OAuthRefrMins: TEdit;
    OAuthScope: TEdit;
    OAuthTokenUrl: TEdit;
    OAuthWebIP: TComboBox;
    OAuthWebPort: TEdit;
    OldOSL: TCheckBox;
    PrivKeyCipher: TRadioGroup;
    PrivKeyPassword: TEdit;
    PrivKeyType: TRadioGroup;
    SuppCertChallenge: TRadioGroup;

 // properties not saved
    X509Certs1: TSslX509Certs;
    LogWin: TMemo;
    CertSANGrid: TStringGrid;
    CertBuy: TEdit;
    OAuthAuthCode: TEdit;
    PageControl1: TPageControl;
    TabCommon: TTabSheet;
    Label3: TLabel;
    Label15: TLabel;
    doWebServer: TButton;
    TabDomain: TTabSheet;
    Label9: TLabel;
    Label2: TLabel;
    Label13: TLabel;
    Label23: TLabel;
    doTestWellKnown: TButton;
    TabInfo: TTabSheet;
    lbCountry: TLabel;
    lbState: TLabel;
    lbLocality: TLabel;
    lbOrganization: TLabel;
    lbOrganizationalUnit: TLabel;
    lbEMail: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label24: TLabel;
    TabAcme1: TTabSheet;
    Label1: TLabel;
    Label5: TLabel;
    Label28: TLabel;
    LabelAcme1Cert: TLabel;
    Label29: TLabel;
    doAcmeOrderV1: TButton;
    doAcmeAccV1: TButton;
    doAcmeCheckOrderV1: TButton;
    doAcmeGetCertV1: TButton;
    TabAcme2: TTabSheet;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    LabelAcme2Cert: TLabel;
    doAcmeAccV2: TButton;
    doAcmeCheckOrderV2: TButton;
    doAcmeOrderV2: TButton;
    doAcmeGetCertV2: TButton;
    TabCCOrder: TTabSheet;
    Label6: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Label16: TLabel;
    LabelCertInfo: TLabel;
    Label4: TLabel;
    doCCProfile: TButton;
    doCertCentreAlways: TButton;
    doCertCentreCollect: TButton;
    doCertCentreOther: TButton;
    doCertCentreOrders: TButton;
    doCertCentreCancel: TButton;
    doCertCentreRevoke: TButton;
    doCertCentreCheck: TButton;
    TabCCOAuth: TTabSheet;
    TabServtas: TTabSheet;
    BoxOAuthApp: TGroupBox;
    Label7: TLabel;
    Label11: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label34: TLabel;
    BoxOAuthTokens: TGroupBox;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    BoxOAuthWeb: TGroupBox;
    Label39: TLabel;
    Label40: TLabel;
    Label14: TLabel;
    Label22: TLabel;
    Label41: TLabel;
    doOARefreshNow: TButton;
    doOACodeToken: TButton;
    Label42: TLabel;
    CertCommonName: TEdit;


    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doCCProfileClick(Sender: TObject);
    procedure doCertCentreCheckClick(Sender: TObject);
    procedure doCertCentreAlwaysClick(Sender: TObject);
    procedure doCertCentreOtherClick(Sender: TObject);
    procedure doCertCentreOrdersClick(Sender: TObject);
    procedure doCertCentreCollectClick(Sender: TObject);
    procedure doCertCentreCancelClick(Sender: TObject);
    procedure doCertCentreRevokeClick(Sender: TObject);
    procedure CertCentreProductsClick(Sender: TObject);
    procedure doTestWellKnownClick(Sender: TObject);
    procedure doAcmeAccV1Click(Sender: TObject);
    procedure doAcmeCheckOrderV1Click(Sender: TObject);
    procedure doAcmeOrderV1Click(Sender: TObject);
    procedure doAcmeGetCertV1Click(Sender: TObject);
    procedure doAcmeAccV2Click(Sender: TObject);
    procedure doAcmeCheckOrderV2Click(Sender: TObject);
    procedure doAcmeOrderV2Click(Sender: TObject);
    procedure doAcmeGetCertV2Click(Sender: TObject);
    procedure doWebServerClick(Sender: TObject);
    procedure X509Certs1CertProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure X509Certs1NewCert(Sender: TObject);
    procedure X509Certs1NewToken(Sender: TObject);
    procedure doOARefreshNowClick(Sender: TObject);
    procedure doOACodeTokenClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FProgDir: String;
    FIniFileName: String;
    FCookieFileName: String;
    FInitialized: Boolean;
    FLogFStream: TFileStream;
    FLogOpen: Boolean;
    FPendCCProfile: Boolean;
    procedure AddLog (const S: string) ;
    procedure SetOAParams;
    procedure SetCommParams;
    procedure SetCertParams;
    procedure ResetButtons;
  end;

const
    SectionMainWindow    = 'MainWindow';
    KeyTop               = 'Top';
    KeyLeft              = 'Left';
    KeyWidth             = 'Width';
    KeyHeight            = 'Height';
    SectionData          = 'Data';
    KeyCertSANGrid       = 'CertSANGrid';

var
  X509CertsForm: TX509CertsForm;

implementation

{$R *.dfm}


procedure TX509CertsForm.FormCreate(Sender: TObject);
begin
    FProgDir     := ExtractFilePath(ParamStr(0));
    FIniFileName := GetIcsIniFileName;
    FCookieFileName := ChangeFileExt(FIniFileName, '.cookie');

 // Avoid dynamical loading and unloading the SSL DLLs plenty of times
    GSSLEAY_DLL_IgnoreNew := False;    { don't ignore OpenSSL 1.1.0 and later }
//  GSSLEAY_DLL_IgnoreNew := True;     { don't ignore OpenSSL 1.1.0 and later }
//  GSSLEAY_DLL_IgnoreOld := True;     { ignore OpenSSL 1.0.2 and earlier }
    GSSL_DLL_DIR := FProgDir;          { only from our directory }
    GSSL_SignTest_Check := True;       { check digitally signed }
    GSSL_SignTest_Certificate := True; { check digital certificate }
    CertSANGrid.Cells[0,0] := 'Domain Name';
    CertSANGrid.Cells[1,0] := 'Web Server UNC HTTP .Well-Known';
    CertSANGrid.Cells[2,0] := 'Web Server UNC Public Certs Dir';
end;

procedure TX509CertsForm.FormDestroy(Sender: TObject);
begin
//
end;

procedure TX509CertsForm.FormShow(Sender: TObject);
var
    IniFile: TIcsIniFile;
    SL: TStringList;
    I, J, K, tot: Integer;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile := TIcsIniFile.Create(FIniFileName);
        Width := IniFile.ReadInteger(SectionMainWindow, KeyWidth,  Width);
        Height := IniFile.ReadInteger(SectionMainWindow, KeyHeight, Height);
        Top := IniFile.ReadInteger(SectionMainWindow, KeyTop, (Screen.Height - Height) div 2);
        Left := IniFile.ReadInteger(SectionMainWindow, KeyLeft, (Screen.Width  - Width)  div 2);
        SL := TStringList.Create;
        try
             SL.Delimiter := '|';
             SL.DelimitedText := IniFile.ReadString(SectionData, KeyCertSANGrid, '');
             tot := SL.Count;
             K := 0;
             for I := 0 to CertSANGrid.RowCount - 1 do begin
                for J := 0 to CertSANGrid.ColCount - 1 do begin
                    if K < tot then
                        CertSANGrid.Cells[J, I] := SL[K];
                    K := K + 1;
                end;
             end;
        finally
             SL.Free;
        end;

       with IniFile do begin
  AccAcmeKeyV1.ItemIndex := ReadInteger (SectionData, 'AccAcmeKeyV1_ItemIndex', AccAcmeKeyV1.ItemIndex) ;
  AccAcmeKeyV2.ItemIndex := ReadInteger (SectionData, 'AccAcmeKeyV2_ItemIndex', AccAcmeKeyV2.ItemIndex) ;
  AcmeServerV1.Text := ReadString (SectionData, 'AcmeServerV1_Text', AcmeServerV1.Text) ;
  AcmeServerV2.Text := ReadString (SectionData, 'AcmeServerV2_Text', AcmeServerV2.Text) ;
  CertAddress.Text := ReadString (SectionData, 'CertAddress_Text', CertAddress.Text) ;
  CertCentreApprovEmail.Text := ReadString (SectionData, 'CertCentreApprovEmail_Text', CertCentreApprovEmail.Text) ;
  CertCentreOrderId.Text := ReadString (SectionData, 'CertCentreOrderId_Text', CertCentreOrderId.Text) ;
  CertCentreOrderRef.Text := ReadString (SectionData, 'CertCentreOrderRef_Text', CertCentreOrderRef.Text) ;
  CertCentreProducts.Items.CommaText := ReadString (SectionData, 'CertCentreProducts_Items', '') ;
  CertCentreProducts.ItemIndex := ReadInteger (SectionData, 'CertCentreProducts_ItemIndex', CertCentreProducts.ItemIndex) ;
  CertCentreServer.Text := ReadString (SectionData, 'CertCentreServer_Text', CertCentreServer.Text) ;
  CertCommonName.Text := ReadString (SectionData, 'CertCommonName_Text', CertCommonName.Text) ;
  CertContactEmail.Text := ReadString (SectionData, 'CertContactEmail_Text', CertContactEmail.Text) ;
  CertContactFirst.Text := ReadString (SectionData, 'CertContactFirst_Text', CertContactFirst.Text) ;
  CertContactLast.Text := ReadString (SectionData, 'CertContactLast_Text', CertContactLast.Text) ;
  CertContactTitle.Text := ReadString (SectionData, 'CertContactTitle_Text', CertContactTitle.Text) ;
  CertCountry.Text := ReadString (SectionData, 'CertCountry_Text', CertCountry.Text) ;
  CertLocality.Text := ReadString (SectionData, 'CertLocality_Text', CertLocality.Text) ;
  CertOrganization.Text := ReadString (SectionData, 'CertOrganization_Text', CertOrganization.Text) ;
  CertOrganizationalUnit.Text := ReadString (SectionData, 'CertOrganizationalUnit_Text', CertOrganizationalUnit.Text) ;
  CertPhone.Text := ReadString (SectionData, 'CertPhone_Text', CertPhone.Text) ;
  CertPostCode.Text := ReadString (SectionData, 'CertPostCode_Text', CertPostCode.Text) ;
  CertSignDigestType.ItemIndex := ReadInteger (SectionData, 'CertSignDigestType_ItemIndex', CertSignDigestType.ItemIndex) ;
  CertState.Text := ReadString (SectionData, 'CertState_Text', CertState.Text) ;
  CertValidity.Text := ReadString (SectionData, 'CertValidity_Text', CertValidity.Text) ;
  DebugLogging.ItemIndex := ReadInteger (SectionData, 'DebugLogging_ItemIndex', DebugLogging.ItemIndex) ;
  DirAcmeConfV1.Text := ReadString (SectionData, 'DirAcmeConfV1_Text', DirAcmeConfV1.Text) ;
  DirAcmeConfV2.Text := ReadString (SectionData, 'DirAcmeConfV2_Text', DirAcmeConfV2.Text) ;
  DirCertCenConf.Text := ReadString (SectionData, 'DirCertCenConf_Text', DirCertCenConf.Text) ;
  DirLogs.Text := ReadString (SectionData, 'DirLogs_Text', DirLogs.Text) ;
  DirPubWebCert.Text := ReadString (SectionData, 'DirPubWebCert_Text', DirPubWebCert.Text) ;
  DirWellKnown.Text := ReadString (SectionData, 'DirWellKnown_Text', DirWellKnown.Text) ;
  DomWebSrvIP.Text := ReadString (SectionData, 'DomWebSrvIP_Text', DomWebSrvIP.Text) ;
  if ReadString (SectionData, 'LogJson_Checked', 'False') = 'True' then LogJson.Checked := true else LogJson.Checked := false ;
  if ReadString (SectionData, 'LogPkeys_Checked', 'False') = 'True' then LogPkeys.Checked := true else LogPkeys.Checked := false ;
  OAuthAccToken.Text := ReadString (SectionData, 'OAuthAccToken_Text', OAuthAccToken.Text) ;
  OAuthAppUrl.Text := ReadString (SectionData, 'OAuthAppUrl_Text', OAuthAppUrl.Text) ;
  if ReadString (SectionData, 'OAuthAutoRefresh_Checked', 'False') = 'True' then OAuthAutoRefresh.Checked := true else OAuthAutoRefresh.Checked := false ;
  OAuthClientId.Text := ReadString (SectionData, 'OAuthClientId_Text', OAuthClientId.Text) ;
  OAuthClientSecret.Text := ReadString (SectionData, 'OAuthClientSecret_Text', OAuthClientSecret.Text) ;
  OAuthExpire.Text := ReadString (SectionData, 'OAuthExpire_Text', OAuthExpire.Text) ;
  OAuthRedirectUrl.Text := ReadString (SectionData, 'OAuthRedirectUrl_Text', OAuthRedirectUrl.Text) ;
  OAuthRefToken.Text := ReadString (SectionData, 'OAuthRefToken_Text', OAuthRefToken.Text) ;
  OAuthRefrMins.Text := ReadString (SectionData, 'OAuthRefrMins_Text', OAuthRefrMins.Text) ;
  OAuthScope.Text := ReadString (SectionData, 'OAuthScope_Text', OAuthScope.Text) ;
  OAuthTokenUrl.Text := ReadString (SectionData, 'OAuthTokenUrl_Text', OAuthTokenUrl.Text) ;
  OAuthWebIP.Text := ReadString (SectionData, 'OAuthWebIP_Text', OAuthWebIP.Text) ;
  OAuthWebPort.Text := ReadString (SectionData, 'OAuthWebPort_Text', OAuthWebPort.Text) ;
  if ReadString (SectionData, 'OldOSL_Checked', 'False') = 'True' then OldOSL.Checked := true else OldOSL.Checked := false ;
  PrivKeyCipher.ItemIndex := ReadInteger (SectionData, 'PrivKeyCipher_ItemIndex', PrivKeyCipher.ItemIndex) ;
  PrivKeyPassword.Text := ReadString (SectionData, 'PrivKeyPassword_Text', PrivKeyPassword.Text) ;
  PrivKeyType.ItemIndex := ReadInteger (SectionData, 'PrivKeyType_ItemIndex', PrivKeyType.ItemIndex) ;
  SuppCertChallenge.ItemIndex := ReadInteger (SectionData, 'SuppCertChallenge_ItemIndex', SuppCertChallenge.ItemIndex) ;
       end;
        IniFile.Free;
    end;

    OAuthWebIP.Items.Assign(LocalIPList);
    DomWebSrvIP.Items.Assign(OAuthWebIP.Items);
    OAuthWebIP.Items.Insert(0, ICS_LOCAL_HOST_V4);
    ResetButtons;
    OverbyteIcsWSocket.LoadSsl;

// get API server URLs
    AcmeServerV1.Items.Add(X509Certs1.GetServerAPIUrl(SuppProtoAcmeV1, False));
    AcmeServerV1.Items.Add(X509Certs1.GetServerAPIUrl(SuppProtoAcmeV1, True));
    AcmeServerV2.Items.Add(X509Certs1.GetServerAPIUrl(SuppProtoAcmeV2, False));
    AcmeServerV2.Items.Add(X509Certs1.GetServerAPIUrl(SuppProtoAcmeV2, True));
    CertCentreServer.Items.Add(X509Certs1.GetServerAPIUrl(SuppProtoCertCentre, False));
    if AcmeServerV1.Text = '' then AcmeServerV1.ItemIndex := 0;
    if AcmeServerV2.Text = '' then AcmeServerV2.ItemIndex := 0;
    if CertCentreServer.Text = '' then CertCentreServer.ItemIndex := 0;

end;

procedure TX509CertsForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
    temp: String;
    SL: TStringList;
    I, J: Integer;
begin
    if FLogOpen then FreeAndNil(FLogFStream);
    FLogOpen := False;
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionMainWindow, KeyTop, Top);
    IniFile.WriteInteger(SectionMainWindow, KeyLeft, Left);
    IniFile.WriteInteger(SectionMainWindow, KeyWidth, Width);
    IniFile.WriteInteger(SectionMainWindow, KeyHeight, Height);
    SL := TStringList.Create;
    try
        SL.Delimiter := '|';
        for I := 0 to CertSANGrid.RowCount - 1 do begin
            for J := 0 to CertSANGrid.ColCount - 1 do begin
                SL.Add(CertSANGrid.Cells[J,I]);
            end;
        end;
        IniFile.WriteString(SectionData, KeyCertSANGrid, SL.DelimitedText);
    finally
        SL.Free;
    end;

    with IniFile do begin
  WriteInteger (SectionData, 'AccAcmeKeyV1_ItemIndex', AccAcmeKeyV1.ItemIndex) ;
  WriteInteger (SectionData, 'AccAcmeKeyV2_ItemIndex', AccAcmeKeyV2.ItemIndex) ;
  WriteString (SectionData, 'AcmeServerV1_Text', AcmeServerV1.Text) ;
  WriteString (SectionData, 'AcmeServerV2_Text', AcmeServerV2.Text) ;
  WriteString (SectionData, 'CertAddress_Text', CertAddress.Text) ;
  WriteString (SectionData, 'CertCentreApprovEmail_Text', CertCentreApprovEmail.Text) ;
  WriteString (SectionData, 'CertCentreOrderId_Text', CertCentreOrderId.Text) ;
  WriteString (SectionData, 'CertCentreOrderRef_Text', CertCentreOrderRef.Text) ;
  WriteInteger (SectionData, 'CertCentreProducts_ItemIndex', CertCentreProducts.ItemIndex) ;
  WriteString (SectionData, 'CertCentreProducts_Items', CertCentreProducts.Items.CommaText) ;
  WriteString (SectionData, 'CertCentreServer_Text', CertCentreServer.Text) ;
  WriteString (SectionData, 'CertCommonName_Text', CertCommonName.Text) ;
  WriteString (SectionData, 'CertContactEmail_Text', CertContactEmail.Text) ;
  WriteString (SectionData, 'CertContactFirst_Text', CertContactFirst.Text) ;
  WriteString (SectionData, 'CertContactLast_Text', CertContactLast.Text) ;
  WriteString (SectionData, 'CertContactTitle_Text', CertContactTitle.Text) ;
  WriteString (SectionData, 'CertCountry_Text', CertCountry.Text) ;
  WriteString (SectionData, 'CertLocality_Text', CertLocality.Text) ;
  WriteString (SectionData, 'CertOrganization_Text', CertOrganization.Text) ;
  WriteString (SectionData, 'CertOrganizationalUnit_Text', CertOrganizationalUnit.Text) ;
  WriteString (SectionData, 'CertPhone_Text', CertPhone.Text) ;
  WriteString (SectionData, 'CertPostCode_Text', CertPostCode.Text) ;
  WriteInteger (SectionData, 'CertSignDigestType_ItemIndex', CertSignDigestType.ItemIndex) ;
  WriteString (SectionData, 'CertState_Text', CertState.Text) ;
  WriteString (SectionData, 'CertValidity_Text', CertValidity.Text) ;
  WriteInteger (SectionData, 'DebugLogging_ItemIndex', DebugLogging.ItemIndex) ;
  WriteString (SectionData, 'DirAcmeConfV1_Text', DirAcmeConfV1.Text) ;
  WriteString (SectionData, 'DirAcmeConfV2_Text', DirAcmeConfV2.Text) ;
  WriteString (SectionData, 'DirCertCenConf_Text', DirCertCenConf.Text) ;
  WriteString (SectionData, 'DirLogs_Text', DirLogs.Text) ;
  WriteString (SectionData, 'DirPubWebCert_Text', DirPubWebCert.Text) ;
  WriteString (SectionData, 'DirWellKnown_Text', DirWellKnown.Text) ;
  WriteString (SectionData, 'DomWebSrvIP_Text', DomWebSrvIP.Text) ;
  if LogJson.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'LogJson_Checked', temp) ;
  if LogPkeys.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'LogPkeys_Checked', temp) ;
  WriteString (SectionData, 'OAuthAccToken_Text', OAuthAccToken.Text) ;
  WriteString (SectionData, 'OAuthAppUrl_Text', OAuthAppUrl.Text) ;
  if OAuthAutoRefresh.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'OAuthAutoRefresh_Checked', temp) ;
  WriteString (SectionData, 'OAuthClientId_Text', OAuthClientId.Text) ;
  WriteString (SectionData, 'OAuthClientSecret_Text', OAuthClientSecret.Text) ;
  WriteString (SectionData, 'OAuthExpire_Text', OAuthExpire.Text) ;
  WriteString (SectionData, 'OAuthRedirectUrl_Text', OAuthRedirectUrl.Text) ;
  WriteString (SectionData, 'OAuthRefToken_Text', OAuthRefToken.Text) ;
  WriteString (SectionData, 'OAuthRefrMins_Text', OAuthRefrMins.Text) ;
  WriteString (SectionData, 'OAuthScope_Text', OAuthScope.Text) ;
  WriteString (SectionData, 'OAuthTokenUrl_Text', OAuthTokenUrl.Text) ;
  WriteString (SectionData, 'OAuthWebIP_Text', OAuthWebIP.Text) ;
  WriteString (SectionData, 'OAuthWebPort_Text', OAuthWebPort.Text) ;
  if OldOSL.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'OldOSL_Checked', temp) ;
  WriteInteger (SectionData, 'PrivKeyCipher_ItemIndex', PrivKeyCipher.ItemIndex) ;
  WriteString (SectionData, 'PrivKeyPassword_Text', PrivKeyPassword.Text) ;
  WriteInteger (SectionData, 'PrivKeyType_ItemIndex', PrivKeyType.ItemIndex) ;
  WriteInteger (SectionData, 'SuppCertChallenge_ItemIndex', SuppCertChallenge.ItemIndex) ;
    end;
    IniFile.UpdateFile;
    IniFile.Free;
end;

procedure TX509CertsForm.AddLog(const S: string);
var
    S2: String;
begin
    if Pos (IcsLF,S) > 0 then
        LogWin.Lines.Text := LogWin.Lines.Text + IcsCRLF + S
    else
       LogWin.Lines.Add (S) ;
    SendMessage(LogWin.Handle, EM_LINESCROLL, 0, 999999);

    try
        if (DirLogs.Text = '') then Exit ;
        if NOT FLogOpen then
           FLogFStream := TFileStream.Create (IncludeTrailingPathDelimiter(DirLogs.Text) +
                 'ics-certlog-' + FormatDateTime (DateMaskPacked, Now) + '.log', fmCreate) ;
        FLogOpen := true ;
        S2 := S + IcsCRLF ;
        FLogFStream.WriteBuffer (S2 [1], Length (S2)) ;
    except
    end;
end;

procedure TX509CertsForm.X509Certs1CertProg(Sender: TObject;
  LogOption: TLogOption; const Msg: string);
begin
    AddLog(Msg);
end;

procedure TX509CertsForm.X509Certs1NewCert(Sender: TObject);
begin
// x
end;

procedure TX509CertsForm.X509Certs1NewToken(Sender: TObject);
begin
    OAuthAccToken.Text := X509Certs1.OAAccToken;
    OAuthRefToken.Text := X509Certs1.OARefreshToken;
    if X509Certs1.OAExpireDT > 10 then
        OAuthExpire.Text := RFC3339_DateToStr(X509Certs1.OAExpireDT)
    else
        OAuthExpire.Text := '';

  // see if repeating CC Profile command, only once 
    if FPendCCProfile and (X509Certs1.OAAccToken <> '') then begin
        FPendCCProfile := False;
        doCCProfileClick(Self);
        FPendCCProfile := False;
    end;
end;

procedure TX509CertsForm.ResetButtons;
begin
    SetCommParams;
    doAcmeCheckOrderV1.Enabled := False;
    doAcmeCheckOrderV2.Enabled := False;
    doAcmeGetCertV1.Enabled := False;
    doAcmeGetCertV2.Enabled := False;
    doAcmeOrderV1.Enabled := False;
    doAcmeOrderV2.Enabled := False;
    doCertCentreAlways.Enabled := False;
    doCertCentreCollect.Enabled := False;
    doCertCentreOrders.Enabled := False;
    doCertCentreCancel.Enabled := False;
    doCertCentreRevoke.Enabled := False;
    doCertCentreCheck.Enabled := False;
end;

procedure TX509CertsForm.SetCommParams;
begin
    X509Certs1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    X509Certs1.LogJson := LogJson.Checked;
    X509Certs1.LogPkeys := LogPkeys.Checked;
end;

procedure TX509CertsForm.SetOAParams;
begin
    X509Certs1.OAAccToken := OAuthAccToken.Text;
    X509Certs1.OAAppUrl := OAuthAppUrl.Text;
    X509Certs1.OAClientId := OAuthClientId.Text;
    X509Certs1.OAClientSecret := OAuthClientSecret.Text;
    X509Certs1.OAExpireDT := RFC3339_StrToDate(OAuthExpire.Text);
    X509Certs1.OARedirectUrl := OAuthRedirectUrl.Text;
    X509Certs1.OARefrMinsPrior := atoi(OAuthRefrMins.Text);
    X509Certs1.OARefreshAuto := OAuthAutoRefresh.Checked;
    X509Certs1.OARefreshToken := OAuthRefToken.Text; ;
    X509Certs1.OAScope := OAuthScope.Text;
    X509Certs1.OATokenUrl := OAuthTokenUrl.Text;
    X509Certs1.OAWebSrvIP := OAuthWebIP.Text;
    X509Certs1.OAWebSrvPort := OAuthWebPort.Text;
end;


procedure TX509CertsForm.SetCertParams;
begin
    SetCommParams;
    X509Certs1.DirWellKnown := IncludeTrailingPathDelimiter (DirWellKnown.Text);
    X509Certs1.DirPubWebCert.Text := IncludeTrailingPathDelimiter (Trim (DirPubWebCert.Text));
    CertCommonName.Text := Trim (CertCommonName.Text);
    X509Certs1.CertCommonName := CertCommonName.Text;
    X509Certs1.PrivKeyType := TSslPrivKeyType (PrivKeyType.ItemIndex);
    X509Certs1.CertSignDigestType := digestlist[CertSignDigestType.ItemIndex];
    X509Certs1.SuppCertChallenge := TChallengeType(SuppCertChallenge.ItemIndex);
    X509Certs1.CertOrganization := CertOrganization.Text;
    X509Certs1.CertOrgUnit := CertOrganizationalUnit.Text;
    X509Certs1.SuppOrderId := '';
    X509Certs1.SuppOrderRef := '';
    X509Certs1.CertAddress := CertAddress.Text;
    X509Certs1.CertLocality := CertLocality.Text;
    X509Certs1.CertState := CertState.Text;
    X509Certs1.CertPostCode := CertPostCode.Text;
    X509Certs1.CertCountry := CertCountry.Text;
//    X509Certs1.CertDescr := CertDescr.Text;
    X509Certs1.CertContactEmail := CertContactEmail.Text;
    X509Certs1.CertContactTitle := CertContactTitle.Text;
    X509Certs1.CertContactFirst := CertContactFirst.Text;
    X509Certs1.CertContactLast := CertContactLast.Text;
    X509Certs1.CertPhone := CertPhone.Text;
    X509Certs1.CertApprovEmail := CertCentreApprovEmail.Text;
    X509Certs1.CertValidity := atoi(CertValidity.Text);
// X509Certs1.CertSubAltNames    - CertSANGrid
end;


procedure TX509CertsForm.doAcmeAccV1Click(Sender: TObject);
begin
    ResetButtons;
    X509Certs1.SupplierProto := SuppProtoAcmeV1;
    X509Certs1.SupplierServer := trim(AcmeServerV1.Text);
    X509Certs1.DirCertWork := IncludeTrailingPathDelimiter (DirAcmeConfV1.Text) ;
    X509Certs1.AcmeAccKeyType := TSslPrivKeyType(AccAcmeKeyV1.ItemIndex);
    X509Certs1.CertContactEmail := CertContactEmail.Text;
    if X509Certs1.SetAcmeAccount then begin
        doAcmeCheckOrderV1.Enabled := True;
    end;
end;

procedure TX509CertsForm.doAcmeAccV2Click(Sender: TObject);
begin
    ResetButtons;
    X509Certs1.SupplierProto := SuppProtoAcmeV2;
    X509Certs1.SupplierServer := trim(AcmeServerV2.Text);
    X509Certs1.DirCertWork := IncludeTrailingPathDelimiter (DirAcmeConfV2.Text) ;
    X509Certs1.AcmeAccKeyType := TSslPrivKeyType (AccAcmeKeyV2.ItemIndex);
    X509Certs1.CertContactEmail := CertContactEmail.Text;
    if X509Certs1.SetAcmeAccount then begin
        doAcmeCheckOrderV2.Enabled := True;
    end;
 end;

procedure TX509CertsForm.doAcmeCheckOrderV1Click(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateAccount then begin
        AddLog('Must Register Acme Account First');
        Exit;
    end;
    SetCertParams;
    if NOT X509Certs1.AcmeCheckOrder then exit;
    LabelAcme1Cert.Caption := 'Let''s Encrypt three month SSL certificate' +
                                 icsCRLF + 'Domain: ' + X509Certs1.CertCommonName;
    doAcmeOrderV1.Enabled := true;
end;

procedure TX509CertsForm.doAcmeCheckOrderV2Click(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateAccount then begin
        AddLog('Must Register Acme Account First');
        Exit;
    end;
    SetCertParams;
    if NOT X509Certs1.AcmeCheckOrder then exit;
    LabelAcme2Cert.Caption := 'Let''s Encrypt three month SSL certificate' +
                                 icsCRLF + 'Domain: ' + X509Certs1.CertCommonName;
    doAcmeOrderV2.Enabled := true;
end;

procedure TX509CertsForm.doAcmeOrderV1Click(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateChecked then begin
        AddLog('Must Check New Certificate First');
        Exit;
    end;
    if NOT X509Certs1.AcmeV1OrderCert then Exit;
    doAcmeGetCertV1.Enabled := true;
end;

procedure TX509CertsForm.doAcmeOrderV2Click(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateChecked then begin
        AddLog('Must Check New Certificate First');
        Exit;
    end;
    if NOT X509Certs1.AcmeV2OrderCert then Exit;
    doAcmeGetCertV2.Enabled := true;

end;

procedure TX509CertsForm.doAcmeGetCertV1Click(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateChallgPend then begin
        AddLog('Acme Challenge Not Started, Must Order First');
        Exit;
    end;
    X509Certs1.AcmeV1GetCert;
end;

procedure TX509CertsForm.doAcmeGetCertV2Click(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateChallgPend then begin
         AddLog('Acme Challenge Not Started, Must Order First');
        Exit;
    end;
    X509Certs1.AcmeV2GetCert;
end;

procedure TX509CertsForm.doCCProfileClick(Sender: TObject);
begin
    ResetButtons;
    SetOAParams;
    X509Certs1.SupplierProto := SuppProtoCertCentre;
    X509Certs1.SupplierServer := trim(CertCentreServer.Text);
    X509Certs1.DirCertWork := IncludeTrailingPathDelimiter (DirCertCenConf.Text) ;

    X509Certs1.SetCertCentre;
    if X509Certs1.CCGetProfile then begin
        doCertCentreCheck.Enabled := True;
        doCertCentreAlways.Enabled := True;
        doCertCentreOrders.Enabled := True;
        doCertCentreCollect.Enabled := True;
        doCertCentreCancel.Enabled := True;
        doCertCentreRevoke.Enabled := True;
        if (CertCentreProducts.Items.Count <> X509Certs1.ProductList.Count) then
            CertCentreProducts.Items.Assign(X509Certs1.ProductList);
    end
    else
        FPendCCProfile := True;  // waiting for oAuth2 response
end;

procedure TX509CertsForm.CertCentreProductsClick(Sender: TObject);
begin
    if (X509Certs1.IssueState < IssStateAccount) or
         (X509Certs1.SupplierProto <> SuppProtoCertCentre) then Exit;
    if CertCentreProducts.ItemIndex < 0 then Exit;
    if X509Certs1.CCGetOneProduct(CertCentreProducts.Items[CertCentreProducts.ItemIndex]) then begin
        LabelCertInfo.Caption := X509Certs1.ProductInfo;
        AddLog(X509Certs1.ProductInfo);
    end;
end;

procedure TX509CertsForm.doCertCentreCheckClick(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateAccount then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    if CertCentreProducts.ItemIndex < 0 then begin
        AddLog('Must Select a Certificate Product First');
        Exit;
    end;
    SetCertParams;
    CertCentreOrderId.Text := '';
    X509Certs1.SuppOrderId := '';
    X509Certs1.SuppOrderRef := CertCentreOrderRef.Text;
    CertCentreProductsClick(Self) ;
    X509Certs1.SuppCertProduct := CertCentreProducts.Items[CertCentreProducts.ItemIndex];
    if X509Certs1.CCCheckOrder then begin
        LabelCertInfo.Caption := LabelCertInfo.Caption + IcsCRLF +
            'Domain: ' + X509Certs1.CertCommonName + ', quote price ' + X509Certs1.ProductQuote;
        CertBuy.Enabled := true;
        doCertCentreOther.Enabled := true;
        if X509Certs1.CCGetApproverEmail then begin
            if (CertCentreApprovEmail.Items.Text <> X509Certs1.ApproverEmails.Text) then
                CertCentreApprovEmail.Items.Assign(X509Certs1.ApproverEmails);
            CertCentreApprovEmail.ItemIndex := 0;
        end;
        if X509Certs1.SuppCertChallenge = ChallEmail then begin
            CertCentreApprovEmail.ItemIndex := -1;
            CertCentreApprovEmail.Text := '';
            AddLog('Must Select a Approver Email Address');
        end;
    end;
end;

procedure TX509CertsForm.doCertCentreAlwaysClick(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateAccount then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    SetCertParams;
    X509Certs1.SuppOrderId := '';
    CertCentreOrderId.Text := '';
    X509Certs1.SuppOrderRef := CertCentreOrderRef.Text;
    if X509Certs1.SuppCertChallenge = ChallEmail then begin
        AddLog('Email challenge not available');
        exit;
    end;
    if (X509Certs1.SuppCertChallenge = ChallFileFtp) or
                  (X509Certs1.SuppCertChallenge = ChallFileSrv) then begin
        AddLog('FTP and local server challenge not available yet');
        exit;
    end;
    X509Certs1.SuppCertProduct := 'AlwaysOnSSL.AlwaysOnSSL';
    if X509Certs1.CCOrderCert then begin
        CertCentreOrderId.Text := X509Certs1.SuppOrderId;
    end;
end;

procedure TX509CertsForm.doCertCentreOtherClick(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateChecked then begin
        AddLog('Must Check New Certificate First');
        Exit;
    end;
    if CertBuy.Text <> 'BUY' then begin
        AddLog('!! Must type BUY before ordering commercial certificates that cost money') ;
        exit ;
    end;
    SetCertParams;
    if (CertCentreApprovEmail.Text = '') then begin
         AddLog('Must Select a Approver Email Address First');
         Exit;
    end;
    X509Certs1.SuppOrderId := '';
    CertCentreOrderId.Text := '';
    X509Certs1.SuppOrderRef := CertCentreOrderRef.Text;
    if (X509Certs1.SuppCertChallenge = ChallFileFtp) or
                  (X509Certs1.SuppCertChallenge = ChallFileSrv) then begin
        AddLog('FTP and local server challenge not available yet');
        exit;
    end;
    CertBuy.Text := '';
    if X509Certs1.CCOrderCert then begin
        CertCentreOrderId.Text := X509Certs1.SuppOrderId;
        doCertCentreCollect.Enabled := True;
    end;
end;

procedure TX509CertsForm.doCertCentreCollectClick(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateAccount then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    if CertCentreOrderId.Text = '' then begin
        AddLog('CertCentre Order ID is Required');
        Exit;
    end;
    SetCertParams;
    X509Certs1.SuppOrderId := CertCentreOrderId.Text;
    if X509Certs1.CCGetOneOrder then begin
      //
    end;
end;

procedure TX509CertsForm.doCertCentreOrdersClick(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateAccount then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    if X509Certs1.CCListAllOrders then begin
        // should we display a grid ??
    end;
end;

procedure TX509CertsForm.doCertCentreCancelClick(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateAccount then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    if CertCentreOrderId.Text = '' then begin
        AddLog('CertCentre Order ID is Required');
        Exit;
    end;
    SetCertParams;
    X509Certs1.SuppOrderId := CertCentreOrderId.Text;
    if X509Certs1.CCCancelOrder (False)then begin
      //
    end;
end;

procedure TX509CertsForm.doCertCentreRevokeClick(Sender: TObject);
begin
    if X509Certs1.IssueState < IssStateAccount then begin
        AddLog('Must Get CertCentre Profile First');
        Exit;
    end;
    if CertCentreOrderId.Text = '' then begin
        AddLog('CertCentre Order ID is Required');
        Exit;
    end;
    SetCertParams;
    X509Certs1.SuppOrderId := CertCentreOrderId.Text;
    if X509Certs1.CCCancelOrder (True)then begin
      //
    end;
end;


procedure TX509CertsForm.doOACodeTokenClick(Sender: TObject);
begin
    SetOAParams;
    if OAuthAuthCode.Text = '' then begin
        AddLog('Must Specify OAuth2 Authorization Code');
        Exit;
    end;
    X509Certs1.OAGrantAuthToken(OAuthAuthCode.Text);
end;

procedure TX509CertsForm.doOARefreshNowClick(Sender: TObject);
begin
    SetOAParams;
    if OAuthRefToken.Text = '' then begin
        AddLog('Must Specify OAuth2 Refresh Token');
        Exit;
    end;
    X509Certs1.OAGrantRefresh;
end;

procedure TX509CertsForm.doTestWellKnownClick(Sender: TObject);
begin
    SetCertParams;
    X509Certs1.TestWellKnown(X509Certs1.CertCommonName, X509Certs1.DirWellKnown);
end;

procedure TX509CertsForm.doWebServerClick(Sender: TObject);
begin
//
end;

end.
