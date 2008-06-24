{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Object:       How to use TSmtpCli component
Creation:     09 october 1997
Version:      6.02
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2007 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

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

Updates:
Oct 26, 1997  V1.00 Released
Jan 10, 1998  V1.01 Added a Port property
Feb 15, 1998  V1.02 Added file attachement support
Mar 06, 1998  V1.03 Check for DisplayMemo overflow (100 lines allowed)
Aug 03, 1998  V2.00 Revised for new asynchronous SMTP component version
Jul 26, 2001  V2.01 Added authentification
Sep 07, 2002  V2.02 Added Cc and Bcc fields.
              Added AllInOne demo to show how to "chain" several operations
              using OnRequest done, avoiding any wait loop. This is how event
              driven operation has to be done.
Sep 15, 2002  V2.02 Corrected typo error in BuildRcptList where CcEdi was used
              where ToEdit should.
              Thanks to konstantinos.Kokkorogiannis@diala.greenpeace.org
Apr 08, 2003  V2.03 Arno Garrels <arno.garrels@gmx.de> made some useful
              changes:
              Search for 04/06/2003 Property AuthComboBox.ItemIndex removed
              from dfm, caused error message with older Delphi versions.
              AuthComboBox.Items "smtpAuthAutoSelect" added.
Apr 19, 2003  V2.04 Replaced BuildRcptList by the new RcptNameAdd component
              method.
May 05, 2003  V2.06 Changed the way data is saved to INI file to allow bigger
              messages.
Aug 23, 2004  V2.07 Removed unused units
Mar 12, 2005  V2.08 Added CinfirmCheckbox
Mar 19, 2006  V6.00 Demo ported from ICS-V5 to ICS-V6
Oct 29, 2006  V6.01 Fixed memory leak in PrepareEMail
              Added compiler switches and DELPHI7_UP check.
              Added D2006 memory leak detection
Nov 05, 2006  V6.02 Fixed typo error in AuthComboBox. Added NTLM.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMailSnd1;

{$I OverbyteIcsDefs.inc}
{$IFNDEF DELPHI7_UP}
    Bomb('This sample requires Delphi 7 or later');
{$ENDIF}
{$B-}                 { Enable partial boolean evaluation   }
{$T-}                 { Untyped pointers                    }
{$X+}                 { Enable extended syntax              }
{$I+}                 { Turn IO exceptions to on            }
{$H+}                 { Use long strings                    }
{$J+}                 { Allow typed constant to be modified }
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, Forms,
  IniFiles, OverbyteIcsWndControl, OverbyteIcsSmtpProt;

const
    SmtpTestVersion    = 6.02;
    CopyRight : String = ' MailSnd (c) 1997-2007 F. Piette V6.02 ';

type
  TSmtpTestForm = class(TForm)
    MsgMemo: TMemo;
    DisplayMemo: TMemo;
    ToolsPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Subject: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    HostEdit: TEdit;
    FromEdit: TEdit;
    ToEdit: TEdit;
    SubjectEdit: TEdit;
    SignOnEdit: TEdit;
    PortEdit: TEdit;
    ClearDisplayButton: TButton;
    ConnectButton: TButton;
    HeloButton: TButton;
    MailFromButton: TButton;
    RcptToButton: TButton;
    DataButton: TButton;
    AbortButton: TButton;
    QuitButton: TButton;
    MailButton: TButton;
    OpenButton: TButton;
    UsernameEdit: TEdit;
    PasswordEdit: TEdit;
    AuthComboBox: TComboBox;
    EhloButton: TButton;
    AuthButton: TButton;
    CcEdit: TEdit;
    BccEdit: TEdit;
    AllInOneButton: TButton;
    PriorityComboBox: TComboBox;
    ConfirmCheckBox: TCheckBox;
    AttachPanel: TPanel;
    Label6: TLabel;
    FileAttachMemo: TMemo;
    InfoPanel: TPanel;
    Label7: TLabel;
    SmtpClient: TSmtpCli;
    procedure FormCreate(Sender: TObject);
    procedure ClearDisplayButtonClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure SmtpClientRequestDone(Sender: TObject; RqType: TSmtpRequest;
      Error: Word);
    procedure HeloButtonClick(Sender: TObject);
    procedure MailFromButtonClick(Sender: TObject);
    procedure RcptToButtonClick(Sender: TObject);
    procedure DataButtonClick(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
    procedure MailButtonClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EhloButtonClick(Sender: TObject);
    procedure AuthButtonClick(Sender: TObject);
    procedure AllInOneButtonClick(Sender: TObject);
    procedure SmtpClientDisplay(Sender: TObject; Msg: String);
    procedure SmtpClientGetData(Sender: TObject; LineNum: Integer;
      MsgLine: Pointer; MaxLen: Integer; var More: Boolean);
    procedure SmtpClientHeaderLine(Sender: TObject; Msg: Pointer;
      Size: Integer);
  private
    FIniFileName  : String;
    FInitialized  : Boolean;
    FAllInOneFlag : Boolean;
    procedure Display(const Msg : String);
    procedure ExceptionHandler(Sender: TObject; E: Exception);
  end;

var
  SmtpTestForm: TSmtpTestForm;

implementation

{$R *.dfm}
const
    SectionData       = 'Data';
    KeyHost           = 'HostName';
    KeyPort           = 'Port';
    KeyFrom           = 'From';
    KeyTo             = 'To';
    KeyCc             = 'Cc';
    KeyBcc            = 'Bcc';
    KeySubject        = 'Subject';
    KeySignOn         = 'SignOn';
    KeyUser           = 'UserName';
    KeyPass           = 'Password';
    KeyAuth           = 'Authentification';
    KeyPriority       = 'Priority';
    KeyConfirm        = 'Confirm';
    SectionWindow     = 'Window';
    KeyTop            = 'Top';
    KeyLeft           = 'Left';
    KeyWidth          = 'Width';
    KeyHeight         = 'Height';
    SectionFileAttach = 'Files';
    KeyFileAttach     = 'File';
    SectionMsgMemo    = 'Message';
    KeyMsgMemo        = 'Msg';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure SaveStringsToIniFile(
    const IniFileName : String;
    const IniSection  : String;
    const IniKey      : String;
    Strings           : TStrings);
var
    IniFile : TIniFile;
    nItem   : Integer;
begin
    if (IniFileName = '') or (IniSection = '') or (IniKey = '') or
       (not Assigned(Strings)) then
        Exit;
    IniFile := TIniFile.Create(IniFileName);
    IniFile.EraseSection(IniSection);
    if Strings.Count <= 0 then
        IniFile.WriteString(IniSection, IniKey + 'EmptyFlag', 'Empty')
    else
        for nItem := 0 to Strings.Count - 1 do
            IniFile.WriteString(IniSection,
                                IniKey + IntToStr(nItem),
                                Strings.Strings[nItem]);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return FALSE if non existant in IniFile                                   }
function LoadStringsFromIniFile(
    const IniFileName : String;
    const IniSection  : String;
    const IniKey      : String;
    Strings           : TStrings) : Boolean;
var
    IniFile : TIniFile;
    nItem   : Integer;
    I       : Integer;
    Buf     : String;
begin
    Result := TRUE;
    if (IniFileName = '') or (IniSection = '') or (IniKey = '') or
       (not Assigned(Strings)) then
        Exit;
    Strings.Clear;
    IniFile := TIniFile.Create(IniFileName);
    try
        if IniFile.ReadString(IniSection, IniKey + 'EmptyFlag', '') <> '' then
             Exit;
        IniFile.ReadSectionValues(IniSection, Strings);
    finally
        IniFile.Free;
    end;
    nItem := Strings.Count - 1;
    while nItem >= 0 do begin
        Buf := Strings.Strings[nItem];
        if CompareText(IniKey, Copy(Buf, 1, Length(IniKey))) <> 0 then
            Strings.Delete(nItem)
        else begin
            if not (Buf[Length(IniKey) + 1] in ['0'..'9']) then
                Strings.Delete(nItem)
            else begin
                I := Pos('=', Buf);
                Strings.Strings[nItem] := Copy(Buf, I + 1, Length(Buf));
            end;
        end;
        Dec(nItem);
    end;
    Result := (Strings.Count <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display a message in display memo box, making sure we don't overflow it.  }
procedure TSmtpTestForm.Display(const Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            { We preserve only 200 lines }
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        { Makes last line visible }
        {$IFNDEF VER80}
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
        {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.FormCreate(Sender: TObject);
begin
    Application.OnException := ExceptionHandler;
    DisplayMemo.Clear;
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
{$IFDEF DELPHI10}
    // BDS2006 has built-in memory leak detection and display
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile := TIniFile.Create(FIniFileName);
        HostEdit.Text    := IniFile.ReadString(SectionData, KeyHost,
                                               'localhost');
        PortEdit.Text    := IniFile.ReadString(SectionData, KeyPort,
                                               'smtp');
        FromEdit.Text    := IniFile.ReadString(SectionData, KeyFrom,
                                               'first.last@company.com');
        ToEdit.Text      := IniFile.ReadString(SectionData, KeyTo,
                                               'john.doe@acme;tartempion@brol.fr');
        CcEdit.Text      := IniFile.ReadString(SectionData, KeyCc,
                                               '');
        BccEdit.Text     := IniFile.ReadString(SectionData, KeyBcc,
                                               'francois.piette@swing.be');
        SubjectEdit.Text := IniFile.ReadString(SectionData, KeySubject,
                                               'This is the message subject');
        SignOnEdit.Text  := IniFile.ReadString(SectionData, KeySignOn,
                                               'your name');
        UsernameEdit.Text :=  IniFile.ReadString(SectionData, KeyUser,
                                               'account name');
        PasswordEdit.Text      :=  IniFile.ReadString(SectionData, KeyPass,
                                               'account password');
        AuthComboBox.ItemIndex     := IniFile.ReadInteger(SectionData, KeyAuth, 0);
        PriorityComboBox.ItemIndex := IniFile.ReadInteger(SectionData, KeyPriority, 2);
        ConfirmCheckBox.Checked    := Boolean(IniFile.ReadInteger(SectionData, KeyConfirm, 0));

        if not LoadStringsFromIniFile(FIniFileName, SectionFileAttach,
                                      KeyFileAttach, FileAttachMemo.Lines) then
        FileAttachMemo.Text := 'ics_logo.gif' + #13#10 + 'fp_small.gif';
        if not LoadStringsFromIniFile(FIniFileName, SectionMsgMemo,
                                      KeyMsgMemo, MsgMemo.Lines) then
            MsgMemo.Text :=
            'This is the first line' + #13#10 +
            'Then the second one' + #13#10 +
            'The next one is empty' + #13#10 +
            '' + #13#10 +
            'The next one has only a single dot' + #13#10 +
            '.' + #13#10 +
            'Finally the last one' + #13#10;
        Top    := IniFile.ReadInteger(SectionWindow, KeyTop,    (Screen.Height - Height) div 2);
        Left   := IniFile.ReadInteger(SectionWindow, KeyLeft,   (Screen.Width - Width) div 2);
        Width  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);

        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyHost,      HostEdit.Text);
    IniFile.WriteString(SectionData, KeyPort,      PortEdit.Text);
    IniFile.WriteString(SectionData, KeyFrom,      FromEdit.Text);
    IniFile.WriteString(SectionData, KeyTo,        ToEdit.Text);
    IniFile.WriteString(SectionData, KeyCc,        CcEdit.Text);
    IniFile.WriteString(SectionData, KeyBcc,       BccEdit.Text);
    IniFile.WriteString(SectionData, KeySubject,   SubjectEdit.Text);
    IniFile.WriteString(SectionData, KeySignOn,    SignOnEdit.Text);
    IniFile.WriteString(SectionData, KeyUser,      UsernameEdit.Text);
    IniFile.WriteString(SectionData, KeyPass,      PasswordEdit.Text);
    IniFile.WriteInteger(SectionData, KeyAuth,     AuthComboBox.ItemIndex);
    IniFile.WriteInteger(SectionData, KeyPriority, PriorityComboBox.ItemIndex);
    IniFile.WriteInteger(SectionData, KeyConfirm,  Ord(ConfirmCheckBox.Checked));
    SaveStringsToIniFile(FIniFileName, SectionFileAttach,
                         KeyFileAttach, FileAttachMemo.Lines);
    SaveStringsToIniFile(FIniFileName, SectionMsgMemo,
                         KeyMsgMemo, MsgMemo.Lines);
    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.SmtpClientDisplay(Sender: TObject; Msg: String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.SmtpClientGetData(
    Sender  : TObject;
    LineNum : Integer;
    MsgLine : Pointer;
    MaxLen  : Integer;
    var More: Boolean);
var
    Len : Integer;
begin
    if LineNum > MsgMemo.Lines.count then
        More := FALSE
    else begin
        Len := Length(MsgMemo.Lines[LineNum - 1]);
        { Truncate the line if too long (should wrap to next line) }
        if Len >= MaxLen then
            StrPCopy(MsgLine, Copy(MsgMemo.Lines[LineNum - 1], 1, MaxLen - 1))
        else
            StrPCopy(MsgLine, MsgMemo.Lines[LineNum - 1]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.SmtpClientHeaderLine(
    Sender : TObject;
    Msg    : Pointer;
    Size   : Integer);
begin
    { This demonstrate how to add a line to the message header              }
    { Just detect one of the header lines and add text at the end of this   }
    { line. Use #13#10 to form a new line                                   }
    { Here we check for the From: header line and add a Comments: line      }
    if StrLIComp(Msg, 'From:', 5) = 0 then
        StrCat(Msg, #13#10 + 'Comments: This is a test');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.ClearDisplayButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.ExceptionHandler(Sender: TObject; E: Exception);
begin
    Application.ShowException(E);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Connect to the mail server }
procedure TSmtpTestForm.ConnectButtonClick(Sender: TObject);
begin
    FAllInOneFlag          := FALSE;
    SmtpClient.Host        := HostEdit.Text;
    SmtpClient.Port        := PortEdit.Text;
    SmtpClient.HdrPriority := TSmtpPriority(PriorityComboBox.ItemIndex);
    SmtpClient.Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Send HELO command with our local identification }
procedure TSmtpTestForm.HeloButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := FALSE;
    SmtpClient.SignOn          := SignOnEdit.Text;
    SmtpClient.Helo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.EhloButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := FALSE;
    SmtpClient.SignOn          := SignOnEdit.Text;
    SmtpClient.EHlo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.AuthButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := FALSE;
    SmtpClient.Username        := UsernameEdit.Text;
    SmtpClient.Password        := PasswordEdit.Text;
    SmtpClient.AuthType        := TSmtpAuthType(AuthComboBox.ItemIndex);
    SmtpClient.Auth;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ If smtpAuthNone is seleted then Open combines methods Connect and Helo.   }
{  If any other authentication type is selected then Open combines methods  }
{  Connect, Ehlo and Auth.                                                  }
procedure TSmtpTestForm.OpenButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := FALSE;
    SmtpClient.Host            := HostEdit.Text;
    SmtpClient.Port            := PortEdit.Text;
    SmtpClient.SignOn          := SignOnEdit.Text;
    SmtpClient.Username        := UsernameEdit.Text;
    SmtpClient.Password        := PasswordEdit.Text;
    SmtpClient.AuthType        := TSmtpAuthType(AuthComboBox.ItemIndex);
    SmtpClient.HdrPriority     := TSmtpPriority(PriorityComboBox.ItemIndex);
    SmtpClient.Open;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Send originator }
procedure TSmtpTestForm.MailFromButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := FALSE;
    SmtpClient.FromName        := FromEdit.Text;
    SmtpClient.MailFrom;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Send recipients }
procedure TSmtpTestForm.RcptToButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := FALSE;
    SmtpClient.RcptName.Clear;
    SmtpClient.RcptNameAdd(ToEdit.Text, CcEdit.Text, BccEdit.text);
    SmtpClient.RcptTo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Send text and attached files to mail server }
procedure TSmtpTestForm.DataButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := FALSE;
    SmtpClient.RcptName.Clear;
    SmtpClient.RcptNameAdd(ToEdit.Text, CcEdit.Text, BccEdit.text);
    SmtpClient.HdrFrom         := FromEdit.Text;
    SmtpClient.HdrTo           := ToEdit.Text;
    SmtpClient.HdrCc           := CcEdit.Text;
    SmtpClient.HdrSubject      := SubjectEdit.Text;
    SmtpClient.EmailFiles      := FileAttachMemo.Lines;
    SmtpClient.ConfirmReceipt  := ConfirmCheckBox.Checked;
    SmtpClient.Data;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ MailFrom, RcptTo and Data methods combined }
procedure TSmtpTestForm.MailButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := FALSE;
    SmtpClient.RcptName.Clear;
    SmtpClient.RcptNameAdd(ToEdit.Text, CcEdit.Text, BccEdit.text);
    SmtpClient.HdrFrom         := FromEdit.Text;
    SmtpClient.HdrTo           := ToEdit.Text;
    SmtpClient.HdrCc           := CcEdit.Text;
    SmtpClient.HdrSubject      := SubjectEdit.Text;
    SmtpClient.SignOn          := SignOnEdit.Text;
    SmtpClient.FromName        := FromEdit.Text;
    SmtpClient.EmailFiles      := FileAttachMemo.Lines;
    SmtpClient.Host            := HostEdit.Text;
    SmtpClient.Port            := PortEdit.Text;
    SmtpClient.Mail;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.QuitButtonClick(Sender: TObject);
begin
    FAllInOneFlag              := FALSE;
    SmtpClient.Quit;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.AbortButtonClick(Sender: TObject);
begin
    FAllInOneFlag := FALSE;
    SmtpClient.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.SmtpClientRequestDone(
    Sender : TObject;
    RqType : TSmtpRequest;
    Error  : Word);
begin
    { For every operation, we display the status }
    if (Error > 0) and  (Error < 10000) then
        Display('RequestDone Rq=' + IntToStr(Ord(RqType)) +
                    ' Error='+ SmtpClient.ErrorMessage)
    else
        Display('RequestDone Rq=' + IntToStr(Ord(RqType)) +
                            ' Error='+ IntToStr(Error));
    { Check if the user has asked for "All-In-One" demo }
    if not FAllInOneFlag then
        Exit;             { No, nothing more to do here }
    { We are in "All-In-One" demo, start next operation }
    { But first check if previous one was OK            }
    if Error <> 0 then begin
        FAllInOneFlag := FALSE;   { Terminate All-In-One demo }
        Display('Error, stoped All-In-One demo');
        Exit;
    end;
    case RqType of
    smtpConnect:  begin
                      if SmtpClient.AuthType = smtpAuthNone then
                          SmtpClient.Helo
                      else
                          SmtpClient.Ehlo;
                  end;
    smtpHelo:     SmtpClient.MailFrom;
    smtpEhlo:     SmtpClient.Auth;
    smtpAuth:     SmtpClient.MailFrom;
    smtpMailFrom: SmtpClient.RcptTo;
    smtpRcptTo:   SmtpClient.Data;
    smtpData:     SmtpClient.Quit;
    smtpQuit:     Display('All-In-One done !');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSmtpTestForm.AllInOneButtonClick(Sender: TObject);
begin
    if SmtpClient.Connected then begin
        MessageBeep(MB_OK);
        Display('All-In-One demo start in non connected state.');
        Display('Please quit or abort the connection first.');
        Exit;
    end;

    FAllInOneFlag          := TRUE;

    { Initialize all SMTP component properties from our GUI }
    SmtpClient.Host           := HostEdit.Text;
    SmtpClient.Port           := PortEdit.Text;
    SmtpClient.SignOn         := SignOnEdit.Text;
    SmtpClient.FromName       := FromEdit.Text;
    SmtpClient.HdrFrom        := FromEdit.Text;
    SmtpClient.HdrTo          := ToEdit.Text;
    SmtpClient.HdrCc          := CcEdit.Text;
    SmtpClient.HdrSubject     := SubjectEdit.Text; { + #13#10#9 + ' Testing continuation line !'};
    SmtpClient.EmailFiles     := FileAttachMemo.Lines;
    SmtpClient.AuthType       := TSmtpAuthType(AuthComboBox.ItemIndex);
    SmtpClient.Username       := UsernameEdit.Text;
    SmtpClient.Password       := PasswordEdit.Text;
    SmtpClient.HdrPriority    := TSmtpPriority(PriorityComboBox.ItemIndex);
    SmtpClient.ConfirmReceipt := ConfirmCheckBox.Checked;
    { Recipient list is computed from To, Cc and Bcc fields }
    SmtpClient.RcptName.Clear;
    SmtpClient.RcptNameAdd(ToEdit.Text, CcEdit.Text, BccEdit.text);
    Display('Connecting to SMTP server...');
    { Start first operation to do to send an email          }
    { Next operations are started from OnRequestDone event  }
    SmtpClient.Connect
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
