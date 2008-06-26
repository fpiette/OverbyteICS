{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Creation:     Aug 26, 2003
Description:  A small utility to export SSL certificate from IE certificate
              store to a directory using OpenSSL PEM file format.
              Make use of the ICS Delphi encapsulation for SSLEAY32.DLL &
              LIBEAY32.DLL (OpenSSL) by Francois Piette <francois.piette@overbyte.be>
              Make use of OpenSSL (http://www.openssl.org)
              Make use of the Jedi CryptoAPI2
              (http://delphi-jedi.org/Jedi:APILIBRARY:172871)(CryptoAPI2.zip).
Version:      1.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list ics-ssl@elists.org
              Follow "SSL" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003-2008 by François PIETTE
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

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Aug 26, 2003 V1.01 F. Piette <francois.piette@overbyte.be> added persistance
             to export directory and windows position & size. Also added
             compiler switches and version constant.
Aug 31, 2003 V1.02 func ParseNameProp rewritten.
Sep 04, 2003 V1.03 Added LVCert sort on column header click, and a simple
             application exception handler. Fixed 'CopyCert' bug,
             and beautyfied source.
Sep 11, 2003 V1.04 Test version for new IcsOpenSsl.DLL.
Aug 07, 2007 V1.05 ICS-SSL V6 compatibility

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsPemtool1;
{$IFNDEF USE_SSL}
    Bomb('Add USE_SSL in the define section in project options');
{$ENDIF}
{$IFDEF VER80}
    Bomb('This unit require a 32 bit compiler !');
{$ENDIF}
{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs,
  StdCtrls, IniFiles, ComCtrls, Menus, ImgList, ExtCtrls, CommCtrl,
  { Uses Jedi CryptoAPI2, see 'Description' above }
  Wcrypt2,
  { Uses ICS SSL code, see 'Description' above }
  OverbyteIcsWSocket, OverbyteIcsSsleay, OverbyteIcsLibeay,
  OverbyteIcsLibeayEx, OverbyteIcsSslX509Utils;

const
     PemToolVersion     = 105;
     PemToolDate        = 'Aug 07, 2007';
     PemToolName        = 'PEM Certificate Tool';
     CopyRight : String = '(c) 2003-2007 Arno Garrels V1.05 ';
     CaptionMain        = 'ICS PEM Certificate Tool - ';
     WM_APPSTARTUP      = WM_USER + 1;

type

  TMyX509 = class(TX509Base)
  private
    function GetNameEntryByNid(IsSubject: Boolean; ANid: Integer): String;
    function GetSubjectOName : String;
    function GetIssuerOName : String;
    function GetSubjectOUName : String;
    function GetIssuerOUName : String;
    function GetIssuerCName : String;
  public
    property SubjectOName : String read GetSubjectOName;
    property SubjectOUName : String read GetSubjectOUName;
    property IssuerOName : String read GetIssuerOName;
    property IssuerOUName : String read GetIssuerOUName;
    property IssuerCName : String read GetIssuerCName;
  end;

  TfrmPemTool1 = class(TForm)
    btnShowCert: TButton;
    pmLv: TPopupMenu;
    pmShowDetails: TMenuItem;
    pmDelete: TMenuItem;
    ImageList1: TImageList;
    OpenDlg: TOpenDialog;
    PageControl1: TPageControl;
    TabCertLv: TTabSheet;
    TabImport: TTabSheet;
    LvCerts: TListView;
    btnRefresh: TButton;
    CurrentCertDirEdit: TEdit;
    Label4: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    ComboBoxStoreType: TComboBox;
    DestDirEdit: TEdit;
    Label2: TLabel;
    CheckBoxWarnDestNotEmpty: TCheckBox;
    CheckBoxOverwriteExisting: TCheckBox;
    CheckBoxEmptyDestDir: TCheckBox;
    btnImport: TButton;
    Bevel1: TBevel;
    Label5: TLabel;
    btnDeleteCert: TButton;
    btnCopyCert: TButton;
    pmCopy: TMenuItem;
    About: TButton;
    Bevel2: TBevel;
    Label6: TLabel;
    btnImportPemFile: TButton;
    N1: TMenuItem;
    N2: TMenuItem;
    MainMenu1: TMainMenu;
    MMFile: TMenuItem;
    MMFileExit: TMenuItem;
    MMExtras: TMenuItem;
    MMExtrasCreateSelfSignedCert: TMenuItem;
    MMExtrasCreateCertRequest: TMenuItem;
    procedure btnImportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnRefreshClick(Sender: TObject);
    procedure LvCertsDblClick(Sender: TObject);
    procedure btnShowCertClick(Sender: TObject);
    procedure CurrentCertDirEditChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure btnDeleteCertClick(Sender: TObject);
    procedure btnCopyCertClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure DestDirEditChange(Sender: TObject);
    procedure btnImportPemFileClick(Sender: TObject);
    procedure LvCertsColumnClick(Sender: TObject; Column: TListColumn);
    procedure LvCertsCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure LvCertsCustomDraw(Sender: TCustomListView;
      const ARect: TRect; var DefaultDraw: Boolean);  
    procedure AppOnException(Sender: TObject; E: Exception);
    procedure MMFileExitClick(Sender: TObject);
    procedure MMExtrasCreateSelfSignedCertClick(Sender: TObject);
    procedure MMExtrasCreateCertRequestClick(Sender: TObject);
  protected
    procedure WMAppStartup(var Msg: TMessage); message WM_APPSTARTUP;  
  private
    FProgDir         : String;
    FInitialized     : Boolean;
    FCurrentCertDir  : String;
    FLVSortFlag      : Boolean;
    procedure AddListView(X: TMyX509; const Filename: String);
    procedure FillListView;
    procedure ShowCert(const FileName: String);
  public
    FIniFileName    : String;  
  end;

  function  FindPemFileName(const FileName: String): String;
  function  DirectoryExists(const Name: string): Boolean;
  function  isDirEmpty(const Path: String): Boolean;
  function  PathAddBackSlash(const Path: String): String;
  procedure EmptyDirectory(Path: String);
  
var
  frmPemTool1 : TfrmPemTool1;
  ColumnToSort: Integer;

implementation

{$R *.DFM}

uses
    OverByteIcsPemTool2, OverByteIcsPemTool3;
    
const
    SectionMainWindow    = 'MainWindow';
    SectionDisplayWindow = 'DisplayWindow';
    KeyTop               = 'Top';
    KeyLeft              = 'Left';
    KeyWidth             = 'Width';
    KeyHeight            = 'Height';
    SectionData          = 'Data';
    KeyDestinationDir    = 'DestinationDir';
    KeyCurrentCertDir    = 'CurrentCertDir';
    KeyWarnDestNotEmpty  = 'WarnDestNotEmpty';
    KeyOverwriteExisting = 'OverwriteExistingFiles';
    KeyEmptyDestDir      = 'EmptyDestDir';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Returns a CRLF-separated list if multiple entries exist }
function TMyX509.GetNameEntryByNid(IsSubject: Boolean; ANid: Integer): String;
var
    Name    : PX509_NAME;
    Entry   : PX509_NAME_ENTRY;
    Asn1    : PASN1_STRING;
    LastPos : Integer;
begin
    Result := '';
    Entry  := nil;
    if not Assigned(X509) then
        Exit;
    if IsSubject then
        Name := f_X509_get_subject_name(X509)
    else
        Name := f_X509_get_issuer_name(X509);
    if Name <> nil then begin
        LastPos := -1;
        repeat
            LastPos := f_X509_NAME_get_index_by_NID(Name, ANid, LastPos);
            if LastPos > -1 then
                Entry := f_X509_NAME_get_entry(Name, LastPos)
            else
                Break;
            if Assigned(Entry) then begin
                Asn1 := f_X509_NAME_ENTRY_get_data(Entry);
                if Assigned(Asn1) then
                    Result := Result + Asn1ToString(Asn1) + #13#10;
            end;
        until
            LastPos = -1;

        while (Length(Result) > 0) and (Result[Length(Result)] in [#13, #10]) do
            SetLength(Result, Length(Result) - 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMyX509.GetSubjectOName: String;
begin
    Result := GetNameEntryByNid(TRUE, NID_organizationName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMyX509.GetSubjectOUName: String;
begin
    Result := GetNameEntryByNid(TRUE, NID_organizationalUnitName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMyX509.GetIssuerOName: String;
begin
    Result := GetNameEntryByNid(FALSE, NID_organizationName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMyX509.GetIssuerOUName: String;
begin
    Result := GetNameEntryByNid(FALSE, NID_organizationalUnitName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMyX509.GetIssuerCName: String;
begin
    Result := GetNameEntryByNid(FALSE, NID_commonName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.AppOnException(Sender: TObject; E: Exception);
begin
    if MessageDlg(E.ClassName + ': ' + E.Message + #13#10
                + 'Exit PemTool now?',
                   mtError, [mbYes, mbNo], 0) = mrYes then
        Application.Terminate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.FormCreate(Sender: TObject);
begin
    Application.OnException := AppOnException;
    FProgDir     := ExtractFilePath(ParamStr(0));
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
    FIniFileName := PathAddBackSlash(FProgDir) + FIniFileName;
    ComboBoxStoreType.ItemIndex := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile      := TIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionMainWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionMainWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionMainWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionMainWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        DestDirEdit.Text                  := IniFile.ReadString(SectionData,
                                                                KeyDestinationDir,
                                                                'TrustedCaStore');
        CurrentCertDirEdit.Text           := IniFile.ReadString(SectionData,
                                                                KeyCurrentCertDir,
                                                                'TrustedCaStore');
        CheckBoxWarnDestNotEmpty.Checked  := IniFile.ReadBool(SectionData,
                                                              KeyWarnDestNotEmpty,
                                                              TRUE);
        CheckBoxOverwriteExisting.Checked := IniFile.ReadBool(SectionData,
                                                              KeyOverwriteExisting,
                                                              FALSE);
        CheckBoxEmptyDestDir.Checked      := IniFile.ReadBool(SectionData,
                                                              KeyEmptyDestDir,
                                                              FALSE);
        IniFile.Destroy;
        PostMessage(Handle, WM_APPSTARTUP, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.WMAppStartup(var Msg: TMessage);
begin
    frmPemTool1.Caption := CaptionMain + Trim(CurrentCertDirEdit.Text);
    PageControl1.ActivePageIndex := 0;
    FillListView;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionMainWindow, KeyTop,               Top);
    IniFile.WriteInteger(SectionMainWindow, KeyLeft,              Left);
    IniFile.WriteInteger(SectionMainWindow, KeyWidth,             Width);
    IniFile.WriteInteger(SectionMainWindow, KeyHeight,            Height);
    IniFile.WriteString(SectionData,        KeyDestinationDir,    DestDirEdit.Text);
    IniFile.WriteString(SectionData,        KeyCurrentCertDir,    CurrentCertDirEdit.Text);
    IniFile.WriteBool(SectionData,          KeyWarnDestNotEmpty,  CheckBoxWarnDestNotEmpty.Checked);
    IniFile.WriteBool(SectionData,          KeyOverwriteExisting, CheckBoxOverwriteExisting.Checked);
    IniFile.WriteBool(SectionData,          KeyEmptyDestDir,      CheckBoxEmptyDestDir.Checked);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{function PasswordCallBack(
    Buf      : PChar;
    Num      : Integer;
    RWFlag   : Integer;
    UserData : Pointer) : Integer; cdecl;
var
    Len, I : Integer;
    ErrMsg : String;
    Pass   : String;
begin
    Pass := 'test';
    Len  := Length(Pass);
    if Len >= Num then begin
        ErrMsg := Format('Certificate password length (%d) exceeds maximum (%d) - ' +
                         'password will be truncated!', [Len, Num - 1]);
        ShowMessage(ErrMsg);
        Len := Num;
     end;

     Buf[Len] := #0;
     for I := Len downto 1 do
         Buf[I -1] := Pass[I];
     Result := Len;
end;}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.ShowCert(const FileName: String);
var
    X : TX509Base;
begin
    if (FileName = '') or not FileExists(FileName) then
        raise Exception.Create('FileName empty or file doesn''t exist');
    X := TX509Base.Create(nil);
    try
        X.LoadFromPemFile(Filename);
        frmPemTool2 := TfrmPemTool2.Create(Self);
        try
            frmPemTool2.Caption          := FileName;
            frmPemTool2.Memo1.Lines.Text := X.GetRawText;
            frmPemTool2.ShowModal;
        finally
            frmPemTool2.free;
        end;
    finally
        X.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.AddListView(X: TMyX509; const Filename: String);
var
    ListItem : TListItem;
    S        : String;
begin
    with LVCerts do begin
        ListItem          := Items.Add;
        ListItem.Caption  := X.SubjectCName;
        if ListItem.Caption = '' then
            ListItem.Caption := X.SubjectOUName;
        if ListItem.Caption = '' then
            ListItem.Caption := X.SubjectOName;
        S := X.IssuerCName;
        if S = '' then
            S := X.IssuerOUName;
        if S = '' then
            S := X.IssuerOName;
        ListItem.SubItems.Add(S);
        ListItem.SubItems.Add(DateTimeToStr(X.GetValidNotAfter));
        ListItem.SubItems.Add(ExtractFileName(FileName));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.FillListView;
var
    SRec    : TSearchRec;
    CertDir : String;
    X       : TMyX509;
begin
    X := TMyX509.Create(nil);
    try
        LvCerts.Items.BeginUpdate;
        try
            LVCerts.Items.Clear;
            FCurrentCertDir := Trim(CurrentCertDirEdit.Text);
            if not DirectoryExists(FCurrentCertDir) then
                Exit;
            CertDir := PathAddBackSlash(FCurrentCertDir);
            if FindFirst(CertDir + '*.*', faAnyFile - faDirectory, SRec) = 0 then
            try
                X.LoadFromPemFile(CertDir + SRec.Name);
                AddListView(X, CertDir + SRec.Name);
                while FindNext(SRec) = 0 do begin
                    X.LoadFromPemFile(CertDir + SRec.Name);
                    AddListView(X, CertDir + SRec.Name);
                end;
            finally
                FindClose(SRec);
            end;    
        finally
            LvCerts.Items.EndUpdate;
        end;
    finally
        X.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.LvCertsDblClick(Sender: TObject);
var
    ListItem : TListItem;
    FileName : String;
begin
    with LvCerts do begin
        if Assigned(Selected) then begin
            ListItem := Items[Selected.Index];
            FileName := ListItem.SubItems[2];
            if FileName <> '' then
                FileName := PathAddBackSlash(FCurrentCertDir) + FileName;
            if FileExists(FileName) then
                ShowCert(FileName);
        end
        else
            MessageDlg('No item selected', mtError, [mbOK], 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.LvCertsColumnClick(Sender: TObject;
    Column: TListColumn);
var
    I : Integer;
begin
    Screen.Cursor := crHourGlass;
    LVCerts.Items.BeginUpdate;
    try
        if ColumnToSort = Column.Index then
            FLVSortFlag := not FLVSortFlag;
        for I := 0 to LVCerts.Columns.Count -1 do begin
             if I <> Column.Index then
                 LVCerts.Columns[I].ImageIndex := -1
             else begin
                 if FLVSortFlag then
                     LVCerts.Columns[Column.Index].ImageIndex := 1
                 else
                     LVCerts.Columns[Column.Index].ImageIndex := 2;
             end;
        end;
        ColumnToSort := Column.Index;
        with (Sender as TCustomListView) do begin
            AlphaSort;
            if Assigned(Selected) then
                Selected.MakeVisible(TRUE);
        end;
        LVCerts.Items.EndUpdate;
        Screen.Cursor := crDefault;
    except
        LVCerts.Items.EndUpdate;
        Screen.Cursor := crDefault;
        raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.LvCertsCompare(Sender: TObject; Item1,
    Item2: TListItem; Data: Integer; var Compare: Integer);
var
    Idx: Integer;
begin
    if not FLVSortFlag then begin
        if ColumnToSort = 0 then
            Compare := CompareText(Item1.Caption, Item2.Caption)
        else begin
            Idx := ColumnToSort - 1;
            if Idx = 1 then begin
                Compare := 0;
                if StrToDate(Item1.SubItems[Idx]) > StrToDate(Item2.SubItems[Idx]) then
                    Compare := 1
                else
                if StrToDate(Item1.SubItems[Idx]) < StrToDate(Item2.SubItems[Idx]) then
                    Compare := -1;
                Exit;
            end;
            Compare := CompareText(Item1.SubItems[Idx], Item2.SubItems[Idx]);
        end;
    end
    else begin
        if ColumnToSort = 0 then
            Compare := CompareText(Item2.Caption, Item1.Caption)
        else begin
            Idx := ColumnToSort - 1;
            if Idx = 1 then begin
                Compare := 0;
                if StrToDate(Item1.SubItems[Idx]) < StrToDate(Item2.SubItems[Idx]) then
                    Compare := 1
                else
                if StrToDate(Item1.SubItems[Idx]) > StrToDate(Item2.SubItems[Idx]) then
                    Compare := -1;
                Exit;
            end;
            Compare := CompareText(Item2.SubItems[Idx], Item1.SubItems[Idx]);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.LvCertsCustomDraw(Sender: TCustomListView;
    const ARect    : TRect;
    var   DefaultDraw: Boolean);
var
    I            : Integer;
    LvColumn     : TLVColumn;
    HeaderHandle : THandle;
begin
    { Display sort BMP on the right of column caption}
    { requires comctl32.dll version 4.70+            }
    HeaderHandle := GetDlgItem(LVCerts.Handle, 0);
    for I := 0 to LVCerts.Columns.Count - 1 do begin
        if (LVCerts.Columns[I].ImageIndex <> -1) then begin
            FillChar(LvColumn, SizeOf(LvColumn), #0);
            ListView_GetColumn(HeaderHandle, I, LvColumn);
            with LvColumn do begin
                iImage := LVCerts.Columns[I].ImageIndex;
                mask   := mask or LVCF_IMAGE or LVCF_FMT;
                fmt    := fmt or LVCFMT_IMAGE or LVCFMT_BITMAP_ON_RIGHT; 
                case LVCerts.Columns[I].Alignment of
                    taLeftJustify  : fmt := fmt or LVCFMT_LEFT;
                    taCenter       : fmt := fmt or LVCFMT_CENTER;
                    taRightJustify : fmt := fmt or LVCFMT_RIGHT;
                end;
            end;
            ListView_SetColumn(LVCerts.Handle, I, LvColumn);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.CurrentCertDirEditChange(Sender: TObject);
begin
    FCurrentCertDir := Trim(CurrentCertDirEdit.Text);
    frmPemTool1.Caption := CaptionMain + FCurrentCertDir;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.PageControl1Change(Sender: TObject);
begin
    case PageControl1.ActivePageIndex of
        0: frmPemTool1.Caption := CaptionMain + FCurrentCertDir;
        1: frmPemTool1.Caption := CaptionMain + Trim(DestDirEdit.Text);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.DestDirEditChange(Sender: TObject);
begin
    frmPemTool1.Caption := CaptionMain + Trim(DestDirEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnRefreshClick(Sender: TObject);
begin
    FillListView;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnShowCertClick(Sender: TObject);
var
    FileName : String;
begin
    OpenDlg.InitialDir := DestDirEdit.Text;
    OpenDlg.Filter     := 'PEM Certs *.pem|*.pem|All Files *.*|*.*';
    if OpenDlg.Execute then
        FileName := OpenDlg.FileName;
    if (FileName = '') or not FileExists(FileName) then
        Exit;
    ShowCert(FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnDeleteCertClick(Sender: TObject);
var
    ListItem : TListItem;
    FileName : String;
begin
    with LvCerts do begin
        if Assigned(Selected) then begin
            ListItem := Items[Selected.Index];
            FileName := ListItem.SubItems[2];
            if FileName <> '' then
                FileName := PathAddBackSlash(FCurrentCertDir) + FileName;
            if FileExists(FileName) then
                if MessageDlg('Delete Certificate ''' + ListItem.Caption + ''','
                            + #13#10
                            + 'file ''' + FileName + ''' now?',
                             mtWarning, [mbYes, mbNo], 0) <> mrYes then
                    Exit;
            if DeleteFile(FileName) then
                FillListView;
        end
        else
            MessageDlg('No item selected', mtError, [mbOK], 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnCopyCertClick(Sender: TObject);
var
    ListItem    : TListItem;
    FileName    : String;
    NewFileName : String;
    ClickedOK   : Boolean;
begin
    ClickedOk := FALSE;
    with LvCerts do begin
        if Assigned(Selected) then begin
            ListItem := Items[Selected.Index];
            FileName := ListItem.SubItems[2];
            if FileName <> '' then
                FileName := PathAddBackSlash(FCurrentCertDir) + FileName;
            if FileExists(FileName) then begin
                NewFileName := FileName;
                ClickedOK := InputQuery(ListItem.Caption,
                                        'Copy to: ', NewFileName);
            end;
            if ClickedOK and (CompareText(NewFileName, FileName) <> 0) then
                if CopyFile(PChar(FileName), PChar(NewFileName), TRUE) then
                    FillListView
                else
                    MessageDlg('''' + FileName + ''' ' + 'copy failed!',
                    mtError, [mbOK], 0);
        end
        else
            MessageDlg('No item selected', mtError, [mbOK], 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.AboutClick(Sender: TObject);
begin
   ShowMessage(
               PemToolName + #13#10
             +  CopyRight + ' ' + PemToolDate);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnImportClick(Sender: TObject);
var
     hSystemStore    : HCERTSTORE;
     pCertContext    : PCCERT_CONTEXT;
     pwszSystemName  : LPCWSTR;
     X               : TX509Base;
     Subject_Hash    : Cardinal;
     FileName        : String;
     Path            : String;
     Count           : Integer;
begin
    Count := 0;
    pCertContext := nil;
    Path  := Trim(DestDirEdit.Text);

    if (Path = '') or (not DirectoryExists(Path)) then begin
        ShowMessage('Invalid destination ''' + Path + '''!');
        Exit;
    end;

    if CheckBoxWarnDestNotEmpty.Checked then
        if not isDirEmpty(Path) then
            if MessageDlg('Directory ''' + Path + ''' is not empty. Continue?',
                          mtWarning,
                          [mbYes, mbNo],
                          0) <> mrYes then
                Exit;

    if CheckBoxEmptyDestDir.Checked then begin
        if MessageDlg('Any file in destination ''' + Path
                     + ''' will be deleted. Continue?',
                      mtWarning,
                      [mbYes, mbNo],
                      0) <> mrYes then
            Exit;
        EmptyDirectory(Path);
    end;

    case ComboBoxStoreType.ItemIndex of
        0 : pwszSystemName := 'CA';
        1 : pwszSystemName := 'ROOT';
        2 : pwszSystemName := 'MY';
      else
        pwszSystemName := nil;
    end;

    { Open the Windows certificate system store }
    hSystemStore := CertOpenStore(CERT_STORE_PROV_SYSTEM,
                                  0,
                                  0,
                                  CERT_SYSTEM_STORE_CURRENT_USER
                                  or CERT_STORE_READONLY_FLAG,
                                  pwszSystemName);

    if hSystemStore <> nil  then
        ShowMessage('Opened the '''
                   + ComboBoxStoreType.Items[ComboBoxStoreType.ItemIndex]
                   + ''' system store.')
    else begin
        ShowMessage('Could not open the '''
                   + ComboBoxStoreType.Items[ComboBoxStoreType.ItemIndex]
                   + ''' system store.');
        Exit;
    end;

    { Enum all the certs in the store and store them in PEM format }
    pCertContext := CertEnumCertificatesInStore(hSystemStore, pCertContext);
    LoadSsl; // Need to load the libraries here since it may be required for the call of f_d2i_X509()
    X := TX509Base.Create(nil);
    try
        while pCertContext <> nil do begin
            X.X509 := f_d2i_X509(nil, @pCertContext.pbCertEncoded,
                             pCertContext.cbCertEncoded);
            if Assigned(X.X509) then begin
                Subject_Hash := f_X509_subject_name_hash(X.X509);
                FileName := PathAddBackSlash(Path) + IntToHex(Subject_Hash, 8) + '.0';
                if not CheckBoxOverwriteExisting.Checked then
                    if FileExists(FileName) then
                        FileName := FindPemFileName(FileName);
                X.SaveToPemFile(FileName);
                Inc(Count);
            end;
            pCertContext := CertEnumCertificatesInStore(hSystemStore, pCertContext);
        end;
        ShowMessage(IntToStr(Count) + ' Certificates exported.');
    finally
        X.Free;
        UnloadSsl;
        if pCertContext <> nil then
            CertFreeCertificateContext(pCertContext);
        if hSystemStore <> nil then
            CertCloseStore(hSystemStore, CERT_CLOSE_STORE_CHECK_FLAG);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.btnImportPemFileClick(Sender: TObject);
var
    X               : TX509Base;
    Subject_Hash    : Cardinal;
    FileName        : String;
begin
    OpenDlg.InitialDir := DestDirEdit.Text;
    OpenDlg.Filter     := 'PEM Certs *.pem|*.pem|All Files *.*|*.*';
    if OpenDlg.Execute then
        FileName := OpenDlg.FileName;
    if (FileName = '') or not FileExists(FileName) then
        raise Exception.Create('FileName empty or file doesn''t exist');
    X := TX509Base.Create(nil);
    try
        X.LoadFromPemFile(FileName);
        Subject_Hash := f_X509_subject_name_hash(X.X509);
        FileName     := PathAddBackSlash(Trim(DestDirEdit.Text))
                        + IntToHex(Subject_Hash, 8) + '.0';

        if FileExists(FileName) then
            if MessageDlg('A certificate with the same subject already '
                           + 'exists in folder ''' + Trim(DestDirEdit.Text)
                           + ''' . Change file extension?' + #13#10
                           +'Click ''Yes'' to change file extension.' + #13#10
                           +'Click ''No'' to overwrite existing file.' + #13#10
                           +'Click ''Cancel'' to abort.',
                           mtWarning, [mbYes, mbNo, mbCancel], 0) = mrYES then

                FileName := FindPemFileName(FileName)
            else
                Exit;

            X.SaveToPemFile(FileName);
            ShowMessage('Certificate has been stored to ''' + FileName + '''');
    finally
        X.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FindPemFileName(const FileName: String): String;
var
    I         : Integer;
    FFileName : String;
    FExt      : String;
begin
    { If more than one CA certificate with the same name hash value exist, the }
    { extension must be different (e.g. 9d66eef0.0, 9d66eef0.1 etc). }
    FExt      := ExtractFileExt(FileName);
    FFileName := Copy(FileName, 1, length(FileName) -Length(FExt));
    I         := StrToInt(Copy(FExt, 2, MaxInt));
    Result    := FFileName + '.' + IntToStr(I);
    while FileExists(Result) do
    begin
       Inc(I);
       Result := FFileName + '.' + IntToStr(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DirectoryExists(const Name: string): Boolean; {from D5 FileCtrl.pas}
var
    Code: Integer;
begin
 {$R-}
    Code := GetFileAttributes(PChar(Name));
    Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
 {$R+}   
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function isDirEmpty(const Path: String): Boolean;
var
    SRec : TSearchRec;
begin
    Result := FindFirst(PathAddBackSlash(Path) + '*.*',
                        FaAnyFile - faDirectory,
                        SRec) <> 0;
    if not Result then
        FindClose(SRec);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure EmptyDirectory(Path: String);
var
    SRec : TSearchRec;
begin
    Path := PathAddBackSlash(Path);
    if FindFirst(Path + '*.*', faAnyFile - faDirectory, SRec) = 0 then begin
        try
            DeleteFile(Path + SRec.Name);
            while FindNext(SRec) = 0 do
                DeleteFile(Path + SRec.Name);
        finally
            FindClose(SRec);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function PathAddBackSlash(const Path: String): String;
begin
    Result := Path;
    if Path[Length(Path)] <> '\' then
         Result := Path + '\';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMFileExitClick(Sender: TObject);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasCreateSelfSignedCertClick(Sender: TObject);
begin
    frmPemTool3.ToDo := tdCreateSelfSignedCert;
    frmPemTool3.Showmodal; 
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TfrmPemTool1.MMExtrasCreateCertRequestClick(Sender: TObject);
begin
    frmPemTool3.ToDo := tdCreateCertRequest;
    frmPemTool3.Showmodal;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
