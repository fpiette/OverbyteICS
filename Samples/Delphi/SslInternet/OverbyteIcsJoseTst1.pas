{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  ICS SSL Json Object Signing (Jose) Demos
Creation:     Apr 2018
Updated:      Dec 2019
Version:      8.64
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
25 Apr 2018 - 8.54 baseline
23 Dec 2019 - 8.64 Corrected jose-ras files to jose-rsa and rsapsss to rsapss.
                   Added new Json/XML tab that allows blocks of Json or XML to
                     be copy/pasted and parsed to objects in a grid, repeatedly.
                     Also pretty prints Json, all to help Json debugging.  

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsJoseTst1;

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
  Dialogs, StdCtrls, TypInfo, ComCtrls, ExtCtrls,
  OverbyteIcsWSocket,
  OverbyteIcsUtils,
  OverbyteIcsMimeUtils,
  OverbyteIcsURL,
  OverbyteIcsSha1,
  OverbyteIcsSSLEAY,
  OverbyteIcsLibeay,
//  OverbyteIcsSslX509Utils,
  OverbyteIcsSuperObject,
  OverbyteIcsSuperXMLParser,   { V8.64 }
  OverbyteIcsSslJose;

type
  TJsonDemoForm = class(TForm)
    TextLines: TMemo;
    Label25: TLabel;
    TestHmacKey: TEdit;
    Label26: TLabel;
    Label27: TLabel;
    TestJWSAlg: TComboBox;
    doBase64Dec: TButton;
    doTestUrlEnc: TButton;
    doB64URLEn: TButton;
    doB64URLDec: TButton;
    doEncodeURL: TButton;
    doDecodeURL: TButton;
    doTestSign: TButton;
    doJWS: TButton;
    doSignHmac: TButton;
    Label1: TLabel;
    Base64Text: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    HexText: TEdit;
    doHexDec: TButton;
    doHexEncode: TButton;
    doBase64Enc: TButton;
    Label4: TLabel;
    TestPrivKeyFile: TComboBox;
    doHashDigest: TButton;
    Label5: TLabel;
    URLText: TEdit;
    PageControl: TPageControl;
    TabSheetJose: TTabSheet;
    TabSheetJson: TTabSheet;
    JsonGrid: TListView;
    JsonInput: TMemo;
    PanelButtons: TPanel;
    doParseJson: TButton;
    doLoadFile: TButton;
    doParseXML: TButton;
    CompactXML: TCheckBox;
    LogWin: TMemo;
    Label33: TLabel;
    OpenDialog: TOpenDialog;
    LogJson: TCheckBox;
    procedure doBase64DecClick(Sender: TObject);
    procedure doBase64EncClick(Sender: TObject);
    procedure doB64URLEnClick(Sender: TObject);
    procedure doB64URLDecClick(Sender: TObject);
    procedure doTestUrlEncClick(Sender: TObject);
    procedure doHexDecClick(Sender: TObject);
    procedure doHexEncodeClick(Sender: TObject);
    procedure doEncodeURLClick(Sender: TObject);
    procedure doDecodeURLClick(Sender: TObject);
    procedure doSignHmacClick(Sender: TObject);
    procedure doTestSignClick(Sender: TObject);
    procedure doJWSClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure doHashDigestClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TextLinesDblClick(Sender: TObject);
    procedure doParseClick(Sender: TObject);
    procedure doLoadFileClick(Sender: TObject);
    procedure JsonGridDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FPrivateKey: TX509Base;
    procedure AddLog (const S: string) ;
    function LoadPKeyFile(fname: string): boolean;
  end;

var
  JsonDemoForm: TJsonDemoForm;

implementation

{$R *.dfm}

Uses OverbyteIcsJoseTst2;

procedure TJsonDemoForm.FormCreate(Sender: TObject);
begin
    LoadSsl;
    FPrivateKey := TX509Base.Create(self);
end;

procedure TJsonDemoForm.FormDestroy(Sender: TObject);
begin
    FPrivateKey.Free;
end;

procedure TJsonDemoForm.AddLog (const S: string) ;
begin
    if Pos (IcsLF,S) > 0 then
        LogWin.Lines.Text := LogWin.Lines.Text + IcsCRLF + S
    else
       LogWin.Lines.Add (S) ;
end;

function TJsonDemoForm.LoadPKeyFile(fname: string): boolean;
begin
    Result := False;
    if NOT FileExists (fname) then
    begin
        AddLog ('Can not find private key: ' +  fname);
        Exit ;
    end;
    try
        FPrivateKey.ClearAll ;
        FPrivateKey.PrivateKeyLoadFromPemFile (fname, '') ;
    except
        on E:Exception do
        begin
            AddLog ('Failed to load private key file: ' + fname + ' - ' + E.Message);
            Exit ;
        end;
    end;
    if NOT FPrivateKey.IsPKeyLoaded then
    begin
        AddLog ('Failed to load private key: ' +  fname);
        Exit ;
    end;
    Result := True;
end;


procedure TJsonDemoForm.TextLinesDblClick(Sender: TObject);
begin
    TextLines.Lines.Clear;
end;

procedure TJsonDemoForm.doEncodeURLClick(Sender: TObject);
begin
    AddLog (URLEncode(trim(TextLines.Lines.Text)));
end;

procedure TJsonDemoForm.doHashDigestClick(Sender: TObject);
var
    AnsiData: AnsiString;
    Digest: AnsiString;
    Sha1Digest: SHA1DigestString;
begin
    try
        AnsiData := AnsiString(Trim(TextLines.Lines.Text));
        Sha1Digest := SHA1ofStr(AnsiData);
        AddLog ('SHA1ofStr (len ' + IntToStr(Length(Digest)) +
                ') Hex: ' + IcsBufferToHex(Sha1Digest));

        Digest := IcsHashDigest(AnsiData, Digest_sha1);
        AddLog ('IcsHashDigest Sha1 (len ' + IntToStr(Length(Digest)) +
                ') Hex: ' + IcsBufferToHex(Digest));

        Digest := IcsHashDigest(AnsiData, Digest_sha256);
        AddLog ('IcsHashDigest Sha256 (len ' + IntToStr(Length(Digest)) +
                ') Hex: ' + IcsBufferToHex(Digest));
        AddLog ('IcsHashDigest Sha256 Base4Url: ' + IcsBase64UrlEncode (String(Digest)));

        Digest := IcsHashDigest(AnsiData, Digest_sha512);
        AddLog ('IcsHashDigest Sha512 (len ' + IntToStr(Length(Digest)) +
                ') Hex: ' + IcsBufferToHex(Digest));
        AddLog ('IcsHashDigest Sha512 Base4Url: ' + IcsBase64UrlEncode (String(Digest)));

        if ICS_OPENSSL_VERSION_NUMBER >= OSSL_VER_1101 then begin  // OpenSSL 1.1.1 and later
            Digest := IcsHashDigest(AnsiData, Digest_sha3_256);
            AddLog ('IcsHashDigest Sha3-256 (len ' + IntToStr(Length(Digest)) +
                    ') Hex: ' + IcsBufferToHex(Digest));
            AddLog ('IcsHashDigest Sha3-256 Base4Url: ' + IcsBase64UrlEncode (String(Digest)));

            Digest := IcsHashDigest(AnsiData, Digest_sha3_512);
            AddLog ('IcsHashDigest Sha3-512 (len ' + IntToStr(Length(Digest)) +
                    ') Hex: ' + IcsBufferToHex(Digest));
            AddLog ('IcsHashDigest Sha3-512 Base4Url: ' + IcsBase64UrlEncode (String(Digest)));

            Digest := IcsHashDigest(AnsiData, Digest_shake128);
            AddLog ('IcsHashDigest Shake128 (len ' + IntToStr(Length(Digest)) +
                    ') Hex: ' + IcsBufferToHex(Digest));
            AddLog ('IcsHashDigest Shake128 Base4Url: ' + IcsBase64UrlEncode (String(Digest)));

            Digest := IcsHashDigest(AnsiData, Digest_shake256);
            AddLog ('IcsHashDigest Shake256 (len ' + IntToStr(Length(Digest)) +
                    ') Hex: ' + IcsBufferToHex(Digest));
            AddLog ('IcsHashDigest Shake256 Base4Url: ' + IcsBase64UrlEncode (String(Digest)));
        end;

     //   Digest := IcsHashDigest(AnsiData, Digest_None);  // should raise exception
    except
        on E:Exception do
            AddLog ('Hash digest exception - ' + E.Message);
    end;
    AddLog ('');
end;

procedure TJsonDemoForm.doHexDecClick(Sender: TObject);
begin
    AddLog (String(IcsHexToBin(AnsiString(Trim(HexText.Text)))));
    AddLog ('');
end;

procedure TJsonDemoForm.doHexEncodeClick(Sender: TObject);
begin
    AddLog (IcsLowerCase(IcsBufferToHex(AnsiString(Trim(TextLines.Lines.Text)))));
    AddLog ('');
end;

procedure TJsonDemoForm.doTestUrlEncClick(Sender: TObject);
var
    Data, S: String;
begin
    Data := Trim(TextLines.Lines.Text);
    AddLog ('Data: ' + Data);
    S := String(Base64Encode(AnsiString(Data)));
    AddLog ('Base64Encode: ' + S) ;
    S := IcsBase64UrlEncode(Data);
    AddLog ('IcsBase64UrlEncode: ' + S) ;
    S := IcsBase64UrlDecode(S);
    AddLog ('IcsBase64UrlDecode: ' + S) ;
    AddLog ('');
end;

procedure TJsonDemoForm.doB64URLEnClick(Sender: TObject);
begin
    AddLog (IcsBase64UrlEncode(Trim(TextLines.Lines.Text))) ;
    AddLog ('');
end;

procedure TJsonDemoForm.doB64URLDecClick(Sender: TObject);
begin
    AddLog (IcsBase64UrlDecode(Trim(Base64Text.Text))) ;
    AddLog ('');
end;

procedure TJsonDemoForm.doDecodeURLClick(Sender: TObject);
begin
    AddLog (URLDecode(trim(TextLines.Lines.Text)));
    AddLog ('');
end;

procedure TJsonDemoForm.doBase64DecClick(Sender: TObject);
begin
    AddLog (Base64Decode(Trim(Base64Text.Text))) ;
    AddLog ('');
end;

procedure TJsonDemoForm.doBase64EncClick(Sender: TObject);
begin
    AddLog (Base64Encode(Trim(TextLines.Lines.Text))) ;
    AddLog ('');
end;

procedure TJsonDemoForm.doSignHmacClick(Sender: TObject);
var
    AnsiData, AnsiSecret, Digest: AnsiString;
    S: String;
    Flag: Boolean;
begin
    AnsiData := AnsiString(Trim(TextLines.Lines.Text));
    AnsiSecret := AnsiString(TestHmacKey.Text);
    AddLog ('Test Data: ' + String(AnsiData) + ', HMAC Key: ' + String(AnsiSecret));
    try
        Digest := HMAC_SHA1_EX(AnsiData, AnsiSecret);
        AddLog ('HMAC_SHA1_EX (len ' + IntToStr(Length(Digest)) +
                ') Base64: ' + String(Base64Encode (PAnsiChar(Digest), Length(Digest))));

        Digest := IcsHMACDigest(AnsiData, AnsiSecret, Digest_sha1);
        AddLog ('IcsHMACDigest Sha1 (len ' + IntToStr(Length(Digest)) +
                ') Base64: ' + String(Base64Encode (PAnsiChar(Digest), Length(Digest))));

        Digest := IcsHMACDigest(AnsiData, AnsiSecret, Digest_sha256);
        AddLog ('IcsHMACDigest Sha256 (len ' + IntToStr(Length(Digest)) +
                ') Base64: ' + String(Base64Encode (PAnsiChar(Digest), Length(Digest))));

        Digest := IcsHMACDigest(AnsiData, AnsiSecret, Digest_sha512);
        AddLog ('IcsHMACDigest Sha512 (len ' + IntToStr(Length(Digest)) +
                ') Base64: ' + String(Base64Encode (PAnsiChar(Digest), Length(Digest))));

        Digest := IcsHMACDigestEx(AnsiData, AnsiSecret, Digest_sha1);
        AddLog ('IcsHMACDigestEx Sha1 (len ' + IntToStr(Length(Digest)) +
                ') Base64: ' + String(Base64Encode (PAnsiChar(Digest), Length(Digest))));

        Digest := IcsHMACDigestEx(AnsiData, AnsiSecret, Digest_sha256);
        AddLog ('IcsHMACDigestEx Sha256 (len ' + IntToStr(Length(Digest)) +
                ') Base64: ' + String(Base64Encode (PAnsiChar(Digest), Length(Digest))));

        Digest := IcsHMACDigestEx(AnsiData, AnsiSecret, Digest_sha512);
        AddLog ('IcsHMACDigestEx Sha512 (len ' + IntToStr(Length(Digest)) +
                ') Base64: ' + String(Base64Encode (PAnsiChar(Digest), Length(Digest))));

        Flag := IcsHMACDigestVerify(AnsiData, AnsiSecret, Digest, Digest_sha512);
        S := 'Failed Verify';
        if Flag then S := 'Passed Verify';
        AddLog ('IcsHMACDigestVerify (len ' + IntToStr(Length(Digest)) + ') Sha512: ' + S);
    except
        on E:Exception do
            AddLog ('Hash digest exception - ' + E.Message);
    end;
    AddLog ('');
end;

procedure TJsonDemoForm.doTestSignClick(Sender: TObject);
var
    AnsiData, Signature: AnsiString;
    PublicKeyStr, S: String;
    PubKey: TX509Base;
    Flag: Boolean;
begin
    AnsiData := AnsiString(Trim(TextLines.Lines.Text));

    try
        if NOT LoadPKeyFile (TestPrivKeyFile.Text) then Exit;
        AddLog ('Data: ' + String(AnsiData) + IcsCRLF + 'Private Key: ' + fPrivateKey.PrivateKeyInfo);

     {   Signature := IcsAsymSignDigest(AnsiData, fAcmePrivKey.PrivateKey, Digest_sha1);
        AddLog ('IcsAsymSignDigest Sha1 (len ' + IntToStr(Length(Signature)) +
                ') IcsBase64UrlEncode: ' + IcsBase64UrlEncode (Signature));
        AddLog ('IcsAsymSignDigest Sha1 (len ' + IntToStr(Length(Signature)) +
                    ') Hex: ' + IcsLowerCase(IcsBufferToHex (Signature));  }

        Signature := IcsAsymSignDigest(AnsiData, fPrivateKey.PrivateKey, Digest_sha256);
        AddLog ('IcsAsymSignDigest Sha256 (len ' + IntToStr(Length(Signature)) +
                ') IcsBase64UrlEncode: ' + IcsBase64UrlEncode (String(Signature)));
        AddLog ('IcsAsymSignDigest Sha256 (len ' + IntToStr(Length(Signature)) +
                    ') Hex: ' + IcsLowerCase(IcsBufferToHex (Signature)));
  //      TestSignDigest(AnsiData, TestPrivKeyFile.Text, 'sha256');

        PublicKeyStr := fPrivateKey.PublicKeySaveToText;
        PubKey := TX509Base.Create(Nil);
        PubKey.PublicKeyLoadFromText(PublicKeyStr);

        Flag := IcsAsymVerifyDigest(AnsiData, Signature, PubKey.PrivateKey, Digest_sha256);
     //   Flag := IcsAsymVerifyDigest(AnsiData, Signature, PublicKeyStr, Digest_sha256);
        S := 'Failed Verify';
        if Flag then S := 'Passed Verify';
        AddLog ('IcsAsymVerifyDigest Sha256: ' + S);
    except
        on E:Exception do
            AddLog ('Signing exception - ' + E.Message);
    end;
    AddLog ('');
end;

procedure TJsonDemoForm.doJWSClick(Sender: TObject);
var
    Data, S, KwkPub, Mykid, Myalg: String;
    JoseAlg: TJoseAlg;
    RespJson: ISuperObject;
begin
    Data := Trim(TextLines.Lines.Text);
    AddLog ('Data: ' + Data);
    try
        Mykid := TimeToStr(Now);
        if TestJWSAlg.ItemIndex < 1 then Exit;
        JoseAlg := TJoseAlg(TestJWSAlg.ItemIndex);
        if JoseAlg >= jsigRsa256 then begin
            if NOT LoadPKeyFile (TestPrivKeyFile.Text) then Exit;
        end;
        MyAlg := IcsJoseFindAlg(JoseAlg, FPrivateKey.PrivateKey);
        if JoseAlg >= jsigRsa256 then
        begin
            AddLog ('Alg: ' + TestJWSAlg.Items[TestJWSAlg.ItemIndex] +
                            ', Private Key: ' + FPrivateKey.PrivateKeyInfo);
            AddLog ( FPrivateKey.GetPKeyRawText);  { V8.64 }
            KwkPub := IcsJoseJWKPubKey(FPrivateKey.PrivateKey, Myalg, Mykid, 'sig');
            AddLog ('IcsJoseJWKPkey Raw: ' + KwkPub);     { V8.64 }
            AddLog ('');
            RespJson := SO(KwkPub);
            AddLog ('IcsJoseJWKPkey: ' + RespJson.AsJson(true, false));
        end
        else
        begin
            AddLog ('Alg: ' + TestJWSAlg.Items[TestJWSAlg.ItemIndex] +
                                        ', HMAC secret: ' + TestHmacKey.Text );
            KwkPub := IcsJoseJWKHmac(TestHmacKey.Text, Myalg, Mykid, 'sig');
            RespJson := SO(KwkPub);
            AddLog ('IcsJoseJWKMac: ' + RespJson.AsJson(true, false));
        end;
        AddLog ('');

        S := IcsJoseHeader(Myalg, 'jws', KwkPub, '', 'Nonce');
        AddLog ('IcsJoseHeader Raw: ' + S);
        AddLog ('');
        RespJson := SO(S);
        AddLog ('IcsJoseHeader: ' + RespJson.AsJson(true, false));
        AddLog ('');

        S := IcsJoseJWSComp(JoseAlg, Data, TestHmacKey.Text, FPrivateKey.PrivateKey,
                                                    'jws', KwkPub, '', 'Nonce');
        AddLog ('IcsJoseJWS Compact JWK: ' + S) ;
        S := IcsJoseJWSComp(JoseAlg, Data, TestHmacKey.Text, FPrivateKey.PrivateKey,
                                                    'jws', '', MyKid, 'Nonce');
        AddLog ('IcsJoseJWS Compact KID: ' + S) ;
        AddLog ('');

        S := IcsJoseJWSJson(JoseAlg, Data, TestHmacKey.Text, FPrivateKey.PrivateKey,
                                                    'jws', KwkPub, '', 'Nonce');
        RespJson := SO(S);
        AddLog ('IcsJoseJWS Json JWK: ' + RespJson.AsJson(true, false));

        S := IcsJoseJWSJson(JoseAlg, Data, TestHmacKey.Text, FPrivateKey.PrivateKey,
                                                    'jws', '', MyKid, 'Nonce');
        RespJson := SO(S);
        AddLog ('IcsJoseJWS Json KID: ' + RespJson.AsJson(true, false));
        AddLog ('');

   {     S := TestJoseJWSJson(JoseAlg, AnsiData, TestPrivKeyFile.Text,
                                                    'jws', KwkPub, '', 'Nonce');
        RespJson := SO(S);
        AddLog ('TestJoseJWS Json JWK: ' + RespJson.AsJson(true, false)); }
    except
        on E:Exception do
            AddLog ('Json Web Signature exception - ' + E.Message);
    end;
    AddLog ('');
end;

procedure TJsonDemoForm.doLoadFileClick(Sender: TObject);
begin
    if OpenDialog.Execute then begin
        JsonInput.Lines.LoadFromFile(OpenDialog.FileName);
    end;
end;

procedure TJsonDemoForm.doParseClick(Sender: TObject);
var
    JsonItem: TSuperAvlEntry;
    JsonObj, RespObj: ISuperObject;
    JsonEnum: TSuperAvlIterator;
    I, CWid: integer;
    FirstCol, FirstRow: Boolean;
    SourceTxt: WideString;
    CVal: String;
begin
    RespObj := Nil;
    JsonGrid.Items.Clear;
    SourceTxt := JsonInput.Lines.Text;

  // look for Json response }
    if ((Pos('{', SourceTxt) > 0) or (Sender = doParseJson)) then begin
        try
            RespObj := TSuperObject.ParseString(PWideChar(SourceTxt), True);
        except
            on E:Exception do
                AddLog('Error parsing Json: ' + E.Message);
        end;
        if Assigned(RespObj) then
            AddLog ('Json main content type: ' +
               GetEnumName(TypeInfo(TSuperType), Ord(RespObj.DataType)));
    end;

  // V8.64 look for XML response }
    if ((Pos('<?xml version=', SourceTxt) > 0) or (Sender = doParseXML)) then begin
        try
            RespObj := XMLParseString(SourceTxt, CompactXML.Checked);
        except
            on E:Exception do
                AddLog('Error parsing XML: ' + E.Message);
        end;
    end;
    if NOT Assigned(RespObj) then Exit;  // nothing to show

  // parse Json or XML response to grid
    if LogJson.Checked then AddLog (RespObj.AsJson (True, False));  // formatted
    try
     // note that values containing objects are displayed as raw Json
        if RespObj.DataType = stObject then begin
            JsonGrid.Columns.Clear;
            with JsonGrid.Columns.Add do begin
                Caption := 'Name';
                Width := 100;
            end;
            with JsonGrid.Columns.Add do begin
                Caption := 'Type';
                Width := 70;
            end;
            with JsonGrid.Columns.Add do begin
                Caption := 'Value';
                Width := 1000;
            end;
            with JsonGrid.Columns.Add do begin
                Caption := '';
                Width := 100;
            end;
            JsonEnum := RespObj.AsObject.GetEnumerator;
            try
                while JsonEnum.MoveNext do begin
                    JsonItem := JsonEnum.GetIter;
                    with JsonGrid.Items.Add do begin
                        Caption := JsonItem.Name;
                        SubItems.Add(GetEnumName(TypeInfo(TSuperType),
                                                Ord(JsonItem.Value.DataType)));
                        CVal := JsonItem.Value.AsString;
                        SubItems.Add(CVal);
                    end;
                end;
            finally
                JsonEnum.Free;
            end;
        end;

     // one column per Value, with Name as title
        if RespObj.DataType = stArray then begin
            JsonGrid.Items.BeginUpdate;
            JsonGrid.Columns.Clear;
            FirstRow := True;
            for I := 0 to  RespObj.AsArray.Length - 1 do begin
                JsonObj := RespObj.AsArray[I];
                FirstCol := True;
                with JsonGrid.Items.Add do begin
                    JsonEnum := JsonObj.AsObject.GetEnumerator;
                    while JsonEnum.MoveNext do begin
                        JsonItem := JsonEnum.GetIter;
                        CVal := JsonItem.Value.AsString;
                        if FirstRow then begin
                            CWid := (Length(CVal) * 5) + 30;
                            if CWid > 400 then CWid := 400;
                            with JsonGrid.Columns.Add do begin
                                Caption := JsonItem.Name;
                                Width := CWid;
                            end;
                        end;
                        if FirstCol then
                            Caption := CVal
                        else
                            SubItems.Add(CVal);
                        FirstCol := False;
                    end;
                end;
                FirstRow := False;
            end;
            JsonGrid.Items.EndUpdate;
        end;

    except
        on E:Exception do
            AddLog('Error parsing Json: ' + E.Message);
    end;
end;

procedure TJsonDemoForm.JsonGridDblClick(Sender: TObject);
var
    I: Integer;
begin
    if NOT Assigned(FormObject) then Exit;
    if JsonGrid.ItemIndex < 0 then Exit;
    FormObject.SubJsonGrid.Items.Clear;
    with JsonGrid.Items[JsonGrid.ItemIndex] do begin
        if (SubItems.Count >= 2) and ((SubItems[0] = 'stArray') or
                                            (SubItems[0] = 'stObject')) then
            FormObject.DispJson(SubItems[1])

     // V8.64 array may have Json object in any column, search for first, sorry ignore others...
        else if ((Pos ('{', Caption) = 1) or (Pos ('[', Caption) = 1)) then
            FormObject.DispJson(Caption)
        else if (SubItems.Count > 0) then begin
            for I := 0 to SubItems.Count - 1 do begin
                if ((Pos ('{', SubItems[I]) = 1) or (Pos ('[', SubItems[I]) = 1)) then begin
                    FormObject.DispJson(SubItems[I]);
                    break;
                end;
            end;
        end;
        FormObject.BringToFront;
    end;
end;


end.
