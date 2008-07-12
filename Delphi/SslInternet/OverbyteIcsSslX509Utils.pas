{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Creation:     Aug 26, 2007
Description:
Version:      1.01
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list ics-ssl@elists.org
              Follow "SSL" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2007-2008 by François PIETTE
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
Jul 11, 2008 v1.01 RTT <pdfe@oniduo.pt> contributed function CreateCertRequest(),
             slightly modified by A. Garrels.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslX509Utils;

interface

uses
    Windows, SysUtils, OverbyteIcsSSLEAY, OverbyteIcsLibeay, OverbyteIcsLibeayEx;

procedure CreateCertRequest(const RequestFileName, KeyFileName, Country,
  State, Locality, Organization, OUnit, CName, Email: String; Bits: Integer);
procedure CreateSelfSignedCert(const FileName, Country, State,
  Locality, Organization, OUnit, CName, Email: String; Bits: Integer;
  IsCA: Boolean; Days: Integer);


implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CreateSelfSignedCert(const FileName, Country, State,
    Locality, Organization, OUnit, CName, Email: String;
    Bits: Integer; IsCA: Boolean; Days: Integer);
var
    X         : PX509;
    PK        : PEVP_PKEY;
    Rsa       : PRSA;
    Name      : PX509_NAME;
    FileBio   : PBIO;
    Ex        : PX509_EXTENSION;
begin
    FileBio := nil;
    X       := nil;
    //PK      := nil;
    //Name    := nil;
    //Ex      := nil;
    if not LibeayExLoaded then
    begin
        LoadLibeayEx;
        IcsRandPoll;
    end;
    PK := f_EVP_PKEY_new;
    if not Assigned(PK) then
        raise Exception.Create('Could not create key object');
    try
        Rsa := f_RSA_generate_key(Bits, RSA_F4, nil{callback}, nil);
        if not Assigned(Rsa) then
            raise Exception.Create('Failed to generate rsa key');

        if f_EVP_PKEY_assign(PK, EVP_PKEY_RSA, PChar(Rsa)) = 0 then
        begin
            f_RSA_free(Rsa);
            raise Exception.Create('Failed to assign rsa key to key object');
        end;

        X := f_X509_new;
        if not Assigned(X) then
            raise Exception.Create('Cert object nil');

        f_X509_set_version(X, 2);
        f_ASN1_INTEGER_set(f_X509_get_serialNumber(X), 0{serial});
        f_X509_gmtime_adj(f_Ics_X509_get_notBefore(X), 0);
        f_X509_gmtime_adj(f_Ics_X509_get_notAfter(X), 60 * 60 * 24 * Days);
        f_X509_set_pubkey(X, PK);

        Name := f_X509_get_subject_name(X);
        if not Assigned(Name) then
            raise Exception.Create('Function "f_X509_get_subject_name" failed');

        { This function creates and adds the entry, working out the
        correct string type and performing checks on its length.
        Normally we'd check the return value for errors...  	}
        if Length(CName) > 0 then
            f_X509_NAME_add_entry_by_txt(Name, PChar('CN'),
                                         MBSTRING_ASC, PChar(CName), -1, -1, 0);
        if Length(OUnit) > 0 then
            f_X509_NAME_add_entry_by_txt(Name, PChar('OU'),
                                         MBSTRING_ASC, PChar(OUnit), -1, -1, 0);
        if Length(State) > 0 then
            f_X509_NAME_add_entry_by_txt(Name, PChar('ST'),
                                         MBSTRING_ASC, PChar(State), -1, -1, 0);
        if Length(Organization) > 0 then
            f_X509_NAME_add_entry_by_txt(Name, PChar(String('O')),
                                  MBSTRING_ASC, PChar(Organization), -1, -1, 0);
        if Length(Country) > 0 then
              f_X509_NAME_add_entry_by_txt(Name, PChar(String('C')),
                                       MBSTRING_ASC, PChar(Country), -1, -1, 0);
        if Length(Locality) > 0 then
            f_X509_NAME_add_entry_by_txt(Name, PChar(String('L')),
                                      MBSTRING_ASC, PChar(Locality), -1, -1, 0);
        if Length(Email) > 0 then
            f_X509_NAME_add_entry_by_NID(Name, NID_pkcs9_emailAddress,
                                         MBSTRING_ASC, PChar(Email), -1, -1, 0);

        { Its self signed so set the issuer name to be the same as the
        subject. }
        f_X509_set_issuer_name(X, Name);

        {* Add extension using V3 code: we can set the config file as NULL
        * because we wont reference any other sections. We can also set
        * the context to NULL because none of these extensions below will need
        * to access it.
        *}
        { Add various extensions }
        if IsCA then
            Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_basic_constraints,
                                        PChar('critical,CA:TRUE'))
        else
            Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_basic_constraints,
                                       PChar('critical,CA:FALSE'));

        if not Assigned(Ex) then
            raise Exception.Create('Function f_X509V3_EXT_conf_nid failed');
        f_X509_add_ext(X, Ex, -1);
        f_X509_EXTENSION_free(Ex);

        (* Optional extensions

        { Purposes }
        Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_key_usage,
                                PChar('critical, keyCertSign, cRLSign'));
        f_X509_add_ext(X, Ex, -1);
        f_X509_EXTENSION_free(Ex);

        { Some Netscape specific extensions }
        Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_netscape_comment,
                                PChar('ICS Group'));
        f_X509_add_ext(X, Ex, -1);
        f_X509_EXTENSION_free(Ex);

        Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_netscape_cert_type,
                                PChar('SSL CA, S/MIME CA, Object Signing CA'));
        f_X509_add_ext(X, Ex, -1);
        f_X509_EXTENSION_free(Ex);

        {Ex := f_X509V3_EXT_conf_nid(nil, nil, NID_crl_distribution_points,
                                PChar('URI:http://www.domain.com/CRL/class1.crl'));
        f_X509_add_ext(X, Ex, -1);
        f_X509_EXTENSION_free(Ex);}

        *)

        { Sign it }
        if f_X509_sign(X, PK, f_EVP_sha1) <= 0 then
            raise Exception.Create('Failed to sign certificate');

        { We write private key as well as certificate to the same file }
        FileBio := f_BIO_new_file(PChar(FileName), PChar('w+'));
        if not Assigned(FileBio) then
            raise Exception.Create('Failed to open output file');

        { Write private key }
        { Callback, old format }
        //if f_PEM_write_bio_PrivateKey(FileBio, PK, f_EVP_des_ede3_cbc, nil, 0, @PasswordCallback, nil) = 0 then
        { Plain, old format }
        if f_PEM_write_bio_PrivateKey(FileBio, PK, nil, nil, 0, nil, nil) = 0 then
            raise Exception.Create('Failed to write private key to BIO');

        { Write certificate }
        if f_PEM_write_bio_X509(FileBio, X) = 0 then
            raise Exception.Create('Failed to write certificate to BIO');

    finally
        if Assigned(PK) then
            f_EVP_PKEY_free(PK);
        if Assigned(X) then
            f_X509_free(X);
        if Assigned(FileBio) then
            f_BIO_free(FileBio);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure CreateCertRequest(const RequestFileName, KeyFileName, Country, State,
  Locality, Organization, OUnit, CName, Email: String; Bits: Integer);

  function Add_Ext(sk : PStack; Nid : Integer; Value : PChar): Boolean;
  var
      Ext : PX509_EXTENSION;
  begin
      Ext := f_X509V3_EXT_conf_nid(nil, nil, NID, value);
      if not Assigned(Ext) then
          Result := FALSE
      else
          Result := f_sk_push(sk, Pointer(ext)) = 1;
  end;

var
    PK        : PEVP_PKEY;
    Rsa       : PRSA;
    Name      : PX509_NAME;
    FileBio   : PBIO;
    Req       : PX509_REQ;
    Exts      : PStack;
begin
    FileBio := nil;
    //Name    := nil;
    //PK      := nil;
    //exts    := nil;
    Req     := nil;

    if not LibeayExLoaded then
    begin
        LoadLibeayEx;
        IcsRandPoll;
    end;

    PK := f_EVP_PKEY_new;
    if not Assigned(PK) then
      raise Exception.Create('Could not create key object');

    try 
        Rsa := f_RSA_generate_key(Bits, RSA_F4, nil{callback}, nil);
        if not Assigned(Rsa) then
            raise Exception.Create('Failed to generate rsa key');

        if f_EVP_PKEY_assign(PK, EVP_PKEY_RSA, PChar(Rsa)) = 0 then
        begin
            f_RSA_free(Rsa);
            raise Exception.Create('Failed to assign rsa key to key object');
        end;

        Req := f_X509_Req_new;

        f_X509_REQ_set_pubkey(Req, pk);

        f_X509_REQ_set_version(Req, 2);

        Name := f_X509_REQ_get_subject_name(Req);

        { This function creates and adds the entry, working out the
          correct string type and performing checks on its length.
          Normally we'd check the return value for errors...
        }

        if Length(CName) > 0 then
            f_X509_NAME_add_entry_by_txt(Name, PChar('CN'),
                                         MBSTRING_ASC, PChar(CName), -1, -1, 0);
        if Length(OUnit) > 0 then
            f_X509_NAME_add_entry_by_txt(Name, PChar('OU'),
                                         MBSTRING_ASC, PChar(OUnit), -1, -1, 0);
        if Length(State) > 0 then
            f_X509_NAME_add_entry_by_txt(Name, PChar('ST'),
                                         MBSTRING_ASC, PChar(State), -1, -1, 0);
        if Length(Organization) > 0 then
            f_X509_NAME_add_entry_by_txt(Name, PChar(String('O')),
                                  MBSTRING_ASC, PChar(Organization), -1, -1, 0);
        if Length(Country) > 0 then
              f_X509_NAME_add_entry_by_txt(Name, PChar(String('C')),
                                       MBSTRING_ASC, PChar(Country), -1, -1, 0);
        if Length(Locality) > 0 then
            f_X509_NAME_add_entry_by_txt(Name, PChar(String('L')),
                                      MBSTRING_ASC, PChar(Locality), -1, -1, 0);
        if Length(Email) > 0 then
            f_X509_NAME_add_entry_by_NID(Name, NID_pkcs9_emailAddress,
                                         MBSTRING_ASC, PChar(Email), -1, -1, 0);

        Exts := f_sk_new_null;
        Add_Ext(Exts, NID_key_usage, 'critical, digitalSignature, keyEncipherment');

        f_X509_REQ_add_extensions(Req, Exts);

        f_sk_pop_free(Exts, @f_X509_EXTENSION_free);

        if f_X509_REQ_sign(Req, PK, f_EVP_sha1) <= 0 then
            raise Exception.Create('Failed to sign request');

        FileBio := f_BIO_new_file(PChar(KeyFileName), PChar('w+'));
        if not Assigned(FileBio) then
            raise Exception.Create('Failed to open output file');
        if f_PEM_write_bio_PrivateKey(FileBio, PK, nil, nil, 0, nil, nil) = 0 then
            raise Exception.Create('Failed to write private key to BIO');
        f_BIO_free(FileBio);
        FileBio := f_BIO_new_file(PChar(RequestFileName), PChar('w+'));
        if not Assigned(FileBio) then
            raise Exception.Create('Failed to open output file');
        { Write request }
        if f_PEM_write_bio_X509_REQ(FileBio, OverByteIcsSSLEAY.PX509_REQ(Req)) = 0 then
            raise Exception.Create('Failed to write certificate to BIO');

    finally
      if Assigned(PK) then
        f_EVP_PKEY_free(PK);
      if Assigned(Req) then
        f_X509_REQ_free(Req);
      if Assigned(FileBio) then
        f_BIO_free(FileBio);
    end;
end;

end.
