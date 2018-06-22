{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Automatically download SSL X509 certifcates from various
              issuers, including free certificates from Let's Encrypt, and
              commercial certificates from CertCentre AG and Servertastic.
              Supports and ACME V1 and V2 protocols, and REST protocols
              for specific vendors.  Domain validated certificates should
              generally be issued without internvention, other commercial
              certificates may take days to be approved.
Creation:     Apr 2018
Updated:      June 2018
Version:      8.55
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
Legal issues: Copyright (C) 1997-2018 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
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


Overview
--------

SSL/TLS X509 certificates
-------------------------

There are effectively three classes of SSL/TLS X509 certificates, Domain Validated,
Organisation Validated and Extended Validated, in order of cost and benefit,
usually with three variations, single domain, multiple domains (SANs), and
wildcard.  Adding multiple domains to a certificate can ease administration and
is cheaper than multiple certificates, wild card means any subdmains usually for
the cost of about six single domains.

Domain Validated certificate issuance is mostly automated so they are cheap (or
free), using one of three challenge methods: file validation where the supplier
checks for a specific file under the domain, usually http://domain/.well-known/file,
domain validation where a special DNS record is created that can be accessed by
the supplier, and email validation where an email is sent to a predefined address
at the domain, ie admin@domain, with a supplier link that must be clicked to
confirm receipt and domain ownership.  A fourth method Server Name Indication
(SNI) validation has been used in the past which allows an SSL server to check
a special domain name, but security weakness in some implementations mean this
is not currently used.

File and domain validation challenges can both be automated, file is easiest using
a simple HTTP server, while domain validation is dependent on being able to access
and control the DNS server of which there are many different products.  Note
file validation challenges are not instant, the supplier may have a queue of
challenges waiting to the tested, but usually happen without a couple of minutes.
Applications need to be aware the wait may be longer.

Organisation and Extended Validated certificates can be ordered online, but
require manual validation that the company or organisation legally exists and is
entitled to use the domain name, which may take several days or weeks for extended
validation if legal evidence is required.  Once approved, the certificate and be
downloaded automatically.


TSslX509Certs Overview
----------------------

The TSslX509Certs component automatically downloads SSL/TLS X509 certificates from
various suppliers, including free certificates from Let's Encrypt, and commercial
certificates from CertCentre AG and Servertastic (not done yet).  The component
automates the process from creating a new private key and certificate request,
placing the order, arranging for domain validated certificates to be checked by
various challenge methods, collecting the certificate and intermediate, creating
PEM and PKC12 bundle files with the private key, then copying the files to the
web server ready for automatic installation.

The component supports automated file challenge for Domain Validated certificates,
initially using an external HTTP server to which files may be copied using UNC
shares, but the next release will support a built-in HTTP server as well.  The
application can also use an FTP server to copy files to an external HTTP server.
DNS challenge currently require the application to update the DNS server, likewise
email challenge needs an external email application.

The component supports the Acme V1 and V2 protocols as implemented by Let's
Encrypt to download free domain validated certificates, beware V1 bears little
resemblance to any of the Acme Internet Draft specifications, V2 is much closer
to draft 10 but only implemented sufficiently for Let's Encrypt, V2 is designed
to also handle commercial certificates which are more complicated to process.
Note that Acme V1 has been superseded by V2, and was only supported because V2
did not go public until March 2018.

You don't need to register with Let's Encrypt, but it only supplies domain
validated certificates so the domains for which you order certificates must
already be registered and have DNS pointing to a live HTTP (not HTTPS) server
where the component can copy a challenge file for Let's Encrypt to access to prove
domain ownership.  Let's Encrypt also supports DNS challenges but this is not yet
implemented by the component.  Currently only single domains are supported,
multiple domains and wildcard will be added for V2 shortly.

Commercial suppliers of certificates have their own APIs, usually using HTTP
REST, currently the component supports CertCentre AG https://www.certcenter.de/,
https://www.certcenter.co.uk/ or https://www.certcenter.com/ from where you
can buy certificates issued by Comondo, DigiCert (including GeoTrust, Symantec
and Thawte) and GlobalSign, and free certificates from AlwaysOnSSL (by resellers
only), see https://alwaysonssl.com/issue.php.  You need to register with
CertCentre AG and open an account to pay for any certificates bought, although
for testing most can be cancelled within 30 days without charge.  CertCentre AG
uses OAuth2 authentication which is complex to set-up, but then mostly invisible.
Domain validated certificates can be purchased and downloaded automatically using
file challenge, other types of certificates can be ordered and then downloaded
when the order is completed.


TSslX509Certs Accounts
----------------------

The TSslX509Certs component has a concept of an account directory for a certificate
supplier, into which all certificate and related files will be saved, with extra
information about the account.  The next release will include an account database
file to keep track of domains being ordered and when they need renewing.  For
Let's Encrypt Acme, the directory includes an account private key (separate to
certificate private keys) that is used to identify the account, this is created
automatically if the directory is blank.

When ordering a new certificate, temporary files may be created in the account
directory, the new certificate private key and certificate request.  Once the
order is accepted and an order number is available, these files are renamed to
include the order number for historic purposes, and when the order is completed
a second copy of all files is saved without the order number for final distribution
to the web server, and optionally copied to the web server using a UNC file share.
Note the files without order numbers are always automatically overwritten by new
orders.  So an Let's Encrypt order for the domain test3.telecom-tariffs.co.uk
will find the following files upon completion:

AcmePrivateKey.pem
AcmePublicKey.pem
LE-SA-v1.2-November-15-2017.pdf
LE-5860824-test3_telecom-tariffs_co_uk-bundle.pem
LE-5860824-test3_telecom-tariffs_co_uk-certonly.pem
LE-5860824-test3_telecom-tariffs_co_uk-inters.pem
LE-5860824-test3_telecom-tariffs_co_uk-privatekey.pem
LE-5860824-test3_telecom-tariffs_co_uk-request.pem
LE-5860824-test3_telecom-tariffs_co_uk.pfx
test3_telecom-tariffs_co_uk-bundle.pem
test3_telecom-tariffs_co_uk-certonly.pem
test3_telecom-tariffs_co_uk-inters.pem
test3_telecom-tariffs_co_uk-privatekey.pem
test3_telecom-tariffs_co_uk-request.pem
test3_telecom-tariffs_co_uk.pfx

There is a PEM certificate signing request (CSR) file, separate PEM files for the
private key, domain certificate and intermediate certificates, then a combined PEM
bundle  with the certificates and private key, and a PKCS12 PFX file which is a
similar bundle that Windows certificate store accepts.  The certificate private
key files (and bundles) may be optionally password protected, the PFX file always
has a password since Windows requires that, it will be 'password' if not otherwise
specified. Note AcmePrivateKey is unprotected.  A wildcard order for
*.telecom-tariffs.co.uk will have a file name x_telecom-tariffs_co_uk since * can
not be used in file names.  Until the order number is available, the file name
will be LE-work, or CC-work for CertCentre.


TSslX509Certs Sample Application
--------------------------------

There is a sample application Samples\Delphi\SslInternet\OverbyteIcsX509CertsTst.dpr
that illustrates all the functionality of the TSslX509Certs component, allowing
certificates to be ordered and collected by clicking a few buttons.  Note that
currently there must be a external HTTP (not HTTPS) server where the component
can copy challenge files to prove domain ownership.

In the following sample descriptions, all the fields and buttons mentioned have
corresponding properties and methods in the TSslX509Certs component itself.

On the Common tab, there are various logging options, to keep track of activity
and for diagnostics when things don't work as expected, if the Log Directory is
not blank.  There are several levels of debug logging from just connections,
through SSL negotiations, then HTTP headers and content, also Json logging for
protocol errors (or changes).

For CertCentre AG, you must create an account on their web site first, then go to
Settings, Your Apps & API Keys, under OAuth2 Your Apps, click the blue + icon to
create a new App, with OAuth2 Redirect-URI: http://localhost:8080/certcenter/.
Back in the sample application, copy the various OAuth2 parameters from your new
app to the sample fields, App Auth URl, Client ID, Client Secret and Redirect-URI,
set App Token URL to https://api.certcenter.com/oauth2/token and scope to write,
web server IP to 127.0.0.1 port 8080.  The first time you access a CertCentre
function, OAuth2 authentication will be triggered to display an account login
page in your default browser, then a German language page appears so click the
'Akzeptieren' button which should result in the sample application completing
OAuth2 and displaying access and refresh tokens with an expiry date and time, and
the browser saying 'App Token Generated Successfully'.  The tokens initially
remain valid for 24 hours before another login is required, but may be refreshed
manually or automatically before they expire without needing another login.
Refreshed tokens expire after six hours, but can be extended again and again,
provided the sample is still running.

For certificate orders, on the Common Tab set the challenge method and optional
private key password and encryption type (3DES) if needed, on the Domain tab
set the certificate common domain name (note alternate names not yet supported),
then the 'Web Server UNC HTTP .Well-Known Directory' and optionally
'Web Server UNC Public Certificates Directory' where the final certificates will
be copied. Clicking the 'Test Well-Known' button will copy a file to the web
server and attempt to access it using the domain name, to prove future challenges
will be successful.  The Cert Admin tab has a lot more fields relating to
certificate order needed to create a private key and certificate signing request,
many can be ignored for Let's Encrypt (except email) but commercial certificates
need most of the fields, including certificate period in months, usually 12 or 24,
but this may be overwritten by the order process. Currently only RSA keys and
SHA-2 are tested, not ECC or SHA-3.


TSslX509Certs Order Process
---------------------------

Let's Encrypt offers live and staging servers, the latter is best for testing
since it issue certificates signed by a fake CA.  Select a directory for the
account and certificates and the server.  For Let's Encrypt, click 'Register
Account' which will create a new account private key or open an old one, and
then register the account.  For CertCentre, Get Profile will trigger OAuth2
if necessary, then check your account and list the certificate products that
can be ordered, from which one will be selected, and the details and cost will
be displayed.

Next, click 'Check Order' which will check the challenge method specified on the
Common tab is valid for the certificate product, and repeat the local 'Test
Well-Known' check for file challenges.  If the checks succeed, the Order button
will be enabled.

For Let's Encrypt Acme and AlwaysOnSSL, click the Order button to start the order
process.  There will be another local 'Test Well-Known' check.  Then the supplier
is asked if it can register the domain name.  If the name is acceptable, information
about the actual challenge is returned by the supplier and the component creates
the domain validation file in the web server .well-known directory and requests
the supplier to test it, returning an order number.

The component then waits up to two minutes periodically checking the order status
to see if the challenge has been completed and been successful.  The next release
of the component will remove this wait and instead implement a queue of challenge
checks over days if necessary, allowing multiple domains to be handled.

Once the challenge succeeds, the 'Collect Certificate' button is enabled and
should be clicked. Another check is made the challenge has been completed OK,
then a private key and certificate signing request will be created and submitted.
If the CSR matches the challenged domain, a new domain SSL/TLS X509 certificate
is generated and saved including the order number, then the intermediate
certificate that will be needed by the web server, then the PEM and PKCS12 bundles
are built by adding the private key, as detailed above.  Finally the component
runs a check to validate the certificate chain, and reports all the details.
If validation passes, all the files are saved a second time without the order
number, as detailed above, and finally a third time by copying to the web server
certificate directory.  Some web servers, such as those based on TWSocketServer
will periodically check for new SSL certificates (RecheckSslCerts method) and
will automatically install the new certificate.  Note that Let's Encrypt
certificates are only valid for three months since they are intended to be
renewed automatically.

AlwaysOnSSL is a similar free certificate available to resellers of CertCentre,
and the order process is similar to Acme, except the CSR is supplied before the
challenge starts, and the certificate is returned when the challenge succeeds,
but may be downloaded again later if needed by using the order number. AlwaysOnSSL
certificates are valid up to one year.

For commercial certificates, when checking the order a quotation is returned for
the certificate cost.  The word BUY needs to typed to avoid spending money
too easily, then 'Order Commercial Cert' clicked. The private key and CSR are
generated and the order placed.  A number of errors may occur at this stage,
mostly related to missing fields such as address, telephone, etc.  For domain
validated certificates, challenge validation will then start, being automatic
for file similarly to Acme and AlwaysOnSSL.  For email validation, organisation
and extended validated certificates, an order number is returned and the process
now stalls for manual processing.

The 'List Orders' button will generate a list in the log of recent CertCentre
orders with their order number and status.  For orders that are completed, the
order number can be entered in the field and 'Collect Order' clicked to collect
the certificate, similarly to Acme.  Likewise, specific orders may be cancelled
within 30 days, and certificates revoked if necessary.




Updates:
May 22, 2018  - 8.54 - baseline
June 22, 2018 - 8.55 - don't load LoadCATrust in Create, it loads OpenSSL in IDE 



Pending - more documentation
Pending - database (ini file) of domains, challenges and expiry for automation
Pending - support multiple challenges (needs database)
Pending - collect certificate when challenge passed (needs database)
Pending - local web server (needs database)
Pending - integrate with SocketServer component (for web server, proxy server)
Pending - multiple SAN domains for AcmeV2 and CertCentre
Pending - wild cards and DNS validation for AcmeV2
Pending - Acme EC accounts, signing currently fails validation
Pending - Acme EC certificates, not properly tested yet
Pending - Acme revoke certificate
Pending - CertCentre re-issue certificate, use ModifiedOrders for last x days
Pending - Servertastic APIv2 for commercial certificates
Pending - install PKCS12 certificates into Windows cert store for IIS
Pending - better error reporting and logging
Pending - Private CA support, create and sign certificates


}

unit OverbyteIcsSslX509Certs;

{$I Include\OverbyteIcsDefs.inc}

{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$H+}             { Use long strings                    }
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
//    {$IFDEF RTL_NAMESPACES}System.IniFiles{$ELSE}IniFiles{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Posix.Time,
    Ics.Posix.WinTypes,
    Ics.Posix.Messages,
{$ENDIF}
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
    {$IFDEF RTL_NAMESPACES}System.TypInfo{$ELSE}TypInfo{$ENDIF},
//    {$IFDEF Rtl_Namespaces}System.StrUtils{$ELSE}StrUtils{$ENDIF},
    Overbyteicsssleay, Overbyteicslibeay,
{$IFDEF FMX}
    Ics.Fmx.OverbyteIcsWndControl,
    Ics.Fmx.OverbyteIcsWSocket,
{$ELSE}
    OverbyteIcsWndControl,
    OverbyteIcsWSocket,
{$ENDIF FMX}
    OverbyteIcsTypes,
    OverbyteIcsIniFiles,
    OverbyteIcsUtils,
    OverbyteIcsLogger,     { for TLogOption }
    OverbyteIcsSslX509Utils,
    OverbyteIcsHttpProt,
    OverbyteIcsSslHttpRest,
    OverbyteIcsUrl,
    OverbyteIcsMimeUtils,
//    OverbyteIcsCharsetUtils,
    OverbyteIcsSuperObject,
    OverbyteIcsSslJose;

{ NOTE - these components only build with SSL, there is no non-SSL option }

{$IFDEF USE_SSL}

const
 // file suffixes to build various file names
    FileSuffPKey     = '-privatekey.pem' ;
    FileSuffCSR      = '-request.pem' ;
    FileSuffCertPem  = '-certonly.pem' ;
    FileSuffInterPem = '-inters.pem' ;
    FileSuffBundPem  = '-bundle.pem' ;
    FileSuffBundP12  = '.pfx' ;
    FileSuffBundP7  = '.p7' ;

    DateMaskPacked = 'yyyymmdd"-"hhnnss' ;

    digestlist: array [0..8] of TEvpDigest =
        (Digest_sha1, Digest_sha224, Digest_sha256, Digest_sha384, Digest_sha512,
        Digest_sha3_224, Digest_sha3_256, Digest_sha3_384, Digest_sha3_512);


type
    TSupplierProto = (SuppProtoAcmeV1, SuppProtoAcmeV2, SuppProtoCertCentre,
                       SuppProtoServtas);

    TChallengeType = (ChallFileUNC, ChallFileFtp, ChallFileSrv,
                      ChallDNS, ChallSNI, ChallEmail);

    TIssueState = (IssStateNone, IssStateAccount, IssStateChecked,
                   IssStateChallgPend, IssStateChallgOK, IssStateCollect);

    AcmeActionDir = record
        Action: string ;
        URL: string ;
    end;

(*
{ V1
  "ZwFVz_99UHU": "https://community.letsencrypt.org/t/adding-random-entries-to-the-directory/33417",
  "key-change": "https://acme-staging.api.letsencrypt.org/acme/key-change",
  "meta": {
    "terms-of-service": "https://letsencrypt.org/documents/LE-SA-v1.2-November-15-2017.pdf"
  },
  "new-authz": "https://acme-staging.api.letsencrypt.org/acme/new-authz",
  "new-cert": "https://acme-staging.api.letsencrypt.org/acme/new-cert",
  "new-reg": "https://acme-staging.api.letsencrypt.org/acme/new-reg",
  "revoke-cert": "https://acme-staging.api.letsencrypt.org/acme/revoke-cert"
}
{ V2
  "Qa5SoBHy3FM": "https://community.letsencrypt.org/t/adding-random-entries-to-the-directory/33417",
  "keyChange": "https://acme-staging-v02.api.letsencrypt.org/acme/key-change",
  "meta": {
    "termsOfService": "https://letsencrypt.org/documents/LE-SA-v1.2-November-15-2017.pdf"
  },
  "newAccount": "https://acme-staging-v02.api.letsencrypt.org/acme/new-acct",
  "newNonce": "https://acme-staging-v02.api.letsencrypt.org/acme/new-nonce",
  "newOrder": "https://acme-staging-v02.api.letsencrypt.org/acme/new-order",
  "revokeCert": "https://acme-staging-v02.api.letsencrypt.org/acme/revoke-cert"
}
 *)

const
    AcmeResNewReg1 = 'new-reg';        // V1 aka newaccount
    AcmeResNewAuthz1 = 'new-authz';    // V1 neworder
    AcmeResNewCert1 = 'new-cert';      // V1
    AcmeResRevokeCert1 = 'revoke-cert';// V1
    AcmeResKeyChange1 = 'key-change';  // V1
    AcmeResNewNonce2 = 'newNonce';     // V2 only
    AcmeResNewAccount2 = 'newAccount'; // V2
    AcmeResNewOrder2 = 'newOrder';     // V2 aka new-cert
    AcmeResRevokeCert2 = 'revokecert'; // V2
    AcmeResKeyChange2 = 'keychange';   // V2
//    AcmeResNewAuthz2 = 'newauthz';   // V2 not implemented yet

 // Acme Actions
    AcmeNewReg1 = 1;
    AcmeNewAuthz1 = 2;
    AcmeNewCert1 = 3;
    AcmeRevokeCert1 = 4;
    AcmeKeyChange1 = 5;
    AcmeNewNonce2 = 6;
    AcmeNewAccount2 = 7;
    AcmeNewOrder2 = 8;
    AcmeRevokeCert2 = 9;
    AcmeKeyChange2 = 10;
 //   AcmeNewAuthz2 =  11;

    AcmeActionTot = 10 ;

const
    LetsEncryptCrossInterLines =
        '# Common Name (CN): Let''s Encrypt Authority X3' + #13#10 +
        '# Organisation (O): Let''s Encrypt' + #13#10 +
        '# ISSUED BY' + #13#10 +
        '# Common Name (CN): DST Root CA X3' + #13#10 +
        '# Organisation (O): Digital Signature Trust Co.' + #13#10 +
        '# Serial Number: 0a0141420000015385736a0b85eca708' + #13#10 +
        '# Issued on: 17/03/2016' + #13#10 +
        '# Expires on: 17/03/2021' + #13#10 +
        '# Fingerprint (sha1): e6a3b45b062d509b3382282d196efe97d5956ccb' + #13#10 +
        '# Key Info: RSA Key Encryption 2048 bits, 112 security bits' + #13#10 +
        '-----BEGIN CERTIFICATE-----' + #13#10 +
        'MIIEkjCCA3qgAwIBAgIQCgFBQgAAAVOFc2oLheynCDANBgkqhkiG9w0BAQsFADA/' + #13#10 +
        'MSQwIgYDVQQKExtEaWdpdGFsIFNpZ25hdHVyZSBUcnVzdCBDby4xFzAVBgNVBAMT' + #13#10 +
        'DkRTVCBSb290IENBIFgzMB4XDTE2MDMxNzE2NDA0NloXDTIxMDMxNzE2NDA0Nlow' + #13#10 +
        'SjELMAkGA1UEBhMCVVMxFjAUBgNVBAoTDUxldCdzIEVuY3J5cHQxIzAhBgNVBAMT' + #13#10 +
        'GkxldCdzIEVuY3J5cHQgQXV0aG9yaXR5IFgzMIIBIjANBgkqhkiG9w0BAQEFAAOC' + #13#10 +
        'AQ8AMIIBCgKCAQEAnNMM8FrlLke3cl03g7NoYzDq1zUmGSXhvb418XCSL7e4S0EF' + #13#10 +
        'q6meNQhY7LEqxGiHC6PjdeTm86dicbp5gWAf15Gan/PQeGdxyGkOlZHP/uaZ6WA8' + #13#10 +
        'SMx+yk13EiSdRxta67nsHjcAHJyse6cF6s5K671B5TaYucv9bTyWaN8jKkKQDIZ0' + #13#10 +
        'Z8h/pZq4UmEUEz9l6YKHy9v6Dlb2honzhT+Xhq+w3Brvaw2VFn3EK6BlspkENnWA' + #13#10 +
        'a6xK8xuQSXgvopZPKiAlKQTGdMDQMc2PMTiVFrqoM7hD8bEfwzB/onkxEz0tNvjj' + #13#10 +
        '/PIzark5McWvxI0NHWQWM6r6hCm21AvA2H3DkwIDAQABo4IBfTCCAXkwEgYDVR0T' + #13#10 +
        'AQH/BAgwBgEB/wIBADAOBgNVHQ8BAf8EBAMCAYYwfwYIKwYBBQUHAQEEczBxMDIG' + #13#10 +
        'CCsGAQUFBzABhiZodHRwOi8vaXNyZy50cnVzdGlkLm9jc3AuaWRlbnRydXN0LmNv' + #13#10 +
        'bTA7BggrBgEFBQcwAoYvaHR0cDovL2FwcHMuaWRlbnRydXN0LmNvbS9yb290cy9k' + #13#10 +
        'c3Ryb290Y2F4My5wN2MwHwYDVR0jBBgwFoAUxKexpHsscfrb4UuQdf/EFWCFiRAw' + #13#10 +
        'VAYDVR0gBE0wSzAIBgZngQwBAgEwPwYLKwYBBAGC3xMBAQEwMDAuBggrBgEFBQcC' + #13#10 +
        'ARYiaHR0cDovL2Nwcy5yb290LXgxLmxldHNlbmNyeXB0Lm9yZzA8BgNVHR8ENTAz' + #13#10 +
        'MDGgL6AthitodHRwOi8vY3JsLmlkZW50cnVzdC5jb20vRFNUUk9PVENBWDNDUkwu' + #13#10 +
        'Y3JsMB0GA1UdDgQWBBSoSmpjBH3duubRObemRWXv86jsoTANBgkqhkiG9w0BAQsF' + #13#10 +
        'AAOCAQEA3TPXEfNjWDjdGBX7CVW+dla5cEilaUcne8IkCJLxWh9KEik3JHRRHGJo' + #13#10 +
        'uM2VcGfl96S8TihRzZvoroed6ti6WqEBmtzw3Wodatg+VyOeph4EYpr/1wXKtx8/' + #13#10 +
        'wApIvJSwtmVi4MFU5aMqrSDE6ea73Mj2tcMyo5jMd6jmeWUHK8so/joWUoHOUgwu' + #13#10 +
        'X4Po1QYz+3dszkDqMp4fklxBwXRsW10KXzPMTZ+sOPAveyxindmjkW8lGy+QsRlG' + #13#10 +
        'PfZ+G6Z6h7mjem0Y+iWlkYcV4PIWL1iwBi8saCbGS5jN2p8M+X+Q7UNKEkROb3N6' + #13#10 +
        'KOqkqm57TH2H3eDJAkSnh6/DNFu0Qg==' + #13#10 +
        '-----END CERTIFICATE-----' + #13#10;


var
    AcmeActionDirs: array [1..AcmeActionTot] of AcmeActionDir = (
      ( Action: AcmeResNewReg1; URL: ''),
      ( Action: AcmeResNewAuthz1; URL: ''),
      ( Action: AcmeResNewCert1; URL: ''),
      ( Action: AcmeResRevokeCert1; URL: ''),
      ( Action: AcmeResKeyChange1; URL: ''),
      ( Action: AcmeResNewNonce2; URL: ''),
      ( Action: AcmeResNewAccount2; URL: ''),
      ( Action: AcmeResNewOrder2; URL: ''),
      ( Action: AcmeResRevokeCert2; URL: ''),
      ( Action: AcmeResKeyChange2; URL: '') );

type
{ TSubAltName is one subject alternate domain name }
  TSubAltName = class(TCollectionItem)
  private
    FDomain: String;
    FDirWellKnown: String;
    FDirPubWebCert: String;
  protected
    function GetDisplayName: string; override;
  published
    constructor Create (Collection: TCollection); Override ;
    property Domain: String                read  FDomain
                                           write FDomain;
    property DirWellKnown: String          read  FDirWellKnown
                                           write FDirWellKnown;
    property DirPubWebCert: String         read  FDirPubWebCert
                                           write FDirPubWebCert;
  end;

{ TSubAltNames defines a collection of TSubAltName }
  TSubAltNames = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TSubAltName;
    procedure SetItem(Index: Integer; Value: TSubAltName);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent);
    function IndexOf(const aDomain: string): Integer;
    procedure AddItem(const aDomain, aDirWellKnown, aDirPubWebCert: string);
    property Items[Index: Integer]: TSubAltName     read GetItem
                                                    write SetItem; default;
  end;



 TSslX509Certs = class(TComponent)
  private
    { Private declarations }
// components
    FHttpRest: TSslHttpRest;
    FHttpTest: TSslHttpRest;
    FRestOAuth: TRestOAuth;
    FDomWebServer: TSimpleWebSrv;
    fSslCert: TSslCertTools;
    fAcmePrivKey: TSslCertTools;
    fRootCAX509: TX509Base;
    FRefreshTimer: TIcsTimer;
    FControlFile: TIcsIniFile;

// published properties
    FAcmeAccKeyType: TSslPrivKeyType;
    FCertAddress: String;
    FCertApprovEmail: String;
    FCertSubAltNames: TSubAltNames;
    FCertCommonName: String;
    FCertContactEmail: String;
    FCertContactFirst: String;
    FCertContactLast: String;
    FCertContactPhone: String;
    FCertContactTitle: String;
    FCertCountry: String;
    FCertDescr: String;
    FCertLocality: String;
    FCertOrgUnit: String;
    FCertOrganization: String;
    FCertPostcode: String;
    FCertPhone: String;
    FCertSignDigestType: TEvpDigest;
    FCertState: String;
    FCertValidity: Integer;
    FDebugLevel: THttpDebugLevel;
    FDirCertWork: String;
    FDirPubWebCert: TStringList;  // may be several wildcard servers
    FDirWellKnown: String;
    FDomWebSrvIP: String;
    FLogJson: Boolean;
    FLogPkeys: Boolean;
    FOAAccToken: String;
    FOAAppUrl: String;
    FOAClientId: String;
    FOAClientSecret: String;
    FOAExpireDT: TDateTime;
    FOARedirectUrl: String;
    FOARefrMinsPrior: Integer;
    FOARefreshAuto: Boolean;
    FOARefreshToken: String;
    FOAScope: String;
    FOATokenUrl: String;
    FOAWebSrvIP: String;
    FOAWebSrvPort: String;
    FPrivKeyCipher: TSslPrivKeyCipher;
    FPrivKeyPassword: String;
    FPrivKeyType: TSslPrivKeyType;
    FSuppCertChallenge: TChallengeType;
    FSuppCertFeatures: String;
    FSuppCertProduct: String;
    FSuppOrderId: String;
    FSuppOrderRef: String;
    FSupplierProto: TSupplierProto;
    FSupplierServer: String;
    FOnCertProg: THttpRestProgEvent;
    FOnNewToken: TNotifyEvent;
    FOnNewCert: TNotifyEvent;

// internal vars
    FAcmeHost: String;
    FIssueState: TIssueState;
    FCCLastStatCode: Integer;
    FPartFNameWork: String;
    FPartFNameFinal: String;
    FPartFNameServer: String;
    FPartFNameOrder: String;
    FFileCSR: string;
    FFilePrvKey: string;
    FFileCertPem: string;
    FFileBundPem: string;
    FFileInterPem: string;
    FFileBundP12: String;
    FFileBundP7: String;
    FCSRLines: string;
    FPrvKeyLines: string;
    FNewCertPrefix: string;
    FNewCertLines: string;
    FNewInterLines: string;
    FNewCertP7Lines: string;
    FNewCertCN: string;
    fNewCertSAN: string ;
    FNewCertChainInfo: string;
    FNewCertErrs: string;
    FNewCertEndDate: string;
    FNewCertStartDate: string ;
    FNewCertValRes: TChainResult;
    FAcmePubFName: string;
    FAcmePrivFName: string;
    FLastErrMsg: String;
    FProductJson: ISuperObject;
    FProductDVAuth: String;
    FProductFeatures: String;
    FProductInfo: String;
    FProductList: TStringList;
    FProductQuote: String;
    FApproverEmails: TStringList;
    FAcmeLastStatus: Integer;
    FAcmeRespNonce: string;
    FAcmeRespLink: String;
    FAcmeRespRequester: String;
    FAcmeRespLocation: String;
    FAcmeRespContLoc: string;
    FAcmeKwkPub: String;
    FAcmeKwkKid: String;
    FAcmeJwsAlg: String;
    FAcmeJoseAlg: TJoseAlg;
    FAcmeAccountNum: String;
    FAcmeAccountUrl: String;
    FAcmeTermsUrl: String;
    FAcmeKwkThumb: String;
    FAcmeChallengeUrl: String;
    FAcmeCertLines: String;
    FAcmeCertUrl: String;
    FAcmeCertSerial: String;
    FAcmeOrderFinalizeUrl: String;
    FAcmeOrderStatus: String;
    FAcmeOrderExpiresDT: TDateTime;
    FAcmeOrderObjUrl: String;
  protected
    { Protected declarations }
    procedure RestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
    procedure LogEvent(const Msg: String);
//    procedure SetError(ErrCode: Integer; const Msg: String);
    procedure WebSrvReq(Sender: TObject; const Host, Path, Params: string; var RespCode, Body: string);
    procedure RefreshOnTimer(Sender: TObject);
    procedure SetSubAltNames(Value: TSubAltNames);
    procedure OAuthNewToken(Sender: TObject);

  public
    { Public declarations }
    constructor  Create (Aowner: TComponent); override;
    destructor   Destroy; override;
    function  StartDomSrv: boolean ;
    function  StopDomSrv: boolean ;
    function  DomSrvIsRunning: Boolean;
    procedure HttpRestRestRequestDone(Sender: TObject;
                              RqType: THttpRequest; ErrCode: Word);
    function GetServerAPIUrl(Supplier: TSupplierProto; TestApi: Boolean = False): String;
    procedure SetOAuth2;
    function OAGrantRefresh: boolean;
    function OAGrantAuthToken(const Code: String): boolean;
    function TestWellKnown(const aDomain, aDirWellKnown: String): Boolean;
    function SaveDataFile(const FName, Data: String): Boolean;
    procedure SetFullFileNames(const FileDir: String);
    function CreateKeyandReq: boolean;
    procedure DumpJson(const Item: String = '');
    function SetPartFNames (ReadOnly: Boolean = False): Boolean;
    procedure SaveCertificateFiles(const CertName: string) ;
    function SetCertCentre: boolean;
    function CCGetRequest(HttpReq: THttpRequest;
                const PageURL: String; const RawParams: String = ''): boolean;
    function CCGetProfile: Boolean;
    function CCGetProducts(Log: Boolean = False): boolean;
    function CCGetOneProduct(const Product: String): Boolean;
    function CCGetApproverEmail: Boolean;
    procedure CCFullfillment (JsonOrder: ISuperObject; const CertName: string);
    function CCListAllOrders: Boolean;
    function CCGetOneOrder: Boolean;
    function CCOrderCert: Boolean;
    function CCCheckOrder: Boolean;
    function CCCancelOrder (Revoke: Boolean): Boolean;
    function SetAcmeAccount: boolean;
    function AcmeGetRequest(HttpReq: THttpRequest;
                const FullURL: String; AcmeJson: ISuperObject): boolean;
    function AcmeLoadPKey(New: Boolean): Boolean;
    function AcmeGetActions: Boolean;
    function AcmeCheckOrder: Boolean;
    function AcmeV1NewAccount: Boolean;
    function AcmeV2NewAccount: Boolean;
    function AcmeV1OrderCert: Boolean;
    function AcmeV1GetCert: Boolean;
    function AcmeV2OrderCert: Boolean;
    function AcmeV2GetCert: Boolean;

    property ProductJson: ISuperObject              read FProductJson;
    property ProductDVAuth: String                  read FProductDVAuth;
    property ProductFeatures: String                read FProductFeatures;
    property ProductInfo: String                    read FProductInfo;
    property ProductList: TStringList               read FProductList;
    property ProductQuote: String                   read FProductQuote;
    property ApproverEmails: TStringList            read FApproverEmails;
    property SslCert: TSslCertTools                 read FSslCert;
    property IssueState: TIssueState                read FIssueState;

  published
    { Published declarations }
    property AcmeAccKeyType: TSslPrivKeyType        read  FAcmeAccKeyType
                                                    write FAcmeAccKeyType;
    property CertAddress: String                    read  FCertAddress
                                                    write FCertAddress;
    property CertSubAltNames: TSubAltNames          read  FCertSubAltNames
                                                    write SetSubAltNames;
    property CertApprovEmail: String                read  FCertApprovEmail
                                                    write FCertApprovEmail;
    property CertCommonName: String                 read  FCertCommonName
                                                    write FCertCommonName;
    property CertContactEmail: String               read  FCertContactEmail
                                                    write FCertContactEmail;
    property CertContactFirst: String               read  FCertContactFirst
                                                    write FCertContactFirst;
    property CertContactLast: String                read  FCertContactLast
                                                    write FCertContactLast;
    property CertContactPhone: String               read  FCertContactPhone
                                                    write FCertContactPhone;
    property CertContactTitle: String               read  FCertContactTitle
                                                    write FCertContactTitle;
    property CertCountry: String                    read  FCertCountry
                                                    write FCertCountry;
    property CertDescr: String                      read  FCertDescr
                                                    write FCertDescr;
    property CertLocality: String                   read  FCertLocality
                                                    write FCertLocality;
    property CertOrgUnit: String                    read  FCertOrgUnit
                                                    write FCertOrgUnit;
    property CertOrganization: String               read  FCertOrganization
                                                    write FCertOrganization;
    property CertPhone: String                      read  FCertPhone
                                                    write FCertPhone;
    property CertPostCode: String                   read  FCertPostCode
                                                    write FCertPostCode;
    property CertSignDigestType: TEvpDigest         read  FCertSignDigestType
                                                    write FCertSignDigestType;
    property CertState: String                      read  FCertState
                                                    write FCertState;
    property CertValidity: Integer                  read  FCertValidity
                                                    write FCertValidity;
    property DebugLevel: THttpDebugLevel            read  FDebugLevel
                                                    write FDebugLevel;
    property DirCertWork: String                    read  FDirCertWork
                                                    write FDirCertWork;
    property DirPubWebCert: TStringList             read  FDirPubWebCert
                                                    write FDirPubWebCert;
    property DirWellKnown: String                   read  FDirWellKnown
                                                    write FDirWellKnown;
    property DomWebSrvIP: String                    read  FDomWebSrvIP
                                                    write FDomWebSrvIP;
    property LogJson: Boolean                       read  FLogJson
                                                    write FLogJson;
    property LogPkeys: Boolean                      read  FLogPkeys
                                                    write FLogPkeys;
    property OAAppUrl: string                       read  FOAAppUrl
                                                    write FOAAppUrl;
    property OAClientId: string                     read  FOAClientId
                                                    write FOAClientId;
    property OAAccToken: String                     read  FOAAccToken
                                                    write FOAAccToken;
    property OAExpireDT: TDateTime                  read  FOAExpireDT
                                                    write FOAExpireDT;
    property OAClientSecret: string                 read  FOAClientSecret
                                                    write FOAClientSecret;
    property OARedirectUrl: string                  read  FOARedirectUrl
                                                    write FOARedirectUrl;
    property OARefreshAuto: Boolean                 read  FOARefreshAuto
                                                    write FOARefreshAuto;
    property OARefrMinsPrior: Integer               read  FOARefrMinsPrior
                                                    write FOARefrMinsPrior;
    property OARefreshToken: string                 read  FOARefreshToken
                                                    write FOARefreshToken;
    property OAScope: string                        read  FOAScope
                                                    write FOAScope;
    property OATokenUrl: string                     read  FOATokenUrl
                                                    write FOATokenUrl;
    property OAWebSrvIP: string                     read  FOAWebSrvIP
                                                    write FOAWebSrvIP;
    property OAWebSrvPort: string                   read  FOAWebSrvPort
                                                    write FOAWebSrvPort;
    property PrivKeyCipher: TSslPrivKeyCipher       read  FPrivKeyCipher
                                                    write FPrivKeyCipher;
    property PrivKeyPassword: string                read  FPrivKeyPassword
                                                    write FPrivKeyPassword;
    property PrivKeyType: TSslPrivKeyType           read  FPrivKeyType
                                                    write FPrivKeyType;
    property SuppCertChallenge: TChallengeType      read  FSuppCertChallenge
                                                    write FSuppCertChallenge;
    property SuppCertFeatures: String               read  FSuppCertFeatures
                                                    write FSuppCertFeatures;
    property SuppOrderId: String                    read  FSuppOrderId
                                                    write FSuppOrderId;
    property SuppOrderRef: String                   read  FSuppOrderRef
                                                    write FSuppOrderRef;
    property SuppCertProduct: String                read  FSuppCertProduct
                                                    write FSuppCertProduct;
    property SupplierProto: TSupplierProto          read  FSupplierProto
                                                    write FSupplierProto;
    property SupplierServer: String                 read  FSupplierServer
                                                    write FSupplierServer;
    property OnCertProg: THttpRestProgEvent         read  FOnCertProg
                                                    write FOnCertProg;
    property OnNewCert: TNotifyEvent                read  FOnNewCert
                                                    write FOnNewCert;
    property OnNewToken: TNotifyEvent               read  FOnNewToken
                                                    write FOnNewToken;



   end;

implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSubAltName }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSubAltName.Create(Collection: TCollection);
begin
    inherited;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSubAltName.GetDisplayName: string;
begin
   Result := Inherited GetDisplayName
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSubAltNames }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSubAltNames.Create(Owner: TPersistent);
begin
  FOwner := Owner;
  inherited Create(TSubAltName);
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSubAltNames.GetItem(Index: Integer): TSubAltName;
begin
  Result := TSubAltName(inherited GetItem(Index));
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSubAltNames.SetItem(Index: Integer; Value: TSubAltName);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSubAltNames.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSubAltNames.IndexOf(const aDomain: string): Integer;
var
    I: Integer;
begin
    Result := -1;
    if Count = 0 then Exit;
    for I := 0 to Count - 1 do begin
        if Items[I].FDomain = aDomain then begin
            Result := I;
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSubAltNames.AddItem(const aDomain, aDirWellKnown, aDirPubWebCert: string);
var
    Index: Integer;
begin
    Index := IndexOf(aDomain);
    if Index < 0 then begin
        Index := Count;
        Add;
    end;
    Items[Index].Domain := aDomain;
    Items[Index].DirWellKnown := aDirWellKnown;
    Items[Index].DirWellKnown := aDirWellKnown;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ TSslX509Certs }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslX509Certs.Create (Aowner: TComponent);
begin
    inherited Create(AOwner);
    FDomWebServer := TSimpleWebSrv.Create(self);
    FDomWebServer.OnServerProg := RestProg;
    FDomWebServer.OnSimpWebSrvReq := WebSrvReq;
    FHttpRest := TSslHttpRest.Create(self);   // REST requests
    FHttpRest.OnHttpRestProg := RestProg;
    FHttpTest := TSslHttpRest.Create(self);   // test .well-known requests
    FHttpTest.OnHttpRestProg := RestProg;
    FRestOAuth := TRestOAuth.Create(self);
    FRestOAuth.OnOAuthProg := RestProg;
    FRestOAuth.OnOAuthNewToken := OAuthNewToken;
    FDomWebSrvIP := '0.0.0.0';
    FOAWebSrvIP := '127.0.0.1';
    FOAWebSrvPort := '8080';
    FDebugLevel := DebugConn;
    FAcmeAccKeyType := PrivKeyRsa2048;
    FSupplierProto := SuppProtoAcmeV2;
    FPrivKeyType := PrivKeyRsa2048;
    FCertSignDigestType := Digest_sha256;
    FCertValidity := 12;
    FRestOAuth.ProtoType := OAuthv2;
    FRestOAuth.AuthType := OAuthTypeWeb;
    FOARefrMinsPrior := 120;
    FPrivKeyCipher := PrivKeyEncNone;
    FRefreshTimer := TIcsTimer.Create(FHttpRest);
    FRefreshTimer.OnTimer := RefreshOnTimer;
    FRefreshTimer.Interval := TicksPerMinute;
    FRefreshTimer.Enabled := True;
    fSslCert := TSslCertTools.Create (self) ;
    fAcmePrivKey := TSslCertTools.Create(self);
//    FRootCAX509 := TX509Base.Create (Self);
//    FRootCAX509.LoadCATrustFromString(sslRootCACertsBundle);  // builtin roots
    FDirPubWebCert := TStringList.Create;
    FProductList := TStringList.Create;
    FApproverEmails := TStringList.Create;
    FIssueState := IssStateNone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslX509Certs.Destroy;
begin
    FRefreshTimer.Enabled := False;
    StopDomSrv;
    FreeAndNil(FRefreshTimer);
    FreeAndNil(FHttpTest);
    FreeAndNil(FHttpRest);
    FreeAndNil(FDomWebServer);
    FreeAndNil(FRestOAuth);
    FreeAndNil(fSslCert);
    FreeAndNil(fAcmePrivKey);
    FreeAndNil(FRootCAX509);
    FreeAndNil(FDirPubWebCert);
    FreeAndNil(FProductList);
    FreeAndNil(FApproverEmails);
    FreeAndNil(FControlFile); 
    inherited Destroy;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.SetSubAltNames(Value: TSubAltNames);
begin
    FCertSubAltNames.Assign(Value);
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.RestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
begin
    if Assigned(FOnCertProg) then
        FOnCertProg(Self, LogOption, Msg) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.LogEvent(const Msg : String);
begin
    if FDebugLevel = DebugNone then Exit;
    if Assigned(FOnCertProg) then
        FOnCertProg(Self, loProtSpecInfo, Msg) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.StartDomSrv: boolean ;
begin
    FDomWebServer.DebugLevel := Self.FDebugLevel;
    FDomWebServer.WebSrvIP := FDomWebSrvIP;
    FDomWebServer.WebSrvPort := '80';
    Result := FDomWebServer.StartSrv;
    if Result then
        LogEvent('Local Web Server Started on: ' + IcsFmtIpv6AddrPort(FDomWebSrvIP, '80'))
    else
        LogEvent('Local Web Server Failed to Start');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.StopDomSrv: boolean ;
begin
//    FLastWebTick := TriggerDisabled;
    Result := FDomWebServer.StopSrv;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.DomSrvIsRunning: Boolean;
begin
    Result := FDomWebServer.IsRunning;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.RefreshOnTimer(Sender : TObject);
begin
    FRefreshTimer.Enabled := False;
    try
//
    finally
        FRefreshTimer.Enabled := True;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ event called by simple web server when any page is requested }
procedure TSslX509Certs.WebSrvReq(Sender: TObject; const Host, Path,
                                Params: string; var RespCode, Body: string);
var
    Title, Msg, FullURL, RespData: String;
//    Client: TSimpleClientSocket;

    procedure BuildBody;
    begin
        Body := '<HTML><HEAD><TITLE>' + Title + '</TITLE></HEAD>' + IcsCRLF +
            '<BODY>' + IcsCRLF +
            '<H1>' + Title + '</H1>' + Msg + '<P>' + IcsCRLF +
            '</BODY></HTML>' + IcsCRLF;
        LogEvent('Web Response: ' + RespCode);
    end;

begin
//    Client := Sender as TSimpleClientSocket;

 // ignore favicon requests completely
    if Path = '/favicon.ico' then begin
        RespCode := '404 Not Found';
        Title := RespCode;
        Msg := 'Error: File Not Found';
        BuildBody;
        Exit;
    end;

//    FLastWebTick := IcsGetTickCountX;   // timeout to close server
    LogEvent('Web Server Request, Host: ' + Host + ', Path: ' + Path + ', Params: ' + Params);

    FullURL := 'http://' + Host + Path;
    RespData := '';

  /// !!! check URL for .Well-Known

    if (RespData = '') then begin
        RespCode := '404 Not Found';
        Title := RespCode;
        Msg := 'Error: File Not Found';
        BuildBody;
        Exit;
    end;

 // found a page o return
    RespCode := '200 OK';
    Body := RespData;
  { web page is sent by event handler }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.GetServerAPIUrl(Supplier: TSupplierProto;
                                        TestApi: Boolean = False): String;
begin
    Result := '';
    case Supplier of
        SuppProtoAcmeV1: begin
            if TestApi then
                Result := 'https://acme-staging.api.letsencrypt.org/directory'
            else
                Result := 'https://acme-v01.api.letsencrypt.org/directory';
        end;
        SuppProtoAcmeV2: begin
            if TestApi then
                Result := 'https://acme-staging-v02.api.letsencrypt.org/directory'
            else
                Result := 'https://acme-v02.api.letsencrypt.org/directory';
        end;
        SuppProtoCertCentre: begin
                Result := 'https://api.certcenter.com/rest/v1/';
        end;
        SuppProtoServtas: begin
            if TestApi then
                Result := 'https://test-api2.servertastic.com'
            else
                Result := 'https://api2.servertastic.com';
        end;

    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.SetOAuth2;
begin
    FRestOAuth.DebugLevel := FDebugLevel;
    FRestOAuth.ProtoType := OAuthv2;
    FRestOAuth.AuthType := OAuthTypeWeb;
    FRestOAuth.AppUrl := Trim(FOAAppUrl);
    FRestOAuth.RedirectMsg := 'App: ' + FRestOAuth.AppUrl;
    FRestOAuth.ClientId := Trim(FOAClientId);
    FRestOAuth.ClientSecret := Trim(FOAClientSecret);
    FRestOAuth.ExpireDT := FOAExpireDT;
    FRestOAuth.OAOptions := [];
    FRestOAuth.RefreshAuto := FOARefreshAuto;
    FRestOAuth.RefrMinsPrior := FOARefrMinsPrior;
    FRestOAuth.RefreshToken := FOARefreshToken;    // sets RefreshDT
    FRestOAuth.Scope := Trim(FOAScope);
    FRestOAuth.TokenUrl := Trim(FOATokenUrl);
    FRestOAuth.RedirectUrl := Trim(FOARedirectUrl);
    FRestOAuth.WebSrvIP := Trim(FOAWebSrvIP);
    FRestOAuth.WebSrvPort := Trim(FOAWebSrvPort);
end;


{* * * * * * * * * * * ** * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.OAuthNewToken(Sender: TObject);
begin
    FOAAccToken := (Sender as TRestOAuth).AccToken;
    FOARefreshToken := (Sender as TRestOAuth).RefreshToken;
    FOAExpireDT := (Sender as TRestOAuth).ExpireDT;
    LogEvent('Got New OAuth Token OK');
    if Assigned(FOnNewToken) then FOnNewToken(self);
end;


 {* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.OAGrantRefresh: boolean;
begin
    SetOAuth2;
    Result := FRestOAuth.GrantRefresh;
    if NOT Result then begin
        FOAAccToken := '';
        FOARefreshToken := '';
        FOAExpireDT := 0;
        if Assigned(FOnNewToken) then FOnNewToken(self);
        if NOT FRestOAuth.StartAuthorization then Exit;
        LogEvent('OAuth2 authorization started, login using browser');
        Result := True;
    end;
end;


 {* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.OAGrantAuthToken(const Code: String): boolean;
begin
    SetOAuth2;
    Result := FRestOAuth.GrantAuthToken(Code);
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.SaveDataFile(const FName, Data: String): Boolean;
var
    NewFStream: TFileStream;
    Attempts: integer;
begin
    Result := False ; ;
    for attempts := 1 to 3 do
    begin
        if attempts > 1 then LogEvent('Failed to save fail, retrying');
        try
            if FileExists (FName) then
            begin
                if NOT DeleteFile(FName) then
                begin
                    LogEvent('Failed to delete old file: ' + FName) ;
                    exit ;
                end;
            end;
            if NOT ForceDirectories (ExtractFileDir (FName)) then
            begin
                LogEvent('Failed to create directory: ' + FName);
                continue;
            end;
            try
                NewFStream := TFileStream.Create (FName, fmCreate) ;
                NewFStream.WriteBuffer(AnsiString(Data) [1], Length (Data)) ;
                LogEvent('Saved file OK: ' + FName);
                Result := true ;
                Exit;
            finally
                FreeAndNil(NewFStream) ;
            end;
        except
            on E:Exception do begin
                LogEvent('Failed to save file: ' + FName + ' - ' + E.Message);
            end;
        end;
    end;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.HttpRestRestRequestDone(Sender: TObject;
  RqType: THttpRequest; ErrCode: Word);
begin
//    LogEvent(String(FHttpRest.ResponseRaw));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// make sure the host well-known directory is accessible from the internet and
// that we can copy files into it.  Otherwise domain validation will fail.

function TSslX509Certs.TestWellKnown(const aDomain, aDirWellKnown: String): Boolean;
var
    URL, testfile, fullname, Randomstr, Content: string;
    StatCode: integer;
begin
    result := false;
    LogEvent('Checking the host well-known directory is accessible from the internet ' +
                                                    'and that we can copy files into it.');

  // where the well known directory is located
    Randomstr := 'My ICS Random String at ' + DateTimeToStr (Now) + ' for ' + aDomain;

  // create file
    testfile := 'icstestfile';
    URL := 'http://' + aDomain + '/.well-known/ics-validation/' + testfile;
    if fSuppCertChallenge = ChallFileUNC then begin
        fullname := aDirWellKnown + 'ics-validation\' + testfile;
        if NOT SaveDataFile(fullname, randomstr) then Exit ;
    end
    else if fSuppCertChallenge = ChallFileSrv then begin
      // start web server

    end
    else begin
        LogEvent('FTP well-known not supported yet');
        exit;
    end;

  // try and read it via HTTP
    try
        FHttpTest.RestParams.Clear;
        StatCode := FHttpTest.RestRequest(HttpGET, URL, False, '');
    except
        on E:Exception do begin
            LogEvent('Could not read file at: ' + URL + ' - ' + E.Message);
            Exit;
        end;
    end;
    if StatCode <> 200 then begin
        LogEvent('Could not read file at: ' + URL + ' - ' + FHttpTest.ReasonPhrase);
        Exit;
    end;
    Content := String(FHttpTest.ResponseOctet); // ignore content coding
    if Content = Randomstr then begin
         LogEvent('Successfully created and accessed Well-Known temporary file at: ' + URL);
         Result := true;
    end
    else
         LogEvent('Failed to compare temporary file content - ' + Content);

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.CCGetRequest(HttpReq: THttpRequest;
                const PageURL: String; const RawParams: String = ''): boolean;
var
    FullURL: String;
begin
    Result := False;
    FullURL := FSupplierServer + PageURL;
    if (FOAExpireDT <= Now) or (FOAAccToken = '') then begin
        LogEvent('Failed, Authorization for CertCentre has expired');
        Exit;
    end;
    FHttpRest.AuthBearerToken := FOAAccToken;
    FHttpRest.ServerAuth := httpAuthBearer;
    FHttpRest.DebugLevel := FDebugLevel;
    FHttpRest.Agent := 'ICS-CertCentre-V8.54';

    try
        FCCLastStatCode := FHttpRest.RestRequest(HttpReq, FullURL, false, RawParams);
    except
        on E:Exception do begin
            LogEvent('Failed to contact CertCentre Server: ' + E.Message);
            // don't exit, may still have something useful
        end;
    end;
    Result := (FHttpRest.ResponseRaw <> '');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.SetCertCentre: boolean;
begin
    Result := False;
    FSupplierProto := SuppProtoCertCentre;
    FIssueState := IssStateNone;
    if Pos ('https://', FSupplierServer) <> 1 then begin
        LogEvent('Invalid certificate supplier server: ' + FSupplierServer);
    end;
    FDirWellKnown := IncludeTrailingPathDelimiter(Trim(FDirWellKnown));
    FDirCertWork := IncludeTrailingPathDelimiter(Trim(FDirCertWork));
//    FCertCommonName := Trim(FCertCommonName);
    FNewCertPrefix := 'CC-' ;

  // clear expired OAuth2 tokens
    SetOAuth2;
    if (FOAExpireDT <= Now) then begin
        FOAAccToken := '';
        FOARefreshToken := '';
        FOAExpireDT := 0;
    end;

  // report next refresh
 //    if (FRestOAuth.RefreshDT > Now) and FRestOAuth.RefreshAuto then
 //            LogEvent('Authorization token will Automatically Refresh at: ' +
 //                                               DateTimeToStr(FRestOAuth.RefreshDT));

  // no tokens, interactive session to login using browser
    if (FOAAccToken = '') then begin
        if NOT FRestOAuth.StartAuthorization then Exit;
        LogEvent('OAuth2 authorization started, login using browser');
        Exit;
    end;
    Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.SetPartFNames(ReadOnly: Boolean = False): Boolean;
var
    CN: string;
begin
    FPartFNameWork := '';
    FPartFNameFinal := '';
    FPartFNameOrder := '' ;
    FPartFNameServer := '' ;
    Result := False ;

  // check for domain
    if Pos ('.', fCertCommonName) = 0 then begin
        LogEvent ('Must specify host domain name');
        exit;
    end;

  // work dir here we will save our certificates and keys
    if NOT ReadOnly then begin
        if NOT ForceDirectories (fDirCertWork) then
        begin
            LogEvent ('Failed to create directory: ' + fDirCertWork);
            exit;
        end;

       // web server certificate location
       // pending, more than one server
        if (FDirPubWebCert.Count > 0) then begin
            if NOT ForceDirectories (DirPubWebCert[0]) then
            begin
                LogEvent ('Failed to create directory: ' + DirPubWebCert[0]);
                exit;
            end;
        end;
    end;

    CN := StringReplace (fCertCommonName, '.', '_', [rfReplaceAll]) ;
    if CN [1] = '*' then CN [1] := 'x';  // can not have * in file names
    fPartFNameWork := fDirCertWork + FNewCertPrefix + 'work-' + CN ;
    if fSuppOrderId <> '' then begin
        fPartFNameOrder := fDirCertWork + FNewCertPrefix + fSuppOrderId + '-' + CN ;
        fPartFNameWork := fPartFNameOrder;
    end;
    fPartFNameFinal := fDirCertWork + CN ;
       // pending, more than one server
        if (FDirPubWebCert.Count > 0) then begin
        fPartFNameServer := DirPubWebCert[0] + CN ;
    end;
    Result := True ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.SetFullFileNames (const FileDir: String) ;
begin
    fFileCSR := FileDir + FileSuffCSR ;
    fFilePrvKey := FileDir + FileSuffPKey ;
    fFileCertPem := FileDir + FileSuffCertPem ;
    fFileInterPem := FileDir + FileSuffInterPem ;
    fFileBundPem := FileDir + FileSuffBundPem ;
    fFileBundP12 := FileDir + FileSuffBundP12 ;
    fFileBundP7 := FileDir + FileSuffBundP7 ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.DumpJson(const Item: String = '');
var
    JsonItem: TSuperAvlEntry;
    JsonEnum: TSuperAvlIterator;
    JsonObj, JsonRec: ISuperObject;
    I: Integer;

    procedure GetEntries;
    begin
        try
         // enumerate Json stuff
            while JsonEnum.MoveNext do begin
                JsonItem := JsonEnum.GetIter;
                LogEvent(JsonItem.Name + '=' + JsonItem.Value.AsString);
            end;
        finally
            JsonEnum.Free;
        end;
    end;

begin
    if NOT FLogJson then Exit ;
    if NOT Assigned (FHttpRest.ResponseJson) then Exit;
    try
        if Item <> '' then begin
            JsonObj := FHttpRest.ResponseJson[Item];
            if NOT Assigned(JsonObj) then Exit;
            if JsonObj.DataType = stArray then begin
                for I := 0 to JsonObj.AsArray.Length - 1 do begin
                    JsonRec := JsonObj.AsArray[I];
                    if JsonRec.DataType = stObject then begin
                        JsonEnum := JsonRec.AsObject.GetEnumerator;
                        GetEntries;
                    end
                    else
                        LogEvent(JsonRec.AsString);
                end;
                exit;
            end;
            JsonEnum := JsonObj.AsObject.GetEnumerator;
            GetEntries;
        end
        else begin
            JsonEnum := FHttpRest.ResponseJson.AsObject.GetEnumerator;
            GetEntries;
        end;
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// CertCentre account information
function TSslX509Certs.CCGetProfile: boolean;
begin
// CertCentre account Profile
    Result := False;
    LogEvent (IcsCRLF + 'Downloading CertCentre Server Profile');
    FHttpRest.RestParams.Clear;
    if NOT CCGetRequest(HttpGET, 'Profile') then exit ;
    DumpJson;
    if FHttpRest.ResponseJson.S['success'] = 'false' then begin
        LogEvent ('Failed to get Profile: ' +  FHttpRest.ResponseJson.S['Message']);
        exit;
    end;

{ expect something like:
Currency=true
Country=
AuthorizationID=999993062
Timezone=Europe/Berlin
AuthType=OAUTH2
Scope=UI
OAuth2_Token=Gxxxxxxxxxxxxxxxx6DVFJJMY.oauth2.certcenter.com
CustomerID=103611
Locale=en_GB

or

success=false
Message=Authorization Failed
ErrorField=Authorization
ErrorId=-2004   }

    LogEvent (IcsCRLF + 'Downloading CertCentre Server Limit');
    FHttpRest.RestParams.Clear;
    if NOT CCGetRequest(HttpGET, 'Limit') then exit ;
    DumpJson;
    Result := CCGetProducts(False);
    if Result then FIssueState := IssStateAccount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.CCGetProducts(Log: Boolean = False): boolean;
var
    JsonItems: ISuperObject;
    I: integer;
begin
    Result := False;
    LogEvent (IcsCRLF + 'Downloading CertCentre Server Products');
    FHttpRest.RestParams.Clear;
    if NOT CCGetRequest (HttpGET, 'Products') then exit ;
    DumpJson;
    if FHttpRest.ResponseJson.S['success'] = 'false' then begin
        LogEvent ('Failed to get Products: ' +  FHttpRest.ResponseJson.S['Message']) ;
        exit ;
    end;
    JsonItems := FHttpRest.ResponseJson['Products'];
    FProductList.Clear ;
    if Assigned(JsonItems) then begin
        if JsonItems.AsArray.Length > 0 then begin
            for I := 0 to JsonItems.AsArray.Length - 1 do begin
                FProductList.Add(JsonItems.AsArray.S[I]) ;
                if Log then LogEvent (JsonItems.AsArray.S[I]) ;
            end;
            FProductList.Sort;
            LogEvent ('Found ' + IntToStr(FProductList.Count) + ' Certiticate Products');
            Result := True;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.CCGetOneProduct(const Product: String): boolean;
var
    tot, mins: Integer;
    OrderCertType: String;
begin
    Result := False;
    LogEvent (IcsCRLF + 'Downloading CertCentre Server ProductCode=' + Product);
    fProductJson := Nil;
    FProductInfo := '';
    FHttpRest.RestParams.Clear;
    FHttpRest.RestParams.PContent := PContUrlencoded;
    FHttpRest.RestParams.AddItem('ProductCode', Product, False);
    if NOT CCGetRequest (HttpGET, 'ProductDetails') then exit;
    DumpJson('ProductDetails');
     {   success=true
        Price=21.6
        MaxValidityPeriod=24
        SANHostPrice=0r
        Features=["DV"]
        RenewPeriod=90
        SANPackagePrice=0r
        Currency=GBP
        ProductType=SSL
        ProductName=RapidSSL
        ProductCode=GeoTrust.RapidSSL
        Licenses=0
        RefundPeriod=30
        CA=GeoTrust
        SANPackageSize=0
        SANMaxHosts=0
        DVAuthMethods=["FILE","DNS","EMAIL"]
        SANFeatures=["HostOnlyIncluded"]   }

    fProductJson := FHttpRest.ResponseJson['ProductDetails'];
    if NOT Assigned(fProductJson) then Exit; 
    fProductDVAuth := fProductJson.S['DVAuthMethods'];
    if (Pos('[', fProductDVAuth) = 1) then
        fProductDVAuth := Copy (fProductDVAuth, 2, Length(fProductDVAuth) - 2);
    fProductFeatures :=  fProductJson.S['Features'];
    if (Pos('[', fProductFeatures) = 1) then
        fProductFeatures := Copy (fProductFeatures, 2, Length(fProductFeatures) - 2);
    FProductInfo := fProductJson.S['ProductName']  + IcsCRLF +
        'Cost ' + fProductJson.S['Price'] + ' ' + fProductJson.S['Currency'] + '/year' + IcsCRLF +
         'Max Validity: ' + fProductJson.S['MaxValidityPeriod'] + ' months, Features: ' +
         fProductFeatures + IcsCRLF  + 'CA: ' + fProductJson.S['CA'] +
         ', Refund Period: ' + fProductJson.S['RefundPeriod'] + ' days' + IcsCRLF;
    if fProductDVAuth <> '' then begin
        FProductInfo := FProductInfo + 'DV Methods: ' + fProductDVAuth + IcsCRLF;
        OrderCertType := 'DV';
    end
    else begin
        if (Pos('EV', fProductFeatures) > 0) then
            OrderCertType := 'EV'
        else if (Pos('OV', fProductFeatures) > 0) then
            OrderCertType := 'OV'
       else
            OrderCertType := '??';
    end;
    tot := atoi (fProductJson.S['SANMaxHosts']);
    if (tot > 0) then
        FProductInfo := FProductInfo + 'SANMaxHosts: ' + IntToStr(tot) + ' at ' +
          fProductJson.S['SANHostPrice'] + ' ' + fProductJson.S['Currency'] + '/year each' + IcsCRLF;

  // estimate issuance time
    FHttpRest.RestParams.Clear;
    FHttpRest.RestParams.PContent := PContUrlencoded;
    FHttpRest.RestParams.AddItem('CA', fProductJson.S['CA'], False);
    FHttpRest.RestParams.AddItem('Type', OrderCertType, False);
    FHttpRest.RestParams.AddItem('Days', '30', False);  // days of old data
    if CCGetRequest (HttpGET, 'IssuanceTimes') then begin
        DumpJson;
        if FHttpRest.ResponseJson.B['success'] then begin
            mins := FHttpRest.ResponseJson.I['prediction'] + 1;  // round up
            FProductInfo := FProductInfo + 'Predicted Approval Duration: ';
            if mins <= 90 then
                FProductInfo := FProductInfo + IntToStr(mins) + ' mins'
            else
                FProductInfo := FProductInfo + IntToStr((mins div 60) + 1) + ' hours';
        end;
    end ;
    Result := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.CCGetApproverEmail: boolean;
var
    JsonItems: ISuperObject;
    I: integer;
    S: string ;
begin
    Result := False;
    if (FCertCommonName = '') or (FSuppCertProduct = '') then begin
        LogEvent ('Email Approver List needs certificate Common Name and Product');
        Exit;
    end;
    FApproverEmails.Clear;
    LogEvent (IcsCRLF + 'Downloading CertCentre Approver List');
    FHttpRest.RestParams.Clear;
    FHttpRest.RestParams.PContent := PContUrlencoded;
    FHttpRest.RestParams.AddItem('ProductCode', FSuppCertProduct, False);
    FHttpRest.RestParams.AddItem('CommonName', FCertCommonName, False);
    if NOT CCGetRequest (HttpGET, 'ApproverList') then exit ;
    DumpJson;

    if FHttpRest.ResponseJson.S['success'] = 'false' then
        LogEvent ('Failed to get Approver List: ' +  FHttpRest.ResponseJson.S['Message'])
    else
    begin
        JsonItems := FHttpRest.ResponseJson['ApproverList'];
        if Assigned (JsonItems) then begin
            if JsonItems.AsArray.Length > 0 then
            begin
                for I := 0 to JsonItems.AsArray.Length - 1 do begin
                    if JsonItems.AsArray.O[I].S['ApproverType'] <> 'Manual' then begin
                        S := JsonItems.AsArray.O[I].S['ApproverEmail'] ;
                        FApproverEmails.Add (S) ;
                        LogEvent (S) ;
                    end;
                end;
                Result := True;
            end;
        end;
    end;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// create new private key and certificate request
function TSslX509Certs.CreateKeyandReq: boolean;
begin
    Result := False ;
    LogEvent ('Generating private and public key pair, please wait');
    try
        fSslCert.PrivKeyType := FPrivKeyType;
        fSslCert.PrivateKey := Nil;
        fSslCert.DoKeyPair;
        LogEvent ('Generated private and public key pair OK:' + IcsCRLF + fSslCert.PrivateKeyInfo);
        fSslCert.PrivateKeySaveToPemFile (fFilePrvKey, FPrivKeyPassword, FPrivKeyCipher) ;
        fPrvKeyLines := fSslCert.SavePKeyToText (FPrivKeyPassword, FPrivKeyCipher);
        LogEvent ('Saved private key file: ' + fFilePrvKey) ;
        if FLogPkeys then LogEvent (IcsCRLF + fPrvKeyLines + IcsCRLF) ;
    except
        on E:Exception do  begin
            LogEvent ('Failed to generate private key - ' + E.Message);
            exit ;
        end;
    end;

    LogEvent ('Generating certificate request');
    try
        fSslCert.X509Req := Nil;
        with fSslCert do begin
            CommonName := fCertCommonName;
            AltDNSList.Clear;
            AltDNSList.Add(fCertCommonName);
         // pending, add SANs
            CertDigest := fCertSignDigestType ;
            Country := FCertCountry;
            State := FCertState;
            Locality := FCertLocality;
            Organization := FCertOrganization;
            OrgUnit := FCertOrgUnit;
            Descr := FCertDescr;
            Email := FCertContactEmail;
        end;
        fSslCert.DoCertReqProps;
        LogEvent('Created certificate request OK:' + IcsCRLF + fSslCert.ReqCertInfo);
        fSslCert.SaveReqToFile(fFileCSR, true);
        fCSRLines := fSslCert.SaveReqToText (false) ;  // no comments, confused order
        LogEvent ('Saved certificate request file: ' + fFileCSR + IcsCRLF + IcsCRLF +
                                                            fCSRLines + IcsCRLF) ;
    except
        on E:Exception do begin
            LogEvent ('Failed to generating CSR - ' + E.Message);
            exit ;
        end;
    end;
    Result := true;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// CertCentre check order for a SSL certificate
function TSslX509Certs.CCCheckOrder: Boolean;
var
    success, isqualified: Boolean;
    S, FullName: string;
begin
    Result := false;
    try // except
        LogEvent (IcsCRLF + 'Checking CertCentre ' + FSuppCertProduct + ' certificate order for: ' + fCertCommonName);

        if Pos ('DV', FProductFeatures) > 0 then begin
            if (fSuppCertChallenge <= ChallFileFtp) and (Pos ('FILE', FProductDVAuth) = 0) then begin
                LogEvent ('FILE validation not available for this certificate');
                Exit;
            end;
            if (fSuppCertChallenge = ChallDNS) and (Pos ('DNS', FProductDVAuth) = 0) then begin
                LogEvent ('DNS validation not available for this certificate');
                Exit;
            end;
            if (fSuppCertChallenge = ChallEmail) and (Pos ('EMAIL', FProductDVAuth) = 0) then begin
                LogEvent ('EMAIL validation not available for this certificate');
                Exit;
            end;

          // where the well known directory is located
            if (fSuppCertChallenge <= ChallFileSrv) then begin
                if NOT TestWellKnown(FCertCommonName, FDirWellKnown) then exit;
            end;
        end;

        if Pos ('WC', FProductFeatures) > 0 then begin
            if Pos ('*.', fCertCommonName) <> 1 then begin
                LogEvent ('Wild card certificate should start with *');
                Exit;
            end;
        end;

     // free AlwaysOnSSL does not have price
        if Pos ('AlwaysOnSSL', FSuppCertProduct) > 0 then begin
          //  fCertValidity := 180;  // days
            LogEvent (IcsCRLF + 'Validate Name domain name at CertCentre for: ' + fCertCommonName);
            FHttpRest.RestParams.Clear;
            FHttpRest.RestParams.PContent := PContJson;
            FHttpRest.RestParams.AddItem('CommonName', FCertCommonName, False);
            if NOT CCGetRequest (HttpPOST, 'ValidateName') then exit ;
            DumpJson;
            success := FHttpRest.ResponseJson.B['success'];
            if NOT success then begin
                LogEvent ('Failed to validate domain: ' +  FHttpRest.ResponseJson.S['Message']);
                exit;
            end;
            isqualified := FHttpRest.ResponseJson.B['IsQualified'];
            if NOT isqualified then begin
                LogEvent ('Domain: ' + fCertCommonName + ', is not qualified for AlwaysOnSSL');
                exit;
            end;
        end
    // commercial cerrts, find price
        else begin

            LogEvent (IcsCRLF + 'Getting quote at CertCentre for: ' + FSuppCertProduct );
            FHttpRest.RestParams.Clear;
            FHttpRest.RestParams.PContent := PContUrlencoded;
            FHttpRest.RestParams.AddItem('ProductCode', FSuppCertProduct, False);
            FHttpRest.RestParams.AddItem('ValidityPeriod', IntToStr (fCertValidity), False);
            FHttpRest.RestParams.AddItem('ServerCount', '0', False);
            FHttpRest.RestParams.AddItem('SubjectAltName', '0', False);
             if NOT CCGetRequest (HttpGET, 'Quote') then exit ;
            DumpJson;
            if FHttpRest.ResponseJson.B['success'] then begin
                FProductQuote := FHttpRest.ResponseJson.S['Price'] + ' ' + FHttpRest.ResponseJson.S['Currency'] ;
                LogEvent ('Price: ' + FProductQuote) ;
                // ask client if happy!!!
            end
            else
                LogEvent ('Failed to get quote: ' + FHttpRest.ResponseJson.AsString);
        end ;

    // get and save user agreement
        LogEvent (IcsCRLF + 'Getting User Agreement at CertCentre for: ' + FSuppCertProduct );
        FHttpRest.RestParams.Clear;
        FHttpRest.RestParams.PContent := PContUrlencoded;
        FHttpRest.RestParams.AddItem('ProductCode', FSuppCertProduct, False);
        if NOT CCGetRequest (HttpGET, 'UserAgreement') then exit ;
        DumpJson;
        if FHttpRest.ResponseJson.B['success'] then  begin
            S := FHttpRest.ResponseJson.S['UserAgreement'] + IcsCRLF;
            FullName := fDirCertWork + FSuppCertProduct + '.txt';
            LogEvent ('Saving user agreement as: ' + FullName);
            SaveDataFile (FullName, S);
            // ask client if happy!!!
        end
        else
            LogEvent ('Failed to get User Agreement: ' + FHttpRest.ResponseJson.AsString);
    except
        on E:Exception do begin
            LogEvent ('Failed to check order - ' + E.Message);
            exit ;
        end;
    end;
    FIssueState := IssStateChecked;
    Result := true ;
 end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// CertCentre order a SSL certificate for an existing
// live HTTP web site whose Well-Known local directory we can access
// from this PC via a UNC path

function TSslX509Certs.CCOrderCert: Boolean;
var
    success, isqualified,  alwaysoncert: Boolean ;
    SigHash, AuthMethod, Partnerorderid, OrderDT: string ;
    JsonItems, JsonOrderParam, JsonContact, JsonOrg, JsonAddr: ISuperObject;
    PointerType, DnsValue, DnsEntry, HashMd5, HashSha256, UniqueValue: string;

    function SaveWellKnown (FileAuth: ISuperObject): boolean ;
    var
        DataFName, DataFPath, DataHash, FullName: string;
    begin
        Result := false;
        try
            DataFName := FileAuth.S['FileName'];   // expect fileauth.txt
            DataFPath := FileAuth.S['FilePath'];   // expect /.well-known/pki-validation
            DataHash := FileAuth.S['FileContents'];
            LogEvent ('FilePath returned: ' + DataFPath);
            DataFPath := StringReplace (DataFPath, '\/', '/', [rfReplaceAll]); //  unescape /
            LogEvent ('Domain validation URL: http://' + fCertCommonName + DataFPath + '/' + DataFName);
            if Pos ('/.well-known/', DataFPath) = 1 then DataFPath := Copy (DataFPath, 14, 99) ;
        except
            on E:Exception do begin
                LogEvent ('Failed to parse json FileAuthDetails');
                exit ;
            end;
        end;
        if (fSuppCertChallenge = ChallFileUNC) then begin
            FullName := FDirWellKnown + DataFPath + '\' + DataFName ;
            LogEvent ('Built domain validation file name: ' + FullName + ', saving hash: ' + DataHash);
            if NOT SaveDataFile (FullName, DataHash) then Exit ;
            Result := true;
        end
        else begin
            LogEvent ('FTP and local server validation not yet available') ;
        end;
    end;

begin
    Result := False;

    if (fCertCommonName = '') then begin
        LogEvent ('Must specify Domain Common Name to place order');
        Exit;
    end;
    if (FSuppCertProduct = '') then begin
        LogEvent ('Must specify certifcate product to check order');
        Exit;
    end;

    try // except
        LogEvent (IcsCRLF + 'Starting CertCentre ' + FSuppCertProduct + ' certificate order for: ' + fCertCommonName);
        alwaysoncert := false;
        if Pos ('AlwaysOnSSL', FSuppCertProduct) > 0 then alwaysoncert := true;
        fSuppOrderId := '' ;
        HashMd5 := '';   // used for Comondo file validation
        HashSha256 := '';
        UniqueValue := '';
        if NOT SetPartFNames (false) then Exit ;  // will only set short path names, no orderid yet
    //    Validity := 12 ;  // months
        case fSuppCertChallenge of
           ChallFileUNC: AuthMethod := 'FILE';
           ChallFileFtp: AuthMethod := 'FILE';
           ChallFileSrv: AuthMethod := 'FILE';
           ChallDNS:     AuthMethod := 'DNS';
           ChallEmail:   AuthMethod := 'EMAIL';
        end;

      // work file names
        SetFullFileNames (FPartFNameWork);

     // order info
        if FSuppOrderRef = '' then
            FSuppOrderRef := 'ICS-' + FormatDateTime (DateMaskPacked, Now);

      // where the well known directory is located
         if fSuppCertChallenge <= ChallFileSrv then begin
            if NOT TestWellKnown(FCertCommonName, FDirWellKnown) then exit;
         end;

     // step one
        if alwaysoncert then begin
            fCertValidity := 365;  // days not months
            LogEvent (IcsCRLF + 'Validate Name domain name at CertCentre for: ' + fCertCommonName);
            FHttpRest.RestParams.Clear;
            FHttpRest.RestParams.PContent := PContJson;
            FHttpRest.RestParams.AddItem('CommonName', FCertCommonName, False);
            if NOT CCGetRequest (HttpPOST, 'ValidateName') then exit ;
            DumpJson;
            success := FHttpRest.ResponseJson.B['success'];
            if NOT success then begin
                LogEvent ('Failed to validate domain: ' +  FHttpRest.ResponseJson.S['Message']);
                exit;
            end;
            isqualified := FHttpRest.ResponseJson.B['IsQualified'];
            if NOT isqualified then begin
                LogEvent ('Domain: ' + fCertCommonName + ', is not qualified for AlwaysOnSSL');
                exit;
            end;
        end;

     // step two - create private key and CSR
        if NOT CreateKeyandReq then exit ;
        if fSslCert.PrivKeyType >= PrivKeyECsecp256 then
            SigHash := 'SHA256-ECC-HYBRID'
     //   SigHash := 'SHA256-ECC-FULL'   //   Symantec only
        else
            SigHash := 'SHA256-FULL-CHAIN' ;   // RSA only

     // step three - optional
        LogEvent (IcsCRLF + 'Validate certificate request at CertCentre');
        FHttpRest.RestParams.Clear;
        FHttpRest.RestParams.PContent := PContJson;
        FHttpRest.RestParams.AddItem('CSR', fCSRLines, True);
        if NOT CCGetRequest (HttpPOST, 'ValidateCSR') then exit ;
        DumpJson;
        success := FHttpRest.ResponseJson.B['success'];
        if NOT success then begin
            LogEvent ('Failed to validate CSR: ' +  FHttpRest.ResponseJson.S['Message']);
            exit;
        end;
        LogEvent ('Validated CSR OK: Common Name: ' +  FHttpRest.ResponseJson.S['ParsedCSR.CommonName'] +
           ', Key: ' +  FHttpRest.ResponseJson.S['ParsedCSR.KeyEncryptionAlgorithm'] +
           ', Length: ' +  FHttpRest.ResponseJson.S['ParsedCSR.KeyLength'] +
           ', Signature: ' +  FHttpRest.ResponseJson.S['ParsedCSR.SignaturAlgorithm'] );

    // used for Comondo file validation - not sure we care since order supplies the same info 
        HashMd5 := FHttpRest.ResponseJson.S['ParsedCSR.HashMD5'];
        HashSha256 := FHttpRest.ResponseJson.S['ParsedCSR.HashSHA256'];
        UniqueValue := FHttpRest.ResponseJson.S['ParsedCSR.UniqueValue'];

     // step four - AlwaysOn validated against well-known file before order
        if alwaysoncert then
        begin
            if AuthMethod = 'DNS' then begin
                LogEvent (IcsCRLF + 'Retrieve appropriate data for DNS-based validation at CertCentre');
                FHttpRest.RestParams.Clear;
                FHttpRest.RestParams.PContent := PContJson;
                FHttpRest.RestParams.AddItem('CSR', fCSRLines, True);
                FHttpRest.RestParams.AddItem('ProductCode', FSuppCertProduct, False);
                if NOT CCGetRequest (HttpPOST, 'DNSData') then exit ;
                DumpJson;
                success := FHttpRest.ResponseJson.B['success'];
                if NOT success then begin
                    LogEvent ('Failed to submit CSR and get DNSData: ' +  FHttpRest.ResponseJson.S['Message']);
                    exit;
                end;
                JsonItems := FHttpRest.ResponseJson['DNSAuthDetails'];
                if NOT Assigned (JsonItems) then begin
                    LogEvent ('Failed to get Json DNSData');
                    exit ;
                end;
                DnsValue := JsonItems.S['DNSValue'];
                DnsEntry := JsonItems.S['DNSEntry'];
                PointerType := JsonItems.S['PointerType'];
                LogEvent (JsonItems.AsString);   // TEMP
                LogEvent ('DNS validation not implemented yet') ;
                exit ;
            end
            else
            begin
                LogEvent (IcsCRLF + 'Retrieve appropriate data for FILE-based validation at CertCentre');
                FHttpRest.RestParams.Clear;
                FHttpRest.RestParams.PContent := PContJson;
                FHttpRest.RestParams.AddItem('CSR', fCSRLines, True);
                FHttpRest.RestParams.AddItem('ProductCode', FSuppCertProduct, False);
                if NOT CCGetRequest (HttpPOST, 'FileData') then exit ;
                DumpJson;
                success := FHttpRest.ResponseJson.B['success'];
                if NOT success then begin
                    LogEvent ('Failed to submit CSR and get FileData: ' + FHttpRest.ResponseJson.S['Message']);
                    exit;
                end;
                JsonItems := FHttpRest.ResponseJson['FileAuthDetails'];
                if NOT Assigned (JsonItems) then begin
                    LogEvent ('Failed to get Json FileData');
                    exit ;
                end;
                if NOT SaveWellKnown (JsonItems) then exit;
            end  ;
        end;

     // step five  real order
        LogEvent (IcsCRLF + 'Ordering ' + FSuppCertProduct + ' certifcate from CertCentre');
        FHttpRest.RestParams.Clear;
        FHttpRest.RestParams.PContent := PContJson;
        JsonOrderParam := SO(['CSR', fCSRLines, 'ProductCode', FSuppCertProduct,
                           'ValidityPeriod', fCertValidity,
                          'SignatureHashAlgorithm', SigHash, 'PartnerOrderID', FSuppOrderRef,
                          'ApproverEmail', fCertApprovEmail ] );
        JsonOrderParam.S['DVAuthMethod'] := AuthMethod;
        JsonAddr := SO(['AddressLine1', FCertAddress, 'PostalCode', FCertPostcode,
                        'City', FCertLocality, 'Region', FCertState,
                        'Country', FCertCountry ]) ;
        JsonOrg := SO(['OrganizationName', FCertOrganization,
                            'OrganizationAddress', JsonAddr,
                             'Phone', FCertPhone, 'Email', FCertContactEmail ]);
        JsonContact := SO(['Title', FCertContactTitle, 'FirstName', FCertContactFirst,
                             'LastName', FCertContactLast, 'OrganizationAddress', JsonAddr,
                             'OrganizationName', FCertOrganization,
                             'Phone', FCertPhone, 'Email', FCertContactEmail ]);
        JsonItems := SO(['OrderParameters', JsonOrderParam, 'OrganizationInfo', JsonOrg,
                             'AdminContact', JsonContact, 'TechContact', JsonContact,
                             'ApproverEmail', fCertApprovEmail ]) ;
  {      FHttpRest.RestParams.AddItem('OrderParameters', JsonOrderParam.AsJson(false,false), True);  not tested 
        FHttpRest.RestParams.AddItem('OrganizationInfo', JsonOrg.AsString, True);
        FHttpRest.RestParams.AddItem('AdminContact', JsonContact.AsString, True);
        FHttpRest.RestParams.AddItem('TechContact', JsonContact.AsString, True);
        FHttpRest.RestParams.AddItem('ApproverEmail', fCertApprovEmail, False);
        LogEvent (FHttpRest.RestParams.GetParameters) ;  }
        if NOT CCGetRequest (HttpPOST, 'Order', JsonItems.AsString) then exit ;
        DumpJson;
        success := FHttpRest.ResponseJson.B['success'];
        if NOT success then begin
            try
                fLastErrMsg := FHttpRest.ResponseJson.S['Message'] ;
                if fLastErrMsg <> '' then
                    fLastErrMsg := fLastErrMsg + ' - ' + FHttpRest.ResponseJson.S['ErrorField']
                else begin
                    fLastErrMsg := FHttpRest.ResponseJson.S['Errors[0].Message']  + ' - ' +
                                     FHttpRest.ResponseJson.S['Errors[0].ErrorMsg'] + ' - ' +
                                                FHttpRest.ResponseJson.S['Errors[0].ErrorField'] ;
                end;
                LogEvent ('Failed to submit CSR and Order: ' +  fLastErrMsg);
                LogEvent ('Response: ' +  FHttpRest.ResponseJson.AsString);
            except
                LogEvent ('Failed to interpret order errors');
                LogEvent ('Response: ' +  FHttpRest.ResponseJson.AsString);
            end;
            exit;
        end;

     // step six - save order information, might have certificate as well or a hash for FILE authentication
        fSuppOrderId := FHttpRest.ResponseJson.S['CertCenterOrderID'];
        OrderDT  := FHttpRest.ResponseJson.S['Timestamp'];
        Partnerorderid := FHttpRest.ResponseJson['OrderParameters'].S['PartnerOrderID'];
        LogEvent ('Succesfully ordered ' + FSuppCertProduct + ' certifcate for ' + fCertCommonName +
                      ', CertCenterOrderID: ' + fSuppOrderId + ', at ' + OrderDT +
                                                          ', PartnerOrderID: ' + Partnerorderid);

        if NOT SetPartFNames (true) then Exit ;  // set long path name with orderid
        SetFullFileNames (FPartFNameOrder) ;
        fSslCert.PrivateKeySaveToPemFile (fFilePrvKey, '', PrivKeyEncNone) ;
        LogEvent ('Saved private key file: ' + fFilePrvKey) ;
        fSslCert.SaveReqToFile(fFileCSR, true);
        LogEvent ('Saved certificate request file: ' + fFileCSR) ;

    // see if we have certificates or need to wait for them
       FIssueState := IssStateChallgPend;
       if FHttpRest.ResponseJson.S['Fulfillment'] = '' then
        begin
            if AuthMethod = 'FILE' then
            begin
             // FILE validation, create well-known file
                JsonItems := FHttpRest.ResponseJson['FileAuthDetails'];
                if Assigned (JsonItems) then begin
                    if NOT SaveWellKnown (JsonItems) then exit;
                end
                else
                    LogEvent ('Failed to get Json File Auth Details');
            end;
            if AuthMethod = 'DNS' then begin
                JsonItems := FHttpRest.ResponseJson['DNSAuthDetails'];
                if Assigned (JsonItems) then begin
                    DnsValue := JsonItems.S['DNSValue'];
                    DnsEntry := JsonItems.S['DNSEntry'];
                    PointerType := JsonItems.S['PointerType'];
                 //   LogEvent (JsonItems.AsString);   // TEMP
                end
                else
                    LogEvent('Failed to get Json DNS Auth Details');
                LogEvent('Automatic DNS validation not implemented yet, do it manually: ' + JsonItems.S['Example']);
            end;


            LogEvent('You now need to collect the ' + FSuppCertProduct +
                    ' certificate, once the order is completed, hopefully a few minutes for DV');
            LogEvent('You should receive an email when the order is completed');
        //    LogEvent(FHttpRest.ResponseJson.AsString);
            // what else should be report
        end
        else
        begin
            CCFullfillment(FHttpRest.ResponseJson, fCertCommonName);
        end;
        Result := True;
    except
        on E:Exception do begin
            LogEvent ('Failed to place order - ' + E.Message);
            exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslX509Certs.CCFullfillment (JsonOrder: ISuperObject; const CertName: string) ;
begin
    FNewCertP7Lines   := JsonOrder['Fulfillment'].S['Certificate_PKCS7'];
    FNewCertLines     := JsonOrder['Fulfillment'].S['Certificate'];
    FNewInterLines    := JsonOrder['Fulfillment'].S['Intermediate'];
    FNewCertStartDate := JsonOrder['Fulfillment'].S['StartDate'];
    FNewCertEndDate   := JsonOrder['Fulfillment'].S['EndDate'];
    if (JsonOrder.S['OrderParameters'] <> '') then begin
        FCSRLines := JsonOrder['OrderParameters'].S['CSR'];
        if FCSRLines = '' then begin
            LogEvent('Did not find Order CSR');
         //   Exit ;
        end;
    end;
    LogEvent(IcsCRLF + 'PEM Certificate for: ' + CertName + IcsCRLF + FNewCertLines + IcsCRLF);
    SaveCertificateFiles(CertName);
    FIssueState := IssStateCollect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ saves three copies of all certificate files:
    1 - work directory, includes unique order number for historic purposes
    2 - work directory, final cert name without order, replacing older version
    3 - server directory, final cert name without order, replacing older version
  Also validates certificate chain to ensure intermediate matches and CA available
}
procedure TSslX509Certs.SaveCertificateFiles(const CertName: string) ;
var
    P12KeyCipher: TSslPrivKeyCipher;
    P12Password: String;

    procedure SaveAllCerts;
    begin
        try
            FSslCert.PrivateKeySaveToPemFile (FFilePrvKey, FPrivKeyPassword, FPrivKeyCipher);
            LogEvent ('Saved private key file: ' + FFilePrvKey);
            if FCSRLines <> '' then begin
                SaveDataFile (FFileCSR, FCSRLines);
                LogEvent('Saved certificate request file: ' + FFileCSR);
            end;
            FSslCert.SaveToPemFile(FFileCertPem, False, True, False);  // no private key or inters
            LogEvent('Saved PEM certficate alone: ' + FFileCertPem);
            if FSslCert.IsInterLoaded then begin
                FSslCert.SaveIntersToToPemFile(FFileInterPem, True);
                LogEvent('Saved PEM intermediate certficate: ' + FFileInterPem);
            end;
            FSslCert.SaveToPemFile(FFileBundPem, True, True, True, FPrivKeyPassword, FPrivKeyCipher);  // add private key and inters
            LogEvent('Saved PEM bundle with certficate, key and intermediate: ' + FFileBundPem);
            FSslCert.SaveToP12File(fFileBundP12, P12Password, True, P12KeyCipher);  // add private key and inters
            LogEvent('Saved PKCS12 bundle with certficate, key and intermediate: ' + FFileBundP12);
            if FNewCertP7Lines <> '' then
                SaveDataFile (FFileBundP7, FNewCertP7Lines) ;
        except
            on E:Exception do begin
                LogEvent('Failed to save file - ' + E.Message);
            end;
        end;
    end;

begin
    LogEvent (IcsCRLF + 'Saving SSL certificate files for: ' + CertName);
    SetFullFileNames (FPartFNameOrder);
    if (Pos(PEM_STRING_HDR_BEGIN, FNewCertLines) = 0) then begin
        LogEvent('Did not receive a valid PEM certificate');
        Exit ;
    end;

 // Windows will not load PKCS12 file without a password, so create one if missing
    P12KeyCipher := FPrivKeyCipher;
    P12Password := FPrivKeyPassword;
    if P12KeyCipher = PrivKeyEncNone then P12KeyCipher := PrivKeyEncTripleDES;
    if P12Password = '' then begin
        P12Password := 'password';
        LogEvent('Set required PKCS12 file password to "password"');
    end;

    try
        FSslCert.LoadFromText(FNewCertLines, croNo, croTry, '');   // look for intermediate cert
        FSslCert.SaveToPemFile(FFileCertPem, False, True, False);  // no private key or inters
        LogEvent('Saved PEM certficate alone: ' + FFileCertPem);
        if NOT FSslCert.CheckCertAndPKey then
            LogEvent ('!!! WARNING, private key does not match certificate public key');
    except
        on E:Exception do begin
            LogEvent('Failed to recognise certificate - ' + E.Message);
            exit ;
        end;
    end;
    FNewCertCN := FSslCert.SubjectCName;
    if CertName <> FNewCertCN then
        LogEvent('Mismatch Subject Common Name, found: ' + FNewCertCN);
    FNewCertSAN := IcsUnwrapNames (fSslCert.SubAltNameDNS);
    LogEvent('Certificate Subject Alt Names (SAN): ' + FNewCertSAN);

    if NOT FSslCert.IsInterLoaded then begin
        LogEvent (IcsCRLF + 'PEM Intermediate Certificate' + IcsCRLF + FNewInterLines + IcsCRLF);
        if (Pos(PEM_STRING_HDR_BEGIN, FNewInterLines) = 0) then begin
            LogEvent ('Did not receive a valid PEM intermediate certificate: ' + FNewInterLines);
            FNewInterLines := '';
        end
        else begin
            try
                if FNewInterLines <> '' then
                    FSslCert.LoadIntersFromString(FNewInterLines);
            except
                on E:Exception do begin
                    LogEvent('Failed to recognise intermediate certificate - ' + E.Message);
                 end;
            end;
        end;
    end;
    if FSslCert.IsInterLoaded then begin
        try
            FSslCert.SaveIntersToToPemFile(FFileInterPem, True);
            LogEvent('Saved PEM intermediate certficate: ' + FFileInterPem);
        except
            on E:Exception do begin
                LogEvent ('Failed to recognise intermediate certificate - ' + E.Message);
             end;
        end;
    end;

    if FNewCertP7Lines <> '' then  begin
        LogEvent(IcsCRLF + 'PEM PKCS7 Certificate' + IcsCRLF + FNewCertP7Lines + IcsCRLF) ;
        SaveDataFile(FFileBundP7, FNewCertP7Lines) ;
    end;

 // log certificate content
      LogEvent (IcsCRLF + 'Certificate Details: ' + IcsCRLF +
                       FSslCert.CertInfo(False) + IcsCRLF + IcsCRLF);

 // save PEM bundle file for Apache and PKCS12 bundle for Windows, both with key passworded
    try
        FSslCert.SaveToPemFile(FFileBundPem, True, True, True, FPrivKeyPassword, FPrivKeyCipher);  // add private key and inters
        LogEvent('Saved PEM bundle with certficate, key and intermediate: ' + fFileBundPem);

        FSslCert.SaveToP12File(FFileBundP12, P12Password, True, P12KeyCipher);  // add private key and inters
        LogEvent('Saved PKCS12 bundle with certficate, key and intermediate: ' + FFileBundP12);
    except
        on E:Exception do
        begin
            LogEvent ('Failed to save bundle - ' + E.Message);
        end;
    end;

// now validate and report certificate  chain
    try
        if NOT Assigned(FRootCAX509) then begin  { V8.55  }
            FRootCAX509 := TX509Base.Create (Self);
            FRootCAX509.LoadCATrustFromString(sslRootCACertsBundle);  // builtin roots
        end ;
        FSslCert.X509CATrust := FRootCAX509.X509CATrust;
     { V8.47 warning, currently only checking first Host name }
        FNewCertValRes := FSslCert.ValidateCertChain(CertName, FNewCertChainInfo, FNewCertErrs);
        if FNewCertValRes = chainOK then
            LogEvent ('SSL certificate chain validated OK: ' + IcsCRLF + FNewCertChainInfo)
        else begin
            if FNewCertValRes = chainWarn then
                FNewCertErrs := 'Chain Warning - ' + FNewCertErrs
            else begin
                FNewCertErrs := 'Chain Failed - ' + FNewCertErrs;
                LogEvent ('SSL certificate errors - ' + FNewCertErrs);
             end;
        end;
    except
        on E:Exception do begin
            LogEvent ('Failed to validate chain - ' + E.Message);
            Exit;
        end;
    end;

// finally save files again without order number, for SSL server
    LogEvent ('Saving final versions of all files without order numbers locally');
    SetFullFileNames (FPartFNameFinal);
    SaveAllCerts;
    if (FPartFNameFinal <> fPartFNameServer) and (fPartFNameServer <> '') then begin
        LogEvent ('Saving final versions of all files without order numbers on server');
        SetFullFileNames (fPartFNameServer);
        SaveAllCerts;
    end;
    LogEvent('Finished collecting and saving certificate for ' + CertName + IcsCRLF + IcsCRLF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function TSslX509Certs.CCListAllOrders: Boolean;
var
    success: Boolean ;
    JsonItems, JsonOrder: ISuperObject;
    I: integer;
    CommonName, OrderId, MajorStatus, OrderDate, Product: string;
    UpdateDate: string ;
begin
    Result := False;
    LogEvent (IcsCRLF + 'Downloading Recent CertCentre Orders');
    FHttpRest.RestParams.Clear;
    FHttpRest.RestParams.PContent := PContUrlencoded;
//    URL := 'Orders?Status=COMPLETE,PENDING&includeFulfillment=False&' +
//             'includeOrderParameters=True&ItemsPerPage=100&includeDCVStatus=True' ;
    FHttpRest.RestParams.AddItem('Status', 'COMPLETE,PENDING', False);
    FHttpRest.RestParams.AddItem('includeFulfillment', 'False', False);
    FHttpRest.RestParams.AddItem('includeOrderParameters','True', False);
    FHttpRest.RestParams.AddItem('ItemsPerPage', '100', False);
    FHttpRest.RestParams.AddItem('includeDCVStatus', 'True', False);
    if NOT CCGetRequest (HttpGET, 'Orders') then exit ;
    if NOT Assigned (FHttpRest.ResponseJson) then Exit ;
    DumpJson;
    success := FHttpRest.ResponseJson.B['success'];
    if NOT success then begin
        LogEvent ('Failed to list Orders: ' +  FHttpRest.ResponseJson.S['Message']);
        exit;
    end;
    JsonItems := FHttpRest.ResponseJson['OrderInfos'];
    if NOT Assigned (JsonItems) or (JsonItems.AsArray.Length = 0) then begin
        LogEvent ('Did not find any Orders');
        exit;
    end;
    LogEvent ('Total Orders Found: ' + IntToStr(JsonItems.AsArray.Length) + IcsCRLF);
    Result := True;
    for I := 0 to JsonItems.AsArray.Length - 1 do begin
        JsonOrder := JsonItems.AsArray.O[I] ;  // get one order
     //   LogEvent (JsonOrder.AsString) ;                // print one order
        OrderId := JsonOrder.S['CertCenterOrderID'] ;
        CommonName := JsonOrder.S['CommonName'] ;
        MajorStatus := JsonOrder.S['OrderStatus.MajorStatus'] ;
        OrderDate := JsonOrder.S['OrderStatus.OrderDate'] ;
        UpdateDate := JsonOrder.S['OrderStatus.UpdateDate'] ;
        Product := JsonOrder.S['OrderParameters.ProductCode'] ;
        LogEvent ('Order ' + OrderId + ' for ' + CommonName +  ' - ' + Product +
                 ' at ' + OrderDate + ' ' + MajorStatus + ', Updated ' + UpdateDate) ;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.CCGetOneOrder: Boolean;
var
    success: Boolean ;
    JsonOrder: ISuperObject;
    CommonName, OrderId, Product, MajorStatus, MinorStatus: string ;
begin
    result := false ;
    if (FSuppOrderId = '') then begin
        LogEvent ('Must specify Order ID to check order');
        Exit;
    end;
    if (fCertCommonName = '') then begin
        LogEvent ('Must specify Domain Common Name to check order');
        Exit;
    end;
    LogEvent (IcsCRLF + 'Checking CertCentre order status for ' + FSuppOrderId +
                                        ' certificate order for: ' + fCertCommonName);
    if NOT SetPartFNames (false) then Exit ;  // set long path name with orderid
    SetFullFileNames (FPartFNameOrder) ;

   // load private key for order
    if NOT FileExists (fFilePrvKey) then begin
        LogEvent ('Can not find private key for this order: ' +  fFilePrvKey);
        Exit ;
    end;
    try
        fSslCert.ClearAll ;
        fSslCert.PrivateKeyLoadFromPemFile (fFilePrvKey, '') ;
    except
        on E:Exception do begin
            LogEvent ('Failed to load private key file: ' + fFilePrvKey + ' - ' + E.Message);
            Exit ;
        end;
    end;
    if NOT fSslCert.IsPKeyLoaded then begin
        LogEvent ('Failed to load private key for this order: ' +  fFilePrvKey);
        Exit ;
    end;
    fPrvKeyLines := fSslCert.SavePKeyToText ('', PrivKeyEncNone) ;
    LogEvent (IcsCRLF + 'Downloading CertCentre OrderId: ' + fSuppOrderId);
    FHttpRest.RestParams.Clear;
    FHttpRest.RestParams.PContent := PContUrlencoded;
//    URL :='Order/' + UrlEncode(fSuppOrderId) + '?includeFulfillment=True&' +
//        'includeOrderParameters=True&includeBillingDetails=True&' +
//                     'includeContacts=True&includeOrganizationInfos=True';
    FHttpRest.RestParams.AddItem('includeFulfillment', 'True', False);
    FHttpRest.RestParams.AddItem('includeOrderParameters','True', False);
    FHttpRest.RestParams.AddItem('includeBillingDetails', 'True', False);
    FHttpRest.RestParams.AddItem('includeContacts', 'True', False);
    FHttpRest.RestParams.AddItem('includeOrganizationInfos', 'True', False);
    if NOT CCGetRequest (HttpGET, 'Order/' + UrlEncode(fSuppOrderId)) then exit ;
//    if NOT Assigned (FHttpRest.ResponseJson) then Exit ;
    DumpJson;
 {"Message": "Invalid OrderID (6666)",
  "ErrorField": "CertCenterOrderID",
  "ErrorId": -2011,
  "success": false }
    success := FHttpRest.ResponseJson.B['success'];
    if NOT success then begin
        LogEvent ('Failed to get order: ' +  FHttpRest.ResponseJson.S['Message']);
        exit;
    end;
    JsonOrder := FHttpRest.ResponseJson.O['OrderInfo'];
//    LogEvent (JsonOrder.AsString) ;                // print one order
    OrderId := JsonOrder.S['CertCenterOrderID'];
    CommonName := JsonOrder.S['CommonName'];
    MajorStatus := JsonOrder.S['OrderStatus.MajorStatus'];
    MinorStatus := JsonOrder.S['OrderStatus.MinorStatus'];
    Product := JsonOrder.S['OrderParameters.ProductCode'];
    LogEvent ('Found order ' + OrderId + ' for ' + CommonName + ' - ' + Product +
                                ', Status ' + MajorStatus + ' - ' + MinorStatus);
    LogEvent ('Parsing OrderStatus') ;
    DumpJson('OrderInfo.OrderStatus');

// now load certitficates and save files
    if (JsonOrder.S['OrderParameters'] <> '') then begin
        LogEvent ('Parsing OrderParameters') ;
        DumpJson('OrderInfo.OrderParameters');
    end;
    if JsonOrder.S['BillingInfo'] <> '' then begin
        LogEvent ('Parsing BillingInfo') ;
        DumpJson('OrderInfo.BillingInfo');
    end;
    if JsonOrder.S['ContactInfo'] <> '' then begin
        LogEvent ('Parsing ContactInfo') ;
        DumpJson('OrderInfo.ContactInfo');
    end;
    if JsonOrder.S['OrganizationInfo'] <> '' then begin
        LogEvent ('Parsing OrganizationInfo') ;
        DumpJson('OrderInfo.OrganizationInfo');
    end;
    if JsonOrder.S['FileAuthDetails'] <> '' then begin
        LogEvent ('Parsing FileAuthDetails') ;
        DumpJson('OrderInfo.FileAuthDetails');
    end;
    if MajorStatus <> 'COMPLETE' then begin
        if MajorStatus = 'PENDING' then
            FIssueState := IssStateChallgPend;
        Exit ;
    end;
    if JsonOrder.S['Fulfillment'] <> '' then begin
        LogEvent ('Parsing Fulfillment') ;
        DumpJson('Fulfillment');
        CCFullfillment (JsonOrder, CommonName) ;
        result := true ;
    end
    else
        LogEvent ('Failed to find Fulfillment') ;

    LogEvent ('Finished collecting order for ' + Product) ;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.CCCancelOrder (Revoke: Boolean): Boolean;
var
    JsonOrder: ISuperObject;
    CommonName, OrderId, MajorStatus, MinorStatus: string ;
begin
    result := false ;
    if NOT SetPartFNames (true) then Exit ;  // set long path name with orderid
    SetFullFileNames (FPartFNameOrder) ;

   // for revoke, we need the old certificate
    if revoke then begin
        if NOT FileExists (fFileCertPem) then begin
            LogEvent ('Can not find cetificate for this order: ' +  fFileCertPem);
            Exit ;
        end;
        try
            fSslCert.ClearAll ;
            fSslCert.LoadFromPemFile (fFileCertPem) ;
        except
            on E:Exception do begin
                LogEvent ('Failed to load certifcate file: ' + fFileCertPem + ' - ' + E.Message);
                Exit ;
            end;
        end;
        if NOT fSslCert.IsCertLoaded then begin
            LogEvent ('Failed to load cetificate for this order: ' +  fFileCertPem);
            Exit ;
        end;
        FNewCertLines := FSslCert.SaveCertToText (false) ;
    end;

    LogEvent (IcsCRLF + 'Checking CertCentre OrderId: ' + fSuppOrderId + ' for ' + fCertCommonName);
    FHttpRest.RestParams.Clear;
    FHttpRest.RestParams.PContent := PContUrlencoded;
//    URL :='Order/' + UrlEncode(fSuppOrderId) + '?includeFulfillment=False&' +
///        'includeOrderParameters=False=True&includeBillingDetails=False&' +
//                     'includeContacts=False&includeOrganizationInfos=False';
    FHttpRest.RestParams.AddItem('includeFulfillment', 'False', False);
    FHttpRest.RestParams.AddItem('includeOrderParameters','False', False);
    FHttpRest.RestParams.AddItem('includeBillingDetails', 'False', False);
    FHttpRest.RestParams.AddItem('includeContacts', 'False', False);
    FHttpRest.RestParams.AddItem('includeOrganizationInfos', 'False', False);
    if NOT CCGetRequest (HttpGET, 'Order/' + UrlEncode(fSuppOrderId)) then exit ;
    DumpJson;
    if NOT FHttpRest.ResponseJson.B['success'] then begin
        LogEvent ('Failed to find order: ' +  FHttpRest.ResponseJson.S['Message']);
        exit;
    end;
    JsonOrder := FHttpRest.ResponseJson.O['OrderInfo'];
    OrderId := JsonOrder.S['CertCenterOrderID'] ;
    CommonName := JsonOrder.S['CommonName'] ;
    if CommonName <> fCertCommonName then begin
        LogEvent ('Mismatch common name, found: ' +  CommonName);
        exit;
    end;
    MajorStatus := JsonOrder.S['OrderStatus.MajorStatus'] ;
    MinorStatus := JsonOrder.S['OrderStatus.MinorStatus'] ;
    LogEvent ('Found order ' + OrderId + ' for ' + CommonName + ', Status ' +
                                            MajorStatus + ' - ' + MinorStatus) ;

  // revoke adds the certificate to OCP and CRL lists to blacklist it
    if revoke then begin
        LogEvent (IcsCRLF + 'Revoking CertCentre OrderId: ' + fSuppOrderId + ' for ' + fCertCommonName);
        FHttpRest.RestParams.Clear;
        FHttpRest.RestParams.PContent := PContUrlencoded;
     //   URL :='Revoke/' + UrlEncode(fSuppOrderId) + '&RevokeReason=' +
     //       UrlEncode('Replaced certificate') + '&Certificate=' + UrlEncode(fCertLines) ;
        FHttpRest.RestParams.AddItem('RevokeReason', 'Replaced certificate', False);
        FHttpRest.RestParams.AddItem('Certificate', FNewCertLines, False);
        if NOT CCGetRequest (httpDELETE, 'Revoke/' + UrlEncode(fSuppOrderId)) then exit ;
        DumpJson;
        if FHttpRest.ResponseJson.B['success'] then begin
            LogEvent ('Succesfully revoked order: ' +  FHttpRest.ResponseJson.AsString);
            Result := True;
        end
        else
            LogEvent ('Failed to revoke order: ' +  FHttpRest.ResponseJson.AsString);
      //   {"Message": "Method not found", "ErrorId": 403, "success": false}
    end

  // simple cancellation within 30 days should get a refund
    else begin
        LogEvent (IcsCRLF + 'Cancelling CertCentre OrderId: ' + fSuppOrderId + ' for ' + fCertCommonName);
        FHttpRest.RestParams.Clear;
  //      URL :='Order/' + UrlEncode(fSuppOrderId);
        if NOT CCGetRequest (httpDELETE, 'Order/' + UrlEncode(fSuppOrderId)) then exit ;
        DumpJson;
        if FHttpRest.ResponseJson.B['success'] then begin
            LogEvent ('Succesfully cancelled order: ' +  FHttpRest.ResponseJson.AsString);
            Result := True;
        end
        else
            LogEvent ('Failed to Cancel order: ' +  FHttpRest.ResponseJson.AsString);
       //  {"Message": "Order has been successfully cancelled", "success": true}
       // {"Errors": [{"Message": "Allgemeiner Fehler", "ErrorField": "ApproverEmail", "ErrorId": -2006}], "success": false}
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.AcmeGetRequest(HttpReq: THttpRequest;
                        const FullURL: String; AcmeJson: ISuperObject): boolean;
var
    I: integer;
    S, JsonWebSig, JsonReq: string;
begin
    result := false;
    if Pos ('https://', FullURL) <> 1 then  begin
        LogEvent ('Invalid URL: ' + FullURL);
        exit;
    end;
    FHttpRest.ServerAuth := httpAuthNone;
    FHttpRest.DebugLevel := FDebugLevel;
    FHttpRest.Agent := 'ICS-ACME1-V8.54';
    if (Pos ('/new-cert', FullURL) > 1) or (Pos ('/cert/', FullURL) > 1) or
                                         (Pos ('issuer-cert', FullURL) > 1) then
        FHttpRest.Accept := 'application/pkix-cert'
     else
        FHttpRest.Accept := '*/*' ;
    FHttpRest.FollowRelocation := False;  // nonce will fail since unchanged
    FHttpRest.ContentTypePost := 'application/jose+json';
    fAcmeRespLocation := '';
    try

      // Json parameters need to be signed by private key as a Json Web Signature
      // adding nonce from last request to prevent playback
        if HttpReq = httpPOST then begin
            JsonReq := AcmeJson.AsJson(False, False);
            if FDebugLevel >= DebugParams then
                LogEvent ('AcmeJson: ' + JsonReq);

          // Acme v1 sends the public key with every request, lengthy
            if FSupplierProto = SuppProtoAcmeV1 then
                JsonWebSig := IcsJoseJWSAcme1(fAcmeJoseAlg, JsonReq,
                        fAcmePrivKey.PrivateKey, fAcmeKwkPub, fAcmeRespNonce)
            else begin

          // Acme v2 sends the public key once, which is stored on the server and then
          // a shorter KeyId sent in subsequent requests , with nonce and URL
                if fAcmeKwkKid = '' then
                    JsonWebSig := IcsJoseJWSJson(fAcmeJoseAlg, JsonReq, '',
                        fAcmePrivKey.PrivateKey, '', fAcmeKwkPub, '', fAcmeRespNonce, FullURL)
                else
                    JsonWebSig := IcsJoseJWSJson(fAcmeJoseAlg, JsonReq, '',
                        fAcmePrivKey.PrivateKey, '', '', fAcmeKwkKid, fAcmeRespNonce, FullURL);
            end;
            FHttpRest.RestParams.Clear;
            fAcmeLastStatus := FHttpRest.RestRequest(httpPOST, FullURL, false, JsonWebSig);
        end
        else
            fAcmeLastStatus := FHttpRest.RestRequest(HttpReq, FullURL, false, '');
    except
        on E:Exception do begin
            LogEvent('Failed to contact CertCentre Server: ' + E.Message);
            // don't exit, may still have something useful
        end;
    end;

 // pending, should be loop to retry  ???

    try
      { look for special Acme headers
        HTTP/1.1 409 Conflict
        Content-Type: application/problem+json
        Boulder-Requester: 5592135
        Location: https://acme-staging.api.letsencrypt.org/acme/reg/5592135
        Replay-Nonce: r1PKRhkQqI7GWwk1sGNwH_V5w2h0U7S0C44jq8vNhH0

        HTTP/1.1 201 Created
        Content-Type: application/json
        Boulder-Requester: 5592485
        Link: <https://acme-staging.api.letsencrypt.org/acme/new-authz>;rel="next"
        Link: <https://letsencrypt.org/documents/LE-SA-v1.2-November-15-2017.pdf>;rel="terms-of-service"
        Location: https://acme-staging.api.letsencrypt.org/acme/reg/5592485
        Replay-Nonce: JcprpXrX-4qNnubjMHWy9XjXcCi-B2xbvpS9spgJrqE    }

        if FHttpRest.RcvdHeader.Count > 0 then begin
            for I := 0 to Pred (FHttpRest.RcvdHeader.Count) do begin
                S := FHttpRest.RcvdHeader [I];
                if Pos ('Replay-Nonce: ', S) = 1 then
                    fAcmeRespNonce := Copy (S, 15, 999);
             // warning, may be two or more locations
                if Pos ('Link: ', S) = 1 then
                    fAcmeRespLink := Copy (S, 7, 999);
                if Pos ('Boulder-Requester: ', S) = 1 then
                    fAcmeRespRequester := Copy (S, 20, 999);
                if Pos ('Location: ', S) = 1 then
                    fAcmeRespLocation := Copy (S, 11, 999);
                if Pos ('Content-Location: ', S) = 1 then
                    fAcmeRespContLoc := Copy (S, 19, 999);
            end;
        end;
        if HttpReq = httpHEAD then begin
            if (fAcmeLastStatus <> 200) and (fAcmeLastStatus <> 204) and (fAcmeLastStatus <> 405) then
                 LogEvent ('Failed to contact Server, HEAD:' + FHttpRest.LastResponse)
            else
                Result := true;  // got a new nonce, hopefully
            exit;
        end;
        if FHttpRest.ResponseSize = 0 then begin
            if fAcmeLastStatus <> 200 then
                 LogEvent ('Failed to contact Server, Zero Content:' + FHttpRest.LastResponse)
            else
                Result := true;
            Exit ;
        end;

      { V1 provides binary DER for one certificate, convert it to PEM }
        if (Pos('application/pkix-cert', FHttpRest.ContentType) = 1) then begin
            fAcmeCertLines := '';
            fAcmeCertLines := '-----BEGIN CERTIFICATE-----' + IcsCRLF +
                              String(Base64Encode(FHttpRest.ResponseOctet)) + IcsCRLF +
                              '-----END CERTIFICATE-----' + IcsCRLF;
        end;

      { V2 provides multiple proper PEM certificates }
        if (Pos('application/pem-certificate-chain', FHttpRest.ContentType) = 1) then begin
            fAcmeCertLines := '';
            fAcmeCertLines := String(FHttpRest.ResponseOctet);
        end;
    except
        on E:Exception do begin
            LogEvent ('Failed to process response: ' + E.Message);
        end;
    end;
    result := true ;  // OK
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// find or create Acme account, get action URLs
function TSslX509Certs.SetAcmeAccount: boolean;
var
    Proto, User, Pass, Port, Path : String;
begin
    Result := False;
    FIssueState := IssStateNone;
    if Pos ('https://', FSupplierServer) <> 1 then begin
        LogEvent('Invalid certificate supplier server: ' + FSupplierServer);
    end;
    FDirCertWork := IncludeTrailingPathDelimiter(Trim(FDirCertWork));
    ParseURL(FSupplierServer, Proto, User, Pass, fAcmeHost, Port, Path);
    fAcmePubFName := DirCertWork + 'AcmePublicKey.pem' ;
    fAcmePrivFName := DirCertWork + 'AcmePrivateKey.pem' ;
    fAcmeKwkKid := '';
    fCertValidity := 3;
    FNewCertPrefix := 'LE-' ;
    fAcmeChallengeURL := '';
    fAcmeOrderFinalizeURL := '';
    FAcmeOrderObjUrl := '';
    fSuppOrderId := '';
    if NOT AcmeLoadPKey(True) then exit; // new account
    if NOT AcmeGetActions then exit;
    if SupplierProto = SuppProtoAcmeV1 then
        Result := AcmeV1NewAccount
    else if SupplierProto = SuppProtoAcmeV2 then
        Result := AcmeV2NewAccount;
end;


 {* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// create or get Acme account private key for signing Json requests
// an DirCertWork is considered an account, one dir per account
// Not used for any certificates !!!!
function TSslX509Certs.AcmeLoadPKey(New: Boolean): Boolean;
begin
    Result := False;
    try
        if (NOT New) and (NOT DirectoryExists (DirCertWork)) then begin
            LogEvent ('Account directory not found: ' + DirCertWork);
            exit;
        end;

    // create admin working directory and files names
        LogEvent ('Checking account directory: ' + DirCertWork);
        if NOT ForceDirectories (DirCertWork) then begin
            LogEvent ('Failed to create directory: ' + DirCertWork);
            exit;
        end;

    // get private keys, Acme prefers Elliptic Curve since shorter
        fAcmePrivKey.PrivKeyType := fAcmeAccKeyType;
        case fAcmeAccKeyType of
            PrivKeyRsa2048, PrivKeyRsa3072, PrivKeyRsa4096: fAcmeJoseAlg := jsigRsa256;
            PrivKeyECsecp256: fAcmeJoseAlg := jsigEcdsa256;
            PrivKeyECsecp384: fAcmeJoseAlg := jsigEcdsa384;
            PrivKeyECsecp512: fAcmeJoseAlg := jsigEcdsa512;
            PrivKeyRsaPss2048, PrivKeyRsaPss3072, PrivKeyRsaPss4096: fAcmeJoseAlg := jsigRsaPss256;
            PrivKeyEd25519: fAcmeJoseAlg := jsigEdDSA;
        end;
        fAcmePrivKey.PrivateKey := Nil;

        if (FileExists (fAcmePrivFName)) then begin  // load account private key
            try
                LogEvent ('Loading old private key file: ' + fAcmePrivFName);
                fAcmePrivKey.PrivateKeyLoadFromPemFile (fAcmePrivFName, '');
                LogEvent ('Loaded old private key OK: ' + fAcmePrivKey.PrivateKeyInfo);
            except
                on E:Exception do begin
                    LogEvent ('Exception loading private key: ' + E.Message + ' - ' + fAcmePrivFName);
                    exit;
                end;
            end;
        end
        else begin
            if NOT New then begin
                LogEvent ('Failed to find old private key: ' + fAcmePrivFName);
                exit;
            end;
            try
                fAcmePrivKey.DoKeyPair;
                LogEvent ('Generated private key OK: ' + fAcmePrivKey.PrivateKeyInfo);
                fAcmePrivKey.PrivateKeySaveToPemFile (fAcmePrivFName, '', PrivKeyEncNone);
                LogEvent ('Saved private key file: ' + fAcmePrivFName);
                fAcmePrivKey.PublicKeySaveToPemFile (fAcmePubFName);
                fPrvKeyLines := fAcmePrivKey.SavePKeyToText ('', PrivKeyEncNone);
                LogEvent ('Saved public key file: ' + fAcmePubFName);
                if FLogPkeys then LogEvent (IcsCRLF + fPrvKeyLines + IcsCRLF);
            except
                on E:Exception do begin
                    LogEvent ('Failed to generate private key - ' + E.Message);
                    exit ;
                end;
            end;
         end;

     // build public Json Web Key for Json Web Signing
     // basic jwk, no alg, kid or sig
        fAcmeJwsAlg := IcsJoseFindAlg(fAcmeJoseAlg, fAcmePrivKey.PrivateKey);
        fAcmeKwkPub := IcsJoseJWKPubKey(fAcmePrivKey.PrivateKey, '', '', '');
        LogEvent ('JWK: ' + fAcmeKwkPub);

     // create JWK Thumbprint, used for challenge
        fAcmeKwkThumb := IcsBase64UrlEncode(String(IcsHashDigest(AnsiString(fAcmeKwkPub), Digest_sha256)));
        LogEvent ('Thumbprint: ' + fAcmeKwkThumb);
        Result := true;
    except
        on E:Exception do begin
            LogEvent ('Fatal ACME protocol error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// contact ACME server to get ACME Server Action URLs and T&Cs
function TSslX509Certs.AcmeGetActions: Boolean;
var
    MetaJson: ISuperObject;
    TermsFname, FullName: String;
    I: integer;
begin
    Result := False;
    try
        LogEvent ('Getting actions from ACME server: ' + fAcmeHost);
        fAcmeRespNonce := '';
        if NOT AcmeGetRequest(httpGET, FSupplierServer, Nil) then exit;
        LogEvent ('Downloaded ACME Server Action URLs OK');
        for I := 1 to AcmeActionTot do begin
            AcmeActionDirs [I].URL := FHttpRest.ResponseJson.S[AcmeActionDirs [I].Action];
        end;
        MetaJson := FHttpRest.ResponseJson.O['meta'];
        if Assigned(MetaJson) then begin
            fAcmeTermsUrl := MetaJson.S['terms-of-service'];   // V1
            if fAcmeTermsUrl = '' then fAcmeTermsUrl := MetaJson.S['termsOfService']; // V2
            if fAcmeTermsUrl <> '' then begin
                I := LastDelimiter('/', fAcmeTermsUrl);
                if I > 0 then begin
                    TermsFname := Copy (fAcmeTermsUrl, I + 1, 999);
                    FullName := DirCertWork + TermsFName;
                    if NOT (FileExists (FullName)) then begin
                        if NOT AcmeGetRequest(httpGET, fAcmeTermsUrl, Nil) then exit;
                        if FHttpRest.ResponseStream.Size > 256 then
                        try
                            FHttpRest.ResponseStream.SaveToFile(FullName);
                            LogEvent ('Downloaded new Terms: ' + FullName);
                        except
                            on E:Exception do
                                LogEvent ('Failed to save terms file: ' +
                                                Fullname + ' - ' + E.Message);
                        end;
                    end
                    else
                        LogEvent ('Terms already downloaded: ' + FullName);
                end;
            end;
        end;
        Result := True;
    except
        on E:Exception do begin
            LogEvent ('Fatal ACME protocol error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.AcmeV1NewAccount: Boolean;
var
    I: Integer;
begin
    LogEvent ('Registering account with ACME Server: ' + fAcmeHost);
    Result := False;
    fAcmeAccountUrl := '';
    fAcmeKwkKid := '';
    fAcmeAccountNum := '';
    try

    // get first Nonce
        if fAcmeRespNonce = '' then begin
            LogEvent ('Get new nonce');
        // acme v1 does not support NewNonce, but we should have it from directory command
            if NOT AcmeGetRequest(httpHEAD, AcmeActionDirs [AcmeNewReg1].URL, Nil) then exit;
            LogEvent ('Initial Nonce: ' + fAcmeRespNonce);
        end;

     // register and create an account, may have one already for this key if old
        if NOT AcmeGetRequest(httpPOST, AcmeActionDirs [AcmeNewReg1].URL,
            SO(['resource', 'new-reg', 'agreement', fAcmeTermsUrl, 'contact',
                SA(['mailto:' + FCertContactEmail]) ]) ) then exit;
        DumpJson;

(* HTTP/1.1 201 Created
{
  "id": 5592485,
  "key": {
    "kty": "RSA",
    "n": "osJT-PZqVCW4wj8_Vxxx",
    "e": "AQAB"
  },
  "contact": [
    "mailto:angus@magsys.co.uk"
  ],
  "agreement": "https://letsencrypt.org/documents/LE-SA-v1.2-November-15-2017.pdf",
  "initialIp": "82.33.197.157",
  "createdAt": "2018-02-17T13:06:35.228116496Z",
  "status": "valid"
}
{
  "type": "urn:acme:error:malformed",
  "detail": "Parse error reading JWS",
  "status": 400
}
  *)

    // did we creatr an account OK, or find one that matches the private key
        if (fAcmeLastStatus = 200) or (fAcmeLastStatus = 409) then begin
            if fAcmeRespLocation <> '' then begin
                fAcmeAccountUrl := fAcmeRespLocation;
                fAcmeKwkKid := fAcmeAccountUrl;       // this is our Kid for future requests
                I := LastDelimiter('/', fAcmeAccountUrl);
                if I > 10 then
                    fAcmeAccountNum := Copy(fAcmeAccountUrl, I + 1, 999);  // not in response with v2
                LogEvent('Using old Acme Account for this key: ' + fAcmeAccountNum +
                                                        ', URL: ' + fAcmeAccountUrl);
            end;
        end
        else if fAcmeLastStatus = 201 then begin
            if fAcmeRespLocation <> '' then begin
                fAcmeAccountUrl := fAcmeRespLocation;
                fAcmeKwkKid := fAcmeAccountUrl;       // this is our Kid for future requests
           //     fAcmeAccountNum := fAcmeRespRequester;
                LogEvent('Created Acme Account: ' + fAcmeAccountNum +
                                                        ', URL: ' + fAcmeAccountUrl);
            end;
        end
        else begin
            LogEvent('Failed to Create Acme Account: ' + FHttpRest.ResponseJson.S['type'] +
                                                 ', ' + FHttpRest.ResponseJson.S['detail']);
            Exit;
        end;
        FIssueState := IssStateAccount;
        Result := True;
    except
        on E:Exception do begin
            LogEvent ('Fatal ACME protocol error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.AcmeV2NewAccount: Boolean;
var
    I: Integer;
begin
    LogEvent ('Registering account with ACME Server: ' + fAcmeHost);
    Result := False;
    fAcmeAccountUrl := '';
    fAcmeKwkKid := '';   // must be blank for AcmeNewAccount2
    fAcmeAccountNum := '';
    try

    // get first Nonce
        if fAcmeRespNonce = '' then begin
            LogEvent ('Get new nonce');
            if NOT AcmeGetRequest(httpHEAD, AcmeActionDirs [AcmeNewNonce2].URL, Nil) then exit;
            LogEvent ('Initial Nonce: ' + fAcmeRespNonce);
        end;

     // register and create an account, may have one already for this key if old
        if NOT AcmeGetRequest(httpPOST, AcmeActionDirs [AcmeNewAccount2].URL,
              SO(['termsOfServiceAgreed', true, 'contact', SA(['mailto:' + FCertContactEmail]) ]) ) then exit;
        DumpJson;

    // did we creatr an account OK, or find one that matches the private key
        if (fAcmeLastStatus = 200) or (fAcmeLastStatus = 409) then begin
            if fAcmeRespLocation <> '' then begin
                fAcmeAccountUrl := fAcmeRespLocation;
                fAcmeKwkKid := fAcmeAccountUrl;       // this is our Kid for future requests
                I := LastDelimiter('/', fAcmeAccountUrl);
                if I > 10 then
                    fAcmeAccountNum := Copy(fAcmeAccountUrl, I + 1, 999);  // not in response with v2
                LogEvent('Using old Acme Account for this key: ' + fAcmeAccountNum +
                                                        ', URL: ' + fAcmeAccountUrl) ;
            end;
        end
        else if fAcmeLastStatus = 201 then begin
            if fAcmeRespLocation <> '' then begin
                fAcmeAccountUrl := fAcmeRespLocation;
                fAcmeKwkKid := fAcmeAccountUrl;       // this is our Kid for future requests
                fAcmeAccountNum := fAcmeRespRequester;
                LogEvent('Created Acme Account: ' + fAcmeAccountNum +
                                                        ', URL: ' + fAcmeAccountUrl) ;
            end;
        end
        else begin
            LogEvent('Failed to Create Acme Account: ' + FHttpRest.ResponseJson.S['type'] +
                                                 ', ' + FHttpRest.ResponseJson.S['detail']) ;
            Exit;
        end;
        FIssueState := IssStateAccount;
        Result := True;
    except
        on E:Exception do begin
            LogEvent ('Fatal ACME protocol error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Acme check order for a SSL certificate, make sure domain exists
function TSslX509Certs.AcmeCheckOrder: Boolean;
begin
    Result := false ;
    FCertCommonName := Trim(FCertCommonName);
    FDirWellKnown := IncludeTrailingPathDelimiter(Trim(FDirWellKnown));
    LogEvent (IcsCRLF + 'Checking Let''s Encrypt certificate order for: ' + fCertCommonName);
    if (fSuppCertChallenge = ChallDNS) then begin
        LogEvent ('DNS validation not available for this certificate');
        Exit;
    end;
    if (fSuppCertChallenge = ChallEmail) then begin
        LogEvent ('EMAIL validation not available for this certificate');
        Exit;
    end;
    if (fSuppCertChallenge = ChallSNI) then begin
        LogEvent ('SNI validation not available for this certificate');
        Exit;
    end;

  // where the well known directory is located
    if (fSuppCertChallenge <= ChallFileSrv) then begin
        if NOT TestWellKnown(FCertCommonName, FDirWellKnown) then exit;
    end;

    if (FSupplierProto <> SuppProtoAcmeV2) and (Pos ('*.', fCertCommonName) = 1) then begin
        LogEvent ('Wild card certificates not supported');
        Exit;
    end;
    FIssueState := IssStateChecked;
    Result := True;
 end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.AcmeV1OrderCert: Boolean;
var
    ArrayJson, ChallgJson, ValidJson: ISuperObject;
    Token, AuthMethod, KeyAuth, WKFullName: string ;
    I, secswait: integer;
    Trg: longword;
begin
    Result := False;
    fAcmeOrderStatus := '';
    LogEvent (IcsCRLF + 'Starting Let''s Encrypt certificate order for: ' + fCertCommonName);
    try
        fAcmeChallengeURL := '';
        Token := '';
    //    Validity := 12 ;  // months
        case fSuppCertChallenge of
           ChallFileUNC: AuthMethod := 'http-01';
           ChallFileFtp: AuthMethod := 'http-01';
           ChallFileSrv: AuthMethod := 'http-01';
           ChallDNS:     AuthMethod := 'dns-01';
           ChallSNI:     AuthMethod := 'tls-sni-02';
        end;

      // where the well known directory is located
         if fSuppCertChallenge <= ChallFileSrv then begin
            if NOT TestWellKnown(FCertCommonName, FDirWellKnown) then exit;
         end;

    //  must have a valid nonce to do POST requests
        if fAcmeRespNonce = '' then begin
        // acme v1 does not support NewNonce, but we should have it from directory command
            if NOT AcmeGetRequest(httpHEAD, AcmeActionDirs [AcmeNewAuthz1].URL, Nil) then exit;
            LogEvent ('Initial Nonce: ' + fAcmeRespNonce);
        end;

    // new authorisation request, get a challenge
        if NOT AcmeGetRequest(httpPOST, AcmeActionDirs [AcmeNewAuthz1].URL,
               SO([ 'resource', 'new-authz', 'identifier',
                   SO (['type', 'DNS', 'value', fCertCommonName]) ])) then exit;
        DumpJson;

 (*
 {
  "identifier": {
    "type": "dns",
    "value": "test3.telecom-tariffs.co.uk"
  },
  "status": "pending",
  "expires": "2018-02-26T17:51:37.280339321Z",
  "challenges": [
    {
      "type": "dns-01",
      "status": "pending",
      "uri": "https://acme-staging.api.letsencrypt.org/acme/challenge/Osw5X5KZ4TiagajE60ppyENauMOJ7ameIm5X0M_UxqM/102864077",
      "token": "RjKG3tQF8kgEhS9evvVIoVPVkzvjMem5RBqzIDzHmFI"
    },
    {
      "type": "http-01",
      "status": "pending",
      "uri": "https://acme-staging.api.letsencrypt.org/acme/challenge/Osw5X5KZ4TiagajE60ppyENauMOJ7ameIm5X0M_UxqM/102864078",
      "token": "rRzrYVEZ3jT3vt_Tf2i1iHOaH_RPvLllnQfjVxK7RvA"
    }
      {
         "type": "tls-sni-02",
         "url": "https://example.com/authz/1234/1",
         "token": "DGyRejmCefe7v4NfDGDKfA"
       },
   ],
  "combinations": [
    [
      1
    ],
    [
      0
    ]
  ]
}
combinations=[[1],[0]]
identifier={"value":"test3.telecom-tariffs.co.uk","type":"dns"}
status=pending
challenges=[{"uri":"https:\/\/acme-staging.api.letsencrypt.org\/acme\/challenge\/Osw5X5KZ4TiagajE60ppyENauMOJ7ameIm5X0M_UxqM\/102864077","status":"pending","token":"RjKG3tQF8kgEhS9evvVIoVPVkzvjMem5RBqzIDzHmFI","type":"dns-01"},{"uri":"https:\/\/acme-staging.api.letsencrypt.org\/acme\/challenge\/Osw5X5KZ4TiagajE60ppyENauMOJ7ameIm5X0M_UxqM\/102864078","status":"pending","token":"rRzrYVEZ3jT3vt_Tf2i1iHOaH_RPvLllnQfjVxK7RvA","type":"http-01"}]
expires=2018-02-26T17:51:37.280339321Z
*)
        if fAcmeLastStatus <> 201 then begin
            LogEvent('Failed to get Acme challenges: ' + FHttpRest.ResponseJson.S['type'] +
                                                 ', ' + FHttpRest.ResponseJson.S['detail']) ;
            Exit;
        end;
        ArrayJson := FHttpRest.ResponseJson.O['challenges'];  // array of challenges
        for I := 0 to ArrayJson.AsArray.Length - 1 do begin
            ChallgJson := ArrayJson.AsArray[I];
            if ChallgJson.S['type'] = AuthMethod then begin
                fAcmeChallengeURL := ChallgJson.S['uri'];
                Token := ChallgJson.S['token'];
                break;
            end;
        end;
        if fAcmeChallengeURL = '' then begin
            LogEvent('Failed to find challenge: ' + AuthMethod);
            Exit;
        end;
     // different challenges have different orderids, but shorter than new-cert id
        I := LastDelimiter('/', fAcmeChallengeURL);
        if I > 10 then fSuppOrderId := Copy(fAcmeChallengeURL, I + 1, 999);
        fAcmeOrderStatus := 'pending';

    // order may already be valid if being repeated !!!!
        if fAcmeOrderStatus <> 'valid' then begin

        // save well-known file with Key Authorization content
            KeyAuth := Token + '.' + fAcmeKwkThumb;
            LogEvent ('Key Authorization: ' + KeyAuth);
            if (fSuppCertChallenge = ChallFileUNC) then begin
                WKFullName := FDirWellKnown + 'acme-challenge\' + Token;
                LogEvent ('Built domain validation file name: ' + WKFullName + ', saving token: ' + Token);
                if NOT SaveDataFile (WKFullName, KeyAuth) then Exit ;
            end
            else
            begin LogEvent ('FTP and local server validation not yet available') ;
                Exit;
            end;

            //  start challenge, so they look up our file
            if NOT AcmeGetRequest(httpPOST, fAcmeChallengeURL,
                   SO(['resource', 'challenge', 'keyAuthorization', KeyAuth])) then Exit;

            DumpJson;
          // was it accepted ???
            if fAcmeLastStatus > 202 then begin
                LogEvent('Failed to start Acme challenge: ' + FHttpRest.ResponseJson.S['type'] +
                                                     ', ' + FHttpRest.ResponseJson.S['detail']);
                Exit;
            end;
            FIssueState := IssStateChallgPend;
            fAcmeOrderStatus := 'pending';
        end;

    // wait for challenge to succeeed or fail, V1 and V2 different respooses
        if fAcmeOrderStatus <> 'valid' then begin
            LogEvent('Challenge accepted, waiting up two minutes for authorization');
            secswait := 0;
            while fAcmeOrderStatus = 'pending' do begin
                Trg := IcsGetTickCount;
                while True do begin
                    FHttpRest.MessagePump;;
                    if FHttpRest.Terminated then Exit;
                    if IcsCalcTickDiff(Trg, IcsGetTickCount) > 5000 then break ;   // five second wait
                    Sleep(0);   // thread now stops for rest of time slice
                end ;
                secswait := secswait + 5;
                if NOT AcmeGetRequest(httpGET, fAcmeChallengeURL, Nil) then exit;
                if fAcmeLastStatus > 202 then break;
                fAcmeOrderStatus := FHttpRest.ResponseJson.S['status'];
                if secswait > 120 then break;
            end;
            if fAcmeOrderStatus <> 'valid' then begin
                LogEvent('Acme did not not respond to challenge');
                Exit;
            end;
            FIssueState := IssStateChallgOK;

        // delete challenge file
            if NOT DeleteFile(WKFullName) then
                        LogEvent ('Failed to delete old file: ' + WKFullName);
 (*  Acme V1 {
  "type": "http-01",
  "status": "valid",
  "uri": "https://acme-staging.api.letsencrypt.org/acme/challenge/Osw5X5KZ4TiagajE60ppyENauMOJ7ameIm5X0M_UxqM/102864078",
  "token": "rRzrYVEZ3jT3vt_Tf2i1iHOaH_RPvLllnQfjVxK7RvA",
  "keyAuthorization": "rRzrYVEZ3jT3vt_Tf2i1iHOaH_RPvLllnQfjVxK7RvA.U76oE3D3QiQ4F9ynCWFecl6FBnth5dj-R01gHkGVpiQ",
  "validationRecord": [
    {
      "url": "http://test3.telecom-tariffs.co.uk/.well-known/acme-challenge/rRzrYVEZ3jT3vt_Tf2i1iHOaH_RPvLllnQfjVxK7RvA",
      "hostname": "test3.telecom-tariffs.co.uk",
      "port": "80",
      "addressesResolved": [
        "217.146.115.84"
      ],
      "addressUsed": "217.146.115.84"
    }
  ]
}
*)
            ArrayJson := FHttpRest.ResponseJson.O['validationRecord']; // array of records
            for I := 0 to ArrayJson.AsArray.Length - 1 do begin
                ValidJson := ArrayJson.AsArray[I];
                LogEvent('Validation URL: ' + ValidJson.S['url'] +
                                    ', IP address ' + ValidJson.S['addressUsed']);
            end;
        end;

    // now collect certificate
        LogEvent('Acme challenge passed OK, Certificate can now be collected' + IcsCRLF);
        Result := True;
    except
        on E:Exception do  begin
            LogEvent ('Fatal ACME protocol error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.AcmeV2OrderCert: Boolean;
var
    ArrayJson, ChallgJson, ValidJson, RecJson: ISuperObject;
    Token, AuthMethod, KeyAuth, NewAuthURL: string;
    ChallgStatus, WKFullName: string ;
    I, J, secswait: integer;
    Trg: longword;
begin
    Result := False;
    fAcmeOrderStatus := '';
    LogEvent (IcsCRLF + 'Starting Let''s Encrypt certificate order for: ' + fCertCommonName);
    try
        if (fAcmeKwkKid = '') then begin
            LogEvent ('Must create/open Acme account first');
            Exit;
        end;
        fAcmeChallengeURL := '';
        Token := '';
    //    Validity := 12 ;  // months
        case fSuppCertChallenge of
           ChallFileUNC: AuthMethod := 'http-01';
           ChallFileFtp: AuthMethod := 'http-01';
           ChallFileSrv: AuthMethod := 'http-01';
           ChallDNS:     AuthMethod := 'dns-01';
           ChallSNI:     AuthMethod := 'tls-sni-02';
        end;

      // where the well known directory is located
         if fSuppCertChallenge <= ChallFileSrv then begin
            if NOT TestWellKnown(FCertCommonName, FDirWellKnown) then exit;
         end;

    //  must have a valid nonce to do POST requests
        if fAcmeRespNonce = '' then begin
            if NOT AcmeGetRequest(httpHEAD, AcmeActionDirs [AcmeNewNonce2].URL, Nil) then exit;
        end;

    // new authorisation request, get a challenge

    { Acme V2 has different ordering and handles multiple host names  }
      // may have multiple host names, which will get separate challenges
        ArrayJson := SA([]);
        ArrayJson.O[''] := SO (['type', 'dns', 'value', fCertCommonName]);
        // add more ???
        if NOT AcmeGetRequest(httpPOST, AcmeActionDirs [AcmeNewOrder2].URL,
                                     SO(['identifiers', ArrayJson])) then Exit;
        DumpJson;
(*
HTTP/1.1 201 Created
{
"status": "pending",
"expires": "2018-03-26T16:57:17.955834778Z",
"identifiers": [
{
  "type": "dns",
  "value": "test3.telecom-tariffs.co.uk"
}
],
"authorizations": [
"https://acme-staging-v02.api.letsencrypt.org/acme/authz/6mDYqM5A5a7L3QnD0FxOUYI5FflSq-3MMqvfOWYPxcg"
],
"finalize": "https://acme-staging-v02.api.letsencrypt.org/acme/finalize/5763117/97378"
}
*)

        if fAcmeLastStatus <> 201 then begin
            LogEvent('Failed to get Acme authorizations: ' + FHttpRest.ResponseJson.S['type'] +
                                                 ', ' + FHttpRest.ResponseJson.S['detail']) ;
            Exit;
        end;
        ArrayJson := FHttpRest.ResponseJson.O['authorizations'];  // array of authorizations
    // pending, allow for more than one host name !!!
        NewAuthURL := '';
        for I := 0 to ArrayJson.AsArray.Length - 1 do begin
            ChallgJson := ArrayJson.AsArray[I];
            fAcmeChallengeURL := ChallgJson.AsString;
            break;
        end;
        if fAcmeChallengeURL = '' then begin
            LogEvent('Failed to find authorization: ' + AuthMethod);
            Exit;
        end;
        fAcmeOrderFinalizeUrl := FHttpRest.ResponseJson.S['finalize'];
        FAcmeOrderObjUrl := fAcmeRespLocation;  // order object
        I := LastDelimiter('/', FAcmeOrderObjUrl);
        if I > 10 then fSuppOrderId := Copy(FAcmeOrderObjUrl, I + 1, 999);
        fAcmeOrderStatus := FHttpRest.ResponseJson.S['status'];
        fAcmeOrderExpiresDT := RFC3339_StrToDate(FHttpRest.ResponseJson.S['expires']);

     // order may already be valid if being repeated !!!!
        if fAcmeOrderStatus <> 'valid' then begin

         // now start each challenge, pending one for each host
            if NOT AcmeGetRequest(httpGET, fAcmeChallengeUrl, Nil) then exit;
            if fAcmeLastStatus <> 200 then begin
                LogEvent('Failed to get Acme challenges: ' + FHttpRest.ResponseJson.S['type'] +
                                                     ', ' + FHttpRest.ResponseJson.S['detail']) ;
                Exit;
            end;

(* HTTP/1.1 200 OK
{
"identifier": {
"type": "dns",
"value": "test3.telecom-tariffs.co.uk"
},
"status": "pending",
"expires": "2018-03-26T16:57:17Z",
"challenges": [
{
  "type": "http-01",
  "status": "pending",
  "url": "https://acme-staging-v02.api.letsencrypt.org/acme/challenge/6mDYqM5A5a7L3QnD0FxOUYI5FflSq-3MMqvfOWYPxcg/110522443",
  "token": "Or9PmU6EtQUEjph3-g8ljyQWmoMiBiQy_YJtKWrF_O8"
},
{
  "type": "dns-01",
  "status": "pending",
  "url": "https://acme-staging-v02.api.letsencrypt.org/acme/challenge/6mDYqM5A5a7L3QnD0FxOUYI5FflSq-3MMqvfOWYPxcg/110522444",
  "token": "ZEejUvKMxueMUOZhVLy5mPNmWZ9-0eroNBrdtanjt9c"
}
]
}
*)
            ChallgStatus := FHttpRest.ResponseJson.S['status'];
            if ChallgStatus <> 'valid' then begin
                ArrayJson := FHttpRest.ResponseJson.O['challenges'];  // array of challenges
                for I := 0 to ArrayJson.AsArray.Length - 1 do begin
                    ChallgJson := ArrayJson.AsArray[I];
                    if ChallgJson.S['type'] = AuthMethod then begin
                        NewAuthURL  := ChallgJson.S['url'];
                        Token := ChallgJson.S['token'];
                        break;
                    end;
                end;
                if NewAuthURL  = '' then begin
                    LogEvent('Failed to find challenge: ' + AuthMethod);
                    Exit;
                end;
            end
            else
                fAcmeOrderStatus := ChallgStatus;  // valid, all done
        end;

    // order may already be valid if being repeated !!!!
        if fAcmeOrderStatus <> 'valid' then begin

        // save well-known file with Key Authorization content
            KeyAuth := Token + '.' + fAcmeKwkThumb;
            LogEvent ('Key Authorization: ' + KeyAuth);
            if (fSuppCertChallenge = ChallFileUNC) then begin
                WKFullName := FDirWellKnown + 'acme-challenge\' + Token;
                LogEvent ('Built domain validation file name: ' + WKFullName + ', saving token: ' + Token);
                if NOT SaveDataFile (WKFullName, KeyAuth) then Exit ;
            end
            else begin
                LogEvent ('FTP and local server validation not yet available') ;
                Exit;
            end;

           //  start challenge, so they look up our file
          // pending, one per host
          // no parameters, just a special URL
            if NOT AcmeGetRequest(httpPOST, NewAuthURL, SO([])) then exit;
            DumpJson;
(* HTTP/1.1 200 OK
 {
  "type": "http-01",
  "status": "pending",
  "url": "https://acme-staging-v02.api.letsencrypt.org/acme/challenge/6mDYqM5A5a7L3QnD0FxOUYI5FflSq-3MMqvfOWYPxcg/110522443",
  "token": "Or9PmU6EtQUEjph3-g8ljyQWmoMiBiQy_YJtKWrF_O8"
}
*)
           // was it accepted ???
            if fAcmeLastStatus <> 200 then begin
                LogEvent('Failed to start Acme challenge: ' + FHttpRest.ResponseJson.S['type'] +
                                                     ', ' + FHttpRest.ResponseJson.S['detail']) ;
                Exit;
            end;
            fAcmeOrderStatus := FHttpRest.ResponseJson.S['status'];
            FIssueState := IssStateChallgPend;
        end;

    // wait for challenge to succeeed or fail, V1 and V2 different respooses
        if fAcmeOrderStatus <> 'valid' then begin
            LogEvent('Challenge accepted, waiting up two minutes for authorization');
            secswait := 0;
            while fAcmeOrderStatus = 'pending' do begin
                Trg := IcsGetTickCount;
                while True do
                begin
                    FHttpRest.MessagePump;;
                    if FHttpRest.Terminated then Exit;
                    if IcsCalcTickDiff(Trg, IcsGetTickCount) > 5000 then break ;   // five second wait
                    Sleep(0);   // thread now stops for rest of time slice
                end ;
                secswait := secswait + 5;
                if NOT AcmeGetRequest(httpGET, fAcmeChallengeURL, Nil) then exit;
                if fAcmeLastStatus > 202 then break;
                fAcmeOrderStatus := FHttpRest.ResponseJson.S['status'];
                if secswait > 120 then break;
            end;
            if fAcmeOrderStatus <> 'valid' then begin
                LogEvent('Acme did not not respond to challenge');
                Exit;
            end;
            FIssueState := IssStateChallgOK;

        // delete challenge file
            if NOT DeleteFile(WKFullName) then
                        LogEvent ('Failed to delete old file: ' + WKFullName) ;
 (*
 Acme V2
 HTTP/1.1 200 OK
{
  "identifier": {
    "type": "dns",
    "value": "test3.telecom-tariffs.co.uk"
  },
  "status": "valid",
  "expires": "2018-04-18T16:57:19Z",
  "challenges": [
    {
      "type": "http-01",
      "status": "valid",
      "url": "https://acme-staging-v02.api.letsencrypt.org/acme/challenge/6mDYqM5A5a7L3QnD0FxOUYI5FflSq-3MMqvfOWYPxcg/110522443",
      "token": "Or9PmU6EtQUEjph3-g8ljyQWmoMiBiQy_YJtKWrF_O8",
      "validationRecord": [
        {
          "url": "http://test3.telecom-tariffs.co.uk/.well-known/acme-challenge/Or9PmU6EtQUEjph3-g8ljyQWmoMiBiQy_YJtKWrF_O8",
          "hostname": "test3.telecom-tariffs.co.uk",
          "port": "80",
          "addressesResolved": [
            "217.146.115.84"
          ],
          "addressUsed": "217.146.115.84"
        }
      ]
    },
    {
      "type": "dns-01",
      "status": "pending",
      "url": "https://acme-staging-v02.api.letsencrypt.org/acme/challenge/6mDYqM5A5a7L3QnD0FxOUYI5FflSq-3MMqvfOWYPxcg/110522444",
      "token": "ZEejUvKMxueMUOZhVLy5mPNmWZ9-0eroNBrdtanjt9c"
    }
  ]
}
*)
         // Acme v2, may be more than one pending challenge !!!
            ArrayJson := FHttpRest.ResponseJson.O['challenges']; // array of records
            for I := 0 to ArrayJson.AsArray.Length - 1 do begin
                ChallgJson := ArrayJson.AsArray[I];
                if ChallgJson.S['status'] = 'valid' then begin
                    ValidJson := ChallgJson.O['validationRecord'];
                    for J := 0 to ValidJson.AsArray.Length - 1 do begin
                        RecJson := ValidJson.AsArray[J];
                        LogEvent('Validation URL: ' + RecJson.S['url'] +
                                    ', IP address ' + RecJson.S['[addressUsed]']);
              //    ', Time: ' + DateTimeToStr(RFC3339_StrToDate(RecJson.S['validated'])));  // not in Acme v2
             //       break;
                    end;
                end;
            end;
        end;

    // now collect certificate
        LogEvent('Acme challenge passed OK, Certificate can now be collected' + IcsCRLF);
        Result := True;
    except
        on E:Exception do begin
            LogEvent ('Fatal ACME protocol error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.AcmeV1GetCert: Boolean;
var
    CSREn, AuthStatus, tempcert, interurl: string ;
    I: Integer;
begin
    Result := False;
    LogEvent (IcsCRLF + 'Collecting Let''s Encrypt SSL certificate for: ' + fCertCommonName);
    try
    //  must have a valid nonce to do POST requests
        if fAcmeRespNonce = '' then begin
        // acme v1 does not support NewNonce, but we should have it from directory command
            if NOT AcmeGetRequest(httpHEAD, AcmeActionDirs [AcmeNewAuthz1].URL, Nil) then exit;
            LogEvent ('Initial Nonce: ' + fAcmeRespNonce);
        end;
        fAcmeCertURL := '';
        fAcmeCertLines := '';
        fNewInterLines := '';
        fAcmeOrderStatus := 'pending';
        AuthStatus := '';

      // see if challenge completed
        if NOT AcmeGetRequest(httpGET, fAcmeChallengeURL, Nil) then exit;
        if fAcmeLastStatus > 202 then begin
            LogEvent('Acme did not not respond to challenge');
            Exit;
        end;
        AuthStatus := FHttpRest.ResponseJson.S['status'];
        if AuthStatus = 'pending' then begin
            LogEvent('Acme has not yet responded to challenge');
            // any more info ??
            Exit;
        end ;

    // work file names, in account directory, with orderid (no work names)
        fSslCert.ClearAll;
        if NOT SetPartFNames (true) then Exit ;
        SetFullFileNames (FPartFNameWork) ;

    // if order still pending, finalize it
        if fAcmeOrderStatus = 'pending' then begin

          // create private key and certificate service request
            if NOT CreateKeyandReq then exit ;

          // Acme needs DER request UrlBase64 encoded no headers, not PEM base64
            CSREn := IcsBase64UrlEncode(String(fSslCert.SaveToDERText));

         // order new certificate for our CSR
            if NOT AcmeGetRequest(httpPOST, AcmeActionDirs [AcmeNewCert1].URL,
                           SO([ 'resource', 'new-cert', 'csr', CSREn]) ) then Exit;
            if fAcmeLastStatus > 202 then begin
                LogEvent('Failed to collect SSL certificate: ' + FHttpRest.ResponseJson.S['type'] +
                                                     ', ' + FHttpRest.ResponseJson.S['detail']) ;
                Exit;
            end;
{ Content-Type: application/pkix-cert
Content-Length: 1543
Boulder-Requester: 5628851
Link: <https://acme-staging.api.letsencrypt.org/acme/issuer-cert>;rel="up"
Location: https://acme-staging.api.letsencrypt.org/acme/cert/faa5856362d8a1b9b79f38741b9a90921d65 }

         // certificate should have come back with POST, if not download it
            if fAcmeCertLines = '' then fAcmeCertURL := fAcmeRespLocation;

         // but we need intermediate anyway
            tempcert := fAcmeCertLines;
            interurl := fAcmeRespLink;
            I := Pos ('https:', interurl);
            if I > 0 then begin
                interurl := Copy(interurl, I, 999);
                I := Pos ('>', interurl);
                interurl := Copy(interurl, 1, I - 1);
                if (NOT AcmeGetRequest(httpGET, interurl, Nil)) then exit;
                if fAcmeLastStatus > 202 then begin
                    LogEvent('Failed to download intermediate SSL certificate from: ' + interurl);
                    fNewInterLines := LetsEncryptCrossInterLines;
                end
                else begin
                    fNewInterLines := fAcmeCertLines;
                    fAcmeCertLines := tempcert;
                end;
            end;
        end;

    // see if downloading certificate
        if fAcmeCertURL <> '' then begin
            LogEvent ('Certificate download URL: ' + fAcmeCertURL);
            if (NOT AcmeGetRequest(httpGET, fAcmeCertURL, Nil)) then exit;
            if fAcmeLastStatus > 202 then begin
                LogEvent('Failed to download SSL certificate from: ' + fAcmeCertURL);
                Exit;
            end;
            I := LastDelimiter('/', fAcmeCertURL);
            if I > 0 then begin
                fAcmeCertSerial := Copy (fAcmeCertURL, I + 1, 999);
                LogEvent ('Certificate serial: ' + fAcmeCertSerial);
            end;
        end;

    // do we need to load private key
        if NOT fSslCert.IsPKeyLoaded then begin
            try
                LogEvent('Loading old private key from: ' + fFilePrvKey);
                fSslCert.PrivateKeyLoadFromPemFile(fFilePrvKey);
            except
                on E:Exception do begin
                    LogEvent ('Failed to load old private key: ' + E.Message);
                end;
            end;
        end;
        fNewCertLines := fAcmeCertLines;
        LogEvent ('Certificate(s):' + IcsCRLF + fAcmeCertLines);
        SaveCertificateFiles(fCertCommonName);
        FIssueState := IssStateCollect;
        Result := True;
    except
        on E:Exception do begin
            LogEvent ('Fatal ACME protocol error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslX509Certs.AcmeV2GetCert: Boolean;
var
    CSREn, AuthStatus: string ;
    I: Integer;
begin
    Result := False;
    LogEvent (IcsCRLF + 'Collecing Let''s Encrypt SSL certificate for: ' + fCertCommonName);
    if (fAcmeOrderFinalizeURL = '') then begin
        LogEvent('Need Acme Fianalize URL from order first');
        Exit;
    end;
    try
    //  must have a valid nonce to do POST requests
        if fAcmeRespNonce = '' then begin
            if NOT AcmeGetRequest(httpHEAD, AcmeActionDirs [AcmeNewNonce2].URL, Nil) then exit;
            LogEvent ('Initial Nonce: ' + fAcmeRespNonce);
        end;
        fAcmeCertURL := '';
        fAcmeCertLines := '';
        fNewInterLines := '';
        fAcmeOrderStatus := 'pending';
        AuthStatus := '';

      // see if challenge completed
        if NOT AcmeGetRequest(httpGET, fAcmeChallengeURL, Nil) then exit;
        if fAcmeLastStatus > 202 then begin
            LogEvent('Acme did not not respond to challenge');
            Exit;
        end;
        AuthStatus := FHttpRest.ResponseJson.S['status'];
        if AuthStatus = 'pending' then begin
            LogEvent('Acme has not yet responded to challenge');
            // any more info ??
            Exit;
        end ;

     // V2 get order object, it may be completed already
        if NOT AcmeGetRequest(httpGET, FAcmeOrderObjUrl, Nil) then exit;
        if fAcmeLastStatus > 202 then begin
            LogEvent('Acme could not find order: ' + FAcmeOrderObjUrl);
            Exit;
        end;
        fAcmeOrderStatus := FHttpRest.ResponseJson.S['status'];
        if fAcmeOrderStatus <> 'pending' then begin
            if fAcmeOrderStatus = 'valid' then begin
                fAcmeOrderExpiresDT := RFC3339_StrToDate(FHttpRest.ResponseJson.S['expires']);
                fAcmeCertURL := FHttpRest.ResponseJson.S['certificate'];
            end
            else begin
                LogEvent('Acme order status not compeleted: ' + fAcmeOrderStatus);
            // any more info ??
                Exit;
            end;
         end ;

    // work file names, in account directory, with orderid (no work names)
        fSslCert.ClearAll;
        if NOT SetPartFNames (true) then Exit ;
        SetFullFileNames (FPartFNameWork) ;

    // if order still pending, finalize it
        if fAcmeOrderStatus = 'pending' then begin

          // create private key and certificate service request
            if NOT CreateKeyandReq then exit ;

          // Acme needs DER request UrlBase64 encoded no headers, not PEM base64
            CSREn := IcsBase64UrlEncode(String(fSslCert.SaveToDERText));

         // order certificate
            if NOT AcmeGetRequest(httpPOST, fAcmeOrderFinalizeURL, SO([ 'csr', CSREn]) ) then Exit;
            if fAcmeLastStatus > 200 then begin
                LogEvent('Failed to collect SSL certificate: ' + FHttpRest.ResponseJson.S['type'] +
                                                     ', ' + FHttpRest.ResponseJson.S['detail']);
                Exit;
            end;
(*  {
  "status": "valid",
  "expires": "2018-03-26T16:57:17Z",
  "identifiers": [
    {
      "type": "dns",
      "value": "test3.telecom-tariffs.co.uk"
    }
  ],
  "authorizations": [
    "https://acme-staging-v02.api.letsencrypt.org/acme/authz/6mDYqM5A5a7L3QnD0FxOUYI5FflSq-3MMqvfOWYPxcg"
  ],
  "finalize": "https://acme-staging-v02.api.letsencrypt.org/acme/finalize/5763117/97378",
  "certificate": "https://acme-staging-v02.api.letsencrypt.org/acme/cert/fac3d324243e1a7c73126018c851287377b5"
}
*)
            fAcmeOrderStatus := FHttpRest.ResponseJson.S['status'];
            fAcmeOrderExpiresDT := RFC3339_StrToDate(FHttpRest.ResponseJson.S['expires']);
            fAcmeCertURL := FHttpRest.ResponseJson.S['certificate'];
            if fAcmeOrderStatus <> 'valid' then begin
                LogEvent('Failed to collect SSL certificate, Order Status: ' + fAcmeOrderStatus);
                exit;
            end;
            if (fAcmeCertURL = '') then begin
                LogEvent('Failed to collect SSL certificate, no certificate URL');
                exit;
            end;
        end;

    // see if downloading certificate
        if fAcmeCertURL <> '' then begin
            LogEvent ('Certificate download URL: ' + fAcmeCertURL);
            if (NOT AcmeGetRequest(httpGET, fAcmeCertURL, Nil)) then exit;
            if fAcmeLastStatus > 202 then begin
                LogEvent('Failed to download SSL certificate from: ' + fAcmeCertURL);
                Exit;
            end;
            I := LastDelimiter('/', fAcmeCertURL);
            if I > 0 then begin
                fAcmeCertSerial := Copy (fAcmeCertURL, I + 1, 999);
                LogEvent ('Certificate serial: ' + fAcmeCertSerial);
            end;
        end;

    // do we need to load private key
        if NOT fSslCert.IsPKeyLoaded then begin
            try
                LogEvent('Loading old private key from: ' + fFilePrvKey);
                fSslCert.PrivateKeyLoadFromPemFile(fFilePrvKey);
            except
                on E:Exception do
                begin
                    LogEvent ('Failed to load old private key: ' + E.Message);
                end;
            end;
        end;
        fNewCertLines := fAcmeCertLines;
        LogEvent ('Certificate(s):' + IcsCRLF + fAcmeCertLines);
        SaveCertificateFiles(fCertCommonName);
        FIssueState := IssStateCollect;
        Result := True;
    except
        on E:Exception do begin
            LogEvent ('Fatal ACME protocol error: ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$ENDIF USE_SSL}

end.
