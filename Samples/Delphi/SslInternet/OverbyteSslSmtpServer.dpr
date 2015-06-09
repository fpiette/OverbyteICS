program OverbyteSslSmtpServer;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteSSlSmtpServ1 in 'OverbyteSslSmtpServ1.pas' {SmtpSslSrvForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Test SSL SMTP Server';
  Application.CreateForm(TSmtpSslSrvForm, SmtpSslSrvForm);
  Application.Run;
end.
