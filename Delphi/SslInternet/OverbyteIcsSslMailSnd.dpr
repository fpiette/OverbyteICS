program OverbyteIcsSslMailSnd;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSslMailSnd1 in 'OverbyteIcsSslMailSnd1.pas' {SslSmtpTestForm};

{$R *.RES}

begin
  Application.CreateForm(TSslSmtpTestForm, SslSmtpTestForm);
  Application.Run;
end.
