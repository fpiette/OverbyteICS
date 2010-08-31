program OverbyteIcsSslFtpServ;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSslFtpServ1 in 'OverbyteIcsSslFtpServ1.pas' {SslFtpServerForm};

{$R *.RES}

begin
  Application.CreateForm(TSslFtpServerForm, SslFtpServerForm);
  Application.Run;
end.
