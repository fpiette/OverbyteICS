program OverbyteIcsMailSnd;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsMailSnd1 in 'OverbyteIcsMailSnd1.pas' {SmtpTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSmtpTestForm, SmtpTestForm);
  Application.Run;
end.
