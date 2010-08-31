program OverbyteIcsBasFtp;

{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}
{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsBasFtp1 in 'OverbyteIcsBasFtp1.pas' {BasicFtpClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBasicFtpClientForm, BasicFtpClientForm);
  Application.Run;
end.
