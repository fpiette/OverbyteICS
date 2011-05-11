program OverbyteIcsMsVerify;

{$R 'OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R 'OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsMsVerify1 in 'OverbyteIcsMsVerify1.pas' {MsVerifyForm},
  OverbyteIcsMsSslUtils in 'OverbyteIcsMsSslUtils.pas',
  WinCrypt in 'WinCrypt.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMsVerifyForm, MsVerifyForm);
  Application.Run;
end.
