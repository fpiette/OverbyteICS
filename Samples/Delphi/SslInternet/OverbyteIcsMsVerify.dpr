program OverbyteIcsMsVerify;

{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}
{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}

uses
  Forms, 
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsMsVerify1 in 'OverbyteIcsMsVerify1.pas' {MsVerifyForm},
  OverbyteIcsMsSslUtils in 'OverbyteIcsMsSslUtils.pas',
  WinCrypt in 'WinCrypt.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMsVerifyForm, MsVerifyForm);
  Application.Run;
end.
