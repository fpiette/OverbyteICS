program OverbyteIcsSslSniSrv;

{$R '..\..\OverbyteIcsCommonVersion.res'  '..\..\OverbyteIcsCommonVersion.rc'}
{$R '..\..\OverbyteIcsXpManifest.res' '..\..\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsSslSniSrv1 in 'OverbyteIcsSslSniSrv1.pas' {MainForm},
  OverbyteIcsLibeayEx in '..\..\..\Source\Extras\OverbyteIcsLibeayEx.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
