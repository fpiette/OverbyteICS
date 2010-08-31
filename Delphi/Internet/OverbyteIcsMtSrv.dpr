program OverbyteIcsMtSrv;

{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}
{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsMtSrv1 in 'OverbyteIcsMtSrv1.pas' {ServerForm},
  OverbyteIcsMtSrv2 in 'OverbyteIcsMtSrv2.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;
end.
