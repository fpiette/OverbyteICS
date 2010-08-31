program OverbyteIcsTnSrv;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsTnSrv1 in 'OverbyteIcsTnSrv1.pas' {ServerForm},
  OverbyteIcsTnSrv2 in 'OverbyteIcsTnSrv2.pas' {ClientForm};

{$R *.RES}

begin
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;
end.
