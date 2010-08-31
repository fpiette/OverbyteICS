program OverbyteIcsSslWebServ;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSslWebServ1 in 'OverbyteIcsSslWebServ1.pas' {SslWebServForm};

{$R *.RES}

begin
  Application.CreateForm(TSslWebServForm, SslWebServForm);
  Application.Run;
end.
