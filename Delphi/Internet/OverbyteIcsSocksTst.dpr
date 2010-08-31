program OverbyteIcsSocksTst;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSocksTst1 in 'OverbyteIcsSocksTst1.pas' {SocksTestForm};

{$R *.RES}

begin
  Application.CreateForm(TSocksTestForm, SocksTestForm);
  Application.Run;
end.
