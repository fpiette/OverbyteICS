program OverbyteIcsSrvDemo;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSrvDemo1 in 'OverbyteIcsSrvDemo1.pas' {SrvForm},
  OverbyteIcsSrvDemo2 in 'OverbyteIcsSrvDemo2.pas' {CliForm};

{$R *.RES}

begin
  Application.CreateForm(TSrvForm, SrvForm);
  Application.Run;
end.
