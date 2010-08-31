program OverbyteIcsSysLogClientDemo;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSysLogClientDemo1 in 'OverbyteIcsSysLogClientDemo1.pas' {SysLogClientForm},
  OverbyteIcsSysLogClient in '..\Vc32\OverbyteIcsSysLogClient.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSysLogClientForm, SysLogClientForm);
  Application.Run;
end.
