program OverbyteIcsSysLogServerDemo;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSysLogServerDemo1 in 'OverbyteIcsSysLogServerDemo1.pas' {SysLogServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSysLogServerForm, SysLogServerForm);
  Application.Run;
end.
