program OverbyteIcsDynCli;

{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}
{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsDynCli1 in 'OverbyteIcsDynCli1.pas' {DynCliForm};

{$R *.RES}

begin
  Application.CreateForm(TDynCliForm, DynCliForm);
  Application.Run;
end.
