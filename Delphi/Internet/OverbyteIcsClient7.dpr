program OverbyteIcsClient7;

{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}
{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsCli7 in 'OverbyteIcsCli7.pas' {Cli7Form};

{$R *.RES}

begin
  Application.CreateForm(TCli7Form, Cli7Form);
  Application.Run;
end.
