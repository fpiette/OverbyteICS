program OverbyteIcsCliDemo;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsCliDemo1 in 'OverbyteIcsCliDemo1.pas' {ClientForm};

{$R *.RES}

begin
  Application.CreateForm(TClientForm, ClientForm);
  Application.Run;
end.
