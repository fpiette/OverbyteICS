program OverbyteIcsTnDemo;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsTnDemo1 in 'OverbyteIcsTnDemo1.pas' {TnDemoForm};

{$R *.RES}

begin
  Application.CreateForm(TTnDemoForm, TnDemoForm);
  Application.Run;
end.
