program OverbyteIcsFinger;

{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}
{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsFinger1 in 'OverbyteIcsFinger1.pas' {FingerDemoForm};

{$R *.RES}

begin
  Application.CreateForm(TFingerDemoForm, FingerDemoForm);
  Application.Run;
end.
