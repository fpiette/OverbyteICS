program OverbyteIcsDllTst;

{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}
{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsDllTst1 in 'OverbyteIcsDllTst1.pas' {DllTestForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDllTestForm, DllTestForm);
  Application.Run;
end.
