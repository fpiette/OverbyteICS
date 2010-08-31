program OverbyteIcsHttpAsy;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsHttpAsy1 in 'OverbyteIcsHttpAsy1.pas' {HttpAsyForm};

{$R *.RES}

begin
  Application.CreateForm(THttpAsyForm, HttpAsyForm);
  Application.Run;
end.
