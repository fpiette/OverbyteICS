program OverbyteIcsHttpsTst;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsHttpsTst1 in 'OverbyteIcsHttpsTst1.pas' {HttpsTstForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THttpsTstForm, HttpsTstForm);
  Application.Run;
end.
