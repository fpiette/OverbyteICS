program OverbyteIcsHttpGet;

{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}
{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsHttpGet1 in 'OverbyteIcsHttpGet1.pas' {HttpGetForm};

{$R *.RES}

begin
  Application.CreateForm(THttpGetForm, HttpGetForm);
  Application.Run;
end.
