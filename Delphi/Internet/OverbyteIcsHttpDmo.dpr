program OverbyteIcsHttpDmo;

{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}
{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsHttpDmo1 in 'OverbyteIcsHttpDmo1.pas' {HttpToMemoForm};

{$R *.RES}

begin
  Application.CreateForm(THttpToMemoForm, HttpToMemoForm);
  Application.Run;
end.
