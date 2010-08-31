program OverbyteIcsHttpAsp;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsHttpAsp1 in 'OverbyteIcsHttpAsp1.pas' {HttpTestForm};

{$R *.RES}

begin
  Application.CreateForm(THttpTestForm, HttpTestForm);
  Application.Run;
end.
