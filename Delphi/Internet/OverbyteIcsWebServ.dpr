program OverbyteIcsWebServ;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsWebServ1 in 'OverbyteIcsWebServ1.pas' {WebServForm};

{$R *.RES}

begin
  Application.CreateForm(TWebServForm, WebServForm);
  Application.Run;
end.
