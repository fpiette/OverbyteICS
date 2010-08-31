program OverbyteIcsHttpChk;

{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}
{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsHttpChk1 in 'OverbyteIcsHttpChk1.pas' {CheckUrlForm};

{$R *.RES}

begin
  Application.CreateForm(TCheckUrlForm, CheckUrlForm);
  Application.Run;
end.
