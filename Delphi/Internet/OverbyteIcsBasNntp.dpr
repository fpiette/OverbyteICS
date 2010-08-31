program OverbyteIcsBasNntp;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsBasNntp1 in 'OverbyteIcsBasNntp1.pas' {BasicNntpForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBasicNntpForm, BasicNntpForm);
  Application.Run;
end.
