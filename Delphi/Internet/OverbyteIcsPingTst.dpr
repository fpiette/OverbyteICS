program OverbyteIcsPingTst;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsPingTst1 in 'OverbyteIcsPingTst1.pas' {PingTstForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPingTstForm, PingTstForm);
  Application.Run;
end.
