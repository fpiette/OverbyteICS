program OverbyteIcsSslNewsRdr;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSslNewsRdr1 in 'OverbyteIcsSslNewsRdr1.pas' {NNTPForm};

{$R *.RES}

begin
  Application.CreateForm(TNNTPForm, NNTPForm);
  Application.Run;
end.
