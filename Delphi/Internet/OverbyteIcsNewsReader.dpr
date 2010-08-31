program OverbyteIcsNewsReader;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsNewsReader1 in 'OverbyteIcsNewsReader1.pas' {NNTPForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TNNTPForm, NNTPForm);
  Application.Run;
end.
