program OverbyteIcsMimeDemo;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsMimeDemo1 in 'OverbyteIcsMimeDemo1.pas' {MimeDecodeForm};

{$R *.RES}

begin
  Application.CreateForm(TMimeDecodeForm, MimeDecodeForm);
  Application.Run;
end.
