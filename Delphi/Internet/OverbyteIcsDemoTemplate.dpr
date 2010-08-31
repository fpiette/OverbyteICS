program OverbyteIcsDemoTemplate;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsDemoTemplate1 in 'OverbyteIcsDemoTemplate1.pas' {TemplateForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTemplateForm, TemplateForm);
  Application.Run;
end.
