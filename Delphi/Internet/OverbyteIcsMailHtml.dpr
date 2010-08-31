program OverbyteIcsMailHtml;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsMailHtm1 in 'OverbyteIcsMailHtm1.pas' {HtmlMailForm};

{$R *.res}

begin
  {$IFNDEF VER80}Application.Initialize;{$ENDIF}
  Application.CreateForm(THtmlMailForm, HtmlMailForm);
  Application.Run;
end.

