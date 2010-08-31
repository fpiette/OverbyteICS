program OverbyteIcsHttpPost;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsHttpPost1 in 'OverbyteIcsHttpPost1.pas' {HttpPostForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THttpPostForm, HttpPostForm);
  Application.Run;
end.
