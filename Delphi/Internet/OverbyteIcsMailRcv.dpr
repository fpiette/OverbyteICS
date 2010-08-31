program OverbyteIcsMailRcv;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsMailRcv1 in 'OverbyteIcsMailRcv1.pas' {POP3ExcercizerForm},
  OverbyteIcsMailRcv2 in 'OverbyteIcsMailRcv2.pas' {MessageForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPOP3ExcercizerForm, POP3ExcercizerForm);
  Application.CreateForm(TMessageForm, MessageForm);
  Application.Run;
end.
