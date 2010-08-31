program OverbyteIcsSslMailRcv;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSslMailRcv1 in 'OverbyteIcsSslMailRcv1.pas' {POP3ExcercizerForm},
  OverbyteIcsSslMailRcv2 in 'OverbyteIcsSslMailRcv2.pas' {MessageForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPOP3ExcercizerForm, POP3ExcercizerForm);
  Application.CreateForm(TMessageForm, MessageForm);
  Application.Run;
end.

