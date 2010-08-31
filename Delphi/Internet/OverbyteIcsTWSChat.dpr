program OverbyteIcsTWSChat;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsTWSChat1 in 'OverbyteIcsTWSChat1.pas' {TWSChatForm};

{$R *.RES}

begin
  Application.CreateForm(TTWSChatForm, TWSChatForm);
  Application.Run;
end.
