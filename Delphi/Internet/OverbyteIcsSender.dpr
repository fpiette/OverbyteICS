program OverbyteIcsSender;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSender1 in 'OverbyteIcsSender1.pas' {SenderForm};

{$R *.RES}

begin
  Application.CreateForm(TSenderForm, SenderForm);
  Application.Run;
end.
