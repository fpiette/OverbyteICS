program OverbyteIcsThreadTimerDemo;
{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  FastAppProcessMessagePatch in 'FastAppProcessMessagePatch.pas',
  Forms,
  OverbyteIcsThreadTimerDemo1 in 'OverbyteIcsThreadTimerDemo1.pas' {IcsTimerDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TIcsTimerDemoForm, IcsTimerDemoForm);
  Application.Run;
end.
