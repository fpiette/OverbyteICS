program OverbyteIcsRecv;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsRecv1 in 'OverbyteIcsRecv1.pas' {RecvForm};

{$R *.RES}

begin
  Application.CreateForm(TRecvForm, RecvForm);
  Application.Run;
end.
