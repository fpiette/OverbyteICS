program OverbyteIcsThrdSrvV2;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsThrdSrvV2_1 in 'OverbyteIcsThrdSrvV2_1.pas' {TcpSrvForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTcpSrvForm, TcpSrvForm);
  Application.Run;
end.
