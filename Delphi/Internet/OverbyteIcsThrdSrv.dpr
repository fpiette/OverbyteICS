program OverbyteIcsThrdSrv;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsThrdSrv1 in 'OverbyteIcsThrdSrv1.pas' {TcpSrvForm};

{$R *.RES}

begin
  Application.CreateForm(TTcpSrvForm, TcpSrvForm);
  Application.Run;
end.
