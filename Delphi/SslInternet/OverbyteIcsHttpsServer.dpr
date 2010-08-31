program OverbyteIcsHttpsServer;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsHttpsServer1 in 'OverbyteIcsHttpsServer1.pas' {HttpsSrvForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(THttpsSrvForm, HttpsSrvForm);
  Application.Run;
end.
