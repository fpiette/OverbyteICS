program OverbyteIcsThrdSrvV3;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsThrdSrvV3_1 in 'OverbyteIcsThrdSrvV3_1.pas' {ThrdSrvForm};

{$R *.RES}

begin
  Application.CreateForm(TThrdSrvForm, ThrdSrvForm);
  Application.Run;
end.
