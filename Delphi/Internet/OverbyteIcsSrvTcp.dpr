program OverbyteIcsSrvTcp;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSrvTcp1 in 'OverbyteIcsSrvTcp1.pas' {GetGroupsForm},
  OverbyteIcsTcpCmd in 'OverbyteIcsTcpCmd.pas';

{$R *.RES}

begin
{$IFNDEF VER80}
  Application.Initialize;
{$ENDIF}
  Application.CreateForm(TGetGroupsForm, GetGroupsForm);
  Application.Run;
end.
