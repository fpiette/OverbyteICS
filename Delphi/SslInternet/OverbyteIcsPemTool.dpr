program OverbyteIcsPemTool;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsPemTool1 in 'OverbyteIcsPemTool1.pas' {frmPemTool1},
  OverbyteIcsPemTool2 in 'OverbyteIcsPemTool2.pas' {frmPemTool2},
  OverbyteIcsPemTool3 in 'OverbyteIcsPemTool3.pas' {frmPemTool3};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'PEM Certificate Tool';
  Application.CreateForm(TfrmPemTool1, frmPemTool1);
  Application.CreateForm(TfrmPemTool3, frmPemTool3);
  Application.CreateForm(TfrmPemTool2, frmPemTool2);
  Application.Run;
end.
