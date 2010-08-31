program OverbyteIcsBinCliDemo;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsBinCliDemo1 in 'OverbyteIcsBinCliDemo1.pas' {BinClientForm};

{$R *.RES}

begin
  Application.CreateForm(TBinClientForm, BinClientForm);
  Application.Run;
end.
