program OverbyteIcsFtpServ;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsFtpServ1 in 'OverbyteIcsFtpServ1.pas' {FtpServerForm};

{$R *.RES}

begin
  Application.CreateForm(TFtpServerForm, FtpServerForm);
  Application.Run;
end.
