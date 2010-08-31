program OverbyteIcsFtpMulti;

{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}
{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsFtpMulti1 in 'OverbyteIcsFtpMulti1.pas' {FtpMultiForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFtpMultiForm, FtpMultiForm);
  Application.Run;
end.
