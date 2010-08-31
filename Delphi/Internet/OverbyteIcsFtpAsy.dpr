program OverbyteIcsFtpAsy;

{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}
{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsFtpAsy1 in 'OverbyteIcsFtpAsy1.pas' {FtpAsyncForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFtpAsyncForm, FtpAsyncForm);
  Application.Run;
end.
