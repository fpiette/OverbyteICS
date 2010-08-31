program OverbyteIcsFtpMultipartDownload;

{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}
{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsFtpMultipartDownload1 in 'OverbyteIcsFtpMultipartDownload1.pas' {MultipartFtpDownloadForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMultipartFtpDownloadForm, MultipartFtpDownloadForm);
  Application.Run;
end.
