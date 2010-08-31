program OverbyteIcsHttpMultipartDownload;

{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}
{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsHttpMultipartDownload1 in 'OverbyteIcsHttpMultipartDownload1.pas' {MultipartHttpDownloadForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMultipartHttpDownloadForm, MultipartHttpDownloadForm);
  Application.Run;
end.
