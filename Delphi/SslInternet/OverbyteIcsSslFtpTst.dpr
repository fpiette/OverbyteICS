program OverbyteIcsSslFtpTst;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsSslFtpTst1 in 'OverbyteIcsSslFtpTst1.pas' {FtpReceiveForm},
  OverbyteIcsSslFtpTst2 in 'OverbyteIcsSslFtpTst2.pas' {DirectoryForm};

{$R *.RES}
 
begin
  Application.CreateForm(TFtpReceiveForm, FtpReceiveForm);
  Application.CreateForm(TDirectoryForm, DirectoryForm);
  Application.Run;
end.
