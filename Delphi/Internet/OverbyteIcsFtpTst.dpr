program OverbyteIcsFtpTst;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsFtpTst1 in 'OverbyteIcsFtpTst1.pas' {FtpReceiveForm},
  OverByteIcsFtpTst2 in 'OverByteIcsFtpTst2.pas' {DirectoryForm};

{$R *.RES}
 
begin
  Application.CreateForm(TFtpReceiveForm, FtpReceiveForm);
  Application.CreateForm(TDirectoryForm, DirectoryForm);
  Application.Run;
end.
