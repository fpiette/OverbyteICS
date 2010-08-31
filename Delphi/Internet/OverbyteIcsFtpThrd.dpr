program OverbyteIcsFtpThrd;

{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}
{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}

uses
  Forms,
  OverbyteIcsFtpThrd1 in 'OverbyteIcsFtpThrd1.pas' {ThrdFtpForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TThrdFtpForm, ThrdFtpForm);
  Application.Run;
end.
