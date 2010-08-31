program OverByteIcsDnsResolver;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsDnsResolver1 in 'OverbyteIcsDnsResolver1.pas' {DnsResolverForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDnsResolverForm, DnsResolverForm);
  Application.Run;
end.
