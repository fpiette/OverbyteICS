program OverbyteIcsNsLookup;

{$R '..\Vc32\OverbyteIcsXpManifest.res' '..\Vc32\OverbyteIcsXpManifest.rc'}
{$R '..\Vc32\OverbyteIcsCommonVersion.res' '..\Vc32\OverbyteIcsCommonVersion.rc'}

uses
  Forms,
  OverbyteIcsNsLookup1 in 'OverbyteIcsNsLookup1.pas' {NsLookupForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TNsLookupForm, NsLookupForm);
  Application.Run;
end.
