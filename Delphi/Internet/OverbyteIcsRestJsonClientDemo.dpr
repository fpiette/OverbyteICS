program OverbyteIcsRestJsonClientDemo;

uses
  Forms,
  OverbyteIcsRestJsonClientDemo1 in 'OverbyteIcsRestJsonClientDemo1.pas' {GoogleSearchJsonClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TGoogleSearchJsonClientForm, GoogleSearchJsonClientForm);
  Application.CreateForm(TGoogleSearchJsonClientForm, GoogleSearchJsonClientForm);
  Application.Run;
end.
