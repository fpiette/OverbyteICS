program OverbyteIcsHttpRestTst;

uses
  Forms,
  OverbyteIcsHttpRestTst1 in 'OverbyteIcsHttpRestTst1.pas' {HttpRestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THttpRestForm, HttpRestForm);
  Application.Run;
end.
