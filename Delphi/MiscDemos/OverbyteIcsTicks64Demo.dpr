program OverbyteIcsTicks64Demo;

uses
  Forms,
  OverbyteIcsTicks64Demo1 in 'OverbyteIcsTicks64Demo1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
