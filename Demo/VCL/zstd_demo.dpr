program zstd_demo;

uses
  Vcl.Forms,
  main in 'main.pas' {Form1},
  zstd in '..\Src\zstd.pas',
  zstdstreams in '..\Src\zstdstreams.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
