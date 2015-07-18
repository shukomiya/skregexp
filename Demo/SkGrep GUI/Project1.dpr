program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  SkTextCoderU in '..\..\Lib\SkLibU\SkTextCoderU.pas',
  Unit2 in 'Unit2.pas' {Form2},
  SkRegExpW in '..\..\SkRegExpW.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
