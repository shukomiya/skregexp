unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SkHisComboBox;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    procedure Button1Click(Sender: TObject);
  private
    { Private éŒ¾ }
  public
    { Public éŒ¾ }
    procedure Log(const S: string);
  end;

var
  Form1: TForm1;

implementation

uses SkRegExpW, PerlRegEx;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  sr: TSkRegExp;
  pr: TPerlRegEx;
  I, Times: Integer;
  T, S: string;
  GI: Cardinal;
begin
  Times := 10000;

  for I := 1 to Times do
    T := T + Edit1.Text;

  T := T + ComboBox2.Text;

  sr := TSkRegExp.Create;
  try
    sr.Expression := ComboBox1.Text;

    GI := GetTickCount;

    if sr.Exec(T) then
    begin
      GI := GetTickCount - GI;
      S := sr.Match[0];
      if Length(S) > 20 then
        S := '(Matched)';
    end
    else
    begin
      GI := GetTickCount - GI;
      S := Format('(Unmatched)%s', [sr.Expression]);
    end;

    Log(Format('SkRegExp Ave Time: %d : %s', [GI, S]));
  finally

  end;
end;

procedure TForm1.Log(const S: string);
begin
  Memo1.Lines.Add(S);
end;

end.
