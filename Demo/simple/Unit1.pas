unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SkRegExpW;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Button1: TButton;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    Label5: TLabel;
    Edit5: TEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private êÈåæ }
    R: TSkRegExp;
  public
    { Public êÈåæ }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses IniFiles;

procedure TForm1.Button1Click(Sender: TObject);
begin
  R.Expression := Edit2.Text;

  R.IgnoreCase := CheckBox1.Checked;
  R.SingleLine := CheckBox2.Checked;
  R.MultiLine := CheckBox3.Checked;

  if R.Exec(Edit1.Text) then
  begin
    Edit3.Text := R.Groups[0].Strings;
    Edit4.Text := IntToStr(R.Groups[0].Index);
    Edit5.Text := IntToStr(R.Groups[0].Length);
  end
  else
  begin
    Edit3.Text := 'É}ÉbÉ`ÇµÇ»Ç¢';
    Edit4.Text := '';
    Edit5.Text := '';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Ini: TMemIniFile;
begin
  R := TSkRegExp.Create;

  Ini := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    Edit1.Text := Ini.ReadString('Settings', 'InputString', '');
    Edit2.Text := Ini.ReadString('Settings', 'RegExpr', '');
    CheckBox1.Checked := Ini.ReadBool('Settings', 'IgnoreCase', False);
    CheckBox2.Checked := Ini.ReadBool('Settings', 'SingleLine', False);
    CheckBox3.Checked := Ini.ReadBool('Settings', 'MultiLine', False);
  finally
    Ini.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    Ini.WriteString('Settings', 'InputString', Edit1.Text);
    Ini.WriteString('Settings', 'RegExpr', Edit2.Text);
    Ini.WriteBool('Settings', 'IgnoreCase', CheckBox1.Checked);
    Ini.WriteBool('Settings', 'SingleLine', CheckBox2.Checked);
    Ini.WriteBool('Settings', 'MultiLine', CheckBox3.Checked);

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;

  R.Free;
end;

end.
