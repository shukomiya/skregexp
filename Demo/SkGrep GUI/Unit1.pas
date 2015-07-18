unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, SkFindFile, SkRegExpW;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    SkFindFile1: TSkFindFile;
    Memo1: TMemo;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SkFindFile1Find(Sender: TObject; const FileName: string;
      SearchRec: TSearchRec; var Accept, Cancel: Boolean);
    procedure Memo1DblClick(Sender: TObject);
  private
    { Private 宣言 }
    FRegExp: TSkRegExp;
  public
    { Public 宣言 }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses FileCtrl;

procedure TForm1.Button1Click(Sender: TObject);
var
  sr: TSearchRec;
  SL: TStrings;
  r: TSkRegExp;
  I, Count: Integer;
  S: string;
  IsViewFile: Boolean;
begin
  FRegExp.IgnoreCase := CheckBox1.Checked;
  FRegExp.IgnoreKana := CheckBox2.Checked;
  FRegExp.IgnoreWidth := CheckBox3.Checked;

  FRegExp.SingleLine := CheckBox4.Checked;
  FRegExp.MultiLine := CheckBox5.Checked;

  FRegExp.Expression := Edit1.Text;

  Screen.Cursor := crHourGlass;
  try
    Memo1.Lines.BeginUpdate;
    try
      Memo1.Clear;
      SkFindFile1.Directory := Edit2.Text;
      SkFindFile1.Execute;
    finally
      Memo1.Lines.EndUpdate;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  S: string;
begin
  if SelectDirectory('フォルダの選択', '', S, []) then
    Edit2.Text := S;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FRegExp := TSkRegExp.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FRegExp.Free;
end;

procedure TForm1.Memo1DblClick(Sender: TObject);
var
  LineNo: Integer;
begin
  LineNo:=Memo1.Perform(EM_LINEFROMCHAR, -1, 0);
  ShowMessage(IntToStr(LineNo));
end;

procedure TForm1.SkFindFile1Find(Sender: TObject; const FileName: string;
  SearchRec: TSearchRec; var Accept, Cancel: Boolean);
var
  I: Integer;
  SL: TREStrings;
begin
  SL := TREStringList.Create;
  try
    SL.LoadFromFile(FileName);

    for I := 0 to SL.Count - 1 do
    begin
      if FRegExp.Exec(SL[I]) then
      begin
        Memo1.Lines.Add(
          Format('"%s"(%d,%d,%d) %s',
            [FileName, I,
            FRegExp.Groups[0].Index, FRegExp.Groups[0].Length,
            Copy(SL[I], FRegExp.Groups[0].Index, MaxInt)]));
      end;
    end;
  finally
    SL.Free;
  end;
end;

end.
