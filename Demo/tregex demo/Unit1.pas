unit Unit1;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    Edit1: TEdit;
    Match: TButton;
    Label2: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Label3: TLabel;
    Edit2: TEdit;
    Button2: TButton;
    Label4: TLabel;
    Memo3: TMemo;
    Memo4: TMemo;
    Label7: TLabel;
    Edit4: TEdit;
    Label5: TLabel;
    Edit3: TEdit;
    Button3: TButton;
    Label8: TLabel;
    Memo5: TMemo;
    Memo6: TMemo;
    procedure MatchClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private 宣言 }
  public
    { Public 宣言 }
    procedure Log(const S: string);
  end;

var
  Form1: TForm1;

implementation

uses SkRegularExpressions;

{$R *.dfm}

procedure TForm1.MatchClick(Sender: TObject);
var
  Group: TGroup;
  Match: TMatch;
  MatchList: TMatchCollection;
begin
  //Memo1.Text 内のすべてのマッチを返す
  MatchList := TRegEx.Matches(Memo1.Text, Edit1.Text);

  //マッチの内容を列挙
  for Match in MatchList do
    for Group in Match.Groups do
      Log(Format('%s [%d, %d]',
        [Group.Value, Group.Index, Group.Length]));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Log(TRegEx.Create(Edit2.Text).Replace(Memo3.Text, Edit4.Text));
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  D: TStringDynArray;
  S: string;
begin
  D := TRegEx.Create(Edit3.Text).Split(Memo5.Text);
  for S in D do
    Log(S);
end;

procedure TForm1.Log(const S: string);
begin
  case PageControl1.ActivePageIndex of
    0:
      Memo2.Lines.Add(S);
    1:
      Memo4.Lines.Add(S);
    else
      Memo6.Lines.Add(S);
  end;
end;

end.
