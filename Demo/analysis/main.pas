unit main;

interface

{$DEFINE SKREGEXP_DEBUG}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    Page: TPageControl;
    Panel1: TPanel;
    ParserTreeTab: TTabSheet;
    NFATab: TTabSheet;
    edtNFA: TMemo;
    Label1: TLabel;
    edtExpression: TEdit;
    btnAnalysis: TButton;
    ParserTree: TTreeView;
    procedure btnAnalysisClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses SkRegExpW;

{$R *.dfm}

procedure TMainForm.btnAnalysisClick(Sender: TObject);
var
  R: TSkRegExp;
  Node: TTreeNode;
begin
  R := TSkRegExp.Create;
  try
    if edtExpression.Text = '' then
    begin
      MessageDlg('ê≥ãKï\åªÇì¸óÕéwíËÇµÇƒÇ≠ÇæÇ≥Ç¢', mtWarning, [mbOK], 0);
      edtExpression.SetFocus;
      Exit;
    end;
    R.Expression := edtExpression.Text;
    R.Compile;
    ParserTree.Items.BeginUpdate;
    try
      edtNFA.Lines.BeginUpdate;
      try
        R.DumpParse(ParserTree);
        Node := ParserTree.Items.GetFirstNode;
        Node.MakeVisible;
        Node.Selected := True;
        R.DumpNFA(edtNFA.Lines);
      finally
        edtNFA.Lines.EndUpdate;
      end;
    finally
      ParserTree.Items.EndUpdate;
    end;

    Page.ActivePageIndex := 0;
    ParserTree.SetFocus;
  finally
    R.Free;
  end;
end;

end.
