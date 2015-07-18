unit Unit1;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, SkRegExpW, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private éŒ¾ }
    FPrevStartMatch: Integer;
    procedure OnCallout(ARegExp: TSkRegExp; AData: PCalloutData;
    var IsMatch: Boolean; var MatchLength: Integer);
  public
    { Public éŒ¾ }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  r: TSkRegExp;
  LGroup: SkRegExpW.TGroup;
  I: Integer;
  S: REString;
begin
  Memo1.Clear;
  Memo1.Lines.BeginUpdate;
  try
    r := TSkRegExp.Create;
    try
      FPrevStartMatch := 0;
      r.OnCallout := OnCallout;
      r.AutoCallout := True;
      r.Expression := Edit1.Text;
      if r.Exec(Edit2.Text) then
      begin
        S := r.InputString;

        Insert('>', S, r.Groups[0].Index + r.Groups[0].Length);
        Insert(#0009'<', S, r.Groups[0].Index);

        Memo1.Lines.Add(Format('%s'#0009'', [S]));

        Memo1.Lines.Add('Match Success!');
        Memo1.Lines.Add(Format('RegExpr: %s',[r.Expression]));
        Memo1.Lines.Add(r.Groups[0].Strings);
        Memo1.Lines.Add('Found');
        Memo1.Lines.Add(Format('Start: %d, Length: %d', [r.Groups[0].Index, r.Groups[0].Length]));
        Memo1.Lines.Add('----------------------------------------------------------');
        Memo1.Lines.Add('No: SubExpr'#0009'Value'#0009'Start & Length');
        Memo1.Lines.Add('----------------------------------------------------------');
        for I := 1 to r.GroupCount do
        begin
          LGroup := r.Groups[I];
          Memo1.Lines.Add(Format('%2.d: (%s)'#0009'%s'#0009'%d, %d',
            [I, LGroup.SubExpression, LGroup.Strings, LGroup.Index, LGroup.Length]));
        end;
      end;
    finally
      r.Free;
    end;
  finally
    Memo1.Lines.EndUpdate;
  end;
end;

procedure TForm1.OnCallout(ARegExp: TSkRegExp; AData: PCalloutData;
  var IsMatch: Boolean; var MatchLength: Integer);
var
  RE, Text: REString;
begin
  if AData.CalloutNumber = 255 then
  begin
    Text := ARegExp.InputString;
    RE := ARegExp.Expression;
    Insert('>', Text, adata.CurrentPosition);
    Insert(#0009'<', Text, AData.StartMatch);

    RE := Copy(RE, AData.PatternPosition, AData.PatternLength);

    Memo1.Lines.Add(Format('%s'#0009':%s', [Text, RE]));
    IsMatch := True;
    MatchLength := 0;
  end;
end;

end.
