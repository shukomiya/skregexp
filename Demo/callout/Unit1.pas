unit Unit1;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, SkRegExpW, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private 宣言 }
    procedure OnCallout(ARegExp: TSkRegExp; AData: PCalloutData;
    var IsMatch: Boolean; var MatchLength: Integer);
  public
    { Public 宣言 }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
const
  RE = '(\d{2,5})\s*[(-]?\s*(\d{1,4})\s*[\s)-]?\s*(\d{4,5})(?C)';
var
  r: TSkRegExp;
  text: String;
begin
  text := '電話番号は 03 ( 1234 ) 5678 です。';
  r := TSkRegExp.Create;
  try
    r.OnCallout := OnCallout;
    r.Expression := RE;
    if r.Exec(text) then
    begin
      ShowMessage(r.Groups[0].Strings + 'はデータベースにあります');
    end;
  finally
    r.Free;
  end;
end;

procedure TForm1.OnCallout(ARegExp: TSkRegExp; AData: PCalloutData;
  var IsMatch: Boolean; var MatchLength: Integer);

  //ダミー
  function CheckDatabase(const ATel: REString): Boolean;
  begin
    Result := True;
  end;

var
  r: TSkRegExp;
  Tel, RE, Text: REString;
begin
  if AData^.CalloutNumber = 0 then
  begin
    if (ARegExp.GroupCount >= 1) and (ARegExp.GroupCount <=  3) then
    begin
      //1 ～ 3 の各グループには電話番号かもしれない文字列が格納されている
      Tel := ARegExp.Groups[1].Strings + ARegExp.Groups[2].Strings + ARegExp.Groups[3].Strings;
      //データベースに接続して電話番号を検索
      //検索した結果を IsMatch に代入
      IsMatch := CheckDatabase(Tel);
      //このイベント内ではマッチは行なわない。MatchLength は 0 を代入
      MatchLength := 0;
    end
    else
      IsMatch := False;
  end
  else
  begin
    //コールアウト番号が 0 以外の時はマッチ失敗 False をIsMatch に代入
    //※この値が初期値なのでこの条件でよければココは不要
    IsMatch := False;
    MatchLength := 0;
  end;
end;

end.
