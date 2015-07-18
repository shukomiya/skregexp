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
    { Private �錾 }
    procedure OnCallout(ARegExp: TSkRegExp; AData: PCalloutData;
    var IsMatch: Boolean; var MatchLength: Integer);
  public
    { Public �錾 }
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
  text := '�d�b�ԍ��� 03 ( 1234 ) 5678 �ł��B';
  r := TSkRegExp.Create;
  try
    r.OnCallout := OnCallout;
    r.Expression := RE;
    if r.Exec(text) then
    begin
      ShowMessage(r.Groups[0].Strings + '�̓f�[�^�x�[�X�ɂ���܂�');
    end;
  finally
    r.Free;
  end;
end;

procedure TForm1.OnCallout(ARegExp: TSkRegExp; AData: PCalloutData;
  var IsMatch: Boolean; var MatchLength: Integer);

  //�_�~�[
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
      //1 �` 3 �̊e�O���[�v�ɂ͓d�b�ԍ���������Ȃ������񂪊i�[����Ă���
      Tel := ARegExp.Groups[1].Strings + ARegExp.Groups[2].Strings + ARegExp.Groups[3].Strings;
      //�f�[�^�x�[�X�ɐڑ����ēd�b�ԍ�������
      //�����������ʂ� IsMatch �ɑ��
      IsMatch := CheckDatabase(Tel);
      //���̃C�x���g���ł̓}�b�`�͍s�Ȃ�Ȃ��BMatchLength �� 0 ����
      MatchLength := 0;
    end
    else
      IsMatch := False;
  end
  else
  begin
    //�R�[���A�E�g�ԍ��� 0 �ȊO�̎��̓}�b�`���s False ��IsMatch �ɑ��
    //�����̒l�������l�Ȃ̂ł��̏����ł悯��΃R�R�͕s�v
    IsMatch := False;
    MatchLength := 0;
  end;
end;

end.
