unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    edtSearch: TEdit;
    Button1: TButton;
    Button2: TButton;
    edtFolder: TEdit;
    Label2: TLabel;
    chkIgnoreCase: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Label3: TLabel;
    edtMask: TEdit;
    chkASCIIOnly: TCheckBox;
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

end.
