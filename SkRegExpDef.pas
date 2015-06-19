unit SkRegExpDef;

interface

uses Classes;

type
{$IFDEF UNICODE}
  TREStrings = TStrings;
  TREStringList = TStringList;
  REString = UnicodeString;
{$ELSE}
  TREStrings = TWideStrings;
  TREStringList = TWideStringList;
  REString = WideString;
{$ENDIF}
  UChar = UCS4Char;
  UCharArray = array of UChar;

implementation

end.
