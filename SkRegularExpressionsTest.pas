unit SkRegularExpressionsTest;

interface

uses SkRegExpW, SkRegularExpressions;

type
  TSkRegExTest = class
  public
    function MatchFunc(const AMatch: TMatch): REString;
    procedure Test;
    procedure BugFixCheck;
  end;

procedure Test;

implementation

uses SysUtils;

procedure Test;
var
  t: TSkRegExTest;
begin
  t := TSkRegExTest.Create;
  t.Test;
  t.BugFixCheck;
  t.Free;
end;

procedure Check(B: Boolean; S: REString = '');
begin
  if not B then
    raise Exception.Create(S);
end;

{ TSkRegExTest }

procedure TSkRegExTest.BugFixCheck;

  procedure GetMatch(Text, Exp: REString; ADest: TREStrings);
  var
    Match: TMatch;
  begin
    ADest.Clear;
    Match := TRegEx.Match(Text, Exp);
    while Match.Success do
    begin
      ADest.Add(Format('%s,%d,%d', [Match.Value, Match.Index, Match.Length]));
      Match := Match.NextMatch;
    end;
  end;

const
  Text = 'The quick brown fox jumps over the lazy dog';
  Exp = '\w{4,}';
var
  SL: TREStrings;
begin

  SL := TREStringList.Create;
  try
    GetMatch(Text, Exp, SL);

    Check(SL[0] = 'quick,5,5');
    Check(SL[1] = 'brown,11,5');
    Check(SL[2] = 'jumps,21,5');
    Check(SL[3] = 'over,27,4');
    Check(SL[4] = 'lazy,36,4');
  finally
    SL.Free;
  end;

end;

function TSkRegExTest.MatchFunc(const AMatch: TMatch): REString;
begin
  Result := '(' + AMatch.Value + ')';
end;

procedure TSkRegExTest.Test;
const
  CS: array[0..2] of string = ('aaab', 'a', 'a');
  CS2: array[0..2] of string = ('123', '456', '789');
  T = 'RegExpr: %s, Input: %s';
var
  Reg: TRegEx;
  Match: TMatch;
  Group: TGroup;
  Count: Integer;
  StrArray: TStringDynArray;
begin
  Check(TRegEx.IsMatch('aaabc', 'a+b') = True, 'aaabc');
  Check(TRegEx.IsMatch('aaacd', 'a+b') = False, 'aaacd');
  Check(TRegEx.IsMatch('AAABC', 'a+b', [roIgnoreCase]) = True, 'AAABC');

  Reg := TRegEx.Create('a+b');
  Check(Reg.IsMatch('bcaaab', 3) = True, 'bcaaab :3');
  Check(Reg.IsMatch('bcaaab', 6) = False, 'bcaaab :6');

  Reg := TRegEx.Create('a+b', [roIgnoreCase]);
  Match := Reg.Match('bcdaaab', 5);
  Check(Match.Value = 'aab', 'aab');

  Match := Reg.Match('bbaab');
  Check(Match.Success = True, 'bbaab');
  Check(Match.Index = 3, 'bbaab :3');

  Match := TRegEx.Match('aaabcab', 'a+b');
  Check(Match.Value = 'aaab', 'aaab');

//  Match := Match.NextMatch;
//  Check(Match.Value = 'ab', 'ab');

  Reg := TRegEx.Create('(?<x>.)(?<x>.)\k<x>b');
  Match := Reg.Match('ababc');
  Check(Match.Value = 'abab', 'abab');
  Group := Match.Groups[1];
  Check(Group.Value = 'a', 'a');
  Group := Match.Groups['x'];
  Check(Group.Value = 'b', 'b');

  Match := TRegEx.Match('aaabc', '(.)(.)\k<1>b');

  Count := 0;
  for Group in Match.Groups do
  begin
    Check(Group.Value = CS[Count], CS[Count]);
    Inc(Count);
  end;

  Match := TRegEx.Match('aab', 'a+c');
  Check(Match.Value = '');

  Check(TRegEx.Replace('abc', '^', 'x') = 'abc', '(abc, ^)');
  Check(TRegEx.Replace('We have Beatles.', 'e', '$1') = 'W hav Batls.', 'We have Beatles.');
  Check(TRegEx.Replace('We have Beatles.', 'e', '${x}') = 'W hav Batls.', 'We have Beatles.');
  Check(TRegEx.Replace('xaabc', 'a*', '-') = 'x-bc', 'x-bc');

  Check(TRegEx.Replace('xaabc', 'a*', '\n') = 'x'#0010'bc', 'x\nbc');
  Check(TRegEx.Replace('xaabc', 'a*', '\x0a') = 'x'#0010'bc', 'x\nbc');
  Check(TRegEx.Replace('xaabc', 'a*', '\x{d}\x{a}') = 'x'#0013#0010'bc', 'x\r\nbc');
  Check(TRegEx.Replace('xaabc', 'a*', '\cj') = 'x'#0010'bc', 'x\r\nbc');
  Check(TRegEx.Replace('xaabc', 'a*', '\012') = 'x'#0010'bc', 'x\r\nbc');

  Check(TRegEx.Replace('abc123', '\d+', '[NUM]') = 'abc[NUM]', 'abc[NUM]');
  Check(TRegEx.Replace('abc123abc123', '\d+', '[NUM]') = 'abc[NUM]abc[NUM]', 'abc[NUM]abc[NUM]');
  Check(TRegEx.Replace('abc123', '(\d)(\d)(\d)', '$3$2$1') = 'abc321', 'abc321');
  Check(TRegEx.Replace('abc123', '(?<n>\d+)', '<<<${n}>>>') = 'abc<<<123>>>', 'abc<<<123>>>');
  Check(TRegEx.Replace('abc123', '(?<n>\d+)', '$$$1') = 'abc$123', 'abc$123');
  Check(TRegEx.Replace('abc123', '(?<n>\d+)', '$&-$&') = 'abc123-123', 'abc123-123');
  Check(TRegEx.Replace('abc123', '\d+', '$`') = 'abcabc', 'abcabc');
  Check(TRegEx.Replace('abc123', '[a-z]+', '$''') = '123123', '123123');
  Check(TRegEx.Replace('abc123', '(\d)(\d)(\d)', '$+') = 'abc3', 'abc3');
  Check(TRegEx.Replace('abc123', '\d+', '$_') = 'abcabc123', 'abcabc123');

  Check(TRegEx.Replace('abc123', '\d+', '$_') = 'abcabc123', 'abcabc123');

  Check(TRegEx.Replace('abc123abc123', '\d+', MatchFunc) = 'abc(123)abc(123)', 'abc(123)abc(123)');

  Check(TRegEx.Create('\d+').Replace('abc123abc123', MatchFunc, 1) = 'abc(123)abc123', 'abc(123)abc123');

  Count := 0;
  for Match in TRegEx.Matches('abc123def456ghi789', '\d+') do
  begin
    Check(Match.Value = CS2[Count], CS2[Count]);
    Inc(Count);
  end;

  Reg := TRegEx.Create('\d+');
  Count := 1;
  for Match in Reg.Matches('abc123def456ghi789', 7) do
  begin
    Check(Match.Value = CS2[Count], CS2[Count]);
    Inc(Count);
  end;

  StrArray := TRegEx.Split('abZ,cd,efZghi,klm', ',*');
  Check(StrArray[1] = 'cd', 'cd');

  StrArray := TRegEx.Split('ab, cd, ef　, ghi, klm',  '[,\s]+');
  Check(StrArray[4] = 'klm', 'kin');

{$IFDEF USE_UNICODE_PROPERTY}
  StrArray := TRegEx.Split('abZ,　cd,　efZ　ghi, klm',  '[,\sz]+', [roIgnoreCase]);
  Check(StrArray[1] = 'cd', 'cd');

  Reg := TRegEx.Create('[,\sz]+', [roIgnoreCase]);
  StrArray := Reg.Split('abZ,　cd,　efZ　ghi, klm', 3);
  Check(StrArray[1] = 'cd', 'cd');

  Reg := TRegEx.Create('[,\sz]+', [roIgnoreCase]);
  StrArray := Reg.Split('abZ,　cd,　efZ　ghi, klm', 3, 5);
  Check(StrArray[1] = 'cd', 'cd');
{$ENDIF USE_UNICODE_PROPERTY}
end;

end.
