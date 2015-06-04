unit SkRegExpUTest;

interface

{$IFDEF UNICODE}
{$WARN IMPLICIT_STRING_CAST ERROR}
{$WARN IMPLICIT_STRING_CAST_LOSS ERROR}
{$WARN WIDECHAR_REDUCED ERROR}
{$ENDIF}
{ .$DEFINE HIsHexDigit }
{$DEFINE SYNTAX_CHECK }
{$DEFINE NoUSEJapaneseExt}
{$DEFINE USE_UNICODE_PROPERTY}

uses SysUtils, Classes, SkRegExpW;

type
  TTestSkRegExp = class
  private
    FRegExp: TSkRegExp;
    FList: TStrings;
    FErrorList: TStrings;
    FCounter: Integer;
    procedure DoReplaceEvent(Sender: TObject; var ReplaceWith: REString);
    function DoReplaceFunc(ARegExp: TSkRegExp): REString;
    procedure OnCalloutBehind(ARegExp: TSkRegExp; AData: PCalloutData;
      var IsMatch: Boolean; var MatchLength: Integer);
    procedure OnCalloutAhead(ARegExp: TSkRegExp; AData: PCalloutData;
      var IsMatch: Boolean; var MatchLength: Integer);
    procedure OnCalloutVerb(ARegExp: TSkRegExp; AData: PCalloutData;
      var IsMatch: Boolean; var MatchLength: Integer);
    procedure Check(B: Boolean; S: REString);
  public
    constructor Create;
    destructor Destroy; override;
    procedure TestNamedGroup;
    procedure TestRepace;
    procedure TestSplit;
    procedure TestEscape;
    procedure TestCallout;
    procedure TestVerb;
    procedure TestPartial;
    procedure TestExecPos;
    procedure Exec(ADest: TStrings);
  end;

procedure Test(ADest: TStrings);

{$IFDEF SKREGEXP_DEBUG}
procedure TestJpExtTable;
procedure TestREStrPos;
procedure TestQuickSearch;
{$ENDIF}
procedure CheckRegExp(const Expression, Source: REString; APos, ALen: Integer;
  ASubLen: array of Integer);

var
  StackDepth: Integer;

implementation

uses Windows;

{ TTestReplace }

procedure TTestSkRegExp.Check(B: Boolean; S: REString);
begin
  try
  if not B then
    FErrorList.Add(S);
  except
    on E: ESkRegExpCompile do
    FErrorList.Add(S);
  end;
end;

constructor TTestSkRegExp.Create;
begin
  inherited;
  FRegExp := TSkRegExp.Create;
  FList := TStringList.Create;
end;

destructor TTestSkRegExp.Destroy;
begin
  FList.Free;
  FRegExp.Free;
  inherited;
end;

procedure TTestSkRegExp.DoReplaceEvent(Sender: TObject;
  var ReplaceWith: REString);
begin
  ReplaceWith := '(' + (Sender as TSkRegExp).Groups[0].Strings + ')';
end;

function TTestSkRegExp.DoReplaceFunc(ARegExp: TSkRegExp): REString;
begin
  Result := '(' + ARegExp.Groups[0].Strings + ')';
end;

procedure TTestSkRegExp.Exec(ADest: TStrings);
begin
  FErrorList := ADest;
  TestNamedGroup;
{$IFDEF NotOptimizeCompil}
  TestVerb;
{$ENDIF NotOptimizeCompil}
  TestEscape;
  TestRepace;
  TestSplit;
  TestCallout;
  TestExecPos;
  // TestPartial;
end;

procedure TTestSkRegExp.OnCalloutAhead(ARegExp: TSkRegExp; AData: PCalloutData;
  var IsMatch: Boolean; var MatchLength: Integer);
var
  S: REString;
begin
  if (AData.CalloutNumber = 1) then
  begin
    S := Copy(ARegExp.InputString, AData.CurrentPosition, 2);
    if S = '一番' then
    begin
      MatchLength := 0;
      IsMatch := True;
    end;
  end;
end;

procedure TTestSkRegExp.OnCalloutBehind(ARegExp: TSkRegExp; AData: PCalloutData;
  var IsMatch: Boolean; var MatchLength: Integer);
var
  S: REString;
begin
  if (AData.CalloutNumber = 1) and (AData.CurrentPosition >= 2) then
  begin
    S := Copy(ARegExp.InputString, AData.CurrentPosition - 2, 2);
    if S = '一番' then
    begin
      MatchLength := 0;
      IsMatch := True;
    end;
  end;
end;

procedure TTestSkRegExp.OnCalloutVerb(ARegExp: TSkRegExp; AData: PCalloutData;
  var IsMatch: Boolean; var MatchLength: Integer);
var
  S: REString;
begin
  if AData.CalloutNumber = 0 then
  begin
    IsMatch := True;
    S := Copy(ARegExp.InputString, AData.StartMatch, AData.CurrentPosition - AData.StartMatch);
    FList.Add(S);
    Inc(FCounter);
  end;
end;

procedure TTestSkRegExp.TestCallout;

  function Callout(const Expression, Text: REString; AEvent: TCalloutEvent;
    AOption: TREOptions = []): REString;
  var
    R: TSkRegExp;
  begin
    R := TSkRegExp.Create;
    try
      R.OnCallout := AEvent;
      R.Options := AOption;
      R.Expression := Expression;
      if R.Exec(Text) then
        Result := R.Groups[0].Strings;
    finally
      R.Free;
    end;
  end;

begin
{$IFDEF USE_UNICODE_PROPERTY}
  Check(Callout('\w\w(?C1)', '一番になれる', OnCalloutBehind) = '一番', '(?C1)');
  Check(Callout('\w{2}(?C1)', '一番になれる', OnCalloutBehind) = '一番', '\w{2](?C1)');
  Check(Callout('\w{2}(?C1)', 'がんばれば一番になれる', OnCalloutBehind) = '一番',
    '\w{2](?C1)');
  Check(Callout('\w{2}(?C1)', 'がんばったら一番', OnCalloutBehind) = '一番',
    '\w{2](?C1)');
{$ENDIF USE_UNICODE_PROPERTY}
  Check(Callout('.+(?C1)', 'がんばれば一番になれる', OnCalloutBehind) = 'がんばれば一番',
    '.+(?C1)');
  Check(Callout('(?C1)', 'がんばれば一番になれる', OnCalloutBehind) = '', '(?C1)');

  Check(Callout('.+(?C1)', 'がんばれば一番になれる', OnCalloutAhead) = 'がんばれば', '.+(?C1)');
end;

procedure TTestSkRegExp.TestEscape;

  procedure CheckDecode(const Source, Dest: REString; c: WideChar = 'y');
  var
    B: Boolean;
    R: TSkRegExp;
  begin
    try
      R := TSkRegExp.Create;
      try
        R.Expression := DecodeEscape(Source);
        B := R.Exec(Dest);

        if (c = 'y') and not B then
          FErrorList.Add('why not Match: ' + Source)
        else if (c = 'n') and B then
          FErrorList.Add('why Match: ' + Source)
        else if (c = 'e') then
          FErrorList.Add('why not error: ' + Source)
      finally
        R.Free;
      end;
    except
      on E: ESkRegexp do
        if c <> 'e' then
          FErrorList.Add(Source + ': ' + E.Message);
    end;
  end;

  procedure CheckEncode(const Source, Dest: REString; c: WideChar = 'y');
  var
    B: Boolean;
  begin
    try
      B := EncodeEscape(Source) = Dest;
      if (c = 'y') and not B then
        FErrorList.Add('why not Match: ' + Source)
      else if (c = 'n') and B then
        FErrorList.Add('why Match: ' + Source)
      else if (c = 'e') then
        FErrorList.Add('why not error: ' + Source)
    except
      on E: ESkRegexp do
        if c <> 'e' then
          FErrorList.Add(Source + ': ' + E.Message);
    end;
  end;

begin
  CheckDecode('abc\r\n\f\a\e', 'abc'#$000D#$000A#$000C#$0007#$001B);
  CheckDecode('abc\012\x{a}\x0d\c[\x{003001}', 'abc'#$000A#$000A#$000D#$001B'、');
  CheckDecode('\x{3042}', 'あ');
  CheckDecode('\x{3042', 'あ', 'e');

  CheckDecode('\o{30102}', 'あ');
  CheckDecode('\o{30102', 'あ', 'e');
  CheckDecode('\o30102}', 'あ', 'e');
  CheckDecode('\o{7777777}', 'あ', 'e');
  CheckDecode('\cA', #0001);
  CheckDecode('a\1', 'a'#0001);
  CheckDecode('(a)\1\2', 'aa'#0002);


  CheckEncode('abc'#$000D#$000A#$000C#$0007#$001B,'abc\r\n\f\a\e');
  CheckEncode('abc'#$000A#$000A#$000D#$001B'、','abc\n\n\r\e、');
  CheckEncode('abc'#0001#0002#$000B, 'abc\x{01}\x{02}\x{0B}');
end;

procedure TTestSkRegExp.TestExecPos;
var
  r: TSkRegExp;
begin
// test for TSkRegExp.ClearMatchExplosionState Method
  r := TSkRegExp.Create;
  try
    r.InputString := '<http120://sunsite.unc.edu/mdw/HOWTO/Kernel-HOWTO.html>(NDT    :   Kernel';
    r.Expression := '([a-zA-Z][a-zA-Z0-9]*)://([^ /]+)(/?[^ ]*)?';
    r.ExecPos;
    r.Expression := '([^ @]+)@([^ @]+)';
    r.ExecPos;
    r.Expression := '([a-zA-Z][a-zA-Z0-9]*)://';
    r.ExecPos;
  finally
    r.Free;
  end;
end;

procedure TTestSkRegExp.TestNamedGroup;
var
  r: TSkRegExp;
begin
  r := TSkRegExp.Create;
  try
    r.Expression := '(?<n>abc)|(?<n>def)|(?<n>efg)';
    if r.Exec('dot net efg version') then
      Check(r.Groups.Names['n'].Strings = 'efg', 'Group Name Error');

    r.Expression := '(?<n>a)(?<n>b)(?<n>c)';
    if r.Exec('abc') then
      Check(r.Groups.Names['n'].Strings = 'a', 'Group Name Error');

    r.Expression := '(?<n>a)?bc';
    if r.Exec('bc') then
      Check(r.Groups.Names['n'].Strings = '', 'Group Name Error');

  finally
    r.Free;
  end;
end;

procedure TTestSkRegExp.TestPartial;

  function p(const RE, Text: REString; AOptions: TREOptions = []): REString;
  var
    R: TSkRegExp;
  begin
    R := TSkRegExp.Create;
    try
      R.Expression := RE;
//       if r.ExecPartial(Text) <> mrNone then
//       Result := r.Groups[0].Strings;
    finally
      R.Free;
    end;
  end;

begin
  Check(p('\d{3}-?\d{4}', '1') = '1', '1');
  Check(p('\d{3}-?\d{4}', '12') = '12', '12');
  Check(p('\d{3}-?\d{4}', '123') = '123', '123');
  Check(p('\d{3}-?\d{4}', '123') = '123', '123');
  Check(p('\d{3}-?\d{4}', '123-4') = '123-4', '123');
end;

procedure TTestSkRegExp.TestRepace;
begin
  FRegExp.Expression := '(\d{2,4})\s?\-\s?ab';
  Check(FRegExp.Replace('20-ab abc', '$1-abc') = '20-abc abc', '20-ab abc');

  FRegExp.Expression := 'e';
  Check(FRegExp.Replace('We have Beatles.', '$1') = 'W hav Batls.',
    'We have Beatles.');

  FRegExp.Expression := 'e';
  Check(FRegExp.Replace('We have Beatles.', '${x}') = 'W hav Batls.',
    'We have Beatles.');

  FRegExp.Expression := 'a*';
  Check(FRegExp.Replace('xaabc', '-') = 'x-bc', 'x-bc');
  Check(FRegExp.Replace('xaabcxaabcxaabcxaabcxaabc', '-', 3)
    = 'x-bcx-bcx-bcxaabcxaabc', 'x-bc');
  Check(FRegExp.Replace('xaabcxaabcxaabcxaabcxaabc', '-', 0, 4)
    = 'xaabcx-bcx-bcx-bcx-bc', 'x-bc');

  FRegExp.Expression := 'a*';
  Check(FRegExp.Replace('xaabc', '\n') = 'x'#0010'bc', 'x\nbc');
  Check(FRegExp.Replace('xaabc', '\x0a') = 'x'#0010'bc', 'x\nbc');
  Check(FRegExp.Replace('xaabc', '\x{d}\x{a}') = 'x'#0013#0010'bc', 'x\r\nbc');
  Check(FRegExp.Replace('xaabc', '\x{3001}\x{003002}') = 'x、。bc', 'x\r\nbc');
  Check(FRegExp.Replace('xaabc', '\cj') = 'x'#0010'bc', 'x\r\nbc');
  Check(FRegExp.Replace('xaabc', '\012') = 'x'#0010'bc', 'x\r\nbc');

  FRegExp.Expression := '^';
  Check(FRegExp.Replace('abc', '1') = 'abc', '(abc, 1)');

  FRegExp.Expression := '^';
  Check(FRegExp.Replace('abc', '1', 0, 2) = 'abc', '(abc, 1, 0, 2)');

  FRegExp.Expression := '\d+';
  Check(FRegExp.Replace('abc123', '[NUM]') = 'abc[NUM]', 'abc[NUM]');
  Check(FRegExp.Replace('abc123abc123', '[NUM]') = 'abc[NUM]abc[NUM]',
    'abc[NUM]abc[NUM]');

  FRegExp.Expression := '(\d)(\d)(\d)';
  Check(FRegExp.Replace('abc123', '$3$2$1') = 'abc321', 'abc321');

  FRegExp.Expression := '(?<n>\d+)';
  Check(FRegExp.Replace('abc123', '<<<${n}>>>') = 'abc<<<123>>>',
    'abc<<<123>>>');

  FRegExp.Expression := '(?<n>\d+)';
  Check(FRegExp.Replace('abc123', '$$$1') = 'abc$123', 'abc$123');

  FRegExp.Expression := '(?<n>\d+)';
  Check(FRegExp.Replace('abc123', '$&-$&') = 'abc123-123', 'abc123-123');

  FRegExp.Expression := '\d+';
  Check(FRegExp.Replace('abc123', '$`') = 'abcabc', 'abcabc');

  FRegExp.Expression := '[a-z]+';
  Check(FRegExp.Replace('abc123', '$''') = '123123', '123123');

  FRegExp.Expression := '(\d)(\d)(\d)';
  Check(FRegExp.Replace('abc123', '$+') = 'abc3', 'abc3');

  FRegExp.Expression := '\d+';
  Check(FRegExp.Replace('abc123', '$_') = 'abcabc123', 'abcabc123');

  FRegExp.Expression := '\d+';
  Check(FRegExp.Replace('abc123', '$_') = 'abcabc123', 'abcabc123');

  FRegExp.Expression := '\d+';
  FRegExp.OnReplace := DoReplaceEvent;
  try
    Check(FRegExp.Replace('abc123abc123', '') = 'abc(123)abc(123)',
      'abc(123)abc(123)');
  finally
    FRegExp.OnReplace := nil;
  end;

  // global func version

  Check(RegReplace('e', 'We have Beatles.', '$1') = 'W hav Batls.',
    'We have Beatles.');

  Check(RegReplace('e', 'We have Beatles.', '${x}') = 'W hav Batls.',
    'We have Beatles.');

  Check(RegReplace('a*', 'xaabc', '-') = 'x-bc', 'x-bc');

  Check(RegReplace('a*', 'xaabc', '\n') = 'x'#0010'bc', 'x\nbc');
  Check(RegReplace('a*', 'xaabc', '\x0a') = 'x'#0010'bc', 'x\nbc');
  Check(RegReplace('a*', 'xaabc', '\x{d}\x{a}') = 'x'#0013#0010'bc', 'x\r\nbc');
  Check(RegReplace('a*', 'xaabc', '\x{3001}\x{003002}') = 'x、。bc', 'x\r\nbc');
  Check(RegReplace('a*', 'xaabc', '\cj') = 'x'#0010'bc', 'x\r\nbc');
  Check(RegReplace('a*', 'xaabc', '\012') = 'x'#0010'bc', 'x\r\nbc');

  Check(RegReplace('^', 'abc', '1') = 'abc', '(abc, 1)');

  Check(RegReplace('\d+', 'abc123', '[NUM]') = 'abc[NUM]', 'abc[NUM]');
  Check(RegReplace('\d+', 'abc123abc123', '[NUM]') = 'abc[NUM]abc[NUM]',
    'abc[NUM]abc[NUM]');

  Check(RegReplace('(\d)(\d)(\d)', 'abc123', '$3$2$1') = 'abc321', 'abc321');

  Check(RegReplace('(?<n>\d+)', 'abc123', '<<<${n}>>>') = 'abc<<<123>>>',
    'abc<<<123>>>');

  Check(RegReplace('(?<n>\d+)', 'abc123', '$$$1') = 'abc$123', 'abc$123');

  Check(RegReplace('(?<n>\d+)', 'abc123', '$&-$&') = 'abc123-123',
    'abc123-123');

  Check(RegReplace('\d+', 'abc123', '$`') = 'abcabc', 'abcabc');

  Check(RegReplace('[a-z]+', 'abc123', '$''') = '123123', '123123');

  Check(RegReplace('(\d)(\d)(\d)', 'abc123', '$+') = 'abc3', 'abc3');

  Check(RegReplace('\d+', 'abc123', '$_') = 'abcabc123', 'abcabc123');

  Check(RegReplace('\d+', 'abc123', '$_') = 'abcabc123', 'abcabc123');

  Check(RegReplace('\d+', 'abc123abc123', DoReplaceFunc) = 'abc(123)abc(123)',
    'abc(123)abc(123)');

  // class func version

  Check(TSkRegExp.RegReplace('e', 'We have Beatles.', '$1') = 'W hav Batls.',
    'We have Beatles.');

  Check(TSkRegExp.RegReplace('e', 'We have Beatles.', '${x}') = 'W hav Batls.',
    'We have Beatles.');

  Check(TSkRegExp.RegReplace('a*', 'xaabc', '-') = 'x-bc', 'x-bc');

  Check(TSkRegExp.RegReplace('a*', 'xaabc', '\n') = 'x'#0010'bc', 'x\nbc');
  Check(TSkRegExp.RegReplace('a*', 'xaabc', '\x0a') = 'x'#0010'bc', 'x\nbc');
  Check(TSkRegExp.RegReplace('a*', 'xaabc', '\x{d}\x{a}') = 'x'#0013#0010'bc',
    'x\r\nbc');
  Check(TSkRegExp.RegReplace('a*', 'xaabc', '\x{3001}\x{003002}') = 'x、。bc',
    'x\r\nbc');
  Check(TSkRegExp.RegReplace('a*', 'xaabc', '\cj') = 'x'#0010'bc', 'x\r\nbc');
  Check(TSkRegExp.RegReplace('a*', 'xaabc', '\012') = 'x'#0010'bc', 'x\r\nbc');

  Check(TSkRegExp.RegReplace('^', 'abc', '1') = 'abc', '(abc, 1)');

  Check(TSkRegExp.RegReplace('\d+', 'abc123', '[NUM]') = 'abc[NUM]',
    'abc[NUM]');
  Check(TSkRegExp.RegReplace('\d+', 'abc123abc123', '[NUM]')
    = 'abc[NUM]abc[NUM]', 'abc[NUM]abc[NUM]');

  Check(TSkRegExp.RegReplace('(\d)(\d)(\d)', 'abc123', '$3$2$1') = 'abc321',
    'abc321');

  Check(TSkRegExp.RegReplace('(?<n>\d+)', 'abc123', '<<<${n}>>>')
    = 'abc<<<123>>>', 'abc<<<123>>>');

  Check(TSkRegExp.RegReplace('(?<n>\d+)', 'abc123', '$$$1') = 'abc$123',
    'abc$123');

  Check(TSkRegExp.RegReplace('(?<n>\d+)', 'abc123', '$&-$&') = 'abc123-123',
    'abc123-123');

  Check(TSkRegExp.RegReplace('\d+', 'abc123', '$`') = 'abcabc', 'abcabc');

  Check(TSkRegExp.RegReplace('[a-z]+', 'abc123', '$''') = '123123', '123123');

  Check(TSkRegExp.RegReplace('(\d)(\d)(\d)', 'abc123', '$+') = 'abc3', 'abc3');

  Check(TSkRegExp.RegReplace('\d+', 'abc123', '$_') = 'abcabc123', 'abcabc123');

  Check(TSkRegExp.RegReplace('\d+', 'abc123', '$_') = 'abcabc123', 'abcabc123');

  Check(TSkRegExp.RegReplace('\d+', 'abc123abc123', DoReplaceFunc)
    = 'abc(123)abc(123)', 'abc(123)abc(123)');

end;

procedure TTestSkRegExp.TestSplit;
var
  SL: TREStrings;
begin
  SL := TREStringList.Create;
  try
    FRegExp.Expression := ',\s*';
    FRegExp.IgnoreCase := True;
    FRegExp.Split('abZ,cd,efZghi,klm', SL);
    Check(SL[1] = 'cd', 'cd');

    FRegExp.Expression := '[,\s]+';
    FRegExp.Split('ab, cd, ef　, ghi, klm', SL);
    Check(SL[4] = 'klm', 'klm');

{$IFDEF USE_UNICODE_PROPERTY}
    FRegExp.Expression := '[,\sz]+';
    FRegExp.IgnoreCase := True;
    FRegExp.Split('abZ,　cd,　efZ　ghi, klm', SL);
    Check(SL[1] = 'cd', 'cd');

    FRegExp.Expression := '[,\sz]+';
    FRegExp.IgnoreCase := True;
    FRegExp.Split('abZ,　cd,　efZ　ghi, klm', SL, 3);
    Check(SL[1] = 'cd', 'cd');

    FRegExp.Expression := '[,\sz]+';
    FRegExp.IgnoreCase := True;
    FRegExp.Split('abZ,　cd,　efZ　ghi, klm', SL, 3, 5);
    Check(SL[1] = 'cd', 'cd');

{$ENDIF USE_UNICODE_PROPERTY}
    // global func version

    RegSplit(',\s*', 'abZ,cd,efZghi,klm', SL, [roIgnoreCase]);
    Check(SL[1] = 'cd', 'cd');

{$IFDEF USE_UNICODE_PROPERTY}
    RegSplit('[,\s]+', 'ab, cd, ef　, ghi, klm', SL);
    Check(SL[4] = 'klm', 'kin');

    RegSplit('[,\sz]+', 'abZ,　cd,　efZ　ghi, klm', SL, [roIgnoreCase]);
    Check(SL[1] = 'cd', 'cd');
{$ENDIF USE_UNICODE_PROPERTY}

    // class func version

    TSkRegExp.RegSplit(',\s*', 'abZ,cd,efZghi,klm', SL, [roIgnoreCase]);
    Check(SL[1] = 'cd', 'cd');

{$IFDEF USE_UNICODE_PROPERTY}
    TSkRegExp.RegSplit('[,\s]+', 'ab, cd, ef　, ghi, klm', SL);
    Check(SL[4] = 'klm', 'kin');

    TSkRegExp.RegSplit('[,\sz]+', 'abZ,　cd,　efZ　ghi, klm', SL, [roIgnoreCase]);
    Check(SL[1] = 'cd', 'cd');

{$ENDIF USE_UNICODE_PROPERTY}
  finally
    SL.Free;
  end;
end;

procedure TTestSkRegExp.TestVerb;

  procedure ct(const RE, Text: REString; Count: Integer; S: REString = '');
  var
    r: TSkRegExp;
  begin
    FCounter := 0;
    FList.Clear;

    r := TSkRegExp.Create;
    try
      r.Expression := RE;
      r.OnCallout := OnCalloutVerb;
      r.Exec(Text);
      if FCounter <> Count then
        raise Exception.CreateFmt('(ct): "%s" %d <> (?)%d', [RE, FCounter, Count]);
      if (S <> '') and (S <> FList[FCounter - 1]) then
        raise Exception.CreateFmt('(ct): "%s" %s <> (?)%s', [RE, FList[FCounter - 1], S]);

    finally
      r.Free;
    end;
  end;

  procedure cm(const RE, Text, Name: REString);
  var
    r: TSkRegExp;
  begin
    FList.Clear;

    r := TSkRegExp.Create;
    try
      r.Expression := RE;
      r.OnCallout := OnCalloutVerb;
      r.Exec(Text);
      if r.RegMark <> Name then
        raise Exception.CreateFmt('(ct): "%s" %s <> (?)%s', [RE, r.RegMark, Name]);
    finally
      r.Free;
    end;
  end;

  procedure cn(const RE, Text, Name: REString);
  var
    r: TSkRegExp;
  begin
    FList.Clear;

    r := TSkRegExp.Create;
    try
      r.Expression := RE;
      r.OnCallout := OnCalloutVerb;
      r.Exec(Text);
      if r.RegError <> Name then
        raise Exception.CreateFmt('(ct): "%s" %s <> (?)%s', [RE, r.RegMark, Name]);
    finally
      r.Free;
    end;
  end;

  procedure c3(const RE, Text: REString; Start, Last, Index: Integer);
  var
    r: TSkRegExp;
  begin
    FList.Clear;

    r := TSkRegExp.Create;
    try
      r.Expression := RE;
//      r.Optimize := opCompileOnly;
      r.OnCallout := OnCalloutVerb;
      if r.Exec(Text) then
      begin
        if (r.Groups[Index].Index <> Start) and
            (r.Groups[Index].Length <> Last) then
          raise Exception.CreateFmt('(ct): "%s" %d, %d <> (?)%d, %d',
            [RE, r.Groups[Index].Index, r.Groups[Index].Length, Start, Last]);
      end
      else
        raise Exception.CreateFmt('(ct): "%s" %d, %d not match', [RE, Start, Last]);
    finally
      r.Free;
    end;
  end;

  procedure c2(const RE, Text: REString; Start, Last: Integer);
  var
    r: TSkRegExp;
  begin
    FList.Clear;

    r := TSkRegExp.Create;
    try
      r.Expression := RE;
      r.OnCallout := OnCalloutVerb;
      if r.Exec(Text) then
      begin
        if (r.Groups[0].Index <> Start) and
            (r.Groups[0].Length <> Last) then
          raise Exception.CreateFmt('(ct): "%s" %d, %d <> (?)%d, %d',
            [RE, r.Groups[0].Index, r.Groups[0].Length, Start, Last]);
      end
      else
        raise Exception.CreateFmt('(ct): "%s" %d, %d not match', [RE, Start, Last]);
    finally
      r.Free;
    end;
  end;

begin
  ct('a+(?C)(*FAIL)', 'aaaa', 10);
  ct('a+b?(?C)(*FAIL)', 'aaab', 4);
//  ct('a+b?(?C)(*FAIL)', 'aaab', 9); //for not define CHECK_MATCH_EXPLOSION
  ct('a+b?(*PRUNE)(?C)(*FAIL)', 'aaab', 2);
  ct('a+b?(*SKIP)(?C)(*FAIL)', 'aaabaaab', 2);
  ct('(*SKIP:N)a+b?(*MARK:N)(?C)(*FAIL)', 'aaabaaab', 2);
  ct('(*SKIP:N)a+b?(*:N)(?C)(*FAIL)', 'aaabaaab', 2);
  ct('a(a+a(*THEN)(?C)z|a+b(*THEN)(?C)y|a+c(*THEN)(?C)x)','aaaacx', 2);
  ct('a(a+a(*THEN)(?C)z|a+b(*THEN)(?C)y|a+c(*THEN)(?C)x)','aaaacx', 2);
  ct('a+b(a+a(*THEN)(?C)z|a+b(*THEN)(?C)y|a+c(*THEN)(?C)x)','aabaaaacx', 2);
  ct('a(a+a(*THEN)(?C)z)','aaaacx', 2);
  ct('a+b?(*COMMIT)(?C)(*FAIL)', 'aaab', 1, 'aaab');

  cm('a+b?(*PRUNE:n)(?C)', 'aaab', 'n');
  cm('a(a+a(*THEN:n)(?C)z)','aaaaz', 'n');
  cm('(?:x(*MARK:x)|y(*MARK:y)|z(*MARK:z))', 'z', 'z');
  cm('(?:x(*:x)|y(*:y)|z(*:z))', 'y', 'y');
  cm('a+b?(*COMMIT:n)(?C)', 'aaab', 'n');
  cn('a+b?(*COMMIT:n)(?C)(*FAIL)', 'aaab', 'n');

  c3('A(A|B(*ACCEPT)|C)D', 'AB', 2, 1, 1);
  c3('(?<foo>a(*ACCEPT)|\(\g<foo>\))', '((a))', 1, 5, 1);
  c2('(?<foo>a(*ACCEPT)b|\(\g<foo>\))', '((ab))', 3, 1);
  c3('(?<foo>a+b(*ACCEPT)|\(\g<foo>\))', '((aab))', 1, 7, 1);

end;

procedure SyntaxCheck(ADest: TStrings);

  procedure Check(B: Boolean; S: REString);
  begin
    if not B then
      raise Exception.Create(S);
  end;

  procedure sx(const Expression, AText: REString; AOptions: TREOptions = []);
  var
    R: TSkRegExp;
  begin
    R := TSkRegExp.Create;
    try
      try
        R.Expression := Expression;
        R.Options := AOptions;
        if R.Exec(AText) then
          ADest.Add('sx: why match?: "' + Expression + '", "' + AText + '"')
        else
          ADest.Add('sx: why not match?: "' + Expression + '", "' +
            AText + '"');
      except
        on E: ESkRegExp do;
      end;

    finally
      R.Free;
    end;
  end;

  procedure CheckReplate;
  var
    R: TSkRegExp;
  begin
    try

      R := TSkRegExp.Create;
      try
        Check(R.Replace('xaabc', '\x2g\c\c\x{003001\x{a}') = 'x'#0013#0010'bc',
          'x\r\nbc');
      finally
        R.Free;
      end;
      Check(TSkRegExp.RegReplace('a*', 'xaabc', '\x2g\c\c\x{003001\x{a}')
        = 'x'#0013#0010'bc', 'x\r\nbc');
      Check(RegReplace('a*', 'xaabc', '\x2g\c\c\x{003001\x{a}')
        = 'x'#0013#0010'bc', 'x\r\nbc');
    except
      // on E: Exception do
      // ;
    end;
  end;

begin

  CheckReplate;
  //2.1 add
  sx('(?<n>a)\1', 'aa', [roNamedGroupOnly]);
  sx('(?<n>a)\k<1>', 'aa', [roNamedGroupOnly]);
  sx('(?<n>a)\k<-1>', 'aa', [roNamedGroupOnly]);
  sx('(?<n>a)\g{1}', 'aa', [roNamedGroupOnly]);
  sx('(?<n>a)\g{-1}', 'aa', [roNamedGroupOnly]);

  sx('(?<n>a)(?<m>b)\g<1>', 'aba', [roNamedGroupOnly]);
  sx('(?<n>a)(?<m>b)\g<-2>', 'aba', [roNamedGroupOnly]);


  sx('[:alnum:]+', '01ab');
  sx('[:[alpam:]]+', '01ab');

  sx('abc(?R', 'abc');
  sx('(?<n>abc)(?&n', 'abc');

  sx('\xt5XY', 'a');
  sx('\x{t5XY', 'a');
  sx('\x{56413abaXY', 'a');
  sx('\x{11FFFF}XY', 'a');
  sx('[a-zA-Z0-1', 'a');
  sx('a{40000}', 'a');
  sx('a{40000,50000}', 'a');
  sx('a{10000,1}', 'a');
  sx('a{10,20', 'a');

  sx('[[:alnu:]]+', 'aaaa');

  sx('[z-a]+', 'aaa');
  sx('[\x{0061}-\x{0041}]+', 'aaa');

  sx('\g<1(abc)', 'abc');
  sx('\g<m(abc)', 'abc');
  sx('\g<-m>(abc)', 'abc');

  sx('(?(1', 'abc');
  sx('(?(<name_1', 'abc');
  sx('(?(''name_1', 'abc');

  sx('(?P<nabc)', 'abc');
  sx('(?Pnabc)', 'abc');

  sx('(?io)abc', 'abc');
  sx('(?i', 'abc');

  sx('\k<-1>(a)', 'aa');

  sx('\g<-1>(a)', 'aa');
  sx('\g<-1>(a)', 'aa');

  sx('(a)(?()a|b)', 'aab');

  sx('(?:ab)??{2}?', '');
  sx('(?:ab)??{2}?', 'ababa');

  sx('(?:ab)*?{0}?', 'ababa');
  sx('(?:ab){2,4}??', 'ababababab');
  sx('(?:abc)+{2}?', 'abcabcabc');

  sx('(?(R|<.*?>)', 'aaa');
  sx('(?(R<.*?>)', 'aaa');
  sx('(?<name>.*(?(&name.*?>)', 'aaa');

  sx('(<a\s+[^>]+>)?<img\s+[^>]+>(?(1<\/a>|<img>)',
    '<a href="index.html"><img src="image.gif"></a>');
  sx('(<a\s+[^>]+>)?<img\s+[^>]+>(?1)<\/a>|<img>)',
    '<a href="index.html"><img src="image.gif"></a>');
  sx('(?<foo><a\s+[^>]+>)?<img\s+[^>]+>(?(foo>)<\/a>|<img>)',
    '<a href="index.html"><img src="image.gif"></a>');
  sx('(?<foo><a\s+[^>]+>)?<img\s+[^>]+>(?(<foo)<\/a>|<img>)',
    '<a href="index.html"><img src="image.gif"></a>');

  sx('(?(?<=NUM:\d+|\w+)', '===:NAO');
  sx('(?x)  ( \( )?    [^()]+    (?(1 \) )', '(明日(10日)は晴れます)');

  sx('(?x) (?(?=[^a-z]*[a-z] \d{2}-[a-z]{3}-\d{2}  |  \d{2}-\d{2}-\d{2} )',
    '00-tap-12');
  sx('(?x) (??=[^a-z]*[a-z]) \d{2}-[a-z]{3}-\d{2}  |  \d{2}-\d{2}-\d{2} )',
    '12-10-12');

  sx('<([^>]+)>[^<>]*(?:</(?(R\1)>|(?R))', '<dd><ul><li>a</li></ul></dd>');
  sx('(<([^>]+)>[^<>]*(?:</(?(R1\2)>|(?1)))', '<dd><ul><li>a</li></ul></dd>');
  sx('(?<name><([^>]+)>[^<>]*(?:</(?(R&name\2)>|(?&name)))',
    '<dd><ul><li>a</li></ul></dd>');
  sx('(?''name''<([^>]+)>[^<>]*(?:</(?(R&name)\2)>|(?&name))',
    '<dd><ul><li>a</li></ul></dd>');
  sx('(?''name''<([^>]+)>[^<>]*(?:</(?(R&name)\2)>|(?P>name))',
    '<dd><ul><li>a</li></ul></dd>');

  sx('(?x) \b (?&byte) (\.(?&byte)){3} \b (?(DEFINE (?<byte> 2[0-4]\d | 25[0-5] | 1\d\d | [1-9]?\d) ))',
    '127.0.01');

  sx('\p{Lk}', 'abc');
  sx('\p{Lk', 'abc');
  sx('\P{Lk}', 'abc');
  sx('\P{Lk', 'abc');
  sx('\pK', 'abc');

  sx('(?<(c|d)*)[ab]', 'a');
  sx('(?<(c|d)+)[ab]', 'a');
  sx('(?<(c|d){1,30})[ab]', 'a');
  sx('(?<(c|d*))[ab]', 'a');
  sx('(?<(c|d+))[ab]', 'a');
  sx('(?<(c|d{1,30}))[ab]', 'a');
  sx('(?<(c*|d))[ab]', 'a');
  sx('(?<(c+|d))[ab]', 'a');
  sx('(?<(c{1,30}|d))[ab]', 'a');
  sx('(?<(c*|d*))[ab]', 'a');
  sx('(?<(c+|d+))[ab]', 'a');
  sx('(?<(c{1,30}|d{1,30}))[ab]', 'a');

  sx('(?<!(c|d)*)[ab]', 'a');
  sx('(?<!(c|d)+)[ab]', 'a');
  sx('(?<!(c|d){1,30})[ab]', 'a');
  sx('(?<!(c|d*))[ab]', 'a');
  sx('(?<!(c|d+))[ab]', 'a');
  sx('(?<!(c|d{1,30}))[ab]', 'a');
  sx('(?<!(c*|d))[ab]', 'a');
  sx('(?<!(c+|d))[ab]', 'a');
  sx('(?<!(c{1,30}|d))[ab]', 'a');
  sx('(?<!(c*|d*))[ab]', 'a');
  sx('(?<!(c+|d+))[ab]', 'a');
  sx('(?<!(c{1,30}|d{1,30}))[ab]', 'a');

  //3.0 add
  sx('(?imsx-a)abc', 'abc');
  sx('(?imsx-d)abc', 'abc');
  sx('(?imsx-l)abc', 'abc');
  sx('(?imsx-u)abc', 'abc');

  sx('(?ims-ax)abc', 'abc');
  sx('(?ims-dx)abc', 'abc');
  sx('(?ims-lx)abc', 'abc');
  sx('(?ims-ux)abc', 'abc');

  sx('(?-aimsx)abc', 'abc');
  sx('(?-dimsx)abc', 'abc');
  sx('(?-limsx)abc', 'abc');
  sx('(?-uimsx)abc', 'abc');

  sx('(?i-amsx)abc', 'abc');
  sx('(?i-dmsx)abc', 'abc');
  sx('(?i-lmsx)abc', 'abc');
  sx('(?i-umsx)abc', 'abc');

  sx('(?^-).*abc', #$000A'abc');
  sx('(?^-i).*abc', #$000A'abc');
  sx('(?^-m).*abc', #$000A'abc');

  sx('(?aaa)abc', 'abc');
  sx('(?au)abc', 'abc');
  sx('(?du)abc', 'abc');
  sx('(?lu)abc', 'abc');
  sx('(?alu)abc', 'abc');

  sx('(?<=(a*)\1)b', 'aab');
end;

procedure CheckRegExp(const Expression, Source: REString; APos, ALen: Integer;
  ASubLen: array of Integer);
var
  R: TSkRegExp;
  I: Integer;
  K: Integer;
begin
  R := TSkRegExp.Create;
  try
    R.Expression := Expression;
    if R.Exec(Source) then
    begin
      if (APos = R.Groups[0].Index) and (ALen = R.Groups[0].Length) then
      begin
        for I := Low(ASubLen) to High(ASubLen) do
        begin
          K := ASubLen[I];
          if K <> R.Groups[I + 1].Length then
            raise Exception.Create(Source + ':' + Expression + ':' +
              'SubExp not found');
        end;
      end
      else
        raise Exception.Create(Source + ':' + Expression + ':' +
          'Match Pos or Lenght Error');
    end
    else
      raise Exception.Create(Source + ':' + Expression + ':' + 'not found');
  finally
    R.Free;
  end;
end;

procedure Test(ADest: TStrings);

  procedure x2(const Expression, Source: REString; Start, Len: Integer;
    AOptions: TREOptions = []; AMin: Integer = 0; AMax: Integer = 0);
  var
    R: TSkRegExp;
    Succ: Boolean;
  begin
    try
      R := TSkRegExp.Create;
      try
        R.Expression := Expression;
        R.Options := AOptions;
        if R.Exec(Source) then
        begin
          Succ := (R.Groups[0].Index = Start) and (R.Groups[0].Length = Len);

          {$IFDEF SKREGEXP_DEBUG}
          if AMin > 0 then
            Succ := R.MinMatchLength = AMin;
          {$ENDIF SKREGEXP_DEBUG}
        end
        else
          Succ := False;
      finally
        R.Free;
      end;
      if not Succ then
        ADest.Add('(x2):''' + Expression + ''', ''' + Source + '''');
    except
      on E: ESkRegExp do
        ADest.Add('(x2):''' + Expression + ''', ''' + Source + '''');
    end;
  end;

  procedure x3(const Expression, Source: REString; Start, Last, SubLen: Integer;
    AOptiosn: TREOptions = []);
  var
    R: TSkRegExp;
    ret: Boolean;
  begin
    try
      R := TSkRegExp.Create;
      try
        R.Expression := Expression;
        R.Options := AOptiosn;

        if R.Exec(Source) then
          ret := (R.Groups[SubLen].Index = Start) and
            (R.Groups[SubLen].Length = Last)
        else
          ret := False;
      finally
        R.Free;
      end;
      if not ret then
        ADest.Add('(x3):''' + Expression + ''', ''' + Source + '''');
    except
      on E: ESkRegExp do
        ADest.Add('(x3):''' + Expression + ''', ''' + Source + '''');
    end;
  end;
  procedure ug2(const Expression, Source: REString; Start, Len: Integer;
    AOptions: TREOptions = []);
  var
    R: TSkRegExp;
    Succ: Boolean;
  begin
    try
      R := TSkRegExp.Create;
      try
        R.Expression := Expression;
        R.Options := AOptions;
        if R.Exec(Source) then
          Succ := (R.Groups[0].Index = Start) and (R.Groups[0].Length = Len)
        else
          Succ := False;
      finally
        R.Free;
      end;
      if not Succ then
        ADest.Add('(ug2):''' + Expression + ''', ''' + Source + '''');
    except
      on E: ESkRegExp do
        ADest.Add('(ug2):''' + Expression + ''', ''' + Source + '''');
    end;
  end;

  procedure p(const Expression, Source: REString; Position, Start, Len: Integer;
    AOptions: TREOptions = []);
  var
    R: TSkRegExp;
    Succ: Boolean;
  begin
    try
      R := TSkRegExp.Create;
      try
        R.Expression := Expression;
        R.InputString := Source;
        R.Options := AOptions;
        if R.ExecPos(Position) then
          Succ := (R.Groups[0].Index = Start) and (R.Groups[0].Length = Len)
        else
          Succ := False;
      finally
        R.Free;
      end;
      if not Succ then
        ADest.Add('(p):''' + Expression + ''', ''' + Source + '''');
    except
      on E: ESkRegExp do
        ADest.Add('(p):''' + Expression + ''', ''' + Source + '''');
    end;
  end;

// procedure m(const Expression, Source, Modifier: REString; Start, Len: Integer);
// var
// r: TSkRegExp;
// Succ: Boolean;
// begin
// try
// r := TSkRegExp.Create;
// try
// r.Expression := Expression;
// r.ModifierStr := Modifier;
// if r.Exec(Source) then
// Succ := (r.Groups[0].Index = Start) and (r.Groups[0].Length = Len)
// else
// Succ := False;
// finally
// r.Free;
// end;
// if not Succ then
// ADest.Add('(m):"' + Expression + '", "' + Modifier + '", ' + Source + '"');
// except
// on E: ESkRegExp do
// ADest.Add('(m):"' + Expression + '", "'  + Modifier + '", '+ Source + '"');
// end;
// end;

  procedure n(const Expression, Source: REString; AOptions: TREOptions = [];
    pref: REString = 'n');
  var
    R: TSkRegExp;
    ret: Boolean;
  begin
    try
      R := TSkRegExp.Create;
      try
        R.Expression := Expression;
        R.Options := AOptions;
        ret := R.Exec(Source);
      finally
        R.Free;
      end;
      if ret then
        ADest.Add('(' + pref + '):''' + Expression + ''', ''' + Source + '''');
    except
      on E: ESkRegExp do;
      // ADest.Add('(n):"' + Expression + '", "' + Source + '"');
    end;
  end;

  procedure GlobalMatch;
  var
    R: TSkRegExp;
    Succ: Boolean;
    S: REString;
    n: Integer;
  begin
    Succ := False;
    try
      R := TSkRegExp.Create;
      try
        n := 1;
        S := 'a,b,c,d';
        R.Expression := '\G\w,?\s*';
        if R.Exec(S) then
        begin
          repeat
            if (n = 1) and (R.Groups[0].Strings = 'a,') then
            begin
              Succ := True;
            end
            else if (n = 2) and (R.Groups[0].Strings = 'b,') then
            begin
              Succ := True;
            end
            else if (n = 3) and (R.Groups[0].Strings = 'c,') then
            begin
              Succ := True;
            end
            else if (n = 4) and (R.Groups[0].Strings = 'd') then
            begin
              Succ := True;
            end
            else
            begin
              Succ := False;
              Break;
            end;

            Inc(n);
          until not R.ExecNext;
        end;
      finally
        R.Free;
      end;
      if not Succ then
        ADest.Add('(g): error"');
    except
      on E: ESkRegExp do
        ADest.Add('(g): error"');
    end;
  end;

  procedure NextMatch(AText, APattern: REString; Times: Integer);
  var
    R: TSkRegExp;
    C: Integer;
  begin
    R := TSkRegExp.Create;
    try
      C := 0;
      R.Expression := APattern;
      if R.Exec(AText) then
      begin
        repeat
          if not(R.Groups[0].Strings = APattern) then
            ADest.Add('(nextmatch):''' + APattern + ''' : ''' + AText + '''');
          Inc(C);
        until not R.ExecNext;
      end;

      if C <> Times then
        ADest.Add('(nextmatch):''' + APattern + ''' : ''' + AText + '''');
    finally
      R.Free;
    end;
  end;

  procedure pl(const Expression, Source: REString;
    Position, Size, Start, Len: Integer);
  var
    R: TSkRegExp;
    Succ: Boolean;
  begin
    try
      R := TSkRegExp.Create;
      try
        R.Expression := Expression;
        R.InputString := Source;
        if R.ExecPos(Position, Size) then
          Succ := (R.Groups[0].Index = Start) and (R.Groups[0].Length = Len)
        else
        begin
          if (Start = 0) and (Len = 0) then
            Succ := True
          else
            Succ := False;
        end;
      finally
        R.Free;
      end;
      if not Succ then
        ADest.Add('(p):''' + Expression + ''', ''' + Source + '''');
    except
      on E: ESkRegExp do
        ADest.Add('(p):''' + Expression + ''', ''' + Source + '''');
    end;
  end;

  procedure EnumRegExp(const Expression, Source: REString;
    ASubStr: array of REString);
  var
    R: TSkRegExp;
    I, J: Integer;
    SL: TREStringList;
  begin
    try
      SL := TREStringList.Create;
      try
        R := TSkRegExp.Create;
        try
          J := 0;
          SL.Text := Source;
          R.Expression := Expression;

          for I := 0 to SL.Count - 1 do
          begin
            if R.Exec(SL[I]) then
            begin
              if R.Groups[0].Strings <> ASubStr[J] then
                ADest.Add('(e):''' + Expression + ''', ''' + SL[I] + '''')
              else
                Inc(J);
            end;
          end;
        finally
          R.Free;
        end;
      finally
        SL.Free;
      end;
    except
      on E: ESkRegExp do
        ADest.Add('(e):''' + Expression + ''', ''' + Source + '''');
    end;
  end;

  procedure TestMatchAll(const Expression, Source: REString;
    ASubStr: array of String);
  var
    R: TSkRegExp;
    I: Integer;
  begin
    R := TSkRegExp.Create;
    try
      try
      R.Expression := Expression;
      R.InputString := Source;
      if R.ExecPos(1) then
      begin
        I := 0;
        repeat
          if R.Groups[0].Strings <> ASubStr[I] then
          begin
            ADest.Add('(e):''' + Expression + ''', ''' + ASubStr[I] + '''')
          end;
          Inc(I);
        until not R.ExecNext;
      end;
      except
        on E: ESkRegExp do
        ADest.Add('(e):''' + Expression + ''', ''' + Source + '''');
      end;
    finally
      R.Free;
    end;
  end;

  procedure PropertyTest(ADest: TStrings);
  var
    R: TSkRegExp;
  begin
    R := TSkRegExp.Create;
    try
      R.Expression := 'ABC';
      R.IgnoreCase := True;
      if not R.Exec('abc') then
        ADest.Add('not IgnoreCase');

      R.Expression := 'abc';
      R.IgnoreCase := True;
      if not R.Exec('ABC') then
        ADest.Add('not IgnoreCase');
      R.IgnoreCase := False;

{$IFNDEF NoUSEJapaneseExt}
      R.Expression := 'イロハ';
      R.IgnoreKana := True;
      if not R.Exec('いろは') then
        ADest.Add('not IgnoreKana');

      R.Expression := 'いろは';
      R.IgnoreKana := True;
      if not R.Exec('イロハ') then
        ADest.Add('not IgnoreKana');
      R.IgnoreKana := False;

      R.Expression := 'ABC';
      R.IgnoreWidth := True;
      if not R.Exec('ＡＢＣ') then
        ADest.Add('not IgnoreWidth');

      R.Expression := 'ＡＢＣ';
      R.IgnoreWidth := True;
      if not R.Exec('ABC') then
        ADest.Add('not IgnoreWidth');
      R.IgnoreWidth := False;
{$ENDIF NoUSEJapaneseExt}

      R.Expression := '^\d+';
      R.MultiLine := True;
      if not R.Exec('abc'#13#10'123') then
        ADest.Add('not Multiline');
      R.MultiLine := False;

      R.Expression := '(ab|cd)(?<n>ef)';
      R.NamedGroupOnly := True;
      if not R.Exec('cdef') then
        ADest.Add('not NamedGroupOnly');

      if R.Groups[1].Strings <> 'ef' then
        ADest.Add('not NamedGroupOnly');
      R.NamedGroupOnly := False;

      R.Expression := '^.*$';
      R.SingleLine := True;
      if not R.Exec('abc'#13#10'123') then
        ADest.Add('not SingleLine');

      if R.Groups[0].Length <> 8 then
        ADest.Add('not SingleLine');
    finally
      R.Free;
    end;
  end;

  procedure CheckEnumCapture(const RE, Text: REString; p: array of Integer;
    ADest: TStrings);
  var
    R: TSkRegExp;
    I, n: Integer;
    Succ: Boolean;
  begin
    Succ := False;
    try
      R := TSkRegExp.Create;
      try
        R.Expression := RE;
        if R.Exec(Text) then
        begin
          I := 0;
          n := 0;
          while n <= High(p) do
          begin
            Succ := (R.Groups[I].Index = p[n]) and
              (R.Groups[I].Length = p[n + 1]);
            if not Succ then
              Break;

            Inc(I);
            Inc(n, 2);
          end;
        end;
      finally
        R.Free;
      end;
      if not Succ then
        ADest.Add('(cec):''' + RE + ''', ''' + Text + '''');
    except
      on E: ESkRegExp do
        ADest.Add('(cec):''' + RE + ''', ''' + Text + '''');
    end;
  end;

  procedure lx2(const Expression, Source: REString; Start, Len: Integer;
    AOptions: TREOptions = []);
  var
    r: TSkRegExp;
    Succ: Boolean;
  begin
    r := TSkRegExp.Create;
    try
      r.Expression := Expression;
      r.DefinedCharClassLegacy := True;
      r.Exec(Source);
      Succ := r.Success and
        (r.Groups[0].Index = Start) and (r.Groups[0].Length = Len);

      if not Succ then
        ADest.Add('(lx2):''' + Expression + ''', ''' + Source + '''');

    finally
      r.Free;
    end;
  end;

var
  rep: TTestSkRegExp;
begin
  StackDepth := 0;

{$IFDEF SYNTAX_CHECK}
  SyntaxCheck(ADest);
{$ENDIF}

  rep := TTestSkRegExp.Create;
  try
    rep.Exec(ADest);
  finally
    rep.Free;
  end;

  PropertyTest(ADest);

  CheckEnumCapture('()(a\Kb)', 'ab', [2, 1, 1, 0, 1, 2], ADest);
  CheckEnumCapture('(?=ab\K)ab', 'ab', [3, 0], ADest);
  CheckEnumCapture('(?<=\Ka)b', 'ab', [1, 2], ADest);

  CheckRegExp('^(.*)(cat)(.*)$', 'the cat in the hat', 1, 18,
    [Length('the '), Length('cat'), Length(' in the hat')]);
  CheckRegExp('^(.*)(at)(.*)$', 'the cat in the hat', 1, 18,
    [Length('the cat in the h'), Length('at')]);
  CheckRegExp('^(.+)(e|r)(.*)$', 'The programming republic of Perl', 1, 32,
    [30, 1, 1]);
  CheckRegExp('(m{1,2}?)(.*?)$', 'The programming republic of Perl', 11, 22,
    [1, Length('ming republic of Perl')]);
  CheckRegExp('(.*?)(m{1,2}?)(.*)$', 'The programming republic of Perl', 1, 32,
    [Length('The progra'), 1, Length('ming republic of Perl')]);
  CheckRegExp('(.??)(m{1,2})(.*)$', 'The programming republic of Perl', 10,
    Length('amming republic of Perl'), [1, 2, Length('ing republic of Perl')]);

  TestMatchAll('^\d', '12345'#$000D#$000A'6789', ['1']);
  TestMatchAll('(?m)^\d', '12345'#$000D#$000A'6789', ['1', '6']);

  EnumRegExp('^(?=.*ab)(?=.*bc)(?=.*cd).*$',
    'abcdefg'#13#10'acbdefg'#13#10'ab'#13#10'bc'#13#10'cdxxabxxbc'#13#10'abc'#13#10'xxabcdefgxx',
    ['abcdefg', 'cdxxabxxbc', 'xxabcdefgxx']);

  EnumRegExp('^(?=.*?ab)(?=.*?bc)(?=.*?cd).*$',
    'abcdefg'#13#10'acbdefg'#13#10'ab'#13#10'bc'#13#10'cdxxabxxbc'#13#10'abc'#13#10'xxabcdefgxx',
    ['abcdefg', 'cdxxabxxbc', 'xxabcdefgxx']);
  EnumRegExp('^(?!.*ab)(?!.*bc)(?=.*cd).*$',
    'abcdefg'#13#10'acbdefg'#13#10'ab'#13#10'bc'#13#10'cd'#13#10'cdxxabxxbc'#13#10'abcd'#13#10'cdab'#13#10'axbxcdxxx',
    ['cd', 'axbxcdxxx']);
  EnumRegExp('^(?!(?=.*(ab|bc)))(?=.*cd).*$',
    'abcdefg'#13#10'acbdefg'#13#10'ab'#13#10'bc'#13#10'cd'#13#10'cdxxabxxbc'#13#10'abcd'#13#10'cdab'#13#10'axbxcdxxx',
    ['cd', 'axbxcdxxx']);
  EnumRegExp('^.*\.(?!jpg|gif|bmp).*$',
    'C:/oraclesqlpuzzle/regex/aaa.jpg'#13#10'C:/oraclesqlpuzzle/regex/aaa.bmp'#13#10'C:/oraclesqlpuzzle/regex/aaa.gif'#13#10
    + 'C:/oraclesqlpuzzle/regex/aaa.txt'#13#10'C:/oraclesqlpuzzle/regex/aaa.zip'#13#10'C:/oraclesqlpuzzle/regex/aaa.cab'#13#10
    + 'C:/oraclesqlpuzzle/aaa.cab', ['C:/oraclesqlpuzzle/regex/aaa.txt',
    'C:/oraclesqlpuzzle/regex/aaa.zip', 'C:/oraclesqlpuzzle/regex/aaa.cab',
    'C:/oraclesqlpuzzle/aaa.cab']);
  EnumRegExp('^.*\.(?!jpg)(?!gif)(?!bmp).*$',
    'C:/oraclesqlpuzzle/regex/aaa.jpg'#13#10'C:/oraclesqlpuzzle/regex/aaa.bmp'#13#10'C:/oraclesqlpuzzle/regex/aaa.gif'#13#10
    + 'C:/oraclesqlpuzzle/regex/aaa.txt'#13#10'C:/oraclesqlpuzzle/regex/aaa.zip'#13#10'C:/oraclesqlpuzzle/regex/aaa.cab'#13#10
    + 'C:/oraclesqlpuzzle/aaa.cab', ['C:/oraclesqlpuzzle/regex/aaa.txt',
    'C:/oraclesqlpuzzle/regex/aaa.zip', 'C:/oraclesqlpuzzle/regex/aaa.cab',
    'C:/oraclesqlpuzzle/aaa.cab']);
  EnumRegExp('^(?!A{4,}).*$',
    'AAAABB'#13#10'AAABB'#13#10'AABB'#13#10'ABB'#13#10'BB'#13#10'BBB'#13#10'AC'#13#10'AAD'#13#10'AAAE'#13#10'AAAAF'#13#10'AAAAAG'#13#10'HAHAHAHAHA',
    ['AAABB', 'AABB', 'ABB', 'BB', 'BBB', 'AC', 'AAD', 'AAAE', 'HAHAHAHAHA']);
  EnumRegExp('^[^0-9]*[0-9]*',
    'abcdefg123hijk'#13#10'abcdefg'#13#10'123'#13#10'123abc123'#13#10'123456'#13#10'123abc123abc',
    ['abcdefg123', 'abcdefg', '123', '123', '123456', '123']);
  EnumRegExp('^([^X]*X){3}',
    'ABCXDXEFXGHXIJ'#13#10'ABCXDXEFXGHXIJ'#13#10'ABCXDXEFXGHXIJ'#13#10'XXDEFXGHXIJ'#13#10'XXXX',
    ['ABCXDXEFX', 'ABCXDXEFX', 'ABCXDXEFX', 'XXDEFX', 'XXX']);


  //
  x2('(.xz|...def(*SKIP)|.bc)f', 'pxzabcdefpxzabcdeff', 13, 7);
  x2('\w+！', '$$%%&！はいもの！', 7, 5);
  x2('([^ @]+)@([^ @]+)', '  3'#$08'3.'#$08'.2'#$08'2.'#$08'.  @'#$08'@H'#$08'Ho'#$08'om'#$08'me'#$08'e', 18, 14);
  x2('([a-zA-Z][a-zA-Z0-9]*)://([^ /]+)(/?[^ ]*)?|([^ @]+)@([^ @]+)',
    '  <http://sunsite.unc.edu/mdw/HOWTO/Kernel-HOWTO.html>(NDT    :   Kernel', 4, 55);

  // newline
  x2('(?m)(*CR)^abc$', '123'#$000D'abc', 5, 3);
  x2('(?m)(*CR)^123$', '123'#$000D'abc', 1, 3);
  x2('(?m)(*CR)abc\Z', '123'#$000D'abc', 5, 3);
  x2('(?m)(*LF)^abc$', '123'#$000A'abc', 5, 3);
  x2('(?m)(*LF)^123$', '123'#$000A'abc', 1, 3);
  x2('(?m)(*LF)abc\Z', '123'#$000A'abc', 5, 3);
  x2('(?m)(*CRLF)^abc$', '123'#$000D#$000A'abc', 6, 3);
  x2('(?m)(*CRLF)^123$', '123'#$000D#$000A'abc', 1, 3);
  x2('(?m)(*CRLF)abc\Z', '123'#$000D#$000A'abc', 6, 3);

  // 1.2.5 add
  x2('(?w)[1-5]+', 'ab123', 3, 3, [], 1);
  x2('(?wk)[ｱ-ｵ]+', 'abアオイ', 3, 3, [], 1);
  x2('(?k)[あ-お]+', 'abアオイ', 3, 3, [], 1);
  //

  // 1.4
  lx2('\s+', '  '#0009, 1, 3);
  x2('\s+', '  '#0009, 1, 3);
  x2('[a-z\d]+', 'a001', 1, 4);
  x2('[&0-9\d]+', '!01&', 2, 3);
  x2('[&0-9p]+', '!01&', 2, 3);

  x2('\pP+', '&!?', 1, 3);
  x2('[@a-z\d\pP]+', '@!a001', 1, 6);
  x2('[@a-z\d\pP\s\v\h]+', '@!a001', 1, 6);
  x2('[@!a-z\d\s\v\h]+', '@!a 01', 1, 6);
  x2('[@a-z\d\p{Pc}\p{Po}\s\v\h]+', '@!a 001', 1, 7);

  // simple
  x2('.hello', '恋をしたら言うのかhello', 9, 6);
  x2('hello.*', '恋をしたら言うのかhelloと', 10, 6);
  x2('(?i).hello', '恋をしたら言うのかhello', 9, 6);
  x2('(?i)hello.*', '恋をしたら言うのかhelloと', 10, 6);
  x2('(?w).ハロー', '恋をしたら言うのかﾊﾛｰ', 9, 4);
  x2('(?w)ハロー.*', '恋をしたら言うのかﾊﾛｰと', 10, 4);
  x2('(?k).ハロー', '恋をしたら言うのかはろー', 9, 4);
  x2('(?k)ハロー.*', '恋をしたら言うのかはろーと', 10, 4);
  n('(?k)ハロー.*', '恋をしたら言うのかはろ');

  x2(#$1041'しました'#$0041, #$0041'これ'#$1041'しました'#$0041, 4, 6);

  n('.hello', '恋をしたら言うのかhellt');
  n('hello.*', '恋をしたら言うのかhell');
  n('(?k)ハロー.*', '恋をしたら言うのかはろ');

  x2('^(?=.*abc)(?!(.*abc){2})(?=.*def)(?!(.*def){2})(?=.*ghi)(?!(.*ghi){2}).*abc.*def.*ghi.*$',
    'abcdefghi', 1, 9);

  x2('(?<=hijelm|abc|defg).*def.*ghi.*', 'hijklabcdefghi', 9, 6);
  x2('(?<=hijelm|abc|defg).*def.*ghi.*', 'acbhijelmdefghi', 10, 6);
  x2('(?<!hijelm|abc|xydef).*defghi.*', 'abcdefghi', 1, 9);
  n('abc(?<!hijelm|abc|xydef).*defghi.*', 'abcdefghi');

  x2('\n', '12345'#0013#0010, 7, 1);


  // 1.6
  x2('(...def|.bc)', 'abcdef', 1, 6);

  x2('b{1,3}[ahi]', 'bbba', 1, 4, [], 2, 4);
  x2('b{1,3}[abhi]', 'bbba', 1, 4);
  x2('\w+！', 'わははは！', 1, 5);
  x2('\d+！', '１２３４！', 1, 5);
  x2('\s+！', '　　　　！', 1, 5);
  x2('\h+！', #0009#0009#0009#0009'！', 1, 5);
  x2('\v+！', #0013#0013#0013#0013'！', 1, 5);

  x2('(?aia)ffi', TSkRegExp.DecodeEscape('\x{FB00}\x{FB01}ffi'), 3, 3);

  n('(?m)\d{3,5}$', 'sns'#$000D#$000A'123e45'#$000D#$000A);
  x2('(?m)\d{3,5}$', 'sns'#$000D#$000A'12345'#$000D#$000A, 6, 5);

//add

  x2('(菌|季節|方法|年間|年|活動|事業|大学|試合|情|生涯|奉仕|仕事|株|雇用|歩くこと|買収|対応|製作|トーク|利用|の|実践|四季|対話|生涯|業務|食材|実習|実務|保障|本業|発表|教育|行動)',
    '正規化保障', 4, 2);
  x2('(?<=側を|面を|高さを|材を|山を|丘を|谷を|峠を|野を|国境を|坂を|冬を|春を|夏を|秋を|権限を|乗り|飛び|時を|壁を|境界を|障害を|党派を|垣根を|板を|点を|出部|それを|紙を|周囲を|部分を)(こえ|超え)',
    '銀河の境界をこえた', 7, 2);

  x2('([\x{30A0}-\x{30FF}\x{3040}-\x{309F}\x{3005}-\x{3007}\x{303B}\x{3400}-\x{9FFF}\x{F900}-\x{FAFF}\x{20000}-\x{2FFFF}]{1,5}?)\1+',
    '連続マイマイテストA', 3, 4);

  n('[^\w-]\d{4}-\d{2}-\d{6}', ' 8051-abffdhksa fhdlsakfdjksalfdsa');


  x2('a\1', 'a'#0001, 1, 2);
  x2('(?:ab)\9', 'ab9', 1, 3);
  x2('(?is:ab)\1', 'ab'#0001, 1, 3);
  x2('(?P<n>ab)\1', 'abab', 1, 4);

  x2('(?i)ffiffl|xyz|ppp', 'abcdef'#$FB03#$FB04, 7, 2);
  x2('(?i)\x{FB03}\x{FB04}|xyz|ppp', 'abcdefffiffl', 7, 6);
  x2('(?i)\x{DF}|ab|de', 'stss', 3, 2);
  x2('(?i)ss|ab|de', 'st'#$00DF, 3, 1);
  x2('[\s\v\h\w]+', 'Do you'#0009'know'#$000B'me?', 1, 14);


  x2('bab|a', 'ba', 2, 1);
  x2('(?i:ba)|B', 'B', 1, 1);
  x2('(?i:a)|b', 'A', 1, 1);

  x2('(\w)?((?i:abc)|dfb|geg)\1b', 'aABCab', 1, 6);
  x2('(\w)?((?i:ABC)|dfb|geg)\1b', 'aABCab', 1, 6);
  x2('(\w)?((?i:ABC)|dfb|geg)\1b', 'aabcab', 1, 6);

  x2('(?<n>.)(?<n>.)(?<n>.)\k<n>', 'abca', 1, 4);
  x3('(?<n>.)(?<n>.)(?<n>.)\k<n>', 'abca', 1, 1, 1);

  x2('[[:alnum:]]+', '01ab', 1, 4);
  x2('[[:alpha:]]+', '01ab', 3, 2);
  x2('[[:ascii:]]+', #$000D#$000A, 1, 2);
  x2('[[:blank:]]+', #0009'　', 1, 2);
  x2('[[:cntrl:]]+', #0001#0003#$0019, 1, 3);
  x2('[[:digit:]]+', '0123', 1, 4);
  x2('[[:digit:]]+', '-9870', 2, 4);
  x2('[[:xdigit:]]+', '0ABf', 1, 4);
  x2('[[:punct:]]+', ',.。、', 1, 4);
  x2('[[:blank:]]+', #0009'　　', 1, 3);
  x2('[[:space:]]+', #0009'　　', 1, 3);
  n('[[:space:]]+', #$0085);
  x2('[[:spaceperl:]]+', ' '#$0085, 1, 2);
  x2('[[:spaceperl:]]+', #$0009' '#$2028#$2029#$0085, 1, 5);
  n('[[:spaceperl:]]+', #$000B);
  x2('[[:cntrl:]]+', #0009#0007#0003, 1, 3);
  x2('[[:graph:]]+', '123', 1, 3);
  x2('[[:print:]]+', 'abc123', 1, 6);

  x2('[[:^alnum:]]+', '<!?>', 1, 4);
  x2('[[:^alpha:]]+', 'ab01', 3, 2);
  x2('[[:^ascii:]]+', '自分', 1, 2);
  x2('[[:^blank:]]+', 'we', 1, 2);
  x2('[[:^cntrl:]]+', '&go', 1, 3);
  x2('[[:^digit:]]+', 'ZyxR', 1, 4);
  x2('[[:^xdigit:]]+', 'GHOK', 1, 4);
  x2('[[:^punct:]]+', '拝啓貴社', 1, 4);
  x2('[[:^blank:]]+', '空白？', 1, 3);
  x2('[[:^space:]]+', '空白？', 1, 3);
  x2('[[:^cntrl:]]+', '制御？', 1, 3);
  x2('[[:^graph:]]+', #0009#0007#0003, 1, 3);
  x2('[[:^print:]]+', #0001#0003#$0019, 1, 3);

  x2('\p{IsAlnum}+', '01ab', 1, 4);
  x2('\p{IsAlpha}+', '01ab', 3, 2);
  x2('\p{IsAscii}+', #$000D#$000A, 1, 2);
  x2('\p{IsBlank}+', #0009'　', 1, 2);
  x2('\p{Iscntrl}+', #0001#0003#$0019, 1, 3);
  x2('\p{Isdigit}+', '0123', 1, 4);
  x2('\p{Isdigit}+', '-9870', 2, 4);
  x2('\p{Isxdigit}+', '0ABf', 1, 4);
  x2('\p{Ispunct}+', ',.。、', 1, 4);
  x2('\p{Isblank}+', #0009'　　', 1, 3);
  x2('\p{Isspace}+', #0009'　　', 1, 3);
  x2('\p{Iscntrl}+', #0009#0007#0003, 1, 3);
  x2('\p{Isgraph}+', '123', 1, 3);
  x2('\p{Isprint}+', 'abc123', 1, 6);

  x2('\p{Alnum}+', '01ab', 1, 4);

  x2('\P{Isalnum}+', '<!?>', 1, 4);
  x2('\P{Isalpha}+', 'ab01', 3, 2);
  x2('\P{Isascii}+', '自分', 1, 2);
  x2('\P{Isblank}+', 'we', 1, 2);
  x2('\P{Iscntrl}+', '&go', 1, 3);
  x2('\P{Isdigit}+', 'ZyxR', 1, 4);
  x2('\P{Isxdigit}+', 'GHOK', 1, 4);
  x2('\P{Ispunct}+', '拝啓貴社', 1, 4);
  x2('\P{Isblank}+', '空白？', 1, 3);
  x2('\P{Isspace}+', '空白？', 1, 3);
  x2('\P{Iscntrl}+', '制御？', 1, 3);
  x2('\P{Isgraph}+', #0009#0007#0003, 1, 3);
  x2('\P{Isprint}+', #0001#0003#$0019, 1, 3);

  x2('\p{Hiragana}+', '初めてのチュー', 2, 3);

  x2('\p{IsHiragana}+', '初めてのチュー', 2, 3);
  x2('\p{InHiragana}+', '初めてのチュー', 2, 3);

  x2('\p{L&}+', 'どうせ明日は Love you.', 8, 4);
  x2('\p{LC}+', 'どうせ明日は Love you.', 8, 4);

  x2('(ab)\g1', 'ccabab', 3, 4);
  x2('(ab)\g-1', 'ccabab', 3, 4);

  x2('(ab)\g{1}', 'ccabab', 3, 4);
  x2('(ab)\g{-1}', 'ccabab', 3, 4);

  x2('(?<n>ab)\g{n}', 'ccabab', 3, 4);

  x2('(?<n>ab)(?P>n)', 'ccabab', 3, 4);
  x2('(?<n>ab)(?&n)', 'ccabab', 3, 4);

  x2('\w*+\R', 'world'#0013#0010, 1, 7);

  x2('(?<n>ab)\g{1}', 'ccabab', 3, 4);
  x2('(?<n>ab)\g1', 'ccabab', 3, 4);
  x2('(?''n''ab)\g{1}', 'ccabab', 3, 4);
  x2('(?P<n>ab)\g{1}', 'ccabab', 3, 4);

  x2('(?<n>ab)\g{n}', 'ccabab', 3, 4);
  x2('(?''n''ab)\g{n}', 'ccabab', 3, 4);
  x2('(?P<n>ab)\g{n}', 'ccabab', 3, 4);

  x2('(x)(?<n>ab)\g{-1}', 'ccxabab', 3, 5);
  x2('(x)(?<n>ab)\g-1', 'ccxabab', 3, 5);
  x2('(x)(?''n''ab)\g{-1}', 'ccxabab', 3, 5);
  x2('(x)(?P<n>ab)\g{-1}', 'ccxabab', 3, 5);

  GlobalMatch;

  x2('(?x) \( ( (?>[^()]+) | (?R) )* \)', '(ab(cd)ef)', 1, 10);
  x2('(?x) (?<name_1>\( ( (?>[^()]+) | (?&name_1) )* \))', '(ab(cd)ef)', 1, 10);
  x3('(?x) (?<name_1>\( ( (?>[^()]+) | (?&name_1) )* \))',
    '(ab(cd)ef)', 8, 2, 2);

  x2('\p{IsHiragana}+', 'abc'#0013#0010'愛してる？', 7, 3);
  x2('\p{InHiragana}+', 'abc'#0013#0010'愛してる？', 7, 3);
  x2('\p{InBasicLatin}+', '123 Hello', 1, 9);

  // switch condition
  x2('(<a\s+[^>]+>)?<img\s+[^>]+>(?(1)<\/a>|<img>)',
    '<a href=''index.html''><img src=''image.gif''></a>', 1, 46);
  x2('(?<foo><a\s+[^>]+>)?<img\s+[^>]+>(?(<foo>)<\/a>|<img>)',
    '<a href=''index.html''><img src=''image.gif''></a>', 1, 46);

  p('(?(?<=NUM:)\d+|\w+)', 'NUM:123', 5, 5, 3);
  p('(?(?<=NUM:)\d+|\w+)', '123:NAO', 5, 5, 3);

  x2('(?(?<=NUM:)\d+|\w+)', '===:NAO', 5, 3);
  x2('(?x)  ( \( )?    [^()]+    (?(1) \) )', '(明日(10日)は晴れます)', 2, 2);

  x2('(?x) (?(?=[^a-z]*[a-z]) \d{2}-[a-z]{3}-\d{2}  |  \d{2}-\d{2}-\d{2} )',
    '00-tap-12', 1, 9);
  x2('(?x) (?(?=[^a-z]*[a-z]) \d{2}-[a-z]{3}-\d{2}  |  \d{2}-\d{2}-\d{2} )',
    '12-10-12', 1, 8);

  x2('<([^>]+)>[^<>]*(?:</(?(R)\1)>|(?R))',
    '<dd><ul><li>a</li></ul></dd>', 1, 18);
  x2('(<([^>]+)>[^<>]*(?:</(?(R1)\2)>|(?1)))',
    '<dd><ul><li>a</li></ul></dd>', 1, 18);
  x2('(?<name><([^>]+)>[^<>]*(?:</(?(R&name)\2)>|(?&name)))',
    '<dd><ul><li>a</li></ul></dd>', 1, 18);
  x2('(?''name''<([^>]+)>[^<>]*(?:</(?(R&name)\2)>|(?&name)))',
    '<dd><ul><li>a</li></ul></dd>', 1, 18);
  x2('(?''name''<([^>]+)>[^<>]*(?:</(?(R&name)\2)>|(?P>name)))',
    '<dd><ul><li>a</li></ul></dd>', 1, 18);

  x2('(?x) \b (?&byte) (\.(?&byte)){3} \b (?(DEFINE) (?<byte> 2[0-4]\d | 25[0-5] | 1\d\d | [1-9]?\d) )',
    '127.0.0.1', 1, 9);
  x3('(?x) \b (?&byte) (\.(?&byte)){3} \b (?(DEFINE) (?<byte> 2[0-4]\d | 25[0-5] | 1\d\d | [1-9]?\d) )',
    '127.0.0.1', 8, 2, 1);
  x2('(?x) (?(DEFINE) (?<byte> 2[0-4]\d | 25[0-5] | 1\d\d | [1-9]?\d) )  \b (?&byte) (\.(?&byte)){3} \b',
    '127.0.0.1', 1, 9);
  x3('(?x)(?(DEFINE) (?<byte> 2[0-4]\d | 25[0-5] | 1\d\d | [1-9]?\d) ) \b (?&byte) (\.(?&byte)){3} \b',
    '127.0.0.1', 8, 2, 2);

  // branch reset
  x3('(a)(?|x(y)z|(p(q)r)|(t)u(v))(z)', 'zzzaxyzz', 6, 1, 2);
  x3('(a)(?|x(y)z|(p(q)r)|(t)u(v))(z)', 'zzzapqrzz', 5, 3, 2);
  x3('(a)(?|x(y)z|(p(q)r)|(t)u(v))(z)', 'zzzatuvzz', 5, 1, 2);
  x3('(a)(?|x(y)z|(p(q)r)|(t)u(v))(z)', 'zzzatuvzz', 5, 1, 2);
  x3('(a)(?|x(y)z|(p(q)r)|(t)u(v))(z)', 'zzzatuvzz', 8, 1, 4);

  x2('(?i)PHP|perl|python', 'Do you know php or perl.', 13, 3);
  x2('(?i)php|perl|python', 'Do you know PHP or perl.', 13, 3);
  x2('(?aia)PHP|perl|python', 'Do you know php or perl.', 13, 3);
  x2('(?w:ｐｈｐ|perl|python)', 'Do you know php or perl.', 13, 3);

  x2('\X', 'A'#$030a, 1, 2);
  x2('\X*'#$1EB7, 'a'#$0323#$0306'a'#$0323#$0306'a'#$0323#$0306#$1EB7, 1, 10);

  x2('(?<=(a))b\1', 'aba', 2, 2);
  x2('(?x)\( ( (?>[^()]+) | \([^()]*\) )+ \)', 'abc(de(fg)h', 7, 4);

  x2('\G', '', 1, 0);
  x2('\Gaz', 'az', 1, 2);
  n('\Gz', 'bza');
  n('az\G', 'az');

  x2('(?=z)z', 'z', 1, 1);
  n('(?=z).', 'a');
  x2('(?!z)a', 'a', 1, 1);
  n('(?!z)a', 'z');

  // 強欲ここから

  x2('(a|b)*+', 'aaaab', 1, 5);

  x2('a?+', '', 1, 0);
  x2('a?+', 'b', 1, 0);
  x2('a?+', 'a', 1, 1);
  x2('a*+', '', 1, 0);
  x2('a*+', 'a', 1, 1);
  x2('a*+', 'aaa', 1, 3);
  x2('a*+', 'baaaa', 1, 0);
  n('a++', '');
  x2('a++', 'a', 1, 1);
  x2('a++', 'aaaa', 1, 4);
  x2('a++', 'aabbb', 1, 2);
  x2('a++', 'baaaa', 2, 4);
  x2('.?+', '', 1, 0);
  x2('.?+', 'f', 1, 1);
  x2('.?+', #$A, 1, 0);
  x2('.*+', '', 1, 0);
  x2('.*+', 'abcde', 1, 5);
  x2('.++', 'z', 1, 1);
  x2('.++', 'zdswer'#$A, 1, 6);

  x2('a?+|b', 'a', 1, 1);
  x2('a?+|b', 'b', 1, 0);
  x2('a?+|b', '', 1, 0);
  x2('a*+|b', 'aa', 1, 2);
  x2('a*+|b*+', 'ba', 1, 0);
  x2('a*+|b*', 'ab', 1, 1);
  x2('a+|b*+', '', 1, 0);
  x2('a++|b*+', 'bbb', 1, 3);
  x2('a++|b*+', 'abbb', 1, 1);
  n('a++|b++', '');
  x2('(a|b)?+', 'b', 1, 1);
  x2('(a|b)*+', 'ba', 1, 2);
  x2('(a|b)++', 'bab', 1, 3);
  x2('(ab|ca)++', 'caabbc', 1, 4);
  x2('(ab|ca)++', 'aabca', 2, 4);
  x2('(ab|ca)++', 'abzca', 1, 2);
  x2('(a|bab)++', 'ababa', 1, 5);
  x2('(a|bab)++', 'ba', 2, 1);
  x2('(a|bab)++', 'baaaba', 2, 3);

  x2('(?:a*+|b*+)(?:a*+|b*+)', 'aaabbb', 1, 3);
  x2('(?:a*+|b*+)(?:a++|b++)', 'aaabbb', 1, 6);
  x2('(?:a++|b++){2}+', 'aaabbb', 1, 6);
  x2('h{0,}+', 'hhhh', 1, 4);
  x2('(?:a++|b++){1,2}+', 'aaabbb', 1, 6);

  x2('(?:a{1,10}|b{1,10}){2}+', 'aaabbb', 1, 6);
  x2('(?:a{1,20}|b{1,20}){1,2}+', 'aaabbb', 1, 6);

  // UnGreedy
  ug2('a??', '', 1, 0);
  ug2('a??', 'b', 1, 0);
  ug2('a??', 'a', 1, 0);
  ug2('a*?', '', 1, 0);
  ug2('a*?', 'a', 1, 0);
  ug2('a*?', 'aaa', 1, 0);
  ug2('a*?', 'baaaa', 1, 0);
  n('a+', '');
  ug2('a+?', 'a', 1, 1);
  ug2('a+?', 'aaaa', 1, 1);
  ug2('a+?', 'aabbb', 1, 1);
  ug2('a+?', 'baaaa', 2, 1);
  ug2('.??', '', 1, 0);
  ug2('.??', 'f', 1, 0);
  ug2('.??', #$A, 1, 0);
  ug2('.*?', '', 1, 0);
  ug2('.*?', 'abcde', 1, 0);
  ug2('.+?', 'z', 1, 1);
  ug2('.+?', 'zdswer'#$A, 1, 1);
  ug2('(.*?)a\1f', 'babfbac', 1, 4);
  ug2('(.*?)a\1f', 'bacbabf', 4, 4);
  ug2('((.*?)a\2f)', 'bacbabf', 4, 4);
  ug2('(.*?)a\1f', 'baczzzzzz'#$A'bazz'#$A'zzzzbabf', 20, 4);
  ug2('a??|b', 'a', 1, 0);
  ug2('a??|b', 'b', 1, 0);
  ug2('a??|b', '', 1, 0);
  ug2('a*?|b', 'aa', 1, 0);
  ug2('a*?|b*?', 'ba', 1, 0);
  ug2('a*?|b*?', 'ab', 1, 0);
  ug2('a+?|b*?', '', 1, 0);
  ug2('a+?|b*?', 'bbb', 1, 0);
  ug2('a+?|b*?', 'abbb', 1, 1);
  n('a+?|b+?', '');
  ug2('(a|b)??', 'b', 1, 0);
  ug2('(a|b)*?', 'ba', 1, 0);
  ug2('(a|b)+?', 'bab', 1, 1);
  ug2('(ab|ca)+?', 'caabbc', 1, 2);
  ug2('(ab|ca)+?', 'aabca', 2, 2);
  ug2('(ab|ca)+?', 'abzca', 1, 2);
  ug2('(a|bab)+?', 'ababa', 1, 1);
  ug2('(a|bab)+?', 'ba', 2, 1);
  ug2('(a|bab)+?', 'baaaba', 2, 1);
  ug2('(?:a*?|b*?)(?:a*?|b*?)', 'aaabbb', 1, 0);
  ug2('(?:a*?|b*?)(?:a+?|b+?)', 'aaabbb', 1, 1);
  ug2('(?:a+?|b+?){2}?', 'aaabbb', 1, 2);
  ug2('h{0,}?', 'hhhh', 1, 0);
  ug2('(?:a+?|b+?){1,2}?', 'aaabbb', 1, 1);
  // n('ax{2}*a', '0axxxa1');
  n('a.{0,2}?a', '0aXXXa0');
  n('a.{0,2}a', '0aXXXa0');
  n('a.{0,2}a', '0aXXXXa0');
  ug2('^a{2,}a$', 'aaa', 1, 3);
  ug2('^[a-z]{2,}$', 'aaa', 1, 3);
  ug2('(?:a+?|\Ab*?)cc', 'cc', 1, 2);
  n('(?:a+?|\Ab*?)cc', 'abcc');
  ug2('(?:^a+?|b+?)*?c', 'aabbbabc', 7, 2);
  ug2('(?:^a+?|b+?)*?c', 'aabbbbc', 1, 7);
  ug2('[abc]??', 'abc', 1, 0);
  ug2('[abc]*?', 'abc', 1, 0);
  ug2('[^abc]*?', 'abc', 1, 0);
  n('[^abc]+?', 'abc');
  ug2('a?', 'aaa', 1, 1);
  ug2('ba?b', 'bab', 1, 3);
  ug2('a*', 'aaa', 1, 3);
  ug2('ba*', 'baa', 1, 3);
  ug2('ba*b', 'baab', 1, 4);
  ug2('a+', 'aaa', 1, 3);
  ug2('ba+', 'baa', 1, 3);
  ug2('ba+b', 'baab', 1, 4);
  ug2('(?:a??)?', 'a', 1, 0);
  ug2('(?:a??\?)??', 'a', 1, 0);
  ug2('(?:a??)+', 'aaa', 1, 0);
  ug2('(?:a+?)?', 'aaa', 1, 1);
  ug2('(?:a+?)?b', 'aaab', 1, 4);
  ug2('(?:ab){3,}?', 'abababab', 1, 6);
  n('(?:ab){3,}?', 'abab');
  ug2('(?:ab){2,4}?', 'ababab', 1, 4);
  ug2('(?:ab){2,4}?', 'ababababab', 1, 4);
  ug2('(?:ab){,}?', 'ab{,}', 1, 5);
  ug2('(?:X*?)(?i:xa)', 'XXXa', 1, 4);
  ug2('(d+?)([^abc]z)', 'dddz', 1, 4);
  ug2('([^abc]*?)([^abc]z)', 'dddz', 1, 4);
  ug2('(\w+?)(\wz)', 'dddz', 1, 4);


  // 強欲ここまで

  x2('a|\Gz', 'bza', 3, 1);
  x2('a|\Gz', 'za', 1, 1);

  x2('abc|(?=zz)..f', 'zzf', 1, 3);
  x2('abc|(?!zz)..f', 'abf', 1, 3);
  x2('(?=za)..a|(?=zz)..a', 'zza', 1, 3);

  x3('((?=az)a)', 'azb', 1, 1, 1);
  x2('(a)(?=\1)', 'aa', 1, 1);

  x2('(?<=a)b', 'ab', 2, 1);
  n('(?<=a)b', 'bb');
  x2('(?<=a|b)b', 'bb', 2, 1);
  x2('(?<=a|bc)b', 'bcb', 3, 1);
  x2('(?<=a|bc)b', 'ab', 2, 1);
  x2('(?<=a|bc||defghij|klmnopq|r)z', 'rz', 2, 1);

  x2('(?<!a)b', 'cb', 2, 1);
  n('(?<!a)b', 'ab');
  x2('(?<!a|bc)b', 'bbb', 1, 1);
  n('(?<!a|bc)z', 'bcz');
  x2('(?<name1>a)', 'a', 1, 1);

  x2('(a)\g<1>', 'aa', 1, 2);
  x3('(a)\g<1>', 'aa', 1, 1, 1);
  x2('(?<name_2>ab)\g<name_2>', 'abab', 1, 4);

  //gosub

  x2('^(.|(.)(?1)\2)$', 'abcba', 1, 5);
  x2('^(?<o>|.|(?:(?<i>.)\g<o>\k<i>))$', 'reer', 1, 4);

  x2('(?<n>|a\g<n>)+', '', 1, 0);
  x2('(?<n>|\(\g<n>\))+$', '()(())', 1, 6);

  x2('^(?<n>|\(\g<n>\))+$', '()(())', 1, 6, [roIgnoreWidth]);

  x3('\g<n>(?<n>.){0}', 'X', 0, 0, 1);
  x2('\g<n>(abc|df(?<n>.YZ){2,8}){0}', 'XYZ', 1, 3);
  x2('\A(?<n>(a\g<n>)|)\z', 'aaaa', 1, 4);

  x3('(z)()()(?<_9>a)\g<_9>', 'zaa', 2, 1, 4);

  x2('(?<foo>a|\(\g<foo>\))', 'a', 1, 1);
  x2('(?<foo>a|\(\g<foo>\))', '((((((a))))))', 1, 13);
  x3('(?<foo>a|\(\g<foo>\))', '((((((((a))))))))', 1, 17, 1);

  x2('(?<n>(a|b\g<n>c){3,5})', 'baaaaca', 2, 4);
  x3('(?<n>(a|b\g<n>c){3,5})', 'baaaaca', 2, 4, 1);
  x2('(?<n>(a|b\g<n>c){3,5})', 'baaaacaaaaa', 1, 10);
  x2('(?<pare>\(([^\(\)]++|\g<pare>)*+\))', '((a))', 1, 5);

  // nest level back reference
  x2('^(<(?:[^<>]+|\g<3>|\g<1>)*>)()(!>!>!>)$', '<<!>!>!>><>>!>!>!>', 1, 18);

  x2('\A(?<foo>abc...def)\g<foo>\z', 'abc123defabc123def', 1, 18);
  x3('\A(?<foo>abc...def)\g<foo>\z', 'abc123defabc456def', 1, 9, 1);
  x2('\A(?<foo>abc...def)\g<foo>\z', 'abc123defabc456def', 1, 18);
  n('\A(?<foo>abc...def)\g<foo>\z', 'abc123def');

  x2('\A(?<foo>abc\g<foo>|def)\z', 'def', 1, 3);
  x2('\A(?<foo>abc\g<foo>|def)\z', 'abcdef', 1, 6);
  x2('\A(?<foo>abc\g<foo>|def)\z', 'abcabcdef', 1, 9);
  n('\A(?<foo>abc\g<foo>|def)\z', 'abc');

  x2('\A(?<foo>\(\g<foo>*\))\z', '()', 1, 2);
  x2('\A(?<foo>\(\g<foo>*\))\z', '(()())', 1, 6);
  n('\A(?<foo>\(\g<foo>*\))\z', '(()()');

  x2('\A(?<foo>0\g<foo>0|1\g<foo>1|)\z', '00', 1, 2);
  x2('\A(?<foo>0\g<foo>0|1\g<foo>1|)\z', '0110', 1, 4);
  x2('\A(?<foo>0\g<foo>0|1\g<foo>1|)\z', '01000010', 1, 8);
  n('\A(?<foo>0\g<foo>0|1\g<foo>1|)\z', '0100001');

  // x2('\A(?<foo>(?<bar>.)\g<foo>|\k<bar-1>)\z', 'abcc', 1, 4);
  // x3('\A(?<foo>(?<bar>.)\g<foo>|\k<bar-1>)\z', 'abcc', 4, 1, 2);
  // n('\A(?<foo>(?<bar>.)\g<foo>|\k<bar-1>)\z', 'abca');
  // n('\A(?<foo>(?<bar>.)\g<foo>|\k<bar-1>)\z', 'abcb');
  //
  // x2('\A(?<foo>(?<bar>.)\g<foo>|\k<bar-2>)\z', 'abcb', 1, 4);
  // n('\A(?<foo>(?<bar>.)\g<foo>|\k<bar-2>)\z', 'abca');
  // n('\A(?<foo>(?<bar>.)\g<foo>|\k<bar-2>)\z', 'abcc');
  //
  // x2('\A(?<foo>(?<bar>.)\g<foo>|\k<bar-3>)\z', 'abca', 1, 4);
  // n('\A(?<foo>(?<bar>.)\g<foo>|\k<bar-3>)\z', 'abcb');
  // n('\A(?<foo>(?<bar>.)\g<foo>|\k<bar-3>)\z', 'abcc');
  //
  // x2('\A(?<foo>(?<bar>.)\g<foo>\k<bar+0>|)\z', '1234554321', 1, 10);
  // x2('\A(?<foo>(?<bar>.)\g<foo>\k<bar+0>|)\z', '123123321321', 1, 12);
  // n('\A(?<foo>(?<bar>.)\g<foo>\k<bar+0>|)\z', '123455432');
  //
  // x2('(?<xml><(?<elem>\w+)>\g<xml><\/\k<elem+0>>\g<xml>|\w*)',
  // '<a><b>text1</b>text2</a>', 1, 24);
  //
  // x2('\A(?<a>|.|(?:(?<b>.)\g<a>\k<b+0>))\z', 'reer', 1, 4);

  x2('^S(?<rec>S\g<rec>E\g<rec>|)E', 'SE', 1, 2);

  x2('^\([^()]*(?<rec>\([^()]*\g<rec>\)[^()]*\g<rec>|)\)', '(1+2+3)', 1, 7);
  x2('^\([^()]*(?<rec>\([^()]*\g<rec>\)[^()]*\g<rec>|)\)',
    '(1+2+(3+4)*5)+1+2', 1, 13);
  x2('^\([^()]*(?<rec>\([^()]*\g<rec>\)[^()]*\g<rec>|)\)',
    '(1+2+(3+4)*5)+1+2', 1, 13);

  x2('^\([^()]*(?<rec>\([^()]*\g<rec>\)[^()]*\g<rec>|)\)',
    '(1+2+(3+4)*5)+(1+2)*3', 1, 13);
  x2('^\([^()]*(?<rec>\([^()]*\g<rec>\)[^()]*\g<rec>|)\)',
    '(((1+2)*2)*3)+(1+2)', 1, 13);
  n('^\([^()]*(?<rec>\([^()]*\g<rec>\)[^()]*\g<rec>|)\)', '1+2+(1*5)');
  x2('^\([^()]*(?<rec>\([^()]*\g<rec>\)[^()]*\g<rec>|)\)',
    '(((-3))*(-9))+(555)', 1, 13);

  // x2('^(?<rec>(?<cap>.)(\g<rec>|.)?\k<cap+0>)$', 'ddd', 1, 3);
  // n('^(?<rec>(?<cap>.)(\g<rec>|.)?\k<cap+0>)$', 'abbb');
  // x2('^(?<rec>(?<cap>.)(\g<rec>|.)?\k<cap+0>)$', 'dddd', 1, 4);
  // x2('^(?<rec>(?<cap>.)(\g<rec>|.)?\k<cap+0>)$', 'abccba', 1, 6);
  // x2('^(?<rec>(?<cap>.)(\g<rec>|.)?\k<cap+0>)$', 'abcba', 1, 5);
  // x2('^(?<rec>(?<cap>.)(\g<rec>|.)?\k<cap+0>)$', 'abcdedcba', 1, 9);
  // x2('^(?<rec>(?<cap>.)(\g<rec>|.)?\k<cap+0>)$', '11', 1, 2);
  // x2('^(?<rec>(?<cap>.)(\g<rec>|.)?\k<cap+0>)$', '111', 1, 3);
  // x2('^(?<rec>(?<cap>.)(\g<rec>|.)?\k<cap+0>)$', '1111', 1, 4);
  // n('^(?<rec>(?<cap>.)(\g<rec>|.)?\k<cap+0>)$', 'tomato');
  // x2('^(?<rec>(?<cap>.)(\g<rec>|.)?\k<cap+0>)$', 'racecar', 1, 7);
  // x2('^(?<rec>(?<cap>.)(\g<rec>|.)?\k<cap+0>)$', 'wasitabarorabatisaw', 1, 19);
  // x2('^(?<rec>(?<cap>.)(\g<rec>|.)?\k<cap+0>)$', 'abcdeffedcba', 1, 12);
  // n('^(?<rec>(?<cap>.)(\g<rec>|.)?\k<cap+0>)$', 'abcdefghijk');
  // x2('^(?<rec>(?<cap>.)(\g<rec>|.)?\k<cap+0>)$', 'abcdefedcba', 1, 11);
  //
  // x2('^(?<rec>(?<cap>.)(\g<rec>)?\k<cap+0>)$', 'abccba', 1, 6);

  //

  x2('(?<ほげ>変|\(\g<ほげ>\))', '((((((変))))))', 1, 13);

  x2('\Gぽぴ', 'ぽぴ', 1, 2);
  n('\Gえ', 'うえお');
  n('とて\G', 'とて');

  x2('(?=せ)せ', 'せ', 1, 1);
  n('(?=う).', 'い');
  x2('(?!う)か', 'か', 1, 1);
  n('(?!と)あ', 'と');

  x2('十|\G区', 'け区十', 3, 1);
  x2('十|\G区', '区十', 1, 1);

  x2('あいう|(?=けけ)..ほ', 'けけほ', 1, 3);
  x2('あいう|(?!けけ)..ほ', 'あいほ', 1, 3);
  x2('(?=をあ)..あ|(?=をを)..あ', 'ををあ', 1, 3);
  x2('(?<=あ|いう)い', 'いうい', 3, 1);

  x3('((?=あん)あ)', 'あんい', 1, 1, 1);

  x2('\h', #9, 1, 1);
  x2('\H', 'a', 1, 1);
  n('\H', #9);

//  m('abc', 'ABC', 'i', 1, 3);
//  m('あいう', 'アイウ', 'k', 1, 3);
//  m('ABC', 'ＡＢＣ', 'w', 1, 3);
//  m('abc', 'ＡＢＣ', 'iw', 1, 3);
//  m('(a)(?<n>b)(c)\k<n>', 'abcb', 'n', 1, 4);
//  x2('(cc)\g+1(ab)', 'ccabab', 1, 6);
//  x2('(cc)\g{+1}(ab)', 'ccabab', 1, 6);

  x2('^.+\R', 'Hello'#0013#0010, 1, 7);
  x2('.*\R', 'Hello'#0013#0010'world', 1, 7);
  x2('[\R]', 'R', 1, 1);
  x2('abc\v*def', 'abc'#$2028#$000B#$000C'def', 1, 9);

  x2('(?<n>ab)\k<n>', 'ccabab', 3, 4);
  x2('(?<n>ab)\k''n''', 'ccabab', 3, 4);
  x2('(?''n''ab)\k''n''', 'ccabab', 3, 4);
  x2('(?P<n>ab)\k<n>', 'ccabab', 3, 4);

  x2('(?<n>ab)\k<1>', 'ccabab', 3, 4);
  x2('(?<n>ab)\k''1''', 'ccabab', 3, 4);
  x2('(?''n''ab)\k''1''', 'ccabab', 3, 4);
  x2('(?P<n>ab)\k<1>', 'ccabab', 3, 4);

  x2('a*\Kbc', 'aaabc', 4, 2);

  n('abc(?!)', 'abc');

  NextMatch('abc123abc456', 'abc', 2);

  TestMatchAll('(?m)^.*$',
    'abc'#0013#0010'123'#0013#0010'def'#0013#0010'456'#0013#0010,
    ['abc', '123', 'def', '456', '']);

  TestMatchAll('(?m)\#.+?\#|\$.+?\$|\r\n',
    'ああああ$いいいい$'#$0d#$0a'うううう#ええええ#'#$d#$0a'おおおお',
    ['$いいいい$', #0013#0010, '#ええええ#', #0013#0010]);

  pl('a*bc', 'aaabcaaabcaaabc', 1, 8, 1, 5);
  pl('a*bc', 'aaabcaaabcaaabc', 5, 8, 6, 5);
  pl('a*bc', 'aaabcaaabcaaabc', 10, 6, 11, 5);
  pl('a*bc', 'aaabcaaabcaaabc', 10, 7, 11, 5);
  pl('a*bc', 'aaabcaaabcaaabc', 10, 8, 11, 5);
  pl('a*bc', 'aaabcaaabcaaabc', 10, 9, 11, 5);

  pl('\bthe\b', 'Wethe, We are the world', 3, 20, 3, 3);
  pl('\bthe\b', 'We are ther world', 5, 6, 8, 3);
  pl('\Aファイル', 'ファイルの先頭をチェック', 1, 10, 1, 4);
  pl('\Aファイル', '先頭（ファイル）をチェック', 4, 10, 4, 4);
  pl('ファイル\Z', '末尾をチェックするファイル', 4, -1, 10, 4);
  pl('ファイル\Z', '末尾をチェックするファイルです。', 4, 10, 10, 4);
  pl('ファイル\z', '末尾をチェックするファイル'#0013#0010'です。', 4, 10, 10, 4);
  pl('^ファイル', 'ファイルの先頭をチェック', 1, 10, 1, 4);
  pl('先頭$', 'ファイルの先頭をチェック', 3, 5, 6, 2);

  x2('(?ikw:カタナ)+', 'カタナカタナ', 1, 6);
  x2('(?i:カタナ)+', 'カタナカタナ', 1, 6);
  x2('(?k:カタナ)+', 'カタナカタナ', 1, 6);
  x2('(?w:カタナ)+', 'カタナカタナ', 1, 6);

  x2('(?ikw:ＡＢＣ)+', 'ＡＢＣＡＢＣ', 1, 6);
  x2('(?i:ＡＢＣ)+', 'ＡＢＣＡＢＣ', 1, 6);
  x2('(?k:ＡＢＣ)+', 'ＡＢＣＡＢＣ', 1, 6);
  x2('(?w:ＡＢＣ)+', 'ＡＢＣＡＢＣ', 1, 6);

  n('(?<n>.)(?<n>.)(?<n>\k<n>c)', 'abbc');

  x2('X*X*X*X*X*X*X*X*X*X*XXXXXXXXXX', 'XXXXXXXXXX', 1, 10);
  X2('.*.*.*.*.*.*.*.*.*.*.*.*Z', 'ABCDEFGHIJKLMNOPZ', 1, 17);
  x2('a*a*a*a*a*a*a*a*a*a*a*a*a*a*aaaaaaaaaaaa', 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', 1, 31);


  x2('^anc', 'anc'#0013#0010'ank', 1, 3);
  x2('(?m)^anc', 'anc'#0013#0010'ank', 1, 3);
  x2('(?m)^anc', 'ank'#0013#0010'anc', 6, 3);
  x2('(?s)^anc', 'anc'#0013#0010'ank', 1, 3);
  n('(?s)^anc', 'ank'#0013#0010'anc');

  x2('\pL+', 'ccabab', 1, 6);
  x2('\PL+', 'abc'#0013#0010'def', 4, 2);

  x2('\p{L}+', 'ccabab', 1, 6);
  x2('\P{L}+', 'abc'#0013#0010'def', 4, 2);

  x2('\p{Ll}+', 'abc'#0013#0010'def', 1, 3);
  x2('\P{Ll}+', 'abc'#0013#0010'def', 4, 2);
  x2('\p{InUnifiedCanadianAboriginalSyllabics}+',
    'Hi, '#$1406#$1407#$1408#$14A0, 5, 4);

  x2('\p{Hiragana}+', '一発はたのしいか？', 3, 6);
  x2('\p{Katakana}+', 'それはパソコンじゃなきゃダメなのか？', 4, 4);

  x2('\p{C}+', #0001#0003#0004, 1, 3);

  x2('(?)', '?', 1, 0);
  n('a+a', 'a');

  x2('\x{2F844}', #$D87E#$DC44, 1, 2);
  x2('[\x{2F844}]', #$D87E#$DC44, 1, 2);
  x2('(?kwi)(abc)+', 'ＡＢＣコンピュータ', 1, 3);
  x2('(?kwi)ｺﾝﾋﾟｭｰﾀ', 'ＡＢＣコンピュータ', 4, 6);
  x2('(?k)(アイウエオ)+', 'あいうえお', 1, 5);
  x2('(?k)ウ', 'あいうえお', 3, 1);
  x2('(?kw)(ｶｷｸｹｺ)+', 'かきくけこ', 1, 5);
  x2('(?i)ＢＣ', 'ａｂｃｄｅ', 2, 2);
  x2('(?i)ＢＣ|ＥＦ', 'ａｂｃｄｅ', 2, 2);
  x2('(?w)カイ|ウエオ', 'ｱｲｳｴｵ', 3, 3);
  x2('(?k)カイ|ウエオ', 'あいうえお', 3, 3);

  x2('(?i)ＢＣ|ＥＦ|(?-i:k)', 'ａｂｃｄｅｋ', 2, 2);
  x2('(?i:ＢＣ|ＥＦ)*k', 'ａｂｃｄｅｆk', 5, 3);

  x2('\Q.*\E(a|b)+', 'abc.*bab', 4, 5);

  x2('[azm0-9]', 'cda', 3, 1);

  x2('(|a)*b', 'aaabb', 1, 4);
  x2('|a', 'a', 1, 0);
  x2('(|a)+$', 'aaa', 1, 3);
  x2('(a+|b+)*c', 'aaabbbc', 1, 7);

  x2('[\x00]', #0, 1, 1);
  x2('[\x00]zx', #0'zx', 1, 3);
  x2('\x00zx', #0'zx', 1, 3);
  x2('\x61\x62', 'ab', 1, 2);
  x2('(?i)\x41\x42', 'ab', 1, 2);
  x2('[a-z0-9]{4,20}', 'aaaaaaaaaaaaaaaaaaaaaa', 1, 20);
  // x2('\cあ', #0'あ', 1, 3);
  x2('\caab', #1'ab', 1, 3);
  x2('(?i)\caab', #1'ab', 1, 3);
  x2('\x{3000}', '　', 1, 1);
  // x2('\uあ', #0'あ', 0, 3);

  x2('', '', 1, 0);
  x2('^', '', 1, 0);
  x2('$', '', 1, 0);
  x2('\A', '', 1, 0);
  x2('\Z', '', 1, 0);
  x2('\z', '', 1, 0);
  x2('^$', '', 1, 0);
  x2('\ca', #01, 1, 1);
  x2('\cb', #02, 1, 1);
  x2('\c\\', #0028, 1, 1);
  x2('q[\c\\]', 'q'#0028, 1, 2);
  x2('', 'a', 1, 0);
  x2('a', 'a', 1, 1);
  x2('\x61', 'a', 1, 1);
  x2('aa', 'aa', 1, 2);
  x2('aaa', 'aaa', 1, 3);
  x2('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
    'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', 1, 35);
  x2('ab', 'ab', 1, 2);
  x2('b', 'ab', 2, 1);
  x2('bc', 'abc', 2, 2);
  x2('(?i:#RET#)', '#INS##RET#', 6, 5);
  x2('\17', #0015, 1, 1);
  x2('\x1f', #$1f, 1, 1);
  x2('\x{3000}', #$3000, 1, 1);
  x2('a(?#....\\JJJJ)b', 'ab', 1, 2);
  x2('(?x)  G (o O(?-x)oO) g L', 'GoOoOgLe', 1, 7);
  x2('.', 'a', 1, 1);
  n('.', '');
  x2('..', 'ab', 1, 2);
  x2('\w', 'e', 1, 1);
  n('\W', 'e');
  x2('\s', ' ', 1, 1);
  x2('\S', 'b', 1, 1);
  x2('\d', '4', 1, 1);
  n('\D', '4');

  x2('\v', #$000A, 1, 1);
  x2('\V', 'a', 1, 1);
  n('\V', #$000A);

  x2('\b', 'z ', 1, 0);
  x2('\b', ' z', 2, 0);
  x2('\B', 'zz ', 2, 0);
  x2('\B', 'z ', 3, 0);
  x2('\B', ' z', 1, 0);
  x2('[ab]', 'b', 1, 1);
  n('[ab]', 'c');
  x2('[a-z]', 't', 1, 1);
  n('[^a]', 'a');
  x2('[^a]', #$A, 1, 1);
  x2('[]]', ']', 1, 1);
  n('[^]]', ']');
  x2('[\^]+', '0^^1', 2, 2);
  x2('[b-]', 'b', 1, 1);
  x2('[b-]', '-', 1, 1);
  x2('[\w]', 'z', 1, 1);
  n('[\w]', ' ');
  x2('[\W]', 'b$', 2, 1);
  x2('[\d]', '5', 1, 1);
  n('[\d]', 'e');
  x2('[\D]', 't', 1, 1);
  n('[\D]', '3');
  x2('[\s]', ' ', 1, 1);
  n('[\s]', 'a');
  x2('[\S]', 'b', 1, 1);
  n('[\S]', ' ');
  x2('[\w\d]', '2', 1, 1);
  n('[\w\d]', ' ');
  x2('[[:upper:]]', 'B', 1, 1);
  x2('[*[:xdigit:]+]', '+', 1, 1);
  x2('[*[:xdigit:]+]', 'GHIKK-9+*', 7, 1);
  x2('[*[:xdigit:]+]', '-@^+', 4, 1);
  n('[[:upper:]]', 'a');
  x2('[[:^upper:]]', ':', 1, 1);
  x2('[\044-\047]', #0038, 1, 1);
  x2('[\x5a-\x5c]', #$5b, 1, 1);
  x2('[\x6A-\x6D]', #$6c, 1, 1);
  n('[\x6A-\x6D]', #$6E);
  n('^[0-9A-F]+ 0+ UNDEF ',
    '75F 00000000 SECT14A notype ()    External    | _rb_apply');
  x2('[\[]', '[', 1, 1);
  x2('[\]]', ']', 1, 1);
  x2('[&]', '&', 1, 1);
  // x2('[[ab]]', 'b', 1, 1);
  // x2('[[ab]c]', 'c', 0, 1);
  // n('[[^a]]', 'a');
  // n('[^[a]]', 'a');
  // x2('[[ab]&&bc]', 'b', 0, 1);
  // n('[[ab]&&bc]', 'a');
  // n('[[ab]&&bc]', 'c');
  // x2('[a-z&&b-y&&c-x]', 'w', 0, 1);
  // n('[^a-z&&b-y&&c-x]', 'w');
  // x2('[[^a&&a]&&a-z]', 'b', 0, 1);
  // n('[[^a&&a]&&a-z]', 'a');
  // x2('[[^a-z&&bcdef]&&[^c-g]]', 'h', 0, 1);
  // n('[[^a-z&&bcdef]&&[^c-g]]', 'c');
  // x2('[^[^abc]&&[^cde]]', 'c', 0, 1);
  // x2('[^[^abc]&&[^cde]]', 'e', 0, 1);
  // n('[^[^abc]&&[^cde]]', 'f');
  // x2('[a-&&-a]', '-', 0, 1);
  // n('[a\-&&\-a]', '&');
  n('\wabc', ' abc');
  x2('a\Wbc', 'a bc', 1, 4);
  x2('a.b.c', 'aabbc', 1, 5);
  x2('.\wb\W..c', 'abb bcc', 1, 7);
  x2('\s\wzzz', ' zzzz', 1, 5);
  x2('aa.b', 'aabb', 1, 4);
  n('.a', 'ab');
  x2('.a', 'aa', 1, 2);
  x2('^a', 'a', 1, 1);
  x2('^a$', 'a', 1, 1);
  x2('^\w$', 'a', 1, 1);
  n('^\w$', ' ');
  x2('^\wab$', 'zab', 1, 3);
  x2('^\wabcdef$', 'zabcdef', 1, 7);
  x2('^\w...def$', 'zabcdef', 1, 7);
  x2('\w\w\s\Waaa\d', 'aa  aaa4', 1, 8);
  x2('\A\Z', '', 1, 0);
  x2('\Axyz', 'xyz', 1, 3);
  x2('xyz\Z', 'xyz', 1, 3);
  x2('xyz\z', 'xyz', 1, 3);
  x2('a\Z', 'a', 1, 1);
  n('az\A', 'az');
  n('a\Az', 'az');
  x2('\^\$', '^$', 1, 2);
  x2('^x?y', 'xy', 1, 2);
  x2('^(x?y)', 'xy', 1, 2);
  x2('\w', '_', 1, 1);
  n('\W', '_');
  x2('(?i:a)', 'a', 1, 1);
  x2('(?i:a)', 'A', 1, 1);
  x2('(?i:A)', 'a', 1, 1);
  n('(?i:A)', 'b');
  x2('(?i:[A-Z])', 'a', 1, 1);
  x2('(?i:[f-m])', 'H', 1, 1);
  x2('(?i:[f-m])', 'h', 1, 1);
  n('(?i:[f-m])', 'e');
  x2('(?i:[A-c])', 'D', 1, 1);
  x2('(?i:[!-k])', 'Z', 1, 1);
  x2('(?i:[!-k])', '7', 1, 1);
  x2('(?i:[T-}])', 'b', 1, 1);
  x2('(?i:[T-}])', '{', 1, 1);
  x2('(?i:\?a)', '?A', 1, 2);
  x2('(?i:\*A)', '*a', 1, 2);
  n('.', #$A);
  x2('(?s:.)', #$A, 1, 1);
  x2('(?s:a.)', 'a'#$A, 1, 2);
  x2('(?s:.b)', 'a'#$A'b', 2, 2);
  x2('.*abc', 'dddabdd'#$A'ddabc', 9, 5);
  x2('(?m:.*abc)', 'dddabddabc', 1, 10);
  x2('(?m:^abc)', 'ddd'#$A'abdd'#$A'abc', 10, 3);
  n('(?i)(?-i)a', 'A');
  n('(?i)(?-i:a)', 'A');

  x2('a?', '', 1, 0);
  x2('a?', 'b', 1, 0);
  x2('a?', 'a', 1, 1);
  x2('a*', '', 1, 0);
  x2('a*', 'a', 1, 1);
  x2('a*', 'aaa', 1, 3);
  x2('a*', 'baaaa', 1, 0);
  n('a+', '');
  x2('a+', 'a', 1, 1);
  x2('a+', 'aaaa', 1, 4);
  x2('a+', 'aabbb', 1, 2);
  x2('a+', 'baaaa', 2, 4);
  x2('.?', '', 1, 0);
  x2('.?', 'f', 1, 1);
  x2('.?', #$A, 1, 0);
  x2('.*', '', 1, 0);
  x2('.*', 'abcde', 1, 5);
  x2('.+', 'z', 1, 1);
  x2('.+', 'zdswer'#$A, 1, 6);
  x2('(.*)a\1f', 'babfbac', 1, 4);
  x2('(.*)a\1f', 'bacbabf', 4, 4);
  x2('((.*)a\2f)', 'bacbabf', 4, 4);
  x2('(.*)a\1f', 'baczzzzzz'#$A'bazz'#$A'zzzzbabf', 20, 4);
  x2('(.+)a\1f', 'babfbac', 1, 4);
  x2('(.+)a\1f', 'bacbabf', 4, 4);
  x2('((.+)a\2f)', 'bacbabf', 4, 4);
  x2('(.+)a\1f', 'baczzzzzz'#$A'bazz'#$A'zzzzbabf', 20, 4);
  x2('a|b', 'a', 1, 1);
  x2('a|b', 'b', 1, 1);
  x2('|a', 'a', 1, 0);
  x2('(|a)', 'a', 1, 0);
  x2('ab|bc', 'ab', 1, 2);
  x2('ab|bc', 'bc', 1, 2);
  x2('z(?:ab|bc)', 'zbc', 1, 3);
  x2('a(?:ab|bc)c', 'aabc', 1, 4);
  x2('ab|(?:ac|az)', 'az', 1, 2);
  x2('a|b|c', 'dc', 2, 1);
  x2('a|b|cd|efg|h|ijk|lmn|o|pq|rstuvwx|yz', 'pqr', 1, 2);
  n('a|b|cd|efg|h|ijk|lmn|o|pq|rstuvwx|yz', 'mn');
  x2('a|^z', 'ba', 2, 1);
  x2('a|^z', 'za', 1, 1);
  x2('a|\Az', 'bza', 3, 1);
  x2('a|\Az', 'za', 1, 1);
  x2('a|b\Z', 'ba', 2, 1);
  x2('a|b\Z', 'b', 1, 1);
  x2('a|b\z', 'ba', 2, 1);
  x2('a|b\z', 'b', 1, 1);
  x2('\w|\s', ' ', 1, 1);
  n('\w|\w', ' ');
  x2('\w|%', '%', 1, 1);
  x2('\w|[&$]', '&', 1, 1);
  x2('[b-d]|[^e-z]', 'a', 1, 1);
  x2('(?:a|[c-f])|bz', 'dz', 1, 1);
  x2('(?:a|[c-f])|bz', 'bz', 1, 2);
  n('(?>a|abd)c', 'abdc');
  x2('(?>abd|a)c', 'abdc', 1, 4);
  x2('a?|b', 'a', 1, 1);
  x2('a?|b', 'b', 1, 0);
  x2('a?|b', '', 1, 0);
  x2('a*|b', 'aa', 1, 2);
  x2('a*|b*', 'ba', 1, 0);
  x2('a*|b*', 'ab', 1, 1);
  x2('a+|b*', '', 1, 0);
  x2('a+|b*', 'bbb', 1, 3);
  x2('a+|b*', 'abbb', 1, 1);
  n('a+|b+', '');
  x2('(a|b)?', 'b', 1, 1);
  x2('(a|b)*', 'ba', 1, 2);
  x2('(a|b)+', 'bab', 1, 3);
  x2('(ab|ca)+', 'caabbc', 1, 4);
  x2('(ab|ca)+', 'aabca', 2, 4);
  x2('(ab|ca)+', 'abzca', 1, 2);
  x2('(a|bab)+', 'ababa', 1, 5);
  x2('(a|bab)+', 'ba', 2, 1);
  x2('(a|bab)+', 'baaaba', 2, 3);
  x2('(?:a|b)(?:a|b)', 'ab', 1, 2);
  x2('(?:a*|b*)(?:a*|b*)', 'aaabbb', 1, 3);
  x2('(?:a*|b*)(?:a+|b+)', 'aaabbb', 1, 6);
  x2('(?:a+|b+){2}', 'aaabbb', 1, 6);
  x2('(?:a*|b*){2}', 'aaabbb', 1, 3);
  x2('(?:a*|b*){1}', 'aaabbb', 1, 3);
  x2('(?:a{0,}|b{0,}){2}', 'aaabbb', 1, 3);
  x2('h{0,}', 'hhhh', 1, 4);
  x2('(?:a+|b+){1,2}', 'aaabbb', 1, 6);
  n('ax{2}*a', '0axxxa1');
  n('a.{0,2}a', '0aXXXa0');
  n('a.{0,2}?a', '0aXXXa0');
  n('a.{0,2}?a', '0aXXXXa0');
  x2('^a{2,}?a$', 'aaa', 1, 3);
  x2('^[a-z]{2,}?$', 'aaa', 1, 3);
  x2('(?:a+|\Ab*)cc', 'cc', 1, 2);
  n('(?:a+|\Ab*)cc', 'abcc');
  x2('(?:^a+|b+)*c', 'aabbbabc', 7, 2);
  x2('(?:^a+|b+)*c', 'aabbbbc', 1, 7);
  x2('a|(?i)c', 'C', 1, 1);
  x2('(?i)c|a', 'C', 1, 1);
  x2('(?i)c|a', 'A', 1, 1);
  x2('(?i:c)|a', 'C', 1, 1);
  n('(?i:c)|a', 'A');
  x2('[abc]?', 'abc', 1, 1);
  x2('[abc]*', 'abc', 1, 3);
  x2('[^abc]*', 'abc', 1, 0);
  n('[^abc]+', 'abc');
  x2('a??', 'aaa', 1, 0);
  x2('ba??b', 'bab', 1, 3);
  x2('a*?', 'aaa', 1, 0);
  x2('ba*?', 'baa', 1, 1);
  x2('ba*?b', 'baab', 1, 4);
  x2('a+?', 'aaa', 1, 1);
  x2('ba+?', 'baa', 1, 2);
  x2('ba+?b', 'baab', 1, 4);
  x2('(?:a?)??', 'a', 1, 0);
  x2('(?:a?\?)?', 'a', 1, 0);
  x2('(?:a?)+?', 'aaa', 1, 1);
  x2('(?:a+)??', 'aaa', 1, 0);
  x2('(?:a+)??b', 'aaab', 1, 4);
  // x2('(?:ab)?{2}', '', 1, 0);
  // x2('(?:ab)?{2}', 'ababa', 1, 4);
  // x2('(?:ab)*{0}', 'ababa', 1, 0);
  x2('(?:ab){3,}', 'abababab', 1, 8);
  n('(?:ab){3,}', 'abab');
  x2('(?:ab){2,4}', 'ababab', 1, 6);
  x2('(?:ab){2,4}', 'ababababab', 1, 8);
  x2('(?:ab){2,4}?', 'ababababab', 1, 4);
  x2('(?:ab){,}', 'ab{,}', 1, 5);
  // x2('(?:abc)+?{2}', 'abcabcabc', 1, 6);
  x2('(?:X*)(?i:xa)', 'XXXa', 1, 4);
  x2('(d+)([^abc]z)', 'dddz', 1, 4);
  x2('([^abc]*)([^abc]z)', 'dddz', 1, 4);
  x2('(\w+)(\wz)', 'dddz', 1, 4);
  x3('(a)', 'a', 1, 1, 1);
  x3('(ab)', 'ab', 1, 2, 1);
  x2('((ab))', 'ab', 1, 2);
  x3('((ab))', 'ab', 1, 2, 1);
  x3('((ab))', 'ab', 1, 2, 2);
  x3('((((((((((((((((((((ab))))))))))))))))))))', 'ab', 1, 2, 20);
  x3('(ab)(cd)', 'abcd', 1, 2, 1);
  x3('(ab)(cd)', 'abcd', 3, 2, 2);
  x3('()(a)bc(def)ghijk', 'abcdefghijk', 4, 3, 3);
  x3('(()(a)bc(def)ghijk)', 'abcdefghijk', 4, 3, 4);
  x2('(^a)', 'a', 1, 1);
  x3('(a)|(a)', 'ba', 2, 1, 1);
  x3('(^a)|(a)', 'ba', 2, 1, 2);
  x3('(a?)', 'aaa', 1, 1, 1);
  x3('(a*)', 'aaa', 1, 3, 1);
  x3('(a*)', '', 1, 0, 1);
  x3('(a+)', 'aaaaaaa', 1, 7, 1);
  x3('(a+|b*)', 'bbbaa', 1, 3, 1);
  x3('(a+|b?)', 'bbbaa', 1, 1, 1);
  x3('(abc)?', 'abc', 1, 3, 1);
  x3('(abc)*', 'abc', 1, 3, 1);
  x3('(abc)+', 'abc', 1, 3, 1);
  x3('(xyz|abc)+', 'abc', 1, 3, 1);
  x3('([xyz][abc]|abc)+', 'abc', 1, 3, 1);
  x3('((?i:abc))', 'AbC', 1, 3, 1);
  x2('(abc)(?i:\1)', 'abcABC', 1, 6);
  x3('((?s:a.c))', 'a'#$A'c', 1, 3, 1);
  x3('abc|(.abd)', 'zabd', 1, 4, 1);
  x2('(?:abc)|(ABC)', 'abc', 1, 3);
  x3('(?i:(abc))|(zzz)', 'ABC', 1, 3, 1);
  x3('a*(.)', 'aaaaz', 5, 1, 1);
  x3('a*?(.)', 'aaaaz', 1, 1, 1);
  x3('a*?(c)', 'aaaac', 5, 1, 1);
  x3('[bcd]a*(.)', 'caaaaz', 6, 1, 1);
  x3('(\Abb)cc', 'bbcc', 1, 2, 1);
  n('(\Abb)cc', 'zbbcc');
  x3('(^bb)cc', 'bbcc', 1, 2, 1);
  n('(^bb)cc', 'zbbcc');
  x3('cc(bb$)', 'ccbb', 3, 2, 1);
  n('cc(bb$)', 'ccbbb');
  n('(\1)', '');
  n('\1(a)', 'aa');
  n('(a(b)\1)\2+', 'ababb');
  n('(?:(?:\1|z)(a))+$', 'zaa');
  x2('(?:(?:\1|z)(a))+$', 'zaaa', 1, 4);
  n('(a)$|\1', 'az');
  x2('(a)\1', 'aa', 1, 2);
  n('(a)\1', 'ab');
  x2('(a?)\1', 'aa', 1, 2);
  x2('(a??)\1', 'aa', 1, 0);
  x2('(a*)\1', 'aaaaa', 1, 4);
  x3('(a*)\1', 'aaaaa', 1, 2, 1);
  x2('a(b*)\1', 'abbbb', 1, 5);
  x2('a(b*)\1', 'ab', 1, 1);
  x2('(a*)(b*)\1\2', 'aaabbaaabb', 1, 10);
  x2('(a*)(b*)\2', 'aaabbbb', 1, 7);
  x2('(((((((a*)b))))))c\7', 'aaabcaaa', 1, 8);
  x3('(((((((a*)b))))))c\7', 'aaabcaaa', 1, 3, 7);
  x2('(a)(b)(c)\2\1\3', 'abcbac', 1, 6);
  x2('([a-d])\1', 'cc', 1, 2);
  x2('(\w\d\s)\1', 'f5 f5 ', 1, 6);
  n('(\w\d\s)\1', 'f5 f5');
  x2('(who|[a-c]{3})\1', 'whowho', 1, 6);
  x2('...(who|[a-c]{3})\1', 'abcwhowho', 1, 9);
  x2('(who|[a-c]{3})\1', 'cbccbc', 1, 6);
  x2('(^a)\1', 'aa', 1, 2);
  n('(^a)\1', 'baa');
  n('(a$)\1', 'aa');
  n('(ab\Z)\1', 'ab');
  x2('(a*\Z)\1', 'a', 2, 0);
  x2('.(a*\Z)\1', 'ba', 2, 1);
  x3('(.(abc)\2)', 'zabcabc', 1, 7, 1);
  x3('(.(..\d.)\2)', 'z12341234', 1, 9, 1);
  x2('((?i:az))\1', 'AzAz', 1, 4);
  n('((?i:az))\1', 'Azaz');

  x2('(a{6}|a{5})*$', 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', 1, 31);

  x2('(?<name_3>.zv.)\k<name_3>', 'azvbazvb', 1, 8);
  // x2('(?<=\g<ab>)|-\zEND (?<ab>XyZ)', 'XyZ', 3, 3);
  // x2('(?<n>|\g<m>\g<n>)\z|\zEND (?<m>a|(b)\g<m>)', 'bbbbabba', 1, 8);
  x2('(.)(((?<_>a)))\k<_>', 'zaa', 1, 3);

  x2('(?<name1240>\w+\sx)a+\k<name1240>', '  fg xaaaaaaaafg x', 3, 16);

  x2('((?<name1>\d)|(?<name2>\w))(\k<name1>|\k<name2>)', 'ff', 1, 2);
  x2('(?:(?<x>)|(?<x>efg))\k<x>', '', 1, 0);
  x2('(?:(?<x>abc)|(?<x>efg))\k<x>', 'abcefgefg', 4, 6);
  n('(?:(?<x>abc)|(?<x>efg))\k<x>', 'abcefg');
  x2('(?:(?<n1>.)|(?<n1>..)|(?<n1>...)|(?<n1>....)|(?<n1>.....)|(?<n1>......)|(?<n1>.......)|(?<n1>........)|(?<n1>.........)|(?<n1>..........)|(?<n1>...........)|(?<n1>............)|(?<n1>.............)|(?<n1>..............))\k<n1>$',
    'a-pyumpyum', 3, 8);
  x3('(?:(?<n1>.)|(?<n1>..)|(?<n1>...)|(?<n1>....)|(?<n1>.....)|(?<n1>......)|(?<n1>.......)|(?<n1>........)|(?<n1>.........)|(?<n1>..........)|(?<n1>...........)|(?<n1>............)|(?<n1>.............)|(?<n1>..............))\k<n1>$',
    'xxxxabcdefghijklmnabcdefghijklmn', 5, 14, 14);

  x2('(?<n>a)(?<n>b)(?<n>\k<n>c)*', 'abbcbc', 1, 2);

  // @  x2('\g<bar>|\zEND(?<bar>.*abc$)', 'abcxxxabc', 1, 9);
  // @  x2('\g<1>|\zEND(.a.)', 'bac', 1, 3);
  // @  x3('\g<_A>\g<_A>|\zEND(.a.)(?<_A>.b.)', 'xbxyby', 3, 6, 1);
  // @  x2('\A(?:\g<pon>|\g<pan>|\zEND  (?<pan>a|c\g<pon>c)(?<pon>b|d\g<pan>d))$', 'cdcbcdc', 1, 7);
  // @  x2('\A(?<n>|a\g<m>)\z|\zEND (?<m>\g<n>)', 'aaaa', 1, 4);
  x3('(?<name1>)(?<name2>)(?<name3>)(?<name4>)(?<name5>)(?<name6>)(?<name7>)(?<name8>)(?<name9>)(?<name10>)(?<name11>)(?<name12>)(?<name13>)(?<name14>)(?<name15>)(?<name16>aaa)(?<name17>)$',
    'aaa', 1, 3, 16);

  x2('()*\1', '', 1, 0);
  n('(?:()|())*\1\2', '');
  x3('(?:\1a|())*', 'a', 1, 0, 1);
  x2('x((.)*)*x', '0x1x2x3', 2, 5);
  x2('x((.)*)*x(?i:\1)\Z', '0x1x2x1X2', 2, 8);
  n('(?:()|()|()|()|()|())*\2\5', '');
  n('(?:()|()|()|(x)|()|())*\2b\5', 'b');

  x2('', 'あ', 1, 0);
  x2('あ', 'あ', 1, 1);
  n('い', 'あ');
  x2('うう', 'うう', 1, 2);
  x2('あいう', 'あいう', 1, 3);
  x2('こここここここここここここここここここここここここここここここここここ',
    'こここここここここここここここここここここここここここここここここここ', 1, 35);
  x2('あ', 'いあ', 2, 1);
  x2('いう', 'あいう', 2, 2);
  // x2('\xca\xb8', #$ca#$b8, 1, 2);文字コードが違うのでマッチしない
  x2('.', 'あ', 1, 1);
  x2('..', 'かき', 1, 2);
  x2('\w', 'お', 1, 1);
  n('\W', 'あ');
  x2('[\W]', 'う$', 2, 1);
  x2('\S', 'そ', 1, 1);
  x2('\S', '漢', 1, 1);
  x2('\b', '気 ', 1, 0);
  x2('\b', ' ほ', 2, 0);
  x2('\B', 'せそ ', 2, 0);
  x2('\B', 'う ', 3, 0);
  x2('\B', ' い', 1, 0);
  x2('[\w]', 'ね', 1, 1);
  n('[\d]', 'ふ');
  x2('[\D]', 'は', 1, 1);
  n('[\s]', 'く');
  x2('[\S]', 'へ', 1, 1);
  x2('[\w\d]', 'よ', 1, 1);
  x2('[\w\d]', '   よ', 4, 1);
  n('\w十区', ' 十区');
  x2('十\W区', '十 区', 1, 3);
  x2('.\wう\W..ぞ', 'えうう うぞぞ', 1, 7);
  x2('\s\wこここ', ' ここここ', 1, 5);
  x2('^\w$', 'に', 1, 1);
  x2('^\wかきくけこ$', 'zかきくけこ', 1, 6);
  x2('^\w...うえお$', 'zあいううえお', 1, 7);
  x2('\w\w\s\Wおおお\d', 'aお  おおお4', 1, 8);
  x2('\w|\s', 'お', 1, 1);
  x2('\w|%', '%お', 1, 1);
  x2('\w|[&$]', 'う&', 1, 1);
  x2('(\w\d\s)\1', 'あ5 あ5 ', 1, 6);
  n('(\w\d\s)\1', 'あ5 あ5');
  x3('(.(..\d.)\2)', 'あ12341234', 1, 9, 1);
  x2('[たち]', 'ち', 1, 1);
  n('[なに]', 'ぬ');
  x2('[う-お]', 'え', 1, 1);
  n('[^け]', 'け');
  x2('あ.い.う', 'ああいいう', 1, 5);
  x2('ああ.け', 'ああけけ', 1, 4);
  n('.い', 'いえ');
  x2('.お', 'おお', 1, 2);
  x2('^あ', 'あ', 1, 1);
  x2('^む$', 'む', 1, 1);
  x2('\Aたちつ', 'たちつ', 1, 3);
  x2('むめも\Z', 'むめも', 1, 3);
  x2('かきく\z', 'かきく', 1, 3);
  x2('かきく\Z', 'かきく'#$A, 1, 3);
  n('まみ\A', 'まみ');
  n('ま\Aみ', 'まみ');
  x2('(?i:あ)', 'あ', 1, 1);
  x2('(?i:ぶべ)', 'ぶべ', 1, 2);
  n('(?i:い)', 'う');
  x2('(?s:よ.)', 'よ'#$A, 1, 2);
  x2('(?s:.め)', 'ま'#$A'め', 2, 2);
  x2('あ?', '', 1, 0);
  x2('変?', '化', 1, 0);
  x2('変?', '変', 1, 1);
  x2('量*', '', 1, 0);
  x2('量*', '量', 1, 1);
  x2('子*', '子子子', 1, 3);
  x2('馬*', '鹿馬馬馬馬', 1, 0);
  n('山+', '');
  x2('河+', '河', 1, 1);
  x2('時+', '時時時時', 1, 4);
  x2('え+', 'ええううう', 1, 2);
  x2('う+', 'おうううう', 2, 4);
  x2('.?', 'た', 1, 1);
  x2('.*', 'ぱぴぷぺ', 1, 4);
  x2('.+', 'ろ', 1, 1);
  x2('.+', 'いうえか'#$A, 1, 4);
  x2('あ|い', 'あ', 1, 1);
  x2('あ|い', 'い', 1, 1);
  x2('あい|いう', 'あい', 1, 2);
  x2('あい|いう', 'いう', 1, 2);
  x2('を(?:かき|きく)', 'をかき', 1, 3);
  x2('を(?:かき|きく)け', 'をきくけ', 1, 4);
  x2('あい|(?:あう|あを)', 'あを', 1, 2);
  x2('あ|い|う', 'えう', 2, 1);
  x2('あ|い|うえ|おかき|く|けこさ|しすせ|そ|たち|つてとなに|ぬね', 'しすせ', 1, 3);
  n('あ|い|うえ|おかき|く|けこさ|しすせ|そ|たち|つてとなに|ぬね', 'すせ');
  x2('あ|^わ', 'ぶあ', 2, 1);
  x2('あ|^を', 'をあ', 1, 1);
  x2('十|\A区', 'b区十', 3, 1);
  x2('十|\A区', '区', 1, 1);
  x2('十|区\Z', '区十', 2, 1);
  x2('十|区\Z', '区', 1, 1);
  x2('十|区\Z', '区'#$A, 1, 1);
  x2('十|区\z', '区十', 2, 1);
  x2('十|区\z', '区', 1, 1);
  x2('[い-け]', 'う', 1, 1);
  x2('[い-け]|[^か-こ]', 'あ', 1, 1);
  x2('[い-け]|[^か-こ]', 'か', 1, 1);
  x2('[^あ]', #$A, 1, 1);
  x2('(?:あ|[う-き])|いを', 'うを', 1, 1);
  x2('(?:あ|[う-き])|いを', 'いを', 1, 2);
  n('(?>あ|あいえ)う', 'あいえう');
  x2('(?>あいえ|あ)う', 'あいえう', 1, 4);
  x2('あ?|い', 'あ', 1, 1);
  x2('あ?|い', 'い', 1, 0);
  x2('あ?|い', '', 1, 0);
  x2('あ*|い', 'ああ', 1, 2);
  x2('あ*|い*', 'いあ', 1, 0);
  x2('あ*|い*', 'あい', 1, 1);
  x2('[aあ]*|い*', 'aあいいい', 1, 2);
  x2('あ+|い*', '', 1, 0);
  x2('あ+|い*', 'いいい', 1, 3);
  x2('あ+|い*', 'あいいい', 1, 1);
  x2('あ+|い*', 'aあいいい', 1, 0);
  n('あ+|い+', '');
  x2('(あ|い)?', 'い', 1, 1);
  x2('(あ|い)*', 'いあ', 1, 2);
  x2('(あ|い)+', 'いあい', 1, 3);
  x2('(あい|うあ)+', 'うああいうえ', 1, 4);
  x2('(あい|うえ)+', 'うああいうえ', 3, 4);
  x2('(あい|うあ)+', 'ああいうあ', 2, 4);
  x2('(あい|うあ)+', 'あいをうあ', 1, 2);
  x2('(あい|うあ)+', '$$zzzzあいをうあ', 7, 2);
  x2('(あ|いあい)+', 'あいあいあ', 1, 5);
  x2('(あ|いあい)+', 'いあ', 2, 1);
  x2('(あ|いあい)+', 'いあああいあ', 2, 3);
  x2('(?:あ|い)(?:あ|い)', 'あい', 1, 2);
  x2('(?:あ*|い*)(?:あ*|い*)', 'あああいいい', 1, 3);
  x2('(?:あ*|い*)(?:あ+|い+)', 'あああいいい', 1, 6);
  x2('(?:あ+|い+){2}', 'あああいいい', 1, 6);
  x2('(?:あ+|い+){1,2}', 'あああいいい', 1, 6);
  x2('(?:あ+|\Aい*)うう', 'うう', 1, 2);
  n('(?:あ+|\Aい*)うう', 'あいうう');
  x2('(?:^あ+|い+)*う', 'ああいいいあいう', 7, 2);
  x2('(?:^あ+|い+)*う', 'ああいいいいう', 1, 7);
  x2('う{0,}', 'うううう', 1, 4);
  x2('あ|(?i)c', 'C', 1, 1);
  x2('(?i)c|あ', 'C', 1, 1);
  x2('(?i:あ)|a', 'a', 1, 1);
  n('(?i:あ)|a', 'A');
  x2('[あいう]?', 'あいう', 1, 1);
  x2('[あいう]*', 'あいう', 1, 3);
  x2('[^あいう]*', 'あいう', 1, 0);
  n('[^あいう]+', 'あいう');
  x2('あ??', 'あああ', 1, 0);
  x2('いあ??い', 'いあい', 1, 3);
  x2('あ*?', 'あああ', 1, 0);
  x2('いあ*?', 'いああ', 1, 1);
  x2('いあ*?い', 'いああい', 1, 4);
  x2('あ+?', 'あああ', 1, 1);
  x2('いあ+?', 'いああ', 1, 2);
  x2('いあ+?い', 'いああい', 1, 4);
  x2('(?:天?)??', '天', 1, 0);
  x2('(?:天??)?', '天', 1, 0);
  x2('(?:夢?)+?', '夢夢夢', 1, 1);
  x2('(?:風+)??', '風風風', 1, 0);
  x2('(?:雪+)??霜', '雪雪雪霜', 1, 4);
  // x2('(?:あい)?{2}', '', 1, 0);
  // x2('(?:十区)?{2}', '十区十区十', 1, 8);
  // x2('(?:十区)*{0}', '十区十区十', 1, 0);
  x2('(?:十区){3,}', '十区十区十区十区', 1, 8);
  n('(?:十区){3,}', '十区十区');
  x2('(?:十区){2,4}', '十区十区十区', 1, 6);
  x2('(?:十区){2,4}', '十区十区十区十区十区', 1, 8);
  x2('(?:十区){2,4}?', '十区十区十区十区十区', 1, 4);
  // x2('(?:十区){,}', '十区{,}', 1, 7);
  // x2('(?:かきく)+?{2}', 'かきくかきくかきく', 1, 12);
  x3('(火)', '火', 1, 1, 1);
  x3('(火水)', '火水', 1, 2, 1);
  x2('((時間))', '時間', 1, 2);
  x3('((風水))', '風水', 1, 2, 1);
  x3('((昨日))', '昨日', 1, 2, 2);
  x3('((((((((((((((((((((量子))))))))))))))))))))', '量子', 1, 2, 20);
  x3('(あい)(うえ)', 'あいうえ', 1, 2, 1);
  x3('(あい)(うえ)', 'あいうえ', 3, 2, 2);
  x3('()(あ)いう(えおか)きくけこ', 'あいうえおかきくけこ', 4, 3, 3);
  x3('(()(あ)いう(えおか)きくけこ)', 'あいうえおかきくけこ', 4, 3, 4);
  x3('.*(フォ)ン・マ(ン()シュタ)イン', 'フォン・マンシュタイン', 6, 4, 2);
  x2('(^あ)', 'あ', 1, 1);
  x3('(あ)|(あ)', 'いあ', 2, 1, 1);
  x3('(^あ)|(あ)', 'いあ', 2, 1, 2);
  x3('(あ?)', 'あああ', 1, 1, 1);
  x3('(ま*)', 'ままま', 1, 3, 1);
  x3('(と*)', '', 1, 0, 1);
  x3('(る+)', 'るるるるるるる', 1, 7, 1);
  x3('(ふ+|へ*)', 'ふふふへへ', 1, 3, 1);
  x3('(あ+|い?)', 'いいいああ', 1, 1, 1);
  x3('(あいう)?', 'あいう', 1, 3, 1);
  x3('(あいう)*', 'あいう', 1, 3, 1);
  x3('(あいう)+', 'あいう', 1, 3, 1);
  x3('(さしす|あいう)+', 'あいう', 1, 3, 1);
  x3('([なにぬ][かきく]|かきく)+', 'かきく', 1, 3, 1);
  x3('((?i:あいう))', 'あいう', 1, 3, 1);
  x3('((?s:あ.う))', 'あ'#$A'う', 1, 3, 1);
  x3('あいう|(.あいえ)', 'んあいえ', 1, 4, 1);
  x3('あ*(.)', 'ああああん', 5, 1, 1);
  x3('あ*?(.)', 'ああああん', 1, 1, 1);
  x3('あ*?(ん)', 'ああああん', 5, 1, 1);
  x3('[いうえ]あ*(.)', 'えああああん', 6, 1, 1);
  x3('(\Aいい)うう', 'いいうう', 1, 2, 1);
  n('(\Aいい)うう', 'んいいうう');
  x3('(^いい)うう', 'いいうう', 1, 2, 1);
  n('(^いい)うう', 'んいいうう');
  x3('ろろ(るる$)', 'ろろるる', 3, 2, 1);
  n('ろろ(るる$)', 'ろろるるる');
  x2('(無)\1', '無無', 1, 2);
  n('(無)\1', '無武');
  x2('(空?)\1', '空空', 1, 2);
  x2('(空??)\1', '空空', 1, 0);
  x2('(空*)\1', '空空空空空', 1, 4);
  x3('(空*)\1', '空空空空空', 1, 2, 1);
  x2('あ(い*)\1', 'あいいいい', 1, 5);
  x2('あ(い*)\1', 'あい', 1, 1);
  x2('(あ*)(い*)\1\2', 'あああいいあああいい', 1, 10);
  x2('(あ*)(い*)\2', 'あああいいいい', 1, 7);
  x3('(あ*)(い*)\2', 'あああいいいい', 4, 2, 2);
  x2('(((((((ぽ*)ぺ))))))ぴ\7', 'ぽぽぽぺぴぽぽぽ', 1, 8);
  x3('(((((((ぽ*)ぺ))))))ぴ\7', 'ぽぽぽぺぴぽぽぽ', 1, 3, 7);
  x2('(は)(ひ)(ふ)\2\1\3', 'はひふひはふ', 1, 6);
  x2('([き-け])\1', 'くく', 1, 2);
  x2('(誰？|[あ-う]{3})\1', '誰？誰？', 1, 4);
  x2('...(誰？|[あ-う]{3})\1', 'あaあ誰？誰？', 1, 7);
  x2('(誰？|[あ-う]{3})\1', 'ういうういう', 1, 6);
  x2('(^こ)\1', 'ここ', 1, 2);
  n('(^む)\1', 'めむむ');
  n('(あ$)\1', 'ああ');
  n('(あい\Z)\1', 'あい');
  x2('(あ*\Z)\1', 'あ', 2, 0);
  x2('.(あ*\Z)\1', 'いあ', 2, 1);
  x3('(.(やいゆ)\2)', 'zやいゆやいゆ', 1, 7, 1);
  x2('((?i:あvず))\1', 'あvずあvず', 1, 6);

  // x2('\A(?:\g<A_1>|\g<A_2>|\z終了  (?<阿_1>観|自\g<云_2>自)(?<云_2>在|菩薩\g<阿_1>菩薩))$', '菩薩自菩薩自在自菩薩自菩薩', 1, 26);
  // x2('[[ひふ]]', 'ふ', 1, 2);
  // x2('[[いおう]か]', 'か', 1, 2);
  // n('[[^あ]]', 'あ');
  // n('[^[あ]]', 'あ');
  // x2('[^[^あ]]', 'あ', 1, 2);
  // x2('[[かきく]&&きく]', 'く', 1, 2);
  // n('[[かきく]&&きく]', 'か');
  // n('[[かきく]&&きく]', 'け');
  // x2('[あ-ん&&い-を&&う-ゑ]', 'ゑ', 1, 2);
  // n('[^あ-ん&&い-を&&う-ゑ]', 'ゑ');
  // x2('[[^あ&&あ]&&あ-ん]', 'い', 1, 2);
  // n('[[^あ&&あ]&&あ-ん]', 'あ');
  // x2('[[^あ-ん&&いうえお]&&[^う-か]]', 'き', 1, 2);
  // n('[[^あ-ん&&いうえお]&&[^う-か]]', 'い');
  // x2('[^[^あいう]&&[^うえお]]', 'う', 1, 2);
  // x2('[^[^あいう]&&[^うえお]]', 'え', 1, 2);
  // n('[^[^あいう]&&[^うえお]]', 'か');
  // x2('[あ-&&-あ]', '-', 1, 1);
  // x2('[^[^a-zあいう]&&[^bcdefgうえお]q-w]', 'え', 1, 2);
  // x2('[^[^a-zあいう]&&[^bcdefgうえお]g-w]', 'f', 1, 1);
  // x2('[^[^a-zあいう]&&[^bcdefgうえお]g-w]', 'g', 1, 1);
  // n('[^[^a-zあいう]&&[^bcdefgうえお]g-w]', '2');
  x2('a<b>バージョンのダウンロード<\/b>', 'a<b>バージョンのダウンロード</b>', 1, 20);
  x2('.<b>バージョンのダウンロード<\/b>', 'a<b>バージョンのダウンロード</b>', 1, 20);

  x2('([^ @]+)@([^ @]+)', 'info@info.com',  1, 13);
  x2('([0-9][0-9]?)/([0-9][0-9]?)/([0-9][0-9]([0-9][0-9])?)', '6/9/97', 1, 6);

  x2('([a-z][0-9])?[Ss]', 'How a0Set', 5, 3);
  x2('([a-z][0-9])*[Ss]', 'How a0Set', 5, 3);
  x2('(?x)< (?: (?(R) \d++  | [^<>]*+) | (?R)) * >', '<dot net <123> fire>', 1, 20);
  x2('(mcb|mc|t|cu|nbp|la|rry)[0-9]{2}-g[bc][0-9]{4}[rshjgop]', 'mcb00-gb0002h', 1, 13);
  x2('(mcb|mc|t|cu|nbp|la|rry)[0-9]*-g[bc][0-9]{4}[rshjgop]', 'mcb00-gb0002h', 1, 13);
  x2('(mcb|mc|t|cu|nbp|la|rry)[0-9]+-g[bc][0-9]{4}[rshjgop]', 'mcb00-gb0002h', 1, 13);

  x2('(?w)[abc]+', 'aａａｂc', 1, 5);
  x2('(?w)[abc]+', 'aｂccａa', 1, 6);
  x2('(?k)[パハバ]+', 'はばぱぱは', 1, 5);

  n('\d{2}[A-Z]-[A-Z]{4}\d{4}[A-Z]{2}', '{90150000-000F-0000-0000-0000000FF1CE}');
end;

procedure TestQuickSearch;
var
  sc: TREQuickSearch;
  S, p: REString;
begin
  sc := TREQuickSearch.Create;
  try
    S := '最初の一歩';
    p := '一歩';

    sc.FindText := p;
    sc.Options := [];
    if not sc.Exec(PWideChar(S), Length(S)) then
      raise Exception.Create('error: ' + S);

    S := 'ＡＢＣＤＥ';
    p := 'ab';
    sc.Options := [coIgnoreCase, coIgnoreWidth];
    sc.FindText := p;
    if not sc.Exec(PWideChar(S), Length(S)) then
      raise Exception.Create('error: ' + S);

    S := 'あいうえお';
    p := 'ｴｵ';
    sc.Options := [coIgnoreKana, coIgnoreWidth];
    sc.FindText := p;
    if not sc.Exec(PWideChar(S), Length(S)) then
      raise Exception.Create('error: ' + S);
  finally
    sc.Free;
  end;

end;

{$IFDEF SKREGEXP_DEBUG}

procedure TestREStrPos;
var
  Source, Pattern: REString;
  LOptions: TRECompareOptions;
  LMatchLen: Integer;
begin
  Source := #0000'zx';
  Pattern := 'zx';
  LOptions := [];

  if REStrPos(PWideChar(Source), Length(Source), PWideChar(Pattern),
    Length(Pattern), LMatchLen, LOptions) = nil then
    raise Exception.Create('error: #0000zx');

  Source := 'abcde';
  Pattern := 'a';
  LOptions := [];

  if REStrPos(PWideChar(Source), Length(Source), PWideChar(Pattern),
    Length(Pattern), LMatchLen, LOptions) = nil then
    raise Exception.Create('error: ' + Source);

  Source := 'abcde';
  Pattern := 'e';
  LOptions := [];

  if REStrPos(PWideChar(Source), Length(Source), PWideChar(Pattern),
    Length(Pattern), LMatchLen, LOptions) = nil then
    raise Exception.Create('error: ' + Source);

  Source := 'abcde';
  Pattern := 'cd';
  LOptions := [];

  if REStrPos(PWideChar(Source), Length(Source), PWideChar(Pattern),
    Length(Pattern), LMatchLen, LOptions) = nil then
    raise Exception.Create('error: ' + Source);

  Source := 'abcde';
  Pattern := 'abcde';
  LOptions := [];

  if REStrPos(PWideChar(Source), Length(Source), PWideChar(Pattern),
    Length(Pattern), LMatchLen, LOptions) = nil then
    raise Exception.Create('error: ' + Source);

  Source := 'AbCdE';
  Pattern := 'abcde';
  LOptions := [coIgnoreCase];

  if REStrPos(PWideChar(Source), Length(Source), PWideChar(Pattern),
    Length(Pattern), LMatchLen, LOptions) = nil then
    raise Exception.Create('error: ' + Source);

  Source := 'ＡｂＣｄＥ';
  Pattern := 'abcde';
  LOptions := [coIgnoreCase, coIgnoreWidth];

  if REStrPos(PWideChar(Source), Length(Source), PWideChar(Pattern),
    Length(Pattern), LMatchLen, LOptions) = nil then
    raise Exception.Create('error: ' + Source);

  Source := 'アイウエオ';
  Pattern := 'ｱ';
  LOptions := [coIgnoreWidth];

  if REStrPos(PWideChar(Source), Length(Source), PWideChar(Pattern),
    Length(Pattern), LMatchLen, LOptions) = nil then
    raise Exception.Create('error: ' + Source);

  Source := 'アイウエオ';
  Pattern := 'ｵ';
  LOptions := [coIgnoreWidth];

  if REStrPos(PWideChar(Source), Length(Source), PWideChar(Pattern),
    Length(Pattern), LMatchLen, LOptions) = nil then
    raise Exception.Create('error: ' + Source);

  Source := 'あいうえお';
  Pattern := 'ｱ';
  LOptions := [coIgnoreWidth, coIgnoreKana];

  if REStrPos(PWideChar(Source), Length(Source), PWideChar(Pattern),
    Length(Pattern), LMatchLen, LOptions) = nil then
    raise Exception.Create('error: ' + Source);

  Source := 'あいうえお';
  Pattern := 'ｵ';
  LOptions := [coIgnoreWidth, coIgnoreKana];

  if REStrPos(PWideChar(Source), Length(Source), PWideChar(Pattern),
    Length(Pattern), LMatchLen, LOptions) = nil then
    raise Exception.Create('error: ' + Source);
end;
{$ENDIF}

{$IFDEF SKREGEXP_DEBUG}
procedure TestJpExtTable;

  function ConvertString(const S: REString; Flag: DWORD): REString;
  var
    L: Integer;
  begin
    L := Length(S);
    if L = 0 then
    begin
      Result := '';
      Exit;
    end;

    SetLength(Result, L * 2);
  {$IFDEF UNICODE}
    L := LCMapString(GetUserDefaultLCID, Flag, PWideChar(S), Length(S),
      PWideChar(Result), Length(Result));
    if L = 0 then
      RaiseLastOSError;
  {$ELSE}
    L := LCMapStringW(GetUserDefaultLCID, Flag, PWideChar(S), Length(S),
      PWideChar(Result), Length(Result));
    if L = 0 then
      RaiseLastOSError;
  {$ENDIF}
    SetLength(Result, L);
  end;

  function ToUpper(const S: REString): REString; inline;
  begin
    Result := ConvertString(S, LCMAP_UPPERCASE);
  end;

  function ToLower(const S: REString): REString; inline;
  begin
    Result := ConvertString(S, LCMAP_LOWERCASE);
  end;

  function ToWide(const S: REString): REString; inline;
  begin
    Result := ConvertString(S, LCMAP_FULLWIDTH);
  end;

  function ToHalf(const S: REString): REString; inline;
  begin
    Result := ConvertString(S, LCMAP_HALFWIDTH);
  end;

  function ToHiragana(const S: REString): REString; inline;
  begin
    Result := ConvertString(S, LCMAP_HIRAGANA);
  end;

  function ToKatakana(const S: REString): REString; inline;
  begin
    Result := ConvertString(S, LCMAP_KATAKANA);
  end;

  procedure Check(S1, S2: REString);
  var
    I1, I2, L1: Integer;
  begin
    I1 := 1;
    I2 := 1;
    L1 := System.Length(S1);

    while I1 <= L1 do
    begin
      if S1[I1] <> S2[I2] then
        raise Exception.CreateFmt('not equals ''%.6x'', ''%.6x''',
          [Ord(S1[I1]), Ord(S2[I2])] );

      Inc(I1);
      Inc(I2);
    end;
  end;

  procedure CheckAll(S: REString);
  begin
    Check(ToUpper(S), SkRegExpW.ToFoldCase(S, False));
    Check(ToLower(S), SkRegExpW.ToFoldCase(S, False));

{$IFNDEF NoUSEJapaneseExt}
    Check(ToWide(S), SkRegExpW.ToWide(S));
    Check(ToHalf(S), SkRegExpW.ToHalf(S));

    Check(ToHiragana(S), SkRegExpW.ToHiragana(S));
    Check(ToKatakana(S), SkRegExpW.ToKatakana(S));
{$ENDIF NoUSEJapaneseExt}
  end;

var
  I: Integer;
  S: REString;
begin
  //half ank
  S := '';
  for I := $20 to $7E do
    S := S + WideChar(I);
  CheckAll(S);

  // half katakana
  S := '';
  for I := $FF61 to $FF9D do
    S := S + WideChar(I);
  CheckAll(S);

  // wide ank
  S := '';
  for I := $FF01 to $FF5E do
    S := S + WideChar(I);
  CheckAll(S);

  // wide katakana
  S := '';
  for I := $3000 to $30FF do
    S := S + WideChar(I);
  CheckAll(S);

  // wide hiragana
  S := '';
  for I := $3040 to $309F do
    S := S + WideChar(I);
  CheckAll(S);

  S := '';
  for I := $20 to $10FFFF do
    S := S + UCharToString(I);

  Check(ToUpper(S), SkRegExpW.ToFoldCase(S, False));
end;
{$ENDIF SKREGEXP_DEBUG}

end.
