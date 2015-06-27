unit SkRegExpPerlTest;

interface

uses SkRegExpW, Classes;

type
  TSkRegExpPerlTest = class
  private
    FMessage: TREStrings;
    function SubStitute(ARegExp: TSkRegExp; const ATemplate: REString): REString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Exec(ADest: TStrings);
    procedure y(APattern, AStrings, AExpr, AExceptedExpr, ASkipReason: REString;
      AOptions: TREOptions = []);
    procedure n(APattern, AStrings, ASkipReason: REString; AOptions: TREOptions = []);
    procedure c(APattern, AStrings, ASkipReason: REString; AOptions: TREOptions = []);
    property Message: TREStrings read FMessage;
  end;

implementation

uses SysUtils, SkRegExpConst, Contnrs;

{ TSkRegExpRETest }

procedure TSkRegExpPerlTest.c(APattern, AStrings, ASkipReason: REString; AOptions: TREOptions);
var
  R: TSkRegExp;
begin
  try
    if AStrings = '-' then
      AStrings := '';

    R := TSkRegExp.Create;
    try
      R.Expression := APattern;
      R.Options := AOptions;
      R.InputString := TSkRegExp.DecodeEscape(AStrings);
      if R.ExecPos() then
      begin
        FMessage.Add(Format('''%s'', ''%s'' : Matched', [APattern, AStrings]));
      end;
    finally
      R.Free;
    end;
  except
    on E: ESkRegExp do
    begin
      //FMessage.Add(Format('''%s'', ''%s'' : %s(%s)', [APattern, AStrings, E.Message, ASkipReason]));
    end;
  end;
end;

constructor TSkRegExpPerlTest.Create;
begin
  inherited;
  FMessage := TREStringList.Create;
end;

destructor TSkRegExpPerlTest.Destroy;
begin
  FMessage.Free;
  inherited;
end;

procedure TSkRegExpPerlTest.Exec(ADest: TStrings);
begin
  FMessage.Clear;

// This stops me getting screenfulls of syntax errors every time I accidentally
// run this file via a shell glob.  The full format of this file is given
// in regexp.t
// Prior to the implementation of autoloading of \N{}, tests that used \N{name}
// could not go in this file, and were farmed out to other .t's, where they
// remain
//
// pat	string	y/n/etc	expr	expected-expr	skip-reason
  y('abc', 'abc', '$&', 'abc', '');
  y('abc', 'abc', '$-[0]', '0', '');
  y('abc', 'abc', '$+[0]', '3', '');
  n('abc', 'xbc', '');
  n('abc', 'axc', '');
  n('abc', 'abx', '');
  y('abc', 'xabcy', '$&', 'abc', '');
  y('abc', 'xabcy', '$-[0]', '1', '');
  y('abc', 'xabcy', '$+[0]', '4', '');
  y('abc', 'ababc', '$&', 'abc', '');
  y('abc', 'ababc', '$-[0]', '2', '');
  y('abc', 'ababc', '$+[0]', '5', '');
  y('ab*c', 'abc', '$&', 'abc', '');
  y('ab*c', 'abc', '$-[0]', '0', '');
  y('ab*c', 'abc', '$+[0]', '3', '');
  y('ab*bc', 'abc', '$&', 'abc', '');
  y('ab*bc', 'abc', '$-[0]', '0', '');
  y('ab*bc', 'abc', '$+[0]', '3', '');
  y('ab*bc', 'abbc', '$&', 'abbc', '');
  y('ab*bc', 'abbc', '$-[0]', '0', '');
  y('ab*bc', 'abbc', '$+[0]', '4', '');
  y('ab*bc', 'abbbbc', '$&', 'abbbbc', '');
  y('ab*bc', 'abbbbc', '$-[0]', '0', '');
  y('ab*bc', 'abbbbc', '$+[0]', '6', '');
  y('.{1}', 'abbbbc', '$&', 'a', '');
  y('.{1}', 'abbbbc', '$-[0]', '0', '');
  y('.{1}', 'abbbbc', '$+[0]', '1', '');
  y('.{3,4}', 'abbbbc', '$&', 'abbb', '');
  y('.{3,4}', 'abbbbc', '$-[0]', '0', '');
  y('.{3,4}', 'abbbbc', '$+[0]', '4', '');
  y('ab{0,}bc', 'abbbbc', '$&', 'abbbbc', '');
  y('ab{0,}bc', 'abbbbc', '$-[0]', '0', '');
  y('ab{0,}bc', 'abbbbc', '$+[0]', '6', '');
  y('ab+bc', 'abbc', '$&', 'abbc', '');
  y('ab+bc', 'abbc', '$-[0]', '0', '');
  y('ab+bc', 'abbc', '$+[0]', '4', '');
  n('ab+bc', 'abc', '');
  n('ab+bc', 'abq', '');
  n('ab{1,}bc', 'abq', '');
  y('ab+bc', 'abbbbc', '$&', 'abbbbc', '');
  y('ab+bc', 'abbbbc', '$-[0]', '0', '');
  y('ab+bc', 'abbbbc', '$+[0]', '6', '');
  y('ab{1,}bc', 'abbbbc', '$&', 'abbbbc', '');
  y('ab{1,}bc', 'abbbbc', '$-[0]', '0', '');
  y('ab{1,}bc', 'abbbbc', '$+[0]', '6', '');
  y('ab{1,3}bc', 'abbbbc', '$&', 'abbbbc', '');
  y('ab{1,3}bc', 'abbbbc', '$-[0]', '0', '');
  y('ab{1,3}bc', 'abbbbc', '$+[0]', '6', '');
  y('ab{3,4}bc', 'abbbbc', '$&', 'abbbbc', '');
  y('ab{3,4}bc', 'abbbbc', '$-[0]', '0', '');
  y('ab{3,4}bc', 'abbbbc', '$+[0]', '6', '');
  n('ab{4,5}bc', 'abbbbc', '');
  y('ab?bc', 'abbc', '$&', 'abbc', '');
  y('ab?bc', 'abc', '$&', 'abc', '');
  y('ab{0,1}bc', 'abc', '$&', 'abc', '');
  n('ab?bc', 'abbbbc', '');
  y('ab?c', 'abc', '$&', 'abc', '');
  y('ab{0,1}c', 'abc', '$&', 'abc', '');
  y('^abc$', 'abc', '$&', 'abc', '');
  n('^abc$', 'abcc', '');
  y('^abc', 'abcc', '$&', 'abc', '');
  n('^abc$', 'aabc', '');
  y('abc$', 'aabc', '$&', 'abc', '');
  n('abc$', 'aabcd', '');
  y('^', 'abc', '$&', '', '');
  y('$', 'abc', '$&', '', '');
  y('a.c', 'abc', '$&', 'abc', '');
  y('a.c', 'axc', '$&', 'axc', '');
  y('a\Nc', 'abc', '$&', 'abc', '');
  y('a\N c', 'abc', '$&', 'abc', '', [roExtended]);
  y('a.*c', 'axyzc', '$&', 'axyzc', '');
  y('a\N*c', 'axyzc', '$&', 'axyzc', '');
  y('a\N *c', 'axyzc', '$&', 'axyzc', '', [roExtended]);
  n('a.*c', 'axyzd', '');
  n('a\N*c', 'axyzd', '');
  n('a\N *c', 'axyzd', '', [roExtended]);
  n('a[bc]d', 'abc', '');
  y('a[bc]d', 'abd', '$&', 'abd', '');
  y('a[b]d', 'abd', '$&', 'abd', '');
  y('[a][b][d]', 'abd', '$&', 'abd', '');
  y('.[b].', 'abd', '$&', 'abd', '');
  n('.[b].', 'aBd', '');
  y('(?i:.[b].)', 'abd', '$&', 'abd', '');
  y('(?i:\N[b]\N)', 'abd', '$&', 'abd', '');
  n('a[b-d]e', 'abd', '');
  y('a[b-d]e', 'ace', '$&', 'ace', '');
  y('a[b-d]', 'aac', '$&', 'ac', '');
  y('a[-b]', 'a-', '$&', 'a-', '');
  y('a[b-]', 'a-', '$&', 'a-', '');
  c('a[b-a]', '-', '');
  c('a[]b', '-', '');
  c('a[', '-', '');
  y('a]', 'a]', '$&', 'a]', '');
  y('a[]]b', 'a]b', '$&', 'a]b', '');
  y('a[^bc]d', 'aed', '$&', 'aed', '');
  n('a[^bc]d', 'abd', '');
  y('a[^-b]c', 'adc', '$&', 'adc', '');
  n('a[^-b]c', 'a-c', '');
  n('a[^]b]c', 'a]c', '');
  y('a[^]b]c', 'adc', '$&', 'adc', '');
  y('\ba\b', 'a-', '-', '-', '');
  y('\ba\b', '-a', '-', '-', '');
  y('\ba\b', '-a-', '-', '-', '');
  n('\by\b', 'xy', '');
  n('\by\b', 'yz', '');
  n('\by\b', 'xyz', '');
  n('\Ba\B', 'a-', '');
  n('\Ba\B', '-a', '');
  n('\Ba\B', '-a-', '');
  y('\By\b', 'xy', '-', '-', '');
  y('\By\b', 'xy', '$-[0]', '1', '');
  y('\By\b', 'xy', '$+[0]', '2', '');
  y('\By\b', 'xy', '-', '-', '');
  y('\by\B', 'yz', '-', '-', '');
  y('\By\B', 'xyz', '-', '-', '');
  y('\w', 'a', '-', '-', '');
  n('\w', '-', '');
  n('\W', 'a', '');
  y('\W', '-', '-', '-', '');
  y('a\sb', 'a b', '-', '-', '');
  n('a\sb', 'a-b', '');
  n('a\Sb', 'a b', '');
  y('a\Sb', 'a-b', '-', '-', '');
  y('\d', '1', '-', '-', '');
  n('\d', '-', '');
  n('\D', '1', '');
  y('\D', '-', '-', '-', '');
  y('[\w]', 'a', '-', '-', '');
  n('[\w]', '-', '');
  n('[\W]', 'a', '');
  y('[\W]', '-', '-', '-', '');
  y('a[\s]b', 'a b', '-', '-', '');
  n('a[\s]b', 'a-b', '');
  n('a[\S]b', 'a b', '');
  y('a[\S]b', 'a-b', '-', '-', '');
  y('[\d]', '1', '-', '-', '');
  n('[\d]', '-', '');
  n('[\D]', '1', '');
  y('[\D]', '-', '-', '-', '');
  y('ab|cd', 'abc', '$&', 'ab', '');
  y('ab|cd', 'abcd', '$&', 'ab', '');
  y('()ef', 'def', '$&-$1', 'ef-', '');
  y('()ef', 'def', '$-[0]', '1', '');
  y('()ef', 'def', '$+[0]', '3', '');
  y('()\x{100}\x{1000}', 'd\x{100}\x{1000}', '$+[0]', '3', '');
  y('()ef', 'def', '$-[1]', '1', '');
  y('()ef', 'def', '$+[1]', '1', '');
  c('*a', '-', '');
  c('(|*)b', '-', '');
  c('(*)b', '-', '');
  n('$b', 'b', '');
  c('a\', '-', '');
  y('a\(b', 'a(b', '$&-$1', 'a(b-', '');
  y('a\(*b', 'ab', '$&', 'ab', '');
  y('a\(*b', 'a((b', '$&', 'a((b', '');
  y('a\\b', 'a\\b', '$&', 'a\\b', '');
  c('abc)', '-', '');
  c('(abc', '-', '');
  y('((a))', 'abc', '$&-$1-$2', 'a-a-a', '');
  y('((a))', 'abc', '$-[0]-$-[1]-$-[2]', '0-0-0', '');
  y('((a))', 'abc', '$+[0]-$+[1]-$+[2]', '1-1-1', '');
  y('((a))', 'abc', '@-', '0 0 0', '');
  y('((a))', 'abc', '@+', '1 1 1', '');
  y('(a)b(c)', 'abc', '$&-$1-$2', 'abc-a-c', '');
  y('(a)b(c)', 'abc', '$-[0]-$-[1]-$-[2]', '0-0-2', '');
  y('(a)b(c)', 'abc', '$+[0]-$+[1]-$+[2]', '3-1-3', '');
  y('a+b+c', 'aabbabc', '$&', 'abc', '');
  y('a{1,}b{1,}c', 'aabbabc', '$&', 'abc', '');
  c('a**', '-', '');
  y('a.+?c', 'abcabc', '$&', 'abc', '');
  y('(a+|b)*', 'ab', '$&-$1', 'ab-b', '');
  y('(a+|b)*', 'ab', '$-[0]', '0', '');
  y('(a+|b)*', 'ab', '$+[0]', '2', '');
  y('(a+|b)*', 'ab', '$-[1]', '1', '');
  y('(a+|b)*', 'ab', '$+[1]', '2', '');
  y('(a+|b){0,}', 'ab', '$&-$1', 'ab-b', '');
  y('(a+|b)+', 'ab', '$&-$1', 'ab-b', '');
  y('(a+|b){1,}', 'ab', '$&-$1', 'ab-b', '');
  y('(a+|b)?', 'ab', '$&-$1', 'a-a', '');
  y('(a+|b){0,1}', 'ab', '$&-$1', 'a-a', '');
  c(')(', '-', '');
  y('[^ab]*', 'cde', '$&', 'cde', '');
  n('abc', '', '');
  y('a*', '', '$&', '', '');
  y('([abc])*d', 'abbbcd', '$&-$1', 'abbbcd-c', '');
  y('([abc])*bcd', 'abcd', '$&-$1', 'abcd-a', '');
  y('a|b|c|d|e', 'e', '$&', 'e', '');
  y('(a|b|c|d|e)f', 'ef', '$&-$1', 'ef-e', '');
  y('(a|b|c|d|e)f', 'ef', '$-[0]', '0', '');
  y('(a|b|c|d|e)f', 'ef', '$+[0]', '2', '');
  y('(a|b|c|d|e)f', 'ef', '$-[1]', '0', '');
  y('(a|b|c|d|e)f', 'ef', '$+[1]', '1', '');
  y('abcd*efg', 'abcdefg', '$&', 'abcdefg', '');
  y('ab*', 'xabyabbbz', '$&', 'ab', '');
  y('ab*', 'xayabbbz', '$&', 'a', '');
  y('(ab|cd)e', 'abcde', '$&-$1', 'cde-cd', '');
  y('[abhgefdc]ij', 'hij', '$&', 'hij', '');
  n('^(ab|cd)e', 'abcde', '');
  y('(abc|)ef', 'abcdef', '$&-$1', 'ef-', '');
  y('(a|b)c*d', 'abcd', '$&-$1', 'bcd-b', '');
  y('(ab|ab*)bc', 'abc', '$&-$1', 'abc-a', '');
  y('a([bc]*)c*', 'abc', '$&-$1', 'abc-bc', '');
  y('a([bc]*)(c*d)', 'abcd', '$&-$1-$2', 'abcd-bc-d', '');
  y('a([bc]*)(c*d)', 'abcd', '$-[0]', '0', '');
  y('a([bc]*)(c*d)', 'abcd', '$+[0]', '4', '');
  y('a([bc]*)(c*d)', 'abcd', '$-[1]', '1', '');
  y('a([bc]*)(c*d)', 'abcd', '$+[1]', '3', '');
  y('a([bc]*)(c*d)', 'abcd', '$-[2]', '3', '');
  y('a([bc]*)(c*d)', 'abcd', '$+[2]', '4', '');
  y('a([bc]+)(c*d)', 'abcd', '$&-$1-$2', 'abcd-bc-d', '');
  y('a([bc]*)(c+d)', 'abcd', '$&-$1-$2', 'abcd-b-cd', '');
  y('a([bc]*)(c+d)', 'abcd', '$-[0]', '0', '');
  y('a([bc]*)(c+d)', 'abcd', '$+[0]', '4', '');
  y('a([bc]*)(c+d)', 'abcd', '$-[1]', '1', '');
  y('a([bc]*)(c+d)', 'abcd', '$+[1]', '2', '');
  y('a([bc]*)(c+d)', 'abcd', '$-[2]', '2', '');
  y('a([bc]*)(c+d)', 'abcd', '$+[2]', '4', '');
  y('a[bcd]*dcdcde', 'adcdcde', '$&', 'adcdcde', '');
  n('a[bcd]+dcdcde', 'adcdcde', '');
  y('(ab|a)b*c', 'abc', '$&-$1', 'abc-ab', '');
  y('(ab|a)b*c', 'abc', '$-[0]', '0', '');
  y('(ab|a)b*c', 'abc', '$+[0]', '3', '');
  y('(ab|a)b*c', 'abc', '$-[1]', '0', '');
  y('(ab|a)b*c', 'abc', '$+[1]', '2', '');
  y('((a)(b)c)(d)', 'abcd', '$1-$2-$3-$4', 'abc-a-b-d', '');
  y('((a)(b)c)(d)', 'abcd', '$-[0]', '0', '');
  y('((a)(b)c)(d)', 'abcd', '$+[0]', '4', '');
  y('((a)(b)c)(d)', 'abcd', '$-[1]', '0', '');
  y('((a)(b)c)(d)', 'abcd', '$+[1]', '3', '');
  y('((a)(b)c)(d)', 'abcd', '$-[2]', '0', '');
  y('((a)(b)c)(d)', 'abcd', '$+[2]', '1', '');
  y('((a)(b)c)(d)', 'abcd', '$-[3]', '1', '');
  y('((a)(b)c)(d)', 'abcd', '$+[3]', '2', '');
  y('((a)(b)c)(d)', 'abcd', '$-[4]', '3', '');
  y('((a)(b)c)(d)', 'abcd', '$+[4]', '4', '');
  y('[a-zA-Z_][a-zA-Z0-9_]*', 'alpha', '$&', 'alpha', '');
  y('^a(bc+|b[eh])g|.h$', 'abh', '$&-$1', 'bh-', '');
  y('(bc+d$|ef*g.|h?i(j|k))', 'effgz', '$&-$1-$2', 'effgz-effgz-', '');
  y('(bc+d$|ef*g.|h?i(j|k))', 'ij', '$&-$1-$2', 'ij-ij-j', '');
  n('(bc+d$|ef*g.|h?i(j|k))', 'effg', '');
  n('(bc+d$|ef*g.|h?i(j|k))', 'bcdd', '');
  y('(bc+d$|ef*g.|h?i(j|k))', 'reffgz', '$&-$1-$2', 'effgz-effgz-', '');
  y('((((((((((a))))))))))', 'a', '$10', 'a', '');
  y('((((((((((a))))))))))', 'a', '$-[0]', '0', '');
  y('((((((((((a))))))))))', 'a', '$+[0]', '1', '');
  y('((((((((((a))))))))))', 'a', '$-[10]', '0', '');
  y('((((((((((a))))))))))', 'a', '$+[10]', '1', '');
  y('((((((((((a))))))))))\10', 'aa', '$&', 'aa', '');
  n('((((((((((a))))))))))!', 'aa', '');
  y('((((((((((a))))))))))!', 'a!', '$&', 'a!', '');
  y('(((((((((a)))))))))', 'a', '$&', 'a', '');
  n('multiple words of text', 'uh-uh', '');
  y('multiple words', 'multiple words, yeah', '$&', 'multiple words', '');
  y('(.*)c(.*)', 'abcde', '$&-$1-$2', 'abcde-ab-de', '');
  y('\((.*), (.*)\)', '(a, b)', '($2, $1)', '(b, a)', '');
  n('[k]', 'ab', '');
  y('abcd', 'abcd', '$&-\$&-\\$&', 'abcd-\$&-\\abcd', '');
  y('a(bc)d', 'abcd', '$1-\$1-\\$1', 'bc-\$1-\\bc', '');
  y('a[-]?c', 'ac', '$&', 'ac', '');
  y('(abc)\1', 'abcabc', '$1', 'abc', '');
  y('([a-c]*)\1', 'abcabc', '$1', 'abc', '');
  c('\1', '-', '');
  c('\2', '-', '');
  c('\g1', '-', '');
  c('\g-1', '-', '');
  c('\g{1}', '-', '');
  c('\g{-1}', '-', '');
  c('\g0', '-', '');
  c('\g-0', '-', '');
  c('\g{0}', '-', '');
  c('\g{-0}', '-', '');
  y('(a)|\1', 'a', '-', '-', '');
  n('(a)|\1', 'x', '');
  n('(?:(b)?a)\1', 'a', '');
  c('(a)|\2', '-', '');
  y('(([a-c])b*?\2)*', 'ababbbcbc', '$&-$1-$2', 'ababb-bb-b', '');
  y('(([a-c])b*?\2){3}', 'ababbbcbc', '$&-$1-$2', 'ababbbcbc-cbc-c', '');
  n('((\3|b)\2(a)x)+', 'aaxabxbaxbbx', '');
  y('((\3|b)\2(a)x)+', 'aaaxabaxbaaxbbax', '$&-$1-$2-$3', 'bbax-bbax-b-a', '');
  y('((\3|b)\2(a)){2,}', 'bbaababbabaaaaabbaaaabba', '$&-$1-$2-$3', 'bbaaaabba-bba-b-a', '');
//Bug #3589 - up to perl-5.6.0 matches incorrectly, from 5.6.1 not anymore
  n('^((.)?a\2)+$', 'babadad', '');
  y('(a)|(b)', 'b', '$-[0]', '0', '');
  y('(a)|(b)', 'b', '$+[0]', '1', '');
  y('(a)|(b)', 'b', 'x$-[1]', 'x', '');
  y('(a)|(b)', 'b', 'x$+[1]', 'x', '');
  y('(a)|(b)', 'b', '$-[2]', '0', '');
  y('(a)|(b)', 'b', '$+[2]', '1', '');
  y('abc', 'ABC', '$&', 'ABC', '', [roIgnoreCase]);
  n('abc', 'XBC', '', [roIgnoreCase]);
  n('abc', 'AXC', '', [roIgnoreCase]);
  n('abc', 'ABX', '', [roIgnoreCase]);
  y('abc', 'XABCY', '$&', 'ABC', '', [roIgnoreCase]);
  y('abc', 'ABABC', '$&', 'ABC', '', [roIgnoreCase]);
  y('ab*c', 'ABC', '$&', 'ABC', '', [roIgnoreCase]);
  y('ab*bc', 'ABC', '$&', 'ABC', '', [roIgnoreCase]);
  y('ab*bc', 'ABBC', '$&', 'ABBC', '', [roIgnoreCase]);
  y('ab*?bc', 'ABBBBC', '$&', 'ABBBBC', '', [roIgnoreCase]);
  y('ab{0,}?bc', 'ABBBBC', '$&', 'ABBBBC', '', [roIgnoreCase]);
  y('ab+?bc', 'ABBC', '$&', 'ABBC', '', [roIgnoreCase]);
  n('ab+bc', 'ABC', '', [roIgnoreCase]);
  n('ab+bc', 'ABQ', '', [roIgnoreCase]);
  n('ab{1,}bc', 'ABQ', '', [roIgnoreCase]);
  y('ab+bc', 'ABBBBC', '$&', 'ABBBBC', '', [roIgnoreCase]);
  y('ab{1,}?bc', 'ABBBBC', '$&', 'ABBBBC', '', [roIgnoreCase]);
  y('ab{1,3}?bc', 'ABBBBC', '$&', 'ABBBBC', '', [roIgnoreCase]);
  y('ab{3,4}?bc', 'ABBBBC', '$&', 'ABBBBC', '', [roIgnoreCase]);
  n('ab{4,5}?bc', 'ABBBBC', '', [roIgnoreCase]);
  y('ab??bc', 'ABBC', '$&', 'ABBC', '', [roIgnoreCase]);
  y('ab??bc', 'ABC', '$&', 'ABC', '', [roIgnoreCase]);
  y('ab{0,1}?bc', 'ABC', '$&', 'ABC', '', [roIgnoreCase]);
  n('ab??bc', 'ABBBBC', '', [roIgnoreCase]);
  y('ab??c', 'ABC', '$&', 'ABC', '', [roIgnoreCase]);
  y('ab{0,1}?c', 'ABC', '$&', 'ABC', '', [roIgnoreCase]);
  y('^abc$', 'ABC', '$&', 'ABC', '', [roIgnoreCase]);
  n('^abc$', 'ABCC', '', [roIgnoreCase]);
  y('^abc', 'ABCC', '$&', 'ABC', '', [roIgnoreCase]);
  n('^abc$', 'AABC', '', [roIgnoreCase]);
  y('abc$', 'AABC', '$&', 'ABC', '', [roIgnoreCase]);
  y('^', 'ABC', '$&', '', '', [roIgnoreCase]);
  y('$', 'ABC', '$&', '', '', [roIgnoreCase]);
  y('a.c', 'ABC', '$&', 'ABC', '', [roIgnoreCase]);
  y('a.c', 'AXC', '$&', 'AXC', '', [roIgnoreCase]);
  y('a\Nc', 'ABC', '$&', 'ABC', '', [roIgnoreCase]);
  y('a.*?c', 'AXYZC', '$&', 'AXYZC', '', [roIgnoreCase]);
  n('a.*c', 'AXYZD', '', [roIgnoreCase]);
  n('a[bc]d', 'ABC', '', [roIgnoreCase]);
  y('a[bc]d', 'ABD', '$&', 'ABD', '', [roIgnoreCase]);
  n('a[b-d]e', 'ABD', '', [roIgnoreCase]);
  y('a[b-d]e', 'ACE', '$&', 'ACE', '', [roIgnoreCase]);
  y('a[b-d]', 'AAC', '$&', 'AC', '', [roIgnoreCase]);
  y('a[-b]', 'A-', '$&', 'A-', '', [roIgnoreCase]);
  y('a[b-]', 'A-', '$&', 'A-', '', [roIgnoreCase]);
  c('a[b-a]', '-', '', [roIgnoreCase]);
  c('a[]b', '-', '', [roIgnoreCase]);
  c('a[', '-', '', [roIgnoreCase]);
  y('a]', 'A]', '$&', 'A]', '', [roIgnoreCase]);
  y('a[]]b', 'A]B', '$&', 'A]B', '', [roIgnoreCase]);
  y('a[^bc]d', 'AED', '$&', 'AED', '', [roIgnoreCase]);
  n('a[^bc]d', 'ABD', '', [roIgnoreCase]);
  y('a[^-b]c', 'ADC', '$&', 'ADC', '', [roIgnoreCase]);
  n('a[^-b]c', 'A-C', '', [roIgnoreCase]);
  n('a[^]b]c', 'A]C', '', [roIgnoreCase]);
  y('a[^]b]c', 'ADC', '$&', 'ADC', '', [roIgnoreCase]);
  y('ab|cd', 'ABC', '$&', 'AB', '', [roIgnoreCase]);
  y('ab|cd', 'ABCD', '$&', 'AB', '', [roIgnoreCase]);
  y('()ef', 'DEF', '$&-$1', 'EF-', '', [roIgnoreCase]);
  c('*a', '-', '', [roIgnoreCase]);
  c('(|*)b', '-', '', [roIgnoreCase]);
  c('(*)b', '-', '', [roIgnoreCase]);
  n('$b', 'B', '', [roIgnoreCase]);
  c('a\', '-', '', [roIgnoreCase]);
  y('a\(b', 'A(B', '$&-$1', 'A(B-', '', [roIgnoreCase]);
  y('a\(*b', 'AB', '$&', 'AB', '', [roIgnoreCase]);
  y('a\(*b', 'A((B', '$&', 'A((B', '', [roIgnoreCase]);
  y('a\\b', 'A\\B', '$&', 'A\\B', '', [roIgnoreCase]);
  c('abc)', '-', '', [roIgnoreCase]);
  c('(abc', '-', '', [roIgnoreCase]);
  y('((a))', 'ABC', '$&-$1-$2', 'A-A-A', '', [roIgnoreCase]);
  y('(a)b(c)', 'ABC', '$&-$1-$2', 'ABC-A-C', '', [roIgnoreCase]);
  y('a+b+c', 'AABBABC', '$&', 'ABC', '', [roIgnoreCase]);
  y('a{1,}b{1,}c', 'AABBABC', '$&', 'ABC', '', [roIgnoreCase]);
  c('a**', '-', '', [roIgnoreCase]);
  y('a.+?c', 'ABCABC', '$&', 'ABC', '', [roIgnoreCase]);
  y('a.*?c', 'ABCABC', '$&', 'ABC', '', [roIgnoreCase]);
  y('a.{0,5}?c', 'ABCABC', '$&', 'ABC', '', [roIgnoreCase]);
  y('(a+|b)*', 'AB', '$&-$1', 'AB-B', '', [roIgnoreCase]);
  y('(a+|b){0,}', 'AB', '$&-$1', 'AB-B', '', [roIgnoreCase]);
  y('(a+|b)+', 'AB', '$&-$1', 'AB-B', '', [roIgnoreCase]);
  y('(a+|b){1,}', 'AB', '$&-$1', 'AB-B', '', [roIgnoreCase]);
  y('(a+|b)?', 'AB', '$&-$1', 'A-A', '', [roIgnoreCase]);
  y('(a+|b){0,1}', 'AB', '$&-$1', 'A-A', '', [roIgnoreCase]);
  y('(a+|b){0,1}?', 'AB', '$&-$1', '-', '', [roIgnoreCase]);
  c(')(', '-', '', [roIgnoreCase]);
  y('[^ab]*', 'CDE', '$&', 'CDE', '', [roIgnoreCase]);
  n('abc', '', '', [roIgnoreCase]);
  y('a*', '', '$&', '', '', [roIgnoreCase]);
  y('([abc])*d', 'ABBBCD', '$&-$1', 'ABBBCD-C', '', [roIgnoreCase]);
  y('([abc])*bcd', 'ABCD', '$&-$1', 'ABCD-A', '', [roIgnoreCase]);
  y('a|b|c|d|e', 'E', '$&', 'E', '', [roIgnoreCase]);
  y('(a|b|c|d|e)f', 'EF', '$&-$1', 'EF-E', '', [roIgnoreCase]);
  y('abcd*efg', 'ABCDEFG', '$&', 'ABCDEFG', '', [roIgnoreCase]);
  y('ab*', 'XABYABBBZ', '$&', 'AB', '', [roIgnoreCase]);
  y('ab*', 'XAYABBBZ', '$&', 'A', '', [roIgnoreCase]);
  y('(ab|cd)e', 'ABCDE', '$&-$1', 'CDE-CD', '', [roIgnoreCase]);
  y('[abhgefdc]ij', 'HIJ', '$&', 'HIJ', '', [roIgnoreCase]);
  n('^(ab|cd)e', 'ABCDE', '', [roIgnoreCase]);
  y('(abc|)ef', 'ABCDEF', '$&-$1', 'EF-', '', [roIgnoreCase]);
  y('(a|b)c*d', 'ABCD', '$&-$1', 'BCD-B', '', [roIgnoreCase]);
  y('(ab|ab*)bc', 'ABC', '$&-$1', 'ABC-A', '', [roIgnoreCase]);
  y('a([bc]*)c*', 'ABC', '$&-$1', 'ABC-BC', '', [roIgnoreCase]);
  y('a([bc]*)(c*d)', 'ABCD', '$&-$1-$2', 'ABCD-BC-D', '', [roIgnoreCase]);
  y('a([bc]+)(c*d)', 'ABCD', '$&-$1-$2', 'ABCD-BC-D', '', [roIgnoreCase]);
  y('a([bc]*)(c+d)', 'ABCD', '$&-$1-$2', 'ABCD-B-CD', '', [roIgnoreCase]);
  y('a[bcd]*dcdcde', 'ADCDCDE', '$&', 'ADCDCDE', '', [roIgnoreCase]);
  n('a[bcd]+dcdcde', 'ADCDCDE', '', [roIgnoreCase]);
  y('(ab|a)b*c', 'ABC', '$&-$1', 'ABC-AB', '', [roIgnoreCase]);
  y('((a)(b)c)(d)', 'ABCD', '$1-$2-$3-$4', 'ABC-A-B-D', '', [roIgnoreCase]);
  y('[a-zA-Z_][a-zA-Z0-9_]*', 'ALPHA', '$&', 'ALPHA', '', [roIgnoreCase]);
  y('^a(bc+|b[eh])g|.h$', 'ABH', '$&-$1', 'BH-', '', [roIgnoreCase]);
  y('(bc+d$|ef*g.|h?i(j|k))', 'EFFGZ', '$&-$1-$2', 'EFFGZ-EFFGZ-', '', [roIgnoreCase]);
  y('(bc+d$|ef*g.|h?i(j|k))', 'IJ', '$&-$1-$2', 'IJ-IJ-J', '', [roIgnoreCase]);
  n('(bc+d$|ef*g.|h?i(j|k))', 'EFFG', '', [roIgnoreCase]);
  n('(bc+d$|ef*g.|h?i(j|k))', 'BCDD', '', [roIgnoreCase]);
  y('(bc+d$|ef*g.|h?i(j|k))', 'REFFGZ', '$&-$1-$2', 'EFFGZ-EFFGZ-', '', [roIgnoreCase]);
  y('((((((((((a))))))))))', 'A', '$10', 'A', '', [roIgnoreCase]);
  y('((((((((((a))))))))))\10', 'AA', '$&', 'AA', '', [roIgnoreCase]);
  n('((((((((((a))))))))))!', 'AA', '', [roIgnoreCase]);
  y('((((((((((a))))))))))!', 'A!', '$&', 'A!', '', [roIgnoreCase]);
  y('(((((((((a)))))))))', 'A', '$&', 'A', '', [roIgnoreCase]);
  y('(?:(?:(?:(?:(?:(?:(?:(?:(?:(a))))))))))', 'A', '$1', 'A', '', [roIgnoreCase]);
  y('(?:(?:(?:(?:(?:(?:(?:(?:(?:(a|b|c))))))))))', 'C', '$1', 'C', '', [roIgnoreCase]);
  n('multiple words of text', 'UH-UH', '', [roIgnoreCase]);
  y('multiple words', 'MULTIPLE WORDS, YEAH', '$&', 'MULTIPLE WORDS', '', [roIgnoreCase]);
  y('(.*)c(.*)', 'ABCDE', '$&-$1-$2', 'ABCDE-AB-DE', '', [roIgnoreCase]);
  y('\((.*), (.*)\)', '(A, B)', '($2, $1)', '(B, A)', '', [roIgnoreCase]);
  n('[k]', 'AB', '', [roIgnoreCase]);
  y('abcd', 'ABCD', '$&-\$&-\\$&', 'ABCD-\$&-\\ABCD', '', [roIgnoreCase]);
  y('a(bc)d', 'ABCD', '$1-\$1-\\$1', 'BC-\$1-\\BC', '', [roIgnoreCase]);
  y('a[-]?c', 'AC', '$&', 'AC', '', [roIgnoreCase]);
  y('(abc)\1', 'ABCABC', '$1', 'ABC', '', [roIgnoreCase]);
  y('([a-c]*)\1', 'ABCABC', '$1', 'ABC', '', [roIgnoreCase]);
  y('a(?!b).', 'abad', '$&', 'ad', '');
  y('(?=)a', 'a', '$&', 'a', '');
  y('a(?=d).', 'abad', '$&', 'ad', '');
  y('a(?=c|d).', 'abad', '$&', 'ad', '');
  y('a(?:b|c|d)(.)', 'ace', '$1', 'e', '');
  y('a(?:b|c|d)*(.)', 'ace', '$1', 'e', '');
  y('a(?:b|c|d)+?(.)', 'ace', '$1', 'e', '');
  y('a(?:b|c|d)+?(.)', 'acdbcdbe', '$1', 'd', '');
  y('a(?:b|c|d)+(.)', 'acdbcdbe', '$1', 'e', '');
  y('a(?:b|c|d){2}(.)', 'acdbcdbe', '$1', 'b', '');
  y('a(?:b|c|d){4,5}(.)', 'acdbcdbe', '$1', 'b', '');
  y('a(?:b|c|d){4,5}?(.)', 'acdbcdbe', '$1', 'd', '');
  y('((foo)|(bar))*', 'foobar', '$1-$2-$3', 'bar-foo-bar', '');
  c(':(?:', '-', '');
  y('a(?:b|c|d){6,7}(.)', 'acdbcdbe', '$1', 'e', '');
  y('a(?:b|c|d){6,7}?(.)', 'acdbcdbe', '$1', 'e', '');
  y('a(?:b|c|d){5,6}(.)', 'acdbcdbe', '$1', 'e', '');
  y('a(?:b|c|d){5,6}?(.)', 'acdbcdbe', '$1', 'b', '');
  y('a(?:b|c|d){5,7}(.)', 'acdbcdbe', '$1', 'e', '');
  y('a(?:b|c|d){5,7}?(.)', 'acdbcdbe', '$1', 'b', '');
  y('a(?:b|(c|e){1,2}?|d)+?(.)', 'ace', '$1$2', 'ce', '');
  y('^(.+)?B', 'AB', '$1', 'A', '');
  y('^([^a-z])|(\^)$', '.', '$1', '.', '');
  y('^[<>]&', '<&OUT', '$&', '<&', '');
  y('^(a\1?){4}$', 'aaaaaaaaaa', '$1', 'aaaa', '');
  n('^(a\1?){4}$', 'aaaaaaaaa', '');
  n('^(a\1?){4}$', 'aaaaaaaaaaa', '');
  y('^(a(?(1)\1)){4}$', 'aaaaaaaaaa', '$1', 'aaaa', '');
  n('^(a(?(1)\1)){4}$', 'aaaaaaaaa', '');
  n('^(a(?(1)\1)){4}$', 'aaaaaaaaaaa', '');
  y('((a{4})+)', 'aaaaaaaaa', '$1', 'aaaaaaaa', '');
  y('(((aa){2})+)', 'aaaaaaaaaa', '$1', 'aaaaaaaa', '');
  y('(((a{2}){2})+)', 'aaaaaaaaaa', '$1', 'aaaaaaaa', '');
  y('(?:(f)(o)(o)|(b)(a)(r))*', 'foobar', '$1:$2:$3:$4:$5:$6', 'f:o:o:b:a:r', '');
  y('(?<=a)b', 'ab', '$&', 'b', '');
  n('(?<=a)b', 'cb', '');
  n('(?<=a)b', 'b', '');
  y('(?<!c)b', 'ab', '$&', 'b', '');
  n('(?<!c)b', 'cb', '');
  y('(?<!c)b', 'b', '-', '-', '');
  y('(?<!c)b', 'b', '$&', 'b', '');
  c('(?<%)b', '-', '');
  y('(?:..)*a', 'aba', '$&', 'aba', '');
  y('(?:..)*?a', 'aba', '$&', 'a', '');
  y('^(?:b|a(?=(.)))*\1', 'abc', '$&', 'ab', '');
  y('^(){3,5}', 'abc', 'a$1', 'a', '');
  y('^(a+)*ax', 'aax', '$1', 'a', '');
  y('^((a|b)+)*ax', 'aax', '$1', 'a', '');
  y('^((a|bc)+)*ax', 'aax', '$1', 'a', '');
  y('(a|x)*ab', 'cab', 'y$1', 'y', '');
  y('(a)*ab', 'cab', 'y$1', 'y', '');
  y('(?:(?i)a)b', 'ab', '$&', 'ab', '');
  y('((?i)a)b', 'ab', '$&:$1', 'ab:a', '');
  y('(?:(?i)a)b', 'Ab', '$&', 'Ab', '');
  y('((?i)a)b', 'Ab', '$&:$1', 'Ab:A', '');
  n('(?:(?i)a)b', 'aB', '');
  n('((?i)a)b', 'aB', '');
  y('(?i:a)b', 'ab', '$&', 'ab', '');
  y('((?i:a))b', 'ab', '$&:$1', 'ab:a', '');
  y('(?i:a)b', 'Ab', '$&', 'Ab', '');
  y('((?i:a))b', 'Ab', '$&:$1', 'Ab:A', '');
  n('(?i:a)b', 'aB', '');
  n('((?i:a))b', 'aB', '');
  y('(?:(?-i)a)b', 'ab', '$&', 'ab', '', [roIgnoreCase]);
  y('((?-i)a)b', 'ab', '$&:$1', 'ab:a', '', [roIgnoreCase]);
  y('(?:(?-i)a)b', 'aB', '$&', 'aB', '', [roIgnoreCase]);
  y('((?-i)a)b', 'aB', '$&:$1', 'aB:a', '', [roIgnoreCase]);
  n('(?:(?-i)a)b', 'Ab', '', [roIgnoreCase]);
  n('((?-i)a)b', 'Ab', '', [roIgnoreCase]);
  y('(?:(?-i)a)b', 'aB', '$&', 'aB', '', [roIgnoreCase]);
  y('((?-i)a)b', 'aB', '$1', 'a', '', [roIgnoreCase]);
  n('(?:(?-i)a)b', 'AB', '', [roIgnoreCase]);
  n('((?-i)a)b', 'AB', '', [roIgnoreCase]);
  y('(?-i:a)b', 'ab', '$&', 'ab', '', [roIgnoreCase]);
  y('((?-i:a))b', 'ab', '$&:$1', 'ab:a', '', [roIgnoreCase]);
  y('(?-i:a)b', 'aB', '$&', 'aB', '', [roIgnoreCase]);
  y('((?-i:a))b', 'aB', '$&:$1', 'aB:a', '', [roIgnoreCase]);
  n('(?-i:a)b', 'Ab', '', [roIgnoreCase]);
  n('((?-i:a))b', 'Ab', '', [roIgnoreCase]);
  y('(?-i:a)b', 'aB', '$&', 'aB', '', [roIgnoreCase]);
  y('((?-i:a))b', 'aB', '$1', 'a', '', [roIgnoreCase]);
  n('(?-i:a)b', 'AB', '', [roIgnoreCase]);
  n('((?-i:a))b', 'AB', '', [roIgnoreCase]);
  n('((?-i:a.))b', 'a\nB', '', [roIgnoreCase]);
  n('((?-i:a\N))b', 'a\nB', '', [roIgnoreCase]);
  y('((?s-i:a.))b', 'a\nB', '$1', 'a\n', '', [roIgnoreCase]);
  n('((?s-i:a\N))b', 'a\nB', '', [roIgnoreCase]);
  n('((?s-i:a.))b', 'B\nB', '', [roIgnoreCase]);
  n('((?s-i:a\N))b', 'B\nB', '', [roIgnoreCase]);
  y('(?:c|d)(?:)(?:a(?:)(?:b)(?:b(?:))(?:b(?:)(?:b)))', 'cabbbb', '$&', 'cabbbb', '');
  y('(?:c|d)(?:)(?:aaaaaaaa(?:)(?:bbbbbbbb)(?:bbbbbbbb(?:))(?:bbbbbbbb(?:)(?:bbbbbbbb)))', 'caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb', '$&', 'caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb', '');
  y('(ab)\d\1', 'Ab4ab', '$1', 'Ab', '', [roIgnoreCase]);
  y('(ab)\d\1', 'ab4Ab', '$1', 'ab', '', [roIgnoreCase]);
  y('foo\w*\d{4}baz', 'foobar1234baz', '$&', 'foobar1234baz', '');








  y('x(~~)*(?:(?:F)?)?', 'x~~', '-', '-', '');
  y('^a(?#xxx){3}c', 'aaac', '$&', 'aaac', '');
  y('^a (?#xxx) (?#yyy) {3}c', 'aaac', '$&', 'aaac', '', [roExtended]);
  n('(?<![cd])b', 'dbcb', '');
  y('(?<![cd])[ab]', 'dbaacb', '$&', 'a', '');
  n('(?<!(c|d))b', 'dbcb', '');
  y('(?<!(c|d))[ab]', 'dbaacb', '$&', 'a', '');
  y('(?<!cd)[ab]', 'cdaccb', '$&', 'b', '');
  n('^(?:a?b?)*$', 'a--', '');
  y('((?s)^a(.))((?m)^b$)', 'a\nb\nc\n', '$1;$2;$3', 'a\n;\n;b', '');
  y('((?m)^b$)', 'a\nb\nc\n', '$1', 'b', '');
  y('(?m)^b', 'a\nb\n', '$&', 'b', '');
  y('(?m)^(b)', 'a\nb\n', '$1', 'b', '');
  y('((?m)^b)', 'a\nb\n', '$1', 'b', '');
  y('\n((?m)^b)', 'a\nb\n', '$1', 'b', '');
  y('((?s).)c(?!.)', 'a\nb\nc\n', '$1', '\n', '');
  y('((?s).)c(?!.)', 'a\nb\nc\n', '$1:$&', '\n:\nc', '');
  y('((?s)b.)c(?!.)', 'a\nb\nc\n', '$1', 'b\n', '');
  y('((?s)b.)c(?!.)', 'a\nb\nc\n', '$1:$&', 'b\n:b\nc', '');
  y('((?s)b.)c(?!\N)', 'a\nb\nc\n', '$1:$&', 'b\n:b\nc', '');
  y('(b.)c(?!\N)', 'a\nb\nc\n', '$1:$&', 'b\n:b\nc', '', [roSingleLine]);
  n('^b', 'a\nb\nc\n', '');
  n('()^b', 'a\nb\nc\n', '');
  y('((?m)^b)', 'a\nb\nc\n', '$1', 'b', '');
  n('(?(1)a|b)', 'a', '');
  y('(?(1)b|a)', 'a', '$&', 'a', '');
  n('(x)?(?(1)a|b)', 'a', '');
  y('(x)?(?(1)b|a)', 'a', '$&', 'a', '');
  y('()?(?(1)b|a)', 'a', '$&', 'a', '');
  n('()(?(1)b|a)', 'a', '');
  y('()?(?(1)a|b)', 'a', '$&', 'a', '');
  y('^(\()?blah(?(1)(\)))$', '(blah)', '$2', ')', '');
  y('^(\()?blah(?(1)(\)))$', 'blah', '($2)', '()', '');
  n('^(\()?blah(?(1)(\)))$', 'blah)', '');
  n('^(\()?blah(?(1)(\)))$', '(blah', '');
  y('^(\(+)?blah(?(1)(\)))$', '(blah)', '$2', ')', '');
  y('^(\(+)?blah(?(1)(\)))$', 'blah', '($2)', '()', '');
  n('^(\(+)?blah(?(1)(\)))$', 'blah)', '');
  n('^(\(+)?blah(?(1)(\)))$', '(blah', '');
  c('(?(1?)a|b)', '-', '');
  c('(?(1)a|b|c)', '-', '');




  n('(?(?!a)a|b)', 'a', '');
  y('(?(?!a)b|a)', 'a', '$&', 'a', '');
  n('(?(?=a)b|a)', 'a', '');
  y('(?(?=a)a|b)', 'a', '$&', 'a', '');
  n('(?(?!\x{100})\x{100}|b)', '\x{100}', '');
  y('(?(?!\x{100})b|\x{100})', '\x{100}', '$&', '\x{100}', '');
  n('(?(?=\x{100})b|\x{100})', '\x{100}', '');
  y('(?(?=\x{100})\x{100}|b)', '\x{100}', '$&', '\x{100}', '');
  y('(?=(a+?))(\1ab)', 'aaab', '$2', 'aab', '');
  n('^(?=(a+?))\1ab', 'aaab', '');
  y('(\w+:)+', 'one:', '$1', 'one:', '');
  y('$(?<=^(a))', 'a', '$1', 'a', '');
  y('(?=(a+?))(\1ab)', 'aaab', '$2', 'aab', '');
  n('^(?=(a+?))\1ab', 'aaab', '');
  n('([\w:]+::)?(\w+)$', 'abcd:', '');
  y('([\w:]+::)?(\w+)$', 'abcd', '$1-$2', '-abcd', '');
  y('([\w:]+::)?(\w+)$', 'xy:z:::abcd', '$1-$2', 'xy:z:::-abcd', '');
  y('^[^bcd]*(c+)', 'aexycd', '$1', 'c', '');
  y('(a*)b+', 'caab', '$1', 'aa', '');
  n('([\w:]+::)?(\w+)$', 'abcd:', '');
  y('([\w:]+::)?(\w+)$', 'abcd', '$1-$2', '-abcd', '');
  y('([\w:]+::)?(\w+)$', 'xy:z:::abcd', '$1-$2', 'xy:z:::-abcd', '');
  y('^[^bcd]*(c+)', 'aexycd', '$1', 'c', '');


  n('(>a+)ab', 'aaab', '');
  y('(?>a+)b', 'aaab', '-', '-', '');
  y('([[:]+)', 'a:[b]:', '$1', ':[', '');
  y('([[=]+)', 'a=[b]=', '$1', '=[', '');
  y('([[.]+)', 'a.[b].', '$1', '.[', '');
  c('[a[:xyz:', '-', '');
  c('[a[:xyz:]', '-', '');
  y('[a[:]b[:c]', 'abc', '$&', 'abc', '');
  c('([a[:xyz:]b]+)', '-', '');
  y('([[:alpha:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'ABcd', '');
  y('([[:alnum:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'ABcd01Xy', '');
  y('([[:ascii:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'ABcd01Xy__--  \x{0}', '');
  y('([[:cntrl:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', '\x{0}', '');
  y('([[:digit:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', '01', '');
  y('([[:graph:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'ABcd01Xy__--', '');
  y('([[:lower:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'cd', '');
  y('([[:print:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'ABcd01Xy__--  ', '');
  y('([[:punct:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', '__--', '');
  y('([[:space:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', '  ', '');
  y('([[:word:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'ABcd01Xy__', '');
  y('([[:upper:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'AB', '');
  y('([[:xdigit:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'ABcd01', '');
  y('([[:^alpha:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', '01', '');
  y('((?a)[[:^alnum:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', '__--  \x{0}\x{ffff}', '');
  y('([[:^ascii:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', '\x{ffff}', '');
  y('([[:^cntrl:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'ABcd01Xy__--  ', '');
  y('([[:^digit:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'ABcd', '');
  y('([[:^lower:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'AB', '');
  y('((?a)[[:^print:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', '\x{0}\x{ffff}', '');
  y('([[:^punct:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'ABcd01Xy', '');
  y('([[:^space:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'ABcd01Xy__--', '');
  y('((?a)[[:^word:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', '--  \x{0}\x{ffff}', '');
  y('([[:^upper:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'cd01', '');
  y('([[:^xdigit:]]+)', 'ABcd01Xy__--  \x{0}\x{ffff}', '$1', 'Xy__--  \x{0}\x{ffff}', '');
  c('[[:foo:]]', '-', '');
  c('[[:^foo:]]', '-', '');
  y('((?>a+)b)', 'aaab', '$1', 'aaab', '');
  y('(?>(a+))b', 'aaab', '$1', 'aaa', '');
  y('((?>[^()]+)|\([^()]*\))+', '((abc(ade)ufh()()x', '$&', 'abc(ade)ufh()()x', '');
  c('(?<=x+)y', '-', '');
//  y('((def){37,17})?ABC', 'ABC', '$&', 'ABC', '');
  y('\Z', 'a\nb\n', '$-[0]', '3', '');
  y('\z', 'a\nb\n', '$-[0]', '4', '');
  y('$', 'a\nb\n', '$-[0]', '3', '');
  y('\Z', 'b\na\n', '$-[0]', '3', '');
  y('\z', 'b\na\n', '$-[0]', '4', '');
  y('$', 'b\na\n', '$-[0]', '3', '');
  y('\Z', 'b\na', '$-[0]', '3', '');
  y('\z', 'b\na', '$-[0]', '3', '');
  y('$', 'b\na', '$-[0]', '3', '');
  y('\Z', 'a\nb\n', '$-[0]', '3', '', [roMultiLine]);
  y('\z', 'a\nb\n', '$-[0]', '4', '', [roMultiLine]);
  y('$', 'a\nb\n', '$-[0]', '1', '', [roMultiLine]);
  y('\Z', 'b\na\n', '$-[0]', '3', '', [roMultiLine]);
  y('\z', 'b\na\n', '$-[0]', '4', '', [roMultiLine]);
  y('$', 'b\na\n', '$-[0]', '1', '', [roMultiLine]);
  y('\Z', 'b\na', '$-[0]', '3', '', [roMultiLine]);
  y('\z', 'b\na', '$-[0]', '3', '', [roMultiLine]);
  y('$', 'b\na', '$-[0]', '1', '', [roMultiLine]);
  n('a\Z', 'a\nb\n', '');
  n('a\z', 'a\nb\n', '');
  n('a$', 'a\nb\n', '');
  y('a\Z', 'b\na\n', '$-[0]', '2', '');
  n('a\z', 'b\na\n', '');
  y('a$', 'b\na\n', '$-[0]', '2', '');
  y('a\Z', 'b\na', '$-[0]', '2', '');
  y('a\z', 'b\na', '$-[0]', '2', '');
  y('a$', 'b\na', '$-[0]', '2', '');
  n('a\Z', 'a\nb\n', '', [roMultiLine]);
  n('a\z', 'a\nb\n', '', [roMultiLine]);
  y('a$', 'a\nb\n', '$-[0]', '0', '', [roMultiLine]);
  y('a\Z', 'b\na\n', '$-[0]', '2', '', [roMultiLine]);
  n('a\z', 'b\na\n', '', [roMultiLine]);
  y('a$', 'b\na\n', '$-[0]', '2', '', [roMultiLine]);
  y('a\Z', 'b\na', '$-[0]', '2', '', [roMultiLine]);
  y('a\z', 'b\na', '$-[0]', '2', '', [roMultiLine]);
  y('a$', 'b\na', '$-[0]', '2', '', [roMultiLine]);
  n('aa\Z', 'aa\nb\n', '');
  n('aa\z', 'aa\nb\n', '');
  n('aa$', 'aa\nb\n', '');
  y('aa\Z', 'b\naa\n', '$-[0]', '2', '');
  n('aa\z', 'b\naa\n', '');
  y('aa$', 'b\naa\n', '$-[0]', '2', '');
  y('aa\Z', 'b\naa', '$-[0]', '2', '');
  y('aa\z', 'b\naa', '$-[0]', '2', '');
  y('aa$', 'b\naa', '$-[0]', '2', '');
  n('aa\Z', 'aa\nb\n', '', [roMultiLine]);
  n('aa\z', 'aa\nb\n', '', [roMultiLine]);
  y('aa$', 'aa\nb\n', '$-[0]', '0', '', [roMultiLine]);
  y('aa\Z', 'b\naa\n', '$-[0]', '2', '', [roMultiLine]);
  n('aa\z', 'b\naa\n', '', [roMultiLine]);
  y('aa$', 'b\naa\n', '$-[0]', '2', '', [roMultiLine]);
  y('aa\Z', 'b\naa', '$-[0]', '2', '', [roMultiLine]);
  y('aa\z', 'b\naa', '$-[0]', '2', '', [roMultiLine]);
  y('aa$', 'b\naa', '$-[0]', '2', '', [roMultiLine]);
  n('aa\Z', 'ac\nb\n', '');
  n('aa\z', 'ac\nb\n', '');
  n('aa$', 'ac\nb\n', '');
  n('aa\Z', 'b\nac\n', '');
  n('aa\z', 'b\nac\n', '');
  n('aa$', 'b\nac\n', '');
  n('aa\Z', 'b\nac', '');
  n('aa\z', 'b\nac', '');
  n('aa$', 'b\nac', '');
  n('aa\Z', 'ac\nb\n', '', [roMultiLine]);
  n('aa\z', 'ac\nb\n', '', [roMultiLine]);
  n('aa$', 'ac\nb\n', '', [roMultiLine]);
  n('aa\Z', 'b\nac\n', '', [roMultiLine]);
  n('aa\z', 'b\nac\n', '', [roMultiLine]);
  n('aa$', 'b\nac\n', '', [roMultiLine]);
  n('aa\Z', 'b\nac', '', [roMultiLine]);
  n('aa\z', 'b\nac', '', [roMultiLine]);
  n('aa$', 'b\nac', '', [roMultiLine]);
  n('aa\Z', 'ca\nb\n', '');
  n('aa\z', 'ca\nb\n', '');
  n('aa$', 'ca\nb\n', '');
  n('aa\Z', 'b\nca\n', '');
  n('aa\z', 'b\nca\n', '');
  n('aa$', 'b\nca\n', '');
  n('aa\Z', 'b\nca', '');
  n('aa\z', 'b\nca', '');
  n('aa$', 'b\nca', '');
  n('aa\Z', 'ca\nb\n', '', [roMultiLine]);
  n('aa\z', 'ca\nb\n', '', [roMultiLine]);
  n('aa$', 'ca\nb\n', '', [roMultiLine]);
  n('aa\Z', 'b\nca\n', '', [roMultiLine]);
  n('aa\z', 'b\nca\n', '', [roMultiLine]);
  n('aa$', 'b\nca\n', '', [roMultiLine]);
  n('aa\Z', 'b\nca', '', [roMultiLine]);
  n('aa\z', 'b\nca', '', [roMultiLine]);
  n('aa$', 'b\nca', '', [roMultiLine]);
  n('ab\Z', 'ab\nb\n', '');
  n('ab\z', 'ab\nb\n', '');
  n('ab$', 'ab\nb\n', '');
  y('ab\Z', 'b\nab\n', '$-[0]', '2', '');
  n('ab\z', 'b\nab\n', '');
  y('ab$', 'b\nab\n', '$-[0]', '2', '');
  y('ab\Z', 'b\nab', '$-[0]', '2', '');
  y('ab\z', 'b\nab', '$-[0]', '2', '');
  y('ab$', 'b\nab', '$-[0]', '2', '');
  n('ab\Z', 'ab\nb\n', '', [roMultiLine]);
  n('ab\z', 'ab\nb\n', '', [roMultiLine]);
  y('ab$', 'ab\nb\n', '$-[0]', '0', '', [roMultiLine]);
  y('ab\Z', 'b\nab\n', '$-[0]', '2', '', [roMultiLine]);
  n('ab\z', 'b\nab\n', '', [roMultiLine]);
  y('ab$', 'b\nab\n', '$-[0]', '2', '', [roMultiLine]);
  y('ab\Z', 'b\nab', '$-[0]', '2', '', [roMultiLine]);
  y('ab\z', 'b\nab', '$-[0]', '2', '', [roMultiLine]);
  y('ab$', 'b\nab', '$-[0]', '2', '', [roMultiLine]);
  n('ab\Z', 'ac\nb\n', '');
  n('ab\z', 'ac\nb\n', '');
  n('ab$', 'ac\nb\n', '');
  n('ab\Z', 'b\nac\n', '');
  n('ab\z', 'b\nac\n', '');
  n('ab$', 'b\nac\n', '');
  n('ab\Z', 'b\nac', '');
  n('ab\z', 'b\nac', '');
  n('ab$', 'b\nac', '');
  n('ab\Z', 'ac\nb\n', '', [roMultiLine]);
  n('ab\z', 'ac\nb\n', '', [roMultiLine]);
  n('ab$', 'ac\nb\n', '', [roMultiLine]);
  n('ab\Z', 'b\nac\n', '', [roMultiLine]);
  n('ab\z', 'b\nac\n', '', [roMultiLine]);
  n('ab$', 'b\nac\n', '', [roMultiLine]);
  n('ab\Z', 'b\nac', '', [roMultiLine]);
  n('ab\z', 'b\nac', '', [roMultiLine]);
  n('ab$', 'b\nac', '', [roMultiLine]);
  n('ab\Z', 'ca\nb\n', '');
  n('ab\z', 'ca\nb\n', '');
  n('ab$', 'ca\nb\n', '');
  n('ab\Z', 'b\nca\n', '');
  n('ab\z', 'b\nca\n', '');
  n('ab$', 'b\nca\n', '');
  n('ab\Z', 'b\nca', '');
  n('ab\z', 'b\nca', '');
  n('ab$', 'b\nca', '');
  n('ab\Z', 'ca\nb\n', '', [roMultiLine]);
  n('ab\z', 'ca\nb\n', '', [roMultiLine]);
  n('ab$', 'ca\nb\n', '', [roMultiLine]);
  n('ab\Z', 'b\nca\n', '', [roMultiLine]);
  n('ab\z', 'b\nca\n', '', [roMultiLine]);
  n('ab$', 'b\nca\n', '', [roMultiLine]);
  n('ab\Z', 'b\nca', '', [roMultiLine]);
  n('ab\z', 'b\nca', '', [roMultiLine]);
  n('ab$', 'b\nca', '', [roMultiLine]);
  n('abb\Z', 'abb\nb\n', '');
  n('abb\z', 'abb\nb\n', '');
  n('abb$', 'abb\nb\n', '');
  y('abb\Z', 'b\nabb\n', '$-[0]', '2', '');
  n('abb\z', 'b\nabb\n', '');
  y('abb$', 'b\nabb\n', '$-[0]', '2', '');
  y('abb\Z', 'b\nabb', '$-[0]', '2', '');
  y('abb\z', 'b\nabb', '$-[0]', '2', '');
  y('abb$', 'b\nabb', '$-[0]', '2', '');
  n('abb\Z', 'abb\nb\n', '', [roMultiLine]);
  n('abb\z', 'abb\nb\n', '', [roMultiLine]);
  y('abb$', 'abb\nb\n', '$-[0]', '0', '', [roMultiLine]);
  y('abb\Z', 'b\nabb\n', '$-[0]', '2', '', [roMultiLine]);
  n('abb\z', 'b\nabb\n', '', [roMultiLine]);
  y('abb$', 'b\nabb\n', '$-[0]', '2', '', [roMultiLine]);
  y('abb\Z', 'b\nabb', '$-[0]', '2', '', [roMultiLine]);
  y('abb\z', 'b\nabb', '$-[0]', '2', '', [roMultiLine]);
  y('abb$', 'b\nabb', '$-[0]', '2', '', [roMultiLine]);
  n('abb\Z', 'ac\nb\n', '');
  n('abb\z', 'ac\nb\n', '');
  n('abb$', 'ac\nb\n', '');
  n('abb\Z', 'b\nac\n', '');
  n('abb\z', 'b\nac\n', '');
  n('abb$', 'b\nac\n', '');
  n('abb\Z', 'b\nac', '');
  n('abb\z', 'b\nac', '');
  n('abb$', 'b\nac', '');
  n('abb\Z', 'ac\nb\n', '', [roMultiLine]);
  n('abb\z', 'ac\nb\n', '', [roMultiLine]);
  n('abb$', 'ac\nb\n', '', [roMultiLine]);
  n('abb\Z', 'b\nac\n', '', [roMultiLine]);
  n('abb\z', 'b\nac\n', '', [roMultiLine]);
  n('abb$', 'b\nac\n', '', [roMultiLine]);
  n('abb\Z', 'b\nac', '', [roMultiLine]);
  n('abb\z', 'b\nac', '', [roMultiLine]);
  n('abb$', 'b\nac', '', [roMultiLine]);
  n('abb\Z', 'ca\nb\n', '');
  n('abb\z', 'ca\nb\n', '');
  n('abb$', 'ca\nb\n', '');
  n('abb\Z', 'b\nca\n', '');
  n('abb\z', 'b\nca\n', '');
  n('abb$', 'b\nca\n', '');
  n('abb\Z', 'b\nca', '');
  n('abb\z', 'b\nca', '');
  n('abb$', 'b\nca', '');
  n('abb\Z', 'ca\nb\n', '', [roMultiLine]);
  n('abb\z', 'ca\nb\n', '', [roMultiLine]);
  n('abb$', 'ca\nb\n', '', [roMultiLine]);
  n('abb\Z', 'b\nca\n', '', [roMultiLine]);
  n('abb\z', 'b\nca\n', '', [roMultiLine]);
  n('abb$', 'b\nca\n', '', [roMultiLine]);
  n('abb\Z', 'b\nca', '', [roMultiLine]);
  n('abb\z', 'b\nca', '', [roMultiLine]);
  n('abb$', 'b\nca', '', [roMultiLine]);
  y('\Aa$', 'a\n\n', '$&', 'a', '', [roMultiLine]);
  y('(^|x)(c)', 'ca', '$2', 'c', '');
  n('a*abc?xyz+pqr{3}ab{2,}xy{4,5}pq{0,6}AB{0,}zz', 'x', '');

  y('round\(((?>[^()]+))\)', '_I(round(xs * sz),1)', '$1', 'xs * sz', '');
  y('((?x:.) )', 'x ', '$1-', 'x -', '');
  y('((?-x:.) )', 'x ', '$1-', 'x-', '', [roExtended]);
  y('foo.bart', 'foo.bart', '-', '-', '');
  y('^d[x][x][x]', 'abcd\ndxxx', '-', '-', '', [roMultiLine]);
  y('.X(.+)+X', 'bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '-', '-', '');
  y('.X(.+)+XX', 'bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '-', '-', '');
  y('.XX(.+)+X', 'bbbbXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '-', '-', '');
  n('.X(.+)+X', 'bbbbXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '');
  n('.X(.+)+XX', 'bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '');
  n('.XX(.+)+X', 'bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '');
  y('.X(.+)+[X]', 'bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '-', '-', '');
  y('.X(.+)+[X][X]', 'bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '-', '-', '');
  y('.XX(.+)+[X]', 'bbbbXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '-', '-', '');
  n('.X(.+)+[X]', 'bbbbXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '');
  n('.X(.+)+[X][X]', 'bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '');
  n('.XX(.+)+[X]', 'bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '');
  y('.[X](.+)+[X]', 'bbbbXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '-', '-', '');
  y('.[X](.+)+[X][X]', 'bbbbXcXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '-', '-', '');
  y('.[X][X](.+)+[X]', 'bbbbXXcXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '-', '-', '');
  n('.[X](.+)+[X]', 'bbbbXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '');
  n('.[X](.+)+[X][X]', 'bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '');
  n('.[X][X](.+)+[X]', 'bbbbXXXaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '');
  y('tt+$', 'xxxtt', '-', '-', '');
  y('([a-\d]+)', 'za-9z', '$1', 'a-9', '');
  c('([a-\d]+)', '-', '');
  y('([\d-z]+)', 'a0-za', '$1', '0-z', '');
  c('([\d-z]+)', '-', '');
  y('([\d-\s]+)', 'a0- z', '$1', '0- ', '');
  c('([\d-\s]+)', '-', '');
  y('([a-[:digit:]]+)', 'za-9z', '$1', 'a-9', '');
  c('([a-[:digit:]]+)', '-', '');
  y('([[:digit:]-z]+)', '=0-z=', '$1', '0-z', '');
  c('([[:digit:]-z]+)', '-', '');
  y('([[:digit:]-[:alpha:]]+)', '=0-z=', '$1', '0-z', '');
  c('([[:digit:]-[:alpha:]]+)', '-', '');
  n('\GX.*X', 'aaaXbX', '');
  y('(\d+\.\d+)', '3.1415926', '$1', '3.1415926', '');
  y('(\ba.{0,10}br)', 'have a web browser', '$1', 'a web br', '');
  n('\.c(pp|xx|c)?$', 'Changes', '', [roIgnoreCase]);
  y('\.c(pp|xx|c)?$', 'IO.c', '-', '-', '', [roIgnoreCase]);
  y('(\.c(pp|xx|c)?$)', 'IO.c', '$1', '.c', '', [roIgnoreCase]);
  n('^([a-z]:)', 'C:/', '');
  y('^\S\s+aa$', '\nx aa', '-', '-', '', [roMultiLine]);
  y('(^|a)b', 'ab', '-', '-', '');
  y('^([ab]*?)(b)?(c)$', 'abac', '-$2-', '--', '');
  n('(\w)?(abc)\1b', 'abcab', '');
  y('^(?:.,){2}c', 'a,b,c', '-', '-', '');
  y('^(.,){2}c', 'a,b,c', '$1', 'b,', '');
  y('^(?:[^,]*,){2}c', 'a,b,c', '-', '-', '');
  y('^([^,]*,){2}c', 'a,b,c', '$1', 'b,', '');
  y('^([^,]*,){3}d', 'aaa,b,c,d', '$1', 'c,', '');
  y('^([^,]*,){3,}d', 'aaa,b,c,d', '$1', 'c,', '');
  y('^([^,]*,){0,3}d', 'aaa,b,c,d', '$1', 'c,', '');
  y('^([^,]{1,3},){3}d', 'aaa,b,c,d', '$1', 'c,', '');
  y('^([^,]{1,3},){3,}d', 'aaa,b,c,d', '$1', 'c,', '');
  y('^([^,]{1,3},){0,3}d', 'aaa,b,c,d', '$1', 'c,', '');
  y('^([^,]{1,},){3}d', 'aaa,b,c,d', '$1', 'c,', '');
  y('^([^,]{1,},){3,}d', 'aaa,b,c,d', '$1', 'c,', '');
  y('^([^,]{1,},){0,3}d', 'aaa,b,c,d', '$1', 'c,', '');
  y('^([^,]{0,3},){3}d', 'aaa,b,c,d', '$1', 'c,', '');
  y('^([^,]{0,3},){3,}d', 'aaa,b,c,d', '$1', 'c,', '');
  y('^([^,]{0,3},){0,3}d', 'aaa,b,c,d', '$1', 'c,', '');
  y('(?i)', '', '-', '-', '');
//  y('(?a:((?u)\w)\W)', '\xC0\xC0', '$&', '\xC0\xC0', '');
  n('(?a:((?u)\w)\W)', '\xC0\xC0', '$&');
  y('(?!\A)x', 'a\nxb\n', '-', '-', '', [roMultiLine]);
  y('^(a(b)?)+$', 'aba', '-$1-$2-', '-a-b-', '');
  y('^(aa(bb)?)+$', 'aabbaa', '-$1-$2-', '-aa-bb-', '');
  y('^.{9}abc.*\n', '123\nabcabcabcabc\n', '-', '-', '', [roMultiLine]);
  y('^(a)?a$', 'a', '-$1-', '--', '');
  n('^(a)?(?(1)a|b)+$', 'a', '');
  y('^(a\1?)(a\1?)(a\2?)(a\3?)$', 'aaaaaa', '$1,$2,$3,$4', 'a,aa,a,aa', '');
  y('^(a\1?){4}$', 'aaaaaa', '$1', 'aa', '');
  y('^(0+)?(?:x(1))?', 'x1', '-', '-', '');
  y('^([0-9a-fA-F]+)(?:x([0-9a-fA-F]+)?)(?:x([0-9a-fA-F]+))?', '012cxx0190', '-', '-', '');
  y('^(b+?|a){1,2}c', 'bbbac', '$1', 'a', '');
  y('^(b+?|a){1,2}c', 'bbbbac', '$1', 'a', '');
  y('\((\w\. \w+)\)', 'cd. (A. Tw)', '-$1-', '-A. Tw-', '');
  y('((?:aaaa|bbbb)cccc)?', 'aaaacccc', '-', '-', '');
  y('((?:aaaa|bbbb)cccc)?', 'bbbbcccc', '-', '-', '');
  y('(a)?(a)+', 'a', '$1:$2', ':a', '-');
  y('(ab)?(ab)+', 'ab', '$1:$2', ':ab', '-');
  y('(abc)?(abc)+', 'abc', '$1:$2', ':abc', '-');
  n('b\s^', 'a\nb\n', '', [roMultiLine]);
  y('\ba', 'a', '-', '-', '');

  n('ab(?i)cd', 'AbCd', '# [ID 20010809.023]');
  y('ab(?i)cd', 'abCd', '-', '-', '');
  y('(A|B)*(?(1)(CD)|(CD))', 'CD', '$2-$3', '-CD', '');
  y('(A|B)*(?(1)(CD)|(CD))', 'ABCD', '$2-$3', 'CD-', '');
  y('(A|B)*?(?(1)(CD)|(CD))', 'CD', '$2-$3', '-CD', '# [ID 20010803.016]');
  y('(A|B)*?(?(1)(CD)|(CD))', 'ABCD', '$2-$3', 'CD-', '');
  n('^(o)(?!.*\1)', 'Oo', '', [roIgnoreCase]);
  y('(.*)\d+\1', 'abc12bc', '$1', 'bc', '');
  y('(?m:(foo\s*$))', 'foo\n bar', '$1', 'foo', '');
  y('(.*)c', 'abcd', '$1', 'ab', '');
  y('(.*)(?=c)', 'abcd', '$1', 'ab', '');
  y('(.*)(?=c)c', 'abcd', '$1', 'ab', '');
  y('(.*)(?=b|c)', 'abcd', '$1', 'ab', '');
  y('(.*)(?=b|c)c', 'abcd', '$1', 'ab', '');
  y('(.*)(?=c|b)', 'abcd', '$1', 'ab', '');
  y('(.*)(?=c|b)c', 'abcd', '$1', 'ab', '');
  y('(.*)(?=[bc])', 'abcd', '$1', 'ab', '');
  y('(.*)(?=[bc])c', 'abcd', '$1', 'ab', '');
  y('(.*)(?<=b)', 'abcd', '$1', 'ab', '');
  y('(.*)(?<=b)c', 'abcd', '$1', 'ab', '');
  y('(.*)(?<=b|c)', 'abcd', '$1', 'abc', '');
  y('(.*)(?<=b|c)c', 'abcd', '$1', 'ab', '');
  y('(.*)(?<=c|b)', 'abcd', '$1', 'abc', '');
  y('(.*)(?<=c|b)c', 'abcd', '$1', 'ab', '');
  y('(.*)(?<=[bc])', 'abcd', '$1', 'abc', '');
  y('(.*)(?<=[bc])c', 'abcd', '$1', 'ab', '');
  y('(.*?)c', 'abcd', '$1', 'ab', '');
  y('(.*?)(?=c)', 'abcd', '$1', 'ab', '');
  y('(.*?)(?=c)c', 'abcd', '$1', 'ab', '');
  y('(.*?)(?=b|c)', 'abcd', '$1', 'a', '');
  y('(.*?)(?=b|c)c', 'abcd', '$1', 'ab', '');
  y('(.*?)(?=c|b)', 'abcd', '$1', 'a', '');
  y('(.*?)(?=c|b)c', 'abcd', '$1', 'ab', '');
  y('(.*?)(?=[bc])', 'abcd', '$1', 'a', '');
  y('(.*?)(?=[bc])c', 'abcd', '$1', 'ab', '');
  y('(.*?)(?<=b)', 'abcd', '$1', 'ab', '');
  y('(.*?)(?<=b)c', 'abcd', '$1', 'ab', '');
  y('(.*?)(?<=b|c)', 'abcd', '$1', 'ab', '');
  y('(.*?)(?<=b|c)c', 'abcd', '$1', 'ab', '');
  y('(.*?)(?<=c|b)', 'abcd', '$1', 'ab', '');
  y('(.*?)(?<=c|b)c', 'abcd', '$1', 'ab', '');
  y('(.*?)(?<=[bc])', 'abcd', '$1', 'ab', '');
  y('(.*?)(?<=[bc])c', 'abcd', '$1', 'ab', '');
  y('2(]*)?$\1', '2', '$&', '2', '');

  y('a(b)??', 'abc', '<$1>', '<>', '# undef [perl #16773]');
  y('(\d{1,3}\.){3,}', '128.134.142.8', '<$1>', '<142.>', '# [perl #18019]');
  y('^.{3,4}(.+)\1\z', 'foobarbar', '$1', 'bar', '# 16 tests for [perl #23171]');
  y('^(?:f|o|b){3,4}(.+)\1\z', 'foobarbar', '$1', 'bar', '');
  y('^.{3,4}((?:b|a|r)+)\1\z', 'foobarbar', '$1', 'bar', '');
  y('^(?:f|o|b){3,4}((?:b|a|r)+)\1\z', 'foobarbar', '$1', 'bar', '');
  y('^.{3,4}(.+?)\1\z', 'foobarbar', '$1', 'bar', '');
  y('^(?:f|o|b){3,4}(.+?)\1\z', 'foobarbar', '$1', 'bar', '');
  y('^.{3,4}((?:b|a|r)+?)\1\z', 'foobarbar', '$1', 'bar', '');
  y('^(?:f|o|b){3,4}((?:b|a|r)+?)\1\z', 'foobarbar', '$1', 'bar', '');
  y('^.{2,3}?(.+)\1\z', 'foobarbar', '$1', 'bar', '');
  y('^(?:f|o|b){2,3}?(.+)\1\z', 'foobarbar', '$1', 'bar', '');
  y('^.{2,3}?((?:b|a|r)+)\1\z', 'foobarbar', '$1', 'bar', '');
  y('^(?:f|o|b){2,3}?((?:b|a|r)+)\1\z', 'foobarbar', '$1', 'bar', '');
  y('^.{2,3}?(.+?)\1\z', 'foobarbar', '$1', 'bar', '');
  y('^(?:f|o|b){2,3}?(.+?)\1\z', 'foobarbar', '$1', 'bar', '');
  y('^.{2,3}?((?:b|a|r)+?)\1\z', 'foobarbar', '$1', 'bar', '');
  y('^(?:f|o|b){2,3}?((?:b|a|r)+?)\1\z', 'foobarbar', '$1', 'bar', '');
  n('.*a(?!(b|cd)*e).*f', '......abef', '# [perl #23030]');
  c('x(?#', '-', '');
  c(':x(?#:', '-', '');
  y('(WORDS|WORD)S', 'WORDS', '$1', 'WORD', '');
  y('(X.|WORDS|X.|WORD)S', 'WORDS', '$1', 'WORD', '');
  y('(WORDS|WORLD|WORD)S', 'WORDS', '$1', 'WORD', '');
  y('(X.|WORDS|WORD|Y.)S', 'WORDS', '$1', 'WORD', '');
  y('(foo|fool|x.|money|parted)$', 'fool', '$1', 'fool', '');
  y('(x.|foo|fool|x.|money|parted|y.)$', 'fool', '$1', 'fool', '');
  y('(foo|fool|money|parted)$', 'fool', '$1', 'fool', '');
  n('(foo|fool|x.|money|parted)$', 'fools', '');
  n('(x.|foo|fool|x.|money|parted|y.)$', 'fools', '');
  n('(foo|fool|money|parted)$', 'fools', '');
  y('(a|aa|aaa||aaaa|aaaaa|aaaaaa)(b|c)', 'aaaaaaaaaaaaaaab', '$1$2', 'aaaaaab', '');


  y('^(a*?)(?!(aa|aaaa)*$)', 'aaaaaaaaaaaaaaaaaaaa', '$1', 'a', '# [perl #34195]');
  y('^(a*?)(?!(aa|aaaa)*$)(?=a\z)', 'aaaaaaaa', '$1', 'aaaaaaa', '');
  y('^(.)\s+.$(?(1))', 'A B', '$1', 'A', '# [perl #37688]');
  y('(?:r?)*?r|(.{2,4})', 'abcde', '$1', 'abcd', '');
  y('(?!)+?|(.{2,4})', 'abcde', '$1', 'abcd', '');
  y('^(a*?)(?!(a{6}|a{5})*$)', 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa', '$+[1]', '12', '# super-linear cache bug may return 18');
  y('^((?>(?:aa)?b)?)', 'aab', '$1', 'aab', '');
  y('^((?:aa)*)(?:X+((?:\d+|-)(?:X+(.+))?))?$', 'aaaaX5', '$1', 'aaaa', '');
  y('X(A|B||C|D)Y', 'XXXYYY', '$&', 'XY', '# Trie w/ NOTHING');
  y('(?i:X([A]|[B]|y[Y]y|[D]|)Y)', 'XXXYYYB', '$&', 'XY', '# Trie w/ NOTHING');
  y('^([a]{1})*$', 'aa', '$1', 'a', '');
  y('a(?!b(?!c))(..)', 'abababc', '$1', 'bc', '# test nested negatives');
  y('a(?!b(?=a))(..)', 'abababc', '$1', 'bc', '# test nested lookaheads');
  y('a(?!b(?!c(?!d(?!e))))...(.)', 'abxabcdxabcde', '$1', 'e', '');
  y('X(?!b+(?!(c+)*(?!(c+)*d))).*X', 'aXbbbbbbbcccccccccccccaaaX', '-', '-', '');
  y('^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):', 'ZEQQQQQQQQQQQQQQQQQQP:', '$1', 'ZEQQQQQQQQQQQQQQQQQQP', '');
  y('^(XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):', 'ZEQQQX:', '$1', 'ZEQQQX', '');
  y('^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):', 'ZEQQQQQQQQQQQQQQQQQQP:', '$1', 'ZEQQQQQQQQQQQQQQQQQQP', '');
  y('^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P):', 'ZEQQQX:', '$1', 'ZEQQQX', '');
  y('^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):', 'ZEQQQQQQQQQQQQQQQQQQP:', '$1', 'ZEQQQQQQQQQQQQQQQQQQP', '');
  y('^([TUV]+|XXXXXXXXXX|YYYYYYYYYY|Z.Q*X|Z[TE]Q*P|[MKJ]):', 'ZEQQQX:', '$1', 'ZEQQQX', '');
  y('^(XXX|YYY|Z.Q*X|Z[TE]Q*P):', 'ZEQQQQQQQQQQQQQQQQQQP:', '$1', 'ZEQQQQQQQQQQQQQQQQQQP', '');
  y('^(XXX|YYY|Z.Q*X|Z[TE]Q*P):', 'ZEQQQX:', '$1', 'ZEQQQX', '');
  y('^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):', 'ZEQQQQQQQQQQQQQQQQQQP:', '$1', 'ZEQQQQQQQQQQQQQQQQQQP', '');
  y('^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P):', 'ZEQQQX:', '$1', 'ZEQQQX', '');
  y('^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):', 'ZEQQQQQQQQQQQQQQQQQQP:', '$1', 'ZEQQQQQQQQQQQQQQQQQQP', '');
  y('^([TUV]+|XXX|YYY|Z.Q*X|Z[TE]Q*P|[MKJ]):', 'ZEQQQX:', '$1', 'ZEQQQX', '');
  y('X(?:ABCF[cC]x*|ABCD|ABCF):(?:DIT|DID|DIM)', 'XABCFCxxxxxxxxxx:DIM', '$&', 'XABCFCxxxxxxxxxx:DIM', '');
  y('(((ABCD|ABCE|ABCF)))(A|B|C[xy]*):', 'ABCFCxxxxxxxxxx:DIM', '$&', 'ABCFCxxxxxxxxxx:', '');
  y('(?=foo)', 'foo', 'pos', '0', '');
  y('(?=foo)', 'XfooY', 'pos', '1', '');
  y('.*(?=foo)', 'XfooY', 'pos', '1', '');
  y('(?<=foo)', 'foo', 'pos', '3', '');
  y('(?<=foo)', 'XfooY', 'pos', '4', '');
  y('.*(?<=foo)', 'foo', 'pos', '3', '');
  y('.*(?<=foo)', 'XfooY', 'pos', '4', '');
  y('(?<=foo)Y', 'XfooY', 'pos', '5', '');
  y('o(?<=foo)Y', '..XfooY..', 'pos', '7', '');
  y('X(?=foo)f', '..XfooY..', 'pos', '4', '');
  y('X(?=foo)', '..XfooY..', 'pos', '3', '');
  y('X(?<=foo.)[YZ]', '..XfooXY..', 'pos', '8', '');
  y('(?=XY*foo)', 'Xfoo', 'pos', '0', '');
  y('^(?=XY*foo)', 'Xfoo', 'pos', '0', '');












  y('^(<(?:[^<>]+|(?3)|(?1))*>)()(!>!>!>)$', '<<!>!>!>><>>!>!>!>', '$1', '<<!>!>!>><>>', '');
  y('^(<(?:[^<>]+|(?1))*>)$', '<<><<<><>>>>', '$1', '<<><<<><>>>>', '');
  y('((?2)*)([fF]o+)', 'fooFoFoo', '$1-$2', 'fooFo-Foo', '');
  y('(<(?:[^<>]+|(?R))*>)', '<<><<<><>>>>', '$1', '<<><<<><>>>>', '');
  y('(?<n>foo|bar|baz)', 'snofooewa', '$1', 'foo', '');
  y('(?<n>foo|bar|baz)', 'snofooewa', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?<n>foo|bar|baz)(?<m>[ew]+)', 'snofooewa', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?<n>foo|bar|baz)(?<m>[ew]+)', 'snofooewa', '$+{m}', 'ew', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?<n>foo)|(?<n>bar)|(?<n>baz)', 'snofooewa', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');

  y('(?P<n>foo|bar|baz)', 'snofooewa', '$1', 'foo', '');
  y('(?P<n>foo|bar|baz)', 'snofooewa', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?P<n>foo|bar|baz)(?P<m>[ew]+)', 'snofooewa', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?P<n>foo|bar|baz)(?P<m>[ew]+)', 'snofooewa', '$+{m}', 'ew', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?P<n>foo)|(?P<n>bar)|(?P<n>baz)', 'snofooewa', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');

  c('(?P<=n>foo|bar|baz)', '-', '');
  c('(?P<!n>foo|bar|baz)', '-', '');
  c('(?PX<n>foo|bar|baz)', '-', '');
  y('(?''n''foo|bar|baz)', 'snofooewa', '$1', 'foo', '');
  y('(?''n''foo|bar|baz)', 'snofooewa', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?''n''foo|bar|baz)(?''m''[ew]+)', 'snofooewa', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?''n''foo|bar|baz)(?''m''[ew]+)', 'snofooewa', '$+{m}', 'ew', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?''n''foo)|(?''n''bar)|(?<n>baz)', 'snobazewa', '$+{n}', 'baz', 'miniperl cannot load Tie::Hash::NamedCapture');

  y('(?''n''foo)\k<n>', '..foofoo..', '$1', 'foo', '');
  y('(?''n''foo)\k<n>', '..foofoo..', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?<n>foo)\k''n''', '..foofoo..', '$1', 'foo', '');
  y('(?<n>foo)\k''n''', '..foofoo..', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?:(?<n>foo)|(?<n>bar))\k<n>', '..barbar..', '$+{n}', 'bar', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('^(?''main''<(?:[^<>]+|(?&crap)|(?&main))*>)(?''empty'')(?''crap''!>!>!>)$', '<<!>!>!>><>>!>!>!>', '$+{main}', '<<!>!>!>><>>', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('^(?''main''<(?:[^<>]+|(?&main))*>)$', '<<><<<><>>>>', '$1', '<<><<<><>>>>', '');
  y('(?''first''(?&second)*)(?''second''[fF]o+)', 'fooFoFoo', '$+{first}-$+{second}', 'fooFo-Foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?<A>foo)?(?(<A>)bar|nada)', 'foobar', '$+{A}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?<A>foo)?(?(<A>)bar|nada)', 'foo-barnada', '$&', 'nada', '');
  y('(?<A>foo)?(?(1)bar|nada)', 'foo-barnada', '$&', 'nada', '');
  y('(?<A>foo(?(R)bar))?(?1)', 'foofoobar', '$1', 'foo', '');
  y('(?<A>foo(?(R)bar))?(?1)', 'foofoobar', '$&', 'foofoobar', '');
  y('(x)(?<A>foo(?(R&A)bar))?(?&A)', 'xfoofoobar', '$2', 'foo', '');
  y('(x)(?<A>foo(?(R&A)bar))?(?&A)', 'xfoofoobar', '$&', 'xfoofoobar', '');
  y('(x)(?<A>foo(?(R2)bar))?(?&A)', 'xfoofoobar', '$2', 'foo', '');
  y('(x)(?<A>foo(?(R2)bar))?(?&A)', 'xfoofoobar', '$&', 'xfoofoobar', '');
  y('(?1)(?(DEFINE)(blah))', 'blah', '$&', 'blah', '');
  y('^(?<PAL>(?<CHAR>.)((?&PAL)|.?)\k<CHAR>)$', 'madamimadam', '$&', 'madamimadam', '');
  n('^(?<PAL>(?<CHAR>.)((?&PAL)|.?)\k<CHAR>)$', 'madamiamadam', '');
  y('(a)?((?1))(fox)', 'aafox', '$1-$2-$3', 'a-a-fox', '');
  y('(a)*((?1))(fox)', 'aafox', '$1-$2-$3', 'a-a-fox', '');
  y('(a)+((?1))(fox)', 'aafox', '$1-$2-$3', 'a-a-fox', '');
  y('(a){1,100}((?1))(fox)', 'aafox', '$1-$2-$3', 'a-a-fox', '');
  y('(a){0,100}((?1))(fox)', 'aafox', '$1-$2-$3', 'a-a-fox', '');
  y('(ab)?((?1))(fox)', 'ababfox', '$1-$2-$3', 'ab-ab-fox', '');
  y('(ab)*((?1))(fox)', 'ababfox', '$1-$2-$3', 'ab-ab-fox', '');
  y('(ab)+((?1))(fox)', 'ababfox', '$1-$2-$3', 'ab-ab-fox', '');
  y('(ab){1,100}((?1))(fox)', 'ababfox', '$1-$2-$3', 'ab-ab-fox', '');
  y('(ab){0,100}((?1))(fox)', 'ababfox', '$1-$2-$3', 'ab-ab-fox', '');
// possessive captures
  n('a++a', 'aaaaa', '');
  n('a*+a', 'aaaaa', '');
  n('a{1,5}+a', 'aaaaa', '');
  n('a?+a', 'ab', '');
  y('a++b', 'aaaaab', '$&', 'aaaaab', '');
  y('a*+b', 'aaaaab', '$&', 'aaaaab', '');
  y('a{1,5}+b', 'aaaaab', '$&', 'aaaaab', '');
  y('a?+b', 'ab', '$&', 'ab', '');
  n('fooa++a', 'fooaaaaa', '');
  n('fooa*+a', 'fooaaaaa', '');
  n('fooa{1,5}+a', 'fooaaaaa', '');
  n('fooa?+a', 'fooab', '');
  y('fooa++b', 'fooaaaaab', '$&', 'fooaaaaab', '');
  y('fooa*+b', 'fooaaaaab', '$&', 'fooaaaaab', '');
  y('fooa{1,5}+b', 'fooaaaaab', '$&', 'fooaaaaab', '');
  y('fooa?+b', 'fooab', '$&', 'fooab', '');
  n('(?:aA)++(?:aA)', 'aAaAaAaAaA', '');
  n('(aA)++(aA)', 'aAaAaAaAaA', '');
  n('(aA|bB)++(aA|bB)', 'aAaAbBaAbB', '');
  n('(?:aA|bB)++(?:aA|bB)', 'aAbBbBbBaA', '');
  n('(?:aA)*+(?:aA)', 'aAaAaAaAaA', '');
  n('(aA)*+(aA)', 'aAaAaAaAaA', '');
  n('(aA|bB)*+(aA|bB)', 'aAaAbBaAaA', '');
  n('(?:aA|bB)*+(?:aA|bB)', 'aAaAaAbBaA', '');
  n('(?:aA){1,5}+(?:aA)', 'aAaAaAaAaA', '');
  n('(aA){1,5}+(aA)', 'aAaAaAaAaA', '');
  n('(aA|bB){1,5}+(aA|bB)', 'aAaAbBaAaA', '');
  n('(?:aA|bB){1,5}+(?:aA|bB)', 'bBbBbBbBbB', '');
  n('(?:aA)?+(?:aA)', 'aAb', '');
  n('(aA)?+(aA)', 'aAb', '');
  n('(aA|bB)?+(aA|bB)', 'bBb', '');
  n('(?:aA|bB)?+(?:aA|bB)', 'aAb', '');
  y('(?:aA)++b', 'aAaAaAaAaAb', '$&', 'aAaAaAaAaAb', '');
  y('(aA)++b', 'aAaAaAaAaAb', '$&', 'aAaAaAaAaAb', '');
  y('(aA|bB)++b', 'aAbBaAaAbBb', '$&', 'aAbBaAaAbBb', '');
  y('(?:aA|bB)++b', 'aAbBbBaAaAb', '$&', 'aAbBbBaAaAb', '');
  y('(?:aA)*+b', 'aAaAaAaAaAb', '$&', 'aAaAaAaAaAb', '');
  y('(aA)*+b', 'aAaAaAaAaAb', '$&', 'aAaAaAaAaAb', '');
  y('(aA|bB)*+b', 'bBbBbBbBbBb', '$&', 'bBbBbBbBbBb', '');
  y('(?:aA|bB)*+b', 'bBaAbBbBaAb', '$&', 'bBaAbBbBaAb', '');
  y('(?:aA){1,5}+b', 'aAaAaAaAaAb', '$&', 'aAaAaAaAaAb', '');
  y('(aA){1,5}+b', 'aAaAaAaAaAb', '$&', 'aAaAaAaAaAb', '');
  y('(aA|bB){1,5}+b', 'bBaAbBaAbBb', '$&', 'bBaAbBaAbBb', '');
  y('(?:aA|bB){1,5}+b', 'aAbBaAbBbBb', '$&', 'aAbBaAbBbBb', '');
  y('(?:aA)?+b', 'aAb', '$&', 'aAb', '');
  y('(aA)?+b', 'aAb', '$&', 'aAb', '');
  y('(aA|bB)?+b', 'bBb', '$&', 'bBb', '');
  y('(?:aA|bB)?+b', 'bBb', '$&', 'bBb', '');
  n('foo(?:aA)++(?:aA)', 'fooaAaAaAaAaA', '');
  n('foo(aA)++(aA)', 'fooaAaAaAaAaA', '');
  n('foo(aA|bB)++(aA|bB)', 'foobBbBbBaAaA', '');
  n('foo(?:aA|bB)++(?:aA|bB)', 'fooaAaAaAaAaA', '');
  n('foo(?:aA)*+(?:aA)', 'fooaAaAaAaAaA', '');
  n('foo(aA)*+(aA)', 'fooaAaAaAaAaA', '');
  n('foo(aA|bB)*+(aA|bB)', 'foobBaAbBaAaA', '');
  n('foo(?:aA|bB)*+(?:aA|bB)', 'fooaAaAbBbBaA', '');
  n('foo(?:aA){1,5}+(?:aA)', 'fooaAaAaAaAaA', '');
  n('foo(aA){1,5}+(aA)', 'fooaAaAaAaAaA', '');
  n('foo(aA|bB){1,5}+(aA|bB)', 'fooaAbBbBaAaA', '');
  n('foo(?:aA|bB){1,5}+(?:aA|bB)', 'fooaAbBbBaAbB', '');
  n('foo(?:aA)?+(?:aA)', 'fooaAb', '');
  n('foo(aA)?+(aA)', 'fooaAb', '');
  n('foo(aA|bB)?+(aA|bB)', 'foobBb', '');
  n('foo(?:aA|bB)?+(?:aA|bB)', 'fooaAb', '');
  y('foo(?:aA)++b', 'fooaAaAaAaAaAb', '$&', 'fooaAaAaAaAaAb', '');
  y('foo(aA)++b', 'fooaAaAaAaAaAb', '$&', 'fooaAaAaAaAaAb', '');
  y('foo(aA|bB)++b', 'foobBaAbBaAbBb', '$&', 'foobBaAbBaAbBb', '');
  y('foo(?:aA|bB)++b', 'fooaAaAbBaAaAb', '$&', 'fooaAaAbBaAaAb', '');
  y('foo(?:aA)*+b', 'fooaAaAaAaAaAb', '$&', 'fooaAaAaAaAaAb', '');
  y('foo(aA)*+b', 'fooaAaAaAaAaAb', '$&', 'fooaAaAaAaAaAb', '');
  y('foo(aA|bB)*+b', 'foobBbBaAaAaAb', '$&', 'foobBbBaAaAaAb', '');
  y('foo(?:aA|bB)*+b', 'foobBaAaAbBaAb', '$&', 'foobBaAaAbBaAb', '');
  y('foo(?:aA){1,5}+b', 'fooaAaAaAaAaAb', '$&', 'fooaAaAaAaAaAb', '');
  y('foo(aA){1,5}+b', 'fooaAaAaAaAaAb', '$&', 'fooaAaAaAaAaAb', '');
  y('foo(aA|bB){1,5}+b', 'foobBaAaAaAaAb', '$&', 'foobBaAaAaAaAb', '');
  y('foo(?:aA|bB){1,5}+b', 'fooaAbBaAbBbBb', '$&', 'fooaAbBaAbBbBb', '');
  y('foo(?:aA)?+b', 'fooaAb', '$&', 'fooaAb', '');
  y('foo(aA)?+b', 'fooaAb', '$&', 'fooaAb', '');
  y('foo(aA|bB)?+b', 'foobBb', '$&', 'foobBb', '');
  y('foo(?:aA|bB)?+b', 'foobBb', '$&', 'foobBb', '');
  y('([^()]++|\([^()]*\))+', '((abc(ade)ufh()()x', '$&', 'abc(ade)ufh()()x', '');
  y('round\(([^()]++)\)', '_I(round(xs * sz),1)', '$1', 'xs * sz', '');
  y('(foo[1x]|bar[2x]|baz[3x])+y', 'foo1bar2baz3y', '$1', 'baz3', '');
  y('(foo[1x]|bar[2x]|baz[3x])+y', 'foo1bar2baz3y', '$&', 'foo1bar2baz3y', '');
  y('(foo[1x]|bar[2x]|baz[3x])*y', 'foo1bar2baz3y', '$1', 'baz3', '');
  y('(foo[1x]|bar[2x]|baz[3x])*y', 'foo1bar2baz3y', '$&', 'foo1bar2baz3y', '');
  y('([yX].|WORDS|[yX].|WORD)S', 'WORDS', '$1', 'WORD', '');
  y('(WORDS|WORLD|WORD)S', 'WORDS', '$1', 'WORD', '');
  y('([yX].|WORDS|WORD|[xY].)S', 'WORDS', '$1', 'WORD', '');
  y('(foo|fool|[zx].|money|parted)$', 'fool', '$1', 'fool', '');
  y('([zx].|foo|fool|[zq].|money|parted|[yx].)$', 'fool', '$1', 'fool', '');
  n('(foo|fool|[zx].|money|parted)$', 'fools', '');
  n('([zx].|foo|fool|[qx].|money|parted|[py].)$', 'fools', '');
  y('([yX].|WORDS|[yX].|WORD)+S', 'WORDS', '$1', 'WORD', '');
  y('(WORDS|WORLD|WORD)+S', 'WORDS', '$1', 'WORD', '');
  y('([yX].|WORDS|WORD|[xY].)+S', 'WORDS', '$1', 'WORD', '');
  y('(foo|fool|[zx].|money|parted)+$', 'fool', '$1', 'fool', '');
  y('([zx].|foo|fool|[zq].|money|parted|[yx].)+$', 'fool', '$1', 'fool', '');
  n('(foo|fool|[zx].|money|parted)+$', 'fools', '');
  n('([zx].|foo|fool|[qx].|money|parted|[py].)+$', 'fools', '');
  y('(x|y|z[QW])+(longish|loquatious|excessive|overblown[QW])+', 'xyzQzWlongishoverblownW', '$1-$2', 'zW-overblownW', '');
  y('(x|y|z[QW])*(longish|loquatious|excessive|overblown[QW])*', 'xyzQzWlongishoverblownW', '$1-$2', 'zW-overblownW', '');
  y('(x|y|z[QW]){1,5}(longish|loquatious|excessive|overblown[QW]){1,5}', 'xyzQzWlongishoverblownW', '$1-$2', 'zW-overblownW', '');
  y('(x|y|z[QW])++(longish|loquatious|excessive|overblown[QW])++', 'xyzQzWlongishoverblownW', '$1-$2', 'zW-overblownW', '');
  y('(x|y|z[QW])*+(longish|loquatious|excessive|overblown[QW])*+', 'xyzQzWlongishoverblownW', '$1-$2', 'zW-overblownW', '');
  y('(x|y|z[QW]){1,5}+(longish|loquatious|excessive|overblown[QW]){1,5}+', 'xyzQzWlongishoverblownW', '$1-$2', 'zW-overblownW', '');
  n('a*(?!)', 'aaaab', '');
  n('a*(*FAIL)', 'aaaab', '');
  n('a*(*F)', 'aaaab', '');
  y('(A(A|B(*ACCEPT)|C)D)(E)', 'AB', '$1', 'AB', '');
  y('(A(A|B(*ACCEPT)|C)D)(E)', 'ACDE', '$1$2$3', 'ACDCE', '');
  y('(a)(?:(?-1)|(?+1))(b)', 'aab', '$&-$1-$2', 'aab-a-b', '');
  y('(a)(?:(?-1)|(?+1))(b)', 'abb', '$1-$2', 'a-b', '');
  n('(a)(?:(?-1)|(?+1))(b)', 'acb', '');
  y('(foo)(\g-2)', 'foofoo', '$1-$2', 'foo-foo', '');
  y('(foo)(\g-2)(foo)(\g-2)', 'foofoofoofoo', '$1-$2-$3-$4', 'foo-foo-foo-foo', '');
  y('(([abc]+) \g-1)(([abc]+) \g{-1})', 'abc abccba cba', '$2-$4', 'abc-cba', '');
  y('(a)(b)(c)\g1\g2\g3', 'abcabc', '$1$2$3', 'abc', '');
// \k<n> preceded by a literal
  y('(?''n''foo) \k<n>', '..foo foo..', '$1', 'foo', '');
  y('(?''n''foo) \k<n>', '..foo foo..', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?<n>foo) \k''n''', '..foo foo..', '$1', 'foo', '');
  y('(?<n>foo) \k''n''', '..foo foo..', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?''a1''foo) \k''a1''', '..foo foo..', '$+{a1}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?<a1>foo) \k<a1>', '..foo foo..', '$+{a1}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?''_''foo) \k''_''', '..foo foo..', '$+{_}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?<_>foo) \k<_>', '..foo foo..', '$+{_}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?''_0_''foo) \k''_0_''', '..foo foo..', '$+{_0_}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?<_0_>foo) \k<_0_>', '..foo foo..', '$+{_0_}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  c('(?''0''foo) bar', '-', '');
  c('(?<0>foo) bar', '-', '');
  c('(?''12''foo) bar', '-', '');
  c('(?<12>foo) bar', '-', '');
  c('(?''1a''foo) bar', '-', '');
  c('(?<1a>foo) bar', '-', '');
  c('(?''''foo) bar', '-', '');
  c('(?<>foo) bar', '-', '');
  c('foo \k''n''', '-', '');
  c('foo \k<n>', '-', '');
  c('foo \k''a1''', '-', '');
  c('foo \k<a1>', '-', '');
  c('foo \k''_''', '-', '');
  c('foo \k<_>', '-', '');
  c('foo \k''_0_''', '-', '');
  c('foo \k<_0_>', '-', '');
  c('foo \k''0''', '-', '');
  c('foo \k<0>', '-', '');
  c('foo \k''12''', '-', '');
  c('foo \k<12>', '-', '');
  c('foo \k''1a''', '-', '');
  c('foo \k<1a>', '-', '');
  c('foo \k''''', '-', '');
  c('foo \k<>', '-', '');
  y('(?<as>as) (\w+) \k<as> (\w+)', 'as easy as pie', '$1-$2-$3', 'as-easy-pie', '');
// \g{...} with a name as the argument
  y('(?''n''foo) \g{n}', '..foo foo..', '$1', 'foo', '');
  y('(?''n''foo) \g{n}', '..foo foo..', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?<n>foo) \g{n}', '..foo foo..', '$1', 'foo', '');
  y('(?<n>foo) \g{n}', '..foo foo..', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?<as>as) (\w+) \g{as} (\w+)', 'as easy as pie', '$1-$2-$3', 'as-easy-pie', '');
// Python style named capture buffer stuff
  y('(?P<n>foo)(?P=n)', '..foofoo..', '$1', 'foo', '');
  y('(?P<n>foo)(?P=n)', '..foofoo..', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?:(?P<n>foo)|(?P<n>bar))(?P=n)', '..barbar..', '$+{n}', 'bar', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('^(?P<PAL>(?P<CHAR>.)((?P>PAL)|.?)(?P=CHAR))$', 'madamimadam', '$&', 'madamimadam', '');
  n('^(?P<PAL>(?P<CHAR>.)((?P>PAL)|.?)(?P=CHAR))$', 'madamiamadam', '');
  y('(?P<n>foo) (?P=n)', '..foo foo..', '$1', 'foo', '');
  y('(?P<n>foo) (?P=n)', '..foo foo..', '$+{n}', 'foo', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?P<as>as) (\w+) (?P=as) (\w+)', 'as easy as pie', '$1-$2-$3', 'as-easy-pie', '');
//check that non identifiers as names are treated as the appropriate lookaround
  y('(?<=bar>)foo', 'bar>foo', '$&', 'foo', '');
  n('(?<!bar>)foo', 'bar>foo', '');
  y('(?<=bar>ABC)foo', 'bar>ABCfoo', '$&', 'foo', '');
  n('(?<!bar>ABC)foo', 'bar>ABCfoo', '');
  y('(?<bar>)foo', 'bar>ABCfoo', '$&', 'foo', '');
  y('(?<bar>ABC)foo', 'bar>ABCfoo', '$&', 'ABCfoo', '');
  y('(?<=abcd(?<=(aaaabcd)))', '..aaaabcd..', '$1', 'aaaabcd', '');
  y('(?=xy(?<=(aaxy)))', '..aaxy..', '$1', 'aaxy', '');
  y('X(\w+)(?=\s)|X(\w+)', 'Xab', '[$1-$2]', '[-ab]', '');
//check that branch reset works ok.
  y('(?|(a))', 'a', '$1-$+-$+', 'a-a-a', '');
  y('(?|a(.)b|d(.(o).)d|i(.)(.)j)(.)', 'd!o!da', '$1-$2-$3', '!o!-o-a', '');
  y('(?|a(.)b|d(.(o).)d|i(.)(.)j)(.)', 'aabc', '$1-$2-$3', 'a--c', '');
  y('(?|a(.)b|d(.(o).)d|i(.)(.)j)(.)', 'ixyjp', '$1-$2-$3', 'x-y-p', '');
  y('(?|(?|(a)|(b))|(?|(c)|(d)))', 'a', '$1', 'a', '');
  y('(?|(?|(a)|(b))|(?|(c)|(d)))', 'b', '$1', 'b', '');
  y('(?|(?|(a)|(b))|(?|(c)|(d)))', 'c', '$1', 'c', '');
  y('(?|(?|(a)|(b))|(?|(c)|(d)))', 'd', '$1', 'd', '');
  y('(.)(?|(.)(.)x|(.)d)(.)', 'abcde', '$1-$2-$3-$4-$5-', 'b-c--e--', '');
  y('(\N)(?|(\N)(\N)x|(\N)d)(\N)', 'abcde', '$1-$2-$3-$4-$5-', 'b-c--e--', '');
  y('(?|(?<foo>x))', 'x', '$+{foo}', 'x', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?|(?<foo>x)|(?<bar>y))', 'x', '$+{foo}', 'x', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?|(?<bar>y)|(?<foo>x))', 'x', '$+{foo}', 'x', 'miniperl cannot load Tie::Hash::NamedCapture');
  y('(?<bar>)(?|(?<foo>x))', 'x', '$+{foo}', 'x', 'miniperl cannot load Tie::Hash::NamedCapture');
// Used to crash, because the last branch was ignored when the parens
// were counted:
  y('(?|(b)|()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()' +
  '()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()' +
  '()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()' +
  '()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(a))', 'a', '$&', 'a', '');
//Bug #41492
  y('(?(DEFINE)(?<A>(?&B)+)(?<B>a))(?&A)', 'a', '$&', 'a', '');
  y('(?(DEFINE)(?<A>(?&B)+)(?<B>a))(?&A)', 'aa', '$&', 'aa', '');

  y('foo(\R)bar', 'foo\r\nbar', '$1', '\r\n', '');
  y('foo(\R)bar', 'foo\nbar', '$1', '\n', '');
  y('foo(\R)bar', 'foo\rbar', '$1', '\r', '');
  y('foo(\R+)bar', 'foo\r\n\x{85}\r\n\nbar', '$1', '\r\n\x{85}\r\n\n', '');
  y('(\V+)(\R)', 'foo\r\n\x{85}\r\n\nbar', '$1-$2', 'foo-\r\n', '');
  y('(\R+)(\V)', 'foo\r\n\x{85}\r\n\nbar', '$1-$2', '\r\n\x{85}\r\n\n-b', '');
  y('foo(\R)bar', 'foo\x{85}bar', '$1', '\x{85}', '');
  y('(\V)(\R)', 'foo\x{85}bar', '$1-$2', 'o-\x{85}', '');
  y('(\R)(\V)', 'foo\x{85}bar', '$1-$2', '\x{85}-b', '');
  y('foo(\R)bar', 'foo\r\nbar', '$1', '\r\n', '');
  y('(\V)(\R)', 'foo\r\nbar', '$1-$2', 'o-\r\n', '');
  y('(\R)(\V)', 'foo\r\nbar', '$1-$2', '\r\n-b', '');
  y('foo(\R)bar', 'foo\r\nbar', '$1', '\r\n', '');
  y('(\V)(\R)', 'foo\r\nbar', '$1-$2', 'o-\r\n', '');
  y('(\R)(\V)', 'foo\r\nbar', '$1-$2', '\r\n-b', '');
  y('foo(\R)bar', 'foo\rbar', '$1', '\r', '');
  y('(\V)(\R)', 'foo\rbar', '$1-$2', 'o-\r', '');
  y('(\R)(\V)', 'foo\rbar', '$1-$2', '\r-b', '');
  y('foo(\v+)bar', 'foo\r\n\x{85}\r\n\nbar', '$1', '\r\n\x{85}\r\n\n', '');
  y('(\V+)(\v)', 'foo\r\n\x{85}\r\n\nbar', '$1-$2', 'foo-\r', '');
  y('(\v+)(\V)', 'foo\r\n\x{85}\r\n\nbar', '$1-$2', '\r\n\x{85}\r\n\n-b', '');
  y('foo(\v)bar', 'foo\x{85}bar', '$1', '\x{85}', '');
  y('(\V)(\v)', 'foo\x{85}bar', '$1-$2', 'o-\x{85}', '');
  y('(\v)(\V)', 'foo\x{85}bar', '$1-$2', '\x{85}-b', '');
  y('foo(\v)bar', 'foo\rbar', '$1', '\r', '');
  y('(\V)(\v)', 'foo\rbar', '$1-$2', 'o-\r', '');
  y('(\v)(\V)', 'foo\rbar', '$1-$2', '\r-b', '');
  y('foo(\h+)bar', 'foo\t\x{A0}bar', '$1', '\t\x{A0}', '');
  y('(\H+)(\h)', 'foo\t\x{A0}bar', '$1-$2', 'foo-\t', '');
  y('(\h+)(\H)', 'foo\t\x{A0}bar', '$1-$2', '\t\x{A0}-b', '');
  y('foo(\h)bar', 'foo\x{A0}bar', '$1', '\x{A0}', '');
  y('(\H)(\h)', 'foo\x{A0}bar', '$1-$2', 'o-\x{A0}', '');
  y('(\h)(\H)', 'foo\x{A0}bar', '$1-$2', '\x{A0}-b', '');
  y('foo(\h)bar', 'foo\tbar', '$1', '\t', '');
  y('(\H)(\h)', 'foo\tbar', '$1-$2', 'o-\t', '');
  y('(\h)(\H)', 'foo\tbar', '$1-$2', '\t-b', '');
  y('.*\z', 'foo\n', '-$&-', '--', '');
  y('\N*\z', 'foo\n', '-$&-', '--', '');
  y('.*\Z', 'foo\n', '-$&-', '-foo-', '');
  y('\N*\Z', 'foo\n', '-$&-', '-foo-', '');
  y('^(?:(\d)x)?\d$', '1', '$1', '', '');
  y('.*?(?:(\w)|(\w))x', 'abx', '$1-$2', 'b-', '');
  y('0{50}', '000000000000000000000000000000000000000000000000000', '-', '-', '');
  y('^a?(?=b)b', 'ab', '$&', 'ab', '# Bug #56690');
  y('^a*(?=b)b', 'ab', '$&', 'ab', '# Bug #56690');
  y('>\d+$ \n', '>10\n', '$&', '>10'#$000A, '', [roIgnoreCase, roExtended]);
  y('>\d+$ \n', '>1\n', '$&', '>1'#$000A, '', [roIgnoreCase, roExtended]);
  y('\d+$ \n', '>10\n', '$&', '10'#$000A, '', [roIgnoreCase, roExtended]);
  y('>\d\d$ \n', '>10\n', '$&', '>10'#$000A, '', [roIgnoreCase, roExtended]);
  y('>\d+$ \n', '>10\n', '$&', '>10'#$000A, '', [roExtended]);
// Two regressions in 5.8.x (only) introduced by change 30638
// Simplification of the test failure in XML::LibXML::Simple:
  y('^\s*i.*?o\s*$', 'io\n io', '-', '-', '', [roSingleLine]);
// As reported in #59168 by Father Chrysostomos:
  y('(.*?)a(?!(a+)b\2c)', 'baaabaac', '$&-$1', 'baa-ba', '');
// [perl #60344] Regex lookbehind failure after an (if)then|else in perl 5.10
  n('\A(?(?=db2)db2|\D+)(?<!processed)\.csv\z', 'sql_processed.csv', '', [roSingleLine, roMultiLine, roExtended]);
// Verify that \N{U+...} forces Unicode semantics
  n('[\s][\S]', '\x{a0}\x{a0}', '# Unicode complements should not match same character');
// was generating malformed utf8
  y('[\x{100}\xff]', '\x{ff}', '$&', '\x{ff}', '', [roIgnoreCase]);

// Verify that \ escapes the { after \N, and causes \N to match non-newline
  y('abc\N\{U+BEEF}', 'abc.{UBEEF}', '$&', 'abc.{UBEEF}', '');
  c('[abc\N\{U+BEEF}]', '-', '');
// Verify that \N can be trailing and causes \N to match non-newline
  y('abc\N', 'abcd', '$&', 'abcd', '');
  n('abc\N', 'abc\n', '');
// Verify get errors.  For these, we need // or else puts it in single quotes,
// and bypasses the lexer.
// Below currently gives a misleading message
// And verify that in single quotes which bypasses the lexer, the regex compiler
// figures it out.
// Verify that under /x that still cant have space before left brace
// Verifies catches hex errors, and doesn't expose our . notation to the outside
// Verify works in single quotish context; regex compiler delivers slightly different msg
// \N{U+BEEF.BEAD} succeeds here, because can't completely hide it from the outside.
  c('\c`', '-', '');
  c('\c1', '-', '');
  y('\cA', '\001', '$&', '\1', '');
  y('\400', '\x{100}', '$&', '\x{100}', '');
  y('\600', '\x{180}', '$&', '\x{180}', '');
  y('\777', '\x{1FF}', '$&', '\x{1FF}', '');
  y('[a\400]', '\x{100}', '$&', '\x{100}', '');
  y('[b\600]', '\x{180}', '$&', '\x{180}', '');
  y('[c\777]', '\x{1FF}', '$&', '\x{1FF}', '');
  y('\o{120}', '\x{50}', '$&', '\x{50}', '');
  y('\o{400}', '\x{100}', '$&', '\x{100}', '');
  y('\o{1000}', '\x{200}', '$&', '\x{200}', '');
  y('[a\o{120}]', '\x{50}', '$&', '\x{50}', '');
  y('[a\o{400}]', '\x{100}', '$&', '\x{100}', '');
  y('[a\o{1000}]', '\x{200}', '$&', '\x{200}', '');
// The below were inserting a NULL
  y('\87', '87', '$&', '87', '');
  y('a\87', 'a87', '$&', 'a87', '');
  y('a\97', 'a97', '$&', 'a97', '');
// The below was inserting a NULL into the character class.
  n('[\8\9]', '\000', '');
  c('[\8\9]', '-', '');
  y('[\8\9]', '8', '$&', '8', '');
  y('[\8\9]', '9', '$&', '9', '');
// Verify that reads 1-3 octal digits, and that \_ works in char class
  y('[\0]', '\000', '$&', '\000', '');
  c('[\0]', '-', '');
  y('[\07]', '\007', '$&', '\007', '');
  c('[\07]', '-', '');
  n('[\07]', '7\000', '');
  c('[\07]', '-', '');
  y('[\006]', '\006', '$&', '\006', '');
  n('[\006]', '6\000', '');
  y('[\0005]', '\0005', '$&', '\000', '');
  y('[\0005]', '5\000', '$&', '5', '');
  y('[\_]', '_', '$&', '_', '');
// RT #79152
  y('(q1|.)*(q2|.)*(x(a|bc)*y){2,}', 'xayxay', '$&', 'xayxay', '');
  y('(q1|.)*(q2|.)*(x(a|bc)*y){2,3}', 'xayxay', '$&', 'xayxay', '');
  y('(q1|z)*(q2|z)*z{15}-.*?(x(a|bc)*y){2,3}Z', 'zzzzzzzzzzzzzzzz-xayxayxayxayZ', '$&', 'zzzzzzzzzzzzzzzz-xayxayxayxayZ', '');
  y('(?:(?:)foo|bar|zot|rt78356)', 'foo', '$&', 'foo', '');
  y('\xe0\pL', '\xc0a', '$&', '\xc0a', '', [roIgnoreCase]);
// RT #85528

// RT #85964
  y('^m?(\S)(.*)\1$', 'aba', '$1', 'a', '');
  n('^m?(\S)(.*)\1$', '\tb\t', '');
  y('^m?(\s)(.*)\1$', '\tb\t', '$1', '\t', '');
  n('^m?(\s)(.*)\1$', 'aba', '');
  y('^m?(\W)(.*)\1$', ':b:', '$1', ':', '');
  n('^m?(\W)(.*)\1$', 'aba', '');
  y('^m?(\w)(.*)\1$', 'aba', '$1', 'a', '');
  n('^m?(\w)(.*)\1$', ':b:', '');
  y('^m?(\D)(.*)\1$', 'aba', '$1', 'a', '');
  n('^m?(\D)(.*)\1$', '5b5', '');
  y('^m?(\d)(.*)\1$', '5b5', '$1', '5', '');
  n('^m?(\d)(.*)\1$', 'aba', '');
// 17F is 'Long s';  This makes sure the a's in /aa can be separate
  y('s', '\x{17F}', '$&', '\x{17F}', '', [roIgnoreCase, roASCIICharClass]);
  n('s', '\x{17F}', '', [roIgnoreCase, roASCIICharClass, roASCIIOnly]);
  y('s', 'S', '$&', 'S', '', [roIgnoreCase, roASCIICharClass, roASCIIOnly]);
  n('(?aia:s)', '\x{17F}', '');
  y('(?aia:s)', 'S', '$&', 'S', '');
// Normally 1E9E generates a multi-char fold, but not in inverted class;
// See [perl #89750].  This makes sure that the simple fold gets generated
// in that case, to DF.
  n('[^\x{1E9E}]', '\x{DF}', '', [roIgnoreCase]);
// RT #96354
  n('^.*\d\H', 'X1', '');
  n('^.*\d\V', 'X1', '');
// \p{L_} was being misinterpreted as \p{L}.  L_ matches cased letters, which
// the ideograph below isn't, whereas it does match L
  c('^\p{L_}', '\x{3400}', '');   //mod n -> c
  y('^\p{L}', '\x{3400}', '$&', '\x{3400}', '');
// RT #89774
  y('[s\xDF]a', 'ssa', '$&', 'sa', '', [roIgnoreCase]); //mod
  y('[s\xDF]a', 'sa', '$&', 'sa', '', [roIgnoreCase]);
// RT #99928
  n('^\R\x0A$', '\x0D\x0A', '');
  y('ff', '\x{FB00}\x{FB01}', '$&', '\x{FB00}', '', [roIgnoreCase]);
  y('ff', '\x{FB01}\x{FB00}', '$&', '\x{FB00}', '', [roIgnoreCase]);
  y('fi', '\x{FB01}\x{FB00}', '$&', '\x{FB01}', '', [roIgnoreCase]);
  y('fi', '\x{FB00}\x{FB01}', '$&', '\x{FB01}', '', [roIgnoreCase]);
//
// Make sure we don't see code blocks where there aren't, and vice-versa












// These test that doesn't cut-off matching too soon in the string for
// multi-char folds
  y('ffiffl', 'abcdef\x{FB03}\x{FB04}', '$&', '\x{FB03}\x{FB04}', '', [roIgnoreCase]);
  y('\xdf\xdf', 'abcdefssss', '$&', 'ssss', '', [roIgnoreCase]);
  y('st', '\x{DF}\x{FB05}', '$&', '\x{FB05}', '', [roIgnoreCase]);
  y('ssst', '\x{DF}\x{FB05}', '$&', '\x{DF}\x{FB05}', '', [roIgnoreCase]);
// [perl #101970]
  y('[[:lower:]]', '\x{100}', '$&', '\x{100}', '', [roIgnoreCase]);
  y('[[:upper:]]', '\x{101}', '$&', '\x{101}', '', [roIgnoreCase]);
// Was matching 'ss' only and failing the entire match, not seeing the
// alternative that would succeed
  y('s\xDF', '\xDFs', '$&', '\xDFs', '', [roIgnoreCase]);
// /i properties shouldn't match more than the property itself
// [[:lower:]]/i and [[:upper:]]/i should match what \p{Lower} and \p{Upper} do.
// which is \p{Cased}, not \p{Alpha},
// [perl #110648]
  n('[^\p{Alpha}]', '\x{100}', '');
// [perl #111400].  Tests the first Y/N boundary above 255 for each of these.
  y('[[:alnum:]]', '\x{2c1}', '-', '-', '');
  n('[[:alnum:]]', '\x{2c2}', '');
  y('[[:alpha:]]', '\x{2c1}', '-', '-', '');
  n('[[:alpha:]]', '\x{2c2}', '');
  y('[[:graph:]]', '\x{377}', '-', '-', '');
  n('[[:graph:]]', '\x{378}', '');
  n('[[:lower:]]', '\x{100}', '');
  y('[[:lower:]]', '\x{101}', '-', '-', '');
  n('[[:lower:]]', '\x{102}', '');
  y('[[:print:]]', '\x{377}', '-', '-', '');
  n('[[:print:]]', '\x{378}', '');
  n('[[:punct:]]', '\x{37D}', '');
  y('[[:punct:]]', '\x{37E}', '-', '-', '');
  n('[[:punct:]]', '\x{388}', '');
  y('[[:upper:]]', '\x{100}', '-', '-', '');
  n('[[:upper:]]', '\x{101}', '');
  y('[[:word:]]', '\x{2c1}', '-', '-', '');
  n('[[:word:]]', '\x{2c2}', '');
// [perl #113400]
  y('syntax OK\s+\z', 't/bin/good.pl syntax OK\n', '-', '-', '', [roIgnoreCase, roSingleLine]);
  y('^(.*?)\s*\|\s*(?:\/\s*|)''(.+)''$', 'text|''sec''', '<$1><$2>', '<text><sec>', '');
  y('^(foo|)bar$', 'bar', '<$&>', '<bar>', '');
  y('^(foo||baz)bar$', 'bar', '<$&>', '<bar>', '');
  y('^(foo||baz)bar$', 'bazbar', '<$1>', '<baz>', '');
  y('^(foo||baz)bar$', 'foobar', '<$1>', '<foo>', '');
  y('^(?:foo|)bar$', 'bar', '<$&>', '<bar>', '');
  y('^(?:foo||baz)bar$', 'bar', '<$&>', '<bar>', '');
  y('^(?:foo||baz)bar$', 'bazbar', '<$&>', '<bazbar>', '');
  y('^(?:foo||baz)bar$', 'foobar', '<$&>', '<foobar>', '');
  y('^(?i:foo|)bar$', 'bar', '<$&>', '<bar>', '');
  y('^(?i:foo||baz)bar$', 'bar', '<$&>', '<bar>', '');
  y('^(?i:foo||baz)bar$', 'bazbar', '<$&>', '<bazbar>', '');
  y('^(?i:foo||baz)bar$', 'foobar', '<$&>', '<foobar>', '');
// $+, $+ on backtrackracking
// BRANCH
  y('^(.)(?:(..)|B)[CX]', 'ABCDE', '$+-$+', 'A-A', '-');
// TRIE
  y('^(.)(?:BC(.)|B)[CX]', 'ABCDE', '$+-$+', 'A-A', '-');
// CURLYX
  y('^(.)(?:(.)+)*[BX]', 'ABCDE', '$+-$+', 'A-A', '-');
// CURLYM
  y('^(.)(BC)*', 'ABCDE', '$+-$+', 'BC-BC', '-');
  y('^(.)(BC)*[BX]', 'ABCDE', '$+-$+', 'A-A', '-');
// CURLYN
  y('^(.)(B)*.[DX]', 'ABCDE', '$+-$+', 'B-B', '-');
  y('^(.)(B)*.[CX]', 'ABCDE', '$+-$+', 'A-A', '-');
// using 'return' in code blocks








// pattern modifier flags should propagate into returned (??{}) pattern
// p,d,l not tested









// #113670 ensure any captures to the right are invalidated when CURLY
// and CURLYM backtrack
  y('^(?:(X)?(\d)|(X)?(\d\d))$', 'X12', '$1-$2-$3-$4', '--X-12', '');
  y('^(?:(XX)?(\d)|(XX)?(\d\d))$', 'XX12', '$1-$2-$3-$4', '--XX-12', '');
// rt 113770
  y('\A(?>\[(?:(?:)(?:R){1}|T|V?|A)\])\z', '[A]', '$&', '[A]', '');
// rt 114068
  y('( [^z] $ [^z]+)', 'aa\nbb\ncc\n', '$1', 'a\nbb\ncc\n', '', [roMultiLine, roExtended]);
// [perl #114220]
  y('[\h]', '\x{A0}', '$&', '\xA0', '');
  y('[\H]', '\x{BF}', '$&', '\xBF', '');
  n('[\H]', '\x{A0}', '');
  y('[\H]', '\x{A1}', '$&', '\xA1', '');
  y('[^\n]+', '\nb', '$&', 'b', '');
  y('[^\n]+', 'a\n', '$&', 'a', '');
// /a has no effect on properties
  y('(?a:\p{Any})', '\x{100}', '$&', '\x{100}', '');
  y('(?aa:\p{Any})', '\x{100}', '$&', '\x{100}', '');
// why not match?
//  y('\w', '\x{200C}', '$&', '\x{200C}', '');
//  n('\W', '\x{200C}', '');
//  y('\w', '\x{200D}', '$&', '\x{200D}', '');
//  n('\W', '\x{200D}', '');
// Cassless MultiChar
//  y('^(?d:\xdf|_)*_', '\x{17f}\x{17f}_', '$&', '\x{17f}\x{17f}_', '', [roIgnoreCase]);

// check that @-, @+ count chars, not bytes; especially if beginning of
// string is not copied
  y('(\x{100})', '\x{2000}\x{2000}\x{2000}\x{100}', '$-[0]:$-[1]:$+[0]:$+[1]', '3:3:4:4', '');
  y('^\R{2}$', '\r\n\r\n', '$&', '\r\n\r\n', '');
  n('^\D{11}', '\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}', '', [roASCIICharClass]);
  n('^\S{11}', '\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}', '', [roASCIICharClass]);
  n('^\W{11}', '\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}\x{10FFFF}', '', [roASCIICharClass]);
// [ perl #114272]
  y('\Vn', '\xFFn/', '$&', '\xFFn', '');
  y('(?l:a?\w)', 'b', '$&', 'b', '');
  y('m?^xy\?$?', 'xy?', '$&', 'xy?', '');
// vim: softtabstop=0 noexpandtab

  ADest.Add(FMessage.Text);
end;

procedure TSkRegExpPerlTest.n(APattern, AStrings, ASkipReason: REString; AOptions: TREOptions);
var
  R: TSkRegExp;
begin
  try
    R := TSkRegExp.Create;
    try
      R.Expression := APattern;
      R.Options := AOptions;
      R.InputString := TSkRegExp.DecodeEscape(AStrings);
      if R.ExecPos() then
      begin
        FMessage.Add(Format('''%s'', ''%s'' : Matched', [APattern, AStrings]));
      end;
    finally
      R.Free;
    end;
  except
    on E: ESkRegExp do
    begin
      FMessage.Add(Format('''%s'', ''%s'' : %s(%s)', [APattern, AStrings, E.Message, ASkipReason]));
    end;
  end;
end;

function TSkRegExpPerlTest.SubStitute(ARegExp: TSkRegExp; const ATemplate: REString): REString;

  function GetGroupNumber(const S: REString; var Index: Integer): Integer;
  begin
    Result := 0;

    while Index <= Length(S) do
    begin
      if (S[Index] >= '0') and (S[Index] <= '9') then
        Result := Result * 10 + (Integer(S[Index]) - Integer('0'))
      else
        Break;

      Inc(Index);
    end;
  end;

  function GetGroupName(const S: REString; var Index: Integer): REString;
  var
    L: Integer;
  begin
    Result := '';

    L := Length(S);

    while Index <= L do
    begin
      if (S[Index] = '}') then
        Exit
      else
        Result := Result + S[Index];

      Inc(Index);
    end;

    raise ESkRegExpCompile.Create('} がない');
  end;

var
  K: Integer;
  LGroupName: REString;
  I, J, L: Integer;
  Start, Last: Integer;
begin
  Result := '';
  if ATemplate = '' then
    Exit;

  if ATemplate = 'pos' then
  begin
    if ARegExp.Groups[0].Success then
    begin
      Last := (ARegExp.Groups[0].Index - 1) + ARegExp.Groups[0].Length;
      Result := IntToStr(Last);
    end;
    Exit;
  end;

  I := 1;
  L := System.Length(ATemplate);

  while I <= L do
  begin
    if ATemplate[I] = '$' then
    begin
      Inc(I);
      if (ATemplate[I] >= '0') and (ATemplate[I] <= '9') then
      begin
        K := GetGroupNumber(ATemplate, I);
//        K := (Integer(ATemplate[I]) - Integer('0'));
        if K <= ARegExp.Groups.Count - 1 then
          Result := Result + ARegExp.Groups[K].Strings;

        Continue;
      end
      else if ATemplate[I] = '^' then
      begin
        Inc(I);
        if ATemplate[I] = 'N' then
          raise ESkRegExpCompile.Create(Format('not suport %s', [Copy(ATemplate, 1, I)]));
      end
      else if ATemplate[I] = '{' then
      begin
        Inc(I);
        LGroupName := GetGroupName(ATemplate, I);
        Result := Result + ARegExp.Groups.Names[LGroupName].Strings;
      end
      else if ATemplate[I] = '&' then
        Result := Result + ARegExp.Groups[0].Strings
      else if ATemplate[I] = '$' then
        Result := Result + '$'
      else if ATemplate[I] = '`' then
      begin
        Result := Result + Copy(ARegExp.InputString, 1, ARegExp.Groups[0].Index - 1);
      end
      else if ATemplate[I] = '''' then
      begin
        Result := Result + Copy(ARegExp.InputString, ARegExp.Groups[0].
          Index + ARegExp.Groups[0].Length, Maxint);
      end
      else if ATemplate[I] = '_' then
      begin
        Result := Result + ARegExp.InputString;
      end
      else if ATemplate[I] = '+' then
      begin
        Inc(I);
        if (I <= L) and (ATemplate[I] = '[') then
        begin
          Inc(I);
          K := GetGroupNumber(ATemplate, I);

          if ATemplate[I] <> ']' then
            raise ESkRegExpCompile.Create('[が足りません');

          if ARegExp.Groups[K].Success then
          begin
            Last := (ARegExp.Groups[K].Index - 1) + ARegExp.Groups[K].Length;
            Result := Result + IntToStr(Last);
          end;
        end
        else if (I <= L) and (ATemplate[I] = '{') then
        begin
          Inc(I);
          LGroupName := GetGroupName(ATemplate, I);
          if ARegExp.Groups.IndexOfName(LGroupName) <> -1 then
            Result := Result + ARegExp.Groups.Names[LGroupName].Strings
        end
        else
        begin
          Dec(I);
          for J := ARegExp.GroupCount downto 1 do
          begin
            if ARegExp.Groups[J].Index > 0 then
            begin
              Result := Result + ARegExp.Groups[J].Strings;
              Break;
            end;
          end;
        end;
      end
      else if ATemplate[I] = '-' then
      begin
        Inc(I);
        if ATemplate[I] = '[' then
        begin
          Inc(I);
          K := GetGroupNumber(ATemplate, I);

          if ATemplate[I] <> ']' then
            raise ESkRegExpCompile.Create('[が足りません');

          if ARegExp.Groups[K].Success then
          begin
            Start  := (ARegExp.Groups[K].Index - 1);
            Result := Result + IntToStr(Start);
          end;
        end
        else
          raise ESkRegExpCompile.Create(Format('%s10進数が必要です', [Copy(ATemplate, 1, I)]));
      end
      else
        Result := Result + ATemplate[I];
    end
    else if ATemplate[I] = '@' then
    begin
      Inc(I);
      if ATemplate[I] = '-' then
      begin
        for J := 0 to ARegExp.GroupCount do
        begin
          if J = 0 then
            Result := Result + IntToStr(ARegExp.Groups[J].Index - 1)
          else
            Result := Result + ' ' + IntToStr(ARegExp.Groups[J].Index - 1)
        end;
      end
      else if ATemplate[I] = '+' then
      begin
        for J := 0 to ARegExp.GroupCount do
        begin
          if J = 0 then
            Result := Result + IntToStr((ARegExp.Groups[J].Index - 1) + ARegExp.Groups[J].Length)
          else
            Result := Result + ' ' + IntToStr((ARegExp.Groups[J].Index - 1) + ARegExp.Groups[J].Length)
        end;
      end
      else
      begin
        Result := '@';
        Dec(I);
      end;
    end
    else if ATemplate[I] = '\' then
    begin
      Inc(I);
      Result := Result + ATemplate[I];
    end
    else
      Result := Result + ATemplate[I];

    Inc(I);
  end;
end;

procedure TSkRegExpPerlTest.y(APattern, AStrings, AExpr, AExceptedExpr, ASkipReason: REString;
  AOptions: TREOptions);
var
  R: TSkRegExp;
  SubStr, EStr: REString;
begin
  try
    R := TSkRegExp.Create;
    try
      R.Expression := APattern;
      R.Options := AOptions;
      R.InputString := TSkRegExp.DecodeEscape(AStrings);
      if not R.ExecPos() then
      begin
        FMessage.Add(Format('''%s'', ''%s'' : not Match(%s)', [APattern, AStrings, ASkipReason]));
      end
      else
      begin
        SubStr := Substitute(R, AExpr);
        EStr := R.DecodeEscape(AExceptedExpr);
        if SubStr <> EStr then
          FMessage.Add(Format('''%s'', ''%s'', ''%s'', ''%s'' : not Match',
            [APattern, AStrings, AExpr, AExceptedExpr]));
      end;
    finally
      R.Free;
    end;
  except
    on E: ESkRegExp do
    begin
      FMessage.Add(Format('''%s'', ''%s'', ''%s'', ''%s'' : %s',
        [APattern, AStrings, AExpr, AExceptedExpr, E.Message]));
    end;
  end;
end;

end.
