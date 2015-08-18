(* ***************************************************************************
  TRegEx wrapper of SkRegExp.

  version 2.1

  for Delphi 2006 or later, SkRegExp version 4.x later.

  usage: see TRegEx in Delphi XE Help.
  **************************************************************************** *)
(*
  Copyright (c) 2010-2015 Shuuichi Komiya <shu AT komish DOT jp>

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1.Redistributions of source code must retain the above copyright notice,
  this list of conditions and the following disclaimer.
  2.Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

unit SkRegularExpressions;

interface

uses SysUtils, Classes, SkRegExpW;

type
  TRegExOption = (roIgnoreCase, roMultiLine, roExplicitCapture, roCompiled, roSingleLine,
    roIgnorePatternSpace);
  TRegExOptions = set of TRegExOption;

  TGroup = record
  private
    FIndex: Integer;
    FLength: Integer;
    FSuccess: Boolean;
    FValue: REString;
    FGroupName: REString;
    constructor Create(const AValue: REString; AIndex, ALength: Integer;
      const AGroupName: REString);
    function GetIndex: Integer;
    function GetLength: Integer;
    function GetValue: REString;
    function GetGroupName: REString;
  public
    property Index: Integer read GetIndex;
    property Length: Integer read GetLength;
    property Success: Boolean read FSuccess;
    property Value: REString read GetValue;
    property GroupName: REString read GetGroupName;
  end;

  TGroupArray = array of TGroup;

  TGroupCollectionEnumerator = class
  private
    FIndex: Integer;
    FList: TGroupArray;
  public
    constructor Create(AList: TGroupArray);
    function GetCurrent: TGroup;
    function MoveNext: Boolean;
    property Current: TGroup read GetCurrent;
  end;

  TGroupCollection = record
  private
    FList: TGroupArray;
    FNotifier: IInterface;
    function GetCount: Integer;
    function GetItem(Index: Variant): TGroup;
    function IndexOfMatchedGroupName(const AGroupName: REString): Integer;
  public
    constructor Create(ARegEx: TSkRegExp; const AValue: REString;
      AIndex, ALength: Integer; ANotifier: IInterface);
    function GetEnumerator: TGroupCollectionEnumerator;
    property Count: Integer read GetCount;
    property Item[Index: Variant]: TGroup read GetItem; default;
  end;

  TMatch = record
  private
    FNotifier: IInterface;
    FGroup: TGroup;
    FGroups: TGroupCollection;
    FRegExp: TSkRegExp;
    constructor Create(ARegEx: TSkRegExp; const AValue: REString;
      AIndex, ALength: Integer; ANotifier: IInterface);
    function GetIndex: Integer;
    function GetGroups: TGroupCollection;
    function GetLength: Integer;
    function GetSuccess: Boolean;
    function GetValue: REString;
  public
    function NextMatch: TMatch;
    function Result(const Pattern: REString): REString;
    property Groups: TGroupCollection read GetGroups;
    property Index: Integer read GetIndex;
    property Length: Integer read GetLength;
    property Success: Boolean read GetSuccess;
    property Value: REString read GetValue;
  end;

  TMatchArray = array of TMatch;

  TMatchCollectionEnumerator = class
  private
    FIndex: Integer;
    FList: TMatchArray;
  public
    constructor Create(AList: TMatchArray);
    function GetCurrent: TMatch;
    function MoveNext: Boolean;
    property Current: TMatch read GetCurrent;
  end;

  TMatchCollection = record
  private
    FList: TMatchArray;
    function GetCount: Integer;
    function GetItem(Index: Integer): TMatch;
    constructor Create(ARegEx: TSkRegExp; const Input: REString;
      AOptions: TRegExOptions; StartPos: Integer; ANotifier: IInterface);
  public
    function GetEnumerator: TMatchCollectionEnumerator;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TMatch read GetItem; default;
  end;

  TMatchEvaluator = function(const AMatch: TMatch): REString of object;
  TStringDynArray = array of REString;

  TRegEx = record
  private
    FNotifier: IInterface;
    FEvaluator: TMatchEvaluator;
    FRegExp: TSkRegExp;
    function ReplaceFunc(ARegExp: TSkRegExp): REString;
  public
    constructor Create(const Pattern: REString); overload;
    constructor Create(const Pattern: REString;
      Options: TRegExOptions); overload;
    class function Escape(const Str: string;
      UseWildCards: Boolean = False): string; static;
    function IsMatch(const Input: REString): Boolean; overload;
    function IsMatch(const Input: REString; StartPos: Integer)
      : Boolean; overload;
    class function IsMatch(const Input,
      Pattern: REString): Boolean; overload; static;
    class function IsMatch(const Input, Pattern: REString;
      Options: TRegExOptions): Boolean; overload; static;
    function Match(const Input: REString): TMatch; overload;
    function Match(const Input: REString; StartPos: Integer): TMatch; overload;
    function Match(const Input: REString; StartPos, Length: Integer)
      : TMatch; overload;
    class function Match(const Input, Pattern: REString): TMatch; overload; static;
    class function Match(const Input, Pattern: REString;
      Options: TRegExOptions): TMatch; overload; static;
    function Matches(const Input: REString): TMatchCollection; overload;
    function Matches(const Input: REString;
      StartPos: Integer): TMatchCollection; overload;
    class function Matches(
      const Input, Pattern: REString): TMatchCollection; overload; static;
    class function Matches(const Input, Pattern: REString;
      Options: TRegExOptions): TMatchCollection; overload; static;
    function Replace(const Input, Replacement: REString): REString; overload;
    function Replace(const Input, Replacement: REString;
      Count: Integer): REString; overload; inline;
    function Replace(const Input, Replacement: REString;
      Count, StartAt: Integer): REString; overload;
    function Replace(const Input: REString;
      Evaluator: TMatchEvaluator): REString; overload; inline;
    function Replace(const Input: REString;
      Evaluator: TMatchEvaluator; Count: Integer): REString; overload; inline;
    function Replace(const Input: REString;
      Evaluator: TMatchEvaluator; Count, StartAt: Integer): REString; overload;
    class function Replace(const Input, Pattern,
      Replacement: REString): REString; overload; static;
    class function Replace(const Input, Pattern: REString;
      Evaluator: TMatchEvaluator): REString; overload; static;
    class function Replace(const Input, Pattern, Replacement: REString;
      Options: TRegExOptions): REString; overload; static;
    class function Replace(const Input, Pattern: REString;
      Evaluator: TMatchEvaluator;
      Options: TRegExOptions): REString; overload; static;
    function Split(const Input: REString): TStringDynArray; overload; inline;
    function Split(const Input: REString;
      Count: Integer): TStringDynArray;overload; inline;
    function Split(const Input: REString;
      Count, StartPos: Integer): TStringDynArray; overload;
    class function Split(const Input,
      Pattern: REString): TStringDynArray; overload; static;
    class function Split(const Input, Pattern: REString;
      Options: TRegExOptions): TStringDynArray; overload; static;
  end;

implementation

uses Variants, IniFiles, SkRegExpConst;

type
  TScopeExitNotifier = class(TInterfacedObject)
  private
    FRegExp: TSkRegExp;
  public
    constructor Create(ARegExp: TSkRegExp);
    destructor Destroy; override;
  end;

constructor TScopeExitNotifier.Create(ARegExp: TSkRegExp);
begin
  FRegExp := ARegExp;
end;

destructor TScopeExitNotifier.Destroy;
begin
  if Assigned(FRegExp) then
    FreeAndNil(FRegExp);
  inherited;
end;

function MakeScopeExitNotifier(ARegEx: TSkRegExp): IInterface;
begin
  Result := TScopeExitNotifier.Create(ARegEx);
end;

function ConvertOptions(AOptions: TRegExOptions): TREOptions;
begin
  Result := [];

  if roIgnoreCase in AOptions then
    Result := Result + [TREOption.roIgnoreCase];
  if roMultiLine in AOptions then
    Result := Result + [TREOption.roMultiLine];
  if roSingleLine in AOptions then
    Result := Result + [TREOption.roSingleLine];
  if roExplicitCapture in AOptions then
    Result := Result + [TREOption.roNamedGroupOnly];
  if roIgnorePatternSpace in AOptions then
    Result := Result + [TREOption.roExtended];
end;

{ TGroup }

constructor TGroup.Create(const AValue: REString; AIndex, ALength: Integer;
  const AGroupName: REString);
begin
  FValue := AValue;
  FIndex := AIndex;
  FLength := ALength;
  FSuccess := AIndex <> 0;
  FGroupName := AGroupName;
end;

function TGroup.GetGroupName: REString;
begin
  Result := FGroupName;
end;

function TGroup.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TGroup.GetLength: Integer;
begin
  Result := FLength;
end;

function TGroup.GetValue: REString;
begin
  Result := Copy(FValue, FIndex, FLength);
end;

{ TGroupCollectionEnumerator }

constructor TGroupCollectionEnumerator.Create(AList: TGroupArray);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TGroupCollectionEnumerator.GetCurrent: TGroup;
begin
  Result := FList[FIndex];
end;

function TGroupCollectionEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < Length(FList) - 1;
  if Result then
    Inc(FIndex);
end;

{ TGroupCollection }

constructor TGroupCollection.Create(ARegEx: TSkRegExp; const AValue: REString;
  AIndex, ALength: Integer; ANotifier: IInterface);
var
  I: Integer;
begin
  FNotifier := ANotifier;
  SetLength(FList, 0);

  if ARegEx.Groups[0].Success then
  begin
    SetLength(FList, ARegEx.GroupCount + 1);

    for I := 0 to ARegEx.GroupCount do
    begin
      FList[I] := TGroup.Create(ARegEx.InputString, ARegEx.Groups[I].Index, ARegEx.Groups[I].Length,
        ARegEx.GroupNameFromIndex[I]);
    end;
  end;
end;

function TGroupCollection.GetCount: Integer;
begin
  Result := Length(FList);
end;

function TGroupCollection.GetEnumerator: TGroupCollectionEnumerator;
begin
  Result := TGroupCollectionEnumerator.Create(FList);
end;

function TGroupCollection.GetItem(Index: Variant): TGroup;
var
  LIndex: Integer;
  S: REString;
begin
  case VarType(Index) of
    varInteger, varWord, varByte:
      begin
        LIndex := Index;
        if (LIndex >= 0) and (LIndex <= Count - 1) then
          Result := FList[LIndex]
        else
          Result := TGroup.Create('', 0, 0, '');
      end;
    else
      begin
        S := Index;
        LIndex := IndexOfMatchedGroupName(S);
        if LIndex <> -1 then
          Result := FList[LIndex]
        else
          Result := TGroup.Create('', 0, 0, '');
      end;
  end;
end;

function TGroupCollection.IndexOfMatchedGroupName(
  const AGroupName: REString): Integer;
begin
  for Result := Length(FList) - 1 downto 1 do
  begin
    if FList[Result].GroupName = AGroupName then
    begin
      if FList[Result].Success then
        Exit;
    end;
  end;
  Result := -1;
end;

{ TMatch }

constructor TMatch.Create(ARegEx: TSkRegExp; const AValue: REString;
  AIndex, ALength: Integer; ANotifier: IInterface);
begin
  FRegExp := ARegEx;
  FNotifier := ANotifier;

  FGroup := TGroup.Create(AValue, AIndex, ALength, '');
  FGroups := TGroupCollection.Create(FRegExp, AValue, AIndex, ALength, ANotifier);
end;

function TMatch.GetGroups: TGroupCollection;
begin
  Result := FGroups;
end;

function TMatch.GetIndex: Integer;
begin
  Result := FGroup.Index;
end;

function TMatch.GetLength: Integer;
begin
  Result := FGroup.Length;
end;

function TMatch.GetSuccess: Boolean;
begin
  Result := FGroup.Success;
end;

function TMatch.GetValue: REString;
begin
  Result := FGroup.Value;
end;

function TMatch.NextMatch: TMatch;
begin
  if Assigned(FRegExp) then
    if FRegExp.ExecNext then
      Result := TMatch.Create(FRegExp, FRegExp.InputString, FRegExp.Groups[0].Index,
        FRegExp.Groups[0].Length, FNotifier)
    else
      Result := TMatch.Create(FRegExp, FRegExp.InputString, 0, 0, nil);
end;

function TMatch.Result(const Pattern: REString): REString;

  function GetControlCode(const S: REString; var Index: Integer): WideChar;
  var
    N: Integer;
  begin
    Inc(Index);
    if ((S[Index] >= '@') and (S[Index] <= '_')) or
        ((S[Index] >= 'a') and (S[Index] <= 'z')) then
    begin
      if S[Index] = '\' then
      begin
        Inc(Index);
        if S[Index] <> '\' then
          raise ESkRegExp.Create('');
      end;

      N := Ord(S[Index]);
      if (S[Index] >= 'a') and (S[Index] <= 'z') then
        Dec(N, $20);
      N := N xor $40;
      Result := WideChar(N);
    end
    else
      raise ESkRegExp.Create('');
  end;

  function GetHexDigit(const S: REString; var Index: Integer): WideChar;
  var
    I, N, C: Integer;
  begin
    N := 0;
    Inc(Index);

    if S[Index] = '{' then
    begin
      Inc(Index);

      C := 0;
      for I := 0 to 5 do
      begin
        case S[Index + I] of
          '0' .. '9':
            N := (N shl 4) + Ord(S[Index + I]) - Ord('0');
          'A' .. 'F':
            N := (N shl 4) + Ord(S[Index + I]) - Ord('7');
          'a' .. 'f':
            N := (N shl 4) + Ord(S[Index + I]) - Ord('W');
          '}':
            Break;
          else
            raise Exception.Create('16進数が必要');
        end;
        Inc(C);
      end;
      Inc(Index, C);

      if S[Index] <> '}' then
        raise Exception.Create('} が必要');

      Result := WideChar(N);
    end
    else
    begin
      C := 0;
      for I := 0 to 1 do
      begin
        case S[Index + I] of
          '0' .. '9':
            N := (N shl 4) + Ord(S[Index + I]) - Ord('0');
          'A' .. 'F':
            N := (N shl 4) + Ord(S[Index + I]) - Ord('7');
          'a' .. 'f':
            N := (N shl 4) + Ord(S[Index + I]) - Ord('W');
          else
            raise Exception.Create('16進数が必要');
        end;
        Inc(C);
      end;
      Inc(Index, C);

      Result := WideChar(N);
    end;
  end;

  function GetOctecDigit(const S: REString; var Index: Integer): WideChar;
  var
    I, N, C: Integer;
  begin
    N := 0;

    C := 0;
    for I := 0 to 2 do
    begin
      case S[Index + I] of
        '0' .. '7':
          N := (N shl 3) + Ord(S[Index + I]) - Ord('0');
        else
          Break;
      end;
      Inc(C);
    end;
    Inc(Index, C);
    Result := WideChar(N);
  end;

var
  K: Integer;
  LGroupName: REString;
  I, L: Integer;
begin
  Result := '';
  if Pattern = '' then
    Exit;

  I := 1;
  L := System.Length(Pattern);

  while I <= L do
  begin
    if Pattern[I] = '$' then
    begin
      Inc(I);
      if (Pattern[I] >= '0') and (Pattern[I] <= '9') then
      begin
        K := (Integer(Pattern[I]) - Integer('0'));
        if K <= FGroups.Count - 1 then
          Result := Result + FGroups[K].Value;
      end
      else if Pattern[I] = '{' then
      begin
        Inc(I);
        LGroupName := '';
        while (Pattern[I] <> '}') and (Pattern[I] <> #0000) do
        begin
          LGroupName := LGroupName + Pattern[I];
          Inc(I);
        end;

        if FGroups.IndexOfMatchedGroupName(LGroupName) <> -1 then
          Result := Result + FGroups[LGroupName].Value;
      end
      else if Pattern[I] = '&' then
        Result := Result + FGroup.Value
      else if Pattern[I] = '$' then
        Result := Result + '$'
      else if Pattern[I] = '`' then
      begin
        Result := Result + Copy(FGroup.FValue, 1, FGroup.Index - 1);
      end
      else if Pattern[I] = '''' then
      begin
        Result := Result + Copy(FGroup.FValue, FGroup.Index + FGroup.Length, MaxInt);
      end
      else if Pattern[I] = '_' then
      begin
        Result := Result + FGroup.FValue;
      end
      else if Pattern[I] = '+' then
      begin
        for I := FGroups.Count - 1 downto 1 do
        begin
          if FGroups[I].Index > 0 then
          begin
            Result := Result + FGroups[I].Value;
            Break;
          end;
        end;
      end
      else
        Result := Result + Pattern[I];
    end
    else if Pattern[I] = '\' then
    begin
      Inc(I);
      case Pattern[I] of
        '0':
          Result := Result + GetOctecDigit(Pattern, I);
        'c':
          Result := Result + GetControlCode(Pattern, I);
        'x':
          Result := Result + GetHexDigit(Pattern, I);
        't':
          Result := Result + #0009;
        'n':
          Result := Result + #0010;
        'r':
          Result := Result + #$000D;
        'f':
          Result := Result + #$000C;
        'a':
          Result := Result +  #0007;
        'e':
          Result := Result + #$001B;
        else
          Result := Result + Pattern[I];
      end;
    end
    else
      Result := Result + Pattern[I];

    Inc(I);
  end;
end;

{ TMatchCollectionEnumerator }

constructor TMatchCollectionEnumerator.Create(AList: TMatchArray);
begin
  FList := AList;
  FIndex := -1;
end;

function TMatchCollectionEnumerator.GetCurrent: TMatch;
begin
  Result := FList[FIndex];
end;

function TMatchCollectionEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < Length(FList) - 1;
  if Result then
    Inc(FIndex);
end;

{ TMatchCollection }

constructor TMatchCollection.Create(ARegEx: TSkRegExp; const Input: REString;
  AOptions: TRegExOptions; StartPos: Integer; ANotifier: IInterface);
var
  Index: Integer;
begin
  Index := 0;

  ARegEx.InputString := Input;
  ARegEx.Options := ConvertOptions(AOptions);

  if ARegEx.ExecPos(StartPos) then
  begin
    repeat
      SetLength(FList, Index + 1);
      FList[Index] := TMatch.Create(ARegEx, ARegEx.InputString, ARegEx.Groups[0].Index,
        ARegEx.Groups[0].Length, ANotifier);
      Inc(Index);
    until not ARegEx.ExecNext;
  end;
end;

function TMatchCollection.GetCount: Integer;
begin
  Result := Length(FList);
end;

function TMatchCollection.GetEnumerator: TMatchCollectionEnumerator;
begin
  Result := TMatchCollectionEnumerator.Create(FList);
end;

function TMatchCollection.GetItem(Index: Integer): TMatch;
begin
  Result := FList[Index];
end;

{ TRegEx }

constructor TRegEx.Create(const Pattern: REString);
begin
  Create(Pattern, []);
end;

constructor TRegEx.Create(const Pattern: REString; Options: TRegExOptions);
begin
  FRegExp := TSkRegExp.Create(Pattern, ConvertOptions(Options));
  if roCompiled in Options then
    FRegExp.Compile;
  FNotifier := MakeScopeExitNotifier(FRegExp);
end;

class function TRegEx.Escape(const Str: string; UseWildCards: Boolean): string;
// from RegularExpression.pas
const
  Special: array [1 .. 14] of string = ('\', '[', ']', '^', '$', '.', '|', '?',
    '*', '+', '(', ')', '{', '}'); // do not localize
var
  I: Integer;
begin
  Result := Str;
  for I := Low(Special) to High(Special) do
  begin
    Result := StringReplace(Result, Special[I], '\' + Special[I], [rfReplaceAll]); // do not localize
  end;
  // CRLF becomes \r\n
  Result := StringReplace(Result, #13#10, '\r\n', [rfReplaceAll]); // do not localize

  // If we're matching wildcards, make them Regex Groups so we can read them back if necessary
  if UseWildCards then
  begin
    // Replace all \*s with (.*)
    Result := StringReplace(Result, '\*', '(.*)', [rfReplaceAll]); // do not localize
    // Replace any \?s with (.)
    Result := StringReplace(Result, '\?', '(.)', [rfReplaceAll]); // do not localize

    // Wildcards can be escaped as ** or ??
    // Change back any escaped wildcards
    Result := StringReplace(Result, '(.*)(.*)', '\*', [rfReplaceAll]); // do not localize
    Result := StringReplace(Result, '(.)(.)', '\?', [rfReplaceAll]); // do not localize
  end;
end;

class function TRegEx.IsMatch(const Input, Pattern: REString;
  Options: TRegExOptions): Boolean;
var
  LRegEx: TRegEx;
begin
  LRegEx := TRegEx.Create(Pattern, Options);
  Result := LRegEx.IsMatch(Input)
end;

class function TRegEx.IsMatch(const Input, Pattern: REString): Boolean;
var
  LRegEx: TRegEx;
begin
  LRegEx := TRegEx.Create(Pattern);
  Result := LRegEx.IsMatch(Input)
end;

function TRegEx.IsMatch(const Input: REString): Boolean;
begin
  Result := FRegExp.Exec(Input);
end;

function TRegEx.IsMatch(const Input: REString; StartPos: Integer): Boolean;
begin
  FRegExp.InputString := Input;
  Result := FRegExp.ExecPos(StartPos);
end;

class function TRegEx.Match(const Input, Pattern: REString): TMatch;
var
  LRegEx: TRegEx;
begin
  LRegEx := TRegEx.Create(Pattern);
  Result := LRegEx.Match(Input);
end;

class function TRegEx.Match(const Input, Pattern: REString;
  Options: TRegExOptions): TMatch;
var
  LRegEx: TRegEx;
begin
  LRegEx := TRegEx.Create(Pattern, Options);
  Result := LRegEx.Match(Input);
end;

function TRegEx.Match(const Input: REString; StartPos, Length: Integer): TMatch;
begin
  FRegExp.InputString := Input;
  if FRegExp.ExecPos(StartPos, Length) then
    Result := TMatch.Create(FRegExp, FRegExp.InputString,
      FRegExp.Groups[0].Index, FRegExp.Groups[0].Length, FNotifier)
  else
    Result := TMatch.Create(FRegExp, '', 0, 0, FNotifier);
end;

function TRegEx.Match(const Input: REString): TMatch;
begin
  if FRegExp.Exec(Input) then
    Result := TMatch.Create(FRegExp, FRegExp.InputString,
      FRegExp.Groups[0].Index, FRegExp.Groups[0].Length, FNotifier)
  else
    Result := TMatch.Create(FRegExp, '', 0, 0, FNotifier);
end;

function TRegEx.Match(const Input: REString; StartPos: Integer): TMatch;
begin
  FRegExp.InputString := Input;
  if FRegExp.ExecPos(StartPos) then
    Result := TMatch.Create(FRegExp, FRegExp.InputString,
      FRegExp.Groups[0].Index, FRegExp.Groups[0].Length, FNotifier)
  else
    Result := TMatch.Create(FRegExp, '', 0, 0, FNotifier);
end;

function TRegEx.Matches(const Input: REString; StartPos: Integer)
  : TMatchCollection;
begin
  Result := TMatchCollection.Create(FRegExp, Input, [], StartPos, FNotifier);
end;

class function TRegEx.Matches(const Input, Pattern: REString): TMatchCollection;
var
  LRegEx: TRegEx;
begin
  LRegEx := TRegEx.Create(Pattern);
  Result := LRegEx.Matches(Input);
end;

class function TRegEx.Matches(const Input, Pattern: REString;
  Options: TRegExOptions): TMatchCollection;
var
  LRegEx: TRegEx;
begin
  LRegEx := TRegEx.Create(Pattern, Options);
  Result := LRegEx.Matches(Input);
end;

function TRegEx.Replace(const Input: REString; Evaluator: TMatchEvaluator;
  Count: Integer): REString;
begin
  Result := Replace(Input, Evaluator, Count, 1);
end;

function TRegEx.Replace(const Input, Replacement: REString): REString;
begin
  Result := Replace(Input, Replacement, 0, 1);
end;

function TRegEx.Replace(const Input: REString; Evaluator: TMatchEvaluator)
  : REString;
begin
  Result := Replace(Input, Evaluator, 0, 1)
end;

class function TRegEx.Replace(const Input, Pattern, Replacement: REString;
  Options: TRegExOptions): REString;
var
  LRegEx: TRegEx;
begin
  LRegEx := TRegEx.Create(Pattern, Options);
  Result := LRegEx.Replace(Input, Replacement);
end;

class function TRegEx.Replace(const Input, Pattern: REString;
  Evaluator: TMatchEvaluator; Options: TRegExOptions): REString;
var
  LRegEx: TRegEx;
begin
  LRegEx := TRegEx.Create(Pattern, Options);
  Result := LRegEx.Replace(Input, Evaluator);
end;

function TRegEx.ReplaceFunc(ARegExp: TSkRegExp): REString;
var
  LMatch: TMatch;
begin
  if Assigned(FEvaluator) then
  begin
    LMatch := TMatch.Create(FRegExp, FRegExp.InputString,
      FRegExp.Groups[0].Index, FRegExp.Groups[0].Length, FNotifier);
    Result := FEvaluator(LMatch);
  end
  else
    Result := '';
end;

function TRegEx.Replace(const Input: REString; Evaluator: TMatchEvaluator;
  Count, StartAt: Integer): REString;
{$IFNDEF UNICODE}
var
  Method: TMethod;
{$ENDIF}
begin
  FEvaluator := Evaluator;
  try
    {$IFDEF UNICODE}
    Result := FRegExp.Replace(Input, ReplaceFunc, Count, StartAt);
    {$ELSE}
    Method.Code := Addr(TRegEx.ReplaceFunc);
    Method.Data := Addr(Self);
    Result := FRegExp.Replace(Input, TSkRegExpReplaceFunction(Method), Count, StartAt);
    {$ENDIF}
  finally
    FEvaluator := nil;
  end;
end;

function TRegEx.Replace(const Input, Replacement: REString;
  Count: Integer): REString;
begin
  Result := Replace(Input, Replacement, Count, 1);
end;

function TRegEx.Replace(const Input, Replacement: REString; Count,
  StartAt: Integer): REString;
begin
  Result := FRegExp.Replace(Input, Replacement, Count, StartAt);
end;

class function TRegEx.Replace(const Input, Pattern, Replacement: REString)
  : REString;
var
  LRegEx: TRegEx;
begin
  LRegEx := TRegEx.Create(Pattern);
  Result := LRegEx.Replace(Input, Replacement);
end;

class function TRegEx.Replace(const Input, Pattern: REString;
  Evaluator: TMatchEvaluator): REString;
var
  LRegEx: TRegEx;
begin
  LRegEx := TRegEx.Create(Pattern);
  Result := LRegEx.Replace(Input, Evaluator);
end;

function TRegEx.Split(const Input: REString; Count: Integer): TStringDynArray;
begin
  Result := Split(Input, Count, 1);
end;

function TRegEx.Split(const Input: REString): TStringDynArray;
begin
  Result := Split(Input, 0, 1);
end;

function TRegEx.Split(const Input: REString; Count, StartPos: Integer)
  : TStringDynArray;
var
  I: Integer;
  LPieces: TREStrings;
begin
  LPieces := TREStringList.Create;
  try
    FRegExp.Split(Input, LPieces, Count, StartPos);

    SetLength(Result, LPieces.Count);
    for I := 0 to LPieces.Count - 1 do
      Result[I] := LPieces[I];

  finally
    LPieces.Free;
  end;
end;

class function TRegEx.Split(const Input, Pattern: REString;
  Options: TRegExOptions): TStringDynArray;
var
  LRegEx: TRegEx;
begin
  LRegEx := TRegEx.Create(Pattern, Options);
  Result := LRegEx.Split(Input, 0, 1);
end;

class function TRegEx.Split(const Input, Pattern: REString): TStringDynArray;
var
  LRegEx: TRegEx;
begin
  LRegEx := TRegEx.Create(Pattern);
  Result := LRegEx.Split(Input);
end;

function TRegEx.Matches(const Input: REString): TMatchCollection;
begin
  Result := TMatchCollection.Create(FRegExp, Input, [], 1, FNotifier);
end;

end.

