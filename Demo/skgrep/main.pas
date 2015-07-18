unit main;

interface

uses Classes, SysUtils, SkRegExpW, SkFindFile;

type
  TOption = (optViewLineNo, optNotMatch);
  TOptions = set of TOption;

  TFileSearch = class
  private
    FRegExp: TSkRegExp;
    FFindFile: TSkFindFile;
    FOptions: TOptions;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Exec;
    function Setup: Boolean;
    procedure OnFind(Sender: TObject;
      const FileName: string; SearchRec: TSearchRec; var Accept, Cancel: Boolean);
    procedure ViewHelp;
    property RegExp: TSkRegExp read FRegExp;
    property FindFile: TSkFindFile read FFindFile;
    property Options: TOptions read FOptions write FOptions;
  end;

implementation

{ TSkGrep }

constructor TFileSearch.Create;
begin
  inherited;
  FFindFile := TSkFindFile.Create(nil);
  FFindFile.OnFind := OnFind;
  FRegExp := TSkRegExp.Create;
end;

destructor TFileSearch.Destroy;
begin
  FRegExp.Free;
  FFindFile.Free;
  inherited;
end;

procedure TFileSearch.Exec;
begin
  FFindFile.Execute;
end;

procedure TFileSearch.OnFind(Sender: TObject; const FileName: string;
  SearchRec: TSearchRec; var Accept, Cancel: Boolean);

  procedure ViewLine(const S: string; LineNo: Integer);
  begin
    if optViewLineNo in FOptions then
      System.Writeln(
        Format('(%d) %s', [LineNo + 1, S]))
    else
      System.Writeln(S);
  end;

var
  SL: TStrings;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);

    System.Writeln(FileName);

    for I := 0 to SL.Count - 1 do
    begin
      if FRegExp.Exec(SL[I]) then
      begin
        ViewLine(SL[I], I);
      end
      else
      begin
        if optNotMatch in FOptions then
          ViewLine(SL[I], I);
      end;
    end;
  finally
    SL.Free;
  end;
end;

function TFileSearch.Setup: Boolean;
var
  I, L, N: Integer;
  S: string;
begin
  Result := True;
  FOptions := [];
  L := ParamCount;

  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if S = '?' then
    begin
      Result := False;
      Exit;
    end
    else if S[1] = '-' then
    begin
      case S[2] of
        '?':
          begin
            Result := False;
            Exit;
          end;
        'i':
          FRegExp.Options := FRegExp.Options + [roIgnoreCase];
        'n':
          FOptions := FOptions + [optViewLineNo];
        'v':
          FOptions := FOptions + [optNotMatch];
        'r':
          FFindFile.SubDirectory := True;
      end;
    end
    else
    begin
      N := I;
      Break;
    end;
  end;

  if S <> '' then
  begin
    FRegExp.Expression := S;
    Inc(N);
  end;

  if N <= ParamCount then
  begin
    S := ParamStr(N);
    FFindFile.Directory := ExtractFilePath(S);
    FFindFile.Mask := ExtractFileName(S);
  end
  else
    raise Exception.Create('No file specified');
end;

procedure TFileSearch.ViewHelp;
begin
  Writeln('Syntax:  skgrep [-inrv] searchstring file');
  Writeln('');
  Writeln('-i  Ignore case');
  Writeln('-n  Line numbers');
  Writeln('-r  Search subdirectories');
  Writeln('-v  Non-matching lines only');
end;

end.
