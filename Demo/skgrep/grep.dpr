program grep;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  SkFindFile,
  main in 'main.pas';

var
  Search: TFileSearch;

begin
  try
    { TODO -oUser -cConsole メイン : ここにコードを記述してください }
    Search := TFileSearch.Create;
    try
      if Search.Setup then
        Search.Exec
      else
        Search.ViewHelp;
    finally
      Search.Free;
    end;

    if DebugHook <>0 then
      Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
