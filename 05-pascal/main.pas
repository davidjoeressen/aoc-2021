program aoc_2021_day_5;
{$mode objfpc}

uses
  Classes, Sysutils, StrUtils, Types, Math;
const
  BOARD_SIZE = 999;
type
  coords = record
    x1, y1, x2, y2: integer;
  end;
  LineProcessor = class
  private
    board: array[0..BOARD_SIZE] of array[0..BOARD_SIZE] of integer;
    function ParseLine(const s: string): coords;
    procedure AddLine(c: coords);
  public
    checkDiagonals: boolean;
    constructor Create();
    procedure ProcessLine(const s: AnsiString);
    function CountIntersections(): integer;
  end;

constructor LineProcessor.Create();
var
  x,y: integer;
begin
  checkDiagonals := false;
  for x := 0 to BOARD_SIZE do
    for y := 0 to BOARD_SIZE do
      board[x,y] := 0;
end;

function LineProcessor.ParseLine(const s: string): coords;
var
  split1, split2: TStringDynArray;
begin
  split1 := SplitString(s, ' -> ');
  split2 := SplitString(split1[0], ',');
  parseLine.x1 := StrToInt(split2[0]);
  parseLine.y1 := StrToInt(split2[1]);
  split2 := SplitString(split1[1], ',');
  parseLine.x2 := StrToInt(split2[0]);
  parseLine.y2 := StrToInt(split2[1]);
end;

procedure LineProcessor.AddLine(c: coords);
var
  x, y, xdir, ydir: integer;
  isDiagonal: boolean;
begin
  xdir := Sign(c.x2 - c.x1);
  ydir := Sign(c.y2 - c.y1);
  isDiagonal := (xdir <> 0) and (ydir <> 0);
  if checkDiagonals or not isDiagonal then
  begin
    x := c.x1;
    y := c.y1;
    repeat
      board[x,y] := board[x,y] + 1;
      x := x + xdir;
      y := y + ydir;
    until (x = c.x2) and (y = c.y2);
    board[c.x2,c.y2] := board[c.x2,c.y2] + 1;
  end;
end;

procedure LineProcessor.ProcessLine(const s: AnsiString);
begin
  AddLine(ParseLine(s));
end;

function LineProcessor.CountIntersections(): integer;
var
  out, x, y: integer;
begin
  out := 0;
  for x := 0 to BOARD_SIZE do
    for y := 0 to BOARD_SIZE do
      if board[x,y] > 1 then
        out := out + 1;
  CountIntersections := out;
end;

var
  sl: TStringList;
  lp: LineProcessor;
begin
  if (ParamCount = 1) and FileExists(ParamStr(1)) then
  begin
    sl := TStringList.Create;
    sl.LoadFromFile(ParamStr(1));

    // Part 1
    lp := LineProcessor.Create;
    sl.forEach(@lp.processLine);
    writeln('Part 1: ',lp.CountIntersections());

    // Part 2
    lp := LineProcessor.Create;
    lp.checkDiagonals := true;
    sl.forEach(@lp.processLine);
    writeln('Part 2: ',lp.CountIntersections());

    sl.Free;
  end else writeln('use: main <infile>');
end.
