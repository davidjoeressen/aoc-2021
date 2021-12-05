program aoc_2021_day_5;
{$mode objfpc}

uses
  Classes, Sysutils, StrUtils, Types, Math;
const
  BOARD_SIZE = 999;
  MAX_NUM_INSTRUCTIONS = 500;
type
  Coords = record
    x1, y1, x2, y2: integer;
  end;

  Board = class
  private
    board: array[0..BOARD_SIZE] of array[0..BOARD_SIZE] of integer;
  public
    constructor Create();
    procedure AddLine(c: Coords);
    function CountIntersections(): integer;
  end;

  Instructions = class
  private
    orthogonalInstructions: array[0..MAX_NUM_INSTRUCTIONS] of Coords;
    numOrthogonalInstructions: integer;
    diagonalInstructions: array[0..MAX_NUM_INSTRUCTIONS] of Coords;
    numDiagonalInstructions: integer;
  public
    constructor Create();
    procedure ParseInstruction(const s: AnsiString);
    procedure ExecuteOrthogonalInstructions(b: Board);
    procedure ExecuteDiagonalInstructions(b: Board);
  end;

constructor Board.Create();
var
  x,y: integer;
begin
  for x := 0 to BOARD_SIZE do
    for y := 0 to BOARD_SIZE do
      board[x,y] := 0;
end;

procedure Board.AddLine(c: Coords);
var
  x, y, xdir, ydir: integer;
begin
  xdir := Sign(c.x2 - c.x1);
  ydir := Sign(c.y2 - c.y1);
  x := c.x1;
  y := c.y1;
  repeat
    board[x,y] := board[x,y] + 1;
    if (x = c.x2) and (y = c.y2) then break;
    x := x + xdir;
    y := y + ydir;
  until false;
end;

function Board.CountIntersections(): integer;
var
  x, y: integer;
begin
  result := 0;
  for x := 0 to BOARD_SIZE do
    for y := 0 to BOARD_SIZE do
      if board[x,y] > 1 then
        result := result + 1;
end;

constructor Instructions.Create();
begin
  numOrthogonalInstructions := -1;
  numDiagonalInstructions := -1;
end;

procedure Instructions.ParseInstruction(const s: AnsiString);
var
  c: Coords;
  split1, split2: TStringDynArray;
begin
  split1 := SplitString(s, ' -> ');
  split2 := SplitString(split1[0], ',');
  c.x1 := StrToInt(split2[0]);
  c.y1 := StrToInt(split2[1]);
  split2 := SplitString(split1[1], ',');
  c.x2 := StrToInt(split2[0]);
  c.y2 := StrToInt(split2[1]);
  if (c.x1 <> c.x2) and (c.y1 <> c.y2) then
  begin
    numDiagonalInstructions := numDiagonalInstructions + 1;
    diagonalInstructions[numDiagonalInstructions] := c;
  end else begin
    numOrthogonalInstructions := numOrthogonalInstructions + 1;
    orthogonalInstructions[numOrthogonalInstructions] := c;
  end;
end;

procedure Instructions.ExecuteOrthogonalInstructions(b: Board);
var
  i: integer;
begin
  for i := 0 to numOrthogonalInstructions do
    b.AddLine(orthogonalInstructions[i]);
end;

procedure Instructions.ExecuteDiagonalInstructions(b: Board);
var
  i: integer;
begin
  for i := 0 to numDiagonalInstructions do
    b.AddLine(diagonalInstructions[i]);
end;

var
  sl: TStringList;
  b: Board;
  i: Instructions;
begin
  if (ParamCount = 1) and FileExists(ParamStr(1)) then
  begin
    sl := TStringList.Create;
    i := Instructions.Create;
    b := Board.Create;

    sl.LoadFromFile(ParamStr(1));
    sl.forEach(@i.ParseInstruction);

    i.ExecuteOrthogonalInstructions(b);
    writeln('Part 1: ',b.CountIntersections());
    i.ExecuteDiagonalInstructions(b);
    writeln('Part 2: ',b.CountIntersections());

    sl.Free;
  end else writeln('use: main <infile>');
end.
