with Ada.Text_Io;
use Ada.Text_Io;
with Ada.Command_line;
use Ada.Command_line;

procedure Main is
  type Board is array(Positive range <>, Positive range <>) of Integer;
  type Coord is record
    X, Y : Integer;
  end record;
  type Coord_List is array(Positive range <>) of Coord;
  Type Int_Array is array(Positive range <>) of Integer;

  function Read_File(File_Name : String) return Board is
    Input : File_Type;
    TmpBoard : Board(1..1000,1..1000);
    N, M : Integer := 0;
  begin
    Open (File => Input,
          Mode => In_File,
          Name => File_Name);
    while not End_Of_File(Input) loop
      declare
        Line : String := Get_Line (Input);
      begin
        N := N + 1;
        M := 0;
        for Char of Line loop
          if Char in '0' .. '9' then
            M := M + 1;
            TmpBoard(N, M) := Character'Pos(Char) - Character'Pos('0');
          end if;
        end loop;
      end;
    end loop;
    Close (Input);
    declare
      B: Board(1..N,1..M);
    begin
      for X in 1..N loop
        for Y in 1..M loop
          B(X,Y) := TmpBoard(X,Y);
        end loop;
      end loop;
      return B;
    end;
  end Read_File;

  function Get_Neighbours(C : Coord) return Coord_List is
    Neighbours : Coord_List(1..4) := ((C.X-1,C.Y),(C.X+1,C.Y),(C.X,C.Y-1),(C.X,C.Y+1));
  begin
    return Neighbours;
  end Get_Neighbours;

  function Get_Or_Else(B : Board; C : Coord; N : Integer) return Integer is
  begin
    if C.X in B'Range(1) and C.Y in B'Range(2) then
      return B(C.X,C.Y);
    end if;
    return N;
  end Get_Or_Else;

  function Find_Low_Points(B : Board) return Coord_List is
    Low_Points : Coord_List(1..200);
    N : Integer := 0;
  begin
    for X in B'Range(1) loop
      for Y in B'Range(2) loop
        declare
          E : Integer := B(X,Y);
          Neighbours : Coord_List := Get_Neighbours((X,Y));
          Is_Low_Point : Boolean := True;
        begin
          for Neighbour of Neighbours loop
            if Get_Or_Else(B, Neighbour, 10) <= E then
              Is_Low_Point := false;
            end if;
          end loop;
          if Is_Low_Point then
            N := N + 1;
            Low_Points(N) := (X,Y);
          end if;
        end;
      end loop;
    end loop;
    return Low_Points(1..N);
  end Find_Low_Points;

  function Element(List : Coord_List; C : Coord) return Boolean is
  begin
    for X of List loop
      if X = C then
        return true;
      end if;
    end loop;
    return false;
  end Element;

  function Get_Basin_Size(B: Board; Start : Coord) return Integer is
    Coords : Coord_List(1..1000);
    Num_Coords, Current_Coord : Integer := 1;
    Basin_Size : Integer := 0;
  begin
    Coords(1) := Start;
    while Current_Coord <= Num_Coords loop
      declare
        C : Coord := Coords(Current_Coord);
        E : Integer := B(C.X, C.Y);
        Neighbours : Coord_List := Get_Neighbours(C);
      begin
        for N of Neighbours loop
          if Get_Or_Else(B, N, 9) in (E+1) .. 8 and then
            not Element(Coords(1..Num_Coords), N) then
            Num_Coords := Num_Coords + 1;
            Coords(Num_Coords) := N;
          end if;
        end loop;
        Current_Coord := Current_Coord + 1;
      end;
    end loop;
    return Num_Coords;
  end Get_Basin_Size;

  function Biggest_Three(L : Int_Array) return Int_Array is
    Tmp : Int_Array(1..3) := (0,0,0);
  begin
    for I of L loop
      declare
        Smallest : Integer := 1;
      begin
        for X in Tmp'Range loop
          if Tmp(X) < Tmp(Smallest) then
            Smallest := X;
          end if;
        end loop;
        if I > Tmp(Smallest) then
          Tmp(Smallest) := I;
        end if;
      end;
    end loop;
    return Tmp;
  end;

  function Calculate_Part1(B : Board; Low_Points : Coord_List) return Integer is
    Sum : Integer := 0;
  begin
    for C of Low_Points loop
      Sum := Sum + B(C.X,C.Y) + 1;
    end loop;
    return Sum;
  end Calculate_Part1;

  function Calculate_Part2(B : Board; Low_Points : Coord_List) return Integer is
    Basin_Sizes : Int_Array(Low_Points'Range);
    Biggest : Int_Array(1..3);
  begin
    for X in Low_Points'Range loop
      Basin_Sizes(X) := Get_Basin_Size(B, Low_Points(X));
    end loop;
    Biggest := Biggest_Three(Basin_Sizes);
    return Biggest(1) * Biggest(2) * Biggest(3);
  end Calculate_Part2;
begin
  if Argument_Count < 1 then
    Put_Line("Usage: " & Command_Name & " <file>");
    return;
  end if;
  declare
    B : Board := Read_File(Argument(1));
    Low_Points : Coord_List := Find_Low_Points(B);
  begin
    Put_Line("Part 1: " & Integer'Image(Calculate_Part1(B, Low_Points)));
    Put_Line("Part 2: " & Integer'Image(Calculate_Part2(B, Low_Points)));
  end;
end Main;
