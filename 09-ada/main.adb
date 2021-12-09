with Ada.Text_Io;
use Ada.Text_Io;
with Ada.Command_line;
use Ada.Command_line;
with Ada.Containers;
use Ada.Containers;

procedure Main is
  type Digit is range 0..9;
  type Board is array(Positive range <>, Positive range <>) of Integer;
  type Coord is array(1..2) of Integer;
  type Coord_List is array(Positive range <>) of Coord;
  Type Int_Array is array(Positive range <>) of Integer;

  function Read_File(File_Name : String) return Board is
    Input : File_Type;
    TmpBoard : Board(1..1000,1..1000);
    N, M : Integer;
  begin
    Open (File => Input,
          Mode => In_File,
          Name => File_Name);
    N := 0;
    while not End_Of_File(Input) loop
      declare
        Line : String := Get_Line (Input);
      begin
        N := N + 1;
        M := 0;
        for index of Line loop
          if index in '0' .. '9' then
            M := M + 1;
            TmpBoard(N, M) := Character'Pos(index) - Character'Pos('0');
          end if;
        end loop;
      end;
    end loop;
    Close (Input);
    declare
      B: Board(1..N,1..M);
    begin
      for x in 1..N loop
        for y in 1..M loop
          B(x,y) := TmpBoard(x,y);
        end loop;
      end loop;
      return B;
    end;
  end Read_File;

  function Find_Low_Points(B : Board) return Coord_List is
    Low_Points : Coord_List(1..200);
    N : Integer := 0;
  begin
    for x in B'Range(1) loop
      for y in B'Range(2) loop
        declare
          e : Integer := B(x,y);
          l,r,u,d : Boolean;
        begin
          u := x = 1 or else B(x-1,y) > e;
          d := x = B'Length(1) or else B(x+1,y) > e;
          l := y = 1 or else B(x,y-1) > e;
          r := y = B'Length(2) or else B(x,y+1) > e;
          if u and d and l and r then
            N := N + 1;
            Low_Points(N) := (x,y);
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
        X : Integer := Coords(Current_Coord)(1);
        Y : Integer := Coords(Current_Coord)(2);
        E : Integer := B(X, Y);
        Neighbours : Coord_List(1..4) := ((X-1,Y),(X+1,Y),(X,Y-1),(X,Y+1));
      begin
        for N of Neighbours loop
          if N(1) in B'Range(1) and then
            N(2) in B'Range(2) and then
            B(N(1),N(2)) < 9 and then
            B(N(1),N(2)) > E and then
            not Element(Coords(1..Num_Coords), N) then
            Num_Coords := Num_Coords + 1;
            Coords(Num_Coords) := N;
          end if;
        end loop;
        Current_Coord := Current_Coord + 1;
      end;
    end loop;
    for C of Coords(1..Num_Coords) loop
      declare
        e : Integer := B(C(1), C(2));
        x : Integer := C(1);
        y : Integer := C(2);
        Neighbours : Coord_List(1..4) := ((x-1,y),(x+1,y),(x,y-1),(x,y+1));
        valid : Boolean := True;
      begin
        for Neighbour of Neighbours loop
          if Neighbour(1) in B'Range(1) and then
            Neighbour(2) in B'Range(2) and then
            B(Neighbour(1),Neighbour(2)) < e and then
            not Element(Coords(1..Num_Coords), Neighbour) then
            valid := false;
          end if;
        end loop;
        if valid then
          Basin_Size := Basin_Size + 1;
        end if;
      end;
    end loop;
    return Basin_Size;
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
    sum : Integer := 0;
  begin
    for x of Low_Points loop
      sum := sum + B(x(1),x(2)) + 1;
    end loop;
    return sum;
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
