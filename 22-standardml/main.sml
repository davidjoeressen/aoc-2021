type range = int * int;
type cube = range * range * range;
datatype command = ON of cube | OFF of cube;

(* Utility functions *)
fun concatMap f = List.concat o map f;
fun flip f x y = f y x;
fun sum (xs: LargeInt.int list) = foldl op+ 0 xs;

fun nullRange (x1,x2) = x1 > x2;
fun nullCube (x,y,z) = nullRange x orelse nullRange y orelse nullRange z;
fun limitRange (x1,x2) (x3,x4) = (Int.max (x1, x3), Int.min (x4, x2));
fun limit (x1,y1,z1) (x2,y2,z2) = (limitRange x1 x2, limitRange y1 y2, limitRange z1 z2);
fun cutRange (x1,x2) (x3,x4) = ((x1,x3-1),(x3,x4),(x4+1,x2));
fun cut x y = if nullCube (limit x y) then
      [x]
   else let
      val (x1,y1,z1) = x
      val (x2,y2,z2) = y
      val (xc1,xc2,xc3) = cutRange x1 (limitRange x1 x2)
      val (yc1,yc2,yc3) = cutRange y1 (limitRange y1 y2)
      val (zc1,zc2,zc3) = cutRange z1 (limitRange z1 z2)
      val candidates =
         [(x1,y1,zc1),
          (x1,y1,zc3),
          (xc1,y1,zc2),
          (xc2,yc1,zc2),
          (xc2,yc3,zc2),
          (xc3,y1,zc2)]
   in
     List.filter (not o nullCube) candidates
   end;
fun eval (ON x, xs) = (x::concatMap (flip cut x) xs)
  | eval (OFF x, xs) = concatMap (flip cut x) xs;

fun rangeLength (x1, x2) = Int.toLarge x2 - Int.toLarge x1 + 1;
fun cubeSize (x,y,z) = rangeLength x * rangeLength y * rangeLength z;
val count = sum o map cubeSize o List.filter (not o nullCube);

(* File input *)
fun readFile file =
   let
      val stream = TextIO.openIn file
      fun loop s =
         case TextIO.inputLine s of
            SOME line => line :: loop s
          | NONE => []
   in
      loop stream before TextIO.closeIn stream
   end;

fun isNumeric c = Char.isDigit c orelse c = #"-";
fun parseNumbers s =
   case List.mapPartial Int.fromString (String.tokens (not o isNumeric) s) of
      [x1,x2,y1,y2,z1,z2] => SOME ((x1,x2),(y1,y2),(z1,z2))
    | _ => NONE

fun parse line = Option.map (fn x => if String.isPrefix "on" line then ON x else OFF x) (parseNumbers line);

fun solve file =
   let
      val cs = List.mapPartial parse (readFile file)
      val result = foldl eval [] cs
      val r = (~50, 50)
      val part1 = (LargeInt.toString o count o map (limit (r,r,r))) result
      val part2 = (LargeInt.toString o count) result
   in
     print (file ^ "\nPart 1: " ^ part1 ^ "\nPart 2: " ^ part2 ^ "\n")
   end;

fun main args =
   if List.length args = 0
      then print ("Usage: " ^ CommandLine.name () ^ " <file>\n")
   else app solve args;

main (CommandLine.arguments ());
