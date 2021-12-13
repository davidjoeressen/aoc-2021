let read_line i = try Some (input_line i) with End_of_file -> None

let read_file file =
  let rec aux i acc = match (read_line i) with
    | None -> List.rev acc
    | Some s -> aux i (s :: acc) in
  aux (open_in file) []

let split_lines xs =
  let rec aux l acc = match l with
    | "" :: l' -> List.rev acc, l'
    | h :: l' -> aux l' (h :: acc)
    | [] -> (List.rev acc, l) in
  aux xs []

type instruction =
  | X of int
  | Y of int

let parse_point p =
  let x = String.split_on_char ',' p in
    (int_of_string @@ List.nth x 0, int_of_string @@ List.nth x 1)

let parse_instruction i =
  String.split_on_char ' ' i
  |> Fun.flip List.nth 2
  |> String.split_on_char '='
  |> (fun x -> (if (List.nth x 0) = "x" then X (int_of_string @@ List.nth x 1) else Y (int_of_string @@ List.nth x 1)))

let parse_file (points,instructions) =
  (List.map parse_point points, List.map parse_instruction instructions)

let fold_point n x =
  if x < n then x
  else if x > n then n - (x - n)
  else -1

let rec nub = function
  | [] -> []
  | x :: xs -> if List.exists (fun a -> a = x) xs then nub xs else x :: nub xs

let both_positive (x,y) = x >= 0 && y >= 0

let apply instr points = match instr with
  | X n -> nub @@ List.filter both_positive @@ List.map (fun (x,y) -> (fold_point n x, y)) points
  | Y n -> nub @@ List.filter both_positive @@ List.map (fun (x,y) -> (x, fold_point n y)) points

let format_points points =
  let (xs,ys) = List.split points in
    List.init (succ @@ List.fold_left max 0 ys) (fun y ->
      List.init (succ @@ List.fold_left max 0 xs) (fun x ->
        if (List.exists (fun a -> a = (x,y)) points) then "#" else " "))

let solve s =
  let (points,instructions) = read_file s |> split_lines |> parse_file in
    let points' = apply (List.hd instructions) points in
      print_string "Part 1: ";
      List.length points' |> string_of_int |> print_endline;
      print_endline "Part 2:";
      List.fold_left (Fun.flip apply) points' (List.tl instructions)
      |> format_points
      |> List.map (String.concat "")
      |> List.iter print_endline

let get_filename = function
  | _ :: f :: _ -> Some f
  | _ -> None

let () =
  Sys.argv
  |> Array.to_list
  |> get_filename
  |> Option.iter solve
