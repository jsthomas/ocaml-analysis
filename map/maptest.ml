(* open Core *)
open Core_bench.Std

(* Set up a modified version of batteries map with ideas from base. *)
type 'a mut_list =  {
  hd: 'a;
  mutable tl: 'a list
}

external inj : 'a mut_list -> 'a list = "%identity"

module Acc = struct
  let dummy () =
    { hd = Obj.magic (); tl = [] }
  let create x =
    { hd = x; tl = [] }
  let accum acc x =
    let cell = create x in
    acc.tl <- inj cell;
    cell
end

let map_imper f = function
  | [] -> []
  | h :: t ->
    let rec loop dst = function
      | [] -> ()
      | h :: t ->
        loop (Acc.accum dst (f h)) t
    in
    let r = Acc.create (f h) in
    loop r t;
    inj r

let rec count_map f l ctr =
  match l with
  | [] -> []
  | [x1] ->
    let f1 = f x1 in
    [f1]
  | [x1; x2] ->
    let f1 = f x1 in
    let f2 = f x2 in
    [f1; f2]
  | [x1; x2; x3] ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    [f1; f2; f3]
  | [x1; x2; x3; x4] ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    let f4 = f x4 in
    [f1; f2; f3; f4]
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    let f4 = f x4 in
    let f5 = f x5 in
    f1 :: f2 :: f3 :: f4 :: f5 ::
    (if ctr > 1000
     then map_imper f tl
     else count_map f tl (ctr + 1))

let map_opt f l = count_map f l 0

(* Define core_bench test below. *)

let make_list n =
  let rec make_list_loop l n =
    if n <= 0 then l
    else make_list_loop (0::l) (n - 1)
  in make_list_loop [] n

let mapper_of_string s =
  match s with
  | "stdlib" -> (List.map)
  | "tail rec." -> (fun f l -> List.rev_map f l |> List.rev)
  | "containers" -> CCList.map
  | "batteries" -> BatList.map
  | "base" -> (fun f l -> Base.List.map l f)
  | "batt-hybr" -> map_opt
  | _ -> raise (Invalid_argument "Invalid map implementation specifier")

let make_test l label =
  let impl = mapper_of_string label in
  Bench.Test.create ~name:label (fun () -> ignore (impl ((+) 1) l))

let () =
  for k = 2 to 5 do
    let n = Batteries.Int.pow 10 k in
    let testlist = make_list n in
    let labels = ["tail rec."; "containers"; "batteries"; "base"; "stdlib"; "batt-hybr"] in
    Printf.printf "Testing Lists of Size %d\n" n;
    Core.Command.run (Bench.make_command (List.map (make_test testlist) labels))
  done
