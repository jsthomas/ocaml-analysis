(* open Core *)
open Core_bench.Std

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
  | _ -> raise (Invalid_argument "Invalid map implementation specifier")

let make_test l label =
  let impl = mapper_of_string label in
  Bench.Test.create ~name:label (fun () -> ignore (impl ((+)1) l))

let () =
  for k = 2 to 5 do
    let n = Batteries.Int.pow 10 k in
    let testlist = make_list n in
    let labels = ["tail rec."; "containers"; "batteries"; "base"; "stdlib"] in
    Printf.printf "Testing Lists of Size %d\n" n;
    Core.Command.run (Bench.make_command (List.map (make_test testlist) labels))
  done
