open Unix
open Printf

let make_list n =
  let rec make_list_loop l n =
    if n <= 0 then
      l
    else
      make_list_loop (0::l) (n - 1)
  in
  make_list_loop [] n

let measure m n mapper label =
  let rec repeat testlist accum n =
    match n with
    | 0 -> accum
    | _ ->

      let t0 = Unix.gettimeofday () in
      let () = mapper ((+)1) testlist |> ignore in
      let t1 = Unix.gettimeofday () in
      let delta = (t1 -. t0) *. 1e6 in
      Gc.full_major;
      repeat testlist (delta :: accum) (n - 1)
  in
  let testlist = make_list m in
  let total = repeat testlist [] n in
  printf "%s\n" label;
  List.iter (printf "%.2f\n") total

let mapper_of_string s =
  match s with
  | "control" -> (List.map)
  | "naive_tail_recursive" -> (fun f l -> List.rev_map f l |> List.rev)
  | "containers" -> CCList.map
  | "batteries" -> BatList.map
  | "base" -> (fun f l -> Base.List.map l f)
  | _ -> raise (Invalid_argument "Invalid map implementation specifier")

let () =
  let map_impl = mapper_of_string Sys.argv.(1) in
  let list_size = int_of_string Sys.argv.(2) in
  let reps = int_of_string Sys.argv.(3) in
  measure list_size reps map_impl Sys.argv.(1)
