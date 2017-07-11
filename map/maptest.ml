(* open Core *)
open Core_bench.Std

let make_list n =
  let rec make_list_loop l n =
    if n <= 0 then l
    else make_list_loop (0::l) (n - 1)
  in make_list_loop [] n

let mappers = [
  "stdlib",
    List.map;

  "tail rec.",
    (fun f l -> List.rev_map f l |> List.rev);

  "containers",
    CCList.map;

  "batteries",
    BatList.map;

  "base",
    (fun f l -> Base.List.map l f);

  "batt-hybr",
    Batt_hybr.map_opt;
]

let make_test l (label, impl) =
  Bench.Test.create ~name:label (fun () -> ignore (impl ((+) 1) l))

let () =
  for k = 2 to 5 do
    let n = Batteries.Int.pow 10 k in
    let testlist = make_list n in
    Printf.printf "Testing Lists of Size %d\n" n;
    Core.Command.run
      (Bench.make_command (List.map (make_test testlist) mappers))
  done
