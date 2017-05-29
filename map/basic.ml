open Unix;;
open Printf;;

(* Raw map implementation *)
let rec map f = function
    | [] -> []
    | a::l ->
      let r = f a in
      let l = map f l in   (* The recursive call *)
      r :: l               (* Stuff done after the recursive call returns *)

let make_list n =
  let rec make_list_loop l n =
    if n <= 0 then
      l
    else
      make_list_loop (0::l) (n - 1)
  in
  make_list_loop [] n

let measure m n label mapper =
  let rec repeat testlist accum n =
    match n with
    | 0 -> accum
    | _ ->

      let t0 = Unix.gettimeofday () in
      let () = mapper ((+)1) testlist |> ignore in
      let t1 = Unix.gettimeofday () in
      let delta = (t1 -. t0) *. 1e6 in
      repeat testlist (delta :: accum) (n - 1)
  in
  let testlist = make_list m in
  let total = repeat testlist [] n in
  List.iter (printf "%.2f\n") total;;

let () =
  let list_size = int_of_string Sys.argv.(1) in
  let reps = int_of_string Sys.argv.(2) in
  measure list_size reps "basic" (map);;
