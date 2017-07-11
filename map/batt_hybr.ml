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
