open Lwt
open Sys

let run_trial read_fd write_fd text =
  let n = String.length text in
  let t0 = Unix.gettimeofday () in
  (Lwt_unix.write write_fd text 0 n >>= fun x ->
   Printf.printf "%f\n" ((Unix.gettimeofday () -. t0) *. 1e6);
   Lwt_unix.read read_fd text 0 n >>= fun y ->
   Lwt.return ()) |> ignore

let () =
  let size = int_of_string Sys.argv.(1) in
  let reps = int_of_string Sys.argv.(2) in
  let read_fd, write_fd = Lwt_unix.pipe () in
  let text = String.make size '0' in
  for rep = 1 to reps do
    run_trial read_fd write_fd text
  done
