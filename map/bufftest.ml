open Core_bench.Std
open Lwt
open Sys

let run_trial read_fd write_fd text =
  let n = String.length text in
  ((Lwt_unix.write write_fd text 0 n >>= fun x ->
   Lwt.return ()) |> ignore;
   Lwt_unix.read read_fd text 0 n >>= fun y ->
   Lwt.return ()) |> ignore

let () =
  let read_fd, write_fd = Lwt_unix.pipe () in
  let text = String.make 4096 '0' in
  Core.Command.run (Bench.make_command [Bench.Test.create
                                          ~name:"pipe read-write"
                                          (fun () -> run_trial read_fd write_fd text) ])
