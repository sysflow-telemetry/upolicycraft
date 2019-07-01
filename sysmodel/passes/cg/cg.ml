open Core_kernel.Std
open Bap.Std
open Graphlib.Std
open Format
open Pervasives

include Self()

module CG = Graphs.Callgraph

let main entrypoint proj =
  let prog = Project.program proj in
  let cg = Program.to_graph prog in
    match Tid.from_string entrypoint with
      | Error _ -> failwith "invalid tid"
      | Ok tid ->
        Graphlib.postorder_traverse (module CG) ~start:tid cg |>
          Seq.map ~f:Tid.name |>
          Seq.take_while ~f:(fun x -> x <> entrypoint) |>
          Seq.iter ~f:(fun n -> printf "%s\n" n)

module Cmdline = struct
  open Config
  let entrypoint = param string "entrypoint"
                     ~doc:"Name of the source function in the callgraph."
  let () = when_ready (fun {get=(!!)} ->
      Project.register_pass' (main !!entrypoint))

  let () = manpage [
      `S "DESCRIPTION";
      `P
        "Collects all functions reachable from the $(v,entrypoint) parameter."
  ]
end
