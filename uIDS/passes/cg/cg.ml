open Bap.Std
open Core_kernel
open Format
open Graphlib.Std
open Pervasives
open Yojson

include Self()

module CG = Graphs.Callgraph

let main entrypoint proj =
  let prog = Project.program proj in
  let cg = Program.to_graph prog in
  let nodes = match entrypoint with
                None -> Graphlib.postorder_traverse (module CG) cg
             | Some entrypoint ->
                (match Tid.from_string entrypoint with
                   Error err -> failwithf "Error processing entrypoint %s: %s" entrypoint (Error.to_string_hum err) ()
                 | Ok tid -> cg |>
                             Graphlib.postorder_traverse (module CG) ~start:tid |>
                             Seq.take_while ~f:(fun x -> (Tid.name x) <> entrypoint)) in
  let output = nodes |>
                  Seq.filter ~f:(fun n -> CG.Node.degree ~dir:`Out n cg = 0) |>
                  Seq.map ~f:(fun x -> (Tid.name x)) |>
                  Seq.to_list |>
                  List.map ~f:(fun n -> `String n) in
    printf "%s\n" (Yojson.Basic.pretty_to_string (`List output))

module Cmdline = struct
  open Config
  let entrypoint = param (some string) "entrypoint"
                     ~doc:"Name of the source function in the callgraph."
  let () = when_ready (fun {get=(!!)} ->
      Project.register_pass' (main !!entrypoint))

  let () = manpage [
      `S "DESCRIPTION";
      `P
        "Collects all functions reachable from the $(v,entrypoint) parameter."
  ]
end
