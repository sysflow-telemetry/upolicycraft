open Core_kernel.Std
open Bap.Std
open Graphlib.Std
open Format
open Pervasives

(** Deprecated *)

include Self()

let main proj =
  let prog = Project.program proj in
  let cg = Program.to_graph prog in
  let () = Format.printf "Nodes: %d" (CG.number_of_nodes cg) in
           Format.printf "Edges: %d" (CG.number_of_edges cg)
  (** CG.pp Format.std_formatter cg *)

let () = Project.register_pass' main
