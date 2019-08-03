open Core_kernel
open Bap.Std
open Graphlib.Std
open Format
open Pervasives

include Self()

module CG = Graphs.Callgraph

module EdgeSet = Set.Make(struct
    type t = string * string
    let sexp_of_t=sexp_of_opaque
    let t_of_sexp=opaque_of_sexp
    let compare (x,y) (x',y') =
      match compare x y with
        0 -> compare x' y'
      | x -> x
end)

let tail str =
    if str = "" then "" else
    String.sub str 1 ((String.length str) - 1)

let main entrypoint proj =
  let prog = Project.program proj in
  let cg = Program.to_graph prog in
    match entrypoint with
      None ->
        let v = CG.nodes cg in
        let e = CG.edges cg in
        let vs = v |>
                Seq.map ~f:(fun node -> node |> Tid.name |> tail) |>
                Seq.to_list |>
                String.Set.of_list in
        let es = e |>
                Seq.map ~f:(fun edge ->
                        let name = Tid.name in
                        (name (CG.Edge.src edge), name (CG.Edge.dst edge))) |>
                Seq.to_list |>
                EdgeSet.of_list in
          String.Set.iter ~f:(fun s -> printf "%s\n" s) vs
    | Some entrypoint -> (match Tid.from_string entrypoint with
          Error _ -> failwith "invalid tid"
        | Ok tid ->
            Graphlib.postorder_traverse (module CG) ~start:tid cg |>
              Seq.map ~f:Tid.name |>
              Seq.take_while ~f:(fun x -> x <> entrypoint) |>
              Seq.iter ~f:(fun n -> printf "%s\n" n))

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
