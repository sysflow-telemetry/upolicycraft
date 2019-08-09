open Bap.Std
open Core_kernel
open Format
open Graphlib.Std
open Odot
open Pervasives

include Self()

module CG = Graphs.Callgraph

let strip_both_chars str =
  match String.length str with
  | 0 | 1 | 2 -> ""
  | len -> String.sub str 1 (len - 2);;

module EdgeSet = Set.Make(struct
  type t = string * string
  let sexp_of_t=sexp_of_opaque
  let t_of_sexp=opaque_of_sexp
  let compare (x,y) (x',y') =
    match compare x x' with
      0 -> compare y y'
    | x -> x
end)

let tail str =
    if str = "" then "" else
    String.sub str 1 ((String.length str) - 1)

let process_dot_file file =
  let graph = Odot.parse_file file in
  let stmt = graph.stmt_list in
  let name id =
    match id with
      Simple_id id -> id
    | Html_id id -> id
    | Double_quoted_id id -> id in
  let name_of_point edge =
    match edge with
      Edge_node_id nid -> nid |> fst |> name
    | Edge_subgraph _ -> "" in
  let nodes = stmt |> List.filter ~f:(fun stmt ->
                              match stmt with
                                Stmt_node _ -> true
                              | _ -> false)
                   |> List.map ~f:(fun stmt ->
                              let Stmt_node (id, attrs) = stmt in
                                  match (List.nth attrs 1) with
                                    None -> ("", "")
                                  | Some (label, opt) ->
                                    (match opt with
                                       None -> ("", "")
                                     | Some v ->
                                       let (i, opt) = id in
                                       let nid = name i in
                                       (nid, strip_both_chars (name v)))) in
  let v = nodes |> List.map ~f:(fun (_, n) -> n) |> String.Set.of_list in
  (**
  let () = List.iter ~f:(fun (id, name) ->
          Printf.printf "%s: %s\n" id name) nodes in *)
  let edges = stmt |> List.filter ~f:(fun stmt ->
                              match stmt with
                                Stmt_edge e -> true
                              | _ -> false)
                   |> List.map ~f:(fun stmt ->
                              let Stmt_edge (src, dst, attrs) = stmt in
                              let src' = name_of_point src in
                              let dst' = name_of_point (List.nth_exn dst 0) in
                              try
                                let src'' = List.Assoc.find_exn nodes
                                              ~equal:String.equal src' in
                                let dst'' = List.Assoc.find_exn nodes
                                              ~equal:String.equal dst' in
                                  (src'', dst'')
                              with
                                  Not_found -> (src', dst'))
                   |> List.filter ~f:(fun (s, d) ->
                              let r = Str.regexp "Node*" in
                                 (not (Str.string_match r s 0) && not (Str.string_match r d 0)))
                   |> EdgeSet.of_list in
  (**
    let () = EdgeSet.iter ~f:(fun (s, d) -> Printf.printf "%s -> %s\n" s d) edges in
  *)
  (v, edges);;

(**
 * If no entrypoint is given, collect all the nodes and edges in the callgraph
 * and compare it to LLVM's call graph.
 * *)
let main entrypoint dot_file proj =
  let prog = Project.program proj in
  let cg = Program.to_graph prog in
    match entrypoint with
      None ->
        let v = CG.nodes cg in
        let e = CG.edges cg in
        let v' = v |>
                Seq.map ~f:(fun node -> node |> Tid.name |> tail) |>
                Seq.to_list |>
                String.Set.of_list in
        let e' = e |>
                Seq.map ~f:(fun edge ->
                        let name n = n |> Tid.name |> tail in
                        (name (CG.Edge.src edge), name (CG.Edge.dst edge))) |>
                Seq.to_list |>
                EdgeSet.of_list in
        (**
          let () = EdgeSet.iter ~f:(fun (x, y) -> printf "%s -> %s\n" x y) e' in
        *)
        (match dot_file with
           None -> printf "Nothing to compare to!\n"
         | Some file ->
           let bap = (v', e') in
           let llvm = process_dot_file file in
           let bothv = String.Set.inter (fst bap) (fst llvm) in
           let bapv = String.Set.diff (fst bap) (fst llvm) in
           let llvmv = String.Set.diff (fst llvm) (fst bap) in
           let bothe = EdgeSet.inter (snd bap) (snd llvm) in
           let bape = EdgeSet.diff (snd bap) (snd llvm) in
           let llvme = EdgeSet.diff (snd llvm) (snd bap) in
             printf "Nodes:\n%-5s %8d\n%-5s %8d\n%-5s %8d\nEdges:\n%-5s %8d\n%-5s %8d\n%-5s %8d\n"
                    "Both" (String.Set.length bothv)
                    "BAP" (String.Set.length bapv)
                    "LLVM" (String.Set.length llvmv)
                    "Both" (EdgeSet.length bothe)
                    "BAP" (EdgeSet.length bape)
                    "LLVM" (EdgeSet.length llvme))
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
  let dot_file = param (some string) "dotfile"
                     ~doc:"A dot file that contains LLVM's call graph."
  let () = when_ready (fun {get=(!!)} ->
      Project.register_pass' (main !!entrypoint !!dot_file))

  let () = manpage [
      `S "DESCRIPTION";
      `P
        "Collects all functions reachable from the $(v,entrypoint) parameter."
  ]
end
