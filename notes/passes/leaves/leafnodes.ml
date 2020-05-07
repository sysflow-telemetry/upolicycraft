open Core_kernel
open Bap.Std
open Graphlib.Std
open Format
open Pervasives
open Yojson

include Self()

(**
  A pass that finds all functions reachable from an entrypoint through direct
  calls.
*)

let entrypoint = "main"

module SS = Set.Make(String);;

(**
  Find all edges in a call graph, and then search for
  those reachable from the entrypoint.
*)

let reach = object
  inherit [string * (SS.t)] Term.visitor
  method! enter_sub sub (pred, visited) =
    let name = Sub.name sub in
    (**
    let () = printf "name: %s\n" name in
    *)
      (name, (SS.add visited name))
  method! enter_jmp j (pred, visited) =
     let kind = Jmp.kind j in
      match kind with
      | Call call ->
      let label = Call.target call in
        (match label with
        | Direct t ->
        let name = Tid.name t in
          (name, (SS.remove visited pred))
        | Indirect _ ->
        (*
        let () = printf "Indirect Call!\n" in
        *)
          (pred, visited))
      | _ -> (pred, visited)
end

(**
An example of how to lookup a term.

let Ok tid = Tid.from_string "@main" in
let opt = Program.lookup sub_t prog tid in
  match opt with
    | None -> printf "Could not find main!"
    | Some t ->
let calls = counter#run t [] in
printf "Ran a pass on main!"
*)

let main entrypoint proj =
  let prog = (Project.program proj) in
  let e = entrypoint in
  let _, leaves = reach#run prog ("", SS.empty) in
  let leaves = SS.to_list leaves in
  let json = `List (List.map ~f:(fun x -> `String x) leaves) in
  let output = Yojson.Basic.pretty_to_string json in
    printf "%s\n" output

module Cmdline = struct
  open Config
  let entrypoint = param (some string) "entrypoint"
                     ~doc:"Name of the function in the binary to begin searching."
  let () = when_ready (fun {get=(!!)} ->
      Project.register_pass' (main !!entrypoint))

  let () = manpage [
      `S "DESCRIPTION";
      `P
        "Collects all functions reachable from the $(v,entrypoint) parameter."
  ]
end
