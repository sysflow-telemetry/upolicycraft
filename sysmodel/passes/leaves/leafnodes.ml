open Core_kernel.Std
open Bap.Std
open Graphlib.Std
open Format
open Pervasives

include Self()

(**
  A pass that finds all functions reachable from an entrypoint through direct
  calls.
*)

let entrypoint = "main"

module SS = Set.Make(String);;

(**
  Find all edges in a call graph, and then search for those reachable from the entrypoint.
*)

let reach = object
  inherit [string * (string * string) list] Term.visitor
  method! enter_sub sub (pred, edges) =
    let name = Sub.name sub in
    let () = printf "name: %s\n" name in
      (name, edges)
  method! enter_jmp j x =
     let (pred, edges) = x in
     let kind = Jmp.kind j in
      match kind with
      | Call call ->
      let label = Call.target call in
        (match label with
        | Direct t ->
        let name = Tid.name t in
          (name, (pred, name) :: edges)
        | Indirect _ ->
        let () = printf "Indirect Call!\n" in
          x)
      | _ -> x
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

let main proj =
  let prog = (Project.program proj) in
  let (_, edges) = reach#run prog ("", [])  in
    List.iter edges (fun (s, t) -> printf "Call %s -> %s\n" s t)

let () = Project.register_pass' main
