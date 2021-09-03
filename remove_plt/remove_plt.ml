(**
  Copyright (C) 2021 IBM Corporation.

  Authors:
  William Blair <wdblair@ibm.com>
*)
open Core_kernel
open Bap.Std
open Format

include Self()

(** Procedure Linkage Table (PLT) remover.
    Remove all calls to the plt.
*)
module PLTRemover = struct

  let sub =
    Term.map blk_t ~f:(fun b ->
    let () = info "Considering block!" in
    Term.filter jmp_t b ~f:(fun j ->
      let () = info "Considering jump!" in
      match Jmp.kind j with
        Call c ->
          let label = c |> Call.target |> Label.to_string in
          let () = info "Considering %s!" label in
          let prefix = label.[0] in
          if phys_equal prefix '@' then
            let func = String.drop_prefix label 1 in
            if phys_equal func ".plt" then
              false
            else
              true
          else
             true
      | _ -> true
    ))

  let prog =
    Term.map sub_t ~f:sub

  let proj =
    Project.map_program ~f:prog
end

let main proj =
  info "translating the program to remove calls to the PLT!";
  PLTRemover.proj proj

open Config;;

let () = manpage [
  `S "DESCRIPTION";
  `P "Ensures that no call is made to the plt.";
];;


let () = when_ready (fun _ ->
    Project.register_pass ~runonce:true main)
