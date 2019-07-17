open Core_kernel.Std
open Bap.Std
open Graphlib.Std
open Format
open Pervasives

include Self()

(** Count the number of syscalls given in a run of instructions. *)
let count_syscalls insns =
  Seq.filter insns (fun p ->
    let bil = (Insn.bil (snd p)) in
      (List.length (List.filter bil (fun stmt -> match stmt with
                                         | Bil.Special s -> s = "syscall"
                                         | _ -> false)) > 0))

(** Collect the set of syscalls given in a run of instructions. *)
let collect_syscalls insns =
  let step = (fun (rax, syscalls) stmt -> match stmt with
               | Bil.Move (v, e) ->
                  let name = Var.name v in
                  let () = printf "%s\n" name in
                    (match name with
                      | "RAX" ->
                        let imm = Exp.eval e in
                          (match imm with
                            | Bil.Imm word ->
                              (match Bitvector.to_int word with
                                | Error _ -> (rax, syscalls)
                                | Ok i -> (i, syscalls))
                            | Bil.Mem storage -> let () = printf "Mem!\n"  in
                              (rax, syscalls)
                            | Bil.Bot -> (rax, syscalls))
                      | _ -> (rax, syscalls))
                | Bil.Special s ->
                  if s = "syscall" then
                    (rax, (Set.add syscalls rax))
                  else
                    (rax, syscalls)
                | _ -> (rax, syscalls)) in
  let acc = (fun res (mem, i) ->
    let bil = (Insn.bil i) in
      match bil with
        | [] -> res
        | stmt :: _ -> (step res stmt)) in
  Seq.fold ~init:(0, (Set.empty Int.comparator)) ~f:acc insns
      |> snd |> Set.to_list |> Seq.of_list

let syscalls = object
  inherit [(int * int) list * (string * int) list] Term.visitor
  method! enter_sub _ _ (ss) = jmps,total+1
  method! enter_jmp _ (ss) = jmps+1,total
end

let main proj =
  let () = printf "Creating project" in
  let symtab = Project.symbols proj in
  let () = printf "System Calls:\n" in
  Seq.iter (Symtab.to_sequence symtab) (fun (name, block, _) ->
    let () = printf "%4s %s\n" "Function" name in
    let mem = Block.memory block in
    let disasm = Disasm.of_mem (Project.arch proj) mem in
      match disasm with
        | Error er -> printf "<failed to disassemble memory region>"
        | Ok dis ->
          Disasm.insns dis
            |> collect_syscalls |> Seq.iter ~f:(fun s -> printf "%6d\n" s))

let () = Project.register_pass' main
