open Core_kernel.Std
open Bap.Std
open Graphlib.Std
open Format
open List
open Pervasives

include Self()

let syscalls insns =
  Seq.filter insns (fun p ->
    let bil = (Insn.bil (snd p)) in
      (List.length (List.filter bil (fun stmt -> match stmt with
                                         | Special s -> s = "syscall"
                                         | _ -> false)) > 0))

let syscalls1 insns =
  let acc = (fun (rax, syscalls) (mem, i) ->
    let bil = (Insn.bil i) in
    let stmt :: [] = bil in
      match stmt with
        |  Move (v, e) ->
          let name = Var.name v in
            (match name with
              | "rax" ->
                let imm = Exp.eval e in
                  (match imm with
                    | Imm word ->
                      match Bitvector.to_int word with
                        | Error _ -> (rax, syscalls)
                        | Ok i -> (i, syscalls)
                    | _ -> (rax, syscalls))
              | _ -> (rax, syscalls))
        | Special s ->
          if s = "syscall" then
            (rax, (Set.add syscalls rax))
          else
            (rax, syscalls)) in
  let xs = Seq.to_list insns in
  List.fold_left xs (0, (Set.empty Int.comparator)) acc

let main proj =
  let disasm = Project.disasm proj in
  let insns = Disasm.insns disasm in
  let symtab = Project.symbols proj in
  Seq.iter (Symtab.to_sequence symtab) (fun (name, block, _) ->
    let () = printf "Function %s\n" name in
    let mem = Block.memory block in
    let disasm = Disasm.of_mem (Project.arch proj) mem in
      match disasm with
        | Error er -> printf "<failed to disassemble memory region>"
        | Ok dis ->
        let n = Disasm.insns dis |> syscalls |> Seq.length in
          printf "Contains %d syscalls\n" n)

let () = Project.register_pass' main
