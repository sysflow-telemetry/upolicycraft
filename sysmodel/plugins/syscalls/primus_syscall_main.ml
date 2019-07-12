open Core_kernel
open Bap.Std
open Bap_primus.Std
open Bap_future.Std
open Monads.Std
open Format

include Self()

module Param = struct
  open Config;;
  manpage [
    `S "DESCRIPTION";
    `P "Identify the System Calls given in a Binary."
  ]

  let identify = param (bool) "identify"
      ~doc:
        "Identify the system calls given in a binary."
end

(**
  Debugging so we know what providers we have in Primus.

let show_providers out =
  Primus.Observation.list_providers () |>
  List.iter ~f:(fun p -> fprintf out "Provider: %s\n" (Primus.Observation.Provider.name p))
*)

type state = {
    syscall_addresses : int list;
    syscall_values : int list;
}

module SS = Set.Make(String);;

let address_of_pos out p =
  match (Primus.Pos.get address p) with
    | None -> -1
    | Some addr -> let opt = Bitvector.to_int addr in
      (match opt with
        | Ok v ->
            fprintf out "SYSFLOW: Cursor at %#08x\n" v;
            v
        | Error s ->
            fprintf out "Error converting int!\n";
            -1)


let state = Primus.Machine.State.declare
    ~name:"primus-syscall"
    ~uuid:"c4696d2f-5d8e-42b4-a65c-4ea6269ce9d1"
    (fun _ -> {syscall_addresses = []; syscall_values = []})

let collect_syscalls insns =
  let detect_syscall = (fun (mem, i) ->
    let bil = (Insn.bil i) in
      match bil with
        | [] -> false
        | stmt :: _ -> (match stmt with
                          | Bil.Special s -> true
                          | _ -> false)) in
  Seq.filter ~f:detect_syscall insns |> Seq.map ~f:fst |>
    Seq.map ~f:(fun mem ->
                  let err = mem |> Memory.min_addr |> Bitvector.to_int in
                    match err with
                      | Ok i -> i
                      | Error err -> -1)

(** Instructions: 0f 05 *)
let syscall_length = 2

let start_monitoring {Config.get=(!)} =
  let out = std_formatter in
  let module Monitor(Machine : Primus.Machine.S) = struct
    open Machine.Syntax

    let record_def t =
      let lhs = Def.lhs t in
      let reg = Var.name lhs in
      let value = t |> Def.rhs |> Exp.normalize |> Exp.simpl |> Exp.to_string in
      let () = fprintf out "SYSFLOW: %s := %s\n" reg value in
      Machine.return()

    let record_pos p =
      Machine.Global.update state ~f:(fun {syscall_addresses;syscall_values} ->
        let a = address_of_pos out p in
        let () = fprintf out "Visiting addr at %x\n" a in
        let hits = List.filter ~f:(fun sa ->
                let () = fprintf out "Checking %x = %x\n" sa a in
                a = (sa + syscall_length)) syscall_addresses in
        {syscall_addresses=syscall_addresses; syscall_values= List.append hits syscall_values})

    let print_syscalls () =
      Machine.Global.get state >>| fun {syscall_values} ->
        fprintf out "Hit System Calls\n";
        List.iter ~f:(fun s -> fprintf out "%x\n" s) syscall_values

    let setup_tracing () =
      Machine.List.sequence [
          Primus.Interpreter.enter_def >>> record_def;
          Primus.Interpreter.enter_pos >>> record_pos;
          Primus.Machine.finished >>> print_syscalls;
      ]

    let init () =
      setup_tracing () >>= fun () ->
        Machine.get () >>= fun proj ->
                let symtab = Project.symbols proj in
                let () = fprintf out "System Calls:\n" in
                let syscalls = Seq.fold ~init:Seq.empty ~f:(fun sc (name, block, _) ->
                                    let () = fprintf out "%4s %s\n" "Function" name in
                                    let mem = Block.memory block in
                                    let disasm = Disasm.of_mem (Project.arch proj) mem in
                                      match disasm with
                                        | Error er ->
                                          fprintf out "<failed to disassemble memory region>";
                                          sc
                                        | Ok dis -> dis |> Disasm.insns |> collect_syscalls |> Seq.append sc) (Symtab.to_sequence symtab) in
                let () = Seq.iter ~f:(fun mem -> fprintf out "%8x\n" mem) syscalls in
                let () = fprintf out "Setting up state!" in
                  Machine.Global.update state ~f:(fun s ->
                    {syscall_addresses = Seq.to_list syscalls; syscall_values = []})
  end in
  Primus.Machine.add_component (module Monitor)

let () = Config.when_ready start_monitoring
