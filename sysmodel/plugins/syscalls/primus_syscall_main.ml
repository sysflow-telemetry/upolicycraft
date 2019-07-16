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
    `P "Identify the system calls given in a binary."
  ]

  let identify = flag "identify"
      ~doc:
        "Identify the system calls given in a binary."
end

(**
  Debugging so we know what providers we have in Primus.

let show_providers out =
  Primus.Observation.list_providers () |>
  List.iter ~f:(fun p ->
    fprintf out "Provider: %s\n" (Primus.Observation.Provider.name p))
*)

(**
 *  addrs: The addresses of syscall instructions in the binary.
 *  vals: An assoc list that stores the concrete value of RAX
 *        at each system call address.
 *  regs: An assoc list that stores the definition of all regs
 *        during the execution of the binary.
 **)
type state = {
    addrs : int list;
    vals : (int * int) list;
    regs : (string * int) list;
}

module SS = Set.Make(String);;

let address_of_pos out p =
  match (Primus.Pos.get address p) with
    None -> -1
  | Some addr -> (match Bitvector.to_int addr with
                    Error s ->
                      fprintf out "Error converting int!\n";
                      -1
                  | Ok v ->
                      fprintf out "SYSFLOW: Cursor at %#08x\n" v;
                      v)

let state = Primus.Machine.State.declare
    ~name:"primus-syscall"
    ~uuid:"c4696d2f-5d8e-42b4-a65c-4ea6269ce9d1"
    (fun _ -> {addrs = []; vals = []; regs = []})

let collect_syscalls insns =
  let detect_syscall = (fun (mem, i) ->
    let bil = (Insn.bil i) in
      match bil with
        [] -> false
      | stmt :: _ -> (match stmt with
                        Bil.Special s -> true
                      | _ -> false)) in
  let int_of_mem = (fun mem ->
    let err = mem |> Memory.min_addr |> Bitvector.to_int in
      match err with
        Error err -> -1
      | Ok i -> i) in
  Seq.filter ~f:detect_syscall insns |> Seq.map ~f:fst |> Seq.map ~f:int_of_mem

let find_value regs out e = (object
  inherit [int] Exp.visitor
  method! enter_int word v =
    match Bitvector.to_int word with
      Error s -> ~-1
    | Ok v' -> v'
  method! enter_var reg v =
    let rname = Var.name reg in
    let () = fprintf out "Lookup var: %s\n" rname in
      match List.Assoc.find regs ~equal:String.equal rname with
        None -> v
      | Some v' -> v'

end)#visit_exp e ~-1

(** SysCall Instruction: 0f 05 **)
let syscall_length = 2

let start_monitoring {Config.get=(!)} =
  let out = std_formatter in
  let module Monitor(Machine : Primus.Machine.S) = struct
    open Machine.Syntax

    let record_def t =
      Machine.Global.update state ~f:(fun {addrs;vals;regs} ->
        let lhs = Def.lhs t in
        let reg = Var.name lhs in
        let exp = t |> Def.rhs |> Exp.normalize |> Exp.simpl in
        let v = find_value regs out exp in
        let () = fprintf out "SYSFLOW: %s := %d\n" reg v in
        let regs' = List.Assoc.add regs ~equal:String.equal reg v in
        {addrs=addrs; vals=vals; regs=regs'}
      )

    let record_pos p = Machine.Global.update state ~f:(fun {addrs;vals;regs} ->
      let a = address_of_pos out p in
      let hit sa =
        let () = fprintf out "Checking %x = %x\n" sa a in
        let target = sa + syscall_length in
          a = target || a = target+1 in
      let rax sa =
        (sa, (List.Assoc.find_exn regs ~equal:String.equal "RAX")) in
      let () = fprintf out "Visiting addr at %x\n" a in
      let opt = addrs |> List.filter ~f:hit
                                |> List.map ~f:rax |> List.hd in
      let vals' = match opt with
                    None -> vals
                  | Some (addr, syscall) ->
                     List.Assoc.add vals ~equal:(=) addr syscall in
      {addrs=addrs; vals=vals'; regs=regs})

    let print_syscalls () =
      Machine.Global.get state >>| fun {addrs;vals} ->
        let findcall addr =
          match List.Assoc.find vals ~equal:(=) addr with
            None -> ()
          | Some rax -> fprintf out "%x: %d\n" addr rax in
        fprintf out "Hit System Calls\n";
        List.iter ~f:findcall addrs

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
          let symtabs = (Symtab.to_sequence symtab) in
          let () = fprintf out "System Calls:\n" in
          let find_syscalls sc symtab =
            let (name, block, _) = symtab in
            let () = fprintf out "%4s %s\n" "Function" name in
            let mem = Block.memory block in
            let arch = Project.arch proj in
              match Disasm.of_mem arch mem with
                Error err ->
                  fprintf out "<failed to disassemble memory region>";
                  sc
              | Ok dis -> dis |> Disasm.insns |>
                          collect_syscalls |> Seq.append sc in
          let syscalls = Seq.fold ~init:Seq.empty ~f:find_syscalls symtabs in
          let () = Seq.iter ~f:(fun mem -> fprintf out "%8x\n" mem) syscalls in
            Machine.Global.update state ~f:(fun s ->
              {addrs=Seq.to_list syscalls; vals=[]; regs =[]})
  end in
  Primus.Machine.add_component (module Monitor)

let () = Config.when_ready start_monitoring
