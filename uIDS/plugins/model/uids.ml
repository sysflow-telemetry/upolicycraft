open Core_kernel
open Bap.Std
open Bap_primus.Std
open Bap_future.Std
open Monads.Std
open Format
open Graphlib.Std
open Yojson

module Id = Monad.State.Multi.Id

include Self()

module Param = struct
  open Config;;
  manpage [
    `S "DESCRIPTION";
    `P "Derive a process model for a binary."
  ]

  let model = flag "model"
      ~doc:
        "Derive a process model for a binary."

end

let state = Primus.Machine.State.declare
    ~name:"uids"
    ~uuid:"ba442400-63dd-11ea-a41a-06f59637065f"
    (fun _ -> 0)

let address_of_pos out addr =
  match Bitvector.to_int addr with
    Error s ->
    (** fprintf out "Error converting int!\n"; *)
    -1
  | Ok v ->
    (** fprintf out "SYSFLOW: Cursor at %#08x\n" v; *)
    v

let find_value regs out e = (object
  inherit [word] Exp.visitor
  method! enter_int word v =
    let () = info "Found word %s" (Bitvector.to_string v) in
    v
  method! enter_var reg v =
    let rname = Var.name reg in
    (**
       let () = fprintf out "Lookup var: %s\n" rname in
    *)
    match List.Assoc.find regs ~equal:String.equal rname with
      None -> v
    | Some v' -> v'
end)#visit_exp e (Bitvector.of_bool false)

let out = std_formatter

module Monitor(Machine : Primus.Machine.S) = struct
  module Eval = Primus.Interpreter.Make(Machine)
  module Env = Primus.Interpreter.Make(Machine)
  module Memory = Primus.Memory.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  let record_pos p = Machine.current () >>= fun pid ->
    match (Primus.Pos.get address p) with
      None -> Machine.return ()
    | Some addr ->
      let a = address_of_pos out addr in
      let () = info "visiting %x\n" a in
      Machine.return ()

  let allow_all_memory_access access =
    Machine.catch access (function exn ->
      let () = info "Caught exception!" in
      Value.of_bool (false))

  let string_of_addr addr =
    let rec loop addr cs =
    (allow_all_memory_access (Memory.get addr)) >>= fun v ->
      let x = (v |> Value.to_word |> Bitvector.to_int_exn) in
       if x = 0 then
         let s = (cs |> List.rev |> String.of_char_list) in
         let () = info "  %s" s in
         Machine.return ()
       else
         let c = Char.of_int_exn x in
         loop (Bitvector.succ addr) (c :: cs) in
    loop addr []

  let record_written (x, v) =
    let () = info "Variable %s <- %s" (Var.to_string x) (Value.to_string v) in
    let addr = (Value.to_word v) in
    string_of_addr addr

  let record_def t =
    let reg = t |> Def.lhs |> Var.name in
    if reg = "RDI" then
      let () = info "Setting %s:" reg in
      let exp = t |> Def.rhs |> Exp.normalize |> Exp.simpl in
      let addr = find_value [] out exp in
      let () = info "Looking for value: %s" (Bitvector.to_string addr) in
      Machine.return ()
      (**
      (allow_all_memory_access ) >>= fun v ->
        let () = info "    %s: %d" (Exp.to_string exp) (v |> Primus.Value.to_word |> Bitvector.to_int_exn) in
        Machine.return () *)
    else
      Machine.return ()

  let record_jmp j = Machine.current () >>= fun pid ->
    match (Jmp.kind j) with
      Call c ->
        let label = c |> Call.target |> Label.to_string in
        let prefix = String.get label 0 in
        if prefix = '@' then
          (** Inspect RDI *)
          (** How can we infer the arguments to this function? *)
          Machine.return()
        else
          Machine.return()
      | _ ->
        let () = info "    Different kind of jump" in
        Machine.return()
  let setup_tracing () =
    Machine.List.sequence [
      Primus.Interpreter.written >>> record_written;
      Primus.Interpreter.enter_pos >>> record_pos;
      Primus.Interpreter.enter_def >>> record_def;
      Primus.Interpreter.enter_jmp >>> record_jmp;
    ]

  let init () =
    setup_tracing ()

end

let main {Config.get=(!)} =
  let open Param in
  if !model then
    Primus.Machine.add_component (module Monitor)

let () =
  Config.when_ready (fun conf ->
      main conf)
