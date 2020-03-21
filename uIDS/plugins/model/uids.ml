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
    Error s -> -1
  | Ok v -> v

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
        let () = info "Error reading memory!" in
        Value.of_bool (false))

  (** Read an address stored at addr *)
  let read_address addr =
    let width = 8 in
    let rec loop n addr p =
      if n = width then
        p
      else
        let cont = (allow_all_memory_access (Memory.get addr)) >>= fun v ->
          p >>= fun x ->
          let v' = v |>
                   Value.to_word |>
                   Bitvector.to_int_exn |>
                   Bitvector.of_int ~width:64 in
          let () = info "  %s" (Bitvector.to_string v') in
          let shift = (Bitvector.of_int 64 (n * 8)) in
          let next = (Bitvector.logor (Bitvector.lshift v' shift) x) in
          let () = info "  intermediate: %s" (Bitvector.to_string next) in
          Machine.return(next) in
        loop (succ n) (Bitvector.succ addr) cont in
    loop 0 addr (Machine.return(Bitvector.of_int 64 0))

  let string_of_addr addr =
    let rec loop addr cs =
      let () = info "Fetching %x" (Bitvector.to_int_exn addr) in
      (allow_all_memory_access (Memory.get addr)) >>= fun v ->
      let x = (v |> Value.to_word |> Bitvector.to_int_exn) in
      if x = 0 then
        let s = (cs |> List.rev |> String.of_char_list) in
        let () = info "  %s" s in
        Machine.return (s)
      else
        let c = Char.of_int_exn x in
        loop (Bitvector.succ addr)  (c :: cs) in
    loop addr []

  let strings_of_addr addr =
    let () = info "Finding strings at %x" (Bitvector.to_int_exn addr) in
    let rec loop addr strings =
      read_address addr >>= fun v ->
      let () = info "Read address %s" (Bitvector.to_string v) in
      if v = (Bitvector.zero 64) then
        strings
      else
        let cont = strings >>= fun xs ->
          (string_of_addr v) >>= fun x ->
          Machine.return(x :: xs) in
        loop (Bitvector.add addr (Bitvector.of_int ~width:64 8)) cont in
    loop addr (Machine.return([]))

  (** This is just for debugging:
      Primus maintains the value of all the variables in the Env module. *)
  let record_written (x, v) =
    let () = info "Variable %s <- %s" (Var.to_string x) (Value.to_string v) in
    Machine.return()

  let record_jmp j = Machine.current () >>= fun pid ->
    match (Jmp.kind j) with
      Call c ->
      let label = c |> Call.target |> Label.to_string in
      let prefix = String.get label 0 in
      if prefix = '@' then
        let func = String.drop_prefix label 1 in
        if func = "execv" then
          let rdi = (Var.create "RDI" reg64_t) in
          let rsi = (Var.create "RSI" reg64_t) in
          (Env.get rdi) >>= fun v ->
          (v |> Value.to_word |> string_of_addr) >>= fun s ->
          (Env.get rsi) >>= fun u ->
          (u |> Value.to_word |> strings_of_addr) >>= fun ss ->
          let () = info " RDI: %s" s in
          let () = info " RSI: %s" (String.concat ~sep:"," ss) in
          Machine.return ()
        else
          Machine.return ()
      else
        Machine.return ()
    | _ ->
      let () = info "    Different kind of jump" in
      Machine.return()
  let setup_tracing () =
    Machine.List.sequence [
      Primus.Interpreter.written >>> record_written;
      Primus.Interpreter.enter_pos >>> record_pos;
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
