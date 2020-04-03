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

type operation =
  | Clone of string list
  | Exec of string list

let string_of_operation op =
  match op with
    Clone argv -> "clone (" ^ (String.concat ~sep:"," argv) ^ ")"
  | Exec argv  -> "exec (" ^ (String.concat ~sep:"," argv)  ^ ")"

type tree = {
  node : operation list;
  leaves : tree list
}

let empty_tree =
  {node= []; leaves= []}

let is_empty tree =
  let {node} = tree in
  match node with
    [] -> true
  | _ -> false

let add_operation op tree =
  let {node} = tree in
  {tree with node = op :: node}

let add_leaf leaf tree =
  let {leaves} = tree in
  {tree with leaves = leaf :: leaves}

type state = {
  history : tree list;
}

let add_operation op state =
  let x :: xs = state.history in
  let x' = (add_operation op x) in
  { history = (x' :: xs) }

let print_state state =
  let {history} = state in
  let rec print_history history =
    match history with
      [] -> ()
    | h :: hs ->
      let {node;leaves} = h in
      let seq = node |>
                List.rev |>
                List.map ~f:string_of_operation |>
                String.concat ~sep:"->" in
      let () = printf "%s\n" seq in
      let () = print_history leaves in
      print_history hs in
  print_history history

let state = Primus.Machine.State.declare
    ~name:"uids"
    ~uuid:"ba442400-63dd-11ea-a41a-06f59637065f"
    (fun _ -> {history = []})

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
        match func with
          "fork" ->
          let () = info "model clone:" in
          Machine.args >>= fun args ->
          Machine.Global.update state ~f:(fun state' ->
              let op = Clone (Array.to_list args) in
              add_operation op state')
        | "execv" ->
          let () = info "model execv:" in
          let rdi = (Var.create "RDI" reg64_t) in
          let rsi = (Var.create "RSI" reg64_t) in
          (Env.get rdi) >>= fun v ->
          (v |> Value.to_word |> string_of_addr) >>= fun s ->
          (Env.get rsi) >>= fun u ->
          (u |> Value.to_word |> strings_of_addr) >>= fun ss ->
          let path = s in
          let argv = (String.concat ~sep:"," ss) in
          let () = info " RDI: %s" path in
          let () = info " RSI: %s" argv in
          Machine.Global.update state ~f:(fun state' ->
              let op = (Exec [path; argv]) in
              (add_operation op state'))
        | _ ->
          let () = info "called %s" func in
          Machine.return ()
      else
        Machine.return ()
    | _ ->
      let () = info "    Different kind of jump" in
      Machine.return()

  let push_context blk =
    let tree = (empty_tree) in
    Machine.Global.update state ~f:(fun state' ->
        {history = tree :: state'.history})

  let pop_context blk =
    Machine.Global.update state ~f:(fun state' ->
        let {history} = state' in
        match history with
        | x :: x' :: xs ->
          if is_empty x then
            {history = x' :: xs}
          else
            {history = (add_leaf x x') :: xs}
        | x :: xs ->
          if is_empty x then
            {history = xs}
          else
            state'
        | _ -> state')

  let record_model () =
    Machine.current () >>= fun pid ->
    let () = info "Machine ending!" in
    if Machine.global = pid then
      let () = info "Global Machine ending." in
      Machine.Global.get state >>= fun state' ->
      let () = print_state state' in
      Machine.return()
    else
      Machine.return()

  let setup_tracing () =
    Machine.List.sequence [
      Primus.Interpreter.written >>> record_written;
      Primus.Interpreter.enter_pos >>> record_pos;
      Primus.Interpreter.enter_jmp >>> record_jmp;
      Primus.Interpreter.enter_blk >>> push_context;
      Primus.Interpreter.leave_blk >>> pop_context;
      Primus.Machine.finished >>> record_model;
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
