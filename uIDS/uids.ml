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

type fd = int

type operation =
  | Clone of string list
  | Exec of string list
  | Open of string
  | Bind of fd * int
  | Accept of fd
  | Read of fd
  | Write of fd

let string_of_operation op =
  match op with
    Clone argv -> "clone (" ^ (String.concat ~sep:"," argv) ^ ")"
  | Exec argv -> "exec (" ^ (String.concat ~sep:"," argv)  ^ ")"
  | Open path -> "open (" ^ path ^ ")"
  | Bind (fd , port) ->
    let fd'  = Printf.sprintf "%d" fd in
    let port' = Printf.sprintf "%d" port in
    "bind ( " ^ fd' ^ ", " ^ port' ^ ")"
  | Accept fd ->
    let fd' = Printf.sprintf "%d" fd in
    "accept(" ^ fd' ^ ")"
  | Read fd ->
    let fd' = Printf.sprintf "%d" fd in
    "read (" ^ fd' ^ ")"
  | Write fd ->
    let fd' = Printf.sprintf "%d" fd in
    "write (" ^ fd' ^ ")"

type tree = {
  node : operation list;
  leaves : tree list
}

let string_of_tree tree =
  let rec loop depth tree =
    let {node;leaves} = tree in
    let sops = node |> List.map ~f:string_of_operation |> String.concat ~sep:"->" in
    let sleaves = leaves |> List.map ~f:(loop (succ depth)) |> String.concat ~sep:"\n" in
    sprintf "depth %d\n%*s\n%*s" depth depth sops depth sleaves in
  loop 0 tree

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
  operations : operation list;
  symbols : Symtab.fn list;
  history : tree list;
  labels : (string, Var.t) Hashtbl.t
}

let add_operation op state =
  let x :: xs = state.history in
  let x' = (add_operation op x) in
  { state with history = (x' :: xs); operations = op :: state.operations }

let graph_of_state nodes edges =
  let nodes' = nodes |> List.map ~f:(fun (node, label) ->
      Printf.sprintf "%s [label=\"%s\"];" node label)
               |> String.concat ~sep:"\n" in
  let edges' = edges |> List.map ~f:(fun (src, dst, label) ->
      Printf.sprintf "%s -> %s [label=\"%s\"];" src dst label)
               |> String.concat ~sep:"\n" in
  Printf.printf "digraph D {\n";
  Printf.printf "%s\n" nodes';
  Printf.printf "%s\n" edges';
  Printf.printf "}\n"

let render_state args state =
  let {history;operations} = state in
  let next_id id = succ id in
  let render_history history nodes edges id =
    let seq = history |>
              List.rev in
    List.fold_left ~init:(nodes, edges, id) ~f:(fun (nodes, edges, id) op ->
        match op with
          Clone argv ->
          let label = String.concat ~sep:"," argv in
          let node = Printf.sprintf "node_%d" id in
          (* Create an edge between the parent and this child *)
          let edges' = match nodes with
              [] -> edges
            |  (n, _) :: ns -> (n, node, "clone") :: edges in
          (((node, label) :: nodes), edges', (next_id id))
        | Exec argv ->
          let label = String.concat ~sep:"," argv in
          let node = Printf.sprintf "node_%d" id in
          (* Create an edge between the parent and this child *)
          let edges' = match nodes with
              [] -> edges
            | (n, _) :: ns -> (n, node, "exec") :: edges in
          (((node, label) :: nodes), edges', (next_id id))
        | Bind (fd, port) ->
          let label = Printf.sprintf "%x %d" fd port in
          let node = Printf.sprintf "node_%d" id in
          let edges' = match nodes with
              [] -> edges
            | (n, _) :: ns -> (n, node, "bind") :: edges in
          (((node, label) :: nodes), edges', (next_id id))
        | Accept fd ->
          let label = Printf.sprintf "%x" fd in
          let node = Printf.sprintf "node_%d" id in
          let edges' = match nodes with
              [] -> edges
            | (n, _) :: ns -> (n, node, "accept") :: edges in
          (((node, label) :: nodes), edges', (next_id id))
        | Read fd ->
          let label = Printf.sprintf "%x" fd in
          let node = Printf.sprintf "node_%d" id in
          let edges' = match nodes with
              [] -> edges
            | (n, _) :: ns -> (n, node, "read") :: edges in
          (((node, label) :: nodes), edges', (next_id id))
        | Write fd ->
          let label = Printf.sprintf "%x" fd in
          let node = Printf.sprintf "node_%d" id in
          let edges' = match nodes with
              [] -> edges
            | (n, _) :: ns -> (n, node, "write") :: edges in
          (((node, label) :: nodes), edges', (next_id id))) seq in
  render_history operations [("node_0", (String.concat ~sep:"," args))] [] 1

let state = Primus.Machine.State.declare
    ~name:"uids"
    ~uuid:"ba442400-63dd-11ea-a41a-06f59637065f"
    (fun _ -> { operations = []; symbols = []; history = []; labels = Hashtbl.create (module String) })

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
        let msg = Primus.Exn.to_string exn in
        let () = info "    %s" msg in
        Value.of_bool (false))

  let dump_memory addr steps =
    let () = info "dump memory" in
    let rec loop n addr =
      if n = steps then
        Machine.return()
      else
        allow_all_memory_access (Memory.get addr) >>= fun v ->
        let x = v |> Value.to_word |> Bitvector.to_int_exn in
        let () = info "  %x" x in
        loop (succ n) (Bitvector.succ addr) in
    loop 0 addr

  let read_number addr width =
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

  let record_function func =
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
    | "open64" ->
      let () = info "model open:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      (v |> Value.to_word |> string_of_addr) >>= fun path ->
      let () = info " RDI: %s" path in
      Machine.Global.update state ~f:(fun state' ->
          let op = (Open path) in
          (add_operation op state'))
    | "bind" ->
      let () = info "model bind:" in
      let rdi = (Var.create "RDI" reg64_t) in
      let rsi = (Var.create "RSI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (Env.get rsi) >>= fun u ->
      let sockaddr = (u |> Value.to_word) in
      let portaddr = Bitvector.nsucc sockaddr 0x2 in
      read_number portaddr 2 >>= fun v ->
      let port = Bitvector.to_int_exn v in
      let () = info " port %d" port in
      Machine.Global.update state ~f:(fun state' ->
          let op = Bind (fd, port) in
          (add_operation op state'))
    | "accept" ->
      let () = info "model accept:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Global.update state ~f:(fun state' ->
          let op = Accept fd in
          (add_operation op state'))
    | "recvmsg" ->
      let () = info "model read:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Global.update state ~f:(fun state' ->
          let op = Read fd in
          (add_operation op state'))
    | "sendmsg" ->
      let () = info "model write:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Global.update state ~f:(fun state' ->
          let op = Write fd in
          (add_operation op state'))
    | _ ->
      let () = info "called %s" func in
      Machine.return ()

  let record_stmt stmt = Machine.current () >>= fun pid ->
    let s = Stmt.to_string stmt in
    let () = info "record statement: %s" s in
    Machine.return()

  (** This is just for debugging:
      Primus maintains the value of all the variables in the Env.module. *)
  let record_written (x, v) =
    let name = Var.to_string x in
    let () = info "Variable %s <- %s" name (Value.to_string v) in
    let start = String.get name 0 in
    if start = '#' then
      Machine.Global.update state ~f:(fun state' ->
          let () = Hashtbl.set state'.labels ~key:name ~data:x in
          state'
        )
    else
      Machine.return()

  let record_jmp j = Machine.current () >>= fun pid ->
    match (Jmp.kind j) with
      Call c ->
      let label = c |> Call.target |> Label.to_string in
      let prefix = String.get label 0 in
      if prefix = '@' then
        let func = String.drop_prefix label 1 in
        record_function func
      else
        let () = info "Indirect function call:" in
        let prefix = String.get label 0 in
        if prefix = '#' then
          Machine.Global.get state >>= fun state' ->
          let {labels} = state' in
          let var = Hashtbl.find_exn labels label in
          (Env.get var) >>= fun v ->
          let target = (v |> Value.to_word |> Bitvector.to_int_exn) in
          let {symbols} = state' in
          let matched = symbols |> List.filter ~f:(fun (name, block, cfg) ->
              let addr = block |> Block.addr |> Bitvector.to_int_exn in
              let () = info "  found %s %x" name addr in
              addr = target) |> List.map ~f:(fun (name, block, cfg) ->
              name
            ) in
          if (List.length matched) > 0 then
            let f = List.nth_exn matched 0 in
            let () = info "  match %s" f in
            record_function f
          else
            let () = info "  target %x" target in
            Machine.return()
        else
          Machine.return()

    | Goto label ->
      let label' = Label.to_string label in
      let () = info "goto label %s" label' in
      Machine.return ()
    | _ ->
      let () = info "    Different kind of jump" in
      Machine.return()

  let push_context blk =
    let () = info "entering block!" in
    let tree = (empty_tree) in
    Machine.Global.update state ~f:(fun state' ->
        {state' with history = tree :: state'.history})

  let pop_context blk =
    let () = info "leaving block!" in
    Machine.Global.update state ~f:(fun state' ->
        let {history} = state' in
        match history with
        | x :: x' :: xs ->
          if is_empty x then
            {state' with history = x' :: xs}
          else
            {state' with history = (add_leaf x x') :: xs}
        | x :: xs ->
          if is_empty x then
            {state' with history = xs}
          else
            state'
        | _ -> state')

  let record_model () =
    Machine.current () >>= fun pid ->
    let () = info "Machine ending!" in
    if Machine.global = pid then
      let () = info "Global Machine ending." in
      Machine.args >>= fun args ->
      Machine.Global.get state >>= fun state' ->
      let args' = (Array.to_list args) in
      let (nodes, edges, id) = render_state args' state' in
      let () = graph_of_state nodes edges in
      Machine.return()
    else
      Machine.return()

  let record_finished () =
    Machine.current () >>= fun pid ->
    let () = info "machine finished!" in
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
    setup_tracing () >>= fun () ->
    Machine.get () >>= fun proj ->
    let symtab = Project.symbols proj in
    let symtabs = (symtab |> Symtab.to_sequence |> Seq.to_list) in
    Machine.Global.update state ~f:(fun s ->
        { symbols = symtabs; operations = []; history = []; labels = Hashtbl.create (module String) }
      )
end

let main {Config.get=(!)} =
  let open Param in
  if !model then
    Primus.Machine.add_component (module Monitor)

let () =
  Config.when_ready (fun conf ->
      main conf)
