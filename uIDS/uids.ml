open Core_kernel
open Bap.Std
open Bap_primus.Std
open Bap_future.Std
open Format
open Graphlib.Std
open Monads.Std
open Regular.Std
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

  let entrypoint = param (string) "entrypoint"
      ~doc:
        "The entrypoint for the container"

  let entrypoint_args = param (string) "entrypoint-args"
      ~doc:
        "The arguments passed to the entrypoint"

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

module BehaviorGraph = Graphlib.Make(Tid)(String)

type state = {
  symbols : Symtab.fn list;
  labels : (string, Var.t) Hashtbl.t;
  nodes : (Tid.t, string) Hashtbl.t;
  root_tid : Tid.t;
  last_tid : Tid.t;
  visited : Tid.Set.t;
  halted : Id.Set.t;
  graph : BehaviorGraph.t;
}

let edge_of_operation op =
  match op with
    Clone argv -> "CLONE"
  | Exec argv -> "EXEC"
  | Open path -> "OPEN"
  | Bind (fd, port) -> "BIND"
  | Accept fd -> "ACCEPT"
  | Read fd -> "READ"
  | Write fd -> "WRITE"

let jsonify xs =
  `Assoc (List.map ~f:(fun (key, vs) ->
      let vs' = List.map ~f:(fun v -> `String v) vs in
      let vs'' = `List vs' in
      (key, vs'')) xs)

let split_argv argv =
  let exe :: args = argv in
  let exe' = exe |> String.split ~on:'/' |> List.last_exn in
  (exe', argv)

let node_of_operation op =
  let json =
    match op with
      Clone argv ->
      let (exe, args) = split_argv argv in
      jsonify [("sf.proc.exe", [exe]); ("sf.proc.args", args)]
    | Exec argv ->
      let (exe, args) = split_argv argv in
      jsonify [("sf.proc.exe", [exe]); ("sf.proc.args", args)]
    | Open path ->
      jsonify [("sf.file.path", [path])]
    | Bind (fd, port) ->
      let fd' = Printf.sprintf "%d" fd in
      let port' = Printf.sprintf "%d" port in
      jsonify [("sf.net.dport", [port'])]
    | Accept fd ->
      let fd' = Printf.sprintf "%d" fd in
      jsonify [("sf.net.dport", [fd'])]
    | Read fd ->
      let fd' = Printf.sprintf "%d" fd in
      jsonify [("sf.net.dport", [fd'])]
    | Write fd ->
      let fd' = Printf.sprintf "%d" fd in
      jsonify [("sf.net.dport", [fd'])] in
  Yojson.Basic.to_string json

let labeled label node =
  { node= node; node_label=label}

let add_operation tid op state =
  let {nodes;graph;last_tid} = state in
  let node_label = node_of_operation op in
  let edge_label = edge_of_operation op in
  let graph' = BehaviorGraph.Node.insert tid graph in
  let edge = BehaviorGraph.Edge.create last_tid tid edge_label in
  let graph'' = BehaviorGraph.Edge.insert edge graph' in
  let () = Hashtbl.set nodes ~key:tid ~data:node_label in
  { state with visited = Tid.Set.add state.visited tid; last_tid = tid; graph=graph'' }

let state = Primus.Machine.State.declare
    ~name:"uids"
    ~uuid:"ba442400-63dd-11ea-a41a-06f59637065f"
    (fun _ ->
       let root = Tid.create() in
       let behavior = Graphlib.create (module BehaviorGraph) () in
       { root_tid = root;
         last_tid = root;
         nodes = Hashtbl.create (module Tid);
         symbols = [];
         labels = Hashtbl.create (module String);
         visited = Tid.Set.empty;
         halted = Id.Set.empty;
         graph = behavior;
       })

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
    let () = info "recording position" in
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
        let x = v |> Value.to_word |> Bitvector.to_int64_exn in
        let () = info "  %s" (Int64.to_string x) in
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
      let () = info "Fetching %s" (Bitvector.to_string addr) in
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
    let () = info "Finding strings at %s" (Bitvector.to_string addr) in
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

  let record_function tid func =
    match func with
      "fork" ->
      let () = info "model clone:" in
      Machine.args >>= fun args ->
      Machine.Local.update state ~f:(fun state' ->
          let op = (Clone (Array.to_list args)) in
          add_operation tid op state')
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
      Machine.Local.update state ~f:(fun state' ->
          let op = (Exec [path; argv]) in
          (add_operation tid op state'))
    | "open64" ->
      let () = info "model open:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      (v |> Value.to_word |> string_of_addr) >>= fun path ->
      let () = info " RDI: %s" path in
      Machine.Local.update state ~f:(fun state' ->
          let op = (Open path) in
          (add_operation tid op state'))
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
      Machine.Local.update state ~f:(fun state' ->
          let op = Bind (fd, port) in
          (add_operation tid op state'))
    | "accept" ->
      let () = info "model accept:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Accept fd in
          (add_operation tid op state'))
    | "recvmsg" ->
      let () = info "model read:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Read fd in
          (add_operation tid op state'))
    | "sendmsg" ->
      let () = info "model write:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Write fd in
          (add_operation tid op state'))
    | _ ->
      let () = info "called %s" func in
      Machine.return ()

  let reschedule () =
    let last = Seq.fold ~init:None ~f:(fun _ x -> Some x) in
    Machine.Global.get state >>= fun {halted} ->
    Machine.forks () >>= fun forks ->
    let active = Seq.filter forks ~f:(fun id -> not (Set.mem halted id)) in
    match last active with
    | None ->
      info "no more pending machines";
      Machine.switch Machine.global
    | Some cid ->
      Machine.current () >>= fun pid ->
      info "uids: switch to machine %a from %a" Id.pp cid Id.pp pid;
      info "uids: killing previous machine %a" Id.pp pid;
      Machine.kill pid >>= fun () ->
      Machine.switch cid

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
    let tid = Term.tid j in
    Machine.Local.get state >>= fun {last_tid; nodes; graph; visited} ->
    let visited = Tid.Set.mem visited tid in
    if visited then
      let () = info "repeated a jump" in
      let preds = BehaviorGraph.Node.preds tid graph in
      let first = Seq.nth_exn preds 0 in
      let edge' = BehaviorGraph.Edge.create last_tid first "" in
      let graph' = BehaviorGraph.Edge.insert edge' graph in
      Machine.Global.update state ~f:(fun s ->
          let graph'' = Graphlib.union (module BehaviorGraph) s.graph graph' in
          {s with graph=graph''}
        ) >>= fun _ ->
      reschedule()
    else
      match (Jmp.kind j) with
        Call c ->
        let label = c |> Call.target |> Label.to_string in
        let prefix = String.get label 0 in
        if prefix = '@' then
          let func = String.drop_prefix label 1 in
          record_function tid func
        else
          let () = info "Indirect function call:" in
          let prefix = String.get label 0 in
          if prefix = '#' then
            Machine.Global.get state >>= fun state' ->
            let {labels} = state' in
            let var = Hashtbl.find_exn labels label in
            (Env.get var) >>= fun v ->
            let target = (v |> Value.to_word)  in
            let {symbols} = state' in
            let matched = symbols |> List.filter ~f:(fun (name, block, cfg) ->
                let addr = block |> Block.addr in
                let () = info "  found %s %s" name (Addr.to_string addr) in
                addr = target) |> List.map ~f:(fun (name, block, cfg) ->
                name
              ) in
            if (List.length matched) > 0 then
              let f = List.nth_exn matched 0 in
              let () = info "  match %s" f in
              record_function tid f
            else
              let () = info "  target %s" (Addr.to_string target) in
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

  let export_model root nodes graph =
    let ns = BehaviorGraph.nodes graph in
    let es = BehaviorGraph.edges graph in
    let nodes' = Seq.map ~f:(fun tid ->
        let name = (Tid.name tid) in
        let label = try Hashtbl.find_exn nodes tid
          with Not_found -> name in
        (name, label)) ns in
    let edges' = Seq.map ~f:(fun edge ->
        (BehaviorGraph.Edge.src edge, BehaviorGraph.Edge.dst edge, BehaviorGraph.Edge.label edge)
      ) es in
    (** SysFlow traces do not contain BIND, patch the model to remove it. *)
    let g' = edges' |>
             Seq.filter ~f:(fun (src, dst, label) ->
                 label = "BIND"
               ) |>
             Seq.map ~f:(fun (src, dst, label) ->
                 dst
               ) |>
             Seq.fold ~f:(fun g v ->
                 let preds = BehaviorGraph.Node.preds v g in
                 let succs = BehaviorGraph.Node.succs v g in
                 let graph' = BehaviorGraph.Node.remove v g in
                 Seq.fold ~f:(fun g' p ->
                     succs |>
                     Seq.map ~f:(fun succ ->
                         let Some edge = BehaviorGraph.Node.edge v succ g in
                         let label = BehaviorGraph.Edge.label edge in
                         BehaviorGraph.Edge.create p succ label) |>
                     Seq.fold ~f:(fun g e ->
                         BehaviorGraph.Edge.insert e g) ~init:g') ~init:graph' preds) ~init:graph in
    let ns = BehaviorGraph.nodes g' in
    let es = BehaviorGraph.edges g' in
    let nodes' = Seq.map ~f:(fun tid ->
        let name = (Tid.name tid) in
        let label = try Hashtbl.find_exn nodes tid
          with Not_found -> name in
        (name, label)) ns in
    let edges' = Seq.map ~f:(fun edge ->
        (BehaviorGraph.Edge.src edge, BehaviorGraph.Edge.dst edge, BehaviorGraph.Edge.label edge)
      ) es in
    let nodes'' = nodes' |> Seq.map ~f:(fun (name, label) -> `String name) |> Seq.to_list in
    let constraints'' = nodes' |> Seq.map ~f:(fun (name, constraints) ->
        `Assoc [("node", `String name); ("constraints", `String constraints)]) |> Seq.to_list in
    let edges'' = edges' |> Seq.map ~f:(fun (src, dst, label) ->
        (**
        let constraints = try Hashtbl.find_exn nodes dst
          with Not_found -> name in
        let jsconstraints = Yojson.Basic.from_string constraints in *)
        `Assoc [("src", `String (Tid.name src)); ("dst", `String (Tid.name dst)); ("label", `String label)]
      ) |>
                  Seq.to_list in
    let model = `Assoc [("initial", `String (Tid.name root));
                        ("nodes", `List nodes'');
                        ("constraints", `List constraints'');
                        ("edges", `List edges'')] in
    let model' = Yojson.Basic.pretty_to_string model in
    printf "%s" model'

  (** Compute the union of the Local and Global
      graphs after a Machine ends and store it in Global. *)
  let record_model () =
    Machine.current () >>= fun pid ->
    let () = info "Machine %a ending!" Id.pp pid in
    if Machine.global = pid then
      let () = info "Global machine ending." in
      Machine.args >>= fun args ->
      Machine.Global.get state >>= fun state' ->
      let {root_tid;nodes;graph} = state' in
      let _ = Graphlib.to_dot (module BehaviorGraph) ~node_attrs:(fun tid ->
          let name = try Hashtbl.find_exn nodes tid
            with Not_found -> (Tid.name tid) in
          [`Label name]
        ) ~string_of_edge:(fun edge -> BehaviorGraph.Edge.label edge) ~channel:stdout graph in
      let _ = export_model root_tid nodes graph in
      Machine.return()
    else
      Machine.Local.get state >>= fun state' ->
      let {nodes;graph;last_tid} = state' in
      let () = info "last tid: %s" (Tid.name last_tid) in
      Machine.Global.update state ~f:(fun s ->
          let () = info " updating global state!" in
          let graph' = Graphlib.union (module BehaviorGraph) s.graph graph in
          {s with halted = Set.add s.halted pid; graph=graph'})

  let record_finished () =
    Machine.current () >>= fun pid ->
    let () = info "machine finished!" in
    Machine.return()

  let setup_tracing () =
    Machine.List.sequence [
      Primus.Interpreter.written >>> record_written;
      Primus.Interpreter.enter_pos >>> record_pos;
      Primus.Interpreter.enter_jmp >>> record_jmp;
      Primus.System.fini >>> record_model;
    ]

  let get x = Future.peek_exn (Config.determined x)

  let init () =
    let open Param in
    setup_tracing () >>= fun () ->
    Machine.get () >>= fun proj ->
    Machine.args >>= fun args ->
    let symtab = Project.symbols proj in
    let symtabs = (symtab |> Symtab.to_sequence |> Seq.to_list) in
    let root = Tid.create() in
    let root' = Tid.create() in
    let proc = Tid.create() in
    let (exe, args') = args |> Array.to_list |> split_argv in
    let entrypoint' = Yojson.Basic.pretty_to_string (jsonify [("sf.proc.exe", [get entrypoint]);
                                                              ("sf.proc.args", [get entrypoint_args])]) in
    let entrypoint'' = Yojson.Basic.pretty_to_string (jsonify [("sf.proc.exe", [get entrypoint]);
                                                               ("sf.proc.args", [get entrypoint_args]);
                                                               ("sf.pproc.pid", [(Tid.name root)])]) in
    let constraints = Yojson.Basic.pretty_to_string (jsonify [("sf.proc.exe", [exe]); ("sf.proc.args", args')]) in
    let behavior = Graphlib.create (module BehaviorGraph) ~nodes:[root;root';proc] ~edges:[(root,root',"CLONE"); (root',proc,"EXEC")] () in
    let nodes = Hashtbl.create (module Tid) in
    let () = Hashtbl.add_exn nodes ~key:root ~data:entrypoint' in
    let () = Hashtbl.add_exn nodes ~key:root' ~data:entrypoint'' in
    let () = Hashtbl.add_exn nodes ~key:proc ~data:constraints in
    Machine.Global.update state ~f:(fun s ->
        { symbols = symtabs;
          nodes = nodes;
          labels = Hashtbl.create (module String);
          visited = Tid.Set.empty;
          halted = Id.Set.empty;
          root_tid = root;
          last_tid = proc;
          graph = behavior;
        }
      ) >>= fun s ->
    Machine.Local.update state ~f:(fun _ ->
        { symbols = symtabs;
          nodes = nodes;
          labels = Hashtbl.create (module String);
          visited = Tid.Set.empty;
          halted = Id.Set.empty;
          root_tid = root;
          last_tid = proc;
          graph = behavior;
        })
end

let main {Config.get=(!)} =
  let open Param in
  if !model then
    Primus.Machine.add_component (module Monitor)

let () =
  Config.when_ready (fun conf ->
      main conf)
