open Core_kernel
open Bap.Std
open Bap_primus.Std
open Bap_future.Std
open Format
open Graphlib.Std
open Monads.Std
open Regular.Std
open Sexplib0
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

  let server_start = param (string) "server-start"
      ~doc:
        "The location of an infinite loop."

end

module Sf = struct

  module Vars = struct
    let pred = "pred"
  end

  let typ                  = "sf.type"
  let opflags              = "sf.opflags"
  let ret                  = "sf.ret"
  let ts                   = "sf.ts"
  let endts                = "sf.endts"
  let proc_oid             = "sf.proc.oid"
  let proc_pid             = "sf.proc.pid"
  let proc_name            = "sf.proc.name"
  let proc_exe             = "sf.proc.exe"
  let proc_args            = "sf.proc.args"
  let proc_uid             = "sf.proc.uid"
  let proc_user            = "sf.proc.user"
  let proc_tid             = "sf.proc.tid"
  let proc_gid             = "sf.proc.gid"
  let proc_group           = "sf.proc.group"
  let proc_createts        = "sf.proc.createts"
  let proc_duration        = "sf.proc.duration"
  let proc_tty             = "sf.proc.tty"
  let proc_cmdline         = "sf.proc.cmdline"
  let proc_aname           = "sf.proc.aname"
  let proc_aexe            = "sf.proc.aexe"
  let proc_acmdline        = "sf.proc.acmdline"
  let proc_apid            = "sf.proc.apid"
  let pproc_oid            = "sf.pproc.oid"
  let pproc_pid            = "sf.pproc.pid"
  let pproc_name           = "sf.pproc.name"
  let pproc_exe            = "sf.pproc.exe"
  let pproc_args           = "sf.pproc.args"
  let pproc_uid            = "sf.pproc.uid"
  let pproc_user           = "sf.pproc.user"
  let pproc_gid            = "sf.pproc.gid"
  let pproc_group          = "sf.pproc.group"
  let pproc_createts       = "sf.pproc.createts"
  let pproc_duration       = "sf.pproc.duration"
  let pproc_tty            = "sf.pproc.tty"
  let pproc_cmdline        = "sf.pproc.cmdline"
  let file_name            = "sf.file.name"
  let file_path            = "sf.file.path"
  let file_canonicalpath   = "sf.file.canonicalpath"
  let file_directory       = "sf.file.directory"
  let file_newname         = "sf.file.newname"
  let file_newpath         = "sf.file.newpath"
  let file_newdirectory    = "sf.file.newdirectory"
  let file_type            = "sf.file.type"
  let file_is_open_write   = "sf.file.is_open_write"
  let file_is_open_read    = "sf.file.is_open_read"
  let file_fd              = "sf.file.fd"
  let file_openflags       = "sf.file.openflags"
  let net_proto            = "sf.net.proto"
  let net_protoname        = "sf.net.protoname"
  let net_sport            = "sf.net.sport"
  let net_dport            = "sf.net.dport"
  let net_port             = "sf.net.port"
  let net_sip              = "sf.net.sip"
  let net_dip              = "sf.net.dip"
  let net_ip               = "sf.net.ip"
  let flow_rbytes          = "sf.flow.rbytes"
  let flow_rops            = "sf.flow.rops"
  let flow_wbytes          = "sf.flow.wbytes"
  let flow_wops            = "sf.flow.wops"
  let container_id         = "sf.container.id"
  let container_name       = "sf.container.name"
  let container_imageid    = "sf.container.imageid"
  let container_image      = "sf.container.image"
  let container_type       = "sf.container.type"
  let container_privileged = "sf.container.privileged"

  let special id attr = Printf.sprintf "%%%s.%s" id attr

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
  | Close of fd
  | Recv of fd
  | Send of fd
  | Exit

module BehaviorGraph = Graphlib.Make(Tid)(String)

type state = {
  symbols : Symtab.fn list;
  labels : (string, Var.t) Hashtbl.t;
  nodes : (Tid.t, string) Hashtbl.t;
  ports : (fd, int) Hashtbl.t;
  files : (fd, string) Hashtbl.t;
  last_jump_conditional: bool;
  root_tid : Tid.t;
  last_tid : Tid.t;
  file_opened : string option;
  loops : (Tid.t, int) Hashtbl.t;
  halted : Id.Set.t;
  graph : BehaviorGraph.t;
  arrays : (string, (Primus.Value.t * Primus.Value.t Seq.t)) Hashtbl.t;
  server_tid : Tid.t;
}

exception InvalidArgv of string
exception MissingEdge of string

let edge_of_operation op =
  match op with
    Clone argv -> "CLONE"
  | Exec argv -> "EXEC"
  | Open path -> "OPEN"
  | Bind (fd, port) -> "BIND"
  | Accept fd -> "ACCEPT"
  | Read fd -> "READ"
  | Write fd -> "WRITE"
  | Close fd -> "CLOSE"
  | Recv fd -> "RECV"
  | Send fd -> "SEND"
  | Exit -> "EXIT"

let jsonify xs =
  `Assoc (List.map ~f:(fun (key, vs) ->
      let vs' = List.map ~f:(fun v -> `String v) vs in
      let vs'' = `List vs' in
      (key, vs'')) xs)

let split_argv argv =
  match argv with
    [] -> raise (InvalidArgv "empty argv provided")
  | exe :: args ->
    let args' = String.concat ~sep:" " args in
    (exe, args')

let string_of_json json =
  json |>
  Yojson.Basic.to_string |>
  String.map ~f:(fun c -> if c = '"' then '\'' else c)

let node_of_operation state op =
  let json =
    match op with
      Clone argv ->
      let (exe, args) = split_argv argv in
      jsonify [(Sf.proc_exe, [exe]); (Sf.proc_args, [args])]
    | Exec argv ->
      let (exe, args) = split_argv argv in
      jsonify [(Sf.proc_exe, [exe]); (Sf.proc_args, [args])]
    | Open path ->
      jsonify [(Sf.file_path, [path])]
    | Bind (fd, port) ->
      let port' = Printf.sprintf "%d" port in
      jsonify [(Sf.net_dport, [port'])]
    | Accept fd ->
      let port = Hashtbl.find_exn state.ports fd in
      let port' = Printf.sprintf "%d" port in
      jsonify [(Sf.net_dport, [port'])]
    | Read fd ->
      let file = Hashtbl.find_exn state.files fd in
      jsonify [(Sf.file_path, [file])]
    | Write fd ->
      let file = Hashtbl.find_exn state.files fd in
      jsonify [(Sf.file_path, [file])]
    | Close fd ->
      let file = Hashtbl.find_exn state.files fd in
      jsonify [(Sf.file_path, [file])]
    | Recv fd ->
      let port = Hashtbl.find_exn state.ports fd in
      let port' = Printf.sprintf "%d" port in
      jsonify [(Sf.net_dport, [port'])]
    | Send fd ->
      let port = Hashtbl.find_exn state.ports fd in
      let port' = Printf.sprintf "%d" port in
      jsonify [(Sf.net_dport, [port'])]
    | Exit ->
      jsonify [] in
  string_of_json json

let labeled label node =
  { node= node; node_label=label }

let add_operation tid op state =
  let () = info "Adding operation:" in
  let {nodes;graph;last_tid} = state in
  let node_label = node_of_operation state op in
  let edge_label = edge_of_operation op in
  let graph' = BehaviorGraph.Node.insert tid graph in
  let () = info "   %s" edge_label in
  let edge = BehaviorGraph.Edge.create last_tid tid edge_label in
  let graph'' = BehaviorGraph.Edge.insert edge graph' in
  let () = Hashtbl.set nodes ~key:tid ~data:node_label in
  { state with last_tid = tid; graph=graph'' }

let state = Primus.Machine.State.declare
    ~name:"uids"
    ~uuid:"ba442400-63dd-11ea-a41a-06f59637065f"
    (fun _ ->
       let root = Tid.create() in
       let behavior = Graphlib.create (module BehaviorGraph) () in
       { root_tid = root;
         last_tid = root;
         nodes = Hashtbl.create (module Tid);
         ports = Hashtbl.create (module Int);
         files = Hashtbl.create (module Int);
         last_jump_conditional = true;
         file_opened = None;
         symbols = [];
         labels = Hashtbl.create (module String);
         loops = Hashtbl.create (module Tid);
         halted = Id.Set.empty;
         graph = behavior;
         arrays = Hashtbl.create (module String);
         server_tid = Tid.create();
       })

module Pre(Machine : Primus.Machine.S) = struct
  include Machine.Syntax
  module Memory = Primus.Memory.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  let string_of_value v = (v |> Value.to_word |> Word.to_string)
  let nil = Value.b0
  let bool = function
    | false -> nil
    | true -> Value.b1

  let allow_all_memory_access access =
    Machine.catch access (function exn ->
        let () = info "Error reading memory!" in
        let msg = Primus.Exn.to_string exn in
        let () = info "    %s" msg in
        Value.of_bool (false))

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

end

module ArrayMake(Machine : Primus.Machine.S) = struct
  [@@@warning "-P"]
  include Pre(Machine)

  let run [arr; size] =
    Machine.Local.update state ~f:(fun s ->
      let {arrays} = s in
      let () = Hashtbl.set arrays ~key:(string_of_value arr) ~data:(size, Seq.empty) in
      s
    ) >>| fun () ->
    arr
end

module ArraySize(Machine : Primus.Machine.S) = struct
  [@@@warning "-P"]
  include Pre(Machine)

  let run [arr] =
    Machine.Local.get state >>= fun s ->
      let {arrays} = s in
      let (sz, arr') = Hashtbl.find_exn arrays (string_of_value arr) in
      Machine.return (sz)

end

module Push(Machine : Primus.Machine.S) = struct
  [@@@warning "-P"]
  include Pre(Machine)

  let run [arr; data] =
    Machine.Local.update state ~f:(fun s ->
      let {arrays} = s in
      let (sz, arr') = Hashtbl.find_exn arrays (string_of_value arr) in
      let arr'' = Seq.cons data arr' in
        Hashtbl.set arrays ~key:(string_of_value arr) ~data:(sz, arr'');
        s
    ) >>| fun () ->
    data
end

module Pop(Machine : Primus.Machine.S) = struct
  [@@@warning "-P"]
  include Pre(Machine)

  let run [arr] =
    Machine.Local.get state >>= fun s ->
      let {arrays} = s in
      let (sz, arr') = Hashtbl.find_exn arrays (string_of_value arr) in
      let opt = Seq.hd arr' in
        match opt with
          None -> nil
        | Some x -> Machine.return (x)
    >>= fun x ->
      Machine.Local.update state ~f:(fun s ->
        let {arrays} = s in
        let (sz, arr') = Hashtbl.find_exn arrays (string_of_value arr) in
        let opt = Seq.tl arr' in
        let () = match opt with
                   None -> Hashtbl.set arrays ~key:(string_of_value arr) ~data:(sz, Seq.empty)
                 | Some tl -> Hashtbl.set arrays ~key:(string_of_value arr) ~data:(sz, tl) in
        s
      ) >>= fun () ->
        Machine.return x
end

module Scan(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let run [str;fmt] =
      let vstr = Value.to_word str in
      let vfmt = Value.to_word fmt in
      string_of_addr vstr >>= fun str' ->
      string_of_addr vfmt >>= fun fmt' ->
        let () = info "str %s %s" str' fmt' in
        let port = (((Scanf.sscanf str') "port: %d") (fun i -> i)) in
        let () = info "port: %d" port in
        nil
end

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
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec
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
    | "apr_file_open" ->
      let () = info "model open:" in
      let rsi = (Var.create "RSI" reg64_t) in
      (Env.get rsi) >>= fun v ->
        (v |> Value.to_word |> string_of_addr) >>= fun path ->
        let () = info " RSI: %s" path in
        Machine.Local.update state ~f:(fun state' ->
            let op = (Open path) in
            (add_operation tid op state'))
    | "apr_file_gets" ->
      let () = info "model read:" in
      let rsi = (Var.create "RSI" reg64_t) in
      let rdx = (Var.create "RDX" reg64_t) in
      (Env.get rsi) >>= fun u ->
        let () = info " RSI: %d" (u |> Value.to_word |> Bitvector.to_int_exn) in
      (Env.get rdx) >>= fun v ->
        let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
        let () = info " RDX: %d" fd in
        Machine.Local.update state ~f:(fun state' ->
            let op = (Read fd) in
            (add_operation tid op state'))
    | "atoi" ->
      let () = info "model atoi:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun u ->
        let () = info " RDI: %d" (u |> Value.to_word |> Bitvector.to_int_exn) in
        (u |> Value.to_word |> string_of_addr) >>= fun str ->
        let () = info "  atoi: %s" str in
        Machine.return()
    | "fopen" ->
      let () = info "model fopen:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      (v |> Value.to_word |> string_of_addr) >>= fun path ->
      let () = info " RDI: %s" path in
      Machine.Local.update state ~f:(fun state' ->
          let op = (Open path) in
          let state'' = {state' with file_opened=Some path} in
          (add_operation tid op state''))
    | "fgetc" ->
      let () = info "model fgetc:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      let () = info " RDI: %d" fd in
      Machine.Local.update state ~f:(fun state' ->
          let op = (Read fd) in
          (add_operation tid op state'))
    | "fclose" ->
      let () = info "model fclose:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      let () = info " RDI: %d" fd in
      Machine.Local.update state ~f:(fun state' ->
          let op = (Close fd) in
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
          let () = Hashtbl.set state'.ports ~key:fd ~data:port in
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
      let () = info "model recv:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Recv fd in
          (add_operation tid op state'))
    | "sendmsg" ->
      let () = info "model send:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Send fd in
          (add_operation tid op state'))
    | "_terminate" ->
      let () = info "model terminate:" in
      Machine.Local.update state ~f:(fun state' ->
          let op = Exit in
          (add_operation tid op state'))
    | "receive" ->
      let () = info "model receive:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Read fd in
          (add_operation tid op state'))
    | "transmit" ->
      let () = info "model transmit:" in
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Write fd in
          (add_operation tid op state'))
    | _ ->
      let () = info "called %s from %s" func (Tid.name tid) in
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
    Machine.Local.get state >>= fun {file_opened} ->
    if start = '#' then
      Machine.Global.update state ~f:(fun state' ->
        let () = Hashtbl.set state'.labels ~key:name ~data:x in
        state'
      )
    else
      match file_opened with
        None -> Machine.return()
      | Some file -> Machine.Local.update state ~f:(fun s ->
        let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
        let _ = Hashtbl.set s.files ~key:fd ~data:file in
        {s with file_opened = None}
      )

  let export_model root nodes graph =
    let es = BehaviorGraph.edges graph in
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
                         let opt = BehaviorGraph.Node.edge v succ g in
                         match opt with
                           None -> raise (MissingEdge "Cannot find edge to successor")
                         | Some edge ->
                             let label = BehaviorGraph.Edge.label edge in
                             BehaviorGraph.Edge.create p succ label) |>
                     Seq.fold ~f:(fun g e ->
                         BehaviorGraph.Edge.insert e g) ~init:g') ~init:graph' preds) ~init:graph in
    let ns = BehaviorGraph.nodes g' in
    let es = BehaviorGraph.edges g' in
    let nodes' = Seq.map ~f:(fun tid ->
        let name = (Tid.name tid) in
        let label = try Hashtbl.find_exn nodes tid
          with Not_found_s s ->
              let () = info "missing node %s %s" name (Sexplib0.Sexp.to_string_hum s) in
              name in
        (name, label)) ns in
    let edges' = Seq.map ~f:(fun edge ->
        (BehaviorGraph.Edge.src edge, BehaviorGraph.Edge.dst edge, BehaviorGraph.Edge.label edge)
      ) es in
    let nodes'' = nodes' |> Seq.map ~f:(fun (name, label) -> `String name) |> Seq.to_list in
    let constraints'' = nodes' |> Seq.map ~f:(fun (name, constraints) ->
        let jsconstraints = constraints |>
             String.map ~f:(fun c -> if c = '\'' then '"' else c) |>
             Yojson.Basic.from_string in
        `Assoc [("node", `String name); ("constraints", jsconstraints)]) |> Seq.to_list in
    let edges'' = edges' |> Seq.map ~f:(fun (src, dst, label) ->
        `Assoc [("src", `String (Tid.name src)); ("dst", `String (Tid.name dst)); ("label", `String label)]
      ) |> Seq.to_list in
    let model = `Assoc [("initial", `String (Tid.name root));
                        ("nodes", `List nodes'');
                        ("constraints", `List constraints'');
                        ("edges", `List edges'')] in
    let model' = Yojson.Basic.pretty_to_string model in
    printf "%s" model'


  (** Compute the union of the Local and Global
      graphs after a Machine ends and store it in Global. *)
  let record_model () =
    let dotfile = Out_channel.create "output.dot" in
    Machine.current () >>= fun pid ->
    let () = info "Machine %a ending!" Id.pp pid in
    if Machine.global = pid then
      let () = info "Global machine ending." in
      Machine.args >>= fun args ->
      Machine.Global.get state >>= fun state' ->
      let {root_tid;nodes;graph} = state' in
      let _ = Graphlib.to_dot (module BehaviorGraph) ~node_attrs:(fun tid ->
          let name = try Hashtbl.find_exn nodes tid
            with Not_found_s s ->
                let () = info "missing node %s %s" name (Sexplib0.Sexp.to_string_hum s) in
                (Tid.name tid) in
          [`Label name; `Shape `Box]
        )
        ~string_of_edge:(fun edge -> BehaviorGraph.Edge.label edge)
        ~channel:dotfile graph in
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
      record_model() >>= fun () ->
      Machine.kill pid >>= fun () ->
      Machine.switch cid


  let record_jmp j = Machine.current () >>= fun pid ->
    let tid = Term.tid j in
    let () = info "entering jump %s" (Tid.name tid) in
    let target_tid = match Jmp.dst j with
                       None -> Tid.create()
                     | Some dst -> (
                       match Jmp.resolve dst with
                         First tid ->
                           let () = info "target tid %s" (Tid.name tid) in
                           tid
                     | Second _ -> Tid.create()) in
    Machine.Global.get state >>= fun gs ->
    Machine.Local.get state >>= fun {last_tid; nodes; graph; last_jump_conditional; loops; server_tid} ->
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
              let target = (v |> Value.to_word) in
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
          let guard = Jmp.guard j in
          let guarded = match guard with
                          None -> false
                        | Some _ -> true in
          let label' = Label.to_string label in
          let () = info "goto label %s, guarded %b" label' guarded in
          Machine.Local.update state ~f:(fun s ->
            let () = Hashtbl.update s.loops target_tid ~f:(fun opt ->
              match opt with
                None -> 1
              | Some x -> succ x) in
            if guarded then
              { s with last_jump_conditional=true }
            else
              s) >>= fun _ ->
              Machine.Local.get state >>= fun {loops} ->
                let hits = match (Hashtbl.find loops target_tid) with
                             Some i -> i
                           | None -> 0 in
                let () = info "Tid %s has %d hits" (Tid.name target_tid) hits in
                if hits > 1 && target_tid = server_tid then
                  reschedule()
                else
                  Machine.return()
        | _ ->
          let () = info "    Different kind of jump" in
          Machine.return ()

  let record_finished () =
    Machine.current () >>= fun pid ->
    let () = info "machine finished!" in
    Machine.return()

  let def name types closure docs =
    Lisp.define ~docs ~types name closure

  let setup_tracing () =
    Machine.List.sequence [
      Primus.Interpreter.written >>> record_written;
      Primus.Interpreter.enter_pos >>> record_pos;
      Primus.Interpreter.enter_jmp >>> record_jmp;
      Primus.System.fini >>> record_model;
      def "array-make" (tuple [a; b] @-> bool) (module ArrayMake)
      {|(array-make ARRAY DATA) makes an ARRAY. |};
      def "array-elt-size" (tuple [a] @-> b) (module ArraySize)
      {|(array-elt-size ARRAY) gets the size of elements in ARRAY. |};
      def "array-push" (tuple [a; b] @-> bool) (module Push)
      {|(array-push ARRAY DATA) pushes DATA onto ARRAY. |};
      def "array-pop" (tuple [a] @-> b) (module Pop)
      {|(array-pop ARRAY DATA) pops DATA from ARRAY. |};
      def "uids-ocaml-sscanf" (tuple [a; b] @-> bool) (module Scan)
      {|(uids-ocaml-sscanf) tries to implement sscanf. |};
    ]

  let get x = Future.peek_exn (Config.determined x)

  let json_string data =
   data |>
   jsonify |>
   Yojson.Basic.pretty_to_string |>
   String.map ~f:(fun c -> if c = '"' then '\'' else c)

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
    let server_tid = ok_exn (Tid.from_string (get server_start)) in
    let () = info "Using %s as server_tid" (Tid.name server_tid) in
    let (exe, args') = args |> Array.to_list |> split_argv in
    let entry = json_string [(Sf.proc_exe, [get entrypoint]);
                             (Sf.proc_args, [get entrypoint_args])] in
    let cloned_entry = json_string [(Sf.proc_exe, [get entrypoint]);
                                    (Sf.proc_args, [get entrypoint_args]);
                                    (Sf.pproc_pid, [Sf.special Sf.Vars.pred Sf.proc_pid])] in
    let constraints = json_string [(Sf.proc_exe, [exe]); (Sf.proc_args, [args'])] in
    let behavior = Graphlib.create (module BehaviorGraph)
                                   ~nodes:[root;root';proc]
                                   ~edges:[(root,root',"CLONE"); (root',proc,"EXEC")] () in
    let nodes = Hashtbl.create (module Tid) in
    let arrays = Hashtbl.create (module String) in
    let ports = Hashtbl.create (module Int) in
    let files = Hashtbl.create (module Int) in
    let () = Hashtbl.add_exn files ~key:0 ~data:"/dev/stdin" in
    let () = Hashtbl.add_exn files ~key:1 ~data:"/dev/stdout" in
    let () = Hashtbl.add_exn files ~key:2 ~data:"/dev/stderr" in
    let () = Hashtbl.add_exn nodes ~key:root ~data:entry in
    let () = Hashtbl.add_exn nodes ~key:root' ~data:cloned_entry in
    let () = Hashtbl.add_exn nodes ~key:proc ~data:constraints in
    Machine.Global.update state ~f:(fun s ->
        { symbols = symtabs;
          nodes = nodes;
          ports = ports;
          files = files;
          last_jump_conditional = true;
          file_opened = None;
          labels = Hashtbl.create (module String);
          loops = Hashtbl.create (module Tid);
          halted = Id.Set.empty;
          root_tid = root;
          last_tid = proc;
          graph = behavior;
          arrays = arrays;
          server_tid = server_tid;
        }
      ) >>= fun s ->
    Machine.Local.update state ~f:(fun _ ->
        { symbols = symtabs;
          nodes = nodes;
          ports = ports;
          files = files;
          last_jump_conditional = true;
          file_opened = None;
          labels = Hashtbl.create (module String);
          loops = Hashtbl.create (module Tid);
          halted = Id.Set.empty;
          root_tid = root;
          last_tid = proc;
          graph = behavior;
          arrays = arrays;
          server_tid = server_tid;
        })
end

let desc =
  "The uIDS modeler will attempt to approximate the behavior of a binary based on
   the system calls found within it."

let main {Config.get=(!)} =
  let open Param in
  if !model then
    Primus.Machine.add_component (module Monitor) [@warning "-D"];
    Primus.Components.register_generic "uids" (module Monitor)
      ~package:"bap"
      ~desc:("Enables the uIDS modeler. " ^ desc)

let () =
  Config.when_ready (fun conf ->
      main conf)
