(**
  Authors:
  William Blair <wdblair@bu.edu>
*)

open Core
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

module Channels = Upolicycraft_lisp_io

type scanf_value =
  | Number of int
  | String of string

let lisp_io_state = Upolicycraft_lisp_io.state

module Redirection = struct
  type t = string * string

  let known_channels = Channels.standard_channels

  let make oldname newname =
    if String.length oldname < 1 ||
      String.length newname < 1
    then `Error ("bad redirection: expected two non-empty names")
    else if Char.equal (String.get oldname 0) '<'
    then if List.mem known_channels ~equal:String.equal oldname
      then `Ok (oldname,newname)
      else `Error (sprintf "unknown channel %s, expected one of %s"
                     oldname (String.concat ~sep:", " known_channels))
    else `Ok (oldname,newname)

  let parse str = match String.split str ~on:':' with
    | [oldname; newname] -> make oldname newname
    | _ -> `Error (sprintf "bad redirection, expected <old>:<new> got %s" str)

  let print ppf (oldname,newname) =
    Format.fprintf ppf "%s:%s" oldname newname

  let convert = Config.converter parse print ("none","none")
end

module Param = struct
  open Config;;
  manpage [
    `S "DESCRIPTION";
    `P "Derive a process model for a binary."
  ]

  let model = flag "model"
      ~doc:
        "Derive a process model for a binary."

  let detect_syscalls = flag "detect-syscalls"
      ~doc:
        "Detect the system calls reachable from the entry."

  let inetd_startup = flag "inetd-startup"
      ~doc:
        "Exec the program and connect to a socket"

  let symbolic_arguments = flag "symbolic-arguments"
      ~doc:
        "Ignore concrete values for printf style functions"

  let entrypoint = param (string) "entrypoint"
      ~doc:
        "The entrypoint for the container"

  let exitpoint = param (int) "exitpoint"
      ~default:0xcfffffff
      ~doc:
        "The point to stop micro-execution"

  let entrypoint_args = param (string) "entrypoint-args"
      ~doc:
        "The arguments passed to the entrypoint"

  let test_cases = param (int) "no-test-cases"
      ~default:0
      ~doc:"The number of test cases."

  let user_id = param (int) "user-id"
      ~default:0
      ~doc:"The default user id."

  let group_id = param (int) "group-id"
      ~default:0
      ~doc:"The default group id."

  let filesystem = param (string) "filesystem"
      ~default:""
      ~doc:"The mapping of the analysis filesystem to the host filesystem."

  let redirects = param (list Redirection.convert) "redirect"
      ~doc:"The mapping from the emulated filesystem to the host."

  let model_dir = param (string) "model-dir"
      ~default:"/tmp/"
      ~doc:"The directory to store models."

  let model_name = param (string) "model-name"
      ~default:"output"
      ~doc:"The name for the model."

  let language = param (string) "language"
      ~default:"c"
      ~doc:"The programming language that produced the binary."
end

module Languages = struct
  let golang = "go"
end

(** For fetching arguments. *)
let get x = Future.peek_exn (Config.determined x)

(** Use a large address outside the 32-bit range for the bottom of the heap. *)
let bottom_of_heap = 0x1000000000L

module Sf = struct

  module Vars = struct
    let pred = "pred"
  end

  type flow =
      File
    | Network
    | Process

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
  let file_perms           = "sf.file.perms"
  let file_size            = "sf.file.size"
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

  let stdin_fd = 0
  let stdout_fd = 1

  let process_label = "P"
  let file_label = "F"
  let network_label = "N"

end

type fd = int
type status = int

module ArgSet = Set.Make(String)
module OperationSet = Set.Make(String)

type permissions =
  | R
  | W
  | X

type operation =
  | Accept of fd
  | Bind of fd * int
  | Connect of fd * int * int
  | ConnectAndRw of fd * string * int
  | Chown of fd * int * int
  | Chmod of fd * int
  | Clone of string list
  | Close of fd
  | Exec of string list
  | Exit of status
  | Mkdir of string * int
  | Mmap of int * (permissions list)
  | Open of string
  | Read of fd
  | Recv of fd
  | Send of fd
  | Setgid of int
  | Setuid of int
  | Write of fd
  | Sendfile of fd * fd

module EffectGraph = Graphlib.Make(Tid)(String)

type state = {
  uid : int;
  gid : int;
  saved_uid : int option;
  saved_gid : int option;
  symbols : Symtab.fn list;
  labels : (string, Var.t) Hashtbl.t;
  nodes : (Tid.t, ArgSet.t) Hashtbl.t;
  functions : (Tid.t, (string * string)) Hashtbl.t;
  ports : (fd, int) Hashtbl.t;
  files : (fd, string) Hashtbl.t;
  root_tid : Tid.t;
  last_tid : Tid.t;
  file_opened : string option;
  halted : Id.Set.t;
  visited_blks : Blk.Set.t;
  graph : EffectGraph.t;
  arrays : (string, (Primus.Value.t * Primus.Value.t Seq.t)) Hashtbl.t;
  current_function : string;
  current_module : string;
  callstack : string list;
  no_test_cases : int;
  filesystem : (string, string) Hashtbl.t;
  duped_file_descriptors : (int, int) Hashtbl.t;
  cwd : string;
  written_vars : Var.Set.t;
  fs_set : bool;
  top_of_heap : int64;
  last_rax : Bitvector.t list;
  exitpoint : Bitvector.t;
}

exception InvalidArgv of string
exception MissingEdge of string

let string_of_permission p =
  match p with
    R -> "READ"
  | W -> "WRITE"
  | X -> "EXEC"

let edge_of_operation op =
  match op with
    Clone argv -> "CLONE"
  | Connect (fd, ipaddr, port) -> "CONNECT"
  | ConnectAndRw (fd, host, port) -> "CONNECT"
  | Chown (fd, uid, gid) -> "CHOWN"
  | Chmod (fd, mode) -> "CHMOD"
  | Exec argv -> "EXEC"
  | Open path -> "OPEN"
  | Bind (fd, port) -> "BIND"
  | Accept fd -> "ACCEPT"
  | Read fd -> "READ"
  | Write fd -> "WRITE"
  | Close fd -> "CLOSE"
  | Recv fd -> "RECV"
  | Send fd -> "SEND"
  | Sendfile (outfd, infd) -> "SENDFILE"
  | Setuid id -> "SETUID"
  | Setgid id -> "SETGID"
  | Mkdir (path, mode) -> "MKDIR"
  | Mmap (size, perms) -> "MMAP"
  | Exit status -> "EXIT"

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
  String.map ~f:(fun c -> if phys_equal c '"' then '\'' else c)

let check_copied_descriptor state fd =
  match Hashtbl.find state.duped_file_descriptors fd with
    None -> fd
  | Some dupedfd -> dupedfd

(** Some binaries erroneously build paths with extra slashes. *)
let sanitize_path path =
    String.substr_replace_all path ~pattern:"//" ~with_:"/"

(** Restrict activity to a given user when applicable. *)
let restrict_to_user state constr =
  let {uid;gid} = state in
  let uid' = string_of_int uid in
  let gid' = string_of_int gid in
  let restrict field value constr =
    if List.Assoc.mem constr ~equal:String.equal field then
      constr
    else
      List.Assoc.add constr ~equal:String.equal field [value] in
  constr |>
  restrict Sf.proc_uid uid' |>
  restrict Sf.proc_gid gid'

let constraint_of_fd state (io_state : Upolicycraft_lisp_io.state) fd =
  (** let () = info "Fetching state of file descriptor %d:" fd in *)
  (** let () = info "  %s" (state.ports) *)
  let fd' = check_copied_descriptor state fd in
  let opt = Hashtbl.find state.ports fd' in
  match opt with
    Some port ->
       let port' = Printf.sprintf "%d" port in
       (** let () = info "  %s" port' in *)
       [(Sf.net_dport, [port'])]
  | None ->
    let opt = Map.find io_state.files fd' in
    match opt with
      |  Some file ->
        (** Pull from the IO state. *)
        let file' = sanitize_path file in
        (* let () = info "  %s" file' in *)
        let const =
          if String.contains file ':' then
            let parts  = String.split ~on:':' file in
            let port = List.nth_exn parts 1 in
              (Sf.net_dport, [port])
          else
            (Sf.file_path, [file']) in
        [const]
      (** Several entities may not be backed by specific files. *)
      | None -> [(Sf.file_path, ["/*"])]

let node_of_operation state io_state op =
  let constr =
    match op with
      Clone argv ->
      let (exe, args) = split_argv argv in
      [(Sf.proc_exe, [exe]); (Sf.proc_args, [args])]
    | Connect (fd, ipaddr, port) ->
      let file_constraints = constraint_of_fd state io_state fd in
      let port = Printf.sprintf "%d" port in
      let ipaddr = Printf.sprintf "%x" ipaddr in
      List.append file_constraints [(Sf.net_ip, [ipaddr]); (Sf.net_dport, [port])]
    | ConnectAndRw (fd, host, port) ->
      let file_constraints = constraint_of_fd state io_state fd in
      let port = Printf.sprintf "%d" port in
      let host = Printf.sprintf "%s" host in
      List.append file_constraints [(Sf.net_ip, [host]); (Sf.net_dport, [port])]
    | Exec argv ->
      let (exe, args) = split_argv argv in
      [(Sf.proc_exe, [exe]); (Sf.proc_args, [args])]
    | Open path ->
      let path' = sanitize_path path in
      [(Sf.file_path, [path'])]
    | Bind (fd, port) ->
      let port' = Printf.sprintf "%d" port in
      [(Sf.net_dport, [port'])]
    | Accept fd ->
      let port = Hashtbl.find_exn state.ports fd in
      let port' = Printf.sprintf "%d" port in
      [(Sf.net_dport, [port'])]
    | Read fd ->
      constraint_of_fd state io_state fd
    | Write fd ->
      constraint_of_fd state io_state fd
    | Close fd ->
      constraint_of_fd state io_state fd
    | Chown (fd, uid, gid) ->
      let constraints = constraint_of_fd state io_state fd in
      constraints
    | Chmod (fd, mode) ->
      let constraints = constraint_of_fd state io_state fd in
      let mode' = Printf.sprintf "%x\n" mode in
      List.append [(Sf.file_perms, [mode'])] constraints
    | Recv fd ->
      let port = Hashtbl.find_exn state.ports fd in
      (** let () = info "Saving port for recv: %d" port in *)
      let port' = Printf.sprintf "%d" port in
      [(Sf.net_dport, [port'])]
    | Send fd ->
      let port = Hashtbl.find_exn state.ports fd in
      let port' = Printf.sprintf "%d" port in
      [(Sf.net_dport, [port'])]
    | Sendfile (outfd, infd) ->
      List.append (constraint_of_fd state io_state outfd) (constraint_of_fd state io_state infd)
    | Setuid id ->
      let id' = Printf.sprintf "%d" id in
      [(Sf.proc_uid, [id'])]
    | Setgid id ->
      let id' = Printf.sprintf "%d" id in
      [(Sf.proc_gid, [id'])]
    | Mkdir (path, mode) ->
      let mode' = Printf.sprintf "%d" mode in
      [(Sf.file_path, [path]); (Sf.file_perms, [mode'])]
    | Mmap (length, perms) ->
      let perms' = List.map ~f:string_of_permission perms in
      let length' = Printf.sprintf "%d" length in
      [(Sf.file_perms, perms'); (Sf.file_size, [length'])]
    | Exit status ->
      let status' = string_of_int status in
      [(Sf.ret, [status'])] in
  constr |>
  restrict_to_user state |>
  jsonify |>
  string_of_json

let labeled label node =
  { node= node; node_label=label }

let operation_affects_privilege op =
  match op with
    Setuid _ -> true
  | Setgid _ -> true
  | _ -> false

let escalate_privileges state =
  let {saved_uid;saved_gid} = state in
  let state' = match saved_uid with
                 Some uid ->
                 { state with uid = uid; saved_uid = None }
               | None -> state in
  let state'' = match saved_gid with
                 Some gid -> { state' with gid = gid; saved_gid = None }
                | None -> state' in
  state''

let add_operation tid op state (io_state: Upolicycraft_lisp_io.state) =
  (** let () = info "Adding operation:" in *)
  let {nodes;graph;last_tid;functions;callstack;current_module} = state in
  let state' =
    if not (operation_affects_privilege op) then
      escalate_privileges state
    else
      state in
  let node_label = node_of_operation state' io_state op in
  let edge_label = edge_of_operation op in
  let graph' = EffectGraph.Node.insert tid graph in
  (** let () = info "   %s" edge_label in *)
  let edge = EffectGraph.Edge.create last_tid tid edge_label in
  let graph'' = EffectGraph.Edge.insert edge graph' in
  let opt = Hashtbl.find nodes tid in
  let argSet = match opt with
                 None -> ArgSet.empty
               | Some set -> set in
  let argSet' = ArgSet.add argSet node_label in
  let () = Hashtbl.set nodes ~key:tid ~data:argSet' in
  let current_function = List.hd_exn callstack in
  let () = Hashtbl.set functions ~key:tid
                                 ~data:(current_module, current_function) in
  { state' with last_tid = tid; graph=graph'' }

let state = Primus.Machine.State.declare
    ~name:"upc"
    ~uuid:"ba442400-63dd-11ea-a41a-06f59637065f"
    (fun _ ->
       let root = Tid.create() in
       let behavior = Graphlib.create (module EffectGraph) () in
       { root_tid = root;
         last_tid = root;
         nodes = Hashtbl.create (module Tid);
         functions = Hashtbl.create (module Tid);
         ports = Hashtbl.create (module Int);
         files = Hashtbl.create (module Int);
         file_opened = None;
         symbols = [];
         labels = Hashtbl.create (module String);
         halted = Id.Set.empty;
         visited_blks = Blk.Set.empty;
         graph = behavior;
         arrays = Hashtbl.create (module String);
         current_function = "main";
         current_module = "main";
         callstack = ["entrypoint"];
         no_test_cases = 0;
         uid = 0;
         gid = 0;
         saved_uid = None;
         saved_gid = None;
         filesystem = Hashtbl.create (module String);
         duped_file_descriptors = Hashtbl.create (module Int);
         cwd = "/";
         written_vars = Var.Set.empty;
         fs_set = false;
         top_of_heap = bottom_of_heap;
         last_rax = [];
         exitpoint = Bitvector.of_int ~width:64 0;
       })

module Pre(Machine : Primus.Machine.S) = struct
  include Machine.Syntax
  module Env = Primus.Interpreter.Make(Machine)
  module Memory = Primus.Memory.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  module Lisp = Primus.Lisp.Make(Machine)

  let arg_regs = ["RDI"; "RSI"; "RDX"; "RCX"; "R8"; "R9"]

  let addr_width =
    Machine.arch >>| Arch.addr_size >>| Size.in_bits
  let int_of_value v = (v |> Value.to_word |> Bitvector.to_int_exn)
  let int64_of_value v = (v |> Value.to_word |> Bitvector.to_int64_exn)
  let value_of_int64 i = (i |> Bitvector.of_int64 ~width:64 |> Value.of_word)
  let string_of_value v = (v |> Value.to_word |> Word.to_string)
  let zero = Value.of_word (Bitvector.of_int ~width:64 0)
  let nil = Value.b0
  let error = addr_width >>= fun w -> Value.of_word (Word.ones w)
  let ok = addr_width >>= Value.zero

  let bool = function
    | false -> nil
    | true -> Value.b1

    let trap_memory_write access =
      Machine.catch access (function exn ->
        (** let () = info "Error reading memory!" in
        let msg = Primus.Exn.to_string exn in
        let () = info "    %s" msg in *)
        Machine.return())

  let allow_all_memory_access access =
    Machine.catch access (function exn ->
        (** let () = info "Error reading memory!" in
        let msg = Primus.Exn.to_string exn in
        let () = info "    %s" msg in *)
        Value.of_bool (false))

  let dump_memory addr steps =
    (* let () = info "dump memory:" in *)
    let rec loop n addr =
      if n = steps then
        Machine.return()
      else
        allow_all_memory_access (Memory.get addr) >>= fun v ->
         let x = v |> Value.to_word |> Bitvector.to_int64_exn in
         let () = info "  %Lx" x in
        loop (succ n) (Bitvector.succ addr) in
    loop 0 addr

  let string_of_addr addr =
    let rec loop addr cs =
      (* let () = info "Fetching %s" (Bitvector.to_string addr) in *)
      (allow_all_memory_access (Memory.get addr)) >>= fun v ->
      let x = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (* let () = info "  Fetched: %d" x in *)
      if x = 0 then
        let s = (cs |> List.rev |> String.of_char_list) in
        (** let () = info "  %s" s in *)
        Machine.return (s)
      else
        let c = Char.of_int_exn x in
        loop (Bitvector.succ addr) (c :: cs) in
    loop addr []

  let string_of_addr_limit addr n =
    let rec loop addr cs n =
      if n = 0 then
        let s = (cs |> List.rev |> String.of_char_list) in
        Machine.return(s)
      else
        (* let () = info "Fetching %s" (Bitvector.to_string addr) in *)
        (allow_all_memory_access (Memory.get addr)) >>= fun v ->
        let x = (v |> Value.to_word |> Bitvector.to_int_exn) in
        (* let () = info "  Fetched: %d" x in *)
        if x = 0 then
          let s = (cs |> List.rev |> String.of_char_list) in
          (** let () = info "  %s" s in *)
          Machine.return (s)
        else
          let c = Char.of_int_exn x in
          loop (Bitvector.succ addr) (c :: cs) (pred n) in
    loop addr [] n

    (** Copy a string into memory *)
    let copy_bytes addr s =
      let addr' = (Value.to_word addr) in
      (* let () = info "copying %s into %x" s (Bitvector.to_int_exn addr') in *)
      let cont' = String.foldi ~f:(fun i cont c ->
        let c' = int_of_char c in
        Value.of_word (Bitvector.of_int ~width:64 c') >>= fun v ->
        cont >>= fun () ->
          let dst = (Bitvector.add addr' (Bitvector.of_int ~width:64 i)) in
          (trap_memory_write (Memory.set dst v))) ~init:(Machine.return()) s in
      cont' >>= fun () ->
         let n = String.length s in
         Value.of_word (Bitvector.of_int ~width:64 0) >>= fun z ->
         let dst = (Bitvector.add addr' (Bitvector.of_int ~width:64 n)) in
         (* let () = info "writing 0 into %x" (Bitvector.to_int_exn dst) in *)
         (trap_memory_write (Memory.set dst z))

    (** Copy an 8 byte integer into an address. *)
    let copy_int addr x =
      (* let () = info "Writing %d into address %x" x (Bitvector.to_int_exn addr) in *)
      let rec loop addr x i =
        if i = 0 then
          Machine.return()
        else
          let v = x land 0xff in
          Value.of_word (Bitvector.of_int ~width:8 v) >>= fun v' ->
          trap_memory_write (Memory.set addr v') >>= fun () ->
            loop (Bitvector.succ addr) (x lsr 8) (pred i) in
      loop addr x 8

    (** Copy an 8 byte integer into an address. *)
    let copy_int64 addr x =
      let open Int64 in
      (* let () = info "Writing %d into address %x" x (Bitvector.to_int_exn addr) in *)
      let rec loop addr x i =
        if i = 0L then
          Machine.return()
        else
          let v = x land 0xffL in
          Value.of_word (Bitvector.of_int64 ~width:8 v) >>= fun v' ->
          trap_memory_write (Memory.set addr v') >>= fun () ->
            loop (Bitvector.succ addr) (x lsr 8) (pred i) in
      loop addr x 8L

    (** Read an 8 byte integer from an address. *)
    let read_int addr =
      (* let () = info "Reading from address %x" (Bitvector.to_int_exn addr) in *)
      let rec loop addr x i =
        if i = 8 then
          Machine.return(x)
        else
          allow_all_memory_access (Memory.get addr) >>= fun v' ->
            let b = v' |> Value.to_word |> Bitvector.to_int_exn in
            (* let () = info "byte: %x" b in *)
            loop (Bitvector.succ addr) (x lor (b lsl (i * 8))) (succ i) in
      loop addr 0 0

    (** Copy an integer into an address. *)
    let copy_int_width width addr x =
      (* let () = info "Writing %d into address %x" x (Bitvector.to_int_exn addr) in *)
      let rec loop addr x i =
        if i = 0 then
          Machine.return()
        else
          let v = x land 0xff in
          Value.of_word (Bitvector.of_int ~width:8 v) >>= fun v' ->
          trap_memory_write (Memory.set addr v') >>= fun () ->
            loop (Bitvector.succ addr) (x lsr 8) (pred i) in
      loop addr x width
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
        (** let () = info "str %s %s" str' fmt' in
        let port = (((Scanf.sscanf str') "port: %d") (fun i -> i)) in
        let () = info "port: %d" port in *)
        nil
end

module Scanf(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    (** Write a bitvector into an address *)
    let write_bitvector addr x =
      (**
       let () = info "writing %x into %x"
        (Bitvector.to_int_exn x) (Bitvector.to_int_exn addr) in *)
      let width = 8 in
      let rec loop n addr x p =
        if n = width then
          p
        else
          let byte = Bitvector.logand x (Bitvector.of_int ~width:64 0xff) in
          (** let () = info " byte %d %x at %x" n (Bitvector.to_int_exn byte) (Bitvector.to_int_exn addr) in *)
          let cont =
            Value.of_word byte >>= fun v ->
            (trap_memory_write (Memory.set addr v)) >>= fun () ->
            p >>= fun () ->
            Machine.return() in
          let x' = Bitvector.rshift x (Bitvector.of_int ~width:64 8) in
          loop (succ n) (Bitvector.succ addr) x' cont in
      loop 0 addr x (Machine.return())

    let run [fmt] =
      (* let () = info "running scanf!" in *)
      (** let vaddr = Value.to_word addr in *)
      let vfmt = Value.to_word fmt in
      string_of_addr vfmt >>= fun fmt' ->
        (** let chan = In_channel.stdin in *)
        let line = In_channel.input_line_exn (In_channel.stdin) in
        let toks = line |>
                   String.split ~on:' ' |>
                   List.filter ~f:(fun s -> String.equal s "") in
        let ftoks = fmt' |>
                    String.split ~on:' ' |>
                    List.filter ~f:(fun s -> String.equal s "") in
        let assignments = (List.zip_exn ftoks toks) in
        let n = String.length fmt' in
        let values = List.map ~f:(fun (fmt, tok) ->
          let tok' = if n > 2 then
                       let fmt' = String.slice fmt 1 (pred (pred n)) in
                       let opt = int_of_string_opt fmt' in
                       let n' = String.length tok in
                         match opt with
                           None -> tok
                         | Some limit -> String.slice tok 0 (min limit n')
                     else
                       tok in
          let flag = String.get fmt (pred n) in
          flag |> function
            'd' -> Number (int_of_string tok')
          | 'u' -> Number (int_of_string tok')
          | 's' -> String tok'
          | _ -> Number 0
        ) assignments in
        let args' = List.take (List.drop arg_regs 1) (List.length ftoks) in
        let assignments = List.zip_exn values args' in
        List.fold_left ~f:(fun tot (value, dest) ->
          tot >>= fun tot' ->
          let tot'' = succ tot' in
          let var = (Var.create dest reg64_t) in
          Env.get var >>= fun v ->
            let dest' = Value.to_word v in
            match value with
              Number x -> copy_int_width 4 dest' x >>= fun _ ->
                          Machine.return tot''
            | String s -> copy_bytes v s >>= fun _ ->
                          Machine.return tot''
         ) ~init:(Machine.return 0) assignments >>= fun total ->
           Value.of_word (Bitvector.of_int ~width:64 total)
end

(**
  Communicating the number of test cases available for micro-execution.
*)
module NetworkTestCases(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let run [] =
      Machine.Local.get state >>= fun s ->
        let {no_test_cases} = s in
        (Value.of_int ~width:64 no_test_cases)

end

(**
  Add a socket to uPolicyCraft's internal state.
*)
module AddSocket(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let run [fd] =
      let fd' = int_of_value fd in
      Machine.Local.update state ~f:(fun s ->
        let {files} = s in
        let () = Hashtbl.add_exn files ~key:fd' ~data:"socket" in
        s
     ) >>= fun _ ->
       zero

end

(**
  A debugging module for tracing the values of LISP modules.
*)
module Debug(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let run [v] =
      let v' = Value.to_string v in
      (* let () = info "primus-debug: %s" v' in *)
      nil
end

(**
  A debugging module for tracing the values of LISP modules.
*)
module Dump(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let run [addr; n] =
      let addr' = Value.to_word addr in
      let n' = int_of_value n in
      dump_memory addr' n' >>= fun _ ->
        nil
end

(**
  Connect the file descriptor representing network traffic
  with the port the program binds to.
*)
module Network(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let run [bfd; fd] =
      let bfd' = int_of_value bfd in
      let fd' = int_of_value fd in
      Machine.Local.update state ~f:(fun s ->
        let port = Hashtbl.find_exn s.ports bfd' in
        let () = Hashtbl.set s.ports ~key:fd' ~data:port in
        s
      ) >>= fun () ->
      nil
end

(**
  TODO: Merge Sprintf and Snprintf
*)
module Sprintf(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let find_subs str =
      let cs = String.to_list str in
      let rec loop cs control acc =
        match cs with
          [] -> List.rev acc
        | c :: css ->
          if phys_equal c '%' then
            loop css true acc
          else if control then
            loop css false (("%" ^ (String.make 1 c)) :: acc)
          else
            loop css false acc in
       loop cs false []

    let fetch_decimal v =
      let open Param in
      let v' =
          if (get symbolic_arguments) then
            "[0-9]+"
          else
            v |> Value.to_word
              |> Bitvector.to_int_exn
              |> string_of_int in
      Machine.return (v')

    let fetch_string v =
      let v' = Value.to_word v in
        string_of_addr v'

    let run [s; fmt] =
      let open Param in
      let vfmt = Value.to_word fmt in
      string_of_addr vfmt >>= fun fmt' ->
        (* let () = info "sprintf: %s" fmt' in *)
        let subs = find_subs fmt' in
        let args' = List.take (List.drop arg_regs 2) (List.length subs) in
        let subs = List.zip_exn subs args' in
        List.fold_left ~f:(fun output (pattern, reg) ->
          let var = (Var.create reg reg64_t) in
          Env.get var >>= fun v ->
            (pattern |> function
              "%d" -> fetch_decimal v
            | "%u" -> fetch_decimal v
            | "%s" -> fetch_string v
            |  _ -> Machine.return "nil") >>= fun vs ->
            output >>= fun s ->
            Machine.return (String.substr_replace_first s ~pattern:pattern ~with_:vs)
        ) ~init:(Machine.return fmt') subs >>= fun output' ->
        copy_bytes s output' >>= fun () ->
          Value.of_word (Bitvector.of_int ~width:64 0)
end

module Snprintf(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    type printformat =
    | Decimal
    | String

    let decimal_pattern = String.Search_pattern.create "%d"
    let string_pattern = String.Search_pattern.create "%s"

    let fetch_decimal fmt reg =
      let open Param in
      (**
      The value for %d may be non-deterministic so just replace it with a regex
      that the MIDS can enforce.
      *)
      let var = (Var.create reg reg64_t) in
      (Env.get var) >>= fun v ->
        let v' =
          if (get symbolic_arguments) then
            "[0-9]+"
          else
            v |> Value.to_word
              |> Bitvector.to_int_exn
              |> string_of_int in
        let fmt' = String.Search_pattern.replace_first ~pos:0 decimal_pattern ~in_:fmt ~with_:v' in
        Machine.return(fmt')

    let fetch_string fmt reg =
       let var = (Var.create reg reg64_t) in
       (Env.get var) >>= fun addr ->
         let addr' = Value.to_word addr in
         string_of_addr addr' >>= fun str ->
           (* let () = info "Found %s from %s" str reg in *)
           let fmt' = String.Search_pattern.replace_first ~pos:0 string_pattern ~in_:fmt ~with_:str in
           Machine.return(fmt')

    let run [s; sz; fmt] =
      let open Param in
      let regs = ["RCX"; "R8"; "R9"] in
      let vfmt = Value.to_word fmt in
      string_of_addr vfmt >>= fun fmt' ->
        (* let () = info "Picked up %s from memory" fmt' in *)
        let decimals = fmt' |>
                       String.substr_index_all ~may_overlap:false ~pattern:"%d" |>
                       List.map ~f:(fun i -> (Decimal, i)) in
        let strings = fmt' |>
                      String.substr_index_all ~may_overlap:false ~pattern:"%s" |>
                      List.map ~f:(fun i -> (String, i)) in
        let replacements = List.sort ~compare:(fun (_, x) (_, y) -> compare x y) (List.concat [decimals;strings]) in
        (** Accumulate the formatted string in res. *)
        List.foldi ~init:(Machine.return(fmt')) ~f:(fun i res (ty, _) ->
            (** Only support 3 arguments now based on the three registers used. *)
            if i >= 3 then
              res
            else
              res >>= fun fmt' ->
                let reg = List.nth_exn regs i in
                match ty with
                | Decimal -> fetch_decimal fmt' reg
                | String -> fetch_string fmt' reg
        ) replacements >>= fun fmt' ->
          (**
          let () = info "Writing %s to memory" fmt' in
          *)
          copy_bytes s fmt' >>= fun () ->
            Value.of_word (Bitvector.of_int ~width:64 0)
end

let address_of_pos addr =
  match Bitvector.to_int addr with
    Error s -> -1
  | Ok v -> v

module StatPre(Machine : Primus.Machine.S) = struct
   [@@@warning "-P"]
   include Pre(Machine)
   include Channels.Lib(Machine)

   let nlink_offset = 16
   let size_offset = 48
   let mode_offset = 24
   let uid_offset = 28

   let stat_file state redirections filename buf =
     Machine.Local.get lisp_io_state >>= fun io_state ->
     let {filesystem;uid} = state in
     (** let opt = String.Map.find redirections filename in *)
     let (absolute_path, opt) = find_path io_state filename in
     match opt with
        None ->
          (* let () = info "Could not find file=%s" filename in *)
          error
      | Some hostfile ->
        (* let () = info "Calling stat on %s" hostfile in *)
        let buf' = Caml_unix.stat hostfile in
        let buf'' = (Value.to_word buf) in
        copy_int (Bitvector.add buf'' (Bitvector.of_int ~width:8 size_offset)) buf'.st_size >>= fun _ ->
          let reg_file = 0o100000 in
          let dir_file = 0o40000 in
          let nlink = 1 in
          let mode = match buf'.st_kind with
                      S_REG -> reg_file
                    | S_DIR -> dir_file
                    | _ -> 0 in
          copy_int (Bitvector.add buf'' (Bitvector.of_int ~width:8 mode_offset)) mode >>= fun _ ->
            copy_int (Bitvector.add buf'' (Bitvector.of_int ~width:8 uid_offset)) uid >>= fun _ ->
              copy_int (Bitvector.add buf'' (Bitvector.of_int ~width:8 nlink_offset)) nlink >>= fun _ ->
                ok
end

module FStat(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include StatPre(Machine)

    (** TODO: Re-structure to simplify the search for the file descriptor's domain
        (File or Socket). *)
    let run [fd; buf] =
      Machine.Local.get lisp_io_state >>= fun io_state ->
        let files = io_state.files in
        Machine.Local.get state >>= fun state ->
          (** Consult the mapping for the true file and fstat that. *)
          let {ports} = state in
          let fd' = fd |>
                    int_of_value |>
                    check_copied_descriptor state in
          (* let () = info "Running fstat on fd=%d" fd' in *)
          let opt = Map.find files fd' in
          let buf'' = (Value.to_word buf) in
          match opt with
            None ->
              let opt = Hashtbl.find ports fd' in
              (match opt with
                None ->
                  (* let () = info "Could not find fd=%d in either files or ports" fd' in *)
                  zero
              | Some port ->
                let socket_file = 0o0140000 in
                  copy_int (Bitvector.add buf'' (Bitvector.of_int ~width:8 mode_offset)) socket_file >>= fun _ ->
                    zero)
          | Some filename -> (stat_file state io_state.redirections filename buf)

end

module Stat(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include StatPre(Machine)

    let run [filename; buf] =
      Machine.Local.get lisp_io_state >>= fun io_state ->
        filename |> Value.to_word |> string_of_addr >>= fun filename' ->
        Machine.Local.get state >>= fun state -> (stat_file state io_state.redirections filename' buf)
end

module GetUid(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let run [] =
      Machine.Local.get state >>= fun {uid} ->
        Value.of_word (Bitvector.of_int ~width:64 uid)
end

module UpcRand(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let run [] =
        Value.of_word (Bitvector.of_int ~width:64 (Random.int 32767))
end


module UpcSqrt(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let word_of_float x = Word.of_int64 (Int64.bits_of_float x)

    let run [x] =
        let x' = x |> Value.to_word |> Bitvector.to_int64_exn |> Int64.float_of_bits in
        Value.of_word (word_of_float (sqrt x'))
end

module UpcLog(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let word_of_float x = Word.of_int64 (Int64.bits_of_float x)

    let run [x] =
        let x' = x |> Value.to_word |> Bitvector.to_int64_exn |> Int64.float_of_bits in
        Value.of_word (word_of_float (log x'))
end

module UpcRound(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let word_of_float x = Word.of_int64 (Int64.bits_of_float x)

    let run [x] =
        let x' = x |> Value.to_word |> Bitvector.to_int64_exn |> Int64.float_of_bits in
        Value.of_word (word_of_float (round x'))
end

module UpcFloor(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let word_of_float x = Word.of_int64 (Int64.bits_of_float x)

    let run [x] =
        let x' = x |> Value.to_word |> Bitvector.to_int64_exn |> Int64.float_of_bits in
        Value.of_word (word_of_float (Float.round_down x'))
end


module Dup2(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let run [oldfd; newfd] =
      let oldfd' = int_of_value oldfd in
      let newfd' = int_of_value newfd in
      Machine.Local.get state >>= fun {duped_file_descriptors} ->
        Hashtbl.set duped_file_descriptors ~key:newfd' ~data:oldfd';
        zero
end

module CheckDup2(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let run [fd] =
      let fd' = int_of_value fd in
      Machine.Local.get state >>= fun state ->
        Value.of_word (Bitvector.of_int ~width:64 (check_copied_descriptor state fd'))
end

module InetAton(Machine : Primus.Machine.S) = struct
    [@@@warning "-P"]
    include Pre(Machine)

    let run [ipaddr;sin] =
      let ipaddr' = Value.to_word ipaddr in
      let sin' = Value.to_word sin in
      Machine.Local.get state >>= fun state' ->
        string_of_addr ipaddr' >>= fun ip ->
          let ipnum = ip |>
                      String.split ~on:'.' |>
                      List.rev |>
                      List.fold_left ~f:(fun num byte ->
                        ((num lsl 8) lor (int_of_string byte))) ~init:0 in
          copy_int sin' ipnum >>= fun () ->
            ipnum |>
            Bitvector.of_int ~width:64 |>
            Value.of_word

end

(**

  Towards an ABI module for fetching arguments without having to
  think about the host ISA.

module ABI = struct
  type t = Value Machine.t

  let args x =
    let register = register_of_position x in

  let regs = ["RDI"; "RSI"; "RDX"; "RCX"; "R8"; "R9"]

end

*)

let out = std_formatter

module Monitor(Machine : Primus.Machine.S) = struct
  include Pre(Machine)

  module Eval = Primus.Interpreter.Make(Machine)

(**
  module Env = Primus.Interpreter.Make(Machine)
  module Memory = Primus.Memory.Make(Machine)
  module Value = Primus.Value.Make(Machine)
*)
  let failf = Lisp.failf

  (** For adding variables to Primus LISP (if needed) *)
  let set_var name x =
    let t = Type.imm (Word.bitwidth x) in
    let var = Var.create name t in
    Value.of_word x >>= fun v ->
    Env.set var v

  let negone = Value.one 8
  let zero = Value.zero 8

  let value_to_int x = Value.to_word x |> Word.to_int

  let make_static_generator x = match Word.to_int x with
    | Ok x -> Ok (Some (Primus.Generator.static x))
    | _ -> Or_error.errorf "memory-allocate: fill in value must fit into int"

  let is_nil x = Word.equal Word.b0 (Value.to_word x)

  let make_uniform_generator ~min ~max =
    let nomin = is_nil min and nomax = is_nil max in
    if nomin && nomax then Ok None
    else match value_to_int  min, value_to_int max with
      | Ok min, Ok max ->
        let min = if nomin then None else Some min
        and max = if nomax then None else Some max in
        let g = Primus.Generator.Random.Seeded.lcg ?min ?max () in
        Ok (Some g)
      | _ -> Or_error.errorf "memory-allocate: random values do not fit into int"

  let memory_map addr size rest =
    let module Memory = Primus.Memory.Make(Machine) in
    let gen = match rest with
      | []  -> Ok None
      | [x] -> make_static_generator (Value.to_word x)
      | [min; max] -> make_uniform_generator ~min:min ~max:max
      | _ -> Or_error.errorf "bad generator" in
    if Result.is_error gen
    then failf "memory-map: bad generator specification" ()
    else
      let generator = Or_error.ok_exn gen in
      let lower = Value.to_word addr in
      Memory.allocate ?generator lower (int_of_value size) >>= fun () ->
      zero

  let memory_allocate size rest =
    Machine.Local.get state >>= fun {top_of_heap} ->
      value_of_int64 top_of_heap >>= fun top_of_heap' ->
      memory_map top_of_heap' size rest >>= fun status ->
        Machine.Local.update state ~f:(fun s ->
          let open Core.Int64 in
          {s with top_of_heap= top_of_heap + int64_of_value(size)}
        ) >>= fun _ ->
          Machine.return top_of_heap'

  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec
  open Machine.Syntax

  let find_symbolic_variables = object
    inherit [Var.Set.t] Term.visitor
    method! enter_def def defined =
      let v = def |> Def.lhs in
      Var.Set.add defined v
  end

  let find_basic_blocks = object
    inherit [Blk.Set.t] Term.visitor
    method! enter_blk b bs =
    Blk.Set.add bs b
  end

  let record_pos p = Machine.current () >>= fun pid ->
    let () = info "recording position" in
    match (Primus.Pos.get address p) with
      None -> Machine.return ()
    | Some addr ->
      let a = address_of_pos addr in
      Machine.Local.get state >>= fun {exitpoint} ->
        let x = Bitvector.to_int_exn exitpoint in
        let () = info "\tvisiting tid: %x\n\t stopping at %x\n" a x in
        if a >= x then
          Machine.current () >>= fun pid ->
            Machine.kill pid >>= fun () ->
              Machine.switch Machine.global
        else
          let rsp = Var.create "RSP" reg64_t in
          Env.get rsp >>= fun v ->
            let () = info "\tRSP : %s" (Value.to_string v) in
            Machine.return ()

  (**
    Primus tracks block coverage, but this has been helpful for debugging.
  *)
  let record_blk blk =
    Machine.Global.update state ~f:(fun s ->
      let {visited_blks} = s in
      {s with visited_blks = Blk.Set.add visited_blks blk}
    )

    (**
    match (Primus.Pos.get address p) with
      None -> Machine.return ()
    | Some addr ->
      let a = address_of_pos addr in
      let () = info "\tvisiting %x\n" a in
        Machine.return () *)

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
          (** let () = info "  %s" (Bitvector.to_string v') in *)
          let shift = (Bitvector.of_int ~width:64 (n * 8)) in
          let next = (Bitvector.logor (Bitvector.lshift v' shift) x) in
          (** let () = info "  intermediate: %s" (Bitvector.to_string next) in *)
          Machine.return(next) in
        loop (succ n) (Bitvector.succ addr) cont in
    loop 0 addr (Machine.return(Bitvector.of_int ~width:64 0))

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
          (** let () = info "  %s" (Bitvector.to_string v') in *)
          let shift = (Bitvector.of_int ~width:64 (n * 8)) in
          let next = (Bitvector.logor (Bitvector.lshift v' shift) x) in
          (** let () = info "  intermediate: %s" (Bitvector.to_string next) in *)
          Machine.return(next) in
        loop (succ n) (Bitvector.succ addr) cont in
    loop 0 addr (Machine.return(Bitvector.of_int ~width:64 0))

  (** Always ensure a path ends with / *)
  let sanitize_absolute_path path =
    let n = String.length path in
    if not (phys_equal path.[n-1] '/') then
      path ^ "/"
    else
      path

  let string_of_addr addr =
    let rec loop addr cs =
      (** let () = info "Fetching %s" (Bitvector.to_string addr) in *)
      (allow_all_memory_access (Memory.get addr)) >>= fun v ->
      let x = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (** let () = info "  Fetched %d" x  *)
      if x = 0 then
        let s = (cs |> List.rev |> String.of_char_list) in
        (** let () = info "  %s" s in *)
        Machine.return (s)
      else
        let c = Char.of_int_exn x in
        loop (Bitvector.succ addr)  (c :: cs) in
    loop addr []

  let strings_of_addr addr =
    (** let () = info "Finding strings at %s" (Bitvector.to_string addr) in *)
    let rec loop addr strings =
      read_address addr >>= fun v ->
      (** let () = info "Read address %s" (Bitvector.to_string v) in *)
      if phys_equal v (Bitvector.zero 64) then
        strings
      else
        let cont = strings >>= fun xs ->
          (string_of_addr v) >>= fun x ->
          Machine.return(x :: xs) in
        loop (Bitvector.add addr (Bitvector.of_int ~width:64 8)) cont in
    loop addr (Machine.return([]))

  let read_from_stdin tid =
      Machine.Local.get lisp_io_state >>= fun io_state ->
      Machine.Local.update state ~f:(fun state' ->
          let op = Read Sf.stdin_fd in
          (add_operation tid op state' io_state))

  let read_from_stdin tid =
    Machine.Local.get lisp_io_state >>= fun io_state ->
    Machine.Local.update state ~f:(fun state' ->
      let op = (Read Sf.stdin_fd) in
      (add_operation tid op state' io_state)
    )

  let write_to_stdout tid =
    Machine.Local.get lisp_io_state >>= fun io_state ->
    Machine.Local.update state ~f:(fun state' ->
      let op = (Write Sf.stdout_fd) in
      (add_operation tid op state' io_state)
    )

  (**
    The following snippet can be helpful to fetch concrete variables for
    executions that crash the plugin. Just insert the code snippet around the
    problematic code.

    Machine.Global.get state >>= fun s ->
    let () = info "concrete vars: %d" (Var.Set.length s.written_vars) in
  *)


  (**
    Record a function call within the effect graph.
  *)
  let record_function tid func =
    let () = info "recording function: %s" func in
    Machine.Local.get lisp_io_state >>= fun io_state ->
    match func with
      "fmt.Println" ->
        (write_to_stdout tid)
    | "syscall" ->
      Machine.args >>= fun args ->
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      Machine.Local.update state ~f:(fun state' ->
        let syscall_number = int_of_value v in
        (* let () = info "syscall no. %d" syscall_number in *)
        match syscall_number with
          56 (* clone *) ->
            let op = (Clone (Array.to_list args)) in
            add_operation tid op state' io_state
        | _ -> state'
      )
    | "mkdir" ->
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun u ->
      let rsi = (Var.create "RSI" reg64_t) in
      (Env.get rsi) >>= fun v ->
      let pathaddr = Value.to_word u in
      let mode = int_of_value v in
      string_of_addr pathaddr >>= fun path ->
        Machine.Local.update state ~f:(fun state' ->
          let op = Mkdir (path, mode) in
          (add_operation tid op state' io_state))
    | "connect" ->
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun u ->
      let fd = int_of_value u in
      let rsi = (Var.create "RSI" reg64_t) in
      (Env.get rsi) >>= fun v ->
      let port_offset = 2 in
      let addr_offset = 4 in
      let sockaddr = (v |> Value.to_word) in
      let portaddr = Bitvector.nsucc sockaddr port_offset in
      let remoteaddr = Bitvector.nsucc sockaddr addr_offset in
      read_number portaddr 2 >>= fun port ->
      read_number remoteaddr 4 >>= fun ipaddr ->
        let port' = Bitvector.to_int_exn port in
        let ipaddr' = Bitvector.to_int_exn ipaddr in
        (* let () = info "Connecting to ip address %x" ipaddr' in *)
        Machine.Local.update state ~f:(fun state' ->
          let op = Connect (fd, ipaddr', port') in
          (add_operation tid op state' io_state))
    | "chdir" ->
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      (v |> Value.to_word |> string_of_addr) >>= fun s ->
      (* let () = info "chdir to %s" s in *)
      Machine.Local.update state ~f:(fun state' ->
        let {cwd} = state' in
        let s' = if (String.is_prefix ~prefix:"/" s) then
                   sanitize_absolute_path s
                 else if phys_equal s ".." then
                   let parts = String.split ~on:'/' cwd in
                   let n = List.length parts in
                   let () = Printf.printf "parts: %d\n" n in
                   if n <= 2 then
                     "/"
                   else
                     let cwd' = (n - 2) |>
                       List.take parts |>
                       String.concat ~sep:"/" in
                       cwd' ^ "/"
                 else
                   cwd ^ s ^ "/" in
        { state' with cwd = s' }
      ) >>= fun _ ->
      Machine.Local.get state >>= fun s ->
        let {cwd} = s in
        Machine.Local.update lisp_io_state ~f:(fun state' ->
          { state' with cwd = cwd }
        )
    | "fchown" ->
      let rdi = (Var.create "RDI" reg64_t) in
      let rsi = (Var.create "RSI" reg64_t) in
      let rdx = (Var.create "RDX" reg64_t) in
      (Env.get rdi) >>= fun t ->
      let fd = (t |> Value.to_word |> Bitvector.to_int_exn) in
      (Env.get rsi) >>= fun u ->
      let user_id = (u |> Value.to_word |> Bitvector.to_int_exn) in
      (Env.get rdx) >>= fun v ->
      let group_id = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
        let op = Chown (fd, user_id, group_id) in
        (add_operation tid op state' io_state))
    | "fchmod" ->
      let rdi = (Var.create "RDI" reg64_t) in
      let rsi = (Var.create "RSI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (Env.get rsi) >>= fun v ->
      let mode = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
        let op = Chmod (fd, mode) in
        (add_operation tid op state' io_state))
    | "fork" ->
      Machine.args >>= fun args ->
      Machine.Local.update state ~f:(fun state' ->
        let op = (Clone (Array.to_list args)) in
        add_operation tid op state' io_state)
    | "tmpfile" ->
      Machine.Local.update state ~f:(fun state' ->
        let path = "/tmp/tmpfile" in
        let op = (Open path) in
            (add_operation tid op state' io_state))
    | "execle" ->
      let rec collect_args regs args =
        match regs with
          [] -> Machine.return (List.rev args)
        | r :: rs ->
          let reg = (Var.create r reg64_t) in
          (Env.get reg) >>= fun v ->
            let v' = int_of_value v in
            if v' = 0 then
              Machine.return (List.rev args)
            else
              let word = Value.to_word v in
              string_of_addr word >>= fun s ->
                collect_args rs (s :: args) in
       collect_args arg_regs [] >>= fun argv ->
         let path = List.nth_exn argv 0 in
         let args = String.concat ~sep:"," argv in
         Machine.Local.update state ~f:(fun state' ->
           let op = (Exec [path; args]) in
           (add_operation tid op state' io_state))
    | "execv" ->
      (** let () = info "model execv:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      let rsi = (Var.create "RSI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      (v |> Value.to_word |> string_of_addr) >>= fun s ->
      (Env.get rsi) >>= fun u ->
      (u |> Value.to_word |> strings_of_addr) >>= fun ss ->
      let path = s in
      let argv = (String.concat ~sep:"," ss) in
      (** let () = info " RDI: %s" path in *)
      (** let () = info " RSI: %s" argv in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Exec [path; argv]) in
          (add_operation tid op state' io_state))
    | "apr_file_open" ->
      (** let () = info "model open:" in *)
      let rsi = (Var.create "RSI" reg64_t) in
      (Env.get rsi) >>= fun v ->
        (v |> Value.to_word |> string_of_addr) >>= fun path ->
        (** let () = info " RSI: %s" path in *)
        Machine.Local.update state ~f:(fun state' ->
            let op = (Open path) in
            (add_operation tid op state' io_state))
    | "apr_file_gets" ->
      (** let () = info "model read:" in *)
      let rsi = (Var.create "RSI" reg64_t) in
      let rdx = (Var.create "RDX" reg64_t) in
      (Env.get rsi) >>= fun u ->
        (** let () = info " RSI: %d" (u |> Value.to_word |> Bitvector.to_int_exn) in *)
      (Env.get rdx) >>= fun v ->
        let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
        (** let () = info " RDX: %d" fd in *)
        Machine.Local.update state ~f:(fun state' ->
            let op = (Read fd) in
            (add_operation tid op state' io_state))
    | "atoi" ->
      (** let () = info "model atoi:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun u ->
        (** let () = info " RDI: %d" (u |> Value.to_word |> Bitvector.to_int_exn) in *)
        (u |> Value.to_word |> string_of_addr) >>= fun str ->
        (** let () = info "  atoi: %s" str in *)
        Machine.return()
    | "fopen" ->
      (** let () = info "model fopen:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      (v |> Value.to_word |> string_of_addr) >>= fun path ->
      (** let () = info " RDI: %s" path in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Open path) in
          let state'' = {state' with file_opened=Some path} in
          (add_operation tid op state'' io_state))
    | "fopen64" ->
      (** let () = info "model fopen:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      (v |> Value.to_word |> string_of_addr) >>= fun path ->
      (** let () = info " RDI: %s" path in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Open path) in
          let state'' = {state' with file_opened=Some path} in
          (add_operation tid op state'' io_state))
    | "open" ->
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      (v |> Value.to_word |> string_of_addr) >>= fun path ->
      (** let () = info " RDI: %s" path in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Open path) in
          let state'' = {state' with file_opened=Some path} in
          (add_operation tid op state'' io_state))
    | "open64" ->
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      (v |> Value.to_word |> string_of_addr) >>= fun path ->
      (** let () = info " RDI: %s" path in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Open path) in
          let state'' = {state' with file_opened=Some path} in
          (add_operation tid op state'' io_state))
    | "opendir" ->
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      (v |> Value.to_word |> string_of_addr) >>= fun path ->
      (** let () = info " RDI: %s" path in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Open path) in
          let state'' = {state' with file_opened=Some path} in
          (add_operation tid op state'' io_state))
    | "fgetc" ->
      (** let () = info "model fgetc:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (** let () = info " RDI: %d" fd in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Read fd) in
          (add_operation tid op state' io_state))
    | "_IO_getc" ->
      (** let () = info "model _IO_getc:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (** let () = info " RDI: %d" fd in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Read fd) in
          (add_operation tid op state' io_state))
    | "pread64" ->
      (** let () = info "model fwrite:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (** let () = info " RCX: %d" fd in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Read fd) in
          (add_operation tid op state' io_state))
    | "fgets" ->
      (** let () = info "model fgets:" in *)
      let rdi = (Var.create "RDX" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (** let () = info " RDX: %d" fd in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Read fd) in
          (add_operation tid op state' io_state))
    | "fread" ->
      (** let () = info "model fwrite:" in *)
      let rcx = (Var.create "RCX" reg64_t) in
      (Env.get rcx) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (** let () = info " RCX: %d" fd in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Read fd) in
          (add_operation tid op state' io_state))
    | "fprintf" ->
      (** let () = info "model fprintf:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (** let () = info " RDI: %d" fd in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Write fd) in
          (add_operation tid op state' io_state))
    | "fputs" ->
      (** let () = info "model fputs:" in *)
      let rdi = (Var.create "RSI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (** let () = info " RSI: %d" fd in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Write fd) in
          (add_operation tid op state' io_state))
    | "fwrite" ->
      (** let () = info "model fwrite:" in *)
      let rcx = (Var.create "RCX" reg64_t) in
      (Env.get rcx) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (** let () = info " RCX: %d" fd in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Write fd) in
          (add_operation tid op state' io_state))
    | "pwrite64" ->
      (** let () = info "model fwrite:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (** let () = info " RCX: %d" fd in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Write fd) in
          (add_operation tid op state' io_state))
    | "fclose" ->
      (** let () = info "model fclose:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (** let () = info " RDI: %d" fd in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Close fd) in
          (add_operation tid op state' io_state))
     | "close" ->
      (** let () = info "model close:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (** let () = info " RDI: %d" fd in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Close fd) in
          (add_operation tid op state' io_state))
    | "closedir" ->
      (** let () = info "model closedir:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (** let () = info " RDI: %d" fd in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = (Close fd) in
          (add_operation tid op state' io_state))
    | "__isoc99_scanf" ->
      (read_from_stdin tid)
    | "scanf" ->
      (read_from_stdin tid)
   (** Simplified for CGC challenges *)
    | "write0" ->
      (write_to_stdout tid)
    | "print" ->
      (write_to_stdout tid)
    | "put" ->
      (write_to_stdout tid)
    | "puts" ->
      (write_to_stdout tid)
    | "printf" ->
      (** let () = info "model printf:" in *)
      (write_to_stdout tid)
    | "upc_log" ->
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      (v |> Value.to_word |> string_of_addr) >>= fun message ->
      (** let () = info " LOG: %s" message in *)
      Machine.return()
    | "upc_debug" ->
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let v' = Value.to_string v in
      (** let () = info " LOG-VALUE: %s" v' in *)
      Machine.return()
    | "bind" ->
      (** let () = info "model bind:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      let rsi = (Var.create "RSI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      (Env.get rsi) >>= fun u ->
      let sockaddr = (u |> Value.to_word) in
      let portaddr = Bitvector.nsucc sockaddr 0x2 in
      read_number portaddr 2 >>= fun v ->
      let port = Bitvector.to_int_exn v in
      (** let () = info " port %d" port in *)
      Machine.Local.update state ~f:(fun state' ->
          let op = Bind (fd, port) in
          let () = Hashtbl.set state'.ports ~key:fd ~data:port in
         (add_operation tid op state' io_state))
    | "accept" ->
      (** let () = info "model accept:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Accept fd in
          (add_operation tid op state' io_state))
    | "accept4" ->
      (** let () = info "model accept4:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Accept fd in
          (add_operation tid op state' io_state))
    | "readdir" ->
      (** let () = info "model readdir:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Read fd in
          (add_operation tid op state' io_state))
    | "read" ->
      (** let () = info "model read:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Read fd in
          (add_operation tid op state' io_state))
    | "write" ->
      (** let () = info "model write:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Write fd in
          (add_operation tid op state' io_state))
    | "readv" ->
      (** let () = info "model read:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Read fd in
          (add_operation tid op state' io_state))
    | "writev" ->
      (** let () = info "model write:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Write fd in
          (add_operation tid op state' io_state))
    | "recvmsg" ->
      (** let () = info "model recv:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Recv fd in
          (add_operation tid op state' io_state))
    | "sendmsg" ->
      (** let () = info "model send:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Send fd in
          (add_operation tid op state' io_state))
    | "setuid" ->
      (** let () = info "model setuid:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let id = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          (** let () = info "Saving uid: %d" id in *)
          let {uid} = state' in
          let op = Setuid uid in
          let state'' = {state' with saved_uid = Some id} in
          (add_operation tid op state'' io_state))
    | "setgid" ->
      (** let () = info "model setgid:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let id = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let {gid} = state' in
          let op = Setgid gid in
          let state'' = { state' with saved_gid = Some id } in
          (add_operation tid op state'' io_state))
    | "terminate" ->
      (** let () = info "model terminate:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let status = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Exit status in
          (add_operation tid op state' io_state))
    | "recvUntil" ->
      (** let () = info "model receive:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Read fd in
          (add_operation tid op state' io_state))
    | "receive_delim" ->
      (** let () = info "model receive_delim:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Read fd in
          (add_operation tid op state' io_state))
    | "receive" ->
      (** let () = info "model receive:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Read fd in
          (add_operation tid op state' io_state))
    | "getline" ->
      (** let () = info "model getline" in *)
      (read_from_stdin tid)
    | "receive_until0" ->
      (** let () = info "model receive_until:" in *)
      (read_from_stdin tid)
    | "receive_until" ->
      (** let () = info "model receive_until:" in *)
      (read_from_stdin tid)
    | "receive_bytes" ->
      (** let () = info "model receive_bytes:" in *)
      (read_from_stdin tid)
    | "transmit_all" ->
      (** let () = info "model transmit:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Write fd in
          (add_operation tid op state' io_state))
    | "transmit" ->
      (** let () = info "model transmit:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      Machine.Local.update state ~f:(fun state' ->
          let op = Write fd in
          (add_operation tid op state' io_state))
    | "allocate" ->
      (** let () = info "model allocate:" in *)
      let rdi = (Var.create "RDI" reg64_t) in
      let rsi = (Var.create "RSI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      (Env.get rsi) >>= fun u ->
        let size = (v |> Value.to_word |> Bitvector.to_int_exn) in
        let isX = (u |> Value.to_word |> Bitvector.to_int_exn) in
        let perms = if isX > 0 then [R; W; X] else [R; W] in
        Machine.Local.update state ~f:(fun state' ->
          let op = Mmap (size, perms) in
          (add_operation tid op state' io_state))
    | "getcwd" ->
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      Machine.Local.get state >>= fun state ->
      let {cwd} = state in
      copy_bytes v cwd
    | "sendfile" ->
      let rdi = (Var.create "RDI" reg64_t) in
      let rsi = (Var.create "RSI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      (Env.get rsi) >>= fun u ->
        let out_fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
        let in_fd = (u |> Value.to_word |> Bitvector.to_int_exn) in
        Machine.Local.update state ~f:(fun state' ->
          let op = Sendfile (out_fd, in_fd) in
          (add_operation tid op state' io_state))
    | "sendfile64" ->
      let rdi = (Var.create "RDI" reg64_t) in
      let rsi = (Var.create "RSI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      (Env.get rsi) >>= fun u ->
        let out_fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
        let in_fd = (u |> Value.to_word |> Bitvector.to_int_exn) in
        Machine.Local.update state ~f:(fun state' ->
          let op = Sendfile (out_fd, in_fd) in
          (add_operation tid op state' io_state))
    | "io_ioutil.ReadFile" ->
      (* Append additional TIDs for the read and close. *)
      let rsi = (Var.create "RSI" reg64_t) in
      let rbp = (Var.create "RBP" reg64_t) in
      (Env.get rsi) >>= fun v ->
        (v |> Value.to_word |> string_of_addr) >>= fun path ->
        (** let () = info "ReadFile is Opening %s" path in *)
        let contents = In_channel.read_all path in
        let count = (String.length contents) + 1 in
        let capacity = count in
        let baseaddr = 0x8000 in
        let countaddr = baseaddr + count in
          (Value.of_word (Bitvector.of_int ~width:64 baseaddr)) >>= fun base ->
          (Value.of_word (Bitvector.of_int ~width:64 (count+8))) >>= fun size ->
          (Value.of_word (Bitvector.of_int ~width:64 0x00)) >>= fun init ->
            let countvalue_target = (Bitvector.of_int ~width:64 countaddr) in
            memory_map base size [init] >>= fun status ->
            copy_bytes base contents >>= fun _ ->
            copy_int countvalue_target count >>= fun _ ->
            (Env.get rbp) >>= fun v' ->
              let v'' = Value.to_word v' in
              let contents_target = (Bitvector.add v'' (Bitvector.of_int ~width:64 (-0x88))) in
              let count_target = (Bitvector.add v'' (Bitvector.of_int ~width:64 (-0x80))) in
              let capacity_target = (Bitvector.add v'' (Bitvector.of_int ~width:64 (-0x78))) in
              copy_int contents_target baseaddr >>= fun _ ->
              copy_int count_target countaddr >>= fun _ ->
              copy_int capacity_target countaddr >>= fun _ ->
              Machine.Local.update state ~f:(fun state' ->
                let op = (Open path) in
                let state'' = {state' with file_opened=Some path} in
                (add_operation tid op state'' io_state))
    | "encoding_json.Unmarshal" ->
      let rsp = (Var.create "RSP" reg64_t) in
      Env.get rsp >>= fun v ->
        dump_memory (Value.to_word v) 768 >>= fun _ ->
        read_int (Bitvector.add (Value.to_word v) (Bitvector.of_int ~width:64 8)) >>= fun addr ->
          (** let () = info "*$rsp -> %x" addr in *)
          string_of_addr (Bitvector.of_int ~width:64 (addr land 0xffffff00)) >>= fun json ->
          (** let () = info "json: %s" json in *)
          let js = Yojson.Basic.from_string json in
          let port = js |> Yojson.Basic.Util.member "port" |> Yojson.Basic.Util.to_int in
          read_int (Bitvector.add (Value.to_word v) (Bitvector.of_int ~width:64 0x50)) >>= fun dest ->
            (** let () = info "Writing unmarshalled data to %x" dest in *)
            copy_int (Bitvector.of_int ~width:64 dest) port >>= fun _ ->
            Machine.return()
    | "fmt.Sprintf" ->
      let rax = (Var.create "RAX" reg64_t) in
      let rdi = (Var.create "RDI" reg64_t) in
      let rsp = (Var.create "RSP" reg64_t) in
      let r14 = (Var.create "R14" reg64_t) in
      Env.get rdi >>= fun v ->
        string_of_addr (Value.to_word v) >>= fun fmt ->
          (** let () = info "Sprintf %s" fmt in *)
          Env.get r14 >>= fun u ->
          let v1 = (int_of_value u) in
          (** let () = info "R14 %x" v1 in *)
           let v1s = Printf.sprintf "%d" v1 in
          let final = String.substr_replace_first fmt ~pattern:"%d" ~with_:v1s in
          let n = (String.length final) + 1 in
          let baseaddr = 0x9000 in
          (Value.of_word (Bitvector.of_int ~width:64 baseaddr)) >>= fun base ->
          (Value.of_word (Bitvector.of_int ~width:64 (n+1))) >>= fun size ->
          (Value.of_word (Bitvector.of_int ~width:64 0x00)) >>= fun init ->
          memory_map base size [init] >>= fun status ->
          Env.set rax base >>= fun _ ->
            copy_bytes base final >>= fun _ ->
            Machine.return()
    | "net_http.ListenAndServe" ->
      let rdi = (Var.create "RDI" reg64_t) in
      Env.get rdi >>= fun v ->
        string_of_addr (Value.to_word v) >>= fun protocol ->
          let port = String.drop_prefix protocol 1 in
          (** let () = info "listenAndServe %s" port in *)
          let port' = int_of_string port in
          let fd = 128 in
          Machine.Local.update state ~f:(fun state' ->
            let op = Accept (fd) in
            let () = Hashtbl.set state'.ports ~key:fd ~data:port' in
            (add_operation tid op state' io_state))
    | "net/http.(*Server).ListenAndServe.abiinternal" ->
      let port = 80 in
      let fd = 128 in
      Machine.Local.update state ~f:(fun state' ->
            let op = Accept (fd) in
            let () = Hashtbl.set state'.ports ~key:fd ~data:port in
            (add_operation tid op state' io_state))
    | "github.com/harlow/go-micro-services/dialer.Dial.abiinternal" ->
      (* Maintain a fresh IP for each host. *)
      let ipaddr = 0xc0a8012a in
      (* Without a port in the name Dial defaults to port 80 *)
      let port = 80 in
      let fd = 999 in
      Machine.Local.update state ~f:(fun state' ->
        let op = Connect (fd, ipaddr, port) in
        (add_operation tid op state' io_state))
    | "os.Open.abiinternal" ->
      let rax = (Var.create "RAX" reg64_t) in
      let rbx = (Var.create "RBX" reg64_t) in
      Env.get rax >>= fun v ->
        Env.get rbx >>= fun v' ->
        let () = info "Target of open given at %x" (int_of_value v) in
        let () = info "Length of open given at %x" (int_of_value v') in
        (** String is given by RAX, length of the string is stored in RBX. *)
        let n = int_of_value v' in
        string_of_addr_limit (Value.to_word v) n >>= fun path ->
           let () = info "path: %s" path in
           zero >>= fun nil ->
           Env.set rbx nil >>= fun _ ->
             Machine.Local.update state ~f:(fun state' ->
               let op = Open (path) in
               let state'' = {state' with file_opened=Some path} in
               (** let () = Hashtbl.set state'.ports ~key:fd ~data:port' in *)
               (add_operation tid op state' io_state))
    | "io/ioutil.ReadAll.abiinternal" ->
      let rdi = (Var.create "RDI" reg64_t) in
      (Env.get rdi) >>= fun v ->
      let fd = (v |> Value.to_word |> Bitvector.to_int_exn) in
      let () = info "reading fd: %d" fd in
      Machine.Local.update state ~f:(fun state' ->
          let op = Read fd in
          (add_operation tid op state' io_state))
    | _ ->
      (** let () = info "called %s from %s" func (Tid.name tid) in *)
      Machine.Local.update state ~f:(fun state' ->
        state'
      )

  let record_stmt stmt = Machine.current () >>= fun pid ->
    (**
    let s = Stmt.to_string stmt in
    let () = info "record statement: %s" s in *)
    Machine.return()

  (**
   * When modelling Go programs, the following is useful for detecting a function
   * preamble.
    if String.equal name "RSP" then
      Machine.Local.update state ~f:(fun s ->
        { s with read_rsp = true }
      )
    else if String.equal name "R14" then
      let r14 = x in
      Machine.Local.get state >>= fun s ->
      if s.read_rsp then
        let rsp = Var.create "RSP" reg64_t in
        Env.get rsp >>= fun v ->
        Env.set r14 v
      else
        Machine.return()
    else
      Machine.Local.update state ~f:(fun s ->
        { s with read_rsp = false }
      )
   **)

  let record_loading (x) =
    let x' = int64_of_value x in
    let () = info "Reading from address: 0x%Lx" x' in
    if phys_equal x' 0x24d1d8L then
      copy_int (Value.to_word x) 4096 >>= fun () ->
      Machine.return()
    else
      Machine.return()

  let record_reading (x) =
    (** TODO: Adjust the widths based on the chosen architecture. *)
    let name = Var.to_string x in
    let () = info "Reading from variable %s" name in
    match name with
       | "FS_BASE" ->
         Machine.Local.get state >>= fun s ->
           if not s.fs_set then
                  (Value.of_word (Bitvector.of_int ~width:64 0x1000)) >>= fun base ->
                  (Value.of_word (Bitvector.of_int ~width:64 0x100)) >>= fun size ->
                  (Value.of_word (Bitvector.of_int ~width:64 0x00)) >>= fun init ->
               memory_map base size [init] >>= fun status ->
             (** Allow negative indexes on the FS register. *)
             (Value.of_word (Bitvector.of_int ~width:64 0x1010)) >>= fun offset ->
             Env.set x offset >>= fun _ ->
             Machine.Local.update state ~f:(fun s ->
               { s with fs_set = true }
             )
           else
             Machine.return()
       | _ -> Machine.return()

  let allocate_slice (size) =
    (* A triple that acts as the slice's meta-data. *)
    let metadata_size = 0x18L in
    value_of_int64 size >>= fun size' ->
      value_of_int64 0x0L >>= fun init ->
        let open Int64 in
        let buffer = 0x4200L in
        let buffer' = (Bitvector.of_int64 ~width:64 (0x1000000000L + buffer)) in
          copy_int64 buffer' (Bitvector.to_int64_exn (Bitvector.add buffer' (Bitvector.of_int ~width:64 0x18))) >>= fun _ ->
          copy_int64 (Bitvector.add buffer' (Bitvector.of_int ~width:64 0x8)) (Bitvector.to_int64_exn (Bitvector.of_int ~width:64 1)) >>= fun _ ->
          copy_int64 (Bitvector.add buffer' (Bitvector.of_int ~width:64 0x10)) (Bitvector.to_int64_exn (Bitvector.of_int64 ~width:64 (size - metadata_size))) >>= fun _ ->
          (Value.of_word (Bitvector.of_int64 ~width:64 buffer))

   let golang_success () =
     let rax = (Var.create "RAX" reg64_t) in
       zero >>= fun z ->
       Env.set rax z >>= fun v ->
         Machine.return()

  let golang_pair_nil_error () =
     (* Assume the function returns a valid value and a nil for err (i.e., x, err := f() *)
     zero >>= fun nil ->
       let rbx = (Var.create "RBX" reg64_t) in
       Env.set rbx nil >>= fun _ ->
         Machine.return()

  let golang_pair_value (rv) =
     (* Assume the function returns the given value and a nil for err (i.e., v, err := f() *)
     rv >>= fun v ->
       let rax = (Var.create "RAX" reg64_t) in
       Env.set rax v >>= fun _ ->
       zero >>= fun nil ->
         let rbx = (Var.create "RBX" reg64_t) in
         Env.set rbx nil >>= fun _ ->
           Machine.return()

  (** This is helpful for obtaining function return values by observing what
      gets stored in RAX.

      For binaries with symbols that aren't valid C/C++ symbols (i.e., Golang), this is important
      for modeling external functions.

      It's also helpful for debugging how registers change during program
      execution.
  *)
  let record_written (x, v) =
    let name = Var.to_string x in
    let () = info "Variable %s <-- %s" name (Value.to_string v) in
    (* Intercept Golang functions here since the Primus LISP ABI doesn't
       recognize special characters in symbol names. *)
    let abi_result = "_result" in
    let rsp = Var.create "RSP" reg64_t in
    if String.equal name "RAX" then
      Machine.Local.update state ~f:(fun s ->
        let v' = (Value.to_word v) in
        { s with last_rax = v' :: s.last_rax }
      )
    else if String.is_suffix ~suffix:abi_result name then
      let function_name = String.chop_suffix_exn ~suffix:abi_result name in
      let () = info "Intercepting %s" function_name in
      Env.get rsp >>= fun v ->
        let v' = Value.to_word v in
        let v'' = (Bitvector.add v' (Bitvector.of_int ~width:64 0x18)) in
      (Value.of_word v'') >>= fun v ->
      Env.set rsp v >>= fun _ ->
        Machine.Local.get state >>= fun {callstack} ->
          let top = List.hd_exn (List.drop callstack 1) in
          (** let () = info "Examining return value for %s" top in *)
          match function_name with
          | "github\\.com/harlow/go-micro-services/dialer\\.Dial\\.abiinternal" ->
            (* Simualte a succesful dial. *)
            (Value.of_word (Bitvector.of_int ~width:64 0)) >>= fun nil ->
            let rbx = (Var.create "RBX" reg64_t) in
            Env.set rbx nil >>= fun _ ->
              Machine.return()
          | "gopkg\\.in/mgo%2ev2\\.(*Collection)\\.EnsureIndexKey\\.abiinternal" -> golang_success()
          | "gopkg\\.in/mgo%2ev2\\.(*Query)\\.Count\\.abiinternal" ->
            let count = (Value.of_word (Bitvector.of_int ~width:64 0x1)) in
              golang_pair_value(count)
          | "gopkg\\.in/mgo%2ev2\\.(*Collection)\\.Insert\\.abiinternal" ->
              golang_success()
          | "runtime\\.mapiternext\\.abiinternal" ->
            (* Write an empty map into memory. *)
            Machine.Local.get state >>= fun {last_rax} ->
              let _ :: rax :: rs = last_rax in
              let () = info "Writing into RAX -> %Lx" (Bitvector.to_int64_exn rax) in
                copy_int64 rax 0x0L >>= fun _ ->
                  Machine.return()
          | "github\\.com/uber/jaeger-client-go/config\\.Configuration\\.New" -> golang_pair_nil_error()
          | "unicode/utf8\\.DecodeRuneInString\\.abiinternal" ->
            let r = (Value.of_word (Bitvector.of_int ~width:64 0x72)) in
            let len = (Value.of_word (Bitvector.of_int ~width:64 0x1)) in
              r >>= fun r' ->
              len >>= fun n ->
              let rax = (Var.create "RAX" reg64_t) in
              let rbx = (Var.create "RBX" reg64_t) in
              Env.set rax r' >>= fun _ ->
              Env.set rbx n >>= fun _ ->
                Machine.return()
          | _ -> Machine.return()
    else
      let start = String.get name 0 in
      Machine.Local.get state >>= fun {file_opened} ->
      if phys_equal start '#' then
        Machine.Global.update state ~f:(fun state' ->
          let () = Hashtbl.set state'.labels ~key:name ~data:x in
          state'
        )
      else
        match file_opened with
          None -> Machine.return()
        | Some file -> Machine.Local.update state ~f:(fun s ->
          let opt = (v |> Value.to_word |> Bitvector.to_int) in
          match opt with
            Ok fd ->
              let _ = Hashtbl.set s.files ~key:fd ~data:file in
              {s with file_opened = None}
          | Error _ -> s)

  let record_def def =
    let v = def |> Def.lhs in
    Machine.Global.update state ~f:(fun state' ->
      let written_vars = state'.written_vars in
      { state' with written_vars = Var.Set.add written_vars v }
    )

  let export_model root nodes functions graph concrete_vars symbolic_vars visited_blks basic_blocks =
    let open Param in
    let redundant_ops = ["BIND"; "SETGID"] |>
                        List.fold ~f:(fun set op -> OperationSet.add set op)
                                  ~init:(OperationSet.empty) in
    let name_and_label_of_tid tid =
        let name = (Tid.name tid) in
        let labels = try Hashtbl.find_exn nodes tid
          with Not_found_s s ->
              (** let () = info "missing node %s %s" name (Sexplib0.Sexp.to_string_hum s) in *)
              ArgSet.singleton name in
        (name, (ArgSet.to_list labels)) in
    let es = EffectGraph.edges graph in
    let edges' = Seq.map ~f:(fun edge ->
        (EffectGraph.Edge.src edge, EffectGraph.Edge.dst edge, EffectGraph.Edge.label edge)
      ) es in
    (** Remove non-SysFlow events from the model. *)
    let g' = edges' |>
             Seq.filter ~f:(fun (src, dst, label) ->
               OperationSet.mem redundant_ops label
             ) |>
             Seq.map ~f:(fun (src, dst, label) ->
                 dst
             ) |>
             Seq.fold ~f:(fun g v ->
                 let preds = EffectGraph.Node.preds v g in
                 let succs = EffectGraph.Node.succs v g in
                 let graph' = EffectGraph.Node.remove v g in
                 Seq.fold ~f:(fun g' p ->
                     succs |>
                     Seq.map ~f:(fun succ ->
                         let opt = EffectGraph.Node.edge v succ g in
                         match opt with
                           None -> raise (MissingEdge "Cannot find edge to successor")
                         | Some edge ->
                             let label = EffectGraph.Edge.label edge in
                             EffectGraph.Edge.create p succ label) |>
                     Seq.fold ~f:(fun g e ->
                         EffectGraph.Edge.insert e g) ~init:g') ~init:graph' preds) ~init:graph in
    let ns = EffectGraph.nodes g' in
    let es = EffectGraph.edges g' in
    let nodes' = Seq.map ~f:name_and_label_of_tid ns in
    let contexts = functions |>
                   Hashtbl.to_alist |>
                   List.map ~f:(fun (tid, (context, func)) ->
                               let (name, _) = name_and_label_of_tid tid in
                               (name, `List [`String context; `String func])) in
    let edges' = Seq.map ~f:(fun edge ->
        (EffectGraph.Edge.src edge, EffectGraph.Edge.dst edge, EffectGraph.Edge.label edge)
      ) es in
    let nodes'' = nodes' |> Seq.map ~f:(fun (name, label) -> `String name) |> Seq.to_list in
    let constraints'' = nodes' |> Seq.map ~f:(fun (name, constraints) ->
        let constraints' = `List (List.map ~f:(fun const ->
                                   const |>
                                   String.map ~f:(fun c -> if phys_equal c '\'' then '"' else c) |>
                                   Yojson.Basic.from_string) constraints) in
        `Assoc [("node", `String name); ("constraints", constraints')]) |> Seq.to_list in
    let edges'' = edges' |> Seq.map ~f:(fun (src, dst, label) ->
        `Assoc [("src", `String (Tid.name src)); ("dst", `String (Tid.name dst)); ("label", `String label)]
      ) |> Seq.to_list in
    let model = `Assoc [("initial", `String (Tid.name root));
                        ("concrete_variables", `Int concrete_vars);
                        ("symbolic_variables", `Int symbolic_vars);
                        ("visited_blocks", `Int visited_blks);
                        ("basic_blocks", `Int basic_blocks);
                        ("nodes", `List nodes'');
                        ("contexts", `Assoc contexts);
                        ("constraints", `List constraints'');
                        ("edges", `List edges'')] in
    let model' = Yojson.Basic.pretty_to_string model in
    let jsonfile_path = Printf.sprintf "%s/%s.json" (get model_dir) (get model_name) in
    let oc = Out_channel.create jsonfile_path in
    let _ = Printf.fprintf oc "%s" model' in
    Out_channel.close oc

  (** Doesn't Core already provide this? *)
  let assoc_field_help pairs field =
    (** let () = info "looking for field %s" field in *)
    pairs |>
    List.filter ~f:(fun (field', v) ->
      (** let () = info "  found field %s" field' in *)
      String.equal field field'
    ) |>
    List.map ~f:(fun (field, v) ->
      match v with
        `List vs ->
          vs |>
          List.map ~f:(fun s ->
            match s with
              `String s' -> s'
            | _ -> "ignored") |>
          String.concat ~sep:" "
      | _ -> "ignored"
    )

  (**
    Fetch a field from an Association List and
    throw an exception if it is absent.
  *)
  let assoc_field_exn pairs field =
    List.hd_exn (assoc_field_help pairs field)

  (**
    Fetch a field from an Association List
  *)
  let assoc_field pairs field =
    List.hd (assoc_field_help pairs field)

  (**
    Fetch a field from an Association List and
    return blank if it is absent.
  *)
  let assoc_field_blank pairs field =
    match List.hd (assoc_field_help pairs field) with
      None -> ""
    | Some x -> x

  exception InvalidConstraint of string

  let list_assoc_field_exn xs field =
    xs |>
    List.map ~f:(fun assoc ->
      match assoc with
        `Assoc constraints -> Printf.sprintf "{%s}" (assoc_field_blank constraints field)
      | _ -> raise (InvalidConstraint "Constraint not represented as Assoc")) |>
    String.concat ~sep:"|"

  (**
    Infer the SysFlow type from the constraints.
  *)
  let flowtype_of_constraints constraints =
    let getflow key = constraints |>
                      List.map ~f:fst |>
                      List.filter ~f:(fun key' -> String.equal key' key) |>
                      List.length in
    if (getflow Sf.net_dport) > 0 then
      Sf.Network
    else if (getflow Sf.file_path) > 0 then
      Sf.File
    else if (getflow Sf.proc_args) > 0 then
      Sf.Process
    else if (getflow Sf.file_size) > 0 then
      Sf.File
    else
      Sf.Process

  (**
    TODO: Render a node's label in a less ad-hoc way.
  *)
  let label_of_node node (called_module, called_function) =
    let context = Printf.sprintf "{%s:%s}" called_module called_function in
    let nodes = ArgSet.to_list node in
    let nodes' = List.map ~f:(fun node ->
       node |> String.map ~f:(fun c -> if phys_equal c '\'' then '"' else c) |> Yojson.Basic.from_string
    ) nodes in
    let hd = List.hd_exn nodes' in
    match hd with
     `Assoc constraints ->
       let flowtype = flowtype_of_constraints constraints in
       (match flowtype with
         File ->
           (** Attempt to pick up on mmap. *)
           let opt = (assoc_field constraints Sf.file_size) in
           (match opt with
             None -> Printf.sprintf "{%s|{%s|%s}}" context Sf.file_label (list_assoc_field_exn nodes' Sf.file_path)
           | Some size -> Printf.sprintf "{%s|{%s|%s|%s}}" context Sf.file_label size (list_assoc_field_exn nodes' Sf.file_perms))
       | Network -> Printf.sprintf "{%s|{%s|%s}}" context Sf.network_label (list_assoc_field_exn nodes' Sf.net_dport)
       | Process ->
         let options = [Sf.ret; Sf.proc_uid; Sf.proc_gid] in
         let opt = List.fold_left ~f:(fun opt' field ->
            match opt' with
              None -> (assoc_field constraints field)
            | _  -> opt'
         ) ~init:None options in
         match opt with
           Some value -> Printf.sprintf "{%s|{%s|%s}}" context Sf.process_label value
         | None ->
           Printf.sprintf "{%s|{%s|%s|%s}}" context Sf.process_label
                                                    (assoc_field_blank constraints Sf.proc_exe)
                                                    (assoc_field_blank constraints Sf.proc_args))
    | _ -> "ignored"

  (** Compute the union of the Local and Global
      graphs after a Machine ends and store it in Global. *)
  let record_model () =
    let open Param in
    let dotfile_path = Printf.sprintf "%s/%s.dot" (get model_dir) (get model_name) in
    let dotfile = Out_channel.create dotfile_path in
    Machine.current () >>= fun pid ->
    Machine.get () >>= fun proj ->
    let prog = (Project.program proj) in
    let symbolic_vars = Var.Set.empty |> find_symbolic_variables#run prog |> Var.Set.length in
    let basic_blocks = Blk.Set.empty |> find_basic_blocks#run prog |> Blk.Set.length in
    (** let () = info "Machine %a ending!" Id.pp pid in *)
    if phys_equal Machine.global pid then
      (** let () = info "Global machine ending." in *)
      Machine.args >>= fun args ->
      Machine.Global.get state >>= fun state' ->
      let {root_tid;nodes;graph;functions;written_vars;visited_blks} = state' in
      let _ = Graphlib.to_dot (module EffectGraph)
        ~node_attrs:(fun tid ->
          let context = try Hashtbl.find_exn functions tid
            with Not_found_s s ->
               (** let () = info "missing context for node %s" name in *)
                 ("N/A", "N/A") in
          let node = try Hashtbl.find_exn nodes tid
            with Not_found_s s ->
                (** let () = info "missing node %s %s" name (Sexplib0.Sexp.to_string_hum s) in *)
                ArgSet.singleton (Tid.name tid) in
          let label = label_of_node node context in
          [`Fontsize 9; `Label label; `Shape `Box])
        ~edge_attrs:(fun edge ->
          let label = EffectGraph.Edge.label edge in
          [`Fontsize 8; `Label label;])
        ~channel:dotfile graph in
      let concrete_vars = (Var.Set.length written_vars) in
      let _ = export_model root_tid nodes functions graph concrete_vars symbolic_vars (Blk.Set.length visited_blks) basic_blocks in
      Machine.return()
    else
      Machine.Local.get state >>= fun state' ->
      let {nodes;graph;last_tid;functions} = state' in
      (** let () = info "last tid: %s" (Tid.name last_tid) in *)
      Machine.Global.update state ~f:(fun s ->
          (** let () = info "Updating global state!" in *)
          let graph' = Graphlib.union (module EffectGraph) s.graph graph in
          let () = Hashtbl.merge_into ~src:functions ~dst:s.functions
                                      ~f:(fun ~key:_ x y -> Set_to x) in
          {s with halted = Set.add s.halted pid; graph=graph'})

  let record_model_pid pid =
    record_model ()

  let reschedule () =
    let last = Seq.fold ~init:None ~f:(fun _ x -> Some x) in
    Machine.Global.get state >>= fun {halted} ->
    Machine.forks () >>= fun forks ->
    let active = Seq.filter forks ~f:(fun id -> not (Set.mem halted id)) in
    match last active with
    | None ->
      (** info "no more pending machines"; *)
      Machine.switch Machine.global
    | Some cid ->
      Machine.current () >>= fun pid ->
      (** info "upc: switch to machine %a from %a" Id.pp cid Id.pp pid; *)
      (** info "upc: killing previous machine %a" Id.pp pid; *)
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
       Machine.Local.get state >>= fun {last_tid; nodes; graph} ->
      match (Jmp.kind j) with
          Call c ->
          let label = c |> Call.target |> Label.to_string in
          let prefix = String.get label 0 in
          if phys_equal prefix '@' then
            let func = String.drop_prefix label 1 in
            record_function tid func
          else
            let () = info "Indirect function call: %s" label in
            let prefix = String.get label 0 in
            if phys_equal prefix '#' then
              Machine.Global.get state >>= fun state' ->
              let {labels} = state' in
              let var = Hashtbl.find_exn labels label in
              (Env.get var) >>= fun v ->
              let target = (v |> Value.to_word) in
              let {symbols} = state' in
              let matched = symbols |> List.filter ~f:(fun (name, block, cfg) ->
                  let addr = block |> Block.addr in
                  (** let () = info "  found %s %s" name (Addr.to_string addr) in *)
                  phys_equal addr target) |> List.map ~f:(fun (name, block, cfg) ->
                  name
                ) in
              if (List.length matched) > 0 then
                let f = List.nth_exn matched 0 in
                (** let () = info "  match %s" f in *)
                record_function tid f
              else
                (** let () = info "  target %s" (Addr.to_string target) in *)
                Machine.return()
            else
              Machine.return()
        | _ ->
          (** let () = info "    Different kind of jump" in *)
          Machine.return ()

  let leave_jmp j =
    let tid = Term.tid j in
    let tid_name = (Tid.name tid) in
    let () = info "leaving jump %s" tid_name in
    Machine.return()

  let addr_of_sub_name name =
    match String.chop_prefix ~prefix:"sub_" name with
      None -> None
    | Some x -> Some (int_of_string ("0x" ^ x))

  let push_sub s =
    let name = Sub.name s in
    (** let () = info "entering sub %s" name in *)
    Machine.Local.update state ~f:(fun s ->
      let {callstack;symbols} = s in
      let name' = match addr_of_sub_name name with
                    None -> name
                  | Some x ->
                    x in
      {s with callstack=name'::callstack}
    )

  let pop_sub s =
    let name = Sub.name s in
    (** let () = info "leaving sub %s" (Sub.name s) in *)
    (** Abstract this for any Go runtime method. *)
    if String.equal (Sub.name s) "io_ioutil.ReadFile" then
      let rbp = Var.create "RBP" reg64_t in
      Env.get rbp >>= fun v ->
        let w = v |> Value.to_word in
        let offset = (Bitvector.of_int ~width:64 (-0x68)) in
        let target = Bitvector.add w offset in
        copy_int target 0x0 >>= fun _ ->
          Machine.Local.update state ~f:(fun s ->
            let {callstack} = s in
            match callstack with
              [] -> s
            | c :: cs ->
              {s with callstack=cs}
          )
    else
        Machine.Local.update state ~f:(fun s ->
          let {callstack} = s in
            match callstack with
              [] -> s
            | c :: cs ->
              {s with callstack=cs}
        )

  let record_finished () =
    Machine.current () >>= fun pid ->
    (** let () = info "machine finished!" in *)
    Machine.return()

  let def name types closure docs =
    Lisp.define ~docs ~types name closure

  let setup_tracing () =
    Machine.List.sequence [
      Primus.Interpreter.loading >>> record_loading;
      Primus.Interpreter.reading >>> record_reading;
      Primus.Interpreter.written >>> record_written;
      Primus.Interpreter.enter_pos >>> record_pos;
      (** Primus.Interpreter.enter_blk >>> record_blk; *)
      Primus.Interpreter.enter_def >>> record_def;
      Primus.Interpreter.enter_jmp >>> record_jmp;
      Primus.Interpreter.leave_jmp >>> leave_jmp;
      Primus.Interpreter.enter_sub >>> push_sub;
      Primus.Interpreter.leave_sub >>> pop_sub;
      Primus.System.fini >>> record_model;
      Primus.Machine.kill >>> record_model_pid;
      def "array-make" (tuple [a; b] @-> bool) (module ArrayMake)
      {|(array-make ARRAY DATA) makes an ARRAY. |};
      def "array-elt-size" (tuple [a] @-> b) (module ArraySize)
      {|(array-elt-size ARRAY) gets the size of elements in ARRAY. |};
      def "array-push" (tuple [a; b] @-> bool) (module Push)
      {|(array-push ARRAY DATA) pushes DATA onto ARRAY. |};
      def "array-pop" (tuple [a] @-> b) (module Pop)
      {|(array-pop ARRAY DATA) pops DATA from ARRAY. |};
      def "upc-ocaml-sscanf" (tuple [a; b] @-> bool) (module Scan)
      {|(upc-ocaml-sscanf) tries to implement sscanf. |};
      def "upc-ocaml-scanf" (tuple [a] @-> b) (module Scanf)
      {|(upc-ocaml-scanf FMT ADDRESS) tries to implement scanf. |};
      def "upc-ocaml-sprintf" (tuple [a; b] @-> bool) (module Sprintf)
      {|(upc-ocaml-snprintf S FMT VAL)  tries to implement snprintf. |};
      def "upc-ocaml-snprintf" (tuple [a; b; c] @-> bool) (module Snprintf)
      {|(upc-ocaml-snprintf S SZ FMT VAL)  tries to implement snprintf. |};
      def "upc-ocaml-fstat" (tuple [a; b] @-> c)  (module FStat)
      {|(upc-ocaml-fstat FD BUF) implements fstat. |};
      def "upc-ocaml-stat" (tuple [a; b] @-> c)  (module Stat)
      {|(upc-ocaml-stat FD BUF) implements stat. |};
      def "upc-ocaml-network-fd" (tuple [a; b] @-> c) (module Network)
      {|(upc-ocaml-network-fd BINDFD NETFD) informs uPolicyCraft of the port attached to a socket returned by accept.|};
      def "upc-ocaml-debug" (tuple [a] @-> b) (module Debug)
      {|(upc-ocaml-debug DATA) logs a lisp value for debugging. |};
      def "upc-ocaml-dump" (tuple [a; b] @-> c) (module Dump)
      {|(upc-ocaml-dump PTR INT) renders the contents of memory. |};
      def "upc-ocaml-network-test-cases" (tuple [] @-> b) (module NetworkTestCases)
      {|(upc-ocaml-network-test-cases) reports the number of test cases available for micro-execution.|};
      def "upc-ocaml-add-socket" (tuple [a] @-> bool) (module AddSocket)
      {|(upc-ocaml-add-socket) adds a file descriptor representing a socket to the micro-execution state.|};
      def "upc-ocaml-getuid" (tuple [] @-> a) (module GetUid)
      {|(upc-ocaml-getuid) fetches the current user id.|};
      def "upc-ocaml-rand" (tuple [] @-> a) (module UpcRand)
      {|(upc-ocaml-rand) computes a random value.|};
      def "upc-ocaml-sqrt" (tuple [a] @-> b) (module UpcSqrt)
      {|(upc-ocaml-sqrt) computes the sqrt.|};
      def "upc-ocaml-log" (tuple [a] @-> b) (module UpcLog)
      {|(upc-ocaml-log) computes the log.|};
      def "upc-ocaml-round" (tuple [a] @-> b) (module UpcRound)
      {|(upc-ocaml-round) computes round.|};
      def "upc-ocaml-floor" (tuple [a] @-> b) (module UpcFloor)
      {|(upc-ocaml-floor) computes floor.|};
      def "upc-ocaml-dup2" (tuple [a;b] @-> c) (module Dup2)
      {|(upc-ocaml-dup2) makes a copy of oldfd into newfd.|};
      def "upc-ocaml-check-dup2" (tuple [a] @-> b) (module CheckDup2)
      {|(upc-ocaml-check-dup2) checks if a file descriptor has been copied.|};
      def "upc-ocaml-inet-aton" (tuple [a; b] @-> c) (module InetAton)
      {|(upc-ocaml-inet-aton) converts an IP address into a number in network byte order..|};

    ]

  let json_string data =
   data |>
   jsonify |>
   Yojson.Basic.pretty_to_string |>
   String.map ~f:(fun c -> if phys_equal c '"' then '\'' else c)

  let find_test_cases test_case_dir =
      let f = Caml_unix.opendir test_case_dir in
      let rec find_files files =
        try
          let next = Caml_unix.readdir f in
          find_files (next :: files)
        with End_of_file ->
          let () = Caml_unix.closedir f in
          files in
      find_files []

  let parse_filesystem filesystem =
    filesystem |>
    String.split_on_chars ~on:[','] |>
    List.filter ~f:(fun s -> not (String.equal s "")) |>
    List.fold_left ~f:(fun tbl s ->
      let parts = String.split ~on:':' s in
      let file = List.nth_exn parts 0 in
      let dst = List.nth_exn parts 1 in
      let () = Hashtbl.set tbl ~key:file ~data:dst in
      tbl) ~init:(Hashtbl.create (module String))

  let setup_golang_runtime =
    let open Param in
     if String.equal (get language) Languages.golang then
      (** Go programs use r14 as a reference to the bottom of the stack.
          Zeroing out the memory allows the check to proceed. *)
      let r14 = Var.create "R14" reg64_t in
      let baseaddr = 0x3000 in
        (Value.of_word (Bitvector.of_int ~width:64 baseaddr)) >>= fun base ->
                 (Value.of_word (Bitvector.of_int ~width:64 0x100)) >>= fun size ->
                 (Value.of_word (Bitvector.of_int ~width:64 0x00)) >>= fun init ->
              memory_map base size [init] >>= fun status ->
            (Value.of_word (Bitvector.of_int ~width:64 baseaddr)) >>= fun offset ->
         Env.set r14 offset >>= fun _ ->
         copy_int (Bitvector.of_int ~width:64 (baseaddr+0x10)) 0x0 >>= fun _ ->
         copy_int (Bitvector.of_int ~width:64 (baseaddr+0x20)) 0x1
    else
      Machine.return()

  let init () =
    let open Param in
    setup_tracing () >>= fun () ->
    Machine.get () >>= fun proj ->
    Machine.args >>= fun args ->
    Machine.envp >>= fun envp ->
    (** let () = envp |> Array.to_list |> List.iter ~f:(fun e -> Printf.printf "envp: %s\n" e) in *)
    let no_test_cases = (get test_cases) in
    let filesystem = parse_filesystem (get filesystem) in
    let symtab = Project.symbols proj in
    let symtabs = (symtab |> Symtab.to_sequence |> Seq.to_list) in
    let root = Tid.create() in
    let root' = Tid.create() in
    let port = Tid.create() in
    let proc = Tid.create() in
    let (exe, args') = args |> Array.to_list |> split_argv in
    (** Make an initialization routine. *)
    let entry = json_string [(Sf.proc_exe, [get entrypoint]);
                             (Sf.proc_args, [get entrypoint_args])] in
    let cloned_entry = json_string [(Sf.proc_exe, [get entrypoint]);
                                    (Sf.proc_args, [get entrypoint_args]);
                                    (Sf.pproc_pid, [Sf.special Sf.Vars.pred Sf.proc_pid])] in
    let accepted_port = json_string [(Sf.net_dport, ["8003"])] in
    let constraints = json_string [(Sf.proc_exe, [exe]); (Sf.proc_args, [args'])] in
    let behavior =
      if (get inetd_startup) then
        Graphlib.create (module EffectGraph)
                        ~nodes:[root;proc]
                        ~edges:[(root,proc,"EXEC")] ()
      else
        Graphlib.create (module EffectGraph)
                        ~nodes:[root;root';proc]
                        ~edges:[(root,root',"CLONE"); (root',proc,"EXEC")] () in
    let nodes = Hashtbl.create (module Tid) in
    let functions = Hashtbl.create (module Tid) in
    let arrays = Hashtbl.create (module String) in
    let ports = Hashtbl.create (module Int) in
    let files = Hashtbl.create (module Int) in
    let network = "0.0.0.0/0:*" in
    let uid = (get user_id) in
    let gid = (get group_id) in
    let () = Hashtbl.add_exn files ~key:255 ~data:network in
    let () = Hashtbl.add_exn files ~key:1024 ~data:"epoll" in
    let () =
      if (get inetd_startup) then
        let xinetd = "/usr/sbin/xinetd" in
        let whitelist = "0.0.0.0/0:8003" in
        let () = Hashtbl.add_exn files ~key:0 ~data:whitelist in
        let () = Hashtbl.add_exn files ~key:1 ~data:whitelist in
        let () = Hashtbl.add_exn functions ~key:root ~data:(xinetd, "main") in
        let () = Hashtbl.add_exn functions ~key:port ~data:(xinetd, "main") in
        Hashtbl.add_exn functions ~key:proc ~data:(xinetd, "main")
      else
        let () = Hashtbl.add_exn files ~key:0 ~data:"/dev/pts/0" in
        let () = Hashtbl.add_exn files ~key:1 ~data:"/dev/pts/0" in
        let () = Hashtbl.add_exn functions ~key:root ~data:("/bin/bash", "main") in
        let () = Hashtbl.add_exn functions ~key:root' ~data:("/bin/bash", "main") in
        Hashtbl.add_exn functions ~key:proc ~data:("/bin/bash", "main") in
    let () = Hashtbl.add_exn files ~key:2 ~data:"/dev/stderr" in
    let () = Hashtbl.add_exn nodes ~key:root  ~data:(ArgSet.singleton entry) in
    let () = Hashtbl.add_exn nodes ~key:root' ~data:(ArgSet.singleton cloned_entry) in
    let () = Hashtbl.add_exn nodes ~key:port  ~data:(ArgSet.singleton accepted_port) in
    let () = Hashtbl.add_exn nodes ~key:proc  ~data:(ArgSet.singleton constraints) in
    let fs_base = Var.create "FS_BASE" reg64_t in
      (Value.of_word (Bitvector.of_int ~width:64 0x1000)) >>= fun base ->
               (Value.of_word (Bitvector.of_int ~width:64 0x100)) >>= fun size ->
               (Value.of_word (Bitvector.of_int ~width:64 0x00)) >>= fun init ->
            memory_map base size [init] >>= fun status ->
          (** Allow negative indexes on the FS register. *)
          (Value.of_word (Bitvector.of_int ~width:64 0x1010)) >>= fun offset ->
          Env.set fs_base offset >>= fun _ ->
            setup_golang_runtime >>= fun _ ->
    Machine.Global.update state ~f:(fun s ->
        { symbols = symtabs;
          nodes = nodes;
          functions = functions;
          ports = ports;
          files = files;
          file_opened = None;
          labels = Hashtbl.create (module String);
          halted = Id.Set.empty;
          visited_blks = Blk.Set.empty;
          root_tid = root;
          last_tid = proc;
          graph = behavior;
          arrays = arrays;
          current_function = "main";
          current_module = exe;
          callstack = ["entrypoint"];
          no_test_cases = no_test_cases;
          uid = uid;
          gid = gid;
          saved_uid = None;
          saved_gid = None;
          filesystem = filesystem;
          duped_file_descriptors = Hashtbl.create (module Int);
          cwd = "/";
          written_vars = Var.Set.empty;
          fs_set = false;
          top_of_heap = bottom_of_heap;
          last_rax = [];
          exitpoint = Bitvector.of_int ~width:64 (get exitpoint);
        }
      ) >>= fun s ->
    Machine.Local.update state ~f:(fun _ ->
        { symbols = symtabs;
          nodes = nodes;
          functions = functions;
          ports = ports;
          files = files;
          file_opened = None;
          labels = Hashtbl.create (module String);
          halted = Id.Set.empty;
          visited_blks = Blk.Set.empty;
          root_tid = root;
          last_tid = proc;
          graph = behavior;
          arrays = arrays;
          current_function = "main";
          current_module = exe;
          callstack = ["entrypoint"];
          no_test_cases = no_test_cases;
          uid = uid;
          gid = gid;
          saved_uid = None;
          saved_gid = None;
          filesystem = filesystem;
          duped_file_descriptors = Hashtbl.create (module Int);
          cwd = "/";
          written_vars = Var.Set.empty;
          fs_set = false;
          top_of_heap = bottom_of_heap;
          last_rax = [];
          exitpoint = Bitvector.of_int ~width:64 (get exitpoint);
        })
end

let desc =
  "The uPolicyCraft modeler builds a program's effect graph by micro-executing
   the program against a lifted static representation of the program."

let main {Config.get=(!)} =
  let open Param in
  if !model then
    Channels.init !redirects;
    Primus.Machine.add_component (module Monitor) [@warning "-D"];
    Primus.Components.register_generic "model" (module Monitor)
      ~package:"upc"
      ~desc:("Runs the uPolicyCraft modeler. " ^ desc)

let () =
  Config.when_ready (fun conf ->
      main conf)
