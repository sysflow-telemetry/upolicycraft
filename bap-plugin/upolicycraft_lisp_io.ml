(**
  A refactored IO library based on the original library authored by the BAP team.

  Authors:
  The BAP development team.
  William Blair <wdblair@bu.edu>
*)
open Core
open Bap.Std
open Bap_primus.Std

include Self()

type channel = {
  input : In_channel.t option;
  output : Out_channel.t option;
}

type state = {
  redirections : string String.Map.t;
  channels : channel Int.Map.t;
  directories: Caml_unix.dir_handle Int.Map.t;
  files : string Int.Map.t;
  cwd : string;
  epoll_fds : (Primus.Value.t * Primus.Value.t) list;
  epoll_replay : int list;
}

let default_channels = Int.Map.of_alist_exn [
    0, {
      input = Some In_channel.stdin;
      output = None;
    };
    1, {
      input = None;
      output = Some Out_channel.stdout;
    };
    2, {
      input = None;
      output = Some Out_channel.stderr;
    }
  ]

let standard_channels = [
  "<upc-stdin>";
  "<upc-stdout>";
  "<upc-stderr>";
]

let empty_channel = {
  input = None;
  output = None;
}

let fd_of_name name =
  List.find_mapi standard_channels ~f:(fun i chan ->
      if String.equal name chan then Some i else None)

let init_channels =
  List.fold ~init:default_channels ~f:(fun chans (chan,name) ->
      match fd_of_name chan with
      | None -> chans
      | Some fd ->
        info "redirecting fd %d to %s" fd name;
        let chan = if fd > 0 then {
            input = None;
            output = Some (Out_channel.create name);
          } else {
            output = None;
            input = Some (In_channel.create name);
          } in
        Map.set chans ~key:fd ~data:chan)

let init_files = Int.Map.of_alist_exn [
   0, "/dev/pts/0";
   1, "/dev/pts/0";
   2, "/dev/stderr";
]

let init_directories =
  Int.Map.empty

let init_redirections =
  List.fold ~init:String.Map.empty ~f:(fun redirs (oldname,newname) ->
      match fd_of_name oldname with
        Some _ -> redirs
      | None -> Map.set redirs ~key:oldname ~data:newname)

let init redirs = {
  redirections = init_redirections redirs;
  channels = init_channels redirs;
  files = init_files;
  directories = init_directories;
  cwd = "/";
  epoll_fds = [];
  epoll_replay = [0; 2; 4; 2; 5; 2; 6; 2; 7; 9];
}

(** Some binaries erroneously build paths with extra slashes. *)
let sanitize_path path =
    String.substr_replace_all path ~pattern:"//" ~with_:"/"

let try_open path = Or_error.try_with (fun () -> {
      input = Some (In_channel.create path);
      output = Some (Out_channel.create ~append:true path)
    })

let try_open_network path = Or_error.try_with (fun () -> {
      input = Some (In_channel.create path);
      output = Some (Out_channel.create ~append:true "/tmp/network")
    })


let try_flush {output} = Or_error.try_with @@ fun () ->
  Option.iter ~f:Out_channel.flush output

let input_byte chan = Or_error.try_with @@ fun () ->
  In_channel.input_byte chan

let next_fd channels = match Map.max_elt channels with
  | None -> 1
  | Some (fd,_) -> fd + 1

let state = Primus.Machine.State.declare
    ~name:"upc-io-channels"
    ~uuid:"21c9485f-c088-4f46-8f85-d5ec0b95c13f"
    (fun _ -> {
        redirections = String.Map.empty;
        channels = Int.Map.empty;
        files = Int.Map.empty;
        directories = Int.Map.empty;
        cwd = "/";
        epoll_fds = [];
        epoll_replay = [0; 2; 4; 2; 5];
       })

module Lib(Machine : Primus.Machine.S) = struct
    include Machine.Syntax
    module Eval = Primus.Interpreter.Make(Machine)
    module Memory = Primus.Memory.Make(Machine)
    module Value = Primus.Value.Make(Machine)

    let int_of_value v = (v |> Value.to_word |> Bitvector.to_int_exn)

    let allow_all_memory_access access =
      Machine.catch access (function exn ->
        (** let () = info "Error reading memory!" in
        let msg = Primus.Exn.to_string exn in
        let () = info "    %s" msg in *)
        Value.of_bool (false))

    let sizeof_event = 12

    let dump_memory addr steps =
      let () = info "dump memory" in
      let rec loop n addr =
        if n = steps then
          Machine.return()
        else
          allow_all_memory_access (Memory.get addr) >>= fun v ->
          let x = v |> Value.to_word |> Bitvector.to_int64_exn in
          let () = info "  %Lx" x in
          loop (succ n) (Bitvector.succ addr) in
      loop 0 addr

    let trap_memory_write access =
      Machine.catch access (function exn ->
          (** let () = info "Error reading memory!" in
          let msg = Primus.Exn.to_string exn in
          let () = info "    %s" msg in *)
          Machine.return())

    let addr_width =
      Machine.arch >>| Arch.addr_size >>| Size.in_bits
    let nil = Value.b0
    let error = addr_width >>= fun w -> Value.of_word (Word.ones w)
    let ok = addr_width >>= Value.zero
    let zero = ok

    let value_to_int x =
      Value.to_word x |> Word.to_int |> function
      | Error _ -> None
      | Ok n -> Some n

    let value_to_fd = value_to_int

    let value_of_int x = addr_width >>= fun w -> Value.of_word (Bitvector.of_int ~width:w x)

    (** Copy a 4 byte integer into an address. *)
    let copy_int32 addr x =
      let () = info "Writing %d into address" x in
      let rec loop addr x i =
        if i = 0 then
          Machine.return()
        else
          let v = x land 0xff in
          Value.of_word (Bitvector.of_int 8 v) >>= fun v' ->
          trap_memory_write (Memory.set addr v') >>= fun () ->
            loop (Bitvector.succ addr) (x lsr 8) (pred i) in
      loop addr x 4

    (** Copy one buffer to another *)
    let memcpy dst src n =
      let dst' = (Value.to_word dst) in
      let src' = (Value.to_word src) in
      let rec loop dst' src' n =
        if n = 0 then
          Machine.return()
        else
          Memory.get src' >>= fun byte ->
          (trap_memory_write (Memory.set dst' byte)) >>= fun () ->
          loop (Bitvector.succ dst') (Bitvector.succ src') (pred n) in
      loop dst' src' n

    let string_of_charp ptr =
      let rec loop chars ptr =
        Eval.load ptr LittleEndian `r8 >>= fun c ->
        let c = (* [load p e `r8] must return a byte *)
          Char.of_int_exn @@
          Word.to_int_exn @@
          Value.to_word c in
        if Char.(c = '\000')
        then Machine.return (String.of_char_list (List.rev chars))
        else Value.succ ptr >>= loop (c::chars) in
      loop [] ptr

    let find_path state path =
      let {redirections;cwd} = state in
      let path' = sanitize_path path in
      let () = info "Finding path %s in %s" path' cwd in
      let opt = Map.find redirections path' in
      match opt with
        None ->
        let path'' = cwd ^ path' in
        let opt = Map.find redirections path'' in
        (path'', opt)
      | _ -> (path', opt)

    let trap_memory_write access =
      Machine.catch access (function exn ->
          (** let () = info "Error reading memory!" in
          let msg = Primus.Exn.to_string exn in
          let () = info "    %s" msg in *)
          Machine.return())

    (** Copy a string into memory (merge into a common module) *)
    let copy_bytes addr s =
      let addr' = (Value.to_word addr) in
      let () = info "copying %s into %x" s (Bitvector.to_int_exn addr') in
      let cont' = String.foldi ~f:(fun i cont c ->
        let c' = int_of_char c in
        Value.of_word (Bitvector.of_int 64 c') >>= fun v ->
        cont >>= fun () ->
          let dst = (Bitvector.add addr' (Bitvector.of_int 64 i)) in
          (trap_memory_write (Memory.set dst v))) ~init:(Machine.return()) s in
      cont' >>= fun () ->
         let n = String.length s in
         Value.of_word (Bitvector.of_int 64 0) >>= fun z ->
         let dst = (Bitvector.add addr' (Bitvector.of_int 64 n)) in
         let () = info "writing 0 into %x" (Bitvector.to_int_exn dst) in
         (trap_memory_write (Memory.set dst z))

    let create_mode = 0x40

    let open_file path =
      Machine.Local.get state >>= fun state' ->
        let (absolute_path, host_path) = find_path state' path in
        match host_path with
          None -> error
        | Some path -> match try_open path with
          | Error _ -> error
          | Ok channel ->
            Machine.Local.get state >>= fun s ->
            let fd = next_fd s.channels in
            Machine.Local.put state {
              s with
              channels = Map.set s.channels
                  ~key:fd
                  ~data:channel;
              files = Map.set s.files
                  ~key:fd
                  ~data:absolute_path
            } >>= fun () ->
            addr_width >>= fun width ->
            Value.of_int ~width:width fd

end

let init redirections =

  let module Open(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]

    let run [path; mode] =
      string_of_charp path >>= fun path ->
      Machine.Local.get state >>= fun state' ->
      let (_, opt) = find_path state' path in
      match opt with
        None ->
          open_file path
          (**
          Left over from trying to implement write:
          let mode' = match value_to_int mode with
                        None -> 0
                      | Some m -> m in
          (** Check if we need to create the file. *)
          if phys_equal (mode' land create_mode) 0 then
            Machine.Local.update state ~f:(fun state ->
              let {redirections;cwd} = state in
              let path' = if String.is_prefix ~prefix:"/" path then
                            path
                          else
                            cwd ^ path in
              let hostfile = "/tmp/writes" in
              let () = info "Creating redirection %s:%s" path' hostfile in
              { state with redirections=(Map.set redirections ~key:path' ~data:hostfile) }
            ) >>= fun _ ->
              open_file path
          else
              open_file path *)
      | Some _ -> open_file path
  end in

  let module OpenTmp(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]

    let run [] =
      let () = info "Opening tmpfile!" in
      open_file "/tmp/tmpfile"
  end in

  let module OpenPipe(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]

    let run [] =
      (** TODO: May want to make a new pipe for each file. *)
      let () = info "Opening pipe!" in
        open_file "/tmp/pipe"
  end in

  let module OpenDir(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]

    let enoent = 2

    let run [path] =
      string_of_charp path >>= fun path ->
      Machine.Local.get state >>= fun s ->
      let (absolute_path, opt) = find_path s path in
      match opt with
        None -> zero
      | Some hostfile ->
        let () = info "Calling opendir %s" hostfile in
        let dir = Caml_unix.opendir hostfile in
        let fd = next_fd s.files in
        Machine.Local.put state {
           s with
           directories = Map.set s.directories
               ~key:fd
               ~data:dir;
           files = Map.set s.files
               ~key:fd
               ~data:hostfile;
           channels = Map.set s.channels
               ~key:fd
               ~data:empty_channel;
         } >>= fun () ->
         addr_width >>= fun width ->
         Value.of_int ~width:width fd

  end in

  let module ReadDir(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]

    let run [dir; dirent] =
       match value_to_fd dir with
        None -> zero
      | Some fd ->
        Machine.Local.get state >>= fun s ->
          let opt = Map.find s.directories fd in
          match opt with
            None -> zero
          | Some dir ->
            try
              let file = Caml_unix.readdir dir in
              let dirent' = (Value.to_word dirent) in
              let dname_offset = 0x13 in
              (Value.of_word (Bitvector.add dirent' (Bitvector.of_int 8 dname_offset))) >>= fun dname ->
              copy_bytes dname file >>= fun () ->
                Machine.return (dirent)
            with End_of_file ->
              zero

  end in


  let module OpenEpoll(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]

    let run [] =
      let () = info "Opening epoll!" in
        open_file "/tmp/epoll"
  end in

  let module EpollWait(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]

    let run [events; nevents] =
      Machine.Local.get state >>= fun s ->
        let {epoll_replay;epoll_fds} = s in
        match epoll_replay with
          [] -> zero
        | e :: es ->
          let (fd, buf) = List.nth_exn epoll_fds e in
          let () = info "Rendering activity on event %d fd:%d" e (int_of_value fd) in
          memcpy events buf sizeof_event >>= fun _ ->
          let events' = Value.to_word events in
          copy_int32 events' 0x1 >>= fun _ ->
          dump_memory events' sizeof_event >>= fun _ ->
          Machine.Local.put state {
            s with
            epoll_replay = es
          } >>= fun () ->
          value_of_int 1

  end in

  let module EpollCtl(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]

    let run [fd; buf] =
      Machine.Local.get state >>= fun s ->
        let () = info "epoll ctl chunk %d" (int_of_value fd) in
        dump_memory (Value.to_word buf) 12 >>= fun _ ->
        let chunk = (fd, buf) in
        let {epoll_fds} = s in
        Machine.Local.put state {
          s with
          epoll_fds = epoll_fds @ [chunk]
        } >>= fun () ->
          zero
  end in

  let module OpenEventFd(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]

    let run [] =
      let () = info "Opening eventfd!" in
        open_file "/tmp/eventfd"
  end in

  let module OpenNetwork(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]

    let run [path] =
      string_of_charp path >>= fun path ->
      Machine.Local.get state >>= fun {redirections} ->
      match Map.find redirections path with
      | None -> error
      | Some path -> match try_open_network path with
        | Error _ -> error
        | Ok channel ->
          Machine.Local.get state >>= fun s ->
          let fd = next_fd s.channels in
          Machine.Local.put state {
            s with
            channels = Map.set s.channels
                ~key:(next_fd s.channels)
                ~data:channel
          } >>= fun () ->
          addr_width >>= fun width ->
          Value.of_int ~width fd
  end in

  let module Close(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]

    let run [fd] =
      value_to_fd fd |> function
      | None -> error
      | Some fd ->
        Machine.Local.get state >>= fun s ->
        if Map.mem s.channels fd
        then Machine.Local.put state {
            s with channels = Map.remove s.channels fd;
          } >>= fun () -> ok
        else error
  end in

  let module Output(Machine : Primus.Machine.S) = struct
    include Lib(Machine)

    [@@@warning "-P"]
    let run (fd :: xs) =
      value_to_fd fd |> function
      | None -> error
      | Some fd ->
        Machine.Local.get state >>= fun {channels} ->
        match Map.find channels fd with
        | Some {output=Some ch} ->
          List.iter xs ~f:(fun w ->
              Word.enum_chars (Value.to_word w) LittleEndian |>
              Seq.hd |> Option.iter ~f:(Out_channel.output_char ch));
          ok
        | _ -> error
  end in

  let module Flush(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]
    let run [fd] =
      value_to_fd fd |> function
      | None -> error
      | Some fd ->
        Machine.Local.get state >>= fun s ->
        match Map.find s.channels fd with
        | None -> error
        | Some chan -> match try_flush chan with
          | Error _ -> error
          | Ok () -> ok
  end in

  let module Input(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]
    let run [fd] =
      value_to_fd fd |> function
      | None -> error
      | Some fd ->
        Machine.Local.get state >>= fun s ->
        match Map.find s.channels fd with
        | None -> error
        | Some {input=None} -> error
        | Some {input=Some ch} -> match input_byte ch with
          | Error _ -> error
          | Ok None -> error
          | Ok (Some ch) -> Value.of_int ~width:8 ch
  end in

  let module Seek(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]
    let run [fd; offs] =
      value_to_fd fd |> function
      | None -> error
      | Some fd ->
        value_to_int offs |> function
        | None -> error
        | Some offs ->
          Machine.Local.get state >>= fun s ->
          match Map.find s.channels fd with
          | None -> error
          | Some {input=None} -> error
          | Some {input=Some ch} ->
            let () = In_channel.seek ch (Int64.of_int offs) in
            ok
  end in

  let module Pos(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]
    let run [fd] =
      value_to_fd fd |> function
      | None -> error
      | Some fd ->
        Machine.Local.get state >>= fun s ->
        match Map.find s.channels fd with
          None -> error
        | Some {input=None} -> error
        | Some {input=Some ch} -> ch |>
                                  In_channel.pos |>
                                  Int64.to_int_exn |>
                                  value_of_int
  end in

  let module Primitives(Machine : Primus.Machine.S) = struct
    open Machine.Syntax
    module Lisp = Primus.Lisp.Make(Machine)
    module Env = Primus.Env.Make(Machine)
    module Value = Primus.Value.Make(Machine)
    module Lib = Lib(Machine)

    let setup_standard_channels =
      let set name descr =
        Lib.addr_width >>= fun width ->
        let v = Var.create name (Type.imm width) in
        Value.of_int ~width descr >>= Env.set v in
      Machine.sequence [
        set "*upc-standard-input*" 0;
        set "*upc-standard-output*" 1;
        set "*upc-error-output*" 2; (* CL name *)
        set "*upc-standard-error*" 2; (* conventional name *)
      ]

    let setup_redirections =
      Machine.Local.put state (init redirections)

    let init () =
      let open Primus.Lisp.Type.Spec in
      let def name types closure docs =
        Lisp.define ~docs ~types name closure in
      Machine.sequence [
        setup_standard_channels;
        setup_redirections;
        def "upc-channel-open" (tuple [int; int] @-> int) (module Open)
          {|(upc-channel-open PTR MODE) creates a new channel that is
            associated with a null-terminated path pointed by PTR.
            Returns a non-negative channel descriptor, if the channel
            subsystem have a mapping from the obtained path to a
            physical file and this file is accessible. Otherwise returns
            a negative value.
          |} ;
        def "upc-channel-open-network" (one int // all int @-> int) (module OpenNetwork)
          {|(upc-channel-open-network PTR) creates a new channel that is
            associated with a null-terminated path pointed by PTR.
            Returns a non-negative channel descriptor, if the channel
            subsystem have a mapping from the obtained path to a
            physical file and this file is accessible. Otherwise returns
            a negative value.
          |} ;
        def "upc-channel-open-epoll" (tuple [] @-> int) (module OpenEpoll)
          {|(upc-channel-open-epoll) creates a new epoll instance.
          |} ;
        def "upc-channel-epoll-wait" (tuple [a; b] @-> c) (module EpollWait)
          {|(upc-channel-epoll-wait) waits on events from an epoll instance.
          |} ;
        def "upc-channel-epoll-ctl" (tuple [a; b] @-> c) (module EpollCtl)
          {|(upc-channel-epoll-ctl) subscribes to events on an epoll instance.
          |} ;
        def "upc-channel-open-eventfd" (tuple [] @-> int) (module OpenEventFd)
          {|(upc-channel-open-eventfd) creates a new eventfd.
          |} ;
        def "upc-channel-open-tmpfile" (tuple []  @-> int) (module OpenTmp)
          {|(upc-channel-open-tmpfile) creates a new channel that is
            associated with a new temporary directory.
            Returns a non-negative channel descriptor, if the channel
            subsystem have a mapping from the obtained path to a
            physical file and this file is accessible. Otherwise returns
            a negative value.
          |} ;
        def "upc-channel-open-pipe" (tuple []  @-> int) (module OpenPipe)
          {|(upc-channel-open-pipe) creates a new channel that is
            associated with a pipe.
            Returns a non-negative channel descriptor, if the channel
            subsystem have a mapping from the obtained path to a
            physical file and this file is accessible. Otherwise returns
            a negative value.
          |} ;
        def "upc-channel-close"  (one int @-> int) (module Close)
          {|(upc-channel-close DESCR) closes a channel that has the
            specified descriptor DESCR. If no such channel exists,
            then returns -1. Otherwise returns 0. The descriptor of the
            closed channel will be reused by the consequent calls
            to `channel-open'. If the channel had any data associated
            with it and not yet flushed, then the data is discarded. |};
        def "upc-channel-flush"  (one int @-> int) (module Flush)
          {|(upc-channel-flush DESCR) forces data that were written to a
            channel that has the descriptor DESCR to be outputted to the
            associated destination. Returns -1 if no such channel exists or
            if in case of an IO error.|};
        def "upc-channel-input"  (one int @-> byte) (module Input)
          {|(upc-channel-input DESC) reads one byte from a channel that
            has the descriptor DESC. Returns -1 if no such channel
            exists, or if any IO error occurs, if the channel is not
            readable, or if the channel is in the end-of-file condition.|};
        def "upc-channel-output" (one int // all byte @-> int) (module Output)
          {|(upc-channel-output DESCR CHAR ...) outputs one or more
            characters to a channel that has the descriptor
            DESCR. Returns -1 if no such channel exits, if a channel
            is not writable, or if any IO error occurs in an
            associated physical file. Otherwise, returns 0.
            Note: the channel system is buffered, and the actual IO
            operation (as well as errors) could be delayed until
            (channel-flush DESCR) is called. |};
        def "upc-channel-seek" (tuple [int; int] @-> int) (module Seek)
          {|(upc-channel-seek DESCR CHAR ...) changes a file descriptor's position
            to a given offset. |};
        def "upc-channel-offset" (one int // all byte @-> int) (module Pos)
          {|(upc-channel-offset DESCR CHAR ...) fetches the current offset of a
            file descriptor. |};
        def "upc-ocaml-opendir" (tuple [a] @-> b) (module OpenDir)
          {|(upc-ocaml-opendir) opens a directory for reading..|};
        def "upc-ocaml-readdir" (tuple [a; b] @-> c) (module ReadDir)
          {|(upc-ocaml-readdir) reads the next entry for a directory..|};
        (** def "upc-ocaml-readdir" (tuple [a] @-> b) (module Readdir)
          {|(upc-ocaml-readdir) reads a dirent ..|}; *)
      ]
  end in
  Primus.Machine.add_component (module Primitives) [@warning "-D"];
  Primus.Components.register_generic "lisp-basic-io" (module Primitives)
    ~package:"upc"
    ~desc:"Provides basic IO primitives to Primus Lisp for uPolicyCraft."
