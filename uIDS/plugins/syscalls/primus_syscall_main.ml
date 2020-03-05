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

let system_calls = [(0,"read");
                    (1,"write");
                    (2,"open");
                    (3,"close");
                    (4,"stat");
                    (5,"fstat");
                    (6,"lstat");
                    (7,"poll");
                    (8,"lseek");
                    (9,"mmap");
                    (10,"mprotect");
                    (11,"munmap");
                    (12,"brk");
                    (13,"rt_sigaction");
                    (14,"rt_sigprocmask");
                    (15,"rt_sigreturn");
                    (16,"ioctl");
                    (17,"pread64");
                    (18,"pwrite64");
                    (19,"readv");
                    (20,"writev");
                    (21,"access");
                    (22,"pipe");
                    (23,"select");
                    (24,"sched_yield");
                    (25,"mremap");
                    (26,"msync");
                    (27,"mincore");
                    (28,"madvise");
                    (29,"shmget");
                    (30,"shmat");
                    (31,"shmctl");
                    (32,"dup");
                    (33,"dup2");
                    (34,"pause");
                    (35,"nanosleep");
                    (36,"getitimer");
                    (37,"alarm");
                    (38,"setitimer");
                    (39,"getpid");
                    (40,"sendfile");
                    (41,"socket");
                    (42,"connect");
                    (43,"accept");
                    (44,"sendto");
                    (45,"recvfrom");
                    (46,"sendmsg");
                    (47,"recvmsg");
                    (48,"shutdown");
                    (49,"bind");
                    (50,"listen");
                    (51,"getsockname");
                    (52,"getpeername");
                    (53,"socketpair");
                    (54,"setsockopt");
                    (55,"getsockopt");
                    (56,"clone");
                    (57,"fork");
                    (58,"vfork");
                    (59,"execve");
                    (60,"exit");
                    (61,"wait4");
                    (62,"kill");
                    (63,"uname");
                    (64,"semget");
                    (65,"semop");
                    (66,"semctl");
                    (67,"shmdt");
                    (68,"msgget");
                    (69,"msgsnd");
                    (70,"msgrcv");
                    (71,"msgctl");
                    (72,"fcntl");
                    (73,"flock");
                    (74,"fsync");
                    (75,"fdatasync");
                    (76,"truncate");
                    (77,"ftruncate");
                    (78,"getdents");
                    (79,"getcwd");
                    (80,"chdir");
                    (81,"fchdir");
                    (82,"rename");
                    (83,"mkdir");
                    (84,"rmdir");
                    (85,"creat");
                    (86,"link");
                    (87,"unlink");
                    (88,"symlink");
                    (89,"readlink");
                    (90,"chmod");
                    (91,"fchmod");
                    (92,"chown");
                    (93,"fchown");
                    (94,"lchown");
                    (95,"umask");
                    (96,"gettimeofday");
                    (97,"getrlimit");
                    (98,"getrusage");
                    (99,"sysinfo");
                    (100,"times");
                    (101,"ptrace");
                    (102,"getuid");
                    (103,"syslog");
                    (104,"getgid");
                    (105,"setuid");
                    (106,"setgid");
                    (107,"geteuid");
                    (108,"getegid");
                    (109,"setpgid");
                    (110,"getppid");
                    (111,"getpgrp");
                    (112,"setsid");
                    (113,"setreuid");
                    (114,"setregid");
                    (115,"getgroups");
                    (116,"setgroups");
                    (117,"setresuid");
                    (118,"getresuid");
                    (119,"setresgid");
                    (120,"getresgid");
                    (121,"getpgid");
                    (122,"setfsuid");
                    (123,"setfsgid");
                    (124,"getsid");
                    (125,"capget");
                    (126,"capset");
                    (127,"rt_sigpending");
                    (128,"rt_sigtimedwait");
                    (129,"rt_sigqueueinfo");
                    (130,"rt_sigsuspend");
                    (131,"sigaltstack");
                    (132,"utime");
                    (133,"mknod");
                    (134,"uselib");
                    (135,"personality");
                    (136,"ustat");
                    (137,"statfs");
                    (138,"fstatfs");
                    (139,"sysfs");
                    (140,"getpriority");
                    (141,"setpriority");
                    (142,"sched_setparam");
                    (143,"sched_getparam");
                    (144,"sched_setscheduler");
                    (145,"sched_getscheduler");
                    (146,"sched_get_priority_max");
                    (147,"sched_get_priority_min");
                    (148,"sched_rr_get_interval");
                    (149,"mlock");
                    (150,"munlock");
                    (151,"mlockall");
                    (152,"munlockall");
                    (153,"vhangup");
                    (154,"modify_ldt");
                    (155,"pivot_root");
                    (156,"_sysctl");
                    (157,"prctl");
                    (158,"arch_prctl");
                    (159,"adjtimex");
                    (160,"setrlimit");
                    (161,"chroot");
                    (162,"sync");
                    (163,"acct");
                    (164,"settimeofday");
                    (165,"mount");
                    (166,"umount2");
                    (167,"swapon");
                    (168,"swapoff");
                    (169,"reboot");
                    (170,"sethostname");
                    (171,"setdomainname");
                    (172,"iopl");
                    (173,"ioperm");
                    (174,"create_module");
                    (175,"init_module");
                    (176,"delete_module");
                    (177,"get_kernel_syms");
                    (178,"query_module");
                    (179,"quotactl");
                    (180,"nfsservctl");
                    (181,"getpmsg");
                    (182,"putpmsg");
                    (183,"afs_syscall");
                    (184,"tuxcall");
                    (185,"security");
                    (186,"gettid");
                    (187,"readahead");
                    (188,"setxattr");
                    (189,"lsetxattr");
                    (190,"fsetxattr");
                    (191,"getxattr");
                    (192,"lgetxattr");
                    (193,"fgetxattr");
                    (194,"listxattr");
                    (195,"llistxattr");
                    (196,"flistxattr");
                    (197,"removexattr");
                    (198,"lremovexattr");
                    (199,"fremovexattr");
                    (200,"tkill");
                    (201,"time");
                    (202,"futex");
                    (203,"sched_setaffinity");
                    (204,"sched_getaffinity");
                    (205,"set_thread_area");
                    (206,"io_setup");
                    (207,"io_destroy");
                    (208,"io_getevents");
                    (209,"io_submit");
                    (210,"io_cancel");
                    (211,"get_thread_area");
                    (212,"lookup_dcookie");
                    (213,"epoll_create");
                    (214,"epoll_ctl_old");
                    (215,"epoll_wait_old");
                    (216,"remap_file_pages");
                    (217,"getdents64");
                    (218,"set_tid_address");
                    (219,"restart_syscall");
                    (220,"semtimedop");
                    (221,"fadvise64");
                    (222,"timer_create");
                    (223,"timer_settime");
                    (224,"timer_gettime");
                    (225,"timer_getoverrun");
                    (226,"timer_delete");
                    (227,"clock_settime");
                    (228,"clock_gettime");
                    (229,"clock_getres");
                    (230,"clock_nanosleep");
                    (231,"exit_group");
                    (232,"epoll_wait");
                    (233,"epoll_ctl");
                    (234,"tgkill");
                    (235,"utimes");
                    (236,"vserver");
                    (237,"mbind");
                    (238,"set_mempolicy");
                    (239,"get_mempolicy");
                    (240,"mq_open");
                    (241,"mq_unlink");
                    (242,"mq_timedsend");
                    (243,"mq_timedreceive");
                    (244,"mq_notify");
                    (245,"mq_getsetattr");
                    (246,"kexec_load");
                    (247,"waitid");
                    (248,"add_key");
                    (249,"request_key");
                    (250,"keyctl");
                    (251,"ioprio_set");
                    (252,"ioprio_get");
                    (253,"inotify_init");
                    (254,"inotify_add_watch");
                    (255,"inotify_rm_watch");
                    (256,"migrate_pages");
                    (257,"openat");
                    (258,"mkdirat");
                    (259,"mknodat");
                    (260,"fchownat");
                    (261,"futimesat");
                    (262,"newfstatat");
                    (263,"unlinkat");
                    (264,"renameat");
                    (265,"linkat");
                    (266,"symlinkat");
                    (267,"readlinkat");
                    (268,"fchmodat");
                    (269,"faccessat");
                    (270,"pselect6");
                    (271,"ppoll");
                    (272,"unshare");
                    (273,"set_robust_list");
                    (274,"get_robust_list");
                    (275,"splice");
                    (276,"tee");
                    (277,"sync_file_range");
                    (278,"vmsplice");
                    (279,"move_pages");
                    (280,"utimensat");
                    (281,"epoll_pwait");
                    (282,"signalfd");
                    (283,"timerfd_create");
                    (284,"eventfd");
                    (285,"fallocate");
                    (286,"timerfd_settime");
                    (287,"timerfd_gettime");
                    (288,"accept4");
                    (289,"signalfd4");
                    (290,"eventfd2");
                    (291,"epoll_create1");
                    (292,"dup3");
                    (293,"pipe2");
                    (294,"inotify_init1");
                    (295,"preadv");
                    (296,"pwritev");
                    (297,"rt_tgsigqueueinfo");
                    (298,"perf_event_open");
                    (299,"recvmmsg");
                    (300,"fanotify_init");
                    (301,"fanotify_mark");
                    (302,"prlimit64");
                    (303,"name_to_handle_at");
                    (304,"open_by_handle_at");
                    (305,"clock_adjtime");
                    (306,"syncfs");
                    (307,"sendmmsg");
                    (308,"setns");
                    (309,"getcpu");
                    (310,"process_vm_readv");
                    (311,"process_vm_writev");
                    (312,"kcmp");
                    (313,"finit_module")]
(**
 * A mapping of OS system calls to SysFlow system calls.
 * SysFlow records a subset of OS system calls relevant for Security.
 **)
let sysflow_calls = [("execve", "execve");
                     ("accept", "accept");
                     ("bind", "bind");
                     ("recvmsg", "recv");
                     ("sendmsg", "send");
                     ("clone", "clone")]

type event = Jmp | Def

let pp_event out evt = match evt with
                     Jmp -> fprintf out "Jmp "
                   | Def -> fprintf out "Def "

let pp_history out history = List.iter ~f:(fun e -> pp_event out e) history; fprintf out "\n"

let push_history evt xs =
  match xs with
    [] -> evt :: []
  | top :: _ -> if top = evt then xs else evt :: xs

(**
 *  addrs: The addresses of syscall instructions in the binary.
 *  vals: An assoc list that stores the concrete value of RAX
 *        at each system call address.
 *  regs: An assoc list that stores the definition of all regs
 *        during the execution of the binary.
 **)
type state = {
    addrs : int list;
    vals : (string * ((int * int) list)) list;
    regs : (string * int) list;
    history : int list;
    stack : string list;
}

module SS = Set.Make(String);;

let address_of_pos out addr =
  match Bitvector.to_int addr with
    Error s ->
      (** fprintf out "Error converting int!\n"; *)
      -1
  | Ok v ->
      (** fprintf out "SYSFLOW: Cursor at %#08x\n" v; *)
      v

let state = Primus.Machine.State.declare
    ~name:"primus-syscall"
    ~uuid:"c4696d2f-5d8e-42b4-a65c-4ea6269ce9d1"
    (fun _ -> {addrs = []; vals = []; regs = []; history = []; stack= []})

let collect_syscalls out name insns =
  (**
  let () = Seq.iter insns ~f:(fun (mem, i) ->
             let asm = (Insn.asm i) in
             fprintf out "  %s\n" asm) in
  *)
  let int_of_mem = (fun mem ->
    let err = mem |> Memory.min_addr |> Bitvector.to_int in
      match err with
        Error err -> -1
      | Ok i -> i) in
  let detect_syscall = (fun (mem, i) ->
    let asm = (Insn.asm i) in
      String.equal asm "syscall"
  ) in
  Seq.filter ~f:detect_syscall insns |>
  Seq.map ~f:fst |>
  Seq.map ~f:int_of_mem

let find_value regs out e = (object
  inherit [int] Exp.visitor
  method! enter_int word v =
    match Bitvector.to_int word with
      Error s -> ~-1
    | Ok v' -> v'
  method! enter_var reg v =
    let rname = Var.name reg in
    (**
    let () = fprintf out "Lookup var: %s\n" rname in
    *)
      match List.Assoc.find regs ~equal:String.equal rname with
        None -> v
      | Some v' -> v'
end)#visit_exp e ~-1

(** SysCall Instruction: 0f 05 **)
let syscall_length = 2

let top stack =
        stack |> List.rev |> List.hd

let start_monitoring {Config.get=(!)} =
  let out = std_formatter in
  let module Monitor(Machine : Primus.Machine.S) = struct
    module Eval = Primus.Interpreter.Make(Machine)
    open Machine.Syntax

    (** Prevent Primus from Exploring other Functions. *)
    let stop_call j = Machine.current () >>= fun pid ->
      let kind = Jmp.kind j in
        match kind with
          Call _ -> Eval.halt >>= never_returns
        | _ -> Machine.return()

    let record_sub s = Machine.current () >>= fun pid ->
      Machine.Local.update state ~f:(fun state ->
        let subname = Sub.name s in
        (**
          let () = fprintf out "Entering %s\n" subname in
         *)
        {state with stack=subname :: state.stack}
      )

    let record_def t =
            Machine.Local.update state ~f:(fun {addrs;vals;regs;history;stack} ->
        let lhs = Def.lhs t in
        let reg = Var.name lhs in
        if reg = "RBP" || reg = "RSP" then
          {addrs;vals;regs;history;stack}
        else
          let exp = t |> Def.rhs |> Exp.normalize |> Exp.simpl in
          let v = find_value regs out exp in
          (**
          let () = fprintf out "SYSFLOW: %s := %d\n" reg v in
          *)
          let regs' = List.Assoc.add regs ~equal:String.equal reg v in
          {addrs=addrs; vals=vals; regs=regs'; history=history;stack}
      )

    let record_pos p = Machine.current () >>= fun pid ->
      match (Primus.Pos.get address p) with
        None -> Machine.return()
      | Some addr -> Machine.Local.update state ~f:(fun state ->
        let a = address_of_pos out addr in
        (**
        let () = fprintf out "Visiting addr at %x\n" a in
        *)
        let {addrs;vals;regs;history;stack} = state in
        let hit sa =
          (**
          let () = fprintf out "Checking %x = %x\n" sa a in
          *)
          let target = sa + syscall_length in
            a = target || a = target+1 in
        let rax sa =
          (sa, (List.Assoc.find_exn regs ~equal:String.equal "RAX")) in
        let opt = addrs |>
                  List.filter ~f:hit |>
                  List.map ~f:rax |>
                  List.hd in
        match top stack with
          None ->
            (**
            let () = fprintf out "No context!\n" in
            *)
            {state with history=push_history a state.history}
        | Some context ->
          let cvals = match List.Assoc.find vals ~equal:String.equal context with
                        None -> []
                      | Some v -> v in
          let cvals' = match opt with
                        None -> cvals
                      | Some (addr, syscall) ->
                          if List.length history >= 3 then
                            let x :: y :: z :: _ = history in
                            (**
                             * When micro-execution jumps and lands right after a
                             * syscall, make sure we don't mark it as visited.
                             * *)
                            if (a - x) >= 9 ||
                               ((a - x) = 1 && (a - y) >= 9) ||
                               ((a = x) && (a - y) = 1 && (a - z) >= 9) then
                              cvals
                            else
                              (**
                              let () = fprintf out "Assoc %x with %x syscall\n" addr syscall in
                              *)
                              List.Assoc.add cvals ~equal:(=) addr syscall
                          else
                            List.Assoc.add cvals ~equal:(=) addr syscall in
          let vals' = List.Assoc.add vals ~equal:String.equal context cvals' in
          {addrs=addrs; vals=vals'; regs=regs; history=push_history a history;stack})

    let record_syscalls blk = Machine.Local.get state >>= fun state' ->
      (**
      let () = fprintf out "Leaving block!!\n" in
      *)
        Machine.Global.update state ~f:(fun state'' ->
              (**
              let () = fprintf out "Updating global state!\n" in
              let () = fprintf out "Vals %d\n" (List.length state'.vals) in
              let () = fprintf out "Vals %d\n" (List.length state''.vals) in
              *)
          {state'' with vals=state'.vals @ state''.vals})

    let pp_list ppf vals =
        List.iter ~f:(fun (x,y) -> fprintf ppf "(%d,%d)" x y) vals

    let print_syscalls () =
      Machine.current () >>= fun pid ->
        if Machine.global = pid then
          Machine.Global.get state >>= fun state' ->
          (**
          let () = fprintf out "Global Machine Ending!\n" in
          *)
          let {addrs;vals} = state' in
          (**
          let () = fprintf out "Vals %d\n" (List.length vals) in
          *)
          let sorted = List.sort (fun x y -> compare (fst x) (fst y)) vals in
          (**
          let () = List.iter ~f:(fun (context, vals) -> fprintf out "%s %a\n" context pp_list vals) sorted in
          *)
          let rec merge xs =
            match xs with
            | (x, xv) :: (y, yv) :: xs ->
              if x = y then
                merge ((x, xv @ yv) :: xs)
              else
                (x, xv) :: merge ((y, yv) :: xs)
            | _ -> xs in
          let summarize (context, vals) =
            (**
            let () = fprintf out "Context %s\n" context in
            *)
            let syscalls =
              vals |>
              List.map ~f:(fun (addr, rax) ->
                match List.Assoc.find system_calls ~equal:(=) rax with
                  None -> (-1,"none")
                | Some syscall ->
                  (addr, syscall)) |>
              List.map ~f:snd |>
              SS.of_list |>
              SS.to_list |>
              List.map ~f:(fun s ->
                      List.Assoc.find sysflow_calls ~equal:String.equal s) |>
              List.filter ~f:(fun opt -> opt <> None) |>
              List.map ~f:(fun opt -> let Some s = opt in s) in
              (**
              let () = fprintf out "Model:\n" in
              let () = List.iter ~f:(fun s -> fprintf out "%s\n" s) syscalls in
              *)
            let jscalls = List.map ~f:(fun syscall -> `String syscall) syscalls in
            `Assoc [("function", `String context); ("syscalls", `List jscalls)] in
          let result = (`List (List.map ~f:summarize (merge sorted))) in
          let output = Yojson.Basic.pretty_to_string result in
          let () = fprintf out "%s\n" output in
          Machine.return()
        else
          Machine.Local.get state >>= fun state' ->
            (**
            let () = fprintf out "Local Machine Ending!\n" in
            *)
            Machine.Global.update state ~f:(fun state'' ->
              (**
              let () = fprintf out "Updating global state!\n" in
              let () = fprintf out "Vals %d\n" (List.length state'.vals) in
              let () = fprintf out "Vals %d\n" (List.length state''.vals) in
              *)
              {state'' with vals=state'.vals @ state''.vals})
        (**
        let findcall addr =
          match List.Assoc.find vals ~equal:(=) addr with
            None -> ()
          | Some rax ->
              (match List.Assoc.find system_calls ~equal:(=) rax with
                 None -> ()
               | Some syscall ->
                 fprintf out "Hit %#010x: %s\n" addr syscall) in
        List.iter ~f:findcall addrs
        *)

    let setup_tracing () =
      Machine.List.sequence [
          (**
            Primus.Interpreter.enter_jmp >> stop_call;
          *)
          Primus.Interpreter.enter_sub >>> record_sub;
          Primus.Interpreter.enter_def >>> record_def;
          Primus.Interpreter.enter_pos >>> record_pos;
          Primus.Interpreter.leave_blk >>> record_syscalls;
          Primus.Machine.finished >>> print_syscalls;
      ]

    module G = Graphs.Cfg

    let init () =
      setup_tracing () >>= fun () ->
        Machine.get () >>= fun proj ->
          let symtab = Project.symbols proj in
          let symtabs = (Symtab.to_sequence symtab) in
          (**
          let () = fprintf out "No. of Functions: %d\n" (Seq.length symtabs) in
          *)
          let find_syscalls sc symtab =
            let (name, _, cfg) = symtab in
              (**
              let () = fprintf out "Examining %s:\n" name in
              *)
              let blocks = cfg |>
                Graphlib.reverse_postorder_traverse (module G) in
              (**
              let () = fprintf out " has %d basic blocks\n" (Seq.length blocks) in
              *)
              let insns = blocks |>
                          Seq.map ~f:Block.insns |>
                          Seq.concat_map ~f:Seq.of_list in
              (**
              let () = fprintf out " has %d instructions\n" (Seq.length insns) in
              *)
              insns |>
              collect_syscalls out name |>
              Seq.append sc in
          let syscalls = Seq.fold ~init:Seq.empty ~f:find_syscalls symtabs in
            (**
            syscalls |>
            Seq.to_list |>
            List.sort ~compare:compare |>
            List.iter ~f:(fun mem -> fprintf out "%8x\n" mem);
            fprintf out "Found %d syscall instructions.\n" (Seq.length syscalls);
            *)
            Machine.Local.update state ~f:(fun s ->
              {addrs=Seq.to_list syscalls; vals=[]; regs=[]; history=[]; stack=[]}
            )
  end in
  Primus.Machine.add_component (module Monitor)

let () = Config.when_ready start_monitoring
