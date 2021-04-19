#use "topfind"

(**
 #warnings "+a"
*)

#thread

#require "core"
#require "core_kernel"
#require "ppx_let"
#require "shexp.process"
#require "yojson"

open Core

open Shexp_process
open Shexp_process.Infix

module List' = Core_kernel.List

exception InvalidArgument of string

module Modes = struct

	let greedy = "--primus-greedy-scheduler"
	let promiscuous = "--primus-promiscuous-mode"

        let inetd = "--primus-uids-inetd-startup"
end

let find_test_cases test_case_dir =
  let f = Unix.opendir test_case_dir in
  let rec find_files files =
    try
      let opt = Unix.readdir_opt f in
        match opt with
          None -> files
        | Some next ->
          if String.is_prefix ~prefix:"." next then
            find_files files
          else
            find_files ((test_case_dir ^ next) :: files)
    with End_of_file ->
      let () = Unix.closedir f in
        files in
      find_files []

let parse_test_cases opt =
  match opt with
    None -> None
  | Some dir -> Some (find_test_cases dir)

let count_tests tests =
  match tests with
    None -> 0
  | Some tests -> List'.length tests

let primus_mode_of_mode mode =
  match mode with
    "greedy" -> Modes.greedy
  | "promiscuous" -> Modes.promiscuous
  | _ -> raise (InvalidArgument "Invalid mode!")

let parse_mode opt =
  match opt with
    None -> Modes.greedy
  | Some mode -> primus_mode_of_mode mode

let primus_exec_of_exec exec =
  match exec with
    "inetd" -> Modes.inetd
  | _ -> ""

let parse_exec_style opt =
  match opt with
    None -> None
  | Some style -> Some (primus_exec_of_exec style)

let parse_redirections opt =
  match opt with
    None -> ""
  | Some redirects -> redirects

let redirect_tests tests =
  List'.mapi ~f:(fun i test -> Printf.sprintf "net%d:%s" i test) tests

let handle_command binary entrypoint argv
                   container_entrypoint container_argv
                   path_length mode exec_style redirections
                   testcases reportprogress =
  let mode' = parse_mode mode in
  let tests' = parse_test_cases testcases in
  let exec_style' = parse_exec_style exec_style in
  let redirections' = parse_redirections redirections in
  let entrypoints' = Printf.sprintf "--run-entry-points=%s" entrypoint in
  let argv' = Printf.sprintf "--run-argv=%s" argv in
  let path_length' = Printf.sprintf "--primus-limit-max-length=%d" path_length in
  let container_entrypoint' = Printf.sprintf "--primus-uids-entrypoint=%s" container_entrypoint in
  let container_argv' = Printf.sprintf "--primus-uids-entrypoint-args=%s" container_argv in
  let redirections'' = match tests' with
                        None -> redirections'
                      | Some tests ->
                        (String.concat ~sep:"," (redirect_tests tests)) ^ "," ^ redirections' in
  let no_tests = Printf.sprintf "--primus-uids-no-test-cases=%d" (count_tests tests') in
  let redirections''' = Printf.sprintf "--primus-lisp-channel-redirect=%s" redirections'' in
  let bap_argv = ["config"; "exec"; "--"; "bap"; binary; "-prun";
                  entrypoints'; argv'; path_length'; mode'; "--primus-uids-model";
                  redirections'''; no_tests; container_entrypoint'; container_argv'] in
  let bap_argv' = match exec_style' with
                    None -> bap_argv
                  | Some exec -> List'.append bap_argv [exec] in
  let bap_argv'' = if reportprogress then List'.append bap_argv ["--report-progress"]
                   else bap_argv' in
  run "opam" bap_argv''

let main =
  let open Command.Let_syntax in
  let command =
    Command.basic ~summary:"uIDS Micro-Service Modeler"
      [%map_open
        let binary = anon ("binary" %: string)
        and entrypoint = anon ("entrypoint" %: string)
        and argv = anon ("argv" %: string)
        and container_entrypoint = anon ("container-entrypoint" %: string)
        and container_argv = anon ("container-argv" %: string)
        and path_length = anon ("path-length" %: int)
        and mode = flag "-m" (optional string) ~doc:"Mode The micro-execution mode (greedy|promiscuous)"
        and exec_style = flag "-e" (optional string) ~doc:"Exec An alternative execution style for the model (inetd)."
        and redirections = flag "-fs" (optional string) ~doc:"FileSystem Reveal programs to the micro-executed program to the host path/to/file:path/to/host/file."
        and testcases = flag "-t" (optional string) ~doc:"TestCases A folder containing test inputs."
        and reportprogress = flag "-r" no_arg ~doc:"ReportProgress Report micro-execution progress." in
       fun () ->
          try
            eval (handle_command binary entrypoint argv container_entrypoint container_argv path_length mode exec_style redirections testcases reportprogress)
          with e ->
            let msg = Exn.to_string e in
            Printf.eprintf "error: %s" msg
      ]
  in
  Core.Command.run command

let () =
  (main)
