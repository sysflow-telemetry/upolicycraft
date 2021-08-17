#use "topfind"

(*
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
  let f = Core.Unix.opendir test_case_dir in
  let rec find_files files =
    try
      let opt = Core.Unix.readdir_opt f in
      match opt with
      | None -> List'.sort ~compare:String.compare files
      | Some next ->
          if String.is_prefix ~prefix:"." next then find_files files
          else find_files ((test_case_dir ^ next) :: files)
    with End_of_file ->
      let () = Unix.closedir f in
      files
  in
  find_files []

let walk_directory_tree dir =
  let rec walk acc = function
    | [] -> acc
    | dir :: tail ->
        let contents = Array.to_list (Sys.readdir dir) in
        let contents =
          List'.map ~f:(fun file -> Filename.concat dir file) contents
        in
        let dirs, files =
          List'.fold_left
            ~f:(fun (dirs, files) f ->
              match (Unix.stat f).st_kind with
              | S_REG -> (dirs, f :: files) (* Regular file *)
              | S_DIR -> (f :: dirs, f :: files) (* Directory *)
              | _ -> (dirs, files) )
            ~init:([], []) contents
        in
        walk (files @ acc) (dirs @ tail)
  in
  walk [] [dir]

let parse_test_cases opt =
  match opt with None -> None | Some dir -> Some (find_test_cases dir)

let count_tests tests =
  match tests with None -> 0 | Some tests -> List'.length tests

let primus_mode_of_mode mode =
  match mode with
  | "greedy" -> Modes.greedy
  | "promiscuous" -> Modes.promiscuous
  | _ -> raise (InvalidArgument "Invalid mode!")

let parse_mode opt =
  match opt with None -> Modes.greedy | Some mode -> primus_mode_of_mode mode

let primus_exec_of_exec exec =
  match exec with "inetd" -> Modes.inetd | _ -> ""

let parse_exec_style opt =
  match opt with
  | None -> None
  | Some style -> Some (primus_exec_of_exec style)

let parse_redirections opt =
  match opt with None -> "" | Some redirects -> redirects

let redirect_tests tests =
  List'.mapi ~f:(fun i test -> Printf.sprintf "net%d:%s" i test) tests

let add_exec_style exec_style bap_argv =
  match exec_style with
  | None -> bap_argv
  | Some exec -> List'.append bap_argv [exec]

let add_environment env bap_argv =
  let default = "--run-env=PATH=/usr/bin,PWD=/root" in
  match env with
  | None -> List'.append bap_argv [default]
  | Some env ->
      let env' = default ^ "," ^ env ^ "'" in
      let () = Printf.printf "env : %s" env' in
      List'.append bap_argv [env']

let add_report_progress reportprogress bap_argv =
  if reportprogress then List'.append bap_argv ["--report-progress"]
  else bap_argv

let handle_command binary entrypoint argv container_entrypoint container_argv
    path_length mode exec_style redirections filesystem env testcases
    reportprogress verbose =
  let mode' = parse_mode mode in
  let tests' = parse_test_cases testcases in
  let exec_style' = parse_exec_style exec_style in
  let redirections' = parse_redirections redirections in
  let entrypoints' = Printf.sprintf "--run-entry-points=%s" entrypoint in
  let argv' = Printf.sprintf "--run-argv=%s" argv in
  let path_length' =
    Printf.sprintf "--primus-limit-max-length=%d" path_length
  in
  let container_entrypoint' =
    Printf.sprintf "--primus-uids-entrypoint=%s" container_entrypoint
  in
  let container_argv' =
    Printf.sprintf "--primus-uids-entrypoint-args=%s" container_argv
  in
  let redirections'' =
    match tests' with
    | None -> redirections'
    | Some tests ->
        String.concat ~sep:"," (redirect_tests tests) ^ "," ^ redirections'
  in
  let redirections''' =
    match filesystem with
    | None -> redirections''
    | Some fs ->
        let mappings =
          fs |> walk_directory_tree
          |> List'.map ~f:(fun file ->
                 Printf.sprintf "%s:%s"
                   (String.chop_prefix_exn ~prefix:fs file)
                   file )
          |> String.concat ~sep:","
        in
        redirections'' ^ "," ^ mappings
  in
  let no_tests =
    Printf.sprintf "--primus-uids-no-test-cases=%d" (count_tests tests')
  in
  let uids_filesystem =
    Printf.sprintf "--primus-uids-filesystem=%s" redirections'''
  in
  let uids_redirects =
    Printf.sprintf "--primus-uids-redirect=%s" redirections'''
  in
  let bap_argv =
    [ "config"
    ; "exec"
    ; "--"
    ; "bap"
    ; binary
    ; "-prun"
    ; entrypoints'
    ; argv'
    ; path_length'
    ; mode'
    ; "--primus-uids-model"
    ; uids_filesystem
    ; uids_redirects
    ; no_tests
    ; container_entrypoint'
    ; container_argv' ]
  in
  let bap_argv' =
    bap_argv |> add_environment env |> add_exec_style exec_style'
    |> add_report_progress reportprogress
  in
  if verbose then
    printf "%s" (String.concat ~sep:" " bap_argv') >> run "opam" bap_argv'
  else run "opam" bap_argv'

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
        and mode =
          flag "-m" (optional string)
            ~doc:"Mode The micro-execution mode (greedy|promiscuous)"
        and exec_style =
          flag "-s" (optional string)
            ~doc:"Exec An alternative execution style for the model (inetd)."
        and redirections =
          flag "-redirects" (optional string)
            ~doc:
              "FileSystem Reveal programs to the micro-executed program to \
               the host path/to/file:path/to/host/file."
        and filesystem =
          flag "-fs" (optional string)
            ~doc:"FileSystem A directory containing the image's filesystem."
        and env =
          flag "-e" (optional string)
            ~doc:"Environment The program's environment variables."
        and testcases =
          flag "-t" (optional string)
            ~doc:"TestCases A folder containing test inputs."
        and reportprogress =
          flag "-r" no_arg
            ~doc:"ReportProgress Report micro-execution progress."
        and verbose =
          flag "-v" no_arg ~doc:"Verbose Show the BAP command executed."
        in
        fun () ->
          try
            eval
              (handle_command binary entrypoint argv container_entrypoint
                 container_argv path_length mode exec_style redirections
                 filesystem env testcases reportprogress verbose)
          with e ->
            let msg = Exn.to_string e in
            Printf.eprintf "error: %s" msg]
  in
  Core.Command.run command

let () = main
