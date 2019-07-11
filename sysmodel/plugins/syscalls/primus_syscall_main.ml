open Core_kernel
open Bap.Std
open Bap_primus.Std
open Bap_future.Std
open Monads.Std
open Format

include Self()

module Param = struct
  open Config;;
  manpage [
    `S "DESCRIPTION";
    `P "Monitors a Lisp Machine execution."
  ]

  let monitors = param (list string) "observations1"
      ~doc:
        "A list of observations to print. A keyword `all` can be use to
      select all events. To ignore a particular event, add `-' before
      the name. An optional + is allowed for the consistency."

  let rules = param (list string) "rules"
end

let starts_with name x =
  String.length name > 1 && name.[0] = x

let strip name =
  if starts_with name '+' || starts_with name '-'
  then String.subo ~pos:1 name
  else name

let has_name name p =
  let pname = Primus.Observation.Provider.name p in
  let () = printf "Provider: %s\n" pname in
    pname = name

let remove_provider name = List.filter ~f:(Fn.non (has_name name))

let monitor_provider name ps =
  Primus.Observation.list_providers () |>
  List.find ~f:(has_name name) |> function
  | None -> invalid_argf "An unknown observation provider `%s'" name ()
  | Some p -> p :: ps

let parse_monitors =
  List.fold ~init:[] ~f:(fun ps -> function
      | "all" -> ps @ Primus.Observation.list_providers ()
      | name when starts_with name '-' -> remove_provider (strip name) ps
      | name -> monitor_provider (strip name) ps)

let print_event out p ev =
  fprintf out "@[(%s %a)@]@\n%!"
    (Primus.Observation.Provider.name p) Sexp.pp_hum ev

let id ppf pos =
  fprintf ppf "%a" Tid.pp (Primus.Pos.tid pos)

let print_pos ppf pos =
  let open Primus.Pos in
  match pos with
  | Top _ -> ()
  | Sub {me} -> fprintf ppf "%a: <%s>@\n" id pos (Sub.name me)
  | Blk _ -> fprintf ppf "%a:@\n" id pos
  | Arg {me} -> fprintf ppf "%a" Arg.pp me
  | Phi {me} -> fprintf ppf "%a" Phi.pp me
  | Def {me} -> fprintf ppf "%a" Def.pp me
  | Jmp {me} -> fprintf ppf "%a" Jmp.pp me

let rule_providers rule =
  Bare.Rule.lhs rule |> List.concat_map ~f:(function
      | Sexp.Atom x
      | Sexp.List (Sexp.Atom x :: _) ->
        if String.length x > 0 && x.[0] = '?'
        then Primus.Observation.list_providers () |>
             List.map ~f:Primus.Observation.Provider.name
        else [x]
      | _ ->
        warning "Rule %a won't match with any observation"
          Bare.Rule.pp rule;
        [])

let print_trace ppf = List.iter ~f:(print_pos ppf)

type state = {
  trace : Primus.pos list;
  syscalls : int;
}


let concat streams =
  let stream,main = Stream.create () in
  List.iter streams ~f:(fun stream ->
      Stream.observe stream (fun x ->
          Signal.send main x));
  stream,main

(* returns a stream of derived facts, each element of the stream is
   a non-empty list of facts provided from some fact in the list of
   facts or another derived fact.  *)
let process_rule rule =
  let module Prov = Primus.Observation.Provider in
  let observing = String.Set.of_list (rule_providers rule) in
  let facts,to_facts =
    Primus.Observation.list_providers () |>
    List.filter ~f:(fun p -> Set.mem observing (Prov.name p)) |>
    List.map ~f:(fun p -> Prov.data p |> Stream.map ~f:(fun ev ->
        Sexp.List [
          Sexp.Atom (Prov.name p);
          ev
        ])) |> concat in
  Stream.parse facts ~init:rule ~f:(fun rule ev ->
      let rule,facts = Bare.Rule.apply rule ev in
      List.iter facts ~f:(Signal.send to_facts);
      match facts with
      | [] -> None,rule
      | facts -> Some facts,rule)

let read_rules filename =
  match Bare.Rule.from_file filename with
  | Ok rules -> rules
  | Error err ->
    let err = asprintf "%a"
        (Bare.Rule.report_error ~filename) err in
    invalid_arg err


let setup_rules_processor out rules =
  rules |>
  List.concat_map ~f:read_rules |>
  List.map ~f:process_rule |>
  List.iter ~f:(fun facts ->
      Stream.observe facts
        (List.iter ~f:(fprintf out "%a@\n%!" Sexp.pp_hum)))

let state = Primus.Machine.State.declare
    ~name:"primus-syscall"
    ~uuid:"c4696d2f-5d8e-42b4-a65c-4ea6269ce9d1"
    (fun _ -> {trace = []; syscalls = 0})

let start_monitoring {Config.get=(!)} =
  let out = std_formatter in
  setup_rules_processor out !Param.rules;
  let module Monitor(Machine : Primus.Machine.S) = struct
    open Machine.Syntax

    let record_trace p =
      Machine.Local.update state ~f:(fun s ->
        {trace = p :: s.trace; syscalls = 0})

    let print_trace () =
      Machine.Local.get state >>| fun {trace;syscalls} ->
        fprintf out "syscalls %d\n" syscalls
        (** print_trace out trace; *)

    let setup_tracing () =
      Machine.List.sequence [
          Primus.Interpreter.enter_pos >>> record_trace;
          Primus.Machine.finished >>> print_trace;
      ]

    let init () =
      setup_tracing () >>= fun () ->
      parse_monitors !Param.monitors |>
      List.iter ~f:(fun m ->
          info "monitoring %s" (Primus.Observation.Provider.name m);
          Stream.observe (Primus.Observation.Provider.data m)
            (print_event out m));
      Machine.return ()
  end in
  Primus.Machine.add_component (module Monitor)

let () = Config.when_ready start_monitoring
