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

let state = Primus.Machine.State.declare
    ~name:"uids"
    ~uuid:"ba442400-63dd-11ea-a41a-06f59637065f"
    (fun _ -> 0)

let address_of_pos out addr =
  match Bitvector.to_int addr with
    Error s ->
    (** fprintf out "Error converting int!\n"; *)
    -1
  | Ok v ->
    (** fprintf out "SYSFLOW: Cursor at %#08x\n" v; *)
    v

let out = std_formatter

module Monitor(Machine : Primus.Machine.S) = struct
  module Eval = Primus.Interpreter.Make(Machine)
  open Machine.Syntax

  let record_pos p = Machine.current () >>= fun pid ->
    match (Primus.Pos.get address p) with
      None -> Machine.return ()
    | Some addr -> 
      let a = address_of_pos out addr in
      let () = info "visiting %x\n" a in
      Machine.return ()

  let setup_tracing () =
    Machine.List.sequence [
      Primus.Interpreter.enter_pos >>> record_pos;  
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
