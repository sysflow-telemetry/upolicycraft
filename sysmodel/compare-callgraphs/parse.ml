#!/usr/bin/env ocaml

#use "topfind"

#require "core_kernel"
#require "dot"
#require "str"

open Array;;
open Core_kernel;;
open Odot;;
open Printf;;
open Str;;
open Sys;;

let strip_both_chars str =
  match String.length str with
  | 0 | 1 | 2 -> ""
  | len -> String.sub str 1 (len - 2);;

if (Array.length Sys.argv) < 2 then
    Printf.printf "Please provide file!\n"
else
    let file = Sys.argv.(1) in
    let graph = Odot.parse_file file in
    let stmt = graph.stmt_list in
    let name id =
      match id with
        Simple_id id -> id
      | Html_id id -> id
      | Double_quoted_id id -> id in
    let name_of_point edge =
      match edge with
        Edge_node_id nid -> nid |> fst |> name
      | Edge_subgraph _ -> "" in
    let nodes = stmt |> List.filter ~f:(fun stmt ->
                                match stmt with
                                  Stmt_node _ -> true
                                | _ -> false)
                     |> List.map ~f:(fun stmt ->
                                let Stmt_node (id, attrs) = stmt in
                                    match (List.nth attrs 1) with
                                      None -> ("", "")
                                    | Some (label, opt) ->
                                      (match opt with
                                         None -> ("", "")
                                       | Some v ->
                                         let (i, opt) = id in
                                         let nid = name i in
                                         (nid, strip_both_chars (name v)))) in
    let () = List.iter ~f:(fun (id, name) ->
            Printf.printf "%s: %s\n" id name) nodes in
    let edges = stmt |> List.filter ~f:(fun stmt ->
                                match stmt with
                                  Stmt_edge e -> true
                                | _ -> false)
                     |> List.map ~f:(fun stmt ->
                                let Stmt_edge (src, dst, attrs) = stmt in
                                let src' = name_of_point src in
                                let dst' = name_of_point (List.nth_exn dst 0) in
                                try
                                  let src'' = List.Assoc.find_exn nodes
                                                ~equal:String.equal src' in
                                  let dst'' = List.Assoc.find_exn nodes
                                                ~equal:String.equal dst' in
                                    (src'', dst'')
                                with
                                    Not_found -> (src', dst'))
                     |> List.filter ~f:(fun (s, d) ->
                                let r = Str.regexp "Node*" in
                                   (not (Str.string_match r s 0) && not (Str.string_match r d 0))) in
    let () = List.iter ~f:(fun (s, d) -> Printf.printf "%s -> %s\n" s d) edges in
    Printf.printf "Done!\n";;
