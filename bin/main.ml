open Types
open Utils
open Loader
(* open Tape_utils *)
open Compiler

let table = load_table "data/test";;
let tapes = load_all_tapes "data/tape";;

List.iter (print_tape;) tapes;

let heads = [{register="0";nodes=tapes}] in 

let step_function = (make_step table) in

let x = List.fold_left (fun acc x -> (step_function x) @ acc) [] heads in 

print_head (List.hd x)

