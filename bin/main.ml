open Types
open Utils
open Loader
(* open Tape_utils *)
open Compiler


let get_input msg =
  print_string msg;
  let machine_name = read_line () in
  machine_name;;

let machine_name = get_input "Enter machine name: " in
let default_value = get_input "Enter default value: " in 
let start_state = get_input "Enter start state: " in

let table = load_table ("data/" ^ machine_name ^ "/table") in
let tapes = load_all_tapes ("data/" ^ machine_name ^ "/tape") in

List.iter (print_tape;) tapes;

let heads = ref [{register=start_state;nodes=tapes}] in 

let i = ref 0 in

while (List.length !heads <> 0) do
  i := !i + 1;
  let step_function = (make_step default_value table) in
  let updated_heads = List.fold_left (fun acc x -> (step_function x) @ acc) [] !heads in 
  print_endline ("Step " ^ (string_of_int !i));
  let _ = List.fold_left (fun acc x -> print_head acc x;acc+1) 0 !heads in
  print_newline ();
  let filtered_heads = List.filter (fun x -> x.register <> "HALT") updated_heads in 
  heads := filtered_heads;
  Unix.sleepf 0.5;
done