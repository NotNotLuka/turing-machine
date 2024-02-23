open Types
open Utils
open Loader
open Tape_utils

let instruct = load_table "data/test";;
let x = TableMap.find "0" instruct;;
let y = TapeMap.find ["1"] x;;
let z = TapeMap.find ["0"] x;;
List.iter print_action y;;
List.iter print_action z;;

let tapes = load_all_tapes "data/tape";;

let first = (List.nth tapes 1);;

print_node first;
print_newline ();
List.iter print_node (read_tape first);
print_newline ();

let first = move Neutral first "111111" in

print_node first;
print_newline ();
List.iter print_node (read_tape first);
print_newline ();
