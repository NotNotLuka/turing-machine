(* open Tape *)
(* open Compiler *)
(* let first = Node { value = Int 1; left = Empty; right = Empty};; *)


(* let instruct = Compiler.compile_table "data/test";;
let x = Compiler.TableMap.find "0" instruct;;
let y = Compiler.TapeMap.find "1" x;;
let z = Compiler.TapeMap.find "0" x;;
print_action y;;
print_action z;; *)

let tapes = Tape.load_all_tapes "data/tape";;

let first = (List.nth tapes 1);;

Tape.print_node first;
print_newline ();
List.iter Tape.print_node (Tape.read_tape first);
print_newline ();

(* let through_instructions =
  List.fold_left (fun tape instr -> move instr.dir tape (Int instr.value)) first instruct;;

let m = (Tape.read_tape through_instructions);;
Tape.print_list Tape.print_node m;; *)
