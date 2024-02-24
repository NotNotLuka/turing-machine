open Types
open Tape_utils

(* https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml#5775024 *)
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

let string_of_direction = function
| Left -> "Left"
| Right -> "Right"
| Neutral -> "Neutral"

let print_action action =
  print_string "write: ";
  List.iter print_string action.write;print_newline ();
  print_string "dir: ";
  List.iter (fun x -> print_string (" " ^ (string_of_direction x))) action.dir;print_newline ();
  print_endline ("next_state:" ^ action.next_state ^ "\n")


let print_node node =
  match node with
  | Empty -> print_string "Empty"
  | Node {value;_} -> print_string (value ^ " ")


let print_tape tape =
  match tape with
  | Node node ->
    let left = left_side_of_tape node.left in 
    let right = right_side_of_tape node.right in
    List.iter print_node left;
    print_string ("|" ^ node.value ^ "| ");
    List.iter print_node right;
    ()
  | Empty -> ()

let print_head n head =
  print_endline ("Printing head " ^ (string_of_int n));
  print_string ("register: " ^ head.register ^ "\n");
  List.iter print_tape head.nodes;print_newline ();
  ()
