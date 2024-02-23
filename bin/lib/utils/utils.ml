open Types

let read_tape node = 
  let rec left_aux node acc = 
    match node with
    | Node left_node-> 
      left_aux (left_node.left) (node :: acc)
    | Empty -> acc in 
  let rec right_aux node acc =
    match node with
    | Node right_node -> 
      right_aux (right_node.right) (node :: acc)
    | Empty -> List.rev acc in
  
  match node with
  | Empty -> []
  | Node node -> left_aux node.left [] @ [Node node] @ (right_aux node.right [])

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
  Printf.printf "write: %s, dir: %s, next_state: %s\n"
    action.write
    (string_of_direction action.dir)
    action.next_state


let print_node node =
  match node with
  | Empty -> print_string "Empty"
  | Node {value;_} -> 
    print_string ("Node: " ^ value ^ ", ")

