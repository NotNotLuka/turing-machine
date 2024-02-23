open Utils


  type node_data = {left: node; right: node; value: string} 
  and node = 
  | Node of node_data
  | Empty

type direction = Left | Right

let print_node node =
  match node with
  | Empty -> print_string "Empty"
  | Node {value;_} -> 
    print_string ("Node: " ^ value ^ ", ")

let move dir node new_value = 
  match dir, node with
  | Left, Node {left=Node next; _} -> Node {next with value=new_value;right=node}
  | Right, Node {right=Node next; _} -> Node {next with value=new_value;left=node}
  | Left, Node {left=Empty; _} ->  Node {left=Empty; right=node; value=new_value}
  | Right, Node {right=Empty; _} -> Node {left=node; right=Empty; value=new_value}
  | _, Empty -> failwith "Moving without a node"


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


let rec tape_init raw_tape prev =
  match raw_tape with
  | h :: t -> 
    let current = {left=prev; right=Empty; value=h} in 
    Node {current with right=(tape_init t (Node current))}
  | [] -> Empty


let rec index_tape index node = 
  match node with
  | Node n -> if index = 0 
              then Node n
              else index_tape (index-1) n.right
  | Empty -> failwith "Invalid head start index"
   
  
let load_all_tapes filename =
  let rec through_tapes file acc =
    match file with
    | [] -> List.rev acc 
    | (index) :: (data::t) -> 
              let tape_data = Str.split (Str.regexp " ") (data) in
              let tape = tape_init tape_data Empty in
              let indexed_node = index_tape (int_of_string index) tape in
              through_tapes (t) (indexed_node :: acc)
    | _ -> failwith "Incorrect formatting"
  in
  let file = read_file filename in
  through_tapes file [] 



