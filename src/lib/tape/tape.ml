

module type TAPE = sig
  type node_data = {left: node; right: node; value: string} 
  and node = 
  | Node of node_data
  | Empty
  type head = {register: string; nodes: node list}

  val print_node: node -> unit
  val print_head: int -> head -> unit
  val print_tape: node -> unit 
end


module Tape : TAPE = struct
  type node_data = {left: node; right: node; value: string} 
  and node = 
  | Node of node_data
  | Empty
  type head = {register: string; nodes: node list}
  
  (* functions to get a full list of values *)
  let left_side_of_tape node =
    let rec left_aux node acc = 
      match node with
      | Node left_node-> 
        left_aux (left_node.left) (node :: acc)
      | Empty -> acc in 
    left_aux node []
  let right_side_of_tape node =
    let rec right_aux node acc =
      match node with
      | Node right_node -> 
        right_aux (right_node.right) (node :: acc)
      | Empty -> List.rev acc in
    right_aux node []

  (* let read_tape node = 
    match node with
    | Empty -> []
    | Node node -> left_side_of_tape node.left @ [Node node] @ (right_side_of_tape node.right) *)

  (* print functions *)
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
end

module type TAPE_MOV = sig
  open Table.Table
  open Tape
  val make_step : string -> action list TapeMap.t TableMap.t -> head -> head list
end

(* Module for executing tape movements *)
module Tape_mov : TAPE_MOV = struct
  open Tape
  open Table.Table
  let move dir node new_value default_val = 
    match dir, node with
    (* when writing a new value to the current node. 
        We leave the neighbour we're moving to as Empty
        as we cannot define them within each other. 
        We can do this as it gets fixed when moving to that node
          so we never detect the mistake*)
    | Left, Node {left=Node next; right=right;_} -> 
      let new_cur = Node {left=Empty;right=right;value=new_value} in
      Node {next with right=new_cur}
  
    | Right, Node {right=Node next; left=left;_} -> 
      let new_cur = Node {right=Empty;left=left;value=new_value} in
      Node {next with left=new_cur}
  
    | Left, Node {left=Empty;right=right;_} ->  
      let new_cur = Node {left=Empty;right=right;value=new_value} in
      Node {left=Empty; right=new_cur; value=default_val}
  
    | Right, Node {right=Empty;left=left;_} -> 
      let new_cur = Node {right=Empty;left=left;value=new_value} in
      Node {left=new_cur; right=Empty; value=default_val}
  
    | Neutral, Node next -> Node {next with value=new_value}
    | _, Empty -> failwith "Moving without a node"

  let read_tape_nodes nodes =
    let rec aux nodes acc = 
      match nodes with
      | [] -> (List.rev acc)
      | (Empty::_) -> failwith "Somehow ended up on an empty node"
      | (Node h)::t -> aux t (h.value :: acc) in 
    aux nodes []

  let determine_moves table head =
    let tape_state = read_tape_nodes head.nodes in 
    match TableMap.find_opt head.register table with
    | Some tape_table ->
      begin
        match TapeMap.find_opt tape_state tape_table with
        | Some acts -> acts
        | None -> []
      end
    | None -> []

  let execute_move default_val og_head movement =
    let rec through_tapes dirs values tapes acc =
      match dirs, values, tapes with
      | (dir::t_dir), (value::t_value), (tape::t_tape) ->
        through_tapes t_dir t_value t_tape ((move dir tape value default_val) :: acc)
      | [], [], [] -> acc
      | _ -> failwith "Incorrect size" in 
  
    let new_tapes = through_tapes movement.dir movement.write og_head.nodes [] in
    {register=movement.next_state;nodes=new_tapes}
  
  let make_step default_val table og_head  =
    let all_moves = determine_moves table og_head in 
    (* goes through all moves and returns new heads *)
    List.map (execute_move default_val og_head) all_moves
end


module type TAPE_LOAD = sig
  open Tape
  val load_all_tapes : string -> node list
end


(* Module for downloading tape *)
module Tape_load : TAPE_LOAD = struct
    open Utils
    open Tape
    (* load tape *)
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
end
  

