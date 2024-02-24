open Types

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
let read_tape node = 
  match node with
  | Empty -> []
  | Node node -> left_side_of_tape node.left @ [Node node] @ (right_side_of_tape node.right)

  
let move dir node new_value default_value = 
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
    Node {left=Empty; right=new_cur; value=default_value}

  | Right, Node {right=Empty;left=left;_} -> 
    let new_cur = Node {right=Empty;left=left;value=new_value} in
    Node {left=new_cur; right=Empty; value=default_value}

  | Neutral, Node next -> Node {next with value=new_value}
  | _, Empty -> failwith "Moving without a node"
