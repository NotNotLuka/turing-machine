open Types

let move dir node new_value = 
  match dir, node with
  | Left, Node {left=Node next; _} -> Node {next with value=new_value;right=node}
  | Right, Node {right=Node next; _} -> Node {next with value=new_value;left=node}
  | Left, Node {left=Empty; _} ->  Node {left=Empty; right=node; value=new_value}
  | Right, Node {right=Empty; _} -> Node {left=node; right=Empty; value=new_value}
  | Neutral, Node next -> Node {next with value=new_value}
  | _, Empty -> failwith "Moving without a node"