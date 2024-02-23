open Types
open Tape_utils
(* open Utils *)


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


let execute_move default og_head movement =
  let rec through_tapes dirs values tapes acc =
    match dirs, values, tapes with
    | (dir::t_dir), (value::t_value), (tape::t_tape) ->
      through_tapes t_dir t_value t_tape ((move dir tape value default) :: acc)
    | [], [], [] -> acc
    | _ -> failwith "Incorrect size" in 
  let new_tapes = through_tapes movement.dir movement.write og_head.nodes [] in
  {register=movement.next_state;nodes=new_tapes}


let make_step default table og_head  =
  let all_moves = determine_moves table og_head in 
  List.map (execute_move default og_head) all_moves
