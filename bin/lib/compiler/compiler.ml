open Types
open Tape_utils
(* open Utils *)


let read_tape_nodes nodes =
  let rec aux nodes acc halt = 
    match nodes with
    | [] -> (List.rev acc), halt
    | (Empty::t) -> aux t (Some "HALT" :: acc) true
    | (Node h)::t -> aux t (h.value :: acc) false in 
  aux nodes [] false

let determine_moves table head =
  let (tape_state, halt) = read_tape_nodes head.nodes in 
  if halt then []
  else 
  match TableMap.find_opt head.register table with
  | Some tape_table ->
    begin
      match TapeMap.find_opt tape_state tape_table with
      | Some acts -> acts
      | None -> []
    end
  | None -> []


let execute_move og_head movement =
  let rec through_tapes dirs values tapes acc =
    match dirs, values, tapes with
    | (dir::t_dir), (value::t_value), (tape::t_tape) ->
      through_tapes t_dir t_value t_tape ((move dir tape value) :: acc)
    | [], [], [] -> acc
    | _ -> failwith "Incorrect size" in 
  let new_tapes = through_tapes movement.dir movement.write og_head.nodes [] in
  {register=movement.next_state;nodes=new_tapes}


let make_step table og_head =
  let all_moves = determine_moves table og_head in 
  List.map (execute_move og_head) all_moves
