open Tape
open Utils

type action = {write: string; dir: direction; next_state: string}

let string_of_direction = function
| Left -> "Left"
| Right -> "Right"

let print_action action =
  Printf.printf "write: %s, dir: %s, next_state: %s\n"
    action.write
    (string_of_direction action.dir)
    action.next_state
  

module TableMap = Map.Make(struct
  type t = string
  let compare = compare
end)

module TapeMap = Map.Make(struct
  type t = string
  let compare = compare
end)


(* (state) (tape symbol) (write symbol) (move direction) (next state) *)
let parse_instruction s table = 
  let split_instruct = Str.split (Str.regexp " ") s in

  let dir = 
    match (List.nth split_instruct 3) with
    | "L" -> Left
    | "R" -> Right
    | _ -> failwith "Invalid direction" in

  let gen_action = {write=(List.nth split_instruct 2); 
                    dir=dir;
                    next_state=(List.nth split_instruct 4)} in
  let current = TableMap.find_opt (List.nth split_instruct 0) table in 
  
  match current with
  | Some x -> let new_tape = TapeMap.add (List.nth split_instruct 1) gen_action x in 
            TableMap.add (List.nth split_instruct 0) new_tape table
  | None -> let new_tape = TapeMap.add (List.nth split_instruct 1) gen_action TapeMap.empty in
            TableMap.add (List.nth split_instruct 0) new_tape table


let compile_table filename = 
  let file = read_file filename in
  let table = List.fold_left (fun acc x -> parse_instruction x acc) TableMap.empty file in
  table



