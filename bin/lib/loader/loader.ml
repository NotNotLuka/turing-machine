open Types
open Utils

(* Loading table *)
let parse_instruction s table = 
  let split_instruct = Str.split (Str.regexp " ") s in

  let dir = 
    match (List.nth split_instruct 3) with
    | "L" -> Left
    | "R" -> Right
    | "N" -> Neutral
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


let load_table filename = 
  let file = read_file filename in
  let table = List.fold_left (fun acc x -> parse_instruction x acc) TableMap.empty file in
  table

(* Loading tape *)
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
