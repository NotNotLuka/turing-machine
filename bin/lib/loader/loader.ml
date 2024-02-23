open Types
open Utils

(* Loading table *)
(* (state) (tape symbols) (write symbols)  (move direction) (next state) *)
let create_actions line =
  (* generates a list of all undetermined options from the given options *)
  let rec append_to_each ls appende acc =
    match ls with
    | [] -> acc
    | h :: t -> append_to_each t appende ((appende :: h) :: acc) in
  let rec combinations options acc =
    match options with
    | [] -> acc
    | h :: t -> (List.fold_left (*with fold_left we go through every option in the current element*)
                  (
                    fun ac x -> (*here we first add the current option to all of the previous scenarios 
                                and then we go onto the next level(element in options) 
                                and then we combine the accumulator with options generated *)
                    ac @ (combinations t (append_to_each acc x []))
                  ) 
                  [] h)
  in 
  let options = List.map (fun x -> Str.split (Str.regexp "|") x) line in
  List.map (List.rev) (combinations options [[]])


let dir_of_string dir = 
  match dir with
  | "L" -> Left
  | "R" -> Right
  | "N" -> Neutral
  | _ -> failwith "Invalid direction"


let actions_of_list ls = 
  let spl_tapes = (fun x -> Str.split (Str.regexp (",")) x) in
  let (write, direct, state) = 
    (spl_tapes (List.nth ls 0), 
    spl_tapes (List.nth ls 1), 
    List.nth ls 2) in 
  {write=write;dir=(List.map dir_of_string direct);next_state=state}


let parse_instruction s table = 
  let line = Str.split (Str.regexp " ") s in

  let state = List.nth line 0 in
  let tape_symbols = Str.split (Str.regexp ",") (List.nth line 1) in
  let acts = List.map actions_of_list (create_actions (List.tl (List.tl line))) in

  let current = TableMap.find_opt (state) table in 
  
  match current with
  | Some x -> 
            if TapeMap.mem tape_symbols x 
            then failwith "Overwriting previous instruction"
            else
            let new_tape = TapeMap.add tape_symbols acts x in 
            TableMap.add (List.nth line 0) new_tape table
  | None -> let new_tape = TapeMap.add tape_symbols acts TapeMap.empty in
            TableMap.add (List.nth line 0) new_tape table


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
