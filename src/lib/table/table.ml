    
module type TABLE = sig
  type direction = Left | Right | Neutral
  type action = {write: string list; dir: direction list; next_state: string}
  module TableMap : Map.S with type key = string
  module TapeMap : Map.S with type key = string list
end


module Table : TABLE = struct
  type direction = Left | Right | Neutral
  type action = {write: string list; dir: direction list; next_state: string}
  module TableMap = Map.Make(struct
    type t = string
    let compare = compare
  end)

  module TapeMap = Map.Make(struct
    type t = string list
    let compare = compare
  end)

  (* let string_of_direction = function
  | Left -> "Left"
  | Right -> "Right"
  | Neutral -> "Neutral"
  
  let print_action action =
    print_string "write: ";
    List.iter print_string action.write;print_newline ();
    print_string "dir: ";
    List.iter (fun x -> print_string (" " ^ (string_of_direction x))) action.dir;print_newline ();
    print_endline ("next_state:" ^ action.next_state ^ "\n") *)
end

module type TABLE_LOAD = sig
  open Table
  val load_table : string -> action list TapeMap.t TableMap.t
end

module Table_load : TABLE_LOAD = struct
  open Table
  open Utils
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
  let action_of_list ls = 
    (* changes a list to action *)
    let spl_tapes = (fun x -> Str.split (Str.regexp (",")) x) in
    let (write, direct, state) = 
      (spl_tapes (List.nth ls 0), 
      spl_tapes (List.nth ls 1), 
      List.nth ls 2) in 
    {write=write;dir=(List.map dir_of_string direct);next_state=state}


  let parse_instruction s table = 
    (* parses a line of instructions *)
    let line = Str.split (Str.regexp " ") s in

    let state = List.nth line 0 in
    let tape_symbols = Str.split (Str.regexp ",") (List.nth line 1) in
    let tape_symbols = tape_symbols in
    let acts = List.map action_of_list (create_actions (List.tl (List.tl line))) in

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
end