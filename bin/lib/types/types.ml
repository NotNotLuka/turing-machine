
type node_data = {left: node; right: node; value: string option} 
and node = 
| Node of node_data
| Empty

type head = {register: string; nodes: node list}

type direction = Left | Right | Neutral

type action = {write: string option list; dir: direction list; next_state: string}

module TableMap = Map.Make(struct
  type t = string
  let compare = compare
end)

module TapeMap = Map.Make(struct
  type t = string option list
  let compare = compare
end)

