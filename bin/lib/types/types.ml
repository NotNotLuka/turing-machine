
type node_data = {left: node; right: node; value: string} 
and node = 
| Node of node_data
| Empty

type direction = Left | Right | Neutral

type action = {write: string; dir: direction; next_state: string}

module TableMap = Map.Make(struct
  type t = string
  let compare = compare
end)

module TapeMap = Map.Make(struct
  type t = string
  let compare = compare
end)
