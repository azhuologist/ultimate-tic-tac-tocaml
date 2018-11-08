open Yojson.Basic.Util

type score_type = TIME | MOVES

type t = {name : string; value : float}

let score_to_string = function
  | TIME -> "time"
  | MOVES -> "move"

let json_to_score json = {
  name = json |> member "name" |> to_string;
  value = json |> member "key" |> to_float }

let rec is_member scores name = 
  match scores with
  | x :: xs -> if name = (fst x) then true else (is_member xs name)
  | [] -> false

let json_to_score_list json t = 
    json |> member (score_to_string t) |> to_list |> List.map json_to_score

let t_compare a b =
  match a.value -. b.value with
  | x when x = 0.0 -> 0
  | x when x < 0.0 -> -1
  | x -> 1

let sort_score_list (scores : t list) = 
    List.sort_uniq t_compare scores

let t_to_string t =
  print_string (t.name ^ " : " ^ (string_of_float t.value) ^ "\n")

let print_moves () =
  let move_json = Yojson.Basic.from_file "moves.json" in
  let move_list = json_to_score_list move_json MOVES in
  let countdown = ref ((List.length move_list) - 1) in
  try
    while !countdown <> -1 do
      t_to_string (List.nth move_list !countdown);
      countdown := !countdown - 1;
    done;
  with Failure _ -> ()

let print_time () =
  let time_json = Yojson.Basic.from_file "time.json" in
  let time_list = json_to_score_list time_json TIME in
  let countdown = ref ((List.length time_list) - 1) in
  try
    while !countdown <> -1 do
      t_to_string (List.nth time_list !countdown);
      countdown := !countdown - 1;
    done;
  with Failure _ -> ()

let rec change_score (scores : t list) name new_value acc =
  match scores with
  | x :: xs when name = x.name -> 
    (List.rev acc @ [{name = x.name; value = 
      (if new_value > x.value then x.value else new_value)}] @ xs, true)
  | x :: xs -> change_score xs name new_value (x :: acc)
  | [] -> (List.rev acc, false)

let add_score scores name sc = 
  match change_score scores name sc [] with
  | (x, b) when b -> sort_score_list x
  | (x, b) -> sort_score_list ({name = name; value = sc} :: scores)

let get_high_score scores =
  match scores with
  | x :: xs -> Some x
  | [] -> None

let stat_of_stats (state : State.t) play_t = 
  match (State.get_stats state) with
  | [] -> None
  | x :: xs -> if play_t = MOVES then
                  Some {name = x.name; value = x.moves}
                else
                  Some {name = x.name; value = x.time}

let get_stat stats = 
  match stats with
  | Some x -> x
  | None -> {name = ""; value = 0.0}

let add_to_file filename json =
  let channel = open_out filename in
  Yojson.Basic.to_channel channel json;
  close_out channel

let assoc_to_json_list scores =
  let rec to_list scores acc =
  match scores with
    | {name;value} :: xs -> 
    to_list xs ((`Assoc [("name",`String name); ("key", `Float value)]) :: acc)
    | [] -> List.rev acc
  in to_list scores []

let assoc_list_to_json t scores = 
  `Assoc [((score_to_string t), `List (assoc_to_json_list scores))]

let update_json filename t state =
  let json = Yojson.Basic.from_file filename in
  let current = json_to_score_list json t in
  let stats = get_stat (stat_of_stats state t) in
  let updated = if stats.name <> "" then
      add_score current stats.name stats.value
      else [] in
  if (updated <> []) then add_to_file filename (assoc_list_to_json t updated)
  else ();

