open Board

let curr_player_str s =
  match s with
  | X -> "X"
  | O -> "O"
  | Blank -> failwith "Failure: curr_player returned Blank"

let prompt_for_coordinates s =
  let curr = curr_player_str s in
  let msg = "\n" ^ curr ^ ", please enter the coordinates of your move:\n" in
  ANSITerminal.(print_string [cyan] msg);
  print_string "> "

let print_help () =
  let help_strs = [
    "mark [r c] : marks the square at row [r] and column [c] as the current " ^ 
      "player.";
    "restart : resets the game.";
    "help : prints this message.";
    "quit : quits the game.\n"
  ] in
  ANSITerminal.(print_string [green] (String.concat "\n\n" help_strs))

let print_invalid_index_error_msg () =
  let msg = "\nInvalid coordinates, please choose again.\n" in
  ANSITerminal.(print_string [red] msg)

let print_empty_error_msg () =
  let msg = "\nYou didn't type anything!\n" in
  ANSITerminal.(print_string [red] msg)

let print_malformed_error_msg () =
  let msg = "\nInvalid command, please try again.\n" in
  ANSITerminal.(print_string [red] msg)

let print_reset_disabled_msg () =
  let msg = "\nResetting is disabled on networked mode!\n" in
  ANSITerminal.(print_string [red] msg)

let print_win_msg p tiebreak =
  let s =
    match p with
    | X -> if tiebreak then "X" else "O"
    | O -> if tiebreak then "O" else "X"
    | Blank -> failwith "Failure: curr_player returned Blank" in
  let msg = "\n" ^ s ^ " is the WINNER!\n" in
  ANSITerminal.(print_string [cyan; Bold] msg)