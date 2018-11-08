(** Printed messages for the user interface. *)

open Board

(** [curr_player_str s] returns a string representation of a player [s]. *)
val curr_player_str : square -> string

(** Prints a prompt message for the user to enter coordinates to mark. *)
val prompt_for_coordinates : square -> unit

(** Prints a help message. *)
val print_help : unit -> unit

(** Prints an error message for invalid coordinates. *)
val print_invalid_index_error_msg : unit -> unit

(** Prints an error message for an empty player input. *)
val print_empty_error_msg : unit -> unit

(** Prints an error message for a malformed command. *)
val print_malformed_error_msg : unit -> unit

(** Prints an error message for a player attempting to reset the board in a
    networked game. *)
val print_reset_disabled_msg : unit -> unit

(** Prints a win message for the player [p]. *)
val print_win_msg : square -> bool -> unit
