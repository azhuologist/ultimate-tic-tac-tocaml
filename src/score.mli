open Yojson.Basic.Util

type t

(** [score_type] specifies which highscore type will be used *)
type score_type = TIME | MOVES

(** [get_high_scores t len] returns the top [len] high_scores found in t. *)
val get_high_score : 'a list -> 'a option

(** [add_score key score] makes a new record of [score] for the given [key].
    If [key] already exists in [t], overwrite the previous score with [score]. *)
val add_score : t list -> string -> float -> t list

val update_json : string -> score_type -> State.t -> unit

val print_moves : unit -> unit

val print_time : unit -> unit