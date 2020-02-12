(** {1 Easy msgpack (de)serialization} *)

type bigstring = Bigstringaf.t

(** AST for msgpack values *)
type t =
  | Nil
  | Bool of bool
  | Int of int
  | Int64 of int64
  | Uint64 of int64
  | Float of float
  | Str of bigstring
  | Bin of bigstring
  | Array of t list
  | Map of (t * t) list
  | Ext of int * bigstring

(** {2 (De)serialization} *)

val of_string : string -> (t, [> `Msg of string]) result
(** [of_string s] returns the first msgpack value parsed from [s] or
    [Error parse_error] if [s] does not start with valid msgpack data. *)

val of_string_exn : string -> t
(** Like {!of_string} except:

    @raise Invalid_argument if the string does not start with valid msgpack
    data. *)

val of_bigstring : bigstring -> (t, [> `Msg of string]) result
(** [of_bigstring s] returns the first msgpack value parsed from [s] or
    [Error parse_error] if [s] does not start with valid msgpack data. *)

val of_bigstring_exn : bigstring -> t
(** Like {!of_bigstring} except:

    @raise Invalid_argument if the bigstring does not start with valid msgpack
    data. *)

val msgs_of_string : string -> (t list, [> `Msg of string]) result
(** [msgs_of_string s] returns all of the msgpack value parsed from [s] or
    [Error parse_error] if [s] does not start with valid msgpack data. *)

val msgs_of_string_exn : string -> t list
(** Like {!msgs_of_string} except:

    @raise Invalid_argument if the string does not start with valid msgpack
    data. *)

val msgs_of_bigstring : bigstring -> (t list, [> `Msg of string]) result
(** [msgs_of_bigstring s] returns all of the msgpack value parsed from [s] or
    [Error parse_error] if [s] does not start with valid msgpack data. *)

val msgs_of_bigstring_exn : bigstring -> t list
(** Like {!msgs_of_bigstring} except:

    @raise Invalid_argument if the bigstring does not start with valid msgpack
    data. *)

val to_string : ?buffer:[`New of int | `Buffer of bigstring] -> t -> string
(** [to_string msg] returns a msgpack encoded serialization of [msg].

    @param [buffer_size] can be used to specify the size of the Faraday buffer
           used during encoding. *)

val msgs_to_string :
  ?buffer:[`New of int | `Buffer of bigstring] -> t list -> string
(** [msgs_to_string msgs] returns a msgpack encoded serialization of [msgs],
    with values concatenated together.

    @param [buffer_size] can be used to specify the size of the Faraday buffer
           used during encoding. *)

val to_bigstring :
  ?buffer:[`New of int | `Buffer of bigstring] -> t -> bigstring
(** [to_bigstring msg] returns a msgpack encoded serialization of [msg].

    @param [buffer_size] can be used to specify the size of the Faraday buffer
           used during encoding. *)

val msgs_to_bigstring :
  ?buffer:[`New of int | `Buffer of bigstring] -> t list -> bigstring
(** [msgs_to_string msgs] returns a msgpack encoded serialization of [msgs],
    with values concatenated together.

    @param [buffer_size] can be used to specify the size of the Faraday buffer
           used during encoding. *)

val serialize : Faraday.t -> t -> unit
(** Faraday serializer for msgpack values *)

val msgpack : t Angstrom.t
(** Angstrom parser for msgpack values *)

(** {2 Utility functions for creating/accessing msgpack values} *)

(** {3 Constructors} *)

val unit : unit -> t

val bool : bool -> t

val int : int -> t

val int64 : int64 -> t

val uint64 : int64 -> t

val float : float -> t

val string : string -> t

val bigstring : bigstring -> t

val bin : bigstring -> t

val list : ('a -> t) -> 'a list -> t

val list' : t list -> t

val array : ('a -> t) -> 'a array -> t

val array' : t array -> t

val option : ('a -> t) -> 'a option -> t

val result : ok:('ok -> t) -> error:('err -> t) -> ('ok, 'err) result -> t

val pair : ('a -> t) -> ('b -> t) -> 'a * 'b -> t

val triple : ('a -> t) -> ('b -> t) -> ('c -> t) -> 'a * 'b * 'c -> t

val map : ('a -> t) -> ('b -> t) -> ('a * 'b) list -> t

val map' : (t * t) list -> t

val dict : ('a -> t) -> (string * 'a) list -> t

val dict' : (string * t) list -> t

(** {3 Accessors}

    Each accessor will raise {!Parse_error} if the type of the given msgpack
    value is different than what was requested.

    Any user-provided conversion function should raise {!Parse_error} if an
    unexpected type is encountered. *)

exception Parse_error of t * string

type ('ok, 'error) result =
  ('ok, 'error) Stdlib.result
  constraint 'error = [> `Parse_error of t * string]

val as_unit : t -> (unit, _) result

val as_bool : t -> (bool, _) result

val as_int : t -> (int, _) result

val as_int64 : t -> (int64, _) result

val as_uint64 : t -> (int64, _) result

val as_float : t -> (float, _) result

val as_string : t -> (string, _) result

val as_bigstring : t -> (bigstring, _) result

val as_bin : t -> (bigstring, _) result

val as_list : (t -> 'a) -> t -> ('a list, _) result

val as_array : (t -> 'a) -> t -> ('a array, _) result

val as_option : (t -> 'a) -> t -> ('a option, _) result

val as_result : (t -> (('ok, _) result as 'r)) -> t -> ('r, _) result

val as_pair : (t -> 'a) -> (t -> 'b) -> t -> ('a * 'b, _) result

val as_triple :
  (t -> 'a) -> (t -> 'b) -> (t -> 'c) -> t -> ('a * 'b * 'c, _) result

val as_map : (t -> 'a) -> (t -> 'b) -> t -> (('a * 'b) list, _) result

val as_map' : t -> ((t * t) list, _) result

val as_dict : (t -> 'a) -> t -> ((string * 'a) list, _) result

val as_dict' : t -> ((string * t) list, _) result

val get_unit : t -> unit

val get_bool : t -> bool

val get_int : t -> int

val get_int64 : t -> int64

val get_uint64 : t -> int64

val get_float : t -> float

val get_string : t -> string

val get_bigstring : t -> bigstring

val get_bin : t -> bigstring

val get_list : (t -> 'a) -> t -> 'a list

val get_array : (t -> 'a) -> t -> 'a array

val get_option : (t -> 'a) -> t -> 'a option

val get_result : (t -> (('ok, 'error) Stdlib.result as 'r)) -> t -> 'r

val get_pair : (t -> 'a) -> (t -> 'b) -> t -> 'a * 'b

val get_triple : (t -> 'a) -> (t -> 'b) -> (t -> 'c) -> t -> 'a * 'b * 'c

val get_map : (t -> 'a) -> (t -> 'b) -> t -> ('a * 'b) list

val get_map' : t -> (t * t) list

val get_dict : (t -> 'a) -> t -> (string * 'a) list

val get_dict' : t -> (string * t) list

val parse_error : t -> ('a, unit, string, 'b) format4 -> 'a
(** Raise {!Parse_error} *)

val catch_parse_error : (t -> 'a) -> t -> ('a, _) result

(** {2 Pretty-printing} *)

val pp : Format.formatter -> t -> unit
(** Debugging/simplistic pretty-printer for msgpack values.  Primarily intended
    for use from the toplevel.

    To use from the utop or another toplevel environment:

    [#install_printer Msgpack_pp.pp;;] *)

(**/**)

val of_precise : Internal.t -> t

val to_precise : t -> Internal.t
