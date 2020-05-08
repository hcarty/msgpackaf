type bigstring = Bigstringaf.t

type t =
  | Nil
  | Bool of bool
  | Int_fix_pos of int
  | Int_fix_neg of int
  | Int8 of int
  | Int16 of int
  | Int32 of int32
  | Int64 of int64
  | Uint8 of int
  | Uint16 of int
  | Uint32 of int32
  | Uint64 of int64
  | Float of float
  | Double of float
  | Str_fix of bigstring
  | Str8 of bigstring
  | Str16 of bigstring
  | Str32 of bigstring
  | Bin8 of bigstring
  | Bin16 of bigstring
  | Bin32 of bigstring
  | Array_fix of t list
  | Array16 of t list
  | Array32 of t list
  | Map_fix of (t * t) list
  | Map16 of (t * t) list
  | Map32 of (t * t) list
  | Ext_fix_1 of int * bigstring
  | Ext_fix_2 of int * bigstring
  | Ext_fix_4 of int * bigstring
  | Ext_fix_8 of int * bigstring
  | Ext_fix_16 of int * bigstring
  | Ext8 of int * bigstring
  | Ext16 of int * bigstring
  | Ext32 of int * bigstring

type msg = t

val msgpack : t Angstrom.t

val msgpacks : t list Angstrom.t

val of_string : ?allow_trailing_bytes:bool -> string -> (t, [> `Msg of string ]) result

val of_string_exn : ?allow_trailing_bytes:bool -> string -> t

val of_bigstring : ?allow_trailing_bytes:bool -> bigstring -> (t, [> `Msg of string ]) result

val of_bigstring_exn : ?allow_trailing_bytes:bool -> bigstring -> t

val msgs_of_string : ?allow_trailing_bytes:bool -> string -> (t list, [> `Msg of string ]) result

val msgs_of_string_exn : ?allow_trailing_bytes:bool -> string -> t list

val msgs_of_bigstring : ?allow_trailing_bytes:bool -> bigstring -> (t list, [> `Msg of string ]) result

val msgs_of_bigstring_exn : ?allow_trailing_bytes:bool -> bigstring -> t list

val serialize : Faraday.t -> t -> unit

val to_string : ?buffer:[ `New of int | `Buffer of bigstring ] -> t -> string

val msgs_to_string :
  ?buffer:[ `New of int | `Buffer of bigstring ] -> t list -> string

val to_bigstring :
  ?buffer:[ `New of int | `Buffer of bigstring ] -> t -> bigstring

val msgs_to_bigstring :
  ?buffer:[ `New of int | `Buffer of bigstring ] -> t list -> bigstring

val serialized_size : t -> int
