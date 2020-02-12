type bigstring = Internal.bigstring

let bs_length = Bigstringaf.length

let string_of_bigstring b =
  Bigstringaf.substring b ~off:0 ~len:(Bigstringaf.length b)

let bigstring_of_string s =
  Bigstringaf.of_string ~off:0 ~len:(String.length s) s

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

let name_of_type = function
  | Nil -> "nil"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Int64 _ -> "int64"
  | Uint64 _ -> "uint64"
  | Float _ -> "float"
  | Str _ -> "str"
  | Bin _ -> "bin"
  | Array _ -> "array"
  | Map _ -> "map"
  | Ext (_, _) -> "ext"

let list_map f l = List.rev_map f l |> List.rev

let int_of_uint32 i = Int32.to_int i land 0xff_ff_ff_ff

let int64_of_uint32 i =
  Int64.logand 0x00_00_00_00_ff_ff_ff_ffL (Int64.of_int32 i)

let max_int' = Int64.of_int max_int

let min_int' = Int64.of_int min_int

let int32_fits_in_int =
  Int64.of_int max_int > Int64.of_int32 Int32.max_int
  && Int64.of_int min_int < Int64.of_int32 Int32.min_int

let uint32_fits_in_int = Int64.of_int max_int > 0xff_ff_ff_ffL

let rec of_precise = function
  | Internal.Nil -> Nil
  | Internal.Bool b -> Bool b
  | Internal.Int_fix_pos i
  | Internal.Int_fix_neg i
  | Internal.Int8 i
  | Internal.Int16 i ->
    Int i
  | Internal.Int32 i ->
    let i' = Int64.of_int32 i in
    if int32_fits_in_int || (i' <= max_int' && i' >= min_int') then
      Int (Int32.to_int i)
    else
      Int64 i'
  | Internal.Int64 i ->
    if i <= max_int' && i >= min_int' then
      Int (Int64.to_int i)
    else
      Int64 i
  | Internal.Uint8 i
  | Internal.Uint16 i ->
    Int i
  | Internal.Uint32 i ->
    let i' = int64_of_uint32 i in
    if uint32_fits_in_int || i' <= max_int' then
      Int (int_of_uint32 i)
    else
      Int64 i'
  | Internal.Uint64 i ->
    if i >= 0L && i <= max_int' then
      Int (Int64.to_int i)
    else if i >= 0L then
      Int64 i
    else
      Uint64 i
  | Internal.Float f
  | Internal.Double f ->
    Float f
  | Internal.Str_fix s
  | Internal.Str8 s
  | Internal.Str16 s
  | Internal.Str32 s ->
    Str s
  | Internal.Bin8 b
  | Internal.Bin16 b
  | Internal.Bin32 b ->
    Bin b
  | Internal.Array_fix a
  | Internal.Array16 a
  | Internal.Array32 a ->
    Array (list_map of_precise a)
  | Internal.Map_fix m
  | Internal.Map16 m
  | Internal.Map32 m ->
    Map (list_map (fun (k, v) -> (of_precise k, of_precise v)) m)
  | Internal.Ext_fix_1 (tag, s)
  | Internal.Ext_fix_2 (tag, s)
  | Internal.Ext_fix_4 (tag, s)
  | Internal.Ext_fix_8 (tag, s)
  | Internal.Ext_fix_16 (tag, s)
  | Internal.Ext8 (tag, s)
  | Internal.Ext16 (tag, s)
  | Internal.Ext32 (tag, s) ->
    Ext (tag, s)

let to_precise_int i =
  if i < 0L && i >= -32L then
    Internal.Int_fix_neg (Int64.to_int i)
  else if i >= 0L && i <= 127L then
    Internal.Int_fix_pos (Int64.to_int i)
  else if i >= 0L && i < 0x1_00L then
    Internal.Uint8 (Int64.to_int i)
  else if i >= 0L && i < 0x1_00_00L then
    Internal.Uint16 (Int64.to_int i)
  else if i >= 0L && i < 0x1_00_00_00_00L then
    Internal.Uint32 (Int64.to_int32 i)
  else if i >= 0L then
    Internal.Uint64 i
  else if i >= -0x80L then
    Internal.Int8 (Int64.to_int i)
  else if i >= -0x80_00L then
    Internal.Int16 (Int64.to_int i)
  else if i >= -0x80_00_00_00L then
    Internal.Int32 (Int64.to_int32 i)
  else
    Internal.Int64 i

let to_precise_uint i =
  if i < 0L then
    (* Not really negative... *)
    Internal.Uint64 i
  else
    to_precise_int i

let to_precise_str s =
  let length = bs_length s in
  if length <= 31 then
    Internal.Str_fix s
  else if length < 0x1_00 then
    Internal.Str8 s
  else if length < 0x1_00_00 then
    Internal.Str16 s
  else
    Internal.Str32 s

let to_precise_bin b =
  let length = bs_length b in
  if length < 0x1_00 then
    Internal.Bin8 b
  else if length < 0x1_00_00 then
    Internal.Bin16 b
  else
    Internal.Bin32 b

let to_precise_ext typ s =
  match bs_length s with
  | 1 -> Internal.Ext_fix_1 (typ, s)
  | 2 -> Internal.Ext_fix_2 (typ, s)
  | 4 -> Internal.Ext_fix_4 (typ, s)
  | 8 -> Internal.Ext_fix_8 (typ, s)
  | 16 -> Internal.Ext_fix_16 (typ, s)
  | length ->
    if length < 0x1_00 then
      Internal.Ext8 (typ, s)
    else if length < 0x1_00_00 then
      Internal.Ext16 (typ, s)
    else
      Internal.Ext32 (typ, s)

let rec to_precise = function
  | Nil -> Internal.Nil
  | Bool b -> Internal.Bool b
  | Int i -> to_precise_int (Int64.of_int i)
  | Int64 i -> to_precise_int i
  | Uint64 i -> to_precise_uint i
  | Float f -> Internal.Double f
  | Str s -> to_precise_str s
  | Bin b -> to_precise_bin b
  | Array a -> to_precise_array a
  | Map m -> to_precise_map m
  | Ext (typ, s) -> to_precise_ext typ s

and to_precise_array a =
  let length = List.length a in
  let precise_array = list_map to_precise a in
  if length <= 15 then
    Internal.Array_fix precise_array
  else if length < 0x1_00_00 then
    Internal.Array16 precise_array
  else
    Internal.Array32 precise_array

and to_precise_map m =
  let length = List.length m in
  let precise_map = list_map (fun (k, v) -> (to_precise k, to_precise v)) m in
  if length <= 15 then
    Internal.Map_fix precise_map
  else if length < 0x1_00_00 then
    Internal.Map16 precise_map
  else
    Internal.Map32 precise_map

let of_string s =
  match Internal.of_string s with
  | Ok msg -> Ok (of_precise msg)
  | Error _ as e -> e

let of_string_exn s =
  match of_string s with
  | Ok msg -> msg
  | Error (`Msg e) -> invalid_arg e

let of_bigstring s =
  match Internal.of_bigstring s with
  | Ok msg -> Ok (of_precise msg)
  | Error _ as e -> e

let of_bigstring_exn s =
  match of_bigstring s with
  | Ok msg -> msg
  | Error (`Msg e) -> invalid_arg e

let msgs_of_string s =
  match Internal.msgs_of_string s with
  | Ok msgs -> Ok (list_map of_precise msgs)
  | Error _ as e -> e

let msgs_of_string_exn s =
  match msgs_of_string s with
  | Ok msgs -> msgs
  | Error (`Msg e) -> invalid_arg e

let msgs_of_bigstring s =
  match Internal.msgs_of_bigstring s with
  | Ok msgs -> Ok (list_map of_precise msgs)
  | Error _ as e -> e

let msgs_of_bigstring_exn s =
  match msgs_of_bigstring s with
  | Ok msgs -> msgs
  | Error (`Msg e) -> invalid_arg e

let to_string ?buffer msg = to_precise msg |> Internal.to_string ?buffer

let msgs_to_string ?buffer msgs =
  list_map to_precise msgs |> Internal.msgs_to_string ?buffer

let to_bigstring ?buffer msg = to_precise msg |> Internal.to_bigstring ?buffer

let msgs_to_bigstring ?buffer msgs =
  list_map to_precise msgs |> Internal.msgs_to_bigstring ?buffer

let msgpack = Angstrom.(of_precise <$> Internal.msgpack)

let serialize fd msg = Internal.serialize fd (to_precise msg)

(* Constructors *)

let unit () = Nil

let bool b = Bool b

let int i = Int i

let int64 i = Int64 i

let uint64 i = Uint64 i

let float f = Float f

let string s = Str (bigstring_of_string s)

let bigstring s = Str s

let bin s = Bin s

let list f l = Array (List.rev_map f l |> List.rev)

let list' l = Array l

let array f a = Array (Array.map f a |> Array.to_list)

let array' a = Array (Array.to_list a)

let option f o =
  match o with
  | None -> Nil
  | Some x -> f x

let result ~ok ~error r =
  match r with
  | Ok o -> ok o
  | Error e -> error e

let pair fa fb (a, b) = Array [ fa a; fb b ]

let triple fa fb fc (a, b, c) = Array [ fa a; fb b; fc c ]

let map fk fv l =
  let conv (k, v) =
    let key = fk k in
    let value = fv v in
    (key, value)
  in
  Map (List.rev_map conv l |> List.rev)

let map' l = Map l

let dict f l =
  let conv (k, v) =
    let key = string k in
    let value = f v in
    (key, value)
  in
  Map (List.rev_map conv l |> List.rev)

let dict' l =
  let conv (k, v) =
    let key = string k in
    (key, v)
  in
  Map (List.rev_map conv l |> List.rev)

(* Accessors *)

exception Parse_error of t * string

type ('ok, 'error) result = ('ok, 'error) Stdlib.result
  constraint 'error = [> `Parse_error of t * string ]

let parse_error v msgf =
  Format.ksprintf (fun msg -> raise (Parse_error (v, msg))) msgf

let access_error v expect =
  parse_error v "Expected %s, got %s" (name_of_type expect) (name_of_type v)

let empty_bigstring : bigstring = Bigarray.(Array1.create char c_layout 0)

let get_unit = function
  | Nil -> ()
  | v -> access_error v Nil

let get_bool = function
  | Bool b -> b
  | v -> access_error v (Bool true)

let get_int = function
  | Int i -> i
  | v -> access_error v (Int 0)

let get_int64 = function
  | Int i -> Int64.of_int i
  | Int64 i -> i
  | v -> access_error v (Int64 0L)

let get_uint64 = function
  | Int i when i >= 0 -> Int64.of_int i
  | Int64 i when i >= 0L -> i
  | Uint64 i -> i
  | v -> access_error v (Uint64 0L)

let get_float = function
  | Float f -> f
  | v -> access_error v (Float 0.0)

let get_string = function
  | Str s -> string_of_bigstring s
  | v -> access_error v (Str empty_bigstring)

let get_bigstring = function
  | Str s -> s
  | v -> access_error v (Str empty_bigstring)

let get_bin = function
  | Bin b -> b
  | v -> access_error v (Bin empty_bigstring)

let get_list f = function
  | Array l -> List.rev_map f l |> List.rev
  | v -> access_error v (Array [])

let get_array f v = get_list f v |> Array.of_list

let get_option f = function
  | Nil -> None
  | v -> Some (f v)

let get_result f v : _ Stdlib.result = f v

let get_pair fa fb = function
  | Array [ a; b ] -> (fa a, fb b)
  | Array a as v ->
    parse_error v "Expected pair, got %d elements" (List.length a)
  | v -> parse_error v "Expected pair, got %s" (name_of_type v)

let get_triple fa fb fc = function
  | Array [ a; b; c ] -> (fa a, fb b, fc c)
  | Array a as v ->
    parse_error v "Expected triple, got %d elements" (List.length a)
  | v -> parse_error v "Expected triple, got %s" (name_of_type v)

let get_map fk fv = function
  | Map m -> List.rev_map (fun (k, v) -> (fk k, fv v)) m |> List.rev
  | v -> access_error v (Map [])

let get_map' = function
  | Map m -> m
  | v -> access_error v (Map [])

let get_dict f = function
  | Map m ->
    List.rev_map
      (function
        | (Str key, v) -> (string_of_bigstring key, f v)
        | (k, _) ->
          parse_error k "Expected string as dict key, got %s" (name_of_type k))
      m
    |> List.rev
  | v -> access_error v (Map [])

let get_dict' = get_dict (fun x -> x)

let catch_parse_error f (t : t) =
  try Ok (f t) with
  | Parse_error (t, msg) -> Error (`Parse_error (t, msg))

let as_unit t = catch_parse_error get_unit t

let as_bool t = catch_parse_error get_bool t

let as_int t = catch_parse_error get_int t

let as_int64 t = catch_parse_error get_int64 t

let as_uint64 t = catch_parse_error get_uint64 t

let as_float t = catch_parse_error get_float t

let as_string t = catch_parse_error get_string t

let as_bigstring t = catch_parse_error get_bigstring t

let as_bin t = catch_parse_error get_bin t

let as_list f t = catch_parse_error (get_list f) t

let as_array f t = catch_parse_error (get_array f) t

let as_option f t = catch_parse_error (get_option f) t

let as_result f t = catch_parse_error (get_result f) t

let as_pair fa fb t = catch_parse_error (get_pair fa fb) t

let as_triple fa fb fc t = catch_parse_error (get_triple fa fb fc) t

let as_map fk fv t = catch_parse_error (get_map fk fv) t

let as_map' t = catch_parse_error get_map' t

let as_dict f t = catch_parse_error (get_dict f) t

let as_dict' t = catch_parse_error get_dict' t

(* Pretty-printing *)

let pp_sep fmt () = Format.fprintf fmt ",@ "

let pp_list pp fmt l =
  Format.fprintf fmt "@[[";
  Format.pp_print_list ~pp_sep pp fmt l;
  Format.fprintf fmt "]@]"

let pp_tuple ppk ppv fmt (k, v) =
  Format.fprintf fmt "@[(";
  Format.fprintf fmt "%a" ppk k;
  pp_sep fmt ();
  Format.fprintf fmt "%a" ppv v;
  Format.fprintf fmt ")@]"

let pp_binary fmt bin = Format.fprintf fmt "<%d bytes>" (bs_length bin)

let rec pp fmt v =
  match v with
  | Nil -> Format.pp_print_string fmt "nil"
  | Bool b -> Format.pp_print_bool fmt b
  | Int i -> Format.pp_print_int fmt i
  | Int64 i -> Format.fprintf fmt "%Ld" i
  | Uint64 i -> Format.fprintf fmt "%Lu" i
  | Float f -> Format.fprintf fmt "%g" f
  | Str s -> Format.fprintf fmt "%S" (string_of_bigstring s)
  | Bin b -> pp_binary fmt b
  | Array a -> pp_list pp fmt a
  | Map m -> pp_list (pp_tuple pp pp) fmt m
  | Ext (typ, s) -> pp_tuple Format.pp_print_int pp_binary fmt (typ, s)
