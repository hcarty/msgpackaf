open Angstrom

type bigstring = Angstrom.bigstring

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

let bs_length = Bigarray.Array1.dim

let serialized_size msg =
  let rec add_elt sum elt = sum + size elt
  and add_kv sum (k, v) = sum + size k + size v
  and size atom =
    match atom with
    | Nil
    | Bool _
    | Int_fix_pos _
    | Int_fix_neg _ ->
      1
    | Uint8 _
    | Int8 _ ->
      1 + 1
    | Uint16 _
    | Int16 _ ->
      1 + 2
    | Uint32 _
    | Int32 _ ->
      1 + 4
    | Uint64 _
    | Int64 _ ->
      1 + 8
    | Float _ -> 1 + 4
    | Double _ -> 1 + 8
    | Str_fix s -> 1 + bs_length s
    | Bin8 s
    | Str8 s ->
      1 + 1 + bs_length s
    | Bin16 s
    | Str16 s ->
      1 + 2 + bs_length s
    | Bin32 s
    | Str32 s ->
      1 + 4 + bs_length s
    | Array_fix a -> 1 + List.fold_left add_elt 0 a
    | Array16 a -> 1 + 2 + List.fold_left add_elt 0 a
    | Array32 a -> 1 + 4 + List.fold_left add_elt 0 a
    | Map_fix m -> 1 + List.fold_left add_kv 0 m
    | Map16 m -> 1 + 2 + List.fold_left add_kv 0 m
    | Map32 m -> 1 + 4 + List.fold_left add_kv 0 m
    | Ext_fix_1 (_, s)
    | Ext_fix_2 (_, s)
    | Ext_fix_4 (_, s)
    | Ext_fix_8 (_, s)
    | Ext_fix_16 (_, s) ->
      1 + 1 + bs_length s
    | Ext8 (_, s) -> 1 + 1 + 1 + bs_length s
    | Ext16 (_, s) -> 1 + 1 + 2 + bs_length s
    | Ext32 (_, s) -> 1 + 1 + 4 + bs_length s
  in
  size msg

type msg = t

module Be = struct
  include BE

  let uint32_as_int =
    let max_int_as_int64 = Int64.of_int max_int in
    let ( + ) = Int64.add in
    any_int32 >>= fun i ->
    (* Conversion from signed to unsigned via Int64.t to avoid potential issues
       on 32-bit architectures *)
    let i =
      if i > 0l then
        Int64.of_int32 i
      else
        Int64.of_int32 i + 0xff_ff_ff_ffL + 1L
    in
    if Int64.compare i max_int_as_int64 > 0 then
      fail "msgpack field with length longer than max_int"
    else
      return (Int64.to_int i)
end

(* Helper functions *)
let raw8 = any_uint8 >>= fun length -> take_bigstring length

let raw16 = Be.any_uint16 >>= fun length -> take_bigstring length

let raw32 = Be.uint32_as_int >>= fun length -> take_bigstring length

let ext_fix_n n =
  any_int8 >>= fun typ ->
  take_bigstring n >>= fun content -> return (typ, content)

let tuple a =
  a >>= fun a' ->
  a >>= fun b' -> return (a', b')

let msgpack any =
  any_char >>= function
  (* Untagged constants *)
  | '\xc0' -> return @@ Nil
  | '\xc2' -> return @@ Bool false
  | '\xc3' -> return @@ Bool true
  (* Untagged integers *)
  | c when Char.code c land 0x80 = 0 -> return @@ Int_fix_pos (Char.code c)
  | c when Char.code c land 0xe0 = 0xe0 ->
    return @@ Int_fix_neg (Char.code c - 256)
  (* Tagged integers *)
  | '\xcc' -> any_uint8 >>| fun i -> Uint8 i
  | '\xcd' -> Be.any_uint16 >>| fun i -> Uint16 i
  | '\xce' -> Be.any_int32 >>| fun i -> Uint32 i
  | '\xcf' -> Be.any_int64 >>| fun i -> Uint64 i
  | '\xd0' -> any_int8 >>| fun i -> Int8 i
  | '\xd1' -> Be.any_int16 >>| fun i -> Int16 i
  | '\xd2' -> Be.any_int32 >>| fun i -> Int32 i
  | '\xd3' -> Be.any_int64 >>| fun i -> Int64 i
  (* Tagged floating point values *)
  | '\xca' -> Be.any_float >>| fun f -> Float f
  | '\xcb' -> Be.any_double >>| fun d -> Double d
  (* Strings *)
  | c when Char.code c land 0xe0 = 0xa0 ->
    let length = Char.code c land 0x1f in
    take_bigstring length >>| fun s -> Str_fix s
  | '\xd9' -> raw8 >>| fun s -> Str8 s
  | '\xda' -> raw16 >>| fun s -> Str16 s
  | '\xdb' -> raw32 >>| fun s -> Str32 s
  (* Binary blobs *)
  | '\xc4' -> raw8 >>| fun b -> Bin8 b
  | '\xc5' -> raw16 >>| fun b -> Bin16 b
  | '\xc6' -> raw32 >>| fun b -> Bin32 b
  (* Extension types *)
  | '\xd4' -> ext_fix_n 1 >>| fun (typ, x) -> Ext_fix_1 (typ, x)
  | '\xd5' -> ext_fix_n 2 >>| fun (typ, x) -> Ext_fix_2 (typ, x)
  | '\xd6' -> ext_fix_n 4 >>| fun (typ, x) -> Ext_fix_4 (typ, x)
  | '\xd7' -> ext_fix_n 8 >>| fun (typ, x) -> Ext_fix_8 (typ, x)
  | '\xd8' -> ext_fix_n 16 >>| fun (typ, x) -> Ext_fix_16 (typ, x)
  | '\xc7' ->
    any_uint8 >>= fun length ->
    any_int8 >>= fun typ ->
    take_bigstring length >>= fun content -> return @@ Ext8 (typ, content)
  | '\xc8' ->
    Be.any_uint16 >>= fun length ->
    any_int8 >>= fun typ ->
    take_bigstring length >>= fun content -> return @@ Ext16 (typ, content)
  | '\xc9' ->
    Be.uint32_as_int >>= fun length ->
    any_int8 >>= fun typ ->
    take_bigstring length >>= fun content -> return @@ Ext32 (typ, content)
  (* Arrays *)
  | c when Char.code c land 0xf0 = 0x90 ->
    let length = Char.code c land 0x0f in
    count length any >>| fun x -> Array_fix x
  | '\xdc' ->
    Be.any_uint16 >>= fun length ->
    count length any >>| fun x -> Array16 x
  | '\xdd' ->
    Be.uint32_as_int >>= fun length ->
    count length any >>| fun x -> Array32 x
  (* Maps *)
  | c when Char.code c land 0xf0 = 0x80 ->
    let length = Char.code c land 0x0f in
    count length (tuple any) >>| fun x -> Map_fix x
  | '\xde' ->
    Be.any_uint16 >>= fun length ->
    count length (tuple any) >>| fun x -> Map16 x
  | '\xdf' ->
    Be.uint32_as_int >>= fun length ->
    count length (tuple any) >>| fun x -> Map32 x
  (* Anything else is invalid *)
  | _ -> fail "msgpack"

let msgpack = fix (fun v -> msgpack v)

let msgpacks = many msgpack

let of_string s =
  match parse_string msgpack s with
  | Ok _ as o -> o
  | Error msg -> Error (`Msg msg)

let of_string_exn s =
  match of_string s with
  | Ok msg -> msg
  | Error (`Msg e) -> invalid_arg e

let of_bigstring s =
  match parse_bigstring msgpack s with
  | Ok _ as o -> o
  | Error msg -> Error (`Msg msg)

let of_bigstring_exn s =
  match of_bigstring s with
  | Ok msg -> msg
  | Error (`Msg e) -> invalid_arg e

let msgs_of_string s =
  match parse_string msgpacks s with
  | Ok _ as o -> o
  | Error msg -> Error (`Msg msg)

let msgs_of_string_exn s =
  match msgs_of_string s with
  | Ok msg -> msg
  | Error (`Msg e) -> invalid_arg e

let msgs_of_bigstring s =
  match parse_bigstring msgpacks s with
  | Ok _ as o -> o
  | Error e -> Error (`Msg e)

let msgs_of_bigstring_exn s =
  match msgs_of_bigstring s with
  | Ok msg -> msg
  | Error (`Msg e) -> invalid_arg e

let write_tagged fd tag f x =
  Faraday.write_uint8 fd tag;
  f fd x

let write_int_as_uint32 fd i = Faraday.BE.write_uint32 fd (Int32.of_int i)

let write_with_length fd tag w s =
  Faraday.write_uint8 fd tag;
  w fd (bs_length s);
  Faraday.schedule_bigstring fd s

let write_with_length8 fd tag s = write_with_length fd tag Faraday.write_uint8 s

let write_with_length16 fd tag s =
  write_with_length fd tag Faraday.BE.write_uint16 s

let write_with_length32 fd tag s =
  write_with_length fd tag write_int_as_uint32 s

let write_ext_fix fd tag typ s =
  Faraday.write_uint8 fd tag;
  Faraday.write_uint8 fd typ;
  Faraday.schedule_bigstring fd s

let write_ext fd tag w typ s =
  Faraday.write_uint8 fd tag;
  w fd (bs_length s);
  Faraday.write_uint8 fd typ;
  Faraday.schedule_bigstring fd s

let rec write fd msg =
  match msg with
  | Nil -> Faraday.write_uint8 fd 0xc0
  | Bool false -> Faraday.write_uint8 fd 0xc2
  | Bool true -> Faraday.write_uint8 fd 0xc3
  | Int_fix_pos i
  | Int_fix_neg i ->
    Faraday.write_uint8 fd i
  | Int8 i -> write_tagged fd 0xd0 Faraday.write_uint8 i
  | Int16 i -> write_tagged fd 0xd1 Faraday.BE.write_uint16 i
  | Int32 i -> write_tagged fd 0xd2 Faraday.BE.write_uint32 i
  | Int64 i -> write_tagged fd 0xd3 Faraday.BE.write_uint64 i
  | Uint8 i -> write_tagged fd 0xcc Faraday.write_uint8 i
  | Uint16 i -> write_tagged fd 0xcd Faraday.BE.write_uint16 i
  | Uint32 i -> write_tagged fd 0xce Faraday.BE.write_uint32 i
  | Uint64 i -> write_tagged fd 0xcf Faraday.BE.write_uint64 i
  | Float f -> write_tagged fd 0xca Faraday.BE.write_float f
  | Double d -> write_tagged fd 0xcb Faraday.BE.write_double d
  | Str_fix s ->
    write_tagged fd
      (0xa0 lor bs_length s)
      (fun fd s -> Faraday.schedule_bigstring fd s)
      s
  | Str8 s -> write_with_length8 fd 0xd9 s
  | Str16 s -> write_with_length16 fd 0xda s
  | Str32 s -> write_with_length32 fd 0xdb s
  | Bin8 b -> write_with_length8 fd 0xc4 b
  | Bin16 b -> write_with_length16 fd 0xc5 b
  | Bin32 b -> write_with_length32 fd 0xc6 b
  | Array_fix a -> write_array_fix fd a
  | Array16 a -> write_array fd 0xdc Faraday.BE.write_uint16 a
  | Array32 a -> write_array fd 0xdd write_int_as_uint32 a
  | Map_fix m -> write_map_fix fd m
  | Map16 m -> write_map fd 0xde Faraday.BE.write_uint16 m
  | Map32 m -> write_map fd 0xdf write_int_as_uint32 m
  | Ext_fix_1 (typ, s) -> write_ext_fix fd 0xd4 typ s
  | Ext_fix_2 (typ, s) -> write_ext_fix fd 0xd5 typ s
  | Ext_fix_4 (typ, s) -> write_ext_fix fd 0xd6 typ s
  | Ext_fix_8 (typ, s) -> write_ext_fix fd 0xd7 typ s
  | Ext_fix_16 (typ, s) -> write_ext_fix fd 0xd8 typ s
  | Ext8 (typ, s) -> write_ext fd 0xc7 Faraday.write_uint8 typ s
  | Ext16 (typ, s) -> write_ext fd 0xc8 Faraday.BE.write_uint16 typ s
  | Ext32 (typ, s) -> write_ext fd 0xc9 write_int_as_uint32 typ s

and write_array_fix fd a =
  Faraday.write_uint8 fd (0x90 lor List.length a);
  List.iter (write fd) a

and write_array fd tag w a =
  Faraday.write_uint8 fd tag;
  w fd (List.length a);
  List.iter (write fd) a

and write_map_fix fd m =
  Faraday.write_uint8 fd (0x80 lor List.length m);
  List.iter
    (fun (k, v) ->
      write fd k;
      write fd v)
    m

and write_map fd tag w m =
  Faraday.write_uint8 fd tag;
  w fd (List.length m);
  List.iter
    (fun (k, v) ->
      write fd k;
      write fd v)
    m

let create_fd buffer =
  match buffer with
  | None -> Faraday.create 0x100
  | Some (`New buffer_size) -> Faraday.create buffer_size
  | Some (`Buffer bs) -> Faraday.of_bigstring bs

let to_string ?buffer msg =
  let fd = create_fd buffer in
  write fd msg;
  Faraday.serialize_to_string fd

let to_bigstring ?buffer msg =
  let fd = create_fd buffer in
  write fd msg;
  Faraday.serialize_to_bigstring fd

let serialize fd msg = write fd msg

let msgs_to_string ?buffer msgs =
  let fd = create_fd buffer in
  List.iter (serialize fd) msgs;
  Faraday.serialize_to_string fd

let msgs_to_bigstring ?buffer msgs =
  let fd = create_fd buffer in
  List.iter (serialize fd) msgs;
  Faraday.serialize_to_bigstring fd
