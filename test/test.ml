module Msgpack = Msgpackaf.Easy

let input_file file =
  let ic = open_in_bin file in
  let length = in_channel_length ic in
  let content = really_input_string ic length in
  close_in ic;
  content

let round_trip_from_file file =
  let mpac =
    Alcotest.testable
      (fun fmt s -> Fmt.Dump.list Msgpack.pp fmt (Msgpack.msgs_of_string_exn s))
      ( = )
  in
  let raw = input_file file in
  let precise = Msgpack.msgs_of_string_exn raw in
  let encoded = Msgpack.msgs_to_string precise in
  Alcotest.(check mpac) "Round trip" raw encoded;
  Alcotest.(check int)
    "Serialized size" (String.length raw)
    (List.fold_left
       (fun sum msg -> sum + Msgpackaf.Internal.serialized_size msg)
       0
       (List.map Msgpack.to_precise precise))

let cases_compact () = round_trip_from_file "../../../test/cases_compact.mpac"

let constructor_accessor () =
  let module String_map = Map.Make (String) in
  let eq typ a b = if not (a = b) then Alcotest.fail typ in
  let er typ get x =
    match get x with
    | _ -> Alcotest.fail typ
    | exception Msgpack.Parse_error _ -> ()
  in
  let test_result = function
    | Msgpack.Int i -> Ok i
    | Msgpack.Str s -> Error s
    | v -> Msgpack.parse_error v "Expected int or str"
  in
  let empty_bigstring = Bigstringaf.empty in
  let a_bigstring = Bigstringaf.of_string ~off:0 ~len:1 "a" in
  eq "unit cons" Msgpack.(unit ()) Msgpack.Nil;
  eq "unit accs" Msgpack.(get_unit Nil) ();
  eq "bool cons" Msgpack.(bool true) Msgpack.(Bool true);
  eq "bool accs" Msgpack.(get_bool (Bool true)) true;
  eq "int cons" Msgpack.(int 0) Msgpack.(Int 0);
  eq "int accs" Msgpack.(get_int (Int 0)) 0;
  eq "int64 cons" Msgpack.(int64 0L) Msgpack.(Int64 0L);
  eq "int64 accs (int)" Msgpack.(get_int64 (Int 0)) 0L;
  eq "int64 accs (int64)" Msgpack.(get_int64 (Int64 0L)) 0L;
  eq "uint64 cons" Msgpack.(uint64 0L) Msgpack.(Uint64 0L);
  eq "uint64 accs (int)" Msgpack.(get_uint64 (Int 0)) 0L;
  eq "uint64 accs (int64)" Msgpack.(get_uint64 (Int64 0L)) 0L;
  eq "uint64 accs (uint64)" Msgpack.(get_uint64 (Uint64 0L)) 0L;
  er "uint64 accs (neg int)" Msgpack.get_uint64 (Msgpack.Int (-1));
  er "uint64 accs (neg int64)" Msgpack.get_uint64 (Msgpack.Int64 (-1L));
  eq "float cons" Msgpack.(float 0.0) Msgpack.(Float 0.0);
  eq "float accs" Msgpack.(get_float (Float 0.0)) 0.0;
  eq "string cons" Msgpack.(string "") Msgpack.(Str empty_bigstring);
  eq "string accs" Msgpack.(get_string (Str empty_bigstring)) "";
  eq "bin cons" Msgpack.(bin empty_bigstring) Msgpack.(Bin empty_bigstring);
  eq "bin accs" Msgpack.(get_bin (Bin empty_bigstring)) empty_bigstring;
  eq "list cons (w/conv)" Msgpack.(list int [ 1 ]) Msgpack.(Array [ Int 1 ]);
  eq "list accs (w/conv)" Msgpack.(get_list get_int (Array [ Int 1 ])) [ 1 ];
  eq "list cons" Msgpack.(list' []) Msgpack.(Array []);
  eq "array cons" Msgpack.(array int [| 1 |]) Msgpack.(Array [ Int 1 ]);
  eq "array accs" Msgpack.(get_array get_int (Array [ Int 1 ])) [| 1 |];
  eq "option cons (none)" Msgpack.(option int None) Msgpack.Nil;
  eq "option cons (some)" Msgpack.(option int (Some 1)) Msgpack.(Int 1);
  eq "option accs (none)" Msgpack.(get_option get_int Nil) None;
  eq "option accs (some)" Msgpack.(get_option get_int (Int 1)) (Some 1);
  eq "result cons (ok)"
    Msgpack.(result ~ok:int ~error:string (Ok 1))
    Msgpack.(Int 1);
  eq "result const (error)"
    Msgpack.(result ~ok:int ~error:string (Error ""))
    Msgpack.(Str empty_bigstring);
  eq "result accs (ok)" Msgpack.(get_result test_result (Int 1)) (Ok 1);
  eq "result accs (error)"
    Msgpack.(get_result test_result (Str empty_bigstring))
    (Error empty_bigstring);
  er "result accs (bad type)" Msgpack.(get_result test_result) Msgpack.Nil;
  eq "pair cons"
    Msgpack.(pair int float (1, 2.0))
    Msgpack.(Array [ Int 1; Float 2.0 ]);
  eq "pair accs"
    Msgpack.(get_pair get_int get_float (Array [ Int 1; Float 2.0 ]))
    (1, 2.0);
  er "pair accs (bad array)"
    Msgpack.(get_pair get_int get_float)
    Msgpack.(Array [ Int 1; Float 2.0; Str empty_bigstring ]);
  eq "triple cons"
    Msgpack.(triple int float string (1, 2.0, ""))
    Msgpack.(Array [ Int 1; Float 2.0; Str empty_bigstring ]);
  eq "triple accs"
    Msgpack.(
      get_triple get_int get_float get_string
        (Array [ Int 1; Float 2.0; Str empty_bigstring ]))
    (1, 2.0, "");
  er "triple accs (bad array)"
    Msgpack.(get_triple get_int get_float get_string)
    Msgpack.(Array [ Int 1; Float 2.0; Str empty_bigstring; Nil ]);
  eq "map cons (w/conv)"
    Msgpack.(map string int [ ("a", 1) ])
    Msgpack.(Map [ (Str a_bigstring, Int 1) ]);
  eq "map accs (w/conv)"
    Msgpack.(get_map get_string get_int (Map [ (Str a_bigstring, Int 1) ]))
    [ ("a", 1) ];
  eq "map cons"
    Msgpack.(map' [ (Str a_bigstring, Int 1) ])
    Msgpack.(Map [ (Str a_bigstring, Int 1) ]);
  eq "map accs"
    Msgpack.(get_map' (Map [ (Str a_bigstring, Int 1) ]))
    Msgpack.[ (Str a_bigstring, Int 1) ];
  eq "dict cons (w/conv)"
    Msgpack.(dict int [ ("a", 1) ])
    Msgpack.(Map [ (Str a_bigstring, Int 1) ]);
  eq "dict accs (w/conv)"
    Msgpack.(get_dict get_int (Map [ (Str a_bigstring, Int 1) ]))
    [ ("a", 1) ];
  eq "dict cons"
    Msgpack.(dict' [ ("a", Int 1) ])
    Msgpack.(Map [ (Str a_bigstring, Int 1) ]);
  eq "dict accs"
    Msgpack.(get_dict' (Map [ (Str a_bigstring, Int 1) ]))
    Msgpack.[ ("a", Int 1) ];
  ()

type 'a quick = {
  name : string;
  qc : 'a QCheck.arbitrary;
  pack : 'a -> Msgpack.t;
  unpack : Msgpack.t -> 'a;
}

let quick ?(count = 1_000) quick =
  let conv i =
    quick.pack i |> Msgpack.to_string |> Msgpack.of_string_exn |> fun v ->
    quick.unpack v = i
  in
  QCheck.Test.make ~count ~name:quick.name quick.qc conv

let make_quick ?count name qc pack unpack =
  quick ?count { name; qc; pack; unpack }

let qcheck_bigstring =
  QCheck.map
    ~rev:(fun bs ->
      Bigstringaf.substring bs ~off:0 ~len:(Bigstringaf.length bs))
    (fun s -> Bigstringaf.of_string s ~off:0 ~len:(String.length s))
    QCheck.string

let quick_bool =
  make_quick ~count:10 "bool" QCheck.bool Msgpack.bool Msgpack.get_bool

let quick_int = make_quick "int" QCheck.int Msgpack.int Msgpack.get_int

let quick_float =
  make_quick "float" QCheck.float Msgpack.float Msgpack.get_float

let quick_string =
  make_quick "string" QCheck.string Msgpack.string Msgpack.get_string

let quick_bin = make_quick "bin" qcheck_bigstring Msgpack.bin Msgpack.get_bin

let quick_int64 =
  make_quick "int64" QCheck.int64 Msgpack.int64 Msgpack.get_int64

let strings length () =
  let eq a b = if not (a = b) then Alcotest.fail "string" in
  let s = String.make length 'x' in
  eq Msgpack.(string s |> to_string |> of_string_exn |> get_string) s

let arrays length content () =
  let eq a b = if not (a = b) then Alcotest.fail "array" in
  let a = Array.make length content in
  eq
    Msgpack.(
      array (fun x -> x) a
      |> to_string
      |> of_string_exn
      |> get_array (fun x -> x))
    a

let maps () =
  let eq a b = if not (a = b) then Alcotest.fail "maps" in
  let msg =
    let raw =
      [
        ("empty", String.make 0 'x');
        ("short", String.make 1 'x');
        ("medium", String.make (0xff + 1) 'x');
        ("long", String.make (0xff_ff + 1) 'x');
        ("empty", String.make 0 'x');
      ]
    in
    Msgpack.(dict string) raw
  in
  eq Msgpack.(to_string msg |> of_string_exn) msg

let basic_cases =
  [
    ("cases_compact", `Quick, cases_compact);
    ("constructor/accessor", `Quick, constructor_accessor);
  ]

let array_tests =
  let empty_string = Msgpack.string (String.make 0 'x') in
  let short_string = Msgpack.string (String.make 1 'x') in
  let medium_string = Msgpack.string (String.make (0xff + 1) 'x') in
  let elts =
    [
      ("nil", Msgpack.Nil);
      ("empty string", empty_string);
      ("short string", short_string);
      ("medium string", medium_string);
    ]
  in
  let make (name, elt) =
    [
      (Fmt.strf "empty array (%s)" name, `Quick, arrays 0 elt);
      (Fmt.strf "short array (%s)" name, `Quick, arrays 1 elt);
      (Fmt.strf "medium array (%s)" name, `Quick, arrays (0xff + 1) elt);
      (Fmt.strf "long array (%s)" name, `Slow, arrays (0xff_ff + 1) elt);
    ]
  in
  List.concat (List.map make elts)

let quickcheck_tests =
  List.map QCheck_alcotest.to_alcotest
    [ quick_bool; quick_int; quick_float; quick_string; quick_bin; quick_int64 ]

let string_tests =
  [
    ("empty string", `Quick, strings 0);
    ("short string", `Quick, strings 1);
    ("medium string", `Quick, strings (0xff + 1));
    ("long string", `Quick, strings (0xff_ff + 1));
  ]

let map_tests = [ ("maps", `Quick, maps) ]

let () =
  Alcotest.run "msgpackaf"
    [
      ("basic", basic_cases);
      ("array", array_tests);
      ("quickcheck", quickcheck_tests);
      ("string", string_tests);
      ("map", map_tests);
    ]
