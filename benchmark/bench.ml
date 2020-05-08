open Benchmark
module Msgpack = Msgpackaf.Easy

let () =
  (* cases.mpac is a test data set from the C msgpack library repo *)
  let cases_raw =
    let file = "test/cases_compact.mpac" in
    let ic = open_in_bin file in
    let length = in_channel_length ic in
    let content = really_input_string ic length in
    close_in ic;
    Bigstringaf.of_string ~off:0 ~len:(String.length content) content
  in
  let cases_parsed = Msgpack.msgs_of_bigstring_exn cases_raw in
  (* Bigger messages *)
  let make_blob len =
    let open Bigarray in
    let bs = Array1.create char c_layout len in
    Array1.fill bs '\x00';
    bs
  in
  let bigger_parsed =
    let open Msgpack in
    map'
      [
        (string "name", string "Bigger");
        (string "tag", int64 Int64.max_int);
        ( string "embedded",
          map'
            [
              ( string "numbers",
                array' (Array.init 1_000 (fun i -> int64 (Int64.of_int i))) );
              (string "string", string (String.make 0x1000 'x'));
              (string "blob", bin (make_blob 65_536));
            ] );
      ]
  in
  let bigger_raw = Msgpack.to_bigstring bigger_parsed in
  (* Really big message *)
  let really_big_parsed =
    let open Msgpack in
    map'
      [
        (string "name", string "Really big!");
        (string "blob", bin (make_blob (10 * 1_024 * 1_024)));
      ]
  in
  let really_big_raw = Msgpack.to_bigstring really_big_parsed in
  ignore
  @@ throughput1 3 ~name:"Msgpack.msgs_of_bigstring" Msgpack.msgs_of_bigstring
       cases_raw;
  ignore
  @@ throughput1 3 ~name:"Msgpack.msgs_to_bigstring" Msgpack.msgs_to_bigstring
       cases_parsed;
  ignore
  @@ throughput1 3 ~name:"Msgpack.of_bigstring (bigger message)"
       Msgpack.of_bigstring bigger_raw;
  ignore
  @@ throughput1 3 ~name:"Msgpack.to_bigstring (bigger message)"
       Msgpack.to_bigstring bigger_parsed;
  Gc.compact ();
  ignore
  @@ throughput1 1 ~name:"Msgpack.of_bigstring (really big message)"
       Msgpack.of_bigstring really_big_raw;
  Gc.compact ();
  ignore
  @@ throughput1 1 ~name:"Msgpack.to_bigstring (really big message)"
       Msgpack.to_bigstring really_big_parsed;
  print_newline ()
