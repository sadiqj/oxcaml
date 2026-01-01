(* TEST
 {
   runtime4;
   skip;
 }{
   include runtime_events;
   runtime5;
   { bytecode; }
   { native; }
 }
*)

(* Tests that:
 * - the perf_sample array is passed to runtime_begin and runtime_end callbacks
 * - when no OCAML_RUNTIME_EVENTS_PERF_COUNTERS env var is set, the array is empty
 * - the callback signatures match the expected API
 *)

open Runtime_events

(* External for getting length of unboxed product arrays *)
external[@layout_poly] array_length : ('a : any mod separable). 'a array -> int =
  "%array_length"

let samples_seen_begin = ref 0
let samples_seen_end = ref 0
let empty_arrays_begin = ref 0
let empty_arrays_end = ref 0

let runtime_begin _domain_id _ts phase (perf_samples : perf_sample array) =
    incr samples_seen_begin;
    if array_length perf_samples = 0 then
      incr empty_arrays_begin

let runtime_end _domain_id _ts phase (perf_samples : perf_sample array) =
    incr samples_seen_end;
    if array_length perf_samples = 0 then
      incr empty_arrays_end

let () =
    start ();
    let cursor = create_cursor None in
    let callbacks = Callbacks.create ~runtime_begin ~runtime_end () in
    (* Trigger some GC activity *)
    for _ = 1 to 10 do
      ignore (Sys.opaque_identity (Array.make 1000 0));
      Gc.minor ()
    done;
    ignore (read_poll cursor callbacks None);
    (* Verify we received some callbacks *)
    assert (!samples_seen_begin > 0);
    assert (!samples_seen_end > 0);
    (* When no perf counters env var is set, all arrays should be empty *)
    assert (!empty_arrays_begin = !samples_seen_begin);
    assert (!empty_arrays_end = !samples_seen_end);
    print_endline "perf_samples callback test passed"
