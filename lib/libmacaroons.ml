(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Ctypes
open Foreign

type t = unit ptr

type third_party_caveat = {
  location : string;
  id : string;
}

exception LibraryError of string
exception ValidationError of int
exception ThirdPartyCaveatError of int * int

let returncode = Macaroons_probe.(view
  ~read:returncode_of_code
  ~write:code_of_returncode
  ty_returncode)

let allocate_returncode () =
  coerce (ptr Macaroons_probe.ty_returncode) (ptr returncode)
    Macaroons_probe.(Ctypes.allocate ty_returncode z_returncode)

let t = ptr void

let len_size_t s = Unsigned.Size_t.of_int (String.length s)

let new_null_ptr_ptr typ =
  Ctypes.allocate (ptr typ) (coerce (ptr void) (ptr typ) null)

let check_returncode destroy f =
  let returncode = allocate_returncode () in
  let r = f returncode in
  Macaroons_probe.(match !@returncode with
  | Success -> r
  | other ->
    destroy r;
    raise (LibraryError (string_of_returncode other))
  )

let destroy =
  let c = foreign "macaroon_destroy"
    (t @-> returning void)
  in
  fun macaroon -> c macaroon

let create : string -> string -> string -> t =
  let c = foreign "macaroon_create"
    (string @-> size_t @-> string @-> size_t @-> string @-> size_t
     @-> ptr returncode @-> returning t)
  in
  fun location key id ->
    check_returncode destroy (
      c
        location (len_size_t location)
        key (len_size_t key)
        id (len_size_t id)
    )

let validate =
  let c = foreign "macaroon_validate"
    (t @-> returning int)
  in
  fun macaroon ->
    let r = c macaroon in
    if r <> 0 then raise (ValidationError r)
    else ()

let add_first_party_caveat =
  let c = foreign "macaroon_add_first_party_caveat"
    (t @-> string @-> size_t
     @-> ptr returncode @-> returning t)
  in
  fun macaroon predicate ->
    check_returncode destroy (
      c macaroon predicate (len_size_t predicate)
    )

let add_third_party_caveat =
  let c = foreign "macaroon_add_third_party_caveat"
    (t
     @-> string @-> size_t @-> string @-> size_t @-> string @-> size_t
     @-> ptr returncode @-> returning t)
  in
  fun macaroon location key id ->
    check_returncode destroy (
      c macaroon
        location (len_size_t location)
        key (len_size_t key)
        id (len_size_t id)
    )

let num_third_party_caveats =
  let c = foreign "macaroon_num_third_party_caveats"
    (t @-> returning int)
  in
  fun macaroon -> c macaroon

let third_party_caveats =
  let c = foreign "macaroon_third_party_caveat"
    (t @-> int
     @-> ptr (ptr char) @-> ptr size_t @-> ptr (ptr char) @-> ptr size_t
     @-> returning int)
  in
  fun macaroon ->
    let rec accumulate acc = function
      | 0 -> acc
      | n ->
        let n = n - 1 in
        let loc = new_null_ptr_ptr char in
        let loc_sz = Ctypes.allocate size_t (Unsigned.Size_t.of_int 0) in
        let id = new_null_ptr_ptr char in
        let id_sz = Ctypes.allocate size_t (Unsigned.Size_t.of_int 0) in
        let r = c macaroon n loc loc_sz id id_sz in
        if r < 0
        then raise (ThirdPartyCaveatError (n,r))
        else {
          location = string_from_ptr (!@ loc)
            ~length:(Unsigned.Size_t.to_int (!@ loc_sz));
          id = string_from_ptr (!@ id)
            ~length:(Unsigned.Size_t.to_int (!@ id_sz));
        }::acc
    in
    accumulate [] (num_third_party_caveats macaroon)

let prepare_for_request =
  let c = foreign "macaroon_prepare_for_request"
    (t @-> t @-> ptr returncode @-> returning t)
  in
  fun macaroon discharge -> check_returncode destroy (c macaroon discharge)

let location =
  let c = foreign "macaroon_location"
    (t @-> ptr (ptr char) @-> ptr size_t @-> returning void)
  in
  fun macaroon ->
    let loc = new_null_ptr_ptr char in
    let loc_sz = Ctypes.allocate size_t (Unsigned.Size_t.of_int 0) in
    c macaroon loc loc_sz;
    string_from_ptr (!@ loc) ~length:(Unsigned.Size_t.to_int (!@ loc_sz))

let identifier =
  let c = foreign "macaroon_identifier"
    (t @-> ptr (ptr char) @-> ptr size_t @-> returning void)
  in
  fun macaroon ->
    let id = new_null_ptr_ptr char in
    let id_sz = Ctypes.allocate size_t (Unsigned.Size_t.of_int 0) in
    c macaroon id id_sz;
    string_from_ptr (!@ id) ~length:(Unsigned.Size_t.to_int (!@ id_sz))

let signature =
  let c = foreign "macaroon_signature"
    (t @-> ptr (ptr char) @-> ptr size_t @-> returning void)
  in
  fun macaroon ->
    let sign = new_null_ptr_ptr char in
    let sign_sz = Ctypes.allocate size_t (Unsigned.Size_t.of_int 0) in
    c macaroon sign sign_sz;
    string_from_ptr (!@ sign) ~length:(Unsigned.Size_t.to_int (!@ sign_sz))

let serialize =
  let sz_hint = foreign "macaroon_serialize_size_hint"
    (t @-> returning size_t)
  in
  let c = foreign "macaroon_serialize"
    (t @-> ptr char @-> size_t @-> ptr returncode @-> returning int)
  in
  fun macaroon ->
    let sz = sz_hint macaroon in
    let buf = Ctypes.allocate_n char ~count:(Unsigned.Size_t.to_int sz) in
    ignore (check_returncode (fun _ -> ()) (c macaroon buf sz));
    coerce (ptr char) string buf

let serialize_json =
  let sz_hint = foreign "macaroon_serialize_json_size_hint"
    (t @-> returning size_t)
  in
  let c = foreign "macaroon_serialize_json"
    (t @-> ptr char @-> size_t @-> ptr returncode @-> returning int)
  in
  fun macaroon ->
    let sz = sz_hint macaroon in
    let buf = Ctypes.allocate_n char ~count:(Unsigned.Size_t.to_int sz) in
    ignore (check_returncode (fun _ -> ()) (c macaroon buf sz));
    coerce (ptr char) string buf

let deserialize =
  let c = foreign "macaroon_deserialize"
    (ocaml_string @-> ptr returncode @-> returning t)
  in
  fun buf ->
    check_returncode destroy (c (ocaml_string_start buf))

let inspect =
  let sz_hint = foreign "macaroon_inspect_size_hint"
    (t @-> returning size_t)
  in
  let c = foreign "macaroon_inspect"
    (t @-> ptr char @-> size_t @-> ptr returncode @-> returning int)
  in
  fun macaroon ->
    let sz = sz_hint macaroon in
    let buf = Ctypes.allocate_n char ~count:(Unsigned.Size_t.to_int sz) in
    ignore (check_returncode (fun _ -> ()) (c macaroon buf sz));
    coerce (ptr char) string buf

let copy =
  let c = foreign "macaroon_copy"
    (t @-> ptr returncode @-> returning t)
  in
  fun macaroon -> check_returncode destroy (c macaroon)

let equal =
  let c = foreign "macaroon_cmp"
    (t @-> t @-> returning int)
  in
  fun macaroon_a macaroon_b -> (c macaroon_a macaroon_b) = 0

module Verifier = struct
  type macaroon = t
  type t = unit ptr

  let macaroon = t
  let t = ptr void

  let general_check = funptr
    (ptr void @-> ptr char @-> size_t @-> returning int)

  let create =
    let c = foreign "macaroon_verifier_create" (void @-> returning t) in
    fun () -> c ()

  let destroy =
    let c = foreign "macaroon_verifier_destroy" (t @-> returning void) in
    fun verifier -> c verifier

  let satisfy_exact =
    let c = foreign "macaroon_verifier_satisfy_exact"
      (t @-> string @-> size_t @-> ptr returncode @-> returning int)
    in
    fun verifier predicate ->
      let r = check_returncode (fun _ -> ())
        (c verifier predicate (len_size_t predicate))
      in
      assert (r = 0);
      ()

  let satisfy_general =
    let c = foreign "macaroon_verifier_satisfy_general"
      (t @-> general_check @-> ptr void
       @-> ptr returncode @-> returning int)
    in
    fun verifier check ->
      (* TODO: keep the closure to prevent GC? *)
      let caveat_check _ buf sz =
        if check (string_from_ptr buf ~length:(Unsigned.Size_t.to_int sz))
        then 0
        else -1
      in
      ignore (check_returncode (fun _ -> ())
                (c verifier caveat_check null)
      )

  let verify =
    let c = foreign "macaroon_verify"
      (t @-> macaroon @-> ocaml_string @-> size_t
       @-> ptr macaroon @-> size_t @-> ptr returncode @-> returning int)
    in
    fun verifier macaroon key discharges ->
      let dl = List.length discharges in
      let dms = Ctypes.allocate_n (ptr void) ~count:dl in
      ignore (List.fold_left (fun i d ->
        (dms +@ i) <-@ d;
        i + 1
      ) 0 discharges);
      ignore (check_returncode (fun _ -> ())
                (c verifier macaroon
                   (Ctypes.ocaml_string_start key) (len_size_t key)
                   dms (Unsigned.Size_t.of_int dl))
      )

end

