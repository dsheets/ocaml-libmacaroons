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

type t
type third_party_caveat = { location : string; id : string; }

exception LibraryError of string
exception ValidationError of int
exception ThirdPartyCaveatError of int * int

val destroy  : t -> unit
val create   : loc:string -> key:string -> id:string -> t
val validate : t -> unit
val add_first_party_caveat : t -> string -> t
val add_third_party_caveat : t -> loc:string -> key:string -> id:string -> t
val num_third_party_caveats : t -> int
val third_party_caveats : t -> third_party_caveat list
val prepare_for_request : t -> t -> t
val location : t -> string
val identifier : t -> string
val signature : t -> string
val serialize : t -> string
val serialize_json : t -> string
val deserialize : string -> t
val inspect : t -> string
val copy : t -> t
val equal : t -> t -> bool

module Verifier : sig
  type macaroon = t
  type t

  val create  : unit -> t
  val destroy : t -> unit
  val satisfy_exact   : t -> string -> unit
  val satisfy_general : t -> (string -> bool) -> unit
  val verify : t -> macaroon -> string -> macaroon list -> unit
end
