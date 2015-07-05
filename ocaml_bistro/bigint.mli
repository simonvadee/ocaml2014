(**************************************************************************)
(*                                                                        *)
(*                        Interface module Bigint                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module type BIGINT =
  sig

    type t

    val create_bigint : int -> int list -> t
    val bigint_of_string : string -> t
    val string_of_bigint : t -> string
    val print : t -> unit
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val modulo : t -> t -> t

  end

module Bigint : BIGINT
