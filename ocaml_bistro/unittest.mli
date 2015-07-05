(* INTERFACE MODULE UNITEST *)


module type UNITTEST =
  sig
    val print_test : unit -> unit
  end

module Add : UNITTEST
module Sub : UNITTEST
module Mul : UNITTEST
module Div : UNITTEST
module Modulo : UNITTEST
