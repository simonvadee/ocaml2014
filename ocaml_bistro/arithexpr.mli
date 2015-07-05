(**************************************************************************)
(*                                                                        *)
(*                        Interface module ArithExpr                      *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module type ARITHEXPR =
  sig

    type arith_expr
    type t

    val string_of_arith_expr : arith_expr -> string
    val solve_arith_expr : arith_expr -> t

  end

module ArithExpr : ARITHEXPR
