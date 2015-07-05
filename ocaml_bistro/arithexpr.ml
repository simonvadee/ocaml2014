(**************************************************************************)
(*                                                                        *)
(*                        Module ArithExpr			          *)
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

module ArithExpr : ARITHEXPR =
  struct
    
    type arith_expr =
      | Product of (arith_expr * arith_expr)
      | Div of (arith_expr * arith_expr)
      | Sum of (arith_expr * arith_expr)
      | Sub of (arith_expr * arith_expr)
      | Mod of (arith_expr * arith_expr)
      | Val of Bigint.Bigint.t

    type t = Bigint.Bigint.t

    let trim str =
      let rec trim_aux str len = function
	| find when find = len	->	""
	| cmp			->	
	   if ((String.get str cmp) != ' ')
	   then (String.make 1 (String.get str cmp))^(trim_aux str len (cmp + 1))
	   else (trim_aux str len (cmp + 1))
      in trim_aux str (String.length str) 0

    let string_of_arith_expr arith =
      let rec string_of_arith_expr_aux str = function
	| Product (left, right)	->		   
	   (string_of_arith_expr_aux (Printf.sprintf "(") left)^(string_of_arith_expr_aux (Printf.sprintf ") * (") right)^(Printf.sprintf ")");
	| Div	(left, right)	->
	   (string_of_arith_expr_aux (Printf.sprintf "(") left)^(string_of_arith_expr_aux (Printf.sprintf ") / (") right)^(Printf.sprintf ")");
	| Sum	(left, right)	->
	   (string_of_arith_expr_aux (Printf.sprintf "(") left)^(string_of_arith_expr_aux (Printf.sprintf ") + (") right)^(Printf.sprintf ")");
	| Sub	(left, right)	->
	   (string_of_arith_expr_aux (Printf.sprintf "(") left)^(string_of_arith_expr_aux (Printf.sprintf ") - (") right)^(Printf.sprintf ")");
	| Mod	(left, right)	->
	   (string_of_arith_expr_aux (Printf.sprintf "(") left)^(string_of_arith_expr_aux (Printf.sprintf ") %% (") right)^(Printf.sprintf ")");
	| Val value		->	Bigint.Bigint.string_of_bigint value
      in string_of_arith_expr_aux "" arith

    let rec solve_arith_expr = function
      | Product (left, right)	->	Bigint.Bigint.mul (solve_arith_expr left) (solve_arith_expr right)
      | Div	(left, right)	->	Bigint.Bigint.div (solve_arith_expr left) (solve_arith_expr right)
      | Sum	(left, right)	->	Bigint.Bigint.add (solve_arith_expr left) (solve_arith_expr right)
      | Sub	(left, right)	->	Bigint.Bigint.sub (solve_arith_expr left) (solve_arith_expr right)
      | Mod	(left, right)	->	Bigint.Bigint.modulo (solve_arith_expr left) (solve_arith_expr right)
      | Val value		->	value

  end
