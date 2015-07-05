(**************************************************************************)
(*                                                                        *)
(*                        Module Bigint                                   *)
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

module Bigint : BIGINT =
  struct

    type t = {sign : int; abs : int list}

    let create_bigint sign abs = {sign = sign; abs = abs}

    let rec print_list = function
      | []		->	()
      | head::tail	->	
				begin
				  Printf.printf "%d" head;
				  print_list tail
				end
	   
    let print = function
      | {sign = -1; abs = abs}	->	
					begin
					  Printf.printf "-";
					  print_list abs;
					end
      | {sign = _; abs = abs}	->	
					begin
					  print_list abs;
					end

    let trim list =
      let rec trim_aux is_trim new_list = function
	| head::tail	-> 
	   if (head = 0) && (is_trim = 0)
	   then trim_aux 0 new_list tail
	   else trim_aux 1 (List.append new_list [head]) tail
	| []		-> new_list
      in trim_aux 0 [] list

    let get_list_char c base =
      let rec get_list_char_aux c len base = function
	| error when error = len	->	raise Not_found
	| cmp				->
	   if c = (String.get base cmp)
	   then [cmp]
	   else get_list_char_aux c (String.length base)
				  base
				  (cmp + 1)
      in get_list_char_aux c (String.length base) base 0

    let get_bigint_list str base =
      let rec get_bigint_list_aux str len new_list base = function
	| cmp when cmp = len		->	new_list
	| cmp				->
	   get_bigint_list_aux str len (List.append new_list
						    (get_list_char (String.get str cmp) base)) base (cmp + 1)
      in get_bigint_list_aux str (String.length str) [] base 0

    let rec format_list list = function
      | (self, other) when self < other	->	format_list (List.append [0] list)
							    (self, other - 1)
      | _				->	list

(*					*)
(*		MAP OPERATIONS		*)
(*					*)

    let my_map2_add list1 list2 =
      let rec my_map2_add_aux new_list ret = function
	| (head1::tail1, head2::tail2)	->
		if (head1 + head2 + ret) > 9
		then
		  my_map2_add_aux (List.append new_list
		  [((head1 + head2 + ret) mod 10)]) 1 (tail1, tail2)
		else
		  my_map2_add_aux (List.append new_list
		  [(head1 + head2 + ret)]) 0 (tail1, tail2)
	| _				->
		if ret > 0
		then
		  List.rev (List.append new_list [ret])
		else
		  List.rev new_list
      in my_map2_add_aux [] 0 (List.rev list1, List.rev list2)

    let my_map2_sub list1 list2 =
      let rec my_map2_sub_aux new_list ret = function
	| (head1::tail1, head2::tail2) 	->
	   if ((head1 - ret) < head2)
	   then
	     my_map2_sub_aux (List.append new_list [(head1 + 10 - head2 - ret)])
			     1 (tail1, tail2)
	   else
	     my_map2_sub_aux (List.append new_list [(head1 - head2 - ret)])
			     0 (tail1, tail2)
	| _				->
	   if ((List.hd (List.rev new_list)) = 0
	       && (List.length new_list) > 1)
	   then List.tl (List.rev new_list)
	   else List.rev new_list
      in my_map2_sub_aux [] 0 (List.rev list1, List.rev list2)

(*					*)
(*		COMPARISONS		*)
(*					*)

    let compare_inf list1 list2 =
      let rec compare_inf_aux = function
	| (head1::tail1, head2::tail2)	->
	   if (head1 < head2)
	   then true
	   else if (head1 = head2)
	   then compare_inf_aux (tail1, tail2)
	   else false
	| _				->	true
      in compare_inf_aux ((format_list list1 ((List.length list1),
					      (List.length list2))),
			  (format_list list2 ((List.length list2),
					      (List.length list1))))

    let compare_sup list1 list2 =
      let rec compare_sup_aux = function
	| (head1::tail1, head2::tail2)	->
	   if (head1 > head2)
	   then true
	   else if (head1 = head2)
	   then compare_sup_aux (tail1, tail2)
	   else false
	| _				->	true
      in compare_sup_aux ((format_list list1 ((List.length list1),
					      (List.length list2))),
			  (format_list list2 ((List.length list2),
					      (List.length list1))))

    let compare_equal list1 list2 =
      let rec compare_equal_aux = function
	| (head1::tail1, head2::tail2)	->
	   if (head1 != head2)
	   then false
	   else compare_equal_aux (tail1, tail2)
	| _				->	true
      in compare_equal_aux ((format_list list1 ((List.length list1),
					      (List.length list2))),
			  (format_list list2 ((List.length list2),
					      (List.length list1))))

(*					*)
(*		OPERATIONS UTILS	*)
(*					*)

    let do_sub list1 list2 =
      let do_sub_aux list1 list2 =
	let res = (my_map2_sub list1 list2) in
	if (compare_equal res [0])
	then [0]
	else (trim res)
      in do_sub_aux (format_list list1 ((List.length list1),
					(List.length list2)))
		    (format_list list2 ((List.length list2),
					(List.length list1)))

    let do_add list1 list2 =
      let do_add_aux list1 list2 =
	my_map2_add list1 list2
      in do_add_aux (format_list list1 ((List.length list1), (List.length list2)))
		    (format_list list2 ((List.length list2), (List.length list1)))

    let rec init_mul_list = function
      | 0	->	[]
      |	cmp	->	List.append [0] (init_mul_list (cmp - 1))

    let get_mul_res list1 value pow =
      let rec get_mul_res_aux new_list value ret = function
	| []		->	
	   if ret > 0
	   then List.rev (List.append new_list [ret])
	   else List.rev new_list
	| head::tail	->	get_mul_res_aux (List.append new_list [((value * head + ret) mod 10)]) value ((value * head + ret) / 10) tail
      in get_mul_res_aux (init_mul_list pow) value 0 list1

    let my_map2_mul list1 list2 =
      let rec my_map2_mul_aux cmp list1 = function
	| head::tail	->	do_add (get_mul_res list1 head cmp) (my_map2_mul_aux (cmp + 1) list1 tail)
	| []		->	[0]
      in my_map2_mul_aux 0 (List.rev list1) (List.rev list2)

    let do_mul list1 list2 = my_map2_mul list1 list2

    let get_div_low dvd div =
      let rec get_div_low_aux dvd div cmp = function
	| true		->	get_div_low_aux (do_sub dvd div) div (cmp + 1) (compare_sup (do_sub dvd div) div)
	| false		->	cmp
      in get_div_low_aux dvd div 0 (compare_sup dvd div)

    let my_map2_modulo list1 list2 =
      let rec my_map2_modulo_aux div remain quot = function
	| head::tail		->
	   if (compare_sup (List.append remain [head]) div)
	   then my_map2_modulo_aux div
				(do_sub (List.append remain [head]) (do_mul div [(get_div_low (List.append remain [head]) div)]))
				(List.append quot [(get_div_low (List.append remain [head]) div)])
				tail
	   else my_map2_modulo_aux div
				(List.append remain [head])
				(List.append quot [0])
				tail
	| _			->	remain
      in my_map2_modulo_aux list2 [] [] list1

    let do_modulo list1 list2 = my_map2_modulo list1 list2

    let my_map2_div list1 list2 =
      let rec my_map2_div_aux div remain quot = function
	| head::tail		->
	   if (compare_sup (List.append remain [head]) div)
	   then my_map2_div_aux div
				(do_sub (List.append remain [head]) (do_mul div [(get_div_low (List.append remain [head]) div)]))
				(List.append quot [(get_div_low (List.append remain [head]) div)])
				tail
	   else my_map2_div_aux div
				(List.append remain [head])
				(List.append quot [0])
				tail
	| []			->	quot
      in my_map2_div_aux list2 [] [] list1

    let do_div list1 list2 = trim (my_map2_div list1 list2)

    let rec mul_pow tmp nb = function
      | 0	->	tmp
      |	cmp	->	mul_pow (do_mul tmp nb) nb (cmp - 1)

    let convert_dec list len_base =
      let rec convert_dec_aux len cmp = function
	| []		->	[0]
	| head::tail	->
	   let res = (mul_pow [head] len cmp)
	   in do_add res (convert_dec_aux len (cmp + 1) tail)
      in convert_dec_aux len_base 0 (List.rev list)

    let detect_base str =
      begin
	if String.length str > 2
	then
	  let detect_base_aux str = function
	    | ('0', 'x')	->	convert_dec (get_bigint_list (String.sub str 2 ((String.length str) - 2)) "0123456789abcdef") [1; 6]
	    | ('0', 'b')	->	convert_dec (get_bigint_list (String.sub str 2 ((String.length str) - 2)) "01") [2]
	    | ('0', _)		->	convert_dec (get_bigint_list (String.sub str 1 ((String.length str) - 1)) "01234567") [8]
	    | _			->	get_bigint_list str "0123456789"
	  in detect_base_aux str (String.get str 0, String.get str 1)
	else
	  get_bigint_list str "0123456789"
      end

    let bigint_of_string str =
      let bigint_of_string_aux str = function
	| '-'	->	{sign = (-1); abs = (detect_base (String.sub str 1 ((String.length str) - 1)))}
	| '+'	->	{sign = 1; abs = (detect_base (String.sub str 1 ((String.length str) - 1)))}
	| _	->	{sign = 1; abs = (detect_base str)}
      in bigint_of_string_aux str (String.get str 0)

    let rec build_string str = function
      | head::tail	->	build_string (str^(String.make 1
				(Char.chr (head + 48)))) tail
      | []		->	str

    let string_of_bigint = function
      | {sign = -1; abs = abs}	->	build_string "-" abs
      | {sign = 1; abs = abs}	->	build_string "" abs
      | _			->	raise Not_found

(*					*)
(*		OPERATIONS		*)
(*					*)

    let sub {sign = s1; abs = abs1} {sign = s2; abs = abs2} =
      let sub_aux b1 b2 = function
	| (1, 1)	->
	   if (compare_sup b1 b2)
	   then {sign = 1; abs = (do_sub b1 b2)}
	   else {sign = (-1); abs = (do_sub b2 b1)}
	| (1, -1)	->	{sign = 1; abs = (do_add b1 b2)}
	| (-1, 1)	->	{sign = (-1); abs = (do_add b1 b2)}
	| (-1, -1)	->
	   if (compare_inf b1 b2)
	   then {sign = 1; abs = (do_sub b2 b1)}
	   else {sign = (-1); abs = (do_sub b1 b2)}
	| _		->	raise Not_found
      in sub_aux abs1 abs2 (s1, s2)

    let add {sign = s1; abs = abs1} {sign = s2; abs = abs2} =
      let add_aux b1 b2 = function
	| (1, 1)	->	{sign = 1; abs = (do_add b1 b2)}
	| (1, -1)	->
	   if (compare_sup b1 b2)
	   then {sign = 1; abs = (do_sub b1 b2)}
	   else {sign = (-1); abs = (do_sub b2 b1)}
	| (-1, 1)	->
	   if (compare_inf b1 b2)
	   then {sign = 1; abs = (do_sub b2 b1)}
	   else {sign = (-1); abs = (do_sub b1 b2)}
	| (-1, -1)	->	{sign = (-1); abs = (do_add b1 b2)}
	| _		->	raise Not_found
      in add_aux abs1 abs2 (s1, s2)

    let mul {sign = s1; abs = abs1} {sign = s2; abs = abs2} =
      let mul_aux b1 b2 = function
	| (1, 1)	->	{sign = 1; abs = (do_mul b1 b2)}
	| (1, -1)	->	{sign = (-1); abs = (do_mul b1 b2)}
	| (-1, 1)	->	{sign = (-1); abs = (do_mul b1 b2)}
	| (-1, -1)	->	{sign = 1; abs = (do_mul b1 b2)}
	| _		->	raise Not_found
      in mul_aux abs1 abs2 (s1, s2)

    let div {sign = s1; abs = abs1} {sign = s2; abs = abs2} =
      let div_aux b1 b2 = function
	| (1, 1)	->	{sign = 1; abs = (do_div b1 b2)}
	| (1, -1)	->	{sign = (-1); abs = (do_div b1 b2)}
	| (-1, 1)	->	{sign = (-1); abs = (do_div b1 b2)}
	| (-1, -1)	->	{sign = 1; abs = (do_div b1 b2)}
	| _		->	raise Not_found
      in div_aux abs1 abs2 (s1, s2)

    let div {sign = s1; abs = abs1} {sign = s2; abs = abs2} =
      let div_aux b1 b2 = function
	| (1, 1)	->	{sign = 1; abs = (do_div b1 b2)}
	| (1, -1)	->	{sign = (-1); abs = (do_div b1 b2)}
	| (-1, 1)	->	{sign = (-1); abs = (do_div b1 b2)}
	| (-1, -1)	->	{sign = 1; abs = (do_div b1 b2)}
	| _		->	raise Not_found
      in div_aux abs1 abs2 (s1, s2)

    let modulo {sign = s1; abs = abs1} {sign = s2; abs = abs2} =
      let modulo_aux b1 b2 = function
	| ((-1), _)	->	{sign = (-1); abs = (do_modulo b1 b2)}
	| (1, _)	->	{sign = 1; abs = (do_modulo b1 b2)}
	| _		->	raise Not_found
      in modulo_aux abs1 abs2 (s1, s2)

  end
