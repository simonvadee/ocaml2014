(* *)
(* vadee_s *)
(* *)

type 'a mylist =
  | Item of ('a * 'a mylist)
  | Empty

let lol1 = Item("Simon", Item("Aurelien", Item("Arthur", Empty)))
let lol2 = Item("Jordan", Item("Pl", Item("Roman", Empty)))
let lol3 = Item("Oscar", Item("Cedric", Item("Bernulf", Empty)))
let lol4 = Item(lol1, Item(lol2, Item(lol3, Empty)))
	       
let rec length = function
  | Empty -> 0
  | Item(a, b) -> 1 + length b

let hd = function
  | Item(a, b) -> a
  | Empty -> raise(Failure "hd")

let tl = function
  | Item(a, b) -> b
  | Empty -> raise(Failure "tl")
		  
let rec nth (elem: 'a mylist) cpt = match elem, cpt with
  | _, m when m < 0 -> raise(Invalid_argument "List.nth")
  | Empty, _ -> raise(Failure "nth")
  | Item(a, b), 0 -> elem
  | Item(a, b), _ -> nth b (cpt-1)
			 
let rec append (l1: 'a mylist) (l2: 'a mylist) = match l1 with
  | Empty -> l2;
  | Item(a, b) -> Item(a, append b l2)

let rec rev =  function
  | Empty -> Empty
  | Item (a, b) -> append (rev b) (Item (a, Empty))

let rev_append l1 l2 = 
  append (rev l1) l2

let rec flatten = function
  | Empty -> Empty
  | Item (a, b) -> append a (flatten b)

let rec iter func = function
  | Empty -> ()
  | Item(a, b) ->
     begin
       func a;
       iter func b
     end

let map func l1 =
  let rec map1 func l2 = function
  | Empty -> rev l2
  | Item(a, b) -> map1 func (Item (func a, l2)) b
  in map1 func Empty l1

let rec fold_left func elem = function
  | Empty -> elem
  | Item(a, b) -> fold_left func (func elem a) b

let rec for_all func  = function
  | Empty -> true
  | Item(a, b) -> if !(func a)
		  then false
		  else for_all func b

let rec exists func = function
  | Empty -> false
  | Item(a, b) -> if (func a)
		  then true
		  else exists func b

let rec mem elem = function
  | Empty -> false
  | Item(a, b) -> if (a = elem)
		  then true
		  else mem elem b
			   
let rec memq elem = function
  | Empty -> false
  | Item(a, b) -> if (a == elem)
		  then true
		  else memq elem b

let filter func list1 =
  let rec filter1 func new_list= function
    | Empty -> rev new_list
    | Item(a, b) -> if (func a)
		    then filter1 func (Item (a, new_list)) list1
		    else filter1 func new_list list1
  in filter1 func Empty list1

let rec mem_assoc key = function
  | Empty -> false
  | Item((a, b), c) -> if (key = a)
		       then true
		       else mem_assoc key c

let rec assoc key = function
  | Empty -> raise(Not_found)
  | Item((a, b), c) -> if (key = a)
		       then b
		       else assoc key c

let split list1 =
  let rec split1 (l1, l2) = function
    | Empty ->  (l1, l2)
    | Item((a, b), c) -> split1 (Item (a, l1), Item (b, l2)) c
  in split1 (Empty, Empty) (rev list1)

let remove_assoc elem list1 =
  let rec remove_assoc1 elem ret_list = function
    | Empty -> ret_list
    | Item((a, b), c) -> if (a = elem)
			 then append c ret_list
			 else remove_assoc1 elem (Item ((a, b), c)) c
  in remove_assoc1 elem Empty list1
;;
