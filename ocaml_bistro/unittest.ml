(* MODULES UNITEST *)

module type UNITTEST =
  sig

    val print_test : unit -> unit
    
  end

module Add : UNITTEST =
  struct

    let print_test () =
      begin
	
	let short_pos = Bigint.Bigint.bigint_of_string "0b0111010"
	and long_pos = Bigint.Bigint.bigint_of_string "0x99865877456589789985478954789"
	and short_neg = Bigint.Bigint.bigint_of_string "-18"
	and long_neg = Bigint.Bigint.bigint_of_string "-1111245699987456987633654523325"
	in

	    Printf.printf "ADDITION TESTS : \n";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " + ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.add short_pos short_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print short_pos;
	    Printf.printf " + ";
	    Bigint.Bigint.print long_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.add short_pos long_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_pos;
	    Printf.printf " + ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.add long_pos short_pos);
	    Printf.printf "\n";

	    Bigint.Bigint.print long_pos;
	    Printf.printf " + ";
	    Bigint.Bigint.print long_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.add long_pos long_pos);
	    Printf.printf "\n";
	    

	    Bigint.Bigint.print short_pos;
	    Printf.printf " + ";
	    Bigint.Bigint.print short_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.add short_pos short_neg);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print short_neg;
	    Printf.printf " + ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.add short_neg short_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_neg;
	    Printf.printf " + ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.add long_neg short_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_pos;
	    Printf.printf " + ";
	    Bigint.Bigint.print long_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.add long_pos long_neg);
	    Printf.printf "\n";

	    
	    Bigint.Bigint.print short_neg;
	    Printf.printf " + ";
	    Bigint.Bigint.print short_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.add short_neg short_neg);
	    Printf.printf "\n";

	    Bigint.Bigint.print short_neg;
	    Printf.printf " + ";
	    Bigint.Bigint.print long_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.add short_neg long_neg);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_neg;
	    Printf.printf " + ";
	    Bigint.Bigint.print short_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.add long_neg short_neg);
	    Printf.printf "\n";

	    Bigint.Bigint.print long_neg;
	    Printf.printf " + ";
	    Bigint.Bigint.print long_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.add long_neg long_neg);
	    Printf.printf "\n";

	    Printf.printf "\n";
	    Printf.printf "\n";

      end
  end
    
module Sub : UNITTEST =
  struct
    
    let print_test () =
      begin
	
	let short_pos = Bigint.Bigint.bigint_of_string "0b0111010"
	and long_pos = Bigint.Bigint.bigint_of_string "0x99865877456589789985478954789"
	and short_neg = Bigint.Bigint.bigint_of_string "-18"
	and long_neg = Bigint.Bigint.bigint_of_string "-1111245699987456987633654523325"
	in
	
	Printf.printf "SUBSTRACTION TESTS : \n";
	Bigint.Bigint.print short_pos;
	Printf.printf " - ";
	Bigint.Bigint.print short_pos;
	Printf.printf " = ";
	Bigint.Bigint.print (Bigint.Bigint.sub short_pos short_pos);
	Printf.printf "\n";
	
	Bigint.Bigint.print short_pos;
	Printf.printf " - ";
	Bigint.Bigint.print long_pos;
	Printf.printf " = ";
	Bigint.Bigint.print (Bigint.Bigint.sub short_pos long_pos);
	Printf.printf "\n";
	
	Bigint.Bigint.print long_pos;
	Printf.printf " - ";
	Bigint.Bigint.print short_pos;
	Printf.printf " = ";
	Bigint.Bigint.print (Bigint.Bigint.sub long_pos short_pos);
	Printf.printf "\n";

	Bigint.Bigint.print long_pos;
	Printf.printf " - ";
	Bigint.Bigint.print long_pos;
	Printf.printf " = ";
	Bigint.Bigint.print (Bigint.Bigint.sub long_pos long_pos);
	Printf.printf "\n";
	

	Bigint.Bigint.print short_pos;
	Printf.printf " - ";
	Bigint.Bigint.print short_neg;
	Printf.printf " = ";
	Bigint.Bigint.print (Bigint.Bigint.sub short_pos short_neg);
	Printf.printf "\n";
	
	Bigint.Bigint.print short_neg;
	Printf.printf " - ";
	Bigint.Bigint.print short_pos;
	Printf.printf " = ";
	Bigint.Bigint.print (Bigint.Bigint.sub short_neg short_pos);
	Printf.printf "\n";
	
	Bigint.Bigint.print long_neg;
	Printf.printf " - ";
	Bigint.Bigint.print short_pos;
	Printf.printf " = ";
	Bigint.Bigint.print (Bigint.Bigint.sub long_neg short_pos);
	Printf.printf "\n";
	
	Bigint.Bigint.print long_pos;
	Printf.printf " - ";
	Bigint.Bigint.print long_neg;
	Printf.printf " = ";
	Bigint.Bigint.print (Bigint.Bigint.sub long_pos long_neg);
	Printf.printf "\n";

	
	Bigint.Bigint.print short_neg;
	Printf.printf " - ";
	Bigint.Bigint.print short_neg;
	Printf.printf " = ";
	Bigint.Bigint.print (Bigint.Bigint.sub short_neg short_neg);
	Printf.printf "\n";

	Bigint.Bigint.print short_neg;
	Printf.printf " - ";
	Bigint.Bigint.print long_neg;
	Printf.printf " = ";
	Bigint.Bigint.print (Bigint.Bigint.sub short_neg long_neg);
	Printf.printf "\n";
	
	Bigint.Bigint.print long_neg;
	Printf.printf " - ";
	Bigint.Bigint.print short_neg;
	Printf.printf " = ";
	Bigint.Bigint.print (Bigint.Bigint.sub long_neg short_neg);
	Printf.printf "\n";

	Bigint.Bigint.print long_neg;
	Printf.printf " - ";
	Bigint.Bigint.print long_neg;
	Printf.printf " = ";
	Bigint.Bigint.print (Bigint.Bigint.sub long_neg long_neg);
	Printf.printf "\n";

	Printf.printf "\n";
	Printf.printf "\n";

      end

  end
    
module Mul : UNITTEST =
  struct

    let print_test () =
      begin
	
	let short_pos = Bigint.Bigint.bigint_of_string "0b0111010"
	and long_pos = Bigint.Bigint.bigint_of_string "0x99865877456589789985478954789"
	and short_neg = Bigint.Bigint.bigint_of_string "-18"
	and long_neg = Bigint.Bigint.bigint_of_string "-1111245699987456987633654523325"
	in

	    Printf.printf "MULTIPLICATION TESTS : \n";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " * ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.mul short_pos short_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print short_pos;
	    Printf.printf " * ";
	    Bigint.Bigint.print long_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.mul short_pos long_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_pos;
	    Printf.printf " * ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.mul long_pos short_pos);
	    Printf.printf "\n";

	    Bigint.Bigint.print long_pos;
	    Printf.printf " * ";
	    Bigint.Bigint.print long_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.mul long_pos long_pos);
	    Printf.printf "\n";
	    

	    Bigint.Bigint.print short_pos;
	    Printf.printf " * ";
	    Bigint.Bigint.print short_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.mul short_pos short_neg);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print short_neg;
	    Printf.printf " * ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.mul short_neg short_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_neg;
	    Printf.printf " * ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.mul long_neg short_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_pos;
	    Printf.printf " * ";
	    Bigint.Bigint.print long_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.mul long_pos long_neg);
	    Printf.printf "\n";

	    
	    Bigint.Bigint.print short_neg;
	    Printf.printf " * ";
	    Bigint.Bigint.print short_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.mul short_neg short_neg);
	    Printf.printf "\n";

	    Bigint.Bigint.print short_neg;
	    Printf.printf " * ";
	    Bigint.Bigint.print long_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.mul short_neg long_neg);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_neg;
	    Printf.printf " * ";
	    Bigint.Bigint.print short_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.mul long_neg short_neg);
	    Printf.printf "\n";

	    Bigint.Bigint.print long_neg;
	    Printf.printf " * ";
	    Bigint.Bigint.print long_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.mul long_neg long_neg);
	    Printf.printf "\n";

	    Printf.printf "\n";
	    Printf.printf "\n";

      end
  end

module Div : UNITTEST =
  struct

    let print_test () =
      begin
	
	let short_pos = Bigint.Bigint.bigint_of_string "0b0111010"
	and long_pos = Bigint.Bigint.bigint_of_string "0x99865877456589789985478954789"
	and short_neg = Bigint.Bigint.bigint_of_string "-18"
	and long_neg = Bigint.Bigint.bigint_of_string "-1111245699987456987633654523325"
	in

	    Printf.printf "DIVISION TESTS : \n";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " / ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.div short_pos short_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print short_pos;
	    Printf.printf " / ";
	    Bigint.Bigint.print long_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.div short_pos long_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_pos;
	    Printf.printf " / ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.div long_pos short_pos);
	    Printf.printf "\n";

	    Bigint.Bigint.print long_pos;
	    Printf.printf " / ";
	    Bigint.Bigint.print long_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.div long_pos long_pos);
	    Printf.printf "\n";
	    

	    Bigint.Bigint.print short_pos;
	    Printf.printf " / ";
	    Bigint.Bigint.print short_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.div short_pos short_neg);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print short_neg;
	    Printf.printf " / ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.div short_neg short_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_neg;
	    Printf.printf " / ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.div long_neg short_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_pos;
	    Printf.printf " / ";
	    Bigint.Bigint.print long_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.div long_pos long_neg);
	    Printf.printf "\n";

	    
	    Bigint.Bigint.print short_neg;
	    Printf.printf " / ";
	    Bigint.Bigint.print short_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.div short_neg short_neg);
	    Printf.printf "\n";

	    Bigint.Bigint.print short_neg;
	    Printf.printf " / ";
	    Bigint.Bigint.print long_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.div short_neg long_neg);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_neg;
	    Printf.printf " / ";
	    Bigint.Bigint.print short_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.div long_neg short_neg);
	    Printf.printf "\n";

	    Bigint.Bigint.print long_neg;
	    Printf.printf " / ";
	    Bigint.Bigint.print long_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.div long_neg long_neg);
	    Printf.printf "\n";

	    Printf.printf "\n";
	    Printf.printf "\n";

      end
  end

module Modulo : UNITTEST =
  struct

    let print_test () =
      begin
	
	let short_pos = Bigint.Bigint.bigint_of_string "0b0111010"
	and long_pos = Bigint.Bigint.bigint_of_string "0x99865877456589789985478954789"
	and short_neg = Bigint.Bigint.bigint_of_string "-18"
	and long_neg = Bigint.Bigint.bigint_of_string "-1111245699987456987633654523325"
	in

	    Printf.printf "MODULO TESTS : \n";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " %% ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.modulo short_pos short_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print short_pos;
	    Printf.printf " %% ";
	    Bigint.Bigint.print long_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.modulo short_pos long_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_pos;
	    Printf.printf " %% ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.modulo long_pos short_pos);
	    Printf.printf "\n";

	    Bigint.Bigint.print long_pos;
	    Printf.printf " %% ";
	    Bigint.Bigint.print long_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.modulo long_pos long_pos);
	    Printf.printf "\n";
	    

	    Bigint.Bigint.print short_pos;
	    Printf.printf " %% ";
	    Bigint.Bigint.print short_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.modulo short_pos short_neg);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print short_neg;
	    Printf.printf " %% ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.modulo short_neg short_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_neg;
	    Printf.printf " %% ";
	    Bigint.Bigint.print short_pos;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.modulo long_neg short_pos);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_pos;
	    Printf.printf " %% ";
	    Bigint.Bigint.print long_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.modulo long_pos long_neg);
	    Printf.printf "\n";

	    
	    Bigint.Bigint.print short_neg;
	    Printf.printf " %% ";
	    Bigint.Bigint.print short_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.modulo short_neg short_neg);
	    Printf.printf "\n";

	    Bigint.Bigint.print short_neg;
	    Printf.printf " %% ";
	    Bigint.Bigint.print long_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.modulo short_neg long_neg);
	    Printf.printf "\n";
	    
	    Bigint.Bigint.print long_neg;
	    Printf.printf " %% ";
	    Bigint.Bigint.print short_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.modulo long_neg short_neg);
	    Printf.printf "\n";

	    Bigint.Bigint.print long_neg;
	    Printf.printf " %% ";
	    Bigint.Bigint.print long_neg;
	    Printf.printf " = ";
	    Bigint.Bigint.print (Bigint.Bigint.modulo long_neg long_neg);
	    Printf.printf "\n";

	    Printf.printf "\n";
	    Printf.printf "\n";

      end
  end
