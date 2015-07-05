(**************************************************************************)
(*                                                                        *)
(*                                 Main.ml                                *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

let main () =
  begin
    Unittest.Add.print_test ();
    Unittest.Sub.print_test ();
    Unittest.Mul.print_test ();
    Unittest.Div.print_test ();
    Unittest.Modulo.print_test ();
  end

let _ = main ()
