open Toylexer.Token
open Toylexer.Main

(*let%test "test_frequencies_1" =
  lexer "x=y; x=x+1" |> frequency 3 = [(ID "x", 3); (ASSIGN, 2); (ID "y", 1)]*)

(* YOUR TESTS HERE *)

let%test "frequency_test" = frequency 3 [ID("x"); ASSIGN; ID("y"); SEQ; ID("x"); ASSIGN; ID("x"); PLUS; CONST("1")] = [(ID "x", 3); (ASSIGN, 2); (ID "y", 1)];;
let%test "atok_test" = lexer "Abc123" = [ATOK "Abc123"; EOF];;