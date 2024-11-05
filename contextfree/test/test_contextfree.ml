open Contextfree
open Types
open Grammar
open Exercises

let to_string = string_of_sentform

let%test "example_test" = derive todo [ 0; 1; 0; 1; 2 ] |> to_string = "01011010"

(** Following the example test, complete the tests below.

    For each test, provide the right sequence of productions
    to generate the word on the right of the "=".
*)

(* #### Exercise 1 *)
let%test "zero_n_one_n_1" = derive zero_n_one_n [ 1 ] |> to_string = ""
let%test "zero_n_one_n_2" = derive zero_n_one_n [ 0; 1 ] |> to_string = "01"
let%test "zero_n_one_n_3" = derive zero_n_one_n [ 0;0;0;0;0;0;0;0;0;0;1 ] |> to_string = "00000000001111111111"

(* #### Exercise 2 *)
let%test "palindromes_1" = derive palindromes [ 1; 1; 2; 3 ] |> to_string = "11011"
let%test "palindromes_2" = derive palindromes [ 0; 1; 5 ] |> to_string = "0110"
let%test "palindromes_3" = derive palindromes [ 0; 1; 1; 1; 2; 3 ] |> to_string = "011101110"

(* #### Exercise 3 *)
let%test "balanced_parentheses_1" = derive balanced_parentheses [ 2; 9; 5; 10; 8; 11 ] |> to_string = "()[]{}"
let%test "balanced_parentheses_2" = derive balanced_parentheses [ 9; 4; 0; 10; 6; 11 ] |> to_string = "({})[]"
let%test "balanced_parentheses_3" = derive balanced_parentheses [ 0; 10; 6; 9; 5; 9; 3; 10; 8; 0; 11 ] |> to_string = "({[][{}()]})"

(* #### Exercise 4 *)
let%test "zero_one_same_1" = derive same_amount [ 4 ] |> to_string = ""
let%test "zero_one_same_2" = derive same_amount [ 0; 3; 1; 2; 4 ] |> to_string = "1001"
let%test "zero_one_same_3" = derive same_amount [ 1; 2; 1; 2; 1; 3; 1; 3; 4 ] |> to_string = "00110101"
let%test "zero_one_same_4" = derive same_amount [ 0; 2; 1; 3; 1; 3; 1; 3; 4 ] |> to_string = "10001110"

(* fatto da me *)
let%test "zero_one_same_5" = derive same_amount [ 5;7;0;3;4 ] |> to_string = "0110" (* per produrre questa parola e "1001" servono 3 symbols *)