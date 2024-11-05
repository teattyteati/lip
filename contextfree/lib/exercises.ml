open Types

(* Use this grammar record as a blueprint for the exercises. *)
let todo : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [              (* Indexes *)
        S --> "0S0"; (* 0 *)
        S --> "1S1"; (* 1 *)
        S --> "";    (* 2 *)
      ];
    start = S;
  }


(* #### Exercise 1, easy (zero_n_one_n) *)
let zero_n_one_n : grammar = {
  symbols = [ S ];
  terminals = [ '0'; '1' ];
  productions =
    [              (* Indexes *)
      S --> "0S1"; (* 0 *)
      S --> ""; (* 1 *)
    ];
  start = S;
}


(* #### Exercise 2, easy (palindromes) *)
let palindromes : grammar = {
  symbols = [ S; A ];
  terminals = [ '0'; '1' ];
  productions =
    [              (* Indexes *)
      S --> "0S0"; (* 0 *)
      S --> "1S1"; (* 1 *)
      S --> "A"; (*2*)
      A --> "0"; (*3*)
      A --> "1"; (*4*)
      S --> ""; (*5*)
    ];
  start = S;
}


(* #### Exercise 3, medium (balanced_parentheses) *)
let balanced_parentheses : grammar = {
  symbols = [ A; B; S ];
  terminals = [ '('; ')'; '['; ']'; '{'; '}' ];
  productions =
    [              (* Indexes *)
      A --> "(A)"; (* 0 *)
      A --> "A()"; (*1*)
      A --> "()A"; (*2*)

      B --> "[A]"; (*3*)
      B --> "A[]"; (*4*)
      B --> "[]A"; (*5*)
      
      S --> "{A}"; (*6*)
      S --> "A{}"; (*7*)
      S --> "{}A"; (*8*)
      
      A --> "B"; (*9*)
      A --> "S"; (*10*)
      A --> "" (*11*)
    ];
  start = A;
}


(* #### Exercise 4, hard (same_amount)

   Hint: model the language of words where the number of 0's is
   one greater than the number of 1's and viceversa, then combine them.
*)
let same_amount : grammar = {
  symbols = [ A; B ];
  terminals = [ '0'; '1' ];
  productions =
    [              (* Indexes *)
      A --> "1B"; (*0*)
      A --> "B1"; (*1*)
      B --> "A0"; (*2*)
      B --> "0A"; (*3*)
      A --> "";   (*4*)
    ];
  start = A;
}
