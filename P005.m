(* Problem 5: Smallest multiple *)

(* 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder. *)

(* What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20? *)

smallestMultiple[n_Integer] := 
    Times @@ Power @@@ Last /@ Split[
        Sort[Flatten[FactorInteger /@ Range[n], 1]],
        #1[[1]] == #2[[1]] &
    ];

smallestMultiple[20]

(* use built-in function LCM *)

LCM @@ Range[20]
