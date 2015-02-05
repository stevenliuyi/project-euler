(* Problem 14: Longest Collatz sequence *)

(* The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million. *)

list = {#, #} & /@ Range[1*^6];

While[Length[list] > 1, 
    list = list /. {{_, 1} -> Sequence[],
    {a_, b_?EvenQ} -> {a, b/2}, {c_, d_?OddQ} -> {c, 3*d + 1}};]

longsetChainNum = list[[1]][[1]]
