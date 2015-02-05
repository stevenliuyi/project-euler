(* Problem 17: Number letter counts *)

(* If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage. *)

onesRule = {
    0 -> "", 1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four",
    5 -> "five", 6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine"};
teenRule = {
    10 -> "ten", 11 -> "eleven", 12 -> "twelve", 13 -> "thirteen",
    14 -> "fourteen", 15 -> "fifteen", 16 -> "sixteen", 17 -> "seventeen",
    18 -> "eighteen", 19 -> "nineteen"};
tensRule = {
    2 -> "twenty", 3 -> "thirty", 4 -> "forty", 5 -> "fifty", 
    6 -> "sixty", 7 -> "seventy", 8 -> "eighty", 9 -> "ninety"};

hundreds[n_Integer] := n~Quotient~100; tens[n_Integer] := n~Mod~100;

hundredsWord[n_Integer] := 
    hundreds[n] /. (10 -> "one thousand") /. (0 -> "") /.
        (onesRule /. HoldPattern[x_ -> y_] :> x -> y <> " hundred");

tensWord[n_Integer] := tens[n] /. onesRule /. teenRule /.
    (Flatten@ Outer[#1*10 + #2 -> #1 &, Range[2, 9], Range[0, 9]] /. 
        tensRule /. HoldPattern[x_ -> y_] :> x -> y <> " " <>
        (x~Mod~10 /. onesRule))

word[n_Integer] := 
    hundredsWord[n] <> If[hundreds[n]*tens[n] == 0, "", " and "] <> tensWord[n];

Length@StringCases[word[#], WordCharacter] & /@ Range[1000] // Total
