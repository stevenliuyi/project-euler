(* Problem 9: Special Pythagorean triplet *)

(* A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.

Find the product abc. *)

pythagoreanTriplet[n_Integer] := 
    Times @@@ Select[Flatten[
        Table[{i, j, n - i - j}, {i, Floor[n/2]}, {j, i, Floor[n/2]}], 
        1], (#[[1]]^2 + #[[2]]^2 == #[[3]]^2) && #[[3]] > 0 &];
pythagoreanTriplet[1000]
