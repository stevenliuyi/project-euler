(* Problem 9: Special Pythagorean triplet *)
pythagoreanTriplet[n_Integer] := 
  Times @@@ 
   Select[Flatten[
     Table[{i, j, n - i - j}, {i, Floor[n/2]}, {j, i, Floor[n/2]}], 
     1], (#[[1]]^2 + #[[2]]^2 == #[[3]]^2) && #[[3]] > 0 &];
pythagoreanTriplet[1000]
