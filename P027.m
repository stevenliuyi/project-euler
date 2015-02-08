(* Problem 27: Quadratic primes *)

(* Euler discovered the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

The incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

n² + an + b, where |a| < 1000 and |b| < 1000

where |n| is the modulus/absolute value of n
e.g. |11| = 11 and |−4| = 4
Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0. *)

consecutivePrimes[a_,b_] := 
    Select[PrimeQ/@ (#^2 + a*# + b & /@Range[0,Abs[b]])
        //Split//First, TrueQ]//Length;

n = 999;
c = consecutivePrimes@@@
    Flatten[Table[{i, j}, {i, -n, n}, {j, -n, n}],1]//Ordering //Last;
a = Quotient[c, 2*n+1] - n; b = Mod[c, 2*n+1] - n - 1;
If[b == -n-1, b = n; a = a-1];
Print[a]; Print[b]
