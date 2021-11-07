{-
	The function (extended_gcd a b) returns (x,y,gcd), where ax + by = gcd.
	Details: 
	    If b is zero, then gcd = a, and x = 1, y= 0.
	    If b is not zero, then we represent a as k*b+a', which means 
	        (kb+a')x + by = gcd
	        b(kx+y) + a'x = gcd. Recursive call the function with a=b, b=a'.
-}


module Extended_GCD
(
extended_gcd
) where

extended_gcd :: (Integral a) => a -> a -> (a, a, a)
extended_gcd a 0 = (1,0,a)
extended_gcd a b = (_y, _x -  (div a b) *_y, gcd)
    where (_x, _y, gcd) = extended_gcd b (mod a b)

