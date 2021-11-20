{-
	primes: an infinity list of primes with tricky implementation
    primes !! k: return the kth prime
-}


module Prime
(
primes
) where

import Data.List.Ordered

primes = 2:3:minus [5,7..] (foldr (\p s -> p*p : union [p*p+p, p*p+2*p..] s) [] primes)
