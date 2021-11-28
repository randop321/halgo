{-
    Chinese Remainder Theorem:
        Given a list of modular equations:
            x = a1 mod b1
            x = a2 mod b2
            ...
        Find x satisfies all above equations.
        There is no garantee that any pair of bi, bj are coprimes, so no solutions may occur.

    chineseRemainder [(ai,bi)]:
        return a number x which satisfies all equations, or return No Solution.
-}


module CRT
(
chineseRemainder
) where

import Data.List
import Extended_GCD

exCRT :: (Integral a) => (a,a) -> (a,a) -> (a,a)
exCRT (a,b) (c,d) = (e,f)
    where
        (t1',t2',g) = extended_gcd b d
        t1 = t1' * (div (c-a) g)
        f = div (b*d) g
        e = mod ((mod (a + t1*b) f) + f) f

chineseRemainder :: (Integral a) => [(a,a)] -> (a,a)
chineseRemainder eqs 
    | or $ map (\(a,b) -> (mod (fst res) b) /= a) eqs = 
        error $ "No solution."
    | otherwise = res
    where
        res = foldl1 (\a b -> exCRT a b) eqs
