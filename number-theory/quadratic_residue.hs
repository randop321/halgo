{-
    Quadratic residue
    sqrtRootModP n p
        return x^2 = n mod p
        using Tonelli-Shanks algorithm
-}


module QuadraticResidue
(
sqrtRootModP
) where

import Data.List
import Prime
import MathBasic
import Data.Bits

sqrtRootModP :: (Integral a) => a -> a -> a
sqrtRootModP n p 
    | n' == 0 = 0
    | (fastpow n' (div (p-1) 2)  p) == (p-1) = 
        error $ "No Solution."
    | otherwise = tonelliShanks n' p
    where
        n' = mod n p

tonelliShanksSeq :: (Integral a) => a -> a -> a -> Int -> Int -> a -> a -> a
tonelliShanksSeq r n t s m b p
    | t == 1 = r
    | (fastpow t (fromInteger $ shift 1 (m-1)) p) == 1 = tonelliShanksSeq r n t s (m-1) b p
    | otherwise = tonelliShanksSeq (mod (r*c) p) n (mod ((mod (t*c) p)*c) p) s (m-1) b p
    where
        c = fastpow b (fromInteger $ shift 1 (s-1-m)) p

tonelliShanks :: (Integral a) => a -> a -> a
tonelliShanks n p = tonelliShanksSeq r n t s (s-1) b p
    where
        (s,q) = find2kb (p-1)
        z = fromInteger $ head [i | i <- primes, (fastpow (fromInteger i)  (div (p-1) 2)  p) == p-1]
        r = fastpow n (div (q+1) 2) p
        t = fastpow n q p
        b = fastpow z q p

    
        


