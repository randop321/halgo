{-
    primes: an infinity list of primes with tricky implementation
    primes !! k: return the kth prime
    
    isPrime: Miller Rabin Test to 30 tests 
    isPrime n s: test whether n is prime or not with random seed s
    
    primeFactoring: prime factoring and return a list of factors using Pollard's Rho Algorithm
    primeFactoring n s: factor n with random seed s
-}


module Prime
(
primes,
isPrime
) where

import Data.List.Ordered
import MathBasic
import System.Random

primes = 2:3:minus [5,7..] (foldr (\p s -> p*p : union [p*p+p, p*p+2*p..] s) [] primes)

-- Miller Rabin Test
--  find2kb n -> (k,b) s.t. 2^k*b = n
find2kb :: Integral a => a -> (Int,a)
find2kb n
    | mod n 2 == 1 = (0,n)
    | otherwise = (k+1,b)
        where (k,b) = find2kb (div n 2)

millerRabinSeqTest :: (Integral a) => a -> Int -> a -> Bool
millerRabinSeqTest v k n
    | k < 0 = False
    | otherwise = (v == n-1) || (millerRabinSeqTest (mod (v*v) n) (k-1) n)

millerRabinTest :: (Integral a) => a -> a -> Bool
millerRabinTest n a
    | a<1 || a>n-1 = 
        error $ "Failed to Miller-Rabin Test: out of range"
    | n == 2 = True
    | n < 2 = False
    | mod n 2 == 0 = False
    | otherwise = (init_val == 1) || (millerRabinSeqTest init_val k n)
    where
        (k,b) = find2kb (n-1)
        init_val = fastpow a b n

-- isPrime n: return True if n is prime
isPrime :: (Integral a) => a -> Int -> Bool
isPrime n seed
    | n <= 1 = False
    | n == 2 = True
    | mod n 2 == 0 = False
    | otherwise = and $ map (millerRabinTest n) randomlist
    where
        init_val = (1, mkStdGen seed)
        randomlist = map (\x -> (mod (fromIntegral $ fst x) (n-1))+1) $ scanl (\a x -> next $ snd a) init_val [1..30]
        
        
        
-- Pollard's Rho Algorithm
-- assuming n is not prime
pollardsRhoSeq :: (Integral a) => a -> a -> a -> a -> a
pollardsRhoSeq n x y k
    | g == 1 = pollardsRhoSeq n xx yy k
    | g == n = n
    | otherwise = g
    where
        g = gcd (abs (xx-yy))  n
        xx = mod (x*x+k) n
        y1 = mod (y*y+k) n
        yy = mod (y1*y1+k) n

pollardsRho :: (Integral a) => a -> StdGen -> a
pollardsRho n stdgen
    | (mod n 2) == 0 = 2
    | g == n = pollardsRho n c
    | otherwise = g
    where
        g = pollardsRhoSeq n m m k
        a = next stdgen
        b = next $ snd a
        c = snd b
        m = fromIntegral $ fst a
        k = fromIntegral $ fst b
        
primeFactoring :: (Integral a) => a -> Int -> [a]
primeFactoring n seed
    | n < 0 = -1 : (primeFactoring (-n) seed)
    | n <=1 = []
    | isPrime n seed = [n]
    | otherwise = (primeFactoring a seed) ++ (primeFactoring (div n a) seed)
    where
        a = pollardsRho n $ mkStdGen seed

