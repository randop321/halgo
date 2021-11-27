{-
-}


module CRT
(
chineseRemainder
) where

import Data.List.Ordered
import Extended_GCD
import System.Random

extract1of3 :: (a,a,a) -> a
extract1of3 (a,_,_) = a

chineseRemainder :: (Integral a) => [a] -> [a] -> a
chineseRemainder a b = mod ((mod (sum [bi*si | (bi,si) <- zip b s]) m)+m) m
    where
        m = product a
        n = [div m ai | ai <- a]
        k = [extract1of3 $ extended_gcd (mod ni ai) ai | (ni,ai) <- zip n a]
        s = [mod (ni*ki) m | (ni,ki) <- zip n k]
