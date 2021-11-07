{-
    Basic functions:
        - fastpow: calc a^n mod b in O(log n)
            fastpow a n b = a^n % b
        - inv: calc the inverse element of a s.t. a^(-1) mod b and b is a prime
            inv a b = a^(-1) % b
-}


module MathBasic
(
fastpow,
inv
) where


--- fastpow a n b = a^n % b
fastpow :: (Integral a) => a -> a -> a -> a
fastpow a 0 b = 1
fastpow a n b
    | odd n = mod (a*res) b
    | otherwise = res
    where res = fastpow (mod (a*a) b) (div n 2) b

-- inv a b = a^(-1) % b
inv :: (Integral a) => a -> a -> a
inv a b = fastpow a (b-2) b
