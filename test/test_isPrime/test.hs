import Prime
import System.Random


main = do
    let a = take 100000 primes
    let b = last a
    let c = filter (flip isPrime 1231) [1..b]
    print $ length a
    print $ (length a) == (length c)

    
    
