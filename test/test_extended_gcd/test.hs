import Extended_GCD

import System.Random

process :: (Int, Int) -> (Int, Int, Int)
process (a,b) = Extended_GCD.extended_gcd a b

check_and_print :: ((Int, Int), (Int, Int, Int)) -> String
check_and_print ((a,b), (x,y,gcd)) = "GCD(" ++ (show a) ++ ", " ++ (show b) ++ ") = " ++ (show gcd) ++ " where " ++ (show a) ++ " * " ++ (show x) ++ " + " ++ (show b) ++ " * " ++ (show y) ++ " = " ++ show (a*x+b*y)

main = do
    let t = 10
    r1 <- sequence $ replicate t $ randomRIO(0, 1000::Int)
    r2 <- sequence $ replicate t $ randomRIO(0, 1000::Int)
    let x = zip r1 r2
    let res = map process x
    let tmp = zip x res
    let out = [ check_and_print i | i <- tmp]
    mapM_ putStrLn out
