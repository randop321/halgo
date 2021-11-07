{-
Test a Treap with three basic operators: insert, delete and lower_bound.
Duplicated elements are removed automatically.
Input:
N: number of operators
Next there are N lines, each with two integers c, k as
1 k: insert an element k
2 k: delete an element k
3 k: print the lower bound of element k, or "None"
-}


{-# Options_GHC -O2 #-}
import System.Random
import Data.Char
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.Bits
import Treap

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine


getres :: Treap.Tree -> String
getres t = if Treap.isEmptyTree t then "None" else show (Treap.getValue t)

process :: (Treap.Tree, [String]) -> (Int, (Int, Int)) -> (Treap.Tree, [String])
process (t, res) (cmd, (v, p))
    | cmd == 1 = (insert (v, p) t, res)
    | cmd == 2 = (delete v t, res)
    | cmd == 3 = (t, (getres (lower_bound v t)) : res)


main = do
    t_ <- getLine
    let t = (read t_ :: Int)
    
    -- generate random numbers    
    rs <- sequence $ replicate t $ randomRIO(0, 1000000::Int)
    
    a <- sequence $ replicate t $ getIntList
    let b0 = [head i | i <- a]
    let b = [last i | i <- a]
    let d = zip b rs
    let e = zip b0 d
    
    let (_,f) = foldl process (Treap.newEmptyTree, []) e
    let g = reverse f
    mapM_ putStrLn g
