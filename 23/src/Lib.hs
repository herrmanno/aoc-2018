module Lib where

import Data.Ord (comparing)
import Data.List (tails, unfoldr, maximumBy)
import Data.Maybe (listToMaybe)
import qualified Data.Set as S

type Coord3 = (Int,Int,Int)

diff :: Coord3 -> Coord3 -> Int
diff (a,b,c) (a',b',c') = abs (a - a') + abs (b - b') + abs (c - c')

overlap :: Nanobot -> Nanobot -> Bool
overlap (c,r) (c',r') = diff c c' <= (r + r')

--contains :: Coord3 -> Nanobot -> Bool
--contains (a,b,c) ((a',b',c'),r) = diff (a,b,c) (a',b',c') <= r

type Range = Int
type Nanobot = (Coord3, Range)

distanceRange :: Nanobot -> (Int,Int)
distanceRange ((a,b,c),r) = let s = a + b + c in (s-r,s+r)

rangeIntersection :: (Int,Int) -> (Int,Int) -> (Int,Int)
rangeIntersection (low,hi) (low',hi') = (max (min hi low') (min hi' low), min (max hi low') (max hi' low))

run1 :: String -> Int
run1 s =
    let xs = parseInput s
        (maxCoord, maxRange) = maximumBy (comparing snd) xs
        inRange = filter ((<=maxRange) . diff maxCoord . fst) xs
    in length inRange

{-
    Solution based on https://www.reddit.com/r/adventofcode/comments/aa9uvg/comment/ecw0d91/?utm_source=share&utm_medium=web2x&context=3

    Would be nice to find a solution on my own some time...
-}
run2 s =
    let xs = parseInput s
        sets =  findLargestSet =<< tails xs
        (y:ys) = S.toList $ maximumBy (comparing S.size) sets
    in fst $ foldr rangeIntersection (distanceRange y) (fmap distanceRange ys)
    where
        findLargestSet :: [Nanobot] -> [S.Set Nanobot]
        findLargestSet [] = [S.empty]
        findLargestSet [x] = [S.singleton x]
        findLargestSet (x:xs) =
            let sets = findLargestSet xs
                sets' = filter (all (overlap x) . S.toList) sets
            in S.singleton x : fmap (S.insert x) sets'


parseInput :: String -> [Nanobot]
parseInput = fmap parse . lines
    where
        parse s =
            let [a,b,c,d] = readAllNums s
            in ((a,b,c),d)


readAllNums :: (Read a, Num a) => String -> [a]
readAllNums = unfoldr (listToMaybe . concatMap reads . tails)

