module Lib where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Debug.Trace

type Coord = (Int,Int)
type Map = M.Map Coord Char

showMap :: Map -> String
showMap m =
    let maxY = maximum . fmap fst . M.keys $ m
        maxX = maximum . fmap snd . M.keys $ m
    in unlines [ [m M.! (y,x) | x <- [0..maxX] ] | y <- [0..maxY] ]

hashMap :: Map -> Integer
hashMap m =
    let maxY = maximum . fmap fst . M.keys $ m
        maxX = maximum . fmap snd . M.keys $ m
        h '.' = 0
        h '|' = 1
        h '#' = 2
        h c = error $ "Bad char: " <> show c
        f i acc = acc * 4 + i
    in foldr f (0 :: Integer) [h (m M.! (y,x)) | x <- [0..maxX], y <- [0..maxY]]

run1 :: String -> Int
run1 = run 10

run2 :: String -> Int
run2 = run 1000000000 

run :: Int -> String -> Int
run n = count . evolve n . parse
    where
        count m = M.size (M.filter (=='|') m) * M.size (M.filter (=='#') m)

evolve :: Int -> Map -> Map
evolve limit m = go 0 M.empty m
    where
        go n visited m
            | n == limit = m
        go n visited m =
            let m' = M.mapWithKey (f m) m
                h = hashMap m'
            in if m == m'
                then m
                else 
                    case M.lookup h visited of
                        Just roundSeen -> 
                            let cycleLength = n - roundSeen
                                t = (limit - n ) `div` cycleLength
                                nextRound = 1 + n + t * cycleLength
                            in if nextRound > n
                                    then trace ("At round " <> show n <> ": already seen this state in round: " <> show roundSeen <>". Skipping to round " <> show nextRound)
                                        go nextRound M.empty m'
                                    else go (n + 1) (M.insert h n visited) m'
                        _ ->
                            go (n + 1) (M.insert h n visited) m'
        f m (y,x) c = 
            let nm = getNeighbourMap (y,x) m
            in case c of
                '.' -> if nm M.! '|' >= 3 then '|' else '.'
                '|' -> if nm M.! '#' >= 3 then '#' else '|'
                '#' -> if nm M.! '#' >= 1 && nm M.! '|' >= 1 then '#' else '.'
                c -> error $ "Got bad char: " <> show c

getNeighbourMap :: Coord -> Map -> M.Map Char Int
getNeighbourMap (y,x) m =
    M.unionWith (+) defaultMap $ M.fromListWith (+) (zip (mapMaybe (`M.lookup` m) neighbours) (repeat 1))
    where
        neighbours = [(y',x') | y' <- [y-1,y,y+1], x' <- [x-1,x,x+1], (y,x) /= (y',x')] 
        defaultMap = M.fromList [('.', 0), ('|', 0), ('#', 0)]

parse :: String -> Map
parse s = M.fromList [((y,x),c) | (y,line) <- zip [0..] (lines s), (x,c) <- zip [0..] line ]
