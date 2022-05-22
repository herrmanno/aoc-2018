module Lib where

import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

type Depth = Int
type ErosionLevel = Int
type Coord = (Int,Int)

(a,b) ./ (c,d) = abs (a - c) + abs (b - d)

data Region = Rocky | Wet | Narrow

instance Show Region where
    show Rocky = "."
    show Wet = "="
    show Narrow = "|"

risk :: Region -> Int
risk Rocky = 0
risk Wet = 1
risk Narrow = 2

regionType :: Int -> Region
regionType i = case i `mod` 3 of
    0 -> Rocky
    1 -> Wet
    2 -> Narrow
    _ -> undefined

allowedTools :: Region -> S.Set Tool
allowedTools Rocky = S.fromList [Torch, Gear]
allowedTools Wet = S.fromList [Neither, Gear]
allowedTools Narrow = S.fromList [Neither, Torch]

data Tool = Torch | Gear | Neither deriving (Eq, Ord, Show)

showMap s =
    let (depth,target@(x,y)) = parseInput s
        cave = M.map regionType $ buildCaveMap depth target -- (2*x,2*y)
    in unlines [ [head . show . (M.!) cave $ (x',y') | x' <- [0..2*x] ] | y' <- [0..2*y] ]

run1 :: String -> Int
run1 s =
    let (depth,target) = parseInput s
        cave = buildCaveMap depth target
        risks = risk . regionType . (M.!) cave <$> [(x,y) | x <- [0..(fst target)], y <- [0..(snd target)]]
    in sum risks

run2 :: String -> Int
run2 s =
    let (depth,target) = parseInput s
        cave = M.map regionType $ buildCaveMap depth target
    in case shortestPath cave target Torch of
        Just p -> p
        _ -> error "No shortest path found"

shortestPath :: M.Map Coord Region -> Coord -> Tool -> Maybe Int
shortestPath cave target tool = go (singleton (0, (0,0) ./ target) (tool, (0,0))) S.empty
    where
        go :: Heap (Int, Int) (Tool, Coord) -> S.Set (Coord, Tool) -> Maybe Int
        go heap visited = do
            ((cost, _), (tool, (x,y)), heap') <- get heap
            let neighbours =
                    [ (x',y')
                    | (x',y') <- [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
                    , x' >= 0
                    , y' >= 0
                    , M.member (x',y') cave
                    ]
                visited' = S.insert ((x,y), tool) visited
                routes = goRoute =<< neighbours
                goRoute :: Coord -> [(Int, Tool, Coord)]
                goRoute (x',y') = case (cave M.! (x,y), cave M.! (x',y')) of
                    (current,next) ->
                        let possibleTools = allowedTools current `S.intersection` allowedTools next
                        in
                            fmap
                                (\t ->
                                    if tool == t
                                        then (cost + 1, tool, (x',y'))
                                        else (cost + 7, t, (x,y)))
                                (S.toList possibleTools)
                heap'' = foldr
                           (\(cost, tool, coord) h ->
                                insert (cost, coord ./ target) (tool,coord) h)
                           heap'
                           routes
            if (x,y) == target
                then if tool == Torch
                    then pure cost
                    else go (insert (cost + 7, 0) (Torch, (x,y)) heap') visited
                else if S.member ((x,y),tool) visited
                    then go heap' visited
                    else go heap'' visited'

buildCaveMap :: Depth -> Coord -> M.Map Coord ErosionLevel
buildCaveMap depth target = memo
    where
        memo = M.fromList (zip coords (fmap e coords))
        coords = [(x',y') | x' <- [0..maxX], y' <- [0..maxY]]
        maxX = 200 + fst target
        maxY = 200 + snd target
        e (x,y) = (g (x,y) + depth) `mod` 20183
        g (0,0) = 0
        g t | t == target = 0
        g (0,y) = y * 48271
        g (x,0) = x * 16807
        g (x,y) = memo M.! (x-1,y) * memo M.! (x,y-1)

parseInput :: String -> (Depth, Coord)
parseInput s =
    let (x1:x2:_) = lines s
        depth = read . (!!1) . words $ x1
        target = read . (\s -> "(" ++ s ++ ")") . (!!1) . words $ x2
    in (depth, target)


newtype Heap k a = Heap (M.Map k [a])

instance (Ord k, Show k, Show a) => Show (Heap k a) where
    show (Heap m) = intercalate "\n" [show k <> ": " <> show (m M.! k) | k <- M.keys m]

singleton :: Ord k => k -> a -> Heap k a
singleton k = Heap . M.singleton k . (:[])

insert :: Ord k => k -> a -> Heap k a -> Heap k a
insert k a (Heap m) = Heap $ M.alter (Just . maybe [a] (a:)) k m

get :: Ord k => Heap k a -> Maybe (k, a, Heap k a)
get (Heap m) = do
    (k, a:as) <- M.lookupMin m
    let m' = if null as then M.delete k m else M.adjust tail k m
    pure (k, a, Heap m')
