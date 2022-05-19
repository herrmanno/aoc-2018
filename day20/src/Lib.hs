{-# LANGUAGE TypeApplications #-}
module Lib where

import Data.List (intercalate, minimumBy)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

type Coord = (Int,Int)

newtype Regex = Regex [R]

instance Show Regex where
    show (Regex xs) = concatMap show xs

data R = Direction D | Option [Regex]

instance Show R where
    show (Direction d) = show d
    show (Option xs) = "(" ++ intercalate "|" (fmap show xs) ++ ")"
data D = N | S | W | E deriving (Show, Read)

newtype VisualMap a = VisualMap (M.Map Coord Char)

instance Show a => Show (VisualMap a) where
    show (VisualMap m) = 
        let minY = minimum . fmap fst . M.keys $ m
            maxY = maximum . fmap fst . M.keys $ m
            minX = minimum . fmap snd . M.keys $ m
            maxX = maximum . fmap snd . M.keys $ m
        in unlines [ [M.findWithDefault '#' (y,x) m | x <- [minX..maxX]] | y <- [minY..maxY]]

run1 :: String -> Int
run1 = findLongestPath . walk . Regex . parse . filter (`elem` "NSEW()|)")

run2 :: String -> Int
run2 = findPathsLongerThan' 999 . walk . Regex . parse . filter (`elem` "NSEW()|)")

findLongestPath :: M.Map Coord Char -> Int
findLongestPath m = pred $ go m (0,0) S.empty
    where
        go m (y,x) visited
            | S.member (y,x) visited = 0
        go m (y,x) visited =
            let neighbours = [(y',x') | y' <- [y-1,y,y+1], x' <- [x-1,x,x+1], (y',x') /= (y,x)]
                doors = filter  ((`elem` "|-") .  flip (M.findWithDefault '#') m) neighbours
                nextCoords = fmap (\(y',x') -> (y + 2 * (y' - y), x + 2 * (x' - x))) doors
                visited' = S.insert (y,x) visited
                maxPathLength = 1 + maximum (fmap (flip (go m) visited') nextCoords)
            in maxPathLength

findShortestPath :: M.Map Coord Char -> Coord -> Maybe Int
findShortestPath m target = pred <$> go m (0,0) S.empty
    where
        go m (y,x) visited
            | S.member (y,x) visited = Nothing
        go m (y,x) visited
            | (y,x) == target = Just 0
        go m (y,x) visited =
            let neighbours = [(y',x') | y' <- [y-1,y,y+1], x' <- [x-1,x,x+1], (y',x') /= (y,x)]
                doors = filter  ((`elem` "|-") .  flip (M.findWithDefault '#') m) neighbours
                nextCoords = fmap (\(y',x') -> (y + 2 * (y' - y), x + 2 * (x' - x))) doors
                visited' = S.insert (y,x) visited
                paths = mapMaybe (flip (go m) visited') nextCoords
            in if null paths
                then Nothing
                else Just . succ . minimum $ paths

findPathsLongerThan' :: Int -> M.Map Coord Char -> Int
findPathsLongerThan' limit m = S.size $ go m limit (0,0) S.empty
    where
        go m _ (y,x) visited
            | S.member (y,x) visited = S.empty
        go m limit (y,x) visited =
            let neighbours = [(y',x') | y' <- [y-1,y,y+1], x' <- [x-1,x,x+1], (y',x') /= (y,x)]
                doors = filter  ((`elem` "|-") .  flip (M.findWithDefault '#') m) neighbours
                nextCoords = fmap (\(y',x') -> (y + 2 * (y' - y), x + 2 * (x' - x))) doors
                visited' = S.insert (y,x) visited
                roomsFound = S.unions . fmap (flip (go m (limit - 1)) visited') $ nextCoords
            in if limit < 0 then S.insert (y,x) roomsFound else roomsFound

walk :: Regex -> M.Map Coord Char
walk (Regex xs) = snd $ go xs (0,0) (M.singleton (0,0) 'X')
    where
        go :: [R] -> Coord -> M.Map Coord Char -> (Coord, M.Map Coord Char)
        go [] coord m = (coord, m)
        go (Direction N:xs) (y,x) m = go xs (y-2,x) (M.insert (y-2,x) '.' . M.insert (y-1,x) '-' $ m)
        go (Direction S:xs) (y,x) m = go xs (y+2,x) (M.insert (y+2,x) '.' . M.insert (y+1,x) '-' $ m)
        go (Direction W:xs) (y,x) m = go xs (y,x-2) (M.insert (y,x-2) '.' . M.insert (y,x-1) '|' $ m)
        go (Direction E:xs) (y,x) m = go xs (y,x+2) (M.insert (y,x+2) '.' . M.insert (y,x+1) '|' $ m)
        go (Option xs:xss) (y,x) m =
            let subResults = (\(Regex xs) -> go xs (y,x) m) <$> xs
                subResultMap = M.fromListWith (M.union) subResults
                endResults = snd . (uncurry $ go xss) <$> M.toList subResultMap 
            in ((0,0), M.unions endResults)

parse :: String -> [R]
parse = fst . head . filter (null . snd) . readP_to_S (many1 parseR)
    where
        parseR = choice [parseDirection, parseOption]
        parseDirection = Direction . (read @D) . (:[]) <$> satisfy (`elem` "NSWE")
        parseOption = Option <$> between (char '(') (char ')') (sepBy1 (Regex <$> multiple parseR) (char '|'))
        multiple p = ((:) <$> p <*> multiple p) <++ ((:[]) <$> p) <++ pure []

