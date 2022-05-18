{-# LANGUAGE TupleSections #-}
module Lib where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative ((<|>))
import Debug.Trace

data Tile = Sand | Clay | Water1 | Water2 | Spring deriving (Eq, Show)

isWater Water1 = True
isWater Water2 = True
isWater _ = False

toChar Sand = '.'
toChar Clay = '#'
toChar Water1 = '|'
toChar Water2 = '~'
toChar Spring = '+'

type Coord = (Int,Int)
type Map = M.Map Coord Tile

showMap :: Map -> String
showMap m =
    let m' = M.insert (0,500) Spring m
        minX = minimum . map snd . M.keys $ m'
        maxX = maximum . map snd . M.keys $ m'
        minY = minimum . map fst . M.keys $ m'
        maxY = maximum . map fst . M.keys $ m'
        getChar y x = maybe '.' toChar (m' M.!? (y,x))
    in
        unlines [ [getChar y x | x <- [minX..maxX]] | y <- [minY..maxY] ]

run :: String -> (Map, Int, Int)
run s =
    let m' = go 0 terrainMap inputMap
    in (m', countWater m', countWater2 m')
    where
        inputMap = parseInput s
        terrains = findTerrains inputMap
        terrainMap = buildTerrainMap inputMap terrains
        go n ts m = case tick (1,500) ts m of
            Just m' -> go (n + 1) ts m'
            Nothing -> m
        countWater = M.size . M.filter isWater . M.filterWithKey (\(y,_) _ -> y >= minY && y <= maxY)
        countWater2 = M.size . M.filter (==Water2) . M.filterWithKey (\(y,_) _ -> y >= minY && y <= maxY)
        minY = minimum . map fst . M.keys $ inputMap
        maxY = maximum . map fst . M.keys $ inputMap

-- tries to add another flowing water at (y,x). Returns Nothing if it can not be added.
tick :: Coord -> S.Set Coord -> Map -> Maybe Map
tick (y,x) ts m = -- trace (showMap m) $
    let maxY = succ . maximum . map fst . M.keys $ m
    in
    case M.lookup (y,x) m of
        Just _ -> Nothing
        _ -> move maxY Water1 (y,x) ts m

move :: Int -> Tile -> Coord -> S.Set Coord -> Map -> Maybe Map
move yLimit what (y,x) _ m
    | y + 1 == yLimit = Just . M.insert (y,x) what $ m
move yLimit what (y,x) ts m =
    let below = M.findWithDefault Sand (y+1,x) m
    in case below of
        Sand -> move yLimit what (y+1,x) ts m
        Water1 -> Just . M.insert (y,x) Water1 $ m
        below | below == Clay || below == Water2 ->
            if insideTerrain (y,x) ts m
                then moveLeft yLimit Water2 (y,x) ts m <|> moveRight yLimit Water2 (y,x) ts m <|> Just (M.insert (y,x) Water2 m)
                else moveLeft yLimit Water1 (y,x) ts m <|> moveRight yLimit Water1 (y,x) ts m <|> Just (M.insert (y,x) Water1 m)
        below -> error $ "Bad tile at " <> show (y,x) <> ": " <> show below

insideTerrain (y,x) ts m = S.member (y,x) ts || bordersLeftAndRight (y,x) m

-- Checks if below (y,x) is Water2 and if so, if above the borders, that hold that Water2
-- are again border, which may hold a Water2 at (y,x)
bordersLeftAndRight (y,x) m =
    case m M.!? (y+1,x) of
        (Just Water2) ->
            let sideBorder f (y,x) =
                    let go (y,x) skipWater =
                            case m M.!? (y,x) of
                                (Just Water2) | skipWater -> go (y,f x) skipWater
                                (Just Clay) -> go (y,f x) False <|> Just x
                                _ -> Nothing
                    in fromMaybe x (go (y+1,x) True)
                leftX = sideBorder pred (y+1,x)
                rightX = sideBorder succ (y+1,x)
                in m M.!? (y,leftX) == Just Clay && m M.!? (y,rightX) == Just Clay
        _ -> False

moveLeft = moveToSide pred

moveRight = moveToSide succ

moveToSide f yLimit what (y,x) ts m =
    let below = M.findWithDefault Sand (y+1,x) m
    in
        case below of
            Sand -> move yLimit what (y+1,x) ts m
            Water1 -> Nothing
            _ -> case M.findWithDefault Sand (y,f x) m of
                    Sand -> moveToSide f yLimit what (y,f x) ts m <|> Just (M.insert (y,f x) what m)
                    _ -> Nothing

newtype Terrain = Terrain ((Int,Int),(Int,Int)) deriving (Show)

isEmpty :: Terrain -> Bool
isEmpty (Terrain ((y,x),(y',x'))) = x == x' || y == y'

contains :: (Int,Int) -> Terrain -> Bool
contains (a,b) (Terrain ((y,x),(y',x'))) = a < y && y' <= a && x < b && b < x'

buildTerrainMap :: Map -> [Terrain] -> S.Set Coord
buildTerrainMap m ts =
    let minX = minimum . map snd . M.keys $ m
        maxX = maximum . map snd . M.keys $ m
        minY = minimum . map fst . M.keys $ m
        maxY = maximum . map fst . M.keys $ m
    in S.fromList [(y,x) | y <- [minY..maxY], x <- [minX..maxX], any (contains (y,x)) ts]

findTerrains :: Map -> [Terrain]
findTerrains m =
    let minX = minimum . map snd . M.keys $ m
        maxX = maximum . map snd . M.keys $ m
        minY = minimum . map fst . M.keys $ m
        maxY = maximum . map fst . M.keys $ m
    in
        [ t
        | y <- [maxY,(maxY-1)..minY]
        , x <- [minX..maxX]
        , m M.!? (y,x) == Just Clay
        , let t = getTerrain (y,x) m
        , not (isEmpty t)
        ]
    where
        getTerrain (leftBottomY@y,x) m =
            let leftTopY =
                    let go (y,x) =
                            case m M.!? (y,x) of
                                (Just Clay) -> go (y-1,x) <|> Just y
                                _ -> Nothing
                    in fromMaybe y (go (y,x))
                rightBottomX =
                    let go (y,x) =
                            case m M.!? (y,x) of
                                (Just Clay) -> go (y,x+1) <|> Just x
                                _ -> Nothing
                    in fromMaybe x (go (y,x))
                rightTopY =
                    let go (y,x) =
                            case m M.!? (y,x) of
                                (Just Clay) -> go (y-1,x) <|> Just y
                                _ -> Nothing
                    in fromMaybe y (go (y,rightBottomX))
            in Terrain ((y,x),(max leftTopY rightTopY, rightBottomX))


parseInput :: String -> Map
parseInput s =
    let coords = (lines s >>= parse parseCoords)
        m = M.fromList . (`zip` repeat Clay) $ coords
    in m
    where
        parseCoords = parseCoords1 <++ parseCoords2
        parseCoords1 = do
            string "x="
            x <- number
            string ", y="
            y1 <- number
            string ".."
            y2 <- number
            return $ fmap (,x) [y1..y2]
        parseCoords2 = do
            string "y="
            y <- number
            string ", x="
            x1 <- number
            string ".."
            x2 <- number
            return $ fmap (y,) [x1..x2]
        number = read <$> munch1 isDigit
        parse p s = case readP_to_S p s of
            [(a,_)] -> a
            [] -> error "No parse"
            xs -> error $ "Bad parse: " <> show xs

