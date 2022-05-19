module Lib (run1, run2, buildMap) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (><))
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, listToMaybe)
import Control.Arrow ((&&&))
import Data.Foldable (find)
import Debug.Trace

type Coord = (Int,Int)
data Map = Map Coord (M.Map Coord Tile)

instance Show Map where
    show (Map (height,width) m) = unlines [[maybe '.' toChar $ M.lookup (y,x) m | x <- [0..width-1]] ++ " " ++ showHPs y (Map (height,width) m) | y <- [0..height-1]]

showHPs y (Map (_,width) m) = unwords . mapMaybe showHP $ catMaybes [tile | x <- [0..width-1], let tile = M.lookup (y,x) m]
    where
        showHP = fmap show <$> getHP

getHP (Unit _ hp) = Just hp
getHP _ = Nothing

neighbours :: Coord -> [Coord]
neighbours (y,x) = [(y-1,x), (y,x-1), (y,x+1), (y+1,x)]

data Tile = Wall | Unit UnitType Int deriving (Eq, Show)
data UnitType = Gnome | Elf deriving (Eq, Show)

fromChar '#' = Just Wall
fromChar 'G' = Just (Unit Gnome 200)
fromChar 'E' = Just (Unit Elf 200)
fromChar _ = Nothing

toChar Wall = '#'
toChar (Unit Gnome _) = 'G'
toChar (Unit Elf _) = 'E'

isUnit (Unit _ _) = True
isUnit _ = False

isElf (Unit Elf _) = True
isElf _ = False

isGnome (Unit Gnome _) = True
isGnome _ = False

run1 :: String -> Int
run1 input =
    let (i, Map _ m') = go 0 (buildMap input)
    in i * M.foldr f 0 m'
    where
        go i m =
            let (m', completeRound) = (traceShow i $ doRound 3 m)
                bonus = if completeRound then 1 else 0
            in if ends m' then (i + bonus,m') else go (i + 1) m'
        f (Unit _ hp) acc = hp + acc
        f _ acc = acc

run2 :: String -> Int
run2 input =
    let (i, Map _ m') = go 0 4 originalMap
    in i * M.foldr f 0 m'
    where
        originalMap = buildMap input
        (Map _ originalInnerMap) = originalMap
        originalElfCount = elfCount originalInnerMap
        go i elfAttack m =
            let (m', completeRound) = (traceShow i $ doRound elfAttack m)
                bonus = if completeRound then 1 else 0
                (Map _ innerMap) = m'
                ec = elfCount innerMap
                gc = gnomeCount innerMap
            in if ec < originalElfCount
                then go 0 (elfAttack + 1) originalMap
                else if 0 == gc
                        then traceShow (elfAttack, i + bonus) (i + bonus,m')
                        else go (i + 1) elfAttack m'
        f (Unit _ hp) acc = hp + acc
        f _ acc = acc

ends (Map _ m) = gnomeCount m == 0 || elfCount m == 0
gnomeCount m = M.size $ M.filter isGnome m
elfCount m = M.size $ M.filter isElf m

buildMap :: String -> Map
buildMap input = Map (height, width) (M.fromList m')
    where
        input' = map (takeWhile (/=' ')) (lines input)
        height = length input'
        width = length (head input')
        m = [ ((r,c), fromChar x) | (r,row) <- zip [0..] input', (c,x) <- zip [0..] row]
        m' = mapMaybe f m
        f (coords, Just x) = Just (coords, x)
        f _ = Nothing

doRound :: Int -> Map -> (Map, Bool)
doRound elfAttack m = traceShow m $ go m (0,0) S.empty
    where
        go m lastCoord forbiddenCoords =
            case getNextUnit lastCoord m of
                Nothing -> (m, True)
                Just coord ->
                    if coord `S.member` forbiddenCoords
                        then go m coord forbiddenCoords
                        else if ends m
                                then (m, False)
                                else
                                    let (m', coord') = doUnit elfAttack coord m
                                    in go m' coord (S.insert coord' forbiddenCoords)

doUnit :: Int -> Coord -> Map -> (Map, Coord)
doUnit elfAttack coord m = case canAttack coord m of
    Just (target, _) -> (attackUnitAt elfAttack target m, coord)
    Nothing -> case move coord m of
        Just (m', coord') -> case canAttack coord' m' of
            Just (target, _) -> (attackUnitAt elfAttack target m', coord')
            Nothing -> (m', coord')
        Nothing -> (m, coord)

canAttack :: Coord -> Map -> Maybe (Coord, Tile)
canAttack coord (Map _ m) =
    let possibleTargets =
            case m M.! coord of
                Unit Gnome _ -> filter (isElf . snd) (mapMaybe findUnit $ neighbours coord)
                Unit Elf _ -> filter (isGnome . snd) (mapMaybe findUnit $ neighbours coord)
                u -> error $ "Unit " ++ show u ++ " cannot attack!"
        lowestHP = minimum . mapMaybe (getHP . snd) $ possibleTargets
        lowestTargets = filter ((==lowestHP) . fromMaybe 201 . getHP . snd) possibleTargets
    in if null possibleTargets then Nothing else listToMaybe lowestTargets
    where
        findUnit c =
            case M.lookup c m of
                Just unit@(Unit _ _) -> Just (c, unit)
                _ -> Nothing

attackUnitAt :: Int-> Coord -> Map -> Map
attackUnitAt elfAttack target (Map size m) = Map size . M.alter f target $ m
    where
        f (Just (Unit Gnome hp)) = if hp > elfAttack then Just (Unit Gnome (hp - elfAttack)) else Nothing
        f (Just (Unit Elf hp)) = if hp > 3 then Just (Unit Elf (hp - 3)) else Nothing
        f x = error $ "Cannot attack at target " ++ show x

move :: Coord -> Map -> Maybe (Map, Coord)
move coord (Map size m) = do
    let targetType =
            case m M.! coord of
                (Unit Gnome _) -> Elf
                (Unit Elf _) -> Gnome
                x -> error $ "Cannot move tile " ++ show x
    targetCoord <- findTarget' targetType coord (Map size m)
    let m' = M.delete coord . M.insert targetCoord (m M.! coord) $ m
    return (Map size m', targetCoord)

findTarget' :: UnitType -> Coord -> Map -> Maybe Coord
findTarget' targetType start (Map size@(height,width) m) =
    let adjacentSquares = Seq.fromList . fmap (:[]) . neighbours $ start
        targetSquarePaths = findTargetSquares m adjacentSquares S.empty
        minLength = minimum . fmap length $ targetSquarePaths
        minTargetSquarePaths = filter ((==minLength) . length) targetSquarePaths
    in if null minTargetSquarePaths
            then Nothing
            else
                let minTargetSquare = minimum (fmap head minTargetSquarePaths)
                    pathToMinTargetSquare = findShortestPathTo m minTargetSquare adjacentSquares S.empty
                in fmap last pathToMinTargetSquare
    where
        findTargetSquares :: M.Map Coord Tile -> Seq [Coord] -> S.Set Coord -> [[Coord]]
        findTargetSquares m xs visited = if Seq.null xs
            then []
            else
                let ((coord:path) :<| xs') = xs
                in
                    if S.member coord visited
                        then findTargetSquares m xs' visited
                        else
                            let visited' = S.insert coord visited
                            in case m M.!? coord of
                                Just (Unit t _) | t == targetType ->
                                    path : findTargetSquares m xs' visited'
                                Nothing | validCoord m coord ->
                                    let newPaths = ((:(coord:path)) <$> neighbours coord)
                                    in findTargetSquares m (xs' >< Seq.fromList newPaths) visited'
                                _ -> findTargetSquares m xs' visited'
        
        findShortestPathTo :: M.Map Coord Tile -> Coord -> Seq [Coord] -> S.Set Coord -> Maybe [Coord] -- [[Coord]]
        findShortestPathTo m target xs visited = if Seq.null xs
            then Nothing -- []
            else
                let ((coord:path) :<| xs') = xs
                in
                    if S.member coord visited
                        then findShortestPathTo m target xs' visited
                        else
                            let visited' = S.insert coord visited
                            in if coord == target
                                -- then (coord:path) : findShortestPathTo m target xs' visited'
                                then Just (coord:path)
                                else if validCoord m coord
                                    then
                                        let newPaths = ((:(coord:path)) <$> neighbours coord)
                                        in findShortestPathTo m target (xs' >< Seq.fromList newPaths) visited'
                                    else findShortestPathTo m target xs' visited'
        validCoord m (y,x) = (y,x) `M.notMember` m && y >= 0 && x >= 0 && y < height && x < width


getNextUnit :: Coord -> Map -> Maybe Coord
getNextUnit coords (Map size m) = do
    ((coords',unit), m') <- M.minViewWithKey m
    if coords' > coords && isUnit unit
        then return coords'
        else getNextUnit coords (Map size m')
