module Lib where

import qualified Data.Map as M
import qualified Data.Set as S

type P4 = (Int,Int,Int,Int)

(a,b,c,d) ./. (a',b',c',d') = abs (a - a') + abs (b - b') + abs (c - c') + abs (d - d')

run1 = length . buildConstellations . buildConnectionsMap . parseInput 

-- | Build transitive hull over graph of connected points
buildConstellations :: M.Map P4 [P4] -> [S.Set P4]
buildConstellations m = fst $ go m (M.keys m) S.empty
    where
        go :: M.Map P4 [P4] -> [P4] -> S.Set P4 -> ([S.Set P4], S.Set P4)
        go _ [] seen = ([], seen)
        go m (x:xs) seen
            | S.member x seen = go m xs seen
            | otherwise =
                let seen' = S.insert x seen
                    connected = m M.! x
                    (transitives, seen'') =
                        foldr
                            (\node (list,tabu) ->
                                let (new,tabu') = go m [node] tabu
                                in (new:list, S.union tabu tabu'))
                            ([],seen')
                            connected
                    children = S.unions (concat transitives)
                in (S.insert x children : fst (go m xs seen''), seen'')

-- | create map from point -> points it is connected to
buildConnectionsMap :: [P4] -> M.Map P4 [P4]
buildConnectionsMap xs = M.fromList [(x, findConnections x xs) | x <- xs]

-- | find all points in ys that x is connected to
findConnections :: P4 -> [P4] -> [P4]
findConnections x [] = []
findConnections x (y:ys)
    | x == y = findConnections x ys
findConnections x (y:ys) =
    if x ./. y <= 3
        then y : findConnections x ys
        else findConnections x ys

parseInput :: String -> [P4]
parseInput = fmap (toP4 . parse) . lines
    where
        parse s = read ("[" <> s <> "]")
        toP4 [a,b,c,d] = (a,b,c,d)
