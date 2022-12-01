{-# LANGUAGE TupleSections, NamedFieldPuns #-}
module Lib where

import Data.Char (isDigit, isAlpha)
import Data.List (maximumBy, sortBy, find)
import Data.Ord (comparing)
import qualified Data.Map as M
import Data.Bifunctor (first)
import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP

data GroupType = Immune | Infection deriving (Eq, Ord, Show)

data Group =
    Group
        { groupType :: GroupType
        , index :: Int
        , units :: Int
        , hp :: Int
        , weak :: [String]
        , immune :: [String]
        , attack :: (Int, String)
        , initiative :: Int
        }

instance Eq Group where
    a == b = groupType a == groupType b && index a == index b

instance Ord Group where
    compare a b = compare (groupType a) (groupType b) <> compare (index a) (index b)

instance Show Group where
    show Group{groupType,index,units} = show groupType <> " " <> show index <> "(" <> show units <> ")"

boostBy :: Int -> Group -> Group
boostBy i g = g { attack = first (+i) (attack g) }

attackType :: Group -> String
attackType = snd . attack

attackStrength :: Group -> Int
attackStrength = fst . attack

effectivePower :: Group -> Int
effectivePower Group {units,hp,attack} = units * fst attack

effectiveDamage :: Group -> Group -> Int
effectiveDamage g g'
    | attackType g `elem` immune g' = 0
    | attackType g `elem` weak g' = effectivePower g * 2
    | otherwise = effectivePower g

run1 :: String -> Int
run1 s =
    let (g1,g2) = parseInput s
        gs = fight g1 g2
    in sum $ fmap units gs

run2 :: String -> Int
run2 s =
    let (g1,g2) = parseInput s
        gs = go g1 g2 1
    in sum $ fmap units gs
    where
        go g1 g2 n =
            let g1' = fmap (boostBy n) g1
                gs = fight g1' g2
            in if not (null gs) && all ((==Immune) . groupType) gs
                then gs
                else go g1 g2 (n + 1)

-- | fight until no more changes possible
fight :: [Group] -> [Group] -> [Group]
fight [] g2 = g2
fight g1 [] = g1
fight g1 g2 =
    let p1 = selectTargets g1 g2
        p2 = selectTargets g2 g1
        attackMapping =  M.fromList (p1 ++ p2)
        (g',damage) = attackTargets attackMapping (g1 ++ g2)
        g1' = filter ((==Immune) . groupType) g'
        g2' = filter ((==Infection) . groupType) g'
    in if damage == 0 then g1 ++ g2 else fight g1' g2'
    where
        showUnit g = show g <> " " <> show (units g)
    

-- | let all groups attack and return resulting groups and sumed lost units
attackTargets :: M.Map Group Group -> [Group] -> ([Group], Int)
attackTargets m gs =
    let groupsSorted = sortBy (flip (comparing initiative)) gs
    in go m groupsSorted gs 0
    where
        go _ [] ys d = (ys,d)
        go m (x:xs) ys d =
            let choosenTarget = m M.!? x
                maybeTarget = find ((==choosenTarget) . Just) ys
            in case maybeTarget of
                    Just target ->
                        let damage = effectiveDamage x target
                            lostUnits = damage `div` hp target
                        in if units target > lostUnits
                            then
                                let target' = target { units = units target - lostUnits }
                                    ys' = target' : filter (/=target) ys
                                    xs' = map (\x -> if x == target then target' else x) xs
                                in go m xs' ys' (d + lostUnits)
                            else
                                let ys' = filter (/=target) ys
                                    xs' = filter (/=target) xs
                                in go m xs' ys' (d + lostUnits)
                    Nothing -> go m xs ys d

selectTargets :: [Group] -> [Group] -> [(Group,Group)]
selectTargets attackers defenders =
    let attackersSorted =
            sortBy
                (\a b ->
                       comparing effectivePower a b
                    <> comparing initiative a b)
                attackers
        matchTargets attacker (pairings, defendersLeft) =
            case selectTarget attacker (filter ((/=groupType attacker) . groupType) defendersLeft) of
                Just defender -> ((attacker,defender):pairings, filter (/=defender) defendersLeft)
                Nothing -> (pairings, defendersLeft)
    in fst $ foldr matchTargets ([], defenders) attackersSorted

selectTarget :: Group -> [Group] -> Maybe Group
selectTarget _ [] = Nothing
selectTarget g gs =
    let choosenTarget =
            maximumBy
                (\a b ->
                        comparing (effectiveDamage g) a b
                    <> comparing effectivePower a b
                    <> comparing initiative a b)
                gs
    in if effectiveDamage g choosenTarget == 0 then Nothing else Just choosenTarget

parseInput :: String -> ([Group],[Group])
parseInput s =
    let (_:g1,_:_:g2) = break null (lines s)
    in (fmap (doParseGroup Immune) (zip [1..] g1), fmap (doParseGroup Infection) (zip[1..] g2))
    where
        doParseGroup gt (idx,s) = fst . head . readP_to_S (parseGroup gt idx) $ s

parseGroup gt idx = do
    units <- number
    string " units each with "
    hp <- number
    string " hit points "
    (weak,immune) <- parseWeakImmune
    optional space
    string "with an attack that does "
    attackStrength <- number
    space
    attackType <- munch1 (/=' ')
    string " damage at initiative "
    initiative <- number
    optional space
    return (Group gt idx units hp weak immune (attackStrength, attackType) initiative)


parseWeakImmune = option ([],[]) $ between (char '(') (char ')') go
    where
        go = choice
                [ (,) <$> parseWeak <*> (separator *> parseImmune)
                , flip (,) <$> parseImmune <*> (separator *> parseWeak)
                , (,[]) <$> parseWeak
                , ([],) <$> parseImmune
                ]
        parseWeak = string "weak to " *> sepBy (munch1 isAlpha) (string ", ")
        parseImmune = string "immune to " *> sepBy (munch1 isAlpha) (string ", ")
        separator = string "; "

number = read <$> munch1 isDigit
space = satisfy (==' ')
