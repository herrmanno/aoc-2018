module Lib where

import Data.List (tails)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Bits ((.&.), (.|.))

type Registers = IM.IntMap Int
type Instruction = (Int,Int,Int,Int)

data OpCode
    = Addr
    | Addi
    | Mulr
    | Muli
    | Banr
    | Bani
    | Borr
    | Bori
    | Setr
    | Seti
    | Gtir
    | Gtri
    | Gtrr
    | Eqir
    | Eqri
    | Eqrr
    deriving (Eq, Ord, Show, Bounded, Enum)

execute :: OpCode -> Int -> Int -> Int -> Registers -> Registers
execute op a1 a2 a3 reg = case op of
    Addr -> set $ get1 + get2
    Addi -> set $ get1 + a2
    Mulr -> set $ get1 * get2
    Muli -> set $ get1 * a2
    Banr -> set $ get1 .&. get2
    Bani -> set $ get1 .&. a2
    Borr -> set $ get1 .|. get2
    Bori -> set $ get1 .|. a2
    Setr -> set get1
    Seti -> set a1
    Gtir -> set $ fromEnum $ a1 > get2
    Gtri -> set $ fromEnum $ get1 > a2
    Gtrr -> set $ fromEnum $ get1 > get2
    Eqir -> set $ fromEnum $ a1 == get2
    Eqri -> set $ fromEnum $ get1 == a2
    Eqrr -> set $ fromEnum $ get1 == get2
    where
        set v = IM.insert a3 v reg
        get1 = reg IM.! a1
        get2 = reg IM.! a2

runProgram :: [Instruction] -> IM.IntMap OpCode -> Registers -> Registers
runProgram [] _ reg = reg
runProgram ((op,a1,a2,a3):xs) opMapping reg =
    let reg' = execute (opMapping IM.! op) a1 a2 a3 reg
    in runProgram xs opMapping reg'


run1 :: String -> Int
run1 input =
    let tests = parseTests input
        tests' = filter ((>=3) . length . matchingOpCodes) tests
    in length tests'

run2 :: String -> Int
run2 input =
    let tests = parseTests input
        program = parseProgram input
        possibleOpMapping = IM.fromListWith S.union .  map (\t -> (op t, S.fromList $ matchingOpCodes t)) $ tests
        opMapping = findOpMapping possibleOpMapping
        defaultRegisters = IM.fromList (zip [0,1,2,3] (repeat 0))
        result = runProgram program opMapping defaultRegisters
    in result IM.! 0

findOpMapping :: IM.IntMap (S.Set OpCode) -> IM.IntMap OpCode
findOpMapping m =
    if IM.null (IM.filter ((>1) . S.size) m)
        then IM.map (maybe undefined fst . S.minView) m
        else
            let singles = S.unions . IM.elems . IM.filter ((==1) . S.size) $ m
                m' = IM.map (\ops -> if S.size ops == 1 then ops else ops `S.difference` singles) m
            in findOpMapping m'

data Test = Test { before :: Registers, after :: Registers, op :: Int, args :: (Int,Int,Int) } deriving (Show)

matchingOpCodes :: Test -> [OpCode]
matchingOpCodes (Test before after op (a1,a2,a3)) = filter testOpCode [(Addr)..(Eqrr)]
    where
        testOpCode op = execute op a1 a2 a3 before == after

parseTests :: String -> [Test]
parseTests s = mapMaybe parseTest (tails (lines s))
    where
        parseTest :: [String] -> Maybe Test
        parseTest (a:b:c:_) = do
            before <- parseRegister a
            [op,a1,a2,a3] <- mapM readMaybe (words b)
            after <- parseRegister c
            pure $ Test before after op (a1,a2,a3)
            where
                parseRegister xs = if length (words xs) > 1
                    then IM.fromList . zip [0..] <$> readMaybe (unwords . tail . words $ xs)
                    else Nothing
        parseTest _ = Nothing

parseProgram :: String -> [(Int,Int,Int,Int)]
parseProgram s =
    let programLines = reverse . takeWhile (/="") . reverse . lines $ s
    in mapMaybe parseLine programLines
    where
        parseLine s = do
            [op,a1,a2,a3] <- mapM readMaybe (words s)
            pure (op,a1,a2,a3)
