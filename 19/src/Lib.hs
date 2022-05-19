{-# LANGUAGE TypeApplications #-}
module Lib where

import Data.Char (toUpper)
import Data.List (tails)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Bits ((.&.), (.|.))
import qualified Data.Vector.Unboxed as VU

import Debug.Trace
import Text.Printf

type Registers = IM.IntMap Int

(.!) :: Registers -> Int -> Int
(.!) reg i = reg IM.! i

(.~) :: Registers -> (Int,Int) -> Registers
(.~) reg (i,v) = IM.insert i v reg

type Instruction = (Int,Int,Int,Int)

showInstr :: Instruction -> String
showInstr (op,a1,a2,a3) = show ((toEnum @OpCode) op, a1, a2, a3)

type Instructions = VU.Vector Instruction

fromList :: [Instruction] -> Instructions
fromList xs = VU.generate (length xs) (xs!!)

--(.!!) :: Instructions -> Int -> Maybe Instruction
--(.!!) xs i
    -- | i < 0 || i >= length xs = Nothing
    -- | otherwise = Just . (!!i) $ xs

(.!!) :: Instructions -> Int -> Maybe Instruction
(.!!) xs i = xs VU.!? i

data OpCode
    = Noop
    | Addr
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
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

execute :: Instruction -> Registers -> Registers
execute (op,a1,a2,a3) reg = case toEnum op of
    Noop -> reg
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

runProgram :: Int -> Instructions -> Registers -> Registers
runProgram ipReg xs reg =
    let ip = reg .! ipReg
        instr = loadInstr ip
    in maybe reg (`go` reg) instr
    where
        loadInstr ip = xs .!! ip
        go :: Instruction -> Registers -> Registers
        go _ reg
            {-  This branch exploits knowledge of the given program.
                We know that when we enter the inner loop (ip=3) for the first time
                some register is filled with a value `i` and we know that when the program
                halts register `0` will be filled with the sum of all divisors of `i`.
                Also we know that no register will hold a value greater than `i` at this point.
            -}
            | reg .! ipReg == 3 =
                let value = maximum (fmap (reg .!) [0..5])
                    result = sum $ filter ((==0) . mod value) [1..value]
                in reg .~ (0,result)
        -- this is the common execution of the program without further knowledge of the specific program
        go instr reg =
            let ip = reg .! ipReg
                reg' = execute instr reg
                ip' = 1 + (reg' .! ipReg)
                reg'' = reg' .~ (ipReg,ip')
                instr' = loadInstr ip'
            in trace (printf "ip: %i\tinstr: %s\treg: %s" ip (showInstr instr) (show reg)) $
                maybe reg' (`go` reg'') instr'

run1 :: String -> Int
run1 input =
    let (ipReg, instrs) = parseProgram input
        reg = IM.fromList $ zip [0..5] (repeat 0)
        reg' = runProgram ipReg (fromList instrs) reg
    in reg' .! 0

run2 :: String -> Int
run2 input =
    let (ipReg, instrs) = parseProgram input
        reg = IM.insert 0 1 . IM.fromList $ zip [0..5] (repeat 0)
        reg' = runProgram ipReg (fromList instrs) reg
    in reg' .! 0

parseProgram :: String -> (Int,[Instruction])
parseProgram s =
    let ipReg = read . (!!1) . words . head . lines $ s
        programLines = tail . lines $ s
    in (ipReg, fmap parseLine programLines)
    where
        parseLine s =
            let op:[a1,a2,a3] = words s
            in (fromEnum . (read @OpCode) . capitalize $ op, read a1, read a2, read a3)
        capitalize [] = []
        capitalize (x:xs) = toUpper x : xs
