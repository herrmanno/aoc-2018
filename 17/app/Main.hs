module Main where

import Lib

main :: IO ()
main = do
    input <- getContents
    let (resultMap, n, n') = run input
    putStrLn $ showMap resultMap
    print n
    print n'


testMap = parseInput <$> readFile "test.txt"
