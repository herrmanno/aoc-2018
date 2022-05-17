module Main where

import System.Environment
import Lib

main :: IO ()
main = do
    input <- getContents
    print "1:"
    print $ run1 input
    print "2:"
    print $ run2 input


testMap = buildMap <$> readFile "test1.txt"