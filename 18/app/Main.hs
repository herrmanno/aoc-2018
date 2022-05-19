module Main where

import Lib

main :: IO ()
main = do
    input <- getContents
    putStrLn "1:"
    print $ run1 input
    putStrLn "2:"
    print $ run2 input

