module Main where

import Lib

main :: IO ()
main = do
    input <- getContents
    print $ run1 input
