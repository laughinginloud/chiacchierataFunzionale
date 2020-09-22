module Main where

import Lib (stampaDisplay')

main :: IO ()
main = do
    x:[] <- getLine
    stampaDisplay' x