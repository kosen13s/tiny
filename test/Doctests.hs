module Main where

import Test.DocTest


main :: IO ()
main = doctest
    [ "src/Lib.hs"
    , "src/TinyParser.hs"
    , "src/LexicalParser.hs"
    ]
