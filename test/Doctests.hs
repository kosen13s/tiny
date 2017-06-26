module Main where

import Test.DocTest


main :: IO ()
main = doctest
    [ "src/TinyParser.hs"
    , "src/TokenParser.hs"
    ]
