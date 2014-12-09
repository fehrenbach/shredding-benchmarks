module Main where

import Nested1
import Nested2

main :: IO ()
main = sequence_ [ Nested1.doRun
                 , Nested2.doRun
                 ]
