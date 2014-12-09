{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Main where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler
import Database.HDBC.PostgreSQL

t1 :: Q [(Integer, Integer)]
t1 = table "t1" -- (v:Int, a:Int)

t2 :: Q [(Integer, Integer)]
t2 = table "t2" -- (a:Int, b:Int)

t3 :: Q [(Integer, Integer)]
t3 = table "t3" -- (b:Int, v:Int)

nested1 :: Q [[[Integer]]]
nested1 = [ [ [ zv
              | (view -> (zb, zv)) <- t3
              , yb == zb ]
            | (view -> (ya, yb)) <- t2
            , xa == ya ]
          | (view -> (xv, xa)) <- t1
          ]

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'stefan' password = '' host = 'localhost' dbname = 'nested1'"

runQ :: (Show a,QA a) => Q a -> IO ()
runQ q = getConn P.>>= \conn -> (fromQ conn q P.>>= P.print) P.>> disconnect conn

main :: IO ()
main = sequence_ [ runQ nested1
                 ]
