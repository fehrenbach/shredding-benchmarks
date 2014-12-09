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

module Nested2 (doRun) where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler
import Database.HDBC.PostgreSQL

t1 :: Q [Integer]
t1 = table "t1" -- (a:Int)

t2 :: Q [(Integer, Integer)]
t2 = table "t2" -- (a:Int, b:Int)

t3 :: Q [(Integer, Integer)]
t3 = table "t3" -- (b:Int, v:Int)

t4 :: Q [(Integer, Integer)]
t4 = table "t4" -- (b:Int, v:Int)

nested2 :: Q [[[Integer]]]
nested2 = [ [ [ zv
              | (view -> (zb, zv)) <- t3
              , yb == zb ]
            | (view -> (ya, yb)) <- t2
            , xa == ya ]
          | (view -> (xa)) <- t1
          ]
          ++
          [ [ [ zv
              | (view -> (zb, zv)) <- t4
              , yb == zb ]
            | (view -> (ya, yb)) <- t2
            , xa == ya ]
          | (view -> (xa)) <- t1
          ]

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'stefan' password = '' host = 'localhost' dbname = 'nested2'"

runQ :: (Show a,QA a) => Q a -> IO ()
runQ q = getConn P.>>= \conn -> (fromQ conn q P.>>= P.print) P.>> disconnect conn

doRun :: IO ()
doRun = runQ nested2
