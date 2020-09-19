module Tasks where


{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}


import Control.Monad
import Data.Bits
import Data.List
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

helloWorlds 0 = return ()
helloWorlds x = do putStrLn "Hello World"
                   helloWorlds (x-1)

main :: IO()
main = do
    n <- readLn :: IO Int
    helloWorlds n