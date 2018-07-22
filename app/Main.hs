module Main where

import Control.Monad.Trans.State
import Lib

main :: IO ()
main = do 
  evalStateT morra $ initGameState
