module Sandbox.ST (main) where

import Control.Applicative (pure)
import Control.Bind (discard, bind)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Function (($))
import Data.Semiring ((+), (*))
import Data.Ring ((-))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Console (log)
import Data.Show (show)

main :: Effect Unit
main = do
  log "ST Demo"
  log $ show sumOfSquares
  pure unit

sumOfSquares :: Int
sumOfSquares = ST.run f
  where
  f :: (forall r. ST.ST r Int)
  f = do
    total <- STRef.new 0
    let
      loop 0 = STRef.read total

      loop n = do
        _ <- STRef.modify (_ + (n * n)) total
        loop (n - 1)
    loop 1
