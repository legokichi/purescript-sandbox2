module Main where

import Sandbox.Arrow as SArrow
import Sandbox.Profunctor as SProfunctor
import Data.Eq (class Eq, (==))
import Data.Function (($), (#))
import Data.Profunctor (dimap, arr)
import Data.Profunctor.Choice
import Data.Profunctor.Strong ((&&&), (***), first, second)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Unit (Unit, unit)
import Control.Applicative (pure)
import Control.Bind (discard)
import Control.Semigroupoid ((>>>))
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  SProfunctor.main
  SArrow.main
  pure unit
