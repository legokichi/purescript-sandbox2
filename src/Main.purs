module Main where

import Control.Applicative (pure)
import Control.Bind (discard)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Sandbox.Arrow as SArrow
import Sandbox.ST as SST
import Sandbox.Profunctor as SProfunctor
import Sandbox.CommandTelemetry as CommandTelemetry

main :: Effect Unit
main = do
  SST.main
  SProfunctor.main
  SArrow.main
  CommandTelemetry.main
  pure unit
