module Main where

import Control.Applicative (pure)
import Control.Bind (discard)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Sandbox.Arrow as SbxArrow
import Sandbox.ST as SbxST
import Sandbox.Profunctor as SbxProfunctor
import Sandbox.EarlyReturn as SbxEarlyReturn
import Sandbox.CommandTelemetry as SbxCommandTelemetry
import Sandbox.TaglessFinal as SbxTaglessFinal
import Sandbox.Operational as SbxOperational
import Sandbox.ExistentialQuantification as SbxExistentialQuantification
import Sandbox.SakuraScriptAlgebra as SbxSakuraScriptAlgebra

main :: Effect Unit
main = do
  SbxST.main
  SbxProfunctor.main
  SbxArrow.main
  SbxEarlyReturn.main
  SbxOperational.main
  SbxTaglessFinal.main
  SbxExistentialQuantification.main
  SbxCommandTelemetry.main
  SbxSakuraScriptAlgebra.main
  pure unit
