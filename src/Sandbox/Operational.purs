module Sandbox.Operational where

import Data.Coyoneda
import Data.Show (show)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Prelude (pure, when, (<>),($),(#),discard, bind,(>>>))

main :: Effect Unit
main = do
  log "Operational Monad Demo"
  pure unit

