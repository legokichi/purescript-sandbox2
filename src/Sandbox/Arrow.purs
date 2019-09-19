module Sandbox.Arrow where

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
  log "arrowDemo"
  log $ show $ A # a2bc >>> bc2cc >>> cc2aa >>> (second a2b)
  pure unit
  where
  a2b A = B

  a2c A = C

  b2c B = C

  c2a C = A

  a2bc = (a2b &&& a2c)

  bc2cc = first b2c

  cc2aa = (c2a *** c2a)

data A
  = A

derive instance eqA :: Eq A

instance showA :: Show A where
  show A = "A"

data B
  = B

derive instance eqB :: Eq B

instance showB :: Show B where
  show B = "B"

data C
  = C

derive instance eqC :: Eq C

instance showC :: Show C where
  show C = "C"

data D
  = D

derive instance eqD :: Eq D

instance showD :: Show D where
  show D = "D"
