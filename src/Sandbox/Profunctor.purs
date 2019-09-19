module Sandbox.Profunctor where

import Data.Eq (class Eq, (==))
import Data.Function (($))
import Data.Profunctor (dimap)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Unit (Unit, unit)
import Control.Applicative (pure)
import Control.Bind (discard)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "dimapDemo"
  let
    a = dimap g i h

    b = (a A == f A) == true
  log $ "a A == f A ==" <> show b
  pure unit
  where
  f :: A -> D
  f A = D

  g :: A -> B
  g A = B

  h :: B -> C
  h B = C

  i :: C -> D
  i C = D

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
