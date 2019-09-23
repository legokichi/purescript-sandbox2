module Sandbox.ExistentialQuantification where

import Prelude

import Data.Exists (Exists, mkExists, runExists)
import Data.Tuple (Tuple(..), snd)
import Data.Maybe (Maybe)
import Data.Array as DA
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Existential Types Demo"
  pure unit

-- haskell の
--          + MkT :: forall a. a -> T
-- data T = (forall a. MkT a)
-- は
-- data T = MkT (exists a. a)
-- と同じ
-- 
-- purescript の
-- Exists f
-- は
-- exists a. f a
-- と同じ

data StreamF a s = StreamF s (s -> Tuple s a)
type Stream a = Exists (StreamF a)

nats :: Stream Number
nats = mkExists $ StreamF (0.0 :: Number) (\n -> Tuple (n + 1.0) n)

head :: forall a. Stream a -> a
head = runExists head'
  where
  head' :: forall s. StreamF a s -> a
  head' (StreamF s f) = snd (f s)

data MkT a = MkT a
type T = Exists MkT

heteroList :: Array T
heteroList = [
  mkExists $ MkT 5,
  mkExists $ MkT unit,
  mkExists $ MkT true,
  mkExists $ MkT "as",
  mkExists $ MkT (identity :: Number -> Number)
]

head2 :: Array T -> Maybe T
head2 arr = o
    where
    o :: Maybe T
    o = DA.head arr


