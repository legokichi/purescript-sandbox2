module Sandbox.TaglessFinal where

import Prelude (pure, when, (<>), ($), (#), (+), (*), (-), (/), discard, bind, (>>>), Unit, unit, show, identity, class Semiring, class Ring, class EuclideanRing)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "GADTs sym by Tagless Final"
  log $ "eval: " <> (show $ eval testR)
  log $ "eval: " <> (show $ eval testR')
  pure unit

-- https://hgiasac.github.io/posts/2018-12-18-PureScript-GADTs-Alternatives---Recap.html
class LambdaSym repr where
  val :: forall a. a -> repr a
  lambda :: forall a b. (repr a -> repr b) -> repr (a -> b)
  apply :: forall a b. repr (a -> b) -> repr a -> repr b
  add :: forall a. Semiring a => repr a -> repr a -> repr a
  sub :: forall a. Ring a => repr a -> repr a -> repr a

data R a
  = R a

unR :: forall a. R a -> a
unR (R a) = a

eval :: forall a. R a -> a
eval (R a) = a

instance lambdaSymmR :: LambdaSym R where
  val a = R a
  lambda f = R (\a -> unR $ f (R a))
  apply (R f) (R a) = R (f a)
  add (R x) (R y) = R $ x + y
  sub (R x) (R y) = R $ x - y

testR ::
  forall repr.
  LambdaSym repr =>
  repr Int
testR = apply (lambda \x -> add x x) (val 10)

class MultiplySymm repr where
  mult :: forall a. Semiring a => repr a -> repr a -> repr a
  div :: forall a. EuclideanRing a => repr a -> repr a -> repr a

instance multiplySymmR :: MultiplySymm R where
  mult (R x) (R y) = R $ x * y
  div (R x) (R y) = R $ x / y

testR' ::
  forall repr.
  LambdaSym repr =>
  MultiplySymm repr =>
  repr Int
testR' = apply (lambda (\x -> mult x x)) (val 10) `div` (val 10)
