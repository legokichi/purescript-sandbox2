module Sandbox.Arrow (main) where

import Control.Applicative (pure)
import Control.Bind ((>>=), discard {--, bind--})
import Control.Semigroupoid ((>>>))
import Control.Lazy (class Lazy)
import Data.Eq (class Eq  {--, (==)--})
import Data.Either (Either(Left))
import Data.Function (($), (#))
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(Just {--, Nothing --}))
import Data.Profunctor (class Profunctor  {--, dimap, arr--})
import Data.Profunctor.Choice ((+++), (|||))
import Data.Profunctor.Costrong (class Costrong, unfirst {--, unsecond--})
import Data.Profunctor.Strong ((&&&), (***), first, second)
import Data.Semigroup ((<>))
import Data.Semiring ((+) {--, (*)--})
-- import Data.Ring ((-))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(Tuple))
import Data.Typelevel.Undefined (undefined)
import Data.Unit (Unit, unit)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Console (log)
-- import Unsafe.Coerce (unsafeCoerce)
import Effect.Unsafe (unsafePerformEffect)

main :: Effect Unit
main = do
  log "Arrow Demo"
  log $ ("arrow: " <> _) $ show $ A # a2bc >>> bc2cc >>> cc2aa >>> (second a2b)
  log $ ("choice: " <> _) $ show $ Left A # (a2b +++ a2c) >>> (b2c +++ c2a) >>> (c2a +++ a2b) >>> (a2c ||| b2c)
  log $ ("loop: " <> _) $ show $ let Function' a2a = unfirst (Function' ooo) in a2a $ a2a A
  log $ ("loop2: " <> _) $ show $ let Function' f = unfirst (Function' iii) in f true
  pure unit
  where
  ooo :: (Tuple A (Maybe Int)) -> (Tuple A (Maybe Int))
  ooo = case _ of
    Tuple a (Just c) ->
      Tuple a $ Just
        $ unsafePerformEffect do
            log "ooo"
            pure $ c + 1
    Tuple a _ -> Tuple a $ Just $ unsafePerformEffect $ (log "ooo") >>= \_ -> pure 0

  iii :: (Tuple Boolean (Unit -> Int)) -> (Tuple Boolean (Unit -> Int))
  iii (Tuple reset lazy) = case reset of
    true -> Tuple false \_ -> spy "iii" 0
    false -> Tuple false $ \_ -> spy "iii" $ 1 + lazy unit

  a2b A = B

  a2c A = C

  b2c B = C

  c2a C = A

  a2bc = (a2b &&& a2c)

  bc2cc = first b2c

  cc2aa = (c2a *** c2a)

--- for ArrowLoop
newtype Function' a b
  = Function' (a -> b)

instance profunctorFn' :: Profunctor Function' where
  dimap a2b c2d (Function' b2c) = Function' (a2b >>> b2c >>> c2d)

instance costrongFn' :: Costrong Function' where
  unfirst :: forall a b c. (Function' (Tuple a c) (Tuple b c)) -> Function' a b
  unfirst (Function' f) =
    Function' \a ->
      let
        (Tuple b c) = f (Tuple a c)
      in
        b
    where
    c = undefined
  unsecond :: forall a b c. (Function' (Tuple a b) (Tuple a c)) -> Function' b c
  unsecond (Function' f) =
    Function' \b ->
      let
        (Tuple a c) = f (Tuple a b)
      in
        c
    where
    a = undefined

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
