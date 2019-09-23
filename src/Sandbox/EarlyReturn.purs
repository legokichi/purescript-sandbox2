module Sandbox.EarlyReturn where

import Control.Applicative (pure, when)
import Control.Bind (discard, bind)
import Control.Monad.Cont.Trans (ContT, runContT, callCC)
import Control.Monad.Trans.Class (lift)
import Control.Semigroupoid ((>>>))
import Data.Eq (class Eq, (==))
import Data.Ord ((<))
import Data.Function (($), (#))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Typelevel.Undefined (undefined)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)

main :: Effect Unit
main = do
  log "Early Return Demo"
  a <- (_ `runContT` \_ -> pure unit) (foo 19)
  log $ "unit: " <> show a
  b <- (bar `runContT` \_ -> pure "b")
  log $ "b: " <> b
  runContT baz log
  n <- runContT qux \msg -> do
    log msg
    pure 9999
  log $ show n
  pure unit

  where

  -- haskell の 
  -- runContT :: forall r m a. ContT r m a -> (a -> m r) -> m r
  -- purescript の
  -- runContT :: forall r m a. ContT r m a -> (a -> m r) -> m r

  -- purescript には when がないから early return できてない
  -- https://stackoverflow.com/questions/15441956/how-do-i-make-a-do-block-return-early
  foo :: Int -> (ContT Unit Effect) Unit
  foo n = callCC \cont -> do
    lift $ log "lift"
    when (n == 0) $ cont unit
    liftEffect do
      log "liftEffect"
    when (n < 10) $ cont unit
    liftEffect do
      log "liftEffect"
    pure unit

  bar :: (ContT String Effect) Unit
  --                                                         + (ContT String Effect)
  --                                                         | + Unit
  --    callCC :: forall a. ((forall b. a -> m b) -> m a) -> m a
  --    |                        + (ContT String Effect)
  --    |                        | + never?
  --    |       + forall b. a -> m b
  --    |       |         + (ContT String Effect)
  --    |       |         | + Unit
  --    |       |       + m a
  bar = callCC \cont -> do
    lift do
      log "lift"
      pure unit
    if true
    then do
      a :: Unit <- cont unit
      pure a
    else do
      pure unit

  baz :: (ContT Unit Effect) String
  baz = callCC \cont -> do
    pure "baz"

  qux :: (ContT Int Effect) String
  qux = callCC \cont -> do
    pure "baz"
