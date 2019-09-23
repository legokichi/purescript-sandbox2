module Sandbox.CommandTelemetry (main) where

import Control.Applicative (pure)
import Control.Bind (discard, bind)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Console (log)
import Signal ( {--Signal,--}(~>), {--(<~),--} constant, runSignal, flattenArray)
import Signal.Channel ( {--Channel,--}channel, send {--, subscribe--})
-- import Signal.Time (every)
import Prelude ( {--(<$>),--}($), (>>>), show)

main :: Effect Unit
main = do
  log "CommandTelemetry Demo "
  cn <- channel ""
  send cn "a"
  send cn "a"
  -- let sig = subscribe cn
  -- let sig = every (1000.0 :: Number)
  let
    sig = flattenArray (constant [ 1, 2, 3 ]) 0
  runSignal $ sig ~> show >>> log
  pure unit

type TelemetryLog
  = Array TelemetryItem

type TelemetryItem
  = { timestamp :: DateTime
    , telemetry :: Telemetry
    }

data Telemetry
  = Stdout { value :: String }
  | Stderr { value :: String }

type CommandLog
  = Array CommandType

data CommandType
  = UpdateCommand (Command UpdateCommandRequest UpdateCommandResponse)
  | TakePhotoCommand (Command Unit Unit)

type UpdateCommandRequest
  = {}

type UpdateCommandResponse
  = {}

type TakePhotoCommandRequest
  = {}

type TakePhotoCommandResponse
  = {}

type Command req res
  = { request :: Packet req
    , response :: Maybe (Packet res)
    }

type Packet a
  = { timestamp :: DateTime
    , value :: a
    }
