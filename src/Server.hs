{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Lens
import Control.Monad (when)
import Control.Monad.State (execState, runState, execStateT, MonadState)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable
import Data.List
import Data.Map (Map)
import Network(PortID(PortNumber))
import qualified Data.Map as Map

import Framework.Server (NetworkServer(..), serverMain, Handles, ConnectionId, announce, announceOne)
import Messages
import Simulation

data ServerState = ServerState
  { _serverWorld         :: World
  , _playerMapping       :: Map ConnectionId Int
  }

makeLenses ''ServerState

main = serverMain myNetworkServer initialState

myNetworkServer = NetworkServer
  { serverPort          = PortNumber 15000
  , eventsPerSecond     = 10
  , onTick              = tick
  , onConnect           = connect
  , onDisconnect        = disconnect
  , onCommand           = command
  }


initialState :: ServerState
initialState = ServerState
  { _serverWorld	= startingWorld
  , _playerMapping	= Map.empty
  }


tick :: Handles ConnectionId -> Float -> ServerState -> IO ServerState
tick hs elapsed = execStateT $
  do (detonated,finished,burned) <- zoom serverWorld $ timeStepWorld elapsed
     traverse_ (announce hs . DetonateBomb  ) detonated
     traverse_ (announce hs . ClearExplosion) finished
     liftIO $ print burned

connect ::
  Handles ConnectionId ->
  ConnectionId ->
  ServerState ->
  IO ServerState
connect hs i = execStateT $
  do entityId <- allocateEntityId i
     zoom serverWorld $ addPlayer entityId (0,0)
     w <- use serverWorld
     announceOne hs i $ SetWorld w
     announce    hs   $ MovePlayer entityId (0,0)

-- | Assign the next available Int to this connection.
allocateEntityId :: MonadState ServerState m => ConnectionId -> m Int
allocateEntityId i =
  do used <- uses playerMapping Map.elems
     let entityId : _ = [0..] \\ used
     who i .= Just entityId
     return entityId

disconnect ::
  Handles ConnectionId ->
  ConnectionId ->
  ServerState ->
  IO ServerState
disconnect hs i = execStateT $
  do Just entityId <- use $ who i
     zoom serverWorld $ removePlayer entityId
     who i .= Nothing
     announce hs $ DeletePlayer entityId

command ::
  Handles ConnectionId ->
  ConnectionId ->
  ClientMessage ->
  ServerState ->
  IO ServerState
command hs i cmd = execStateT $
  do Just entityId <- use $ who i
     zoom serverWorld $
       case cmd of
         Move dir ->
           do mbCoord <- movePlayer entityId dir
              for_ mbCoord $ \coord ->
                announce hs $ MovePlayer entityId coord
         DropBomb ->
           do success <- decrementBomb entityId
              when success $
                do p <- use $ player entityId
                   placeBomb entityId (p^.playerCoord)
                   announce hs $ AddBomb entityId (p^.playerCoord)

who i = playerMapping . at i
