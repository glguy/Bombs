{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Lens
import Control.Monad (when)
import Control.Monad.State (execState, runState, execStateT, MonadState)
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
  do (detonated,finished) <- zoom serverWorld $ timeStepWorld elapsed
     forM_ detonated $ \b ->
         announce hs $ DetonateBomb $ bombCoord ^$ b
     forM_ finished $ \b ->
         announce hs $ ClearExplosion $ bombCoord ^$ b

connect ::
  Handles ConnectionId ->
  ConnectionId ->
  ServerState ->
  IO ServerState
connect hs i = execStateT $
  do ps      <- use (serverWorld.players)
     mapping <- use playerMapping
     let entityId = head $ [0..] \\ Map.elems mapping
     playerMapping %= Map.insert i entityId
     zoom serverWorld $ addPlayer entityId (0,0)
     announceOne hs i $ SetWorld $ Map.toList $ fmap (view playerCoord) ps
     announce    hs   $ MovePlayer entityId (0,0)

disconnect ::
  Handles ConnectionId ->
  ConnectionId ->
  ServerState ->
  IO ServerState
disconnect hs i = execStateT $
  do entityId <- who i
     announce hs $ DeletePlayer entityId
     zoom serverWorld $ removePlayer entityId
     playerMapping %= Map.delete i

command ::
  Handles ConnectionId ->
  ConnectionId ->
  ClientMessage ->
  ServerState ->
  IO ServerState
command hs i cmd = execStateT $
  do entityId   <- who i
     p		<- use $ serverWorld.player entityId
     zoom serverWorld $
       case cmd of
         Move dir ->
           do mbCoord <- movePlayer entityId dir
              for_ mbCoord $ \coord ->
                announce hs $ MovePlayer entityId coord
         DropBomb ->
           do success <- decrementBomb entityId
              when success $
                do placeBomb entityId (p^.playerPower) (p^.playerCoord)
                   announce hs $ AddBomb (p^.playerPower) (p^.playerCoord)

who :: MonadState ServerState m => ConnectionId -> m Int
who i = do Just entityId <- uses playerMapping $ Map.lookup i
           return entityId 
