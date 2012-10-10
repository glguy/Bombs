{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad.State (execState, runState, execStateT)
import Control.Monad.IO.Class
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
tick hs elapsed s =
  do forM_ exploded $ \b ->
       announce hs $ DetonateBomb $ b^.bombCoord
     return s { _serverWorld = w' }
  where
  (exploded,w') = flip runState (_serverWorld s)
                $ timeStepWorld elapsed

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
     zoom serverWorld $ addEntity entityId (0,0)
     liftIO $ announceOne hs i $ SetWorld $ Map.toList ps
     liftIO $ announce    hs   $ MoveEntity entityId (0,0)

disconnect ::
  Handles ConnectionId ->
  ConnectionId ->
  ServerState ->
  IO ServerState
disconnect hs i = execStateT $
  do Just entityId <- uses playerMapping (Map.lookup i)
     liftIO $ announce hs $ DeleteEntity entityId
     zoom serverWorld $ removeEntity entityId
     playerMapping %= Map.delete i

command ::
  Handles ConnectionId ->
  ConnectionId ->
  ClientMessage ->
  ServerState ->
  IO ServerState
command hs i cmd = execStateT $
  do Just entityId    <- uses playerMapping         (Map.lookup i       )
     Just playerCoord <- uses (serverWorld.players) (Map.lookup entityId)
     zoom serverWorld $
       case cmd of
         Move dir ->
           do mbCoord <- moveEntity entityId dir
              for_ mbCoord $ \coord ->
                liftIO $ announce hs $ MoveEntity entityId coord
         DropBomb ->
           do placeBomb entityId playerCoord
              liftIO $ announce hs $ AddBomb playerCoord
