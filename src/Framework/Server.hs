{-# LANGUAGE RecordWildCards #-}
module Framework.Server
  (NetworkServer(..), serverMain,
   Handles, ConnectionId,
   announce, announceOne
  )
  where

import Control.Concurrent (forkIO, threadDelay, ThreadId,
                           Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, handle, bracket_)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Binary (Binary, encode, decode)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Network
import Network.Socket (getSocketName)
import System.IO
import qualified Data.ByteString.Lazy as B

import Framework.Handles

data NetworkServer c w = NetworkServer
  { serverPort   :: PortID
  , eventsPerSecond :: Int
  , onTick       :: Handles ConnectionId -> Float             -> w -> IO w
  , onConnect    :: Handles ConnectionId -> ConnectionId      -> w -> IO w
  , onDisconnect :: Handles ConnectionId -> ConnectionId      -> w -> IO w
  , onCommand    :: Handles ConnectionId -> ConnectionId -> c -> w -> IO w
  }

newtype ConnectionId = ConnectionId Int
  deriving (Eq, Show, Ord)

-- | Main entry point for server
serverMain ::
  Binary c =>
  NetworkServer c w -> w -> IO ()
serverMain env w =
  do events             <- newChan
     _acceptThreadId    <- startNetwork env events
     lastTick           <- getCurrentTime
     _tickThreadId      <- forkIO $ tickThread env events
     eventLoop env emptyHandles w events lastTick

-- | Create a thread which will accept new connections.
-- Connections and disconnections will be announced to the event channel.
startNetwork ::
  Binary c =>
  NetworkServer c w -> Chan (ServerEvent c) -> IO ThreadId
startNetwork env events =
  do sock       <- listenOn $ serverPort env
     sockName   <- getSocketName sock
     putStrLn $ "Server listening on " ++ show sockName
     forkIO $ mapM_ (acceptClient events sock . ConnectionId) [0..]

-- | Accept a connection and create a thread to manage incoming data
-- from that connection.
acceptClient ::
  Binary c => 
  Chan (ServerEvent c) -> Socket -> ConnectionId -> IO ThreadId
acceptClient events sock i =
  do (h,host,port) <- accept sock
     putStrLn $ concat ["Got connection from ", host, ":", show port]
     hSetBuffering h NoBuffering
     forkIO $ bracket_ (writeChan events $ JoinEvent i h)
                       (writeChan events $ DisconnectEvent i)
                       (clientSocketLoop i h events)

-- | Read incoming packets, decode them, and pass them along to
-- the event channel.
clientSocketLoop ::
  Binary c =>
  ConnectionId -> Handle -> Chan (ServerEvent c) -> IO ()
clientSocketLoop i h events =
  handle ignoreExceptions $
  forever $ do nBs <- B.hGet h 8
               let n = decode nBs :: Int64
               bs <- B.hGet h (fromIntegral n)
               writeChan events $ ClientEvent i $ decode bs

-- | Send a command to a collection of clients
announce :: (MonadIO m, Binary c) => Handles ConnectionId -> c -> m ()
announce hs msg = liftIO $
  do let bs = encode msg
         n  = encode (B.length bs)
     forM_ (listHandles hs) $ \(_name,h) ->
       handle ignoreExceptions $
       do B.hPutStr h n
          B.hPutStr h bs
          hFlush h

-- | Send a command to a single client identified by id.
announceOne ::
  (MonadIO m, Binary c) =>
  Handles ConnectionId ->
  ConnectionId ->
  c ->
  m ()
announceOne hs i msg = liftIO $
  do let bs = encode msg
         n  = encode (B.length bs)
     for_ (lookupHandle i hs) $ \h ->
       handle ignoreExceptions $
         do B.hPutStr h n
            B.hPutStr h bs
            hFlush h

ignoreExceptions :: SomeException -> IO ()
ignoreExceptions _ = return ()

tickThread :: NetworkServer c w -> Chan (ServerEvent c) -> IO ()
tickThread env events =
  forever $ do writeChan events TickEvent
               threadDelay $ 1000000 `div` eventsPerSecond env

data ServerEvent c
  = TickEvent
  | JoinEvent       ConnectionId Handle
  | DisconnectEvent ConnectionId
  | ClientEvent     ConnectionId c

eventLoop ::
  NetworkServer c w ->
  Handles ConnectionId ->
  w ->
  Chan (ServerEvent c) ->
  UTCTime {- ^ time previous tick was processed -} ->
  IO ()
eventLoop env hs w events lastTick =
  do event <- readChan events
     case event of
       JoinEvent i h ->
         do let hs' = addHandle i h hs
            w' <- onConnect env hs' i w
            eventLoop env hs' w' events lastTick

       TickEvent ->
         do now <- getCurrentTime
            let elapsed = realToFrac (diffUTCTime now lastTick)
            w' <- onTick env hs elapsed w
            eventLoop env hs w' events now

       ClientEvent i c ->
         do w' <- onCommand env hs i c w
            eventLoop env hs w' events lastTick

       DisconnectEvent i ->
         do let hs' = removeHandle i hs
            w' <- onDisconnect env hs i w
            eventLoop env hs' w' events lastTick
