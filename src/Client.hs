module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad (forever)
import Control.Monad.State (execState, put)
import Control.Lens
import Data.Binary
import Data.Int
import Data.Map (Map)
import Graphics.Gloss.Interface.IO.Game
import Network (connectTo, PortID(PortNumber))
import System.Exit (exitSuccess)
import System.IO (Handle,hFlush)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map

import Simulation
import Messages
import Framework.Packet

main :: IO ()
main =
  do h <- connectTo "localhost" $ PortNumber 15000
     var <- newMVar startingWorld

     forkIO $ recv h var
     
     playIO
            (InWindow "Framework Demo" (16*11,16*11) (10,10))
            black
            1
            ()
            (\_ -> fmap drawWorld $ readMVar var)
            (event h)
            tick

tick :: Float -> () -> IO ()
tick _ () = return ()

event :: Handle -> Event -> () -> IO ()
event h (EventKey (SpecialKey KeyUp   ) Down _ _) () = send h $ Move U
event h (EventKey (SpecialKey KeyDown ) Down _ _) () = send h $ Move D
event h (EventKey (SpecialKey KeyLeft ) Down _ _) () = send h $ Move L
event h (EventKey (SpecialKey KeyRight) Down _ _) () = send h $ Move R
event h (EventKey (Char 'b'           ) Down _ _) () = send h $ DropBomb
event h _                                         () = return ()


updateWorldFromNetwork :: MVar World -> ServerMessage -> IO ()
updateWorldFromNetwork var msg = modifyMVar_ var $ return . execState
      (case msg of
        SetWorld w              -> Control.Monad.State.put w
        MovePlayer i c          -> addPlayer i c
        DeletePlayer i          -> removePlayer i
        AddBomb i c             -> placeBomb i c
        DetonateBomb c          -> explodeBomb c >> return ()
        ClearExplosion c        -> removeBomb c)

translateCoord :: Coord -> Picture -> Picture
translateCoord (x,y) = translate (fromIntegral (16*x)) (fromIntegral (16*y))

drawWorld :: World -> Picture
drawWorld w =
  pictures $
    background :
    drawBombs (w^.bombs) :
    [ translateCoord (p^.playerCoord)
    $ pictures
        [ color red $ rectangleSolid 14 14
        , translate (-4) (-4) $ scale 0.1 0.1 $ color white $ text $ show i
        ]
    | (i,p) <- Map.toList $ players ^$ w
    ]
    ++
    [ translateCoord c
    $ color green $ rectangleSolid 14 14
    | x <- [minX..maxX]
    , y <- [minY..maxY]
    , let c = (x,y)
    , not (isValidCoord c)
    ]

  where
  background = color (greyN 0.5)
             $ rectangleSolid (fromIntegral (16 * (1 + (maxX - minX))))
                              (fromIntegral (16 * (1 + (maxY - minY))))

drawBombs :: [(Coord,Bomb)] -> Picture
drawBombs bs = pictures
    [ translateCoord c
    $ if b^.bombExploded
        then drawExplosion c (b^.bombPower)
        else drawBomb
    | (c,b) <- bs
    ]

drawBomb :: Picture
drawBomb = color black $ circleSolid 8

drawExplosion :: Coord -> Int -> Picture
drawExplosion (x,y) power
  = color orange
  $ pictures
  $ drawExplosionCenter :
    [ rotate angle
    $ translateCoord (i,0)
    $ drawExplosionCell
    | i     <- [1..power]
    , angle <- [0,90,180,270]
    , onBoardTest i angle
    ]
  where
  -- This function will have to be updated to avoid
  -- blocks on the board, not just borders and pillars
  onBoardTest i   0 = even y && x+i <= maxX
  onBoardTest i  90 = even x && y-i >= minY
  onBoardTest i 180 = even y && x-i >= minX
  onBoardTest i 270 = even x && y+i <= maxY

drawExplosionCell :: Picture
drawExplosionCell
  = color orange
  $ pictures
     [ translate (fromIntegral (4*i)) 0
     $ line [(-8,8),(-4,0),(-8,-8)]
     | i <- [0 .. 3 :: Int]
     ]

drawExplosionCenter :: Picture
drawExplosionCenter =
  line [(0,8),(-k,k),(-8,0)
       ,(-k,-k),(0,-8),(k,-k)
       ,(8,0),(k,k),(0,8)]
  where
  k = 16/3

send :: Handle -> ClientMessage -> IO ()
send h msg = handle ignoreExceptions $ hPutPacket h $ mkPacket msg

ignoreExceptions :: SomeException -> IO ()
ignoreExceptions _ = return ()

recv :: Handle -> MVar World -> IO a
recv h var = forever $ updateWorldFromNetwork var =<< hGetPacketed h
