module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad (forever)
import Data.Binary
import Data.Int
import Data.Map (Map)
import Graphics.Gloss.Interface.IO.Game
import Network (connectTo, PortID(PortNumber))
import System.Exit (exitSuccess)
import System.IO (hFlush)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map

import Simulation
import Messages

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

tick _ () = return ()

event h (EventKey (SpecialKey KeyUp   ) Down _ _) () = send h $ Move U
event h (EventKey (SpecialKey KeyDown ) Down _ _) () = send h $ Move D
event h (EventKey (SpecialKey KeyLeft ) Down _ _) () = send h $ Move L
event h (EventKey (SpecialKey KeyRight) Down _ _) () = send h $ Move R
event h (EventKey (Char 'b'           ) Down _ _) () = send h $ DropBomb
event h _                                         () = return ()


updateWorldFromNetwork var msg =
  modifyMVar_ var $ \w ->
  case msg of
    SetWorld xs    -> return $ w { players = Map.fromList xs }
    MoveEntity i c -> return $ addEntity i c w
    DeleteEntity i -> return $ removeEntity i w
    AddBomb c      -> return $ placeBomb 0 c w
    DetonateBomb c -> return $ removeBomb c w

translateI x y = translate (fromIntegral (16*x)) (fromIntegral (16*y))

drawWorld w =
  pictures $
    background :
    [ translateI  x y
    $ pictures
        [ color black $ circleSolid 8
        , translate (-4) (-4) $ scale 0.1 0.1 $ color white $ text $ show $ ceiling $ bombTimer b
        ]
    | b <- bombs w
    , let (x,y) = bombCoord b ]
    ++
    [ translateI x y
    $ pictures
        [ color red $ rectangleSolid 16 16
        , translate (-4) (-4) $ scale 0.1 0.1 $ color white $ text $ show i
        ]
    | (i,(x,y)) <- Map.toList $ players w]
    ++
    [ translateI x y
    $ color green $ rectangleSolid 14 14
    | x <- [minX..maxX]
    , y <- [minY..maxY]
    , not (isValidCoord (x,y))
    ]

  where
  background = color (greyN 0.5)
             $ rectangleSolid (fromIntegral (16 * (1 + (maxX - minX))))
                              (fromIntegral (16 * (1 + (maxY - minY))))

--------------------------
-- Client framework code
--------------------------


-- | Send a command to a single client identified by id.
send h msg =
  handle ignoreExceptions $
  do let bs = encode msg
         n  = encode (B.length bs)
     B.hPutStr h n
     B.hPutStr h bs
     hFlush h

ignoreExceptions :: SomeException -> IO ()
ignoreExceptions _ = return ()

recv h var =
  forever (do nBs <- B.hGet h 8
              let n = decode nBs :: Int64
              bs <- B.hGet h (fromIntegral n)
              updateWorldFromNetwork var $ decode bs)


