module Framework.Handles
 (Handles, listHandles, emptyHandles, removeHandle, lookupHandle,
  nullHandles, addHandle)
 where

import System.IO

newtype Handles k = Handles { listHandles :: [(k,Handle)] }

emptyHandles :: Handles k
emptyHandles = Handles []

addHandle :: k -> Handle -> Handles k -> Handles k
addHandle i h (Handles hs) = Handles ((i,h):hs)

removeHandle :: Eq k => k -> Handles k -> Handles k
removeHandle i (Handles hs) = Handles (aux hs)
  where
  aux [] = []
  aux (x:xs)
    | i == fst x = xs
    | otherwise  = x : aux xs

lookupHandle :: Eq k => k -> Handles k -> Maybe Handle
lookupHandle i (Handles xs) = lookup i xs

nullHandles :: Handles k -> Bool
nullHandles (Handles xs) = null xs
