module Chatroom.Manager where

import Chatroom
import Control.Concurrent (MVar, takeMVar, putMVar, readMVar)
import Control.Concurrent.Chan (Chan, readChan) 
import Data.List (find)
import Utils (updateMutex)

data Manage = Add Chatroom
            | Remove Chatroom

data Manager = Manager { rooms :: MVar [Chatroom]
                       , actions :: Chan Manage
                       }

-- Some helper functions
addRoom :: Manager -> Chatroom -> IO ()
addRoom srv r = updateMutex (rooms srv) (r:)

removeRoom :: Manager -> Chatroom -> IO ()
removeRoom srv r = updateMutex (rooms srv) (\x -> [y | y <- x, y /= r])

findRoom :: Manager -> (Chatroom -> Bool) -> IO (Maybe Chatroom)
findRoom (Manager mrs _) op = do
  rs <- readMVar mrs
  return $ find op rs

-- run the room manager
runManager :: Manager -> IO ()
runManager m@(Manager _ actionChan) = do
  act <- readChan actionChan
  case act of
    (Add c) -> addRoom m c >> runManager m
    (Remove c) -> removeRoom m c >> runManager m
