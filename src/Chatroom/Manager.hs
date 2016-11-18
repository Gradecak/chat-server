module Chatroom.Manager where

import Chatroom
import Client
import Control.Concurrent (forkFinally)
import Control.Concurrent (MVar, takeMVar, putMVar, readMVar, newMVar)
import Control.Concurrent.Chan (Chan, readChan, newChan, writeChan)
import Data.List (find)
import Utils (updateMutex, joinedMessage)

data Manage = Create String Client -- name of room and first client to add
            | Remove Chatroom

data Manager = Manager { rooms :: MVar [Chatroom]
                       , actions :: Chan Manage
                       , kill :: MVar ()
                       }

newManager :: MVar () -> IO Manager
newManager k = do
  rs <- newMVar []
  ch <- newChan
  return $ Manager rs ch k

addToQueue :: Manager -> Manage -> IO ()
addToQueue (Manager _ chan _) = writeChan chan

-- Some helper functions
addRoom :: Manager -> Chatroom -> IO ()
addRoom srv r = updateMutex (rooms srv) (r:)

removeRoom :: Manager -> Chatroom -> IO ()
removeRoom mgr r = updateMutex (rooms mgr) (\x -> [y | y <- x, y /= r])

findRoom :: Manager -> (Chatroom -> Bool) -> IO (Maybe Chatroom)
findRoom (Manager mrs _ _) op = do
  rs <- readMVar mrs
  return $ find op rs

createChatroom :: String -> Int -> Client -> IO Chatroom
createChatroom n i cl = do
  cls <- newMVar [cl]
  ch <- newChan
  return $ Chatroom n i cls ch

roomCloseHandler :: Manager -> Chatroom -> IO ()
roomCloseHandler mgr r = Chatroom.Manager.addToQueue mgr (Remove r)

-- run the room manager
runManager :: Manager -> Int -> IO ()
runManager m@(Manager _ actionChan _) i = do
  act <- readChan actionChan
  case act of
    (Remove c) -> removeRoom m c >> runManager m i
    (Create n cl) -> do
      room <- createChatroom n i cl
      addRoom m room
      _ <-forkFinally (runRoom room) (\_ -> roomCloseHandler m room)
      -- notify the joining client that they have succesfully joined
      Chatroom.addToQueue room (Message cl $ joinedMessage n "5000" (roomId room) (Client.id cl)) 
      runManager m (i+1)
