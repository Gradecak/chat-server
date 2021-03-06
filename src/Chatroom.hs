module Chatroom (Chatroom(..), Message(..), ControlMsg(..),
                 broadcast, addClient, removeClient, findRoom,
                 newRoom, nextId, notifyClient, existsClient
                )where

import           Client
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import qualified Data.ByteString.Char8       as BS
import           Data.List                   (delete, find)
import           Utils                       (joinedMsg, leaveMsg, roomMsg)

data Message = Message Client String
               deriving Show

data ControlMsg = Join  {getCl :: Client}
                | Leave {getCl :: Client}
                | Error String
                  deriving Show

data Chatroom = Chatroom { roomName :: String
                         , roomId   :: Int
                         , clients  :: TVar [Client]
                         } deriving Eq

--deliver message to all clients in room
broadcast :: Chatroom -> Message -> IO () -- TODO take in client that sends message so that we know who sent it
broadcast (Chatroom _ rId cls) (Message c s) = do
  cli <- atomically (readTVar cls)
  let msg = roomMsg rId (name c) s
  mapM_ (messageClient $ BS.pack msg) cli --map message client over list of clients

notifyClient :: Chatroom -> ControlMsg -> IO ()
notifyClient (Chatroom n rId _) ctrl = do
  let msg = case ctrl of
        (Join c)  -> joinedMsg n "5000" rId (clientId c)
        (Leave c) -> leaveMsg rId (clientId c)
  messageClient (BS.pack msg) (getCl ctrl)

--atomically add a clien to list of rooms and notify room of new client
addClient :: Chatroom -> Client -> IO ()
addClient r@(Chatroom _ _ cls) nc = do
  atomically $ do
    cl <- readTVar cls
    writeTVar cls (nc:cl)
  broadcast r (Message nc (name nc ++ " has joined this chatroom."))

--remove a client by an id
removeClient :: Client -> Chatroom -> IO ()
removeClient c r@(Chatroom _ _  cls) = do
  broadcast r (Message c (name c ++ " has left this chatroom."))
  atomically $ do
    cl <- readTVar cls
    writeTVar cls (delete c cl)


newRoom ::TVar [Chatroom] -> String -> IO Chatroom
newRoom rs rName = atomically $ do
  rooms <- readTVar rs
  rClients <- newTVar []
  let nRoom = Chatroom rName (nextId rooms) rClients
  writeTVar rs (nRoom:rooms)
  return nRoom

nextId :: [Chatroom] -> Int
nextId [] = 0
nextId rs = roomId (head rs) + 1

findRoom :: TVar [Chatroom] -> (Chatroom -> Bool) -> IO (Maybe Chatroom)
findRoom rooms op  =  do
  rs <- atomically $ readTVar rooms
  return $ find op rs

existsClient :: Chatroom -> Client -> IO Bool
existsClient cr cl = do
  cls <- atomically $ readTVar $ clients cr
  return $ any (\x -> clientId cl == clientId x) cls
  
