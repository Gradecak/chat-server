module Chatroom (Chatroom(..), Message(..), ControlMsg(..),
                 broadcast, addClient, removeClient, findRoom,
                 newRoom, nextId, notifyClient
                )where

--import Network.Socket
import           Client
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
--import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BS
import           Data.List                   (delete, find)
import Utils (joinedMsg, roomMsg, leaveMsg)

data Message = Message Client String
               deriving Show

data ControlMsg = Join  {getCl :: Client}
                | Leave {getCl :: Client}
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
        (Join c) -> joinedMsg n "5000" rId (Client.id c)
        (Leave c) -> leaveMsg rId (Client.id c)
  messageClient (BS.pack msg) (getCl ctrl)

--add a client to the chatroom
addClient :: Chatroom -> Client -> STM ()
addClient (Chatroom _ _ cls) nc = do
  cl <- readTVar cls
  writeTVar cls (nc:cl)

--remove a client by an id
removeClient :: Chatroom -> Client -> IO ()
removeClient (Chatroom _ _  cls) c = atomically $ do
  cl <- readTVar cls
  writeTVar cls (delete c cl)
  --updateMutex cl (delete c)

newRoom :: String -> Int -> STM Chatroom
newRoom rName rId = do
  clie <- newTVar []
  return $ Chatroom rName rId clie

nextId :: [Chatroom] -> Int
nextId [] = 0
nextId rs = roomId (head rs) + 1

findRoom :: TVar [Chatroom] -> (Chatroom -> Bool) -> IO (Maybe Chatroom)
findRoom rooms op  =  do
  rs <- atomically $ readTVar rooms
  return $ find op rs

-- -- returns true if a client is in a chatroom
-- existsClient :: Chatroom -> Int -> IO Bool
-- existsClient (Chatroom _ _ cls) cid = do
--   cl <- atomically $ readTVar cls
--   return $ any (\x -> Client.id x == cid) cl

-- --check if room has any clients in it
-- emptyRoom :: Chatroom -> IO Bool
-- emptyRoom (Chatroom _ _ c) = fmap null (atomically $ readTVar c) --readMVar c >>= return . null
