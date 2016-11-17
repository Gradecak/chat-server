module Chatroom ( Action(..), Chatroom(..),
                 runRoomIO, addToQueue
                )where

--import Network.Socket
import           Client
import           Control.Concurrent.Chan (Chan, readChan, writeChan)
import           Control.Concurrent.MVar (MVar, readMVar)
import           Control.Monad           (unless)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as BS
import           Data.List               (delete)
import           Utils                   (updateMutex)

data Action = Join Client
            | Leave Client
            | Message Client String

data Chatroom = Chatroom { roomName :: String
                         , roomId   :: Int
                         , clients  :: MVar [Client]
                         , action   :: Chan Action  -- a queue of actions for the chatroom
                         } deriving Eq

runRoomIO :: Chatroom -> IO ()
runRoomIO cr = do
  act <- readChan $ action cr
  case act of
    (Join c) -> addClient cr c >> runRoomIO cr
    (Message cl msg) -> broadcast cr (BS.pack msg) >> runRoomIO cr --TODO include client in message to room
    (Leave c) -> do
      removeClient cr c
      b <- emptyRoom cr
      unless b  (runRoomIO cr)

addToQueue :: Chatroom -> Action -> IO () -- does this need to be a function?
addToQueue (Chatroom _ _ _ chan) = writeChan chan

--deliver message to all clients in room
broadcast :: Chatroom -> B.ByteString -> IO () -- TODO take in client that sends message so that we know who sent it
broadcast room msg = do
  cli <- readMVar (clients room)
  mapM_ (messageClient msg) cli

--add a client to the chatroom
addClient :: Chatroom -> Client -> IO ()
addClient (Chatroom _ _ cl _) nc = updateMutex cl (nc:)

--remove a client by an id
removeClient :: Chatroom -> Client -> IO ()
removeClient (Chatroom _ _  cl _) c = updateMutex cl (delete c)

-- returns true if a client is in a chatroom
existsClient :: Chatroom -> Int -> IO Bool
existsClient (Chatroom _ _ cs _) cid = do
  clis <- readMVar cs
  return $ any (\x -> Client.id x == cid) clis

--check if room has any clients in it
emptyRoom :: Chatroom -> IO Bool
emptyRoom (Chatroom _ _ c _) = fmap null (readMVar c) --readMVar c >>= return . null
