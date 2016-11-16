module Chatroom ( Action(..), Actions, Chatroom(..),
                  runActionIO, runRoomIO, addToQueue
                )
where

--import Network.Socket
import           Client
import Control.Monad (unless)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar,
                                          takeMVar)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8 as BS

data Action = Join Client
            | Leave Client
            | Message Client String

type Actions = [Action]

data Chatroom = Chatroom { roomName :: String
                         , roomId   :: Int
                         , clients  :: [Client]
                         , actions  :: MVar Actions  -- a queue of actions for the chatroom
                         } deriving Eq

runActionIO :: Chatroom -> IO Chatroom
runActionIO cr@(Chatroom _ _ _ mactions ) = do
  acti <- takeMVar mactions
  if null acti then putMVar mactions acti >> return cr --if queue is empty
    else do
    let x:xs = acti -- get head of queue
    putMVar mactions xs -- put remainder back in mutex
    case x of -- preform action
      (Join c) -> return $ addClient cr c -- if shit doesnt work rethink this line - possible mutex fuckery
      (Leave c) -> return $ removeClient cr (Client.id c)
      (Message cl msg) -> broadcast cr (BS.pack msg) >> return cr --TODO include client in message to room

runRoomIO :: Chatroom -> IO ()
runRoomIO cr = do
  newCr <- runActionIO cr
  unless (emptyRoom newCr) $ runRoomIO newCr

addToQueue :: Chatroom -> Action -> IO ()
addToQueue (Chatroom _ _ _ ac) newAct = do
  acti <- takeMVar ac
  putMVar ac (acti ++ [newAct])
  --return $ Chatroom n i cl ac


--deliver message to all clients in room
broadcast :: Chatroom -> B.ByteString -> IO ()
broadcast room msg = mapM_ (messageClient msg) (clients room)

--add a client to the chatroom
addClient :: Chatroom -> Client -> Chatroom
addClient (Chatroom r rid cl a) nc = Chatroom r rid  (nc:cl) a

--remove a client by an id
removeClient :: Chatroom -> Int -> Chatroom
removeClient (Chatroom r rid cl a) cid = Chatroom r rid nc a
  where nc = [x | x <- cl, Client.id x /= cid] -- create a new list of clients

-- create a new chatroom
createRoom :: String -> Int -> [Client] -> MVar Actions -> Chatroom
createRoom = Chatroom

-- returns true if a client is in a chatroom
existsClient :: Chatroom -> Int -> Bool
existsClient (Chatroom _ _ cs _) cid = any (\x -> Client.id x == cid) cs

--check if room has any clients in it
emptyRoom :: Chatroom -> Bool
emptyRoom (Chatroom _ _ c _) = null c
