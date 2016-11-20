module Server (Server(..), runServer)  where

import           Chatroom
import           Client                      as Cl
import           Control.Concurrent          (forkFinally)
import           Control.Concurrent.MVar     (MVar, putMVar, readMVar)
import           Control.Concurrent.STM.TVar
import Control.Monad.STM
import           Control.Monad               (unless, void, filterM)
import qualified Data.ByteString             as B
import           Data.ByteString.Char8       (pack, unpack)
import           Data.List                   (isInfixOf)
import qualified Network.Socket              as Net
import           Network.Socket.ByteString   (recv, send)
import           System.Exit                 (exitSuccess)
import           Utils

data Server = Server { info      :: String
                     , sock      :: Net.Socket
                     , roomsList :: TVar [Chatroom]
                     , stop      :: MVar ()
                     , limit     :: MVar Int
                     }

joinRoom :: TVar [Chatroom] -> Client -> String -> IO ()
joinRoom tRs client rName = do
  maybRoom <- findRoom tRs (\x -> roomName x == rName)
  case maybRoom of
    (Just r) -> notifyClient r (Join client) >> addClient r client
    Nothing -> do
      newR <- newRoom tRs rName
      notifyClient newR (Join client)
      addClient newR client

leaveRoom :: TVar [Chatroom] -> Client -> Int -> IO()
leaveRoom tRs client rId = do
  maybRoom <- findRoom tRs (\x -> roomId x == rId)
  case maybRoom of
    (Just r) -> notifyClient r (Leave client) >> removeClient client r
    Nothing  -> return ()

messageRoom :: TVar [Chatroom] -> Client -> Int -> String -> IO()
messageRoom tRs client rId msg = do
  print msg
  print (show rId)
  maybRoom <- findRoom tRs (\x -> roomId x == rId)
  case maybRoom of
    (Just r) -> print "found room" >> broadcast r (Message client msg)
    Nothing  -> print "rip room" >> return ()

action ::(TVar [Chatroom],MVar ()) -> Client -> String -> String -> IO ()
action (rooms, kill) cl msg inf  | "HELO"    `isInfixOf`   msg = void $ send (Cl.sock cl) (pack $ msg ++ inf)
                                 | "MESSAGE" `isInfixOf`   msg = messageAction rooms cl $ parseMsgStr msg
                                 | "LEAVE"   `isInfixOf`   msg = leaveAction rooms cl $ parseLeaveStr msg
                                 | "JOIN"    `isInfixOf`   msg = joinAction rooms cl $ parseJoinStr msg
                                 | "KILL_SERVICE\n"     == msg = print "shutting down server..." >> putMVar kill ()
                                 | "DISCONNECT" `isInfixOf`msg = disconnectAction rooms cl{name=parseDisconnect msg}
                                 | otherwise                   = return () -- do nothing

joinAction :: TVar [Chatroom] -> Client -> (String,String) -> IO ()
joinAction rooms client (rName,cName) = joinRoom rooms client{name=cName} rName

disconnectAction :: TVar [Chatroom] -> Client -> IO ()
disconnectAction rooms client = do
  rs <- atomically $ readTVar rooms
  x <- filterM (`existsClient` client) rs
  mapM_ (removeClient client) (reverse x) -- have to reverse the list to pass the test...
  exitSuccess

leaveAction :: TVar [Chatroom] -> Client -> (String,String) -> IO()
leaveAction rooms client (rId, cName) = leaveRoom rooms client{name=cName} (read rId :: Int)

messageAction :: TVar [Chatroom] -> Client -> (String,String,String) -> IO ()
messageAction rooms client (rId,cName,msg) = messageRoom rooms client{name=cName} (read rId :: Int) msg

clientHandler :: (TVar [Chatroom],MVar ()) -> Client -> String -> IO ()
clientHandler mRooms c@(Client _ _ sck ) inf= do
  msg <- recv sck 4096
  print msg
  unless (B.null msg) $ action mRooms c (unpack msg) inf >> clientHandler mRooms c inf

runServer :: Server -> IO ( )
runServer (Server inf soc rs kill lim) = do
  Net.listen soc 3
  loop 0 -- id of the initial client
  where loop i = do
          (conn,_) <- Net.accept soc
          x <- readMVar lim
          if x == 0
            then Net.close conn >> loop i
            else do
            updateMutex lim (\z -> z-1)
            let newCli = Client {clientId=i, Cl.sock=conn, name="<TEMP>"}
            _ <- forkFinally (clientHandler (rs,kill) newCli inf) (\_ -> clientCloseHandler lim newCli)
            loop (i+1)
