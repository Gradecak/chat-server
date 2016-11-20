module Server (Server(..), runServer)  where

import Chatroom
import Client                      as Cl
import           Control.Concurrent          (forkFinally)
import           Control.Concurrent.MVar     (MVar, putMVar, readMVar)
import           Control.Concurrent.STM.TVar
import           Control.Monad               (unless, void)
import           Control.Monad.STM
import qualified Data.ByteString             as B
import           Data.ByteString.Char8       (pack, unpack)
import           Data.List                   (isInfixOf)
import qualified Network.Socket              as Net
import           Network.Socket.ByteString   (recv, send)
import           System.Exit                 (exitSuccess)
import           Utils

data Server = Server { info  :: String
                     , sock  :: Net.Socket
                     , rooms :: TVar [Chatroom]
                     , stop  :: MVar ()
                     , limit :: MVar Int
                     }

joinRoom :: TVar [Chatroom] -> Client -> String -> IO ()
joinRoom tRs client rName = do
  room <- findRoom tRs (\x -> roomName x == rName)
  case room of
    (Just r) -> print "adding to existing room" >> notifyClient r (Join client) >> addClient r client
    Nothing -> do
      newR <- newRoom tRs rName
      notifyClient newR (Join client)
      addClient newR client
      print $ " joined room " ++ roomName newR

leaveRoom :: TVar [Chatroom] -> Client -> Int -> IO()
leaveRoom tRs client rId = do
  print $ "leaving room" ++ show rId
  maybRoom <- findRoom tRs (\x -> roomId x == rId)
  case maybRoom of
    (Just r) -> notifyClient r (Leave client) >> removeClient r client
    Nothing  -> return ()

messageRoom :: TVar [Chatroom] -> Client -> Int -> String -> IO()
messageRoom tRs client rId msg = do
  maybRoom <- findRoom tRs (\x -> roomId x == rId)
  case maybRoom of
    (Just r) -> broadcast r (Message client msg)
    Nothing -> return ()

action ::(TVar [Chatroom],MVar ()) -> Client -> String -> String -> IO ()
action inf cl msg q  | "HELO"    `isInfixOf`   msg = void $ send (Cl.sock cl) (pack $ msg ++ q)
                     | "LEAVE"   `isInfixOf`   msg = let (rId,cName) = parseLeaveStr msg
                                                     in leaveRoom (fst inf) (setName cl cName) (read rId :: Int)

                     | "JOIN"    `isInfixOf`   msg =  let (rName,cName) = parseJoinStr msg
                                                          client = setName cl cName
                                                      in joinRoom (fst inf) client rName

                     | "MESSAGE" `isInfixOf`   msg = let (rId, m) = parseMsgStr msg -- return () --TODO implement room messaging
                                                     in messageRoom (fst inf) cl (read rId :: Int) m
                     | "KILL_SERVICE\n"       == msg = print "shutting down server " >> putMVar (snd inf) ()
                     | "DISCONNECT" `isInfixOf`msg = exitSuccess
                     | otherwise                   = return () -- do nothing

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
            let newCli = Client {clientId=i, Cl.sock=conn, name="t"}
            _ <- forkFinally (clientHandler (rs,kill) newCli inf) (\_ -> clientCloseHandler lim newCli)
            loop (i+1)
