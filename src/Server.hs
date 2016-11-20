module Server (Server(..), runServer)  where

import qualified Chatroom                    as Cr
import qualified Client                      as Cl
import           Control.Concurrent          (forkFinally, forkIO)
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
                     , rooms :: TVar [Cr.Chatroom]
                     , stop  :: MVar ()
                     , limit :: MVar Int
                     }

joinRoom :: TVar [Cr.Chatroom] -> Cl.Client -> String -> IO ()
joinRoom tRs client name = do
  room <- Cr.findRoom tRs (\x -> Cr.roomName x == name)
  case room of
    (Just r) -> atomically (Cr.addClient r client) >> Cr.notifyClient r (Cr.Join client)
    Nothing -> do
      newR <- atomically $ do
        rs <- readTVar tRs
        nRoom <- Cr.newRoom name (Cr.nextId rs)
        Cr.addClient nRoom client
        writeTVar tRs $ nRoom:rs
        return nRoom
      Cr.notifyClient newR (Cr.Join client)
      Cr.broadcast newR (Cr.Message client (Cl.name client ++ " has joined this chatroom."))
      print $ " joined room " ++ (Cr.roomName newR)

leaveRoom :: TVar [Cr.Chatroom] -> Cl.Client -> Int -> IO()
leaveRoom tRs client rId = do
  print $ "leaving room" ++ (show rId)
  maybRoom <- Cr.findRoom tRs (\x -> Cr.roomId x == rId)
  case maybRoom of
    (Just r) -> Cr.notifyClient r (Cr.Leave client)
    Nothing  -> return ()

messageRoom :: TVar [Cr.Chatroom] -> Cl.Client -> Int -> String -> IO()
messageRoom tRs client rId msg = do
  maybRoom <- Cr.findRoom tRs (\x -> Cr.roomId x == rId)
  case maybRoom of
    (Just r) -> Cr.broadcast r (Cr.Message client msg)
    Nothing -> return ()

action ::(TVar [Cr.Chatroom],MVar ()) -> Cl.Client -> String -> String -> IO ()
action inf cl msg q  | "HELO"    `isInfixOf`   msg = void $ send (Cl.sock cl) (pack $ msg ++ q)
                     | "LEAVE"   `isInfixOf`   msg = leaveRoom (fst inf) cl (read $ parseLeaveStr msg :: Int)
                     | "JOIN"    `isInfixOf`   msg =  let (rName,name) = parseJoinStr msg
                                                          client = Cl.setName cl name
                                                      in joinRoom (fst inf) client rName
                     | "MESSAGE" `isInfixOf`   msg = let (rId, m) = parseMsgStr msg -- return () --TODO implement room messaging
                                                     in messageRoom (fst inf) cl (read rId :: Int) m
                     | "KILL_SERVICE\n"       == msg = print "shutting down server " >> putMVar (snd inf) ()
                     | "DISCONNECT" `isInfixOf`msg = exitSuccess
                     | otherwise                   = return () -- do nothing

clientHandler :: (TVar [Cr.Chatroom],MVar ()) -> Cl.Client -> String -> IO ()
clientHandler mRooms c@(Cl.Client _ _ sck ) inf= do
  msg <- recv sck 4096
  print msg
  unless (B.null msg) $ action mRooms c (unpack msg) inf >> clientHandler mRooms c inf

runServer :: Server -> IO ()
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
            let newCli = Cl.Client {Cl.id=i, Cl.sock=conn}
            _ <- forkFinally (clientHandler (rs,kill) newCli inf) (\_ -> Cl.clientCloseHandler lim newCli)
            loop (i+1)
