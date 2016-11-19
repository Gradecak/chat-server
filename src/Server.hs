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
    (Just r) -> atomically (Cr.addClient r client) >> Cr.broadcast r (Cr.Join client)
    Nothing -> do
      newR <- atomically $ do
        rs <- readTVar tRs
        nRoom <- Cr.newRoom name (Cr.nextId rs)
        Cr.addClient nRoom client
        writeTVar tRs $ nRoom:rs
        return nRoom
      Cr.broadcast newR (Cr.Join client)

leaveRoom :: TVar [Cr.Chatroom] -> Cl.Client -> String -> IO()
leaveRoom tRs client rName = do
  maybRoom <- Cr.findRoom tRs (\x -> Cr.roomName x == rName)
  case maybRoom of
    (Just r) -> Cr.removeClient r client >> Cr.broadcast r (Cr.Leave client)
    Nothing  -> return ()

messageRoom :: TVar [Cr.Chatroom] -> Cl.Client -> Int -> String -> IO()
messageRoom tRs client rId msg = do
  maybRoom <- Cr.findRoom tRs (\x -> Cr.roomId x == rId)
  case maybRoom of
    (Just r) -> Cr.broadcast r (Cr.Message client msg)
    Nothing -> return ()

action ::(TVar [Cr.Chatroom],MVar ()) -> Cl.Client -> String -> IO ()
action inf cl msg | "HELO"    `isInfixOf`   msg = void $ send (Cl.sock cl) (pack $ msg ++ "poo")
                  | "JOIN"    `isInfixOf`   msg =  let (rName,name) = parseJoinStr msg
                                                       client = Cl.setName cl name
                                                   in joinRoom (fst inf) client rName
                  | "LEAVE"   `isInfixOf`   msg = leaveRoom (fst inf) cl msg
                  | "MESSAGE" `isInfixOf`   msg = let (rId, m) = parseMsgStr msg -- return () --TODO implement room messaging
                                                  in messageRoom (fst inf) cl (read rId :: Int) m
                  | "KILL_SERVICE"       == msg = putMVar (snd inf) ()
                  | "DISCONNECT" `isInfixOf`msg = exitSuccess
                  | otherwise                   = return () -- do nothing

clientHandler :: (TVar [Cr.Chatroom],MVar ()) -> Cl.Client -> IO ()
clientHandler mRooms c@(Cl.Client _ _ sck ) = do
  msg <- recv sck 4096
  print msg
  unless (B.null msg) $ action mRooms c (unpack msg) >> clientHandler mRooms c

runServer :: Server -> IO ()
runServer (Server _ soc rs kill lim) = do
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
            _ <- forkFinally (clientHandler (rs,kill) newCli) (\_ -> Cl.clientCloseHandler lim newCli)
            loop (i+1)
