module Server where

import qualified Chatroom                  as Cr
import qualified Chatroom.Manager          as Cm
import qualified Client                    as Cl
import           Control.Concurrent        (forkFinally)
import           Control.Concurrent.MVar   (MVar, putMVar, readMVar, takeMVar)
import           Control.Monad             (unless, void)
import qualified Data.ByteString           as B
import           Data.ByteString.Char8     (pack, unpack)
import           Data.List                 (find, isInfixOf)
import qualified Network.Socket            as Net
import           Network.Socket.ByteString (recv, send)
import qualified Utils                     as Ut

data Server = Server { info  :: String
                     , sock  :: Net.Socket
                     , stop  :: MVar ()
                     , limit :: MVar Int
                     }

messageRoom :: Server -> Cl.Client -> (String, String) -> IO ()
messageRoom srv cl (rid, msg) = do
  let roomID = read rid :: Int
  maybRoom <- findRoom srv ((roomID ==). Cr.roomId)
  case maybRoom of
    (Just r) -> Cr.addToQueue r (Cr.Message cl msg)
    Nothing  -> return ()

joinRoom :: Server -> Cl.Client -> String -> IO ()
joinRoom srv cli msg = do
  let (rName, clName) = parseJoinStr msg
      namedCli = Cl.setName cli clName --TODO maybe check if name is taken in Chatroom
  maybRoom <- findRoom srv ((rName ==). Cr.roomName)
  case maybRoom of
    (Just r ) -> Cr.addToQueue r (Cr.Join namedCli )
    Nothing   -> return () -- TODO implement case new room

leaveRoom :: Server -> Cl.Client -> String -> IO ()
leaveRoom srv cli msg = do
  let rName = parseLeaveStr msg
  maybRoom <- findRoom srv ((rName ==) . Cr.roomName)
  case maybRoom of
    (Just r ) -> Cr.addToQueue r (Cr.Leave cli )
    Nothing   -> return () -- Take no action as room doesnt exist

action :: Server -> Cl.Client -> String -> IO ()
action srv cl msg | "HELO"    `isInfixOf` msg = void $ send (Cl.sock cl) (pack $ msg ++ info srv)
                  | "JOIN"    `isInfixOf` msg = joinRoom srv cl msg
                  | "LEAVE"   `isInfixOf` msg = leaveRoom srv cl msg
                  | "MESSAGE" `isInfixOf` msg = return () --TODO
                  | "KILL_SERVICE" == msg     = putMVar (stop srv) ()
                  | otherwise                 = return () -- do nothing

handleClient :: Server -> Cl.Client -> IO ()
handleClient serv c@(Cl.Client _ _ sck ) = do
  msg <- recv sck 4096
  unless (B.null msg) $ action serv c (unpack msg) >> handleClient serv c

runServer :: Server -> IO ()
runServer s@(Server _ soc _ _ lim _) = do
  Net.listen soc 3
  loop 0 -- id of the initial client
  where loop i = do
          (conn,_) <- Net.accept soc
          x <- takeMVar lim
          putMVar lim x
          if x == 0
            then Net.close conn >> loop i
            else do
            updateMutex lim (\z -> z-1)
            let newCli = Cl.Client {Cl.id=i, Cl.sock=conn}
            _ <- forkFinally (handleClient s newCli) (\_ -> print "poo")
            loop (i+1)
