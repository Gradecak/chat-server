module Server where

import qualified Chatroom                  as Cr
import qualified Chatroom.Manager          as Cm
import qualified Client                    as Cl
import           Control.Concurrent        (forkFinally, forkIO)
import           Control.Concurrent.MVar   (MVar, putMVar, readMVar)
import           Control.Monad             (unless, void)
import qualified Data.ByteString           as B
import           Data.ByteString.Char8     (pack, unpack)
import           Data.List                 (isInfixOf)
import qualified Network.Socket            as Net
import           Network.Socket.ByteString (recv, send)
import           Utils

data Server = Server { info  :: String
                     , sock  :: Net.Socket
                     , stop  :: MVar ()
                     , limit :: MVar Int
                     }

messageRoom :: Cm.Manager -> Cl.Client -> (String, String) -> IO ()
messageRoom mgr cl (rid, msg) = do
  let roomID = read rid :: Int
  maybRoom <- Cm.findRoom mgr ((roomID ==). Cr.roomId)
  case maybRoom of
    (Just r) -> Cr.addToQueue r (Cr.Message cl msg)
    Nothing  -> return ()

joinRoom :: Cm.Manager -> Cl.Client -> String -> IO ()
joinRoom mgr cli msg = do
  let (rName, clName) = parseJoinStr msg
      namedCli = Cl.setName cli clName
  maybRoom <- Cm.findRoom mgr ((rName ==). Cr.roomName)
  case maybRoom of
    (Just r ) -> do
      Cr.addToQueue r (Cr.Join namedCli )
      Cr.addToQueue r (Cr.Message namedCli (joinedMessage rName "5000" (Cr.roomId r) (Cl.id cli)) )
    Nothing   -> Cm.addToQueue mgr (Cm.Create rName namedCli)

leaveRoom :: Cm.Manager -> Cl.Client -> String -> IO ()
leaveRoom mgr cli msg = do
  let rName = parseLeaveStr msg
  maybRoom <- Cm.findRoom mgr ((rName ==) . Cr.roomName)
  case maybRoom of
    (Just r ) -> Cr.addToQueue r (Cr.Leave cli )
    Nothing   -> return () -- TODO add error feedback

action :: Cm.Manager -> Cl.Client -> String -> IO ()
action mgr cl msg | "HELO"    `isInfixOf` msg = void $ send (Cl.sock cl) (pack $ msg ++ "poo")
                  | "JOIN"    `isInfixOf` msg = joinRoom mgr cl msg
                  | "LEAVE"   `isInfixOf` msg = leaveRoom mgr cl msg
                  | "MESSAGE" `isInfixOf` msg = return () --TODO implement room messaging
                  | "KILL_SERVICE"     == msg = putMVar (Cm.kill mgr) ()
                  | otherwise                 = return () -- do nothing

clientHandler :: Cm.Manager -> Cl.Client -> IO ()
clientHandler mgr c@(Cl.Client _ _ sck ) = do
  msg <- recv sck 4096
  print msg
  unless (B.null msg) $ action mgr c (unpack msg) >> clientHandler mgr c

runServer :: Server -> IO ()
runServer (Server _ soc kill lim) = do
  Net.listen soc 3
  manager <- Cm.newManager kill
  forkIO (Cm.runManager manager 0)
  loop 0 manager -- id of the initial client
  where loop i m = do
          (conn,_) <- Net.accept soc
          x <- readMVar lim
          if x == 0
            then Net.close conn >> loop i m
            else do
            updateMutex lim (\z -> z-1)
            let newCli = Cl.Client {Cl.id=i, Cl.sock=conn}
            _ <- forkFinally (clientHandler m newCli) (\_ -> Cl.clientCloseHandler lim newCli)
            loop (i+1) m
