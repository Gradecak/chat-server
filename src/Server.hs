module Server where

import qualified Chatroom                  as Cr
import qualified Client                    as Cl
import           Control.Concurrent        (forkFinally, forkIO)
import           Control.Concurrent.MVar   (MVar, newEmptyMVar, newMVar,
                                            putMVar, readMVar, takeMVar)
import           Control.Monad             (unless, void)
import qualified Data.ByteString           as B
import           Data.ByteString.Char8     (pack, unpack)
import           Data.List                 (find, isInfixOf)
import qualified Network.Socket            as Net
import           Network.Socket.ByteString (recv, send)

data Action = Add Cr.Chatroom
            | Remove Cr.Chatroom

addRoom :: Server -> Cr.Chatroom -> IO ()
addRoom srv r = updateMutex (rooms srv) (r:)

removeRoom :: Server -> Cr.Chatroom -> IO ()
removeRoom srv r = updateMutex (rooms srv) (\x -> [y | y <- x, y /= r])

data Server = Server { info    :: String
                     , sock    :: Net.Socket
                     , rooms   :: MVar [Cr.Chatroom]
                     , stop    :: MVar ()
                     , limit   :: MVar Int
                     , actions :: MVar [Action]
                     }

runRoomsIO :: Server -> IO ()
runRoomsIO s@(Server _ _ _ _ _ muAct) = do
  act <- takeMVar muAct
  if null act then putMVar muAct act
    else do
    let x:xs = act -- might fail to pattern match on singleton list [x]
    putMVar muAct xs
    case x of
      (Add c) -> addRoom s c >> runRoomsIO s
      (Remove c) -> removeRoom s c >> runRoomsIO s

-- Some helper functions
findRoom :: Server -> (Cr.Chatroom -> Bool) -> IO (Maybe Cr.Chatroom)
findRoom serv op = do
  rs <- readMVar (rooms serv)
  return $ find op rs

--HOF for updating the contents of a mutex atomically
updateMutex :: MVar a -> (a -> a) -> IO ()
updateMutex mv op = do
  x <- takeMVar mv
  putMVar mv (op x)

parseJoinStr :: String -> (String, String) -- returns (Chatroom name, Client name)
parseJoinStr str = (head x, last x)        -- returns (Chat ref, Message)  if called with msg string
  where x = map (last . words) $ lines str

parseLeaveStr :: String -> String -- returns the name of the room to leave
parseLeaveStr str = head x
  where x = map (last . words) $ lines str

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
            let newCli = Cl.Client {Cl.id=i, Cl.sock = conn}
            _ <- forkFinally (handleClient s newCli) (\_ -> print "poo")
            loop i
