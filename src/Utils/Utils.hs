module Utils (Message(..), ControlMsg(..), updateMutex, parseJoinStr,
              parseLeaveStr, joinedMsg, parseMsgStr,
              roomMsg, leaveMsg
             ) where

import Control.Concurrent (MVar, takeMVar, putMVar)
import Data.List.Split (splitOn)

data Message a = Message a String
               deriving Show

data ControlMsg a = Join  {getCl :: a}
                  | Leave {getCl :: a}
                  deriving Show

--HOF for updating the contents of a mutex atomically
updateMutex :: MVar a -> (a -> a) -> IO ()
updateMutex mv op = do
  x <- takeMVar mv
  putMVar mv (op x)

parseJoinStr :: String -> (String, String) -- returns (Chatroom name, Client name)
parseJoinStr str = (head x, last x)
  where x = map (last . splitOn ":") $ lines str

parseMsgStr :: String -> (String, String)
parseMsgStr str = (head x , x !! (-2))
  where x = map (last . splitOn ":") $ lines str

parseLeaveStr :: String -> String -- returns the name of the room to leave
parseLeaveStr str = head x
  where x = map (last . words) $ lines str

joinedMsg :: String -> String ->  Int -> Int -> String
joinedMsg rName port rId cId    = "JOINED_CHATROOM:" ++rName
                                   ++ "\nSERVER_IP:10.62.0.104"
                                   ++ "\nPORT:" ++ port
                                   ++ "\nROOM_REF:" ++show rId
                                   ++ "\nJOIN_ID:" ++ show cId ++ "\n"

roomMsg :: Int -> String -> String -> String
roomMsg rId cName msg    = "CHAT:" ++ show rId
                            ++ "\nCLIENT_NAME:" ++ cName
                            ++ "\nMESSAGE:" ++ msg ++ "\n\n"

leaveMsg :: Int -> Int -> String
leaveMsg rId cId   = "LEFT_CHATROOM:"++ show rId
                          ++"\nJOIN_ID:"++ show cId ++ "\n"
