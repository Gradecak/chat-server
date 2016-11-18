module Utils (updateMutex, parseJoinStr,
              parseLeaveStr, joinedMessage,
              roomMessage, leftRoomMessage
             ) where

import Control.Concurrent (MVar, takeMVar, putMVar)
import Data.List.Split (splitOn)

--HOF for updating the contents of a mutex atomically
updateMutex :: MVar a -> (a -> a) -> IO ()
updateMutex mv op = do
  x <- takeMVar mv
  putMVar mv (op x)

parseJoinStr :: String -> (String, String) -- returns (Chatroom name, Client name)
parseJoinStr str = (head x, last x)        -- returns (Chat ref, Message)  if called with msg string
  where x = map (last . splitOn ":") $ lines str

parseLeaveStr :: String -> String -- returns the name of the room to leave
parseLeaveStr str = head x
  where x = map (last . splitOn ":") $ lines str

joinedMessage :: String -> String ->  Int -> Int -> String
joinedMessage rName port rId cId    = "JOINED_CHATROOM:" ++rName
                                   ++ "\nSERVER_IP:10.62.0.104"
                                   ++ "\nPORT:" ++ port
                                   ++ "\nROOM_REF:" ++show rId
                                   ++ "\nJOIN_ID:" ++ show cId

roomMessage :: String -> String -> String -> String
roomMessage rId cName msg    = "CHAT:" ++ rId
                            ++ "CLIENT_NAME:" ++ cName
                            ++ "MESSAGE:" ++ msg

leftRoomMessage :: String -> String -> String
leftRoomMessage rId cId   = "LEFT_CHATROOM:"++rId
                          ++"JOIN_ID:"++cId
