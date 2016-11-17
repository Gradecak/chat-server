module Utils (updateMutex, parseJoinStr, parseLeaveStr) where

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
