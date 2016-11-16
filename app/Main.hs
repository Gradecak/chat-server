module Main where

import qualified Network.Socket as Net
import           Control.Concurrent        (forkFinally, forkIO)
import           Control.Concurrent.MVar   (MVar, newEmptyMVar, newMVar,
                                                                                        putMVar, takeMVar)
import           System.Environment        (getArgs)
import Server

{- Initialise the socket that will be used for the server-}
initSocket :: String -> String -> IO Net.Socket
initSocket host port = do
  addr:_ <- Net.getAddrInfo Nothing (Just host) (Just port)
  sock <- Net.socket (Net.addrFamily addr) Net.Stream  Net.defaultProtocol-- (Net.addrProtocol addr)
  Net.bind sock (Net.addrAddress addr)
  return sock


main :: IO ()
main = do
  [host, port, n] <- getArgs
  sock <- initSocket host port -- intialise the server socket
  putStrLn $ "staring server on " ++ host ++ ":" ++ port
  kill <- newEmptyMVar
  _ <- forkIO $ runServer sock  (read n :: Int) ("IP:10.62.0.104\nPort:"++port++"\nStudentID:13319506\n") kill
  takeMVar kill
  putStrLn "Terminating Server"
