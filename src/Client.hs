module Client ( Client(..),
                messageClient, closeClientSock, clientCloseHandler
              )where

import           Control.Concurrent.MVar   (MVar)
import           Control.Monad             (void)

import           Data.ByteString
import           Network.Socket
import           Network.Socket.ByteString as NB (send)
import           Utils                     (updateMutex)

data Client = Client { name :: String
                     , clientId   :: Int
                     , sock :: Socket
                     } deriving (Eq, Show)

-- send a message to the client
messageClient :: ByteString -> Client -> IO()
messageClient m (Client _ _ s)= void $ NB.send s m

closeClientSock :: Client -> IO ()
closeClientSock cl = close (sock cl)

clientCloseHandler :: MVar Int -> Client -> IO ()
clientCloseHandler mu cl = do
  closeClientSock cl
  updateMutex mu (+1)
