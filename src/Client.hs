module Client ( Client(..),
                messageClient, setName
              )where

import           Control.Monad             (void)
import           Data.ByteString
import           Network.Socket
import           Network.Socket.ByteString as NB (send)

data Client = Client { name :: String
                     , id   :: Int
                     , sock :: Socket
                     } deriving Eq

-- send a message to the client
messageClient :: ByteString -> Client -> IO()
messageClient m (Client _ _ s)= void $ NB.send s m


setName :: Client -> String -> Client
setName (Client _ i s) n = Client n i s




