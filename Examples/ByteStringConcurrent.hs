{-# LANGUAGE OverloadedStrings #-}
 
import Data.ByteString.Char8
 
import Network hiding (accept)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Control.Concurrent
 
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 5002
    loop sock
 
loop sock = do
   (conn, _) <- accept sock
   forkIO $ body conn
   loop sock
  where
   body c = do sendAll c msg
               sClose c
 
msg = "HTTP/1.1 200 OK\r\nContent-Length: 13\r\n\r\nHello, World!"
