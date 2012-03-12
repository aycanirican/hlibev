import Network
import Control.Concurrent
import System.IO
 
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 5002
    loop sock

loop sock = do
   (h,_,_) <- accept sock
   forkIO $ body h
   loop sock
  where
   body h = do
       hPutStr h msg
       hFlush h
       hClose h
 
msg = "HTTP/1.1 200 OK\r\nContent-Length: 13\r\n\r\nHello, World!"
