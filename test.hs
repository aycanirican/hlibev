module Main where

import System.IO
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc

import System.Environment (getArgs)

import Network hiding (accept)
import Network.Libev
import Network.Socket (fdSocket, recv, send, accept)
import Network.BSD

import Control.Concurrent
import Control.Concurrent.MVar (putMVar)
import Control.Exception (finally)

-- stdinCB :: IoCallback
-- stdinCB loop watcher revents = do
--   putStrLn "Stdin Ready"
--   evIoStop loop watcher
--   evUnloop loop 0
--   return ()

-- bind :: String -> String -> IO Socket
-- bind node service = do
--   putStrLn "Binding..."
--   addrinfos <- getAddrInfo Nothing (Just node) (Just service)
--   let serveraddr = head addrinfos
--   sock <- socket (addrFamily serveraddr) Stream defaultProtocol
--   bindSocket sock (addrAddress serveraddr)
--   listen sock 10
--   return sock

connectorCB :: Socket -> IoCallback
connectorCB s l w r = do
  putStrLn "Connector Called..."
  evIoStop l w
  free w
  recvdata <- recv s 1024
  senddata <- send s "<html><body><p>Haskell and libev working together...</p></body></html>"
  sClose s
  return ()

acceptorCB :: Socket -> IoCallback
acceptorCB s l w r = do
  putStrLn "Accepting..."
  (sock, sockaddr) <- accept s
  putStrLn "Accepted."
  ioW <- mkEvIo
  ioCB <- mkIoCallback $ connectorCB sock
  evIoInit ioW ioCB (fdSocket sock) ev_read
  evIoStart l ioW
  putStrLn "client callback registered."
  return ()
    
start servSock = withSocketsDo $ do
  loop <- evLoopNew 0
  ioW <- mkEvIo
  ioCB <- mkIoCallback $ acceptorCB servSock
  evIoInit ioW ioCB (fdSocket servSock) ev_read
  evIoStart loop ioW
  evLoop loop 0
  putStrLn "Loop Done."
  freeHaskellFunPtr ioCB
  return ()

main = do
  [portStr] <- getArgs
  let port = fromIntegral (read portStr :: Int)
  servSock <- listenOn $ PortNumber port
  putStrLn $ "listening on: " ++ show port 
  start servSock `finally` sClose servSock
  -- mvar <- newEmptyMVar
  -- tid <- forkOS (test `finally` putMVar mvar ())
  -- return mvar
