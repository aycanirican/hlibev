{-# OPTIONS -ffi -fvia-C -O2 #-}
module Main where

import System.IO
import Foreign
import Foreign.Marshal.Alloc
import Foreign.C

import System.Environment (getArgs)

import Network hiding (accept)
import Network.Libev as EV

import Network.Socket -- hiding (send, sendTo, recv, recvFrom)
-- import Network.Socket.ByteString
-- import qualified Data.ByteString as B
import Data.Char (ord)

import Control.Concurrent
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

-- testData = B.pack $ map (fromIntegral . ord) "HTTP/1.1 200 OK\nContent-Type:text/html;charset=UTF-8\n\n<html><body><p>Haskell and libev working together...</p></body></html>\n"
testData = "HTTP/1.1 200 OK\nContent-Type:text/html;charset=UTF-8\n\n<html><body><p>Haskell and libev working together...</p></body></html>\n"

processSocket fd = do
  _ <- allocaBytes 1024 $ \ptr -> c_read fd ptr 1024
  _ <- withCString testData $ \str -> c_write fd str 125
  c_close fd
  return ()

connectorCB :: CInt -> IoCallback
connectorCB fd l w _ = do
  evIoStop l w
  free w
  --putStrLn $ "Connector Called on: " ++ show (fdSocket s)
  forkIO (processSocket fd)
  --putStrLn $ "Connector socket closed: " ++ show (fdSocket s)
  return ()

acceptorCB :: Socket -> IoCallback
acceptorCB s l _ _ = do
  --putStrLn "Accepting..."
  fd <- c_accept (fdSocket s)
  --putStrLn "Accepted."
  ioW <- mkEvIo
  ioCB <- mkIoCallback $ connectorCB fd
  evIoInit ioW ioCB fd ev_read
  evIoStart l ioW
  --putStrLn "client callback registered."
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

main :: IO ()
main = do
  [portStr] <- getArgs
  let port = fromIntegral (read portStr :: Int)
  servSock <- listenOn $ PortNumber port
  putStrLn $ "listening on: " ++ show port
  start servSock `finally` sClose servSock
  return ()
