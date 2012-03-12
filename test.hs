{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import System.IO
import Foreign
import Foreign.Marshal.Alloc
import Foreign.C
import System.Environment (getArgs)
import Network hiding (accept)
import Network.Libev as EV

import Network.Socket (fdSocket)

import Control.Concurrent
import Control.Exception (finally)

-- stdinCB :: IoCallback
-- stdinCB loop watcher revents = do
--   putStrLn "Stdin Ready"
--   evIoStop loop watcher
--   evUnloop loop 0
--   return ()

testData = "HTTP/1.1 200 OK\nContent-Length: 13\nContent-Type:text/plain;charset=utf-8\n\nHello, World!\n"

data Server = 
    Server { serverManager :: !EvLoopPtr
           , serverSocket :: !Socket
           , serverResponse :: !CString
           }

processSocket fd resp = do
  _ <- allocaBytes 256 $ \ptr -> c_read fd ptr 256
  _ <- c_write fd resp 89
  c_close fd
  return ()

connectorCB :: CInt -> CString -> IoCallback
connectorCB fd resp l w _ = do
  evIoStop l w
  free w
  forkIO (processSocket fd resp)
  return ()

acceptorCB :: Socket -> CString -> IoCallback
acceptorCB s resp l _ _ = do
  fd <- c_accept (fdSocket s)
  ioW <- mkEvIo
  ioCB <- mkIoCallback $ connectorCB fd resp
  evIoInit ioW ioCB fd ev_read
  evIoStart l ioW
  return ()

start server = withSocketsDo $ do
  ioW <- mkEvIo
  ioCB <- mkIoCallback $ acceptorCB sock resp
  evIoInit ioW ioCB (fdSocket sock) ev_read
  evIoStart loop ioW
  evLoop loop 0
  freeHaskellFunPtr ioCB
  return ()
    where
      loop = serverManager server
      sock = serverSocket server
      resp = serverResponse server
 
main :: IO ()
main = do
  port:_ <- getArgs 
  resp <- newCString testData
  sock <- listenOn $ PortNumber $ fromIntegral (read port :: Int)
  em <- evLoopNew 0
  let server = Server em sock resp
  start server `finally` sClose sock
  return ()
