module Main where

import Network.Libev

{- This is a Haskell port of the demo libev application at
 - http://pod.tst.eu/http://cvs.schmorp.de/libev/ev.pod#EXAMPLE_PROGRAM
 -}

-- This callback is called when data is readable on stdin.
stdinCb :: IoCallback
stdinCb evLoopPtr evIoPtr revents = do
  putStrLn "stdin ready"
  evIoStop evLoopPtr evIoPtr
  evUnloop evLoopPtr 2 {- 2 = EVUNLOOP_ALL -}

-- Another callback, this time for a timeout.
timeoutCb :: TimerCallback
timeoutCb evLoopPtr evIoPtr revents = do
  putStrLn "timeout"
  evUnloop evLoopPtr 1 {- 1 = EVUNLOOP_ONE -}

main = do
  stdinWatcher <- mkEvIo
  timeoutWatcher <- mkEvTimer

  stdinCb_ <- mkIoCallback stdinCb
  timeoutCb_ <- mkTimerCallback timeoutCb

  -- Use the default event loop unless you have special needs
  loop <- evDefaultLoop 0

  -- Initialise an io watcher, then start it.
  -- This one will watch for stdin to become readable
  evIoInit stdinWatcher stdinCb_ {- 0 = STDIN_FILENO -} 0 ev_read
  evIoStart loop stdinWatcher
  
  -- Initialise a timer watcher, then start it.
  -- Simple non-repeating 5.5 second timeout.
  evTimerInit timeoutWatcher timeoutCb_ 10 0.0
  evTimerStart loop timeoutWatcher

  -- Now wait for events to arrive
  evLoop loop 0

  -- Unloop was called, so exit.
  putStrLn "exiting"
