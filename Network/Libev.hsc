{-# OPTIONS -ffi #-}

module Network.Libev
    -- ( evDefaultLoop
    -- , evLoop
    -- , evLoopNew
    -- , evLoopDestroy
    -- , evIoInit
    -- , evIoStart
    -- , evIoStop
    -- , evTimerInit
    -- , evTimerStart
    -- , evTimerStop
    -- )
    where

import System.IO
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc

#include <ev.h>

-- * Marhsaled Data Types

type CEventType = CInt

#{enum CEventType, ,
     ev_read  = EV_READ
   , ev_write = EV_WRITE
}

data EvLoop       = EvLoop
type EvLoopPtr    = Ptr EvLoop

data EvWatcher    = EvWatcher
type EvWatcherPtr = Ptr EvWatcher

data EvIo         = EvIo
type EvIoPtr      = Ptr EvIo

data EvTimer      = EvTimer
type EvTimerPtr   = Ptr EvTimer

instance Storable EvWatcher where
    sizeOf    _ = (#size struct ev_watcher)
    alignment _ = alignment (undefined :: CInt)

instance Storable EvIo where
    sizeOf    _ = (#size struct ev_io)
    alignment _ = alignment (undefined :: CInt)

instance Storable EvTimer where
    sizeOf    _ = (#size struct ev_timer)
    alignment _ = alignment (undefined :: CInt)

-- * Low-level C functions

type IoCallback = EvLoopPtr -> EvIoPtr -> CInt -> IO ()
type TimerCallback = EvLoopPtr -> EvTimerPtr -> CInt -> IO ()

foreign import ccall "wev_default_loop" evDefaultLoop :: CInt -> IO EvLoopPtr
foreign import ccall "wev_loop" evLoop :: EvLoopPtr -> CInt -> IO ()
foreign import ccall unsafe "wev_unloop" evUnloop :: EvLoopPtr -> CInt -> IO ()
foreign import ccall unsafe "wev_loop_new" evLoopNew :: CUInt -> IO EvLoopPtr
foreign import ccall unsafe "wev_loop_destroy" evLoopDestroy :: EvLoopPtr -> IO ()

foreign import ccall "wev_io_init" evIoInit :: EvIoPtr -> FunPtr IoCallback -> CInt -> CInt -> IO ()
foreign import ccall unsafe "wev_io_start" evIoStart :: EvLoopPtr -> EvIoPtr -> IO ()
foreign import ccall unsafe "wev_io_stop" evIoStop :: EvLoopPtr -> EvIoPtr -> IO ()

foreign import ccall "wev_timer_init" evTimerInit :: EvTimerPtr -> FunPtr TimerCallback -> CInt -> CInt -> IO ()
foreign import ccall unsafe "wev_timer_start" evTimerStart :: EvLoopPtr -> EvTimerPtr -> IO ()
foreign import ccall unsafe "wev_timer_stop" evTimerStop :: EvLoopPtr -> EvTimerPtr -> IO ()

-- mem allocators

-- foreign import ccall unsafe "wmkevio" mkEvIo :: IO (EvIoPtr)
-- foreign import ccall unsafe "wfreeevio" freeEvIo :: EvIoPtr -> IO ()

mkEvIo :: IO (EvIoPtr)
mkEvIo = malloc

-- The type of a wrapper stub has to be of the form ft -> IO (FunPtr
-- ft), where ft may be any foreign type.

-- The stub factory wrapCallback turns any Haskell computation of type
-- Callback a into a C function pointer that can be passed to C
-- routines, which can call back into the Haskell context by invoking
-- the referenced function.
foreign import ccall "wrapper" mkIoCallback :: IoCallback -> IO (FunPtr IoCallback)

-- type CallbackFn a = (EvLoopPtr -> EvIoPtr -> CInt -> IO ())

-- mkCallback :: CallbackFn a -> IO (CCallbackPtr a)
-- mkCallback f = wrapCallback $ \l w r -> f l w r

-- test :: IO ()
-- test = do
--   loop <- evLoopNew 0
--   ioW <- mkEvIo
--   ioCB <- mkIoCallback stdinCB
--   evIoInit ioW ioCB 0 ev_read
--   evIoStart loop ioW
--   evLoop loop 0
--   freeHaskellFunPtr ioCB
--   return ()