{-# OPTIONS -ffi #-}

module Network.Libev
    ( evDefaultLoop
    , evLoopNew
    , evLoop
    , evUnloop
    , evLoopDestroy
    -- ev_io
    , mkEvIo
    , mkIoCallback
    , IoCallback (..)
    , evIoInit
    , evIoStart
    , evIoStop
    -- ev_timer
    , evTimerInit
    , evTimerStart
    , evTimerStop
    -- events
    , ev_read
    , ev_write
    , c_accept
    , c_close
    , c_read
    , c_write
    )
    where
import Prelude hiding (repeat)
import System.IO
import Foreign
import Foreign.C

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

data EvIo         = EvIo { fd :: CInt, events :: CInt }
type EvIoPtr      = Ptr EvIo

data EvTimer      = EvTimer { repeat :: Double }
type EvTimerPtr   = Ptr EvTimer

instance Storable EvWatcher where
    sizeOf    _ = (#size struct ev_watcher)
    alignment _ = alignment (undefined :: CInt)

instance Storable EvIo where
    sizeOf    _ = #size struct ev_io
    alignment _ = alignment (undefined :: CInt)
    peek ptr    = do
      fd'       <- (#peek ev_io, fd) ptr
      events'   <- (#peek ev_io, events) ptr
      return EvIo { fd = fd', events = events' }
    poke ptr (EvIo fd' events') = do
      (#poke ev_io, fd) ptr fd'
      (#poke ev_io, events) ptr events'

instance Storable EvTimer where
    sizeOf    _ = (#size struct ev_timer)
    alignment _ = alignment (undefined :: CInt)
    peek ptr    = do
      repeat'   <- (#peek ev_timer, repeat) ptr
      return EvTimer { repeat = repeat' }
    poke ptr (EvTimer repeat') = do
      (#poke ev_timer, repeat) ptr repeat'

-- * Low-level C functions

type IoCallback = EvLoopPtr -> EvIoPtr -> CInt -> IO ()
type TimerCallback = EvLoopPtr -> EvTimerPtr -> CInt -> IO ()

foreign import ccall unsafe "wev_default_loop" evDefaultLoop :: CInt -> IO EvLoopPtr
foreign import ccall "wev_loop" evLoop :: EvLoopPtr -> CInt -> IO ()
foreign import ccall "wev_unloop" evUnloop :: EvLoopPtr -> CInt -> IO ()
foreign import ccall unsafe "wev_loop_new" evLoopNew :: CUInt -> IO EvLoopPtr
foreign import ccall unsafe "wev_loop_destroy" evLoopDestroy :: EvLoopPtr -> IO ()

foreign import ccall unsafe "wev_io_init" evIoInit :: EvIoPtr -> FunPtr IoCallback -> CInt -> CInt -> IO ()
foreign import ccall unsafe "wev_io_start" evIoStart :: EvLoopPtr -> EvIoPtr -> IO ()
foreign import ccall unsafe "wev_io_stop" evIoStop :: EvLoopPtr -> EvIoPtr -> IO ()

foreign import ccall unsafe "wev_timer_init" evTimerInit :: EvTimerPtr -> FunPtr TimerCallback -> CInt -> CInt -> IO ()
foreign import ccall unsafe "wev_timer_start" evTimerStart :: EvLoopPtr -> EvTimerPtr -> IO ()
foreign import ccall unsafe "wev_timer_stop" evTimerStop :: EvLoopPtr -> EvTimerPtr -> IO ()

foreign import ccall unsafe "unistd.h close" c_close :: CInt -> IO (CInt)
foreign import ccall unsafe "unistd.h read" c_read :: CInt -> CString -> CSize -> IO (CSize)
foreign import ccall unsafe "unistd.h write" c_write :: CInt -> CString -> CSize -> IO (CSize)
foreign import ccall unsafe "c_accept" c_accept :: CInt -> IO (CInt)

-- callback wrappers
foreign import ccall "wrapper" mkIoCallback :: IoCallback -> IO (FunPtr IoCallback)

-- mem allocators
-- foreign import ccall unsafe "wmkevio" mkEvIo :: IO (EvIoPtr)
-- foreign import ccall unsafe "wfreeevio" freeEvIo :: EvIoPtr -> IO ()

mkEvIo :: IO (EvIoPtr)
mkEvIo = malloc

mkEvTimer :: IO (EvTimerPtr)
mkEvTimer = malloc