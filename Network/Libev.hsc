{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | @Network.Libev@ is a low-level binding to the libev library
-- (<http://libev.schmorp.de/>). The @libev@ documentation is available here:
-- <http://pod.tst.eu/http://cvs.schmorp.de/libev/ev.pod>.
module Network.Libev
    ( 
    -- * Event loops
    -- | see <http://pod.tst.eu/http://cvs.schmorp.de/libev/ev.pod#FUNCTIONS_CONTROLLING_THE_EVENT_LOOP>

      EvLoopPtr
    , evDefaultLoop
    , evLoopNew
    , evLoop
    , evUnloop
    , evLoopDestroy

    -- ** Flags for 'evDefaultLoop'
    , evRecommendedBackends
    , evflag_auto
    , evflag_noenv
    , evbackend_select
    , evbackend_poll
    , evbackend_epoll
    , evbackend_kqueue
    , evbackend_devpoll
    , evbackend_port

    -- ** Event flags
    , CEventType
    , CEvFlagType
    , ev_read
    , ev_write

    -- * @ev\_io@
    -- | See libev docs:  <http://pod.tst.eu/http://cvs.schmorp.de/libev/ev.pod#code_ev_io_code_is_this_file_descrip>

    , EvIoPtr
    , IoCallback
    , mkEvIo
    , mkEvTimer
    , freeEvIo
    , freeEvTimer
    , mkIoCallback
    , mkTimerCallback
    , freeIoCallback
    , freeTimerCallback
    , evIoInit
    , evIoStart
    , evIoStop

    -- * @ev\_timer@
    -- | See libev docs: <http://pod.tst.eu/http://cvs.schmorp.de/libev/ev.pod#code_ev_timer_code_relative_and_opti>
    , EvTimerPtr
    , TimerCallback

    , evTimerInit
    , evTimerStart
    , evTimerStop

    -- * Time functions
    , EvTimestamp
    , evNow
    , evTime

    -- * C utility functions
    , c_accept
    , c_close
    , c_read
    , c_write
    , c_setnonblocking
    )
    where

import Prelude hiding (repeat)
import System.IO
import Foreign
import Foreign.C

#include <ev.h>

-- | 'CEventType' is a bitfield used to flag whether a file descriptor is
-- readable, writable, or both. Valid values are 'ev_read' and
-- 'ev_write'. TODO: deprecate and replace by a datatype
type CEventType = CInt

-- | 'CEvFlagType' is a bitfield used to pass flags into
-- 'evDefaultLoop'. Values ('evflag_auto', 'evflag_noenv', etc.) are combined
-- with bitwise or. TODO: replace with a newtype with a monoid instance
type CEvFlagType = CInt

#{enum CEvFlagType, ,
   evflag_auto       = EVFLAG_AUTO
 , evflag_noenv      = EVFLAG_NOENV
 , evbackend_select  = EVBACKEND_SELECT
 , evbackend_poll    = EVBACKEND_POLL
 , evbackend_epoll   = EVBACKEND_EPOLL
 , evbackend_kqueue  = EVBACKEND_KQUEUE
 , evbackend_devpoll = EVBACKEND_DEVPOLL
 , evbackend_port    = EVBACKEND_PORT
}

#{enum CEventType, ,
     ev_read  = EV_READ
   , ev_write = EV_WRITE
}

data EvLoop
type EvLoopPtr    = Ptr EvLoop

data EvWatcher
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


-- | An 'IoCallback' is called when a file descriptor becomes readable or
-- writable. It takes a pointer to an @ev\_loop@ structure, a pointer to an
-- @ev\_io@ structure, and an event mask.
type IoCallback = EvLoopPtr -> EvIoPtr -> CEventType -> IO ()


-- | A 'TimerCallback' is called when a timer expires. It takes a pointer to an
-- @ev\_loop@ structure, a pointer to an @ev\_io@ structure, and an (unused?)
-- event mask.
type TimerCallback = EvLoopPtr -> EvTimerPtr -> CEventType -> IO ()

-- | Libev timestamp values are C doubles containing the (floating) number of
-- seconds since Jan 1, 1970.
type EvTimestamp = CDouble


-- | Returns the default set of 'CEvFlagType' flags
foreign import ccall unsafe "ev.h ev_recommended_backends" evRecommendedBackends :: IO CEvFlagType


foreign import ccall unsafe "wev_default_loop" evDefaultLoop :: CInt -> IO EvLoopPtr
foreign import ccall "wev_loop" evLoop :: EvLoopPtr -> CInt -> IO ()
foreign import ccall "wev_unloop" evUnloop :: EvLoopPtr -> CInt -> IO ()
foreign import ccall unsafe "wev_loop_new" evLoopNew :: CUInt -> IO EvLoopPtr
foreign import ccall unsafe "wev_loop_destroy" evLoopDestroy :: EvLoopPtr -> IO ()

foreign import ccall unsafe "wev_io_init" evIoInit :: EvIoPtr -> FunPtr IoCallback -> CInt -> CEventType -> IO ()
foreign import ccall unsafe "wev_io_start" evIoStart :: EvLoopPtr -> EvIoPtr -> IO ()
foreign import ccall unsafe "wev_io_stop" evIoStop :: EvLoopPtr -> EvIoPtr -> IO ()

foreign import ccall unsafe "wev_timer_init" evTimerInit :: EvTimerPtr -> FunPtr TimerCallback -> EvTimestamp -> EvTimestamp -> IO ()
foreign import ccall unsafe "wev_timer_start" evTimerStart :: EvLoopPtr -> EvTimerPtr -> IO ()
foreign import ccall unsafe "wev_timer_stop" evTimerStop :: EvLoopPtr -> EvTimerPtr -> IO ()

foreign import ccall unsafe "unistd.h close" c_close :: CInt -> IO (CInt)
foreign import ccall unsafe "unistd.h read" c_read :: CInt -> CString -> CSize -> IO (CSize)
foreign import ccall unsafe "unistd.h write" c_write :: CInt -> CString -> CSize -> IO (CSize)

-- | Calls @accept()@ and sets the socket non-blocking.
foreign import ccall unsafe "c_accept" c_accept :: CInt -> IO (CInt)
foreign import ccall unsafe "set_nonblocking" c_setnonblocking :: CInt -> IO ()

-- | Fetches the current time from the operating system. Usually 'evNow' is
-- preferred since it avoids a context switch by returning a cached value.
foreign import ccall unsafe "ev.h ev_time" evTime :: IO EvTimestamp

-- | Fetch a the cached copy of the current time from a loop.
foreign import ccall unsafe "ev.h ev_now"  evNow  :: EvLoopPtr -> IO EvTimestamp

-----------------------
-- callback wrappers --
-----------------------

-- | Wrap up an 'IoCallback' so it can be delivered into C-land. This resource
-- is not garbage-collected, you are responsible for freeing it with
-- 'freeIoCallback'.
foreign import ccall "wrapper" mkIoCallback :: IoCallback -> IO (FunPtr IoCallback)

-- | Wrap up a 'TimerCallback' so it can be delivered into C-land. This
-- resource is not garbage-collected, you are responsible for freeing it with
-- 'freeTimerCallback'.
foreign import ccall "wrapper" mkTimerCallback :: TimerCallback -> IO (FunPtr TimerCallback)

freeIoCallback :: FunPtr IoCallback -> IO ()
freeIoCallback = freeHaskellFunPtr

freeTimerCallback :: FunPtr TimerCallback -> IO ()
freeTimerCallback = freeHaskellFunPtr

-- mem allocators
-- foreign import ccall unsafe "wmkevio" mkEvIo :: IO (EvIoPtr)
-- foreign import ccall unsafe "wfreeevio" freeEvIo :: EvIoPtr -> IO ()

-- | Makes a new @ev_io@ struct using 'malloc'. You are responsible for freeing
-- it with 'freeEvIo'.
mkEvIo :: IO (EvIoPtr)
mkEvIo = malloc

-- | free() an 'EvIoPtr'
freeEvIo :: EvIoPtr -> IO ()
freeEvIo = free

-- | Makes a new @ev_timer@ struct using 'malloc'. You are responsible for freeing
-- it with 'freeEvTimer'.
mkEvTimer :: IO (EvTimerPtr)
mkEvTimer = malloc

-- | free() an 'EvIoPtr'
freeEvTimer :: EvTimerPtr -> IO ()
freeEvTimer = free
