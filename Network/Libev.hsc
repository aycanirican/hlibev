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

    -- ** EVLOOP_*, EVUNLOOP_* flags
    , CEvLoopFlagType
    , evloop_nonblock
    , evloop_oneshot
    , CEvUnloopFlagType
    , evunloop_cancel
    , evunloop_one
    , evunloop_all

    -- ** EVFLAG_* flags
    , CEvFlagType
    , evRecommendedBackends
    , evflag_auto
    , evflag_noenv
    , evflag_forkcheck
    , evflag_noinotify
    , evflag_nosigfd
    , evflag_signalfd

    -- ** EVBACKEND_* flags
    , CEvBackendFlagType
    , evbackend_select
    , evbackend_poll
    , evbackend_epoll
    , evbackend_kqueue
    , evbackend_devpoll
    , evbackend_port
    , evbackend_all

    -- ** Locking for event loops
    , MutexCallback
    , setupLockingForLoop
    , freeMutexCallback

    -- ** Event flags
    , CEventType
    , ev_undef
    , ev_none
    , ev_read
    , ev_write
    , ev__iofdset
    , ev_io
    , ev_timeout
    , ev_timer
    , ev_periodic
    , ev_signal
    , ev_child
    , ev_stat
    , ev_idle
    , ev_prepare
    , ev_check
    , ev_embed
    , ev_fork
    , ev_async
    , ev_custom
    , ev_error

    -- * @ev\_io@
    -- | See libev docs:  <http://pod.tst.eu/http://cvs.schmorp.de/libev/ev.pod#code_ev_io_code_is_this_file_descrip>

    , EvIoPtr
    , IoCallback
    , mkEvIo
    , freeEvIo
    , mkIoCallback
    , freeIoCallback
    , evIoInit
    , evIoStart
    , evIoStop

    -- * @ev\_timer@
    -- | See libev docs: <http://pod.tst.eu/http://cvs.schmorp.de/libev/ev.pod#code_ev_timer_code_relative_and_opti>
    , EvTimer
    , EvTimerPtr
    , TimerCallback
    , mkEvTimer
    , freeEvTimer
    , mkTimerCallback
    , freeTimerCallback
    , evTimerInit
    , evTimerStart
    , evTimerStop
    , evTimerAgain
    , evTimerRemaining
    , evTimerSetRepeat

    -- * @ev\_async@@
    -- | See libev docs: <http://pod.tst.eu/http://cvs.schmorp.de/libev/ev.pod#code_ev_async_code_how_to_wake_up_an>
    , EvAsyncPtr
    , AsyncCallback
    , mkEvAsync
    , freeEvAsync
    , evAsyncInit
    , evAsyncSend
    , evAsyncStart
    , evAsyncStop
    , mkAsyncCallback
    , freeAsyncCallback

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

import Control.Concurrent.MVar
import Prelude hiding (repeat)
import Foreign
import Foreign.C

#define EV_COMPAT3 1
#include <ev.h>

-- | 'CEventType' is a bitfield used to flag whether a file descriptor is
-- readable, writable, or both. Valid values are 'ev_read' and
-- 'ev_write'. TODO: deprecate and replace by a datatype
type CEventType = CInt

-- | eventmask, revents, events...
#{enum CEventType, ,
   ev_undef    = EV_UNDEF
 , ev_none     = EV_NONE
 , ev_read     = EV_READ
 , ev_write    = EV_WRITE
 , ev__iofdset = EV__IOFDSET
 , ev_io       = EV_IO
 , ev_timeout  = EV_TIMEOUT
 , ev_timer    = EV_TIMER
 , ev_periodic = EV_PERIODIC
 , ev_signal   = EV_SIGNAL
 , ev_child    = EV_CHILD
 , ev_stat     = EV_STAT
 , ev_idle     = EV_IDLE
 , ev_prepare  = EV_PREPARE
 , ev_check    = EV_CHECK
 , ev_embed    = EV_EMBED
 , ev_fork     = EV_FORK
 , ev_async    = EV_ASYNC
 , ev_custom   = EV_CUSTOM
 , ev_error    = EV_ERROR
}

-- | 'CEvFlagType' is a bitfield used to pass flags into
-- 'evDefaultLoop'. Values ('evflag_auto', 'evflag_noenv', etc.) are combined
-- with bitwise or. TODO: replace with a newtype with a monoid instance
type CEvFlagType = CInt

#{enum CEvFlagType, ,
   evflag_auto       = EVFLAG_AUTO
 , evflag_noenv      = EVFLAG_NOENV
 , evflag_forkcheck  = EVFLAG_FORKCHECK
 , evflag_noinotify  = EVFLAG_NOINOTIFY
 , evflag_nosigfd    = EVFLAG_NOSIGFD
 , evflag_signalfd   = EVFLAG_SIGNALFD
}

type CEvBackendFlagType = CInt

#{enum CEvBackendFlagType, ,
   evbackend_select  = EVBACKEND_SELECT
 , evbackend_poll    = EVBACKEND_POLL
 , evbackend_epoll   = EVBACKEND_EPOLL
 , evbackend_kqueue  = EVBACKEND_KQUEUE
 , evbackend_devpoll = EVBACKEND_DEVPOLL
 , evbackend_port    = EVBACKEND_PORT
 , evbackend_all     = EVBACKEND_ALL
}

type CEvLoopFlagType = CInt
#{enum CEvLoopFlagType, , 
   evloop_nonblock = EVLOOP_NONBLOCK
 , evloop_oneshot  = EVLOOP_ONESHOT
}

type CEvUnloopFlagType = CInt
#{enum CEvUnloopFlagType, ,
   evunloop_cancel = EVUNLOOP_CANCEL
 , evunloop_one    = EVUNLOOP_ONE
 , evunloop_all    = EVUNLOOP_ALL
}


data EvLoop
type EvLoopPtr    = Ptr EvLoop

data EvWatcher
type EvWatcherPtr = Ptr EvWatcher

data EvIo         = EvIo { fd :: CInt, events :: CInt }
type EvIoPtr      = Ptr EvIo

data EvTimer      = EvTimer { repeat :: Double }
type EvTimerPtr   = Ptr EvTimer

data EvAsync
type EvAsyncPtr   = Ptr EvAsync

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

instance Storable EvAsync where
    sizeOf    _ = (#size struct ev_async)
    alignment _ = alignment (undefined :: CInt)


-- | An 'IoCallback' is called when a file descriptor becomes readable or
-- writable. It takes a pointer to an @ev\_loop@ structure, a pointer to an
-- @ev\_io@ structure, and an event mask.
type IoCallback = EvLoopPtr -> EvIoPtr -> CEventType -> IO ()

-- | A 'TimerCallback' is called when a timer expires. It takes a pointer to an
-- @ev\_loop@ structure, a pointer to an @ev\_timer@ structure, and an (unused?)
-- event mask.
type TimerCallback = EvLoopPtr -> EvTimerPtr -> CEventType -> IO ()

-- | An 'AsyncCallback' is called when you wakeup an event loop with
-- @ev_async_send@
type AsyncCallback = EvLoopPtr -> EvAsyncPtr -> CEventType -> IO ()

-- | 'MutexCallback' is called by @ev\_set\_loop\_release\_cb@
type MutexCallback = EvLoopPtr -> IO ()

-- | Libev timestamp values are C doubles containing the (floating) number of
-- seconds since Jan 1, 1970.
type EvTimestamp = CDouble

-- | Set up the given loop for mutex locking from haskell-land -- if you want
-- to touch the loop from other Haskell threads, you'll need to do this. The
-- two FunPtr objects returned need to be explicitly freed with
-- 'freeMutexCallback'.
--
-- IMPORTANT: if you want multithreaded access to an 'EvLoopPtr', you'll have
-- to acquire the 'MVar' returned here (using 'withMVar') whenever you call any
-- of the @ev@ functions. Very bad C-land crash\/bang\/boom could otherwise
-- result.
--
-- ALSO IMPORTANT: any changes you make to an 'EvLoopPtr' from another thread
-- while the event loop thread is blocked inside @ev\_loop()@ will NOT take
-- effect until the the event loop thread unblocks. You'll need to set up an
-- @ev\_async@ watcher in order to wake up the event loop thread.
setupLockingForLoop :: EvLoopPtr
                    -> IO (FunPtr MutexCallback, FunPtr MutexCallback, MVar ())
setupLockingForLoop loop = do
    mvar <- newMVar ()

    acq <- mkMutexCallback $ acquire mvar
    rel <- mkMutexCallback $ release mvar

    evSetLoopReleaseCB loop rel acq

    return (rel, acq, mvar)
  where
    release mvar _ = putMVar mvar ()
    acquire mvar _ = takeMVar mvar


foreign import ccall safe "ev_set_loop_release_cb"
        evSetLoopReleaseCB :: EvLoopPtr
                           -> FunPtr MutexCallback
                           -> FunPtr MutexCallback
                           -> IO ()

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

foreign import ccall unsafe "wev_async_init" evAsyncInit :: EvAsyncPtr
                                                         -> FunPtr AsyncCallback
                                                         -> IO ()

foreign import ccall unsafe "wev_async_send" evAsyncSend :: EvLoopPtr
                                                         -> EvAsyncPtr
                                                         -> IO ()

foreign import ccall unsafe "wev_async_start" evAsyncStart :: EvLoopPtr
                                                           -> EvAsyncPtr
                                                           -> IO ()

foreign import ccall unsafe "wev_async_stop" evAsyncStop :: EvLoopPtr
                                                         -> EvAsyncPtr
                                                         -> IO ()

foreign import ccall unsafe "wev_timer_init" evTimerInit :: EvTimerPtr -> FunPtr TimerCallback -> EvTimestamp -> EvTimestamp -> IO ()
foreign import ccall unsafe "wev_timer_set" evTimerSet :: EvTimerPtr -> EvTimestamp -> EvTimestamp -> IO ()
foreign import ccall unsafe "wev_timer_start" evTimerStart :: EvLoopPtr -> EvTimerPtr -> IO ()
foreign import ccall unsafe "wev_timer_stop" evTimerStop :: EvLoopPtr -> EvTimerPtr -> IO ()
foreign import ccall unsafe "wev_timer_again" evTimerAgain :: EvLoopPtr -> EvTimerPtr -> IO ()
foreign import ccall unsafe "wev_timer_remaining" evTimerRemaining :: EvLoopPtr -> EvTimerPtr -> IO (EvTimestamp)

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
foreign import ccall "wrapper" mkIoCallback :: IoCallback
                                            -> IO (FunPtr IoCallback)

-- | Wrap up a 'TimerCallback' so it can be delivered into C-land. This
-- resource is not garbage-collected, you are responsible for freeing it with
-- 'freeTimerCallback'.
foreign import ccall "wrapper" mkTimerCallback :: TimerCallback
                                               -> IO (FunPtr TimerCallback)

-- | Wrap up an 'AsyncCallback' so it can be delivered into C-land. This
-- resource is not garbage-collected, you are responsible for freeing it with
-- 'freeAsyncCallback'.
foreign import ccall "wrapper" mkAsyncCallback :: AsyncCallback
                                               -> IO (FunPtr AsyncCallback)


foreign import ccall "wrapper" mkMutexCallback :: MutexCallback
                                               -> IO (FunPtr MutexCallback)

freeIoCallback :: FunPtr IoCallback -> IO ()
freeIoCallback = freeHaskellFunPtr

freeMutexCallback :: FunPtr MutexCallback -> IO ()
freeMutexCallback = freeHaskellFunPtr

freeTimerCallback :: FunPtr TimerCallback -> IO ()
freeTimerCallback = freeHaskellFunPtr

freeAsyncCallback :: FunPtr AsyncCallback -> IO ()
freeAsyncCallback = freeHaskellFunPtr

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

evTimerSetRepeat :: EvTimerPtr -> Double -> IO ()
evTimerSetRepeat p t = do
  evtimer <- peek p
  poke p evtimer { repeat = t }

-- | free() an 'EvTimer'
freeEvTimer :: EvTimerPtr -> IO ()
freeEvTimer = free

-- | Makes a new @ev_async@ struct using 'malloc'. You are responsible for
-- freeing it with 'freeEvAsync'.
mkEvAsync :: IO (EvAsyncPtr)
mkEvAsync = malloc

-- | free() an 'EvAsync'
freeEvAsync :: EvAsyncPtr -> IO ()
freeEvAsync = free
