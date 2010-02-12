// thnx to:
// http://therning.org/magnus/archives/238

#include <ev.h>
#include <stdio.h>  // puts
#include <stdlib.h> // malloc
#include <string.h> // bzero

#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <netinet/tcp.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>


/* evloop */
struct ev_loop * wev_default_loop (int f) { ev_default_loop(f); }
struct ev_loop * wev_loop_new (unsigned int f) { ev_loop_new(f); }
void wev_loop (struct ev_loop *l, int f) { ev_loop(l,f);}
void wev_unloop(struct ev_loop *l, int h) { ev_unloop(l,h); }
void wev_loop_destroy(struct ev_loop *l) { ev_loop_destroy(l); }

/* ev_io */
void wev_io_init (ev_io *w, void *cb, int fd, int events) { ev_io_init(w,cb,fd,events); }
void wev_io_start (struct ev_loop *l, ev_io *w) { ev_io_start(l, w); }
void wev_io_stop (struct ev_loop *l, ev_io *w) { ev_io_stop(l, w); }
/* void *wmkevio () { size_t s = (sizeof (struct ev_io)); void *ptr = malloc (s); bzero(ptr,s); return ptr;} */
/* void wfreeevio (struct ev_io *evio) { free(evio); } */

/* ev_timer */
void wev_timer_init (ev_timer *w, void *cb, ev_tstamp after, ev_tstamp repeat) 
{ ev_timer_init(w,cb,after,repeat); }
void wev_timer_start (struct ev_loop *l, ev_timer *w) { ev_timer_start(l, w); }
void wev_timer_stop (struct ev_loop *l, ev_timer *w) { ev_timer_stop(l, w); }

/* ev_async */
void wev_async_init (ev_async *w, void *cb) {
    ev_async_init(w,cb);
}
void wev_async_send (struct ev_loop *l, ev_async *w) { ev_async_send(l,w); }
void wev_async_start (struct ev_loop *l, ev_async *w) { ev_async_start(l,w); }
void wev_async_stop (struct ev_loop *l, ev_async *w) { ev_async_stop(l,w); }

void set_nonblocking(int fd)
{
    int flags;
    /* If they have O_NONBLOCK, use the Posix way to do it */
    if (-1 == (flags = fcntl(fd, F_GETFL, 0))) flags = 0;
    int res = fcntl(fd, F_SETFL, flags | O_NONBLOCK);
    assert(res==0);
}

int c_accept(int server_fd) {
  int fd = accept(server_fd, NULL, NULL);
  if (fd == -1 && ( errno == EAGAIN || errno == EWOULDBLOCK) ) return -2;
  if (fd > 0) {
    set_nonblocking(fd);
    return fd; 
  } else {
    return -1;
  }
}

/* test: gcc -g -lev wrapper.c -o wrapper */

/* ev_io stdin_watcher; */
/* ev_timer timeout_watcher; */

/* static void stdin_cb (EV_P_ struct ev_io *w, int revents) { */
/*   puts ("stdin ready"); */
/*   wev_io_stop (EV_A_ w); */
/*   wev_unloop (EV_A_ EVUNLOOP_ALL); */
/* } */

/* static void timeout_cb (EV_P_ struct ev_timer *w, int revents) { */
/*   puts ("timeout"); */
/*   wev_unloop (EV_A_ EVUNLOOP_ONE); */
/* } */

/* int main (int argc, char** argv) { */
/*   struct ev_loop *loop = wev_default_loop(0); */
/*   wev_io_init (&stdin_watcher, stdin_cb, /\*STDIN_FILENO*\/ 0, EV_READ); */
/*   wev_io_start (loop, &stdin_watcher); */

/*   wev_timer_init (&timeout_watcher, timeout_cb, 5.5, 0.); */
/*   wev_timer_start (loop, &timeout_watcher); */

/*   wev_loop (loop, 0); */
/*   return 0; */
/* } */
