/*
 * signals.h - header file for signals handling code
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 1992-1997 Paul Falstad
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Paul Falstad or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Paul Falstad and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Paul Falstad and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Paul Falstad and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#define SIGNAL_HANDTYPE RETSIGTYPE (*)_((int))

#ifndef HAVE_KILLPG
# define killpg(pgrp,sig) kill(-(pgrp),sig)
#endif

#define SIGZERR   (SIGCOUNT+1)
#define SIGDEBUG  (SIGCOUNT+2)
#define VSIGCOUNT (SIGCOUNT+3)
#define SIGEXIT    0

#ifdef SV_BSDSIG
# define SV_INTERRUPT SV_BSDSIG
#endif

/* If not a POSIX machine, then we create our *
 * own POSIX style signal sets functions.     */
#ifndef POSIX_SIGNALS
# define sigemptyset(s)    (*(s) = 0)
# if NSIG == 32
#  define sigfillset(s)    (*(s) = ~(sigset_t)0, 0)
# else
#  define sigfillset(s)    (*(s) = (1 << NSIG) - 1, 0)
# endif
# define sigaddset(s,n)    (*(s) |=  (1 << ((n) - 1)), 0)
# define sigdelset(s,n)    (*(s) &= ~(1 << ((n) - 1)), 0)
# define sigismember(s,n)  ((*(s) & (1 << ((n) - 1))) != 0)
#endif   /* ifndef POSIX_SIGNALS */
 
#define child_block()      signal_block(signal_mask(SIGCHLD))
#define child_unblock()    signal_unblock(signal_mask(SIGCHLD))
#define child_suspend(S)   signal_suspend(SIGCHLD, S)

/* ignore a signal */
#define signal_ignore(S)   signal(S, SIG_IGN)

/* return a signal to it default action */
#define signal_default(S)  signal(S, SIG_DFL)

/* Use a circular queue to save signals caught during    *
 * critical sections of code.  You call queue_signals to *
 * start queueing, and unqueue_signals to process the    *
 * queue and stop queueing.  Since the kernel doesn't    *
 * queue signals, it is probably overkill for zsh to do  *
 * this, but it shouldn't hurt anything to do it anyway. */

/* Right now I'm queueing all signals, but maybe we only *
 * need to queue SIGCHLD.  Anybody know?                 */

#define MAX_QUEUE_SIZE 16

#define queue_signals()    (queueing_enabled++)

#define unqueue_signals()  do { \
    DPUTS(!queueing_enabled, "BUG: unqueue_signals called but not queueing"); \
    if (!--queueing_enabled) { \
	while (queue_front != queue_rear) {      /* while signals in queue */ \
	    sigset_t oset; \
	    queue_front = (queue_front + 1) % MAX_QUEUE_SIZE; \
	    oset = signal_setmask(signal_mask_queue[queue_front]); \
	    handler(signal_queue[queue_front]);  /* handle queued signal   */ \
	    signal_setmask(oset); \
	} \
    } \
} while (0)
