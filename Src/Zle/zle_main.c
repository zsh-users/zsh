/*
 * zle_main.c - main routines for line editor
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

#include "zle.mdh"
#include "zle_main.pro"

#ifdef HAVE_POLL_H
# include <poll.h>
#endif
#if defined(HAVE_POLL) && !defined(POLLIN) && !defined(POLLNORM)
# undef HAVE_POLL
#endif

/* != 0 if in a shell function called from completion, such that read -[cl]  *
 * will work (i.e., the line is metafied, and the above word arrays are OK). */

/**/
mod_export int incompctlfunc;

/* != 0 if we are in a new style completion function */

/**/
mod_export int incompfunc;

/* != 0 if completion module is loaded */

/**/
mod_export int hascompmod;

/* ZLRF_* flags passed to zleread() */

/**/
int zlereadflags;

/* ZLCON_* flags passed to zleread() */

/**/
int zlecontext;

/* != 0 if we're done editing */

/**/
int done;

/* location of mark */

/**/
int mark;

/* last character pressed */

/**/
mod_export int lastchar;

/* the bindings for the previous and for this key */

/**/
mod_export Thingy lbindk, bindk;

/* insert mode/overwrite mode flag */

/**/
int insmode;

/**/
mod_export int eofchar;

static int eofsent;
static long keytimeout;

#if defined(HAVE_SELECT) || defined(HAVE_POLL)
/* Terminal baud rate */

static int baud;
static long costmult;
#endif

/* flags associated with last command */

/**/
mod_export int lastcmd;

/**/
mod_export Widget compwidget;

/* the status line, and its length */

/**/
mod_export char *statusline;
/**/
mod_export int statusll;

/* The current history line and cursor position for the top line *
 * on the buffer stack.                                          */

/**/
int stackhist, stackcs;

/* != 0 if we are making undo records */

/**/
int undoing;

/* current modifier status */

/**/
mod_export struct modifier zmod;

/* Current command prefix status.  This is normally 0.  Prefixes set *
 * this to 1.  Each time round the main loop, this is checked: if it *
 * is 0, the modifier status is reset; if it is 1, the modifier      *
 * status is left unchanged, and this flag is reset to 0.  The       *
 * effect is that several prefix commands can be executed, and have  *
 * cumulative effect, but any other command execution will clear the *
 * modifiers.                                                        */

/**/
int prefixflag;

/* Number of characters waiting to be read by the ungetkeys mechanism */
/**/
int kungetct;

/**/
mod_export char *zlenoargs[1] = { NULL };

static char **raw_lp, **raw_rp;

#ifdef FIONREAD
static int delayzsetterm;
#endif

/*
 * File descriptors we are watching as well as the terminal fd. 
 * These are all for reading; we don't watch for writes or exceptions.
 */
/**/
int nwatch;		/* Number of fd's we are watching */
/**/
int *watch_fds;		/* The list of fds, not terminated! */
/**/
char **watch_funcs;	/* The corresponding functions to call, normal array */

/* set up terminal */

/**/
mod_export void
zsetterm(void)
{
    struct ttyinfo ti;

#if defined(FIONREAD)
    int val;

    ioctl(SHTTY, FIONREAD, (char *)&val);
    if (val) {
	/*
	 * Problems can occur on some systems when switching from
	 * canonical to non-canonical input.  The former is usually
	 * set while running programmes, but the latter is necessary
	 * for zle.  If there is input in canonical mode, then we
	 * need to read it without setting up the terminal.  Furthermore,
	 * while that input gets processed there may be more input
	 * being typed (i.e. further typeahead).  This means that
	 * we can't set up the terminal for zle *at all* until
	 * we are sure there is no more typeahead to come.  So
	 * if there is typeahead, we set the flag delayzsetterm.
	 * Then getkey() performs another FIONREAD call; if that is
	 * 0, we have finally used up all the typeahead, and it is
	 * safe to alter the terminal, which we do at that point.
	 */
	delayzsetterm = 1;
	return;
    } else
	delayzsetterm = 0;
#endif

/* sanitize the tty */
#ifdef HAS_TIO
    shttyinfo.tio.c_lflag |= ICANON | ECHO;
# ifdef FLUSHO
    shttyinfo.tio.c_lflag &= ~FLUSHO;
# endif
#else				/* not HAS_TIO */
    shttyinfo.sgttyb.sg_flags = (shttyinfo.sgttyb.sg_flags & ~CBREAK) | ECHO;
    shttyinfo.lmodes &= ~LFLUSHO;
#endif

    attachtty(mypgrp);
    ti = shttyinfo;
#ifdef HAS_TIO
    if (unset(FLOWCONTROL))
	ti.tio.c_iflag &= ~IXON;
    ti.tio.c_lflag &= ~(ICANON | ECHO
# ifdef FLUSHO
			| FLUSHO
# endif
	);
# ifdef TAB3
    ti.tio.c_oflag &= ~TAB3;
# else
#  ifdef OXTABS
    ti.tio.c_oflag &= ~OXTABS;
#  else
#   ifdef XTABS
    ti.tio.c_oflag &= ~XTABS;
#   endif
#  endif
# endif
#ifdef ONLCR
    ti.tio.c_oflag |= ONLCR;
#endif
    ti.tio.c_cc[VQUIT] =
# ifdef VDISCARD
	ti.tio.c_cc[VDISCARD] =
# endif
# ifdef VSUSP
	ti.tio.c_cc[VSUSP] =
# endif
# ifdef VDSUSP
	ti.tio.c_cc[VDSUSP] =
# endif
# ifdef VSWTCH
	ti.tio.c_cc[VSWTCH] =
# endif
# ifdef VLNEXT
	ti.tio.c_cc[VLNEXT] =
# endif
	VDISABLEVAL;
# if defined(VSTART) && defined(VSTOP)
    if (unset(FLOWCONTROL))
	ti.tio.c_cc[VSTART] = ti.tio.c_cc[VSTOP] = VDISABLEVAL;
# endif
    eofchar = ti.tio.c_cc[VEOF];
    ti.tio.c_cc[VMIN] = 1;
    ti.tio.c_cc[VTIME] = 0;
    ti.tio.c_iflag |= (INLCR | ICRNL);
 /* this line exchanges \n and \r; it's changed back in getkey
	so that the net effect is no change at all inside the shell.
	This double swap is to allow typeahead in common cases, eg.

	% bindkey -s '^J' 'echo foo^M'
	% sleep 10
	echo foo<return>  <--- typed before sleep returns

	The shell sees \n instead of \r, since it was changed by the kernel
	while zsh wasn't looking. Then in getkey() \n is changed back to \r,
	and it sees "echo foo<accept line>", as expected. Without the double
	swap the shell would see "echo foo\n", which is translated to
	"echo fooecho foo<accept line>" because of the binding.
	Note that if you type <line-feed> during the sleep the shell just sees
	\n, which is translated to \r in getkey(), and you just get another
	prompt. For type-ahead to work in ALL cases you have to use
	stty inlcr.

	Unfortunately it's IMPOSSIBLE to have a general solution if both
	<return> and <line-feed> are mapped to the same character. The shell
	could check if there is input and read it before setting it's own
	terminal modes but if we get a \n we don't know whether to keep it or
	change to \r :-(
	*/

#else				/* not HAS_TIO */
    ti.sgttyb.sg_flags = (ti.sgttyb.sg_flags | CBREAK) & ~ECHO & ~XTABS;
    ti.lmodes &= ~LFLUSHO;
    eofchar = ti.tchars.t_eofc;
    ti.tchars.t_quitc =
	ti.ltchars.t_suspc =
	ti.ltchars.t_flushc =
	ti.ltchars.t_dsuspc = ti.ltchars.t_lnextc = -1;
#endif

#if defined(TTY_NEEDS_DRAINING) && defined(TIOCOUTQ) && defined(HAVE_SELECT)
    if (baud) {			/**/
	int n = 0;

	while ((ioctl(SHTTY, TIOCOUTQ, (char *)&n) >= 0) && n) {
	    struct timeval tv;

	    tv.tv_sec = n / baud;
	    tv.tv_usec = ((n % baud) * 1000000) / baud;
	    select(0, NULL, NULL, NULL, &tv);
	}
    }
#endif

    settyinfo(&ti);
}

static char *kungetbuf;
static int kungetsz;

/**/
void
ungetkey(int ch)
{
    if (kungetct == kungetsz)
	kungetbuf = realloc(kungetbuf, kungetsz *= 2);
    kungetbuf[kungetct++] = ch;
}

/**/
void
ungetkeys(char *s, int len)
{
    s += len;
    while (len--)
	ungetkey(*--s);
}

#if defined(pyr) && defined(HAVE_SELECT)
static int
breakread(int fd, char *buf, int n)
{
    fd_set f;

    FD_ZERO(&f);
    FD_SET(fd, &f);

    return (select(fd + 1, (SELECT_ARG_2_T) & f, NULL, NULL, NULL) == -1 ?
	    EOF : read(fd, buf, n));
}

# define read    breakread
#endif

static int
raw_getkey(int keytmout, char *cptr)
{
    long exp100ths;
    int ret;
#if defined(HAS_TIO) && \
  (defined(sun) || (!defined(HAVE_POLL) && !defined(HAVE_SELECT)))
    struct ttyinfo ti;
#endif
#ifndef HAVE_POLL
# ifdef HAVE_SELECT
    fd_set foofd;
# endif
#endif

    /*
     * Handle timeouts and watched fd's.  We only do one at once;
     * key timeouts take precedence.  This saves tricky timing
     * problems with the key timeout.
     */
    if ((nwatch || keytmout)
#ifdef FIONREAD
	&& ! delayzsetterm
#endif
	) {
	if (!keytmout || keytimeout <= 0)
	    exp100ths = 0;
	else if (keytimeout > 500)
	    exp100ths = 500;
	else
	    exp100ths = keytimeout;
#if defined(HAVE_SELECT) || defined(HAVE_POLL)
	if (!keytmout || exp100ths) {
	    int i, errtry = 0, selret;
# ifdef HAVE_POLL
	    int poll_timeout;
	    int nfds;
	    struct pollfd *fds;
# else
	    int fdmax;
	    struct timeval *tvptr;
	    struct timeval expire_tv;
# endif
# if defined(HAS_TIO) && defined(sun)
	    /*
	     * Yes, I know this is complicated.  Yes, I know we
	     * already have three bits of code to poll the terminal
	     * down below.  No, I don't want to do this either.
	     * However, it turns out on certain OSes, specifically
	     * Solaris, that you can't poll typeahead for love nor
	     * money without actually trying to read it.  But
	     * if we are trying to select (and we need to if we
	     * are watching other fd's) we won't pick that up.
	     * So we just try and read it without blocking in
	     * the time-honoured (i.e. absurdly baroque) termios
	     * fashion.
	     */
	    gettyinfo(&ti);
	    ti.tio.c_cc[VMIN] = 0;
	    settyinfo(&ti);
	    ret = read(SHTTY, cptr, 1);
	    ti.tio.c_cc[VMIN] = 1;
	    settyinfo(&ti);
	    if (ret > 0)
		return 1;
# endif
# ifdef HAVE_POLL
	    nfds = keytmout ? 1 : 1 + nwatch;
	    /* First pollfd is SHTTY, following are the nwatch fds */
	    fds = zalloc(sizeof(struct pollfd) * nfds);
	    if (exp100ths)
		poll_timeout = exp100ths * 10;
	    else
		poll_timeout = -1;

	    fds[0].fd = SHTTY;
	    /*
	     * POLLIN, POLLIN, POLLIN,
	     * Keep those fd's POLLIN...
	     */
	    fds[0].events = POLLIN;
	    if (!keytmout) {
		for (i = 0; i < nwatch; i++) {
		    fds[i+1].fd = watch_fds[i];
		    fds[i+1].events = POLLIN;
		}
	    }
# else
	    fdmax = SHTTY;
	    tvptr = NULL;
	    if (exp100ths) {
		expire_tv.tv_sec = exp100ths / 100;
		expire_tv.tv_usec = (exp100ths % 100) * 10000L;
		tvptr = &expire_tv;
	    }
# endif
	    do {
# ifdef HAVE_POLL
		selret = poll(fds, errtry ? 1 : nfds, poll_timeout);
# else
		FD_ZERO(&foofd);
		FD_SET(SHTTY, &foofd);
		if (!keytmout && !errtry) {
		    for (i = 0; i < nwatch; i++) {
			int fd = watch_fds[i];
			FD_SET(fd, &foofd);
			if (fd > fdmax)
			    fdmax = fd;
		    }
		}
		selret = select(fdmax+1, (SELECT_ARG_2_T) & foofd,
				NULL, NULL, tvptr);
# endif
		/*
		 * Make sure a user interrupt gets passed on straight away.
		 */
		if (selret < 0 && errflag)
		    break;
		/*
		 * Try to avoid errors on our special fd's from
		 * messing up reads from the terminal.  Try first
		 * with all fds, then try unsetting the special ones.
		 */
		if (selret < 0 && !keytmout && !errtry) {
		    errtry = 1;
		    continue;
		}
		if (selret == 0) {
		    /* Special value -2 signals nothing ready */
		    selret = -2;
		}
		if (selret < 0)
		    break;
		if (!keytmout && nwatch) {
		    /*
		     * Copy the details of the watch fds in case the
		     * user decides to delete one from inside the
		     * handler function.
		     */
		    int lnwatch = nwatch;
		    int *lwatch_fds = zalloc(lnwatch*sizeof(int));
		    char **lwatch_funcs = zarrdup(watch_funcs);
		    memcpy(lwatch_fds, watch_fds, lnwatch*sizeof(int));
		    for (i = 0; i < lnwatch; i++) {
			if (
# ifdef HAVE_POLL
			    (fds[i+1].revents & POLLIN)
# else
			    FD_ISSET(lwatch_fds[i], &foofd)
# endif
			    ) {
			    /* Handle the fd. */
			    LinkList funcargs = znewlinklist();
			    zaddlinknode(funcargs, ztrdup(lwatch_funcs[i]));
			    {
				char buf[BDIGBUFSIZE];
				convbase(buf, lwatch_fds[i], 10);
				zaddlinknode(funcargs, ztrdup(buf));
			    }
# ifdef HAVE_POLL
#  ifdef POLLERR
			    if (fds[i+1].revents & POLLERR)
				zaddlinknode(funcargs, ztrdup("err"));
#  endif
#  ifdef POLLHUP
			    if (fds[i+1].revents & POLLHUP)
				zaddlinknode(funcargs, ztrdup("hup"));
#  endif
#  ifdef POLLNVAL
			    if (fds[i+1].revents & POLLNVAL)
				zaddlinknode(funcargs, ztrdup("nval"));
#  endif
# endif


			    callhookfunc(lwatch_funcs[i], funcargs);
			    if (errflag) {
				/* No sensible way of handling errors here */
				errflag = 0;
				/*
				 * Paranoia: don't run the hooks again this
				 * time.
				 */
				errtry = 1;
			    }
			    freelinklist(funcargs, freestr);
			}
		    }
		    /* Function may have invalidated the display. */
		    if (resetneeded)
			zrefresh();
		    zfree(lwatch_fds, lnwatch*sizeof(int));
		    freearray(lwatch_funcs);
		}
	    } while (!
# ifdef HAVE_POLL
		     (fds[0].revents & POLLIN)
# else
		     FD_ISSET(SHTTY, &foofd)
# endif
		);
# ifdef HAVE_POLL
	    zfree(fds, sizeof(struct pollfd) * nfds);
# endif
	    if (selret < 0)
		return selret;
	}
#else
# ifdef HAS_TIO
	ti = shttyinfo;
	ti.tio.c_lflag &= ~ICANON;
	ti.tio.c_cc[VMIN] = 0;
	ti.tio.c_cc[VTIME] = exp100ths / 10;
#  ifdef HAVE_TERMIOS_H
	tcsetattr(SHTTY, TCSANOW, &ti.tio);
#  else
	ioctl(SHTTY, TCSETA, &ti.tio);
#  endif
	ret = read(SHTTY, cptr, 1);
#  ifdef HAVE_TERMIOS_H
	tcsetattr(SHTTY, TCSANOW, &shttyinfo.tio);
#  else
	ioctl(SHTTY, TCSETA, &shttyinfo.tio);
#  endif
	return (ret <= 0) ? ret : *cptr;
# endif
#endif
    }

    ret = read(SHTTY, cptr, 1);

    return ret;
}

/**/
mod_export int
getkey(int keytmout)
{
    char cc;
    unsigned int ret;
    int die = 0, r, icnt = 0;
    int old_errno = errno, obreaks = breaks;

    if (kungetct)
	ret = STOUC(kungetbuf[--kungetct]);
    else {
#ifdef FIONREAD
	if (delayzsetterm) {
	    int val;
	    ioctl(SHTTY, FIONREAD, (char *)&val);
	    if (!val)
		zsetterm();
	}
#endif
	for (;;) {
	    int q = queue_signal_level();
	    dont_queue_signals();
	    r = raw_getkey(keytmout, &cc);
	    restore_queue_signals(q);
	    if (r == -2)	/* timeout */
		return EOF;
	    if (r == 1)
		break;
	    if (r == 0) {
		/* The test for IGNOREEOF was added to make zsh ignore ^Ds
		   that were typed while commands are running.  Unfortuantely
		   this caused trouble under at least one system (SunOS 4.1).
		   Here shells that lost their xterm (e.g. if it was killed
		   with -9) didn't fail to read from the terminal but instead
		   happily continued to read EOFs, so that the above read
		   returned with 0, and, with IGNOREEOF set, this caused
		   an infinite loop.  The simple way around this was to add
		   the counter (icnt) so that this happens 20 times and than
		   the shell gives up (yes, this is a bit dirty...). */
		if ((zlereadflags & ZLRF_IGNOREEOF) && icnt++ < 20)
		    continue;
		stopmsg = 1;
		zexit(1, 0);
	    }
	    icnt = 0;
	    if (errno == EINTR) {
		die = 0;
		if (!errflag && !retflag && !breaks)
		    continue;
		errflag = 0;
		breaks = obreaks;
		errno = old_errno;
		return EOF;
	    } else if (errno == EWOULDBLOCK) {
		fcntl(0, F_SETFL, 0);
	    } else if (errno == EIO && !die) {
		ret = opts[MONITOR];
		opts[MONITOR] = 1;
		attachtty(mypgrp);
		zrefresh();	/* kludge! */
		opts[MONITOR] = ret;
		die = 1;
	    } else if (errno != 0) {
		zerr("error on TTY read: %e", NULL, errno);
		stopmsg = 1;
		zexit(1, 0);
	    }
	}
	if (cc == '\r')		/* undo the exchange of \n and \r determined by */
	    cc = '\n';		/* zsetterm() */
	else if (cc == '\n')
	    cc = '\r';

	ret = STOUC(cc);
    }
    if (vichgflag) {
	if (vichgbufptr == vichgbufsz)
	    vichgbuf = realloc(vichgbuf, vichgbufsz *= 2);
	vichgbuf[vichgbufptr++] = ret;
    }
    errno = old_errno;
    return ret;
}

/**/
void
zlecore(void)
{
#if !defined(HAVE_POLL) && defined(HAVE_SELECT)
    struct timeval tv;
    fd_set foofd;

    FD_ZERO(&foofd);
#endif

    /*
     * A widget function may decide to exit the shell.
     * We never exit directly from functions, to allow
     * the shell to tidy up, so we have to test for
     * that explicitly.
     */
    while (!done && !errflag && !exit_pending) {

	statusline = NULL;
	vilinerange = 0;
	reselectkeymap();
	selectlocalmap(NULL);
	bindk = getkeycmd();
	if (bindk) {
	    if (!ll && isfirstln && !(zlereadflags & ZLRF_IGNOREEOF) &&
		lastchar == eofchar) {
		/*
		 * Slight hack: this relies on getkeycmd returning
		 * a value for the EOF character.  However,
		 * undefined-key is fine.  That's necessary because
		 * otherwise we can't distinguish this case from
		 * a ^C.
		 */
		eofsent = 1;
		break;
	    }
	    if (execzlefunc(bindk, zlenoargs)) {
		handlefeep(zlenoargs);
		if (eofsent)
		    break;
	    }
	    handleprefixes();
	    /* for vi mode, make sure the cursor isn't somewhere illegal */
	    if (invicmdmode() && cs > findbol() &&
		(cs == ll || line[cs] == '\n'))
		cs--;
	    if (undoing)
		handleundo();
	} else {
	    errflag = 1;
	    break;
	}
#ifdef HAVE_POLL
	if (baud && !(lastcmd & ZLE_MENUCMP)) {
	    struct pollfd pfd;
	    int to = cost * costmult / 1000; /* milliseconds */

	    if (to > 500)
		to = 500;
	    pfd.fd = SHTTY;
	    pfd.events = POLLIN;
	    if (!kungetct && poll(&pfd, 1, to) <= 0)
		zrefresh();
	} else
#else
# ifdef HAVE_SELECT
	if (baud && !(lastcmd & ZLE_MENUCMP)) {
	    FD_SET(SHTTY, &foofd);
	    tv.tv_sec = 0;
	    if ((tv.tv_usec = cost * costmult) > 500000)
		tv.tv_usec = 500000;
	    if (!kungetct && select(SHTTY+1, (SELECT_ARG_2_T) & foofd,
				    NULL, NULL, &tv) <= 0)
		zrefresh();
	} else
# endif
#endif
	    if (!kungetct)
		zrefresh();
    }
}

/* Read a line.  It is returned metafied. */

/**/
unsigned char *
zleread(char **lp, char **rp, int flags, int context)
{
    unsigned char *s;
    int old_errno = errno;
    int tmout = getiparam("TMOUT");
    Thingy initthingy;

#if defined(HAVE_POLL) || defined(HAVE_SELECT)
    baud = getiparam("BAUD");
    costmult = (baud) ? 3840000L / baud : 0;
#endif

    /* ZLE doesn't currently work recursively.  This is needed in case a *
     * select loop is used in a function called from ZLE.  vared handles *
     * this differently itself.                                          */
    if(zleactive) {
	char *pptbuf;
	int pptlen;

	pptbuf = unmetafy(promptexpand(lp ? *lp : NULL, 0, NULL, NULL),
			  &pptlen);
	write(2, (WRITE_ARG_2_T)pptbuf, pptlen);
	free(pptbuf);
	return (unsigned char *)shingetline();
    }

    keytimeout = getiparam("KEYTIMEOUT");
    if (!shout) {
	if (SHTTY != -1)
	    init_shout();

	if (!shout)
	    return NULL;
	/* We could be smarter and default to a system read. */

	/* If we just got a new shout, make sure the terminal is set up. */
	if (termflags & TERM_UNKNOWN)
	    init_term();
    }

    fflush(shout);
    fflush(stderr);
    intr();
    insmode = unset(OVERSTRIKE);
    eofsent = 0;
    resetneeded = 0;
    raw_lp = lp;
    lpromptbuf = promptexpand(lp ? *lp : NULL, 1, NULL, NULL);
    pmpt_attr = txtchange;
    raw_rp = rp;
    rpromptbuf = promptexpand(rp ? *rp : NULL, 1, NULL, NULL);
    rpmpt_attr = txtchange;
    free_prepostdisplay();

    zlereadflags = flags;
    zlecontext = context;
    histline = curhist;
    undoing = 1;
    line = (unsigned char *)zalloc((linesz = 256) + 2);
    *line = '\0';
    virangeflag = lastcmd = done = cs = ll = mark = 0;
    vichgflag = 0;
    viinsbegin = 0;
    statusline = NULL;
    selectkeymap("main", 1);
    selectlocalmap(NULL);
    fixsuffix();
    if ((s = (unsigned char *)getlinknode(bufstack))) {
	setline((char *)s);
	zsfree((char *)s);
	if (stackcs != -1) {
	    cs = stackcs;
	    stackcs = -1;
	    if (cs > ll)
		cs = ll;
	}
	if (stackhist != -1) {
	    histline = stackhist;
	    stackhist = -1;
	}
    }
    initundo();
    if (isset(PROMPTCR))
	putc('\r', shout);
    if (tmout)
	alarm(tmout);
    zleactive = 1;
    resetneeded = 1;
    errflag = retflag = 0;
    lastcol = -1;
    initmodifier(&zmod);
    prefixflag = 0;

    zrefresh();

    if ((initthingy = rthingy_nocreate("zle-line-init"))) {
	char *args[2];
	args[0] = initthingy->nam;
	args[1] = NULL;
	execzlefunc(initthingy, args);
	unrefthingy(initthingy);
	errflag = retflag = 0;
    }

    zlecore();

    statusline = NULL;
    invalidatelist();
    trashzle();
    free(lpromptbuf);
    free(rpromptbuf);
    zleactive = zlereadflags = lastlistlen = zlecontext = 0;
    alarm(0);

    freeundo();
    if (eofsent) {
	free(line);
	line = NULL;
    } else {
	line[ll++] = '\n';
	line = (unsigned char *) metafy((char *) line, ll, META_REALLOC);
    }
    forget_edits();
    errno = old_errno;
    return line;
}

/* execute a widget */

/**/
int
execzlefunc(Thingy func, char **args)
{
    int r = 0, ret = 0;
    Widget w;

    if(func->flags & DISABLED) {
	/* this thingy is not the name of a widget */
	char *nm = niceztrdup(func->nam);
	char *msg = tricat("No such widget `", nm, "'");

	zsfree(nm);
	showmsg(msg);
	zsfree(msg);
	ret = 1;
    } else if((w = func->widget)->flags & (WIDGET_INT|WIDGET_NCOMP)) {
	int wflags = w->flags;

	/*
	 * The rule is that "zle -N" widgets suppress EOF warnings.  When
	 * a "zle -N" widget invokes "zle another-widget" we pass through
	 * this code again, but with actual arguments rather than with the
	 * zlenoargs placeholder.
	 */
	if (keybuf[0] == eofchar && !keybuf[1] && args == zlenoargs &&
	    !ll && isfirstln && (zlereadflags & ZLRF_IGNOREEOF)) {
	    showmsg((!islogin) ? "zsh: use 'exit' to exit." :
		    "zsh: use 'logout' to logout.");
	    eofsent = 1;
	    ret = 1;
	} else {
	    if(!(wflags & ZLE_KEEPSUFFIX))
		removesuffix();
	    if(!(wflags & ZLE_MENUCMP)) {
		fixsuffix();
		invalidatelist();
	    }
	    if (wflags & ZLE_LINEMOVE)
		vilinerange = 1;
	    if(!(wflags & ZLE_LASTCOL))
		lastcol = -1;
	    if (wflags & WIDGET_NCOMP) {
		int atcurhist = histline == curhist;
		compwidget = w;
		ret = completecall(args);
		if (atcurhist)
		    histline = curhist;
	    } else {
		queue_signals();
		ret = w->u.fn(args);
		unqueue_signals();
	    }
	    if (!(wflags & ZLE_NOTCOMMAND))
		lastcmd = wflags;
	}
	r = 1;
    } else {
	Shfunc shf = (Shfunc) shfunctab->getnode(shfunctab, w->u.fnnam);
	Eprog prog = (shf ? shf->funcdef : &dummy_eprog);

	if(prog == &dummy_eprog) {
	    /* the shell function doesn't exist */
	    char *nm = niceztrdup(w->u.fnnam);
	    char *msg = tricat("No such shell function `", nm, "'");

	    zsfree(nm);
	    showmsg(msg);
	    zsfree(msg);
	    ret = 1;
	} else {
	    int osc = sfcontext, osi = movefd(0);
	    int oxt = isset(XTRACE);
	    LinkList largs = NULL;

	    if (*args) {
		largs = newlinklist();
		addlinknode(largs, dupstring(w->u.fnnam));
		while (*args)
		    addlinknode(largs, dupstring(*args++));
	    }
	    startparamscope();
	    makezleparams(0);
	    sfcontext = SFC_WIDGET;
	    opts[XTRACE] = 0;
	    ret = doshfunc(w->u.fnnam, prog, largs, shf->flags, 1);
	    opts[XTRACE] = oxt;
	    sfcontext = osc;
	    endparamscope();
	    lastcmd = 0;
	    r = 1;
	    redup(osi, 0);
	}
    }
    if (r) {
	unrefthingy(lbindk);
	refthingy(func);
	lbindk = func;
    }
    return ret;
}

/* initialise command modifiers */

/**/
static void
initmodifier(struct modifier *mp)
{
    mp->flags = 0;
    mp->mult = 1;
    mp->tmult = 1;
    mp->vibuf = 0;
}

/* Reset command modifiers, unless the command just executed was a prefix. *
 * Also set zmult, if the multiplier has been amended.                     */

/**/
static void
handleprefixes(void)
{
    if (prefixflag) {
	prefixflag = 0;
	if(zmod.flags & MOD_TMULT) {
	    zmod.flags |= MOD_MULT;
	    zmod.mult = zmod.tmult;
	}
    } else
	initmodifier(&zmod);
}

/**/
static int
savekeymap(char *cmdname, char *oldname, char *newname, Keymap *savemapptr)
{
    Keymap km = openkeymap(newname);

    if (km) {
	*savemapptr = openkeymap(oldname);
	/* I love special cases */
	if (*savemapptr == km)
	    *savemapptr = NULL;
	else {
	    /* make sure this doesn't get deleted. */
	    if (*savemapptr)
		refkeymap(*savemapptr);
	    linkkeymap(km, oldname, 0);
	}
	return 0;
    } else {
	zwarnnam(cmdname, "no such keymap: %s", newname, 0);
	return 1;
    }
}

/**/
static void
restorekeymap(char *cmdname, char *oldname, char *newname, Keymap savemap)
{
    if (savemap) {
	linkkeymap(savemap, oldname, 0);
	/* we incremented the reference count above */
	unrefkeymap(savemap);
    } else if (newname) {
	/* urr... can this happen? */
	zwarnnam(cmdname,
		 "keymap %s was not defined, not restored", oldname, 0);
    }
}

/* this exports the argument we are currently vared'iting if != NULL */

/**/
mod_export char *varedarg;

/* vared: edit (literally) a parameter value */

/**/
static int
bin_vared(char *name, char **args, Options ops, UNUSED(int func))
{
    char *s, *t, *ova = varedarg;
    struct value vbuf;
    Value v;
    Param pm = 0;
    int ifl;
    int type = PM_SCALAR, obreaks = breaks, haso = 0;
    char *p1, *p2, *main_keymapname, *vicmd_keymapname;
    Keymap main_keymapsave = NULL, vicmd_keymapsave = NULL;
    FILE *oshout = NULL;

    if ((interact && unset(USEZLE)) || !strcmp(term, "emacs")) {
	zwarnnam(name, "ZLE not enabled", NULL, 0);
	return 1;
    }
    if (zleactive) {
	zwarnnam(name, "ZLE cannot be used recursively (yet)", NULL, 0);
	return 1;
    }

    if (OPT_ISSET(ops,'A'))
    {
	if (OPT_ISSET(ops, 'a'))
	{
	    zwarnnam(name, "specify only one of -a and -A", NULL, 0);
	    return 1;
	}
	type = PM_HASHED;
    }
    else if (OPT_ISSET(ops,'a'))
	type = PM_ARRAY;
    p1 = OPT_ARG_SAFE(ops,'p');
    p2 = OPT_ARG_SAFE(ops,'r');
    main_keymapname = OPT_ARG_SAFE(ops,'M');
    vicmd_keymapname = OPT_ARG_SAFE(ops,'m');

    if (type != PM_SCALAR && !OPT_ISSET(ops,'c')) {
	zwarnnam(name, "-%s ignored", type == PM_ARRAY ? "a" : "A", 0);
    }

    /* handle non-existent parameter */
    s = args[0];
    queue_signals();
    v = fetchvalue(&vbuf, &s, (!OPT_ISSET(ops,'c') || type == PM_SCALAR),
		   SCANPM_WANTKEYS|SCANPM_WANTVALS|SCANPM_MATCHMANY);
    if (!v && !OPT_ISSET(ops,'c')) {
	unqueue_signals();
	zwarnnam(name, "no such variable: %s", args[0], 0);
	return 1;
    } else if (v) {
	if (v->isarr) {
	    /* Array: check for separators and quote them. */
	    char **arr = getarrvalue(v), **aptr, **tmparr, **tptr;
	    tptr = tmparr = (char **)zhalloc(sizeof(char *)*(arrlen(arr)+1));
	    for (aptr = arr; *aptr; aptr++) {
		int sepcount = 0;
		/*
		 * See if this word contains a separator character
		 * or backslash
		 */
		for (t = *aptr; *t; t++) {
		    if (*t == Meta) {
			if (isep(t[1] ^ 32))
			    sepcount++;
			t++;
		    } else if (isep(*t) || *t == '\\')
			sepcount++;
		}
		if (sepcount) {
		    /* Yes, so allocate enough space to quote it. */
		    char *newstr, *nptr;
		    newstr = zhalloc(strlen(*aptr)+sepcount+1);
		    /* Go through string quoting separators */
		    for (t = *aptr, nptr = newstr; *t; ) {
			if (*t == Meta) {
			    if (isep(t[1] ^ 32))
				*nptr++ = '\\';
			    *nptr++ = *t++;
			} else if (isep(*t) || *t == '\\')
			    *nptr++ = '\\';
			*nptr++ = *t++;
		    }
		    *nptr = '\0';
		    /* Stick this into the array of words to join up */
		    *tptr++ = newstr;
		} else
		    *tptr++ = *aptr; /* No, keep original array element */
	    }
	    *tptr = NULL;
	    s = sepjoin(tmparr, NULL, 0);
	} else {
	    s = ztrdup(getstrvalue(v));
	}
	unqueue_signals();
    } else if (*s) {
	unqueue_signals();
	zwarnnam(name, "invalid parameter name: %s", args[0], 0);
	return 1;
    } else {
	unqueue_signals();
	s = ztrdup(s);
    }

    if (SHTTY == -1) {
	/* need to open /dev/tty specially */
	if ((SHTTY = open("/dev/tty", O_RDWR|O_NOCTTY)) == -1) {
	    zwarnnam(name, "can't access terminal", NULL, 0);
	    return 1;
	}
	oshout = shout;
	init_shout();

	haso = 1;
    }
    /* edit the parameter value */
    zpushnode(bufstack, s);

    if (main_keymapname &&
	savekeymap(name, "main", main_keymapname, &main_keymapsave))
	main_keymapname = NULL;
    if (vicmd_keymapname &&
	savekeymap(name, "vicmd", vicmd_keymapname, &vicmd_keymapsave))
	vicmd_keymapname = NULL;

    varedarg = *args;
    ifl = isfirstln;
    if (OPT_ISSET(ops,'h'))
	hbegin(2);
    isfirstln = OPT_ISSET(ops,'e');
    t = (char *) zleread(&p1, &p2, OPT_ISSET(ops,'h') ? ZLRF_HISTORY : 0,
			 ZLCON_VARED);
    if (OPT_ISSET(ops,'h'))
	hend(NULL);
    isfirstln = ifl;
    varedarg = ova;

    restorekeymap(name, "main", main_keymapname, main_keymapsave);
    restorekeymap(name, "vicmd", vicmd_keymapname, vicmd_keymapsave);

    if (haso) {
	fclose(shout);	/* close(SHTTY) */
	shout = oshout;
	SHTTY = -1;
    }
    if (!t || errflag) {
	/* error in editing */
	errflag = 0;
	breaks = obreaks;
	return 1;
    }
    /* strip off trailing newline, if any */
    if (t[strlen(t) - 1] == '\n')
	t[strlen(t) - 1] = '\0';
    /* final assignment of parameter value */
    if (OPT_ISSET(ops,'c')) {
	unsetparam(args[0]);
	createparam(args[0], type);
    }
    queue_signals();
    pm = (Param) paramtab->getnode(paramtab, args[0]);
    if (pm && (PM_TYPE(pm->flags) & (PM_ARRAY|PM_HASHED))) {
	char **a;

	/*
	 * Use spacesplit with fourth argument 1: identify quoted separators,
	 * and unquote.  This duplicates the string, so we still need to free.
	 */
	a = spacesplit(t, 1, 0, 1);
	zsfree(t);
	if (PM_TYPE(pm->flags) == PM_ARRAY)
	    setaparam(args[0], a);
	else
	    sethparam(args[0], a);
    } else
	setsparam(args[0], t);
    unqueue_signals();
    return 0;
}

/**/
int
describekeybriefly(UNUSED(char **args))
{
    char *seq, *str, *msg, *is;
    Thingy func;

    if (statusline)
	return 1;
    clearlist = 1;
    statusline = "Describe key briefly: _";
    statusll = strlen(statusline);
    zrefresh();
    seq = getkeymapcmd(curkeymap, &func, &str);
    statusline = NULL;
    if(!*seq)
	return 1;
    msg = bindztrdup(seq);
    msg = appstr(msg, " is ");
    if (!func)
	is = bindztrdup(str);
    else
	is = niceztrdup(func->nam);
    msg = appstr(msg, is);
    zsfree(is);
    showmsg(msg);
    zsfree(msg);
    return 0;
}

#define MAXFOUND 4

struct findfunc {
    Thingy func;
    int found;
    char *msg;
};

/**/
static void
scanfindfunc(char *seq, Thingy func, UNUSED(char *str), void *magic)
{
    struct findfunc *ff = magic;

    if(func != ff->func)
	return;
    if (!ff->found++)
	ff->msg = appstr(ff->msg, " is on");
    if(ff->found <= MAXFOUND) {
	char *b = bindztrdup(seq);

	ff->msg = appstr(ff->msg, " ");
	ff->msg = appstr(ff->msg, b);
	zsfree(b);
    }
}

/**/
int
whereis(UNUSED(char **args))
{
    struct findfunc ff;

    if (!(ff.func = executenamedcommand("Where is: ")))
	return 1;
    ff.found = 0;
    ff.msg = niceztrdup(ff.func->nam);
    scankeymap(curkeymap, 1, scanfindfunc, &ff);
    if (!ff.found)
	ff.msg = appstr(ff.msg, " is not bound to any key");
    else if(ff.found > MAXFOUND)
	ff.msg = appstr(ff.msg, " et al");
    showmsg(ff.msg);
    zsfree(ff.msg);
    return 0;
}

/**/
int
recursiveedit(UNUSED(char **args))
{
    int locerror;

    zrefresh();
    zlecore();

    locerror = errflag;
    errflag = done = 0;

    return locerror;
}

/**/
void
reexpandprompt(void)
{
    free(lpromptbuf);
    lpromptbuf = promptexpand(raw_lp ? *raw_lp : NULL, 1, NULL, NULL);
    free(rpromptbuf);
    rpromptbuf = promptexpand(raw_rp ? *raw_rp : NULL, 1, NULL, NULL);
}

/**/
int
resetprompt(UNUSED(char **args))
{
    reexpandprompt();
    return redisplay(NULL);
}

/**/
mod_export void
trashzle(void)
{
    if (zleactive) {
	/* This zrefresh() is just to get the main editor display right and *
	 * get the cursor in the right place.  For that reason, we disable  *
	 * list display (which would otherwise result in infinite           *
	 * recursion [at least, it would if zrefresh() didn't have its      *
	 * extra `inlist' check]).                                          */
	int sl = showinglist;
	showinglist = 0;
	trashedzle = 1;
	zrefresh();
	showinglist = sl;
	moveto(nlnct, 0);
	if (clearflag && tccan(TCCLEAREOD)) {
	    tcout(TCCLEAREOD);
	    clearflag = listshown = 0;
	}
	if (postedit)
	    fprintf(shout, "%s", postedit);
	fflush(shout);
	resetneeded = 1;
	if (!(zlereadflags & ZLRF_NOSETTY))
	  settyinfo(&shttyinfo);
    }
    if (errflag)
	kungetct = 0;
}

/* Hook functions. Used to allow access to zle parameters if zle is
 * active. */

static int
zlebeforetrap(UNUSED(Hookdef dummy), UNUSED(void *dat))
{
    if (zleactive) {
	startparamscope();
	makezleparams(1);
    }
    return 0;
}

static int
zleaftertrap(UNUSED(Hookdef dummy), UNUSED(void *dat))
{
    if (zleactive)
	endparamscope();

    return 0;
}

static struct builtin bintab[] = {
    BUILTIN("bindkey", 0, bin_bindkey, 0, -1, 0, "evaM:ldDANmrsLRp", NULL),
    BUILTIN("vared",   0, bin_vared,   1,  1, 0, "aAcehM:m:p:r:", NULL),
    BUILTIN("zle",     0, bin_zle,     0, -1, 0, "aAcCDFgGIKlLmMNRU", NULL),
};

/* The order of the entries in this table has to match the *HOOK
 * macros in zle.h */

/**/
mod_export struct hookdef zlehooks[] = {
    HOOKDEF("list_matches", NULL, 0),
    HOOKDEF("complete", NULL, 0),
    HOOKDEF("before_complete", NULL, 0),
    HOOKDEF("after_complete", NULL, 0),
    HOOKDEF("accept_completion", NULL, 0),
    HOOKDEF("reverse_menu", NULL, 0),
    HOOKDEF("invalidate_list", NULL, 0),
};

/**/
int
setup_(UNUSED(Module m))
{
    /* Set up editor entry points */
    trashzleptr = trashzle;
    refreshptr = zrefresh;
    spaceinlineptr = spaceinline;
    zlereadptr = zleread;
    zlesetkeymapptr = zlesetkeymap;

    getkeyptr = getkey;

    /* initialise the thingies */
    init_thingies();
    lbindk = NULL;

    /* miscellaneous initialisations */
    stackhist = stackcs = -1;
    kungetbuf = (char *) zalloc(kungetsz = 32);
    comprecursive = 0;
    rdstrs = NULL;

    /* initialise the keymap system */
    init_keymaps();

    varedarg = NULL;

    incompfunc = incompctlfunc = hascompmod = 0;
    hascompwidgets = 0;

    clwords = (char **) zshcalloc((clwsize = 16) * sizeof(char *));

    return 0;
}

/**/
int
boot_(Module m)
{
    addhookfunc("before_trap", (Hookfn) zlebeforetrap);
    addhookfunc("after_trap", (Hookfn) zleaftertrap);
    addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
    addhookdefs(m->nam, zlehooks, sizeof(zlehooks)/sizeof(*zlehooks));
    return 0;
}

/**/
int
cleanup_(Module m)
{
    if(zleactive) {
	zerrnam(m->nam, "can't unload the zle module while zle is active",
	    NULL, 0);
	return 1;
    }
    deletehookfunc("before_trap", (Hookfn) zlebeforetrap);
    deletehookfunc("after_trap", (Hookfn) zleaftertrap);
    deletebuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
    deletehookdefs(m->nam, zlehooks, sizeof(zlehooks)/sizeof(*zlehooks));
    return 0;
}

/**/
int
finish_(UNUSED(Module m))
{
    int i;

    unrefthingy(lbindk);

    cleanup_keymaps();
    deletehashtable(thingytab);

    zfree(vichgbuf, vichgbufsz);
    zfree(kungetbuf, kungetsz);
    free_isrch_spots();
    if (rdstrs)
        freelinklist(rdstrs, freestr);
    zfree(cutbuf.buf, cutbuf.len);
    if (kring) {
	for(i = kringsize; i--; )
	    zfree(kring[i].buf, kring[i].len);
	zfree(kring, kringsize * sizeof(struct cutbuffer));
    }
    for(i = 35; i--; )
	zfree(vibuf[i].buf, vibuf[i].len);

    /* editor entry points */
    trashzleptr = noop_function;
    refreshptr = noop_function;
    spaceinlineptr = noop_function_int;
    zlereadptr = fallback_zleread;
    zlesetkeymapptr= noop_function_int;

    getkeyptr = NULL;

    zfree(clwords, clwsize * sizeof(char *));

    return 0;
}
