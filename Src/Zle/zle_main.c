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

/* != 0 if we're done editing */

/**/
int done;

/* location of mark */

/**/
int mark;

/* last character pressed */

/**/
int c;

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

#ifdef HAVE_SELECT
/* Terminal baud rate */

static int baud;
#endif

/* flags associated with last command */

/**/
mod_export int lastcmd;

/**/
mod_export Widget compwidget;

/* the status line, and its length */

/**/
char *statusline;
/**/
int statusll;

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

#ifdef FIONREAD
static int delayzsetterm;
#endif

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

/**/
mod_export int
getkey(int keytmout)
{
    char cc;
    unsigned int ret;
    long exp100ths;
    int die = 0, r, icnt = 0;
    int old_errno = errno, obreaks = breaks;

#ifdef HAVE_SELECT
    fd_set foofd;

#else
# ifdef HAS_TIO
    struct ttyinfo ti;

# endif
#endif

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
	if (keytmout
#ifdef FIONREAD
	    && ! delayzsetterm
#endif
	    ) {
	    if (keytimeout > 500)
		exp100ths = 500;
	    else if (keytimeout > 0)
		exp100ths = keytimeout;
	    else
		exp100ths = 0;
#ifdef HAVE_SELECT
	    if (exp100ths) {
		struct timeval expire_tv;

		expire_tv.tv_sec = exp100ths / 100;
		expire_tv.tv_usec = (exp100ths % 100) * 10000L;
		FD_ZERO(&foofd);
		FD_SET(SHTTY, &foofd);
		if (select(SHTTY+1, (SELECT_ARG_2_T) & foofd,
			   NULL, NULL, &expire_tv) <= 0)
		    return EOF;
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
	    r = read(SHTTY, &cc, 1);
#  ifdef HAVE_TERMIOS_H
	    tcsetattr(SHTTY, TCSANOW, &shttyinfo.tio);
#  else
	    ioctl(SHTTY, TCSETA, &shttyinfo.tio);
#  endif
	    return (r <= 0) ? EOF : cc;
# endif
#endif
	}
	for (;;) {
	    int q = queue_signal_level();
	    dont_queue_signals();
	    r = read(SHTTY, &cc, 1);
	    restore_queue_signals(q);
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
		if (isset(IGNOREEOF) && icnt++ < 20)
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

/* Read a line.  It is returned metafied. */

/**/
unsigned char *
zleread(char *lp, char *rp, int flags)
{
    unsigned char *s;
    int old_errno = errno;
    int tmout = getiparam("TMOUT");

#ifdef HAVE_SELECT
    long costmult;
    struct timeval tv;
    fd_set foofd;

    baud = getiparam("BAUD");
    costmult = (baud) ? 3840000L / baud : 0;
#endif

    /* ZLE doesn't currently work recursively.  This is needed in case a *
     * select loop is used in a function called from ZLE.  vared handles *
     * this differently itself.                                          */
    if(zleactive) {
	char *pptbuf;
	int pptlen;

	pptbuf = unmetafy(promptexpand(lp, 0, NULL, NULL), &pptlen);
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
    lpromptbuf = promptexpand(lp, 1, NULL, NULL);
    pmpt_attr = txtchange;
    rpromptbuf = promptexpand(rp, 1, NULL, NULL);
    rpmpt_attr = txtchange;

    zlereadflags = flags;
    histline = curhist;
#ifdef HAVE_SELECT
    FD_ZERO(&foofd);
#endif
    undoing = 1;
    line = (unsigned char *)zalloc((linesz = 256) + 2);
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
    while (!done && !errflag) {

	statusline = NULL;
	vilinerange = 0;
	reselectkeymap();
	selectlocalmap(NULL);
	bindk = getkeycmd();
	if (!ll && isfirstln && unset(IGNOREEOF) && c == eofchar) {
	    eofsent = 1;
	    break;
	}
	if (bindk) {
	    if (execzlefunc(bindk, zlenoargs))
		handlefeep(zlenoargs);
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
#ifdef HAVE_SELECT
	if (baud && !(lastcmd & ZLE_MENUCMP)) {
	    FD_SET(SHTTY, &foofd);
	    tv.tv_sec = 0;
	    if ((tv.tv_usec = cost * costmult) > 500000)
		tv.tv_usec = 500000;
	    if (!kungetct && select(SHTTY+1, (SELECT_ARG_2_T) & foofd,
				    NULL, NULL, &tv) <= 0)
		zrefresh();
	} else
#endif
	    if (!kungetct)
		zrefresh();
    }
    statusline = NULL;
    invalidatelist();
    trashzle();
    free(lpromptbuf);
    free(rpromptbuf);
    zleactive = zlereadflags = lastlistlen = 0;
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

	if (keybuf[0] == eofchar && !keybuf[1] &&
	    !ll && isfirstln && isset(IGNOREEOF)) {
	    showmsg((!islogin) ? "zsh: use 'exit' to exit." :
		    "zsh: use 'logout' to logout.");
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
	Eprog prog = getshfunc(w->u.fnnam);

	if(prog == &dummy_eprog) {
	    /* the shell function doesn't exist */
	    char *nm = niceztrdup(w->u.fnnam);
	    char *msg = tricat("No such shell function `", nm, "'");

	    zsfree(nm);
	    showmsg(msg);
	    zsfree(msg);
	    ret = 1;
	} else {
	    int osc = sfcontext, osi = movefd(0), olv = lastval;
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
	    doshfunc(w->u.fnnam, prog, largs, 0, 0);
	    ret = lastval;
	    lastval = olv;
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

/* this exports the argument we are currently vared'iting if != NULL */

/**/
mod_export char *varedarg;

/* vared: edit (literally) a parameter value */

/**/
static int
bin_vared(char *name, char **args, char *ops, int func)
{
    char *s, *t, *ova = varedarg;
    struct value vbuf;
    Value v;
    Param pm = 0;
    int create = 0, ifl, ieof;
    int type = PM_SCALAR, obreaks = breaks, haso = 0;
    char *p1 = NULL, *p2 = NULL;
    FILE *oshout = NULL;

    if ((interact && unset(USEZLE)) || !strcmp(term, "emacs")) {
	zwarnnam(name, "ZLE not enabled", NULL, 0);
	return 1;
    }
    if (zleactive) {
	zwarnnam(name, "ZLE cannot be used recursively (yet)", NULL, 0);
	return 1;
    }

    /* all options are handled as arguments */
    while (*args && **args == '-') {
	while (*++(*args))
	    switch (**args) {
	    case 'c':
		/* -c option -- allow creation of the parameter if it doesn't
		yet exist */
		create = 1;
		break;
	    case 'a':
		type = PM_ARRAY;
		break;
	    case 'A':
		type = PM_HASHED;
		break;
	    case 'p':
		/* -p option -- set main prompt string */
		if ((*args)[1])
		    p1 = *args + 1, *args = "" - 1;
		else if (args[1])
		    p1 = *(++args), *args = "" - 1;
		else {
		    zwarnnam(name, "prompt string expected after -%c", NULL,
			     **args);
		    return 1;
		}
		break;
	    case 'r':
		/* -r option -- set right prompt string */
		if ((*args)[1])
		    p2 = *args + 1, *args = "" - 1;
		else if (args[1])
		    p2 = *(++args), *args = "" - 1;
		else {
		    zwarnnam(name, "prompt string expected after -%c", NULL,
			     **args);
		    return 1;
		}
		break;
	    case 'h':
		/* -h option -- enable history */
		ops['h'] = 1;
		break;
	    case 'e':
		/* -e option -- enable EOF */
		ops['e'] = 1;
		break;
	    default:
		/* unrecognised option character */
		zwarnnam(name, "unknown option: %s", *args, 0);
		return 1;
	    }
	args++;
    }
    if (type && !create) {
	zwarnnam(name, "-%s ignored", type == PM_ARRAY ? "a" : "A", 0);
    }

    /* check we have a parameter name */
    if (!*args) {
	zwarnnam(name, "missing variable", NULL, 0);
	return 1;
    }
    /* handle non-existent parameter */
    s = args[0];
    queue_signals();
    v = fetchvalue(&vbuf, &s, (!create || type == PM_SCALAR),
		   SCANPM_WANTKEYS|SCANPM_WANTVALS|SCANPM_MATCHMANY);
    if (!v && !create) {
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

    varedarg = *args;
    ifl = isfirstln;
    if (ops['h'])
	hbegin(2);
    isfirstln = ops['e'];
    ieof = opts[IGNOREEOF];
    opts[IGNOREEOF] = 0;
    t = (char *) zleread(p1, p2, ops['h'] ? ZLRF_HISTORY : 0);
    opts[IGNOREEOF] = ieof;
    if (ops['h'])
	hend(NULL);
    isfirstln = ifl;
    varedarg = ova;
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
    if (create) {
	unsetparam(args[0]);
	createparam(args[0], type);
    }
    queue_signals();
    pm = (Param) paramtab->getnode(paramtab, args[0]);
    if (pm && (PM_TYPE(pm->flags) & (PM_ARRAY|PM_HASHED))) {
	char **a;

	/*
	 * Use spacesplit with fourth argument 1: identify quoted separators,
	 * unquote but don't split.
	 */
	a = spacesplit(t, 1, 0, 1);
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
describekeybriefly(char **args)
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
scanfindfunc(char *seq, Thingy func, char *str, void *magic)
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
whereis(char **args)
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
zlebeforetrap(Hookdef dummy, void *dat)
{
    if (zleactive) {
	startparamscope();
	makezleparams(1);
    }
    return 0;
}

static int
zleaftertrap(Hookdef dummy, void *dat)
{
    if (zleactive)
	endparamscope();

    return 0;
}

static struct builtin bintab[] = {
    BUILTIN("bindkey", 0, bin_bindkey, 0, -1, 0, "evaMldDANmrsLRp", NULL),
    BUILTIN("vared",   0, bin_vared,   1,  7, 0, NULL,             NULL),
    BUILTIN("zle",     0, bin_zle,     0, -1, 0, "lDANCLmMgGcRaUI", NULL),
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
setup_(Module m)
{
    /* Set up editor entry points */
    trashzleptr = trashzle;
    refreshptr = zrefresh;
    spaceinlineptr = spaceinline;
    zlereadptr = zleread;

    getkeyptr = getkey;

    /* initialise the thingies */
    init_thingies();
    lbindk = NULL;

    /* miscellaneous initialisations */
    stackhist = stackcs = -1;
    kungetbuf = (char *) zalloc(kungetsz = 32);
    comprecursive = 0;

    /* initialise the keymap system */
    init_keymaps();

    varedarg = NULL;

    incompfunc = incompctlfunc = hascompmod = 0;

    clwords = (char **) zcalloc((clwsize = 16) * sizeof(char *));

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
finish_(Module m)
{
    int i;

    unrefthingy(lbindk);

    cleanup_keymaps();
    deletehashtable(thingytab);

    zfree(vichgbuf, vichgbufsz);
    zfree(kungetbuf, kungetsz);
    free_isrch_spots();

    zfree(cutbuf.buf, cutbuf.len);
    for(i = KRINGCT; i--; )
	zfree(kring[i].buf, kring[i].len);
    for(i = 35; i--; )
	zfree(vibuf[i].buf, vibuf[i].len);

    /* editor entry points */
    trashzleptr = noop_function;
    refreshptr = noop_function;
    spaceinlineptr = noop_function_int;
    zlereadptr = fallback_zleread;

    getkeyptr = NULL;

    zfree(clwords, clwsize * sizeof(char *));

    return 0;
}
