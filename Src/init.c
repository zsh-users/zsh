/*
 * init.c - main loop and initialization routines
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

#include "zsh.mdh"
#include "init.pro"

#include "zshpaths.h"
#include "zshxmods.h"

/**/
int noexitct = 0;

/* what level of sourcing we are at */
 
/**/
int sourcelevel;

/* the shell tty fd */

/**/
int SHTTY;

/* the FILE attached to the shell tty */

/**/
FILE *shout;

/* termcap strings */
 
/**/
char *tcstr[TC_COUNT];

/* lengths of each termcap string */
 
/**/
int tclen[TC_COUNT];

/* Values of the li, co and am entries */

/**/
int tclines, tccolumns, hasam;

#ifdef DEBUG
/* depth of allocation type stack */

/**/
int alloc_stackp;
#endif

/* keep executing lists until EOF found */

/**/
void
loop(int toplevel, int justonce)
{
    List list;
#ifdef DEBUG
    int oasp = toplevel ? 0 : alloc_stackp;
#endif

    pushheap();
    for (;;) {
	freeheap();
	errflag = 0;
	if (isset(SHINSTDIN)) {
	    setblock_stdin();
	    if (interact)
		preprompt();
	}
	hbegin();		/* init history mech        */
	intr();			/* interrupts on            */
	lexinit();              /* initialize lexical state */
	if (!(list = parse_event())) {	/* if we couldn't parse a list */
	    hend();
	    if ((tok == ENDINPUT && !errflag) ||
		(tok == LEXERR && (!isset(SHINSTDIN) || !toplevel)) ||
		justonce)
		break;
	    continue;
	}
	if (hend()) {
	    int toksav = tok;
	    List prelist;

	    if (toplevel && (prelist = getshfunc("preexec")) != &dummy_list) {
		Histent he = gethistent(curhist);
		LinkList args;
		PERMALLOC {
		    args = newlinklist();
		    addlinknode(args, "preexec");
		    if (he && he->text)
			addlinknode(args, he->text);
		} LASTALLOC;
		doshfunc(prelist, args, 0, 1);
		freelinklist(args, (FreeFunc) NULL);
		errflag = 0;
	    }
	    if (stopmsg)	/* unset 'you have stopped jobs' flag */
		stopmsg--;
	    execlist(list, 0, 0);
	    tok = toksav;
	    if (toplevel)
		noexitct = 0;
	}
	DPUTS(alloc_stackp != oasp, "BUG: alloc_stackp changed in loop()");
	if (ferror(stderr)) {
	    zerr("write error", NULL, 0);
	    clearerr(stderr);
	}
	if (subsh)		/* how'd we get this far in a subshell? */
	    exit(lastval);
	if (((!interact || sourcelevel) && errflag) || retflag)
	    break;
	if (trapreturn) {
	    lastval = trapreturn;
	    trapreturn = 0;
	}
	if (isset(SINGLECOMMAND) && toplevel) {
	    if (sigtrapped[SIGEXIT])
		dotrap(SIGEXIT);
	    exit(lastval);
	}
	if (justonce)
	    break;
    }
    popheap();
}

static char *cmd;
static int restricted;

/**/
void
parseargs(char **argv)
{
    char **x;
    int action, optno;
    LinkList paramlist;
    int bourne = (emulation == EMULATE_KSH || emulation == EMULATE_SH);

    argzero = *argv++;
    SHIN = 0;

    /* There's a bit of trickery with opts[INTERACTIVE] here.  It starts *
     * at a value of 2 (instead of 1) or 0.  If it is explicitly set on  *
     * the command line, it goes to 1 or 0.  If input is coming from     *
     * somewhere that normally makes the shell non-interactive, we do    *
     * "opts[INTERACTIVE] &= 1", so that only a *default* on state will  *
     * be changed.  At the end of the function, a value of 2 gets        *
     * changed to 1.                                                     */
    opts[INTERACTIVE] = isatty(0) ? 2 : 0;
    opts[SHINSTDIN] = 0;
    opts[SINGLECOMMAND] = 0;

    /* loop through command line options (begins with "-" or "+") */
    while (*argv && (**argv == '-' || **argv == '+')) {
	action = (**argv == '-');
	if(!argv[0][1])
	    *argv = "--";
	while (*++*argv) {
	    /* The pseudo-option `--' signifies the end of options. *
	     * `-b' does too, csh-style, unless we're emulating a   *
	     * Bourne style shell.                                  */
	    if (**argv == '-' || (!bourne && **argv == 'b')) {
		argv++;
		goto doneoptions;
	    }

	    if (**argv == 'c') {         /* -c command */
		if (!*++argv) {
		    zerr("string expected after -c", NULL, 0);
		    exit(1);
		}
		cmd = *argv++;
		opts[INTERACTIVE] &= 1;
		opts[SHINSTDIN] = 0;
		goto doneoptions;
	    } else if (**argv == 'o') {
		if (!*++*argv)
		    argv++;
		if (!*argv) {
		    zerr("string expected after -o", NULL, 0);
		    exit(1);
		}
		if(!(optno = optlookup(*argv)))
		    zerr("no such option: %s", *argv, 0);
		else if (optno == RESTRICTED)
		    restricted = action;
		else
		    dosetopt(optno, action, 1);
		break;
	    } else {
	    	if (!(optno = optlookupc(**argv))) {
		    zerr("bad option: -%c", NULL, **argv);
		    exit(1);
		} else if (optno == RESTRICTED)
		    restricted = action;
		else
		    dosetopt(optno, action, 1);
	    }
	}
	argv++;
    }
    doneoptions:
    paramlist = newlinklist();
    if (*argv) {
	if (unset(SHINSTDIN)) {
	    argzero = *argv;
	    if (!cmd)
		SHIN = movefd(open(unmeta(argzero), O_RDONLY | O_NOCTTY));
	    if (SHIN == -1) {
		zerr("can't open input file: %s", argzero, 0);
		exit(1);
	    }
	    opts[INTERACTIVE] &= 1;
	    argv++;
	}
	while (*argv)
	    addlinknode(paramlist, ztrdup(*argv++));
    } else
	opts[SHINSTDIN] = 1;
    if(isset(SINGLECOMMAND))
	opts[INTERACTIVE] &= 1;
    opts[INTERACTIVE] = !!opts[INTERACTIVE];
    pparams = x = (char **) zcalloc((countlinknodes(paramlist) + 1) * sizeof(char *));

    while ((*x++ = (char *)getlinknode(paramlist)));
    free(paramlist);
    argzero = ztrdup(argzero);
}


/**/
void
init_io(void)
{
    long ttpgrp;
    static char outbuf[BUFSIZ], errbuf[BUFSIZ];

#ifdef RSH_BUG_WORKAROUND
    int i;
#endif

/* stdout, stderr fully buffered */
#ifdef _IOFBF
    setvbuf(stdout, outbuf, _IOFBF, BUFSIZ);
    setvbuf(stderr, errbuf, _IOFBF, BUFSIZ);
#else
    setbuffer(stdout, outbuf, BUFSIZ);
    setbuffer(stderr, errbuf, BUFSIZ);
#endif

/* This works around a bug in some versions of in.rshd. *
 * Currently this is not defined by default.            */
#ifdef RSH_BUG_WORKAROUND
    if (cmd) {
	for (i = 3; i < 10; i++)
	    close(i);
    }
#endif

    if (shout) {
	fclose(shout);
	shout = 0;
    }
    if (SHTTY != -1) {
	zclose(SHTTY);
	SHTTY = -1;
    }

    /* Make sure the tty is opened read/write. */
    if (isatty(0)) {
	zsfree(ttystrname);
	if ((ttystrname = ztrdup(ttyname(0))))
	    SHTTY = movefd(open(ttystrname, O_RDWR | O_NOCTTY));
    }
    if (SHTTY == -1 &&
	(SHTTY = movefd(open("/dev/tty", O_RDWR | O_NOCTTY))) != -1) {
	zsfree(ttystrname);
	ttystrname = ztrdup("/dev/tty");
    }
    if (SHTTY == -1) {
	zsfree(ttystrname);
	ttystrname = ztrdup("");
    }

    /* We will only use zle if shell is interactive, *
     * SHTTY != -1, and shout != 0                   */
    if (interact && SHTTY != -1) {
	init_shout();
	if(!shout)
	    opts[USEZLE] = 0;
    } else
	opts[USEZLE] = 0;

#ifdef JOB_CONTROL
    /* If interactive, make the shell the foreground process */
    if (opts[MONITOR] && interact && (SHTTY != -1)) {
	attachtty(GETPGRP());
	if ((mypgrp = GETPGRP()) > 0) {
	    while ((ttpgrp = gettygrp()) != -1 && ttpgrp != mypgrp) {
		sleep(1);
		mypgrp = GETPGRP();
		if (mypgrp == gettygrp())
		    break;
		killpg(mypgrp, SIGTTIN);
		mypgrp = GETPGRP();
	    }
	} else
	    opts[MONITOR] = 0;
    } else
	opts[MONITOR] = 0;
#else
    opts[MONITOR] = 0;
#endif
}

/**/
void
init_shout(void)
{
    static char shoutbuf[BUFSIZ];
#if defined(JOB_CONTROL) && defined(TIOCSETD) && defined(NTTYDISC)
    int ldisc = NTTYDISC;

    ioctl(SHTTY, TIOCSETD, (char *)&ldisc);
#endif

    /* Associate terminal file descriptor with a FILE pointer */
    shout = fdopen(SHTTY, "w");
#ifdef _IOFBF
    setvbuf(shout, shoutbuf, _IOFBF, BUFSIZ);
#endif
  
    gettyinfo(&shttyinfo);	/* get tty state */
#if defined(__sgi)
    if (shttyinfo.tio.c_cc[VSWTCH] <= 0)	/* hack for irises */
	shttyinfo.tio.c_cc[VSWTCH] = CSWTCH;
#endif
}

/* names of the termcap strings we want */

static char *tccapnams[TC_COUNT] = {
    "cl", "le", "LE", "nd", "RI", "up", "UP", "do",
    "DO", "dc", "DC", "ic", "IC", "cd", "ce", "al", "dl", "ta",
    "md", "so", "us", "me", "se", "ue"
};

/* Initialise termcap */

/**/
int
init_term(void)
{
#ifndef TGETENT_ACCEPTS_NULL
    static char termbuf[2048];	/* the termcap buffer */
#endif

    if (!*term) {
	termflags |= TERM_UNKNOWN;
	return 0;
    }

    /* unset zle if using zsh under emacs */
    if (!strcmp(term, "emacs"))
	opts[USEZLE] = 0;

#ifdef TGETENT_ACCEPTS_NULL
    /* If possible, we let tgetent allocate its own termcap buffer */
    if (tgetent(NULL, term) != 1) {
#else
    if (tgetent(termbuf, term) != 1) {
#endif

	if (isset(INTERACTIVE))
	    zerr("can't find termcap info for %s", term, 0);
	errflag = 0;
	termflags |= TERM_BAD;
	return 0;
    } else {
	char tbuf[1024], *pp;
	int t0;

	termflags &= ~TERM_BAD;
	termflags &= ~TERM_UNKNOWN;
	for (t0 = 0; t0 != TC_COUNT; t0++) {
	    pp = tbuf;
	    zsfree(tcstr[t0]);
	/* AIX tgetstr() ignores second argument */
	    if (!(pp = tgetstr(tccapnams[t0], &pp)))
		tcstr[t0] = NULL, tclen[t0] = 0;
	    else {
		tclen[t0] = strlen(pp);
		tcstr[t0] = (char *) zalloc(tclen[t0] + 1);
		memcpy(tcstr[t0], pp, tclen[t0] + 1);
	    }
	}

	/* check whether terminal has automargin (wraparound) capability */
	hasam = tgetflag("am");

	tclines = tgetnum("li");
	tccolumns = tgetnum("co");

	/* if there's no termcap entry for cursor up, use single line mode: *
	 * this is flagged by termflags which is examined in zle_refresh.c  *
	 */
	if (tccan(TCUP))
	    termflags &= ~TERM_NOUP;
	else {
	    tcstr[TCUP] = NULL;
	    termflags |= TERM_NOUP;
	}

	/* if there's no termcap entry for cursor left, use \b. */
	if (!tccan(TCLEFT)) {
	    tcstr[TCLEFT] = ztrdup("\b");
	    tclen[TCLEFT] = 1;
	}

	/* if the termcap entry for down is \n, don't use it. */
	if (tccan(TCDOWN) && tcstr[TCDOWN][0] == '\n') {
	    tclen[TCDOWN] = 0;
	    zsfree(tcstr[TCDOWN]);
	    tcstr[TCDOWN] = NULL;
	}

	/* if there's no termcap entry for clear, use ^L. */
	if (!tccan(TCCLEARSCREEN)) {
	    tcstr[TCCLEARSCREEN] = ztrdup("\14");
	    tclen[TCCLEARSCREEN] = 1;
	}
    }
    return 1;
}

/* Initialize lots of global variables and hash tables */

/**/
void
setupvals(void)
{
#ifdef HAVE_GETPWUID
    struct passwd *pswd;
#endif
    struct timezone dummy_tz;
    char *ptr;
#ifdef HAVE_GETRLIMIT
    int i;
#endif

    noeval = 0;
    curhist = 0;
    histsiz = DEFAULT_HISTSIZE;
    inithist();

    cmdstack = (unsigned char *) zalloc(256);
    cmdsp = 0;

    bangchar = '!';
    hashchar = '#';
    hatchar = '^';
    termflags = TERM_UNKNOWN;
    curjob = prevjob = coprocin = coprocout = -1;
    gettimeofday(&shtimer, &dummy_tz);	/* init $SECONDS */
    srand((unsigned int)(shtimer.tv_sec + shtimer.tv_usec)); /* seed $RANDOM */

    hostnam     = (char *) zalloc(256);
    gethostname(hostnam, 256);

    /* Set default path */
    path    = (char **) zalloc(sizeof(*path) * 5);
    path[0] = ztrdup("/bin");
    path[1] = ztrdup("/usr/bin");
    path[2] = ztrdup("/usr/ucb");
    path[3] = ztrdup("/usr/local/bin");
    path[4] = NULL;

    cdpath   = mkarray(NULL);
    manpath  = mkarray(NULL);
    fignore  = mkarray(NULL);
    fpath    = mkarray(NULL);
    mailpath = mkarray(NULL);
    watch    = mkarray(NULL);
    psvar    = mkarray(NULL);
#ifdef DYNAMIC
    module_path = mkarray(ztrdup(MODULE_DIR));
    modules = newlinklist();
#endif

    /* Set default prompts */
    if(unset(INTERACTIVE)) {
	prompt = ztrdup("");
	prompt2 = ztrdup("");
    } else if (emulation == EMULATE_KSH || emulation == EMULATE_SH) {
	prompt  = ztrdup(privasserted() ? "# " : "$ ");
	prompt2 = ztrdup("> ");
    } else {
	prompt  = ztrdup("%m%# ");
	prompt2 = ztrdup("%_> ");
    }
    prompt3 = ztrdup("?# ");
    prompt4 = ztrdup("+ ");
    sprompt = ztrdup("zsh: correct '%R' to '%r' [nyae]? ");

    ifs         = ztrdup(DEFAULT_IFS);
    wordchars   = ztrdup(DEFAULT_WORDCHARS);
    postedit    = ztrdup("");
    underscore  = ztrdup("");

    zoptarg = ztrdup("");
    zoptind = 1;

    ppid  = (long) getppid();
    mypid = (long) getpid();
    term  = ztrdup("");

    /* The following variable assignments cause zsh to behave more *
     * like Bourne and Korn shells when invoked as "sh" or "ksh".  *
     * NULLCMD=":" and READNULLCMD=":"                             */

    if (emulation == EMULATE_KSH || emulation == EMULATE_SH) {
	nullcmd     = ztrdup(":");
	readnullcmd = ztrdup(":");
    } else {
	nullcmd     = ztrdup("cat");
	readnullcmd = ztrdup("more");
    }

    /* We cache the uid so we know when to *
     * recheck the info for `USERNAME'     */
    cached_uid = getuid();

    /* Get password entry and set info for `HOME' and `USERNAME' */
#ifdef HAVE_GETPWUID
    if ((pswd = getpwuid(cached_uid))) {
	home = metafy(pswd->pw_dir, -1, META_DUP);
	cached_username = ztrdup(pswd->pw_name);
    } else
#endif /* HAVE_GETPWUID */
	   {
	home = ztrdup("/");
	cached_username = ztrdup("");
    }

    /* Try a cheap test to see if we can *
     * initialize `PWD' from `HOME'      */
    if (ispwd(home))
	pwd = ztrdup(home);
    else if ((ptr = zgetenv("PWD")) && ispwd(ptr))
	pwd = ztrdup(ptr);
    else
	pwd = metafy(zgetcwd(), -1, META_DUP);

    oldpwd = ztrdup(pwd);  /* initialize `OLDPWD' = `PWD' */

    inittyptab();     /* initialize the ztypes table */
    initlextabs();    /* initialize lexing tables    */

    createreswdtable();     /* create hash table for reserved words    */
    createaliastable();     /* create hash table for aliases           */
    createcmdnamtable();    /* create hash table for external commands */
    createshfunctable();    /* create hash table for shell functions   */
    createbuiltintable();   /* create hash table for builtin commands  */
    createnameddirtable();  /* create hash table for named directories */
    createparamtable();     /* create paramater hash table             */

#ifdef TIOCGWINSZ
    adjustwinsize();
#else
    /* Using zero below sets the defaults from termcap */
    setiparam("COLUMNS", 0);
    setiparam("LINES", 0);
#endif

#ifdef HAVE_GETRLIMIT
    for (i = 0; i != RLIM_NLIMITS; i++) {
	getrlimit(i, current_limits + i);
	limits[i] = current_limits[i];
    }
#endif

    breaks = loops = 0;
    lastmailcheck = time(NULL);
    locallevel = sourcelevel = 0;
    trapreturn = 0;
    noerrexit = -1;
    nohistsave = 1;
    dirstack = newlinklist();
    bufstack = newlinklist();
    prepromptfns = newlinklist();
    hsubl = hsubr = NULL;
    lastpid = 0;
    bshin = SHIN ? fdopen(SHIN, "r") : stdin;
    if (isset(SHINSTDIN) && !SHIN && unset(INTERACTIVE)) {
#ifdef _IONBF
	setvbuf(stdin, NULL, _IONBF, 0);
#else
	setlinebuf(stdin);
#endif
    }

    times(&shtms);
}

/* Initialize signal handling */

/**/
void
init_signals(void)
{
    intr();

#ifndef QDEBUG
    signal_ignore(SIGQUIT);
#endif

    install_handler(SIGHUP);
    install_handler(SIGCHLD);
#ifdef SIGWINCH
    install_handler(SIGWINCH);
#endif
    if (interact) {
	install_handler(SIGALRM);
	signal_ignore(SIGTERM);
    }
    if (jobbing) {
	long ttypgrp;

	while ((ttypgrp = gettygrp()) != -1 && ttypgrp != mypgrp)
	    kill(0, SIGTTIN);
	if (ttypgrp == -1) {
	    opts[MONITOR] = 0;
	} else {
	    signal_ignore(SIGTTOU);
	    signal_ignore(SIGTSTP);
	    signal_ignore(SIGTTIN);
	    attachtty(mypgrp);
	}
    }
    if (islogin) {
	signal_setmask(signal_mask(0));
    } else if (interact) {
	sigset_t set;

	sigemptyset(&set);
	sigaddset(&set, SIGINT);
	sigaddset(&set, SIGQUIT);
	signal_unblock(set);
    }
}

/* Source the init scripts.  If called as "ksh" or "sh"  *
 * then we source the standard sh/ksh scripts instead of *
 * the standard zsh scripts                              */

/**/
void
run_init_scripts(void)
{
    noerrexit = -1;

    if (emulation == EMULATE_KSH || emulation == EMULATE_SH) {
	if (islogin)
	    source("/etc/profile");
	if (unset(PRIVILEGED)) {
	    char *s = getsparam("ENV");
	    if (islogin)
		sourcehome(".profile");
	    noerrs = 1;
	    if (s && !parsestr(s)) {
		singsub(&s);
		noerrs = 0;
		source(s);
	    }
	    noerrs = 0;
	} else
	    source("/etc/suid_profile");
    } else {
#ifdef GLOBAL_ZSHENV
	source(GLOBAL_ZSHENV);
#endif
	if (isset(RCS)) {
	    if (unset(PRIVILEGED))
		sourcehome(".zshenv");
	    if (islogin) {
#ifdef GLOBAL_ZPROFILE
		source(GLOBAL_ZPROFILE);
#endif
		if (unset(PRIVILEGED))
		    sourcehome(".zprofile");
	    }
	    if (interact) {
#ifdef GLOBAL_ZSHRC
		source(GLOBAL_ZSHRC);
#endif
		if (unset(PRIVILEGED))
		    sourcehome(".zshrc");
	    }
	    if (islogin) {
#ifdef GLOBAL_ZLOGIN
		source(GLOBAL_ZLOGIN);
#endif
		if (unset(PRIVILEGED))
		    sourcehome(".zlogin");
	    }
	}
    }
    noerrexit = 0;
    nohistsave = 0;
}

/* Miscellaneous initializations that happen after init scripts are run */

/**/
void
init_misc(void)
{
    if (*zsh_name == 'r' || restricted)
	dosetopt(RESTRICTED, 1, 0);
    if (cmd) {
	if (SHIN >= 10)
	    fclose(bshin);
	SHIN = movefd(open("/dev/null", O_RDONLY | O_NOCTTY));
	bshin = fdopen(SHIN, "r");
	execstring(cmd, 0, 1);
	stopmsg = 1;
	zexit(lastval, 0);
    }

    if (interact && isset(RCS))
	readhistfile(getsparam("HISTFILE"), 0);
}

/* source a file */

/**/
int
source(char *s)
{
    int tempfd, fd, cj, oldlineno;
    int oldshst, osubsh, oloops;
    FILE *obshin;
    char *old_scriptname = scriptname;

    if (!s || (tempfd = movefd(open(unmeta(s), O_RDONLY | O_NOCTTY))) == -1) {
	return 1;
    }

    /* save the current shell state */
    fd        = SHIN;            /* store the shell input fd                  */
    obshin    = bshin;          /* store file handle for buffered shell input */
    osubsh    = subsh;           /* store whether we are in a subshell        */
    cj        = thisjob;         /* store our current job number              */
    oldlineno = lineno;          /* store our current lineno                  */
    oloops    = loops;           /* stored the # of nested loops we are in    */
    oldshst   = opts[SHINSTDIN]; /* store current value of this option        */

    SHIN = tempfd;
    bshin = fdopen(SHIN, "r");
    subsh  = 0;
    lineno = 0;
    loops  = 0;
    dosetopt(SHINSTDIN, 0, 1);
    scriptname = s;

    sourcelevel++;
    loop(0, 0);			/* loop through the file to be sourced        */
    sourcelevel--;
    fclose(bshin);
    fdtable[SHIN] = 0;

    /* restore the current shell state */
    SHIN = fd;                       /* the shell input fd                   */
    bshin = obshin;                  /* file handle for buffered shell input */
    subsh = osubsh;                  /* whether we are in a subshell         */
    thisjob = cj;                    /* current job number                   */
    lineno = oldlineno;              /* our current lineno                   */
    loops = oloops;                  /* the # of nested loops we are in      */
    dosetopt(SHINSTDIN, oldshst, 1); /* SHINSTDIN option                     */
    errflag = 0;
    retflag = 0;
    scriptname = old_scriptname;

    return 0;
}

/* Try to source a file in the home directory */

/**/
void
sourcehome(char *s)
{
    char buf[PATH_MAX];
    char *h;

    if (emulation == EMULATE_SH || emulation == EMULATE_KSH ||
	!(h = getsparam("ZDOTDIR")))
	h = home;
    if (strlen(h) + strlen(s) + 1 >= PATH_MAX) {
	zerr("path too long: %s", s, 0);
	return;
    }
    sprintf(buf, "%s/%s", h, s);
    source(buf);
}

/**/
void
init_bltinmods(void)
{
    static struct module mod = { NULL, 0, NULL, NULL };
#include "bltinmods.list"
    mod.nam = NULL;
}

/* ZLE entry point pointers.  They are defined here because the initial *
 * values depend on whether ZLE is linked in or not -- if it is, we     *
 * avoid wasting space with the fallback functions.  No other source    *
 * file needs to know which modules are linked in.                      */

#ifdef LINKED_XMOD_zle

/**/
ZleVoidFn trashzleptr;
/**/
ZleVoidFn gotwordptr;
/**/
ZleVoidFn refreshptr;
/**/
ZleVoidIntFn spaceinlineptr;
/**/
ZleReadFn zlereadptr;

#else /* !LINKED_XMOD_zle */

ZleVoidFn trashzleptr = noop_function;
ZleVoidFn gotwordptr = noop_function;
ZleVoidFn refreshptr = noop_function;
ZleVoidIntFn spaceinlineptr = noop_function_int;
# ifdef UNLINKED_XMOD_zle
ZleReadFn zlereadptr = autoload_zleread;
# else /* !UNLINKED_XMOD_zle */
ZleReadFn zlereadptr = fallback_zleread;
# endif /* !UNLINKED_XMOD_zle */

/**/
void
noop_function(void)
{
    /* do nothing */
}

/**/
void
noop_function_int(int nothing)
{
    /* do nothing */
}

# ifdef UNLINKED_XMOD_zle

/**/
static unsigned char *
autoload_zleread(char *lp, char *rp, int ha)
{
    zlereadptr = fallback_zleread;
    load_module("zle");
    return zleread(lp, rp, ha);
}

# endif /* UNLINKED_XMOD_zle */

/**/
unsigned char *
fallback_zleread(char *lp, char *rp, int ha)
{
    char *pptbuf;
    int pptlen;

    pptbuf = unmetafy(promptexpand(lp, 0, NULL, NULL), &pptlen);
    write(2, (WRITE_ARG_2_T)pptbuf, pptlen);
    free(pptbuf);
    return (unsigned char *)shingetline();
}

#endif /* !LINKED_XMOD_zle */

/* compctl entry point pointers.  Similar to the ZLE ones. */

#ifdef LINKED_XMOD_comp1

/**/
CompctlReadFn compctlreadptr;

#else /* !LINKED_XMOD_comp1 */

CompctlReadFn compctlreadptr = fallback_compctlread;

/**/
int
fallback_compctlread(char *name, char **args, char *ops, char *reply)
{
    zwarnnam(name, "option valid only in functions called from completion",
	    NULL, 0);
    return 1;
}

#endif /* !LINKED_XMOD_comp1 */
