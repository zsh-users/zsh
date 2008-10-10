/*
 * exec.c - command execution
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
#include "exec.pro"

/* Flags for last argument of addvars */

enum {
    /* Export the variable for "VAR=val cmd ..." */
    ADDVAR_EXPORT =   1 << 0,
    /* Apply restrictions for variable */
    ADDVAR_RESTRICT = 1 << 1,
    /* Variable list is being restored later */
    ADDVAR_RESTORE =  1 << 2
};

/* used to suppress ERREXIT and trapping of SIGZERR, SIGEXIT. */

/**/
int noerrexit;

/*
 * noerrs = 1: suppress error messages
 * noerrs = 2: don't set errflag on parse error, either
 */
 
/**/
mod_export int noerrs;
 
/* do not save history on exec and exit */

/**/
int nohistsave;
 
/* error/break flag */
 
/**/
mod_export int errflag;
 
/*
 * State of trap return value.  Value is from enum trap_state.
 */

/**/
int trap_state;

/*
 * Value associated with return from a trap.
 * This is only active if we are inside a trap, else its value
 * is irrelevant.  It is initialised to -1 for a function trap and
 * -2 for a non-function trap and if negative is decremented as
 * we go deeper into functions and incremented as we come back up.
 * The value is used to decide if an explicit "return" should cause
 * a return from the caller of the trap; it does this by setting
 * trap_return to a status (i.e. a non-negative value).
 *
 * In summary, trap_return is
 * - zero unless we are in a trap
 * - negative in a trap unless it has triggered.  Code uses this
 *   to detect an active trap.
 * - non-negative in a trap once it was triggered.  It should remain
 *   non-negative until restored after execution of the trap.
 */
 
/**/
int trap_return;
 
/* != 0 if this is a subshell */
 
/**/
int subsh;
 
/* != 0 if we have a return pending */
 
/**/
mod_export int retflag;

/**/
long lastval2;
 
/* The table of file descriptors.  A table element is zero if the  *
 * corresponding fd is not used by the shell.  It is greater than  *
 * 1 if the fd is used by a <(...) or >(...) substitution and 1 if *
 * it is an internal file descriptor which must be closed before   *
 * executing an external command.  The first ten elements of the   *
 * table is not used.  A table element is set by movefd and cleard *
 * by zclose.                                                      */

/**/
mod_export unsigned char *fdtable;

/* The allocated size of fdtable */

/**/
int fdtable_size;

/* The highest fd that marked with nonzero in fdtable */

/**/
mod_export int max_zsh_fd;

/* input fd from the coprocess */

/**/
mod_export int coprocin;

/* output fd from the coprocess */

/**/
mod_export int coprocout;

/* != 0 if the line editor is active */

/**/
mod_export int zleactive;

/* pid of process undergoing 'process substitution' */
 
/**/
pid_t cmdoutpid;
 
/* exit status of process undergoing 'process substitution' */
 
/**/
int cmdoutval;

/* The context in which a shell function is called, see SFC_* in zsh.h. */ 

/**/
mod_export int sfcontext;

/* Stack to save some variables before executing a signal handler function */

/**/
struct execstack *exstack;

/* Stack with names of functions currently active. */

/**/
mod_export Funcstack funcstack;

#define execerr() if (!forked) { lastval = 1; goto done; } else _exit(1)

static int doneps4;
static char *STTYval;
static char *blank_env[] = { NULL };

/* Execution functions. */

static int (*execfuncs[WC_COUNT-WC_CURSH]) _((Estate, int)) = {
    execcursh, exectime, execfuncdef, execfor, execselect,
    execwhile, execrepeat, execcase, execif, execcond,
    execarith, execautofn, exectry
};

/* structure for command builtin for when it is used with -v or -V */
static struct builtin commandbn =
    BUILTIN(0, 0, bin_whence, 0, -1, BIN_COMMAND, "vV", NULL);

/* parse string into a list */

/**/
mod_export Eprog
parse_string(char *s, int reset_lineno)
{
    Eprog p;
    zlong oldlineno;

    lexsave();
    inpush(s, INP_LINENO, NULL);
    strinbeg(0);
    oldlineno = lineno;
    if (reset_lineno)
	lineno = 1;
    p = parse_list();
    lineno = oldlineno;
    if (tok == LEXERR && !lastval)
	lastval = 1;
    strinend();
    inpop();
    lexrestore();
    return p;
}

/**/
#ifdef HAVE_GETRLIMIT

/* the resource limits for the shell and its children */

/**/
mod_export struct rlimit current_limits[RLIM_NLIMITS], limits[RLIM_NLIMITS];
 
/**/
mod_export int
zsetlimit(int limnum, char *nam)
{
    if (limits[limnum].rlim_max != current_limits[limnum].rlim_max ||
	limits[limnum].rlim_cur != current_limits[limnum].rlim_cur) {
	if (setrlimit(limnum, limits + limnum)) {
	    if (nam)
		zwarnnam(nam, "setrlimit failed: %e", errno);
	    return -1;
	}
	current_limits[limnum] = limits[limnum];
    }
    return 0;
}

/**/
mod_export int
setlimits(char *nam)
{
    int limnum;
    int ret = 0;

    for (limnum = 0; limnum < RLIM_NLIMITS; limnum++)
	if (zsetlimit(limnum, nam))
	    ret++;
    return ret;
}

/**/
#endif /* HAVE_GETRLIMIT */

/* fork and set limits */

/**/
static pid_t
zfork(struct timeval *tv)
{
    pid_t pid;
    struct timezone dummy_tz;

    /*
     * Is anybody willing to explain this test?
     */
    if (thisjob != -1 && thisjob >= jobtabsize - 1 && !expandjobtab()) {
	zerr("job table full");
	return -1;
    }
    if (tv)
	gettimeofday(tv, &dummy_tz);
    /*
     * Queueing signals is necessary on Linux because fork()
     * manipulates mutexes, leading to deadlock in memory
     * allocation.  We don't expect fork() to be particularly
     * zippy anyway.
     */
    queue_signals();
    pid = fork();
    unqueue_signals();
    if (pid == -1) {
	zerr("fork failed: %e", errno);
	return -1;
    }
#ifdef HAVE_GETRLIMIT
    if (!pid)
	/* set resource limits for the child process */
	setlimits(NULL);
#endif
    return pid;
}

/*
 *   Allen Edeln gebiet ich Andacht,
 *   Hohen und Niedern von Heimdalls Geschlecht;
 *   Ich will list_pipe's Wirken kuenden
 *   Die aeltesten Sagen, der ich mich entsinne...
 *
 * In most shells, if you do something like:
 *
 *   cat foo | while read a; do grep $a bar; done
 *
 * the shell forks and executes the loop in the sub-shell thus created.
 * In zsh this traditionally executes the loop in the current shell, which
 * is nice to have if the loop does something to change the shell, like
 * setting parameters or calling builtins.
 * Putting the loop in a sub-shell makes life easy, because the shell only
 * has to put it into the job-structure and then treats it as a normal
 * process. Suspending and interrupting is no problem then.
 * Some years ago, zsh either couldn't suspend such things at all, or
 * it got really messed up when users tried to do it. As a solution, we
 * implemented the list_pipe-stuff, which has since then become a reason
 * for many nightmares.
 * Pipelines like the one above are executed by the functions in this file
 * which call each other (and sometimes recursively). The one above, for
 * example would lead to a function call stack roughly like:
 *
 *  execlist->execpline->execcmd->execwhile->execlist->execpline
 *
 * (when waiting for the grep, ignoring execpline2 for now). At this time,
 * zsh has built two job-table entries for it: one for the cat and one for
 * the grep. If the user hits ^Z at this point (and jobbing is used), the 
 * shell is notified that the grep was suspended. The list_pipe flag is
 * used to tell the execpline where it was waiting that it was in a pipeline
 * with a shell construct at the end (which may also be a shell function or
 * several other things). When zsh sees the suspended grep, it forks to let
 * the sub-shell execute the rest of the while loop. The parent shell walks
 * up in the function call stack to the first execpline. There it has to find
 * out that it has just forked and then has to add information about the sub-
 * shell (its pid and the text for it) in the job entry of the cat. The pid
 * is passed down in the list_pipe_pid variable.
 * But there is a problem: the suspended grep is a child of the parent shell
 * and can't be adopted by the sub-shell. So the parent shell also has to 
 * keep the information about this process (more precisely: this pipeline)
 * by keeping the job table entry it created for it. The fact that there
 * are two jobs which have to be treated together is remembered by setting
 * the STAT_SUPERJOB flag in the entry for the cat-job (which now also
 * contains a process-entry for the whole loop -- the sub-shell) and by
 * setting STAT_SUBJOB in the job of the grep-job. With that we can keep
 * sub-jobs from being displayed and we can handle an fg/bg on the super-
 * job correctly. When the super-job is continued, the shell also wakes up
 * the sub-job. But then, the grep will exit sometime. Now the parent shell
 * has to remember not to try to wake it up again (in case of another ^Z).
 * It also has to wake up the sub-shell (which suspended itself immediately
 * after creation), so that the rest of the loop is executed by it.
 * But there is more: when the sub-shell is created, the cat may already
 * have exited, so we can't put the sub-shell in the process group of it.
 * In this case, we put the sub-shell in the process group of the parent
 * shell and in any case, the sub-shell has to put all commands executed
 * by it into its own process group, because only this way the parent
 * shell can control them since it only knows the process group of the sub-
 * shell. Of course, this information is also important when putting a job
 * in the foreground, where we have to attach its process group to the
 * controlling tty.
 * All this is made more difficult because we have to handle return values
 * correctly. If the grep is signaled, its exit status has to be propagated
 * back to the parent shell which needs it to set the exit status of the
 * super-job. And of course, when the grep is signaled (including ^C), the
 * loop has to be stopped, etc.
 * The code for all this is distributed over three files (exec.c, jobs.c,
 * and signals.c) and none of them is a simple one. So, all in all, there
 * may still be bugs, but considering the complexity (with race conditions,
 * signal handling, and all that), this should probably be expected.
 */

/**/
int list_pipe = 0, simple_pline = 0;

static pid_t list_pipe_pid;
static struct timeval list_pipe_start;
static int nowait, pline_level = 0;
static int list_pipe_child = 0, list_pipe_job;
static char list_pipe_text[JOBTEXTSIZE];

/* execute a current shell command */

/**/
static int
execcursh(Estate state, int do_exec)
{
    Wordcode end = state->pc + WC_CURSH_SKIP(state->pc[-1]);

    /* Skip word only used for try/always */
    state->pc++;

    if (!list_pipe && thisjob != list_pipe_job && !hasprocs(thisjob))
	deletejob(jobtab + thisjob);
    cmdpush(CS_CURSH);
    execlist(state, 1, do_exec);
    cmdpop();

    state->pc = end;

    return lastval;
}

/* execve after handling $_ and #! */

#define POUNDBANGLIMIT 64

/**/
static int
zexecve(char *pth, char **argv, char **newenvp)
{
    int eno;
    static char buf[PATH_MAX * 2];
    char **eep;

    unmetafy(pth, NULL);
    for (eep = argv; *eep; eep++)
	if (*eep != pth)
	    unmetafy(*eep, NULL);
    buf[0] = '_';
    buf[1] = '=';
    if (*pth == '/')
	strcpy(buf + 2, pth);
    else
	sprintf(buf + 2, "%s/%s", pwd, pth);
    zputenv(buf);
    closedumps();

    if (newenvp == NULL)
	    newenvp = environ;
    execve(pth, argv, newenvp);

    /* If the execve returns (which in general shouldn't happen),   *
     * then check for an errno equal to ENOEXEC.  This errno is set *
     * if the process file has the appropriate access permission,   *
     * but has an invalid magic number in its header.               */
    if ((eno = errno) == ENOEXEC || eno == ENOENT) {
	char execvebuf[POUNDBANGLIMIT + 1], *ptr, *ptr2, *argv0;
	int fd, ct, t0;

	if ((fd = open(pth, O_RDONLY|O_NOCTTY)) >= 0) {
	    argv0 = *argv;
	    *argv = pth;
	    ct = read(fd, execvebuf, POUNDBANGLIMIT);
	    close(fd);
	    if (ct > 0) {
		if (execvebuf[0] == '#') {
		    if (execvebuf[1] == '!') {
			for (t0 = 0; t0 != ct; t0++)
			    if (execvebuf[t0] == '\n')
				break;
			while (inblank(execvebuf[t0]))
			    execvebuf[t0--] = '\0';
			execvebuf[POUNDBANGLIMIT] = '\0';
			for (ptr = execvebuf + 2; *ptr && *ptr == ' '; ptr++);
			for (ptr2 = ptr; *ptr && *ptr != ' '; ptr++);
			if (eno == ENOENT) {
			    if (*ptr)
				*ptr = '\0';
			    zerr("%s: bad interpreter: %s: %e", pth, ptr2,
				 eno);
			} else if (*ptr) {
			    *ptr = '\0';
			    argv[-2] = ptr2;
			    argv[-1] = ptr + 1;
			    execve(ptr2, argv - 2, newenvp);
			} else {
			    argv[-1] = ptr2;
			    execve(ptr2, argv - 1, newenvp);
			}
		    } else if (eno == ENOEXEC) {
			argv[-1] = "sh";
			execve("/bin/sh", argv - 1, newenvp);
		    }
		} else if (eno == ENOEXEC) {
		    for (t0 = 0; t0 != ct; t0++)
			if (!execvebuf[t0])
			    break;
		    if (t0 == ct) {
			argv[-1] = "sh";
			execve("/bin/sh", argv - 1, newenvp);
		    }
		}
	    } else
		eno = errno;
	    *argv = argv0;
	} else
	    eno = errno;
    }
    /* restore the original arguments and path but do not bother with *
     * null characters as these cannot be passed to external commands *
     * anyway.  So the result is truncated at the first null char.    */
    pth = metafy(pth, -1, META_NOALLOC);
    for (eep = argv; *eep; eep++)
	if (*eep != pth)
	    (void) metafy(*eep, -1, META_NOALLOC);
    return eno;
}

#define MAXCMDLEN (PATH_MAX*4)

/* test whether we really want to believe the error number */

/**/
static int
isgooderr(int e, char *dir)
{
    /*
     * Maybe the directory was unreadable, or maybe it wasn't
     * even a directory. 
     */
    return ((e != EACCES || !access(dir, X_OK)) &&
	    e != ENOENT && e != ENOTDIR); 
}

/*
 * Attempt to handle command not found.
 * Return 0 if the condition was handled, non-zero otherwise.
 */

/**/
static int
commandnotfound(char *arg0, LinkList args)
{
    Shfunc shf = (Shfunc)
	shfunctab->getnode(shfunctab, "command_not_found_handler");

    if (!shf)
	return 127;

    pushnode(args, arg0);
    return doshfunc(shf, args, 1);
}

/* execute an external command */

/**/
static void
execute(LinkList args, int flags, int defpath)
{
    Cmdnam cn;
    char buf[MAXCMDLEN], buf2[MAXCMDLEN];
    char *s, *z, *arg0;
    char **argv, **pp, **newenvp = NULL;
    int eno = 0, ee;

    arg0 = (char *) peekfirst(args);
    if (isset(RESTRICTED) && (strchr(arg0, '/') || defpath)) {
	zerr("%s: restricted", arg0);
	_exit(1);
    }

    /* If the parameter STTY is set in the command's environment, *
     * we first run the stty command with the value of this       *
     * parameter as it arguments.                                 */
    if ((s = STTYval) && isatty(0) && (GETPGRP() == getpid())) {
	char *t = tricat("stty", " ", s);

	STTYval = 0;	/* this prevents infinite recursion */
	zsfree(s);
	execstring(t, 1, 0);
	zsfree(t);
    } else if (s) {
	STTYval = 0;
	zsfree(s);
    }

    /* If ARGV0 is in the commands environment, we use *
     * that as argv[0] for this external command       */
    if (unset(RESTRICTED) && (z = zgetenv("ARGV0"))) {
	setdata(firstnode(args), (void *) ztrdup(z));
	/*
	 * Note we don't do anything with the parameter structure
	 * for ARGV0: that's OK since we're about to exec or exit
	 * on failure.
	 */
#ifdef USE_SET_UNSET_ENV
	unsetenv("ARGV0");
#else
	delenvvalue(z - 6);
#endif
    } else if (flags & BINF_DASH) {
    /* Else if the pre-command `-' was given, we add `-' *
     * to the front of argv[0] for this command.         */
	sprintf(buf2, "-%s", arg0);
	setdata(firstnode(args), (void *) ztrdup(buf2));
    }

    argv = makecline(args);
    if (flags & BINF_CLEARENV)
	newenvp = blank_env;

    /*
     * Note that we don't close fd's attached to process substitution
     * here, which should be visible to external processes.
     */
    closem(FDT_XTRACE);
    child_unblock();
    if ((int) strlen(arg0) >= PATH_MAX) {
	zerr("command too long: %s", arg0);
	_exit(1);
    }
    for (s = arg0; *s; s++)
	if (*s == '/') {
	    int lerrno = zexecve(arg0, argv, newenvp);
	    if (arg0 == s || unset(PATHDIRS) ||
		(arg0[0] == '.' && (arg0 + 1 == s ||
				    (arg0[1] == '.' && arg0 + 2 == s)))) {
		zerr("%e: %s", lerrno, arg0);
		_exit((lerrno == EACCES || lerrno == ENOEXEC) ? 126 : 127);
	    }
	    break;
	}

    /* for command -p, search the default path */ 
    if (defpath) {
	char *s, pbuf[PATH_MAX];
	char *dptr, *pe, *ps = DEFAULT_PATH;

	for(;ps;ps = pe ? pe+1 : NULL) {
	    pe = strchr(ps, ':');
	    if (*ps == '/') {
		s = pbuf;
		if (pe)
		    struncpy(&s, ps, pe-ps);
		else
		    strucpy(&s, ps);
		*s++ = '/';
		if ((s - pbuf) + strlen(arg0) >= PATH_MAX)
		    continue;
		strucpy(&s, arg0);
		if (iscom(pbuf))
		    break;
	    }
	}

	if (!ps) {
	    if (commandnotfound(arg0, args) == 0)
		_exit(0);
	    zerr("command not found: %s", arg0);
	    _exit(127);
	}

	ee = zexecve(pbuf, argv, newenvp);

	if ((dptr = strrchr(pbuf, '/')))
	    *dptr = '\0';
	if (isgooderr(ee, *pbuf ? pbuf : "/"))
	    eno = ee;

    } else {
   
	if ((cn = (Cmdnam) cmdnamtab->getnode(cmdnamtab, arg0))) {
	    char nn[PATH_MAX], *dptr;

	    if (cn->node.flags & HASHED)
		strcpy(nn, cn->u.cmd);
	    else {
		for (pp = path; pp < cn->u.name; pp++)
		    if (!**pp || (**pp == '.' && (*pp)[1] == '\0')) {
			ee = zexecve(arg0, argv, newenvp);
			if (isgooderr(ee, *pp))
			    eno = ee;
		    } else if (**pp != '/') {
			z = buf;
			strucpy(&z, *pp);
			*z++ = '/';
			strcpy(z, arg0);
			ee = zexecve(buf, argv, newenvp);
			if (isgooderr(ee, *pp))
			    eno = ee;
		    }
		strcpy(nn, cn->u.name ? *(cn->u.name) : "");
		strcat(nn, "/");
		strcat(nn, cn->node.nam);
	    }
	    ee = zexecve(nn, argv, newenvp);

	    if ((dptr = strrchr(nn, '/')))
		*dptr = '\0';
	    if (isgooderr(ee, *nn ? nn : "/"))
		eno = ee;
	}
	for (pp = path; *pp; pp++)
	    if (!(*pp)[0] || ((*pp)[0] == '.' && !(*pp)[1])) {
		ee = zexecve(arg0, argv, newenvp);
		if (isgooderr(ee, *pp))
		    eno = ee;
	    } else {
		z = buf;
		strucpy(&z, *pp);
		*z++ = '/';
		strcpy(z, arg0);
		ee = zexecve(buf, argv, newenvp);
		if (isgooderr(ee, *pp))
		    eno = ee;
	    }
    }

    if (eno)
	zerr("%e: %s", eno, arg0);
    else if (commandnotfound(arg0, args) == 0)
	_exit(0);
    else
	zerr("command not found: %s", arg0);
    _exit((eno == EACCES || eno == ENOEXEC) ? 126 : 127);
}

#define RET_IF_COM(X) { if (iscom(X)) return docopy ? dupstring(X) : arg0; }

/*
 * Get the full pathname of an external command.
 * If the second argument is zero, return the first argument if found;
 * if non-zero, return the path using heap memory.  (RET_IF_COM(X), above).
 */

/**/
mod_export char *
findcmd(char *arg0, int docopy)
{
    char **pp;
    char *z, *s, buf[MAXCMDLEN];
    Cmdnam cn;

    cn = (Cmdnam) cmdnamtab->getnode(cmdnamtab, arg0);
    if (!cn && isset(HASHCMDS))
	cn = hashcmd(arg0, path);
    if ((int) strlen(arg0) > PATH_MAX)
	return NULL;
    for (s = arg0; *s; s++)
	if (*s == '/') {
	    RET_IF_COM(arg0);
	    if (arg0 == s || unset(PATHDIRS)) {
		return NULL;
	    }
	    break;
	}
    if (cn) {
	char nn[PATH_MAX];

	if (cn->node.flags & HASHED)
	    strcpy(nn, cn->u.cmd);
	else {
	    for (pp = path; pp < cn->u.name; pp++)
		if (**pp != '/') {
		    z = buf;
		    if (**pp) {
			strucpy(&z, *pp);
			*z++ = '/';
		    }
		    strcpy(z, arg0);
		    RET_IF_COM(buf);
		}
	    strcpy(nn, cn->u.name ? *(cn->u.name) : "");
	    strcat(nn, "/");
	    strcat(nn, cn->node.nam);
	}
	RET_IF_COM(nn);
    }
    for (pp = path; *pp; pp++) {
	z = buf;
	if (**pp) {
	    strucpy(&z, *pp);
	    *z++ = '/';
	}
	strcpy(z, arg0);
	RET_IF_COM(buf);
    }
    return NULL;
}

/**/
int
iscom(char *s)
{
    struct stat statbuf;
    char *us = unmeta(s);

    return (access(us, X_OK) == 0 && stat(us, &statbuf) >= 0 &&
	    S_ISREG(statbuf.st_mode));
}

/**/
int
isreallycom(Cmdnam cn)
{
    char fullnam[MAXCMDLEN];

    if (cn->node.flags & HASHED)
	strcpy(fullnam, cn->u.cmd);
    else if (!cn->u.name)
	return 0;
    else {
	strcpy(fullnam, *(cn->u.name));
	strcat(fullnam, "/");
	strcat(fullnam, cn->node.nam);
    }
    return iscom(fullnam);
}

/**/
int
isrelative(char *s)
{
    if (*s != '/')
	return 1;
    for (; *s; s++)
	if (*s == '.' && s[-1] == '/' &&
	    (s[1] == '/' || s[1] == '\0' ||
	     (s[1] == '.' && (s[2] == '/' || s[2] == '\0'))))
	    return 1;
    return 0;
}

/**/
mod_export Cmdnam
hashcmd(char *arg0, char **pp)
{
    Cmdnam cn;
    char *s, buf[PATH_MAX];
    char **pq;

    for (; *pp; pp++)
	if (**pp == '/') {
	    s = buf;
	    strucpy(&s, *pp);
	    *s++ = '/';
	    if ((s - buf) + strlen(arg0) >= PATH_MAX)
		continue;
	    strcpy(s, arg0);
	    if (iscom(buf))
		break;
	}

    if (!*pp)
	return NULL;

    cn = (Cmdnam) zshcalloc(sizeof *cn);
    cn->node.flags = 0;
    cn->u.name = pp;
    cmdnamtab->addnode(cmdnamtab, ztrdup(arg0), cn);

    if (isset(HASHDIRS)) {
	for (pq = pathchecked; pq <= pp; pq++)
	    hashdir(pq);
	pathchecked = pp + 1;
    }

    return cn;
}

/**/
int
forklevel;

/* Arguments to entersubsh() */
enum {
    /* Subshell is to be run asynchronously (else synchronously) */
    ESUB_ASYNC = 0x01,
    /*
     * Perform process group and tty handling and clear the
     * (real) job table, since it won't be any longer valid
     */
    ESUB_PGRP = 0x02,
    /* Don't unset traps */
    ESUB_KEEPTRAP = 0x04,
    /* This is only a fake entry to a subshell */
    ESUB_FAKE = 0x08,
    /* Release the process group if pid is the shell's process group */
    ESUB_REVERTPGRP = 0x10,
    /* Don't handle the MONITOR option even if previously set */
    ESUB_NOMONITOR = 0x20
};

/**/
static void
entersubsh(int flags)
{
    int sig, monitor;

    if (!(flags & ESUB_KEEPTRAP))
	for (sig = 0; sig < VSIGCOUNT; sig++)
	    if (!(sigtrapped[sig] & ZSIG_FUNC) &&
		sig != SIGDEBUG && sig != SIGZERR)
		unsettrap(sig);
    monitor = isset(MONITOR);
    if (flags & ESUB_NOMONITOR)
	opts[MONITOR] = 0;
    if (!isset(MONITOR)) {
	if (flags & ESUB_ASYNC) {
	    settrap(SIGINT, NULL, 0);
	    settrap(SIGQUIT, NULL, 0);
	    if (isatty(0)) {
		close(0);
		if (open("/dev/null", O_RDWR | O_NOCTTY)) {
		    zerr("can't open /dev/null: %e", errno);
		    _exit(1);
		}
	    }
	}
    } else if (thisjob != -1 && (flags & ESUB_PGRP)) {
	if (jobtab[list_pipe_job].gleader && (list_pipe || list_pipe_child)) {
	    if (setpgrp(0L, jobtab[list_pipe_job].gleader) == -1 ||
		killpg(jobtab[list_pipe_job].gleader, 0) == -1) {
		jobtab[list_pipe_job].gleader =
		    jobtab[thisjob].gleader = (list_pipe_child ? mypgrp : getpid());
		setpgrp(0L, jobtab[list_pipe_job].gleader);
		if (!(flags & ESUB_ASYNC))
		    attachtty(jobtab[thisjob].gleader);
	    }
	}
	else if (!jobtab[thisjob].gleader ||
		 setpgrp(0L, jobtab[thisjob].gleader) == -1) {
	    jobtab[thisjob].gleader = getpid();
	    if (list_pipe_job != thisjob &&
		!jobtab[list_pipe_job].gleader)
		jobtab[list_pipe_job].gleader = jobtab[thisjob].gleader;
	    setpgrp(0L, jobtab[thisjob].gleader);
	    if (!(flags & ESUB_ASYNC))
		attachtty(jobtab[thisjob].gleader);
	}
    }
    if (!(flags & ESUB_FAKE))
	subsh = 1;
    /*
     * Increment the visible parameter ZSH_SUBSHELL even if this
     * is a fake subshell because we are exec'ing at the end.
     * Logically this should be equivalent to a real subshell so
     * we don't hang out the dirty washing.
     */
    zsh_subshell++;
    if ((flags & ESUB_REVERTPGRP) && getpid() == mypgrp)
	release_pgrp();
    if (SHTTY != -1) {
	shout = NULL;
	zclose(SHTTY);
	SHTTY = -1;
    }
    if (isset(MONITOR)) {
	signal_default(SIGTTOU);
	signal_default(SIGTTIN);
	signal_default(SIGTSTP);
    }
    if (interact) {
	signal_default(SIGTERM);
	if (!(sigtrapped[SIGINT] & ZSIG_IGNORED))
	    signal_default(SIGINT);
    }
    if (!(sigtrapped[SIGQUIT] & ZSIG_IGNORED))
	signal_default(SIGQUIT);
    opts[MONITOR] = opts[USEZLE] = 0;
    zleactive = 0;
    if (flags & ESUB_PGRP)
	clearjobtab(monitor);
    get_usage();
    forklevel = locallevel;
}

/* execute a string */

/**/
mod_export void
execstring(char *s, int dont_change_job, int exiting)
{
    Eprog prog;

    pushheap();
    if ((prog = parse_string(s, 0)))
	execode(prog, dont_change_job, exiting);
    popheap();
}

/**/
mod_export void
execode(Eprog p, int dont_change_job, int exiting)
{
    struct estate s;

    s.prog = p;
    s.pc = p->prog;
    s.strs = p->strs;
    useeprog(p);		/* Mark as in use */

    execlist(&s, dont_change_job, exiting);

    freeeprog(p);		/* Free if now unused */
}

/* Execute a simplified command. This is used to execute things that
 * will run completely in the shell, so that we can by-pass all that
 * nasty job-handling and redirection stuff in execpline and execcmd. */

/**/
static int
execsimple(Estate state)
{
    wordcode code = *state->pc++;
    int lv;

    if (errflag)
	return (lastval = 1);

    /* In evaluated traps, don't modify the line number. */
    if (!IN_EVAL_TRAP() && !ineval && code)
	lineno = code - 1;

    code = wc_code(*state->pc++);

    if (code == WC_ASSIGN) {
	cmdoutval = 0;
	addvars(state, state->pc - 1, 0);
	if (isset(XTRACE)) {
	    fputc('\n', xtrerr);
	    fflush(xtrerr);
	}
	lv = (errflag ? errflag : cmdoutval);
    } else
	lv = (execfuncs[code - WC_CURSH])(state, 0);

    return lastval = lv;
}

/* Main routine for executing a list.                                *
 * exiting means that the (sub)shell we are in is a definite goner   *
 * after the current list is finished, so we may be able to exec the *
 * last command directly instead of forking.  If dont_change_job is  *
 * nonzero, then restore the current job number after executing the  *
 * list.                                                             */

/**/
void
execlist(Estate state, int dont_change_job, int exiting)
{
    static int donetrap;
    Wordcode next;
    wordcode code;
    int ret, cj, csp, ltype;
    int old_pline_level, old_list_pipe;
    zlong oldlineno;
    /*
     * ERREXIT only forces the shell to exit if the last command in a &&
     * or || fails.  This is the case even if an earlier command is a
     * shell function or other current shell structure, so we have to set
     * noerrexit here if the sublist is not of type END.
     */
    int oldnoerrexit = noerrexit;

    cj = thisjob;
    old_pline_level = pline_level;
    old_list_pipe = list_pipe;
    oldlineno = lineno;

    if (sourcelevel && unset(SHINSTDIN))
	pline_level = list_pipe = 0;

    /* Loop over all sets of comands separated by newline, *
     * semi-colon or ampersand (`sublists').               */
    code = *state->pc++;
    while (wc_code(code) == WC_LIST && !breaks && !retflag && !errflag) {
	int donedebug;

	ltype = WC_LIST_TYPE(code);
	csp = cmdsp;

	if (!IN_EVAL_TRAP() && !ineval) {
	    /*
	     * Ensure we have a valid line number for debugging,
	     * unless we are in an evaluated trap in which case
	     * we retain the line number from the context.
	     * This was added for DEBUGBEFORECMD but I've made
	     * it unconditional to keep dependencies to a minimum.
	     *
	     * The line number is updated for individual pipelines.
	     * This isn't necessary for debug traps since they only
	     * run once per sublist.
	     */
	    wordcode code2 = *state->pc, lnp1 = 0;
	    if (ltype & Z_SIMPLE) {
		lnp1 = code2;
	    } else if (wc_code(code2) == WC_SUBLIST) {
		if (WC_SUBLIST_FLAGS(code2) == WC_SUBLIST_SIMPLE)
		    lnp1 = state->pc[1];
		else
		    lnp1 = WC_PIPE_LINENO(state->pc[1]);
	    }
	    if (lnp1)
		lineno = lnp1 - 1;
	}

	if (sigtrapped[SIGDEBUG] && isset(DEBUGBEFORECMD) && !intrap) {
	    Wordcode pc2 = state->pc;
	    int oerrexit_opt = opts[ERREXIT];
	    Param pm;
	    opts[ERREXIT] = 0;
	    noerrexit = 1;
	    if (ltype & Z_SIMPLE) /* skip the line number */
		pc2++;
	    pm = setsparam("ZSH_DEBUG_CMD", getpermtext(state->prog, pc2, 0));

	    exiting = donetrap;
	    ret = lastval;
	    dotrap(SIGDEBUG);
	    if (!retflag)
		lastval = ret;
	    donetrap = exiting;
	    noerrexit = oldnoerrexit;
	    /*
	     * Only execute the trap once per sublist, even
	     * if the DEBUGBEFORECMD option changes.
	     */
	    donedebug = isset(ERREXIT) ? 2 : 1;
	    opts[ERREXIT] = oerrexit_opt;
	    if (pm)
		unsetparam_pm(pm, 0, 1);
	} else
	    donedebug = intrap ? 1 : 0;

	if (ltype & Z_SIMPLE) {
	    next = state->pc + WC_LIST_SKIP(code);
	    if (donedebug != 2)
		execsimple(state);
	    state->pc = next;
	    goto sublist_done;
	}
	/* Reset donetrap:  this ensures that a trap is only *
	 * called once for each sublist that fails.          */
	donetrap = 0;

	/* Loop through code followed by &&, ||, or end of sublist. */
	code = *state->pc++;
	if (donedebug == 2) {
	    /* Skip sublist. */
	    while (wc_code(code) == WC_SUBLIST) {
		state->pc = state->pc + WC_SUBLIST_SKIP(code);
		if (WC_SUBLIST_TYPE(code) == WC_SUBLIST_END)
		    break;
		code = *state->pc++;
	    }
	    donetrap = 1;
	    /* yucky but consistent... */
	    goto sublist_done;
	}
	while (wc_code(code) == WC_SUBLIST) {
	    next = state->pc + WC_SUBLIST_SKIP(code);
	    if (!oldnoerrexit)
		noerrexit = (WC_SUBLIST_TYPE(code) != WC_SUBLIST_END);
	    switch (WC_SUBLIST_TYPE(code)) {
	    case WC_SUBLIST_END:
		/* End of sublist; just execute, ignoring status. */
		if (WC_SUBLIST_FLAGS(code) & WC_SUBLIST_SIMPLE)
		    execsimple(state);
		else
		    execpline(state, code, ltype, (ltype & Z_END) && exiting);
		state->pc = next;
		goto sublist_done;
		break;
	    case WC_SUBLIST_AND:
		/* If the return code is non-zero, we skip pipelines until *
		 * we find a sublist followed by ORNEXT.                   */
		if ((ret = ((WC_SUBLIST_FLAGS(code) & WC_SUBLIST_SIMPLE) ?
			    execsimple(state) :
			    execpline(state, code, Z_SYNC, 0)))) {
		    state->pc = next;
		    code = *state->pc++;
		    next = state->pc + WC_SUBLIST_SKIP(code);
		    while (wc_code(code) == WC_SUBLIST &&
			   WC_SUBLIST_TYPE(code) == WC_SUBLIST_AND) {
			state->pc = next;
			code = *state->pc++;
			next = state->pc + WC_SUBLIST_SKIP(code);
		    }
		    if (wc_code(code) != WC_SUBLIST) {
			/* We've skipped to the end of the list, not executing *
			 * the final pipeline, so don't perform error handling *
			 * for this sublist.                                   */
			donetrap = 1;
			goto sublist_done;
		    } else if (WC_SUBLIST_TYPE(code) == WC_SUBLIST_END) {
			donetrap = 1;
			/*
			 * Treat this in the same way as if we reached
			 * the end of the sublist normally.
			 */
			state->pc = next;
			goto sublist_done;
		    }
		}
		cmdpush(CS_CMDAND);
		break;
	    case WC_SUBLIST_OR:
		/* If the return code is zero, we skip pipelines until *
		 * we find a sublist followed by ANDNEXT.              */
		if (!(ret = ((WC_SUBLIST_FLAGS(code) & WC_SUBLIST_SIMPLE) ?
			     execsimple(state) :
			     execpline(state, code, Z_SYNC, 0)))) {
		    state->pc = next;
		    code = *state->pc++;
		    next = state->pc + WC_SUBLIST_SKIP(code);
		    while (wc_code(code) == WC_SUBLIST &&
			   WC_SUBLIST_TYPE(code) == WC_SUBLIST_OR) {
			state->pc = next;
			code = *state->pc++;
			next = state->pc + WC_SUBLIST_SKIP(code);
		    }
		    if (wc_code(code) != WC_SUBLIST) {
			/* We've skipped to the end of the list, not executing *
			 * the final pipeline, so don't perform error handling *
			 * for this sublist.                                   */
			donetrap = 1;
			goto sublist_done;
		    } else if (WC_SUBLIST_TYPE(code) == WC_SUBLIST_END) {
			donetrap = 1;
			/*
			 * Treat this in the same way as if we reached
			 * the end of the sublist normally.
			 */
			state->pc = next;
			goto sublist_done;
		    }
		}
		cmdpush(CS_CMDOR);
		break;
	    }
	    state->pc = next;
	    code = *state->pc++;
	}
	state->pc--;
sublist_done:

	noerrexit = oldnoerrexit;

	if (sigtrapped[SIGDEBUG] && !isset(DEBUGBEFORECMD) && !donedebug) {
	    /*
	     * Save and restore ERREXIT for consistency with
	     * DEBUGBEFORECMD, even though it's not used.
	     */
	    int oerrexit_opt = opts[ERREXIT];
	    opts[ERREXIT] = 0;
	    noerrexit = 1;
	    exiting = donetrap;
	    ret = lastval;
	    dotrap(SIGDEBUG);
	    if (!retflag)
		lastval = ret;
	    donetrap = exiting;
	    noerrexit = oldnoerrexit;
	    opts[ERREXIT] = oerrexit_opt;
	}

	cmdsp = csp;

	/* Check whether we are suppressing traps/errexit *
	 * (typically in init scripts) and if we haven't  *
	 * already performed them for this sublist.       */
	if (!noerrexit && !donetrap) {
	    if (sigtrapped[SIGZERR] && lastval) {
		dotrap(SIGZERR);
		donetrap = 1;
	    }
	    if (lastval) {
		int errreturn = isset(ERRRETURN) && 
		    (isset(INTERACTIVE) || locallevel || sourcelevel);
		int errexit = isset(ERREXIT) || 
		    (isset(ERRRETURN) && !errreturn);
		if (errexit) {
		    if (sigtrapped[SIGEXIT])
			dotrap(SIGEXIT);
		    if (mypid != getpid())
			_exit(lastval);
		    else
			exit(lastval);
		}
		if (errreturn) {
		    retflag = 1;
		    breaks = loops;
		}
	    }
	}
	if (ltype & Z_END)
	    break;
	code = *state->pc++;
    }
    pline_level = old_pline_level;
    list_pipe = old_list_pipe;
    lineno = oldlineno;
    if (dont_change_job)
	thisjob = cj;
}

/* Execute a pipeline.                                                *
 * last1 is a flag that this command is the last command in a shell   *
 * that is about to exit, so we can exec instead of forking.  It gets *
 * passed all the way down to execcmd() which actually makes the      *
 * decision.  A 0 is always passed if the command is not the last in  *
 * the pipeline.  This function assumes that the sublist is not NULL. *
 * If last1 is zero but the command is at the end of a pipeline, we   *
 * pass 2 down to execcmd().                                          *
 */

/**/
static int
execpline(Estate state, wordcode slcode, int how, int last1)
{
    int ipipe[2], opipe[2];
    int pj, newjob;
    int old_simple_pline = simple_pline;
    int slflags = WC_SUBLIST_FLAGS(slcode);
    wordcode code = *state->pc++;
    static int lastwj, lpforked;

    if (wc_code(code) != WC_PIPE)
	return lastval = (slflags & WC_SUBLIST_NOT) != 0;
    else if (slflags & WC_SUBLIST_NOT)
	last1 = 0;

    pj = thisjob;
    ipipe[0] = ipipe[1] = opipe[0] = opipe[1] = 0;
    child_block();

    /*
     * Get free entry in job table and initialize it.  This is currently
     * the only call to initjob() (apart from a minor exception in
     * clearjobtab()), so this is also the only place where we can
     * expand the job table under us.
     */
    if ((thisjob = newjob = initjob()) == -1) {
	child_unblock();
	return 1;
    }
    if (how & Z_TIMED)
	jobtab[thisjob].stat |= STAT_TIMED;

    if (slflags & WC_SUBLIST_COPROC) {
	how = Z_ASYNC;
	if (coprocin >= 0) {
	    zclose(coprocin);
	    zclose(coprocout);
	}
	mpipe(ipipe);
	mpipe(opipe);
	coprocin = ipipe[0];
	coprocout = opipe[1];
	fdtable[coprocin] = fdtable[coprocout] = FDT_UNUSED;
    }
    /* This used to set list_pipe_pid=0 unconditionally, but in things
     * like `ls|if true; then sleep 20; cat; fi' where the sleep was
     * stopped, the top-level execpline() didn't get the pid for the
     * sub-shell because it was overwritten. */
    if (!pline_level++) {
        list_pipe_pid = 0;
	nowait = 0;
	simple_pline = (WC_PIPE_TYPE(code) == WC_PIPE_END);
	list_pipe_job = newjob;
    }
    lastwj = lpforked = 0;
    execpline2(state, code, how, opipe[0], ipipe[1], last1);
    pline_level--;
    if (how & Z_ASYNC) {
	lastwj = newjob;

        if (thisjob == list_pipe_job)
            list_pipe_job = 0;
	jobtab[thisjob].stat |= STAT_NOSTTY;
	if (slflags & WC_SUBLIST_COPROC) {
	    zclose(ipipe[1]);
	    zclose(opipe[0]);
	}
	if (how & Z_DISOWN) {
	    deletejob(jobtab + thisjob);
	    thisjob = -1;
	}
	else
	    spawnjob();
	child_unblock();
	return 0;
    } else {
	if (newjob != lastwj) {
	    Job jn = jobtab + newjob;
	    int updated;

	    if (newjob == list_pipe_job && list_pipe_child)
		_exit(0);

	    lastwj = thisjob = newjob;

	    if (list_pipe || (pline_level && !(how & Z_TIMED)))
		jn->stat |= STAT_NOPRINT;

	    if (nowait) {
		if(!pline_level) {
		    struct process *pn, *qn;

		    curjob = newjob;
		    DPUTS(!list_pipe_pid, "invalid list_pipe_pid");
		    addproc(list_pipe_pid, list_pipe_text, 0,
			    &list_pipe_start);

		    /* If the super-job contains only the sub-shell, the
		       sub-shell is the group leader. */
		    if (!jn->procs->next || lpforked == 2) {
			jn->gleader = list_pipe_pid;
			jn->stat |= STAT_SUBLEADER;
		    }
		    for (pn = jobtab[jn->other].procs; pn; pn = pn->next)
			if (WIFSTOPPED(pn->status))
			    break;

		    if (pn) {
			for (qn = jn->procs; qn->next; qn = qn->next);
			qn->status = pn->status;
		    }

		    jn->stat &= ~(STAT_DONE | STAT_NOPRINT);
		    jn->stat |= STAT_STOPPED | STAT_CHANGED | STAT_LOCKED;
		    printjob(jn, !!isset(LONGLISTJOBS), 1);
		}
		else if (newjob != list_pipe_job)
		    deletejob(jn);
		else
		    lastwj = -1;
	    }

	    errbrk_saved = 0;
	    for (; !nowait;) {
		if (list_pipe_child) {
		    jn->stat |= STAT_NOPRINT;
		    makerunning(jn);
		}
		if (!(jn->stat & STAT_LOCKED)) {
		    updated = hasprocs(thisjob);
		    waitjobs();
		    child_block();
		} else
		    updated = 0;
		if (!updated &&
		    list_pipe_job && hasprocs(list_pipe_job) &&
		    !(jobtab[list_pipe_job].stat & STAT_STOPPED)) {
		    child_unblock();
		    child_block();
		}
		if (list_pipe_child &&
		    jn->stat & STAT_DONE &&
		    lastval2 & 0200)
		    killpg(mypgrp, lastval2 & ~0200);
		if (!list_pipe_child && !lpforked && !subsh && jobbing &&
		    (list_pipe || last1 || pline_level) &&
		    ((jn->stat & STAT_STOPPED) ||
		     (list_pipe_job && pline_level &&
		      (jobtab[list_pipe_job].stat & STAT_STOPPED)))) {
		    pid_t pid;
		    int synch[2];
		    struct timeval bgtime;

		    pipe(synch);

		    if ((pid = zfork(&bgtime)) == -1) {
			zleentry(ZLE_CMD_TRASH);
			close(synch[0]);
			close(synch[1]);
			fprintf(stderr, "zsh: job can't be suspended\n");
			fflush(stderr);
			makerunning(jn);
			killjb(jn, SIGCONT);
			thisjob = newjob;
		    }
		    else if (pid) {
			char dummy;

			lpforked = 
			    (killpg(jobtab[list_pipe_job].gleader, 0) == -1 ? 2 : 1);
			list_pipe_pid = pid;
			list_pipe_start = bgtime;
			nowait = errflag = 1;
			breaks = loops;
			close(synch[1]);
			read(synch[0], &dummy, 1);
			close(synch[0]);
			/* If this job has finished, we leave it as a
			 * normal (non-super-) job. */
			if (!(jn->stat & STAT_DONE)) {
			    jobtab[list_pipe_job].other = newjob;
			    jobtab[list_pipe_job].stat |= STAT_SUPERJOB;
			    jn->stat |= STAT_SUBJOB | STAT_NOPRINT;
			    jn->other = pid;
			}
			if ((list_pipe || last1) && hasprocs(list_pipe_job))
			    killpg(jobtab[list_pipe_job].gleader, SIGSTOP);
			break;
		    }
		    else {
			close(synch[0]);
			entersubsh(ESUB_ASYNC);
			if (jobtab[list_pipe_job].procs) {
			    if (setpgrp(0L, mypgrp = jobtab[list_pipe_job].gleader)
				== -1) {
				setpgrp(0L, mypgrp = getpid());
			    }
			} else
			    setpgrp(0L, mypgrp = getpid());
			close(synch[1]);
			kill(getpid(), SIGSTOP);
			list_pipe = 0;
			list_pipe_child = 1;
			opts[INTERACTIVE] = 0;
			if (errbrk_saved) {
			    errflag = prev_errflag;
			    breaks = prev_breaks;
			}
			break;
		    }
		}
		else if (subsh && jn->stat & STAT_STOPPED)
		    thisjob = newjob;
		else
		    break;
	    }
	    child_unblock();

	    if (list_pipe && (lastval & 0200) && pj >= 0 &&
		(!(jn->stat & STAT_INUSE) || (jn->stat & STAT_DONE))) {
		deletejob(jn);
		jn = jobtab + pj;
		if (jn->gleader)
		    killjb(jn, lastval & ~0200);
	    }
	    if (list_pipe_child ||
		((jn->stat & STAT_DONE) &&
		 (list_pipe || (pline_level && !(jn->stat & STAT_SUBJOB)))))
		deletejob(jn);
	    thisjob = pj;

	}
	if (slflags & WC_SUBLIST_NOT)
	    lastval = !lastval;
    }
    if (!pline_level)
	simple_pline = old_simple_pline;
    return lastval;
}

static int subsh_close = -1;

/* execute pipeline.  This function assumes the `pline' is not NULL. */

/**/
static void
execpline2(Estate state, wordcode pcode,
	   int how, int input, int output, int last1)
{
    pid_t pid;
    int pipes[2];

    if (breaks || retflag)
	return;

    /* In evaluated traps, don't modify the line number. */
    if (!IN_EVAL_TRAP() && !ineval && WC_PIPE_LINENO(pcode))
	lineno = WC_PIPE_LINENO(pcode) - 1;

    if (pline_level == 1) {
	if ((how & Z_ASYNC) || (!sfcontext && !sourcelevel))
	    strcpy(list_pipe_text,
		   getjobtext(state->prog,
			      state->pc + (WC_PIPE_TYPE(pcode) == WC_PIPE_END ?
					   0 : 1)));
	else
	    list_pipe_text[0] = '\0';
    }
    if (WC_PIPE_TYPE(pcode) == WC_PIPE_END)
	execcmd(state, input, output, how, last1 ? 1 : 2);
    else {
	int old_list_pipe = list_pipe;
	Wordcode next = state->pc + (*state->pc), pc;
	wordcode code;

	state->pc++;
	for (pc = state->pc; wc_code(code = *pc) == WC_REDIR;
	     pc += WC_REDIR_WORDS(code));

	mpipe(pipes);

	/* if we are doing "foo | bar" where foo is a current *
	 * shell command, do foo in a subshell and do the     *
	 * rest of the pipeline in the current shell.         */
	if (wc_code(code) >= WC_CURSH && (how & Z_SYNC)) {
	    int synch[2];
	    struct timeval bgtime;

	    pipe(synch);
	    if ((pid = zfork(&bgtime)) == -1) {
		close(synch[0]);
		close(synch[1]);
	    } else if (pid) {
		char dummy, *text;

		text = getjobtext(state->prog, state->pc);
		addproc(pid, text, 0, &bgtime);
		close(synch[1]);
		read(synch[0], &dummy, 1);
		close(synch[0]);
	    } else {
		zclose(pipes[0]);
		close(synch[0]);
		entersubsh(((how & Z_ASYNC) ? ESUB_ASYNC : 0)
			   | ESUB_PGRP | ESUB_KEEPTRAP);
		close(synch[1]);
		execcmd(state, input, pipes[1], how, 0);
		_exit(lastval);
	    }
	} else {
	    /* otherwise just do the pipeline normally. */
	    subsh_close = pipes[0];
	    execcmd(state, input, pipes[1], how, 0);
	}
	zclose(pipes[1]);
	state->pc = next;

	/* if another execpline() is invoked because the command is *
	 * a list it must know that we're already in a pipeline     */
	cmdpush(CS_PIPE);
	list_pipe = 1;
	execpline2(state, *state->pc++, how, pipes[0], output, last1);
	list_pipe = old_list_pipe;
	cmdpop();
	zclose(pipes[0]);
	subsh_close = -1;
    }
}

/* make the argv array */

/**/
static char **
makecline(LinkList list)
{
    LinkNode node;
    char **argv, **ptr;

    /* A bigger argv is necessary for executing scripts */
    ptr = argv = 2 + (char **) hcalloc((countlinknodes(list) + 4) *
				       sizeof(char *));

    if (isset(XTRACE)) {
	if (!doneps4)
	    printprompt4();

	for (node = firstnode(list); node; incnode(node)) {
	    *ptr++ = (char *)getdata(node);
	    quotedzputs(getdata(node), xtrerr);
	    if (nextnode(node))
		fputc(' ', xtrerr);
	}
	fputc('\n', xtrerr);
	fflush(xtrerr);
    } else {
	for (node = firstnode(list); node; incnode(node))
	    *ptr++ = (char *)getdata(node);
    }
    *ptr = NULL;
    return (argv);
}

/**/
mod_export void
untokenize(char *s)
{
    if (*s) {
	int c;

	while ((c = *s++))
	    if (itok(c)) {
		char *p = s - 1;

		if (c != Nularg)
		    *p++ = ztokens[c - Pound];

		while ((c = *s++)) {
		    if (itok(c)) {
			if (c != Nularg)
			    *p++ = ztokens[c - Pound];
		    } else
			*p++ = c;
		}
		*p = '\0';
		break;
	    }
    }
}


/*
 * Given a tokenized string, output it to standard output in
 * such a way that it's clear which tokens are active.
 * Hence Star becomes an unquoted "*", while a "*" becomes "\*".
 *
 * The code here is a kind of amalgamation of the tests in
 * zshtokenize() and untokenize() with some outputting.
 */

/**/
void
quote_tokenized_output(char *str, FILE *file)
{
    char *s = str;

    for (; *s; s++) {
	switch (*s) {
	case Meta:
	    putc(*++s ^ 32, file);
	    continue;

	case Nularg:
	    /* Do nothing.  I think. */
	    continue;

	case '\\':
	case '<':
	case '>':
	case '(':
	case '|':
	case ')':
	case '^':
	case '#':
	case '~':
	case '[':
	case ']':
	case '*':
	case '?':
	case '$':
	    putc('\\', file);
	    break;

	case '=':
	    if (s == str)
		putc('\\', file);
	    break;

	default:
	    if (itok(*s)) {
		putc(ztokens[*s - Pound], file);
		continue;
	    }
	    break;
	}

	putc(*s, file);
    }
}

/* Check that we can use a parameter for allocating a file descriptor. */

static int
checkclobberparam(struct redir *f)
{
    struct value vbuf;
    Value v;
    char *s = f->varid;
    int fd;

    if (!s)
	return 1;

    if (!(v = getvalue(&vbuf, &s, 0)))
	return 1;

    if (v->pm->node.flags & PM_READONLY) {
	zwarn("can't allocate file descriptor to readonly parameter %s",
	      f->varid);
	/* don't flag a system error for this */
	errno = 0;
	return 0;
    }

    if (!isset(CLOBBER) && (fd = (int)getintvalue(v)) &&
	fd <= max_zsh_fd && fdtable[fd] == FDT_EXTERNAL) {
	zwarn("can't clobber parameter %s containing file descriptor %d",
	     f->varid, fd);
	/* don't flag a system error for this */
	errno = 0;
	return 0;
    }
    return 1;
}

/* Open a file for writing redirection */

/**/
static int
clobber_open(struct redir *f)
{
    struct stat buf;
    int fd, oerrno;

    /* If clobbering, just open. */
    if (isset(CLOBBER) || IS_CLOBBER_REDIR(f->type))
	return open(unmeta(f->name),
		O_WRONLY | O_CREAT | O_TRUNC | O_NOCTTY, 0666);

    /* If not clobbering, attempt to create file exclusively. */
    if ((fd = open(unmeta(f->name),
		   O_WRONLY | O_CREAT | O_EXCL | O_NOCTTY, 0666)) >= 0)
	return fd;

    /* If that fails, we are still allowed to open non-regular files. *
     * Try opening, and if it's a regular file then close it again    *
     * because we weren't supposed to open it.                        */
    oerrno = errno;
    if ((fd = open(unmeta(f->name), O_WRONLY | O_NOCTTY)) != -1) {
	if(!fstat(fd, &buf) && !S_ISREG(buf.st_mode))
	    return fd;
	close(fd);
    }
    errno = oerrno;
    return -1;
}

/* size of buffer for tee and cat processes */
#define TCBUFSIZE 4092

/* close an multio (success) */

/**/
static void
closemn(struct multio **mfds, int fd)
{
    if (fd >= 0 && mfds[fd] && mfds[fd]->ct >= 2) {
	struct multio *mn = mfds[fd];
	char buf[TCBUFSIZE];
	int len, i;
	pid_t pid;
	struct timeval bgtime;

	/*
	 * We need to block SIGCHLD in case the process
	 * we are spawning terminates before the job table
	 * is set up to handle it.
	 */
	child_block();
	if ((pid = zfork(&bgtime))) {
	    for (i = 0; i < mn->ct; i++)
		zclose(mn->fds[i]);
	    zclose(mn->pipe);
	    if (pid == -1) {
		mfds[fd] = NULL;
		child_unblock();
		return;
	    }
	    mn->ct = 1;
	    mn->fds[0] = fd;
	    addproc(pid, NULL, 1, &bgtime);
	    child_unblock();
	    return;
	}
	/* pid == 0 */
	child_unblock();
	closeallelse(mn);
	if (mn->rflag) {
	    /* tee process */
	    while ((len = read(mn->pipe, buf, TCBUFSIZE)) != 0) {
		if (len < 0) {
		    if (errno == EINTR)
			continue;
		    else
			break;
		}
		for (i = 0; i < mn->ct; i++)
		    write(mn->fds[i], buf, len);
	    }
	} else {
	    /* cat process */
	    for (i = 0; i < mn->ct; i++)
		while ((len = read(mn->fds[i], buf, TCBUFSIZE)) != 0) {
		    if (len < 0) {
			if (errno == EINTR)
			    continue;
			else
			    break;
		    }
		    write(mn->pipe, buf, len);
		}
	}
	_exit(0);
    } else if (fd >= 0)
	mfds[fd] = NULL;
}

/* close all the mnodes (failure) */

/**/
static void
closemnodes(struct multio **mfds)
{
    int i, j;

    for (i = 0; i < 10; i++)
	if (mfds[i]) {
	    for (j = 0; j < mfds[i]->ct; j++)
		zclose(mfds[i]->fds[j]);
	    mfds[i] = NULL;
	}
}

/**/
static void
closeallelse(struct multio *mn)
{
    int i, j;
    long openmax;

    openmax = zopenmax();

    for (i = 0; i < openmax; i++)
	if (mn->pipe != i) {
	    for (j = 0; j < mn->ct; j++)
		if (mn->fds[j] == i)
		    break;
	    if (j == mn->ct)
		zclose(i);
	}
}

/*
 * A multio is a list of fds associated with a certain fd.
 * Thus if you do "foo >bar >ble", the multio for fd 1 will have
 * two fds, the result of open("bar",...), and the result of
 * open("ble",....).
 */

/*
 * Add a fd to an multio.  fd1 must be < 10, and may be in any state.
 * fd2 must be open, and is `consumed' by this function.  Note that
 * fd1 == fd2 is possible, and indicates that fd1 was really closed.
 * We effectively do `fd2 = movefd(fd2)' at the beginning of this
 * function, but in most cases we can avoid an extra dup by delaying
 * the movefd: we only >need< to move it if we're actually doing a
 * multiple redirection.
 *
 * If varid is not NULL, we open an fd above 10 and set the parameter
 * named varid to that value.  fd1 is not used.
 */

/**/
static void
addfd(int forked, int *save, struct multio **mfds, int fd1, int fd2, int rflag,
      char *varid)
{
    int pipes[2];

    if (varid) {
	/* fd will be over 10, don't touch mfds */
	fd1 = movefd(fd2);
	fdtable[fd1] = FDT_EXTERNAL;
	setiparam(varid, (zlong)fd1);
	/*
	 * If setting the parameter failed, close the fd else
	 * it will leak.
	 */
	if (errflag)
	    zclose(fd1);
    } else if (!mfds[fd1] || unset(MULTIOS)) {
	if(!mfds[fd1]) {		/* starting a new multio */
	    mfds[fd1] = (struct multio *) zhalloc(sizeof(struct multio));
	    if (!forked && save[fd1] == -2) {
		if (fd1 == fd2)
		    save[fd1] = -1;
		else {
		    int fdN = movefd(fd1);
		    /*
		     * fd1 may already be closed here, so
		     * ignore bad file descriptor error
		     */
		    if (fdN < 0 && errno != EBADF) {
			zerr("cannot duplicate fd %d: %e", fd1, errno);
			closemnodes(mfds);
			return;
		    }
		    save[fd1] = fdN;
		}
	    }
	}
	if (!varid)
	    redup(fd2, fd1);
	mfds[fd1]->ct = 1;
	mfds[fd1]->fds[0] = fd1;
	mfds[fd1]->rflag = rflag;
    } else {
	if (mfds[fd1]->rflag != rflag) {
	    zerr("file mode mismatch on fd %d", fd1);
	    closemnodes(mfds);
	    return;
	}
	if (mfds[fd1]->ct == 1) {	/* split the stream */
	    int fdN = movefd(fd1);
	    if (fdN < 0) {
		zerr("multio failed for fd %d: %e", fd1, errno);
		closemnodes(mfds);
		return;
	    }
	    mfds[fd1]->fds[0] = fdN;
	    fdN = movefd(fd2);
	    if (fdN < 0) {
		zerr("multio failed for fd %d: %e", fd2, errno);
		closemnodes(mfds);
		return;
	    }
	    mfds[fd1]->fds[1] = fdN;
	    mpipe(pipes);
	    mfds[fd1]->pipe = pipes[1 - rflag];
	    redup(pipes[rflag], fd1);
	    mfds[fd1]->ct = 2;
	} else {		/* add another fd to an already split stream */
	    int fdN;
	    if(!(mfds[fd1]->ct % MULTIOUNIT)) {
		int new = sizeof(struct multio) + sizeof(int) * mfds[fd1]->ct;
		int old = new - sizeof(int) * MULTIOUNIT;
		mfds[fd1] = hrealloc((char *)mfds[fd1], old, new);
	    }
	    if ((fdN = movefd(fd2)) < 0) {
		zerr("multio failed for fd %d: %e", fd2, errno);
		closemnodes(mfds);
		return;
	    }
	    mfds[fd1]->fds[mfds[fd1]->ct++] = fdN;
	}
    }
    if (subsh_close >= 0 && fdtable[subsh_close] == FDT_UNUSED)
	subsh_close = -1;
}

/**/
static void
addvars(Estate state, Wordcode pc, int addflags)
{
    LinkList vl;
    int xtr, isstr, htok = 0;
    char **arr, **ptr, *name;
    int flags;

    Wordcode opc = state->pc;
    wordcode ac;
    local_list1(svl);

    /*
     * Warn when creating a global without using typeset -g in a
     * function.  Don't do this if there is a list of variables marked
     * to be restored after the command, since then the assignment
     * is implicitly scoped.
     */
    flags = (!(addflags & ADDVAR_RESTORE) &&
	     locallevel > 0 && isset(WARNCREATEGLOBAL)) ?
	ASSPM_WARN_CREATE : 0;
    xtr = isset(XTRACE);
    if (xtr) {
	printprompt4();
	doneps4 = 1;
    }
    state->pc = pc;
    while (wc_code(ac = *state->pc++) == WC_ASSIGN) {
	int myflags = flags;
	name = ecgetstr(state, EC_DUPTOK, &htok);
	if (htok)
	    untokenize(name);
	if (WC_ASSIGN_TYPE2(ac) == WC_ASSIGN_INC)
	    myflags |= ASSPM_AUGMENT;
	if (xtr)
	    fprintf(xtrerr,
		WC_ASSIGN_TYPE2(ac) == WC_ASSIGN_INC ? "%s+=" : "%s=", name);
	if ((isstr = (WC_ASSIGN_TYPE(ac) == WC_ASSIGN_SCALAR))) {
	    init_list1(svl, ecgetstr(state, EC_DUPTOK, &htok));
	    vl = &svl;
	} else
	    vl = ecgetlist(state, WC_ASSIGN_NUM(ac), EC_DUPTOK, &htok);

	if (vl && htok) {
	    prefork(vl, (isstr ? (PF_SINGLE|PF_ASSIGN) :
			 PF_ASSIGN));
	    if (errflag) {
		state->pc = opc;
		return;
	    }
	    if (isset(GLOBASSIGN) || !isstr)
		globlist(vl, 0);
	    if (errflag) {
		state->pc = opc;
		return;
	    }
	}
	if (isstr && (empty(vl) || !nextnode(firstnode(vl)))) {
	    Param pm;
	    char *val;
	    int allexp;

	    if (empty(vl))
		val = ztrdup("");
	    else {
		untokenize(peekfirst(vl));
		val = ztrdup(ugetnode(vl));
	    }
	    if (xtr) {
		quotedzputs(val, xtrerr);
		fputc(' ', xtrerr);
	    }
	    if ((addflags & ADDVAR_EXPORT) && !strchr(name, '[')) {
		if ((addflags & ADDVAR_RESTRICT) && isset(RESTRICTED) &&
		    (pm = (Param) paramtab->removenode(paramtab, name)) &&
		    (pm->node.flags & PM_RESTRICTED)) {
		    zerr("%s: restricted", pm->node.nam);
		    zsfree(val);
		    state->pc = opc;
		    return;
		}
		if (strcmp(name, "STTY") == 0) {
		    zsfree(STTYval);
		    STTYval = ztrdup(val);
		}
		allexp = opts[ALLEXPORT];
		opts[ALLEXPORT] = 1;
	    	pm = assignsparam(name, val, myflags);
		opts[ALLEXPORT] = allexp;
	    } else
	    	pm = assignsparam(name, val, myflags);
	    if (errflag) {
		state->pc = opc;
		return;
	    }
	    continue;
	}
	if (vl) {
	    ptr = arr = (char **) zalloc(sizeof(char **) *
					 (countlinknodes(vl) + 1));

	    while (nonempty(vl))
		*ptr++ = ztrdup((char *) ugetnode(vl));
	} else
	    ptr = arr = (char **) zalloc(sizeof(char **));

	*ptr = NULL;
	if (xtr) {
	    fprintf(xtrerr, "( ");
	    for (ptr = arr; *ptr; ptr++) {
		quotedzputs(*ptr, xtrerr);
		fputc(' ', xtrerr);
	    }
	    fprintf(xtrerr, ") ");
	}
	assignaparam(name, arr, myflags);
	if (errflag) {
	    state->pc = opc;
	    return;
	}
    }
    state->pc = opc;
}

/**/
void
setunderscore(char *str)
{
    if (str && *str) {
	int l = strlen(str) + 1, nl = (l + 31) & ~31;

	if (nl > underscorelen || (underscorelen - nl) > 64) {
	    zfree(underscore, underscorelen);
	    underscore = (char *) zalloc(underscorelen = nl);
	}
	strcpy(underscore, str);
	underscoreused = l;
    } else {
	if (underscorelen > 128) {
	    zfree(underscore, underscorelen);
	    underscore = (char *) zalloc(underscorelen = 32);
	}
	*underscore = '\0';
	underscoreused = 1;
    }
}

/* These describe the type of expansions that need to be done on the words
 * used in the thing we are about to execute. They are set in execcmd() and
 * used in execsubst() which might be called from one of the functions
 * called from execcmd() (like execfor() and so on). */

static int esprefork, esglob = 1;

/**/
void
execsubst(LinkList strs)
{
    if (strs) {
	prefork(strs, esprefork);
	if (esglob) {
	    LinkList ostrs = strs;
	    globlist(strs, 0);
	    strs = ostrs;
	}
    }
}

/*
 * Check if a builtin requires an autoload and if so
 * deal with it.  This may return NULL.
 */

/**/
static HashNode
resolvebuiltin(const char *cmdarg, HashNode hn)
{
    if (!((Builtin) hn)->handlerfunc) {
	/*
	 * Ensure the module is loaded and the
	 * feature corresponding to the builtin
	 * is enabled.
	 */
	(void)ensurefeature(((Builtin) hn)->optstr, "b:",
			    (hn->flags & BINF_AUTOALL) ? NULL :
			    hn->nam);
	hn = builtintab->getnode(builtintab, cmdarg);
	if (!hn) {
	    lastval = 1;
	    zerr("unknown builtin: %s", cmdarg);
	    return NULL;
	}
    }
    return hn;
}

/**/
static void
execcmd(Estate state, int input, int output, int how, int last1)
{
    HashNode hn = NULL;
    LinkList args;
    LinkNode node;
    Redir fn;
    struct multio *mfds[10];
    char *text;
    int save[10];
    int fil, dfil, is_cursh, type, do_exec = 0, i, htok = 0;
    int nullexec = 0, assign = 0, forked = 0;
    int is_shfunc = 0, is_builtin = 0, is_exec = 0, use_defpath = 0;
    /* Various flags to the command. */
    int cflags = 0, checked = 0, oautocont = -1;
    LinkList redir;
    wordcode code;
    Wordcode beg = state->pc, varspc;
    FILE *oxtrerr = xtrerr, *newxtrerr = NULL;

    doneps4 = 0;
    redir = (wc_code(*state->pc) == WC_REDIR ? ecgetredirs(state) : NULL);
    if (wc_code(*state->pc) == WC_ASSIGN) {
	cmdoutval = 0;
	varspc = state->pc;
	while (wc_code((code = *state->pc)) == WC_ASSIGN)
	    state->pc += (WC_ASSIGN_TYPE(code) == WC_ASSIGN_SCALAR ?
			  3 : WC_ASSIGN_NUM(code) + 2);
    } else
	varspc = NULL;

    code = *state->pc++;

    type = wc_code(code);

    /* It would be nice if we could use EC_DUPTOK instead of EC_DUP here.
     * But for that we would need to check/change all builtins so that
     * they don't modify their argument strings. */
    args = (type == WC_SIMPLE ?
	    ecgetlist(state, WC_SIMPLE_ARGC(code), EC_DUP, &htok) : NULL);
    /*
     * If assignment but no command get the status from variable
     * assignment.
     */
    if (!args && varspc)
	lastval = errflag ? errflag : cmdoutval;

    for (i = 0; i < 10; i++) {
	save[i] = -2;
	mfds[i] = NULL;
    }

    /* If the command begins with `%', then assume it is a *
     * reference to a job in the job table.                */
    if (type == WC_SIMPLE && args && nonempty(args) &&
	*(char *)peekfirst(args) == '%') {
        if (how & Z_DISOWN) {
	    oautocont = opts[AUTOCONTINUE];
            opts[AUTOCONTINUE] = 1;
	}
	pushnode(args, dupstring((how & Z_DISOWN)
				 ? "disown" : (how & Z_ASYNC) ? "bg" : "fg"));
	how = Z_SYNC;
    }

    /* If AUTORESUME is set, the command is SIMPLE, and doesn't have *
     * any redirections, then check if it matches as a prefix of a   *
     * job currently in the job table.  If it does, then we treat it *
     * as a command to resume this job.                              */
    if (isset(AUTORESUME) && type == WC_SIMPLE && (how & Z_SYNC) &&
	args && nonempty(args) && (!redir || empty(redir)) && !input &&
	!nextnode(firstnode(args))) {
	if (unset(NOTIFY))
	    scanjobs();
	if (findjobnam(peekfirst(args)) != -1)
	    pushnode(args, dupstring("fg"));
    }

    /* Check if it's a builtin needing automatic MAGIC_EQUALS_SUBST      *
     * handling.  Things like typeset need this.  We can't detect the    *
     * command if it contains some tokens (e.g. x=ex; ${x}port), so this *
     * only works in simple cases.  has_token() is called to make sure   *
     * this really is a simple case.                                     */
    if (type == WC_SIMPLE) {
	while (args && nonempty(args)) {
	    char *cmdarg = (char *) peekfirst(args);
	    checked = !has_token(cmdarg);
	    if (!checked)
		break;
	    if (!(cflags & (BINF_BUILTIN | BINF_COMMAND)) &&
		(hn = shfunctab->getnode(shfunctab, cmdarg))) {
		is_shfunc = 1;
		break;
	    }
	    if (!(hn = builtintab->getnode(builtintab, cmdarg))) {
		checked = !(cflags & BINF_BUILTIN);
		break;
	    }
	    if (!(hn->flags & BINF_PREFIX)) {
		is_builtin = 1;

		/* autoload the builtin if necessary */
		if (!(hn = resolvebuiltin(cmdarg, hn)))
		    return;
		assign = (hn->flags & BINF_MAGICEQUALS);
		break;
	    }
	    cflags &= ~BINF_BUILTIN & ~BINF_COMMAND;
	    cflags |= hn->flags;
	    checked = 0;
	    if ((cflags & BINF_COMMAND) && nextnode(firstnode(args))) {
		/* check for options to command builtin */
		char *next = (char *) getdata(nextnode(firstnode(args)));
		char *cmdopt;
		if (next && *next == '-' && strlen(next) == 2 &&
		        (cmdopt = strchr("pvV", next[1])))
		{
		    if (*cmdopt == 'p') {
			uremnode(args, firstnode(args));
			use_defpath = 1;
			if (nextnode(firstnode(args)))
			    next = (char *) getdata(nextnode(firstnode(args)));
		    } else {
			hn = &commandbn.node;
			is_builtin = 1;
			break;
		    }
		}
		if (!strcmp(next, "--"))
		     uremnode(args, firstnode(args));
	    }
	    if ((cflags & BINF_EXEC) && nextnode(firstnode(args))) {
		/*
		 * Check for compatibility options to exec builtin.
		 * It would be nice to do these more generically,
		 * but currently we don't have a mechanism for
		 * precommand modifiers.
		 */
		char *next = (char *) getdata(nextnode(firstnode(args)));
		char *cmdopt, *exec_argv0 = NULL;
		/*
		 * Careful here: we want to make sure a final dash
		 * is passed through in order that it still behaves
		 * as a precommand modifier (zsh equivalent of -l).
		 * It has to be last, but I think that's OK since
		 * people aren't likely to mix the option style
		 * with the zsh style.
		 */
		while (next && *next == '-' && strlen(next) >= 2) {
		    uremnode(args, firstnode(args));
		    if (!strcmp(next, "--"))
			break;
		    for (cmdopt = &next[1]; *cmdopt; ++cmdopt) {
			switch (*cmdopt) {
			case 'a':
			    /* argument is ARGV0 string */
			    if (cmdopt[1]) {
				exec_argv0 = cmdopt+1;
				/* position on last non-NULL character */
				cmdopt += strlen(cmdopt+1);
			    } else {
				if (!nextnode(firstnode(args))) {
				    zerr("exec flag -a requires a parameter");
				    errflag = lastval = 1;
				    return;
				}
				exec_argv0 = (char *)
				    getdata(nextnode(firstnode(args)));
				uremnode(args, firstnode(args));
			    }
			    break;
			case 'c':
			    cflags |= BINF_CLEARENV;
			    break;
			case 'l':
			    cflags |= BINF_DASH;
			    break;
			default:
			    zerr("unknown exec flag -%c", *cmdopt);
			    errflag = lastval = 1;
			    return;
			}
		    }
		    next = (char *) getdata(nextnode(firstnode(args)));
		}
		if (exec_argv0) {
		    char *str, *s;
		    size_t sz = strlen(exec_argv0);
		    str = s = zalloc(5 + 1 + sz + 1);
		    strcpy(s, "ARGV0=");
		    s+=6;
		    strcpy(s, exec_argv0);
		    zputenv(str);
		}
	    }
	    uremnode(args, firstnode(args));
	    hn = NULL;
	    if ((cflags & BINF_COMMAND) && unset(POSIXBUILTINS))
		break;
	}
    }

    /* Do prefork substitutions */
    esprefork = (assign || isset(MAGICEQUALSUBST)) ? PF_TYPESET : 0;
    if (args && htok)
	prefork(args, esprefork);

    if (type == WC_SIMPLE) {
	int unglobbed = 0;

	for (;;) {
	    char *cmdarg;

	    if (!(cflags & BINF_NOGLOB))
		while (!checked && !errflag && args && nonempty(args) &&
		       has_token((char *) peekfirst(args)))
		    zglob(args, firstnode(args), 0);
	    else if (!unglobbed) {
		for (node = firstnode(args); node; incnode(node))
		    untokenize((char *) getdata(node));
		unglobbed = 1;
	    }

	    /* Current shell should not fork unless the *
	     * exec occurs at the end of a pipeline.    */
	    if ((cflags & BINF_EXEC) && last1)
		do_exec = 1;

	    /* Empty command */
	    if (!args || empty(args)) {
		if (redir && nonempty(redir)) {
		    if (do_exec) {
			/* Was this "exec < foobar"? */
			nullexec = 1;
			break;
		    } else if (varspc) {
			nullexec = 2;
			break;
		    } else if (!nullcmd || !*nullcmd || opts[CSHNULLCMD] ||
			       (cflags & BINF_PREFIX)) {
			zerr("redirection with no command");
			errflag = lastval = 1;
			return;
		    } else if (!nullcmd || !*nullcmd || opts[SHNULLCMD]) {
			if (!args)
			    args = newlinklist();
			addlinknode(args, dupstring(":"));
		    } else if (readnullcmd && *readnullcmd &&
			       ((Redir) peekfirst(redir))->type == REDIR_READ &&
			       !nextnode(firstnode(redir))) {
			if (!args)
			    args = newlinklist();
			addlinknode(args, dupstring(readnullcmd));
		    } else {
			if (!args)
			    args = newlinklist();
			addlinknode(args, dupstring(nullcmd));
		    }
		} else if ((cflags & BINF_PREFIX) && (cflags & BINF_COMMAND)) {
		    lastval = 0;
		    return;
		} else {
		    cmdoutval = lastval;
		    if (varspc)
			addvars(state, varspc, 0);
		    if (errflag)
			lastval = errflag;
		    else
			lastval = cmdoutval;
		    if (isset(XTRACE)) {
			fputc('\n', xtrerr);
			fflush(xtrerr);
		    }
		    return;
		}
	    } else if (isset(RESTRICTED) && (cflags & BINF_EXEC) && do_exec) {
		zerrnam("exec", "%s: restricted",
			(char *) getdata(firstnode(args)));
		lastval = 1;
		return;
	    }

	    /*
	     * Quit looking for a command if:
	     * - there was an error; or
	     * - we checked the simple cases needing MAGIC_EQUAL_SUBST; or
	     * - we know we already found a builtin (because either:
	     *   - we loaded a builtin from a module, or
	     *   - we have determined there are options which would
	     *     require us to use the "command" builtin); or
	     * - we aren't using POSIX and so BINF_COMMAND indicates a zsh
	     *   precommand modifier is being used in place of the builtin
	     */
	    if (errflag || checked || is_builtin ||
		(unset(POSIXBUILTINS) && (cflags & BINF_COMMAND)))
		break;

	    cmdarg = (char *) peekfirst(args);
	    if (!(cflags & (BINF_BUILTIN | BINF_COMMAND)) &&
		(hn = shfunctab->getnode(shfunctab, cmdarg))) {
		is_shfunc = 1;
		break;
	    }
	    if (!(hn = builtintab->getnode(builtintab, cmdarg))) {
		if (cflags & BINF_BUILTIN) {
		    zwarn("no such builtin: %s", cmdarg);
		    lastval = 1;
		    if (oautocont >= 0)
			opts[AUTOCONTINUE] = oautocont;
		    return;
		}
		break;
	    }
	    if (!(hn->flags & BINF_PREFIX)) {
		is_builtin = 1;

		/* autoload the builtin if necessary */
		if (!(hn = resolvebuiltin(cmdarg, hn)))
		    return;
		break;
	    }
	    cflags &= ~BINF_BUILTIN & ~BINF_COMMAND;
	    cflags |= hn->flags;
	    uremnode(args, firstnode(args));
	    hn = NULL;
	}
    }

    if (errflag) {
	lastval = 1;
	if (oautocont >= 0)
	    opts[AUTOCONTINUE] = oautocont;
	return;
    }

    /* Get the text associated with this command. */
    if ((how & Z_ASYNC) ||
	(!sfcontext && !sourcelevel && (jobbing || (how & Z_TIMED))))
	text = getjobtext(state->prog, beg);
    else
	text = NULL;

    /* Set up special parameter $_ */

    setunderscore((args && nonempty(args)) ? ((char *) getdata(lastnode(args))) : "");

    /* Warn about "rm *" */
    if (type == WC_SIMPLE && interact && unset(RMSTARSILENT) &&
	isset(SHINSTDIN) && args && nonempty(args) &&
	nextnode(firstnode(args)) && !strcmp(peekfirst(args), "rm")) {
	LinkNode node, next;

	for (node = nextnode(firstnode(args)); node && !errflag; node = next) {
	    char *s = (char *) getdata(node);
	    int l = strlen(s);

	    next = nextnode(node);
	    if (s[0] == Star && !s[1]) {
		if (!checkrmall(pwd))
		    uremnode(args, node);
	    } else if (l > 2 && s[l - 2] == '/' && s[l - 1] == Star) {
		char t = s[l - 2];

		s[l - 2] = 0;
		if (!checkrmall(s))
		    uremnode(args, node);
		s[l - 2] = t;
	    }
	}
	if (!nextnode(firstnode(args)))
	    errflag = 1;
    }

    if (errflag) {
	lastval = 1;
	if (oautocont >= 0)
	    opts[AUTOCONTINUE] = oautocont;
	return;
    }

    if (type == WC_SIMPLE && !nullexec) {
	char *s;
	char trycd = (isset(AUTOCD) && isset(SHINSTDIN) &&
		      (!redir || empty(redir)) && args && !empty(args) &&
		      !nextnode(firstnode(args)) && *(char *)peekfirst(args));

	DPUTS((!args || empty(args)), "BUG: empty(args) in exec.c");
	if (!hn) {
	    /* Resolve external commands */
	    char *cmdarg = (char *) peekfirst(args);
	    char **checkpath = pathchecked;
	    int dohashcmd = isset(HASHCMDS);

	    hn = cmdnamtab->getnode(cmdnamtab, cmdarg);
	    if (hn && trycd && !isreallycom((Cmdnam)hn)) {
		if (!(((Cmdnam)hn)->node.flags & HASHED)) {
		    checkpath = path;
		    dohashcmd = 1;
		}
		cmdnamtab->removenode(cmdnamtab, cmdarg);
		cmdnamtab->freenode(hn);
		hn = NULL;
	    }
	    if (!hn && dohashcmd && strcmp(cmdarg, "..")) {
		for (s = cmdarg; *s && *s != '/'; s++);
		if (!*s)
		    hn = (HashNode) hashcmd(cmdarg, checkpath);
	    }
	}

	/* If no command found yet, see if it  *
	 * is a directory we should AUTOCD to. */
	if (!hn && trycd && (s = cancd(peekfirst(args)))) {
	    peekfirst(args) = (void *) s;
	    pushnode(args, dupstring("cd"));
	    if ((hn = builtintab->getnode(builtintab, "cd")))
		is_builtin = 1;
	}
    }

    /* This is nonzero if the command is a current shell procedure? */
    is_cursh = (is_builtin || is_shfunc || nullexec || type >= WC_CURSH);

    /**************************************************************************
     * Do we need to fork?  We need to fork if:                               *
     * 1) The command is supposed to run in the background. (or)              *
     * 2) There is no `exec' flag, and either:                                *
     *    a) This is a builtin or shell function with output piped somewhere. *
     *    b) This is an external command and we can't do a `fake exec'.       *
     *                                                                        *
     * A `fake exec' is possible if we have all the following conditions:     *
     * 1) last1 flag is 1.  This indicates that the current shell will not    *
     *    be needed after the current command.  This is typically the case    *
     *    when when the command is the last stage in a subshell, or is the    *
     *    last command after the option `-c'.                                 *
     * 2) We don't have any traps set.                                        *
     * 3) We don't have any files to delete.                                  *
     *                                                                        *
     * The condition above for a `fake exec' will also work for a current     *
     * shell command such as a builtin, but doesn't really buy us anything    *
     * (doesn't save us a process), since it is already running in the        *
     * current shell.                                                         *
     **************************************************************************/

    if ((how & Z_ASYNC) ||
	(!do_exec &&
	 (((is_builtin || is_shfunc) && output) ||
	  (!is_cursh && (last1 != 1 || nsigtrapped || havefiles()))))) {

	pid_t pid;
	int synch[2], flags;
	char dummy;
	struct timeval bgtime;

	child_block();
	pipe(synch);

	if ((pid = zfork(&bgtime)) == -1) {
	    close(synch[0]);
	    close(synch[1]);
	    if (oautocont >= 0)
		opts[AUTOCONTINUE] = oautocont;
	    return;
	}
	if (pid) {

	    close(synch[1]);
	    read(synch[0], &dummy, 1);
	    close(synch[0]);
#ifdef PATH_DEV_FD
	    closem(FDT_PROC_SUBST);
#endif
	    if (how & Z_ASYNC) {
		lastpid = (zlong) pid;
	    } else if (!jobtab[thisjob].stty_in_env && varspc) {
		/* search for STTY=... */
		Wordcode p = varspc;
		wordcode ac;

		while (wc_code(ac = *p) == WC_ASSIGN) {
		    if (!strcmp(ecrawstr(state->prog, p + 1, NULL), "STTY")) {
			jobtab[thisjob].stty_in_env = 1;
			break;
		    }
		    p += (WC_ASSIGN_TYPE(ac) == WC_ASSIGN_SCALAR ?
			  3 : WC_ASSIGN_NUM(ac) + 2);
		}
	    }
	    addproc(pid, text, 0, &bgtime);
	    if (oautocont >= 0)
		opts[AUTOCONTINUE] = oautocont;
	    return;
	}
	/* pid == 0 */
	close(synch[0]);
	flags = ((how & Z_ASYNC) ? ESUB_ASYNC : 0) | ESUB_PGRP;
	if ((type != WC_SUBSH) && !(how & Z_ASYNC))
	    flags |= ESUB_KEEPTRAP;
	entersubsh(flags);
	close(synch[1]);
	forked = 1;
	if (sigtrapped[SIGINT] & ZSIG_IGNORED)
	    holdintr();
#ifdef HAVE_NICE
	/* Check if we should run background jobs at a lower priority. */
	if ((how & Z_ASYNC) && isset(BGNICE))
	    nice(5);
#endif /* HAVE_NICE */

    } else if (is_cursh) {
	/* This is a current shell procedure that didn't need to fork.    *
	 * This includes current shell procedures that are being exec'ed, *
	 * as well as null execs.                                         */
	jobtab[thisjob].stat |= STAT_CURSH|STAT_NOPRINT;
    } else {
	/* This is an exec (real or fake) for an external command.    *
	 * Note that any form of exec means that the subshell is fake *
	 * (but we may be in a subshell already).                     */
	is_exec = 1;
    }

    if ((esglob = !(cflags & BINF_NOGLOB)) && args && htok) {
	LinkList oargs = args;
	globlist(args, 0);
	args = oargs;
    }
    if (errflag) {
	lastval = 1;
	goto err;
    }

    /* Make a copy of stderr for xtrace output before redirecting */
    fflush(xtrerr);
    if (isset(XTRACE) && xtrerr == stderr &&
	(type < WC_SUBSH || type == WC_TIMED)) {
	if ((newxtrerr = fdopen(movefd(dup(fileno(stderr))), "w"))) {
	    xtrerr = newxtrerr;
	    fdtable[fileno(xtrerr)] = FDT_XTRACE;
	}
    }

    /* Add pipeline input/output to mnodes */
    if (input)
	addfd(forked, save, mfds, 0, input, 0, NULL);
    if (output)
	addfd(forked, save, mfds, 1, output, 1, NULL);

    /* Do process substitutions */
    if (redir)
	spawnpipes(redir, nullexec);

    /* Do io redirections */
    while (redir && nonempty(redir)) {
	fn = (Redir) ugetnode(redir);

	DPUTS(fn->type == REDIR_HEREDOC || fn->type == REDIR_HEREDOCDASH,
	      "BUG: unexpanded here document");
	if (fn->type == REDIR_INPIPE) {
	    if (!checkclobberparam(fn) || fn->fd2 == -1) {
		if (fn->fd2 != -1)
		    zclose(fn->fd2);
		closemnodes(mfds);
		fixfds(save);
		execerr();
	    }
	    addfd(forked, save, mfds, fn->fd1, fn->fd2, 0, fn->varid);
	} else if (fn->type == REDIR_OUTPIPE) {
	    if (!checkclobberparam(fn) || fn->fd2 == -1) {
		if (fn->fd2 != -1)
		    zclose(fn->fd2);
		closemnodes(mfds);
		fixfds(save);
		execerr();
	    }
	    addfd(forked, save, mfds, fn->fd1, fn->fd2, 1, fn->varid);
	} else {
	    if (fn->type != REDIR_HERESTR && xpandredir(fn, redir))
		continue;
	    if (errflag) {
		closemnodes(mfds);
		fixfds(save);
		execerr();
	    }
	    if (isset(RESTRICTED) && IS_WRITE_FILE(fn->type)) {
		zwarn("writing redirection not allowed in restricted mode");
		execerr();
	    }
	    if (unset(EXECOPT))
		continue;
	    switch(fn->type) {
	    case REDIR_HERESTR:
		if (!checkclobberparam(fn))
		    fil = -1;
		else
		    fil = getherestr(fn);
		if (fil == -1) {
		    if (errno && errno != EINTR)
			zwarn("can't create temp file for here document: %e",
			      errno);
		    closemnodes(mfds);
		    fixfds(save);
		    execerr();
		}
		addfd(forked, save, mfds, fn->fd1, fil, 0, fn->varid);
		break;
	    case REDIR_READ:
	    case REDIR_READWRITE:
		if (!checkclobberparam(fn))
		    fil = -1;
		else if (fn->type == REDIR_READ)
		    fil = open(unmeta(fn->name), O_RDONLY | O_NOCTTY);
		else
		    fil = open(unmeta(fn->name),
			       O_RDWR | O_CREAT | O_NOCTTY, 0666);
		if (fil == -1) {
		    closemnodes(mfds);
		    fixfds(save);
		    if (errno != EINTR)
			zwarn("%e: %s", errno, fn->name);
		    execerr();
		}
		addfd(forked, save, mfds, fn->fd1, fil, 0, fn->varid);
		/* If this is 'exec < file', read from stdin, *
		 * not terminal, unless `file' is a terminal. */
		if (nullexec == 1 && fn->fd1 == 0 &&
		    isset(SHINSTDIN) && interact && !zleactive)
		    init_io();
		break;
	    case REDIR_CLOSE:
		if (fn->varid) {
		    char *s = fn->varid;
		    struct value vbuf;
		    Value v;
		    int bad = 0;

		    if (!(v = getvalue(&vbuf, &s, 0))) {
			bad = 1;
		    } else if (v->pm->node.flags & PM_READONLY) {
			bad = 2;
		    } else {
			fn->fd1 = (int)getintvalue(v);
			if (errflag)
			    bad = 1;
			else if (fn->fd1 > max_zsh_fd)
			    bad = 3;
			else if (fn->fd1 >= 10 &&
				 fdtable[fn->fd1] == FDT_INTERNAL)
			    bad = 4;
		    }
		    if (bad) {
			const char *bad_msg[] = {
			    "parameter %s does not contain a file descriptor",
			    "can't close file descriptor from readonly parameter %s",
			    "file descriptor %d out of range, not closed",
			    "file descriptor %d used by shell, not closed"
			};
			if (bad > 2)
			    zwarn(bad_msg[bad-1], fn->fd1);
			else
			    zwarn(bad_msg[bad-1], fn->varid);
			execerr();
		    }
		}
		if (!forked && fn->fd1 < 10 && save[fn->fd1] == -2)
		    save[fn->fd1] = movefd(fn->fd1);
		if (fn->fd1 < 10)
		    closemn(mfds, fn->fd1);
		zclose(fn->fd1);
		break;
	    case REDIR_MERGEIN:
	    case REDIR_MERGEOUT:
		if (fn->fd2 < 10)
		    closemn(mfds, fn->fd2);
		if (!checkclobberparam(fn))
		    fil = -1;
		else if (fn->fd2 > 9 &&
		    ((fdtable[fn->fd2] != FDT_UNUSED &&
		      fdtable[fn->fd2] != FDT_EXTERNAL) ||
		     fn->fd2 == coprocin ||
		     fn->fd2 == coprocout)) {
		    fil = -1;
		    errno = EBADF;
		} else {
		    int fd = fn->fd2;
		    if(fd == -2)
			fd = (fn->type == REDIR_MERGEOUT) ? coprocout : coprocin;
		    fil = dup(fd);
		}
		if (fil == -1) {
		    char fdstr[4];

		    closemnodes(mfds);
		    fixfds(save);
		    if (fn->fd2 != -2)
		    	sprintf(fdstr, "%d", fn->fd2);
		    if (errno)
			zwarn("%s: %e", fn->fd2 == -2 ? "coprocess" : fdstr,
			      errno);
		    execerr();
		}
		addfd(forked, save, mfds, fn->fd1, fil,
		      fn->type == REDIR_MERGEOUT, fn->varid);
		break;
	    default:
		if (!checkclobberparam(fn))
		    fil = -1;
		else if (IS_APPEND_REDIR(fn->type))
		    fil = open(unmeta(fn->name),
			       (unset(CLOBBER) && !IS_CLOBBER_REDIR(fn->type)) ?
			       O_WRONLY | O_APPEND | O_NOCTTY :
			       O_WRONLY | O_APPEND | O_CREAT | O_NOCTTY, 0666);
		else
		    fil = clobber_open(fn);
		if(fil != -1 && IS_ERROR_REDIR(fn->type))
		    dfil = dup(fil);
		else
		    dfil = 0;
		if (fil == -1 || dfil == -1) {
		    if(fil != -1)
			close(fil);
		    closemnodes(mfds);
		    fixfds(save);
		    if (errno && errno != EINTR)
			zwarn("%e: %s", errno, fn->name);
		    execerr();
		}
		addfd(forked, save, mfds, fn->fd1, fil, 1, fn->varid);
		if(IS_ERROR_REDIR(fn->type))
		    addfd(forked, save, mfds, 2, dfil, 1, NULL);
		break;
	    }
	    /* May be error in addfd due to setting parameter. */
	    if (errflag) {
		closemnodes(mfds);
		fixfds(save);
		execerr();
	    }
	}
    }

    /* We are done with redirection.  close the mnodes, *
     * spawning tee/cat processes as necessary.         */
    for (i = 0; i < 10; i++)
	if (mfds[i] && mfds[i]->ct >= 2)
	    closemn(mfds, i);

    xtrerr = stderr;
    if (nullexec) {
	if (nullexec == 1) {
	    /*
	     * If nullexec is 1 we specifically *don't* restore the original
	     * fd's before returning.
	     */
	    for (i = 0; i < 10; i++)
		if (save[i] != -2)
		    zclose(save[i]);
	    goto done;
	}
	/*
	 * If nullexec is 2, we have variables to add with the redirections
	 * in place.
	 */
	if (varspc)
	    addvars(state, varspc, 0);
	lastval = errflag ? errflag : cmdoutval;
	if (isset(XTRACE)) {
	    fputc('\n', xtrerr);
	    fflush(xtrerr);
	}
    } else if (isset(EXECOPT) && !errflag) {
	/*
	 * We delay the entersubsh() to here when we are exec'ing
	 * the current shell (including a fake exec to run a builtin then
	 * exit) in case there is an error return.
	 */
	if (is_exec) {
	    int flags = ((how & Z_ASYNC) ? ESUB_ASYNC : 0) |
		ESUB_PGRP | ESUB_FAKE;
	    if (type != WC_SUBSH)
		flags |= ESUB_KEEPTRAP;
	    if ((do_exec || (type >= WC_CURSH && last1 == 1)) 
		&& !forked)
		flags |= ESUB_REVERTPGRP;
	    entersubsh(flags);
	}
	if (type >= WC_CURSH) {
	    if (last1 == 1)
		do_exec = 1;
	    lastval = (execfuncs[type - WC_CURSH])(state, do_exec);
	} else if (is_builtin || is_shfunc) {
	    LinkList restorelist = 0, removelist = 0;
	    /* builtin or shell function */

	    if (!forked && ((cflags & BINF_COMMAND) ||
			    (unset(POSIXBUILTINS) && !assign) ||
			    (isset(POSIXBUILTINS) && !is_shfunc &&
			     !(hn->flags & BINF_PSPECIAL)))) {
		if (varspc)
		    save_params(state, varspc, &restorelist, &removelist);
		else
		    restorelist = removelist = NULL;
	    }
	    if (varspc) {
		/* Export this if the command is a shell function,
		 * but not if it's a builtin.
		 */
		int flags = 0;
		if (is_shfunc)
		    flags |= ADDVAR_EXPORT;
		if (restorelist)
		    flags |= ADDVAR_RESTORE;

		addvars(state, varspc, flags);
		if (errflag) {
		    if (restorelist)
			restore_params(restorelist, removelist);
		    lastval = 1;
		    fixfds(save);
		    goto done;
		}
	    }

	    if (is_shfunc) {
		/* It's a shell function */

#ifdef PATH_DEV_FD
		int i;

		for (i = 10; i <= max_zsh_fd; i++)
		    if (fdtable[i] >= FDT_PROC_SUBST)
			fdtable[i]++;
#endif
		if (subsh_close >= 0)
		    zclose(subsh_close);
		subsh_close = -1;

		execshfunc((Shfunc) hn, args);
#ifdef PATH_DEV_FD
		for (i = 10; i <= max_zsh_fd; i++)
		    if (fdtable[i] >= FDT_PROC_SUBST)
			if (--(fdtable[i]) <= FDT_PROC_SUBST)
			    zclose(i);
#endif
	    } else {
		/* It's a builtin */
		if (forked)
		    closem(FDT_INTERNAL);
		lastval = execbuiltin(args, (Builtin) hn);
#ifdef PATH_DEV_FD
		closem(FDT_PROC_SUBST);
#endif
		fflush(stdout);
		if (save[1] == -2) {
		    if (ferror(stdout)) {
			zwarn("write error: %e", errno);
			clearerr(stdout);
		    }
		} else
		    clearerr(stdout);
	    }
	    if (isset(PRINTEXITVALUE) && isset(SHINSTDIN) &&
		lastval && !subsh) {
		fprintf(stderr, "zsh: exit %ld\n", (long)lastval);
	    }

	    if (do_exec) {
		if (subsh)
		    _exit(lastval);

		/* If we are exec'ing a command, and we are not in a subshell, *
		 * then check if we should save the history file.              */
		if (isset(RCS) && interact && !nohistsave)
		    savehistfile(NULL, 1, HFILE_USE_OPTIONS);
		exit(lastval);
	    }
	    if (restorelist)
		restore_params(restorelist, removelist);

	} else {
	    if (!forked)
		setiparam("SHLVL", --shlvl);
	    if (do_exec) {
		/* If we are exec'ing a command, and we are not *
		 * in a subshell, then save the history file.   */
		if (!subsh && isset(RCS) && interact && !nohistsave)
		    savehistfile(NULL, 1, HFILE_USE_OPTIONS);
	    }
	    if (type == WC_SIMPLE) {
		if (varspc) {
		    addvars(state, varspc, ADDVAR_EXPORT|ADDVAR_RESTRICT);
		    if (errflag)
			_exit(1);
		}
		closem(FDT_INTERNAL);
		if (coprocin)
		    zclose(coprocin);
		if (coprocout)
		    zclose(coprocout);
#ifdef HAVE_GETRLIMIT
		if (!forked)
		    setlimits(NULL);
#endif
		if (how & Z_ASYNC) {
		    zsfree(STTYval);
		    STTYval = 0;
		}
		execute(args, cflags, use_defpath);
	    } else {		/* ( ... ) */
		DPUTS(varspc,
		      "BUG: assignment before complex command");
		list_pipe = 0;
		if (subsh_close >= 0)
		    zclose(subsh_close);
                subsh_close = -1;
		/* If we're forked (and we should be), no need to return */
		DPUTS(last1 != 1 && !forked, "BUG: not exiting?");
		DPUTS(type != WC_SUBSH, "Not sure what we're doing.");
		/* Skip word only used for try/always blocks */
		state->pc++;
		execlist(state, 0, 1);
	    }
	}
    }

  err:
    if (forked) {
	/*
	 * So what's going on here then?  Well, I'm glad you asked.
	 *
	 * If we create multios for use in a subshell we do
	 * this after forking, in this function above.  That
	 * means that the current (sub)process is responsible
	 * for clearing them up.  However, the processes won't
	 * go away until we have closed the fd's talking to them.
	 * Since we're about to exit the shell there's nothing
	 * to stop us closing all fd's (including the ones 0 to 9
	 * that we usually leave alone).
	 *
	 * Then we wait for any processes.  When we forked,
	 * we cleared the jobtable and started a new job just for
	 * any oddments like this, so if there aren't any we won't
	 * need to wait.  The result of not waiting is that
	 * the multios haven't flushed the fd's properly, leading
	 * to obscure missing data.
	 *
	 * It would probably be cleaner to ensure that the
	 * parent shell handled multios, but that requires
	 * some architectural changes which are likely to be
	 * hairy.
	 */
	for (i = 0; i < 10; i++)
	    if (fdtable[i] != FDT_UNUSED)
		close(i);
	closem(FDT_UNUSED);
	if (thisjob != -1)
	    waitjobs();
	_exit(lastval);
    }
    fixfds(save);

 done:
    if (newxtrerr) {
	fil = fileno(newxtrerr);
	fclose(newxtrerr);
	xtrerr = oxtrerr;
	zclose(fil);
    }

    zsfree(STTYval);
    STTYval = 0;
    if (oautocont >= 0)
	opts[AUTOCONTINUE] = oautocont;
}

/* Arrange to have variables restored. */

/**/
static void
save_params(Estate state, Wordcode pc, LinkList *restore_p, LinkList *remove_p)
{
    Param pm;
    char *s;
    wordcode ac;

    *restore_p = newlinklist();
    *remove_p = newlinklist();

    while (wc_code(ac = *pc) == WC_ASSIGN) {
	s = ecrawstr(state->prog, pc + 1, NULL);
	if ((pm = (Param) paramtab->getnode(paramtab, s))) {
	    if (pm->env)
		delenv(pm);
	    if (!(pm->node.flags & PM_SPECIAL)) {
		paramtab->removenode(paramtab, s);
	    } else if (!(pm->node.flags & PM_READONLY) &&
		       (unset(RESTRICTED) || !(pm->node.flags & PM_RESTRICTED))) {
		Param tpm = (Param) hcalloc(sizeof *tpm);
		tpm->node.nam = pm->node.nam;
		copyparam(tpm, pm, 1);
		pm = tpm;
	    }
	    addlinknode(*remove_p, dupstring(s));
	    addlinknode(*restore_p, pm);
	} else
	    addlinknode(*remove_p, dupstring(s));

	pc += (WC_ASSIGN_TYPE(ac) == WC_ASSIGN_SCALAR ?
	       3 : WC_ASSIGN_NUM(ac) + 2);
    }
}

/* Restore saved parameters after executing a shfunc or builtin */

/**/
static void
restore_params(LinkList restorelist, LinkList removelist)
{
    Param pm;
    char *s;

    /* remove temporary parameters */
    while ((s = (char *) ugetnode(removelist))) {
	if ((pm = (Param) paramtab->getnode(paramtab, s)) &&
	    !(pm->node.flags & PM_SPECIAL)) {
	    pm->node.flags &= ~PM_READONLY;
	    unsetparam_pm(pm, 0, 0);
	}
    }

    if (restorelist) {
	/* restore saved parameters */
	while ((pm = (Param) ugetnode(restorelist))) {
	    if (pm->node.flags & PM_SPECIAL) {
		Param tpm = (Param) paramtab->getnode(paramtab, pm->node.nam);

		DPUTS(!tpm || PM_TYPE(pm->node.flags) != PM_TYPE(tpm->node.flags) ||
		      !(pm->node.flags & PM_SPECIAL),
		      "BUG: in restoring special parameters");
		if (!pm->env && tpm->env)
		    delenv(tpm);
		tpm->node.flags = pm->node.flags;
		switch (PM_TYPE(pm->node.flags)) {
		case PM_SCALAR:
		    tpm->gsu.s->setfn(tpm, pm->u.str);
		    break;
		case PM_INTEGER:
		    tpm->gsu.i->setfn(tpm, pm->u.val);
		    break;
		case PM_EFLOAT:
		case PM_FFLOAT:
		    tpm->gsu.f->setfn(tpm, pm->u.dval);
		    break;
		case PM_ARRAY:
		    tpm->gsu.a->setfn(tpm, pm->u.arr);
		    break;
		case PM_HASHED:
		    tpm->gsu.h->setfn(tpm, pm->u.hash);
		    break;
		}
		pm = tpm;
	    } else
		paramtab->addnode(paramtab, pm->node.nam, pm);
	    if ((pm->node.flags & PM_EXPORTED) && ((s = getsparam(pm->node.nam))))
		addenv(pm, s);
	}
    }
}

/* restore fds after redirecting a builtin */

/**/
static void
fixfds(int *save)
{
    int old_errno = errno;
    int i;

    for (i = 0; i != 10; i++)
	if (save[i] != -2)
	    redup(save[i], i);
    errno = old_errno;
}

/*
 * Close internal shell fds.
 *
 * Close any that are marked as used if "how" is FDT_UNUSED, else
 * close any with the value "how".
 */

/**/
mod_export void
closem(int how)
{
    int i;

    for (i = 10; i <= max_zsh_fd; i++)
	if (fdtable[i] != FDT_UNUSED &&
	    (how == FDT_UNUSED || fdtable[i] == how))
	    zclose(i);
}

/* convert here document into a here string */

/**/
char *
gethere(char *str, int typ)
{
    char *buf;
    int bsiz, qt = 0, strip = 0;
    char *s, *t, *bptr, c;

    for (s = str; *s; s++)
	if (inull(*s)) {
	    qt = 1;
	    break;
	}
    str = quotesubst(str);
    untokenize(str);
    if (typ == REDIR_HEREDOCDASH) {
	strip = 1;
	while (*str == '\t')
	    str++;
    }
    bptr = buf = zalloc(bsiz = 256);
    for (;;) {
	t = bptr;

	while ((c = hgetc()) == '\t' && strip)
	    ;
	for (;;) {
	    if (bptr == buf + bsiz) {
		char *newbuf = realloc(buf, 2 * bsiz);
		if (!newbuf) {
		    /* out of memory */
		    zfree(buf, bsiz);
		    return NULL;
		}
		buf = newbuf;
		t = buf + bsiz - (bptr - t);
		bptr = buf + bsiz;
		bsiz *= 2;
	    }
	    if (lexstop || c == '\n')
		break;
	    *bptr++ = c;
	    c = hgetc();
	}
	*bptr = '\0';
	if (!strcmp(t, str))
	    break;
	if (lexstop) {
	    t = bptr;
	    break;
	}
	*bptr++ = '\n';
    }
    if (t > buf && t[-1] == '\n')
	t--;
    *t = '\0';
    if (!qt) {
	int ef = errflag;

	parsestr(buf);

	if (!errflag)
	    errflag = ef;
    }
    s = dupstring(buf);
    zfree(buf, bsiz);
    return s;
}

/* open here string fd */

/**/
static int
getherestr(struct redir *fn)
{
    char *s, *t;
    int fd, len;

    t = fn->name;
    singsub(&t);
    untokenize(t);
    unmetafy(t, &len);
    t[len++] = '\n';
    if ((fd = gettempfile(NULL, 1, &s)) < 0)
	return -1;
    write(fd, t, len);
    close(fd);
    fd = open(s, O_RDONLY | O_NOCTTY);
    unlink(s);
    return fd;
}

/*
 * Test if some wordcode starts with a simple redirection of type
 * redir_type.  If it does, return the name of the file, copied onto
 * the heap.  If it doesn't, return NULL.
 */

static char *
simple_redir_name(Eprog prog, int redir_type)
{
    Wordcode pc;

    pc = prog->prog;
    if (prog != &dummy_eprog &&
	wc_code(pc[0]) == WC_LIST && (WC_LIST_TYPE(pc[0]) & Z_END) &&
	wc_code(pc[1]) == WC_SUBLIST && !WC_SUBLIST_FLAGS(pc[1]) &&
	WC_SUBLIST_TYPE(pc[1]) == WC_SUBLIST_END &&
	wc_code(pc[2]) == WC_PIPE && WC_PIPE_TYPE(pc[2]) == WC_PIPE_END &&
	wc_code(pc[3]) == WC_REDIR && WC_REDIR_TYPE(pc[3]) == redir_type &&
	!WC_REDIR_VARID(pc[3]) &&
	!pc[4] &&
	wc_code(pc[6]) == WC_SIMPLE && !WC_SIMPLE_ARGC(pc[6])) {
	return dupstring(ecrawstr(prog, pc + 5, NULL));
    }

    return NULL;
}

/* $(...) */

/**/
LinkList
getoutput(char *cmd, int qt)
{
    Eprog prog;
    int pipes[2];
    pid_t pid;
    char *s;

    if (!(prog = parse_string(cmd, 0)))
	return NULL;

    if ((s = simple_redir_name(prog, REDIR_READ))) {
	/* $(< word) */
	int stream;

	singsub(&s);
	if (errflag)
	    return NULL;
	untokenize(s);
	if ((stream = open(unmeta(s), O_RDONLY | O_NOCTTY)) == -1) {
	    zerr("%e: %s", errno, s);
	    return NULL;
	}
	return readoutput(stream, qt);
    }
    mpipe(pipes);
    child_block();
    cmdoutval = 0;
    if ((cmdoutpid = pid = zfork(NULL)) == -1) {
	/* fork error */
	zclose(pipes[0]);
	zclose(pipes[1]);
	errflag = 1;
	cmdoutpid = 0;
	child_unblock();
	return NULL;
    } else if (pid) {
	LinkList retval;

	zclose(pipes[1]);
	retval = readoutput(pipes[0], qt);
	fdtable[pipes[0]] = FDT_UNUSED;
	waitforpid(pid, 0);		/* unblocks */
	lastval = cmdoutval;
	return retval;
    }
    /* pid == 0 */
    child_unblock();
    zclose(pipes[0]);
    redup(pipes[1], 1);
    entersubsh(ESUB_PGRP|ESUB_NOMONITOR);
    cmdpush(CS_CMDSUBST);
    execode(prog, 0, 1);
    cmdpop();
    close(1);
    _exit(lastval);
    zerr("exit returned in child!!");
    kill(getpid(), SIGKILL);
    return NULL;
}

/* read output of command substitution */

/**/
mod_export LinkList
readoutput(int in, int qt)
{
    LinkList ret;
    char *buf, *ptr;
    int bsiz, c, cnt = 0;
    FILE *fin;

    fin = fdopen(in, "r");
    ret = newlinklist();
    ptr = buf = (char *) hcalloc(bsiz = 64);
    while ((c = fgetc(fin)) != EOF || errno == EINTR) {
	if (c == EOF) {
	    errno = 0;
	    clearerr(fin);
	    continue;
	}
	if (imeta(c)) {
	    *ptr++ = Meta;
	    c ^= 32;
	    cnt++;
	}
	if (++cnt >= bsiz) {
	    char *pp = (char *) hcalloc(bsiz *= 2);

	    memcpy(pp, buf, cnt - 1);
	    ptr = (buf = pp) + cnt - 1;
	}
	*ptr++ = c;
    }
    fclose(fin);
    while (cnt && ptr[-1] == '\n')
	ptr--, cnt--;
    *ptr = '\0';
    if (qt) {
	if (!cnt) {
	    *ptr++ = Nularg;
	    *ptr = '\0';
	}
	addlinknode(ret, buf);
    } else {
	char **words = spacesplit(buf, 0, 1, 0);

	while (*words) {
	    if (isset(GLOBSUBST))
		shtokenize(*words);
	    addlinknode(ret, *words++);
	}
    }
    return ret;
}

/**/
static Eprog
parsecmd(char *cmd)
{
    char *str;
    Eprog prog;

    for (str = cmd + 2; *str && *str != Outpar; str++);
    if (!*str || cmd[1] != Inpar) {
	zerr("oops.");
	return NULL;
    }
    *str = '\0';
    if (str[1] || !(prog = parse_string(cmd + 2, 0))) {
	zerr("parse error in process substitution");
	return NULL;
    }
    return prog;
}

/* =(...) */

/**/
char *
getoutputfile(char *cmd)
{
    pid_t pid;
    char *nam;
    Eprog prog;
    int fd;
    char *s;

    if (thisjob == -1)
	return NULL;
    if (!(prog = parsecmd(cmd)))
	return NULL;
    if (!(nam = gettempname(NULL, 0)))
	return NULL;

    if ((s = simple_redir_name(prog, REDIR_HERESTR))) {
	/*
	 * =(<<<stuff).  Optimise a la $(<file).  It's
	 * effectively the reverse, converting a string into a file name
	 * rather than vice versa.
	 */
	singsub(&s);
	if (errflag)
	    s = NULL;
	else
	    untokenize(s);
    }

    if (!jobtab[thisjob].filelist)
	jobtab[thisjob].filelist = znewlinklist();
    zaddlinknode(jobtab[thisjob].filelist, nam);

    if (!s)
	child_block();
    fd = open(nam, O_WRONLY | O_CREAT | O_EXCL | O_NOCTTY, 0600);

    if (s) {
	/* optimised here-string */
	int len;
	unmetafy(s, &len);
	write(fd, s, len);
	close(fd);
	return nam;
    }

    if (fd < 0 || (cmdoutpid = pid = zfork(NULL)) == -1) {
	/* fork or open error */
	child_unblock();
	return nam;
    } else if (pid) {
	int os;

	close(fd);
	os = jobtab[thisjob].stat;
	waitforpid(pid, 0);
	cmdoutval = 0;
	jobtab[thisjob].stat = os;
	return nam;
    }

    /* pid == 0 */
    redup(fd, 1);
    entersubsh(ESUB_PGRP|ESUB_NOMONITOR);
    cmdpush(CS_CMDSUBST);
    execode(prog, 0, 1);
    cmdpop();
    close(1);
    _exit(lastval);
    zerr("exit returned in child!!");
    kill(getpid(), SIGKILL);
    return NULL;
}

#if !defined(PATH_DEV_FD) && defined(HAVE_FIFOS)
/* get a temporary named pipe */

static char *
namedpipe(void)
{
    char *tnam = gettempname(NULL, 1);

# ifdef HAVE_MKFIFO
    if (mkfifo(tnam, 0600) < 0)
# else
    if (mknod(tnam, 0010600, 0) < 0)
# endif
	return NULL;
    return tnam;
}
#endif /* ! PATH_DEV_FD && HAVE_FIFOS */

/* <(...) or >(...) */

/**/
char *
getproc(char *cmd)
{
#if !defined(HAVE_FIFOS) && !defined(PATH_DEV_FD)
    zerr("doesn't look like your system supports FIFOs.");
    return NULL;
#else
    Eprog prog;
    int out = *cmd == Inang;
    char *pnam;
    pid_t pid;
    struct timeval bgtime;

#ifndef PATH_DEV_FD
    int fd;

    if (thisjob == -1)
	return NULL;
    if (!(pnam = namedpipe()))
	return NULL;
    if (!(prog = parsecmd(cmd)))
	return NULL;
    if (!jobtab[thisjob].filelist)
	jobtab[thisjob].filelist = znewlinklist();
    zaddlinknode(jobtab[thisjob].filelist, ztrdup(pnam));

    if ((pid = zfork(&bgtime))) {
	if (pid == -1)
	    return NULL;
	if (!out)
	    addproc(pid, NULL, 1, &bgtime);
	return pnam;
    }
    closem(FDT_UNUSED);
    fd = open(pnam, out ? O_WRONLY | O_NOCTTY : O_RDONLY | O_NOCTTY);
    if (fd == -1) {
	zerr("can't open %s: %e", pnam, errno);
	_exit(1);
    }
    entersubsh(ESUB_ASYNC|ESUB_PGRP);
    redup(fd, out);
#else /* PATH_DEV_FD */
    int pipes[2];

    if (thisjob == -1)
	return NULL;
    pnam = hcalloc(strlen(PATH_DEV_FD) + 6);
    if (!(prog = parsecmd(cmd)))
	return NULL;
    mpipe(pipes);
    if ((pid = zfork(&bgtime))) {
	sprintf(pnam, "%s/%d", PATH_DEV_FD, pipes[!out]);
	zclose(pipes[out]);
	if (pid == -1)
	{
	    zclose(pipes[!out]);
	    return NULL;
	}
	fdtable[pipes[!out]] = FDT_PROC_SUBST;
	if (!out)
	{
	    addproc(pid, NULL, 1, &bgtime);
	}
	return pnam;
    }
    entersubsh(ESUB_ASYNC|ESUB_PGRP);
    redup(pipes[out], out);
    closem(FDT_UNUSED);   /* this closes pipes[!out] as well */
#endif /* PATH_DEV_FD */

    cmdpush(CS_CMDSUBST);
    execode(prog, 0, 1);
    cmdpop();
    zclose(out);
    _exit(lastval);
    return NULL;
#endif   /* HAVE_FIFOS and PATH_DEV_FD not defined */
}

/*
 * > >(...) or < <(...) (does not use named pipes)
 *
 * If the second argument is 1, this is part of
 * an "exec < <(...)" or "exec > >(...)" and we shouldn't
 * wait for the job to finish before continuing.
 */

/**/
static int
getpipe(char *cmd, int nullexec)
{
    Eprog prog;
    int pipes[2], out = *cmd == Inang;
    pid_t pid;
    struct timeval bgtime;

    if (!(prog = parsecmd(cmd)))
	return -1;
    mpipe(pipes);
    if ((pid = zfork(&bgtime))) {
	zclose(pipes[out]);
	if (pid == -1) {
	    zclose(pipes[!out]);
	    return -1;
	}
	if (!nullexec)
	    addproc(pid, NULL, 1, &bgtime);
	return pipes[!out];
    }
    entersubsh(ESUB_ASYNC|ESUB_PGRP);
    redup(pipes[out], out);
    closem(FDT_UNUSED);	/* this closes pipes[!out] as well */
    cmdpush(CS_CMDSUBST);
    execode(prog, 0, 1);
    cmdpop();
    _exit(lastval);
    return 0;
}

/* open pipes with fds >= 10 */

/**/
static void
mpipe(int *pp)
{
    pipe(pp);
    pp[0] = movefd(pp[0]);
    pp[1] = movefd(pp[1]);
}

/*
 * Do process substitution with redirection
 *
 * If the second argument is 1, this is part of
 * an "exec < <(...)" or "exec > >(...)" and we shouldn't
 * wait for the job to finish before continuing.
 * Likewise, we shouldn't wait if we are opening the file
 * descriptor using the {fd}>>(...) notation since it stays
 * valid for subsequent commands.
 */

/**/
static void
spawnpipes(LinkList l, int nullexec)
{
    LinkNode n;
    Redir f;
    char *str;

    n = firstnode(l);
    for (; n; incnode(n)) {
	f = (Redir) getdata(n);
	if (f->type == REDIR_OUTPIPE || f->type == REDIR_INPIPE) {
	    str = f->name;
	    f->fd2 = getpipe(str, nullexec || f->varid);
	}
    }
}

extern int tracingcond;

/* evaluate a [[ ... ]] */

/**/
static int
execcond(Estate state, UNUSED(int do_exec))
{
    int stat;

    state->pc--;
    if (isset(XTRACE)) {
	printprompt4();
	fprintf(xtrerr, "[[");
	tracingcond++;
    }
    cmdpush(CS_COND);
    stat = evalcond(state, NULL);
    /*
     * 2 indicates a syntax error.  For compatibility, turn this
     * into a shell error.
     */
    if (stat == 2)
	errflag = 1;
    cmdpop();
    if (isset(XTRACE)) {
	fprintf(xtrerr, " ]]\n");
	fflush(xtrerr);
	tracingcond--;
    }
    return stat;
}

/* evaluate a ((...)) arithmetic command */

/**/
static int
execarith(Estate state, UNUSED(int do_exec))
{
    char *e;
    mnumber val = zero_mnumber;
    int htok = 0;

    if (isset(XTRACE)) {
	printprompt4();
	fprintf(xtrerr, "((");
    }
    cmdpush(CS_MATH);
    e = ecgetstr(state, EC_DUPTOK, &htok);
    if (htok)
	singsub(&e);
    if (isset(XTRACE))
	fprintf(xtrerr, " %s", e);

    val = matheval(e);

    cmdpop();

    if (isset(XTRACE)) {
	fprintf(xtrerr, " ))\n");
	fflush(xtrerr);
    }
    if (errflag) {
	errflag = 0;
	return 2;
    }
    /* should test for fabs(val.u.d) < epsilon? */
    return (val.type == MN_INTEGER) ? val.u.l == 0 : val.u.d == 0.0;
}

/* perform time ... command */

/**/
static int
exectime(Estate state, UNUSED(int do_exec))
{
    int jb;

    jb = thisjob;
    if (WC_TIMED_TYPE(state->pc[-1]) == WC_TIMED_EMPTY) {
	shelltime();
	return 0;
    }
    execpline(state, *state->pc++, Z_TIMED|Z_SYNC, 0);
    thisjob = jb;
    return lastval;
}

/* Define a shell function */

/**/
static int
execfuncdef(Estate state, UNUSED(int do_exec))
{
    Shfunc shf;
    char *s = NULL;
    int signum, nprg, sbeg, nstrs, npats, len, plen, i, htok = 0, ret = 0;
    Wordcode beg = state->pc, end;
    Eprog prog;
    Patprog *pp;
    LinkList names;

    end = beg + WC_FUNCDEF_SKIP(state->pc[-1]);
    names = ecgetlist(state, *state->pc++, EC_DUPTOK, &htok);
    nprg = end - beg;
    sbeg = *state->pc++;
    nstrs = *state->pc++;
    npats = *state->pc++;

    nprg = (end - state->pc);
    plen = nprg * sizeof(wordcode);
    len = plen + (npats * sizeof(Patprog)) + nstrs;

    if (htok && names)
	execsubst(names);

    while (!names || (s = (char *) ugetnode(names))) {
	if (!names) {
	    prog = (Eprog) zhalloc(sizeof(*prog));
	    prog->nref = -1; /* on the heap */
	} else {
	    prog = (Eprog) zalloc(sizeof(*prog));
	    prog->nref = 1; /* allocated from permanent storage */
	}
	prog->npats = npats;
	prog->len = len;
	if (state->prog->dump || !names) {
	    if (!names) {
		prog->flags = EF_HEAP;
		prog->dump = NULL;
		prog->pats = pp = (Patprog *) zhalloc(npats * sizeof(Patprog));
	    } else {
		prog->flags = EF_MAP;
		incrdumpcount(state->prog->dump);
		prog->dump = state->prog->dump;
		prog->pats = pp = (Patprog *) zalloc(npats * sizeof(Patprog));
	    }
	    prog->prog = state->pc;
	    prog->strs = state->strs + sbeg;
	} else {
	    prog->flags = EF_REAL;
	    prog->pats = pp = (Patprog *) zalloc(len);
	    prog->prog = (Wordcode) (prog->pats + npats);
	    prog->strs = (char *) (prog->prog + nprg);
	    prog->dump = NULL;
	    memcpy(prog->prog, state->pc, plen);
	    memcpy(prog->strs, state->strs + sbeg, nstrs);
	}
	for (i = npats; i--; pp++)
	    *pp = dummy_patprog1;
	prog->shf = NULL;

	shf = (Shfunc) zalloc(sizeof(*shf));
	shf->funcdef = prog;
	shf->node.flags = 0;
	shf->filename = ztrdup(scriptfilename);
	shf->lineno = lineno;

	if (!names) {
	    /*
	     * Anonymous function, execute immediately.
	     * Function name is "(anon)", parameter list is empty.
	     */
	    LinkList args = newlinklist();

	    shf->node.nam = "(anon)";
	    addlinknode(args, shf->node.nam);

	    execshfunc(shf, args);
	    ret = lastval;
	    break;
	} else {
	    /* is this shell function a signal trap? */
	    if (!strncmp(s, "TRAP", 4) &&
		(signum = getsignum(s + 4)) != -1) {
		if (settrap(signum, NULL, ZSIG_FUNC)) {
		    freeeprog(shf->funcdef);
		    zfree(shf, sizeof(*shf));
		    state->pc = end;
		    return 1;
		}

		/*
		 * Remove the old node explicitly in case it has
		 * an alternative name
		 */
		removetrapnode(signum);
	    }
	    shfunctab->addnode(shfunctab, ztrdup(s), shf);
	}
    }
    state->pc = end;
    return ret;
}

/* Main entry point to execute a shell function. */

/**/
static void
execshfunc(Shfunc shf, LinkList args)
{
    LinkList last_file_list = NULL;
    unsigned char *ocs;
    int ocsp, osfc;

    if (errflag)
	return;

    if (!list_pipe && thisjob != list_pipe_job && !hasprocs(thisjob)) {
	/* Without this deletejob the process table *
	 * would be filled by a recursive function. */
	last_file_list = jobtab[thisjob].filelist;
	jobtab[thisjob].filelist = NULL;
	deletejob(jobtab + thisjob);
    }

    if (isset(XTRACE)) {
	LinkNode lptr;
	printprompt4();
	if (args)
	    for (lptr = firstnode(args); lptr; incnode(lptr)) {
		if (lptr != firstnode(args))
		    fputc(' ', xtrerr);
		quotedzputs((char *)getdata(lptr), xtrerr);
	    }
	fputc('\n', xtrerr);
	fflush(xtrerr);
    }
    ocs = cmdstack;
    ocsp = cmdsp;
    cmdstack = (unsigned char *) zalloc(CMDSTACKSZ);
    cmdsp = 0;
    if ((osfc = sfcontext) == SFC_NONE)
	sfcontext = SFC_DIRECT;
    doshfunc(shf, args, 0);
    sfcontext = osfc;
    free(cmdstack);
    cmdstack = ocs;
    cmdsp = ocsp;

    if (!list_pipe)
	deletefilelist(last_file_list);
}

/* Function to execute the special type of command that represents an *
 * autoloaded shell function.  The command structure tells us which   *
 * function it is.  This function is actually called as part of the   *
 * execution of the autoloaded function itself, so when the function  *
 * has been autoloaded, its list is just run with no frills.          */

/**/
static int
execautofn(Estate state, UNUSED(int do_exec))
{
    Shfunc shf;
    char *oldscriptname, *oldscriptfilename;

    if (!(shf = loadautofn(state->prog->shf, 1, 0)))
	return 1;

    /*
     * Probably we didn't know the filename where this function was
     * defined yet.
     */
    if (funcstack && !funcstack->filename)
	funcstack->filename = dupstring(shf->filename);

    oldscriptname = scriptname;
    oldscriptfilename = scriptfilename;
    scriptname = scriptfilename = dupstring(shf->node.nam);
    execode(shf->funcdef, 1, 0);
    scriptname = oldscriptname;
    scriptfilename = oldscriptfilename;

    return lastval;
}

/**/
Shfunc
loadautofn(Shfunc shf, int fksh, int autol)
{
    int noalias = noaliases, ksh = 1;
    Eprog prog;
    char *fname;

    pushheap();

    noaliases = (shf->node.flags & PM_UNALIASED);
    prog = getfpfunc(shf->node.nam, &ksh, &fname);
    noaliases = noalias;

    if (ksh == 1) {
	ksh = fksh;
	if (ksh == 1)
	    ksh = (shf->node.flags & PM_KSHSTORED) ? 2 :
		  (shf->node.flags & PM_ZSHSTORED) ? 0 : 1;
    }

    if (prog == &dummy_eprog) {
	/* We're not actually in the function; decrement locallevel */
	locallevel--;
	zwarn("%s: function definition file not found", shf->node.nam);
	locallevel++;
	popheap();
	return NULL;
    }
    if (!prog) {
	zsfree(fname);
	return NULL;
    }
    if (ksh == 2 || (ksh == 1 && isset(KSHAUTOLOAD))) {
	if (autol) {
	    prog->flags |= EF_RUN;

	    freeeprog(shf->funcdef);
	    if (prog->flags & EF_MAP)
		shf->funcdef = prog;
	    else
		shf->funcdef = dupeprog(prog, 0);
	    shf->node.flags &= ~PM_UNDEFINED;
	    shf->filename = fname;
	} else {
	    VARARR(char, n, strlen(shf->node.nam) + 1);
	    strcpy(n, shf->node.nam);
	    execode(prog, 1, 0);
	    shf = (Shfunc) shfunctab->getnode(shfunctab, n);
	    if (!shf || (shf->node.flags & PM_UNDEFINED)) {
		/* We're not actually in the function; decrement locallevel */
		locallevel--;
		zwarn("%s: function not defined by file", n);
		locallevel++;
		popheap();
		zsfree(fname);
		return NULL;
	    }
	}
    } else {
	freeeprog(shf->funcdef);
	if (prog->flags & EF_MAP)
	    shf->funcdef = stripkshdef(prog, shf->node.nam);
	else
	    shf->funcdef = dupeprog(stripkshdef(prog, shf->node.nam), 0);
	shf->node.flags &= ~PM_UNDEFINED;
	shf->filename = fname;
    }
    popheap();

    return shf;
}

/*
 * execute a shell function
 *
 * name is the name of the function
 *
 * prog is the code to execute
 *
 * doshargs, if set, are parameters to pass to the function,
 * in which the first element is the function name (even if
 * FUNCTIONARGZERO is set as this is handled inside this function).
 *
 * If noreturnval is nonzero, then reset the current return
 * value (lastval) to its value before the shell function
 * was executed.  However, in any case return the status value
 * from the function (i.e. if noreturnval is not set, this
 * will be the same as lastval).
 */

/**/
mod_export int
doshfunc(Shfunc shfunc, LinkList doshargs, int noreturnval)
{
    char **tab, **x, *oargv0;
    int oldzoptind, oldlastval, oldoptcind, oldnumpipestats, ret;
    int *oldpipestats = NULL;
    char saveopts[OPT_SIZE], *oldscriptname = scriptname;
    char *name = shfunc->node.nam;
    int flags = shfunc->node.flags;
    char *fname = dupstring(name);
    int obreaks, saveemulation ;
    Eprog prog;
    struct funcstack fstack;
#ifdef MAX_FUNCTION_DEPTH
    static int funcdepth;
#endif

    pushheap();

    oargv0 = NULL;
    obreaks = breaks;;
    if (trap_state == TRAP_STATE_PRIMED)
	trap_return--;
    oldlastval = lastval;
    oldnumpipestats = numpipestats;
    if (noreturnval) {
	/*
	 * Easiest to use the heap here since we're bracketed
	 * immediately by a pushheap/popheap pair.
	 */
	size_t bytes = sizeof(int)*numpipestats;
	oldpipestats = (int *)zhalloc(bytes);
	memcpy(oldpipestats, pipestats, bytes);
    }

    starttrapscope();

    tab = pparams;
    if (!(flags & PM_UNDEFINED))
	scriptname = dupstring(name);
    oldzoptind = zoptind;
    zoptind = 1;
    oldoptcind = optcind;
    optcind = 0;

    /* We need to save the current options even if LOCALOPTIONS is *
     * not currently set.  That's because if it gets set in the    *
     * function we need to restore the original options on exit.   */
    memcpy(saveopts, opts, sizeof(opts));
    saveemulation = emulation;

    if (flags & PM_TAGGED)
	opts[XTRACE] = 1;
    opts[PRINTEXITVALUE] = 0;
    if (doshargs) {
	LinkNode node;

	node = firstnode(doshargs);
	pparams = x = (char **) zshcalloc(((sizeof *x) *
					 (1 + countlinknodes(doshargs))));
	if (isset(FUNCTIONARGZERO)) {
	    oargv0 = argzero;
	    argzero = ztrdup(getdata(node));
	}
	/* first node contains name regardless of option */
	node = node->next;
	for (; node; node = node->next, x++)
	    *x = ztrdup(getdata(node));
    } else {
	pparams = (char **) zshcalloc(sizeof *pparams);
	if (isset(FUNCTIONARGZERO)) {
	    oargv0 = argzero;
	    argzero = ztrdup(argzero);
	}
    }
#ifdef MAX_FUNCTION_DEPTH
    if(++funcdepth > MAX_FUNCTION_DEPTH)
    {
        zerr("maximum nested function level reached");
	goto undoshfunc;
    }
#endif
    fstack.name = dupstring(name);
    /*
     * The caller is whatever is immediately before on the stack,
     * unless we're at the top, in which case it's the script
     * or interactive shell name.
     */
    fstack.caller = funcstack ? funcstack->name :
	dupstring(oargv0 ? oargv0 : argzero);
    fstack.lineno = lineno;
    fstack.prev = funcstack;
    fstack.tp = FS_FUNC;
    funcstack = &fstack;

    fstack.flineno = shfunc->lineno;
    fstack.filename = dupstring(shfunc->filename);

    prog = shfunc->funcdef;
    if (prog->flags & EF_RUN) {
	Shfunc shf;

	prog->flags &= ~EF_RUN;

	runshfunc(prog, NULL, fstack.name);

	if (!(shf = (Shfunc) shfunctab->getnode(shfunctab,
						(name = fname)))) {
	    zwarn("%s: function not defined by file", name);
	    if (noreturnval)
		errflag = 1;
	    else
		lastval = 1;
	    goto doneshfunc;
	}
	prog = shf->funcdef;
    }
    runshfunc(prog, wrappers, fstack.name);
 doneshfunc:
    funcstack = fstack.prev;
#ifdef MAX_FUNCTION_DEPTH
 undoshfunc:
    --funcdepth;
#endif
    if (retflag) {
	retflag = 0;
	breaks = obreaks;
    }
    freearray(pparams);
    if (oargv0) {
	zsfree(argzero);
	argzero = oargv0;
    }
    pparams = tab;
    optcind = oldoptcind;
    zoptind = oldzoptind;
    scriptname = oldscriptname;

    if (isset(LOCALOPTIONS)) {
	/* restore all shell options except PRIVILEGED and RESTRICTED */
	saveopts[PRIVILEGED] = opts[PRIVILEGED];
	saveopts[RESTRICTED] = opts[RESTRICTED];
	memcpy(opts, saveopts, sizeof(opts));
	emulation = saveemulation;
    } else {
	/* just restore a couple. */
	opts[XTRACE] = saveopts[XTRACE];
	opts[PRINTEXITVALUE] = saveopts[PRINTEXITVALUE];
	opts[LOCALOPTIONS] = saveopts[LOCALOPTIONS];
    }

    endtrapscope();

    if (trap_state == TRAP_STATE_PRIMED)
	trap_return++;
    ret = lastval;
    if (noreturnval) {
	lastval = oldlastval;
	numpipestats = oldnumpipestats;
	memcpy(pipestats, oldpipestats, sizeof(int)*numpipestats);
    }
    popheap();

    if (exit_pending) {
	if (locallevel > forklevel) {
	    /* Still functions to return: force them to do so. */
	    retflag = 1;
	    breaks = loops;
	} else {
	    /*
	     * All functions finished: time to exit the shell.
	     * We already did the `stopmsg' test when the
	     * exit command was handled.
	     */
	    stopmsg = 1;
	    zexit(exit_pending >> 1, 0);
	}
    }

    return ret;
}

/* This finally executes a shell function and any function wrappers     *
 * defined by modules. This works by calling the wrapper function which *
 * in turn has to call back this function with the arguments it gets.   */

/**/
mod_export void
runshfunc(Eprog prog, FuncWrap wrap, char *name)
{
    int cont;
    VARARR(char, ou, underscoreused);

    memcpy(ou, underscore, underscoreused);

    while (wrap) {
	wrap->module->wrapper++;
	cont = wrap->handler(prog, wrap->next, name);
	wrap->module->wrapper--;

	if (!wrap->module->wrapper &&
	    (wrap->module->node.flags & MOD_UNLOAD))
	    unload_module(wrap->module);

	if (!cont)
	    return;
	wrap = wrap->next;
    }
    startparamscope();
    execode(prog, 1, 0);
    setunderscore(ou);
    endparamscope();
}

/* Search fpath for an undefined function.  Finds the file, and returns the *
 * list of its contents.                                                    */

/**/
Eprog
getfpfunc(char *s, int *ksh, char **fname)
{
    char **pp, buf[PATH_MAX];
    off_t len;
    off_t rlen;
    char *d;
    Eprog r;
    int fd;

    pp = fpath;
    for (; *pp; pp++) {
	if (strlen(*pp) + strlen(s) + 1 >= PATH_MAX)
	    continue;
	if (**pp)
	    sprintf(buf, "%s/%s", *pp, s);
	else
	    strcpy(buf, s);
	if ((r = try_dump_file(*pp, s, buf, ksh))) {
	    if (fname)
		*fname = ztrdup(buf);
	    return r;
	}
	unmetafy(buf, NULL);
	if (!access(buf, R_OK) && (fd = open(buf, O_RDONLY | O_NOCTTY)) != -1) {
	    if ((len = lseek(fd, 0, 2)) != -1) {
		d = (char *) zalloc(len + 1);
		lseek(fd, 0, 0);
		if ((rlen = read(fd, d, len)) >= 0) {
		    char *oldscriptname = scriptname;

		    close(fd);
		    d[rlen] = '\0';
		    d = metafy(d, rlen, META_REALLOC);

		    scriptname = dupstring(s);
		    r = parse_string(d, 1);
		    scriptname = oldscriptname;

		    if (fname)
			*fname = ztrdup(buf);

		    zfree(d, len + 1);

		    return r;
		} else
		    close(fd);

		zfree(d, len + 1);
	    } else
		close(fd);
	}
    }
    return &dummy_eprog;
}

/* Handle the most common type of ksh-style autoloading, when doing a      *
 * zsh-style autoload.  Given the list read from an autoload file, and the *
 * name of the function being defined, check to see if the file consists   *
 * entirely of a single definition for that function.  If so, use the      *
 * contents of that definition.  Otherwise, use the entire file.           */

/**/
Eprog
stripkshdef(Eprog prog, char *name)
{
    Wordcode pc;
    wordcode code;

    if (!prog)
	return NULL;
    pc = prog->prog;
    code = *pc++;
    if (wc_code(code) != WC_LIST ||
	(WC_LIST_TYPE(code) & (Z_SYNC|Z_END|Z_SIMPLE)) != (Z_SYNC|Z_END|Z_SIMPLE))
	return prog;
    pc++;
    code = *pc++;
    if (wc_code(code) != WC_FUNCDEF ||
	*pc != 1 || strcmp(name, ecrawstr(prog, pc + 1, NULL)))
	return prog;

    {
	Eprog ret;
	Wordcode end = pc + WC_FUNCDEF_SKIP(code);
	int sbeg = pc[2], nstrs = pc[3], nprg, npats = pc[4], plen, len, i;
	Patprog *pp;

	pc += 5;

	nprg = end - pc;
	plen = nprg * sizeof(wordcode);
	len = plen + (npats * sizeof(Patprog)) + nstrs;

	if (prog->flags & EF_MAP) {
	    ret = prog;
	    free(prog->pats);
	    ret->pats = pp = (Patprog *) zalloc(npats * sizeof(Patprog));
	    ret->prog = pc;
	    ret->strs = prog->strs + sbeg;
	} else {
	    ret = (Eprog) zhalloc(sizeof(*ret));
	    ret->flags = EF_HEAP;
	    ret->pats = pp = (Patprog *) zhalloc(len);
	    ret->prog = (Wordcode) (ret->pats + npats);
	    ret->strs = (char *) (ret->prog + nprg);
	    memcpy(ret->prog, pc, plen);
	    memcpy(ret->strs, prog->strs + sbeg, nstrs);
	    ret->dump = NULL;
	}
	ret->len = len;
	ret->npats = npats;
	for (i = npats; i--; pp++)
	    *pp = dummy_patprog1;
	ret->shf = NULL;

	return ret;
    }
}

/* check to see if AUTOCD applies here */

/**/
static char *
cancd(char *s)
{
    int nocdpath = s[0] == '.' &&
    (s[1] == '/' || !s[1] || (s[1] == '.' && (s[2] == '/' || !s[1])));
    char *t;

    if (*s != '/') {
	char sbuf[PATH_MAX], **cp;

	if (cancd2(s))
	    return s;
	if (access(unmeta(s), X_OK) == 0)
	    return NULL;
	if (!nocdpath)
	    for (cp = cdpath; *cp; cp++) {
		if (strlen(*cp) + strlen(s) + 1 >= PATH_MAX)
		    continue;
		if (**cp)
		    sprintf(sbuf, "%s/%s", *cp, s);
		else
		    strcpy(sbuf, s);
		if (cancd2(sbuf)) {
		    doprintdir = -1;
		    return dupstring(sbuf);
		}
	    }
	if ((t = cd_able_vars(s))) {
	    if (cancd2(t)) {
		doprintdir = -1;
		return t;
	    }
	}
	return NULL;
    }
    return cancd2(s) ? s : NULL;
}

/**/
static int
cancd2(char *s)
{
    struct stat buf;
    char *us, *us2 = NULL;
    int ret;

    /*
     * If CHASEDOTS and CHASELINKS are not set, we want to rationalize the
     * path by removing foo/.. combinations in the logical rather than
     * the physical path.  If either is set, we test the physical path.
     */
    if (!isset(CHASEDOTS) && !isset(CHASELINKS)) {
	if (*s != '/')
	    us = tricat(pwd[1] ? pwd : "", "/", s);
	else
	    us = ztrdup(s);
	fixdir(us2 = us);
    } else
	us = unmeta(s);
    ret = !(access(us, X_OK) || stat(us, &buf) || !S_ISDIR(buf.st_mode));
    if (us2)
	free(us2);
    return ret;
}

/**/
void
execsave(void)
{
    struct execstack *es;

    es = (struct execstack *) malloc(sizeof(struct execstack));
    es->list_pipe_pid = list_pipe_pid;
    es->nowait = nowait;
    es->pline_level = pline_level;
    es->list_pipe_child = list_pipe_child;
    es->list_pipe_job = list_pipe_job;
    strcpy(es->list_pipe_text, list_pipe_text);
    es->lastval = lastval;
    es->noeval = noeval;
    es->badcshglob = badcshglob;
    es->cmdoutpid = cmdoutpid;
    es->cmdoutval = cmdoutval;
    es->trap_return = trap_return;
    es->trap_state = trap_state;
    es->trapisfunc = trapisfunc;
    es->traplocallevel = traplocallevel;
    es->noerrs = noerrs;
    es->subsh_close = subsh_close;
    es->underscore = ztrdup(underscore);
    es->next = exstack;
    exstack = es;
    noerrs = cmdoutpid = 0;
}

/**/
void
execrestore(void)
{
    struct execstack *en;

    DPUTS(!exstack, "BUG: execrestore() without execsave()");
    list_pipe_pid = exstack->list_pipe_pid;
    nowait = exstack->nowait;
    pline_level = exstack->pline_level;
    list_pipe_child = exstack->list_pipe_child;
    list_pipe_job = exstack->list_pipe_job;
    strcpy(list_pipe_text, exstack->list_pipe_text);
    lastval = exstack->lastval;
    noeval = exstack->noeval;
    badcshglob = exstack->badcshglob;
    cmdoutpid = exstack->cmdoutpid;
    cmdoutval = exstack->cmdoutval;
    trap_return = exstack->trap_return;
    trap_state = exstack->trap_state;
    trapisfunc = exstack->trapisfunc;
    traplocallevel = exstack->traplocallevel;
    noerrs = exstack->noerrs;
    subsh_close = exstack->subsh_close;
    setunderscore(exstack->underscore);
    zsfree(exstack->underscore);
    en = exstack->next;
    free(exstack);
    exstack = en;
}
