/*
 * signals.c - signals handling code
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
#include "signals.pro"
 
/* Array describing the state of each signal: an element contains *
 * 0 for the default action or some ZSIG_* flags ored together.   */

/**/
mod_export int sigtrapped[VSIGCOUNT];

/* trap functions for each signal */

/**/
mod_export Eprog sigfuncs[VSIGCOUNT];

/* Total count of trapped signals */

/**/
mod_export int nsigtrapped;

/* Variables used by signal queueing */

/**/
mod_export int queueing_enabled, queue_front, queue_rear;
/**/
mod_export int signal_queue[MAX_QUEUE_SIZE];
/**/
mod_export sigset_t signal_mask_queue[MAX_QUEUE_SIZE];

/* This is only used on machines that don't understand signal sets.  *
 * On SYSV machines this will represent the signals that are blocked *
 * (held) using sighold.  On machines which can't block signals at   *
 * all, we will simulate this by ignoring them and remembering them  *
 * in this variable.                                                 */
#if !defined(POSIX_SIGNALS) && !defined(BSD_SIGNALS)
static sigset_t blocked_set;
#endif

#ifdef POSIX_SIGNALS
# define signal_jmp_buf       sigjmp_buf
# define signal_setjmp(b)     sigsetjmp((b),1)
# define signal_longjmp(b,n)  siglongjmp((b),(n))
#else
# define signal_jmp_buf       jmp_buf
# define signal_setjmp(b)     setjmp(b)
# define signal_longjmp(b,n)  longjmp((b),(n))
#endif
 
#ifdef NO_SIGNAL_BLOCKING
# define signal_process(sig)  signal_ignore(sig)
# define signal_reset(sig)    install_handler(sig)
#else
# define signal_process(sig)  ;
# define signal_reset(sig)    ;
#endif

/* Install signal handler for given signal.           *
 * If possible, we want to make sure that interrupted *
 * system calls are not restarted.                    */

/**/
mod_export void
install_handler(int sig)
{
#ifdef POSIX_SIGNALS
    struct sigaction act;
 
    act.sa_handler = (SIGNAL_HANDTYPE) zhandler;
    sigemptyset(&act.sa_mask);        /* only block sig while in handler */
    act.sa_flags = 0;
# ifdef SA_INTERRUPT                  /* SunOS 4.x */
    if (interact)
        act.sa_flags |= SA_INTERRUPT; /* make sure system calls are not restarted */
# endif
    sigaction(sig, &act, (struct sigaction *)NULL);
#else
# ifdef BSD_SIGNALS
    struct sigvec vec;
 
    vec.sv_handler = (SIGNAL_HANDTYPE) zhandler;
    vec.sv_mask = sigmask(sig);    /* mask out this signal while in handler    */
#  ifdef SV_INTERRUPT
    vec.sv_flags = SV_INTERRUPT;   /* make sure system calls are not restarted */
#  endif
    sigvec(sig, &vec, (struct sigvec *)NULL);
# else
#  ifdef SYSV_SIGNALS
    /* we want sigset rather than signal because it will   *
     * block sig while in handler.  signal usually doesn't */
    sigset(sig, zhandler);
#  else  /* NO_SIGNAL_BLOCKING (bummer) */
    signal(sig, zhandler);

#  endif /* SYSV_SIGNALS  */
# endif  /* BSD_SIGNALS   */
#endif   /* POSIX_SIGNALS */
}

/* enable ^C interrupts */
 
/**/
mod_export void
intr(void)
{
    if (interact)
        install_handler(SIGINT);
}

/* disable ^C interrupts */
 
#if 0 /**/
void
nointr(void)
{
    if (interact)
        signal_ignore(SIGINT);
}
#endif
 
/* temporarily block ^C interrupts */
 
/**/
mod_export void
holdintr(void)
{
    if (interact)
        signal_block(signal_mask(SIGINT));
}

/* release ^C interrupts */
 
/**/
mod_export void
noholdintr(void)
{
    if (interact)
        signal_unblock(signal_mask(SIGINT));
}
 
/* create a signal mask containing *
 * only the given signal           */
 
/**/
sigset_t
signal_mask(int sig)
{
    sigset_t set;
 
    sigemptyset(&set);
    if (sig)
        sigaddset(&set, sig);
    return set;
}

/* Block the signals in the given signal *
 * set. Return the old signal set.       */

/**/
#ifdef POSIX_SIGNALS

/**/
mod_export sigset_t dummy_sigset1, dummy_sigset2;

/**/
#else

/**/
#ifndef BSD_SIGNALS

sigset_t
signal_block(sigset_t set)
{
    sigset_t oset;
 
#ifdef SYSV_SIGNALS
    int i;
 
    oset = blocked_set;
    for (i = 1; i <= NSIG; ++i) {
        if (sigismember(&set, i) && !sigismember(&blocked_set, i)) {
            sigaddset(&blocked_set, i);
            sighold(i);
        }
    }
#else  /* NO_SIGNAL_BLOCKING */
/* We will just ignore signals if the system doesn't have *
 * the ability to block them.                             */
    int i;

    oset = blocked_set;
    for (i = 1; i <= NSIG; ++i) {
        if (sigismember(&set, i) && !sigismember(&blocked_set, i)) {
            sigaddset(&blocked_set, i);
            signal_ignore(i);
        }
   }
#endif /* SYSV_SIGNALS  */
 
    return oset;
}

/**/
#endif /* BSD_SIGNALS */

/**/
#endif /* POSIX_SIGNALS */

/* Unblock the signals in the given signal *
 * set. Return the old signal set.         */

#ifndef POSIX_SIGNALS

sigset_t
signal_unblock(sigset_t set)
{
    sigset_t oset;
 
# ifdef BSD_SIGNALS
    sigfillset(&oset);
    oset = sigsetmask(oset);
    sigsetmask(oset & ~set);
# else
#  ifdef SYSV_SIGNALS
    int i;
 
    oset = blocked_set;
    for (i = 1; i <= NSIG; ++i) {
        if (sigismember(&set, i) && sigismember(&blocked_set, i)) {
            sigdelset(&blocked_set, i);
            sigrelse(i);
        }
    }
#  else  /* NO_SIGNAL_BLOCKING */
/* On systems that can't block signals, we are just ignoring them.  So *
 * to unblock signals, we just reenable the signal handler for them.   */
    int i;

    oset = blocked_set;
    for (i = 1; i <= NSIG; ++i) {
        if (sigismember(&set, i) && sigismember(&blocked_set, i)) {
            sigdelset(&blocked_set, i);
            install_handler(i);
        }
   }
#  endif /* SYSV_SIGNALS  */
# endif  /* BSD_SIGNALS   */
 
    return oset;
}

#endif   /* POSIX_SIGNALS */

/* set the process signal mask to *
 * be the given signal mask       */

/**/
mod_export sigset_t
signal_setmask(sigset_t set)
{
    sigset_t oset;
 
#ifdef POSIX_SIGNALS
    sigprocmask(SIG_SETMASK, &set, &oset);
#else
# ifdef BSD_SIGNALS
    oset = sigsetmask(set);
# else
#  ifdef SYSV_SIGNALS
    int i;
 
    oset = blocked_set;
    for (i = 1; i <= NSIG; ++i) {
        if (sigismember(&set, i) && !sigismember(&blocked_set, i)) {
            sigaddset(&blocked_set, i);
            sighold(i);
        } else if (!sigismember(&set, i) && sigismember(&blocked_set, i)) {
            sigdelset(&blocked_set, i);
            sigrelse(i);
        }
    }
#  else  /* NO_SIGNAL_BLOCKING */
    int i;

    oset = blocked_set;
    for (i = 1; i < NSIG; ++i) {
        if (sigismember(&set, i) && !sigismember(&blocked_set, i)) {
            sigaddset(&blocked_set, i);
            signal_ignore(i);
        } else if (!sigismember(&set, i) && sigismember(&blocked_set, i)) {
            sigdelset(&blocked_set, i);
            install_handler(i);
        }
    }
#  endif /* SYSV_SIGNALS  */
# endif  /* BSD_SIGNALS   */
#endif   /* POSIX_SIGNALS */
 
    return oset;
}

#if defined(NO_SIGNAL_BLOCKING)
static int suspend_longjmp = 0;
static signal_jmp_buf suspend_jmp_buf;
#endif
 
/**/
int
signal_suspend(int sig, int sig2)
{
    int ret;
 
#ifdef POSIX_SIGNALS
    sigset_t set;
#ifdef BROKEN_POSIX_SIGSUSPEND
    sigset_t oset;
#endif /* BROKEN_POSIX_SIGSUSPEND */

    sigfillset(&set);
    sigdelset(&set, sig);
    sigdelset(&set, SIGHUP);  /* still don't know why we add this? */
    if (sig2)
        sigdelset(&set, sig2);
#ifdef BROKEN_POSIX_SIGSUSPEND
    sigprocmask(SIG_SETMASK, &set, &oset);
    pause();
    sigprocmask(SIG_SETMASK, &oset, NULL);
#else /* not BROKEN_POSIX_SIGSUSPEND */
    ret = sigsuspend(&set);
#endif /* BROKEN_POSIX_SIGSUSPEND */
#else /* not POSIX_SIGNALS */
# ifdef BSD_SIGNALS
    sigset_t set;

    sigfillset(&set);
    sigdelset(&set, sig);
    if (sig2)
      sigdelset(&set, sig2);
    ret = sigpause(set);
# else
#  ifdef SYSV_SIGNALS
    ret = sigpause(sig);

#  else  /* NO_SIGNAL_BLOCKING */
    /* need to use signal_longjmp to make this race-free *
     * between the child_unblock() and pause()           */
    if (signal_setjmp(suspend_jmp_buf) == 0) {
        suspend_longjmp = 1;   /* we want to signal_longjmp after catching signal */
        child_unblock();       /* do we need to unblock sig2 as well?             */
        ret = pause();
    }
    suspend_longjmp = 0;       /* turn off using signal_longjmp since we are past *
                                * the pause() function.                           */
#  endif /* SYSV_SIGNALS  */
# endif  /* BSD_SIGNALS   */
#endif   /* POSIX_SIGNALS */
 
    return ret;
}

/* What flavor of waitpid/wait3/wait shall we use? */
 
#ifdef HAVE_WAITPID
# define  WAIT(pid, statusp, options) waitpid(pid, statusp, options)
#else
# ifdef HAVE_WAIT3
#  define WAIT(pid, statusp, options) wait3((void *) statusp, options, NULL)
# else
#  define WAIT(pid, statusp, options) wait(statusp)
# endif
#endif

/* the signal handler */
 
/**/
mod_export RETSIGTYPE
zhandler(int sig)
{
    sigset_t newmask, oldmask;

#if defined(NO_SIGNAL_BLOCKING)
    int do_jump;
    signal_jmp_buf jump_to;
#endif
 
    signal_process(sig);
 
    sigfillset(&newmask);
    oldmask = signal_block(newmask);        /* Block all signals temporarily           */
 
#if defined(NO_SIGNAL_BLOCKING)
    do_jump = suspend_longjmp;              /* do we need to longjmp to signal_suspend */
    suspend_longjmp = 0;                    /* In case a SIGCHLD somehow arrives       */

    if (sig == SIGCHLD) {                   /* Traps can cause nested child_suspend()  */
        if (do_jump)
            jump_to = suspend_jmp_buf;      /* Copy suspend_jmp_buf                    */
    }
#endif

    if (queueing_enabled) {           /* Are we queueing signals now?      */
        int temp_rear = ++queue_rear % MAX_QUEUE_SIZE;

	DPUTS(temp_rear == queue_front, "BUG: signal queue full");
        if (temp_rear != queue_front) { /* Make sure it's not full (extremely unlikely) */
            queue_rear = temp_rear;                  /* ok, not full, so add to queue   */
            signal_queue[queue_rear] = sig;          /* save signal caught              */
            signal_mask_queue[queue_rear] = oldmask; /* save current signal mask        */
        }
        signal_reset(sig);
        return;
    }
 
    signal_setmask(oldmask);          /* Reset signal mask, signal traps ok now */
 
    switch (sig) {
    case SIGCHLD:

	/* keep WAITING until no more child processes to reap */
	for (;;)
	  cont: {
            int old_errno = errno; /* save the errno, since WAIT may change it */
	    int status;
	    Job jn;
	    Process pn;
            pid_t pid;
	    pid_t *procsubpid = &cmdoutpid;
	    int *procsubval = &cmdoutval;
	    struct execstack *es = exstack;

            pid = WAIT(-1, &status, WNOHANG|WUNTRACED);  /* reap the child process */

            if (!pid)  /* no more children to reap */
                break;

	    /* check if child returned was from process substitution */
	    for (;;) {
		if (pid == *procsubpid) {
		    *procsubpid = 0;
		    if (WIFSIGNALED(status))
			*procsubval = (0200 | WTERMSIG(status));
		    else
			*procsubval = WEXITSTATUS(status);
		    times(&shtms);
		    goto cont;
		}
		if (!es)
		    break;
		procsubpid = &es->cmdoutpid;
		procsubval = &es->cmdoutval;
		es = es->next;
	    }

	    /* check for WAIT error */
            if (pid == -1) {
                if (errno != ECHILD)
                    zerr("wait failed: %e", NULL, errno);
                errno = old_errno;    /* WAIT changed errno, so restore the original */
                break;
            }

	    /* Find the process and job containing this pid and update it. */
	    if (findproc(pid, &jn, &pn)) {
		update_process(pn, status);
		update_job(jn);
	    } else {
		/* If not found, update the shell record of time spent by
		 * children in sub processes anyway:  otherwise, this
		 * will get added on to the next found process that terminates.
		 */
		times(&shtms);
	    }
        }
        break;
 
    case SIGHUP:
        if (sigtrapped[SIGHUP])
            dotrap(SIGHUP);
        else {
            stopmsg = 1;
            zexit(SIGHUP, 1);
        }
        break;
 
    case SIGINT:
        if (sigtrapped[SIGINT])
            dotrap(SIGINT);
        else {
	    if ((isset(PRIVILEGED) || isset(RESTRICTED)) &&
		isset(INTERACTIVE) && noerrexit < 0)
		zexit(SIGINT, 1);
            if (list_pipe || chline || simple_pline) {
                breaks = loops;
                errflag = 1;
		inerrflush();
            }
        }
        break;

#ifdef SIGWINCH
    case SIGWINCH:
        adjustwinsize(1);  /* check window size and adjust */
	if (sigtrapped[SIGWINCH])
	    dotrap(SIGWINCH);
        break;
#endif

    case SIGALRM:
        if (sigtrapped[SIGALRM]) {
	    int tmout;
            dotrap(SIGALRM);

	    if ((tmout = getiparam("TMOUT")))
		alarm(tmout);           /* reset the alarm */
        } else {
	    int idle = ttyidlegetfn(NULL);
	    int tmout = getiparam("TMOUT");
	    if (idle >= 0 && idle < tmout)
		alarm(tmout - idle);
	    else {
		errflag = noerrs = 0;
		zwarn("timeout", NULL, 0);
		stopmsg = 1;
		zexit(SIGALRM, 1);
	    }
        }
        break;
 
    default:
        dotrap(sig);
        break;
    }   /* end of switch(sig) */
 
    signal_reset(sig);

/* This is used to make signal_suspend() race-free */
#if defined(NO_SIGNAL_BLOCKING)
    if (do_jump)
        signal_longjmp(jump_to, 1);
#endif

} /* handler */

 
/* SIGHUP any jobs left running */
 
/**/
void
killrunjobs(int from_signal)
{
    int i, killed = 0;
 
    if (unset(HUP))
        return;
    for (i = 1; i < MAXJOB; i++)
        if ((from_signal || i != thisjob) && (jobtab[i].stat & STAT_LOCKED) &&
            !(jobtab[i].stat & STAT_NOPRINT) &&
            !(jobtab[i].stat & STAT_STOPPED)) {
            if (jobtab[i].gleader != getpid() &&
		killpg(jobtab[i].gleader, SIGHUP) != -1)
                killed++;
        }
    if (killed)
        zwarn("warning: %d jobs SIGHUPed", NULL, killed);
}


/* send a signal to a job (simply involves kill if monitoring is on) */
 
/**/
int
killjb(Job jn, int sig)
{
    Process pn;
    int err = 0;
 
    if (jobbing) {
        if (jn->stat & STAT_SUPERJOB) {
            if (sig == SIGCONT) {
                for (pn = jobtab[jn->other].procs; pn; pn = pn->next)
                    if (killpg(pn->pid, sig) == -1)
			if (kill(pn->pid, sig) == -1 && errno != ESRCH)
			    err = -1;
 
                for (pn = jn->procs; pn->next; pn = pn->next)
                    if (kill(pn->pid, sig) == -1 && errno != ESRCH)
			err = -1;

		if (!jobtab[jn->other].procs && pn)
		    if (kill(pn->pid, sig) == -1 && errno != ESRCH)
			err = -1;

                return err;
            }
            if (killpg(jobtab[jn->other].gleader, sig) == -1 && errno != ESRCH)
		err = -1;
		
	    if (killpg(jn->gleader, sig) == -1 && errno != ESRCH)
		err = -1;

	    return err;
        }
        else
	    return killpg(jn->gleader, sig);
    }
    for (pn = jn->procs; pn; pn = pn->next)
        if ((err = kill(pn->pid, sig)) == -1 && errno != ESRCH && sig != 0)
            return -1;
    return err;
}

/*
 * List for saving traps.  We don't usually have that many traps
 * at once, so just use a linked list.
 */
struct savetrap {
    int sig, flags, local;
    void *list;
};

static LinkList savetraps;
static int dontsavetrap;

/*
 * Save the current trap by copying it.  This does nothing to
 * the existing value of sigtrapped or sigfuncs.
 */

static void
dosavetrap(int sig, int level)
{
    struct savetrap *st;
    st = (struct savetrap *)zalloc(sizeof(*st));
    st->sig = sig;
    st->local = level;
    if ((st->flags = sigtrapped[sig]) & ZSIG_FUNC) {
	/*
	 * Get the old function: this assumes we haven't added
	 * the new one yet.
	 */
	char func[20];
	Shfunc shf, newshf = NULL;
	sprintf(func, "TRAP%s", sigs[sig]);
	if ((shf = (Shfunc)shfunctab->getnode2(shfunctab, func))) {
	    /* Copy the node for saving */
	    newshf = (Shfunc) zalloc(sizeof(*newshf));
	    newshf->nam = ztrdup(shf->nam);
	    newshf->flags = shf->flags;
	    newshf->funcdef = dupeprog(shf->funcdef, 0);
	}
#ifdef DEBUG
	else dputs("BUG: no function present with function trap flag set.");
#endif
	st->list = newshf;
    } else if (sigtrapped[sig]) {
	st->list = sigfuncs[sig] ? dupeprog(sigfuncs[sig], 0) : NULL;
    } else {
	DPUTS(sigfuncs[sig], "BUG: sigfuncs not null for untrapped signal");
	st->list = NULL;
    }
    if (!savetraps)
	savetraps = znewlinklist();
    /*
     * Put this at the front of the list
     */
    zinsertlinknode(savetraps, (LinkNode)savetraps, st);
}

/**/
mod_export int
settrap(int sig, Eprog l)
{
    if (sig == -1)
        return 1;
    if (jobbing && (sig == SIGTTOU || sig == SIGTSTP || sig == SIGTTIN)) {
        zerr("can't trap SIG%s in interactive shells", sigs[sig], 0);
        return 1;
    }

    /*
     * Call unsettrap() unconditionally, to make sure trap is saved
     * if necessary.
     */
    queue_signals();
    unsettrap(sig);

    sigfuncs[sig] = l;
    if (empty_eprog(l)) {
	sigtrapped[sig] = ZSIG_IGNORED;
        if (sig && sig <= SIGCOUNT &&
#ifdef SIGWINCH
            sig != SIGWINCH &&
#endif
            sig != SIGCHLD)
            signal_ignore(sig);
    } else {
	nsigtrapped++;
        sigtrapped[sig] = ZSIG_TRAPPED;
        if (sig && sig <= SIGCOUNT &&
#ifdef SIGWINCH
            sig != SIGWINCH &&
#endif
            sig != SIGCHLD)
            install_handler(sig);
    }
    /*
     * Note that introducing the locallevel does not affect whether
     * sigtrapped[sig] is zero or not, i.e. a test without a mask
     * works just the same.
     */
    sigtrapped[sig] |= (locallevel << ZSIG_SHIFT);
    unqueue_signals();
    return 0;
}

/**/
void
unsettrap(int sig)
{
    HashNode hn;

    queue_signals();
    hn = removetrap(sig);
    if (hn)
	shfunctab->freenode(hn);
    unqueue_signals();
}

/**/
HashNode
removetrap(int sig)
{
    int trapped;

    if (sig == -1 ||
	(jobbing && (sig == SIGTTOU || sig == SIGTSTP || sig == SIGTTIN)))
	return NULL;

    queue_signals();
    trapped = sigtrapped[sig];
    /*
     * Note that we save the trap here even if there isn't an existing
     * one, to aid in removing this one.  However, if there's
     * already one at the current locallevel we just overwrite it.
     */
    if (!dontsavetrap && (isset(LOCALTRAPS) || sig == SIGEXIT) &&
	locallevel &&
	(!trapped || locallevel > (sigtrapped[sig] >> ZSIG_SHIFT)))
	dosavetrap(sig, locallevel);

    if (!trapped) {
	unqueue_signals();
        return NULL;
    }
    if (sigtrapped[sig] & ZSIG_TRAPPED)
	nsigtrapped--;
    sigtrapped[sig] = 0;
    if (sig == SIGINT && interact) {
	/* PWS 1995/05/16:  added test for interactive, also noholdintr() *
	 * as subshells ignoring SIGINT have it blocked from delivery     */
        intr();
	noholdintr();
    } else if (sig == SIGHUP)
        install_handler(sig);
    else if (sig && sig <= SIGCOUNT &&
#ifdef SIGWINCH
             sig != SIGWINCH &&
#endif
             sig != SIGCHLD)
        signal_default(sig);

    /*
     * At this point we free the appropriate structs.  If we don't
     * want that to happen then either the function should already have been
     * removed from shfunctab, or the entry in sigfuncs should have been set
     * to NULL.  This is no longer necessary for saving traps as that
     * copies the structures, so here we are remove the originals.
     * That causes a little inefficiency, but a good deal more reliability.
     */
    if (trapped & ZSIG_FUNC) {
	char func[20];
	HashNode node;

	sprintf(func, "TRAP%s", sigs[sig]);
	/*
	 * As in dosavetrap(), don't call removeshfuncnode() because
	 * that calls back into unsettrap();
	 */
	sigfuncs[sig] = NULL;
	node = removehashnode(shfunctab, func);
	unqueue_signals();

	return node;
    } else if (sigfuncs[sig]) {
	freeeprog(sigfuncs[sig]);
	sigfuncs[sig] = NULL;
    }
    unqueue_signals();

    return NULL;
}

/**/
void
starttrapscope(void)
{
    /*
     * SIGEXIT needs to be restored at the current locallevel,
     * so give it the next higher one. dosavetrap() is called
     * automatically where necessary.
     */
    if (sigtrapped[SIGEXIT]) {
	locallevel++;
	unsettrap(SIGEXIT);
	locallevel--;
    }
}

/*
 * Reset traps after the end of a function: must be called after
 * endparamscope() so that the locallevel has been decremented.
 */

/**/
void
endtrapscope(void)
{
    LinkNode ln;
    struct savetrap *st;
    int exittr;
    void *exitfn = NULL;

    /*
     * Remember the exit trap, but don't run it until
     * after all the other traps have been put back.
     */
    if ((exittr = sigtrapped[SIGEXIT])) {
	if (exittr & ZSIG_FUNC) {
	    exitfn = removehashnode(shfunctab, "TRAPEXIT");
	} else {
	    exitfn = sigfuncs[SIGEXIT];
	}
	sigfuncs[SIGEXIT] = NULL;
	if (sigtrapped[SIGEXIT] & ZSIG_TRAPPED)
	    nsigtrapped--;
	sigtrapped[SIGEXIT] = 0;
    }

    if (savetraps) {
	while ((ln = firstnode(savetraps)) &&
	       (st = (struct savetrap *) ln->dat) &&
	       st->local > locallevel) {
	    int sig = st->sig;

	    remnode(savetraps, ln);

	    if (st->flags && (st->list != NULL)) {
		Eprog prog = (st->flags & ZSIG_FUNC) ?
		    ((Shfunc) st->list)->funcdef : (Eprog) st->list;
		/* prevent settrap from saving this */
		dontsavetrap++;
		settrap(sig, prog);
		dontsavetrap--;
		/*
		 * counting of nsigtrapped should presumably be handled
		 * in settrap...
		 */
		DPUTS((sigtrapped[sig] ^ st->flags) & ZSIG_TRAPPED,
		      "BUG: settrap didn't restore correct ZSIG_TRAPPED");
		if ((sigtrapped[sig] = st->flags) & ZSIG_FUNC)
		    shfunctab->addnode(shfunctab, ((Shfunc)st->list)->nam,
				       (Shfunc) st->list);
	    } else if (sigtrapped[sig])
		unsettrap(sig);

	    zfree(st, sizeof(*st));
	}
    }

    if (exittr) {
	dotrapargs(SIGEXIT, &exittr, (exittr & ZSIG_FUNC) ?
		   ((Shfunc)exitfn)->funcdef : (Eprog) exitfn);
	if (exittr & ZSIG_FUNC)
	    shfunctab->freenode((HashNode)exitfn);
	else
	    freeeprog(exitfn);
    }
    DPUTS(!locallevel && savetraps && firstnode(savetraps),
	  "BUG: still saved traps outside all function scope");
}

/* Execute a trap function for a given signal, possibly
 * with non-standard sigtrapped & sigfuncs values
 */

/**/
void
dotrapargs(int sig, int *sigtr, void *sigfn)
{
    LinkList args;
    char *name, num[4];
    int trapret = 0;
    int obreaks = breaks;
 
    /* if signal is being ignored or the trap function		      *
     * is NULL, then return					      *
     *								      *
     * Also return if errflag is set.  In fact, the code in the       *
     * function will test for this, but this way we keep status flags *
     * intact without working too hard.  Special cases (e.g. calling  *
     * a trap for SIGINT after the error flag was set) are handled    *
     * by the calling code.  (PWS 1995/06/08).			      *
     *                                                                *
     * This test is now replicated in dotrap().                       */
    if ((*sigtr & ZSIG_IGNORED) || !sigfn || errflag)
        return;

    *sigtr |= ZSIG_IGNORED;

    lexsave();
    execsave();
    breaks = 0;
    runhookdef(BEFORETRAPHOOK, NULL);
    if (*sigtr & ZSIG_FUNC) {
	int osc = sfcontext;

	args = znewlinklist();
	name = (char *) zalloc(5 + strlen(sigs[sig]));
	sprintf(name, "TRAP%s", sigs[sig]);
	zaddlinknode(args, name);
	sprintf(num, "%d", sig);
	zaddlinknode(args, num);

	trapreturn = -1;
	sfcontext = SFC_SIGNAL;
	doshfunc(name, sigfn, args, 0, 1);
	sfcontext = osc;
	freelinklist(args, (FreeFunc) NULL);
	zsfree(name);
    } else
	execode(sigfn, 1, 0);
    runhookdef(AFTERTRAPHOOK, NULL);

    if (trapreturn > 0)
	trapret = trapreturn;
    else if (errflag)
	trapret = 1;
    execrestore();
    lexrestore();

    if (trapret > 0) {
	breaks = loops;
	errflag = 1;
    } else {
	breaks += obreaks;
	if (breaks > loops)
	    breaks = loops;
    }

    /*
     * If zle was running while the trap was executed, see if we
     * need to restore the display.
     */
    if (zleactive && resetneeded)
	zrefresh();

    if (*sigtr != ZSIG_IGNORED)
	*sigtr &= ~ZSIG_IGNORED;
}

/* Standard call to execute a trap for a given signal. */

/**/
void
dotrap(int sig)
{
    /* Copied from dotrapargs(). */
    if ((sigtrapped[sig] & ZSIG_IGNORED) || !sigfuncs[sig] || errflag)
	return;

    dotrapargs(sig, sigtrapped+sig, sigfuncs[sig]);
}
