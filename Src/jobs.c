/*
 * jobs.c - job control
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
#include "jobs.pro"

/* the process group of the shell at startup (equal to mypgprp, except
   when we started without being process group leader */

/**/
mod_export pid_t origpgrp;

/* the process group of the shell */

/**/
mod_export pid_t mypgrp;
 
/* the job we are working on */
 
/**/
mod_export int thisjob;

/* the current job (+) */
 
/**/
mod_export int curjob;
 
/* the previous job (-) */
 
/**/
mod_export int prevjob;
 
/* the job table */
 
/**/
mod_export struct job *jobtab;

/* Size of the job table. */

/**/
mod_export int jobtabsize;

/* The highest numbered job in the jobtable */

/**/
mod_export int maxjob;

/* If we have entered a subshell, the original shell's job table. */
static struct job *oldjobtab;

/* The size of that. */
static int oldmaxjob;

/* shell timings */
 
/**/
#ifdef HAVE_GETRUSAGE
/**/
static struct rusage child_usage;
/**/
#else
/**/
static struct tms shtms;
/**/
#endif
 
/* 1 if ttyctl -f has been executed */
 
/**/
int ttyfrozen;

/* Previous values of errflag and breaks if the signal handler had to
 * change them. And a flag saying if it did that. */

/**/
int prev_errflag, prev_breaks, errbrk_saved;

/**/
int numpipestats, pipestats[MAX_PIPESTATS];

/* Diff two timevals for elapsed-time computations */

/**/
static struct timeval *
dtime(struct timeval *dt, struct timeval *t1, struct timeval *t2)
{
    dt->tv_sec = t2->tv_sec - t1->tv_sec;
    dt->tv_usec = t2->tv_usec - t1->tv_usec;
    if (dt->tv_usec < 0) {
	dt->tv_usec += 1000000.0;
	dt->tv_sec -= 1.0;
    }
    return dt;
}

/* change job table entry from stopped to running */

/**/
void
makerunning(Job jn)
{
    Process pn;

    jn->stat &= ~STAT_STOPPED;
    for (pn = jn->procs; pn; pn = pn->next)
#if 0
	if (WIFSTOPPED(pn->status) && 
	    (!(jn->stat & STAT_SUPERJOB) || pn->next))
	    pn->status = SP_RUNNING;
#endif
        if (WIFSTOPPED(pn->status))
	    pn->status = SP_RUNNING;

    if (jn->stat & STAT_SUPERJOB)
	makerunning(jobtab + jn->other);
}

/* Find process and job associated with pid.         *
 * Return 1 if search was successful, else return 0. */

/**/
int
findproc(pid_t pid, Job *jptr, Process *pptr, int aux)
{
    Process pn;
    int i;

    for (i = 1; i <= maxjob; i++)
    {
	for (pn = aux ? jobtab[i].auxprocs : jobtab[i].procs;
	     pn; pn = pn->next)
	    if (pn->pid == pid) {
		*pptr = pn;
		*jptr = jobtab + i;
		return 1;
	    }
    }

    return 0;
}

/* Does the given job number have any processes? */

/**/
int
hasprocs(int job)
{
    Job jn = jobtab + job;

    return jn->procs || jn->auxprocs;
}

/* Find the super-job of a sub-job. */

/**/
static int
super_job(int sub)
{
    int i;

    for (i = 1; i <= maxjob; i++)
	if ((jobtab[i].stat & STAT_SUPERJOB) &&
	    jobtab[i].other == sub &&
	    jobtab[i].gleader)
	    return i;
    return 0;
}

/**/
static int
handle_sub(int job, int fg)
{
    Job jn = jobtab + job, sj = jobtab + jn->other;

    if ((sj->stat & STAT_DONE) || (!sj->procs && !sj->auxprocs)) {
	struct process *p;
		    
	for (p = sj->procs; p; p = p->next)
	    if (WIFSIGNALED(p->status)) {
		if (jn->gleader != mypgrp && jn->procs->next)
		    killpg(jn->gleader, WTERMSIG(p->status));
		else
		    kill(jn->procs->pid, WTERMSIG(p->status));
		kill(sj->other, SIGCONT);
		kill(sj->other, WTERMSIG(p->status));
		break;
	    }
	if (!p) {
	    int cp;

	    jn->stat &= ~STAT_SUPERJOB;
	    jn->stat |= STAT_WASSUPER;

	    if ((cp = ((WIFEXITED(jn->procs->status) ||
			WIFSIGNALED(jn->procs->status)) &&
		       killpg(jn->gleader, 0) == -1))) {
		Process p;
		for (p = jn->procs; p->next; p = p->next);
		jn->gleader = p->pid;
	    }
	    /* This deleted the job too early if the parent
	       shell waited for a command in a list that will
	       be executed by the sub-shell (e.g.: if we have
	       `ls|if true;then sleep 20;cat;fi' and ^Z the
	       sleep, the rest will be executed by a sub-shell,
	       but the parent shell gets notified for the
	       sleep.
	       deletejob(sj); */
	    /* If this super-job contains only the sub-shell,
	       we have to attach the tty to its process group
	       now. */
	    if ((fg || thisjob == job) &&
		(!jn->procs->next || cp || jn->procs->pid != jn->gleader))
		attachtty(jn->gleader);
	    kill(sj->other, SIGCONT);
	}
	curjob = jn - jobtab;
    } else if (sj->stat & STAT_STOPPED) {
	struct process *p;

	jn->stat |= STAT_STOPPED;
	for (p = jn->procs; p; p = p->next)
	    if (p->status == SP_RUNNING ||
		(!WIFEXITED(p->status) && !WIFSIGNALED(p->status)))
		p->status = sj->procs->status;
	curjob = jn - jobtab;
	printjob(jn, !!isset(LONGLISTJOBS), 1);
	return 1;
    }
    return 0;
}


/* Get the latest usage information */

/**/
void 
get_usage(void)
{
#ifdef HAVE_GETRUSAGE
    getrusage(RUSAGE_CHILDREN, &child_usage);
#else
    times(&shtms);
#endif
}


#if !defined HAVE_WAIT3 || !defined HAVE_GETRUSAGE
/* Update status of process that we have just WAIT'ed for */

/**/
void
update_process(Process pn, int status)
{
    struct timezone dummy_tz;
#ifdef HAVE_GETRUSAGE
    struct timeval childs = child_usage.ru_stime;
    struct timeval childu = child_usage.ru_utime;
#else
    long childs = shtms.tms_cstime;
    long childu = shtms.tms_cutime;
#endif

    /* get time-accounting info          */
    get_usage();
    gettimeofday(&pn->endtime, &dummy_tz);  /* record time process exited        */

    pn->status = status;                    /* save the status returned by WAIT  */
#ifdef HAVE_GETRUSAGE
    dtime(&pn->ti.ru_stime, &childs, &child_usage.ru_stime);
    dtime(&pn->ti.ru_utime, &childu, &child_usage.ru_utime);
#else
    pn->ti.st  = shtms.tms_cstime - childs; /* compute process system space time */
    pn->ti.ut  = shtms.tms_cutime - childu; /* compute process user space time   */
#endif
}
#endif

/* Update status of job, possibly printing it */

/**/
void
update_job(Job jn)
{
    Process pn;
    int job;
    int val = 0, status = 0;
    int somestopped = 0, inforeground = 0;

    for (pn = jn->auxprocs; pn; pn = pn->next)
	if (pn->status == SP_RUNNING)
	    return;

    for (pn = jn->procs; pn; pn = pn->next) {
	if (pn->status == SP_RUNNING)      /* some processes in this job are running       */
	    return;                        /* so no need to update job table entry         */
	if (WIFSTOPPED(pn->status))        /* some processes are stopped                   */
	    somestopped = 1;               /* so job is not done, but entry needs updating */
	if (!pn->next)                     /* last job in pipeline determines exit status  */
	    val = (WIFSIGNALED(pn->status)) ? 0200 | WTERMSIG(pn->status) :
		WEXITSTATUS(pn->status);
	if (pn->pid == jn->gleader)        /* if this process is process group leader      */
	    status = pn->status;
    }

    job = jn - jobtab;   /* compute job number */

    if (somestopped) {
	if (jn->stty_in_env && !jn->ty) {
	    jn->ty = (struct ttyinfo *) zalloc(sizeof(struct ttyinfo));
	    gettyinfo(jn->ty);
	}
	if (jn->stat & STAT_STOPPED) {
	    if (jn->stat & STAT_SUBJOB) {
		/* If we have `cat foo|while read a; grep $a bar;done'
		 * and have hit ^Z, the sub-job is stopped, but the
		 * super-job may still be running, waiting to be stopped
		 * or to exit. So we have to send it a SIGTSTP. */
		int i;

		if ((i = super_job(job)))
		    killpg(jobtab[i].gleader, SIGTSTP);
	    }
	    return;
	}
    }
    {                   /* job is done or stopped, remember return value */
	lastval2 = val;
	/* If last process was run in the current shell, keep old status
	 * and let it handle its own traps, but always allow the test
	 * for the pgrp.
	 */
	if (jn->stat & STAT_CURSH)
	    inforeground = 1;
	else if (job == thisjob) {
	    lastval = val;
	    inforeground = 2;
	}
    }

    if (shout && shout != stderr && !ttyfrozen && !jn->stty_in_env &&
	!zleactive && job == thisjob && !somestopped &&
	!(jn->stat & STAT_NOSTTY)) 
	gettyinfo(&shttyinfo);

    if (isset(MONITOR)) {
	pid_t pgrp = gettygrp();           /* get process group of tty      */

	/* is this job in the foreground of an interactive shell? */
	if (mypgrp != pgrp && inforeground &&
	    (jn->gleader == pgrp || (pgrp > 1 && kill(-pgrp, 0) == -1))) {
	    if (list_pipe) {
		if (somestopped || (pgrp > 1 && kill(-pgrp, 0) == -1)) {
		    attachtty(mypgrp);
		    /* check window size and adjust if necessary */
		    adjustwinsize(0);
		} else {
		    /*
		     * Oh, dear, we're right in the middle of some confusion
		     * of shell jobs on the righthand side of a pipeline, so
		     * it's death to call attachtty() just yet.  Mark the
		     * fact in the job, so that the attachtty() will be called
		     * when the job is finally deleted.
		     */
		    jn->stat |= STAT_ATTACH;
		}
		/* If we have `foo|while true; (( x++ )); done', and hit
		 * ^C, we have to stop the loop, too. */
		if ((val & 0200) && inforeground == 1 &&
		    ((val & ~0200) == SIGINT || (val & ~0200) == SIGQUIT)) {
		    if (!errbrk_saved) {
			errbrk_saved = 1;
			prev_breaks = breaks;
			prev_errflag = errflag;
		    }
		    breaks = loops;
		    errflag = 1;
		    inerrflush();
		}
	    } else {
		attachtty(mypgrp);
		/* check window size and adjust if necessary */
		adjustwinsize(0);
	    }
	}
    } else if (list_pipe && (val & 0200) && inforeground == 1 &&
	       ((val & ~0200) == SIGINT || (val & ~0200) == SIGQUIT)) {
	if (!errbrk_saved) {
	    errbrk_saved = 1;
	    prev_breaks = breaks;
	    prev_errflag = errflag;
	}
	breaks = loops;
	errflag = 1;
	inerrflush();
    }
    if (somestopped && jn->stat & STAT_SUPERJOB)
	return;
    jn->stat |= (somestopped) ? STAT_CHANGED | STAT_STOPPED :
	STAT_CHANGED | STAT_DONE;
    if (job == thisjob && (jn->stat & STAT_DONE)) {
	int i;
	Process p;

	for (p = jn->procs, i = 0; p && i < MAX_PIPESTATS; p = p->next, i++)
	    pipestats[i] = ((WIFSIGNALED(p->status)) ?
			    0200 | WTERMSIG(p->status) :
			    WEXITSTATUS(p->status));
	if ((jn->stat & STAT_CURSH) && i < MAX_PIPESTATS)
	    pipestats[i++] = lastval;
	numpipestats = i;
    }
    if (!inforeground &&
	(jn->stat & (STAT_SUBJOB | STAT_DONE)) == (STAT_SUBJOB | STAT_DONE)) {
	int su;

	if ((su = super_job(jn - jobtab)))
	    handle_sub(su, 0);
    }
    if ((jn->stat & (STAT_DONE | STAT_STOPPED)) == STAT_STOPPED) {
	prevjob = curjob;
	curjob = job;
    }
    if ((isset(NOTIFY) || job == thisjob) && (jn->stat & STAT_LOCKED)) {
	if (printjob(jn, !!isset(LONGLISTJOBS), 0) &&
	    zleactive)
	    zrefreshptr();
    }
    if (sigtrapped[SIGCHLD] && job != thisjob)
	dotrap(SIGCHLD);

    /* When MONITOR is set, the foreground process runs in a different *
     * process group from the shell, so the shell will not receive     *
     * terminal signals, therefore we we pretend that the shell got    *
     * the signal too.                                                 */
    if (inforeground == 2 && isset(MONITOR) && WIFSIGNALED(status)) {
	int sig = WTERMSIG(status);

	if (sig == SIGINT || sig == SIGQUIT) {
	    if (sigtrapped[sig]) {
		dotrap(sig);
		/* We keep the errflag as set or not by dotrap.
		 * This is to fulfil the promise to carry on
		 * with the jobs if trap returns zero.
		 * Setting breaks = loops ensures a consistent return
		 * status if inside a loop.  Maybe the code in loops
		 * should be changed.
		 */
		if (errflag)
		    breaks = loops;
	    } else {
		breaks = loops;
		errflag = 1;
	    }
	}
    }
}

/* set the previous job to something reasonable */

/**/
static void
setprevjob(void)
{
    int i;

    for (i = maxjob; i; i--)
	if ((jobtab[i].stat & STAT_INUSE) && (jobtab[i].stat & STAT_STOPPED) &&
	    !(jobtab[i].stat & STAT_SUBJOB) && i != curjob && i != thisjob) {
	    prevjob = i;
	    return;
	}

    for (i = maxjob; i; i--)
	if ((jobtab[i].stat & STAT_INUSE) && !(jobtab[i].stat & STAT_SUBJOB) &&
	    i != curjob && i != thisjob) {
	    prevjob = i;
	    return;
	}

    prevjob = -1;
}

/**/
#ifndef HAVE_GETRUSAGE
static long clktck = 0;

/**/
static void
set_clktck(void)
{
#ifdef _SC_CLK_TCK
    if (!clktck)
	/* fetch clock ticks per second from *
	 * sysconf only the first time       */
	clktck = sysconf(_SC_CLK_TCK);
#else
# ifdef __NeXT__
    /* NeXTStep 3.3 defines CLK_TCK wrongly */
    clktck = 60;
# else
#  ifdef CLK_TCK
    clktck = CLK_TCK;
#  else
#   ifdef HZ
     clktck = HZ;
#   else
     clktck = 60;
#   endif
#  endif
# endif
#endif
}
/**/
#endif

/**/
static void
printhhmmss(double secs)
{
    int mins = (int) secs / 60;
    int hours = mins / 60;

    secs -= 60 * mins;
    mins -= 60 * hours;
    if (hours)
	fprintf(stderr, "%d:%02d:%05.2f", hours, mins, secs);
    else if (mins)
	fprintf(stderr,      "%d:%05.2f",        mins, secs);
    else
	fprintf(stderr,           "%.3f",              secs);
}

static void
printtime(struct timeval *real, child_times_t *ti, char *desc)
{
    char *s;
    double elapsed_time, user_time, system_time;
#ifdef HAVE_GETRUSAGE
    double total_time;
#endif
    int percent, desclen;

    if (!desc)
    {
	desc = "";
	desclen = 0;
    }
    else
    {
	desc = dupstring(desc);
	unmetafy(desc, &desclen);
    }

    /* go ahead and compute these, since almost every TIMEFMT will have them */
    elapsed_time = real->tv_sec + real->tv_usec / 1000000.0;

#ifdef HAVE_GETRUSAGE
    user_time = ti->ru_utime.tv_sec + ti->ru_utime.tv_usec / 1000000.0;
    system_time = ti->ru_stime.tv_sec + ti->ru_stime.tv_usec / 1000000.0;
    total_time = user_time + system_time;
    percent = 100.0 * total_time
	/ (real->tv_sec + real->tv_usec / 1000000.0);
#else
    set_clktck();
    user_time    = ti->ut / (double) clktck;
    system_time  = ti->st / (double) clktck;
    percent      =  100.0 * (ti->ut + ti->st)
	/ (clktck * real->tv_sec + clktck * real->tv_usec / 1000000.0);
#endif

    queue_signals();
    if (!(s = getsparam("TIMEFMT")))
	s = DEFAULT_TIMEFMT;

    for (; *s; s++)
	if (*s == '%')
	    switch (*++s) {
	    case 'E':
		fprintf(stderr, "%4.2fs", elapsed_time);
		break;
	    case 'U':
		fprintf(stderr, "%4.2fs", user_time);
		break;
	    case 'S':
		fprintf(stderr, "%4.2fs", system_time);
		break;
	    case '*':
		switch (*++s) {
		case 'E':
		    printhhmmss(elapsed_time);
		    break;
		case 'U':
		    printhhmmss(user_time);
		    break;
		case 'S':
		    printhhmmss(system_time);
		    break;
		default:
		    fprintf(stderr, "%%*");
		    s--;
		    break;
		}
		break;
	    case 'P':
		fprintf(stderr, "%d%%", percent);
		break;
#ifdef HAVE_STRUCT_RUSAGE_RU_NSWAP
	    case 'W':
		fprintf(stderr, "%ld", ti->ru_nswap);
		break;
#endif
#ifdef HAVE_STRUCT_RUSAGE_RU_IXRSS
	    case 'X':
		fprintf(stderr, "%ld", (long)(ti->ru_ixrss / total_time));
		break;
#endif
#ifdef HAVE_STRUCT_RUSAGE_RU_IDRSS
	    case 'D':
		fprintf(stderr, "%ld",
			(long) ((ti->ru_idrss
#ifdef HAVE_STRUCT_RUSAGE_RU_ISRSS
				 + ti->ru_isrss
#endif
				    ) / total_time));
		break;
#endif
#if defined(HAVE_STRUCT_RUSAGE_RU_IDRSS) || \
    defined(HAVE_STRUCT_RUSAGE_RU_ISRSS) || \
    defined(HAVE_STRUCT_RUSAGE_RU_IXRSS)
	    case 'K':
		/* treat as D if X not available */
		fprintf(stderr, "%ld",
			(long) ((
#ifdef HAVE_STRUCT_RUSAGE_RU_IXRSS
				    ti->ru_ixrss
#else
				    0
#endif
#ifdef HAVE_STRUCT_RUSAGE_RU_IDRSS
				    + ti->ru_idrss
#endif
#ifdef HAVE_STRUCT_RUSAGE_RU_ISRSS
				    + ti->ru_isrss
#endif
				    ) / total_time));
		break;
#endif
#ifdef HAVE_STRUCT_RUSAGE_RU_MAXRSS
	    case 'M':
		fprintf(stderr, "%ld", ti->ru_maxrss / 1024);
		break;
#endif
#ifdef HAVE_STRUCT_RUSAGE_RU_MAJFLT
	    case 'F':
		fprintf(stderr, "%ld", ti->ru_majflt);
		break;
#endif
#ifdef HAVE_STRUCT_RUSAGE_RU_MINFLT
	    case 'R':
		fprintf(stderr, "%ld", ti->ru_minflt);
		break;
#endif
#ifdef HAVE_STRUCT_RUSAGE_RU_INBLOCK
	    case 'I':
		fprintf(stderr, "%ld", ti->ru_inblock);
		break;
#endif
#ifdef HAVE_STRUCT_RUSAGE_RU_OUBLOCK
	    case 'O':
		fprintf(stderr, "%ld", ti->ru_oublock);
		break;
#endif
#ifdef HAVE_STRUCT_RUSAGE_RU_MSGRCV
	    case 'r':
		fprintf(stderr, "%ld", ti->ru_msgrcv);
		break;
#endif
#ifdef HAVE_STRUCT_RUSAGE_RU_MSGSND
	    case 's':
		fprintf(stderr, "%ld", ti->ru_msgsnd);
		break;
#endif
#ifdef HAVE_STRUCT_RUSAGE_RU_NSIGNALS
	    case 'k':
		fprintf(stderr, "%ld", ti->ru_nsignals);
		break;
#endif
#ifdef HAVE_STRUCT_RUSAGE_RU_NVCSW
	    case 'w':
		fprintf(stderr, "%ld", ti->ru_nvcsw);
		break;
#endif
#ifdef HAVE_STRUCT_RUSAGE_RU_NIVCSW
	    case 'c':
		fprintf(stderr, "%ld", ti->ru_nivcsw);
		break;
#endif
	    case 'J':
		fwrite(desc, sizeof(char), desclen, stderr);
		break;
	    case '%':
		putc('%', stderr);
		break;
	    case '\0':
		s--;
		break;
	    default:
		fprintf(stderr, "%%%c", *s);
		break;
	} else
	    putc(*s, stderr);
    unqueue_signals();
    putc('\n', stderr);
    fflush(stderr);
}

/**/
static void
dumptime(Job jn)
{
    Process pn;
    struct timeval dtimeval;

    if (!jn->procs)
	return;
    for (pn = jn->procs; pn; pn = pn->next)
	printtime(dtime(&dtimeval, &pn->bgtime, &pn->endtime), &pn->ti,
		  pn->text);
}

/* Check whether shell should report the amount of time consumed   *
 * by job.  This will be the case if we have preceded the command  *
 * with the keyword time, or if REPORTTIME is non-negative and the *
 * amount of time consumed by the job is greater than REPORTTIME   */

/**/
static int
should_report_time(Job j)
{
    struct value vbuf;
    Value v;
    char *s = "REPORTTIME";
    zlong reporttime;

    /* if the time keyword was used */
    if (j->stat & STAT_TIMED)
	return 1;

    queue_signals();
    if (!(v = getvalue(&vbuf, &s, 0)) ||
	(reporttime = getintvalue(v)) < 0) {
	unqueue_signals();
	return 0;
    }
    unqueue_signals();
    /* can this ever happen? */
    if (!j->procs)
	return 0;

#ifdef HAVE_GETRUSAGE
    reporttime -= j->procs->ti.ru_utime.tv_sec + j->procs->ti.ru_stime.tv_sec;
    if (j->procs->ti.ru_utime.tv_usec +
	j->procs->ti.ru_stime.tv_usec >= 1000000)
	reporttime--;
    return reporttime <= 0;
#else
    set_clktck();
    return ((j->procs->ti.ut + j->procs->ti.st) / clktck >= reporttime);
#endif
}

/* !(lng & 3) means jobs    *
 *  (lng & 1) means jobs -l *
 *  (lng & 2) means jobs -p
 *  (lng & 4) means jobs -d
 *
 * synch = 0 means asynchronous
 * synch = 1 means synchronous
 * synch = 2 means called synchronously from jobs
 *
 * Returns 1 if some output was done.
 *
 * The function also deletes the job if it was done, even it
 * is not printed.
 */

/**/
int
printjob(Job jn, int lng, int synch)
{
    Process pn;
    int job, len = 9, sig, sflag = 0, llen;
    int conted = 0, lineleng = columns, skip = 0, doputnl = 0;
    int doneprint = 0;
    FILE *fout = (synch == 2) ? stdout : shout;

    /*
     * Wow, what a hack.  Did I really write this? --- pws
     */
    if (jn < jobtab || jn >= jobtab + jobtabsize)
	job = jn - oldjobtab;
    else
	job = jn - jobtab;

    if (jn->stat & STAT_NOPRINT) {
	if (jn->stat & STAT_DONE) {
	    deletejob(jn);
	    if (job == curjob) {
		curjob = prevjob;
		prevjob = job;
	    }
	    if (job == prevjob)
		setprevjob();
	}
	return 0;
    }

    if (lng < 0) {
	conted = 1;
	lng = !!isset(LONGLISTJOBS);
    }

/* find length of longest signame, check to see */
/* if we really need to print this job          */

    for (pn = jn->procs; pn; pn = pn->next) {
	if (jn->stat & STAT_SUPERJOB &&
	    jn->procs->status == SP_RUNNING && !pn->next)
	    pn->status = SP_RUNNING;
	if (pn->status != SP_RUNNING) {
	    if (WIFSIGNALED(pn->status)) {
		sig = WTERMSIG(pn->status);
		llen = strlen(sigmsg(sig));
		if (WCOREDUMP(pn->status))
		    llen += 14;
		if (llen > len)
		    len = llen;
		if (sig != SIGINT && sig != SIGPIPE)
		    sflag = 1;
		if (job == thisjob && sig == SIGINT)
		    doputnl = 1;
	    } else if (WIFSTOPPED(pn->status)) {
		sig = WSTOPSIG(pn->status);
		if ((int)strlen(sigmsg(sig)) > len)
		    len = strlen(sigmsg(sig));
		if (job == thisjob && sig == SIGTSTP)
		    doputnl = 1;
	    } else if (isset(PRINTEXITVALUE) && isset(SHINSTDIN) &&
		       WEXITSTATUS(pn->status))
		sflag = 1;
	}
    }

/* print if necessary: ignore option state on explicit call to `jobs'. */

    if (synch == 2 || 
	(interact && jobbing &&
	 ((jn->stat & STAT_STOPPED) || sflag || job != thisjob))) {
	int len2, fline = 1;
	/* use special format for current job, except in `jobs' */
	int thisfmt = job == thisjob && synch != 2;
	Process qn;

	if (!synch)
	    trashzleptr();
	if (doputnl && !synch) {
	    doneprint = 1;
	    putc('\n', fout);
	}
	for (pn = jn->procs; pn;) {
	    len2 = (thisfmt ? 5 : 10) + len;	/* 2 spaces */
	    if (lng & 3)
		qn = pn->next;
	    else
		for (qn = pn->next; qn; qn = qn->next) {
		    if (qn->status != pn->status)
			break;
		    if ((int)strlen(qn->text) + len2 + ((qn->next) ? 3 : 0) > lineleng)
			break;
		    len2 += strlen(qn->text) + 2;
		}
	    doneprint = 1;
	    if (!thisfmt || lng) {
		if (fline)
		    fprintf(fout, "[%ld]  %c ",
			    (long)job,
			    (job == curjob) ? '+'
			    : (job == prevjob) ? '-' : ' ');
		else
		    fprintf(fout, (job > 9) ? "        " : "       ");
	    } else
		fprintf(fout, "zsh: ");
	    if (lng & 1)
		fprintf(fout, "%ld ", (long) pn->pid);
	    else if (lng & 2) {
		pid_t x = jn->gleader;

		fprintf(fout, "%ld ", (long) x);
		do
		    skip++;
		while ((x /= 10));
		skip++;
		lng &= ~3;
	    } else
		fprintf(fout, "%*s", skip, "");
	    if (pn->status == SP_RUNNING) {
		if (!conted)
		    fprintf(fout, "running%*s", len - 7 + 2, "");
		else
		    fprintf(fout, "continued%*s", len - 9 + 2, "");
	    }
	    else if (WIFEXITED(pn->status)) {
		if (WEXITSTATUS(pn->status))
		    fprintf(fout, "exit %-4d%*s", WEXITSTATUS(pn->status),
			    len - 9 + 2, "");
		else
		    fprintf(fout, "done%*s", len - 4 + 2, "");
	    } else if (WIFSTOPPED(pn->status))
		fprintf(fout, "%-*s", len + 2, sigmsg(WSTOPSIG(pn->status)));
	    else if (WCOREDUMP(pn->status))
		fprintf(fout, "%s (core dumped)%*s",
			sigmsg(WTERMSIG(pn->status)),
			(int)(len - 14 + 2 - strlen(sigmsg(WTERMSIG(pn->status)))), "");
	    else
		fprintf(fout, "%-*s", len + 2, sigmsg(WTERMSIG(pn->status)));
	    for (; pn != qn; pn = pn->next) {
		char *txt = dupstring(pn->text);
		int txtlen;
		unmetafy(txt, &txtlen);
		fwrite(txt, sizeof(char), txtlen, fout);
		if (pn->next)
		    fputs(" | ", fout);
	    }
	    putc('\n', fout);
	    fline = 0;
	}
	fflush(fout);
    } else if (doputnl && interact && !synch) {
	doneprint = 1;
	putc('\n', fout);
	fflush(fout);
    }

/* print "(pwd now: foo)" messages: with (lng & 4) we are printing
 * the directory where the job is running, otherwise the current directory
 */

    if ((lng & 4) || (interact && job == thisjob &&
		      jn->pwd && strcmp(jn->pwd, pwd))) {
	doneprint = 1;
	fprintf(fout, "(pwd %s: ", (lng & 4) ? "" : "now");
	fprintdir(((lng & 4) && jn->pwd) ? jn->pwd : pwd, fout);
	fprintf(fout, ")\n");
	fflush(fout);
    }
/* delete job if done */

    if (jn->stat & STAT_DONE) {
	if (should_report_time(jn))
	    dumptime(jn);
	deletejob(jn);
	if (job == curjob) {
	    curjob = prevjob;
	    prevjob = job;
	}
	if (job == prevjob)
	    setprevjob();
    } else
	jn->stat &= ~STAT_CHANGED;

    return doneprint;
}

/**/
void
deletefilelist(LinkList file_list)
{
    char *s;
    if (file_list) {
	while ((s = (char *)getlinknode(file_list))) {
	    unlink(s);
	    zsfree(s);
	}
	zfree(file_list, sizeof(struct linklist));
    }
}

/**/
void
freejob(Job jn, int deleting)
{
    struct process *pn, *nx;

    pn = jn->procs;
    jn->procs = NULL;
    for (; pn; pn = nx) {
	nx = pn->next;
	zfree(pn, sizeof(struct process));
    }

    pn = jn->auxprocs;
    jn->auxprocs = NULL;
    for (; pn; pn = nx) {
	nx = pn->next;
	zfree(pn, sizeof(struct process));
    }

    if (jn->ty)
	zfree(jn->ty, sizeof(struct ttyinfo));
    if (jn->pwd)
	zsfree(jn->pwd);
    jn->pwd = NULL;
    if (jn->stat & STAT_WASSUPER) {
	/* careful in case we shrink and move the job table */
	int job = jn - jobtab;
	if (deleting)
	    deletejob(jobtab + jn->other);
	else
	    freejob(jobtab + jn->other, 0);
	jn = jobtab + job;
    }
    jn->gleader = jn->other = 0;
    jn->stat = jn->stty_in_env = 0;
    jn->filelist = NULL;
    jn->ty = NULL;

    /* Find the new highest job number. */
    if (maxjob == jn - jobtab) {
	while (maxjob && !(jobtab[maxjob].stat & STAT_INUSE))
	    maxjob--;
    }
}

/*
 * We are actually finished with this job, rather
 * than freeing it to make space.
 */

/**/
void
deletejob(Job jn)
{
    deletefilelist(jn->filelist);
    if (jn->stat & STAT_ATTACH) {
	attachtty(mypgrp);
	adjustwinsize(0);
    }

    freejob(jn, 1);
}

/*
 * Add a process to the current job.
 * The third argument is 1 if we are adding a process which is not
 * part of the main pipeline but an auxiliary process used for
 * handling MULTIOS or process substitution.  We will wait for it
 * but not display job information about it.
 */

/**/
void
addproc(pid_t pid, char *text, int aux, struct timeval *bgtime)
{
    Process pn, *pnlist;

    DPUTS(thisjob == -1, "No valid job in addproc.");
    pn = (Process) zshcalloc(sizeof *pn);
    pn->pid = pid;
    if (text)
	strcpy(pn->text, text);
    else
	*pn->text = '\0';
    pn->status = SP_RUNNING;
    pn->next = NULL;

    if (!aux)
    {
	pn->bgtime = *bgtime;
	/* if this is the first process we are adding to *
	 * the job, then it's the group leader.          */
	if (!jobtab[thisjob].gleader)
	    jobtab[thisjob].gleader = pid;
	/* attach this process to end of process list of current job */
	pnlist = &jobtab[thisjob].procs;
    }
    else
	pnlist = &jobtab[thisjob].auxprocs;

    if (*pnlist) {
	Process n;

	for (n = *pnlist; n->next; n = n->next);
	n->next = pn;
    } else {
	/* first process for this job */
	*pnlist = pn;
    }
    /* If the first process in the job finished before any others were *
     * added, maybe STAT_DONE got set incorrectly.  This can happen if *
     * a $(...) was waited for and the last existing job in the        *
     * pipeline was already finished.  We need to be very careful that *
     * there was no call to printjob() between then and now, else      *
     * the job will already have been deleted from the table.          */
    jobtab[thisjob].stat &= ~STAT_DONE;
}

/* Check if we have files to delete.  We need to check this to see *
 * if it's all right to exec a command without forking in the last *
 * component of subshells or after the `-c' option.                */

/**/
int
havefiles(void)
{
    int i;

    for (i = 1; i <= maxjob; i++)
	if (jobtab[i].stat && jobtab[i].filelist)
	    return 1;
    return 0;

}

/*
 * Wait for a particular process.
 * wait_cmd indicates this is from the interactive wait command,
 * in which case the behaviour is a little different:  the command
 * itself can be interrupted by a trapped signal.
 */

/**/
int
waitforpid(pid_t pid, int wait_cmd)
{
    int first = 1, q = queue_signal_level();

    /* child_block() around this loop in case #ifndef WNOHANG */
    dont_queue_signals();
    child_block();		/* unblocked in signal_suspend() */
    queue_traps(wait_cmd);
    while (!errflag && (kill(pid, 0) >= 0 || errno != ESRCH)) {
	if (first)
	    first = 0;
	else
	    kill(pid, SIGCONT);

	last_signal = -1;
	signal_suspend(SIGCHLD);
	if (last_signal != SIGCHLD && wait_cmd) {
	    /* wait command interrupted, but no error: return */
	    restore_queue_signals(q);
	    return 128 + last_signal;
	}
	child_block();
    }
    unqueue_traps();
    child_unblock();
    restore_queue_signals(q);

    return 0;
}

/*
 * Wait for a job to finish.
 * wait_cmd indicates this is from the wait builtin; see
 * wait_cmd in waitforpid().
 */

/**/
static int
zwaitjob(int job, int wait_cmd)
{
    int q = queue_signal_level();
    Job jn = jobtab + job;

    dont_queue_signals();
    child_block();		 /* unblocked during signal_suspend() */
    queue_traps(wait_cmd);
    if (jn->procs || jn->auxprocs) { /* if any forks were done         */
	jn->stat |= STAT_LOCKED;
	if (jn->stat & STAT_CHANGED)
	    printjob(jn, !!isset(LONGLISTJOBS), 1);
	while (!errflag && jn->stat &&
	       !(jn->stat & STAT_DONE) &&
	       !(interact && (jn->stat & STAT_STOPPED))) {
	    signal_suspend(SIGCHLD);
	    if (last_signal != SIGCHLD && wait_cmd)
	    {
		/* builtin wait interrupted by trapped signal */
		restore_queue_signals(q);
		return 128 + last_signal;
	    }
	    /* Commenting this out makes ^C-ing a job started by a function
	       stop the whole function again.  But I guess it will stop
	       something else from working properly, we have to find out
	       what this might be.  --oberon

	    errflag = 0; */
	    if (subsh) {
		killjb(jn, SIGCONT);
		jn->stat &= ~STAT_STOPPED;
	    }
	    if (jn->stat & STAT_SUPERJOB)
		if (handle_sub(jn - jobtab, 1))
		    break;
	    child_block();
	}
    } else {
	deletejob(jn);
	pipestats[0] = lastval;
	numpipestats = 1;
    }
    unqueue_traps();
    child_unblock();
    restore_queue_signals(q);

    return 0;
}

/* wait for running job to finish */

/**/
void
waitjobs(void)
{
    Job jn = jobtab + thisjob;
    DPUTS(thisjob == -1, "No valid job in waitjobs.");

    if (jn->procs || jn->auxprocs)
	zwaitjob(thisjob, 0);
    else {
	deletejob(jn);
	pipestats[0] = lastval;
	numpipestats = 1;
    }
    thisjob = -1;
}

/* clear job table when entering subshells */

/**/
mod_export void
clearjobtab(int monitor)
{
    int i;

    for (i = 1; i <= maxjob; i++) {
	/*
	 * See if there is a jobtable worth saving.
	 * We never free the saved version; it only happens
	 * once for each subshell of a shell with job control,
	 * so doesn't create a leak.
	 */
	if (monitor && jobtab[i].stat)
	    oldmaxjob = i+1;
	else if (jobtab[i].stat & STAT_INUSE)
	    freejob(jobtab + i, 0);
    }

    if (monitor && oldmaxjob) {
	int sz = oldmaxjob * sizeof(struct job);
	oldjobtab = (struct job *)zalloc(sz);
	memcpy(oldjobtab, jobtab, sz);

	/* Don't report any job we're part of */
	if (thisjob != -1 && thisjob < oldmaxjob)
	    memset(oldjobtab+thisjob, 0, sizeof(struct job));
    }

    memset(jobtab, 0, jobtabsize * sizeof(struct job)); /* zero out table */
    maxjob = 0;
}

static int initnewjob(int i)
{
    jobtab[i].stat = STAT_INUSE;
    if (jobtab[i].pwd) {
	zsfree(jobtab[i].pwd);
	jobtab[i].pwd = NULL;
    }
    jobtab[i].gleader = 0;

    if (i > maxjob)
	maxjob = i;

    return i;
}

/* Get a free entry in the job table and initialize it. */

/**/
int
initjob(void)
{
    int i;

    for (i = 1; i <= maxjob; i++)
	if (!jobtab[i].stat)
	    return initnewjob(i);
    if (maxjob + 1 < jobtabsize)
	return initnewjob(maxjob+1);

    if (expandjobtab())
	return initnewjob(i);

    zerr("job table full or recursion limit exceeded");
    return -1;
}

/**/
void
setjobpwd(void)
{
    int i;

    for (i = 1; i <= maxjob; i++)
	if (jobtab[i].stat && !jobtab[i].pwd)
	    jobtab[i].pwd = ztrdup(pwd);
}

/* print pids for & */

/**/
void
spawnjob(void)
{
    Process pn;

    DPUTS(thisjob == -1, "No valid job in spawnjob.");
    /* if we are not in a subshell */
    if (!subsh) {
	if (curjob == -1 || !(jobtab[curjob].stat & STAT_STOPPED)) {
	    curjob = thisjob;
	    setprevjob();
	} else if (prevjob == -1 || !(jobtab[prevjob].stat & STAT_STOPPED))
	    prevjob = thisjob;
	if (interact && jobbing && jobtab[thisjob].procs) {
	    fprintf(stderr, "[%d]", thisjob);
	    for (pn = jobtab[thisjob].procs; pn; pn = pn->next)
		fprintf(stderr, " %ld", (long) pn->pid);
	    fprintf(stderr, "\n");
	    fflush(stderr);
	}
    }
    if (!hasprocs(thisjob))
	deletejob(jobtab + thisjob);
    else
	jobtab[thisjob].stat |= STAT_LOCKED;
    thisjob = -1;
}

/**/
void
shelltime(void)
{
    struct timezone dummy_tz;
    struct timeval dtimeval, now;
    child_times_t ti;
#ifndef HAVE_GETRUSAGE
    struct tms buf;
#endif

    gettimeofday(&now, &dummy_tz);

#ifdef HAVE_GETRUSAGE
    getrusage(RUSAGE_SELF, &ti);
#else
    times(&buf);

    ti.ut = buf.tms_utime;
    ti.st = buf.tms_stime;
#endif
    printtime(dtime(&dtimeval, &shtimer, &now), &ti, "shell");

#ifdef HAVE_GETRUSAGE
    getrusage(RUSAGE_CHILDREN, &ti);
#else
    ti.ut = buf.tms_cutime;
    ti.st = buf.tms_cstime;
#endif
    printtime(&dtimeval, &ti, "children");

}

/* see if jobs need printing */
 
/**/
void
scanjobs(void)
{
    int i;
 
    for (i = 1; i <= maxjob; i++)
        if (jobtab[i].stat & STAT_CHANGED)
            printjob(jobtab + i, !!isset(LONGLISTJOBS), 1);
}

/**** job control builtins ****/

/* This simple function indicates whether or not s may represent      *
 * a number.  It returns true iff s consists purely of digits and     *
 * minuses.  Note that minus may appear more than once, and the empty *
 * string will produce a `true' response.                             */

/**/
static int
isanum(char *s)
{
    while (*s == '-' || idigit(*s))
	s++;
    return *s == '\0';
}

/* Make sure we have a suitable current and previous job set. */

/**/
static void
setcurjob(void)
{
    if (curjob == thisjob ||
	(curjob != -1 && !(jobtab[curjob].stat & STAT_INUSE))) {
	curjob = prevjob;
	setprevjob();
	if (curjob == thisjob ||
	    (curjob != -1 && !((jobtab[curjob].stat & STAT_INUSE) &&
			       curjob != thisjob))) {
	    curjob = prevjob;
	    setprevjob();
	}
    }
}

/* Convert a job specifier ("%%", "%1", "%foo", "%?bar?", etc.) *
 * to a job number.                                             */

/**/
static int
getjob(char *s, char *prog)
{
    int jobnum, returnval;

    /* if there is no %, treat as a name */
    if (*s != '%')
	goto jump;
    s++;
    /* "%%", "%+" and "%" all represent the current job */
    if (*s == '%' || *s == '+' || !*s) {
	if (curjob == -1) {
	    zwarnnam(prog, "no current job");
	    returnval = -1;
	    goto done;
	}
	returnval = curjob;
	goto done;
    }
    /* "%-" represents the previous job */
    if (*s == '-') {
	if (prevjob == -1) {
	    zwarnnam(prog, "no previous job");
	    returnval = -1;
	    goto done;
	}
	returnval = prevjob;
	goto done;
    }
    /* a digit here means we have a job number */
    if (idigit(*s)) {
	jobnum = atoi(s);
	if (jobnum && jobnum <= maxjob && jobtab[jobnum].stat &&
	    !(jobtab[jobnum].stat & STAT_SUBJOB) && jobnum != thisjob) {
	    returnval = jobnum;
	    goto done;
	}
	zwarnnam(prog, "%%%s: no such job", s);
	returnval = -1;
	goto done;
    }
    /* "%?" introduces a search string */
    if (*s == '?') {
	struct process *pn;

	for (jobnum = maxjob; jobnum >= 0; jobnum--)
	    if (jobtab[jobnum].stat && !(jobtab[jobnum].stat & STAT_SUBJOB) &&
		jobnum != thisjob)
		for (pn = jobtab[jobnum].procs; pn; pn = pn->next)
		    if (strstr(pn->text, s + 1)) {
			returnval = jobnum;
			goto done;
		    }
	zwarnnam(prog, "job not found: %s", s);
	returnval = -1;
	goto done;
    }
  jump:
    /* anything else is a job name, specified as a string that begins the
    job's command */
    if ((jobnum = findjobnam(s)) != -1) {
	returnval = jobnum;
	goto done;
    }
    /* if we get here, it is because none of the above succeeded and went
    to done */
    zwarnnam(prog, "job not found: %s", s);
    returnval = -1;
  done:
    return returnval;
}

/* For jobs -Z (which modifies the shell's name as seen in ps listings).  *
 * hackzero is the start of the safely writable space, and hackspace is   *
 * its length, excluding a final NUL terminator that will always be left. */

static char *hackzero;
static int hackspace;


/* Initialise job handling. */

/**/
void
init_jobs(char **argv, char **envp)
{
    char *p, *q;
    size_t init_bytes = MAXJOBS_ALLOC*sizeof(struct job);

    /*
     * Initialise the job table.  If this fails, we're in trouble.
     */
    jobtab = (struct job *)zalloc(init_bytes);
    if (!jobtab) {
	zerr("failed to allocate job table, aborting.");
	exit(1);
    }
    jobtabsize = MAXJOBS_ALLOC;
    memset(jobtab, 0, init_bytes);

    /*
     * Initialise the jobs -Z system.  The technique is borrowed from
     * perl: check through the argument and environment space, to see
     * how many of the strings are in contiguous space.  This determines
     * the value of hackspace.
     */
    hackzero = *argv;
    p = strchr(hackzero, 0);
    while(*++argv) {
	q = *argv;
	if(q != p+1)
	    goto done;
	p = strchr(q, 0);
    }
    for(; *envp; envp++) {
	q = *envp;
	if(q != p+1)
	    goto done;
	p = strchr(q, 0);
    }
    done:
    hackspace = p - hackzero;
}


/*
 * We have run out of space in the job table.
 * Expand it by an additional MAXJOBS_ALLOC slots.
 */

/*
 * An arbitrary limit on the absolute maximum size of the job table.
 * This prevents us taking over the entire universe.
 * Ought to be a multiple of MAXJOBS_ALLOC, but doesn't need to be.
 */
#define MAX_MAXJOBS	1000

/**/
int
expandjobtab(void)
{
    int newsize = jobtabsize + MAXJOBS_ALLOC;
    struct job *newjobtab;

    if (newsize > MAX_MAXJOBS)
	return 0;

    newjobtab = (struct job *)zrealloc(jobtab, newsize * sizeof(struct job));
    if (!newjobtab)
	return 0;

    /*
     * Clear the new section of the table; this is necessary for
     * the jobs to appear unused.
     */
    memset(newjobtab + jobtabsize, 0, MAXJOBS_ALLOC * sizeof(struct job));

    jobtab = newjobtab;
    jobtabsize = newsize;

    return 1;
}


/*
 * See if we can reduce the job table.  We can if we go over
 * a MAXJOBS_ALLOC boundary.  However, we leave a boundary,
 * currently 20 jobs, so that we have a place for immediate
 * expansion and don't play ping pong with the job table size.
 */

/**/
void
maybeshrinkjobtab(void)
{
    int jobbound;

    queue_signals();
    jobbound = maxjob + MAXJOBS_ALLOC - (maxjob % MAXJOBS_ALLOC);
    if (jobbound < jobtabsize && jobbound > maxjob + 20) {
	struct job *newjobtab;

	/* Hope this can't fail, but anyway... */
	newjobtab = (struct job *)zrealloc(jobtab,
					   jobbound*sizeof(struct job));

	if (newjobtab) {
	    jobtab = newjobtab;
	    jobtabsize = jobbound;
	}
    }
    unqueue_signals();
}


/* bg, disown, fg, jobs, wait: most of the job control commands are     *
 * here.  They all take the same type of argument.  Exception: wait can *
 * take a pid or a job specifier, whereas the others only work on jobs. */

/**/
int
bin_fg(char *name, char **argv, Options ops, int func)
{
    int job, lng, firstjob = -1, retval = 0, ofunc = func;

    if (OPT_ISSET(ops,'Z')) {
	int len;

	if(isset(RESTRICTED)) {
	    zwarnnam(name, "-Z is restricted");
	    return 1;
	}
	if(!argv[0] || argv[1]) {
	    zwarnnam(name, "-Z requires one argument");
	    return 1;
	}
	queue_signals();
	unmetafy(*argv, &len);
	if(len > hackspace)
	    len = hackspace;
	memcpy(hackzero, *argv, len);
	memset(hackzero + len, 0, hackspace - len);
	unqueue_signals();
	return 0;
    }

    if (func == BIN_JOBS) {
	lng = (OPT_ISSET(ops,'l')) ? 1 : (OPT_ISSET(ops,'p')) ? 2 : 0;
	if (OPT_ISSET(ops,'d'))
	    lng |= 4;
    } else {
	lng = !!isset(LONGLISTJOBS);
    }

    if ((func == BIN_FG || func == BIN_BG) && !jobbing) {
	/* oops... maybe bg and fg should have been disabled? */
	zwarnnam(name, "no job control in this shell.");
	return 1;
    }

    queue_signals();
    /* If necessary, update job table. */
    if (unset(NOTIFY))
	scanjobs();

    if (func != BIN_JOBS || isset(MONITOR) || !oldmaxjob)
	setcurjob();

    if (func == BIN_JOBS)
        /* If you immediately type "exit" after "jobs", this      *
         * will prevent zexit from complaining about stopped jobs */
	stopmsg = 2;
    if (!*argv) {
	/* This block handles all of the default cases (no arguments).  bg,
	fg and disown act on the current job, and jobs and wait act on all the
	jobs. */
 	if (func == BIN_FG || func == BIN_BG || func == BIN_DISOWN) {
	    /* W.r.t. the above comment, we'd better have a current job at this
	    point or else. */
	    if (curjob == -1 || (jobtab[curjob].stat & STAT_NOPRINT)) {
		zwarnnam(name, "no current job");
		unqueue_signals();
		return 1;
	    }
	    firstjob = curjob;
	} else if (func == BIN_JOBS) {
	    /* List jobs. */
	    struct job *jobptr;
	    int curmaxjob, ignorejob;
	    if (unset(MONITOR) && oldmaxjob) {
		jobptr = oldjobtab;
		curmaxjob = oldmaxjob ? oldmaxjob - 1 : 0;
		ignorejob = 0;
	    } else {
		jobptr = jobtab;
		curmaxjob = maxjob;
		ignorejob = thisjob;
	    }
	    for (job = 0; job <= curmaxjob; job++, jobptr++)
		if (job != ignorejob && jobptr->stat) {
		    if ((!OPT_ISSET(ops,'r') && !OPT_ISSET(ops,'s')) ||
			(OPT_ISSET(ops,'r') && OPT_ISSET(ops,'s')) ||
			(OPT_ISSET(ops,'r') && 
			 !(jobptr->stat & STAT_STOPPED)) ||
			(OPT_ISSET(ops,'s') && jobptr->stat & STAT_STOPPED))
			printjob(jobptr, lng, 2);
		}
	    unqueue_signals();
	    return 0;
	} else {   /* Must be BIN_WAIT, so wait for all jobs */
	    for (job = 0; job <= maxjob; job++)
		if (job != thisjob && jobtab[job].stat)
		    retval = zwaitjob(job, 1);
	    unqueue_signals();
	    return retval;
	}
    }

    /* Defaults have been handled.  We now have an argument or two, or three...
    In the default case for bg, fg and disown, the argument will be provided by
    the above routine.  We now loop over the arguments. */
    for (; (firstjob != -1) || *argv; (void)(*argv && argv++)) {
	int stopped, ocj = thisjob;

        func = ofunc;

	if (func == BIN_WAIT && isanum(*argv)) {
	    /* wait can take a pid; the others can't. */
	    pid_t pid = (long)atoi(*argv);
	    Job j;
	    Process p;

	    if (findproc(pid, &j, &p, 0)) {
		/*
		 * returns 0 for normal exit, else signal+128
		 * in which case we should return that status.
		 */
		retval = waitforpid(pid, 1);
		if (!retval)
		    retval = lastval2;
	    } else {
		zwarnnam(name, "pid %d is not a child of this shell", pid);
		/* presumably lastval2 doesn't tell us a heck of a lot? */
		retval = 1;
	    }
	    thisjob = ocj;
	    continue;
	}
	/* The only type of argument allowed now is a job spec.  Check it. */
	job = (*argv) ? getjob(*argv, name) : firstjob;
	firstjob = -1;
	if (job == -1) {
	    retval = 1;
	    break;
	}
	if (!(jobtab[job].stat & STAT_INUSE) ||
	    (jobtab[job].stat & STAT_NOPRINT)) {
	    zwarnnam(name, "%%%d: no such job", job);
	    unqueue_signals();
	    return 1;
	}
        /* If AUTO_CONTINUE is set (automatically make stopped jobs running
         * on disown), we actually do a bg and then delete the job table entry. */

        if (isset(AUTOCONTINUE) && func == BIN_DISOWN &&
            jobtab[job].stat & STAT_STOPPED)
            func = BIN_BG;

	/* We have a job number.  Now decide what to do with it. */
	switch (func) {
	case BIN_FG:
	case BIN_BG:
	case BIN_WAIT:
	    if (func == BIN_BG)
		jobtab[job].stat |= STAT_NOSTTY;
	    if ((stopped = (jobtab[job].stat & STAT_STOPPED))) {
		makerunning(jobtab + job);
		if (func == BIN_BG) {
		    /* Set $! to indicate this was backgrounded */
		    Process pn = jobtab[job].procs;
		    for (;;) {
			Process next = pn->next;
			if (!next) {
			    lastpid = (zlong) pn->pid;
			    break;
			}
			pn = next;
		    }
		}
	    } else if (func == BIN_BG) {
		/* Silly to bg a job already running. */
		zwarnnam(name, "job already in background");
		thisjob = ocj;
		unqueue_signals();
		return 1;
	    }
	    /* It's time to shuffle the jobs around!  Reset the current job,
	    and pick a sensible secondary job. */
	    if (curjob == job) {
		curjob = prevjob;
		prevjob = (func == BIN_BG) ? -1 : job;
	    }
	    if (prevjob == job || prevjob == -1)
		setprevjob();
	    if (curjob == -1) {
		curjob = prevjob;
		setprevjob();
	    }
	    if (func != BIN_WAIT)
		/* for bg and fg -- show the job we are operating on */
		printjob(jobtab + job, (stopped) ? -1 : lng, 1);
	    if (func != BIN_BG) {		/* fg or wait */
		if (jobtab[job].pwd && strcmp(jobtab[job].pwd, pwd)) {
		    FILE *fout = (func == BIN_JOBS) ? stdout : shout;
		    fprintf(fout, "(pwd : ");
		    fprintdir(jobtab[job].pwd, fout);
		    fprintf(fout, ")\n");
		    fflush(fout);
		}
		if (func != BIN_WAIT) {		/* fg */
		    thisjob = job;
		    if ((jobtab[job].stat & STAT_SUPERJOB) &&
			((!jobtab[job].procs->next ||
			  (jobtab[job].stat & STAT_SUBLEADER) ||
			  killpg(jobtab[job].gleader, 0) == -1)) &&
			jobtab[jobtab[job].other].gleader)
			attachtty(jobtab[jobtab[job].other].gleader);
		    else
			attachtty(jobtab[job].gleader);
		}
	    }
	    if (stopped) {
		if (func != BIN_BG && jobtab[job].ty)
		    settyinfo(jobtab[job].ty);
		killjb(jobtab + job, SIGCONT);
	    }
	    if (func == BIN_WAIT)
	    {
		retval = zwaitjob(job, 1);
		if (!retval)
		    retval = lastval2;
	    }
	    else if (func != BIN_BG) {
		/*
		 * HERE: there used not to be an "else" above.  How
		 * could it be right to wait for the foreground job
		 * when we've just been told to wait for another
		 * job (and done it)?
		 */
		waitjobs();
		retval = lastval2;
	    } else if (ofunc == BIN_DISOWN)
	        deletejob(jobtab + job);
	    break;
	case BIN_JOBS:
	    printjob(job + jobtab, lng, 2);
	    break;
	case BIN_DISOWN:
	    if (jobtab[job].stat & STAT_STOPPED) {
		char buf[20], *pids = "";

		if (jobtab[job].stat & STAT_SUPERJOB) {
		    Process pn;

		    for (pn = jobtab[jobtab[job].other].procs; pn; pn = pn->next) {
			sprintf(buf, " -%d", pn->pid);
			pids = dyncat(pids, buf);
		    }
		    for (pn = jobtab[job].procs; pn->next; pn = pn->next) {
			sprintf(buf, " %d", pn->pid);
			pids = dyncat(pids, buf);
		    }
		    if (!jobtab[jobtab[job].other].procs && pn) {
			sprintf(buf, " %d", pn->pid);
			pids = dyncat(pids, buf);
		    }
		} else {
		    sprintf(buf, " -%d", jobtab[job].gleader);
		    pids = buf;
		}
                zwarnnam(name,
#ifdef USE_SUSPENDED
                         "warning: job is suspended, use `kill -CONT%s' to resume",
#else
                         "warning: job is stopped, use `kill -CONT%s' to resume",
#endif
                         pids);
	    }
	    deletejob(jobtab + job);
	    break;
	}
	thisjob = ocj;
    }
    unqueue_signals();
    return retval;
}

const struct {
    const char *name;
    int num;
} alt_sigs[] = {
#if defined(SIGCHLD) && defined(SIGCLD)
#if SIGCHLD == SIGCLD
    { "CLD", SIGCLD },
#endif
#endif
#if defined(SIGPOLL) && defined(SIGIO)
#if SIGPOLL == SIGIO
    { "IO", SIGIO },
#endif
#endif
#if !defined(SIGERR)
    /*
     * If SIGERR is not defined by the operating system, use it
     * as an alias for SIGZERR.
     */
    { "ERR", SIGZERR },
#endif
    { NULL, 0 }
};

/* kill: send a signal to a process.  The process(es) may be specified *
 * by job specifier (see above) or pid.  A signal, defaulting to       *
 * SIGTERM, may be specified by name or number, preceded by a dash.    */

/**/
int
bin_kill(char *nam, char **argv, UNUSED(Options ops), UNUSED(int func))
{
    int sig = SIGTERM;
    int returnval = 0;

    /* check for, and interpret, a signal specifier */
    if (*argv && **argv == '-') {
	if (idigit((*argv)[1]))
	    /* signal specified by number */
	    sig = atoi(*argv + 1);
	else if ((*argv)[1] != '-' || (*argv)[2]) {
	    char *signame;

	    /* with argument "-l" display the list of signal names */
	    if ((*argv)[1] == 'l' && (*argv)[2] == '\0') {
		if (argv[1]) {
		    while (*++argv) {
			sig = zstrtol(*argv, &signame, 10);
			if (signame == *argv) {
			    if (!strncmp(signame, "SIG", 3))
				signame += 3;
			    for (sig = 1; sig <= SIGCOUNT; sig++)
				if (!strcasecmp(sigs[sig], signame))
				    break;
			    if (sig > SIGCOUNT) {
				int i;

				for (i = 0; alt_sigs[i].name; i++)
				    if (!strcasecmp(alt_sigs[i].name, signame))
				    {
					sig = alt_sigs[i].num;
					break;
				    }
			    }
			    if (sig > SIGCOUNT) {
				zwarnnam(nam, "unknown signal: SIG%s",
					 signame);
				returnval++;
			    } else
				printf("%d\n", sig);
			} else {
			    if (*signame) {
				zwarnnam(nam, "unknown signal: SIG%s",
					 signame);
				returnval++;
			    } else {
				if (WIFSIGNALED(sig))
				    sig = WTERMSIG(sig);
				else if (WIFSTOPPED(sig))
				    sig = WSTOPSIG(sig);
				if (1 <= sig && sig <= SIGCOUNT)
				    printf("%s\n", sigs[sig]);
				else
				    printf("%d\n", sig);
			    }
			}
		    }
		    return returnval;
		}
		printf("%s", sigs[1]);
		for (sig = 2; sig <= SIGCOUNT; sig++)
		    printf(" %s", sigs[sig]);
		putchar('\n');
		return 0;
	    }

    	    if ((*argv)[1] == 'n' && (*argv)[2] == '\0') {
	    	char *endp;

	    	if (!*++argv) {
		    zwarnnam(nam, "-n: argument expected");
		    return 1;
		}
		sig = zstrtol(*argv, &endp, 10);
		if (*endp) {
		    zwarnnam(nam, "invalid signal number: %s", signame);
		    return 1;
		}
	    } else {
		if (!((*argv)[1] == 's' && (*argv)[2] == '\0'))
		    signame = *argv + 1;
		else if (!(*++argv)) {
		    zwarnnam(nam, "-s: argument expected");
		    return 1;
		} else
		    signame = *argv;
		if (!*signame) {
		    zwarnnam(nam, "-: signal name expected");
		    return 1;
		}
		signame = casemodify(signame, CASMOD_UPPER);
		if (!strncmp(signame, "SIG", 3))
		    signame+=3;

		/* check for signal matching specified name */
		for (sig = 1; sig <= SIGCOUNT; sig++)
		    if (!strcmp(*(sigs + sig), signame))
			break;
		if (*signame == '0' && !signame[1])
		    sig = 0;
		if (sig > SIGCOUNT) {
		    int i;

		    for (i = 0; alt_sigs[i].name; i++)
			if (!strcmp(alt_sigs[i].name, signame))
			{
			    sig = alt_sigs[i].num;
			    break;
			}
		}
		if (sig > SIGCOUNT) {
		    zwarnnam(nam, "unknown signal: SIG%s", signame);
		    zwarnnam(nam, "type kill -l for a List of signals");
		    return 1;
		}
	    }
	}
	argv++;
    }

    if (!*argv) {
    	zwarnnam(nam, "not enough arguments");
	return 1;
    }

    queue_signals();
    setcurjob();

    /* Remaining arguments specify processes.  Loop over them, and send the
    signal (number sig) to each process. */
    for (; *argv; argv++) {
	if (**argv == '%') {
	    /* job specifier introduced by '%' */
	    int p;

	    if ((p = getjob(*argv, nam)) == -1) {
		returnval++;
		continue;
	    }
	    if (killjb(jobtab + p, sig) == -1) {
		zwarnnam("kill", "kill %s failed: %e", *argv, errno);
		returnval++;
		continue;
	    }
	    /* automatically update the job table if sending a SIGCONT to a
	    job, and send the job a SIGCONT if sending it a non-stopping
	    signal. */
	    if (jobtab[p].stat & STAT_STOPPED) {
		if (sig == SIGCONT)
		    jobtab[p].stat &= ~STAT_STOPPED;
		if (sig != SIGKILL && sig != SIGCONT && sig != SIGTSTP
		    && sig != SIGTTOU && sig != SIGTTIN && sig != SIGSTOP)
		    killjb(jobtab + p, SIGCONT);
	    }
	} else if (!isanum(*argv)) {
	    zwarnnam("kill", "illegal pid: %s", *argv);
	    returnval++;
	} else if (kill(atoi(*argv), sig) == -1) {
	    zwarnnam("kill", "kill %s failed: %e", *argv, errno);
	    returnval++;
	}
    }
    unqueue_signals();

    return returnval < 126 ? returnval : 1;
}
/* Get a signal number from a string */

/**/
mod_export int
getsignum(char *s)
{
    int x, i;

    /* check for a signal specified by number */
    x = atoi(s);
    if (idigit(*s) && x >= 0 && x < VSIGCOUNT)
	return x;

    /* search for signal by name */
    if (!strncmp(s, "SIG", 3))
	s += 3;

    for (i = 0; i < VSIGCOUNT; i++)
	if (!strcmp(s, sigs[i]))
	    return i;

    for (i = 0; alt_sigs[i].name; i++)
    {
	if (!strcmp(s, alt_sigs[i].name))
	    return alt_sigs[i].num;
    }

    /* no matching signal */
    return -1;
}

/* Get the name for a signal. */

/**/
mod_export const char *
getsigname(int sig)
{
    if (sigtrapped[sig] & ZSIG_ALIAS)
    {
	int i;
	for (i = 0; alt_sigs[i].name; i++)
	    if (sig == alt_sigs[i].num)
		return alt_sigs[i].name;
    }
    else
	return sigs[sig];

    /* shouldn't reach here */
#ifdef DEBUG
    dputs("Bad alias flag for signal");
#endif
    return "";
}


/* Get the function node for a trap, taking care about alternative names */
/**/
HashNode
gettrapnode(int sig, int ignoredisable)
{
    char fname[20];
    HashNode hn;
    HashNode (*getptr)(HashTable ht, char *name);
    int i;
    if (ignoredisable)
	getptr = shfunctab->getnode2;
    else
	getptr = shfunctab->getnode;

    sprintf(fname, "TRAP%s", sigs[sig]);
    if ((hn = getptr(shfunctab, fname)))
	return hn;

    for (i = 0; alt_sigs[i].name; i++) {
	if (alt_sigs[i].num == sig) {
	    sprintf(fname, "TRAP%s", alt_sigs[i].name);
	    if ((hn = getptr(shfunctab, fname)))
		return hn;
	}
    }

    return NULL;
}

/* Remove a TRAP function under any name for the signal */

/**/
void
removetrapnode(int sig)
{
    HashNode hn = gettrapnode(sig, 1);
    if (hn) {
	shfunctab->removenode(shfunctab, hn->nam);
	shfunctab->freenode(hn);
    }
}

/* Suspend this shell */

/**/
int
bin_suspend(char *name, UNUSED(char **argv), Options ops, UNUSED(int func))
{
    /* won't suspend a login shell, unless forced */
    if (islogin && !OPT_ISSET(ops,'f')) {
	zwarnnam(name, "can't suspend login shell");
	return 1;
    }
    if (jobbing) {
	/* stop ignoring signals */
	signal_default(SIGTTIN);
	signal_default(SIGTSTP);
	signal_default(SIGTTOU);

	/* Move ourselves back to the process group we came from */
	release_pgrp();
    }

    /* suspend ourselves with a SIGTSTP */
    killpg(origpgrp, SIGTSTP);

    if (jobbing) {
	acquire_pgrp();
	/* restore signal handling */
	signal_ignore(SIGTTOU);
	signal_ignore(SIGTSTP);
	signal_ignore(SIGTTIN);
    }
    return 0;
}

/* find a job named s */

/**/
int
findjobnam(char *s)
{
    int jobnum;

    for (jobnum = maxjob; jobnum >= 0; jobnum--)
	if (!(jobtab[jobnum].stat & (STAT_SUBJOB | STAT_NOPRINT)) &&
	    jobtab[jobnum].stat && jobtab[jobnum].procs && jobnum != thisjob &&
	    jobtab[jobnum].procs->text && strpfx(s, jobtab[jobnum].procs->text))
	    return jobnum;
    return -1;
}


/* make sure we are a process group leader by creating a new process
   group if necessary */

/**/
void
acquire_pgrp(void)
{
    long ttpgrp;
    sigset_t blockset, oldset;

    if ((mypgrp = GETPGRP()) > 0) {
	sigemptyset(&blockset);
	sigaddset(&blockset, SIGTTIN);
	sigaddset(&blockset, SIGTTOU);
	sigaddset(&blockset, SIGTSTP);
	oldset = signal_block(blockset);
	while ((ttpgrp = gettygrp()) != -1 && ttpgrp != mypgrp) {
	    mypgrp = GETPGRP();
	    if (mypgrp == mypid) {
		signal_setmask(oldset);
		attachtty(mypgrp); /* Might generate SIGT* */
		signal_block(blockset);
	    }
	    if (mypgrp == gettygrp())
		break;
	    signal_setmask(oldset);
	    read(0, NULL, 0); /* Might generate SIGT* */
	    signal_block(blockset);
	    mypgrp = GETPGRP();
	}
	if (mypgrp != mypid) {
	    if (setpgrp(0, 0) == 0) {
		mypgrp = mypid;
		attachtty(mypgrp);
	    } else
		opts[MONITOR] = 0;
	}
	signal_setmask(oldset);
    } else
	opts[MONITOR] = 0;
}

/* revert back to the process group we came from (before acquire_pgrp) */

/**/
void
release_pgrp(void)
{
    if (origpgrp != mypgrp) {
	attachtty(origpgrp);
	setpgrp(0, origpgrp);
	mypgrp = origpgrp;
    }
}
