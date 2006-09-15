/*
 * rlimits.c - resource limit builtins
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

#include "rlimits.mdh"
#include "rlimits.pro"

#if defined(HAVE_GETRLIMIT) && defined(RLIM_INFINITY)

enum {
    ZLIMTYPE_MEMORY,
    ZLIMTYPE_NUMBER,
    ZLIMTYPE_TIME,
    ZLIMTYPE_UNKNOWN
};

/* Generated rec array containing limits required for the limit builtin.     *
 * They must appear in this array in numerical order of the RLIMIT_* macros. */

# include "rlimits.h"

static rlim_t
zstrtorlimt(const char *s, char **t, int base)
{
    rlim_t ret = 0;

    if (strcmp(s, "unlimited") == 0) {
	if (t)
	    *t = (char *) s + 9;
	return RLIM_INFINITY;
    }
# if defined(RLIM_T_IS_QUAD_T) || defined(RLIM_T_IS_LONG_LONG) || defined(RLIM_T_IS_UNSIGNED)
    if (!base) {
	if (*s != '0')
	    base = 10;
	else if (*++s == 'x' || *s == 'X')
	    base = 16, s++;
	else
	    base = 8;
    } 
    if (base <= 10)
	for (; *s >= '0' && *s < ('0' + base); s++)
	    ret = ret * base + *s - '0';
    else
	for (; idigit(*s) || (*s >= 'a' && *s < ('a' + base - 10))
	     || (*s >= 'A' && *s < ('A' + base - 10)); s++)
	    ret = ret * base + (idigit(*s) ? (*s - '0') : (*s & 0x1f) + 9);
    if (t)
	*t = (char *)s;
# else /* !RLIM_T_IS_QUAD_T && !RLIM_T_IS_LONG_LONG && !RLIM_T_IS_UNSIGNED */
    ret = zstrtol(s, t, base);
# endif /* !RLIM_T_IS_QUAD_T && !RLIM_T_IS_LONG_LONG && !RLIM_T_IS_UNSIGNED */
    return ret;
}

/**/
static void
showlimitvalue(int lim, rlim_t val)
{
    /* display limit for resource number lim */
    if (lim < ZSH_NLIMITS)
	printf("%-16s", recs[lim]);
    else
    {
	/* Unknown limit, hence unknown units. */
	printf("%-16d", lim);
    }
    if (val == RLIM_INFINITY)
	printf("unlimited\n");
    else if (lim >= ZSH_NLIMITS)
    {
# ifdef RLIM_T_IS_QUAD_T
	printf("%qd\n", val);
# else
#  ifdef RLIM_T_IS_LONG_LONG
	printf("%lld\n", val);
#  else
#   ifdef RLIM_T_IS_UNSIGNED
	printf("%lu\n", val);
#   else
	printf("%ld\n", val);
#   endif /* RLIM_T_IS_UNSIGNED */
#  endif /* RLIM_T_IS_LONG_LONG */
# endif /* RLIM_T_IS_QUAD_T */
    }
    else if (limtype[lim] == ZLIMTYPE_TIME) {
	/* time-type resource -- display as hours, minutes and
	   seconds. */
	printf("%d:%02d:%02d\n", (int)(val / 3600),
	       (int)(val / 60) % 60, (int)(val % 60));
    } else if (limtype[lim] == ZLIMTYPE_NUMBER ||
	       limtype[lim] == ZLIMTYPE_UNKNOWN) {
	/* pure numeric resource */
	printf("%d\n", (int)val);
    } else if (val >= 1024L * 1024L)
	/* memory resource -- display with `K' or `M' modifier */
# ifdef RLIM_T_IS_QUAD_T
	printf("%qdMB\n", val / (1024L * 1024L));
    else
	printf("%qdkB\n", val / 1024L);
# else
#  ifdef RLIM_T_IS_LONG_LONG
    printf("%lldMB\n", val / (1024L * 1024L));
    else
	printf("%lldkB\n", val / 1024L);
#  else
#   ifdef RLIM_T_IS_UNSIGNED
    printf("%luMB\n", val / (1024L * 1024L));
    else
	printf("%lukB\n", val / 1024L);
#   else
    printf("%ldMB\n", val / (1024L * 1024L));
    else
	printf("%ldkB\n", val / 1024L);
#   endif /* RLIM_T_IS_UNSIGNED */
#  endif /* RLIM_T_IS_LONG_LONG */
# endif /* RLIM_T_IS_QUAD_T */
}

/* Display resource limits.  hard indicates whether `hard' or `soft'  *
 * limits should be displayed.  lim specifies the limit, or may be -1 *
 * to show all.                                                       */

/**/
static int
showlimits(char *nam, int hard, int lim)
{
    int rt;

    if (lim >= ZSH_NLIMITS)
    {
	/*
	 * Not configured into the shell.  Ask the OS
	 * explicitly for this limit.
	 */
	struct rlimit vals;
	if (getrlimit(lim, &vals) < 0)
	{
	    zwarnnam(nam, "can't read limit: %e", errno);
	    return 1;
	}
	showlimitvalue(lim, hard ? vals.rlim_max : vals.rlim_cur);
    }
    else if (lim != -1)
    {
	showlimitvalue(lim, hard ? limits[lim].rlim_max :
		       limits[lim].rlim_cur);
    }
    else
    {
	/* main loop over resource types */
	for (rt = 0; rt != ZSH_NLIMITS; rt++)
	    showlimitvalue(rt, (hard) ? limits[rt].rlim_max :
			   limits[rt].rlim_cur);
    }

    return 0;
}

/* Display a resource limit, in ulimit style.  lim specifies which   *
 * limit should be displayed, and hard indicates whether the hard or *
 * soft limit should be displayed.                                   */

/**/
static int
printulimit(char *nam, int lim, int hard, int head)
{
    rlim_t limit;

    /* get the limit in question */
    if (lim >= ZSH_NLIMITS)
    {
	struct rlimit vals;

	if (getrlimit(lim, &vals) < 0)
	{
	    zwarnnam(nam, "can't read limit: %e", errno);
	    return 1;
	}
	limit = (hard) ? vals.rlim_max : vals.rlim_cur;
    }
    else
	limit = (hard) ? limits[lim].rlim_max : limits[lim].rlim_cur;
    /* display the appropriate heading */
    switch (lim) {
    case RLIMIT_CORE:
	if (head)
	    printf("-c: core file size (blocks)    ");
	if (limit != RLIM_INFINITY)
	    limit /= 512;
	break;
    case RLIMIT_DATA:
	if (head)
	    printf("-d: data seg size (kbytes)     ");
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
    case RLIMIT_FSIZE:
	if (head)
	    printf("-f: file size (blocks)         ");
	if (limit != RLIM_INFINITY)
	    limit /= 512;
	break;
# ifdef HAVE_RLIMIT_SIGPENDING
    case RLIMIT_SIGPENDING:
	if (head)
	    printf("-i: pending signals            ");
	break;
# endif
# ifdef HAVE_RLIMIT_MEMLOCK
    case RLIMIT_MEMLOCK:
	if (head)
	    printf("-l: locked-in-memory size (kb) ");
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
# endif /* HAVE_RLIMIT_MEMLOCK */
/* If RLIMIT_VMEM and RLIMIT_RSS are defined and equal, avoid *
 * duplicate case statement.  Observed on QNX Neutrino 6.1.0. */
# if defined(HAVE_RLIMIT_RSS) && !defined(RLIMIT_VMEM_IS_RSS) && !defined(RLIMIT_RSS_IS_AS)
    case RLIMIT_RSS:
	if (head)
	    printf("-m: resident set size (kbytes) ");
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
# endif /* HAVE_RLIMIT_RSS */
# if defined(HAVE_RLIMIT_VMEM) && defined(HAVE_RLIMIT_RSS) && defined(RLIMIT_VMEM_IS_RSS)
    case RLIMIT_VMEM:
	if (head)
	    printf("-m: memory size (kb)           ");
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
# endif /* HAVE_RLIMIT_VMEM */
# ifdef HAVE_RLIMIT_NOFILE
    case RLIMIT_NOFILE:
	if (head)
	    printf("-n: file descriptors           ");
	break;
# endif /* HAVE_RLIMIT_NOFILE */
# ifdef HAVE_RLIMIT_MSGQUEUE
    case RLIMIT_MSGQUEUE:
	if (head)
	    printf("-q: bytes in POSIX msg queues  ");
	break;
# endif
    case RLIMIT_STACK:
	if (head)
	    printf("-s: stack size (kbytes)        ");
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
    case RLIMIT_CPU:
	if (head)
	    printf("-t: cpu time (seconds)         ");
	break;
# ifdef HAVE_RLIMIT_NPROC
    case RLIMIT_NPROC:
	if (head)
	    printf("-u: processes                  ");
	break;
# endif /* HAVE_RLIMIT_NPROC */
# if defined(HAVE_RLIMIT_VMEM) && (!defined(HAVE_RLIMIT_RSS) || !defined(RLIMIT_VMEM_IS_RSS))
    case RLIMIT_VMEM:
	if (head)
	    printf("-v: virtual memory size (kb)   ");
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
# endif /* HAVE_RLIMIT_VMEM */
# if defined HAVE_RLIMIT_AS && !defined(RLIMIT_VMEM_IS_AS)
    case RLIMIT_AS:
	if (head)
	    printf("-v: address space (kb)         ");
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
# endif /* HAVE_RLIMIT_AS */
# ifdef HAVE_RLIMIT_LOCKS
    case RLIMIT_LOCKS:
	if (head)
	    printf("-x: file locks                 ");
	break;
# endif /* HAVE_RLIMIT_LOCKS */
# ifdef HAVE_RLIMIT_AIO_MEM
    case RLIMIT_AIO_MEM:
	if (head)
	    printf("-N %2d: AIO locked-in-memory (kb) ", RLIMIT_AIO_MEM);
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
# endif /* HAVE_RLIMIT_AIO_MEM */
# ifdef HAVE_RLIMIT_AIO_OPS
    case RLIMIT_AIO_OPS:
	if (head)
	    printf("-N %2d: AIO operations          ", RLIMIT_AIO_OPS);
	break;
# endif /* HAVE_RLIMIT_AIO_OPS */
# ifdef HAVE_RLIMIT_TCACHE
    case RLIMIT_TCACHE:
	if (head)
	    printf("-N %2d: cached threads          ", RLIMIT_TCACHE);
	break;
# endif /* HAVE_RLIMIT_TCACHE */
# ifdef HAVE_RLIMIT_SBSIZE
    case RLIMIT_SBSIZE:
	if (head)
	    printf("-N %2d: socket buffer size (kb) ", RLIMIT_SBSIZE);
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
# endif /* HAVE_RLIMIT_SBSIZE */
# ifdef HAVE_RLIMIT_PTHREAD
    case RLIMIT_PTHREAD:
	if (head)
	    printf("-N %2d: threads per process     ", RLIMIT_PTHREAD);
	break;
# endif /* HAVE_RLIMIT_PTHREAD */
# ifdef HAVE_RLIMIT_NICE
    case RLIMIT_NICE:
	if (head)
	    printf("-e: max nice                   ");
	break;
# endif /* HAVE_RLIMIT_NICE */
# ifdef HAVE_RLIMIT_RTPRIO
    case RLIMIT_RTPRIO:
	if (head)
	    printf("-r: max rt priority            ");
	break;
# endif /* HAVE_RLIMIT_RTPRIO */
    default:
	if (head)
	    printf("-N %2d:                         ", lim);
	break;
    }
    /* display the limit */
    if (limit == RLIM_INFINITY)
	printf("unlimited\n");
    else {
# ifdef RLIM_T_IS_QUAD_T
	printf("%qd\n", limit);
# else
#  ifdef RLIM_T_IS_LONG_LONG
	printf("%lld\n", limit);
#  else
#   ifdef RLIM_T_IS_UNSIGNED
	printf("%lu\n", limit);
#   else
	printf("%ld\n", limit);
#   endif /* RLIM_T_IS_UNSIGNED */
#  endif /* RLIM_T_IS_LONG_LONG */
# endif /* RLIM_T_IS_QUAD_T */
    }

    return 0;
}

/**/
static int
do_limit(char *nam, int lim, rlim_t val, int hard, int soft, int set)
{
    if (lim >= ZSH_NLIMITS) {
	struct rlimit vals;
	if (getrlimit(lim, &vals) < 0)
	{
	    /* best guess about error */
	    zwarnnam(nam, "can't read limit: %e", errno);
	    return 1;
	}
	if (hard)
	{
	    if (val > vals.rlim_max && geteuid()) {
		zwarnnam(nam, "can't raise hard limits");
		return 1;
	    }
	    vals.rlim_max = val;
	    /*
	     * not show if all systems will do this silently, but
	     * best be safe...
	     */
	    if (val < vals.rlim_cur)
		vals.rlim_cur = val;
	}
	if (soft || !hard) {
	    if (val > vals.rlim_max) {
		zwarnnam(nam, "limit exceeds hard limit");
		return 1;
	    }
	    else
		vals.rlim_cur = val;
	}
	if (!set)
	{
	    zwarnnam(nam,
		     "warning: unrecognised limit %d, use -s to set",
		     lim);
	    return 1;
	}
	else if (setrlimit(lim, &vals) < 0)
	{
	    zwarnnam(nam, "setrlimit failed: %e", errno);
	    return 1;
	}
    } else {
	/* new limit is valid and has been interpreted; apply it to the
	specified resource */
	if (hard) {
	    /* can only raise hard limits if running as root */
	    if (val > current_limits[lim].rlim_max && geteuid()) {
		zwarnnam(nam, "can't raise hard limits");
		return 1;
	    } else {
		limits[lim].rlim_max = val;
		if (val < limits[lim].rlim_cur)
		    limits[lim].rlim_cur = val;
	    }
	}
	if (soft || !hard) {
	    if (val > limits[lim].rlim_max) {
		/* no idea about this difference, don't intend to worry */
		if (*nam == 'u')
		{
		    /* ulimit does this */
		    if (val > current_limits[lim].rlim_max && geteuid()) {
			zwarnnam(nam, "value exceeds hard limit");
			return 1;
		    }
		    limits[lim].rlim_max = limits[lim].rlim_cur = val;
		} else {
		    /* but limit does this */
		    zwarnnam(nam, "limit exceeds hard limit");
		    return 1;
		}
	    } else
		limits[lim].rlim_cur = val;
	    if (set && zsetlimit(lim, "limit"))
		return 1;
	}
    }
    return 0;
}

/* limit: set or show resource limits.  The variable hard indicates *
 * whether `hard' or `soft' resource limits are being set/shown.    */

/**/
static int
bin_limit(char *nam, char **argv, Options ops, UNUSED(int func))
{
    char *s;
    int hard, limnum, lim;
    rlim_t val;
    int ret = 0;

    hard = OPT_ISSET(ops,'h');
    if (OPT_ISSET(ops,'s') && !*argv)
	return setlimits(NULL);
    /* without arguments, display limits */
    if (!*argv)
	return showlimits(nam, hard, -1);
    while ((s = *argv++)) {
	/* Search for the appropriate resource name.  When a name matches (i.e. *
	 * starts with) the argument, the lim variable changes from -1 to the   *
	 * number of the resource.  If another match is found, lim goes to -2.  */
	if (idigit(*s))
	{
	    lim = (int)zstrtol(s, NULL, 10);
	}
	else
	    for (lim = -1, limnum = 0; limnum < ZSH_NLIMITS; limnum++)
		if (!strncmp(recs[limnum], s, strlen(s))) {
		    if (lim != -1)
			lim = -2;
		    else
			lim = limnum;
		}
	/* lim==-1 indicates that no matches were found.       *
	 * lim==-2 indicates that multiple matches were found. */
	if (lim < 0) {
	    zwarnnam(nam,
		     (lim == -2) ? "ambiguous resource specification: %s"
		     : "no such resource: %s", s);
	    return 1;
	}
	/* without value for limit, display the current limit */
	if (!(s = *argv++))
	    return showlimits(nam, hard, lim);
	if (lim >= ZSH_NLIMITS)
	{
	    val = zstrtorlimt(s, &s, 10);
	    if (*s)
	    {
		/* unknown limit, no idea how to scale */
		zwarnnam(nam, "unknown scaling factor: %s", s);
		return 1;
	    }
	}
	else if (limtype[lim] == ZLIMTYPE_TIME) {
	    /* time-type resource -- may be specified as seconds, or minutes or *
	     * hours with the `m' and `h' modifiers, and `:' may be used to add *
	     * together more than one of these.  It's easier to understand from *
	     * the code:                                                        */
	    val = zstrtorlimt(s, &s, 10);
	    if (*s) {
		if ((*s == 'h' || *s == 'H') && !s[1])
		    val *= 3600L;
		else if ((*s == 'm' || *s == 'M') && !s[1])
		    val *= 60L;
		else if (*s == ':')
		    val = val * 60 + zstrtorlimt(s + 1, &s, 10);
		else {
		    zwarnnam(nam, "unknown scaling factor: %s", s);
		    return 1;
		}
	    }
	} else if (limtype[lim] == ZLIMTYPE_NUMBER || limtype[lim] == ZLIMTYPE_UNKNOWN) {
	    /* pure numeric resource -- only a straight decimal number is
	    permitted. */
	    char *t = s;
	    val = zstrtorlimt(t, &s, 10);
	    if (s == t) {
		zwarnnam(nam, "limit must be a number");
		return 1;
	    }
	} else {
	    /* memory-type resource -- `k' and `M' modifiers are permitted,
	    meaning (respectively) 2^10 and 2^20. */
	    val = zstrtorlimt(s, &s, 10);
	    if (!*s || ((*s == 'k' || *s == 'K') && !s[1])) {
		if (val != RLIM_INFINITY)
		    val *= 1024L;
	    } else if ((*s == 'M' || *s == 'm') && !s[1])
		val *= 1024L * 1024;
	    else {
		zwarnnam(nam, "unknown scaling factor: %s", s);
		return 1;
	    }
	}
	if (do_limit(nam, lim, val, hard, !hard, OPT_ISSET(ops, 's')))
	    ret++;
    }
    return ret;
}

/**/
static int
do_unlimit(char *nam, int lim, int hard, int soft, int set, int euid)
{
    /* remove specified limit */
    if (lim >= ZSH_NLIMITS) {
	struct rlimit vals;
	if (getrlimit(lim, &vals) < 0)
	{
	    zwarnnam(nam, "can't read limit: %e", errno);
	    return 1;
	}
	if (hard) {
	    if (euid && vals.rlim_max != RLIM_INFINITY) {
		zwarnnam(nam, "can't remove hard limits");
		return 1;
	    } else
		vals.rlim_max = RLIM_INFINITY;
	}
	if (!hard || soft)
	    vals.rlim_cur = vals.rlim_max;
	if (!set) {
	    zwarnnam(nam,
		     "warning: unrecognised limit %d, use -s to set", lim);
	    return 1;
	} else if (setrlimit(lim, &vals) < 0) {
	    zwarnnam(nam, "setrlimit failed: %e", errno);
	    return 1;
	}
    } else {
	if (hard) {
	    if (euid && current_limits[lim].rlim_max != RLIM_INFINITY) {
		zwarnnam(nam, "can't remove hard limits");
		return 1;
	    } else
		limits[lim].rlim_max = RLIM_INFINITY;
	}
	if (!hard || soft)
	    limits[lim].rlim_cur = limits[lim].rlim_max;
	if (set && zsetlimit(lim, nam))
	    return 1;
    }
    return 0;
}

/* unlimit: remove resource limits.  Much of this code is the same as *
 * that in bin_limit().                                               */

/**/
static int
bin_unlimit(char *nam, char **argv, Options ops, UNUSED(int func))
{
    int hard, limnum, lim;
    int ret = 0;
    uid_t euid = geteuid();

    hard = OPT_ISSET(ops,'h');
    /* Without arguments, remove all limits. */
    if (!*argv) {
	for (limnum = 0; limnum != RLIM_NLIMITS; limnum++) {
	    if (hard) {
		if (euid && current_limits[limnum].rlim_max != RLIM_INFINITY)
		    ret++;
		else
		    limits[limnum].rlim_max = RLIM_INFINITY;
	    } else
		limits[limnum].rlim_cur = limits[limnum].rlim_max;
	}
	if (OPT_ISSET(ops,'s'))
	    ret += setlimits(nam);
	if (ret)
	    zwarnnam(nam, "can't remove hard limits");
    } else {
	for (; *argv; argv++) {
	    /* Search for the appropriate resource name.  When a name     *
	     * matches (i.e. starts with) the argument, the lim variable  *
	     * changes from -1 to the number of the resource.  If another *
	     * match is found, lim goes to -2.                            */
	    if (idigit(**argv)) {
		lim = (int)zstrtol(*argv, NULL, 10);
	    } else {
		for (lim = -1, limnum = 0; limnum < ZSH_NLIMITS; limnum++)
		    if (!strncmp(recs[limnum], *argv, strlen(*argv))) {
			if (lim != -1)
			    lim = -2;
			else
			    lim = limnum;
		    }
	    }
	    /* lim==-1 indicates that no matches were found.       *
	     * lim==-2 indicates that multiple matches were found. */
	    if (lim < 0) {
		zwarnnam(nam,
			 (lim == -2) ? "ambiguous resource specification: %s"
			 : "no such resource: %s", *argv);
		return 1;
	    }
	    else if (do_unlimit(nam, lim, hard, !hard, OPT_ISSET(ops, 's'),
				euid))
		ret++;
	}
    }
    return ret;
}

/* ulimit: set or display resource limits */

/**/
static int
bin_ulimit(char *name, char **argv, UNUSED(Options ops), UNUSED(int func))
{
    int res, resmask = 0, hard = 0, soft = 0, nres = 0, all = 0, ret = 0;
    char *options;

    do {
	options = *argv;
	if (options && *options == '-' && !options[1]) {
	    zwarnnam(name, "missing option letter");
	    return 1;
	}
	res = -1;
	if (options && *options == '-') {
	    argv++;
	    while (*++options) {
		if(*options == Meta)
		    *++options ^= 32;
		res = -1;
		switch (*options) {
		case 'H':
		    hard = 1;
		    continue;
		case 'S':
		    soft = 1;
		    continue;
		case 'N':
		    if (options[1]) {
			res = (int)zstrtol(options+1, NULL, 10);
		    } else if (*argv) {
			res = (int)zstrtol(*argv++, NULL, 10);
		    } else {
			zwarnnam(name, "number required after -N");
			return 1;
		    }
		    /*
		     * fake it so it looks like we just finished an option...
		     */
		    while (options[1])
			options++;
		    break;
		case 'a':
		    if (resmask) {
			zwarnnam(name, "no limits allowed with -a");
			return 1;
		    }
		    all = 1;
		    resmask = (1 << RLIM_NLIMITS) - 1;
		    nres = RLIM_NLIMITS;
		    continue;
		case 't':
		    res = RLIMIT_CPU;
		    break;
		case 'f':
		    res = RLIMIT_FSIZE;
		    break;
		case 'd':
		    res = RLIMIT_DATA;
		    break;
		case 's':
		    res = RLIMIT_STACK;
		    break;
		case 'c':
		    res = RLIMIT_CORE;
		    break;
# ifdef HAVE_RLIMIT_RSS
		case 'm':
		    res = RLIMIT_RSS;
		    break;
# endif /* HAVE_RLIMIT_RSS */
# ifdef HAVE_RLIMIT_MEMLOCK
		case 'l':
		    res = RLIMIT_MEMLOCK;
		    break;
# endif /* HAVE_RLIMIT_MEMLOCK */
# ifdef HAVE_RLIMIT_NOFILE
		case 'n':
		    res = RLIMIT_NOFILE;
		    break;
# endif /* HAVE_RLIMIT_NOFILE */
# ifdef HAVE_RLIMIT_NPROC
		case 'u':
		    res = RLIMIT_NPROC;
		    break;
# endif /* HAVE_RLIMIT_NPROC */
# if defined(HAVE_RLIMIT_VMEM) || defined(HAVE_RLIMIT_AS)
		case 'v':
#  ifdef HAVE_RLIMIT_VMEM
		    res = RLIMIT_VMEM;
#  else
		    res = RLIMIT_AS;
#  endif
		    break;
# endif /* HAVE_RLIMIT_VMEM */
# ifdef HAVE_RLIMIT_LOCKS
		case 'x':
		    res = RLIMIT_LOCKS;
		    break;
# endif
# ifdef HAVE_RLIMIT_SIGPENDING
		case 'i':
		    res = RLIMIT_SIGPENDING;
		    break;
# endif
# ifdef HAVE_RLIMIT_MSGQUEUE
		case 'q':
		    res = RLIMIT_MSGQUEUE;
		    break;
# endif
# ifdef HAVE_RLIMIT_NICE
		case 'e':
		    res = RLIMIT_NICE;
		    break;
# endif
# ifdef HAVE_RLIMIT_RTPRIO
		case 'r':
		    res = RLIMIT_RTPRIO;
		    break;
# endif
		default:
		    /* unrecognised limit */
		    zwarnnam(name, "bad option: -%c", *options);
		    return 1;
		}
		if (options[1]) {
		    resmask |= 1 << res;
		    nres++;
		}
		if (all && res != -1) {
		    zwarnnam(name, "no limits allowed with -a");
		    return 1;
		}
	    }
	}
	if (!*argv || **argv == '-') {
	    if (res < 0) {
		if (*argv || nres)
		    continue;
		else
		    res = RLIMIT_FSIZE;
	    }
	    resmask |= 1 << res;
	    nres++;
	    continue;
	}
	if (all) {
	    zwarnnam(name, "no arguments allowed after -a");
	    return 1;
	}
	if (res < 0)
	    res = RLIMIT_FSIZE;
	if (strcmp(*argv, "unlimited")) {
	    /* set limit to specified value */
	    rlim_t limit;

	    limit = zstrtorlimt(*argv, NULL, 10);
	    /* scale appropriately */
	    switch (res) {
	    case RLIMIT_FSIZE:
	    case RLIMIT_CORE:
		limit *= 512;
		break;
	    case RLIMIT_DATA:
	    case RLIMIT_STACK:
# ifdef HAVE_RLIMIT_RSS
	    case RLIMIT_RSS:
# endif /* HAVE_RLIMIT_RSS */
# ifdef HAVE_RLIMIT_MEMLOCK
	    case RLIMIT_MEMLOCK:
# endif /* HAVE_RLIMIT_MEMLOCK */
/* If RLIMIT_VMEM and RLIMIT_RSS are defined and equal, avoid *
 * duplicate case statement.  Observed on QNX Neutrino 6.1.0. */
# if defined(HAVE_RLIMIT_VMEM) && !defined(RLIMIT_VMEM_IS_RSS)
	    case RLIMIT_VMEM:
# endif /* HAVE_RLIMIT_VMEM */
/* ditto RLIMIT_VMEM and RLIMIT_AS */
# if defined(HAVE_RLIMIT_AS) && !defined(RLIMIT_VMEM_IS_AS) && !defined(RLIMIT_RSS_IS_AS)
	    case RLIMIT_AS:
# endif /* HAVE_RLIMIT_AS */
# ifdef HAVE_RLIMIT_AIO_MEM
	    case RLIMIT_AIO_MEM:
# endif /* HAVE_RLIMIT_AIO_MEM */
		limit *= 1024;
		break;
	    }
	    if (do_limit(name, res, limit, hard, soft, 1))
		ret++;
	} else {
	    if (do_unlimit(name, res, hard, soft, 1, geteuid()))
		ret++;
	}
	argv++;
    } while (*argv);
    for (res = 0; resmask; res++, resmask >>= 1)
	if ((resmask & 1) && printulimit(name, res, hard, nres > 1))
	    ret++;
    return ret;
}

#else /* !HAVE_GETRLIMIT || !RLIM_INFINITY */

# define bin_limit   bin_notavail
# define bin_ulimit  bin_notavail
# define bin_unlimit bin_notavail

#endif /* !HAVE_GETRLIMIT || !RLIM_INFINITY */

static struct builtin bintab[] = {
    BUILTIN("limit",   0, bin_limit,   0, -1, 0, "sh", NULL),
    BUILTIN("ulimit",  0, bin_ulimit,  0, -1, 0, NULL, NULL),
    BUILTIN("unlimit", 0, bin_unlimit, 0, -1, 0, "hs", NULL),
};

/**/
int
setup_(UNUSED(Module m))
{
    return 0;
}

/**/
int
boot_(Module m)
{
    return !addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
}

/**/
int
cleanup_(Module m)
{
    deletebuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
    return 0;
}

/**/
int
finish_(UNUSED(Module m))
{
    return 0;
}
