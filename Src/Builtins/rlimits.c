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

/* Display resource limits.  hard indicates whether `hard' or `soft'  *
 * limits should be displayed.  lim specifies the limit, or may be -1 *
 * to show all.                                                       */

/**/
static void
showlimits(int hard, int lim)
{
    int rt;
    rlim_t val;

    /* main loop over resource types */
    for (rt = 0; rt != ZSH_NLIMITS; rt++)
	if (rt == lim || lim == -1) {
	    /* display limit for resource number rt */
	    printf("%-16s", recs[rt]);
	    val = (hard) ? limits[rt].rlim_max : limits[rt].rlim_cur;
	    if (val == RLIM_INFINITY)
		printf("unlimited\n");
	    else if (limtype[rt] == ZLIMTYPE_TIME) {
		/* time-type resource -- display as hours, minutes and
		seconds. */
		printf("%d:%02d:%02d\n", (int)(val / 3600),
		       (int)(val / 60) % 60, (int)(val % 60));
	    } else if (limtype[rt] == ZLIMTYPE_NUMBER || limtype[rt] == ZLIMTYPE_UNKNOWN) {
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
}

/* Display a resource limit, in ulimit style.  lim specifies which   *
 * limit should be displayed, and hard indicates whether the hard or *
 * soft limit should be displayed.                                   */

/**/
static void
printulimit(int lim, int hard, int head)
{
    rlim_t limit;

    /* get the limit in question */
    limit = (hard) ? limits[lim].rlim_max : limits[lim].rlim_cur;
    /* display the appropriate heading */
    switch (lim) {
    case RLIMIT_CPU:
	if (head)
	    printf("cpu time (seconds)         ");
	break;
    case RLIMIT_FSIZE:
	if (head)
	    printf("file size (blocks)         ");
	if (limit != RLIM_INFINITY)
	    limit /= 512;
	break;
    case RLIMIT_DATA:
	if (head)
	    printf("data seg size (kbytes)     ");
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
    case RLIMIT_STACK:
	if (head)
	    printf("stack size (kbytes)        ");
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
    case RLIMIT_CORE:
	if (head)
	    printf("core file size (blocks)    ");
	if (limit != RLIM_INFINITY)
	    limit /= 512;
	break;
/* If RLIMIT_VMEM and RLIMIT_RSS are defined and equal, avoid *
 * duplicate case statement.  Observed on QNX Neutrino 6.1.0. */
# if defined(HAVE_RLIMIT_RSS) && !defined(RLIMIT_VMEM_IS_RSS)
    case RLIMIT_RSS:
	if (head)
	    printf("resident set size (kbytes) ");
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
# endif /* HAVE_RLIMIT_RSS */
# ifdef HAVE_RLIMIT_MEMLOCK
    case RLIMIT_MEMLOCK:
	if (head)
	    printf("locked-in-memory size (kb) ");
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
# endif /* HAVE_RLIMIT_MEMLOCK */
# ifdef HAVE_RLIMIT_NPROC
    case RLIMIT_NPROC:
	if (head)
	    printf("processes                  ");
	break;
# endif /* HAVE_RLIMIT_NPROC */
# ifdef HAVE_RLIMIT_NOFILE
    case RLIMIT_NOFILE:
	if (head)
	    printf("file descriptors           ");
	break;
# endif /* HAVE_RLIMIT_NOFILE */
# ifdef HAVE_RLIMIT_VMEM
    case RLIMIT_VMEM:
	if (head)
#  if defined(HAVE_RLIMIT_RSS) && defined(RLIMIT_VMEM_IS_RSS)
	    printf("memory size (kb)           ");
#  else
	    printf("virtual memory size (kb)   ");
#  endif
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
# endif /* HAVE_RLIMIT_VMEM */
# if defined HAVE_RLIMIT_AS && !defined(RLIMIT_VMEM_IS_AS)
    case RLIMIT_AS:
	if (head)
	    printf("address space (kb)         ");
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
# endif /* HAVE_RLIMIT_AS */
# ifdef HAVE_RLIMIT_TCACHE
    case RLIMIT_TCACHE:
	if (head)
	    printf("cached threads             ");
	break;
# endif /* HAVE_RLIMIT_TCACHE */
# ifdef HAVE_RLIMIT_AIO_OPS
    case RLIMIT_AIO_OPS:
	if (head)
	    printf("AIO operations             ");
	break;
# endif /* HAVE_RLIMIT_AIO_OPS */
# ifdef HAVE_RLIMIT_AIO_MEM
    case RLIMIT_AIO_MEM:
	if (head)
	    printf("AIO locked-in-memory (kb)  ");
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
# endif /* HAVE_RLIMIT_AIO_MEM */
# ifdef HAVE_RLIMIT_SBSIZE
    case RLIMIT_SBSIZE:
	if (head)
	    printf("socket buffer size (kb)    ");
	if (limit != RLIM_INFINITY)
	    limit /= 1024;
	break;
# endif /* HAVE_RLIMIT_SBSIZE */
# ifdef HAVE_RLIMIT_PTHREAD
    case RLIMIT_PTHREAD:
	if (head)
	    printf("threads per process        ");
	break;
# endif /* HAVE_RLIMIT_PTHREAD */
# ifdef HAVE_RLIMIT_LOCKS
    case RLIMIT_LOCKS:
	if (head)
	    printf("file locks                 ");
	break;
# endif /* HAVE_RLIMIT_LOCKS */
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
}

/* limit: set or show resource limits.  The variable hard indicates *
 * whether `hard' or `soft' resource limits are being set/shown.    */

/**/
static int
bin_limit(char *nam, char **argv, Options ops, int func)
{
    char *s;
    int hard, limnum, lim;
    rlim_t val;
    int ret = 0;

    hard = OPT_ISSET(ops,'h');
    if (OPT_ISSET(ops,'s') && !*argv)
	return setlimits(NULL);
    /* without arguments, display limits */
    if (!*argv) {
	showlimits(hard, -1);
	return 0;
    }
    while ((s = *argv++)) {
	/* Search for the appropriate resource name.  When a name matches (i.e. *
	 * starts with) the argument, the lim variable changes from -1 to the   *
	 * number of the resource.  If another match is found, lim goes to -2.  */
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
	    zwarnnam("limit",
		     (lim == -2) ? "ambiguous resource specification: %s"
		     : "no such resource: %s", s, 0);
	    return 1;
	}
	/* without value for limit, display the current limit */
	if (!(s = *argv++)) {
	    showlimits(hard, lim);
	    return 0;
	}
	if (limtype[lim] == ZLIMTYPE_TIME) {
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
		    zwarnnam("limit", "unknown scaling factor: %s", s, 0);
		    return 1;
		}
	    }
	} else if (limtype[lim] == ZLIMTYPE_NUMBER || limtype[lim] == ZLIMTYPE_UNKNOWN) {
	    /* pure numeric resource -- only a straight decimal number is
	    permitted. */
	    char *t = s;
	    val = zstrtorlimt(t, &s, 10);
	    if (s == t) {
		zwarnnam("limit", "limit must be a number", NULL, 0);
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
		zwarnnam("limit", "unknown scaling factor: %s", s, 0);
		return 1;
	    }
	}
	/* new limit is valid and has been interpreted; apply it to the
	specified resource */
	if (hard) {
	    /* can only raise hard limits if running as root */
	    if (val > current_limits[lim].rlim_max && geteuid()) {
		zwarnnam("limit", "can't raise hard limits", NULL, 0);
		return 1;
	    } else {
		limits[lim].rlim_max = val;
		if (val < limits[lim].rlim_cur)
		    limits[lim].rlim_cur = val;
	    }
	} else if (val > limits[lim].rlim_max) {
	    zwarnnam("limit", "limit exceeds hard limit", NULL, 0);
	    return 1;
	} else
	    limits[lim].rlim_cur = val;
	if (OPT_ISSET(ops,'s') && zsetlimit(lim, "limit"))
	    ret++;
    }
    return ret;
}

/* unlimit: remove resource limits.  Much of this code is the same as *
 * that in bin_limit().                                               */

/**/
static int
bin_unlimit(char *nam, char **argv, Options ops, int func)
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
	    zwarnnam(nam, "can't remove hard limits", NULL, 0);
    } else {
	for (; *argv; argv++) {
	    /* Search for the appropriate resource name.  When a name     *
	     * matches (i.e. starts with) the argument, the lim variable  *
	     * changes from -1 to the number of the resource.  If another *
	     * match is found, lim goes to -2.                            */
	    for (lim = -1, limnum = 0; limnum < ZSH_NLIMITS; limnum++)
		if (!strncmp(recs[limnum], *argv, strlen(*argv))) {
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
			 : "no such resource: %s", *argv, 0);
		return 1;
	    }
	    /* remove specified limit */
	    if (hard) {
		if (euid && current_limits[lim].rlim_max != RLIM_INFINITY) {
		    zwarnnam(nam, "can't remove hard limits", NULL, 0);
		    ret++;
		} else
		    limits[lim].rlim_max = RLIM_INFINITY;
	    } else
		limits[lim].rlim_cur = limits[lim].rlim_max;
	    if (OPT_ISSET(ops,'s') && zsetlimit(lim, nam))
		ret++;
	}
    }
    return ret;
}

/* ulimit: set or display resource limits */

/**/
static int
bin_ulimit(char *name, char **argv, Options ops, int func)
{
    int res, resmask = 0, hard = 0, soft = 0, nres = 0;
    char *options;

    do {
	options = *argv;
	if (options && *options == '-' && !options[1]) {
	    zwarnnam(name, "missing option letter", NULL, 0);
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
		case 'a':
		    if (*argv || options[1] || resmask) {
			zwarnnam(name, "no arguments required after -a",
				 NULL, 0);
			return 1;
		    }
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
		default:
		    /* unrecognised limit */
		    zwarnnam(name, "bad option: -%c", NULL, *options);
		    return 1;
		}
		if (options[1]) {
		    resmask |= 1 << res;
		    nres++;
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
# if defined(HAVE_RLIMIT_AS) && !defined(RLIMIT_VMEM_IS_AS)
	    case RLIMIT_AS:
# endif /* HAVE_RLIMIT_AS */
# ifdef HAVE_RLIMIT_AIO_MEM
	    case RLIMIT_AIO_MEM:
# endif /* HAVE_RLIMIT_AIO_MEM */
		limit *= 1024;
		break;
	    }
	    if (hard) {
		/* can't raise hard limit unless running as root */
		if (limit > current_limits[res].rlim_max && geteuid()) {
		    zwarnnam(name, "can't raise hard limits", NULL, 0);
		    return 1;
		}
		limits[res].rlim_max = limit;
		if (limit < limits[res].rlim_cur)
		    limits[res].rlim_cur = limit;
	    }
	    if (!hard || soft) {
		/* can't raise soft limit above hard limit */
		if (limit > limits[res].rlim_max) {
		    if (limit > current_limits[res].rlim_max && geteuid()) {
			zwarnnam(name, "value exceeds hard limit", NULL, 0);
			return 1;
		    }
		    limits[res].rlim_max = limits[res].rlim_cur = limit;
		} else
		    limits[res].rlim_cur = limit;
	    }
	} else {
	    /* remove specified limit */
	    if (hard) {
		/* can't remove hard limit unless running as root */
		if (current_limits[res].rlim_max != RLIM_INFINITY && geteuid()) {
		    zwarnnam(name, "can't remove hard limits", NULL, 0);
		    return 1;
		}
		limits[res].rlim_max = RLIM_INFINITY;
	    }
	    if (!hard || soft)
		/* `removal' of soft limit means setting it equal to the
		   corresponding hard limit */
		limits[res].rlim_cur = limits[res].rlim_max;
	}
	if (zsetlimit(res, name))
	    return 1;
	argv++;
    } while (*argv);
    for (res = 0; res < RLIM_NLIMITS; res++, resmask >>= 1)
	if (resmask & 1)
	    printulimit(res, hard, nres > 1);
    return 0;
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
setup_(Module m)
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
finish_(Module m)
{
    return 0;
}
