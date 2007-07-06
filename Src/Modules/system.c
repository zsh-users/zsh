/*
 * sysread.c - interface to system read/write
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 1998-2003 Peter Stephenson
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Peter Stephenson or the Zsh Development
 * Group be liable to any party for direct, indirect, special, incidental,
 * or consequential damages arising out of the use of this software and
 * its documentation, even if Peter Stephenson, and the Zsh
 * Development Group have been advised of the possibility of such damage.
 *
 * Peter Stephenson and the Zsh Development Group specifically
 * disclaim any warranties, including, but not limited to, the implied
 * warranties of merchantability and fitness for a particular purpose.  The
 * software provided hereunder is on an "as is" basis, and Peter Stephenson
 * and the Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "system.mdh"
#include "system.pro"

#ifdef HAVE_POLL_H
# include <poll.h>
#endif
#if defined(HAVE_POLL) && !defined(POLLIN)
# undef HAVE_POLL
#endif

#define SYSREAD_BUFSIZE	8192

/**/
static int
getposint(char *instr, char *nam)
{
    char *eptr;
    int ret;

    ret = (int)zstrtol(instr, &eptr, 10);
    if (*eptr || ret < 0) {
	zwarnnam(nam, "integer expected: %s", instr);
	return -1;
    }

    return ret;
}


/*
 * Return values of bin_sysread:
 *	0	Successfully read (and written if appropriate)
 *	1	Error in parameters to command
 *	2	Error on read, or polling read fd ) ERRNO set by
 *      3	Error on write			  ) system
 *	4	Timeout on read
 *	5       Zero bytes read, end of file
 */

/**/
static int
bin_sysread(char *nam, char **args, Options ops, UNUSED(int func))
{
    int infd = 0, outfd = -1, bufsize = SYSREAD_BUFSIZE, count;
    char *outvar = NULL, *countvar = NULL, *inbuf;

    /* -i: input file descriptor if not stdin */
    if (OPT_ISSET(ops, 'i')) {
	infd = getposint(OPT_ARG(ops, 'i'), nam);
	if (infd < 0)
	    return 1;
    }

    /* -o: output file descriptor, else store in REPLY */
    if (OPT_ISSET(ops, 'o')) {
	if (*args) {
	    zwarnnam(nam, "no argument allowed with -o");
	    return 1;
	}
	outfd = getposint(OPT_ARG(ops, 'o'), nam);
	if (outfd < 0)
	    return 1;
    }

    /* -s: buffer size if not default SYSREAD_BUFSIZE */
    if (OPT_ISSET(ops, 's')) {
	bufsize = getposint(OPT_ARG(ops, 's'), nam);
	if (bufsize < 0)
	    return 1;
    }

    /* -c: name of variable to store count of transferred bytes */
    if (OPT_ISSET(ops, 'c')) {
	countvar = OPT_ARG(ops, 'c');
	if (!isident(countvar)) {
	    zwarnnam(nam, "not an identifier: %s", countvar);
	    return 1;
	}
    }

    if (*args) {
	/*
	 * Variable in which to store result if doing a plain read.
	 * Default variable if not specified is REPLY.
	 * If writing, only stuff we couldn't write is stored here,
	 * no default in that case (we just discard it if no variable).
	 */
	outvar = *args;
	if (!isident(outvar)) {
	    zwarnnam(nam, "not an identifier: %s", outvar);
	    return 1;
	}
    }

    inbuf = zhalloc(bufsize);

#if defined(HAVE_POLL) || defined(HAVE_SELECT)
    /* -t: timeout */
    if (OPT_ISSET(ops, 't'))
    {
# ifdef HAVE_POLL
	struct pollfd poll_fd;
	mnumber to_mn;
	int to_int, ret;

	poll_fd.fd = infd;
	poll_fd.events = POLLIN;

	to_mn = matheval(OPT_ARG(ops, 't'));
	if (errflag)
	    return 1;
	if (to_mn.type == MN_FLOAT)
	    to_int = (int) (1000 * to_mn.u.d);
	else
	    to_int = 1000 * (int)to_mn.u.l;

	while ((ret = poll(&poll_fd, 1, to_int)) < 0) {
	    if (errno != EINTR || errflag || retflag || breaks || contflag)
		break;
	}
	if (ret <= 0) {
	    /* treat non-timeout error as error on read */
	    return ret ? 2 : 4;
	}
# else
	/* using select */
	struct timeval select_tv;
	fd_set fds;
	mnumber to_mn;
	int ret;

	FD_ZERO(&fds);
	FD_SET(infd, &fds);
	to_mn = matheval(OPT_ARG(ops, 't'));
	if (errflag)
	    return 1;

	if (to_mn.type == MN_FLOAT) {
	    select_tv.tv_sec = (int) to_mn.u.d;
	    select_tv.tv_usec =
		(int) ((to_mn.u.d - select_tv.tv_sec) * 1e6);
	} else {
	    select_tv.tv_sec = (int) to_mn.u.l;
	    select_tv.tv_usec = 0;
	}

	while ((ret = select(infd+1, (SELECT_ARG_2_T) &fds, 
			     NULL, NULL,&select_tv)) < 1) {
	    if (errno != EINTR || errflag || retflag || breaks || contflag)
		break;
	}
	if (ret <= 0) {
	    /* treat non-timeout error as error on read */
	    return ret ? 2 : 4;
	}
# endif
    }
#endif

    while ((count = read(infd, inbuf, bufsize)) < 0) {
	if (errno != EINTR || errflag || retflag || breaks || contflag)
	    break;
    }
    if (countvar)
	setiparam(countvar, count);
    if (count < 0)
	return 2;

    if (outfd >= 0) {
	if (!count)
	    return 5;
	while (count > 0) {
	    int ret;

	    ret = write(outfd, inbuf, count);
	    if (ret < 0) {
		if (errno == EINTR && !errflag &&
		    !retflag && !breaks && !contflag)
		    continue;
		if (outvar)
		    setsparam(outvar, metafy(inbuf, count, META_DUP));
		if (countvar)
		    setiparam(countvar, count);
		return 3;
	    }
	    inbuf += ret;
	    count -= ret;
	}
	return 0;
    }

    if (!outvar)
	    outvar = "REPLY";
    /* do this even if we read zero bytes */
    setsparam(outvar, metafy(inbuf, count, META_DUP));

    return count ? 0 : 5;
}


/*
 * Return values of bin_syswrite:
 *	0	Successfully written
 *	1	Error in parameters to command
 *	2	Error on write, ERRNO set by system
 */

/**/
static int
bin_syswrite(char *nam, char **args, Options ops, UNUSED(int func))
{
    int outfd = 1, len, count, totcount;
    char *countvar = NULL;

    /* -o: output file descriptor if not stdout */
    if (OPT_ISSET(ops, 'o')) {
	outfd = getposint(OPT_ARG(ops, 'o'), nam);
	if (outfd < 0)
	    return 1;
    }

    /* -c: variable in which to store count of bytes written */
    if (OPT_ISSET(ops, 'c')) {
	countvar = OPT_ARG(ops, 'c');
	if (!isident(countvar)) {
	    zwarnnam(nam, "not an identifier: %s", countvar);
	    return 1;
	}
    }

    totcount = 0;
    unmetafy(*args, &len);
    while (len) {
	while ((count = write(outfd, *args, len)) < 0) {
	    if (errno != EINTR || errflag || retflag || breaks || contflag)
	    {
		if (countvar)
		    setiparam(countvar, totcount);
		return 2;
	    }
	}
	*args += count;
	totcount += count;
	len -= count;
    }
    if (countvar)
	setiparam(countvar, totcount);

    return 0;
}


/*
 * Return values of bin_syserror:
 *	0	Successfully processed error
 *		(although if the number was invalid the string
 *		may not be useful)
 *	1	Error in parameters
 *	2	Name of error not recognised.
 */

/**/
static int
bin_syserror(char *nam, char **args, Options ops, UNUSED(int func))
{
    int num = 0;
    char *errvar = NULL, *msg, *pfx = "", *str;

    /* variable in which to write error message */
    if (OPT_ISSET(ops, 'e')) {
	errvar = OPT_ARG(ops, 'e');
	if (!isident(errvar)) {
	    zwarnnam(nam, "not an identifier: %s", errvar);
	    return 1;
	}
    }
    /* prefix for error message */
    if (OPT_ISSET(ops, 'p'))
	pfx = OPT_ARG(ops, 'p');

    if (!*args)
	num = errno;
    else {
	char *ptr = *args;
	while (*ptr && idigit(*ptr))
	    ptr++;
	if (!*ptr && ptr > *args)
	    num = atoi(*args);
	else {
	    const char **eptr;
	    for (eptr = sys_errnames; *eptr; eptr++) {
		if (!strcmp(*eptr, *args)) {
		    num = (eptr - sys_errnames) + 1;
		    break;
		}
	    }
	    if (!*eptr)
		return 2;
	}
    }

    msg = strerror(num);
    if (errvar) {
	str = (char *)zalloc(strlen(msg) + strlen(pfx) + 1);
	sprintf(str, "%s%s", pfx, msg);
	setsparam(errvar, str);
    } else {
	fprintf(stderr, "%s%s\n", pfx, msg);
    }

    return 0;
}

static struct builtin bintab[] = {
    BUILTIN("syserror", 0, bin_syserror, 0, 1, 0, "e:p:", NULL),
    BUILTIN("sysread", 0, bin_sysread, 0, 1, 0, "c:i:o:s:t:", NULL),
    BUILTIN("syswrite", 0, bin_syswrite, 1, 1, 0, "c:o:", NULL),
};


/* Functions for the errnos special parameter. */

/**/
static char **
errnosgetfn(UNUSED(Param pm))
{
    /* arrdup etc. should really take const pointers as arguments */
    return arrdup((char **)sys_errnames);
}

static const struct gsu_array errnos_gsu =
{ errnosgetfn, arrsetfn, stdunsetfn };


/* Functions for the sysparams special parameter. */

/**/
static void
fillpmsysparams(Param pm, const char *name)
{
    char buf[DIGBUFSIZE];
    int num;

    pm->node.nam = dupstring(name);
    pm->node.flags = PM_SCALAR | PM_READONLY;
    pm->gsu.s = &nullsetscalar_gsu;
    if (!strcmp(name, "pid")) {
	num = (int)getpid();
    } else if (!strcmp(name, "ppid")) {
	num = (int)getppid();
    } else {
	pm->u.str = dupstring("");
	pm->node.flags |= PM_UNSET;
	return;
    }

    sprintf(buf, "%d", num);
    pm->u.str = dupstring(buf);
}


/**/
static HashNode
getpmsysparams(UNUSED(HashTable ht), const char *name)
{
    Param pm;

    pm = (Param) hcalloc(sizeof(struct param));
    fillpmsysparams(pm, name);
    return &pm->node;
}


/**/
static void
scanpmsysparams(UNUSED(HashTable ht), ScanFunc func, int flags)
{
    struct param spm;

    fillpmsysparams(&spm, "pid");
    func(&spm.node, flags);
    fillpmsysparams(&spm, "ppid");
    func(&spm.node, flags);
}


static struct paramdef partab[] = {
    SPECIALPMDEF("errnos", PM_ARRAY|PM_READONLY,
		 &errnos_gsu, NULL, NULL),
    SPECIALPMDEF("sysparams", PM_READONLY,
		 NULL, getpmsysparams, scanpmsysparams)
};

static struct features module_features = {
    bintab, sizeof(bintab)/sizeof(*bintab),
    NULL, 0,
    NULL, 0,
    partab, sizeof(partab)/sizeof(*partab),
    0
};

/* The load/unload routines required by the zsh library interface */

/**/
int
setup_(UNUSED(Module m))
{
    return 0;
}

/**/
int
features_(Module m, char ***features)
{
    *features = featuresarray(m, &module_features);
    return 0;
}

/**/
int
enables_(Module m, int **enables)
{
    return handlefeatures(m, &module_features, enables);
}

/**/
int
boot_(Module m)
{
    return 0;
}


/**/
int
cleanup_(Module m)
{
    return setfeatureenables(m, &module_features, NULL);
}

/**/
int
finish_(UNUSED(Module m))
{
    return 0;
}
