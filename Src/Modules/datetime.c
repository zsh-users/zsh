/*
 * datetime.c - parameter and command interface to date and time utilities
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 2002 Peter Stephenson, Clint Adams
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Peter Stephenson, Clint Adams or the Zsh Development Group
 * be liable to any party for direct, indirect, special, incidental, or
 * consequential damages arising out of the use of this software and its
 * documentation, even if Peter Stephenson, Clint Adams and the Zsh
 * Development Group have been advised of the possibility of such damage.
 *
 * Peter Stephenson, Clint Adams and the Zsh Development Group specifically
 * disclaim any warranties, including, but not limited to, the implied
 * warranties of merchantability and fitness for a particular purpose.
 * The software provided hereunder is on an "as is" basis, and Peter
 * Stephenson, Clint Adams and the Zsh Development Group have no obligation
 * to provide maintenance, support, updates, enhancements, or modifications.
 *
 */

#include "datetime.mdh"
#include "datetime.pro"
#include <time.h>

#ifndef HAVE_MKTIME
#ifdef HAVE_TIMELOCAL
#define	mktime(x)	timelocal(x)
#define HAVE_MKTIME	1
#endif
#endif

static int
reverse_strftime(char *nam, char **argv, char *scalar, int quiet)
{
#if defined(HAVE_STRPTIME) && defined(HAVE_MKTIME)
    struct tm tm;
    zlong mytime;
    char *endp;

    /*
     * Initialise all parameters to zero; there's no floating point
     * so memset() will do the trick.  The exception is that tm_isdst
     * is set to -1 which, if not overridden, will cause mktime()
     * to use the current timezone.  This is probably the best guess;
     * it's the one that will cause dates and times output by strftime
     * without the -r option and without an explicit timezone to be
     * converted back correctly.
     */
    (void)memset(&tm, 0, sizeof(tm));
    tm.tm_isdst = -1;
    endp = strptime(argv[1], argv[0], &tm);

    if (!endp) {
	/* Conversion failed completely. */
	if (!quiet)
	    zwarnnam(nam, "format not matched");
	return 1;
    }

    mytime = (zlong)mktime(&tm);

    if (scalar)
	setiparam(scalar, mytime);
    else {
	char buf[DIGBUFSIZE];
	convbase(buf, mytime, 10);
	printf("%s\n", buf);
    }

    if (*endp && !quiet) {
	/*
	 * Not everything in the input string was converted.
	 * This is probably benign, since the format has been satisfied,
	 * but issue a warning unless the quiet flag is set.
	 */
	zwarnnam(nam, "warning: input string not completely matched");
    }

    return 0;
#else
    if (!quiet)
	zwarnnam(nam, "not implemented on this system");
    return 2;
#endif
}

static int
bin_strftime(char *nam, char **argv, Options ops, UNUSED(int func))
{
    int bufsize, x;
    char *endptr = NULL, *scalar = NULL, *buffer;
    time_t secs;
    struct tm *t;

    if (OPT_ISSET(ops,'s')) {
	scalar = OPT_ARG(ops, 's');
	if (!isident(scalar)) {
	    zwarnnam(nam, "not an identifier: %s", scalar);
	    return 1;
	}
    }
    if (OPT_ISSET(ops, 'r'))
	return reverse_strftime(nam, argv, scalar, OPT_ISSET(ops, 'q'));

    errno = 0;
    secs = (time_t)strtoul(argv[1], &endptr, 10);
    if (errno != 0) {
	zwarnnam(nam, "%s: %e", argv[1], errno);
	return 1;
    } else if (*endptr != '\0') {
	zwarnnam(nam, "%s: invalid decimal number", argv[1]);
	return 1;
    }

    t = localtime(&secs);
    if (!t) {
	zwarnnam(nam, "%s: unable to convert to time", argv[1]);
	return 1;
    }
    bufsize = strlen(argv[0]) * 8;
    buffer = zalloc(bufsize);

    for (x=0; x < 4; x++) {
        if (ztrftime(buffer, bufsize, argv[0], t) >= 0)
	    break;
	buffer = zrealloc(buffer, bufsize *= 2);
    }

    if (scalar) {
	setsparam(scalar, metafy(buffer, -1, META_DUP));
    } else {
	printf("%s\n", buffer);
    }
    zfree(buffer, bufsize);

    return 0;
}

static zlong
getcurrentsecs()
{
    return (zlong) time(NULL);
}

static struct builtin bintab[] = {
    BUILTIN("strftime",    0, bin_strftime,    2,   2, 0, "qrs:", NULL),
};

static const struct gsu_integer epochseconds_gsu =
{ getcurrentsecs, NULL, stdunsetfn };

static struct paramdef patab[] = {
    SPECIALPMDEF("EPOCHSECONDS", PM_INTEGER|PM_READONLY,
		 &epochseconds_gsu, NULL, NULL),
};

static struct features module_features = {
    bintab, sizeof(bintab)/sizeof(*bintab),
    NULL, 0,
    NULL, 0,
    patab, sizeof(patab)/sizeof(*patab),
    0
};

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
