/*
 * datetime.c - parameter interface to langinfo via curses
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

static int
bin_strftime(char *nam, char **argv, Options ops, int func)
{
    int bufsize, x;
    char *endptr = NULL, *buffer;
    time_t secs;
    struct tm *t;

    secs = (time_t)strtoul(argv[1], &endptr, 10);
    if (secs == ULONG_MAX) {
	zwarnnam(nam, "%s: %e", argv[1], errno);
	return 1;
    } else if (*endptr != '\0') {
	zwarnnam(nam, "%s: invalid decimal number", argv[1], 0);
	return 1;
    }

    t = localtime(&secs);
    bufsize = strlen(argv[0]) * 2;
    buffer = zalloc(bufsize);

    for (x=0; x < 4; x++) {
        if (ztrftime(buffer, bufsize, argv[0], t))
	    break;
	buffer = zrealloc(buffer, bufsize *= 2);
    }

    printf("%s\n", buffer);
    zfree(buffer, bufsize);

    return 0;
}

static zlong
getcurrentsecs()
{
    return (zlong) time(NULL);
}

static struct builtin bintab[] = {
    BUILTIN("strftime",    0, bin_strftime,    2,   2, 0, NULL, NULL),
};

static struct paramdef patab[] = {
    PARAMDEF("SECS", PM_INTEGER|PM_SPECIAL|PM_READONLY,
		    NULL, NULL, &getcurrentsecs, NULL),
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
    return !(addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab)) |
	     addparamdefs(m->nam, patab, sizeof(patab)/sizeof(*patab))
	    );
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
