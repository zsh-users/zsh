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
    char *endptr = NULL, *buffer = NULL;
    time_t secs;
    struct tm *t;
    int size;

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

    for (x=1;x<4;x++) {
	buffer = zrealloc(buffer, bufsize * x);
        size = ztrftime(buffer, bufsize * x, argv[0], t);
	if (size) x = 4;
    }

    printf("%s\n", buffer);
    
    return 0;
}

static struct builtin bintab[] = {
    BUILTIN("strftime",    0, bin_strftime,    2,   2, 0, NULL, NULL),
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
