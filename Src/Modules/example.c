/*
 * example.c - an example module for zsh
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 1996-1997 Zoltán Hidvégi
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Zoltán Hidvégi or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Zoltán Hidvégi and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Zoltán Hidvégi and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Zoltán Hidvégi and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "example.mdh"
#include "example.pro"

/**/
static int
bin_example(char *nam, char **args, char *ops, int func)
{
    unsigned char c;

    printf("Options: ");
    for (c = 32; ++c < 128;)
	if (ops[c])
	    putchar(c);
    printf("\nArguments:");
    for (; *args; args++) {
	putchar(' ');
	fputs(*args, stdout);
    }
    printf("\nName: %s\n", nam);
    return 0;
}

/**/
static int
cond_p_len(Conddef c, char **a)
{
    char *s1 = a[0], *s2 = a[1];

    singsub(&s1);
    untokenize(s1);
    if (s2) {
	singsub(&s2);
	untokenize(s2);
	return strlen(s1) == matheval(s2);
    } else {
	return !s1[0];
    }
}

/**/
static int
cond_i_ex(Conddef c, char **a)
{
    char *s1 = a[0], *s2 = a[1];

    singsub(&s1);
    untokenize(s1);
    singsub(&s2);
    untokenize(s2);
    return !strcmp("example", dyncat(s1, s2));
}

/**/
static int
ex_wrapper(List list, FuncWrap w, char *name)
{
    if (strncmp(name, "example", 7))
	return 1;
    else {
	int ogd = opts[GLOBDOTS];

	opts[GLOBDOTS] = 1;
	runshfunc(list, w, name);
	opts[GLOBDOTS] = ogd;

	return 0;
    }
}

/*
 * boot_example is executed when the module is loaded.
 */

static struct builtin bintab[] = {
    BUILTIN("example", 0, bin_example, 0, -1, 0, "flags", NULL),
};

static struct conddef cotab[] = {
    CONDDEF("len", 0, 1, 2, cond_p_len),
    CONDDEF("ex", CONDF_INFIX, 0, 0, cond_i_ex),
};

static struct funcwrap wrapper[] = {
    WRAPDEF(ex_wrapper),
};

/**/
int
boot_example(Module m)
{
    return !(addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab)) |
	     addconddefs(m->nam, cotab, sizeof(cotab)/sizeof(*cotab)) |
	     !addwrapper(m, wrapper));
}

#ifdef MODULE

/**/
int
cleanup_example(Module m)
{
    deletebuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
    deleteconddefs(m->nam, cotab, sizeof(cotab)/sizeof(*cotab));
    deletewrapper(m, wrapper);
    return 0;
}
#endif
