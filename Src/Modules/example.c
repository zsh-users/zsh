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

/* parameters */

static zlong intparam;
static char *strparam;
static char **arrparam;


/**/
static int
bin_example(char *nam, char **args, char *ops, int func)
{
    unsigned char c;
    char **oargs = args, **p = arrparam;
    long i = 0;

    printf("Options: ");
    for (c = 32; ++c < 128;)
	if (ops[c])
	    putchar(c);
    printf("\nArguments:");
    for (; *args; i++, args++) {
	putchar(' ');
	fputs(*args, stdout);
    }
    printf("\nName: %s\n", nam);
#ifdef ZSH_64_BIT_TYPE
    printf("\nInteger Parameter: %s\n", output64(intparam));
#else
    printf("\nInteger Parameter: %ld\n", intparam);
#endif
    printf("String Parameter: %s\n", strparam ? strparam : "");
    printf("Array Parameter:");
    if (p)
	while (*p) printf(" %s", *p++);
    printf("\n");

    intparam = i;
    zsfree(strparam);
    strparam = ztrdup(*oargs ? *oargs : "");
    freearray(arrparam);
    arrparam = zarrdup(oargs);
    return 0;
}

/**/
static int
cond_p_len(char **a, int id)
{
    char *s1 = cond_str(a, 0, 0);

    if (a[1]) {
	zlong v = cond_val(a, 1);

	return strlen(s1) == v;
    } else {
	return !s1[0];
    }
}

/**/
static int
cond_i_ex(char **a, int id)
{
    char *s1 = cond_str(a, 0, 0), *s2 = cond_str(a, 1, 0);

    return !strcmp("example", dyncat(s1, s2));
}

/**/
static mnumber
math_sum(char *name, int argc, mnumber *argv, int id)
{
    mnumber ret;
    int f = 0;

    ret.u.l = 0;
    while (argc--) {
	if (argv->type == MN_INTEGER) {
	    if (f)
		ret.u.d += (double) argv->u.l;
	    else
		ret.u.l += argv->u.l;
	} else {
	    if (f)
		ret.u.d += argv->u.d;
	    else {
		ret.u.d = ((double) ret.u.l) + ((double) argv->u.d);
		f = 1;
	    }
	}
	argv++;
    }
    ret.type = (f ? MN_FLOAT : MN_INTEGER);

    return ret;
}

/**/
static mnumber
math_length(char *name, char *arg, int id)
{
    mnumber ret;

    ret.type = MN_INTEGER;
    ret.u.l = strlen(arg);

    return ret;
}

/**/
static int
ex_wrapper(Eprog prog, FuncWrap w, char *name)
{
    if (strncmp(name, "example", 7))
	return 1;
    else {
	int ogd = opts[GLOBDOTS];

	opts[GLOBDOTS] = 1;
	runshfunc(prog, w, name);
	opts[GLOBDOTS] = ogd;

	return 0;
    }
}

/*
 * boot_ is executed when the module is loaded.
 */

static struct builtin bintab[] = {
    BUILTIN("example", 0, bin_example, 0, -1, 0, "flags", NULL),
};

static struct conddef cotab[] = {
    CONDDEF("len", 0, cond_p_len, 1, 2, 0),
    CONDDEF("ex", CONDF_INFIX, cond_i_ex, 0, 0, 0),
};

static struct paramdef patab[] = {
    INTPARAMDEF("exint", &intparam),
    STRPARAMDEF("exstr", &strparam),
    ARRPARAMDEF("exarr", &arrparam),
};

static struct mathfunc mftab[] = {
    NUMMATHFUNC("sum", math_sum, 1, -1, 0),
    STRMATHFUNC("length", math_length, 0),
};

static struct funcwrap wrapper[] = {
    WRAPDEF(ex_wrapper),
};

/**/
int
setup_(Module m)
{
    printf("The example module has now been set up.\n");
    fflush(stdout);
    return 0;
}

/**/
int
boot_(Module m)
{
    intparam = 42;
    strparam = ztrdup("example");
    arrparam = (char **) zalloc(3 * sizeof(char *));
    arrparam[0] = ztrdup("example");
    arrparam[1] = ztrdup("array");
    arrparam[2] = NULL;
    return !(addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab)) |
	     addconddefs(m->nam, cotab, sizeof(cotab)/sizeof(*cotab)) |
	     addparamdefs(m->nam, patab, sizeof(patab)/sizeof(*patab)) |
	     addmathfuncs(m->nam, mftab, sizeof(mftab)/sizeof(*mftab)) |
	     !addwrapper(m, wrapper));
}

/**/
int
cleanup_(Module m)
{
    deletebuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
    deleteconddefs(m->nam, cotab, sizeof(cotab)/sizeof(*cotab));
    deleteparamdefs(m->nam, patab, sizeof(patab)/sizeof(*patab));
    deletemathfuncs(m->nam, mftab, sizeof(mftab)/sizeof(*mftab));
    deletewrapper(m, wrapper);
    return 0;
}

/**/
int
finish_(Module m)
{
    printf("Thank you for using the example module.  Have a nice day.\n");
    fflush(stdout);
    return 0;
}
