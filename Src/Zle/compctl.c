/*
 * compctl.c - the compctl builtin
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

#include "compctl.mdh"
#include "compctl.pro"

#define COMP_LIST	(1<<0)	/* -L */
#define COMP_COMMAND	(1<<1)	/* -C */
#define COMP_DEFAULT	(1<<2)	/* -D */
#define COMP_FIRST	(1<<3)	/* -T */
#define COMP_REMOVE	(1<<4)

#define COMP_SPECIAL (COMP_COMMAND|COMP_DEFAULT|COMP_FIRST)

/* Flag for listing, command, default, or first completion */
static int cclist;

/* Mask for determining what to print */
static unsigned long showmask = 0;

/* Parse the basic flags for `compctl' */

/**/
static int
get_compctl(char *name, char ***av, Compctl cc, int first, int isdef)
{
    /* Parse the basic flags for completion:
     * first is a flag that we are not in extended completion,
     * while hx indicates or (+) completion (need to know for
     * default and command completion as the initial compctl is special). 
     * cct is a temporary just to hold flags; it never needs freeing.
     */
    struct compctl cct;
    char **argv = *av;
    int ready = 0, hx = 0;

    /* Handle `compctl + foo ...' specially:  turn it into
     * a default compctl by removing it from the hash table.
     */
    if (first && argv[0][0] == '+' && !argv[0][1] &&
	!(argv[1] && argv[1][0] == '-' && argv[1][1])) {
	argv++;
	if(argv[0] && argv[0][0] == '-')
	    argv++;
	*av = argv;
	freecompctl(cc);
 	cclist = COMP_REMOVE;
	return 0;
    }

    memset((void *)&cct, 0, sizeof(cct));

    /* Loop through the flags until we have no more:        *
     * those with arguments are not properly allocated yet, *
     * we just hang on to the argument that was passed.     */
    for (; !ready && argv[0] && argv[0][0] == '-' && (argv[0][1] || !first);) {
	if (!argv[0][1])
	    *argv = "-+";
	while (!ready && *++(*argv)) {
	    if(**argv == Meta)
		*++*argv ^= 32;
	    switch (**argv) {
	    case 'f':
		cct.mask |= CC_FILES;
		break;
	    case 'c':
		cct.mask |= CC_COMMPATH;
		break;
	    case 'm':
		cct.mask |= CC_EXTCMDS;
		break;
	    case 'w':
		cct.mask |= CC_RESWDS;
		break;
	    case 'o':
		cct.mask |= CC_OPTIONS;
		break;
	    case 'v':
		cct.mask |= CC_VARS;
		break;
	    case 'b':
		cct.mask |= CC_BINDINGS;
		break;
	    case 'A':
		cct.mask |= CC_ARRAYS;
		break;
	    case 'I':
		cct.mask |= CC_INTVARS;
		break;
	    case 'F':
		cct.mask |= CC_SHFUNCS;
		break;
	    case 'p':
		cct.mask |= CC_PARAMS;
		break;
	    case 'E':
		cct.mask |= CC_ENVVARS;
		break;
	    case 'j':
		cct.mask |= CC_JOBS;
		break;
	    case 'r':
		cct.mask |= CC_RUNNING;
		break;
	    case 'z':
		cct.mask |= CC_STOPPED;
		break;
	    case 'B':
		cct.mask |= CC_BUILTINS;
		break;
	    case 'a':
		cct.mask |= CC_ALREG | CC_ALGLOB;
		break;
	    case 'R':
		cct.mask |= CC_ALREG;
		break;
	    case 'G':
		cct.mask |= CC_ALGLOB;
		break;
	    case 'u':
		cct.mask |= CC_USERS;
		break;
	    case 'd':
		cct.mask |= CC_DISCMDS;
		break;
	    case 'e':
		cct.mask |= CC_EXCMDS;
		break;
	    case 'N':
		cct.mask |= CC_SCALARS;
		break;
	    case 'O':
		cct.mask |= CC_READONLYS;
		break;
	    case 'Z':
		cct.mask |= CC_SPECIALS;
		break;
	    case 'q':
		cct.mask |= CC_REMOVE;
		break;
	    case 'U':
		cct.mask |= CC_DELETE;
		break;
	    case 'n':
		cct.mask |= CC_NAMED;
		break;
	    case 'Q':
		cct.mask |= CC_QUOTEFLAG;
		break;
	    case '/':
		cct.mask |= CC_DIRS;
		break;
	    case 'k':
		if ((*argv)[1]) {
		    cct.keyvar = (*argv) + 1;
		    *argv = "" - 1;
		} else if (!argv[1]) {
		    zwarnnam(name, "variable name expected after -%c", NULL,
			    **argv);
		    return 1;
		} else {
		    cct.keyvar = *++argv;
		    *argv = "" - 1;
		}
		break;
	    case 'K':
		if ((*argv)[1]) {
		    cct.func = (*argv) + 1;
		    *argv = "" - 1;
		} else if (!argv[1]) {
		    zwarnnam(name, "function name expected after -%c", NULL,
			    **argv);
		    return 1;
		} else {
		    cct.func = *++argv;
		    *argv = "" - 1;
		}
		break;
	    case 'Y':
		cct.mask |= CC_EXPANDEXPL;
		goto expl;
	    case 'X':
		cct.mask &= ~CC_EXPANDEXPL;
	    expl:
		if ((*argv)[1]) {
		    cct.explain = (*argv) + 1;
		    *argv = "" - 1;
		} else if (!argv[1]) {
		    zwarnnam(name, "string expected after -%c", NULL, **argv);
		    return 1;
		} else {
		    cct.explain = *++argv;
		    *argv = "" - 1;
		}
		break;
	    case 'y':
		if ((*argv)[1]) {
		    cct.ylist = (*argv) + 1;
		    *argv = "" - 1;
		} else if (!argv[1]) {
		    zwarnnam(name, "function/variable expected after -%c",
			     NULL, **argv);
		} else {
		    cct.ylist = *++argv;
		    *argv = "" - 1;
		}
		break;
	    case 'P':
		if ((*argv)[1]) {
		    cct.prefix = (*argv) + 1;
		    *argv = "" - 1;
		} else if (!argv[1]) {
		    zwarnnam(name, "string expected after -%c", NULL, **argv);
		    return 1;
		} else {
		    cct.prefix = *++argv;
		    *argv = "" - 1;
		}
		break;
	    case 'S':
		if ((*argv)[1]) {
		    cct.suffix = (*argv) + 1;
		    *argv = "" - 1;
		} else if (!argv[1]) {
		    zwarnnam(name, "string expected after -%c", NULL, **argv);
		    return 1;
		} else {
		    cct.suffix = *++argv;
		    *argv = "" - 1;
		}
		break;
	    case 'g':
		if ((*argv)[1]) {
		    cct.glob = (*argv) + 1;
		    *argv = "" - 1;
		} else if (!argv[1]) {
		    zwarnnam(name, "glob pattern expected after -%c", NULL,
			    **argv);
		    return 1;
		} else {
		    cct.glob = *++argv;
		    *argv = "" - 1;
		}
		break;
	    case 's':
		if ((*argv)[1]) {
		    cct.str = (*argv) + 1;
		    *argv = "" - 1;
		} else if (!argv[1]) {
		    zwarnnam(name, "command string expected after -%c", NULL,
			    **argv);
		    return 1;
		} else {
		    cct.str = *++argv;
		    *argv = "" - 1;
		}
		break;
	    case 'l':
		if ((*argv)[1]) {
		    cct.subcmd = (*argv) + 1;
		    *argv = "" - 1;
		} else if (!argv[1]) {
		    zwarnnam(name, "command name expected after -%c", NULL,
			    **argv);
		    return 1;
		} else {
		    cct.subcmd = *++argv;
		    *argv = "" - 1;
		}
		break;
	    case 'W':
		if ((*argv)[1]) {
		    cct.withd = (*argv) + 1;
		    *argv = "" - 1;
		} else if (!argv[1]) {
		    zwarnnam(name, "path expected after -%c", NULL,
			    **argv);
		    return 1;
		} else {
		    cct.withd = *++argv;
		    *argv = "" - 1;
		}
		break;
	    case 'H':
		if ((*argv)[1])
		    cct.hnum = atoi((*argv) + 1);
		else if (argv[1])
		    cct.hnum = atoi(*++argv);
		else {
		    zwarnnam(name, "number expected after -%c", NULL,
			    **argv);
		    return 1;
		}
		if (!argv[1]) {
		    zwarnnam(name, "missing pattern after -%c", NULL,
			    **argv);
		    return 1;
		}
		cct.hpat = *++argv;
		if (cct.hnum < 1)
		    cct.hnum = 0;
		if (*cct.hpat == '*' && !cct.hpat[1])
		    cct.hpat = "";
		*argv = "" - 1;
		break;
	    case 'C':
		if (first && !hx) {
		    cclist |= COMP_COMMAND;
		} else {
		    zwarnnam(name, "misplaced command completion (-C) flag",
			    NULL, 0);
		    return 1;
		}
		break;
	    case 'D':
		if (first && !hx) {
		    isdef = 1;
		    cclist |= COMP_DEFAULT;
		} else {
		    zwarnnam(name, "misplaced default completion (-D) flag",
			    NULL, 0);
		    return 1;
		}
		break;
 	    case 'T':
              if (first && !hx) {
 		    cclist |= COMP_FIRST;
 		} else {
 		    zwarnnam(name, "misplaced first completion (-T) flag",
 			    NULL, 0);
 		    return 1;
 		}
 		break;
	    case 'L':
		if (!first || hx) {
		    zwarnnam(name, "illegal use of -L flag", NULL, 0);
		    return 1;
		}
		cclist |= COMP_LIST;
		break;
	    case 'x':
		if (!argv[1]) {
		    zwarnnam(name, "condition expected after -%c", NULL,
			    **argv);
		    return 1;
		}
		if (first) {
		    argv++;
		    if (get_xcompctl(name, &argv, &cct, isdef)) {
			if (cct.ext)
			    freecompctl(cct.ext);
			return 1;
		    }
		    ready = 2;
		} else {
		    zwarnnam(name, "recursive extended completion not allowed",
			    NULL, 0);
		    return 1;
		}
		break;
	    default:
		if (!first && (**argv == '-' || **argv == '+'))
		    (*argv)--, argv--, ready = 1;
		else {
		    zwarnnam(name, "bad option: -%c", NULL, **argv);
		    return 1;
		}
	    }
	}

	if (*++argv && (!ready || ready == 2) &&
	    **argv == '+' && !argv[0][1]) {
	    /* There's an alternative (+) completion:  assign
	     * what we have so far before moving on to that.
	     */
	    if (cc_assign(name, &cc, &cct, first && !hx))
		return 1;

	    hx = 1;
	    ready = 0;

	    if (!*++argv || **argv != '-' ||
		(**argv == '-' && (!argv[0][1] ||
				   (argv[0][1] == '-' && !argv[0][2])))) {
		/* No argument to +, which means do default completion */
		if (isdef)
		    zwarnnam(name,
			    "recursive xor'd default completions not allowed",
			    NULL, 0);
		else
		    cc->xor = &cc_default;
	    } else {
		/* more flags follow:  prepare to loop again */
		cc->xor = (Compctl) zcalloc(sizeof(*cc));
		cc = cc->xor;
		memset((void *)&cct, 0, sizeof(cct));
	    }
	}
    }
    if (!ready && *argv && **argv == '-')
	argv++;

    if (! (cct.mask & (CC_EXCMDS | CC_DISCMDS)))
	cct.mask |= CC_EXCMDS;

    /* assign the last set of flags we parsed */
    if (cc_assign(name, &cc, &cct, first && !hx))
	return 1;

    *av = argv;

    return 0;
}

/* Handle the -x ... -- part of compctl. */

/**/
static int
get_xcompctl(char *name, char ***av, Compctl cc, int isdef)
{
    char **argv = *av, *t, *tt, sav;
    int n, l = 0, ready = 0;
    Compcond m, c, o;
    Compctl *next = &(cc->ext);

    while (!ready) {
	/* o keeps track of or's, m remembers the starting condition,
	 * c is the current condition being parsed
	 */
	o = m = c = (Compcond) zcalloc(sizeof(*c));
	/* Loop over each condition:  something like 's[...][...], p[...]' */
	for (t = *argv; *t;) {
	    while (*t == ' ')
		t++;
	    /* First get the condition code */
	    switch (*t) {
	    case 's':
		c->type = CCT_CURSUF;
		break;
	    case 'S':
		c->type = CCT_CURPRE;
		break;
	    case 'p':
		c->type = CCT_POS;
		break;
	    case 'c':
		c->type = CCT_CURSTR;
		break;
	    case 'C':
		c->type = CCT_CURPAT;
		break;
	    case 'w':
		c->type = CCT_WORDSTR;
		break;
	    case 'W':
		c->type = CCT_WORDPAT;
		break;
	    case 'n':
		c->type = CCT_CURSUB;
		break;
	    case 'N':
		c->type = CCT_CURSUBC;
		break;
	    case 'm':
		c->type = CCT_NUMWORDS;
		break;
	    case 'r':
		c->type = CCT_RANGESTR;
		break;
	    case 'R':
		c->type = CCT_RANGEPAT;
		break;
	    default:
		t[1] = '\0';
		zwarnnam(name, "unknown condition code: %s", t, 0);
		zfree(m, sizeof(struct compcond));

		return 1;
	    }
	    /* Now get the arguments in square brackets */
	    if (t[1] != '[') {
		t[1] = '\0';
		zwarnnam(name, "expected condition after condition code: %s", t, 0);
		zfree(m, sizeof(struct compcond));

		return 1;
	    }
	    t++;
	    /* First count how many or'd arguments there are,
	     * marking the active ]'s and ,'s with unprintable characters.
	     */
	    for (n = 0, tt = t; *tt == '['; n++) {
		for (l = 1, tt++; *tt && l; tt++)
		    if (*tt == '\\' && tt[1])
			tt++;
		    else if (*tt == '[')
			l++;
		    else if (*tt == ']')
			l--;
		    else if (l == 1 && *tt == ',')
			*tt = '\201';
		if (tt[-1] == ']')
		    tt[-1] = '\200';
	    }

	    if (l) {
		t[1] = '\0';
		zwarnnam(name, "error after condition code: %s", t, 0);
		zfree(m, sizeof(struct compcond));

		return 1;
	    }
	    c->n = n;

	    /* Allocate space for all the arguments of the conditions */
	    if (c->type == CCT_POS ||
		c->type == CCT_NUMWORDS) {
		c->u.r.a = (int *)zcalloc(n * sizeof(int));
		c->u.r.b = (int *)zcalloc(n * sizeof(int));
	    } else if (c->type == CCT_CURSUF ||
		       c->type == CCT_CURPRE)
		c->u.s.s = (char **)zcalloc(n * sizeof(char *));

	    else if (c->type == CCT_RANGESTR ||
		     c->type == CCT_RANGEPAT) {
		c->u.l.a = (char **)zcalloc(n * sizeof(char *));
		c->u.l.b = (char **)zcalloc(n * sizeof(char *));
	    } else {
		c->u.s.p = (int *)zcalloc(n * sizeof(int));
		c->u.s.s = (char **)zcalloc(n * sizeof(char *));
	    }
	    /* Now loop over the actual arguments */
	    for (l = 0; *t == '['; l++, t++) {
		for (t++; *t && *t == ' '; t++);
		tt = t;
		if (c->type == CCT_POS ||
		    c->type == CCT_NUMWORDS) {
		    /* p[...] or m[...]:  one or two numbers expected */
		    for (; *t && *t != '\201' && *t != '\200'; t++);
		    if (!(sav = *t)) {
			zwarnnam(name, "error in condition", NULL, 0);
			freecompcond(m);
			return 1;
		    }
		    *t = '\0';
		    c->u.r.a[l] = atoi(tt);
		    /* Second argument is optional:  see if it's there */
		    if (sav == '\200')
			/* no:  copy first argument */
			c->u.r.b[l] = c->u.r.a[l];
		    else {
			tt = ++t;
			for (; *t && *t != '\200'; t++);
			if (!*t) {
			    zwarnnam(name, "error in condition", NULL, 0);
			    freecompcond(m);
			    return 1;
			}
			*t = '\0';
			c->u.r.b[l] = atoi(tt);
		    }
		} else if (c->type == CCT_CURSUF ||
			   c->type == CCT_CURPRE) {
		    /* -s[..] or -S[..]:  single string expected */
		    for (; *t && *t != '\200'; t++)
			if (*t == '\201')
			    *t = ',';
		    if (!*t) {
			zwarnnam(name, "error in condition", NULL, 0);
			freecompcond(m);
			return 1;
		    }
		    *t = '\0';
		    c->u.s.s[l] = ztrdup(tt);
		} else if (c->type == CCT_RANGESTR ||
			   c->type == CCT_RANGEPAT) {
		    /* -r[..,..] or -R[..,..]:  two strings expected */
		    for (; *t && *t != '\201'; t++);
		    if (!*t) {
			zwarnnam(name, "error in condition", NULL, 0);
			freecompcond(m);
			return 1;
		    }
		    *t = '\0';
		    c->u.l.a[l] = ztrdup(tt);
		    tt = ++t;
		    /* any more commas are text, not active */
		    for (; *t && *t != '\200'; t++)
			if (*t == '\201')
			    *t = ',';
		    if (!*t) {
			zwarnnam(name, "error in condition", NULL, 0);
			freecompcond(m);
			return 1;
		    }
		    *t = '\0';
		    c->u.l.b[l] = ztrdup(tt);
		} else {
		    /* remaining patterns are number followed by string */
		    for (; *t && *t != '\200' && *t != '\201'; t++);
		    if (!*t || *t == '\200') {
			zwarnnam(name, "error in condition", NULL, 0);
			freecompcond(m);
			return 1;
		    }
		    *t = '\0';
		    c->u.s.p[l] = atoi(tt);
		    tt = ++t;
		    for (; *t && *t != '\200'; t++)
			if (*t == '\201')
			    *t = ',';
		    if (!*t) {
			zwarnnam(name, "error in condition", NULL, 0);
			freecompcond(m);
			return 1;
		    }
		    *t = '\0';
		    c->u.s.s[l] = ztrdup(tt);
		}
	    }
	    while (*t == ' ')
		t++;
	    if (*t == ',') {
		/* Another condition to `or' */
		o->or = c = (Compcond) zcalloc(sizeof(*c));
		o = c;
		t++;
	    } else if (*t) {
		/* Another condition to `and' */
		c->and = (Compcond) zcalloc(sizeof(*c));
		c = c->and;
	    }
	}
	/* Assign condition to current compctl */
	*next = (Compctl) zcalloc(sizeof(*cc));
	(*next)->cond = m;
	argv++;
	/* End of the condition; get the flags that go with it. */
	if (get_compctl(name, &argv, *next, 0, isdef))
	    return 1;
 	if ((!argv || !*argv) && (cclist & COMP_SPECIAL))
 	    /* default, first, or command completion finished */
	    ready = 1;
	else {
	    /* see if we are looking for more conditions or are
	     * ready to return (ready = 1)
	     */
	    if (!argv || !*argv || **argv != '-' ||
		((!argv[0][1] || argv[0][1] == '+') && !argv[1])) {
		zwarnnam(name, "missing command names", NULL, 0);
		return 1;
	    }
	    if (!strcmp(*argv, "--"))
		ready = 1;
	    else if (!strcmp(*argv, "-+") && argv[1] &&
		     !strcmp(argv[1], "--")) {
		ready = 1;
		argv++;
	    }
	    argv++;
	    /* prepare to put the next lot of conditions on the end */
	    next = &((*next)->next);
	}
    }
    /* save position at end of parsing */
    *av = argv - 1;
    return 0;
}

/**/
static int
cc_assign(char *name, Compctl *ccptr, Compctl cct, int reass)
{
    /* Copy over the details from the values in cct to those in *ccptr */
    Compctl cc;

    if (cct->subcmd && (cct->keyvar || cct->glob || cct->str ||
			cct->func || cct->explain || cct->ylist ||
			cct->prefix)) {
	zwarnnam(name, "illegal combination of options", NULL, 0);
	return 1;
    }

    /* Handle assignment of new default or command completion */
    if (reass && !(cclist & COMP_LIST)) {
	/* if not listing */
	if (cclist == (COMP_COMMAND|COMP_DEFAULT)
	    || cclist == (COMP_COMMAND|COMP_FIRST)
	    || cclist == (COMP_DEFAULT|COMP_FIRST)
	    || cclist == COMP_SPECIAL) {
 	    zwarnnam(name, "can't set -D, -T, and -C simultaneously", NULL, 0);
	    /* ... because the following code wouldn't work. */
	    return 1;
	}
	if (cclist & COMP_COMMAND) {
	    /* command */
	    *ccptr = &cc_compos;
	    cc_reassign(*ccptr);
	} else if (cclist & COMP_DEFAULT) {
	    /* default */
	    *ccptr = &cc_default;
	    cc_reassign(*ccptr);
 	} else if (cclist & COMP_FIRST) {
 	    /* first */
 	    *ccptr = &cc_first;
 	    cc_reassign(*ccptr);
	}
    }

    /* Free the old compctl */
    cc = *ccptr;
    zsfree(cc->keyvar);
    zsfree(cc->glob);
    zsfree(cc->str);
    zsfree(cc->func);
    zsfree(cc->explain);
    zsfree(cc->ylist);
    zsfree(cc->prefix);
    zsfree(cc->suffix);
    zsfree(cc->subcmd);
    zsfree(cc->withd);
    zsfree(cc->hpat);
    
    /* and copy over the new stuff, (permanently) allocating
     * space for strings.
     */
    cc->mask = cct->mask;
    cc->keyvar = ztrdup(cct->keyvar);
    cc->glob = ztrdup(cct->glob);
    cc->str = ztrdup(cct->str);
    cc->func = ztrdup(cct->func);
    cc->explain = ztrdup(cct->explain);
    cc->ylist = ztrdup(cct->ylist);
    cc->prefix = ztrdup(cct->prefix);
    cc->suffix = ztrdup(cct->suffix);
    cc->subcmd = ztrdup(cct->subcmd);
    cc->withd = ztrdup(cct->withd);
    cc->hpat = ztrdup(cct->hpat);
    cc->hnum = cct->hnum;

    /* careful with extended completion:  it's already allocated */
    cc->ext = cct->ext;

    return 0;
}

/**/
static void
cc_reassign(Compctl cc)
{
    /* Free up a new default or command completion:
     * this is a hack to free up the parts which should be deleted,
     * without removing the basic variable which is statically allocated
     */
    Compctl c2;

    c2 = (Compctl) zcalloc(sizeof *cc);
    c2->xor = cc->xor;
    c2->ext = cc->ext;
    c2->refc = 1;

    freecompctl(c2);

    cc->ext = cc->xor = NULL;
}

/**/
static void
compctl_process_cc(char **s, Compctl cc)
{
    Compctlp ccp;

    if (cclist & COMP_REMOVE) {
	/* Delete entries for the commands listed */
	for (; *s; s++) {
	    if ((ccp = (Compctlp) compctltab->removenode(compctltab, *s)))
		compctltab->freenode((HashNode) ccp);
	}
    } else {
	/* Add the compctl just read to the hash table */

	cc->refc = 0;
	for (; *s; s++) {
	    cc->refc++;
	    ccp = (Compctlp) zalloc(sizeof *ccp);
	    ccp->cc = cc;
	    compctltab->addnode(compctltab, ztrdup(*s), ccp);
	}
    }
}

/* Print a `compctl' */

/**/
static void
printcompctl(char *s, Compctl cc, int printflags)
{
    Compctl cc2;
    char *css = "fcqovbAIFpEjrzBRGudeNOZUnQmw/";
    char *mss = " pcCwWsSnNmrR";
    unsigned long t = 0x7fffffff;
    unsigned long flags = cc->mask;
    unsigned long oldshowmask;

    if ((flags & CC_EXCMDS) && !(flags & CC_DISCMDS))
	flags &= ~CC_EXCMDS;

    /* If showmask is non-zero, then print only those *
     * commands with that flag set.                   */
    if (showmask && !(flags & showmask))
	return;

    /* Temporarily clear showmask in case we make *
     * recursive calls to printcompctl.           */
    oldshowmask = showmask;
    showmask = 0;

    /* print either command name or start of compctl command itself */
    if (s) {
	if (cclist & COMP_LIST) {
	    printf("compctl");
	    if (cc == &cc_compos)
		printf(" -C");
	    if (cc == &cc_default)
		printf(" -D");
	    if (cc == &cc_first)
		printf(" -T");
	} else
	    quotedzputs(s, stdout);
    }

    /* loop through flags w/o args that are set, printing them if so */
    if (flags & t) {
	printf(" -");
	if ((flags & (CC_ALREG | CC_ALGLOB)) == (CC_ALREG | CC_ALGLOB))
	    putchar('a'), flags &= ~(CC_ALREG | CC_ALGLOB);
	while (*css) {
	    if (flags & t & 1)
		putchar(*css);
	    css++;
	    flags >>= 1;
	    t >>= 1;
	}
    }

    /* now flags with arguments */
    flags = cc->mask;
    printif(cc->keyvar, 'k');
    printif(cc->func, 'K');
    printif(cc->explain, (cc->mask & CC_EXPANDEXPL) ? 'Y' : 'X');
    printif(cc->ylist, 'y');
    printif(cc->prefix, 'P');
    printif(cc->suffix, 'S');
    printif(cc->glob, 'g');
    printif(cc->str, 's');
    printif(cc->subcmd, 'l');
    printif(cc->withd, 'W');
    if (cc->hpat) {
	printf(" -H %d ", cc->hnum);
	quotedzputs(cc->hpat, stdout);
    }

    /* now the -x ... -- extended completion part */
    if (cc->ext) {
	Compcond c, o;
	int i;

	cc2 = cc->ext;
	printf(" -x");

	while (cc2) {
	    /* loop over conditions */
	    c = cc2->cond;

	    printf(" '");
	    for (c = cc2->cond; c;) {
		/* loop over or's */
		o = c->or;
		while (c) {
		    /* loop over and's */
		    putchar(mss[c->type]);

		    for (i = 0; i < c->n; i++) {
			/* for all [...]'s of a given condition */
			putchar('[');
			switch (c->type) {
			case CCT_POS:
			case CCT_NUMWORDS:
			    printf("%d,%d", c->u.r.a[i], c->u.r.b[i]);
			    break;
			case CCT_CURSUF:
			case CCT_CURPRE:
			    printqt(c->u.s.s[i]);
			    break;
			case CCT_RANGESTR:
			case CCT_RANGEPAT:
			    printqt(c->u.l.a[i]);
			    putchar(',');
			    printqt(c->u.l.b[i]);
			    break;
			default:
			    printf("%d,", c->u.s.p[i]);
			    printqt(c->u.s.s[i]);
			}
			putchar(']');
		    }
		    if ((c = c->and))
			putchar(' ');
		}
		if ((c = o))
		    printf(" , ");
	    }
	    putchar('\'');
	    c = cc2->cond;
	    cc2->cond = NULL;
	    /* now print the flags for the current condition */
	    printcompctl(NULL, cc2, 0);
	    cc2->cond = c;
	    if ((cc2 = (Compctl) (cc2->next)))
		printf(" -");
	}
	if (cclist & COMP_LIST)
	    printf(" --");
    }
    if (cc && cc->xor) {
	/* print xor'd (+) completions */
	printf(" +");
	if (cc->xor != &cc_default)
	    printcompctl(NULL, cc->xor, 0);
    }
    if (s) {
	if ((cclist & COMP_LIST) && (cc != &cc_compos)
	    && (cc != &cc_default) && (cc != &cc_first)) {
	    if(s[0] == '-' || s[0] == '+')
		printf(" -");
	    putchar(' ');
	    quotedzputs(s, stdout);
	}
	putchar('\n');
    }

    showmask = oldshowmask;
}

/**/
static void
printcompctlp(HashNode hn, int printflags)
{
    Compctlp ccp = (Compctlp) hn;

    /* Function needed for use by scanhashtable() */
    printcompctl(ccp->nam, ccp->cc, printflags);
}

/* Main entry point for the `compctl' builtin */

/**/
static int
bin_compctl(char *name, char **argv, char *ops, int func)
{
    Compctl cc = NULL;
    int ret = 0;

    /* clear static flags */
    cclist = 0;
    showmask = 0;

    /* Parse all the arguments */
    if (*argv) {
	cc = (Compctl) zcalloc(sizeof(*cc));
	if (get_compctl(name, &argv, cc, 1, 0)) {
	    freecompctl(cc);
	    return 1;
	}

	/* remember flags for printing */
	showmask = cc->mask;
	if ((showmask & CC_EXCMDS) && !(showmask & CC_DISCMDS))
	    showmask &= ~CC_EXCMDS;

	/* if no command arguments or just listing, we don't want cc */
	if (!*argv || (cclist & COMP_LIST))
	    freecompctl(cc);
    }

    /* If no commands and no -C, -T, or -D, print all the compctl's *
     * If some flags (other than -C, -T, or -D) were given, then    *
     * only print compctl containing those flags.                   */
    if (!*argv && !(cclist & COMP_SPECIAL)) {
	scanhashtable(compctltab, 1, 0, 0, compctltab->printnode, 0);
	printcompctl((cclist & COMP_LIST) ? "" : "COMMAND", &cc_compos, 0);
	printcompctl((cclist & COMP_LIST) ? "" : "DEFAULT", &cc_default, 0);
 	printcompctl((cclist & COMP_LIST) ? "" : "FIRST", &cc_first, 0);
	return ret;
    }

    /* If we're listing and we've made it to here, then there are arguments *
     * or a COMP_SPECIAL flag (-D, -C, -T), so print only those.            */
    if (cclist & COMP_LIST) {
	HashNode hn;
	char **ptr;

	showmask = 0;
	for (ptr = argv; *ptr; ptr++) {
	    if ((hn = compctltab->getnode(compctltab, *ptr))) {
		compctltab->printnode(hn, 0);
	    } else {
		zwarnnam(name, "no compctl defined for %s", *ptr, 0);
		ret = 1;
	    }
	}
	if (cclist & COMP_COMMAND)
	    printcompctl("", &cc_compos, 0);
	if (cclist & COMP_DEFAULT)
	    printcompctl("", &cc_default, 0);
	if (cclist & COMP_FIRST)
	    printcompctl("", &cc_first, 0);
	return ret;
    }

    /* Assign the compctl to the commands given */
    if (*argv) {
	if(cclist & COMP_SPECIAL)
	    /* Ideally we'd handle this properly, setting both the *
	     * special and normal completions.  For the moment,    *
	     * this is better than silently failing.               */
	    zwarnnam(name, "extraneous commands ignored", NULL, 0);
	else
	    compctl_process_cc(argv, cc);
    }

    return ret;
}

static struct builtin bintab[] = {
    BUILTIN("compctl", 0, bin_compctl, 0, -1, 0, NULL, NULL),
};

/**/
int
boot_compctl(Module m)
{
    if(!addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab)))
	return 1;
    compctltab->printnode = printcompctlp;
    return 0;
}

#ifdef MODULE

/**/
int
cleanup_compctl(Module m)
{
    compctltab->printnode = NULL;
    deletebuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
    return 0;
}
#endif
