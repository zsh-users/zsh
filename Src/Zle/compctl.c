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
#define GLOBAL_PROTOTYPES
#include "zle_tricky.pro"
#undef GLOBAL_PROTOTYPES

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

/* This is a special return value for parse_cmatcher(), *
 * signalling an error. */

#define pcm_err ((Cmatcher) 1)

/* Copy a list of completion matchers. */

static Cmlist
cpcmlist(Cmlist l)
{
    Cmlist r = NULL, *p = &r, n;

    while (l) {
	*p = n = (Cmlist) zalloc(sizeof(struct cmlist));
	n->next = NULL;
	n->matcher = cpcmatcher(l->matcher);
	n->str = ztrdup(l->str);

	p = &(n->next);
	l = l->next;
    }
    return r;
}

/* Copy a completion matcher list. */

/**/
static Cmatcher
cpcmatcher(Cmatcher m)
{
    Cmatcher r = NULL, *p = &r, n;

    while (m) {
	*p = n = (Cmatcher) zalloc(sizeof(struct cmatcher));

	n->next = NULL;
	n->flags = m->flags;
	n->line = cpcpattern(m->line);
	n->llen = m->llen;
	n->word = cpcpattern(m->word);
	n->wlen = m->wlen;
	n->left = cpcpattern(m->left);
	n->lalen = m->lalen;
	n->right = cpcpattern(m->right);
	n->ralen = m->ralen;

	p = &(n->next);
	m = m->next;
    }
    return r;
}

/* Copy a completion matcher pattern. */

/**/
static Cpattern
cpcpattern(Cpattern o)
{
    Cpattern r = NULL, *p = &r, n;

    while (o) {
	*p = n = (Cpattern) zalloc(sizeof(struct cpattern));

	n->next = NULL;
	memcpy(n->tab, o->tab, 256);
	n->equiv = o->equiv;

	p = &(n->next);
	o = o->next;
    }
    return r;
}

/* Try to get the global matcher from the given compctl. */

/**/
static int
get_gmatcher(char *name, char **argv)
{
    if (!strcmp(*argv, "-M")) {
	char **p = ++argv;
	Cmlist l = NULL, *q = &l, n;
	Cmatcher m;

	while (*p) {
	    if (**p++ == '-')
		return 0;
	}
	while (*argv) {
	    if ((m = parse_cmatcher(name, *argv)) == pcm_err)
		return 2;
	    *q = n = (Cmlist) halloc(sizeof(struct cmlist));
	    n->next = NULL;
	    n->matcher = m;
	    n->str = *argv++;

	    q = &(n->next);
	}
	freecmlist(cmatcher);
	PERMALLOC {
	    cmatcher = cpcmlist(l);
	} LASTALLOC;

	return 1;
    }
    return 0;
}

/* This prints the global matcher definitions. */

/**/
static void
print_gmatcher(int ac)
{
    Cmlist p;

    if ((p = cmatcher)) {
	printf((ac ? "compctl -M" : "MATCH"));

	while (p) {
	    printf(" \'%s\'", p->str);

	    p = p->next;
	}
	putchar('\n');
    }
}

/* Parse a string for matcher control, containing multiple matchers. */

/**/
static Cmatcher
parse_cmatcher(char *name, char *s)
{
    Cmatcher ret = NULL, r, n;
    Cpattern line, word, left, right;
    int fl, ll, wl, lal, ral, err;

    if (!*s)
	return NULL;

    while (*s) {
	while (*s && inblank(*s)) s++;

	if (!*s) break;

	switch (*s) {
	case 'l': fl = CMF_LEFT; break;
	case 'r': fl = CMF_RIGHT; break;
	case 'm': fl = 0; break;
	case 'L': fl = CMF_LEFT | CMF_LINE; break;
	case 'R': fl = CMF_RIGHT | CMF_LINE; break;
	case 'M': fl = CMF_LINE; break;
	default:
	    zwarnnam(name, "unknown match specification character `%c'", NULL, *s);
	    return pcm_err;
	}
	if (s[1] != ':') {
	    zwarnnam(name, "missing `:'", NULL, 0);
	    return pcm_err;
	}
	s += 2;
	if (!*s) {
	    zwarnnam(name, "missing patterns", NULL, 0);
	    return pcm_err;
	}
	if (fl & CMF_LEFT) {
	    left = parse_pattern(name, &s, &lal, '|', &err);
	    if (err)
		return pcm_err;
	    if (!*s || !*++s) {
		zwarnnam(name, "missing line pattern", NULL, 0);
		return pcm_err;
	    }
	} else
	    left = NULL;

	line = parse_pattern(name, &s, &ll, ((fl & CMF_RIGHT) ? '|' : '='),
			     &err);
	if (err)
	    return pcm_err;
	if (!*s || !*++s) {
	    zwarnnam(name, ((fl & CMF_RIGHT) ? "missing right anchor" : "missing word pattern"), NULL, 0);
	    return pcm_err;
	}
	if (fl & CMF_RIGHT) {
	    right = parse_pattern(name, &s, &ral, '=', &err);
	    if (err)
		return pcm_err;
	    if (!*s || !*++s) {
		zwarnnam(name, "missing word pattern", NULL, 0);
		return pcm_err;
	    }
	} else
	    right = NULL;

	if (*s == '*') {
	    if (!(fl & (CMF_LEFT | CMF_RIGHT))) {
		zwarnnam(name, "need anchor for `*'", NULL, 0);
		return pcm_err;
	    }
	    word = NULL;
	    wl = -1;
	    s++;
	} else {
	    word = parse_pattern(name, &s, &wl, 0, &err);

	    if (!word && !line) {
		zwarnnam(name, "need non-empty word or line pattern", NULL, 0);
		return pcm_err;
	    }
	}
	if (err)
	    return pcm_err;

	n = (Cmatcher) zcalloc(sizeof(*ret));
	n->next = NULL;
	n->flags = fl;
	n->line = line;
	n->llen = ll;
	n->word = word;
	n->wlen = wl;
	n->left = left;
	n->lalen = lal;
	n->right = right;
	n->ralen = ral;

	if (ret) r->next = n;
	else ret = n;

	r = n;
    }
    return ret;
}

/* Parse a pattern for matcher control. */

/**/
static Cpattern
parse_pattern(char *name, char **sp, int *lp, char e, int *err)
{
    Cpattern ret = NULL, r, n;
    unsigned char *s = (unsigned char *) *sp;
    int l = 0;

    *err = 0;

    while (*s && (e ? (*s != e) : !inblank(*s))) {
	n = (Cpattern) hcalloc(sizeof(*n));
	n->next = NULL;
	n->equiv = 0;

	if (*s == '[') {
	    s = parse_class(n, s + 1, ']');
	    if (!*s) {
		*err = 1;
		zwarnnam(name, "unterminated character class", NULL, 0);
		return NULL;
	    }
	} else if (*s == '{') {
	    n->equiv = 1;
	    s = parse_class(n, s + 1, '}');
	    if (!*s) {
		*err = 1;
		zwarnnam(name, "unterminated character class", NULL, 0);
		return NULL;
	    }
	} else if (*s == '?') {
	    memset(n->tab, 1, 256);
	} else if (*s == '*' || *s == '(' || *s == ')' || *s == '=') {
	    *err = 1;
	    zwarnnam(name, "invalid pattern character `%c'", NULL, *s);
	    return NULL;
	} else {
	    if (*s == '\\' && s[1])
		s++;

	    memset(n->tab, 0, 256);
	    n->tab[*s] = 1;
	}
	if (ret)
	    r->next = n;
	else
	    ret = n;

	r = n;

	l++;
	s++;
    }
    *sp = (char *) s;
    *lp = l;
    return ret;
}

/* Parse a character class for matcher control. */

/**/
static unsigned char *
parse_class(Cpattern p, unsigned char *s, unsigned char e)
{
    int n = 0, i = 1, j, eq = (e == '}'), k = 1;

    if (!eq && (*s == '!' || *s == '^') && s[1] != e) { n = 1; s++; }

    memset(p->tab, n, 256);

    n = !n;
    while (*s && (k || *s != e)) {
	if (s[1] == '-' && s[2] != e) {
	    /* a run of characters */
	    for (j = (int) *s; j <= (int) s[2]; j++)
		p->tab[j] = (eq ? i++ : n);

	    s += 3;
	} else
	    p->tab[*s++] = (eq ? i++ : n);
	k = 0;
    }
    return s;
}

/* Parse the basic flags for `compctl' */

/**/
static int
get_compctl(char *name, char ***av, Compctl cc, int first, int isdef, int cl)
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
	if (cl)
	    return 1;
	else {
	    freecompctl(cc);
	    cclist = COMP_REMOVE;
	    return 0;
	}
    }

    memset((void *)&cct, 0, sizeof(cct));
    cct.mask2 = CC_CCCONT;

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
	    case 't':
		{
		    char *p;

		    if (cl) {
			zerrnam(name, "illegal option -%c", NULL, **argv);
			return 1;
		    }
		    if ((*argv)[1]) {
			p = (*argv) + 1;
			*argv = "" - 1;
		    } else if (!argv[1]) {
			zwarnnam(name, "retry specification expected after -%c", NULL,
				 **argv);
			return 1;
		    } else {
			p = *++argv;
			*argv = "" - 1;
		    }
		    switch (*p) {
		    case '+':
			cct.mask2 = CC_XORCONT;
			break;
		    case 'n':
			cct.mask2 = 0;
			break;
		    case '-':
			cct.mask2 = CC_PATCONT;
			break;
		    case 'x':
			cct.mask2 = CC_DEFCONT;
			break;
		    default:
			zwarnnam(name, "invalid retry specification character `%c'",
				 NULL, *p);
			return 1;
		    }
		    if (p[1]) {
			zwarnnam(name, "too many retry specification characters: `%s'",
				 p + 1, 0);
			return 1;
		    }
		}
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
		if (cl) {
		    zerrnam(name, "illegal option -%c", NULL, **argv);
		    return 1;
		} else if ((*argv)[1]) {
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
	    case 'J':
		if ((*argv)[1]) {
		    cct.gname = (*argv) + 1;
		    *argv = "" - 1;
		} else if (!argv[1]) {
		    zwarnnam(name, "group name expected after -%c", NULL,
			    **argv);
		    return 1;
		} else {
		    cct.gname = *++argv;
		    *argv = "" - 1;
		}
		break;
	    case 'V':
		if ((*argv)[1]) {
		    cct.gname = (*argv) + 1;
		    *argv = "" - 1;
		} else if (!argv[1]) {
		    zwarnnam(name, "group name expected after -%c", NULL,
			    **argv);
		    return 1;
		} else {
		    cct.gname = *++argv;
		    *argv = "" - 1;
		}
		cct.mask2 |= CC_NOSORT;
		break;
	    case 'M':
		if ((*argv)[1]) {
		    if ((cct.matcher =
			 parse_cmatcher(name, (cct.mstr = (*argv) + 1))) ==
			pcm_err) {
			cct.matcher = NULL;
			cct.mstr = NULL;
			return 1;
		    }
		    *argv = "" - 1;
		} else if (!argv[1]) {
		    zwarnnam(name, "matching specification expected after -%c", NULL,
			    **argv);
		    return 1;
		} else {
		    if ((cct.matcher =
			 parse_cmatcher(name, (cct.mstr = *++argv))) ==
			pcm_err) {
			cct.matcher = NULL;
			cct.mstr = NULL;
			return 1;
		    }
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
		if (cl) {
		    zerrnam(name, "illegal option -%c", NULL, **argv);
		    return 1;
		}
		if (first && !hx) {
		    cclist |= COMP_COMMAND;
		} else {
		    zwarnnam(name, "misplaced command completion (-C) flag",
			    NULL, 0);
		    return 1;
		}
		break;
	    case 'D':
		if (cl) {
		    zerrnam(name, "illegal option -%c", NULL, **argv);
		    return 1;
		}
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
		if (cl) {
		    zerrnam(name, "illegal option -%c", NULL, **argv);
		    return 1;
		}
		if (first && !hx) {
 		    cclist |= COMP_FIRST;
 		} else {
 		    zwarnnam(name, "misplaced first completion (-T) flag",
 			    NULL, 0);
 		    return 1;
 		}
 		break;
	    case 'L':
		if (cl) {
		    zerrnam(name, "illegal option -%c", NULL, **argv);
		    return 1;
		}
		if (!first || hx) {
		    zwarnnam(name, "illegal use of -L flag", NULL, 0);
		    return 1;
		}
		cclist |= COMP_LIST;
		break;
	    case 'x':
		if (cl) {
		    zerrnam(name, "extended completion not allowed", NULL, 0);
		    return 1;
		}
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
	    if (cl) {
		zerrnam(name, "xor'ed completion illegal", NULL, 0);
		return 1;
	    }
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
		cct.mask2 = CC_CCCONT;
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
		    int hc;

		    /* -r[..,..] or -R[..,..]:  two strings expected */
		    for (; *t && *t != '\201' && *t != '\200'; t++);
		    if (!*t) {
			zwarnnam(name, "error in condition", NULL, 0);
			freecompcond(m);
			return 1;
		    }
		    hc = (*t == '\201');
		    *t = '\0';
		    c->u.l.a[l] = ztrdup(tt);
		    if (hc) {
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
		    }
		    else
			c->u.l.b[l] = NULL;
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
	if (get_compctl(name, &argv, *next, 0, isdef, 0))
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
    zsfree(cc->gname);
    zsfree(cc->mstr);
    freecmatcher(cc->matcher);

    /* and copy over the new stuff, (permanently) allocating
     * space for strings.
     */
    cc->mask = cct->mask;
    cc->mask2 = cct->mask2;
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
    cc->gname = ztrdup(cct->gname);
    cc->hpat = ztrdup(cct->hpat);
    cc->hnum = cct->hnum;
    PERMALLOC {
	cc->matcher = cpcmatcher(cct->matcher);
    } LASTALLOC;
    cc->mstr = ztrdup(cct->mstr);

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

/* Check if the given string is a pattern. If so, return one and tokenize *
 * it. If not, we just remove the backslashes. */

static int
compctl_name_pat(char **p)
{
    char *s = *p;

    tokenize(s = dupstring(s));
    remnulargs(s);

    if (haswilds(s)) {
	*p = s;
	return 1;
    } else
	*p = rembslash(*p);

    return 0;
}

/* Delete the pattern compctl with the given name. */

static void
delpatcomp(char *n)
{
    Patcomp p, q;

    for (q = 0, p = patcomps; p; q = p, p = p->next) {
	if (!strcmp(n, p->pat)) {
	    if (q)
		q->next = p->next;
	    else
		patcomps = p->next;
	    zsfree(p->pat);
	    freecompctl(p->cc);
	    free(p);

	    break;
	}
    }
}

/**/
static void
compctl_process_cc(char **s, Compctl cc)
{
    Compctlp ccp;
    char *n;

    if (cclist & COMP_REMOVE) {
	/* Delete entries for the commands listed */
	for (; *s; s++) {
	    n = *s;
	    if (compctl_name_pat(&n))
		delpatcomp(n);
	    else if ((ccp = (Compctlp) compctltab->removenode(compctltab, n)))
		compctltab->freenode((HashNode) ccp);
	}
    } else {
	/* Add the compctl just read to the hash table */

	cc->refc = 0;
	for (; *s; s++) {
	    n = *s;

	    cc->refc++;
	    if (compctl_name_pat(&n)) {
		Patcomp pc;

		delpatcomp(n);
		pc = zalloc(sizeof *pc);
		pc->pat = ztrdup(n);
		pc->cc = cc;
		pc->next = patcomps;
		patcomps = pc;
	    } else {
		ccp = (Compctlp) zalloc(sizeof *ccp);
		ccp->cc = cc;
		compctltab->addnode(compctltab, ztrdup(n), ccp);
	    }
	}
    }
}

/* Print a `compctl' */

/**/
static void
printcompctl(char *s, Compctl cc, int printflags, int ispat)
{
    Compctl cc2;
    char *css = "fcqovbAIFpEjrzBRGudeNOZUnQmw/";
    char *mss = " pcCwWsSnNmrR";
    unsigned long t = 0x7fffffff;
    unsigned long flags = cc->mask, flags2 = cc->mask2;
    unsigned long oldshowmask;

    /* Printflags is used outside the standard compctl commands*/
    if (printflags & PRINT_LIST)
	cclist |= COMP_LIST;
    else if (printflags & PRINT_TYPE)
	cclist &= ~COMP_LIST;

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
	} else if (ispat) {
	    char *p = dupstring(s);

	    untokenize(p);
	    quotedzputs(p, stdout);
	} else
	    quotedzputs(bslashquote(s, NULL, NULL, NULL, 0), stdout);
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
    if (flags2 & (CC_XORCONT | CC_PATCONT | CC_DEFCONT)) {
	printf(" -t");
	if (flags2 & CC_XORCONT)
	    putchar('+');
	if (flags2 & CC_PATCONT)
	    putchar('-');
	if (flags2 & CC_DEFCONT)
	    putchar('x');
    } else if (!(flags2 & CC_CCCONT))
	printf(" -tn");
    /* now flags with arguments */
    printif(cc->mstr, 'M');
    if (flags2 & CC_NOSORT)
	printif(cc->gname, 'V');
    else
	printif(cc->gname, 'J');
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
	    printcompctl(NULL, cc2, 0, 0);
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
	    printcompctl(NULL, cc->xor, 0, 0);
    }
    if (s) {
	if ((cclist & COMP_LIST) && (cc != &cc_compos)
	    && (cc != &cc_default) && (cc != &cc_first)) {
	    if(s[0] == '-' || s[0] == '+')
		printf(" -");
	    putchar(' ');
	    if (ispat) {
		char *p = dupstring(s);

		untokenize(p);
		quotedzputs(p, stdout);
	    } else {
		char *p = dupstring(s);

		untokenize(p);
		quotedzputs(bslashquote(p, NULL, NULL, NULL, 0), stdout);
	    }
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
    printcompctl(ccp->nam, ccp->cc, printflags, 0);
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
	/* Let's see if this is a global matcher definition. */
	if ((ret = get_gmatcher(name, argv)))
	    return ret - 1;

	cc = (Compctl) zcalloc(sizeof(*cc));
	if (get_compctl(name, &argv, cc, 1, 0, 0)) {
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
	Patcomp pc;

	for (pc = patcomps; pc; pc = pc->next)
	    printcompctl(pc->pat, pc->cc, 0, 1);

	scanhashtable(compctltab, 1, 0, 0, compctltab->printnode, 0);
	printcompctl((cclist & COMP_LIST) ? "" : "COMMAND", &cc_compos, 0, 0);
	printcompctl((cclist & COMP_LIST) ? "" : "DEFAULT", &cc_default, 0, 0);
 	printcompctl((cclist & COMP_LIST) ? "" : "FIRST", &cc_first, 0, 0);
	print_gmatcher((cclist & COMP_LIST));
	return ret;
    }

    /* If we're listing and we've made it to here, then there are arguments *
     * or a COMP_SPECIAL flag (-D, -C, -T), so print only those.            */
    if (cclist & COMP_LIST) {
	HashNode hn;
	char **ptr, *n;

	showmask = 0;
	for (ptr = argv; *ptr; ptr++) {
	    n = *ptr;
	    if (compctl_name_pat(&n)) {
		Patcomp pc;

		for (pc = patcomps; pc; pc = pc->next)
		    if (!strcmp(n, pc->pat)) {
			printcompctl(pc->pat, pc->cc, 0, 1);
			n = NULL;
			break;
		    }
	    } else if ((hn = compctltab->getnode(compctltab, n))) {
		compctltab->printnode(hn, 0);
		n = NULL;
	    }
	    if (n) {
		zwarnnam(name, "no compctl defined for %s", n, 0);
		ret = 1;
	    }
	}
	if (cclist & COMP_COMMAND)
	    printcompctl("", &cc_compos, 0, 0);
	if (cclist & COMP_DEFAULT)
	    printcompctl("", &cc_default, 0, 0);
	if (cclist & COMP_FIRST)
	    printcompctl("", &cc_first, 0, 0);
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

/**/
static int
bin_complist(char *name, char **argv, char *ops, int func)
{
    Compctl cc;
    int ret = 0;

    if (incompfunc != 1) {
	zerrnam(name, "can only be called from completion function", NULL, 0);
	return 1;
    }
    cc = (Compctl) zcalloc(sizeof(*cc));
    cclist = 0;
    showmask = 0;

    if (get_compctl(name, &argv, cc, 1, 0, 1))
	ret = 1;
    else if (*argv) {
	zerrnam(name, "command names illegal", NULL, 0);
	ret = 1;
    } else
	makecomplistcallptr(cc);

    freecompctl(cc);
    return ret;
}

/**/
static int
bin_compadd(char *name, char **argv, char *ops, int func)
{
    char *p, **sp, *e;
    char *ipre = NULL, *ppre = NULL, *psuf = NULL, *prpre = NULL;
    char *pre = NULL, *suf = NULL, *group = NULL, *m = NULL, *rs = NULL;
    int f = 0, a = 0, dm;
    Cmatcher match = NULL;

    if (incompfunc != 1) {
	zerrnam(name, "can only be called from completion function", NULL, 0);
	return 1;
    }
    for (; *argv && **argv ==  '-'; argv++) {
	for (p = *argv + 1; *p; p++) {
	    sp = NULL;
	    dm = 0;
	    switch (*p) {
	    case 'q':
		f |= CMF_REMOVE;
		break;
	    case 'Q':
		a |= CAF_QUOTE;
		break;
	    case 'f':
		f |= CMF_FILE;
		break;
	    case 'F':
		a |= CAF_FIGNORE;
		break;
	    case 'n':
		f |= CMF_NOLIST;
		break;
	    case 'U':
		a |= CAF_MENU;
		break;
	    case 'P':
		sp = &pre;
		e = "string expected after -%c";
		break;
	    case 'S':
		sp = &suf;
		e = "string expected after -%c";
		break;
	    case 'J':
		sp = &group;
		e = "group name expected after -%c";
		break;
	    case 'V':
		if (!group)
		    a |= CAF_NOSORT;
		sp = &group;
		e = "group name expected after -%c";
		break;
	    case 'i':
		sp = &ipre;
		e = "string expected after -%c";
		break;
	    case 'p':
		sp = &ppre;
		e = "string expected after -%c";
		break;
	    case 's':
		sp = &psuf;
		e = "string expected after -%c";
		break;
	    case 'W':
		sp = &prpre;
		e = "string expected after -%c";
		break;
	    case 'a':
		a |= CAF_ALT;
		break;
	    case 'm':
		a |= CAF_MATCH;
		break;
	    case 'M':
		sp = &m;
		e = "matching specification expected after -%c";
		dm = 1;
		break;
	    case 'r':
		sp = &rs;
		e = "string expected after -%c";
		break;
	    case '-':
		argv++;
		goto ca_args;
	    default:
		zerrnam(name, "bad option: -%c", NULL, *p);
		return 1;
	    }
	    if (sp) {
		if (*sp) {
		    zerrnam(name, "doubled option: -%c", NULL, *p);
		    return 1;
		}
		if (p[1]) {
		    *sp = p + 1;
		    p = "" - 1;
		} else if (argv[1]) {
		    *sp = *++argv;
		    p = "" - 1;
		} else {
		    zerrnam(name, e, NULL, *p);
		    return 1;
		}
		if (dm && (match = parse_cmatcher(name, m)) == pcm_err) {
		    match = NULL;
		    return 1;
		}
	    }
	}
    }
 ca_args:
    if (!*argv)
	return 1;

    addmatchesptr(ipre, ppre, psuf, prpre, pre, suf, group,
		  rs, f, a, match, argv);
    return 0;
}

/**/
static int
bin_compcall(char *name, char **argv, char *ops, int func)
{
    if (incompfunc != 1) {
	zerrnam(name, "can only be called from completion function", NULL, 0);
	return 1;
    }
    makecomplistctlptr((ops['T'] ? 0 : CFN_FIRST) |
		       (ops['D'] ? 0 : CFN_DEFAULT));
    return 0;
}

#define VAR(X) ((void *) (&(X)))
static struct compparam {
    char *name;
    int type;
    void *var;
} compparams[] = {
    { "CURRENT", PM_INTEGER, VAR(compcurrent) },
    { "CONTEXT", PM_SCALAR, VAR(compcontext) },
    { "COMMAND", PM_SCALAR, VAR(compcommand) },
    { "PREFIX", PM_SCALAR, VAR(compprefix) },
    { "SUFFIX", PM_SCALAR, VAR(compsuffix) },
    { "IPREFIX", PM_SCALAR, VAR(compiprefix) },
    { "NMATCHES", PM_INTEGER, VAR(compnmatches) },
    { "MATCHER", PM_INTEGER, VAR(compmatcher) },
    { NULL, 0, NULL }
};

/**/
void makecompparams(void)
{
    struct compparam *cp;

    for (cp = compparams; cp->name; cp++) {
	Param pm = createparam(cp->name, cp->type | PM_SPECIAL|PM_REMOVABLE);
	if (!pm)
	    pm = (Param) paramtab->getnode(paramtab, cp->name);
	DPUTS(!pm, "param not set in makecompparams");

	pm->level = locallevel;
	pm->u.data = cp->var;
	switch(PM_TYPE(cp->type)) {
	case PM_SCALAR:
	    pm->sets.cfn = strvarsetfn;
	    pm->gets.cfn = strvargetfn;
	    break;
	case PM_INTEGER:
	    pm->sets.ifn = intvarsetfn;
	    pm->gets.ifn = intvargetfn;
	    break;
	}
	pm->unsetfn = compunsetfn;
    }
}

/**/
static void
compunsetfn(Param pm, int exp)
{
    if (exp)
	stdunsetfn(pm, exp);
}

/**/
static int
comp_wrapper(List list, FuncWrap w, char *name)
{
    if (incompfunc != 1)
	return 1;
    else {
	char *octxt, *ocmd, *opre, *osuf, *oipre;
	long ocur;

	ocur = compcurrent;
	octxt = dupstring(compcontext);
	ocmd = dupstring(compcommand);
	opre = dupstring(compprefix);
	osuf = dupstring(compsuffix);
	oipre = dupstring(compiprefix);

	runshfunc(list, w, name);

	compcurrent = ocur;
	zsfree(compcontext);
	compcontext = ztrdup(octxt);
	zsfree(compcommand);
	compcommand = ztrdup(ocmd);
	zsfree(compprefix);
	compprefix = ztrdup(opre);
	zsfree(compsuffix);
	compsuffix = ztrdup(osuf);
	zsfree(compiprefix);
	compiprefix = ztrdup(oipre);

	return 0;
    }
}

/**/
static void
ignore_prefix(int l)
{
    char *o, sav = compprefix[l];

    compprefix[l] = '\0';
    o = compiprefix;
    compiprefix = tricat(o, compprefix, "");
    zsfree(o);
    compprefix[l] = sav;
    o = compprefix;
    compprefix = ztrdup(o + l);
    zsfree(o);
}

/**/
static int
comp_check(void)
{
    if (incompfunc != 1) {
	zerr("condition can only be used in completion function", NULL, 0);
	return 0;
    }
    return 1;
}

/**/
static void
restrict_range(int b, int e)
{
    int i = e - b;
    char **p = (char **) zcalloc((i + 1) * sizeof(char *)), **q, **pp;

    for (q = p, pp = pparams + b + 1; i; i--, q++, pp++)
	*q = ztrdup(*pp);
    zsfree(compcommand);
    compcommand = ztrdup(pparams[b]);
    freearray(pparams);
    pparams = p;
    zsfree(compcontext);
    if ((compcurrent -= b + 1))
	compcontext = ztrdup("arg");
    else
	compcontext = ztrdup("cmd");
}

/**/
static int
cond_prefix(char **a, int id)
{
    if (comp_check())
	return strpfx(cond_str(a, 0), compprefix);
    return 0;
}

/**/
static int
cond_iprefix(char **a, int id)
{
    if (comp_check()) {
	char *s = cond_str(a, 0);

	if (strpfx(s, compprefix)) {
	    ignore_prefix(strlen(s));
	    return 1;
	}
    }
    return 0;
}

/**/
static int
cond_position(char **a, int id)
{
    if (comp_check()) {
	int b = cond_val(a, 0), e = (a[1] ? cond_val(a, 1) : b);
	int l = arrlen(pparams), t, i = compcurrent - 1;

	if (b > 0)
	    b--;
	if (e > 0)
	    e--;
	if (b < 0)
	    b += l;
	if (e < 0)
	    e += l;
	t = (b >= 0 && e >= 0 && i >= b && i <= e && b <= e);

	if (t && a[1]) {
	    if (b > l)
		b = l;
	    if (e > l)
		e = l;
	    restrict_range(b, e);
	}
	return t;
    }
    return 0;
}

/**/
static int
cond_word(char **a, int id)
{
    if (comp_check()) {
	int o = ((id & 2) ? compcurrent : 0) + cond_val(a, 0);
	int l = arrlen(pparams);
	char *s;

	if (o < 0)
	    o += l;

	o--;
	if (o < 0 || o >= l)
	    return 0;

	s = pparams[o];
	return ((id & 1) ? cond_match(a, 1, s) : !strcmp(s, cond_str(a, 1)));
    }
    return 0;
}

/**/
static int
cond_strcl(char **a, int id)
{
    if (comp_check()) {
	char *s;
	int i;

	if (a[1]) {
	    s = cond_str(a, 1);
	    i = cond_val(a, 0);
	} else {
	    s = cond_str(a, 0);
	    i = -1;
	}
	if (!getcpatptr) {
	    zerr("zle not loaded, zle condition not available", NULL, 0);
	    return 1;
	}
	i = getcpatptr(comp_strptr(NULL, NULL), i, s, id);
	if (i != -1) {
	    ignore_prefix(i);
	    return 1;
	}
    }
    return 0;
}

/**/
static int
cond_words(char **a, int id)
{
    if (comp_check()) {
	int b = cond_val(a, 0), e = (a[1] ? cond_val(a, 1) : -1);
	int l = arrlen(pparams);

	return (l >= b && l <= e);
    }
    return 0;
}

/**/
static int
cond_range(char **a, int id)
{
    if (comp_check()) {
	char *s, **p;
	int i, l = arrlen(pparams), t = 0, b = 0, e = l - 1;
	Comp c;

	i = compcurrent - 1;
	if (i < 0 || i >= l)
	    return 0;

	if (id & 1) {
	    s = a[0];
	    singsub(&s);
	    c = parsereg(s);
	} else
	    s = cond_str(a, 0);

	for (i--, p = pparams + i; i >= 0; p--, i--) {
	    if (((id & 1) ? domatch(*p, c, 0) : !strcmp(*p, s))) {
		b = i + 1;
		t = 1;
		break;
	    }
	}
	if (t && (id & 2)) {
	    int tt = 0;

	    if (id & 1) {
		s = a[1];
		singsub(&s);
		c = parsereg(s);
	    } else
		s = cond_str(a, 1);

	    for (i++, p = pparams + i; i < l; p++, i++) {
		if (((id & 1) ? domatch(*p, c, 0) : !strcmp(*p, s))) {
		    e = i - 1;
		    tt = 1;
		    break;
		}
	    }
	    if (tt && i < compcurrent)
		t = 0;
	}
	if (e < b)
	    t = 0;
	if (t)
	    restrict_range(b, e);
	return t;
    }
    return 0;
}

/**/
static int
cond_nmatches(char **a, int id)
{
    if (comp_check())
	return compnmatches == cond_val(a, 0);
    return 0;
}

/**/
static int
cond_matcher(char **a, int id)
{
    if (comp_check())
	return compmatcher == cond_val(a, 0);
    return 0;
}

static struct builtin bintab[] = {
    BUILTIN("compctl", 0, bin_compctl, 0, -1, 0, NULL, NULL),
    BUILTIN("complist", 0, bin_complist, 1, -1, 0, NULL, NULL),
    BUILTIN("compadd", 0, bin_compadd, 0, -1, 0, NULL, NULL),
    BUILTIN("compcall", 0, bin_compcall, 0, 0, 0, "TD", NULL),
};

static struct conddef cotab[] = {
    CONDDEF("prefix", 0, cond_prefix, 1, 1, 0),
    CONDDEF("iprefix", 0, cond_iprefix, 1, 1, 0),
    CONDDEF("position", 0, cond_position, 1, 2, 0),
    CONDDEF("word", 0, cond_word, 2, 2, 0),
    CONDDEF("mword", 0, cond_word, 2, 2, 1),
    CONDDEF("current", 0, cond_word, 2, 2, 2),
    CONDDEF("mcurrent", 0, cond_word, 2, 2, 3),
    CONDDEF("string", 0, cond_strcl, 1, 2, 0),
    CONDDEF("class", 0, cond_strcl, 1, 2, 1),
    CONDDEF("words", 0, cond_words, 1, 2, 0),
    CONDDEF("between", 0, cond_range, 2, 2, 2),
    CONDDEF("mbetween", 0, cond_range, 2, 2, 3),
    CONDDEF("after", 0, cond_range, 1, 1, 0),
    CONDDEF("mafter", 0, cond_range, 1, 1, 1),
    CONDDEF("nmatches", 0, cond_nmatches, 1, 1, 0),
    CONDDEF("matcher", 0, cond_matcher, 1, 1, 0),
};

static struct funcwrap wrapper[] = {
    WRAPDEF(comp_wrapper),
};

/**/
int
setup_compctl(Module m)
{
    compctltab->printnode = printcompctlp;
    makecompparamsptr = makecompparams;
    return 0;
}

/**/
int
boot_compctl(Module m)
{
    if(!(addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab)) |
	 addconddefs(m->nam, cotab, sizeof(cotab)/sizeof(*cotab)) |
	 !addwrapper(m, wrapper)))
	return 1;
    return 0;
}

#ifdef MODULE

/**/
int
cleanup_compctl(Module m)
{
    deletebuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
    deleteconddefs(m->nam, cotab, sizeof(cotab)/sizeof(*cotab));
    deletewrapper(m, wrapper);
    return 0;
}

/**/
int
finish_compctl(Module m)
{
    compctltab->printnode = NULL;
    makecompparamsptr = NULL;
    return 0;
}

#endif
