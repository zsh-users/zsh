/*
 * computil.c - completion utilities
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 1999 Sven Wischnowsky
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Sven Wischnowsky or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Sven Wischnowsky and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Sven Wischnowsky and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Sven Wischnowsky and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "computil.mdh"
#include "computil.pro"


/* Help for `_display'. */

/* Calculation state. */

typedef struct cdisp *Cdisp;

struct cdisp {
    int pre;			/* prefix length */
    int suf;			/* suffix length */
    int colon;			/* number of strings with descriptions */
};

/* Calculate longest prefix and suffix and count the strings with
 * descriptions. */

static void
cdisp_calc(Cdisp disp, char **args)
{
    char *cp;
    int i, nbc;

    for (; *args; args++) {
	for (nbc = 0, cp = *args; *cp && *cp != ':'; cp++)
	    if (*cp == '\\' && cp[1])
		cp++, nbc++;
	if (*cp == ':' && cp[1]) {
	    disp->colon++;
	    if ((i = cp - *args - nbc) > disp->pre)
		disp->pre = i;
	    if ((i = strlen(cp + 1)) > disp->suf)
		disp->suf = i;
	}
    }
}

/* Help fuer `_describe'. */

typedef struct cdset *Cdset;

struct cdstate {
    int showd;			/* != 0 if descriptions should be shown */
    char *sep;			/* the separator string */
    Cdset sets;			/* the sets of matches */
    struct cdisp disp;		/* used to calculate the alignment */
};

struct cdset {
    Cdset next;			/* guess what */
    char **opts;		/* the compadd-options */
    char **strs;		/* the display-strings */
    char **matches;		/* the matches (or NULL) */
};

static struct cdstate cd_state;
static int cd_parsed = 0;

static void
freecdsets(Cdset p)
{
    Cdset n;

    for (; p; p = n) {
	n = p->next;
	if (p->opts)
	    freearray(p->opts);
	if (p->strs)
	    freearray(p->strs);
	if (p->matches)
	    freearray(p->matches);
	zfree(p, sizeof(*p));
    }
}

/* Initialisation. Store and calculate the string and matches and so on. */

static int
cd_init(char *nam, char *sep, char **args, int disp)
{
    Cdset *setp, set;
    char **ap, *tmp;

    if (cd_parsed) {
	zsfree(cd_state.sep);
	freecdsets(cd_state.sets);
    }
    setp = &(cd_state.sets);
    cd_state.sep = ztrdup(sep);
    cd_state.sets = NULL;
    cd_state.disp.pre = cd_state.disp.suf = cd_state.disp.colon = 0;
    cd_state.showd = disp;

    while (*args) {
	*setp = set = (Cdset) zcalloc(sizeof(*set));
	setp = &(set->next);

	if (!(ap = get_user_var(*args))) {
	    zwarnnam(nam, "invalid argument: %s", *args, 0);
	    return 1;
	}
	set->strs = zarrdup(ap);

	if (disp)
	    cdisp_calc(&(cd_state.disp), set->strs);

	if (*++args && **args != '-') {
	    if (!(ap = get_user_var(*args))) {
		zwarnnam(nam, "invalid argument: %s", *args, 0);
		return 1;
	    }
	    set->matches = zarrdup(ap);
	    args++;
	}
	for (ap = args; *args &&
		 (args[0][0] != '-' || args[0][1] != '-' || args[0][2]);
	     args++);

	tmp = *args;
	*args = NULL;
	set->opts = zarrdup(ap);
	if ((*args = tmp))
	    args++;
    }
    return 0;
}

/* Get the next set. */

static int
cd_get(char **params)
{
    Cdset set;

    if ((set = cd_state.sets)) {
	char **sd, **sdp, **md, **mdp, **ss, **ssp, **ms, **msp;
	char **p, **mp, *cp, *copy, *cpp, oldc;
	int dl = 1, sl = 1, sepl = strlen(cd_state.sep);
	int pre = cd_state.disp.pre, suf = cd_state.disp.suf;
	VARARR(char, buf, pre + suf + sepl + 1);

	for (p = set->strs; *p; p++)
	    if (cd_state.showd) {
		for (cp = *p; *cp && *cp != ':'; cp++)
		    if (*cp == '\\' && cp[1])
			cp++;
		if (*cp == ':' && cp[1])
		    dl++;
		else
		    sl++;
	    } else
		sl++;

	sd = (char **) zalloc(dl * sizeof(char *));
	ss = (char **) zalloc(sl * sizeof(char *));
	md = (char **) zalloc(dl * sizeof(char *));
	ms = (char **) zalloc(sl * sizeof(char *));

	if (cd_state.showd) {
	    memcpy(buf + pre, cd_state.sep, sepl);
	    suf = pre + sepl;
	}

	/* Build the aligned display strings. */

	for (sdp = sd, ssp = ss, mdp = md, msp = ms,
		 p = set->strs, mp = set->matches; *p; p++) {
	    copy = dupstring(*p);
	    for (cp = cpp = copy; *cp && *cp != ':'; cp++) {
		if (*cp == '\\' && cp[1])
		    cp++;
		*cpp++ = *cp;
	    }
	    oldc = *cpp;
	    *cpp = '\0';
	    if (((cpp == cp && oldc == ':') || *cp == ':') && cp[1] &&
		cd_state.showd) {
		memset(buf, ' ', pre);
		memcpy(buf, copy, (cpp - copy));
		strcpy(buf + suf, cp + 1);
		*sdp++ = ztrdup(buf);
		if (mp) {
		    *mdp++ = ztrdup(*mp);
		    if (*mp)
			mp++;
		} else
		    *mdp++ = ztrdup(copy);
	    } else {
		*ssp++ = ztrdup(copy);
		if (mp) {
		    *msp++ = ztrdup(*mp);
		    if (*mp)
			mp++;
		} else
		    *msp++ = ztrdup(copy);
	    }
	}
	*sdp = *ssp = *mdp = *msp = NULL;

	p = zarrdup(set->opts);

	setaparam(params[0], p);
	setaparam(params[1], sd);
	setaparam(params[2], md);
	setaparam(params[3], ss);
	setaparam(params[4], ms);

	cd_state.sets = set->next;
	set->next = NULL;
	freecdsets(set);

	return 0;
    }
    return 1;
}

/**/
static int
bin_compdescribe(char *nam, char **args, char *ops, int func)
{
    if (incompfunc != 1) {
	zwarnnam(nam, "can only be called from completion function", NULL, 0);
	return 1;
    }
    if (!args[0][0] || !args[0][1] || args[0][2]) {
	zwarnnam(nam, "invalid argument: %s", args[0], 0);
	return 1;
    }
    switch (args[0][1]) {
    case 'i':
	cd_parsed = 1;
	return cd_init(nam, "", args + 1, 0);
    case 'I':
	cd_parsed = 1;
	return cd_init(nam, args[1], args + 2, 1);
    case 'g':
	if (cd_parsed) {
	    int n = arrlen(args);

	    if (n != 6) {
		zwarnnam(nam, (n < 6 ? "not enough arguments" :
			      "too many arguments"), NULL, 0);
		return 1;
	    }
	    return cd_get(args + 1);
	} else {
	    zwarnnam(nam, "no parsed state", NULL, 0);
	    return 1;
	}
    }
    zwarnnam(nam, "invalid option: %s", args[0], 0);
    return 1;
}

/* Help for `_arguments'. */

typedef struct cadef *Cadef;
typedef struct caopt *Caopt;
typedef struct caarg *Caarg;

/* Cache for a set of _arguments-definitions. */

struct cadef {
    Cadef next;			/* next in cache */
    Caopt opts;			/* the options */
    int nopts, ndopts, nodopts;	/* number of options/direct/optional direct */
    Caarg args;			/* the normal arguments */
    Caarg rest;			/* the rest-argument */
    char **defs;		/* the original strings */
    int ndefs;			/* number of ... */
    int lastt;			/* last time this was used */
    Caopt *single;		/* array of single-letter options */
    char *match;		/* -M spec to use */
    int argsactive;		/* if arguments are still allowed */
				/* used while parsing a command line */
    char *set;			/* set name, shared */
};

/* Description for an option. */

struct caopt {
    Caopt next;
    char *name;			/* option name */
    char *descr;		/* the description */
    char **xor;			/* if this, then not ... */
    int type;			/* type, CAO_* */
    Caarg args;			/* option arguments */
    int active;			/* still allowed on command line */
    int num;			/* it's the num'th option */
    char *set;			/* set name, shared */
};

#define CAO_NEXT    1
#define CAO_DIRECT  2
#define CAO_ODIRECT 3
#define CAO_EQUAL   4

/* Description for an argument */

struct caarg {
    Caarg next;
    char *descr;		/* description */
    char **xor;			/* if this, then not ... */
    char *action;		/* what to do for it */
    int type;			/* CAA_* below */
    char *end;			/* end-pattern for ::<pat>:... */
    char *opt;			/* option name if for an option */
    int num;			/* it's the num'th argument */
    int min;			/* it's also this argument, using opt. args */
    int direct;			/* number was given directly */
    int active;			/* still allowed on command line */
    char *set;			/* set name, shared */
};

#define CAA_NORMAL 1
#define CAA_OPT    2
#define CAA_REST   3
#define CAA_RARGS  4
#define CAA_RREST  5

/* The cache of parsed descriptons. */

#define MAX_CACACHE 8
static Cadef cadef_cache[MAX_CACACHE];

/* Compare two arrays of strings for equality. */

static int
arrcmp(char **a, char **b)
{
    if (!a && !b)
	return 1;
    else if (!a || !b)
	return 0;
    else {
	while (*a && *b)
	    if (strcmp(*a++, *b++))
		return 0;

	return (!*a && !*b);
    }
}

/* Memory stuff. Obviously. */

static void
freecaargs(Caarg a)
{
    Caarg n;

    for (; a; a = n) {
	n = a->next;
	zsfree(a->descr);
	if (a->xor)
	    freearray(a->xor);
	zsfree(a->action);
	zsfree(a->end);
	zsfree(a->opt);
	zfree(a, sizeof(*a));
    }
}

static void
freecadef(Cadef d)
{
    if (d) {
	Caopt p, n;

	zsfree(d->match);
	zsfree(d->set);
	if (d->defs)
	    freearray(d->defs);

	for (p = d->opts; p; p = n) {
	    n = p->next;
	    zsfree(p->name);
	    zsfree(p->descr);
	    if (p->xor)
		freearray(p->xor);
	    freecaargs(p->args);
	    zfree(p, sizeof(*p));
	}
	freecaargs(d->args);
	freecaargs(d->rest);
	if (d->single)
	    zfree(d->single, 256 * sizeof(Caopt));
	zfree(d, sizeof(*d));
    }
}

/* Remove backslashes before colons. */

static char *
rembslashcolon(char *s)
{
    char *p, *r;

    r = p = s = dupstring(s);

    while (*s) {
	if (s[0] != '\\' || s[1] != ':')
	    *p++ = *s;
	s++;
    }
    *p = '\0';

    return r;
}

/* Add backslashes before colons. */

static char *
bslashcolon(char *s)
{
    char *p, *r;

    r = p = zhalloc((2 * strlen(s)) + 1);

    while (*s) {
	if (*s == ':')
	    *p++ = '\\';
	*p++ = *s++;
    }
    *p = '\0';

    return r;
}

/* Parse an argument definition. */

static Caarg
parse_caarg(int mult, int type, int num, int opt, char *oname, char **def,
	    char *set)
{
    Caarg ret = (Caarg) zalloc(sizeof(*ret));
    char *p = *def, *d, sav;

    ret->next = NULL;
    ret->descr = ret->action = ret->end = NULL;
    ret->xor = NULL;
    ret->num = num;
    ret->min = num - opt;
    ret->type = type;
    ret->opt = ztrdup(oname);
    ret->direct = 0;
    ret->set = set;

    /* Get the description. */

    for (d = p; *p && *p != ':'; p++)
	if (*p == '\\' && p[1])
	    p++;
    sav = *p;
    *p = '\0';
    ret->descr = ztrdup(rembslashcolon(d));

    /* Get the action if there is one. */

    if (sav) {
	if (mult) {
	    for (d = ++p; *p && *p != ':'; p++)
		if (*p == '\\' && p[1])
		    p++;
	    sav = *p;
	    *p = '\0';
	    ret->action = ztrdup(rembslashcolon(d));
	    if (sav)
		*p = ':';
	} else
	    ret->action = ztrdup(rembslashcolon(p + 1));
    }
    *def = p;

    return ret;
}

/* Parse an array of definitions. */

static Cadef
parse_cadef(char *nam, char **args, int multi)
{
    Cadef ret;
    Caopt *optp;
    Caarg argp;
    char **oargs = args, *p, *q, *match = "r:|[_-]=* r:|=*", **xor;
    char *adpre, *adsuf, *set = NULL, *doset = NULL;
    int single = 0, anum = 1, xnum, nopts, ndopts, nodopts;

    nopts = ndopts = nodopts = 0;

    if (multi) {
	if (!args[1])
	    return NULL;
	set = tricat(*args++, "-", "");
    }
    /* First string is the auto-description definition. */

    for (p = args[0]; *p && (p[0] != '%' || p[1] != 'd'); p++);

    if (*p) {
	*p = '\0';
	adpre = dupstring(args[0]);
	*p = '%';
	adsuf = dupstring(p + 2);
    } else
	adpre = adsuf = NULL;

    /* Now get the -s and -M options. */

    args++;
    while ((p = *args)) {
	if (!strcmp(p, "-s"))
	    single = 1;
	else if (p[0] == '-' && p[1] == 'M') {
	    if (p[2])
		match = p + 2;
	    else if (args[1])
		match = *++args;
	    else {
		args++;
		break;
	    }
	} else
	    break;
	args++;
    }
    if (!*args)
	return NULL;

    /* Looks good. Optimistically allocate the cadef structure. */

    ret = (Cadef) zalloc(sizeof(*ret));
    ret->next = NULL;
    ret->opts = NULL;
    ret->args = ret->rest = NULL;
    ret->defs = zarrdup(oargs);
    ret->ndefs = arrlen(oargs);
    ret->lastt = time(0);
    ret->set = set;
    if (single) {
	ret->single = (Caopt *) zalloc(256 * sizeof(Caopt));
	memset(ret->single, 0, 256 * sizeof(Caopt));
    } else
	ret->single = NULL;
    ret->match = ztrdup(match);

    /* Get the definitions. */

    for (optp = &(ret->opts); *args; args++) {
        if (args[0][0] == '-' && !args[0][1]) {
	    doset = set;
	    continue;
	}
	p = dupstring(*args);
	xnum = 0;
	if (*p == '(') {
	    /* There is a xor list, get it. */

	    LinkList list = newlinklist();
	    LinkNode node;
	    char **xp, sav;

	    while (*p && *p != ')') {
		for (p++; inblank(*p); p++);

		if (*p == ')')
		    break;
		for (q = p++; *p && *p != ')' && !inblank(*p); p++);

		if (!*p)
		    break;

		sav = *p;
		*p = '\0';
		addlinknode(list, dupstring(q));
		xnum++;
		*p = sav;
	    }
	    /* Oops, end-of-string. */
	    if (*p != ')') {
		freecadef(ret);
		zwarnnam(nam, "invalid argument: %s", *args, 0);
		return NULL;
	    }
	    xor = (char **) zalloc((xnum + 2) * sizeof(char *));
	    for (node = firstnode(list), xp = xor; node; incnode(node), xp++)
		*xp = ztrdup((char *) getdata(node));
	    xp[0] = xp[1] = NULL;

	    p++;
	} else
	    xor = NULL;

	if (*p == '-' || *p == '+' ||
	    (*p == '*' && (p[1] == '-' || p[1] == '+'))) {
	    /* It's an option. */
	    Caopt opt;
	    Caarg oargs = NULL;
	    int multi, otype = CAO_NEXT, again = 0;
	    char *name, *descr, c;

	    rec:

	    /* Allowed more than once? */
	    if ((multi = (*p == '*')))
		p++;

	    if (((p[0] == '-' && p[1] == '+') ||
		 (p[0] == '+' && p[1] == '-')) &&
		p[2] && p[2] != ':' && p[2] != '[' &&
		p[2] != '=' && p[2] != '-' && p[2] != '+') {
		/* It's a -+ or +- definition. We just execute the whole
		 * stuff twice for such things. */
		name = ++p;
		*p = (again ? '-' : '+');
		again++;
	    } else {
		name = p;
		/* If it's a long option skip over the first `-'. */
		if (p[0] == '-' && p[1] == '-')
		    p++;
	    }
	    if (!p[1]) {
		freecadef(ret);
		zwarnnam(nam, "invalid argument: %s", *args, 0);
		return NULL;
	    }

	    /* Skip over the name. */
	    for (p++; *p && *p != ':' && *p != '[' &&
		     ((*p != '-' && *p != '+' && *p != '=') ||
		      (p[1] != ':' && p[1] != '[')); p++)
		if (*p == '\\' && p[1])
		    p++;

	    /* The character after the option name specifies the type. */
	    c = *p;
	    *p = '\0';
	    if (c == '-') {
		otype = CAO_DIRECT;
		c = *++p;
	    } else if (c == '+') {
		otype = CAO_ODIRECT;
		c = *++p;
	    } else if (c == '=') {
		otype = CAO_EQUAL;
		c = *++p;
	    }
	    /* Get the optional description, if any. */
	    if (c == '[') {
		for (descr = ++p; *p && *p != ']'; p++)
		    if (*p == '\\' && p[1])
			p++;

		if (!*p) {
		    freecadef(ret);
		    zwarnnam(nam, "invalid option definition: %s", *args, 0);
		    return NULL;
		}
		*p++ = '\0';
		c = *p;
	    } else
		descr = NULL;

	    if (c && c != ':') {
		freecadef(ret);
		zwarnnam(nam, "invalid option definition: %s", *args, 0);
		return NULL;
	    }
	    /* Add the option name to the xor list if not `*-...'. */
	    if (!multi) {
		if (!xor) {
		    xor = (char **) zalloc(2 * sizeof(char *));
		    xor[1] = NULL;
		}
		xor[xnum] = ztrdup(name);
	    }
	    if (c == ':') {
		/* There's at least one argument. */

		Caarg *oargp = &oargs;
		int atype, rest, oanum = 1, onum = 0;
		char *end;

		/* Loop over the arguments. */

		while (c == ':') {
		    rest = 0;
		    end = NULL;

		    /* Get the argument type. */
		    if (*++p == ':') {
			atype = CAA_OPT;
			p++;
		    } else if (*p == '*') {
			if (*++p != ':') {
			    char sav;

			    for (end = p++; *p && *p != ':'; p++)
				if (*p == '\\' && p[1])
				    p++;
			    sav = *p;
			    *p = '\0';
			    end = dupstring(end);
			    tokenize(end);
			    *p = sav;
			}
			if (*p != ':') {
			    freecadef(ret);
			    freecaargs(oargs);
			    zwarnnam(nam, "invalid option definition: %s",
				    *args, 0);
			    return NULL;
			}
			if (*++p == ':') {
			    if (*++p == ':') {
				atype = CAA_RREST;
				p++;
			    } else
				atype = CAA_RARGS;
			} else
			    atype = CAA_REST;
			rest = 1;
		    } else
			atype = CAA_NORMAL;

		    /* And the definition. */

		    *oargp = parse_caarg(!rest, atype, oanum++, onum,
					 name, &p, doset);
		    if (atype == CAA_OPT)
			onum++;
		    if (end)
			(*oargp)->end = ztrdup(end);
		    oargp = &((*oargp)->next);
		    if (rest)
			break;
		    c = *p;
		}
	    }
	    /* Store the option definition. */

	    *optp = opt = (Caopt) zalloc(sizeof(*opt));
	    optp = &((*optp)->next);

	    opt->next = NULL;
	    opt->set = doset;
	    opt->name = ztrdup(rembslashcolon(name));
	    if (descr)
		opt->descr = ztrdup(descr);
	    else if (adpre && oargs && !oargs->next) {
		char *d;

		for (d = oargs->descr; *d; d++)
		    if (!iblank(*d))
			break;

		if (*d)
		    opt->descr = tricat(adpre, oargs->descr, adsuf);
		else
		    opt->descr = NULL;
	    } else
		opt->descr = NULL;
	    opt->xor = (again == 1 ? zarrdup(xor) : xor);
	    opt->type = otype;
	    opt->args = oargs;
	    opt->num = nopts++;

	    if (otype == CAO_DIRECT)
		ndopts++;
	    else if (otype == CAO_ODIRECT || otype == CAO_EQUAL)
		nodopts++;

	    /* If this is for single-letter option we also store a
	     * pointer for the definition in the array for fast lookup. */

	    if (single && name[1] && !name[2])
		ret->single[STOUC(name[1])] = opt;

	    if (again == 1) {
		/* Do it all again for `*-...'. */
		p = dupstring(*args);
		goto rec;
	    }
	} else if (*p == '*') {
	    /* It's a rest-argument definition. */

	    int type = CAA_REST;

	    if (*++p != ':') {
		freecadef(ret);
		zwarnnam(nam, "invalid rest argument definition: %s", *args, 0);
		return NULL;
	    }
	    if (ret->rest) {
		freecadef(ret);
		zwarnnam(nam, "doubled rest argument definition: %s", *args, 0);
		return NULL;
	    }
	    if (*++p == ':') {
		if (*++p == ':') {
		    type = CAA_RREST;
		    p++;
		} else
		    type = CAA_RARGS;
	    }
	    ret->rest = parse_caarg(0, type, -1, 0, NULL, &p, doset);
	    ret->rest->xor = xor;
	} else {
	    /* It's a normal argument definition. */

	    int type = CAA_NORMAL, direct;
	    Caarg arg, tmp, pre;

	    if ((direct = idigit(*p))) {
		/* Argment number is given. */
		int num = 0;

		while (*p && idigit(*p))
		    num = (num * 10) + (((int) *p++) - '0');

		anum = num + 1;
	    } else
		/* Default number. */
		anum++;

	    if (*p != ':') {
		freecadef(ret);
		zwarnnam(nam, "invalid argument: %s", *args, 0);
		return NULL;
	    }
	    if (*++p == ':') {
		/* Optional argument. */
		type = CAA_OPT;
		p++;
	    }
	    arg = parse_caarg(0, type, anum - 1, 0, NULL, &p, doset);
	    arg->xor = xor;
	    arg->direct = direct;

	    /* Sort the new definition into the existing list. */

	    for (tmp = ret->args, pre = NULL;
		 tmp && tmp->num < anum - 1;
		 pre = tmp, tmp = tmp->next);

	    if (tmp && tmp->num == anum - 1) {
		freecadef(ret);
		freecaargs(arg);
		zwarnnam(nam, "doubled argument definition: %s", *args, 0);
		return NULL;
	    }
	    arg->next = tmp;
	    if (pre)
		pre->next = arg;
	    else
		ret->args = arg;
	}
    }
    ret->nopts = nopts;
    ret->ndopts = ndopts;
    ret->nodopts = nodopts;

    for (argp = ret->args, xnum = 0; argp; argp = argp->next) {
	if (!argp->direct)
	    argp->min = argp->num - xnum;
	if (argp->type == CAA_OPT)
	    xnum++;
    }
    return ret;
}

/* Given an array of definitions, return the cadef for it. From the cache
 * are newly built. */

static Cadef
get_cadef(char *nam, char **args, int multi)
{
    Cadef *p, *min, new;
    int i, na = arrlen(args);

    for (i = MAX_CACACHE, p = cadef_cache, min = NULL; *p && i; p++, i--)
	if (*p && na == (*p)->ndefs && arrcmp(args, (*p)->defs)) {
	    (*p)->lastt = time(0);

	    return *p;
	} else if (!min || !*p || (*p)->lastt < (*min)->lastt)
	    min = p;
    if (i)
	min = p;
    if ((new = parse_cadef(nam, args, multi))) {
	freecadef(*min);
	*min = new;
    }
    return new;
}

/* Get the option used in a word from the line, if any. */

static Caopt
ca_get_opt(Cadef d, char *line, int full, char **end)
{
    Caopt p;

    /* The full string may be an option. */

    for (p = d->opts; p; p = p->next)
	if (p->active && !strcmp(p->name, line)) {
	    if (end)
		*end = line + strlen(line);

	    return p;
	}

    if (!full) {
	/* The string from the line probably only begins with an option. */
	for (p = d->opts; p; p = p->next)
	    if (p->active && ((!p->args || p->type == CAO_NEXT) ?
			      !strcmp(p->name, line) : strpfx(p->name, line))) {
		if (end) {
		    /* Return a pointer to the end of the option. */
		    int l = strlen(p->name);

		    if (p->type == CAO_EQUAL && line[l] == '=')
			l++;

		    *end = line + l;
		}
		return p;
	    }
    }
    return NULL;
}

/* Same as above, only for single-letter-style. */

static Caopt
ca_get_sopt(Cadef d, char *line, int full, char **end)
{
    Caopt p;
    char pre = *line++;

    if (full) {
	for (p = NULL; *line; line++)
	    if (!(p = d->single[STOUC(*line)]) || !p->active ||
		(line[1] && p->args))
		return NULL;
	return p;
    } else {
	for (p = NULL; *line; line++)
	    if ((p = d->single[STOUC(*line)]) && p->active &&
		p->args && p->type != CAO_NEXT && p->name[0] == pre) {
		if (end) {
		    line++;
		    if (p->type == CAO_EQUAL && *line == '=')
			line++;
		    *end = line;
		}
		break;
	    } else if (!p || !p->active || (line[1] && p->args) ||
		       p->name[0] != pre)
		return NULL;
	if (p && end)
	    *end = line;
	return p;
    }
    return NULL;
}

/* Return the n'th argument definition. */

static Caarg
ca_get_arg(Cadef d, int n)
{
    if (d->argsactive) {
	Caarg a = d->args;

	while (a && (n < a->min || n > a->num))
	    a = a->next;

	if (a && a->min <= n && a->num >= n && a->active)
	    return a;

	return (d->rest && d->rest->active ? d->rest : NULL);
    }
    return NULL;
}

/* Use a xor list, marking options as inactive. */

static LinkList ca_xor;

static int
ca_inactive(Cadef d, char **xor, int cur)
{
    if (xor && cur <= compcurrent) {
	Caopt opt;
	char *x;
	int sl = (d->set ? strlen(d->set) : -1);

	for (; (x = *xor); xor++) {
	    if (ca_xor)
		addlinknode(ca_xor, x);
	    if (sl > 0) {
		if (strpfx(d->set, x))
		    x += sl;
		else if (!strncmp(d->set, x, sl - 1))
		    return 1;
	    }
	    if (x[0] == ':' && !x[1])
		d->argsactive = 0;
	    else if (x[0] == '*' && !x[1]) {
		if (d->rest)
		    d->rest->active = 0;
	    } else if (x[0] >= '0' && x[0] <= '9') {
		int n = atoi(x);
		Caarg a = d->args;

		while (a && a->num < n)
		    a = a->next;

		if (a && a->num == n)
		    a->active = 0;
	    } else if ((opt = ca_get_opt(d, x, 1, NULL)))
		opt->active = 0;
	}
    }
    return 0;
}

/* State when parsing a command line. */

struct castate {
    Cadef d;
    int nopts;
    Caarg def, ddef;
    Caopt curopt;
    int opt, arg, argbeg, optbeg, nargbeg, restbeg, curpos;
    int inopt, inrest, inarg, nth, doff, singles, oopt;
    LinkList args;
    LinkList *oargs;
};

static struct castate ca_laststate;
static int ca_parsed = 0, ca_alloced = 0;

/* Parse a command line. */

static int
ca_parse_line(Cadef d, int multi)
{
    Caarg adef, ddef;
    Caopt ptr, wasopt;
    struct castate state;
    char *line, *pe, **argxor = NULL;
    int cur, doff;
    Patprog endpat = NULL;

    /* Free old state. */

    if (ca_alloced) {
	int i = ca_laststate.nopts;
	LinkList *p = ca_laststate.oargs;

	freelinklist(ca_laststate.args, freestr);
	while (i--)
	    if (*p++)
		freelinklist(p[-1], freestr);

	zfree(ca_laststate.oargs, ca_laststate.d->nopts * sizeof(LinkList));
    }
    /* Mark everything as active. */

    for (ptr = d->opts; ptr; ptr = ptr->next)
	ptr->active = 1;
    d->argsactive = 1;
    if (d->rest)
	d->rest->active = 1;
    for (adef = d->args; adef; adef = adef->next)
	adef->active = 1;

    /* Default values for the state. */

    state.d = d;
    state.nopts = d->nopts;
    state.def = state.ddef = NULL;
    state.curopt = NULL;
    state.argbeg = state.optbeg = state.nargbeg = state.restbeg =
	state.nth = state.inopt = state.inarg = state.opt = state.arg = 1;
    state.inrest = state.doff = state.singles = state.doff = state.oopt = 0;
    state.curpos = compcurrent;
    state.args = znewlinklist();
    state.oargs = (LinkList *) zalloc(d->nopts * sizeof(LinkList));
    memset(state.oargs, 0, d->nopts * sizeof(LinkList));

    ca_alloced = 1;

    memcpy(&ca_laststate, &state, sizeof(state));

    if (!compwords[1]) {
	ca_laststate.opt = ca_laststate.arg = 0;

	return 0;
    }
    /* Loop over the words from the line. */

    for (line = compwords[1], cur = 2, state.curopt = NULL, state.def = NULL;
	 line; line = compwords[cur++]) {
	ddef = adef = NULL;
	doff = state.singles = 0;

	if (ca_inactive(d, argxor, cur))
	    return 1;

	/* We've a definition for an argument, skip to the next. */

	if (state.def) {
	    state.arg = 0;
	    if (state.curopt)
		zaddlinknode(state.oargs[state.curopt->num], ztrdup(line));

	    if ((state.opt = (state.def->type == CAA_OPT)) && state.def->opt)
		state.oopt++;

	    if (state.def->type == CAA_REST || state.def->type == CAA_RARGS ||
		state.def->type == CAA_RREST) {
		if (state.def->end && pattry(endpat, line)) {
		    state.def = NULL;
		    state.curopt = NULL;
		    state.opt = state.arg = 1;
		    continue;
		}
	    } else if ((state.def = state.def->next))
		state.argbeg = cur;
	    else {
		state.curopt = NULL;
		state.opt = 1;
	    }
	} else {
	    state.opt = state.arg = 1;
	    state.curopt = NULL;
	}
	if (state.opt)
	    state.opt = (line[0] ? (line[1] ? 2 : 1) : 0);

	pe = NULL;

	wasopt = NULL;

	/* See if it's an option. */

	if (state.opt == 2 && (state.curopt = ca_get_opt(d, line, 0, &pe)) &&
	    (state.curopt->type != CAO_EQUAL || 
	     compwords[cur] || pe[-1] == '=')) {

	    ddef = state.def = state.curopt->args;
	    doff = pe - line;
	    state.optbeg = state.argbeg = state.inopt = cur;
	    state.singles = (d->single && (!pe || !*pe) &&
			     state.curopt->name[1] && !state.curopt->name[2]);

	    state.oargs[state.curopt->num] = znewlinklist();

	    if (ca_inactive(d, state.curopt->xor, cur))
		return 1;

	    /* Collect the argument strings. Maybe. */

	    if (state.def &&
		(state.curopt->type == CAO_DIRECT ||
		 (state.curopt->type == CAO_ODIRECT && pe[0]) ||
		 (state.curopt->type == CAO_EQUAL &&
		  (pe[0] || pe[-1] == '=')))) {
		if (state.def->type != CAA_REST &&
		    state.def->type != CAA_RARGS &&
		    state.def->type != CAA_RREST)
		    state.def = state.def->next;

		zaddlinknode(state.oargs[state.curopt->num], ztrdup(pe));
	    }
	    if (state.def)
		state.opt = 0;
	    else {
		if (!d->single || (state.curopt->name[1] && state.curopt->name[2]))
		    wasopt = state.curopt;
		state.curopt = NULL;
	    }
	} else if (state.opt == 2 && d->single &&
		   (state.curopt = ca_get_sopt(d, line, 0, &pe))) {
	    /* Or maybe it's a single-letter option? */

	    char *p;
	    Caopt tmpopt;

	    ddef = state.def = state.curopt->args;
	    doff = pe - line;
	    state.optbeg = state.argbeg = state.inopt = cur;
	    state.singles = (!pe || !*pe);

	    for (p = line + 1; p < pe; p++) {
		if ((tmpopt = d->single[STOUC(*p)])) {
		    state.oargs[tmpopt->num] = znewlinklist();

		    if (ca_inactive(d, tmpopt->xor, cur))
			return 1;
		}
	    }
	    if (state.def &&
		(state.curopt->type == CAO_DIRECT ||
		 (state.curopt->type == CAO_ODIRECT && pe[0]) ||
		 (state.curopt->type == CAO_EQUAL &&
		  (pe[0] || pe[-1] == '=')))) {
		if (state.def->type != CAA_REST &&
		    state.def->type != CAA_RARGS &&
		    state.def->type != CAA_RREST)
		    state.def = state.def->next;

		zaddlinknode(state.oargs[state.curopt->num], ztrdup(pe));
	    }
	    if (state.def)
		state.opt = 0;
	    else
		state.curopt = NULL;
	} else if (multi && (*line == '-' || *line == '+') && cur != compcurrent)
	    return 1;
	else if (state.arg) {
	    /* Otherwise it's a normal argument. */
	    if (state.inopt) {
		state.inopt = 0;
		state.nargbeg = cur - 1;
	    }
	    if (!d->args && !d->rest)
		return 1;
	    if ((adef = state.def = ca_get_arg(d, state.nth)) &&
		(state.def->type == CAA_RREST ||
		 state.def->type == CAA_RARGS)) {
		state.inrest = 0;
		state.opt = (cur == state.nargbeg + 1);
		state.optbeg = state.nargbeg;
		state.argbeg = cur - 1;

		for (; line; line = compwords[cur++])
		    zaddlinknode(state.args, ztrdup(line));

		memcpy(&ca_laststate, &state, sizeof(state));
		ca_laststate.ddef = NULL;
		ca_laststate.doff = 0;
		break;
	    }
	    zaddlinknode(state.args, ztrdup(line));

	    if (state.def)
		argxor = state.def->xor;

	    if (state.def && state.def->type != CAA_NORMAL &&
		state.def->type != CAA_OPT && state.inarg) {
		state.restbeg = cur;
		state.inarg = 0;
	    } else if (!state.def || state.def->type == CAA_NORMAL ||
		       state.def->type == CAA_OPT)
		state.inarg = 1;
	    state.nth++;
	    state.def = NULL;
	}
	/* Do the end-pattern test if needed. */

	if (state.def && state.curopt &&
	    (state.def->type == CAA_RREST || state.def->type == CAA_RARGS)) {
	    if (state.def->end)
		endpat = patcompile(state.def->end, 0, NULL);
	    else {
		LinkList l = state.oargs[state.curopt->num];

		if (cur < compcurrent)
		    memcpy(&ca_laststate, &state, sizeof(state));

		for (; line; line = compwords[cur++])
		    zaddlinknode(l, ztrdup(line));

		ca_laststate.ddef = NULL;
		ca_laststate.doff = 0;
		break;
	    }
	} else if (state.def && state.def->end)
	    endpat = patcompile(state.def->end, 0, NULL);

	/* Copy the state into the global one. */

	if (cur + 1 == compcurrent) {
	    memcpy(&ca_laststate, &state, sizeof(state));
	    ca_laststate.ddef = NULL;
	    ca_laststate.doff = 0;
	} else if (cur == compcurrent && !ca_laststate.def) {
	    if ((ca_laststate.def = ddef)) {
		ca_laststate.singles = state.singles;
		if (state.curopt && state.curopt->type == CAO_NEXT) {
		    ca_laststate.ddef = ddef;
		    ca_laststate.def = NULL;
		    ca_laststate.opt = 1;
		    state.curopt->active = 1;
		} else {
		    ca_laststate.doff = doff;
		    ca_laststate.opt = 0;
		}
	    } else {
		ca_laststate.def = adef;
		ca_laststate.ddef = NULL;
		ca_laststate.optbeg = state.nargbeg;
		ca_laststate.argbeg = state.restbeg;
		ca_laststate.singles = state.singles;
		if (wasopt)
		    wasopt->active = 1;
	    }
	}
    }
    return 0;
}

/* Build a colon-list from a list. */

static char *
ca_colonlist(LinkList l)
{
    if (l) {
	LinkNode n;
	int len = 0;
	char *p, *ret, *q;

	for (n = firstnode(l); n; incnode(n)) {
	    len++;
	    for (p = (char *) getdata(n); *p; p++)
		len += (*p == ':' ? 2 : 1);
	}
	ret = q = (char *) zalloc(len);

	for (n = firstnode(l); n;) {
	    for (p = (char *) getdata(n); *p; p++) {
		if (*p == ':')
		    *q++ = '\\';
		*q++ = *p;
	    }
	    incnode(n);
	    if (n)
		*q++ = ':';
	}
	*q = '\0';

	return ret;
    } else
	return ztrdup("");
}

static void
ca_set_data(char *opt, Caarg arg, char **args, int single)
{
    LinkList descr, act, subc;
    char nbuf[40], *buf;
    int restr = 0, onum, miss = 0, rest, oopt = 1, lopt = 0, addopt;

    descr = newlinklist();
    act = newlinklist();
    subc = newlinklist();

 rec:

    addopt = (opt ? 0 : ca_laststate.oopt);

    for (; arg && (opt || (arg->num < 0 ||
			   (arg->min <= ca_laststate.nth + addopt &&
			    arg->num >= ca_laststate.nth)));) {
	lopt = (arg->type == CAA_OPT);
	if (!opt && !lopt && oopt > 0)
	    oopt = 0;

	addlinknode(descr, arg->descr);
	addlinknode(act, arg->action);

	if (!restr) {
	    if ((restr = (arg->type == CAA_RARGS)))
		restrict_range(ca_laststate.optbeg, arrlen(compwords) - 1);
	    else if ((restr = (arg->type == CAA_RREST)))
		restrict_range(ca_laststate.argbeg, arrlen(compwords) - 1);
	}
	if (arg->opt) {
	    buf = (char *) zhalloc((arg->set ? strlen(arg->set) : 0) +
				   strlen(arg->opt) + 40);
	    if (arg->num > 0)
		sprintf(buf, "%soption%s-%d",
			(arg->set ? arg->set : ""), arg->opt, arg->num);
	    else
		sprintf(buf, "%soption%s-rest",
			(arg->set ? arg->set : ""), arg->opt);
	} else if (arg->num > 0) {
	    sprintf(nbuf, "argument-%d", arg->num);
	    buf = (arg->set ? dyncat(arg->set, nbuf) : dupstring(nbuf));
	} else
	    buf = (arg->set ? dyncat(arg->set, "argument-rest") :
		   dupstring("argument-rest"));

	addlinknode(subc, buf);

	if (single)
	    break;

	if (!opt && arg->num >= 0 && !arg->next && miss)
	    arg = ca_laststate.d->rest;
	else {
	    onum = arg->num;
	    rest = (onum != arg->min && onum == ca_laststate.nth);
	    if ((arg = arg->next)) {
		if (arg->num != onum + 1)
		    miss = 1;
	    } else if (rest || (oopt > 0 && !opt)) {
		arg = ca_laststate.d->rest;
		oopt = -1;
	    }
	}
    }
    if (!single && opt && lopt) {
	opt = NULL;
	arg = ca_get_arg(ca_laststate.d, ca_laststate.nth);

	goto rec;
    }
    if (!opt && oopt > 0) {
	oopt = -1;
	arg = ca_laststate.d->rest;

	goto rec;
    }
    set_list_array(args[0], descr);
    set_list_array(args[1], act);
    set_list_array(args[2], subc);
}

static int
bin_comparguments(char *nam, char **args, char *ops, int func)
{
    int min, max, n;

    if (incompfunc != 1) {
	zwarnnam(nam, "can only be called from completion function", NULL, 0);
	return 1;
    }
    if (args[0][0] != '-' || !args[0][1] || args[0][2]) {
	zwarnnam(nam, "invalid argument: %s", args[0], 0);
	return 1;
    }
    if (args[0][1] != 'i' && args[0][1] != 'I' && !ca_parsed) {
	zwarnnam(nam, "no parsed state", NULL, 0);
	return 1;
    }
    switch (args[0][1]) {
    case 'i':
    case 'I': min = 2; max = -1; break;
    case 'D': min = 3; max =  3; break;
    case 'O': min = 4; max =  4; break;
    case 'L': min = 3; max =  4; break;
    case 's': min = 1; max =  1; break;
    case 'M': min = 1; max =  1; break;
    case 'a': min = 0; max =  0; break;
    case 'W': min = 2; max =  2; break;
    default:
	zwarnnam(nam, "invalid option: %s", args[0], 0);
	return 1;
    }
    n = arrlen(args) - 1;
    if (n < min) {
	zwarnnam(nam, "not enough arguments", NULL, 0);
	return 1;
    } else if (max >= 0 && n > max) {
	zwarnnam(nam, "too many arguments", NULL, 0);
	return 1;
    }
    switch (args[0][1]) {
    case 'i':
    case 'I':
	if (compcurrent > 1 && compwords[0]) {
	    Cadef def;
	    int cap = ca_parsed;
	    LinkList cax = ca_xor;

	    ca_parsed = 0;

	    if (args[0][1] == 'I') {
		char **xor;

		if (!(def = get_cadef(nam, args + 2, 1)))
		    return 1;

		ca_parsed = cap;
		ca_xor = newlinklist();
		if ((xor = getaparam(args[1]))) {
		    if (arrcontains(xor, args[2], 0) ||
			ca_inactive(def, xor, compcurrent)) {
			ca_xor = cax;
			return 1;
		    }
		}
		if (ca_parse_line(def, 1)) {
		    ca_xor = cax;
		    return 1;
		}
		set_list_array(args[1], ca_xor);
	    } else {
		if (!(def = get_cadef(nam, args + 1, 0)))
		    return 1;

		ca_parsed = cap;
		ca_xor = NULL;
		ca_parse_line(def, 0);
	    }
	    ca_xor = cax;
	    ca_parsed = 1;

	    return 0;
	}
	return 1;

    case 'D':
	{
	    Caarg arg = ca_laststate.def;

	    if (arg) {
		if (ca_laststate.doff > 0)
		    ignore_prefix(ca_laststate.doff);

		ca_set_data(arg->opt, arg, args + 1, (ca_laststate.doff > 0));

		return 0;
	    }
	    return 1;
	}
    case 'O':
	if ((ca_laststate.opt || (ca_laststate.doff && ca_laststate.def) ||
	     (ca_laststate.def &&
	      (ca_laststate.def->type == CAA_OPT ||
	       (ca_laststate.def->type >= CAA_RARGS &&
		ca_laststate.def->num < 0)))) &&
	    (!ca_laststate.def || ca_laststate.def->type < CAA_RARGS ||
	     (ca_laststate.def->type == CAA_RARGS ?
	      (ca_laststate.curpos == ca_laststate.argbeg + 1) :
	      (compcurrent == 1)))) {
	    LinkList next = newlinklist();
	    LinkList direct = newlinklist();
	    LinkList odirect = newlinklist();
	    LinkList equal = newlinklist(), l;
	    Caopt p;
	    char *str;

	    for (p = ca_laststate.d->opts; p; p = p->next) {
		if (p->active) {
		    switch (p->type) {
		    case CAO_NEXT:    l = next;    break;
		    case CAO_DIRECT:  l = direct;  break;
		    case CAO_ODIRECT: l = odirect; break;
		    default:          l = equal;   break;
		    }
		    if (p->descr) {
			char *n = bslashcolon(p->name);
			int len = strlen(n) + strlen(p->descr) + 2;

			str = (char *) zhalloc(len);
			strcpy(str, n);
			strcat(str, ":");
			strcat(str, p->descr);
		    } else
			str = bslashcolon(p->name);
		    addlinknode(l, str);
		}
	    }
	    set_list_array(args[1], next);
	    set_list_array(args[2], direct);
	    set_list_array(args[3], odirect);
	    set_list_array(args[4], equal);

	    return 0;
	}
	return 1;
    case 'L':
	{
	    Caopt opt = ca_get_opt(ca_laststate.d, args[1], 1, NULL);

	    if (opt && opt->args) {
		ca_set_data(opt->name, opt->args, args + 2, 1);

		return 0;
	    }
	    return 1;
	}
    case 's':
	if (ca_laststate.d->single && ca_laststate.singles &&
	    ca_laststate.opt) {
	    setsparam(args[1],
		      ztrdup(ca_laststate.ddef ?
			     (ca_laststate.ddef->type == CAO_DIRECT ?
			      "direct" :
			      (ca_laststate.ddef->type == CAO_EQUAL ?
			       "equal" : "next")) : ""));
	    return 0;
	}
	return 1;
    case 'M':
	setsparam(args[1], ztrdup(ca_laststate.d->match));
	return 0;
    case 'a':
	return !(ca_laststate.d->args || ca_laststate.d->rest);
    case 'W':
	{
	    char **ret, **p;
	    LinkNode n;
	    LinkList *a;
	    Caopt o;
	    int num;

	    ret = p = zalloc((countlinknodes(ca_laststate.args) + 1) *
			     sizeof(char *));

	    for (n = firstnode(ca_laststate.args); n; incnode(n))
		*p++ = ztrdup((char *) getdata(n));
	    *p = NULL;

	    setaparam(args[1], ret);

	    for (num = 0, o = ca_laststate.d->opts, a = ca_laststate.oargs; o;
		 o = o->next, a++)
		if (*a)
		    num += 2;

	    ret = p = zalloc((num + 1) * sizeof(char *));

	    for (o = ca_laststate.d->opts, a = ca_laststate.oargs; o;
		 o = o->next, a++) {
		if (*a) {
		    *p++ = (o->set ? tricat(o->set, o->name, "") :
			    ztrdup(o->name));
		    *p++ = ca_colonlist(*a);
		}
	    }
	    *p = NULL;

	    sethparam(args[2], ret);
	}
	return 0;
    }
    return 1;
}

/* Help for `_values'. */

typedef struct cvdef *Cvdef;
typedef struct cvval *Cvval;

/* Definitions for _values. */

struct cvdef {
    char *descr;		/* global description */
    int hassep;			/* multiple values allowed */
    char sep;			/* separator character */
    Cvdef next;			/* next in cache */
    Cvval vals;			/* value definitions */
    char **defs;		/* original strings */
    int ndefs;			/* number of ... */
    int lastt;			/* last time used */
};

/* One value definition. */

struct cvval {
    Cvval next;
    char *name;			/* value name */
    char *descr;		/* description */
    char **xor;			/* xor-list */
    int type;			/* CVV_* below */
    Caarg arg;			/* argument definition */
    int active;			/* still allowed */
};

#define CVV_NOARG 0
#define CVV_ARG   1
#define CVV_OPT   2

/* Cache. */

#define MAX_CVCACHE 8
static Cvdef cvdef_cache[MAX_CVCACHE];

/* Memory stuff. */

static void
freecvdef(Cvdef d)
{
    if (d) {
	Cvval p, n;

	zsfree(d->descr);
	if (d->defs)
	    freearray(d->defs);

	for (p = d->vals; p; p = n) {
	    n = p->next;
	    zsfree(p->name);
	    zsfree(p->descr);
	    if (p->xor)
		freearray(p->xor);
	    freecaargs(p->arg);
	    zfree(p, sizeof(*p));
	}
	zfree(d, sizeof(*d));
    }
}

/* Parse option definitions. */

static Cvdef
parse_cvdef(char *nam, char **args)
{
    Cvdef ret;
    Cvval val, *valp;
    Caarg arg;
    char **oargs = args, sep = '\0', *name, *descr, *p, *q, **xor, c;
    int xnum, multi, vtype, hassep = 0;

    if (args[0][0] == '-' && args[0][1] == 's' && !args[0][2]) {
	if (args[1][0] && args[1][1]) {
	    zwarnnam(nam, "invalid separator: %s", args[1], 0);
	    return NULL;
	}
	hassep = 1;
	sep = args[1][0];
	args += 2;
    }
    if (!args[0] || !args[1]) {
	zwarnnam(nam, "not enough arguments", NULL, 0);
	return NULL;
    }
    descr = *args++;

    ret = (Cvdef) zalloc(sizeof(*ret));
    ret->descr = ztrdup(descr);
    ret->hassep = hassep;
    ret->sep = sep;
    ret->next = NULL;
    ret->vals = NULL;
    ret->defs = zarrdup(oargs);
    ret->ndefs = arrlen(oargs);
    ret->lastt = time(0);

    for (valp = &(ret->vals); *args; args++) {
	p = dupstring(*args);
	xnum = 0;

	/* xor list? */
	if (*p == '(') {
	    LinkList list = newlinklist();
	    LinkNode node;
	    char **xp, sav;

	    while (*p && *p != ')') {
		for (p++; inblank(*p); p++);

		if (*p == ')')
		    break;
		for (q = p++; *p && *p != ')' && !inblank(*p); p++);

		if (!*p)
		    break;

		sav = *p;
		*p = '\0';
		addlinknode(list, dupstring(q));
		xnum++;
		*p = sav;
	    }
	    if (*p != ')') {
		freecvdef(ret);
		zwarnnam(nam, "invalid argument: %s", *args, 0);
		return NULL;
	    }
	    xor = (char **) zalloc((xnum + 2) * sizeof(char *));
	    for (node = firstnode(list), xp = xor; node; incnode(node), xp++)
		*xp = ztrdup((char *) getdata(node));
	    xp[0] = xp[1] = NULL;

	    p++;
	} else
	    xor = NULL;

	/* More than once allowed? */
	if ((multi = (*p == '*')))
	    p++;

	/* Skip option name. */

	for (name = p; *p && *p != ':' && *p != '['; p++)
	    if (*p == '\\' && p[1])
		p++;

	if (hassep && !sep && name + 1 != p) {
	    freecvdef(ret);
	    zwarnnam(nam, "no multi-letter values with empty separator allowed", NULL, 0);
	    return NULL;
	}
	/* Optional description? */

	if ((c = *p) == '[') {
	    *p = '\0';
	    for (descr = ++p; *p && *p != ']'; p++)
		if (*p == '\\' && p[1])
		    p++;

	    if (!*p) {
		freecvdef(ret);
		zwarnnam(nam, "invalid value definition: %s", *args, 0);
		return NULL;
	    }
	    *p++ = '\0';
	    c = *p;
	} else {
	    *p = '\0';
	    descr = NULL;
	}
	if (c && c != ':') {
	    freecvdef(ret);
	    zwarnnam(nam, "invalid value definition: %s", *args, 0);
	    return NULL;
	}
	if (!multi) {
	    if (!xor) {
		xor = (char **) zalloc(2 * sizeof(char *));
		xor[1] = NULL;
	    }
	    xor[xnum] = ztrdup(name);
	}
	/* Get argument? */

	if (c == ':') {
	    if (hassep && !sep) {
		freecvdef(ret);
		zwarnnam(nam, "no value with argument with empty separator allowed", NULL, 0);
		return NULL;
	    }
	    if (*++p == ':') {
		p++;
		vtype = CVV_OPT;
	    } else
		vtype = CVV_ARG;
	    arg = parse_caarg(0, 0, 0, 0, name, &p, NULL);
	} else {
	    vtype = CVV_NOARG;
	    arg = NULL;
	}
	*valp = val = (Cvval) zalloc(sizeof(*val));
	valp = &((*valp)->next);

	val->next = NULL;
	val->name = ztrdup(name);
	val->descr = ztrdup(descr);
	val->xor = xor;
	val->type = vtype;
	val->arg = arg;
    }
    return ret;
}

/* Get the definition from the cache or newly built. */

static Cvdef
get_cvdef(char *nam, char **args)
{
    Cvdef *p, *min, new;
    int i, na = arrlen(args);

    for (i = MAX_CVCACHE, p = cvdef_cache, min = NULL; *p && i--; p++)
	if (*p && na == (*p)->ndefs && arrcmp(args, (*p)->defs)) {
	    (*p)->lastt = time(0);

	    return *p;
	} else if (!min || !*p || (*p)->lastt < (*min)->lastt)
	    min = p;
    if (i)
	min = p;
    if ((new = parse_cvdef(nam, args))) {
	freecvdef(*min);
	*min = new;
    }
    return new;
}

/* Get the definition for a value. */

static Cvval
cv_get_val(Cvdef d, char *name)
{
    Cvval p;

    for (p = d->vals; p; p = p->next)
	if (!strcmp(name, p->name))
	    return p;

    return NULL;
}

/* Handle a xor list. */

static void
cv_inactive(Cvdef d, char **xor)
{
    if (xor) {
	Cvval val;

	for (; *xor; xor++)
	    if ((val = cv_get_val(d, *xor)))
		val->active = 0;
    }
}

/* Parse state. */

struct cvstate {
    Cvdef d;
    Caarg def;
    Cvval val;
    LinkList vals;
};

static struct cvstate cv_laststate;
static int cv_parsed = 0, cv_alloced = 0;

/* Parse the current word. */

static void
cv_parse_word(Cvdef d)
{
    Cvval ptr;
    struct cvstate state;
    char *str, *eq;

    if (cv_alloced)
	freelinklist(cv_laststate.vals, freestr);

    for (ptr = d->vals; ptr; ptr = ptr->next)
	ptr->active = 1;

    state.d = d;
    state.def = NULL;
    state.val = NULL;
    state.vals = (LinkList) znewlinklist();

    cv_alloced = 1;

    if (d->hassep) {
	if (d->sep) {
	    char *end;
	    int heq;

	    for (str = compprefix, end = strchr(str, d->sep); end;) {
		*end = '\0';

		if ((heq = !!(eq = strchr(str, '='))))
		    *eq++ = '\0';
		else
		    eq = "";

		if ((ptr = cv_get_val(d, str))) {
		    zaddlinknode(state.vals, ztrdup(str));
		    zaddlinknode(state.vals, ztrdup(eq));

		    cv_inactive(d, ptr->xor);
		}
		if (heq)
		    eq[-1] = '=';

		*end = d->sep;
		str = end + 1;
		end = strchr(str, d->sep);
	    }
	    ignore_prefix(str - compprefix);

	    if ((str = strchr(compsuffix, d->sep))) {
		char *beg = str;

		for (str++; str; str = end) {
		    if ((end = strchr(str, d->sep)))
			*end = '\0';

		    if ((heq = !!(eq = strchr(str, '='))))
			*eq++ = '\0';
		    else
			eq = "";

		    if ((ptr = cv_get_val(d, str))) {
			zaddlinknode(state.vals, ztrdup(str));
			zaddlinknode(state.vals, ztrdup(eq));

			cv_inactive(d, ptr->xor);
		    }
		    if (heq)
			eq[-1] = '=';
		    if (end)
			*end++ = d->sep;
		}
		ignore_suffix(strlen(beg));
	    }
	} else {
	    char tmp[2];

	    tmp[1] = '\0';

	    for (str = compprefix; *str; str++) {
		tmp[0] = *str;
		if ((ptr = cv_get_val(d, tmp))) {
		    zaddlinknode(state.vals, ztrdup(tmp));
		    zaddlinknode(state.vals, ztrdup(""));

		    cv_inactive(d, ptr->xor);
		}
	    }
	    for (str = compsuffix; *str; str++) {
		tmp[0] = *str;
		if ((ptr = cv_get_val(d, tmp))) {
		    zaddlinknode(state.vals, ztrdup(tmp));
		    zaddlinknode(state.vals, ztrdup(""));

		    cv_inactive(d, ptr->xor);
		}
	    }
	    ignore_prefix(strlen(compprefix));
	    ignore_suffix(strlen(compsuffix));
	}
    }
    str = tricat(compprefix, compsuffix, "");
    zsfree(compprefix);
    zsfree(compsuffix);
    compprefix = str;
    compsuffix = ztrdup("");

    if ((eq = strchr(str, '='))) {
	*eq++ = '\0';

	if ((ptr = cv_get_val(d, str)) && ptr->type != CVV_NOARG) {
	    eq[-1] = '=';
	    ignore_prefix(eq - str);
	    state.def = ptr->arg;
	    state.val = ptr;
	} else
	    eq[-1] = '=';
    }
    memcpy(&cv_laststate, &state, sizeof(state));
}

static int
bin_compvalues(char *nam, char **args, char *ops, int func)
{
    int min, max, n;

    if (incompfunc != 1) {
	zwarnnam(nam, "can only be called from completion function", NULL, 0);
	return 1;
    }
    if (args[0][0] != '-' || !args[0][1] || args[0][2]) {
	zwarnnam(nam, "invalid argument: %s", args[0], 0);
	return 1;
    }
    if (args[0][1] != 'i' && !cv_parsed) {
	zwarnnam(nam, "no parsed state", NULL, 0);
	return 1;
    }
    switch (args[0][1]) {
    case 'i': min = 2; max = -1; break;
    case 'D': min = 2; max =  2; break;
    case 'C': min = 1; max =  1; break;
    case 'V': min = 3; max =  3; break;
    case 's': min = 1; max =  1; break;
    case 'd': min = 1; max =  1; break;
    case 'L': min = 3; max =  4; break;
    case 'v': min = 1; max =  1; break;
    default:
	zwarnnam(nam, "invalid option: %s", args[0], 0);
	return 1;
    }
    n = arrlen(args) - 1;
    if (n < min) {
	zwarnnam(nam, "not enough arguments", NULL, 0);
	return 1;
    } else if (max >= 0 && n > max) {
	zwarnnam(nam, "too many arguments", NULL, 0);
	return 1;
    }
    switch (args[0][1]) {
    case 'i':
	{
	    Cvdef def = get_cvdef(nam, args + 1);
	    int cvp = cv_parsed;

	    cv_parsed = 0;

	    if (!def)
		return 1;

	    cv_parsed = cvp;
	    cv_parse_word(def);
	    cv_parsed = 1;

	    return 0;
	}
	return 1;

    case 'D':
	{
	    Caarg arg = cv_laststate.def;

	    if (arg) {
		setsparam(args[1], ztrdup(arg->descr));
		setsparam(args[2], ztrdup(arg->action));

		return 0;
	    }
	    return 1;
	}
    case 'C':
	{
	    Caarg arg = cv_laststate.def;

	    if (arg) {
		setsparam(args[1], ztrdup(arg->opt));

		return 0;
	    }
	    return 1;
	}
    case 'V':
	{
	    LinkList noarg = newlinklist();
	    LinkList arg = newlinklist();
	    LinkList opt = newlinklist(), l;
	    Cvval p;
	    char *str;

	    for (p = cv_laststate.d->vals; p; p = p->next) {
		if (p->active) {
		    switch (p->type) {
		    case CVV_NOARG: l = noarg; break;
		    case CVV_ARG:   l = arg;   break;
		    default:        l = opt;   break;
		    }
		    if (p->descr) {
			int len = strlen(p->name) + strlen(p->descr) + 2;

			str = (char *) zhalloc(len);
			strcpy(str, p->name);
			strcat(str, ":");
			strcat(str, p->descr);
		    } else
			str = p->name;
		    addlinknode(l, str);
		}
	    }
	    set_list_array(args[1], noarg);
	    set_list_array(args[2], arg);
	    set_list_array(args[3], opt);

	    return 0;
	}
    case 's':
	if (cv_laststate.d->hassep) {
	    char tmp[2];

	    tmp[0] = cv_laststate.d->sep;
	    tmp[1] = '\0';
	    setsparam(args[1], ztrdup(tmp));

	    return 0;
	}
	return 1;
    case 'd':
	setsparam(args[1], ztrdup(cv_laststate.d->descr));
	return 0;
    case 'L':
	{
	    Cvval val = cv_get_val(cv_laststate.d, args[1]);

	    if (val && val->arg) {
		setsparam(args[2], val->arg->descr);
		setsparam(args[3], val->arg->action);

		if (args[4])
		    setsparam(args[4], ztrdup(val->name));

		return 0;
	    }
	    return 1;
	}
    case 'v':
	if (cv_laststate.vals) {
	    char **ret, **p;
	    LinkNode n;

	    ret = (char **) zalloc((countlinknodes(cv_laststate.vals) + 1) *
				   sizeof(char *));

	    for (n = firstnode(cv_laststate.vals), p = ret; n; incnode(n), p++)
		*p = ztrdup((char *) getdata(n));
	    *p = NULL;

	    sethparam(args[1], ret);

	    return 0;
	}
	return 1;
    }
    return 1;
}

static int
bin_compquote(char *nam, char **args, char *ops, int func)
{
    char *name;
    struct value vbuf;
    Value v;

    /* Anything to do? */

    if (!compqstack || !*compqstack)
	return 0;

    /* For all parameters given... */

    while ((name = *args++)) {
	name = dupstring(name);
	if ((v = getvalue(&vbuf, &name, 0))) {
	    switch (PM_TYPE(v->pm->flags)) {
	    case PM_SCALAR:
		{
		    char *val = getstrvalue(v);

		    val = bslashquote(val, NULL,
				      (*compqstack == '\'' ? 1 :
				       (*compqstack == '"' ? 2 : 0)));

		    setstrvalue(v, ztrdup(val));
		}
		break;
	    case PM_ARRAY:
		{
		    char **val = v->pm->gets.afn(v->pm);
		    char **new = (char **) zalloc((arrlen(val) + 1) *
						  sizeof(char *));
		    char **p = new;

		    for (; *val; val++, p++)
			*p = ztrdup(bslashquote(*val, NULL,
						(*compqstack == '\'' ? 1 :
						 (*compqstack == '"' ? 2 :
						  0))));
		    *p = NULL;

		    setarrvalue(v, new);
		}
		break;
	    default:
		zwarnnam(nam, "invalid parameter type: %s", args[-1], 0);
	    }
	} else
	    zwarnnam(nam, "unknown parameter: %s", args[-1], 0);
    }
    return 0;
}

/* Tags stuff. */

typedef struct ctags *Ctags;
typedef struct ctset *Ctset;

/* A bunch of tag sets. */

struct ctags {
    char **all;			/* all tags offered */
    char *context;		/* the current context */
    int init;			/* not yet used */
    Ctset sets;			/* the tag sets */
};

/* A tag set. */

struct ctset {
    Ctset next;
    char **tags;		/* the tags */
    char *tag;			/* last tag checked for -A */
    char **ptr;			/* ptr into tags for -A */
};

/* Array of tag-set infos. Index is the locallevel. */

#define MAX_TAGS 256
static Ctags comptags[MAX_TAGS];

/* locallevel at last comptags -i */

static int lasttaglevel;

static void
freectset(Ctset s)
{
    Ctset n;

    while (s) {
	n = s->next;

	if (s->tags)
	    freearray(s->tags);
	zsfree(s->tag);
	zfree(s, sizeof(*s));

	s = n;
    }
}

static void
freectags(Ctags t)
{
    if (t) {
	if (t->all)
	    freearray(t->all);
	zsfree(t->context);
	freectset(t->sets);
	zfree(t, sizeof(*t));
    }
}

/* Set the tags for the current local level. */

static void
settags(int level, char **tags)
{
    Ctags t;

    if (comptags[level])
	freectags(comptags[level]);

    comptags[level] = t = (Ctags) zalloc(sizeof(*t));

    t->all = zarrdup(tags + 1);
    t->context = ztrdup(*tags);
    t->sets = NULL;
    t->init = 1;
}

/* Check if an array contains a string. */

/**/
static int
arrcontains(char **a, char *s, int colon)
{
    char *p, *q;

    while (*a) {
	if (colon) {
	    for (p = s, q = *a++; *p && *q && *p != ':' && *q != ':'; p++, q++)
		if (*p != *q)
		    break;
	    if ((!*p || *p == ':') && (!*q || *q == ':'))
		return 1;
	} else if (!strcmp(*a++, s))
	    return 1;
    }
    return 0;
}

static int
bin_comptags(char *nam, char **args, char *ops, int func)
{
    int min, max, n, level;

    if (incompfunc != 1) {
	zwarnnam(nam, "can only be called from completion function", NULL, 0);
	return 1;
    }
    if (args[0][0] != '-' || !args[0][1] ||
	(args[0][2] && (args[0][2] != '-' || args[0][3]))) {
	zwarnnam(nam, "invalid argument: %s", args[0], 0);
	return 1;
    }
    level = locallevel - (args[0][2] ? 1 : 0);
    if (level >= MAX_TAGS) {
	zwarnnam(nam, "nesting level too deep", NULL, 0);
	return 1;
    }
    if (args[0][1] != 'i' && args[0][1] != 'I' && !comptags[level]) {
	zwarnnam(nam, "no tags registered", NULL, 0);
	return 1;
    }
    switch (args[0][1]) {
    case 'i': min = 2; max = -1; break;
    case 'C': min = 1; max =  1; break;
    case 'T': min = 0; max =  0; break;
    case 'N': min = 0; max =  0; break;
    case 'R': min = 1; max =  1; break;
    case 'S': min = 1; max =  1; break;
    case 'A': min = 2; max =  3; break;
    default:
	zwarnnam(nam, "invalid option: %s", args[0], 0);
	return 1;
    }
    n = arrlen(args) - 1;
    if (n < min) {
	zwarnnam(nam, "not enough arguments", NULL, 0);
	return 1;
    } else if (max >= 0 && n > max) {
	zwarnnam(nam, "too many arguments", NULL, 0);
	return 1;
    }
    switch (args[0][1]) {
    case 'i':
	settags(level, args + 1);
	lasttaglevel = level;
	break;
    case 'C':
	setsparam(args[1], ztrdup(comptags[level]->context));
	break;
    case 'T':
	return !comptags[level]->sets;
    case 'N':
	{
	    Ctset s;

	    if (comptags[level]->init)
		comptags[level]->init = 0;
	    else if ((s = comptags[level]->sets)) {
		comptags[level]->sets = s->next;
		s->next = NULL;
		freectset(s);
	    }
	    return !comptags[level]->sets;
	}
    case 'R':
	{
	    Ctset s;

	    return !((s = comptags[level]->sets) &&
		     arrcontains(s->tags, args[1], 1));
	}
    case 'A':
	{
	    Ctset s;

	    if (comptags[level] && (s = comptags[level]->sets)) {
		char **q, *v = NULL;
		int l = strlen(args[1]);

		if (!s->tag || strcmp(s->tag, args[1])) {
		    zsfree(s->tag);
		    s->tag = ztrdup(args[1]);
		    s->ptr = s->tags;
		}
		for (q = s->ptr; *q; q++) {
		    if (strpfx(args[1], *q)) {
			if (!(*q)[l]) {
			    v = *q;
			    break;
			} else if ((*q)[l] == ':') {
			    v = (*q) + l + 1;
			    break;
			}
		    }
		}
		if (!v) {
		    zsfree(s->tag);
		    s->tag = NULL;
		    return 1;
		}
		s->ptr = q + 1;
		setsparam(args[2], ztrdup(*v == '-' ? dyncat(args[1], v) : v));
		if (args[3]) {
		    char *r = dupstring(*q), *p;

		    for (p = r + (v - *q); *p && *p != ':'; p++);
		    *p = '\0';

		    setsparam(args[3], ztrdup(r));
		}
		return 0;
	    }
	    return 1;
	}
    case 'S':
	if (comptags[level]->sets) {
	    char **ret;

	    ret = zarrdup(comptags[level]->sets->tags);
	    setaparam(args[1], ret);
	} else
	    return 1;

	break;
    }
    return 0;
}

static int
bin_comptry(char *nam, char **args, char *ops, int func)
{
    if (incompfunc != 1) {
	zwarnnam(nam, "can only be called from completion function", NULL, 0);
	return 1;
    }
    if (!lasttaglevel || !comptags[lasttaglevel]) {
	zwarnnam(nam, "no tags registered", NULL, 0);
	return 1;
    }
    if (*args) {
	if (!strcmp(*args, "-m")) {
	    char *s, *p, *q, *c, **all = comptags[lasttaglevel]->all;
	    LinkList list = newlinklist();
	    LinkNode node;
	    int num = 0;
	    Ctset set;

	    while ((s = *++args)) {
		while (*s) {
		    while (*s && iblank(*s))
			s++;
		    for (p = q = s, c = NULL; *s && !inblank(*s); s++) {
			if (!c && *s == ':')
			    c = p;
			if (*s == '\\' && s[1])
			    s++;
			*p++ = *s;
		    }
		    if (*s)
			s++;
		    *p = '\0';
		    if (*q) {
			char *qq, *qqq;

			if (c)
			    *c = '\0';

			qqq = qq = dupstring(q);
			while (*qqq) {
			    if (qqq == qq || qqq[-1] != '\\') {
				if (*qqq == '{')
				    *qqq = Inbrace;
				else if (*qqq == '}')
				    *qqq = Outbrace;
				else if (*qqq == ',')
				    *qqq = Comma;
			    }
			    qqq++;
			}
			tokenize(qq);
			if (haswilds(qq) || hasbraces(qq)) {
			    Patprog prog;
			    LinkNode bnode, node;
			    LinkList blist = newlinklist();

			    addlinknode(blist, qq);
			    for (bnode = firstnode(blist); bnode; incnode(bnode))
				while (hasbraces(getdata(bnode)))
				    xpandbraces(blist, &bnode);

			    for (bnode = firstnode(blist); bnode; incnode(bnode)) {
				qq = (char *) getdata(bnode);
				if ((prog = patcompile(qq, PAT_STATIC, NULL))) {
				    char **a, *n;
				    int l = (c ? strlen(c + 1) + 2 : 1), al;

				    for (a = all; *a; a++) {
					for (node = firstnode(list); node;
					     incnode(node)) {
					    char *as, *ls;

					    for (as = *a, ls = (char *) getdata(node);
						 *as && *ls && *ls != ':'; as++, ls++)
						if (*as != *ls)
						    break;
					    if (!*as && (!*ls || *ls == ':'))
						break;
					}
					if (node)
					    continue;
					if (pattry(prog, *a)) {
					    n = (char *) zhalloc((al = strlen(*a)) + l);
					    strcpy(n, *a);
					    if (c) {
						n[al] = ':';
						strcpy(n + al + 1, c + 1);
					    }
					    addlinknode(list, n);
					    num++;
					}
				    }
				}
			    }
			} else if (arrcontains(all, q, 0)) {
			    for (set = comptags[lasttaglevel]->sets; set;
				 set = set->next)
				if (arrcontains(set->tags, q, 0))
				    break;
			    if (!set) {
				addlinknode(list, q);
				num++;
			    }
			}
			if (c)
			    *c = ':';
		    }
		}
		if (num) {
		    char **a;
		    Ctset l;

		    set = (Ctset) zalloc(sizeof(*set));

		    a = set->tags = (char **) zalloc((num + 1) * sizeof(char *));
		    for (node = firstnode(list); node; incnode(node))
			*a++ = ztrdup((char *) getdata(node));

		    *a = NULL;
		    set->next = NULL;
		    set->ptr = NULL;
		    set->tag = NULL;

		    if ((l = comptags[lasttaglevel]->sets)) {
			while (l->next)
			    l = l->next;

			l->next = set;
		    } else
			comptags[lasttaglevel]->sets = set;
		}
	    }
	} else {
	    char **p, **q, **all;
	    int sep = 0;

	    if ((sep = !strcmp(*args, "-s")))
		args++;

	    for (p = q = args, all = comptags[lasttaglevel]->all; *p; p++)
		if (arrcontains(all, *p, 1)) {
		    Ctset s;

		    for (s = comptags[lasttaglevel]->sets; s; s = s->next)
			if (arrcontains(s->tags, *p, 0))
			    break;

		    if (!s)
			*q++ = *p;
		}
	    *q = NULL;

	    if (*args) {
		char *dummy[2];

		do {
		    Ctset s = (Ctset) zalloc(sizeof(*s)), l;

		    if (sep) {
			dummy[0] = *args++;
			dummy[1] = NULL;
			s->tags = zarrdup(dummy);
		    } else
			s->tags = zarrdup(args);
		    s->next = NULL;
		    s->ptr = NULL;
		    s->tag = NULL;

		    if ((l = comptags[lasttaglevel]->sets)) {
			while (l->next)
			    l = l->next;

			l->next = s;
		    } else
			comptags[lasttaglevel]->sets = s;
		} while (sep && *args);
	    }
	}
    }
    return 0;
}

static char *
fmtstr(char *str, char c, char *repl)
{
    int len, num, rlen;
    char *s, *ret, *rp;

    len = strlen(str);
    rlen = strlen(repl);

    for (num = 0, s = str; *s; s++)
	if (*s == '%' && s[1] == c)
	    num++, s++;

    ret = (char *) zhalloc((num * (rlen - 2)) + len + 1);

    for (s = str, rp = ret; *s; s++) {
	if (*s == '%' && s[1] == c) {
	    strcpy(rp, repl);
	    rp += rlen;
	    s++;
	} else
	    *rp++ = *s;
    }
    *rp = '\0';

    return ret;
}

static int
bin_compfmt(char *nam, char **args, char *ops, int func)
{
    char *param = args[0], *str = args[1];

    for (args += 2; *args; args++) {
	if (args[0][1] != ':') {
	    zwarnnam(nam, "invalid argument `%s'", args[0], 0);
	    return 1;
	}
	str = fmtstr(str, **args, *args + 2);
    }
    setsparam(param, ztrdup(str));
    return 0;
}

static struct builtin bintab[] = {
    BUILTIN("compdescribe", 0, bin_compdescribe, 3, -1, 0, NULL, NULL),
    BUILTIN("comparguments", 0, bin_comparguments, 1, -1, 0, NULL, NULL),
    BUILTIN("compvalues", 0, bin_compvalues, 1, -1, 0, NULL, NULL),
    BUILTIN("compquote", 0, bin_compquote, 1, -1, 0, NULL, NULL),
    BUILTIN("comptags", 0, bin_comptags, 1, -1, 0, NULL, NULL),
    BUILTIN("comptry", 0, bin_comptry, 0, -1, 0, NULL, NULL),
    BUILTIN("compfmt", 0, bin_compfmt, 2, -1, 0, NULL, NULL),
};


/**/
int
setup_(Module m)
{
    memset(cadef_cache, 0, sizeof(cadef_cache));
    memset(cvdef_cache, 0, sizeof(cvdef_cache));

    memset(comptags, 0, sizeof(comptags));

    lasttaglevel = 0;

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
    int i;

    for (i = 0; i < MAX_CACACHE; i++)
	freecadef(cadef_cache[i]);
    for (i = 0; i < MAX_CVCACHE; i++)
	freecvdef(cvdef_cache[i]);

    for (i = 0; i < MAX_TAGS; i++)
	freectags(comptags[i]);

    return 0;
}
