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
	    zerrnam(nam, "invalid argument: %s", *args, 0);
	    return 1;
	}
	PERMALLOC {
	    set->strs = arrdup(ap);
	} LASTALLOC;

	if (disp)
	    cdisp_calc(&(cd_state.disp), set->strs);

	if (*++args && **args != '-') {
	    if (!(ap = get_user_var(*args))) {
		zerrnam(nam, "invalid argument: %s", *args, 0);
		return 1;
	    }
	    PERMALLOC {
		set->matches = arrdup(ap);
	    } LASTALLOC;
	    args++;
	}
	for (ap = args; *args &&
		 (args[0][0] != '-' || args[0][1] != '-' || args[0][2]);
	     args++);

	tmp = *args;
	*args = NULL;
	PERMALLOC {
	    set->opts = arrdup(ap);
	} LASTALLOC;
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

	PERMALLOC {
	    p = arrdup(set->opts);
	} LASTALLOC;

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
	zerrnam(nam, "can only be called from completion function", NULL, 0);
	return 1;
    }
    if (!args[0][0] || !args[0][1] || args[0][2]) {
	zerrnam(nam, "invalid argument: %s", args[0], 0);
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
		zerrnam(nam, (n < 6 ? "not enough arguments" :
			      "too many arguments"), NULL, 0);
		return 1;
	    }
	    return cd_get(args + 1);
	} else {
	    zerrnam(nam, "no parsed state", NULL, 0);
	    return 1;
	}
    }
    zerrnam(nam, "invalid option: %s", args[0], 0);
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
};

#define CAO_NEXT    1
#define CAO_DIRECT  2
#define CAO_ODIRECT 3
#define CAO_EQUAL   4

/* Description for an argument */

struct caarg {
    Caarg next;
    char *descr;		/* description */
    char *action;		/* what to do for it */
    int type;			/* CAA_* below */
    char *end;			/* end-pattern for ::<pat>:... */
    char *opt;			/* option name if for an option */
    int num;			/* it's the num'th argument */
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
parse_caarg(int mult, int type, int num, char *oname, char **def)
{
    Caarg ret = (Caarg) zalloc(sizeof(*ret));
    char *p = *def, *d, sav;

    ret->next = NULL;
    ret->descr = ret->action = ret->end = NULL;
    ret->num = num;
    ret->type = type;
    ret->opt = ztrdup(oname);

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
parse_cadef(char *nam, char **args)
{
    Cadef ret;
    Caopt *optp;
    char **oargs = args, *p, *q, *match = "r:|[_-]=* r:|=*", **xor;
    char *adpre, *adsuf;
    int single = 0, anum = 1, xnum, nopts, ndopts, nodopts;

    nopts = ndopts = nodopts = 0;

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

    PERMALLOC {
	ret = (Cadef) zalloc(sizeof(*ret));
	ret->next = NULL;
	ret->opts = NULL;
	ret->args = ret->rest = NULL;
	ret->defs = arrdup(oargs);
	ret->ndefs = arrlen(oargs);
	ret->lastt = time(0);
	if (single) {
	    ret->single = (Caopt *) zalloc(256 * sizeof(Caopt));
	    memset(ret->single, 0, 256 * sizeof(Caopt));
	} else
	    ret->single = NULL;
	ret->match = ztrdup(match);
    } LASTALLOC;

    /* Get the definitions. */

    for (optp = &(ret->opts); *args; args++) {
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
		zerrnam(nam, "invalid argument: %s", *args, 0);
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
		again = 1 - again;
	    } else {
		name = p;
		/* If it's a long option skip over the first `-'. */
		if (p[0] == '-' && p[1] == '-')
		    p++;
	    }
	    if (!p[1]) {
		freecadef(ret);
		zerrnam(nam, "invalid argument: %s", *args, 0);
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
		    zerrnam(nam, "invalid option definition: %s", *args, 0);
		    return NULL;
		}
		*p++ = '\0';
		c = *p;
	    } else
		descr = NULL;

	    if (c && c != ':') {
		freecadef(ret);
		zerrnam(nam, "invalid option definition: %s", *args, 0);
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
		int atype, rest, oanum = 1;
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
			    *p = sav;
			}
			if (*p != ':') {
			    freecadef(ret);
			    freecaargs(oargs);
			    zerrnam(nam, "invalid option definition: %s",
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

		    *oargp = parse_caarg(!rest, atype, oanum++, name, &p);
		    if (end)
			(*oargp)->end = ztrdup(end);
		    oargp = &((*oargp)->next);
		    if (rest)
			break;
		    c = *p;
		}
	    }
	    /* Store the option definition. */

	    PERMALLOC {
		*optp = opt = (Caopt) zalloc(sizeof(*opt));
		optp = &((*optp)->next);

		opt->next = NULL;
		opt->name = ztrdup(rembslashcolon(name));
		if (descr)
		    opt->descr = ztrdup(descr);
		else if (adpre && oargs && !oargs->next &&
			 oargs->descr && oargs->descr[0])
		    opt->descr = tricat(adpre, oargs->descr, adsuf);
		else
		    opt->descr = NULL;
		opt->xor = xor;
		opt->type = otype;
		opt->args = oargs;
		opt->num = nopts++;
	    } LASTALLOC;

	    if (otype == CAO_DIRECT)
		ndopts++;
	    else if (otype == CAO_ODIRECT || otype == CAO_EQUAL)
		nodopts++;

	    /* If this is for single-letter option we also store a
	     * pointer for the definition in the array for fast lookup. */

	    if (single && name[1] && !name[2])
		ret->single[STOUC(name[1])] = opt;

	    if (again) {
		/* Do it all again for `*-...'. */
		p = dupstring(*args);
		goto rec;
	    }
	} else if (*p == '*') {
	    /* It's a rest-argument definition. */

	    int type = CAA_REST;

	    if (*++p != ':') {
		freecadef(ret);
		zerrnam(nam, "invalid rest argument definition: %s", *args, 0);
		return NULL;
	    }
	    if (ret->rest) {
		freecadef(ret);
		zerrnam(nam, "doubled rest argument definition: %s", *args, 0);
		return NULL;
	    }
	    if (*++p == ':') {
		if (*++p == ':') {
		    type = CAA_RREST;
		    p++;
		} else
		    type = CAA_RARGS;
	    }
	    ret->rest = parse_caarg(0, type, -1, NULL, &p);
	} else {
	    /* It's a normal argument definition. */

	    int type = CAA_NORMAL;
	    Caarg arg, tmp, pre;

	    if (idigit(*p)) {
		/* Argment number is given. */
		int num = 0;

		while (*p && idigit(*p))
		    num = (num * 10) + ((int) *p++);

		anum = num + 1;
	    } else
		/* Default number. */
		anum++;

	    if (*p != ':') {
		freecadef(ret);
		zerrnam(nam, "invalid argument: %s", *args, 0);
		return NULL;
	    }
	    if (*++p == ':') {
		/* Optional argument. */
		type = CAA_OPT;
		p++;
	    }
	    arg = parse_caarg(0, type, anum - 1, NULL, &p);

	    /* Sort the new definition into the existing list. */

	    for (tmp = ret->args, pre = NULL;
		 tmp && tmp->num < anum - 1;
		 pre = tmp, tmp = tmp->next);

	    if (tmp && tmp->num == anum - 1) {
		freecadef(ret);
		freecaargs(arg);
		zerrnam(nam, "doubled argument definition: %s", *args, 0);
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
    
    return ret;
}

/* Given an array of definitions, return the cadef for it. From the cache
 * are newly built. */

static Cadef
get_cadef(char *nam, char **args)
{
    Cadef *p, *min, new;
    int i, na = arrlen(args);

    for (i = MAX_CACACHE, p = cadef_cache, min = NULL; *p && i--; p++)
	if (*p && na == (*p)->ndefs && arrcmp(args, (*p)->defs)) {
	    (*p)->lastt = time(0);

	    return *p;
	} else if (!min || !*p || (*p)->lastt < (*min)->lastt)
	    min = p;
    if (i)
	min = p;
    if ((new = parse_cadef(nam, args))) {
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

	while (a && a->num < n)
	    a = a->next;

	if (a && a->num == n)
	    return a;

	return d->rest;
    }
    return NULL;
}

/* Use a xor list, marking options as inactive. */

static void
ca_inactive(Cadef d, char **xor)
{
    if (xor) {
	Caopt opt;

	for (; *xor; xor++) {
	    if (xor[0][0] == ':' && !xor[0][1])
		d->argsactive = 0;
	    else if ((opt = ca_get_opt(d, *xor, 1, NULL)))
		opt->active = 0;
	}
    }
}

/* State when parsing a command line. */

struct castate {
    Cadef d;
    Caarg def, ddef;
    Caopt curopt;
    int opt, arg, argbeg, optbeg, nargbeg, restbeg;
    int inopt, inrest, inarg, nth, doff, singles;
    LinkList args;
    LinkList *oargs;
};

static struct castate ca_laststate;
static int ca_parsed = 0, ca_alloced = 0;

/* Pars a command line. */

static void
ca_parse_line(Cadef d)
{
    Caarg adef, ddef;
    Caopt ptr;
    struct castate state;
    char *line, *pe;
    int cur, doff;
    Patprog endpat = NULL;

    /* Free old state. */

    if (ca_alloced) {
	int i = ca_laststate.d->nopts;
	LinkList *p = ca_laststate.oargs;

	freelinklist(ca_laststate.args, freestr);
	while (i--)
	    if (*p++)
		freelinklist(p[-1], freestr);

	zfree(ca_laststate.oargs, ca_laststate.d->nopts * sizeof(LinkList));
    }
    /* MArk everything as active. */

    for (ptr = d->opts; ptr; ptr = ptr->next)
	ptr->active = 1;
    d->argsactive = 1;

    /* Default values for the state. */

    state.d = d;
    state.def = state.ddef = NULL;
    state.curopt = NULL;
    state.argbeg = state.optbeg = state.nargbeg = state.restbeg =
	state.nth = state.inopt = state.inarg = state.opt = state.arg = 1;
    state.inrest = state.doff = state.singles = state.doff = 0;
    PERMALLOC {
	state.args = newlinklist();
	state.oargs = (LinkList *) zalloc(d->nopts * sizeof(LinkList));
	memset(state.oargs, 0, d->nopts * sizeof(LinkList));
    } LASTALLOC;
    ca_alloced = 1;

    memcpy(&ca_laststate, &state, sizeof(state));

    if (!compwords[1]) {
	ca_laststate.opt = ca_laststate.arg = 0;

	return;
    }
    /* Loop over the words from the line. */

    for (line = compwords[1], cur = 2, state.curopt = NULL, state.def = NULL;
	 line; line = compwords[cur++]) {
	ddef = adef = NULL;
	doff = state.singles = 0;

	/* We've a definition for an argument, skip to the next. */

	if (state.def) {
	    state.arg = 0;
	    if (state.curopt) {
		PERMALLOC {
		    addlinknode(state.oargs[state.curopt->num], ztrdup(line));
		} LASTALLOC;
	    }
	    state.opt = (state.def->type == CAA_OPT);

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

	/* See if it's an option. */

	if (state.opt == 2 && (state.curopt = ca_get_opt(d, line, 0, &pe))) {
	    ddef = state.def = state.curopt->args;
	    doff = pe - line;
	    state.optbeg = state.argbeg = state.inopt = cur;
	    state.singles = (d->single && (!pe || !*pe) &&
			     state.curopt->name[1] && !state.curopt->name[2]);

	    PERMALLOC {
		state.oargs[state.curopt->num] = newlinklist();
	    } LASTALLOC;
	    ca_inactive(d, state.curopt->xor);

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
		PERMALLOC {
		    addlinknode(state.oargs[state.curopt->num], ztrdup(pe));
		} LASTALLOC;
	    }
	    if (state.def)
		state.opt = 0;
	    else
		state.curopt = NULL;
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
		    PERMALLOC {
			state.oargs[tmpopt->num] = newlinklist();
		    } LASTALLOC;
		    ca_inactive(d, tmpopt->xor);
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
		PERMALLOC {
		    addlinknode(state.oargs[state.curopt->num], ztrdup(pe));
		} LASTALLOC;
	    }
	    if (state.def)
		state.opt = 0;
	    else
		state.curopt = NULL;
	} else if (state.arg) {
	    /* Otherwise it's a normal argument. */
	    if (state.inopt) {
		state.inopt = 0;
		state.nargbeg = cur - 1;
	    }
	    if ((adef = state.def = ca_get_arg(d, state.nth)) &&
		(state.def->type == CAA_RREST ||
		 state.def->type == CAA_RARGS)) {
		state.inrest = 0;
		state.opt = (cur == state.nargbeg + 1);
		state.optbeg = state.nargbeg;
		state.argbeg = cur - 1;

		for (; line; line = compwords[cur++]) {
		    PERMALLOC {
			addlinknode(state.args, ztrdup(line));
		    } LASTALLOC;
		}
		memcpy(&ca_laststate, &state, sizeof(state));
		ca_laststate.ddef = NULL;
		ca_laststate.doff = 0;
		break;
	    }
	    PERMALLOC {
		addlinknode(state.args, ztrdup(line));
	    } LASTALLOC;
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

		PERMALLOC {
		    for (; line; line = compwords[cur++])
			addlinknode(l, ztrdup(line));
		} LASTALLOC;
		if (cur < compcurrent)
		    memcpy(&ca_laststate, &state, sizeof(state));
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
	    }
	}
    }
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

static int
bin_comparguments(char *nam, char **args, char *ops, int func)
{
    int min, max, n;

    if (incompfunc != 1) {
	zerrnam(nam, "can only be called from completion function", NULL, 0);
	return 1;
    }
    if (args[0][0] != '-' || !args[0][1] || args[0][2]) {
	zerrnam(nam, "invalid argument: %s", args[0], 0);
	return 1;
    }
    if (args[0][1] != 'i' && !ca_parsed) {
	zerrnam(nam, "no parsed state", NULL, 0);
	return 1;
    }
    switch (args[0][1]) {
    case 'i': min = 2; max = -1; break;
    case 'D': min = 2; max =  2; break;
    case 'C': min = 1; max =  1; break;
    case 'O': min = 4; max =  4; break;
    case 'L': min = 3; max =  4; break;
    case 's': min = 1; max =  1; break;
    case 'M': min = 1; max =  1; break;
    case 'a': min = 0; max =  0; break;
    case 'W': min = 2; max =  2; break;
    default:
	zerrnam(nam, "invalid option: %s", args[0], 0);
	return 1;
    }
    n = arrlen(args) - 1;
    if (n < min) {
	zerrnam(nam, "not enough arguments", NULL, 0);
	return 1;
    } else if (max >= 0 && n > max) {
	zerrnam(nam, "too many arguments", NULL, 0);
	return 1;
    }
    switch (args[0][1]) {
    case 'i':
	if (compcurrent > 1 && compwords[0]) {
	    Cadef def = get_cadef(nam, args + 1);
	    int cap = ca_parsed;

	    ca_parsed = 0;

	    if (!def)
		return 1;

	    ca_parsed = cap;
	    ca_parse_line(def);
	    ca_parsed = 1;

	    return 0;
	}
	return 1;

    case 'D':
	{
	    Caarg arg = ca_laststate.def;

	    if (arg) {
		setsparam(args[1], ztrdup(arg->descr));
		setsparam(args[2], ztrdup(arg->action));

		if (ca_laststate.doff > 0)
		    ignore_prefix(ca_laststate.doff);
		if (arg->type == CAA_RARGS)
		    restrict_range(ca_laststate.optbeg,
				   arrlen(compwords) - 1);
		else if (arg->type == CAA_RREST)
		    restrict_range(ca_laststate.argbeg,
				   arrlen(compwords) - 1);
		return 0;
	    }
	    return 1;
	}
    case 'C':
	{
	    Caarg arg = ca_laststate.def;

	    if (arg) {
		char buf[20];

		if (arg->num > 0)
		    sprintf(buf, "%d", arg->num);
		else
		    strcpy(buf, "rest");

		setsparam(args[1], (arg->opt ? tricat(arg->opt, "-", buf) :
				    tricat("argument-", buf, "")));
		return 0;
	    }
	    return 1;
	}
    case 'O':
	if (ca_laststate.opt || (ca_laststate.doff && ca_laststate.def)) {
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
		setsparam(args[2], ztrdup(opt->args->descr));
		setsparam(args[3], ztrdup(opt->args->action));

		if (args[4])
		    setsparam(args[4], tricat(opt->name, "-1", ""));

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
		    *p++ = ztrdup(o->name);
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
	    zerrnam(nam, "invalid separator: %s", args[1], 0);
	    return NULL;
	}
	hassep = 1;
	sep = args[1][0];
	args += 2;
    }
    if (!args[0] || !args[1]) {
	zerrnam(nam, "not enough arguments", NULL, 0);
	return NULL;
    }
    descr = *args++;

    PERMALLOC {
	ret = (Cvdef) zalloc(sizeof(*ret));
	ret->descr = ztrdup(descr);
	ret->hassep = hassep;
	ret->sep = sep;
	ret->next = NULL;
	ret->vals = NULL;
	ret->defs = arrdup(oargs);
	ret->ndefs = arrlen(oargs);
	ret->lastt = time(0);
    } LASTALLOC;

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
		zerrnam(nam, "invalid argument: %s", *args, 0);
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
	    zerrnam(nam, "no multi-letter values with empty separator allowed", NULL, 0);
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
		zerrnam(nam, "invalid value definition: %s", *args, 0);
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
	    zerrnam(nam, "invalid value definition: %s", *args, 0);
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
		zerrnam(nam, "no value with argument with empty separator allowed", NULL, 0);
		return NULL;
	    }
	    if (*++p == ':') {
		p++;
		vtype = CVV_OPT;
	    } else
		vtype = CVV_ARG;
	    arg = parse_caarg(0, 0, 0, name, &p);
	} else {
	    vtype = CVV_NOARG;
	    arg = NULL;
	}
	PERMALLOC {
	    *valp = val = (Cvval) zalloc(sizeof(*val));
	    valp = &((*valp)->next);

	    val->next = NULL;
	    val->name = ztrdup(name);
	    val->descr = ztrdup(descr);
	    val->xor = xor;
	    val->type = vtype;
	    val->arg = arg;
	} LASTALLOC;
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
    PERMALLOC {
	state.vals = (LinkList) newlinklist();
    } LASTALLOC;
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
		    PERMALLOC {
			addlinknode(state.vals, ztrdup(str));
			addlinknode(state.vals, ztrdup(eq));
		    } LASTALLOC;

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
			PERMALLOC {
			    addlinknode(state.vals, ztrdup(str));
			    addlinknode(state.vals, ztrdup(eq));
			} LASTALLOC;

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
		    PERMALLOC {
			addlinknode(state.vals, ztrdup(tmp));
			addlinknode(state.vals, ztrdup(""));
		    } LASTALLOC;

		    cv_inactive(d, ptr->xor);
		}
	    }
	    for (str = compsuffix; *str; str++) {
		tmp[0] = *str;
		if ((ptr = cv_get_val(d, tmp))) {
		    PERMALLOC {
			addlinknode(state.vals, ztrdup(tmp));
			addlinknode(state.vals, ztrdup(""));
		    } LASTALLOC;

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
	zerrnam(nam, "can only be called from completion function", NULL, 0);
	return 1;
    }
    if (args[0][0] != '-' || !args[0][1] || args[0][2]) {
	zerrnam(nam, "invalid argument: %s", args[0], 0);
	return 1;
    }
    if (args[0][1] != 'i' && !cv_parsed) {
	zerrnam(nam, "no parsed state", NULL, 0);
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
	zerrnam(nam, "invalid option: %s", args[0], 0);
	return 1;
    }
    n = arrlen(args) - 1;
    if (n < min) {
	zerrnam(nam, "not enough arguments", NULL, 0);
	return 1;
    } else if (max >= 0 && n > max) {
	zerrnam(nam, "too many arguments", NULL, 0);
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
    Value v;

    /* Anything to do? */

    if (!compqstack || !*compqstack)
	return 0;

    /* For all parameters given... */

    while ((name = *args++)) {
	name = dupstring(name);
	if ((v = getvalue(&name, 0))) {
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
settags(char **tags)
{
    Ctags t;

    if (comptags[locallevel])
	freectags(comptags[locallevel]);

    comptags[locallevel] = t = (Ctags) zalloc(sizeof(*t));

    PERMALLOC {
	t->all = arrdup(tags + 1);
    } LASTALLOC;
    t->context = ztrdup(*tags);
    t->sets = NULL;
    t->init = 1;
}

/* Check if an array contains a string. */

static int
arrcontains(char **a, char *s)
{
    while (*a)
	if (!strcmp(s, *a++))
	    return 1;

    return 0;
}

static int
bin_comptags(char *nam, char **args, char *ops, int func)
{
    int min, max, n;

    if (incompfunc != 1) {
	zerrnam(nam, "can only be called from completion function", NULL, 0);
	return 1;
    }
    if (args[0][0] != '-' || !args[0][1] || args[0][2]) {
	zerrnam(nam, "invalid argument: %s", args[0], 0);
	return 1;
    }
    if (locallevel >= MAX_TAGS) {
	zerrnam(nam, "nesting level too deep", NULL, 0);
	return 1;
    }
    if (args[0][1] != 'i' && !comptags[locallevel]) {
	zerrnam(nam, "no tags registered", NULL, 0);
	return 1;
    }
    switch (args[0][1]) {
    case 'i': min = 2; max = -1; break;
    case 'C': min = 1; max =  1; break;
    case 'T': min = 0; max =  0; break;
    case 'N': min = 0; max =  0; break;
    case 'R': min = 1; max =  1; break;
    case 'S': min = 1; max =  1; break;
    default:
	zerrnam(nam, "invalid option: %s", args[0], 0);
	return 1;
    }
    n = arrlen(args) - 1;
    if (n < min) {
	zerrnam(nam, "not enough arguments", NULL, 0);
	return 1;
    } else if (max >= 0 && n > max) {
	zerrnam(nam, "too many arguments", NULL, 0);
	return 1;
    }
    switch (args[0][1]) {
    case 'i':
	settags(args + 1);
	lasttaglevel = locallevel;
	break;
    case 'C':
	setsparam(args[1], ztrdup(comptags[locallevel]->context));
	break;
    case 'T':
	return !comptags[locallevel]->sets;
    case 'N':
	{
	    Ctset s;

	    if (comptags[locallevel]->init)
		comptags[locallevel]->init = 0;
	    else if ((s = comptags[locallevel]->sets)) {
		comptags[locallevel]->sets = s->next;
		s->next = NULL;
		freectset(s);
	    }
	    return !comptags[locallevel]->sets;
	}
    case 'R':
	{
	    Ctset s;

	    return !((s = comptags[locallevel]->sets) &&
		     arrcontains(s->tags, args[1]));
	}
    case 'S':
	if (comptags[locallevel]->sets) {
	    char **ret;

	    PERMALLOC {
		ret = arrdup(comptags[locallevel]->sets->tags);
	    } LASTALLOC;

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
	zerrnam(nam, "can only be called from completion function", NULL, 0);
	return 1;
    }
    if (!lasttaglevel || !comptags[lasttaglevel]) {
	zerrnam(nam, "no tags registered", NULL, 0);
	return 1;
    }
    if (*args) {
	char **p, **q, **all;

	args = arrdup(args);

	for (p = q = args, all = comptags[lasttaglevel]->all; *p; p++)
	    if (arrcontains(all, *p)) {
		Ctset s;

		for (s = comptags[lasttaglevel]->sets; s; s = s->next)
		    if (arrcontains(s->tags, *p))
			break;

		if (!s)
		    *q++ = *p;
	    }
	*q = NULL;

	if (*args) {
	    Ctset s = (Ctset) zalloc(sizeof(*s)), l;

	    PERMALLOC {
		s->tags = arrdup(args);
	    } LASTALLOC;
	    s->next = NULL;

	    if ((l = comptags[lasttaglevel]->sets)) {
		while (l->next)
		    l = l->next;

		l->next = s;
	    } else
		comptags[lasttaglevel]->sets = s;
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
	    zerrnam(nam, "invalid argument `%s'", args[0], 0);
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
