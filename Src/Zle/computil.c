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

typedef struct cdisp *Cdisp;

struct cdisp {
    int pre, suf, colon;
};

static void
cdisp_calc(Cdisp disp, char **args)
{
    char *cp;
    int i;

    for (; *args; args++) {
	if ((cp = strchr(*args, ':')) && cp[1]) {
	    disp->colon++;
	    if ((i = cp - *args) > disp->pre)
		disp->pre = i;
	    if ((i = strlen(cp + 1)) > disp->suf)
		disp->suf = i;
	}
    }
}

static char **
cdisp_build(Cdisp disp, char *sep, char **args)
{
    int sl = strlen(sep), pre = disp->pre, suf;
    VARARR(char, buf, disp->pre + disp->suf + sl + 1);
    char **ret, **rp, *cp;

    ret = (char **) zalloc((arrlen(args) + 1) * sizeof(char *));

    memcpy(buf + pre, sep, sl);
    suf = pre + sl;

    for (rp = ret; *args; args++) {
	if ((cp = strchr(*args, ':')) && cp[1]) {
	    memset(buf, ' ', pre);
	    memcpy(buf, *args, (cp - *args));
	    strcpy(buf + suf, cp + 1);
	    *rp++ = ztrdup(buf);
	} else {
	    if (cp)
		*cp = '\0';
	    *rp++ = ztrdup(*args);
	    if (cp)
		*cp = ':';
	}
    }
    *rp = NULL;

    return ret;
}

/**/
static int
bin_compdisplay(char *nam, char **args, char *ops, int func)
{
    struct cdisp disp;

    if (incompfunc != 1) {
	zerrnam(nam, "can only be called from completion function", NULL, 0);
	return 1;
    }
    disp.pre = disp.suf = disp.colon = 0;

    cdisp_calc(&disp, args + 2);
    setaparam(args[0], cdisp_build(&disp, args[1], args + 2));

    return !disp.colon;
}

/* Help fuer `_describe'. */

typedef struct cdset *Cdset;

struct cdstate {
    int showd;
    char *sep;
    Cdset sets;
    struct cdisp disp;
};

struct cdset {
    Cdset next;
    char **opts;
    char **strs;
    char **matches;
};

static struct cdstate cd_state;
static int cd_parsed = 0;

static void
free_cdsets(Cdset p)
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

static int
cd_init(char *nam, char *sep, char **args, int disp)
{
    Cdset *setp, set;
    char **ap, *tmp;

    if (cd_parsed) {
	zsfree(cd_state.sep);
	free_cdsets(cd_state.sets);
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

static int
cd_get(char **params)
{
    Cdset set;

    if ((set = cd_state.sets)) {
	char **sd, **sdp, **md, **mdp, **ss, **ssp, **ms, **msp;
	char **p, **mp, *cp;
	int dl = 1, sl = 1, sepl = strlen(cd_state.sep);
	int pre = cd_state.disp.pre, suf = cd_state.disp.suf;
	VARARR(char, buf, pre + suf + sepl + 1);

	for (p = set->strs; *p; p++)
	    if (cd_state.showd && (cp = strchr(*p, ':')) && cp[1])
		dl++;
	    else
		sl++;

	sd = (char **) zalloc(dl * sizeof(char *));
	ss = (char **) zalloc(sl * sizeof(char *));
	md = (char **) zalloc(dl * sizeof(char *));
	ms = (char **) zalloc(sl * sizeof(char *));

	if (cd_state.showd) {
	    memcpy(buf + pre, cd_state.sep, sepl);
	    suf = pre + sepl;
	}
	for (sdp = sd, ssp = ss, mdp = md, msp = ms,
		 p = set->strs, mp = set->matches; *p; p++) {
	    if ((cp = strchr(*p, ':')) && cp[1] && cd_state.showd) {
		memset(buf, ' ', pre);
		memcpy(buf, *p, (cp - *p));
		strcpy(buf + suf, cp + 1);
		*sdp++ = ztrdup(buf);
		if (mp) {
		    *mdp++ = ztrdup(*mp);
		    if (*mp)
			mp++;
		} else {
		    *cp = '\0';
		    *mdp++ = ztrdup(*p);
		    *cp = ':';
		}
	    } else {
		if (cp)
		    *cp = '\0';
		*ssp++ = ztrdup(*p);
		if (mp) {
		    *msp++ = ztrdup(*mp);
		    if (*mp)
			mp++;
		} else
		    *msp++ = ztrdup(*p);
		if (cp)
		    *cp = ':';
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
	free_cdsets(set);

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
    case 'I':
	cd_parsed = 1;
	return cd_init(nam, args[1], args + 2, (args[0][1] == 'I'));
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

struct cadef {
    Cadef next;
    Caopt opts;
    int nopts, ndopts, nodopts;
    Caarg args;
    Caarg rest;
    char **defs;
    int ndefs;
    int lastt;
    Caopt *single;
    char *match;
    int argsactive;
};

struct caopt {
    Caopt next;
    char *name;
    char *descr;
    char **xor;
    int type;
    Caarg args;
    int active;
    int num;
};

#define CAO_NEXT    1
#define CAO_DIRECT  2
#define CAO_ODIRECT 3
#define CAO_EQUAL   4

struct caarg {
    Caarg next;
    char *descr;
    char *action;
    int type;
    char *end;
    int num;
};

#define CAA_NORMAL 1
#define CAA_OPT    2
#define CAA_REST   3
#define CAA_RARGS  4
#define CAA_RREST  5

#define MAX_CACACHE 8
static Cadef cadef_cache[MAX_CACACHE];

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

static void
free_caargs(Caarg a)
{
    Caarg n;

    for (; a; a = n) {
	n = a->next;
	zsfree(a->descr);
	zsfree(a->action);
	zsfree(a->end);
	zfree(a, sizeof(*a));
    }
}

static void
free_cadef(Cadef d)
{
    if (d) {
	Caopt p, n;

	zsfree(d->match);
	freearray(d->defs);

	for (p = d->opts; p; p = n) {
	    n = p->next;
	    zsfree(p->name);
	    zsfree(p->descr);
	    freearray(p->xor);
	    free_caargs(p->args);
	    zfree(p, sizeof(*p));
	}
	free_caargs(d->args);
	free_caargs(d->rest);
	if (d->single)
	    zfree(d->single, 256 * sizeof(Caopt));
	zfree(d, sizeof(*d));
    }
}

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

static Caarg
parse_caarg(int mult, int type, int num, char **def)
{
    Caarg ret = (Caarg) zalloc(sizeof(*ret));
    char *p = *def, *d, sav;

    ret->next = NULL;
    ret->descr = ret->action = ret->end = NULL;
    ret->num = num;
    ret->type = type;

    for (d = p; *p && *p != ':'; p++)
	if (*p == '\\' && p[1])
	    p++;
    sav = *p;
    *p = '\0';
    ret->descr = ztrdup(rembslashcolon(d));
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

static Cadef
parse_cadef(char *nam, char **args)
{
    Cadef ret;
    Caopt *optp;
    char **oargs = args, *p, *q, *match = "r:|[_-]=* r:|=*", **xor;
    char *adpre, *adsuf;
    int single = 0, anum = 1, xnum, nopts, ndopts, nodopts;

    nopts = ndopts = nodopts = 0;

    for (p = args[0]; *p && (p[0] != '%' || p[1] != 'd'); p++);

    if (*p) {
	*p = '\0';
	adpre = dupstring(args[0]);
	*p = '%';
	adsuf = dupstring(p + 2);
    } else
	adpre = adsuf = NULL;

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

    for (optp = &(ret->opts); *args; args++) {
	p = dupstring(*args);
	xnum = 0;
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
		free_cadef(ret);
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
	    Caopt opt;
	    Caarg oargs = NULL;
	    int multi, otype = CAO_NEXT, again = 0;
	    char *name, *descr, c;

	    rec:

	    if ((multi = (*p == '*')))
		p++;

	    if ((p[0] == '-' && p[1] == '+') ||
		(p[0] == '+' && p[1] == '-')) {
		name = ++p;
		*p = (again ? '-' : '+');
		again = 1 - again;
	    } else {
		name = p;
		if (p[0] == '-' && p[1] == '-')
		    p++;
	    }
	    for (p++; *p && *p != ':' && *p != '[' &&
		     ((*p != '-' && *p != '+' && *p != '=') ||
		      (p[1] != ':' && p[1] != '[')); p++)
		if (*p == '\\' && p[1])
		    p++;

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
	    if (c == '[') {
		for (descr = ++p; *p && *p != ']'; p++)
		    if (*p == '\\' && p[1])
			p++;

		if (!*p) {
		    free_cadef(ret);
		    zerrnam(nam, "invalid option definition: %s", *args, 0);
		    return NULL;
		}
		*p++ = '\0';
		c = *p;
	    } else
		descr = NULL;

	    if (c && c != ':') {
		free_cadef(ret);
		zerrnam(nam, "invalid option definition: %s", *args, 0);
		return NULL;
	    }
	    if (!multi) {
		if (!xor) {
		    xor = (char **) zalloc(2 * sizeof(char *));
		    xor[1] = NULL;
		}
		xor[xnum] = ztrdup(name);
	    }
	    if (c == ':') {
		Caarg *oargp = &oargs;
		int atype, rest;
		char *end;

		while (c == ':') {
		    rest = 0;
		    end = NULL;

		    if (*++p == ':') {
			atype = CAA_OPT;
			p++;
		    } else if (*p == '*') {
			if (*++p != ':') {
			    for (end = ++p; *p && *p != ':'; p++)
				if (*p == '\\' && p[1])
				    p++;
			}
			if (*p != ':') {
			    free_cadef(ret);
			    free_caargs(oargs);
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
		    *oargp = parse_caarg(!rest, atype, 0, &p);
		    oargp = &((*oargp)->next);
		    if (rest)
			break;
		    c = *p;
		}
	    }
	    PERMALLOC {
		*optp = opt = (Caopt) zalloc(sizeof(*opt));
		optp = &((*optp)->next);

		opt->next = NULL;
		opt->name = ztrdup(name);
		if (descr)
		    opt->descr = ztrdup(descr);
		else if (adpre && oargs && !oargs->next)
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

	    if (single && name[1] && !name[2])
		ret->single[STOUC(name[1])] = opt;

	    if (again) {
		p = dupstring(*args);
		goto rec;
	    }
	} else if (*p == '*') {
	    int type = CAA_REST;

	    if (*++p != ':') {
		free_cadef(ret);
		zerrnam(nam, "invalid rest argument definition: %s", *args, 0);
		return NULL;
	    }
	    if (ret->rest) {
		free_cadef(ret);
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
	    ret->rest = parse_caarg(0, type, -1, &p);
	} else {
	    int type = CAA_NORMAL;
	    Caarg arg, tmp, pre;

	    if (idigit(*p)) {
		int num = 0;

		while (*p && idigit(*p))
		    num = (num * 10) + ((int) *p++);

		anum = num + 1;
	    } else
		anum++;

	    if (*p != ':') {
		free_cadef(ret);
		zerrnam(nam, "invalid argument: %s", *args, 0);
		return NULL;
	    }
	    if (*++p == ':') {
		type = CAA_OPT;
		p++;
	    }
	    arg = parse_caarg(0, type, anum - 1, &p);

	    for (tmp = ret->args, pre = NULL;
		 tmp && tmp->num < anum - 1;
		 pre = tmp, tmp = tmp->next);

	    if (tmp && tmp->num == anum - 1) {
		free_cadef(ret);
		free_caargs(arg);
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
	free_cadef(*min);
	*min = new;
    }
    return new;
}

static Caopt
ca_get_opt(Cadef d, char *line, int full, char **end)
{
    Caopt p;

    if (full) {
	for (p = d->opts; p; p = p->next)
	    if (p->active && !strcmp(p->name, line))
		return p;
    } else {
	for (p = d->opts; p; p = p->next)
	    if (p->active && ((!p->args || p->type == CAO_NEXT) ?
			      !strcmp(p->name, line) : strpfx(p->name, line))) {
		if (end) {
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

static Caopt
ca_get_sopt(Cadef d, char *line, int full, char **end)
{
    Caopt p;

    line++;

    if (full) {
	for (p = NULL; *line; line++)
	    if (!(p = d->single[STOUC(*line)]) || !p->active ||
		(line[1] && p->args))
		return NULL;
	return p;
    } else {
	for (p = NULL; *line; line++)
	    if ((p = d->single[STOUC(*line)]) && p->active &&
		p->args && p->type != CAO_NEXT) {
		if (end) {
		    line++;
		    if (p->type == CAO_EQUAL && *line == '=')
			line++;
		    *end = line;
		}
		break;
	    } else if (!p || !p->active || (line[1] && p->args))
		return NULL;
	if (end)
	    *end = line;
	return p;
    }
    return NULL;
}

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

static void
ca_parse_line(Cadef d)
{
    Caarg adef, ddef;
    Caopt ptr;
    struct castate state;
    char *line, *pe;
    int cur, doff;
    Patprog endpat = NULL;

    if (ca_alloced) {
	int i = ca_laststate.d->nopts;
	LinkList *p = ca_laststate.oargs;

	freelinklist(ca_laststate.args, freestr);
	while (i--)
	    if (*p++)
		freelinklist(p[-1], freestr);
    }
    for (ptr = d->opts; ptr; ptr = ptr->next)
	ptr->active = 1;
    d->argsactive = 1;

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
    for (line = compwords[1], cur = 2, state.curopt = NULL, state.def = NULL;
	 line; line = compwords[cur++]) {
	ddef = adef = NULL;
	doff = state.singles = 0;
	if (state.def) {
	    state.arg = 0;
	    if (state.curopt) {
		PERMALLOC {
		    addlinknode(state.oargs[state.curopt->num], ztrdup(line));
		} LASTALLOC;
	    }
	    state.opt = (state.def->type == CAA_OPT && line[0] && line[1]);

	    if (state.def->type == CAA_REST || state.def->type == CAA_RARGS ||
		state.def->type == CAA_RREST) {
		if (state.def->end && pattry(endpat, line)) {
		    state.def = NULL;
		    state.curopt = NULL;
		    continue;
		}
	    } else if ((state.def = state.def->next))
		state.argbeg = cur;
	    else
		state.curopt = NULL;
	} else {
	    state.opt = (line[0] && line[1]);
	    state.arg = 1;
	    state.curopt = NULL;
	}
	pe = NULL;

	if (state.opt && (state.curopt = ca_get_opt(d, line, 0, &pe))) {
	    ddef = state.def = state.curopt->args;
	    doff = pe - line;
	    state.optbeg = state.argbeg = state.inopt = cur;
	    state.singles = (d->single && (!pe || !*pe) &&
			     state.curopt->name[1] && !state.curopt->name[2]);

	    PERMALLOC {
		state.oargs[state.curopt->num] = newlinklist();
	    } LASTALLOC;
	    ca_inactive(d, state.curopt->xor);

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
	    if (!state.def)
		state.curopt = NULL;
	} else if (state.opt && d->single &&
		   (state.curopt = ca_get_sopt(d, line, 0, &pe))) {
	    char *p;
	    Caopt tmpopt;

	    ddef = state.def = state.curopt->args;
	    doff = pe - line;
	    state.optbeg = state.argbeg = state.inopt = cur;
	    state.singles = (!pe || !*pe);

	    for (p = line + 1; p <= pe; p++) {
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
	    if (!state.def)
		state.curopt = NULL;
	} else if (state.arg) {
	    PERMALLOC {
		addlinknode(state.args, ztrdup(line));
	    } LASTALLOC;
	    if ((adef = state.def = ca_get_arg(d, state.nth)) &&
		(state.def->type == CAA_RREST ||
		 state.def->type == CAA_RARGS)) {
		state.inrest = 0;
		for (; line; line = compwords[cur++]) {
		    PERMALLOC {
			addlinknode(state.args, ztrdup(line));
		    } LASTALLOC;
		}
		break;
	    }
	    if (state.inopt) {
		state.inopt = 0;
		state.nargbeg = cur - 1;
	    }
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
	if (state.def && state.curopt &&
	    (state.def->type == CAA_RREST || state.def->type == CAA_RARGS)) {
	    if (state.def->end)
		endpat = patcompile(state.def->end, 0, NULL);
	    else {
		LinkList l = state.oargs[state.curopt->num];

		for (; line; line = compwords[cur++]) {
		    PERMALLOC {
			addlinknode(l, line);
		    } LASTALLOC;
		}
		break;
	    }
	}
	if (cur + 1 == compcurrent) {
	    memcpy(&ca_laststate, &state, sizeof(state));
	    ca_laststate.ddef = NULL;
	    ca_laststate.doff = 0;
	} else if (cur == compcurrent && !ca_laststate.def) {
	    if ((ca_laststate.def = ddef))
		ca_laststate.doff = doff;
	    else {
		ca_laststate.def = adef;
		ca_laststate.ddef = NULL;
		ca_laststate.argbeg = state.nargbeg;
		ca_laststate.optbeg = state.restbeg;
		ca_laststate.singles = state.singles;
	    }
	}
    }
}

static char *
ca_colonlist(LinkList l)
{
    if (l) {
	LinkNode n;
	int len = 1;
	char *p, *ret, *q;

	for (n = firstnode(l); n; incnode(n))
	    for (p = (char *) getdata(n); *p; p++)
		len += (*p == ':' ? 2 : 1);

	ret = q = (char *) zalloc(len);

	for (n = firstnode(l); n; incnode(n))
	    for (p = (char *) getdata(n); *p; p++) {
		if (*p == ':')
		    *q++ = '\\';
		*q++ = *p;
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
    case 'O': min = 4; max =  4; break;
    case 'L': min = 3; max =  3; break;
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
		    restrict_range(ca_laststate.argbeg - 1,
				   arrlen(compwords) - 1);
		else if (arg->type == CAA_RREST)
		    restrict_range(ca_laststate.optbeg - 1,
				   arrlen(compwords) - 1);
		return 0;
	    }
	    return 1;
	}
    case 'O':
	if (ca_laststate.opt) {
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
	    set_list_array(args[1], next);
	    set_list_array(args[2], direct);
	    set_list_array(args[3], odirect);
	    set_list_array(args[4], equal);

	    return 0;
	} else
	    return 1;
    case 'L':
	{
	    Caopt opt = ca_get_opt(ca_laststate.d, args[1], 1, NULL);

	    if (opt && opt->args) {
		setsparam(args[2], ztrdup(opt->args->descr));
		setsparam(args[3], ztrdup(opt->args->action));

		return 0;
	    }
	    return 1;
	}
    case 's':
	if (ca_laststate.d->single && ca_laststate.singles) {
	    setsparam(args[1],
		      ztrdup(ca_laststate.ddef ?
			     (ca_laststate.ddef->type == CAO_DIRECT ?
			      "direct" :
			      (ca_laststate.ddef->type == CAO_EQUAL ?
			       "equal" : "next")) : ""));
	    return 0;
	} else
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

struct cvdef {
    char *descr;
    int hassep;
    char sep;
    Cvdef next;
    Cvval vals;
    char **defs;
    int ndefs;
    int lastt;
};

struct cvval {
    Cvval next;
    char *name;
    char *descr;
    char **xor;
    int type;
    Caarg arg;
    int active;
};

#define CVV_NOARG 0
#define CVV_ARG   1
#define CVV_OPT   2

#define MAX_CVCACHE 8
static Cvdef cvdef_cache[MAX_CVCACHE];

static void
free_cvdef(Cvdef d)
{
    if (d) {
	Cvval p, n;

	zsfree(d->descr);
	freearray(d->defs);

	for (p = d->vals; p; p = n) {
	    n = p->next;
	    zsfree(p->name);
	    zsfree(p->descr);
	    freearray(p->xor);
	    free_caargs(p->arg);
	    zfree(p, sizeof(*p));
	}
	zfree(d, sizeof(*d));
    }
}

static Cvdef
parse_cvdef(char *nam, char **args)
{
    Cvdef ret;
    Cvval val, *valp;
    Caarg arg;
    char **oargs = args, sep, *name, *descr, *p, *q, **xor, c;
    int xnum, multi, vtype, hassep;

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
		free_cvdef(ret);
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

	if ((multi = (*p == '*')))
	    p++;

	for (name = p; *p && *p != ':' && *p != '['; p++)
	    if (*p == '\\' && p[1])
		p++;

	if (hassep && !sep && name + 1 != p) {
	    free_cvdef(ret);
	    zerrnam(nam, "no multi-letter values with empty separator allowed", NULL, 0);
	    return NULL;
	}
	if ((c = *p) == '[') {
	    *p = '\0';
	    for (descr = ++p; *p && *p != ']'; p++)
		if (*p == '\\' && p[1])
		    p++;

	    if (!*p) {
		free_cvdef(ret);
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
	    free_cvdef(ret);
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
	if (c == ':') {
	    if (hassep && !sep) {
		free_cvdef(ret);
		zerrnam(nam, "no value with argument with empty separator allowed", NULL, 0);
		return NULL;
	    }
	    if (*++p == ':') {
		p++;
		vtype = CVV_OPT;
	    } else
		vtype = CVV_ARG;
	    arg = parse_caarg(0, 0, 0, &p);
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
	free_cvdef(*min);
	*min = new;
    }
    return new;
}

static Cvval
cv_get_val(Cvdef d, char *name)
{
    Cvval p;

    for (p = d->vals; p; p = p->next)
	if (!strcmp(name, p->name))
	    return p;

    return NULL;
}

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

struct cvstate {
    Cvdef d;
    Caarg def;
    Cvval val;
    LinkList vals;
};

static struct cvstate cv_laststate;
static int cv_parsed = 0, cv_alloced = 0;

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
    case 'V': min = 3; max =  3; break;
    case 's': min = 1; max =  1; break;
    case 'd': min = 1; max =  1; break;
    case 'L': min = 3; max =  3; break;
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


static struct builtin bintab[] = {
    BUILTIN("compdisplay", 0, bin_compdisplay, 2, -1, 0, NULL, NULL),
    BUILTIN("compdescribe", 0, bin_compdescribe, 3, -1, 0, NULL, NULL),
    BUILTIN("comparguments", 0, bin_comparguments, 1, -1, 0, NULL, NULL),
    BUILTIN("compvalues", 0, bin_compvalues, 1, -1, 0, NULL, NULL),
};


/**/
int
setup_computil(Module m)
{
    memset(cadef_cache, 0, sizeof(cadef_cache));
    memset(cvdef_cache, 0, sizeof(cvdef_cache));

    return 0;
}

/**/
int
boot_computil(Module m)
{
    return !addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
}

#ifdef MODULE

/**/
int
cleanup_computil(Module m)
{
    deletebuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
    return 0;
}

/**/
int
finish_computil(Module m)
{
    int i;

    for (i = 0; i < MAX_CACACHE; i++)
	free_cadef(cadef_cache[i]);
    for (i = 0; i < MAX_CVCACHE; i++)
	free_cvdef(cvdef_cache[i]);

    return 0;
}

#endif
