/*
 * complete.c - the complete module
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

#include "complete.mdh"
#include "complete.pro"
#define GLOBAL_PROTOTYPES
#include "zle_tricky.pro"
#undef GLOBAL_PROTOTYPES

/**/
void
freecmlist(Cmlist l)
{
    Cmlist n;

    while (l) {
	n = l->next;
	freecmatcher(l->matcher);
	zsfree(l->str);

	zfree(l, sizeof(struct cmlist));

	l = n;
    }
}

/**/
void
freecmatcher(Cmatcher m)
{
    Cmatcher n;

    if (!m || --(m->refc))
	return;

    while (m) {
	n = m->next;
	freecpattern(m->line);
	freecpattern(m->word);
	freecpattern(m->left);
	freecpattern(m->right);

	zfree(m, sizeof(struct cmatcher));

	m = n;
    }
}

/**/
void
freecpattern(Cpattern p)
{
    Cpattern n;

    while (p) {
	n = p->next;
	zfree(p, sizeof(struct cpattern));

	p = n;
    }
}

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
Cmatcher
cpcmatcher(Cmatcher m)
{
    Cmatcher r = NULL, *p = &r, n;

    while (m) {
	*p = n = (Cmatcher) zalloc(sizeof(struct cmatcher));

	n->refc = 1;
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

/* Set the global match specs. */

/**/
int
set_gmatcher(char *name, char **argv)
{
    Cmlist l = NULL, *q = &l, n;
    Cmatcher m;

    while (*argv) {
	if ((m = parse_cmatcher(name, *argv)) == pcm_err)
	    return 1;
	*q = n = (Cmlist) zhalloc(sizeof(struct cmlist));
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

/* Parse a string for matcher control, containing multiple matchers. */

/**/
Cmatcher
parse_cmatcher(char *name, char *s)
{
    Cmatcher ret = NULL, r = NULL, n;
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
	if ((fl & CMF_RIGHT) && (!*s || !*++s)) {
	    zwarnnam(name, "missing right anchor", NULL, 0);
	} else if (!(fl & CMF_RIGHT)) {
	    if (!*s) {
		zwarnnam(name, "missing word pattern", NULL, 0);
		return pcm_err;
	    }
	    s++;
	}
	if (fl & CMF_RIGHT) {
	    right = parse_pattern(name, &s, &ral, '=', &err);
	    if (err)
		return pcm_err;
	    if (!*s) {
		zwarnnam(name, "missing word pattern", NULL, 0);
		return pcm_err;
	    }
	    s++;
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

	if (ret)
	    r->next = n;
	else
	    ret = n;

	r = n;
    }
    return ret;
}

/* Parse a pattern for matcher control. */

/**/
static Cpattern
parse_pattern(char *name, char **sp, int *lp, char e, int *err)
{
    Cpattern ret = NULL, r = NULL, n;
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

/**/
static int
bin_compadd(char *name, char **argv, char *ops, int func)
{
    struct cadata dat;
    char *p, **sp, *e, *m = NULL;
    int dm;
    Cmatcher match = NULL;

    if (incompfunc != 1) {
	zerrnam(name, "can only be called from completion function", NULL, 0);
	return 1;
    }
    dat.ipre = dat.isuf = dat.ppre = dat.psuf = dat.prpre =
	dat.pre = dat.suf = dat.group = dat.rems = dat.remf = dat.disp = 
	dat.ign = dat.exp = dat.apar = dat.opar = dat.dpar = dat.ylist = NULL;
    dat.match = NULL;
    dat.flags = 0;
    dat.aflags = CAF_MATCH;

    for (; *argv && **argv ==  '-'; argv++) {
	if (!(*argv)[1]) {
	    argv++;
	    break;
	}
	for (p = *argv + 1; *p; p++) {
	    sp = NULL;
	    e = NULL;
	    dm = 0;
	    switch (*p) {
	    case 'q':
		dat.flags |= CMF_REMOVE;
		break;
	    case 'Q':
		dat.aflags |= CAF_QUOTE;
		break;
	    case 'f':
		dat.flags |= CMF_FILE;
		break;
	    case 'e':
		dat.flags |= CMF_ISPAR;
		break;
	    case 'F':
		sp = &(dat.ign);
		e = "string expected after -%c";
		break;
	    case 'n':
		dat.flags |= CMF_NOLIST;
		break;
	    case 'U':
		dat.aflags &= ~CAF_MATCH;
		break;
	    case 'P':
		sp = &(dat.pre);
		e = "string expected after -%c";
		break;
	    case 'S':
		sp = &(dat.suf);
		e = "string expected after -%c";
		break;
	    case 'J':
		sp = &(dat.group);
		e = "group name expected after -%c";
		break;
	    case 'V':
		if (!dat.group)
		    dat.aflags |= CAF_NOSORT;
		sp = &(dat.group);
		e = "group name expected after -%c";
		break;
	    case '1':
		if (!(dat.aflags & CAF_UNIQCON))
		    dat.aflags |= CAF_UNIQALL;
		break;
	    case '2':
		if (!(dat.aflags & CAF_UNIQALL))
		    dat.aflags |= CAF_UNIQCON;
		break;
	    case 'y':
		sp = &(dat.ylist);
		e = "string expected after -%c";
		break;
	    case 'i':
		sp = &(dat.ipre);
		e = "string expected after -%c";
		break;
	    case 'I':
		sp = &(dat.isuf);
		e = "string expected after -%c";
		break;
	    case 'p':
		sp = &(dat.ppre);
		e = "string expected after -%c";
		break;
	    case 's':
		sp = &(dat.psuf);
		e = "string expected after -%c";
		break;
	    case 'W':
		sp = &(dat.prpre);
		e = "string expected after -%c";
		break;
	    case 'a':
		dat.aflags |= CAF_ALT;
		break;
	    case 'M':
		sp = &m;
		e = "matching specification expected after -%c";
		dm = 1;
		break;
	    case 'X':
		sp = &(dat.exp);
		e = "string expected after -%c";
		break;
	    case 'r':
		dat.flags |= CMF_REMOVE;
		sp = &(dat.rems);
		e = "string expected after -%c";
		break;
	    case 'R':
		dat.flags |= CMF_REMOVE;
		sp = &(dat.remf);
		e = "function name expected after -%c";
		break;
	    case 'A':
		sp = &(dat.apar);
		e = "parameter name expected after -%c";
		break;
	    case 'O':
		sp = &(dat.opar);
		e = "parameter name expected after -%c";
		break;
	    case 'D':
		sp = &(dat.dpar);
		e = "parameter name expected after -%c";
		break;
	    case 'd':
		sp = &(dat.disp);
		e = "parameter name expected after -%c";
		break;
	    case 'l':
		dat.flags |= CMF_DISPLINE;
		break;
	    case '-':
		argv++;
		goto ca_args;
	    default:
		zerrnam(name, "bad option: -%c", NULL, *p);
		return 1;
	    }
	    if (sp) {
		if (p[1]) {
		    if (!*sp)
			*sp = p + 1;
		    p = "" - 1;
		} else if (argv[1]) {
		    argv++;
		    if (!*sp)
			*sp = *argv;
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

    dat.match = match = cpcmatcher(match);
    dm = addmatches(&dat, argv);
    freecmatcher(match);

    return dm;
}

#define CVT_RANGENUM 0
#define CVT_RANGEPAT 1
#define CVT_PRENUM   2
#define CVT_PREPAT   3
#define CVT_SUFNUM   4
#define CVT_SUFPAT   5

/**/
void
ignore_prefix(int l)
{
    if (l) {
	char *tmp, sav;
	int pl = strlen(compprefix);

	if (l > pl)
	    l = pl;

	sav = compprefix[l];

	compprefix[l] = '\0';
	tmp = tricat(compiprefix, compprefix, "");
	zsfree(compiprefix);
	compiprefix = tmp;
	compprefix[l] = sav;
	tmp = ztrdup(compprefix + l);
	zsfree(compprefix);
	compprefix = tmp;
    }
}

/**/
void
ignore_suffix(int l)
{
    if (l) {
	char *tmp, sav;
	int sl = strlen(compsuffix);

	if ((l = sl - l) < 0)
	    l = 0;

	tmp = tricat(compsuffix + l, compisuffix, "");
	zsfree(compisuffix);
	compisuffix = tmp;
	sav = compsuffix[l];
	compsuffix[l] = '\0';
	tmp = ztrdup(compsuffix);
	compsuffix[l] = sav;
	zsfree(compsuffix);
	compsuffix = tmp;
    }
}

/**/
void
restrict_range(int b, int e)
{
    int wl = arrlen(compwords) - 1;

    if (wl && b >= 0 && e >= 0 && (b > 0 || e < wl)) {
	int i;
	char **p, **q, **pp;

	if (e > wl)
	    e = wl;

	i = e - b + 1;
	p = (char **) zcalloc((i + 1) * sizeof(char *));

	for (q = p, pp = compwords + b; i; i--, q++, pp++)
	    *q = ztrdup(*pp);
	freearray(compwords);
	compwords = p;
	compcurrent -= b;
    }
}

static int
do_comp_vars(int test, int na, char *sa, int nb, char *sb, int mod)
{
    switch (test) {
    case CVT_RANGENUM:
	{
	    int l = arrlen(compwords);

	    if (na < 0)
		na += l;
	    else
		na--;
	    if (nb < 0)
		nb += l;
	    else
		nb--;

	    if (compcurrent - 1 < na || compcurrent - 1 > nb)
		return 0;
	    if (mod)
		restrict_range(na, nb);
	    return 1;
	}
    case CVT_RANGEPAT:
	{
	    char **p;
	    int i, l = arrlen(compwords), t = 0, b = 0, e = l - 1;
	    Patprog pp;

	    i = compcurrent - 1;
	    if (i < 0 || i >= l)
		return 0;

	    singsub(&sa);
	    pp = patcompile(sa, PAT_STATIC, NULL);

	    for (i--, p = compwords + i; i >= 0; p--, i--) {
		if (pattry(pp, *p)) {
		    b = i + 1;
		    t = 1;
		    break;
		}
	    }
	    if (t && sb) {
		int tt = 0;

		singsub(&sb);
		pp = patcompile(sb, PAT_STATIC, NULL);

		for (i++, p = compwords + i; i < l; p++, i++) {
		    if (pattry(pp, *p)) {
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
	    if (t && mod)
		restrict_range(b, e);
	    return t;
	}
    case CVT_PRENUM:
    case CVT_SUFNUM:
	if (!na)
	    return 1;
	if (na > 0 &&
	    strlen(test == CVT_PRENUM ? compprefix : compsuffix) >= na) {
	    if (mod) {
		if (test == CVT_PRENUM)
		    ignore_prefix(na);
		else
		    ignore_suffix(na);
		return 1;
	    }
	    return 0;
	}
    case CVT_PREPAT:
    case CVT_SUFPAT:
	{
	    Patprog pp;

	    if (!na)
		return 0;

	    if (!(pp = patcompile(sa, PAT_STATIC, 0)))
		return 0;

	    if (test == CVT_PREPAT) {
		int l, add;
		char *p, sav;

		if (!(l = strlen(compprefix)))
		    return 0;
		if (na < 0) {
		    p = compprefix + l;
		    na = -na;
		    add = -1;
		} else {
		    p = compprefix + 1;
		    add = 1;
		}
		for (; l; l--, p += add) {
		    sav = *p;
		    *p = '\0';
		    test = pattry(pp, compprefix);
		    *p = sav;
		    if (test && !--na)
			break;
		}
		if (!l)
		    return 0;
		if (mod)
		    ignore_prefix(p - compprefix);
	    } else {
		int l, ol, add;
		char *p;

		if (!(ol = l = strlen(compsuffix)))
		    return 0;
		if (na < 0) {
		    p = compsuffix;
		    na = -na;
		    add = 1;
		} else {
		    p = compsuffix + l - 1;
		    add = -1;
		}
		for (; l; l--, p += add)
		    if (pattry(pp, p) && !--na)
			break;

		if (!l)
		    return 0;
		if (mod)
		    ignore_suffix(ol - (p - compsuffix));
	    }
	    return 1;
	}
    }
    return 0;
}

/**/
static int
bin_compset(char *name, char **argv, char *ops, int func)
{
    int test = 0, na = 0, nb = 0;
    char *sa = NULL, *sb = NULL;

    if (incompfunc != 1) {
	zerrnam(name, "can only be called from completion function", NULL, 0);
	return 1;
    }
    if (argv[0][0] != '-') {
	zerrnam(name, "missing option", NULL, 0);
	return 1;
    }
    switch (argv[0][1]) {
    case 'n': test = CVT_RANGENUM; break;
    case 'N': test = CVT_RANGEPAT; break;
    case 'p': test = CVT_PRENUM; break;
    case 'P': test = CVT_PREPAT; break;
    case 's': test = CVT_SUFNUM; break;
    case 'S': test = CVT_SUFPAT; break;
    case 'q': return set_comp_sep();
    default:
	zerrnam(name, "bad option -%c", NULL, argv[0][1]);
	return 1;
    }
    if (argv[0][2]) {
	sa = argv[0] + 2;
	sb = argv[1];
	na = 2;
    } else {
	if (!(sa = argv[1])) {
	    zerrnam(name, "missing string for option -%c", NULL, argv[0][1]);
	    return 1;
	}
	sb = argv[2];
	na = 3;
    }
    if (((test == CVT_PRENUM || test == CVT_SUFNUM) ? !!sb :
	 (sb && argv[na]))) {
	zerrnam(name, "too many arguments", NULL, 0);
	return 1;
    }
    switch (test) {
    case CVT_RANGENUM:
	na = atoi(sa);
	nb = (sb ? atoi(sb) : -1);
	break;
    case CVT_RANGEPAT:
	tokenize(sa);
	sa = rembslash(sa);
	remnulargs(sa);
	if (sb) {
	    tokenize(sb);
	    sb = rembslash(sb);
	    remnulargs(sb);
	}
	break;
    case CVT_PRENUM:
    case CVT_SUFNUM:
	na = atoi(sa);
	break;
    case CVT_PREPAT:
    case CVT_SUFPAT:
	if (sb) {
	    na = atoi(sa);
	    sa = sb;
	} else
	    na = -1;
	tokenize(sa);
	sa = rembslash(sa);
	remnulargs(sa);
	break;
    }
    return !do_comp_vars(test, na, sa, nb, sb, 1);
}

/* Definitions for the special parameters. Note that these have to match the
 * order of the CP_* bits in comp.h */

#define VAL(X) ((void *) (&(X)))
struct compparam {
    char *name;
    int type;
    void *var, *set, *get;
};

static struct compparam comprparams[] = {
    { "words", PM_ARRAY, VAL(compwords), NULL, NULL },
    { "CURRENT", PM_INTEGER, VAL(compcurrent), NULL, NULL },
    { "PREFIX", PM_SCALAR, VAL(compprefix), NULL, NULL },
    { "SUFFIX", PM_SCALAR, VAL(compsuffix), NULL, NULL },
    { "IPREFIX", PM_SCALAR, VAL(compiprefix), NULL, NULL },
    { "ISUFFIX", PM_SCALAR, VAL(compisuffix), NULL, NULL },
    { "QIPREFIX", PM_SCALAR | PM_READONLY, VAL(compqiprefix), NULL, NULL },
    { "QISUFFIX", PM_SCALAR | PM_READONLY, VAL(compqisuffix), NULL, NULL },
    { NULL, 0, NULL, NULL, NULL }
};

static struct compparam compkparams[] = {
    { "nmatches", PM_INTEGER | PM_READONLY, NULL, NULL, VAL(get_nmatches) },
    { "matcher", PM_INTEGER, VAL(compmatcher), NULL, NULL },
    { "matcher_string", PM_SCALAR, VAL(compmatcherstr), NULL, NULL },
    { "total_matchers", PM_INTEGER, VAL(compmatchertot), NULL, NULL },
    { "context", PM_SCALAR, VAL(compcontext), NULL, NULL },
    { "parameter", PM_SCALAR, VAL(compparameter), NULL, NULL },
    { "redirect", PM_SCALAR, VAL(compredirect), NULL, NULL },
    { "quote", PM_SCALAR | PM_READONLY, VAL(compquote), NULL, NULL },
    { "quoting", PM_SCALAR | PM_READONLY, VAL(compquoting), NULL, NULL },
    { "restore", PM_SCALAR, VAL(comprestore), NULL, NULL },
    { "list", PM_SCALAR, NULL, VAL(set_complist), VAL(get_complist) },
    { "force_list", PM_SCALAR, VAL(compforcelist), NULL, NULL },
    { "insert", PM_SCALAR, VAL(compinsert), NULL, NULL },
    { "exact", PM_SCALAR, VAL(compexact), NULL, NULL },
    { "exact_string", PM_SCALAR, VAL(compexactstr), NULL, NULL },
    { "pattern_match", PM_SCALAR, VAL(comppatmatch), NULL, NULL },
    { "pattern_insert", PM_SCALAR, VAL(comppatinsert), NULL, NULL },
    { "unambiguous", PM_SCALAR | PM_READONLY, NULL, NULL, VAL(get_unambig) },
    { "unambiguous_cursor", PM_INTEGER | PM_READONLY, NULL, NULL,
      VAL(get_unambig_curs) },
    { "list_max", PM_INTEGER, VAL(complistmax), NULL, NULL },
    { "last_prompt", PM_SCALAR, VAL(complastprompt), NULL, NULL },
    { "to_end", PM_SCALAR, VAL(comptoend), NULL, NULL },
    { "old_list", PM_SCALAR, VAL(compoldlist), NULL, NULL },
    { "old_insert", PM_SCALAR, VAL(compoldins), NULL, NULL },
    { "vared", PM_SCALAR, VAL(compvared), NULL, NULL },
    { "alternate_nmatches", PM_INTEGER | PM_READONLY, NULL, NULL, VAL(get_anmatches) },
    { "list_lines", PM_INTEGER | PM_READONLY, NULL, NULL, VAL(get_listlines) },
    { NULL, 0, NULL, NULL, NULL }
};

#define COMPSTATENAME "compstate"

static void
addcompparams(struct compparam *cp, Param *pp)
{
    for (; cp->name; cp++, pp++) {
	Param pm = createparam(cp->name,
			       cp->type |PM_SPECIAL|PM_REMOVABLE|PM_LOCAL);
	if (!pm)
	    pm = (Param) paramtab->getnode(paramtab, cp->name);
	DPUTS(!pm, "param not set in addcompparams");

	*pp = pm;
	pm->level = locallevel + 1;
	if ((pm->u.data = cp->var)) {
	    switch(PM_TYPE(cp->type)) {
	    case PM_SCALAR:
		pm->sets.cfn = strvarsetfn;
		pm->gets.cfn = strvargetfn;
		break;
	    case PM_INTEGER:
		pm->sets.ifn = intvarsetfn;
		pm->gets.ifn = intvargetfn;
		pm->ct = 10;
		break;
	    case PM_ARRAY:
		pm->sets.afn = arrvarsetfn;
		pm->gets.afn = arrvargetfn;
		break;
	    }
	} else {
	    pm->sets.cfn = (void (*) _((Param, char *))) cp->set;
	    pm->gets.cfn = (char *(*) _((Param))) cp->get;
	}
	pm->unsetfn = compunsetfn;
    }
}

/**/
void
makecompparams(void)
{
    Param cpm;
    HashTable tht;

    addcompparams(comprparams, comprpms);

    if (!(cpm = createparam(COMPSTATENAME,
			    PM_SPECIAL|PM_REMOVABLE|PM_LOCAL|PM_HASHED)))
	cpm = (Param) paramtab->getnode(paramtab, COMPSTATENAME);
    DPUTS(!cpm, "param not set in makecompparams");

    comprpms[CPN_COMPSTATE] = cpm;
    tht = paramtab;
    cpm->level = locallevel;
    cpm->gets.hfn = get_compstate;
    cpm->sets.hfn = set_compstate;
    cpm->unsetfn = compunsetfn;
    cpm->u.hash = paramtab = newparamtable(31, COMPSTATENAME);
    addcompparams(compkparams, compkpms);
    paramtab = tht;
}

/**/
static HashTable
get_compstate(Param pm)
{
    return pm->u.hash;
}

/**/
static void
set_compstate(Param pm, HashTable ht)
{
    struct compparam *cp;
    Param *pp;
    HashNode hn;
    int i;
    struct value v;
    char *str;

    for (i = 0; i < ht->hsize; i++)
	for (hn = ht->nodes[i]; hn; hn = hn->next)
	    for (cp = compkparams,
		 pp = compkpms; cp->name; cp++, pp++)
		if (!strcmp(hn->nam, cp->name)) {
		    v.isarr = v.inv = v.a = 0;
		    v.b = -1;
		    v.arr = NULL;
		    v.pm = (Param) hn;
		    if (cp->type == PM_INTEGER)
			*((zlong *) cp->var) = getintvalue(&v);
		    else if ((str = getstrvalue(&v))) {
			zsfree(*((char **) cp->var));
			*((char **) cp->var) = ztrdup(str);
		    }
		    (*pp)->flags &= ~PM_UNSET;

		    break;
		}
    deleteparamtable(ht);
}

/**/
static zlong
get_nmatches(Param pm)
{
    return num_matches(1);
}

/**/
static zlong
get_anmatches(Param pm)
{
    return num_matches(0);
}

/**/
static zlong
get_listlines(Param pm)
{
    return list_lines();
}

/**/
static void
set_complist(Param pm, char *v)
{
    comp_list(v);
}

/**/
static char *
get_complist(Param pm)
{
    return complist;
}

/**/
static char *
get_unambig(Param pm)
{
    return unambig_data(NULL);
}

/**/
static zlong
get_unambig_curs(Param pm)
{
    int c;

    unambig_data(&c);

    return c;
}

/**/
static void
compunsetfn(Param pm, int exp)
{
    if (exp) {
	if (PM_TYPE(pm->flags) == PM_SCALAR) {
	    zsfree(*((char **) pm->u.data));
	    *((char **) pm->u.data) = ztrdup("");
	} else if (PM_TYPE(pm->flags) == PM_ARRAY) {
	    freearray(*((char ***) pm->u.data));
	    *((char ***) pm->u.data) = zcalloc(sizeof(char *));
	}
	pm->flags |= PM_UNSET;
    }
}

/**/
void
comp_setunset(int rset, int runset, int kset, int kunset)
{
    Param *p;

    if (comprpms && (rset >= 0 || runset >= 0)) {
	for (p = comprpms; rset || runset; rset >>= 1, runset >>= 1, p++) {
	    if (rset & 1)
		(*p)->flags &= ~PM_UNSET;
	    if (runset & 1)
		(*p)->flags |= PM_UNSET;
	}
    }
    if (comprpms && (kset >= 0 || kunset >= 0)) {
	for (p = compkpms; kset || kunset; kset >>= 1, kunset >>= 1, p++) {
	    if (kset & 1)
		(*p)->flags &= ~PM_UNSET;
	    if (kunset & 1)
		(*p)->flags |= PM_UNSET;
	}
    }
}

/**/
static int
comp_wrapper(List list, FuncWrap w, char *name)
{
    if (incompfunc != 1)
	return 1;
    else {
	char *orest, *opre, *osuf, *oipre, *oisuf, **owords;
	char *oqipre, *oqisuf, *oq, *oqi;
	zlong ocur;
	unsigned int runset = 0, kunset = 0, m, sm;
	Param *pp;

	m = CP_WORDS | CP_CURRENT | CP_PREFIX | CP_SUFFIX | 
	    CP_IPREFIX | CP_ISUFFIX | CP_QIPREFIX | CP_QISUFFIX;
	for (pp = comprpms, sm = 1; m; pp++, m >>= 1, sm <<= 1) {
	    if ((m & 1) && ((*pp)->flags & PM_UNSET))
		runset |= sm;
	}
	if (compkpms[CPN_RESTORE]->flags & PM_UNSET)
	    kunset = CP_RESTORE;
	orest = comprestore;
	comprestore = ztrdup("auto");
	ocur = compcurrent;
	opre = dupstring(compprefix);
	osuf = dupstring(compsuffix);
	oipre = dupstring(compiprefix);
	oisuf = dupstring(compisuffix);
	oqipre = dupstring(compqiprefix);
	oqisuf = dupstring(compqisuffix);
	oq = dupstring(compquote);
	oqi = dupstring(compquoting);

	HEAPALLOC {
	    owords = arrdup(compwords);
	} LASTALLOC;

	runshfunc(list, w, name);

	if (comprestore && !strcmp(comprestore, "auto")) {
	    compcurrent = ocur;
	    zsfree(compprefix);
	    compprefix = ztrdup(opre);
	    zsfree(compsuffix);
	    compsuffix = ztrdup(osuf);
	    zsfree(compiprefix);
	    compiprefix = ztrdup(oipre);
	    zsfree(compisuffix);
	    compisuffix = ztrdup(oisuf);
	    zsfree(compqiprefix);
	    compqiprefix = ztrdup(oqipre);
	    zsfree(compqisuffix);
	    compqisuffix = ztrdup(oqisuf);
	    zsfree(compquote);
	    compquote = ztrdup(oq);
	    zsfree(compquoting);
	    compquoting = ztrdup(oqi);
	    freearray(compwords);
	    PERMALLOC {
		compwords = arrdup(owords);
	    } LASTALLOC;
	    comp_setunset(CP_COMPSTATE |
			  (~runset & (CP_WORDS | CP_CURRENT | CP_PREFIX |
				     CP_SUFFIX | CP_IPREFIX | CP_ISUFFIX |
				     CP_QIPREFIX | CP_QISUFFIX)),
			  (runset & CP_ALLREALS),
			  (~kunset & CP_RESTORE), (kunset & CP_ALLKEYS));
	} else
	    comp_setunset(CP_COMPSTATE, 0, (~kunset & CP_RESTORE),
			  (kunset & CP_RESTORE));
	zsfree(comprestore);
	comprestore = orest;

	return 0;
    }
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
static int
cond_psfix(char **a, int id)
{
    if (comp_check()) {
	if (a[1])
	    return do_comp_vars(id, cond_val(a, 0), cond_str(a, 1, 1),
				0, NULL, 0);
	else
	    return do_comp_vars(id, -1, cond_str(a, 0, 1), 0, NULL, 0);
    }
    return 0;
}

/**/
static int
cond_range(char **a, int id)
{
    return do_comp_vars(CVT_RANGEPAT, 0, cond_str(a, 0, 1), 0,
			(id ? cond_str(a, 1, 1) : NULL), 0);
}

/**/
static void
cmsetfn(Param pm, char **v)
{
    set_gmatcher(pm->nam, v);
}

/**/
static char **
cmgetfn(Param pm)
{
    int num;
    Cmlist p;
    char **ret, **q;

    for (num = 0, p = cmatcher; p; p = p->next, num++);

    ret = (char **) zhalloc((num + 1) * sizeof(char *));

    for (q = ret, p = cmatcher; p; p = p->next, q++)
	*q = dupstring(p->str);
    *q = NULL;

    return ret;
}

/**/
static void
cmunsetfn(Param pm, int exp)
{
    char *dummy[1];

    dummy[0] = NULL;
    set_gmatcher(pm->nam, dummy);
}

static struct builtin bintab[] = {
    BUILTIN("compadd", 0, bin_compadd, 0, -1, 0, NULL, NULL),
    BUILTIN("compset", 0, bin_compset, 1, 3, 0, NULL, NULL),
};

static struct conddef cotab[] = {
    CONDDEF("prefix", 0, cond_psfix, 1, 2, CVT_PREPAT),
    CONDDEF("suffix", 0, cond_psfix, 1, 2, CVT_SUFPAT),
    CONDDEF("between", 0, cond_range, 2, 2, 1),
    CONDDEF("after", 0, cond_range, 1, 1, 0),
};

static struct funcwrap wrapper[] = {
    WRAPDEF(comp_wrapper),
};

static struct paramdef patab[] = {
    PARAMDEF("compmatchers", PM_ARRAY|PM_SPECIAL, NULL, cmsetfn, cmgetfn, cmunsetfn)
};

/**/
int
setup_complete(Module m)
{
    makecompparamsptr = makecompparams;
    comp_setunsetptr = comp_setunset;

    return 0;
}

/**/
int
boot_complete(Module m)
{
    if (!(addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab)) |
	  addconddefs(m->nam, cotab, sizeof(cotab)/sizeof(*cotab)) |
	  addparamdefs(m->nam, patab, sizeof(patab)/sizeof(*patab)) |
	  !addwrapper(m, wrapper)))
	return 1;
    return 0;
}

#ifdef MODULE

/**/
int
cleanup_complete(Module m)
{
    deletebuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
    deleteconddefs(m->nam, cotab, sizeof(cotab)/sizeof(*cotab));
    deleteparamdefs(m->nam, patab, sizeof(patab)/sizeof(*patab));
    deletewrapper(m, wrapper);
    return 0;
}

/**/
int
finish_complete(Module m)
{
    makecompparamsptr = NULL;
    comp_setunsetptr = NULL;

    return 0;
}

#endif
