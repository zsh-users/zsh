/*
 * zutil.c - misc utilities
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

#include "zutil.mdh"
#include "zutil.pro"

/* Style stuff. */

typedef struct stypat *Stypat;
typedef struct style *Style;

/* A pattern and the styles for it. */

struct style {
    Style next;			/* next in stypat list */
    Stypat pats;		/* patterns */
    char *name;
};

struct stypat {
    Stypat next;
    char *pat;			/* pattern string */
    Patprog prog;		/* compiled pattern */
    int weight;			/* how specific is the pattern? */
    char **vals;
};
    
/* List of styles. */

static Style zstyles, zlstyles;

/* Memory stuff. */

static void
freestypat(Stypat p)
{
    zsfree(p->pat);
    freepatprog(p->prog);
    if (p->vals)
	freearray(p->vals);
    zfree(p, sizeof(*p));
}

static void
freeallstyles(void)
{
    Style s, sn;
    Stypat p, pn;

    for (s = zstyles; s; s = sn) {
	sn = s->next;
	for (p = s->pats; p; p = pn) {
	    pn = p->next;
	    freestypat(p);
	}
	zsfree(s->name);
	zfree(s, sizeof(*s));
    }
    zstyles = zlstyles = NULL;
}

/* Get the style struct for a name. */

static Style
getstyle(char *name)
{
    Style s;

    for (s = zstyles; s; s = s->next)
	if (!strcmp(name, s->name))
	    return s;

    return NULL;
}

/* Store a value for a style. */

static void
setstypat(Style s, char *pat, Patprog prog, char **vals)
{
    int weight, tmp, first;
    char *str;
    Stypat p, q, qq;

    for (p = s->pats; p; p = p->next)
	if (!strcmp(pat, p->pat)) {

	    /* Exists -> replace. */

	    if (p->vals)
		freearray(p->vals);
	    PERMALLOC {
		p->vals = arrdup(vals);
	    } LASTALLOC;

	    return;
	}

    /* New pattern. */

    p = (Stypat) zalloc(sizeof(*p));
    p->pat = ztrdup(pat);
    p->prog = prog;
    PERMALLOC {
	p->vals = arrdup(vals);
    } LASTALLOC;
    p->next = NULL;

    /* Calculate the weight. */

    for (weight = 0, tmp = 2, first = 1, str = pat; *str; str++) {
	if (first && *str == '*' && (!str[1] || str[1] == ':')) {
	    /* Only `*' in this component. */
	    tmp = 0;
	    continue;
	}
	first = 0;

	if (*str == '(' || *str == '|' || *str == '*' || *str == '[' ||
	    *str == '<' ||  *str == '?' || *str == '#' || *str == '^')
	    /* Is pattern. */
	    tmp = 1;

	if (*str == ':') {
	    /* Yet another component. */

	    first = 1;
	    weight += tmp;
	    tmp = 2;
	}
    }
    p->weight = weight + tmp;

    for (qq = NULL, q = s->pats; q && q->weight >= weight;
	 qq = q, q = q->next);

    p->next = q;
    if (qq)
	qq->next = p;
    else
	s->pats = p;
}

/* Add a new style. */

static Style
addstyle(char *name)
{
    Style s;

    s = (Style) zalloc(sizeof(*s));
    s->next = NULL;
    s->pats = NULL;
    s->name = ztrdup(name);

    if (zlstyles)
	zlstyles->next = s;
    else
	zstyles = s;
    zlstyles = s;

    return s;
}

/* Look up a style for a context pattern. This does the matching. */

static Stypat
lookupstyle(char *ctxt, char *style)
{
    Style s;
    Stypat p;

    for (s = zstyles; s; s = s->next)
	if (!strcmp(s->name, style))
	    for (p = s->pats; p; p = p->next)
		if (pattry(p->prog, ctxt))
		    return p;

    return NULL;
}

static int
bin_zstyle(char *nam, char **args, char *ops, int func)
{
    int min, max, n, add = 0, list = 0;

    if (!args[0])
	list = 1;
    else if (args[0][0] == '-') {
	char oc;

	if ((oc = args[0][1]) && oc != '-') {
	    if (args[0][2]) {
		zerrnam(nam, "invalid argument: %s", args[0], 0);
		return 1;
	    }
	    if (oc == 'L')
		list = 2;
	} else {
	    add = 1;
	    args++;
	}
    } else
	add = 1;

    if (add) {
	Style s;
	Patprog prog;
	char *pat;

	if (arrlen(args) < 2) {
	    zerrnam(nam, "not enough arguments", NULL, 0);
	    return 1;
	}
	pat = dupstring(args[0]);
	tokenize(pat);

	if (!(prog = patcompile(pat, PAT_ZDUP, NULL))) {
	    zerrnam(nam, "invalid pattern: %s", args[0], 0);
	    return 1;
	}
	if (!(s = getstyle(args[1])))
	    s = addstyle(args[1]);
	setstypat(s, args[0], prog, args + 2);

	return 0;
    }
    if (list) {
	Style s;
	Stypat p;
	char **v;

	for (s = zstyles; s; s = s->next) {
	    if (list == 1) {
		quotedzputs(s->name, stdout);
		putchar('\n');
	    }
	    for (p = s->pats; p; p = p->next) {
		if (list == 1)
		    printf("    %s", p->pat);
		else {
		    printf("zstyle ");
		    quotedzputs(p->pat, stdout);
		    printf(" %s", s->name);
		}
		for (v = p->vals; *v; v++) {
		    putchar(' ');
		    quotedzputs(*v, stdout);
		}
		putchar('\n');
	    }
	}
	return 0;
    }
    switch (args[0][1]) {
    case 'd': min = 0; max = -1; break;
    case 's': min = 3; max =  4; break;
    case 'b': min = 3; max =  3; break;
    case 'a': min = 3; max =  3; break;
    case 'h': min = 3; max =  3; break;
    case 't': min = 2; max = -1; break;
    case 'm': min = 3; max =  3; break;
    case 'g': min = 1; max =  3; break;
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
    case 'd':
	{
	    Style s;

	    if (args[1]) {
		if (args[2]) {
		    char *pat = args[1];

		    for (args += 2; *args; args++) {
			if ((s = getstyle(*args))) {
			    Stypat p, q;

			    for (q = NULL, p = s->pats; p;
				 q = p, p = p->next) {
				if (!strcmp(p->pat, pat)) {
				    if (q)
					q->next = p->next;
				    else
					s->pats = p->next;
				    freestypat(p);
				}
			    }
			}
		    }
		} else {
		    Stypat p, q;

		    for (s = zstyles; s; s = s->next) {
			for (q = NULL, p = s->pats; p; q = p, p = p->next) {
			    if (!strcmp(p->pat, args[1])) {
				if (q)
				    q->next = p->next;
				else
				    s->pats = p->next;
				freestypat(p);
			    }
			}
		    }
		}
	    } else
		freeallstyles();
	}
	break;
    case 's':
	{
	    Stypat s;
	    char *ret;
	    int val;

	    if ((s = lookupstyle(args[1], args[2])) && s->vals[0]) {
		PERMALLOC {
		    ret = sepjoin(s->vals, (args[4] ? args[4] : " "));
		} LASTALLOC;
		val = 0;
	    } else {
		ret = ztrdup("");
		val = 1;
	    }
	    setsparam(args[3], ret);

	    return val;
	}
	break;
    case 'b':
	{
	    Stypat s;
	    char *ret;
	    int val;

	    if ((s = lookupstyle(args[1], args[2])) &&
		s->vals[0] && !s->vals[1] &&
		(!strcmp(s->vals[0], "yes") ||
		 !strcmp(s->vals[0], "true") ||
		 !strcmp(s->vals[0], "on") ||
		 !strcmp(s->vals[0], "1"))) {
		ret = "yes";
		val = 0;
	    } else {
		ret = "no";
		val = 1;
	    }
	    setsparam(args[3], ztrdup(ret));

	    return val;
	}
	break;
    case 'a':
    case 'h':
	{
	    Stypat s;
	    char **ret;
	    int val;

	    if ((s = lookupstyle(args[1], args[2]))) {
		PERMALLOC {
		    ret = arrdup(s->vals);
		} LASTALLOC;
		val = 0;
	    } else {
		char *dummy = NULL;

		PERMALLOC {
		    ret = arrdup(&dummy);
		} LASTALLOC;
		val = 1;
	    }
	    if (args[0][1] == 'a')
		setaparam(args[3], ret);
	    else
		sethparam(args[3], ret);

	    return val;
	}
	break;
    case 't':
	{
	    Stypat s;

	    if ((s = lookupstyle(args[1], args[2])) && s->vals[0]) {
		if (args[3]) {
		    char **ap = args + 3, **p;

		    while (*ap) {
			p = s->vals;
			while (*p)
			    if (!strcmp(*ap, *p++))
				return 0;
			ap++;
		    }
		    return 1;
		} else
		    return !(!strcmp(s->vals[0], "true") ||
			     !strcmp(s->vals[0], "yes") ||
			     !strcmp(s->vals[0], "on") ||
			     !strcmp(s->vals[0], "1"));
	    }
	    return 1;
	}
	break;
    case 'm':
	{
	    Stypat s;
	    Patprog prog;

	    tokenize(args[3]);

	    if ((s = lookupstyle(args[1], args[2])) &&
		(prog = patcompile(args[3], PAT_STATIC, NULL))) {
		char **p = s->vals;

		while (*p)
		    if (pattry(prog, *p++))
			return 0;
	    }
	    return 1;
	}
	break;
    case 'g':
	{
	    LinkList l = newlinklist();
	    int ret = 1;
	    Style s;
	    Stypat p;

	    if (args[2]) {
		if (args[3]) {
		    if ((s = getstyle(args[3]))) {
			for (p = s->pats; p; p = p->next) {
			    if (!strcmp(args[2], p->pat)) {
				char **v = p->vals;

				while (*v)
				    addlinknode(l, *v++);

				ret = 0;
				break;
			    }
			}
		    }
		} else {
		    for (s = zstyles; s; s = s->next)
			for (p = s->pats; p; p = p->next)
			    if (!strcmp(args[2], p->pat)) {
				addlinknode(l, s->name);
				break;
			    }
		    ret = 0;
		}
	    } else {
		LinkNode n;

		for (s = zstyles; s; s = s->next)
		    for (p = s->pats; p; p = p->next) {
			for (n = firstnode(l); n; incnode(n))
			    if (!strcmp(p->pat, (char *) getdata(n)))
				break;
			if (!n)
			    addlinknode(l, p->pat);
		    }
		ret = 0;
	    }
	    set_list_array(args[1], l);

	    return ret;
	}
    }
    return 0;
}

/* Format stuff. */

static int
bin_zformat(char *nam, char **args, char *ops, int func)
{
    char opt;

    if (args[0][0] != '-' || !(opt = args[0][1]) || args[0][2]) {
	zerrnam(nam, "invalid argument: %s", args[0], 0);
	return 1;
    }
    args++;

    switch (opt) {
    case 'f':
	{
	    char **ap, *specs[256], *out, *s;
	    int olen, oused = 0;

	    memset(specs, 0, 256 * sizeof(char *));

	    for (ap = args + 2; *ap; ap++) {
		if (!ap[0][0] || ap[0][0] == '-' || ap[0][0] == '.' ||
		    (ap[0][0] >= '0' && ap[0][0] <= '9') ||
		    ap[0][1] != ':') {
		    zerrnam(nam, "invalid argument: %s", *ap, 0);
		    return 1;
		}
		specs[STOUC(ap[0][0])] = ap[0] + 2;
	    }
	    out = (char *) zhalloc(olen = 128);

	    for (s = args[1]; *s; s++) {
		if (*s == '%') {
		    int right, min = -1, max = -1, outl;
		    char *spec, *start = s;

		    if ((right = (*++s == '-')))
			s++;

		    if (*s >= '0' && *s <= '9') {
			for (min = 0; *s >= '0' && *s <= '9'; s++)
			    min = (min * 10) + (int) STOUC(*s) - '0';
		    }
		    if (*s == '.' && s[1] >= '0' && s[1] <= '9') {
			for (max = 0, s++; *s >= '0' && *s <= '9'; s++)
			    max = (max * 10) + (int) STOUC(*s) - '0';
		    }
		    if ((spec = specs[STOUC(*s)])) {
			int len;

			if ((len = strlen(spec)) > max && max >= 0)
			    len = max;
			outl = (min >= 0 ? (min > len ? min : len) : len);

			if (oused + outl >= olen) {
			    int nlen = olen + outl + 128;
			    char *tmp = (char *) zhalloc(nlen);

			    memcpy(tmp, out, olen);
			    olen = nlen;
			    out = tmp;
			}
			if (len >= outl) {
			    memcpy(out + oused, spec, outl);
			    oused += outl;
			} else {
			    int diff = outl - len;

			    if (right) {
				while (diff--)
				    out[oused++] = ' ';
				memcpy(out + oused, spec, len);
				oused += len;
			    } else {
				memcpy(out + oused, spec, len);
				oused += len;
				while (diff--)
				    out[oused++] = ' ';
			    }				
			}
		    } else {
			int len = s - start + 1;

			if (oused + len >= olen) {
			    int nlen = olen + len + 128;
			    char *tmp = (char *) zhalloc(nlen);

			    memcpy(tmp, out, olen);
			    olen = nlen;
			    out = tmp;
			}
			memcpy(out + oused, start, len);
			oused += len;
		    }
		} else {
		    if (oused + 1 >= olen) {
			char *tmp = (char *) zhalloc(olen << 1);

			memcpy(tmp, out, olen);
			olen <<= 1;
			out = tmp;
		    }
		    out[oused++] = *s;
		}
	    }
	    out[oused] = '\0';

	    setsparam(args[0], ztrdup(out));
	    return 0;
	}
	break;
    case 'a':
	{
	    char **ap, *cp;
	    int nbc = 0, colon = 0, pre = 0, suf = 0;

	    for (ap = args + 2; *ap; ap++) {
		for (nbc = 0, cp = *ap; *cp && *cp != ':'; cp++)
		    if (*cp == '\\' && cp[1])
			cp++, nbc++;
		if (*cp == ':' && cp[1]) {
		    int d;

		    colon++;
		    if ((d = cp - *ap - nbc) > pre)
			pre = d;
		    if ((d = strlen(cp + 1)) > suf)
			suf = d;
		}
	    }
	    {
		int sl = strlen(args[1]);
		VARARR(char, buf, pre + suf + sl + 1);
		char **ret, **rp, *copy, *cpp, oldc;

		ret = (char **) zalloc((arrlen(args + 2) + 1) *
				       sizeof(char *));

		memcpy(buf + pre, args[1], sl);
		suf = pre + sl;

		for (rp = ret, ap = args + 2; *ap; ap++) {
		    copy = dupstring(*ap);
		    for (cp = cpp = copy; *cp && *cp != ':'; cp++) {
			if (*cp == '\\' && cp[1])
			    cp++;
			*cpp++ = *cp;
		    }
		    oldc = *cpp;
		    *cpp = '\0';
		    if (((cpp == cp && oldc == ':') || *cp == ':') && cp[1]) {
			memset(buf, ' ', pre);
			memcpy(buf, copy, (cpp - copy));
			strcpy(buf + suf, cp + 1);
			*rp++ = ztrdup(buf);
		    } else
			*rp++ = ztrdup(copy);
		}
		*rp = NULL;

		setaparam(args[0], ret);
		return 0;
	    }
	}
	break;
    }
    zerrnam(nam, "invalid option: -%c", 0, opt);
    return 1;
}

/* Zregexparse stuff. */

typedef struct {
    int cutoff;
    char *pattern;
    Patprog patprog;
    char *guard;
    char *action;
    LinkList branches;
} RParseState;

typedef struct {
  RParseState *state;
  LinkList actions;
} RParseBranch;

typedef struct {
    LinkList nullacts;
    LinkList in;
    LinkList out;
} RParseResult;

static char **rparseargs;
static LinkList rparsestates;

static int rparsealt(RParseResult *result, jmp_buf *perr);

static void
connectstates(LinkList out, LinkList in)
{
    LinkNode outnode, innode, ln;

    for (outnode = firstnode(out); outnode; outnode = nextnode(outnode)) {
	RParseBranch *outbranch = getdata(outnode);

	for (innode = firstnode(in); innode; innode = nextnode(innode)) {
	    RParseBranch *inbranch = getdata(innode);
	    RParseBranch *br = ncalloc(sizeof(*br));

	    br->state = inbranch->state;
	    br->actions = newlinklist();
	    for (ln = firstnode(outbranch->actions); ln; ln = nextnode(ln))
		addlinknode(br->actions, getdata(ln));
	    for (ln = firstnode(inbranch->actions); ln; ln = nextnode(ln))
		addlinknode(br->actions, getdata(ln));
	    addlinknode(outbranch->state->branches, br);
	}
    }
}

static int
rparseelt(RParseResult *result, jmp_buf *perr)
{
    int l;
    char *s = *rparseargs;

    if (!s)
        return 1;

    switch (s[0]) {
    case '/': {
	RParseState *st;
	RParseBranch *br;
	char *pattern, *lookahead;
	int patternlen, lookaheadlen = 0;

	l = strlen(s);
	if (!((2 <= l && s[l - 1] == '/') ||
	      (3 <= l && s[l - 2] == '/' && (s[l - 1] == '+' ||
					     s[l - 1] == '-'))))
	    return 1;
	st = ncalloc(sizeof(*st));
	st->branches = newlinklist();
	st->cutoff = s[l - 1];
	if (s[l - 1] == '/') {
	    pattern = s + 1;
	    patternlen = l - 2;
	} else {
	    pattern = s + 1;
	    patternlen = l - 3;
	}
	rparseargs++;
	if ((s = *rparseargs) && s[0] == '%' &&
	   2 <= (l = strlen(s)) && s[l - 1] == '%') {
	    rparseargs++;
	    lookahead = s + 1;
	    lookaheadlen = l - 2;
	} else {
	    lookahead = NULL;
	}
	if (patternlen == 2 && !strncmp(pattern, "[]", 2))
	    st->pattern = NULL;
	else {
	    char *cp;
	    int l = patternlen + 12; /* (#b)((#B)...)...* */
	    if(lookahead)
	        l += lookaheadlen + 4; /* (#B)... */
	    cp = st->pattern = ncalloc(l);
	    strcpy(cp, "(#b)((#B)");
	    cp += 9;
	    strcpy(cp, pattern);
	    cp += patternlen;
	    strcpy(cp, ")");
	    cp += 1;
	    if (lookahead) {
		strcpy(cp, "(#B)");
		cp += 4;
		strcpy(cp, lookahead);
		cp += lookaheadlen;
	    }
	    strcpy(cp, "*");
	}
	st->patprog = NULL;
	if ((s = *rparseargs) && *s == '-') {
	    rparseargs++;
	    l = strlen(s);
	    st->guard = ncalloc(l);
	    memcpy(st->guard, s + 1, l - 1);
	    st->guard[l - 1] = '\0';
	} else
	    st->guard = NULL;
	if ((s = *rparseargs) && *s == ':') {
	    rparseargs++;
	    l = strlen(s);
	    st->action = ncalloc(l);
	    memcpy(st->action, s + 1, l - 1);
	    st->action[l - 1] = '\0';
	} else
	    st->action = NULL;
	result->nullacts = NULL;
	result->in = newlinklist();
	br = ncalloc(sizeof(*br));
	br->state = st;
	br->actions = newlinklist();
	addlinknode(result->in, br);
	result->out = newlinklist();
	br = ncalloc(sizeof(*br));
	br->state = st;
	br->actions = newlinklist();
	addlinknode(result->out, br);
	break;
    }
    case '(':
	if (s[1])
	    return 1;
	rparseargs++;
	if (rparsealt(result, perr))
	    longjmp(*perr, 2);
	s = *rparseargs;
	if (!s || s[0] != ')' || s[1] != '\0')
	    longjmp(*perr, 2);
	rparseargs++;
        break;
    default:
        return 1;
    }

    return 0;
}

static int
rparseclo(RParseResult *result, jmp_buf *perr)
{
    if (rparseelt(result, perr))
	return 1;

    if (*rparseargs && !strcmp(*rparseargs, "#")) {
	rparseargs++;
	while (*rparseargs && !strcmp(*rparseargs, "#"))
	    rparseargs++;

	connectstates(result->out, result->in);
	result->nullacts = newlinklist();
    }
    return 0;
}

static void
prependactions(LinkList acts, LinkList branches)
{
    LinkNode aln, bln;

    for (bln = firstnode(branches); bln; bln = nextnode(bln)) {
	RParseBranch *br = getdata(bln);

	for (aln = lastnode(acts); aln != (LinkNode)acts; aln = prevnode(aln))
	    pushnode(br->actions, getdata(aln));
    }
}

static void
appendactions(LinkList acts, LinkList branches)
{
    LinkNode aln, bln;
    for (bln = firstnode(branches); bln; bln = nextnode(bln)) {
	RParseBranch *br = getdata(bln);

	for (aln = firstnode(acts); aln; aln = nextnode(aln))
	    addlinknode(br->actions, getdata(aln));
    }
}

static int
rparseseq(RParseResult *result, jmp_buf *perr)
{
    int l;
    char *s;
    RParseResult sub;

    result->nullacts = newlinklist();
    result->in = newlinklist();
    result->out = newlinklist();

    while (1) {
	if ((s = *rparseargs) && s[0] == '{' && s[(l = strlen(s)) - 1] == '}') {
	    char *action = ncalloc(l - 1);
	    LinkNode ln;

	    rparseargs++;
	    memcpy(action, s + 1, l - 2);
	    action[l - 2] = '\0';
	    if (result->nullacts)
		addlinknode(result->nullacts, action);
	    for (ln = firstnode(result->out); ln; ln = nextnode(ln)) {
		RParseBranch *br = getdata(ln);
		addlinknode(br->actions, action);
	    }
	}
        else if (!rparseclo(&sub, perr)) {
	    connectstates(result->out, sub.in);

	    if (result->nullacts) {
		prependactions(result->nullacts, sub.in);
		insertlinklist(sub.in, lastnode(result->in), result->in);
	    }
	    if (sub.nullacts) {
		appendactions(sub.nullacts, result->out);
		insertlinklist(sub.out, lastnode(result->out), result->out);
	    } else
		result->out = sub.out;

	    if (result->nullacts && sub.nullacts)
		insertlinklist(sub.nullacts, lastnode(result->nullacts),
			       result->nullacts);
	    else
		result->nullacts = NULL;
	}
	else
	    break;
    }
    return 0;
}

static int
rparsealt(RParseResult *result, jmp_buf *perr)
{
    RParseResult sub;

    if (rparseseq(result, perr))
	return 1;

    while (*rparseargs && !strcmp(*rparseargs, "|")) {
	rparseargs++;
	if (rparseseq(&sub, perr))
	    longjmp(*perr, 2);
	if (!result->nullacts && sub.nullacts)
	    result->nullacts = sub.nullacts;

	insertlinklist(sub.in, lastnode(result->in), result->in);
	insertlinklist(sub.out, lastnode(result->out), result->out);
    }
    return 0;
}

static int
rmatch(RParseResult *sm, char *subj, char *var1, char *var2, int comp)
{
    LinkNode ln, lnn;
    LinkList nexts;
    LinkList nextslist;
    RParseBranch *br;
    RParseState *st = NULL;
    int point1 = 0, point2 = 0;

    setiparam(var1, point1);
    setiparam(var2, point2);

    if (!*subj) {
	if (sm->nullacts)
	    for (ln = firstnode(sm->nullacts); ln; ln = nextnode(ln)) {
	        char *action = getdata(ln);

		if (action)
		    execstring(action, 1, 0);
	    }
	return 0;
    }

    nextslist = newlinklist();
    nexts = sm->in;
    addlinknode(nextslist, nexts);
    do {
	char **savematch1, **savembegin1, **savemend1;
	char **savematch2, **savembegin2, **savemend2;

	PERMALLOC {
	    savematch1 = arrdup(getaparam("match"));
	    savembegin1 = arrdup(getaparam("mbegin"));
	    savemend1 = arrdup(getaparam("mend"));
	} LASTALLOC;

	for (ln = firstnode(nexts); ln; ln = nextnode(ln)) {
	    int i;
	    RParseState *next;

	    br = getdata(ln);
	    next = br->state;
	    if (next->pattern && !next->patprog) {
	        tokenize(next->pattern);
		if (!(next->patprog = patcompile(next->pattern, 0, NULL)))
		    return 2;
	    }
	    if (next->pattern && pattry(next->patprog, subj) &&
		(!next->guard || (execstring(next->guard, 1, 0), !lastval))) {
		LinkNode aln;
		char **mend = getaparam("mend");
		int len = atoi(mend[0]);

		for (i = len; i; i--)
		  if (*subj++ == Meta)
		    subj++;

		PERMALLOC {
		    savematch2 = arrdup(getaparam("match"));
		    savembegin2 = arrdup(getaparam("mbegin"));
		    savemend2 = arrdup(getaparam("mend"));
		} LASTALLOC;

		if (savematch1)
		    setaparam("match", savematch1);
		if (savembegin1)
		    setaparam("mbegin", savembegin1);
		if (savemend1)
		    setaparam("mend", savemend1);

		for (aln = firstnode(br->actions); aln; aln = nextnode(aln)) {
		    char *action = getdata(aln);

		    if (action)
			execstring(action, 1, 0);
		}
		if (savematch2)
		    setaparam("match", savematch2);
		if (savembegin2)
		    setaparam("mbegin", savembegin2);
		if (savemend2)
		    setaparam("mend", savemend2);

		point2 += len;
		setiparam(var2, point2);
		st = br->state;
		nexts = st->branches;
		if (next->cutoff == '-' || (next->cutoff == '/' && len)) {
		    nextslist = newlinklist();
		    point1 = point2;
		    setiparam(var1, point1);
		}
		addlinknode(nextslist, nexts);
		break;
	    }
	}
	if (!ln) {
	    if (savematch1)
		freearray(savematch1);
	    if (savembegin1)
		freearray(savembegin1);
	    if (savemend1)
		freearray(savemend1);
	}
    } while (ln);

    if (!comp && !*subj)
	for (ln = firstnode(sm->out); ln; ln = nextnode(ln)) {
	    br = getdata(ln);
	    if (br->state == st) {
		for (ln = firstnode(br->actions); ln; ln = nextnode(ln)) {
		    char *action = getdata(ln);

		    if (action)
			execstring(action, 1, 0);
		}
		return 0;
	    }
	}

    for (lnn = firstnode(nextslist); lnn; lnn = nextnode(lnn)) {
	nexts = getdata(lnn);
	for (ln = firstnode(nexts); ln; ln = nextnode(ln)) {
	    br = getdata(ln);
	    if (br->state->action)
		execstring(br->state->action, 1, 0);
	}
    }
    return empty(nexts) ? 2 : 1;
}

/*
  usage: zregexparse string regex...
  status:
    0: matched
    1: unmatched (all next state candidates are failed)
    2: unmatched (there is no next state candidates)
    3: regex parse error
*/

static int
bin_zregexparse(char *nam, char **args, char *ops, int func)
{
    int oldextendedglob = opts[EXTENDEDGLOB];
    char *var1 = args[0];
    char *var2 = args[1];
    char *subj = args[2];
    int ret;
    jmp_buf rparseerr;
    RParseResult result;

    opts[EXTENDEDGLOB] = 1;

    rparseargs = args + 3;
    HEAPALLOC {
	pushheap();
        rparsestates = newlinklist();
	if (setjmp(rparseerr) || rparsealt(&result, &rparseerr) || *rparseargs) {
	    if (*rparseargs)
		zwarnnam(nam, "invalid regex : %s", *rparseargs, 0);
	    else
		zwarnnam(nam, "not enough regex arguments", NULL, 0);
	    ret = 3;
	} else
	    ret = 0;

	if (!ret)
	    ret = rmatch(&result, subj, var1, var2, ops['c']);
        popheap();
    } LASTALLOC;

    opts[EXTENDEDGLOB] = oldextendedglob;
    return ret;
}

static struct builtin bintab[] = {
    BUILTIN("zstyle", 0, bin_zstyle, 0, -1, 0, NULL, NULL),
    BUILTIN("zformat", 0, bin_zformat, 3, -1, 0, NULL, NULL),
    BUILTIN("zregexparse", 0, bin_zregexparse, 3, -1, 0, "c", NULL),
};


/**/
int
setup_(Module m)
{
    zstyles = zlstyles = NULL;

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
    freeallstyles();

    return 0;
}
