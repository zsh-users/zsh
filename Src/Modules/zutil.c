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

struct stypat {
    Stypat next;
    char *pat;			/* pattern string */
    Patprog prog;		/* compiled pattern */
    int weight;			/* how specific is the pattern? */
    Style styles, lstyles;	/* first/last style */
};
    
struct style {
    Style next;
    char *name;
    char **vals;
};

/* List of styles. */

static Stypat zstyles, lzstyles;

/* Memory stuff. */

static void
freestyle(Style s)
{
    Style n;

    while (s) {
	n = s->next;

	zsfree(s->name);
	if (s->vals)
	    freearray(s->vals);
	zfree(s, sizeof(*s));

	s = n;
    }
}

static void
freestypat(Stypat p)
{
    Stypat n;

    while (p) {
	n = p->next;

	zsfree(p->pat);
	freepatprog(p->prog);
	zfree(p, sizeof(*p));

	p = n;
    }
}

/* Get the struct for a pattern, if any. */

static Stypat
getstypat(char *pat)
{
    Stypat p;

    for (p = zstyles; p; p = p->next)
	if (!strcmp(pat, p->pat))
	    return p;

    return NULL;
}

/* Get the style stuff for a name. */

static Style
getstyle(Stypat p, char *name)
{
    Style s;

    for (s = p->styles; s; s=  s->next)
	if (!strcmp(name, s->name))
	    return s;

    return NULL;
}

/* Store a value for a style. */

static void
setstyle(Stypat p, char *name, char **vals)
{
    Style s;

    for (s = p->styles; s; s = s->next)
	if (!strcmp(name, s->name)) {

	    /* Exists -> replace. */

	    if (s->vals)
		freearray(s->vals);
	    PERMALLOC {
		s->vals = arrdup(vals);
	    } LASTALLOC;

	    return;
	}

    /* New style. */

    s = (Style) zalloc(sizeof(*s));

    s->name = ztrdup(name);
    PERMALLOC {
	s->vals = arrdup(vals);
    } LASTALLOC;
    s->next = NULL;

    if (p->lstyles)
	p->lstyles->next = s;
    else
	p->styles = s;
    p->lstyles = s;
}

/* Add a new pattern. */

static Stypat
addstypat(char *pat, Patprog prog)
{
    Stypat p, q, qq;
    int weight, tmp, first;
    char *s;

    /* Calculate the weight. */

    for (weight = 0, tmp = 2, first = 1, s = pat; *s; s++) {
	if (first && *s == '*' && (!s[1] || s[1] == ':')) {
	    /* Only `*' in this component. */
	    tmp = 0;
	    continue;
	}
	first = 0;

	if (*s == '(' || *s == '|' || *s == '*' || *s == '[' || *s == '<' ||
	    *s == '?' || *s == '#' || *s == '^')
	    /* Is pattern. */
	    tmp = 1;

	if (*s == ':') {
	    /* Yet another component. */

	    first = 1;
	    weight += tmp;
	    tmp = 2;
	}
    }
    weight += tmp;

    p = (Stypat) zalloc(sizeof(*p));

    p->pat = ztrdup(pat);
    p->weight = weight;
    p->prog = prog;
    p->styles = p->lstyles = NULL;

    for (qq = NULL, q = zstyles; q && q->weight >= weight;
	 qq = q, q = q->next);

    p->next = q;
    if (qq)
	qq->next = p;
    else
	zstyles = p;
    if (!q)
	lzstyles = p;

    return p;
}

/* Delete a style. */

static void
deletestyle(Stypat p, char *name)
{
    Style ps, s;

    for (ps = NULL, s = p->styles; s; ps = s, s = s->next)
	if (!strcmp(name, s->name)) {
	    if (ps)
		ps->next = s->next;
	    else
		p->styles = s->next;
	    if (s == p->lstyles)
		p->lstyles = ps;

	    s->next = NULL;
	    freestyle(s);

	    return;
	}
}

/* Delete a whole pattern with all its styles. */

static void
deletestypat(Stypat pat)
{
    Stypat pp, p;

    for (pp = NULL, p = zstyles; p; pp = p, p = p->next)
	if (p == pat) {
	    if (pp)
		pp->next = p->next;
	    else
		zstyles = p->next;
	    if (p == lzstyles)
		lzstyles = pp;

	    p->next = NULL;
	    zsfree(p->pat);
	    freepatprog(p->prog);
	    freestyle(p->styles);
	    zfree(p, sizeof(*p));

	    return;
	}
}

/* Look up a style for a context pattern. This does the matching. */

static Style
lookupstyle(char *ctxt, char *style)
{
    Stypat p;
    Style s;

    for (p = zstyles; p; p = p->next)
	if (pattry(p->prog, ctxt))
	    for (s = p->styles; s; s = s->next)
		if (!strcmp(style, s->name))
		    return s;

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
	Stypat p;

	if (arrlen(args) < 2) {
	    zerrnam(nam, "not enough arguments", NULL, 0);
	    return 1;
	}
	if (!(p = getstypat(args[0]))) {
	    Patprog prog;
	    char *pat = dupstring(args[0]);

	    tokenize(pat);

	    if (!(prog = patcompile(pat, PAT_ZDUP, NULL))) {
		zerrnam(nam, "invalid pattern: %s", args[0], 0);
		return 1;
	    }
	    p = addstypat(args[0], prog);
	}
	setstyle(p, args[1], args + 2);

	return 0;
    }
    if (list) {
	Stypat p;
	Style s;
	char **v;

	for (p = zstyles; p; p = p->next) {
	    if (list == 1) {
		quotedzputs(p->pat, stdout);
		putchar('\n');
	    }
	    for (s = p->styles; s; s = s->next) {
		if (list == 1)
		    printf("    %s", s->name);
		else {
		    printf("zstyle ");
		    quotedzputs(p->pat, stdout);
		    printf(" %s", s->name);
		}
		for (v = s->vals; *v; v++) {
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
	    Stypat p;

	    if (args[1]) {
		if ((p = getstypat(args[1]))) {
		    if (args[2]) {
			char **ap = args + 2;

			while (*ap)
			    deletestyle(p, *ap++);

			if (!p->styles)
			    deletestypat(p);
		    } else
			deletestypat(p);
		}
	    } else {
		freestypat(zstyles);
		zstyles = lzstyles = NULL;
	    }
	}
	break;
    case 's':
	{
	    Style s;
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
	    Style s;
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
	    Style s;
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
	    Style s;

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
	    Style s;
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
	    Stypat p;

	    if (args[2]) {
		if ((p = getstypat(args[2]))) {
		    Style s;

		    if (args[3]) {
			if ((s = getstyle(p, args[3]))) {
			    char **v = s->vals;

			    while (*v)
				addlinknode(l, *v++);

			    ret = 0;
			}
		    } else {
			for (s = p->styles; s; s = s->next)
			    addlinknode(l, s->name);

			ret = 0;
		    }
		}
	    } else {
		for (p = zstyles; p; p = p->next)
		    addlinknode(l, p->pat);

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

static struct builtin bintab[] = {
    BUILTIN("zstyle", 0, bin_zstyle, 0, -1, 0, NULL, NULL),
    BUILTIN("zformat", 0, bin_zformat, 3, -1, 0, NULL, NULL),
};


/**/
int
setup_zutil(Module m)
{
    zstyles = NULL;

    return 0;
}

/**/
int
boot_zutil(Module m)
{
    return !addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
}

/**/
int
cleanup_zutil(Module m)
{
    deletebuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
    return 0;
}

/**/
int
finish_zutil(Module m)
{
    freestypat(zstyles);

    return 0;
}
