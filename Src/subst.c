/*
 * subst.c - various substitutions
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

#include "zsh.mdh"
#include "subst.pro"

/**/
char nulstring[] = {Nularg, '\0'};

/* Do substitutions before fork. These are:
 *  - Process substitution: <(...), >(...), =(...)
 *  - Parameter substitution
 *  - Command substitution
 * Followed by
 *  - Quote removal
 *  - Brace expansion
 *  - Tilde and equals substitution
 *
 * PF_* flags are defined in zsh.h
 */

/**/
mod_export void
prefork(LinkList list, int flags)
{
    LinkNode node, stop = 0;
    int keep = 0, asssub = (flags & PF_TYPESET) && isset(KSHTYPESET);

    queue_signals();
    for (node = firstnode(list); node; incnode(node)) {
	char *str, c;

	str = (char *)getdata(node);
	if (((c = *str) == Inang || c == Outang || c == Equals) &&
	    str[1] == Inpar) {
	    if (c == Inang || c == Outang)
		setdata(node, (void *) getproc(str));	/* <(...) or >(...) */
	    else
		setdata(node, (void *) getoutputfile(str));	/* =(...) */
	    if (!getdata(node)) {
		unqueue_signals();
		return;
	    }
	} else {
	    if (isset(SHFILEEXPANSION))
		filesub((char **)getaddrdata(node),
			flags & (PF_TYPESET|PF_ASSIGN));
	    if (!(node = stringsubst(list, node, flags & PF_SINGLE, asssub))) {
		unqueue_signals();
		return;
	    }
	}
    }
    for (node = firstnode(list); node; incnode(node)) {
	if (node == stop)
	    keep = 0;
	if (*(char *)getdata(node)) {
	    remnulargs(getdata(node));
	    if (unset(IGNOREBRACES) && !(flags & PF_SINGLE)) {
		if (!keep)
		    stop = nextnode(node);
		while (hasbraces(getdata(node))) {
		    keep = 1;
		    xpandbraces(list, &node);
		}
	    }
	    if (unset(SHFILEEXPANSION))
		filesub((char **)getaddrdata(node),
			flags & (PF_TYPESET|PF_ASSIGN));
	} else if (!(flags & PF_SINGLE) && !keep)
	    uremnode(list, node);
	if (errflag) {
	    unqueue_signals();
	    return;
	}
    }
    unqueue_signals();
}

/**/
static LinkNode
stringsubst(LinkList list, LinkNode node, int ssub, int asssub)
{
    int qt;
    char *str3 = (char *)getdata(node);
    char *str  = str3, c;

    while (!errflag && (c = *str)) {
	if ((qt = c == Qstring) || c == String) {
	    if ((c = str[1]) == Inpar) {
		if (!qt)
		    mult_isarr = 1;
		str++;
		goto comsub;
	    } else if (c == Inbrack) {
		/* $[...] */
		char *str2 = str;
		str2++;
		if (skipparens(Inbrack, Outbrack, &str2)) {
		    zerr("closing bracket missing", NULL, 0);
		    return NULL;
		}
		str2[-1] = *str = '\0';
		str = arithsubst(str + 2, &str3, str2);
		setdata(node, (void *) str3);
		continue;
	    } else if (c == Snull) {
		str = getkeystring(str, NULL, 4, NULL);
		continue;
	    } else {
		node = paramsubst(list, node, &str, qt, ssub);
		if (errflag || !node)
		    return NULL;
		str3 = (char *)getdata(node);
		continue;
	    }
	} else if ((qt = c == Qtick) || (c == Tick ? (mult_isarr = 1) : 0))
	  comsub: {
	    LinkList pl;
	    char *s, *str2 = str;
	    char endchar;
	    int l1, l2;

	    if (c == Inpar) {
		endchar = Outpar;
		str[-1] = '\0';
#ifdef DEBUG
		if (skipparens(Inpar, Outpar, &str))
		    dputs("BUG: parse error in command substitution");
#else
		skipparens(Inpar, Outpar, &str);
#endif
		str--;
	    } else {
		endchar = c;
		*str = '\0';

		while (*++str != endchar)
		    DPUTS(!*str, "BUG: parse error in command substitution");
	    }
	    *str++ = '\0';
	    if (endchar == Outpar && str2[1] == '(' && str[-2] == ')') {
		/* Math substitution of the form $((...)) */
		str[-2] = '\0';
		str = arithsubst(str2 + 2, &str3, str);
		setdata(node, (void *) str3);
		continue;
	    }

	    /* It is a command substitution, which will be parsed again   *
	     * by the lexer, so we untokenize it first, but we cannot use *
	     * untokenize() since in the case of `...` some Bnulls should *
	     * be left unchanged.  Note that the lexer doesn't tokenize   *
	     * the body of a command substitution so if there are some    *
	     * tokens here they are from a ${(e)~...} substitution.       */
	    for (str = str2; (c = *++str); )
		if (itok(c) && c != Nularg &&
		    !(endchar != Outpar && c == Bnull &&
		      (str[1] == '$' || str[1] == '\\' || str[1] == '`' ||
		       (qt && str[1] == '"'))))
		    *str = ztokens[c - Pound];
	    str++;
	    if (!(pl = getoutput(str2 + 1, qt || ssub))) {
		zerr("parse error in command substitution", NULL, 0);
		return NULL;
	    }
	    if (endchar == Outpar)
		str2--;
	    if (!(s = (char *) ugetnode(pl))) {
		str = strcpy(str2, str);
		continue;
	    }
	    if (!qt && ssub && isset(GLOBSUBST))
		shtokenize(s);
	    l1 = str2 - str3;
	    l2 = strlen(s);
	    if (nonempty(pl)) {
		LinkNode n = lastnode(pl);
		str2 = (char *) hcalloc(l1 + l2 + 1);
		strcpy(str2, str3);
		strcpy(str2 + l1, s);
		setdata(node, str2);
		insertlinklist(pl, node, list);
		s = (char *) getdata(node = n);
		l1 = 0;
		l2 = strlen(s);
	    }
	    str2 = (char *) hcalloc(l1 + l2 + strlen(str) + 1);
	    if (l1)
		strcpy(str2, str3);
	    strcpy(str2 + l1, s);
	    str = strcpy(str2 + l1 + l2, str);
	    str3 = str2;
	    setdata(node, str3);
	    continue;
	} else if (asssub && ((c == '=') || c == Equals) && str != str3) {
	    /*
	     * We are in a normal argument which looks like an assignment
	     * and is to be treated like one, with no word splitting.
	     */
	    ssub = 1;
	}
	str++;
    }
    return errflag ? NULL : node;
}

/*
 * Simplified version of the prefork/singsub processing where
 * we only do substitutions appropriate to quoting.  Currently
 * this means only the expansions in $'....'.  This is used
 * for the end tag for here documents.  As we are not doing
 * `...` expansions, we just use those for quoting.  However,
 * they stay in the text.  This is weird, but that's not
 * my fault.
 *
 * The remnulargs() makes this consistent with the other forms
 * of substitution, indicating that quotes have been fully
 * processed.
 */

/**/
void
quotesubst(char *str)
{
    char *s = str;

    while (*s) {
	if (*s == String && s[1] == Snull) {
	    s = getkeystring(s, NULL, 4, NULL);
	} else {
	    s++;
	}
    }
    remnulargs(str);
}

/**/
mod_export void
globlist(LinkList list, int nountok)
{
    LinkNode node, next;

    badcshglob = 0;
    for (node = firstnode(list); !errflag && node; node = next) {
	next = nextnode(node);
	zglob(list, node, nountok);
    }
    if (badcshglob == 1)
	zerr("no match", NULL, 0);
}

/* perform substitution on a single word */

/**/
mod_export void
singsub(char **s)
{
    int omi = mult_isarr;
    local_list1(foo);

    init_list1(foo, *s);

    prefork(&foo, PF_SINGLE);
    mult_isarr = omi;
    if (errflag)
	return;
    *s = (char *) ugetnode(&foo);
    DPUTS(nonempty(&foo), "BUG: singsub() produced more than one word!");
}

/* Perform substitution on a single word. Unlike with singsub, the      *
 * result can have more than one word. A single word result is stored   *
 * in *s and *isarr is set to zero; otherwise *isarr is set to 1 and    *
 * the result is stored in *a. If `a' is zero a multiple word result is *
 * joined using sep or the IFS parameter if sep is zero and the result  *
 * is returned in *s.  The return value is true iff the expansion       *
 * resulted in an empty list.                                           *
 * The mult_isarr variable is used by paramsubst() to tell if it yields *
 * an array.                                                            */

/**/
static int mult_isarr;

/**/
static int
multsub(char **s, char ***a, int *isarr, UNUSED(char *sep))
{
    int l, omi = mult_isarr;
    char **r, **p;
    local_list1(foo);

    mult_isarr = 0;
    init_list1(foo, *s);
    prefork(&foo, 0);
    if (errflag) {
	if (isarr)
	    *isarr = 0;
	mult_isarr = omi;
	return 0;
    }
    if ((l = countlinknodes(&foo))) {
	p = r = hcalloc((l + 1) * sizeof(char*));
	while (nonempty(&foo))
	    *p++ = (char *)ugetnode(&foo);
	*p = NULL;
	/*
	 * This is the most obscure way of deciding whether a value is
	 * an array it would be possible to imagine.  It seems to result
	 * partly because we don't pass down the qt and ssub flags from
	 * paramsubst() through prefork() properly, partly because we
	 * don't tidy up to get back the return type from multsub we
	 * need properly.  The crux of neatening this up is to get rid
	 * of the following test.
	 */
	if (a && mult_isarr) {
	    *a = r;
	    *isarr = SCANPM_MATCHMANY;
	    mult_isarr = omi;
	    return 0;
	}
	*s = sepjoin(r, NULL, 1);
	mult_isarr = omi;
	if (isarr)
	    *isarr = 0;
	return 0;
    }
    if (l)
	*s = (char *) ugetnode(&foo);
    else
	*s = dupstring("");
    if (isarr)
	*isarr = 0;
    mult_isarr = omi;
    return !l;
}

/*
 * ~, = subs: assign & PF_TYPESET => typeset or magic equals
 *            assign & PF_ASSIGN => normal assignment
 */

/**/
mod_export void
filesub(char **namptr, int assign)
{
    char *eql = NULL, *sub = NULL, *str, *ptr;
    int len;

    filesubstr(namptr, assign);

    if (!assign)
	return;

    if (assign & PF_TYPESET) {
	if ((*namptr)[1] && (eql = sub = strchr(*namptr + 1, Equals))) {
	    str = sub + 1;
	    if ((sub[1] == Tilde || sub[1] == Equals) && filesubstr(&str, assign)) {
		sub[1] = '\0';
		*namptr = dyncat(*namptr, str);
	    }
	} else
	    return;
    }

    ptr = *namptr;
    while ((sub = strchr(ptr, ':'))) {
	str = sub + 1;
	len = sub - *namptr;
	if (sub > eql &&
	    (sub[1] == Tilde || sub[1] == Equals) &&
	    filesubstr(&str, assign)) {
	    sub[1] = '\0';
	    *namptr = dyncat(*namptr, str);
	}
	ptr = *namptr + len + 1;
    }
}

/**/
mod_export int
filesubstr(char **namptr, int assign)
{
#define isend(c) ( !(c) || (c)=='/' || (c)==Inpar || (assign && (c)==':') )
#define isend2(c) ( !(c) || (c)==Inpar || (assign && (c)==':') )
    char *str = *namptr;

    if (*str == Tilde && str[1] != '=' && str[1] != Equals) {
	char *ptr;
	int val;

	val = zstrtol(str + 1, &ptr, 10);
	if (isend(str[1])) {   /* ~ */
	    *namptr = dyncat(home, str + 1);
	    return 1;
	} else if (str[1] == '+' && isend(str[2])) {   /* ~+ */
	    *namptr = dyncat(pwd, str + 2);
	    return 1;
	} else if (str[1] == '-' && isend(str[2])) {   /* ~- */
	    char *tmp;
	    *namptr = dyncat((tmp = oldpwd) ? tmp : pwd, str + 2);
	    return 1;
	} else if (!inblank(str[1]) && isend(*ptr) &&
		   (!idigit(str[1]) || (ptr - str < 4))) {
	    char *ds;

	    if (val < 0)
		val = -val;
	    ds = dstackent(str[1], val);
	    if (!ds)
		return 0;
	    *namptr = dyncat(ds, ptr);
	    return 1;
	} else if (iuser(str[1])) {   /* ~foo */
	    char *ptr, *hom, save;

	    for (ptr = ++str; *ptr && iuser(*ptr); ptr++);
	    save = *ptr;
	    if (!isend(save))
		return 0;
	    *ptr = 0;
	    if (!(hom = getnameddir(str))) {
		if (isset(NOMATCH))
		    zerr("no such user or named directory: %s", str, 0);
		*ptr = save;
		return 0;
	    }
	    *ptr = save;
	    *namptr = dyncat(hom, ptr);
	    return 1;
	}
    } else if (*str == Equals && isset(EQUALS) && str[1]) {   /* =foo */
	char sav, *pp, *cnam;

	for (pp = str + 1; !isend2(*pp); pp++);
	sav = *pp;
	*pp = 0;
	if (!(cnam = findcmd(str + 1, 1))) {
	    if (isset(NOMATCH))
		zerr("%s not found", str + 1, 0);
	    return 0;
	}
	*namptr = dupstring(cnam);
	if (sav) {
	    *pp = sav;
	    *namptr = dyncat(*namptr, pp);
	}
	return 1;
    }
    return 0;
#undef isend
#undef isend2
}

/**/
static char *
strcatsub(char **d, char *pb, char *pe, char *src, int l, char *s, int glbsub,
	  int copied)
{
    char *dest;
    int pl = pe - pb;

    if (!pl && (!s || !*s)) {
	*d = dest = (copied ? src : dupstring(src));
	if (glbsub)
	    shtokenize(dest);
    } else {
	*d = dest = hcalloc(pl + l + (s ? strlen(s) : 0) + 1);
	strncpy(dest, pb, pl);
	dest += pl;
	strcpy(dest, src);
	if (glbsub)
	    shtokenize(dest);
	dest += l;
	if (s)
	    strcpy(dest, s);
    }
    return dest;
}

typedef int (*CompareFn) _((const void *, const void *));

/**/
int
strpcmp(const void *a, const void *b)
{
#ifdef HAVE_STRCOLL
    return strcoll(*(char **)a, *(char **)b);
#else
    return strcmp(*(char **)a, *(char **)b);
#endif
}

/**/
int
invstrpcmp(const void *a, const void *b)
{
#ifdef HAVE_STRCOLL
    return -strcoll(*(char **)a, *(char **)b);
#else
    return -strcmp(*(char **)a, *(char **)b);
#endif
}

/**/
int
cstrpcmp(const void *a, const void *b)
{
#ifdef HAVE_STRCOLL
    VARARR(char, c, strlen(*(char **) a) + 1);
    VARARR(char, d, strlen(*(char **) b) + 1);
    char *s, *t;
    int   cmp;

    for (s = *(char **) a, t = c; (*t++ = tulower(*s++)););
    for (s = *(char **) b, t = d; (*t++ = tulower(*s++)););

    cmp = strcoll(c, d);

    return cmp;
#else
    char *c = *(char **)a, *d = *(char **)b;

    for (; *c && tulower(*c) == tulower(*d); c++, d++);

    return (int)STOUC(tulower(*c)) - (int)STOUC(tulower(*d));
#endif
}

/**/
int
invcstrpcmp(const void *a, const void *b)
{
#ifdef HAVE_STRCOLL
    VARARR(char, c, strlen(*(char **) a) + 1);
    VARARR(char, d, strlen(*(char **) b) + 1);
    char *s, *t;
    int   cmp;

    for (s = *(char **) a, t = c; (*t++ = tulower(*s++)););
    for (s = *(char **) b, t = d; (*t++ = tulower(*s++)););

    cmp = strcoll(c, d);

    return -cmp;
#else
    char *c = *(char **)a, *d = *(char **)b;

    for (; *c && tulower(*c) == tulower(*d); c++, d++);

    return (int)STOUC(tulower(*d)) - (int)STOUC(tulower(*c));
#endif
}

/**/
int
nstrpcmp(const void *a, const void *b)
{
    char *c = *(char **)a, *d = *(char **)b;
    int cmp;

#ifdef HAVE_STRCOLL
    cmp = strcoll(c, d);
#endif
    for (; *c == *d && *c; c++, d++);
#ifndef HAVE_STRCOLL
    cmp = (int)STOUC(*c) - (int)STOUC(*d);
#endif
    if (idigit(*c) || idigit(*d)) {
	for (; c > *(char **)a && idigit(c[-1]); c--, d--);
	if (idigit(*c) && idigit(*d)) {
	    while (*c == '0')
		c++;
	    while (*d == '0')
		d++;
	    for (; idigit(*c) && *c == *d; c++, d++);
	    if (idigit(*c) || idigit(*d)) {
		cmp = (int)STOUC(*c) - (int)STOUC(*d);
		while (idigit(*c) && idigit(*d))
		    c++, d++;
		if (idigit(*c) && !idigit(*d))
		    return 1;
		if (idigit(*d) && !idigit(*c))
		    return -1;
	    }
	}
    }
    return cmp;
}

/**/
int
invnstrpcmp(const void *a, const void *b)
{
    return -nstrpcmp(a, b);
}

/**/
int
instrpcmp(const void *a, const void *b)
{
    VARARR(char, c, strlen(*(char **) a) + 1);
    VARARR(char, d, strlen(*(char **) b) + 1);
    char **e = (char **)&c;
    char **f = (char **)&d;
    char *s, *t;

    for (s = *(char **) a, t = c; (*t++ = tulower(*s++)););
    for (s = *(char **) b, t = d; (*t++ = tulower(*s++)););

    return nstrpcmp(&e, &f);
}

/**/
int
invinstrpcmp(const void *a, const void *b)
{
    return -instrpcmp(a, b);
}

/**/
static char *
dopadding(char *str, int prenum, int postnum, char *preone, char *postone, char *premul, char *postmul)
{
    char def[3], *ret, *t, *r;
    int ls, ls2, lpreone, lpostone, lpremul, lpostmul, lr, f, m, c, cc;

    def[0] = *ifs ? *ifs : ' ';
    def[1] = *ifs == Meta ? ifs[1] ^ 32 : '\0';
    def[2] = '\0';
    if (preone && !*preone)
	preone = def;
    if (postone && !*postone)
	postone = def;
    if (!premul || !*premul)
	premul = def;
    if (!postmul || !*postmul)
	postmul = def;

    ls = strlen(str);
    lpreone = preone ? strlen(preone) : 0;
    lpostone = postone ? strlen(postone) : 0;
    lpremul = strlen(premul);
    lpostmul = strlen(postmul);

    lr = prenum + postnum;

    if (lr == ls)
	return str;

    r = ret = (char *)zhalloc(lr + 1);

    if (prenum) {
	if (postnum) {
	    ls2 = ls / 2;

	    f = prenum - ls2;
	    if (f <= 0)
		for (str -= f, c = prenum; c--; *r++ = *str++);
	    else {
		if (f <= lpreone)
		    for (c = f, t = preone + lpreone - f; c--; *r++ = *t++);
		else {
		    f -= lpreone;
		    if ((m = f % lpremul))
			for (c = m, t = premul + lpremul - m; c--; *r++ = *t++);
		    for (cc = f / lpremul; cc--;)
			for (c = lpremul, t = premul; c--; *r++ = *t++);
		    for (c = lpreone; c--; *r++ = *preone++);
		}
		for (c = ls2; c--; *r++ = *str++);
	    }
	    ls2 = ls - ls2;
	    f = postnum - ls2;
	    if (f <= 0)
		for (c = postnum; c--; *r++ = *str++);
	    else {
		for (c = ls2; c--; *r++ = *str++);
		if (f <= lpostone)
		    for (c = f; c--; *r++ = *postone++);
		else {
		    f -= lpostone;
		    for (c = lpostone; c--; *r++ = *postone++);
		    for (cc = f / lpostmul; cc--;)
			for (c = lpostmul, t = postmul; c--; *r++ = *t++);
		    if ((m = f % lpostmul))
			for (; m--; *r++ = *postmul++);
		}
	    }
	} else {
	    f = prenum - ls;
	    if (f <= 0)
		for (c = prenum, str -= f; c--; *r++ = *str++);
	    else {
		if (f <= lpreone)
		    for (c = f, t = preone + lpreone - f; c--; *r++ = *t++);
		else {
		    f -= lpreone;
		    if ((m = f % lpremul))
			for (c = m, t = premul + lpremul - m; c--; *r++ = *t++);
		    for (cc = f / lpremul; cc--;)
			for (c = lpremul, t = premul; c--; *r++ = *t++);
		    for (c = lpreone; c--; *r++ = *preone++);
		}
		for (c = ls; c--; *r++ = *str++);
	    }
	}
    } else if (postnum) {
	f = postnum - ls;
	if (f <= 0)
	    for (c = postnum; c--; *r++ = *str++);
	else {
	    for (c = ls; c--; *r++ = *str++);
	    if (f <= lpostone)
		for (c = f; c--; *r++ = *postone++);
	    else {
		f -= lpostone;
		for (c = lpostone; c--; *r++ = *postone++);
		for (cc = f / lpostmul; cc--;)
		    for (c = lpostmul, t = postmul; c--; *r++ = *t++);
		if ((m = f % lpostmul))
		    for (; m--; *r++ = *postmul++);
	    }
	}
    }
    *r = '\0';

    return ret;
}

/**/
char *
get_strarg(char *s)
{
    char t = *s++;

    if (!t)
	return s - 1;

    switch (t) {
    case '(':
	t = ')';
	break;
    case '[':
	t = ']';
	break;
    case '{':
	t = '}';
	break;
    case '<':
	t = '>';
	break;
    case Inpar:
	t = Outpar;
	break;
    case Inang:
	t = Outang;
	break;
    case Inbrace:
	t = Outbrace;
	break;
    case Inbrack:
	t = Outbrack;
	break;
    }

    while (*s && *s != t)
	s++;

    return s;
}

/**/
static int
get_intarg(char **s)
{
    char *t = get_strarg(*s + 1);
    char *p, sav;
    zlong ret;

    if (!*t)
	return -1;
    sav = *t;
    *t = '\0';
    p = dupstring(*s + 2);
    *s = t;
    *t = sav;
    if (parsestr(p))
	return -1;
    singsub(&p);
    if (errflag)
	return -1;
    ret = mathevali(p);
    if (errflag)
	return -1;
    if (ret < 0)
	ret = -ret;
    return ret < 0 ? -ret : ret;
}

/* Parsing for the (e) flag. */

static int
subst_parse_str(char **sp, int single, int err)
{
    char *s;

    *sp = s = dupstring(*sp);

    if (!(err ? parsestr(s) : parsestrnoerr(s))) {
	if (!single) {
            int qt = 0;

	    for (; *s; s++)
		if (!qt) {
		    if (*s == Qstring)
			*s = String;
		    else if (*s == Qtick)
			*s = Tick;
                } else if (*s == Dnull)
                    qt = !qt;
	}
	return 0;
    }
    return 1;
}

/* parameter substitution */

#define	isstring(c) ((c) == '$' || (char)(c) == String || (char)(c) == Qstring)
#define isbrack(c)  ((c) == '[' || (char)(c) == Inbrack)

/*
 * Given a linked list l with node n, perform parameter substitution
 * starting from *str.  Return the node with the substitutuion performed
 * or NULL if it failed.
 *
 * If qt is true, the `$' was quoted.  TODO: why can't we just look
 * to see if the first character was String or Qstring?
 *
 * If ssub is true, we are being called via singsubst(), which means
 * the result will be a single word.  TODO: can we generate the
 * single word at the end?  TODO: if not, or maybe in any case,
 * can we pass down the ssub flag from prefork with the other flags
 * instead of pushing it into different arguments?  (How exactly
 * to qt and ssub differ?  Are both necessary, if so is there some
 * better way of separating the two?)
 */

/**/
LinkNode
paramsubst(LinkList l, LinkNode n, char **str, int qt, int ssub)
{
    char *aptr = *str, c, cc;
    char *s = aptr, *fstr, *idbeg, *idend, *ostr = (char *) getdata(n);
    int colf;			/* != 0 means we found a colon after the name */
    /*
     * There are far too many flags.  They need to be grouped
     * together into some structure which ties them to where they
     * came from.
     *
     * Some flags have a an obscure relationship to their effect which
     * depends on incrementing them to particular values in particular
     * ways.
     */
    /*
     * Whether the value is an array (in aval) or not (in val).  There's
     * a movement from storing the value in the stuff read from the
     * parameter (the value v) to storing them in val and aval.
     * However, sometimes you find v reappearing temporarily.
     *
     * The values -1 and 2 are special to isarr.  It looks like 2 is
     * some kind of an internal flag to do with whether the array's been
     * copied, in which case I don't know why we don't use the copied
     * flag, but they do both occur close together so they presumably
     * have different effects.  The value -1 is isued to force us to
     * keep an empty array.  It's tested in the YUK chunk (I mean the
     * one explicitly marked as such).
     */
    int isarr = 0;
    /*
     * This is just the setting of the option except we need to
     * take account of ^ and ^^.
     */
    int plan9 = isset(RCEXPANDPARAM);
    /*
     * Likwise, but with ~ and ~~.  Also, we turn it off later
     * on if qt is passed down.
     */
    int globsubst = isset(GLOBSUBST);
    /*
     * Indicates ${#pm}, massaged by whichlen which is set by
     * the (c), (w), and (W) flags to indicate how we take the length.
     */
    int getlen = 0;
    int whichlen = 0;
    /*
     * Indicates ${+pm}: a simple boolean for once.
     */
    int chkset = 0;
    /*
     * Indicates we have tried to get a value in v but that was
     * unset.  I don't quite understand why (v == NULL) isn't
     * good enough, but there are places where we seem to need
     * to second guess whether a value is a real value or not.
     */
    int vunset = 0;
    /*
     * Indicates (t) flag, i.e. print out types.  The code for
     * this actually isn't too horrifically inbred compared with
     * that for (P).
     */
    int wantt = 0;
    /*
     * Indicates spliting a string into an array.  There aren't
     * actually that many special cases for this --- which may
     * be why it doesn't work properly; we split in some cases
     * where we shouldn't, in particular on the multsubs for
     * handling embedded values for ${...=...} and the like.
     */
    int spbreak = isset(SHWORDSPLIT) && !ssub && !qt;
    /* Scalar and array value, see isarr above */
    char *val = NULL, **aval = NULL;
    /*
     * Padding based on setting in parameter rather than substitution
     * flags.  This is only used locally.
     */
    unsigned int fwidth = 0;
    /*
     * vbuf and v are both used to retrieve parameter values; this
     * is a kludge, we pass down vbuf and it may or may not return v.
     */
    struct value vbuf;
    Value v = NULL;
    /*
     * This expressive name refers to the set of flags which
     * is applied to matching for #, %, / and their doubled variants:
     * (M), (R), (B), (E), (N), (S).
     */
    int flags = 0;
    /* Value from (I) flag, used for ditto. */
    int flnum = 0;
    /*
     * sortit is an obscure combination of the settings for (o), (O),
     * (i) and (n). casind is (i) and numord is (n); these are
     * separate so we can have fun doing the obscure combinatorics later.
     * indord is the (a) flag, which for consistency doesn't get
     * combined into sortit.
     */
    int sortit = 0, casind = 0, numord = 0, indord = 0;
    /* (u): straightforward. */
    int unique = 0;
    /* combination of (L), (U) and (C) flags. */
    int casmod = 0;
    /*
     * quotemod says we are doing either (q) (positive), (Q) (negative)
     * or not (0).  quotetype counts the q's for the first case.
     * quoterr is simply (X) but gets passed around a lot because the
     * combination (eX) needs it.
     */
    int quotemod = 0, quotetype = 0, quoteerr = 0;
    /*
     * (V) flag: fairly straightforward, except that as with so
     * many flags it's not easy to decide where to put it in the order.
     */
    int visiblemod = 0;
    /*
     * The (z) flag, nothing to do with SH_WORD_SPLIT which is tied
     * spbreak, see above; fairly straighforward in use but c.f.
     * the comment for visiblemod.
     */
    int shsplit = 0;
    /*
     * The separator from (j) and (s) respectively, or (F) and (f)
     * respectively (hardwired to "\n" in that case).  Slightly
     * confusingly also used for ${#pm}, thought that's at least
     * documented in the manual
     */
    char *sep = NULL, *spsep = NULL;
    /*
     * Padding strings.  The left and right padding strings which
     * are repeated, then the ones which only occur once, for
     * the (l) and (r) flags.
     */
    char *premul = NULL, *postmul = NULL, *preone = NULL, *postone = NULL;
    /* Replacement string for /orig/repl and //orig/repl */
    char *replstr = NULL;
    /* The numbers for (l) and (r) */
    zlong prenum = 0, postnum = 0;
    /*
     * Whether the value has been copied.  Optimisation:  if we
     * are modifying an expression, we only need to copy it the
     * first time, and if we don't modify it we can just use the
     * value from the parameter or input.
     */
    int copied = 0;
    /*
     * The (A) flag for array assignment, with consequences for
     * splitting and joining; (AA) gives arrasg == 2 for associative
     * arrays.
     */
    int arrasg = 0;
    /*
     * The (e) flag.  As we need to do extra work not quite
     * at the end, the effect of this is kludged in in several places.
     */
    int eval = 0;
    /*
     * The (P) flag.  This interacts a bit obscurely with whether
     * or not we are dealing with a sub expression (subexp).
     */
    int aspar = 0;
    /*
     * The (%) flag, c.f. visiblemod again.
     */	
    int presc = 0;
    /*
     * The (@) flag; interacts obscurely with qt and isarr.
     * This is one of the things that decides whether multsub
     * will produce an array, but in an extremely indirect fashion.
     */
    int nojoin = 0;
    /*
     * != 0 means ${...}, otherwise $...  What works without braces
     * is largely a historical artefact (everything works with braces,
     * I sincerely hope).
     */
    char inbrace = 0;
    /*
     * Use for the (k) flag.  Goes down into the parameter code,
     * sometimes.
     */
    char hkeys = 0;
    /*
     * Used for the (v) flag, ditto.  Not quite sure why they're
     * separate, but the tradition seems to be that things only
     * get combined when that makes the result more obscure rather
     * than less.
     */
    char hvals = 0;
    /*
     * Whether we had to evaluate a subexpression, i.e. an
     * internal ${...} or $(...) or plain $pm.  We almost don't
     * need to remember this (which would be neater), but the (P)
     * flag means the subexp and !subexp code is obscurely combined,
     * and the argument passing to fetchvalue has another kludge.
     */
    int subexp;

    *s++ = '\0';
    /*
     * Nothing to do unless the character following the $ is
     * something we recognise.
     *
     * Shouldn't this be a table or something?  We test for all
     * these later on, too.
     */
    if (!ialnum(c = *s) && c != '#' && c != Pound && c != '-' &&
	c != '!' && c != '$' && c != String && c != Qstring &&
	c != '?' && c != Quest && c != '_' &&
	c != '*' && c != Star && c != '@' && c != '{' &&
	c != Inbrace && c != '=' && c != Equals && c != Hat &&
	c != '^' && c != '~' && c != Tilde && c != '+') {
	s[-1] = '$';
	*str = s;
	return n;
    }
    DPUTS(c == '{', "BUG: inbrace == '{' in paramsubst()");
    /*
     * Extra processing if there is an opening brace: mostly
     * flags in parentheses, but also one ksh hack.
     */
    if (c == Inbrace) {
	inbrace = 1;
	s++;
	/*
	 * In ksh emulation a leading `!' is a special flag working
	 * sort of like our (k).
	 * TODO: this is one of very few cases tied directly to
	 * the emulation mode rather than an option.  Since ksh
	 * doesn't have parameter flags it might be neater to
	 * handle this with the ^, =, ~ stuff, below.
	 */
	if ((c = *s) == '!' && s[1] != Outbrace && emulation == EMULATE_KSH) {
	    hkeys = SCANPM_WANTKEYS;
	    s++;
	} else if (c == '(' || c == Inpar) {
	    char *t, sav;
	    int tt = 0;
	    zlong num;
	    /*
	     * The (p) flag is (uniquely) only remembered within
	     * this block.  It says we do print-style handling
	     * on the values for flags, but only on those.
	     * This explains the ghastly macro, but why can't it
	     * be a function?  UNTOK_AND_ESCAPE is defined
	     * so that the argument must be an lvalue.
	     */
	    int escapes = 0;
	    int klen;
#define UNTOK(C)  (itok(C) ? ztokens[(C) - Pound] : (C))
#define UNTOK_AND_ESCAPE(X) {\
		untokenize(X = dupstring(s + 1));\
		if (escapes) {\
		    X = getkeystring(X, &klen, 3, NULL);\
		    X = metafy(X, klen, META_HREALLOC);\
		}\
	    }

	    for (s++; (c = *s) != ')' && c != Outpar; s++, tt = 0) {
		switch (c) {
		case ')':
		case Outpar:
		    break;
		case 'A':
		    ++arrasg;
		    break;
		case '@':
		    nojoin = 1;
		    break;
		case 'M':
		    flags |= SUB_MATCH;
		    break;
		case 'R':
		    flags |= SUB_REST;
		    break;
		case 'B':
		    flags |= SUB_BIND;
		    break;
		case 'E':
		    flags |= SUB_EIND;
		    break;
		case 'N':
		    flags |= SUB_LEN;
		    break;
		case 'S':
		    flags |= SUB_SUBSTR;
		    break;
		case 'I':
		    flnum = get_intarg(&s);
		    if (flnum < 0)
			goto flagerr;
		    break;

		case 'L':
		    casmod = 2;
		    break;
		case 'U':
		    casmod = 1;
		    break;
		case 'C':
		    casmod = 3;
		    break;

		case 'o':
		    sortit = 1;
		    break;
		case 'O':
		    sortit = 2;
		    break;
		case 'i':
		    casind = 1;
		    break;
		case 'n':
		    numord = 1;
		    break;
		case 'a':
		    indord = 1;
		    break;

		case 'V':
		    visiblemod++;
		    break;

		case 'q':
		    quotemod++, quotetype++;
		    break;
		case 'Q':
		    quotemod--;
		    break;
		case 'X':
		    quoteerr = 1;
		    break;

		case 'e':
		    eval = 1;
		    break;
		case 'P':
		    aspar = 1;
		    break;

		case 'c':
		    whichlen = 1;
		    break;
		case 'w':
		    whichlen = 2;
		    break;
		case 'W':
		    whichlen = 3;
		    break;

		case 'f':
		    spsep = "\n";
		    break;
		case 'F':
		    sep = "\n";
		    break;

		case 's':
		    tt = 1;
		/* fall through */
		case 'j':
		    t = get_strarg(++s);
		    if (*t) {
			sav = *t;
			*t = '\0';
			if (tt)
			    UNTOK_AND_ESCAPE(spsep)
			else
			    UNTOK_AND_ESCAPE(sep)
			*t = sav;
			s = t;
		    } else
			goto flagerr;
		    break;

		case 'l':
		    tt = 1;
		/* fall through */
		case 'r':
		    sav = s[1];
		    num = get_intarg(&s);
		    if (num < 0)
			goto flagerr;
		    if (tt)
			prenum = num;
		    else
			postnum = num;
		    if (UNTOK(s[1]) != UNTOK(sav))
			break;
		    t = get_strarg(++s);
		    if (!*t)
			goto flagerr;
		    sav = *t;
		    *t = '\0';
		    if (tt)
			UNTOK_AND_ESCAPE(premul)
		    else
			UNTOK_AND_ESCAPE(postmul)
		    *t = sav;
		    sav = *s;
		    s = t + 1;
		    if (UNTOK(*s) != UNTOK(sav)) {
			s--;
			break;
		    }
		    t = get_strarg(s);
		    if (!*t)
			goto flagerr;
		    sav = *t;
		    *t = '\0';
		    if (tt)
			UNTOK_AND_ESCAPE(preone)
		    else
			UNTOK_AND_ESCAPE(postone)
		    *t = sav;
		    s = t;
		    break;

		case 'p':
		    escapes = 1;
		    break;

		case 'k':
		    hkeys = SCANPM_WANTKEYS;
		    break;
		case 'v':
		    hvals = SCANPM_WANTVALS;
		    break;

		case 't':
		    wantt = 1;
		    break;

		case '%':
		    presc++;
		    break;

		case 'z':
		    shsplit = 1;
		    break;

		case 'u':
		    unique = 1;
		    break;

		default:
		  flagerr:
		    zerr("error in flags", NULL, 0);
		    return NULL;
		}
	    }
	    s++;
	}
    }
    /* Sort is done by indexing on sortit-1:
     *   bit 1: ascending (o)/descending (O)
     *   bit 2: case sensitive/independent (i)
     *   bit 3: strict order/numeric (n)
     * unless indord (a) is set set, in which case only test for
     * descending by assuming only (O) is possible (not verified).
     */
    if (sortit)
	sortit += (casind << 1) + (numord << 2);

    /*
     * premul, postmul specify the padding character to be used
     * multiple times with the (l) and (r) flags respectively.
     */
    if (!premul)
	premul = " ";
    if (!postmul)
	postmul = " ";

    /*
     * Look for special unparenthesised flags.
     * TODO: could make these able to appear inside parentheses, too,
     * i.e. ${(^)...} etc.
     */
    for (;;) {
	if ((c = *s) == '^' || c == Hat) {
	    /* RC_EXPAND_PARAM on or off (doubled )*/
	    if ((c = *++s) == '^' || c == Hat) {
		plan9 = 0;
		s++;
	    } else
		plan9 = 1;
	} else if ((c = *s) == '=' || c == Equals) {
	    /* SH_WORD_SPLIT on or off (doubled). spbreak = 2 means force */
	    if ((c = *++s) == '=' || c == Equals) {
		spbreak = 0;
		s++;
	    } else
		spbreak = 2;
	} else if ((c == '#' || c == Pound) &&
		   (iident(cc = s[1])
		    || cc == '*' || cc == Star || cc == '@'
		    || cc == '-' || (cc == ':' && s[2] == '-')
		    || (isstring(cc) && (s[2] == Inbrace || s[2] == Inpar)))) {
	    getlen = 1 + whichlen, s++;
	    /*
	     * Return the length of the parameter.
	     * getlen can be more than 1 to indicate characters (2),
	     * words ignoring multiple delimiters (3), words taking
	     * account of multiple delimiters.  delimiter is in
	     * spsep, NULL means $IFS.
	     */
	} else if (c == '~' || c == Tilde) {
	    /* GLOB_SUBST on or off (doubled) */
	    if ((c = *++s) == '~' || c == Tilde) {
		globsubst = 0;
		s++;
	    } else
		globsubst = 1;
	} else if (c == '+') {
	    /*
	     * Return whether indicated parameter is set. 
	     * Try to handle this when parameter is named
	     * by (P) (second part of test).
	     */
	    if (iident(s[1]) || (aspar && isstring(s[1]) &&
				 (s[2] == Inbrace || s[2] == Inpar)))
		chkset = 1, s++;
	    else if (!inbrace) {
		/* Special case for `$+' on its own --- leave unmodified */
		*aptr = '$';
		*str = aptr + 1;
		return n;
	    } else {
		zerr("bad substitution", NULL, 0);
		return NULL;
	    }
	} else if (inbrace && INULL(*s)) {
	    /*
	     * Handles things like ${(f)"$(<file)"} by skipping 
	     * the double quotes.  We don't need to know what was
	     * actually there; the presence of a String or Qstring
	     * is good enough.
	     */
	    s++;
	} else
	    break;
    }
    /* Don't activate special pattern characters if inside quotes */
    globsubst = globsubst && !qt;

    /*
     * At this point, we usually expect a parameter name.
     * However, there may be a nested ${...} or $(...).
     * These say that the parameter itself is somewhere inside,
     * or that there isn't a parameter and we will get the values
     * from a command substitution itself.  In either case,
     * the current instance of paramsubst() doesn't fetch a value,
     * it just operates on what gets passed up.
     * (The first ought to have been {...}, reserving ${...}
     * for substituting a value at that point, but it's too late now.)
     */
    idbeg = s;
    if ((subexp = (inbrace && s[-1] && isstring(*s) &&
		   (s[1] == Inbrace || s[1] == Inpar)))) {
	int sav;
	int quoted = *s == Qstring;

	val = s++;
	skipparens(*s, *s == Inpar ? Outpar : Outbrace, &s);
	sav = *s;
	*s = 0;
	/*
	 * This handles arrays.  TODO: this is not the most obscure call to
	 * multsub() (see below) but even so it would be nicer to pass down
	 * and back the arrayness more rationally.  In that case, we should
	 * remove the aspar test and extract a value from an array, if
	 * necessary, when we handle (P) lower down.
	 */
	if (multsub(&val, (aspar ? NULL : &aval), &isarr, NULL) && quoted) {
	    /* Empty quoted string --- treat as null string, not elided */
	    isarr = -1;
	    aval = (char **) hcalloc(sizeof(char *));
	    aspar = 0;
	} else if (aspar)
	    idbeg = val;
	*s = sav;
	/*
	 * This tests for the second double quote in an expression
	 * like ${(f)"$(<file)"}, compare above.
	 */
	while (INULL(*s))
	    s++;
	v = (Value) NULL;
    } else if (aspar) {
	/*
	 * No subexpression, but in any case the value is going
	 * to give us the name of a parameter on which we do
	 * our remaining processing.  In other words, this
	 * makes ${(P)param} work like ${(P)${param}}.  (Probably
	 * better looked at, this is the basic code for ${(P)param}
	 * and it's been kludged into the subexp code because no
	 * opportunity for a kludge has been neglected.)
	 */
	if ((v = fetchvalue(&vbuf, &s, 1, (qt ? SCANPM_DQUOTED : 0)))) {
	    val = idbeg = getstrvalue(v);
	    subexp = 1;
	} else
	    vunset = 1;
    }
    /*
     * We need to retrieve a value either if we haven't already
     * got it from a subexpression, or if the processing so
     * far has just yielded us a parameter name to be processed
     * with (P).
     */
    if (!subexp || aspar) {
	char *ov = val;

	/*
	 * Second argument: decide whether to use the subexpression or
	 *   the string next on the line as the parameter name.
	 * Third argument:  decide how processing for brackets
	 *   1 means full processing
	 *   -1 appears to mean something along the lines of
	 *     only handle single digits and don't handle brackets.
	 *     I *think* (but it's really only a guess) that this
	 *     is used by the test below the wantt handling, so
	 *     that in certain cases we handle brackets there.
	 *   0 would apparently mean something like we know we
	 *     should have the name of a scalar and we get cross
	 *     if there's anything present which disagrees with that
	 * but you will search fetchvalue() in vain for comments on this.
	 * Fourth argument gives flags to do with keys, values, quoting,
	 * assigning depending on context and parameter flags.
	 *
	 * This is the last mention of subexp, so presumably this
	 * is what the code which makes sure subexp is set if aspar (the
	 * (P) flag) is set.  I *think* what's going on here is the
	 * second argument is for both input and output: with
	 * subexp, we only want the input effect, whereas normally
	 * we let fetchvalue set the main string pointer s to
	 * the end of the bit it's fetched.
	 */
	if (!(v = fetchvalue(&vbuf, (subexp ? &ov : &s),
			     (wantt ? -1 :
			      ((unset(KSHARRAYS) || inbrace) ? 1 : -1)),
			     hkeys|hvals|
			     (arrasg ? SCANPM_ASSIGNING : 0)|
			     (qt ? SCANPM_DQUOTED : 0))) ||
	    (v->pm && (v->pm->flags & PM_UNSET)))
	    vunset = 1;

	if (wantt) {
	    /*
	     * Handle the (t) flag: value now becomes the type
	     * information for the parameter.
	     */
	    if (v && v->pm && !(v->pm->flags & PM_UNSET)) {
		int f = v->pm->flags;

		switch (PM_TYPE(f)) {
		case PM_SCALAR:  val = "scalar"; break;
		case PM_ARRAY:   val = "array"; break;
		case PM_INTEGER: val = "integer"; break;
		case PM_EFLOAT:
		case PM_FFLOAT:  val = "float"; break;
		case PM_HASHED:  val = "association"; break;
		}
		val = dupstring(val);
		if (v->pm->level)
		    val = dyncat(val, "-local");
		if (f & PM_LEFT)
		    val = dyncat(val, "-left");
		if (f & PM_RIGHT_B)
		    val = dyncat(val, "-right_blanks");
		if (f & PM_RIGHT_Z)
		    val = dyncat(val, "-right_zeros");
		if (f & PM_LOWER)
		    val = dyncat(val, "-lower");
		if (f & PM_UPPER)
		    val = dyncat(val, "-upper");
		if (f & PM_READONLY)
		    val = dyncat(val, "-readonly");
		if (f & PM_TAGGED)
		    val = dyncat(val, "-tag");
		if (f & PM_EXPORTED)
		    val = dyncat(val, "-export");
		if (f & PM_UNIQUE)
		    val = dyncat(val, "-unique");
		if (f & PM_HIDE)
		    val = dyncat(val, "-hide");
		if (f & PM_HIDE)
		    val = dyncat(val, "-hideval");
		if (f & PM_SPECIAL)
		    val = dyncat(val, "-special");
		vunset = 0;
	    } else
		val = dupstring("");

	    v = NULL;
	    isarr = 0;
	}
    }
    /*
     * We get in here two ways; either we need to convert v into
     * the local value system, or we need to get rid of brackets
     * even if there isn't a v.
     */
    while (v || ((inbrace || (unset(KSHARRAYS) && vunset)) && isbrack(*s))) {
	if (!v) {
	    /*
	     * Index applied to non-existent parameter; we may or may
	     * not have a value to index, however.  Create a temporary
	     * empty parameter as a trick, and index on that.  This
	     * usually happens the second time around the loop when
	     * we've used up the original parameter value and want to
	     * apply a subscript to what's left.  However, it's also
	     * possible it's got something to do with some of that murky
	     * passing of -1's as the third argument to fetchvalue() to
	     * inhibit bracket parsing at that stage.
	     */
	    Param pm;
	    char *os = s;

	    if (!isbrack(*s))
		break;
	    if (vunset) {
		val = dupstring("");
		isarr = 0;
	    }
	    pm = createparam(nulstring, isarr ? PM_ARRAY : PM_SCALAR);
	    DPUTS(!pm, "BUG: parameter not created");
	    if (isarr)
		pm->u.arr = aval;
	    else
		pm->u.str = val;
	    v = (Value) hcalloc(sizeof *v);
	    v->isarr = isarr;
	    v->pm = pm;
	    v->end = -1;
	    if (getindex(&s, v, qt) || s == os)
		break;
	}
	/*
	 * This is where we extract a value (we know now we have
	 * one) into the local parameters for a scalar (val) or
	 * array (aval) value.  TODO: move val and aval into
	 * a structure with a discriminator.  Hope we can make
	 * more things array values at this point and dearrayify later.
	 * v->isarr tells us whether the stuff form down below looks
	 * like an array.  Unlike multsub() this is probably clean
	 * enough to keep, although possibly the parameter passing
	 * needs reorganising.
	 *
	 * I think we get to discard the existing value of isarr
	 * here because it's already been taken account of, either
	 * in the subexp stuff or immediately above.
	 */
	if ((isarr = v->isarr)) {
	    /* No way to get here with v->inv != 0, so getvaluearr() *
	     * is called by getarrvalue(); needn't test PM_HASHED.   */
	    if (v->isarr == SCANPM_WANTINDEX) {
		isarr = v->isarr = 0;
		val = dupstring(v->pm->nam);
	    } else
		aval = getarrvalue(v);
	} else {
	    /* Value retrieved from parameter/subexpression is scalar */
	    if (v->pm->flags & PM_ARRAY) {
		/*
		 * Although the value is a scalar, the parameter
		 * itself is an array.  Presumably this is due to
		 * being quoted, or doing single substitution or something,
		 * TODO: we're about to do some definitely stringy
		 * stuff, so something like this bit is probably
		 * necessary.  However, I'd like to leave any
		 * necessary joining of arrays until this point
		 * to avoid the multsub() horror.
		 */
		int tmplen = arrlen(v->pm->gsu.a->getfn(v->pm));

		if (v->start < 0)
		    v->start += tmplen + v->inv;
		if (!v->inv && (v->start >= tmplen || v->start < 0))
		    vunset = 1;
	    }
	    if (!vunset) {
		/*
		 * There really is a value.  Apply any necessary
		 * padding or case transformation.  Note these
		 * are the per-parameter transformations specified
		 * with typeset, not the per-substitution ones set
		 * by flags.  TODO: maybe therefore this would
		 * be more consistent if moved into getstrvalue()?
		 * Bet that's easier said than done.
		 */
		val = getstrvalue(v);
		fwidth = v->pm->width ? v->pm->width : (int)strlen(val);
		switch (v->pm->flags & (PM_LEFT | PM_RIGHT_B | PM_RIGHT_Z)) {
		    char *t;
		    unsigned int t0;

		case PM_LEFT:
		case PM_LEFT | PM_RIGHT_Z:
		    t = val;
		    if (v->pm->flags & PM_RIGHT_Z)
			while (*t == '0')
			    t++;
		    else
			while (iblank(*t))
			    t++;
		    val = (char *) hcalloc(fwidth + 1);
		    val[fwidth] = '\0';
		    if ((t0 = strlen(t)) > fwidth)
			t0 = fwidth;
		    memset(val, ' ', fwidth);
		    strncpy(val, t, t0);
		    break;
		case PM_RIGHT_B:
		case PM_RIGHT_Z:
		case PM_RIGHT_Z | PM_RIGHT_B:
		    {
			int zero = 1;

			if (strlen(val) < fwidth) {
			    char *valprefend = val;
			    if (v->pm->flags & PM_RIGHT_Z) {
				/*
				 * This is a documented feature: when deciding
				 * whether to pad with zeroes, ignore
				 * leading blanks already in the value;
				 * only look for numbers after that.
				 * Not sure how useful this really is.
				 * It's certainly confusing to code around.
				 */
				for (t = val; iblank(*t); t++)
				    ;
				/*
				 * Allow padding after initial minus
				 * for numeric variables.
				 */
				if ((v->pm->flags &
				     (PM_INTEGER|PM_EFLOAT|PM_FFLOAT)) &&
				    *t == '-')
				    t++;
				/*
				 * Allow padding after initial 0x or
				 * base# for integer variables.
				 */
				if (v->pm->flags & PM_INTEGER) {
				    if (isset(CBASES) &&
					t[0] == '0' && t[1] == 'x')
					t += 2;
				    else if ((valprefend = strchr(t, '#')))
					t = valprefend + 1;
				}
				valprefend = t;
				if (!*t)
				    zero = 0;
				else if (v->pm->flags &
					 (PM_INTEGER|PM_EFLOAT|PM_FFLOAT)) {
				    /* zero always OK */
				} else if (!idigit(*t))
				    zero = 0;
			    }
			    t = (char *) hcalloc(fwidth + 1);
			    memset(t, (((v->pm->flags & PM_RIGHT_B) || !zero) ?
				       ' ' : '0'), fwidth);
			    /*
			     * How can the following trigger?  We
			     * haven't altered val or fwidth since
			     * the last time we tested this.
			     */
			    if ((t0 = strlen(val)) > fwidth)
				t0 = fwidth;
			    /*
			     * Copy - or 0x or base# before any padding
			     * zeroes.
			     */
			    if (zero && val != valprefend) {
				int preflen = valprefend - val;
				memcpy(t, val, preflen);
				strcpy(t + (fwidth - t0) + preflen,
				       valprefend);
			    } else
				strcpy(t + (fwidth - t0), val);
			    val = t;
			} else {
			    t = (char *) hcalloc(fwidth + 1);
			    t[fwidth] = '\0';
			    strncpy(t, val + strlen(val) - fwidth, fwidth);
			    val = t;
			}
		    }
		    break;
		}
		switch (v->pm->flags & (PM_LOWER | PM_UPPER)) {
		    char *t;

		case PM_LOWER:
		    t = val;
		    for (; (c = *t); t++)
			*t = tulower(c);
		    break;
		case PM_UPPER:
		    t = val;
		    for (; (c = *t); t++)
			*t = tuupper(c);
		    break;
		}
	    }
	}
	/*
	 * Finished with the original parameter and its indices;
	 * carry on looping to see if we need to do more indexing.
	 * This means we final get rid of v in favour of val and
	 * aval.  We could do with somehow encapsulating the bit
	 * where we need v.
	 */
	v = NULL;
	if (!inbrace)
	    break;
    }
    /*
     * We're now past the name or subexpression; the only things
     * which can happen now are a closing brace, one of the standard
     * parameter postmodifiers, or a history-style colon-modifier.
     *
     * Again, this duplicates tests for characters we're about to
     * examine properly later on.
     */
    if (inbrace &&
	(c = *s) != '-' && c != '+' && c != ':' && c != '%'  && c != '/' &&
	c != '=' && c != Equals &&
	c != '#' && c != Pound &&
	c != '?' && c != Quest &&
	c != '}' && c != Outbrace) {
	zerr("bad substitution", NULL, 0);
	return NULL;
    }
    /*
     * Join arrays up if we're in quotes and there isn't some
     * override such as (@).
     * TODO: hmm, if we're called as part of some recursive
     * substitution do we want to delay this until we get back to
     * the top level?  Or is if there's a qt (i.e. this parameter
     * substitution is in quotes) always good enough?  Potentially
     * we may be OK by now --- all potential `@'s and subexpressions
     * have been handled, including any [@] index which comes up
     * by virture of v->isarr being set to SCANPM_ISVAR_AT which
     * is now in isarr.
     *
     * However, if we are replacing multsub() with something that
     * doesn't mangle arrays, we may need to delay this step until after
     * the foo:- or foo:= or whatever that causes that.  Note the value
     * (string or array) at this point is irrelevant if we are going to
     * be doing that.  This would mean // and stuff get applied
     * arraywise even if quoted.  That's probably wrong, so maybe
     * this just stays.
     *
     * We do a separate stage of dearrayification in the YUK chunk,
     * I think mostly because of the way we make array or scalar
     * values appear to the caller.
     */
    if (isarr) {
	if (nojoin)
	    isarr = -1;
	if (qt && !getlen && isarr > 0) {
	    val = sepjoin(aval, sep, 1);
	    isarr = 0;
	}
    }

    idend = s;
    if (inbrace) {
	/*
	 * This is to match a closing double quote in case
	 * we didn't have a subexpression, e.g. ${"foo"}.
	 * This form is pointless, but logically it ought to work.
	 */
	while (INULL(*s))
	    s++;
    }
    /*
     * We don't yet know whether a `:' introduces a history-style
     * colon modifier or qualifies something like ${...:=...}.
     * But if we remember the colon here it's easy to check later.
     */
    if ((colf = *s == ':'))
	s++;


    /* fstr is to be the text following the substitution.  If we have *
     * braces, we look for it here, else we infer it later on.        */
    fstr = s;
    if (inbrace) {
	int bct;
	for (bct = 1; (c = *fstr); fstr++) {
	    if (c == Inbrace)
		bct++;
	    else if (c == Outbrace && !--bct)
		break;
	}

	if (bct) {
	noclosebrace:
	    zerr("closing brace expected", NULL, 0);
	    return NULL;
	}
	if (c)
	    *fstr++ = '\0';
    }

    /* Check for ${..?..} or ${..=..} or one of those. *
     * Only works if the name is in braces.            */

    if (inbrace && ((c = *s) == '-' ||
		    c == '+' ||
		    c == ':' ||	/* i.e. a doubled colon */
		    c == '=' || c == Equals ||
		    c == '%' ||
		    c == '#' || c == Pound ||
		    c == '?' || c == Quest ||
		    c == '/')) {

	/*
	 * Default index is 1 if no (I) or (I) gave zero.   But
	 * why don't we set the default explicitly at the start
	 * and massage any passed index where we set flnum anyway?
	 */
	if (!flnum)
	    flnum++;
	if (c == '%')
	    flags |= SUB_END;

	/* Check for ${..%%..} or ${..##..} */
	if ((c == '%' || c == '#' || c == Pound) && c == s[1]) {
	    s++;
	    /* we have %%, not %, or ##, not # */
	    flags |= SUB_LONG;
	}
	s++;
	if (s[-1] == '/') {
	    char *ptr;
	    /*
	     * previous flags are irrelevant, except for (S) which
	     * indicates shortest substring; else look for longest.
	     */
	    flags = (flags & SUB_SUBSTR) ? 0 : SUB_LONG;
	    if ((c = *s) == '/') {
		/* doubled, so replace all occurrences */
		flags |= SUB_GLOBAL;
		c = *++s;
	    }
	    /* Check for anchored substitution */
	    if (c == '%') {
		/* anchor at tail */
		flags |= SUB_END;
		s++;
	    } else if (c == '#' || c == Pound) {
		/* anchor at head: this is the `normal' case in getmatch */
		s++;
	    } else
		flags |= SUB_SUBSTR;
	    /*
	     * Find the / marking the end of the search pattern.
	     * If there isn't one, we're just going to delete that,
	     * i.e. replace it with an empty string.
	     *
	     * We used to use double backslashes to quote slashes,
	     * but actually that was buggy and using a single backslash
	     * is easier and more obvious.
	     */
	    for (ptr = s; (c = *ptr) && c != '/'; ptr++)
	    {
		if ((c == Bnull || c == '\\') && ptr[1])
		{
		    if (ptr[1] == '/')
			chuck(ptr);
		    else
			ptr++;
		}
	    }
	    replstr = (*ptr && ptr[1]) ? ptr+1 : "";
	    *ptr = '\0';
	}

	/* See if this was ${...:-...}, ${...:=...}, etc. */
	if (colf)
	    flags |= SUB_ALL;
	/*
	 * With no special flags, i.e. just a # or % or whatever,
	 * the matched portion is removed and we keep the rest.
	 * We also want the rest when we're doing a substitution.
	 */
	if (!(flags & (SUB_MATCH|SUB_REST|SUB_BIND|SUB_EIND|SUB_LEN)))
	    flags |= SUB_REST;

	if (colf && !vunset)
	    vunset = (isarr) ? !*aval : !*val || (*val == Nularg && !val[1]);

	switch (s[-1]) {
	case '+':
	    if (vunset) {
		val = dupstring("");
		copied = 1;
		isarr = 0;
		break;
	    }
	    vunset = 1;
	/* Fall Through! */
	case '-':
	    if (vunset) {
		val = dupstring(s);
		/*
		 * This is not good enough for sh emulation!  Sh would
		 * split unquoted substrings, yet not split quoted ones
		 * (except according to $@ rules); but this leaves the
		 * unquoted substrings unsplit, and other code below
		 * for spbreak splits even within the quoted substrings.
		 *
		 * TODO: I think multsub needs to be told enough to
		 * decide about splitting with spbreak at this point
		 * (and equally in the `=' handler below).  Then
		 * we can turn off spbreak to avoid the join & split
		 * nastiness later.
		 *
		 * What we really want to do is make this look as
		 * if it were the result of an assignment from
		 * the same value, taking account of quoting.
		 */
		multsub(&val, (aspar ? NULL : &aval), &isarr, NULL);
		copied = 1;
	    }
	    break;
	case ':':
	    /* this must be `::=', unconditional assignment */
	    if (*s != '=' && *s != Equals)
		goto noclosebrace;
	    vunset = 1;
	    s++;
	    /* Fall through */
	case '=':
	case Equals:
	    if (vunset) {
		char sav = *idend;
		int l;

		*idend = '\0';
		val = dupstring(s);
		isarr = 0;
		/*
		 * TODO: this is one of those places where I don't
		 * think we want to do the joining until later on.
		 * We also need to handle spbreak and spsep at this
		 * point and unset them.
		 */
		if (spsep || spbreak || !arrasg)
		    multsub(&val, NULL, NULL, sep);
		else
		    multsub(&val, &aval, &isarr, NULL);
		if (arrasg) {
		    /*
		     * This is an array assignment in a context
		     * where we have no syntactic way of finding
		     * out what an array element is.  So we just guess.
		     */
		    char *arr[2], **t, **a, **p;
		    if (spsep || spbreak) {
			aval = sepsplit(val, spsep, 0, 1);
			isarr = 2;
			l = arrlen(aval);
			if (l && !*(aval[l-1]))
			    l--;
			if (l && !**aval)
			    l--, t = aval + 1;
			else
			    t = aval;
		    } else if (!isarr) {
			if (!*val && arrasg > 1) {
			    arr[0] = NULL;
			    l = 0;
			} else {
			    arr[0] = val;
			    arr[1] = NULL;
			    l = 1;
			}
			t = aval = arr;
		    } else
			l = arrlen(aval), t = aval;
		    p = a = zalloc(sizeof(char *) * (l + 1));
		    while (l--) {
			untokenize(*t);
			*p++ = ztrdup(*t++);
		    }
		    *p++ = NULL;
		    if (arrasg > 1) {
			Param pm = sethparam(idbeg, a);
			if (pm)
			    aval = paramvalarr(pm->gsu.h->getfn(pm), hkeys|hvals);
		    } else
			setaparam(idbeg, a);
		} else {
		    untokenize(val);
		    setsparam(idbeg, ztrdup(val));
		}
		*idend = sav;
		copied = 1;
		if (isarr) {
		  if (nojoin)
		    isarr = -1;
		  if (qt && !getlen && isarr > 0 && !spsep && spbreak < 2) {
		    val = sepjoin(aval, sep, 1);
		    isarr = 0;
		  }
		  sep = spsep = NULL;
		  spbreak = 0;
		}
	    }
	    break;
	case '?':
	case Quest:
	    if (vunset) {
		char *msg;

		*idend = '\0';
		msg = tricat(idbeg, ": ", *s ? s : "parameter not set");
		zerr("%s", msg, 0);
		zsfree(msg);
		if (!interact)
		    exit(1);
		return NULL;
	    }
	    break;
	case '%':
	case '#':
	case Pound:
	case '/':
            /* This once was executed only `if (qt) ...'. But with that
             * patterns in a expansion resulting from a ${(e)...} aren't
             * tokenized even though this function thinks they are (it thinks
             * they are because subst_parse_str() turns Qstring tokens
             * into String tokens and for unquoted parameter expansions the
             * lexer normally does tokenize patterns inside parameter
             * expansions). */
            {
		int one = noerrs, oef = errflag, haserr;

		if (!quoteerr)
		    noerrs = 1;
		haserr = parse_subst_string(s);
		noerrs = one;
		if (!quoteerr) {
		    errflag = oef;
		    if (haserr)
			shtokenize(s);
		} else if (haserr || errflag) {
		    zerr("parse error in ${...%c...} substitution",
			 NULL, s[-1]);
		    return NULL;
		}
	    }
	    {
#if 0
		/*
		 * This allows # and % to be at the start of
		 * a parameter in the substitution, which is
		 * a bit nasty, and can be done (although
		 * less efficiently) with anchors.
		 */

		char t = s[-1];

		singsub(&s);

		if (t == '/' && (flags & SUB_SUBSTR)) {
		    if ((c = *s) == '#' || c == '%') {
			flags &= ~SUB_SUBSTR;
			if (c == '%')
			    flags |= SUB_END;
			s++;
		    } else if (c == '\\') {
			s++;
		    }
		}
#else
		singsub(&s);
#endif
	    }

	    /*
	     * Either loop over an array doing replacements or
	     * do the replacment on a string.
	     */
	    if (!vunset && isarr) {
		getmatcharr(&aval, s, flags, flnum, replstr);
		copied = 1;
	    } else {
		if (vunset)
		    val = dupstring("");
		getmatch(&val, s, flags, flnum, replstr);
		copied = 1;
	    }
	    break;
	}
    } else {			/* no ${...=...} or anything, but possible modifiers. */
	/*
	 * Handler ${+...}.  TODO: strange, why do we handle this only
	 * if there isn't a trailing modifier?  Why don't we do this
	 * e.g. when we hanlder the ${(t)...} flag?
	 */
	if (chkset) {
	    val = dupstring(vunset ? "0" : "1");
	    isarr = 0;
	} else if (vunset) {
	    if (unset(UNSET)) {
		*idend = '\0';
		zerr("%s: parameter not set", idbeg, 0);
		return NULL;
	    }
	    val = dupstring("");
	}
	if (colf) {
	    /*
	     * History style colon modifiers.  May need to apply
	     * on multiple elements of an array.
	     */
	    s--;
	    if (unset(KSHARRAYS) || inbrace) {
		if (!isarr)
		    modify(&val, &s);
		else {
		    char *ss;
		    char **ap = aval;
		    char **pp = aval = (char **) hcalloc(sizeof(char *) *
							 (arrlen(aval) + 1));

		    while ((*pp = *ap++)) {
			ss = s;
			modify(pp++, &ss);
		    }
		    if (pp == aval) {
			char *t = "";
			ss = s;
			modify(&t, &ss);
		    }
		    s = ss;
		}
		copied = 1;
		if (inbrace && *s) {
		    if (*s == ':' && !imeta(s[1]))
			zerr("unrecognized modifier `%c'", NULL, s[1]);
		    else
			zerr("unrecognized modifier", NULL, 0);
		    return NULL;
		}
	    }
	}
	if (!inbrace)
	    fstr = s;
    }
    if (errflag)
	return NULL;
    /*
     * This handles taking a length with ${#foo} and variations.
     * TODO: again. one might naively have thought this had the
     * same sort of effect as the ${(t)...} flag and the ${+...}
     * test, although in this case we do need the value rather
     * the the parameter, so maybe it's a bit different.
     */
    if (getlen) {
	long len = 0;
	char buf[14];

	if (isarr) {
	    char **ctr;
	    int sl = sep ? ztrlen(sep) : 1;

	    if (getlen == 1)
		for (ctr = aval; *ctr; ctr++, len++);
	    else if (getlen == 2) {
		if (*aval)
		    for (len = -sl, ctr = aval;
			 len += sl + ztrlen(*ctr), *++ctr;);
	    }
	    else
		for (ctr = aval;
		     *ctr;
		     len += wordcount(*ctr, spsep, getlen > 3), ctr++);
	} else {
	    if (getlen < 3)
		len = ztrlen(val);
	    else
		len = wordcount(val, spsep, getlen > 3);
	}

	sprintf(buf, "%ld", len);
	val = dupstring(buf);
	isarr = 0;
    }
    /*
     * I think this mult_isarr stuff here is used to pass back
     * the setting of whether we are an array to multsub, and
     * thence to the top-level paramsubst().  The way the
     * setting is passed back is completely obscure, however.
     * It's presumably at this point because we try to remember
     * whether the value was `really' an array before massaging
     * some special cases.
     *
     * TODO: YUK.  This is not the right place to turn arrays into
     * scalars; we should pass back as an array, and let the calling
     * code decide how to deal with it.  This is almost certainly
     * a lot harder than it sounds.  Do we really need to handle
     * one-element arrays as scalars at this point?  Couldn't
     * we just test for it later rather than having a multiple-valued
     * wave-function for isarr?
     */
    mult_isarr = isarr;
    if (isarr > 0 && !plan9 && (!aval || !aval[0])) {
	val = dupstring("");
	isarr = 0;
    } else if (isarr && aval && aval[0] && !aval[1]) {
	/* treat a one-element array as a scalar for purposes of   *
	 * concatenation with surrounding text (some${param}thing) *
	 * and rc_expand_param handling.  Note: mult_isarr (above) *
	 * propagates the true array type from nested expansions.  */
	val = aval[0];
	isarr = 0;
    }
    /* ssub is true when we are called from singsub (via prefork).
     * It means that we must join arrays and should not split words. */
    /*
     * TODO: this is what is screwing up the use of SH_WORD_SPLIT
     * after `:-' etc.  If we fix multsub(), we might get away
     * with simply unsetting the appropriate flags when they
     * get handled.
     */
    if (ssub || spbreak || spsep || sep) {
	if (isarr)
	    val = sepjoin(aval, sep, 1), isarr = 0;
	if (!ssub && (spbreak || spsep)) {
	    aval = sepsplit(val, spsep, 0, 1);
	    if (!aval || !aval[0])
		val = dupstring("");
	    else if (!aval[1])
		val = aval[0];
	    else
		isarr = 2;
	}
	mult_isarr = isarr;
    }
    /*
     * Perform case modififications.
     */
    if (casmod) {
	if (isarr) {
	    char **ap;

	    if (!copied)
		aval = arrdup(aval), copied = 1;
	    ap = aval;

	    if (casmod == 1)
		for (; *ap; ap++)
		    makeuppercase(ap);
	    else if (casmod == 2)
		for (; *ap; ap++)
		    makelowercase(ap);
	    else
		for (; *ap; ap++)
		    makecapitals(ap);

	} else {
	    if (!copied)
		val = dupstring(val), copied = 1;
	    if (casmod == 1)
		makeuppercase(&val);
	    else if (casmod == 2)
		makelowercase(&val);
	    else
		makecapitals(&val);
	}
    }
    /*
     * Perform prompt-style modifications.
     */
    if (presc) {
	int ops = opts[PROMPTSUBST], opb = opts[PROMPTBANG];
	int opp = opts[PROMPTPERCENT], len;

	if (presc < 2) {
	    opts[PROMPTPERCENT] = 1;
	    opts[PROMPTSUBST] = opts[PROMPTBANG] = 0;
	}
	/*
	 * TODO:  It would be really quite nice to abstract the
	 * isarr and !issarr code into a function which gets
	 * passed a pointer to a function with the effect of
	 * the promptexpand bit.  Then we could use this for
	 * a lot of stuff and bury val/aval/isarr inside a structure
	 * which gets passed to it.
	 */
	if (isarr) {
	    char **ap;

	    if (!copied)
		aval = arrdup(aval), copied = 1;
	    ap = aval;
	    for (; *ap; ap++) {
		char *tmps;
		unmetafy(*ap, &len);
		untokenize(*ap);
		tmps = unmetafy(promptexpand(metafy(*ap, len, META_NOALLOC),
					     0, NULL, NULL), &len);
		*ap = dupstring(tmps);
		free(tmps);
	    }
	} else {
	    char *tmps;
	    if (!copied)
		val = dupstring(val), copied = 1;
	    unmetafy(val, &len);
	    untokenize(val);
	    tmps = unmetafy(promptexpand(metafy(val, len, META_NOALLOC),
					0, NULL, NULL), &len);
	    val = dupstring(tmps);
	    free(tmps);
	}
	opts[PROMPTSUBST] = ops;
	opts[PROMPTBANG] = opb;
	opts[PROMPTPERCENT] = opp;
    }
    /*
     * One of the possible set of quotes to apply, depending on
     * the repetitions of the (q) flag.
     */
    if (quotemod) {
	if (--quotetype > 3)
	    quotetype = 3;
	if (isarr) {
	    char **ap;

	    if (!copied)
		aval = arrdup(aval), copied = 1;
	    ap = aval;

	    if (quotemod > 0) {
		if (quotetype) {
		    int sl;
		    char *tmp;

		    for (; *ap; ap++) {
			int pre = quotetype != 3 ? 1 : 2;
			tmp = bslashquote(*ap, NULL, quotetype);
			sl = strlen(tmp);
			*ap = (char *) zhalloc(pre + sl + 2);
			strcpy((*ap) + pre, tmp);
			ap[0][pre - 1] = ap[0][pre + sl] = (quotetype != 2 ? '\'' : '"');
			ap[0][pre + sl + 1] = '\0';
			if (quotetype == 3)
			  ap[0][0] = '$';
		    }
		} else
		    for (; *ap; ap++)
			*ap = bslashquote(*ap, NULL, 0);
	    } else {
		int one = noerrs, oef = errflag, haserr = 0;

		if (!quoteerr)
		    noerrs = 1;
		for (; *ap; ap++) {
		    haserr |= parse_subst_string(*ap);
		    remnulargs(*ap);
		    untokenize(*ap);
		}
		noerrs = one;
		if (!quoteerr)
		    errflag = oef;
		else if (haserr || errflag) {
		    zerr("parse error in parameter value", NULL, 0);
		    return NULL;
		}
	    }
	} else {
	    if (!copied)
		val = dupstring(val), copied = 1;
	    if (quotemod > 0) {
		if (quotetype) {
		    int pre = quotetype != 3 ? 1 : 2;
		    int sl;
		    char *tmp;
		    tmp = bslashquote(val, NULL, quotetype);
		    sl = strlen(tmp);
		    val = (char *) zhalloc(pre + sl + 2);
		    strcpy(val + pre, tmp);
		    val[pre - 1] = val[pre + sl] = (quotetype != 2 ? '\'' : '"');
		    val[pre + sl + 1] = '\0';
		    if (quotetype == 3)
		      val[0] = '$';
		} else
		    val = bslashquote(val, NULL, 0);
	    } else {
		int one = noerrs, oef = errflag, haserr;

		if (!quoteerr)
		    noerrs = 1;
		haserr = parse_subst_string(val);
		noerrs = one;
		if (!quoteerr)
		    errflag = oef;
		else if (haserr || errflag) {
		    zerr("parse error in parameter value", NULL, 0);
		    return NULL;
		}
		remnulargs(val);
		untokenize(val);
	    }
	}
    }
    /*
     * Transform special characters in the string to make them
     * printable.
     */
    if (visiblemod) {
	if (isarr) {
	    char **ap;
	    if (!copied)
		aval = arrdup(aval), copied = 1;
	    for (ap = aval; *ap; ap++)
		*ap = nicedupstring(*ap);
	} else {
	    if (!copied)
		val = dupstring(val), copied = 1;
	    val = nicedupstring(val);
	}
    }
    /*
     * Nothing particularly to do with SH_WORD_SPLIT --- this
     * performs lexical splitting on a string as specified by
     * the (z) flag.
     */
    if (shsplit) {
	LinkList list = NULL;

	if (isarr) {
	    char **ap;
	    for (ap = aval; *ap; ap++)
		list = bufferwords(list, *ap, NULL);
	    isarr = 0;
	} else
	    list = bufferwords(NULL, val, NULL);

	if (!list || !firstnode(list))
	    val = dupstring("");
	else if (!nextnode(firstnode(list)))
	    val = getdata(firstnode(list));
	else {
	    char **ap;
	    LinkNode node;

	    aval = ap = (char **) zhalloc((countlinknodes(list) + 1) *
					  sizeof(char *));
	    for (node = firstnode(list); node; incnode(node))
		*ap++ = (char *) getdata(node);
	    *ap = NULL;
	    mult_isarr = isarr = 2;
	}
	copied = 1;
    }
    /*
     * TODO: hmm.  At this point we have to be on our toes about
     * whether we're putting stuff into a line or not, i.e.
     * we don't want to do this from a recursive call; this is
     * probably part of the point of the mult_isarr monkey business.
     * Rather than passing back flags in a non-trivial way, maybe
     * we could decide on the basis of flags passed down to us.
     *
     * This is the ideal place to do any last-minute conversion from
     * array to strings.  However, given all the transformations we've
     * already done, probably if it's going to be done it will already
     * have been.  (I'd really like to keep everying in aval or
     * equivalent and only locally decide if we need to treat it
     * as a scalar.)
     */
    if (isarr) {
	char *x;
	char *y;
	int xlen;
	int i;
	LinkNode on = n;

	/* Handle the (u) flag; we need this before the next test */
	if (unique) {
	    if(!copied)
		aval = arrdup(aval);

	    i = arrlen(aval);
	    if (i > 1)
		zhuniqarray(aval);
	}
	if ((!aval[0] || !aval[1]) && !plan9) {
	    /*
	     * Empty array or single element.  Currently you only
	     * get a single element array at this point from the
	     * unique expansion above. but we can potentially
	     * have other reasons.
	     *
	     * The following test removes the markers
	     * from surrounding double quotes, but I don't know why
	     * that's necessary.
	     */
	    int vallen;
	    if (aptr > (char *) getdata(n) &&
		aptr[-1] == Dnull && *fstr == Dnull)
		*--aptr = '\0', fstr++;
	    vallen = aval[0] ? strlen(aval[0]) : 0;
	    y = (char *) hcalloc((aptr - ostr) + vallen + strlen(fstr) + 1);
	    strcpy(y, ostr);
	    *str = y + (aptr - ostr);
	    if (vallen)
	    {
		strcpy(*str, aval[0]);
		*str += vallen;
	    }
	    strcpy(*str, fstr);
	    setdata(n, y);
	    return n;
	}
	/* Handle (o) and (O) and their variants */
	if (sortit) {
	    if (!copied)
		aval = arrdup(aval);
	    if (indord) {
		if (sortit & 2) {
		    char *copy;
		    char **end = aval + arrlen(aval) - 1, **start = aval;

		    /* reverse the array */
		    while (start < end) {
			copy = *end;
			*end-- = *start;
			*start++ = copy;
		    }
		}
	    } else {
		static CompareFn sortfn[] = {
		    strpcmp, invstrpcmp, cstrpcmp, invcstrpcmp,
		    nstrpcmp, invnstrpcmp, instrpcmp, invinstrpcmp
		};

		i = arrlen(aval);
		if (i && (*aval[i-1] || --i))
		    qsort(aval, i, sizeof(char *), sortfn[sortit-1]);
	    }
	}
	if (plan9) {
	    /* Handle RC_EXPAND_PARAM */
	    LinkNode tn;
	    local_list1(tl);

	    *--fstr = Marker;
	    init_list1(tl, fstr);
	    if (!eval && !stringsubst(&tl, firstnode(&tl), ssub, 0))
		return NULL;
	    *str = aptr;
	    tn = firstnode(&tl);
	    while ((x = *aval++)) {
		if (prenum || postnum)
		    x = dopadding(x, prenum, postnum, preone, postone,
				  premul, postmul);
		if (eval && subst_parse_str(&x, (qt && !nojoin), quoteerr))
		    return NULL;
		xlen = strlen(x);
		for (tn = firstnode(&tl);
		     tn && *(y = (char *) getdata(tn)) == Marker;
		     incnode(tn)) {
		    strcatsub(&y, ostr, aptr, x, xlen, y + 1, globsubst,
			      copied);
		    if (qt && !*y && isarr != 2)
			y = dupstring(nulstring);
		    if (plan9)
			setdata(n, (void *) y), plan9 = 0;
		    else
			insertlinknode(l, n, (void *) y), incnode(n);
		}
	    }
	    for (; tn; incnode(tn)) {
		y = (char *) getdata(tn);
		if (*y == Marker)
		    continue;
		if (qt && !*y && isarr != 2)
		    y = dupstring(nulstring);
		if (plan9)
		    setdata(n, (void *) y), plan9 = 0;
		else
		    insertlinknode(l, n, (void *) y), incnode(n);
	    }
	    if (plan9) {
		uremnode(l, n);
		return n;
	    }
	} else {
	    /*
	     * Not RC_EXPAND_PARAM: simply join the first and
	     * last values.
	     * TODO: how about removing the restriction that
	     * aval[1] is non-NULL to promote consistency?, or
	     * simply changing the test so that we drop into
	     * the scalar branch, instead of tricking isarr?
	     */
	    x = aval[0];
	    if (prenum || postnum)
		x = dopadding(x, prenum, postnum, preone, postone,
			      premul, postmul);
	    if (eval && subst_parse_str(&x, (qt && !nojoin), quoteerr))
		return NULL;
	    xlen = strlen(x);
	    strcatsub(&y, ostr, aptr, x, xlen, NULL, globsubst, copied);
	    if (qt && !*y && isarr != 2)
		y = dupstring(nulstring);
	    setdata(n, (void *) y);

	    i = 1;
	    /* aval[1] is non-null here */
	    while (aval[i + 1]) {
		x = aval[i++];
		if (prenum || postnum)
		    x = dopadding(x, prenum, postnum, preone, postone,
				  premul, postmul);
		if (eval && subst_parse_str(&x, (qt && !nojoin), quoteerr))
		    return NULL;
		if (qt && !*x && isarr != 2)
		    y = dupstring(nulstring);
		else {
		    y = dupstring(x);
		    if (globsubst)
			shtokenize(y);
		}
		insertlinknode(l, n, (void *) y), incnode(n);
	    }

	    x = aval[i];
	    if (prenum || postnum)
		x = dopadding(x, prenum, postnum, preone, postone,
			      premul, postmul);
	    if (eval && subst_parse_str(&x, (qt && !nojoin), quoteerr))
		return NULL;
	    xlen = strlen(x);
	    *str = strcatsub(&y, aptr, aptr, x, xlen, fstr, globsubst, copied);
	    if (qt && !*y && isarr != 2)
		y = dupstring(nulstring);
	    insertlinknode(l, n, (void *) y), incnode(n);
	}
	if (eval)
	    n = on;
    } else {
	/*
	 * Scalar value.  Handle last minute transformations
	 * such as left- or right-padding and the (e) flag to
	 * revaluate the result.
	 */
	int xlen;
	char *x;
	char *y;

	x = val;
	if (prenum || postnum)
	    x = dopadding(x, prenum, postnum, preone, postone,
			  premul, postmul);
	if (eval && subst_parse_str(&x, (qt && !nojoin), quoteerr))
	    return NULL;
	xlen = strlen(x);
	*str = strcatsub(&y, ostr, aptr, x, xlen, fstr, globsubst, copied);
	if (qt && !*y)
	    y = dupstring(nulstring);
	setdata(n, (void *) y);
    }
    if (eval)
	*str = (char *) getdata(n);

    return n;
}

/*
 * Arithmetic substitution: `a' is the string to be evaluated, `bptr'
 * points to the beginning of the string containing it.  The tail of
 * the string is given by `rest'. *bptr is modified with the substituted
 * string. The function returns a pointer to the tail in the substituted
 * string.
 */

/**/
static char *
arithsubst(char *a, char **bptr, char *rest)
{
    char *s = *bptr, *t;
    char buf[BDIGBUFSIZE], *b = buf;
    mnumber v;

    singsub(&a);
    v = matheval(a);
    if ((v.type & MN_FLOAT) && !outputradix)
	b = convfloat(v.u.d, 0, 0, NULL);
    else {
	if (v.type & MN_FLOAT)
	    v.u.l = (zlong) v.u.d;
	convbase(buf, v.u.l, outputradix);
    }
    t = *bptr = (char *) hcalloc(strlen(*bptr) + strlen(b) + 
				 strlen(rest) + 1);
    t--;
    while ((*++t = *s++));
    t--;
    while ((*++t = *b++));
    strcat(t, rest);
    return t;
}

/**/
void
modify(char **str, char **ptr)
{
    char *ptr1, *ptr2, *ptr3, del, *lptr, c, *test, *sep, *t, *tt, tc, *e;
    char *copy, *all, *tmp, sav;
    int gbal, wall, rec, al, nl;

    test = NULL;

    if (**ptr == ':')
	*str = dupstring(*str);

    while (**ptr == ':') {
	lptr = *ptr;
	(*ptr)++;
	wall = gbal = 0;
	rec = 1;
	c = '\0';
	sep = NULL;

	for (; !c && **ptr;) {
	    switch (**ptr) {
	    case 'h':
	    case 'r':
	    case 'e':
	    case 't':
	    case 'l':
	    case 'u':
	    case 'q':
	    case 'Q':
		c = **ptr;
		break;

	    case 's':
		c = **ptr;
		(*ptr)++;
		ptr1 = *ptr;
		del = *ptr1++;
		for (ptr2 = ptr1; *ptr2 != del && *ptr2; ptr2++);
		if (!*ptr2) {
		    zerr("bad substitution", NULL, 0);
		    return;
		}
		*ptr2++ = '\0';
		for (ptr3 = ptr2; *ptr3 != del && *ptr3; ptr3++);
		if ((sav = *ptr3))
		    *ptr3++ = '\0';
		if (*ptr1) {
		    zsfree(hsubl);
		    hsubl = ztrdup(ptr1);
 		}
		if (!hsubl) {
		    zerr("no previous substitution", NULL, 0);
		    return;
		}
		zsfree(hsubr);
		for (tt = hsubl; *tt; tt++)
		    if (INULL(*tt))
			chuck(tt--);
		untokenize(hsubl);
		for (tt = hsubr = ztrdup(ptr2); *tt; tt++)
		    if (INULL(*tt))
			chuck(tt--);
		ptr2[-1] = del;
		if (sav)
		    ptr3[-1] = sav;
		*ptr = ptr3 - 1;
		break;

	    case '&':
		c = 's';
		break;

	    case 'g':
		(*ptr)++;
		gbal = 1;
		break;

	    case 'w':
		wall = 1;
		(*ptr)++;
		break;
	    case 'W':
		wall = 1;
		(*ptr)++;
		ptr1 = get_strarg(ptr2 = *ptr);
		if ((sav = *ptr1))
		    *ptr1 = '\0';
		sep = dupstring(ptr2 + 1);
		if (sav)
		    *ptr1 = sav;
		*ptr = ptr1 + 1;
		c = '\0';
		break;

	    case 'f':
		rec = -1;
		(*ptr)++;
		break;
	    case 'F':
		rec = get_intarg(ptr);
		(*ptr)++;
		break;
	    default:
		*ptr = lptr;
		return;
	    }
	}
	(*ptr)++;
	if (!c) {
	    *ptr = lptr;
	    return;
	}
	if (rec < 0)
	    test = dupstring(*str);

	while (rec--) {
	    if (wall) {
		al = 0;
		all = NULL;
		for (t = e = *str; (tt = findword(&e, sep));) {
		    tc = *e;
		    *e = '\0';
		    copy = dupstring(tt);
		    *e = tc;
		    switch (c) {
		    case 'h':
			remtpath(&copy);
			break;
		    case 'r':
			remtext(&copy);
			break;
		    case 'e':
			rembutext(&copy);
			break;
		    case 't':
			remlpaths(&copy);
			break;
		    case 'l':
			downcase(&copy);
			break;
		    case 'u':
			upcase(&copy);
			break;
		    case 's':
			if (hsubl && hsubr)
			    subst(&copy, hsubl, hsubr, gbal);
			break;
		    case 'q':
			copy = bslashquote(copy, NULL, 0);
			break;
		    case 'Q':
			{
			    int one = noerrs, oef = errflag;

			    noerrs = 1;
			    parse_subst_string(copy);
			    noerrs = one;
			    errflag = oef;
			    remnulargs(copy);
			    untokenize(copy);
			}
			break;
		    }
		    tc = *tt;
		    *tt = '\0';
		    nl = al + strlen(t) + strlen(copy);
		    ptr1 = tmp = (char *)zhalloc(nl + 1);
		    if (all)
			for (ptr2 = all; *ptr2;)
			    *ptr1++ = *ptr2++;
		    for (ptr2 = t; *ptr2;)
			*ptr1++ = *ptr2++;
		    *tt = tc;
		    for (ptr2 = copy; *ptr2;)
			*ptr1++ = *ptr2++;
		    *ptr1 = '\0';
		    al = nl;
		    all = tmp;
		    t = e;
		}
		*str = all;

	    } else {
		switch (c) {
		case 'h':
		    remtpath(str);
		    break;
		case 'r':
		    remtext(str);
		    break;
		case 'e':
		    rembutext(str);
		    break;
		case 't':
		    remlpaths(str);
		    break;
		case 'l':
		    downcase(str);
		    break;
		case 'u':
		    upcase(str);
		    break;
		case 's':
		    if (hsubl && hsubr) {
			char *oldstr = *str;

			subst(str, hsubl, hsubr, gbal);
			if (*str != oldstr) {
			    *str = dupstring(oldstr = *str);
			    zsfree(oldstr);
			}
		    }
		    break;
		case 'q':
		    *str = bslashquote(*str, NULL, 0);
		    break;
		case 'Q':
		    {
			int one = noerrs, oef = errflag;

			noerrs = 1;
			parse_subst_string(*str);
			noerrs = one;
			errflag = oef;
			remnulargs(*str);
			untokenize(*str);
		    }
		    break;
		}
	    }
	    if (rec < 0) {
		if (!strcmp(test, *str))
		    rec = 0;
		else
		    test = dupstring(*str);
	    }
	}
    }
}

/* get a directory stack entry */

/**/
static char *
dstackent(char ch, int val)
{
    int backwards;
    LinkNode end=(LinkNode)dirstack, n;

    backwards = ch == (isset(PUSHDMINUS) ? '+' : '-');
    if(!backwards && !val--)
	return pwd;
    if (backwards)
	for (n=lastnode(dirstack); n != end && val; val--, n=prevnode(n));
    else
	for (end=NULL, n=firstnode(dirstack); n && val; val--, n=nextnode(n));
    if (n == end) {
	if (backwards && !val)
	    return pwd;
	if (isset(NOMATCH))
	    zerr("not enough directory stack entries.", NULL, 0);
	return NULL;
    }
    return (char *)getdata(n);
}
