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
 * Bits 0 and 1 of flags are used in filesub.
 * bit 0 is set when we are doing MAGIC_EQUALSUBST or normal
 *	 assignment but not a typeset.
 * bit 1 is set on a real assignment (both typeset and normal).
 * bit 2 is a flag to paramsubst (single word sub)
 */

/**/
void
prefork(LinkList list, int flags)
{
    LinkNode node;

    MUSTUSEHEAP("prefork");
    for (node = firstnode(list); node; incnode(node)) {
	char *str, *str3;

	str = str3 = (char *)getdata(node);
	if ((*str == Inang || *str == Outang || *str == Equals) &&
	    str[1] == Inpar) {
	    if (*str == Inang || *str == Outang)
		setdata(node, (void *) getproc(str));	/* <(...) or >(...) */
	    else
		setdata(node, (void *) getoutputfile(str));	/* =(...) */
	    if (!getdata(node))
		return;
	} else {
	    if (isset(SHFILEEXPANSION))
		filesub((char **)getaddrdata(node), flags & 3);
	    if (!(node = stringsubst(list, node, flags & 4)))
		return;
	}
    }
    for (node = firstnode(list); node; incnode(node)) {
	if (*(char *)getdata(node)) {
	    remnulargs(getdata(node));
	    if (unset(IGNOREBRACES) && !(flags & 4))
		while (hasbraces(getdata(node)))
		    xpandbraces(list, &node);
	    if (unset(SHFILEEXPANSION))
		filesub((char **)getaddrdata(node), flags & 3);
	} else if (!(flags & 4))
	    uremnode(list, node);
	if (errflag)
	    return;
    }
}

/**/
static LinkNode
stringsubst(LinkList list, LinkNode node, int ssub)
{
    int qt;
    char *str3 = (char *)getdata(node);
    char *str  = str3;

    while (!errflag && *str) {
	if ((qt = *str == Qstring) || *str == String)
	    if (str[1] == Inpar) {
		str++;
		goto comsub;
	    } else if (str[1] == Inbrack) {
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
	    } else if (str[1] == Snull) {
		str = getkeystring(str, NULL, 4, NULL);
		continue;
	    } else {
		node = paramsubst(list, node, &str, qt, ssub);
		if (errflag || !node)
		    return NULL;
		str3 = (char *)getdata(node);
		continue;
	    }
	else if ((qt = *str == Qtick) || *str == Tick)
	  comsub: {
	    LinkList pl;
	    char *s, *str2 = str;
	    char endchar;
	    int l1, l2;

	    if (*str == Inpar) {
		endchar = Outpar;
		str[-1] = '\0';
		if (skipparens(Inpar, Outpar, &str))
		    DPUTS(1, "BUG: parse error in command substitution");
		str--;
	    } else {
		endchar = *str;
		*str = '\0';

		while (*++str != endchar)
		    DPUTS(!*str, "BUG: parse error in command substitution");
	    }
	    *str++ = '\0';
	    if (endchar == Outpar && str2[1] == '(' && str[-2] == ')') {
		/* Math substitution of the form $((...)) */
		str = arithsubst(str2 + 1, &str3, str);
		setdata(node, (void *) str3);
		continue;
	    }

	    /* It is a command substitution, which will be parsed again   *
	     * by the lexer, so we untokenize it first, but we cannot use *
	     * untokenize() since in the case of `...` some Bnulls should *
	     * be left unchanged.  Note that the lexer doesn't tokenize   *
	     * the body of a command substitution so if there are some    *
	     * tokens here they are from a ${(e)~...} substitution.       */
	    for (str = str2; *++str; )
		if (itok(*str) && *str != Nularg &&
		    !(endchar != Outpar && *str == Bnull &&
		      (str[1] == '$' || str[1] == '\\' || str[1] == '`' ||
		       (qt && str[1] == '"'))))
		    *str = ztokens[*str - Pound];
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
		tokenize(s);
	    l1 = str2 - str3;
	    l2 = strlen(s);
	    if (nonempty(pl)) {
		LinkNode n = lastnode(pl);
		str2 = (char *) ncalloc(l1 + l2 + 1);
		strcpy(str2, str3);
		strcpy(str2 + l1, s);
		setdata(node, str2);
		insertlinklist(pl, node, list);
		s = (char *) getdata(node = n);
		l1 = 0;
		l2 = strlen(s);
	    }
	    str2 = (char *) ncalloc(l1 + l2 + strlen(str) + 1);
	    if (l1)
		strcpy(str2, str3);
	    strcpy(str2 + l1, s);
	    str = strcpy(str2 + l1 + l2, str);
	    str3 = str2;
	    setdata(node, str3);
	    continue;
	}
	str++;
    }
    return errflag ? NULL : node;
}

/**/
void
globlist(LinkList list)
{
    LinkNode node, next;

    badcshglob = 0;
    for (node = firstnode(list); !errflag && node; node = next) {
	next = nextnode(node);
	glob(list, node);
    }
    if (badcshglob == 1)
	zerr("no match", NULL, 0);
}

/* perform substitution on a single word */

/**/
void
singsub(char **s)
{
    LinkList foo;

    foo = newlinklist();
    addlinknode(foo, *s);
    prefork(foo, 4);
    if (errflag)
	return;
    *s = (char *) ugetnode(foo);
    DPUTS(nonempty(foo), "BUG: singsub() produced more than one word!");
}

/* Perform substitution on a single word. Unlike with singsub, the      *
 * result can have more than one words. A single word result is sroted  *
 * in *s and *isarr is set to zero; otherwise *isarr is set to 1 and    *
 * the result is stored in *a. If `a' is zero a multiple word result is *
 * joined using sep or the IFS parameter if sep is zero and the result  *
 * is returned in *s.  The return value is true iff the expansion       *
 * resulted in an empty list                                            */

/**/
static int
multsub(char **s, char ***a, int *isarr, char *sep)
{
    LinkList foo;
    int l;
    char **r, **p;

    foo = newlinklist();
    addlinknode(foo, *s);
    prefork(foo, 0);
    if (errflag) {
	if (isarr)
	    *isarr = 0;
	return 0;
    }
    if ((l = countlinknodes(foo)) > 1) {
	p = r = ncalloc((l + 1) * sizeof(char*));
	while (nonempty(foo))
	    *p++ = (char *)ugetnode(foo);
	*p = NULL;
	if (a) {
	    *a = r;
	    *isarr = 1;
	    return 0;
	}
	*s = sepjoin(r, NULL);
	return 0;
    }
    if (l)
	*s = (char *) ugetnode(foo);
    else
	*s = dupstring("");
    if (isarr)
	*isarr = 0;
    return !l;
}

/* ~, = subs: assign = 2 => typeset; assign = 1 => something that looks
	like an assignment but may not be; assign = 3 => normal assignment */

/**/
void
filesub(char **namptr, int assign)
{
    char *sub = NULL, *str, *ptr;
    int len;

    filesubstr(namptr, assign);

    if (!assign)
	return;

    if (assign < 3)
	if ((*namptr)[1] && (sub = strchr(*namptr + 1, Equals))) {
	    if (assign == 1)
		for (ptr = *namptr; ptr != sub; ptr++)
		    if (!iident(*ptr) && !INULL(*ptr))
			return;
	    str = sub + 1;
	    if ((sub[1] == Tilde || sub[1] == Equals) && filesubstr(&str, assign)) {
		sub[1] = '\0';
		*namptr = dyncat(*namptr, str);
	    }
	} else
	    return;

    ptr = *namptr;
    while ((sub = strchr(ptr, ':'))) {
	str = sub + 1;
	len = sub - *namptr;
	if ((sub[1] == Tilde || sub[1] == Equals) && filesubstr(&str, assign)) {
	    sub[1] = '\0';
	    *namptr = dyncat(*namptr, str);
	}
	ptr = *namptr + len + 1;
    }
}

/**/
int
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
	if (!(cnam = findcmd(str + 1))) {
	    Alias a = (Alias) aliastab->getnode(aliastab, str + 1);
	    
	    if (a)
		cnam = ztrdup(a->text);
	    else {
		if (isset(NOMATCH))
		    zerr("%s not found", str + 1, 0);
		return 0;
	    }
	}
	*namptr = dupstring(cnam);
	zsfree(cnam);
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
strcatsub(char **d, char *pb, char *pe, char *src, int l, char *s, int glbsub)
{
    int pl = pe - pb;
    char *dest = ncalloc(pl + l + (s ? strlen(s) : 0) + 1);

    *d = dest;
    strncpy(dest, pb, pl);
    dest += pl;
    strcpy(dest, src);
    if (glbsub)
	tokenize(dest);
    dest += l;
    if (s)
	strcpy(dest, s);
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

    r = ret = (char *)halloc(lr + 1);

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
    long ret;

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
    ret = matheval(p);
    if (errflag)
	return -1;
    if (ret < 0)
	ret = -ret;
    return ret < 0 ? -ret : ret;
}

/* parameter substitution */

#define	isstring(c) ((c) == '$' || (char)(c) == String || (char)(c) == Qstring)
#define isbrack(c)  ((c) == '[' || (char)(c) == Inbrack)

/**/
LinkNode
paramsubst(LinkList l, LinkNode n, char **str, int qt, int ssub)
{
    char *aptr = *str;
    char *s = aptr, *fstr, *idbeg, *idend, *ostr = (char *) getdata(n);
    int colf;			/* != 0 means we found a colon after the name */
    int doub = 0;		/* != 0 means we have %%, not %, or ##, not # */
    int isarr = 0;
    int plan9 = isset(RCEXPANDPARAM);
    int globsubst = isset(GLOBSUBST);
    int getlen = 0;
    int whichlen = 0;
    int chkset = 0;
    int vunset = 0;
    int spbreak = isset(SHWORDSPLIT) && !ssub && !qt;
    char *val = NULL, **aval = NULL;
    unsigned int fwidth = 0;
    Value v;
    int flags = 0;
    int flnum = 0;
    int substr = 0;
    int sortit = 0, casind = 0;
    int casmod = 0;
    char *sep = NULL, *spsep = NULL;
    char *premul = NULL, *postmul = NULL, *preone = NULL, *postone = NULL;
    long prenum = 0, postnum = 0;
    int copied = 0;
    int arrasg = 0;
    int eval = 0;
    int nojoin = 0;
    char inbrace = 0;		/* != 0 means ${...}, otherwise $... */

    *s++ = '\0';
    if (!ialnum(*s) && *s != '#' && *s != Pound && *s != '-' &&
	*s != '!' && *s != '$' && *s != String && *s != Qstring &&
	*s != '?' && *s != Quest && *s != '_' &&
	*s != '*' && *s != Star && *s != '@' && *s != '{' &&
	*s != Inbrace && *s != '=' && *s != Equals && *s != Hat &&
	*s != '^' && *s != '~' && *s != Tilde && *s != '+') {
	s[-1] = '$';
	*str = s;
	return n;
    }
    DPUTS(*s == '{', "BUG: inbrace == '{' in paramsubst()");
    if (*s == Inbrace) {
	inbrace = 1;
	s++;
	if (*s == '(' || *s == Inpar) {
	    char *t, sav;
	    int tt = 0;
	    long num;
	    int escapes = 0;
	    int klen;
#define UNTOK_AND_ESCAPE(X) {\
		untokenize(X = dupstring(s + 1));\
		if (escapes) {\
		    X = getkeystring(X, &klen, 3, NULL);\
		    X = metafy(X, klen, META_HREALLOC);\
		}\
	    }

	    for (s++; *s != ')' && *s != Outpar; s++, tt = 0) {
		switch (*s) {
		case ')':
		case Outpar:
		    break;
		case 'A':
		    arrasg = 1;
		    break;
		case '@':
		    nojoin = 1;
		    break;
		case 'M':
		    flags |= 8;
		    break;
		case 'R':
		    flags |= 16;
		    break;
		case 'B':
		    flags |= 32;
		    break;
		case 'E':
		    flags |= 64;
		    break;
		case 'N':
		    flags |= 128;
		    break;
		case 'S':
		    substr = 1;
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
		case 'e':
		    eval = 1;
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
		    if (s[1] != sav)
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
		    if (*s != sav) {
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

		default:
		  flagerr:
		    zerr("error in flags", NULL, 0);
		    return NULL;
		}
	    }
	    s++;
	}
    }
    if (sortit)
	sortit += (casind << 1);

    if (!premul)
	premul = " ";
    if (!postmul)
	postmul = " ";

    for (;;) {
	if (*s == '^' || *s == Hat) {
	    if (*++s == '^' || *s == Hat) {
		plan9 = 0;
		s++;
	    } else
		plan9 = 1;
	} else if (*s == '=' || *s == Equals) {
	    if (*++s == '=' || *s == Equals) {
		spbreak = 0;
		s++;
	    } else
		spbreak = 1;
	} else if ((*s == '#' || *s == Pound) &&
		   (iident(s[1])
		    || s[1] == '*' || s[1] == Star || s[1] == '@'
		    || (isstring(s[1]) && (s[2] == Inbrace || s[2] == Inpar))))
	    getlen = 1 + whichlen, s++;
	else if (*s == '~' || *s == Tilde) {
	    if (*++s == '~' || *s == Tilde) {
		globsubst = 0;
		s++;
	    } else
		globsubst = 1;
	} else if (*s == '+')
	    if (iident(s[1]))
		chkset = 1, s++;
	    else if (!inbrace) {
		*aptr = '$';
		*str = aptr + 1;
		return n;
	    } else {
		zerr("bad substitution", NULL, 0);
		return NULL;
	    }
	else
	    break;
    }
    globsubst = globsubst && !qt;

    idbeg = s;
    if (s[-1] && isstring(*s) && (s[1] == Inbrace || s[1] == Inpar)) {
	int sav;
	int quoted = *s == Qstring;

	val = s++;
	skipparens(*s, *s == Inpar ? Outpar : Outbrace, &s);
	sav = *s;
	*s = 0;
	if (multsub(&val, &aval, &isarr, NULL) && quoted) {
	    isarr = -1;
	    aval = alloc(sizeof(char *));
	}
	if (isarr)
	    isarr = -1;
	copied = 1;
	*s = sav;
	v = (Value) NULL;
    } else if (!(v = getvalue(&s, (unset(KSHARRAYS) || inbrace) ? 1 : -1)))
	vunset = 1;
    while (v || ((inbrace || (unset(KSHARRAYS) && vunset)) && isbrack(*s))) {
	if (!v) {
	    Param pm;
	    char *os = s;

	    if (!isbrack(*s))
		break;
	    if (vunset) {
		val = dupstring("");
		isarr = 0;
	    }
	    pm = createparam(nulstring, isarr ? PM_ARRAY : PM_SCALAR);
	    if (isarr)
		pm->u.arr = aval;
	    else
		pm->u.str = val;
	    v = (Value) hcalloc(sizeof *v);
	    v->isarr = isarr;
	    v->pm = pm;
	    v->b = -1;
	    if (getindex(&s, v) || s == os)
		break;
	}
	if ((isarr = v->isarr))
	    aval = getarrvalue(v);
	else {
	    if (v->pm->flags & PM_ARRAY) {
		int tmplen = arrlen(v->pm->gets.afn(v->pm));

		if (v->a < 0)
		    v->a += tmplen + v->inv;
		if (!v->inv && (v->a >= tmplen || v->a < 0))
		    vunset = 1;
	    }
	    if (!vunset) {
		val = getstrvalue(v);
		fwidth = v->pm->ct ? v->pm->ct : strlen(val);
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
		    val = (char *)ncalloc(fwidth + 1);
		    val[fwidth] = '\0';
		    if ((t0 = strlen(t)) > fwidth)
			t0 = fwidth;
		    memset(val, ' ', fwidth);
		    strncpy(val, t, t0);
		    break;
		case PM_RIGHT_B:
		case PM_RIGHT_Z:
		case PM_RIGHT_Z | PM_RIGHT_B:
		    if (strlen(val) < fwidth) {
			t = (char *)ncalloc(fwidth + 1);
			memset(t, (v->pm->flags & PM_RIGHT_B) ? ' ' : '0', fwidth);
			if ((t0 = strlen(val)) > fwidth)
			    t0 = fwidth;
			strcpy(t + (fwidth - t0), val);
			val = t;
		    } else {
			t = (char *)ncalloc(fwidth + 1);
			t[fwidth] = '\0';
			strncpy(t, val + strlen(val) - fwidth, fwidth);
			val = t;
		    }
		    break;
		}
		switch (v->pm->flags & (PM_LOWER | PM_UPPER)) {
		    char *t;

		case PM_LOWER:
		    t = val;
		    for (; *t; t++)
			*t = tulower(*t);
		    break;
		case PM_UPPER:
		    t = val;
		    for (; *t; t++)
			*t = tuupper(*t);
		    break;
		}
	    }
	}
	v = NULL;
	if (!inbrace)
	    break;
    }
    if (isarr) {
	if (nojoin)
	    isarr = -1;
	if (qt && !getlen && isarr > 0) {
	    val = sepjoin(aval, sep);
	    isarr = 0;
	}
    }

    idend = s;
    if ((colf = *s == ':'))
	s++;


    /* fstr is to be the text following the substitution.  If we have *
     * braces, we look for it here, else we infer it later on.        */
    fstr = s;
    if (inbrace) {
	int bct;
	for (bct = 1;; fstr++) {
	    if (!*fstr)
		break;
	    else if (*fstr == Inbrace)
		bct++;
	    else if (*fstr == Outbrace && !--bct)
		break;
	}

	if (bct) {
	noclosebrace:
	    zerr("closing brace expected", NULL, 0);
	    return NULL;
	}
	if (*fstr)
	    *fstr++ = '\0';
    }

    /* Check for ${..?..} or ${..=..} or one of those. *
     * Only works if the name is in braces.            */

    if (inbrace && (*s == '-' ||
		    *s == '+' ||
		    *s == ':' ||
		    *s == '=' || *s == Equals ||
		    *s == '%' ||
		    *s == '#' || *s == Pound ||
		    *s == '?' || *s == Quest)) {

	if (!flnum)
	    flnum++;
	if (*s == '%')
	    flags |= 1;

	/* Check for ${..%%..} or ${..##..} */
	if ((*s == '%' || *s == '#' || *s == Pound) && *s == s[1]) {
	    s++;
	    doub = 1;
	}
	s++;

	flags |= (doub << 1) | (substr << 2) | (colf << 8);
	if (!(flags & 0xf8))
	    flags |= 16;

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
		multsub(&val, &aval, &isarr, NULL);
		copied = 1;
	    }
	    break;
	case ':':
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
		if (spsep || spbreak || !arrasg)
		    multsub(&val, NULL, NULL, sep);
		else
		    multsub(&val, &aval, &isarr, NULL);
		if (arrasg) {
		    char *arr[2], **t, **a, **p;
		    if (spsep || spbreak) {
			aval = sepsplit(val, spsep, 0);
			isarr = 2;
			sep = spsep = NULL;
			spbreak = 0;
			l = arrlen(aval);
			if (l && !*(aval[l-1]))
			    l--;
			if (l && !**aval)
			    l--, t = aval + 1;
			else
			    t = aval;
		    } else if (!isarr) {
			arr[0] = val;
			arr[1] = NULL;
			t = aval = arr;
			l = 1;
		    } else
			l = arrlen(aval), t = aval;
		    p = a = zalloc(sizeof(char *) * (l + 1));
		    while (l--) {
			untokenize(*t);
			*p++ = ztrdup(*t++);
		    }
		    *p++ = NULL;
		    setaparam(idbeg, a);
		} else {
		    untokenize(val);
		    setsparam(idbeg, ztrdup(val));
		}
		*idend = sav;
		copied = 1;
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
	    if (qt)
		if (parse_subst_string(s)) {
		    zerr("parse error in ${...%c...} substitution",
			 NULL, s[-1]);
		    return NULL;
		}
	    singsub(&s);

	    if (!vunset && isarr) {
		char **ap = aval;
		char **pp = aval = (char **)ncalloc(sizeof(char *) * (arrlen(aval) + 1));

		while ((*pp = *ap++)) {
		    if (getmatch(pp, s, flags, flnum))
			pp++;
		}
		copied = 1;
	    } else {
		if (vunset)
		    val = dupstring("");
		getmatch(&val, s, flags, flnum);
		copied = 1;
	    }
	    break;
	}
    } else {			/* no ${...=...} or anything, but possible modifiers. */
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
	    s--;
	    if (unset(KSHARRAYS) || inbrace) {
		if (!isarr)
		    modify(&val, &s);
		else {
		    char *ss;
		    char **ap = aval;
		    char **pp = aval = (char **)ncalloc(sizeof(char *) * (arrlen(aval) + 1));

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
    if (isarr > 0 && !plan9 && (!aval || !aval[0])) {
	val = dupstring("");
	isarr = 0;
    } else if (isarr && aval && aval[0] && !aval[1]) {
	val = aval[0];
	isarr = 0;
    }
    /* ssub is true when we are called from singsub (via prefork).
     * It means that we must join arrays and should not split words. */
    if (ssub || spbreak || spsep || sep) {
	if (isarr)
	    val = sepjoin(aval, sep), isarr = 0;
	if (!ssub && (spbreak || spsep)) {
	    aval = sepsplit(val, spsep, 0);
	    if (!aval || !aval[0])
		val = dupstring("");
	    else if (!aval[1])
		val = aval[0];
	    else
		isarr = 2;
	}
    }
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
    if (isarr) {
	char *x;
	char *y;
	int xlen;
	int i;
	LinkNode on = n;

	if (!aval[0] && !plan9) {
	    if (aptr > (char *) getdata(n) &&
		aptr[-1] == Dnull && *fstr == Dnull)
		*--aptr = '\0', fstr++;
	    y = (char *)ncalloc((aptr - ostr) + strlen(fstr) + 1);
	    strcpy(y, ostr);
	    *str = y + (aptr - ostr);
	    strcpy(*str, fstr);
	    setdata(n, y);
	    return n;
	}
	if (sortit) {
	    static CompareFn sortfn[] = {
		strpcmp, invstrpcmp, cstrpcmp, invcstrpcmp
	    };

	    if (!copied)
		aval = arrdup(aval);

	    i = arrlen(aval);
	    if (i && (*aval[i-1] || --i))
		qsort(aval, i, sizeof(char *), sortfn[sortit-1]);
	}
	if (plan9) {
	    LinkList tl = newlinklist();
	    LinkNode tn;

	    *--fstr = Marker;
	    addlinknode(tl, fstr);
	    if (!eval && !stringsubst(tl, firstnode(tl), ssub))
		return NULL;
	    *str = aptr;
	    tn = firstnode(tl);
	    while ((x = *aval++)) {
		if (prenum || postnum)
		    x = dopadding(x, prenum, postnum, preone, postone,
				  premul, postmul);
		if (eval && parsestr(x))
		    return NULL;
		xlen = strlen(x);
		for (tn = firstnode(tl);
		     tn && *(y = (char *) getdata(tn)) == Marker;
		     incnode(tn)) {
		    strcatsub(&y, ostr, aptr, x, xlen, y + 1, globsubst);
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
	    x = aval[0];
	    if (prenum || postnum)
		x = dopadding(x, prenum, postnum, preone, postone,
			      premul, postmul);
	    if (eval && parsestr(x))
		return NULL;
	    xlen = strlen(x);
	    strcatsub(&y, ostr, aptr, x, xlen, NULL, globsubst);
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
		if (eval && parsestr(x))
		    return NULL;
		if (qt && !*x && isarr != 2)
		    y = dupstring(nulstring);
		else {
		    y = dupstring(x);
		    if (globsubst)
			tokenize(y);
		}
		insertlinknode(l, n, (void *) y), incnode(n);
	    }

	    x = aval[i];
	    if (prenum || postnum)
		x = dopadding(x, prenum, postnum, preone, postone,
			      premul, postmul);
	    if (eval && parsestr(x))
		return NULL;
	    xlen = strlen(x);
	    *str = strcatsub(&y, aptr, aptr, x, xlen, fstr, globsubst);
	    if (qt && !*y && isarr != 2)
		y = dupstring(nulstring);
	    insertlinknode(l, n, (void *) y), incnode(n);
	}
	if (eval)
	    n = on;
    } else {
	int xlen;
	char *x;
	char *y;

	x = val;
	if (prenum || postnum)
	    x = dopadding(x, prenum, postnum, preone, postone,
			  premul, postmul);
	if (eval && parsestr(x))
	    return NULL;
	xlen = strlen(x);
	*str = strcatsub(&y, ostr, aptr, x, xlen, fstr, globsubst);
	if (qt && !*y && isarr != 2)
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
    char *s = *bptr, *t, buf[DIGBUFSIZE];
    char *b = buf;
    long v;

    singsub(&a);
    v = matheval(a);
    sprintf(buf, "%ld", v);
    t = *bptr = (char *)ncalloc(strlen(*bptr) + strlen(buf) + strlen(rest) + 1);
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
		    }
		    tc = *tt;
		    *tt = '\0';
		    nl = al + strlen(t) + strlen(copy);
		    ptr1 = tmp = (char *)halloc(nl + 1);
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
	if (isset(NOMATCH))
	    zerr("not enough directory stack entries.", NULL, 0);
	return NULL;
    }
    return (char *)getdata(n);
}
