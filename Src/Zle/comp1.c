/*
 * comp1.c - base of the completion system
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

#include "comp1.mdh"

#include "comp1.pro"

/* default completion infos */
 
/**/
struct compctl cc_compos, cc_default, cc_first, cc_dummy;
 
/* hash table for completion info for commands */
 
/**/
HashTable compctltab;

/* Words on the command line, for use in completion */
 
/**/
int clwsize, clwnum, clwpos;
/**/
char **clwords;

/* != 0 if in a shell function called from completion, such that read -[cl]  *
 * will work (i.e., the line is metafied, and the above word arrays are OK). */

/**/
int incompctlfunc;

/**/
static void
createcompctltable(void)
{
    compctltab = newhashtable(23, "compctltab", NULL);

    compctltab->hash        = hasher;
    compctltab->emptytable  = emptyhashtable;
    compctltab->filltable   = NULL;
    compctltab->addnode     = addhashnode;
    compctltab->getnode     = gethashnode2;
    compctltab->getnode2    = gethashnode2;
    compctltab->removenode  = removehashnode;
    compctltab->disablenode = NULL;
    compctltab->enablenode  = NULL;
    compctltab->freenode    = freecompctlp;
    compctltab->printnode   = NULL;
}

/**/
static void
freecompctlp(HashNode hn)
{
    Compctlp ccp = (Compctlp) hn;

    zsfree(ccp->nam);
    freecompctl(ccp->cc);
    zfree(ccp, sizeof(struct compctlp));
}

/**/
void
freecompctl(Compctl cc)
{
    if (cc == &cc_default ||
 	cc == &cc_first ||
	cc == &cc_compos ||
	--cc->refc > 0)
	return;

    zsfree(cc->keyvar);
    zsfree(cc->glob);
    zsfree(cc->str);
    zsfree(cc->func);
    zsfree(cc->explain);
    zsfree(cc->ylist);
    zsfree(cc->prefix);
    zsfree(cc->suffix);
    zsfree(cc->hpat);
    zsfree(cc->subcmd);
    if (cc->cond)
	freecompcond(cc->cond);
    if (cc->ext) {
	Compctl n, m;

	n = cc->ext;
	do {
	    m = (Compctl) (n->next);
	    freecompctl(n);
	    n = m;
	}
	while (n);
    }
    if (cc->xor && cc->xor != &cc_default)
	freecompctl(cc->xor);
    zfree(cc, sizeof(struct compctl));
}

/**/
void
freecompcond(void *a)
{
    Compcond cc = (Compcond) a;
    Compcond and, or, c;
    int n;

    for (c = cc; c; c = or) {
	or = c->or;
	for (; c; c = and) {
	    and = c->and;
	    if (c->type == CCT_POS ||
		c->type == CCT_NUMWORDS) {
		free(c->u.r.a);
		free(c->u.r.b);
	    } else if (c->type == CCT_CURSUF ||
		       c->type == CCT_CURPRE) {
		for (n = 0; n < c->n; n++)
		    if (c->u.s.s[n])
			zsfree(c->u.s.s[n]);
		free(c->u.s.s);
	    } else if (c->type == CCT_RANGESTR ||
		       c->type == CCT_RANGEPAT) {
		for (n = 0; n < c->n; n++)
		    if (c->u.l.a[n])
			zsfree(c->u.l.a[n]);
		free(c->u.l.a);
		for (n = 0; n < c->n; n++)
		    if (c->u.l.b[n])
			zsfree(c->u.l.b[n]);
		free(c->u.l.b);
	    } else {
		for (n = 0; n < c->n; n++)
		    if (c->u.s.s[n])
			zsfree(c->u.s.s[n]);
		free(c->u.s.p);
		free(c->u.s.s);
	    }
	    zfree(c, sizeof(struct compcond));
	}
    }
}

/**/
int
compctlread(char *name, char **args, char *ops, char *reply)
{
    char *buf, *bptr;

    /* only allowed to be called for completion */
    if (!incompctlfunc) {
	zwarnnam(name, "option valid only in functions called for completion",
		NULL, 0);
	return 1;
    }

    if (ops['l']) {
	/* -ln gives the index of the word the cursor is currently on, which is
	available in cs (but remember that Zsh counts from one, not zero!) */
	if (ops['n']) {
	    char nbuf[14];

	    if (ops['e'] || ops['E'])
		printf("%d\n", cs + 1);
	    if (!ops['e']) {
		sprintf(nbuf, "%d", cs + 1);
		setsparam(reply, ztrdup(nbuf));
	    }
	    return 0;
	}
	/* without -n, the current line is assigned to the given parameter as a
	scalar */
	if (ops['e'] || ops['E']) {
	    zputs((char *) line, stdout);
	    putchar('\n');
	}
	if (!ops['e'])
	    setsparam(reply, ztrdup((char *) line));
    } else {
	int i;

	/* -cn gives the current cursor position within the current word, which
	is available in clwpos (but remember that Zsh counts from one, not
	zero!) */
	if (ops['n']) {
	    char nbuf[14];

	    if (ops['e'] || ops['E'])
		printf("%d\n", clwpos + 1);
	    if (!ops['e']) {
		sprintf(nbuf, "%d", clwpos + 1);
		setsparam(reply, ztrdup(nbuf));
	    }
	    return 0;
	}
	/* without -n, the words of the current line are assigned to the given
	parameters separately */
	if (ops['A'] && !ops['e']) {
	    /* the -A option means that one array is specified, instead of
	    many parameters */
	    char **p, **b = (char **)zcalloc((clwnum + 1) * sizeof(char *));

	    for (i = 0, p = b; i < clwnum; p++, i++)
		*p = ztrdup(clwords[i]);

	    setaparam(reply, b);
	    return 0;
	}
	if (ops['e'] || ops['E']) {
	    for (i = 0; i < clwnum; i++) {
		zputs(clwords[i], stdout);
		putchar('\n');
	    }

	    if (ops['e'])
		return 0;
	}

	for (i = 0; i < clwnum && *args; reply = *args++, i++)
	    setsparam(reply, ztrdup(clwords[i]));

	if (i < clwnum) {
	    int j, len;

	    for (j = i, len = 0; j < clwnum; len += strlen(clwords[j++]));
	    bptr = buf = zalloc(len + j - i);
	    while (i < clwnum) {
		strucpy(&bptr, clwords[i++]);
		*bptr++ = ' ';
	    }
	    bptr[-1] = '\0';
	} else
	    buf = ztrdup("");
	setsparam(reply, buf);
    }
    return 0;
}

/**/
int
boot_comp1(Module m)
{
    compctlreadptr = compctlread;
    clwords = (char **) zcalloc((clwsize = 16) * sizeof(char *));
    createcompctltable();
    cc_compos.mask = CC_COMMPATH;
    cc_default.refc = 10000;
    cc_default.mask = CC_FILES;
    cc_first.refc = 10000;
    cc_first.mask = 0;
    return 0;
}

#ifdef MODULE

/**/
int
cleanup_comp1(Module m)
{
    deletehashtable(compctltab);
    zfree(clwords, clwsize * sizeof(char *));
    compctlreadptr = fallback_compctlread;
    return 0;
}

#endif /* MODULE */
