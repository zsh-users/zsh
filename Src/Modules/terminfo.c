/*
 * terminfo.c - parameter interface to terminfo via curses
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 2000 Sven Wishnowsky, Clint Adams
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Sven Wishnowsky, Clint Adams or the Zsh Development Group
 * be liable to any party for direct, indirect, special, incidental, or
 * consequential damages arising out of the use of this software and its
 * documentation, even if Sven Wishnowsky, Clint Adams and the Zsh
 * Development Group have been advised of the possibility of such damage.
 *
 * Sven Wishnowsky, Clint Adams and the Zsh Development Group specifically
 * disclaim any warranties, including, but not limited to, the implied
 * warranties of merchantability and fitness for a particular purpose.
 * The software provided hereunder is on an "as is" basis, and Sven
 * Wishnowsky, Clint Adams and the Zsh Development Group have no obligation
 * to provide maintenance, support, updates, enhancements, or modifications.
 *
 */

#include "terminfo.mdh"
#include "terminfo.pro"

static char terminfo_nam[] = "terminfo";
static Param terminfo_pm;

/* echoti: output a terminfo capability */

#ifdef HAVE_TIGETSTR

/**/
static int
bin_echoti(char *name, char **argv, char *ops, int func)
{
    char *s, *t;
    int num;

    s = *argv++;
    /* This depends on the termcap stuff in init.c */
    if (termflags & TERM_BAD)
	return 1;
    if ((termflags & TERM_UNKNOWN) && (isset(INTERACTIVE) || !init_term()))
	return 1;
    /* if the specified capability has a numeric value, display it */
    if (((num = tigetnum(s)) != -1) && (num != -2)) {
	printf("%d\n", num);
	return 0;
    }

    switch (tigetflag(s)) {
    case -1:
	break;
    case 0:
	puts("no");
	return 0;
    default:
	puts("yes");
	return 0;
    }

/* get a string-type capability */
    t = (char *)tigetstr(s);
    if (!t || t == (char *)-1 || !*t) {
	/* capability doesn't exist, or (if boolean) is off */
	zwarnnam(name, "no such terminfo capability: %s", s, 0);
	return 1;
    }

    tputs(t, 1, putchar);
    return 0;
}

#else

#define bin_echoti bin_notavail

#endif

static struct builtin bintab[] = {
    BUILTIN("echoti", 0, bin_echoti, 1, -1, 0, NULL, NULL),
};

/* This says if we are cleaning up when the module is unloaded. */

static int incleanup;

#ifdef HAVE_TIGETSTR

/* Empty dummy function for special hash parameters. */

/**/
static void
shempty(void)
{
}

/* Create a simple special hash parameter. */

/**/
static Param
createtihash()
{
    Param pm;
    HashTable ht;

    unsetparam(terminfo_nam);

    if (!(pm = createparam(terminfo_nam, PM_SPECIAL|PM_HIDE|PM_HIDEVAL|
			   PM_REMOVABLE|PM_HASHED)))
	return NULL;

    pm->level = pm->old ? locallevel : 0;
    pm->gets.hfn = hashgetfn;
    pm->sets.hfn = hashsetfn;
    pm->unsetfn = stdunsetfn;
    pm->u.hash = ht = newhashtable(7, terminfo_nam, NULL);

    ht->hash        = hasher;
    ht->emptytable  = (TableFunc) shempty;
    ht->filltable   = NULL;
    ht->addnode     = (AddNodeFunc) shempty;
    ht->getnode     = ht->getnode2 = getterminfo;
    ht->removenode  = (RemoveNodeFunc) shempty;
    ht->disablenode = NULL;
    ht->enablenode  = NULL;
    ht->freenode    = (FreeNodeFunc) shempty;
    ht->printnode   = printparamnode;
    ht->scantab     = scanterminfo;

    return (terminfo_pm = pm);
}

/**/
static HashNode
getterminfo(HashTable ht, char *name)
{
    int len, num;
    char *tistr;
    Param pm = NULL;

    /* This depends on the termcap stuff in init.c */
    if (termflags & TERM_BAD)
	return NULL;
    if ((termflags & TERM_UNKNOWN) && (isset(INTERACTIVE) || !init_term()))
	return NULL;

    unmetafy(name, &len);

    pm = (Param) zhalloc(sizeof(struct param));
    pm->nam = dupstring(name);
    pm->flags = PM_READONLY;
    pm->sets.cfn = NULL;
    pm->gets.cfn = strgetfn;
    pm->unsetfn = NULL;
    pm->ct = 0;
    pm->env = NULL;
    pm->ename = NULL;
    pm->old = NULL;
    pm->level = 0;

    if (((num = tigetnum(name)) != -1) && (num != -2)) {
	pm->u.val = num;
	pm->flags |= PM_INTEGER;
    }
    else if ((num = tigetflag(name)) != -1) {
	pm->u.str = num ? dupstring("yes") : dupstring("no");
	pm->flags |= PM_SCALAR;
    }
    else if ((tistr = (char *)tigetstr(name)) != NULL && tistr != (char *)-1)
    {
	pm->u.str = dupstring(tistr);
	pm->flags |= PM_SCALAR;
    }
    else
    {
	/* zwarn("no such capability: %s", name, 0); */
	pm->u.str = dupstring("");
	pm->flags |= PM_UNSET;
    }
    return (HashNode) pm;
}

/**/
static void
scanterminfo(HashTable ht, ScanFunc func, int flags)
{
}

#endif /* HAVE_TIGETSTR */

/**/
int
setup_(Module m)
{
    incleanup = 0;

    return 0;
}

/**/
int
boot_(Module m)
{
#ifdef HAVE_TIGETSTR
    setupterm((char *)0, 1, (int *)0);

    if (!createtihash())
    	return 1;
#endif
    return  !addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
}

/**/
int
cleanup_(Module m)
{
    Param pm;

    incleanup = 1;

#ifdef HAVE_TIGETSTR
    if ((pm = (Param) paramtab->getnode(paramtab, terminfo_nam)) &&
	pm == terminfo_pm) {
	pm->flags &= ~PM_READONLY;
	unsetparam_pm(pm, 0, 1);
    }
#endif
    deletebuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
    return 0;
}

/**/
int
finish_(Module m)
{
    return 0;
}
