/*
 * zleparameter.c - parameter interface to zle internals
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

#include "zleparameter.mdh"
#include "zleparameter.pro"

/* Empty dummy function for special hash parameters. */

/**/
static void
shempty(void)
{
}

/* Create a simple special hash parameter. */

/**/
static Param
createspecialhash(char *name, GetNodeFunc get, ScanTabFunc scan)
{
    Param pm;
    HashTable ht;

    if (!(pm = createparam(name, PM_SPECIAL|PM_HIDE|PM_REMOVABLE|PM_HASHED)))
	return NULL;

    pm->level = pm->old ? locallevel : 0;
    pm->gets.hfn = hashgetfn;
    pm->sets.hfn = hashsetfn;
    pm->unsetfn = stdunsetfn;
    pm->u.hash = ht = newhashtable(0, name, NULL);
    pm->ct = 0;

    ht->hash        = hasher;
    ht->emptytable  = (TableFunc) shempty;
    ht->filltable   = NULL;
    ht->addnode     = (AddNodeFunc) shempty;
    ht->getnode     = ht->getnode2 = get;
    ht->removenode  = (RemoveNodeFunc) shempty;
    ht->disablenode = NULL;
    ht->enablenode  = NULL;
    ht->freenode    = (FreeNodeFunc) shempty;
    ht->printnode   = printparamnode;
    ht->scantab     = scan;

    return pm;
}

/* Functions for the zlewidgets special parameter. */

/**/
static char *
widgetstr(Widget w)
{
    if (w->flags & WIDGET_INT)
	return dupstring("builtin");
    if (w->flags & WIDGET_NCOMP) {
	char *t = (char *) zhalloc(13 + strlen(w->u.comp.wid) +
				   strlen(w->u.comp.func));

	strcpy(t, "completion:");
	strcat(t, w->u.comp.wid);
	strcat(t, ":");
	strcat(t, w->u.comp.func);

	return t;
    }
    return dyncat("user:", w->u.fnnam);
}

/**/
static HashNode
getpmwidgets(HashTable ht, char *name)
{
    Param pm = NULL;
    Thingy th;

    pm = (Param) zhalloc(sizeof(struct param));
    pm->nam = dupstring(name);
    pm->flags = PM_SCALAR | PM_READONLY;
    pm->sets.cfn = NULL;
    pm->gets.cfn = strgetfn;
    pm->unsetfn = NULL;
    pm->ct = 0;
    pm->env = NULL;
    pm->ename = NULL;
    pm->old = NULL;
    pm->level = 0;
    if ((th = (Thingy) thingytab->getnode(thingytab, name)) &&
	!(th->flags & DISABLED))
	pm->u.str = widgetstr(th->widget);
    else {
	pm->u.str = dupstring("");
	pm->flags |= PM_UNSET;
    }
    return (HashNode) pm;
}

/**/
static void
scanpmwidgets(HashTable ht, ScanFunc func, int flags)
{
    struct param pm;
    int i;
    HashNode hn;

    pm.flags = PM_SCALAR | PM_READONLY;
    pm.sets.cfn = NULL;
    pm.gets.cfn = strgetfn;
    pm.unsetfn = NULL;
    pm.ct = 0;
    pm.env = NULL;
    pm.ename = NULL;
    pm.old = NULL;
    pm.level = 0;

    for (i = 0; i < thingytab->hsize; i++)
	for (hn = thingytab->nodes[i]; hn; hn = hn->next) {
	    pm.nam = hn->nam;
	    if (func != scancountparams &&
		((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		 !(flags & SCANPM_WANTKEYS)))
		pm.u.str = widgetstr(((Thingy) hn)->widget);
	    func((HashNode) &pm, flags);
	}
}

/* Functions for the zlekeymaps special parameter. */

static char **
keymapsgetfn(Param pm)
{
    int i;
    HashNode hn;
    char **ret, **p;

    p = ret = (char **) zhalloc((keymapnamtab->ct + 1) * sizeof(char *));

    for (i = 0; i < keymapnamtab->hsize; i++)
	for (hn = keymapnamtab->nodes[i]; hn; hn = hn->next)
	    *p++ = dupstring(hn->nam);
    *p = NULL;

    return ret;
}

/* Table for defined parameters. */

struct pardef {
    char *name;
    int flags;
    GetNodeFunc getnfn;
    ScanTabFunc scantfn;
    void (*hsetfn) _((Param, HashTable));
    void (*setfn) _((Param, char **));
    char **(*getfn) _((Param));
    void (*unsetfn) _((Param, int));
    Param pm;
};

static struct pardef partab[] = {
    { "widgets", PM_READONLY,
      getpmwidgets, scanpmwidgets, hashsetfn,
      NULL, NULL, stdunsetfn, NULL },
    { "keymaps", PM_ARRAY|PM_SPECIAL|PM_READONLY,
      NULL, NULL, NULL,
      arrsetfn, keymapsgetfn, stdunsetfn, NULL },
    { NULL, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL }
};

/**/
int
setup_(Module m)
{
    return 0;
}

/**/
int
boot_(Module m)
{
    struct pardef *def;

    for (def = partab; def->name; def++) {
	unsetparam(def->name);

	if (def->getnfn) {
	    if (!(def->pm = createspecialhash(def->name, def->getnfn,
					      def->scantfn)))
		return 1;
	    def->pm->flags |= def->flags;
	    if (def->hsetfn)
		def->pm->sets.hfn = def->hsetfn;
	} else {
	    if (!(def->pm = createparam(def->name, def->flags | PM_HIDE)))
		return 1;
	    def->pm->sets.afn = def->setfn;
	    def->pm->gets.afn = def->getfn;
	    def->pm->unsetfn = def->unsetfn;
	}
    }
    return 0;
}

/**/
int
cleanup_(Module m)
{
    Param pm;
    struct pardef *def;

    for (def = partab; def->name; def++) {
	if ((pm = (Param) paramtab->getnode(paramtab, def->name)) &&
	    pm == def->pm) {
	    pm->flags &= ~PM_READONLY;
	    unsetparam_pm(pm, 0, 1);
	}
    }
    return 0;
}

/**/
int
finish_(Module m)
{
    return 0;
}
