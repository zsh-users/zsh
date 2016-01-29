/*
 * db_gdbm.c - bindings for gdbm
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 2008 Clint Adams
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Clint Adams or the Zsh Development
 * Group be liable to any party for direct, indirect, special, incidental, or
 * consequential damages arising out of the use of this software and its
 * documentation, even if Peter Stephenson, Sven Wischnowsky and the Zsh
 * Development Group have been advised of the possibility of such damage.
 *
 * Clint Adams and the Zsh Development Group
 * specifically disclaim any warranties, including, but not limited to, the
 * implied warranties of merchantability and fitness for a particular purpose.
 * The software provided hereunder is on an "as is" basis, and Peter
 * Stephenson, Sven Wischnowsky and the Zsh Development Group have no
 * obligation to provide maintenance, support, updates, enhancements, or
 * modifications.
 *
 */

#include "db_gdbm.mdh"
#include "db_gdbm.pro"

/*
 * Make sure we have all the bits I'm using for memory mapping, otherwise
 * I don't know what I'm doing.
 */
#if defined(HAVE_GDBM_H) && defined(HAVE_GDBM_OPEN)

#include <gdbm.h>

static char *backtype = "db/gdbm";

static const struct gsu_scalar gdbm_gsu =
{ gdbmgetfn, gdbmsetfn, gdbmunsetfn };
/**/
static const struct gsu_hash gdbm_hash_gsu =
{ hashgetfn, gdbmhashsetfn, gdbmhashunsetfn };

static struct builtin bintab[] = {
    BUILTIN("ztie", 0, bin_ztie, 1, -1, 0, "d:f:r", NULL),
    BUILTIN("zuntie", 0, bin_zuntie, 1, -1, 0, "u", NULL),
};

/**/
static int
bin_ztie(char *nam, char **args, Options ops, UNUSED(int func))
{
    char *resource_name, *pmname;
    GDBM_FILE dbf = NULL;
    int read_write = GDBM_SYNC, pmflags = PM_REMOVABLE;
    Param tied_param;

    if(!OPT_ISSET(ops,'d')) {
        zwarnnam(nam, "you must pass `-d %s'", backtype);
	return 1;
    }
    if(!OPT_ISSET(ops,'f')) {
        zwarnnam(nam, "you must pass `-f' with a filename", NULL);
	return 1;
    }
    if (OPT_ISSET(ops,'r')) {
	read_write |= GDBM_READER;
	pmflags |= PM_READONLY;
    } else {
	read_write |= GDBM_WRCREAT;
    }

    /* Here should be a lookup of the backend type against
     * a registry.
     */
    if (strcmp(OPT_ARG(ops, 'd'), backtype) != 0) {
        zwarnnam(nam, "unsupported backend type `%s'", OPT_ARG(ops, 'd'));
	return 1;
    }

    resource_name = OPT_ARG(ops, 'f');
    pmname = *args;

    if ((tied_param = (Param)paramtab->getnode(paramtab, pmname)) &&
	!(tied_param->node.flags & PM_UNSET)) {
	/*
	 * Unset any existing parameter.  Note there's no implicit
	 * "local" here, but if the existing parameter is local
	 * that will be reflected in the new one.
	 *
	 * We need to do this before attempting to open the DB
	 * in case this variable is already tied to a DB.
	 *
	 * This can fail if the variable is readonly or restricted.
	 * We could call unsetparam() and check errflag instead
	 * of the return status.
	 */
	if (unsetparam_pm(tied_param, 0, 1))
	    return 1;
    }

    dbf = gdbm_open(resource_name, 0, read_write, 0666, 0);
    if(dbf)
	addmodulefd(gdbm_fdesc(dbf), FDT_INTERNAL);
    else {
	zwarnnam(nam, "error opening database file %s", resource_name);
	return 1;
    }

    if (!(tied_param = createspecialhash(pmname, &getgdbmnode, &scangdbmkeys,
					 pmflags))) {
        zwarnnam(nam, "cannot create the requested parameter %s", pmname);
	fdtable[gdbm_fdesc(dbf)] = FDT_UNUSED;
	gdbm_close(dbf);
	return 1;
    }

    tied_param->gsu.h = &gdbm_hash_gsu;
    tied_param->u.hash->tmpdata = (void *)dbf;

    return 0;
}

/**/
static int
bin_zuntie(char *nam, char **args, Options ops, UNUSED(int func))
{
    Param pm;
    char *pmname;
    int ret = 0;

    for (pmname = *args; *args++; pmname = *args) {
	pm = (Param) paramtab->getnode(paramtab, pmname);
	if(!pm) {
	    zwarnnam(nam, "cannot untie %s", pmname);
	    ret = 1;
	    continue;
	}
	if (pm->gsu.h != &gdbm_hash_gsu) {
	    zwarnnam(nam, "not a tied gdbm hash: %s", pmname);
	    ret = 1;
	    continue;
	}

	queue_signals();
	if (OPT_ISSET(ops,'u'))
	    gdbmuntie(pm);	/* clear read-only-ness */
	if (unsetparam_pm(pm, 0, 1)) {
	    /* assume already reported */
	    ret = 1;
	}
	unqueue_signals();
    }

    return ret;
}

/**/
static char *
gdbmgetfn(Param pm)
{
    datum key, content;
    int ret;
    GDBM_FILE dbf;

    key.dptr = pm->node.nam;
    key.dsize = strlen(key.dptr) + 1;

    dbf = (GDBM_FILE)(pm->u.hash->tmpdata);
    ret = gdbm_exists(dbf, key);
    if(ret) {
        content = gdbm_fetch(dbf, key);
    } else {
        content.dptr = dupstring("");
    }

    return content.dptr;
}

/**/
static void
gdbmsetfn(Param pm, char *val)
{
    datum key, content;
    GDBM_FILE dbf;

    key.dptr = pm->node.nam;
    key.dsize = strlen(key.dptr) + 1;
    content.dptr = val;
    content.dsize = strlen(content.dptr) + 1;

    dbf = (GDBM_FILE)(pm->u.hash->tmpdata);
    (void)gdbm_store(dbf, key, content, GDBM_REPLACE);
}

/**/
static void
gdbmunsetfn(Param pm, UNUSED(int um))
{
    datum key;
    GDBM_FILE dbf;

    key.dptr = pm->node.nam;
    key.dsize = strlen(key.dptr) + 1;

    dbf = (GDBM_FILE)(pm->u.hash->tmpdata);
    (void)gdbm_delete(dbf, key);
}

/**/
static HashNode
getgdbmnode(HashTable ht, const char *name)
{
    int len;
    char *nameu;
    Param pm = NULL;

    nameu = dupstring(name);
    unmetafy(nameu, &len);

    pm = (Param) hcalloc(sizeof(struct param));
    pm->node.nam = nameu;
    pm->node.flags = PM_SCALAR;
    pm->gsu.s = &gdbm_gsu;
    pm->u.hash = ht;

    return &pm->node;
}

/**/
static void
scangdbmkeys(HashTable ht, ScanFunc func, int flags)
{
    Param pm = NULL;
    datum key, content;
    GDBM_FILE dbf = (GDBM_FILE)(ht->tmpdata);

    pm = (Param) hcalloc(sizeof(struct param));

    pm->node.flags = PM_SCALAR;
    pm->gsu.s = &nullsetscalar_gsu;

    key = gdbm_firstkey(dbf);

    while(key.dptr) {
	content = gdbm_fetch(dbf, key);

	pm->node.nam = key.dptr;
	pm->u.str = content.dptr;
	pm->gsu.s = &nullsetscalar_gsu;

	func(&pm->node, flags);

        key = gdbm_nextkey(dbf, key);
    }

}

/**/
static void
gdbmhashsetfn(Param pm, HashTable ht)
{
    int i;
    HashNode hn;
    GDBM_FILE dbf;
    datum key, content;

    if (!pm->u.hash || pm->u.hash == ht)
	return;

    if (!(dbf = (GDBM_FILE)(pm->u.hash->tmpdata)))
	return;

    key = gdbm_firstkey(dbf);
    while (key.dptr) {
	queue_signals();
	(void)gdbm_delete(dbf, key);
	free(key.dptr);
	unqueue_signals();
	key = gdbm_firstkey(dbf);
    }

    /* just deleted everything, clean up */
    (void)gdbm_reorganize(dbf);

    if (!ht)
	return;

    for (i = 0; i < ht->hsize; i++)
	for (hn = ht->nodes[i]; hn; hn = hn->next) {
	    struct value v;

	    v.isarr = v.flags = v.start = 0;
	    v.end = -1;
	    v.arr = NULL;
	    v.pm = (Param) hn;

	    key.dptr = v.pm->node.nam;
	    key.dsize = strlen(key.dptr) + 1;

	    queue_signals();

	    content.dptr = getstrvalue(&v);
	    content.dsize = strlen(content.dptr) + 1;

	    (void)gdbm_store(dbf, key, content, GDBM_REPLACE);	

	    unqueue_signals();
	}
}

/**/
static void
gdbmuntie(Param pm)
{
    GDBM_FILE dbf = (GDBM_FILE)(pm->u.hash->tmpdata);
    HashTable ht = pm->u.hash;

    if (dbf) { /* paranoia */
	fdtable[gdbm_fdesc(dbf)] = FDT_UNUSED;
	gdbm_close(dbf);
    }

    ht->tmpdata = NULL;

    /* for completeness ... createspecialhash() should have an inverse */
    ht->getnode = ht->getnode2 = gethashnode2;
    ht->scantab = NULL;

    pm->node.flags &= ~(PM_SPECIAL|PM_READONLY);
    pm->gsu.h = &stdhash_gsu;
}

/**/
static void
gdbmhashunsetfn(Param pm, UNUSED(int exp))
{
    gdbmuntie(pm);
    /* hash table is now normal, so proceed normally... */
    pm->gsu.h->setfn(pm, NULL);
    pm->node.flags |= PM_UNSET;
}

#else
# error no gdbm
#endif /* have gdbm */

static struct features module_features = {
    bintab, sizeof(bintab)/sizeof(*bintab),
    NULL, 0,
    NULL, 0,
    NULL, 0,
    0
};

/**/
int
setup_(UNUSED(Module m))
{
    return 0;
}

/**/
int
features_(Module m, char ***features)
{
    *features = featuresarray(m, &module_features);
    return 0;
}

/**/
int
enables_(Module m, int **enables)
{
    return handlefeatures(m, &module_features, enables);
}

/**/
int
boot_(UNUSED(Module m))
{
    return 0;
}

/**/
int
cleanup_(Module m)
{
    return setfeatureenables(m, &module_features, NULL);
}

/**/
int
finish_(UNUSED(Module m))
{
    return 0;
}
