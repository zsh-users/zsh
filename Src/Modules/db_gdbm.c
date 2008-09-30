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

static struct builtin bintab[] = {
    BUILTIN("ztie", 0, bin_ztie, 1, -1, 0, "d:f:", NULL),
    BUILTIN("zuntie", 0, bin_zuntie, 1, -1, 0, NULL, NULL),
};

/**/
static int
bin_ztie(char *nam, char **args, Options ops, UNUSED(int func))
{
    char *resource_name, *pmname;
    GDBM_FILE dbf = NULL;
    Param tied_param;

    if(!OPT_ISSET(ops,'d')) {
        zwarnnam(nam, "you must pass `-d db/gdbm' to ztie", NULL);
	return 1;
    }
    if(!OPT_ISSET(ops,'f')) {
        zwarnnam(nam, "you must pass `-f' with a filename to ztie", NULL);
	return 1;
    }

    /* Here should be a lookup of the backend type against
     * a registry.
     */

    pmname = ztrdup(*args);

    resource_name = OPT_ARG(ops, 'f');

    if (!(tied_param = createspecialhash(pmname, &getgdbmnode, &scangdbmkeys, 0))) {
        zwarnnam(nam, "cannot create the requested parameter name", NULL);
	return 1;
    }

    dbf = gdbm_open(resource_name, 0, GDBM_WRCREAT | GDBM_SYNC, 0666, 0);
    if(!dbf) {
        zwarnnam(nam, "error opening database file %s", resource_name);
	return 1;
    }

    tied_param->u.hash->tmpdata = (void *)dbf;

    return 0;
}

/**/
static int
bin_zuntie(char *nam, char **args, Options ops, UNUSED(int func))
{
    Param pm;
    GDBM_FILE dbf;

    pm = (Param) paramtab->getnode(paramtab, args[0]);
    if(!pm) {
        zwarnnam(nam, "cannot untie %s", args[0]);
	return 1;
    }

    dbf = (GDBM_FILE)(pm->u.hash->tmpdata);
    gdbm_close(dbf);
/*    free(pm->u.hash->tmpdata); */
    paramtab->removenode(paramtab, pm->node.nam);

    return 0;
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
    int ret;
    GDBM_FILE dbf;

    key.dptr = pm->node.nam;
    key.dsize = strlen(key.dptr) + 1;
    content.dptr = val;
    content.dsize = strlen(content.dptr) + 1;

    dbf = (GDBM_FILE)(pm->u.hash->tmpdata);
    ret = gdbm_store(dbf, key, content, GDBM_REPLACE);
}

/**/
static void
gdbmunsetfn(Param pm, int um)
{
    datum key;
    int ret;
    GDBM_FILE dbf;

    key.dptr = pm->node.nam;
    key.dsize = strlen(key.dptr) + 1;

    dbf = (GDBM_FILE)(pm->u.hash->tmpdata);
    ret = gdbm_delete(dbf, key);
}

/**/
static HashNode
getgdbmnode(HashTable ht, const char *name)
{
    int len;
    char *nameu;
    datum key;
    Param pm = NULL;

    nameu = dupstring(name);
    unmetafy(nameu, &len);
    key.dptr = nameu;

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
    GDBM_FILE dbf;

    dbf = (GDBM_FILE)(ht->tmpdata);

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
cleanup_(UNUSED(Module m))
{
    return setfeatureenables(m, &module_features, NULL);
}

/**/
int
finish_(UNUSED(Module m))
{
    return 0;
}
