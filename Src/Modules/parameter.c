/*
 * parameter.c - parameter interface to zsh internals
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

#include "parameter.mdh"
#include "parameter.pro"

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

    if (!(pm = createparam(name, PM_SPECIAL|PM_REMOVABLE|PM_HASHED)))
	return NULL;

    pm->level = pm->old ? locallevel : 0;
    pm->gets.hfn = hashgetfn;
    pm->sets.hfn = hashsetfn;
    pm->unsetfn = stdunsetfn;
    pm->u.hash = ht = newhashtable(7, name, NULL);

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

/* Functions for the parameters special parameter. */

/* Return a string describing the type of a parameter. */

/**/
static char *
paramtypestr(Param pm)
{
    char *val = NULL;
    int f = pm->flags;

    if (!(f & PM_UNSET)) {
	switch (PM_TYPE(f)) {
	case PM_SCALAR:  val = "scalar"; break;
	case PM_ARRAY:   val = "array"; break;
	case PM_INTEGER: val = "integer"; break;
	case PM_HASHED:  val = "association"; break;
	}
	DPUTS(!val, "BUG: type not handled in parameter");
	val = dupstring(val);
	if (pm->level)
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
    } else
	val = dupstring("");

    return val;
}

/**/
static HashNode
getpmparameter(HashTable ht, char *name)
{
    Param rpm, pm = NULL;

    HEAPALLOC {
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
	if ((rpm = (Param) realparamtab->getnode(realparamtab, name)) &&
	    !(rpm->flags & PM_UNSET))
	    pm->u.str = paramtypestr(rpm);
	else {
	    pm->u.str = "";
	    pm->flags |= PM_UNSET;
	}
    } LASTALLOC;

    return (HashNode) pm;
}

/**/
static void
scanpmparameters(HashTable ht, ScanFunc func, int flags)
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

    for (i = 0; i < realparamtab->hsize; i++)
	for (hn = realparamtab->nodes[i]; hn; hn = hn->next) {
	    pm.nam = hn->nam;
	    if (func != scancountparams)
		pm.u.str = paramtypestr((Param) hn);
	    func((HashNode) &pm, flags);
	}
}

/* Functions for the commands special parameter. */

/**/
static void
setpmcommand(Param pm, char *value)
{
    if (isset(RESTRICTED))
	zwarnnam(NULL, "restricted: %s", value, 0);
    else {
	Cmdnam cn = zcalloc(sizeof(*cn));

	cn->flags = HASHED;
	cn->u.cmd = value;

	cmdnamtab->addnode(cmdnamtab, ztrdup(pm->nam), (HashNode) cn);
    }
}

/**/
static void
unsetpmcommand(Param pm, int exp)
{
    HashNode hn = cmdnamtab->removenode(cmdnamtab, pm->nam);

    if (hn)
	cmdnamtab->freenode(hn);
}

/**/
static void
setpmcommands(Param pm, HashTable ht)
{
    int i;
    HashNode hn;

    if (!ht)
	return;

    for (i = 0; i < ht->hsize; i++)
	for (hn = ht->nodes[i]; hn; hn = hn->next) {
	    Cmdnam cn = zcalloc(sizeof(*cn));
	    struct value v;

	    v.isarr = v.inv = v.a = 0;
	    v.b = -1;
	    v.arr = NULL;
	    v.pm = (Param) hn;

	    cn->flags = HASHED;
	    cn->u.cmd = ztrdup(getstrvalue(&v));

	    cmdnamtab->addnode(cmdnamtab, ztrdup(hn->nam), (HashNode) cn);
	}
    deleteparamtable(ht);
}

/**/
static HashNode
getpmcommand(HashTable ht, char *name)
{
    Cmdnam cmd;
    Param pm = NULL;

    if (!(cmd = (Cmdnam) cmdnamtab->getnode(cmdnamtab, name)) &&
	isset(HASHLISTALL)) {
	cmdnamtab->filltable(cmdnamtab);
	cmd = (Cmdnam) cmdnamtab->getnode(cmdnamtab, name);
    }
    HEAPALLOC {
	pm = (Param) zhalloc(sizeof(struct param));
	pm->nam = dupstring(name);
	pm->flags = PM_SCALAR;
	pm->sets.cfn = setpmcommand;
	pm->gets.cfn = strgetfn;
	pm->unsetfn = unsetpmcommand;
	pm->ct = 0;
	pm->env = NULL;
	pm->ename = NULL;
	pm->old = NULL;
	pm->level = 0;
	if (cmd) {
	    if (cmd->flags & HASHED)
		pm->u.str = cmd->u.cmd;
	    else {
		pm->u.str = zhalloc(strlen(*(cmd->u.name)) +
				    strlen(name) + 2);
		strcpy(pm->u.str, *(cmd->u.name));
		strcat(pm->u.str, "/");
		strcat(pm->u.str, name);
	    }
	} else {
	    pm->u.str = "";
	    pm->flags |= PM_UNSET;
	}
    } LASTALLOC;

    return (HashNode) pm;
}

/**/
static void
scanpmcommands(HashTable ht, ScanFunc func, int flags)
{
    struct param pm;
    int i;
    HashNode hn;
    Cmdnam cmd;

    if (isset(HASHLISTALL))
	cmdnamtab->filltable(cmdnamtab);

    pm.flags = PM_SCALAR;
    pm.sets.cfn = setpmcommand;
    pm.gets.cfn = strgetfn;
    pm.unsetfn = unsetpmcommand;
    pm.ct = 0;
    pm.env = NULL;
    pm.ename = NULL;
    pm.old = NULL;
    pm.level = 0;

    for (i = 0; i < cmdnamtab->hsize; i++)
	for (hn = cmdnamtab->nodes[i]; hn; hn = hn->next) {
	    pm.nam = hn->nam;
	    cmd = (Cmdnam) hn;
	    if (func != scancountparams) {
		if (cmd->flags & HASHED)
		    pm.u.str = cmd->u.cmd;
		else {
		    pm.u.str = zhalloc(strlen(*(cmd->u.name)) +
				       strlen(cmd->nam) + 2);
		    strcpy(pm.u.str, *(cmd->u.name));
		    strcat(pm.u.str, "/");
		    strcat(pm.u.str, cmd->nam);
		}
	    }
	    func((HashNode) &pm, flags);
	}
}

/* Functions for the functions special parameter. */

/**/
static void
setfunction(char *name, char *val)
{
    char *value = dupstring(val);
    Shfunc shf;
    List list;
    int sn;

    val = metafy(val, strlen(val), META_REALLOC);

    HEAPALLOC {
	list = parse_string(val, 1);
    } LASTALLOC;

    if (!list || list == &dummy_list) {
	zwarnnam(NULL, "invalid function definition", value, 0);
	zsfree(val);
	return;
    }
    PERMALLOC {
	shf = (Shfunc) zalloc(sizeof(*shf));
	shf->funcdef = (List) dupstruct(list);
	shf->flags = 0;

	if (!strncmp(name, "TRAP", 4) &&
	    (sn = getsignum(name + 4)) != -1) {
	    if (settrap(sn, shf->funcdef)) {
		freestruct(shf->funcdef);
		zfree(shf, sizeof(*shf));
		zsfree(val);
		LASTALLOC_RETURN;
	    }
	    sigtrapped[sn] |= ZSIG_FUNC;
	}
	shfunctab->addnode(shfunctab, ztrdup(name), shf);
    } LASTALLOC;
    zsfree(val);
}

/**/
static void
setpmfunction(Param pm, char *value)
{
    setfunction(pm->nam, value);
}

/**/
static void
unsetpmfunction(Param pm, int exp)
{
    HashNode hn = shfunctab->removenode(shfunctab, pm->nam);

    if (hn)
	shfunctab->freenode(hn);
}

/**/
static void
setpmfunctions(Param pm, HashTable ht)
{
    int i;
    HashNode hn;

    if (!ht)
	return;

    for (i = 0; i < ht->hsize; i++)
	for (hn = ht->nodes[i]; hn; hn = hn->next) {
	    struct value v;

	    v.isarr = v.inv = v.a = 0;
	    v.b = -1;
	    v.arr = NULL;
	    v.pm = (Param) hn;

	    setfunction(hn->nam, ztrdup(getstrvalue(&v)));
	}
    deleteparamtable(ht);
}

/**/
static HashNode
getpmfunction(HashTable ht, char *name)
{
    Shfunc shf;
    Param pm = NULL;

    HEAPALLOC {
	pm = (Param) zhalloc(sizeof(struct param));
	pm->nam = dupstring(name);
	pm->flags = PM_SCALAR;
	pm->sets.cfn = setpmfunction;
	pm->gets.cfn = strgetfn;
	pm->unsetfn = unsetpmfunction;
	pm->ct = 0;
	pm->env = NULL;
	pm->ename = NULL;
	pm->old = NULL;
	pm->level = 0;

	if ((shf = (Shfunc) shfunctab->getnode(shfunctab, name))) {
	    if (shf->flags & PM_UNDEFINED)
		pm->u.str = "undefined";
	    else {
		char *t = getpermtext((void *) dupstruct((void *)
							 shf->funcdef)), *h;

		h = dupstring(t);
		zsfree(t);
		unmetafy(h, NULL);

		pm->u.str = h;
	    }
	} else {
	    pm->u.str = "";
	    pm->flags |= PM_UNSET;
	}
    } LASTALLOC;

    return (HashNode) pm;
}

/**/
static void
scanpmfunctions(HashTable ht, ScanFunc func, int flags)
{
    struct param pm;
    int i;
    HashNode hn;

    pm.flags = PM_SCALAR;
    pm.sets.cfn = setpmcommand;
    pm.gets.cfn = strgetfn;
    pm.unsetfn = unsetpmcommand;
    pm.ct = 0;
    pm.env = NULL;
    pm.ename = NULL;
    pm.old = NULL;
    pm.level = 0;

    for (i = 0; i < shfunctab->hsize; i++)
	for (hn = shfunctab->nodes[i]; hn; hn = hn->next) {
	    if (!(hn->flags & DISABLED)) {
		pm.nam = hn->nam;
		if (func != scancountparams) {
		    if (((Shfunc) hn)->flags & PM_UNDEFINED)
			pm.u.str = "undefined";
		    else {
			char *t = getpermtext((void *)
					      dupstruct((void *) ((Shfunc) hn)->funcdef));

			unmetafy((pm.u.str = dupstring(t)), NULL);
			zsfree(t);
		    }
		}
		func((HashNode) &pm, flags);
	    }
	}
}

/* Functions for the options special parameter. */

/**/
static void
setpmoption(Param pm, char *value)
{
    int n;

    if (!value || (strcmp(value, "on") && strcmp(value, "off")))
	zwarnnam(NULL, "invalid value: %s", value, 0);
    else if (!(n = optlookup(pm->nam)))
	zwarnnam(NULL, "no such option: %s", pm->nam, 0);
    else if (dosetopt(n, (value && strcmp(value, "off")), 0))
	zwarnnam(NULL, "can't change option: %s", pm->nam, 0);
    zsfree(value);
}

/**/
static void
unsetpmoption(Param pm, int exp)
{
    int n;

    if (!(n = optlookup(pm->nam)))
	zwarnnam(NULL, "no such option: %s", pm->nam, 0);
    else if (dosetopt(n, 0, 0))
	zwarnnam(NULL, "can't change option: %s", pm->nam, 0);
}

/**/
static void
setpmoptions(Param pm, HashTable ht)
{
    int i;
    HashNode hn;

    if (!ht)
	return;

    for (i = 0; i < ht->hsize; i++)
	for (hn = ht->nodes[i]; hn; hn = hn->next) {
	    struct value v;
	    char *val;

	    v.isarr = v.inv = v.a = 0;
	    v.b = -1;
	    v.arr = NULL;
	    v.pm = (Param) hn;

	    val = getstrvalue(&v);
	    if (!val || (strcmp(val, "on") && strcmp(val, "off")))
		zwarnnam(NULL, "invalid value: %s", val, 0);
	    else if (dosetopt(optlookup(hn->nam),
			      (val && strcmp(val, "off")), 0))
		zwarnnam(NULL, "can't change option: %s", hn->nam, 0);
	}
    deleteparamtable(ht);
}

/**/
static HashNode
getpmoption(HashTable ht, char *name)
{
    Param pm = NULL;
    int n;

    HEAPALLOC {
	pm = (Param) zhalloc(sizeof(struct param));
	pm->nam = dupstring(name);
	pm->flags = PM_SCALAR;
	pm->sets.cfn = setpmoption;
	pm->gets.cfn = strgetfn;
	pm->unsetfn = unsetpmoption;
	pm->ct = 0;
	pm->env = NULL;
	pm->ename = NULL;
	pm->old = NULL;
	pm->level = 0;

	if ((n = optlookup(name)))
	    pm->u.str = dupstring(opts[n] ? "on" : "off");
	else {
	    pm->u.str = "";
	    pm->flags |= PM_UNSET;
	}
    } LASTALLOC;

    return (HashNode) pm;
}

/**/
static void
scanpmoptions(HashTable ht, ScanFunc func, int flags)
{
    struct param pm;
    int i;
    HashNode hn;

    pm.flags = PM_SCALAR;
    pm.sets.cfn = setpmoption;
    pm.gets.cfn = strgetfn;
    pm.unsetfn = unsetpmoption;
    pm.ct = 0;
    pm.env = NULL;
    pm.ename = NULL;
    pm.old = NULL;
    pm.level = 0;

    for (i = 0; i < optiontab->hsize; i++)
	for (hn = optiontab->nodes[i]; hn; hn = hn->next) {
	    pm.nam = hn->nam;
	    pm.u.str = opts[((Optname) hn)->optno] ? "on" : "off";
	    func((HashNode) &pm, flags);
	}
}

/* Names and Params for the special parameters. */

#define PAR_NAM "parameters"
#define CMD_NAM "commands"
#define FUN_NAM "functions"
#define OPT_NAM "options"

static Param parpm, cmdpm, funpm, optpm;

/**/
int
setup_parameter(Module m)
{
    return 0;
}

/**/
int
boot_parameter(Module m)
{
    /* Create the special associative arrays.
     * As an example for autoloaded parameters, this is probably a bad
     * example, because we the zsh core doesn't support creation of
     * special hashes, yet. */
    Param pm;

    if ((pm = (Param) gethashnode2(paramtab, PAR_NAM)))
	unsetparam_pm(pm, 0, 1);
    if (!(parpm = createspecialhash(PAR_NAM, getpmparameter,
				    scanpmparameters)))
	return 1;
    parpm->flags |= PM_READONLY;
    if ((pm = (Param) gethashnode2(paramtab, CMD_NAM)))
	unsetparam_pm(pm, 0, 1);
    if (!(cmdpm = createspecialhash(CMD_NAM, getpmcommand,
				    scanpmcommands)))
	return 1;
    cmdpm->sets.hfn = setpmcommands;
    if ((pm = (Param) gethashnode2(paramtab, FUN_NAM)))
	unsetparam_pm(pm, 0, 1);
    if (!(funpm = createspecialhash(FUN_NAM, getpmfunction,
				    scanpmfunctions)))
	return 1;
    funpm->sets.hfn = setpmfunctions;
    if ((pm = (Param) gethashnode2(paramtab, OPT_NAM)))
	unsetparam_pm(pm, 0, 1);
    if (!(optpm = createspecialhash(OPT_NAM, getpmoption,
				    scanpmoptions)))
	return 1;
    optpm->sets.hfn = setpmoptions;

    return 0;
}

#ifdef MODULE

/**/
int
cleanup_parameter(Module m)
{
    Param pm;

    /* Remove the special parameters if they are still the same. */

    if ((pm = (Param) paramtab->getnode(paramtab, PAR_NAM)) && pm == parpm) {
	pm->flags &= ~PM_READONLY;
	unsetparam_pm(pm, 0, 1);
    }
    if ((pm = (Param) paramtab->getnode(paramtab, CMD_NAM)) && pm == cmdpm)
	unsetparam_pm(pm, 0, 1);
    if ((pm = (Param) paramtab->getnode(paramtab, FUN_NAM)) && pm == funpm)
	unsetparam_pm(pm, 0, 1);
    if ((pm = (Param) paramtab->getnode(paramtab, OPT_NAM)) && pm == optpm)
	unsetparam_pm(pm, 0, 1);
    return 0;
}

/**/
int
finish_parameter(Module m)
{
    return 0;
}

#endif
