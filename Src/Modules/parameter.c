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

/* This says if we are cleaning up when the module is unloaded. */

static int incleanup;

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

    if (!(pm = createparam(name, PM_SPECIAL|PM_HIDE|PM_HIDEVAL|
			   PM_REMOVABLE|PM_HASHED)))
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

/* Functions for the parameters special parameter. */

/* Return a string describing the type of a parameter. */

/**/
static char *
paramtypestr(Param pm)
{
    char *val = NULL;
    int f = pm->flags;

    if (!(f & PM_UNSET)) {
	if (pm->flags & PM_AUTOLOAD)
	    return dupstring("undefined");

	switch (PM_TYPE(f)) {
	case PM_SCALAR:  val = "scalar"; break;
	case PM_ARRAY:   val = "array"; break;
	case PM_INTEGER: val = "integer"; break;
	case PM_EFLOAT:
	case PM_FFLOAT:  val = "float"; break;
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
	if (f & PM_HIDE)
	    val = dyncat(val, "-hide");
	if (f & PM_HIDEVAL)
	    val = dyncat(val, "-hideval");
	if (f & PM_SPECIAL)
	    val = dyncat(val, "-special");
    } else
	val = dupstring("");

    return val;
}

/**/
static HashNode
getpmparameter(HashTable ht, char *name)
{
    Param rpm, pm = NULL;

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
	pm->u.str = dupstring("");
	pm->flags |= PM_UNSET;
    }
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
	    if (func != scancountparams &&
		((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		 !(flags & SCANPM_WANTKEYS)))
		pm.u.str = paramtypestr((Param) hn);
	    func((HashNode) &pm, flags);
	}
}

/* Functions for the commands special parameter. */

/**/
static void
setpmcommand(Param pm, char *value)
{
    if (isset(RESTRICTED)) {
	zwarn("restricted: %s", value, 0);
	zsfree(value);
    } else {
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

	    v.isarr = v.inv = v.start = 0;
	    v.end = -1;
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
	    pm->u.str = zhalloc(strlen(*(cmd->u.name)) + strlen(name) + 2);
	    strcpy(pm->u.str, *(cmd->u.name));
	    strcat(pm->u.str, "/");
	    strcat(pm->u.str, name);
	}
    } else {
	pm->u.str = dupstring("");
	pm->flags |= PM_UNSET;
    }
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
	    if (func != scancountparams &&
		((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		 !(flags & SCANPM_WANTKEYS))) {
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
setfunction(char *name, char *val, int dis)
{
    char *value = dupstring(val);
    Shfunc shf;
    Eprog prog;
    int sn;

    val = metafy(val, strlen(val), META_REALLOC);

    prog = parse_string(val);

    if (!prog || prog == &dummy_eprog) {
	zwarn("invalid function definition", value, 0);
	zsfree(val);
	return;
    }
    shf = (Shfunc) zalloc(sizeof(*shf));
    shf->funcdef = dupeprog(prog, 0);
    shf->flags = dis;

    if (!strncmp(name, "TRAP", 4) &&
	(sn = getsignum(name + 4)) != -1) {
	if (settrap(sn, shf->funcdef)) {
	    freeeprog(shf->funcdef);
	    zfree(shf, sizeof(*shf));
	    zsfree(val);
	    return;
	}
	sigtrapped[sn] |= ZSIG_FUNC;
    }
    shfunctab->addnode(shfunctab, ztrdup(name), shf);
    zsfree(val);
}

/**/
static void
setpmfunction(Param pm, char *value)
{
    setfunction(pm->nam, value, 0);
}

/**/
static void
setpmdisfunction(Param pm, char *value)
{
    setfunction(pm->nam, value, DISABLED);
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
setfunctions(Param pm, HashTable ht, int dis)
{
    int i;
    HashNode hn;

    if (!ht)
	return;

    for (i = 0; i < ht->hsize; i++)
	for (hn = ht->nodes[i]; hn; hn = hn->next) {
	    struct value v;

	    v.isarr = v.inv = v.start = 0;
	    v.end = -1;
	    v.arr = NULL;
	    v.pm = (Param) hn;

	    setfunction(hn->nam, ztrdup(getstrvalue(&v)), dis);
	}
    deleteparamtable(ht);
}

/**/
static void
setpmfunctions(Param pm, HashTable ht)
{
    setfunctions(pm, ht, 0);
}

/**/
static void
setpmdisfunctions(Param pm, HashTable ht)
{
    setfunctions(pm, ht, DISABLED);
}

/**/
static HashNode
getfunction(HashTable ht, char *name, int dis)
{
    Shfunc shf;
    Param pm = NULL;

    pm = (Param) zhalloc(sizeof(struct param));
    pm->nam = dupstring(name);
    pm->flags = PM_SCALAR;
    pm->sets.cfn = (dis ? setpmdisfunction : setpmfunction);
    pm->gets.cfn = strgetfn;
    pm->unsetfn = unsetpmfunction;
    pm->ct = 0;
    pm->env = NULL;
    pm->ename = NULL;
    pm->old = NULL;
    pm->level = 0;

    if ((shf = (Shfunc) shfunctab->getnode2(shfunctab, name)) &&
	(dis ? (shf->flags & DISABLED) : !(shf->flags & DISABLED))) {
	if (shf->flags & PM_UNDEFINED) {
	    pm->u.str = dyncat("builtin autoload -X",
			       ((shf->flags & PM_UNALIASED) ?
				((shf->flags & PM_TAGGED) ? "Ut" : "U") :
				((shf->flags & PM_TAGGED) ? "t" : "")));
	} else {
	    char *t = getpermtext(shf->funcdef, NULL), *n, *h;

	    if (shf->funcdef->flags & EF_RUN) {
		n = nicedupstring(name);
		h = (char *) zhalloc(strlen(t) + strlen(n) + 9);
		h[0] = '\t';
		strcpy(h + 1, t);
		strcat(h, "\n\t");
		strcat(h, n);
		strcat(h, " \"$@\"");
	    } else
		h = dyncat("\t", t);
	    zsfree(t);
	    unmetafy(h, NULL);

	    pm->u.str = h;
	}
    } else {
	pm->u.str = dupstring("");
	pm->flags |= PM_UNSET;
    }
    return (HashNode) pm;
}

/**/
static HashNode
getpmfunction(HashTable ht, char *name)
{
    return getfunction(ht, name, 0);
}

/**/
static HashNode
getpmdisfunction(HashTable ht, char *name)
{
    return getfunction(ht, name, DISABLED);
}

/**/
static void
scanfunctions(HashTable ht, ScanFunc func, int flags, int dis)
{
    struct param pm;
    int i;
    HashNode hn;

    pm.flags = PM_SCALAR;
    pm.sets.cfn = (dis ? setpmdisfunction : setpmfunction);
    pm.gets.cfn = strgetfn;
    pm.unsetfn = unsetpmcommand;
    pm.ct = 0;
    pm.env = NULL;
    pm.ename = NULL;
    pm.old = NULL;
    pm.level = 0;

    for (i = 0; i < shfunctab->hsize; i++)
	for (hn = shfunctab->nodes[i]; hn; hn = hn->next) {
	    if (dis ? (hn->flags & DISABLED) : !(hn->flags & DISABLED)) {
		pm.nam = hn->nam;
		if (func != scancountparams &&
		    ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		     !(flags & SCANPM_WANTKEYS))) {
		    if (((Shfunc) hn)->flags & PM_UNDEFINED) {
			Shfunc shf = (Shfunc) hn;
			pm.u.str =
			    dyncat("builtin autoload -X",
				   ((shf->flags & PM_UNALIASED) ?
				    ((shf->flags & PM_TAGGED) ? "Ut" : "U") :
				    ((shf->flags & PM_TAGGED) ? "t" : "")));
		    } else {
			char *t = getpermtext(((Shfunc) hn)->funcdef, NULL), *n;

			if (((Shfunc) hn)->funcdef->flags & EF_RUN) {
			    n = nicedupstring(hn->nam);
			    pm.u.str = (char *) zhalloc(strlen(t) + strlen(n) + 9);
			    pm.u.str[0] = '\t';
			    strcpy(pm.u.str + 1, t);
			    strcat(pm.u.str, "\n\t");
			    strcat(pm.u.str, n);
			    strcat(pm.u.str, " \"$@\"");
			} else
			    pm.u.str = dyncat("\t", t);
			unmetafy(pm.u.str, NULL);
			zsfree(t);
		    }
		}
		func((HashNode) &pm, flags);
	    }
	}
}

/**/
static void
scanpmfunctions(HashTable ht, ScanFunc func, int flags)
{
    scanfunctions(ht, func, flags, 0);
}

/**/
static void
scanpmdisfunctions(HashTable ht, ScanFunc func, int flags)
{
    scanfunctions(ht, func, flags, DISABLED);
}

/* Functions for the funcstack special parameter. */

/**/
static char **
funcstackgetfn(Param pm)
{
    Funcstack f;
    int num;
    char **ret, **p;

    for (f = funcstack, num = 0; f; f = f->prev, num++);

    ret = (char **) zhalloc((num + 1) * sizeof(char *));

    for (f = funcstack, p = ret; f; f = f->prev, p++)
	*p = f->name;
    *p = NULL;

    return ret;
}

/* Functions for the builtins special parameter. */

/**/
static HashNode
getbuiltin(HashTable ht, char *name, int dis)
{
    Param pm = NULL;
    Builtin bn;

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
    if ((bn = (Builtin) builtintab->getnode2(builtintab, name)) &&
	(dis ? (bn->flags & DISABLED) : !(bn->flags & DISABLED))) {
	char *t = ((bn->handlerfunc || (bn->flags & BINF_PREFIX)) ?
		   "defined" : "undefined");

	pm->u.str = dupstring(t);
    } else {
	pm->u.str = dupstring("");
	pm->flags |= PM_UNSET;
    }
    return (HashNode) pm;
}

/**/
static HashNode
getpmbuiltin(HashTable ht, char *name)
{
    return getbuiltin(ht, name, 0);
}

/**/
static HashNode
getpmdisbuiltin(HashTable ht, char *name)
{
    return getbuiltin(ht, name, DISABLED);
}

/**/
static void
scanbuiltins(HashTable ht, ScanFunc func, int flags, int dis)
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

    for (i = 0; i < builtintab->hsize; i++)
	for (hn = builtintab->nodes[i]; hn; hn = hn->next) {
	    if (dis ? (hn->flags & DISABLED) : !(hn->flags & DISABLED)) {
		pm.nam = hn->nam;
		if (func != scancountparams &&
		    ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		     !(flags & SCANPM_WANTKEYS))) {
		    char *t = ((((Builtin) hn)->handlerfunc ||
				(hn->flags & BINF_PREFIX)) ?
			       "defined" : "undefined");

		    pm.u.str = dupstring(t);
		}
		func((HashNode) &pm, flags);
	    }
	}
}

/**/
static void
scanpmbuiltins(HashTable ht, ScanFunc func, int flags)
{
    scanbuiltins(ht, func, flags, 0);
}

/**/
static void
scanpmdisbuiltins(HashTable ht, ScanFunc func, int flags)
{
    scanbuiltins(ht, func, flags, DISABLED);
}

/* Functions for the reswords special parameter. */

/**/
static char **
getreswords(int dis)
{
    int i;
    HashNode hn;
    char **ret, **p;

    p = ret = (char **) zhalloc((reswdtab->ct + 1) * sizeof(char *));

    for (i = 0; i < reswdtab->hsize; i++)
	for (hn = reswdtab->nodes[i]; hn; hn = hn->next)
	    if (dis ? (hn->flags & DISABLED) : !(hn->flags & DISABLED))
		*p++ = dupstring(hn->nam);
    *p = NULL;

    return ret;
}

/**/
static char **
reswordsgetfn(Param pm)
{
    return getreswords(0);
}

/**/
static char **
disreswordsgetfn(Param pm)
{
    return getreswords(DISABLED);
}

/* Functions for the options special parameter. */

/**/
static void
setpmoption(Param pm, char *value)
{
    int n;

    if (!value || (strcmp(value, "on") && strcmp(value, "off")))
	zwarn("invalid value: %s", value, 0);
    else if (!(n = optlookup(pm->nam)))
	zwarn("no such option: %s", pm->nam, 0);
    else if (dosetopt(n, (value && strcmp(value, "off")), 0))
	zwarn("can't change option: %s", pm->nam, 0);
    zsfree(value);
}

/**/
static void
unsetpmoption(Param pm, int exp)
{
    int n;

    if (!(n = optlookup(pm->nam)))
	zwarn("no such option: %s", pm->nam, 0);
    else if (dosetopt(n, 0, 0))
	zwarn("can't change option: %s", pm->nam, 0);
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

	    v.isarr = v.inv = v.start = 0;
	    v.end = -1;
	    v.arr = NULL;
	    v.pm = (Param) hn;

	    val = getstrvalue(&v);
	    if (!val || (strcmp(val, "on") && strcmp(val, "off")))
		zwarn("invalid value: %s", val, 0);
	    else if (dosetopt(optlookup(hn->nam),
			      (val && strcmp(val, "off")), 0))
		zwarn("can't change option: %s", hn->nam, 0);
	}
    deleteparamtable(ht);
}

/**/
static HashNode
getpmoption(HashTable ht, char *name)
{
    Param pm = NULL;
    int n;

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
    {
	int ison;
	if (n > 0)
	    ison = opts[n];
	else
	    ison = !opts[-n];
	pm->u.str = dupstring(ison ? "on" : "off");
    }
    else {
	pm->u.str = dupstring("");
	pm->flags |= PM_UNSET;
    }
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
	    int optno = ((Optname) hn)->optno, ison;
	    pm.nam = hn->nam;
	    ison = optno < 0 ? !opts[-optno] : opts[optno];
	    pm.u.str = dupstring(ison ? "on" : "off");
	    func((HashNode) &pm, flags);
	}
}

/* Functions for the modules special parameter. */

static char *modpmname;
static int modpmfound;

/**/
static void
modpmbuiltinscan(HashNode hn, int dummy)
{
    if (!(((Builtin) hn)->flags & BINF_ADDED) &&
	!strcmp(((Builtin) hn)->optstr, modpmname))
	modpmfound = 1;
}

/**/
static void
modpmparamscan(HashNode hn, int dummy)
{
    if ((((Param) hn)->flags & PM_AUTOLOAD) &&
	!strcmp(((Param) hn)->u.str, modpmname))
	modpmfound = 1;
}

/**/
static int
findmodnode(LinkList l, char *nam)
{
    LinkNode node;

    for (node = firstnode(l); node; incnode(node))
	if (!strcmp(nam, (char *) getdata(node)))
	    return 1;

    return 0;
}

/**/
static HashNode
getpmmodule(HashTable ht, char *name)
{
    Param pm = NULL;
    char *type = NULL;
    LinkNode node;

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

    if (!type) {
	Module m;

	for (node = firstnode(modules); node; incnode(node)) {
	    m = (Module) getdata(node);
	    if (m->u.handle && !(m->flags & MOD_UNLOAD) &&
		!strcmp(name, m->nam)) {
		type = ((m->flags & MOD_ALIAS) ?
			dyncat("alias:", m->u.alias) : "loaded");
		break;
	    }
	}
    }
    modpmname = name;
    modpmfound = 0;
    if (!type) {
	scanhashtable(builtintab, 0, 0, 0, modpmbuiltinscan, 0);
	if (!modpmfound) {
	    Conddef p;

	    for (p = condtab; p; p = p->next)
		if (p->module && !strcmp(name, p->module)) {
		    modpmfound = 1;
		    break;
		}
	    if (!modpmfound)
		scanhashtable(realparamtab, 0, 0, 0, modpmparamscan, 0);
	}
	if (modpmfound)
	    type = "autoloaded";
    }
    if (type)
	pm->u.str = dupstring(type);
    else {
	pm->u.str = dupstring("");
	pm->flags |= PM_UNSET;
    }
    return (HashNode) pm;
}

/**/
static void
scanpmmodules(HashTable ht, ScanFunc func, int flags)
{
    struct param pm;
    int i;
    HashNode hn;
    LinkList done = newlinklist();
    LinkNode node;
    Module m;
    Conddef p;
    char *loaded = dupstring("loaded");

    pm.flags = PM_SCALAR | PM_READONLY;
    pm.sets.cfn = NULL;
    pm.gets.cfn = strgetfn;
    pm.unsetfn = NULL;
    pm.ct = 0;
    pm.env = NULL;
    pm.ename = NULL;
    pm.old = NULL;
    pm.level = 0;

    for (node = firstnode(modules); node; incnode(node)) {
	m = (Module) getdata(node);
	if (m->u.handle && !(m->flags & MOD_UNLOAD)) {
	    pm.nam = m->nam;
	    pm.u.str = ((m->flags & MOD_ALIAS) ?
			dyncat("alias:", m->u.alias) : loaded);
	    addlinknode(done, pm.nam);
	    func((HashNode) &pm, flags);
	}
    }
    pm.u.str = dupstring("autoloaded");
    for (i = 0; i < builtintab->hsize; i++)
	for (hn = builtintab->nodes[i]; hn; hn = hn->next) {
	    if (!(((Builtin) hn)->flags & BINF_ADDED) &&
		!findmodnode(done, ((Builtin) hn)->optstr)) {
		pm.nam = ((Builtin) hn)->optstr;
		addlinknode(done, pm.nam);
		func((HashNode) &pm, flags);
	    }
	}
    for (p = condtab; p; p = p->next)
	if (p->module && !findmodnode(done, p->module)) {
	    pm.nam = p->module;
	    addlinknode(done, pm.nam);
	    func((HashNode) &pm, flags);
	}
    for (i = 0; i < realparamtab->hsize; i++)
	for (hn = realparamtab->nodes[i]; hn; hn = hn->next) {
	    if ((((Param) hn)->flags & PM_AUTOLOAD) &&
		!findmodnode(done, ((Param) hn)->u.str)) {
		pm.nam = ((Param) hn)->u.str;
		addlinknode(done, pm.nam);
		func((HashNode) &pm, flags);
	    }
	}
}

/* Functions for the dirstack special parameter. */

/**/
static void
dirssetfn(Param pm, char **x)
{
    char **ox = x;

    if (!incleanup) {
	freelinklist(dirstack, freestr);
	dirstack = znewlinklist();
	while (x && *x)
	    zaddlinknode(dirstack, ztrdup(*x++));
    }
    if (ox)
	freearray(ox);
}

/**/
static char **
dirsgetfn(Param pm)
{
    int l = countlinknodes(dirstack);
    char **ret = (char **) zhalloc((l + 1) * sizeof(char *)), **p;
    LinkNode n;

    for (n = firstnode(dirstack), p = ret; n; incnode(n), p++)
	*p = dupstring((char *) getdata(n));
    *p = NULL;

    return ret;
}

/* Functions for the history special parameter. */

/**/
static HashNode
getpmhistory(HashTable ht, char *name)
{
    Param pm = NULL;
    Histent he;
    char *p;
    int ok = 1;

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

    if (*name != '0' || name[1]) {
	if (*name == '0')
	    ok = 0;
	else {
	    for (p = name; *p && idigit(*p); p++);
	    if (*p)
		ok = 0;
	}
    }
    if (ok && (he = quietgethist(atoi(name))))
	pm->u.str = dupstring(he->text);
    else {
	pm->u.str = dupstring("");
	pm->flags |= PM_UNSET;
    }
    return (HashNode) pm;
}

/**/
static void
scanpmhistory(HashTable ht, ScanFunc func, int flags)
{
    struct param pm;
    int i = addhistnum(curhist, -1, HIST_FOREIGN);
    Histent he = gethistent(i, GETHIST_UPWARD);
    char buf[40];

    pm.flags = PM_SCALAR | PM_READONLY;
    pm.sets.cfn = NULL;
    pm.gets.cfn = strgetfn;
    pm.unsetfn = NULL;
    pm.ct = 0;
    pm.env = NULL;
    pm.ename = NULL;
    pm.old = NULL;
    pm.level = 0;

    while (he) {
	if (func != scancountparams) {
	    sprintf(buf, "%d", he->histnum);
	    pm.nam = dupstring(buf);
	    if ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		!(flags & SCANPM_WANTKEYS))
		pm.u.str = dupstring(he->text);
	}
	func((HashNode) &pm, flags);

	he = up_histent(he);
    }
}

/* Function for the historywords special parameter. */

/**/
static char **
histwgetfn(Param pm)
{
    char **ret, **p, *h, *e, sav;
    LinkList l = newlinklist(), ll;
    LinkNode n;
    int i = addhistnum(curhist, -1, HIST_FOREIGN), iw;
    Histent he = gethistent(i, GETHIST_UPWARD);

    if ((ll = bufferwords(NULL, NULL, NULL)))
        for (n = firstnode(ll); n; incnode(n))
            pushnode(l, getdata(n));

    while (he) {
	for (iw = he->nwords - 1; iw >= 0; iw--) {
	    h = he->text + he->words[iw * 2];
	    e = he->text + he->words[iw * 2 + 1];
	    sav = *e;
	    *e = '\0';
	    addlinknode(l, dupstring(h));
	    *e = sav;
	}
	he = up_histent(he);
    }
    ret = (char **) zhalloc((countlinknodes(l) + 1) * sizeof(char *));

    for (p = ret, n = firstnode(l); n; incnode(n), p++)
	*p = (char *) getdata(n);
    *p = NULL;

    return ret;
}

/* Functions for the jobtexts special parameter. */

/**/
static char *
pmjobtext(int job)
{
    Process pn;
    int len = 1;
    char *ret;

    for (pn = jobtab[job].procs; pn; pn = pn->next)
	len += strlen(pn->text) + 3;

    ret = (char *) zhalloc(len);
    ret[0] = '\0';

    for (pn = jobtab[job].procs; pn; pn = pn->next) {
	strcat(ret, pn->text);
	if (pn->next)
	    strcat(ret, " | ");
    }
    return ret;
}

/**/
static HashNode
getpmjobtext(HashTable ht, char *name)
{
    Param pm = NULL;
    int job;

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

    if ((job = atoi(name)) >= 1 && job < MAXJOB &&
	jobtab[job].stat && jobtab[job].procs &&
	!(jobtab[job].stat & STAT_NOPRINT))
	pm->u.str = pmjobtext(job);
    else {
	pm->u.str = dupstring("");
	pm->flags |= PM_UNSET;
    }
    return (HashNode) pm;
}

/**/
static void
scanpmjobtexts(HashTable ht, ScanFunc func, int flags)
{
    struct param pm;
    int job;
    char buf[40];

    pm.flags = PM_SCALAR | PM_READONLY;
    pm.sets.cfn = NULL;
    pm.gets.cfn = strgetfn;
    pm.unsetfn = NULL;
    pm.ct = 0;
    pm.env = NULL;
    pm.ename = NULL;
    pm.old = NULL;
    pm.level = 0;

    for (job = 1; job < MAXJOB; job++) {
	if (jobtab[job].stat && jobtab[job].procs &&
	    !(jobtab[job].stat & STAT_NOPRINT)) {
	    if (func != scancountparams) {
		sprintf(buf, "%d", job);
		pm.nam = dupstring(buf);
		if ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		    !(flags & SCANPM_WANTKEYS))
		    pm.u.str = pmjobtext(job);
	    }
	    func((HashNode) &pm, flags);
	}
    }
}

/* Functions for the jobstates special parameter. */

/**/
static char *
pmjobstate(int job)
{
    Process pn;
    char buf[256], buf2[128], *ret, *state, *cp;

    if (job == curjob)
	cp = ":+";
    else if (job == prevjob)
	cp = ":-";
    else
	cp = ":";

    if (jobtab[job].stat & STAT_DONE)
	ret = dyncat("done", cp);
    else if (jobtab[job].stat & STAT_STOPPED)
	ret = dyncat("suspended", cp);
    else
	ret = dyncat("running", cp);

    for (pn = jobtab[job].procs; pn; pn = pn->next) {

	if (pn->status == SP_RUNNING)
	    state = "running";
	else if (WIFEXITED(pn->status)) {
	    if (WEXITSTATUS(pn->status))
		sprintf((state = buf2), "exit %d", (pn->status));
	    else
		state = "done";
	} else if (WIFSTOPPED(pn->status))
	    state = sigmsg(WSTOPSIG(pn->status));
	else if (WCOREDUMP(pn->status))
	    sprintf((state = buf2), "%s (core dumped)",
		    sigmsg(WTERMSIG(pn->status)));
	else
	    state = sigmsg(WTERMSIG(pn->status));

	sprintf(buf, ":%d=%s", (int)pn->pid, state);

	ret = dyncat(ret, buf);
    }
    return ret;
}

/**/
static HashNode
getpmjobstate(HashTable ht, char *name)
{
    Param pm = NULL;
    int job;

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

    if ((job = atoi(name)) >= 1 && job < MAXJOB &&
	jobtab[job].stat && jobtab[job].procs &&
	!(jobtab[job].stat & STAT_NOPRINT))
	pm->u.str = pmjobstate(job);
    else {
	pm->u.str = dupstring("");
	pm->flags |= PM_UNSET;
    }
    return (HashNode) pm;
}

/**/
static void
scanpmjobstates(HashTable ht, ScanFunc func, int flags)
{
    struct param pm;
    int job;
    char buf[40];

    pm.flags = PM_SCALAR | PM_READONLY;
    pm.sets.cfn = NULL;
    pm.gets.cfn = strgetfn;
    pm.unsetfn = NULL;
    pm.ct = 0;
    pm.env = NULL;
    pm.ename = NULL;
    pm.old = NULL;
    pm.level = 0;

    for (job = 1; job < MAXJOB; job++) {
	if (jobtab[job].stat && jobtab[job].procs &&
	    !(jobtab[job].stat & STAT_NOPRINT)) {
	    if (func != scancountparams) {
		sprintf(buf, "%d", job);
		pm.nam = dupstring(buf);
		if ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		    !(flags & SCANPM_WANTKEYS))
		    pm.u.str = pmjobstate(job);
	    }
	    func((HashNode) &pm, flags);
	}
    }
}

/* Functions for the jobdirs special parameter. */

/**/
static char *
pmjobdir(int job)
{
    char *ret;

    ret = dupstring(jobtab[job].pwd ? jobtab[job].pwd : pwd);
    return ret;
}

/**/
static HashNode
getpmjobdir(HashTable ht, char *name)
{
    Param pm = NULL;
    int job;

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

    if ((job = atoi(name)) >= 1 && job < MAXJOB &&
	jobtab[job].stat && jobtab[job].procs &&
	!(jobtab[job].stat & STAT_NOPRINT))
	pm->u.str = pmjobdir(job);
    else {
	pm->u.str = dupstring("");
	pm->flags |= PM_UNSET;
    }
    return (HashNode) pm;
}

/**/
static void
scanpmjobdirs(HashTable ht, ScanFunc func, int flags)
{
    struct param pm;
    int job;
    char buf[40];

    pm.flags = PM_SCALAR | PM_READONLY;
    pm.sets.cfn = NULL;
    pm.gets.cfn = strgetfn;
    pm.unsetfn = NULL;
    pm.ct = 0;
    pm.env = NULL;
    pm.ename = NULL;
    pm.old = NULL;
    pm.level = 0;

    for (job = 1; job < MAXJOB; job++) {
       if (jobtab[job].stat && jobtab[job].procs &&
           !(jobtab[job].stat & STAT_NOPRINT)) {
           if (func != scancountparams) {
	       sprintf(buf, "%d", job);
	       pm.nam = dupstring(buf);
               if ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		   !(flags & SCANPM_WANTKEYS))
		   pm.u.str = pmjobdir(job);
	   }
           func((HashNode) &pm, flags);
       }
    }
}

/* Functions for the nameddirs special parameter. */

/**/
static void
setpmnameddir(Param pm, char *value)
{
    if (!value)
	zwarn("invalid value: ''", NULL, 0);
    else {
	Nameddir nd = (Nameddir) zcalloc(sizeof(*nd));

	nd->flags = 0;
	nd->dir = value;
	nameddirtab->addnode(nameddirtab, ztrdup(pm->nam), nd);
    }
}

/**/
static void
unsetpmnameddir(Param pm, int exp)
{
    HashNode hd = nameddirtab->removenode(nameddirtab, pm->nam);

    if (hd)
	nameddirtab->freenode(hd);
}

/**/
static void
setpmnameddirs(Param pm, HashTable ht)
{
    int i;
    HashNode hn, next, hd;

    if (!ht)
	return;

    for (i = 0; i < nameddirtab->hsize; i++)
	for (hn = nameddirtab->nodes[i]; hn; hn = next) {
	    next = hn->next;
	    if (!(((Nameddir) hn)->flags & ND_USERNAME) &&
		(hd = nameddirtab->removenode(nameddirtab, hn->nam)))
		nameddirtab->freenode(hd);
	}

    for (i = 0; i < ht->hsize; i++)
	for (hn = ht->nodes[i]; hn; hn = hn->next) {
	    struct value v;
	    char *val;

	    v.isarr = v.inv = v.start = 0;
	    v.end = -1;
	    v.arr = NULL;
	    v.pm = (Param) hn;

	    if (!(val = getstrvalue(&v)))
		zwarn("invalid value: ''", NULL, 0);
	    else {
		Nameddir nd = (Nameddir) zcalloc(sizeof(*nd));

		nd->flags = 0;
		nd->dir = ztrdup(val);
		nameddirtab->addnode(nameddirtab, ztrdup(hn->nam), nd);
	    }
	}

    /* The INTERACTIVE stuff ensures that the dirs are not immediatly removed
     * when the sub-pms are deleted. */

    i = opts[INTERACTIVE];
    opts[INTERACTIVE] = 0;
    deleteparamtable(ht);
    opts[INTERACTIVE] = i;
}

/**/
static HashNode
getpmnameddir(HashTable ht, char *name)
{
    Param pm = NULL;
    Nameddir nd;

    pm = (Param) zhalloc(sizeof(struct param));
    pm->nam = dupstring(name);
    pm->flags = PM_SCALAR;
    pm->sets.cfn = setpmnameddir;
    pm->gets.cfn = strgetfn;
    pm->unsetfn = unsetpmnameddir;
    pm->ct = 0;
    pm->env = NULL;
    pm->ename = NULL;
    pm->old = NULL;
    pm->level = 0;
    if ((nd = (Nameddir) nameddirtab->getnode(nameddirtab, name)) &&
	!(nd->flags & ND_USERNAME))
	pm->u.str = dupstring(nd->dir);
    else {
	pm->u.str = dupstring("");
	pm->flags |= PM_UNSET;
    }
    return (HashNode) pm;
}

/**/
static void
scanpmnameddirs(HashTable ht, ScanFunc func, int flags)
{
    struct param pm;
    int i;
    HashNode hn;
    Nameddir nd;

    pm.flags = PM_SCALAR;
    pm.sets.cfn = setpmnameddir;
    pm.gets.cfn = strgetfn;
    pm.unsetfn = unsetpmnameddir;
    pm.ct = 0;
    pm.env = NULL;
    pm.ename = NULL;
    pm.old = NULL;
    pm.level = 0;

    for (i = 0; i < nameddirtab->hsize; i++)
	for (hn = nameddirtab->nodes[i]; hn; hn = hn->next) {
	    if (!((nd = (Nameddir) hn)->flags & ND_USERNAME)) {
		pm.nam = hn->nam;
		if (func != scancountparams &&
		    ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		     !(flags & SCANPM_WANTKEYS)))
		    pm.u.str = dupstring(nd->dir);
		func((HashNode) &pm, flags);
	    }
	}
}

/* Functions for the userdirs special parameter. */

/**/
static HashNode
getpmuserdir(HashTable ht, char *name)
{
    Param pm = NULL;
    Nameddir nd;

    nameddirtab->filltable(nameddirtab);

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
    if ((nd = (Nameddir) nameddirtab->getnode(nameddirtab, name)) &&
	(nd->flags & ND_USERNAME))
	pm->u.str = dupstring(nd->dir);
    else {
	pm->u.str = dupstring("");
	pm->flags |= PM_UNSET;
    }
    return (HashNode) pm;
}

/**/
static void
scanpmuserdirs(HashTable ht, ScanFunc func, int flags)
{
    struct param pm;
    int i;
    HashNode hn;
    Nameddir nd;

    nameddirtab->filltable(nameddirtab);

    pm.flags = PM_SCALAR | PM_READONLY;
    pm.sets.cfn = NULL;
    pm.gets.cfn = strgetfn;
    pm.unsetfn = NULL;
    pm.ct = 0;
    pm.env = NULL;
    pm.ename = NULL;
    pm.old = NULL;
    pm.level = 0;

    for (i = 0; i < nameddirtab->hsize; i++)
	for (hn = nameddirtab->nodes[i]; hn; hn = hn->next) {
	    if ((nd = (Nameddir) hn)->flags & ND_USERNAME) {
		pm.nam = hn->nam;
		if (func != scancountparams &&
		    ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		     !(flags & SCANPM_WANTKEYS)))
		    pm.u.str = dupstring(nd->dir);
		func((HashNode) &pm, flags);
	    }
	}
}

/* Functions for the regularaliases and globalaliases special parameters. */

/**/
static void
setralias(Param pm, char *value, int dis)
{
    aliastab->addnode(aliastab, ztrdup(pm->nam), createaliasnode(value, dis));
}

/**/
static void
setpmralias(Param pm, char *value)
{
    setralias(pm, value, 0);
}

/**/
static void
setpmdisralias(Param pm, char *value)
{
    setralias(pm, value, DISABLED);
}

/**/
static void
setgalias(Param pm, char *value, int dis)
{
    aliastab->addnode(aliastab, ztrdup(pm->nam),
		      createaliasnode(value, dis | ALIAS_GLOBAL));
}

/**/
static void
setpmgalias(Param pm, char *value)
{
    setgalias(pm, value, 0);
}

/**/
static void
setpmdisgalias(Param pm, char *value)
{
    setgalias(pm, value, DISABLED);
}

/**/
static void
unsetpmalias(Param pm, int exp)
{
    HashNode hd = aliastab->removenode(aliastab, pm->nam);

    if (hd)
	aliastab->freenode(hd);
}

/**/
static void
setaliases(Param pm, HashTable ht, int global, int dis)
{
    int i;
    HashNode hn, next, hd;

    if (!ht)
	return;

    for (i = 0; i < aliastab->hsize; i++)
	for (hn = aliastab->nodes[i]; hn; hn = next) {
	    next = hn->next;
	    if (((global && (((Alias) hn)->flags & ALIAS_GLOBAL)) ||
		 (!global && !(((Alias) hn)->flags & ALIAS_GLOBAL))) &&
		(hd = aliastab->removenode(aliastab, hn->nam)))
		aliastab->freenode(hd);
	}

    for (i = 0; i < ht->hsize; i++)
	for (hn = ht->nodes[i]; hn; hn = hn->next) {
	    struct value v;
	    char *val;

	    v.isarr = v.inv = v.start = 0;
	    v.end = -1;
	    v.arr = NULL;
	    v.pm = (Param) hn;

	    if ((val = getstrvalue(&v)))
		aliastab->addnode(aliastab, ztrdup(hn->nam),
				  createaliasnode(ztrdup(val),
						  (global ? ALIAS_GLOBAL : 0) |
						  (dis ? DISABLED : 0)));
	}
    deleteparamtable(ht);
}

/**/
static void
setpmraliases(Param pm, HashTable ht)
{
    setaliases(pm, ht, 0, 0);
}

/**/
static void
setpmdisraliases(Param pm, HashTable ht)
{
    setaliases(pm, ht, 0, DISABLED);
}

/**/
static void
setpmgaliases(Param pm, HashTable ht)
{
    setaliases(pm, ht, 1, 0);
}

/**/
static void
setpmdisgaliases(Param pm, HashTable ht)
{
    setaliases(pm, ht, 1, DISABLED);
}

/**/
static HashNode
getalias(HashTable ht, char *name, int global, int dis)
{
    Param pm = NULL;
    Alias al;

    pm = (Param) zhalloc(sizeof(struct param));
    pm->nam = dupstring(name);
    pm->flags = PM_SCALAR;
    pm->sets.cfn = (global ? (dis ? setpmdisgalias : setpmgalias) :
		    (dis ? setpmdisralias : setpmralias));
    pm->gets.cfn = strgetfn;
    pm->unsetfn = unsetpmalias;
    pm->ct = 0;
    pm->env = NULL;
    pm->ename = NULL;
    pm->old = NULL;
    pm->level = 0;
    if ((al = (Alias) aliastab->getnode2(aliastab, name)) &&
	((global && (al->flags & ALIAS_GLOBAL)) ||
	 (!global && !(al->flags & ALIAS_GLOBAL))) &&
	(dis ? (al->flags & DISABLED) : !(al->flags & DISABLED)))
	pm->u.str = dupstring(al->text);
    else {
	pm->u.str = dupstring("");
	pm->flags |= PM_UNSET;
    }
    return (HashNode) pm;
}

/**/
static HashNode
getpmralias(HashTable ht, char *name)
{
    return getalias(ht, name, 0, 0);
}

/**/
static HashNode
getpmdisralias(HashTable ht, char *name)
{
    return getalias(ht, name, 0, 0);
}

/**/
static HashNode
getpmgalias(HashTable ht, char *name)
{
    return getalias(ht, name, 1, 0);
}

/**/
static HashNode
getpmdisgalias(HashTable ht, char *name)
{
    return getalias(ht, name, 1, DISABLED);
}

/**/
static void
scanaliases(HashTable ht, ScanFunc func, int flags, int global, int dis)
{
    struct param pm;
    int i;
    HashNode hn;
    Alias al;

    pm.flags = PM_SCALAR;
    pm.sets.cfn = (global ? (dis ? setpmdisgalias : setpmgalias) :
		   (dis ? setpmdisralias : setpmralias));
    pm.gets.cfn = strgetfn;
    pm.unsetfn = unsetpmalias;
    pm.ct = 0;
    pm.env = NULL;
    pm.ename = NULL;
    pm.old = NULL;
    pm.level = 0;

    for (i = 0; i < aliastab->hsize; i++)
	for (hn = aliastab->nodes[i]; hn; hn = hn->next) {
	    if (((global && ((al = (Alias) hn)->flags & ALIAS_GLOBAL)) ||
		 (!global && !((al = (Alias) hn)->flags & ALIAS_GLOBAL))) &&
		(dis ? (al->flags & DISABLED) : !(al->flags & DISABLED))) {
		pm.nam = hn->nam;
		if (func != scancountparams &&
		    ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		     !(flags & SCANPM_WANTKEYS)))
		    pm.u.str = dupstring(al->text);
		func((HashNode) &pm, flags);
	    }
	}
}

/**/
static void
scanpmraliases(HashTable ht, ScanFunc func, int flags)
{
    scanaliases(ht, func, flags, 0, 0);
}

/**/
static void
scanpmdisraliases(HashTable ht, ScanFunc func, int flags)
{
    scanaliases(ht, func, flags, 0, DISABLED);
}

/**/
static void
scanpmgaliases(HashTable ht, ScanFunc func, int flags)
{
    scanaliases(ht, func, flags, 1, 0);
}

/**/
static void
scanpmdisgaliases(HashTable ht, ScanFunc func, int flags)
{
    scanaliases(ht, func, flags, 1, DISABLED);
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
    { "parameters", PM_READONLY,
      getpmparameter, scanpmparameters, hashsetfn,
      NULL, NULL, stdunsetfn, NULL },
    { "commands", 0,
      getpmcommand, scanpmcommands, setpmcommands,
      NULL, NULL, stdunsetfn, NULL },
    { "functions", 0,
      getpmfunction, scanpmfunctions, setpmfunctions,
      NULL, NULL, stdunsetfn, NULL },
    { "dis_functions", 0,
      getpmdisfunction, scanpmdisfunctions, setpmdisfunctions,
      NULL, NULL, stdunsetfn, NULL },
    { "funcstack", PM_ARRAY|PM_SPECIAL|PM_READONLY,
      NULL, NULL, NULL,
      arrsetfn, funcstackgetfn, stdunsetfn, NULL },
    { "builtins", PM_READONLY,
      getpmbuiltin, scanpmbuiltins, hashsetfn,
      NULL, NULL, stdunsetfn, NULL },
    { "dis_builtins", PM_READONLY,
      getpmdisbuiltin, scanpmdisbuiltins, hashsetfn,
      NULL, NULL, stdunsetfn, NULL },
    { "reswords", PM_ARRAY|PM_SPECIAL|PM_READONLY,
      NULL, NULL, NULL,
      arrsetfn, reswordsgetfn, stdunsetfn, NULL },
    { "dis_reswords", PM_ARRAY|PM_SPECIAL|PM_READONLY,
      NULL, NULL, NULL,
      arrsetfn, disreswordsgetfn, stdunsetfn, NULL },
    { "options", 0,
      getpmoption, scanpmoptions, setpmoptions,
      NULL, NULL, stdunsetfn, NULL },
    { "modules", PM_READONLY,
      getpmmodule, scanpmmodules, hashsetfn,
      NULL, NULL, stdunsetfn, NULL },
    { "dirstack", PM_ARRAY|PM_SPECIAL|PM_REMOVABLE,
      NULL, NULL, NULL,
      dirssetfn, dirsgetfn, stdunsetfn, NULL },
    { "history", PM_READONLY,
      getpmhistory, scanpmhistory, hashsetfn,
      NULL, NULL, stdunsetfn, NULL },
    { "historywords", PM_ARRAY|PM_SPECIAL|PM_READONLY,
      NULL, NULL, NULL,
      arrsetfn, histwgetfn, stdunsetfn, NULL },
    { "jobtexts", PM_READONLY,
      getpmjobtext, scanpmjobtexts, hashsetfn,
      NULL, NULL, stdunsetfn, NULL },
    { "jobstates", PM_READONLY,
      getpmjobstate, scanpmjobstates, hashsetfn,
      NULL, NULL, stdunsetfn, NULL },
    { "jobdirs", PM_READONLY,
      getpmjobdir, scanpmjobdirs, hashsetfn,
      NULL, NULL, stdunsetfn, NULL },
    { "nameddirs", 0,
      getpmnameddir, scanpmnameddirs, setpmnameddirs,
      NULL, NULL, stdunsetfn, NULL },
    { "userdirs", PM_READONLY,
      getpmuserdir, scanpmuserdirs, hashsetfn,
      NULL, NULL, stdunsetfn, NULL },
    { "aliases", 0,
      getpmralias, scanpmraliases, setpmraliases,
      NULL, NULL, stdunsetfn, NULL },
    { "galiases", 0,
      getpmgalias, scanpmgaliases, setpmgaliases,
      NULL, NULL, stdunsetfn, NULL },
    { "dis_aliases", 0,
      getpmdisralias, scanpmdisraliases, setpmdisraliases,
      NULL, NULL, stdunsetfn, NULL },
    { "dis_galiases", 0,
      getpmdisgalias, scanpmdisgaliases, setpmdisgaliases,
      NULL, NULL, stdunsetfn, NULL },
    { NULL, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL }
};

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
    /* Create the special associative arrays.
     * As an example for autoloaded parameters, this is probably a bad
     * example, because the zsh core doesn't support creation of
     * special hashes, yet. */

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
	    if (!(def->pm = createparam(def->name, def->flags | PM_HIDE|
					PM_HIDEVAL | PM_REMOVABLE)))
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

    incleanup = 1;

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
