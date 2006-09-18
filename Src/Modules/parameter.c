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
    pm->gsu.h = &stdhash_gsu;
    pm->u.hash = ht = newhashtable(0, name, NULL);

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
    int f = pm->node.flags;

    if (!(f & PM_UNSET)) {
	if (pm->node.flags & PM_AUTOLOAD)
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
getpmparameter(UNUSED(HashTable ht), char *name)
{
    Param rpm, pm = NULL;

    pm = (Param) hcalloc(sizeof(struct param));
    pm->node.nam = dupstring(name);
    pm->node.flags = PM_SCALAR | PM_READONLY;
    pm->gsu.s = &nullsetscalar_gsu;
    if ((rpm = (Param) realparamtab->getnode(realparamtab, name)) &&
	!(rpm->node.flags & PM_UNSET))
	pm->u.str = paramtypestr(rpm);
    else {
	pm->u.str = dupstring("");
	pm->node.flags |= PM_UNSET;
    }
    return &pm->node;
}

/**/
static void
scanpmparameters(UNUSED(HashTable ht), ScanFunc func, int flags)
{
    struct param pm;
    int i;
    HashNode hn;

    memset((void *)&pm, 0, sizeof(struct param));
    pm.node.flags = PM_SCALAR | PM_READONLY;
    pm.gsu.s = &nullsetscalar_gsu;

    for (i = 0; i < realparamtab->hsize; i++)
	for (hn = realparamtab->nodes[i]; hn; hn = hn->next) {
	    if (((Param)hn)->node.flags & PM_UNSET)
		continue;
	    pm.node.nam = hn->nam;
	    if (func != scancountparams &&
		((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		 !(flags & SCANPM_WANTKEYS)))
		pm.u.str = paramtypestr((Param) hn);
	    func(&pm.node, flags);
	}
}

/* Functions for the commands special parameter. */

/**/
static void
setpmcommand(Param pm, char *value)
{
    if (isset(RESTRICTED)) {
	zwarn("restricted: %s", value);
	zsfree(value);
    } else {
	Cmdnam cn = zshcalloc(sizeof(*cn));

	cn->node.flags = HASHED;
	cn->u.cmd = value;

	cmdnamtab->addnode(cmdnamtab, ztrdup(pm->node.nam), &cn->node);
    }
}

/**/
static void
unsetpmcommand(Param pm, UNUSED(int exp))
{
    HashNode hn = cmdnamtab->removenode(cmdnamtab, pm->node.nam);

    if (hn)
	cmdnamtab->freenode(hn);
}

/**/
static void
setpmcommands(UNUSED(Param pm), HashTable ht)
{
    int i;
    HashNode hn;

    if (!ht)
	return;

    for (i = 0; i < ht->hsize; i++)
	for (hn = ht->nodes[i]; hn; hn = hn->next) {
	    Cmdnam cn = zshcalloc(sizeof(*cn));
	    struct value v;

	    v.isarr = v.inv = v.start = 0;
	    v.end = -1;
	    v.arr = NULL;
	    v.pm = (Param) hn;

	    cn->node.flags = HASHED;
	    cn->u.cmd = ztrdup(getstrvalue(&v));

	    cmdnamtab->addnode(cmdnamtab, ztrdup(hn->nam), &cn->node);
	}
    deleteparamtable(ht);
}

static const struct gsu_scalar pmcommand_gsu =
{ strgetfn, setpmcommand, unsetpmcommand };


/**/
static HashNode
getpmcommand(UNUSED(HashTable ht), char *name)
{
    Cmdnam cmd;
    Param pm = NULL;

    if (!(cmd = (Cmdnam) cmdnamtab->getnode(cmdnamtab, name)) &&
	isset(HASHLISTALL)) {
	cmdnamtab->filltable(cmdnamtab);
	cmd = (Cmdnam) cmdnamtab->getnode(cmdnamtab, name);
    }
    pm = (Param) hcalloc(sizeof(struct param));
    pm->node.nam = dupstring(name);
    pm->node.flags = PM_SCALAR;
    pm->gsu.s = &pmcommand_gsu;
    if (cmd) {
	if (cmd->node.flags & HASHED)
	    pm->u.str = cmd->u.cmd;
	else {
	    pm->u.str = zhalloc(strlen(*(cmd->u.name)) + strlen(name) + 2);
	    strcpy(pm->u.str, *(cmd->u.name));
	    strcat(pm->u.str, "/");
	    strcat(pm->u.str, name);
	}
    } else {
	pm->u.str = dupstring("");
	pm->node.flags |= PM_UNSET;
    }
    return &pm->node;
}

/**/
static void
scanpmcommands(UNUSED(HashTable ht), ScanFunc func, int flags)
{
    struct param pm;
    int i;
    HashNode hn;
    Cmdnam cmd;

    if (isset(HASHLISTALL))
	cmdnamtab->filltable(cmdnamtab);

    memset((void *)&pm, 0, sizeof(struct param));
    pm.node.flags = PM_SCALAR;
    pm.gsu.s = &pmcommand_gsu;

    for (i = 0; i < cmdnamtab->hsize; i++)
	for (hn = cmdnamtab->nodes[i]; hn; hn = hn->next) {
	    pm.node.nam = hn->nam;
	    cmd = (Cmdnam) hn;
	    if (func != scancountparams &&
		((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		 !(flags & SCANPM_WANTKEYS))) {
		if (cmd->node.flags & HASHED)
		    pm.u.str = cmd->u.cmd;
		else {
		    pm.u.str = zhalloc(strlen(*(cmd->u.name)) +
				       strlen(cmd->node.nam) + 2);
		    strcpy(pm.u.str, *(cmd->u.name));
		    strcat(pm.u.str, "/");
		    strcat(pm.u.str, cmd->node.nam);
		}
	    }
	    func(&pm.node, flags);
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
	zwarn("invalid function definition", value);
	zsfree(val);
	return;
    }
    shf = (Shfunc) zalloc(sizeof(*shf));
    shf->funcdef = dupeprog(prog, 0);
    shf->node.flags = dis;

    if (!strncmp(name, "TRAP", 4) &&
	(sn = getsignum(name + 4)) != -1) {
	if (settrap(sn, NULL, ZSIG_FUNC)) {
	    freeeprog(shf->funcdef);
	    zfree(shf, sizeof(*shf));
	    zsfree(val);
	    return;
	}
    }
    shfunctab->addnode(shfunctab, ztrdup(name), shf);
    zsfree(val);
}

/**/
static void
setpmfunction(Param pm, char *value)
{
    setfunction(pm->node.nam, value, 0);
}

/**/
static void
setpmdisfunction(Param pm, char *value)
{
    setfunction(pm->node.nam, value, DISABLED);
}

/**/
static void
unsetpmfunction(Param pm, UNUSED(int exp))
{
    HashNode hn = shfunctab->removenode(shfunctab, pm->node.nam);

    if (hn)
	shfunctab->freenode(hn);
}

/**/
static void
setfunctions(UNUSED(Param pm), HashTable ht, int dis)
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

static const struct gsu_scalar pmfunction_gsu =
{ strgetfn, setpmfunction, unsetpmfunction };
static const struct gsu_scalar pmdisfunction_gsu =
{ strgetfn, setpmdisfunction, unsetpmfunction };

/**/
static HashNode
getfunction(UNUSED(HashTable ht), char *name, int dis)
{
    Shfunc shf;
    Param pm = NULL;

    pm = (Param) hcalloc(sizeof(struct param));
    pm->node.nam = dupstring(name);
    pm->node.flags = PM_SCALAR;
    pm->gsu.s = dis ? &pmdisfunction_gsu :  &pmfunction_gsu;

    if ((shf = (Shfunc) shfunctab->getnode2(shfunctab, name)) &&
	(dis ? (shf->node.flags & DISABLED) : !(shf->node.flags & DISABLED))) {
	if (shf->node.flags & PM_UNDEFINED) {
	    pm->u.str = dyncat("builtin autoload -X",
			       ((shf->node.flags & PM_UNALIASED) ?
				((shf->node.flags & PM_TAGGED) ? "Ut" : "U") :
				((shf->node.flags & PM_TAGGED) ? "t" : "")));
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
	pm->node.flags |= PM_UNSET;
    }
    return &pm->node;
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
scanfunctions(UNUSED(HashTable ht), ScanFunc func, int flags, int dis)
{
    struct param pm;
    int i;
    HashNode hn;

    memset((void *)&pm, 0, sizeof(struct param));
    pm.node.flags = PM_SCALAR;
    pm.gsu.s = dis ? &pmdisfunction_gsu : &pmfunction_gsu;

    for (i = 0; i < shfunctab->hsize; i++)
	for (hn = shfunctab->nodes[i]; hn; hn = hn->next) {
	    if (dis ? (hn->flags & DISABLED) : !(hn->flags & DISABLED)) {
		pm.node.nam = hn->nam;
		if (func != scancountparams &&
		    ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		     !(flags & SCANPM_WANTKEYS))) {
		    if (((Shfunc) hn)->node.flags & PM_UNDEFINED) {
			Shfunc shf = (Shfunc) hn;
			pm.u.str =
			    dyncat("builtin autoload -X",
				   ((shf->node.flags & PM_UNALIASED) ?
				    ((shf->node.flags & PM_TAGGED) ? "Ut" : "U") :
				    ((shf->node.flags & PM_TAGGED) ? "t" : "")));
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
		func(&pm.node, flags);
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
funcstackgetfn(UNUSED(Param pm))
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

/* Functions for the functrace special parameter. */

/**/
static char **
functracegetfn(UNUSED(Param pm))
{
    Funcstack f;
    int num;
    char **ret, **p;

    for (f = funcstack, num = 0; f; f = f->prev, num++);

    ret = (char **) zhalloc((num + 1) * sizeof(char *));

    for (f = funcstack, p = ret; f; f = f->prev, p++) {
	char *colonpair;

	colonpair = zhalloc(strlen(f->caller) + (f->lineno > 9999 ? 24 : 6));
	sprintf(colonpair, "%s:%d", f->caller, f->lineno);

	*p = colonpair;
    }
    *p = NULL;

    return ret;
}

/* Functions for the builtins special parameter. */

/**/
static HashNode
getbuiltin(UNUSED(HashTable ht), char *name, int dis)
{
    Param pm = NULL;
    Builtin bn;

    pm = (Param) hcalloc(sizeof(struct param));
    pm->node.nam = dupstring(name);
    pm->node.flags = PM_SCALAR | PM_READONLY;
    pm->gsu.s = &nullsetscalar_gsu;
    if ((bn = (Builtin) builtintab->getnode2(builtintab, name)) &&
	(dis ? (bn->node.flags & DISABLED) : !(bn->node.flags & DISABLED))) {
	char *t = ((bn->handlerfunc || (bn->node.flags & BINF_PREFIX)) ?
		   "defined" : "undefined");

	pm->u.str = dupstring(t);
    } else {
	pm->u.str = dupstring("");
	pm->node.flags |= PM_UNSET;
    }
    return &pm->node;
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
scanbuiltins(UNUSED(HashTable ht), ScanFunc func, int flags, int dis)
{
    struct param pm;
    int i;
    HashNode hn;

    memset((void *)&pm, 0, sizeof(struct param));
    pm.node.flags = PM_SCALAR | PM_READONLY;
    pm.gsu.s = &nullsetscalar_gsu;

    for (i = 0; i < builtintab->hsize; i++)
	for (hn = builtintab->nodes[i]; hn; hn = hn->next) {
	    if (dis ? (hn->flags & DISABLED) : !(hn->flags & DISABLED)) {
		pm.node.nam = hn->nam;
		if (func != scancountparams &&
		    ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		     !(flags & SCANPM_WANTKEYS))) {
		    char *t = ((((Builtin) hn)->handlerfunc ||
				(hn->flags & BINF_PREFIX)) ?
			       "defined" : "undefined");

		    pm.u.str = dupstring(t);
		}
		func(&pm.node, flags);
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
reswordsgetfn(UNUSED(Param pm))
{
    return getreswords(0);
}

/**/
static char **
disreswordsgetfn(UNUSED(Param pm))
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
	zwarn("invalid value: %s", value);
    else if (!(n = optlookup(pm->node.nam)))
	zwarn("no such option: %s", pm->node.nam);
    else if (dosetopt(n, (value && strcmp(value, "off")), 0))
	zwarn("can't change option: %s", pm->node.nam);
    zsfree(value);
}

/**/
static void
unsetpmoption(Param pm, UNUSED(int exp))
{
    int n;

    if (!(n = optlookup(pm->node.nam)))
	zwarn("no such option: %s", pm->node.nam);
    else if (dosetopt(n, 0, 0))
	zwarn("can't change option: %s", pm->node.nam);
}

/**/
static void
setpmoptions(UNUSED(Param pm), HashTable ht)
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
		zwarn("invalid value: %s", val);
	    else if (dosetopt(optlookup(hn->nam),
			      (val && strcmp(val, "off")), 0))
		zwarn("can't change option: %s", hn->nam);
	}
    deleteparamtable(ht);
}

static const struct gsu_scalar pmoption_gsu =
{ strgetfn, setpmoption, unsetpmoption };

/**/
static HashNode
getpmoption(UNUSED(HashTable ht), char *name)
{
    Param pm = NULL;
    int n;

    pm = (Param) hcalloc(sizeof(struct param));
    pm->node.nam = dupstring(name);
    pm->node.flags = PM_SCALAR;
    pm->gsu.s = &pmoption_gsu;

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
	pm->node.flags |= PM_UNSET;
    }
    return &pm->node;
}

/**/
static void
scanpmoptions(UNUSED(HashTable ht), ScanFunc func, int flags)
{
    struct param pm;
    int i;
    HashNode hn;

    memset((void *)&pm, 0, sizeof(struct param));
    pm.node.flags = PM_SCALAR;
    pm.gsu.s = &pmoption_gsu;

    for (i = 0; i < optiontab->hsize; i++)
	for (hn = optiontab->nodes[i]; hn; hn = hn->next) {
	    int optno = ((Optname) hn)->optno, ison;
	    pm.node.nam = hn->nam;
	    ison = optno < 0 ? !opts[-optno] : opts[optno];
	    pm.u.str = dupstring(ison ? "on" : "off");
	    func(&pm.node, flags);
	}
}

/* Functions for the modules special parameter. */

static char *modpmname;
static int modpmfound;

/**/
static void
modpmbuiltinscan(HashNode hn, UNUSED(int dummy))
{
    if (!(((Builtin) hn)->node.flags & BINF_ADDED) &&
	!strcmp(((Builtin) hn)->optstr, modpmname))
	modpmfound = 1;
}

/**/
static void
modpmparamscan(HashNode hn, UNUSED(int dummy))
{
    if ((((Param) hn)->node.flags & PM_AUTOLOAD) &&
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
getpmmodule(UNUSED(HashTable ht), char *name)
{
    Param pm = NULL;
    char *type = NULL;
    LinkNode node;

    pm = (Param) hcalloc(sizeof(struct param));
    pm->node.nam = dupstring(name);
    pm->node.flags = PM_SCALAR | PM_READONLY;
    pm->gsu.s = &nullsetscalar_gsu;

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
	pm->node.flags |= PM_UNSET;
    }
    return &pm->node;
}

/**/
static void
scanpmmodules(UNUSED(HashTable ht), ScanFunc func, int flags)
{
    struct param pm;
    int i;
    HashNode hn;
    LinkList done = newlinklist();
    LinkNode node;
    Module m;
    Conddef p;
    char *loaded = dupstring("loaded");

    memset((void *)&pm, 0, sizeof(struct param));
    pm.node.flags = PM_SCALAR | PM_READONLY;
    pm.gsu.s = &nullsetscalar_gsu;

    for (node = firstnode(modules); node; incnode(node)) {
	m = (Module) getdata(node);
	if (m->u.handle && !(m->flags & MOD_UNLOAD)) {
	    pm.node.nam = m->nam;
	    pm.u.str = ((m->flags & MOD_ALIAS) ?
			dyncat("alias:", m->u.alias) : loaded);
	    addlinknode(done, pm.node.nam);
	    func(&pm.node, flags);
	}
    }
    pm.u.str = dupstring("autoloaded");
    for (i = 0; i < builtintab->hsize; i++)
	for (hn = builtintab->nodes[i]; hn; hn = hn->next) {
	    if (!(((Builtin) hn)->node.flags & BINF_ADDED) &&
		!findmodnode(done, ((Builtin) hn)->optstr)) {
		pm.node.nam = ((Builtin) hn)->optstr;
		addlinknode(done, pm.node.nam);
		func(&pm.node, flags);
	    }
	}
    for (p = condtab; p; p = p->next)
	if (p->module && !findmodnode(done, p->module)) {
	    pm.node.nam = p->module;
	    addlinknode(done, pm.node.nam);
	    func(&pm.node, flags);
	}
    for (i = 0; i < realparamtab->hsize; i++)
	for (hn = realparamtab->nodes[i]; hn; hn = hn->next) {
	    if ((((Param) hn)->node.flags & PM_AUTOLOAD) &&
		!findmodnode(done, ((Param) hn)->u.str)) {
		pm.node.nam = ((Param) hn)->u.str;
		addlinknode(done, pm.node.nam);
		func(&pm.node, flags);
	    }
	}
}

/* Functions for the dirstack special parameter. */

/**/
static void
dirssetfn(UNUSED(Param pm), char **x)
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
dirsgetfn(UNUSED(Param pm))
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
getpmhistory(UNUSED(HashTable ht), char *name)
{
    Param pm = NULL;
    Histent he;
    char *p;
    int ok = 1;

    pm = (Param) hcalloc(sizeof(struct param));
    pm->node.nam = dupstring(name);
    pm->node.flags = PM_SCALAR | PM_READONLY;
    pm->gsu.s = &nullsetscalar_gsu;

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
	pm->u.str = dupstring(he->node.nam);
    else {
	pm->u.str = dupstring("");
	pm->node.flags |= PM_UNSET;
    }
    return &pm->node;
}

/**/
static void
scanpmhistory(UNUSED(HashTable ht), ScanFunc func, int flags)
{
    struct param pm;
    int i = addhistnum(curhist, -1, HIST_FOREIGN);
    Histent he = gethistent(i, GETHIST_UPWARD);
    char buf[40];

    memset((void *)&pm, 0, sizeof(struct param));
    pm.node.flags = PM_SCALAR | PM_READONLY;
    pm.gsu.s = &nullsetscalar_gsu;

    while (he) {
	if (func != scancountparams) {
	    convbase(buf, he->histnum, 10);
	    pm.node.nam = dupstring(buf);
	    if ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		!(flags & SCANPM_WANTKEYS))
		pm.u.str = dupstring(he->node.nam);
	}
	func(&pm.node, flags);

	he = up_histent(he);
    }
}

/* Function for the historywords special parameter. */

/**/
static char **
histwgetfn(UNUSED(Param pm))
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
	    h = he->node.nam + he->words[iw * 2];
	    e = he->node.nam + he->words[iw * 2 + 1];
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
getpmjobtext(UNUSED(HashTable ht), char *name)
{
    Param pm = NULL;
    int job;

    pm = (Param) hcalloc(sizeof(struct param));
    pm->node.nam = dupstring(name);
    pm->node.flags = PM_SCALAR | PM_READONLY;
    pm->gsu.s = &nullsetscalar_gsu;

    if ((job = atoi(name)) >= 1 && job <= maxjob &&
	jobtab[job].stat && jobtab[job].procs &&
	!(jobtab[job].stat & STAT_NOPRINT))
	pm->u.str = pmjobtext(job);
    else {
	pm->u.str = dupstring("");
	pm->node.flags |= PM_UNSET;
    }
    return &pm->node;
}

/**/
static void
scanpmjobtexts(UNUSED(HashTable ht), ScanFunc func, int flags)
{
    struct param pm;
    int job;
    char buf[40];

    memset((void *)&pm, 0, sizeof(struct param));
    pm.node.flags = PM_SCALAR | PM_READONLY;
    pm.gsu.s = &nullsetscalar_gsu;

    for (job = 1; job <= maxjob; job++) {
	if (jobtab[job].stat && jobtab[job].procs &&
	    !(jobtab[job].stat & STAT_NOPRINT)) {
	    if (func != scancountparams) {
		sprintf(buf, "%d", job);
		pm.node.nam = dupstring(buf);
		if ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		    !(flags & SCANPM_WANTKEYS))
		    pm.u.str = pmjobtext(job);
	    }
	    func(&pm.node, flags);
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
getpmjobstate(UNUSED(HashTable ht), char *name)
{
    Param pm = NULL;
    int job;

    pm = (Param) hcalloc(sizeof(struct param));
    pm->node.nam = dupstring(name);
    pm->node.flags = PM_SCALAR | PM_READONLY;
    pm->gsu.s = &nullsetscalar_gsu;

    if ((job = atoi(name)) >= 1 && job <= maxjob &&
	jobtab[job].stat && jobtab[job].procs &&
	!(jobtab[job].stat & STAT_NOPRINT))
	pm->u.str = pmjobstate(job);
    else {
	pm->u.str = dupstring("");
	pm->node.flags |= PM_UNSET;
    }
    return &pm->node;
}

/**/
static void
scanpmjobstates(UNUSED(HashTable ht), ScanFunc func, int flags)
{
    struct param pm;
    int job;
    char buf[40];

    memset((void *)&pm, 0, sizeof(struct param));
    pm.node.flags = PM_SCALAR | PM_READONLY;
    pm.gsu.s = &nullsetscalar_gsu;

    for (job = 1; job <= maxjob; job++) {
	if (jobtab[job].stat && jobtab[job].procs &&
	    !(jobtab[job].stat & STAT_NOPRINT)) {
	    if (func != scancountparams) {
		sprintf(buf, "%d", job);
		pm.node.nam = dupstring(buf);
		if ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		    !(flags & SCANPM_WANTKEYS))
		    pm.u.str = pmjobstate(job);
	    }
	    func(&pm.node, flags);
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
getpmjobdir(UNUSED(HashTable ht), char *name)
{
    Param pm = NULL;
    int job;

    pm = (Param) hcalloc(sizeof(struct param));
    pm->node.nam = dupstring(name);
    pm->node.flags = PM_SCALAR | PM_READONLY;
    pm->gsu.s = &nullsetscalar_gsu;

    if ((job = atoi(name)) >= 1 && job <= maxjob &&
	jobtab[job].stat && jobtab[job].procs &&
	!(jobtab[job].stat & STAT_NOPRINT))
	pm->u.str = pmjobdir(job);
    else {
	pm->u.str = dupstring("");
	pm->node.flags |= PM_UNSET;
    }
    return &pm->node;
}

/**/
static void
scanpmjobdirs(UNUSED(HashTable ht), ScanFunc func, int flags)
{
    struct param pm;
    int job;
    char buf[40];

    memset((void *)&pm, 0, sizeof(struct param));
    pm.node.flags = PM_SCALAR | PM_READONLY;
    pm.gsu.s = &nullsetscalar_gsu;

    for (job = 1; job <= maxjob; job++) {
       if (jobtab[job].stat && jobtab[job].procs &&
           !(jobtab[job].stat & STAT_NOPRINT)) {
           if (func != scancountparams) {
	       sprintf(buf, "%d", job);
	       pm.node.nam = dupstring(buf);
               if ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		   !(flags & SCANPM_WANTKEYS))
		   pm.u.str = pmjobdir(job);
	   }
           func(&pm.node, flags);
       }
    }
}

/* Functions for the nameddirs special parameter. */

/**/
static void
setpmnameddir(Param pm, char *value)
{
    if (!value)
	zwarn("invalid value: ''");
    else {
	Nameddir nd = (Nameddir) zshcalloc(sizeof(*nd));

	nd->node.flags = 0;
	nd->dir = value;
	nameddirtab->addnode(nameddirtab, ztrdup(pm->node.nam), nd);
    }
}

/**/
static void
unsetpmnameddir(Param pm, UNUSED(int exp))
{
    HashNode hd = nameddirtab->removenode(nameddirtab, pm->node.nam);

    if (hd)
	nameddirtab->freenode(hd);
}

/**/
static void
setpmnameddirs(UNUSED(Param pm), HashTable ht)
{
    int i;
    HashNode hn, next, hd;

    if (!ht)
	return;

    for (i = 0; i < nameddirtab->hsize; i++)
	for (hn = nameddirtab->nodes[i]; hn; hn = next) {
	    next = hn->next;
	    if (!(((Nameddir) hn)->node.flags & ND_USERNAME) &&
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
		zwarn("invalid value: ''");
	    else {
		Nameddir nd = (Nameddir) zshcalloc(sizeof(*nd));

		nd->node.flags = 0;
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

static const struct gsu_scalar pmnamedir_gsu =
{ strgetfn, setpmnameddir, unsetpmnameddir };

/**/
static HashNode
getpmnameddir(UNUSED(HashTable ht), char *name)
{
    Param pm = NULL;
    Nameddir nd;

    pm = (Param) hcalloc(sizeof(struct param));
    pm->node.nam = dupstring(name);
    pm->node.flags = PM_SCALAR;
    pm->gsu.s = &pmnamedir_gsu;
    if ((nd = (Nameddir) nameddirtab->getnode(nameddirtab, name)) &&
	!(nd->node.flags & ND_USERNAME))
	pm->u.str = dupstring(nd->dir);
    else {
	pm->u.str = dupstring("");
	pm->node.flags |= PM_UNSET;
    }
    return &pm->node;
}

/**/
static void
scanpmnameddirs(UNUSED(HashTable ht), ScanFunc func, int flags)
{
    struct param pm;
    int i;
    HashNode hn;
    Nameddir nd;

    memset((void *)&pm, 0, sizeof(struct param));
    pm.node.flags = PM_SCALAR;
    pm.gsu.s = &pmnamedir_gsu;

    for (i = 0; i < nameddirtab->hsize; i++)
	for (hn = nameddirtab->nodes[i]; hn; hn = hn->next) {
	    if (!((nd = (Nameddir) hn)->node.flags & ND_USERNAME)) {
		pm.node.nam = hn->nam;
		if (func != scancountparams &&
		    ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		     !(flags & SCANPM_WANTKEYS)))
		    pm.u.str = dupstring(nd->dir);
		func(&pm.node, flags);
	    }
	}
}

/* Functions for the userdirs special parameter. */

/**/
static HashNode
getpmuserdir(UNUSED(HashTable ht), char *name)
{
    Param pm = NULL;
    Nameddir nd;

    nameddirtab->filltable(nameddirtab);

    pm = (Param) hcalloc(sizeof(struct param));
    pm->node.nam = dupstring(name);
    pm->node.flags = PM_SCALAR | PM_READONLY;
    pm->gsu.s = &nullsetscalar_gsu;
    if ((nd = (Nameddir) nameddirtab->getnode(nameddirtab, name)) &&
	(nd->node.flags & ND_USERNAME))
	pm->u.str = dupstring(nd->dir);
    else {
	pm->u.str = dupstring("");
	pm->node.flags |= PM_UNSET;
    }
    return &pm->node;
}

/**/
static void
scanpmuserdirs(UNUSED(HashTable ht), ScanFunc func, int flags)
{
    struct param pm;
    int i;
    HashNode hn;
    Nameddir nd;

    nameddirtab->filltable(nameddirtab);

    memset((void *)&pm, 0, sizeof(struct param));
    pm.node.flags = PM_SCALAR | PM_READONLY;
    pm.gsu.s = &nullsetscalar_gsu;

    for (i = 0; i < nameddirtab->hsize; i++)
	for (hn = nameddirtab->nodes[i]; hn; hn = hn->next) {
	    if ((nd = (Nameddir) hn)->node.flags & ND_USERNAME) {
		pm.node.nam = hn->nam;
		if (func != scancountparams &&
		    ((flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		     !(flags & SCANPM_WANTKEYS)))
		    pm.u.str = dupstring(nd->dir);
		func(&pm.node, flags);
	    }
	}
}

/* Functions for the raliases, galiases and saliases special parameters. */

/**/
static void
setalias(HashTable ht, Param pm, char *value, int flags)
{
    ht->addnode(ht, ztrdup(pm->node.nam),
		createaliasnode(value, flags));
}

/**/
static void
setpmralias(Param pm, char *value)
{
    setalias(aliastab, pm, value, 0);
}

/**/
static void
setpmdisralias(Param pm, char *value)
{
    setalias(aliastab, pm, value, DISABLED);
}

/**/
static void
setpmgalias(Param pm, char *value)
{
    setalias(aliastab, pm, value, ALIAS_GLOBAL);
}

/**/
static void
setpmdisgalias(Param pm, char *value)
{
    setalias(aliastab, pm, value, ALIAS_GLOBAL|DISABLED);
}

/**/
static void
setpmsalias(Param pm, char *value)
{
    setalias(sufaliastab, pm, value, ALIAS_SUFFIX);
}

/**/
static void
setpmdissalias(Param pm, char *value)
{
    setalias(sufaliastab, pm, value, ALIAS_SUFFIX|DISABLED);
}

/**/
static void
unsetpmalias(Param pm, UNUSED(int exp))
{
    HashNode hd = aliastab->removenode(aliastab, pm->node.nam);

    if (hd)
	aliastab->freenode(hd);
}

/**/
static void
unsetpmsalias(Param pm, UNUSED(int exp))
{
    HashNode hd = sufaliastab->removenode(sufaliastab, pm->node.nam);

    if (hd)
	sufaliastab->freenode(hd);
}

/**/
static void
setaliases(HashTable alht, UNUSED(Param pm), HashTable ht, int flags)
{
    int i;
    HashNode hn, next, hd;

    if (!ht)
	return;

    for (i = 0; i < alht->hsize; i++)
	for (hn = alht->nodes[i]; hn; hn = next) {
	    next = hn->next;
	    /*
	     * The following respects the DISABLED flag, e.g.
	     * we get a different behaviour for raliases and dis_raliases.
	     * The predecessor to this code didn't do that; presumably
	     * that was a bug.
	     */
	    if (flags == ((Alias)hn)->node.flags &&
		(hd = alht->removenode(alht, hn->nam)))
		alht->freenode(hd);
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
		alht->addnode(alht, ztrdup(hn->nam),
			      createaliasnode(ztrdup(val), flags));
	}
    deleteparamtable(ht);
}

/**/
static void
setpmraliases(Param pm, HashTable ht)
{
    setaliases(aliastab, pm, ht, 0);
}

/**/
static void
setpmdisraliases(Param pm, HashTable ht)
{
    setaliases(aliastab, pm, ht, DISABLED);
}

/**/
static void
setpmgaliases(Param pm, HashTable ht)
{
    setaliases(aliastab, pm, ht, ALIAS_GLOBAL);
}

/**/
static void
setpmdisgaliases(Param pm, HashTable ht)
{
    setaliases(aliastab, pm, ht, ALIAS_GLOBAL|DISABLED);
}

/**/
static void
setpmsaliases(Param pm, HashTable ht)
{
    setaliases(sufaliastab, pm, ht, ALIAS_SUFFIX);
}

/**/
static void
setpmdissaliases(Param pm, HashTable ht)
{
    setaliases(sufaliastab, pm, ht, ALIAS_SUFFIX|DISABLED);
}

static const struct gsu_scalar pmralias_gsu =
{ strgetfn, setpmralias, unsetpmalias };
static const struct gsu_scalar pmgalias_gsu =
{ strgetfn, setpmgalias, unsetpmalias };
static const struct gsu_scalar pmsalias_gsu =
{ strgetfn, setpmsalias, unsetpmsalias };
static const struct gsu_scalar pmdisralias_gsu =
{ strgetfn, setpmdisralias, unsetpmalias };
static const struct gsu_scalar pmdisgalias_gsu =
{ strgetfn, setpmdisgalias, unsetpmalias };
static const struct gsu_scalar pmdissalias_gsu =
{ strgetfn, setpmdissalias, unsetpmsalias };

/**/
static void
assignaliasdefs(Param pm, int flags)
{
    pm->node.flags = PM_SCALAR;

    /* we really need to squirrel the flags away somewhere... */
    switch (flags) {
    case 0:
	pm->gsu.s = &pmralias_gsu;
	break;

    case ALIAS_GLOBAL:
	pm->gsu.s = &pmgalias_gsu;
	break;

    case ALIAS_SUFFIX:
	pm->gsu.s = &pmsalias_gsu;
	break;

    case DISABLED:
	pm->gsu.s = &pmdisralias_gsu;
	break;

    case ALIAS_GLOBAL|DISABLED:
	pm->gsu.s = &pmdisgalias_gsu;
	break;

    case ALIAS_SUFFIX|DISABLED:
	pm->gsu.s = &pmdissalias_gsu;
	break;
    }
}

/**/
static HashNode
getalias(HashTable alht, UNUSED(HashTable ht), char *name, int flags)
{
    Param pm = NULL;
    Alias al;

    pm = (Param) hcalloc(sizeof(struct param));
    pm->node.nam = dupstring(name);

    assignaliasdefs(pm, flags);

    if ((al = (Alias) alht->getnode2(alht, name)) &&
	flags == al->node.flags)
	pm->u.str = dupstring(al->text);
    else {
	pm->u.str = dupstring("");
	pm->node.flags |= PM_UNSET;
    }
    return &pm->node;
}

/**/
static HashNode
getpmralias(HashTable ht, char *name)
{
    return getalias(aliastab, ht, name, 0);
}

/**/
static HashNode
getpmdisralias(HashTable ht, char *name)
{
    return getalias(aliastab, ht, name, DISABLED);
}

/**/
static HashNode
getpmgalias(HashTable ht, char *name)
{
    return getalias(aliastab, ht, name, ALIAS_GLOBAL);
}

/**/
static HashNode
getpmdisgalias(HashTable ht, char *name)
{
    return getalias(aliastab, ht, name, ALIAS_GLOBAL|DISABLED);
}

/**/
static HashNode
getpmsalias(HashTable ht, char *name)
{
    return getalias(sufaliastab, ht, name, ALIAS_SUFFIX);
}

/**/
static HashNode
getpmdissalias(HashTable ht, char *name)
{
    return getalias(sufaliastab, ht, name, ALIAS_SUFFIX|DISABLED);
}

/**/
static void
scanaliases(HashTable alht, UNUSED(HashTable ht), ScanFunc func,
	    int pmflags, int alflags)
{
    struct param pm;
    int i;
    Alias al;

    memset((void *)&pm, 0, sizeof(struct param));
    assignaliasdefs(&pm, alflags);

    for (i = 0; i < alht->hsize; i++)
	for (al = (Alias) alht->nodes[i]; al; al = (Alias) al->node.next) {
	    if (alflags == al->node.flags) {
		pm.node.nam = al->node.nam;
		if (func != scancountparams &&
		    ((pmflags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)) ||
		     !(pmflags & SCANPM_WANTKEYS)))
		    pm.u.str = dupstring(al->node.nam);
		func(&pm.node, pmflags);
	    }
	}
}

/**/
static void
scanpmraliases(HashTable ht, ScanFunc func, int flags)
{
    scanaliases(aliastab, ht, func, flags, 0);
}

/**/
static void
scanpmdisraliases(HashTable ht, ScanFunc func, int flags)
{
    scanaliases(aliastab, ht, func, flags, DISABLED);
}

/**/
static void
scanpmgaliases(HashTable ht, ScanFunc func, int flags)
{
    scanaliases(aliastab, ht, func, flags, ALIAS_GLOBAL);
}

/**/
static void
scanpmdisgaliases(HashTable ht, ScanFunc func, int flags)
{
    scanaliases(aliastab, ht, func, flags, ALIAS_GLOBAL|DISABLED);
}

/**/
static void
scanpmsaliases(HashTable ht, ScanFunc func, int flags)
{
    scanaliases(sufaliastab, ht, func, flags, ALIAS_SUFFIX);
}

/**/
static void
scanpmdissaliases(HashTable ht, ScanFunc func, int flags)
{
    scanaliases(sufaliastab, ht, func, flags, ALIAS_SUFFIX|DISABLED);
}

/* Table for defined parameters. */

struct pardef {
    char *name;
    int flags;
    GetNodeFunc getnfn;
    ScanTabFunc scantfn;
    GsuHash hash_gsu;
    GsuArray array_gsu;
    Param pm;
};

/*
 * This is a duplicate of nullsethash_gsu.  On some systems
 * (such as Cygwin) we can't put a pointer to an imported variable
 * in a compile-time initialiser, so we use this instead.
 */
static const struct gsu_hash pmnullsethash_gsu =
{ hashgetfn, nullsethashfn, nullunsetfn };
static const struct gsu_hash pmcommands_gsu =
{ hashgetfn, setpmcommands, stdunsetfn };
static const struct gsu_hash pmfunctions_gsu =
{ hashgetfn, setpmfunctions, stdunsetfn };
static const struct gsu_hash pmdisfunctions_gsu =
{ hashgetfn, setpmdisfunctions, stdunsetfn };
static const struct gsu_hash pmoptions_gsu =
{ hashgetfn, setpmoptions, stdunsetfn };
static const struct gsu_hash pmnameddirs_gsu =
{ hashgetfn, setpmnameddirs, stdunsetfn };
static const struct gsu_hash pmraliases_gsu =
{ hashgetfn, setpmraliases, stdunsetfn };
static const struct gsu_hash pmgaliases_gsu =
{ hashgetfn, setpmgaliases, stdunsetfn };
static const struct gsu_hash pmsaliases_gsu =
{ hashgetfn, setpmsaliases, stdunsetfn };
static const struct gsu_hash pmdisraliases_gsu =
{ hashgetfn, setpmdisraliases, stdunsetfn };
static const struct gsu_hash pmdisgaliases_gsu =
{ hashgetfn, setpmdisgaliases, stdunsetfn };
static const struct gsu_hash pmdissaliases_gsu =
{ hashgetfn, setpmdissaliases, stdunsetfn };

static const struct gsu_array funcstack_gsu =
{ funcstackgetfn, arrsetfn, stdunsetfn };
static const struct gsu_array functrace_gsu =
{ functracegetfn, arrsetfn, stdunsetfn };
static const struct gsu_array reswords_gsu =
{ reswordsgetfn, arrsetfn, stdunsetfn };
static const struct gsu_array disreswords_gsu =
{ disreswordsgetfn, arrsetfn, stdunsetfn };
static const struct gsu_array dirs_gsu =
{ dirsgetfn, dirssetfn, stdunsetfn };
static const struct gsu_array historywords_gsu =
{ histwgetfn, arrsetfn, stdunsetfn };

static struct pardef partab[] = {
    { "parameters", PM_READONLY,
      getpmparameter, scanpmparameters, &pmnullsethash_gsu,
      NULL, NULL },
    { "commands", 0,
      getpmcommand, scanpmcommands, &pmcommands_gsu,
      NULL, NULL },
    { "functions", 0,
      getpmfunction, scanpmfunctions, &pmfunctions_gsu,
      NULL, NULL },
    { "dis_functions", 0,
      getpmdisfunction, scanpmdisfunctions, &pmdisfunctions_gsu,
      NULL, NULL },
    { "funcstack", PM_ARRAY|PM_SPECIAL|PM_READONLY,
      NULL, NULL, NULL,
      &funcstack_gsu, NULL },
    { "functrace", PM_ARRAY|PM_SPECIAL|PM_READONLY,
      NULL, NULL, NULL,
      &functrace_gsu, NULL },
    { "builtins", PM_READONLY,
      getpmbuiltin, scanpmbuiltins, NULL,
      NULL, NULL },
    { "dis_builtins", PM_READONLY,
      getpmdisbuiltin, scanpmdisbuiltins, NULL,
      NULL, NULL },
    { "reswords", PM_ARRAY|PM_SPECIAL|PM_READONLY,
      NULL, NULL, NULL,
      &reswords_gsu, NULL },
    { "dis_reswords", PM_ARRAY|PM_SPECIAL|PM_READONLY,
      NULL, NULL, NULL,
      &disreswords_gsu, NULL },
    { "options", 0,
      getpmoption, scanpmoptions, &pmoptions_gsu,
      NULL, NULL },
    { "modules", PM_READONLY,
      getpmmodule, scanpmmodules, NULL,
      NULL, NULL },
    { "dirstack", PM_ARRAY|PM_SPECIAL|PM_REMOVABLE,
      NULL, NULL, NULL,
      &dirs_gsu, NULL },
    { "history", PM_READONLY,
      getpmhistory, scanpmhistory, NULL,
      NULL, NULL,  },
    { "historywords", PM_ARRAY|PM_SPECIAL|PM_READONLY,
      NULL, NULL, NULL,
      &historywords_gsu, NULL },
    { "jobtexts", PM_READONLY,
      getpmjobtext, scanpmjobtexts, NULL,
      NULL, NULL },
    { "jobstates", PM_READONLY,
      getpmjobstate, scanpmjobstates, NULL,
      NULL, NULL },
    { "jobdirs", PM_READONLY,
      getpmjobdir, scanpmjobdirs, NULL,
      NULL, NULL },
    { "nameddirs", 0,
      getpmnameddir, scanpmnameddirs, &pmnameddirs_gsu,
      NULL, NULL },
    { "userdirs", PM_READONLY,
      getpmuserdir, scanpmuserdirs, NULL,
      NULL, NULL },
    { "aliases", 0,
      getpmralias, scanpmraliases, &pmraliases_gsu,
      NULL, NULL },
    { "galiases", 0,
      getpmgalias, scanpmgaliases, &pmgaliases_gsu,
      NULL, NULL },
    { "saliases", 0,
      getpmsalias, scanpmsaliases, &pmsaliases_gsu,
      NULL, NULL },
    { "dis_aliases", 0,
      getpmdisralias, scanpmdisraliases, &pmdisraliases_gsu,
      NULL, NULL },
    { "dis_galiases", 0,
      getpmdisgalias, scanpmdisgaliases, &pmdisgaliases_gsu,
      NULL, NULL },
    { "dis_saliases", 0,
      getpmdissalias, scanpmdissaliases, &pmdissaliases_gsu,
      NULL, NULL },
    { NULL, 0, NULL, NULL, NULL, NULL, NULL }
};

/**/
int
setup_(UNUSED(Module m))
{
    incleanup = 0;

    return 0;
}

/**/
int
boot_(UNUSED(Module m))
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
	    def->pm->node.flags |= def->flags;
	    if (def->hash_gsu)
		def->pm->gsu.h = def->hash_gsu;
	} else {
	    if (!(def->pm = createparam(def->name, def->flags | PM_HIDE|
					PM_HIDEVAL | PM_REMOVABLE)))
		return 1;
	    def->pm->gsu.a = def->array_gsu;
	}
    }
    return 0;
}

/**/
int
cleanup_(UNUSED(Module m))
{
    Param pm;
    struct pardef *def;

    incleanup = 1;

    for (def = partab; def->name; def++) {
	if ((pm = (Param) paramtab->getnode(paramtab, def->name)) &&
	    pm == def->pm) {
	    pm->node.flags &= ~PM_READONLY;
	    unsetparam_pm(pm, 0, 1);
	}
    }
    return 0;
}

/**/
int
finish_(UNUSED(Module m))
{
    return 0;
}
