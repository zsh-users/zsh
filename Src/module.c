/*
 * module.c - deal with dynamic modules
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 1996-1997 Zoltán Hidvégi
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Zoltán Hidvégi or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Zoltán Hidvégi and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Zoltán Hidvégi and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Zoltán Hidvégi and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "zsh.mdh"
#include "module.pro"

/* The `zsh' module contains all the base code that can't actually be built *
 * as a separate module.  It is initialised by main(), so there's nothing   *
 * for the boot function to do.                                             */

/**/
int
boot_zsh(Module m)
{
    return 0;
}

/* addbuiltin() can be used to add a new builtin.  It returns zero on *
 * success, 1 on failure.  The only possible type of failure is that  *
 * a builtin with the specified name already exists.  An autoloaded   *
 * builtin can be replaced using this function.                       */

/**/
int
addbuiltin(Builtin b)
{
    Builtin bn = (Builtin) builtintab->getnode2(builtintab, b->nam);
    if (bn && (bn->flags & BINF_ADDED))
	return 1;
    if (bn)
	builtintab->freenode(builtintab->removenode(builtintab, b->nam));
    PERMALLOC {
	builtintab->addnode(builtintab, b->nam, b);
    } LASTALLOC;
    return 0;
}

/* Add multiple builtins.  binl points to a table of `size' builtin      *
 * structures.  Those for which (.flags & BINF_ADDED) is false are to be *
 * added; that flag is set if they succeed.  If any fail, an error       *
 * message is printed, using nam as the leading name.  Returns 1 if all  *
 * additions succeed, 2 if some succeed and some fail, and 0 if all (and *
 * at least 1) fail.  The usual usage in a boot_*() function would be    *
 *  return !addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab)); */

/**/
int
addbuiltins(char const *nam, Builtin binl, int size)
{
    int hads = 0, hadf = 0, n;

    for(n = 0; n < size; n++) {
	Builtin b = &binl[n];
	if(b->flags & BINF_ADDED)
	    continue;
	if(addbuiltin(b)) {
	    zwarnnam(nam, "name clash when adding builtin `%s'", b->nam, 0);
	    hadf = 1;
	} else {
	    b->flags |= BINF_ADDED;
	    hads = 2;
	}
    }
    return hadf ? hads : 1;
}

#ifdef DYNAMIC

/* $module_path ($MODULE_PATH) */

/**/
char **module_path;

/* List of modules */

/**/
LinkList modules;

/* Define an autoloadable builtin.  It returns 0 on success, or 1 on *
 * failure.  The only possible cause of failure is that a builtin    *
 * with the specified name already exists.                           */

/**/
int
add_autobin(char *nam, char *module)
{
    Builtin bn = zcalloc(sizeof(*bn));
    bn->nam = ztrdup(nam);
    bn->optstr = ztrdup(module);
    return addbuiltin(bn);
}

/* Remove the builtin added previously by addbuiltin().  Returns *
 * zero on succes and -1 if there is no builtin with that name.  */

/**/
int
deletebuiltin(char *nam)
{
    Builtin bn;

    bn = (Builtin) builtintab->removenode(builtintab, nam);
    if (!bn)
	return -1;
    builtintab->freenode((HashNode)bn);
    return 0;
}

/* Delete multiple builtins.  binl points to a table of `size' builtin  *
 * structures.  Those for which (.flags & BINF_ADDED) is true are to be *
 * deleted; that flag is cleared.  If any fail, an error message is     *
 * printed, using nam as the leading name.  Returns 1 if all deletions  *
 * succeed, 2 if some succeed and some fail, and 0 if all (and at least *
 * 1) fail.  In normal use, from a cleanup_*() function, this return    *
 * value would be ignored -- the only cause of failure would be that a  *
 * wayward module had deleted our builtin without telling us.           */

/**/
int
deletebuiltins(char const *nam, Builtin binl, int size)
{
    int hads = 0, hadf = 0, n;

    for(n = 0; n < size; n++) {
	Builtin b = &binl[n];
	if(!(b->flags & BINF_ADDED))
	    continue;
	if(deletebuiltin(b->nam)) {
	    zwarnnam(nam, "builtin `%s' already deleted", b->nam, 0);
	    hadf = 1;
	} else
	    hads = 2;
	b->flags &= ~BINF_ADDED;
    }
    return hadf ? hads : 1;
}

#ifdef HAVE_DLFCN_H
# include <dlfcn.h>
#else
# include <sys/types.h>
# include <nlist.h>
# include <link.h>
#endif
#ifndef RTLD_LAZY
# define RTLD_LAZY 1
#endif
#ifndef RTLD_GLOBAL
# define RTLD_GLOBAL 0
#endif
#ifndef HAVE_DLCLOSE
# define dlclose(X) ((X), 0)
#endif

#ifdef DLSYM_NEEDS_UNDERSCORE
# define STR_BOOT      "_boot_"
# define STR_BOOT_S    "_boot_%s"
# define STR_CLEANUP   "_cleanup_"
# define STR_CLEANUP_S "_cleanup_%s"
#else /* !DLSYM_NEEDS_UNDERSCORE */
# define STR_BOOT      "boot_"
# define STR_BOOT_S    "boot_%s"
# define STR_CLEANUP   "cleanup_"
# define STR_CLEANUP_S "cleanup_%s"
#endif /* !DLSYM_NEEDS_UNDERSCORE */
typedef int (*Module_func) _((Module));

/**/
static void *
try_load_module(char const *name)
{
    char buf[PATH_MAX + 1];
    char **pp;
    void *ret = NULL;
    int l;

    if (strchr(name, '/')) {
	ret = dlopen(unmeta(name), RTLD_LAZY | RTLD_GLOBAL);
	if (ret || 
	    unset(PATHDIRS) ||
	    (*name == '/') ||
	    (*name == '.' && name[1] == '/') ||
	    (*name == '.' && name[1] == '.' && name[2] == '/'))
	    return ret;
    }

    l = strlen(name) + 1;
    for (pp = module_path; !ret && *pp; pp++) {
	if (l + (**pp ? strlen(*pp) : 1) > PATH_MAX)
	    continue;
	sprintf(buf, "%s/%s", **pp ? *pp : ".", name);
	ret = dlopen(unmeta(buf), RTLD_LAZY | RTLD_GLOBAL);
    }

    return ret;
}

/**/
static void *
do_load_module(char const *name)
{
    void *ret = NULL;
    char buf[PATH_MAX + 1];

    if (strlen(name) + strlen(DL_EXT) < PATH_MAX) {
	sprintf(buf, "%s.%s", name, DL_EXT);
	ret = try_load_module(buf);
    }
    if (!ret)
	ret = try_load_module(name);
    if (!ret) {
	int waserr = errflag;
	zerr("failed to load module: %s", name, 0);
	errflag = waserr;
    }
    return ret;
}

/**/
static LinkNode
find_module(const char *name)
{
    Module m;
    LinkNode node;

    for (node = firstnode(modules); node; incnode(node)) {
	m = (Module) getdata(node);
	if (!strcmp(m->nam, name))
	    return node;
    }
    return NULL;
}

/**/
static int
init_module(Module m)
{
    char *s, *t;
#ifndef DYNAMIC_NAME_CLASH_OK
    char buf[PATH_MAX + 1];
#endif
    Module_func fn;

    s = strrchr(m->nam, '/');
    if (s)
	s = dupstring(++s);
    else
	s = m->nam;
    if ((t = strrchr(s, '.')))
	*t = '\0';
#ifdef DYNAMIC_NAME_CLASH_OK
    fn = (Module_func) dlsym(m->handle, STR_BOOT);
#else /* !DYNAMIC_NAME_CLASH_OK */
    if (strlen(s) + 6 > PATH_MAX)
	return 1;
    sprintf(buf, STR_BOOT_S, s);
    fn = (Module_func) dlsym(m->handle, buf);
#endif /* !DYNAMIC_NAME_CLASH_OK */
    if(fn)
	return fn(m);
    zwarnnam(m->nam, "no boot function", NULL, 0);
    return 1;
}

/**/
Module
load_module(char const *name)
{
    Module m;
    void *handle;
    LinkNode node, n;

    if (!(node = find_module(name))) {
	if (!(handle = do_load_module(name)))
	    return NULL;
	m = zcalloc(sizeof(*m));
	m->nam = ztrdup(name);
	m->handle = handle;
	if (init_module(m)) {
	    dlclose(handle);
	    zsfree(m->nam);
	    zfree(m, sizeof(*m));
	    return NULL;
	}
	PERMALLOC {
	    addlinknode(modules, m);
	} LASTALLOC;
	return m;
    }
    m = (Module) getdata(node);
    if (m->handle)
	return m;
    if (m->flags & MOD_BUSY) {
	zerr("circular dependencies for module %s", name, 0);
	return NULL;
    }
    m->flags |= MOD_BUSY;
    for (n = firstnode(m->deps); n; incnode(n))
	if (!load_module((char *) getdata(n))) {
	    m->flags &= ~MOD_BUSY;
	    return NULL;
	}
    m->flags &= ~MOD_BUSY;
    if (!(m->handle = do_load_module(name)))
	return NULL;
    if (init_module(m)) {
	dlclose(m->handle);
	m->handle = NULL;
	return NULL;
    }
    return m;
}

/**/
static int
cleanup_module(Module m)
{
    char *s, *t;
#ifndef DYNAMIC_NAME_CLASH_OK
    char buf[PATH_MAX + 1];
#endif
    Module_func fn;

    s = strrchr(m->nam, '/');
    if (s)
	s = dupstring(++s);
    else
	s = m->nam;
    if ((t = strrchr(s, '.')))
	*t = '\0';
#ifdef DYNAMIC_NAME_CLASH_OK
    fn = (Module_func) dlsym(m->handle, STR_CLEANUP);
#else /* !DYNAMIC_NAME_CLASH_OK */
    if (strlen(s) + 9 > PATH_MAX)
	return 1;
    sprintf(buf, STR_CLEANUP_S, s);
    fn = (Module_func) dlsym(m->handle, buf);
#endif /* !DYNAMIC_NAME_CLASH_OK */
    if(fn)
	return fn(m);
    zwarnnam(m->nam, "no cleanup function", NULL, 0);
    return 1;
}

/**/
void
add_dep(char *name, char *from)
{
    LinkNode node;
    Module m;

    PERMALLOC {
	if (!(node = find_module(name))) {
	    m = zcalloc(sizeof(*m));
	    m->nam = ztrdup(name);
	    addlinknode(modules, m);
	} else
	    m = (Module) getdata(node);
	if (!m->deps)
	    m->deps = newlinklist();
	for (node = firstnode(m->deps);
	     node && strcmp((char *) getdata(node), from);
	     incnode(node));
	if (!node)
	    addlinknode(m->deps, ztrdup(from));
    } LASTALLOC;
}

/**/
static void
autoloadscan(HashNode hn, int printflags)
{
    Builtin bn = (Builtin) hn;

    if(bn->flags & BINF_ADDED)
	return;
    if(printflags & PRINT_LIST) {
	fputs("zmodload -a ", stdout);
	if(bn->optstr[0] == '-')
	    fputs("-- ", stdout);
	quotedzputs(bn->optstr, stdout);
	if(strcmp(bn->nam, bn->optstr)) {
	    putchar(' ');
	    quotedzputs(bn->nam, stdout);
	}
    } else {
	nicezputs(bn->nam, stdout);
	if(strcmp(bn->nam, bn->optstr)) {
	    fputs(" (", stdout);
	    nicezputs(bn->optstr, stdout);
	    putchar(')');
	}
    }
    putchar('\n');
}

/**/
int
bin_zmodload(char *nam, char **args, char *ops, int func)
{
    if(ops['d'] && ops['a']) {
	zwarnnam(nam, "-d cannot be combined with -a", NULL, 0);
	return 1;
    }
    if (ops['u'] && !*args) {
	zwarnnam(nam, "what do you want to unload?", NULL, 0);
	return 1;
    }
    if(ops['d'])
	return bin_zmodload_dep(nam, args, ops);
    else if(ops['a'])
	return bin_zmodload_auto(nam, args, ops);
    else
	return bin_zmodload_load(nam, args, ops);
}

/**/
static int
bin_zmodload_dep(char *nam, char **args, char *ops)
{
    LinkNode node;
    Module m;
    if(ops['u']) {
	/* remove dependencies */
	char *tnam = *args++;
	node = find_module(tnam);
	if (!node)
	    return 0;
	m = (Module) getdata(node);
	if(*args && m->deps) {
	    do {
		for(node = firstnode(m->deps); node; incnode(node))
		    if(!strcmp(*args, getdata(node))) {
			zsfree(getdata(node));
			remnode(m->deps, node);
			break;
		    }
	    } while(*++args);
	    if(empty(m->deps)) {
		freelinklist(m->deps, freestr);
		m->deps = NULL;
	    }
	} else {
	    if (m->deps) {
		freelinklist(m->deps, freestr);
		m->deps = NULL;
	    }
	}
	if (!m->deps && !m->handle) {
	    remnode(modules, node);
	    zsfree(m->nam);
	    zfree(m, sizeof(*m));
	}
	return 0;
    } else if(!args[0] || !args[1]) {
	/* list dependencies */
	for (node = firstnode(modules); node; incnode(node)) {
	    m = (Module) getdata(node);
	    if (m->deps && (!args[0] || !strcmp(args[0], m->nam))) {
		LinkNode n;
		if(ops['L']) {
		    printf("zmodload -d ");
		    if(m->nam[0] == '-')
			fputs("-- ", stdout);
		    quotedzputs(m->nam, stdout);
		} else {
		    nicezputs(m->nam, stdout);
		    putchar(':');
		}
		for (n = firstnode(m->deps); n; incnode(n)) {
		    putchar(' ');
		    if(ops['L'])
			quotedzputs((char *) getdata(n), stdout);
		    else
			nicezputs((char *) getdata(n), stdout);
		}
		putchar('\n');
	    }
	}
	return 0;
    } else {
	/* add dependencies */
	int ret = 0;
	char *tnam = *args++;

	for(; *args; args++) {
	    if(isset(RESTRICTED) && strchr(*args, '/')) {
		zwarnnam(nam, "%s: restricted", *args, 0);
		ret = 1;
	    } else
		add_dep(tnam, *args);
	}
	return ret;
    }
}

/**/
static int
bin_zmodload_auto(char *nam, char **args, char *ops)
{
    int ret = 0;
    if(ops['u']) {
	/* remove autoloaded builtins */
	for (; *args; args++) {
	    Builtin bn = (Builtin) builtintab->getnode2(builtintab, *args);
	    if (!bn) {
		if(!ops['i']) {
		    zwarnnam(nam, "%s: no such builtin", *args, 0);
		    ret = 1;
		}
	    } else if (bn->flags & BINF_ADDED) {
		zwarnnam(nam, "%s: builtin is already defined", *args, 0);
		ret = 1;
	    } else
		deletebuiltin(*args);
	}
	return ret;
    } else if(!*args) {
	/* list autoloaded builtins */
	scanhashtable(builtintab, 0, 0, 0,
	    autoloadscan, ops['L'] ? PRINT_LIST : 0);
	return 0;
    } else {
	/* add autoloaded builtins */
	char *modnam;
	modnam = *args++;
	if(isset(RESTRICTED) && strchr(modnam, '/')) {
	    zwarnnam(nam, "%s: restricted", modnam, 0);
	    return 1;
	}
	do {
	    char *bnam = *args ? *args++ : modnam;
	    if (strchr(bnam, '/')) {
		zwarnnam(nam, "%s: `/' is illegal in a builtin", bnam, 0);
		ret = 1;
	    } else if (add_autobin(bnam, modnam) && !ops['i']) {
		zwarnnam(nam, "failed to add builtin %s", bnam, 0);
		ret = 1;
	    }
	} while(*args);
	return ret;
    }
}

/**/
static int
bin_zmodload_load(char *nam, char **args, char *ops)
{
    LinkNode node;
    Module m;
    int ret = 0;
    if(ops['u']) {
	/* unload modules */
	for(; *args; args++) {
	    node = find_module(*args);
	    if (node) {
		LinkNode mn, dn;

		for (mn = firstnode(modules); mn; incnode(mn)) {
		    m = (Module) getdata(mn);
		    if (m->deps && m->handle)
			for (dn = firstnode(m->deps); dn; incnode(dn))
			    if (!strcmp((char *) getdata(dn), *args)) {
				zwarnnam(nam, "module %s is in use by another module and cannot be unloaded", *args, 0);
				ret = 1;
				goto cont;
			    }
		}

		m = (Module) getdata(node);
		if (m->handle && cleanup_module(m))
		    ret = 1;
		else {
		    if (m->handle)
			dlclose(m->handle);
		    m->handle = NULL;
		    if(!m->deps) {
			remnode(modules, node);
			zsfree(m->nam);
			zfree(m, sizeof(*m));
		    }
		}
	    } else if (!ops['i']) {
		zwarnnam(nam, "no such module %s", *args, 0);
		ret = 1;
	    }
	    cont: ;
	}
	return ret;
    } else if(!*args) {
	/* list modules */
	for (node = firstnode(modules); node; incnode(node)) {
	    m = (Module) getdata(node);
	    if (m->handle) {
		if(ops['L']) {
		    printf("zmodload ");
		    if(m->nam[0] == '-')
			fputs("-- ", stdout);
		    quotedzputs(m->nam, stdout);
		} else
		    nicezputs(m->nam, stdout);
		putchar('\n');
	    }
	}
	return 0;
    } else {
	/* load modules */
	for (; *args; args++) {
	    node = find_module(*args);
	    if (node && ((Module) getdata(node))->handle) {
		if (!ops['i']) {
		    zwarnnam(nam, "module %s already loaded.", *args, 0);
		    ret = 1;
		}
	    } else if (isset(RESTRICTED) && strchr(*args, '/')) {
		zwarnnam(nam, "%s: restricted", *args, 0);
		ret = 1;
	    } else if (!load_module(*args))
		ret = 1;
	}
	return ret;
    }
}

#endif /* DYNAMIC */
