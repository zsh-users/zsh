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

/* List of builtin modules. */

/**/
LinkList bltinmodules;


/* The `zsh' module contains all the base code that can't actually be built *
 * as a separate module.  It is initialised by main(), so there's nothing   *
 * for the boot function to do.                                             */

/**/
int
setup_zsh(Module m)
{
    return 0;
}

/**/
int
boot_zsh(Module m)
{
    return 0;
}

/* This registers a builtin module.                                   */

/**/
void
register_module(char *n)
{
    PERMALLOC {
	addlinknode(bltinmodules, n);
    } LASTALLOC;
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

/* The list of function wrappers defined. */

/**/
FuncWrap wrappers;

/* This adds a definition for a wrapper. Return value is one in case of *
 * error and zero if all went fine. */

/**/
int
addwrapper(Module m, FuncWrap w)
{
    FuncWrap p, q;

    if (w->flags & WRAPF_ADDED)
	return 1;
    for (p = wrappers, q = NULL; p; q = p, p = p->next);
    if (q)
	q->next = w;
    else
	wrappers = w;
    w->next = NULL;
    w->flags |= WRAPF_ADDED;
    w->module = m;

    return 0;
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

/* This removes the given wrapper definition from the list. Returned is *
 * one in case of error and zero otherwise. */

/**/
int
deletewrapper(Module m, FuncWrap w)
{
    FuncWrap p, q;

    if (w->flags & WRAPF_ADDED) {
	for (p = wrappers, q = NULL; p && p != w; q = p, p = p->next);

	if (p) {
	    if (q)
		q->next = p->next;
	    else
		wrappers = p->next;
	    p->flags &= ~WRAPF_ADDED;

	    return 0;
	}
    }
    return 1;
}

#ifdef AIXDYNAMIC

#include <sys/ldr.h>

static char *dlerrstr[256];

static void *
load_and_bind(const char *fn)
{
    void *ret = (void *) load((char *) fn, L_NOAUTODEFER, NULL);

    if (ret) {
	LinkNode node;
	int err = loadbind(0, (void *) addbuiltin, ret);
	for (node = firstnode(modules); !err && node; incnode(node)) {
	    Module m = (Module) getdata(node);
	    if (m->handle)
		err |= loadbind(0, m->handle, ret);
	}

	if (err) {
	    loadquery(L_GETMESSAGES, dlerrstr, sizeof(dlerrstr));
	    unload(ret);
	    ret = NULL;
	}
    } else
	loadquery(L_GETMESSAGES, dlerrstr, sizeof(dlerrstr));

    return ret;
}

#define dlopen(X,Y) load_and_bind(X)
#define dlclose(X)  unload(X)
#define dlerror()   (dlerrstr[0])

#else

#ifdef HAVE_DLFCN_H
# include <dlfcn.h>
#else
# ifdef HAVE_DL_H
#  include <dl.h>
#  define RTLD_LAZY BIND_DEFERRED
#  define RTLD_GLOBAL DYNAMIC_PATH
# else
#  include <sys/types.h>
#  include <nlist.h>
#  include <link.h>
# endif
#endif

#ifdef HPUXDYNAMIC
# define dlopen(file,mode) (void *)shl_load((file), (mode), (long) 0)
# define dlclose(handle) shl_unload((shl_t)(handle))

static
void *
hpux_dlsym(void *handle, char *name)
{
    void *sym_addr;
    if (!shl_findsym((shl_t *)&handle, name, TYPE_UNDEFINED, &sym_addr))
	return sym_addr;
    return NULL;
}

# define dlsym(handle,name) hpux_dlsym(handle,name)
# define dlerror() 0
#else
# ifndef HAVE_DLCLOSE
#  define dlclose(X) ((X), 0)
# endif
#endif

#ifdef DLSYM_NEEDS_UNDERSCORE
# define STR_SETUP     "_setup_"
# define STR_SETUP_S   "_setup_%s"
# define STR_BOOT      "_boot_"
# define STR_BOOT_S    "_boot_%s"
# define STR_CLEANUP   "_cleanup_"
# define STR_CLEANUP_S "_cleanup_%s"
# define STR_FINISH    "_finish_"
# define STR_FINISH_S  "_finish_%s"
#else /* !DLSYM_NEEDS_UNDERSCORE */
# define STR_SETUP     "setup_"
# define STR_SETUP_S   "setup_%s"
# define STR_BOOT      "boot_"
# define STR_BOOT_S    "boot_%s"
# define STR_CLEANUP   "cleanup_"
# define STR_CLEANUP_S "cleanup_%s"
# define STR_FINISH    "finish_"
# define STR_FINISH_S  "finish_%s"
#endif /* !DLSYM_NEEDS_UNDERSCORE */

#endif /* !AIXDYNAMIC */

#ifndef RTLD_LAZY
# define RTLD_LAZY 1
#endif
#ifndef RTLD_GLOBAL
# define RTLD_GLOBAL 0
#endif

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

#ifdef AIXDYNAMIC

/**/
static int
setup_module(Module m)
{
    return ((int (*)_((int,Module))) m->handle)(0, m);
}

/**/
static int
init_module(Module m)
{
    return ((int (*)_((int,Module))) m->handle)(1, m);
}

/**/
static int
cleanup_module(Module m)
{
    return ((int (*)_((int,Module))) m->handle)(2, m);
}

/**/
static int
finish_module(Module m)
{
    return ((int (*)_((int,Module))) m->handle)(3, m);
}

#else

static Module_func
module_func(Module m, char *name, char *name_s)
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
    fn = (Module_func) dlsym(m->handle, name);
#else /* !DYNAMIC_NAME_CLASH_OK */
    if (strlen(s) + 6 > PATH_MAX)
	return NULL;
    sprintf(buf, name_s, s);
    fn = (Module_func) dlsym(m->handle, buf);
#endif /* !DYNAMIC_NAME_CLASH_OK */
    return fn;
}

/**/
static int
setup_module(Module m)
{
    Module_func fn = module_func(m, STR_SETUP, STR_SETUP_S);

    if (fn)
	return fn(m);
    zwarnnam(m->nam, "no setup function", NULL, 0);
    return 1;
}

/**/
static int
init_module(Module m)
{
    Module_func fn = module_func(m, STR_BOOT, STR_BOOT_S);

    if(fn)
	return fn(m);
    zwarnnam(m->nam, "no boot function", NULL, 0);
    return 1;
}

/**/
static int
cleanup_module(Module m)
{
    Module_func fn = module_func(m, STR_CLEANUP, STR_CLEANUP_S);

    if(fn)
	return fn(m);
    zwarnnam(m->nam, "no cleanup function", NULL, 0);
    return 1;
}

/* Note that this function does more than just calling finish_foo(), *
 * it really unloads the module. */

/**/
static int
finish_module(Module m)
{
    Module_func fn = module_func(m, STR_FINISH, STR_FINISH_S);
    int r;

    if (fn)
	r = fn(m);
    else {
	zwarnnam(m->nam, "no finish function", NULL, 0);
	r = 1;
    }
    dlclose(m->handle);
    return r;
}

#endif /* !AIXDYNAMIC */

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
	m->flags |= MOD_SETUP;
	PERMALLOC {
	    node = addlinknode(modules, m);
	} LASTALLOC;
	if (setup_module(m) || init_module(m)) {
	    finish_module(m);
	    remnode(modules, node);
	    zsfree(m->nam);
	    zfree(m, sizeof(*m));
	    m->flags &= ~MOD_SETUP;
	    return NULL;
	}
	m->flags &= ~MOD_SETUP;
	return m;
    } 
    m = (Module) getdata(node);
    if (m->flags & MOD_SETUP)
	return m;
    if (m->flags & MOD_UNLOAD)
	m->flags &= ~MOD_UNLOAD;
    else if (m->handle)
	return m;
    if (m->flags & MOD_BUSY) {
	zerr("circular dependencies for module %s", name, 0);
	return NULL;
    }
    m->flags |= MOD_BUSY;
    if (m->deps)
	for (n = firstnode(m->deps); n; incnode(n))
	    if (!load_module((char *) getdata(n))) {
		m->flags &= ~MOD_BUSY;
		return NULL;
	    }
    m->flags &= ~MOD_BUSY;
    if (!m->handle) {
	if (!(m->handle = do_load_module(name)))
	    return NULL;
	m->flags |= MOD_SETUP;
	if (setup_module(m)) {
	    finish_module(m->handle);
	    m->handle = NULL;
	    m->flags &= ~MOD_SETUP;
	    return NULL;
	}
    }
    m->flags |= MOD_SETUP;
    if (init_module(m)) {
	finish_module(m->handle);
	m->handle = NULL;
	m->flags &= ~MOD_SETUP;
	return NULL;
    }
    m->flags &= ~MOD_SETUP;
    return m;
}

/* This ensures that the module with the name given as the second argument
 * is loaded.
 * The third argument should be non-zero if the function should complain
 * about trying to load a module with a full path name in restricted mode.
 * The last argument should be non-zero if this function should signal an
 * error if the module is already loaded.
 * The return value is non-zero if the module was found or loaded. */

/**/
int
require_module(char *nam, char *module, int res, int test)
{
    Module m = NULL;
    LinkNode node;

    /* First see if the module is linked in. */
    for (node = firstnode(bltinmodules); node; incnode(node)) {
	if (!strcmp((char *) getdata(node), nam))
	    return 1;
    }
    node = find_module(module);
    if (node && (m = ((Module) getdata(node)))->handle &&
	!(m->flags & MOD_UNLOAD)) {
	if (test) {
	    zwarnnam(nam, "module %s already loaded.", module, 0);
	    return 0;
	}
    } else if (res && isset(RESTRICTED) && strchr(module, '/')) {
	zwarnnam(nam, "%s: restricted", module, 0);
	return 0;
    } else
	return !!load_module(module);

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
    else if (ops['c'] || ops['C'])
	return bin_zmodload_cond(nam, args, ops);
    else if (ops['p'])
	return bin_zmodload_param(nam, args, ops);
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
bin_zmodload_cond(char *nam, char **args, char *ops)
{
    int ret = 0;

    if (ops['u']) {
	/* remove autoloaded conditions */
	for (; *args; args++) {
	    Conddef cd = getconddef(ops['I'], *args, 0);

	    if (!cd) {
		if (!ops['i']) {
		    zwarnnam(nam, "%s: no such condition", *args, 0);
		    ret = 1;
		}
	    } else if (cd->flags & CONDF_ADDED) {
		zwarnnam(nam, "%s: condition is already defined", *args, 0);
		ret = 1;
	    } else
		deleteconddef(cd);
	}
	return ret;
    } else if (!*args) {
	/* list autoloaded conditions */
	Conddef p;

	for (p = condtab; p; p = p->next) {
	    if (p->module) {
		if (ops['L']) {
		    fputs("zmodload -c", stdout);
		    if (p->flags & CONDF_INFIX)
			putchar('I');
		    printf(" %s %s\n", p->module, p->name);
		} else {
		    fputs("post ", stdout);
		    if (p->flags & CONDF_INFIX)
			fputs("infix ", stdout);
		    printf("%s (%s)\n",p->name, p->module);
		}
	    }
	}
	return 0;
    } else {
	/* add autoloaded conditions */
	char *modnam;

	modnam = *args++;
	if(isset(RESTRICTED) && strchr(modnam, '/')) {
	    zwarnnam(nam, "%s: restricted", modnam, 0);
	    return 1;
	}
	do {
	    char *cnam = *args ? *args++ : modnam;
	    if (strchr(cnam, '/')) {
		zwarnnam(nam, "%s: `/' is illegal in a condition", cnam, 0);
		ret = 1;
	    } else if (add_autocond(cnam, ops['I'], modnam) && !ops['i']) {
		zwarnnam(nam, "failed to add condition %s", cnam, 0);
		ret = 1;
	    }
	} while(*args);
	return ret;
    }
}

static void
printautoparams(HashNode hn, int lon)
{
    Param pm = (Param) hn;

    if (pm->flags & PM_AUTOLOAD) {
	if (lon)
	    printf("zmodload -p %s %s\n", pm->u.str, pm->nam);
	else
	    printf("%s (%s)\n", pm->nam, pm->u.str);
    }
}

/**/
static int
bin_zmodload_param(char *nam, char **args, char *ops)
{
    int ret = 0;

    if (ops['u']) {
	/* remove autoloaded parameters */
	for (; *args; args++) {
	    Param pm = (Param) gethashnode2(paramtab, *args);

	    if (!pm) {
		if (!ops['i']) {
		    zwarnnam(nam, "%s: no such parameter", *args, 0);
		    ret = 1;
		}
	    } else if (!(pm->flags & PM_AUTOLOAD)) {
		zwarnnam(nam, "%s: parameter is already defined", *args, 0);
		ret = 1;
	    } else
		unsetparam_pm(pm, 0, 1);
	}
	return ret;
    } else if (!*args) {
	scanhashtable(paramtab, 1, 0, 0, printautoparams, ops['L']);
	return 0;
    } else {
	/* add autoloaded parameters */
	char *modnam;

	modnam = *args++;
	if(isset(RESTRICTED) && strchr(modnam, '/')) {
	    zwarnnam(nam, "%s: restricted", modnam, 0);
	    return 1;
	}
	do {
	    char *pnam = *args ? *args++ : modnam;
	    if (strchr(pnam, '/')) {
		zwarnnam(nam, "%s: `/' is illegal in a parameter", pnam, 0);
		ret = 1;
	    } else
		add_autoparam(pnam, modnam);
	} while(*args);
	return ret;
    }
}

/**/
int
unload_module(Module m, LinkNode node)
{
    if (m->handle && !(m->flags & MOD_UNLOAD) && cleanup_module(m))
	return 1;
    else {
	int del = (m->flags & MOD_UNLOAD);

	if (m->wrapper) {
	    m->flags |= MOD_UNLOAD;
	    return 0;
	}
	m->flags &= ~MOD_UNLOAD;
	if (m->handle)
	    finish_module(m);
	m->handle = NULL;
	if (del && m->deps) {
	    /* The module was unloaded delayed, unload all modules *
	     * on which it depended. */
	    LinkNode n;

	    for (n = firstnode(m->deps); n; incnode(n)) {
		LinkNode dn = find_module((char *) getdata(n));
		Module dm;

		if (dn && (dm = (Module) getdata(dn)) &&
		    (dm->flags & MOD_UNLOAD)) {
		    /* See if this is the only module depending on it. */

		    LinkNode an;
		    Module am;
		    int du = 1;

		    for (an = firstnode(modules); du && an; incnode(an)) {
			am = (Module) getdata(an);
			if (am != m && am->handle && am->deps) {
			    LinkNode sn;

			    for (sn = firstnode(am->deps); du && sn;
				 incnode(sn)) {
				if (!strcmp((char *) getdata(sn), dm->nam))
				    du = 0;
			    }
			}
		    }
		    if (du)
			unload_module(dm, NULL);
		}
	    }
	}
	if(!m->deps) {
	    if (!node) {
		for (node = firstnode(modules); node; incnode(node))
		    if (m == (Module) getdata(node))
			break;
		if (!node)
		    return 1;
	    }
	    remnode(modules, node);
	    zsfree(m->nam);
	    zfree(m, sizeof(*m));
	}
    }
    return 0;
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
		int del = 0;

		for (mn = firstnode(modules); mn; incnode(mn)) {
		    m = (Module) getdata(mn);
		    if (m->deps && m->handle)
			for (dn = firstnode(m->deps); dn; incnode(dn))
			    if (!strcmp((char *) getdata(dn), *args)) {
				if (m->flags & MOD_UNLOAD)
				    del = 1;
				else {
				    zwarnnam(nam, "module %s is in use by another module and cannot be unloaded", *args, 0);
				    ret = 1;
				    goto cont;
				}
			    }
		}
		m = (Module) getdata(node);
		if (del)
		    m->wrapper++;
		if (unload_module(m, node))
		    ret = 1;
		if (del)
		    m->wrapper--;
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
	    if (m->handle && !(m->flags & MOD_UNLOAD)) {
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
	for (; *args; args++)
	    if (!require_module(nam, *args, 1, (!ops['i'])))
		ret = 1;

	return ret;
    }
}

#endif /* DYNAMIC */

/* The list of module-defined conditions. */

/**/
Conddef condtab;

/* This gets a condition definition with the given name. The first        *
 * argument says if we have to look for an infix condition. The last      *
 * argument is non-zero if we should autoload modules if needed. */

/**/
Conddef
getconddef(int inf, char *name, int autol)
{
    Conddef p;
#ifdef DYNAMIC
    int f = 1;
#endif

    do {
	for (p = condtab; p; p = p->next) {
	    if ((!!inf == !!(p->flags & CONDF_INFIX)) &&
		!strcmp(name, p->name))
		break;
	}
#ifdef DYNAMIC
	if (autol && p && p->module) {
	    /* This is a definition for an autoloaded condition, load the *
	     * module if we haven't tried that already. */
	    if (f) {
		load_module(p->module);
		f = 0;
		p = NULL;
	    } else
		break;
	} else
#endif
	    break;
    } while (!p);
    return p;
}

/* This adds the given condition definition. The return value is zero on *
 * success and 1 on failure. If there is a matching definition for an    *
 * autoloaded condition, it is removed. */

/**/
int
addconddef(Conddef c)
{
    Conddef p = getconddef((c->flags & CONDF_INFIX), c->name, 0);

    if (p) {
	if (!p->module || (p->flags & CONDF_ADDED))
	    return 1;
#ifdef DYNAMIC
	/* There is an autoload definition. */

	deleteconddef(p);
#endif
    }
    c->next = condtab;
    condtab = c;
    return 0;
}

/* This adds multiple condition definitions. This is like addbuiltins(). */

/**/
int
addconddefs(char const *nam, Conddef c, int size)
{
    int hads = 0, hadf = 0;

    while (size--) {
	if (c->flags & CONDF_ADDED)
	    continue;
	if (addconddef(c)) {
	    zwarnnam(nam, "name clash when adding condition `%s'", c->name, 0);
	    hadf = 1;
	} else {
	    c->flags |= CONDF_ADDED;
	    hads = 2;
	}
	c++;
    }
    return hadf ? hads : 1;
}

/* This adds the given parameter definition. The return value is zero on *
 * success and 1 on failure. */

/**/
int
addparamdef(Paramdef d)
{
    Param pm;

    if ((pm = (Param) gethashnode2(paramtab, d->name)))
	unsetparam_pm(pm, 0, 1);

    if (!(pm = createparam(d->name, d->flags)) &&
	!(pm = (Param) paramtab->getnode(paramtab, d->name)))
	return 1;

    pm->level = 0;
    pm->u.data = d->var;
    pm->sets.ifn = (void (*)(Param, long)) d->set;
    pm->gets.ifn = (long (*)(Param)) d->get;
    pm->unsetfn = (void (*)(Param, int)) d->unset;

    return 0;
}

/* This adds multiple parameter definitions. This is like addbuiltins(). */

/**/
int
addparamdefs(char const *nam, Paramdef d, int size)
{
    int hads = 0, hadf = 0;

    while (size--) {
	if (addparamdef(d)) {
	    zwarnnam(nam, "error when adding parameter `%s'", d->name, 0);
	    hadf = 1;
	} else
	    hads = 2;
	d++;
    }
    return hadf ? hads : 1;
}

/* Delete parameters defined. No error checking yet. */

/**/
int
deleteparamdef(Paramdef d)
{
    unsetparam(d->name);
    return 0;
}

/**/
int
deleteparamdefs(char const *nam, Paramdef d, int size)
{
    while (size--) {
	deleteparamdef(d);
	d++;
    }
    return 1;
}

#ifdef DYNAMIC

/* This adds a definition for autoloading a module for a condition. */

/**/
int
add_autocond(char *nam, int inf, char *module)
{
    Conddef c = zalloc(sizeof(*c));

    c->name = ztrdup(nam);
    c->flags = (inf  ? CONDF_INFIX : 0);
    c->module = ztrdup(module);

    if (addconddef(c)) {
	zsfree(c->name);
	zsfree(c->module);
	zfree(c, sizeof(*c));

	return 1;
    }
    return 0;
}

/* This removes the given condition definition from the list(s). If this *
 * is a definition for a autoloaded condition, the memory is freed. */

/**/
int
deleteconddef(Conddef c)
{
    Conddef p, q;

    for (p = condtab, q = NULL; p && p != c; q = p, p = p->next);

    if (p) {
	if (q)
	    q->next = p->next;
	else 
	    condtab = p->next;
		
	if (p->module) {
	    /* autoloaded, free it */
	    zsfree(p->name);
	    zsfree(p->module);
	    zfree(p, sizeof(*p));
	}
	return 0;
    }
    return -1;
}

/* This removes multiple condition definitions (like deletebuiltins()). */

/**/
int
deleteconddefs(char const *nam, Conddef c, int size)
{
    int hads = 0, hadf = 0;

    while (size--) {
	if (!(c->flags & CONDF_ADDED))
	    continue;
	if (deleteconddef(c)) {
	    zwarnnam(nam, "condition `%s' already deleted", c->name, 0);
	    hadf = 1;
	} else
	    hads = 2;
	c->flags &= ~CONDF_ADDED;
	c++;
    }
    return hadf ? hads : 1;
}

/* This adds a definition for autoloading a module for a parameter. */

/**/
void
add_autoparam(char *nam, char *module)
{
    Param pm;

    if ((pm = (Param) gethashnode2(paramtab, nam)))
	unsetparam_pm(pm, 0, 1);

    pm = setsparam(ztrdup(nam), ztrdup(module));

    pm->flags |= PM_AUTOLOAD;
}

#endif
