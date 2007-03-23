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

/* List of linked-in modules. */

/**/
LinkList linkedmodules;


/* The `zsh/main' module contains all the base code that can't actually be *
 * built as a separate module.  It is initialised by main(), so there's    *
 * nothing for the boot function to do.                                    */

/**/
int
setup_(UNUSED(Module m))
{
    return 0;
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
    return 0;
}

/**/
int
finish_(UNUSED(Module m))
{
    return 0;
}

/* This registers a builtin module.                                   */

/**/
void
register_module(char *n, Module_func setup, Module_func boot,
		Module_func cleanup, Module_func finish)
{
    Linkedmod m;

    m = (Linkedmod) zalloc(sizeof(*m));

    m->name = ztrdup(n);
    m->setup = setup;
    m->boot = boot;
    m->cleanup = cleanup;
    m->finish = finish;

    zaddlinknode(linkedmodules, m);
}

/* Print an alias. */

/**/
static void
printmodalias(Module m, Options ops)
{
    if (OPT_ISSET(ops,'L')) {
	printf("zmodload -A ");
	if (m->nam[0] == '-')
	    fputs("-- ", stdout);
	quotedzputs(m->nam, stdout);
	putchar('=');
	quotedzputs(m->u.alias, stdout);
    } else {
	nicezputs(m->nam, stdout);
	fputs(" -> ", stdout);
	nicezputs(m->u.alias, stdout);
    }
    putchar('\n');
}

/* Check if a module is linked in. */

/**/
Linkedmod
module_linked(char const *name)
{
    LinkNode node;

    for (node = firstnode(linkedmodules); node; incnode(node))
	if (!strcmp(((Linkedmod) getdata(node))->name, name))
	    return (Linkedmod) getdata(node);

    return NULL;
}

/* addbuiltin() can be used to add a new builtin.  It returns zero on *
 * success, 1 on failure.  The only possible type of failure is that  *
 * a builtin with the specified name already exists.  An autoloaded   *
 * builtin can be replaced using this function.                       */

/**/
int
addbuiltin(Builtin b)
{
    Builtin bn = (Builtin) builtintab->getnode2(builtintab, b->node.nam);
    if (bn && (bn->node.flags & BINF_ADDED))
	return 1;
    if (bn)
	builtintab->freenode(builtintab->removenode(builtintab, b->node.nam));
    builtintab->addnode(builtintab, b->node.nam, b);
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
mod_export int
addbuiltins(char const *nam, Builtin binl, int size)
{
    int hads = 0, hadf = 0, n;

    for(n = 0; n < size; n++) {
	Builtin b = &binl[n];
	if(b->node.flags & BINF_ADDED)
	    continue;
	if(addbuiltin(b)) {
	    zwarnnam(nam, "name clash when adding builtin `%s'", b->node.nam);
	    hadf = 1;
	} else {
	    b->node.flags |= BINF_ADDED;
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
mod_export int
addwrapper(Module m, FuncWrap w)
{
    FuncWrap p, q;

    /*
     * We can't add a wrapper to an alias, since it's supposed
     * to behave identically to the resolved module.  This shouldn't
     * happen since we usually add wrappers when a real module is
     * loaded.
     */
    if (m->flags & MOD_ALIAS)
	return 1;

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

/* $module_path ($MODULE_PATH) */

/**/
char **module_path;

/* List of modules */

/**/
mod_export LinkList modules;

/* Define an autoloadable builtin.  It returns 0 on success, or 1 on *
 * failure.  The only possible cause of failure is that a builtin    *
 * with the specified name already exists.                           */

/**/
int
add_autobin(char *nam, char *module)
{
    Builtin bn = zshcalloc(sizeof(*bn));
    bn->node.nam = ztrdup(nam);
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
    builtintab->freenode(&bn->node);
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
mod_export int
deletebuiltins(char const *nam, Builtin binl, int size)
{
    int hads = 0, hadf = 0, n;

    for(n = 0; n < size; n++) {
	Builtin b = &binl[n];
	if(!(b->node.flags & BINF_ADDED))
	    continue;
	if(deletebuiltin(b->node.nam)) {
	    zwarnnam(nam, "builtin `%s' already deleted", b->node.nam);
	    hadf = 1;
	} else
	    hads = 2;
	b->node.flags &= ~BINF_ADDED;
    }
    return hadf ? hads : 1;
}

/* This removes the given wrapper definition from the list. Returned is *
 * one in case of error and zero otherwise. */

/**/
mod_export int
deletewrapper(Module m, FuncWrap w)
{
    FuncWrap p, q;

    if (m->flags & MOD_ALIAS)
	return 1;

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

/**/
#ifdef DYNAMIC

/**/
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
	    if (!(m->flags & MOD_ALIAS) &&
		m->u.handle && !(m->flags & MOD_LINKED))
		err |= loadbind(0, m->u.handle, ret);
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

/**/
#else

#ifdef HAVE_DLFCN_H
# if defined(HAVE_DL_H) && defined(HPUXDYNAMIC)
#  include <dl.h>
# else
#  include <dlfcn.h>
# endif
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

/**/
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
/**/
#endif

#ifdef DLSYM_NEEDS_UNDERSCORE
# define STR_SETUP     "_setup_"
# define STR_BOOT      "_boot_"
# define STR_CLEANUP   "_cleanup_"
# define STR_FINISH    "_finish_"
#else /* !DLSYM_NEEDS_UNDERSCORE */
# define STR_SETUP     "setup_"
# define STR_BOOT      "boot_"
# define STR_CLEANUP   "cleanup_"
# define STR_FINISH    "finish_"
#endif /* !DLSYM_NEEDS_UNDERSCORE */

/**/
#endif /* !AIXDYNAMIC */

#ifndef RTLD_LAZY
# define RTLD_LAZY 1
#endif
#ifndef RTLD_GLOBAL
# define RTLD_GLOBAL 0
#endif

/**/
static void *
try_load_module(char const *name)
{
    char buf[PATH_MAX + 1];
    char **pp;
    void *ret = NULL;
    int l;

    l = 1 + strlen(name) + 1 + strlen(DL_EXT);
    for (pp = module_path; !ret && *pp; pp++) {
	if (l + (**pp ? strlen(*pp) : 1) > PATH_MAX)
	    continue;
	sprintf(buf, "%s/%s.%s", **pp ? *pp : ".", name, DL_EXT);
	ret = dlopen(unmeta(buf), RTLD_LAZY | RTLD_GLOBAL);
    }

    return ret;
}

/**/
static void *
do_load_module(char const *name, int silent)
{
    void *ret;

    ret = try_load_module(name);
    if (!ret && !silent)
	zwarn("failed to load module: %s", name);
    return ret;
}

/**/
#else /* !DYNAMIC */

/**/
static void *
do_load_module(char const *name, int silent)
{
    if (!silent)
	zwarn("failed to load module: %s", name);

    return NULL;
}

/**/
#endif /* !DYNAMIC */

/*
 * Find a module in the list.
 * If aliasp is non-zero, resolve any aliases to the underlying module.
 * If namep is set, this is set to point to the last alias value resolved,
 *   even if that module was not loaded. or the module name if no aliases.
 *   Hence this is always the physical module to load in a chain of aliases.
 * Return NULL if the module named is not stored as a structure, or if we were
 * resolving aliases and the final module named is not stored as a
 * structure.
 *
 * TODO: now we have aliases, there may be some merit in using a hash
 * table instead of a linked list.
 */
/**/
static LinkNode
find_module(const char *name, int aliasp, const char **namep)
{
    Module m;
    LinkNode node;

    for (node = firstnode(modules); node; incnode(node)) {
	m = (Module) getdata(node);
	if (!strcmp(m->nam, name)) {
	    if (aliasp && (m->flags & MOD_ALIAS)) {
		if (namep)
		    *namep = m->u.alias;
		return find_module(m->u.alias, 1, namep);
	    }
	    if (namep)
		*namep = m->nam;
	    return node;
	}
    }
    return NULL;
}

/*
 * Unlink and free a module node from the linked list.
 */

/**/
static void
delete_module(LinkNode node)
{
    Module m = (Module) remnode(modules, node);

    if (m->flags & MOD_ALIAS)
	zsfree(m->u.alias);
    zsfree(m->nam);
    if (m->deps)
	freelinklist(m->deps, freestr);
    zfree(m, sizeof(*m));
}

/**/
mod_export int
module_loaded(const char *name)
{
    LinkNode node;
    Module m;

    return ((node = find_module(name, 1, NULL)) &&
	    (m = ((Module) getdata(node)))->u.handle &&
	    !(m->flags & MOD_UNLOAD));
}

/*
 * Setup and cleanup functions:  we don't search for aliases here,
 * since they should have been resolved before we try to load or unload
 * the module.
 */

/**/
#ifdef DYNAMIC

/**/
#ifdef AIXDYNAMIC

/**/
static int
dyn_setup_module(Module m)
{
    return ((int (*)_((int,Module))) m->u.handle)(0, m);
}

/**/
static int
dyn_boot_module(Module m)
{
    return ((int (*)_((int,Module))) m->u.handle)(1, m);
}

/**/
static int
dyn_cleanup_module(Module m)
{
    return ((int (*)_((int,Module))) m->u.handle)(2, m);
}

/**/
static int
dyn_finish_module(Module m)
{
    return ((int (*)_((int,Module))) m->u.handle)(3, m);
}

/**/
#else

static Module_func
module_func(Module m, char *name)
{
#ifdef DYNAMIC_NAME_CLASH_OK
    return (Module_func) dlsym(m->u.handle, name);
#else /* !DYNAMIC_NAME_CLASH_OK */
    VARARR(char, buf, strlen(name) + strlen(m->nam)*2 + 1);
    char const *p;
    char *q;
    strcpy(buf, name);
    q = strchr(buf, 0);
    for(p = m->nam; *p; p++) {
	if(*p == '/') {
	    *q++ = 'Q';
	    *q++ = 's';
	} else if(*p == '_') {
	    *q++ = 'Q';
	    *q++ = 'u';
	} else if(*p == 'Q') {
	    *q++ = 'Q';
	    *q++ = 'q';
	} else
	    *q++ = *p;
    }
    *q = 0;
    return (Module_func) dlsym(m->u.handle, buf);
#endif /* !DYNAMIC_NAME_CLASH_OK */
}

/**/
static int
dyn_setup_module(Module m)
{
    Module_func fn = module_func(m, STR_SETUP);

    if (fn)
	return fn(m);
    zwarnnam(m->nam, "no setup function");
    return 1;
}

/**/
static int
dyn_boot_module(Module m)
{
    Module_func fn = module_func(m, STR_BOOT);

    if(fn)
	return fn(m);
    zwarnnam(m->nam, "no boot function");
    return 1;
}

/**/
static int
dyn_cleanup_module(Module m)
{
    Module_func fn = module_func(m, STR_CLEANUP);

    if(fn)
	return fn(m);
    zwarnnam(m->nam, "no cleanup function");
    return 1;
}

/* Note that this function does more than just calling finish_foo(), *
 * it really unloads the module. */

/**/
static int
dyn_finish_module(Module m)
{
    Module_func fn = module_func(m, STR_FINISH);
    int r;

    if (fn)
	r = fn(m);
    else {
	zwarnnam(m->nam, "no finish function");
	r = 1;
    }
    dlclose(m->u.handle);
    return r;
}

/**/
#endif /* !AIXDYNAMIC */

/**/
static int
setup_module(Module m)
{
    return ((m->flags & MOD_LINKED) ?
	    (m->u.linked->setup)(m) : dyn_setup_module(m));
}

/**/
static int
boot_module(Module m)
{
    return ((m->flags & MOD_LINKED) ?
	    (m->u.linked->boot)(m) : dyn_boot_module(m));
}

/**/
static int
cleanup_module(Module m)
{
    return ((m->flags & MOD_LINKED) ?
	    (m->u.linked->cleanup)(m) : dyn_cleanup_module(m));
}

/**/
static int
finish_module(Module m)
{
    return ((m->flags & MOD_LINKED) ?
	    (m->u.linked->finish)(m) : dyn_finish_module(m));
}

/**/
#else /* !DYNAMIC */

/**/
static int
setup_module(Module m)
{
    return ((m->flags & MOD_LINKED) ? (m->u.linked->setup)(m) : 1);
}

/**/
static int
boot_module(Module m)
{
    return ((m->flags & MOD_LINKED) ? (m->u.linked->boot)(m) : 1);
}

/**/
static int
cleanup_module(Module m)
{
    return ((m->flags & MOD_LINKED) ? (m->u.linked->cleanup)(m) : 1);
}

/**/
static int
finish_module(Module m)
{
    return ((m->flags & MOD_LINKED) ? (m->u.linked->finish)(m) : 1);
}

/**/
#endif /* !DYNAMIC */

/**/
static int
modname_ok(char const *p)
{
    do {
	p = itype_end(p, IIDENT, 0);
	if (!*p)
	    return 1;
    } while(*p++ == '/');
    return 0;
}

/**/
mod_export int
load_module(char const *name)
{
    return load_module_silence(name, 0);
}

/**/
mod_export int
load_module_silence(char const *name, int silent)
{
    Module m;
    void *handle = NULL;
    Linkedmod linked;
    LinkNode node, n;
    int set;

    if (!modname_ok(name)) {
	if (!silent)
	    zerr("invalid module name `%s'", name);
	return 0;
    }
    /*
     * The following function call may alter name to the final name in a
     * chain of aliases.  This makes sure the actual module loaded
     * is the right one.
     */
    queue_signals();
    if (!(node = find_module(name, 1, &name))) {
	if (!(linked = module_linked(name)) &&
	    !(handle = do_load_module(name, silent))) {
	    unqueue_signals();
	    return 0;
	}
	m = zshcalloc(sizeof(*m));
	m->nam = ztrdup(name);
	if (handle) {
	    m->u.handle = handle;
	    m->flags |= MOD_SETUP;
	} else {
	    m->u.linked = linked;
	    m->flags |= MOD_SETUP | MOD_LINKED;
	}
	node = zaddlinknode(modules, m);

	if ((set = setup_module(m)) || boot_module(m)) {
	    if (!set)
		finish_module(m);
	    delete_module(node);
	    unqueue_signals();
	    return 0;
	}
	m->flags |= MOD_INIT_S | MOD_INIT_B;
	m->flags &= ~MOD_SETUP;
	unqueue_signals();
	return 1;
    } 
    m = (Module) getdata(node);
    if (m->flags & MOD_SETUP) {
	unqueue_signals();
	return 1;
    }
    if (m->flags & MOD_UNLOAD)
	m->flags &= ~MOD_UNLOAD;
    else if ((m->flags & MOD_LINKED) ? m->u.linked : m->u.handle) {
	unqueue_signals();
	return 1;
    }
    if (m->flags & MOD_BUSY) {
	zerr("circular dependencies for module %s", name);
	return 0;
    }
    m->flags |= MOD_BUSY;
    if (m->deps)
	for (n = firstnode(m->deps); n; incnode(n))
	    if (!load_module_silence((char *) getdata(n), silent)) {
		m->flags &= ~MOD_BUSY;
		unqueue_signals();
		return 0;
	    }
    m->flags &= ~MOD_BUSY;
    if (!m->u.handle) {
	handle = NULL;
	if (!(linked = module_linked(name)) &&
	    !(handle = do_load_module(name, silent))) {
	    unqueue_signals();
	    return 0;
	}
	if (handle) {
	    m->u.handle = handle;
	    m->flags |= MOD_SETUP;
	} else {
	    m->u.linked = linked;
	    m->flags |= MOD_SETUP | MOD_LINKED;
	}
	if (setup_module(m)) {
	    if (handle)
		m->u.handle = NULL;
	    else
		m->u.linked = NULL;
	    m->flags &= ~MOD_SETUP;
	    unqueue_signals();
	    return 0;
	}
	m->flags |= MOD_INIT_S;
    }
    m->flags |= MOD_SETUP;
    if (boot_module(m)) {
	finish_module(m);
	if (m->flags & MOD_LINKED)
	    m->u.linked = NULL;
	else
	    m->u.handle = NULL;
	m->flags &= ~MOD_SETUP;
	unqueue_signals();
	return 0;
    }
    m->flags |= MOD_INIT_B;
    m->flags &= ~MOD_SETUP;
    unqueue_signals();
    return 1;
}

/* This ensures that the module with the name given as the second argument
 * is loaded.
 * The third argument should be non-zero if the function should complain
 * about trying to load a module with a full path name in restricted mode.
 * The last argument should be non-zero if this function should signal an
 * error if the module is already loaded.
 * The return value is non-zero if the module was found or loaded. */

/**/
mod_export int
require_module(char *nam, const char *module, UNUSED(int res), int test)
{
    Module m = NULL;
    LinkNode node;
    int ret = 1;

    /* Resolve aliases and actual loadable module as for load_module */
    queue_signals();
    node = find_module(module, 1, &module);
    if (node && (m = ((Module) getdata(node)))->u.handle &&
	!(m->flags & MOD_UNLOAD)) {
	if (test) {
	    unqueue_signals();
	    zwarnnam(nam, "module %s already loaded.", module);
	    return 0;
	}
    } else
	ret = load_module_silence(module, 0);
    unqueue_signals();

    return ret;
}

/**/
void
add_dep(const char *name, char *from)
{
    LinkNode node;
    Module m;

    /*
     * If we were passed an alias, we must resolve it to a final
     * module name (and maybe add the corresponding struct), since otherwise
     * we would need to check all modules to see if they happen
     * to be aliased to the same thing to implement dependencies properly.
     *
     * This should mean that an attempt to add an alias which would
     * have the same name as a module which has dependencies is correctly
     * rejected, because then the module named already exists as a non-alias.
     * Better make sure.  (There's no problem making a an alias which
     * *points* to a module with dependencies, of course.)
     */
    if (!(node = find_module(name, 1, &name))) {
	m = zshcalloc(sizeof(*m));
	m->nam = ztrdup(name);
	zaddlinknode(modules, m);
    } else
	m = (Module) getdata(node);
    if (!m->deps)
	m->deps = znewlinklist();
    for (node = firstnode(m->deps);
	 node && strcmp((char *) getdata(node), from);
	 incnode(node));
    if (!node)
	zaddlinknode(m->deps, ztrdup(from));
}

/**/
static void
autoloadscan(HashNode hn, int printflags)
{
    Builtin bn = (Builtin) hn;

    if(bn->node.flags & BINF_ADDED)
	return;
    if(printflags & PRINT_LIST) {
	fputs("zmodload -ab ", stdout);
	if(bn->optstr[0] == '-')
	    fputs("-- ", stdout);
	quotedzputs(bn->optstr, stdout);
	if(strcmp(bn->node.nam, bn->optstr)) {
	    putchar(' ');
	    quotedzputs(bn->node.nam, stdout);
	}
    } else {
	nicezputs(bn->node.nam, stdout);
	if(strcmp(bn->node.nam, bn->optstr)) {
	    fputs(" (", stdout);
	    nicezputs(bn->optstr, stdout);
	    putchar(')');
	}
    }
    putchar('\n');
}

/**/
int
bin_zmodload(char *nam, char **args, Options ops, UNUSED(int func))
{
    int ops_bcpf = OPT_ISSET(ops,'b') || OPT_ISSET(ops,'c') || 
	OPT_ISSET(ops,'p') || OPT_ISSET(ops,'f');
    int ops_au = OPT_ISSET(ops,'a') || OPT_ISSET(ops,'u');
    int ret = 1;

    if (ops_bcpf && !ops_au) {
	zwarnnam(nam, "-b, -c, -f, and -p must be combined with -a or -u");
	return 1;
    }
    if (OPT_ISSET(ops,'A') || OPT_ISSET(ops,'R')) {
	if (ops_bcpf || ops_au || OPT_ISSET(ops,'d') || 
	    (OPT_ISSET(ops,'R') && OPT_ISSET(ops,'e'))) {
	    zwarnnam(nam, "illegal flags combined with -A or -R");
	    return 1;
	}
	if (!OPT_ISSET(ops,'e'))
	    return bin_zmodload_alias(nam, args, ops);
    }
    if (OPT_ISSET(ops,'d') && OPT_ISSET(ops,'a')) {
	zwarnnam(nam, "-d cannot be combined with -a");
	return 1;
    }
    if (OPT_ISSET(ops,'u') && !*args) {
	zwarnnam(nam, "what do you want to unload?");
	return 1;
    }
    if (OPT_ISSET(ops,'e') && (OPT_ISSET(ops,'I') || OPT_ISSET(ops,'L') || 
			       OPT_ISSET(ops,'a') || OPT_ISSET(ops,'d') ||
			       OPT_ISSET(ops,'i') || OPT_ISSET(ops,'u'))) {
	zwarnnam(nam, "-e cannot be combined with other options");
	return 1;
    }
    queue_signals();
    if (OPT_ISSET(ops,'e'))
	ret = bin_zmodload_exist(nam, args, ops);
    else if (OPT_ISSET(ops,'d'))
	ret = bin_zmodload_dep(nam, args, ops);
    else if ((OPT_ISSET(ops,'a') || OPT_ISSET(ops,'b')) && 
	     !(OPT_ISSET(ops,'c') || OPT_ISSET(ops,'p') || OPT_ISSET(ops,'f')))
	ret = bin_zmodload_auto(nam, args, ops);
    else if (OPT_ISSET(ops,'c') && !(OPT_ISSET(ops,'b') || OPT_ISSET(ops,'p')))
	ret = bin_zmodload_cond(nam, args, ops);
    else if (OPT_ISSET(ops,'f') && !(OPT_ISSET(ops,'b') || OPT_ISSET(ops,'p')))
	ret = bin_zmodload_math(nam, args, ops);
    else if (OPT_ISSET(ops,'p') && !(OPT_ISSET(ops,'b') || OPT_ISSET(ops,'c')))
	ret = bin_zmodload_param(nam, args, ops);
    else if (!(OPT_ISSET(ops,'a') || OPT_ISSET(ops,'b') || 
	       OPT_ISSET(ops,'c') || OPT_ISSET(ops,'p')))
	ret = bin_zmodload_load(nam, args, ops);
    else
	zwarnnam(nam, "use only one of -b, -c, or -p");
    unqueue_signals();

    return ret;
}

/**/
static int
bin_zmodload_alias(char *nam, char **args, Options ops)
{
    /*
     * TODO: while it would be too nasty to have aliases, as opposed
     * to real loadable modules, with dependencies --- just what would
     * we need to load when, exactly? --- there is in principle no objection
     * to making it possible to force an alias onto an existing unloaded
     * module which has dependencies.  This would simply transfer
     * the dependencies down the line to the aliased-to module name.
     * This is actually useful, since then you can alias zsh/zle=mytestzle
     * to load another version of zle.  But then what happens when the
     * alias is removed?  Do you transfer the dependencies back? And
     * suppose other names are aliased to the same file?  It might be
     * kettle of fish best left unwormed.
     */
    LinkNode node;
    Module m;

    if (!*args) {
	if (OPT_ISSET(ops,'R')) {
	    zwarnnam(nam, "no module alias to remove");
	    return 1;
	}
	for (node = firstnode(modules); node; incnode(node)) {
	    m = (Module) getdata(node);
	    if (m->flags & MOD_ALIAS)
		printmodalias(m, ops);
	}
	return 0;
    }

    for (; *args; args++) {
	char *eqpos = strchr(*args, '=');
	char *aliasname = eqpos ? eqpos+1 : NULL;
	if (eqpos)
	    *eqpos = '\0';
	if (!modname_ok(*args)) {
	    zwarnnam(nam, "invalid module name `%s'", *args);
	    return 1;
	}
	if (OPT_ISSET(ops,'R')) {
	    if (aliasname) {
		zwarnnam(nam, "bad syntax for removing module alias: %s",
			 *args);
		return 1;
	    }
	    node = find_module(*args, 0, NULL);
	    if (node) {
		m = (Module) getdata(node);
		if (!(m->flags & MOD_ALIAS)) {
		    zwarnnam(nam, "module is not an alias: %s", *args);
		    return 1;
		}
		delete_module(node);
	    } else {
		zwarnnam(nam, "no such module alias: %s", *args);
		return 1;
	    }
	} else {
	    if (aliasname) {
		const char *mname = aliasname;
		if (!modname_ok(aliasname)) {
		    zwarnnam(nam, "invalid module name `%s'", aliasname);
		    return 1;
		}
		do {
		    if (!strcmp(mname, *args)) {
			zwarnnam(nam, "module alias would refer to itself: %s",
				 *args);
			return 1;
		    }
		} while ((node = find_module(mname, 0, NULL))
			 && ((m = (Module) getdata(node))->flags & MOD_ALIAS)
			 && (mname = m->u.alias));
		node = find_module(*args, 0, NULL);
		if (node) {
		    m = (Module) getdata(node);
		    if (!(m->flags & MOD_ALIAS)) {
			zwarnnam(nam, "module is not an alias: %s", *args);
			return 1;
		    }
		    zsfree(m->u.alias);
		} else {
		    m = (Module) zshcalloc(sizeof(*m));
		    m->nam = ztrdup(*args);
		    m->flags = MOD_ALIAS;
		    zaddlinknode(modules, m);
		}
		m->u.alias = ztrdup(aliasname);
	    } else {
		if ((node = find_module(*args, 0, NULL))) {
		    m = (Module) getdata(node);
		    if (m->flags & MOD_ALIAS)
			printmodalias(m, ops);
		    else {
			zwarnnam(nam, "module is not an alias: %s", *args);
			return 1;
		    }
		} else {
		    zwarnnam(nam, "no such module alias: %s", *args);
		    return 1;
		}
	    }
	}
    }

    return 0;
}

/**/
static int
bin_zmodload_exist(UNUSED(char *nam), char **args, Options ops)
{
    LinkNode node;
    Module m;
    char *modname;

    if (!*args) {
	for (node = firstnode(modules); node; incnode(node)) {
	    m = (Module) getdata(node);
	    modname = m->nam;
	    if (m->flags & MOD_ALIAS) {
		LinkNode node2;
		if (OPT_ISSET(ops,'A') && 
		    (node2 = find_module(m->u.alias, 1, NULL)))
		    m = (Module) getdata(node2);
		else
		    continue;
	    } 
	    if (m->u.handle && !(m->flags & MOD_UNLOAD)) {
		nicezputs(modname, stdout);
		putchar('\n');
	    }
	}
	return 0;
    } else {
	int ret = 0;

	for (; !ret && *args; args++) {
	    if (!(node = find_module(*args, 1, NULL))
		|| !(m = (Module) getdata(node))->u.handle
		|| (m->flags & MOD_UNLOAD))
		ret = 1;
	}
	return ret;
    }
}

/**/
static int
bin_zmodload_dep(UNUSED(char *nam), char **args, Options ops)
{
    LinkNode node;
    Module m;
    if (OPT_ISSET(ops,'u')) {
	/* remove dependencies, which can't pertain to aliases */
	const char *tnam = *args++;
	node = find_module(tnam, 1, &tnam);
	if (!node)
	    return 0;
	m = (Module) getdata(node);
	if (*args && m->deps) {
	    do {
		LinkNode dnode;
		for (dnode = firstnode(m->deps); dnode; incnode(dnode))
		    if (!strcmp(*args, getdata(dnode))) {
			zsfree(getdata(dnode));
			remnode(m->deps, dnode);
			break;
		    }
	    } while(*++args);
	    if (empty(m->deps)) {
		freelinklist(m->deps, freestr);
		m->deps = NULL;
	    }
	} else {
	    if (m->deps) {
		freelinklist(m->deps, freestr);
		m->deps = NULL;
	    }
	}
	if (!m->deps && !m->u.handle)
	    delete_module(node);
	return 0;
    } else if (!args[0] || !args[1]) {
	/* list dependencies */
	for (node = firstnode(modules); node; incnode(node)) {
	    m = (Module) getdata(node);
	    if (m->deps && (!args[0] || !strcmp(args[0], m->nam))) {
		LinkNode n;
		if (OPT_ISSET(ops,'L')) {
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
		    if(OPT_ISSET(ops,'L'))
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

	for (; *args; args++)
	    add_dep(tnam, *args);
	return ret;
    }
}

/**/
static int
bin_zmodload_auto(char *nam, char **args, Options ops)
{
    int ret = 0;
    if(OPT_ISSET(ops,'u')) {
	/* remove autoloaded builtins */
	for (; *args; args++) {
	    Builtin bn = (Builtin) builtintab->getnode2(builtintab, *args);
	    if (!bn) {
		if(!OPT_ISSET(ops,'i')) {
		    zwarnnam(nam, "%s: no such builtin", *args);
		    ret = 1;
		}
	    } else if (bn->node.flags & BINF_ADDED) {
		zwarnnam(nam, "%s: builtin is already defined", *args);
		ret = 1;
	    } else
		deletebuiltin(*args);
	}
	return ret;
    } else if(!*args) {
	/* list autoloaded builtins */
	scanhashtable(builtintab, 1, 0, 0,
	    autoloadscan, OPT_ISSET(ops,'L') ? PRINT_LIST : 0);
	return 0;
    } else {
	/* add autoloaded builtins */
	char *modnam;
	modnam = *args++;
	do {
	    char *bnam = *args ? *args++ : modnam;
	    if (strchr(bnam, '/')) {
		zwarnnam(nam, "%s: `/' is illegal in a builtin", bnam);
		ret = 1;
	    } else if (add_autobin(bnam, modnam) && !OPT_ISSET(ops,'i')) {
		zwarnnam(nam, "failed to add builtin %s", bnam);
		ret = 1;
	    }
	} while(*args);
	return ret;
    }
}

/**/
static int
bin_zmodload_cond(char *nam, char **args, Options ops)
{
    int ret = 0;

    if (OPT_ISSET(ops,'u')) {
	/* remove autoloaded conditions */
	for (; *args; args++) {
	    Conddef cd = getconddef(OPT_ISSET(ops,'I'), *args, 0);

	    if (!cd) {
		if (!OPT_ISSET(ops,'i')) {
		    zwarnnam(nam, "%s: no such condition", *args);
		    ret = 1;
		}
	    } else if (cd->flags & CONDF_ADDED) {
		zwarnnam(nam, "%s: condition is already defined", *args);
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
		if (OPT_ISSET(ops,'L')) {
		    fputs("zmodload -ac", stdout);
		    if (p->flags & CONDF_INFIX)
			putchar('I');
		    printf(" %s %s\n", p->module, p->name);
		} else {
		    if (p->flags & CONDF_INFIX)
			fputs("infix ", stdout);
		    else
			fputs("post ", stdout);
		    printf("%s (%s)\n",p->name, p->module);
		}
	    }
	}
	return 0;
    } else {
	/* add autoloaded conditions */
	char *modnam;

	modnam = *args++;
	do {
	    char *cnam = *args ? *args++ : modnam;
	    if (strchr(cnam, '/')) {
		zwarnnam(nam, "%s: `/' is illegal in a condition", cnam);
		ret = 1;
	    } else if (add_autocond(cnam, OPT_ISSET(ops,'I'), modnam) &&
		       !OPT_ISSET(ops,'i')) {
		zwarnnam(nam, "failed to add condition `%s'", cnam);
		ret = 1;
	    }
	} while(*args);
	return ret;
    }
}

/**/
static int
bin_zmodload_math(char *nam, char **args, Options ops)
{
    int ret = 0;

    if (OPT_ISSET(ops,'u')) {
	/* remove autoloaded math functions */
	for (; *args; args++) {
	    MathFunc f = getmathfunc(*args, 0);

	    if (!f) {
		if (!OPT_ISSET(ops,'i')) {
		    zwarnnam(nam, "%s: no such math function", *args);
		    ret = 1;
		}
	    } else if (f->flags & MFF_ADDED) {
		zwarnnam(nam, "%s: math function is already defined", *args);
		ret = 1;
	    } else
		deletemathfunc(f);
	}
	return ret;
    } else if (!*args) {
	/* list autoloaded math functions */
	MathFunc p;

	for (p = mathfuncs; p; p = p->next) {
	    if (!(p->flags & MFF_USERFUNC) && p->module) {
		if (OPT_ISSET(ops,'L')) {
		    fputs("zmodload -af", stdout);
		    printf(" %s %s\n", p->module, p->name);
		} else
		    printf("%s (%s)\n",p->name, p->module);
	    }
	}
	return 0;
    } else {
	/* add autoloaded math functions */
	char *modnam;

	modnam = *args++;
	do {
	    char *fnam = *args ? *args++ : modnam;
	    if (strchr(fnam, '/')) {
		zwarnnam(nam, "%s: `/' is illegal in a math function", fnam);
		ret = 1;
	    } else if (add_automathfunc(fnam, modnam) && !OPT_ISSET(ops,'i')) {
		zwarnnam(nam, "failed to add math function `%s'", fnam);
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

    if (pm->node.flags & PM_AUTOLOAD) {
	if (lon)
	    printf("zmodload -ap %s %s\n", pm->u.str, pm->node.nam);
	else
	    printf("%s (%s)\n", pm->node.nam, pm->u.str);
    }
}

/**/
static int
bin_zmodload_param(char *nam, char **args, Options ops)
{
    int ret = 0;

    if (OPT_ISSET(ops,'u')) {
	/* remove autoloaded parameters */
	for (; *args; args++) {
	    Param pm = (Param) gethashnode2(paramtab, *args);

	    if (!pm) {
		if (!OPT_ISSET(ops,'i')) {
		    zwarnnam(nam, "%s: no such parameter", *args);
		    ret = 1;
		}
	    } else if (!(pm->node.flags & PM_AUTOLOAD)) {
		zwarnnam(nam, "%s: parameter is already defined", *args);
		ret = 1;
	    } else
		unsetparam_pm(pm, 0, 1);
	}
	return ret;
    } else if (!*args) {
	scanhashtable(paramtab, 1, 0, 0, printautoparams, OPT_ISSET(ops,'L'));
	return 0;
    } else {
	/* add autoloaded parameters */
	char *modnam;

	modnam = *args++;
	do {
	    char *pnam = *args ? *args++ : modnam;
	    if (strchr(pnam, '/')) {
		zwarnnam(nam, "%s: `/' is illegal in a parameter", pnam);
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
    /*
     * Only unload the real module, so resolve aliases.
     */
    if (m->flags & MOD_ALIAS) {
	LinkNode node = find_module(m->u.alias, 1, NULL);
	if (!node)
	    return 1;
	m = (Module) getdata(node);
    }
    if ((m->flags & MOD_INIT_S) &&
	!(m->flags & MOD_UNLOAD) &&
	((m->flags & MOD_LINKED) ?
	 (m->u.linked && m->u.linked->cleanup(m)) :
	 (m->u.handle && cleanup_module(m))))
	return 1;
    else {
	int del = (m->flags & MOD_UNLOAD);

	if (m->wrapper) {
	    m->flags |= MOD_UNLOAD;
	    return 0;
	}
	m->flags &= ~MOD_UNLOAD;
	if (m->flags & MOD_INIT_B) {
	    if (m->flags & MOD_LINKED) {
		if (m->u.linked) {
		    m->u.linked->finish(m);
		    m->u.linked = NULL;
		}
	    } else {
		if (m->u.handle) {
		    finish_module(m);
		    m->u.handle = NULL;
		}
	    }
	}
	if (del && m->deps) {
	    /* The module was unloaded delayed, unload all modules *
	     * on which it depended. */
	    LinkNode n;

	    for (n = firstnode(m->deps); n; incnode(n)) {
		LinkNode dn = find_module((char *) getdata(n), 1, NULL);
		Module dm;

		if (dn && (dm = (Module) getdata(dn)) &&
		    (dm->flags & MOD_UNLOAD)) {
		    /* See if this is the only module depending on it. */

		    LinkNode an;
		    Module am;
		    int du = 1;

		    for (an = firstnode(modules); du && an; incnode(an)) {
			am = (Module) getdata(an);
			if (am != m && am->deps &&
			    ((am->flags & MOD_LINKED) ?
			     am->u.linked : am->u.handle)) {
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
	    delete_module(node);
	}
    }
    return 0;
}


/**/
int
unload_named_module(char *modname, char *nam, int silent)
{
    const char *mname;
    LinkNode node;
    Module m;
    int ret = 0;

    node = find_module(modname, 1, &mname);
    if (node) {
	LinkNode mn, dn;
	int del = 0;

	for (mn = firstnode(modules); mn; incnode(mn)) {
	    m = (Module) getdata(mn);
	    if (m->deps && m->u.handle)
		for (dn = firstnode(m->deps); dn; incnode(dn))
		    if (!strcmp((char *) getdata(dn), mname)) {
			if (m->flags & MOD_UNLOAD)
			    del = 1;
			else {
			    zwarnnam(nam, "module %s is in use by another module and cannot be unloaded", mname);
			    return 1;
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
    } else if (!silent) {
	zwarnnam(nam, "no such module %s", modname);
	ret = 1;
    }

    return ret;
}


/**/
static int
bin_zmodload_load(char *nam, char **args, Options ops)
{
    LinkNode node;
    Module m;
    int ret = 0;
    if(OPT_ISSET(ops,'u')) {
	/* unload modules */
	for(; *args; args++) {
	    if (unload_named_module(*args, nam, OPT_ISSET(ops,'i')))
		ret = 1;
	}
	return ret;
    } else if(!*args) {
	/* list modules */
	for (node = firstnode(modules); node; incnode(node)) {
	    m = (Module) getdata(node);
	    if (m->u.handle && !(m->flags & (MOD_UNLOAD|MOD_ALIAS))) {
		if(OPT_ISSET(ops,'L')) {
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
	    if (!require_module(nam, *args, 1, (!OPT_ISSET(ops,'i'))))
		ret = 1;

	return ret;
    }
}

/* The list of module-defined conditions. */

/**/
mod_export Conddef condtab;

/* This gets a condition definition with the given name. The first        *
 * argument says if we have to look for an infix condition. The last      *
 * argument is non-zero if we should autoload modules if needed. */

/**/
Conddef
getconddef(int inf, char *name, int autol)
{
    Conddef p;
    int f = 1;

    do {
	for (p = condtab; p; p = p->next) {
	    if ((!!inf == !!(p->flags & CONDF_INFIX)) &&
		!strcmp(name, p->name))
		break;
	}
	if (autol && p && p->module) {
	    /* This is a definition for an autoloaded condition, load the *
	     * module if we haven't tried that already. */
	    if (f) {
		load_module_silence(p->module, 0);
		f = 0;
		p = NULL;
	    } else {
		deleteconddef(p);
		return NULL;
	    }
	} else
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
	/* There is an autoload definition. */

	deleteconddef(p);
    }
    c->next = condtab;
    condtab = c;
    return 0;
}

/* This adds multiple condition definitions. This is like addbuiltins(). */

/**/
mod_export int
addconddefs(char const *nam, Conddef c, int size)
{
    int hads = 0, hadf = 0;

    while (size--) {
	if (c->flags & CONDF_ADDED) {
	    c++;
	    continue;
	}
	if (addconddef(c)) {
	    zwarnnam(nam, "name clash when adding condition `%s'", c->name);
	    hadf = 1;
	} else {
	    c->flags |= CONDF_ADDED;
	    hads = 2;
	}
	c++;
    }
    return hadf ? hads : 1;
}

/* This list of hook functions defined. */

/**/
Hookdef hooktab;

/* Find a hook definition given the name. */

/**/
Hookdef
gethookdef(char *n)
{
    Hookdef p;

    for (p = hooktab; p; p = p->next)
	if (!strcmp(n, p->name))
	    return p;
    return NULL;
}

/* This adds the given hook definition. The return value is zero on      *
 * success and 1 on failure.                                             */

/**/
int
addhookdef(Hookdef h)
{
    if (gethookdef(h->name))
	return 1;

    h->next = hooktab;
    hooktab = h;
    h->funcs = znewlinklist();

    return 0;
}

/* This adds multiple hook definitions. This is like addbuiltins(). */

/**/
mod_export int
addhookdefs(char const *nam, Hookdef h, int size)
{
    int hads = 0, hadf = 0;

    while (size--) {
	if (addhookdef(h)) {
	    zwarnnam(nam, "name clash when adding hook `%s'", h->name);
	    hadf = 1;
	} else
	    hads = 2;
	h++;
    }
    return hadf ? hads : 1;
}

/* Delete hook definitions. */

/**/
int
deletehookdef(Hookdef h)
{
    Hookdef p, q;

    for (p = hooktab, q = NULL; p && p != h; q = p, p = p->next);

    if (!p)
	return 1;

    if (q)
	q->next = p->next;
    else
	hooktab = p->next;
    freelinklist(p->funcs, NULL);
    return 0;
}

/**/
mod_export int
deletehookdefs(UNUSED(char const *nam), Hookdef h, int size)
{
    while (size--) {
	deletehookdef(h);
	h++;
    }
    return 1;
}

/* Add a function to a hook. */

/**/
int
addhookdeffunc(Hookdef h, Hookfn f)
{
    zaddlinknode(h->funcs, (void *) f);

    return 0;
}

/**/
mod_export int
addhookfunc(char *n, Hookfn f)
{
    Hookdef h = gethookdef(n);

    if (h)
	return addhookdeffunc(h, f);
    return 1;
}

/* Delete a function from a hook. */

/**/
int
deletehookdeffunc(Hookdef h, Hookfn f)
{
    LinkNode p;

    for (p = firstnode(h->funcs); p; incnode(p))
	if (f == (Hookfn) getdata(p)) {
	    remnode(h->funcs, p);
	    return 0;
	}
    return 1;
}

/**/
mod_export int
deletehookfunc(char *n, Hookfn f)
{
    Hookdef h = gethookdef(n);

    if (h)
	return deletehookdeffunc(h, f);
    return 1;
}

/* Run the function(s) for a hook. */

/**/
mod_export int
runhookdef(Hookdef h, void *d)
{
    if (empty(h->funcs)) {
	if (h->def)
	    return h->def(h, d);
	return 0;
    } else if (h->flags & HOOKF_ALL) {
	LinkNode p;
	int r;

	for (p = firstnode(h->funcs); p; incnode(p))
	    if ((r = ((Hookfn) getdata(p))(h, d)))
		return r;
	if (h->def)
	    return h->def(h, d);
	return 0;
    } else
	return ((Hookfn) getdata(lastnode(h->funcs)))(h, d);
}

/**/
int
runhook(char *n, void *d)
{
    Hookdef h = gethookdef(n);

    if (h)
	return runhookdef(h, d);
    return 0;
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
    if (d->gsu)
	pm->gsu.i = (GsuInteger) d->gsu;
    else {
	/*
	 * If no get/set/unset class, use the appropriate
	 * variable type.
	 */
	switch (PM_TYPE(pm->node.flags)) {
	case PM_SCALAR:
	    pm->gsu.s = &varscalar_gsu;
	    break;

	case PM_INTEGER:
	    pm->gsu.i = &varinteger_gsu;
	    break;

	case PM_ARRAY:
	    pm->gsu.a = &vararray_gsu;
	    break;

	default:
	    unsetparam_pm(pm, 0, 1);
	    return 1;
	}
    }

    return 0;
}

/* This adds multiple parameter definitions. This is like addbuiltins(). */

/**/
mod_export int
addparamdefs(char const *nam, Paramdef d, int size)
{
    int hads = 0, hadf = 0;

    while (size--) {
	if (addparamdef(d)) {
	    zwarnnam(nam, "error when adding parameter `%s'", d->name);
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
mod_export int
deleteparamdefs(UNUSED(char const *nam), Paramdef d, int size)
{
    while (size--) {
	deleteparamdef(d);
	d++;
    }
    return 1;
}

/* This adds a definition for autoloading a module for a condition. */

/**/
int
add_autocond(char *nam, int inf, char *module)
{
    Conddef c = (Conddef) zalloc(sizeof(*c));

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
mod_export int
deleteconddefs(char const *nam, Conddef c, int size)
{
    int hads = 0, hadf = 0;

    while (size--) {
	if (!(c->flags & CONDF_ADDED)) {
	    c++;
	    continue;
	}
	if (deleteconddef(c)) {
	    zwarnnam(nam, "condition `%s' already deleted", c->name);
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

    queue_signals();
    if ((pm = (Param) gethashnode2(paramtab, nam)))
	unsetparam_pm(pm, 0, 1);

    pm = setsparam(nam, ztrdup(module));

    pm->node.flags |= PM_AUTOLOAD;
    unqueue_signals();
}

/* List of math functions. */

/**/
MathFunc mathfuncs;

/**/
void
removemathfunc(MathFunc previous, MathFunc current)
{
    if (previous)
	previous->next = current->next;
    else
	mathfuncs = current->next;

    zsfree(current->name);
    zsfree(current->module);
    zfree(current, sizeof(*current));
}

/**/
MathFunc
getmathfunc(char *name, int autol)
{
    MathFunc p, q = NULL;

    for (p = mathfuncs; p; q = p, p = p->next)
	if (!strcmp(name, p->name)) {
	    if (autol && p->module && !(p->flags & MFF_USERFUNC)) {
		char *n = dupstring(p->module);

		removemathfunc(q, p);

		load_module_silence(n, 0);

		return getmathfunc(name, 0);
	    }
	    return p;
	}

    return NULL;
}

/**/
mod_export int
addmathfunc(MathFunc f)
{
    MathFunc p, q = NULL;

    if (f->flags & MFF_ADDED)
	return 1;

    for (p = mathfuncs; p; q = p, p = p->next)
	if (!strcmp(f->name, p->name)) {
	    if (p->module && !(p->flags & MFF_USERFUNC)) {
		/*
		 * Autoloadable, replace.
		 */
		removemathfunc(q, p);
		break;
	    }
	    return 1;
	}

    f->flags |= MFF_ADDED;
    f->next = mathfuncs;
    mathfuncs = f;

    return 0;
}

/**/
mod_export int
addmathfuncs(char const *nam, MathFunc f, int size)
{
    int hads = 0, hadf = 0;

    while (size--) {
	if (f->flags & MFF_ADDED) {
	    f++;
	    continue;
	}
	if (addmathfunc(f)) {
	    zwarnnam(nam, "name clash when adding math function `%s'",
		     f->name);
	    hadf = 1;
	} else
	    hads = 2;
	f++;
    }
    return hadf ? hads : 1;
}

/**/
int
add_automathfunc(char *nam, char *module)
{
    MathFunc f = (MathFunc) zalloc(sizeof(*f));

    f->name = ztrdup(nam);
    f->module = ztrdup(module);
    f->flags = 0;

    if (addmathfunc(f)) {
	zsfree(f->name);
	zsfree(f->module);
	zfree(f, sizeof(*f));

	return 1;
    }
    f->flags &= ~MFF_ADDED; /* still to autoload, not added yet */

    return 0;
}

/**/
mod_export int
deletemathfunc(MathFunc f)
{
    MathFunc p, q;

    for (p = mathfuncs, q = NULL; p && p != f; q = p, p = p->next);

    if (p) {
	if (q)
	    q->next = f->next;
	else
	    mathfuncs = f->next;

	/* the following applies to both unloaded and user-defined functions */
	if (f->module) {
	    zsfree(f->name);
	    zsfree(f->module);
	    zfree(f, sizeof(*f));
	} else
	    f->flags &= ~MFF_ADDED;

	return 0;
    }
    return -1;
}

/**/
mod_export int
deletemathfuncs(char const *nam, MathFunc f, int size)
{
    int hads = 0, hadf = 0;

    while (size--) {
	if (!(f->flags & MFF_ADDED)) {
	    f++;
	    continue;
	}
	if (deletemathfunc(f)) {
	    zwarnnam(nam, "math function `%s' already deleted", f->name);
	    hadf = 1;
	} else
	    hads = 2;
	f++;
    }
    return hadf ? hads : 1;
}
