/*
 * params.c - parameters
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 1992-1997 Paul Falstad
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Paul Falstad or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Paul Falstad and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Paul Falstad and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Paul Falstad and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "zsh.mdh"
#include "params.pro"

#include "version.h"

/* what level of localness we are at */
 
/**/
mod_export int locallevel;
 
/* Variables holding values of special parameters */
 
/**/
mod_export
char **pparams,		/* $argv        */
     **cdpath,		/* $cdpath      */
     **fpath,		/* $fpath       */
     **mailpath,	/* $mailpath    */
     **manpath,		/* $manpath     */
     **psvar,		/* $psvar       */
     **watch;		/* $watch       */
/**/
mod_export
char **path,		/* $path        */
     **fignore;		/* $fignore     */
 
/**/
char *argzero,		/* $0           */
     *home,		/* $HOME        */
     *nullcmd,		/* $NULLCMD     */
     *oldpwd,		/* $OLDPWD      */
     *zoptarg,		/* $OPTARG      */
     *prompt,		/* $PROMPT      */
     *prompt2,		/* $PROMPT2     */
     *prompt3,		/* $PROMPT3     */
     *prompt4,		/* $PROMPT4     */
     *readnullcmd,	/* $READNULLCMD */
     *rprompt,		/* $RPROMPT     */
     *rprompt2,		/* $RPROMPT2    */
     *sprompt,		/* $SPROMPT     */
     *wordchars,	/* $WORDCHARS   */
     *zsh_name;		/* $ZSH_NAME    */
/**/
mod_export
char *ifs,		/* $IFS         */
     *postedit,		/* $POSTEDIT    */
     *term,		/* $TERM        */
     *ttystrname,	/* $TTY         */
     *pwd;		/* $PWD         */

/**/
mod_export
zlong lastval,		/* $?           */
     mypid,		/* $$           */
     lastpid,		/* $!           */
     columns,		/* $COLUMNS     */
     lines,		/* $LINES       */
     ppid;		/* $PPID        */
/**/
zlong lineno,		/* $LINENO      */
     zoptind,		/* $OPTIND      */
     shlvl;		/* $SHLVL       */

/* $histchars */
 
/**/
mod_export unsigned char bangchar;
/**/
unsigned char hatchar, hashchar;
 
/* $SECONDS = now.tv_sec - shtimer.tv_sec
 *          + (now.tv_usec - shtimer.tv_usec) / 1000000.0
 * (rounded to an integer if the parameter is not set to float) */
 
/**/
struct timeval shtimer;
 
/* 0 if this $TERM setup is usable, otherwise it contains TERM_* flags */

/**/
mod_export int termflags;
 
/* Nodes for special parameters for parameter hash table */

#ifdef HAVE_UNION_INIT
# define BR(X) {X}
typedef struct param initparam;
#else
# define BR(X) X
typedef struct iparam {
    struct hashnode *next;
    char *nam;			/* hash data                             */
    int flags;			/* PM_* flags (defined in zsh.h)         */
    void *value;
    void (*func1) _((void));	/* set func                              */
    char *(*func2) _((void));	/* get func                              */
    void (*unsetfn) _((Param, int));	/* unset func                    */
    int ct;			/* output base or field width            */
    char *env;			/* location in environment, if exported  */
    char *ename;		/* name of corresponding environment var */
    Param old;			/* old struct for use with local         */
    int level;			/* if (old != NULL), level of localness  */
} initparam;
#endif

static initparam special_params[] ={
#define SFN(X) BR(((void (*)_((Param, char *)))(X)))
#define GFN(X) BR(((char *(*)_((Param)))(X)))
#define IPDEF1(A,B,C,D) {NULL,A,PM_INTEGER|PM_SPECIAL|D,BR(NULL),SFN(C),GFN(B),stdunsetfn,10,NULL,NULL,NULL,0}
IPDEF1("#", poundgetfn, nullintsetfn, PM_READONLY),
IPDEF1("ERRNO", errnogetfn, nullintsetfn, PM_READONLY),
IPDEF1("GID", gidgetfn, gidsetfn, PM_DONTIMPORT | PM_RESTRICTED),
IPDEF1("EGID", egidgetfn, egidsetfn, PM_DONTIMPORT | PM_RESTRICTED),
IPDEF1("HISTSIZE", histsizegetfn, histsizesetfn, PM_RESTRICTED),
IPDEF1("RANDOM", randomgetfn, randomsetfn, 0),
IPDEF1("SAVEHIST", savehistsizegetfn, savehistsizesetfn, PM_RESTRICTED),
IPDEF1("SECONDS", intsecondsgetfn, intsecondssetfn, 0),
IPDEF1("UID", uidgetfn, uidsetfn, PM_DONTIMPORT | PM_RESTRICTED),
IPDEF1("EUID", euidgetfn, euidsetfn, PM_DONTIMPORT | PM_RESTRICTED),
IPDEF1("TTYIDLE", ttyidlegetfn, nullintsetfn, PM_READONLY),

#define IPDEF2(A,B,C,D) {NULL,A,PM_SCALAR|PM_SPECIAL|D,BR(NULL),SFN(C),GFN(B),stdunsetfn,0,NULL,NULL,NULL,0}
IPDEF2("USERNAME", usernamegetfn, usernamesetfn, PM_DONTIMPORT|PM_RESTRICTED),
IPDEF2("-", dashgetfn, nullstrsetfn, PM_READONLY),
IPDEF2("histchars", histcharsgetfn, histcharssetfn, PM_DONTIMPORT),
IPDEF2("HOME", homegetfn, homesetfn, 0),
IPDEF2("TERM", termgetfn, termsetfn, 0),
IPDEF2("WORDCHARS", wordcharsgetfn, wordcharssetfn, 0),
IPDEF2("IFS", ifsgetfn, ifssetfn, PM_DONTIMPORT),
IPDEF2("_", underscoregetfn, nullstrsetfn, PM_READONLY),

#ifdef USE_LOCALE
# define LCIPDEF(name) IPDEF2(name, strgetfn, lcsetfn, PM_UNSET)
IPDEF2("LANG", strgetfn, langsetfn, PM_UNSET),
IPDEF2("LC_ALL", strgetfn, lc_allsetfn, PM_UNSET),
# ifdef LC_COLLATE
LCIPDEF("LC_COLLATE"),
# endif
# ifdef LC_CTYPE
LCIPDEF("LC_CTYPE"),
# endif
# ifdef LC_MESSAGES
LCIPDEF("LC_MESSAGES"),
# endif
# ifdef LC_NUMERIC
LCIPDEF("LC_NUMERIC"),
# endif
# ifdef LC_TIME
LCIPDEF("LC_TIME"),
# endif
#endif /* USE_LOCALE */

#define IPDEF4(A,B) {NULL,A,PM_INTEGER|PM_READONLY|PM_SPECIAL,BR((void *)B),SFN(nullintsetfn),GFN(intvargetfn),stdunsetfn,10,NULL,NULL,NULL,0}
IPDEF4("!", &lastpid),
IPDEF4("$", &mypid),
IPDEF4("?", &lastval),
IPDEF4("LINENO", &lineno),
IPDEF4("PPID", &ppid),

#define IPDEF5(A,B,F) {NULL,A,PM_INTEGER|PM_SPECIAL,BR((void *)B),SFN(F),GFN(intvargetfn),stdunsetfn,10,NULL,NULL,NULL,0}
IPDEF5("COLUMNS", &columns, zlevarsetfn),
IPDEF5("LINES", &lines, zlevarsetfn),
IPDEF5("OPTIND", &zoptind, intvarsetfn),
IPDEF5("SHLVL", &shlvl, intvarsetfn),

#define IPDEF7(A,B) {NULL,A,PM_SCALAR|PM_SPECIAL,BR((void *)B),SFN(strvarsetfn),GFN(strvargetfn),stdunsetfn,0,NULL,NULL,NULL,0}
IPDEF7("OPTARG", &zoptarg),
IPDEF7("NULLCMD", &nullcmd),
IPDEF7("POSTEDIT", &postedit),
IPDEF7("READNULLCMD", &readnullcmd),
IPDEF7("PS1", &prompt),
IPDEF7("RPS1", &rprompt),
IPDEF7("RPROMPT", &rprompt),
IPDEF7("PS2", &prompt2),
IPDEF7("RPS2", &rprompt2),
IPDEF7("RPROMPT2", &rprompt2),
IPDEF7("PS3", &prompt3),
IPDEF7("PS4", &prompt4),
IPDEF7("SPROMPT", &sprompt),
IPDEF7("0", &argzero),

#define IPDEF8(A,B,C,D) {NULL,A,D|PM_SCALAR|PM_SPECIAL,BR((void *)B),SFN(colonarrsetfn),GFN(colonarrgetfn),stdunsetfn,0,NULL,C,NULL,0}
IPDEF8("CDPATH", &cdpath, "cdpath", 0),
IPDEF8("FIGNORE", &fignore, "fignore", 0),
IPDEF8("FPATH", &fpath, "fpath", 0),
IPDEF8("MAILPATH", &mailpath, "mailpath", 0),
IPDEF8("WATCH", &watch, "watch", 0),
IPDEF8("PATH", &path, "path", PM_RESTRICTED),
IPDEF8("PSVAR", &psvar, "psvar", 0),

/* MODULE_PATH is not imported for security reasons */
IPDEF8("MODULE_PATH", &module_path, "module_path", PM_DONTIMPORT|PM_RESTRICTED),

#define IPDEF9F(A,B,C,D) {NULL,A,D|PM_ARRAY|PM_SPECIAL|PM_DONTIMPORT,BR((void *)B),SFN(arrvarsetfn),GFN(arrvargetfn),stdunsetfn,0,NULL,C,NULL,0}
#define IPDEF9(A,B,C) IPDEF9F(A,B,C,0)
IPDEF9F("*", &pparams, NULL, PM_ARRAY|PM_SPECIAL|PM_DONTIMPORT|PM_READONLY),
IPDEF9F("@", &pparams, NULL, PM_ARRAY|PM_SPECIAL|PM_DONTIMPORT|PM_READONLY),
{NULL, NULL},
#define IPDEF10(A,B,C) {NULL,A,PM_ARRAY|PM_SPECIAL,BR(NULL),SFN(C),GFN(B),stdunsetfn,10,NULL,NULL,NULL,0}

/* The following parameters are not available in sh/ksh compatibility *
 * mode. All of these have sh compatible equivalents.                */
IPDEF1("ARGC", poundgetfn, nullintsetfn, PM_READONLY),
IPDEF2("HISTCHARS", histcharsgetfn, histcharssetfn, PM_DONTIMPORT),
IPDEF4("status", &lastval),
IPDEF7("prompt", &prompt),
IPDEF7("PROMPT", &prompt),
IPDEF7("PROMPT2", &prompt2),
IPDEF7("PROMPT3", &prompt3),
IPDEF7("PROMPT4", &prompt4),
IPDEF8("MANPATH", &manpath, "manpath", 0),
IPDEF9("argv", &pparams, NULL),
IPDEF9("fignore", &fignore, "FIGNORE"),
IPDEF9("cdpath", &cdpath, "CDPATH"),
IPDEF9("fpath", &fpath, "FPATH"),
IPDEF9("mailpath", &mailpath, "MAILPATH"),
IPDEF9("manpath", &manpath, "MANPATH"),
IPDEF9("psvar", &psvar, "PSVAR"),
IPDEF9("watch", &watch, "WATCH"),

IPDEF9F("module_path", &module_path, "MODULE_PATH", PM_RESTRICTED),
IPDEF9F("path", &path, "PATH", PM_RESTRICTED),

IPDEF10("pipestatus", pipestatgetfn, pipestatsetfn),

{NULL, NULL}
};

/*
 * Special way of referring to the positional parameters.  Unlike $*
 * and $@, this is not readonly.  This parameter is not directly
 * visible in user space.
 */
initparam argvparam_pm = IPDEF9F("", &pparams, NULL, \
				 PM_ARRAY|PM_SPECIAL|PM_DONTIMPORT);

#undef BR

#define IS_UNSET_VALUE(V) \
	((V) && (!(V)->pm || ((V)->pm->flags & PM_UNSET) || \
		 !(V)->pm->nam || !*(V)->pm->nam))

static Param argvparam;

/* hash table containing the parameters */
 
/**/
mod_export HashTable paramtab, realparamtab;

/**/
mod_export HashTable
newparamtable(int size, char const *name)
{
    HashTable ht;
    if (!size)
	size = 17;
    ht = newhashtable(size, name, NULL);

    ht->hash        = hasher;
    ht->emptytable  = emptyhashtable;
    ht->filltable   = NULL;
    ht->cmpnodes    = strcmp;
    ht->addnode     = addhashnode;
    ht->getnode     = getparamnode;
    ht->getnode2    = getparamnode;
    ht->removenode  = removehashnode;
    ht->disablenode = NULL;
    ht->enablenode  = NULL;
    ht->freenode    = freeparamnode;
    ht->printnode   = printparamnode;

    return ht;
}

/**/
static HashNode
getparamnode(HashTable ht, char *nam)
{
    HashNode hn = gethashnode2(ht, nam);
    Param pm = (Param) hn;

    if (pm && pm->u.str && (pm->flags & PM_AUTOLOAD)) {
	char *mn = dupstring(pm->u.str);

	if (!load_module(mn))
	    return NULL;
	hn = gethashnode2(ht, nam);
	if (((Param) hn) == pm && (pm->flags & PM_AUTOLOAD)) {
	    pm->flags &= ~PM_AUTOLOAD;
	    zwarnnam(nam, "autoload failed", NULL, 0);
	}
    }
    return hn;
}

/* Copy a parameter hash table */

static HashTable outtable;

/**/
static void
scancopyparams(HashNode hn, int flags)
{
    /* Going into a real parameter, so always use permanent storage */
    Param pm = (Param)hn;
    Param tpm = (Param) zcalloc(sizeof *tpm);
    tpm->nam = ztrdup(pm->nam);
    copyparam(tpm, pm, 0);
    addhashnode(outtable, tpm->nam, tpm);
}

/**/
HashTable
copyparamtable(HashTable ht, char *name)
{
    HashTable nht = newparamtable(ht->hsize, name);
    outtable = nht;
    scanhashtable(ht, 0, 0, 0, scancopyparams, 0);
    outtable = NULL;
    return nht;
}

/* Flag to freeparamnode to unset the struct */

static int delunset;

/* Function to delete a parameter table. */

/**/
mod_export void
deleteparamtable(HashTable t)
{
    /* The parameters in the hash table need to be unset *
     * before being deleted.                             */
    int odelunset = delunset;
    delunset = 1;
    deletehashtable(t);
    delunset = odelunset;
}

static unsigned numparamvals;

/**/
mod_export void
scancountparams(HashNode hn, int flags)
{
    ++numparamvals;
    if ((flags & SCANPM_WANTKEYS) && (flags & SCANPM_WANTVALS))
	++numparamvals;
}

static Patprog scanprog;
static char *scanstr;
static char **paramvals;

/**/
void
scanparamvals(HashNode hn, int flags)
{
    struct value v;
    Patprog prog;

    if (numparamvals && !(flags & SCANPM_MATCHMANY) &&
	(flags & (SCANPM_MATCHVAL|SCANPM_MATCHKEY|SCANPM_KEYMATCH)))
	return;
    v.pm = (Param)hn;
    if ((flags & SCANPM_KEYMATCH)) {
	char *tmp = dupstring(v.pm->nam);

	tokenize(tmp);
	remnulargs(tmp);

	if (!(prog = patcompile(tmp, 0, NULL)) || !pattry(prog, scanstr))
	    return;
    } else if ((flags & SCANPM_MATCHKEY) && !pattry(scanprog, v.pm->nam)) {
	return;
    }
    if (flags & SCANPM_WANTKEYS) {
	paramvals[numparamvals++] = v.pm->nam;
	if (!(flags & (SCANPM_WANTVALS|SCANPM_MATCHVAL)))
	    return;
    }
    v.isarr = (PM_TYPE(v.pm->flags) & (PM_ARRAY|PM_HASHED));
    v.inv = 0;
    v.start = 0;
    v.end = -1;
    paramvals[numparamvals] = getstrvalue(&v);
    if (flags & SCANPM_MATCHVAL) {
	if (pattry(scanprog, paramvals[numparamvals])) {
	    numparamvals += ((flags & SCANPM_WANTVALS) ? 1 :
			     !(flags & SCANPM_WANTKEYS));
	} else if (flags & SCANPM_WANTKEYS)
	    --numparamvals;	/* Value didn't match, discard key */
    } else
	++numparamvals;
}

/**/
char **
paramvalarr(HashTable ht, int flags)
{
    numparamvals = 0;
    if (ht)
	scanhashtable(ht, 0, 0, PM_UNSET, scancountparams, flags);
    paramvals = (char **) zhalloc((numparamvals + 1) * sizeof(char *));
    if (ht) {
	numparamvals = 0;
	scanhashtable(ht, 0, 0, PM_UNSET, scanparamvals, flags);
    }
    paramvals[numparamvals] = 0;
    return paramvals;
}

/* Return the full array (no indexing) referred to by a Value. *
 * The array value is cached for the lifetime of the Value.    */

/**/
static char **
getvaluearr(Value v)
{
    if (v->arr)
	return v->arr;
    else if (PM_TYPE(v->pm->flags) == PM_ARRAY)
	return v->arr = v->pm->gets.afn(v->pm);
    else if (PM_TYPE(v->pm->flags) == PM_HASHED) {
	v->arr = paramvalarr(v->pm->gets.hfn(v->pm), v->isarr);
	/* Can't take numeric slices of associative arrays */
	v->start = 0;
	v->end = numparamvals + 1;
	return v->arr;
    } else
	return NULL;
}

/*
 * Split environment string into (name, value) pair.
 * this is used to avoid in-place editing of environment table
 * that results in core dump on some systems
 */

static int
split_env_string(char *env, char **name, char **value)
{
    char *str, *tenv;

    if (!env || !name || !value)
	return 0;

    tenv = strcpy(zhalloc(strlen(env) + 1), env);
    for (str = tenv; *str && *str != '='; str++)
	;
    if (str != tenv && *str == '=') {
	*str = '\0';
	*name = tenv;
	*value = str + 1;
	return 1;
    } else
	return 0;
}
    
/* Set up parameter hash table.  This will add predefined  *
 * parameter entries as well as setting up parameter table *
 * entries for environment variables we inherit.           */

/**/
void
createparamtable(void)
{
    Param ip, pm;
#ifndef HAVE_PUTENV
    char **new_environ;
    int  envsize;
#endif
    char **envp, **envp2, **sigptr, **t;
    char buf[50], *str, *iname, *ivalue, *hostnam;
    int  oae = opts[ALLEXPORT];
#ifdef HAVE_UNAME
    struct utsname unamebuf;
    char *machinebuf;
#endif

    paramtab = realparamtab = newparamtable(151, "paramtab");

    /* Add the special parameters to the hash table */
    for (ip = special_params; ip->nam; ip++)
	paramtab->addnode(paramtab, ztrdup(ip->nam), ip);
    if (emulation != EMULATE_SH && emulation != EMULATE_KSH)
	while ((++ip)->nam)
	    paramtab->addnode(paramtab, ztrdup(ip->nam), ip);

    argvparam = (Param) &argvparam_pm;

    noerrs = 2;

    /* Add the standard non-special parameters which have to    *
     * be initialized before we copy the environment variables. *
     * We don't want to override whatever values the user has   *
     * given them in the environment.                           */
    opts[ALLEXPORT] = 0;
    setiparam("MAILCHECK", 60);
    setiparam("LOGCHECK", 60);
    setiparam("KEYTIMEOUT", 40);
    setiparam("LISTMAX", 100);
#ifdef HAVE_SELECT
    setiparam("BAUD", getbaudrate(&shttyinfo));  /* get the output baudrate */
#endif
    setsparam("FCEDIT", ztrdup(DEFAULT_FCEDIT));
    setsparam("TMPPREFIX", ztrdup(DEFAULT_TMPPREFIX));
    setsparam("TIMEFMT", ztrdup(DEFAULT_TIMEFMT));
    setsparam("WATCHFMT", ztrdup(default_watchfmt));

    hostnam = (char *)zalloc(256);
    gethostname(hostnam, 256);
    setsparam("HOST", ztrdup(hostnam));
    zfree(hostnam, 256);

    setsparam("LOGNAME", ztrdup((str = getlogin()) && *str ? str : cached_username));

#ifndef HAVE_PUTENV
    /* Copy the environment variables we are inheriting to dynamic *
     * memory, so we can do mallocs and frees on it.               */
    envsize = sizeof(char *)*(1 + arrlen(environ));
    new_environ = (char **) zalloc(envsize);
    memcpy(new_environ, environ, envsize);
    environ = new_environ;
#endif

    /* Use heap allocation to avoid many small alloc/free calls */
    pushheap();

    /* Now incorporate environment variables we are inheriting *
     * into the parameter hash table. Copy them into dynamic   *
     * memory so that we can free them if needed               */
    for (envp = envp2 = environ; *envp2; envp2++) {
	if (split_env_string(*envp2, &iname, &ivalue)) {
	    if (!idigit(*iname) && isident(iname) && !strchr(iname, '[')) {
		if ((!(pm = (Param) paramtab->getnode(paramtab, iname)) ||
		     !(pm->flags & PM_DONTIMPORT || pm->flags & PM_EXPORTED)) &&
		    (pm = setsparam(iname, metafy(ivalue, -1, META_DUP)))) {
		    pm->flags |= PM_EXPORTED;
		    if (pm->flags & PM_SPECIAL)
			pm->env = mkenvstr (pm->nam,
					    getsparam(pm->nam), pm->flags);
		    else
			pm->env = ztrdup(*envp2);
		    *envp++ = pm->env;
		}
	    }
	}
    }
    popheap();
    *envp = '\0';
    opts[ALLEXPORT] = oae;

    pm = (Param) paramtab->getnode(paramtab, "HOME");
    if (!(pm->flags & PM_EXPORTED)) {
	pm->flags |= PM_EXPORTED;
	pm->env = addenv("HOME", home, pm->flags);
    }
    pm = (Param) paramtab->getnode(paramtab, "LOGNAME");
    if (!(pm->flags & PM_EXPORTED)) {
	pm->flags |= PM_EXPORTED;
	pm->env = addenv("LOGNAME", pm->u.str, pm->flags);
    }
    pm = (Param) paramtab->getnode(paramtab, "SHLVL");
    if (!(pm->flags & PM_EXPORTED))
	pm->flags |= PM_EXPORTED;
    sprintf(buf, "%d", (int)++shlvl);
    pm->env = addenv("SHLVL", buf, pm->flags);

    /* Add the standard non-special parameters */
    set_pwd_env();
#ifdef HAVE_UNAME
    if(uname(&unamebuf)) setsparam("CPUTYPE", ztrdup("unknown"));
    else
    {
       machinebuf = ztrdup(unamebuf.machine);
       setsparam("CPUTYPE", machinebuf);
    }
	
#else
    setsparam("CPUTYPE", ztrdup("unknown"));
#endif
    setsparam("MACHTYPE", ztrdup(MACHTYPE));
    setsparam("OSTYPE", ztrdup(OSTYPE));
    setsparam("TTY", ztrdup(ttystrname));
    setsparam("VENDOR", ztrdup(VENDOR));
    setsparam("ZSH_NAME", ztrdup(zsh_name));
    setsparam("ZSH_VERSION", ztrdup(ZSH_VERSION));
    setaparam("signals", sigptr = zalloc((SIGCOUNT+4) * sizeof(char *)));
    for (t = sigs; (*sigptr++ = ztrdup(*t++)); );

    noerrs = 0;
}

/* assign various functions used for non-special parameters */

/**/
static void
assigngetset(Param pm)
{
    switch (PM_TYPE(pm->flags)) {
    case PM_SCALAR:
	pm->sets.cfn = strsetfn;
	pm->gets.cfn = strgetfn;
	break;
    case PM_INTEGER:
	pm->sets.ifn = intsetfn;
	pm->gets.ifn = intgetfn;
	break;
    case PM_EFLOAT:
    case PM_FFLOAT:
	pm->sets.ffn = floatsetfn;
	pm->gets.ffn = floatgetfn;
	break;
    case PM_ARRAY:
	pm->sets.afn = arrsetfn;
	pm->gets.afn = arrgetfn;
	break;
    case PM_HASHED:
	pm->sets.hfn = hashsetfn;
	pm->gets.hfn = hashgetfn;
	break;
    default:
	DPUTS(1, "BUG: tried to create param node without valid flag");
	break;
    }
    pm->unsetfn = stdunsetfn;
}

/* Create a parameter, so that it can be assigned to.  Returns NULL if the *
 * parameter already exists or can't be created, otherwise returns the     *
 * parameter node.  If a parameter of the same name exists in an outer     *
 * scope, it is hidden by a newly created parameter.  An already existing  *
 * parameter node at the current level may be `created' and returned       *
 * provided it is unset and not special.  If the parameter can't be        *
 * created because it already exists, the PM_UNSET flag is cleared.        */

/**/
mod_export Param
createparam(char *name, int flags)
{
    Param pm, oldpm;

    if (paramtab != realparamtab)
	flags = (flags & ~PM_EXPORTED) | PM_HASHELEM;

    if (name != nulstring) {
	oldpm = (Param) (paramtab == realparamtab ?
			 gethashnode2(paramtab, name) :
			 paramtab->getnode(paramtab, name));

	DPUTS(oldpm && oldpm->level > locallevel,
	      "BUG: old local parameter not deleted");
	if (oldpm && (oldpm->level == locallevel || !(flags & PM_LOCAL))) {
	    if (!(oldpm->flags & PM_UNSET) || (oldpm->flags & PM_SPECIAL)) {
		oldpm->flags &= ~PM_UNSET;
		if ((oldpm->flags & PM_SPECIAL) && oldpm->ename) {
		    Param altpm = 
			(Param) paramtab->getnode(paramtab, oldpm->ename);
		    if (altpm)
			altpm->flags &= ~PM_UNSET;
		}
		return NULL;
	    }
	    if ((oldpm->flags & PM_RESTRICTED) && isset(RESTRICTED)) {
		zerr("%s: restricted", name, 0);
		return NULL;
	    }

	    pm = oldpm;
	    pm->ct = 0;
	    oldpm = pm->old;
	} else {
	    pm = (Param) zcalloc(sizeof *pm);
	    if ((pm->old = oldpm)) {
		/*
		 * needed to avoid freeing oldpm, but we do take it
		 * out of the environment when it's hidden.
		 */
		if (oldpm->env) {
		    delenv(oldpm->env);
		    oldpm->env = NULL;
		}
		paramtab->removenode(paramtab, name);
	    }
	    paramtab->addnode(paramtab, ztrdup(name), pm);
	}

	if (isset(ALLEXPORT) && !(flags & PM_HASHELEM))
	    flags |= PM_EXPORTED;
    } else {
	pm = (Param) hcalloc(sizeof *pm);
	pm->nam = nulstring;
    }
    pm->flags = flags & ~PM_LOCAL;

    if(!(pm->flags & PM_SPECIAL))
	assigngetset(pm);
    return pm;
}

/* Copy a parameter */

/**/
void
copyparam(Param tpm, Param pm, int toplevel)
{
    /*
     * Note that tpm, into which we're copying, may not be in permanent
     * storage.  However, the values themselves are later used directly
     * to set the parameter, so must be permanently allocated (in accordance
     * with sets.?fn() usage).
     */
    tpm->flags = pm->flags;
    tpm->ct = pm->ct;
    if (!toplevel)
	tpm->flags &= ~PM_SPECIAL;
    switch (PM_TYPE(pm->flags)) {
    case PM_SCALAR:
	tpm->u.str = ztrdup(pm->gets.cfn(pm));
	break;
    case PM_INTEGER:
	tpm->u.val = pm->gets.ifn(pm);
	break;
    case PM_EFLOAT:
    case PM_FFLOAT:
	tpm->u.dval = pm->gets.ffn(pm);
	break;
    case PM_ARRAY:
	tpm->u.arr = zarrdup(pm->gets.afn(pm));
	break;
    case PM_HASHED:
	tpm->u.hash = copyparamtable(pm->gets.hfn(pm), pm->nam);
	break;
    }
    /*
     * If called from inside an associative array, that array is later going
     * to be passed as a real parameter, so we need the gets and sets
     * functions to be useful.  However, the saved associated array is
     * not itself special, so we just use the standard ones.
     * This is also why we switch off PM_SPECIAL.
     */
    if (!toplevel)
	assigngetset(tpm);
}

/* Return 1 if the string s is a valid identifier, else return 0. */

/**/
int
isident(char *s)
{
    char *ss;
    int ne;

    ne = noeval;		/* save the current value of noeval     */
    if (!*s)			/* empty string is definitely not valid */
	return 0;

    if (idigit(*s)) {
	/* If the first character is `s' is a digit, then all must be */
	for (ss = ++s; *ss; ss++)
	    if (!idigit(*ss))
		break;
    } else {
	/* Find the first character in `s' not in the iident type table */
	for (ss = s; *ss; ss++)
	    if (!iident(*ss))
		break;
    }

    /* If the next character is not [, then it is *
     * definitely not a valid identifier.         */
    if (!*ss)
	return 1;
    if (*ss != '[')
	return 0;

    /* Require balanced [ ] pairs with something between */
    if (!(ss = parse_subscript(++ss, 1)))
	return 0;
    untokenize(s);
    return !ss[1];
}

/**/
static zlong
getarg(char **str, int *inv, Value v, int a2, zlong *w)
{
    int hasbeg = 0, word = 0, rev = 0, ind = 0, down = 0, l, i, ishash;
    int keymatch = 0, needtok = 0;
    char *s = *str, *sep = NULL, *t, sav, *d, **ta, **p, *tt, c;
    zlong num = 1, beg = 0, r = 0;
    Patprog pprog = NULL;

    ishash = (v->pm && PM_TYPE(v->pm->flags) == PM_HASHED);

    /* first parse any subscription flags */
    if (v->pm && (*s == '(' || *s == Inpar)) {
	int escapes = 0;
	int waste;
	for (s++; *s != ')' && *s != Outpar && s != *str; s++) {
	    switch (*s) {
	    case 'r':
		rev = 1;
		keymatch = down = ind = 0;
		break;
	    case 'R':
		rev = down = 1;
		keymatch = ind = 0;
		break;
	    case 'k':
		keymatch = ishash;
		rev = 1;
		down = ind = 0;
		break;
	    case 'K':
		keymatch = ishash;
		rev = down = 1;
		ind = 0;
		break;
	    case 'i':
		rev = ind = 1;
		down = keymatch = 0;
		break;
	    case 'I':
		rev = ind = down = 1;
		keymatch = 0;
		break;
	    case 'w':
		/* If the parameter is a scalar, then make subscription *
		 * work on a per-word basis instead of characters.      */
		word = 1;
		break;
	    case 'f':
		word = 1;
		sep = "\n";
		break;
	    case 'e':
		/* Compatibility flag with no effect except to prevent *
		 * special interpretation by getindex() of `*' or `@'. */
		break;
	    case 'n':
		t = get_strarg(++s);
		if (!*t)
		    goto flagerr;
		sav = *t;
		*t = '\0';
		num = mathevalarg(s + 1, &d);
		if (!num)
		    num = 1;
		*t = sav;
		s = t;
		break;
	    case 'b':
		hasbeg = 1;
		t = get_strarg(++s);
		if (!*t)
		    goto flagerr;
		sav = *t;
		*t = '\0';
		if ((beg = mathevalarg(s + 1, &d)) > 0)
		    beg--;
		*t = sav;
		s = t;
		break;
	    case 'p':
		escapes = 1;
		break;
	    case 's':
		/* This gives the string that separates words *
		 * (for use with the `w' flag).               */
		t = get_strarg(++s);
		if (!*t)
		    goto flagerr;
		sav = *t;
		*t = '\0';
		sep = escapes ? getkeystring(s + 1, &waste, 1, &waste) :
				dupstring(s + 1);
		*t = sav;
		s = t;
		break;
	    default:
	      flagerr:
		num = 1;
		word = rev = ind = down = keymatch = 0;
		sep = NULL;
		s = *str - 1;
	    }
	}
	if (s != *str)
	    s++;
    }
    if (num < 0) {
	down = !down;
	num = -num;
    }
    if (v->isarr & SCANPM_WANTKEYS)
	*inv = (ind || !(v->isarr & SCANPM_WANTVALS));
    else if (v->isarr & SCANPM_WANTVALS)
	*inv = 0;
    else {
	if (v->isarr) {
	    if (ind) {
		v->isarr |= SCANPM_WANTKEYS;
		v->isarr &= ~SCANPM_WANTVALS;
	    } else if (rev)
		v->isarr |= SCANPM_WANTVALS;
	    if (!down && !keymatch && ishash)
		v->isarr &= ~SCANPM_MATCHMANY;
	}
	*inv = ind;
    }

    for (t = s, i = 0;
	 (c = *t) && ((c != Outbrack &&
		       (ishash || c != ',')) || i); t++) {
	/* Untokenize INULL() except before brackets and double-quotes */
	if (INULL(c)) {
	    c = t[1];
	    if (c == '[' || c == ']' ||
		c == '(' || c == ')' ||
		c == '{' || c == '}') {
		/* This test handles nested subscripts in hash keys */
		if (ishash && i)
		    *t = ztokens[*t - Pound];
		needtok = 1;
		++t;
	    } else if (c != '"')
		*t = ztokens[*t - Pound];
	    continue;
	}
	/* Inbrack and Outbrack are probably never found here ... */
	if (c == '[' || c == Inbrack)
	    i++;
	else if (c == ']' || c == Outbrack)
	    i--;
	if (ispecial(c))
	    needtok = 1;
    }
    if (!c)
	return 0;
    s = dupstrpfx(s, t - s);
    *str = tt = t;
    /* If we're NOT reverse subscripting, strip the INULL()s so brackets *
     * are not backslashed after parsestr().  Otherwise leave them alone *
     * so that the brackets will be escaped when we patcompile() or when *
     * subscript arithmetic is performed (for nested subscripts).        */
    if (ishash && (keymatch || !rev))
	remnulargs(s);
    if (needtok) {
	if (parsestr(s))
	    return 0;
	singsub(&s);
    } else if (rev)
	remnulargs(s);	/* This is probably always a no-op, but ... */
    if (!rev) {
	if (ishash) {
	    HashTable ht = v->pm->gets.hfn(v->pm);
	    if (!ht) {
		ht = newparamtable(17, v->pm->nam);
		v->pm->sets.hfn(v->pm, ht);
	    }
	    untokenize(s);
	    if (!(v->pm = (Param) ht->getnode(ht, s))) {
		HashTable tht = paramtab;
		paramtab = ht;
		v->pm = createparam(s, PM_SCALAR|PM_UNSET);
		paramtab = tht;
	    }
	    v->isarr = (*inv ? SCANPM_WANTINDEX : 0);
	    v->start = 0;
	    *inv = 0;	/* We've already obtained the "index" (key) */
	    *w = v->end = -1;
	    r = isset(KSHARRAYS) ? 1 : 0;
	} else {
	    r = mathevalarg(s, &s);
	    if (isset(KSHARRAYS) && r >= 0)
		r++;
	}
	if (word && !v->isarr) {
	    s = t = getstrvalue(v);
	    i = wordcount(s, sep, 0);
	    if (r < 0)
		r += i + 1;
	    if (r < 1)
		r = 1;
	    if (r > i)
		r = i;
	    if (!s || !*s)
		return 0;
	    while ((d = findword(&s, sep)) && --r);
	    if (!d)
		return 0;

	    if (!a2 && *tt != ',')
		*w = (zlong)(s - t);

	    return (a2 ? s : d + 1) - t;
	} else if (!v->isarr && !word) {
	    s = getstrvalue(v);
	    if (r > 0) {
		for (t = s + r - 1; *s && s < t;)
		    if (*s++ == Meta)
			s++, t++, r++;
	    } else {
		r += ztrlen(s);
		for (t = s + r; *s && s < t; r--)
		    if (*s++ == Meta)
			t++, r++;
		r -= strlen(s);
	    }
	}
    } else {
	if (!v->isarr && !word) {
	    l = strlen(s);
	    if (a2) {
		if (!l || *s != '*') {
		    d = (char *) hcalloc(l + 2);
		    *d = '*';
		    strcpy(d + 1, s);
		    s = d;
		}
	    } else {
		if (!l || s[l - 1] != '*' || (l > 1 && s[l - 2] == '\\')) {
		    d = (char *) hcalloc(l + 2);
		    strcpy(d, s);
		    strcat(d, "*");
		    s = d;
		}
	    }
	}
	if (!keymatch) {
	    tokenize(s);
	    remnulargs(s);
	}

	if (keymatch || (pprog = patcompile(s, 0, NULL))) {
	    int len;

	    if (v->isarr) {
		if (ishash) {
		    scanprog = pprog;
		    scanstr = s;
		    if (keymatch)
			v->isarr |= SCANPM_KEYMATCH;
		    else if (ind)
			v->isarr |= SCANPM_MATCHKEY;
		    else
			v->isarr |= SCANPM_MATCHVAL;
		    if (down)
			v->isarr |= SCANPM_MATCHMANY;
		    if ((ta = getvaluearr(v)) &&
			(*ta || ((v->isarr & SCANPM_MATCHMANY) &&
				 (v->isarr & (SCANPM_MATCHKEY | SCANPM_MATCHVAL |
					      SCANPM_KEYMATCH))))) {
			*inv = v->inv;
			*w = v->end;
			return 1;
		    }
		} else
		    ta = getarrvalue(v);
		if (!ta || !*ta)
		    return 0;
		len = arrlen(ta);
		if (beg < 0)
		    beg += len;
		if (beg >= 0 && beg < len) {
		    if (down) {
			if (!hasbeg)
			    beg = len - 1;
			for (r = 1 + beg, p = ta + beg; p >= ta; r--, p--) {
			    if (pattry(pprog, *p) && !--num)
				return r;
			}
		    } else
			for (r = 1 + beg, p = ta + beg; *p; r++, p++)
			    if (pattry(pprog, *p) && !--num)
				return r;
		}
	    } else if (word) {
		ta = sepsplit(d = s = getstrvalue(v), sep, 1, 1);
		len = arrlen(ta);
		if (beg < 0)
		    beg += len;
		if (beg >= 0 && beg < len) {
		    if (down) {
			if (!hasbeg)
			    beg = len - 1;
			for (r = 1 + beg, p = ta + beg; p >= ta; p--, r--)
			    if (pattry(pprog, *p) && !--num)
				break;
			if (p < ta)
			    return 0;
		    } else {
			for (r = 1 + beg, p = ta + beg; *p; r++, p++)
			    if (pattry(pprog, *p) && !--num)
				break;
			if (!*p)
			    return 0;
		    }
		}
		if (a2)
		    r++;
		for (i = 0; (t = findword(&d, sep)) && *t; i++)
		    if (!--r) {
			r = (zlong)(t - s + (a2 ? -1 : 1));
			if (!a2 && *tt != ',')
			    *w = r + strlen(ta[i]) - 1;
			return r;
		    }
		return a2 ? -1 : 0;
	    } else {
		d = getstrvalue(v);
		if (!d || !*d)
		    return 0;
		len = strlen(d);
		if (beg < 0)
		    beg += len;
		if (beg >= 0 && beg < len) {
                    char *de = d + len;

		    if (a2) {
			if (down) {
			    if (!hasbeg)
				beg = len;
			    for (r = beg, t = d + beg; t >= d; r--, t--) {
				sav = *t;
				*t = '\0';
				if (pattry(pprog, d)
				    && !--num) {
				    *t = sav;
				    return r;
				}
				*t = sav;
			    }
			} else
			    for (r = beg, t = d + beg; t <= de; r++, t++) {
				sav = *t;
				*t = '\0';
				if (pattry(pprog, d) &&
				    !--num) {
				    *t = sav;
				    return r;
				}
				*t = sav;
			    }
		    } else {
			if (down) {
			    if (!hasbeg)
				beg = len;
			    for (r = beg + 1, t = d + beg; t >= d; r--, t--) {
				if (pattry(pprog, t) &&
				    !--num)
				    return r;
			    }
			} else
			    for (r = beg + 1, t = d + beg; t <= de; r++, t++)
				if (pattry(pprog, t) &&
				    !--num)
				    return r;
		    }
		}
		return down ? 0 : len + 1;
	    }
	}
    }
    return r;
}

/**/
int
getindex(char **pptr, Value v, int dq)
{
    int start, end, inv = 0;
    char *s = *pptr, *tbrack;

    *s++ = '[';
    s = parse_subscript(s, dq);	/* Error handled after untokenizing */
    /* Now we untokenize everything except INULL() markers so we can check *
     * for the '*' and '@' special subscripts.  The INULL()s are removed  *
     * in getarg() after we know whether we're doing reverse indexing.    */
    for (tbrack = *pptr + 1; *tbrack && tbrack != s; tbrack++) {
	if (INULL(*tbrack) && !*++tbrack)
	    break;
	if (itok(*tbrack))	/* Need to check for Nularg here? */
	    *tbrack = ztokens[*tbrack - Pound];
    }
    /* If we reached the end of the string (s == NULL) we have an error */
    if (*tbrack)
	*tbrack = Outbrack;
    else {
	zerr("invalid subscript", NULL, 0);
	*pptr = tbrack;
	return 1;
    }
    s = *pptr + 1;
    if ((s[0] == '*' || s[0] == '@') && s + 1 == tbrack) {
	if ((v->isarr || IS_UNSET_VALUE(v)) && s[0] == '@')
	    v->isarr |= SCANPM_ISVAR_AT;
	v->start = 0;
	v->end = -1;
	s += 2;
    } else {
	zlong we = 0, dummy;

	start = getarg(&s, &inv, v, 0, &we);

	if (inv) {
	    if (!v->isarr && start != 0) {
		char *t, *p;
		t = getstrvalue(v);
		if (start > 0) {
		    for (p = t + start - 1; p-- > t; )
			if (*p == Meta)
			    start--;
		} else
		    start = -ztrlen(t + start + strlen(t));
	    }
	    if (start > 0 && (isset(KSHARRAYS) || (v->pm->flags & PM_HASHED)))
		start--;
	    if (v->isarr != SCANPM_WANTINDEX) {
		v->inv = 1;
		v->isarr = 0;
		v->start = start;
		v->end = start + 1;
	    }
	    if (*s == ',') {
		zerr("invalid subscript", NULL, 0);
		*tbrack = ']';
		*pptr = tbrack+1;
		return 1;
	    }
	    if (s == tbrack)
		s++;
	} else {
	    int com;

	    if ((com = (*s == ','))) {
		s++;
		end = getarg(&s, &inv, v, 1, &dummy);
	    } else {
		end = we ? we : start;
	    }
	    if (start > 0)
		start--;
	    else if (start == 0 && end == 0)
		end++;
	    if (s == tbrack) {
		s++;
		if (v->isarr && start == end-1 && !com &&
		    (!(v->isarr & SCANPM_MATCHMANY) ||
		     !(v->isarr & (SCANPM_MATCHKEY | SCANPM_MATCHVAL |
				   SCANPM_KEYMATCH))))
		    v->isarr = 0;
		v->start = start;
		v->end = end;
	    } else
		s = *pptr;
	}
    }
    *tbrack = ']';
    *pptr = s;
    return 0;
}


/**/
mod_export Value
getvalue(Value v, char **pptr, int bracks)
{
  return fetchvalue(v, pptr, bracks, 0);
}

/**/
mod_export Value
fetchvalue(Value v, char **pptr, int bracks, int flags)
{
    char *s, *t;
    char sav, c;
    int ppar = 0;

    s = t = *pptr;

    if (idigit(c = *s)) {
	if (bracks >= 0)
	    ppar = zstrtol(s, &s, 10);
	else
	    ppar = *s++ - '0';
    }
    else if (iident(c))
	while (iident(*s))
	    s++;
    else if (c == Quest)
	*s++ = '?';
    else if (c == Pound)
	*s++ = '#';
    else if (c == String)
	*s++ = '$';
    else if (c == Qstring)
	*s++ = '$';
    else if (c == Star)
	*s++ = '*';
    else if (c == '#' || c == '-' || c == '?' || c == '$' ||
	     c == '!' || c == '@' || c == '*')
	s++;
    else
	return NULL;

    if ((sav = *s))
	*s = '\0';
    if (ppar) {
	if (v)
	    memset(v, 0, sizeof(*v));
	else
	    v = (Value) hcalloc(sizeof *v);
	v->pm = argvparam;
	v->inv = 0;
	v->start = ppar - 1;
	v->end = ppar;
	if (sav)
	    *s = sav;
    } else {
	Param pm;
	int isvarat;

        isvarat = (t[0] == '@' && !t[1]);
	pm = (Param) paramtab->getnode(paramtab, *t == '0' ? "0" : t);
	if (sav)
	    *s = sav;
	*pptr = s;
	if (!pm || (pm->flags & PM_UNSET))
	    return NULL;
	if (v)
	    memset(v, 0, sizeof(*v));
	else
	    v = (Value) hcalloc(sizeof *v);
	if (PM_TYPE(pm->flags) & (PM_ARRAY|PM_HASHED)) {
	    /* Overload v->isarr as the flag bits for hashed arrays. */
	    v->isarr = flags | (isvarat ? SCANPM_ISVAR_AT : 0);
	    /* If no flags were passed, we need something to represent *
	     * `true' yet differ from an explicit WANTVALS.  This is a *
	     * bit of a hack, but makes some sense:  When no subscript *
	     * is provided, all values are substituted.                */
	    if (!v->isarr)
		v->isarr = SCANPM_MATCHMANY;
	}
	v->pm = pm;
	v->inv = 0;
	v->start = 0;
	v->end = -1;
	if (bracks > 0 && (*s == '[' || *s == Inbrack)) {
	    if (getindex(&s, v, (flags & SCANPM_DQUOTED))) {
		*pptr = s;
		return v;
	    }
	} else if (!(flags & SCANPM_ASSIGNING) && v->isarr &&
		   iident(*t) && isset(KSHARRAYS))
	    v->end = 1, v->isarr = 0;
    }
    if (!bracks && *s)
	return NULL;
    *pptr = s;
    if (v->start > MAX_ARRLEN) {
	zerr("subscript too %s: %d", "big", v->start + !isset(KSHARRAYS));
	return NULL;
    }
    if (v->start < -MAX_ARRLEN) {
	zerr("subscript too %s: %d", "small", v->start);
	return NULL;
    }
    if (v->end > MAX_ARRLEN+1) {
	zerr("subscript too %s: %d", "big", v->end - !!isset(KSHARRAYS));
	return NULL;
    }
    if (v->end < -MAX_ARRLEN) {
	zerr("subscript too %s: %d", "small", v->end);
	return NULL;
    }
    return v;
}

/**/
mod_export char *
getstrvalue(Value v)
{
    char *s, **ss;
    char buf[BDIGBUFSIZE];

    if (!v)
	return hcalloc(1);

    if (v->inv && !(v->pm->flags & PM_HASHED)) {
	sprintf(buf, "%d", v->start);
	s = dupstring(buf);
	return s;
    }

    switch(PM_TYPE(v->pm->flags)) {
    case PM_HASHED:
	/* (!v->isarr) should be impossible unless emulating ksh */
	if (!v->isarr && emulation == EMULATE_KSH) {
	    s = dupstring("[0]");
	    if (getindex(&s, v, 0) == 0)
		s = getstrvalue(v);
	    return s;
	} /* else fall through */
    case PM_ARRAY:
	ss = getvaluearr(v);
	if (v->isarr)
	    s = sepjoin(ss, NULL, 1);
	else {
	    if (v->start < 0)
		v->start += arrlen(ss);
	    s = (v->start >= arrlen(ss) || v->start < 0) ?
		(char *) hcalloc(1) : ss[v->start];
	}
	return s;
    case PM_INTEGER:
	convbase(buf, v->pm->gets.ifn(v->pm), v->pm->ct);
	s = dupstring(buf);
	break;
    case PM_EFLOAT:
    case PM_FFLOAT:
	s = convfloat(v->pm->gets.ffn(v->pm), v->pm->ct, v->pm->flags, NULL);
	break;
    case PM_SCALAR:
	s = v->pm->gets.cfn(v->pm);
	break;
    default:
	s = NULL;
	DPUTS(1, "BUG: param node without valid type");
	break;
    }

    if (v->start == 0 && v->end == -1)
	return s;

    if (v->start < 0)
	v->start += strlen(s);
    if (v->end < 0)
	v->end += strlen(s) + 1;
    s = (v->start > (int)strlen(s)) ? dupstring("") : dupstring(s + v->start);
    if (v->end <= v->start)
	s[0] = '\0';
    else if (v->end - v->start <= (int)strlen(s))
	s[v->end - v->start + (s[v->end - v->start - 1] == Meta)] = '\0';

    return s;
}

static char *nular[] = {"", NULL};

/**/
mod_export char **
getarrvalue(Value v)
{
    char **s;

    if (!v)
	return arrdup(nular);
    else if (IS_UNSET_VALUE(v))
	return arrdup(&nular[1]);
    if (v->inv) {
	char buf[DIGBUFSIZE];

	s = arrdup(nular);
	sprintf(buf, "%d", v->start);
	s[0] = dupstring(buf);
	return s;
    }
    s = getvaluearr(v);
    if (v->start == 0 && v->end == -1)
	return s;
    if (v->start < 0)
	v->start += arrlen(s);
    if (v->end < 0)
	v->end += arrlen(s) + 1;
    if (v->start > arrlen(s) || v->start < 0)
	s = arrdup(nular);
    else
	s = arrdup(s + v->start);
    if (v->end <= v->start)
	s[0] = NULL;
    else if (v->end - v->start <= arrlen(s))
	s[v->end - v->start] = NULL;
    return s;
}

/**/
mod_export zlong
getintvalue(Value v)
{
    if (!v || v->isarr)
	return 0;
    if (v->inv)
	return v->start;
    if (PM_TYPE(v->pm->flags) == PM_INTEGER)
	return v->pm->gets.ifn(v->pm);
    if (v->pm->flags & (PM_EFLOAT|PM_FFLOAT))
	return (zlong)v->pm->gets.ffn(v->pm);
    return mathevali(getstrvalue(v));
}

/**/
mnumber
getnumvalue(Value v)
{
    mnumber mn;
    mn.type = MN_INTEGER;

    if (!v || v->isarr) {
	mn.u.l = 0;
    } else if (v->inv) {
	mn.u.l = v->start;
    } else if (PM_TYPE(v->pm->flags) == PM_INTEGER) {
	mn.u.l = v->pm->gets.ifn(v->pm);
    } else if (v->pm->flags & (PM_EFLOAT|PM_FFLOAT)) {
	mn.type = MN_FLOAT;
	mn.u.d = v->pm->gets.ffn(v->pm);
    } else
	return matheval(getstrvalue(v));
    return mn;
}

/**/
void
export_param(Param pm)
{
    char buf[BDIGBUFSIZE], *val;

    if (PM_TYPE(pm->flags) & (PM_ARRAY|PM_HASHED)) {
#if 0	/* Requires changes elsewhere in params.c and builtin.c */
	if (emulation == EMULATE_KSH /* isset(KSHARRAYS) */) {
	    struct value v;
	    v.isarr = 1;
	    v.inv = 0;
	    v.start = 0;
	    v.end = -1;
	    val = getstrvalue(&v);
	} else
#endif
	    return;
    } else if (PM_TYPE(pm->flags) == PM_INTEGER)
	convbase(val = buf, pm->gets.ifn(pm), pm->ct);
    else if (pm->flags & (PM_EFLOAT|PM_FFLOAT))
	val = convfloat(pm->gets.ffn(pm), pm->ct,
			pm->flags, NULL);
    else
	val = pm->gets.cfn(pm);

    pm->flags |= PM_EXPORTED;
    pm->env = addenv(pm->nam, val, pm->flags);
}

/**/
mod_export void
setstrvalue(Value v, char *val)
{
    if (v->pm->flags & PM_READONLY) {
	zerr("read-only variable: %s", v->pm->nam, 0);
	zsfree(val);
	return;
    }
    if ((v->pm->flags & PM_RESTRICTED) && isset(RESTRICTED)) {
	zerr("%s: restricted", v->pm->nam, 0);
	zsfree(val);
	return;
    }
    if (v->pm->flags & PM_HASHED) {
	zerr("%s: attempt to set slice of associative array", v->pm->nam, 0);
	return;
    }
    v->pm->flags &= ~PM_UNSET;
    switch (PM_TYPE(v->pm->flags)) {
    case PM_SCALAR:
	if (v->start == 0 && v->end == -1) {
	    (v->pm->sets.cfn) (v->pm, val);
	    if (v->pm->flags & (PM_LEFT | PM_RIGHT_B | PM_RIGHT_Z) && !v->pm->ct)
		v->pm->ct = strlen(val);
	} else {
	    char *z, *x;
	    int zlen;

	    z = dupstring((v->pm->gets.cfn) (v->pm));
	    zlen = strlen(z);
	    if (v->inv && unset(KSHARRAYS))
		v->start--, v->end--;
	    if (v->start < 0) {
		v->start += zlen;
		if (v->start < 0)
		    v->start = 0;
	    }
	    if (v->start > zlen)
		v->start = zlen;
	    if (v->end < 0)
		v->end += zlen + 1;
	    else if (v->end > zlen)
		v->end = zlen;
	    x = (char *) zalloc(v->start + strlen(val) + zlen - v->end + 1);
	    strncpy(x, z, v->start);
	    strcpy(x + v->start, val);
	    strcat(x + v->start, z + v->end);
	    (v->pm->sets.cfn) (v->pm, x);
	    zsfree(val);
	}
	break;
    case PM_INTEGER:
	if (val) {
	    (v->pm->sets.ifn) (v->pm, mathevali(val));
	    zsfree(val);
	}
	if (!v->pm->ct && lastbase != -1)
	    v->pm->ct = lastbase;
	break;
    case PM_EFLOAT:
    case PM_FFLOAT:
	if (val) {
	    mnumber mn = matheval(val);
	    (v->pm->sets.ffn) (v->pm, (mn.type & MN_FLOAT) ? mn.u.d :
			       (double)mn.u.l);
	    zsfree(val);
	}
	break;
    case PM_ARRAY:
	{
	    char **ss = (char **) zalloc(2 * sizeof(char *));

	    ss[0] = val;
	    ss[1] = NULL;
	    setarrvalue(v, ss);
	}
	break;
    }
    if ((!v->pm->env && !(v->pm->flags & PM_EXPORTED) &&
	 !(isset(ALLEXPORT) && !(v->pm->flags & PM_HASHELEM))) ||
	(v->pm->flags & PM_ARRAY) || v->pm->ename)
	return;
    export_param(v->pm);
}

/**/
void
setnumvalue(Value v, mnumber val)
{
    char buf[BDIGBUFSIZE], *p;

    if (v->pm->flags & PM_READONLY) {
	zerr("read-only variable: %s", v->pm->nam, 0);
	return;
    }
    if ((v->pm->flags & PM_RESTRICTED) && isset(RESTRICTED)) {
	zerr("%s: restricted", v->pm->nam, 0);
	return;
    }
    switch (PM_TYPE(v->pm->flags)) {
    case PM_SCALAR:
    case PM_ARRAY:
	if ((val.type & MN_INTEGER) || outputradix) {
	    if (!(val.type & MN_INTEGER))
		val.u.l = (zlong) val.u.d;
	    convbase(p = buf, val.u.l, outputradix);
	} else
	    p = convfloat(val.u.d, 0, 0, NULL);
	setstrvalue(v, ztrdup(p));
	break;
    case PM_INTEGER:
	(v->pm->sets.ifn) (v->pm, (val.type & MN_INTEGER) ? val.u.l :
			   (zlong) val.u.d);
	setstrvalue(v, NULL);
	break;
    case PM_EFLOAT:
    case PM_FFLOAT:
	(v->pm->sets.ffn) (v->pm, (val.type & MN_INTEGER) ?
			   (double)val.u.l : val.u.d);
	setstrvalue(v, NULL);
	break;
    }
}

/**/
mod_export void
setarrvalue(Value v, char **val)
{
    if (v->pm->flags & PM_READONLY) {
	zerr("read-only variable: %s", v->pm->nam, 0);
	freearray(val);
	return;
    }
    if ((v->pm->flags & PM_RESTRICTED) && isset(RESTRICTED)) {
	zerr("%s: restricted", v->pm->nam, 0);
	freearray(val);
	return;
    }
    if (!(PM_TYPE(v->pm->flags) & (PM_ARRAY|PM_HASHED))) {
	freearray(val);
	zerr("%s: attempt to assign array value to non-array",
	     v->pm->nam, 0);
	return;
    }
    if (v->start == 0 && v->end == -1) {
	if (PM_TYPE(v->pm->flags) == PM_HASHED)
	    arrhashsetfn(v->pm, val, 0);
	else
	    (v->pm->sets.afn) (v->pm, val);
    } else if (v->start == -1 && v->end == 0 &&
    	    PM_TYPE(v->pm->flags) == PM_HASHED) {
    	arrhashsetfn(v->pm, val, 1);
    } else {
	char **old, **new, **p, **q, **r;
	int n, ll, i;

	if ((PM_TYPE(v->pm->flags) == PM_HASHED)) {
	    freearray(val);
	    zerr("%s: attempt to set slice of associative array",
		 v->pm->nam, 0);
	    return;
	}
	if (v->inv && unset(KSHARRAYS)) {
	    if (v->start > 0)
		v->start--;
	    v->end--;
	}
	if (v->end < v->start)
	    v->end = v->start;
	q = old = v->pm->gets.afn(v->pm);
	n = arrlen(old);
	if (v->start < 0) {
	    v->start += n;
	    if (v->start < 0)
		v->start = 0;
	}
	if (v->end < 0) {
	    v->end += n + 1;
	    if (v->end < 0)
		v->end = 0;
	}

	ll = v->start + arrlen(val);
	if (v->end <= n)
	    ll += n - v->end + 1;

	p = new = (char **) zcalloc(sizeof(char *) * (ll + 1));

	for (i = 0; i < v->start; i++)
	    *p++ = i < n ? ztrdup(*q++) : ztrdup("");
	for (r = val; *r;)
	    *p++ = ztrdup(*r++);
	if (v->end < n)
	    for (q = old + v->end; *q;)
		*p++ = ztrdup(*q++);
	*p = NULL;

	(v->pm->sets.afn) (v->pm, new);
	freearray(val);
    }
}

/* Retrieve an integer parameter */

/**/
mod_export zlong
getiparam(char *s)
{
    struct value vbuf;
    Value v;

    if (!(v = getvalue(&vbuf, &s, 1)))
	return 0;
    return getintvalue(v);
}

/* Retrieve a numerical parameter, either integer or floating */

/**/
mnumber
getnparam(char *s)
{
    struct value vbuf;
    Value v;

    if (!(v = getvalue(&vbuf, &s, 1))) {
	mnumber mn;
	mn.type = MN_INTEGER;
	mn.u.l = 0;
	return mn;
    }
    return getnumvalue(v);
}

/* Retrieve a scalar (string) parameter */

/**/
mod_export char *
getsparam(char *s)
{
    struct value vbuf;
    Value v;

    if (!(v = getvalue(&vbuf, &s, 0)))
	return NULL;
    return getstrvalue(v);
}

/* Retrieve an array parameter */

/**/
mod_export char **
getaparam(char *s)
{
    struct value vbuf;
    Value v;

    if (!idigit(*s) && (v = getvalue(&vbuf, &s, 0)) &&
	PM_TYPE(v->pm->flags) == PM_ARRAY)
	return v->pm->gets.afn(v->pm);
    return NULL;
}

/* Retrieve an assoc array parameter as an array */

/**/
mod_export char **
gethparam(char *s)
{
    struct value vbuf;
    Value v;

    if (!idigit(*s) && (v = getvalue(&vbuf, &s, 0)) &&
	PM_TYPE(v->pm->flags) == PM_HASHED)
	return paramvalarr(v->pm->gets.hfn(v->pm), SCANPM_WANTVALS);
    return NULL;
}

/* Retrieve the keys of an assoc array parameter as an array */

/**/
mod_export char **
gethkparam(char *s)
{
    struct value vbuf;
    Value v;

    if (!idigit(*s) && (v = getvalue(&vbuf, &s, 0)) &&
	PM_TYPE(v->pm->flags) == PM_HASHED)
	return paramvalarr(v->pm->gets.hfn(v->pm), SCANPM_WANTKEYS);
    return NULL;
}

/**/
mod_export Param
assignsparam(char *s, char *val, int augment)
{
    struct value vbuf;
    Value v;
    char *t = s;
    char *ss, *copy, *var;
    size_t lvar;
    mnumber lhs, rhs;
    int sstart;

    if (!isident(s)) {
	zerr("not an identifier: %s", s, 0);
	zsfree(val);
	errflag = 1;
	return NULL;
    }
    queue_signals();
    if ((ss = strchr(s, '['))) {
	*ss = '\0';
	if (!(v = getvalue(&vbuf, &s, 1)))
	    createparam(t, PM_ARRAY);
	*ss = '[';
	v = NULL;
    } else {
	if (!(v = getvalue(&vbuf, &s, 1)))
	    createparam(t, PM_SCALAR);
	else if ((((v->pm->flags & PM_ARRAY) && !augment) ||
	    	 (v->pm->flags & PM_HASHED)) &&
		 !(v->pm->flags & (PM_SPECIAL|PM_TIED)) && 
		 unset(KSHARRAYS)) {
	    unsetparam(t);
	    createparam(t, PM_SCALAR);
	    v = NULL;
	}
    }
    if (!v && !(v = getvalue(&vbuf, &t, 1))) {
	unqueue_signals();
	zsfree(val);
	return NULL;
    }
    if (augment) {
	if (v->start == 0 && v->end == -1) {
	    switch (PM_TYPE(v->pm->flags)) {
	    case PM_SCALAR:
		v->start = INT_MAX;  /* just append to scalar value */
		break;
	    case PM_INTEGER:
	    case PM_EFLOAT:
	    case PM_FFLOAT:
		rhs = matheval(val);
		lhs = getnumvalue(v);
		if (lhs.type == MN_FLOAT) {
		    if ((rhs.type) == MN_FLOAT)
        		lhs.u.d = lhs.u.d + rhs.u.d;
		    else
			lhs.u.d = lhs.u.d + (double)rhs.u.l;
		} else {
        	    if ((rhs.type) == MN_INTEGER)
			lhs.u.l = lhs.u.l + rhs.u.l;
		    else
			lhs.u.l = lhs.u.l + (zlong)rhs.u.d;
		}
		setnumvalue(v, lhs);
    	    	unqueue_signals();
		zsfree(val);
		return v->pm; /* avoid later setstrvalue() call */
	    case PM_ARRAY:
	    	if (unset(KSHARRAYS)) {
		    v->start = arrlen(v->pm->gets.afn(v->pm));
		    v->end = v->start + 1;
		} else {
		    /* ksh appends scalar to first element */
		    v->end = 1;
		    goto kshappend;
		}
		break;
	    }
	} else {
	    switch (PM_TYPE(v->pm->flags)) {
	    case PM_SCALAR:
    		if (v->end > 0)
		    v->start = v->end;
		else
		    v->start = v->end = strlen(v->pm->gets.cfn(v->pm)) +
			v->end + 1;
	    	break;
	    case PM_INTEGER:
	    case PM_EFLOAT:
	    case PM_FFLOAT:
		unqueue_signals();
		zerr("attempt to add to slice of a numeric variable",
		    NULL, 0);
		zsfree(val);
		return NULL;
	    case PM_ARRAY:
	      kshappend:
		/* treat slice as the end element */
		v->start = sstart = v->end > 0 ? v->end - 1 : v->end;
		v->isarr = 0;
		var = getstrvalue(v);
		v->start = sstart;
		copy = val;
		lvar = strlen(var);
		val = (char *)zalloc(lvar + strlen(val) + 1);
		strcpy(val, var);
		strcpy(val + lvar, copy);
		zsfree(copy);
		break;
	    }
	}
    }
    
    setstrvalue(v, val);
    unqueue_signals();
    return v->pm;
}

/**/
mod_export Param
assignaparam(char *s, char **val, int augment)
{
    struct value vbuf;
    Value v;
    char *t = s;
    char *ss;

    if (!isident(s)) {
	zerr("not an identifier: %s", s, 0);
	freearray(val);
	errflag = 1;
	return NULL;
    }
    queue_signals();
    if ((ss = strchr(s, '['))) {
	*ss = '\0';
	if (!(v = getvalue(&vbuf, &s, 1)))
	    createparam(t, PM_ARRAY);
	*ss = '[';
	if (v && PM_TYPE(v->pm->flags) == PM_HASHED) {
	    unqueue_signals();
	    zerr("%s: attempt to set slice of associative array",
		 v->pm->nam, 0);
	    freearray(val);
	    errflag = 1;
	    return NULL;
	}
	v = NULL;
    } else {
	if (!(v = fetchvalue(&vbuf, &s, 1, SCANPM_ASSIGNING)))
	    createparam(t, PM_ARRAY);
	else if (!(PM_TYPE(v->pm->flags) & (PM_ARRAY|PM_HASHED)) &&
		 !(v->pm->flags & (PM_SPECIAL|PM_TIED))) {
	    int uniq = v->pm->flags & PM_UNIQUE;
	    if (augment) {
	    	/* insert old value at the beginning of the val array */
		char **new;
		int lv = arrlen(val);

		new = (char **) zalloc(sizeof(char *) * (lv + 2));
		*new = ztrdup(getstrvalue(v));
		memcpy(new+1, val, sizeof(char *) * (lv + 1));
		free(val);
		val = new;
		
	    }
	    unsetparam(t);
	    createparam(t, PM_ARRAY | uniq);
	    v = NULL;
	}
    }
    if (!v)
	if (!(v = fetchvalue(&vbuf, &t, 1, SCANPM_ASSIGNING))) {
	    unqueue_signals();
	    freearray(val);
	    return NULL;
	}

    if (augment) {
    	if (v->start == 0 && v->end == -1) {
	    if (PM_TYPE(v->pm->flags) & PM_ARRAY) {
	    	v->start = arrlen(v->pm->gets.afn(v->pm));
	    	v->end = v->start + 1;
	    } else if (PM_TYPE(v->pm->flags) & PM_HASHED)
	    	v->start = -1, v->end = 0;
	} else {
	    if (v->end > 0)
		v->start = v->end--;
	    else if (PM_TYPE(v->pm->flags) & PM_ARRAY) {
		v->end = arrlen(v->pm->gets.afn(v->pm)) + v->end;
		v->start = v->end + 1;
	    }
	}
    }

    setarrvalue(v, val);
    unqueue_signals();
    return v->pm;
}

/**/
mod_export Param
sethparam(char *s, char **val)
{
    struct value vbuf;
    Value v;
    char *t = s;

    if (!isident(s)) {
	zerr("not an identifier: %s", s, 0);
	freearray(val);
	errflag = 1;
	return NULL;
    }
    if (strchr(s, '[')) {
	freearray(val);
	zerr("nested associative arrays not yet supported", NULL, 0);
	errflag = 1;
	return NULL;
    }
    queue_signals();
    if (!(v = fetchvalue(&vbuf, &s, 1, SCANPM_ASSIGNING)))
	createparam(t, PM_HASHED);
    else if (!(PM_TYPE(v->pm->flags) & PM_HASHED) &&
	     !(v->pm->flags & PM_SPECIAL)) {
	unsetparam(t);
	createparam(t, PM_HASHED);
	v = NULL;
    }
    if (!v)
	if (!(v = fetchvalue(&vbuf, &t, 1, SCANPM_ASSIGNING))) {
	    unqueue_signals();
	    return NULL;
	}
    setarrvalue(v, val);
    unqueue_signals();
    return v->pm;
}

/**/
mod_export Param
setiparam(char *s, zlong val)
{
    struct value vbuf;
    Value v;
    char *t = s, *ss;
    Param pm;
    mnumber mnval;

    if (!isident(s)) {
	zerr("not an identifier: %s", s, 0);
	errflag = 1;
	return NULL;
    }
    queue_signals();
    if (!(v = getvalue(&vbuf, &s, 1))) {
	if ((ss = strchr(s, '[')))
	    *ss = '\0';
	if (!(pm = createparam(t, ss ? PM_ARRAY : PM_INTEGER)))
	    pm = (Param) paramtab->getnode(paramtab, t);
	DPUTS(!pm, "BUG: parameter not created");
	if (ss) {
	    *ss = '[';
	} else {
	    pm->ct = outputradix;
	}
	v = getvalue(&vbuf, &t, 1);
	DPUTS(!v, "BUG: value not found for new parameter");
    }
    mnval.type = MN_INTEGER;
    mnval.u.l = val;
    setnumvalue(v, mnval);
    unqueue_signals();
    return v->pm;
}

/*
 * Like setiparam(), but can take an mnumber which can be integer or
 * floating.
 */

/**/
Param
setnparam(char *s, mnumber val)
{
    struct value vbuf;
    Value v;
    char *t = s, *ss = NULL;
    Param pm;

    if (!isident(s)) {
	zerr("not an identifier: %s", s, 0);
	errflag = 1;
	return NULL;
    }
    queue_signals();
    if (!(v = getvalue(&vbuf, &s, 1))) {
	if ((ss = strchr(s, '[')))
	    *ss = '\0';
	pm = createparam(t, ss ? PM_ARRAY :
			 (val.type & MN_INTEGER) ? PM_INTEGER : PM_FFLOAT);
	if (!pm)
	    pm = (Param) paramtab->getnode(paramtab, t);
	DPUTS(!pm, "BUG: parameter not created");
	if (ss) {
	    *ss = '[';
	} else if (val.type & MN_INTEGER) {
	    pm->ct = outputradix;
	}
	v = getvalue(&vbuf, &t, 1);
	DPUTS(!v, "BUG: value not found for new parameter");
    }
    setnumvalue(v, val);
    unqueue_signals();
    return v->pm;
}

/* Unset a parameter */

/**/
mod_export void
unsetparam(char *s)
{
    Param pm;

    queue_signals();
    if ((pm = (Param) (paramtab == realparamtab ?
		       gethashnode2(paramtab, s) :
		       paramtab->getnode(paramtab, s))))
	unsetparam_pm(pm, 0, 1);
    unqueue_signals();
}

/* Unset a parameter */

/**/
mod_export int
unsetparam_pm(Param pm, int altflag, int exp)
{
    Param oldpm, altpm;

    if ((pm->flags & PM_READONLY) && pm->level <= locallevel) {
	zerr("read-only variable: %s", pm->nam, 0);
	return 1;
    }
    if ((pm->flags & PM_RESTRICTED) && isset(RESTRICTED)) {
	zerr("%s: restricted", pm->nam, 0);
	return 1;
    }
    pm->unsetfn(pm, exp);
    if ((pm->flags & PM_EXPORTED) && pm->env) {
	delenv(pm->env);
	pm->env = NULL;
    }

    /* remove it under its alternate name if necessary */
    if (pm->ename && !altflag) {
	altpm = (Param) paramtab->getnode(paramtab, pm->ename);
	/* tied parameters are at the same local level as each other */
	oldpm = NULL;
	while (altpm && altpm->level > pm->level) {
	    /* param under alternate name hidden by a local */
	    oldpm = altpm;
	    altpm = altpm->old;
	}
	if (altpm) {
	    if (oldpm && !altpm->level) {
		oldpm->old = NULL;
		/* fudge things so removenode isn't called */
		altpm->level = 1;
	    }
	    unsetparam_pm(altpm, 1, exp);
	}
    }

    /*
     * If this was a local variable, we need to keep the old
     * struct so that it is resurrected at the right level.
     * This is partly because when an array/scalar value is set
     * and the parameter used to be the other sort, unsetparam()
     * is called.  Beyond that, there is an ambiguity:  should
     * foo() { local bar; unset bar; } make the global bar
     * available or not?  The following makes the answer "no".
     *
     * Some specials, such as those used in zle, still need removing
     * from the parameter table; they have the PM_REMOVABLE flag.
     */
    if ((pm->level && locallevel >= pm->level) ||
	(pm->flags & (PM_SPECIAL|PM_REMOVABLE)) == PM_SPECIAL)
	return 0;

    /* remove parameter node from table */
    paramtab->removenode(paramtab, pm->nam);

    if (pm->old) {
	oldpm = pm->old;
	paramtab->addnode(paramtab, oldpm->nam, oldpm);
	if ((PM_TYPE(oldpm->flags) == PM_SCALAR) &&
	    !(pm->flags & PM_HASHELEM) &&
	    (oldpm->flags & PM_NAMEDDIR) &&
	    oldpm->sets.cfn == strsetfn)
	    adduserdir(oldpm->nam, oldpm->u.str, 0, 0);
	if (oldpm->flags & PM_EXPORTED) {
	    /*
	     * Re-export the old value which we removed in typeset_single().
	     * I don't think we need to test for ALL_EXPORT here, since if
	     * it was used to export the parameter originally the parameter
	     * should still have the PM_EXPORTED flag.
	     */
	    export_param(oldpm);
	}
    }

    paramtab->freenode((HashNode) pm); /* free parameter node */

    return 0;
}

/* Standard function to unset a parameter.  This is mostly delegated to *
 * the specific set function.                                           */

/**/
mod_export void
stdunsetfn(Param pm, int exp)
{
    switch (PM_TYPE(pm->flags)) {
	case PM_SCALAR: pm->sets.cfn(pm, NULL); break;
	case PM_ARRAY:  pm->sets.afn(pm, NULL); break;
	case PM_HASHED: pm->sets.hfn(pm, NULL); break;
	default:
	    if (!(pm->flags & PM_SPECIAL))
	    	pm->u.str = NULL;
	    break;
    }
    pm->flags |= PM_UNSET;
}

/* Function to get value of an integer parameter */

/**/
mod_export zlong
intgetfn(Param pm)
{
    return pm->u.val;
}

/* Function to set value of an integer parameter */

/**/
static void
intsetfn(Param pm, zlong x)
{
    pm->u.val = x;
}

/* Function to get value of a floating point parameter */

/**/
static double
floatgetfn(Param pm)
{
    return pm->u.dval;
}

/* Function to set value of an integer parameter */

/**/
static void
floatsetfn(Param pm, double x)
{
    pm->u.dval = x;
}

/* Function to get value of a scalar (string) parameter */

/**/
mod_export char *
strgetfn(Param pm)
{
    return pm->u.str ? pm->u.str : (char *) hcalloc(1);
}

/* Function to set value of a scalar (string) parameter */

/**/
static void
strsetfn(Param pm, char *x)
{
    zsfree(pm->u.str);
    pm->u.str = x;
    if (!(pm->flags & PM_HASHELEM) &&
	((pm->flags & PM_NAMEDDIR) || isset(AUTONAMEDIRS))) {
	pm->flags |= PM_NAMEDDIR;
	adduserdir(pm->nam, x, 0, 0);
    }
}

/* Function to get value of an array parameter */

/**/
char **
arrgetfn(Param pm)
{
    static char *nullarray = NULL;

    return pm->u.arr ? pm->u.arr : &nullarray;
}

/* Function to set value of an array parameter */

/**/
mod_export void
arrsetfn(Param pm, char **x)
{
    if (pm->u.arr && pm->u.arr != x)
	freearray(pm->u.arr);
    if (pm->flags & PM_UNIQUE)
	uniqarray(x);
    pm->u.arr = x;
    /* Arrays tied to colon-arrays may need to fix the environment */
    if (pm->ename && x)
	arrfixenv(pm->ename, x);
}

/* Function to get value of an association parameter */

/**/
mod_export HashTable
hashgetfn(Param pm)
{
    return pm->u.hash;
}

/* Function to set value of an association parameter */

/**/
mod_export void
hashsetfn(Param pm, HashTable x)
{
    if (pm->u.hash && pm->u.hash != x)
	deleteparamtable(pm->u.hash);
    pm->u.hash = x;
}

/* Function to set value of an association parameter using key/value pairs */

/**/
static void
arrhashsetfn(Param pm, char **val, int augment)
{
    /* Best not to shortcut this by using the existing hash table,   *
     * since that could cause trouble for special hashes.  This way, *
     * it's up to pm->sets.hfn() what to do.                         */
    int alen = arrlen(val);
    HashTable opmtab = paramtab, ht = 0;
    char **aptr = val;
    Value v = (Value) hcalloc(sizeof *v);
    v->end = -1;

    if (alen % 2) {
	freearray(val);
	zerr("bad set of key/value pairs for associative array",
	     NULL, 0);
	return;
    }
    if (alen)
    	if (!(augment && (ht = paramtab = pm->gets.hfn(pm))))
	    ht = paramtab = newparamtable(17, pm->nam);
    while (*aptr) {
	/* The parameter name is ztrdup'd... */
	v->pm = createparam(*aptr, PM_SCALAR|PM_UNSET);
	/*
	 * createparam() doesn't return anything if the parameter
	 * already existed.
	 */
	if (!v->pm)
	    v->pm = (Param) paramtab->getnode(paramtab, *aptr);
	zsfree(*aptr++);
	/* ...but we can use the value without copying. */
	setstrvalue(v, *aptr++);
    }
    paramtab = opmtab;
    pm->sets.hfn(pm, ht);
    free(val);		/* not freearray() */
}

/*
 * These functions are used as the set function for special parameters that
 * cannot be set by the user.  The set is incomplete as the only such
 * parameters are scalar and integer.
 */

/**/
void
nullstrsetfn(Param pm, char *x)
{
    zsfree(x);
}

/**/
void
nullintsetfn(Param pm, zlong x)
{}


/* Function to get value of generic special integer *
 * parameter.  data is pointer to global variable   *
 * containing the integer value.                    */

/**/
mod_export zlong
intvargetfn(Param pm)
{
    return *((zlong *)pm->u.data);
}

/* Function to set value of generic special integer *
 * parameter.  data is pointer to global variable   *
 * where the value is to be stored.                 */

/**/
mod_export void
intvarsetfn(Param pm, zlong x)
{
    *((zlong *)pm->u.data) = x;
}

/* Function to set value of any ZLE-related integer *
 * parameter.  data is pointer to global variable   *
 * where the value is to be stored.                 */

/**/
void
zlevarsetfn(Param pm, zlong x)
{
    zlong *p = (zlong *)pm->u.data;

    *p = x;
    if (p == &lines || p == &columns)
	adjustwinsize(2 + (p == &columns));
}

/* Function to set value of generic special scalar    *
 * parameter.  data is pointer to a character pointer *
 * representing the scalar (string).                  */

/**/
mod_export void
strvarsetfn(Param pm, char *x)
{
    char **q = ((char **)pm->u.data);

    zsfree(*q);
    *q = x;
}

/* Function to get value of generic special scalar    *
 * parameter.  data is pointer to a character pointer *
 * representing the scalar (string).                  */

/**/
mod_export char *
strvargetfn(Param pm)
{
    char *s = *((char **)pm->u.data);

    if (!s)
	return hcalloc(1);
    return s;
}

/* Function to get value of generic special array  *
 * parameter.  data is a pointer to the pointer to *
 * a pointer (a pointer to a variable length array *
 * of pointers).                                   */

/**/
mod_export char **
arrvargetfn(Param pm)
{
    return *((char ***)pm->u.data);
}

/* Function to set value of generic special array parameter.    *
 * data is pointer to a variable length array of pointers which *
 * represents this array of scalars (strings).  If pm->ename is *
 * non NULL, then it is a colon separated environment variable  *
 * version of this array which will need to be updated.         */

/**/
mod_export void
arrvarsetfn(Param pm, char **x)
{
    char ***dptr = (char ***)pm->u.data;

    if (*dptr != x)
	freearray(*dptr);
    if (pm->flags & PM_UNIQUE)
	uniqarray(x);
    *dptr = x ? x : mkarray(NULL);
    if (pm->ename && x)
	arrfixenv(pm->ename, x);
}

/**/
char *
colonarrgetfn(Param pm)
{
    char ***dptr = (char ***)pm->u.data;
    return *dptr ? zjoin(*dptr, ':', 1) : "";
}

/**/
void
colonarrsetfn(Param pm, char *x)
{
    char ***dptr = (char ***)pm->u.data;

    /*
     * If this is tied to a parameter (rather than internal) array,
     * the array itself may be NULL.  Otherwise, we have to make
     * sure it doesn't ever get null.
     */
    if (*dptr)
	freearray(*dptr);
    *dptr = x ? colonsplit(x, pm->flags & PM_UNIQUE) :
	(pm->flags & PM_TIED) ? NULL : mkarray(NULL);
    if (pm->ename)
	arrfixenv(pm->nam, *dptr);
    zsfree(x);
}

/**/
void
uniqarray(char **x)
{
    char **t, **p = x;

    if (!x || !*x)
	return;
    while (*++p)
	for (t = x; t < p; t++)
	    if (!strcmp(*p, *t)) {
		zsfree(*p);
		for (t = p--; (*t = t[1]) != NULL; t++);
		break;
	    }
}

/**/
void
zhuniqarray(char **x)
{
    char **t, **p = x;

    if (!x || !*x)
	return;
    while (*++p)
	for (t = x; t < p; t++)
	    if (!strcmp(*p, *t)) {
		for (t = p--; (*t = t[1]) != NULL; t++);
		break;
	    }
}

/* Function to get value of special parameter `#' and `ARGC' */

/**/
zlong
poundgetfn(Param pm)
{
    return arrlen(pparams);
}

/* Function to get value for special parameter `RANDOM' */

/**/
zlong
randomgetfn(Param pm)
{
    return rand() & 0x7fff;
}

/* Function to set value of special parameter `RANDOM' */

/**/
void
randomsetfn(Param pm, zlong v)
{
    srand((unsigned int)v);
}

/* Function to get value for special parameter `SECONDS' */

/**/
zlong
intsecondsgetfn(Param pm)
{
    return (zlong)floatsecondsgetfn(pm);
}

/* Function to set value of special parameter `SECONDS' */

/**/
void
intsecondssetfn(Param pm, zlong x)
{
    floatsecondssetfn(pm, (double)x);
}

/**/
double
floatsecondsgetfn(Param pm)
{
    struct timeval now;
    struct timezone dummy_tz;

    gettimeofday(&now, &dummy_tz);

    return (double)(now.tv_sec - shtimer.tv_sec) +
	(double)(now.tv_usec - shtimer.tv_usec) / 1000000.0;
}

/**/
void
floatsecondssetfn(Param pm, double x)
{
    struct timeval now;
    struct timezone dummy_tz;

    gettimeofday(&now, &dummy_tz);
    shtimer.tv_sec = now.tv_sec - (zlong)x;
    shtimer.tv_usec = now.tv_usec - (zlong)((x - (zlong)x) * 1000000.0);
}

/**/
double
getrawseconds(void)
{
    return (double)shtimer.tv_sec + (double)shtimer.tv_usec / 1000000.0;
}

/**/
void
setrawseconds(double x)
{
    shtimer.tv_sec = (zlong)x;
    shtimer.tv_usec = (zlong)((x - (zlong)x) * 1000000.0);
}

/**/
int
setsecondstype(Param pm, int on, int off)
{
    int newflags = (pm->flags | on) & ~off;
    int tp = PM_TYPE(newflags);
    /* Only one of the numeric types is allowed. */
    if (tp == PM_EFLOAT || tp == PM_FFLOAT)
    {
	pm->gets.ffn = floatsecondsgetfn;
	pm->sets.ffn = floatsecondssetfn;
    }
    else if (tp == PM_INTEGER)
    {
	pm->gets.ifn = intsecondsgetfn;
	pm->sets.ifn = intsecondssetfn;
    }
    else
	return 1;
    pm->flags = newflags;
    return 0;
}

/* Function to get value for special parameter `USERNAME' */

/**/
char *
usernamegetfn(Param pm)
{
    return get_username();
}

/* Function to set value of special parameter `USERNAME' */

/**/
void
usernamesetfn(Param pm, char *x)
{
#if defined(HAVE_SETUID) && defined(HAVE_GETPWNAM)
    struct passwd *pswd;

    if (x && (pswd = getpwnam(x)) && (pswd->pw_uid != cached_uid)) {
# ifdef HAVE_INITGROUPS
	initgroups(x, pswd->pw_gid);
# endif
	if(!setgid(pswd->pw_gid) && !setuid(pswd->pw_uid)) {
	    zsfree(cached_username);
	    cached_username = ztrdup(pswd->pw_name);
	    cached_uid = pswd->pw_uid;
	}
    }
#endif /* HAVE_SETUID && HAVE_GETPWNAM */
    zsfree(x);
}

/* Function to get value for special parameter `UID' */

/**/
zlong
uidgetfn(Param pm)
{
    return getuid();
}

/* Function to set value of special parameter `UID' */

/**/
void
uidsetfn(Param pm, uid_t x)
{
#ifdef HAVE_SETUID
    setuid(x);
#endif
}

/* Function to get value for special parameter `EUID' */

/**/
zlong
euidgetfn(Param pm)
{
    return geteuid();
}

/* Function to set value of special parameter `EUID' */

/**/
void
euidsetfn(Param pm, uid_t x)
{
#ifdef HAVE_SETEUID
    seteuid(x);
#endif
}

/* Function to get value for special parameter `GID' */

/**/
zlong
gidgetfn(Param pm)
{
    return getgid();
}

/* Function to set value of special parameter `GID' */

/**/
void
gidsetfn(Param pm, gid_t x)
{
#ifdef HAVE_SETUID
    setgid(x);
#endif
}

/* Function to get value for special parameter `EGID' */

/**/
zlong
egidgetfn(Param pm)
{
    return getegid();
}

/* Function to set value of special parameter `EGID' */

/**/
void
egidsetfn(Param pm, gid_t x)
{
#ifdef HAVE_SETEUID
    setegid(x);
#endif
}

/**/
zlong
ttyidlegetfn(Param pm)
{
    struct stat ttystat;

    if (SHTTY == -1 || fstat(SHTTY, &ttystat))
	return -1;
    return time(NULL) - ttystat.st_atime;
}

/* Function to get value for special parameter `IFS' */

/**/
char *
ifsgetfn(Param pm)
{
    return ifs;
}

/* Function to set value of special parameter `IFS' */

/**/
void
ifssetfn(Param pm, char *x)
{
    zsfree(ifs);
    ifs = x;
    inittyptab();
}

/* Functions to set value of special parameters `LANG' and `LC_*' */

#ifdef USE_LOCALE
static struct localename {
    char *name;
    int category;
} lc_names[] = {
#ifdef LC_COLLATE
    {"LC_COLLATE", LC_COLLATE},
#endif
#ifdef LC_CTYPE
    {"LC_CTYPE", LC_CTYPE},
#endif
#ifdef LC_MESSAGES
    {"LC_MESSAGES", LC_MESSAGES},
#endif
#ifdef LC_NUMERIC
    {"LC_NUMERIC", LC_NUMERIC},
#endif
#ifdef LC_TIME
    {"LC_TIME", LC_TIME},
#endif
    {NULL, 0}
};

/**/
static void
setlang(char *x)
{
    struct localename *ln;

    setlocale(LC_ALL, x ? x : "");
    queue_signals();
    for (ln = lc_names; ln->name; ln++)
	if ((x = getsparam(ln->name)))
	    setlocale(ln->category, x);
    unqueue_signals();
}

/**/
void
lc_allsetfn(Param pm, char *x)
{
    strsetfn(pm, x);
    if (!x) {
	queue_signals();
	setlang(getsparam("LANG"));
	unqueue_signals();
    }
    else
	setlocale(LC_ALL, x);
}

/**/
void
langsetfn(Param pm, char *x)
{
    strsetfn(pm, x);
    setlang(x);
}

/**/
void
lcsetfn(Param pm, char *x)
{
    struct localename *ln;

    strsetfn(pm, x);
    if (getsparam("LC_ALL"))
	return;
    queue_signals();
    if (!x)
	x = getsparam("LANG");

    for (ln = lc_names; ln->name; ln++)
	if (!strcmp(ln->name, pm->nam))
	    setlocale(ln->category, x ? x : "");
    unqueue_signals();
}
#endif /* USE_LOCALE */

/* Function to get value for special parameter `HISTSIZE' */

/**/
zlong
histsizegetfn(Param pm)
{
    return histsiz;
}

/* Function to set value of special parameter `HISTSIZE' */

/**/
void
histsizesetfn(Param pm, zlong v)
{
    if ((histsiz = v) < 1)
	histsiz = 1;
    resizehistents();
}

/* Function to get value for special parameter `SAVEHIST' */

/**/
zlong
savehistsizegetfn(Param pm)
{
    return savehistsiz;
}

/* Function to set value of special parameter `SAVEHIST' */

/**/
void
savehistsizesetfn(Param pm, zlong v)
{
    if ((savehistsiz = v) < 0)
	savehistsiz = 0;
}

/* Function to get value for special parameter `ERRNO' */

/**/
zlong
errnogetfn(Param pm)
{
    return errno;
}

/* Function to get value for special parameter `histchar' */

/**/
char *
histcharsgetfn(Param pm)
{
    static char buf[4];

    buf[0] = bangchar;
    buf[1] = hatchar;
    buf[2] = hashchar;
    buf[3] = '\0';
    return buf;
}

/* Function to set value of special parameter `histchar' */

/**/
void
histcharssetfn(Param pm, char *x)
{
    if (x) {
	bangchar = x[0];
	hatchar = (bangchar) ? x[1] : '\0';
	hashchar = (hatchar) ? x[2] : '\0';
	zsfree(x);
    } else {
	bangchar = '!';
	hashchar = '#';
	hatchar = '^';
    }
    inittyptab();
}

/* Function to get value for special parameter `HOME' */

/**/
char *
homegetfn(Param pm)
{
    return home;
}

/* Function to set value of special parameter `HOME' */

/**/
void
homesetfn(Param pm, char *x)
{
    zsfree(home);
    if (x && isset(CHASELINKS) && (home = xsymlink(x)))
	zsfree(x);
    else
	home = x ? x : ztrdup("");
    finddir(NULL);
}

/* Function to get value for special parameter `WORDCHARS' */

/**/
char *
wordcharsgetfn(Param pm)
{
    return wordchars;
}

/* Function to set value of special parameter `WORDCHARS' */

/**/
void
wordcharssetfn(Param pm, char *x)
{
    zsfree(wordchars);
    wordchars = x;
    inittyptab();
}

/* Function to get value for special parameter `_' */

/**/
char *
underscoregetfn(Param pm)
{
    char *u = dupstring(underscore);

    untokenize(u);
    return u;
}

/* Function to get value for special parameter `TERM' */

/**/
char *
termgetfn(Param pm)
{
    return term;
}

/* Function to set value of special parameter `TERM' */

/**/
void
termsetfn(Param pm, char *x)
{
    zsfree(term);
    term = x ? x : ztrdup("");

    /* If non-interactive, delay setting up term till we need it. */
    if (unset(INTERACTIVE) || !*term)
	termflags |= TERM_UNKNOWN;
    else 
	init_term();
}

/* Function to get value for special parameter `pipestatus' */

/**/
static char **
pipestatgetfn(Param pm)
{
    char **x = (char **) zhalloc((numpipestats + 1) * sizeof(char *));
    char buf[20], **p;
    int *q, i;

    for (p = x, q = pipestats, i = numpipestats; i--; p++, q++) {
	sprintf(buf, "%d", *q);
	*p = dupstring(buf);
    }
    *p = NULL;

    return x;
}

/* Function to get value for special parameter `pipestatus' */

/**/
static void
pipestatsetfn(Param pm, char **x)
{
    if (x) {
        int i;

        for (i = 0; *x && i < MAX_PIPESTATS; i++, x++)
            pipestats[i] = atoi(*x);
        numpipestats = i;
    }
    else
        numpipestats = 0;
}

/**/
void
arrfixenv(char *s, char **t)
{
    Param pm;

    if (t == path)
	cmdnamtab->emptytable(cmdnamtab);

    pm = (Param) paramtab->getnode(paramtab, s);
    
    /*
     * Only one level of a parameter can be exported.  Unless
     * ALLEXPORT is set, this must be global.
     */

    if (pm->flags & PM_HASHELEM)
	return;

    if (isset(ALLEXPORT))
	pm->flags |= PM_EXPORTED;

    /*
     * Do not "fix" parameters that were not exported
     */

    if (pm->flags & PM_EXPORTED)
	pm->env = addenv(s, t ? zjoin(t, ':', 1) : "", pm->flags);
}


static int
zputenv(char *str)
{
#ifdef HAVE_PUTENV
    return putenv(str);
#else
    char **ep;
    int num_env;


    /* First check if there is already an environment *
     * variable matching string `name'.               */
    if (findenv(str, &num_env)) {
	environ[num_env] = str;
    } else {
    /* Else we have to make room and add it */
	num_env = arrlen(environ);
	environ = (char **) zrealloc(environ, (sizeof(char *)) * (num_env + 2));

	/* Now add it at the end */
	ep = environ + num_env;
	*ep = str;
	*(ep + 1) = NULL;
    }
    return 0;
#endif
}

/**/
static int
findenv(char *name, int *pos)
{
    char **ep, *eq;
    int  nlen;


    eq = strchr(name, '=');
    nlen = eq ? eq - name : strlen(name);
    for (ep = environ; *ep; ep++) 
	if (!strncmp (*ep, name, nlen) && *((*ep)+nlen) == '=') {
	    if (pos)
		*pos = ep - environ;
	    return 1;
	}
    
    return 0;
}

/* Given *name = "foo", it searches the environment for string *
 * "foo=bar", and returns a pointer to the beginning of "bar"  */

/**/
mod_export char *
zgetenv(char *name)
{
#ifdef HAVE_GETENV
    return getenv(name);
#else
    char **ep, *s, *t;
 
    for (ep = environ; *ep; ep++) {
       for (s = *ep, t = name; *s && *s == *t; s++, t++);
       if (*s == '=' && !*t)
           return s + 1;
    }
    return NULL;
#endif
}

/**/
static void
copyenvstr(char *s, char *value, int flags)
{
    while (*s++) {
	if ((*s = *value++) == Meta)
	    *s = *value++ ^ 32;
	if (flags & PM_LOWER)
	    *s = tulower(*s);
	else if (flags & PM_UPPER)
	    *s = tuupper(*s);
    }
}

/**/
char *
addenv(char *name, char *value, int flags)
{
    char *oldenv = 0, *newenv = 0, *env = 0;
    int pos;

    /* First check if there is already an environment *
     * variable matching string `name'. If not, and   *
     * we are not requested to add new, return        */
    if (findenv(name, &pos))
	oldenv = environ[pos];

     newenv = mkenvstr(name, value, flags);
     if (zputenv(newenv)) {
        zsfree(newenv);
	return NULL;
    }
    /*
     * Under Cygwin we must use putenv() to maintain consistency.
     * Unfortunately, current version (1.1.2) copies argument and may
     * silently reuse existing environment string. This tries to
     * check for both cases
     */
    if (findenv(name, &pos)) {
	env = environ[pos];
	if (env != oldenv)
	    zsfree(oldenv);
	if (env != newenv)
	    zsfree(newenv);
	return env;
    }

    return NULL; /* Cannot happen */
}


/* Given strings *name = "foo", *value = "bar", *
 * return a new string *str = "foo=bar".        */

/**/
static char *
mkenvstr(char *name, char *value, int flags)
{
    char *str, *s;
    int len_name, len_value;

    len_name = strlen(name);
    for (len_value = 0, s = value;
	 *s && (*s++ != Meta || *s++ != 32); len_value++);
    s = str = (char *) zalloc(len_name + len_value + 2);
    strcpy(s, name);
    s += len_name;
    *s = '=';
    copyenvstr(s, value, flags);
    return str;
}

/* Given *name = "foo", *value = "bar", add the    *
 * string "foo=bar" to the environment.  Return a  *
 * pointer to the location of this new environment *
 * string.                                         */


/* Delete a pointer from the list of pointers to environment *
 * variables by shifting all the other pointers up one slot. */

/**/
void
delenv(char *x)
{
    char **ep;

    for (ep = environ; *ep; ep++) {
	if (*ep == x)
	    break;
    }
    if (*ep) {
	for (; (ep[0] = ep[1]); ep++);
    }
    zsfree(x);
}

/**/
mod_export void
convbase(char *s, zlong v, int base)
{
    int digs = 0;
    zulong x;

    if (v < 0)
	*s++ = '-', v = -v;
    if (base >= -1 && base <= 1)
	base = -10;

    if (base > 0) {
	if (isset(CBASES) && base == 16)
	    sprintf(s, "0x");
	else if (isset(CBASES) && base == 8 && isset(OCTALZEROES))
	    sprintf(s, "0");
	else if (base != 10)
	    sprintf(s, "%d#", base);
	else
	    *s = 0;
	s += strlen(s);
    } else
	base = -base;
    for (x = v; x; digs++)
	x /= base;
    if (!digs)
	digs = 1;
    s[digs--] = '\0';
    x = v;
    while (digs >= 0) {
	int dig = x % base;

	s[digs--] = (dig < 10) ? '0' + dig : dig - 10 + 'A';
	x /= base;
    }
}

/*
 * Convert a floating point value for output.
 * Unlike convbase(), this has its own internal storage and returns
 * a value from the heap.
 */

/**/
char *
convfloat(double dval, int digits, int flags, FILE *fout)
{
    char fmt[] = "%.*e";
    char *prev_locale, *ret;

    /*
     * The difficulty with the buffer size is that a %f conversion
     * prints all digits before the decimal point: with 64 bit doubles,
     * that's around 310.  We can't check without doing some quite
     * serious floating point operations we'd like to avoid.
     * Then we are liable to get all the digits
     * we asked for after the decimal point, or we should at least
     * bargain for it.  So we just allocate 512 + digits.  This
     * should work until somebody decides on 128-bit doubles.
     */
    if (!(flags & (PM_EFLOAT|PM_FFLOAT))) {
	/*
	 * Conversion from a floating point expression without using
	 * a variable.  The best bet in this case just seems to be
	 * to use the general %g format with something like the maximum
	 * double precision.
	 */
	fmt[3] = 'g';
	if (!digits)
	    digits = 17;
    } else {
	if (flags & PM_FFLOAT)
	    fmt[3] = 'f';
	if (digits <= 0)
	    digits = 10;
	if (flags & PM_EFLOAT) {
	    /*
	     * Here, we are given the number of significant figures, but
	     * %e wants the number of decimal places (unlike %g)
	     */
	    digits--;
	}
    }
#ifdef USE_LOCALE
    prev_locale = dupstring(setlocale(LC_NUMERIC, NULL));
    setlocale(LC_NUMERIC, "POSIX");
#endif
    if (fout) {
	fprintf(fout, fmt, digits, dval);
	ret = NULL;
    } else {
	VARARR(char, buf, 512 + digits);
	sprintf(buf, fmt, digits, dval);
	if (!strchr(buf, 'e') && !strchr(buf, '.'))
	    strcat(buf, ".");
	ret = dupstring(buf);
    }
#ifdef USE_LOCALE
    if (prev_locale) setlocale(LC_NUMERIC, prev_locale);
#endif
    return ret;
}

/* Start a parameter scope */

/**/
mod_export void
startparamscope(void)
{
    locallevel++;
}

/* End a parameter scope: delete the parameters local to the scope. */

/**/
mod_export void
endparamscope(void)
{
    locallevel--;
    scanhashtable(paramtab, 0, 0, 0, scanendscope, 0);
}

/**/
static void
scanendscope(HashNode hn, int flags)
{
    Param pm = (Param)hn;
    if (pm->level > locallevel) {
	if ((pm->flags & (PM_SPECIAL|PM_REMOVABLE)) == PM_SPECIAL) {
	    /*
	     * Removable specials are normal in that they can be removed
	     * to reveal an ordinary parameter beneath.  Here we handle
	     * non-removable specials, which were made local by stealth
	     * (see newspecial code in typeset_single()).  In fact the
	     * visible pm is always the same struct; the pm->old is
	     * just a place holder for old data and flags.
	     */
	    Param tpm = pm->old;

	    if (!strcmp(pm->nam, "SECONDS"))
	    {
		setsecondstype(pm, PM_TYPE(tpm->flags), PM_TYPE(pm->flags));
		/*
		 * We restore SECONDS by restoring its raw internal value
		 * that we cached off into tpm->u.dval.
		 */
		setrawseconds(tpm->u.dval);
		tpm->flags |= PM_NORESTORE;
	    }
	    DPUTS(!tpm || PM_TYPE(pm->flags) != PM_TYPE(tpm->flags) ||
		  !(tpm->flags & PM_SPECIAL),
		  "BUG: in restoring scope of special parameter");
	    pm->old = tpm->old;
	    pm->flags = (tpm->flags & ~PM_NORESTORE);
	    pm->level = tpm->level;
	    pm->ct = tpm->ct;
	    if (pm->env) {
		delenv(pm->env);
	    }
	    pm->env = NULL;

	    if (!(tpm->flags & PM_NORESTORE))
		switch (PM_TYPE(pm->flags)) {
		case PM_SCALAR:
		    pm->sets.cfn(pm, tpm->u.str);
		    break;
		case PM_INTEGER:
		    pm->sets.ifn(pm, tpm->u.val);
		    break;
		case PM_EFLOAT:
		case PM_FFLOAT:
		    pm->sets.ffn(pm, tpm->u.dval);
		    break;
		case PM_ARRAY:
		    pm->sets.afn(pm, tpm->u.arr);
		    break;
		case PM_HASHED:
		    pm->sets.hfn(pm, tpm->u.hash);
		    break;
		}
	    zfree(tpm, sizeof(*tpm));

	    if (pm->flags & PM_EXPORTED)
		export_param(pm);
	} else
	    unsetparam_pm(pm, 0, 0);
    }
}


/**********************************/
/* Parameter Hash Table Functions */
/**********************************/

/**/
void
freeparamnode(HashNode hn)
{
    Param pm = (Param) hn;
 
    /* Since the second flag to unsetfn isn't used, I don't *
     * know what its value should be.                       */
    if (delunset)
	pm->unsetfn(pm, 1);
    zsfree(pm->nam);
    /* If this variable was tied by the user, ename was ztrdup'd */
    if (pm->flags & PM_TIED)
	zsfree(pm->ename);
    zfree(pm, sizeof(struct param));
}

/* Print a parameter */

enum paramtypes_flags {
    PMTF_USE_CT		= (1<<0),
    PMTF_TEST_LEVEL	= (1<<1)
};

struct paramtypes {
    int binflag;	/* The relevant PM_FLAG(S) */
    const char *string;	/* String for verbose output */
    int typeflag;	/* Flag for typeset -? */
    int flags;		/* The enum above */
};

static const struct paramtypes pmtypes[] = {
    { PM_AUTOLOAD, "undefined", 0, 0},
    { PM_INTEGER, "integer", 'i', PMTF_USE_CT},
    { PM_EFLOAT, "float", 'E', 0},
    { PM_FFLOAT, "float", 'F', 0},
    { PM_ARRAY, "array", 'a', 0},
    { PM_HASHED, "association", 'A', 0},
    { 0, "local", 0, PMTF_TEST_LEVEL},
    { PM_LEFT, "left justified", 'L', PMTF_USE_CT},
    { PM_RIGHT_B, "right justified", 'R', PMTF_USE_CT},
    { PM_RIGHT_Z, "zero filled", 'Z', PMTF_USE_CT},
    { PM_LOWER, "lowercase", 'l', 0},
    { PM_UPPER, "uppercase", 'u', 0},
    { PM_READONLY, "readonly", 'r', 0},
    { PM_TAGGED, "tagged", 't', 0},
    { PM_EXPORTED, "exported", 'x', 0}
};

/**/
mod_export void
printparamnode(HashNode hn, int printflags)
{
    Param p = (Param) hn;
    char *t, **u;

    if (p->flags & PM_UNSET)
	return;

    if (printflags & PRINT_TYPESET)
	printf("typeset ");

    /* Print the attributes of the parameter */
    if (printflags & (PRINT_TYPE|PRINT_TYPESET)) {
	int doneminus = 0, i;
	const struct paramtypes *pmptr;

	for (pmptr = pmtypes, i = 0; i < sizeof(pmtypes)/sizeof(*pmptr);
	     i++, pmptr++) {
	    int doprint = 0;
	    if (pmptr->flags & PMTF_TEST_LEVEL) {
		if (p->level)
		    doprint = 1;
	    } else if (p->flags & pmptr->binflag)
		doprint = 1;

	    if (doprint) {
		if (printflags & PRINT_TYPESET) {
		    if (pmptr->typeflag) {
			if (!doneminus) {
			    putchar('-');
			    doneminus = 1;
			}
			putchar(pmptr->typeflag);
		    }
		} else {
		    printf("%s ", pmptr->string);
		}
		if ((pmptr->flags & PMTF_USE_CT) && p->ct) {
		    printf("%d ", p->ct);
		    doneminus = 0;
		}
	    }
	}
	if (doneminus)
	    putchar(' ');
    }

    if ((printflags & PRINT_NAMEONLY) ||
	((p->flags & PM_HIDEVAL) && !(printflags & PRINT_INCLUDEVALUE))) {
	zputs(p->nam, stdout);
	putchar('\n');
	return;
    }

    quotedzputs(p->nam, stdout);

    if (p->flags & PM_AUTOLOAD) {
	putchar('\n');
	return;
    }
    if (printflags & PRINT_KV_PAIR)
	putchar(' ');
    else if ((printflags & PRINT_TYPESET) &&
	     (PM_TYPE(p->flags) == PM_ARRAY || PM_TYPE(p->flags) == PM_HASHED))
	printf("\n%s=", p->nam);
    else
	putchar('=');

    /* How the value is displayed depends *
     * on the type of the parameter       */
    switch (PM_TYPE(p->flags)) {
    case PM_SCALAR:
	/* string: simple output */
	if (p->gets.cfn && (t = p->gets.cfn(p)))
	    quotedzputs(t, stdout);
	break;
    case PM_INTEGER:
	/* integer */
#ifdef ZSH_64_BIT_TYPE
	fputs(output64(p->gets.ifn(p)), stdout);
#else
	printf("%ld", p->gets.ifn(p));
#endif
	break;
    case PM_EFLOAT:
    case PM_FFLOAT:
	/* float */
	convfloat(p->gets.ffn(p), p->ct, p->flags, stdout);
	break;
    case PM_ARRAY:
	/* array */
	if (!(printflags & PRINT_KV_PAIR))
	    putchar('(');
	u = p->gets.afn(p);
	if(*u) {
	    quotedzputs(*u++, stdout);
	    while (*u) {
		putchar(' ');
		quotedzputs(*u++, stdout);
	    }
	}
	if (!(printflags & PRINT_KV_PAIR))
	    putchar(')');
	break;
    case PM_HASHED:
	/* association */
	if (!(printflags & PRINT_KV_PAIR))
	    putchar('(');
	{
            HashTable ht = p->gets.hfn(p);
            if (ht)
		scanhashtable(ht, 0, 0, PM_UNSET,
			      ht->printnode, PRINT_KV_PAIR);
	}
	if (!(printflags & PRINT_KV_PAIR))
	    putchar(')');
	break;
    }
    if (printflags & PRINT_KV_PAIR)
	putchar(' ');
    else
	putchar('\n');
}
