/*
 * builtin.c - builtin commands
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
#include "builtin.pro"

/* Builtins in the main executable */

static struct builtin builtins[] =
{
    BIN_PREFIX("-", BINF_DASH),
    BIN_PREFIX("builtin", BINF_BUILTIN),
    BIN_PREFIX("command", BINF_COMMAND),
    BIN_PREFIX("exec", BINF_EXEC),
    BIN_PREFIX("noglob", BINF_NOGLOB),
    BUILTIN("[", 0, bin_test, 0, -1, BIN_BRACKET, NULL, NULL),
    BUILTIN(".", BINF_PSPECIAL, bin_dot, 1, -1, 0, NULL, NULL),
    BUILTIN(":", BINF_PSPECIAL, bin_true, 0, -1, 0, NULL, NULL),
    BUILTIN("alias", BINF_MAGICEQUALS, bin_alias, 0, -1, 0, "Lgmr", NULL),
    BUILTIN("autoload", BINF_TYPEOPTS, bin_functions, 0, -1, 0, "t", "u"),
    BUILTIN("bg", 0, bin_fg, 0, -1, BIN_BG, NULL, NULL),
    BUILTIN("break", BINF_PSPECIAL, bin_break, 0, 1, BIN_BREAK, NULL, NULL),
    BUILTIN("bye", 0, bin_break, 0, 1, BIN_EXIT, NULL, NULL),
    BUILTIN("cd", 0, bin_cd, 0, 2, BIN_CD, NULL, NULL),
    BUILTIN("chdir", 0, bin_cd, 0, 2, BIN_CD, NULL, NULL),
    BUILTIN("continue", BINF_PSPECIAL, bin_break, 0, 1, BIN_CONTINUE, NULL, NULL),
    BUILTIN("declare", BINF_TYPEOPTS | BINF_MAGICEQUALS | BINF_PSPECIAL, bin_typeset, 0, -1, 0, "LRUZfilrtux", NULL),
    BUILTIN("dirs", 0, bin_dirs, 0, -1, 0, "v", NULL),
    BUILTIN("disable", 0, bin_enable, 0, -1, BIN_DISABLE, "afmr", NULL),
    BUILTIN("disown", 0, bin_fg, 0, -1, BIN_DISOWN, NULL, NULL),
    BUILTIN("echo", BINF_PRINTOPTS | BINF_ECHOPTS, bin_print, 0, -1, BIN_ECHO, "neE", "-"),
    BUILTIN("echotc", 0, bin_echotc, 1, -1, 0, NULL, NULL),
    BUILTIN("emulate", 0, bin_emulate, 1, 1, 0, "R", NULL),
    BUILTIN("enable", 0, bin_enable, 0, -1, BIN_ENABLE, "afmr", NULL),
    BUILTIN("eval", BINF_PSPECIAL, bin_eval, 0, -1, BIN_EVAL, NULL, NULL),
    BUILTIN("exit", BINF_PSPECIAL, bin_break, 0, 1, BIN_EXIT, NULL, NULL),
    BUILTIN("export", BINF_TYPEOPTS | BINF_MAGICEQUALS | BINF_PSPECIAL, bin_typeset, 0, -1, BIN_EXPORT, "LRUZfilrtu", "x"),
    BUILTIN("false", 0, bin_false, 0, -1, 0, NULL, NULL),
    BUILTIN("fc", BINF_FCOPTS, bin_fc, 0, -1, BIN_FC, "nlreIRWAdDfEim", NULL),
    BUILTIN("fg", 0, bin_fg, 0, -1, BIN_FG, NULL, NULL),
    BUILTIN("functions", BINF_TYPEOPTS, bin_functions, 0, -1, 0, "mtu", NULL),
    BUILTIN("getln", 0, bin_read, 0, -1, 0, "ecnAlE", "zr"),
    BUILTIN("getopts", 0, bin_getopts, 2, -1, 0, NULL, NULL),
    BUILTIN("hash", BINF_MAGICEQUALS, bin_hash, 0, -1, 0, "dfmrv", NULL),

#ifdef ZSH_HASH_DEBUG
    BUILTIN("hashinfo", 0, bin_hashinfo, 0, 0, 0, NULL, NULL),
#endif

    BUILTIN("history", 0, bin_fc, 0, -1, BIN_FC, "nrdDfEim", "l"),
    BUILTIN("integer", BINF_TYPEOPTS | BINF_MAGICEQUALS | BINF_PSPECIAL, bin_typeset, 0, -1, 0, "lrtux", "i"),
    BUILTIN("jobs", 0, bin_fg, 0, -1, BIN_JOBS, "dlpZrs", NULL),
    BUILTIN("kill", 0, bin_kill, 0, -1, 0, NULL, NULL),
    BUILTIN("let", 0, bin_let, 1, -1, 0, NULL, NULL),
    BUILTIN("local", BINF_TYPEOPTS | BINF_MAGICEQUALS | BINF_PSPECIAL, bin_typeset, 0, -1, 0, "LRUZilrtu", NULL),
    BUILTIN("log", 0, bin_log, 0, 0, 0, NULL, NULL),
    BUILTIN("logout", 0, bin_break, 0, 1, BIN_LOGOUT, NULL, NULL),

#if defined(ZSH_MEM) & defined(ZSH_MEM_DEBUG)
    BUILTIN("mem", 0, bin_mem, 0, 0, 0, "v", NULL),
#endif

    BUILTIN("popd", 0, bin_cd, 0, 2, BIN_POPD, NULL, NULL),
    BUILTIN("print", BINF_PRINTOPTS, bin_print, 0, -1, BIN_PRINT, "RDPnrslzNu0123456789pioOcm-", NULL),
    BUILTIN("pushd", 0, bin_cd, 0, 2, BIN_PUSHD, NULL, NULL),
    BUILTIN("pushln", BINF_PRINTOPTS, bin_print, 0, -1, BIN_PRINT, NULL, "-nz"),
    BUILTIN("pwd", 0, bin_pwd, 0, 0, 0, "rLP", NULL),
    BUILTIN("r", BINF_R, bin_fc, 0, -1, BIN_FC, "nrl", NULL),
    BUILTIN("read", 0, bin_read, 0, -1, 0, "rzu0123456789pkqecnAlE", NULL),
    BUILTIN("readonly", BINF_TYPEOPTS | BINF_MAGICEQUALS | BINF_PSPECIAL, bin_typeset, 0, -1, 0, "LRUZfiltux", "r"),
    BUILTIN("rehash", 0, bin_hash, 0, 0, 0, "dfv", "r"),
    BUILTIN("return", BINF_PSPECIAL, bin_break, 0, 1, BIN_RETURN, NULL, NULL),
    BUILTIN("set", BINF_PSPECIAL, bin_set, 0, -1, 0, NULL, NULL),
    BUILTIN("setopt", 0, bin_setopt, 0, -1, BIN_SETOPT, NULL, NULL),
    BUILTIN("shift", BINF_PSPECIAL, bin_shift, 0, -1, 0, NULL, NULL),
    BUILTIN("source", BINF_PSPECIAL, bin_dot, 1, -1, 0, NULL, NULL),
    BUILTIN("suspend", 0, bin_suspend, 0, 0, 0, "f", NULL),
    BUILTIN("test", 0, bin_test, 0, -1, BIN_TEST, NULL, NULL),
    BUILTIN("ttyctl", 0, bin_ttyctl, 0, 0, 0, "fu", NULL),
    BUILTIN("times", BINF_PSPECIAL, bin_times, 0, 0, 0, NULL, NULL),
    BUILTIN("trap", BINF_PSPECIAL, bin_trap, 0, -1, 0, NULL, NULL),
    BUILTIN("true", 0, bin_true, 0, -1, 0, NULL, NULL),
    BUILTIN("type", 0, bin_whence, 0, -1, 0, "ampfsw", "v"),
    BUILTIN("typeset", BINF_TYPEOPTS | BINF_MAGICEQUALS | BINF_PSPECIAL, bin_typeset, 0, -1, 0, "LRUZfilrtuxm", NULL),
    BUILTIN("umask", 0, bin_umask, 0, 1, 0, "S", NULL),
    BUILTIN("unalias", 0, bin_unhash, 1, -1, 0, "m", "a"),
    BUILTIN("unfunction", 0, bin_unhash, 1, -1, 0, "m", "f"),
    BUILTIN("unhash", 0, bin_unhash, 1, -1, 0, "adfm", NULL),
    BUILTIN("unset", BINF_PSPECIAL, bin_unset, 1, -1, 0, "fm", NULL),
    BUILTIN("unsetopt", 0, bin_setopt, 0, -1, BIN_UNSETOPT, NULL, NULL),
    BUILTIN("wait", 0, bin_fg, 0, -1, BIN_WAIT, NULL, NULL),
    BUILTIN("whence", 0, bin_whence, 0, -1, 0, "acmpvfsw", NULL),
    BUILTIN("where", 0, bin_whence, 0, -1, 0, "pmsw", "ca"),
    BUILTIN("which", 0, bin_whence, 0, -1, 0, "ampsw", "c"),

#ifdef DYNAMIC
    BUILTIN("zmodload", 0, bin_zmodload, 0, -1, 0, "Laudi", NULL),
#endif
};

/****************************************/
/* Builtin Command Hash Table Functions */
/****************************************/

/* hash table containing builtin commands */

/**/
HashTable builtintab;
 
/**/
void
createbuiltintable(void)
{
    builtintab = newhashtable(85, "builtintab", NULL);

    builtintab->hash        = hasher;
    builtintab->emptytable  = NULL;
    builtintab->filltable   = NULL;
    builtintab->addnode     = addhashnode;
    builtintab->getnode     = gethashnode;
    builtintab->getnode2    = gethashnode2;
    builtintab->removenode  = removehashnode;
    builtintab->disablenode = disablehashnode;
    builtintab->enablenode  = enablehashnode;
    builtintab->freenode    = freebuiltinnode;
    builtintab->printnode   = printbuiltinnode;

    addbuiltins("zsh", builtins, sizeof(builtins)/sizeof(*builtins));
}

/* Print a builtin */

/**/
static void
printbuiltinnode(HashNode hn, int printflags)
{
    Builtin bn = (Builtin) hn;

    if (printflags & PRINT_WHENCE_WORD) {
	printf("%s: builtin\n", bn->nam);
	return;
    }

    if (printflags & PRINT_WHENCE_CSH) {
	printf("%s: shell built-in command\n", bn->nam);
	return;
    }

    if (printflags & PRINT_WHENCE_VERBOSE) {
	printf("%s is a shell builtin\n", bn->nam);
	return;
    }

    /* default is name only */
    printf("%s\n", bn->nam);
}

/**/
static void
freebuiltinnode(HashNode hn)
{
    Builtin bn = (Builtin) hn;
 
    if(!(bn->flags & BINF_ADDED)) {
	zsfree(bn->nam);
	zsfree(bn->optstr);
	zfree(bn, sizeof(struct builtin));
    }
}

static char *auxdata;
static int auxlen;

/* execute a builtin handler function after parsing the arguments */

#define MAX_OPS 128

/**/
int
execbuiltin(LinkList args, Builtin bn)
{
    LinkNode n;
    char ops[MAX_OPS], *arg, *pp, *name, **argv, **oargv, *optstr;
    char *oxarg, *xarg = NULL;
    int flags, sense, argc = 0, execop;

    /* initialise some static variables */
    auxdata = NULL;
    auxlen = 0;

    /* initialize some local variables */
    memset(ops, 0, MAX_OPS);
    name = (char *) ugetnode(args);

    arg = (char *) ugetnode(args);

#ifdef DYNAMIC
    if (!bn->handlerfunc) {
	zwarnnam(name, "autoload failed", NULL, 0);
	deletebuiltin(bn->nam);
	return 1;
    }
#endif

    /* get some information about the command */
    flags = bn->flags;
    optstr = bn->optstr;

    /* Sort out the options. */
    if ((flags & BINF_ECHOPTS) && isset(BSDECHO))
	ops['E'] = 1;
    if (optstr)
	/* while arguments look like options ... */
	while (arg &&
	       ((sense = (*arg == '-')) ||
		 ((flags & BINF_PLUSOPTS) && *arg == '+')) &&
	       ((flags & BINF_PLUSOPTS) || !atoi(arg))) {
	    /* unrecognised options to echo etc. are not really options */
	    if (flags & BINF_ECHOPTS) {
		char *p = arg;
		while (*++p && strchr(optstr, (int) *p));
		if (*p)
		    break;
	    }
	    /* save the options in xarg, for execution tracing */
	    if (xarg) {
		oxarg = tricat(xarg, " ", arg);
		zsfree(xarg);
		xarg = oxarg;
	    } else
		xarg = ztrdup(arg);
	    /* handle -- or - (ops['-']), and + (ops['-'] and ops['+']) */
	    if (arg[1] == '-')
		arg++;
	    if (!arg[1]) {
		ops['-'] = 1;
		if (!sense)
		    ops['+'] = 1;
	    }
	    /* save options in ops, as long as they are in bn->optstr */
	    execop = -1;
	    while (*++arg)
		if (strchr(optstr, execop = (int)*arg))
		    ops[(int)*arg] = (sense) ? 1 : 2;
		else
		    break;
	    /* "typeset" may take a numeric argument *
	     * at the tail of the options            */
	    if (idigit(*arg) && (flags & BINF_TYPEOPT) &&
		(arg[-1] == 'L' || arg[-1] == 'R' ||
		 arg[-1] == 'Z' || arg[-1] == 'i'))
		auxlen = (int)zstrtol(arg, &arg, 10);
	    /* The above loop may have exited on an invalid option.  (We  *
	     * assume that any option requiring metafication is invalid.) */
	    if (*arg) {
		if(*arg == Meta)
		    *++arg ^= 32;
		zerr("bad option: -%c", NULL, *arg);
		zsfree(xarg);
		return 1;
	    }
	    arg = (char *) ugetnode(args);
	    /* for the "print" builtin, the options after -R are treated as
	    options to "echo" */
	    if ((flags & BINF_PRINTOPTS) && ops['R']) {
		optstr = "ne";
		flags |= BINF_ECHOPTS;
	    }
	    /* the option -- indicates the end of the options */
	    if (ops['-'])
		break;
	    /* for "fc", -e takes an extra argument */
	    if ((flags & BINF_FCOPTS) && execop == 'e') {
		auxdata = arg;
		arg = (char *) ugetnode(args);
	    }
	    /* for "typeset", -L, -R, -Z and -i take a numeric extra argument */
	    if ((flags & BINF_TYPEOPT) && (execop == 'L' || execop == 'R' ||
		execop == 'Z' || execop == 'i') && arg && idigit(*arg)) {
		auxlen = atoi(arg);
		arg = (char *) ugetnode(args);
	    }
	}
    if (flags & BINF_R)
	auxdata = "-";
    /* handle built-in options, for overloaded handler functions */
    if ((pp = bn->defopts))
	while (*pp)
	    ops[(int)*pp++] = 1;

    /* Set up the argument list. */
    if (arg) {
	/* count the arguments */
	argc = 1;
	n = firstnode(args);
	while (n)
	    argc++, incnode(n);
    }
    /* Get the actual arguments, into argv.  Oargv saves the *
     * beginning of the array for later reference.           */
    oargv = argv = (char **)ncalloc(sizeof(char **) * (argc + 1));
    if ((*argv++ = arg))
	while ((*argv++ = (char *)ugetnode(args)));
    argv = oargv;
    if (errflag) {
	zsfree(xarg);
	errflag = 0;
	return 1;
    }

    /* check that the argument count lies within the specified bounds */
    if (argc < bn->minargs || (argc > bn->maxargs && bn->maxargs != -1)) {
	zwarnnam(name, (argc < bn->minargs)
		? "not enough arguments" : "too many arguments", NULL, 0);
	zsfree(xarg);
	return 1;
    }

    /* display execution trace information, if required */
    if (isset(XTRACE)) {
	fprintf(stderr, "%s%s", (prompt4) ? prompt4 : "", name);
	if (xarg)
	    fprintf(stderr, " %s", xarg);
	while (*oargv)
	    fprintf(stderr, " %s", *oargv++);
	fputc('\n', stderr);
	fflush(stderr);
    }
    zsfree(xarg);
    /* call the handler function, and return its return value */
    return (*(bn->handlerfunc)) (name, argv, ops, bn->funcid);
}

/* Enable/disable an element in one of the internal hash tables.  *
 * With no arguments, it lists all the currently enabled/disabled *
 * elements in that particular hash table.                        */

/**/
int
bin_enable(char *name, char **argv, char *ops, int func)
{
    HashTable ht;
    HashNode hn;
    ScanFunc scanfunc;
    Comp com;
    int flags1 = 0, flags2 = 0;
    int match = 0, returnval = 0;

    /* Find out which hash table we are working with. */
    if (ops['f'])
	ht = shfunctab;
    else if (ops['r'])
	ht = reswdtab;
    else if (ops['a'])
	ht = aliastab;
    else
	ht = builtintab;

    /* Do we want to enable or disable? */
    if (func == BIN_ENABLE) {
	flags2 = DISABLED;
	scanfunc = ht->enablenode;
    } else {
	flags1 = DISABLED;
	scanfunc = ht->disablenode;
    }

    /* Given no arguments, print the names of the enabled/disabled elements  *
     * in this hash table.  If func == BIN_ENABLE, then scanhashtable will   *
     * print nodes NOT containing the DISABLED flag, else scanhashtable will *
     * print nodes containing the DISABLED flag.                             */
    if (!*argv) {
	scanhashtable(ht, 1, flags1, flags2, ht->printnode, 0);
	return 0;
    }

    /* With -m option, treat arguments as glob patterns. */
    if (ops['m']) {
	for (; *argv; argv++) {
	    /* parse pattern */
	    tokenize(*argv);
	    if ((com = parsereg(*argv)))
		match += scanmatchtable(ht, com, 0, 0, scanfunc, 0);
	    else {
		untokenize(*argv);
		zwarnnam(name, "bad pattern : %s", *argv, 0);
		returnval = 1;
	    }
	}
	/* If we didn't match anything, we return 1. */
	if (!match)
	    returnval = 1;
	return returnval;
    }

    /* Take arguments literally -- do not glob */
    for (; *argv; argv++) {
	    if ((hn = ht->getnode2(ht, *argv))) {
		scanfunc(hn, 0);
	    } else {
		zwarnnam(name, "no such hash table element: %s", *argv, 0);
		returnval = 1;
	    }
	}
    return returnval;
}

/* set: either set the shell options, or set the shell arguments, *
 * or declare an array, or show various things                    */

/**/
int
bin_set(char *nam, char **args, char *ops, int func)
{
    int action, optno, array = 0, hadopt = 0,
	hadplus = 0, hadend = 0, sort = 0;
    char **x;

    /* Obsolecent sh compatibility: set - is the same as set +xv *
     * and set - args is the same as set +xv -- args             */
    if (*args && **args == '-' && !args[0][1]) {
	dosetopt(VERBOSE, 0, 0);
	dosetopt(XTRACE, 0, 0);
	if (!args[1])
	    return 0;
    }

    /* loop through command line options (begins with "-" or "+") */
    while (*args && (**args == '-' || **args == '+')) {
	action = (**args == '-');
	hadplus |= !action;
	if(!args[0][1])
	    *args = "--";
	while (*++*args) {
	    if(**args == Meta)
		*++*args ^= 32;
	    if(**args != '-' || action)
		hadopt = 1;
	    /* The pseudo-option `--' signifies the end of options. */
	    if (**args == '-') {
		hadend = 1;
		args++;
		goto doneoptions;
	    } else if (**args == 'o') {
		if (!*++*args)
		    args++;
		if (!*args) {
		    zwarnnam(nam, "string expected after -o", NULL, 0);
		    inittyptab();
		    return 1;
		}
		if(!(optno = optlookup(*args)))
		    zwarnnam(nam, "no such option: %s", *args, 0);
		else if(dosetopt(optno, action, 0))
		    zwarnnam(nam, "can't change option: %s", *args, 0);
		break;
	    } else if(**args == 'A') {
		if(!*++*args)
		    args++;
		array = action ? 1 : -1;
		goto doneoptions;
	    } else if (**args == 's')
		sort = action ? 1 : -1;
	    else {
	    	if (!(optno = optlookupc(**args)))
		    zwarnnam(nam, "bad option: -%c", NULL, **args);
		else if(dosetopt(optno, action, 0))
		    zwarnnam(nam, "can't change option: -%c", NULL, **args);
	    }
	}
	args++;
    }
    doneoptions:
    inittyptab();

    /* Show the parameters, possibly with values */
    if (!hadopt && !*args)
	scanhashtable(paramtab, 1, 0, 0, paramtab->printnode,
	    hadplus ? PRINT_NAMEONLY : 0);

    if (array && !*args) {
	/* display arrays */
	scanhashtable(paramtab, 1, PM_ARRAY, 0, paramtab->printnode,
	    hadplus ? PRINT_NAMEONLY : 0);
    }
    if (!*args && !hadend)
	return 0;
    if (array)
	args++;
    if (sort)
	qsort(args, arrlen(args), sizeof(char *),
	      sort > 0 ? strpcmp : invstrpcmp);
    if (array) {
	/* create an array with the specified elements */
	char **a = NULL, **y, *name = args[-1];
	int len = arrlen(args);

	if (array < 0 && (a = getaparam(name))) {
	    int al = arrlen(a);

	    if (al > len)
		len = al;
	}
	for (x = y = zalloc((len + 1) * sizeof(char *)); len--; a++) {
	    if (!*args)
		args = a;
	    *y++ = ztrdup(*args++);
	}
	*y++ = NULL;
	setaparam(name, x);
    } else {
	/* set shell arguments */
	freearray(pparams);
	PERMALLOC {
	    pparams = arrdup(args);
	} LASTALLOC;
    }
    return 0;
}

/**** directory-handling builtins ****/

/**/
int doprintdir = 0;		/* set in exec.c (for autocd) */

/* pwd: display the name of the current directory */

/**/
int
bin_pwd(char *name, char **argv, char *ops, int func)
{
    if (ops['r'] || ops['P'] || (isset(CHASELINKS) && !ops['L']))
	printf("%s\n", zgetcwd());
    else {
	zputs(pwd, stdout);
	putchar('\n');
    }
    return 0;
}

/* the directory stack */
 
/**/
LinkList dirstack;
 
/* dirs: list the directory stack, or replace it with a provided list */

/**/
int
bin_dirs(char *name, char **argv, char *ops, int func)
{
    LinkList l;

    /* with the -v option, provide a numbered list of directories, starting at
    zero */
    if (ops['v']) {
	LinkNode node;
	int pos = 1;

	printf("0\t");
	fprintdir(pwd, stdout);
	for (node = firstnode(dirstack); node; incnode(node)) {
	    printf("\n%d\t", pos++);
	    fprintdir(getdata(node), stdout);
	}
	putchar('\n');
	return 0;
    }
    /* given no arguments, list the stack normally */
    if (!*argv) {
	printdirstack();
	return 0;
    }
    /* replace the stack with the specified directories */
    PERMALLOC {
	l = newlinklist();
	if (*argv) {
	    while (*argv)
		addlinknode(l, ztrdup(*argv++));
	    freelinklist(dirstack, freestr);
	    dirstack = l;
	}
    } LASTALLOC;
    return 0;
}

/* cd, chdir, pushd, popd */

/**/
void
set_pwd_env(void)
{
    Param pm;

    pm = (Param) paramtab->getnode(paramtab, "PWD");
    if (pm && PM_TYPE(pm->flags) != PM_SCALAR) {
	pm->flags &= ~PM_READONLY;
	unsetparam_pm(pm, 0, 1);
    }

    pm = (Param) paramtab->getnode(paramtab, "OLDPWD");
    if (pm && PM_TYPE(pm->flags) != PM_SCALAR) {
	pm->flags &= ~PM_READONLY;
	unsetparam_pm(pm, 0, 1);
    }

    setsparam("PWD", ztrdup(pwd));
    setsparam("OLDPWD", ztrdup(oldpwd));

    pm = (Param) paramtab->getnode(paramtab, "PWD");
    if (!(pm->flags & PM_EXPORTED)) {
	pm->flags |= PM_EXPORTED;
	pm->env = addenv("PWD", pwd);
    }
    pm = (Param) paramtab->getnode(paramtab, "OLDPWD");
    if (!(pm->flags & PM_EXPORTED)) {
	pm->flags |= PM_EXPORTED;
	pm->env = addenv("PWD", pwd);
    }
}

/* The main pwd changing function.  The real work is done by other     *
 * functions.  cd_get_dest() does the initial argument processing;     *
 * cd_do_chdir() actually changes directory, if possible; cd_new_pwd() *
 * does the ancilliary processing associated with actually changing    *
 * directory.                                                          */

/**/
int
bin_cd(char *nam, char **argv, char *ops, int func)
{
    LinkNode dir;
    struct stat st1, st2;
    int chaselinks;

    if (isset(RESTRICTED)) {
	zwarnnam(nam, "restricted", NULL, 0);
	return 1;
    }
    doprintdir = (doprintdir == -1);

    for (; *argv && **argv == '-'; argv++) {
	char *s = *argv + 1;

	do {
	    switch (*s) {
	    case 's':
	    case 'P':
	    case 'L':
		break;
	    default:
		goto brk;
	    }
	} while (*++s);
	for (s = *argv; *++s; ops[*s] = 1);
    }
  brk:
    chaselinks = ops['P'] || (isset(CHASELINKS) && !ops['L']);
    PERMALLOC {
	pushnode(dirstack, ztrdup(pwd));
	if (!(dir = cd_get_dest(nam, argv, ops, func))) {
	    zsfree(getlinknode(dirstack));
	    LASTALLOC_RETURN 1;
	}
    } LASTALLOC;
    cd_new_pwd(func, dir, chaselinks);

    if (stat(unmeta(pwd), &st1) < 0) {
	zsfree(pwd);
	pwd = metafy(zgetcwd(), -1, META_DUP);
    } else if (stat(".", &st2) < 0)
	chdir(unmeta(pwd));
    else if (st1.st_ino != st2.st_ino || st1.st_dev != st2.st_dev) {
	if (chaselinks) {
	    zsfree(pwd);
	    pwd = metafy(zgetcwd(), -1, META_DUP);
	} else {
	    chdir(unmeta(pwd));
	}
    }
    set_pwd_env();
    return 0;
}

/* Get directory to chdir to */

/**/
static LinkNode
cd_get_dest(char *nam, char **argv, char *ops, int func)
{
    LinkNode dir = NULL;
    LinkNode target;
    char *dest;

    if (!argv[0]) {
	if (func == BIN_POPD && !nextnode(firstnode(dirstack))) {
	    zwarnnam(nam, "directory stack empty", NULL, 0);
	    return NULL;
	}
	if (func == BIN_PUSHD && unset(PUSHDTOHOME))
	    dir = nextnode(firstnode(dirstack));
	if (dir)
	    insertlinknode(dirstack, dir, getlinknode(dirstack));
	else if (func != BIN_POPD)
	    pushnode(dirstack, ztrdup(home));
    } else if (!argv[1]) {
	int dd;
	char *end;

	doprintdir++;
	if (argv[0][1] && (argv[0][0] == '+' || argv[0][0] == '-')) {
	    dd = zstrtol(argv[0] + 1, &end, 10); 
	    if (*end == '\0') {
		if ((argv[0][0] == '+') ^ isset(PUSHDMINUS))
		    for (dir = firstnode(dirstack); dir && dd; dd--, incnode(dir));
		else
		    for (dir = lastnode(dirstack); dir != (LinkNode) dirstack && dd;
			 dd--, dir = prevnode(dir)); 
		if (!dir || dir == (LinkNode) dirstack) {
		    zwarnnam(nam, "no such entry in dir stack", NULL, 0);
		    return NULL;
		}
	    }
	}
	if (!dir)
	    pushnode(dirstack, ztrdup(strcmp(argv[0], "-")
				      ? (doprintdir--, argv[0]) : oldpwd));
    } else {
	char *u, *d;
	int len1, len2, len3;

	if (!(u = strstr(pwd, argv[0]))) {
	    zwarnnam(nam, "string not in pwd: %s", argv[0], 0);
	    return NULL;
	}
	len1 = strlen(argv[0]);
	len2 = strlen(argv[1]);
	len3 = u - pwd;
	d = (char *)zalloc(len3 + len2 + strlen(u + len1) + 1);
	strncpy(d, pwd, len3);
	strcpy(d + len3, argv[1]);
	strcat(d, u + len1);
	pushnode(dirstack, d);
	doprintdir++;
    }

    target = dir;
    if (func == BIN_POPD) {
	if (!dir) {
	    target = dir = firstnode(dirstack);
	} else if (dir != firstnode(dirstack)) {
	    return dir;
	}
	dir = nextnode(dir);
    }
    if (!dir) {
	dir = firstnode(dirstack);
    }
    if (!(dest = cd_do_chdir(nam, getdata(dir), ops['s']))) {
	if (!target)
	    zsfree(getlinknode(dirstack));
	if (func == BIN_POPD)
	    zsfree(remnode(dirstack, dir));
	return NULL;
    }
    if (dest != getdata(dir)) {
	zsfree(getdata(dir));
	setdata(dir, dest);
    }
    return target ? target : dir;
}

/* Change to given directory, if possible.  This function works out  *
 * exactly how the directory should be interpreted, including cdpath *
 * and CDABLEVARS.  For each possible interpretation of the given    *
 * path, this calls cd_try_chdir(), which attempts to chdir to that  *
 * particular path.                                                  */

/**/
static char *
cd_do_chdir(char *cnam, char *dest, int hard)
{
    char **pp, *ret;
    int hasdot = 0, eno = ENOENT;
    /* nocdpath indicates that cdpath should not be used.  This is the case iff
    dest is a relative path whose first segment is . or .., but if the path is
    absolute then cdpath won't be used anyway. */
    int nocdpath = dest[0] == '.' &&
    (dest[1] == '/' || !dest[1] || (dest[1] == '.' &&
				    (dest[2] == '/' || !dest[2])));

    /* if we have an absolute path, use it as-is only */
    if (*dest == '/') {
	if ((ret = cd_try_chdir(NULL, dest, hard)))
	    return ret;
	zwarnnam(cnam, "%e: %s", dest, errno);
	return NULL;
    }

    /* if cdpath is being used, check it for . */
    if (!nocdpath)
	for (pp = cdpath; *pp; pp++)
	    if (!(*pp)[0] || ((*pp)[0] == '.' && (*pp)[1] == '\0'))
		hasdot = 1;
    /* if there is no . in cdpath (or it is not being used), try the directory
    as-is (i.e. from .) */
    if (!hasdot) {
	if ((ret = cd_try_chdir(NULL, dest, hard)))
	    return ret;
	if (errno != ENOENT)
	    eno = errno;
    }
    /* if cdpath is being used, try given directory relative to each element in
    cdpath in turn */
    if (!nocdpath)
	for (pp = cdpath; *pp; pp++) {
	    if ((ret = cd_try_chdir(*pp, dest, hard))) {
		if (strcmp(*pp, ".")) {
		    doprintdir++;
		}
		return ret;
	    }
	    if (errno != ENOENT)
		eno = errno;
	}

    /* handle the CDABLEVARS option */
    if ((ret = cd_able_vars(dest))) {
	if ((ret = cd_try_chdir(NULL, ret,hard))) {
	    doprintdir++;
	    return ret;
	}
	if (errno != ENOENT)
	    eno = errno;
    }

    /* If we got here, it means that we couldn't chdir to any of the
    multitudinous possible paths allowed by zsh.  We've run out of options!
    Add more here! */
    zwarnnam(cnam, "%e: %s", dest, eno);
    return NULL;
}

/* If the CDABLEVARS option is set, return the new *
 * interpretation of the given path.               */

/**/
char *
cd_able_vars(char *s)
{
    char *rest, save;

    if (isset(CDABLEVARS)) {
	for (rest = s; *rest && *rest != '/'; rest++);
	save = *rest;
	*rest = 0;
	s = getnameddir(s);
	*rest = save;

	if (s && *rest)
	    s = dyncat(s, rest);

	return s;
    }
    return NULL;
}

/* Attempt to change to a single given directory.  The directory,    *
 * for the convenience of the calling function, may be provided in   *
 * two parts, which must be concatenated before attempting to chdir. *
 * Returns NULL if the chdir fails.  If the directory change is      *
 * possible, it is performed, and a pointer to the new full pathname *
 * is returned.                                                      */

/**/
static char *
cd_try_chdir(char *pfix, char *dest, int hard)
{
    char *buf;

    /* handle directory prefix */
    if (pfix && *pfix) {
	if (*pfix == '/')
	    buf = tricat(pfix, "/", dest);
	else {
	    int pwl = strlen(pwd);
	    int pfl = strlen(pfix);

	    buf = zalloc(pwl + pfl + strlen(dest) + 3);
	    strcpy(buf, pwd);
	    buf[pwl] = '/';
	    strcpy(buf + pwl + 1, pfix);
	    buf[pwl + 1 + pfl] = '/';
	    strcpy(buf + pwl + pfl + 2, dest);
	}
    } else if (*dest == '/')
	buf = ztrdup(dest);
    else {
	int pwl = strlen(pwd);

	buf = zalloc(pwl + strlen(dest) + 2);
	strcpy(buf, pwd);
	buf[pwl] = '/';
	strcpy(buf + pwl + 1, dest);
    }

    /* Normalise path.  See the definition of fixdir() for what this means. */
    fixdir(buf);

    if (lchdir(buf, NULL, hard)) {
	zsfree(buf);
	return NULL;
    }
    return metafy(buf, -1, META_NOALLOC);
}

/* do the extra processing associated with changing directory */

/**/
static void
cd_new_pwd(int func, LinkNode dir, int chaselinks)
{
    Param pm;
    List l;
    char *new_pwd, *s;
    int dirstacksize;

    if (func == BIN_PUSHD)
	rolllist(dirstack, dir);
    new_pwd = remnode(dirstack, dir);

    if (func == BIN_POPD && firstnode(dirstack)) {
	zsfree(new_pwd);
	new_pwd = getlinknode(dirstack);
    } else if (func == BIN_CD && unset(AUTOPUSHD))
	zsfree(getlinknode(dirstack));

    if (chaselinks) {
	s = new_pwd;
	new_pwd = findpwd(s);
	zsfree(s);
    }
    if (isset(PUSHDIGNOREDUPS)) {
	LinkNode n; 
	for (n = firstnode(dirstack); n; incnode(n)) {
	    if (!strcmp(new_pwd, getdata(n))) {
		zsfree(remnode(dirstack, n));
		break;
	    }
	}
    }

    /* shift around the pwd variables, to make oldpwd and pwd relate to the
    current (i.e. new) pwd */
    zsfree(oldpwd);
    oldpwd = pwd;
    pwd = new_pwd;
    /* update the PWD and OLDPWD shell parameters */
    if ((pm = (Param) paramtab->getnode(paramtab, "PWD")) &&
	(pm->flags & PM_EXPORTED) && pm->env)
	pm->env = replenv(pm->env, pwd);
    if ((pm = (Param) paramtab->getnode(paramtab, "OLDPWD")) &&
	(pm->flags & PM_EXPORTED) && pm->env)
	pm->env = replenv(pm->env, oldpwd);
    if (unset(PUSHDSILENT) && func != BIN_CD && isset(INTERACTIVE))
	printdirstack();
    else if (doprintdir) {
	fprintdir(pwd, stdout);
        putchar('\n');
    }

    /* execute the chpwd function */
    if ((l = getshfunc("chpwd")) != &dummy_list) {
	fflush(stdout);
	fflush(stderr);
	doshfunc(l, NULL, 0, 1);
    }

    dirstacksize = getiparam("DIRSTACKSIZE");
    /* handle directory stack sizes out of range */
    if (dirstacksize > 0) {
	int remove = countlinknodes(dirstack) -
		     (dirstacksize < 2 ? 2 : dirstacksize);
	while (remove-- >= 0)
	    zsfree(remnode(dirstack, lastnode(dirstack)));
    }
}

/* Print the directory stack */

/**/
static void
printdirstack(void)
{
    LinkNode node;

    fprintdir(pwd, stdout);
    for (node = firstnode(dirstack); node; incnode(node)) {
	putchar(' ');
	fprintdir(getdata(node), stdout);
    }
    putchar('\n');
}

/* Normalise a path.  Segments consisting of ., and foo/.. *
 * combinations, are removed and the path is unmetafied.   */

/**/
static void
fixdir(char *src)
{
    char *dest = src;
    char *d0 = dest;

/*** if have RFS superroot directory ***/
#ifdef HAVE_SUPERROOT
    /* allow /.. segments to remain */
    while (*src == '/' && src[1] == '.' && src[2] == '.' &&
      (!src[3] || src[3] == '/')) {
	*dest++ = '/';
	*dest++ = '.';
	*dest++ = '.';
	src += 3;
    }
#endif

    for (;;) {
	/* compress multiple /es into single */
	if (*src == '/') {
	    *dest++ = *src++;
	    while (*src == '/')
		src++;
	}
	/* if we are at the end of the input path, remove a trailing / (if it
	exists), and return ct */
	if (!*src) {
	    while (dest > d0 + 1 && dest[-1] == '/')
		dest--;
	    *dest = '\0';
	    return;
	}
	if (dest > d0 + 1 && src[0] == '.' && src[1] == '.' &&
	  (src[2] == '\0' || src[2] == '/')) {
	    /* remove a foo/.. combination */
	    for (dest--; dest > d0 + 1 && dest[-1] != '/'; dest--);
	    if (dest[-1] != '/')
		dest--;
	    src++;
	    while (*++src == '/');
	} else if (src[0] == '.' && (src[1] == '/' || src[1] == '\0')) {
	    /* skip a . section */
	    while (*++src == '/');
	} else {
	    /* copy a normal segment into the output */
	    while (*src != '/' && *src != '\0')
		if ((*dest++ = *src++) == Meta)
		    dest[-1] = *src++ ^ 32;
	}
    }
}

/**/
void
printqt(char *str)
{
    /* Print str, but turn any single quote into '\'' or ''. */
    for (; *str; str++)
	if (*str == '\'')
	    printf(isset(RCQUOTES) ? "''" : "'\\''");
	else
	    putchar(*str);
}

/**/
void
printif(char *str, int c)
{
    /* If flag c has an argument, print that */
    if (str) {
	printf(" -%c ", c);
	quotedzputs(str, stdout);
    }
}

/**** history list functions ****/

/* fc, history, r */

/**/
int
bin_fc(char *nam, char **argv, char *ops, int func)
{
    int first = -1, last = -1, retval, minflag = 0;
    char *s;
    struct asgment *asgf = NULL, *asgl = NULL;
    Comp com = NULL;

    /* fc is only permitted in interactive shells */
    if (!interact) {
	zwarnnam(nam, "not interactive shell", NULL, 0);
	return 1;
    }
    /* with the -m option, the first argument is taken *
     * as a pattern that history lines have to match   */
    if (*argv && ops['m']) {
	tokenize(*argv);
	if (!(com = parsereg(*argv++))) {
	    zwarnnam(nam, "invalid match pattern", NULL, 0);
	    return 1;
	}
    }
    if (ops['R']) {
	/* read history from a file */
	readhistfile(*argv ? *argv : getsparam("HISTFILE"), 1);
	return 0;
    }
    if (ops['W']) {
	/* write history to a file */
	savehistfile(*argv ? *argv : getsparam("HISTFILE"), 1,
		     (ops['I'] ? 2 : 0));
	return 0;
    }
    if (ops['A']) {
	/* append history to a file */
	savehistfile(*argv ? *argv : getsparam("HISTFILE"), 1,
		     (ops['I'] ? 3 : 1));
	return 0;
    }
    if (!(ops['l'] && unset(HISTNOSTORE)))
	remhist();
    /* put foo=bar type arguments into the substitution list */
    while (*argv && equalsplit(*argv, &s)) {
	Asgment a = (Asgment) alloc(sizeof *a);

	if (!asgf)
	    asgf = asgl = a;
	else {
	    asgl->next = a;
	    asgl = a;
	}
	a->name = *argv;
	a->value = s;
	argv++;
    }
    /* interpret and check first history line specifier */
    if (*argv) {
	minflag = **argv == '-';
	first = fcgetcomm(*argv);
	if (first == -1)
	    return 1;
	argv++;
    }
    /* interpret and check second history line specifier */
    if (*argv) {
	last = fcgetcomm(*argv);
	if (last == -1)
	    return 1;
	argv++;
    }
    /* There is a maximum of two history specifiers.  At least, there *
     * will be as long as the history list is one-dimensional.        */
    if (*argv) {
	zwarnnam("fc", "too many arguments", NULL, 0);
	return 1;
    }
    /* default values of first and last, and range checking */
    if (first == -1)
	first = (ops['l']) ? curhist - 16 : curhist - 1;
    if (last == -1)
	last = (ops['l']) ? curhist - 1 : first;
    if (first < firsthist())
	first = firsthist();
    if (last == -1)
	last = (minflag) ? curhist : first;
    else if (last < first)
	last = first;
    if (ops['l'])
	/* list the required part of the history */
	retval = fclist(stdout, !ops['n'], ops['r'], ops['D'],
			ops['d'] + ops['f'] * 2 + ops['E'] * 4 + ops['i'] * 8,
			first, last, asgf, com);
    else {
	/* edit history file, and (if successful) use the result as a new command */
	int tempfd;
	FILE *out;
	char *fil;

	retval = 1;
	fil = gettempname();
	if (((tempfd = open(fil, O_WRONLY | O_CREAT | O_EXCL | O_NOCTTY, 0600))
	    == -1) ||
		((out = fdopen(tempfd, "w")) == NULL)) {
	    zwarnnam("fc", "can't open temp file: %e", NULL, errno);
	} else {
	    if (!fclist(out, 0, ops['r'], 0, 0, first, last, asgf, com)) {
		char *editor;

		editor = auxdata ? auxdata : getsparam("FCEDIT");
		if (!editor)
		    editor = DEFAULT_FCEDIT;

		if (fcedit(editor, fil))
		    if (stuff(fil))
			zwarnnam("fc", "%e: %s", s, errno);
		    else {
			loop(0,1);
			retval = lastval;
		    }
	    }
	}
	unlink(fil);
    }
    return retval;
}

/* History handling functions: these are called by ZLE, as well as  *
 * the actual builtins.  fcgetcomm() gets a history line, specified *
 * either by number or leading string.  fcsubs() performs a given   *
 * set of simple old=new substitutions on a given command line.     *
 * fclist() outputs a given range of history lines to a text file.  */

/* get the history event associated with s */

/**/
static int
fcgetcomm(char *s)
{
    int cmd;

    /* First try to match a history number.  Negative *
     * numbers indicate reversed numbering.           */
    if ((cmd = atoi(s))) {
	if (cmd < 0)
	    cmd = curhist + cmd;
	if (cmd >= curhist) {
	    zwarnnam("fc", "bad history number: %d", 0, cmd);
	    return -1;
	}
	return cmd;
    }
    /* not a number, so search by string */
    cmd = hcomsearch(s);
    if (cmd == -1)
	zwarnnam("fc", "event not found: %s", s, 0);
    return cmd;
}

/* Perform old=new substituions.  Uses the asgment structure from zsh.h, *
 * which is essentially a linked list of string,replacement pairs.       */

/**/
static int
fcsubs(char **sp, struct asgment *sub)
{
    char *oldstr, *newstr, *oldpos, *newpos, *newmem, *s = *sp;
    int subbed = 0;

    /* loop through the linked list */
    while (sub) {
	oldstr = sub->name;
	newstr = sub->value;
	sub = sub->next;
	oldpos = s;
	/* loop over occurences of oldstr in s, replacing them with newstr */
	while ((newpos = (char *)strstr(oldpos, oldstr))) {
	    newmem = (char *) alloc(1 + (newpos - s)
			+ strlen(newstr) + strlen(newpos + strlen(oldstr)));
	    ztrncpy(newmem, s, newpos - s);
	    strcat(newmem, newstr);
	    oldpos = newmem + strlen(newmem);
	    strcat(newmem, newpos + strlen(oldstr));
	    s = newmem;
	    subbed = 1;
	}
    }
    *sp = s;
    return subbed;
}

/* Print a series of history events to a file.  The file pointer is     *
 * given by f, and the required range of events by first and last.      *
 * subs is an optional list of foo=bar substitutions to perform on the  *
 * history lines before output.  com is an optional comp structure      *
 * that the history lines are required to match.  n, r, D and d are     *
 * options: n indicates that each line should be numbered.  r indicates *
 * that the lines should be output in reverse order (newest first).     *
 * D indicates that the real time taken by each command should be       *
 * output.  d indicates that the time of execution of each command      *
 * should be output; d>1 means that the date should be output too; d>3  *
 * means that mm/dd/yyyy form should be used for the dates, as opposed  *
 * to dd.mm.yyyy form; d>7 means that yyyy-mm-dd form should be used.   */

/**/
static int
fclist(FILE *f, int n, int r, int D, int d, int first, int last, struct asgment *subs, Comp com)
{
    int fclistdone = 0;
    char *s, *hs;
    Histent ent;

    /* reverse range if required */
    if (r) {
	r = last;
	last = first;
	first = r;
    }
    /* suppress "no substitution" warning if no substitution is requested */
    if (!subs)
	fclistdone = 1;

    for (;;) {
	hs = quietgetevent(first);
	if (!hs) {
	    zwarnnam("fc", "no such event: %d", NULL, first);
	    return 1;
	}
	s = dupstring(hs);
	/* this if does the pattern matching, if required */
	if (!com || domatch(s, com, 0)) {
	    /* perform substitution */
	    fclistdone |= fcsubs(&s, subs);

	    /* do numbering */
	    if (n)
		fprintf(f, "%5d  ", first);
	    ent = NULL;
	    /* output actual time (and possibly date) of execution of the
	    command, if required */
	    if (d) {
		struct tm *ltm;
		if (!ent)
		    ent = gethistent(first);
		ltm = localtime(&ent->stim);
		if (d >= 2) {
		    if (d >= 8) {
			fprintf(f, "%d-%02d-%02d ",
				ltm->tm_year + 1900,
				ltm->tm_mon + 1, ltm->tm_mday);
		    } else if (d >= 4) {
			fprintf(f, "%d.%d.%d ",
				ltm->tm_mday, ltm->tm_mon + 1,
				ltm->tm_year + 1900);
		    } else {
			fprintf(f, "%d/%d/%d ",
				ltm->tm_mon + 1, ltm->tm_mday,
				ltm->tm_year + 1900);
		    }
		}
		fprintf(f, "%02d:%02d  ", ltm->tm_hour, ltm->tm_min);
	    }
	    /* display the time taken by the command, if required */
	    if (D) {
		long diff;
		if (!ent)
		    ent = gethistent(first);
		diff = (ent->ftim) ? ent->ftim - ent->stim : 0;
		fprintf(f, "%ld:%02ld  ", diff / 60, diff % 60);
	    }

	    /* output the command */
	    if (f == stdout) {
		nicezputs(s, f);
		putc('\n', f);
	    } else
		fprintf(f, "%s\n", s);
	}
	/* move on to the next history line, or quit the loop */
	if (first == last)
	    break;
	else if (first > last)
	    first--;
	else
	    first++;
    }

    /* final processing */
    if (f != stdout)
	fclose(f);
    if (!fclistdone) {
	zwarnnam("fc", "no substitutions performed", NULL, 0);
	return 1;
    }
    return 0;
}

/* edit a history file */

/**/
static int
fcedit(char *ename, char *fn)
{
    char *s;

    if (!strcmp(ename, "-"))
	return 1;

    s = tricat(ename, " ", fn);
    execstring(s, 1, 0);
    zsfree(s);

    return !lastval;
}

/**** parameter builtins ****/

/* Separate an argument into name=value parts, returning them in an     *
 * asgment structure.  Because the asgment structure used is global,    *
 * only one of these can be active at a time.  The string s gets placed *
 * in this global structure, so it needs to be in permanent memory.     */

/**/
static Asgment
getasg(char *s)
{
    static struct asgment asg;

    /* sanity check for valid argument */
    if (!s)
	return NULL;

    /* check if name is empty */
    if (*s == '=') {
	zerr("bad assignment", NULL, 0);
	return NULL;
    }
    asg.name = s;

    /* search for `=' */
    for (; *s && *s != '='; s++);

    /* found `=', so return with a value */
    if (*s) {
	*s = '\0';
	asg.value = s + 1;
    } else {
    /* didn't find `=', so we only have a name */
	asg.value = NULL;
    }
    return &asg;
}

/* declare, export, integer, local, readonly, typeset */

/**/
int
bin_typeset(char *name, char **argv, char *ops, int func)
{
    Param pm;
    Asgment asg;
    Comp com;
    char *optstr = "iLRZlurtxU";
    int on = 0, off = 0, roff, bit = PM_INTEGER;
    int initon, initoff, of, i;
    int returnval = 0, printflags = 0;

    /* hash -f is really the builtin `functions' */
    if (ops['f'])
	return bin_functions(name, argv, ops, func);

    /* Translate the options into PM_* flags.   *
     * Unfortunately, this depends on the order *
     * these flags are defined in zsh.h         */
    for (; *optstr; optstr++, bit <<= 1)
	if (ops[*optstr] == 1)
	    on |= bit;
	else if (ops[*optstr] == 2)
	    off |= bit;
    roff = off;

    /* Sanity checks on the options.  Remove conficting options. */
    if (on & PM_INTEGER)
	off |= PM_RIGHT_B | PM_LEFT | PM_RIGHT_Z | PM_UPPER | PM_ARRAY;
    if (on & PM_LEFT)
	off |= PM_RIGHT_B | PM_INTEGER;
    if (on & PM_RIGHT_B)
	off |= PM_LEFT | PM_INTEGER;
    if (on & PM_RIGHT_Z)
	off |= PM_INTEGER;
    if (on & PM_UPPER)
	off |= PM_LOWER;
    if (on & PM_LOWER)
	off |= PM_UPPER;
    on &= ~off;

    /* Given no arguments, list whatever the options specify. */
    if (!*argv) {
	if (!(on|roff))
	    printflags |= PRINT_TYPE;
	if (roff || ops['+'])
	    printflags |= PRINT_NAMEONLY;
	scanhashtable(paramtab, 1, on|roff, 0, paramtab->printnode, printflags);
	return 0;
    }

    /* With the -m option, treat arguments as glob patterns */
    if (ops['m']) {
	while ((asg = getasg(*argv++))) {
	    tokenize(asg->name);   /* expand argument */
	    if (!(com = parsereg(asg->name))) {
		untokenize(asg->name);
		zwarnnam(name, "bad pattern : %s", argv[-1], 0);
		returnval = 1;
		continue;
	    }
	    /* If no options or values are given, display all *
	     * parameters matching the glob pattern.          */
	    if (!(on || roff || asg->value)) {
		scanmatchtable(paramtab, com, 0, 0, paramtab->printnode, 0);
		continue;
	    }
	    /* Since either options or values are given, we search   *
	     * through the parameter table and change all parameters *
	     * matching the glob pattern to have these flags and/or  *
	     * value.                                                */
	    for (i = 0; i < paramtab->hsize; i++) {
		for (pm = (Param) paramtab->nodes[i]; pm; pm = (Param) pm->next) {
		    if ((pm->flags & PM_RESTRICTED) && isset(RESTRICTED))
			continue;
		    if (domatch(pm->nam, com, 0)) {
			/* set up flags if we have any */
			if (on || roff) {
			    if (PM_TYPE(pm->flags) == PM_ARRAY && (on & PM_UNIQUE) &&
				!(pm->flags & PM_READONLY & ~off))
				uniqarray((*pm->gets.afn) (pm));
			    pm->flags = (pm->flags | on) & ~off;
			    if (PM_TYPE(pm->flags) != PM_ARRAY) {
				if ((on & (PM_LEFT | PM_RIGHT_B | PM_RIGHT_Z | PM_INTEGER)) && auxlen)
				    pm->ct = auxlen;
				/* did we just export this? */
				if ((pm->flags & PM_EXPORTED) && !pm->env) {
				    pm->env = addenv(pm->nam, (asg->value) ? asg->value : getsparam(pm->nam));
				} else if (!(pm->flags & PM_EXPORTED) && pm->env) {
				/* did we just unexport this? */
				    delenv(pm->env);
				    zsfree(pm->env);
				    pm->env = NULL;
				}
			    }
			}
			/* set up a new value if given */
			if (asg->value) {
			    setsparam(pm->nam, ztrdup(asg->value));
			}
		    }
		}
	    }
	}
	return returnval;
    }

    /* Save the values of on, off, and func */
    initon = on;
    initoff = off;
    of = func;

    /* Take arguments literally.  Don't glob */
    while ((asg = getasg(*argv++))) {
	/* restore the original values of on, off, and func */
	on = initon;
	off = initoff;
	func = of;
	on &= ~PM_ARRAY;

	/* check if argument is a valid identifier */
	if (!isident(asg->name)) {
	    zerr("not an identifier: %s", asg->name, 0);
	    returnval = 1;
	    continue;
	}
	bit = 0;    /* flag for switching int<->not-int */
	if ((pm = (Param)paramtab->getnode(paramtab, asg->name)) &&
	    (((pm->flags & PM_SPECIAL) && pm->level == locallevel) ||
	     (!(pm->flags & PM_UNSET) &&
	      ((locallevel == pm->level) || func == BIN_EXPORT) &&
	      !(bit = ((off & pm->flags) | (on & ~pm->flags)) & PM_INTEGER)))) {
	    /* if no flags or values are given, just print this parameter */
	    if (!on && !roff && !asg->value) {
		paramtab->printnode((HashNode) pm, 0);
		continue;
	    }
	    if ((pm->flags & PM_RESTRICTED) && isset(RESTRICTED)) {
		zerrnam(name, "%s: restricted", pm->nam, 0);
		returnval = 1;
		continue;
	    }
	    if((pm->flags & PM_SPECIAL) &&
	       PM_TYPE((pm->flags | on) & ~off) != PM_TYPE(pm->flags)) {
		zerrnam(name, "%s: cannot change type of a special parameter",
		    pm->nam, 0);
		returnval = 1;
		continue;
	    }
	    if (PM_TYPE(pm->flags) == PM_ARRAY && (on & PM_UNIQUE) &&
		!(pm->flags & PM_READONLY & ~off))
		uniqarray((*pm->gets.afn) (pm));
	    pm->flags = (pm->flags | on) & ~off;
	    if ((on & (PM_LEFT | PM_RIGHT_B | PM_RIGHT_Z | PM_INTEGER)) &&
		auxlen)
		pm->ct = auxlen;
	    if (PM_TYPE(pm->flags) != PM_ARRAY) {
		if (pm->flags & PM_EXPORTED) {
		    if (!(pm->flags & PM_UNSET) && !pm->env && !asg->value)
			pm->env = addenv(asg->name, getsparam(asg->name));
		} else if (pm->env) {
		    delenv(pm->env);
		    zsfree(pm->env);
		    pm->env = NULL;
		}
		if (asg->value)
		    setsparam(asg->name, ztrdup(asg->value));
	    }
	} else {
	    if (bit) {
		if (pm->flags & PM_READONLY) {
		    on |= ~off & PM_READONLY;
		    pm->flags &= ~PM_READONLY;
		}
		if (!asg->value)
		    asg->value = dupstring(getsparam(asg->name));
		unsetparam(asg->name);
	    }
	    /* create a new node for a parameter with the *
	     * flags in `on' minus the readonly flag      */
	    pm = createparam(ztrdup(asg->name), on & ~PM_READONLY);
	    DPUTS(!pm, "BUG: parameter not created");
	    pm->ct = auxlen;
	    if (func != BIN_EXPORT)
		pm->level = locallevel;
	    if (asg->value)
		setsparam(asg->name, ztrdup(asg->value));
	    pm->flags |= (on & PM_READONLY);
	}
    }
    return returnval;
}

/* Display or change the attributes of shell functions.   *
 * If called as autoload, it will define a new autoloaded *
 * (undefined) shell function.                            */

/**/
int
bin_functions(char *name, char **argv, char *ops, int func)
{
    Comp com;
    Shfunc shf;
    int i, returnval = 0;
    int on = 0, off = 0;

    /* Do we have any flags defined? */
    if (ops['u'] || ops['t']) {
	if (ops['u'] == 1)
	    on |= PM_UNDEFINED;
	else if (ops['u'] == 2)
	    off |= PM_UNDEFINED;

	if (ops['t'] == 1)
	    on |= PM_TAGGED;
	else if (ops['t'] == 2)
	    off |= PM_TAGGED;
    }

    if (off & PM_UNDEFINED) {
	zwarnnam(name, "invalid option(s)", NULL, 0);
	return 1;
    }

    /* If no arguments given, we will print functions.  If flags *
     * are given, we will print only functions containing these  *
     * flags, else we'll print them all.                         */
    if (!*argv) {
	scanhashtable(shfunctab, 1, on|off, DISABLED, shfunctab->printnode, 0);
	return 0;
    }

    /* With the -m option, treat arguments as glob patterns */
    if (ops['m']) {
	on &= ~PM_UNDEFINED;
	for (; *argv; argv++) {
	    /* expand argument */
	    tokenize(*argv);
	    if ((com = parsereg(*argv))) {
		/* with no options, just print all functions matching the glob pattern */
		if (!(on|off)) {
		    scanmatchtable(shfunctab, com, 0, DISABLED, shfunctab->printnode, 0);
		} else {
		/* apply the options to all functions matching the glob pattern */
		    for (i = 0; i < shfunctab->hsize; i++) {
			for (shf = (Shfunc) shfunctab->nodes[i]; shf; shf = (Shfunc) shf->next)
			    if (domatch(shf->nam, com, 0) && !(shf->flags & DISABLED))
				shf->flags = (shf->flags | on) & (~off);
		    }
		}
	    } else {
		untokenize(*argv);
		zwarnnam(name, "bad pattern : %s", *argv, 0);
		returnval = 1;
	    }
	}
	return returnval;
    }

    /* Take the arguments literally -- do not glob */
    for (; *argv; argv++) {
	if ((shf = (Shfunc) shfunctab->getnode(shfunctab, *argv))) {
	    /* if any flag was given */
	    if (on|off)
		/* turn on/off the given flags */
		shf->flags = (shf->flags | (on & ~PM_UNDEFINED)) & ~off;
	    else
		/* no flags, so just print */
		shfunctab->printnode((HashNode) shf, 0);
	} else if (on & PM_UNDEFINED) {
	    /* Add a new undefined (autoloaded) function to the *
	     * hash table with the corresponding flags set.     */
	    shf = (Shfunc) zcalloc(sizeof *shf);
	    shf->flags = on;
	    shf->funcdef = mkautofn(shf);
	    shfunctab->addnode(shfunctab, ztrdup(*argv), shf);
	} else
	    returnval = 1;
    }
    return returnval;
}

/**/
static List
mkautofn(Shfunc shf)
{
    List l;
    Sublist s;
    Pline p;
    Cmd c;
    AutoFn a;
    PERMALLOC {
	a = (AutoFn)allocnode(N_AUTOFN);
	a->shf = shf;
	c = (Cmd)allocnode(N_CMD);
	c->type = AUTOFN;
	c->u.autofn = a;
	p = (Pline)allocnode(N_PLINE);
	p->left = c;
	p->type = END;
	s = (Sublist)allocnode(N_SUBLIST);
	s->left = p;
	l = (List)allocnode(N_LIST);
	l->left = s;
	l->type = Z_SYNC;
    } LASTALLOC;
    return l;
}

/* unset: unset parameters */

/**/
int
bin_unset(char *name, char **argv, char *ops, int func)
{
    Param pm, next;
    Comp com;
    char *s;
    int match = 0, returnval = 0;
    int i;

    /* unset -f is the same as unfunction */
    if (ops['f'])
	return bin_unhash(name, argv, ops, func);

    /* with -m option, treat arguments as glob patterns */
    if (ops['m']) {
	while ((s = *argv++)) {
	    /* expand */
	    tokenize(s);
	    if ((com = parsereg(s))) {
		/* Go through the parameter table, and unset any matches */
		for (i = 0; i < paramtab->hsize; i++) {
		    for (pm = (Param) paramtab->nodes[i]; pm; pm = next) {
			/* record pointer to next, since we may free this one */
			next = (Param) pm->next;
			if ((!(pm->flags & PM_RESTRICTED) ||
			    unset(RESTRICTED)) && domatch(pm->nam, com, 0)) {
			    unsetparam(pm->nam);
			    match++;
			}
		    }
		}
	    } else {
		untokenize(s);
		zwarnnam(name, "bad pattern : %s", s, 0);
		returnval = 1;
	    }
	}
	/* If we didn't match anything, we return 1. */
	if (!match)
	    returnval = 1;
	return returnval;
    }

    /* do not glob -- unset the given parameter */
    while ((s = *argv++)) {
	pm = (Param) paramtab->getnode(paramtab, s);
	if (!pm)
	    returnval = 1;
	else if ((pm->flags & PM_RESTRICTED) && isset(RESTRICTED)) {
	    zerrnam(name, "%s: restricted", pm->nam, 0);
	    returnval = 1;
	} else
	    unsetparam(s);
    }
    return returnval;
}

/* type, whence, which */

/**/
int
bin_whence(char *nam, char **argv, char *ops, int func)
{
    HashNode hn;
    Comp com;
    int returnval = 0;
    int printflags = 0;
    int csh, all, v, wd;
    int informed;
    char *cnam;

    /* Check some option information */
    csh = ops['c'];
    v   = ops['v'];
    all = ops['a'];
    wd  = ops['w'];

    if (ops['w'])
	printflags |= PRINT_WHENCE_WORD;
    else if (ops['c'])
	printflags |= PRINT_WHENCE_CSH;
    else if (ops['v'])
	printflags |= PRINT_WHENCE_VERBOSE;
    else
	printflags |= PRINT_WHENCE_SIMPLE;
    if (ops['f'])
	printflags |= PRINT_WHENCE_FUNCDEF;

    /* With -m option -- treat arguments as a glob patterns */
    if (ops['m']) {
	for (; *argv; argv++) {
	    /* parse the pattern */
	    tokenize(*argv);
	    if (!(com = parsereg(*argv))) {
		untokenize(*argv);
		zwarnnam(nam, "bad pattern : %s", *argv, 0);
		returnval = 1;
		continue;
	    }
	    if (!ops['p']) {
		/* -p option is for path search only.    *
		 * We're not using it, so search for ... */

		/* aliases ... */
		scanmatchtable(aliastab, com, 0, DISABLED, aliastab->printnode, printflags);

		/* and reserved words ... */
		scanmatchtable(reswdtab, com, 0, DISABLED, reswdtab->printnode, printflags);

		/* and shell functions... */
		scanmatchtable(shfunctab, com, 0, DISABLED, shfunctab->printnode, printflags);

		/* and builtins. */
		scanmatchtable(builtintab, com, 0, DISABLED, builtintab->printnode, printflags);
	    }
	    /* Done search for `internal' commands, if the -p option *
	     * was not used.  Now search the path.                   */
	    cmdnamtab->filltable(cmdnamtab);
	    scanmatchtable(cmdnamtab, com, 0, 0, cmdnamtab->printnode, printflags);

	}
    return returnval;
    }

    /* Take arguments literally -- do not glob */
    for (; *argv; argv++) {
	informed = 0;

	if (!ops['p']) {
	    /* Look for alias */
	    if ((hn = aliastab->getnode(aliastab, *argv))) {
		aliastab->printnode(hn, printflags);
		if (!all)
		    continue;
		informed = 1;
	    }
	    /* Look for reserved word */
	    if ((hn = reswdtab->getnode(reswdtab, *argv))) {
		reswdtab->printnode(hn, printflags);
		if (!all)
		    continue;
		informed = 1;
	    }
	    /* Look for shell function */
	    if ((hn = shfunctab->getnode(shfunctab, *argv))) {
		shfunctab->printnode(hn, printflags);
		if (!all)
		    continue;
		informed = 1;
	    }
	    /* Look for builtin command */
	    if ((hn = builtintab->getnode(builtintab, *argv))) {
		builtintab->printnode(hn, printflags);
		if (!all)
		    continue;
		informed = 1;
	    }
	    /* Look for commands that have been added to the *
	     * cmdnamtab with the builtin `hash foo=bar'.    */
	    if ((hn = cmdnamtab->getnode(cmdnamtab, *argv)) && (hn->flags & HASHED)) {
		cmdnamtab->printnode(hn, printflags);
		if (!all)
		    continue;
		informed = 1;
	    }
	}

	/* Option -a is to search the entire path, *
	 * rather than just looking for one match. */
	if (all) {
	    char **pp, buf[PATH_MAX], *z;

	    for (pp = path; *pp; pp++) {
		z = buf;
		if (**pp) {
		    strucpy(&z, *pp);
		    *z++ = '/';
		}
		if ((z - buf) + strlen(*argv) >= PATH_MAX)
		    continue;
		strcpy(z, *argv);
		if (iscom(buf)) {
		    if (wd) {
			printf("%s: command\n", *argv);
		    } else {
			if (v && !csh)
			    zputs(*argv, stdout), fputs(" is ", stdout);
			zputs(buf, stdout);
			if (ops['s'])
			    print_if_link(buf);
			fputc('\n', stdout);
		    }
		    informed = 1;
		}
	    }
	    if (!informed && (wd || v || csh)) {
		zputs(*argv, stdout);
		puts(wd ? ": none" : " not found");
		returnval = 1;
	    }
	} else if ((cnam = findcmd(*argv))) {
	    /* Found external command. */
	    if (wd) {
		printf("%s: command\n", *argv);
	    } else {
		if (v && !csh)
		    zputs(*argv, stdout), fputs(" is ", stdout);
		zputs(cnam, stdout);
		if (ops['s'])
		    print_if_link(cnam);
		fputc('\n', stdout);
	    }
	    zsfree(cnam);
	} else {
	    /* Not found at all. */
	    if (v || csh || wd)
		zputs(*argv, stdout), puts(wd ? ": none" : " not found");
	    returnval = 1;
	}
    }
    return returnval;
}

/**** command & named directory hash table builtins ****/

/*****************************************************************
 * hash -- explicitly hash a command.                            *
 * 1) Given no arguments, list the hash table.                   *
 * 2) The -m option prints out commands in the hash table that   *
 *    match a given glob pattern.                                *
 * 3) The -f option causes the entire path to be added to the    *
 *    hash table (cannot be combined with any arguments).        *
 * 4) The -r option causes the entire hash table to be discarded *
 *    (cannot be combined with any arguments).                   *
 * 5) Given argument of the form foo=bar, add element to command *
 *    hash table, so that when `foo' is entered, then `bar' is   *
 *    executed.                                                  *
 * 6) Given arguments not of the previous form, add it to the    *
 *    command hash table as if it were being executed.           *
 * 7) The -d option causes analogous things to be done using     *
 *    the named directory hash table.                            *
 *****************************************************************/

/**/
int
bin_hash(char *name, char **argv, char *ops, int func)
{
    HashTable ht;
    Comp com;
    Asgment asg;
    int returnval = 0;

    if (ops['d'])
	ht = nameddirtab;
    else
	ht = cmdnamtab;

    if (ops['r'] || ops['f']) {
	/* -f and -r can't be used with any arguments */
	if (*argv) {
	    zwarnnam("hash", "too many arguments", NULL, 0);
	    return 1;
	}

	/* empty the hash table */
	if (ops['r'])
	    ht->emptytable(ht);

	/* fill the hash table in a standard way */
	if (ops['f'])
	    ht->filltable(ht);

	return 0;
    }

    /* Given no arguments, display current hash table. */
    if (!*argv) {
	scanhashtable(ht, 1, 0, 0, ht->printnode, 0);
	return 0;
    }

    while (*argv) {
	void *hn;
	if (ops['m']) {
	    /* with the -m option, treat the argument as a glob pattern */
	    tokenize(*argv);  /* expand */
	    if ((com = parsereg(*argv))) {
		/* display matching hash table elements */
		scanmatchtable(ht, com, 0, 0, ht->printnode, 0);
	    } else {
		untokenize(*argv);
		zwarnnam(name, "bad pattern : %s", *argv, 0);
		returnval = 1;
	    }
	} else if((asg = getasg(*argv))->value) {
	    if(isset(RESTRICTED)) {
		zwarnnam(name, "restricted: %s", asg->value, 0);
		returnval = 1;
	    } else {
		/* The argument is of the form foo=bar, *
		 * so define an entry for the table.    */
		if(ops['d']) {
		    Nameddir nd = hn = zcalloc(sizeof *nd);
		    nd->flags = 0;
		    nd->dir = ztrdup(asg->value);
		} else {
		    Cmdnam cn = hn = zcalloc(sizeof *cn);
		    cn->flags = HASHED;
		    cn->u.cmd = ztrdup(asg->value);
		}
		ht->addnode(ht, ztrdup(asg->name), hn);
		if(ops['v'])
		    ht->printnode(hn, 0);
	    }
	} else if (!(hn = ht->getnode2(ht, asg->name))) {
	    /* With no `=value' part to the argument, *
	     * work out what it ought to be.          */
	    if(ops['d']) {
		if(!getnameddir(asg->name)) {
		    zwarnnam(name, "no such directory name: %s", asg->name, 0);
		    returnval = 1;
		}
	    } else {
		if (!hashcmd(asg->name, path)) {
		    zwarnnam(name, "no such command: %s", asg->name, 0);
		    returnval = 1;
		}
	    }
	    if(ops['v'] && (hn = ht->getnode2(ht, asg->name)))
		ht->printnode(hn, 0);
	} else if(ops['v'])
	    ht->printnode(hn, 0);
	argv++;
    }
    return returnval;
}

/* unhash: remove specified elements from a hash table */

/**/
int
bin_unhash(char *name, char **argv, char *ops, int func)
{
    HashTable ht;
    HashNode hn, nhn;
    Comp com;
    int match = 0, returnval = 0;
    int i;

    /* Check which hash table we are working with. */
    if (ops['d'])
	ht = nameddirtab;	/* named directories */
    else if (ops['f'])
	ht = shfunctab;		/* shell functions   */
    else if (ops['a'])
	ht = aliastab;		/* aliases           */
    else
	ht = cmdnamtab;		/* external commands */

    /* With -m option, treat arguments as glob patterns. *
     * "unhash -m '*'" is legal, but not recommended.    */
    if (ops['m']) {
	for (; *argv; argv++) {
	    /* expand argument */
	    tokenize(*argv);
	    if ((com = parsereg(*argv))) {
		/* remove all nodes matching glob pattern */
		for (i = 0; i < ht->hsize; i++) {
		    for (hn = ht->nodes[i]; hn; hn = nhn) {
			/* record pointer to next, since we may free this one */
			nhn = hn->next;
			if (domatch(hn->nam, com, 0)) {
			    ht->freenode(ht->removenode(ht, hn->nam));
			    match++;
			}
		    }
		}
	    } else {
		untokenize(*argv);
		zwarnnam(name, "bad pattern : %s", *argv, 0);
		returnval = 1;
	    }
	}
	/* If we didn't match anything, we return 1. */
	if (!match)
	    returnval = 1;
	return returnval;
    }

    /* Take arguments literally -- do not glob */
    for (; *argv; argv++) {
	if ((hn = ht->removenode(ht, *argv))) {
	    ht->freenode(hn);
	} else {
	    zwarnnam(name, "no such hash table element: %s", *argv, 0);
	    returnval = 1;
	}
    }
    return returnval;
}

/**** alias builtins ****/

/* alias: display or create aliases. */

/**/
int
bin_alias(char *name, char **argv, char *ops, int func)
{
    Alias a;
    Comp com;
    Asgment asg;
    int haveflags = 0, returnval = 0;
    int flags1 = 0, flags2 = DISABLED;
    int printflags = 0;

    /* Did we specify the type of alias? */
    if (ops['r'] || ops['g']) {
	if (ops['r'] && ops['g']) {
	    zwarnnam(name, "illegal combination of options", NULL, 0);
	    return 1;
	}
	haveflags = 1;
	if (ops['g'])
	    flags1 |= ALIAS_GLOBAL;
	else
	    flags2 |= ALIAS_GLOBAL;
    }

    if (ops['L'])
	printflags |= PRINT_LIST;

    /* In the absence of arguments, list all aliases.  If a command *
     * line flag is specified, list only those of that type.        */
    if (!*argv) {
	scanhashtable(aliastab, 1, flags1, flags2, aliastab->printnode, printflags);
	return 0;
    }

    /* With the -m option, treat the arguments as *
     * glob patterns of aliases to display.       */
    if (ops['m']) {
	for (; *argv; argv++) {
	    tokenize(*argv);  /* expand argument */
	    if ((com = parsereg(*argv))) {
		/* display the matching aliases */
		scanmatchtable(aliastab, com, flags1, flags2, aliastab->printnode, printflags);
	    } else {
		untokenize(*argv);
		zwarnnam(name, "bad pattern : %s", *argv, 0);
		returnval = 1;
	    }
	}
	return returnval;
    }

    /* Take arguments literally.  Don't glob */
    while ((asg = getasg(*argv++))) {
	if (asg->value && !ops['L']) {
	    /* The argument is of the form foo=bar and we are not *
	     * forcing a listing with -L, so define an alias      */
	    aliastab->addnode(aliastab, ztrdup(asg->name),
		createaliasnode(ztrdup(asg->value), flags1));
	} else if ((a = (Alias) aliastab->getnode(aliastab, asg->name))) {
	    /* display alias if appropriate */
	    if (!haveflags ||
		(ops['r'] && !(a->flags & ALIAS_GLOBAL)) ||
		(ops['g'] &&  (a->flags & ALIAS_GLOBAL)))
		aliastab->printnode((HashNode) a, printflags);
	} else
	    returnval = 1;
    }
    return returnval;
}


/**** miscellaneous builtins ****/

/* true, : (colon) */

/**/
int
bin_true(char *name, char **argv, char *ops, int func)
{
    return 0;
}

/* false builtin */

/**/
int
bin_false(char *name, char **argv, char *ops, int func)
{
    return 1;
}

/* the zle buffer stack */
 
/**/
LinkList bufstack;

/* echo, print, pushln */

/**/
int
bin_print(char *name, char **args, char *ops, int func)
{
    int nnl = 0, fd, argc, n;
    int *len;
    Histent ent;
    FILE *fout = stdout;

    /* -m option -- treat the first argument as a pattern and remove
     * arguments not matching */
    if (ops['m']) {
	Comp com;
	char **t, **p;

	tokenize(*args);
	if (!(com = parsereg(*args))) {
	    untokenize(*args);
	    zwarnnam(name, "bad pattern : %s", *args, 0);
	    return 1;
	}
	for (p = ++args; *p; p++)
	    if (!domatch(*p, com, 0))
		for (t = p--; (*t = t[1]); t++);
    }
    /* compute lengths, and interpret according to -P, -D, -e, etc. */
    argc = arrlen(args);
    len = (int *)ncalloc(argc * sizeof(int));
    for(n = 0; n < argc; n++) {
	/* first \ sequences */
	if (!ops['e'] && (ops['R'] || ops['r'] || ops['E']))
	    unmetafy(args[n], &len[n]);
	else
	    args[n] = getkeystring(args[n], &len[n],
				    func != BIN_ECHO && !ops['e'], &nnl);
	/* -P option -- interpret as a prompt sequence */
	if(ops['P'])
	    args[n] = unmetafy(promptexpand(metafy(args[n], len[n],
		META_NOALLOC), 0, NULL, NULL), &len[n]);
	/* -D option -- interpret as a directory, and use ~ */
	if(ops['D']) {
	    Nameddir d = finddir(args[n]);
	    if(d) {
		char *arg = alloc(strlen(args[n]) + 1);
		sprintf(arg, "~%s%s", d->nam,
			args[n] + strlen(d->dir));
		args[n] = arg;
		len[n] = strlen(args[n]);
	    }
	}
    }

    /* -z option -- push the arguments onto the editing buffer stack */
    if (ops['z']) {
	PERMALLOC {
	    pushnode(bufstack, sepjoin(args, NULL));
	} LASTALLOC;
	return 0;
    }
    /* -s option -- add the arguments to the history list */
    if (ops['s']) {
	int nwords = 0, nlen, iwords;
	char **pargs = args;

	PERMALLOC {
	    ent = gethistent(++curhist);
	    zsfree(ent->text);
	    if (ent->nwords)
		zfree(ent->words, ent->nwords*2*sizeof(short));
	    while (*pargs++)
		nwords++;
	    if ((ent->nwords = nwords)) {
		ent->words = (short *)zalloc(nwords*2*sizeof(short));
		nlen = iwords = 0;
		for (pargs = args; *pargs; pargs++) {
		    ent->words[iwords++] = nlen;
		    nlen += strlen(*pargs);
		    ent->words[iwords++] = nlen;
		    nlen++;
		}
	    } else
		ent->words = (short *)NULL;
	    ent->text = zjoin(args, ' ');
	    ent->stim = ent->ftim = time(NULL);
	    ent->flags = 0;
	} LASTALLOC;
	return 0;
    }
    /* -u and -p -- output to other than standard output */
    if (ops['u'] || ops['p']) {
	if (ops['u']) {
	    for (fd = 0; fd < 10; fd++)
		if (ops[fd + '0'])
		    break;
	    if (fd == 10)
		fd = 0;
	} else
	    fd = coprocout;
	if ((fd = dup(fd)) < 0) {
	    zwarnnam(name, "bad file number", NULL, 0);
	    return 1;
	}
	if ((fout = fdopen(fd, "w")) == 0) {
	    zwarnnam(name, "bad mode on fd", NULL, 0);
	    return 1;
	}
    }

    /* -o and -O -- sort the arguments */
    if (ops['o']) {
	if (ops['i'])
	    qsort(args, arrlen(args), sizeof(char *), cstrpcmp);

	else
	    qsort(args, arrlen(args), sizeof(char *), strpcmp);
    } else if (ops['O']) {
	if (ops['i'])
	    qsort(args, arrlen(args), sizeof(char *), invcstrpcmp);

	else
	    qsort(args, arrlen(args), sizeof(char *), invstrpcmp);
    }
    /* after sorting arguments, recalculate lengths */
    if(ops['o'] || ops['O'])
	for(n = 0; n < argc; n++)
	    len[n] = strlen(args[n]);

    /* -c -- output in columns */
    if (ops['c']) {
	int l, nc, nr, sc, n, t, i;
	char **ap;

	for (n = l = 0, ap = args; *ap; ap++, n++)
	    if (l < (t = strlen(*ap)))
		l = t;

	sc = l + 2;
	nc = (columns + 1) / sc;
	if (!nc)
	    nc = 1;
	nr = (n + nc - 1) / nc;

	for (i = 0; i < nr; i++) {
	    ap = args + i;
	    do {
		l = strlen(*ap);
		fprintf(fout, "%s", *ap);
		for (t = nr; t && *ap; t--, ap++);
		if(*ap)
		    for (; l < sc; l++)
			fputc(' ', fout);
	    } while (*ap);
	    fputc(ops['N'] ? '\0' : '\n', fout);
	}
	if (fout != stdout)
	    fclose(fout);
	return 0;
    }
    /* normal output */
    for (; *args; args++, len++) {
	fwrite(*args, *len, 1, fout);
	if (args[1])
	    fputc(ops['l'] ? '\n' : ops['N'] ? '\0' : ' ', fout);
    }
    if (!(ops['n'] || nnl))
	fputc(ops['N'] ? '\0' : '\n', fout);
    if (fout != stdout)
	fclose(fout);
    return 0;
}

/* echotc: output a termcap */

/**/
int
bin_echotc(char *name, char **argv, char *ops, int func)
{
    char *s, buf[2048], *t, *u;
    int num, argct;

    s = *argv++;
    if (termflags & TERM_BAD)
	return 1;
    if ((termflags & TERM_UNKNOWN) && (isset(INTERACTIVE) || !init_term()))
	return 1;
    /* if the specified termcap has a numeric value, display it */
    if ((num = tgetnum(s)) != -1) {
	printf("%d\n", num);
	return 0;
    }
    /* if the specified termcap is boolean, and set, say so  *
     * ncurses can tell if an existing boolean capability is *
     * off so in this case we print "no".                    */
#if !defined(NCURSES_VERSION) || !defined(COLOR_PAIR)
    if (tgetflag(s) > 0) {
	puts("yes");
	return (0);
    }
#else /* NCURSES_VERSION && COLOR_PAIR */
    switch (tgetflag(s)) {
    case -1:
	break;
    case 0:
	puts("no");
	return 0;
    default:
	puts("yes");
	return 0;
    }
#endif /* NCURSES_VERSION && COLOR_PAIR */
    /* get a string-type capability */
    u = buf;
    t = tgetstr(s, &u);
    if (!t || !*t) {
	/* capability doesn't exist, or (if boolean) is off */
	zwarnnam(name, "no such capability: %s", s, 0);
	return 1;
    }
    /* count the number of arguments required */
    for (argct = 0, u = t; *u; u++)
	if (*u == '%') {
	    if (u++, (*u == 'd' || *u == '2' || *u == '3' || *u == '.' ||
		      *u == '+'))
		argct++;
	}
    /* check that the number of arguments provided is correct */
    if (arrlen(argv) != argct) {
	zwarnnam(name, (arrlen(argv) < argct) ? "not enough arguments" :
		 "too many arguments", NULL, 0);
	return 1;
    }
    /* output string, through the proper termcap functions */
    if (!argct)
	tputs(t, 1, putraw);
    else {
	num = (argv[1]) ? atoi(argv[1]) : atoi(*argv);
	tputs(tgoto(t, atoi(*argv), num), num, putraw);
    }
    return 0;
}

/* shift builtin */

/**/
int
bin_shift(char *name, char **argv, char *ops, int func)
{
    int num = 1, l, ret = 0;
    char **s;
 
    /* optional argument can be either numeric or an array */
    if (*argv && !getaparam(*argv))
        num = matheval(*argv++);
 
    if (num < 0) {
        zwarnnam(name, "argument to shift must be non-negative", NULL, 0);
        return 1;
    }

    if (*argv) {
        for (; *argv; argv++)
            if ((s = getaparam(*argv))) {
                if (num > arrlen(s)) {
		    zwarnnam(name, "shift count must be <= $#", NULL, 0);
		    ret++;
		    continue;
		}
                PERMALLOC {
		    s = arrdup(s + num);
                } LASTALLOC;
                setaparam(*argv, s);
            }
    } else {
        if (num > (l = arrlen(pparams))) {
	    zwarnnam(name, "shift count must be <= $#", NULL, 0);
	    ret = 1;
	} else {
	    s = zalloc((l - num + 1) * sizeof(char *));
	    memcpy(s, pparams + num, (l - num + 1) * sizeof(char *));
	    while (num--)
		zsfree(pparams[num]);
	    zfree(pparams, (l + 1) * sizeof(char *));
	    pparams = s;
	}
    }
    return ret;
}

/* getopts: automagical option handling for shell scripts */

/**/
int
bin_getopts(char *name, char **argv, char *ops, int func)
{
    int lenstr, lenoptstr, quiet, lenoptbuf;
    char *optstr = unmetafy(*argv++, &lenoptstr), *var = *argv++;
    char **args = (*argv) ? argv : pparams;
    static int optcind = 0;
    char *str, optbuf[2] = " ", *p, opch;

    /* zoptind keeps count of the current argument number.  The *
     * user can set it to zero to start a new option parse.     */
    if (zoptind < 1) {
	/* first call */
	zoptind = 1;
	optcind = 0;
    }
    if(zoptind > arrlen(args))
	/* no more options */
	return 1;

    /* leading ':' in optstr means don't print an error message */
    quiet = *optstr == ':';
    optstr += quiet;
    lenoptstr -= quiet;

    /* find place in relevant argument */
    str = unmetafy(dupstring(args[zoptind - 1]), &lenstr);
    if(optcind >= lenstr) {
	optcind = 0;
	if(!args[zoptind++])
	    return 1;
	str = unmetafy(dupstring(args[zoptind - 1]), &lenstr);
    }
    if(!optcind) {
	if(lenstr < 2 || (*str != '-' && *str != '+'))
	    return 1;
	if(lenstr == 2 && str[0] == '-' && str[1] == '-') {
	    zoptind++;
	    return 1;
	}
	optcind++;
    }
    opch = str[optcind++];
    if(str[0] == '+') {
	optbuf[0] = '+';
	lenoptbuf = 2;
    } else
	lenoptbuf = 1;
    optbuf[lenoptbuf - 1] = opch;

    /* check for legality */
    if(opch == ':' || !(p = memchr(optstr, opch, lenoptstr))) {
	p = "?";
err:
      zsfree(zoptarg);
	if(quiet) {
	    setsparam(var, ztrdup(p));
	    zoptarg = metafy(optbuf, lenoptbuf, META_DUP);
	} else {
	    zerr(*p == '?' ? "bad option: -%c" :
		"argument expected after -%c option", NULL, opch);
          zoptarg=ztrdup("");
	    errflag = 0;
	}
	return 0;
    }

    /* check for required argument */
    if(p[1] == ':') {
	if(optcind == lenstr) {
	    if(!args[zoptind]) {
		p = ":";
		goto err;
	    }
	    p = ztrdup(args[zoptind++]);
	} else
	    p = metafy(str+optcind, lenstr-optcind, META_DUP);
	optcind = ztrlen(args[zoptind - 1]);
	zsfree(zoptarg);
	zoptarg = p;
    } else {
	zsfree(zoptarg);
	zoptarg = ztrdup("");
    }

    setsparam(var, metafy(optbuf, lenoptbuf, META_DUP));
    return 0;
}

/* break, bye, continue, exit, logout, return -- most of these take   *
 * one numeric argument, and the other (logout) is related to return. *
 * (return is treated as a logout when in a login shell.)             */

/**/
int
bin_break(char *name, char **argv, char *ops, int func)
{
    int num = lastval, nump = 0;

    /* handle one optional numeric argument */
    if (*argv) {
	num = matheval(*argv++);
	nump = 1;
    }

    switch (func) {
    case BIN_CONTINUE:
	if (!loops) {   /* continue is only permitted in loops */
	    zerrnam(name, "not in while, until, select, or repeat loop", NULL, 0);
	    return 1;
	}
	contflag = 1;   /* ARE WE SUPPOSED TO FALL THROUGH HERE? */
    case BIN_BREAK:
	if (!loops) {   /* break is only permitted in loops */
	    zerrnam(name, "not in while, until, select, or repeat loop", NULL, 0);
	    return 1;
	}
	breaks = nump ? minimum(num,loops) : 1;
	break;
    case BIN_RETURN:
	if (isset(INTERACTIVE) || locallevel || sourcelevel) {
	    retflag = 1;
	    breaks = loops;
	    lastval = num;
	    if (trapreturn == -2)
		trapreturn = lastval;
	    return lastval;
	}
	zexit(num, 0);	/* else treat return as logout/exit */
	break;
    case BIN_LOGOUT:
	if (unset(LOGINSHELL)) {
	    zerrnam(name, "not login shell", NULL, 0);
	    return 1;
	}
	zexit(num, 0);
	break;
    case BIN_EXIT:
	zexit(num, 0);
	break;
    }
    return 0;
}

/* we have printed a 'you have stopped (running) jobs.' message */
 
/**/
int stopmsg;
 
/* check to see if user has jobs running/stopped */

/**/
static void
checkjobs(void)
{
    int i;

    for (i = 1; i < MAXJOB; i++)
	if (i != thisjob && (jobtab[i].stat & STAT_LOCKED) &&
	    !(jobtab[i].stat & STAT_NOPRINT))
	    break;
    if (i < MAXJOB) {
	if (jobtab[i].stat & STAT_STOPPED) {

#ifdef USE_SUSPENDED
	    zerr("you have suspended jobs.", NULL, 0);
#else
	    zerr("you have stopped jobs.", NULL, 0);
#endif

	} else
	    zerr("you have running jobs.", NULL, 0);
	stopmsg = 1;
    }
}

/* exit the shell.  val is the return value of the shell.  *
 * from_signal should be non-zero if zexit is being called *
 * because of a signal.                                    */

/**/
void
zexit(int val, int from_signal)
{
    static int in_exit;

    HEAPALLOC {
	if (isset(MONITOR) && !stopmsg && !from_signal) {
	    scanjobs();    /* check if jobs need printing           */
	    checkjobs();   /* check if any jobs are running/stopped */
	    if (stopmsg) {
		stopmsg = 2;
		LASTALLOC_RETURN;
	    }
	}
	if (in_exit++ && from_signal)
	    LASTALLOC_RETURN;
	if (isset(MONITOR))
	    /* send SIGHUP to any jobs left running  */
	    killrunjobs(from_signal);
	if (isset(RCS) && interact) {
	    if (!nohistsave)
		savehistfile(getsparam("HISTFILE"), 1, isset(APPENDHISTORY) ? 3 : 0);
	    if (islogin && !subsh) {
		sourcehome(".zlogout");
#ifdef GLOBAL_ZLOGOUT
		source(GLOBAL_ZLOGOUT);
#endif
	    }
	}
	if (sigtrapped[SIGEXIT])
	    dotrap(SIGEXIT);
	if (mypid != getpid())
	    _exit(val);
	else
	    exit(val);
    } LASTALLOC;
}

/* . (dot), source */

/**/
int
bin_dot(char *name, char **argv, char *ops, int func)
{
    char **old, *old0 = NULL;
    int ret, diddot = 0, dotdot = 0;
    char buf[PATH_MAX];
    char *s, **t, *enam, *arg0;
    struct stat st;

    if (!*argv || strlen(*argv) >= PATH_MAX)
	return 0;
    old = pparams;
    /* get arguments for the script */
    if (argv[1]) {
	PERMALLOC {
	    pparams = arrdup(argv + 1);
	} LASTALLOC;
    }
    enam = arg0 = ztrdup(*argv);
    if (isset(FUNCTIONARGZERO)) {
	old0 = argzero;
	argzero = arg0;
    }
    s = unmeta(enam);
    errno = ENOENT;
    ret = 1;
    /* for source only, check in current directory first */
    if (*name != '.' && access(s, F_OK) == 0
	&& stat(s, &st) >= 0 && !S_ISDIR(st.st_mode)) {
	diddot = 1;
	ret = source(enam);
    }
    if (ret) {
	/* use a path with / in it */
	for (s = arg0; *s; s++)
	    if (*s == '/') {
		if (*arg0 == '.') {
		    if (arg0 + 1 == s)
			++diddot;
		    else if (arg0[1] == '.' && arg0 + 2 == s)
			++dotdot;
		}
		ret = source(arg0);
		break;
	    }
	if (!*s || (ret && isset(PATHDIRS) && diddot < 2 && dotdot == 0)) {
	    /* search path for script */
	    for (t = path; *t; t++) {
		if (!(*t)[0] || ((*t)[0] == '.' && !(*t)[1])) {
		    if (diddot)
			continue;
		    diddot = 1;
		    strcpy(buf, arg0);
		} else {
		    if (strlen(*t) + strlen(arg0) + 1 >= PATH_MAX)
			continue;
		    sprintf(buf, "%s/%s", *t, arg0);
		}
		s = unmeta(buf);
		if (access(s, F_OK) == 0 && stat(s, &st) >= 0
		    && !S_ISDIR(st.st_mode)) {
		    ret = source(enam = buf);
		    break;
		}
	    }
	}
    }
    /* clean up and return */
    if (argv[1]) {
	freearray(pparams);
	pparams = old;
    }
    if (ret)
	zwarnnam(name, "%e: %s", enam, errno);
    zsfree(arg0);
    if (old0)
	argzero = old0;
    return ret ? ret : lastval;
}

/**/
int
bin_emulate(char *nam, char **argv, char *ops, int func)
{
    emulate(*argv, ops['R']);
    return 0;
}

/* eval: simple evaluation */

/**/
int
bin_eval(char *nam, char **argv, char *ops, int func)
{
    List list;

    inpush(zjoin(argv, ' '), 0, NULL);
    strinbeg();
    stophist = 2;
    list = parse_list();
    strinend();
    inpop();
    if (!list) {
	errflag = 0;
	return 1;
    }
    execlist(list, 1, 0);
    if (errflag) {
	lastval = errflag;
	errflag = 0;
    }
    return lastval;
}

static char *zbuf;
static int readfd;

/* Read a character from readfd, or from the buffer zbuf.  Return EOF on end of
file/buffer. */

/* read: get a line of input, or (for compctl functions) return some *
 * useful data about the state of the editing line.  The -E and -e   *
 * options mean that the result should be sent to stdout.  -e means, *
 * in addition, that the result should not actually be assigned to   *
 * the specified parameters.                                         */

/**/
int
bin_read(char *name, char **args, char *ops, int func)
{
    char *reply, *readpmpt;
    int bsiz, c = 0, gotnl = 0, al = 0, first, nchars = 1, bslash;
    int haso = 0;	/* true if /dev/tty has been opened specially */
    int isem = !strcmp(term, "emacs");
    char *buf, *bptr, *firstarg, *zbuforig;
    LinkList readll = newlinklist();

    if ((ops['k'] || ops['b']) && *args && idigit(**args)) {
	if (!(nchars = atoi(*args)))
	    nchars = 1;
	args++;
    }

    firstarg = *args;
    if (*args && **args == '?')
	args++;
    /* default result parameter */
    reply = *args ? *args++ : ops['A'] ? "reply" : "REPLY";
    if (ops['A'] && *args) {
	zwarnnam(name, "only one array argument allowed", NULL, 0);
	return 1;
    }

    /* handle compctl case */
    if(ops['l'] || ops['c'])
	return compctlread(name, args, ops, reply);

    if ((ops['k'] && !ops['u'] && !ops['p']) || ops['q']) {
	if (SHTTY == -1) {
	    /* need to open /dev/tty specially */
	    SHTTY = open("/dev/tty", O_RDWR|O_NOCTTY);
	    haso = 1;
	}
	/* We should have a SHTTY opened by now. */
	if (SHTTY == -1) {
	    /* Unfortunately, we didn't. */
	    fprintf(stderr, "not interactive and can't open terminal\n");
	    fflush(stderr);
	    return 1;
	}
	if (unset(INTERACTIVE))
	    gettyinfo(&shttyinfo);
	/* attach to the tty */
	attachtty(mypgrp);
	if (!isem && ops['k'])
	    setcbreak();
	readfd = SHTTY;
    } else if (ops['u'] && !ops['p']) {
	/* -u means take input from the specified file descriptor. *
	 * -up means take input from the coprocess.                */
	for (readfd = 9; readfd && !ops[readfd + '0']; --readfd);
    } else if (ops['p'])
	readfd = coprocin;
    else
	readfd = 0;

    /* handle prompt */
    if (firstarg) {
	for (readpmpt = firstarg;
	     *readpmpt && *readpmpt != '?'; readpmpt++);
	if (*readpmpt++) {
	    if (isatty(0)) {
		zputs(readpmpt, stderr);
		fflush(stderr);
	    }
	    readpmpt[-1] = '\0';
	}
    }

    /* option -k means read only a given number of characters (default 1) */
    if (ops['k']) {
	int val;
	char d;

	/* allocate buffer space for result */
	bptr = buf = (char *)zalloc(nchars+1);

	do {
	    /* If read returns 0, is end of file */
	    if ((val = read(readfd, bptr, nchars)) <= 0)
		break;
	    
	    /* decrement number of characters read from number required */
	    nchars -= val;

	    /* increment pointer past read characters */
	    bptr += val;
	} while (nchars > 0);
	
	if (!ops['u'] && !ops['p']) {
	    /* dispose of result appropriately, etc. */
	    if (isem)
		while (val > 0 && read(SHTTY, &d, 1) == 1 && d != '\n');
	    else
		settyinfo(&shttyinfo);
	    if (haso) {
		close(SHTTY);
		SHTTY = -1;
	    }
	}

	if (ops['e'] || ops['E'])
	    fwrite(buf, bptr - buf, 1, stdout);
	if (!ops['e'])
	    setsparam(reply, metafy(buf, bptr - buf, META_REALLOC));
	else
	    zfree(buf, bptr - buf + 1);
	return val <= 0;
    }

    /* option -q means get one character, and interpret it as a Y or N */
    if (ops['q']) {
	char readbuf[2];

	/* set up the buffer */
	readbuf[1] = '\0';

	/* get, and store, reply */
	readbuf[0] = ((char)getquery(NULL, 0)) == 'y' ? 'y' : 'n';

	/* dispose of result appropriately, etc. */
	if (haso) {
	    close(SHTTY);
	    SHTTY = -1;
	}

	if (ops['e'] || ops['E'])
	    printf("%s\n", readbuf);
	if (!ops['e'])
	    setsparam(reply, ztrdup(readbuf));

	return readbuf[0] == 'n';
    }

    /* All possible special types of input have been exhausted.  Take one line,
    and assign words to the parameters until they run out.  Leftover words go
    onto the last parameter.  If an array is specified, all the words become
    separate elements of the array. */

    zbuforig = zbuf = (!ops['z']) ? NULL :
	(nonempty(bufstack)) ? (char *) getlinknode(bufstack) : ztrdup("");
    first = 1;
    bslash = 0;
    while (*args || (ops['A'] && !gotnl)) {
	buf = bptr = (char *)zalloc(bsiz = 64);
	/* get input, a character at a time */
	while (!gotnl) {
	    c = zread();
	    /* \ at the end of a line indicates a continuation *
	     * line, except in raw mode (-r option)            */
	    if (bslash && c == '\n') {
		bslash = 0;
		continue;
	    }
	    if (c == EOF || c == '\n')
		break;
	    if (!bslash && isep(c)) {
		if (bptr != buf || (!iwsep(c) && first)) {
		    first |= !iwsep(c);
		    break;
		}
		first |= !iwsep(c);
		continue;
	    }
	    bslash = c == '\\' && !bslash && !ops['r'];
	    if (bslash)
		continue;
	    first = 0;
	    if (imeta(c)) {
		*bptr++ = Meta;
		*bptr++ = c ^ 32;
	    } else
		*bptr++ = c;
	    /* increase the buffer size, if necessary */
	    if (bptr >= buf + bsiz - 1) {
		int blen = bptr - buf;

		buf = realloc(buf, bsiz *= 2);
		bptr = buf + blen;
	    }
	}
	if (c == '\n' || c == EOF)
	    gotnl = 1;
	*bptr = '\0';
	/* dispose of word appropriately */
	if (ops['e'] || ops['E']) {
	    zputs(buf, stdout);
	    putchar('\n');
	}
	if (!ops['e']) {
	    if (ops['A']) {
		addlinknode(readll, buf);
		al++;
	    } else
		setsparam(reply, buf);
	} else
	    free(buf);
	if (!ops['A'])
	    reply = *args++;
    }
    /* handle EOF */
    if (c == EOF) {
	if (readfd == coprocin) {
	    close(coprocin);
	    close(coprocout);
	    coprocin = coprocout = -1;
	}
    }
    /* final assignment (and display) of array parameter */
    if (ops['A']) {
	char **pp, **p = NULL;
	LinkNode n;

	p = (ops['e'] ? (char **)NULL
	     : (char **)zalloc((al + 1) * sizeof(char *)));

	for (pp = p, n = firstnode(readll); n; incnode(n)) {
	    if (ops['e'] || ops['E']) {
		zputs((char *) getdata(n), stdout);
		putchar('\n');
	    }
	    if (p)
		*pp++ = (char *)getdata(n);
	    else
		zsfree(getdata(n));
	}
	if (p) {
	    *pp++ = NULL;
	    setaparam(reply, p);
	}
	return c == EOF;
    }
    buf = bptr = (char *)zalloc(bsiz = 64);
    /* any remaining part of the line goes into one parameter */
    bslash = 0;
    if (!gotnl)
	for (;;) {
	    c = zread();
	    /* \ at the end of a line introduces a continuation line, except in
	    raw mode (-r option) */
	    if (bslash && c == '\n') {
		bslash = 0;
		continue;
	    }
	    if (c == EOF || (c == '\n' && !zbuf))
		break;
	    if (!bslash && isep(c) && bptr == buf)
		if (iwsep(c))
		    continue;
		else if (!first) {
		    first = 1;
		    continue;
		}
	    bslash = c == '\\' && !bslash && !ops['r'];
	    if (bslash)
		continue;
	    if (imeta(c)) {
		*bptr++ = Meta;
		*bptr++ = c ^ 32;
	    } else
		*bptr++ = c;
	    /* increase the buffer size, if necessary */
	    if (bptr >= buf + bsiz - 1) {
		int blen = bptr - buf;

		buf = realloc(buf, bsiz *= 2);
		bptr = buf + blen;
	    }
	}
    while (bptr > buf && iwsep(bptr[-1]))
	bptr--;
    *bptr = '\0';
    /* final assignment of reply, etc. */
    if (ops['e'] || ops['E']) {
	zputs(buf, stdout);
	putchar('\n');
    }
    if (!ops['e'])
	setsparam(reply, buf);
    else
	zsfree(buf);
    if (zbuforig) {
	char first = *zbuforig;

	zsfree(zbuforig);
	if (!first)
	    return 1;
    } else if (c == EOF) {
	if (readfd == coprocin) {
	    close(coprocin);
	    close(coprocout);
	    coprocin = coprocout = -1;
	}
	return 1;
    }
    return 0;
}

/**/
static int
zread(void)
{
    char cc, retry = 0;

    /* use zbuf if possible */
    if (zbuf)
	/* If zbuf points to anything, it points to the next character in the
	buffer.  This may be a null byte to indicate EOF.  If reading from the
	buffer, move on the buffer pointer. */
	if (*zbuf == Meta)
	    return zbuf++, STOUC(*zbuf++ ^ 32);
	else
	    return (*zbuf) ? STOUC(*zbuf++) : EOF;
    for (;;) {
	/* read a character from readfd */
	switch (read(readfd, &cc, 1)) {
	case 1:
	    /* return the character read */
	    return STOUC(cc);
#if defined(EAGAIN) || defined(EWOULDBLOCK)
	case -1:
	    if (!retry && readfd == 0 && (
# ifdef EAGAIN
		    errno == EAGAIN
#  ifdef EWOULDBLOCK
		    ||
#  endif /* EWOULDBLOCK */
# endif /* EAGAIN */
# ifdef EWOULDBLOCK
		    errno == EWOULDBLOCK
# endif /* EWOULDBLOCK */
		) && setblock_stdin()) {
		retry = 1;
		continue;
	    }
	    break;
#endif /* EAGAIN || EWOULDBLOCK */
	}
	return EOF;
    }
}

/* holds arguments for testlex() */
/**/
char **testargs;

/* test, [: the old-style general purpose logical expression builtin */

/**/
void
testlex(void)
{
    if (tok == LEXERR)
	return;

    tokstr = *testargs;
    if (!*testargs) {
	/* if tok is already zero, reading past the end:  error */
	tok = tok ? NULLTOK : LEXERR;
	return;
    } else if (!strcmp(*testargs, "-o"))
	tok = DBAR;
    else if (!strcmp(*testargs, "-a"))
	tok = DAMPER;
    else if (!strcmp(*testargs, "!"))
	tok = BANG;
    else if (!strcmp(*testargs, "("))
	tok = INPAR;
    else if (!strcmp(*testargs, ")"))
	tok = OUTPAR;
    else
	tok = STRING;
    testargs++;
}

/**/
int
bin_test(char *name, char **argv, char *ops, int func)
{
    char **s;
    Cond c;

    /* if "test" was invoked as "[", it needs a matching "]" *
     * which is subsequently ignored                         */
    if (func == BIN_BRACKET) {
	for (s = argv; *s; s++);
	if (s == argv || strcmp(s[-1], "]")) {
	    zwarnnam(name, "']' expected", NULL, 0);
	    return 1;
	}
	s[-1] = NULL;
    }
    /* an empty argument list evaluates to false (1) */
    if (!*argv)
	return 1;

    testargs = argv;
    tok = NULLTOK;
    condlex = testlex;
    testlex();
    c = par_cond();
    condlex = yylex;

    if (errflag) {
	errflag = 0;
	return 1;
    }

    if (!c || tok == LEXERR) {
	zwarnnam(name, tokstr ? "parse error" : "argument expected", NULL, 0);
	return 1;
    }

    /* syntax is OK, so evaluate */
    return !evalcond(c);
}

/* display a time, provided in units of 1/60s, as minutes and seconds */
#define pttime(X) printf("%ldm%ld.%02lds",((long) (X))/3600,\
			 ((long) (X))/60%60,((long) (X))*100/60%100)

/* times: display, in a two-line format, the times provided by times(3) */

/**/
int
bin_times(char *name, char **argv, char *ops, int func)
{
    struct tms buf;

    /* get time accounting information */
    if (times(&buf) == -1)
	return 1;
    pttime(buf.tms_utime);	/* user time */
    putchar(' ');
    pttime(buf.tms_stime);	/* system time */
    putchar('\n');
    pttime(buf.tms_cutime);	/* user time, children */
    putchar(' ');
    pttime(buf.tms_cstime);	/* system time, children */
    putchar('\n');
    return 0;
}

/* trap: set/unset signal traps */

/**/
int
bin_trap(char *name, char **argv, char *ops, int func)
{
    List l;
    char *arg, *s;
    int sig;

    if (*argv && !strcmp(*argv, "--"))
	argv++;

    /* If given no arguments, list all currently-set traps */
    if (!*argv) {
	for (sig = 0; sig < VSIGCOUNT; sig++) {
	    if (sigtrapped[sig] & ZSIG_FUNC) {
		char fname[20];
		HashNode hn;

		sprintf(fname, "TRAP%s", sigs[sig]);
		if ((hn = shfunctab->getnode(shfunctab, fname)))
		    shfunctab->printnode(hn, 0);
		DPUTS(!hn, "BUG: I did not find any trap functions!");
	    } else if (sigtrapped[sig]) {
		if (!sigfuncs[sig])
		    printf("trap -- '' %s\n", sigs[sig]);
		else {
		    s = getpermtext((void *) dupstruct((void *) sigfuncs[sig]));
		    printf("trap -- ");
		    quotedzputs(s, stdout);
		    printf(" %s\n", sigs[sig]);
		    zsfree(s);
		}
	    }
	}
	return 0;
    }

    /* If we have a signal number, unset the specified *
     * signals.  With only -, remove all traps.        */
    if ((getsignum(*argv) != -1) || (!strcmp(*argv, "-") && argv++)) {
	if (!*argv)
	    for (sig = 0; sig < VSIGCOUNT; sig++)
		unsettrap(sig);
	else
	    while (*argv)
		unsettrap(getsignum(*argv++));
	return 0;
    }

    /* Sort out the command to execute on trap */
    arg = *argv++;
    if (!*arg)
	l = NULL;
    else if (!(l = parse_string(arg))) {
	zwarnnam(name, "couldn't parse trap command", NULL, 0);
	return 1;
    }

    /* set traps */
    for (; *argv; argv++) {
	List t;

	sig = getsignum(*argv);
	if (sig == -1) {
	    zwarnnam(name, "undefined signal: %s", *argv, 0);
	    break;
	}
	PERMALLOC {
	    t = (List) dupstruct(l);
	} LASTALLOC;
	if (settrap(sig, t))
	    freestruct(t);
    }
    return *argv != NULL;
}

/**/
int
bin_ttyctl(char *name, char **argv, char *ops, int func)
{
    if (ops['f'])
	ttyfrozen = 1;
    else if (ops['u'])
	ttyfrozen = 0;
    else
	printf("tty is %sfrozen\n", ttyfrozen ? "" : "not ");
    return 0;
}

/* let -- mathematical evaluation */

/**/
int
bin_let(char *name, char **argv, char *ops, int func)
{
    long val = 0;

    while (*argv)
	val = matheval(*argv++);
    /* Errors in math evaluation in let are non-fatal. */
    errflag = 0;
    return !val;
}

/* umask command.  umask may be specified as octal digits, or in the  *
 * symbolic form that chmod(1) uses.  Well, a subset of it.  Remember *
 * that only the bottom nine bits of umask are used, so there's no    *
 * point allowing the set{u,g}id and sticky bits to be specified.     */

/**/
int
bin_umask(char *nam, char **args, char *ops, int func)
{
    mode_t um;
    char *s = *args;

    /* Get the current umask. */
    um = umask(0);
    umask(um);
    /* No arguments means to display the current setting. */
    if (!s) {
	if (ops['S']) {
	    char *who = "ugo";

	    while (*who) {
		char *what = "rwx";
		printf("%c=", *who++);
		while (*what) {
		    if (!(um & 0400))
			putchar(*what);
		    um <<= 1;
		    what++;
		}
		putchar(*who ? ',' : '\n');
	    }
	} else {
	    if (um & 0700)
		putchar('0');
	    printf("%03o\n", (unsigned)um);
	}
	return 0;
    }

    if (idigit(*s)) {
	/* Simple digital umask. */
	um = zstrtol(s, &s, 8);
	if (*s) {
	    zwarnnam(nam, "bad umask", NULL, 0);
	    return 1;
	}
    } else {
	/* Symbolic notation -- slightly complicated. */
	int whomask, umaskop, mask;

	/* More than one symbolic argument may be used at once, each separated
	by commas. */
	for (;;) {
	    /* First part of the argument -- who does this apply to?
	    u=owner, g=group, o=other. */
	    whomask = 0;
	    while (*s == 'u' || *s == 'g' || *s == 'o' || *s == 'a')
		if (*s == 'u')
		    s++, whomask |= 0700;
		else if (*s == 'g')
		    s++, whomask |= 0070;
		else if (*s == 'o')
		    s++, whomask |= 0007;
		else if (*s == 'a')
		    s++, whomask |= 0777;
	    /* Default whomask is everyone. */
	    if (!whomask)
		whomask = 0777;
	    /* Operation may be +, - or =. */
	    umaskop = (int)*s;
	    if (!(umaskop == '+' || umaskop == '-' || umaskop == '=')) {
		if (umaskop)
		    zwarnnam(nam, "bad symbolic mode operator: %c", NULL, umaskop);
		else
		    zwarnnam(nam, "bad umask", NULL, 0);
		return 1;
	    }
	    /* Permissions mask -- r=read, w=write, x=execute. */
	    mask = 0;
	    while (*++s && *s != ',')
		if (*s == 'r')
		    mask |= 0444 & whomask;
		else if (*s == 'w')
		    mask |= 0222 & whomask;
		else if (*s == 'x')
		    mask |= 0111 & whomask;
		else {
		    zwarnnam(nam, "bad symbolic mode permission: %c",
			     NULL, *s);
		    return 1;
		}
	    /* Apply parsed argument to um. */
	    if (umaskop == '+')
		um &= ~mask;
	    else if (umaskop == '-')
		um |= mask;
	    else		/* umaskop == '=' */
		um = (um | (whomask)) & ~mask;
	    if (*s == ',')
		s++;
	    else
		break;
	}
	if (*s) {
	    zwarnnam(nam, "bad character in symbolic mode: %c", NULL, *s);
	    return 1;
	}
    }

    /* Finally, set the new umask. */
    umask(um);
    return 0;
}

/* Generic builtin for facilities not available on this OS */

/**/
int
bin_notavail(char *nam, char **argv, char *ops, int func)
{
    zwarnnam(nam, "not available on this system", NULL, 0);
    return 1;
}
