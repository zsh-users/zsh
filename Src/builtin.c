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

/* this is defined so we get the prototype for open_memstream */
#define _GNU_SOURCE 1

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
    BUILTIN("alias", BINF_MAGICEQUALS | BINF_PLUSOPTS, bin_alias, 0, -1, 0, "Lgmr", NULL),
    BUILTIN("autoload", BINF_PLUSOPTS, bin_functions, 0, -1, 0, "tUXwkz", "u"),
    BUILTIN("bg", 0, bin_fg, 0, -1, BIN_BG, NULL, NULL),
    BUILTIN("break", BINF_PSPECIAL, bin_break, 0, 1, BIN_BREAK, NULL, NULL),
    BUILTIN("bye", 0, bin_break, 0, 1, BIN_EXIT, NULL, NULL),
    BUILTIN("cd", BINF_SKIPINVALID | BINF_SKIPDASH | BINF_DASHDASHVALID, bin_cd, 0, 2, BIN_CD, "sPL", NULL),
    BUILTIN("chdir", BINF_SKIPINVALID | BINF_SKIPDASH | BINF_DASHDASHVALID, bin_cd, 0, 2, BIN_CD, "sPL", NULL),
    BUILTIN("continue", BINF_PSPECIAL, bin_break, 0, 1, BIN_CONTINUE, NULL, NULL),
    BUILTIN("declare", BINF_PLUSOPTS | BINF_MAGICEQUALS | BINF_PSPECIAL, bin_typeset, 0, -1, 0, "AE:%F:%HL:%R:%TUZ:%afghi:%lprtux", NULL),
    BUILTIN("dirs", 0, bin_dirs, 0, -1, 0, "clpv", NULL),
    BUILTIN("disable", 0, bin_enable, 0, -1, BIN_DISABLE, "afmr", NULL),
    BUILTIN("disown", 0, bin_fg, 0, -1, BIN_DISOWN, NULL, NULL),
    BUILTIN("echo", BINF_PRINTOPTS | BINF_SKIPINVALID, bin_print, 0, -1, BIN_ECHO, "neE", "-"),
    BUILTIN("emulate", 0, bin_emulate, 1, 1, 0, "LR", NULL),
    BUILTIN("enable", 0, bin_enable, 0, -1, BIN_ENABLE, "afmr", NULL),
    BUILTIN("eval", BINF_PSPECIAL, bin_eval, 0, -1, BIN_EVAL, NULL, NULL),
    BUILTIN("exit", BINF_PSPECIAL, bin_break, 0, 1, BIN_EXIT, NULL, NULL),
    BUILTIN("export", BINF_PLUSOPTS | BINF_MAGICEQUALS | BINF_PSPECIAL, bin_typeset, 0, -1, BIN_EXPORT, "E:%F:%HL:%R:%TUZ:%afhi:%lprtu", "xg"),
    BUILTIN("false", 0, bin_false, 0, -1, 0, NULL, NULL),
    /*
     * We used to behave as if the argument to -e was optional.
     * But that's actually not useful, so it's more consistent to
     * cause an error.
     */
    BUILTIN("fc", 0, bin_fc, 0, -1, BIN_FC, "nlre:IRWAdDfEim",
	    NULL),
    BUILTIN("fg", 0, bin_fg, 0, -1, BIN_FG, NULL, NULL),
    BUILTIN("float", BINF_PLUSOPTS | BINF_MAGICEQUALS | BINF_PSPECIAL, bin_typeset, 0, -1, 0, "E:%F:%Hghlprtux", "E"),
    BUILTIN("functions", BINF_PLUSOPTS, bin_functions, 0, -1, 0, "mtuU", NULL),
    BUILTIN("getln", 0, bin_read, 0, -1, 0, "ecnAlE", "zr"),
    BUILTIN("getopts", 0, bin_getopts, 2, -1, 0, NULL, NULL),
    BUILTIN("hash", BINF_MAGICEQUALS, bin_hash, 0, -1, 0, "Ldfmrv", NULL),

#ifdef ZSH_HASH_DEBUG
    BUILTIN("hashinfo", 0, bin_hashinfo, 0, 0, 0, NULL, NULL),
#endif

    BUILTIN("history", 0, bin_fc, 0, -1, BIN_FC, "nrdDfEim", "l"),
    BUILTIN("integer", BINF_PLUSOPTS | BINF_MAGICEQUALS | BINF_PSPECIAL, bin_typeset, 0, -1, 0, "Hghi:%lprtux", "i"),
    BUILTIN("jobs", 0, bin_fg, 0, -1, BIN_JOBS, "dlpZrs", NULL),
    BUILTIN("kill", 0, bin_kill, 0, -1, 0, NULL, NULL),
    BUILTIN("let", 0, bin_let, 1, -1, 0, NULL, NULL),
    BUILTIN("local", BINF_PLUSOPTS | BINF_MAGICEQUALS | BINF_PSPECIAL, bin_typeset, 0, -1, 0, "AE:%F:%HL:%R:%TUZ:%ahi:%lprtux", NULL),
    BUILTIN("log", 0, bin_log, 0, 0, 0, NULL, NULL),
    BUILTIN("logout", 0, bin_break, 0, 1, BIN_LOGOUT, NULL, NULL),

#if defined(ZSH_MEM) & defined(ZSH_MEM_DEBUG)
    BUILTIN("mem", 0, bin_mem, 0, 0, 0, "v", NULL),
#endif

#if defined(ZSH_PAT_DEBUG)
    BUILTIN("patdebug", 0, bin_patdebug, 1, -1, 0, "p", NULL),
#endif

    BUILTIN("popd", 0, bin_cd, 0, 1, BIN_POPD, NULL, NULL),
    BUILTIN("print", BINF_PRINTOPTS, bin_print, 0, -1, BIN_PRINT, "abcC:Df:ilmnNoOpPrRsu:z-", NULL),
    BUILTIN("printf", 0, bin_print, 1, -1, BIN_PRINTF, NULL, NULL),
    BUILTIN("pushd", BINF_SKIPINVALID | BINF_SKIPDASH | BINF_DASHDASHVALID, bin_cd, 0, 2, BIN_PUSHD, "sPL", NULL),
    BUILTIN("pushln", BINF_PRINTOPTS, bin_print, 0, -1, BIN_PRINT, NULL, "-nz"),
    BUILTIN("pwd", 0, bin_pwd, 0, 0, 0, "rLP", NULL),
    BUILTIN("r", 0, bin_fc, 0, -1, BIN_R, "nrl", NULL),
    BUILTIN("read", 0, bin_read, 0, -1, 0, "cd:ek:%lnpqrst:%zu:AE", NULL),
    BUILTIN("readonly", BINF_PLUSOPTS | BINF_MAGICEQUALS | BINF_PSPECIAL, bin_typeset, 0, -1, 0, "AE:%F:%HL:%R:%TUZ:%afghi:%lptux", "r"),
    BUILTIN("rehash", 0, bin_hash, 0, 0, 0, "df", "r"),
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
    BUILTIN("typeset", BINF_PLUSOPTS | BINF_MAGICEQUALS | BINF_PSPECIAL, bin_typeset, 0, -1, 0, "AE:%F:%HL:%R:%TUZ:%afghi:%lprtuxm", NULL),
    BUILTIN("umask", 0, bin_umask, 0, 1, 0, "S", NULL),
    BUILTIN("unalias", 0, bin_unhash, 1, -1, 0, "m", "a"),
    BUILTIN("unfunction", 0, bin_unhash, 1, -1, 0, "m", "f"),
    BUILTIN("unhash", 0, bin_unhash, 1, -1, 0, "adfm", NULL),
    BUILTIN("unset", BINF_PSPECIAL, bin_unset, 1, -1, 0, "fmv", NULL),
    BUILTIN("unsetopt", 0, bin_setopt, 0, -1, BIN_UNSETOPT, NULL, NULL),
    BUILTIN("wait", 0, bin_fg, 0, -1, BIN_WAIT, NULL, NULL),
    BUILTIN("whence", 0, bin_whence, 0, -1, 0, "acmpvfsw", NULL),
    BUILTIN("where", 0, bin_whence, 0, -1, 0, "pmsw", "ca"),
    BUILTIN("which", 0, bin_whence, 0, -1, 0, "ampsw", "c"),
    BUILTIN("zmodload", 0, bin_zmodload, 0, -1, 0, "ARILabcfdipue", NULL),
    BUILTIN("zcompile", 0, bin_zcompile, 0, -1, 0, "tUMRcmzka", NULL),
};

/****************************************/
/* Builtin Command Hash Table Functions */
/****************************************/

/* hash table containing builtin commands */

/**/
mod_export HashTable builtintab;
 
/**/
void
createbuiltintable(void)
{
    builtintab = newhashtable(85, "builtintab", NULL);

    builtintab->hash        = hasher;
    builtintab->emptytable  = NULL;
    builtintab->filltable   = NULL;
    builtintab->cmpnodes    = strcmp;
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

/* Make sure we have space for a new option and increment. */

#define OPT_ALLOC_CHUNK 16

/**/
static int
new_optarg(Options ops)
{
    /* Argument index must be a non-zero 6-bit number. */
    if (ops->argscount == 63)
	return 1;
    if (ops->argsalloc == ops->argscount) {
	char **newptr = 
	    (char **)zhalloc((ops->argsalloc + OPT_ALLOC_CHUNK) *
			     sizeof(char *));
	if (ops->argsalloc)
	    memcpy(newptr, ops->args, ops->argsalloc * sizeof(char *));
	ops->args = newptr;
	ops->argsalloc += OPT_ALLOC_CHUNK;
    }
    ops->argscount++;
    return 0;
}


/* execute a builtin handler function after parsing the arguments */

/**/
int
execbuiltin(LinkList args, Builtin bn)
{
    char *pp, *name, *optstr;
    int flags, sense, argc, execop, xtr = isset(XTRACE);
    struct options ops;

    /* initialise options structure */
    memset(ops.ind, 0, MAX_OPS*sizeof(unsigned char));
    ops.args = NULL;
    ops.argscount = ops.argsalloc = 0;

    /* initialize some local variables */
    name = (char *) ugetnode(args);

    if (!bn->handlerfunc) {
	zwarnnam(name, "autoload failed", NULL, 0);
	deletebuiltin(bn->nam);
	return 1;
    }
    /* get some information about the command */
    flags = bn->flags;
    optstr = bn->optstr;

    /* Set up the argument list. */
    /* count the arguments */
    argc = countlinknodes(args);

    {
	/*
	 * Keep all arguments, including options, in an array.
	 * We don't actually need the option part of the argument
	 * after option processing, but it makes XTRACE output
	 * much simpler.
	 */
	VARARR(char *, argarr, argc + 1);
	char **argv;

	/*
	 * Get the actual arguments, into argv.  Remember argarr
	 * may be an array declaration, depending on the compiler.
	 */
	argv = argarr;
	while ((*argv++ = (char *)ugetnode(args)));
	argv = argarr;

	/* Sort out the options. */
	if (optstr) {
	    char *arg = *argv;
	    /* while arguments look like options ... */
	    while (arg &&
		   /* Must begin with - or maybe + */
		   ((sense = (*arg == '-')) ||
		    ((flags & BINF_PLUSOPTS) && *arg == '+'))) {
		/* Digits aren't arguments unless the command says they are. */
		if (!(flags & BINF_KEEPNUM) && idigit(arg[1]))
		    break;
		/* For cd and friends, a single dash is not an option. */
		if ((flags & BINF_SKIPDASH) && !arg[1])
		    break;
		if ((flags & BINF_DASHDASHVALID) && !strcmp(arg, "--")) {
		    /*
		     * Need to skip this before checking whether this is
		     * really an option.
		     */
		    argv++;
		    break;
		}
		/*
		 * Unrecognised options to echo etc. are not really
		 * options.
		 *
		 * Note this flag is not smart enough to handle option
		 * arguments.  In fact, ideally it shouldn't be added
		 * to any new builtins, to preserve standard option
		 * handling as much as possible.
		*/
		if (flags & BINF_SKIPINVALID) {
		    char *p = arg;
		    if (optstr)
			while (*++p && strchr(optstr, (int) *p));
		    else
			p++;
		    if (*p)
			break;
		}
		/* handle -- or - (ops.ind['-']), and +
		 * (ops.ind['-'] and ops.ind['+']) */
		if (arg[1] == '-')
		    arg++;
		if (!arg[1]) {
		    ops.ind['-'] = 1;
		    if (!sense)
			ops.ind['+'] = 1;
		}
		/* save options in ops, as long as they are in bn->optstr */
		while (*++arg) {
		    char *optptr;
		    if ((optptr = strchr(optstr, execop = (int)*arg))) {
			ops.ind[(int)*arg] = (sense) ? 1 : 2;
			if (optptr[1] == ':') {
			    char *argptr = NULL;
			    if (optptr[2] == ':') {
				if (arg[1])
				    argptr = arg+1;
				/* Optional argument in same word*/
			    } else if (optptr[2] == '%') {
				/* Optional numeric argument in same
				 * or next word. */
				if (arg[1] && idigit(arg[1]))
				    argptr = arg+1;
				else if (argv[1] && idigit(*argv[1]))
				    argptr = arg = *++argv;
			    } else {
				/* Mandatory argument */
				if (arg[1])
				    argptr = arg+1;
				else if ((arg = *++argv))
				    argptr = arg;
				else {
				    zwarnnam(name, "argument expected: -%c",
					     NULL, execop);
				    return 1;
				}
			    }
			    if (argptr) {
				if (new_optarg(&ops)) {
				    zwarnnam(name, 
					     "too many option arguments",
					     NULL, 0);
				    return 1;
				}
				ops.ind[execop] |= ops.argscount << 2;
				ops.args[ops.argscount-1] = argptr;
				while (arg[1])
				    arg++;
			    }
			}
		    } else
			break;
		}
		/* The above loop may have exited on an invalid option.  (We  *
		 * assume that any option requiring metafication is invalid.) */
		if (*arg) {
		    if(*arg == Meta)
			*++arg ^= 32;
		    zwarn("bad option: -%c", NULL, *arg);
		    return 1;
		}
		arg = *++argv;
		/* for the "print" builtin, the options after -R are treated as
		   options to "echo" */
		if ((flags & BINF_PRINTOPTS) && ops.ind['R'] &&
		    !ops.ind['f']) {
		    optstr = "ne";
		    flags |= BINF_SKIPINVALID;
		}
		/* the option -- indicates the end of the options */
		if (ops.ind['-'])
		    break;
	    }
	}

	/* handle built-in options, for overloaded handler functions */
	if ((pp = bn->defopts)) {
	    while (*pp) {
		/* only if not already set */
		if (!ops.ind[(int)*pp])
		    ops.ind[(int)*pp] = 1;
		pp++;
	    }
	}

	/* Fix the argument count by subtracting option arguments */
	argc -= argv - argarr;

	if (errflag) {
	    errflag = 0;
	    return 1;
	}

	/* check that the argument count lies within the specified bounds */
	if (argc < bn->minargs || (argc > bn->maxargs && bn->maxargs != -1)) {
	    zwarnnam(name, (argc < bn->minargs)
		     ? "not enough arguments" : "too many arguments", NULL, 0);
	    return 1;
	}

	/* display execution trace information, if required */
	if (xtr) {
	    /* Use full argument list including options for trace output */
	    char **fullargv = argarr;
	    printprompt4();
	    fprintf(xtrerr, "%s", name);
	    while (*fullargv) {
	        fputc(' ', xtrerr);
	        quotedzputs(*fullargv++, xtrerr);
	    }
	    fputc('\n', xtrerr);
	    fflush(xtrerr);
	}
	/* call the handler function, and return its return value */
	return (*(bn->handlerfunc)) (name, argv, &ops, bn->funcid);
    }
}

/* Enable/disable an element in one of the internal hash tables.  *
 * With no arguments, it lists all the currently enabled/disabled *
 * elements in that particular hash table.                        */

/**/
int
bin_enable(char *name, char **argv, Options ops, int func)
{
    HashTable ht;
    HashNode hn;
    ScanFunc scanfunc;
    Patprog pprog;
    int flags1 = 0, flags2 = 0;
    int match = 0, returnval = 0;

    /* Find out which hash table we are working with. */
    if (OPT_ISSET(ops,'f'))
	ht = shfunctab;
    else if (OPT_ISSET(ops,'r'))
	ht = reswdtab;
    else if (OPT_ISSET(ops,'a'))
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
	queue_signals();
	scanhashtable(ht, 1, flags1, flags2, ht->printnode, 0);
	unqueue_signals();
	return 0;
    }

    /* With -m option, treat arguments as glob patterns. */
    if (OPT_ISSET(ops,'m')) {
	for (; *argv; argv++) {
	    /* parse pattern */
	    tokenize(*argv);
	    if ((pprog = patcompile(*argv, PAT_STATIC, 0))) {
		queue_signals();
		match += scanmatchtable(ht, pprog, 0, 0, scanfunc, 0);
		unqueue_signals();
	    }
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
    queue_signals();
    for (; *argv; argv++) {
	if ((hn = ht->getnode2(ht, *argv))) {
	    scanfunc(hn, 0);
	} else {
	    zwarnnam(name, "no such hash table element: %s", *argv, 0);
	    returnval = 1;
	}
    }
    unqueue_signals();
    return returnval;
}

/* set: either set the shell options, or set the shell arguments, *
 * or declare an array, or show various things                    */

/**/
int
bin_set(char *nam, char **args, Options ops, int func)
{
    int action, optno, array = 0, hadopt = 0,
	hadplus = 0, hadend = 0, sort = 0;
    char **x, *arrayname = NULL;

    /* Obsolescent sh compatibility: set - is the same as set +xv *
     * and set - args is the same as set +xv -- args              */
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
		    printoptionstates(hadplus);
		    inittyptab();
		    return 0;
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
		arrayname = *args;
		if (!arrayname)
		    goto doneoptions;
		else if  (!isset(KSHARRAYS))
		{
		    args++;
		    goto doneoptions;
		}
		break;
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
    queue_signals();
    if (!arrayname)
    {
	if (!hadopt && !*args)
	    scanhashtable(paramtab, 1, 0, 0, paramtab->printnode,
			  hadplus ? PRINT_NAMEONLY : 0);

	if (array) {
	    /* display arrays */
	    scanhashtable(paramtab, 1, PM_ARRAY, 0, paramtab->printnode,
			  hadplus ? PRINT_NAMEONLY : 0);
	}
	if (!*args && !hadend) {
	    unqueue_signals();
	    return 0;
	}
    }
    if (sort)
	qsort(args, arrlen(args), sizeof(char *),
	      sort > 0 ? strpcmp : invstrpcmp);
    if (array) {
	/* create an array with the specified elements */
	char **a = NULL, **y;
	int len = arrlen(args);

	if (array < 0 && (a = getaparam(arrayname))) {
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
	setaparam(arrayname, x);
    } else {
	/* set shell arguments */
	freearray(pparams);
	pparams = zarrdup(args);
    }
    unqueue_signals();
    return 0;
}

/**** directory-handling builtins ****/

/**/
int doprintdir = 0;		/* set in exec.c (for autocd) */

/* pwd: display the name of the current directory */

/**/
int
bin_pwd(char *name, char **argv, Options ops, int func)
{
    if (OPT_ISSET(ops,'r') || OPT_ISSET(ops,'P') ||
	(isset(CHASELINKS) && !OPT_ISSET(ops,'L')))
	printf("%s\n", zgetcwd());
    else {
	zputs(pwd, stdout);
	putchar('\n');
    }
    return 0;
}

/* the directory stack */
 
/**/
mod_export LinkList dirstack;
 
/* dirs: list the directory stack, or replace it with a provided list */

/**/
int
bin_dirs(char *name, char **argv, Options ops, int func)
{
    LinkList l;

    queue_signals();
    /* with -v, -p or no arguments display the directory stack */
    if (!(*argv || OPT_ISSET(ops,'c')) || OPT_ISSET(ops,'v') || 
	OPT_ISSET(ops,'p')) {
	LinkNode node;
	char *fmt;
	int pos = 1;

	/* with the -v option, display a numbered list, starting at zero */
	if (OPT_ISSET(ops,'v')) {
	    printf("0\t");
	    fmt = "\n%d\t";
	/* with the -p option, display entries one per line */
	} else if (OPT_ISSET(ops,'p'))
	    fmt = "\n";
	else
	    fmt = " ";
	if (OPT_ISSET(ops,'l'))
	    fputs(pwd, stdout);
	else
	    fprintdir(pwd, stdout);
	for (node = firstnode(dirstack); node; incnode(node)) {
	    printf(fmt, pos++);
	    if (OPT_ISSET(ops,'l'))
		fputs(getdata(node), stdout);
	    else
		fprintdir(getdata(node), stdout);

	}
	unqueue_signals();
	putchar('\n');
	return 0;
    }
    /* replace the stack with the specified directories */
    l = znewlinklist();
    while (*argv)
	zaddlinknode(l, ztrdup(*argv++));
    freelinklist(dirstack, freestr);
    dirstack = l;
    unqueue_signals();
    return 0;
}

/* cd, chdir, pushd, popd */

/**/
void
set_pwd_env(void)
{
    Param pm;

    /* update the PWD and OLDPWD shell parameters */

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
	pm->env = addenv("PWD", pwd, pm->flags);
    }
    pm = (Param) paramtab->getnode(paramtab, "OLDPWD");
    if (!(pm->flags & PM_EXPORTED)) {
	pm->flags |= PM_EXPORTED;
	pm->env = addenv("OLDPWD", oldpwd, pm->flags);
    }
}

/* set if we are resolving links to their true paths */
static int chasinglinks;

/* The main pwd changing function.  The real work is done by other     *
 * functions.  cd_get_dest() does the initial argument processing;     *
 * cd_do_chdir() actually changes directory, if possible; cd_new_pwd() *
 * does the ancillary processing associated with actually changing    *
 * directory.                                                          */

/**/
int
bin_cd(char *nam, char **argv, Options ops, int func)
{
    LinkNode dir;
    struct stat st1, st2;

    if (isset(RESTRICTED)) {
	zwarnnam(nam, "restricted", NULL, 0);
	return 1;
    }
    doprintdir = (doprintdir == -1);

    chasinglinks = OPT_ISSET(ops,'P') || 
	(isset(CHASELINKS) && !OPT_ISSET(ops,'L'));
    queue_signals();
    zpushnode(dirstack, ztrdup(pwd));
    if (!(dir = cd_get_dest(nam, argv, OPT_ISSET(ops,'s'), func))) {
	zsfree(getlinknode(dirstack));
	unqueue_signals();
	return 1;
    }
    cd_new_pwd(func, dir);

    if (stat(unmeta(pwd), &st1) < 0) {
	setjobpwd();
	zsfree(pwd);
	pwd = metafy(zgetcwd(), -1, META_DUP);
    } else if (stat(".", &st2) < 0)
	chdir(unmeta(pwd));
    else if (st1.st_ino != st2.st_ino || st1.st_dev != st2.st_dev) {
	if (chasinglinks) {
	    setjobpwd();
	    zsfree(pwd);
	    pwd = metafy(zgetcwd(), -1, META_DUP);
	} else {
	    chdir(unmeta(pwd));
	}
    }
    unqueue_signals();
    return 0;
}

/* Get directory to chdir to */

/**/
static LinkNode
cd_get_dest(char *nam, char **argv, int hard, int func)
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
	    zinsertlinknode(dirstack, dir, getlinknode(dirstack));
	else if (func != BIN_POPD)
	    zpushnode(dirstack, ztrdup(home));
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
	    zpushnode(dirstack, ztrdup(strcmp(argv[0], "-")
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
	zpushnode(dirstack, d);
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
    if (!(dest = cd_do_chdir(nam, getdata(dir), hard))) {
	if (!target)
	    zsfree(getlinknode(dirstack));
	if (func == BIN_POPD)
	    zsfree(remnode(dirstack, dir));
	return NULL;
    }
    if (dest != (char *)getdata(dir)) {
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
    /*
     * nocdpath indicates that cdpath should not be used.
     * This is the case iff dest is a relative path
     * whose first segment is . or .., but if the path is
     * absolute then cdpath won't be used anyway.
     */
    int nocdpath;
#ifdef __CYGWIN__
    /*
     * Normalize path under Cygwin to avoid messing with
     * DOS style names with drives in them
     */
    static char buf[PATH_MAX];
    void cygwin_conv_to_posix_path(const char *, char *);

    cygwin_conv_to_posix_path(dest, buf);
    dest = buf;
#endif
    nocdpath = dest[0] == '.' &&
	(dest[1] == '/' || !dest[1] || (dest[1] == '.' &&
					(dest[2] == '/' || !dest[2])));

    /*
     * If we have an absolute path, use it as-is only
     */
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
    int dlen, dochaselinks = 0;

    /* handle directory prefix */
    if (pfix && *pfix) {
	if (*pfix == '/')
	    buf = tricat(pfix, "/", dest);
	else {
	    int pfl = strlen(pfix);
	    dlen = strlen(pwd);

	    buf = zalloc(dlen + pfl + strlen(dest) + 3);
	    strcpy(buf, pwd);
	    buf[dlen] = '/';
	    strcpy(buf + dlen + 1, pfix);
	    buf[dlen + 1 + pfl] = '/';
	    strcpy(buf + dlen + pfl + 2, dest);
	}
    } else if (*dest == '/')
	buf = ztrdup(dest);
    else {
	dlen = strlen(pwd);
	if (pwd[dlen-1] == '/')
	    --dlen;
	buf = zalloc(dlen + strlen(dest) + 2);
	strcpy(buf, pwd);
	buf[dlen] = '/';
	strcpy(buf + dlen + 1, dest);
    }

    /* Normalise path.  See the definition of fixdir() for what this means.
     * We do not do this if we are chasing links.
     */
    if (!chasinglinks)
	dochaselinks = fixdir(buf);
    else
	unmetafy(buf, &dlen);

    /* We try the full path first.  If that fails, try the
     * argument to cd relatively.  This is useful if the cwd
     * or a parent directory is renamed in the interim.
     */
    if (lchdir(buf, NULL, hard) && lchdir(dest, NULL, hard)) {
	free(buf);
	return NULL;
    }
    /* the chdir succeeded, so decide if we should force links to be chased */
    if (dochaselinks)
	chasinglinks = 1;
    return metafy(buf, -1, META_NOALLOC);
}

/* do the extra processing associated with changing directory */

/**/
static void
cd_new_pwd(int func, LinkNode dir)
{
    Eprog prog;
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

    if (chasinglinks) {
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
    setjobpwd();
    pwd = new_pwd;
    set_pwd_env();

    if (isset(INTERACTIVE)) {
	if (unset(PUSHDSILENT) && func != BIN_CD)
	    printdirstack();
	else if (doprintdir) {
	    fprintdir(pwd, stdout);
	    putchar('\n');
	}
    }

    /* execute the chpwd function */
    if ((prog = getshfunc("chpwd")) != &dummy_eprog) {
	int osc = sfcontext;

	fflush(stdout);
	fflush(stderr);
	sfcontext = SFC_HOOK;
	doshfunc("chpwd", prog, NULL, 0, 1);
	sfcontext = osc;
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
 * combinations, are removed and the path is unmetafied.
 * Returns 1 if we found a ../ path which should force links to
 * be chased, 0 otherwise.
 */

/**/
int
fixdir(char *src)
{
    char *dest = src, *d0 = dest;
#ifdef __CYGWIN__
    char *s0 = src;
#endif
    int ret = 0;

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
#ifdef __CYGWIN__
	    /* allow leading // under cygwin */
	    if (src == s0 && src[1] == '/')
		*dest++ = *src++;
#endif
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
	    return ret;
	}
	if (src[0] == '.' && src[1] == '.' &&
	    (src[2] == '\0' || src[2] == '/')) {
	    if (isset(CHASEDOTS)) {
		ret = 1;
		/* and treat as normal path segment */
	    } else {
		if (dest > d0 + 1) {
		    /*
		     * remove a foo/.. combination:
		     * first check foo exists, else return.
		     */
		    struct stat st;
		    *dest = '\0';
		    if (stat(d0, &st) < 0 || !S_ISDIR(st.st_mode)) {
			char *ptrd, *ptrs;
			if (dest == src)
			    *dest = '.';
			for (ptrs = src, ptrd = dest; *ptrs; ptrs++, ptrd++)
			    *ptrd = (*ptrs == Meta) ? (*++ptrs ^ 32) : *ptrs;
			*ptrd = '\0';
			return 1;
		    }
		    for (dest--; dest > d0 + 1 && dest[-1] != '/'; dest--);
		    if (dest[-1] != '/')
			dest--;
		}
		src++;
		while (*++src == '/');
		continue;
	    }
	}
	if (src[0] == '.' && (src[1] == '/' || src[1] == '\0')) {
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
mod_export void
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
mod_export void
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
bin_fc(char *nam, char **argv, Options ops, int func)
{
    int first = -1, last = -1, retval;
    char *s;
    struct asgment *asgf = NULL, *asgl = NULL;
    Patprog pprog = NULL;

    /* fc is only permitted in interactive shells */
    if (!interact) {
	zwarnnam(nam, "not interactive shell", NULL, 0);
	return 1;
    }
    /* with the -m option, the first argument is taken *
     * as a pattern that history lines have to match   */
    if (*argv && OPT_ISSET(ops,'m')) {
	tokenize(*argv);
	if (!(pprog = patcompile(*argv++, 0, NULL))) {
	    zwarnnam(nam, "invalid match pattern", NULL, 0);
	    return 1;
	}
    }
    queue_signals();
    if (OPT_ISSET(ops,'R')) {
	/* read history from a file */
	readhistfile(*argv, 1, OPT_ISSET(ops,'I') ? HFILE_SKIPOLD : 0);
	unqueue_signals();
	return 0;
    }
    if (OPT_ISSET(ops,'W')) {
	/* write history to a file */
	savehistfile(*argv, 1, OPT_ISSET(ops,'I') ? HFILE_SKIPOLD : 0);
	unqueue_signals();
	return 0;
    }
    if (OPT_ISSET(ops,'A')) {
	/* append history to a file */
	savehistfile(*argv, 1, HFILE_APPEND | 
		     (OPT_ISSET(ops,'I') ? HFILE_SKIPOLD : 0));
	unqueue_signals();
	return 0;
    }
    /* put foo=bar type arguments into the substitution list */
    while (*argv && equalsplit(*argv, &s)) {
	Asgment a = (Asgment) zhalloc(sizeof *a);

	if (!**argv) {
	    zwarnnam(nam, "invalid replacement pattern: =%s", s, 0);
	    return 1;
	}
	if (!asgf)
	    asgf = asgl = a;
	else {
	    asgl->next = a;
	    asgl = a;
	}
	a->name = *argv;
	a->value = s;
	a->next = NULL;
	argv++;
    }
    /* interpret and check first history line specifier */
    if (*argv) {
	first = fcgetcomm(*argv);
	if (first == -1) {
	    unqueue_signals();
	    return 1;
	}
	argv++;
    }
    /* interpret and check second history line specifier */
    if (*argv) {
	last = fcgetcomm(*argv);
	if (last == -1) {
	    unqueue_signals();
	    return 1;
	}
	argv++;
    }
    /* There is a maximum of two history specifiers.  At least, there *
     * will be as long as the history list is one-dimensional.        */
    if (*argv) {
	unqueue_signals();
	zwarnnam("fc", "too many arguments", NULL, 0);
	return 1;
    }
    /* default values of first and last, and range checking */
    if (last == -1) {
	if (OPT_ISSET(ops,'l') && first < curhist) {
	    last = addhistnum(curline.histnum,-1,0);
	    if (last < firsthist())
		last = firsthist();
	}
	else
	    last = first;
    }
    if (first == -1) {
	first = OPT_ISSET(ops,'l')? addhistnum(curline.histnum,-16,0)
			: addhistnum(curline.histnum,-1,0);
	if (first < 1)
	    first = 1;
	if (last < first)
	    last = first;
    }
    if (OPT_ISSET(ops,'l')) {
	/* list the required part of the history */
	retval = fclist(stdout, ops, first, last, asgf, pprog);
	unqueue_signals();
    }
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
	    unqueue_signals();
	    zwarnnam("fc", "can't open temp file: %e", NULL, errno);
	} else {
	    ops->ind['n'] = 1;	/* No line numbers here. */
	    if (!fclist(out, ops, first, last, asgf, pprog)) {
		char *editor;

		if (func == BIN_R)
		    editor = "-";
		else if (OPT_HASARG(ops, 'e'))
		    editor = OPT_ARG(ops, 'e');
		else
		    editor = getsparam("FCEDIT");
		if (!editor)
		    editor = DEFAULT_FCEDIT;

		unqueue_signals();
		if (fcedit(editor, fil)) {
		    if (stuff(fil))
			zwarnnam("fc", "%e: %s", s, errno);
		    else {
			loop(0,1);
			retval = lastval;
		    }
		}
	    } else
		unqueue_signals();
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
    if ((cmd = atoi(s)) != 0 || *s == '0') {
	if (cmd < 0)
	    cmd = addhistnum(curline.histnum,cmd,HIST_FOREIGN);
	if (cmd < 0)
	    cmd = 0;
	return cmd;
    }
    /* not a number, so search by string */
    cmd = hcomsearch(s);
    if (cmd == -1)
	zwarnnam("fc", "event not found: %s", s, 0);
    return cmd;
}

/* Perform old=new substitutions.  Uses the asgment structure from zsh.h, *
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
	    newmem = (char *) zhalloc(1 + (newpos - s)
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
fclist(FILE *f, Options ops, int first, int last, struct asgment *subs, Patprog pprog)
{
    int fclistdone = 0, tmp;
    char *s;
    Histent ent;

    /* reverse range if required */
    if (OPT_ISSET(ops,'r')) {
	tmp = last;
	last = first;
	first = tmp;
    }
    /* suppress "no substitution" warning if no substitution is requested */
    if (!subs)
	fclistdone = 1;

    ent = gethistent(first, first < last? GETHIST_DOWNWARD : GETHIST_UPWARD);
    if (!ent || (first < last? ent->histnum > last : ent->histnum < last)) {
	if (first == last)
	    zwarnnam("fc", "no such event: %d", NULL, first);
	else
	    zwarnnam("fc", "no events in that range", NULL, 0);
	return 1;
    }

    for (;;) {
	s = dupstring(ent->text);
	/* this if does the pattern matching, if required */
	if (!pprog || pattry(pprog, s)) {
	    /* perform substitution */
	    fclistdone |= fcsubs(&s, subs);

	    /* do numbering */
	    if (!OPT_ISSET(ops,'n')) {
		fprintf(f, "%5d%c ", ent->histnum,
			ent->flags & HIST_FOREIGN? '*' : ' ');
	    }
	    /* output actual time (and possibly date) of execution of the
	       command, if required */
	    if (OPT_ISSET(ops,'d') || OPT_ISSET(ops,'f') ||
		OPT_ISSET(ops,'E') || OPT_ISSET(ops,'i')) {
		struct tm *ltm;
		ltm = localtime(&ent->stim);
		if (OPT_ISSET(ops,'i')) {
		    fprintf(f, "%d-%02d-%02d ",
			    ltm->tm_year + 1900,
			    ltm->tm_mon + 1, ltm->tm_mday);
		} else if (OPT_ISSET(ops,'E')) {
		    fprintf(f, "%d.%d.%d ",
			    ltm->tm_mday, ltm->tm_mon + 1,
			    ltm->tm_year + 1900);
		} else if (OPT_ISSET(ops,'f')) {
		    fprintf(f, "%d/%d/%d ",
			    ltm->tm_mon + 1, ltm->tm_mday,
			    ltm->tm_year + 1900);
		}
		fprintf(f, "%02d:%02d  ", ltm->tm_hour, ltm->tm_min);
	    }
	    /* display the time taken by the command, if required */
	    if (OPT_ISSET(ops,'D')) {
		long diff;
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
	if (first < last) {
	    if (!(ent = down_histent(ent)) || ent->histnum > last)
		break;
	}
	else {
	    if (!(ent = up_histent(ent)) || ent->histnum < last)
		break;
	}
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

/* for new special parameters */
enum {
    NS_NONE,
    NS_NORMAL,
    NS_SECONDS
};

/* function to set a single parameter */

/**/
static Param
typeset_single(char *cname, char *pname, Param pm, int func,
	       int on, int off, int roff, char *value, Param altpm,
	       Options ops, int auxlen)
{
    int usepm, tc, keeplocal = 0, newspecial = NS_NONE, readonly;
    char *subscript;

    /*
     * Do we use the existing pm?  Note that this isn't the end of the
     * story, because if we try and create a new pm at the same
     * locallevel as an unset one we use the pm struct anyway: that's
     * handled in createparam().  Here we just avoid using it for the
     * present tests if it's unset.
     */
    usepm = pm && !(pm->flags & PM_UNSET);

    /*
     * We need to compare types with an existing pm if special,
     * even if that's unset
     */
    if (pm && (pm->flags & PM_SPECIAL))
	usepm = 1;

    /*
     * Don't use an existing param if
     *   - the local level has changed, and
     *   - we are really locallizing the parameter
     */
    if (usepm && locallevel != pm->level && (on & PM_LOCAL)) {
	/*
	 * If the original parameter was special and we're creating
	 * a new one, we need to keep it special.
	 *
	 * The -h (hide) flag prevents an existing special being made
	 * local.  It can be applied either to the special or in the
	 * typeset/local statement for the local variable.
	 */
	if ((pm->flags & PM_SPECIAL)
	    && !(on & PM_HIDE) && !(pm->flags & PM_HIDE & ~off))
	    newspecial = NS_NORMAL;
	usepm = 0;
    }

    /* attempting a type conversion, or making a tied colonarray? */
    tc = 0;
    if (usepm || newspecial != NS_NONE) {
	int chflags = ((off & pm->flags) | (on & ~pm->flags)) &
	    (PM_INTEGER|PM_EFLOAT|PM_FFLOAT|PM_HASHED|
	     PM_ARRAY|PM_TIED|PM_AUTOLOAD);
	/* keep the parameter if just switching between floating types */
	if ((tc = chflags && chflags != (PM_EFLOAT|PM_FFLOAT)))
	    usepm = 0;
    }

    /*
     * Extra checks if converting the type of a parameter, or if
     * trying to remove readonlyness.  It's dangerous doing either
     * with a special or a parameter which isn't loaded yet (which
     * may be special when it is loaded; we can't tell yet).
     */
    if ((readonly =
	 ((usepm || newspecial != NS_NONE) && 
	  (off & pm->flags & PM_READONLY))) ||
	tc) {
	if (pm->flags & PM_SPECIAL) {
	    int err = 1;
	    if (!readonly && !strcmp(pname, "SECONDS"))
	    {
		/*
		 * We allow SECONDS to change type between integer
		 * and floating point.  If we are creating a new
		 * local copy we check the type here and allow
		 * a new special to be created with that type.
		 * We then need to make sure the correct type
		 * for the special is restored at the end of the scope.
		 * If we are changing the type of an existing
		 * parameter, we do the whole thing here.
		 */
		if (newspecial != NS_NONE)
		{
		    /*
		     * The first test allows `typeset' to copy the
		     * existing type.  This is the usual behaviour
		     * for making special parameters local.
		     */
		    if (PM_TYPE(on) == 0 || PM_TYPE(on) == PM_INTEGER ||
			PM_TYPE(on) == PM_FFLOAT || PM_TYPE(on) == PM_EFLOAT)
		    {
			newspecial = NS_SECONDS;
			err = 0;	/* and continue */
			tc = 0;	/* but don't do a normal conversion */
		    }
		} else if (!setsecondstype(pm, on, off)) {
		    if (value && !setsparam(pname, ztrdup(value)))
			return NULL;
		    return pm;
		}
	    }
	    if (err)
	    {
		zerrnam(cname, "%s: can't change type of a special parameter",
			pname, 0);
		return NULL;
	    }
	} else if (pm->flags & PM_AUTOLOAD) {
	    zerrnam(cname, "%s: can't change type of autoloaded parameter",
		    pname, 0);
	    return NULL;
	}
    }
    else if (newspecial != NS_NONE && strcmp(pname, "SECONDS") == 0)
	newspecial = NS_SECONDS;

    /*
     * A parameter will be local if
     * 1. we are re-using an existing local parameter
     *    or
     * 2. we are not using an existing parameter, but
     *   i. there is already a parameter, which will be hidden
     *     or
     *   ii. we are creating a new local parameter
     */
    if (usepm) {
	on &= ~PM_LOCAL;
	if (!on && !roff && !value) {
	    if (OPT_ISSET(ops,'p'))
		paramtab->printnode((HashNode)pm, PRINT_TYPESET);
	    else if (unset(TYPESETSILENT) || OPT_ISSET(ops,'m'))
		paramtab->printnode((HashNode)pm, PRINT_INCLUDEVALUE);
	    return pm;
	}
	if ((pm->flags & PM_RESTRICTED) && isset(RESTRICTED)) {
	    zerrnam(cname, "%s: restricted", pname, 0);
	    return pm;
	}
	if ((on & PM_UNIQUE) && !(pm->flags & PM_READONLY & ~off)) {
	    Param apm;
	    char **x;
	    if (PM_TYPE(pm->flags) == PM_ARRAY) {
		x = (*pm->gets.afn)(pm);
		uniqarray(x);
		if (pm->ename && x)
		    arrfixenv(pm->ename, x);
	    } else if (PM_TYPE(pm->flags) == PM_SCALAR && pm->ename &&
		       (apm =
			(Param) paramtab->getnode(paramtab, pm->ename))) {
		x = (*apm->gets.afn)(apm);
		uniqarray(x);
		if (x)
		    arrfixenv(pm->nam, x);
	    }
	}
	pm->flags = (pm->flags | (on & ~PM_READONLY)) & ~(off | PM_UNSET);
	/* This auxlen/pm->ct stuff is a nasty hack. */
	if ((on & (PM_LEFT | PM_RIGHT_B | PM_RIGHT_Z | PM_INTEGER |
		   PM_EFLOAT | PM_FFLOAT)) &&
	    auxlen)
	    pm->ct = auxlen;
	if (!(pm->flags & (PM_ARRAY|PM_HASHED))) {
	    if (pm->flags & PM_EXPORTED) {
		if (!(pm->flags & PM_UNSET) && !pm->env && !value)
		    pm->env = addenv(pname, getsparam(pname), pm->flags);
	    } else if (pm->env && !(pm->flags & PM_HASHELEM)) {
		delenv(pm->env);
		pm->env = NULL;
	    }
	    if (value && !(pm = setsparam(pname, ztrdup(value))))
		return NULL;
	} else if (value) {
	    zwarnnam(cname, "can't assign new value for array %s", pname, 0);
	    return NULL;
	}
	pm->flags |= (on & PM_READONLY);
	if (OPT_ISSET(ops,'p'))
	    paramtab->printnode((HashNode)pm, PRINT_TYPESET);
	return pm;
    }

    /*
     * We're here either because we're creating a new parameter,
     * or we're adding a parameter at a different local level,
     * or we're converting the type of a parameter.  In the
     * last case only, we need to delete the old parameter.
     */
    if (tc) {
	/* Maintain existing readonly/exported status... */
	on |= ~off & (PM_READONLY|PM_EXPORTED) & pm->flags;
	/* ...but turn off existing readonly so we can delete it */
	pm->flags &= ~PM_READONLY;
	/*
	 * If we're just changing the type, we should keep the
	 * variable at the current level of localness.
	 */
	keeplocal = pm->level;
	/*
	 * Try to carry over a value, but not when changing from,
	 * to, or between non-scalar types.
	 */
	if (!value && !((pm->flags|on) & (PM_ARRAY|PM_HASHED)))
	    value = dupstring(getsparam(pname));
	/* pname may point to pm->nam which is about to disappear */
	pname = dupstring(pname);
	unsetparam_pm(pm, 0, 1);
    }

    if (newspecial != NS_NONE) {
	Param tpm, pm2;
	if ((pm->flags & PM_RESTRICTED) && isset(RESTRICTED)) {
	    zerrnam(cname, "%s: restricted", pname, 0);
	    return pm;
	}
	/*
	 * For specials, we keep the same struct but zero everything.
	 * Maybe it would be easier to create a new struct but copy
	 * the get/set methods.
	 */
	tpm = (Param) zalloc(sizeof *tpm);

	tpm->nam = pm->nam;
	if (pm->ename &&
	    (pm2 = (Param) paramtab->getnode(paramtab, pm->ename)) &&
	    pm2->level == locallevel) {
	    /* This is getting silly, but anyway:  if one of a path/PATH
	     * pair has already been made local at the current level, we
	     * have to make sure that the other one does not have its value
	     * saved:  since that comes from an internal variable it will
	     * already reflect the local value, so restoring it on exit
	     * would be wrong.
	     *
	     * This problem is also why we make sure we have a copy
	     * of the environment entry in tpm->env, rather than relying
	     * on the restored value to provide it.
	     */
	    tpm->flags = pm->flags | PM_NORESTORE;
	} else {
	    copyparam(tpm, pm, 1);
	}
	tpm->old = pm->old;
	tpm->level = pm->level;
	tpm->ct = pm->ct;
	if (pm->env) {
	    delenv(pm->env);
	}
	tpm->env = pm->env = NULL;

	pm->old = tpm;
	/*
	 * The remaining on/off flags should be harmless to use,
	 * because we've checked for unpleasant surprises above.
	 */
	pm->flags = (PM_TYPE(pm->flags) | on | PM_SPECIAL) & ~off;
	if (newspecial == NS_SECONDS) {
	    /* We save off the raw internal value of the SECONDS var */
	    tpm->u.dval = getrawseconds();
	    setsecondstype(pm, on, off);
	}

	/*
	 * Final tweak: if we've turned on one of the flags with
	 * numbers, we should use the appropriate integer.
	 */
	if (on & (PM_LEFT|PM_RIGHT_B|PM_RIGHT_Z|PM_INTEGER|
		  PM_EFLOAT|PM_FFLOAT))
	    pm->ct = auxlen;
	else
	    pm->ct = 0;
    } else if ((subscript = strchr(pname, '['))) {
	if (on & PM_READONLY) {
	    zerrnam(cname,
		    "%s: can't create readonly array elements", pname, 0);
	    return NULL;
	} else if (on & PM_LOCAL) {
	    *subscript = 0;
	    pm = (Param) (paramtab == realparamtab ?
			  gethashnode2(paramtab, pname) :
			  paramtab->getnode(paramtab, pname));
	    *subscript = '[';
	    if (!pm || pm->level != locallevel) {
		zerrnam(cname,
			"%s: can't create local array elements", pname, 0);
		return NULL;
	    }
	}
	if (PM_TYPE(on) == PM_SCALAR) {
	    /*
	     * This will either complain about bad identifiers, or will set
	     * a hash element or array slice.  This once worked by accident,
	     * creating a stray parameter along the way via createparam(),
	     * now called below in the isident() branch.
	     */
	    if (!(pm = setsparam(pname, ztrdup(value ? value : ""))))
		return NULL;
	    value = NULL;
	    keeplocal = 0;
	    on = pm->flags;
	} else {
	    zerrnam(cname,
		    "%s: array elements must be scalar", pname, 0);
	    return NULL;
	}
    } else if (isident(pname) && !idigit(*pname)) {
	/*
	 * Create a new node for a parameter with the flags in `on' minus the
	 * readonly flag
	 */
	pm = createparam(pname, on & ~PM_READONLY);
	DPUTS(!pm, "BUG: parameter not created");
	pm->ct = auxlen;
    } else {
	if (isident(pname))
	    zerrnam(cname, "not valid in this context: %s", pname, 0);
	else
	    zerrnam(cname, "not an identifier: %s", pname, 0);
	return NULL;
    }

    if (altpm && PM_TYPE(pm->flags) == PM_SCALAR) {
	/*
	 * It seems safer to set this here than in createparam(),
	 * to make sure we only ever use the colonarr functions
	 * when u.data is correctly set.
	 */
	pm->sets.cfn = colonarrsetfn;
	pm->gets.cfn = colonarrgetfn;
	pm->u.data = &altpm->u.arr;
    }

    if (keeplocal)
	pm->level = keeplocal;
    else if (on & PM_LOCAL)
	pm->level = locallevel;
    if (value && !(pm->flags & (PM_ARRAY|PM_HASHED))) {
	Param ipm = pm;
	if (!(pm = setsparam(pname, ztrdup(value))))
	    return NULL;
	if (pm != ipm) {
	    DPUTS(ipm->flags != pm->flags,
		  "BUG: parameter recreated with wrong flags");
	    unsetparam_pm(ipm, 0, 1);
	}
    } else if (newspecial != NS_NONE && !(pm->old->flags & PM_NORESTORE)) {
	/*
	 * We need to use the special setting function to re-initialise
	 * the special parameter to empty.
	 */
	switch (PM_TYPE(pm->flags)) {
	case PM_SCALAR:
	    pm->sets.cfn(pm, ztrdup(""));
	    break;
	case PM_INTEGER:
	    pm->sets.ifn(pm, 0);
	    break;
	case PM_EFLOAT:
	case PM_FFLOAT:
	    pm->sets.ffn(pm, 0.0);
	    break;
	case PM_ARRAY:
	    pm->sets.afn(pm, mkarray(NULL));
	    break;
	case PM_HASHED:
	    pm->sets.hfn(pm, newparamtable(17, pm->nam));
	    break;
	}
    }
    pm->flags |= (on & PM_READONLY);
    if (value && (pm->flags & (PM_ARRAY|PM_HASHED))) {
	zerrnam(cname, "%s: can't assign initial value for array", pname, 0);
	/* the only safe thing to do here seems to be unset the param */
	unsetparam_pm(pm, 0, 1);
	return NULL;
    }

    if (OPT_ISSET(ops,'p'))
	paramtab->printnode((HashNode)pm, PRINT_TYPESET);

    return pm;
}

/* declare, export, integer, local, readonly, typeset */

/**/
int
bin_typeset(char *name, char **argv, Options ops, int func)
{
    Param pm;
    Asgment asg;
    Patprog pprog;
    char *optstr = TYPESET_OPTSTR;
    int on = 0, off = 0, roff, bit = PM_ARRAY;
    int i;
    int returnval = 0, printflags = 0, auxlen = 0;

    /* hash -f is really the builtin `functions' */
    if (OPT_ISSET(ops,'f'))
	return bin_functions(name, argv, ops, func);

    /* Translate the options into PM_* flags.   *
     * Unfortunately, this depends on the order *
     * these flags are defined in zsh.h         */
    for (; *optstr; optstr++, bit <<= 1)
    {
	int optval = STOUC(*optstr);
	if (OPT_MINUS(ops,optval))
	    on |= bit;
	else if (OPT_PLUS(ops,optval))
	    off |= bit;
	/*
	 * There is only a single field in struct param for widths,
	 * precisions and bases.  Until this gets fixed, we can therefore
	 * bundle all optional arguments up into a single word.  You
	 * may think this is very nasty, but then you should have seen the
	 * code before option arguments were handled properly.
	 */
	if (OPT_HASARG(ops,optval)) {
	    char *eptr, *arg = OPT_ARG(ops,optval);
	    auxlen = (int)zstrtol(arg, &eptr, 10);
	    if (*eptr) {
		zwarnnam(name, "bad integer value: %s", arg, 0);
		return 1;
	    }
	}
    }
    roff = off;

    /* Sanity checks on the options.  Remove conflicting options. */
    if (on & PM_FFLOAT) {
	off |= PM_RIGHT_B | PM_LEFT | PM_RIGHT_Z | PM_UPPER | PM_ARRAY |
	    PM_HASHED | PM_INTEGER | PM_EFLOAT;
	/* Allow `float -F' to work even though float sets -E by default */
	on &= ~PM_EFLOAT;
    }
    if (on & PM_EFLOAT)
	off |= PM_RIGHT_B | PM_LEFT | PM_RIGHT_Z | PM_UPPER | PM_ARRAY |
	    PM_HASHED | PM_INTEGER | PM_FFLOAT;
    if (on & PM_INTEGER)
	off |= PM_RIGHT_B | PM_LEFT | PM_RIGHT_Z | PM_UPPER | PM_ARRAY |
	    PM_HASHED | PM_EFLOAT | PM_FFLOAT;
    if (on & PM_LEFT)
	off |= PM_RIGHT_B | PM_INTEGER | PM_EFLOAT | PM_FFLOAT;
    if (on & PM_RIGHT_B)
	off |= PM_LEFT | PM_INTEGER | PM_EFLOAT | PM_FFLOAT;
    if (on & PM_RIGHT_Z)
	off |= PM_INTEGER | PM_EFLOAT | PM_FFLOAT;
    if (on & PM_UPPER)
	off |= PM_LOWER;
    if (on & PM_LOWER)
	off |= PM_UPPER;
    if (on & PM_HASHED)
	off |= PM_ARRAY;
    if (on & PM_TIED)
	off |= PM_INTEGER | PM_EFLOAT | PM_FFLOAT | PM_ARRAY | PM_HASHED;

    on &= ~off;

    queue_signals();

    /* Given no arguments, list whatever the options specify. */
    if (OPT_ISSET(ops,'p'))
	printflags |= PRINT_TYPESET;
    if (!*argv) {
	if (!OPT_ISSET(ops,'p')) {
	    if (!(on|roff))
		printflags |= PRINT_TYPE;
	    if (roff || OPT_ISSET(ops,'+'))
		printflags |= PRINT_NAMEONLY;
	}
	scanhashtable(paramtab, 1, on|roff, 0, paramtab->printnode, printflags);
	unqueue_signals();
	return 0;
    }

    if (!(OPT_ISSET(ops,'g') || OPT_ISSET(ops,'x') || OPT_ISSET(ops,'m')) || 
	OPT_PLUS(ops,'g') || *name == 'l' ||
	(!isset(GLOBALEXPORT) && !OPT_ISSET(ops,'g')))
	on |= PM_LOCAL;

    if (on & PM_TIED) {
	Param apm;
	struct asgment asg0;
	char *oldval = NULL;

	if (OPT_ISSET(ops,'m')) {
	    zwarnnam(name, "incompatible options for -T", NULL, 0);
	    unqueue_signals();
	    return 1;
	}
	on &= ~off;
	if (!argv[1] || argv[2]) {
	    zwarnnam(name, "-T requires names of scalar and array", NULL, 0);
	    unqueue_signals();
	    return 1;
	}

	if (!(asg = getasg(argv[0]))) {
	    unqueue_signals();
	    return 1;
	}
	asg0 = *asg;
	if (!(asg = getasg(argv[1]))) {
	    unqueue_signals();
	    return 1;
	}
	if (!strcmp(asg0.name, asg->name)) {
	    unqueue_signals();
	    zerrnam(name, "can't tie a variable to itself", NULL, 0);
	    return 1;
	}
	if (strchr(asg0.name, '[') || strchr(asg->name, '[')) {
	    unqueue_signals();
	    zerrnam(name, "can't tie array elements", NULL, 0);
	    return 1;
	}
	/*
	 * Keep the old value of the scalar.  We need to do this
	 * here as if it is already tied to the same array it
	 * will be unset when we retie the array.  This is all
	 * so that typeset -T is idempotent.
	 *
	 * We also need to remember here whether the damn thing is
	 * exported and pass that along.  Isn't the world complicated?
	 */
	if ((pm = (Param) paramtab->getnode(paramtab, asg0.name))
	    && !(pm->flags & PM_UNSET)
	    && (locallevel == pm->level || !(on & PM_LOCAL))) {
	    if (!asg0.value && !(PM_TYPE(pm->flags) & (PM_ARRAY|PM_HASHED)))
		oldval = ztrdup(getsparam(asg0.name));
	    on |= (pm->flags & PM_EXPORTED);
	}
	/*
	 * Create the tied array; this is normal except that
	 * it has the PM_TIED flag set.  Do it first because
	 * we need the address.
	 */
	if (!(apm=typeset_single(name, asg->name,
				 (Param)paramtab->getnode(paramtab,
							  asg->name),
				 func, (on | PM_ARRAY) & ~PM_EXPORTED,
				 off, roff, asg->value, NULL, ops, auxlen))) {
	    unqueue_signals();
	    return 1;
	}
	/*
	 * Create the tied colonarray.  We make it as a normal scalar
	 * and fix up the oddities later.
	 */
	if (!(pm=typeset_single(name, asg0.name,
				(Param)paramtab->getnode(paramtab,
							 asg0.name),
				func, on, off, roff, asg0.value, apm,
				ops, auxlen))) {
	    if (oldval)
		zsfree(oldval);
	    unsetparam_pm(apm, 1, 1);
	    unqueue_signals();
	    return 1;
	}

	pm->ename = ztrdup(asg->name);
	apm->ename = ztrdup(asg0.name);
	if (oldval)
	    setsparam(asg0.name, oldval);
	unqueue_signals();

	return 0;
    }
    if (off & PM_TIED) {
	zerrnam(name, "use unset to remove tied variables", NULL, 0);
	return 1;
    }

    /* With the -m option, treat arguments as glob patterns */
    if (OPT_ISSET(ops,'m')) {
	if (!OPT_ISSET(ops,'p')) {
	    if (!(on|roff))
		printflags |= PRINT_TYPE;
	    if (!on)
		printflags |= PRINT_NAMEONLY;
	}

	while ((asg = getasg(*argv++))) {
	    LinkList pmlist = newlinklist();
	    LinkNode pmnode;

	    tokenize(asg->name);   /* expand argument */
	    if (!(pprog = patcompile(asg->name, 0, NULL))) {
		untokenize(asg->name);
		zwarnnam(name, "bad pattern : %s", argv[-1], 0);
		returnval = 1;
		continue;
	    }
	    if (OPT_PLUS(ops,'m') && !asg->value) {
		scanmatchtable(paramtab, pprog, on|roff, 0,
			       paramtab->printnode, printflags);
		continue;
	    }
	    /*
	     * Search through the parameter table and change all parameters
	     * matching the glob pattern to have these flags and/or value.
	     * Bad news:  if the parameter gets altered, e.g. by
	     * a type conversion, then paramtab can be shifted around,
	     * so we need to store the parameters to alter on a separate
	     * list for later use.	     
	     */
	    for (i = 0; i < paramtab->hsize; i++) {
		for (pm = (Param) paramtab->nodes[i]; pm;
		     pm = (Param) pm->next) {
		    if (((pm->flags & PM_RESTRICTED) && isset(RESTRICTED)) ||
			(pm->flags & PM_UNSET))
			continue;
		    if (pattry(pprog, pm->nam))
			addlinknode(pmlist, pm);
		}
	    }
	    for (pmnode = firstnode(pmlist); pmnode; incnode(pmnode)) {
		pm = (Param) getdata(pmnode);
		if (!typeset_single(name, pm->nam, pm, func, on, off, roff,
				    asg->value, NULL, ops, auxlen))
		    returnval = 1;
	    }
	}
	unqueue_signals();
	return returnval;
    }

    /* Take arguments literally.  Don't glob */
    while ((asg = getasg(*argv++))) {
	if (!typeset_single(name, asg->name,
			    (Param) (paramtab == realparamtab ?
				     gethashnode2(paramtab, asg->name) :
				     paramtab->getnode(paramtab, asg->name)),
			    func, on, off, roff, asg->value, NULL,
			    ops, auxlen))
	    returnval = 1;
    }
    unqueue_signals();
    return returnval;
}

/* Helper for bin_functions() when run as "autoload -X" */

/**/
int
eval_autoload(Shfunc shf, char *name, Options ops, int func)
{
    if (!(shf->flags & PM_UNDEFINED))
	return 1;

    if (shf->funcdef) {
	freeeprog(shf->funcdef);
	shf->funcdef = &dummy_eprog;
    }
    if (OPT_MINUS(ops,'X')) {
	char *fargv[3];
	fargv[0] = name;
	fargv[1] = "\"$@\"";
	fargv[2] = 0;
	shf->funcdef = mkautofn(shf);
	return bin_eval(name, fargv, ops, func);
    }

    return !loadautofn(shf, (OPT_ISSET(ops,'k') ? 2 : 
			     (OPT_ISSET(ops,'z') ? 0 : 1)), 1);
}

/* Display or change the attributes of shell functions.   *
 * If called as autoload, it will define a new autoloaded *
 * (undefined) shell function.                            */

/**/
int
bin_functions(char *name, char **argv, Options ops, int func)
{
    Patprog pprog;
    Shfunc shf;
    int i, returnval = 0;
    int on = 0, off = 0, pflags = 0;

    /* Do we have any flags defined? */
    if (OPT_PLUS(ops,'u'))
	off |= PM_UNDEFINED;
    else if (OPT_MINUS(ops,'u') || OPT_ISSET(ops,'X'))
	on |= PM_UNDEFINED;
    if (OPT_MINUS(ops,'U'))
	on |= PM_UNALIASED|PM_UNDEFINED;
    else if (OPT_PLUS(ops,'U'))
	off |= PM_UNALIASED;
    if (OPT_MINUS(ops,'t'))
	on |= PM_TAGGED;
    else if (OPT_PLUS(ops,'t'))
	off |= PM_TAGGED;

    if ((off & PM_UNDEFINED) || (OPT_ISSET(ops,'k') && OPT_ISSET(ops,'z')) ||
	(!OPT_PLUS(ops,'X') && (OPT_ISSET(ops,'k') || OPT_ISSET(ops,'z'))) ||
	(OPT_MINUS(ops,'X') && (OPT_ISSET(ops,'m') || *argv || !scriptname))) {
	zwarnnam(name, "invalid option(s)", NULL, 0);
	return 1;
    }

    if (OPT_PLUS(ops,'f') || OPT_ISSET(ops,'+'))
	pflags |= PRINT_NAMEONLY;

    /* If no arguments given, we will print functions.  If flags *
     * are given, we will print only functions containing these  *
     * flags, else we'll print them all.                         */
    if (!*argv) {
	int ret = 0;

	queue_signals();
	if (OPT_MINUS(ops,'X')) {
	    if ((shf = (Shfunc) shfunctab->getnode(shfunctab, scriptname))) {
		DPUTS(!shf->funcdef,
		      "BUG: Calling autoload from empty function");
	    } else {
		shf = (Shfunc) zcalloc(sizeof *shf);
		shfunctab->addnode(shfunctab, ztrdup(scriptname), shf);
	    }
	    shf->flags = on;
	    ret = eval_autoload(shf, scriptname, ops, func);
	} else {
	    if (OPT_ISSET(ops,'U') && !OPT_ISSET(ops,'u'))
		on &= ~PM_UNDEFINED;
	    scanhashtable(shfunctab, 1, on|off, DISABLED, shfunctab->printnode,
			  pflags);
	}
	unqueue_signals();
	return ret;
    }

    /* With the -m option, treat arguments as glob patterns */
    if (OPT_ISSET(ops,'m')) {
	on &= ~PM_UNDEFINED;
	for (; *argv; argv++) {
	    /* expand argument */
	    tokenize(*argv);
	    if ((pprog = patcompile(*argv, PAT_STATIC, 0))) {
		/* with no options, just print all functions matching the glob pattern */
		queue_signals();
		if (!(on|off)) {
		    scanmatchtable(shfunctab, pprog, 0, DISABLED,
				   shfunctab->printnode, pflags);
		} else {
		    /* apply the options to all functions matching the glob pattern */
		    for (i = 0; i < shfunctab->hsize; i++) {
			for (shf = (Shfunc) shfunctab->nodes[i]; shf;
			     shf = (Shfunc) shf->next)
			    if (pattry(pprog, shf->nam) &&
				!(shf->flags & DISABLED)) {
				shf->flags = (shf->flags |
					      (on & ~PM_UNDEFINED)) & ~off;
				if (OPT_ISSET(ops,'X') &&
				    eval_autoload(shf, shf->nam, ops, func)) {
				    returnval = 1;
				}
			    }
		    }
		}
		unqueue_signals();
	    } else {
		untokenize(*argv);
		zwarnnam(name, "bad pattern : %s", *argv, 0);
		returnval = 1;
	    }
	}
	return returnval;
    }

    /* Take the arguments literally -- do not glob */
    queue_signals();
    for (; *argv; argv++) {
	if (OPT_ISSET(ops,'w'))
	    returnval = dump_autoload(name, *argv, on, ops, func);
	else if ((shf = (Shfunc) shfunctab->getnode(shfunctab, *argv))) {
	    /* if any flag was given */
	    if (on|off) {
		/* turn on/off the given flags */
		shf->flags = (shf->flags | (on & ~PM_UNDEFINED)) & ~off;
		if (OPT_ISSET(ops,'X') && 
		    eval_autoload(shf, shf->nam, ops, func))
		    returnval = 1;
	    } else
		/* no flags, so just print */
		shfunctab->printnode((HashNode) shf, pflags);
	} else if (on & PM_UNDEFINED) {
	    /* Add a new undefined (autoloaded) function to the *
	     * hash table with the corresponding flags set.     */
	    shf = (Shfunc) zcalloc(sizeof *shf);
	    shf->flags = on;
	    shf->funcdef = mkautofn(shf);
	    shfunctab->addnode(shfunctab, ztrdup(*argv), shf);
	    if (OPT_ISSET(ops,'X') && eval_autoload(shf, shf->nam, ops, func))
		returnval = 1;
	} else
	    returnval = 1;
    }
    unqueue_signals();
    return returnval;
}

/**/
Eprog
mkautofn(Shfunc shf)
{
    Eprog p;

    p = (Eprog) zalloc(sizeof(*p));
    p->len = 5 * sizeof(wordcode);
    p->prog = (Wordcode) zalloc(p->len);
    p->strs = NULL;
    p->shf = shf;
    p->npats = 0;
    p->nref = 1; /* allocated from permanent storage */
    p->pats = (Patprog *) p->prog;
    p->flags = EF_REAL;
    p->dump = NULL;

    p->prog[0] = WCB_LIST((Z_SYNC | Z_END), 0);
    p->prog[1] = WCB_SUBLIST(WC_SUBLIST_END, 0, 3);
    p->prog[2] = WCB_PIPE(WC_PIPE_END, 0);
    p->prog[3] = WCB_AUTOFN();
    p->prog[4] = WCB_END();

    return p;
}

/* unset: unset parameters */

/**/
int
bin_unset(char *name, char **argv, Options ops, int func)
{
    Param pm, next;
    Patprog pprog;
    char *s;
    int match = 0, returnval = 0;
    int i;

    /* unset -f is the same as unfunction */
    if (OPT_ISSET(ops,'f'))
	return bin_unhash(name, argv, ops, func);

    /* with -m option, treat arguments as glob patterns */
    if (OPT_ISSET(ops,'m')) {
	while ((s = *argv++)) {
	    /* expand */
	    tokenize(s);
	    if ((pprog = patcompile(s, PAT_STATIC, NULL))) {
		/* Go through the parameter table, and unset any matches */
		queue_signals();
		for (i = 0; i < paramtab->hsize; i++) {
		    for (pm = (Param) paramtab->nodes[i]; pm; pm = next) {
			/* record pointer to next, since we may free this one */
			next = (Param) pm->next;
			if ((!(pm->flags & PM_RESTRICTED) ||
			     unset(RESTRICTED)) &&
			    pattry(pprog, pm->nam)) {
			    unsetparam_pm(pm, 0, 1);
			    match++;
			}
		    }
		}
		unqueue_signals();
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
    queue_signals();
    while ((s = *argv++)) {
	char *ss = strchr(s, '[');
	char *sse = ss;
	if (ss) {
	    if (skipparens('[', ']', &sse) || *sse) {
		zerrnam(name, "%s: invalid parameter name", s, 0);
		returnval = 1;
		continue;
	    }
	    *ss = 0;
	}
	pm = (Param) (paramtab == realparamtab ?
		      gethashnode2(paramtab, s) :
		      paramtab->getnode(paramtab, s));
	if (!pm)
	    returnval = 1;
	else if ((pm->flags & PM_RESTRICTED) && isset(RESTRICTED)) {
	    zerrnam(name, "%s: restricted", pm->nam, 0);
	    returnval = 1;
	} else if (ss) {
	    if (PM_TYPE(pm->flags) == PM_HASHED) {
		HashTable tht = paramtab;
		if ((paramtab = pm->gets.hfn(pm))) {
		    *--sse = 0;
		    unsetparam(ss+1);
		    *sse = ']';
		}
		paramtab = tht;
	    } else {
		zerrnam(name, "%s: invalid element for unset", s, 0);
		returnval = 1;
	    }
	} else {
	    if (unsetparam_pm(pm, 0, 1))
		returnval = 1;
	}
	if (ss)
	    *ss = '[';
    }
    unqueue_signals();
    return returnval;
}

/* type, whence, which */

/**/
int
bin_whence(char *nam, char **argv, Options ops, int func)
{
    HashNode hn;
    Patprog pprog;
    int returnval = 0;
    int printflags = 0;
    int csh, all, v, wd;
    int informed;
    char *cnam;

    /* Check some option information */
    csh = OPT_ISSET(ops,'c');
    v   = OPT_ISSET(ops,'v');
    all = OPT_ISSET(ops,'a');
    wd  = OPT_ISSET(ops,'w');

    if (OPT_ISSET(ops,'w'))
	printflags |= PRINT_WHENCE_WORD;
    else if (OPT_ISSET(ops,'c'))
	printflags |= PRINT_WHENCE_CSH;
    else if (OPT_ISSET(ops,'v'))
	printflags |= PRINT_WHENCE_VERBOSE;
    else
	printflags |= PRINT_WHENCE_SIMPLE;
    if (OPT_ISSET(ops,'f'))
	printflags |= PRINT_WHENCE_FUNCDEF;

    /* With -m option -- treat arguments as a glob patterns */
    if (OPT_ISSET(ops,'m')) {
	for (; *argv; argv++) {
	    /* parse the pattern */
	    tokenize(*argv);
	    if (!(pprog = patcompile(*argv, PAT_STATIC, NULL))) {
		untokenize(*argv);
		zwarnnam(nam, "bad pattern : %s", *argv, 0);
		returnval = 1;
		continue;
	    }
	    queue_signals();
	    if (!OPT_ISSET(ops,'p')) {
		/* -p option is for path search only.    *
		 * We're not using it, so search for ... */

		/* aliases ... */
		scanmatchtable(aliastab, pprog, 0, DISABLED,
			       aliastab->printnode, printflags);

		/* and reserved words ... */
		scanmatchtable(reswdtab, pprog, 0, DISABLED,
			       reswdtab->printnode, printflags);

		/* and shell functions... */
		scanmatchtable(shfunctab, pprog, 0, DISABLED,
			       shfunctab->printnode, printflags);

		/* and builtins. */
		scanmatchtable(builtintab, pprog, 0, DISABLED,
			       builtintab->printnode, printflags);
	    }
	    /* Done search for `internal' commands, if the -p option *
	     * was not used.  Now search the path.                   */
	    cmdnamtab->filltable(cmdnamtab);
	    scanmatchtable(cmdnamtab, pprog, 0, 0,
			   cmdnamtab->printnode, printflags);

	    unqueue_signals();
	}
	return returnval;
    }

    /* Take arguments literally -- do not glob */
    queue_signals();
    for (; *argv; argv++) {
	informed = 0;

	if (!OPT_ISSET(ops,'p')) {
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
	    char **pp, *buf;

	    pushheap();
	    for (pp = path; *pp; pp++) {
		if (**pp) {
		    buf = zhtricat(*pp, "/", *argv);
		} else buf = ztrdup(*argv);

		if (iscom(buf)) {
		    if (wd) {
			printf("%s: command\n", *argv);
		    } else {
			if (v && !csh)
			    zputs(*argv, stdout), fputs(" is ", stdout);
			zputs(buf, stdout);
			if (OPT_ISSET(ops,'s'))
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
	    popheap();
	} else if ((cnam = findcmd(*argv, 1))) {
	    /* Found external command. */
	    if (wd) {
		printf("%s: command\n", *argv);
	    } else {
		if (v && !csh)
		    zputs(*argv, stdout), fputs(" is ", stdout);
		zputs(cnam, stdout);
		if (OPT_ISSET(ops,'s'))
		    print_if_link(cnam);
		fputc('\n', stdout);
	    }
	} else {
	    /* Not found at all. */
	    if (v || csh || wd)
		zputs(*argv, stdout), puts(wd ? ": none" : " not found");
	    returnval = 1;
	}
    }
    unqueue_signals();
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
bin_hash(char *name, char **argv, Options ops, int func)
{
    HashTable ht;
    Patprog pprog;
    Asgment asg;
    int returnval = 0;
    int printflags = 0;

    if (OPT_ISSET(ops,'d'))
	ht = nameddirtab;
    else
	ht = cmdnamtab;

    if (OPT_ISSET(ops,'r') || OPT_ISSET(ops,'f')) {
	/* -f and -r can't be used with any arguments */
	if (*argv) {
	    zwarnnam("hash", "too many arguments", NULL, 0);
	    return 1;
	}

	/* empty the hash table */
	if (OPT_ISSET(ops,'r'))
	    ht->emptytable(ht);

	/* fill the hash table in a standard way */
	if (OPT_ISSET(ops,'f'))
	    ht->filltable(ht);

	return 0;
    }

    if (OPT_ISSET(ops,'L')) printflags |= PRINT_LIST;

    /* Given no arguments, display current hash table. */
    if (!*argv) {
	queue_signals();
	scanhashtable(ht, 1, 0, 0, ht->printnode, printflags);
	unqueue_signals();
	return 0;
    }

    queue_signals();
    while (*argv) {
	void *hn;
	if (OPT_ISSET(ops,'m')) {
	    /* with the -m option, treat the argument as a glob pattern */
	    tokenize(*argv);  /* expand */
	    if ((pprog = patcompile(*argv, PAT_STATIC, NULL))) {
		/* display matching hash table elements */
		scanmatchtable(ht, pprog, 0, 0, ht->printnode, printflags);
	    } else {
		untokenize(*argv);
		zwarnnam(name, "bad pattern : %s", *argv, 0);
		returnval = 1;
	    }
	} else if ((asg = getasg(*argv)) && asg->value) {
	    if(isset(RESTRICTED)) {
		zwarnnam(name, "restricted: %s", asg->value, 0);
		returnval = 1;
	    } else {
		/* The argument is of the form foo=bar, *
		 * so define an entry for the table.    */
		if(OPT_ISSET(ops,'d')) {
		    Nameddir nd = hn = zcalloc(sizeof *nd);
		    nd->flags = 0;
		    nd->dir = ztrdup(asg->value);
		} else {
		    Cmdnam cn = hn = zcalloc(sizeof *cn);
		    cn->flags = HASHED;
		    cn->u.cmd = ztrdup(asg->value);
		}
		ht->addnode(ht, ztrdup(asg->name), hn);
		if(OPT_ISSET(ops,'v'))
		    ht->printnode(hn, 0);
	    }
	} else if (!(hn = ht->getnode2(ht, asg->name))) {
	    /* With no `=value' part to the argument, *
	     * work out what it ought to be.          */
	    if(OPT_ISSET(ops,'d')) {
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
	    if(OPT_ISSET(ops,'v') && (hn = ht->getnode2(ht, asg->name)))
		ht->printnode(hn, 0);
	} else if(OPT_ISSET(ops,'v'))
	    ht->printnode(hn, 0);
	argv++;
    }
    unqueue_signals();
    return returnval;
}

/* unhash: remove specified elements from a hash table */

/**/
int
bin_unhash(char *name, char **argv, Options ops, int func)
{
    HashTable ht;
    HashNode hn, nhn;
    Patprog pprog;
    int match = 0, returnval = 0;
    int i;

    /* Check which hash table we are working with. */
    if (OPT_ISSET(ops,'d'))
	ht = nameddirtab;	/* named directories */
    else if (OPT_ISSET(ops,'f'))
	ht = shfunctab;		/* shell functions   */
    else if (OPT_ISSET(ops,'a'))
	ht = aliastab;		/* aliases           */
    else
	ht = cmdnamtab;		/* external commands */

    /* With -m option, treat arguments as glob patterns. *
     * "unhash -m '*'" is legal, but not recommended.    */
    if (OPT_ISSET(ops,'m')) {
	for (; *argv; argv++) {
	    /* expand argument */
	    tokenize(*argv);
	    if ((pprog = patcompile(*argv, PAT_STATIC, NULL))) {
		/* remove all nodes matching glob pattern */
		queue_signals();
		for (i = 0; i < ht->hsize; i++) {
		    for (hn = ht->nodes[i]; hn; hn = nhn) {
			/* record pointer to next, since we may free this one */
			nhn = hn->next;
			if (pattry(pprog, hn->nam)) {
			    ht->freenode(ht->removenode(ht, hn->nam));
			    match++;
			}
		    }
		}
		unqueue_signals();
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
    queue_signals();
    for (; *argv; argv++) {
	if ((hn = ht->removenode(ht, *argv))) {
	    ht->freenode(hn);
	} else {
	    zwarnnam(name, "no such hash table element: %s", *argv, 0);
	    returnval = 1;
	}
    }
    unqueue_signals();
    return returnval;
}

/**** alias builtins ****/

/* alias: display or create aliases. */

/**/
int
bin_alias(char *name, char **argv, Options ops, int func)
{
    Alias a;
    Patprog pprog;
    Asgment asg;
    int haveflags = 0, returnval = 0;
    int flags1 = 0, flags2 = DISABLED;
    int printflags = 0;

    /* Did we specify the type of alias? */
    if (OPT_ISSET(ops,'r') || OPT_ISSET(ops,'g')) {
	if (OPT_ISSET(ops,'r') && OPT_ISSET(ops,'g')) {
	    zwarnnam(name, "illegal combination of options", NULL, 0);
	    return 1;
	}
	haveflags = 1;
	if (OPT_ISSET(ops,'g'))
	    flags1 |= ALIAS_GLOBAL;
	else
	    flags2 |= ALIAS_GLOBAL;
    }

    if (OPT_ISSET(ops,'L'))
	printflags |= PRINT_LIST;
    else if (OPT_PLUS(ops,'r') || OPT_PLUS(ops,'g')|| OPT_PLUS(ops,'m') ||
	     OPT_ISSET(ops,'+'))
	printflags |= PRINT_NAMEONLY;

    /* In the absence of arguments, list all aliases.  If a command *
     * line flag is specified, list only those of that type.        */
    if (!*argv) {
	queue_signals();
	scanhashtable(aliastab, 1, flags1, flags2, aliastab->printnode, printflags);
	unqueue_signals();
	return 0;
    }

    /* With the -m option, treat the arguments as *
     * glob patterns of aliases to display.       */
    if (OPT_ISSET(ops,'m')) {
	for (; *argv; argv++) {
	    tokenize(*argv);  /* expand argument */
	    if ((pprog = patcompile(*argv, PAT_STATIC, NULL))) {
		/* display the matching aliases */
		queue_signals();
		scanmatchtable(aliastab, pprog, flags1, flags2,
			       aliastab->printnode, printflags);
		unqueue_signals();
	    } else {
		untokenize(*argv);
		zwarnnam(name, "bad pattern : %s", *argv, 0);
		returnval = 1;
	    }
	}
	return returnval;
    }

    /* Take arguments literally.  Don't glob */
    queue_signals();
    while ((asg = getasg(*argv++))) {
	if (asg->value && !OPT_ISSET(ops,'L')) {
	    /* The argument is of the form foo=bar and we are not *
	     * forcing a listing with -L, so define an alias      */
	    aliastab->addnode(aliastab, ztrdup(asg->name),
			      createaliasnode(ztrdup(asg->value), flags1));
	} else if ((a = (Alias) aliastab->getnode(aliastab, asg->name))) {
	    /* display alias if appropriate */
	    if (!haveflags ||
		(OPT_ISSET(ops,'r') && !(a->flags & ALIAS_GLOBAL)) ||
		(OPT_ISSET(ops,'g') &&  (a->flags & ALIAS_GLOBAL)))
		aliastab->printnode((HashNode) a, printflags);
	} else
	    returnval = 1;
    }
    unqueue_signals();
    return returnval;
}


/**** miscellaneous builtins ****/

/* true, : (colon) */

/**/
int
bin_true(char *name, char **argv, Options ops, int func)
{
    return 0;
}

/* false builtin */

/**/
int
bin_false(char *name, char **argv, Options ops, int func)
{
    return 1;
}

/* the zle buffer stack */
 
/**/
mod_export LinkList bufstack;

/* echo, print, printf, pushln */

#define print_val(VAL) \
    if (prec >= 0) \
	count += fprintf(fout, spec, width, prec, VAL); \
    else \
	count += fprintf(fout, spec, width, VAL);

/**/
int
bin_print(char *name, char **args, Options ops, int func)
{
    int flen, width, prec, type, argc, n, narg;
    int nnl = 0, ret = 0, maxarg = 0;
    int flags[5], *len;
    char *start, *endptr, *c, *d, *flag, *buf, spec[11], *fmt = NULL;
    char **first, *curarg, *flagch = "0+- #", save = '\0', nullstr = '\0';
    size_t rcount, count = 0;
#ifdef HAVE_OPEN_MEMSTREAM
    size_t mcount;
#endif
    FILE *fout = stdout;
    Histent ent;
    
    mnumber mnumval;
    double doubleval;
    int intval;
    zlong zlongval;
    zulong zulongval;
    char *stringval;
    
    if (func == BIN_PRINTF) {
        if (!strcmp(*args, "--") && !*++args) {
            zwarnnam(name, "not enough arguments", NULL, 0);
	    return 1;
        }
  	fmt = *args++;
    } else if (func == BIN_ECHO && isset(BSDECHO))
	ops->ind['E'] = 1;
    else if (OPT_HASARG(ops,'f'))
	fmt = OPT_ARG(ops,'f');
    if (fmt)
	fmt = getkeystring(fmt, &flen, OPT_ISSET(ops,'b') ? 2 : 0, &nnl);

    first = args;
    
    /* -m option -- treat the first argument as a pattern and remove
     * arguments not matching */
    if (OPT_ISSET(ops,'m')) {
	Patprog pprog;
	char **t, **p;

	if (!*args) {
	    zwarnnam(name, "no pattern specified", NULL, 0);
	    return 1;
	}
	tokenize(*args);
	if (!(pprog = patcompile(*args, PAT_STATIC, NULL))) {
	    untokenize(*args);
	    zwarnnam(name, "bad pattern : %s", *args, 0);
	    return 1;
	}
	for (t = p = ++args; *p; p++)
	    if (pattry(pprog, *p))
		*t++ = *p;
	*t = NULL;
	first = args;
	if (fmt && !*args) return 0;
    }
    /* compute lengths, and interpret according to -P, -D, -e, etc. */
    argc = arrlen(args);
    len = (int *) hcalloc(argc * sizeof(int));
    for(n = 0; n < argc; n++) {
	/* first \ sequences */
	if (fmt || 
	    (!OPT_ISSET(ops,'e') && 
	     (OPT_ISSET(ops,'R') || OPT_ISSET(ops,'r') || OPT_ISSET(ops,'E'))))
	    unmetafy(args[n], &len[n]);
	else
	    args[n] = getkeystring(args[n], &len[n], OPT_ISSET(ops,'b') ? 2 :
				   (func != BIN_ECHO && !OPT_ISSET(ops,'e')),
				   &nnl);
	/* -P option -- interpret as a prompt sequence */
	if(OPT_ISSET(ops,'P')) {
	    /*
	     * promptexpand uses permanent storage: to avoid
	     * messy memory management, stick it on the heap
	     * instead.
	     */
	    char *str = unmetafy(promptexpand(metafy(args[n], len[n],
						     META_NOALLOC), 0, NULL, NULL), &len[n]);
	    args[n] = dupstring(str);
	    free(str);
	}
	/* -D option -- interpret as a directory, and use ~ */
	if(OPT_ISSET(ops,'D')) {
	    Nameddir d;
	    
	    queue_signals();
	    d = finddir(args[n]);
	    if(d) {
		char *arg = zhalloc(strlen(args[n]) + 1);
		sprintf(arg, "~%s%s", d->nam,
			args[n] + strlen(d->dir));
		args[n] = arg;
		len[n] = strlen(args[n]);
	    }
	    unqueue_signals();
	}
    }
    
    /* -u and -p -- output to other than standard output */
    if (OPT_HASARG(ops,'u') || OPT_ISSET(ops,'p')) {
	int fd;

	if (OPT_ISSET(ops, 'p'))
	    fd = coprocout;
	else {
	    char *argptr = OPT_ARG(ops,'u'), *eptr;
	    /* Handle undocumented feature that -up worked */
	    if (!strcmp(argptr, "p")) {
		fd = coprocout;
	    } else {
		fd = (int)zstrtol(argptr, &eptr, 10);
		if (*eptr) {
		    zwarnnam(name, "number expected after -%c: %s", argptr,
			     'u');
		    return 1;
		}
	    }
	}

	if ((fd = dup(fd)) < 0) {
	    zwarnnam(name, "bad file number: %d", NULL, fd);
	    return 1;
	}
	if ((fout = fdopen(fd, "w")) == 0) {
	    close(fd);
	    zwarnnam(name, "bad mode on fd %d", NULL, fd);
	    return 1;
	}
    }

    /* -o and -O -- sort the arguments */
    if (OPT_ISSET(ops,'o')) {
	if (fmt && !*args) return 0;
	if (OPT_ISSET(ops,'i'))
	    qsort(args, arrlen(args), sizeof(char *), cstrpcmp);
	else
	    qsort(args, arrlen(args), sizeof(char *), strpcmp);
    } else if (OPT_ISSET(ops,'O')) {
	if (fmt && !*args) return 0;
	if (OPT_ISSET(ops,'i'))
	    qsort(args, arrlen(args), sizeof(char *), invcstrpcmp);
	else
	    qsort(args, arrlen(args), sizeof(char *), invstrpcmp);
    }
    /* after sorting arguments, recalculate lengths */
    if(OPT_ISSET(ops,'o') || OPT_ISSET(ops,'O'))
	for(n = 0; n < argc; n++)
	    len[n] = strlen(args[n]);

    /* -c -- output in columns */
    if (OPT_ISSET(ops,'c') || OPT_ISSET(ops,'C')) {
	int l, nc, nr, sc, n, t, i;
	char **ap;

	if (OPT_ISSET(ops,'C')) {
	    char *eptr, *argptr = OPT_ARG(ops,'C');
	    nc = (int)zstrtol(argptr, &eptr, 10);
	    if (*eptr) {
		zwarnnam(name, "number expected after -%c: %s", argptr, 'C');
		return 1;
	    }
	    if (nc <= 0) {
		zwarnnam(name, "invalid number of columns: %s", argptr, 0);
		return 1;
	    }
	    /*
	     * n: number of elements
	     * nc: number of columns
	     * nr: number of rows
	     */
	    n = arrlen(args);
	    nr = (n + nc - 1) / nc;

	    /*
	     * i: loop counter
	     * ap: array iterator
	     * l: maximum length seen
	     *
	     * Ignore lengths in last column since they don't affect
	     * the separation.
	     */
	    for (i = l = 0, ap = args; *ap; ap++, i++) {
		if (OPT_ISSET(ops, 'a')) {
		    if ((i % nc) == nc - 1)
			continue;
		} else {
		    if (i >= nr * (nc - 1))
			break;
		}
		if (l < (t = strlen(*ap)))
		    l = t;
	    }
	    sc = l + 2;
	}
	else
	{
	    /*
	     * n: loop counter
	     * ap: array iterator
	     * l: maximum length seen
	     */
	    for (n = l = 0, ap = args; *ap; ap++, n++)
		if (l < (t = strlen(*ap)))
		    l = t;

	    /*
	     * sc: column width
	     * nc: number of columns (at least one)
	     */
	    sc = l + 2;
	    nc = (columns + 1) / sc;
	    if (!nc)
		nc = 1;
	    nr = (n + nc - 1) / nc;
	}

	if (OPT_ISSET(ops,'a'))	/* print across, i.e. columns first */
	    ap = args;
	for (i = 0; i < nr; i++) {
	    if (OPT_ISSET(ops,'a'))
	    {
		int ic;
		for (ic = 0; ic < nc && *ap; ic++, ap++)
		{
		    l = strlen(*ap);
		    fprintf(fout, "%s", *ap);
		    if (*ap)
			for (; l < sc; l++)
			    fputc(' ', fout);
		}
	    }
	    else
	    {
		ap = args + i;
		do {
		    l = strlen(*ap);
		    fprintf(fout, "%s", *ap);
		    for (t = nr; t && *ap; t--, ap++);
		    if(*ap)
			for (; l < sc; l++)
			    fputc(' ', fout);
		} while (*ap);
	    }
	    fputc(OPT_ISSET(ops,'N') ? '\0' : '\n', fout);
	}
	/* Testing EBADF special-cases >&- redirections */
	if ((fout != stdout) ? (fclose(fout) != 0) :
	    (fflush(fout) != 0 && errno != EBADF)) {
            zwarnnam(name, "write error: %e", NULL, errno);
            ret = 1;
	}
	return ret;
    }
    
    /* normal output */
    if (!fmt) {
	/* -z option -- push the arguments onto the editing buffer stack */
	if (OPT_ISSET(ops,'z')) {
	    queue_signals();
	    zpushnode(bufstack, sepjoin(args, NULL, 0));
	    unqueue_signals();
	    return 0;
	}
	/* -s option -- add the arguments to the history list */
	if (OPT_ISSET(ops,'s')) {
	    int nwords = 0, nlen, iwords;
	    char **pargs = args;

	    queue_signals();
	    ent = prepnexthistent();
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
	    ent->text = zjoin(args, ' ', 0);
	    ent->stim = ent->ftim = time(NULL);
	    ent->flags = 0;
	    addhistnode(histtab, ent->text, ent);
	    unqueue_signals();
	    return 0;
	}

	for (; *args; args++, len++) {
	    fwrite(*args, *len, 1, fout);
	    if (args[1])
		fputc(OPT_ISSET(ops,'l') ? '\n' : 
		      OPT_ISSET(ops,'N') ? '\0' : ' ', fout);
	}
	if (!(OPT_ISSET(ops,'n') || nnl))
	    fputc(OPT_ISSET(ops,'N') ? '\0' : '\n', fout);
	/* Testing EBADF special-cases >&- redirections */
	if ((fout != stdout) ? (fclose(fout) != 0) :
	    (fflush(fout) != 0 && errno != EBADF)) {
            zwarnnam(name, "write error: %e", NULL, errno);
            ret = 1;
	}
	return ret;
    }
    
    if (OPT_ISSET(ops,'z') || OPT_ISSET(ops,'s')) {
#ifdef HAVE_OPEN_MEMSTREAM
    	if ((fout = open_memstream(&buf, &mcount)) == NULL)
	    zwarnnam(name, "open_memstream failed", NULL, 0);
#else
	char *tmpf = gettempname();
    	if ((fout = fopen(tmpf, "w+")) == NULL)
	    zwarnnam(name, "can't open temp file: %e", NULL, errno);
	unlink(tmpf);
#endif
    } 
    
    /* printf style output */
    *spec='%';
    do {
    	rcount = count;
    	if (maxarg) {
	    first += maxarg;
	    argc -= maxarg;
    	    maxarg = 0;
	}
	for (c = fmt;c-fmt < flen;c++) {
	    if (*c != '%') {
		putc(*c, fout);
		++count;
		continue;
	    }

	    start = c++;
	    if (*c == '%') {
		putchar('%');
		++count;
		continue;
	    }

	    type = prec = -1;
	    width = 0;
	    curarg = NULL;
	    d = spec + 1;

	    if (*c >= '1' && *c <= '9') {
	    	narg = strtoul(c, &endptr, 0);
		if (*endptr == '$') {
		    c = endptr + 1;
		    DPUTS(narg <= 0, "specified zero or negative arg");
		    if (narg > argc) {
		    	zwarnnam(name, "%d: argument specifier out of range",
				 0, narg);
			return 1;
		    } else {
		    	if (narg > maxarg) maxarg = narg;
		    	curarg = *(first + narg - 1);
		    }
		}
	    }
		    
	    
	    /* copy only one of each flag as spec has finite size */
	    memset(flags, 0, sizeof(flags));
	    while ((flag = strchr(flagch, *c))) {
	    	if (!flags[flag - flagch]) {
	    	    flags[flag - flagch] = 1;
		    *d++ = *c;
		}
	    	c++;
	    }

	    if (idigit(*c)) {
		width = strtoul(c, &endptr, 0);
		c = endptr;
	    } else if (*c == '*') {
		if (idigit(*++c)) {
		    narg = strtoul(c, &endptr, 0);
		    if (*endptr == '$') {
		    	c = endptr + 1;
			if (narg > argc || narg <= 0) {
		    	    zwarnnam(name,
				     "%d: argument specifier out of range",
				     0, narg);
			    return 1;
			} else {
		    	    if (narg > maxarg) maxarg = narg;
		    	    args = first + narg - 1;
			}
		    }
		}
		if (*args) {
		    width = (int)mathevali(*args++);
		    if (errflag) {
			errflag = 0;
			ret = 1;
		    }
		}
	    }
	    *d++ = '*';

	    if (*c == '.') {
		if (*++c == '*') {
		    if (idigit(*++c)) {
			narg = strtoul(c, &endptr, 0);
			if (*endptr == '$') {
			    c = endptr + 1;
			    if (narg > argc || narg <= 0) {
		    		zwarnnam(name,
					 "%d: argument specifier out of range",
					 0, narg);
				return 1;
			    } else {
		    		if (narg > maxarg) maxarg = narg;
		    		args = first + narg - 1;
			    }
			}
		    }
		    
		    if (*args) {
			prec = (int)mathevali(*args++);
			if (errflag) {
			    errflag = 0;
			    ret = 1;
			}
		    }
		} else if (idigit(*c)) {
		    prec = strtoul(c, &endptr, 0);
		    c = endptr;
		}
		if (prec >= 0) *d++ = '.', *d++ = '*';
	    }

	    /* ignore any size modifier */
	    if (*c == 'l' || *c == 'L' || *c == 'h') c++;

	    if (!curarg && *args) curarg = *args++;
	    d[1] = '\0';
	    switch (*d = *c) {
	    case 'c':
		if (curarg) {
		    intval = *curarg;
		} else
		    intval = 0;
		print_val(intval);
		break;
	    case 's':
		stringval = curarg ? curarg : &nullstr;
		print_val(stringval);
		break;
	    case 'b':
		if (curarg) {
		    int l;
		    char *b = getkeystring(curarg, &l, 
					   OPT_ISSET(ops,'b') ? 2 : 0, &nnl);
		    /* handle width/precision here and use fwrite so that
		     * nul characters can be output */
		    if (prec >= 0 && prec < l) l = prec;
		    if (width > 0 && flags[2]) width = -width;
		    if (width > 0 && l < width)
		    	printf("%*c", width - l, ' ');
		    fwrite(b, l, 1, fout);
		    if (width < 0 && l < -width)
		    	printf("%*c", -width - l, ' ');
		    count += l;
		}
		break;
	    case 'q':
		stringval = curarg ? bslashquote(curarg, NULL, 0) : &nullstr;
		*d = 's';
		print_val(stringval);
		break;
	    case 'd':
	    case 'i':
		type=1;
		break;
	    case 'e':
	    case 'E':
	    case 'f':
	    case 'g':
	    case 'G':
		type=2;
		break;
	    case 'o':
	    case 'u':
	    case 'x':
	    case 'X':
		type=3;
		break;
	    case 'n':
		if (curarg) setiparam(curarg, count - rcount);
		break;
	    default:
	        if (*c) {
		    save = c[1];
	            c[1] = '\0';
		}
		zwarnnam(name, "%s: invalid directive", start, 0);
		if (*c) c[1] = save;
		/* Testing EBADF special-cases >&- redirections */
		if ((fout != stdout) ? (fclose(fout) != 0) :
		    (fflush(fout) != 0 && errno != EBADF)) {
		    zwarnnam(name, "write error: %e", NULL, errno);
		}
		return 1;
	    }

	    if (type > 0) {
		if (curarg && (*curarg == '\'' || *curarg == '"' )) {
		    if (type == 2) {
			doubleval = (unsigned char)curarg[1];
			print_val(doubleval);
		    } else {
			intval = (unsigned char)curarg[1];
			print_val(intval);
		    }
		} else {
		    switch (type) {
		    case 1:
#ifdef ZSH_64_BIT_TYPE
 		    	*d++ = 'l';
#endif
		    	*d++ = 'l', *d++ = *c, *d = '\0';
			zlongval = (curarg) ? mathevali(curarg) : 0;
			if (errflag) {
			    zlongval = 0;
			    errflag = 0;
			    ret = 1;
			}
			print_val(zlongval)
			    break;
		    case 2:
			if (curarg) {
			    mnumval = matheval(curarg);
			    doubleval = (mnumval.type & MN_FLOAT) ?
			    	mnumval.u.d : (double)mnumval.u.l;
			} else doubleval = 0;
			if (errflag) {
			    doubleval = 0;
			    errflag = 0;
			    ret = 1;
			}
			print_val(doubleval)
			    break;
		    case 3:
#ifdef ZSH_64_BIT_UTYPE
 		    	*d++ = 'l';
#endif
		    	*d++ = 'l', *d++ = *c, *d = '\0';
			zulongval = (curarg) ? mathevali(curarg) : 0;
			if (errflag) {
			    zulongval = 0;
			    errflag = 0;
			    ret = 1;
			}
			print_val(zulongval)
			    }
		}
	    }
	    if (maxarg && (args - first > maxarg))
	    	maxarg = args - first;
	}

    	if (maxarg) args = first + maxarg;
	/* if there are remaining args, reuse format string */
    } while (*args && args != first && !OPT_ISSET(ops,'r'));

    if (OPT_ISSET(ops,'z') || OPT_ISSET(ops,'s')) {
#ifdef HAVE_OPEN_MEMSTREAM
	putc(0, fout);
	fflush(fout);
	count = mcount;
#else
	rewind(fout);
	buf = (char *)zalloc(count + 1);
	fread(buf, count, 1, fout);
	buf[count] = '\0';
#endif
	queue_signals();
	if (OPT_ISSET(ops,'z')) {
	    zpushnode(bufstack, buf);
	} else {
	    ent = prepnexthistent();
	    ent->text = buf;
	    ent->stim = ent->ftim = time(NULL);
	    ent->flags = 0;
	    ent->words = (short *)NULL;
	    addhistnode(histtab, ent->text, ent);
	}
	unqueue_signals();
    }

    /* Testing EBADF special-cases >&- redirections */
    if ((fout != stdout) ? (fclose(fout) != 0) :
	(fflush(fout) != 0 && errno != EBADF)) {
	zwarnnam(name, "write error: %e", NULL, errno);
	ret = 1;
    }
    return ret;
}

/* shift builtin */

/**/
int
bin_shift(char *name, char **argv, Options ops, int func)
{
    int num = 1, l, ret = 0;
    char **s;
 
    /* optional argument can be either numeric or an array */
    queue_signals();
    if (*argv && !getaparam(*argv))
        num = mathevali(*argv++);
 
    if (num < 0) {
	unqueue_signals();
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
		s = zarrdup(s + num);
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
    unqueue_signals();
    return ret;
}

/**/
int optcind;

/* getopts: automagical option handling for shell scripts */

/**/
int
bin_getopts(char *name, char **argv, Options ops, int func)
{
    int lenstr, lenoptstr, quiet, lenoptbuf;
    char *optstr = unmetafy(*argv++, &lenoptstr), *var = *argv++;
    char **args = (*argv) ? argv : pparams;
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
    if (!lenstr)		/* Definitely not an option. */
	return 1;
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
	setsparam(var, ztrdup(p));
	if(quiet) {
	    zoptarg = metafy(optbuf, lenoptbuf, META_DUP);
	} else {
	    zwarn(*p == '?' ? "bad option: -%c" :
		  "argument expected after -%c option", NULL, opch);
	    zoptarg=ztrdup("");
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
	/*
	 * Careful:  I've just changed the following two lines from
	 *   optcind = ztrlen(args[zoptind - 1]);
	 * and it's a rigorous theorem that every change in getopts breaks
	 * something.  See zsh-workers/9095 for the bug fixed here.
	 *   PWS 2000/05/02
	 */
	optcind = 0;
	zoptind++;
	zsfree(zoptarg);
	zoptarg = p;
    } else {
	zsfree(zoptarg);
	zoptarg = ztrdup("");
    }

    setsparam(var, metafy(optbuf, lenoptbuf, META_DUP));
    return 0;
}

/* Flag that we should exit the shell as soon as all functions return. */
/**/
int
exit_pending;

/* break, bye, continue, exit, logout, return -- most of these take   *
 * one numeric argument, and the other (logout) is related to return. *
 * (return is treated as a logout when in a login shell.)             */

/**/
int
bin_break(char *name, char **argv, Options ops, int func)
{
    int num = lastval, nump = 0;

    /* handle one optional numeric argument */
    if (*argv) {
	num = mathevali(*argv++);
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
	/*FALLTHROUGH*/
    case BIN_EXIT:
	if (locallevel) {
	    /*
	     * We don't exit directly from functions to allow tidying
	     * up, in particular EXIT traps.  We still need to perform
	     * the usual interactive tests to see if we can exit at
	     * all, however.
	     */
	    if (stopmsg || (zexit(0,2), !stopmsg)) {
		retflag = 1;
		breaks = loops;
		exit_pending = (num << 1) | 1;
	    }
	} else
	    zexit(num, 0);
	break;
    }
    return 0;
}

/* we have printed a 'you have stopped (running) jobs.' message */
 
/**/
mod_export int stopmsg;
 
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
 * from_where is
 *   1   if zexit is called because of a signal
 *   2   if we can't actually exit yet (e.g. functions need
 *       terminating) but should perform the usual interactive tests.
 */

/**/
mod_export void
zexit(int val, int from_where)
{
    static int in_exit;

    if (isset(MONITOR) && !stopmsg && from_where != 1) {
	scanjobs();    /* check if jobs need printing           */
	if (isset(CHECKJOBS))
	    checkjobs();   /* check if any jobs are running/stopped */
	if (stopmsg) {
	    stopmsg = 2;
	    return;
	}
    }
    if (from_where == 2 || (in_exit++ && from_where))
	return;

    if (isset(MONITOR)) {
	/* send SIGHUP to any jobs left running  */
	killrunjobs(from_where == 1);
    }
    if (isset(RCS) && interact) {
	if (!nohistsave)
	    savehistfile(NULL, 1, HFILE_USE_OPTIONS);
	if (islogin && !subsh) {
	    sourcehome(".zlogout");
#ifdef GLOBAL_ZLOGOUT
	    if (isset(RCS) && isset(GLOBALRCS))
		source(GLOBAL_ZLOGOUT);
#endif
	}
    }
    if (sigtrapped[SIGEXIT])
	dotrap(SIGEXIT);
    runhookdef(EXITHOOK, NULL);
    if (mypid != getpid())
	_exit(val);
    else
	exit(val);
}

/* . (dot), source */

/**/
int
bin_dot(char *name, char **argv, Options ops, int func)
{
    char **old, *old0 = NULL;
    int ret, diddot = 0, dotdot = 0;
    char *s, **t, *enam, *arg0, *buf;
    struct stat st;

    if (!*argv)
	return 0;
    old = pparams;
    /* get arguments for the script */
    if (argv[1])
	pparams = zarrdup(argv + 1);

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
	    pushheap();
	    /* search path for script */
	    for (t = path; *t; t++) {
		if (!(*t)[0] || ((*t)[0] == '.' && !(*t)[1])) {
		    if (diddot)
			continue;
		    diddot = 1;
		    buf = dupstring(arg0);
		} else
		    buf = zhtricat(*t, "/", arg0);

		s = unmeta(buf);
		if (access(s, F_OK) == 0 && stat(s, &st) >= 0
		    && !S_ISDIR(st.st_mode)) {
		    ret = source(enam = buf);
		    break;
		}
	    }
	    popheap();
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
bin_emulate(char *nam, char **argv, Options ops, int func)
{
    emulate(*argv, OPT_ISSET(ops,'R'));
    if (OPT_ISSET(ops,'L'))
	opts[LOCALOPTIONS] = opts[LOCALTRAPS] = 1;
    return 0;
}

/* eval: simple evaluation */

/**/
int
bin_eval(char *nam, char **argv, Options ops, int func)
{
    Eprog prog;
    char *oscriptname = scriptname;

    scriptname = "(eval)";

    prog = parse_string(zjoin(argv, ' ', 1));
    if (!prog) {
	errflag = 0;
	return 1;
    }
    execode(prog, 1, 0);
    if (errflag) {
	lastval = errflag;
	errflag = 0;
    }

    scriptname = oscriptname;

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
bin_read(char *name, char **args, Options ops, int func)
{
    char *reply, *readpmpt;
    int bsiz, c = 0, gotnl = 0, al = 0, first, nchars = 1, bslash, keys = 0;
    int haso = 0;	/* true if /dev/tty has been opened specially */
    int isem = !strcmp(term, "emacs"), izle = zleactive && getkeyptr;
    char *buf, *bptr, *firstarg, *zbuforig;
    LinkList readll = newlinklist();
    FILE *oshout = NULL;
    int readchar = -1, val, resettty = 0;
    struct ttyinfo saveti;
    char d;
    char delim = '\n';

    if (OPT_HASARG(ops,c='k')) {
	char *eptr, *optarg = OPT_ARG(ops,c);
	nchars = (int)zstrtol(optarg, &eptr, 10);
	if (*eptr) {
	    zwarnnam(name, "number expected after -%c: %s", optarg, c);
	    return 1;
	}
    }
    /* This `*args++ : *args' looks a bit weird, but it works around a bug
     * in gcc-2.8.1 under DU 4.0. */
    firstarg = (*args && **args == '?' ? *args++ : *args);
    reply = *args ? *args++ : OPT_ISSET(ops,'A') ? "reply" : "REPLY";

    if (OPT_ISSET(ops,'A') && *args) {
	zwarnnam(name, "only one array argument allowed", NULL, 0);
	return 1;
    }

    /* handle compctl case */
    if(OPT_ISSET(ops,'l') || OPT_ISSET(ops,'c'))
	return compctlread(name, args, ops, reply);

    if ((OPT_ISSET(ops,'k') && !OPT_ISSET(ops,'u') && 
	 !OPT_ISSET(ops,'p')) || OPT_ISSET(ops,'q')) {
	if (!zleactive) {
	    if (SHTTY == -1) {
		/* need to open /dev/tty specially */
		if ((SHTTY = open("/dev/tty", O_RDWR|O_NOCTTY)) != -1) {
		    haso = 1;
		    oshout = shout;
		    init_shout();
		}
	    } else if (!shout) {
		/* We need an output FILE* on the tty */
		init_shout();
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
	    if (!isem && OPT_ISSET(ops,'k'))
		setcbreak();
	    readfd = SHTTY;
	}
	keys = 1;
    } else if (OPT_HASARG(ops,'u') && !OPT_ISSET(ops,'p')) {
	/* -u means take input from the specified file descriptor. */
	char *eptr, *argptr = OPT_ARG(ops,'u');
	/* The old code handled -up, but that was never documented. Still...*/
	if (!strcmp(argptr, "p")) {
	    readfd = coprocin;
	} else {
	    readfd = (int)zstrtol(argptr, &eptr, 10);
	    if (*eptr) {
		zwarnnam(name, "number expected after -%c: %s", argptr, 'u');
		return 1;
	    }
	}
#if 0
	/* This code is left as a warning to future generations --- pws. */
	for (readfd = 9; readfd && !OPT_ISSET(ops,readfd + '0'); --readfd);
#endif
	izle = 0;
    } else if (OPT_ISSET(ops,'p')) {
	readfd = coprocin;
	izle = 0;
    } else
	readfd = izle = 0;

    if (OPT_ISSET(ops,'t')) {
	zlong timeout = 0;
	if (OPT_HASARG(ops,'t')) {
	    mnumber mn = zero_mnumber;
	    mn = matheval(OPT_ARG(ops,'t'));
	    if (errflag)
		return 1;
	    if (mn.type == MN_FLOAT) {
		mn.u.d *= 1e6;
		timeout = (zlong)mn.u.d;
	    } else {
		timeout = (zlong)mn.u.l * (zlong)1000000;
	    }
	}
	if (readfd == -1 ||
	    !read_poll(readfd, &readchar, keys && !zleactive, timeout)) {
	    if (OPT_ISSET(ops,'k') && !zleactive && !isem)
		settyinfo(&shttyinfo);
	    if (haso) {
		fclose(shout);
		shout = oshout;
		SHTTY = -1;
	    }
	    return 1;
	}
    }
    if (OPT_ISSET(ops,'d')) {
        delim = *OPT_ARG(ops,'d');
	if (SHTTY != -1) {
	    struct ttyinfo ti;
	    gettyinfo(&ti);
	    saveti = ti;
	    resettty = 1;
#ifdef HAS_TIO
	    ti.tio.c_lflag &= ~ICANON;
	    ti.tio.c_cc[VMIN] = 1;
	    ti.tio.c_cc[VTIME] = 0;
#else
	    ti.sgttyb.sg_flags |= CBREAK;
#endif
	    settyinfo(&ti);
	}
    }
    if (OPT_ISSET(ops,'s') && SHTTY != -1) {
	struct ttyinfo ti;
	gettyinfo(&ti);
	if (! resettty) {
	    saveti = ti;
	    resettty = 1;
	}
#ifdef HAS_TIO
	ti.tio.c_lflag &= ~ECHO;
#else
	ti.sgttyb.sg_flags &= ~ECHO;
#endif
	settyinfo(&ti);
    }

    /* handle prompt */
    if (firstarg) {
	for (readpmpt = firstarg;
	     *readpmpt && *readpmpt != '?'; readpmpt++);
	if (*readpmpt++) {
	    if (keys || isatty(0)) {
		zputs(readpmpt, (shout ? shout : stderr));
		fflush(shout ? shout : stderr);
	    }
	    readpmpt[-1] = '\0';
	}
    }

    /* option -k means read only a given number of characters (default 1) */
    if (OPT_ISSET(ops,'k')) {
	/* allocate buffer space for result */
	bptr = buf = (char *)zalloc(nchars+1);

	do {
	    if (izle) {
		if ((val = getkeyptr(0)) < 0)
		    break;
		*bptr++ = (char) val;
		nchars--;
	    } else {
		/* If read returns 0, is end of file */
		if (readchar >= 0) {
		    *bptr = readchar;
		    val = 1;
		    readchar = -1;
		} else if ((val = read(readfd, bptr, nchars)) <= 0)
		    break;
	    
		/* decrement number of characters read from number required */
		nchars -= val;

		/* increment pointer past read characters */
		bptr += val;
	    }
	} while (nchars > 0);
	
	if (!izle && !OPT_ISSET(ops,'u') && !OPT_ISSET(ops,'p')) {
	    /* dispose of result appropriately, etc. */
	    if (isem)
		while (val > 0 && read(SHTTY, &d, 1) == 1 && d != '\n');
	    else {
		settyinfo(&shttyinfo);
		resettty = 0;
	    }
	    if (haso) {
		fclose(shout);	/* close(SHTTY) */
		shout = oshout;
		SHTTY = -1;
	    }
	}

	if (OPT_ISSET(ops,'e') || OPT_ISSET(ops,'E'))
	    fwrite(buf, bptr - buf, 1, stdout);
	if (!OPT_ISSET(ops,'e'))
	    setsparam(reply, metafy(buf, bptr - buf, META_REALLOC));
	else
	    zfree(buf, bptr - buf + 1);
	if (resettty && SHTTY != -1)
	    settyinfo(&saveti);
	return val <= 0;
    }

    /* option -q means get one character, and interpret it as a Y or N */
    if (OPT_ISSET(ops,'q')) {
	char readbuf[2];

	/* set up the buffer */
	readbuf[1] = '\0';

	/* get, and store, reply */
	if (izle) {
	    int key = getkeyptr(0);

	    readbuf[0] = (key == 'y' ? 'y' : 'n');
	} else {
	    readbuf[0] = ((char)getquery(NULL, 0)) == 'y' ? 'y' : 'n';

	    /* dispose of result appropriately, etc. */
	    if (haso) {
		fclose(shout);	/* close(SHTTY) */
		shout = oshout;
		SHTTY = -1;
	    }
	}
	if (OPT_ISSET(ops,'e') || OPT_ISSET(ops,'E'))
	    printf("%s\n", readbuf);
	if (!OPT_ISSET(ops,'e'))
	    setsparam(reply, ztrdup(readbuf));

	if (resettty && SHTTY != -1)
	    settyinfo(&saveti);
	return readbuf[0] == 'n';
    }

    /* All possible special types of input have been exhausted.  Take one line,
       and assign words to the parameters until they run out.  Leftover words go
       onto the last parameter.  If an array is specified, all the words become
       separate elements of the array. */

    zbuforig = zbuf = (!OPT_ISSET(ops,'z')) ? NULL :
	(nonempty(bufstack)) ? (char *) getlinknode(bufstack) : ztrdup("");
    first = 1;
    bslash = 0;
    while (*args || (OPT_ISSET(ops,'A') && !gotnl)) {
	sigset_t s = child_unblock();
	buf = bptr = (char *)zalloc(bsiz = 64);
	/* get input, a character at a time */
	while (!gotnl) {
	    c = zread(izle, &readchar);
	    /* \ at the end of a line indicates a continuation *
	     * line, except in raw mode (-r option)            */
	    if (bslash && c == delim) {
		bslash = 0;
		continue;
	    }
	    if (c == EOF || c == delim)
		break;
	    /*
	     * `first' is non-zero if any separator we encounter is a
	     * non-whitespace separator, which means that anything
	     * (even an empty string) between, before or after separators
	     * is significant.  If it is zero, we have a whitespace
	     * separator, which shouldn't cause extra empty strings to
	     * be emitted.  Hence the test for (*buf || first) when
	     * we assign the result of reading a word.
	     */
	    if (!bslash && isep(c)) {
		if (bptr != buf || (!iwsep(c) && first)) {
		    first |= !iwsep(c);
		    break;
		}
		first |= !iwsep(c);
		continue;
	    }
	    bslash = c == '\\' && !bslash && !OPT_ISSET(ops,'r');
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
	signal_setmask(s);
	if (c == delim || c == EOF)
	    gotnl = 1;
	*bptr = '\0';
	/* dispose of word appropriately */
	if (OPT_ISSET(ops,'e') || OPT_ISSET(ops,'E')) {
	    zputs(buf, stdout);
	    putchar('\n');
	}
	if (!OPT_ISSET(ops,'e') && (*buf || first)) {
	    if (OPT_ISSET(ops,'A')) {
		addlinknode(readll, buf);
		al++;
	    } else
		setsparam(reply, buf);
	} else
	    free(buf);
	if (!OPT_ISSET(ops,'A'))
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
    if (OPT_ISSET(ops,'A')) {
	char **pp, **p = NULL;
	LinkNode n;

	p = (OPT_ISSET(ops,'e') ? (char **)NULL
	     : (char **)zalloc((al + 1) * sizeof(char *)));

	for (pp = p, n = firstnode(readll); n; incnode(n)) {
	    if (OPT_ISSET(ops,'e') || OPT_ISSET(ops,'E')) {
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
	if (resettty && SHTTY != -1)
	    settyinfo(&saveti);
	return c == EOF;
    }
    buf = bptr = (char *)zalloc(bsiz = 64);
    /* any remaining part of the line goes into one parameter */
    bslash = 0;
    if (!gotnl) {
	sigset_t s = child_unblock();
	for (;;) {
	    c = zread(izle, &readchar);
	    /* \ at the end of a line introduces a continuation line, except in
	       raw mode (-r option) */
	    if (bslash && c == delim) {
		bslash = 0;
		continue;
	    }
	    if (c == EOF || (c == delim && !zbuf))
		break;
	    if (!bslash && isep(c) && bptr == buf) {
		if (iwsep(c))
		    continue;
		else if (!first) {
		    first = 1;
		    continue;
		}
	    }
	    bslash = c == '\\' && !bslash && !OPT_ISSET(ops,'r');
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
	signal_setmask(s);
    }
    while (bptr > buf && iwsep(bptr[-1]))
	bptr--;
    *bptr = '\0';
    if (resettty && SHTTY != -1)
	settyinfo(&saveti);
    /* final assignment of reply, etc. */
    if (OPT_ISSET(ops,'e') || OPT_ISSET(ops,'E')) {
	zputs(buf, stdout);
	putchar('\n');
    }
    if (!OPT_ISSET(ops,'e'))
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
zread(int izle, int *readchar)
{
    char cc, retry = 0;
    int ret;

    if (izle) {
	int c = getkeyptr(0);

	return (c < 0 ? EOF : c);
    }
    /* use zbuf if possible */
    if (zbuf) {
	/* If zbuf points to anything, it points to the next character in the
	   buffer.  This may be a null byte to indicate EOF.  If reading from the
	   buffer, move on the buffer pointer. */
	if (*zbuf == Meta)
	    return zbuf++, STOUC(*zbuf++ ^ 32);
	else
	    return (*zbuf) ? STOUC(*zbuf++) : EOF;
    }
    if (*readchar >= 0) {
	cc = *readchar;
	*readchar = -1;
	return STOUC(cc);
    }
    for (;;) {
	/* read a character from readfd */
	ret = read(readfd, &cc, 1);
	switch (ret) {
	case 1:
	    /* return the character read */
	    return STOUC(cc);
	case -1:
#if defined(EAGAIN) || defined(EWOULDBLOCK)
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
	    } else
#endif /* EAGAIN || EWOULDBLOCK */
		if (errno == EINTR && !(errflag || retflag || breaks || contflag))
		    continue;
	    break;
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
bin_test(char *name, char **argv, Options ops, int func)
{
    char **s;
    Eprog prog;
    struct estate state;

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
    prog = parse_cond();
    condlex = yylex;

    if (errflag) {
	errflag = 0;
	return 1;
    }

    if (!prog || tok == LEXERR) {
	zwarnnam(name, tokstr ? "parse error" : "argument expected", NULL, 0);
	return 1;
    }

    /* syntax is OK, so evaluate */

    state.prog = prog;
    state.pc = prog->prog;
    state.strs = prog->strs;


    return !evalcond(&state);
}

/* display a time, provided in units of 1/60s, as minutes and seconds */
#define pttime(X) printf("%ldm%ld.%02lds",((long) (X))/3600,\
			 ((long) (X))/60%60,((long) (X))*100/60%100)

/* times: display, in a two-line format, the times provided by times(3) */

/**/
int
bin_times(char *name, char **argv, Options ops, int func)
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
bin_trap(char *name, char **argv, Options ops, int func)
{
    Eprog prog;
    char *arg, *s;
    int sig;

    if (*argv && !strcmp(*argv, "--"))
	argv++;

    /* If given no arguments, list all currently-set traps */
    if (!*argv) {
	queue_signals();
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
		    s = getpermtext(sigfuncs[sig], NULL);
		    printf("trap -- ");
		    quotedzputs(s, stdout);
		    printf(" %s\n", sigs[sig]);
		    zsfree(s);
		}
	    }
	}
	unqueue_signals();
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
	prog = &dummy_eprog;
    else if (!(prog = parse_string(arg))) {
	zwarnnam(name, "couldn't parse trap command", NULL, 0);
	return 1;
    }

    /* set traps */
    for (; *argv; argv++) {
	Eprog t;

	sig = getsignum(*argv);
	if (sig == -1) {
	    zwarnnam(name, "undefined signal: %s", *argv, 0);
	    break;
	}
	t = dupeprog(prog, 0);
	if (settrap(sig, t))
	    freeeprog(t);
    }
    return *argv != NULL;
}

/**/
int
bin_ttyctl(char *name, char **argv, Options ops, int func)
{
    if (OPT_ISSET(ops,'f'))
	ttyfrozen = 1;
    else if (OPT_ISSET(ops,'u'))
	ttyfrozen = 0;
    else
	printf("tty is %sfrozen\n", ttyfrozen ? "" : "not ");
    return 0;
}

/* let -- mathematical evaluation */

/**/
int
bin_let(char *name, char **argv, Options ops, int func)
{
    mnumber val = zero_mnumber;

    while (*argv)
	val = matheval(*argv++);
    /* Errors in math evaluation in let are non-fatal. */
    errflag = 0;
    /* should test for fabs(val.u.d) < epsilon? */
    return (val.type == MN_INTEGER) ? val.u.l == 0 : val.u.d == 0.0;
}

/* umask command.  umask may be specified as octal digits, or in the  *
 * symbolic form that chmod(1) uses.  Well, a subset of it.  Remember *
 * that only the bottom nine bits of umask are used, so there's no    *
 * point allowing the set{u,g}id and sticky bits to be specified.     */

/**/
int
bin_umask(char *nam, char **args, Options ops, int func)
{
    mode_t um;
    char *s = *args;

    /* Get the current umask. */
    um = umask(0);
    umask(um);
    /* No arguments means to display the current setting. */
    if (!s) {
	if (OPT_ISSET(ops,'S')) {
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
mod_export int
bin_notavail(char *nam, char **argv, Options ops, int func)
{
    zwarnnam(nam, "not available on this system", NULL, 0);
    return 1;
}
