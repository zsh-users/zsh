/*
 * cond.c - evaluate conditional expressions
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
#include "cond.pro"

int tracingcond;

static char *condstr[COND_MOD] = {
    "!", "&&", "||", "==", "!=", "<", ">", "-nt", "-ot", "-ef", "-eq",
    "-ne", "-lt", "-gt", "-le", "-ge"
};

/**/
int
evalcond(Cond c)
{
    struct stat *st;

    switch (c->type) {
    case COND_NOT:
	if (tracingcond)
	    fprintf(stderr, " %s", condstr[c->type]);
	return !evalcond(c->left);
    case COND_AND:
	if (evalcond(c->left)) {
	    if (tracingcond)
		fprintf(stderr, " %s", condstr[c->type]);
	    return evalcond(c->right);
	} else
	    return 0;
    case COND_OR:
	if (!evalcond(c->left)) {
	    if (tracingcond)
		fprintf(stderr, " %s", condstr[c->type]);
	    return evalcond(c->right);
	} else
	    return 1;
    case COND_MOD:
    case COND_MODI:
	{
	    Conddef cd;

	    if ((cd = getconddef((c->type == COND_MODI),
				 ((char *) c->left) + 1, 1))) {
		if (c->type == COND_MOD) {
		    int l = arrlen((char **) c->right);

		    if (l < cd->min || (cd->max >= 0 && l > cd->max)) {
			zerr("unrecognized condition: `%s'", (char *) c->left, 0);
			return 0;
		    }
		}
		if (tracingcond)
		    tracemodcond((char *)c->left, (char **)c->right,
				 c->type == COND_MODI);
		return cd->handler((char **) c->right, cd->condid);
	    }
	    else {
		char **a = (char **) c->right, *s = a[0];

		if (s && s[0] == '-' &&
		    (cd = getconddef(0, s + 1, 1))) {
		    int l = arrlen(a);

		    if (l < cd->min || (cd->max >= 0 && l > cd->max)) {
			zerr("unrecognized condition: `%s'", (char *) c->left, 0);
			return 0;
		    }
		    if (tracingcond)
			tracemodcond((char *)c->left, a, c->type == COND_MODI);
		    a[0] = (char *) c->left;
		    return cd->handler(a, cd->condid);
		} else
		    zerr("unrecognized condition: `%s'", (char *) c->left, 0);
	    }
	    return 0;
	}
    }
    singsub((char **)&c->left);
    untokenize(c->left);
    if (c->right) {
	singsub((char **)&c->right);
	if (c->type != COND_STREQ && c->type != COND_STRNEQ)
	    untokenize(c->right);
    }

    if (tracingcond) {
	if (c->type < COND_MOD) {
	    char *rt = (char *)c->right;
	    if (c->type == COND_STREQ || c->type == COND_STRNEQ) {
		rt = dupstring(rt);
		untokenize(rt);
	    }
	    fprintf(stderr, " %s %s %s", (char *)c->left, condstr[c->type],
		    rt);
	} else
	    fprintf(stderr, " -%c %s", c->type, (char *)c->left);
    }

    switch (c->type) {
    case COND_STREQ:
	return matchpat(c->left, c->right);
    case COND_STRNEQ:
	return !matchpat(c->left, c->right);
    case COND_STRLT:
	return strcmp(c->left, c->right) < 0;
    case COND_STRGTR:
	return strcmp(c->left, c->right) > 0;
    case 'e':
    case 'a':
	return (doaccess(c->left, F_OK));
    case 'b':
	return (S_ISBLK(dostat(c->left)));
    case 'c':
	return (S_ISCHR(dostat(c->left)));
    case 'd':
	return (S_ISDIR(dostat(c->left)));
    case 'f':
	return (S_ISREG(dostat(c->left)));
    case 'g':
	return (!!(dostat(c->left) & S_ISGID));
    case 'k':
	return (!!(dostat(c->left) & S_ISVTX));
    case 'n':
	return (!!strlen(c->left));
    case 'o':
	return (optison(c->left));
    case 'p':
	return (S_ISFIFO(dostat(c->left)));
    case 'r':
	return (doaccess(c->left, R_OK));
    case 's':
	return ((st = getstat(c->left)) && !!(st->st_size));
    case 'S':
	return (S_ISSOCK(dostat(c->left)));
    case 'u':
	return (!!(dostat(c->left) & S_ISUID));
    case 'w':
	return (doaccess(c->left, W_OK));
    case 'x':
	if (privasserted()) {
	    mode_t mode = dostat(c->left);
	    return (mode & S_IXUGO) || S_ISDIR(mode);
	}
	return doaccess(c->left, X_OK);
    case 'z':
	return (!strlen(c->left));
    case 'h':
    case 'L':
	return (S_ISLNK(dolstat(c->left)));
    case 'O':
	return ((st = getstat(c->left)) && st->st_uid == geteuid());
    case 'G':
	return ((st = getstat(c->left)) && st->st_gid == getegid());
    case 'N':
	return ((st = getstat(c->left)) && st->st_atime <= st->st_mtime);
    case 't':
	return isatty(matheval(c->left));
    case COND_EQ:
	return matheval(c->left) == matheval(c->right);
    case COND_NE:
	return matheval(c->left) != matheval(c->right);
    case COND_LT:
	return matheval(c->left) < matheval(c->right);
    case COND_GT:
	return matheval(c->left) > matheval(c->right);
    case COND_LE:
	return matheval(c->left) <= matheval(c->right);
    case COND_GE:
	return matheval(c->left) >= matheval(c->right);
    case COND_NT:
    case COND_OT:
	{
	    time_t a;

	    if (!(st = getstat(c->left)))
		return 0;
	    a = st->st_mtime;
	    if (!(st = getstat(c->right)))
		return 0;
	    return (c->type == COND_NT) ? a > st->st_mtime : a < st->st_mtime;
	}
    case COND_EF:
	{
	    dev_t d;
	    ino_t i;

	    if (!(st = getstat(c->left)))
		return 0;
	    d = st->st_dev;
	    i = st->st_ino;
	    if (!(st = getstat(c->right)))
		return 0;
	    return d == st->st_dev && i == st->st_ino;
	}
    default:
	zerr("bad cond structure", NULL, 0);
    }
    return 0;
}


/**/
static int
doaccess(char *s, int c)
{
    return !access(unmeta(s), c);
}


static struct stat st;

/**/
static struct stat *
getstat(char *s)
{
/* /dev/fd/n refers to the open file descriptor n.  We always use fstat *
 * in this case since on Solaris /dev/fd/n is a device special file     */
    if (!strncmp(s, "/dev/fd/", 8)) {
	if (fstat(atoi(s + 8), &st))
	    return NULL;
        return &st;
    }

    if (stat(unmeta(s), &st))
	return NULL;
    return &st;
}


/**/
static mode_t
dostat(char *s)
{
    struct stat *statp;

    if (!(statp = getstat(s)))
	return 0;
    return statp->st_mode;
}


/* pem@aaii.oz; needed since dostat now uses "stat" */

/**/
static mode_t
dolstat(char *s)
{
    if (lstat(unmeta(s), &st) < 0)
	return 0;
    return st.st_mode;
}


/**/
static int
optison(char *s)
{
    int i;

    if (strlen(s) == 1)
	i = optlookupc(*s);
    else
	i = optlookup(s);
    if (!i) {
	zerr("no such option: %s", s, 0);
	return 0;
    } else if(i < 0)
	return unset(-i);
    else
	return isset(i);
}

/**/
char *
cond_str(char **args, int num)
{
    char *s = args[num];

    singsub(&s);
    untokenize(s);

    return s;
}

/**/
zlong
cond_val(char **args, int num)
{
    char *s = args[num];

    singsub(&s);
    untokenize(s);

    return matheval(s);
}

/**/
int
cond_match(char **args, int num, char *str)
{
    char *s = args[num];

    singsub(&s);

    return matchpat(str, s);
}

/**/
static void
tracemodcond(char *name, char **args, int inf)
{
    char **aptr;
    MUSTUSEHEAP("tracemodcond");
    args = duparray(args, (VFunc) dupstring);
    for (aptr = args; *aptr; aptr++)
	untokenize(*aptr);
    if (inf) {
	fprintf(stderr, " %s %s %s", args[0], name, args[1]);
    } else {
	fprintf(stderr, " %s", name);
	while (*args)
	    fprintf(stderr, " %s", *args++);
    }
}
