/*
 * loop.c - loop execution
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
#include "loop.pro"

/* # of nested loops we are in */
 
/**/
int loops;
 
/* # of continue levels */
 
/**/
int contflag;
 
/* # of break levels */
 
/**/
int breaks;
 
/**/
int
execfor(Cmd cmd)
{
    List list;
    Forcmd node;
    char *str;
    int val;
    LinkList args;

    node = cmd->u.forcmd;
    args = cmd->args;
    if (node->condition) {
	str = node->name;
	singsub(&str);
	if (!errflag)
	    matheval(str);
	if (errflag)
	    return lastval = errflag;
    } else if (!node->inflag) {
	char **x;

	args = newlinklist();
	for (x = pparams; *x; x++)
	    addlinknode(args, ztrdup(*x));
    }
    lastval = 0;
    loops++;
    pushheap();
    for (;;) {
	if (node->condition) {
	    str = dupstring(node->condition);
	    singsub(&str);
	    if (!errflag) {
		while (iblank(*str))
		    str++;
		if (*str)
		    val = matheval(str);
		else
		    val = 1;
	    }
	    if (errflag) {
		if (breaks)
		    breaks--;
		lastval = 1;
		break;
	    }
	    if (!val)
		break;
	} else {
	    str = (char *) ugetnode(args);
	    if (!str)
		break;
	    setsparam(node->name, ztrdup(str));
	}
	list = (List) dupstruct(node->list);
	execlist(list, 1, (cmd->flags & CFLAG_EXEC) && empty(args));
	if (breaks) {
	    breaks--;
	    if (breaks || !contflag)
		break;
	    contflag = 0;
	}
	if (node->condition && !errflag) {
	    str = dupstring(node->advance);
	    singsub(&str);
	    if (!errflag)
		matheval(str);
	}
	if (errflag) {
	    if (breaks)
		breaks--;
	    lastval = 1;
	    break;
	}
	freeheap();
    }
    popheap();
    loops--;
    return lastval;
}

/**/
int
execselect(Cmd cmd)
{
    List list;
    Forcmd node;
    char *str, *s;
    LinkList args;
    LinkNode n;
    int i;
    FILE *inp;

    node = cmd->u.forcmd;
    args = cmd->args;
    if (!node->inflag) {
	char **x;

	args = newlinklist();
	for (x = pparams; *x; x++)
	    addlinknode(args, ztrdup(*x));
    }
    if (empty(args))
	return 1;
    loops++;
    lastval = 0;
    pushheap();
    inp = fdopen(dup((SHTTY == -1) ? 0 : SHTTY), "r");
    selectlist(args);
    for (;;) {
	for (;;) {
	    if (empty(bufstack)) {
	    	if (interact && SHTTY != -1 && isset(USEZLE)) {
		    isfirstln = 1;
		    str = (char *)zleread(prompt3, NULL, 0);
	    	} else {
		    str = promptexpand(prompt3, 0, NULL, NULL);
		    zputs(str, stderr);
		    free(str);
		    fflush(stderr);
		    str = fgets(zalloc(256), 256, inp);
	    	}
	    } else
		str = (char *)getlinknode(bufstack);
	    if (!str || errflag) {
		if (breaks)
		    breaks--;
		fprintf(stderr, "\n");
		fflush(stderr);
		goto done;
	    }
	    if ((s = strchr(str, '\n')))
		*s = '\0';
	    if (*str)
	      break;
	    selectlist(args);
	}
	setsparam("REPLY", ztrdup(str));
	i = atoi(str);
	if (!i)
	    str = "";
	else {
	    for (i--, n = firstnode(args); n && i; incnode(n), i--);
	    if (n)
		str = (char *) getdata(n);
	    else
		str = "";
	}
	setsparam(node->name, ztrdup(str));
	list = (List) dupstruct(node->list);
	execlist(list, 1, 0);
	freeheap();
	if (breaks) {
	    breaks--;
	    if (breaks || !contflag)
		break;
	    contflag = 0;
	}
	if (errflag)
	    break;
    }
  done:
    popheap();
    fclose(inp);
    loops--;
    return lastval;
}

/* And this is used to print select lists. */

/**/
static void
selectlist(LinkList l)
{
    size_t longest = 1, fct, fw = 0, colsz, t0, t1, ct;
    LinkNode n;
    char **arr, **ap;

    trashzle();
    ct = countlinknodes(l);
    ap = arr = (char **)alloc((countlinknodes(l) + 1) * sizeof(char **));

    for (n = (LinkNode) firstnode(l); n; incnode(n))
	*ap++ = (char *)getdata(n);
    *ap = NULL;
    for (ap = arr; *ap; ap++)
	if (strlen(*ap) > longest)
	    longest = strlen(*ap);
    t0 = ct;
    longest++;
    while (t0)
	t0 /= 10, longest++;
    /* to compensate for added ')' */
    fct = (columns - 1) / (longest + 3);
    if (fct == 0)
	fct = 1;
    else
	fw = (columns - 1) / fct;
    colsz = (ct + fct - 1) / fct;
    for (t1 = 0; t1 != colsz; t1++) {
	ap = arr + t1;
	do {
	    int t2 = strlen(*ap) + 2, t3;

	    fprintf(stderr, "%d) %s", t3 = ap - arr + 1, *ap);
	    while (t3)
		t2++, t3 /= 10;
	    for (; t2 < fw; t2++)
		fputc(' ', stderr);
	    for (t0 = colsz; t0 && *ap; t0--, ap++);
	}
	while (*ap);
	fputc('\n', stderr);
    }

 /* Below is a simple attempt at doing it the Korn Way..
       ap = arr;
       t0 = 0;
       do {
           t0++;
           fprintf(stderr,"%d) %s\n",t0,*ap);
           ap++;
       }
       while (*ap);*/
    fflush(stderr);
}

/**/
int
execwhile(Cmd cmd)
{
    List list;
    struct whilecmd *node;
    int olderrexit, oldval;

    olderrexit = noerrexit;
    node = cmd->u.whilecmd;
    oldval = 0;
    pushheap();
    loops++;
    for (;;) {
	list = (List) dupstruct(node->cont);
	noerrexit = 1;
	execlist(list, 1, 0);
	noerrexit = olderrexit;
	if (!((lastval == 0) ^ node->cond)) {
	    if (breaks)
		breaks--;
	    lastval = oldval;
	    break;
	}
	list = (List) dupstruct(node->loop);
	execlist(list, 1, 0);
	if (breaks) {
	    breaks--;
	    if (breaks || !contflag)
		break;
	    contflag = 0;
	}
	freeheap();
	if (errflag) {
	    lastval = 1;
	    break;
	}
	oldval = lastval;
    }
    popheap();
    loops--;
    return lastval;
}

/**/
int
execrepeat(Cmd cmd)
{
    List list;
    int count;

    lastval = 0;
    if (empty(cmd->args) || nextnode(firstnode(cmd->args))) {
	zerr("bad argument for repeat", NULL, 0);
	return 1;
    }
    count = atoi(peekfirst(cmd->args));
    pushheap();
    loops++;
    while (count--) {
	list = (List) dupstruct(cmd->u.list);
	execlist(list, 1, 0);
	freeheap();
	if (breaks) {
	    breaks--;
	    if (breaks || !contflag)
		break;
	    contflag = 0;
	}
	if (errflag) {
	    lastval = 1;
	    break;
	}
    }
    popheap();
    loops--;
    return lastval;
}

/**/
int
execif(Cmd cmd)
{
    struct ifcmd *node;
    int olderrexit;
    List *i, *t;

    olderrexit = noerrexit;
    node = cmd->u.ifcmd;
    i = node->ifls;
    t = node->thenls;

    if (!noerrexit)
	noerrexit = 1;
    while (*i) {
	execlist(*i, 1, 0);
	if (!lastval)
	    break;
	i++;
	t++;
    }
    noerrexit = olderrexit;

    if (*t)
	execlist(*t, 1, cmd->flags & CFLAG_EXEC);
    else
	lastval = 0;

    return lastval;
}

/**/
int
execcase(Cmd cmd)
{
    struct casecmd *node;
    char *word;
    List *l;
    char **p;

    node = cmd->u.casecmd;
    l = node->lists;
    p = node->pats;

    word = *p++;
    singsub(&word);
    untokenize(word);
    lastval = 0;

    if (node) {
	while (*p) {
	    char *pat = *p + 1;
	    singsub(&pat);
	    if (matchpat(word, pat)) {
		do {
		    execlist(*l++, 1, **p == ';' && (cmd->flags & CFLAG_EXEC));
		} while(**p++ == '&' && *p);
		break;
	    }
	    p++;
	    l++;
	}
    }
    return lastval;
}

