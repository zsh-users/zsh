/*
 * text.c - textual representations of syntax trees
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
#include "text.pro"

static char *tptr, *tbuf, *tlim;
static int tsiz, tindent, tnewlins;

/* add a character to the text buffer */

/**/
static void
taddchr(int c)
{
    *tptr++ = c;
    if (tptr == tlim) {
	if (!tbuf) {
	    tptr--;
	    return;
	}
	tbuf = realloc(tbuf, tsiz *= 2);
	tlim = tbuf + tsiz;
	tptr = tbuf + tsiz / 2;
    }
}

/* add a string to the text buffer */

/**/
static void
taddstr(char *s)
{
    int sl = strlen(s);

    while (tptr + sl >= tlim) {
	int x = tptr - tbuf;

	if (!tbuf)
	    return;
	tbuf = realloc(tbuf, tsiz *= 2);
	tlim = tbuf + tsiz;
	tptr = tbuf + x;
    }
    strcpy(tptr, s);
    tptr += sl;
}

#if 0
/* add an integer to the text buffer */

/**/
void
taddint(int x)
{
    char buf[DIGBUFSIZE];

    sprintf(buf, "%d", x);
    taddstr(buf);
}
#endif

/* add a newline, or something equivalent, to the text buffer */

/**/
static void
taddnl(void)
{
    int t0;

    if (tnewlins) {
	taddchr('\n');
	for (t0 = 0; t0 != tindent; t0++)
	    taddchr('\t');
    } else
	taddstr("; ");
}

/* get a permanent textual representation of n */

/**/
char *
getpermtext(struct node *n)
{
    tnewlins = 1;
    tbuf = (char *)zalloc(tsiz = 32);
    tptr = tbuf;
    tlim = tbuf + tsiz;
    tindent = 1;
    gettext2(n);
    *tptr = '\0';
    untokenize(tbuf);
    return tbuf;
}

/* get a representation of n in a job text buffer */

/**/
char *
getjobtext(struct node *n)
{
    static char jbuf[JOBTEXTSIZE];

    tnewlins = 0;
    tbuf = NULL;
    tptr = jbuf;
    tlim = tptr + JOBTEXTSIZE - 1;
    tindent = 1;
    gettext2(n);
    *tptr = '\0';
    untokenize(jbuf);
    return jbuf;
}

#define gt2(X) gettext2((struct node *) (X))

/*
	"gettext2" or "type checking and how to avoid it"
	an epic function by Paul Falstad
*/

#define _Cond(X) ((Cond) (X))
#define _Cmd(X) ((Cmd) (X))
#define _Pline(X) ((Pline) (X))
#define _Sublist(X) ((Sublist) (X))
#define _List(X) ((List) (X))
#define _casecmd(X) ((struct casecmd *) (X))
#define _ifcmd(X) ((struct ifcmd *) (X))
#define _whilecmd(X) ((struct whilecmd *) (X))

/**/
static void
gettext2(struct node *n)
{
    Cmd nn;

    if (!n || ((List) n) == &dummy_list)
	return;
    switch (NT_TYPE(n->ntype)) {
    case N_LIST:
	gt2(_List(n)->left);
	if (_List(n)->type & Z_ASYNC) {
	    taddstr(" &");
	    if (_List(n)->type & Z_DISOWN)
		taddstr("|");
	}
	simplifyright(_List(n));
	if (_List(n)->right) {
	    if (tnewlins)
		taddnl();
	    else
		taddstr((_List(n)->type & Z_ASYNC) ? " " : "; ");
	    gt2(_List(n)->right);
	}
	break;
    case N_SUBLIST:
	if (_Sublist(n)->flags & PFLAG_NOT)
	    taddstr("! ");
	if (_Sublist(n)->flags & PFLAG_COPROC)
	    taddstr("coproc ");
	gt2(_Sublist(n)->left);
	if (_Sublist(n)->right) {
	    taddstr((_Sublist(n)->type == ORNEXT) ? " || " : " && ");
	    gt2(_Sublist(n)->right);
	}
	break;
    case N_PLINE:
	gt2(_Pline(n)->left);
	if (_Pline(n)->type == PIPE) {
	    taddstr(" | ");
	    gt2(_Pline(n)->right);
	}
	break;
    case N_CMD:
	nn = _Cmd(n);
	switch (nn->type) {
	case SIMPLE:
	    getsimptext(nn);
	    break;
	case SUBSH:
	    taddstr("( ");
	    tindent++;
	    gt2(nn->u.list);
	    tindent--;
	    taddstr(" )");
	    break;
	case ZCTIME:
	    taddstr("time ");
	    tindent++;
	    gt2(nn->u.pline);
	    tindent--;
	    break;
	case FUNCDEF:
	    taddlist(nn->args);
	    taddstr(" () {");
	    tindent++;
	    taddnl();
	    gt2(nn->u.list);
	    tindent--;
	    taddnl();
	    taddstr("}");
	    break;
	case CURSH:
	    taddstr("{ ");
	    tindent++;
	    gt2(nn->u.list);
	    tindent--;
	    taddstr(" }");
	    break;
	case CFOR:
	case CSELECT:
	    taddstr((nn->type == CFOR) ? "for " : "select ");
	    if (nn->u.forcmd->condition) {
		taddstr("((");
		taddstr(nn->u.forcmd->name);
		taddstr("; ");
		taddstr(nn->u.forcmd->condition);
		taddstr("; ");
		taddstr(nn->u.forcmd->advance);
		taddstr(")) do");
	    } else {
		taddstr(nn->u.forcmd->name);
		if (nn->u.forcmd->inflag) {
		    taddstr(" in ");
		    taddlist(nn->args);
		}
		taddnl();
		taddstr("do");
	    }
	    tindent++;
	    taddnl();
	    gt2(nn->u.forcmd->list);
	    tindent--;
	    taddnl();
	    taddstr("done");
	    break;
	case CIF:
	    gt2(nn->u.ifcmd);
	    taddstr("fi");
	    break;
	case CCASE:
	    gt2(nn->u.casecmd);
	    break;
	case COND:
	    taddstr("[[ ");
	    gt2(nn->u.cond);
	    taddstr(" ]]");
	    break;
	case CARITH:
	    taddstr("((");
	    taddlist(nn->args);
	    taddstr("))");
	    break;
	case CREPEAT:
	    taddstr("repeat ");
	    taddlist(nn->args);
	    taddnl();
	    taddstr("do");
	    tindent++;
	    taddnl();
	    gt2(nn->u.list);
	    tindent--;
	    taddnl();
	    taddstr("done");
	    break;
	case CWHILE:
	    gt2(nn->u.whilecmd);
	    break;
	}
	getredirs(nn);
	break;
    case N_COND:
	getcond(_Cond(n), 0);
	break;
    case N_CASE:
	{
	    List *l;
	    char **p;

	    l = _casecmd(n)->lists;
	    p = _casecmd(n)->pats;

	    taddstr("case ");
	    taddstr(*p++);
	    taddstr(" in");
	    tindent++;
	    for (; *l; p++, l++) {
		if (tnewlins)
		    taddnl();
		else
		    taddchr(' ');
		taddstr(*p + 1);
		taddstr(") ");
		tindent++;
		gt2(*l);
		tindent--;
		taddstr(" ;");
		taddchr(**p);
	    }
	    tindent--;
	    if (tnewlins)
		taddnl();
	    else
		taddchr(' ');
	    taddstr("esac");
	    break;
	}
    case N_IF:
	{
	    List *i, *t;

	    taddstr("if ");
	    for (i = _ifcmd(n)->ifls, t = _ifcmd(n)->thenls; *i; i++, t++) {
		tindent++;
		gt2(*i);
		tindent--;
		taddnl();
		taddstr("then");
		tindent++;
		taddnl();
		gt2(*t);
		tindent--;
		taddnl();
		if (i[1]) {
		    taddstr("elif ");
		}
	    }
	    if (*t) {
		taddstr("else");
		tindent++;
		taddnl();
		gt2(*t);
		tindent--;
		taddnl();
	    }
	    break;
	}
    case N_WHILE:
	taddstr((_whilecmd(n)->cond) ? "until " : "while ");
	tindent++;
	gt2(_whilecmd(n)->cont);
	tindent--;
	taddnl();
	taddstr("do");
	tindent++;
	taddnl();
	gt2(_whilecmd(n)->loop);
	tindent--;
	taddnl();
	taddstr("done");
	break;
    }
}

/* Print a condition bracketed by [[ ... ]].             *
 * With addpar non-zero, parenthesise the subexpression. */

/**/
static void
getcond(Cond nm, int addpar)
{
    static char *c1[] =
    {
	"=", "!=", "<", ">", "-nt", "-ot", "-ef", "-eq",
	"-ne", "-lt", "-gt", "-le", "-ge"
    };

    if (addpar)
	taddstr("( ");
    switch (nm->type) {
    case COND_NOT:
	taddstr("! ");
	getcond(nm->left, _Cond(nm->left)->type <= COND_OR);
	break;
    case COND_AND:
	getcond(nm->left, _Cond(nm->left)->type == COND_OR);
	taddstr(" && ");
	getcond(nm->right, _Cond(nm->right)->type == COND_OR);
	break;
    case COND_OR:
	/* This is deliberately over-generous with parentheses: *
	 * in fact omitting them gives correct precedence.      */
	getcond(nm->left, _Cond(nm->left)->type == COND_AND);
	taddstr(" || ");
	getcond(nm->right, _Cond(nm->right)->type == COND_AND);
	break;
    default:
	if (nm->type <= COND_GE) {
	    /* Binary test: `a = b' etc. */
	    taddstr(nm->left);
	    taddstr(" ");
	    taddstr(c1[nm->type - COND_STREQ]);
	    taddstr(" ");
	    taddstr(nm->right);
	} else {
	    /* Unary test: `-f foo' etc. */ 
	    char c2[4];

	    c2[0] = '-';
	    c2[1] = nm->type;
	    c2[2] = ' ';
	    c2[3] = '\0';
	    taddstr(c2);
	    taddstr(nm->left);
	}
	break;
    }
    if (addpar)
	taddstr(" )");
}

/**/
static void
getsimptext(Cmd cmd)
{
    LinkNode n;

    for (n = firstnode(cmd->vars); n; incnode(n)) {
	struct varasg *v = (struct varasg *)getdata(n);

	taddstr(v->name);
	taddchr('=');
	if (PM_TYPE(v->type) == PM_ARRAY) {
	    taddchr('(');
	    taddlist(v->arr);
	    taddstr(") ");
	} else {
	    taddstr(v->str);
	    taddchr(' ');
	}
    }
    taddlist(cmd->args);
}

/**/
void
getredirs(Cmd cmd)
{
    LinkNode n;
    static char *fstr[] =
    {
	">", ">|", ">>", ">>|", "&>", "&>|", "&>>", "&>>|", "<>", "<",
	"<<", "<<-", "<<<", "<&", ">&", NULL /* >&- */, "<", ">"
    };

    taddchr(' ');
    for (n = firstnode(cmd->redir); n; incnode(n)) {
	struct redir *f = (struct redir *)getdata(n);

	switch (f->type) {
	case WRITE:
	case WRITENOW:
	case APP:
	case APPNOW:
	case ERRWRITE:
	case ERRWRITENOW:
	case ERRAPP:
	case ERRAPPNOW:
	case READ:
	case READWRITE:
	case HERESTR:
	case MERGEIN:
	case MERGEOUT:
	case INPIPE:
	case OUTPIPE:
	    if (f->fd1 != (IS_READFD(f->type) ? 0 : 1))
		taddchr('0' + f->fd1);
	    taddstr(fstr[f->type]);
	    taddchr(' ');
	    taddstr(f->name);
	    taddchr(' ');
	    break;
#ifdef DEBUG
	case CLOSE:
	    DPUTS(1, "BUG: CLOSE in getredirs()");
	    taddchr(f->fd1 + '0');
	    taddstr(">&- ");
	    break;
	default:
	    DPUTS(1, "BUG: unknown redirection in getredirs()");
#endif
	}
    }
    tptr--;
}

/**/
static void
taddlist(LinkList l)
{
    LinkNode n;

    if (!(n = firstnode(l)))
	return;
    for (; n; incnode(n)) {
	taddstr(getdata(n));
	taddchr(' ');
    }
    tptr--;
}
