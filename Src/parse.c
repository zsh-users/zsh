/*
 * parse.c - parser
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
#include "parse.pro"

/* != 0 if we are about to read a command word */
 
/**/
int incmdpos;
 
/* != 0 if we are in the middle of a [[ ... ]] */
 
/**/
int incond;
 
/* != 0 if we are after a redirection (for ctxtlex only) */
 
/**/
int inredir;
 
/* != 0 if we are about to read a case pattern */
 
/**/
int incasepat;
 
/* != 0 if we just read a newline */
 
/**/
int isnewlin;

/* != 0 if we are after a for keyword */

/**/
int infor;

/* list of here-documents */

/**/
struct heredocs *hdocs;
 
/* used in arrays of lists instead of NULL pointers */
 
/**/
struct list dummy_list;

#define YYERROR  { tok = LEXERR; return NULL; }
#define YYERRORV { tok = LEXERR; return; }
#define COND_ERROR(X,Y) do{herrflush();zerr(X,Y,0);YYERROR}while(0)

#define make_list()     allocnode(N_LIST)
#define make_sublist()  allocnode(N_SUBLIST)
#define make_pline()    allocnode(N_PLINE)
#define make_cmd()      allocnode(N_CMD)
#define make_forcmd()   allocnode(N_FOR)
#define make_casecmd()  allocnode(N_CASE)
#define make_ifcmd()    allocnode(N_IF)
#define make_whilecmd() allocnode(N_WHILE)
#define make_varnode()  allocnode(N_VARASG)
#define make_cond()     allocnode(N_COND)

/*
 * event	: ENDINPUT
 *			| SEPER
 *			| sublist [ SEPER | AMPER | AMPERBANG ]
 */
/**/
List
parse_event(void)
{
    tok = ENDINPUT;
    incmdpos = 1;
    yylex();
    return par_event();
}

/**/
static List
par_event(void)
{
    Sublist sl;
    List l = NULL;

    while (tok == SEPER) {
	if (isnewlin > 0)
	    return NULL;
	yylex();
    }
    if (tok == ENDINPUT)
	return NULL;
    if ((sl = par_sublist()))
	if (tok == ENDINPUT) {
	    l = (List) make_list();
	    l->type = Z_SYNC;
	    l->left = sl;
	} else if (tok == SEPER) {
	    l = (List) make_list();
	    l->type = Z_SYNC;
	    l->left = sl;
	    if (isnewlin <= 0)
		yylex();
	} else if (tok == AMPER) {
	    l = (List) make_list();
	    l->type = Z_ASYNC;
	    l->left = sl;
	    yylex();
	} else if (tok == AMPERBANG) {
	    l = (List) make_list();
	    l->type = Z_ASYNC | Z_DISOWN;
	    l->left = sl;
	    yylex();
	} else
	    l = NULL;
    if (!l) {
	if (errflag) {
	    yyerror();
	    return NULL;
	}
	herrflush();
	yyerror();
	return NULL;
    } else {
	l->right = par_event();
    }
    return l;
}

/**/
List
parse_list(void)
{
    List ret;

    tok = ENDINPUT;
    incmdpos = 1;
    yylex();
    ret = par_list();
    if (tok == LEXERR) {
	yyerror();
	return NULL;
    }
    return ret;
}

/*
 * list	: { SEPER } [ sublist [ { SEPER | AMPER | AMPERBANG } list ] ]
 */

/**/
static List
par_list(void)
{
    Sublist sl;
    List l = NULL;

    while (tok == SEPER)
	yylex();
    if ((sl = par_sublist()))
	if (tok == SEPER || tok == AMPER || tok == AMPERBANG) {
	    l = (List) make_list();
	    l->left = sl;
	    l->type = (tok == SEPER) ? Z_SYNC :
		(tok == AMPER) ? Z_ASYNC : Z_ASYNC | Z_DISOWN;
	    incmdpos = 1;
	    do {
		yylex();
	    } while (tok == SEPER);
	    l->right = par_list();
	} else {
	    l = (List) make_list();
	    l->left = sl;
	    l->type = Z_SYNC;
	}
    return l;
}

/**/
static List
par_list1(void)
{
    Sublist sl;
    List l = NULL;

    if ((sl = par_sublist())) {
	l = (List) make_list();
	l->type = Z_SYNC;
	l->left = sl;
    }
    return l;
}

/*
 * sublist	: sublist2 [ ( DBAR | DAMPER ) { SEPER } sublist ]
 */

/**/
static Sublist
par_sublist(void)
{
    Sublist sl;

    if ((sl = par_sublist2()))
	if (tok == DBAR || tok == DAMPER) {
	    int qtok = tok;

	    cmdpush(tok == DBAR ? CS_CMDOR : CS_CMDAND);
	    yylex();
	    while (tok == SEPER)
		yylex();
	    sl->right = par_sublist();
	    sl->type = (qtok == DBAR) ? ORNEXT : ANDNEXT;
	    cmdpop();
	}
    return sl;
}

/*
 * sublist2	: [ COPROC | BANG ] pline
 */

/**/
static Sublist
par_sublist2(void)
{
    Sublist sl;
    Pline p;

    sl = (Sublist) make_sublist();
    if (tok == COPROC) {
	sl->flags |= PFLAG_COPROC;
	yylex();
    } else if (tok == BANG) {
	sl->flags |= PFLAG_NOT;
	yylex();
    }
    if (!(p = par_pline()) && !sl->flags)
	return NULL;
    sl->left = p;
    return sl;
}

/*
 * pline	: cmd [ ( BAR | BARAMP ) { SEPER } pline ]
 */

/**/
static Pline
par_pline(void)
{
    Cmd c;
    Pline p, p2;

    if (!(c = par_cmd()))
	return NULL;
    if (tok == BAR) {
	cmdpush(CS_PIPE);
	yylex();
	while (tok == SEPER)
	    yylex();
	p2 = par_pline();
	cmdpop();
	p = (Pline) make_pline();
	p->left = c;
	p->right = p2;
	p->type = PIPE;
	return p;
    } else if (tok == BARAMP) {
	struct redir *rdr = (struct redir *)allocnode(N_REDIR);

	rdr->type = MERGEOUT;
	rdr->fd1 = 2;
	rdr->name = dupstring("1");
	addlinknode(c->redir, rdr);

	cmdpush(CS_ERRPIPE);
	yylex();
	p2 = par_pline();
	cmdpop();
	p = (Pline) make_pline();
	p->left = c;
	p->right = p2;
	p->type = PIPE;
	return p;
    } else {
	p = (Pline) make_pline();
	p->left = c;
	p->type = END;
	return p;
    }
}

/*
 * cmd	: { redir } ( for | case | if | while | repeat |
 *				subsh | funcdef | time | dinbrack | dinpar | simple ) { redir }
 */

/**/
static Cmd
par_cmd(void)
{
    Cmd c;

    c = (Cmd) make_cmd();
    c->lineno = lineno;
    c->args = newlinklist();
    c->redir = newlinklist();
    c->vars = newlinklist();
    while (IS_REDIROP(tok))
	par_redir(c->redir);
    switch (tok) {
    case FOR:
	cmdpush(CS_FOR);
	par_for(c);
	cmdpop();
	break;
    case FOREACH:
	cmdpush(CS_FOREACH);
	par_for(c);
	cmdpop();
	break;
    case SELECT:
	cmdpush(CS_SELECT);
	par_for(c);
	cmdpop();
	break;
    case CASE:
	cmdpush(CS_CASE);
	par_case(c);
	cmdpop();
	break;
    case IF:
	par_if(c);
	break;
    case WHILE:
	cmdpush(CS_WHILE);
	par_while(c);
	cmdpop();
	break;
    case UNTIL:
	cmdpush(CS_UNTIL);
	par_while(c);
	cmdpop();
	break;
    case REPEAT:
	cmdpush(CS_REPEAT);
	par_repeat(c);
	cmdpop();
	break;
    case INPAR:
	cmdpush(CS_SUBSH);
	par_subsh(c);
	cmdpop();
	break;
    case INBRACE:
	cmdpush(CS_CURSH);
	par_subsh(c);
	cmdpop();
	break;
    case FUNC:
	cmdpush(CS_FUNCDEF);
	par_funcdef(c);
	cmdpop();
	break;
    case TIME:
	par_time(c);
	break;
    case DINBRACK:
	cmdpush(CS_COND);
	par_dinbrack(c);
	cmdpop();
	break;
    case DINPAR:
	c->type = CARITH;
	addlinknode(c->args, tokstr);
	yylex();
	break;
    default:
	if (!par_simple(c))
	    return NULL;
	break;
    }
    while (IS_REDIROP(tok))
	par_redir(c->redir);
    incmdpos = 1;
    incasepat = 0;
    incond = 0;
    return c;
}

/*
 * for  : ( FOR DINPAR expr SEMI expr SEMI expr DOUTPAR |
 *    ( FOR[EACH] | SELECT ) name ( "in" wordlist | INPAR wordlist OUTPAR ) )
 *	{ SEPER } ( DO list DONE | INBRACE list OUTBRACE | list ZEND | list1 )
 */

/**/
static void
par_for(Cmd c)
{
    Forcmd f;
    int csh = (tok == FOREACH);

    f = (Forcmd) make_forcmd();
    c->type = (tok == SELECT) ? CSELECT : CFOR;
    incmdpos = 0;
    infor = tok == FOR ? 2 : 0;
    yylex();
    if (tok == DINPAR) {
	yylex();
	if (tok != DINPAR)
	    YYERRORV;
	f->name = tokstr;
	yylex();
	if (tok != DINPAR)
	    YYERRORV;
	f->condition = tokstr;
	yylex();
	if (tok != DOUTPAR)
	    YYERRORV;
	f->advance = tokstr;
	infor = 0;
	incmdpos = 1;
	yylex();
    } else {
	infor = 0;
	if (tok != STRING || !isident(tokstr))
	    YYERRORV;
	f->name = tokstr;
	incmdpos = 1;
	yylex();
	if (tok == STRING && !strcmp(tokstr, "in")) {
	    f->inflag = 1;
	    incmdpos = 0;
	    yylex();
	    c->args = par_wordlist();
	    if (tok != SEPER)
		YYERRORV;
	} else if (tok == INPAR) {
	    f->inflag = 1;
	    incmdpos = 0;
	    yylex();
	    c->args = par_nl_wordlist();
	    if (tok != OUTPAR)
		YYERRORV;
	    incmdpos = 1;
	    yylex();
	}
    }
    incmdpos = 1;
    while (tok == SEPER)
	yylex();
    if (tok == DO) {
	yylex();
	f->list = par_list();
	if (tok != DONE)
	    YYERRORV;
	yylex();
    } else if (tok == INBRACE) {
	yylex();
	f->list = par_list();
	if (tok != OUTBRACE)
	    YYERRORV;
	yylex();
    } else if (csh || isset(CSHJUNKIELOOPS)) {
	f->list = par_list();
	if (tok != ZEND)
	    YYERRORV;
	yylex();
    } else if (unset(SHORTLOOPS)) {
	YYERRORV;
    } else
	f->list = par_list1();
    c->u.forcmd = f;
}

/*
 * case	: CASE STRING { SEPER } ( "in" | INBRACE )
				{ { SEPER } STRING { BAR STRING } OUTPAR
					list [ DSEMI | SEMIAMP ] }
				{ SEPER } ( "esac" | OUTBRACE )
 */

/**/
static void
par_case(Cmd c)
{
    int brflag;
    LinkList pats, lists;
    int n = 1;
    char **pp;
    List *ll;
    LinkNode no;
    struct casecmd *cc;

    c->type = CCASE;
    incmdpos = 0;
    yylex();
    if (tok != STRING)
	YYERRORV;
    pats = newlinklist();
    addlinknode(pats, tokstr);
    incmdpos = 1;
    yylex();
    while (tok == SEPER)
	yylex();
    if (!(tok == STRING && !strcmp(tokstr, "in")) && tok != INBRACE)
	YYERRORV;
    brflag = (tok == INBRACE);
    incasepat = 1;
    incmdpos = 0;
    yylex();
    cc = c->u.casecmd = (struct casecmd *)make_casecmd();
    lists = newlinklist();
    for (;;) {
	char *str;

	while (tok == SEPER)
	    yylex();
	if (tok == OUTBRACE)
	    break;
	if (tok != STRING)
	    YYERRORV;
	if (!strcmp(tokstr, "esac"))
	    break;
	str = ncalloc(strlen(tokstr) + 2);
	*str = ';';
	strcpy(str + 1, tokstr);
	incasepat = 0;
	incmdpos = 1;
	for (;;) {
	    yylex();
	    if (tok == OUTPAR) {
		incasepat = 0;
		incmdpos = 1;
		yylex();
		break;
	    } else if (tok == BAR) {
		char *str2;
		int sl = strlen(str);

		incasepat = 1;
		incmdpos = 0;
		str2 = ncalloc(sl + 2);
		strcpy(str2, str);
		str2[sl] = Bar;
		str2[sl+1] = '\0';
		str = str2;
	    } else {
		int sl = strlen(str);

		if (str[sl - 1] != Bar) {
		    /* POSIX allows (foo*) patterns */
		    int pct;
		    char *s;

		    for (s = str + 1, pct = 0; *s; s++) {
			if (*s == Inpar)
			    pct++;
			if (!pct)
			    break;
			if (pct == 1) {
			    if (*s == Bar || *s == Inpar)
				while (iblank(s[1]))
				    chuck(s+1);
			    if (*s == Bar || *s == Outpar)
				while (iblank(s[-1]) &&
				       (s < str+2 || s[-2] != Meta))
				    chuck(--s);
			}
			if (*s == Outpar)
			    pct--;
		    }
		    if (*s || pct || s == str + 1)
			YYERRORV;
		    break;
		} else {
		    char *str2;

		    if (tok != STRING)
			YYERRORV;
		    str2 = ncalloc(sl + strlen(tokstr) + 1);
		    strcpy(str2, str);
		    strcpy(str2 + sl, tokstr);
		    str = str2;
		}
	    }
	}
	addlinknode(pats, str);
	addlinknode(lists, par_list());
	n++;
	if ((tok == ESAC && !brflag) || (tok == OUTBRACE && brflag))
	    break;
	if(tok == SEMIAMP)
	    *str = '&';
	else if (tok != DSEMI)
	    YYERRORV;
	incasepat = 1;
	incmdpos = 0;
	yylex();
    }

    incmdpos = 1;
    yylex();

    cc->pats = (char **)alloc((n + 1) * sizeof(char *));

    for (pp = cc->pats, no = firstnode(pats); no; incnode(no))
	*pp++ = (char *)getdata(no);
    *pp = NULL;
    cc->lists = (List *) alloc((n + 1) * sizeof(List));
    for (ll = cc->lists, no = firstnode(lists); no; incnode(no), ll++)
	if (!(*ll = (List) getdata(no)))
	    *ll = &dummy_list;
    *ll = NULL;
}

/*
 * if	: { ( IF | ELIF ) { SEPER } ( INPAR list OUTPAR | list )
			{ SEPER } ( THEN list | INBRACE list OUTBRACE | list1 ) }
			[ FI | ELSE list FI | ELSE { SEPER } INBRACE list OUTBRACE ]
			(you get the idea...?)
 */

/**/
static void
par_if(Cmd c)
{
    struct ifcmd *i;
    int xtok;
    unsigned char nc;
    LinkList ifsl, thensl;
    LinkNode no;
    int ni = 0, nt = 0, usebrace = 0;
    List l, *ll;

    ifsl = newlinklist();
    thensl = newlinklist();

    c->type = CIF;
    for (;;) {
	xtok = tok;
	cmdpush(xtok == IF ? CS_IF : CS_ELIF);
	yylex();
	if (xtok == FI)
	    break;
	if (xtok == ELSE)
	    break;
	while (tok == SEPER)
	    yylex();
	if (!(xtok == IF || xtok == ELIF)) {
	    cmdpop();
	    YYERRORV;
	}
	addlinknode(ifsl, par_list());
	ni++;
	incmdpos = 1;
	while (tok == SEPER)
	    yylex();
	xtok = FI;
	nc = cmdstack[cmdsp - 1] == CS_IF ? CS_IFTHEN : CS_ELIFTHEN;
	if (tok == THEN) {
	    usebrace = 0;
	    cmdpop();
	    cmdpush(nc);
	    yylex();
	    addlinknode(thensl, par_list());
	    nt++;
	    incmdpos = 1;
	    cmdpop();
	} else {
	    if (tok == INBRACE) {
		usebrace = 1;
		cmdpop();
		cmdpush(nc);
		yylex();
		l = par_list();
		if (tok != OUTBRACE) {
		    cmdpop();
		    YYERRORV;
		}
		addlinknode(thensl, l);
		nt++;
		yylex();
		incmdpos = 1;
		if (tok == SEPER)
		    break;
		cmdpop();
	    } else if (unset(SHORTLOOPS)) {
		cmdpop();
		YYERRORV;
	    } else {
		cmdpop();
		cmdpush(nc);
		addlinknode(thensl, par_list1());
		nt++;
		incmdpos = 1;
		break;
	    }
	}
    }
    cmdpop();
    if (xtok == ELSE) {
	cmdpush(CS_ELSE);
	while (tok == SEPER)
	    yylex();
	if (tok == INBRACE && usebrace) {
	    yylex();
	    l = par_list();
	    if (tok != OUTBRACE) {
		cmdpop();
		YYERRORV;
	    }
	} else {
	    l = par_list();
	    if (tok != FI) {
		cmdpop();
		YYERRORV;
	    }
	}
	addlinknode(thensl, l);
	nt++;
	yylex();
	cmdpop();
    }
    i = (struct ifcmd *)make_ifcmd();
    i->ifls = (List *) alloc((ni + 1) * sizeof(List));
    i->thenls = (List *) alloc((nt + 1) * sizeof(List));

    for (ll = i->ifls, no = firstnode(ifsl); no; incnode(no), ll++)
	if (!(*ll = (List) getdata(no)))
	    *ll = &dummy_list;
    *ll = NULL;
    for (ll = i->thenls, no = firstnode(thensl); no; incnode(no), ll++)
	if (!(*ll = (List) getdata(no)))
	    *ll = &dummy_list;
    *ll = NULL;

    c->u.ifcmd = i;
}

/*
 * while	: ( WHILE | UNTIL ) ( INPAR list OUTPAR | list ) { SEPER }
				( DO list DONE | INBRACE list OUTBRACE | list ZEND )
 */

/**/
static void
par_while(Cmd c)
{
    struct whilecmd *w;

    c->type = CWHILE;
    w = c->u.whilecmd = (struct whilecmd *)make_whilecmd();
    w->cond = (tok == UNTIL);
    yylex();
    w->cont = par_list();
    incmdpos = 1;
    while (tok == SEPER)
	yylex();
    if (tok == DO) {
	yylex();
	w->loop = par_list();
	if (tok != DONE)
	    YYERRORV;
	yylex();
    } else if (tok == INBRACE) {
	yylex();
	w->loop = par_list();
	if (tok != OUTBRACE)
	    YYERRORV;
	yylex();
    } else if (isset(CSHJUNKIELOOPS)) {
	w->loop = par_list();
	if (tok != ZEND)
	    YYERRORV;
	yylex();
    } else
	YYERRORV;
}

/*
 * repeat	: REPEAT STRING { SEPER } ( DO list DONE | list1 )
 */

/**/
static void
par_repeat(Cmd c)
{
    c->type = CREPEAT;
    incmdpos = 0;
    yylex();
    if (tok != STRING)
	YYERRORV;
    addlinknode(c->args, tokstr);
    incmdpos = 1;
    yylex();
    while (tok == SEPER)
	yylex();
    if (tok == DO) {
	yylex();
	c->u.list = par_list();
	if (tok != DONE)
	    YYERRORV;
	yylex();
    } else if (tok == INBRACE) {
	yylex();
	c->u.list = par_list();
	if (tok != OUTBRACE)
	    YYERRORV;
	yylex();
    } else if (isset(CSHJUNKIELOOPS)) {
	c->u.list = par_list();
	if (tok != ZEND)
	    YYERRORV;
	yylex();
    } else if (unset(SHORTLOOPS)) {
	YYERRORV;
    } else
	c->u.list = par_list1();
}

/*
 * subsh	: ( INPAR | INBRACE ) list ( OUTPAR | OUTBRACE )
 */

/**/
static void
par_subsh(Cmd c)
{
    c->type = (tok == INPAR) ? SUBSH : CURSH;
    yylex();
    c->u.list = par_list();
    if (tok != ((c->type == SUBSH) ? OUTPAR : OUTBRACE))
	YYERRORV;
    incmdpos = 1;
    yylex();
}

/*
 * funcdef	: FUNCTION wordlist [ INOUTPAR ] { SEPER }
 *					( list1 | INBRACE list OUTBRACE )
 */

/**/
static void
par_funcdef(Cmd c)
{
    nocorrect = 1;
    incmdpos = 0;
    yylex();
    c->type = FUNCDEF;
    c->args = newlinklist();
    incmdpos = 1;
    while (tok == STRING) {
	if (*tokstr == Inbrace && !tokstr[1]) {
	    tok = INBRACE;
	    break;
	}
	addlinknode(c->args, tokstr);
	yylex();
    }
    nocorrect = 0;
    if (tok == INOUTPAR)
	yylex();
    while (tok == SEPER)
	yylex();
    if (tok == INBRACE) {
	yylex();
	c->u.list = par_list();
	if (tok != OUTBRACE)
	    YYERRORV;
	yylex();
    } else if (unset(SHORTLOOPS)) {
	YYERRORV;
    } else
	c->u.list = par_list1();
}

/*
 * time	: TIME sublist2
 */

/**/
static void
par_time(Cmd c)
{
    yylex();
    c->type = ZCTIME;
    c->u.pline = par_sublist2();
}

/*
 * dinbrack	: DINBRACK cond DOUTBRACK
 */

/**/
static void
par_dinbrack(Cmd c)
{
    c->type = COND;
    incond = 1;
    incmdpos = 0;
    yylex();
    c->u.cond = par_cond();
    if (tok != DOUTBRACK)
	YYERRORV;
    incond = 0;
    incmdpos = 1;
    yylex();
}

/*
 * simple	: { COMMAND | EXEC | NOGLOB | NOCORRECT | DASH }
					{ STRING | ENVSTRING | ENVARRAY wordlist OUTPAR | redir }
					[ INOUTPAR { SEPER } ( list1 | INBRACE list OUTBRACE ) ]
 */

/**/
static Cmd
par_simple(Cmd c)
{
    int isnull = 1;

    c->type = SIMPLE;
    for (;;) {
	if (tok == NOCORRECT)
	    nocorrect = 1;
	else if (tok == ENVSTRING) {
	    struct varasg *v = (struct varasg *)make_varnode();

	    v->type = PM_SCALAR;
	    equalsplit(v->name = tokstr, &v->str);
	    addlinknode(c->vars, v);
	    isnull = 0;
	} else if (tok == ENVARRAY) {
	    struct varasg *v = (struct varasg *)make_varnode();
	    int oldcmdpos = incmdpos;

	    v->type = PM_ARRAY;
	    incmdpos = 0;
	    v->name = tokstr;
	    cmdpush(CS_ARRAY);
	    yylex();
	    v->arr = par_nl_wordlist();
	    cmdpop();
	    if (tok != OUTPAR)
		YYERROR;
	    incmdpos = oldcmdpos;
	    addlinknode(c->vars, v);
	    isnull = 0;
	} else
	    break;
	yylex();
    }
    if (tok == AMPER || tok == AMPERBANG)
	YYERROR;
    for (;;) {
	if (tok == STRING) {
	    incmdpos = 0;
	    addlinknode(c->args, tokstr);
	    yylex();
	} else if (IS_REDIROP(tok)) {
	    par_redir(c->redir);
	} else if (tok == INOUTPAR) {
	    incmdpos = 1;
	    cmdpush(CS_FUNCDEF);
	    yylex();
	    while (tok == SEPER)
		yylex();
	    if (tok == INBRACE) {
		yylex();
		c->u.list = par_list();
		if (tok != OUTBRACE) {
		    cmdpop();
		    YYERROR;
		}
		yylex();
	    } else
		c->u.list = (List) expandstruct((struct node *) par_cmd(), N_LIST);
	    cmdpop();
	    c->type = FUNCDEF;
	} else
	    break;
	isnull = 0;
    }
    if (isnull && empty(c->redir))
	return NULL;
    incmdpos = 1;
    return c;
}

/*
 * condlex is yylex for normal parsing, but is altered to allow
 * the test builtin to use par_cond.
 */

/**/
void (*condlex) _((void)) = yylex;

/*
 * cond	: cond_1 { SEPER } [ DBAR { SEPER } cond ]
 */

/**/
Cond
par_cond(void)
{
    Cond c, c2;

    c = par_cond_1();
    while (tok == SEPER)
	condlex();
    if (tok == DBAR) {
	condlex();
	while (tok == SEPER)
	    condlex();
	c2 = (Cond) make_cond();
	c2->left = (void *) c;
	c2->right = (void *) par_cond();
	c2->type = COND_OR;
	return c2;
    }
    return c;
}

/*
 * cond_1 : cond_2 { SEPER } [ DAMPER { SEPER } cond_1 ]
 */

/**/
static Cond
par_cond_1(void)
{
    Cond c, c2;

    c = par_cond_2();
    while (tok == SEPER)
	condlex();
    if (tok == DAMPER) {
	condlex();
	while (tok == SEPER)
	    condlex();
	c2 = (Cond) make_cond();
	c2->left = (void *) c;
	c2->right = (void *) par_cond_1();
	c2->type = COND_AND;
	return c2;
    }
    return c;
}

/*
 * cond_2	: BANG cond_2
				| INPAR { SEPER } cond_2 { SEPER } OUTPAR
				| STRING STRING STRING
				| STRING STRING
				| STRING ( INANG | OUTANG ) STRING
 */

/**/
static Cond
par_cond_2(void)
{
    Cond c, c2;
    char *s1, *s2, *s3;
    int dble = 0;

    if (condlex == testlex) {
	/* See the description of test in POSIX 1003.2 */
	if (tok == NULLTOK)
	    /* no arguments: false */
	    return par_cond_double(dupstring("-n"), dupstring(""));
	if (!*testargs) {
	    /* one argument: [ foo ] is equivalent to [ -n foo ] */
	    s1 = tokstr;
	    condlex();
	    return par_cond_double(dupstring("-n"), s1);
	}
	if (testargs[1] && !testargs[2]) {
	    /* three arguments: if the second argument is a binary operator, *
	     * perform that binary test on the first and the trird argument  */
	    if (!strcmp(*testargs, "=")  ||
		!strcmp(*testargs, "==") ||
		!strcmp(*testargs, "!=") ||
		(**testargs == '-' && get_cond_num(*testargs + 1) >= 0)) {
		s1 = tokstr;
		condlex();
		s2 = tokstr;
		condlex();
		s3 = tokstr;
		condlex();
		return par_cond_triple(s1, s2, s3);
	    }
	}
    }
    if (tok == BANG) {
	condlex();
	c = par_cond_2();
	c2 = (Cond) make_cond();
	c2->left = (void *) c;
	c2->type = COND_NOT;
	return c2;
    }
    if (tok == INPAR) {
	condlex();
	while (tok == SEPER)
	    condlex();
	c = par_cond();
	while (tok == SEPER)
	    condlex();
	if (tok != OUTPAR)
	    YYERROR;
	condlex();
	return c;
    }
    if (tok != STRING)
	if (tok && tok != LEXERR && condlex == testlex) {
	    s1 = tokstr;
	    condlex();
	    return par_cond_double("-n", s1);
	} else
	    YYERROR;
    s1 = tokstr;
    if (condlex == testlex)
	dble = (*s1 == '-' && strspn(s1+1, "abcdefghknoprstuwxzLONGS") == 1
		  && !s1[2]);
    condlex();
    if (tok == INANG || tok == OUTANG) {
	int xtok = tok;
	condlex();
	if (tok != STRING)
	    YYERROR;
	s3 = tokstr;
	condlex();
	c = (Cond) make_cond();
	c->left = (void *) s1;
	c->right = (void *) s3;
	c->type = (xtok == INANG) ? COND_STRLT : COND_STRGTR;
	c->ntype = NT_SET(N_COND, NT_STR, NT_STR, 0, 0);
	return c;
    }
    if (tok != STRING)
	if (tok != LEXERR && condlex == testlex) {
	    if (!dble)
		return par_cond_double("-n", s1);
	    else if (!strcmp(s1, "-t"))
		return par_cond_double(s1, "1");
	} else
	    YYERROR;
    s2 = tokstr;
    incond++;			/* parentheses do globbing */
    condlex();
    incond--;			/* parentheses do grouping */
    if (tok == STRING && !dble) {
	s3 = tokstr;
	condlex();
	return par_cond_triple(s1, s2, s3);
    } else
	return par_cond_double(s1, s2);
}

/*
 * redir	: ( OUTANG | ... | TRINANG ) STRING
 */

static int redirtab[TRINANG - OUTANG + 1] = {
    WRITE,
    WRITENOW,
    APP,
    APPNOW,
    READ,
    READWRITE,
    HEREDOC,
    HEREDOCDASH,
    MERGEIN,
    MERGEOUT,
    ERRWRITE,
    ERRWRITENOW,
    ERRAPP,
    ERRAPPNOW,
    HERESTR,
};

/**/
static void
par_redir(LinkList l)
{
    struct redir *fn = (struct redir *)allocnode(N_REDIR);
    int oldcmdpos, oldnc;

    oldcmdpos = incmdpos;
    incmdpos = 0;
    oldnc = nocorrect;
    if (tok != INANG && tok != INOUTANG)
	nocorrect = 1;
    fn->type = redirtab[tok - OUTANG];
    fn->fd1 = tokfd;
    yylex();
    if (tok != STRING && tok != ENVSTRING)
	YYERRORV;
    incmdpos = oldcmdpos;
    nocorrect = oldnc;

    /* assign default fd */
    if (fn->fd1 == -1)
	fn->fd1 = IS_READFD(fn->type) ? 0 : 1;

    fn->name = tokstr;

    switch (fn->type) {
    case HEREDOC:
    case HEREDOCDASH: {
	/* <<[-] name */
	struct heredocs **hd;

	for (hd = &hdocs; *hd; hd = &(*hd)->next);
	*hd = zalloc(sizeof(struct heredocs));
	(*hd)->next = NULL;
	(*hd)->rd = fn;
	break;
    }
    case WRITE:
    case WRITENOW:
	if (tokstr[0] == Outang && tokstr[1] == Inpar)
	    /* > >(...) */
	    fn->type = OUTPIPE;
	else if (tokstr[0] == Inang && tokstr[1] == Inpar)
	    YYERRORV;
	break;
    case READ:
	if (tokstr[0] == Inang && tokstr[1] == Inpar)
	    /* < <(...) */
	    fn->type = INPIPE;
	else if (tokstr[0] == Outang && tokstr[1] == Inpar)
	    YYERRORV;
	break;
    case READWRITE:
	if ((tokstr[0] == Inang || tokstr[0] == Outang) && tokstr[1] == Inpar)
	    fn->type = tokstr[0] == Inang ? INPIPE : OUTPIPE;
	break;
    }
    yylex();
    addlinknode(l, fn);
}

/*
 * wordlist	: { STRING }
 */

/**/
static LinkList
par_wordlist(void)
{
    LinkList l;

    l = newlinklist();
    while (tok == STRING) {
	addlinknode(l, tokstr);
	yylex();
    }
    return l;
}

/*
 * nl_wordlist	: { STRING | SEPER }
 */

/**/
static LinkList
par_nl_wordlist(void)
{
    LinkList l;

    l = newlinklist();
    while (tok == STRING || tok == SEPER) {
	if (tok != SEPER)
	    addlinknode(l, tokstr);
	yylex();
    }
    return l;
}

/**/
static Cond
par_cond_double(char *a, char *b)
{
    Cond n = (Cond) make_cond();

    if (a[0] != '-' || !a[1] || a[2])
	COND_ERROR("parse error: condition expected: %s", a);
    n->left = (void *) b;
    n->type = a[1];
    n->ntype = NT_SET(N_COND, NT_STR, NT_STR, 0, 0);
    return n;
}

/**/
static int
get_cond_num(char *tst)
{
    static char *condstrs[] =
    {
	"nt", "ot", "ef", "eq", "ne", "lt", "gt", "le", "ge", NULL
    };
    int t0;

    for (t0 = 0; condstrs[t0]; t0++)
	if (!strcmp(condstrs[t0], tst))
	    return t0;
    return -1;
}

/**/
static Cond
par_cond_triple(char *a, char *b, char *c)
{
    Cond n = (Cond) make_cond();
    int t0;

    if ((b[0] == Equals || b[0] == '=') &&
	(!b[1] || ((b[1] == Equals || b[1] == '=') && !b[2])))
	n->type = COND_STREQ;
    else if (b[0] == '!' && (b[1] == Equals || b[1] == '=') && !b[2])
	n->type = COND_STRNEQ;
    else if (b[0] == '-') {
	if ((t0 = get_cond_num(b + 1)) > -1)
	    n->type = t0 + COND_NT;
	else
	    COND_ERROR("unrecognized condition: %s", b);
    } else
	COND_ERROR("condition expected: %s", b);
    n->left = (void *) a;
    n->right = (void *) c;
    n->ntype = NT_SET(N_COND, NT_STR, NT_STR, 0, 0);
    return n;
}

/**/
static void
yyerror(void)
{
    int t0;

    for (t0 = 0; t0 != 20; t0++)
	if (!yytext || !yytext[t0] || yytext[t0] == '\n')
	    break;
    if (t0 == 20)
	zerr("parse error near `%l...'", yytext, 20);
    else if (t0)
	zerr("parse error near `%l'", yytext, t0);
    else
	zerr("parse error", NULL, 0);
}
