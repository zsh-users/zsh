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

/********************************/
/* Definitions for syntax trees */
/********************************/

typedef struct cond      *Cond;
typedef struct cmd       *Cmd;
typedef struct pline     *Pline;
typedef struct sublist   *Sublist;
typedef struct list      *List;
typedef struct forcmd    *Forcmd;
typedef struct autofn    *AutoFn;
typedef struct varasg    *Varasg;


/* struct list, struct sublist, struct pline, etc.  all fit the form *
 * of this structure and are used interchangably. The ptrs may hold  *
 * integers or pointers, depending on the type of the node.          */

/* Generic node structure for syntax trees */
struct node {
    int ntype;			/* node type */
};

#define N_LIST    0
#define N_SUBLIST 1
#define N_PLINE   2
#define N_CMD     3
#define N_REDIR   4
#define N_COND    5
#define N_FOR     6
#define N_CASE    7
#define N_IF      8
#define N_WHILE   9
#define N_VARASG 10
#define N_AUTOFN 11
#define N_COUNT  12

/* values for types[4] */

#define NT_EMPTY 0
#define NT_NODE  1
#define NT_STR   2
#define NT_PAT   3
#define NT_LIST  4
#define NT_ARR   8

#define NT_TYPE(T) ((T) & 0xff)
#define NT_N(T, N) (((T) >> (8 + (N) * 4)) & 0xf)
#define NT_SET(T0, T1, T2, T3, T4) \
    ((T0) | ((T1) << 8) | ((T2) << 12) | ((T3) << 16) | ((T4) << 20))

/* tree element for lists */

struct list {
    int ntype;			/* node type */
    int type;
    Sublist left;
    List right;
};

/* tree element for sublists */

struct sublist {
    int ntype;			/* node type */
    int type;
    int flags;			/* see PFLAGs below */
    Pline left;
    Sublist right;
};

#define ORNEXT  10		/* || */
#define ANDNEXT 11		/* && */

#define PFLAG_NOT     1		/* ! ... */
#define PFLAG_COPROC 32		/* coproc ... */

/* tree element for pipes */

struct pline {
    int ntype;			/* node type */
    int type;
    Cmd left;
    Pline right;
};

#define END	0		/* pnode *right is null                     */
#define PIPE	1		/* pnode *right is the rest of the pipeline */

/* tree element for commands */

struct cmd {
    int ntype;			/* node type */
    int type;
    int flags;			/* see CFLAGs below             */
    int lineno;			/* lineno of script for command */
    union {
	List list;		/* for SUBSH/CURSH/SHFUNC       */
	Forcmd forcmd;
	struct casecmd *casecmd;
	struct ifcmd *ifcmd;
	struct whilecmd *whilecmd;
	Sublist pline;
	Cond cond;
	AutoFn autofn;
	void *generic;
    } u;
    LinkList args;		/* command & argmument List (char *'s)   */
    LinkList redir;		/* i/o redirections (struct redir *'s)   */
    LinkList vars;		/* param assignments (struct varasg *'s) */
};

/* cmd types */
#define SIMPLE   0
#define SUBSH    1
#define CURSH    2
#define ZCTIME   3
#define FUNCDEF  4
#define CFOR     5
#define CWHILE   6
#define CREPEAT  7
#define CIF      8
#define CCASE    9
#define CSELECT 10
#define COND    11
#define CARITH  12

/* tree element for conditionals */

struct cond {
    int ntype;			/* node type */
    int type;		/* can be cond_type, or a single */
			/* letter (-a, -b, ...)          */
    void *left, *right;
};

struct forcmd {			/* for/select */
/* Cmd->args contains list of words to loop thru */
    int ntype;			/* node type */
    int inflag;			/* if there is an in ... clause       */
    char *name;			/* initializer or parameter name      */
    char *condition;		/* arithmetic terminating condition   */
    char *advance;		/* evaluated after each loop          */
    List list;			/* list to look through for each name */
};

struct casecmd {
/* Cmd->args contains word to test */
    int ntype;			/* node type */
    char **pats;		/* pattern strings */
    List *lists;		/* list to execute */
};

struct ifcmd {
    int ntype;			/* node type */
    List *ifls;
    List *thenls;
};

struct whilecmd {
    int ntype;			/* node type */
    int cond;			/* 0 for while, 1 for until            */
    List cont;			/* condition                           */
    List loop;			/* list to execute until condition met */
};

/* variable assignment tree element */

struct varasg {
    int ntype;			/* node type */
    int type;			/* nonzero means array                   */
    char *name;
    char *str;			/* should've been a union here.  oh well */
    LinkList arr;
};

#include "parse.pro"

/* != 0 if we are about to read a command word */
 
/**/
mod_export int incmdpos;
 
/* != 0 if we are in the middle of a [[ ... ]] */
 
/**/
mod_export int incond;
 
/* != 0 if we are after a redirection (for ctxtlex only) */
 
/**/
mod_export int inredir;
 
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
static struct list dummy_list;

#define YYERROR  { tok = LEXERR; return NULL; }
#define YYERRORV { tok = LEXERR; return; }
#define COND_ERROR(X,Y) do { \
  zwarn(X,Y,0); \
  herrflush(); \
  if (noerrs != 2) \
    errflag = 1; \
  YYERROR \
} while(0)

#define make_list()     allocnode(sizeof(struct list), N_LIST)
#define make_sublist()  allocnode(sizeof(struct sublist), N_SUBLIST)
#define make_pline()    allocnode(sizeof(struct pline), N_PLINE)
#define make_cmd()      allocnode(sizeof(struct cmd), N_CMD)
#define make_forcmd()   allocnode(sizeof(struct forcmd), N_FOR)
#define make_casecmd()  allocnode(sizeof(struct casecmd), N_CASE)
#define make_ifcmd()    allocnode(sizeof(struct ifcmd), N_IF)
#define make_whilecmd() allocnode(sizeof(struct whilecmd), N_WHILE)
#define make_varnode()  allocnode(sizeof(struct varasg), N_VARASG)
#define make_cond()     allocnode(sizeof(struct cond), N_COND)

static void *
allocnode(size_t s, int t)
{
    struct node *r = (struct node *) hcalloc(s);

    r->ntype = t;

    return (void *) r;
}

/*
 * event	: ENDINPUT
 *			| SEPER
 *			| sublist [ SEPER | AMPER | AMPERBANG ]
 */
/**/
Eprog
parse_event(void)
{
    List ret;
    tok = ENDINPUT;
    incmdpos = 1;
    yylex();
    return ((ret = par_event()) ? execompile(ret) : NULL);
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
    if ((sl = par_sublist())) {
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
    }
    if (!l) {
	if (errflag) {
	    yyerror(0);
	    return NULL;
	}
	yyerror(1);
	herrflush();
	if (noerrs != 2)
	    errflag = 1;
	return NULL;
    } else {
	l->right = par_event();
    }
    return l;
}

/**/
mod_export Eprog
parse_list(void)
{
    List ret;

    tok = ENDINPUT;
    incmdpos = 1;
    yylex();
    ret = par_list();
    if (tok == LEXERR) {
	yyerror(0);
	return NULL;
    }
    return execompile(ret);
}

/**/
mod_export Eprog
parse_cond(void)
{
    Cond c = par_cond();

    if (!c)
	return NULL;

    return execompile((List) c);
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
    if ((sl = par_sublist())) {
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
	struct redir *rdr = (struct redir *)
	    allocnode(sizeof(struct redir), N_REDIR);

	rdr->type = MERGEOUT;
	rdr->fd1 = 2;
	rdr->name = dupstring("1");
	if (!c->redir)
	    c->redir = newlinklist();
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
    c->args = NULL;
    c->vars = NULL;
    if (IS_REDIROP(tok)) {
	c->redir = newlinklist();
	while (IS_REDIROP(tok))
	    par_redir(c->redir);
    } else
	c->redir = NULL;
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
	if (!c->args)
	    c->args = newlinklist();
	addlinknode(c->args, tokstr);
	yylex();
	break;
    default:
	if (!par_simple(c))
	    return NULL;
	break;
    }
    if (IS_REDIROP(tok)) {
	if (!c->redir)
	    c->redir = newlinklist();
	while (IS_REDIROP(tok))
	    par_redir(c->redir);
    }
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
	    if (!c->args)
		c->args = newlinklist();
	    c->args = par_wordlist();
	    if (tok != SEPER)
		YYERRORV;
	} else if (tok == INPAR) {
	    f->inflag = 1;
	    incmdpos = 0;
	    yylex();
	    if (!c->args)
		c->args = newlinklist();
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
		    /* Simplify pattern by removing surrounding (...) */
		    sl = strlen(str);
		    DPUTS(str[1] != Inpar || str[sl-1] != Outpar,
			  "BUG: strange case pattern");
		    str[sl-1] = '\0';
		    chuck(str+1);
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

    cc->pats = (char **) alloc((n + 1) * sizeof(char *));

    for (pp = cc->pats, no = firstnode(pats);
	 no; incnode(no))
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
    if (!c->args)
	c->args = newlinklist();
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
    int oldlineno = lineno;
    lineno = 0;
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
	if (tok != OUTBRACE) {
	    lineno += oldlineno;
	    YYERRORV;
	}
	yylex();
    } else if (unset(SHORTLOOPS)) {
	lineno += oldlineno;
	YYERRORV;
    } else
	c->u.list = par_list1();
    lineno += oldlineno;
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
	    char *p;

	    v->type = PM_SCALAR;
	    v->name = tokstr;
	    for (p = tokstr; *p && *p != Inbrack && *p != '='; p++);
	    if (*p == Inbrack && !skipparens(Inbrack, Outbrack, &p) &&
		*p == '=') {
		*p = '\0';
		v->str = p + 1;
	    } else
		equalsplit(tokstr, &v->str);
	    if (!c->vars)
		c->vars = newlinklist();
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
	    if (!c->vars)
		c->vars = newlinklist();
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
	    if (!c->args)
		c->args = newlinklist();
	    addlinknode(c->args, tokstr);
	    yylex();
	} else if (IS_REDIROP(tok)) {
	    if (!c->redir)
		c->redir = newlinklist();
	    par_redir(c->redir);
	} else if (tok == INOUTPAR) {
	    int oldlineno = lineno;
	    lineno = 0;
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
		    lineno += oldlineno;
		    YYERROR;
		}
		yylex();
	    } else {
		List l;
		Sublist sl;
		Pline pl;

		l = (List) allocnode(sizeof(*l), N_LIST);
		l->type = Z_SYNC;
		l->left = sl = (Sublist) allocnode(sizeof(*sl), N_SUBLIST);
		sl->type = END;
		sl->left = pl = (Pline) allocnode(sizeof(*pl), N_PLINE);
		pl->type = END;
		pl->left = par_cmd();
		c->u.list = l;
	    }
	    cmdpop();
	    c->type = FUNCDEF;
	    lineno += oldlineno;
	} else
	    break;
	isnull = 0;
    }
    if (isnull && (!c->redir || empty(c->redir)))
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
static Cond
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
    if (tok != STRING) {
	if (tok && tok != LEXERR && condlex == testlex) {
	    s1 = tokstr;
	    condlex();
	    return par_cond_double("-n", s1);
	} else
	    YYERROR;
    }
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
    if (tok != STRING) {
	if (tok != LEXERR && condlex == testlex) {
	    if (!dble)
		return par_cond_double("-n", s1);
	    else if (!strcmp(s1, "-t"))
		return par_cond_double(s1, "1");
	} else
	    YYERROR;
    }
    s2 = tokstr;
    incond++;			/* parentheses do globbing */
    condlex();
    incond--;			/* parentheses do grouping */
    if (tok == STRING && !dble) {
	s3 = tokstr;
	condlex();
	if (tok == STRING) {
	    LinkList l = newlinklist();

	    addlinknode(l, s2);
	    addlinknode(l, s3);

	    while (tok == STRING) {
		addlinknode(l, tokstr);
		condlex();
	    }
	    return par_cond_multi(s1, l);
	} else
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
    struct redir *fn = (struct redir *)
	allocnode(sizeof(struct redir), N_REDIR);
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

    n->ntype = NT_SET(N_COND, NT_STR, NT_STR, 0, 0);
    n->left = (void *) b;
    if (a[0] != '-' || !a[1])
	COND_ERROR("parse error: condition expected: %s", a);
    else if (!a[2] && strspn(a+1, "abcdefgknoprstuwxzhLONGS") == 1)
	n->type = a[1];
    else {
	char *d[2];

	n->ntype = NT_SET(N_COND, NT_STR, NT_STR | NT_ARR, 0, 0);
	n->type = COND_MOD;
	n->left = (void *) a;
	d[0] = b;
	d[1] = NULL;
	n->right = (void *) arrdup(d);
    }
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

    n->left = (void *) a;
    n->right = (void *) c;
    if ((b[0] == Equals || b[0] == '=') &&
	(!b[1] || ((b[1] == Equals || b[1] == '=') && !b[2]))) {
	n->ntype = NT_SET(N_COND, NT_STR, NT_STR, NT_PAT, 0);
	n->type = COND_STREQ;
    } else if (b[0] == '!' && (b[1] == Equals || b[1] == '=') && !b[2]) {
	n->ntype = NT_SET(N_COND, NT_STR, NT_STR, NT_PAT, 0);
	n->type = COND_STRNEQ;
    } else if (b[0] == '-') {
	if ((t0 = get_cond_num(b + 1)) > -1) {
	    n->ntype = NT_SET(N_COND, NT_STR, NT_STR, 0, 0);
	    n->type = t0 + COND_NT;
	} else {
	    char *d[3];

	    n->ntype = NT_SET(N_COND, NT_STR, NT_STR | NT_ARR, 0, 0);
	    n->type = COND_MODI;
	    n->left = (void *) b;
	    d[0] = a;
	    d[1] = c;
	    d[2] = NULL;
	    n->right = (void *) arrdup(d);
	}
    } else if (a[0] == '-' && a[1]) {
	char *d[3];

	n->ntype = NT_SET(N_COND, NT_STR, NT_STR | NT_ARR, 0, 0);
	n->type = COND_MOD;
	n->left = (void *) a;
	d[0] = b;
	d[1] = c;
	d[2] = NULL;
	n->right = (void *) arrdup(d);
    } else
	COND_ERROR("condition expected: %s", b);
    return n;
}

/**/
static Cond
par_cond_multi(char *a, LinkList l)
{
    Cond n = (Cond) make_cond();

    n->ntype = NT_SET(N_COND, NT_STR, NT_STR | NT_ARR, 0, 0);
    if (a[0] != '-' || !a[1])
	COND_ERROR("condition expected: %s", a);
    else {
	n->type = COND_MOD;
	n->left = (void *) a;
	n->right = (void *) listarr(l);
    }
    return n;
}

/**/
static void
yyerror(int noerr)
{
    int t0;

    for (t0 = 0; t0 != 20; t0++)
	if (!yytext || !yytext[t0] || yytext[t0] == '\n')
	    break;
    if (t0 == 20)
	zwarn("parse error near `%l...'", yytext, 20);
    else if (t0)
	zwarn("parse error near `%l'", yytext, t0);
    else
	zwarn("parse error", NULL, 0);
    if (!noerr && noerrs != 2)
	errflag = 1;
}

/* 
 * Word code.
 *
 * For now we simply post-process the syntax tree produced by the
 * parser. We compile it into a struct eprog. Some day the parser
 * above should be changed to emit the word code directly.
 *
 * Word code layout:
 *
 *   WC_END
 *     - end of program code
 *
 *   WC_LIST
 *     - data contains type (sync, ...)
 *     - follwed by code for this list
 *     - if not (type & Z_END), followed by next WC_LIST
 *
 *   WC_SUBLIST
 *     - data contains type (&&, ||, END) and flags (coprog, not)
 *     - followed by code for sublist
 *     - if not (type == END), followed by next WC_SUBLIST
 *
 *   WC_PIPE
 *     - data contains type (end, mid) and LINENO
 *     - if not (type == END), followed by offset to next WC_PIPE
 *     - followed by command
 *     - if not (type == END), followed by next WC_PIPE
 *
 *   WC_REDIR
 *     - must precede command-code (or WC_ASSIGN)
 *     - data contains type (<, >, ...)
 *     - followed by fd1 and name from struct redir
 *
 *   WC_ASSIGN
 *     - data contains type (scalar, array) and number of array-elements
 *     - followed by name and value
 *
 *   WC_SIMPLE
 *     - data contains the number of arguments (plus command)
 *     - followed by strings
 *
 *   WC_SUBSH
 *     - data unused
 *     - followed by list
 *
 *   WC_CURSH
 *     - data unused
 *     - followed by list
 *
 *   WC_TIMED
 *     - data contains type (followed by pipe or not)
 *     - if (type == PIPE), followed by pipe
 *
 *   WC_FUNCDEF
 *     - data contains offset to after body-strings
 *     - followed by number of names
 *     - followed by names
 *     - followed by number of codes for body
 *     - followed by number of patterns for body
 *     - follwoed by codes for body
 *     - followed by strings for body
 *
 *   WC_FOR
 *     - data contains type (list, ...) and offset to after body
 *     - if (type == COND), followed by init, cond, advance expressions
 *     - else if (type == PPARAM), followed by param name
 *     - else if (type == LIST), followed by param name, num strings, strings
 *     - followed by body
 *
 *   WC_SELECT
 *     - data contains type (list, ...) and offset to after body
 *     - if (type == PPARAM), followed by param name
 *     - else if (type == LIST), followed by param name, num strings, strings
 *     - followed by body
 *
 *   WC_WHILE
 *     - data contains type (while, until) and ofsset to after body
 *     - followed by condition
 *     - followed by body
 *
 *   WC_REPEAT
 *     - data contains offset to after body
 *     - followed by number-string
 *     - followed by body
 *
 *   WC_CASE
 *     - first CASE is always of type HEAD, data contains offset to esac
 *     - after that CASEs of type OR (;;) and AND (;&), data is offset to
 *       next case
 *     - each OR/AND case is followed by pattern, pattern-number, list
 *
 *   WC_IF
 *     - first IF is of type HEAD, data contains offset to fi
 *     - after that IFs of type IF, ELIF, ELSE, data is offset to next
 *     - each non-HEAD is followed by condition (only IF, ELIF) and body
 *
 *   WC_COND
 *     - data contains type
 *     - if (type == AND/OR), data contains offset to after this one,
 *       followed by two CONDs
 *     - else if (type == NOT), followed by COND
 *     - else if (type == MOD), followed by name and strings
 *     - else if (type == MODI), followed by name, left, right
 *     - else if (type == STR[N]EQ), followed by left, right, pattern-number
 *     - else if (has two args) followed by left, right
 *     - else followed by string
 *
 *   WC_ARITH
 *     - followed by string (there's only one)
 *
 *   WC_AUTOFN
 *     - only used by the autoload builtin
 *
 * In each of the above, strings are encoded as one word code. For empty
 * strings this is the bit pattern 0xfe000000. For short strings (one to
 * three characters), this is the marker 0xff000000 with the lower three
 * bytes containing the characters. Longer strings are encoded as the
 * offset into the strs character array stored in the eprog struct.
 * The ecstr() function that adds the code for a string uses a simple
 * list of strings already added so that long strings are encoded only
 * once.
 *
 * Note also that in the eprog struct the pattern, code, and string
 * arrays all point to the same memory block.
 */

static int eclen, ecused, ecfree, ecnpats;
static Wordcode ecbuf;

typedef struct eccstr *Eccstr;

struct eccstr {
    Eccstr next;
    char *str;
    wordcode offs;
};

static Eccstr ecstrs;
static int ecsoffs;

/* Make at least n bytes free (aligned to sizeof(wordcode)). */

static int
ecspace(int n)
{
    n = (n + sizeof(wordcode) - 1) / sizeof(wordcode);

    if (ecfree < n) {
	int a = (n > 256 ? n : 256);

	ecbuf = (Wordcode) hrealloc((char *) ecbuf, eclen * sizeof(wordcode),
				    (eclen + a) * sizeof(wordcode));
	eclen += a;
	ecfree += a;
    }
    ecused += n;
    ecfree -= n;

    return ecused - 1;
}

/* Add one wordcode. */

static int
ecadd(wordcode c)
{
    if (ecfree < 1) {
	ecbuf = (Wordcode) hrealloc((char *) ecbuf, eclen * sizeof(wordcode),
				    (eclen + 256) * sizeof(wordcode));
	eclen += 256;
	ecfree += 256;
    }
    ecbuf[ecused] = c;
    ecused++;
    ecfree--;

    return ecused - 1;
}

/* Add a string and the wordcode for it. */

static int
ecstr(char *s)
{
    int l;

    if ((l = strlen(s) + 1) && l <= 4) {
	wordcode c = 0xff000000;
	switch (l) {
	case 4: c |= ((wordcode) STOUC(s[2])) << 16;
	case 3: c |= ((wordcode) STOUC(s[1])) <<  8;
	case 2: c |= ((wordcode) STOUC(s[0])); break;
	case 1: c = 0xfe000000;   break;
	}
	return ecadd(c);
    } else {
	Eccstr p, q = NULL;

	for (p = ecstrs; p; q = p, p = p->next)
	    if (!strcmp(s, p->str))
		return ecadd(p->offs);

	p = (Eccstr) zhalloc(sizeof(*p));
	p->next = NULL;
	if (q)
	    q->next = p;
	else
	    ecstrs = p;
	p->offs = ecsoffs;
	p->str = s;
	ecsoffs += l;

	return ecadd(p->offs);
    }
}

#define ec(N) ecomp((struct node *) (N))
#define ecsave(N) \
  do { int u = ecused; ec(N); if (u == ecused) ecadd(WCB_END()); } while (0)

#define _Cond(X) ((Cond) (X))
#define _Cmd(X) ((Cmd) (X))
#define _Pline(X) ((Pline) (X))
#define _Sublist(X) ((Sublist) (X))
#define _List(X) ((List) (X))
#define _casecmd(X) ((struct casecmd *) (X))
#define _ifcmd(X) ((struct ifcmd *) (X))
#define _whilecmd(X) ((struct whilecmd *) (X))

#define cont(N) do { n = (struct node *) (N); goto rec; } while (0)

/* Compile a node. */

static void
ecomp(struct node *n)
{
    int p, c;

 rec:

    if (!n || ((List) n) == &dummy_list)
	return;

    switch (NT_TYPE(n->ntype)) {
    case N_LIST:
	ecadd(WCB_LIST(_List(n)->type | (_List(n)->right ? 0 : Z_END)));
	if (_List(n)->right) {
	    ec(_List(n)->left);
	    cont(_List(n)->right);
	} else
	    cont(_List(n)->left);
	break;
    case N_SUBLIST:
	p = ecadd(0);
	ec(_Sublist(n)->left);
	ecbuf[p] = WCB_SUBLIST((_Sublist(n)->right ?
				((_Sublist(n)->type == ORNEXT) ?
				 WC_SUBLIST_OR : WC_SUBLIST_AND) :
				WC_SUBLIST_END),
			       (((_Sublist(n)->flags & PFLAG_NOT) ?
				 WC_SUBLIST_NOT : 0) |
				((_Sublist(n)->flags & PFLAG_COPROC) ?
				 WC_SUBLIST_COPROC : 0)),
			       (ecused - 1 - p));
	if (_Sublist(n)->right)
	    cont(_Sublist(n)->right);
	break;
    case N_PLINE:
	ecadd(WCB_PIPE((_Pline(n)->right ? WC_PIPE_MID : WC_PIPE_END),
		       (_Cmd(_Pline(n)->left)->lineno >= 0 ?
			_Cmd(_Pline(n)->left)->lineno + 1 : 0)));
	if (_Pline(n)->right) {
	    p = ecadd(0);
	    ec(_Pline(n)->left);
	    ecbuf[p] = (wordcode) (ecused - p);
	    cont(_Pline(n)->right);
	} else
	    cont(_Pline(n)->left);
	break;
    case N_CMD:
	{
	    Cmd nn = _Cmd(n);

	    /* Note that the execution and text code require that the
	     * redirs and assignments are in exactly this order and that
	     * they are before the command. */

	    ecredirs(nn->redir);

	    switch (nn->type) {
	    case SIMPLE:
		{
		    int num = 0;

		    ecassigns(nn->vars);
		    p = ecadd(0);

		    if (nn->args) {
			LinkNode ap;

			for (ap = firstnode(nn->args); ap;
			     incnode(ap), num++)
			    ecstr((char *) getdata(ap));
		    }
		    ecbuf[p] = WCB_SIMPLE(num);
		}
		break;
	    case SUBSH:
		ecadd(WCB_SUBSH());
		ecsave(nn->u.list);
		break;
	    case ZCTIME:
		ecadd(WCB_TIMED(nn->u.pline ? WC_TIMED_PIPE : WC_TIMED_EMPTY));
		if (nn->u.pline)
		    ec(nn->u.pline);
		break;
	    case FUNCDEF:
		{
		    LinkNode np;
		    int num, sbeg, onp;
		    Eccstr ostrs;

		    /* Defined functions and their strings are stored
		     * inline. */

		    p = ecadd(0);
		    ecadd(0);

		    for (np = firstnode(nn->args), num = 0; np;
			 incnode(np), num++)
			ecstr((char *) getdata(np));

		    ecadd(0);
		    ecadd(0);

		    sbeg = ecsoffs;
		    ecsoffs = 0;
		    ostrs = ecstrs;
		    ecstrs = NULL;
		    onp = ecnpats;
		    ecnpats = 0;

		    ecsave(nn->u.list);

		    ecbuf[p + num + 2] = ecused - num - p;
		    ecbuf[p + num + 3] = ecnpats;
		    ecbuf[p + 1] = num;

		    if (ecsoffs) {
			int beg = ecused, l;
			Eccstr sp;
			char *sq;

			ecspace(ecsoffs);

			for (sp = ecstrs, sq = (char *) (ecbuf + beg); sp;
			     sp = sp->next, sq += l) {
			    l = strlen(sp->str) + 1;
			    memcpy(sq, sp->str, l);
			}
		    }
		    ecsoffs = sbeg;
		    ecstrs = ostrs;
		    ecnpats = onp;

		    ecbuf[p] = WCB_FUNCDEF(ecused - 1 - p);
		}
		break;
	    case CURSH:
		ecadd(WCB_CURSH());
		ecsave(nn->u.list);
		break;
	    case CFOR:
		{
		    int type;

		    p = ecadd(0);
		    ecstr(nn->u.forcmd->name);

		    if (nn->u.forcmd->condition) {
			type = WC_FOR_COND;
			ecstr(nn->u.forcmd->condition);
			ecstr(nn->u.forcmd->advance);
		    } else {
			if (nn->args) {
			    LinkNode fp;
			    int num;

			    type = WC_FOR_LIST;

			    ecadd(0);

			    for (fp = firstnode(nn->args), num = 0; fp;
				 incnode(fp), num++)
				ecstr((char *) getdata(fp));

			    ecbuf[p + 2] = num;
			} else
			    type = WC_FOR_PPARAM;
		    }
		    ecsave(nn->u.forcmd->list);

		    ecbuf[p] = WCB_FOR(type, ecused - 1 - p);
		}
		break;
	    case CSELECT:
		{
		    int type;

		    p = ecadd(0);
		    ecstr(nn->u.forcmd->name);

		    if (nn->args) {
			LinkNode fp;
			int num;

			type = WC_SELECT_LIST;
			ecadd(0);

			for (fp = firstnode(nn->args), num = 0; fp;
			     incnode(fp), num++)
			    ecstr((char *) getdata(fp));

			ecbuf[p + 2] = num;
		    } else
			type = WC_SELECT_PPARAM;

		    ecsave(nn->u.forcmd->list);

		    ecbuf[p] = WCB_SELECT(type, ecused - 1 - p);
		}
		break;
	    case CIF:
		{
		    List *i, *t;
		    int type = WC_IF_IF;

		    c = ecadd(0);

		    for (i = nn->u.ifcmd->ifls, t = nn->u.ifcmd->thenls;
			 *i; i++, t++) {
			p = ecadd(0);
			ecsave(*i);
			ecsave(*t);
			ecbuf[p] = WCB_IF(type, ecused - 1 - p);
			type = WC_IF_ELIF;
		    }
		    if (*t) {
			p = ecadd(0);
			ecsave(*t);
			ecbuf[p] = WCB_IF(WC_IF_ELSE, ecused - 1 - p);
		    }
		    ecbuf[c] = WCB_IF(WC_IF_HEAD, ecused - 1 - c);
		}
		break;
	    case CCASE:
		{
		    List *l;
		    char **pp = nn->u.casecmd->pats;

		    p = ecadd(0);
		    ecstr(*pp++);

		    for (l = nn->u.casecmd->lists; l && *l; l++, pp++) {
			c = ecadd(0);
			ecstr(*pp + 1);
			ecadd(ecnpats++);
			ecsave(*l);
			ecbuf[c] = WCB_CASE((**pp == ';' ?
					     WC_CASE_OR : WC_CASE_AND),
					    ecused - 1 - c);
		    }
		    ecbuf[p] = WCB_CASE(WC_CASE_HEAD, ecused - 1 - p);
		}
		break;
	    case COND:
		eccond(nn->u.cond);
		break;
	    case CARITH:
		ecadd(WCB_ARITH());
		ecstr((char *) getdata(firstnode(nn->args)));
		break;
	    case CREPEAT:
		p = ecadd(0);
		ecstr((char *) getdata(firstnode(nn->args)));
		ecsave(nn->u.list);
		ecbuf[p] = WCB_REPEAT(ecused - 1 - p);
		break;
	    case CWHILE:
		p = ecadd(0);
		ecsave(nn->u.whilecmd->cont);
		ecsave(nn->u.whilecmd->loop);
		ecbuf[p] = WCB_WHILE((nn->u.whilecmd->cond ?
				      WC_WHILE_UNTIL : WC_WHILE_WHILE),
				     ecused - 1 - p);
		break;
	    }
	}
	break;
    case N_COND:
	eccond((Cond) n);
	break;
#ifdef DEBUG
    default:
	dputs("BUG: node type not handled in ecomp().");
	break;
#endif
    }
}

/**/
static void
ecredirs(LinkList l)
{
    LinkNode n;
    Redir f;

    if (!l)
	return;

    for (n = firstnode(l); n; incnode(n)) {
	f = (Redir) getdata(n);

	ecadd(WCB_REDIR(f->type));
	ecadd(f->fd1);
	ecstr(f->name);
    }
}

/**/
static void
ecassigns(LinkList l)
{
    int p;
    LinkNode n;
    Varasg v;

    if (!l)
	return;

    for (n = firstnode(l); n; incnode(n)) {
	v = (Varasg) getdata(n);

	p = ecadd(0);
	ecstr(v->name);

	if (PM_TYPE(v->type) == PM_ARRAY) {
	    LinkNode vp;
	    int num;

	    for (vp = firstnode(v->arr), num = 0; vp; incnode(vp), num++)
		ecstr((char *) getdata(vp));
	    ecbuf[p] = WCB_ASSIGN(WC_ASSIGN_ARRAY, num);
	} else {
	    ecstr(v->str);
	    ecbuf[p] = WCB_ASSIGN(WC_ASSIGN_SCALAR, 0);
	}
    }
}

/**/
static void
eccond(Cond c)
{
    int p;

    switch (c->type) {
    case COND_NOT:
	ecadd(WCB_COND(COND_NOT, 0));
	eccond(c->left);
	break;
    case COND_AND:
    case COND_OR:
	p = ecadd(0);
	eccond(c->left);
	eccond(c->right);
	ecbuf[p] = WCB_COND(c->type, ecused - 1 - p);
	break;
    case COND_MOD:
	{
	    char **pp;
	    int num;

	    p = ecadd(0);
	    ecstr((char *) c->left);
	    for (pp = (char **) c->right, num = 0; *pp; pp++, num++)
		ecstr(*pp);
	    ecbuf[p] = WCB_COND(COND_MOD, num);
	}
	break;
    case COND_MODI:
	ecadd(WCB_COND(COND_MODI, 0));
	ecstr((char *) c->left);
	ecstr(((char **) c->right)[0]);
	ecstr(((char **) c->right)[1]);
	break;
    default:
	ecadd(WCB_COND(c->type, 0));
	ecstr((char *) c->left);
	if (c->type <= COND_GE) {
	    ecstr((char *) c->right);
	    if (c->type == COND_STREQ || c->type == COND_STRNEQ)
		ecadd(ecnpats++);
	}
	break;
    }
}

/**/
static Eprog
execompile(List list)
{
    Eprog ret;
    Eccstr p;
    char *q;
    int l;

    MUSTUSEHEAP("execompile");

    ecbuf = (Wordcode) zhalloc((eclen = ecfree = 256) * sizeof(wordcode));
    ecused = 0;
    ecstrs = NULL;
    ecsoffs = ecnpats = 0;

    ec(list);
    ecadd(WCB_END());

    ret = (Eprog) zhalloc(sizeof(*ret));
    ret->len = ((ecnpats * sizeof(Patprog)) +
		(ecused * sizeof(wordcode)) +
		ecsoffs);
    ret->npats = ecnpats;
    ret->pats = (Patprog *) zhalloc(ret->len);
    ret->prog = (Wordcode) (ret->pats + ecnpats);
    ret->strs = (char *) (ret->prog + ecused);
    ret->shf = NULL;
    ret->heap = 1;
    for (l = 0; l < ecnpats; l++)
	ret->pats[l] = dummy_patprog1;
    memcpy(ret->prog, ecbuf, ecused * sizeof(wordcode));
    for (p = ecstrs, q = ret->strs; p; p = p->next, q += l) {
	l = strlen(p->str) + 1;
	memcpy(q, p->str, l);
    }
    return ret;
}

/**/
mod_export Eprog
dupeprog(Eprog p)
{
    Eprog r;
    int i;
    Patprog *pp;

    if (p == &dummy_eprog)
	return p;

    r = (Eprog) ncalloc(sizeof(*r));
    r->heap = useheap;
    r->len = p->len;
    r->npats = p->npats;
    pp = r->pats = (Patprog *) ncalloc(r->len);
    r->prog = (Wordcode) (r->pats + r->npats);
    r->strs = ((char *) r->prog) + (p->strs - ((char *) p->prog));
    memcpy(r->prog, p->prog, r->len - (p->npats * sizeof(Patprog)));

    for (i = r->npats; i--; pp++)
	*pp = dummy_patprog1;

    return r;
}

static LinkList eprog_free;

/**/
mod_export void
freeeprog(Eprog p)
{
    if (p && p != &dummy_eprog) {
	PERMALLOC {
	    addlinknode(eprog_free, p);
	} LASTALLOC;
    }
}

/**/
void
freeeprogs(void)
{
    Eprog p;
    int i;
    Patprog *pp;

    while ((p = (Eprog) getlinknode(eprog_free))) {
	for (i = p->npats, pp = p->pats; i--; pp++)
	    freepatprog(*pp);
	zfree(p->pats, p->len);
	zfree(p, sizeof(*p));
    }
}

/**/
char *
ecgetstr(Estate s, int dup)
{
    static char buf[4];
    wordcode c = *s->pc++;
    char *r;

    if (c == 0xfe000000)
	r = "";
    else if (c >= 0xff000000) {
	buf[0] = (char) (c & 0xff);
	buf[1] = (char) ((c >>  8) & 0xff);
	buf[2] = (char) ((c >> 16) & 0xff);
	buf[3] = '\0';
	r = dupstring(buf);
	dup = 0;
    } else
	r = s->strs + c;

    return (dup ? dupstring(r) : r);
}

/**/
char *
ecrawstr(Eprog p, Wordcode pc)
{
    static char buf[4];
    wordcode c = *pc;

    if (c == 0xfe000000)
	return "";
    else if (c >= 0xff000000) {
	buf[0] = (char) (c & 0xff);
	buf[1] = (char) ((c >>  8) & 0xff);
	buf[2] = (char) ((c >> 16) & 0xff);
	buf[3] = '\0';
	return buf;
    } else
	return p->strs + c;
}

/**/
char **
ecgetarr(Estate s, int num, int dup)
{
    char **ret, **rp;

    ret = rp = (char **) zhalloc((num + 1) * sizeof(char *));

    while (num--)
	*rp++ = ecgetstr(s, dup);
    *rp = NULL;

    return ret;
}

/**/
LinkList
ecgetlist(Estate s, int num, int dup)
{
    if (num) {
	LinkList ret;

	ret = newlinklist();

	while (num--)
	    addlinknode(ret, ecgetstr(s, dup));

	return ret;
    }
    return NULL;
}

/**/
LinkList
ecgetredirs(Estate s)
{
    LinkList ret = newlinklist();
    wordcode code = *s->pc++;

    while (wc_code(code) == WC_REDIR) {
	Redir r = (Redir) zhalloc(sizeof(*r));

	r->type = WC_REDIR_TYPE(code);
	r->fd1 = *s->pc++;
	r->name = ecgetstr(s, 1);

	addlinknode(ret, r);

	code = *s->pc++;
    }
    s->pc--;

    return ret;
}

/**/
mod_export struct eprog dummy_eprog;

static wordcode dummy_eprog_code;

/**/
void
init_eprog(void)
{
    dummy_eprog_code = WCB_END();
    dummy_eprog.len = sizeof(wordcode);
    dummy_eprog.prog = &dummy_eprog_code;
    dummy_eprog.strs = NULL;

    PERMALLOC {
	eprog_free = newlinklist();
    } LASTALLOC;
}
