/*
 * math.c - mathematical expression evaluation
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
#include "math.pro"

/* nonzero means we are not evaluating, just parsing */
 
/**/
int noeval;
 
/* last input base we used */

/**/
int lastbase;
 
static char *ptr;

static long yyval;
static LV yylval;

static int mlevel = 0;

/* != 0 means recognize unary plus, minus, etc. */

static int unary = 1;

/* LR = left-to-right associativity *
 * RL = right-to-left associativity *
 * BOO = short-circuiting boolean   */

#define LR 0
#define RL 1
#define BOOL 2

#define M_INPAR 0
#define M_OUTPAR 1
#define NOT 2
#define COMP 3
#define POSTPLUS 4
#define POSTMINUS 5
#define UPLUS 6
#define UMINUS 7
#define AND 8
#define XOR 9
#define OR 10
#define MUL 11
#define DIV 12
#define MOD 13
#define PLUS 14
#define MINUS 15
#define SHLEFT 16
#define SHRIGHT 17
#define LES 18
#define LEQ 19
#define GRE 20
#define GEQ 21
#define DEQ 22
#define NEQ 23
#define DAND 24
#define DOR 25
#define DXOR 26
#define QUEST 27
#define COLON 28
#define EQ 29
#define PLUSEQ 30
#define MINUSEQ 31
#define MULEQ 32
#define DIVEQ 33
#define MODEQ 34
#define ANDEQ 35
#define XOREQ 36
#define OREQ 37
#define SHLEFTEQ 38
#define SHRIGHTEQ 39
#define DANDEQ 40
#define DOREQ 41
#define DXOREQ 42
#define COMMA 43
#define EOI 44
#define PREPLUS 45
#define PREMINUS 46
#define NUM 47
#define ID 48
#define POWER 49
#define CID 50
#define POWEREQ 51
#define TOKCOUNT 52

/* precedences */

static int prec[TOKCOUNT] =
{
     1, 137,  2,  2,   2,
     2,   2,  2,  4,   5,
     6,   8,  8,  8,   9,
     9,   3,  3, 10,  10,
    10,  10, 11, 11,  12,
    13,  13, 14, 14,  15,
    15,  15, 15, 15,  15,
    15,  15, 15, 15,  15,
    15,  15, 15, 16, 200,
     2,   2,  0,  0,   7,
     0,  15
};

#define TOPPREC 16
#define ARGPREC (TOPPREC-1)

static int type[TOKCOUNT] =
{
    LR, LR, RL, RL, RL,
    RL, RL, RL, LR, LR,
    LR, LR, LR, LR, LR,
    LR, LR, LR, LR, LR,
    LR, LR, LR, LR, BOOL,
    BOOL, LR, RL, RL, RL,
    RL, RL, RL, RL, RL,
    RL, RL, RL, RL, RL,
    BOOL, BOOL, RL, RL, RL,
    RL, RL, LR, LR, RL,
    LR, RL
};

#define LVCOUNT 32

/* list of lvalues (variables) */

static int lvc;
static char **lvals;


/**/
static int
zzlex(void)
{
    int cct = 0;

    for (;; cct = 0)
	switch (*ptr++) {
	case '+':
	    if (*ptr == '+' && (unary || !ialnum(*ptr))) {
		ptr++;
		return (unary) ? PREPLUS : POSTPLUS;
	    }
	    if (*ptr == '=') {
		unary = 1;
		ptr++;
		return PLUSEQ;
	    }
	    return (unary) ? UPLUS : PLUS;
	case '-':
	    if (*ptr == '-' && (unary || !ialnum(*ptr))) {
		ptr++;
		return (unary) ? PREMINUS : POSTMINUS;
	    }
	    if (*ptr == '=') {
		unary = 1;
		ptr++;
		return MINUSEQ;
	    }
	    return (unary) ? UMINUS : MINUS;
	case '(':
	    unary = 1;
	    return M_INPAR;
	case ')':
	    return M_OUTPAR;
	case '!':
	    if (*ptr == '=') {
		unary = 1;
		ptr++;
		return NEQ;
	    }
	    return NOT;
	case '~':
	    return COMP;
	case '&':
	    unary = 1;
	    if (*ptr == '&') {
		if (*++ptr == '=') {
		    ptr++;
		    return DANDEQ;
		}
		return DAND;
	    } else if (*ptr == '=') {
		ptr++;
		return ANDEQ;
	    }
	    return AND;
	case '|':
	    unary = 1;
	    if (*ptr == '|') {
		if (*++ptr == '=') {
		    ptr++;
		    return DOREQ;
		}
		return DOR;
	    } else if (*ptr == '=') {
		ptr++;
		return OREQ;
	    }
	    return OR;
	case '^':
	    unary = 1;
	    if (*ptr == '^') {
		if (*++ptr == '=') {
		    ptr++;
		    return DXOREQ;
		}
		return DXOR;
	    } else if (*ptr == '=') {
		ptr++;
		return XOREQ;
	    }
	    return XOR;
	case '*':
	    unary = 1;
	    if (*ptr == '*') {
		if (*++ptr == '=') {
		    ptr++;
		    return POWEREQ;
		}
		return POWER;
	    }
	    if (*ptr == '=') {
		ptr++;
		return MULEQ;
	    }
	    return MUL;
	case '/':
	    unary = 1;
	    if (*ptr == '=') {
		ptr++;
		return DIVEQ;
	    }
	    return DIV;
	case '%':
	    unary = 1;
	    if (*ptr == '=') {
		ptr++;
		return MODEQ;
	    }
	    return MOD;
	case '<':
	    unary = 1;
	    if (*ptr == '<') {
		if (*++ptr == '=') {
		    ptr++;
		    return SHLEFTEQ;
		}
		return SHLEFT;
	    } else if (*ptr == '=') {
		ptr++;
		return LEQ;
	    }
	    return LES;
	case '>':
	    unary = 1;
	    if (*ptr == '>') {
		if (*++ptr == '=') {
		    ptr++;
		    return SHRIGHTEQ;
		}
		return SHRIGHT;
	    } else if (*ptr == '=') {
		ptr++;
		return GEQ;
	    }
	    return GRE;
	case '=':
	    unary = 1;
	    if (*ptr == '=') {
		ptr++;
		return DEQ;
	    }
	    return EQ;
	case '$':
	    unary = 0;
	    yyval = mypid;
	    return NUM;
	case '?':
	    if (unary) {
		yyval = lastval;
		unary = 0;
		return NUM;
	    }
	    unary = 1;
	    return QUEST;
	case ':':
	    unary = 1;
	    return COLON;
	case ',':
	    unary = 1;
	    return COMMA;
	case '\0':
	    unary = 1;
	    ptr--;
	    return EOI;
	case '[':
	    unary = 0;
	    {
		int base = zstrtol(ptr, &ptr, 10);

		if (*ptr == ']')
		    ptr++;
		yyval = zstrtol(ptr, &ptr, lastbase = base);
		return NUM;
	    }
	case ' ':
	case '\t':
	case '\n':
	    break;
	case '0':
	    if (*ptr == 'x' || *ptr == 'X') {
		unary = 0;
		/* Should we set lastbase here? */
		yyval = zstrtol(++ptr, &ptr, lastbase = 16);
		return NUM;
	    }
	/* Fall through! */
	default:
	    if (idigit(*--ptr)) {
		unary = 0;
		yyval = zstrtol(ptr, &ptr, 10);

		if (*ptr == '#') {
		    ptr++;
		    yyval = zstrtol(ptr, &ptr, lastbase = yyval);
		}
		return NUM;
	    }
	    if (*ptr == '#') {
		if (*++ptr == '\\') {
		    ptr++;
		    yyval = *ptr == Meta ? *++ptr ^ 32 : *ptr;
		    ptr++;
		    unary = 0;
		    return NUM;
		}
		cct = 1;
	    }
	    if (iident(*ptr)) {
		char *p, q;

		p = ptr;
		if (lvc == LVCOUNT) {
		    zerr("too many identifiers (complain to author)", NULL, 0);
		    return EOI;
		}
		unary = 0;
		while (iident(*++ptr));
		if (*ptr == '[') {
		    int l;
		    for (ptr++, l = 1; *ptr && l; ptr++) {
			if (*ptr == '[')
			    l++;
			if (*ptr == ']')
			    l--;
			if (*ptr == '\\' && ptr[1])
			    ptr++;
		    }
		}
		q = *ptr;
		*ptr = '\0';
		lvals[yylval = lvc++] = ztrdup(p);
		*ptr = q;
		return cct ? CID : ID;
	    }
	    else if (cct) {
		yyval = poundgetfn(NULL);
		unary = 0;
		return NUM;
	    }
	    return EOI;
	}
}

/* the value stack */

#define STACKSZ 100
static int mtok;			/* last token */
static int sp = -1;			/* stack pointer */

struct mathvalue {
    LV lval;
    long val;
};

static struct mathvalue *stack;

/**/
static void
push(long val, LV lval)
{
    if (sp == STACKSZ - 1)
	zerr("stack overflow", NULL, 0);
    else
	sp++;
    stack[sp].val = val;
    stack[sp].lval = lval;
}


/**/
static long
getcvar(LV s)
{
    char *t;

    if (!(t = getsparam(lvals[s])))
	return 0;
    return STOUC(*t == Meta ? t[1] ^ 32 : *t);
}


/**/
static long
setvar(LV s, long v)
{
    if (s == -1 || s >= lvc) {
	zerr("lvalue required", NULL, 0);
	return 0;
    }
    if (noeval)
	return v;
    setiparam(lvals[s], v);
    return v;
}


/**/
static int
notzero(long a)
{
    if (a == 0) {
	zerr("division by zero", NULL, 0);
	return 0;
    }
    return 1;
}

/* macro to pop two values off the value stack */
#define pop2() { \
	if (sp < 1) { \
 	    zerr("bad math expression: unbalanced stack", NULL, 0); \
	    return; \
	} \
	b = stack[sp--].val; \
	a = stack[sp--].val; \
    }

/* macro to pop three values off the value stack */
#define pop3() { \
	if (sp < 2) { \
 	    zerr("bad math expression: unbalanced stack", NULL, 0); \
	    return; \
	} \
	c = stack[sp--].val; \
	b = stack[sp--].val; \
	a = stack[sp--].val; \
    }

#define nolval() {stack[sp].lval= -1;}
#define pushv(X) { push(X,-1); }
#define pop2lv() { pop2() lv = stack[sp+1].lval; }
#define set(X) { push(setvar(lv,X),lv); }


/**/
void
op(int what)
{
    long a, b, c;
    LV lv;

    if (sp < 0) {
	zerr("bad math expression: stack empty", NULL, 0);
	return;
    }
    switch (what) {
    case NOT:
	stack[sp].val = !stack[sp].val;
	nolval();
	break;
    case COMP:
	stack[sp].val = ~stack[sp].val;
	nolval();
	break;
    case POSTPLUS:
	(void)setvar(stack[sp].lval, stack[sp].val + 1);
	break;
    case POSTMINUS:
	(void)setvar(stack[sp].lval, stack[sp].val - 1);
	break;
    case UPLUS:
	nolval();
	break;
    case UMINUS:
	stack[sp].val = -stack[sp].val;
	nolval();
	break;
    case AND:
	pop2();
	pushv(a & b);
	break;
    case XOR:
	pop2();
	pushv(a ^ b);
	break;
    case OR:
	pop2();
	pushv(a | b);
	break;
    case MUL:
	pop2();
	pushv(a * b);
	break;
    case DIV:
	pop2();
	if (notzero(b))
	    pushv(a / b);
	break;
    case MOD:
	pop2();
	if (notzero(b))
	    pushv(a % b);
	break;
    case PLUS:
	pop2();
	pushv(a + b);
	break;
    case MINUS:
	pop2();
	pushv(a - b);
	break;
    case SHLEFT:
	pop2();
	pushv(a << b);
	break;
    case SHRIGHT:
	pop2();
	pushv(a >> b);
	break;
    case LES:
	pop2();
	pushv((long)(a < b));
	break;
    case LEQ:
	pop2();
	pushv((long)(a <= b));
	break;
    case GRE:
	pop2();
	pushv((long)(a > b));
	break;
    case GEQ:
	pop2();
	pushv((long)(a >= b));
	break;
    case DEQ:
	pop2();
	pushv((long)(a == b));
	break;
    case NEQ:
	pop2();
	pushv((long)(a != b));
	break;
    case DAND:
	pop2();
	pushv((long)(a && b));
	break;
    case DOR:
	pop2();
	pushv((long)(a || b));
	break;
    case DXOR:
	pop2();
	pushv((long)((a && !b) || (!a && b)));
	break;
    case QUEST:
	pop3();
	pushv((a) ? b : c);
	break;
    case COLON:
	break;
    case EQ:
	pop2();
	lv = stack[sp + 1].lval;
	set(b);
	break;
    case PLUSEQ:
	pop2lv();
	set(a + b);
	break;
    case MINUSEQ:
	pop2lv();
	set(a - b);
	break;
    case MULEQ:
	pop2lv();
	set(a * b);
	break;
    case DIVEQ:
	pop2lv();
	if (notzero(b))
	    set(a / b);
	break;
    case MODEQ:
	pop2lv();
	if (notzero(b))
	    set(a % b);
	break;
    case ANDEQ:
	pop2lv();
	set(a & b);
	break;
    case XOREQ:
	pop2lv();
	set(a ^ b);
	break;
    case OREQ:
	pop2lv();
	set(a | b);
	break;
    case SHLEFTEQ:
	pop2lv();
	set(a << b);
	break;
    case SHRIGHTEQ:
	pop2lv();
	set(a >> b);
	break;
    case DANDEQ:
	pop2lv();
	set((long)(a && b));
	break;
    case DOREQ:
	pop2lv();
	set((long)(a || b));
	break;
    case DXOREQ:
	pop2lv();
	set((long)((a && !b) || (!a && b)));
	break;
    case COMMA:
	pop2();
	pushv(b);
	break;
    case PREPLUS:
	stack[sp].val = setvar(stack[sp].lval,
			       stack[sp].val + 1);
	break;
    case PREMINUS:
	stack[sp].val = setvar(stack[sp].lval,
			       stack[sp].val - 1);
	break;
    case POWER:
	pop2();
	if (b < 0) {
	    zerr("can't handle negative exponents", NULL, 0);
	    return;
	}
	for (c = 1; b--; c *= a);
	pushv(c);
	break;
    case POWEREQ:
	pop2lv();
	if (b < 0) {
	    zerr("can't handle negative exponents", NULL, 0);
	    return;
	}
	for (c = 1; b--; c *= a);
	set(c);
	break;
    default:
	zerr("out of integers", NULL, 0);
	return;
    }
}


/**/
static void
bop(int tk)
{
    switch (tk) {
    case DAND:
    case DANDEQ:
	if (!stack[sp].val)
	    noeval++;
	break;
    case DOR:
    case DOREQ:
	if (stack[sp].val)
	    noeval++;
	break;
    };
}


/**/
static long
mathevall(char *s, int prek, char **ep)
{
    int t0;
    int xlastbase, xnoeval, xunary, xlvc;
    char *xptr;
    long xyyval;
    LV xyylval;
    char **xlvals = 0;
    int xsp;
    struct mathvalue *xstack = 0;
    long ret;

    xlastbase = xnoeval = xunary = xlvc = xyyval = xyylval = xsp = 0;
    xptr = NULL;
    if (mlevel++) {
	xlastbase = lastbase;
	xnoeval = noeval;
	xunary = unary;
	xlvc = lvc;
	xptr = ptr;
	xyyval = yyval;
	xyylval = yylval;
	xlvals = lvals;

	xsp = sp;
	xstack = stack;
    }
    stack = (struct mathvalue *)zalloc(STACKSZ*sizeof(struct mathvalue));
    lastbase = -1;
    lvals = (char **)zcalloc(LVCOUNT*sizeof(char *));
    lvc = 0;
    ptr = s;
    sp = -1;
    unary = 1;
    mathparse(prek);
    *ep = ptr;
    if (sp)
	zerr("bad math expression: unbalanced stack", NULL, 0);
    for (t0 = 0; t0 != lvc; t0++)
	zsfree(lvals[t0]);

    ret = stack[0].val;

    zfree(lvals, LVCOUNT*sizeof(char *));
    zfree(stack, STACKSZ*sizeof(struct mathvalue));
    if (--mlevel) {
	lastbase = xlastbase;
	noeval = xnoeval;
	unary = xunary;
	lvc = xlvc;
	ptr = xptr;
	yyval = xyyval;
	yylval = xyylval;
	lvals = xlvals;

	sp = xsp;
	stack = xstack;
    }
    return ret;
}


/**/
long
matheval(char *s)
{
    char *junk;
    long x;
    int xmtok = mtok;

    if (!*s)
	return 0;
    x = mathevall(s, TOPPREC, &junk);
    mtok = xmtok;
    if (*junk)
	zerr("bad math expression: illegal character: %c", NULL, *junk);
    return x;
}


/**/
long
mathevalarg(char *s, char **ss)
{
    long x;
    int xmtok = mtok;

    x = mathevall(s, ARGPREC, ss);
    if (mtok == COMMA)
	(*ss)--;
    mtok = xmtok;
    return x;
}


/* operator-precedence parse the string and execute */

/**/
static void
mathparse(int pc)
{
    int q, otok, onoeval;

    if (errflag)
	return;
    mtok = zzlex();
    while (prec[mtok] <= pc) {
	if (errflag)
	    return;
	switch (mtok) {
	case NUM:
	    push(yyval, -1);
	    break;
	case ID:
	    push(getiparam(lvals[yylval]), yylval);
	    break;
	case CID:
	    push(getcvar(yylval), yylval);
	    break;
	case M_INPAR:
	    mathparse(TOPPREC);
	    if (mtok != M_OUTPAR) {
		if (!errflag)
		    zerr("')' expected", NULL, 0);
		return;
	    }
	    break;
	case QUEST:
	    q = stack[sp].val;

	    if (!q)
		noeval++;
	    mathparse(prec[QUEST] - 1);
	    if (!q)
		noeval--;
	    else
		noeval++;
	    mathparse(prec[QUEST]);
	    if (q)
		noeval--;
	    op(QUEST);
	    continue;
	default:
	    otok = mtok;
	    onoeval = noeval;
	    if (type[otok] == BOOL)
		bop(otok);
	    mathparse(prec[otok] - (type[otok] != RL));
	    noeval = onoeval;
	    op(otok);
	    continue;
	}
	mtok = zzlex();
    }
}
