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

#include <math.h>

/* nonzero means we are not evaluating, just parsing */
 
/**/
int noeval;
 
/* last input base we used */

/**/
int lastbase;
 
static char *ptr;

static mnumber yyval;
static LV yylval;

static int mlevel = 0;

/* != 0 means recognize unary plus, minus, etc. */

static int unary = 1;

/* LR = left-to-right associativity *
 * RL = right-to-left associativity *
 * BOOL = short-circuiting boolean   */

#define LR   0x0000
#define RL   0x0001
#define BOOL 0x0002

#define MTYPE(x)  ((x) & 3)

/*
 * OP_A2    2 arguments
 * OP_A2IR  2 arguments, return integer
 * OP_A2IO  2 arguments, must be integer, return integer
 * OP_E2    2 arguments with assignment
 * OP_E2IO  2 arguments with assignment, must be integer, return integer
 * OP_OP    None of the above, but occurs where we are expecting an operator
 *          rather than an operand.
 * OP_OPF   Followed by an operator, not an operand.
 *
 * OP_A2*, OP_E2*, OP_OP*:
 *   Occur when we need an operator; the next object must be an operand,
 *   unless OP_OPF is also supplied.
 *
 * Others:
 *   Occur when we need an operand; the next object must also be an operand,
 *   unless OP_OPF is also supplied.
 */
#define OP_A2   0x0004
#define OP_A2IR 0x0008
#define OP_A2IO 0x0010
#define OP_E2   0x0020
#define OP_E2IO 0x0040
#define OP_OP   0x0080
#define OP_OPF  0x0100

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
/*  0 */  LR, LR|OP_OP|OP_OPF, RL, RL, RL|OP_OP|OP_OPF,
/*  5 */  RL|OP_OP|OP_OPF, RL, RL, LR|OP_A2IO, LR|OP_A2IO,
/* 10 */  LR|OP_A2IO, LR|OP_A2, LR|OP_A2, LR|OP_A2IO, LR|OP_A2,
/* 15 */  LR|OP_A2, LR|OP_A2IO, LR|OP_A2IO, LR|OP_A2IR, LR|OP_A2IR,
/* 20 */  LR|OP_A2IR, LR|OP_A2IR, LR|OP_A2IR, LR|OP_A2IR, BOOL|OP_A2IO,
/* 25 */  BOOL|OP_A2IO, LR|OP_A2IO, RL|OP_OP, RL|OP_OP, RL|OP_E2,
/* 30 */  RL|OP_E2, RL|OP_E2, RL|OP_E2, RL|OP_E2, RL|OP_E2IO,
/* 35 */  RL|OP_E2IO, RL|OP_E2IO, RL|OP_E2IO, RL|OP_E2IO, RL|OP_E2IO,
/* 40 */  BOOL|OP_E2IO, BOOL|OP_E2IO, RL|OP_A2IO, RL|OP_A2, RL|OP_OP,
/* 45 */  RL, RL, LR|OP_OPF, LR|OP_OPF, RL|OP_A2,
/* 50 */  LR|OP_OPF, RL|OP_E2
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

    yyval.type = MN_INTEGER;
    for (;; cct = 0)
	switch (*ptr++) {
	case '+':
	    if (*ptr == '+' && (unary || !ialnum(*ptr))) {
		ptr++;
		return (unary) ? PREPLUS : POSTPLUS;
	    }
	    if (*ptr == '=') {
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
		ptr++;
		return MINUSEQ;
	    }
	    return (unary) ? UMINUS : MINUS;
	case '(':
	    return M_INPAR;
	case ')':
	    return M_OUTPAR;
	case '!':
	    if (*ptr == '=') {
		ptr++;
		return NEQ;
	    }
	    return NOT;
	case '~':
	    return COMP;
	case '&':
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
	    if (*ptr == '=') {
		ptr++;
		return DIVEQ;
	    }
	    return DIV;
	case '%':
	    if (*ptr == '=') {
		ptr++;
		return MODEQ;
	    }
	    return MOD;
	case '<':
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
	    if (*ptr == '=') {
		ptr++;
		return DEQ;
	    }
	    return EQ;
	case '$':
	    yyval.u.l = mypid;
	    return NUM;
	case '?':
	    if (unary) {
		yyval.u.l = lastval;
		unary = 0;
		return NUM;
	    }
	    return QUEST;
	case ':':
	    return COLON;
	case ',':
	    return COMMA;
	case '\0':
	    ptr--;
	    return EOI;
	case '[':
	    {
		int base = zstrtol(ptr, &ptr, 10);

		if (*ptr == ']')
		    ptr++;
		yyval.u.l = zstrtol(ptr, &ptr, lastbase = base);
		return NUM;
	    }
	case ' ':
	case '\t':
	case '\n':
	    break;
	case '0':
	    if (*ptr == 'x' || *ptr == 'X') {
		/* Should we set lastbase here? */
		yyval.u.l = zstrtol(++ptr, &ptr, lastbase = 16);
		return NUM;
	    }
	/* Fall through! */
	default:
	    if (idigit(*--ptr) || *ptr == '.') {
		char *nptr;
		for (nptr = ptr; idigit(*nptr); nptr++);

		if (*nptr == '.' || *nptr == 'e' || *nptr == 'E') {
		    /* it's a float */
		    yyval.type = MN_FLOAT;
		    yyval.u.d = strtod(ptr, &nptr);
		    if (ptr == nptr || *nptr == '.') {
			zerr("bad floating point constant", NULL, 0);
			return EOI;
		    }
		    ptr = nptr;
		} else {
		    /* it's an integer */
		    yyval.u.l = zstrtol(ptr, &ptr, 10);

		    if (*ptr == '#') {
			ptr++;
			yyval.u.l = zstrtol(ptr, &ptr, lastbase = yyval.u.l);
		    }
		}
		return NUM;
	    }
	    if (*ptr == '#') {
		if (*++ptr == '\\') {
		    int v;

		    ptr++;
		    ptr = getkeystring(ptr, NULL, 6, &v);
		    yyval.u.l = v;
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
		yyval.u.l = poundgetfn(NULL);
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
    mnumber val;
};

static struct mathvalue *stack;

/**/
static void
push(mnumber val, LV lval)
{
    if (sp == STACKSZ - 1)
	zerr("stack overflow", NULL, 0);
    else
	sp++;
    stack[sp].val = val;
    stack[sp].lval = lval;
}


/**/
static mnumber
getcvar(LV s)
{
    char *t;
    mnumber mn;
    mn.type = MN_INTEGER;

    if (!(t = getsparam(lvals[s])))
	mn.u.l = 0;
    else
        mn.u.l = STOUC(*t == Meta ? t[1] ^ 32 : *t);
    return mn;
}


/**/
static mnumber
setvar(LV s, mnumber v)
{
    if (s == -1 || s >= lvc) {
	zerr("lvalue required", NULL, 0);
	v.type = MN_INTEGER;
	v.u.l = 0;
    }
    if (noeval)
	return v;
    setnparam(lvals[s], v);
    return v;
}


/**/
static int
notzero(mnumber a)
{
    if ((a.type & MN_INTEGER) ? a.u.l == 0 : a.u.d == 0.0) {
	zerr("division by zero", NULL, 0);
	return 0;
    }
    return 1;
}

/* macro to pop three values off the value stack */

/**/
void
op(int what)
{
    mnumber a, b, c, *spval;
    LV lv;
    int tp = type[what];

    if (errflag)
	return;
    if (sp < 0) {
	zerr("bad math expression: stack empty", NULL, 0);
	return;
    }

    if (tp & (OP_A2|OP_A2IR|OP_A2IO|OP_E2|OP_E2IO)) {
	if (sp < 1) {
 	    zerr("bad math expression: unbalanced stack", NULL, 0); \
	    return;
	}
	b = stack[sp--].val;
	a = stack[sp--].val;

	if (tp & (OP_A2IO|OP_E2IO)) {
	    /* coerce to integers */
	    if (a.type & MN_FLOAT) {
		a.type = MN_INTEGER;
		a.u.l = (zlong)a.u.d;
	    }
	    if (b.type & MN_FLOAT) {
		b.type = MN_INTEGER;
		b.u.l = (zlong)b.u.d;
	    }
	} else if (a.type != b.type && what != COMMA) {
	    /*
	     * Different types, so coerce to float.
	     * It may happen during an assigment that the LHS
	     * variable is actually an integer, but there's still
	     * no harm in doing the arithmetic in floating point;
	     * the assignment will do the correct conversion.
	     * This way, if the parameter is actually a scalar, but
	     * used to contain an integer, we can write a float into it.
	     */
	    if (a.type & MN_INTEGER) {
		a.type = MN_FLOAT;
		a.u.d = (double)a.u.l;
	    }
	    if (b.type & MN_INTEGER) {
		b.type = MN_FLOAT;
		b.u.d = (double)b.u.l;
	    }
	}
	/*
	 * type for operation: usually same as operands, but e.g.
	 * (a == b) returns int.
	 */
	c.type = (tp & OP_A2IR) ? MN_INTEGER : a.type;

	switch(what) {
	case AND:
	case ANDEQ:
	    c.u.l = a.u.l & b.u.l;
	    break;
	case XOR:
	case XOREQ:
	    c.u.l = a.u.l ^ b.u.l;
	    break;
	case OR:
	case OREQ:
	    c.u.l = a.u.l | b.u.l;
	    break;
	case MUL:
	case MULEQ:
	    if (c.type == MN_FLOAT)
		c.u.d = a.u.d * b.u.d;
	    else
		c.u.l = a.u.l * b.u.l;
	    break;
	case DIV:
	case DIVEQ:
	    if (!notzero(b))
		return;
	    if (c.type == MN_FLOAT)
		c.u.d = a.u.d / b.u.d;
	    else
		c.u.l = a.u.l / b.u.l;
	    break;
	case MOD:
	case MODEQ:
	    if (!notzero(b))
		return;
	    c.u.l = a.u.l % b.u.l;
	    break;
	case PLUS:
	case PLUSEQ:
	    if (c.type == MN_FLOAT)
		c.u.d = a.u.d + b.u.d;
	    else
		c.u.l = a.u.l + b.u.l;
	    break;
	case MINUS:
	case MINUSEQ:
	    if (c.type == MN_FLOAT)
		c.u.d = a.u.d - b.u.d;
	    else
		c.u.l = a.u.l - b.u.l;
	    break;
	case SHLEFT:
	case SHLEFTEQ:
	    c.u.l = a.u.l << b.u.l;
	    break;
	case SHRIGHT:
	case SHRIGHTEQ:
	    c.u.l = a.u.l >> b.u.l;
	    break;
	case LES:
	    c.u.l = (zlong)
		(a.type == MN_FLOAT ? (a.u.d < b.u.d) : (a.u.l < b.u.l));
	    break;
	case LEQ:
	    c.u.l = (zlong)
		(a.type == MN_FLOAT ? (a.u.d <= b.u.d) : (a.u.l <= b.u.l));
	    break;
	case GRE:
	    c.u.l = (zlong)
		(a.type == MN_FLOAT ? (a.u.d > b.u.d) : (a.u.l > b.u.l));
	    break;
	case GEQ:
	    c.u.l = (zlong)
		(a.type == MN_FLOAT ? (a.u.d >= b.u.d) : (a.u.l >= b.u.l));
	    break;
	case DEQ:
	    c.u.l = (zlong)
		(a.type == MN_FLOAT ? (a.u.d == b.u.d) : (a.u.l == b.u.l));
	    break;
	case NEQ:
	    c.u.l = (zlong)
		(a.type == MN_FLOAT ? (a.u.d != b.u.d) : (a.u.l != b.u.l));
	    break;
	case DAND:
	case DANDEQ:
	    c.u.l = (zlong)(a.u.l && b.u.l);
	    break;
	case DOR:
	case DOREQ:
	    c.u.l = (zlong)(a.u.l || b.u.l);
	    break;
	case DXOR:
	case DXOREQ:
	    c.u.l = (zlong)((a.u.l && !b.u.l) || (!a.u.l && b.u.l));
	    break;
	case COMMA:
	    c = b;
	    break;
	case POWER:
	case POWEREQ:
	    if (c.type == MN_INTEGER && b.u.l < 0) {
		/* produces a real result, so cast to real. */
		a.type = b.type = c.type = MN_FLOAT;
		a.u.d = (double) a.u.l;
		b.u.d = (double) b.u.l;
	    }
	    if (c.type == MN_INTEGER) {
		for (c.u.l = 1; b.u.l--; c.u.l *= a.u.l);
	    } else {
		if (b.u.d <= 0 && !notzero(a))
		    return;
		if (a.u.d < 0) {
		    /* Error if (-num ** b) and b is not an integer */
		    double tst = (double)(zlong)b.u.d;
		    if (tst != b.u.d) {
			zerr("imaginary power", NULL, 0);
			return;
		    }
		}
		c.u.d = pow(a.u.d, b.u.d);
	    }
	    break;
	case EQ:
	    c = b;
	    break;
	}
	if (tp & (OP_E2|OP_E2IO)) {
	    lv = stack[sp+1].lval;
	    push(setvar(lv,c), lv);
	} else
	    push(c,-1);
	return;
    }

    spval = &stack[sp].val;
    switch (what) {
    case NOT:
	if (spval->type & MN_FLOAT) {
	    spval->u.l = !spval->u.d;
	    spval->type = MN_INTEGER;
	} else
	    spval->u.l = !spval->u.l;
	stack[sp].lval = -1;
	break;
    case COMP:
	if (spval->type & MN_FLOAT) {
	    spval->u.l = ~((zlong)spval->u.d);
	    spval->type = MN_INTEGER;
	} else
	    spval->u.l = ~spval->u.l;
	stack[sp].lval = -1;
	break;
    case POSTPLUS:
	a = *spval;
	if (spval->type & MN_FLOAT)
	    a.u.d++;
	else
	    a.u.l++;
	(void)setvar(stack[sp].lval, a);
	break;
    case POSTMINUS:
	a = *spval;
	if (spval->type & MN_FLOAT)
	    a.u.d--;
	else
	    a.u.l--;
	(void)setvar(stack[sp].lval, a);
	break;
    case UPLUS:
	stack[sp].lval = -1;
	break;
    case UMINUS:
	if (spval->type & MN_FLOAT)
	    spval->u.d = -spval->u.d;
	else
	    spval->u.l = -spval->u.l;
	stack[sp].lval = -1;
	break;
    case QUEST:
	if (sp < 2) {
 	    zerr("bad math expression: unbalanced stack", NULL, 0);
	    return;
	}
	c = stack[sp--].val;
	b = stack[sp--].val;
	a = stack[sp--].val;
	/* b and c can stay different types in this case. */
	push(((a.type & MN_FLOAT) ? a.u.d : a.u.l) ? b : c, -1);
	break;
    case COLON:
	break;
    case PREPLUS:
	if (spval->type & MN_FLOAT)
	    spval->u.d++;
	else
	    spval->u.l++;
	setvar(stack[sp].lval, *spval);
	break;
    case PREMINUS:
	if (spval->type & MN_FLOAT)
	    spval->u.d--;
	else
	    spval->u.l--;
	setvar(stack[sp].lval, *spval);
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
    mnumber *spval = &stack[sp].val;
    int tst = (spval->type & MN_FLOAT) ? (zlong)spval->u.d : spval->u.l; 

    switch (tk) {
    case DAND:
    case DANDEQ:
	if (!tst)
	    noeval++;
	break;
    case DOR:
    case DOREQ:
	if (tst)
	    noeval++;
	break;
    };
}


/**/
static mnumber
mathevall(char *s, int prek, char **ep)
{
    int t0;
    int xlastbase, xnoeval, xunary, xlvc;
    char *xptr;
    mnumber xyyval;
    LV xyylval;
    char **xlvals = 0, *nlvals[LVCOUNT];
    int xsp;
    struct mathvalue *xstack = 0, nstack[STACKSZ];
    mnumber ret;

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
    } else {
	xlastbase = xnoeval = xunary = xlvc = xyylval = xsp = 0;
	xyyval.type = MN_INTEGER;
	xyyval.u.l = 0;
	xptr = NULL;
    }
    stack = nstack;
    lastbase = -1;
    memset(nlvals, 0, LVCOUNT*sizeof(char *));
    lvals = nlvals;
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
mnumber
matheval(char *s)
{
    char *junk;
    mnumber x;
    int xmtok = mtok;

    if (!*s) {
	x.type = MN_INTEGER;
	x.u.l = 0;
	return x;
    }
    x = mathevall(s, TOPPREC, &junk);
    mtok = xmtok;
    if (*junk)
	zerr("bad math expression: illegal character: %c", NULL, *junk);
    return x;
}

/**/
zlong
mathevali(char *s)
{
    mnumber x = matheval(s);
    return (x.type & MN_FLOAT) ? (zlong)x.u.d : x.u.l;
}


/**/
zlong
mathevalarg(char *s, char **ss)
{
    mnumber x;
    int xmtok = mtok;

    x = mathevall(s, ARGPREC, ss);
    if (mtok == COMMA)
	(*ss)--;
    mtok = xmtok;
    return (x.type & MN_FLOAT) ? (zlong)x.u.d : x.u.l;
}

/*
 * Make sure we have an operator or an operand, whatever is expected.
 * For this purpose, unary operators constitute part of an operand.
 */

/**/
static void
checkunary(int tp, char *ptr)
{
    int errmsg = 0;
    if (tp & (OP_A2|OP_A2IR|OP_A2IO|OP_E2|OP_E2IO|OP_OP)) {
	if (unary)
	    errmsg = 1;
    } else {
	if (!unary)
	    errmsg = 2;
    }
    if (errmsg) {
	char errbuf[40];
	int len, over = 0;
	while (inblank(*ptr))
	    ptr++;
	len = strlen(ptr);
	if (len > 10) {
	    len = 10;
	    over = 1;
	}
	sprintf(errbuf, "bad math expression: %s expected at `%%l%s'",
		errmsg == 2 ? "operator" : "operand",
		over ? "..." : ""); 
	zerr(errbuf, ptr, len);
    }
    unary = !(tp & OP_OPF);
}

/* operator-precedence parse the string and execute */

/**/
static void
mathparse(int pc)
{
    zlong q;
    int otok, onoeval;
    char *optr = ptr;

    if (errflag)
	return;
    mtok = zzlex();
    checkunary(type[mtok], optr);
    while (prec[mtok] <= pc) {
	if (errflag)
	    return;
	switch (mtok) {
	case NUM:
	    push(yyval, -1);
	    break;
	case ID:
	    push(getnparam(lvals[yylval]), yylval);
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
	    q = (stack[sp].val.type == MN_FLOAT) ? (zlong)stack[sp].val.u.d :
		stack[sp].val.u.l;

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
	    if (MTYPE(type[otok]) == BOOL)
		bop(otok);
	    mathparse(prec[otok] - (MTYPE(type[otok]) != RL));
	    noeval = onoeval;
	    op(otok);
	    continue;
	}
	optr = ptr;
	mtok = zzlex();
	checkunary(type[mtok], optr);
    }
}
