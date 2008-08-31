/*
 * lex.c - lexical analysis
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
#include "lex.pro"

/* tokens */

/**/
mod_export char ztokens[] = "#$^*()$=|{}[]`<>?~`,'\"\\\\";

/* parts of the current token */

/**/
char *yytext;
/**/
mod_export char *tokstr;
/**/
mod_export int tok;
/**/
mod_export int tokfd;

/*
 * Line number at which the first character of a token was found.
 * We always set this in gettok(), which is always called from
 * yylex() unless we have reached an error.  So it is always
 * valid when parsing.  It is not useful during execution
 * of the parsed structure.
 */

/**/
zlong toklineno;

/* lexical analyzer error flag */
 
/**/
mod_export int lexstop;

/* if != 0, this is the first line of the command */
 
/**/
mod_export int isfirstln;
 
/* if != 0, this is the first char of the command (not including white space) */
 
/**/
int isfirstch;

/* flag that an alias should be expanded after expansion ending in space */

/**/
int inalmore;

/*
 * Don't do spelling correction.
 * Bit 1 is only valid for the current word.  It's
 * set when we detect a lookahead that stops the word from
 * needing correction.
 */
 
/**/
int nocorrect;

/*
 * Cursor position and line length in zle when the line is
 * metafied for access from the main shell.
 */

/**/
mod_export int zlemetacs, zlemetall;

/* inwhat says what exactly we are in     *
 * (its value is one of the IN_* things). */

/**/
mod_export int inwhat;

/* 1 if x added to complete in a blank between words */

/**/
mod_export int addedx;

/* wb and we hold the beginning/end position of the word we are completing. */

/**/
mod_export int wb, we;

/* 1 if aliases should not be expanded */

/**/
mod_export int noaliases;

/* we are parsing a line sent to use by the editor */

/**/
mod_export int zleparse;

/**/
mod_export int wordbeg;

/**/
mod_export int parbegin;

/**/
mod_export int parend;

/* don't recognize comments */

/**/
mod_export int nocomments;

/* text of punctuation tokens */

/**/
mod_export char *tokstrings[WHILE + 1] = {
    NULL,	/* NULLTOK	  0  */
    ";",	/* SEPER	     */
    "\\n",	/* NEWLIN	     */
    ";",	/* SEMI		     */
    ";;",	/* DSEMI	     */
    "&",	/* AMPER	  5  */
    "(",	/* INPAR	     */
    ")",	/* OUTPAR	     */
    "||",	/* DBAR		     */
    "&&",	/* DAMPER	     */
    ">",	/* OUTANG	  10 */
    ">|",	/* OUTANGBANG	     */
    ">>",	/* DOUTANG	     */
    ">>|",	/* DOUTANGBANG	     */
    "<",	/* INANG	     */
    "<>",	/* INOUTANG	  15 */
    "<<",	/* DINANG	     */
    "<<-",	/* DINANGDASH	     */
    "<&",	/* INANGAMP	     */
    ">&",	/* OUTANGAMP	     */
    "&>",	/* AMPOUTANG	  20 */
    "&>|",	/* OUTANGAMPBANG     */
    ">>&",	/* DOUTANGAMP	     */
    ">>&|",	/* DOUTANGAMPBANG    */
    "<<<",	/* TRINANG	     */
    "|",	/* BAR		  25 */
    "|&",	/* BARAMP	     */
    "()",	/* INOUTPAR	     */
    "((",	/* DINPAR	     */
    "))",	/* DOUTPAR	     */
    "&|",	/* AMPERBANG	  30 */
    ";&",	/* SEMIAMP	     */
    ";|",	/* SEMIBAR	     */
};

/* lexical state */

static int dbparens;
static int len = 0, bsiz = 256;
static char *bptr;

struct lexstack {
    struct lexstack *next;

    int incmdpos;
    int incond;
    int incasepat;
    int dbparens;
    int isfirstln;
    int isfirstch;
    int histactive;
    int histdone;
    int stophist;
    int hlinesz;
    char *hline;
    char *hptr;
    int tok;
    int isnewlin;
    char *tokstr;
    char *yytext;
    char *bptr;
    int bsiz;
    int len;
    short *chwords;
    int chwordlen;
    int chwordpos;
    int hwgetword;
    int lexstop;
    struct heredocs *hdocs;
    int (*hgetc) _((void));
    void (*hungetc) _((int));
    void (*hwaddc) _((int));
    void (*hwbegin) _((int));
    void (*hwend) _((void));
    void (*addtoline) _((int));

    int eclen, ecused, ecnpats;
    Wordcode ecbuf;
    Eccstr ecstrs;
    int ecsoffs, ecssub, ecnfunc;

    unsigned char *cstack;
    int csp;
    zlong toklineno;
};

static struct lexstack *lstack = NULL;

/* save the lexical state */

/* is this a hack or what? */

/**/
mod_export void
lexsave(void)
{
    struct lexstack *ls;

    ls = (struct lexstack *)malloc(sizeof(struct lexstack));

    ls->incmdpos = incmdpos;
    ls->incond = incond;
    ls->incasepat = incasepat;
    ls->dbparens = dbparens;
    ls->isfirstln = isfirstln;
    ls->isfirstch = isfirstch;
    ls->histactive = histactive;
    ls->histdone = histdone;
    ls->stophist = stophist;
    ls->hline = chline;
    ls->hptr = hptr;
    ls->hlinesz = hlinesz;
    ls->cstack = cmdstack;
    ls->csp = cmdsp;
    cmdstack = (unsigned char *)zalloc(CMDSTACKSZ);
    ls->tok = tok;
    ls->isnewlin = isnewlin;
    ls->tokstr = tokstr;
    ls->yytext = yytext;
    ls->bptr = bptr;
    ls->bsiz = bsiz;
    ls->len = len;
    ls->chwords = chwords;
    ls->chwordlen = chwordlen;
    ls->chwordpos = chwordpos;
    ls->hwgetword = hwgetword;
    ls->lexstop = lexstop;
    ls->hdocs = hdocs;
    ls->hgetc = hgetc;
    ls->hungetc = hungetc;
    ls->hwaddc = hwaddc;
    ls->hwbegin = hwbegin;
    ls->hwend = hwend;
    ls->addtoline = addtoline;
    ls->eclen = eclen;
    ls->ecused = ecused;
    ls->ecnpats = ecnpats;
    ls->ecbuf = ecbuf;
    ls->ecstrs = ecstrs;
    ls->ecsoffs = ecsoffs;
    ls->ecssub = ecssub;
    ls->ecnfunc = ecnfunc;
    ls->toklineno = toklineno;
    cmdsp = 0;
    inredir = 0;
    hdocs = NULL;
    histactive = 0;
    ecbuf = NULL;

    ls->next = lstack;
    lstack = ls;
}

/* restore lexical state */

/**/
mod_export void
lexrestore(void)
{
    struct lexstack *ln;

    DPUTS(!lstack, "BUG: lexrestore() without lexsave()");
    incmdpos = lstack->incmdpos;
    incond = lstack->incond;
    incasepat = lstack->incasepat;
    dbparens = lstack->dbparens;
    isfirstln = lstack->isfirstln;
    isfirstch = lstack->isfirstch;
    histactive = lstack->histactive;
    histdone = lstack->histdone;
    stophist = lstack->stophist;
    chline = lstack->hline;
    hptr = lstack->hptr;
    if (cmdstack)
	free(cmdstack);
    cmdstack = lstack->cstack;
    cmdsp = lstack->csp;
    tok = lstack->tok;
    isnewlin = lstack->isnewlin;
    tokstr = lstack->tokstr;
    yytext = lstack->yytext;
    bptr = lstack->bptr;
    bsiz = lstack->bsiz;
    len = lstack->len;
    chwords = lstack->chwords;
    chwordlen = lstack->chwordlen;
    chwordpos = lstack->chwordpos;
    hwgetword = lstack->hwgetword;
    lexstop = lstack->lexstop;
    hdocs = lstack->hdocs;
    hgetc = lstack->hgetc;
    hungetc = lstack->hungetc;
    hwaddc = lstack->hwaddc;
    hwbegin = lstack->hwbegin;
    hwend = lstack->hwend;
    addtoline = lstack->addtoline;
    if (ecbuf)
	zfree(ecbuf, eclen);
    eclen = lstack->eclen;
    ecused = lstack->ecused;
    ecnpats = lstack->ecnpats;
    ecbuf = lstack->ecbuf;
    ecstrs = lstack->ecstrs;
    ecsoffs = lstack->ecsoffs;
    ecssub = lstack->ecssub;
    ecnfunc = lstack->ecnfunc;
    hlinesz = lstack->hlinesz;
    toklineno = lstack->toklineno;
    errflag = 0;

    ln = lstack->next;
    free(lstack);
    lstack = ln;
}

/**/
void
yylex(void)
{
    if (tok == LEXERR)
	return;
    do
	tok = gettok();
    while (tok != ENDINPUT && exalias());
    nocorrect &= 1;
    if (tok == NEWLIN || tok == ENDINPUT) {
	while (hdocs) {
	    struct heredocs *next = hdocs->next;
	    char *name;

	    hwbegin(0);
	    cmdpush(hdocs->type == REDIR_HEREDOC ? CS_HEREDOC : CS_HEREDOCD);
	    STOPHIST
	    name = gethere(hdocs->str, hdocs->type);
	    ALLOWHIST
	    cmdpop();
	    hwend();
	    if (!name) {
		zerr("here document too large");
		while (hdocs) {
		    next = hdocs->next;
		    zfree(hdocs, sizeof(struct heredocs));
		    hdocs = next;
		}
		tok = LEXERR;
		break;
	    }
	    setheredoc(hdocs->pc, REDIR_HERESTR, name);
	    zfree(hdocs, sizeof(struct heredocs));
	    hdocs = next;
	}
    }
    if (tok != NEWLIN)
	isnewlin = 0;
    else
	isnewlin = (inbufct) ? -1 : 1;
    if (tok == SEMI || tok == NEWLIN)
	tok = SEPER;
}

/**/
mod_export void
ctxtlex(void)
{
    static int oldpos;

    yylex();
    switch (tok) {
    case SEPER:
    case NEWLIN:
    case SEMI:
    case DSEMI:
    case SEMIAMP:
    case SEMIBAR:
    case AMPER:
    case AMPERBANG:
    case INPAR:
    case INBRACE:
    case DBAR:
    case DAMPER:
    case BAR:
    case BARAMP:
    case INOUTPAR:
    case DOLOOP:
    case THEN:
    case ELIF:
    case ELSE:
    case DOUTBRACK:
	incmdpos = 1;
	break;
    case STRING:
 /* case ENVSTRING: */
    case ENVARRAY:
    case OUTPAR:
    case CASE:
    case DINBRACK:
	incmdpos = 0;
	break;
    }
    if (tok != DINPAR)
	infor = tok == FOR ? 2 : 0;
    if (IS_REDIROP(tok) || tok == FOR || tok == FOREACH || tok == SELECT) {
	inredir = 1;
	oldpos = incmdpos;
	incmdpos = 0;
    } else if (inredir) {
	incmdpos = oldpos;
	inredir = 0;
    }
}

#define LX1_BKSLASH 0
#define LX1_COMMENT 1
#define LX1_NEWLIN 2
#define LX1_SEMI 3
#define LX1_AMPER 5
#define LX1_BAR 6
#define LX1_INPAR 7
#define LX1_OUTPAR 8
#define LX1_INANG 13
#define LX1_OUTANG 14
#define LX1_OTHER 15

#define LX2_BREAK 0
#define LX2_OUTPAR 1
#define LX2_BAR 2
#define LX2_STRING 3
#define LX2_INBRACK 4
#define LX2_OUTBRACK 5
#define LX2_TILDE 6
#define LX2_INPAR 7
#define LX2_INBRACE 8
#define LX2_OUTBRACE 9
#define LX2_OUTANG 10
#define LX2_INANG 11
#define LX2_EQUALS 12
#define LX2_BKSLASH 13
#define LX2_QUOTE 14
#define LX2_DQUOTE 15
#define LX2_BQUOTE 16
#define LX2_COMMA 17
#define LX2_OTHER 18
#define LX2_META 19

static unsigned char lexact1[256], lexact2[256], lextok2[256];

/**/
void
initlextabs(void)
{
    int t0;
    static char *lx1 = "\\q\n;!&|(){}[]<>";
    static char *lx2 = ";)|$[]~({}><=\\\'\"`,";

    for (t0 = 0; t0 != 256; t0++) {
	lexact1[t0] = LX1_OTHER;
	lexact2[t0] = LX2_OTHER;
	lextok2[t0] = t0;
    }
    for (t0 = 0; lx1[t0]; t0++)
	lexact1[(int)lx1[t0]] = t0;
    for (t0 = 0; lx2[t0]; t0++)
	lexact2[(int)lx2[t0]] = t0;
    lexact2['&'] = LX2_BREAK;
    lexact2[STOUC(Meta)] = LX2_META;
    lextok2['*'] = Star;
    lextok2['?'] = Quest;
    lextok2['{'] = Inbrace;
    lextok2['['] = Inbrack;
    lextok2['$'] = String;
    lextok2['~'] = Tilde;
    lextok2['#'] = Pound;
    lextok2['^'] = Hat;
}

/* initialize lexical state */

/**/
void
lexinit(void)
{
    incond = incasepat = nocorrect =
    infor = dbparens = lexstop = 0;
    incmdpos = 1;
    tok = ENDINPUT;
}

/* add a char to the string buffer */

/**/
void
add(int c)
{
    *bptr++ = c;
    if (bsiz == ++len) {
#if 0
	int newbsiz;

	newbsiz = bsiz * 8;
	while (newbsiz < inbufct)
	    newbsiz *= 2;
	bptr = len + (tokstr = (char *)hrealloc(tokstr, bsiz, newbsiz));
	bsiz = newbsiz;
#endif

	int newbsiz = bsiz * 2;

	if (newbsiz > inbufct && inbufct > bsiz)
	    newbsiz = inbufct;

	bptr = len + (tokstr = (char *)hrealloc(tokstr, bsiz, newbsiz));
	bsiz = newbsiz;
    }
}

#define SETPARBEGIN {if (zleparse && !(inbufflags & INP_ALIAS) && zlemetacs >= zlemetall+1-inbufct) parbegin = inbufct;}
#define SETPAREND {\
	    if (zleparse && !(inbufflags & INP_ALIAS) && parbegin != -1 && parend == -1) {\
		if (zlemetacs >= zlemetall + 1 - inbufct)\
		    parbegin = -1;\
		else\
		    parend = inbufct;} }

/*
 * Return 1 for math, 0 for a command, 2 for an error.  If it couldn't be
 * parsed as math, but there was no gross error, it's a command.
 */

static int
cmd_or_math(int cs_type)
{
    int oldlen = len;
    int c;

    cmdpush(cs_type);
    c = dquote_parse(')', 0);
    cmdpop();
    *bptr = '\0';
    if (!c) {
	/* Successfully parsed, see if it was math */
	c = hgetc();
	if (c == ')')
	    return 1; /* yes */
	hungetc(c);
	lexstop = 0;
	c = ')';
    } else if (lexstop) {
	/* we haven't got anything to unget */
	return 2;
    }
    /* else unsuccessful: unget the whole thing */
    hungetc(c);
    lexstop = 0;
    while (len > oldlen) {
	len--;
	hungetc(itok(*--bptr) ? ztokens[*bptr - Pound] : *bptr);
    }
    hungetc('(');
    return 0;
}


/*
 * Parse either a $(( ... )) or a $(...)
 * Return 0 on success, 1 on failure.
 */
static int
cmd_or_math_sub(void)
{
    int c = hgetc(), ret;

    if (c == '(') {
	add(Inpar);
	add('(');
	if ((ret = cmd_or_math(CS_MATHSUBST)) == 1) {
	    add(')');
	    return 0;
	}
	if (ret == 2)
	    return 1;
	bptr -= 2;
	len -= 2;
    } else {
	hungetc(c);
	lexstop = 0;
    }
    return skipcomm();
}

/* Check whether we're looking at valid numeric globbing syntax      *
 * (/\<[0-9]*-[0-9]*\>/).  Call pointing just after the opening "<". *
 * Leaves the input in the same place, returning 0 or 1.             */

/**/
static int
isnumglob(void)
{
    int c, ec = '-', ret = 0;
    int tbs = 256, n = 0;
    char *tbuf = (char *)zalloc(tbs);

    while(1) {
	c = hgetc();
	if(lexstop) {
	    lexstop = 0;
	    break;
	}
	tbuf[n++] = c;
	if(!idigit(c)) {
	    if(c != ec)
		break;
	    if(ec == '>') {
		ret = 1;
		break;
	    }
	    ec = '>';
	}
	if(n == tbs)
	    tbuf = (char *)realloc(tbuf, tbs *= 2);
    }
    while(n--)
	hungetc(tbuf[n]);
    zfree(tbuf, tbs);
    return ret;
}

/**/
static int
gettok(void)
{
    int c, d;
    int peekfd = -1, peek;

  beginning:
    tokstr = NULL;
    while (iblank(c = hgetc()) && !lexstop);
    toklineno = lineno;
    if (lexstop)
	return (errflag) ? LEXERR : ENDINPUT;
    isfirstln = 0;
    wordbeg = inbufct - (qbang && c == bangchar);
    hwbegin(-1-(qbang && c == bangchar));
    /* word includes the last character read and possibly \ before ! */
    if (dbparens) {
	len = 0;
	bptr = tokstr = (char *) hcalloc(bsiz = 32);
	hungetc(c);
	cmdpush(CS_MATH);
	c = dquote_parse(infor ? ';' : ')', 0);
	cmdpop();
	*bptr = '\0';
	if (!c && infor) {
	    infor--;
	    return DINPAR;
	}
	if (c || (c = hgetc()) != ')') {
	    hungetc(c);
	    return LEXERR;
	}
	dbparens = 0;
	return DOUTPAR;
    } else if (idigit(c)) {	/* handle 1< foo */
	d = hgetc();
	if(d == '&') {
	    d = hgetc();
	    if(d == '>') {
		peekfd = c - '0';
		hungetc('>');
		c = '&';
	    } else {
		hungetc(d);
		lexstop = 0;
		hungetc('&');
	    }
	} else if (d == '>' || d == '<') {
	    peekfd = c - '0';
	    c = d;
	} else {
	    hungetc(d);
	    lexstop = 0;
	}
    }

    /* chars in initial position in word */

    if (c == hashchar && !nocomments &&
	(isset(INTERACTIVECOMMENTS) ||
	 (!zleparse && !expanding &&
	  (!interact || unset(SHINSTDIN) || strin)))) {
	/* History is handled here to prevent extra  *
	 * newlines being inserted into the history. */

	while ((c = ingetc()) != '\n' && !lexstop) {
	    hwaddc(c);
	    addtoline(c);
	}

	if (errflag)
	    peek = LEXERR;
	else {
	    hwend();
	    hwbegin(0);
	    hwaddc('\n');
	    addtoline('\n');
	    peek = NEWLIN;
	}
	return peek;
    }
    switch (lexact1[STOUC(c)]) {
    case LX1_BKSLASH:
	d = hgetc();
	if (d == '\n')
	    goto beginning;
	hungetc(d);
	lexstop = 0;
	break;
    case LX1_NEWLIN:
	return NEWLIN;
    case LX1_SEMI:
	d = hgetc();
	if(d == ';')
	    return DSEMI;
	else if(d == '&')
	    return SEMIAMP;
	else if (d == '|')
	    return SEMIBAR;
	hungetc(d);
	lexstop = 0;
	return SEMI;
    case LX1_AMPER:
	d = hgetc();
	if (d == '&')
	    return DAMPER;
	else if (d == '!' || d == '|')
	    return AMPERBANG;
	else if (d == '>') {
	    tokfd = peekfd;
	    d = hgetc();
	    if (d == '!' || d == '|')
		return OUTANGAMPBANG;
	    else if (d == '>') {
		d = hgetc();
		if (d == '!' || d == '|')
		    return DOUTANGAMPBANG;
		hungetc(d);
		lexstop = 0;
		return DOUTANGAMP;
	    }
	    hungetc(d);
	    lexstop = 0;
	    return AMPOUTANG;
	}
	hungetc(d);
	lexstop = 0;
	return AMPER;
    case LX1_BAR:
	d = hgetc();
	if (d == '|')
	    return DBAR;
	else if (d == '&')
	    return BARAMP;
	hungetc(d);
	lexstop = 0;
	return BAR;
    case LX1_INPAR:
	d = hgetc();
	if (d == '(') {
	    if (infor) {
		dbparens = 1;
		return DINPAR;
	    }
	    if (incmdpos) {
		len = 0;
		bptr = tokstr = (char *) hcalloc(bsiz = 32);
		switch (cmd_or_math(CS_MATH)) {
		case 1:
		    return DINPAR;

		case 0:
		    return INPAR;

		default:
		    return LEXERR;
		}
	    }
	} else if (d == ')')
	    return INOUTPAR;
	hungetc(d);
	lexstop = 0;
	if (!(incond == 1 || incmdpos))
	    break;
	return INPAR;
    case LX1_OUTPAR:
	return OUTPAR;
    case LX1_INANG:
	d = hgetc();
	if (!incmdpos && d == '(') {
	    hungetc(d);
	    lexstop = 0;
	    unpeekfd:
	    if(peekfd != -1) {
		hungetc(c);
		c = '0' + peekfd;
	    }
	    break;
	}
	if (d == '>') {
	    peek = INOUTANG;
	} else if (d == '<') {
	    int e = hgetc();

	    if (e == '(') {
		hungetc(e);
		hungetc(d);
		peek = INANG;
	    } else if (e == '<')
		peek = TRINANG;
	    else if (e == '-')
		peek = DINANGDASH;
	    else {
		hungetc(e);
		lexstop = 0;
		peek = DINANG;
	    }
	} else if (d == '&') {
	    peek = INANGAMP;
	} else {
	    hungetc(d);
	    if(isnumglob())
		goto unpeekfd;
	    peek = INANG;
	}
	tokfd = peekfd;
	return peek;
    case LX1_OUTANG:
	d = hgetc();
	if (d == '(') {
	    hungetc(d);
	    goto unpeekfd;
	} else if (d == '&') {
	    d = hgetc();
	    if (d == '!' || d == '|')
		peek = OUTANGAMPBANG;
	    else {
		hungetc(d);
		lexstop = 0;
		peek = OUTANGAMP;
	    }
	} else if (d == '!' || d == '|')
	    peek = OUTANGBANG;
	else if (d == '>') {
	    d = hgetc();
	    if (d == '&') {
		d = hgetc();
		if (d == '!' || d == '|')
		    peek = DOUTANGAMPBANG;
		else {
		    hungetc(d);
		    lexstop = 0;
		    peek = DOUTANGAMP;
		}
	    } else if (d == '!' || d == '|')
		peek = DOUTANGBANG;
	    else if (d == '(') {
		hungetc(d);
		hungetc('>');
		peek = OUTANG;
	    } else {
		hungetc(d);
		lexstop = 0;
		peek = DOUTANG;
		if (isset(HISTALLOWCLOBBER))
		    hwaddc('|');
	    }
	} else {
	    hungetc(d);
	    lexstop = 0;
	    peek = OUTANG;
	    if (!incond && isset(HISTALLOWCLOBBER))
		hwaddc('|');
	}
	tokfd = peekfd;
	return peek;
    }

    /* we've started a string, now get the *
     * rest of it, performing tokenization */
    return gettokstr(c, 0);
}

/*
 * Get the remains of a token string.  This has two uses.
 * When called from gettok(), with sub = 0, we have already identified
 * any interesting initial character and want to get the rest of
 * what we now know is a string.  However, the string may still include
 * metacharacters and potentially substitutions.
 *
 * When called from parse_subst_string() with sub = 1, we are not
 * fully parsing a command line, merely tokenizing a string.
 * In this case we always add characters to the parsed string
 * unless there is a parse error.
 */

/**/
static int
gettokstr(int c, int sub)
{
    int bct = 0, pct = 0, brct = 0, fdpar = 0;
    int intpos = 1, in_brace_param = 0;
    int peek, inquote, unmatched = 0;
#ifdef DEBUG
    int ocmdsp = cmdsp;
#endif

    peek = STRING;
    if (!sub) {
	len = 0;
	bptr = tokstr = (char *) hcalloc(bsiz = 32);
    }
    for (;;) {
	int act;
	int e;
	int inbl = inblank(c);
	
	if (fdpar && !inbl && c != ')')
	    fdpar = 0;

	if (inbl && !in_brace_param && !pct)
	    act = LX2_BREAK;
	else {
	    act = lexact2[STOUC(c)];
	    c = lextok2[STOUC(c)];
	}
	switch (act) {
	case LX2_BREAK:
	    if (!in_brace_param && !sub)
		goto brk;
	    break;
	case LX2_META:
	    c = hgetc();
#ifdef DEBUG
	    if (lexstop) {
		fputs("BUG: input terminated by Meta\n", stderr);
		fflush(stderr);
		goto brk;
	    }
#endif
	    add(Meta);
	    break;
	case LX2_OUTPAR:
	    if (fdpar) {
		/* this is a single word `(   )', treat as INOUTPAR */
		add(c);
		*bptr = '\0';
		return INOUTPAR;
	    }
	    if ((sub || in_brace_param) && isset(SHGLOB))
		break;
	    if (!in_brace_param && !pct--) {
		if (sub) {
		    pct = 0;
		    break;
		} else
		    goto brk;
	    }
	    c = Outpar;
	    break;
	case LX2_BAR:
	    if (!pct && !in_brace_param) {
		if (sub)
		    break;
		else
		    goto brk;
	    }
	    if (unset(SHGLOB) || (!sub && !in_brace_param))
		c = Bar;
	    break;
	case LX2_STRING:
	    e = hgetc();
	    if (e == '[') {
		cmdpush(CS_MATHSUBST);
		add(String);
		add(Inbrack);
		c = dquote_parse(']', sub);
		cmdpop();
		if (c) {
		    peek = LEXERR;
		    goto brk;
		}
		c = Outbrack;
	    } else if (e == '(') {
		add(String);
		c = cmd_or_math_sub();
		if (c) {
		    peek = LEXERR;
		    goto brk;
		}
		c = Outpar;
	    } else {
		if (e == '{') {
		    add(c);
		    c = Inbrace;
		    ++bct;
		    cmdpush(CS_BRACEPAR);
		    if (!in_brace_param)
			in_brace_param = bct;
		} else {
		    hungetc(e);
		    lexstop = 0;
		}
	    }
	    break;
	case LX2_INBRACK:
	    if (!in_brace_param)
		brct++;
	    c = Inbrack;
	    break;
	case LX2_OUTBRACK:
	    if (!in_brace_param)
		brct--;
	    if (brct < 0)
		brct = 0;
	    c = Outbrack;
	    break;
	case LX2_INPAR:
	    if (isset(SHGLOB)) {
		if (sub || in_brace_param)
		    break;
		if (incasepat && !len)
		    return INPAR;
	    }
	    if (!in_brace_param) {
		if (!sub) {
		    e = hgetc();
		    hungetc(e);
		    lexstop = 0;
		    /* For command words, parentheses are only
		     * special at the start.  But now we're tokenising
		     * the remaining string.  So I don't see what
		     * the old incmdpos test here is for.
		     *   pws 1999/6/8
		     *
		     * Oh, no.
		     *  func1(   )
		     * is a valid function definition in [k]sh.  The best
		     * thing we can do, without really nasty lookahead tricks,
		     * is break if we find a blank after a parenthesis.  At
		     * least this can't happen inside braces or brackets.  We
		     * only allow this with SHGLOB (set for both sh and ksh).
		     *
		     * Things like `print @( |foo)' should still
		     * work, because [k]sh don't allow multiple words
		     * in a function definition, so we only do this
		     * in command position.
		     *   pws 1999/6/14
		     */
		    if (e == ')' || (isset(SHGLOB) && inblank(e) && !bct &&
				     !brct && !intpos && incmdpos)) {
			/*
			 * Either a () token, or a command word with
			 * something suspiciously like a ksh function
			 * definition.
			 * The current word isn't spellcheckable.
			 */
			nocorrect |= 2;
			goto brk;
		    }
		}
		/*
		 * This also handles the [k]sh `foo( )' function definition.
		 * Maintain a variable fdpar, set as long as a single set of
		 * parentheses contains only space.  Then if we get to the
		 * closing parenthesis and it is still set, we can assume we
		 * have a function definition.  Only do this at the start of
		 * the word, since the (...) must be a separate token.
		 */
		if (!pct++ && isset(SHGLOB) && intpos && !bct && !brct)
		    fdpar = 1;
	    }
	    c = Inpar;
	    break;
	case LX2_INBRACE:
	    if (isset(IGNOREBRACES) || sub)
		c = '{';
	    else {
		if (!len && incmdpos) {
		    add('{');
		    *bptr = '\0';
		    return STRING;
		}
		if (in_brace_param) {
		    cmdpush(CS_BRACE);
		}
		bct++;
	    }
	    break;
	case LX2_OUTBRACE:
	    if ((isset(IGNOREBRACES) || sub) && !in_brace_param)
		break;
	    if (!bct)
		break;
	    if (in_brace_param) {
		cmdpop();
	    }
	    if (bct-- == in_brace_param)
		in_brace_param = 0;
	    c = Outbrace;
	    break;
	case LX2_COMMA:
	    if (unset(IGNOREBRACES) && !sub && bct > in_brace_param)
		c = Comma;
	    break;
	case LX2_OUTANG:
	    if (!intpos) {
		if (in_brace_param || sub)
		    break;
		else
		    goto brk;
	    }
	    e = hgetc();
	    if (e != '(') {
		hungetc(e);
		lexstop = 0;
		if (in_brace_param || sub)
		    break;
		else
		    goto brk;
	    }
	    add(Outang);
	    if (skipcomm()) {
		peek = LEXERR;
		goto brk;
	    }
	    c = Outpar;
	    break;
	case LX2_INANG:
	    if (isset(SHGLOB) && sub)
		break;
	    e = hgetc();
	    if(e == '(' && intpos) {
		add(Inang);
		if (skipcomm()) {
		    peek = LEXERR;
		    goto brk;
		}
		c = Outpar;
		break;
	    }
	    hungetc(e);
	    if(isnumglob()) {
		add(Inang);
		while ((c = hgetc()) != '>')
		    add(c);
		c = Outang;
		break;
	    }
	    lexstop = 0;
	    if (in_brace_param || sub)
		break;
	    goto brk;
	case LX2_EQUALS:
	    if (intpos) {
		e = hgetc();
		if (e != '(') {
		    hungetc(e);
		    lexstop = 0;
		    c = Equals;
		} else {
		    add(Equals);
		    if (skipcomm()) {
			peek = LEXERR;
			goto brk;
		    }
		    c = Outpar;
		}
	    } else if (!sub && peek != ENVSTRING &&
		       incmdpos && !bct && !brct) {
		char *t = tokstr;
		if (idigit(*t))
		    while (++t < bptr && idigit(*t));
		else {
		    int sav = *bptr;
		    *bptr = '\0';
		    t = itype_end(t, IIDENT, 0);
		    if (t < bptr) {
			skipparens(Inbrack, Outbrack, &t);
		    } else {
			*bptr = sav;
		    }
		}
		if (*t == '+')
                    t++;
		if (t == bptr) {
		    e = hgetc();
		    if (e == '(' && incmdpos) {
			*bptr = '\0';
			return ENVARRAY;
		    }
		    hungetc(e);
		    lexstop = 0;
		    peek = ENVSTRING;
		    intpos = 2;
		} else
		    c = Equals;
	    } else
		c = Equals;
	    break;
	case LX2_BKSLASH:
	    c = hgetc();
	    if (c == '\n') {
		c = hgetc();
		if (!lexstop)
		    continue;
	    } else
		add(Bnull);
	    if (lexstop)
		goto brk;
	    break;
	case LX2_QUOTE: {
	    int strquote = (len && bptr[-1] == String);

	    add(Snull);
	    cmdpush(CS_QUOTE);
	    for (;;) {
		STOPHIST
		while ((c = hgetc()) != '\'' && !lexstop) {
		    if (strquote && c == '\\') {
			c = hgetc();
			if (lexstop)
			    break;
			/*
			 * Mostly we don't need to do anything special
			 * with escape backslashes or closing quotes
			 * inside $'...'; however in completion we
			 * need to be able to strip multiple backslashes
			 * neatly.
			 */
			if (c == '\\' || c == '\'')
			    add(Bnull);
			else
			    add('\\');
		    } else if (!sub && isset(CSHJUNKIEQUOTES) && c == '\n') {
			if (bptr[-1] == '\\')
			    bptr--, len--;
			else
			    break;
		    }
		    add(c);
		}
		ALLOWHIST
		if (c != '\'') {
		    unmatched = '\'';
		    peek = LEXERR;
		    cmdpop();
		    goto brk;
		}
		e = hgetc();
		if (e != '\'' || unset(RCQUOTES) || strquote)
		    break;
		add(c);
	    }
	    cmdpop();
	    hungetc(e);
	    lexstop = 0;
	    c = Snull;
	    break;
	}
	case LX2_DQUOTE:
	    add(Dnull);
	    cmdpush(CS_DQUOTE);
	    c = dquote_parse('"', sub);
	    cmdpop();
	    if (c) {
		unmatched = '"';
		peek = LEXERR;
		goto brk;
	    }
	    c = Dnull;
	    break;
	case LX2_BQUOTE:
	    add(Tick);
	    cmdpush(CS_BQUOTE);
	    SETPARBEGIN
	    inquote = 0;
	    while ((c = hgetc()) != '`' && !lexstop) {
		if (c == '\\') {
		    c = hgetc();
		    if (c != '\n') {
			add(c == '`' || c == '\\' || c == '$' ? Bnull : '\\');
			add(c);
		    }
		    else if (!sub && isset(CSHJUNKIEQUOTES))
			add(c);
		} else {
		    if (!sub && isset(CSHJUNKIEQUOTES) && c == '\n') {
			break;
		    }
		    add(c);
		    if (c == '\'') {
			if ((inquote = !inquote))
			    STOPHIST
			else
			    ALLOWHIST
		    }
		}
	    }
	    if (inquote)
		ALLOWHIST
	    cmdpop();
	    if (c != '`') {
		unmatched = '`';
		peek = LEXERR;
		goto brk;
	    }
	    c = Tick;
	    SETPAREND
	    break;
	}
	add(c);
	c = hgetc();
	if (intpos)
	    intpos--;
	if (lexstop)
	    break;
    }
  brk:
    hungetc(c);
    if (unmatched)
	zerr("unmatched %c", unmatched);
    if (in_brace_param) {
	while(bct-- >= in_brace_param)
	    cmdpop();
	zerr("closing brace expected");
    } else if (unset(IGNOREBRACES) && !sub && len > 1 &&
	       peek == STRING && bptr[-1] == '}' && bptr[-2] != Bnull) {
	/* hack to get {foo} command syntax work */
	bptr--;
	len--;
	lexstop = 0;
	hungetc('}');
    }
    *bptr = '\0';
    DPUTS(cmdsp != ocmdsp, "BUG: gettok: cmdstack changed.");
    return peek;
}


/* Return non-zero for error (character to unget), else zero */

/**/
static int
dquote_parse(char endchar, int sub)
{
    int pct = 0, brct = 0, bct = 0, intick = 0, err = 0;
    int c;
    int math = endchar == ')' || endchar == ']';
    int zlemath = math && zlemetacs > zlemetall + addedx - inbufct;

    while (((c = hgetc()) != endchar || bct ||
	    (math && ((pct > 0) || (brct > 0))) ||
	    intick) && !lexstop) {
      cont:
	switch (c) {
	case '\\':
	    c = hgetc();
	    if (c != '\n') {
		if (c == '$' || c == '\\' || (c == '}' && !intick && bct) ||
		    c == endchar || c == '`' ||
		    (endchar == ']' && (c == '[' || c == ']' ||
					c == '(' || c == ')' ||
					c == '{' || c == '}' ||
					(c == '"' && sub))))
		    add(Bnull);
		else {
		    /* lexstop is implicitly handled here */
		    add('\\');
		    goto cont;
		}
	    } else if (sub || unset(CSHJUNKIEQUOTES) || endchar != '"')
		continue;
	    break;
	case '\n':
	    err = !sub && isset(CSHJUNKIEQUOTES) && endchar == '"';
	    break;
	case '$':
	    if (intick)
		break;
	    c = hgetc();
	    if (c == '(') {
		add(Qstring);
		err = cmd_or_math_sub();
		c = Outpar;
	    } else if (c == '[') {
		add(String);
		add(Inbrack);
		cmdpush(CS_MATHSUBST);
		err = dquote_parse(']', sub);
		cmdpop();
		c = Outbrack;
	    } else if (c == '{') {
		add(Qstring);
		c = Inbrace;
		cmdpush(CS_BRACEPAR);
		bct++;
	    } else if (c == '$')
		add(Qstring);
	    else {
		hungetc(c);
		lexstop = 0;
		c = Qstring;
	    }
	    break;
	case '}':
	    if (intick || !bct)
		break;
	    c = Outbrace;
	    bct--;
	    cmdpop();
	    break;
	case '`':
	    c = Qtick;
	    if (intick == 2)
		ALLOWHIST
	    if ((intick = !intick)) {
		SETPARBEGIN
		cmdpush(CS_BQUOTE);
	    } else {
		SETPAREND
	        cmdpop();
	    }
	    break;
	case '\'':
	    if (!intick)
		break;
	    if (intick == 1)
		intick = 2, STOPHIST
	    else
		intick = 1, ALLOWHIST
	    break;
	case '(':
	    if (!math || !bct)
		pct++;
	    break;
	case ')':
	    if (!math || !bct)
		err = (!pct-- && math);
	    break;
	case '[':
	    if (!math || !bct)
		brct++;
	    break;
	case ']':
	    if (!math || !bct)
		err = (!brct-- && math);
	    break;
	case '"':
	    if (intick || ((endchar == ']' || !endchar) && !bct))
		break;
	    if (bct) {
		add(Dnull);
		cmdpush(CS_DQUOTE);
		err = dquote_parse('"', sub);
		cmdpop();
		c = Dnull;
	    } else
		err = 1;
	    break;
	}
	if (err || lexstop)
	    break;
	add(c);
    }
    if (intick == 2)
	ALLOWHIST
    if (intick) {
	cmdpop();
    }
    while (bct--)
	cmdpop();
    if (lexstop)
	err = intick || endchar || err;
    else if (err == 1) {
	/*
	 * TODO: as far as I can see, this hack is used in gettokstr()
	 * to hungetc() a character on an error.  However, I don't
	 * understand what that actually gets us, and we can't guarantee
	 * it's a character anyway, because of the previous test.
	 *
	 * We use the same feature in cmd_or_math where we actually do
	 * need to unget if we decide it's really a command substitution.
	 * We try to handle the other case by testing for lexstop.
	 */
	err = c;
    }
    if (zlemath && zlemetacs <= zlemetall + 1 - inbufct)
	inwhat = IN_MATH;
    return err;
}

/* Tokenize a string given in s. Parsing is done as in double *
 * quotes.  This is usually called before singsub().          */

/**/
mod_export int
parsestr(char *s)
{
    int err;

    if ((err = parsestrnoerr(s))) {
	untokenize(s);
	if (err > 32 && err < 127)
	    zerr("parse error near `%c'", err);
	else
	    zerr("parse error");
    }
    return err;
}

/**/
mod_export int
parsestrnoerr(char *s)
{
    int l = strlen(s), err;

    lexsave();
    untokenize(s);
    inpush(dupstring(s), 0, NULL);
    strinbeg(0);
    len = 0;
    bptr = tokstr = s;
    bsiz = l + 1;
    err = dquote_parse('\0', 1);
    *bptr = '\0';
    strinend();
    inpop();
    DPUTS(cmdsp, "BUG: parsestr: cmdstack not empty.");
    lexrestore();
    return err;
}

/**/
mod_export char *
parse_subscript(char *s, int sub)
{
    int l = strlen(s), err;
    char *t;

    if (!*s || *s == ']')
	return 0;
    lexsave();
    untokenize(t = dupstring(s));
    inpush(t, 0, NULL);
    strinbeg(0);
    len = 0;
    bptr = tokstr = s;
    bsiz = l + 1;
    err = dquote_parse(']', sub);
    if (err) {
	err = *bptr;
	*bptr = 0;
	untokenize(s);
	*bptr = err;
	s = 0;
    } else
	s = bptr;
    strinend();
    inpop();
    DPUTS(cmdsp, "BUG: parse_subscript: cmdstack not empty.");
    lexrestore();
    return s;
}

/* Tokenize a string given in s. Parsing is done as if s were a normal *
 * command-line argument but it may contain separators.  This is used  *
 * to parse the right-hand side of ${...%...} substitutions.           */

/**/
mod_export int
parse_subst_string(char *s)
{
    int c, l = strlen(s), err, olen, lexstop_ret;
    char *ptr;

    if (!*s || !strcmp(s, nulstring))
	return 0;
    lexsave();
    untokenize(s);
    inpush(dupstring(s), 0, NULL);
    strinbeg(0);
    len = 0;
    bptr = tokstr = s;
    bsiz = l + 1;
    c = hgetc();
    lexstop_ret = lexstop;
    c = gettokstr(c, 1);
    err = errflag;
    strinend();
    inpop();
    DPUTS(cmdsp, "BUG: parse_subst_string: cmdstack not empty.");
    olen = len;
    lexrestore();
    errflag = err;
    if (c == LEXERR) {
	untokenize(s);
	return 1;
    }
#ifdef DEBUG
    /*
     * Historical note: we used to check here for olen == l, but
     * that's not necessarily the case if we stripped an RCQUOTE.
     */
    if (c != STRING || (errflag && !noerrs)) {
	fprintf(stderr, "Oops. Bug in parse_subst_string: %s\n",
		errflag ? "errflag" : "c != STRING");
	fflush(stderr);
	untokenize(s);
	return 1;
    }
#endif
    /* Check for $'...' quoting.  This needs special handling. */
    for (ptr = s; *ptr; )
    {
	if (*ptr == String && ptr[1] == Snull)
	{
	    char *t;
	    int len, tlen, diff;
	    t = getkeystring(ptr + 2, &len, GETKEYS_DOLLARS_QUOTE, NULL);
	    len += 2;
	    tlen = strlen(t);
	    diff = len - tlen;
	    /*
	     * Yuk.
	     * parse_subst_string() currently handles strings in-place.
	     * That's not so easy to fix without knowing whether
	     * additional memory should come off the heap or
	     * otherwise.  So we cheat by copying the unquoted string
	     * into place, unless it's too long.  That's not the
	     * normal case, but I'm worried there are are pathological
	     * cases with converting metafied multibyte strings.
	     * If someone can prove there aren't I will be very happy.
	     */
	    if (diff < 0) {
		DPUTS(1, "$'...' subst too long: fix get_parse_string()");
		return 1;
	    }
	    memcpy(ptr, t, tlen);
	    ptr += tlen;
	    if (diff > 0) {
		char *dptr = ptr;
		char *sptr = ptr + diff;
		while ((*dptr++ = *sptr++))
		    ;
	    }
	} else
	    ptr++;
    }
    return 0;
}

/* Called below to report word positions. */

/**/
mod_export void
gotword(void)
{
    we = zlemetall + 1 - inbufct + (addedx == 2 ? 1 : 0);
    if (zlemetacs <= we) {
	wb = zlemetall - wordbeg + addedx;
	zleparse = 0;
    }
}

/* expand aliases and reserved words */

/**/
int
exalias(void)
{
    Alias an;
    Reswd rw;

    hwend();
    if (interact && isset(SHINSTDIN) && !strin && !incasepat &&
	tok == STRING && !nocorrect && !(inbufflags & INP_ALIAS) &&
	(isset(CORRECTALL) || (isset(CORRECT) && incmdpos)))
	spckword(&tokstr, 1, incmdpos, 1);

    if (!tokstr) {
	yytext = tokstrings[tok];

	return 0;
    } else {
	VARARR(char, copy, (strlen(tokstr) + 1));

	if (has_token(tokstr)) {
	    char *p, *t;

	    yytext = p = copy;
	    for (t = tokstr;
		 (*p++ = itok(*t) ? ztokens[*t++ - Pound] : *t++););
	} else
	    yytext = tokstr;

	if (zleparse && !(inbufflags & INP_ALIAS)) {
	    int zp = zleparse;

	    gotword();
	    if (zp == 1 && !zleparse) {
		if (yytext == copy)
		    yytext = tokstr;
		return 0;
	    }
	}

	if (tok == STRING) {
	    /* Check for an alias */
	    if (!noaliases && isset(ALIASESOPT)) {
		char *suf;
		
		an = (Alias) aliastab->getnode(aliastab, yytext);
		if (an && !an->inuse &&
		    ((an->node.flags & ALIAS_GLOBAL) || incmdpos || inalmore)) {
		    inpush(an->text, INP_ALIAS, an);
		    if (an->text[0] == ' ')
			aliasspaceflag = 1;
		    lexstop = 0;
		    if (yytext == copy)
			yytext = tokstr;
		    return 1;
		}
		if ((suf = strrchr(yytext, '.')) && suf[1] &&
		    suf > yytext && suf[-1] != Meta &&
		    (an = (Alias)sufaliastab->getnode(sufaliastab, suf+1)) &&
		    !an->inuse && incmdpos) {
		    inpush(dupstring(yytext), INP_ALIAS, NULL);
		    inpush(" ", INP_ALIAS, NULL);
		    inpush(an->text, INP_ALIAS, an);
		    lexstop = 0;
		    if (yytext == copy)
			yytext = tokstr;
		    return 1;
		}
	    }

	    /* Then check for a reserved word */
	    if ((incmdpos ||
		 (unset(IGNOREBRACES) && yytext[0] == '}' && !yytext[1])) &&
		(rw = (Reswd) reswdtab->getnode(reswdtab, yytext))) {
		tok = rw->token;
		if (tok == DINBRACK)
		    incond = 1;
	    } else if (incond && !strcmp(yytext, "]]")) {
		tok = DOUTBRACK;
		incond = 0;
	    } else if (incond == 1 && yytext[0] == '!' && !yytext[1])
		tok = BANG;
	}
	inalmore = 0;
	if (yytext == copy)
	    yytext = tokstr;
    }
    return 0;
}

/* skip (...) */

/**/
static int
skipcomm(void)
{
    int pct = 1, c;

    cmdpush(CS_CMDSUBST);
    SETPARBEGIN
    c = Inpar;
    do {
	add(c);
	c = hgetc();
	if (itok(c) || lexstop)
	    break;
	switch (c) {
	case '(':
	    pct++;
	    break;
	case ')':
	    pct--;
	    break;
	case '\\':
	    add(c);
	    c = hgetc();
	    break;
	case '\'': {
	    int strquote = bptr[-1] == '$';
	    add(c);
	    STOPHIST
	    while ((c = hgetc()) != '\'' && !lexstop) {
		if (c == '\\' && strquote) {
		    add(c);
		    c = hgetc();
		}
		add(c);
	    }
	    ALLOWHIST
	    break;
	}
	case '\"':
	    add(c);
	    while ((c = hgetc()) != '\"' && !lexstop)
		if (c == '\\') {
		    add(c);
		    add(hgetc());
		} else
		    add(c);
	    break;
	case '`':
	    add(c);
	    while ((c = hgetc()) != '`' && !lexstop)
		if (c == '\\')
		    add(c), add(hgetc());
		else
		    add(c);
	    break;
	}
    }
    while (pct);
    if (!lexstop)
	SETPAREND
    cmdpop();
    return lexstop;
}
