/*
 * zle_tricky.c - expansion and completion
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

#include "zle.mdh"
#include "zle_tricky.pro"

/* The main part of ZLE maintains the line being edited as binary data, *
 * but here, where we interface with the lexer and other bits of zsh,   *
 * we need the line metafied.  The technique used is quite simple: on   *
 * entry to the expansion/completion system, we metafy the line in      *
 * place, adjusting ll and cs to match.  All completion and expansion   *
 * is done on the metafied line.  Immediately before returning, the     *
 * line is unmetafied again, changing ll and cs back.  (ll and cs might *
 * have changed during completion, so they can't be merely saved and    *
 * restored.)  The various indexes into the line that are used in this  *
 * file only are not translated: they remain indexes into the metafied  *
 * line.                                                                */

#ifdef HAVE_NIS_PLUS
# include <rpcsvc/nis.h>
#else
# ifdef HAVE_NIS
#  include	<rpc/types.h>
#  include	<rpc/rpc.h>
#  include	<rpcsvc/ypclnt.h>
#  include	<rpcsvc/yp_prot.h>

/* This is used when getting usernames from the NIS. */
typedef struct {
    int len;
    char *s;
}
dopestring;
# endif
#endif


#define inststr(X) inststrlen((X),1,-1)

/* wb and we hold the beginning/end position of the word we are completing. */

static int wb, we;

/* offs is the cursor position within the tokenized *
 * current word after removing nulargs.             */

static int offs;

/* These control the type of completion that will be done.  They are    *
 * affected by the choice of ZLE command and by relevant shell options. */

static int usemenu, useglob;

/* != 0 if we are in the middle of a menu completion */

static int menucmp;

/* Pointers to the current position in the groups list and in the menu-    *
 * completion array (the one that was put in the command line last).       */

static Cmgroup menugrp;
static Cmatch *menucur;

/* menupos is the point (in the command line) where the menu-completion   *
 * strings are inserted.  menulen is the length of the string that was    *
 * inserted last.  menuend is the end position of this string in the      *
 * command line.  menuwe is non-zero if the cursor was at the end of the  *
 * word (meaning that suffixes should go before the cursor).  menuinsc is *
 * the length of any suffix that has been temporarily added.              */

static int menupos, menulen, menuend, menuwe, menuinsc;

/* This is for completion inside a brace expansion. brbeg and brend hold  *
 * strings that were temporarily removed from the string to complete.     *
 * brpl and brsl hold the offset of these strings.                        */

static char *brbeg = NULL, *brend = NULL;
static int brpl, brsl;

/* The list of matches.  fmatches contains the matches we first ignore *
 * because of fignore.                                                 */

static LinkList matches, fmatches;

/* This holds the list of matches-groups. lmatches is a pointer to the  *
 * last element in this list. */

static Cmgroup amatches, lmatches;

/* The total number of matches and the number of matches to be listed. */

static int nmatches, smatches;

/* !=0 if we have a valid completion list. */

static int validlist;

/* This flag is non-zero if we are completing a pattern (with globcomplete) */

static int ispattern;

/* Two patterns used when doing glob-completion.  The first one is built *
 * from the whole word we are completing and the second one from that    *
 * part of the word that was identified as a possible filename.          */

static Comp patcomp, filecomp;

/* We store the following prefixes/suffixes:                               *
 * lpre/lsuf -- what's on the line                                         *
 * rpre/rsuf -- same as lpre/lsuf, but expanded                            *
 *                                                                         *
 * ... and if we are completing files, too:                                *
 * ppre/psuf   -- the path prefix/suffix                                   *
 * lppre/lpsuf -- the path prefix/suffix, unexpanded                       *
 * fpre/fsuf   -- prefix/suffix of the pathname component the cursor is in *
 * prpre       -- ppre in expanded form usable for opendir                 *
 * ipre,ripre  -- the ignored prefix (quotes and unquoted)                 *
 *                                                                         *
 * The integer variables hold the lengths of lpre, lsuf, rpre, rsuf,       *
 * fpre, fsuf, lppre, and lpsuf.  noreal is non-zero if we have rpre/rsuf. */

static char *lpre, *lsuf;
static char *rpre, *rsuf;
static char *ppre, *psuf, *lppre, *lpsuf, *prpre;
static char *fpre, *fsuf;
static char *ipre, *ripre;
static int lpl, lsl, rpl, rsl, fpl, fsl, lppl, lpsl;
static int noreal;

/* This is either zero or equal to the special character the word we are *
 * trying to complete starts with (e.g. Tilde or Equals).                */

static char ic;

/* This variable says what we are currently adding to the list of matches. */

static int addwhat;

/* This holds the word we are completing in quoted from. */

static char *qword;

/* This is non-zero if we are doing a menu-completion and this is not the *
 * first call (e.g. when automenu is set and menu-completion was entered  *
 * due to this). */

static int amenu;

/* The current group of matches. */

static Cmgroup mgroup;

/* A match counter. */

static int mnum;

/* Match flags for all matches in this group. */

static int mflags;

/* This holds the explanation strings we have to print in this group and *
 * a pointer to the current cexpl structure. */

static LinkList expls;
static Cexpl expl;

/* A pointer to the compctl we are using. */

static Compctl curcc;

/* A list of all compctls we have already used. */

static LinkList ccused;

/* A list of all compctls used so far. */

static LinkList allccs;

/* A stack of currently used compctls. */

static LinkList ccstack;

/* A stack of completion matchers to be used. */

static Cmlist mstack;

/* A heap of free Cline structures. */

static Cline freecl;

/* Information for ambiguous completions. One for fignore ignored and   *
 * one for normal completion. */

typedef struct aminfo *Aminfo;

struct aminfo {
    int cpl, csl, icpl, icsl;	/* common prefix/suffix lengths           */
    int prerest;		/* minimum prefix rest                    */
    int suflen;			/* minimum suffix length                  */
    Cmatch firstm;		/* the first match                        */
    char *pprefix;		/* common part of the -P prefixes         */
    char *aprefix;		/* common line prefix                     */
    int noipre;			/* if the was no ignored prefix           */
    char *iprefix;		/* common ignored prefix                  */
    char *iaprefix;		/* like aprefix, without ignored prefixes */
    int exact;			/* if there was an exact match            */
    Cmatch exactm;		/* the exact match (if any)               */
    Cline linecl, ilinecl;	/* what to put on the line as a Cline     */
    int count;			/* number of matches                      */
};

static Aminfo ainfo, fainfo;

/* Find out if we have to insert a tab (instead of trying to complete). */

/**/
static int
usetab(void)
{
    unsigned char *s = line + cs - 1;

    for (; s >= line && *s != '\n'; s--)
	if (*s != '\t' && *s != ' ')
	    return 0;
    return 1;
}

enum { COMP_COMPLETE,
       COMP_WIDGET,
       COMP_LIST_COMPLETE,
       COMP_SPELL,
       COMP_EXPAND,
       COMP_EXPAND_COMPLETE,
       COMP_LIST_EXPAND };
#define COMP_ISEXPAND(X) ((X) >= COMP_EXPAND)

/**/
void
completespecial(void)
{
    int flags = compwidget->flags;
    usemenu = (flags & ZLE_USEMENU) ? 1 : (flags & ZLE_NOMENU) ? 0
	: isset(MENUCOMPLETE);
    useglob = (flags & ZLE_USEGLOB) ? 1 : (flags & ZLE_NOGLOB) ? 0
	: isset(GLOBCOMPLETE);
    docomplete(compwidget->u.cc ? COMP_WIDGET : COMP_COMPLETE);
}

/**/
void
completeword(void)
{
    usemenu = isset(MENUCOMPLETE);
    useglob = isset(GLOBCOMPLETE);
    if (c == '\t' && usetab())
	selfinsert();
    else
	docomplete(COMP_COMPLETE);
}

/**/
void
menucomplete(void)
{
    usemenu = 1;
    useglob = isset(GLOBCOMPLETE);
    if (c == '\t' && usetab())
	selfinsert();
    else
	docomplete(COMP_COMPLETE);
}

/**/
void
listchoices(void)
{
    usemenu = isset(MENUCOMPLETE);
    useglob = isset(GLOBCOMPLETE);
    docomplete(COMP_LIST_COMPLETE);
}

/**/
void
spellword(void)
{
    usemenu = useglob = 0;
    docomplete(COMP_SPELL);
}

/**/
void
deletecharorlist(void)
{
    Cmgroup mg = menugrp;
    Cmatch *mc = menucur;

    usemenu = isset(MENUCOMPLETE);
    useglob = isset(GLOBCOMPLETE);
    if (cs != ll) {
	fixsuffix();
	deletechar();
    } else
	docomplete(COMP_LIST_COMPLETE);

    menucur = mc;
    menugrp = mg;
}

/**/
void
expandword(void)
{
    usemenu = useglob = 0;
    if (c == '\t' && usetab())
	selfinsert();
    else
	docomplete(COMP_EXPAND);
}

/**/
void
expandorcomplete(void)
{
    usemenu = isset(MENUCOMPLETE);
    useglob = isset(GLOBCOMPLETE);
    if (c == '\t' && usetab())
	selfinsert();
    else
	docomplete(COMP_EXPAND_COMPLETE);
}

/**/
void
menuexpandorcomplete(void)
{
    usemenu = 1;
    useglob = isset(GLOBCOMPLETE);
    if (c == '\t' && usetab())
	selfinsert();
    else
	docomplete(COMP_EXPAND_COMPLETE);
}

/**/
void
listexpand(void)
{
    usemenu = isset(MENUCOMPLETE);
    useglob = isset(GLOBCOMPLETE);
    docomplete(COMP_LIST_EXPAND);
}

/**/
void
reversemenucomplete(void)
{
    if (!menucmp) {
	menucomplete();
	return;
    }
    HEAPALLOC {
	if (menucur == menugrp->matches) {
	    do {
		if (!(menugrp = menugrp->prev))
		    menugrp = lmatches;
	    } while (!menugrp->mcount);
	    menucur = menugrp->matches + menugrp->mcount - 1;
	}
	else
	    menucur--;
	metafy_line();
	do_single(*menucur);
	unmetafy_line();
    } LASTALLOC;
}

/* Accepts the current completion and starts a new arg, *
 * with the next completions. This gives you a way to   *
 * accept several selections from the list of matches.  */

/**/
void
acceptandmenucomplete(void)
{
    if (!menucmp) {
	feep();
	return;
    }
    cs = menuend + menuinsc;
    inststrlen(" ", 1, 1);
    menuinsc = menulen = 0;
    menupos = cs;
    menuwe = 1;
    menucomplete();
}

/* These are flags saying if we are completing in the command *
 * position or in a redirection.                              */

static int lincmd, linredir;

/* Non-zero if the last completion done was ambiguous (used to find   *
 * out if AUTOMENU should start).  More precisely, it's nonzero after *
 * successfully doing any completion, unless the completion was       *
 * unambiguous and did not cause the display of a completion list.    *
 * From the other point of view, it's nonzero iff AUTOMENU (if set)   *
 * should kick in on another completion.                              */

static int lastambig;

/* This holds the name of the current command (used to find the right *
 * compctl).                                                          */

static char *cmdstr;


/* Check if the given string is the name of a parameter and if this *
 * parameter is one worth expanding.                                */

/**/
static int
checkparams(char *p)
{
    int t0, n, l = strlen(p), e = 0;
    struct hashnode *hn;

    for (t0 = paramtab->hsize - 1, n = 0; n < 2 && t0 >= 0; t0--)
	for (hn = paramtab->nodes[t0]; n < 2 && hn; hn = hn->next)
	    if (pfxlen(p, hn->nam) == l) {
		n++;
		if (strlen(hn->nam) == l)
		    e = 1;
	    }
    return (n == 1) ? (getsparam(p) != NULL) :
	(!menucmp && e && isset(RECEXACT));
}

/* Check if the given string has wildcards.  The difficulty is that we *
 * have to treat things like job specifications (%...) and parameter   *
 * expressions correctly.                                              */

/**/
static int
cmphaswilds(char *str)
{
    if ((*str == Inbrack || *str == Outbrack) && !str[1])
	return 0;

    /* If a leading % is immediately followed by ?, then don't *
     * treat that ? as a wildcard.  This is so you don't have  *
     * to escape job references such as %?foo.                 */
    if (str[0] == '%' && str[1] ==Quest)
	str += 2;

    for (; *str;) {
	if (*str == String || *str == Qstring) {
	    /* A parameter expression. */

	    if (*++str == Inbrace)
		skipparens(Inbrace, Outbrace, &str);
	    else if (*str == String || *str == Qstring)
		str++;
	    else {
		/* Skip all the things a parameter expression might start *
		 * with (before we come to the parameter name).           */
		for (; *str; str++)
		    if (*str != '^' && *str != Hat &&
			*str != '=' && *str != Equals &&
			*str != '~' && *str != Tilde)
			break;
		if (*str == '#' || *str == Pound)
		    str++;
		/* Star and Quest are parameter names here, not wildcards */
		if (*str == Star || *str == Quest)
		    str++;
	    }
	} else {
	    /* Not a parameter expression so we check for wildcards */
	    if (((*str == Pound || *str == Hat) && isset(EXTENDEDGLOB)) ||
		*str == Star || *str == Bar || *str == Quest ||
		!skipparens(Inbrack, Outbrack, &str) ||
		!skipparens(Inang,   Outang,   &str) ||
		(unset(IGNOREBRACES) &&
		 !skipparens(Inbrace, Outbrace, &str)) ||
		(*str == Inpar && str[1] == ':' &&
		 !skipparens(Inpar, Outpar, &str)))
		return 1;
	    if (*str)
		str++;
	}
    }
    return 0;
}

/* The main entry point for completion. */

/**/
static void
docomplete(int lst)
{
    char *s, *ol;
    int olst = lst, chl = 0, ne = noerrs, ocs;

    /* If we are doing a menu-completion... */

    if (menucmp && lst != COMP_LIST_EXPAND) {
	do_menucmp(lst);
	return;
    }

    /* Check if we have to start a menu-completion (via automenu). */

    if ((amenu = (isset(AUTOMENU) && lastambig)))
	usemenu = 1;

    /* Expand history references before starting completion.  If anything *
     * changed, do no more.                                               */

    if (doexpandhist())
	return;

    metafy_line();

    ocs = cs;
    if (!isfirstln && chline != NULL) {
	/* If we are completing in a multi-line buffer (which was not  *
	 * taken from the history), we have to prepend the stuff saved *
	 * in chline to the contents of line.                          */

	ol = dupstring((char *)line);
	/* Make sure that chline is zero-terminated. */
	*hptr = '\0';
	cs = 0;
	inststr(chline);
	chl = cs;
	cs += ocs;
    } else
	ol = NULL;
    inwhat = IN_NOTHING;
    qword = NULL;
    /* Get the word to complete. */
    noerrs = 1;
    s = get_comp_string();
    DPUTS(wb < 0 || cs < wb || cs > we,
	  "BUG: 0 <= wb <= cs <= we is not true!");
    noerrs = ne;
    /* For vi mode, reset the start-of-insertion pointer to the beginning *
     * of the word being completed, if it is currently later.  Vi itself  *
     * would never change the pointer in the middle of an insertion, but  *
     * then vi doesn't have completion.  More to the point, this is only  *
     * an emulation.                                                      */
    if (viinsbegin > ztrsub((char *) line + wb, (char *) line))
	viinsbegin = ztrsub((char *) line + wb, (char *) line);
    /* If we added chline to the line buffer, reset the original contents. */
    if (ol) {
	cs -= chl;
	wb -= chl;
	we -= chl;
	if (wb < 0) {
	    strcpy((char *) line, ol);
	    ll = strlen((char *) line);
	    cs = ocs;
	    unmetafy_line();
	    feep();
	    return;
	}
	ocs = cs;
	cs = 0;
	foredel(chl);
	cs = ocs;
    }
    freeheap();
    /* Save the lexer state, in case the completion code uses the lexer *
     * somewhere (e.g. when processing a compctl -s flag).              */
    lexsave();
    if (inwhat == IN_ENV)
	lincmd = 0;
    if (s) {
	if (lst == COMP_EXPAND_COMPLETE) {
	    /* Check if we have to do expansion or completion. */
	    char *q = s;

	    if (*q == Equals) {
		/* The word starts with `=', see if we can expand it. */
		q = s + 1;
		if (cmdnamtab->getnode(cmdnamtab, q) || hashcmd(q, pathchecked)) {
		    if (isset(RECEXACT))
			lst = COMP_EXPAND;
		    else {
			int t0, n = 0;
			char *fc;
			struct hashnode *hn;

			for (t0 = cmdnamtab->hsize - 1; t0 >= 0; t0--)
			    for (hn = cmdnamtab->nodes[t0]; hn;
				 hn = hn->next) {
				if (strpfx(q, hn->nam) && (fc = findcmd(hn->nam))) {
				    zsfree(fc);
				    n++;
				}
				if (n == 2)
				    break;
			    }

			if (n == 1)
			    lst = COMP_EXPAND;
		    }
		}
	    }
	    if (lst == COMP_EXPAND_COMPLETE)
		do {
		    /* check if there is a parameter expresiion. */
		    for (; *q && *q != String; q++);
		    if (*q == String && q[1] != Inpar && q[1] != Inbrack) {
			if (*++q == Inbrace) {
			    if (! skipparens(Inbrace, Outbrace, &q) &&
				q == s + cs - wb)
				lst = COMP_EXPAND;
			} else {
			    char *t, sav, sav2;

			    /* Skip the things parameter expressions might *
			     * start with (the things before the parameter *
			     * name).                                      */
			    for (; *q; q++)
				if (*q != '^' && *q != Hat &&
				    *q != '=' && *q != Equals &&
				    *q != '~' && *q != Tilde)
				    break;
			    if ((*q == '#' || *q == Pound || *q == '+') &&
				q[1] != String)
				q++;

			    sav2 = *(t = q);
			    if (*q == Quest || *q == Star || *q == String ||
				*q == Qstring)
				*q = ztokens[*q - Pound], ++q;
			    else if (*q == '?' || *q == '*' || *q == '$' ||
				     *q == '-' || *q == '!' || *q == '@')
				q++;
			    else if (idigit(*q))
				do q++; while (idigit(*q));
			    else
				while (iident(*q))
				    q++;
			    sav = *q;
			    *q = '\0';
			    if (cs - wb == q - s &&
				(idigit(sav2) || checkparams(t)))
				lst = COMP_EXPAND;
			    *q = sav;
			    *t = sav2;
			}
			if (lst != COMP_EXPAND)
			    lst = COMP_COMPLETE;
		    } else
			break;
		} while (q < s + cs - wb);
	    if (lst == COMP_EXPAND_COMPLETE) {
		/* If it is still not clear if we should use expansion or   *
		 * completion and there is a `$' or a backtick in the word, *
		 * than do expansion.                                       */
		for (q = s; *q; q++)
		    if (*q == Tick || *q == Qtick ||
			*q == String || *q == Qstring)
			break;
		lst = *q ? COMP_EXPAND : COMP_COMPLETE;
	    }
	    /* And do expansion if there are wildcards and globcomplete is *
	     * not used.                                                   */
	    if (unset(GLOBCOMPLETE) && cmphaswilds(s))
		lst = COMP_EXPAND;
	}
	if (lincmd && (inwhat == IN_NOTHING))
	    inwhat = IN_CMD;

	if (lst == COMP_SPELL) {
	    char *x, *q;

	    for (q = s; *q; q++)
		if (INULL(*q))
		    *q = Nularg;
	    cs = wb;
	    foredel(we - wb);
	    HEAPALLOC {
		untokenize(x = dupstring(s));
		if (*s == Tilde || *s == Equals || *s == String)
		    *x = *s;
		spckword(&x, 0, lincmd, 0);
	    } LASTALLOC;
	    untokenize(x);
	    inststr(x);
	} else if (COMP_ISEXPAND(lst)) {
	    /* Do expansion. */
	    char *ol = (olst == COMP_EXPAND_COMPLETE) ?
		dupstring((char *)line) : (char *)line;
	    int ocs = cs, ne = noerrs;

	    noerrs = 1;
	    doexpansion(s, lst, olst, lincmd);
	    lastambig = 0;
	    noerrs = ne;

	    /* If expandorcomplete was invoked and the expansion didn't *
	     * change the command line, do completion.                  */
	    if (olst == COMP_EXPAND_COMPLETE &&
		!strcmp(ol, (char *)line)) {
		char *p;

		cs = ocs;
		errflag = 0;

		p = s;
		if (*p == Tilde || *p == Equals)
		    p++;
		for (; *p; p++)
		    if (itok(*p)) {
			if (*p != String && *p != Qstring)
			    *p = ztokens[*p - Pound];
			else if (p[1] == Inbrace)
			    p++, skipparens(Inbrace, Outbrace, &p);
		    }
		docompletion(s, lst, lincmd);
	    }
	} else
	    /* Just do completion. */
	    docompletion(s, lst, lincmd);
	zsfree(s);
    }
    /* Reset the lexer state, pop the heap. */
    lexrestore();
    popheap();
    zsfree(qword);
    unmetafy_line();
}

/* Do completion, given that we are in the middle of a menu completion.  We *
 * don't need to generate a list of matches, because that's already been    *
 * done by previous commands.  We will either list the completions, or      *
 * insert the next completion.                                              */

/**/
static void
do_menucmp(int lst)
{
    /* Just list the matches if the list was requested. */
    if (lst == COMP_LIST_COMPLETE) {
	showinglist = -2;
	return;
    }
    /* Otherwise go to the next match in the array... */
    HEAPALLOC {
	if (!*++menucur) {
	    do {
		if (!(menugrp = menugrp->next))
		    menugrp = amatches;
	    } while (!menugrp->mcount);
	    menucur = menugrp->matches;
	}
	/* ... and insert it into the command line. */
	metafy_line();
	do_single(*menucur);
	unmetafy_line();
    } LASTALLOC;
}

/* 1 if we are completing the prefix */
static int comppref;

/* This function inserts an `x' in the command line at the cursor position. *
 *                                                                          *
 * Oh, you want to know why?  Well, if completion is tried somewhere on an  *
 * empty part of the command line, the lexer code would normally not be     *
 * able to give us the `word' we want to complete, since there is no word.  *
 * But we need to call the lexer to find out where we are (and for which    *
 * command we are completing and such things).  So we temporarily add a `x' *
 * (any character without special meaning would do the job) at the cursor   *
 * position, than the lexer gives us the word `x' and its beginning and end *
 * positions and we can remove the `x'.                                     *
 *									    *
 * If we are just completing the prefix (comppref set), we also insert a    *
 * space after the x to end the word.  We never need to remove the space:   *
 * anywhere we are able to retrieve a word for completion it will be	    *
 * discarded as whitespace.  It has the effect of making any suffix	    *
 * referrable to as the next word on the command line when indexing	    *
 * from a completion function.                                              */

/**/
static void
addx(char **ptmp)
{
    int addspace = 0;

    if (!line[cs] || line[cs] == '\n' ||
	(iblank(line[cs]) && (!cs || line[cs-1] != '\\')) ||
	line[cs] == ')' || line[cs] == '`' ||
	(instring && (line[cs] == '"' || line[cs] == '\'')) ||
	(addspace = (comppref && !iblank(line[cs])))) {
	*ptmp = (char *)line;
	line = (unsigned char *)halloc(strlen((char *)line) + 3 + addspace);
	memcpy(line, *ptmp, cs);
	line[cs] = 'x';
	if (addspace)
	    line[cs+1] = ' ';
	strcpy((char *)line + cs + 1 + addspace, (*ptmp) + cs);
	addedx = 1 + addspace;
    } else {
	addedx = 0;
	*ptmp = NULL;
    }
}

/* Like dupstring, but add an extra space at the end of the string. */

/**/
static char *
dupstrspace(const char *str)
{
    int len = strlen((char *)str);
    char *t = (char *)ncalloc(len + 2);
    strcpy(t, str);
    strcpy(t+len, " ");
    return t;
}

/* These functions metafy and unmetafy the ZLE buffer, as described at the *
 * top of this file.  Note that ll and cs are translated.  They *must* be  *
 * called in matching pairs, around all the expansion/completion code.     *
 * Currently, there are four pairs: in history expansion, in the main      *
 * completion function, and one in each of the middle-of-menu-completion   *
 * functions (there's one for each direction).                             */

/**/
static void
metafy_line(void)
{
    int len = ll;
    char *s;

    for (s = (char *) line; s < (char *) line + ll;)
	if (imeta(*s++))
	    len++;
    sizeline(len);
    (void) metafy((char *) line, ll, META_NOALLOC);
    ll = len;
    cs = metalen((char *) line, cs);
}

/**/
static void
unmetafy_line(void)
{
    cs = ztrsub((char *) line + cs, (char *) line);
    (void) unmetafy((char *) line, &ll);
}

/* Lasciate ogni speranza.                                                  *
 * This function is a nightmare.  It works, but I'm sure that nobody really *
 * understands why.  The problem is: to make it cleaner we would need       *
 * changes in the lexer code (and then in the parser, and then...).         */

/**/
static char *
get_comp_string(void)
{
    int t0, tt0, i, j, k, cp, rd, sl, ocs;
    char *s = NULL, *linptr, *tmp, *p, *tt = NULL;

    zsfree(brbeg);
    zsfree(brend);
    brbeg = brend = NULL;
    /* This global flag is used to signal the lexer code if it should *
     * expand aliases or not.                                         */
    noaliases = isset(COMPLETEALIASES);

    /* Find out if we are somewhere in a `string', i.e. inside '...', *
     * "...", `...`, or ((...)).                                      */

    for (i = j = k = 0, p = (char *)line; p < (char *)line + cs; p++)
	if (*p == '`' && !(k & 1))
	    i++;
	else if (*p == '\"' && !(k & 1) && !(i & 1))
	    j++;
	else if (*p == '\'' && !(j & 1))
	    k++;
	else if (*p == '\\' && p[1] && !(k & 1))
	    p++;
    instring = (j & 1) ? 2 : (k & 1);
    addx(&tmp);
    if (instring) {
	/* Yes, we are in a string. */
	if (!tmp) {
	    tmp = (char *)line;
	    line = (unsigned char *) dupstring((char *) line);
	}
	/* Now remove the quotes.                                   *
	 * What??  Why that??  Well, we want to be able to complete *
	 * inside strings.  The lexer code gives us no help here,   *
	 * so we have to cheat.  We remove the quotes, the lexer    *
	 * will than treat the words in the strings normally and we *
	 * can complete them.                                       *
	 * This is completely the wrong thing to do, but it's       *
	 * occasionally useful, and we can't handle quotes properly *
	 * yet anyway.                                              */
	for (p = (char *)line; *p; p++)
	    if (*p == '"' || *p == '\'')
		*p = ' ';
    }
    linptr = (char *)line;
    pushheap();
    HEAPALLOC {
      start:
	inwhat = IN_NOTHING;
	/* Now set up the lexer and start it. */
	parbegin = parend = -1;
	lincmd = incmdpos;
	linredir = inredir;
	zsfree(cmdstr);
	cmdstr = NULL;
	zleparse = 1;
	clwpos = -1;
	lexsave();
	inpush(dupstrspace((char *) linptr), 0, NULL);
	strinbeg();
	stophist = 2;
	i = tt0 = cp = rd = 0;

	/* This loop is possibly the wrong way to do this.  It goes through *
	 * the previously massaged command line using the lexer.  It stores *
	 * each token in each command (commands being regarded, roughly, as *
	 * being separated by tokens | & &! |& || &&).  The loop stops when *
	 * the end of the command containing the cursor is reached.  It's a *
	 * simple way to do things, but suffers from an inability to        *
	 * distinguish actual command arguments from, for example,          *
	 * filenames in redirections.  (But note that code elsewhere checks *
	 * if we are completing *in* a redirection.)  The only way to fix   *
	 * this would be to pass the command line through the parser too,   *
	 * and get the arguments that way.  Maybe in 3.1...                 */
	do {
	    lincmd = incmdpos;
	    linredir = inredir;
	    /* Get the next token. */
	    ctxtlex();
	    if (tok == DINPAR)
		tokstr = NULL;

	    /* We reached the end. */
	    if (tok == ENDINPUT)
		break;
	    if (tok == BAR    || tok == AMPER     ||
		tok == BARAMP || tok == AMPERBANG ||
		((tok == DBAR || tok == DAMPER) && !incond)) {
		/* This is one of the things that separate commands.  If we  *
		 * already have the things we need (e.g. the token strings), *
		 * leave the loop.                                           */
		if (tt)
		    break;
		/* Otherwise reset the variables we are collecting data in. */
		i = tt0 = cp = rd = 0;
	    }
	    if (lincmd && tok == STRING) {
		/* The lexer says, this token is in command position, so *
		 * store the token string (to find the right compctl).   */
		zsfree(cmdstr);
		cmdstr = ztrdup(tokstr);
		i = 0;
	    }
	    if (!zleparse && !tt0) {
		/* This is done when the lexer reached the word the cursor is on. */
		tt = tokstr ? dupstring(tokstr) : NULL;
		/* If we added a `x', remove it. */
		if (addedx && tt)
		    chuck(tt + cs - wb);
		tt0 = tok;
		/* Store the number of this word. */
		clwpos = i;
		cp = lincmd;
		rd = linredir;
		if (inwhat == IN_NOTHING && incond)
		    inwhat = IN_COND;
	    }
	    if (!tokstr)
		continue;
	    /* We need to store the token strings of all words (for some of *
	     * the more complicated compctl -x things).  They are stored in *
	     * the clwords array.  Make this array big enough.              */
	    if (i + 1 == clwsize) {
		int n;
		clwords = (char **)realloc(clwords,
					   (clwsize *= 2) * sizeof(char *));
		for(n = clwsize; --n > i; )
		    clwords[n] = NULL;
	    }
	    zsfree(clwords[i]);
	    /* And store the current token string. */
	    clwords[i] = ztrdup(tokstr);
	    sl = strlen(tokstr);
	    /* Sometimes the lexer gives us token strings ending with *
	     * spaces we delete the spaces.                           */
	    while (sl && clwords[i][sl - 1] == ' ' &&
		   (sl < 2 || (clwords[i][sl - 2] != Bnull &&
			       clwords[i][sl - 2] != Meta)))
		clwords[i][--sl] = '\0';
	    /* If this is the word the cursor is in and we added a `x', *
	     * remove it.                                               */
	    if (clwpos == i++ && addedx)
		chuck(&clwords[i - 1][((cs - wb) >= sl) ?
				     (sl - 1) : (cs - wb)]);
	} while (tok != LEXERR && tok != ENDINPUT &&
		 (tok != SEPER || (zleparse && !tt0)));
	/* Calculate the number of words stored in the clwords array. */
	clwnum = (tt || !i) ? i : i - 1;
	zsfree(clwords[clwnum]);
	clwords[clwnum] = NULL;
	t0 = tt0;
	lincmd = cp;
	linredir = rd;
	strinend();
	inpop();
	errflag = zleparse = 0;
	if (parbegin != -1) {
	    /* We are in command or process substitution */
	    if (parend >= 0 && !tmp)
		line = (unsigned char *) dupstring(tmp = (char *)line);
	    linptr = (char *) line + ll + addedx - parbegin + 1;
	    if (parend >= 0) {
		ll -= parend;
		line[ll + addedx] = '\0';
	    }
	    lexrestore();
	    goto start;
	}

	if (inwhat == IN_MATH)
	    s = NULL;
	else if (!t0 || t0 == ENDINPUT) {
	    /* There was no word (empty line). */
	    s = ztrdup("");
	    we = wb = cs;
	    clwpos = clwnum;
	    t0 = STRING;
	} else if (t0 == STRING) {
	    /* We found a simple string. */
	    s = ztrdup(clwords[clwpos]);
	} else if (t0 == ENVSTRING) {
	    /* The cursor was inside a parameter assignment. */
	    for (s = tt; iident(*s); s++);
	    if (skipparens(Inbrack, Outbrack, &s) > 0 || s > tt + cs - wb)
		s = NULL, inwhat = IN_MATH;
	    else if (*s == '=') {
		s++;
		wb += s - tt;
		t0 = STRING;
		s = ztrdup(s);
		inwhat = IN_ENV;
	    }
	    lincmd = 1;
	}
	if (we > ll)
	    we = ll;
	tt = (char *)line;
	if (tmp) {
	    line = (unsigned char *)tmp;
	    ll = strlen((char *)line);
	}
	if (t0 != STRING && inwhat != IN_MATH) {
	    if (tmp) {
		tmp = NULL;
		linptr = (char *)line;
		lexrestore();
		goto start;
	    }
	    feep();
	    noaliases = 0;
	    lexrestore();
	    LASTALLOC_RETURN NULL;
	}

	noaliases = 0;

	/* Check if we are in an array subscript.  We simply assume that  *
	 * we are in a subscript if we are in brackets.  Correct solution *
	 * is very difficult.  This is quite close, but gets things like  *
	 * foo[_ wrong (note no $).  If we are in a subscript, treat it   *
	 * as being in math.                                              */
	if (inwhat != IN_MATH) {
	    int i = 0;
	    for (tt = s; ++tt < s + cs - wb;)
		if (*tt == Inbrack)
		    i++;
		else if (i && *tt == Outbrack)
		    i--;
	    if (i)
		inwhat = IN_MATH;
	}
	if (inwhat == IN_MATH) {
	    /* In mathematical expression, we complete parameter names (even *
	     * if they don't have a `$' in front of them).  So we have to    *
	     * find that name.                                               */
	    for (we = cs; iident(line[we]); we++);
	    for (wb = cs; --wb >= 0 && iident(line[wb]););
	    wb++;
	    zsfree(s);
	    s = zalloc(we - wb + 1);
	    strncpy(s, (char *) line + wb, we - wb);
	    s[we - wb] = '\0';
	}
	/* This variable will hold the current word in quoted form. */
	qword = ztrdup(s);
	/* While building the quoted form, we also clean up the command line. */
	offs = cs - wb;
	for (p = s, tt = qword, i = wb; *p; p++, tt++, i++)
	    if (INULL(*p)) {
		if (i < cs)
		    offs--;
		if (p[1] || *p != Bnull) {
		    if (*p == Bnull) {
			*tt = '\\';
			if (cs == i + 1)
			    cs++, offs++;
		    } else {
			ocs = cs;
			cs = i;
			foredel(1);
			chuck(tt--);
			if ((cs = ocs) > i--)
			    cs--;
			we--;
		    }
		} else {
		    ocs = cs;
		    *tt = '\0';
		    cs = we;
		    backdel(1);
		    if (ocs == we)
			cs = we - 1;
		    else
			cs = ocs;
		    we--;
		}
		chuck(p--);
	    }

	if (!isset(IGNOREBRACES)) {
	    /* Try and deal with foo{xxx etc.; only simple cases
	     * (only one inbrace, completion after inbrace and before outbrace
	     * if present).
	     */
	    int myoffs = isset(COMPLETEINWORD) ? offs : strlen(s);
	    tt = NULL;
	    /* First check the conditions mentioned above
	     * and locate opening brace
	     */
	    for (i = 0, p = s; *p; p++, i++) {
		/* careful, ${... is not a brace expansion...
		 * in fact, if it's got a substitution in it's too
		 * hard for us anyway.  sorry.
		 */
		if (*p == String || *p == Qstring) {
		    tt = NULL;
		    break;
		} else if (*p == Inbrace) {
		    if (tt) {
			/* too many inbraces */
			tt = NULL;
			break;
		    }
		    tt = p;
		} else if (*p == Outbrace && i < myoffs) {
		    /* outbrace is before cursor pos, so nothing to complete */
		    tt = NULL;
		    break;
		}
	    }

	    if (tt && tt < s + myoffs) {
		/* Braces are go:  delete opening brace */
		char *com = NULL;
		int pl, sl;

		brbeg = dupstring(tt);
		brpl = tt - s;
		pl = 1;
		sl = 0;
		chuck(tt);
		offs--;
		myoffs--;

		/* Look for text up to comma before cursor and delete it */
		for (i = tt - s, p = tt; *p && i < myoffs; p++, i++)
		    if (*p == Comma)
			com = p;
		if (com) {
		    i = com - tt + 1;
		    offs -= i;
		    myoffs -= i;
		    strcpy(tt, tt + i);
		    pl += i;
		}
		brbeg[pl] = '\0';

		/* Look for text between subsequent comma
		 * and closing brace or end of string and delete it
		 */
		for (p = s + myoffs; *p && *p != Outbrace && *p != Comma; p++);
		if (*p == Comma || *p == Outbrace) {
		    brend = dupstring(p);
		    sl = 1;
		    while (*p && *p != Outbrace) {
			chuck(p); sl++;
		    }
		    if (*p == Outbrace)
			chuck(p);
		    brsl = strlen(s) - (p - s);
		    brend[sl] = '\0';
		}
		/* we are still waiting for an outbrace and maybe commas */
		if (brbeg)
		    untokenize(brbeg = ztrdup(brbeg));
		if (brend)
		    untokenize(brend = ztrdup(brend));
	    }
	}

    } LASTALLOC;
    lexrestore();

    return (char *)s;
}

/* Expand the current word. */

/**/
static void
doexpansion(char *s, int lst, int olst, int explincmd)
{
    LinkList vl;
    char *ss;

    DPUTS(useheap, "BUG: useheap in doexpansion()");
    HEAPALLOC {
	pushheap();
	vl = newlinklist();
	ss = dupstring(s);
	addlinknode(vl, ss);
	prefork(vl, 0);
	if (errflag)
	    goto end;
	if ((lst == COMP_LIST_EXPAND) || (lst == COMP_EXPAND)) {
	    int ng = opts[NULLGLOB];

	    opts[NULLGLOB] = 1;
	    globlist(vl);
	    opts[NULLGLOB] = ng;
	}
	if (errflag)
	    goto end;
	if (empty(vl) || !*(char *)peekfirst(vl)) {
	    if (!noerrs)
		feep();
	    goto end;
	}
	if (peekfirst(vl) == (void *) ss ||
		(olst == COMP_EXPAND_COMPLETE &&
		 !nextnode(firstnode(vl)) && *s == Tilde &&
		 (ss = dupstring(s), filesubstr(&ss, 0)) &&
		 !strcmp(ss, (char *)peekfirst(vl)))) {
	    /* If expansion didn't change the word, try completion if *
	     * expandorcomplete was called, otherwise, just beep.     */
	    if (lst == COMP_EXPAND_COMPLETE)
		docompletion(s, COMP_COMPLETE, explincmd);
	    else
		feep();
	    goto end;
	}
	if (lst == COMP_LIST_EXPAND) {
	    /* Only the list of expansions was requested. */
	    listlist(vl);
	    goto end;
	}
	/* Remove the current word and put the expansions there. */
	cs = wb;
	foredel(we - wb);
	while ((ss = (char *)ugetnode(vl))) {
	    untokenize(ss);
	    ss = quotename(ss, NULL, NULL, NULL);
	    inststr(ss);
#if 0
	    if (nonempty(vl)) {
		spaceinline(1);
		line[cs++] = ' ';
	    }
#endif
	    if (olst != COMP_EXPAND_COMPLETE || nonempty(vl) ||
		(cs && line[cs-1] != '/')) {
		spaceinline(1);
		line[cs++] = ' ';
	    }
	}
      end:
	popheap();
    } LASTALLOC;
}

/* This is called from the lexer to give us word positions. */

/**/
void
gotword(void)
{
    we = ll + 1 - inbufct + (addedx == 2 ? 1 : 0);
    if (cs <= we) {
	wb = ll - wordbeg + addedx;
	zleparse = 0;
    }
}

/* This adds a string to the currently built match. The last argument  *
 * is non zero if we are building the suffix, where we have to prepend *
 * the given string. */

static char *
addtoword(char **rwp, int *rwlenp, char *nw,
	  Cmatcher m, char *l, char *w, int wl, int prep)
{
    int al, rwlen = *rwlenp, nl;
    char *as, *rw = *rwp;
    
    if (m && (m->flags & CMF_LINE)) {
	al = m->llen;
	as = l;
    } else {
	al = wl;
	as = w;
    }
    if (!rwlen || (nl = al + (nw - rw)) >= rwlen) {
	char *np;

	if (!rwlen)
	    nl = al + 20;

	np = (char *) zalloc(nl + 1);

	*rwlenp = nl;
	if (rwlen) {
	    memcpy(np, rw, rwlen);
	    nw += np - rw;
	    zfree(rw, rwlen);
	}
	else
	    nw = np;
	*rwp = rw = np;
	rwlen = nl;
    }
    if (prep) {
	memmove(rw + al, rw, rwlen - al);
	memcpy(rw, as, al);
    }
    else
	memcpy(nw, as, al);

    return nw + al;
}

/* This get a new Cline structure. */

static Cline
getcline(char *l, int ll, char *w, int wl, Cmatcher m, int fl)
{
    Cline r;

    if ((r = freecl))
	freecl = r->next;
    else
	r = (Cline) halloc(sizeof(*r));

    r->next = NULL;
    r->line = l;
    r->llen = ll;
    r->word = w;
    r->wlen = wl;
    r->matcher = m;
    r->flags = fl;

    return r;
}

/* This add a Cline structure with the given parameters. */

static void
addtocline(Cline *retp, Cline *lrp,
	   char *l, int ll, char *w, int wl, Cmatcher m, int fl)
{
    Cline ln = getcline(l, ll, w, wl, m, fl);

    if (m && (m->flags & CMF_LINE))
	ln->word = NULL;
    if (*retp)
	(*lrp)->next = ln;
    else
	*retp = ln;

    *lrp = ln;
}

/* Joins two Cline lists, building the most specific line string *
 * that is possible. */

static Cline
join_clines(Cline o, Cline n)
{
    Cline oo = o;

    if (!o)
	return n;
    else {
	Cline f = freecl, q, op = NULL;
	int ol, nl;

	while (o && n) {
	    if (o->flags & CLF_MID) {
		while (n && !(n->flags & CLF_MID)) {
		    q = n->next;
		    n->next = f;
		    f = n;

		    n = q;
		}
	    }
	    if (n->flags & CLF_MID) {
		while (o && !(o->flags & CLF_MID)) {
		    o->word = NULL;
		    o->flags |= CLF_DIFF;

		    o = o->next;
		}
	    }
	    if (o && n && !((o->flags | n->flags) & CLF_MID)) {
		ol = o->llen;
		nl = n->llen;

		while (o && n && ol != nl) {
		    /* The matched strings have different lengths, so    *
		     * continue walking the lists until we have the same *
		     * matched lengths. */
		    o->word = NULL;
		    o->flags |= CLF_DIFF;
		    if (ol < nl) {
			op = o;
			if ((o = o->next))
			    ol += o->llen;
		    } else {
			q = n->next;
			n->next = f;
			f = n;

			if ((n = q))
			    nl += n->llen;
		    }
		}
	    }
	    if (!o || !n)
		break;
	    if (o->flags & CLF_MID) {
		char so, sn, *os, *ns;
		int ol = o->llen, l, lo, ln;

		so = o->line[o->llen];
		sn = n->line[n->llen];
		o->line[o->llen] = '\0';
		n->line[n->llen] = '\0';
		l = pfxlen(o->line, n->line);
		if (l != o->llen)
		    o->flags |= CLF_MISS;
		o->line[o->llen] = so;
		n->line[n->llen] = sn;
		o->llen = l;
		if ((lo = o->wlen) < 0) {
		    os = o->line + l;
		    lo = ol - l;
		} else
		    os = o->word;
		ns = n->line + l;
		ln = n->llen - l;
		so = os[lo];
		sn = ns[ln];
		os[lo] = '\0';
		ns[ln] = '\0';
		l = sfxlen(os, ns);
		os[lo] = so;
		ns[ln] = sn;
		o->word = os + lo - l;
		o->wlen = l;
	    } else if (o->word) {
		if (n->word) {
		    if (o->matcher == n->matcher &&
			(o->flags & CLF_SUF) == (n->flags & CLF_SUF) &&
			((o->flags & CLF_END) || o->matcher->wlen < 0)) {
			char so = o->word[o->wlen];
			char sn = n->word[n->wlen];
			int l;

			o->word[o->wlen] = '\0';
			n->word[n->wlen] = '\0';

			/* We have two chunks from `*' patterns.    *
			 * reduce them to the common prefix/suffix. */
			if (o->flags & CLF_SUF) {
			    l = sfxlen(o->word, n->word);
			    o->word[o->wlen] = so;
			    n->word[n->wlen] = sn;
			    o->word += o->wlen - l;
			} else {
			    l = pfxlen(o->word, n->word);
			    o->word[o->wlen] = so;
			    n->word[n->wlen] = sn;
			    o->word = dupstring(o->word);
			    o->word[l] = '\0';
			}
			o->wlen = l;
			if (l < o->wlen || l < n->wlen)
			    o->flags |= CLF_MISS;
		    } else if (o->wlen == n->wlen) {
			/* Otherwise keep them if they are equal. */
			if (strncmp(o->word, n->word, o->wlen)) {
			    /* If they are not equal, we make them *
			     * be left unchanged on the line. */
			    o->word = NULL;
			    o->flags |= CLF_DIFF;
			}
		    } else {
			o->word = NULL;
			o->flags |= CLF_DIFF;
		    }
		}
		else {
		    o->word = NULL;
		    o->flags |= CLF_DIFF;
		}
	    }
	    else if (n->word)
		o->flags |= CLF_DIFF;

	    q = n->next;
	    n->next = f;
	    f = n;

	    n = q;
	    op = o;
	    o = o->next;
	}
	if (o) {
	    for (q = o; q->next; q = q->next);

	    q->next = f;
	    f = o;

	    if (op)
		op->next = NULL;
	    else
		return NULL;
	}
	if (n) {
	    /* We always put the chunks from the second list back on *
	     * the free list. */
	    for (q = n; q->next; q = q->next);

	    q->next = f;
	    f = n;
	}
	freecl = f;
    }
    return oo;
}

/* This returns a Cline for the given string. */

static Cline
str_cline(char *s, int l, Cline *lp)
{
    Cline r = NULL, *p = &r, n = NULL;

    if (l < 0)
	l = strlen(s);
    while (l) {
	*p = n = getcline(s, 1, NULL, 0, NULL, 0);

	p = &(n->next);
	s++;
	l--;
    }
    if (lp)
	*lp = n;

    return r;
}

/* This reverts the order of the chunks. */

static Cline
revert_clines(Cline p)
{
    Cline r = NULL, n;

    while (p) {
	n = p->next;
	p->next = r;
	r = p;
	p = n;
    }
    return r;
}

/* Prepend a string to a Cline. */

static Cline
prepend_cline(char *s, Cline l)
{
    Cline n, r = str_cline(s, -1, &n), *p = &(n->next);

    while (l) {
	*p = n = getcline(l->line, l->llen, l->word, l->wlen,
			  l->matcher, l->flags);

	p = &(n->next);
	l = l->next;
    }
    return r;
}

/* This simplifies the Cline to match the given strings. The strings are: *
 * the common prefix and its length, a string with the suffix at the end  *
 * and the suffix length. */

static void
merge_cline(Cline lc, char *p, int pl, char *s, int sl, int psl)
{
    int pll, sll;
    Cline l = NULL, *q = &l, n;

    pll = strlen(p);
    if (s) {
	int i = strlen(s);

	if (ainfo->suflen < 10000)
	    s = s + i - ainfo->suflen;
	else {
	    s = s + i - sl;
	    p[pll - (sl - psl)] = '\0';
	    pll -= sl - psl;
	}
	sll = strlen(s) - sl;
    }
    else
	sll = 0;

    pll -= pl;

    l = str_cline(p, pl, &n);
    q = &(n->next);
    if (!sl)
	*q = getcline(NULL, 0, p + pl, pll, NULL, CLF_END);
    else {
	*q = n = getcline(p + pl, pll, s, sll, NULL, CLF_MID);

	n->next = str_cline(s + sll, sl, NULL);
    }
    join_clines(lc, l);
}

/* This inserts the Cline built into the command line. */

static void
inst_cline(Cline l, int pl, int sl)
{
    int m = -1, d = -1, b = -1, hb = 0, i = 0;
    Cline p = l;

    if (brend && *brend) {
	while (p) {
	    if (l->flags & CLF_MID) {
		i += l->llen;
		if (l->wlen >= 0)
		    i += l->wlen;
	    } else
		i += l->llen;

	    p = p->next;
	}
	sl = i - (brsl + sl) - 1;
    }
    else
	sl = -1;
    pl += brpl;

    i = cs - wb;
    while (l) {
	if (d < 0 && (l->flags & CLF_DIFF))
	    d = cs;
	if (m < 0 && (l->flags & (CLF_MISS | CLF_SUF)) == (CLF_MISS | CLF_SUF))
	    m = cs;
	if (l->flags & CLF_MID) {
	    inststrlen(l->line, 1, l->llen);
	    if (b < 0)
		b = cs;
	    if (l->wlen > 0)
		inststrlen(l->word, 1, l->wlen);
	} else if (l->word) {
	    inststrlen(l->word, 1, l->wlen);
	} else {
	    inststrlen(l->line, 1, l->llen);
	}
	if (m < 0 && (l->flags & CLF_MISS))
	    m = cs;
	i += l->llen;
	if (pl >= 0 && i >= pl && brbeg && *brbeg) {
	    inststrlen(brbeg, 1, -1);
	    pl = -1;
	    hb = 1;
	}
	if (sl >= 0 && i >= sl && brend && *brend) {
	    inststrlen(brend, 1, -1);
	    sl = -1;
	    hb = 1;
	}
	l = l->next;
    }
    cs = (b >= 0 && hb ? b : (m >= 0 ? m : (d >= 0 ? d : cs)));
    if (cs > ll)
	cs = ll;
}

/* Check if the given pattern matches the given string.             *
 * `in' and `out' are used for {...} classes. In `out' we store the *
 * character number that was matched. In the word pattern this is   *
 * given in `in' so that we can easily test if we found the         *
 * corresponding character. */

static int
pattern_match(Cpattern p, char *s, unsigned char *in, unsigned char *out)
{
    unsigned char c;

    while (p) {
	c = *((unsigned char *) s);

	if (out) *out = 0;

	if (p->equiv) {
	    if (in) {
		c = p->tab[c];
		if ((*in && *in != c) || (!*in && !c)) return 0;
	    } else if (out) {
		if (!(*out = p->tab[c])) return 0;
	    } else {
		if (!p->tab[c]) return 0;
	    }
	    if (in) in++;
	    if (out) out++;
	} else {
	    if (!p->tab[c]) return 0;
	}

	s++;
	p = p->next;
    }
    return 1;
}

/* Do the matching for a prefix. */

static char *
match_pfx(char *l, char *w, Cline *nlp, int *lp, Cline *rlp, int *bplp)
{
    static unsigned char *ea;
    static int ealen = 0;
    static char *rw;
    static int rwlen;

    int ll = strlen(l), lw = strlen(w), mlw;
    int il = 0, iw = 0, t, stil, stiw, std, bc = brpl;
    char *nw = rw, *stl = NULL, *stw;
    Cmlist ms;
    Cmatcher mp, stm;
    Cline lr = NULL;

    *nlp = NULL;

    if (ll > ealen) {
	/* This is the `in'/`out' string for pattern matching. */
	if (ealen)
	    zfree(ea, ealen);
	ea = (unsigned char *) zalloc(ealen = ll + 20);
    }
    while (ll && lw) {
	if (*l == *w) {
	    /* Same character, take it. */

	    if (stl) {
		/* But first check, if we were collecting characters *
		 * for a `*'. */
		int sl = iw - stiw;

		nw = addtoword(&rw, &rwlen, nw, stm, stl, stw, sl, 0);

		addtocline(nlp, &lr, stl, stm->llen,
			   stw, sl, stm, (std ? CLF_SUF : 0));

		stl = NULL;

		if (bc <= 0 && bplp) {
		    *bplp = nw - rw;
		    bplp = NULL;
		}
	    }
	    nw = addtoword(&rw, &rwlen, nw, NULL, NULL, l, 1, 0);

	    addtocline(nlp, &lr, l, 1, NULL, 0, NULL, 0);

	    l++;
	    w++;
	    il++;
	    iw++;
	    ll--;
	    lw--;
	    bc--;

	    if (bc <= 0 && bplp) {
		*bplp = nw - rw;
		bplp = NULL;
	    }
	    continue;
	}
	for (ms = mstack; ms; ms = ms->next) {
	    for (mp = ms->matcher; mp; mp = mp->next) {
		t = 1;
		/* Try to match the prefix, if any. */
		if (mp->flags & CMF_LEFT) {
		    if (il < mp->lalen || iw < mp->lalen)
			t = 0;
		    else if (mp->left)
			t = pattern_match(mp->left, l - mp->lalen, NULL, NULL) &&
			    pattern_match(mp->left, w - mp->lalen, NULL, NULL);
		    else
			t = (!il && !iw);
		}
		if (t) {
		    /* Now match the line pattern. */
		    if (ll < mp->llen || lw < mp->wlen)
			t = 0;
		    else if (mp->wlen < 0) {
			/* This is reached, if we have a `*' pattern. */
			if ((t = pattern_match(mp->line, l, NULL, NULL))) {
			    if (mp->flags & CMF_RIGHT) {
				if (mp->right && ll >= mp->llen + mp->ralen)
				    t = pattern_match(mp->right, l + mp->llen,
						      NULL, NULL);
				else
				    t = 0;
			    }
			    if (t && !stl) {
				/* We simply keep the current position   *
				 * and start collecting characters until *
				 * another matcher matches. */
				std = (mp->flags & CMF_LEFT);
				stl = l;
				stil = il;
				stw = w;
				stiw = iw;
				stm = mp;
				t = 0;
				l += mp->llen;
				il += mp->llen;
				ll -= mp->llen;
				
				break;
			    }
			    else
				t = 0;
			}
		    } else {
			/* No `*', just try to match the line and word *
			 * patterns. */
			t = pattern_match(mp->line, l, NULL, ea) &&
			    pattern_match(mp->word, w, ea, NULL);
			mlw = mp->wlen;
		    }
		}
		/* Now test the right anchor, if any. */
		if (t && (mp->flags & CMF_RIGHT)) {
		    if (ll < mp->llen + mp->ralen || lw < mlw + mp->ralen)
			t = 0;
		    else if (mp->right)
			t = pattern_match(mp->right, l + mp->llen, NULL, NULL) &&
			    pattern_match(mp->right, w + mlw, NULL, NULL);
		    else
			t = 0;
		}
		if (t) {
		    /* If it matched, build a new chunk on the Cline list *
		     * and add the string to the built match. */
		    if (stl) {
			int sl = iw - stiw;
			
			nw = addtoword(&rw, &rwlen, nw, stm, stl, stw, sl, 0);
			
			addtocline(nlp, &lr, 
				   stl, stm->llen, stw, sl, stm,
				   (std ? CLF_SUF : 0));
			
			stl = NULL;

			if (bc <= 0 && bplp) {
			    *bplp = nw - rw;
			    bplp = NULL;
			}
		    }
		    nw = addtoword(&rw, &rwlen, nw, mp, l, w, mlw, 0);
		    
		    addtocline(nlp, &lr, l, mp->llen, w, mlw, mp, 0);
		    
		    l += mp->llen;
		    w += mlw;
		    ll -= mp->llen;
		    lw -= mlw;
		    il += mp->llen;
		    iw += mlw;
		    bc -= mp->llen;

		    if (bc <= 0 && bplp) {
			*bplp = nw - rw;
			bplp = NULL;
		    }
		    break;
		}
	    }
	    if (mp)
		break;
	}
	if (!stl && !t) {
	    if (*nlp) {
		lr->next = freecl;
		freecl = *nlp;
	    }
	    return NULL;
	}
	if (stl) {
	    /* We are collecting characters, just skip over. */
	    w++;
	    lw--;
	    iw++;
	}
    }
    *lp = iw;
    if (lw) {
	/* There is a unmatched portion in the word, keep it. */
	if (rlp) {
	    w = dupstring(w);
	    addtocline(nlp, &lr, w, lw, w, -1, NULL, CLF_MID);

	    *rlp = lr;
	} else {
	    addtocline(nlp, &lr, l, 0, dupstring(w), lw, NULL, CLF_END);

	    nw = addtoword(&rw, &rwlen, nw, NULL, NULL, w, lw, 0);
	}
    }
    else if (rlp) {
	if (lr) {
	    lr->next = freecl;
	    freecl = *nlp;
	}
	return NULL;
    }
    if (nw)
	*nw = '\0';

    if (ll) {
	if (*nlp) {
	    lr->next = freecl;
	    freecl = *nlp;
	}
	return 0;
    }
    /* Finally, return the built match string. */
    return dupstring(rw);
}

/* Do the matching for a suffix. */

static char *
match_sfx(char *l, char *w, Cline *nlp, int *lp, int *bslp)
{
    static unsigned char *ea;
    static int ealen = 0;
    static char *rw;
    static int rwlen;

    int ll = strlen(l), lw = strlen(w), mlw;
    int il = 0, iw = 0, t, stil, stiw, std, bc = brsl;
    char *nw = rw, *stl = NULL, *stw;
    Cmlist ms;
    Cmatcher mp, stm;
    Cline lr = NULL;

    l += ll;
    w += lw;

    *nlp = NULL;

    if (ll > ealen) {
	if (ealen)
	    zfree(ea, ealen);
	ea = (unsigned char *) zalloc(ealen = ll + 20);
    }
    while (ll && lw) {
	if (l[-1] == w[-1]) {
	    if (stl) {
		int sl = iw - stiw;

		stl -= stm->llen;
		stw -= sl;
		nw = addtoword(&rw, &rwlen, nw, stm, stl, stw, sl, 1);

		addtocline(nlp, &lr, stl, stm->llen,
			   stw, sl, stm, (std ? CLF_SUF : 0));

		stl = NULL;

		if (bc <= 0 && bslp) {
		    *bslp = nw - rw;
		    bslp = NULL;
		}
	    }
	    nw = addtoword(&rw, &rwlen, nw, NULL, NULL, l - 1, 1, 1);

	    addtocline(nlp, &lr, l - 1, 1, NULL, 0, NULL, 0);

	    l--;
	    w--;
	    il++;
	    iw++;
	    ll--;
	    lw--;
	    bc--;
	    if (bc <= 0 && bslp) {
		*bslp = nw - rw;
		bslp = NULL;
	    }
	    continue;
	}
	for (ms = mstack; ms; ms = ms->next) {
	    for (mp = ms->matcher; mp; mp = mp->next) {
		t = 1;
		if (mp->flags & CMF_RIGHT) {
		    if (il < mp->ralen || iw < mp->ralen)
			t = 0;
		    else if (mp->right)
			t = pattern_match(mp->right, l, NULL, NULL) &&
			    pattern_match(mp->right, w, NULL, NULL);
		    else
			t = (!il && !iw);
		}
		if (t) {
		    if (ll < mp->llen || lw < mp->wlen)
			t = 0;
		    else if (mp->wlen < 0) {
			if ((t = pattern_match(mp->line, l - mp->llen,
					       NULL, NULL))) {
			    if (mp->flags & CMF_LEFT) {
				if (mp->left && ll >= mp->llen + mp->lalen)
				    t = pattern_match(mp->left,
						      l - mp->llen - mp->lalen,
						      NULL, NULL);
				else
				    t = 0;
			    }
			    if (t && !stl) {
				std = (mp->flags & CMF_LEFT);
				stl = l;
				stil = il;
				stw = w;
				stiw = iw;
				stm = mp;
				t = 0;
				l -= mp->llen;
				il += mp->llen;
				ll -= mp->llen;
				
				break;
			    }
			    else
				t = 0;
			}
		    } else {
			t = pattern_match(mp->line, l - mp->llen, NULL, ea) &&
			    pattern_match(mp->word, w - mp->wlen, ea, NULL);
			mlw = mp->wlen;
		    }
		}
		if (t && (mp->flags & CMF_LEFT)) {
		    if (ll < mp->llen + mp->lalen || lw < mlw + mp->lalen)
			t = 0;
		    else if (mp->left)
			t = pattern_match(mp->right, l - mp->llen - mp->lalen,
					  NULL, NULL) &&
			    pattern_match(mp->right, w - mlw - mp->lalen,
					  NULL, NULL);
		    else
			t = 0;
		}
		if (t) {
		    if (stl) {
			int sl = iw - stiw;
			
			stl -= stm->llen;
			stw -= sl;
			
			nw = addtoword(&rw, &rwlen, nw, stm, stl, stw, sl, 1);
			
			addtocline(nlp, &lr,
				   stl, stm->llen, stw, sl, stm,
				   (std ? CLF_SUF : 0));
			
			stl = NULL;

			if (bc <= 0 && bslp) {
			    *bslp = nw - rw;
			    bslp = NULL;
			}
		    }
		    nw = addtoword(&rw, &rwlen, nw, mp, l, w, mlw, 1);
		    
		    addtocline(nlp, &lr, l - mp->llen, mp->llen,
			       w - mlw, mlw, mp, 0);
		    
		    l -= mp->llen;
		    w -= mlw;
		    ll -= mp->llen;
		    lw -= mlw;
		    il += mp->llen;
		    iw += mlw;
		    bc -= mp->llen;
		    if (bc <= 0 && bslp) {
			*bslp = nw - rw;
			bslp = NULL;
		    }
		    break;
		}
	    }
	    if (mp)
		break;
	}
	if (!stl && !t) {
	    if (*nlp) {
		lr->next = freecl;
		freecl = *nlp;
	    }
	    return NULL;
	}
	if (stl) {
	    w--;
	    lw--;
	    iw++;
	}
    }
    *lp = iw;
    if (nw)
	*nw = '\0';

    if (ll) {
	if (*nlp) {
	    lr->next = freecl;
	    freecl = *nlp;
	}
	return 0;
    }
    return dupstring(rw);
}

/* Check if the word `w' matches. */

static char *
comp_match(char *pfx, char *sfx, char *w, Cline *clp, int qu, int *bpl, int *bsl)
{
    char *r = NULL;
    Cline pli;
    int pl;

    if (qu)
	w = quotename(w, NULL, NULL, NULL);
    if (*sfx) {
	char *p, *s;
	int sl;
	Cline sli, last;

	if ((p = match_pfx(pfx, w, &pli, &pl, &last, bpl))) {
	    if ((s = match_sfx(sfx, w + pl, &sli, &sl, bsl))) {
		int pml, sml;

		last->llen -= sl;
		last->next = revert_clines(sli);

		pml = strlen(p);
		sml = strlen(s);
		r = (char *) halloc(pml + sml + last->llen + 1);
		strcpy(r, p);
		strncpy(r + pml, last->line, last->llen);
		strcpy(r + pml + last->llen, s);
	    } else {
		last->next = freecl;
		freecl = pli;

		return NULL;
	    }
	}
	else
	    return NULL;
    } else if (!(r = match_pfx(pfx, w, &pli, &pl, NULL, bpl)))
	return NULL;

    if (lppre && *lppre) {
	Cline l, t = str_cline(lppre, -1, &l);

	l->next = pli;
	pli = t;
    }
    if (lpsuf && *lpsuf) {
	Cline n, t = str_cline(lpsuf, -1, NULL);

	if ((n = pli)) {
	    while (n->next) n = n->next;

	    n->next = t;
	} else
	    pli = t;
    }
    *clp = pli;

    return r;
}

/* Insert the given string into the command line.  If move is non-zero, *
 * the cursor position is changed and len is the length of the string   *
 * to insert (if it is -1, the length is calculated here).              */

/**/
static void
inststrlen(char *str, int move, int len)
{
    if (!len)
	return;
    if (len == -1)
	len = strlen(str);
    spaceinline(len);
    strncpy((char *)(line + cs), str, len);
    if (move)
	cs += len;
}

/* Insert the given match. This returns the number of characters inserted.*/

/**/
static int
instmatch(Cmatch m)
{
    int l, r = 0, ocs, a = cs;

    if (m->ipre) {
	inststrlen(m->ipre, 1, (l = strlen(m->ipre)));
	r += l;
    } 
    if (m->pre) {
	inststrlen(m->pre, 1, (l = strlen(m->pre)));
	r += l;
    }
    if (m->ppre) {
	inststrlen(m->ppre, 1, (l = strlen(m->ppre)));
	r += l;
    }
    inststrlen(m->str, 1, (l = strlen(m->str)));
    r += l;
    ocs = cs;
    if (brbeg && *brbeg) {
	cs = a + m->brpl + (m->pre ? strlen(m->pre) : 0);
	l = strlen(brbeg);
	inststrlen(brbeg, 1, l);
	r += l;
	ocs += l;
	cs = ocs;
    }
    if (m->psuf) {
	inststrlen(m->psuf, 1, (l = strlen(m->psuf)));
	r += l;
    }
    if (brend && *brend) {
	a = cs;
	cs -= m->brsl;
	ocs = cs;
	l = strlen(brend);
	inststrlen(brend, 1, l);
	r += l;
	cs = a + l;
    }
    if (m->suf) {
	inststrlen(m->suf, 1, (l = strlen(m->suf)));
	r += l;
    }
    cs = ocs;
    return r;
}


/* This adds a match to the list of matches.  The string to add is given   *
 * in s, the type of match is given in the global variable addwhat and     *
 * the parameter t (if not NULL) is a pointer to a hash node node which    *
 * may be used to give other information to this function.                 *
 *                                                                         *
 * addwhat contains either one of the special values (negative, see below) *
 * or the inclusive OR of some of the CC_* flags used for compctls.        */

/**/
static void
addmatch(char *s, char *t)
{
    int test = 0, sl = strlen(s), pl = rpl, cc = 0, isf = 0;
    int mpl = 0, msl = 0, bpl = brpl, bsl = brsl;
    char *e = NULL, *tt, *te, *fc, *ms = NULL;
    Comp cp = patcomp;
    HashNode hn;
    Param pm;
    LinkList l = matches;
    Cmatch cm;
    Cline lc = NULL;
    Aminfo ai = ainfo;

/*
 * addwhat: -5 is for files,
 *          -6 is for glob expansions,
 *          -8 is for executable files (e.g. command paths),
 *          -9 is for parameters
 *          -7 is for command names (from cmdnamtab)
 *          -4 is for a cdable parameter
 *          -3 is for executable command names.
 *          -2 is for anything unquoted
 *          -1 is for other file specifications
 *          (things with `~' of `=' at the beginning, ...).
 */

    /* Just to make the code cleaner */
    hn = (HashNode) t;
    pm = (Param) t;

    if (!addwhat) {
	test = 1;
    } else if (addwhat == -1 || addwhat == -5 || addwhat == -6 ||
	       addwhat == CC_FILES || addwhat == -7 || addwhat == -8) {
	if (sl < fpl + fsl)
	    return;

	if ((addwhat == CC_FILES ||
	     addwhat == -5) && !*psuf && !*fsuf) {
	    /* If this is a filename, do the fignore check. */
	    char **pt = fignore;
	    int filell;

	    for (test = 1; test && *pt; pt++)
		if ((filell = strlen(*pt)) < sl
		    && !strcmp(*pt, s + sl - filell))
		    test = 0;

	    if (!test) {
		l = fmatches;
		ai = fainfo;
	    }
	}
	pl = fpl;
	if (addwhat == -5 || addwhat == -8) {
	    test = 1;
	    cp = filecomp;
	    cc = cp || ispattern;
	    e = s + sl - fsl;
	    mpl = fpl; msl = fsl;
	} else {
	    if ((cp = filecomp)) {
		if ((test = domatch(s, filecomp, 0)))
		    cc = 1;
	    } else {
		e = s + sl - fsl;
		if ((test = !strncmp(s, fpre, fpl)))
		    if ((test = !strcmp(e, fsuf))) {
			mpl = fpl; msl = fsl;
		    }
		if (!test && mstack &&
		    (ms = comp_match(fpre, fsuf, s, &lc,
				     (addwhat == CC_FILES ||
				      addwhat == -6), &bpl, &bsl)))
		    test = 1;
		if (ispattern)
		    cc = 1;
	    }
	}
	if (test) {
	    fc = NULL;
	    if (addwhat == -7 && !(fc = findcmd(s)))
		return;
	    if (fc)
		zsfree(fc);
	    isf = CMF_FILE;

	    if (addwhat == CC_FILES || addwhat == -6 ||
		addwhat == -5 || addwhat == -8) {
		te = s + pl;
		s = quotename(s, &e, te, &pl);
		sl = strlen(s);
	    } else if (!cc) {
		s = dupstring(t = s);
		e += s - t;
	    }
	    if (cc) {
		tt = (char *)halloc(lppl + lpsl + sl + 1);
		tt[0] = '\0';
		if (lppre)
		    strcpy(tt, lppre);
		strcat(tt, s);
		if (lpsuf)
		    strcat(tt, lpsuf);
		untokenize(s = tt);
		sl = strlen(s);
	    }
	}
    } else if (addwhat == CC_QUOTEFLAG || addwhat == -2  ||
	      (addwhat == -3 && !(hn->flags & DISABLED)) ||
	      (addwhat == -4 && (PM_TYPE(pm->flags) == PM_SCALAR) &&
	       (tt = pm->gets.cfn(pm)) && *tt == '/')    ||
	      (addwhat == -9 && !(hn->flags & PM_UNSET)) ||
	      (addwhat > 0 &&
	       ((!(hn->flags & PM_UNSET) &&
		 (((addwhat & CC_ARRAYS)    &&  (hn->flags & PM_ARRAY))    ||
		  ((addwhat & CC_INTVARS)   &&  (hn->flags & PM_INTEGER))  ||
		  ((addwhat & CC_ENVVARS)   &&  (hn->flags & PM_EXPORTED)) ||
		  ((addwhat & CC_SCALARS)   &&  (hn->flags & PM_SCALAR))   ||
		  ((addwhat & CC_READONLYS) &&  (hn->flags & PM_READONLY)) ||
		  ((addwhat & CC_SPECIALS)  &&  (hn->flags & PM_SPECIAL))  ||
		  ((addwhat & CC_PARAMS)    && !(hn->flags & PM_EXPORTED)))) ||
		((( addwhat & CC_SHFUNCS)				  ||
		  ( addwhat & CC_BUILTINS)				  ||
		  ( addwhat & CC_EXTCMDS)				  ||
		  ( addwhat & CC_RESWDS)				  ||
		  ((addwhat & CC_ALREG)   && !(hn->flags & ALIAS_GLOBAL)) ||
		  ((addwhat & CC_ALGLOB)  &&  (hn->flags & ALIAS_GLOBAL))) &&
		 (((addwhat & CC_DISCMDS) && (hn->flags & DISABLED)) ||
		  ((addwhat & CC_EXCMDS)  && !(hn->flags & DISABLED)))) ||
		((addwhat & CC_BINDINGS) && !(hn->flags & DISABLED))))) {
	if (sl >= rpl + rsl || mstack) {
	    if (cp)
		test = domatch(s, patcomp, 0);
	    else {
		e = s + sl - rsl;
		if ((test = !strncmp(s, rpre, rpl)))
		    if ((test = !strcmp(e, rsuf))) {
			mpl = rpl; msl = rsl;
		    }
		if (!test && mstack &&
		    (ms = comp_match(rpre, rsuf, s, &lc,
				     (addwhat == CC_QUOTEFLAG), &bpl, &bsl)))
		    test = 1;
	    }
	}
	if (!test && sl < lpl + lsl && !mstack)
	    return;
	if (!test && lpre && lsuf) {
	    e = s + sl - lsl;
	    if ((test = !strncmp(s, lpre, lpl)))
		if ((test = !strcmp(e, lsuf))) {
		    mpl = lpl; msl = lsl;
		}
	    if (!test && mstack &&
		(ms = comp_match(lpre, lsuf, s, &lc,
				 (addwhat == CC_QUOTEFLAG), &bpl, &bsl)))
		test = 1;
	    pl = lpl;
	}
	if (addwhat == CC_QUOTEFLAG) {
	    te = s + pl;
	    s = quotename(s, &e, te, &pl);
	    sl = strlen(s);
	}
    }
    if (!test)
	return;

    if (!ms && !ispattern && ai->firstm) {
	if ((test = sl - pfxlen(ai->firstm->str, s)) < ai->prerest)
	    ai->prerest = test;
	if ((test = sfxlen(ai->firstm->str, s)) < ai->suflen)
	    ai->suflen = test;
    }

    /* Generate the common -P prefix. */

    if (ai->pprefix) {
	if (curcc->prefix)
	    ai->pprefix[pfxlen(ai->pprefix, curcc->prefix)] = '\0';
	else
	    ai->pprefix[0] = '\0';
    } else
	ai->pprefix = dupstring(curcc->prefix ? curcc->prefix : "");

    /* Generate the prefix to insert for ambiguous completions. */
    t = s;
    if (lppre)
	t = dyncat(lppre, t);
    if (ipre && *ipre) {
	Cline tlc = prepend_cline(ipre, lc);

	ai->noipre = 0;
	if (!ms) {
	    ai->icpl = lppl + mpl;
	    ai->icsl = lpsl + msl;
	    if (ai->iaprefix)
		ai->iaprefix[pfxlen(ai->iaprefix, t)] = '\0';
	    else
		ai->iaprefix = dupstring(t);
	}
	else
	    ai->ilinecl = join_clines(ai->ilinecl, lc);
	if (ai->iprefix) {
	    if (strcmp(ipre, ai->iprefix))
		ai->iprefix = "";
	} else
	    ai->iprefix = dupstring(ipre);

	t = dyncat(ipre, t);
	lc = tlc;
    } else
	ai->iprefix = "";

    if (!ms) {
	ai->cpl = lppl + mpl;
	ai->csl = lpsl + msl;
	if (ai->aprefix)
	    ai->aprefix[pfxlen(ai->aprefix, t)] = '\0';
	else
	    ai->aprefix = dupstring(t);
    }
    else
	ai->linecl = join_clines(ai->linecl, lc);

    mnum++;
    ai->count++;

    /* Allocate and fill the match structure. */
    cm = (Cmatch) halloc(sizeof(struct cmatch));
    if (ispattern) {
	if (lpsuf && *lpsuf && strsfx(lpsuf, s)) {
	    s[sl - lpsl] = '\0';
	    cm->psuf = lpsuf;
	}
	else
	    cm->psuf = NULL;

	if (lppre && *lppre && strpfx(lppre, s)) {
	    s += lppl;
	    cm->ppre = lppre;
	    cm->prpre = (isf && prpre && *prpre ? prpre : NULL);
	}
	else
	    cm->ppre = cm->prpre = NULL;
    }
    else {
	cm->ppre = (lppre && *lppre ? lppre : NULL);
	cm->psuf = (lpsuf && *lpsuf ? lpsuf : NULL);
	cm->prpre = (isf && prpre && *prpre ? prpre : NULL);
    }
    cm->str = (ms ? ms : s);
    cm->ipre = (ipre && *ipre ? ipre : NULL);
    cm->ripre = (ripre && *ripre ? ripre : NULL);
    cm->pre = curcc->prefix;
    cm->suf = curcc->suffix;
    cm->flags = mflags | isf;
    cm->brpl = bpl;
    cm->brsl = bsl;
    addlinknode(l, cm);

    /* One more match for this explanation. */
    if (expl) {
	if (l == matches)
	    expl->count++;
	else
	    expl->fcount++;
    }
    if (!ms) {
	if (!ai->firstm)
	    ai->firstm = cm;

	/* Do we have an exact match? More than one? */
	if (!ispattern && !(e - (s + pl))) {
	    if (!ai->exact)
		ai->exact = 1;
	    else {
		ai->exact = 2;
		cm = NULL;
	    }
	    ai->exactm = cm;
	}
    }
}

#ifdef HAVE_NIS_PLUS
static int
match_username(nis_name table, nis_object *object, void *userdata)
{
    if (errflag)
	return 1;
    else {
	static char buf[40];
	register entry_col *ec =
	    object->zo_data.objdata_u.en_data.en_cols.en_cols_val;
	register int l = minimum(ec->ec_value.ec_value_len, 39);

	memcpy(buf, ec->ec_value.ec_value_val, l);
	buf[l] = '\0';

	addmatch(dupstring(buf), NULL);
    }
    return 0;
}
#else
# ifdef HAVE_NIS
static int
match_username(int status, char *key, int keylen, char *val, int vallen, dopestring *data)
{
    if (errflag || status != YP_TRUE)
	return 1;

    if (vallen > keylen && val[keylen] == ':') {
	val[keylen] = '\0';
	addmatch(dupstring(val), NULL);
    }
    return 0;
}
# endif /* HAVE_NIS */
#endif  /* HAVE_NIS_PLUS */

/**/
static void
maketildelist(void)
{
#if defined(HAVE_NIS) || defined(HAVE_NIS_PLUS)
    FILE *pwf;
    char buf[BUFSIZ], *p;
    int skipping;

# ifndef HAVE_NIS_PLUS
    char domain[YPMAXDOMAIN];
    struct ypall_callback cb;
    dopestring data;

    data.s = fpre;
    data.len = fpl;
    /* Get potential matches from NIS and cull those without local accounts */
    if (getdomainname(domain, YPMAXDOMAIN) == 0) {
	cb.foreach = (int (*)()) match_username;
	cb.data = (char *)&data;
	yp_all(domain, PASSWD_MAP, &cb);
/*	for (n = firstnode(matches); n; incnode(n))
	    if (getpwnam(getdata(n)) == NULL)
		uremnode(matches, n);*/
    }
# else  /* HAVE_NIS_PLUS */
       /* Maybe we should turn this string into a #define'd constant...? */

    nis_list("passwd.org_dir", EXPAND_NAME|ALL_RESULTS|FOLLOW_LINKS|FOLLOW_PATH,
	     match_username, 0);
# endif
    /* Don't forget the non-NIS matches from the flat passwd file */
    if ((pwf = fopen(PASSWD_FILE, "r")) != NULL) {
	skipping = 0;
	while (fgets(buf, BUFSIZ, pwf) != NULL) {
	    if (strchr(buf, '\n') != NULL) {
		if (!skipping) {
		    if ((p = strchr(buf, ':')) != NULL) {
			*p = '\0';
			addmatch(dupstring(buf), NULL);
		    }
		} else
		    skipping = 0;
	    } else
		skipping = 1;
	}
	fclose(pwf);
    }
#else  /* no NIS or NIS_PLUS */
    /* add all the usernames to the named directory table */
    nameddirtab->filltable(nameddirtab);
#endif

    scanhashtable(nameddirtab, 0, (addwhat==-1) ? 0 : ND_USERNAME, 0,
	    addhnmatch, 0);
}

/* This does the check for compctl -x `n' and `N' patterns. */

/**/
static int
getcpat(char *wrd, int cpatindex, char *cpat, int class)
{
    char *str, *s, *t, *p;
    int d = 0;

    if (!wrd || !*wrd)
	return -1;

    cpat = rembslash(cpat);

    str = ztrdup(wrd);
    untokenize(str);
    if (!cpatindex)
	cpatindex++, d = 0;
    else if ((d = (cpatindex < 0)))
	cpatindex = -cpatindex;

    for (s = d ? str + strlen(str) - 1 : str;
	 d ? (s >= str) : *s;
	 d ? s-- : s++) {
	for (t = s, p = cpat; *t && *p; p++) {
	    if (class) {
		if (*p == *s && !--cpatindex) {
		    zsfree(str);
		    return (int)(s - str + 1);
		}
	    } else if (*t++ != *p)
		break;
	}
	if (!class && !*p && !--cpatindex) {
	    zsfree(str);
	    t += wrd - str;
	    for (d = 0; --t >= wrd;)
		if (! INULL(*t))
		    d++;
	    return d;
	}
    }
    zsfree(str);
    return -1;
}

/* Dump a hash table (without sorting).  For each element the addmatch  *
 * function is called and at the beginning the addwhat variable is set. *
 * This could be done using scanhashtable(), but this is easy and much  *
 * more efficient.                                                      */

/**/
static void
dumphashtable(HashTable ht, int what)
{
    HashNode hn;
    int i;

    addwhat = what;

    for (i = 0; i < ht->hsize; i++)
	for (hn = ht->nodes[i]; hn; hn = hn->next)
	    addmatch(hn->nam, (char *) hn);

}

/* ScanFunc used by maketildelist() et al. */

/**/
static void
addhnmatch(HashNode hn, int flags)
{
    addmatch(hn->nam, NULL);
}

/* Perform expansion on the given string and return the result. *
 * During this errors are not reported.                         */

/**/
static char *
getreal(char *str)
{
    LinkList l = newlinklist();
    int ne = noerrs;

    noerrs = 1;
    addlinknode(l, dupstring(str));
    prefork(l, 0);
    noerrs = ne;
    if (!errflag && nonempty(l))
	return dupstring(peekfirst(l));
    errflag = 0;

    return dupstring(str);
}

/* This reads a directory and adds the files to the list of  *
 * matches.  The parameters say which files should be added. */

/**/
static void
gen_matches_files(int dirs, int execs, int all)
{
    DIR *d;
    struct stat buf;
    char *n, p[PATH_MAX], *q = NULL, *e;
    LinkList l = NULL;
    int ns = 0, ng = opts[NULLGLOB], test, aw = addwhat;

    opts[NULLGLOB] = 1;

    if (*psuf) {
	/* If there is a path suffix, check if it doesn't have a `*' or *
	 * `)' at the end (this is used to determine if we should use   *
	 * globbing).                                                   */
	q = psuf + strlen(psuf) - 1;
	ns = !(*q == Star || *q == Outpar);
	l = newlinklist();
	/* And generate only directory names. */
	dirs = 1;
	all = execs = 0;
    }
    /* Open directory. */
    if ((d = opendir((prpre && *prpre) ? prpre : "."))) {
	/* If we search only special files, prepare a path buffer for stat. */
	if (!all && prpre) {
	    strcpy(p, prpre);
	    q = p + strlen(prpre);
	}
	/* Fine, now read the directory. */
	while ((n = zreaddir(d, 1)) && !errflag) {
	    /* Ignore files beginning with `.' unless the thing we found on *
	     * the command line also starts with a dot or GLOBDOTS is set.  */
	    if (*n != '.' || *fpre == '.' || isset(GLOBDOTS)) {
		addwhat = execs ? -8 : -5;
		if (filecomp)
		    /* If we have a pattern for the filename check, use it. */
		    test = domatch(n, filecomp, 0);
		else {
		    /* Otherwise use the prefix and suffix strings directly. */
		    e = n + strlen(n) - fsl;
		    if ((test = !strncmp(n, fpre, fpl)))
			test = !strcmp(e, fsuf);
		    if (!test && mstack) {
			test = 1;
			addwhat = CC_FILES;
		    }
		}
		/* Filename didn't match? */
		if (!test)
		    continue;
		if (!all) {
		    /* We still have to check the file type, so prepare *
		     * the path buffer by appending the filename.       */
		    strcpy(q, n);
		    /* And do the stat. */
		    if (stat(p, &buf) < 0)
			continue;
		}
		if (all ||
		    (dirs && S_ISDIR(buf.st_mode)) ||
		    (execs && S_ISREG(buf.st_mode) && (buf.st_mode&S_IXUGO))) {
		    /* If we want all files or the file has the right type... */
		    if (*psuf) {
			/* We have to test for a path suffix. */
			int o = strlen(p), tt;

			/* Append it to the path buffer. */
			strcpy(p + o, psuf);

			/* Do we have to use globbing? */
			if (ispattern || (ns && isset(GLOBCOMPLETE))) {
			    /* Yes, so append a `*' if needed. */
			    if (ns) {
				int tl = strlen(p);

				p[tl] = Star;
				p[tl + 1] = '\0';
			    }
			    /* Do the globbing... */
			    remnulargs(p);
			    addlinknode(l, p);
			    globlist(l);
			    /* And see if that produced a filename. */
			    tt = nonempty(l);
			    while (ugetnode(l));
			} else
			    /* Otherwise just check, if we have access *
			     * to the file.                            */
			    tt = !access(p, F_OK);

			p[o] = '\0';
			if (tt)
			    /* Ok, we can add the filename to the *
			     * list of matches.                   */
			    addmatch(dupstring(n), NULL);
		    } else
			/* We want all files, so just add the name *
			 * to the matches.                         */
			addmatch(dupstring(n), NULL);
		}
	    }
	}
	closedir(d);
    }
    opts[NULLGLOB] = ng;
    addwhat = aw;
}

/**/
static void
docompletion(char *s, int lst, int incmd)
{
    HEAPALLOC {
	pushheap();

	ainfo = fainfo = NULL;

	/* Make sure we have the completion list and compctl. */
	if (makecomplist(s, incmd)) {
	    /* Error condition: feeeeeeeeeeeeep(). */
	    feep();
	    goto compend;
	}

	if (lst == COMP_LIST_COMPLETE)
	    /* All this and the guy only wants to see the list, sigh. */
	    showinglist = -2;
	else {
	    /* We have matches. */
	    if (nmatches > 1)
		/* There are more than one match. */
		do_ambiguous();

	    else if (nmatches == 1) {
		/* Only one match. */
		do_single(amatches->matches[0]);
		invalidatelist();
	    }
	}

	/* Print the explanation strings if needed. */
	if (!showinglist && validlist && nmatches != 1) {
	    Cmgroup g = amatches;
	    Cexpl *e;
	    int up = 0, tr = 1;

	    if (!nmatches)
		feep();

	    while (g) {
		if ((e = g->expls))
		    while (*e) {
			if ((*e)->count) {
			    if (tr) {
				trashzle();
				tr = 0;
			    }
			    up += printfmt((*e)->str, (*e)->count, 1);
			}
			e++;
		    }
		g = g->next;
	    }
	    if (!tr) {
		clearflag = ((isset(USEZLE) && !termflags &&
			      (isset(ALWAYSLASTPROMPT) && zmult == 1)) ||
			     (unset(ALWAYSLASTPROMPT) && zmult != 1));

		if (clearflag && up + nlnct < lines)
		    tcmultout(TCUP, TCMULTUP, up + nlnct);
		else
		    putc('\n', shout);
		fflush(shout);
	    }
	}
      compend:
	ll = strlen((char *)line);
	if (cs > ll)
	    cs = ll;
	popheap();
    } LASTALLOC;
}

/* The beginning and end of a word range to be used by -l. */

static int brange, erange;

/* This is used to detect when and what to continue. */

static unsigned long ccont;

/* Create the completion list.  This is called whenever some bit of  *
 * completion code needs the list.  If the list is already available *
 * (validlist!=0), this function doesn't do anything.  Along with    *
 * the list is maintained the prefixes/suffixes etc.  When any of    *
 * this becomes invalid -- e.g. if some text is changed on the       *
 * command line -- invalidatelist() should be called, to set         *
 * validlist to zero and free up the memory used.  This function     *
 * returns non-zero on error.                                        */

/**/
static int
makecomplist(char *s, int incmd)
{
    struct cmlist ms;
    Cmlist m = cmatcher;

    /* If we already have a list from a previous execution of this *
     * function, skip the list building code.                      */
    if (validlist)
	return !nmatches;

    for (;;) {
	if (m) {
	    ms.next = NULL;
	    ms.matcher = m->matcher;
	    mstack = &ms;
	}
	ainfo = (Aminfo) hcalloc(sizeof(struct aminfo));
	fainfo = (Aminfo) hcalloc(sizeof(struct aminfo));

	ainfo->prerest = ainfo->suflen = 
	    fainfo->prerest = fainfo->suflen = 10000;
	ainfo->noipre = fainfo->noipre= 1;

	freecl = NULL;

	lastambig = 0;
	amatches = 0;
	mnum = 0;
	begcmgroup("default", 0);

	ccused = newlinklist();
	ccstack = newlinklist();

	makecomplistglobal(s, incmd);

	endcmgroup(NULL);

	if (amatches)
	    amatches->ccs = (Compctl *) makearray(ccused, 0,
						  &(amatches->ccount), NULL);
	else {
	    LinkNode n;

	    for (n = firstnode(ccused); n; incnode(n))
		freecompctl((Compctl) getdata(n));
	}

	PERMALLOC {
	    permmatches();
	} LASTALLOC;

	if (nmatches && !errflag) {
	    validlist = 1;

	    return 0;
	}
	if (!m || !(m = m->next))
	    break;

	errflag = 0;
    }
    return 1;
}

/* This function gets the compctls for the given command line and *
 * adds all completions for them. */

/**/
static void
makecomplistglobal(char *os, int incmd)
{
    Compctl cc;
    char *s;

    if (inwhat == IN_ENV)
        /* Default completion for parameter values. */
        cc = &cc_default;
    else if (inwhat == IN_MATH) {
        /* Parameter names inside mathematical expression. */
        cc_dummy.mask = CC_PARAMS;
	cc = &cc_dummy;
	cc_dummy.refc = 10000;
    } else if (inwhat == IN_COND) {
	/* We try to be clever here: in conditions we complete option   *
	 * names after a `-o', file names after `-nt', `-ot', and `-ef' *
	 * and file names and parameter names elsewhere.                */
	s = clwpos ? clwords[clwpos - 1] : "";
	cc_dummy.mask = !strcmp("-o", s) ? CC_OPTIONS :
	    ((*s == '-' && s[1] && !s[2]) ||
	     !strcmp("-nt", s) ||
	     !strcmp("-ot", s) ||
	     !strcmp("-ef", s)) ? CC_FILES :
	    (CC_FILES | CC_PARAMS);
	cc = &cc_dummy;
	cc_dummy.refc = 10000;
    } else if (linredir)
	/* In redirections use default completion. */
	cc = &cc_default;
    else {
	/* Otherwise get the matches for the command. */
	makecomplistcmd(os, incmd);
	cc = NULL;
    }
    if (cc) {
	/* First, use the -T compctl. */
	makecomplistcc(&cc_first, os, incmd);

	if (!(ccont & CC_CCCONT))
	    return;

	makecomplistcc(cc, os, incmd);
    }
}

/* This produces the matches for a command. */

/**/
static void
makecomplistcmd(char *os, int incmd)
{
    Compctl cc;
    Compctlp ccp;
    char *s;

    /* First, use the -T compctl. */
    makecomplistcc(&cc_first, os, incmd);

    if (!(ccont & CC_CCCONT))
	return;

    /* Then search the pattern compctls, with the command name and the *
     * full pathname of the command. */
    makecomplistpc(os, incmd);
    if (!(ccont & CC_CCCONT))
	return;

    /* If the command string starts with `=', try the path name of the *
     * command. */
    if (cmdstr && cmdstr[0] == Equals) {
	char *c = findcmd(cmdstr + 1);

	if (c) {
	    zsfree(cmdstr);
	    cmdstr = ztrdup(c);
	}
    }

    /* Find the compctl for this command, trying the full name and then *
     * the trailing pathname component. If that doesn't yield anything, *
     * use default completion. */
    if (incmd)
	cc = &cc_compos;
    else if (!(cmdstr &&
	  (((ccp = (Compctlp) compctltab->getnode(compctltab, cmdstr)) &&
	    (cc = ccp->cc)) ||
	   ((s = dupstring(cmdstr)) && remlpaths(&s) &&
	    (ccp = (Compctlp) compctltab->getnode(compctltab, s)) &&
	    (cc = ccp->cc)))))
	cc = &cc_default;

    makecomplistcc(cc, os, incmd);
}

/* This add the matches for the pattern compctls. */

/**/
static void
makecomplistpc(char *os, int incmd)
{
    Patcomp pc;
    Comp pat;
    char *s = findcmd(cmdstr);

    for (pc = patcomps; pc; pc = pc->next) {
	if ((pat = parsereg(pc->pat)) &&
	    (domatch(cmdstr, pat, 0) ||
	     (s && domatch(s, pat, 0)))) {
	    makecomplistcc(pc->cc, os, incmd);
	    if (!(ccont & CC_CCCONT))
		return;
	}
    }
}

/* This produces the matches for one compctl. */

/**/
static void
makecomplistcc(Compctl cc, char *s, int incmd)
{
    cc->refc++;
    addlinknode(ccused, cc);

    ccont = 0;

    makecomplistor(cc, s, incmd, 0, 0);
}

/* This adds the completions for one run of [x]or'ed completions. */

/**/
static void
makecomplistor(Compctl cc, char *s, int incmd, int compadd, int sub)
{
    int mn, ct, um = usemenu;

    /* Loop over xors. */
    do {
	mn = mnum;

	/* Loop over ors. */
	do {
	    /* Reset the range information if we are not in a sub-list. */
	    if (!sub) {
		brange = 0;
		erange = clwnum - 1;
	    }
	    usemenu = 0;
	    makecomplistlist(cc, s, incmd, compadd);
	    um |= usemenu;

	    ct = cc->mask2 & CC_XORCONT;

	    cc = cc->xor;
	} while (cc && ct);

	/* Stop if we got some matches. */
	if (mn != mnum)
	    break;
	if (cc) {
	    ccont &= ~(CC_DEFCONT | CC_PATCONT);
	    if (!sub)
		ccont &= ~CC_CCCONT;
	}
    } while (cc);

    usemenu = um;
}

/* This dispatches for simple and extended completion. */

/**/
static void
makecomplistlist(Compctl cc, char *s, int incmd, int compadd)
{
    int oloffs = offs, owe = we, owb = wb, ocs = cs;

    if (cc->ext)
	/* Handle extended completion. */
	makecomplistext(cc, s, incmd);
    else
	/* Only normal flags. */
	makecomplistflags(cc, s, incmd, compadd);

    /* Reset some information variables for the next try. */
    errflag = 0;
    offs = oloffs;
    wb = owb;
    we = owe;
    cs = ocs;
}

/* This add matches for extended completion patterns */

/**/
static void
makecomplistext(Compctl occ, char *os, int incmd)
{
    Compctl compc;
    Compcond or, cc;
    Comp comp;
    int compadd, m = 0, d = 0, t, tt, i, j, a, b;
    char *sc, *s, *ss;

    /* This loops over the patterns separated by `-'s. */
    for (compc = occ->ext; compc; compc = compc->next) {
	compadd = t = brange = 0;
	erange = clwnum - 1;
	/* This loops over OR'ed patterns. */
	for (cc = compc->cond; cc && !t; cc = or) {
	    or = cc->or;
	    /* This loops over AND'ed patterns. */
	    for (t = 1; cc && t; cc = cc->and) {
		/* And this loops over [...] pairs. */
		for (t = i = 0; i < cc->n && !t; i++) {
		    s = NULL;
		    brange = 0;
		    erange = clwnum - 1;
		    switch (cc->type) {
		    case CCT_POS:
			tt = clwpos;
			goto cct_num;
		    case CCT_NUMWORDS:
			tt = clwnum;
		    cct_num:
			if ((a = cc->u.r.a[i]) < 0)
			    a += clwnum;
			if ((b = cc->u.r.b[i]) < 0)
			    b += clwnum;
			if (cc->type == CCT_POS)
			    brange = a, erange = b;
			t = (tt >= a && tt <= b);
			break;
		    case CCT_CURSUF:
		    case CCT_CURPRE:
			s = ztrdup(clwpos < clwnum ? clwords[clwpos] : "");
			untokenize(s);
			sc = rembslash(cc->u.s.s[i]);
			a = strlen(sc);
			if (!strncmp(s, sc, a)) {
			    compadd = (cc->type == CCT_CURSUF ? a : 0);
			    t = 1;
			}
			break;
		    case CCT_CURSUB:
		    case CCT_CURSUBC:
			if (clwpos < 0 || clwpos > clwnum)
			    t = 0;
			else {
			    a = getcpat(clwords[clwpos],
					cc->u.s.p[i],
					cc->u.s.s[i],
					cc->type == CCT_CURSUBC);
			    if (a != -1)
				compadd = a, t = 1;
			}
			break;
			
		    case CCT_CURPAT:
		    case CCT_CURSTR:
			tt = clwpos;
			goto cct_str;
		    case CCT_WORDPAT:
		    case CCT_WORDSTR:
			tt = 0;
		    cct_str:
			if ((a = tt + cc->u.s.p[i]) < 0)
			    a += clwnum;
			s = ztrdup((a < 0 || a >= clwnum) ? "" :
				   clwords[a]);
			untokenize(s);
			
			if (cc->type == CCT_CURPAT ||
			    cc->type == CCT_WORDPAT) {
			    tokenize(ss = dupstring(cc->u.s.s[i]));
			    t = ((comp = parsereg(ss)) &&
				 domatch(s, comp, 0));
			} else
			    t = (!strcmp(s, rembslash(cc->u.s.s[i])));
			break;
		    case CCT_RANGESTR:
		    case CCT_RANGEPAT:
			if (cc->type == CCT_RANGEPAT)
			    tokenize(sc = dupstring(cc->u.l.a[i]));
			for (j = clwpos; j; j--) {
			    untokenize(s = ztrdup(clwords[j]));
			    if (cc->type == CCT_RANGESTR)
				sc = rembslash(cc->u.l.a[i]);
			    if (cc->type == CCT_RANGESTR ?
				!strncmp(s, sc, strlen(sc)) :
				((comp = parsereg(sc)) &&
				 domatch(s, comp, 0))) {
				zsfree(s);
				brange = j + 1;
				t = 1;
				break;
			    }
			    zsfree(s);
			}
			if (t && cc->u.l.b[i]) {
			    if (cc->type == CCT_RANGEPAT)
				tokenize(sc = dupstring(cc->u.l.b[i]));
			    for (j++; j < clwnum; j++) {
				untokenize(s = ztrdup(clwords[j]));
				if (cc->type == CCT_RANGESTR)
				    sc = rembslash(cc->u.l.b[i]);
				if (cc->type == CCT_RANGESTR ?
				    !strncmp(s, sc, strlen(sc)) :
				    ((comp = parsereg(sc)) &&
				     domatch(s, comp, 0))) {
				    zsfree(s);
				    erange = j - 1;
				    t = clwpos <= erange;
				    break;
				}
				zsfree(s);
			    }
			}
			s = NULL;
		    }
		    zsfree(s);
		}
	    }
	}
	if (t) {
	    /* The patterns matched, use the flags. */
	    m = 1;
	    ccont &= ~(CC_PATCONT | CC_DEFCONT);
	    makecomplistor(compc, os, incmd, compadd, 1);
	    if (!d && (ccont & CC_DEFCONT)) {
		d = 1;
		compadd = 0;
		brange = 0;
		erange = clwnum - 1;
		makecomplistflags(occ, os, incmd, 0);
	    }
	    if (!(ccont & CC_PATCONT))
		break;
	}
    }
    /* If no pattern matched, use the standard flags. */
    if (!m) {
	compadd = 0;
	brange = 0;
	erange = clwnum - 1;
	makecomplistflags(occ, os, incmd, 0);
    }
}

/* This returns the node with the given data. */
/* ...should probably be moved to linklist.c. */

static LinkNode
findnode(LinkList list, void *dat)
{
    LinkNode tmp = list->first;

    while (tmp && tmp->dat != dat) tmp = tmp->next;

    return tmp;
}

/* This adds the completions for the flags in the given compctl. */

/**/
static void
makecomplistflags(Compctl cc, char *s, int incmd, int compadd)
{
    int t, sf1, sf2, ooffs, um = usemenu, delit, ispar = 0;
    char *p, *sd = NULL, *tt, *s1, *s2, *os =  dupstring(s);
    struct cmlist ms;

    ccont |= (cc->mask2 & (CC_CCCONT | CC_DEFCONT | CC_PATCONT));

    if (findnode(ccstack, cc))
	return;

    addlinknode(ccstack, cc);

    if (allccs) {
	if (findnode(allccs, cc)) {
	    uremnode(ccstack, firstnode(ccstack));
	    return;
	}
	addlinknode(allccs, cc);
    }
    /* Go to the end of the word if complete_in_word is not set. */
    if (unset(COMPLETEINWORD) && cs != we)
	cs = we, offs = strlen(s);

    s = dupstring(s);
    delit = ispattern = 0;
    usemenu = um;
    patcomp = filecomp = NULL;
    menucur = NULL;
    rpre = rsuf = lpre = lsuf = ppre = psuf = lppre = lpsuf = prpre =
	fpre = fsuf = ipre = ripre = prpre = NULL;

    curcc = cc;

    mflags = 0;
    if (cc->ylist || cc->gname) {
	endcmgroup(NULL);
	begcmgroup((cc->ylist ? NULL : cc->gname), cc->mask2 & CC_NOSORT);
    }
    if (cc->mask & CC_REMOVE)
	mflags |= CMF_REMOVE;
    if (cc->mask2 & CC_NOSORT)
	mgroup->flags |= CGF_NOSORT;
    if (cc->explain) {
	expl = (Cexpl) halloc(sizeof(struct cexpl));
	expl->count = expl->fcount = 0;
    }
    else
	expl = NULL;
    /* compadd is the number of characters we have to ignore at the  *
     * beginning of the word.                                        */
    if (compadd) {
	ipre = dupstring(s);
	ipre[compadd] = '\0';
	untokenize(ipre);
	wb += compadd;
	s += compadd;
	if ((offs -= compadd) < 0) {
	    /* It's bigger than our word prefix, so we can't help here... */
	    uremnode(ccstack, firstnode(ccstack));
	    return;
	}
    }
    else
	ipre = NULL;

    if (cc->matcher) {
	ms.next = mstack;
	ms.matcher = cc->matcher;
	mstack = &ms;
    }
    /* Insert the prefix (compctl -P), if any. */
    if (cc->prefix) {
	int pl = 0;

	if (*s) {
	    /* First find out how much of the prefix is already on the line. */
	    sd = dupstring(s);
	    untokenize(sd);
	    pl = pfxlen(cc->prefix, sd);
	    s += pl;
	    sd += pl;
	    offs -= pl;
	}
    }
    /* Does this compctl have a suffix (compctl -S)? */
    if (cc->suffix) {
	char *sdup = dupstring(cc->suffix);
	int sl = strlen(sdup), suffixll;

	/* Ignore trailing spaces. */
	for (p = sdup + sl - 1; p >= sdup && *p == ' '; p--, sl--);
	p[1] = '\0';

	if (!sd) {
	    sd = dupstring(s);
	    untokenize(sd);
	}
	/* If the suffix is already there, ignore it (and don't add *
	 * it again).                                               */
	if (*sd && (suffixll = strlen(sd)) >= sl &&
	    offs <= suffixll - sl && !strcmp(sdup, sd + suffixll - sl))
	    s[suffixll - sl] = '\0';
    }
    /* Do we have one of the special characters `~' and `=' at the beginning? */
    if ((ic = *s) != Tilde && ic != Equals)
	ic = 0;

    /* Check if we have to complete a parameter name... */

    /* Try to find a `$'. */
    for (p = s + offs; p > s && *p != String; p--);
    if (*p == String) {
	/* Handle $$'s */
	while (p > s && p[-1] == String)
	    p--;
	while (p[1] == String && p[2] == String)
	    p += 2;
    }
    if (*p == String &&	p[1] != Inpar && p[1] != Inbrack) {
	/* This is really a parameter expression (not $(...) or $[...]). */
	char *b = p + 1, *e = b;
	int n = 0, br = 1;

	if (*b == Inbrace) {
	    /* If this is a ${...}, ignore the possible (...) flags. */
	    b++, br++;
	    n = skipparens(Inpar, Outpar, &b);
	}

	/* Ignore the stuff before the parameter name. */
	for (; *b; b++)
	    if (*b != '^' && *b != Hat &&
		*b != '=' && *b != Equals &&
		*b != '~' && *b != Tilde)
		break;
	if (*b == '#' || *b == Pound || *b == '+')
	    b++;

	e = b;
	/* Find the end of the name. */
	if (*e == Quest || *e == Star || *e == String || *e == Qstring ||
	    *e == '?'   || *e == '*'  || *e == '$'    ||
	    *e == '-'   || *e == '!'  || *e == '@')
	    e++;
	else if (idigit(*e))
	    while (idigit(*e))
		e++;
	else if (iident(*e))
	    while (iident(*e) ||
		   (useglob && (*e == Star || *e == Quest)))
		e++;

	/* Now make sure that the cursor is inside the name. */
	if (offs <= e - s && offs >= b - s && n <= 0) {
	    /* It is. */
	    if (br >= 2)
		mflags |= CMF_PARBR;

	    /* Get the prefix (anything up to the character before the name). */
	    lpsuf = dupstring(quotename(e, NULL, NULL, NULL));
	    *e = '\0';
	    lpsl = strlen(lpsuf);
	    ripre = dupstring(s);
	    ripre[b - s] = '\0';
	    ipre = dupstring(quotename(ripre, NULL, NULL, NULL));
	    untokenize(ipre);
	    ispar = 1;
	    /* And adjust wb, we, and offs again. */
	    offs -= b - s;
	    wb = cs - offs;
	    we = wb + e - b;
	    s = b;
	    /* And now make sure that we complete parameter names. */
	    cc = &cc_dummy;
	    cc_dummy.refc = 10000;
	    cc_dummy.mask = CC_PARAMS | CC_ENVVARS;
	}
    }
    ooffs = offs;
    /* If we have to ignore the word, do that. */
    if (cc->mask & CC_DELETE) {
	delit = 1;
	*s = '\0';
	offs = 0;
	if (isset(AUTOMENU)) usemenu = 1;
    }

    /* Compute line prefix/suffix. */
    lpl = offs;
    lpre = halloc(lpl + 1);
    memcpy(lpre, s, lpl);
    lpre[lpl] = '\0';
    lsuf = dupstring(s + offs);
    lsl = strlen(lsuf);

    /* First check for ~.../... */
    if (ic == Tilde) {
	for (p = lpre + lpl; p > lpre; p--)
	    if (*p == '/')
		break;

	if (*p == '/')
	    ic = 0;
    }
    /* Compute real prefix/suffix. */

    noreal = !delit;
    for (p = lpre; *p && *p != String && *p != Tick; p++);
    tt = ic && !ispar ? lpre + 1 : lpre;
    rpre = (*p || *lpre == Tilde || *lpre == Equals) ?
	(noreal = 0, getreal(tt)) :
	dupstring(tt);

    for (p = lsuf; *p && *p != String && *p != Tick; p++);
    rsuf = *p ? (noreal = 0, getreal(lsuf)) : dupstring(lsuf);

    /* Check if word is a pattern. */

    for (s1 = NULL, sf1 = 0, p = rpre + (rpl = strlen(rpre)) - 1;
	 p >= rpre && (ispattern != 3 || !sf1);
	 p--)
	if (itok(*p) && (p > rpre || (*p != Equals && *p != Tilde)))
	    ispattern |= sf1 ? 1 : 2;
	else if (*p == '/') {
	    sf1++;
	    if (!s1)
		s1 = p;
	}
    for (s2 = NULL, sf2 = t = 0, p = rsuf; *p && (!t || !sf2); p++)
	if (itok(*p))
	    t |= sf2 ? 4 : 2;
	else if (*p == '/') {
	    sf2++;
	    if (!s2)
		s2 = p;
	}
    ispattern = ispattern | t;

    /* But if we were asked not to do glob completion, we never treat the *
     * thing as a pattern.                                                */
    if (!useglob)
	ispattern = 0;

    if (ispattern) {
	/* The word should be treated as a pattern, so compute the matcher. */
	p = (char *)ncalloc(rpl + rsl + 2);
	strcpy(p, rpre);
	if (rpl && p[rpl - 1] != Star) {
	    p[rpl] = Star;
	    strcpy(p + rpl + 1, rsuf);
	} else
	    strcpy(p + rpl, rsuf);
	patcomp = parsereg(p);
    }
    if (!patcomp) {
	untokenize(rpre);
	untokenize(rsuf);

	rpl = strlen(rpre);
	rsl = strlen(rsuf);
    }
    untokenize(lpre);
    untokenize(lsuf);

    /* Handle completion of files specially (of course). */

    if ((cc->mask & (CC_FILES | CC_DIRS | CC_COMMPATH)) || cc->glob) {
	/* s1 and s2 point to the last/first slash in the prefix/suffix. */
	if (!s1)
	    s1 = rpre;
	if (!s2)
	    s2 = rsuf + rsl;

	/* Compute the path prefix/suffix. */
	if (*s1 != '/')
	    ppre = "";
	else
	    ppre = dupstrpfx(rpre, s1 - rpre + 1);
	psuf = dupstring(s2);

	if (cs != wb) {
	    char save = line[cs];

	    line[cs] = 0;
	    lppre = dupstring((char *) (line + wb));
	    line[cs] = save;
	    if ((p = strrchr(lppre, '/'))) {
		p[1] = '\0';
		lppl = strlen(lppre);
	    } else {
		lppre = NULL;
		lppl = 0;
	    }
	}
	else {
	    lppre = NULL;
	    lppl = 0;
	}
	if (cs != we) {
	    char save = line[we];

	    line[we] = 0;
	    lpsuf = strchr(dupstring((char *) (line + cs)), '/');
	    line[we] = save;
	    lpsl = (lpsuf ? strlen(lpsuf) : 0);
	}
	else {
	    lpsuf = NULL;
	    lpsl = 0;
	}

	/* And get the file prefix. */
	fpre = dupstring(((s1 == s || s1 == rpre || ic) &&
			  (*s != '/' || cs == wb)) ? s1 : s1 + 1);
	/* And the suffix. */
	fsuf = dupstrpfx(rsuf, s2 - rsuf);

	if (useglob && (ispattern & 2)) {
	    int t2;

	    /* We have to use globbing, so compute the pattern from *
	     * the file prefix and suffix with a `*' between them.  */
	    p = (char *)ncalloc((t2 = strlen(fpre)) + strlen(fsuf) + 2);
	    strcpy(p, fpre);
	    if ((!t2 || p[t2 - 1] != Star) && *fsuf != Star)
		p[t2++] = Star;
	    strcpy(p + t2, fsuf);
	    filecomp = parsereg(p);
	}
	if (!filecomp) {
	    untokenize(fpre);
	    untokenize(fsuf);

	    fpl = strlen(fpre);
	    fsl = strlen(fsuf);
	}
	addwhat = -1;

	/* Completion after `~', maketildelist adds the usernames *
	 * and named directories.                                 */
	if (ic == Tilde)
	    maketildelist();
	else if (ic == Equals) {
	    /* Completion after `=', get the command names from *
	     * the cmdnamtab and aliases from aliastab.         */
	    if (isset(HASHLISTALL))
		cmdnamtab->filltable(cmdnamtab);
	    dumphashtable(cmdnamtab, -7);
	    dumphashtable(aliastab, -2);
	} else {
	    /* Normal file completion... */
	    if (ispattern & 1) {
		/* But with pattern matching. */
		LinkList l = newlinklist();
		LinkNode n;
		int ng = opts[NULLGLOB];

		opts[NULLGLOB] = 1;

		addwhat = 0;
		p = (char *)ncalloc(lpl + lsl + 3);
		strcpy(p, lpre);
		if (*lsuf != '*' && *lpre && lpre[lpl - 1] != '*')
		    strcat(p, "*");
		strcat(p, lsuf);
		if (*lsuf && lsuf[lsl - 1] != '*' && lsuf[lsl - 1] != ')')
		    strcat(p, "*");

		/* Do the globbing. */
		tokenize(p);
		remnulargs(p);
		addlinknode(l, p);
		globlist(l);

		if (nonempty(l)) {
		    /* And add the resulting words. */
		    mflags |= CMF_FILE;
		    for (n = firstnode(l); n; incnode(n))
			addmatch(getdata(n), NULL);
		    mflags &= !CMF_FILE;
		}
		opts[NULLGLOB] = ng;
	    } else {
		char **dirs = 0, *ta[2];

		/* No pattern matching. */
		addwhat = CC_FILES;

		if (cc->withd) {
		    char **pp, **npp, *tp;
		    int tl = strlen(ppre) + 2, pl;

		    if ((pp = get_user_var(cc->withd))) {
			dirs = npp =
			    (char**) halloc(sizeof(char *)*(arrlen(pp)+1));
			while (*pp) {
			    pl = strlen(*pp);
			    tp = (char *) halloc(strlen(*pp) + tl);
			    strcpy(tp, *pp);
			    tp[pl] = '/';
			    strcpy(tp + pl + 1, ppre);
			    *npp++ = tp;
			    pp++;
			}
			*npp = '\0';
		    }
		}
		if (!dirs) {
		    dirs = ta;
		    if (cc->withd) {
			char *tp;
			int pl = strlen(cc->withd);

			ta[0] = tp = (char *) halloc(strlen(ppre) + pl + 2);
			strcpy(tp, cc->withd);
			tp[pl] = '/';
			strcpy(tp + pl + 1, ppre);
		    } else
			ta[0] = ppre;
		    ta[1] = NULL;
		}
		while (*dirs) {
		    prpre = *dirs;

		    if (sf2)
			/* We are in the path, so add only directories. */
			gen_matches_files(1, 0, 0);
		    else {
			if (cc->mask & CC_FILES)
			    /* Add all files. */
			    gen_matches_files(0, 0, 1);
			else if (cc->mask & CC_COMMPATH) {
			    /* Completion of command paths. */
			    if (sf1 || cc->withd)
				/* There is a path prefix, so add *
				 * directories and executables.   */
				gen_matches_files(1, 1, 0);
			    else {
				/* No path prefix, so add the things *
				 * reachable via the PATH variable.  */
				char **pc = path, *pp = prpre;

				for (; *pc; pc++)
				    if (!**pc || (pc[0][0] == '.' && !pc[0][1]))
					break;
				if (*pc) {
				    prpre = "./";
				    gen_matches_files(1, 1, 0);
				    prpre = pp;
				}
			    }
			} else if (cc->mask & CC_DIRS)
			    gen_matches_files(1, 0, 0);
			/* The compctl has a glob pattern (compctl -g). */
			if (cc->glob) {
			    int ns, pl = strlen(prpre), o;
			    char *g = dupstring(cc->glob), pa[PATH_MAX];
			    char *p2, *p3;
			    int ne = noerrs, md = opts[MARKDIRS];

			    /* These are used in the globbing code to make *
			     * things a bit faster.                        */
			    if (ispattern || mstack)
				glob_pre = glob_suf = NULL;
			    else {
				glob_pre = fpre;
				glob_suf = fsuf;
			    }
			    noerrs = 1;
			    addwhat = -6;
			    strcpy(pa, prpre);
			    o = strlen(pa);
			    opts[MARKDIRS] = 0;

			    /* The compctl -g string may contain more than *
			     * one pattern, so we need a loop.             */
			    while (*g) {
				LinkList l = newlinklist();
				int ng;

				/* Find the blank terminating the pattern. */
				while (*g && inblank(*g))
				    g++;
				/* Oops, we already reached the end of the
				   string. */
				if (!*g)
				    break;
				for (p = g + 1; *p && !inblank(*p); p++)
				    if (*p == '\\' && p[1])
					p++;
				/* Get the pattern string. */
				tokenize(g = dupstrpfx(g, p - g));
				if (*g == '=')
				    *g = Equals;
				if (*g == '~')
				    *g = Tilde;
				remnulargs(g);
				if ((*g == Equals || *g == Tilde) && !cc->withd) {
				/* The pattern has a `~' or `=' at the  *
				 * beginning, so we expand this and use *
				 * the result.                          */
				    filesub(&g, 0);
				    addlinknode(l, dupstring(g));
				} else if (*g == '/' && !cc->withd)
				/* The pattern is a full path (starting *
				 * with '/'), so add it unchanged.      */
				    addlinknode(l, dupstring(g));
				else {
				/* It's a simple pattern, so append it to *
				 * the path we have on the command line.  */
				    strcpy(pa + o, g);
				    addlinknode(l, dupstring(pa));
				}
				/* Do the globbing. */
				ng = opts[NULLGLOB];
				opts[NULLGLOB] = 1;
				globlist(l);
				opts[NULLGLOB] = ng;
				/* Get the results. */
				if (nonempty(l) && peekfirst(l)) {
				    for (p2 = (char *)peekfirst(l); *p2; p2++)
					if (itok(*p2))
					    break;
				    if (!*p2) {
					if ((*g == Equals || *g == Tilde ||
					     *g == '/') || cc->withd) {
					    /* IF the pattern started with `~',  *
					     * `=', or `/', add the result only, *
					     * if it really matches what we have *
					     * on the line.                      *
					     * Do this if an initial directory   *
					     * was specified, too.               */
					    while ((p2 = (char *)ugetnode(l)))
						if (strpfx(prpre, p2))
						    addmatch(p2 + pl, NULL);
					} else {
					    /* Otherwise ignore the path we *
					     * prepended to the pattern.    */
					    while ((p2 = p3 =
						    (char *)ugetnode(l))) {
						for (ns = sf1; *p3 && ns; p3++)
						    if (*p3 == '/')
							ns--;

						addmatch(p3, NULL);
					    }
					}
				    }
				}
				pa[o] = '\0';
				g = p;
			    }
			    glob_pre = glob_suf = NULL;
			    noerrs = ne;
			    opts[MARKDIRS] = md;
			}
		    }
		    dirs++;
		}
		prpre = NULL;
	    }
	}
	lppre = lpsuf = NULL;
	lppl = lpsl = 0;
    }
    if (ic) {
	/* Now change the `~' and `=' tokens to the real characters so *
	 * that things starting with these characters will be added.   */
	rpre = dyncat((ic == Tilde) ? "~" : "=", rpre);
	rpl++;
    }
    if (!ic && (cc->mask & CC_COMMPATH) && !*ppre && !*psuf) {
	/* If we have to complete commands, add alias names, *
	 * shell functions and builtins too.                 */
	dumphashtable(aliastab, -3);
	dumphashtable(reswdtab, -3);
	dumphashtable(shfunctab, -3);
	dumphashtable(builtintab, -3);
	if (isset(HASHLISTALL))
	    cmdnamtab->filltable(cmdnamtab);
	dumphashtable(cmdnamtab, -3);
	/* And parameter names if autocd and cdablevars are set. */
	if (isset(AUTOCD) && isset(CDABLEVARS))
	    dumphashtable(paramtab, -4);
    }
    addwhat = (cc->mask & CC_QUOTEFLAG) ? -2 : CC_QUOTEFLAG;

    if (cc->mask & CC_NAMED)
	/* Add named directories. */
	dumphashtable(nameddirtab, addwhat);
    if (cc->mask & CC_OPTIONS)
	/* Add option names. */
	dumphashtable(optiontab, addwhat);
    if (cc->mask & CC_VARS)
	/* And parameter names. */
	dumphashtable(paramtab, -9);
    if (cc->mask & CC_BINDINGS)
	/* And zle function names... */
	dumphashtable(thingytab, CC_BINDINGS);
    if (cc->keyvar) {
	/* This adds things given to the compctl -k flag *
	 * (from a parameter or a list of words).        */
	char **usr = get_user_var(cc->keyvar);

	if (usr)
	    while (*usr)
		addmatch(*usr++, NULL);
    }
    if (cc->mask & CC_USERS)
	/* Add user names. */
	maketildelist();
    if (cc->func) {
	/* This handles the compctl -K flag. */
	List list;
	char **r;
	int lv = lastval;

	/* Get the function. */
	if ((list = getshfunc(cc->func)) != &dummy_list) {
	    /* We have it, so build a argument list. */
	    LinkList args = newlinklist();

	    addlinknode(args, cc->func);

	    if (delit) {
		p = dupstrpfx(os, ooffs);
		untokenize(p);
		addlinknode(args, p);
		p = dupstring(os + ooffs);
		untokenize(p);
		addlinknode(args, p);
	    } else {
		addlinknode(args, lpre);
		addlinknode(args, lsuf);
	    }

	    /* This flag allows us to use read -l and -c. */
	    incompctlfunc = 1;
	    /* Call the function. */
	    doshfunc(list, args, 0, 1);
	    incompctlfunc = 0;
	    /* And get the result from the reply parameter. */
	    if ((r = get_user_var("reply")))
		while (*r)
		    addmatch(*r++, NULL);
	}
	lastval = lv;
    }
    if (cc->mask & (CC_JOBS | CC_RUNNING | CC_STOPPED)) {
	/* Get job names. */
	int i;
	char *j, *jj;

	for (i = 0; i < MAXJOB; i++)
	    if (jobtab[i].stat & STAT_INUSE) {
		int stopped = jobtab[i].stat & STAT_STOPPED;

		j = jj = dupstring(jobtab[i].procs->text);
		/* Find the first word. */
		for (; *jj; jj++)
		    if (*jj == ' ') {
			*jj = '\0';
			break;
		    }
		if ((cc->mask & CC_JOBS) ||
		    (stopped && (cc->mask & CC_STOPPED)) ||
		    (!stopped && (cc->mask & CC_RUNNING)))
		    addmatch(j, NULL);
	    }
    }
    if (cc->str) {
	/* Get the stuff from a compctl -s. */
	LinkList foo = newlinklist();
	LinkNode n;
	int first = 1, ng = opts[NULLGLOB], oowe = we, oowb = wb;
	char *tmpbuf;

	opts[NULLGLOB] = 1;

	/* Put the strin in the lexer buffer and call the lexer to *
	 * get the words we have to expand.                        */
	zleparse = 1;
	lexsave();
	tmpbuf = (char *)halloc(strlen(cc->str) + 5);
	sprintf(tmpbuf, "foo %s", cc->str); /* KLUDGE! */
	inpush(tmpbuf, 0, NULL);
	strinbeg();
	noaliases = 1;
	do {
	    ctxtlex();
	    if (tok == ENDINPUT || tok == LEXERR)
		break;
	    if (!first && tokstr && *tokstr)
		addlinknode(foo, ztrdup(tokstr));
	    first = 0;
	} while (tok != ENDINPUT && tok != LEXERR);
	noaliases = 0;
	strinend();
	inpop();
	errflag = zleparse = 0;
	lexrestore();
	/* Fine, now do full expansion. */
	prefork(foo, 0);
	if (!errflag) {
	    globlist(foo);
	    if (!errflag)
		/* And add the resulting words as matches. */
		for (n = firstnode(foo); n; incnode(n))
		    addmatch((char *)n->dat, NULL);
	}
	opts[NULLGLOB] = ng;
	we = oowe;
	wb = oowb;
    }
    if (cc->hpat) {
	/* We have a pattern to take things from the history. */
	Comp compc = NULL;
	char *e, *h, hpatsav;
	Histent he;
	int i = curhist - 1, n = cc->hnum;

	/* Parse the pattern, if it isn't the null string. */
	if (*(cc->hpat)) {
	    char *thpat = dupstring(cc->hpat);

	    tokenize(thpat);
	    compc = parsereg(thpat);
	}
	/* n holds the number of history line we have to search. */
	if (!n)
	    n = -1;

	/* Now search the history. */
	while (n-- && (he = quietgethist(i--))) {
	    int iwords;
	    for (iwords = 0; iwords < he->nwords; iwords++) {
		h = he->text + he->words[iwords*2];
		e = he->text + he->words[iwords*2+1];
		hpatsav = *e;
		*e = '\0';
		/* We now have a word from the history, ignore it *
		 * if it begins with a quote or `$'.              */
		if (*h != '\'' && *h != '"' && *h != '`' && *h != '$' &&
		    (!compc || domatch(h, compc, 0)))
		    /* Otherwise add it if it was matched. */
		    addmatch(dupstring(h), NULL);
		if (hpatsav)
		    *e = hpatsav;
	    }
	}
    }
    if ((t = cc->mask & (CC_ARRAYS | CC_INTVARS | CC_ENVVARS | CC_SCALARS |
			 CC_READONLYS | CC_SPECIALS | CC_PARAMS)))
	/* Add various flavours of parameters. */
	dumphashtable(paramtab, t);
    if ((t = cc->mask & CC_SHFUNCS))
	/* Add shell functions. */
	dumphashtable(shfunctab, t | (cc->mask & (CC_DISCMDS|CC_EXCMDS)));
    if ((t = cc->mask & CC_BUILTINS))
	/* Add builtins. */
	dumphashtable(builtintab, t | (cc->mask & (CC_DISCMDS|CC_EXCMDS)));
    if ((t = cc->mask & CC_EXTCMDS))
	/* Add external commands */
	dumphashtable(cmdnamtab, t | (cc->mask & (CC_DISCMDS|CC_EXCMDS)));
    if ((t = cc->mask & CC_RESWDS))
	/* Add reserved words */
	dumphashtable(reswdtab, t | (cc->mask & (CC_DISCMDS|CC_EXCMDS)));
    if ((t = cc->mask & (CC_ALREG | CC_ALGLOB)))
	/* Add the two types of aliases. */
	dumphashtable(aliastab, t | (cc->mask & (CC_DISCMDS|CC_EXCMDS)));

    if (!errflag && cc->ylist) {
	/* generate the user-defined display list: if anything fails, *
	 * we silently allow the normal completion list to be used.   */
	char **yaptr, *uv = NULL;
	List list;

	if (cc->ylist[0] == '$' || cc->ylist[0] == '(') {
	    /* from variable */
	    uv = cc->ylist + (cc->ylist[0] == '$');
	} else if ((list = getshfunc(cc->ylist)) != &dummy_list) {
	    /* from function:  pass completions as arg list */
	    LinkList args = newlinklist();
	    LinkNode ln;
	    Cmatch m;

	    addlinknode(args, cc->ylist);
	    for (ln = firstnode(matches); ln; ln = nextnode(ln)) {
		m = (Cmatch) getdata(ln);
		if (m->ppre) {
		    char *p = (char *) halloc(strlen(m->ppre) + strlen(m->str) +
					      strlen(m->psuf) + 1);

		    sprintf(p, "%s%s%s", m->ppre, m->str, m->psuf);
		    addlinknode(args, dupstring(p));
		}
		else
		    addlinknode(args, dupstring(m->str));
	    }

	    /* No harm in allowing read -l and -c here, too */
	    incompctlfunc = 1;
	    doshfunc(list, args, 0, 1);
	    incompctlfunc = 0;
	    uv = "reply";
	}
	
	if ((tt = cc->explain)) {
	    if (cc->mask & CC_EXPANDEXPL && !parsestr(tt = dupstring(tt))) {
		singsub(&tt);
		untokenize(tt);
	    }
	    expl->str = tt;
	    addexpl();
	}
	if (uv && (yaptr = get_user_var(uv)))
	    endcmgroup(yaptr);
	else
	    endcmgroup(NULL);
	begcmgroup("default", 0);
    }
    else if ((tt = cc->explain)) {
	if (cc->mask & CC_EXPANDEXPL && !parsestr(tt = dupstring(tt))) {
	    singsub(&tt);
	    untokenize(tt);
	}
	expl->str = tt;
	addexpl();
    }
    if (cc->subcmd) {
	/* Handle -l sub-completion. */
	char **ow = clwords, *os = cmdstr, *ops = NULL;
	int oldn = clwnum, oldp = clwpos;
	unsigned long occ = ccont;
	
	ccont = 0;
	
	/* So we restrict the words-array. */
	if (brange >= clwnum)
	    brange = clwnum - 1;
	if (brange < 1)
	    brange = 1;
	if (erange >= clwnum)
	    erange = clwnum - 1;
	if (erange < 1)
	    erange = 1;
	clwnum = erange - brange + 1;
	clwpos = clwpos - brange;
	
	if (cc->subcmd[0]) {
	    /* And probably put the command name given to the flag *
	     * in the array.                                       */
	    clwpos++;
	    clwnum++;
	    incmd = 0;
	    ops = clwords[brange - 1];
	    clwords[brange - 1] = cc->subcmd;
	    cmdstr = ztrdup(cc->subcmd);
	    clwords += brange - 1;
	} else {
	    cmdstr = ztrdup(clwords[brange]);
	    incmd = !clwpos;
	    clwords += brange;
	}
	/* Produce the matches. */
	makecomplistcmd(s, incmd);

	/* And restore the things we changed. */
	clwords = ow;
	zsfree(cmdstr);
	cmdstr = os;
	clwnum = oldn;
	clwpos = oldp;
	if (ops)
	    clwords[brange - 1] = ops;
	ccont = occ;
    }
    uremnode(ccstack, firstnode(ccstack));
    if (cc->matcher)
	mstack = mstack->next;
}

/* Invalidate the completion list. */

/**/
void
invalidatelist(void)
{
    if(showinglist == -2)
	listmatches();
    if(validlist)
	freematches();
    lastambig = menucmp = validlist = showinglist = 0;
    menucur = NULL;
    compwidget = NULL;
}

/* Get the words from a variable or a compctl -k list. */

/**/
static char **
get_user_var(char *nam)
{
    if (!nam)
	return NULL;
    else if (*nam == '(') {
	/* It's a (...) list, not a parameter name. */
	char *ptr, *s, **uarr, **aptr;
	int count = 0, notempty = 0, brk = 0;
	LinkList arrlist = newlinklist();

	ptr = dupstring(nam);
	s = ptr + 1;
	while (*++ptr) {
	    if (*ptr == '\\' && ptr[1])
		chuck(ptr), notempty = 1;
	    else if (*ptr == ',' || inblank(*ptr) || *ptr == ')') {
		if (*ptr == ')')
		    brk++;
		if (notempty) {
		    *ptr = '\0';
		    count++;
		    if (*s == '\n')
			s++;
		    addlinknode(arrlist, s);
		}
		s = ptr + 1;
		notempty = 0;
	    } else {
		notempty = 1;
		if(*ptr == Meta)
		    ptr++;
	    }
	    if (brk)
		break;
	}
	if (!brk || !count)
	    return NULL;
	*ptr = '\0';
	aptr = uarr = (char **)ncalloc(sizeof(char *) * (count + 1));

	while ((*aptr++ = (char *)ugetnode(arrlist)));
	uarr[count] = NULL;
	return uarr;
    } else {
	/* Otherwise it should be a parameter name. */
	char **arr = NULL, *val;
	if (!(arr = getaparam(nam)) && (val = getsparam(nam))) {
	    arr = (char **)ncalloc(2*sizeof(char *));
	    arr[0] = val;
	    arr[1] = NULL;
	}
	return arr;
    }

}

/* This is strcmp with ignoring backslashes. */

/**/
static int
strbpcmp(char **aa, char **bb)
{
    char *a = *aa, *b = *bb;

    while (*a && *b) {
	if (*a == '\\')
	    a++;
	if (*b == '\\')
	    b++;
	if (*a != *b)
	    return (int)(*a - *b);
	if (*a)
	    a++;
	if (*b)
	    b++;
    }
    return (int)(*a - *b);
}

/* The comparison function for matches (used for sorting). */

static int
matchcmp(Cmatch *a, Cmatch *b)
{
    return strbpcmp(&((*a)->str), &((*b)->str));
}

/* This tests, whether two matches are equal (would produce the same  *
 * strings on the command line). */

#define matchstreq(a, b) ((!(a) && !(b)) || ((a) && (b) && !strcmp((a), (b))))

static int
matcheq(Cmatch a, Cmatch b)
{
    return matchstreq(a->ipre, b->ipre) &&
	matchstreq(a->pre, b->pre) &&
	matchstreq(a->ppre, b->ppre) &&
	matchstreq(a->str, b->str) &&
	matchstreq(a->psuf, b->psuf) &&
	matchstreq(a->suf, b->suf);
}

/* Make an array from a linked list. The second argument says whether *
 * the array should be sorted. The third argument is used to return   *
 * the number of elements in the resulting array. The fourth argument *
 * is used to return the number of NOLIST elements. */

/**/
static Cmatch *
makearray(LinkList l, int s, int *np, int *nlp)
{
    Cmatch *ap, *bp, *cp, *rp;
    LinkNode nod;
    int n, nl = 0;

    /* Build an array for the matches. */
    rp = ap = (Cmatch *)ncalloc(((n = countlinknodes(l)) + 1) *
				sizeof(Cmatch));

    /* And copy them into it. */
    for (nod = firstnode(l); nod; incnode(nod))
	*ap++ = (Cmatch) getdata(nod);
    *ap = NULL;

    if (s == 1) {
	char **ap, **bp, **cp;

	/* Now sort the array (it contains strings). */
	qsort((void *) rp, n, sizeof(char *),
	      (int (*) _((const void *, const void *)))strbpcmp);

	/* And delete the ones that occur more than once. */
	for (ap = cp = (char **) rp; *ap; ap++) {
	    *cp++ = *ap;
	    for (bp = ap; bp[1] && !strcmp(*ap, bp[1]); bp++, n--);
	    ap = bp;
	}
	*cp = NULL;
    }
    else if (s) {
	/* Now sort the array (it contains matches). */
	qsort((void *) rp, n, sizeof(Cmatch),
	      (int (*) _((const void *, const void *)))matchcmp);

	/* And delete the ones that occur more than once. */
	for (ap = cp = rp; *ap; ap++) {
	    *cp++ = *ap;
	    for (bp = ap; bp[1] && matcheq(*ap, bp[1]); bp++, n--);
	    ap = bp;
	    /* Mark those, that would show the same string in the list. */
	    for (; bp[1] && !strcmp((*ap)->str, (bp[1])->str); bp++) {
		(bp[1])->flags |= CMF_NOLIST; nl++;
	    }
	}
	*cp = NULL;
    }
    if (np)
	*np = n;
    if (nlp)
	*nlp = nl;
    return rp;
}

/* This begins a new group of matches. */

/**/
static void
begcmgroup(char *n, int nu)
{
    if (n) {
	Cmgroup p = amatches;

	while (p) {
	    if (p->name && ((nu && !p->lallccs) || (!nu && p->lallccs)) &&
		!strcmp(n, p->name)) {
		mgroup = p;

		expls = p->lexpls;
		matches = p->lmatches;
		fmatches = p->lfmatches;
		allccs = p->lallccs;

		return;
	    }
	    p = p->next;
	}
    }
    mgroup = (Cmgroup) halloc(sizeof(struct cmgroup));
    mgroup->name = n;
    mgroup->flags = mgroup->lcount = mgroup->mcount = 0;
    mgroup->matches = NULL;
    mgroup->ylist = NULL;
    mgroup->expls = NULL;

    mgroup->lexpls = expls = newlinklist();
    mgroup->lmatches = matches = newlinklist();
    mgroup->lfmatches = fmatches = newlinklist();

    mgroup->lallccs = allccs = (nu ? NULL : newlinklist());

    mgroup->next = amatches;
    amatches = mgroup;
}

/* End the current group for now. */

/**/
static void
endcmgroup(char **ylist)
{
    mgroup->ylist = ylist;
}

/* Add an explanation string to the current group, joining duplicates. */

/**/
static void
addexpl(void)
{
    LinkNode n;
    Cexpl e;

    for (n = firstnode(expls); n; incnode(n)) {
	e = (Cexpl) getdata(n);
	if (!strcmp(expl->str, e->str)) {
	    e->count += expl->count;
	    e->fcount += expl->fcount;

	    return;
	}
    }
    addlinknode(expls, expl);
}

/* This duplicates one match. */

/**/
static Cmatch
dupmatch(Cmatch m)
{
    Cmatch r;

    r = (Cmatch) ncalloc(sizeof(struct cmatch));

    r->str = ztrdup(m->str);
    r->ipre = ztrdup(m->ipre);
    r->ripre = ztrdup(m->ripre);
    r->ppre = ztrdup(m->ppre);
    r->psuf = ztrdup(m->psuf);
    r->prpre = ztrdup(m->prpre);
    r->pre = m->pre;
    r->suf = m->suf;
    r->flags = m->flags;
    r->brpl = m->brpl;
    r->brsl = m->brsl;

    return r;
}

/* This duplicates all groups of matches. */

/**/
static void
permmatches(void)
{
    Cmgroup g = amatches, n;
    Cmatch *p, *q;
    Cexpl *ep, *eq, e, o;
    Compctl *cp, *cq;
    int nn, nl, fi = 0;

    amatches = lmatches = NULL;
    nmatches = smatches = 0;

    if (!ainfo->count) {
	ainfo = fainfo;
	fi = 1;
    }
    while (g) {
	HEAPALLOC {
	    if (empty(g->lmatches))
		/* We have no matches, try ignoring fignore. */
		g->lmatches = g->lfmatches;

	    g->matches = makearray(g->lmatches,
				   ((g->flags & CGF_NOSORT) ? 0 : 2),
				   &nn, &nl);
	    g->mcount = nn;
	    g->lcount = nn - nl;
	    if (g->ylist) 
		g->lcount = arrlen(g->ylist);

	    g->expls = (Cexpl *) makearray(g->lexpls, 0, &(g->ecount), NULL);

	    g->ccount = 0;
	    g->ccs = NULL;
	} LASTALLOC;

	nmatches += g->mcount;
	smatches += g->lcount;

	n = (Cmgroup) ncalloc(sizeof(struct cmgroup));

	if (!lmatches)
	    lmatches = n;
	if (amatches)
	    amatches->prev = n;
	n->next = amatches;
	amatches = n;
	n->prev = 0;

	n->flags = g->flags;
	n->mcount = g->mcount;
	n->matches = p = (Cmatch *) ncalloc((n->mcount + 1) *
					    sizeof(Cmatch));
	for (q = g->matches; *q; q++, p++)
	    *p = dupmatch(*q);
	*p = NULL;

	n->lcount = g->lcount;
	if (g->ylist)
	    n->ylist = arrdup(g->ylist);
	else
	    n->ylist = NULL;

	if ((n->ecount = g->ecount)) {
	    n->expls = ep = (Cexpl *) ncalloc((n->ecount + 1) *
					      sizeof(Cexpl));
	    for (eq = g->expls; (o = *eq); eq++, ep++) {
		*ep = e = (Cexpl) ncalloc(sizeof(struct cexpl));
		e->count = (fi ? o->fcount : o->count);
		e->str = ztrdup(o->str);
	    }
	    *ep = NULL;
	}
	else
	    n->expls = NULL;

	if ((n->ccount = g->ccount)) {
	    n->ccs = cp = (Compctl *) ncalloc((n->ccount + 1) *
					      sizeof(Compctl));
	    for (cq = g->ccs; *cq; cq++, cp++)
		*cp = *cq;
	    *cp = NULL;
	}
	else
	    n->ccs = NULL;
	g = g->next;
    }
}

/* This frees one match. */

/**/
static void
freematch(Cmatch m)
{
    if (!m) return;

    zsfree(m->str);
    zsfree(m->ipre);
    zsfree(m->ripre);
    zsfree(m->ppre);
    zsfree(m->psuf);
    zsfree(m->prpre);

    zfree(m, sizeof(m));
}

/* This frees the groups of matches. */

/**/
static void
freematches(void)
{
    Cmgroup g = amatches, n;
    Cmatch *m;
    Cexpl *e;
    Compctl *c;

    while (g) {
	n = g->next;
	
	for (m = g->matches; *m; m++)
	    freematch(*m);

	if (g->ylist)
	    freearray(g->ylist);

	if ((e = g->expls)) {
	    while (*e) {
		zsfree((*e)->str);
		free(*e);
		e++;
	    }
	    free(g->expls);
	}
	if ((c = g->ccs)) {
	    while (*c) {
		if (*c != &cc_dummy)
		    freecompctl(*c);
		c++;
	    }
	    free(g->ccs);
	}
	free(g);

	g = n;
    }
}

/* Handle the case were we found more than one match. */

/**/
static void
do_ambiguous(void)
{
    int p = (usemenu || ispattern), atend = (cs == we);
    int am = 0;

    menucmp = 0;

    /* If we have to insert the first match, call do_single().  This is *
     * how REC_EXACT takes effect.  We effectively turn the ambiguous   *
     * completion into an unambiguous one.                              */
    if (ainfo && ainfo->exact == 1 && isset(RECEXACT) &&
	(usemenu == 0 || unset(AUTOMENU))) {
	do_single(ainfo->exactm);
	invalidatelist();
	return;
    }
    /* Setting lastambig here means that the completion is ambiguous and *
     * AUTO_MENU might want to start a menu completion next time round,  *
     * but this might be overridden below if we can complete an          *
     * unambiguous prefix.                                               */
    lastambig = 1;

    if(p) {
	/* p is set if we are in a position to start using menu completion *
	 * due to one of the menu completion options, or due to the        *
	 * menu-complete-word command, or due to using GLOB_COMPLETE which *
	 * does menu-style completion regardless of the setting of the     *
	 * normal menu completion options.                                 */
	do_ambig_menu();
    } else {
	int ics = cs, ocs, pl = 0, l, lp, ls;
	char *ps;
	Cline lc;

	if (!ainfo)
	    return;

	fixsuffix();

	/* Delete the old stuff from the command line. */
	cs = wb;
	foredel(we - wb);

	/* Sort-of general case: we have an ambiguous completion, and aren't *
	 * starting menu completion or doing anything really weird.  We need *
	 * to insert any unambiguous prefix and suffix, if possible.         */

	if (ainfo->iprefix && *ainfo->iprefix) {
	    inststrlen(ainfo->iprefix, 1, -1);
	    inststrlen(ainfo->pprefix, 1, -1);
	    ps = ainfo->iaprefix;
	    lc = ainfo->ilinecl;
	    lp = ainfo->icpl;
	    ls = ainfo->icsl;
	} else {
	    if (ainfo->noipre && ainfo->pprefix) {
		pl = strlen(ainfo->pprefix);
		inststrlen(ainfo->pprefix, 1, pl);
	    }
	    ps = ainfo->aprefix;
	    lc = ainfo->linecl;
	    lp = ainfo->cpl;
	    ls = ainfo->csl;
	}
	if (lc) {
	    int sl = 0;

	    if (lp) {
		if (ls) {
		    if (ainfo->firstm->psuf)
			merge_cline(lc, ps, lp,
				    dyncat(ainfo->firstm->str,
					   ainfo->firstm->psuf),
				    ls, (sl = strlen(ainfo->firstm->psuf)));
		    else
			merge_cline(lc, ps, lp, ainfo->firstm->str, ls, 0);
		} else
		    merge_cline(lc, ps, lp, NULL, 0, 0);
	    }
	    inst_cline(lc, pl, sl);
	} else {
	    inststrlen(ps, 1, -1);
	    ocs = cs;
	    if (brbeg && *brbeg) {
		cs = wb + brpl + pl;
		l = strlen(brbeg);
		inststrlen(brbeg, 1, l);
		ocs += l;
		cs = ocs;
	    }
	    if(ainfo->suflen && !atend)
		inststrlen(ainfo->firstm->str +
			   strlen(ainfo->firstm->str) - ainfo->suflen, 1,
			   ainfo->suflen);
	    if (ainfo->firstm->psuf)
		inststrlen(ainfo->firstm->psuf, 0, -1);
	    if (brend && *brend) {
		cs -= brsl;
		inststrlen(brend, 1, -1);
	    }
	    cs = ocs;
	}
	/* If REC_EXACT and AUTO_MENU are set and what we inserted is an   *
	 * exact match, we want to start menu completion now. Otherwise    *
	 * on the next call to completion the inserted string would be     *
	 * taken as a match and no menu completion would be started.       */

	if (isset(RECEXACT) && !lc && !ainfo->prerest)
	    am = 1;

	/* If the LIST_AMBIGUOUS option (meaning roughly `show a list only *
	 * if the completion is completely ambiguous') is set, and some    *
	 * prefix was inserted, return now, bypassing the list-displaying  *
	 * code.  On the way, invalidate the list and note that we don't   *
	 * want to enter an AUTO_MENU imediately.                          */
	if(isset(LISTAMBIGUOUS) && !am &&
	   (ics != cs || (ainfo->suflen && !atend))) {
	    invalidatelist();
	    lastambig = 0;
	    return;
	}
    }
    /* At this point, we might want a completion listing.  Show the listing *
     * if it is needed.                                                     */
    if (isset(LISTBEEP))
	feep();
    if (isset(AUTOLIST) && !amenu && !showinglist)
	showinglist = -2;
    if (am)
	lastambig = 1;
}

/* This is a stat that ignores backslashes in the filename.  The `ls' *
 * parameter says if we have to do lstat() or stat().  I think this   *
 * should instead be done by use of a general function to expand a    *
 * filename (stripping backslashes), combined with the actual         *
 * (l)stat().                                                         */

/**/
static int
ztat(char *nam, struct stat *buf, int ls)
{
    char b[PATH_MAX], *p;

    for (p = b; p < b + sizeof(b) - 1 && *nam; nam++)
	if (*nam == '\\' && nam[1])
	    *p++ = *++nam;
	else
	    *p++ = *nam;
    *p = '\0';

    return ls ? lstat(b, buf) : stat(b, buf);
}

/* Insert a single match in the command line. */

/**/
static void
do_single(Cmatch m)
{
    int l;
    int havesuff = 0;

    char *str = m->str, *ppre = m->ppre, *psuf = m->psuf, *prpre = m->prpre;

    if (!prpre) prpre = "";
    if (!ppre) ppre = "";
    if (!psuf) psuf = "";

    fixsuffix();

    if (!menucur) {
	/* We are currently not in a menu-completion, *
	 * so set the position variables.             */
	menupos = wb;
	menuwe = (cs == we) || isset(ALWAYSTOEND);
	menuend = we;
    }
    /* If we are already in a menu-completion or if we have done a *
     * glob completion, we have to delete some of the stuff on the *
     * command line.                                               */
    if (menucur)
	l = menulen + menuinsc;
    else
	l = we - wb;

    menuinsc = 0;
    cs = menupos;
    foredel(l);

    /* And then we insert the new string. */
    menulen = instmatch(m);
    menuend = cs;
    cs = menupos + menulen;

    if (m->suf) {
	havesuff = 1;
	menuinsc = ztrlen(m->suf);
	if (menuwe && (m->flags & CMF_REMOVE)) {
	    makesuffix(menuinsc);
	    if (menuinsc == 1)
		suffixlen[m->suf[0]] = 1;
	}
    } else {
	/* There is no user-specified suffix, *
	 * so generate one automagically.     */
	if(m->ripre && (m->flags & CMF_PARBR)) {
	    /*{{*/
	    /* Completing a parameter in braces.  Add a removable `}' suffix. */
	    inststrlen("}", 1, 1);
	    menuinsc++;
	    if (menuwe)
		menuend++;
	}
	if((m->flags & CMF_FILE) || (m->ripre && isset(AUTOPARAMSLASH))) {
	    /* If we have a filename or we completed a parameter name      *
	     * and AUTO_PARAM_SLASH is set, lets see if it is a directory. *
	     * If it is, we append a slash.                                */
	    char *p;
	    struct stat buf;

	    /* Build the path name. */
	    if (ispattern || ic || m->ripre) {
		int ne = noerrs;

		noerrs = 1;

		if (m->ripre) {
		    int pl = strlen(m->ripre);

		    p = (char *) ncalloc(pl + strlen(str) + strlen(psuf) + 1);
		    sprintf(p, "%s%s%s", m->ripre, str, psuf);
		    if (pl && p[pl-1] == Inbrace)
			strcpy(p+pl-1, p+pl);
		}
		else if (ic) {
		    p = (char *) ncalloc(strlen(ppre) + strlen(str) +
					 strlen(psuf) + 2);
		    sprintf(p, "%c%s%s%s", ic, ppre, str, psuf);
		}
		else {
		    p = (char *) ncalloc(strlen(ppre) + strlen(str) +
					 strlen(psuf) + 1);
		}
		parsestr(p);
		if (ic)
		    *p = ic;
		singsub(&p);

		noerrs = ne;
	    } else {
		p = (char *) ncalloc(strlen(prpre) + strlen(str) +
				     strlen(psuf) + 3);
		sprintf(p, "%s%s%s", (prpre && *prpre) ? prpre : "./", str, psuf);
	    }
	    /* And do the stat. */
	    if (!ztat(p, &buf, 0) && S_ISDIR(buf.st_mode)) {
		/* It is a directory, so add the slash. */
		havesuff = 1;
		inststrlen("/", 1, 1);
		menuinsc++;
		if (menuwe)
		    menuend++;
		if ((!menucmp || menuwe) && isset(AUTOREMOVESLASH)) {
		    makesuffix(1);
		    suffixlen['/'] = 1;
		}
	    }
	}
    }
    /* If completing in a brace expansion... */
    if (brbeg) {
	if (havesuff) {
	    /*{{*/
	    /* If a suffix was added, and is removable, let *
	     * `,' and `}' remove it.                       */
	    if (isset(AUTOPARAMKEYS))
		suffixlen[','] = suffixlen['}'] = suffixlen[256];
	} else if (!menucmp) {
	    /*{{*/
	    /* Otherwise, add a `,' suffix, and let `}' remove it. */
	    cs = menuend;
	    havesuff = 1;
	    inststrlen(",", 1, 1);
	    menuinsc++;
	    makesuffix(1);
	    if (menuwe && isset(AUTOPARAMKEYS))
		suffixlen[','] = suffixlen['}'] = 1;
	}
    } else if (!menucmp && !havesuff) {
	/* If we didn't add a suffix, add a space, unless we are *
	 * doing menu completion.                                */
	inststrlen(" ", 1, 1);
	menuinsc++;
	if (menuwe)
	    makesuffix(1);
    }
    if (menuwe && m->ripre && isset(AUTOPARAMKEYS))
	makeparamsuffix(((m->flags & CMF_PARBR) ? 1 : 0), menuinsc);

    if (menucmp && !menuwe)
	cs = menuend;
}

/* This handles the beginning of menu-completion. */

/**/
static void
do_ambig_menu(void)
{
    menucmp = 1;
    menucur = NULL;
    menugrp = amatches;
    while (!menugrp->mcount)
	menugrp = menugrp->next;
    do_single(menugrp->matches[0]);
    menucur = menugrp->matches;
}

/* Return the length of the common prefix of s and t. */

/**/
int
pfxlen(char *s, char *t)
{
    int i = 0;

    while (*s && *s == *t)
	s++, t++, i++;
    return i;
}

/* Return the length of the common suffix of s and t. */

/**/
static int
sfxlen(char *s, char *t)
{
    if (*s && *t) {
	int i = 0;
	char *s2 = s + strlen(s) - 1, *t2 = t + strlen(t) - 1;

	while (s2 >= s && t2 >= t && *s2 == *t2)
	    s2--, t2--, i++;

	return i;
    } else
	return 0;
}

/* This is used to print the explanation string. *
 * It returns the number of lines printed.       */

/**/
static int
printfmt(char *fmt, int n, int dopr)
{
    char *p = fmt, nc[DIGBUFSIZE];
    int l = 0, cc = 0, b = 0, s = 0, u = 0, m;

    for (; *p; p++) {
	/* Handle the `%' stuff (%% == %, %n == <number of matches>). */
	if (*p == '%') {
	    if (*++p) {
		m = 0;
		switch (*p) {
		case '%':
		    if (dopr)
			putc('%', shout);
		    cc++;
		    break;
		case 'n':
		    sprintf(nc, "%d", n);
		    if (dopr)
			fprintf(shout, nc);
		    cc += strlen(nc);
		    break;
		case 'B':
		    b = 1;
		    tcout(TCBOLDFACEBEG);
		    break;
		case 'b':
		    b = 0; m = 1;
		    tcout(TCALLATTRSOFF);
		    break;
		case 'S':
		    s = 1;
		    tcout(TCSTANDOUTBEG);
		    break;
		case 's':
		    s = 0; m = 1;
		    tcout(TCSTANDOUTEND);
		    break;
		case 'U':
		    u = 1;
		    tcout(TCUNDERLINEBEG);
		    break;
		case 'u':
		    u = 0; m = 1;
		    tcout(TCUNDERLINEEND);
		    break;
		}
		if (m) {
		    if (b)
			tcout(TCBOLDFACEBEG);
		    if (s)
			tcout(TCSTANDOUTBEG);
		    if (u)
			tcout(TCUNDERLINEBEG);
		}
	    } else
		break;
	} else {
	    cc++;
	    if (*p == '\n') {
		l += 1 + (cc / columns);
		cc = 0;
	    }
	    if (dopr)
		putc(*p, shout);
	}
    }

    return l + (cc / columns);
}

/* This skips over matches that are not to be listed. */

static Cmatch *
skipnolist(Cmatch *p)
{
    while (*p && ((*p)->flags & CMF_NOLIST))
	p++;

    return p;
}

/* List the matches.  Note that the list entries are metafied. */

/**/
void
listmatches(void)
{
    Cmgroup g;
    Cmatch *p, m;
    Cexpl *e;
    int nlines = 0, ncols, colsz, ngr = 0, nlist = 0, longest = 1, pnl = 0;
    int of = isset(LISTTYPES), opl = 0;
    int listmax = getiparam("LISTMAX");

    if (smatches < 2) {
	showinglist = 0;
	return;
    }
#ifdef DEBUG
    /* Sanity check */
    if(!validlist) {
	showmsg("BUG: listmatches called with bogus list");
	return;
    }
#endif

    /* Set the cursor below the prompt. */
    trashzle();
    showinglist = 0;

    clearflag = (isset(USEZLE) && !termflags &&
		 (isset(ALWAYSLASTPROMPT) && zmult == 1)) ||
	(unset(ALWAYSLASTPROMPT) && zmult != 1);

    for (g = amatches; g; g = g->next) {
	char **pp = g->ylist;
	int nl = 0, l;

	if (pp) {
	    /* We have an ylist, lets see, if it contains newlines. */
	    while (!nl && *pp)
		nl = !!strchr(*pp++, '\n');

	    pp = g->ylist;
	    if (nl) {
		/* Yup, there are newlines, count lines. */
		char *nlptr, *sptr;

		g->flags |= CGF_LINES;
		
		while ((sptr = *pp)) {
		    while (sptr && *sptr) {
			nlines += (nlptr = strchr(sptr, '\n'))
			    ? 1 + (nlptr-sptr)/columns
			    : strlen(sptr)/columns;
			sptr = nlptr ? nlptr+1 : NULL;
		    }
		    nlines++;
		    pp++;
		}
		nlines--;
	    }
	    else {
		while (*pp) {
		    if ((l = strlen(*pp)) > longest)
			longest = l;
		    nlist++;
		    pp++;
		}
	    }
	}
	else {
	    for (p = g->matches; (m = *p); p++) {
		if (!(m->flags & CMF_NOLIST)) {
		    if ((l = niceztrlen(m->str)) > longest)
			longest = l;
		    nlist++;
		}
	    }
	}
	if ((e = g->expls)) {
	    while (*e) {
		if ((*e)->count)
		    nlines += 1 + printfmt((*e)->str, (*e)->count, 0);
		e++;
	    }
	}
	if (g->lcount)
	    ngr++;
    }
    longest += 2 + of;
    if ((ncols = (columns + 1) / longest)) {
	colsz = (nlist + ncols - 1) / ncols;
	nlines += ngr - 1 + colsz + (nlist == 0);
    } else {
	ncols = 1;
	colsz = 1;
	opl = 1;
	for (g = amatches; g; g = g->next) {
	    char **pp = g->ylist;

	    if (pp) {
		if (!(g->flags & CGF_LINES)) {
		    while (*pp) {
			nlines += 1 + (strlen(*pp) / columns);
			pp++;
		    }
		}
	    } else
		for (p = g->matches; (m = *p); p++)
		    if (!(m->flags & CMF_NOLIST))
			nlines += 1 + ((1 + niceztrlen(m->str)) / columns);
	}
    }

    /* Maybe we have to ask if the user wants to see the list. */
    if ((listmax && nlist > listmax) || (!listmax && nlines >= lines)) {
	int qup;
	zsetterm();
	qup = printfmt("zsh: do you wish to see all %n possibilities? ", nlist, 1);
	fflush(shout);
	if (getzlequery() != 'y') {
	    if (clearflag) {
		putc('\r', shout);
		tcmultout(TCUP, TCMULTUP, qup);
		if (tccan(TCCLEAREOD))
		    tcout(TCCLEAREOD);
		tcmultout(TCUP, TCMULTUP, nlnct);
	    } else
		putc('\n', shout);
	    return;
	}
	if (clearflag) {
	    putc('\r', shout);
	    tcmultout(TCUP, TCMULTUP, qup);
	    if (tccan(TCCLEAREOD))
		tcout(TCCLEAREOD);
	} else
	    putc('\n', shout);
	settyinfo(&shttyinfo);
    }

    /* Now print the matches. */
    g = amatches;
    while (g) {
	char **pp = g->ylist;

	if ((e = g->expls)) {
	    if (pnl) {
		putc('\n', shout);
		pnl = 0;
	    }
	    while (*e) {
		if ((*e)->count) {
		    printfmt((*e)->str, (*e)->count, 1);
		    putc('\n', shout);
		}
		e++;
	    }
	}
	if (pp) {
	    if (pnl) {
		putc('\n', shout);
		pnl = 0;
	    }
	    if (g->flags & CGF_LINES) {
		while (*pp) {
		    zputs(*pp, shout);
		    if (*++pp)
			putc('\n', shout);
		}
	    }
	    else {
		int n = g->lcount, nl = (n + ncols - 1) / ncols, i, a;
		int nc = (opl ? 1 : (n + colsz - 1) / colsz);
		char **pq;

		while (n && nl--) {
		    i = nc;
		    pq = pp;
		    while (n && i--) {
			if (pq - g->ylist >= g->lcount)
			    break;
			zputs(*pq, shout);
			if (i) {
			    a = longest - strlen(*pq);
			    while (a--)
				putc(' ', shout);
			}
			pq += colsz;
			n--;
		    }
		    if (n)
			putc('\n', shout);
		    pp++;
		}
	    }
	}
	else if (g->lcount) {
	    int n = g->lcount, nl = (n + ncols - 1) / ncols, i, j, a;
	    int nc = (opl ? 1 : (n + colsz - 1) / colsz);
	    Cmatch *q;

	    if (n && pnl) {
		putc('\n', shout);
		pnl = 0;
	    }
	    for (p = skipnolist(g->matches); n && nl--;) {
		i = nc;
		q = p;
		while (n && i--) {
		    if (!(m = *q))
			break;
		    nicezputs(m->str, shout);
		    if (i)
			a = longest - niceztrlen(m->str);

		    if (of && m->flags & CMF_FILE) {
			struct stat buf;
			char *pb;

			pb = (char *) halloc((m->prpre ? strlen(m->prpre) : 0) +
					     3 + strlen(m->str));
			sprintf(pb, "%s%s", (m->prpre ? m->prpre : "./"),
				m->str);

			if (ztat(pb, &buf, 1))
			    putc(' ', shout);
			else
			    putc(file_type(buf.st_mode), shout);

			a--;
		    }
		    if (i && !opl)
			while (a--)
			    putc(' ', shout);
		    if (--n)
			for (j = colsz; j && *q; j--)
			    q = skipnolist(q + 1);
		}
		if (n) {
		    putc('\n', shout);
		    p = skipnolist(p + 1);
		}
	    }
	}
	if (g->lcount)
	    pnl = 1;
	g = g->next;
    }

    if (clearflag) {
	/* Move the cursor up to the prompt, if always_last_prompt *
	 * is set and all that...                                  */
	if ((nlines += nlnct - 1) < lines) {
	    tcmultout(TCUP, TCMULTUP, nlines);
	    showinglist = -1;
	} else
	    clearflag = 0, putc('\n', shout);
    }
    else
	putc('\n', shout);
}

/* This is used to print expansions. */

/**/
void
listlist(LinkList l)
{
    struct cmgroup dg;
    Cmgroup am = amatches;
    int vl = validlist, sm = smatches;

    smatches = 1;
    validlist = 1;
    amatches = &dg;
    memset(&dg, 0, sizeof(struct cmgroup));
    dg.ylist = (char **) makearray(l, 1, &(dg.lcount), NULL);
    listmatches();

    amatches = am;
    validlist = vl;
    smatches = sm;
}

/* Expand the history references. */

/**/
int
doexpandhist(void)
{
    unsigned char *ol;
    int oll, ocs, ne = noerrs, err;

    DPUTS(useheap, "BUG: useheap in doexpandhist()");
    HEAPALLOC {
	pushheap();
	metafy_line();
	oll = ll;
	ocs = cs;
	ol = (unsigned char *)dupstring((char *)line);
	expanding = 1;
	excs = cs;
	ll = cs = 0;
	lexsave();
	/* We push ol as it will remain unchanged */
	inpush((char *) ol, 0, NULL);
	strinbeg();
	noaliases = 1;
	noerrs = 1;
	exlast = inbufct;
	do {
	    ctxtlex();
	} while (tok != ENDINPUT && tok != LEXERR);
	stophist = 2;
	while (!lexstop)
	    hgetc();
	/* We have to save errflags because it's reset in lexrestore. Since  *
	 * noerrs was set to 1 errflag is true if there was a habort() which *
	 * means that the expanded string is unusable.                       */
	err = errflag;
	noerrs = ne;
	noaliases = 0;
	strinend();
	inpop();
	zleparse = 0;
	lexrestore();
	expanding = 0;

	if (!err) {
	    cs = excs;
	    if (strcmp((char *)line, (char *)ol)) {
		unmetafy_line();
		/* For vi mode -- reset the beginning-of-insertion pointer   *
		 * to the beginning of the line.  This seems a little silly, *
		 * if we are, for example, expanding "exec !!".              */
		if (viinsbegin > findbol())
		    viinsbegin = findbol();
		popheap();
		LASTALLOC_RETURN 1;
	    }
	}

	strcpy((char *)line, (char *)ol);
	ll = oll;
	cs = ocs;
	unmetafy_line();

	popheap();
    } LASTALLOC;
    return 0;
}

/**/
void
magicspace(void)
{
    c = ' ';
    selfinsert();
    doexpandhist();
}

/**/
void
expandhistory(void)
{
    if (!doexpandhist())
	feep();
}

static int cmdwb, cmdwe;

/**/
static char *
getcurcmd(void)
{
    int curlincmd;
    char *s = NULL;

    DPUTS(useheap, "BUG: useheap in getcurcmd()");
    HEAPALLOC {
	zleparse = 2;
	lexsave();
	metafy_line();
	inpush(dupstrspace((char *) line), 0, NULL);
	unmetafy_line();
	strinbeg();
	pushheap();
	do {
	    curlincmd = incmdpos;
	    ctxtlex();
	    if (tok == ENDINPUT || tok == LEXERR)
		break;
	    if (tok == STRING && curlincmd) {
		zsfree(s);
		s = ztrdup(tokstr);
		cmdwb = ll - wordbeg;
		cmdwe = ll + 1 - inbufct;
	    }
	}
	while (tok != ENDINPUT && tok != LEXERR && zleparse);
	popheap();
	strinend();
	inpop();
	errflag = zleparse = 0;
	lexrestore();
    } LASTALLOC;
    return s;
}

/**/
void
processcmd(void)
{
    char *s;
    int m = zmult;

    s = getcurcmd();
    if (!s) {
	feep();
	return;
    }
    zmult = 1;
    pushline();
    zmult = m;
    inststr(bindk->nam);
    inststr(" ");
    untokenize(s);
    HEAPALLOC {
	inststr(quotename(s, NULL, NULL, NULL));
    } LASTALLOC;
    zsfree(s);
    done = 1;
}

/**/
void
expandcmdpath(void)
{
    int oldcs = cs, na = noaliases;
    char *s, *str;

    noaliases = 1;
    s = getcurcmd();
    noaliases = na;
    if (!s || cmdwb < 0 || cmdwe < cmdwb) {
	feep();
	return;
    }
    str = findcmd(s);
    zsfree(s);
    if (!str) {
	feep();
	return;
    }
    cs = cmdwb;
    foredel(cmdwe - cmdwb);
    spaceinline(strlen(str));
    strncpy((char *)line + cs, str, strlen(str));
    cs = oldcs;
    if (cs >= cmdwe - 1)
	cs += cmdwe - cmdwb + strlen(str);
    if (cs > ll)
	cs = ll;
    zsfree(str);
}

/* Extra function added by AR Iano-Fletcher. */
/* This is a expand/complete in the vein of wash. */

/**/
void
expandorcompleteprefix(void)
{
    comppref = 1;
    expandorcomplete();
    comppref = 0;
}
