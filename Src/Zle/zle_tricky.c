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
 * brpl and brsl, brbsl hold the offset of these strings.                 *
 * brpcs and brscs hold the positions of the re-inserted string in the    *
 * line.                                                                  */

static char *brbeg = NULL, *brend = NULL;
static int brpl, brsl, brbsl, brpcs, brscs;

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

/* The completion matchers used when building new stuff for the line. */

static Cmlist bmatchers;

/* A list with references to all matchers we used. */

static LinkList matchers;

/* A heap of free Cline structures. */

static Cline freecl;

/* Information for ambiguous completions. One for fignore ignored and   *
 * one for normal completion. */

typedef struct aminfo *Aminfo;

struct aminfo {
    int cpl, csl, icpl, icsl;	/* common prefix/suffix lengths           */
    int minlen;			/* minimum match length                   */
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

/* This contains the name of the function to call if this is for a new  *
 * style completion. */

static char *compfunc = NULL;

/* The memory heap to use for new style completion generation. */

static Heap compheap;

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
       COMP_LIST_COMPLETE,
       COMP_SPELL,
       COMP_EXPAND,
       COMP_EXPAND_COMPLETE,
       COMP_LIST_EXPAND };
#define COMP_ISEXPAND(X) ((X) >= COMP_EXPAND)

/* Non-zero if the last completion done was ambiguous (used to find   *
 * out if AUTOMENU should start).  More precisely, it's nonzero after *
 * successfully doing any completion, unless the completion was       *
 * unambiguous and did not cause the display of a completion list.    *
 * From the other point of view, it's nonzero iff AUTOMENU (if set)   *
 * should kick in on another completion.                              *
 *                                                                    *
 * If both AUTOMENU and BASHAUTOLIST are set, then we get a listing   *
 * on the second tab, a` la bash, and then automenu kicks in when     *
 * lastambig == 2.                                                    */

static int lastambig;

/* This says what of the state the line is in when completion is started *
 * came from a previous completion. If the FC_LINE bit is set, the       *
 * string was inserted. If FC_INWORD is set, the last completion moved   *
 * the cursor into the word although it was at the end of it when the    *
 * last completion was invoked.                                          *
 * This is used to detect if the string should be taken as an exact      *
 * match (see do_ambiguous()) and if the cursor has to be moved to the   *
 * end of the word before generating the completions.                    */

static int fromcomp;

/* This holds the end-position of the last string inserted into the line. */

static int lastend;

#define FC_LINE   1
#define FC_INWORD 2

/**/
void
completecall(void)
{
    compfunc = compwidget->u.comp.func;
    compwidget->u.comp.fn();
    compfunc = NULL;
}

/**/
void
completeword(void)
{
    usemenu = isset(MENUCOMPLETE);
    useglob = isset(GLOBCOMPLETE);
    if (c == '\t' && usetab())
	selfinsert();
    else {
	if (lastambig == 1 && isset(BASHAUTOLIST) && !usemenu && !menucmp) {
	    docomplete(COMP_LIST_COMPLETE);
	    lastambig = 2;
	} else
	    docomplete(COMP_COMPLETE);
    }
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
    else {
	if (lastambig == 1 && isset(BASHAUTOLIST) && !usemenu && !menucmp) {
	    docomplete(COMP_LIST_COMPLETE);
	    lastambig = 2;
	} else
	    docomplete(COMP_EXPAND_COMPLETE);
    }
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
    if (brbeg && *brbeg) {
	int l = (brscs >= 0 ? brscs : cs) - brpcs;

	zsfree(brbeg);
	brbeg = (char *) zalloc(l + 2);
	memcpy(brbeg, line + brpcs, l);
	brbeg[l] = ',';
	brbeg[l + 1] = '\0';
    } else {
	int sl = suffixlen[' '];

	cs = menupos + menulen + menuinsc;
	if (sl)
	    backdel(sl);

	inststrlen(" ", 1, 1);
	menuinsc = menulen = 0;
	menupos = cs;
	menuwe = 1;
    }
    menucomplete();
}

/* These are flags saying if we are completing in the command *
 * position or in a redirection.                              */

static int lincmd, linredir;

/* The string for the redirection operator. */

static char *rdstr;

/* This holds the name of the current command (used to find the right *
 * compctl).                                                          */

static char *cmdstr;

/* This hold the name of the variable we are working on. */

static char *varname;

/* != 0 if we are in a subscript */

static int insubscr;

/* 1 if we are completing in a string */

/**/
int instring;

/* Convenience macro for calling bslashquote() (formerly quotename()). *
 * This uses the instring variable above.                              */

#define quotename(s, e, te, pl) bslashquote(s, e, te, pl, instring)

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

    /* We may have to reset the cursor to its position after the   *
     * string inserted by the last completion. */

    if (fromcomp & FC_INWORD)
	cs = lastend;

    /* Check if we have to start a menu-completion (via automenu). */

    if ((amenu = (isset(AUTOMENU) && lastambig &&
		  (!isset(BASHAUTOLIST) || lastambig == 2))))
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
			struct hashnode *hn;

			for (t0 = cmdnamtab->hsize - 1; t0 >= 0; t0--)
			    for (hn = cmdnamtab->nodes[t0]; hn;
				 hn = hn->next) {
				if (strpfx(q, hn->nam) && findcmd(hn->nam, 0))
				    n++;
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
    int t0, tt0, i, j, k, cp, rd, sl, ocs, ins, oins, inarr, ia, parct;
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
	zsfree(varname);
	varname = NULL;
	insubscr = 0;
	zleparse = 1;
	clwpos = -1;
	lexsave();
	inpush(dupstrspace((char *) linptr), 0, NULL);
	strinbeg();
	stophist = 2;
	i = tt0 = cp = rd = ins = oins = inarr = parct = ia = 0;

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
	    lincmd = ((incmdpos && !ins) || (oins == 2 && i == 2) ||
		      (ins == 3 && i == 1));
	    linredir = (inredir && !ins);
	    oins = ins;
	    /* Get the next token. */
	    if (inarr)
		incmdpos = 0;
	    ctxtlex();
	    if (tok == ENVARRAY) {
		inarr = 1;
		zsfree(varname);
		varname = ztrdup(tokstr);
	    } else if (tok == INPAR)
		parct++;
	    else if (tok == OUTPAR) {
		if (parct)
		    parct--;
		else
		    inarr = 0;
	    }
	    if (inredir)
		rdstr = tokstrings[tok];
	    if (tok == DINPAR)
		tokstr = NULL;

	    /* We reached the end. */
	    if (tok == ENDINPUT)
		break;
	    if ((ins && (tok == DO || tok == SEPER)) ||
		(ins == 2 && i == 2) ||	(ins == 3 && i == 3) ||
		tok == BAR    || tok == AMPER     ||
		tok == BARAMP || tok == AMPERBANG ||
		((tok == DBAR || tok == DAMPER) && !incond)) {
		/* This is one of the things that separate commands.  If we  *
		 * already have the things we need (e.g. the token strings), *
		 * leave the loop.                                           */
		if (tt)
		    break;
		/* Otherwise reset the variables we are collecting data in. */
		i = tt0 = cp = rd = ins = 0;
	    }
	    if (lincmd && (tok == STRING || tok == FOR || tok == FOREACH ||
			   tok == SELECT || tok == REPEAT || tok == CASE)) {
		/* The lexer says, this token is in command position, so *
		 * store the token string (to find the right compctl).   */
		ins = (tok == REPEAT ? 2 : (tok != STRING));
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
		ia = inarr;
		if (inwhat == IN_NOTHING && incond)
		    inwhat = IN_COND;
	    } else if (linredir)
		continue;
	    if (!tokstr)
		continue;
	    /* Hack to allow completion after `repeat n do'. */
	    if (oins == 2 && !i && !strcmp(tokstr, "do"))
		ins = 3;
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
	if (ia) {
	    lincmd = linredir = 0;
	    inwhat = IN_ENV;
	} else {
	    lincmd = cp;
	    linredir = rd;
	}
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
	    char sav;
	    /* The cursor was inside a parameter assignment. */
	    for (s = tt; iident(*s); s++);
	    sav = *s;
	    *s = '\0';
	    zsfree(varname);
	    varname = ztrdup(tt);
	    *s = sav;
	    if (skipparens(Inbrack, Outbrack, &s) > 0 || s > tt + cs - wb)
		s = NULL, inwhat = IN_MATH, insubscr = 1;
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
	    int i = 0, hn = 0;
	    char *nb = (*s == String ? s + 1 : NULL), *ne = NULL;

	    for (tt = s; ++tt < s + cs - wb;)
		if (*tt == String) {
		    hn = 0;
		    nb = tt + 1;
		} else if (*tt == Inbrack) {
		    i++;
		    if (nb && !hn) {
			hn = 1;
			ne = tt;
		    }
		} else if (i && *tt == Outbrack)
		    i--;
	    if (i) {
		inwhat = IN_MATH;
		insubscr = 1;
		if (hn && nb && ne) {
		    char sav = *ne;
		    *ne = '\0';
		    zsfree(varname);
		    varname = ztrdup(nb);
		    *ne = sav;
		}
	    }
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
		    brbsl = p - s;
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

    /* Get the string and length to insert: either from the line 
     * or from the match. */
    if (m && (m->flags & CMF_LINE)) {
	al = m->llen;
	as = l;
    } else {
	al = wl;
	as = w;
    }
    /* Allocate some more space if needed. */
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
    /* Copy it in the buffer. */
    if (prep) {
	memmove(rw + al, rw, rwlen - al);
	memcpy(rw, as, al);
    }
    else
	memcpy(nw, as, al);

    return nw + al;
}

/* This returns a new Cline structure. */

static Cline
getcline(char *l, int ll, char *w, int wl, int fl)
{
    Cline r;

    /* Preverably take it from the buffer list (freecl), if there
     * is none, allocate a new one. */
    if ((r = freecl))
	freecl = r->next;
    else
	r = (Cline) halloc(sizeof(*r));

    r->next = NULL;
    r->line = l;
    r->llen = ll;
    r->word = w;
    r->wlen = wl;
    r->flags = fl;
    r->prefix = r->suffix = NULL;

    return r;
}

/* This adds a Cline structure with the given parameters to the list we
 * are building during matching. */

static void
addtocline(Cline *retp, Cline *lrp,
	   char *l, int ll, char *w, int wl, Cmatcher m, int fl)
{
    Cline ln = getcline(l, ll, w, wl, fl);

    if (m && (m->flags & CMF_LINE))
	ln->word = NULL;
    if (*retp)
	(*lrp)->next = ln;
    else
	*retp = ln;

    *lrp = ln;
}

/* This compares two cpattern lists and returns non-zero if they are
 * equal. */

static int
cmp_cpatterns(Cpattern a, Cpattern b)
{
    while (a) {
	if (a->equiv != b->equiv || memcmp(a->tab, b->tab, 256))
	    return 0;
	a = a->next;
	b = b->next;
    }
    return 1;
}

/* This compares two cmatchers and returns non-zero if they are equal. */

static int
cmp_cmatchers(Cmatcher a, Cmatcher b)
{
    return (a == b ||
	    (a->flags == b->flags &&
	     a->llen == b->llen && a->wlen == b->wlen &&
	     (!a->llen || cmp_cpatterns(a->line, b->line)) &&
	     (a->wlen <= 0 || cmp_cpatterns(a->word, b->word)) &&
	     (!(a->flags & CMF_LEFT) ||
	      (a->lalen == b->lalen &&
	       (!a->lalen || cmp_cpatterns(a->left, b->left)))) &&
	     (!(a->flags & CMF_RIGHT) ||
	      (a->ralen == b->ralen &&
	       (!a->ralen || cmp_cpatterns(a->right, b->right))))));
}

/* Add the given matchers to the bmatcher list. */

static void
add_bmatchers(Cmatcher m)
{
    Cmlist old = bmatchers, *q = &bmatchers, n;

    for (; m; m = m->next) {
	if ((!m->flags && m->wlen > 0 && m->llen > 0) ||
	    (m->flags == CMF_RIGHT && m->wlen == -1 && !m->llen)) {
	    *q = n = (Cmlist) halloc(sizeof(struct cmlist));
	    n->matcher = m;
	    q = &(n->next);
	}
    }
    *q = old;
}

/* This is called when the matchers in the mstack have changed to
 * ensure that the bmatchers list contains no matchers not in mstack. */

static void
update_bmatchers(void)
{
    Cmlist p = bmatchers, q = NULL, ms;
    Cmatcher mp;
    int t;

    while (p) {
	t = 0;
	for (ms = mstack; ms && !t; ms = ms->next)
	    for (mp = ms->matcher; mp && !t; mp = mp->next)
		t = cmp_cmatchers(mp, p->matcher);

	p = p->next;
	if (!t) {
	    if (q)
		q->next = p;
	    else
		bmatchers = p;
	}
    }
}

/* When building up cline lists that are to be inserted at the end of the
 * string or on the left hand side in the middle, we do this separately for
 * multiple runs of characters separated by the anchors of `*' match patterns.
 * This function builds such a list from the given string. */

static Cline
end_list(int len, char *str)
{
    Cline ret = NULL, *q = &ret;
    Cmlist ms;
    Cmatcher mp;
    int t;
    char *p = str;

    while (len) {
	for (t = 0, ms = bmatchers; ms && !t; ms = ms->next) {
	    mp = ms->matcher;
	    if (mp->flags == CMF_RIGHT && mp->wlen == -1 &&
		!mp->llen && len >= mp->ralen && mp->ralen &&
		pattern_match(mp->right, str, NULL, NULL)) {
		/* This is one of those patterns, so add a cline struct.
		 * We store the anchor string in the line and the contents
		 * (i.e. the strings between the anchors) in the word field. */
		*q = getcline(str, mp->ralen, p, str - p, 0);
		q = &((*q)->next);
		str += mp->ralen;
		len -= mp->ralen;
		p = str;
		t = 1;
	    }
	}
	if (!t) {
	    str++;
	    len--;
	}
    }
    /* This is the cline struct for the remaining string at the end. */
    if (p != str) {
	*q = getcline("", 0, p, str - p, 0);
	q = &((*q)->next);
    }
    *q = NULL;

    return ret;
}

/* This builds a string that may be put on the line that fully matches the
 * given strings. The return value is NULL if no such string could be built
 * or that string in local static memory, dup it.
 * All this is a lot like the procedure in bld_new_pfx(), only slightly
 * simpler, see that function for more comments. */

static char *
join_strs(int la, char *sa, int lb, char *sb)
{
    static unsigned char *ea = NULL;
    static int ealen = 0;
    static char *line = NULL;
    static int llen = 0;
    static char *rs = NULL;
    static int rl = 0;

    Cmlist ms;
    Cmatcher mp;
    int t, bl, rr = rl;
    char *rp = rs;

    /* This is a buffer for the characters to use for pairs of correspondence
     * classes. */
    if (la + 1 > ealen || lb + 1 > ealen) {
	if (ealen)
	    zfree(ea, ealen);
	ea = (unsigned char *) zalloc(ealen = (la > lb ? la : lb) + 20);
    }
    while (la && lb) {
	if (*sa != *sb) {
	    for (t = 0, ms = bmatchers; ms && !t; ms = ms->next) {
		mp = ms->matcher;
		if (!mp->flags && mp->wlen > 0 && mp->llen > 0 &&
		    mp->wlen <= la && mp->wlen <= lb) {
		    if (pattern_match(mp->word, sa, NULL, ea)) {
			if (mp->llen + 1 > llen) {
			    if (llen)
				zfree(line, llen);
			    line = (char *) zalloc(llen = mp->llen + 20);
			}
			if ((bl = bld_line_pfx(mp->line, line, line,
					       lb, sb, ea))) {
			    line[mp->llen] = '\0';
			    if (rr <= mp->llen) {
				char *or = rs;

				rs = realloc(rs, (rl += 20));
				rr += 20;
				rp += rs - or;
			    }
			    memcpy(rp, line, mp->llen);
			    rp += mp->llen;
			    rr -= mp->llen;
			    sa += mp->wlen;
			    sb += bl;
			    la -= mp->wlen;
			    lb -= bl;
			    t = 1;
			}
		    } else if (pattern_match(mp->word, sb, NULL, ea)) {
			if (mp->llen + 1 > llen) {
			    if (llen)
				zfree(line, llen);
			    line = (char *) zalloc(llen = mp->llen + 20);
			}
			if ((bl = bld_line_pfx(mp->line, line, line,
					       la, sa, ea))) {
			    line[mp->llen] = '\0';
			    if (rr <= mp->llen) {
				char *or = rs;

				rs = realloc(rs, (rl += 20));
				rr += 20;
				rp += rs - or;
			    }
			    memcpy(rp, line, mp->llen);
			    rp += mp->llen;
			    rr -= mp->llen;
			    sa += bl;
			    sb += mp->wlen;
			    la -= bl;
			    lb -= mp->wlen;
			    t = 1;
			}
		    }
		}
	    }
	    if (!t)
		break;
	} else {
	    if (rr <= mp->llen) {
		char *or = rs;

		rs = realloc(rs, (rl += 20));
		rr += 20;
		rp += rs - or;
	    }
	    *rp++ = *sa;
	    rr--;
	    sa++;
	    sb++;
	    la--;
	    lb--;
	}
    }
    if (la || lb)
	return NULL;

    *rp = '\0';

    return rs;
}

/* This gets two cline lists with separated runs and joins the corresponding
 * elements in these lists. In olp and nlp we return the lengths of the matched
 * portions in o and n respectively. As always the list in o is the one that
 * contains the joined result after this function. */

static Cline
join_ends(Cline o, Cline n, int *olp, int *nlp)
{
    Cline ret = o;
    int mol, mnl, smol = 0, smnl = 0;
    char *j;

    while (o && n) {
	if (o->llen == n->llen && !strncmp(o->line, n->line, o->llen)) {
	    /* The anchors are the same, so join the contents. */
	    bld_pfx(o, n, &mol, &mnl);
	    smol += mol + o->llen;
	    smnl += mnl + n->llen;
	} else if (!(o->flags & CLF_JOIN) &&
		   (j = join_strs(o->llen, o->line, n->llen, n->line))) {
	    /* We could build a string matching both anchors, so use that
	     * and mark the cline so that we don't try to join it again. */
	    o->flags |= CLF_JOIN;
	    o->llen = strlen(j);
	    o->line = dupstring(j);
	    bld_pfx(o, n, &mol, &mnl);
	    smol += mol + o->llen;
	    smnl += mnl + n->llen;
	} else {
	    /* Different anchors, see if we can find matching anchors
	     * further down the lists. */
	    Cline to, tn;
	    int t = 0;

	    /* But first build the common prefix. */
	    bld_pfx(o, n, &mol, &mnl);
	    smol += mol;
	    smnl += mnl;

	    for (to = o; to && !t; to = to->next) {
		for (tn = n; tn && !t; tn = tn->next) {
		    if ((t = ((to->llen == tn->llen &&
			       !strncmp(to->line, tn->line, to->llen)) ||
			      (!(to->flags & CLF_JOIN) &&
			       join_strs(to->llen, to->line,
					 tn->llen, tn->line)))))
			break;
		}
		if (t)
		    break;
	    }
	    if (t) {
		/* Found matching anchors, continue with them. */
		o->line = to->line;
		o->llen = to->llen;
		o->next = to->next;
		o->flags |= CLF_MISS;
		n = tn;
	    } else {
		/* No matching anchors found, shorten the list. */
		o->flags |= CLF_MISS;
		o->next = NULL;
		o->llen = 0;
		if (olp)
		    *olp = smol;
		if (nlp)
		    *nlp = smnl;
		return ret;
	    }
	}
	/* If we reached the end of the new list but not the end of the old
	 * list, we mark the old list (saying that characters are missing 
	 * here). */
	if (!(n = n->next) && o->next)
	    o->flags |= CLF_MISS;
	o = o->next;
    }
    if (o) {
	/* Shorten the list if we haven't reached the end. */
	if (n)
	    o->flags |= CLF_MISS;
	o->next = NULL;
	o->llen = 0;
    }
    if (olp)
	*olp = smol;
    if (nlp)
	*nlp = smnl;
    return ret;
}

/* This builds all the possible line patterns for the pattern pat in the
 * buffer line. Initially line is the same as lp, but during recursive
 * calls lp is incremented for storing successive characters. Whenever
 * a full possible string is build, we test if this line matches the
 * string given by wlen and word. The last argument contains the characters
 * to use for the correspondence classes, it was filled by a call to 
 * pattern_match() in the calling function.
 * The return value is the length of the string matched in the word, it
 * is zero if we couldn't build a line that matches the word. */

/**/
static int
bld_line_pfx(Cpattern pat, char *line, char *lp,
	     int wlen, char *word, unsigned char *in)
{
    if (pat) {
	/* Still working on the pattern. */

	int i, l;
	unsigned char c = 0;

	/* Get the number of the character for a correspondence class
	 * if it has a correxponding class. */
	if (pat->equiv)
	    if ((c = *in))
		in++;

	/* Walk through the table in the pattern and try the characters
	 * that may appear in the current position. */
	for (i = 0; i < 256; i++)
	    if ((pat->equiv && c) ? (c == pat->tab[i]) : pat->tab[i]) {
		*lp = i;
		/* We stored the character, now call ourselves to build
		 * the rest. */
		if ((l = bld_line_pfx(pat->next, line, lp + 1,
				      wlen, word, in)))
		    return l;
	    }
    } else {
	/* We reached the end, i.e. the line string is fully build, now
	 * see if it matches the given word. */
	static unsigned char *ea = NULL;
	static int ealen = 0;

	Cmlist ms;
	Cmatcher mp;
	int l = lp - line, t, rl = 0;

	/* Quick test if the strings are exactly the same. */
	if (l == wlen && !strncmp(line, word, l))
	    return l;

	/* We need another buffer for correspondence classes. */
	if (l + 1 > ealen) {
	    if (ealen)
		zfree(ea, ealen);
	    ea = (unsigned char *) zalloc(ealen = l + 20);
	}
	/* We loop through the whole line string built. */
	while (l && wlen) {
	    if (*word == *line) {
		/* The same character in both strings, skip over. */
		line++;
		word++;
		l--;
		wlen--;
		rl++;
	    } else {
		t = 0;
		for (ms = bmatchers; ms && !t; ms = ms->next) {
		    mp = ms->matcher;
		    if (!mp->flags && mp->wlen <= wlen && mp->llen <= l &&
			pattern_match(mp->line, line, NULL, ea) &&
			pattern_match(mp->word, word, ea, NULL)) {
			/* Both the line and the word pattern matched,
			 * now skip over the matched portions. */
			line += mp->llen;
			word += mp->wlen;
			l -= mp->llen;
			wlen -= mp->wlen;
			rl += mp->wlen;
			t = 1;
		    }
		}
		if (!t)
		    /* Didn't match, give up. */
		    return 0;
	    }
	}
	if (!l)
	    /* Unmatched portion in the line built, return no-match. */
	    return rl;
    }
    return 0;
}

/* This builds a list of cline structs describing a string to insert in
 * the line in a place where we didn't had something on the line when
 * completion was invoked. This is called after we get the second match
 * so we have two strings to start with given by (ol,ow) and (nl,nw) and
 * the cline list returned describes a string that matches both of these
 * strings.
 * In olp and nlp we return the number of characters in ow and nw that
 * are not matched by the cline list returned. Especially this means that
 * they are non-zero if there are such unmatched portions.
 * In lp we return a pointer to the last cline struct created. */

static Cline
bld_new_pfx(int ol, char *ow, int nl, char *nw, int *olp, int *nlp, Cline *lp)
{
    static unsigned char *ea = NULL;
    static int ealen = 0;
    static char *line = NULL;
    static int llen = 0;

    Cmlist ms;
    Cmatcher mp;
    Cline ret = NULL, l = NULL, n;
    char *p = ow;
    int t, bl;

    /* This is a buffer for the characters to use for pairs of correspondence
     * classes. */
    if (ol + 1 > ealen || nl + 1 > ealen) {
	if (ealen)
	    zfree(ea, ealen);
	ea = (unsigned char *) zalloc(ealen = (ol > nl ? ol : nl) + 20);
    }
    /* Loop through the strings. */
    while (ol && nl) {
	if (*ow != *nw) {
	    /* Not the same character, use the patterns. */
	    for (t = 0, ms = bmatchers; ms && !t; ms = ms->next) {
		mp = ms->matcher;
		/* We use only those patterns that match a non-empty
		 * string in both the line and the word, that match
		 * strings no longer than the string we still have
		 * to compare, that don't have anchors, and that don't
		 * use the line strings for insertion. */
		if (!mp->flags && mp->wlen > 0 && mp->llen > 0 &&
		    mp->wlen <= ol && mp->wlen <= nl) {
		    /* Try the pattern only if the word-pattern matches
		     * one of the strings. */
		    if (pattern_match(mp->word, ow, NULL, ea)) {
			/* Make the buffer where we build the possible
			 * line patterns big enough. */
			if (mp->llen + 1 > llen) {
			    if (llen)
				zfree(line, llen);
			    line = (char *) zalloc(llen = mp->llen + 20);
			}
			/* Then build all the possible lines and see
			 * if one of them matches the othe string. */
			if ((bl = bld_line_pfx(mp->line, line, line,
					       nl, nw, ea))) {
			    /* Yep, one of the lines matched the other
			     * string. */
			    if (p != ow) {
				/* There is still a substring that is
				 * the same in both strings, so add
				 * a cline for it. */
				char sav = *ow;

				*ow = '\0';
				n = getcline(NULL, 0, dupstring(p),
					     ow - p, 0);
				*ow = sav;
				if (l)
				    l->next = n;
				else
				    ret = n;
				l = n;
			    }
			    /* Then we add the line string built. */
			    line[mp->llen] = '\0';
			    n = getcline(dupstring(line), mp->llen,
					 NULL, 0, CLF_DIFF);
			    if (l)
				l->next = n;
			    else
				ret = n;
			    l = n;
			    ow += mp->wlen;
			    nw += bl;
			    ol -= mp->wlen;
			    nl -= bl;
			    p = ow;
			    t = 1;
			}
		    } else if (pattern_match(mp->word, nw, NULL, ea)) {
			/* Now do the same for the other string. */
			if (mp->llen + 1 > llen) {
			    if (llen)
				zfree(line, llen);
			    line = (char *) zalloc(llen = mp->llen + 20);
			}
			if ((bl = bld_line_pfx(mp->line, line, line,
					       ol, ow, ea))) {
			    if (p != ow) {
				char sav = *ow;

				*ow = '\0';
				n = getcline(NULL, 0, dupstring(p),
					     ow - p, 0);
				*ow = sav;
				if (l)
				    l->next = n;
				else
				    ret = n;
				l = n;
			    }
			    line[mp->llen] = '\0';
			    n = getcline(dupstring(line), mp->llen,
					 NULL, 0, CLF_DIFF);
			    if (l)
				l->next = n;
			    else
				ret = n;
			    l = n;
			    ow += bl;
			    nw += mp->wlen;
			    ol -= bl;
			    nl -= mp->wlen;
			    p = ow;
			    t = 1;
			}
		    }
		}
	    }
	    if (!t)
		/* No pattern matched, so give up. */
		break;
	} else {
	    /* Same character, skip over. */
	    ow++;
	    nw++;
	    ol--;
	    nl--;
	}
    }
    if (p != ow) {
	/* There is a equal substring in both strings, build a cline
	 * for it. */
	char sav = *ow;

	*ow = '\0';
	n = getcline(NULL, 0, dupstring(p), ow - p, 0);
	*ow = sav;
	if (l)
	    l->next = n;
	else
	    ret = n;
	l = n;
    }
    if (l)
	l->next = NULL;
    else
	ret = NULL;

    if (olp)
	*olp = ol;
    if (nlp)
	*nlp = nl;
    if (lp)
	*lp = l;

    return ret;
}

/* Given a cline list for an unmatched part of the string to insert in the
 * line and a new match substring, modify the cline list so that it also
 * matches this string. The cline list is shortened in the place where
 * we can't build a cline matching the new string.
 * However, the resulting cline list is returned. The string is described
 * by len and word. In missp we return non-zero if the cline list returned
 * had to be shortened (and hence doesn't fully match the strings it was
 * built from anymore) or if it doesn't fully match the given string.
 * This function checks the string left to right and thus is to be used
 * for strings where we want a common prefix. */

static Cline
join_new_pfx(Cline line, int len, char *word, int *missp)
{
    static unsigned char *ea = NULL;
    static int ealen = 0;

    Cline ret = NULL, l = NULL, next;
    int miss = 0;

    /* Walk through the list and the string. */
    while (line && len) {
	next = line->next;
	/* The line element is used in those places where a new line
	 * string was built. */
	if (line->line) {
	    Cmlist ms;
	    Cmatcher mp;
	    int ll = line->llen, t;
	    char *p = line->line;

	    /* Make the buffer for the correspondence classes big enough. */
	    if (line->llen + 1 > ealen) {
		if (ealen)
		    zfree(ea, ealen);
		ea = (unsigned char *) zalloc(ealen = line->llen + 20);
	    }
	    /* Check if the line string from the cline list matches the
	     * new string. */
	    while (ll && len) {
		if (*p == *word) {
		    p++;
		    word++;
		    ll--;
		    len--;
		} else {
		    for (t = 0, ms = bmatchers; ms && !t; ms = ms->next) {
			mp = ms->matcher;
			if (!mp->flags && mp->wlen > 0 && mp->llen > 0 &&
			    mp->wlen <= len && mp->llen <= len &&
			    pattern_match(mp->word, word, NULL, ea) &&
			    pattern_match(mp->line, p, ea, NULL)) {
			    /* We have a matched substring, skip over. */
			    p += mp->llen;
			    word += mp->wlen;
			    ll -= mp->llen;
			    len -= mp->wlen;
			    t = 1;
			}
		    }
		    if (!t)
			/* No match, give up. */
			break;
		}
	    }
	    if (p == line->line) {
		/* We couldn't match any character from the string in the
		 * cline, so shorten the list and don't even keep this
		 * struct. */
		miss = 1;
		len = 0;
	    } else {
		/* At least the beginning of the cline string can be used. */
		if (ll) {
		    /* But there is a portion of the string that can't be
		     * used, so we have to shorten the list. */
		    miss = 1;
		    *p = '\0';
		    line->llen -= ll;
		    len = 0;
		}
		line->next = NULL;
		if (l)
		    l->next = line;
		else
		    ret = line;
		l = line;
	    }
	} else {
	    /* The cline doesn't have a string built by reverse matching,
	     * so we have to work on the original substring in the cline
	     * and the new string. */
	    if (line->wlen == len && strncmp(line->word, word, len)) {
		/* They are equal, accept and return. If there was
		 * another element in the list, shorten the list. */
		if (next)
		    miss = 1;
		line->next = NULL;
		if (l)
		    l->next = line;
		else
		    ret = line;
		l = line;
		len = 0;
	    } else {
		char sav = word[len];

		/* Check if one is the prefix of the other one. */
		word[len] = '\0';
		if (strpfx(word, line->word)) {
		    word[len] = sav;

		    line->word[len] = '\0';
		    line->wlen = len;
		    miss = 1;
		    line->next = NULL;
		    if (l)
			l->next = line;
		    else
			ret = line;
		    l = line;
		    len = 0;
		} else if (strpfx(line->word, word)) {
		    word[len] = sav;

		    miss = 1;
		    line->next = NULL;
		    if (l)
			l->next = line;
		    else
			ret = line;
		    l = line;
		    len = 0;
		} else {
		    /* Not the same and no prefix, so we try to build a
		     * new line string matching the string in the cline
		     * and the new string. */
		    int mol, mnl;
		    Cline sl, send;

		    word[len] = sav;

		    if ((sl = bld_new_pfx(line->wlen, line->word,
					  len, word, &mol, &mnl, &send))) {
			/* We could build such a string, use it in the
			 * cline structure. */
			if (l)
			    l->next = sl;
			else
			    ret = sl;
			l = sl;
			if (!mol) {
			    send->next = next;
			    word += len - mnl;
			    len = mnl;
			} else
			    len = 0;
			l = send;
		    } else
			len = 0;
		}
	    }
	}
	line = next;
    }
    *missp = (line || len || miss);

    return ret;
}

/* This function gets two cline structs for which we want to build a
 * common prefix to put on the line. The result is placed in the cline
 * struct given as first argument.
 * In olp and nlp we return the matched lengths for o and n, respectively
 * (but this is only guaranteed to give correct results if this is the
 * first call for the given o and n). */

/**/
static void
bld_pfx(Cline o, Cline n, int *olp, int *nlp)
{
    if (olp)
	*olp = 0;
    if (nlp)
	*nlp = 0;
    if (o->flags & CLF_PNEW) {
	if (o->flags & (CLF_END | CLF_MID))
	    /* We split the suffix in the middle and at the end into
	     * separate runs. */
	    o->prefix = join_ends(o->prefix, end_list(n->wlen, n->word),
				  NULL, NULL);
	else {
	    /* This flag is set if we already built a cline list for such
	     * a prefix. We join it with the string from the other cline
	     * struct. */
	    int miss;

	    o->prefix = join_new_pfx(o->prefix, n->wlen, n->word, &miss);
	    if (miss)
		o->flags |= CLF_MISS;
	}
    } else if (o->flags & (CLF_END | CLF_MID)) {
	o->flags |= CLF_PNEW;
	o->prefix = join_ends(end_list(o->wlen, o->word),
			      end_list(n->wlen, n->word), olp, nlp);
    } else if (o->wlen && n->wlen) {
	/* We haven't built a cline list for it yet. */
	char so = o->word[o->wlen], sn = n->word[n->wlen];
	char *new = o->word;
	int newl = o->wlen, mol, mnl;

	/* If one of the strings is a prefix of the other one, just keep
	 * that prefix. */
	o->word[o->wlen] = n->word[n->wlen] = '\0';
	if (strpfx(n->word, o->word)) {
	    new = dupstring(n->word);
	    newl = n->wlen;
	    if (olp)
		*olp = *nlp = n->wlen;
	} else if (strpfx(o->word, n->word)) {
	    if (olp)
		*olp = *nlp = o->wlen;
	} else {
	    /* Otherwise build a cline list describing a string that
	     * matches both strings from the original cline structs
	     * and thus can be put in the command line to represent
	     * them. This cline list is stored in o. */
	    o->flags |= CLF_PNEW;
	    o->prefix = bld_new_pfx(o->wlen, o->word, n->wlen, n->word,
				    &mol, &mnl, NULL);
	    newl = 0;
	    new = "";
	    if (mol || mnl)
		o->flags |= CLF_MISS;
	    if (olp) {
		*olp = o->wlen - mol;
		*nlp = n->wlen - mnl;
	    }
	}
	o->word[o->wlen] = so;
	n->word[n->wlen] = sn;

	o->word = new;
	o->wlen = newl;

	if (!o->prefix && n->wlen != o->wlen)
	    o->flags |= CLF_MISS;
    } else
	o->wlen = 0;
}

/* The following function are like their counterparts above, only for
 * the other direction. */

static int
bld_line_sfx(Cpattern pat, char *line, char *lp,
	     int wlen, char *word, unsigned char *in)
{
    if (pat) {
	int i, l;
	unsigned char c = 0;

	if (pat->equiv)
	    if ((c = *in))
		in++;

	for (i = 0; i < 256; i++)
	    if ((pat->equiv && c) ? (c == pat->tab[i]) : pat->tab[i]) {
		*lp = i;
		if ((l = bld_line_pfx(pat->next, line, lp + 1,
				      wlen, word, in)))
		    return l;
	    }
    } else {
	static unsigned char *ea = NULL;
	static int ealen = 0;

	Cmlist ms;
	Cmatcher mp;
	int l = lp - line, t, rl = 0;

	if (l == wlen && !strncmp(line, word, l))
	    return l;

	line = lp;
	word += wlen;
	
	if (l + 1 > ealen) {
	    if (ealen)
		zfree(ea, ealen);
	    ea = (unsigned char *) zalloc(ealen = l + 20);
	}
	while (l && wlen) {
	    if (word[-1] == line[-1]) {
		line--;
		word--;
		l--;
		wlen--;
		rl++;
	    } else {
		t = 0;
		for (ms = bmatchers; ms && !t; ms = ms->next) {
		    mp = ms->matcher;
		    if (!mp->flags && mp->wlen <= wlen && mp->llen <= l &&
			pattern_match(mp->line, line - mp->llen,
				      NULL, ea) &&
			pattern_match(mp->word, word - mp->wlen,
				      ea, NULL)) {
			line -= mp->llen;
			word -= mp->wlen;
			l -= mp->llen;
			wlen -= mp->wlen;
			rl += mp->wlen;
			t = 1;
		    }
		}
		if (!t)
		    return 0;
	    }
	}
	if (!l)
	    return rl;
    }
    return 0;
}

static Cline
bld_new_sfx(int ol, char *ow, int nl, char *nw, int *olp, int *nlp, Cline *lp)
{
    static unsigned char *ea = NULL;
    static int ealen = 0;
    static char *line = NULL;
    static int llen = 0;

    Cmlist ms;
    Cmatcher mp;
    Cline ret = NULL, l = NULL, n;
    char *p;
    int t, bl;

    ow += ol;
    nw += nl;
    p = ow;

    if (ol + 1 > ealen || nl + 1 > ealen) {
	if (ealen)
	    zfree(ea, ealen);
	ea = (unsigned char *) zalloc((ealen = (ol > nl ? ol : nl) + 20));
    }
    while (ol && nl) {
	if (ow[-1] != nw[-1]) {
	    for (t = 0, ms = bmatchers; ms && !t; ms = ms->next) {
		mp = ms->matcher;
		if (!mp->flags && mp->wlen > 0 && mp->llen > 0 &&
		    mp->wlen <= ol && mp->wlen <= nl) {
		    if (pattern_match(mp->word, ow - mp->wlen,
				      NULL, ea)) {
			if (mp->llen + 1 > llen) {
			    if (llen)
				zfree(line, llen);
			    line = (char *) zalloc(llen = mp->llen + 20);
			}
			if ((bl = bld_line_sfx(mp->line, line, line,
					       nl, nw, ea))) {
			    if (p != ow) {
				char sav = *p;

				*p = '\0';
				n = getcline(NULL, 0, dupstring(ow),
					     p - ow, 0);
				*p = sav;
				if (l)
				    l->next = n;
				else
				    ret = n;
				l = n;
			    }
			    line[mp->llen] = '\0';
			    n = getcline(dupstring(line), mp->llen,
					 NULL, 0, CLF_DIFF);
			    if (l)
				l->next = n;
			    else
				ret = n;
			    l = n;
			    ow -= mp->wlen;
			    nw -= bl;
			    ol -= mp->wlen;
			    nl -= bl;
			    p = ow;
			    t = 1;
			}
		    } else if (pattern_match(mp->word, nw - mp->wlen,
					     NULL, ea)) {
			if (mp->llen + 1 > llen) {
			    if (llen)
				zfree(line, llen);
			    line = (char *) zalloc(llen = mp->llen + 20);
			}
			if ((bl = bld_line_sfx(mp->line, line, line,
					       ol, ow, ea))) {
			    if (p != ow) {
				char sav = *p;

				*p = '\0';
				n = getcline(NULL, 0, dupstring(ow),
					     p - ow, 0);
				*p = sav;
				if (l)
				    l->next = n;
				else
				    ret = n;
				l = n;
			    }
			    line[mp->llen] = '\0';
			    n = getcline(dupstring(line), mp->llen,
					 NULL, 0, CLF_DIFF);
			    if (l)
				l->next = n;
			    else
				ret = n;
			    l = n;
			    ow -= bl;
			    nw -= mp->wlen;
			    ol -= bl;
			    nl -= mp->wlen;
			    p = ow;
			    t = 1;
			}
		    }
		}
	    }
	    if (!t)
		break;
	} else {
	    ow--;
	    nw--;
	    ol--;
	    nl--;
	}
    }
    if (p != ow) {
	char sav = *p;

	*p = '\0';
	n = getcline(NULL, 0, dupstring(ow), p - ow, 0);
	*p = sav;
	if (l)
	    l->next = n;
	else
	    ret = n;
	l = n;
    }
    if (l)
	l->next = NULL;
    else
	ret = NULL;

    if (olp)
	*olp = ol;
    if (nlp)
	*nlp = nl;
    if (lp)
	*lp = l;

    return ret;
}

static Cline
join_new_sfx(Cline line, int len, char *word, int *missp)
{
    static unsigned char *ea = NULL;
    static int ealen = 0;

    Cline ret = NULL, l = NULL, next;
    int miss = 0, ind = 0;

    word += len;

    while (line && len) {
	next = line->next;
	if (line->line) {
	    Cmlist ms;
	    Cmatcher mp;
	    int ll = line->llen, t;
	    char *p = line->line + ll;

	    if (line->llen + 1 > ealen) {
		if (ealen)
		    zfree(ea, ealen);
		ea = (unsigned char *) zalloc(ealen = line->llen + 20);
	    }
	    while (ll && len) {
		if (p[-1] == word[-1]) {
		    p--;
		    word--;
		    ll--;
		    len--;
		    ind++;
		} else {
		    for (t = 0, ms = bmatchers; ms && !t; ms = ms->next) {
			mp = ms->matcher;
			if (!mp->flags && mp->wlen > 0 && mp->llen > 0 &&
			    mp->wlen <= len && mp->llen <= len &&
			    pattern_match(mp->word, word - mp->wlen,
					  NULL, ea) &&
			    pattern_match(mp->line, p - mp->llen,
					  ea, NULL)) {
			    p -= mp->llen;
			    word -= mp->wlen;
			    ll -= mp->llen;
			    len -= mp->wlen;
			    ind += mp->wlen;
			    t = 1;
			}
		    }
		    if (!t)
			break;
		}
	    }
	    if (p == line->line + line->llen) {
		miss = 1;
		len = 0;
	    } else {
		if (ll) {
		    miss = 1;
		    line->line = p;
		    line->llen -= ll;
		    len = 0;
		}
		line->next = NULL;
		if (l)
		    l->next = line;
		else
		    ret = line;
	    }
	} else {
	    if (line->wlen == len && strncmp(line->word, word - len, len)) {
		if (next)
		    miss = 1;
		line->next = NULL;
		if (l)
		    l->next = line;
		else
		    ret = line;
		len = 0;
	    } else {
		char sav = word[ind];

		word[ind] = '\0';
		if (strpfx(word - len, line->word)) {
		    word[ind] = sav;

		    line->word += line->wlen - len;
		    line->wlen = ind;
		    miss = 1;
		    line->next = NULL;
		    if (l)
			l->next = line;
		    else
			ret = line;
		    len = 0;
		} else if (strpfx(line->word, word - len)) {
		    word[ind] = sav;

		    miss = 1;
		    line->next = NULL;
		    if (l)
			l->next = line;
		    else
			ret = line;
		    len = 0;
		} else {
		    int mol, mnl;
		    Cline sl, send;

		    word[len] = sav;

		    if ((sl = bld_new_sfx(line->wlen, line->word,
					  len, word - len,
					  &mol, &mnl, &send))) {
			if (l)
			    l->next = sl;
			else
			    ret = sl;
			if (!mol) {
			    send->next = next;
			    word -= len - mnl;
			    len = mnl;
			} else
			    len = 0;
			l = send;
		    } else
			len = 0;
		}
	    }
	}
	line = next;
    }
    *missp = (line || len || miss);

    return ret;
}

/**/
static void
bld_sfx(Cline o, Cline n)
{
    if (o->flags & CLF_SNEW) {
	int miss;

	o->suffix = join_new_sfx(o->suffix, n->wlen, n->word, &miss);
	if (miss)
	    o->flags |= CLF_MISS;
    } else {
	char so = o->word[o->wlen], sn = n->word[n->wlen];
	char *new = o->word;
	int newl = o->wlen, mol, mnl;

	o->word[o->wlen] = n->word[n->wlen] = '\0';
	if (strpfx(n->word, o->word)) {
	    new = dupstring(n->word);
	    newl = n->wlen;
	} else if (!strpfx(o->word, n->word)) {
	    o->flags |= CLF_SNEW;
	    o->suffix = bld_new_sfx(o->wlen, o->word, n->wlen, n->word,
				    &mol, &mnl, NULL);
	    newl = 0;
	    new = "";
	    if (mol || mnl)
		o->flags |= CLF_MISS;
	}
	o->word[o->wlen] = so;
	n->word[n->wlen] = sn;

	o->word = new;
	o->wlen = newl;

	if (!o->suffix && n->wlen != o->wlen)
	    o->flags |= CLF_MISS;
    }
}

/* Joins two Cline lists, building the most specific line string *
 * that is possible and returns it. This is done by modifying the
 * cline list given as the first argument. */

static Cline
join_clines(Cline o, Cline n)
{
    Cline oo = o;

    if (!o)
	/* This is the first time this was called, so just return the
	 * the second list. In future calls we will get this list as
	 * the first argument. */
	return n;
    else {
	Cline f = freecl, q, op = NULL;
	int ol, nl;

	freecl = NULL;

	while (o && n) {
	    /* CLF_MID is set in the cline struct where the prefix and the
	     * suffix from the line meet. If we have reached the cline
	     * for it in one of the lists, search the corresponding 
	     * cline in the other list, removing all structs up to it. */
	    if (o->flags & CLF_MID) {
		while (n && !(n->flags & CLF_MID)) {
		    q = n->next;
		    n->next = f;
		    f = n;

		    n = q;
		}
	    }
	    if (n && n->flags & CLF_MID) {
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
		/* These are the structs in the middle, so simplify the
		 * prefix and the suffix in it to their longest common
		 * versions. */
		
		char *os, *ns, *oss = o->line, *nss = n->line;
		int ol, nl, mol, mnl, oll = o->llen, nll = n->llen;

		os = o->word;
		ol = o->wlen;
		ns = n->word;
		nl = n->wlen;

		o->word = o->line;
		o->wlen = o->llen;
		n->word = n->line;
		n->wlen = n->llen;
		bld_pfx(o, n, &mol, &mnl);
		o->line = o->word;
		o->llen = o->wlen;

		o->word = os;
		o->wlen = ol;
		n->word = ns;
		n->wlen = nl;

		if (o->wlen < 0) {
		    o->word = oss + mol;
		    o->wlen = oll - mol;
		}
		if (n->wlen < 0) {
		    n->word = nss + mnl;
		    n->wlen = nll - mnl;
		}
		bld_sfx(o, n);
	    } else if (o->word) {
		if (n->word) {
		    if (o->llen == n->llen &&
			(o->flags & CLF_VAR) && (n->flags & CLF_VAR) &&
			(o->flags & (CLF_END | CLF_SUF)) ==
			(n->flags & (CLF_END | CLF_SUF))) {
			/* We have two chunks from `*' patterns,
			 * reduce them to the common prefix/suffix. */
			if (o->flags & CLF_SUF)
			    bld_sfx(o, n);
			else
			    bld_pfx(o, n, NULL, NULL);
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
	    /* The old list has elements not matched by the second list
	     * we put all these elements in the free list. */
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

/* This returns a Cline for the given string. In lp we return a pointer to
 * the last cline struct created. */

static Cline
str_cline(char *s, int l, Cline *lp)
{
    Cline r = NULL, *p = &r, n = NULL;

    if (l < 0)
	l = strlen(s);
    /* We build one struct per character, this makes joining it faster
     * and easier. */
    while (l) {
	*p = n = getcline(s, 1, NULL, 0, 0);

	p = &(n->next);
	s++;
	l--;
    }
    if (lp)
	*lp = n;

    return r;
}

/* This reverts the order of the elements of the given cline list and
 * returns a pointer to the new head. */

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

/* Prepend a string to a Cline and return a pointer to the new cline list. */

static Cline
prepend_cline(char *s, Cline l)
{
    Cline n, r = str_cline(s, -1, &n), *p = &(n->next);

    while (l) {
	*p = n = getcline(l->line, l->llen, l->word, l->wlen,
			  l->flags);

	p = &(n->next);
	l = l->next;
    }
    return r;
}

/* This simplifies the Cline to match the given strings. The strings are:
 * the common prefix and its length, a string with the suffix at the end
 * and the suffix length. */

static void
merge_cline(Cline lc, char *p, int pl, char *s, int sl, int psl)
{
    int pll, sll;
    Cline l = NULL, *q = &l, n;

    pll = strlen(p);
    if (s) {
	/* If there is a suffix, get a pointer to the beginning of the
	 * common suffix. */
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

    /* Now build a cline list from the string(s) and join it with the
     * cline list we have built while testing possible matches. */
    l = str_cline(p, pl, &n);
    q = &(n->next);
    if (!sl)
	*q = getcline(NULL, 0, p + pl, pll, CLF_END | CLF_VAR);
    else {
	*q = n = getcline(p + pl, pll, s, sll, CLF_MID);

	n->next = str_cline(s + sll, sl, NULL);
    }
    join_clines(lc, l);
}

/* This inserts the Cline built into the command line. The last two
 * arguments are the relative positions where the begining and the
 * end of the brace expansion strings have to be re-inserted. */

static void
inst_cline(Cline l, int pl, int sl)
{
    int m = -1, d = -1, b = -1, hb = 0, i = 0;

    /* Adjust the position of the braces. */
    pl += brpl;
    sl += brbsl;

    i = cs - wb;

    /* Insert the brace strings now? */
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
    /* Walk the list. */
    while (l) {
	/* If this cline describes a suffix where something is missing
	 * for some matches, remember the position. */
	if (m < 0 && (l->flags & (CLF_MISS | CLF_SUF)) == (CLF_MISS | CLF_SUF))
	    m = cs;

	if (l->prefix) {
	    Cline sl = l->prefix, ssl;
	    int hm;

	    if (l->flags & (CLF_END | CLF_MID)) {
		/* At the end and in the middle the suffix we have
		 * separate runs. */
		for (; sl; sl = sl->next) {
		    hm = 0;
		    if (sl->prefix) {
			for (ssl = sl->prefix; ssl; ssl = ssl->next) {
			    if (ssl->line)
				/* line for a string derived from applying
				 * the matchers the other way round. */
				inststrlen(ssl->line, 1, ssl->llen);
			    else
				/* and word for substrings equal in all
				 * matches. */
				inststrlen(ssl->word, 1, ssl->wlen);
			    /* If the string differs from any of the 
			     * matches, remember the position. */
			    if (d < 0 && (ssl->flags & CLF_DIFF))
				d = cs;
			}
		    } else if (sl->wlen)
			inststrlen(sl->word, 1, sl->wlen);

		    if (m < 0 && (sl->flags & CLF_MISS))
			m = cs;
		    if (sl->llen)
			inststrlen(sl->line, 1, sl->llen);
		    hm = (sl->flags & CLF_MISS);
		}
		if ((l->flags & CLF_MID) &&hm && b < 0) {
		    b = cs;
		    hb = 1;
		}
	    } else {
		/* The cline contains a newly build part of the string 
		 * in a sub-list. */
		for (; sl; sl = sl->next) {
		    if (sl->line)
			/* line for a string derived from applying the 
			 * matchers the other way round. */
			inststrlen(sl->line, 1, sl->llen);
		    else
			/* and word for substrings equal in all matches. */
			inststrlen(sl->word, 1, sl->wlen);
		    /* If the string differs from any of the matches,
		     * remember the position. */
		    if (d < 0 && (sl->flags & CLF_DIFF))
			d = cs;
		}
	    }
	    if (!(l->flags & (CLF_END | CLF_MID)))
		i += l->llen;
	}
	if (l->suffix) {
	    Cline sl = revert_clines(l->suffix);

	    if ((sl->flags & CLF_MISS) && b < 0) {
		b = cs;
		hb = 1;
	    }
	    for (; sl; sl = sl->next) {
		if (sl->line)
		    inststrlen(sl->line, 1, sl->llen);
		else
		    inststrlen(sl->word, 1, sl->wlen);
		if (d < 0 && (sl->flags & CLF_DIFF))
		    d = cs;
	    }
	}
	if (l->flags & CLF_MID) {
	    /* The cline in the middle, insert the prefix and the 
	     * suffix. */
	    if (!l->prefix && l->llen) {
		inststrlen(l->line, 1, l->llen);
		if (b < 0) {
		    b = cs;
		    hb = l->flags & CLF_MISS;
		}
	    }
	    if (!l->suffix && l->wlen > 0)
		inststrlen(l->word, 1, l->wlen);
	} else if (!l->prefix && !l->suffix) {
	    if (l->word &&
		!((pl >= 0 && brbeg && *brbeg &&
		   i < pl && i + l->llen >= pl) ||
		  (sl >= 0 && brend && *brend &&
		   i < sl && i + l->llen >= sl))) {
		/* We insert the prefered string stored in word only if we
		 * don't have to put the brace beginning or end here. */
		inststrlen(l->word, 1, l->wlen);
	    } else {
		/* Otherwise just re-insert the original string. */
		inststrlen(l->line, 1, l->llen);
	    }
	    i += l->llen;
	}
	/* Remember the position if there is a difference or a missing
	 * substring. */
	if (d < 0 && (l->flags & CLF_DIFF))
	    d = cs;
	if (m < 0 && (l->flags & CLF_MISS))
	    m = cs;

	/* Probably insert the brace beginning or end. */
	if (pl >= 0 && i >= pl && brbeg && *brbeg) {
	    cs -= i - pl;
	    inststrlen(brbeg, 1, -1);
	    cs += i - pl;
	    pl = -1;
	    hb = 1;
	}
	if (sl >= 0 && i > sl && brend && *brend) {
	    cs -= i - sl;
	    inststrlen(brend, 1, -1);
	    cs += i - sl;
	    sl = -1;
	    hb = 1;
	}
	l = l->next;
    }
    lastend = cs;
    /* Now place the cursor. Preferably in a position where something
     * is missing, otherwise in a place where the string differs from
     * any of the matches, or just leave it at the end. */
    cs = (b >= 0 && hb ? b : (m >= 0 ? m : (d >= 0 ? d : cs)));
    if (cs > ll)
	cs = ll;
}

/* Check if the given pattern matches the given string.             *
 * `in' and `out' are used for {...} classes. In `out' we store the *
 * character number that was matched. In the word pattern this is   *
 * given in `in' so that we can easily test if we found the         *
 * corresponding character. */

/**/
static int
pattern_match(Cpattern p, char *s, unsigned char *in, unsigned char *out)
{
    unsigned char c;

    while (p) {
	c = *((unsigned char *) s);

	if (out)
	    *out = 0;

	if (p->equiv) {
	    if (in) {
		c = p->tab[c];
		if ((*in && *in != c) || (!*in && !c))
		    return 0;
	    } else if (out) {
		if (!(*out = p->tab[c]))
		    return 0;
	    } else if (!p->tab[c])
		return 0;

	    if (in && *in)
		in++;
	    if (out)
		out++;
	} else if (!p->tab[c])
	    return 0;

	s++;
	p = p->next;
    }
    return 1;
}

/* Do the matching for a prefix. l and w contain the strings on the line and
 * for the generated word, respectively. In nlp we return a cline list for this
 * match. In lp we return the length of the matched string. rlp is used to
 * return a pointer to the last cline struct in the list returned in nlp, and
 * in bplp we return the relative position where the brace beginning would
 * have to be insrted in the string returned, which is the string to use
 * as the completion. */

static char *
match_pfx(char *l, char *w, Cline *nlp, int *lp, Cline *rlp, int *bplp)
{
    static unsigned char *ea;
    static int ealen = 0;
    static char *rw;
    static int rwlen;

    int ll = strlen(l), lw = strlen(w), oll = ll, olw = lw, mlw;
    int il = 0, iw = 0, t, bc = brpl;
    char *nw = rw;
    Cmlist ms;
    Cmatcher mp, lm = NULL;
    Cline lr = NULL;

    if (nlp) {
	*nlp = NULL;

	if (ll + 1 > ealen) {
	    /* This is the `in'/`out' string for pattern matching. */
	    if (ealen)
		zfree(ea, ealen);
	    ea = (unsigned char *) zalloc(ealen = ll + 20);
	}
    }
    while (ll && lw) {
	/* First try the matchers. */
	for (ms = mstack; ms; ms = ms->next) {
	    for (mp = ms->matcher; mp; mp = mp->next) {
		if (lm == mp)
		    continue;

		t = 1;
		if ((oll == ll || olw == lw) && !nlp && mp->wlen < 0)
		    /* If we were called recursively, don't use `*' patterns
		     * at the beginning (avoiding infinite recursion). */
		    t = 0;
		else if (mp->flags & CMF_LEFT) {
		    /* Try to match the left anchor, if any. */
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
			/* This is reached if we have a `*' pattern. */
			if ((t = pattern_match(mp->line, l, NULL, NULL))) {
			    if (mp->flags & CMF_RIGHT)
				/* Check if the anchor matches what's on the
				 * line. If it also matches the word, we don't
				 * use the matcher since we don't want one of
				 * these patterns on the line to match more
				 * than one such sub-string in the word. */
				t = (mp->right && ll >= mp->llen + mp->ralen &&
				     pattern_match(mp->right, l + mp->llen,
						   NULL, NULL) &&
				     lw >= mp->ralen &&
				     !pattern_match(mp->right, w, NULL, NULL));
			    if (t) {
				/* The anchor matched, so find out how many
				 * characters are matched by the `*' pattern.
				 * We do this by looping over the string
				 * and calling this function recursively. */
				int i = 0, j = iw, k = lw;
				int jj = il + mp->llen, kk = ll - mp->llen;
				char *p = l + mp->llen, *q = w;

				for (; k; i++, j++, k--, q++) {
				    if (match_pfx(p, q, NULL, NULL, NULL, NULL))
					break;
				}
				if (k && i) {
				    /* We found a position where the rest of
				     * the line matched again (k != 0) and
				     * we skipped over at least one character
				     * (i != 0), so add a cline for this */
				    if (nlp) {
					nw = addtoword(&rw, &rwlen, nw, mp,
						       l, w, i, 0);
					addtocline(nlp, &lr, l, mp->llen,
						   w, i, mp, 
						   ((mp->flags & CMF_LEFT) ?
						    CLF_SUF : 0) | CLF_VAR);
				    }
				    /* ...and adjust the pointers and counters
				     * to continue after the matched portion. */
				    w = q;
				    iw = j;
				    lw = k;
				    l = p;
				    il = jj;
				    ll = kk;
				    bc -= mp->llen;

				    /* In bc we count the characters used
				     * backward starting with the original
				     * position of the brace beginning. So, 
				     * if it becomes less than or equal to
				     * zero, we have reached the place where
				     * the brace string would have to be 
				     * inserted. */
				    if (bc <= 0 && bplp) {
					*bplp = nw - rw;
					bplp = NULL;
				    }
				    lm = mp;
				    break;
				}
				else
				    t = 0;
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
		    if (nlp) {
			nw = addtoword(&rw, &rwlen, nw, mp, l, w, mlw, 0);
			addtocline(nlp, &lr, l, mp->llen, w, mlw, mp, 0);
		    }
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
	    if (mp) {
		if (mp != lm)
		    lm = NULL;
		break;
	    }
	}
	if (t)
	    continue;
	if (*l == *w) {
	    /* Same character, take it. */
	    if (nlp) {
		nw = addtoword(&rw, &rwlen, nw, NULL, NULL, l, 1, 0);
		addtocline(nlp, &lr, l, 1, NULL, 0, NULL, 0);
	    }
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
	    lm = NULL;
	} else {
	    if (nlp && *nlp) {
		lr->next = freecl;
		freecl = *nlp;
	    }
	    return NULL;
	}
    }
    if (lp)
	*lp = iw;
    if (lw) {
	if (nlp) {
	    /* There is a unmatched portion in the word, keep it. */
	    if (rlp) {
		w = dupstring(w);
		addtocline(nlp, &lr, w, lw, w, -1, NULL, CLF_MID);

		*rlp = lr;
	    } else {
		addtocline(nlp, &lr, l, 0, dupstring(w), lw, NULL,
			   CLF_VAR | CLF_END);
		nw = addtoword(&rw, &rwlen, nw, NULL, NULL, w, lw, 0);
	    }
	}
    } else if (rlp) {
	if (nlp && lr) {
	    lr->next = freecl;
	    freecl = *nlp;
	}
	return NULL;
    }
    if (nlp && nw)
	*nw = '\0';

    if (ll) {
	if (nlp && *nlp) {
	    lr->next = freecl;
	    freecl = *nlp;
	}
	return NULL;
    }
    if (nlp)
	/* Finally, return the built match string. */
	return dupstring(rw);
    
    return ((char *) 1);
}

/* Do the matching for a suffix. Almost the same as match_pfx(), only in the
* other direction. */

static char *
match_sfx(char *l, char *w, Cline *nlp, int *lp, int *bslp)
{
    static unsigned char *ea;
    static int ealen = 0;
    static char *rw;
    static int rwlen;

    int ll = strlen(l), lw = strlen(w), mlw;
    int il = 0, iw = 0, t, bc = brsl;
    char *nw = rw;
    Cmlist ms;
    Cmatcher mp, lm = NULL;
    Cline lr = NULL;

    l += ll;
    w += lw;

    if (nlp) {
	*nlp = NULL;

	if (ll + 1 > ealen) {
	    if (ealen)
		zfree(ea, ealen);
	    ea = (unsigned char *) zalloc(ealen = ll + 20);
	}
    }
    while (ll && lw) {
	for (ms = mstack; ms; ms = ms->next) {
	    for (mp = ms->matcher; mp; mp = mp->next) {
		if (lm == mp)
		    continue;

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
			    if (mp->flags & CMF_LEFT)
				t = (mp->left && ll >= mp->llen + mp->lalen &&
				     pattern_match(mp->left,
						   l - mp->llen - mp->lalen,
						   NULL, NULL) &&
				     lw >= mp->lalen &&
				     !pattern_match(mp->left, w - mp->lalen,
						    NULL, NULL));
			    if (t) {
				int i = 0, j = iw, k = lw;
				int jj = il + mp->llen, kk = ll - mp->llen;
				char *p = l - mp->llen - 1, *q = w - 1;

				for (; k; i++, j++, k--, q--)
				    if (match_pfx(p, q, NULL, NULL, NULL, NULL))
					break;
				if (k && i) {
				    if (nlp) {
					nw = addtoword(&rw, &rwlen, nw, mp,
						       l - mp->llen, w - i,
						       i, 1);
					addtocline(nlp, &lr, l - mp->llen,
						   mp->llen, w - i, i, mp, 
						   ((mp->flags & CMF_LEFT) ?
						    CLF_SUF : 0) | CLF_VAR);
				    }
				    w = q + 1;
				    iw = j;
				    lw = k;
				    l = p + 1;
				    il = jj;
				    ll = kk;
				    bc -= mp->llen;

				    if (bc <= 0 && bslp) {
					*bslp = nw - rw;
					bslp = NULL;
				    }
				    lm = mp;
				    break;
				}
				else
				    t = 0;
			    }
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
		    if (nlp) {
			nw = addtoword(&rw, &rwlen, nw, mp, l - mp->llen,
				       w - mlw, mlw, 1);
			addtocline(nlp, &lr, l - mp->llen, mp->llen,
				   w - mlw, mlw, mp, 0);
		    }
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
	    if (mp) {
		if (mp != lm)
		    lm = NULL;
		break;
	    }
	}
	if (t)
	    continue;
	if (l[-1] == w[-1]) {
	    if (nlp) {
		nw = addtoword(&rw, &rwlen, nw, NULL, NULL, l - 1, 1, 1);
		addtocline(nlp, &lr, l - 1, 1, NULL, 0, NULL, 0);
	    }
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
	    lm = NULL;
	} else {
	    if (nlp && *nlp) {
		lr->next = freecl;
		freecl = *nlp;
	    }
	    return NULL;
	}
    }
    if (lp)
	*lp = iw;
    if (nlp && nw)
	*nw = '\0';

    if (ll) {
	if (nlp && *nlp) {
	    lr->next = freecl;
	    freecl = *nlp;
	}
	return NULL;
    }
    if (nlp)
	return dupstring(rw);

    return ((char *) 1);
}

/* Check if the word `w' is matched by the strings in pfx and sfx (the prefix
 * and the suffix from the line. In clp a cline list is returned for w.
 * qu is non-zero if the words has to be quoted before processed any further.
 * bpl and bsl are used to report the positions where the brace-strings in
 * the prefix and the suffix have to be re-inserted if this match is inserted
 * in the line.
 * The return value is the string to use as a completion or NULL if the prefix
 * and the suffix don't match the word w. */

static char *
comp_match(char *pfx, char *sfx, char *w, Cline *clp, int qu, int *bpl, int *bsl)
{
    char *r = NULL;
    Cline pli;
    int pl;

    if (qu)
	w = quotename(w, NULL, NULL, NULL);

    if (*sfx) {
	/* We have a suffix, so this gets a bit more complicated. */
	char *p, *s;
	int sl;
	Cline sli, last;

	/* First see if the prefix matches. In pl we get the length of
	 * the string matched by it. */
	if ((p = match_pfx(pfx, w, &pli, &pl, &last, bpl))) {
	    /* Then try to match the rest of the string with the suffix. */
	    if ((s = match_sfx(sfx, w + pl, &sli, &sl, bsl))) {
		int pml, sml;

		/* Now append the cline list for the suffix to the one
		 * for the prefix. */
		last->llen -= sl;
		last->next = revert_clines(sli);

		/* And store the correct parts of the prefix and the suffix
		 * in the cline struct in the middle. */
		pml = strlen(p);
		sml = strlen(s);
		r = (char *) halloc(pml + sml + last->llen + 1);
		strcpy(r, p);
		strncpy(r + pml, last->line, last->llen);
		strcpy(r + pml + last->llen, s);
	    } else {
		/* Suffix didn't match, so free the cline list for the
		 * prefix. */
		last->next = freecl;
		freecl = pli;

		return NULL;
	    }
	}
	else
	    return NULL;
    } else if (!(r = match_pfx(pfx, w, &pli, &pl, NULL, bpl)))
	/* We had only the prefix to match and that didn't match. */
	return NULL;

    /* If there are path prefixes or suffixes, pre- and append them to
     * the cline list built. */
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

    /* Ignored prefix. */
    if (m->ipre) {
	inststrlen(m->ipre, 1, (l = strlen(m->ipre)));
	r += l;
    }
    /* -P prefix. */
    if (m->pre) {
	inststrlen(m->pre, 1, (l = strlen(m->pre)));
	r += l;
    }
    /* Path prefix. */
    if (m->ppre) {
	inststrlen(m->ppre, 1, (l = strlen(m->ppre)));
	r += l;
    }
    /* The string itself. */
    inststrlen(m->str, 1, (l = strlen(m->str)));
    r += l;
    ocs = cs;
    /* Re-insert the brace beginning, if any. */
    if (brbeg && *brbeg) {
	cs = a + m->brpl + (m->pre ? strlen(m->pre) : 0);
	l = strlen(brbeg);
	brpcs = cs;
	inststrlen(brbeg, 1, l);
	r += l;
	ocs += l;
	cs = ocs;
    }
    /* Path suffix. */
    if (m->psuf) {
	inststrlen(m->psuf, 1, (l = strlen(m->psuf)));
	r += l;
    }
    /* Re-insert the brace end. */
    if (brend && *brend) {
	a = cs;
	cs -= m->brsl;
	ocs = brscs = cs;
	l = strlen(brend);
	inststrlen(brend, 1, l);
	r += l;
	cs = a + l;
    } else
	brscs = -1;
    /* -S suffix */
    if (m->suf) {
	inststrlen(m->suf, 1, (l = strlen(m->suf)));
	r += l;
    }
    lastend = cs;
    cs = ocs;
    return r;
}

/* This is used by compadd to add a couple of matches. The arguments are
 * the strings given via options. The last argument is the array with
 * the matches. */

/**/
void
addmatches(char *ipre, char *ppre, char *psuf, char *prpre, char *pre,
	   char *suf, char *group, char *rems, char *remf, char *ign,
	   int flags, int aflags, Cmatcher match, char *exp, char **argv)
{
    char *s, *t, *e, *me, *ms, *lipre = NULL, *lpre, *lsuf, **aign = NULL;
    int lpl, lsl, i, pl, sl, test, bpl, bsl, llpl, llsl;
    Aminfo ai;
    Cline lc = NULL;
    LinkList l;
    Cmatch cm;
    struct cmlist mst;
    Cmlist oms = mstack;

    /* Use menu-completion (-U)? */
    if ((aflags & CAF_MENU) && isset(AUTOMENU))
	usemenu = 1;

    /* Switch back to the heap that was used when the completion widget
     * was invoked. */
    SWITCHHEAPS(compheap) {
	HEAPALLOC {
	    if (exp) {
		expl = (Cexpl) halloc(sizeof(struct cexpl));
		expl->count = expl->fcount = 0;
		expl->str = dupstring(exp);
	    } else
		expl = NULL;

	    /* Store the matcher in our stack of matchers. */
	    if (match) {
		mst.next = mstack;
		mst.matcher = match;
		mstack = &mst;

		if (!mnum)
		    add_bmatchers(match);

		addlinknode(matchers, match);
		match->refc++;
	    }
	    if (mnum && (mstack || bmatchers))
		update_bmatchers();

	    /* Get the suffixes to ignore. */
	    if (ign)
		aign = get_user_var(ign);
	    /* Get the contents of the completion variables if we have
	     * to perform matching. */
	    if (aflags & CAF_MATCH) {
		lipre = dupstring(compiprefix);
		lpre = dupstring(compprefix);
		llpl = strlen(lpre);
		lsuf = dupstring(compsuffix);
		llsl = strlen(lsuf);
	    }
	    /* Now duplicate the strings we have from the command line. */
	    if (ipre)
		ipre = (lipre ? dyncat(lipre, ipre) : dupstring(ipre));
	    else if (lipre)
		ipre = lipre;
	    if (ppre) {
		ppre = dupstring(ppre);
		lpl = strlen(ppre);
	    } else
		lpl = 0;
	    if (psuf) {
		psuf = dupstring(psuf);
		lsl = strlen(psuf);
	    } else
		lsl = 0;
	    if (pre)
		pre = dupstring(pre);
	    if (suf)
		suf = dupstring(suf);
	    if (!prpre && (prpre = ppre)) {
		singsub(&prpre);
		untokenize(prpre);
	    } else
		prpre = dupstring(prpre);
	    /* Select the group in which to store the matches. */
	    if (group) {
		endcmgroup(NULL);
		begcmgroup(group, (aflags & CAF_NOSORT));
		if (aflags & CAF_NOSORT)
		    mgroup->flags |= CGF_NOSORT;
	    } else {
		endcmgroup(NULL);
		begcmgroup("default", 0);
	    }
	    /* Select the set of matches. */
	    if (aflags & CAF_ALT) {
		l = fmatches;
		ai = fainfo;
	    } else {
		l = matches;
		ai = ainfo;
	    }
	    if (remf) {
		remf = dupstring(remf);
		rems = NULL;
	    } else if (rems)
		rems = dupstring(rems);
	    /* Build the common -P prefix. */
    	    if (ai->pprefix) {
		if (pre)
		    ai->pprefix[pfxlen(ai->pprefix, pre)] = '\0';
		else
		    ai->pprefix[0] = '\0';
	    } else
		ai->pprefix = dupstring(pre ? pre : "");

	    /* Walk through the matches given. */
	    for (; (s = dupstring(*argv)); argv++) {
		sl = strlen(s);
		lc = NULL;
		ms = NULL;
		bpl = brpl;
		bsl = brsl;
		if ((!psuf || !*psuf) && aign) {
		    /* Do the suffix-test. If the match has one of the
		     * suffixes from ign, we put it in the alternate set. */
		    char **pt = aign;
		    int filell;

		    for (test = 1; test && *pt; pt++)
			if ((filell = strlen(*pt)) < sl
			    && !strcmp(*pt, s + sl - filell))
			    test = 0;

		    if (!test) {
			l = fmatches;
			ai = fainfo;
		    } else {
			l = matches;
			ai = ainfo;
		    }
		}
		if (aflags & CAF_MATCH) {
		    /* Do the matching. */
		    test = (sl >= llpl + llsl &&
			    strpfx(lpre, s) && strsfx(lsuf, s));
		    if (!test && mstack &&
			(ms = comp_match(lpre, lsuf, s,
					 &lc, (aflags & CAF_QUOTE),
					 &bpl, &bsl)))
			test = 1;

		    if (!test)
			continue;
		    pl = sl - llsl;
		    me = s + sl - llsl;
		    e = s + llpl;
		} else {
		    e = s;
		    me = s + sl;
		    pl = sl;
		}
		/* Quoting? */
		if (!(aflags & CAF_QUOTE)) {
		    int tmp = me - s;

		    s = quotename(s, &e, me, &tmp);
		    me = s + tmp;
		    sl = strlen(s);
		}
		/* The rest is almost the same as in addmatch(). */
		if (!ms) {
		    if (sl < ai->minlen)
			ai->minlen = sl;
		    if (!mstack && ai->firstm &&
			(i = sfxlen(ai->firstm->str, s)) < ai->suflen)
			ai->suflen = i;
		}
		t = s;
		if (ppre)
		    t = dyncat(ppre, t);
		if (!ms && mstack) {
		    int bl = ((aflags & CAF_MATCH) ? llpl : 0);
		    Cline *clp = &lc, tlc;
		    char *ss = dupstring(s), *ee = me + (ss - s);

		    if (ppre && *ppre) {
			*clp = tlc = getcline(NULL, 0, ppre, lpl, CLF_VAR);
			clp = &(tlc->next);
		    }
		    if (bl) {
			*clp = str_cline(ss, bl, &tlc);
			clp = &(tlc->next);
		    }
		    if (ee != ss + sl) {
			*clp = tlc = getcline(ss + bl, ee - ss - bl,
					      NULL, 0, CLF_MID);
			clp = &(tlc->next);
			*clp = str_cline(ee, (ss + sl) - ee, &tlc);
			clp = &(tlc->next);
		    } else {
			*clp = tlc = getcline(NULL, 0, ss + bl, sl - bl,
					      CLF_END | CLF_VAR);
			clp = &(tlc->next);
		    }
		    if (psuf && *psuf) {
			*clp = tlc = getcline(NULL, 0, psuf, lsl,
					      CLF_END | CLF_VAR);
			clp = &(tlc->next);
		    }
		    *clp = NULL;
		} else if (mstack) {
		    Cline tlc;

		    if (ppre && *ppre) {
			tlc = getcline(NULL, 0, ppre, lpl, CLF_VAR);
			tlc->next = lc;
			lc = tlc;
		    }
		    if (psuf && *psuf) {
			for (tlc = lc; tlc->next; tlc = tlc->next);
			tlc->next = getcline(NULL, 0, psuf, lsl,
					     CLF_END | CLF_VAR);
		    }
		}
		if (ipre && *ipre) {
		    Cline tlc = prepend_cline(ipre, lc);

		    ai->noipre = 0;
		    if (!ms && !mstack) {
			if ((aflags & CAF_MATCH) || ai->icpl > pl)
			    ai->icpl = pl;
			if ((aflags & CAF_MATCH) || ai->icsl > lsl)
			    ai->icsl = lsl;
			if (ai->iaprefix)
			    ai->iaprefix[pfxlen(ai->iaprefix, t)] = '\0';
			else
			    ai->iaprefix = dupstring(t);
		    } else
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
		if (!ms && !mstack) {
		    if ((aflags & CAF_MATCH) || ai->cpl > pl)
			ai->cpl = pl;
		    if ((aflags & CAF_MATCH) || ai->csl > lsl)
			ai->csl = lsl;
		    if (ai->aprefix)
			ai->aprefix[pfxlen(ai->aprefix, t)] = '\0';
		    else
			ai->aprefix = dupstring(t);
		} else
		    ai->linecl = join_clines(ai->linecl, lc);

		mnum++;
		ai->count++;

		/* Finally add the match. */
		cm = (Cmatch) halloc(sizeof(struct cmatch));
		cm->ppre = ppre;
		cm->psuf = psuf;
		cm->prpre = prpre;
		cm->str = (ms ? ms : dupstring(s));
		cm->ipre = cm->ripre = ipre;
		cm->pre = pre;
		cm->suf = suf;
		cm->flags = flags;
		cm->brpl = bpl;
		cm->brsl = bsl;
		cm->remf = remf;
		cm->rems = rems;
		addlinknode(l, cm);

		if (expl) {
		    if (l == matches)
			expl->count++;
		    else
			expl->fcount++;
		}
		if (!ms) {
		    if (!ai->firstm)
			ai->firstm = cm;
		    if ((aflags & CAF_MATCH) && !(e - (s + pl))) {
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
	    compnmatches = mnum;
	    if (exp)
		addexpl();
	} LASTALLOC;
    } SWITCHBACKHEAPS;

    /* We switched back to the current heap, now restore the stack of
     * matchers. */
    mstack = oms;
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
    char *e = NULL, *tt, *te, *ms = NULL;
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

    if (incompfunc)
	s = dupstring(s);
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
		if ((test = domatch(s, filecomp, 0))) {
		    e = s + sl;
		    cc = 1;
		}
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
	    if (addwhat == -7 && !findcmd(s, 0))
		return;
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
		e += (tt - s);
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
	    if (cp) {
		test = domatch(s, patcomp, 0);
		e = s + sl;
	    } else {
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
    if (!ms) {
	if (sl < ai->minlen)
	    ai->minlen = sl;
	if (!mstack && !ispattern && ai->firstm &&
	    (test = sfxlen(ai->firstm->str, s)) < ai->suflen)
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
    if (!ms && mstack) {
	Cline *clp = &lc, tlc;
	char *ss = dupstring(s), *ee = e + (ss - s);

	if (lppre && *lppre) {
	    *clp = str_cline(lppre, strlen(lppre), &tlc);
	    clp = &(tlc->next);
	}
	if (pl) {
	    *clp = str_cline(ss, pl, &tlc);
	    clp = &(tlc->next);
	}
	if (ee != ss + sl || (lpsuf && *lpsuf)) {
	    *clp = tlc = getcline(ss + pl, (ee - ss) - pl, NULL, 0, CLF_MID);
	    clp = &(tlc->next);
	    if (ee != ss + sl) {
		*clp = str_cline(ee, (ss + sl) - ee, &tlc);
		clp = &(tlc->next);
	    }
	    if (lpsuf && *lpsuf) {
		*clp = str_cline(lpsuf, strlen(lpsuf), &tlc);
		clp = &(tlc->next);
	    }
	} else {
	    *clp = tlc = getcline(NULL, 0, ss + pl, sl - pl,
				  CLF_END | CLF_VAR);
	    clp = &(tlc->next);
	}
	*clp = NULL;
    }
    if (ipre && *ipre) {
	Cline tlc = prepend_cline(ipre, lc);

	ai->noipre = 0;
	if (!ms && !mstack) {
	    ai->icpl = lppl + mpl;
	    ai->icsl = lpsl + msl;
	    if (ai->iaprefix)
		ai->iaprefix[pfxlen(ai->iaprefix, t)] = '\0';
	    else
		ai->iaprefix = dupstring(t);
	} else
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

    if (!ms && !mstack) {
	ai->cpl = lppl + mpl;
	ai->csl = lpsl + msl;
	if (ai->aprefix)
	    ai->aprefix[pfxlen(ai->aprefix, t)] = '\0';
	else
	    ai->aprefix = dupstring(t);
    } else
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
    if (incompfunc) {
	cm->pre = dupstring(curcc->prefix);
	cm->suf = dupstring(curcc->suffix);
    } else {
	cm->pre = curcc->prefix;
	cm->suf = curcc->suffix;
    }
    cm->flags = mflags | isf;
    cm->brpl = bpl;
    cm->brsl = bsl;
    cm->rems = cm->remf = NULL;
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
int
getcpat(char *str, int cpatindex, char *cpat, int class)
{
    char *s, *t, *p;
    int d = 0;

    if (!str || !*str)
	return -1;

    cpat = rembslash(cpat);

    if (!cpatindex)
	cpatindex++, d = 0;
    else if ((d = (cpatindex < 0)))
	cpatindex = -cpatindex;

    for (s = d ? str + strlen(str) - 1 : str;
	 d ? (s >= str) : *s;
	 d ? s-- : s++) {
	for (t = s, p = cpat; *t && *p; p++) {
	    if (class) {
		if (*p == *s && !--cpatindex)
		    return (int)(s - str + 1);
	    } else if (*t++ != *p)
		break;
	}
	if (!class && !*p && !--cpatindex)
	    return t - str;
    }
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
    if (!errflag && nonempty(l) &&
	((char *) peekfirst(l)) && ((char *) peekfirst(l))[0])
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
	LinkNode n;

	pushheap();

	ainfo = fainfo = NULL;
	matchers = newlinklist();

	/* Make sure we have the completion list and compctl. */
	if (makecomplist(s, incmd, lst)) {
	    /* Error condition: feeeeeeeeeeeeep(). */
	    feep();
	    clearlist = 1;
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
		while (!amatches->mcount)
		    amatches = amatches->next;
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
	for (n = firstnode(matchers); n; incnode(n))
	    freecmatcher((Cmatcher) getdata(n));

	ll = strlen((char *)line);
	if (cs > ll)
	    cs = ll;
	popheap();
    } LASTALLOC;
}

/* This calls the given function for new style completion. */

/**/
static void
callcompfunc(char *s, char *fn)
{
    List list;
    int lv = lastval;
    
    if ((list = getshfunc(fn)) != &dummy_list) {
	LinkList args = newlinklist();
	char **p, *tmp;
	int aadd = 0, usea = 1, icf = incompfunc, osc = sfcontext;
	
	addlinknode(args, fn);
	
	zsfree(compcontext);
	zsfree(compcommand);
	compcommand = "";
	if (inwhat == IN_MATH) {
	    if (insubscr) {
		compcontext = "subscript";
		compcommand = varname ? varname : "";
	    } else
		compcontext = "math";
	    usea = 0;
	} else if (lincmd)
	    compcontext = (insubscr ? "subscript" : "command");
	else if (linredir) {
	    compcontext = "redirect";
	    if (rdstr)
		compcommand = rdstr;
	} else
	    switch (inwhat) {
	    case IN_ENV:
		compcontext = "value";
		compcommand = varname;
		if (!clwpos) {
		    clwpos = 1;
		    zsfree(clwords[1]);
		    clwords[1] = ztrdup(s);
		}
		aadd = 1;
		break;
	    case IN_COND:
		compcontext = "condition";
		break;
	    default:
		if (cmdstr) {
		    compcontext = "argument";
		    compcommand = cmdstr;
		} else {
		    compcontext = "value";
		    if (clwords[0])
			compcommand = clwords[0];
		}
		aadd = 1;
	    }
	compcontext = ztrdup(compcontext);
	tmp = quotename(compcommand, NULL, NULL, NULL);
	untokenize(tmp);
	compcommand = ztrdup(tmp);
	if (usea && (!aadd || clwords[0]))
	    for (p = clwords + aadd; *p; p++) {
		tmp = dupstring(*p);
		untokenize(tmp);
		addlinknode(args, tmp);
	    }
	zsfree(compprefix);
	zsfree(compsuffix);
	if (unset(COMPLETEINWORD)) {
	    tmp = quotename(s, NULL, NULL, NULL);
	    untokenize(tmp);
	    compprefix = ztrdup(tmp);
	    compsuffix = ztrdup("");
	} else {
	    char *ss = s + offs, sav;
	    
	    tmp = quotename(s, &ss, NULL, NULL);
	    sav = *ss;
	    *ss = '\0';
	    untokenize(tmp);
	    compprefix = ztrdup(tmp);
	    *ss = sav;
	    untokenize(ss);
	    compsuffix = ztrdup(ss);
	}
	zsfree(compiprefix);
	compiprefix = ztrdup("");
	compcurrent = (usea ? (clwpos + 1 - aadd) : 1);
	compnmatches = mnum;
	incompfunc = 1;
	startparamscope();
	makecompparamsptr();
	makezleparams(1);
	sfcontext = SFC_CWIDGET;
	NEWHEAPS(compheap) {
	    doshfunc(fn, list, args, 0, 1);
	} OLDHEAPS;
	sfcontext = osc;
	endparamscope();
	lastcmd = 0;
	incompfunc = icf;
    }
    lastval = lv;
}

/* The beginning and end of a word range to be used by -l. */

static int brange, erange;

/* This is used to detect when and what to continue. */

static unsigned long ccont;

/* Create the completion list.  This is called whenever some bit of   *
 * completion code needs the list.                                    *
 * Along with the list is maintained the prefixes/suffixes etc.  When *
 * any of this becomes invalid -- e.g. if some text is changed on the *
 * command line -- invalidatelist() should be called, to set          *
 * validlist to zero and free up the memory used.  This function      *
 * returns non-zero on error.                                         */

/**/
static int
makecomplist(char *s, int incmd, int lst)
{
    struct cmlist ms;
    Cmlist m;
    char *os = s;

    /* We build a copy of the list of matchers to use to make sure that this
     * works even if a shell function called from the completion code changes
     * the global matchers. */

    if ((m = cmatcher)) {
	Cmlist mm, *mp = &mm;

	for (; m; m = m->next) {
	    *mp = (Cmlist) halloc(sizeof(struct cmlist));
	    (*mp)->matcher = m->matcher;
	    (*mp)->next = NULL;
	    mp = &((*mp)->next);
	    addlinknode(matchers, m->matcher);
	    m->matcher->refc++;
	}
	m = mm;
    }
    compmatcher = 1;

    /* Walk through the global matchers. */
    for (;;) {
	bmatchers = NULL;
	if (m) {
	    ms.next = NULL;
	    ms.matcher = m->matcher;
	    mstack = &ms;

	    /* Store the matchers used in the bmatchers list which is used
	     * when building new parts for the string to insert into the 
	     * line. */
	    add_bmatchers(m->matcher);
	} else
	    mstack = NULL;

	ainfo = (Aminfo) hcalloc(sizeof(struct aminfo));
	fainfo = (Aminfo) hcalloc(sizeof(struct aminfo));

	ainfo->minlen = ainfo->suflen = 
	    fainfo->minlen = fainfo->suflen = 10000;
	ainfo->noipre = fainfo->noipre= 1;

	freecl = NULL;

	if (!validlist)
	    lastambig = 0;
	amatches = 0;
	mnum = 0;
	begcmgroup("default", 0);

	ccused = newlinklist();
	ccstack = newlinklist();

	s = dupstring(os);
	if (compfunc)
	    callcompfunc(s, compfunc);
	else
	    makecomplistglobal(s, incmd, lst, 0);

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
	compmatcher++;
    }
    return 1;
}

/* This should probably be moved into tokenize(). */

/**/
static char *
ctokenize(char *p)
{
    char *r = p;
    int bslash = 0;

    tokenize(p);

    for (p = r; *p; p++) {
	if (*p == '\\')
	    bslash = 1;
	else {
	    if (*p == '$' || *p == '=' || *p == '{' || *p == '}') {
		if (bslash)
		    p[-1] = Bnull;
		else
		    *p = (*p == '$' ? String :
			  (*p == '=' ? Equals :
			   (*p == '{' ? Inbrace : Outbrace)));
	    }
	    bslash = 0;
	}
    }
    return r;
}

/**/
char *
comp_str(int *ipl, int *pl, int untok)
{
    char *p = dupstring(compprefix);
    char *s = dupstring(compsuffix);
    char *ip = dupstring(compiprefix);
    char *str;
    int lp, ls, lip;

    if (!untok) {
	ctokenize(p);
	remnulargs(p);
	ctokenize(s);
	remnulargs(s);
	ctokenize(ip);
	remnulargs(ip);
    }
    ls = strlen(s);
    lip = strlen(ip);
    lp = strlen(p);
    str = halloc(lip + lp + ls + 1);
    strcpy(str, ip);
    strcat(str, p);
    strcat(str, s);

    if (ipl)
	*ipl = lip;
    if (pl)
	*pl = lp;

    return str;
}

/**/
void
makecomplistcall(Compctl cc)
{
    SWITCHHEAPS(compheap) {
	HEAPALLOC {
	    int ooffs = offs, lip, lp;
	    char *str = comp_str(&lip, &lp, 0);

	    offs = lip + lp;
	    cc->refc++;
	    ccont = 0;
	    makecomplistor(cc, str, lincmd, lip, 0);
	    offs = ooffs;
	    compnmatches = mnum;
	} LASTALLOC;
    } SWITCHBACKHEAPS;
}

/* A simple counter to avoid endless recursion between old and new style *
 * completion. */

static int cdepth = 0;

#define MAX_CDEPTH 16

/**/
int
makecomplistctl(int flags)
{
    int ret;

    if (cdepth == MAX_CDEPTH)
	return 0;

    cdepth++;
    SWITCHHEAPS(compheap) {
	HEAPALLOC {
	    int ooffs = offs, lip, lp;
	    char *str = comp_str(&lip, &lp, 0), *t;
	    char *os = cmdstr, **ow = clwords, **p, **q;
	    int on = clwnum, op = clwpos;

	    clwnum = arrlen(pparams) + 1;
	    clwpos = compcurrent - 1;
	    cmdstr = ztrdup(compcommand);
	    clwords = (char **) zalloc((clwnum + 1) * sizeof(char *));
	    clwords[0] = ztrdup(cmdstr);
	    for (p = pparams, q = clwords + 1; *p; p++, q++) {
		t = dupstring(*p);
		ctokenize(t);
		remnulargs(t);
		*q = ztrdup(t);
	    }
	    *q = NULL;
	    offs = lip + lp;
	    incompfunc = 2;
	    ret = makecomplistglobal(str,
				     (!clwpos &&
				      !strcmp(compcontext, "command")),
				     COMP_COMPLETE, flags);
	    incompfunc = 1;
	    offs = ooffs;
	    compnmatches = mnum;
	    zsfree(cmdstr);
	    freearray(clwords);
	    cmdstr = os;
	    clwords = ow;
	    clwnum = on;
	    clwpos = op;
	} LASTALLOC;
    } SWITCHBACKHEAPS;
    cdepth--;

    return ret;
}

/* This function gets the compctls for the given command line and *
 * adds all completions for them. */

/**/
static int
makecomplistglobal(char *os, int incmd, int lst, int flags)
{
    Compctl cc;
    char *s;

    ccont = CC_CCCONT;

    if (inwhat == IN_ENV) {
        /* Default completion for parameter values. */
        cc = &cc_default;
    } else if (inwhat == IN_MATH) {
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
    else
	/* Otherwise get the matches for the command. */
	return makecomplistcmd(os, incmd, flags);

    if (cc) {
	/* First, use the -T compctl. */
	if (!(flags & CFN_FIRST)) {
	    makecomplistcc(&cc_first, os, incmd);

	    if (!(ccont & CC_CCCONT))
		return 0;
	}
	makecomplistcc(cc, os, incmd);
	return 1;
    }
    return 0;
}

/* This produces the matches for a command. */

/**/
static int
makecomplistcmd(char *os, int incmd, int flags)
{
    Compctl cc;
    Compctlp ccp;
    char *s;
    int ret = 0;

    /* First, use the -T compctl. */
    if (!(flags & CFN_FIRST)) {
	makecomplistcc(&cc_first, os, incmd);

	if (!(ccont & CC_CCCONT))
	    return 0;
    }
    /* Then search the pattern compctls, with the command name and the *
     * full pathname of the command. */
    if (cmdstr) {
	ret |= makecomplistpc(os, incmd);
	if (!(ccont & CC_CCCONT))
	    return ret;
    }
    /* If the command string starts with `=', try the path name of the *
     * command. */
    if (cmdstr && cmdstr[0] == Equals) {
	char *c = findcmd(cmdstr + 1, 1);

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
	    (cc = ccp->cc))))) {
	if (flags & CFN_DEFAULT)
	    return ret;
	cc = &cc_default;
    } else
	ret|= 1;
    makecomplistcc(cc, os, incmd);
    return ret;
}

/* This add the matches for the pattern compctls. */

/**/
static int
makecomplistpc(char *os, int incmd)
{
    Patcomp pc;
    Comp pat;
    char *s = findcmd(cmdstr, 1);
    int ret = 0;

    for (pc = patcomps; pc; pc = pc->next) {
	if ((pat = parsereg(pc->pat)) &&
	    (domatch(cmdstr, pat, 0) ||
	     (s && domatch(s, pat, 0)))) {
	    makecomplistcc(pc->cc, os, incmd);
	    ret |= 2;
	    if (!(ccont & CC_CCCONT))
		return ret;
	}
    }
    return ret;
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
			s = ztrdup(clwpos < clwnum ? os : "");
			untokenize(s);
			if (isset(COMPLETEINWORD)) s[offs] = '\0';
			sc = rembslash(cc->u.s.s[i]);
			a = strlen(sc);
			if (!strncmp(s, sc, a)) {
			    compadd = (cc->type == CCT_CURSUF ? a : 0);
			    t = 1;
			}
			break;
		    case CCT_CURSUB:
		    case CCT_CURSUBC:
			if (clwpos < 0 || clwpos >= clwnum)
			    t = 0;
			else {
			    s = ztrdup(os);
			    untokenize(s);
			    if (isset(COMPLETEINWORD)) s[offs] = '\0';
			    a = getcpat(s,
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

    if (incompfunc != 1 && findnode(ccstack, cc))
	return;

    addlinknode(ccstack, cc);

    if (incompfunc != 1 && allccs) {
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

	if (!mnum)
	    add_bmatchers(cc->matcher);

	addlinknode(matchers, cc->matcher);
	cc->matcher->refc++;
    }
    if (mnum && (mstack || bmatchers))
	update_bmatchers();

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
	if (ic == Tilde) {
	    char *oi = ipre;

	    ipre = (ipre ? dyncat("~", ipre) : "~");
	    maketildelist();
	    ipre = oi;
	} else if (ic == Equals) {
	    /* Completion after `=', get the command names from *
	     * the cmdnamtab and aliases from aliastab.         */
	    char *oi = ipre;

	    ipre = (ipre ? dyncat("=", ipre) : "=");
	    if (isset(HASHLISTALL))
		cmdnamtab->filltable(cmdnamtab);
	    dumphashtable(cmdnamtab, -7);
	    dumphashtable(aliastab, -2);
	    ipre = oi;
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
	if (cc->func[0] == ' ')
	    /* Temporary hack for access to new style completione. */
	    callcompfunc(os, cc->func + 1);
	else {
	    /* This handles the compctl -K flag. */
	    List list;
	    char **r;
	    int lv = lastval;
	    
	    /* Get the function. */
	    if ((list = getshfunc(cc->func)) != &dummy_list) {
		/* We have it, so build a argument list. */
		LinkList args = newlinklist();
		int osc = sfcontext;
		
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
		if (incompfunc != 1)
		    incompctlfunc = 1;
		sfcontext = SFC_COMPLETE;
		/* Call the function. */
		doshfunc(cc->func, list, args, 0, 1);
		sfcontext = osc;
		incompctlfunc = 0;
		/* And get the result from the reply parameter. */
		if ((r = get_user_var("reply")))
		    while (*r)
			addmatch(*r++, NULL);
	    }
	    lastval = lv;
	}
    }
    if (cc->mask & (CC_JOBS | CC_RUNNING | CC_STOPPED)) {
	/* Get job names. */
	int i;
	char *j, *jj;

	for (i = 0; i < MAXJOB; i++)
	    if ((jobtab[i].stat & STAT_INUSE) &&
		jobtab[i].procs && jobtab[i].procs->text) {
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
	    int osc = sfcontext;

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
	    if (incompfunc != 1)
		incompctlfunc = 1;
	    sfcontext = SFC_COMPLETE;
	    doshfunc(cc->ylist, list, args, 0, 1);
	    sfcontext = osc;
	    incompctlfunc = 0;
	    uv = "reply";
	}
	
	if ((tt = cc->explain)) {
	    tt = dupstring(tt);
	    if ((cc->mask & CC_EXPANDEXPL) && !parsestr(tt)) {
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
	tt = dupstring(tt);
	if ((cc->mask & CC_EXPANDEXPL) && !parsestr(tt)) {
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
	makecomplistcmd(s, incmd, CFN_FIRST);

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
    lastambig = menucmp = validlist = showinglist = fromcomp = 0;
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

	if ((arr = getaparam(nam)) || (arr = gethparam(nam)))
	    return (incompfunc ? arrdup(arr) : arr);

	if ((val = getsparam(nam))) {
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
	    for (; bp[1] && !strcmp((*ap)->str, (bp[1])->str); bp++)
		(bp[1])->flags |= CMF_NOLIST;
	}
	for (ap = rp; *ap; ap++)
	    if ((*ap)->flags & CMF_NOLIST)
		nl++;
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
    mgroup->name = dupstring(n);
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
    r->rems = ztrdup(m->rems);
    r->remf = ztrdup(m->remf);

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
	    if (g->ylist) {
		g->lcount = arrlen(g->ylist);
		smatches = 2;
	    }
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
    zsfree(m->rems);
    zsfree(m->remf);

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

    menucmp = 0;

    /* If we have to insert the first match, call do_single().  This is *
     * how REC_EXACT takes effect.  We effectively turn the ambiguous   *
     * completion into an unambiguous one.                              */
    if (ainfo && ainfo->exact == 1 && isset(RECEXACT) && !(fromcomp & FC_LINE) &&
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
	VARARR(char, oline, ll);
	int sl = 0, oll = ll;
	int ocs, pl = 0, l, lp, ls, la = 0;
	char *ps;
	Cline lc;

	if (!ainfo)
	    return;

	/* Copy the line buffer to be able to easily test if it changed. */
	memcpy(oline, line, ll);

	fixsuffix();

	/* First remove the old string from the line. */
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
	    lastend = cs;
	    cs = ocs;
	}
	/* la is non-zero if listambiguous may be used. Copying and
	 * comparing the line looks like BFI but it is the easiest
	 * solution. Really. */
	la = (ll != oll || strncmp(oline, (char *) line, ll));

	/* If REC_EXACT and AUTO_MENU are set and what we inserted is an  *
	 * exact match, we want menu completion the next time round       *
	 * so we set fromcomp,to ensure that the word on the line is not  *
	 * taken as an exact match. Also we remember if we just moved the *
	 * cursor into the word.                                          */
	fromcomp = ((isset(AUTOMENU) ? FC_LINE : 0) |
		    ((atend && cs != lastend) ? FC_INWORD : 0));

	/*
	 * If the LIST_AMBIGUOUS option (meaning roughly `show a list only *
	 * if the completion is completely ambiguous') is set, and some    *
	 * prefix was inserted, return now, bypassing the list-displaying  *
	 * code.  On the way, invalidate the list and note that we don't   *
	 * want to enter an AUTO_MENU imediately.                          */
	if(isset(LISTAMBIGUOUS) && la) {
	    int fc = fromcomp;

	    invalidatelist();
	    fromcomp = fc;
	    lastambig = 0;
	    return;
	}
    }
    /* At this point, we might want a completion listing.  Show the listing *
     * if it is needed.                                                     */
    if (isset(LISTBEEP))
	feep();
    if (isset(AUTOLIST) && !isset(BASHAUTOLIST) && !amenu && !showinglist &&
	smatches >= 2)
	showinglist = -2;
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
	menulen -= menuinsc;
	if (menuwe) {
	    menuend += menuinsc;
	    if (m->flags & CMF_REMOVE) {
		makesuffixstr(m->remf, m->rems, menuinsc);
		if (menuinsc == 1)
		    suffixlen[STOUC(m->suf[0])] = 1;
	    }
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
		    sprintf(p, "%s%s%s", ppre, str, psuf);
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
    int nlines = 0, ncols, nlist = 0, longest = 1, pnl = 0;
    int of = isset(LISTTYPES), opl = 0;
    int listmax = getiparam("LISTMAX");

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
    }
    longest += 2 + of;
    if ((ncols = (columns + 1) / longest)) {
	for (g = amatches; g; g = g->next)
	    nlines += (g->lcount + ncols - 1) / ncols;
    } else {
	ncols = 1;
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
		int n = g->lcount, nl = (n + ncols - 1) / ncols, nc = nl, i, a;
		char **pq;

		while (n && nl--) {
		    i = ncols;
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
			pq += nc;
			n--;
		    }
		    if (n)
			putc('\n', shout);
		    pp++;
		}
	    }
	}
	else if (g->lcount) {
	    int n = g->lcount, nl = (n + ncols - 1) / ncols, nc = nl, i, j, a;
	    Cmatch *q;

	    if (n && pnl) {
		putc('\n', shout);
		pnl = 0;
	    }
	    for (p = skipnolist(g->matches); n && nl--;) {
		i = ncols;
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
			for (j = nc; j && *q; j--)
			    q = skipnolist(q + 1);
		}
		if (n) {
		    putc('\n', shout);
		    if (n && nl)
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
    str = findcmd(s, 1);
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
