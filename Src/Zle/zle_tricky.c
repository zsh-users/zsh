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

/* The line before completion was tried. */

static char *origline;
static int origcs;

/* wb and we hold the beginning/end position of the word we are completing. */

static int wb, we;

/* offs is the cursor position within the tokenized *
 * current word after removing nulargs.             */

static int offs;

/* the last completion widget called */

static Widget lastcompwidget;

/* These control the type of completion that will be done.  They are      *
 * affected by the choice of ZLE command and by relevant shell options.   *
 * usemenu is set to 2 if we have to start automenu and 3 if we have to   *
 * insert a match as if for menucompletion but without really stating it. */

static int usemenu, useglob, useexact, useline, uselist;

/* Non-zero if we should keep an old list. */

static int oldlist, oldins;

/* Non-zero if we have to redisplay the list of matches. */

static int showagain = 0;

/* The match and group number to insert when starting menucompletion.   */

static int insmnum, insgnum, insgroup, insspace;

/* This is used to decide when the cursor should be moved to the end of    *
 * the inserted word: 0 - never, 1 - only when a single match is inserted, *
 * 2 - when a full match is inserted (single or menu), 3 - always.         */

static int movetoend;

/* != 0 if we are in the middle of a menu completion and number of matches
* accepted with accept-and-menu-complete */

/**/
int menucmp, menuacc;

/* Information about menucompletion. */

/**/
struct menuinfo minfo;

/* This is for completion inside a brace expansion. brbeg and brend hold  *
 * strings that were temporarily removed from the string to complete.     *
 * brpl and brsl, hold the offset of these strings.                 *
 * brpcs and brscs hold the positions of the re-inserted string in the    *
 * line.                                                                  */

static char *brbeg = NULL, *brend = NULL;
static int brpl, brsl, brpcs, brscs, qbrpl, qbrsl, hasunqu;

/* The list of matches.  fmatches contains the matches we first ignore *
 * because of fignore.                                                 */

static LinkList matches, fmatches;

/* This holds the list of matches-groups. lmatches is a pointer to the  *
 * last element in this list. */

/**/
Cmgroup pmatches, amatches, lmatches;

/* Non-zero if we have permanently allocated matches. */

/**/
int hasperm;

/* Number of permanently allocated matches and groups. */

static int permmnum, permgnum;

/* The total number of matches and the number of matches to be listed. */

static int nmatches, smatches;

/* !=0 if we have a valid completion list. */

/**/
int validlist;

/* This flag is non-zero if we are completing a pattern (with globcomplete) */

static int ispattern, haspattern;

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
 * ipre,ripre  -- the ignored prefix (quoted and unquoted)                 *
 * isuf        -- the ignored suffix                                       *
 * qipre, qisuf-- ingnored quoted string                                   *
 * autoq       -- quotes to automatically insert                           *
 *                                                                         *
 * The integer variables hold the lengths of lpre, lsuf, rpre, rsuf,       *
 * fpre, fsuf, lppre, and lpsuf.  noreal is non-zero if we have rpre/rsuf. */

static char *lpre, *lsuf;
static char *rpre, *rsuf;
static char *ppre, *psuf, *lppre, *lpsuf, *prpre;
static char *fpre, *fsuf;
static char *ipre, *ripre;
static char *isuf;
static char *qfpre, *qfsuf, *qrpre, *qrsuf, *qlpre, *qlsuf;
static char *qipre, *qisuf, autoq;
static int lpl, lsl, rpl, rsl, fpl, fsl, lppl, lpsl;
static int noreal;

/* A parameter expansion prefix (like ${). */

static char *parpre;

/* This is either zero or equal to the special character the word we are *
 * trying to complete starts with (e.g. Tilde or Equals).                */

static char ic;

/* This variable says what we are currently adding to the list of matches. */

static int addwhat;

/* This holds the word we are completing in quoted from. */

static char *qword;

/* The current group of matches. */

static Cmgroup mgroup;

/* Match counters: all matches, normal matches (not alternate set). */

static int mnum, nmnum;

/* The match counter when unambig_data() was called. */

static int unambig_mnum;

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

/* Information about what to put on the line as the unambiguous string.
 * The code always keeps lists of these structs up to date while
 * matches are added (in the aminfo structs below).
 * The lists have two levels: in the first one we have one struct per
 * word-part, where parts are separated by the anchors of `*' patterns.
 * These structs have pointers (in the prefix and suffix fields) to
 * lists of cline structs describing the strings before or after the
 * the anchor. */

typedef struct cline *Cline;
typedef struct clsub Clsub;

struct cline {
    Cline next;
    int flags;
    char *line;
    int llen;
    char *word;
    int wlen;
    char *orig;
    int olen;
    int slen;
    Cline prefix, suffix;
};

#define CLF_MISS  1
#define CLF_DIFF  2
#define CLF_SUF   4
#define CLF_MID   8
#define CLF_NEW  16
#define CLF_LINE 32
#define CLF_JOIN 64

/* A heap of free Cline structures. */

static Cline freecl;

/* Information for ambiguous completions. One for fignore ignored and   *
 * one for normal completion. */

typedef struct aminfo *Aminfo;

struct aminfo {
    Cmatch firstm;		/* the first match                        */
    int exact;			/* if there was an exact match            */
    Cmatch exactm;		/* the exact match (if any)               */
    int count;			/* number of matches                      */
    Cline line;			/* unambiguous line string                */
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

/**/
int lastambig;

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

/* Arguments for and return value of completion widget. */

static char **cfargs;
static int cfret;

/**/
int
completecall(char **args)
{
    cfargs = args;
    cfret = 0;
    compfunc = compwidget->u.comp.func;
    if (compwidget->u.comp.fn(zlenoargs) && !cfret)
	cfret = 1;
    compfunc = NULL;

    return cfret;
}

/**/
int
completeword(char **args)
{
    usemenu = !!isset(MENUCOMPLETE);
    useglob = isset(GLOBCOMPLETE);
    if (c == '\t' && usetab())
	return selfinsert(args);
    else {
	int ret;
	if (lastambig == 1 && isset(BASHAUTOLIST) && !usemenu && !menucmp) {
	    ret = docomplete(COMP_LIST_COMPLETE);
	    lastambig = 2;
	} else
	    ret = docomplete(COMP_COMPLETE);
	return ret;
    }
}

/**/
int
menucomplete(char **args)
{
    usemenu = 1;
    useglob = isset(GLOBCOMPLETE);
    if (c == '\t' && usetab())
	return selfinsert(args);
    else
	return docomplete(COMP_COMPLETE);
}

/**/
int
listchoices(char **args)
{
    usemenu = !!isset(MENUCOMPLETE);
    useglob = isset(GLOBCOMPLETE);
    return docomplete(COMP_LIST_COMPLETE);
}

/**/
int
spellword(char **args)
{
    usemenu = useglob = 0;
    return docomplete(COMP_SPELL);
}

/**/
int
deletecharorlist(char **args)
{
    Cmgroup mg = minfo.group;
    Cmatch *mc = minfo.cur;
    int ret;

    usemenu = !!isset(MENUCOMPLETE);
    useglob = isset(GLOBCOMPLETE);
    if (cs != ll) {
	fixsuffix();
	ret = deletechar(args);
    } else
	ret = docomplete(COMP_LIST_COMPLETE);

    minfo.cur = mc;
    minfo.group = mg;
    return ret;
}

/**/
int
expandword(char **args)
{
    usemenu = useglob = 0;
    if (c == '\t' && usetab())
	return selfinsert(args);
    else
	return docomplete(COMP_EXPAND);
}

/**/
int
expandorcomplete(char **args)
{
    usemenu = !!isset(MENUCOMPLETE);
    useglob = isset(GLOBCOMPLETE);
    if (c == '\t' && usetab())
	return selfinsert(args);
    else {
	int ret;
	if (lastambig == 1 && isset(BASHAUTOLIST) && !usemenu && !menucmp) {
	    ret = docomplete(COMP_LIST_COMPLETE);
	    lastambig = 2;
	} else
	    ret = docomplete(COMP_EXPAND_COMPLETE);
	return ret;
    }
}

/**/
int
menuexpandorcomplete(char **args)
{
    usemenu = 1;
    useglob = isset(GLOBCOMPLETE);
    if (c == '\t' && usetab())
	return selfinsert(args);
    else
	return docomplete(COMP_EXPAND_COMPLETE);
}

/**/
int
listexpand(char **args)
{
    usemenu = !!isset(MENUCOMPLETE);
    useglob = isset(GLOBCOMPLETE);
    return docomplete(COMP_LIST_EXPAND);
}

/**/
int
reversemenucomplete(char **args)
{
    if (!menucmp)
	return menucomplete(args);

    HEAPALLOC {
	if (minfo.cur == (minfo.group)->matches) {
	    do {
		if (!(minfo.group = (minfo.group)->prev))
		    minfo.group = lmatches;
	    } while (!(minfo.group)->mcount);
	    minfo.cur = (minfo.group)->matches + (minfo.group)->mcount - 1;
	} else
	    minfo.cur--;
	metafy_line();
	do_single(*(minfo.cur));
	unmetafy_line();
    } LASTALLOC;
    return 0;
}

/* Accepts the current completion and starts a new arg, *
 * with the next completions. This gives you a way to   *
 * accept several selections from the list of matches.  */

/**/
void
acceptlast(void)
{
    menuacc++;

    if (brbeg && *brbeg) {
	int l;

	iremovesuffix(',', 1);

	l = (brscs >= 0 ? brscs : cs) - brpcs;

	zsfree(brbeg);
	brbeg = (char *) zalloc(l + 2);
	memcpy(brbeg, line + brpcs, l);
	brbeg[l] = ',';
	brbeg[l + 1] = '\0';
    } else {
	int l;

	cs = minfo.pos + minfo.len + minfo.insc;
	iremovesuffix(' ', 1);
	l = cs;
	cs = minfo.pos + minfo.len + minfo.insc - (*(minfo.cur))->qisl;
	if (cs < l)
	    foredel(l - cs);
	else if (cs > ll)
	    cs = ll;
	inststrlen(" ", 1, 1);
	if (parpre)
	    inststr(parpre);
	minfo.insc = minfo.len = 0;
	minfo.pos = cs;
	minfo.we = 1;
    }
}

/**/
int
acceptandmenucomplete(char **args)
{
    if (!menucmp)
	return 1;
    acceptlast();
    return menucomplete(args);
}

/* These are flags saying if we are completing in the command *
 * position, in a redirection, or in a parameter expansion.   */

static int lincmd, linredir, ispar, parq, eparq, linwhat, linarr;

/* The string for the redirection operator. */

static char *rdstr;

/* This holds the name of the current command (used to find the right *
 * compctl).                                                          */

static char *cmdstr;

/* This hold the name of the variable we are working on. */

static char *varname;

/* != 0 if we are in a subscript */

static int insubscr;

/* Parameter pointer for completing keys of an assoc array. */

static Param keypm;

/* 1 if we are completing in a quoted string (or inside `...`) */

/**/
int instring, inbackt;

/* Convenience macro for calling bslashquote() (formerly quotename()). *
 * This uses the instring variable above.                              */

#define quotename(s, e) bslashquote(s, e, instring)

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

/* Check if we have to complete a parameter name. */

static char *
check_param(char *s, int set, int test)
{
    char *p;

    zsfree(parpre);
    parpre = NULL;

    if (!test)
	ispar = parq = eparq = 0;
    /* Try to find a `$'. */
    for (p = s + offs; p > s && *p != String && *p != Qstring; p--);
    if (*p == String || *p == Qstring) {
	/* Handle $$'s */
	while (p > s && (p[-1] == String || p[-1] == Qstring))
	    p--;
	while ((p[1] == String || p[1] == Qstring) &&
	       (p[2] == String || p[2] == Qstring))
	    p += 2;
    }
    if ((*p == String || *p == Qstring) && p[1] != Inpar && p[1] != Inbrack) {
	/* This is really a parameter expression (not $(...) or $[...]). */
	char *b = p + 1, *e = b;
	int n = 0, br = 1;

	if (*b == Inbrace) {
	    char *tb = b;

	    /* If this is a ${...}, see if we are before the '}'. */
	    if (!skipparens(Inbrace, Outbrace, &tb))
		return NULL;

	    /* Ignore the possible (...) flags. */
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
	if (br) {
	    while (*e == (test ? Dnull : '"'))
		e++, parq++;
	    if (!test)
		b = e;
	}
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
		   (comppatmatch && *comppatmatch &&
		    (*e == Star || *e == Quest)))
		e++;

	/* Now make sure that the cursor is inside the name. */
	if (offs <= e - s && offs >= b - s && n <= 0) {
	    char sav;

	    if (br) {
		p = e;
		while (*p == (test ? Dnull : '"'))
		    p++, parq--, eparq++;
	    }
	    /* It is. */
	    if (test)
		return b;
	    /* If we were called from makecomplistflags(), we have to set the
	     * global variables. */

	    if (set) {
		if (br >= 2)
		    mflags |= CMF_PARBR;

		/* Get the prefix (anything up to the character before the name). */
		isuf = dupstring(e);
		untokenize(isuf);
		*e = '\0';
		ripre = dupstring(s);
		ripre[b - s] = '\0';
		ipre = dupstring(ripre);
		untokenize(ipre);
	    }
	    else
		parq = eparq = 0;

	    /* Save the prefix. */
	    if (incompfunc) {
		sav = *b;
		*b = '\0';
		untokenize(parpre = ztrdup(s));
		*b = sav;
	    }
	    /* And adjust wb, we, and offs again. */
	    offs -= b - s;
	    wb = cs - offs;
	    we = wb + e - b;
	    ispar = (br >= 2 ? 2 : 1);
	    b[we-wb] = '\0';
	    return b;
	}
    }
    return NULL;
}

/* The main entry point for completion. */

/**/
static int
docomplete(int lst)
{
    char *s, *ol;
    int olst = lst, chl = 0, ne = noerrs, ocs, ret = 0, omc = menucmp;

    if (showagain && validlist)
	showinglist = -2;
    showagain = 0;

    /* If we are doing a menu-completion... */

    if (menucmp && lst != COMP_LIST_EXPAND && 
	(!compwidget || compwidget == lastcompwidget)) {
	do_menucmp(lst);
	return 0;
    }
    lastcompwidget = compwidget;

    /* We may have to reset the cursor to its position after the   *
     * string inserted by the last completion. */

    if (fromcomp & FC_INWORD)
	if ((cs = lastend) > ll)
	    cs = ll;

    /* Check if we have to start a menu-completion (via automenu). */

    if (isset(AUTOMENU) && lastambig &&
	(!isset(BASHAUTOLIST) || lastambig == 2))
	usemenu = 2;

    /* Expand history references before starting completion.  If anything *
     * changed, do no more.                                               */

    if (doexpandhist())
	return 0;

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
    zsfree(qipre);
    qipre = ztrdup("");
    zsfree(qisuf);
    qisuf = ztrdup("");
    autoq = '\0';
    /* Get the word to complete. */
    noerrs = 1;
    origline = dupstring((char *) line);
    origcs = cs;
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
	    return 1;
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
		    /* Check if there is a parameter expression. */
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
	    char *x, *q, *ox;

	    for (q = s; *q; q++)
		if (INULL(*q))
		    *q = Nularg;
	    cs = wb;
	    foredel(we - wb);
	    HEAPALLOC {
		untokenize(x = ox = dupstring(s));
		if (*s == Tilde || *s == Equals || *s == String)
		    *x = *s;
		spckword(&x, 0, lincmd, 0);
		ret = !strcmp(x, ox);
	    } LASTALLOC;
	    untokenize(x);
	    inststr(x);
	} else if (COMP_ISEXPAND(lst)) {
	    /* Do expansion. */
	    char *ol = (olst == COMP_EXPAND_COMPLETE) ?
		dupstring((char *)line) : (char *)line;
	    int ocs = cs, ne = noerrs;

	    noerrs = 1;
	    ret = doexpansion(s, lst, olst, lincmd);
	    lastambig = 0;
	    noerrs = ne;

	    /* If expandorcomplete was invoked and the expansion didn't *
	     * change the command line, do completion.                  */
	    if (olst == COMP_EXPAND_COMPLETE &&
		!strcmp(ol, (char *)line)) {
		cs = ocs;
		errflag = 0;

		if (!compfunc) {
		    char *p;

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
		}
		ret = docompletion(s, lst, lincmd);
	    } else if (ret)
		clearlist = 1;
	} else
	    /* Just do completion. */
	    ret = docompletion(s, lst, lincmd);
	zsfree(s);
    } else
	ret = 1;
    /* Reset the lexer state, pop the heap. */
    lexrestore();
    popheap();
    zsfree(qword);
    unmetafy_line();

    if (menucmp && !omc) {
	struct chdata dat;

	dat.matches = amatches;
	dat.num = nmatches;
	dat.cur = NULL;
	if (runhookdef(MENUSTARTHOOK, (void *) &dat))
	    menucmp = menuacc = 0;
    }
    return ret;
}

/* Do completion, given that we are in the middle of a menu completion.  We *
 * don't need to generate a list of matches, because that's already been    *
 * done by previous commands.  We will either list the completions, or      *
 * insert the next completion.                                              */

/**/
void
do_menucmp(int lst)
{
    /* Just list the matches if the list was requested. */
    if (lst == COMP_LIST_COMPLETE) {
	showinglist = -2;
	return;
    }
    /* Otherwise go to the next match in the array... */
    HEAPALLOC {
	if (!*++(minfo.cur)) {
	    do {
		if (!(minfo.group = (minfo.group)->next))
		    minfo.group = amatches;
	    } while (!(minfo.group)->mcount);
	    minfo.cur = minfo.group->matches;
	}
	/* ... and insert it into the command line. */
	metafy_line();
	do_single(*(minfo.cur));
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
	line = (unsigned char *)zhalloc(strlen((char *)line) + 3 + addspace);
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
    char *t = (char *) ncalloc(len + 2);
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
    int t0, tt0, i, j, k, cp, rd, sl, ocs, ins, oins, ia, parct;
    char *s = NULL, *linptr, *tmp, *p, *tt = NULL;

    zsfree(brbeg);
    zsfree(brend);
    brbeg = brend = NULL;
    /* This global flag is used to signal the lexer code if it should *
     * expand aliases or not.                                         */
    noaliases = isset(COMPLETEALIASES);

    /* Find out if we are somewhere in a `string', i.e. inside '...', *
     * "...", `...`, or ((...)). Nowadays this is only used to find   *
     * out if we are inside `...`.                                    */

    for (i = j = k = 0, p = (char *)line; p < (char *)line + cs; p++)
	if (*p == '`' && !(k & 1))
	    i++;
	else if (*p == '\"' && !(k & 1) && !(i & 1))
	    j++;
	else if (*p == '\'' && !(j & 1))
	    k++;
	else if (*p == '\\' && p[1] && !(k & 1))
	    p++;
    inbackt = (i & 1);
    instring = 0;
    addx(&tmp);
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
	strinbeg(0);
	i = tt0 = cp = rd = ins = oins = linarr = parct = ia = 0;

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
	    if (linarr)
		incmdpos = 0;
	    ctxtlex();

	    if (tok == LEXERR) {
		if (!tokstr)
		    break;
		for (j = 0, p = tokstr; *p; p++)
		    if (*p == Snull || *p == Dnull)
			j++;
		if (j & 1)
		    tok = STRING;
	    }
	    if (tok == ENVARRAY) {
		linarr = 1;
		zsfree(varname);
		varname = ztrdup(tokstr);
	    } else if (tok == INPAR)
		parct++;
	    else if (tok == OUTPAR) {
		if (parct)
		    parct--;
		else
		    linarr = 0;
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
		ia = linarr;
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
	    /* We are in command or process substitution if we are not in
	     * a $((...)). */
	    if (parend >= 0 && !tmp)
		line = (unsigned char *) dupstring(tmp = (char *)line);
	    linptr = (char *) line + ll + addedx - parbegin + 1;
	    if ((linptr - (char *) line) < 2 ||
		linptr[-1] != '(' || linptr[-2] != '$') {
		if (parend >= 0) {
		    ll -= parend;
		    line[ll + addedx] = '\0';
		}
		lexrestore();
		goto start;
	    }
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
	    if (skipparens(Inbrack, Outbrack, &s) > 0 || s > tt + cs - wb) {
		s = NULL;
		inwhat = IN_MATH;
		if ((keypm = (Param) paramtab->getnode(paramtab, varname)) &&
		    (keypm->flags & PM_HASHED))
		    insubscr = 2;
		else
		    insubscr = 1;
	    } else if (*s == '=' && cs > wb + (s - tt)) {
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
	    char *nnb = (iident(*s) ? s : s + 1), *nb = NULL, *ne = NULL;

	    for (tt = s; ++tt < s + cs - wb;)
		if (*tt == Inbrack) {
		    i++;
		    nb = nnb;
		    ne = tt;
		} else if (i && *tt == Outbrack)
		    i--;
		else if (!iident(*tt))
		    nnb = tt + 1;
	    if (i) {
		inwhat = IN_MATH;
		insubscr = 1;
		if (nb < ne) {
		    char sav = *ne;
		    *ne = '\0';
		    zsfree(varname);
		    varname = ztrdup(nb);
		    *ne = sav;
		    if ((keypm = (Param) paramtab->getnode(paramtab,
							   varname)) &&
			(keypm->flags & PM_HASHED))
			insubscr = 2;
		}
	    }
	}
	if (inwhat == IN_MATH) {
	    if (compfunc || insubscr == 2) {
		int lev;
		char *p;

		for (wb = cs - 1, lev = 0; wb > 0; wb--)
		    if (line[wb] == ']' || line[wb] == ')')
			lev++;
		    else if (line[wb] == '[') {
			if (!lev--)
			    break;
		    } else if (line[wb] == '(') {
			if (!lev && line[wb - 1] == '(')
			    break;
			if (lev)
			    lev--;
		    }
		p = (char *) line + wb;
		wb++;
		if (wb && (*p == '[' || *p == '(') &&
		    !skipparens(*p, (*p == '[' ? ']' : ')'), &p)) {
			we = (p - (char *) line) - 1;
			if (insubscr == 2)
			    insubscr = 3;
		}
	    } else {
		/* In mathematical expression, we complete parameter names  *
		 * (even if they don't have a `$' in front of them).  So we *
		 * have to find that name.                                  */
		for (we = cs; iident(line[we]); we++);
		for (wb = cs; --wb >= 0 && iident(line[wb]););
		wb++;
	    }
	    zsfree(s);
	    s = zalloc(we - wb + 1);
	    strncpy(s, (char *) line + wb, we - wb);
	    s[we - wb] = '\0';
	    if (wb > 2 && line[wb - 1] == '[' && iident(line[wb - 2])) {
		int i = wb - 3;
		unsigned char sav = line[wb - 1];

		while (i >= 0 && iident(line[i]))
		    i--;

		line[wb - 1] = '\0';
		zsfree(varname);
		varname = ztrdup((char *) line + i + 1);
		line[wb - 1] = sav;
		if ((keypm = (Param) paramtab->getnode(paramtab, varname)) &&
		    (keypm->flags & PM_HASHED)) {
		    if (insubscr != 3)
			insubscr = 2;
		} else
		    insubscr = 1;
	    }
	}
	/* This variable will hold the current word in quoted form. */
	qword = ztrdup(s);
	offs = cs - wb;
	if ((p = check_param(s, 0, 1))) {
	    for (p = s; *p; p++)
		if (*p == Dnull)
		    *p = '"';
		else if (*p == Snull)
		    *p = '\'';
	}
	if (*s == Snull || *s == Dnull) {
	    char *q = (*s == Snull ? "'" : "\""), *n = tricat(qipre, q, "");
	    int sl = strlen(s);

	    instring = (*s == Snull ? 1 : 2);
	    zsfree(qipre);
	    qipre = n;
	    if (sl > 1 && s[sl - 1] == *s) {
		n = tricat(q, qisuf, "");
		zsfree(qisuf);
		qisuf = n;
	    }
	    autoq = *q;
	}
	/* While building the quoted form, we also clean up the command line. */
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
		 * we try to get braces after a parameter expansion right,
		 * but this may fail sometimes. sorry.
		 */
		if (*p == String || *p == Qstring) {
		    if (p[1] == Inbrace || p[1] == Inpar || p[1] == Inbrack) {
			char *tp = p + 1;
			if (skipparens(*tp, (*tp == Inbrace ? Outbrace :
					     (*tp == Inpar ? Outpar : Outbrack)),
				       &tp)) {
			    tt = NULL;
			    break;
			}
			i += tp - p;
			p = tp;
		    } else {
			char *tp = p + 1;

			for (; *tp == '^' || *tp == Hat ||
				 *tp == '=' || *tp == Equals ||
				 *tp == '~' || *tp == Tilde ||
				 *tp == '#' || *tp == Pound || *tp == '+';
			     tp++);
			if (*tp == Quest || *tp == Star || *tp == String ||
			    *tp == Qstring || *tp == '?' || *tp == '*' ||
			    *tp == '$' || *tp == '-' || *tp == '!' ||
			    *tp == '@')
			    p++, i++;
			else {
			    if (idigit(*tp))
				while (idigit(*tp))
				    tp++;
			    else if (iident(*tp))
				while (iident(*tp))
				    tp++;
			    else {
				tt = NULL;
				break;
			    }
			    if (*tp == Inbrace) {
				tt = NULL;
				break;
			    }
			    tp--;
			    i += tp - p;
			    p = tp;
			}
		    }
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
		char *com = NULL, *tmp;
		int pl, sl;

		brbeg = dupstring(tt);
		brpl = tt - s;
		tmp = dupstrpfx(s, tt - s);
		qbrpl = strlen(quotename(tmp, NULL));
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
		    qbrsl = strlen(quotename(p, NULL));
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
static int
doexpansion(char *s, int lst, int olst, int explincmd)
{
    int ret = 1;
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
	if (empty(vl) || !*(char *)peekfirst(vl))
	    goto end;
	if (peekfirst(vl) == (void *) ss ||
		(olst == COMP_EXPAND_COMPLETE &&
		 !nextnode(firstnode(vl)) && *s == Tilde &&
		 (ss = dupstring(s), filesubstr(&ss, 0)) &&
		 !strcmp(ss, (char *)peekfirst(vl)))) {
	    /* If expansion didn't change the word, try completion if *
	     * expandorcomplete was called, otherwise, just beep.     */
	    if (lst == COMP_EXPAND_COMPLETE)
		docompletion(s, COMP_COMPLETE, explincmd);
	    goto end;
	}
	if (lst == COMP_LIST_EXPAND) {
	    /* Only the list of expansions was requested. */
	    ret = listlist(vl);
	    showinglist = 0;
	    goto end;
	}
	/* Remove the current word and put the expansions there. */
	cs = wb;
	foredel(we - wb);
	while ((ss = (char *)ugetnode(vl))) {
	    ret = 0;
	    untokenize(ss);
	    ss = quotename(ss, NULL);
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

    return ret;
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
	    *q = n = (Cmlist) zhalloc(sizeof(struct cmlist));
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

/* This returns a new Cline structure. */

static Cline
get_cline(char *l, int ll, char *w, int wl, char *o, int ol, int fl)
{
    Cline r;

    /* Preverably take it from the buffer list (freecl), if there
     * is none, allocate a new one. */

    if ((r = freecl))
	freecl = r->next;
    else
	r = (Cline) zhalloc(sizeof(*r));

    r->next = NULL;
    r->line = l; r->llen = ll;
    r->word = w; r->wlen = wl;
    r->orig = o; r->olen = ol;
    r->slen = 0;
    r->flags = fl;
    r->prefix = r->suffix = NULL;
    return r;
}

/* This frees a cline list. */

static void
free_cline(Cline l)
{
    Cline n;

    while (l) {
	n = l->next;
	l->next = freecl;
	freecl = l;
	free_cline(l->prefix);
	free_cline(l->suffix);
	l = n;
    }
}

/* This reverts the order of the elements of the given cline list and
 * returns a pointer to the new head. */

static Cline
revert_cline(Cline p)
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

/* This splits the given string into a list of cline structs, separated
 * at those places where one of the anchors of an `*' pattern was found.
 * plen gives the number of characters on the line that matched this
 * string. In lp we return a pointer to the last cline struct we build. */

static Cline
bld_parts(char *str, int len, int plen, Cline *lp)
{
    Cline ret = NULL, *q = &ret, n;
    Cmlist ms;
    Cmatcher mp;
    int t, op = plen;
    char *p = str;

    while (len) {
	for (t = 0, ms = bmatchers; ms && !t; ms = ms->next) {
	    mp = ms->matcher;
	    if (mp->flags == CMF_RIGHT && mp->wlen == -1 &&
		!mp->llen && len >= mp->ralen && mp->ralen &&
		pattern_match(mp->right, str, NULL, NULL)) {
		int olen = str - p, llen;

		/* We found an anchor, create a new cline. The NEW flag
		 * is set if the characters before the anchor were not
		 * on the line. */
		*q = n = get_cline(NULL, mp->ralen, str, mp->ralen, NULL, 0,
				   ((plen < 0) ? CLF_NEW : 0));

		/* If there were any characters before the anchor, add
		 * them as a cline struct. */

		if (p != str) {
		    llen = (op < 0 ? 0 : op);

		    if (llen > olen)
			llen = olen;
		    n->prefix = get_cline(NULL, llen, p, olen, NULL, 0, 0);
		}
		q = &(n->next);
		str += mp->ralen; len -= mp->ralen;
		plen -= mp->ralen;
		op -= olen;
		p = str;
		t = 1;
	    }
	}
	if (!t) {
	    /* No anchor was found here, skip. */
	    str++; len--;
	    plen--;
	}
    }
    /* This is the cline struct for the remaining string at the end. */

    *q = n = get_cline(NULL, 0, NULL, 0, NULL, 0, (plen < 0 ? CLF_NEW : 0));
    if (p != str) {
	int olen = str - p, llen = (op < 0 ? 0 : op);

	if (llen > olen)
	    llen = olen;
	n->prefix = get_cline(NULL, llen, p, olen, NULL, 0, 0);
    }
    n->next = NULL;

    if (lp)
	*lp = n;

    return ret;
}

/* Global variables used during matching: a char-buffer for the string to
 * use for the match, and two cline lists for the two levels we use. */

static char *matchbuf = NULL;
static int matchbuflen = 0, matchbufadded;

static Cline matchparts, matchlastpart;
static Cline matchsubs, matchlastsub;

/* This initialises the variables above. */

static void
start_match(void)
{
    if (matchbuf)
	*matchbuf = '\0';
    matchbufadded = 0;
    matchparts = matchlastpart = matchsubs = matchlastsub = NULL;
}

/* This aborts a matching, freeing the cline lists build. */

static void
abort_match(void)
{
    free_cline(matchparts);
    free_cline(matchsubs);
}

/* This adds a new string in the static char buffer. The arguments are
 * the matcher used (if any), the strings from the line and the word
 * and the length of the string from the word. The last argument is
 * non-zero if we are matching a suffix (where the given string has to 
 * be prepended to the contents of the buffer). */

static void
add_match_str(Cmatcher m, char *l, char *w, int wl, int sfx)
{
    /* Get the string and length to insert: either from the line 
     * or from the match. */
    if (m && (m->flags & CMF_LINE)) {
	wl = m->llen; w = l;
    }
    if (wl) {
	/* Probably resie the buffer. */
	if (matchbuflen - matchbufadded <= wl) {
	    int blen = matchbuflen + wl + 20;
	    char *buf;

	    buf = (char *) zalloc(blen);
	    memcpy(buf, matchbuf, matchbuflen);
	    zfree(matchbuf, matchbuflen);
	    matchbuf = buf;
	    matchbuflen = blen;
	}
	/* Insert the string. */
	if (sfx) {
	    memmove(matchbuf + wl, matchbuf, matchbufadded + 1);
	    memcpy(matchbuf, w, wl);
	} else
	    memcpy(matchbuf + matchbufadded, w, wl);
	matchbufadded += wl;
	matchbuf[matchbufadded] = '\0';
    }
}

/* This adds a cline for a word-part during matching. Arguments are the
 * matcher used, pointers to the line and word strings for the anchor,
 * a pointer to the original line string for the whole part, the string
 * before (or after) the anchor that has not yet been added, the length
 * of the line-string for that, and a flag saying if we are matching a 
 * suffix. */

static void
add_match_part(Cmatcher m, char *l, char *w, int wl,
	       char *o, int ol, char *s, int sl, int osl, int sfx)
{
    Cline p, lp;

    /* If the anchors are equal, we keep only one. */

    if (!strncmp(l, w, wl))
	l = NULL;

    /* Split the new part into parts and turn the last one into a `suffix'
     * if we have a left anchor. */

    p = bld_parts(s, sl, osl, &lp);

    p->flags &= ~CLF_NEW;
    if (m && (m->flags & CMF_LEFT)) {
	lp->flags |= CLF_SUF;
	lp->suffix = lp->prefix;
	lp->prefix = NULL;
    }
    /* cline lists for suffixes are sorted from back to front, so we have
     * to revert the list we got. */
    if (sfx)
	p = revert_cline(lp = p);
    /* Now add the sub-clines we already had. */
    if (matchsubs) {
	matchlastsub->next = p->prefix;
	p->prefix = matchsubs;
	matchsubs = matchlastsub = NULL;
    }
    /* Store the arguments in the last part-cline. */
    lp->line = l; lp->llen = wl;
    lp->word = w; lp->wlen = wl;
    lp->orig = o; lp->olen = ol;
    lp->flags &= ~CLF_NEW;

    /* Finally, put the new parts on the list. */
    if (matchlastpart)
	matchlastpart->next = p;
    else
	matchparts = p;
    matchlastpart = lp;
}

/* This adds a new sub-cline. Arguments are the matcher and the strings from
 * the line and the word. */

static void
add_match_sub(Cmatcher m, char *l, int ll, char *w, int wl)
{
    int flags;
    Cline n;

    /* Check if we are interested only in the string from the line. */
    if (m && (m->flags & CMF_LINE)) {
	w = NULL; wl = 0;
	flags = CLF_LINE;
    } else
	flags = 0;

    /* And add the cline. */
    if (wl || ll) {
	n = get_cline(l, ll, w, wl, NULL, 0, flags);
	if (matchlastsub)
	    matchlastsub->next = n;
	else
	    matchsubs = n;
	matchlastsub = n;
    }
}

/* This tests if the string from the line l matches the word w. In bp
 * the offset for the brace is returned, in rwlp the length of the
 * matched prefix or suffix, not including the stuff before or after
 * the last anchor is given. When sfx is non-zero matching is done from
 * the ends of the strings backward, if test is zero, the global variables
 * above are used to build the string for the match and the cline. */

static int
match_str(char *l, char *w, int *bp, int *rwlp, int sfx, int test)
{
    int ll = strlen(l), lw = strlen(w), oll = ll, olw = lw;
    int il = 0, iw = 0, t, ind, add, bc = (bp ? *bp : 0);
    VARARR(unsigned char, ea, ll + 1);
    char *ow;
    Cmlist ms;
    Cmatcher mp, lm = NULL;

    if (!test)
	start_match();

    /* Adjust the pointers and get the values for subscripting and
     * incrementing. */

    if (sfx) {
	l += ll; w += lw;
	ind = -1; add = -1;
    } else {
	ind = 0; add = 1;
    }
    /* ow will always point to the beginning (or end) of that sub-string
     * in w that wasn't put in the match-variables yet. */

    ow = w;

    /* If the brace is at the beginning, we have to treat it now. */

    if (!test && !bc && bp) {
	*bp = 0;
	bp = NULL;
    }
    while (ll && lw) {
	/* First try the matchers. */
	for (mp = NULL, ms = mstack; !mp && ms; ms = ms->next) {
	    for (mp = ms->matcher; mp; mp = mp->next) {
		t = 1;
		if (lm == mp ||
		    ((oll == ll || olw == lw) && test && mp->wlen < 0))
		    /* If we were called recursively, don't use `*' patterns
		     * at the beginning (avoiding infinite recursion). */
		    continue;

		if (mp->wlen < 0) {
		    int both, loff, aoff, llen, alen, zoff, moff, ct, ict;
		    char *tp, savl = '\0', savw;
		    Cpattern ap;

		    /* This is for `*' patterns, first initialise some
		     * local variables. */
		    llen = mp->llen;
		    alen = (mp->flags & CMF_LEFT ? mp->lalen : mp->ralen);

		    /* Give up if we don't have enough characters for the
		     * line-string and the anchor. */
		    if (ll < llen + alen || lw < alen)
			continue;

		    if (mp->flags & CMF_LEFT) {
			ap = mp->left; zoff = 0; moff = alen;
			if (sfx) {
			    both = 0; loff = -llen; aoff = -(llen + alen);
			} else {
			    both = 1; loff = alen; aoff = 0;
			}
		    } else {
			ap = mp->right; zoff = alen; moff = 0;
			if (sfx) {
			    both = 1; loff = -(llen + alen); aoff = -alen;
			} else {
			    both = 0; loff = 0; aoff = llen;
			}
		    }
		    /* Try to match the line pattern and the anchor. */
		    if (!pattern_match(mp->line, l + loff, NULL, NULL))
			continue;
		    if (ap) {
			if (!pattern_match(ap, l + aoff, NULL, NULL) ||
			    (both && !pattern_match(ap, w + aoff, NULL, NULL)))
			    continue;
		    } else if (!both || il || iw)
			continue;

		    /* Fine, now we call ourselves recursively to find the
		     * string matched by the `*'. */
		    if (sfx) {
			savl = l[-(llen + zoff)];
			l[-(llen + zoff)] = '\0';
		    }
		    for (t = 0, tp = w, ct = 0, ict = lw - alen;
			 ict;
			 tp += add, ct++, ict--) {
			if (both ||
			    pattern_match(ap, tp - moff, NULL, NULL)) {
			    if (sfx) {
				savw = tp[-zoff];
				tp[-zoff] = '\0';
				t = match_str(l - ll, w - lw,
					      NULL, NULL, 1, 1);
				tp[-zoff] = savw;
			    } else
				t = match_str(l + llen + moff, tp + moff,
					      NULL, NULL, 0, 1);
			    if (t || !both)
				break;
			}
		    }
		    ict = ct;
		    if (sfx)
			l[-(llen + zoff)] = savl;

		    /* Have we found a position in w where the rest of l
		     * matches? */
		    if (!t)
			continue;

		    /* Yes, add the strings and clines if this is a 
		     * top-level call. */
		    if (!test) {
			char *op, *lp, *map, *wap, *wmp;
			int ol;

			if (sfx) {
			    op = w; ol = ow - w; lp = l - (llen + alen);
			    map = tp - alen;
			    if (mp->flags & CMF_LEFT) {
				wap = tp - alen; wmp = tp;
			    } else {
				wap = w - alen; wmp = tp - alen;
			    }
			} else {
			    op = ow; ol = w - ow; lp = l;
			    map = ow;
			    if (mp->flags & CMF_LEFT) {
				wap = w; wmp = w + alen;
			    } else {
				wap = tp; wmp = ow;
			    }
			}
			/* If the matcher says that we are only interested
			 * in the line pattern, we just add that and the
			 * anchor and the string not added yet. Otherwise
			 * we add a new part. */
			if (mp->flags & CMF_LINE) {
			    add_match_str(NULL, NULL, op, ol, sfx);
			    add_match_str(NULL, NULL, lp, llen + alen, sfx);
			    add_match_sub(NULL, NULL, ol, op, ol);
			    add_match_sub(NULL, NULL, llen + alen,
					  lp, llen + alen);
			} else {
			    add_match_str(NULL, NULL,
					  map, ct + ol + alen, sfx);
			    if (both) {
				add_match_sub(NULL, NULL, ol, op, ol);
				ol = -1;
			    } else
				ct += ol;
			    add_match_part(mp, l + aoff, wap, alen,
					   l + loff, llen, wmp, ct, ol, sfx);
			}
		    }
		    /* Now skip over the matched portion and the anchor. */
		    llen += alen; alen += ict;
		    if (sfx) {
			l -= llen; w -= alen;
		    } else {
			l += llen; w += alen;
		    }
		    ll -= llen; il += llen;
		    lw -= alen; iw += alen;
		    bc -= llen;

		    if (!test && bc <= 0 && bp) {
			*bp = matchbufadded + bc;
			bp = NULL;
		    }
		    ow = w;

		    if (!llen && !alen)
			lm = mp;
		    else
			lm = NULL;
		    break;
		} else if (ll >= mp->llen && lw >= mp->wlen) {
		    /* Non-`*'-pattern. */
		    char *tl, *tw;
		    int tll, tlw, til, tiw;

		    /* We do this only if the line- and word-substrings
		     * are not equal. */
		    if (!(mp->flags & (CMF_LEFT | CMF_RIGHT)) &&
			mp->llen == mp->wlen &&
			!(sfx ? strncmp(l - mp->llen, w - mp->wlen, mp->llen) :
			  strncmp(l, w, mp->llen)))
			continue;

		    /* Using local variables to make the following
		     * independent of whether we match a prefix or a
		     * suffix. */
		    if (sfx) {
			tl = l - mp->llen; tw = w - mp->wlen;
			til = ll - mp->llen; tiw = lw - mp->wlen;
			tll = il + mp->llen; tlw = iw + mp->wlen;
		    } else {
			tl = l; tw = w;
			til = il; tiw = iw;
			tll = ll; tlw = lw;
		    }
		    if (mp->flags & CMF_LEFT) {
			/* Try to match the left anchor, if any. */
			if (til < mp->lalen || tiw < mp->lalen)
			    continue;
			else if (mp->left)
			    t = pattern_match(mp->left, tl - mp->lalen,
					      NULL, NULL) &&
				pattern_match(mp->left, tw - mp->lalen,
					      NULL, NULL);
			else
			    t = (!sfx && !il && !iw);
		    }
		    if (mp->flags & CMF_RIGHT) {
			/* Try to match the right anchor, if any. */
			if (tll < mp->llen + mp->ralen ||
			    tlw < mp->wlen + mp->ralen)
			    continue;
			else if (mp->left)
			    t = pattern_match(mp->right,
					      tl + mp->llen - mp->ralen,
					      NULL, NULL) &&
				pattern_match(mp->right,
					      tw + mp->wlen - mp->ralen,
					      NULL, NULL);
			else
			    t = (sfx && !il && !iw);
		    }
		    /* Now try to match the line and word patterns. */
		    if (!t ||
			!pattern_match(mp->line, tl, NULL, ea) ||
			!pattern_match(mp->word, tw, ea, NULL))
			continue;

		    /* Probably add the matched strings. */
		    if (!test) {
			if (sfx)
			    add_match_str(NULL, NULL, w, ow - w, 0);
			else
			    add_match_str(NULL, NULL, ow, w - ow, 0);
			add_match_str(mp, tl, tw, mp->wlen, 0);
			if (sfx)
			    add_match_sub(NULL, NULL, 0, w, ow - w);
			else
			    add_match_sub(NULL, NULL, 0, ow, w - ow);

			add_match_sub(mp, tl, mp->llen, tw, mp->wlen);
		    }
		    if (sfx) {
			l = tl;	w = tw;
		    } else {
			l += mp->llen; w += mp->wlen;
		    }
		    il += mp->llen; iw += mp->wlen;
		    ll -= mp->llen; lw -= mp->wlen;
		    bc -= mp->llen;

		    if (!test && bc <= 0 && bp) {
			*bp = matchbufadded + bc;
			bp = NULL;
		    }
		    ow = w;
		    lm = NULL;
		    break;
		}
	    }
	}
	if (mp)
	    continue;

	if (l[ind] == w[ind]) {
	    /* No matcher could be used, but the strings have the same
	     * character here, skip over it. */
	    l += add; w += add;
	    il++; iw++;
	    ll--; lw--;
	    bc--;
	    if (!test && bc <= 0 && bp) {
		*bp = matchbufadded + (sfx ? (ow - w) : (w - ow));
		bp = NULL;
	    }
	    lm = NULL;
	} else {
	    /* No matcher and different characters: l does not match w. */
	    abort_match();

	    return (test ? 0 : -1);
	}
    }
    /* If this is a recursive call, we just return if l matched w or not. */
    if (test)
	return !ll;

    /* In top-level calls, if ll is non-zero (unmatched portion in l),
     * we have to free the collected clines. */
    if (ll) {
	abort_match();

	return -1;
    }
    if (rwlp)
	*rwlp = iw - (sfx ? ow - w : w - ow);

    /* If we matched a suffix, the anchors stored in the top-clines
     * will be in the wrong clines: shifted by one. Adjust this. */
    if (sfx && matchparts) {
	Cline t, tn, s;

	if (matchparts->prefix || matchparts->suffix) {
	    t = get_cline(NULL, 0, NULL, 0, NULL, 0, 0);
	    t->next = matchparts;
	    if (matchparts->prefix)
		t->prefix = (Cline) 1;
	    else
		t->suffix = (Cline) 1;
	    matchparts = t;
	}
	for (t = matchparts; (tn = t->next); t = tn) {
	    s = (tn->prefix ? tn->prefix : tn->suffix);
	    if (t->prefix)
		t->prefix = s;
	    else
		t->suffix = s;
	}
	t->prefix = t->suffix = NULL;
    }
    /* Finally, return the number of matched characters. */

    return iw;
}

/* Check if the word w is matched by the strings in pfx and sfx (the prefix
 * and the suffix from the line) or the pattern cp. In clp a cline list for
 * w is returned.
 * qu is non-zero if the words has to be quoted before processed any further.
 * bpl and bsl are used to report the positions where the brace-strings in
 * the prefix and the suffix have to be re-inserted if this match is inserted
 * in the line.
 * The return value is the string to use as a completion or NULL if the prefix
 * and the suffix don't match the word w. */

static char *
comp_match(char *pfx, char *sfx, char *w, Comp cp,
	   Cline *clp, int qu, int *bpl, int *bsl, int *exact)
{
    char *r = NULL;

    if (cp) {
	/* We have a globcomplete-like pattern, just use that. */
	int wl;

	r = w;
	if (!domatch(r, cp, 0))
	    return NULL;
    
	r = (qu ? quotename(r, NULL) : dupstring(r));
	if (qu == 2 && r[0] == '\\' && r[1] == '~')
	    chuck(r);
	/* We still break it into parts here, trying to build a sensible
	 * cline list for these matches, too. */
	w = dupstring(w);
	wl = strlen(w);
	*clp = bld_parts(w, wl, wl, NULL);
	*exact = 0;
	*bpl = (qu ? qbrpl : brpl);
	*bsl = (qu ? qbrsl : brsl);
    } else {
	Cline pli, plil;
	int mpl, rpl, wl;

	w = (qu ? quotename(w, NULL) : dupstring(w));
	if (qu == 2 && w[0] == '\\' && w[1] == '~')
	    chuck(w);

	wl = strlen(w);

	/* Always try to match the prefix. */

	*bpl = (qu ? qbrpl : brpl);
	if ((mpl = match_str(pfx, w, bpl, &rpl, 0, 0)) < 0)
	    return NULL;

	if (sfx && *sfx) {
	    int wpl = matchbufadded, msl, rsl;
	    VARARR(char, wpfx, wpl);
	    Cline mli, mlil;

	    /* We also have a suffix to match, so first save the
	     * contents of the global matching variables. */
	    memcpy(wpfx, matchbuf, wpl);
	    if (matchsubs) {
		Cline tmp = get_cline(NULL, 0, NULL, 0, NULL, 0, 0);

		tmp->prefix = matchsubs;
		if (matchlastpart)
		    matchlastpart->next = tmp;
		else
		    matchparts = tmp;
	    }
	    pli = matchparts;
	    plil = matchlastpart;

	    /* The try to match the suffix. */
	    *bsl = (qu ? qbrsl : brsl);
	    if ((msl = match_str(sfx, w + mpl, bsl, &rsl, 1, 0)) < 0) {
		free_cline(pli);

		return NULL;
	    }
	    /* Matched, so add the string in the middle and the saved
	     * string for the prefix, and build a combined cline list
	     * for the prefix and the suffix. */
	    if (matchsubs) {
		Cline tmp = get_cline(NULL, 0, NULL, 0, NULL, 0, CLF_SUF);

		tmp->suffix = matchsubs;
		if (matchlastpart)
		    matchlastpart->next = tmp;
		else
		    matchparts = tmp;
	    }
	    add_match_str(NULL, NULL, w + rpl, wl - rpl - rsl, 1);
	    add_match_str(NULL, NULL, wpfx, wpl, 1);

	    mli = bld_parts(w + rpl, wl - rpl - rsl, (mpl - rpl), &mlil);
	    mlil->flags |= CLF_MID;
	    mlil->slen = msl - rsl;
	    mlil->next = revert_cline(matchparts);

	    if (plil)
		plil->next = mli;
	    else
		pli = mli;
	} else {
	    /* Only a prefix, add the string and a part-cline for it. */
	    add_match_str(NULL, NULL, w + rpl, wl - rpl, 0);

	    add_match_part(NULL, NULL, NULL, 0, NULL, 0, w + rpl, wl - rpl,
			   mpl - rpl, 0);
	    pli = matchparts;
	}
	r = dupstring(matchbuf);

	*clp = pli;

	/* Test if the string built is equal to the one from the line. */
	if (sfx && *sfx) {
	    int pl = strlen(pfx);

	    *exact = (!strncmp(pfx, w, pl) && !strcmp(sfx, w + pl));
	} else
	    *exact = !strcmp(pfx, w);
    }
    if (!qu)
	hasunqu = 1;

    return r;
}

/* This builds all the possible line patterns for the pattern pat in the
 * buffer line. Initially line is the same as lp, but during recursive
 * calls lp is incremented for storing successive characters. Whenever
 * a full possible string is build, we test if this line matches the
 * string given by wlen and word. The in argument contains the characters
 * to use for the correspondence classes, it was filled by a call to 
 * pattern_match() in the calling function.
 * The return value is the length of the string matched in the word, it
 * is zero if we couldn't build a line that matches the word. */

static int
bld_line(Cpattern pat, char *line, char *lp,
	 char *word, int wlen, unsigned char *in, int sfx)
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
		if ((l = bld_line(pat->next, line, lp + 1, word, wlen,
				  in, sfx)))
		    return l;
	    }
    } else {
	/* We reached the end, i.e. the line string is fully build, now
	 * see if it matches the given word. */

	Cmlist ms;
	Cmatcher mp;
	int l = lp - line, t, rl = 0, ind, add;
	VARARR(unsigned char, ea, l + 1);

	/* Quick test if the strings are exactly the same. */
	if (l == wlen && !strncmp(line, word, l))
	    return l;

	if (sfx) {
	    line = lp; word += wlen;
	    ind = -1; add = -1;
	} else {
	    ind = 0; add = 1;
	}
	/* We loop through the whole line string built. */
	while (l && wlen) {
	    if (word[ind] == line[ind]) {
		/* The same character in both strings, skip over. */
		line += add; word += add;
		l--; wlen--; rl++;
	    } else {
		t = 0;
		for (ms = bmatchers; ms && !t; ms = ms->next) {
		    mp = ms->matcher;
		    if (!mp->flags && mp->wlen <= wlen && mp->llen <= l &&
			pattern_match(mp->line, (sfx ? line - mp->llen : line),
				      NULL, ea) &&
			pattern_match(mp->word, (sfx ? word - mp->wlen : word),
				      ea, NULL)) {
			/* Both the line and the word pattern matched,
			 * now skip over the matched portions. */
			if (sfx) {
			    line -= mp->llen; word -= mp->wlen;
			} else {
			    line += mp->llen; word += mp->wlen;
			}
			l -= mp->llen; wlen -= mp->wlen; rl += mp->wlen;
			t = 1;
		    }
		}
		if (!t)
		    /* Didn't match, give up. */
		    return 0;
	    }
	}
	if (!l)
	    /* Unmatched portion in the line built, return matched length. */
	    return rl;
    }
    return 0;
}

/* This builds a string that may be put on the line that fully matches the
 * given strings. The return value is NULL if no such string could be built
 * or that string in local static memory, dup it. */

static char *
join_strs(int la, char *sa, int lb, char *sb)
{
    static char *rs = NULL;
    static int rl = 0;

    VARARR(unsigned char, ea, (la > lb ? la : lb) + 1);
    Cmlist ms;
    Cmatcher mp;
    int t, bl, rr = rl;
    char *rp = rs;

    while (la && lb) {
	if (*sa != *sb) {
	    /* Different characters, try the matchers. */
	    for (t = 0, ms = bmatchers; ms && !t; ms = ms->next) {
		mp = ms->matcher;
		if (!mp->flags && mp->wlen > 0 && mp->llen > 0 &&
		    mp->wlen <= la && mp->wlen <= lb) {
		    /* The pattern has no anchors and the word
		     * pattern fits, try it. */
		    if ((t = pattern_match(mp->word, sa, NULL, ea)) ||
			pattern_match(mp->word, sb, NULL, ea)) {
			/* It matched one of the strings, t says which one. */
			VARARR(char, line, mp->llen + 1);
			char **ap, **bp;
			int *alp, *blp;

			if (t) {
			    ap = &sa; alp = &la;
			    bp = &sb; blp = &lb;
			} else {
			    ap = &sb; alp = &lb;
			    bp = &sa; blp = &la;
			}
			/* Now try to build a string that matches the other
			 * string. */
			if ((bl = bld_line(mp->line, line, line,
					   *bp, *blp, ea, 0))) {
			    /* Found one, put it into the return string. */
			    line[mp->llen] = '\0';
			    if (rr <= mp->llen) {
				char *or = rs;

				rs = realloc(rs, (rl += 20));
				rr += 20;
				rp += rs - or;
			    }
			    memcpy(rp, line, mp->llen);
			    rp += mp->llen; rr -= mp->llen;
			    *ap += mp->wlen; *alp -= mp->wlen;
			    *bp += bl; *blp -= bl;
			    t = 1;
			}
		    }
		}
	    }
	    if (!t)
		break;
	} else {
	    /* Same character, just take it. */
	    if (rr <= 1) {
		char *or = rs;

		rs = realloc(rs, (rl += 20));
		rr += 20;
		rp += rs - or;
	    }
	    *rp++ = *sa; rr--;
	    sa++; sb++;
	    la--; lb--;
	}
    }
    if (la || lb)
	return NULL;

    *rp = '\0';

    return rs;
}

/* This compares the anchors stored in two top-level clines. */

static int
cmp_anchors(Cline o, Cline n, int join)
{
    int line = 0;
    char *j;

    /* First try the exact strings. */
    if ((!(o->flags & CLF_LINE) && o->wlen == n->wlen &&
	 (!o->word || !strncmp(o->word, n->word, o->wlen))) ||
	(line = ((!o->line && !n->line) ||
		 (o->llen == n->llen && o->line && n->line &&
		  !strncmp(o->line, n->line, o->llen))))) {
	if (line) {
	    o->flags |= CLF_LINE;
	    o->word = NULL;
	    n->wlen = 0;
	}
	return 1;
    }
    /* Didn't work, try to build a string matching both anchors. */
    if (join && !(o->flags & CLF_JOIN) &&
	(j = join_strs(o->wlen, o->word, n->wlen, n->word))) {
	o->flags |= CLF_JOIN;
	o->wlen = strlen(j);
	o->word = dupstring(j);

	return 2;
    }
    return 0;
}

/* Below is the code to join two cline lists. This struct is used to walk
 * through a sub-list. */

typedef struct cmdata *Cmdata;

struct cmdata {
    Cline cl, pcl;
    char *str, *astr;
    int len, alen, olen, line;
};

/* This is used to ensure that a cmdata struct contains usable data.
 * The return value is non-zero if we reached the end. */

static int
check_cmdata(Cmdata md, int sfx)
{
    /* We will use the str and len fields to contain the next sub-string
     * in the list. If len is zero, we have to use the next cline. */
    if (!md->len) {
	/* If there is none, we reached the end. */
	if (!md->cl)
	    return 1;

	/* Otherwise, get the string. Only the line-string or both.
	 * We also have to adjust the pointer if this is for a suffix. */
	if (md->cl->flags & CLF_LINE) {
	    md->line = 1;
	    md->len = md->cl->llen;
	    md->str = md->cl->line;
	} else {
	    md->line = 0;
	    md->len = md->olen = md->cl->wlen;
	    if ((md->str = md->cl->word) && sfx)
		md->str += md->len;
	    md->alen = md->cl->llen;
	    if ((md->astr = md->cl->line) && sfx)
		md->astr += md->alen;
	}
	md->pcl = md->cl;
	md->cl = md->cl->next;
    }
    return 0;
}

/* This puts the not-yet-matched portion back into the last cline and 
 * returns that. */

static Cline
undo_cmdata(Cmdata md, int sfx)
{
    Cline r = md->pcl;

    if (md->line) {
	r->word = NULL;
	r->wlen = 0;
	r->flags |= CLF_LINE;
	r->llen = md->len;
	r->line = md->str - (sfx ? md->len : 0);
    } else if (md->len != md->olen) {
	r->wlen = md->len;
	r->word = md->str - (sfx ? md->len : 0);
    }
    return r;
}

/* This tries to build a string matching a sub-string in a sub-cline
 * that could not be matched otherwise. */

static Cline
join_sub(Cmdata md, char *str, int len, int *mlen, int sfx, int join)
{
    if (!check_cmdata(md, sfx)) {
	char *ow = str, *nw = md->str;
	int ol = len, nl = md->len;
	Cmlist ms;
	Cmatcher mp;
	VARARR(unsigned char, ea, (ol > nl ? ol : nl) + 1);
	int t;

	if (sfx) {
	    ow += ol; nw += nl;
	}
	for (t = 0, ms = bmatchers; ms && !t; ms = ms->next) {
	    mp = ms->matcher;
	    /* We use only those patterns that match a non-empty
	     * string in both the line and the word and that have
	     * no anchors. */
	    if (!mp->flags && mp->wlen > 0 && mp->llen > 0) {
		/* We first test, if the old string matches already the
		 * new one. */
		if (mp->llen <= ol && mp->wlen <= nl &&
		    pattern_match(mp->line, ow - (sfx ? mp->llen : 0),
				  NULL, ea) &&
		    pattern_match(mp->word, nw - (sfx ? mp->wlen : 0),
				  ea, NULL)) {
		    /* It did, update the contents of the cmdata struct
		     * and return a cline for the matched part. */
		    if (sfx)
			md->str -= mp->wlen;
		    else
			md->str += mp->wlen;
		    md->len -= mp->wlen;
		    *mlen = mp->llen;

		    return get_cline(NULL, 0, ow - (sfx ? mp->llen : 0),
				     mp->llen, NULL, 0, 0);
		}
		/* Otherwise we will try to build a string that matches
		 * both strings. But try the pattern only if the word-
		 * pattern matches one of the strings. */
		if (join && mp->wlen <= ol && mp->wlen <= nl &&
		    ((t = pattern_match(mp->word, ow - (sfx ? mp->wlen : 0),
				       NULL, ea)) ||
		     pattern_match(mp->word, nw - (sfx ? mp->wlen : 0),
				   NULL, ea))) {
		    VARARR(char, line, mp->llen + 1);
		    int bl;

		    /* Then build all the possible lines and see
		     * if one of them matches the other string. */
		    if ((bl = bld_line(mp->line, line, line,
				       (t ? nw : ow), (t ? nl : ol),
				       ea, sfx))) {
			/* Yep, one of the lines matched the other
			 * string. */
			line[mp->llen] = '\0';

			if (t) {
			    ol = mp->wlen; nl = bl;
			} else {
			    ol = bl; nl = mp->wlen;
			}
			if (sfx)
			    md->str -= nl;
			else
			    md->str += nl;
			md->len -= nl;
			*mlen = ol;

			return get_cline(NULL, 0, dupstring(line), mp->llen,
					 NULL, 0, CLF_JOIN);
		    }
		}
	    }
	}
    }
    return NULL;
}

/* This is used to match a sub-string in a sub-cline. The length of the
 * matched portion is returned. This tests only for exact equality. */

static int
sub_match(Cmdata md, char *str, int len, int sfx)
{
    int ret = 0, l, ind, add;
    char *p, *q;

    if (sfx) {
	str += len;
	ind = -1; add = -1;
    } else {
	ind = 0; add = 1;
    }
    /* str and len describe the old string, in md we have the new one. */
    while (len) {
	if (check_cmdata(md, sfx))
	    return ret;

	for (l = 0, p = str, q = md->str;
	     l < len && l < md->len && p[ind] == q[ind];
	     l++, p += add, q += add);

	if (l) {
	    /* There was a common prefix, use it. */
	    md->len -= l; len -= l;
	    if (sfx) {
		md->str -= l; str -= l;
	    } else {
		md->str += l; str += l;
	    }
	    ret += l;
	} else if (md->line || md->len != md->olen || !md->astr)
	    return ret;
	else {
	    /* We still have the line string to try. */
	    md->line = 1;
	    md->len = md->alen;
	    md->str = md->astr;
	}
    }
    return ret;
}

/* This is used to build a common prefix or suffix sub-list. If requested
 * it returns the unmatched cline lists in orest and nrest. */

static void
join_psfx(Cline ot, Cline nt, Cline *orest, Cline *nrest, int sfx)
{
    Cline p = NULL, o, n;
    struct cmdata md, omd;
    char **sstr = NULL;
    int len, join = 0, line = 0, *slen = NULL;

    if (sfx) {
	o = ot->suffix; n = nt->suffix;
    } else {
	o = ot->prefix;	n = nt->prefix;
    }
    if (!o) {
	if (orest)
	    *orest = NULL;
	if (nrest)
	    *nrest = n;

	return;
    }
    if (!n) {
	if (sfx)
	    ot->suffix = NULL;
	else
	    ot->prefix = NULL;

	if (orest)
	    *orest = o;
	else
	    free_cline(o);
	if (nrest)
	    *nrest = NULL;
	return;
    }
    md.cl = n;
    md.len = 0;

    /* Walk through the old list. */
    while (o) {
	join = 0;
	memcpy(&omd, &md, sizeof(struct cmdata));

	/* We first get the length of the prefix equal in both strings. */
	if (o->flags & CLF_LINE) {
	    if ((len = sub_match(&md, o->line, o->llen, sfx)) != o->llen) {
		join = 1; line = 1; slen = &(o->llen); sstr = &(o->line);
	    }
	} else if ((len = sub_match(&md, o->word, o->wlen, sfx)) != o->wlen) {
	    if (o->line) {
		memcpy(&md, &omd, sizeof(struct cmdata));
		o->flags |= CLF_LINE | CLF_DIFF;

		continue;
	    }
	    join = 1; line = 0; slen = &(o->wlen); sstr = &(o->word);
	}
	if (join) {
	    /* There is a rest that is different in the two lists,
	     * we try to build a new cline matching both strings. */
	    Cline joinl;
	    int jlen;

	    if ((joinl = join_sub(&md, *sstr + len, *slen - len,
				  &jlen, sfx, !(o->flags & CLF_JOIN)))) {
		/* We have one, insert it into the list. */
		joinl->flags |= CLF_DIFF;
		if (len + jlen != *slen) {
		    Cline rest;

		    rest = get_cline(NULL, 0, *sstr + (sfx ? 0 : len + jlen),
				     *slen - len - jlen, NULL, 0, 0);

		    rest->next = o->next;
		    joinl->next = rest;
		} else
		    joinl->next = o->next;

		if (len) {
		    if (sfx)
			*sstr += *slen - len;
		    *slen = len;
		    o->next = joinl;
		} else {
		    o->next = NULL;
		    free_cline(o);
		    if (p)
			p->next = joinl;
		    else if (sfx)
			ot->suffix = joinl;
		    else
			ot->prefix = joinl;
		}
		o = joinl;
		join = 0;
	    }
	}
	if (join) {
	    /* We couldn't build a cline for a common string, so we
	     * cut the list here. */
	    if (len) {
		Cline r;

		if (orest) {
		    if (line)
			r = get_cline(o->line + len, *slen - len,
				      NULL, 0, NULL, 0, o->flags);
		    else
			r = get_cline(NULL, 0, o->word + len, *slen - len,
				      NULL, 0, o->flags);

		    r->next = o->next;
		    *orest = r;

		    *slen = len;
		    o->next = NULL;
		} else {
		    if (sfx)
			*sstr += *slen - len;
		    *slen = len;
		    free_cline(o->next);
		    o->next = NULL;
		}
	    } else {
		if (p)
		    p->next = NULL;
		else if (sfx)
		    ot->suffix = NULL;
		else
		    ot->prefix = NULL;

		if (orest)
		    *orest = o;
		else
		    free_cline(o);
	    }
	    if (!orest || !nrest)
		ot->flags |= CLF_MISS;

	    if (nrest)
		*nrest = undo_cmdata(&md, sfx);

	    return;
	}
	p = o;
	o = o->next;
    }
    if (md.len || md.cl)
	ot->flags |= CLF_MISS;
    if (orest)
	*orest = NULL;
    if (nrest)
	*nrest = undo_cmdata(&md, sfx);
}

/* This builds the common prefix and suffix for a mid-cline -- the one
 * describing the place where the prefix and the suffix meet. */

static void
join_mid(Cline o, Cline n)
{
    if (o->flags & CLF_JOIN) {
	/* The JOIN flag is set in the old cline struct if it was
	 * already joined with another one. In this case the suffix
	 * field contains the suffix from previous calls. */
	Cline nr;

	join_psfx(o, n, NULL, &nr, 0);

	n->suffix = revert_cline(nr);

	join_psfx(o, n, NULL, NULL, 1);
    } else {
	/* This is the first time for both structs, so the prefix field
	 * contains the whole sub-list. */
	Cline or, nr;

	o->flags |= CLF_JOIN;

	/* We let us give both rests and use them as the suffixes. */
	join_psfx(o, n, &or, &nr, 0);

	if (or)
	    or->llen = (o->slen > or->wlen ? or->wlen : o->slen);
	o->suffix = revert_cline(or);
	n->suffix = revert_cline(nr);

	join_psfx(o, n, NULL, NULL, 1);
    }
    n->suffix = NULL;
}

/* This simplifies the cline list given as the first argument so that
 * it also matches the second list. */

static Cline
join_clines(Cline o, Cline n)
{
    /* First time called, just return the new list. On further invocations
     * we will get it as the first argument. */
    if (!o)
	return n;
    else {
	Cline oo = o, nn = n, po = NULL, pn = NULL;

	/* Walk through the lists. */
	while (o && n) {
	    /* If one of them describes a new part and the other one does
	     * not, synchronise them by searching an old part in the
	     * other list. */
	    if ((o->flags & CLF_NEW) && !(n->flags & CLF_NEW)) {
		Cline t, tn;

		for (t = o; (tn = t->next) && (tn->flags & CLF_NEW); t = tn);
		if (tn && cmp_anchors(tn, n, 0)) {
		    Cline tmp;

		    tmp = o->prefix;
		    o->prefix = tn->prefix;
		    tn->prefix = tmp;

		    if (po)
			po->next = tn;
		    else
			oo = tn;
		    t->next = NULL;
		    free_cline(o);
		    o = tn;
		    o->flags |= CLF_MISS;
		    continue;
		}
	    }
	    if (!(o->flags & CLF_NEW) && (n->flags & CLF_NEW)) {
		Cline t, tn;

		for (t = n; (tn = t->next) && (tn->flags & CLF_NEW); t = tn);
		if (tn && cmp_anchors(o, tn, 0)) {
		    Cline tmp;

		    tmp = n->prefix;
		    n->prefix = tn->prefix;
		    tn->prefix = tmp;

		    n = tn;
		    o->flags |= CLF_MISS;
		    continue;
		}
	    }
	    /* Almost the same as above, but for the case that they
	     * describe different types of parts (prefix, suffix, or mid). */
	    if ((o->flags & (CLF_SUF | CLF_MID)) !=
		(n->flags & (CLF_SUF | CLF_MID))) {
		Cline t, tn;

		for (t = n;
		     (tn = t->next) &&
			 (tn->flags & (CLF_SUF | CLF_MID)) !=
			 (o->flags  & (CLF_SUF | CLF_MID));
		     t = tn);
		if (tn && cmp_anchors(o, tn, 1)) {
		    n = tn;
		    continue;
		}
		for (t = o;
		     (tn = t->next) &&
			 (tn->flags & (CLF_SUF | CLF_MID)) !=
			 (n->flags  & (CLF_SUF | CLF_MID));
		     t = tn);
		if (tn && cmp_anchors(tn, n, 1)) {
		    if (po)
			po->next = tn;
		    else
			oo = tn;
		    t->next = NULL;
		    free_cline(o);
		    o = tn;
		    continue;
		}
		if (o->flags & CLF_MID) {
		    o->flags = (o->flags & ~CLF_MID) | (n->flags & CLF_SUF);
		    if (n->flags & CLF_SUF) {
			free_cline(o->prefix);
			o->prefix = NULL;
		    } else {
			free_cline(o->suffix);
			o->suffix = NULL;
		    }
		}
		break;
	    }
	    /* Now see if they have matching anchors. If not, cut the list. */
	    if (!(o->flags & CLF_MID) && !cmp_anchors(o, n, 1)) {
		Cline t, tn;

		for (t = n; (tn = t->next) && !cmp_anchors(o, tn, 1); t = tn);

		if (tn) {
		    n = tn;
		    continue;
		} else {
		    if (o->flags & CLF_SUF)
			break;

		    o->word = o->line = o->orig = NULL;
		    o->wlen = 0;
		    free_cline(o->next);
		    o->next = NULL;
		}
	    }
	    /* Ok, they are equal, now join the sub-lists. */
	    if (o->flags & CLF_MID)
		join_mid(o, n);
	    else
		join_psfx(o, n, NULL, NULL, (o->flags & CLF_SUF));

	    po = o;
	    o = o->next;
	    pn = n;
	    n = n->next;
	}
	/* Free the rest of the old list. */
	if (o) {
	    if (po)
		po->next = NULL;
	    else
		oo = NULL;

	    free_cline(o);
	}
	free_cline(nn);

	return oo;
    }
}

/* This adds all the data we have for a match. */

static Cmatch
add_match_data(int alt, char *str, Cline line,
	       char *ipre, char *ripre, char *isuf,
	       char *pre, char *prpre, char *ppre, char *psuf, char *suf,
	       int bpl, int bsl, int flags, int exact)
{
    Cmatch cm;
    Aminfo ai = (alt ? fainfo : ainfo);
    int palen, salen, qipl, ipl, pl, ppl, qisl, isl, psl;

    palen = salen = qipl = ipl = pl = ppl = qisl = isl = psl = 0;

    DPUTS(!line, "BUG: add_match_data() without cline");

    /* If there is a path suffix, we build a cline list for it and
     * append it to the list for the match itself. */
    if (psuf)
	salen = (psl = strlen(psuf));
    if (isuf)
	salen += (isl = strlen(isuf));
    if (qisuf)
	salen += (qisl = strlen(qisuf));

    if (salen) {
	char *asuf = (char *) zhalloc(salen);
	Cline pp, p, s, sl = NULL;

	if (psl)
	    memcpy(asuf, psuf, psl);
	if (isl)
	    memcpy(asuf + psl, isuf, isl);
	if (qisl)
	    memcpy(asuf + psl + isl, qisuf, qisl);

	for (pp = NULL, p = line; p->next; pp = p, p = p->next);

	if (salen > qisl) {
	    s = bld_parts(asuf, salen - qisl, salen - qisl, &sl);

	    if (!(p->flags & (CLF_SUF | CLF_MID)) &&
		!p->llen && !p->wlen && !p->olen) {
		if (p->prefix) {
		    Cline q;

		    for (q = p->prefix; q->next; q = q->next);
		    q->next = s->prefix;
		    s->prefix = p->prefix;
		    p->prefix = NULL;
		}
		free_cline(p);
		if (pp)
		    pp->next = s;
		else
		    line = s;
	    } else
		p->next = s;
	}
	if (qisl) {
	    Cline qsl = bld_parts(asuf + psl + isl, qisl, qisl, NULL);

	    qsl->flags |= CLF_SUF;
	    qsl->suffix = qsl->prefix;
	    qsl->prefix = NULL;
	    if (sl)
		sl->next = qsl;
	    else
		p->next = qsl;
	}
    }
    /* The prefix is handled differently because the completion code
     * is much more eager to insert the -P prefix than it is to insert
     * the -S suffix. */
    if (qipre)
	palen = (qipl = strlen(qipre));
    if (ipre)
	palen += (ipl = strlen(ipre));
    if (pre)
	palen += (pl = strlen(pre));
    if (ppre)
	palen += (ppl = strlen(ppre));

    if (pl) {
	if (ppl) {
	    Cline lp, p = bld_parts(ppre, ppl, ppl, &lp);

	    if (lp->prefix && !(line->flags & (CLF_SUF | CLF_MID))) {
		lp->prefix->next = line->prefix;
		line->prefix = lp->prefix;
		lp->prefix = NULL;

		free_cline(lp);

		if (p != lp) {
		    Cline q;

		    for (q = p; q->next != lp; q = q->next);

		    q->next = line;
		    line = p;
		}
	    } else {
		lp->next = line;
		line = p;
	    }
	}
	if (pl) {
	    Cline lp, p = bld_parts(pre, pl, pl, &lp);

	    lp->next = line;
	    line = p;
	}
	if (ipl) {
	    Cline lp, p = bld_parts(ipre, ipl, ipl, &lp);

	    lp->next = line;
	    line = p;
	}
	if (qipl) {
	    Cline lp, p = bld_parts(qipre, qipl, qipl, &lp);

	    lp->next = line;
	    line = p;
	}
    } else if (palen) {
	char *apre = (char *) zhalloc(palen);
	Cline p, lp;

	if (qipl)
	    memcpy(apre, qipre, qipl);
	if (ipl)
	    memcpy(apre + qipl, ipre, ipl);
	if (pl)
	    memcpy(apre + qipl + ipl, pre, pl);
	if (ppl)
	    memcpy(apre + qipl + ipl + pl, ppre, ppl);

	p = bld_parts(apre, palen, palen, &lp);
	if (lp->prefix && !(line->flags & (CLF_SUF | CLF_MID))) {
	    lp->prefix->next = line->prefix;
	    line->prefix = lp->prefix;
	    lp->prefix = NULL;

	    free_cline(lp);

	    if (p != lp) {
		Cline q;

		for (q = p; q->next != lp; q = q->next);

		q->next = line;
		line = p;
	    }
	} else {
	    lp->next = line;
	    line = p;
	}
    }
    /* Then build the unambiguous cline list. */
    ai->line = join_clines(ai->line, line);

    mnum++;
    if (!alt)
	nmnum++;
    ai->count++;
    
    /* Allocate and fill the match structure. */
    cm = (Cmatch) zhalloc(sizeof(struct cmatch));
    cm->str = str;
    cm->ppre = (ppre && *ppre ? ppre : NULL);
    cm->psuf = (psuf && *psuf ? psuf : NULL);
    cm->prpre = ((flags & CMF_FILE) && prpre && *prpre ? prpre : NULL);
    if (qipre && *qipre)
	cm->ipre = (ipre && *ipre ? dyncat(qipre, ipre) : dupstring(qipre));
    else
	cm->ipre = (ipre && *ipre ? ipre : NULL);
    cm->ripre = (ripre && *ripre ? ripre : NULL);
    if (qisuf && *qisuf)
	cm->isuf = (isuf && *isuf ? dyncat(isuf, qisuf) : dupstring(qisuf));
    else
	cm->isuf = (isuf && *isuf ? isuf : NULL);
    cm->pre = pre;
    cm->suf = suf;
    cm->flags = flags;
    cm->brpl = bpl;
    cm->brsl = bsl;
    cm->qipl = qipl;
    cm->qisl = qisl;
    cm->autoq = (autoq ? autoq : (inbackt ? '`' : '\0'));
    cm->rems = cm->remf = NULL;
    addlinknode((alt ? fmatches : matches), cm);

    /* One more match for this explanation. */
    if (expl) {
	if (alt)
	    expl->fcount++;
	else
	    expl->count++;
    }
    if (!ai->firstm)
	ai->firstm = cm;

    /* Do we have an exact match? More than one? */
    if (exact) {
	if (!ai->exact) {
	    ai->exact = 1;
	    if (incompfunc) {
		/* If a completion widget is active, we make the exact
		 * string available in `compstate'. */

		int sl = strlen(str);
		int lpl = (cm->ppre ? strlen(cm->ppre) : 0);
		int lsl = (cm->psuf ? strlen(cm->psuf) : 0);
		char *e;

		zsfree(compexactstr);
		compexactstr = e = (char *) zalloc(lpl + sl + lsl + 1);
		if (cm->ppre) {
		    strcpy(e, cm->ppre);
		    e += lpl;
		}
		strcpy(e, str);
		e += sl;
		if (cm->psuf)
		    strcpy(e, cm->psuf);
		comp_setunsetptr(0, 0, CP_EXACTSTR, 0);
	    }
	    ai->exactm = cm;
	} else {
	    ai->exact = 2;
	    ai->exactm = NULL;
	    if (incompfunc)
		comp_setunsetptr(0, 0, 0, CP_EXACTSTR);
	}
    }
    return cm;
}

/* This stores the strings from the list in an array. */

static void
set_param(char *name, LinkList l)
{
    char **a, **p;
    LinkNode n;

    a = (char **) zalloc((countlinknodes(l) + 1) * sizeof(char *));
    for (p = a, n = firstnode(l); n; incnode(n))
	*p++ = ztrdup((char *) getdata(n));
    *p = NULL;

    setaparam(name, a);
}

/* This is used by compadd to add a couple of matches. The arguments are
 * the strings given via options. The last argument is the array with
 * the matches. */

/**/
int
addmatches(Cadata dat, char **argv)
{
    char *s, *ms, *lipre = NULL, *lisuf = NULL, *lpre = NULL, *lsuf = NULL;
    char **aign = NULL, **dparr = NULL, oaq = autoq;
    char *oqp = qipre, *oqs = qisuf, qc;
    int lpl, lsl, pl, sl, bpl, bsl, llpl = 0, llsl = 0, nm = mnum;
    int oisalt = 0, isalt, isexact, doadd, ois = instring, oib = inbackt;
    Cline lc = NULL;
    Cmatch cm;
    struct cmlist mst;
    Cmlist oms = mstack;
    Comp cp = NULL;
    LinkList aparl = NULL, oparl = NULL, dparl = NULL;

    if (compquote && (qc = *compquote)) {
	if (qc == '`') {
	    instring = 0;
	    inbackt = 0;
	    autoq = '\0';
	} else {
	    instring = (qc == '\'' ? 1 : 2);
	    inbackt = 0;
	    autoq = qc;
	}
    } else {
	instring = inbackt = 0;
	autoq = '\0';
    }
    qipre = ztrdup(compqiprefix ? compqiprefix : "");
    qisuf = ztrdup(compqisuffix ? compqisuffix : "");

    /* Switch back to the heap that was used when the completion widget
     * was invoked. */
    SWITCHHEAPS(compheap) {
	HEAPALLOC {
	    doadd = (!dat->apar && !dat->opar && !dat->dpar);
	    if (dat->apar)
		aparl = newlinklist();
	    if (dat->opar)
		oparl = newlinklist();
	    if (dat->dpar) {
		if (*(dat->dpar) == '(')
		    dparr = NULL;
		else if ((dparr = get_user_var(dat->dpar)) && !*dparr)
		    dparr = NULL;
		dparl = newlinklist();
	    }
	    if (dat->exp) {
		expl = (Cexpl) zhalloc(sizeof(struct cexpl));
		expl->count = expl->fcount = 0;
		expl->str = dupstring(dat->exp);
	    } else
		expl = NULL;

	    /* Store the matcher in our stack of matchers. */
	    if (dat->match) {
		mst.next = mstack;
		mst.matcher = dat->match;
		mstack = &mst;

		if (!mnum)
		    add_bmatchers(dat->match);

		addlinknode(matchers, dat->match);
		dat->match->refc++;
	    }
	    if (mnum && (mstack || bmatchers))
		update_bmatchers();

	    /* Get the suffixes to ignore. */
	    if (dat->ign)
		aign = get_user_var(dat->ign);
	    /* Get the contents of the completion variables if we have
	     * to perform matching. */
	    if (dat->aflags & CAF_MATCH) {
		if (dat->aflags & CAF_QUOTE) {
		    lipre = dupstring(compiprefix);
		    lisuf = dupstring(compisuffix);
		} else {
		    lipre = quotename(compiprefix, NULL);
		    lisuf = quotename(compisuffix, NULL);
		}
		lpre = dupstring(compprefix);
		lsuf = dupstring(compsuffix);
		llpl = strlen(lpre);
		llsl = strlen(lsuf);
		/* Test if there is an existing -P prefix. */
		if (dat->pre && *dat->pre) {
		    char *dp = rembslash(dat->pre);

		    pl = pfxlen(dp, lpre);
		    llpl -= pl;
		    lpre += pl;
		}
		if (comppatmatch && *comppatmatch) {
		    int is = (*comppatmatch == '*');
		    char *tmp = (char *) zhalloc(2 + llpl + llsl);

		    strcpy(tmp, lpre);
		    tmp[llpl] = 'x';
		    strcpy(tmp + llpl + is, lsuf);

		    tokenize(tmp);
		    remnulargs(tmp);
		    if (haswilds(tmp)) {
			if (is)
			    tmp[llpl] = Star;
			if ((cp = parsereg(tmp)))
			    haspattern = 1;
		    }
		}
	    }
	    /* Now duplicate the strings we have from the command line. */
	    if (dat->ipre)
		dat->ipre = (lipre ? dyncat(lipre, dat->ipre) :
			     dupstring(dat->ipre));
	    else if (lipre)
		dat->ipre = lipre;
	    if (dat->isuf)
		dat->isuf = (lisuf ? dyncat(lisuf, dat->isuf) :
			     dupstring(dat->isuf));
	    else if (lisuf)
		dat->isuf = lisuf;
	    if (dat->ppre) {
		dat->ppre = dupstring(dat->ppre);
		lpl = strlen(dat->ppre);
	    } else
		lpl = 0;
	    if (dat->psuf) {
		dat->psuf = dupstring(dat->psuf);
		lsl = strlen(dat->psuf);
	    } else
		lsl = 0;
	    if (dat->aflags & CAF_MATCH) {
		s = dat->ppre ? dat->ppre : "";
		if (llpl <= lpl && strpfx(lpre, s))
		    lpre = "";
		else if (llpl > lpl && strpfx(s, lpre))
		    lpre += lpl;
		else
		    *argv = NULL;
		s = dat->psuf ? dat->psuf : "";
		if (llsl <= lsl && strsfx(lsuf, s))
		    lsuf = "";
		else if (llsl > lsl && strsfx(s, lsuf))
		    lsuf[llsl - lsl] = '\0';
		else
		    *argv = NULL;
	    }
	    if (*argv) {
		if (dat->pre)
		    dat->pre = dupstring(dat->pre);
		if (dat->suf)
		    dat->suf = dupstring(dat->suf);
		if (!dat->prpre && (dat->prpre = dat->ppre)) {
		    singsub(&(dat->prpre));
		    untokenize(dat->prpre);
		} else
		    dat->prpre = dupstring(dat->prpre);
		/* Select the group in which to store the matches. */
		if (dat->group || dat->ylist) {
		    endcmgroup(NULL);
		    begcmgroup((dat->ylist ? NULL : dat->group),
			       (dat->aflags & CAF_NOSORT));
		    if (dat->aflags & CAF_NOSORT)
			mgroup->flags |= CGF_NOSORT;
		} else {
		    endcmgroup(NULL);
		    begcmgroup("default", 0);
		}
		/* Select the set of matches. */
		oisalt = (dat->aflags & CAF_ALT);

		if (dat->remf) {
		    dat->remf = dupstring(dat->remf);
		    dat->rems = NULL;
		} else if (dat->rems)
		    dat->rems = dupstring(dat->rems);

		/* Probably quote the prefix and suffix for testing. */
		if (!(dat->aflags & CAF_QUOTE)) {
		    if (!cp && (dat->aflags & CAF_MATCH)) {
			lpre = quotename(lpre, NULL);
			lsuf = quotename(lsuf, NULL);
		    }
		    if (dat->ppre) {
			dat->ppre = quotename(dat->ppre, NULL);
			if ((dat->flags & CMF_FILE) &&
			    dat->ppre[0] == '\\' && dat->ppre[1] == '~')
			    chuck(dat->ppre);
		    }
		    if (dat->psuf)
			dat->psuf = quotename(dat->psuf, NULL);
		}
	    }
	    /* Walk through the matches given. */
	    for (; (s = *argv); argv++) {
		sl = strlen(s);
		bpl = brpl;
		bsl = brsl;
		isalt = oisalt;
		if ((!dat->psuf || !*(dat->psuf)) && aign) {
		    /* Do the suffix-test. If the match has one of the
		     * suffixes from ign, we put it in the alternate set. */
		    char **pt = aign;
		    int filell;

		    for (isalt = 0; !isalt && *pt; pt++)
			if ((filell = strlen(*pt)) < sl
			    && !strcmp(*pt, s + sl - filell))
			    isalt = 1;

		    if (isalt && !doadd) {
			if (dparr && !*++dparr)
			    dparr = NULL;
			continue;
		    }
		}
		if (!(dat->aflags & CAF_MATCH)) {
		    if (dat->aflags & CAF_QUOTE)
			ms = dupstring(s);
		    else
			sl = strlen(ms = quotename(s, NULL));
		    lc = bld_parts(ms, sl, -1, NULL);
		    isexact = 0;
		} else if (!(ms = comp_match(lpre, lsuf, s, cp, &lc,
					     (!(dat->aflags & CAF_QUOTE) ?
					      ((dat->ppre && dat->ppre) ||
					       !(dat->flags & CMF_FILE) ? 1 : 2) : 0),
					     &bpl, &bsl, &isexact))) {
		    if (dparr && !*++dparr)
			dparr = NULL;
		    continue;
		}
		if (doadd) {
		    cm = add_match_data(isalt, ms, lc, dat->ipre, NULL,
					dat->isuf, dat->pre, dat->prpre,
					dat->ppre, dat->psuf, dat->suf,
					bpl, bsl, dat->flags, isexact);
		    cm->rems = dat->rems;
		    cm->remf = dat->remf;
		} else {
		    if (dat->apar)
			addlinknode(aparl, ms);
		    if (dat->opar)
			addlinknode(oparl, s);
		    if (dat->dpar && dparr) {
			addlinknode(dparl, *dparr);
			if (!*++dparr)
			    dparr = NULL;
		    }
		    free_cline(lc);
		}
	    }
	    compnmatches = mnum;
	    compnnmatches = nmnum;
	    if (dat->exp)
		addexpl();
	    if (dat->apar)
		set_param(dat->apar, aparl);
	    if (dat->opar)
		set_param(dat->opar, oparl);
	    if (dat->dpar)
		set_param(dat->dpar, dparl);
	    if (dat->ylist) {
		endcmgroup(get_user_var(dat->ylist));
		begcmgroup("default", 0);
	    }
	} LASTALLOC;
    } SWITCHBACKHEAPS;

    /* We switched back to the current heap, now restore the stack of
     * matchers. */
    mstack = oms;

    instring = ois;
    inbackt = oib;
    autoq = oaq;
    zsfree(qipre);
    zsfree(qisuf);
    qipre = oqp;
    qisuf = oqs;

    return (mnum == nm);
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
    int isfile = 0, isalt = 0, isexact, bpl = brpl, bsl = brsl;
    char *ms = NULL, *tt;
    HashNode hn;
    Param pm;
    Cline lc = NULL;

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
 *          (things with `~' or `=' at the beginning, ...).
 */

    /* Just to make the code cleaner */
    hn = (HashNode) t;
    pm = (Param) t;

    if (addwhat == -1 || addwhat == -5 || addwhat == -6 ||
	addwhat == CC_FILES || addwhat == -7 || addwhat == -8) {
	if ((addwhat == CC_FILES ||
	     addwhat == -5) && !*psuf) {
	    /* If this is a filename, do the fignore check. */
	    char **pt = fignore;
	    int filell, sl = strlen(s);

	    for (isalt = 0; !isalt && *pt; pt++)
		if ((filell = strlen(*pt)) < sl &&
		    !strcmp(*pt, s + sl - filell))
		    isalt = 1;
	}
	ms = ((addwhat == CC_FILES || addwhat == -6 ||
	       addwhat == -5 || addwhat == -8) ? 
	      comp_match(qfpre, qfsuf, s, filecomp, &lc, (ppre && *ppre ? 1 : 2),
			 &bpl, &bsl, &isexact) :
	      comp_match(fpre, fsuf, s, filecomp, &lc, 0,
			 &bpl, &bsl, &isexact));
	if (!ms)
	    return;

	if (addwhat == -7 && !findcmd(s, 0))
	    return;
	isfile = CMF_FILE;
    } else if (addwhat == CC_QUOTEFLAG || addwhat == -2  ||
	      (addwhat == -3 && !(hn->flags & DISABLED)) ||
	      (addwhat == -4 && (PM_TYPE(pm->flags) == PM_SCALAR) &&
	       !pm->level && (tt = pm->gets.cfn(pm)) && *tt == '/')    ||
	      (addwhat == -9 && !(hn->flags & PM_UNSET) && !pm->level) ||
	      (addwhat > 0 &&
	       ((!(hn->flags & PM_UNSET) &&
		 (((addwhat & CC_ARRAYS)    &&  (hn->flags & PM_ARRAY))    ||
		  ((addwhat & CC_INTVARS)   &&  (hn->flags & PM_INTEGER))  ||
		  ((addwhat & CC_ENVVARS)   &&  (hn->flags & PM_EXPORTED)) ||
		  ((addwhat & CC_SCALARS)   &&  (hn->flags & PM_SCALAR))   ||
		  ((addwhat & CC_READONLYS) &&  (hn->flags & PM_READONLY)) ||
		  ((addwhat & CC_SPECIALS)  &&  (hn->flags & PM_SPECIAL))  ||
		  ((addwhat & CC_PARAMS)    && !(hn->flags & PM_EXPORTED))) &&
		 !pm->level) ||
		((( addwhat & CC_SHFUNCS)				  ||
		  ( addwhat & CC_BUILTINS)				  ||
		  ( addwhat & CC_EXTCMDS)				  ||
		  ( addwhat & CC_RESWDS)				  ||
		  ((addwhat & CC_ALREG)   && !(hn->flags & ALIAS_GLOBAL)) ||
		  ((addwhat & CC_ALGLOB)  &&  (hn->flags & ALIAS_GLOBAL))) &&
		 (((addwhat & CC_DISCMDS) && (hn->flags & DISABLED)) ||
		  ((addwhat & CC_EXCMDS)  && !(hn->flags & DISABLED)))) ||
		((addwhat & CC_BINDINGS) && !(hn->flags & DISABLED))))) {
	char *p1, *s1, *p2, *s2;

	if (addwhat == CC_QUOTEFLAG) {
	    p1 = qrpre; s1 = qrsuf;
	    p2 = rpre;  s2 = rsuf;
	} else {
	    p1 = qlpre; s1 = qlsuf;
	    p2 = lpre;  s2 = lsuf;
	}
	if (!(ms = comp_match(p1, s1, s, patcomp, &lc,
			      (addwhat == CC_QUOTEFLAG),
			      &bpl, &bsl, &isexact)) &&
	    !(ms = comp_match(p2, s2, s, NULL, &lc,
			      (addwhat == CC_QUOTEFLAG),
			      &bpl, &bsl, &isexact)))
	    return;
    }
    if (!ms)
	return;
    add_match_data(isalt, ms, lc, ipre, ripre, isuf, 
		   (incompfunc ? dupstring(curcc->prefix) : curcc->prefix),
		   prpre, 
		   (isfile ? lppre : NULL),
		   (isfile ? lpsuf : NULL),
		   (incompfunc ? dupstring(curcc->suffix) : curcc->suffix),
		   bpl, bsl, (mflags | isfile), isexact);
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
			if (ispattern ||
			    (ns && comppatmatch && *comppatmatch)) {
			    /* Yes, so append a `*' if needed. */
			    if (ns && comppatmatch && *comppatmatch == '*') {
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
static int
docompletion(char *s, int lst, int incmd)
{
    int ret = 0;

    HEAPALLOC {
	char *opm;
	LinkNode n;

	pushheap();

	ainfo = fainfo = NULL;
	matchers = newlinklist();

	hasunqu = 0;
	useline = (lst != COMP_LIST_COMPLETE);
	useexact = (isset(RECEXACT) && usemenu != 1);
	uselist = (useline ?
		   ((isset(AUTOLIST) && !isset(BASHAUTOLIST)) ? 
		    (isset(LISTAMBIGUOUS) ? 3 : 2) : 0) : 1);
	zsfree(comppatmatch);
	opm = comppatmatch = ztrdup(useglob ? "*" : "");
	zsfree(comppatinsert);
	comppatinsert = ztrdup("menu");
	zsfree(compforcelist);
	compforcelist = ztrdup("");
	haspattern = 0;
	complistmax = getiparam("LISTMAX");
	zsfree(complastprompt);
	complastprompt = ztrdup(((isset(ALWAYSLASTPROMPT) && zmult == 1) ||
				(unset(ALWAYSLASTPROMPT) && zmult != 1)) ?
				"yes" : "");
	movetoend = ((cs == we || isset(ALWAYSTOEND)) ? 2 : 1);
	showinglist = 0;

	/* Make sure we have the completion list and compctl. */
	if (makecomplist(s, incmd, lst)) {
	    /* Error condition: feeeeeeeeeeeeep(). */
	    cs = 0;
	    foredel(ll);
	    inststr(origline);
	    cs = origcs;
	    clearlist = 1;
	    ret = 1;
	    minfo.cur = NULL;
	    goto compend;
	}
	if (comppatmatch && *comppatmatch && comppatmatch != opm)
	    haspattern = 1;
	if (!useline && uselist) {
	    /* All this and the guy only wants to see the list, sigh. */
	    cs = 0;
	    foredel(ll);
	    inststr(origline);
	    cs = origcs;
	    showinglist = -2;
	} else if (useline) {
	    /* We have matches. */
	    if (nmatches > 1) {
		/* There is more than one match. */
		ret = do_ambiguous();
	    } else if (nmatches == 1) {
		/* Only one match. */
		Cmgroup m = amatches;

		while (!m->mcount)
		    m = m->next;
		minfo.cur = NULL;
		minfo.asked = 0;
		do_single(m->matches[0]);
		if (compforcelist && *compforcelist && uselist)
		    showinglist = -2;
		else
		    invalidatelist();
	    }
	} else {
	    invalidatelist();
	    cs = 0;
	    foredel(ll);
	    inststr(origline);
	    cs = origcs;
	}
	/* Print the explanation strings if needed. */
	if (!showinglist && validlist && usemenu != 2 && nmatches != 1) {
	    Cmgroup g = amatches;
	    Cexpl *e;
	    int up = 0, tr = 1, nn = 0;

	    if (!nmatches)
		ret = 1;

	    while (g) {
		if ((e = g->expls))
		    while (*e) {
			if ((*e)->count) {
			    if (tr) {
				trashzle();
				tr = 0;
			    }
			    if (nn) {
				up++;
				putc('\n', shout);
			    }
			    up += printfmt((*e)->str, (*e)->count, 1);
			    nn = 1;
			}
			e++;
		    }
		g = g->next;
	    }
	    if (!tr) {
		clearflag = (isset(USEZLE) && !termflags &&
			      complastprompt && *complastprompt);

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
    return ret;
}

/* This calls the given function for new style completion. */

/**/
static void
callcompfunc(char *s, char *fn)
{
    List list;
    int lv = lastval;
    char buf[20];

    if ((list = getshfunc(fn)) != &dummy_list) {
	char **p, *tmp;
	int aadd = 0, usea = 1, icf = incompfunc, osc = sfcontext;
	unsigned int rset, kset;
	Param *ocrpms = comprpms, *ockpms = compkpms;

	comprpms = (Param *) zalloc(CP_REALPARAMS * sizeof(Param));
	compkpms = (Param *) zalloc(CP_KEYPARAMS * sizeof(Param));

	rset = CP_ALLREALS;
	kset = CP_ALLKEYS &
	    ~(CP_PARAMETER | CP_REDIRECT | CP_QUOTE | CP_QUOTING |
	      CP_EXACTSTR | CP_FORCELIST | CP_OLDLIST | CP_OLDINS |
	      (useglob ? 0 : CP_PATMATCH));
	zsfree(compvared);
	if (varedarg) {
	    compvared = ztrdup(varedarg);
	    kset |= CP_VARED;
	} else
	    compvared = ztrdup("");
	if (!*complastprompt)
	    kset &= ~CP_LASTPROMPT;
	zsfree(compcontext);
	zsfree(compparameter);
	zsfree(compredirect);
	compparameter = compredirect = "";
	if (ispar)
	    compcontext = (ispar == 2 ? "brace_parameter" : "parameter");
	else if (linwhat == IN_MATH) {
	    if (insubscr) {
		compcontext = "subscript";
		if (varname) {
		    compparameter = varname;
		    kset |= CP_PARAMETER;
		}
	    } else
		compcontext = "math";
	    usea = 0;
	} else if (lincmd) {
	    if (insubscr) {
		compcontext = "subscript";
		kset |= CP_PARAMETER;
	    } else
		compcontext = "command";
	} else if (linredir) {
	    compcontext = "redirect";
	    if (rdstr)
		compredirect = rdstr;
	    kset |= CP_REDIRECT;
	} else
	    switch (linwhat) {
	    case IN_ENV:
		compcontext = (linarr ? "array_value" : "value");
		compparameter = varname;
		kset |= CP_PARAMETER;
		if (!clwpos) {
		    clwpos = 1;
		    clwnum = 2;
		    zsfree(clwords[1]);
		    clwords[1] = ztrdup(s);
		    zsfree(clwords[2]);
		    clwords[2] = NULL;
		}
		aadd = 1;
		break;
	    case IN_COND:
		compcontext = "condition";
		break;
	    default:
		if (cmdstr)
		    compcontext = "command";
		else {
		    compcontext = "value";
		    kset |= CP_PARAMETER;
		    if (clwords[0])
			compparameter = clwords[0];
		    aadd = 1;
		}
	    }
	compcontext = ztrdup(compcontext);
	if (compwords)
	    freearray(compwords);
	if (usea && (!aadd || clwords[0])) {
	    char **q;

	    PERMALLOC {
		q = compwords = (char **)
		    zalloc((clwnum + 1) * sizeof(char *));
		for (p = clwords + aadd; *p; p++, q++) {
		    tmp = dupstring(*p);
		    untokenize(tmp);
		    *q = ztrdup(tmp);
		}
		*q = NULL;
	    } LASTALLOC;
	} else
	    compwords = (char **) zcalloc(sizeof(char *));

	compparameter = ztrdup(compparameter);
	compredirect = ztrdup(compredirect);
	zsfree(compquote);
	zsfree(compquoting);
	if (instring) {
	    if (instring == 1) {
		compquote = ztrdup("\'");
		compquoting = ztrdup("single");
	    } else {
		compquote = ztrdup("\"");
		compquoting = ztrdup("double");
	    }
	    kset |= CP_QUOTE | CP_QUOTING;
	} else if (inbackt) {
	    compquote = ztrdup("`");
	    compquoting = ztrdup("backtick");
	    kset |= CP_QUOTE | CP_QUOTING;
	} else {
	    compquote = ztrdup("");
	    compquoting = ztrdup("");
	}
	zsfree(compprefix);
	zsfree(compsuffix);
	if (unset(COMPLETEINWORD)) {
	    tmp = quotename(s, NULL);
	    untokenize(tmp);
	    compprefix = ztrdup(tmp);
	    compsuffix = ztrdup("");
	} else {
	    char *ss, sav;
	    
	    ss = s + offs;

	    sav = *ss;
	    *ss = '\0';
	    tmp = quotename(s, NULL);
	    untokenize(tmp);
	    compprefix = ztrdup(tmp);
	    *ss = sav;
	    ss = quotename(ss, NULL);
	    untokenize(ss);
	    compsuffix = ztrdup(ss);
	}
	zsfree(compiprefix);
	compiprefix = ztrdup("");
	zsfree(compisuffix);
	compisuffix = ztrdup("");
	zsfree(compqiprefix);
	compqiprefix = ztrdup(qipre ? qipre : "");
	zsfree(compqisuffix);
	compqisuffix = ztrdup(qisuf ? qisuf : "");
	compcurrent = (usea ? (clwpos + 1 - aadd) : 0);
	compnmatches = mnum;
	compnnmatches = nmnum;

	zsfree(complist);
	switch (uselist) {
	case 0: complist = ""; kset &= ~CP_LIST; break;
	case 1: complist = "list"; break;
	case 2: complist = "autolist"; break;
	case 3: complist = "ambiguous"; break;
	}
	complist = ztrdup(complist);
	zsfree(compinsert);
	if (useline) {
	    switch (usemenu) {
	    case 0: compinsert = "unambiguous"; break;
	    case 1: compinsert = "menu"; break;
	    case 2: compinsert = "automenu"; break;
	    }
	} else {
	    compinsert = "";
	    kset &= ~CP_INSERT;
	}
	compinsert = ztrdup(compinsert);
	if (useexact)
	    compexact = ztrdup("accept");
	else {
	    compexact = ztrdup("");
	    kset &= ~CP_EXACT;
	}
	zsfree(comptoend);
	if (movetoend == 1)
	    comptoend = ztrdup("single");
	else
	    comptoend = ztrdup("match");
	zsfree(compoldlist);
	zsfree(compoldins);
	if (hasperm && permmnum) {
	    if (listshown)
		compoldlist = "shown";
	    else
		compoldlist = "yes";
	    kset |= CP_OLDLIST;
	    if (minfo.cur) {
		sprintf(buf, "%d", (*(minfo.cur))->gnum);
		compoldins = buf;
		kset |= CP_OLDINS;
	    } else
		compoldins = "";
	} else
	    compoldlist = compoldins = "";
	compoldlist = ztrdup(compoldlist);
	compoldins = ztrdup(compoldins);

	incompfunc = 1;
	startparamscope();
	makecompparamsptr();
	comp_setunsetptr(rset, (~rset & CP_ALLREALS),
			 kset, (~kset & CP_ALLKEYS));
	makezleparams(1);
	sfcontext = SFC_CWIDGET;
	NEWHEAPS(compheap) {
	    LinkList largs = NULL;
	    int olv = lastval;

	    if (*cfargs) {
		char **p = cfargs;

		largs = newlinklist();
		addlinknode(largs, dupstring(fn));
		while (*p)
		    addlinknode(largs, dupstring(*p++));
	    }
	    doshfunc(fn, list, largs, 0, 0);
	    cfret = lastval;
	    lastval = olv;
	} OLDHEAPS;
	sfcontext = osc;
	endparamscope();
	lastcmd = 0;
	incompfunc = icf;

	if (!complist)
	    uselist = 0;
	else if (!strcmp(complist, "list"))
	    uselist = 1;
	else if (!strcmp(complist, "auto") || !strcmp(complist, "autolist"))
	    uselist = 2;
	else if (!strcmp(complist, "ambig") || !strcmp(complist, "ambiguous"))
	    uselist = 3;
	else
	    uselist = 0;
	if (!compinsert)
	    useline = 0;
	else if (!strcmp(compinsert, "unambig") ||
		 !strcmp(compinsert, "unambiguous"))
	    useline = 1, usemenu = 0;
	else if (!strcmp(compinsert, "menu"))
	    useline = 1, usemenu = 1;
	else if (!strcmp(compinsert, "auto") ||
		 !strcmp(compinsert, "automenu"))
	    useline = 1, usemenu = 2;
	else if (idigit(*compinsert)) {
	    char *m;

	    useline = 1; usemenu = 3;
	    insmnum = atoi(compinsert);
	    if ((m = strchr(compinsert, ':'))) {
		insgroup = 1;
		insgnum = atoi(m + 1);
	    }
	    insspace = (compinsert[strlen(compinsert) - 1] == ' ');
	} else
	    useline = usemenu = 0;
	useexact = (compexact && !strcmp(compexact, "accept"));

	if (!comptoend || !*comptoend)
	    movetoend = 0;
	else if (!strcmp(comptoend, "single"))
	    movetoend = 1;
	else if (!strcmp(comptoend, "always"))
	    movetoend = 3;
	else
	    movetoend = 2;

	oldlist = (hasperm && compoldlist && !strcmp(compoldlist, "keep"));
	oldins = (hasperm && minfo.cur &&
		  compoldins && !strcmp(compoldins, "keep"));

	zfree(comprpms, CP_REALPARAMS * sizeof(Param));
	zfree(compkpms, CP_KEYPARAMS * sizeof(Param));
	comprpms = ocrpms;
	compkpms = ockpms;
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
    char *p, *os = s;
    int onm = nmatches, osi = movefd(0);

    /* Inside $... ? */
    if (compfunc && (p = check_param(s, 0, 0)))
	os = s = p;

    /* We build a copy of the list of matchers to use to make sure that this
     * works even if a shell function called from the completion code changes
     * the global matchers. */

    if ((m = cmatcher)) {
	Cmlist mm, *mp = &mm;
	int n;

	for (n = 0; m; m = m->next, n++) {
	    *mp = (Cmlist) zhalloc(sizeof(struct cmlist));
	    (*mp)->matcher = m->matcher;
	    (*mp)->next = NULL;
	    (*mp)->str = dupstring(m->str);
	    mp = &((*mp)->next);
	    addlinknode(matchers, m->matcher);
	    m->matcher->refc++;
	}
	m = mm;
	compmatcher = 1;
	compmatchertot = n;
    } else
	compmatcher = 0;

    linwhat = inwhat;

    /* Walk through the global matchers. */
    for (;;) {
	bmatchers = NULL;
	zsfree(compmatcherstr);
	if (m) {
	    ms.next = NULL;
	    ms.matcher = m->matcher;
	    mstack = &ms;

	    /* Store the matchers used in the bmatchers list which is used
	     * when building new parts for the string to insert into the 
	     * line. */
	    add_bmatchers(m->matcher);
	    compmatcherstr = ztrdup(m->str);
	} else {
	    mstack = NULL;
	    compmatcherstr = ztrdup("");
	}
	ainfo = (Aminfo) hcalloc(sizeof(struct aminfo));
	fainfo = (Aminfo) hcalloc(sizeof(struct aminfo));

	freecl = NULL;

	if (!validlist)
	    lastambig = 0;
	amatches = NULL;
	mnum = nmnum = 0;
	unambig_mnum = -1;
	isuf = NULL;
	insmnum = insgnum = 1;
	insgroup = oldlist = oldins = 0;
	begcmgroup("default", 0);
	menucmp = menuacc = 0;

	ccused = newlinklist();
	ccstack = newlinklist();

	s = dupstring(os);
	if (compfunc)
	    callcompfunc(s, compfunc);
	else
	    makecomplistglobal(s, incmd, lst, 0);

	endcmgroup(NULL);

	if (amatches && !oldlist)
	    amatches->ccs = (Compctl *) makearray(ccused, 0,
						  &(amatches->ccount), NULL);
	else {
	    LinkNode n;

	    for (n = firstnode(ccused); n; incnode(n))
		freecompctl((Compctl) getdata(n));
	}
	if (oldlist) {
	    nmatches = onm;
	    validlist = 1;
	    amatches = pmatches;

	    redup(osi, 0);

	    return 0;
	}
	PERMALLOC {
	    permmatches();
	} LASTALLOC;

	if (nmatches && !errflag) {
	    validlist = 1;

	    redup(osi, 0);

	    return 0;
	}
	if (!m || !(m = m->next))
	    break;

	errflag = 0;
	compmatcher++;
    }
    redup(osi, 0);
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
	    if (*p == '$' || *p == '{' || *p == '}') {
		if (bslash)
		    p[-1] = Bnull;
		else
		    *p = (*p == '$' ? String :
			  (*p == '{' ? Inbrace : Outbrace));
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
    lp = strlen(p);
    ls = strlen(s);
    lip = strlen(ip);
    str = zhalloc(lip + lp + ls + 1);
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
int
set_comp_sep(void)
{
    int lip, lp;
    char *s = comp_str(&lip, &lp, 0);

    if (compisuffix)
	s = dyncat(s, compisuffix);

    return sep_comp_string("", s, lip + lp, 0);
}

/**/
static int
sep_comp_string(char *ss, char *s, int noffs, int rec)
{
    LinkList foo = newlinklist();
    LinkNode n;
    int owe = we, owb = wb, ocs = cs, swb, swe, scs, soffs, ne = noerrs;
    int sl = strlen(ss), tl, got = 0, i = 0, cur = -1, oll = ll;
    int ois = instring, oib = inbackt;
    char *tmp, *p, *ns, *ol = (char *) line, sav, oaq = autoq, *qp, *qs;

    swb = swe = soffs = 0;
    ns = NULL;

    /* Put the string in the lexer buffer and call the lexer to *
     * get the words we have to expand.                        */
    zleparse = 1;
    addedx = 1;
    noerrs = 1;
    lexsave();
    tmp = (char *) zhalloc(tl = sl + 3 + strlen(s));
    strcpy(tmp, ss);
    tmp[sl] = ' ';
    memcpy(tmp + sl + 1, s, noffs);
    tmp[(scs = cs = sl + 1 + noffs)] = 'x';
    strcpy(tmp + sl + 2 + noffs, s + noffs);
    if (incompfunc)
	tmp = rembslash(tmp);
    inpush(dupstrspace(tmp), 0, NULL);
    line = (unsigned char *) tmp;
    ll = tl - 1;
    strinbeg(0);
    noaliases = 1;
    do {
	ctxtlex();
	if (tok == LEXERR) {
	    int j;

	    if (!tokstr)
		break;
	    for (j = 0, p = tokstr; *p; p++)
		if (*p == Snull || *p == Dnull)
		    j++;
	    if (j & 1) {
		tok = STRING;
		if (p > tokstr && p[-1] == ' ')
		    p[-1] = '\0';
	    }
	}
	if (tok == ENDINPUT || tok == LEXERR)
	    break;
	if (tokstr && *tokstr)
	    addlinknode(foo, (p = ztrdup(tokstr)));
	else
	    p = NULL;
	if (!got && !zleparse) {
	    DPUTS(!p, "no current word in substr");
	    got = 1;
	    cur = i;
	    swb = wb - 1;
	    swe = we - 1;
	    soffs = cs - swb;
	    chuck(p + soffs);
	    ns = dupstring(p);
	}
	i++;
    } while (tok != ENDINPUT && tok != LEXERR);
    noaliases = 0;
    strinend();
    inpop();
    errflag = zleparse = 0;
    noerrs = ne;
    lexrestore();
    wb = owb;
    we = owe;
    cs = ocs;
    line = (unsigned char *) ol;
    ll = oll;
    if (cur < 0 || i < 1)
	return 1;
    owb = offs;
    offs = soffs;
    if ((p = check_param(ns, 0, 1))) {
	for (p = ns; *p; p++)
	    if (*p == Dnull)
		*p = '"';
	    else if (*p == Snull)
		*p = '\'';
    }
    offs = owb;
    if (*ns == Snull || *ns == Dnull) {
	instring = (*ns == Snull ? 1 : 2);
	inbackt = 0;
	swb++;
	if (ns[strlen(ns) - 1] == *ns && ns[1])
	    swe--;
	autoq = (*ns == Snull ? '\'' : '"');
    } else {
	instring = 0;
	autoq = '\0';
    }
    for (p = ns, i = swb; *p; p++, i++) {
	if (INULL(*p)) {
	    if (i < scs)
		soffs--;
	    if (p[1] || *p != Bnull) {
		if (*p == Bnull) {
		    if (scs == i + 1)
			scs++, soffs++;
		} else {
		    if (scs > i--)
			scs--;
		}
	    } else {
		if (scs == swe)
		    scs--;
	    }
	    chuck(p--);
	}
    }
    sav = s[(i = swb - sl - 1)];
    s[i] = '\0';
    qp = tricat(qipre, (incompfunc ? rembslash(s) : s), "");
    s[i] = sav;
    if (swe < swb)
	swe = swb;
    swe -= sl + 1;
    sl = strlen(s);
    if (swe > sl)
	swe = sl, ns[swe - swb + 1] = '\0';
    qs = tricat((incompfunc ? rembslash(s + swe) : s + swe), qisuf, "");
    sl = strlen(ns);
    if (soffs > sl)
	soffs = sl;

    if (rec) {
	char **ow = clwords, *os = cmdstr, *oqp = qipre, *oqs = qisuf;
	int olws = clwsize, olwn = clwnum, olwp = clwpos;
	int obr = brange, oer = erange, oof = offs;
	unsigned long occ = ccont;

	clwsize = clwnum = countlinknodes(foo);
	clwords = (char **) zalloc((clwnum + 1) * sizeof(char *));
	for (n = firstnode(foo), i = 0; n; incnode(n), i++) {
	    p = clwords[i] = (char *) getdata(n);
	    untokenize(p);
	}
	clwords[i] = NULL;
	clwpos = cur;
	cmdstr = ztrdup(clwords[0]);
	brange = 0;
	erange = clwnum - 1;
	qipre = qp;
	qisuf = qs;
	offs = soffs;
	ccont = CC_CCCONT;
	makecomplistcmd(ns, !clwpos, CFN_FIRST);
	ccont = occ;
	offs = oof;
	zsfree(cmdstr);
	cmdstr = os;
	freearray(clwords);
	clwords = ow;
	clwsize = olws;
	clwnum = olwn;
	clwpos = olwp;
	brange = obr;
	erange = oer;
	zsfree(qipre);
	qipre = oqp;
	zsfree(qisuf);
	qisuf = oqs;
    } else {
	int set = CP_QUOTE | CP_QUOTING, unset = 0;

	zsfree(compquote);
	zsfree(compquoting);
	if (instring == 2) {
	    compquote = "\"";
	    compquoting = "double";
	} else if (instring == 1) {
	    compquote = "'";
	    compquoting = "single";
	} else {
	    compquote = compquoting = "";
	    unset = set;
	    set = 0;
	}
	compquote = ztrdup(compquote);
	compquoting = ztrdup(compquoting);
	comp_setunsetptr(0, 0, set, unset);

	if (unset(COMPLETEINWORD)) {
	    untokenize(ns);
	    zsfree(compprefix);
	    compprefix = ztrdup(ns);
	    zsfree(compsuffix);
	    compsuffix = ztrdup("");
	} else {
	    char *ss, sav;
	    
	    ss = ns + soffs;

	    sav = *ss;
	    *ss = '\0';
	    untokenize(ns);
	    compprefix = ztrdup(ns);
	    *ss = sav;
	    untokenize(ss);
	    compsuffix = ztrdup(ss);
	}
	zsfree(compiprefix);
	compiprefix = ztrdup("");
	zsfree(compisuffix);
	compisuffix = ztrdup("");
	zsfree(compqiprefix);
	zsfree(compqisuffix);
	if (ois) {
	    compqiprefix = qp;
	    compqisuffix = qs;
	} else {
	    compqiprefix = ztrdup(quotename(qp, NULL));
	    zsfree(qp);
	    compqisuffix = ztrdup(quotename(qs, NULL));
	    zsfree(qs);
	}
	freearray(compwords);
	i = countlinknodes(foo);
	compwords = (char **) zalloc((i + 1) * sizeof(char *));
	for (n = firstnode(foo), i = 0; n; incnode(n), i++) {
	    p = compwords[i] = (char *) getdata(n);
	    untokenize(p);
	}
	compcurrent = cur + 1;
	compwords[i] = NULL;
    }
    autoq = oaq;
    instring = ois;
    inbackt = oib;

    return 0;
}

/**/
int
makecomplistcall(Compctl cc)
{
    int nm = mnum;

    SWITCHHEAPS(compheap) {
	HEAPALLOC {
	    int ooffs = offs, lip, lp, ois = instring, oib = inbackt;
	    char *str = comp_str(&lip, &lp, 0), qc;
	    char *oisuf = isuf, *oqp = qipre, *oqs = qisuf, oaq = autoq;

	    if (compquote && (qc = *compquote)) {
		if (qc == '`') {
		    instring = 0;
		    inbackt = 0;
		    autoq = '\0';
		} else {
		    instring = (qc == '\'' ? 1 : 2);
		    inbackt = 0;
		    autoq = qc;
		}
	    } else {
		instring = inbackt = 0;
		autoq = '\0';
	    }
	    isuf = dupstring(compisuffix);
	    ctokenize(isuf);
	    remnulargs(isuf);
	    qipre = ztrdup(compqiprefix ? compqiprefix : "");
	    qisuf = ztrdup(compqisuffix ? compqisuffix : "");
	    offs = lip + lp;
	    cc->refc++;
	    ccont = 0;
	    makecomplistor(cc, str, lincmd, lip, 0);
	    offs = ooffs;
	    isuf = oisuf;
	    zsfree(qipre);
	    zsfree(qisuf);
	    qipre = oqp;
	    qisuf = oqs;
	    instring = ois;
	    inbackt = oib;
	    autoq = oaq;
	    compnmatches = mnum;
	    compnnmatches = nmnum;
	} LASTALLOC;
    } SWITCHBACKHEAPS;

    return (mnum == nm);
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
	    char *os = cmdstr, **ow = clwords, **p, **q, qc;
	    int on = clwnum, op = clwpos, ois =  instring, oib = inbackt;
	    char *oisuf = isuf, *oqp = qipre, *oqs = qisuf, oaq = autoq;

	    if (compquote && (qc = *compquote)) {
		if (qc == '`') {
		    instring = 0;
		    inbackt = 0;
		    autoq = '\0';
		} else {
		    instring = (qc == '\'' ? 1 : 2);
		    inbackt = 0;
		    autoq = qc;
		}
	    } else {
		instring = inbackt = 0;
		autoq = '\0';
	    }
	    qipre = ztrdup(compqiprefix ? compqiprefix : "");
	    qisuf = ztrdup(compqisuffix ? compqisuffix : "");
	    isuf = dupstring(compisuffix);
	    ctokenize(isuf);
	    remnulargs(isuf);
	    clwnum = arrlen(compwords);
	    clwpos = compcurrent - 1;
	    cmdstr = ztrdup(compwords[0]);
	    clwords = (char **) zalloc((clwnum + 1) * sizeof(char *));
	    for (p = compwords, q = clwords; *p; p++, q++) {
		t = dupstring(*p);
		tokenize(t);
		remnulargs(t);
		*q = ztrdup(t);
	    }
	    *q = NULL;
	    offs = lip + lp;
	    incompfunc = 2;
	    ret = makecomplistglobal(str, !clwpos, COMP_COMPLETE, flags);
	    incompfunc = 1;
	    isuf = oisuf;
	    zsfree(qipre);
	    zsfree(qisuf);
	    qipre = oqp;
	    qisuf = oqs;
	    instring = ois;
	    inbackt = oib;
	    autoq = oaq;
	    offs = ooffs;
	    compnmatches = mnum;
	    compnnmatches = nmnum;
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
    cc_dummy.suffix = NULL;

    if (linwhat == IN_ENV) {
        /* Default completion for parameter values. */
        cc = &cc_default;
	keypm = NULL;
    } else if (linwhat == IN_MATH) {
	if (insubscr >= 2) {
	    /* Inside subscript of assoc array, complete keys. */
	    cc_dummy.mask = 0;
	    cc_dummy.suffix = (insubscr == 2 ? "]" : "");
	} else {
	    /* Other math environment, complete paramete names. */
	    keypm = NULL;
	    cc_dummy.mask = CC_PARAMS;
	}
	cc = &cc_dummy;
	cc_dummy.refc = 10000;
    } else if (linwhat == IN_COND) {
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
	keypm = NULL;
    } else if (linredir) {
	/* In redirections use default completion. */
	cc = &cc_default;
	keypm = NULL;
    } else {
	/* Otherwise get the matches for the command. */
	keypm = NULL;
	return makecomplistcmd(os, incmd, flags);
    }
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
    int compadd, m = 0, d = 0, t, tt, i, j, a, b, ins;
    char *sc = NULL, *s, *ss;

    ins = (instring ? instring : (inbackt ? 3 : 0));

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
		    case CCT_QUOTE:
			t = ((cc->u.s.s[i][0] == 's' && ins == 1) ||
			     (cc->u.s.s[i][0] == 'd' && ins == 2) ||
			     (cc->u.s.s[i][0] == 'b' && ins == 3));
			break;
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
			for (j = clwpos - 1; j > 0; j--) {
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
    int t, sf1, sf2, ooffs, um = usemenu, delit, oaw;
    char *p, *sd = NULL, *tt, *s1, *s2, *os =  dupstring(s);
    struct cmlist ms;

    ccont |= (cc->mask2 & (CC_CCCONT | CC_DEFCONT | CC_PATCONT));

    if (incompfunc != 1 && findnode(ccstack, cc))
	return;

    MUSTUSEHEAP("complistflags");

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
    rpre = rsuf = lpre = lsuf = ppre = psuf = lppre = lpsuf =
	fpre = fsuf = ipre = ripre = prpre = 
	qfpre = qfsuf = qrpre = qrsuf = qlpre = qlsuf = NULL;

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
	expl = (Cexpl) zhalloc(sizeof(struct cexpl));
	expl->count = expl->fcount = 0;
    } else
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
    } else
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
	    char *dp = rembslash(cc->prefix);
	    /* First find out how much of the prefix is already on the line. */
	    sd = dupstring(s);
	    untokenize(sd);
	    pl = pfxlen(dp, sd);
	    s += pl;
	    sd += pl;
	    offs -= pl;
	}
    }
    /* Does this compctl have a suffix (compctl -S)? */
    if (cc->suffix) {
	char *sdup = rembslash(cc->suffix);
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
    if (incompfunc || ((ic = *s) != Tilde && ic != Equals))
	ic = 0;

    /* Check if we have to complete a parameter name... */
    if (!incompfunc && (p = check_param(s, 1, 0))) {
	s = p;
	/* And now make sure that we complete parameter names. */
	cc = &cc_dummy;
	cc_dummy.refc = 10000;
	cc_dummy.mask = CC_PARAMS | CC_ENVVARS;
    }
    ooffs = offs;
    /* If we have to ignore the word, do that. */
    if (cc->mask & CC_DELETE) {
	delit = 1;
	*s = '\0';
	offs = 0;
	if (isset(AUTOMENU))
	    usemenu = 1;
    }

    /* Compute line prefix/suffix. */
    lpl = offs;
    lpre = zhalloc(lpl + 1);
    memcpy(lpre, s, lpl);
    lpre[lpl] = '\0';
    qlpre = quotename(lpre, NULL);
    lsuf = dupstring(s + offs);
    lsl = strlen(lsuf);
    qlsuf = quotename(lsuf, NULL);

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
    qrpre = quotename(rpre, NULL);

    for (p = lsuf; *p && *p != String && *p != Tick; p++);
    rsuf = *p ? (noreal = 0, getreal(lsuf)) : dupstring(lsuf);
    qrsuf = quotename(rsuf, NULL);

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
    rsl = strlen(rsuf);
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
    if (!comppatmatch || !*comppatmatch)
	ispattern = 0;

    if (ispattern) {
	/* The word should be treated as a pattern, so compute the matcher. */
	p = (char *) zhalloc(rpl + rsl + 2);
	strcpy(p, rpre);
	if (rpl && p[rpl - 1] != Star &&
	    (!comppatmatch || *comppatmatch == '*')) {
	    p[rpl] = Star;
	    strcpy(p + rpl + 1, rsuf);
	} else
	    strcpy(p + rpl, rsuf);
	patcomp = parsereg(p);
	haspattern = 1;
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
	    lppre = dupstring((char *) line + wb +
			      (qipre && *qipre ?
			       (strlen(qipre) -
				(*qipre == '\'' || *qipre == '\"')) : 0));
	    line[cs] = save;
	    if (brbeg && *brbeg)
		strcpy(lppre + qbrpl, lppre + qbrpl + strlen(brbeg));
	    if ((p = strrchr(lppre, '/'))) {
		p[1] = '\0';
		lppl = strlen(lppre);
	    } else if (!sf1) {
		lppre = NULL;
		lppl = 0;
	    } else {
		lppre = ppre;
		lppl = strlen(lppre);
	    }
	} else {
	    lppre = NULL;
	    lppl = 0;
	}
	if (cs != we) {
	    int end = we;
	    char save = line[end];

	    if (qisuf && *qisuf) {
		int ql = strlen(qisuf);

		end -= ql - (qisuf[ql-1] == '\'' || qisuf[ql-1] == '"');
	    }
	    line[end] = 0;
	    lpsuf = dupstring((char *) (line + cs));
	    line[end] = save;
	    if (brend && *brend) {
		char *p = lpsuf + qbrsl - (cs - wb);

		strcpy(p, p + strlen(brend));
	    }
	    if (!(lpsuf = strchr(lpsuf, '/')) && sf2)
		lpsuf = psuf;
	    lpsl = (lpsuf ? strlen(lpsuf) : 0);
	} else {
	    lpsuf = NULL;
	    lpsl = 0;
	}

	/* And get the file prefix. */
	fpre = dupstring(((s1 == s || s1 == rpre || ic) &&
			  (*s != '/' || cs == wb)) ? s1 : s1 + 1);
	qfpre = quotename(fpre, NULL);
	/* And the suffix. */
	fsuf = dupstrpfx(rsuf, s2 - rsuf);
	qfsuf = quotename(fsuf, NULL);

	if (comppatmatch && *comppatmatch && (ispattern & 2)) {
	    int t2;

	    /* We have to use globbing, so compute the pattern from *
	     * the file prefix and suffix with a `*' between them.  */
	    p = (char *) zhalloc((t2 = strlen(fpre)) + strlen(fsuf) + 2);
	    strcpy(p, fpre);
	    if ((!t2 || p[t2 - 1] != Star) && *fsuf != Star &&
		(!comppatmatch || *comppatmatch == '*'))
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
		p = (char *) zhalloc(lpl + lsl + 3);
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
			    (char**) zhalloc(sizeof(char *)*(arrlen(pp)+1));
			while (*pp) {
			    pl = strlen(*pp);
			    tp = (char *) zhalloc(strlen(*pp) + tl);
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

			ta[0] = tp = (char *) zhalloc(strlen(ppre) + pl + 2);
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
			    int ns, pl = strlen(prpre), o, paalloc;
			    char *g = dupstring(cc->glob), *pa;
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
			    o = strlen(prpre);
			    pa = (char *)zalloc(paalloc = o + PATH_MAX);
			    strcpy(pa, prpre);
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
				    int minlen = o + strlen(g);
				    if (minlen >= paalloc)
					pa = (char *)
					    zrealloc(pa, paalloc = minlen+1);
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

			    zfree(pa, paalloc);
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
	qrpre = dyncat((ic == Tilde) ? "~" : "=", qrpre);
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
    oaw = addwhat = (cc->mask & CC_QUOTEFLAG) ? -2 : CC_QUOTEFLAG;

    if (cc->mask & CC_NAMED)
	/* Add named directories. */
	dumphashtable(nameddirtab, addwhat);
    if (cc->mask & CC_OPTIONS)
	/* Add option names. */
	dumphashtable(optiontab, addwhat);
    if (cc->mask & CC_VARS) {
	/* And parameter names. */
	dumphashtable(paramtab, -9);
	addwhat = oaw;
    }
    if (cc->mask & CC_BINDINGS) {
	/* And zle function names... */
	dumphashtable(thingytab, CC_BINDINGS);
	addwhat = oaw;
    }
    if (cc->keyvar) {
	/* This adds things given to the compctl -k flag *
	 * (from a parameter or a list of words).        */
	char **usr = get_user_var(cc->keyvar);

	if (usr)
	    while (*usr)
		addmatch(*usr++, NULL);
    }
    if (cc->mask & CC_USERS) {
	/* Add user names. */
	maketildelist();
	addwhat = oaw;
    }
    if (cc->widget) {
	char **ocfa = cfargs;
	int ocfr = cfret;

	cfargs = zlenoargs;
	callcompfunc(os, cc->widget);
	cfargs = ocfa;
	cfret = ocfr;
    }
    if (cc->func) {
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
    if (cc->mask & (CC_JOBS | CC_RUNNING | CC_STOPPED)) {
	/* Get job names. */
	int i;
	char *j;

	for (i = 0; i < MAXJOB; i++)
	    if ((jobtab[i].stat & STAT_INUSE) &&
		jobtab[i].procs && jobtab[i].procs->text) {
		int stopped = jobtab[i].stat & STAT_STOPPED;

		j = dupstring(jobtab[i].procs->text);
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

	/* Put the string in the lexer buffer and call the lexer to *
	 * get the words we have to expand.                        */
	zleparse = 1;
	lexsave();
	tmpbuf = (char *)zhalloc(strlen(cc->str) + 5);
	sprintf(tmpbuf, "foo %s", cc->str); /* KLUDGE! */
	inpush(tmpbuf, 0, NULL);
	strinbeg(0);
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
	int i = addhistnum(curhist,-1,HIST_FOREIGN), n = cc->hnum;

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
    if ((t = cc->mask & CC_EXTCMDS)) {
	/* Add external commands */
	if (isset(HASHLISTALL))
	    cmdnamtab->filltable(cmdnamtab);
	dumphashtable(cmdnamtab, t | (cc->mask & (CC_DISCMDS|CC_EXCMDS)));
    }
    if ((t = cc->mask & CC_RESWDS))
	/* Add reserved words */
	dumphashtable(reswdtab, t | (cc->mask & (CC_DISCMDS|CC_EXCMDS)));
    if ((t = cc->mask & (CC_ALREG | CC_ALGLOB)))
	/* Add the two types of aliases. */
	dumphashtable(aliastab, t | (cc->mask & (CC_DISCMDS|CC_EXCMDS)));
    if (keypm && cc == &cc_dummy) {
	/* Add the keys of the parameter in keypm. */
	scanhashtable(keypm->gets.hfn(keypm), 0, 0, PM_UNSET, addhnmatch, 0);
	keypm = NULL;
	cc_dummy.suffix = NULL;
    }
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
		    char *p = (char *) zhalloc(strlen(m->ppre) + strlen(m->str) +
					      strlen(m->psuf) + 1);

		    sprintf(p, "%s%s%s", m->ppre, m->str, m->psuf);
		    addlinknode(args, dupstring(p));
		} else
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
    } else if ((tt = cc->explain)) {
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
	int oldn = clwnum, oldp = clwpos, br;
	unsigned long occ = ccont;
	
	ccont = CC_CCCONT;
	
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
	br = brange;

	if (cc->subcmd[0]) {
	    /* And probably put the command name given to the flag *
	     * in the array.                                       */
	    clwpos++;
	    clwnum++;
	    incmd = 0;
	    ops = clwords[br - 1];
	    clwords[br - 1] = ztrdup(cc->subcmd);
	    cmdstr = ztrdup(cc->subcmd);
	    clwords += br - 1;
	} else {
	    cmdstr = ztrdup(clwords[br]);
	    incmd = !clwpos;
	    clwords += br;
	}
	/* Produce the matches. */
	makecomplistcmd(s, incmd, CFN_FIRST);

	/* And restore the things we changed. */
	clwords = ow;
	zsfree(cmdstr);
	cmdstr = os;
	clwnum = oldn;
	clwpos = oldp;
	if (ops) {
	    zsfree(clwords[br - 1]);
	    clwords[br - 1] = ops;
	}
	ccont = occ;
    }
    if (cc->substr)
	sep_comp_string(cc->substr, s, offs, 1);
    uremnode(ccstack, firstnode(ccstack));
    if (cc->matcher)
	mstack = mstack->next;
}

/* Invalidate the completion list. */

/**/
void
invalidatelist(void)
{
    if (showinglist == -2)
	listmatches();
    if (validlist)
	freematches();
    lastambig = menucmp = menuacc = validlist = showinglist = fromcomp = 0;
    if (listshown < 0)
	listshown = 0;
    minfo.cur = NULL;
    minfo.asked = 0;
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
		if (*ptr == Meta)
		    ptr++;
	    }
	    if (brk)
		break;
	}
	if (!brk || !count)
	    return NULL;
	*ptr = '\0';
	aptr = uarr = (char **) zhalloc(sizeof(char *) * (count + 1));

	while ((*aptr++ = (char *)ugetnode(arrlist)));
	uarr[count] = NULL;
	return uarr;
    } else {
	/* Otherwise it should be a parameter name. */
	char **arr = NULL, *val;

	if ((arr = getaparam(nam)) || (arr = gethparam(nam)))
	    return (incompfunc ? arrdup(arr) : arr);

	if ((val = getsparam(nam))) {
	    arr = (char **) zhalloc(2*sizeof(char *));
	    arr[0] = (incompfunc ? dupstring(val) : val);
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
    rp = ap = (Cmatch *) ncalloc(((n = countlinknodes(l)) + 1) *
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
    } else if (s) {
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
    } else
	for (ap = rp; *ap; ap++)
	    if ((*ap)->flags & CMF_NOLIST)
		nl++;
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
    mgroup = (Cmgroup) zhalloc(sizeof(struct cmgroup));
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
    r->isuf = ztrdup(m->isuf);
    r->ppre = ztrdup(m->ppre);
    r->psuf = ztrdup(m->psuf);
    r->prpre = ztrdup(m->prpre);
    r->pre = ztrdup(m->pre);
    r->suf = ztrdup(m->suf);
    r->flags = m->flags;
    r->brpl = m->brpl;
    r->brsl = m->brsl;
    r->rems = ztrdup(m->rems);
    r->remf = ztrdup(m->remf);
    r->autoq = m->autoq;
    r->qipl = m->qipl;
    r->qisl = m->qisl;

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
    int nn, nl, fi = 0, gn = 1, mn = 1, rn;

    if (hasperm)
	freematches();

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
	n->num = gn++;

	n->flags = g->flags;
	n->mcount = g->mcount;
	n->matches = p = (Cmatch *) ncalloc((n->mcount + 1) *
					    sizeof(Cmatch));
	for (rn = 1, q = g->matches; *q; q++, p++, rn) {
	    *p = dupmatch(*q);
	    (*p)->rnum = rn++;
	    (*p)->gnum = mn++;
	}
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
	} else
	    n->expls = NULL;

	if ((n->ccount = g->ccount)) {
	    n->ccs = cp = (Compctl *) ncalloc((n->ccount + 1) *
					      sizeof(Compctl));
	    for (cq = g->ccs; *cq; cq++, cp++)
		*cp = *cq;
	    *cp = NULL;
	} else
	    n->ccs = NULL;
	g = g->next;
    }
    pmatches = amatches;
    hasperm = 1;
    permmnum = mn - 1;
    permgnum = gn - 1;
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
    zsfree(m->isuf);
    zsfree(m->ppre);
    zsfree(m->psuf);
    zsfree(m->pre);
    zsfree(m->suf);
    zsfree(m->prpre);
    zsfree(m->rems);
    zsfree(m->remf);

    zfree(m, sizeof(m));
}

/* This frees the groups of matches. */

/**/
void
freematches(void)
{
    Cmgroup g = pmatches, n;
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
    hasperm = 0;
}

/* Insert the given string into the command line.  If move is non-zero, *
 * the cursor position is changed and len is the length of the string   *
 * to insert (if it is -1, the length is calculated here).              *
 * The last argument says if we should quote the string.                */

/**/
static int
inststrlen(char *str, int move, int len)
{
    if (!len || !str)
	return 0;
    if (len == -1)
	len = strlen(str);
    spaceinline(len);
    strncpy((char *)(line + cs), str, len);
    if (move)
	cs += len;
    return len;
}

/* This builds the unambiguous string. If ins is non-zero, it is
 * immediatly inserted in the line. Otherwise csp is used to return
 * the relative cursor position in the string returned. */

static char *
cline_str(Cline l, int ins, int *csp)
{
    Cline s;
    int ocs = cs, ncs, pcs, pm, sm, d, b, i, j, li = 0;
    int pl, sl, hasp, hass, ppos, spos, plen, slen;

    ppos = spos = plen = slen = hasp = hass = 0;
    pm = sm = d = b = pl = sl = -1;

    /* Get the information about the brace beginning and end we have
     * to re-insert. */
    if (ins) {
	if ((hasp = (brbeg && *brbeg))) {
	    plen = strlen(brbeg); pl = (hasunqu ? brpl : qbrpl);
	}
	if ((hass = (brend && *brend))) {
	    slen = strlen(brend);
	    sl = we - wb - (hasunqu ? brsl : qbrsl) - plen - slen + 1;
	}
	if (!pl) {
	    inststrlen(brbeg, 1, -1);
	    pl = -1; hasp = 0;
	}
	if (!sl) {
	    inststrlen(brend, 1, -1);
	    sl = -1; hass = 0;
	}
    }
    /* Walk through the top-level cline list. */
    while (l) {
	if (pl >= 0)
	    ppos = -1;
	if (sl >= 0)
	    spos = -1;
	/* Insert the original string if no prefix. */
	if (l->olen && !(l->flags & CLF_SUF) && !l->prefix) {
	    inststrlen(l->orig, 1, l->olen);
	    if (ins) {
		li += l->olen;
		if (pl >= 0 && li >= pl) {
		    ppos = cs - (li - pl); pl = -1;
		}
		if (sl >= 0 && li >= sl) {
		    spos = cs - (li - sl) - 1; sl = -1;
		}
	    }
	} else {
	    /* Otherwise insert the prefix. */
	    for (s = l->prefix; s; s = s->next) {
		pcs = cs;
		if (s->flags & CLF_LINE)
		    inststrlen(s->line, 1, s->llen);
		else
		    inststrlen(s->word, 1, s->wlen);
		if (d < 0 && (s->flags & CLF_DIFF))
		    d = cs;
		if (ins) {
		    li += s->llen;
		    if (pl >= 0 && li >= pl) {
			ppos = pcs + s->llen - (li - pl); pl = -1;
		    }
		    if (sl >= 0 && li >= sl) {
			spos = pcs + s->llen - (li - sl) - 1; sl = -1;
		    }
		}
	    }
	}
	/* Remember the position if this is the first prefix with
	 * missing characters. */
	if (pm < 0 && (l->flags & CLF_MISS) && !(l->flags & CLF_SUF))
	    pm = cs;
	pcs = cs;
	/* Insert the anchor. */
	if (l->flags & CLF_LINE)
	    inststrlen(l->line, 1, l->llen);
	else
	    inststrlen(l->word, 1, l->wlen);
	if (ins) {
	    li += l->llen;
	    if (pl >= 0 && li >= pl) {
		ppos = pcs + l->llen - (li - pl); pl = -1;
	    }
	    if (sl >= 0 && li >= sl) {
		spos = pcs + l->llen - (li - sl) - 1; sl = -1;
	    }
	}
	/* Remember the cursor position for suffixes and mids. */
	if (l->flags & CLF_MISS) {
	    if (l->flags & CLF_MID)
		b = cs;
	    else if (sm < 0 && (l->flags & CLF_SUF))
		sm = cs;
	}
	/* And now insert the suffix or the original string. */
	if (l->olen && (l->flags & CLF_SUF) && !l->suffix) {
	    pcs = cs;
	    inststrlen(l->orig, 1, l->olen);
	    if (ins) {
		li += l->olen;
		if (pl >= 0 && li >= pl) {
		    ppos = pcs + l->olen - (li - pl); pl = -1;
		}
		if (sl >= 0 && li >= sl) {
		    spos = pcs + l->olen - (li - sl) - 1; sl = -1;
		}
	    }
	} else {
	    int hp = 0, hs = 0;

	    for (j = -1, i = 0, s = l->suffix; s; s = s->next) {
		if (j < 0 && (s->flags & CLF_DIFF))
		    j = i;
		if (s->flags & CLF_LINE) {
		    inststrlen(s->line, 0, s->llen);
		    i += s->llen; pcs = cs + s->llen;
		} else {
		    inststrlen(s->word, 0, s->wlen);
		    i += s->wlen; pcs = cs + s->wlen;
		}
		if (ins) {
		    li += s->llen;
		    if (pl >= 0 && li >= pl) {
			hp = 1; ppos = pcs - (li - pl) - i; pl = -1;
		    }
		    if (sl >= 0 && li >= sl) {
			hs = 1; spos = pcs - (li - sl) - i; sl = -1;
		    }
		}
	    }
	    if (hp)
		ppos += i;
	    if (hs)
		spos += i;
	    cs += i;
	    if (d < 0 && j >= 0)
		d = cs - j;
	}
	/* If we reached the right positions, re-insert the braces. */
	if (ins) {
	    if (hasp && ppos >= 0) {
		i = cs;
		cs = ppos;
		inststrlen(brbeg, 1, plen);
		cs = i + plen;
		hasp = 0;
	    }
	    if (hass && spos >= 0) {
		i = cs;
		cs = spos;
		inststrlen(brend, 1, slen);
		cs = i + slen;
		hass = 0;
	    }
	}
	l = l->next;
    }
    if (pl >= 0)
	inststrlen(brbeg, 1, plen);
    if (sl >= 0)
	inststrlen(brend, 1, slen);

    /* This calculates the new cursor position. If we had a mid cline
     * with missing characters, we take this, otherwise if we have a
     * prefix with missing characters, we take that, the same for a
     * suffix, and finally a place where the matches differ. */
    ncs = (b >= 0 ? b : (pm >= 0 ? pm : (sm >= 0 ? sm : (d >= 0 ? d : cs))));

    if (!ins) {
	/* We always inserted the string in the line. If that was not
	 * requested, we copy it and remove from the line. */
	char *r = zalloc((i = cs - ocs) + 1);

	memcpy(r, (char *) (line + ocs), i);
	r[i] = '\0';
	cs = ocs;
	foredel(i);

	*csp = ncs - ocs;

	return r;
    }
    if (ncs >= ppos)
	ncs += plen;
    if (ncs > spos)
	ncs += slen;

    lastend = cs;
    cs = ncs;

    return NULL;
}

/* This is a utility function using the function above to allow access
 * to the unambiguous string and cursor position via compstate. */

/**/
char *
unambig_data(int *cp)
{
    static char *scache = NULL;
    static int ccache;

    if (mnum && ainfo) {
	if (mnum != unambig_mnum) {
	    zsfree(scache);
	    scache = cline_str((ainfo->count ? ainfo->line : fainfo->line),
			       0, &ccache);
	}
    } else {
	zsfree(scache);
	scache = ztrdup("");
	ccache = 0;
    }
    unambig_mnum = mnum;
    if (cp)
	*cp = ccache + 1;

    return scache;
}

/* Insert the given match. This returns the number of characters inserted.
 * scs is used to return the position where a automatically created suffix
 * has to be inserted. */

/**/
static int
instmatch(Cmatch m, int *scs)
{
    int l, r = 0, ocs, a = cs;

    /* Ignored prefix. */
    if (m->ipre) {
	char *p = m->ipre + (menuacc ? m->qipl : 0);

	inststrlen(p, 1, (l = strlen(p)));
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
    *scs = cs;
    if (m->suf) {
	inststrlen(m->suf, 1, (l = strlen(m->suf)));
	r += l;
    }
    /* ignored suffix */
    if (m->isuf) {
	inststrlen(m->isuf, 1, (l = strlen(m->isuf)));
	r += l;
    }
    lastend = cs;
    cs = ocs;

    return r;
}

/* Handle the case were we found more than one match. */

/**/
static int
do_ambiguous(void)
{
    int ret = 0;

    menucmp = menuacc = 0;

    /* If we have to insert the first match, call do_single().  This is *
     * how REC_EXACT takes effect.  We effectively turn the ambiguous   *
     * completion into an unambiguous one.                              */
    if (ainfo && ainfo->exact == 1 && useexact && !(fromcomp & FC_LINE)) {
	minfo.cur = NULL;
	do_single(ainfo->exactm);
	invalidatelist();
	return ret;
    }
    /* Setting lastambig here means that the completion is ambiguous and *
     * AUTO_MENU might want to start a menu completion next time round,  *
     * but this might be overridden below if we can complete an          *
     * unambiguous prefix.                                               */
    lastambig = 1;

    if (usemenu || (haspattern && comppatinsert &&
		    !strcmp(comppatinsert, "menu"))) {
	/* We are in a position to start using menu completion due to one  *
	 * of the menu completion options, or due to the menu-complete-    *
	 * word command, or due to using GLOB_COMPLETE which does menu-    *
	 * style completion regardless of the setting of the normal menu   *
	 * completion options.                                             */
	do_ambig_menu();
    } else if (ainfo) {
	int atend = (cs == we), oll = ll, la, eq, tcs;
	VARARR(char, oline, ll);

	minfo.cur = NULL;
	minfo.asked = 0;

	/* Copy the line buffer to be able to easily test if it changed. */
	memcpy(oline, line, ll);

	fixsuffix();

	/* First remove the old string from the line. */
	cs = wb;
	foredel(we - wb);

	/* Now get the unambiguous string and insert it into the line. */
	cline_str(ainfo->line, 1, NULL);
	if (eparq) {
	    tcs = cs;
	    cs = lastend;
	    for (eq = eparq; eq; eq--)
		inststrlen("\"", 0, 1);
	    cs = tcs;
	}
	/* la is non-zero if listambiguous may be used. Copying and
	 * comparing the line looks like BFI but it is the easiest
	 * solution. Really. */
	la = (ll != oll || strncmp(oline, (char *) line, ll));

	/* If REC_EXACT and AUTO_MENU are set and what we inserted is an  *
	 * exact match, we want menu completion the next time round       *
	 * so we set fromcomp, to ensure that the word on the line is not *
	 * taken as an exact match. Also we remember if we just moved the *
	 * cursor into the word.                                          */
	fromcomp = ((isset(AUTOMENU) ? FC_LINE : 0) |
		    ((atend && cs != lastend) ? FC_INWORD : 0));

	/* Probably move the cursor to the end. */
	if (movetoend == 3)
	    cs = lastend;

	/* If the LIST_AMBIGUOUS option (meaning roughly `show a list only *
	 * if the completion is completely ambiguous') is set, and some    *
	 * prefix was inserted, return now, bypassing the list-displaying  *
	 * code.  On the way, invalidate the list and note that we don't   *
	 * want to enter an AUTO_MENU imediately.                          */
	if (uselist == 3 && la) {
	    int fc = fromcomp;

	    invalidatelist();
	    fromcomp = fc;
	    lastambig = 0;
	    clearlist = 1;
	    return ret;
	}
    } else
	return ret;

    /* At this point, we might want a completion listing.  Show the listing *
     * if it is needed.                                                     */
    if (isset(LISTBEEP))
	ret = 1;

    if (uselist && (usemenu != 2 || (!listshown && !oldlist)) &&
	((!showinglist && (!listshown || !oldlist)) ||
	 (usemenu == 3 && !oldlist)) &&
	(smatches >= 2 || (compforcelist && *compforcelist)))
	showinglist = -2;

    return ret;
}

/* This is a stat that ignores backslashes in the filename.  The `ls' *
 * parameter says if we have to do lstat() or stat().  I think this   *
 * should instead be done by use of a general function to expand a    *
 * filename (stripping backslashes), combined with the actual         *
 * (l)stat().                                                         */

/**/
int
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
void
do_single(Cmatch m)
{
    int l, sr = 0, scs;
    int havesuff = 0;
    char *str = m->str, *ppre = m->ppre, *psuf = m->psuf, *prpre = m->prpre;

    if (!prpre) prpre = "";
    if (!ppre) ppre = "";
    if (!psuf) psuf = "";

    fixsuffix();

    if (!minfo.cur) {
	/* We are currently not in a menu-completion, *
	 * so set the position variables.             */
	minfo.pos = wb;
	minfo.we = (movetoend >= 2 || (movetoend == 1 && !menucmp));
	minfo.end = we;
    }
    /* If we are already in a menu-completion or if we have done a *
     * glob completion, we have to delete some of the stuff on the *
     * command line.                                               */
    if (minfo.cur)
	l = minfo.len + minfo.insc;
    else
	l = we - wb;

    minfo.insc = 0;
    cs = minfo.pos;
    foredel(l);

    /* And then we insert the new string. */
    minfo.len = instmatch(m, &scs);
    minfo.end = cs;
    cs = minfo.pos + minfo.len;

    if (m->suf) {
	havesuff = 1;
	minfo.insc = ztrlen(m->suf);
	minfo.len -= minfo.insc;
	if (minfo.we) {
	    minfo.end += minfo.insc;
	    if (m->flags & CMF_REMOVE) {
		makesuffixstr(m->remf, m->rems, minfo.insc);
		if (minfo.insc == 1)
		    suffixlen[STOUC(m->suf[0])] = 1;
	    }
	}
    } else {
	/* There is no user-specified suffix, *
	 * so generate one automagically.     */
	cs = scs;
	if (m->ripre && (m->flags & CMF_PARBR)) {
	    int pq;

	    /*{{*/
	    /* Completing a parameter in braces.  Add a removable `}' suffix. */
	    cs += eparq;
	    for (pq = parq; pq; pq--)
		inststrlen("\"", 1, 1);
	    minfo.insc += parq;
	    inststrlen("}", 1, 1);
	    minfo.insc++;
	    if (minfo.we)
		minfo.end += minfo.insc;
	}
	if ((m->flags & CMF_FILE) || (m->ripre && isset(AUTOPARAMSLASH))) {
	    /* If we have a filename or we completed a parameter name      *
	     * and AUTO_PARAM_SLASH is set, lets see if it is a directory. *
	     * If it is, we append a slash.                                */
	    struct stat buf;
	    char *p;
	    int t = 0;

	    if (m->ipre && m->ipre[0] == '~' && !m->ipre[1])
		t = 1;
	    else {
		/* Build the path name. */
		if (m->ripre && !*psuf) {
		    int ne = noerrs;

		    p = (char *) zhalloc(strlen(m->ripre) + strlen(str) + 1);
		    sprintf(p, "%s%s", m->ripre, str);
		    noerrs = 1;
		    parsestr(p);
		    singsub(&p);
		    errflag = 0;
		    noerrs = ne;
		} else {
		    p = (char *) zhalloc(strlen(prpre) + strlen(str) +
				 strlen(psuf) + 3);
		    sprintf(p, "%s%s%s", (prpre && *prpre) ? prpre : "./", str, psuf);
		}
		/* And do the stat. */
		t = (!(sr = ztat(p, &buf, 0)) && S_ISDIR(buf.st_mode));
	    }
	    if (t) {
		/* It is a directory, so add the slash. */
		havesuff = 1;
		inststrlen("/", 1, 1);
		minfo.insc++;
		if (minfo.we)
		    minfo.end++;
		if (!menucmp || minfo.we) {
		    if (m->remf || m->rems)
			makesuffixstr(m->remf, m->rems, 1);
		    else if (isset(AUTOREMOVESLASH)) {
			makesuffix(1);
			suffixlen['/'] = 1;
		    }
		}
	    }
	}
	if (!minfo.insc)
	    cs = minfo.pos + minfo.len - m->qisl;
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
	    cs = scs;
	    havesuff = 1;
	    inststrlen(",", 1, 1);
	    minfo.insc++;
	    makesuffix(1);
	    if ((!menucmp || minfo.we) && isset(AUTOPARAMKEYS))
		suffixlen[','] = suffixlen['}'] = 1;
	}
    } else if (!havesuff && (!(m->flags & CMF_FILE) || !sr)) {
	/* If we didn't add a suffix, add a space, unless we are *
	 * doing menu completion or we are completing files and  *
	 * the string doesn't name an existing file.             */
	if (m->autoq && (!m->isuf || m->isuf[0] != m->autoq)) {
	    inststrlen(&(m->autoq), 1, 1);
	    minfo.insc++;
	}
	if (!menucmp && (usemenu != 3 || insspace)) {
	    inststrlen(" ", 1, 1);
	    minfo.insc++;
	    if (minfo.we)
		makesuffix(1);
	}
    }
    if (minfo.we && m->ripre && isset(AUTOPARAMKEYS))
	makeparamsuffix(((m->flags & CMF_PARBR) ? 1 : 0), minfo.insc - parq);

    if ((menucmp && !minfo.we) || !movetoend) {
	cs = minfo.end;
	if (cs + m->qisl == lastend)
	    cs += minfo.insc;
    }
    {
	Cmatch *om = minfo.cur;
	struct chdata dat;

	dat.matches = amatches;
	dat.num = nmatches;
	dat.cur = m;

	if (menucmp)
	    minfo.cur = &m;
	runhookdef(INSERTMATCHHOOK, (void *) &dat);
	minfo.cur = om;
    }
}

/* This maps the value in v into the range [0,m-1], decrementing v
 * if it is non-negative and making negative values count backwards. */

static int
comp_mod(int v, int m)
{
    if (v >= 0)
	v--;
    if (v >= 0)
	return v % m;
    else {
	while (v < 0)
	    v += m;
	return v;
    }
}

/* This handles the beginning of menu-completion. */

/**/
static void
do_ambig_menu(void)
{
    Cmatch *mc;

    if (usemenu != 3) {
	menucmp = 1;
	menuacc = 0;
	minfo.cur = NULL;
    } else {
	if (oldlist) {
	    if (oldins)
		acceptlast();
	} else
	    minfo.cur = NULL;
    }
    if (insgroup) {
	insgnum = comp_mod(insgnum, permgnum);
	for (minfo.group = amatches;
	     minfo.group && (minfo.group)->num != insgnum + 1;
	     minfo.group = (minfo.group)->next);
	if (!minfo.group || !(minfo.group)->mcount) {
	    minfo.cur = NULL;
	    minfo.asked = 0;
	    return;
	}
	insmnum = comp_mod(insmnum, (minfo.group)->mcount);
    } else {
	int c = 0;

	insmnum = comp_mod(insmnum, permmnum);
	for (minfo.group = amatches;
	     minfo.group && (c += (minfo.group)->mcount) <= insmnum;
	     minfo.group = (minfo.group)->next)
	    insmnum -= (minfo.group)->mcount;
	if (!minfo.group) {
	    minfo.cur = NULL;
	    minfo.asked = 0;
	    return;
	}
    }
    mc = (minfo.group)->matches + insmnum;
    do_single(*mc);
    minfo.cur = mc;
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

#if 0
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
#endif

/* This is used to print the explanation string. *
 * It returns the number of lines printed.       */

/**/
int
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
		case '{':
		    for (p++; *p && (*p != '%' || p[1] != '}'); p++, cc++)
			if (dopr)
			    putc(*p, shout);
		    if (*p)
			p++;
		    else
			p--;
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

/**/
Cmatch *
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
    struct chdata dat;

#ifdef DEBUG
    /* Sanity check */
    if (!validlist) {
	showmsg("BUG: listmatches called with bogus list");
	return;
    }
#endif

    dat.matches = amatches;
    dat.num = nmatches;
    dat.cur = NULL;
    runhookdef(LISTMATCHESHOOK, (void *) &dat);
}

/**/
int
ilistmatches(Hookdef dummy, Chdata dat)
{
    Cmgroup g;
    Cmatch *p, m;
    Cexpl *e;
    int nlines = 0, ncols, nlist = 0, longest = 1, pnl = 0;
    int of = isset(LISTTYPES), opl = 0;

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
	    } else {
		while (*pp) {
		    if ((l = strlen(*pp)) > longest)
			longest = l;
		    nlist++;
		    pp++;
		}
	    }
	} else {
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
    if (!nlines) {
	showinglist = listshown = 0;
	return 1;
    }
    /* Set the cursor below the prompt. */
    trashzle();
    showinglist = listshown = 0;

    clearflag = (isset(USEZLE) && !termflags &&
		 complastprompt && *complastprompt);

    /* Maybe we have to ask if the user wants to see the list. */
    if ((!minfo.cur || !minfo.asked) &&
	((complistmax && nlist > complistmax) ||
	 (!complistmax && nlines >= lines))) {
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
	    if (minfo.cur)
		minfo.asked = 2;
	    return 0;
	}
	if (clearflag) {
	    putc('\r', shout);
	    tcmultout(TCUP, TCMULTUP, qup);
	    if (tccan(TCCLEAREOD))
		tcout(TCCLEAREOD);
	} else
	    putc('\n', shout);
	settyinfo(&shttyinfo);
	if (minfo.cur)
	    minfo.asked = 1;
    }

    /* Now print the matches. */
    g = amatches;
    while (g) {
	char **pp = g->ylist;

	if ((e = g->expls)) {
	    while (*e) {
		if ((*e)->count) {
		    if (pnl) {
			putc('\n', shout);
			pnl = 0;
		    }
		    printfmt((*e)->str, (*e)->count, 1);
		    pnl = 1;
		}
		e++;
	    }
	}
	if (pp && *pp) {
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
	    } else {
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
	} else if (g->lcount) {
	    int n = g->lcount, nl = (n + ncols - 1) / ncols, nc = nl, i, j, a = 0;
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

			pb = (char *) zhalloc((m->prpre ? strlen(m->prpre) : 0) +
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
    } else
	putc('\n', shout);
    listshown = (clearflag ? 1 : -1);

    return 0;
}

/* This is used to print expansions. */

/**/
int
listlist(LinkList l)
{
    struct cmgroup dg;
    int vl = validlist, sm = smatches;
    char *oclp = complastprompt;
    Cmgroup am = amatches;

    if (listshown)
	showagain = 1;

    complastprompt = ((zmult == 1) == !!isset(ALWAYSLASTPROMPT) ? "yes" : NULL);
    smatches = 1;
    validlist = 1;
    memset(&dg, 0, sizeof(struct cmgroup));
    dg.ylist = (char **) makearray(l, 1, &(dg.lcount), NULL);
    amatches = &dg;
    ilistmatches(NULL, NULL);
    amatches = am;

    validlist = vl;
    smatches = sm;
    complastprompt = oclp;

    return !dg.lcount;
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
	strinbeg(1);
	noaliases = 1;
	noerrs = 1;
	exlast = inbufct;
	do {
	    ctxtlex();
	} while (tok != ENDINPUT && tok != LEXERR);
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
int
magicspace(char **args)
{
    int ret;
    c = ' ';
    if (!(ret = selfinsert(args)))
	doexpandhist();
    return ret;
}

/**/
int
expandhistory(char **args)
{
    if (!doexpandhist())
	return 1;
    return 0;
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
	strinbeg(1);
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
int
processcmd(char **args)
{
    char *s;
    int m = zmult;

    s = getcurcmd();
    if (!s)
	return 1;
    zmult = 1;
    pushline(zlenoargs);
    zmult = m;
    inststr(bindk->nam);
    inststr(" ");
    untokenize(s);
    HEAPALLOC {
	inststr(quotename(s, NULL));
    } LASTALLOC;
    zsfree(s);
    done = 1;
    return 0;
}

/**/
int
expandcmdpath(char **args)
{
    int oldcs = cs, na = noaliases;
    char *s, *str;

    noaliases = 1;
    s = getcurcmd();
    noaliases = na;
    if (!s || cmdwb < 0 || cmdwe < cmdwb)
	return 1;
    str = findcmd(s, 1);
    zsfree(s);
    if (!str)
	return 1;
    cs = cmdwb;
    foredel(cmdwe - cmdwb);
    spaceinline(strlen(str));
    strncpy((char *)line + cs, str, strlen(str));
    cs = oldcs;
    if (cs >= cmdwe - 1)
	cs += cmdwe - cmdwb + strlen(str);
    if (cs > ll)
	cs = ll;
    return 0;
}

/* Extra function added by AR Iano-Fletcher. */
/* This is a expand/complete in the vein of wash. */

/**/
int
expandorcompleteprefix(char **args)
{
    int ret;

    comppref = 1;
    ret = expandorcomplete(args);
    comppref = 0;
    return ret;
}
