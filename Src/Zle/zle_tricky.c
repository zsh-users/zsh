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

/* A pointer to the current position in the menu-completion array (the one *
 * that was put in the command line last).                                 */

static char **menucur;

/* menupos is the point (in the command line) where the menu-completion   *
 * strings are inserted.  menulen is the length of the string that was    *
 * inserted last.  menuend is the end position of this string in the      *
 * command line.  menuwe is non-zero if the cursor was at the end of the  *
 * word (meaning that suffixes should go before the cursor).  menuinsc is *
 * the length of any suffix that has been temporarily added.              */

static int menupos, menulen, menuend, menuwe, menuinsc;

/* This is used as a flag from get_comp_string() that we are doing *
 * completion inside a brace expansion.                            */

static int complinbrace;

/* The list of matches.  fmatches contains the matches we first ignore *
 * because of fignore.                                                 */

static LinkList matches, fmatches;

/* The list of matches turned into an array.  This is used to sort this *
 * list and when menu-completion is used (directly or via automenu).    */

static char **amatches;

/* The number of matches. */

static int nmatches;

/* A list of user-defined explanations for the completions to be shown *
 * instead of amatches when listing completions.                       */

static char **aylist;

/* !=0 if we have a valid completion list. */

static int validlist;

/* This flag is non-zero if we are completing a pattern (with globcomplete) */

static int ispattern;

/* Two patterns used when doing glob-completion.  The first one is built *
 * from the whole word we are completing and the second one from that    *
 * part of the word that was identified as a possible filename.          */

static Comp patcomp, filecomp;

/* We store the following prefixes/suffixes:                             *
 * lpre/lsuf -- what's on the line                                       *
 * rpre/rsuf -- same as lpre/lsuf, but expanded                          *
 *                                                                       *
 * ... and if we are completing files, too:                              *
 * ppre/psuf -- the path prefix/suffix                                   *
 * fpre/fsuf -- prefix/suffix of the pathname component the cursor is in *
 * prpre     -- ppre in expanded form usable for opendir                 *
 *                                                                       *
 * The integer variables hold the lengths of lpre, lsuf, rpre, rsuf,     *
 * fpre, and fsuf.  noreal is non-zero if we have rpre/rsuf.             */

static char *lpre, *lsuf;
static char *rpre, *rsuf;
static char *ppre, *psuf, *prpre;
static char *fpre, *fsuf;
static int lpl, lsl, rpl, rsl, fpl, fsl;
static int noreal;

/* This is used when completing after `$' and holds the whole prefix,   *
 * used in do_single() to check whether the word expands to a directory *
 * name (in that case and if autoparamslash is set, we add a `/').      *
 * qparampre is the same but quoted. The length of it is in qparprelen. *
 * parambr is != 0 if the parameter name is in braces.                  */

static char *parampre = NULL, *qparampre = NULL;
static int qparprelen, parambr;

/* This is either zero or equal to the special character the word we are *
 * trying to complete starts with (e.g. Tilde or Equals).                */

static char ic;

/* These hold the minimum common prefix/suffix lengths (normal and for *
 * fignore ignored).                                                   */

static int ab, ae, fab, fae;

/* This variable says what we are currently adding to the list of matches. */

static int addwhat;

/* firstm hold the first match we found, shortest contains the shortest *
 * one (normal and for fignore ignored).                                */

static char *firstm, *shortest, *ffirstm, *fshortest;

/* This holds the word we are completing in quoted from. */

static char *qword;

/* This is the length of the shortest match we found (normal and for *
 * fignore ignored).                                                 */

static int shortl, fshortl;

/* This is non-zero if we are doing a menu-completion and this is not the *
 * first call (e.g. when automenu is set and menu-completion was entered  *
 * due to this). */

static int amenu;

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

#define COMP_COMPLETE 0
#define COMP_LIST_COMPLETE 1
#define COMP_SPELL 2
#define COMP_EXPAND 3
#define COMP_EXPAND_COMPLETE 4
#define COMP_LIST_EXPAND 5
#define COMP_ISEXPAND(X) ((X) >= COMP_EXPAND)

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
    char **mc = menucur;

    usemenu = isset(MENUCOMPLETE);
    useglob = isset(GLOBCOMPLETE);
    if (cs != ll)
	deletechar();
    else
	docomplete(COMP_LIST_COMPLETE);

    menucur = mc;
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
	if (menucur == amatches)
	    menucur = amatches + nmatches - 1;
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
    if (qparampre)
	inststrlen(qparampre, 1, qparprelen);
    if (lpre && !ispattern)
	inststrlen(lpre, 1, -1);
    if (lsuf && !ispattern)
	inststrlen(lsuf, 0, -1);
    menupos = cs;
    menuend = cs + (lsuf ? strlen(lsuf) : 0);
    menulen = 0;
    menuinsc = 0;
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

/* This describes some important things collected during the last *
 * completion.  Its value is zero or the inclusive OR of some of  *
 * the HAS_* things below.                                        */

static int haswhat;

/* We have a suffix to add (given with compctl -S). */

#define HAS_SUFFIX  1

/* We have filenames in the completion list. */

#define HAS_FILES   2

/* We have other things than files in the completion list.  If this is *
 * not set but HAS_FILES is, we probably put the file type characters  *
 * in the completion list (if listtypes is set) and we attempt to add  *
 * a slash to completed directories.                                   */

#define HAS_MISC    4

/* This is set if we have filenames in the completion list that were *
 * generated by a globcompletion pattern.                            */

#define HAS_PATHPAT 8


/* This holds the naem of the current command (used to find the right *
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
		if (cmdnamtab->getnode(cmdnamtab, q) || hashcmd(q, pathchecked))
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
		    if (itok(*p))
			if (*p != String && *p != Qstring)
			    *p = ztokens[*p - Pound];
			else if (p[1] == Inbrace)
			    p++, skipparens(Inbrace, Outbrace, &p);
		docompletion(s, lst, lincmd, 1);
	    }
	} else
	    /* Just do completion. */
	    docompletion(s, lst, lincmd, 0);
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
	if (!*++menucur)
	    menucur = amatches;
	/* ... and insert it into the command line. */
	metafy_line();
	do_single(*menucur);
	unmetafy_line();
    } LASTALLOC;
}

/* 1 if we are completing in a string */
static int instring;

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

    complinbrace = 0;
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
		chuck(tt);
		offs--;
		myoffs--;

		/* Look for text up to comma before cursor and delete it */
		for (i = tt - s, p = tt; *p && i < myoffs; p++, i++)
		    if (*p == Comma)
			com = p;
		if (com) {
		    i = com - tt + 1;
		    while (i--)
			chuck(tt), offs--, myoffs--;
		}

		/* Look for text between subsequent comma
		 * and closing brace or end of string and delete it
		 */
		for (p = s + myoffs; *p && *p != Outbrace; p++)
		    if (*p == Comma) {
			while (*p && *p != Outbrace)
			    chuck(p);
			break;
		    }
		if (*p == Outbrace)
		    chuck(p);
		else {
		    /* we are still waiting for an outbrace and maybe commas */
		    complinbrace = 1;
		}
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
		docompletion(s, COMP_COMPLETE, explincmd, 0);
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

/* Quote the string s and return the result.  If e is non-zero, it the    *
 * pointer it points to may point to aposition in s and in e the position *
 * of the corresponding character in the quoted string is returned.  Like *
 * e, te may point to a position in the string and pl is used to return   *
 * the position of the character pointed to by te in the quoted string.   *
 * The string is metafied and may contain tokens.                         */

/**/
static char *
quotename(const char *s, char **e, char *te, int *pl)
{
    const char *u, *tt;
    char *v, buf[PATH_MAX * 2];
    int sf = 0;

    tt = v = buf;
    u = s;
    for (; *u; u++) {
	if (e && *e == u)
	    *e = v, sf |= 1;
	if (te == u)
	    *pl = v - tt, sf |= 2;
	if (ispecial(*u) &&
	    (!instring || (isset(BANGHIST) &&
			   *u == (char)bangchar) ||
	     (instring == 2 &&
	      (*u == '$' || *u == '`' || *u == '\"')) ||
	     (instring == 1 && *u == '\'')))
	    if (*u == '\n' || (instring == 1 && *u == '\'')) {
		if (unset(RCQUOTES)) {
		    *v++ = '\'';
		    if (*u == '\'')
			*v++ = '\\';
		    *v++ = *u;
		    *v++ = '\'';
		} else if (*u == '\n')
		    *v++ = '"', *v++ = '\n', *v++ = '"';
		else
		    *v++ = '\'', *v++ = '\'';
		continue;
	    } else
		*v++ = '\\';
	if(*u == Meta)
	    *v++ = *u++;
	*v++ = *u;
    }
    *v = '\0';
    if (strcmp(buf, s))
	tt = dupstring(buf);
    else
	tt = s;
    v += tt - buf;
    if (e && (sf & 1))
	*e += tt - buf;

    if (e && *e == u)
	*e = v;
    if (te == u)
	*pl = v - tt;

    return (char *) tt;
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
    int test = 0, sl = strlen(s), pl = rpl, cc = 0, *bp, *ep, *sp;
    char *e = NULL, *tt, *te, *fc, **fm;
    Comp cp = patcomp;
    HashNode hn;
    Param pm;
    LinkList l = matches;

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

	    if (!test)
		l = fmatches;
	}
	pl = fpl;
	if (addwhat == -5 || addwhat == -8) {
	    test = 1;
	    cp = filecomp;
	    cc = cp || ispattern;
	    e = s + sl - fsl;
	} else {
	    if ((cp = filecomp)) {
		if ((test = domatch(s, filecomp, 0)))
		    cc = 1;
	    } else {
		e = s + sl - fsl;
		if ((test = !strncmp(s, fpre, fpl)))
		    test = !strcmp(e, fsuf);
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
	    haswhat |= HAS_FILES;

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
		tt = (char *)halloc(strlen(ppre) + strlen(psuf) + sl + 1);
		strcpy(tt, ppre);
		strcat(tt, s);
		strcat(tt, psuf);
		untokenize(s = tt);
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
	if (sl >= rpl + rsl) {
	    if (cp)
		test = domatch(s, patcomp, 0);
	    else {
		e = s + sl - rsl;
		if ((test = !strncmp(s, rpre, rpl)))
		    test = !strcmp(e, rsuf);
	    }
	}
	if (!test && sl < lpl + lsl)
	    return;
	if (!test && lpre && lsuf && sl >= lpl + lsl) {
	    e = s + sl - lsl;
	    if ((test = !strncmp(s, lpre, lpl)))
		test = !strcmp(e, lsuf);
	    pl = lpl;
	}
	if (addwhat == CC_QUOTEFLAG) {
	    te = s + pl;
	    s = quotename(s, &e, te, &pl);
	    sl = strlen(s);
	}
	if (test)
	    haswhat |= HAS_MISC;
    }
    if (!test)
	return;

    if (ispattern) {
	t = s;
    } else {
	t = s += pl;
	if (*e)
	    t = s = dupstrpfx(t, e - t);
    }

    if (l == fmatches) {
	bp = &fab;
	ep = &fae;
	sp = &fshortl;
	fm = &ffirstm;
    } else {
	bp = &ab;
	ep = &ae;
	sp = &shortl;
	fm = &firstm;
    }

    if (!ispattern && *fm) {
	if ((test = pfxlen(*fm, s)) < *bp)
	    *bp = test;
	if ((test = sfxlen(*fm, s)) < *ep)
	    *ep = test;
	if (*ep > *sp - *bp)
	    *ep = *sp - *bp;
    }

    /* If we are doing a glob completion we store the whole string in *
     * the list. Otherwise only the part that fits between the prefix *
     * and the suffix is stored.                                      */
    addlinknode(l, t);
    if (!*fm) {
	*bp = *ep = 10000;
	*fm = t;
	*sp = 100000;
    }
    if (!ispattern && (sl = strlen(t)) < *sp) {
	*sp = sl;
	if (l == fmatches)
	    fshortest = t;
	else
	    shortest = t;
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

/* Copy the given string and remove backslashes from the copy and return it. */

/**/
static char *
rembslash(char *s)
{
    char *t = s = dupstring(s);

    while (*s)
	if (*s == '\\') {
	    chuck(s);
	    if (*s)
		s++;
	} else
	    s++;

    return t;
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

/* This holds a pointer to the compctl we are using. */

static Compctl ccmain;


/* Find the compctl to use and return it.  The first argument gives a *
 * compctl to start searching with (if it is zero, the hash table is  *
 * searched).  compadd is used to return a number of characters that  *
 * should be ignored at the beginning of the word and incmd is        *
 * non-zero if we are in command position.                            */

/**/
static Compctl
get_ccompctl(Compctl occ, int *compadd, int incmd)
{
    Compctl compc, ret;
    Compctlp ccp;
    int t, i, a, b, tt, ra, rb, j, isf = 1;
    Compcond or, cc;
    char *s, *ss, *sc, *cmd = dupstring(cmdstr);
    Comp comp;

   first_rec:
    *compadd = 0;
    ra = 0;
    rb = clwnum - 1;
    sc = NULL;

    if (!(ret = compc = occ)) {
      if (isf) {
        isf = 0;
        ret = &cc_first;
      }
      else if (inwhat == IN_ENV)
        /* Default completion for parameter values. */
        ret = &cc_default;
      else if (inwhat == IN_MATH) {
        /* Parameter names inside mathematical expression. */
        cc_dummy.mask = CC_PARAMS;
	    ret = &cc_dummy;
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
	    ret = &cc_dummy;
	    cc_dummy.refc = 10000;
	} else if (incmd)
	    ret = &cc_compos;
	/* And in redirections or if there is no command name (and we are *
	 * not in command position) or if no special compctl was given    *
	 * for the command: use default completion.  Note that we first   *
	 * search the complete command name and than the trailing         *
	 * pathname component.                                            */
	else if (linredir ||
 		 !(cmd &&
 		   (((ccp = (Compctlp) compctltab->getnode(compctltab, cmd)) &&
		     (compc = ret = ccp->cc)) ||
 		    ((s = dupstring(cmd)) && remlpaths(&s) &&
		     (ccp = (Compctlp) compctltab->getnode(compctltab, s)) &&
		     (compc = ret = ccp->cc)))))
	    ret = &cc_default;

	ccmain = compc = ret;
	ccmain->refc++;
    }
    /* The compctl we found has extended completion patterns, check them. */
    if (compc && compc->ext) {
	compc = compc->ext;
	/* This loops over the patterns separated by `--'. */
	for (t = 0; compc && !t; compc = compc->next) {
	    /* This loops over OR'ed patterns. */
	    for (cc = compc->cond; cc && !t; cc = or) {
		or = cc->or;
		/* This loops over AND'ed patterns. */
		for (t = 1; cc && t; cc = cc->and) {
		    /* And this loops of [...] pairs. */
		    for (t = i = 0; i < cc->n && !t; i++) {
			s = NULL;
			ra = 0;
			rb = clwnum - 1;
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
				ra = a, rb = b;
			    t = (tt >= a && tt <= b);
			    break;
			case CCT_CURSUF:
			case CCT_CURPRE:
			    s = ztrdup(clwpos < clwnum ? clwords[clwpos] : "");
			    untokenize(s);
			    sc = rembslash(cc->u.s.s[i]);
			    a = strlen(sc);
			    if (!strncmp(s, sc, a)) {
				*compadd = (cc->type == CCT_CURSUF ? a : 0);
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
				    *compadd = a, t = 1;
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
				    ra = j + 1;
				    t = 1;
				    break;
				}
				zsfree(s);
			    }
			    if (t) {
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
					rb = j - 1;
					t = clwpos <= rb;
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
	    if (t)
		break;
	}
	if (compc)
	    /* We found a matching pattern, we may return it. */
	    ret = compc;
    }
    if (ret->subcmd) {
	/* The thing we want to return has a subcmd flag (-l). */
	char **ow = clwords, *os = cmdstr, *ops = NULL;
	int oldn = clwnum, oldp = clwpos;

	/* So we restrict the words-array. */
	if (ra >= clwnum)
	    ra = clwnum - 1;
	if (ra < 1)
	    ra = 1;
	if (rb >= clwnum)
	    rb = clwnum - 1;
	if (rb < 1)
	    rb = 1;
	clwnum = rb - ra + 1;
	clwpos = clwpos - ra;

	if (ret->subcmd[0]) {
	    /* And probably put the command name given to the flag *
	     * in the array.                                       */
	    clwpos++;
	    clwnum++;
	    incmd = 0;
	    ops = clwords[ra - 1];
	    clwords[ra - 1] = cmdstr = ret->subcmd;
	    clwords += ra - 1;
	} else {
	    cmdstr = clwords[ra];
	    incmd = !clwpos;
	    clwords += ra;
	}
	*compadd = 0;
	if (ccmain != &cc_dummy)
	    freecompctl(ccmain);
	/* Then we call this function recursively. */

	ret = get_ccompctl(NULL, compadd, incmd);
	/* And restore the things we changed. */
	clwords = ow;
	cmdstr = os;
	clwnum = oldn;
	clwpos = oldp;
	if (ops)
	    clwords[ra - 1] = ops;
    }
    if (ret == &cc_first)
      goto first_rec;
    return ret;
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
	return ztrdup(peekfirst(l));
    errflag = 0;

    return ztrdup(str);
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

    addwhat = execs ? -8 : -5;
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
		if (filecomp)
		    /* If we have a pattern for the filename check, use it. */
		    test = domatch(n, filecomp, 0);
		else {
		    /* Otherwise use the prefix and suffix strings directly. */
		    e = n + strlen(n) - fsl;
		    if ((test = !strncmp(n, fpre, fpl)))
			test = !strcmp(e, fsuf);
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

/* This holds the explanation string we have to print. */

static char *expl;

/* This holds the suffix to add (given with compctl -S). */

static char *ccsuffix;

/* This s non-zero if the compctl -q flag was given (the suffix should *
 * be removed when a space or something like that is typed next).      */

static int remsuffix;

/**/
static void
quotepresuf(char **ps)
{
    if (*ps) {
	char *p = quotename(*ps, NULL, NULL, NULL);

	if (p != *ps) {
	    zsfree(*ps);
	    *ps = ztrdup(p);
	}
    }
}

/**/
static void
docompletion(char *s, int lst, int incmd, int untokenized)
{
    static int delit, compadd;

    fixsuffix();
    HEAPALLOC {
	pushheap();

	/* Make sure we have the completion list and compctl. */
	if(makecomplist(s, incmd, &delit, &compadd, untokenized)) {
	    /* Error condition: feeeeeeeeeeeeep(). */
	    feep();
	    goto compend;
	}

	if (lst == COMP_LIST_COMPLETE)
	    /* All this and the guy only wants to see the list, sigh. */
	    showinglist = -2;
	else {
	    /* We have matches. */
	    if (delit) {
		/* If we have to delete the word from the command line, *
		 * do it now.                                           */
		wb -= compadd;
		strcpy((char *)line + wb, (char *)line + we);
		we = cs = wb;
	    }
	    if (nmatches > 1)
		/* There are more than one match. */
		do_ambiguous();
	    else if (nmatches == 1) {
		/* Only one match. */
		do_single(amatches[0]);
		invalidatelist();
	    }
	}

	/* Print the explanation string if needed. */
	if (!showinglist && expl && nmatches != 1) {
	    int up;

	    if (!nmatches)
		feep();
	    trashzle();

	    clearflag = (isset(USEZLE) && !termflags &&
			 (isset(ALWAYSLASTPROMPT) && zmult == 1)) ||
			(unset(ALWAYSLASTPROMPT) && zmult != 1);

	    up = printfmt(expl, nmatches, 1);

	    if (clearflag)
		tcmultout(TCUP, TCMULTUP, up + nlnct);
	    else
		putc('\n', shout);
	    fflush(shout);
	}
      compend:
	ll = strlen((char *)line);
	if (cs > ll)
	    cs = ll;
	popheap();
    } LASTALLOC;
}

/* Create the completion list.  This is called whenever some bit of  *
 * completion code needs the list.  If the list is already available *
 * (validlist!=0), this function doesn't do anything.  Along with    *
 * the list is maintained the prefixes/suffixes etc.  When any of    *
 * this becomes invalid -- e.g. if some text is changed on the       *
 * command line -- invalidatelist() should be called, to set         *
 * validlist to zero and free up the memory used.  This function     *
 * returns non-zero on error.  delit and compadd return information  *
 * about bits of the command line that need to be deleted.           */

/**/
static int
makecomplist(char *s, int incmd, int *delit, int *compadd, int untokenized)
{
    Compctl cc = NULL;
    int oloffs = offs, owe = we, owb = wb, ocs = cs, oll = ll, isf = 1;
    int t, sf1, sf2, ooffs;
    char *p, *sd = NULL, *tt, *s1, *s2, *os = NULL;
    unsigned char *ol = NULL;

    /* If we already have a list from a previous execution of this *
     * function, skip the list building code.                      */
    if (validlist)
	return !nmatches;

    os = dupstring(s);
    ol = (unsigned char *)dupstring((char *)line);

  xorrec:

    DPUTS(ll != strlen((char *) line), "BUG: xorrec: ll != strlen(line)");

    /* Go to the end of the word if complete_in_word is not set. */
    if (unset(COMPLETEINWORD) && cs != we)
	cs = we, offs = strlen(s);

    ispattern = haswhat = lastambig = 0;
    patcomp = filecomp = NULL;
    menucur = NULL;
    shortest = NULL;
    fshortest = NULL;
    rpre = rsuf = lpre = lsuf = ppre = psuf = prpre =
	fpre = fsuf = firstm = ffirstm = parampre = qparampre = NULL;

    /* Blank out the lists. */
    matches = newlinklist();
    fmatches = newlinklist();

    /* If we don't have a compctl definition yet or we have a compctl *
     * with extended completion, get it (or the next one, resp.).     */
    if (!cc || cc->ext)
	cc = get_ccompctl(cc, compadd, incmd);

    /* *compadd is the number of characters we have to ignore at the *
     * beginning of the word.                                        */
    wb += *compadd;
    s += *compadd;
    if ((offs -= *compadd) < 0)
	/* It's bigger than our word prefix, so we can't help here... */
	return 1;

    /* Insert the prefix (compctl -P), if any. */
    if (cc->prefix) {
	int pl = 0, sl = strlen(cc->prefix);

	if (*s) {
	    /* First find out how much of the prefix is already on the line. */
	    sd = dupstring(s);
	    untokenize(sd);
	    pl = pfxlen(cc->prefix, sd);
	    s += pl;
	}
	if (pl < sl) {
	    int savecs = cs;

	    /* Then insert the prefix. */
	    cs = wb + pl;
	    inststrlen(cc->prefix + pl, 0, sl - pl);
	    cs = savecs + sl - pl;
	}
	/* And adjust the word beginning/end variables. */
	wb += sl;
	we += sl - pl;
	offs -= pl;
    }
    /* Does this compctl have a suffix (compctl -S)? */
    if ((ccsuffix = cc->suffix) && *ccsuffix) {
	char *sdup = dupstring(ccsuffix);
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
	    offs <= suffixll - sl && !strcmp(sdup, sd + suffixll - sl)) {
	    ccsuffix = NULL;
	    haswhat |= HAS_SUFFIX;
	    s[suffixll - sl] = '\0';
	}
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
	    parambr = br - 1;
	    /* Get the prefix (anything up to the character before the name). */
	    *e = '\0';
	    parampre = ztrduppfx(s, b - s);
	    qparampre = ztrdup(quotename(parampre, NULL, NULL, NULL));
	    untokenize(qparampre);
	    qparprelen = strlen(qparampre);
	    /* And adjust wb, we, and offs again. */
	    offs -= b - s;
	    wb = cs - offs;
	    we = wb + e - b;
	    s = b;
	    /* And now make sure that we complete parameter names. */
	    cc = ccmain = &cc_dummy;
	    cc_dummy.refc = 10000;
	    cc_dummy.mask = CC_PARAMS | CC_ENVVARS;
	}
    }
    ooffs = offs;
    /* If we have to ignore the word, do that. */
    if (cc->mask & CC_DELETE) {
	*delit = 1;
	*s = '\0';
	offs = 0;
    } else
	*delit = 0;

    /* Compute line prefix/suffix. */

    lpl = offs;
    lpre = zalloc(lpl + 1);
    memcpy(lpre, s, lpl);
    lpre[lpl] = '\0';
    p = quotename(lpre, NULL, NULL, NULL);
    if (strcmp(p, lpre) && !strpfx(p, qword)) {
	int l1, l2;

	backdel(l1 = cs - wb);
	untokenize(p);
	inststrlen(p, 1, l2 = strlen(p));
	we += l2 - l1;
    }
    lsuf = ztrdup(s + offs);
    lsl = strlen(lsuf);
    if (lsl && (p = quotename(lsuf, NULL, NULL, NULL)) &&
	(strcmp(p, lsuf) && !strsfx(p, qword))) {
	int l1, l2;

	foredel(l1 = strlen(s + offs));
	untokenize(p);
	inststrlen(p, 0, l2 = strlen(p));
	we += l2 - l1;
    }

    /* First check for ~.../... */
    if (ic == Tilde) {
	for (p = lpre + lpl; p > lpre; p--)
	    if (*p == '/')
		break;

	if (*p == '/')
	    ic = 0;
    }
    /* Compute real prefix/suffix. */

    noreal = !*delit;
    for (p = lpre; *p && *p != String && *p != Tick; p++);
    tt = ic && !parampre ? lpre + 1 : lpre;
    rpre = (*p || *lpre == Tilde || *lpre == Equals) ?
	(noreal = 0, getreal(tt)) :
	ztrdup(tt);

    for (p = lsuf; *p && *p != String && *p != Tick; p++);
    rsuf = *p ? (noreal = 0, getreal(lsuf)) : ztrdup(lsuf);

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
	    ppre = ztrdup("");
	else
	    ppre = ztrduppfx(rpre, s1 - rpre + 1);
	psuf = ztrdup(s2);

	/* And get the file prefix. */
	fpre = ztrdup(((s1 == s || s1 == rpre || ic) &&
		       (*s != '/' || cs == wb)) ? s1 : s1 + 1);
	/* And the suffix. */
	fsuf = ztrduppfx(rsuf, s2 - rsuf);

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
		    haswhat |= HAS_PATHPAT;
		    for (n = firstnode(l); n; incnode(n))
			addmatch(getdata(n), NULL);
		}
		opts[NULLGLOB] = ng;
	    } else {
		/* No pattern matching. */
		addwhat = CC_FILES;
		if (cc->withd) {
		    prpre = tricat(cc->withd, "/", ppre);
		} else
		    prpre = ztrdup(ppre);

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
			glob_pre = fpre;
			glob_suf = fsuf;

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
	    }
	}
    }
    /* Use tricat() instead of dyncat() to get zalloc()'d memory. */
    if (ic) {
	/* Now change the `~' and `=' tokens to the real characters so *
	 * that things starting with these characters will be added.   */
	char *orpre = rpre;

	rpre = tricat("", (ic == Tilde) ? "~" : "=", rpre);
	rpl++;
	zsfree(orpre);
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

	    if (*delit) {
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

    /* If we have no matches, ignore fignore. */
    if (empty(matches)) {
	matches = fmatches;
	firstm = ffirstm;
	shortest = fshortest;
	ab = fab;
	ae = fae;
	shortl = fshortl;
    }

    /* Make an array from the list of matches. */
    makearray(matches);
    PERMALLOC {
	amatches = arrdup(amatches);
	if (firstm)
	    firstm = ztrdup(firstm);
	/* And quote the prefixes/suffixes. */
	if (hasspecial(s)) {
	    zfree(lpre, lpl);
	    zfree(lsuf, lsl);
	    lpre = zalloc(lpl + 1);
	    memcpy(lpre, s, lpl);
	    lpre[lpl] = '\0';
	    lsuf = ztrdup(s + offs);
	    quotepresuf(&lpre);
	    quotepresuf(&lsuf);
	    untokenize(lpre);
	    untokenize(lsuf);
	}
	quotepresuf(&fpre);
	quotepresuf(&fsuf);
	quotepresuf(&ppre);
	quotepresuf(&psuf);
    } LASTALLOC;

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
	    int addlen = strlen(rpre) + strlen(rsuf) + 1;

	    addlinknode(args, cc->ylist);
	    for (yaptr = amatches; *yaptr; yaptr++) {
		/* can't use tricat(). rats. */
		char *ptr = (char *)halloc(addlen + strlen(*yaptr));
		sprintf(ptr, "%s%s%s", rpre, *yaptr, rsuf);
		addlinknode(args, ptr);
	    }

	    /* No harm in allowing read -l and -c here, too */
	    incompctlfunc = 1;
	    doshfunc(list, args, 0, 1);
	    incompctlfunc = 0;
	    uv = "reply";
	}
	if (uv && (yaptr = get_user_var(uv))) {
	    PERMALLOC {
		aylist = arrdup(yaptr);
	    } LASTALLOC;
	}
    }

    /* Get the explanation string we will have to print:    *
     * do this here in case a -y function alters the messge */
    if ((expl = cc->explain)) {
	if (cc->mask & CC_EXPANDEXPL && !parsestr(expl = dupstring(expl))) {
	    singsub(&expl);
	    untokenize(expl);
	}
	expl = ztrdup(expl);
    }

    remsuffix = (cc->mask & CC_REMOVE);
    ccsuffix = cc->suffix;

    validlist = 1;
    if (nmatches && !errflag)
	return 0;

    if ((isf || cc->xor) && !parampre) {
	/* We found no matches, but there is a xor'ed completion: *
	 * fine, so go back and continue with that compctl.       */
	errflag = 0;
	cc = cc->xor;
	isf = 0;
	wb = owb;
	we = owe;
	cs = ocs;
	ll = oll;
	strcpy((char *)line, (char *)ol);
	offs = oloffs;
	s = dupstring(os);
	free(amatches);
	zsfree(rpre);
	zsfree(rsuf);
	zsfree(lpre);
	zsfree(lsuf);
	zsfree(ppre);
	zsfree(psuf);
	zsfree(fpre);
	zsfree(fsuf);
	zsfree(prpre);
	zsfree(parampre);
	zsfree(qparampre);
	zsfree(firstm);
	if (expl)
	    zsfree(expl);
	expl = NULL;
	if (aylist)
	    freearray(aylist);
	aylist = NULL;
	goto xorrec;
    }

    /* No matches and xor'ed completion: restore the command line if  *
     * it was alredy quoted, which is the case when s is untokenized. */
    if (untokenized)
	strcpy((char *)line, (char *)ol);
    return 1;
}

/* Invalidate the completion list. */

/**/
void
invalidatelist(void)
{
    if(showinglist == -2)
	listmatches();
    if(validlist) {
	freearray(amatches);
	if (aylist)
	    freearray(aylist);
	aylist = NULL;
	if (expl)
	    zsfree(expl);
	expl = 0;
	zsfree(rpre);
	zsfree(rsuf);
	zsfree(lpre);
	zsfree(lsuf);
	zsfree(ppre);
	zsfree(psuf);
	zsfree(fpre);
	zsfree(fsuf);
	zsfree(prpre);
	zsfree(parampre);
	zsfree(qparampre);
	zsfree(firstm);
	if (ccmain != &cc_dummy)
	    freecompctl(ccmain);
    }
    lastambig = menucmp = showinglist = validlist = 0;
    menucur = NULL;
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
strbpcmp(const void *a, const void *b)
{
    char *aa = *((char **)a), *bb = *((char **)b);

    while (*aa && *bb) {
	if (*aa == '\\')
	    aa++;
	if (*bb == '\\')
	    bb++;
	if (*aa != *bb)
	    return (int)(*aa - *bb);
	if (*aa)
	    aa++;
	if (*bb)
	    bb++;
    }
    return (int)(*aa - *bb);
}

/* Make an array from a linked list */

/**/
static void
makearray(LinkList l)
{
    char **ap, **bp, **cp;
    LinkNode nod;

    /* Build an array for the matches. */
    ap = amatches = (char **)ncalloc(((nmatches = countlinknodes(l)) + 1) *
				     sizeof(char *));

    /* And copy them into it. */
    for (nod = firstnode(l); nod; incnode(nod))
	*ap++ = (char *)getdata(nod);
    *ap = NULL;

    /* Now sort the array. */
    qsort((void *) amatches, nmatches, sizeof(char *),
	       (int (*) _((const void *, const void *)))strbpcmp);

    /* And delete the ones that occur more than once. */
    for (ap = cp = amatches; *ap; ap++) {
	*cp++ = *ap;
	for (bp = ap; bp[1] && !strcmp(*ap, bp[1]); bp++);
	ap = bp;
    }
    *cp = NULL;
    nmatches = arrlen(amatches);
}

/* Handle the case were we found more than one match. */

/**/
static void
do_ambiguous(void)
{
    int p = (usemenu || ispattern), atend = (cs == we);
    int inv = 0;

    menucmp = 0;

    /* If we have to insert the first match, call do_single().  This is *
     * how REC_EXACT takes effect.  We effectively turn the ambiguous   *
     * completion into an unambiguous one.                              */
    if (shortest && shortl == 0 && isset(RECEXACT) &&
	(usemenu == 0 || unset(AUTOMENU))) {
	do_single(shortest);
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
	/* Sort-of general case: we have an ambiguous completion, and aren't *
	 * starting menu completion or doing anything really weird.  We need *
	 * to insert any unambiguous prefix and suffix, if possible.         */
	if(ab)
	    inststrlen(firstm, 1, ab);
	if(ae && !atend)
	    inststrlen(firstm + strlen(firstm) - ae, 0, ae);
	if(ab || (ae && !atend))
	    inv = 1;
	/* If the LIST_AMBIGUOUS option (meaning roughly `show a list only *
	 * if the completion is completely ambiguous') is set, and some    *
	 * prefix was inserted, return now, bypassing the list-displaying  *
	 * code.  On the way, invalidate the list and note that we don't   *
	 * want to enter an AUTO_MENU imediately.                          */
	if(isset(LISTAMBIGUOUS) && inv) {
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
    if(inv)
	invalidatelist();
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
do_single(char *str)
{
    int l;
    int havesuff = 0;

    fixsuffix();

    if (!menucur) {
	/* We are currently not in a menu-completion, *
	 * so set the position variables.             */
	if (ispattern) {
	    cs = we;
	    menupos = wb;
	} else
	    menupos = cs;
	menuwe = (cs == we) || isset(ALWAYSTOEND);
	menuend = we;
    }
    /* If we are already in a menu-completion or if we have done a *
     * glob completion, we have to delete some of the stuff on the *
     * command line.                                               */
    if (menucur) {
	if (menuinsc) {
	    cs = menuend + lsl;
	    foredel(menuinsc);
	}
	l = menulen;
    } else if (ispattern)
	l = we - wb;
    else
	l = 0;

    menuinsc = 0;
    cs = menupos;
    foredel(l);

    /* And than we insert the new string. */
    inststrlen(str, 1, menulen = strlen(str));
    menuend = cs;

    cs += lsl;

    if (ccsuffix) {
	/* There is a compctl -S suffix.  Add it. */
	if (!(haswhat & HAS_SUFFIX) && *ccsuffix) {
	    havesuff = 1;
	    inststr(ccsuffix);
	    menuinsc = ztrlen(ccsuffix);
	    if (remsuffix && menuwe)
		makesuffix(menuinsc);
	}
	havesuff = 1;
    } else {
	/* There is no user-specified suffix, *
	 * so generate one automagically.     */
	if(parampre && parambr) {
	    /*{{*/
	    /* Completing a parameter in braces.  Add a removable `}' suffix. */
	    inststrlen("}", 1, 1);
	    menuinsc++;
	}
	if(!(haswhat & HAS_MISC) ||
	    	  (parampre && isset(AUTOPARAMSLASH))) {
	    /* If we have only filenames or we completed a parameter name  *
	     * and AUTO_PARAM_SLASH is set, lets see if it is a directory. *
	     * If it is, we append a slash.                                */
	    char *p;
	    struct stat buf;

	    /* Build the path name. */
	    if (ispattern || ic || parampre) {
		int ne = noerrs;

		noerrs = 1;

		if (parampre) {
		    int pl = strlen(parampre);
		    p = (char *) ncalloc(pl + strlen(lpre) + strlen(str) +
					 strlen(lsuf) + 1);
		    sprintf(p, "%s%s%s%s", parampre, lpre, str, lsuf);
		    if (pl && p[pl-1] == Inbrace)
			strcpy(p+pl-1, p+pl);
		}
		else if (ic) {
		    p = (char *) ncalloc(strlen(ppre) + strlen(fpre) + strlen(str) +
					 strlen(fsuf) + strlen(psuf) + 2);
		    sprintf(p, "%c%s%s%s%s%s", ic,
			    ppre, fpre, str, fsuf, psuf);
		}
		else
		    p = dupstring(str);
		parsestr(p);
		if (ic)
		    *p = ic;
		singsub(&p);

		noerrs = ne;
	    } else {
		p = (char *) ncalloc((prpre ? strlen(prpre) : 0) + strlen(fpre) +
				     strlen(str) + strlen(fsuf) + strlen(psuf) + 3);
		sprintf(p, "%s%s%s%s%s",
			(prpre && *prpre) ? prpre : "./", fpre, str,
			fsuf, psuf);
	    }
	    /* And do the stat. */
	    if (!ztat(p, &buf, 0) && S_ISDIR(buf.st_mode)) {
		/* It is a directory, so add the slash. */
		havesuff = 1;
		inststrlen("/", 1, 1);
		menuinsc++;
		if(menuwe && isset(AUTOREMOVESLASH)) {
		    makesuffix(1);
		    suffixlen['/'] = 1;
		}
	    }
	}
    }
    /* If completing in a brace expansion... */
    if(complinbrace) {
	if(havesuff) {
	    /*{{*/
	    /* If a suffix was added, and is removable, let *
	     * `,' and `}' remove it.                       */
	    if(isset(AUTOPARAMKEYS))
		suffixlen[','] = suffixlen['}'] = suffixlen[256];
	} else {
	    /*{{*/
	    /* Otherwise, add a `,' suffix, and let `}' remove it. */
	    havesuff = 1;
	    inststrlen(",", 1, 1);
	    menuinsc++;
	    if(menuwe && isset(AUTOPARAMKEYS))
		suffixlen[','] = suffixlen['}'] = 1;
	}
    } else if(!menucmp && !havesuff) {
	/* If we didn't add a suffix, add a space, unless we are *
	 * doing menu completion.                                */
	inststrlen(" ", 1, 1);
	menuinsc++;
	if(menuwe)
	    makesuffix(1);
    }
    if(menuwe && parampre && isset(AUTOPARAMKEYS))
	makeparamsuffix(parambr, menuinsc);

    if (!menuwe)
	cs = menuend;
}

/* This handles the beginning of menu-completion. */

/**/
static void
do_ambig_menu(void)
{
    menucmp = 1;
    menucur = NULL;
    do_single(amatches[0]);
    menucur = amatches;
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
    int l = 0, cc = 0;

    for (; *p; p++) {
	/* Handle the `%' stuff (%% == %, %n == <number of matches>). */
	if (*p == '%') {
	    if (*++p) {
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

/* List the matches.  Note that the list entries are metafied. */

/**/
void
listmatches(void)
{
    int longest = 1, fct, fw, colsz, t0, t1, ct, up, cl, xup = 0;
    int off = 0, boff = 0, nboff = 0;
    int of = (!aylist && isset(LISTTYPES) && !(haswhat & HAS_MISC));
    char **arr, **ap, sav;
    int nfpl, nfsl, nlpl, nlsl;
    int listmax = getiparam("LISTMAX"), litnl = 0;
    size_t (*strlenfn) _((char const *));

#ifdef DEBUG
    /* Sanity check */
    if(!validlist) {
	showmsg("BUG: listmatches called with bogus list");
	return;
    }
#endif

    /* Calculate lengths of prefixes/suffixes to be added */
    nfpl = fpre ? niceztrlen(fpre) : 0;
    nfsl = fsuf ? niceztrlen(fsuf) : 0;
    nlpl = lpre ? niceztrlen(lpre) : 0;
    nlsl = lsuf ? niceztrlen(lsuf) : 0;

    /* Calculate the lengths of the prefixes/suffixes we have to ignore
       during printing. */
    if (ispattern && !aylist && !(haswhat & (HAS_MISC | HAS_PATHPAT))) {
	if (ppre && *ppre)
	    off = strlen(ppre);
	if (psuf && *psuf) {
	    boff = strlen(psuf);
	    nboff = niceztrlen(psuf);
	}
    }

    /* Set the cursor below the prompt. */
    trashzle();
    showinglist = 0;

    clearflag = (isset(USEZLE) && !termflags &&
		 (isset(ALWAYSLASTPROMPT) && zmult == 1)) ||
	(unset(ALWAYSLASTPROMPT) && zmult != 1);

    /* just to keep gcc happy */
    fw = colsz = up = 0;
    if (aylist) {
	arr = aylist;
	/* If no literal newlines, the remaining code should use strlen() */
	strlenfn = (size_t (*) _((char const *)))strlen;

	/* The hard bit here is that we are handling newlines literally.   *
	 * In fact, we are in principle handling all characters literally, *
	 * but it's quite enough work with just newlines.                  *
	 * If there are such, we give up trying to print the list as       *
	 * columns and print as rows, counting the extra newlines.         */
	ct = 0;
	for (ap = arr; *ap; ap++) {
	    ct++;
	    if (strchr(*ap, '\n'))
		litnl++;
	}
	if (litnl) {
	    colsz = ct;
	    up = colsz + nlnct - clearflag;
	    /* Count real newlines, as well as overflowing lines. */
	    for (ap = arr; *ap; ap++) {
		char *nlptr, *sptr = *ap;
		while (sptr && *sptr) {
		    up += (nlptr = strchr(sptr, '\n'))
			? 1 + (nlptr-sptr)/columns
			   : strlen(sptr)/columns;
		    sptr = nlptr ? nlptr+1 : NULL;
		}
	    }
	}
    } else {
	arr = amatches;
	ct = nmatches;
	strlenfn = niceztrlen;
    }


    if (!litnl) {
	/* Calculate the column width, the number of columns and the
	   number of lines. */
	for (ap = arr; *ap; ap++)
	    if ((cl = strlenfn(*ap + off) - nboff +
		 ((ispattern || aylist) ? 0 :
		  (!(haswhat & HAS_MISC) ?
		   nfpl + nfsl : nlpl + nlsl))) > longest)
		longest = cl;
	if (of)
	    longest++;

	fw = longest + 2;
	fct = (columns + 1) / fw;
	if (fct == 0) {
	    fct = 1;
	    colsz = ct;
	    up = colsz + nlnct - clearflag;
	    for (ap = arr; *ap; ap++)
		up += (strlenfn(*ap + off) - nboff + of +
		       ((ispattern || aylist) ? 0 :
			(!(haswhat & HAS_MISC) ?
			 nfpl + nfsl : nlpl + nlsl))) / columns;
	} else {
	    colsz = (ct + fct - 1) / fct;
	    up = colsz + nlnct - clearflag + (ct == 0);
	}
    }

    /* Print the explanation string, if any. */
    if (expl) {
	xup = printfmt(expl, ct, 1) + 1;
	putc('\n', shout);
	up += xup;
    }

    /* Maybe we have to ask if the user wants to see the list. */
    if ((listmax && ct > listmax) || (!listmax && up >= lines)) {
	int qup;
	setterm();
	qup = printfmt("zsh: do you wish to see all %n possibilities? ", ct, 1);
	fflush(shout);
	if (getzlequery() != 'y') {
	    if (clearflag) {
		putc('\r', shout);
		tcmultout(TCUP, TCMULTUP, qup);
		if (tccan(TCCLEAREOD))
		    tcout(TCCLEAREOD);
		tcmultout(TCUP, TCMULTUP, nlnct + xup);
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
    for (t1 = 0; t1 != colsz; t1++) {
	ap = arr + t1;
	if (of) {
	    /* We have to print the file types. */
	    while (*ap) {
		int t2;
		char *pb;
		struct stat buf;

		/* Build the path name for the stat. */
		if (ispattern) {
		    int cut = strlen(*ap) - boff;

		    sav = ap[0][cut];
		    ap[0][cut] = '\0';
		    nicezputs(*ap + off, shout);
		    t2 = niceztrlen(*ap + off);
		    ap[0][cut] = sav;
		    pb = *ap;
		} else {
		    nicezputs(fpre, shout);
		    nicezputs(*ap, shout);
		    nicezputs(fsuf, shout);
		    t2 = nfpl + niceztrlen(*ap) + nfsl;
		    pb = (char *) halloc((prpre ? strlen(prpre) : 0) + 3 +
					 strlen(fpre) + strlen(*ap) + strlen(fsuf));
		    sprintf(pb, "%s%s%s%s",
			    (prpre && *prpre) ? prpre : "./", fpre, *ap, fsuf);
		}
		if (ztat(pb, &buf, 1))
		    putc(' ', shout);
		else
		    /* Print the file type character. */
		    putc(file_type(buf.st_mode), shout);
		for (t0 = colsz; t0 && *ap; t0--, ap++);
		if (*ap)
		    /* And add spaces to make the columns aligned. */
		    for (++t2; t2 < fw; t2++)
			putc(' ', shout);
	    }
	} else
	    while (*ap) {
		int t2;

		if (aylist) {
		    zputs(*ap, shout);
		    t2 = strlen(*ap);
		} else if (ispattern) {
		    int cut = strlen(*ap) - boff;

		    sav = ap[0][cut];
		    ap[0][cut] = '\0';
		    nicezputs(*ap + off, shout);
		    t2 = niceztrlen(*ap + off);
		    ap[0][cut] = sav;
		} else if (!(haswhat & HAS_MISC)) {
		    nicezputs(fpre, shout);
		    nicezputs(*ap, shout);
		    nicezputs(fsuf, shout);
		    t2 = nfpl + niceztrlen(*ap) + nfsl;
		} else {
		    nicezputs(lpre, shout);
		    nicezputs(*ap, shout);
		    nicezputs(lsuf, shout);
		    t2 = nlpl + niceztrlen(*ap) + nlsl;
		}
		for (t0 = colsz; t0 && *ap; t0--, ap++);
		if (*ap)
		    for (; t2 < fw; t2++)
			putc(' ', shout);
	    }
	if (t1 != colsz - 1 || !clearflag)
	    putc('\n', shout);
    }
    if (clearflag)
	/* Move the cursor up to the prompt, if always_last_prompt *
	 * is set and all that...                                  */
	if (up < lines) {
	    tcmultout(TCUP, TCMULTUP, up);
	    showinglist = -1;
	} else
	    clearflag = 0, putc('\n', shout);
}

/* This is used to print expansions. */

/**/
void
listlist(LinkList l)
{
    int hw = haswhat, ip = ispattern;
    char *lp = lpre, *ls = lsuf;
    int nm = nmatches, vl = validlist;
    char **am = amatches, **ay = aylist;
    char *ex = expl;

    haswhat = HAS_MISC;
    ispattern = 0;
    validlist = 1;
    lpre = lsuf = "";
    aylist = NULL;
    expl = NULL;

    makearray(l);
    listmatches();
    showinglist = 0;

    expl = ex;
    amatches = am;
    aylist = ay;
    nmatches = nm;
    validlist = vl;
    lpre = lp;
    lsuf = ls;
    ispattern = ip;
    haswhat = hw;
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
