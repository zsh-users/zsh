/*
 * hist.c - history expansion
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
#include "hist.pro"

/* Functions to call for getting/ungetting a character and for history
 * word control. */

/**/
mod_export int (*hgetc) _((void));

/**/
void (*hungetc) _((int));

/**/
void (*hwaddc) _((int));

/**/
void (*hwbegin) _((int));

/**/
void (*hwend) _((void));

/**/
void (*addtoline) _((int));

/* != 0 means history substitution is turned off */
 
/**/
mod_export int stophist;

/* if != 0, we are expanding the current line */

/**/
mod_export int expanding;

/* these are used to modify the cursor position during expansion */

/**/
mod_export int excs, exlast;

/*
 * Current history event number
 *
 * Note on curhist: with history inactive, this points to the
 * last line actually added to the history list.  With history active,
 * the line does not get added to the list until hend(), if at all.
 * However, curhist is incremented to reflect the current line anyway
 * and a temporary history entry is inserted while the user is editing.
 * If the resulting line was not added to the list, a flag is set so
 * that curhist will be decremented in hbegin().
 */
 
/**/
mod_export zlong curhist;

/**/
struct histent curline;

/* current line count of allocated history entries */

/**/
zlong histlinect;

/* The history lines are kept in a hash, and also doubly-linked in a ring */

/**/
HashTable histtab;
/**/
mod_export Histent hist_ring;
 
/* capacity of history lists */
 
/**/
zlong histsiz;
 
/* desired history-file size (in lines) */
 
/**/
zlong savehistsiz;
 
/* if = 1, we have performed history substitution on the current line *
 * if = 2, we have used the 'p' modifier                              */
 
/**/
int histdone;
 
/* state of the history mechanism */
 
/**/
int histactive;

/* Current setting of the associated option, but sometimes also includes
 * the setting of the HIST_SAVE_NO_DUPS option. */

/**/
int hist_ignore_all_dups;

/* What flags (if any) we should skip when moving through the history */

/**/
mod_export int hist_skip_flags;

/* Bits of histactive variable */
#define HA_ACTIVE	(1<<0)	/* History mechanism is active */
#define HA_NOSTORE	(1<<1)	/* Don't store the line when finished */
#define HA_NOINC	(1<<2)	/* Don't store, curhist not incremented */

/* Array of word beginnings and endings in current history line. */

/**/
short *chwords;

/* Max, actual position in chwords.
 * nwords = chwordpos/2 because we record beginning and end of words.
 */

/**/
int chwordlen, chwordpos;

/* the last l for s/l/r/ history substitution */
 
/**/
char *hsubl;

/* the last r for s/l/r/ history substitution */
 
/**/
char *hsubr;
 
/* pointer into the history line */
 
/**/
mod_export char *hptr;
 
/* the current history line */
 
/**/
mod_export char *chline;

/* true if the last character returned by hgetc was an escaped bangchar *
 * if it is set and NOBANGHIST is unset hwaddc escapes bangchars        */

/**/
int qbang;
 
/* max size of histline */
 
/**/
int hlinesz;
 
/* default event (usually curhist-1, that is, "!!") */
 
static zlong defev;

/* add a character to the current history word */

static void
ihwaddc(int c)
{
    /* Only if history line exists and lexing has not finished. */
    if (chline && !(errflag || lexstop)) {
	/* Quote un-expanded bangs in the history line. */
	if (c == bangchar && stophist < 2 && qbang)
	    /* If qbang is not set, we do not escape this bangchar as it's *
	     * not mecessary (e.g. it's a bang in !=, or it is followed    *
	     * by a space). Roughly speaking, qbang is zero only if the    *
	     * history interpreter has already digested this bang and      *
	     * found that it is not necessary to escape it.                */
	    hwaddc('\\');
	*hptr++ = c;

	/* Resize history line if necessary */
	if (hptr - chline >= hlinesz) {
	    int oldsiz = hlinesz;

	    chline = realloc(chline, hlinesz = oldsiz + 64);
	    hptr = chline + oldsiz;
	}
    }
}

/* This function adds a character to the zle input line. It is used when *
 * zsh expands history (see doexpandhist() in zle_tricky.c). It also     *
 * calculates the new cursor position after the expansion. It is called  *
 * from hgetc() and from gettok() in lex.c for characters in comments.   */

/**/
void
iaddtoline(int c)
{
    if (!expanding || lexstop)
	return;
    if (qbang && c == bangchar && stophist < 2) {
	exlast--;
	zleaddtolineptr('\\');
    }
    if (excs > zlemetacs) {
	excs += 1 + inbufct - exlast;
	if (excs < zlemetacs)
	    /* this case could be handled better but it is    *
	     * so rare that it does not worth it              */
	    excs = zlemetacs;
    }
    exlast = inbufct;
    zleaddtolineptr(itok(c) ? ztokens[c - Pound] : c);
}


static int
ihgetc(void)
{
    int c = ingetc();

    qbang = 0;
    if (!stophist && !(inbufflags & INP_ALIAS)) {
	/* If necessary, expand history characters. */
	c = histsubchar(c);
	if (c < 0) {
	    /* bad expansion */
	    errflag = lexstop = 1;
	    return ' ';
	}
    }
    if ((inbufflags & INP_HIST) && !stophist) {
	/* the current character c came from a history expansion          *
	 * (inbufflags & INP_HIST) and history is not disabled            *
	 * (e.g. we are not inside single quotes). In that case, \!       *
	 * should be treated as ! (since this \! came from a previous     *
	 * history line where \ was used to escape the bang). So if       *
	 * c == '\\' we fetch one more character to see if it's a bang,   *
	 * and if it is not, we unget it and reset c back to '\\'         */
	qbang = 0;
	if (c == '\\' && !(qbang = (c = ingetc()) == bangchar))
	    safeinungetc(c), c = '\\';
    } else if (stophist || (inbufflags & INP_ALIAS))
	/* If the result is a bangchar which came from history or alias  *
	 * expansion, we treat it as an escaped bangchar, unless history *
	 * is disabled. If stophist == 1 it only means that history is   *
	 * temporarily disabled by a !" which won't appear in in the     *
	 * history, so we still have an escaped bang. stophist > 1 if    *
	 * history is disabled with NOBANGHIST or by someone else (e.g.  *
	 * when the lexer scans single quoted text).                     */
	qbang = c == bangchar && (stophist < 2);
    hwaddc(c);
    addtoline(c);

    return c;
}

/**/
static void
safeinungetc(int c)
{
    if (lexstop)
	lexstop = 0;
    else
	inungetc(c);
}

/**/
void
herrflush(void)
{
    inpopalias();

    while (!lexstop && inbufct && !strin)
	hwaddc(ingetc());
}

/*
 * Extract :s/foo/bar/ delimiters and arguments
 *
 * The first character expected is the first delimiter.
 * The arguments are stored in the hsubl and hsubr variables.
 *
 * subline is the part of the command line to be matched.
 *
 * If a ':' was found but was not followed by a 'G',
 * *cflagp is set to 1 and the input is backed up to the
 * character following the colon.
 */

/**/
static int
getsubsargs(char *subline, int *gbalp, int *cflagp)
{
    int del, follow;
    char *ptr1, *ptr2;

    del = ingetc();
    ptr1 = hdynread2(del);
    if (!ptr1)
	return 1;
    ptr2 = hdynread2(del);
    if (strlen(ptr1)) {
	zsfree(hsubl);
	hsubl = ptr1;
    } else if (!hsubl)		/* fail silently on this */
	return 0;
    zsfree(hsubr);
    hsubr = ptr2;
    follow = ingetc();
    if (follow == ':') {
	follow = ingetc();
	if (follow == 'G')
	    *gbalp = 1;
	else {
	    inungetc(follow);
	    *cflagp = 1;
	}
    } else
	inungetc(follow);
    return 0;
}

/* Get the maximum no. of words for a history entry. */

/**/
static int
getargc(Histent ehist)
{
    return ehist->nwords ? ehist->nwords-1 : 0;
}

/**/
static int
substfailed(void)
{
    herrflush();
    zerr("substitution failed");
    return -1;
}

/* Perform history substitution, returning the next character afterwards. */

/**/
static int
histsubchar(int c)
{
    int farg, evset = -1, larg, argc, cflag = 0, bflag = 0;
    zlong ev;
    static int marg = -1;
    static zlong mev = -1;
    char buf[256], *ptr;
    char *sline;
    Histent ehist;

    /* look, no goto's */
    if (isfirstch && c == hatchar) {
	int gbal = 0;

	/* Line begins ^foo^bar */
	isfirstch = 0;
	inungetc(hatchar);
	if (!(ehist = gethist(defev))
	    || !(sline = getargs(ehist, 0, getargc(ehist))))
	    return -1;

	if (getsubsargs(sline, &gbal, &cflag))
	    return substfailed();
	if (!hsubl)
	    return -1;
	if (subst(&sline, hsubl, hsubr, gbal))
	    return substfailed();
    } else {
	/* Line doesn't begin ^foo^bar */
	if (c != ' ')
	    isfirstch = 0;
	if (c == '\\') {
	    int g = ingetc();

	    if (g != bangchar)
		safeinungetc(g);
	    else {
		qbang = 1;
		return bangchar;
	    }
	}
	if (c != bangchar)
	    return c;
	*hptr = '\0';
	if ((c = ingetc()) == '{') {
	    bflag = cflag = 1;
	    c = ingetc();
	}
	if (c == '\"') {
	    stophist = 1;
	    return ingetc();
	}
	if ((!cflag && inblank(c)) || c == '=' || c == '(' || lexstop) {
	    safeinungetc(c);
	    return bangchar;
	}
	cflag = 0;
	ptr = buf;

	/* get event number */

	queue_signals();
	if (c == '?') {
	    for (;;) {
		c = ingetc();
		if (c == '?' || c == '\n' || lexstop)
		    break;
		else
		    *ptr++ = c;
	    }
	    if (c != '\n' && !lexstop)
		c = ingetc();
	    *ptr = '\0';
	    mev = ev = hconsearch(hsubl = ztrdup(buf), &marg);
	    evset = 0;
	    if (ev == -1) {
		herrflush();
		unqueue_signals();
		zerr("no such event: %s", buf);
		return -1;
	    }
	} else {
	    zlong t0;

	    for (;;) {
		if (inblank(c) || c == ';' || c == ':' || c == '^' ||
		    c == '$' || c == '*' || c == '%' || c == '}' ||
		    c == '\'' || c == '"' || c == '`' || lexstop)
		    break;
		if (ptr != buf) {
		    if (c == '-')
			break;
		    if ((idigit(buf[0]) || buf[0] == '-') && !idigit(c))
			break;
		}
		*ptr++ = c;
		if (c == '#' || c == bangchar) {
		    c = ingetc();
		    break;
		}
		c = ingetc();
	    }
	    *ptr = 0;
	    if (!*buf) {
		if (c != '%') {
		    if (isset(CSHJUNKIEHISTORY))
			ev = addhistnum(curhist,-1,HIST_FOREIGN);
		    else
			ev = defev;
		    if (c == ':' && evset == -1)
			evset = 0;
		    else
			evset = 1;
		} else {
		    if (marg != -1)
			ev = mev;
		    else
			ev = defev;
		    evset = 0;
		}
	    } else if ((t0 = zstrtol(buf, NULL, 10))) {
		ev = (t0 < 0) ? addhistnum(curhist,t0,HIST_FOREIGN) : t0;
		evset = 1;
	    } else if ((unsigned)*buf == bangchar) {
		ev = addhistnum(curhist,-1,HIST_FOREIGN);
		evset = 1;
	    } else if (*buf == '#') {
		ev = curhist;
		evset = 1;
	    } else if ((ev = hcomsearch(buf)) == -1) {
		herrflush();
		unqueue_signals();
		zerr("event not found: %s", buf);
		return -1;
	    } else
		evset = 1;
	}

	/* get the event */

	if (!(ehist = gethist(defev = ev))) {
	    unqueue_signals();
	    return -1;
	}
	/* extract the relevant arguments */

	argc = getargc(ehist);
	if (c == ':') {
	    cflag = 1;
	    c = ingetc();
	    if (c == '%' && marg != -1) {
		if (!evset) {
		    ehist = gethist(defev = mev);
		    argc = getargc(ehist);
		} else {
		    herrflush();
		    unqueue_signals();
		    zerr("Ambiguous history reference");
		    return -1;
		}

	    }
	}
	if (c == '*') {
	    farg = 1;
	    larg = argc;
	    cflag = 0;
	} else {
	    inungetc(c);
	    larg = farg = getargspec(argc, marg, evset);
	    if (larg == -2) {
		unqueue_signals();
		return -1;
	    }
	    if (farg != -1)
		cflag = 0;
	    c = ingetc();
	    if (c == '*') {
		cflag = 0;
		larg = argc;
	    } else if (c == '-') {
		cflag = 0;
		larg = getargspec(argc, marg, evset);
		if (larg == -2) {
		    unqueue_signals();
		    return -1;
		}
		if (larg == -1)
		    larg = argc - 1;
	    } else
		inungetc(c);
	}
	if (farg == -1)
	    farg = 0;
	if (larg == -1)
	    larg = argc;
	if (!(sline = getargs(ehist, farg, larg))) {
	    unqueue_signals();
	    return -1;
	}
	unqueue_signals();
    }

    /* do the modifiers */

    for (;;) {
	c = (cflag) ? ':' : ingetc();
	cflag = 0;
	if (c == ':') {
	    int gbal = 0;

	    if ((c = ingetc()) == 'g') {
		gbal = 1;
		c = ingetc();
		if (c != 's' && c != '&') {
		    zerr("'s' or '&' modifier expected after 'g'");
		    return -1;
		}
	    }
	    switch (c) {
	    case 'p':
		histdone = HISTFLAG_DONE | HISTFLAG_NOEXEC;
		break;
	    case 'h':
		if (!remtpath(&sline)) {
		    herrflush();
		    zerr("modifier failed: h");
		    return -1;
		}
		break;
	    case 'e':
		if (!rembutext(&sline)) {
		    herrflush();
		    zerr("modifier failed: e");
		    return -1;
		}
		break;
	    case 'r':
		if (!remtext(&sline)) {
		    herrflush();
		    zerr("modifier failed: r");
		    return -1;
		}
		break;
	    case 't':
		if (!remlpaths(&sline)) {
		    herrflush();
		    zerr("modifier failed: t");
		    return -1;
		}
		break;
	    case 's':
		if (getsubsargs(sline, &gbal, &cflag))
		    return -1; /* fall through */
	    case '&':
		if (hsubl && hsubr) {
		    if (subst(&sline, hsubl, hsubr, gbal))
			return substfailed();
		} else {
		    herrflush();
		    zerr("no previous substitution");
		    return -1;
		}
		break;
	    case 'q':
		quote(&sline);
		break;
	    case 'Q':
		{
		    int one = noerrs, oef = errflag;

		    noerrs = 1;
		    parse_subst_string(sline);
		    noerrs = one;
		    errflag = oef;
		    remnulargs(sline);
		    untokenize(sline);
		}
		break;
	    case 'x':
		quotebreak(&sline);
		break;
	    case 'l':
		sline = casemodify(sline, CASMOD_LOWER);
		break;
	    case 'u':
		sline = casemodify(sline, CASMOD_UPPER);
		break;
	    default:
		herrflush();
		zerr("illegal modifier: %c", c);
		return -1;
	    }
	} else {
	    if (c != '}' || !bflag)
		inungetc(c);
	    if (c != '}' && bflag) {
		zerr("'}' expected");
		return -1;
	    }
	    break;
	}
    }

    /*
     * Push the expanded value onto the input stack,
     * marking this as a history word for purposes of the alias stack.
     */

    lexstop = 0;
    /* this function is called only called from hgetc and only if      *
     * !(inbufflags & INP_ALIAS). History expansion should never be    *
     * done with INP_ALIAS (to prevent recursive history expansion and *
     * histoty expansion of aliases). Escapes are not removed here.    *
     * This is now handled in hgetc.                                   */
    inpush(sline, INP_HIST, NULL); /* sline from heap, don't free */
    histdone |= HISTFLAG_DONE;
    if (isset(HISTVERIFY))
	histdone |= HISTFLAG_NOEXEC | HISTFLAG_RECALL;

    /* Don't try and re-expand line. */
    return ingetc();
}

/* unget a char and remove it from chline. It can only be used *
 * to unget a character returned by hgetc.                     */

static void
ihungetc(int c)
{
    int doit = 1;

    while (!lexstop && !errflag) {
	if (hptr[-1] != (char) c && stophist < 4 &&
	    hptr > chline + 1 && hptr[-1] == '\n' && hptr[-2] == '\\')
	    hungetc('\n'), hungetc('\\');

	if (expanding) {
	    zlemetacs--;
	    zlemetall--;
	    exlast++;
	}
	DPUTS(hptr <= chline, "BUG: hungetc attempted at buffer start");
	hptr--;
	DPUTS(*hptr != (char) c, "BUG: wrong character in hungetc() ");
	qbang = (c == bangchar && stophist < 2 &&
		 hptr > chline && hptr[-1] == '\\');
	if (doit)
	    inungetc(c);
	if (!qbang)
	    return;
	doit = !stophist && ((inbufflags & INP_HIST) ||
				 !(inbufflags & INP_ALIAS));
	c = '\\';
    }
}

/* begin reading a string */

/**/
mod_export void
strinbeg(int dohist)
{
    strin++;
    hbegin(dohist);
    lexinit();
}

/* done reading a string */

/**/
mod_export void
strinend(void)
{
    hend(NULL);
    DPUTS(!strin, "BUG: strinend() called without strinbeg()");
    strin--;
    isfirstch = 1;
    histdone = 0;
}

/* dummy functions to use instead of hwaddc(), hwbegin(), and hwend() when
 * they aren't needed */

static void
nohw(UNUSED(int c))
{
}

static void
nohwe(void)
{
}

/* these functions handle adding/removing curline to/from the hist_ring */

static void
linkcurline(void)
{
    if (!hist_ring)
	hist_ring = curline.up = curline.down = &curline;
    else {
	curline.up = hist_ring;
	curline.down = hist_ring->down;
	hist_ring->down = hist_ring->down->up = &curline;
	hist_ring = &curline;
    }
    curline.histnum = ++curhist;
}

static void
unlinkcurline(void)
{
    curline.up->down = curline.down;
    curline.down->up = curline.up;
    if (hist_ring == &curline) {
	if (!histlinect)
	    hist_ring = NULL;
	else
	    hist_ring = curline.up;
    }
    curhist--;
}

/* initialize the history mechanism */

/**/
mod_export void
hbegin(int dohist)
{
    isfirstln = isfirstch = 1;
    errflag = histdone = 0;
    if (!dohist)
	stophist = 2;
    else if (dohist != 2)
	stophist = (!interact || unset(SHINSTDIN)) ? 2 : 0;
    else
	stophist = 0;
    if (stophist == 2 || (inbufflags & INP_ALIAS)) {
	chline = hptr = NULL;
	hlinesz = 0;
	chwords = NULL;
	chwordlen = 0;
	hgetc = ingetc;
	hungetc = inungetc;
	hwaddc = nohw;
	hwbegin = nohw;
	hwend = nohwe;
	addtoline = nohw;
    } else {
	chline = hptr = zshcalloc(hlinesz = 64);
	chwords = zalloc((chwordlen = 64) * sizeof(short));
	hgetc = ihgetc;
	hungetc = ihungetc;
	hwaddc = ihwaddc;
	hwbegin = ihwbegin;
	hwend = ihwend;
	addtoline = iaddtoline;
	if (!isset(BANGHIST))
	    stophist = 4;
    }
    chwordpos = 0;

    if (hist_ring && !hist_ring->ftim)
	hist_ring->ftim = time(NULL);
    if ((dohist == 2 || (interact && isset(SHINSTDIN))) && !strin) {
	histactive = HA_ACTIVE;
	attachtty(mypgrp);
	linkcurline();
	defev = addhistnum(curhist, -1, HIST_FOREIGN);
    } else
	histactive = HA_ACTIVE | HA_NOINC;
}

/**/
void
histreduceblanks(void)
{
    int i, len, pos, needblank, spacecount = 0;

    if (isset(HISTIGNORESPACE))
	while (chline[spacecount] == ' ') spacecount++;

    for (i = 0, len = spacecount; i < chwordpos; i += 2) {
	len += chwords[i+1] - chwords[i]
	     + (i > 0 && chwords[i] > chwords[i-1]);
    }
    if (chline[len] == '\0')
	return;

    for (i = 0, pos = spacecount; i < chwordpos; i += 2) {
	len = chwords[i+1] - chwords[i];
	needblank = (i < chwordpos-2 && chwords[i+2] > chwords[i+1]);
	if (pos != chwords[i]) {
	    memcpy(chline + pos, chline + chwords[i], len + needblank);
	    chwords[i] = pos;
	    chwords[i+1] = chwords[i] + len;
	}
	pos += len + needblank;
    }
    chline[pos] = '\0';
}

/**/
void
histremovedups(void)
{
    Histent he, next;
    for (he = hist_ring; he; he = next) {
	next = up_histent(he);
	if (he->node.flags & HIST_DUP)
	    freehistnode(&he->node);
    }
}

/**/
mod_export zlong
addhistnum(zlong hl, int n, int xflags)
{
    int dir = n < 0? -1 : n > 0? 1 : 0;
    Histent he = gethistent(hl, dir);
			     
    if (!he)
	return 0;
    if (he->histnum != hl)
	n -= dir;
    if (n)
	he = movehistent(he, n, xflags);
    if (!he)
	return dir < 0? firsthist() - 1 : curhist + 1;
    return he->histnum;
}

/**/
mod_export Histent
movehistent(Histent he, int n, int xflags)
{
    while (n < 0) {
	if (!(he = up_histent(he)))
	    return NULL;
	if (!(he->node.flags & xflags))
	    n++;
    }
    while (n > 0) {
	if (!(he = down_histent(he)))
	    return NULL;
	if (!(he->node.flags & xflags))
	    n--;
    }
    checkcurline(he);
    return he;
}

/**/
mod_export Histent
up_histent(Histent he)
{
    return he->up == hist_ring? NULL : he->up;
}

/**/
mod_export Histent
down_histent(Histent he)
{
    return he == hist_ring? NULL : he->down;
}

/**/
mod_export Histent
gethistent(zlong ev, int nearmatch)
{
    Histent he;

    if (!hist_ring)
	return NULL;

    if (ev - hist_ring->down->histnum < hist_ring->histnum - ev) {
	for (he = hist_ring->down; he->histnum < ev; he = he->down) ;
	if (he->histnum != ev) {
	    if (nearmatch == 0
	     || (nearmatch < 0 && (he = up_histent(he)) == NULL))
		return NULL;
	}
    }
    else {
	for (he = hist_ring; he->histnum > ev; he = he->up) ;
	if (he->histnum != ev) {
	    if (nearmatch == 0
	     || (nearmatch > 0 && (he = down_histent(he)) == NULL))
		return NULL;
	}
    }

    checkcurline(he);
    return he;
}

static void
putoldhistentryontop(short keep_going)
{
    static Histent next = NULL;
    Histent he = keep_going? next : hist_ring->down;
    next = he->down;
    if (isset(HISTEXPIREDUPSFIRST) && !(he->node.flags & HIST_DUP)) {
	static zlong max_unique_ct = 0;
	if (!keep_going)
	    max_unique_ct = savehistsiz;
	do {
	    if (max_unique_ct-- <= 0 || he == hist_ring) {
		max_unique_ct = 0;
		he = hist_ring->down;
		next = hist_ring;
		break;
	    }
	    he = next;
	    next = he->down;
	} while (!(he->node.flags & HIST_DUP));
    }
    if (he != hist_ring->down) {
	he->up->down = he->down;
	he->down->up = he->up;
	he->up = hist_ring;
	he->down = hist_ring->down;
	hist_ring->down = he->down->up = he;
    }
    hist_ring = he;
}

/**/
Histent
prepnexthistent(void)
{
    Histent he; 
    int curline_in_ring = hist_ring == &curline;

    if (curline_in_ring)
	unlinkcurline();
    if (hist_ring && hist_ring->node.flags & HIST_TMPSTORE) {
	curhist--;
	freehistnode(&hist_ring->node);
    }

    if (histlinect < histsiz) {
	he = (Histent)zshcalloc(sizeof *he);
	if (!hist_ring)
	    hist_ring = he->up = he->down = he;
	else {
	    he->up = hist_ring;
	    he->down = hist_ring->down;
	    hist_ring->down = he->down->up = he;
	    hist_ring = he;
	}
	histlinect++;
    }
    else {
	putoldhistentryontop(0);
	freehistdata(hist_ring, 0);
	he = hist_ring;
    }
    he->histnum = ++curhist;
    if (curline_in_ring)
	linkcurline();
    return he;
}

/* A helper function for hend() */

static int
should_ignore_line(Eprog prog)
{
    if (isset(HISTIGNORESPACE)) {
	if (*chline == ' ' || aliasspaceflag)
	    return 1;
    }

    if (!prog)
	return 0;

    if (isset(HISTNOFUNCTIONS)) {
	Wordcode pc = prog->prog;
	wordcode code = *pc;
	if (wc_code(code) == WC_LIST && WC_LIST_TYPE(code) & Z_SIMPLE
	 && wc_code(pc[2]) == WC_FUNCDEF)
	    return 1;
    }

    if (isset(HISTNOSTORE)) {
	char *b = getjobtext(prog, NULL);
	int saw_builtin;
	if (*b == 'b' && strncmp(b,"builtin ",8) == 0) {
	    b += 8;
	    saw_builtin = 1;
	} else
	    saw_builtin = 0;
	if (*b == 'h' && strncmp(b,"history",7) == 0 && (!b[7] || b[7] == ' ')
	 && (saw_builtin || !shfunctab->getnode(shfunctab,"history")))
	    return 1;
	if (*b == 'r' && (!b[1] || b[1] == ' ')
	 && (saw_builtin || !shfunctab->getnode(shfunctab,"r")))
	    return 1;
	if (*b == 'f' && b[1] == 'c' && b[2] == ' ' && b[3] == '-'
	 && (saw_builtin || !shfunctab->getnode(shfunctab,"fc"))) {
	    b += 3;
	    do {
		if (*++b == 'l')
		    return 1;
	    } while (ialpha(*b));
	}
    }

    return 0;
}

/* say we're done using the history mechanism */

/**/
mod_export int
hend(Eprog prog)
{
    int flag, save = 1;
    char *hf;

    DPUTS(stophist != 2 && !(inbufflags & INP_ALIAS) && !chline,
	  "BUG: chline is NULL in hend()");
    queue_signals();
    if (histdone & HISTFLAG_SETTY)
	settyinfo(&shttyinfo);
    if (!(histactive & HA_NOINC))
	unlinkcurline();
    if (histactive & (HA_NOSTORE|HA_NOINC)) {
	zfree(chline, hlinesz);
	zfree(chwords, chwordlen*sizeof(short));
	chline = NULL;
	histactive = 0;
	unqueue_signals();
	return 1;
    }
    if (hist_ignore_all_dups != isset(HISTIGNOREALLDUPS)
     && (hist_ignore_all_dups = isset(HISTIGNOREALLDUPS)) != 0)
	histremovedups();
    /* For history sharing, lock history file once for both read and write */
    hf = getsparam("HISTFILE");
    if (isset(SHAREHISTORY) && lockhistfile(hf, 0)) {
	readhistfile(hf, 0, HFILE_USE_OPTIONS | HFILE_FAST);
	curline.histnum = curhist+1;
    }
    flag = histdone;
    histdone = 0;
    if (hptr < chline + 1)
	save = 0;
    else {
	*hptr = '\0';
	if (hptr[-1] == '\n') {
	    if (chline[1]) {
		*--hptr = '\0';
	    } else
		save = 0;
	}
	if (chwordpos <= 2)
	    save = 0;
	else if (should_ignore_line(prog))
	    save = -1;
    }
    if (flag & (HISTFLAG_DONE | HISTFLAG_RECALL)) {
	char *ptr;

	ptr = ztrdup(chline);
	if ((flag & (HISTFLAG_DONE | HISTFLAG_RECALL)) == HISTFLAG_DONE) {
	    zputs(ptr, shout);
	    fputc('\n', shout);
	    fflush(shout);
	}
	if (flag & HISTFLAG_RECALL) {
	    zpushnode(bufstack, ptr);
	    save = 0;
	} else
	    zsfree(ptr);
    }
    if (save || *chline == ' ') {
	Histent he;
	for (he = hist_ring; he && he->node.flags & HIST_FOREIGN;
	     he = up_histent(he)) ;
	if (he && he->node.flags & HIST_TMPSTORE) {
	    if (he == hist_ring)
		curline.histnum = curhist--;
	    freehistnode(&he->node);
	}
    }
    if (save) {
	Histent he;
	int newflags;

#ifdef DEBUG
	/* debugging only */
	if (chwordpos%2) {
	    hwend();
	    DPUTS(1, "BUG: uncompleted line in history");
	}
#endif
	/* get rid of pesky \n which we've already nulled out */
	if (chwordpos > 1 && !chline[chwords[chwordpos-2]]) {
	    chwordpos -= 2;
	    /* strip superfluous blanks, if desired */
	    if (isset(HISTREDUCEBLANKS))
		histreduceblanks();
	}
	newflags = save > 0? 0 : HIST_TMPSTORE;
	if ((isset(HISTIGNOREDUPS) || isset(HISTIGNOREALLDUPS)) && save > 0
	 && hist_ring && histstrcmp(chline, hist_ring->node.nam) == 0) {
	    /* This history entry compares the same as the previous.
	     * In case minor changes were made, we overwrite the
	     * previous one with the current one.  This also gets the
	     * timestamp right.  Perhaps, preserve the HIST_OLD flag.
	     */
	    he = hist_ring;
	    newflags |= he->node.flags & HIST_OLD; /* Avoid re-saving */
	    freehistdata(he, 0);
	    curline.histnum = curhist;
	} else
	    he = prepnexthistent();

	he->node.nam = ztrdup(chline);
	he->stim = time(NULL);
	he->ftim = 0L;
	he->node.flags = newflags;

	if ((he->nwords = chwordpos/2)) {
	    he->words = (short *)zalloc(chwordpos * sizeof(short));
	    memcpy(he->words, chwords, chwordpos * sizeof(short));
	}
	if (!(newflags & HIST_TMPSTORE))
	    addhistnode(histtab, he->node.nam, he);
    }
    zfree(chline, hlinesz);
    zfree(chwords, chwordlen*sizeof(short));
    chline = NULL;
    histactive = 0;
    if (isset(SHAREHISTORY)? histfileIsLocked() : isset(INCAPPENDHISTORY))
	savehistfile(hf, 0, HFILE_USE_OPTIONS | HFILE_FAST);
    unlockhistfile(hf); /* It's OK to call this even if we aren't locked */
    unqueue_signals();
    return !(flag & HISTFLAG_NOEXEC || errflag);
}

/* Gives current expansion word if not last word before chwordpos. */

/**/
int hwgetword = -1;

/* begin a word */

/**/
void
ihwbegin(int offset)
{
    if (stophist == 2)
	return;
    if (chwordpos%2)
	chwordpos--;	/* make sure we're on a word start, not end */
    /* If we're expanding an alias, we should overwrite the expansion
     * in the history.
     */
    if ((inbufflags & INP_ALIAS) && !(inbufflags & INP_HIST))
	hwgetword = chwordpos;
    else
	hwgetword = -1;
    chwords[chwordpos++] = hptr - chline + offset;
}

/* add a word to the history List */

/**/
void
ihwend(void)
{
    if (stophist == 2)
	return;
    if (chwordpos%2 && chline) {
	/* end of word reached and we've already begun a word */
	if (hptr > chline + chwords[chwordpos-1]) {
	    chwords[chwordpos++] = hptr - chline;
	    if (chwordpos >= chwordlen) {
		chwords = (short *) realloc(chwords,
					    (chwordlen += 32) * 
					    sizeof(short));
	    }
	    if (hwgetword > -1) {
		/* We want to reuse the current word position */
		chwordpos = hwgetword;
		/* Start from where previous word ended, if possible */
		hptr = chline + chwords[chwordpos ? chwordpos - 1 : 0];
	    }
	} else {
	    /* scrub that last word, it doesn't exist */
	    chwordpos--;
	}
    }
}

/* Go back to immediately after the last word, skipping space. */

/**/
void
histbackword(void)
{
    if (!(chwordpos%2) && chwordpos)
	hptr = chline + chwords[chwordpos-1];
}

/* Get the start and end point of the current history word */

/**/
static void
hwget(char **startptr)
{
    int pos = hwgetword > -1 ? hwgetword : chwordpos - 2;

#ifdef DEBUG
    /* debugging only */
    if (hwgetword == -1 && !chwordpos) {
	/* no words available */
	DPUTS(1, "BUG: hwget() called with no words");
	*startptr = "";
	return;
    } 
    else if (hwgetword == -1 && chwordpos%2) {
	DPUTS(1, "BUG: hwget() called in middle of word");
	*startptr = "";
	return;
    }
#endif

    *startptr = chline + chwords[pos];
    chline[chwords[++pos]] = '\0';
}

/* Replace the current history word with rep, if different */

/**/
void
hwrep(char *rep)
{
    char *start;
    hwget(&start);

    if (!strcmp(rep, start))
	return;
    
    hptr = start;
    chwordpos = (hwgetword > -1) ? hwgetword : chwordpos - 2;
    hwbegin(0);
    qbang = 1;
    while (*rep)
	hwaddc(*rep++);
    hwend();
}

/* Get the entire current line, deleting it in the history. */

/**/
mod_export char *
hgetline(void)
{
    /* Currently only used by pushlineoredit().
     * It's necessary to prevent that from getting too pally with
     * the history code.
     */
    char *ret;

    if (!chline || hptr == chline)
	return NULL;
    *hptr = '\0';
    ret = dupstring(chline);

    /* reset line */
    hptr = chline;
    chwordpos = 0;
    hwgetword = -1;

    return ret;
}

/* get an argument specification */

/**/
static int
getargspec(int argc, int marg, int evset)
{
    int c, ret = -1;

    if ((c = ingetc()) == '0')
	return 0;
    if (idigit(c)) {
	ret = 0;
	while (idigit(c)) {
	    ret = ret * 10 + c - '0';
	    c = ingetc();
	}
	inungetc(c);
    } else if (c == '^')
	ret = 1;
    else if (c == '$')
	ret = argc;
    else if (c == '%') {
	if (evset) {
	    herrflush();
	    zerr("Ambiguous history reference");
	    return -2;
	}
	if (marg == -1) {
	    herrflush();
	    zerr("%% with no previous word matched");
	    return -2;
	}
	ret = marg;
    } else
	inungetc(c);
    return ret;
}

/* do ?foo? search */

/**/
static zlong
hconsearch(char *str, int *marg)
{
    int t1 = 0;
    char *s;
    Histent he;

    for (he = up_histent(hist_ring); he; he = up_histent(he)) {
	if (he->node.flags & HIST_FOREIGN)
	    continue;
	if ((s = strstr(he->node.nam, str))) {
	    int pos = s - he->node.nam;
	    while (t1 < he->nwords && he->words[2*t1] <= pos)
		t1++;
	    *marg = t1 - 1;
	    return he->histnum;
	}
    }
    return -1;
}

/* do !foo search */

/**/
zlong
hcomsearch(char *str)
{
    Histent he;
    int len = strlen(str);

    for (he = up_histent(hist_ring); he; he = up_histent(he)) {
	if (he->node.flags & HIST_FOREIGN)
	    continue;
	if (strncmp(he->node.nam, str, len) == 0)
	    return he->histnum;
    }
    return -1;
}

/* various utilities for : modifiers */

/**/
int
remtpath(char **junkptr)
{
    char *str = strend(*junkptr);

    /* ignore trailing slashes */
    while (str >= *junkptr && IS_DIRSEP(*str))
	--str;
    /* skip filename */
    while (str >= *junkptr && !IS_DIRSEP(*str))
	--str;
    if (str < *junkptr) {
	if (IS_DIRSEP(**junkptr))
	    *junkptr = dupstring ("/");
	else
	    *junkptr = dupstring (".");

	return 0;
    }
    /* repeated slashes are considered like a single slash */
    while (str > *junkptr && IS_DIRSEP(str[-1]))
	--str;
    /* never erase the root slash */
    if (str == *junkptr) {
	++str;
	/* Leading doubled slashes (`//') have a special meaning on cygwin
	   and some old flavor of UNIX, so we do not assimilate them to
	   a single slash.  However a greater number is ok to squeeze. */
	if (IS_DIRSEP(*str) && !IS_DIRSEP(str[1]))
	    ++str;
    }
    *str = '\0';
    return 1;
}

/**/
int
remtext(char **junkptr)
{
    char *str;

    for (str = strend(*junkptr); str >= *junkptr && !IS_DIRSEP(*str); --str)
	if (*str == '.') {
	    *str = '\0';
	    return 1;
	}
    return 0;
}

/**/
int
rembutext(char **junkptr)
{
    char *str;

    for (str = strend(*junkptr); str >= *junkptr && !IS_DIRSEP(*str); --str)
	if (*str == '.') {
	    *junkptr = dupstring(str + 1); /* .xx or xx? */
	    return 1;
	}
    /* no extension */
    *junkptr = dupstring ("");
    return 0;
}

/**/
mod_export int
remlpaths(char **junkptr)
{
    char *str = strend(*junkptr);

    if (IS_DIRSEP(*str)) {
	/* remove trailing slashes */
	while (str >= *junkptr && IS_DIRSEP(*str))
	    --str;
	str[1] = '\0';
    }
    for (; str >= *junkptr; --str)
	if (IS_DIRSEP(*str)) {
	    *str = '\0';
	    *junkptr = dupstring(str + 1);
	    return 1;
	}
    return 0;
}

/*
 * Return modified version of str from the heap with modification
 * according to one of the CASMOD_* types defined in zsh.h; CASMOD_NONE
 * is not handled, for obvious reasons.
 */

/**/
char *
casemodify(char *str, int how)
{
    char *str2 = zhalloc(2 * strlen(str) + 1);
    char *ptr2 = str2;
    int nextupper = 1;

#ifdef MULTIBYTE_SUPPORT
    if (isset(MULTIBYTE)) {
	VARARR(char, mbstr, MB_CUR_MAX);
	mbstate_t ps;

	mb_metacharinit();
	memset(&ps, 0, sizeof(ps));
	while (*str) {
	    wint_t wc;
	    int len = mb_metacharlenconv(str, &wc), mod = 0, len2;
	    /*
	     * wc is set to WEOF if the start of str couldn't be
	     * converted.  Presumably WEOF doesn't match iswlower(), but
	     * better be safe.
	     */
	    if (wc == WEOF) {
		while (len--)
		    *ptr2++ = *str++;
		/* not alphanumeric */
		nextupper = 1;
		continue;
	    }
	    switch (how) {
	    case CASMOD_LOWER:
		if (iswupper(wc)) {
		    wc = towlower(wc);
		    mod = 1;
		}
		break;

	    case CASMOD_UPPER:
		if (iswlower(wc)) {
		    wc = towupper(wc);
		    mod = 1;
		}
		break;

	    case CASMOD_CAPS:
	    default:		/* shuts up compiler */
		if (!iswalnum(wc))
		    nextupper = 1;
		else if (nextupper) {
		    if (iswlower(wc)) {
			wc = towupper(wc);
			mod = 1;
		    }
		    nextupper = 0;
		} else if (iswupper(wc)) {
		    wc = towlower(wc);
		    mod = 1;
		}
		break;
	    }
	    if (mod && (len2 = wcrtomb(mbstr, wc, &ps)) > 0) {
		char *mbptr;

		for (mbptr = mbstr; mbptr < mbstr + len2; mbptr++) {
		    if (imeta(STOUC(*mbptr))) {
			*ptr2++ = Meta;
			*ptr2++ = *mbptr ^ 32;
		    } else
			*ptr2++ = *mbptr;
		}
		str += len;
	    } else {
		while (len--)
		    *ptr2++ = *str++;
	    }
	}
    }
    else
#endif
	while (*str) {
	    int c;
	    if (*str == Meta) {
		c = str[1] ^ 32;
		str += 2;
	    } else
		c = *str++;
	    switch (how) {
	    case CASMOD_LOWER:
		if (isupper(c))
		    c = tolower(c);
		break;

	    case CASMOD_UPPER:
		if (islower(c))
		    c = toupper(c);
		break;

	    case CASMOD_CAPS:
	    default:		/* shuts up compiler */
		if (!ialnum(c))
		    nextupper = 1;
		else if (nextupper) {
		    if (islower(c))
			c = toupper(c);
		    nextupper = 0;
		} else if (isupper(c))
		    c = tolower(c);
		break;
	    }
	    if (imeta(c)) {
		*ptr2++ = Meta;
		*ptr2++ = c ^ 32;
	    } else
		*ptr2++ = c;
	}
    *ptr2 = '\0';
    return str2;
}


/*
 * Substitute "in" for "out" in "*strptr" and update "*strptr".
 * If "gbal", do global substitution.
 *
 * This returns a result from the heap.  There seems to have
 * been some confusion on this point.
 */

/**/
int
subst(char **strptr, char *in, char *out, int gbal)
{
    char *str = *strptr, *substcut, *sptr;
    int off, inlen, outlen;

    if (!*in)
	in = str, gbal = 0;

    if (isset(HISTSUBSTPATTERN)) {
	int fl = SUB_LONG|SUB_REST|SUB_RETFAIL;
	char *oldin = in;
	if (gbal)
	    fl |= SUB_GLOBAL;
	if (*in == '#' || *in == Pound) {
	    /* anchor at head, flag needed if SUB_END is also set */
	    fl |= SUB_START;
	    in++;
	}
	if (*in == '%') {
	    /* anchor at tail */
	    in++;
	    fl |= SUB_END;
	}
	if (in == oldin) {
	    /* no anchor, substring match */
	    fl |= SUB_SUBSTR;
	}
	if (in == str)
	    in = dupstring(in);
	if (parse_subst_string(in) || errflag)
	    return 1;
	if (parse_subst_string(out) || errflag)
	    return 1;
	singsub(&in);
	if (getmatch(strptr, in, fl, 1, out))
	    return 0;
    } else {
	if ((substcut = (char *)strstr(str, in))) {
	    inlen = strlen(in);
	    sptr = convamps(out, in, inlen);
	    outlen = strlen(sptr);

	    do {
		*substcut = '\0';
		off = substcut - *strptr + outlen;
		substcut += inlen;
		*strptr = zhtricat(*strptr, sptr, substcut);
		str = (char *)*strptr + off;
	    } while (gbal && (substcut = (char *)strstr(str, in)));

	    return 0;
	}
    }

    return 1;
}

/**/
static char *
convamps(char *out, char *in, int inlen)
{
    char *ptr, *ret, *pp;
    int slen, sdup = 0;

    for (ptr = out, slen = 0; *ptr; ptr++, slen++)
	if (*ptr == '\\')
	    ptr++, sdup = 1;
	else if (*ptr == '&')
	    slen += inlen - 1, sdup = 1;
    if (!sdup)
	return out;
    ret = pp = (char *) zhalloc(slen + 1);
    for (ptr = out; *ptr; ptr++)
	if (*ptr == '\\')
	    *pp++ = *++ptr;
	else if (*ptr == '&') {
	    strcpy(pp, in);
	    pp += inlen;
	} else
	    *pp++ = *ptr;
    *pp = '\0';
    return ret;
}

/**/
mod_export void
checkcurline(Histent he)
{
    if (he->histnum == curhist && (histactive & HA_ACTIVE)) {
	curline.node.nam = chline;
	curline.nwords = chwordpos/2;
	curline.words = chwords;
    }
}

/**/
mod_export Histent
quietgethist(int ev)
{
    return gethistent(ev, GETHIST_EXACT);
}

/**/
static Histent
gethist(int ev)
{
    Histent ret;

    ret = quietgethist(ev);
    if (!ret) {
	herrflush();
	zerr("no such event: %d", ev);
    }
    return ret;
}

/**/
static char *
getargs(Histent elist, int arg1, int arg2)
{
    short *words = elist->words;
    int pos1, nwords = elist->nwords;

    if (arg2 < arg1 || arg1 >= nwords || arg2 >= nwords) {
	/* remember, argN is indexed from 0, nwords is total no. of words */
	herrflush();
	zerr("no such word in event");
	return NULL;
    }

    pos1 = words[2*arg1];
    return dupstrpfx(elist->node.nam + pos1, words[2*arg2+1] - pos1);
}

/**/
int
quote(char **tr)
{
    char *ptr, *rptr, **str = (char **)tr;
    int len = 3;
    int inquotes = 0;

    for (ptr = *str; *ptr; ptr++, len++)
	if (*ptr == '\'') {
	    len += 3;
	    if (!inquotes)
		inquotes = 1;
	    else
		inquotes = 0;
	} else if (inblank(*ptr) && !inquotes && ptr[-1] != '\\')
	    len += 2;
    ptr = *str;
    *str = rptr = (char *) zhalloc(len);
    *rptr++ = '\'';
    for (; *ptr; ptr++)
	if (*ptr == '\'') {
	    if (!inquotes)
		inquotes = 1;
	    else
		inquotes = 0;
	    *rptr++ = '\'';
	    *rptr++ = '\\';
	    *rptr++ = '\'';
	    *rptr++ = '\'';
	} else if (inblank(*ptr) && !inquotes && ptr[-1] != '\\') {
	    *rptr++ = '\'';
	    *rptr++ = *ptr;
	    *rptr++ = '\'';
	} else
	    *rptr++ = *ptr;
    *rptr++ = '\'';
    *rptr++ = 0;
    str[1] = NULL;
    return 0;
}

/**/
static int
quotebreak(char **tr)
{
    char *ptr, *rptr, **str = (char **)tr;
    int len = 3;

    for (ptr = *str; *ptr; ptr++, len++)
	if (*ptr == '\'')
	    len += 3;
	else if (inblank(*ptr))
	    len += 2;
    ptr = *str;
    *str = rptr = (char *) zhalloc(len);
    *rptr++ = '\'';
    for (; *ptr;)
	if (*ptr == '\'') {
	    *rptr++ = '\'';
	    *rptr++ = '\\';
	    *rptr++ = '\'';
	    *rptr++ = '\'';
	    ptr++;
	} else if (inblank(*ptr)) {
	    *rptr++ = '\'';
	    *rptr++ = *ptr++;
	    *rptr++ = '\'';
	} else
	    *rptr++ = *ptr++;
    *rptr++ = '\'';
    *rptr++ = '\0';
    return 0;
}

/* read an arbitrary amount of data into a buffer until stop is found */

#if 0 /**/
char *
hdynread(int stop)
{
    int bsiz = 256, ct = 0, c;
    char *buf = (char *)zalloc(bsiz), *ptr;

    ptr = buf;
    while ((c = ingetc()) != stop && c != '\n' && !lexstop) {
	if (c == '\\')
	    c = ingetc();
	*ptr++ = c;
	if (++ct == bsiz) {
	    buf = realloc(buf, bsiz *= 2);
	    ptr = buf + ct;
	}
    }
    *ptr = 0;
    if (c == '\n') {
	inungetc('\n');
	zerr("delimiter expected");
	zfree(buf, bsiz);
	return NULL;
    }
    return buf;
}
#endif

/**/
static char *
hdynread2(int stop)
{
    int bsiz = 256, ct = 0, c;
    char *buf = (char *)zalloc(bsiz), *ptr;

    ptr = buf;
    while ((c = ingetc()) != stop && c != '\n' && !lexstop) {
	if (c == '\n') {
	    inungetc(c);
	    break;
	}
	if (c == '\\')
	    c = ingetc();
	*ptr++ = c;
	if (++ct == bsiz) {
	    buf = realloc(buf, bsiz *= 2);
	    ptr = buf + ct;
	}
    }
    *ptr = 0;
    if (c == '\n')
	inungetc('\n');
    return buf;
}

/**/
void
inithist(void)
{
    createhisttable();
}

/**/
void
resizehistents(void)
{
    if (histlinect > histsiz) {
	/* The reason we don't just call freehistnode(hist_ring->down) is
	 * so that we can honor the HISTEXPIREDUPSFIRST setting. */
	putoldhistentryontop(0);
	freehistnode(&hist_ring->node);
	while (histlinect > histsiz) {
	    putoldhistentryontop(1);
	    freehistnode(&hist_ring->node);
	}
    }
}

/* Remember the last line in the history file so we can find it again. */
static struct histfile_stats {
    char *text;
    time_t stim, mtim;
    off_t fpos, fsiz;
    zlong next_write_ev;
} lasthist;

static struct histsave {
    struct histfile_stats lasthist;
    char *histfile;
    HashTable histtab;
    Histent hist_ring;
    zlong curhist;
    zlong histlinect;
    zlong histsiz;
    zlong savehistsiz;
    int locallevel;
} *histsave_stack;
static int histsave_stack_size = 0;
static int histsave_stack_pos = 0;

static zlong histfile_linect;

static int
readhistline(int start, char **bufp, int *bufsiz, FILE *in)
{
    char *buf = *bufp;
    if (fgets(buf + start, *bufsiz - start, in)) {
	int len = start + strlen(buf + start);
	if (len == start)
	    return -1;
	if (buf[len - 1] != '\n') {
	    if (!feof(in)) {
		if (len < (*bufsiz) - 1)
		    return -1;
		*bufp = zrealloc(buf, 2 * (*bufsiz));
		*bufsiz = 2 * (*bufsiz);
		return readhistline(len, bufp, bufsiz, in);
	    }
	}
	else {
	    buf[len - 1] = '\0';
	    if (len > 1 && buf[len - 2] == '\\') {
		buf[--len - 1] = '\n';
		if (!feof(in))
		    return readhistline(len, bufp, bufsiz, in);
	    }
	}
	return len;
    }
    return 0;
}

/**/
void
readhistfile(char *fn, int err, int readflags)
{
    char *buf, *start = NULL;
    FILE *in;
    Histent he;
    time_t stim, ftim, tim = time(NULL);
    off_t fpos;
    short *wordlist;
    struct stat sb;
    int nwordpos, nwordlist, bufsiz;
    int searching, newflags, l;

    if (!fn && !(fn = getsparam("HISTFILE")))
	return;
    if (readflags & HFILE_FAST) {
	if (stat(unmeta(fn), &sb) < 0
	 || (lasthist.fsiz == sb.st_size && lasthist.mtim == sb.st_mtime)
	 || !lockhistfile(fn, 0))
	    return;
	lasthist.fsiz = sb.st_size;
	lasthist.mtim = sb.st_mtime;
    }
    else if (!lockhistfile(fn, 1))
	return;
    if ((in = fopen(unmeta(fn), "r"))) {
	nwordlist = 64;
	wordlist = (short *)zalloc(nwordlist*sizeof(short));
	bufsiz = 1024;
	buf = zalloc(bufsiz);

	if (readflags & HFILE_FAST && lasthist.text) {
	    if (lasthist.fpos < lasthist.fsiz) {
		fseek(in, lasthist.fpos, 0);
		searching = 1;
	    }
	    else {
		histfile_linect = 0;
		searching = -1;
	    }
	} else
	    searching = 0;

	newflags = HIST_OLD | HIST_READ;
	if (readflags & HFILE_FAST)
	    newflags |= HIST_FOREIGN;
	if (readflags & HFILE_SKIPOLD
	 || (hist_ignore_all_dups && newflags & hist_skip_flags))
	    newflags |= HIST_MAKEUNIQUE;
	while (fpos = ftell(in), (l = readhistline(0, &buf, &bufsiz, in))) {
	    char *pt = buf;

	    if (l < 0) {
		zerr("corrupt history file %s", fn);
		break;
	    }
	    if (*pt == ':') {
		pt++;
		stim = zstrtol(pt, NULL, 0);
		for (; *pt != ':' && *pt; pt++);
		if (*pt) {
		    pt++;
		    ftim = zstrtol(pt, NULL, 0);
		    for (; *pt != ';' && *pt; pt++);
		    if (*pt)
			pt++;
		} else
		    ftim = stim;
	    } else {
		if (*pt == '\\' && pt[1] == ':')
		    pt++;
		stim = ftim = 0;
	    }

	    if (searching) {
		if (searching > 0) {
		    if (stim == lasthist.stim
		     && histstrcmp(pt, lasthist.text) == 0)
			searching = 0;
		    else {
			fseek(in, 0, 0);
			histfile_linect = 0;
			searching = -1;
		    }
		    continue;
		}
		else if (stim < lasthist.stim) {
		    histfile_linect++;
		    continue;
		}
		searching = 0;
	    }

	    if (readflags & HFILE_USE_OPTIONS) {
		histfile_linect++;
		lasthist.fpos = fpos;
		lasthist.stim = stim;
	    }

	    he = prepnexthistent();
	    he->node.nam = ztrdup(pt);
	    he->node.flags = newflags;
	    if ((he->stim = stim) == 0)
		he->stim = he->ftim = tim;
	    else if (ftim < stim)
		he->ftim = stim + ftim;
	    else
		he->ftim = ftim;

	    /* Divide up the words.  We don't know how it lexes,
	       so just look for white-space.
	       */
	    nwordpos = 0;
	    start = pt;
	    do {
		while (inblank(*pt))
		    pt++;
		if (*pt) {
		    if (nwordpos >= nwordlist)
			wordlist = (short *) realloc(wordlist,
					(nwordlist += 64)*sizeof(short));
		    wordlist[nwordpos++] = pt - start;
		    while (*pt && !inblank(*pt))
			pt++;
		    wordlist[nwordpos++] = pt - start;
		}
	    } while (*pt);

	    he->nwords = nwordpos/2;
	    if (he->nwords) {
		he->words = (short *)zalloc(nwordpos*sizeof(short));
		memcpy(he->words, wordlist, nwordpos*sizeof(short));
	    } else
		he->words = (short *)NULL;
	    addhistnode(histtab, he->node.nam, he);
	    if (he->node.flags & HIST_DUP) {
		freehistnode(&he->node);
		curhist--;
	    }
	}
	if (start && readflags & HFILE_USE_OPTIONS) {
	    zsfree(lasthist.text);
	    lasthist.text = ztrdup(start);
	}
	zfree(wordlist, nwordlist*sizeof(short));
	zfree(buf, bufsiz);

	fclose(in);
    } else if (err)
	zerr("can't read history file %s", fn);

    unlockhistfile(fn);
}

/**/
void
savehistfile(char *fn, int err, int writeflags)
{
    char *t, *tmpfile, *start = NULL;
    FILE *out;
    Histent he;
    zlong xcurhist = curhist - !!(histactive & HA_ACTIVE);
    int extended_history = isset(EXTENDEDHISTORY);

    if (!interact || savehistsiz <= 0 || !hist_ring
     || (!fn && !(fn = getsparam("HISTFILE"))))
	return;
    if (writeflags & HFILE_FAST) {
	he = gethistent(lasthist.next_write_ev, GETHIST_DOWNWARD);
	while (he && he->node.flags & HIST_OLD) {
	    lasthist.next_write_ev = he->histnum + 1;
	    he = down_histent(he);
	}
	if (!he || !lockhistfile(fn, 0))
	    return;
	if (histfile_linect > savehistsiz + savehistsiz / 5)
	    writeflags &= ~HFILE_FAST;
    }
    else {
	if (!lockhistfile(fn, 1))
	    return;
	he = hist_ring->down;
    }
    if (writeflags & HFILE_USE_OPTIONS) {
	if (isset(APPENDHISTORY) || isset(INCAPPENDHISTORY)
	 || isset(SHAREHISTORY))
	    writeflags |= HFILE_APPEND | HFILE_SKIPOLD;
	else
	    histfile_linect = 0;
	if (isset(HISTSAVENODUPS))
	    writeflags |= HFILE_SKIPDUPS;
	if (isset(SHAREHISTORY))
	    extended_history = 1;
    }
    if (writeflags & HFILE_APPEND) {
	tmpfile = NULL;
	out = fdopen(open(unmeta(fn),
			O_CREAT | O_WRONLY | O_APPEND | O_NOCTTY, 0600), "a");
    } else if (!isset(HISTSAVEBYCOPY)) {
	tmpfile = NULL;
	out = fdopen(open(unmeta(fn),
			 O_CREAT | O_WRONLY | O_TRUNC | O_NOCTTY, 0600), "w");
    } else {
	tmpfile = bicat(unmeta(fn), ".new");
	if (unlink(tmpfile) < 0 && errno != ENOENT)
	    out = NULL;
	else {
	    struct stat sb;
	    int old_exists = stat(unmeta(fn), &sb) == 0;

	    if (old_exists && sb.st_uid != geteuid()) {
		free(tmpfile);
		tmpfile = NULL; /* Avoid an error about HISTFILE.new */
		out = NULL;
	    } else
		out = fdopen(open(tmpfile, O_CREAT | O_WRONLY | O_EXCL, 0600), "w");

#ifdef HAVE_FCHMOD
	    if (old_exists && out) {
#ifdef HAVE_FCHOWN
		fchown(fileno(out), sb.st_uid, sb.st_gid);
#endif
		fchmod(fileno(out), sb.st_mode);
	    }
#endif
	}
    }
    if (out) {
	for (; he && he->histnum <= xcurhist; he = down_histent(he)) {
	    if ((writeflags & HFILE_SKIPDUPS && he->node.flags & HIST_DUP)
	     || (writeflags & HFILE_SKIPFOREIGN && he->node.flags & HIST_FOREIGN)
	     || he->node.flags & HIST_TMPSTORE)
		continue;
	    if (writeflags & HFILE_SKIPOLD) {
		if (he->node.flags & HIST_OLD)
		    continue;
		he->node.flags |= HIST_OLD;
		if (writeflags & HFILE_USE_OPTIONS)
		    lasthist.next_write_ev = he->histnum + 1;
	    }
	    if (writeflags & HFILE_USE_OPTIONS) {
		lasthist.fpos = ftell(out);
		lasthist.stim = he->stim;
		histfile_linect++;
	    }
	    t = start = he->node.nam;
	    if (extended_history) {
		fprintf(out, ": %ld:%ld;", (long)he->stim,
			he->ftim? (long)(he->ftim - he->stim) : 0L);
	    } else if (*t == ':')
		fputc('\\', out);

	    for (; *t; t++) {
		if (*t == '\n')
		    fputc('\\', out);
		fputc(*t, out);
	    }
	    fputc('\n', out);
	}
	if (start && writeflags & HFILE_USE_OPTIONS) {
	    struct stat sb;
	    fflush(out);
	    if (fstat(fileno(out), &sb) == 0) {
		lasthist.fsiz = sb.st_size;
		lasthist.mtim = sb.st_mtime;
	    }
	    zsfree(lasthist.text);
	    lasthist.text = ztrdup(start);
	}
	fclose(out);
	if (tmpfile) {
	    if (rename(tmpfile, unmeta(fn)) < 0)
		zerr("can't rename %s.new to $HISTFILE", fn);
	    free(tmpfile);
	}

	if (writeflags & HFILE_SKIPOLD
	 && !(writeflags & (HFILE_FAST | HFILE_NO_REWRITE))) {
	    int remember_histactive = histactive;

	    /* Zeroing histactive avoids unnecessary munging of curline. */
	    histactive = 0;
	    /* The NULL leaves HISTFILE alone, preserving fn's value. */
	    pushhiststack(NULL, savehistsiz, savehistsiz, -1);

	    hist_ignore_all_dups |= isset(HISTSAVENODUPS);
	    readhistfile(fn, err, 0);
	    hist_ignore_all_dups = isset(HISTIGNOREALLDUPS);
	    if (histlinect)
		savehistfile(fn, err, 0);

	    pophiststack();
	    histactive = remember_histactive;
	}
    } else if (err) {
	if (tmpfile) {
	    zerr("can't write history file %s.new", fn);
	    free(tmpfile);
	} else
	    zerr("can't write history file %s", fn);
    }

    unlockhistfile(fn);
}

static int lockhistct;

/**/
int
lockhistfile(char *fn, int keep_trying)
{
    int ct = lockhistct;

    if (!fn && !(fn = getsparam("HISTFILE")))
	return 0;
    if (!lockhistct++) {
	struct stat sb;
	int fd;
	char *lockfile;
#ifdef HAVE_LINK
	char *tmpfile;
#endif

	lockfile = bicat(unmeta(fn), ".LOCK");
#ifdef HAVE_LINK
	if ((fd = gettempfile(fn, 0, &tmpfile)) >= 0) {
	    FILE *out = fdopen(fd, "w");
	    if (out) {
		fprintf(out, "%ld %s\n", (long)getpid(), getsparam("HOST"));
		fclose(out);
	    } else
		close(fd);
	    while (link(tmpfile, lockfile) < 0) {
		if (errno != EEXIST || !keep_trying)
		    ;
		else if (stat(lockfile, &sb) < 0) {
		    if (errno == ENOENT)
			continue;
		}
		else {
		    if (time(NULL) - sb.st_mtime < 10)
			sleep(1);
		    else
			unlink(lockfile);
		    continue;
		}
		lockhistct--;
		break;
	    }
	    unlink(tmpfile);
	    free(tmpfile);
	}
#else /* not HAVE_LINK */
	while ((fd = open(lockfile, O_WRONLY|O_CREAT|O_EXCL, 0644)) < 0) {
	    if (errno != EEXIST || !keep_trying)
		break;
	    if (stat(lockfile, &sb) < 0) {
		if (errno == ENOENT)
		    continue;
		break;
	    }
	    if (time(NULL) - sb.st_mtime < 10)
		sleep(1);
	    else
		unlink(lockfile);
	}
	if (fd < 0)
	    lockhistct--;
	else {
	    FILE *out = fdopen(fd, "w");
	    if (out) {
		fprintf(out, "%ld %s\n", (long)mypid, getsparam("HOST"));
		fclose(out);
	    } else
		close(fd);
	}
#endif /* not HAVE_LINK */
	free(lockfile);
    }
    return ct != lockhistct;
}

/* Unlock the history file if this corresponds to the last nested lock
 * request.  If we don't have the file locked, just return.
 */

/**/
void
unlockhistfile(char *fn)
{
    if (!fn && !(fn = getsparam("HISTFILE")))
	return;
    if (--lockhistct) {
	if (lockhistct < 0)
	    lockhistct = 0;
    }
    else {
	char *lockfile;
	fn = unmeta(fn);
	lockfile = zalloc(strlen(fn) + 5 + 1);
	sprintf(lockfile, "%s.LOCK", fn);
	unlink(lockfile);
	free(lockfile);
    }
}

/**/
int
histfileIsLocked(void)
{
    return lockhistct > 0;
}

/*
 * Get the words in the current buffer. Using the lexer. 
 *
 * As far as I can make out, this is a gross hack based on a gross hack.
 * When analysing lines from within zle, we tweak the metafied line
 * positions (zlemetall and zlemetacs) directly in the lexer.  That's
 * bad enough, but this function appears to be designed to be called
 * from outside zle, pretending to be in zle and calling out, so
 * we set zlemetall and zlemetacs locally and copy the current zle line,
 * which may not even be valid at this point.
 *
 * However, I'm so confused it could simply be baking Bakewell tarts.
 */

/**/
mod_export LinkList
bufferwords(LinkList list, char *buf, int *index)
{
    int num = 0, cur = -1, got = 0, ne = noerrs;
    int owb = wb, owe = we, oadx = addedx, ozp = zleparse, onc = nocomments;
    int ona = noaliases, ocs = zlemetacs, oll = zlemetall;
    char *p;

    if (!list)
	list = newlinklist();

    zleparse = 1;
    addedx = 0;
    noerrs = 1;
    lexsave();
    if (buf) {
	int l = strlen(buf);

	p = (char *) zhalloc(l + 2);
	memcpy(p, buf, l);
	p[l] = ' ';
	p[l + 1] = '\0';
	inpush(p, 0, NULL);
	zlemetall = strlen(p) ;
	zlemetacs = zlemetall + 1;
	nocomments = 1;
    } else {
	int ll, cs;
	char *linein;

	if (zlegetlineptr) {
	    linein = (char *)zlegetlineptr(&ll, &cs);
	} else {
	    linein = ztrdup("");
	    ll = cs = 0;
	}
	zlemetall = ll + 1; /* length of line plus space added below */
	zlemetacs = cs;

	if (!isfirstln && chline) {
	    p = (char *) zhalloc(hptr - chline + ll + 2);
	    memcpy(p, chline, hptr - chline);
	    memcpy(p + (hptr - chline), linein, ll);
	    p[(hptr - chline) + ll] = ' ';
	    p[(hptr - chline) + zlemetall] = '\0';
	    inpush(p, 0, NULL);

	    /*
	     * advance line length and character position over
	     * prepended string.
	     */
	    zlemetall += hptr - chline;
	    zlemetacs += hptr - chline;
	} else {
	    p = (char *) zhalloc(ll + 2);
	    memcpy(p, linein, ll);
	    p[ll] = ' ';
	    p[zlemetall] = '\0';
	    inpush(p, 0, NULL);
	}
	zsfree(linein);
    }
    if (zlemetacs)
	zlemetacs--;
    strinbeg(0);
    noaliases = 1;
    do {
	if (incond)
	    incond = 1 + (tok != DINBRACK && tok != INPAR &&
			  tok != DBAR && tok != DAMPER &&
			  tok != BANG);
	ctxtlex();
	if (tok == ENDINPUT || tok == LEXERR)
	    break;
	if (tokstr && *tokstr) {
	    untokenize((p = dupstring(tokstr)));
	    addlinknode(list, p);
	    num++;
	} else if (buf) {
	    if (IS_REDIROP(tok) && tokfd >= 0) {
		char b[20];

		sprintf(b, "%d%s", tokfd, tokstrings[tok]);
		addlinknode(list, dupstring(b));
		num++;
	    } else if (tok != NEWLIN) {
		addlinknode(list, dupstring(tokstrings[tok]));
		num++;
	    }
	}
	if (!got && !zleparse) {
	    got = 1;
	    cur = num - 1;
	}
    } while (tok != ENDINPUT && tok != LEXERR);
    if (buf && tok == LEXERR && tokstr && *tokstr) {
	int plen;
	untokenize((p = dupstring(tokstr)));
	plen = strlen(p);
	/*
	 * Strip the space we added for lexing but which won't have
	 * been swallowed by the lexer because we aborted early.
	 * The test is paranoia.
	 */
	if (plen && p[plen-1] == ' ' && (plen == 1 || p[plen-2] != Meta))
	    p[plen - 1] = '\0';
	addlinknode(list, p);
	num++;
    }
    if (cur < 0 && num)
	cur = num - 1;
    noaliases = ona;
    strinend();
    inpop();
    errflag = 0;
    zleparse = ozp;
    nocomments = onc;
    noerrs = ne;
    lexrestore();
    zlemetacs = ocs;
    zlemetall = oll;
    wb = owb;
    we = owe;
    addedx = oadx;

    if (index)
	*index = cur;

    return list;
}

/* Move the current history list out of the way and prepare a fresh history
 * list using hf for HISTFILE, hs for HISTSIZE, and shs for SAVEHIST.  If
 * the hf value is an empty string, HISTFILE will be unset from the new
 * environment; if it is NULL, HISTFILE will not be changed, not even by the
 * pop function (this functionality is used internally to rewrite the current
 * history file without affecting pointers into the environment).
 */

/**/
int
pushhiststack(char *hf, zlong hs, zlong shs, int level)
{
    struct histsave *h;
    int curline_in_ring = (histactive & HA_ACTIVE) && hist_ring == &curline;

    if (histsave_stack_pos == histsave_stack_size) {
	histsave_stack_size += 5;
	histsave_stack = zrealloc(histsave_stack,
			    histsave_stack_size * sizeof (struct histsave));
    }

    if (curline_in_ring)
	unlinkcurline();

    h = &histsave_stack[histsave_stack_pos++];

    h->lasthist = lasthist;
    if (hf) {
	if ((h->histfile = getsparam("HISTFILE")) != NULL && *h->histfile)
	    h->histfile = ztrdup(h->histfile);
	else
	    h->histfile = "";
    } else
	h->histfile = NULL;
    h->histtab = histtab;
    h->hist_ring = hist_ring;
    h->curhist = curhist;
    h->histlinect = histlinect;
    h->histsiz = histsiz;
    h->savehistsiz = savehistsiz;
    h->locallevel = level;

    memset(&lasthist, 0, sizeof lasthist);
    if (hf) {
	if (*hf)
	    setsparam("HISTFILE", ztrdup(hf));
	else
	    unsetparam("HISTFILE");
    }
    hist_ring = NULL;
    curhist = histlinect = 0;
    histsiz = hs;
    savehistsiz = shs;
    inithist(); /* sets histtab */

    if (curline_in_ring)
	linkcurline();

    return histsave_stack_pos;
}


/**/
int
pophiststack(void)
{
    struct histsave *h;
    int curline_in_ring = (histactive & HA_ACTIVE) && hist_ring == &curline;

    if (histsave_stack_pos == 0)
	return 0;

    if (curline_in_ring)
	unlinkcurline();

    deletehashtable(histtab);
    zsfree(lasthist.text);

    h = &histsave_stack[--histsave_stack_pos];

    lasthist = h->lasthist;
    if (h->histfile) {
	if (*h->histfile)
	    setsparam("HISTFILE", h->histfile);
	else
	    unsetparam("HISTFILE");
    }
    histtab = h->histtab;
    hist_ring = h->hist_ring;
    curhist = h->curhist;
    histlinect = h->histlinect;
    histsiz = h->histsiz;
    savehistsiz = h->savehistsiz;

    if (curline_in_ring)
	linkcurline();

    return histsave_stack_pos + 1;
}

/* If pop_through > 0, pop all array items >= the 1-relative index value.
 * If pop_through <= 0, pop (-1)*pop_through levels off the stack.
 * If the (new) top of stack is from a higher locallevel, auto-pop until
 * it is not.
 */

/**/
int
saveandpophiststack(int pop_through, int writeflags)
{
    if (pop_through <= 0) {
	pop_through += histsave_stack_pos + 1;
	if (pop_through <= 0)
	    pop_through = 1;
    }
    while (pop_through > 1
     && histsave_stack[pop_through-2].locallevel > locallevel)
	pop_through--;
    if (histsave_stack_pos < pop_through)
	return 0;
    do {
	if (!nohistsave)
	    savehistfile(NULL, 1, writeflags);
	pophiststack();
    } while (histsave_stack_pos >= pop_through);
    return 1;
}
