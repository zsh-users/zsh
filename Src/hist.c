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

/* != 0 means history substitution is turned off */
 
/**/
int stophist;
 
/* this line began with a space, so junk it if HISTIGNORESPACE is on */
 
/**/
int spaceflag;
 
/* if != 0, we are expanding the current line */

/**/
int expanding;

/* these are used to modify the cursor position during expansion */

/**/
int excs, exlast;

/*
 * Current history event number
 *
 * Note on curhist: with history inactive, this points to the
 * last line actually added to the history list.  With history active,
 * the line does not get added to the list until hend(), if at all.
 * However, curhist is incremented to reflect the current line anyway.
 * Thus if the line is not added to the list, curhist must be
 * decremented in hend().
 */
 
/**/
int curhist;

/* number of history entries */
 
/**/
int histentct;
 
/* array of history entries */
 
/**/
Histent histentarr;
 
/* capacity of history lists */
 
/**/
int histsiz;
 
/* if = 1, we have performed history substitution on the current line *
 * if = 2, we have used the 'p' modifier                              */
 
/**/
int histdone;
 
/* state of the history mechanism */
 
/**/
int histactive;

/* Bits of histactive variable */
#define HA_ACTIVE	(1<<0)	/* History mechanism is active */
#define HA_NOSTORE	(1<<1)	/* Don't store the line when finished */
#define HA_JUNKED	(1<<2)	/* Last history line was already junked */
#define HA_NOINC	(1<<3)	/* Don't store, curhist not incremented */

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
char *hptr;
 
/* the current history line */
 
/**/
char *chline;

/* true if the last character returned by hgetc was an escaped bangchar *
 * if it is set and NOBANGHIST is unset hwaddc escapes bangchars        */

/**/
int qbang;
 
/* max size of histline */
 
/**/
int hlinesz;
 
/* default event (usually curhist-1, that is, "!!") */
 
static int defev;
 
/* add a character to the current history word */

/**/
void
hwaddc(int c)
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

	    chline = realloc(chline, hlinesz = oldsiz + 16);
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
addtoline(int c)
{
    if (! expanding || lexstop)
	return;
    if (qbang && c == bangchar && stophist < 2) {
	exlast--;
	spaceinline(1);
	line[cs++] = '\\';
    }
    if (excs > cs) {
	excs += 1 + inbufct - exlast;
	if (excs < cs)
	    /* this case could be handled better but it is    *
	     * so rare that it does not worth it              */
	    excs = cs;
    }
    exlast = inbufct;
    spaceinline(1);
    line[cs++] = itok(c) ? ztokens[c - Pound] : c;
}

/**/
int
hgetc(void)
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
	 * (inbufflags && INP_HIST) and history is not disabled           *
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
    while (!lexstop && inbufct)
	hwaddc(ingetc());
}

/* extract :s/foo/bar/ delimiters and arguments */

/**/
static int
getsubsargs(char *subline)
{
    int del;
    char *ptr1, *ptr2;

    del = ingetc();
    ptr1 = hdynread2(del);
    if (!ptr1)
	return 1;
    ptr2 = hdynread2(del);
    if (strlen(ptr1)) {
	zsfree(hsubl);
	hsubl = ptr1;
    }
    zsfree(hsubr);
    hsubr = ptr2;
    if (hsubl && !strstr(subline, hsubl)) {
	herrflush();
	zerr("substitution failed", NULL, 0);
	return 1;
    }
    return 0;
}

/* Get the maximum no. of words for a history entry. */

/**/
static int
getargc(Histent ehist)
{
    return ehist->nwords ? ehist->nwords-1 : 0;
}

/* Perform history substitution, returning the next character afterwards. */

/**/
static int
histsubchar(int c)
{
    int ev, farg, evset = -1, larg, argc, cflag = 0, bflag = 0;
    static int mev = -1, marg = -1;
    char buf[256], *ptr;
    char *sline;
    Histent ehist;

    /* look, no goto's */
    if (isfirstch && c == hatchar) {
	/* Line begins ^foo^bar */
	isfirstch = 0;
	inungetc(hatchar);
	if (!(ehist = gethist(defev))
	    || !(sline = getargs(ehist, 0, getargc(ehist)))
	    || getsubsargs(sline) || !hsubl)
	    return -1;
	subst(&sline, hsubl, hsubr, 0);
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
		zerr("no such event: %s", buf, 0);
		return -1;
	    }
	} else {
	    int t0;

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
	    if (!*buf)
		if (c != '%') {
		    if (isset(CSHJUNKIEHISTORY))
			ev = curhist - 1;
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
	    } else if ((t0 = atoi(buf))) {
		ev = (t0 < 0) ? curhist + t0 : t0;
		evset = 1;
	    } else if ((unsigned)*buf == bangchar) {
		ev = curhist - 1;
		evset = 1;
	    } else if (*buf == '#') {
		ev = curhist;
		evset = 1;
	    } else if ((ev = hcomsearch(buf)) == -1) {
		herrflush();
		zerr("event not found: %s", buf, 0);
		return -1;
	    } else
		evset = 1;
	}

	/* get the event */

	if (!(ehist = gethist(defev = ev)))
	    return -1;

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
		    zerr("Ambiguous history reference", NULL, 0);
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
	    if (larg == -2)
		return -1;
	    if (farg != -1)
		cflag = 0;
	    c = ingetc();
	    if (c == '*') {
		cflag = 0;
		larg = argc;
	    } else if (c == '-') {
		cflag = 0;
		larg = getargspec(argc, marg, evset);
		if (larg == -2)
		    return -1;
		if (larg == -1)
		    larg = argc - 1;
	    } else
		inungetc(c);
	}
	if (farg == -1)
	    farg = 0;
	if (larg == -1)
	    larg = argc;
	if (!(sline = getargs(ehist, farg, larg)))
	    return -1;
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
	    }
	    switch (c) {
	    case 'p':
		histdone = HISTFLAG_DONE | HISTFLAG_NOEXEC;
		break;
	    case 'h':
		if (!remtpath(&sline)) {
		    herrflush();
		    zerr("modifier failed: h", NULL, 0);
		    return -1;
		}
		break;
	    case 'e':
		if (!rembutext(&sline)) {
		    herrflush();
		    zerr("modifier failed: e", NULL, 0);
		    return -1;
		}
		break;
	    case 'r':
		if (!remtext(&sline)) {
		    herrflush();
		    zerr("modifier failed: r", NULL, 0);
		    return -1;
		}
		break;
	    case 't':
		if (!remlpaths(&sline)) {
		    herrflush();
		    zerr("modifier failed: t", NULL, 0);
		    return -1;
		}
		break;
	    case 's':
		if (getsubsargs(sline))
		    return -1; /* fall through */
	    case '&':
		if (hsubl && hsubr)
		    subst(&sline, hsubl, hsubr, gbal);
		else {
		    herrflush();
		    zerr("no previous substitution", NULL, 0);
		    return -1;
		}
		break;
	    case 'q':
		quote(&sline);
		break;
	    case 'x':
		quotebreak(&sline);
		break;
	    case 'l':
		downcase(&sline);
		break;
	    case 'u':
		upcase(&sline);
		break;
	    default:
		herrflush();
		zerr("illegal modifier: %c", NULL, c);
		return -1;
	    }
	} else {
	    if (c != '}' || !bflag)
		inungetc(c);
	    if (c != '}' && bflag) {
		zerr("'}' expected", NULL, 0);
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

/**/
void
hungetc(int c)
{
    int doit = 1;

    while (!lexstop) {
	if (hptr[-1] != (char) c && stophist < 4 &&
	    hptr > chline + 1 && hptr[-1] == '\n' && hptr[-2] == '\\')
	    hungetc('\n'), hungetc('\\');

	if (expanding) {
	    cs--;
	    ll--;
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
void
strinbeg(void)
{
    strin++;
    hbegin();
    lexinit();
}

/* done reading a string */

/**/
void
strinend(void)
{
    hend();
    DPUTS(!strin, "BUG: strinend() called without strinbeg()");
    strin--;
    isfirstch = 1;
    histdone = 0;
}

/* initialize the history mechanism */

/**/
void
hbegin(void)
{
    Histent curhistent;

    isfirstln = isfirstch = 1;
    errflag = histdone = spaceflag = 0;
    stophist = (!interact || unset(BANGHIST) || unset(SHINSTDIN)) << 1;
    chline = hptr = zcalloc(hlinesz = 16);
    chwords = zalloc((chwordlen = 16)*sizeof(short));
    chwordpos = 0;

    if (histactive & HA_JUNKED)
	curhist--;
    curhistent = gethistent(curhist);
    if (!curhistent->ftim)
	curhistent->ftim = time(NULL);
    histactive = HA_ACTIVE;
    if (interact && isset(SHINSTDIN) && !strin) {
	attachtty(mypgrp);
	defev = curhist;
	curhist++;
    } else
	histactive |= HA_NOINC;
}

/* compare current line with history entry using only text in words */

/**/
static int
histcmp(Histent he)
{
    int kword, lword;
    int nwords = chwordpos/2;

    /* If the history entry came from a file, the words were not
     * divided by the lexer so we have to resort to strcmp.
     */
    if (he->flags & HIST_READ)
	return strcmp(he->text, chline);

    if (nwords != he->nwords)
	return 1;

    for (kword = 0; kword < 2*nwords; kword += 2)
	if ((lword = chwords[kword+1]-chwords[kword])
	    != he->words[kword+1]-he->words[kword] ||
	    memcmp(he->text+he->words[kword], chline+chwords[kword], lword))
	    return 1;

    return 0;
}

/**/
void
histreduceblanks(void)
{
    int i, len, pos, needblank;

    for (i = 0, len = 0; i < chwordpos; i += 2) {
	len += chwords[i+1] - chwords[i]
	     + (i > 0 && chwords[i] > chwords[i-1]);
    }
    if (chline[len] == '\0')
	return;

    for (i = 0, pos = 0; i < chwordpos; i += 2) {
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

/* say we're done using the history mechanism */

/**/
int
hend(void)
{
    int flag, save = 1;

    DPUTS(!chline, "BUG: chline is NULL in hend()");
    if (histactive & (HA_NOSTORE|HA_NOINC)) {
	zfree(chline, hlinesz);
	zfree(chwords, chwordlen*sizeof(short));
	chline = NULL;
	if (!(histactive & HA_NOINC))
	    curhist--;
	histactive = 0;
	return 1;
    }
    flag = histdone;
    histdone = 0;
    if (hptr < chline + 1)
	save = 0;
    else {
	*hptr = '\0';
	if (hptr[-1] == '\n')
	    if (chline[1]) {
		*--hptr = '\0';
	    } else
		save = 0;
	if (!*chline || !strcmp(chline, "\n") ||
	    (isset(HISTIGNORESPACE) && spaceflag))
	    save = 0;
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
	    PERMALLOC {
		pushnode(bufstack, ptr);
	    } LASTALLOC;
	    save = 0;
	} else
	    zsfree(ptr);
    }
    if (save) {
	Histent he;
	int keepflags = 0;

#ifdef DEBUG
	/* debugging only */
	if (chwordpos%2) {
	    hwend();
	    DPUTS(1, "BUG: uncompleted line in history");
	}
#endif
	/* get rid of pesky \n which we've already nulled out */
	if (!chline[chwords[chwordpos-2]])
	    chwordpos -= 2;
	/* strip superfluous blanks, if desired */
	if (isset(HISTREDUCEBLANKS))
	    histreduceblanks();

	if (isset(HISTIGNOREDUPS) && (he = gethistent(curhist - 1))
	 && he->text && !histcmp(he)) {
	    /* This history entry compares the same as the previous.
	     * In case minor changes were made, we overwrite the
	     * previous one with the current one.  This also gets
	     * the timestamp right.  However, keep the old flags.
	     */
	    keepflags = he->flags;
	    curhist--;
	}

	he =  gethistent(curhist);
	zsfree(he->text);
	he->text = ztrdup(chline);
	if (he->nwords)
	    zfree(he->words, he->nwords*2*sizeof(short));
	he->stim = time(NULL);
	he->ftim = 0L;
	he->flags = keepflags;

	if ((he->nwords = chwordpos/2)) {
	    he->words = (short *)zalloc(chwordpos * sizeof(short));
	    memcpy(he->words, chwords, chwordpos * sizeof(short));
	}
    } else
	curhist--;
    zfree(chline, hlinesz);
    zfree(chwords, chwordlen*sizeof(short));
    chline = NULL;
    histactive = 0;
    return !(flag & HISTFLAG_NOEXEC || errflag);
}

/* remove the current line from the history List */

/**/
void
remhist(void)
{
    if (!(histactive & HA_ACTIVE)) {
	if (!(histactive & HA_JUNKED)) {
	    /* make sure this doesn't show up when we do firsthist() */
	    Histent he = gethistent(curhist);
	    zsfree(he->text);
	    he->text = NULL;
	    histactive |= HA_JUNKED;
	    /* curhist-- is delayed until the next hbegin() */
	}
    } else
	histactive |= HA_NOSTORE;
}

/* Gives current expansion word if not last word before chwordpos. */

/**/
int hwgetword = -1;

/* begin a word */

/**/
void
hwbegin(int offset)
{
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
hwend(void)
{
    if (chwordpos%2 && chline) {
	/* end of word reached and we've already begun a word */
	if (hptr > chline + chwords[chwordpos-1]) {
	    chwords[chwordpos++] = hptr - chline;
	    if (chwordpos >= chwordlen) {
		chwords = (short *) realloc(chwords,
					    (chwordlen += 16)*sizeof(short));
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
char *
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
	    zerr("Ambiguous history reference", NULL, 0);
	    return -2;
	}
	if (marg == -1) {
	    herrflush();
	    zerr("%% with no previous word matched", NULL, 0);
	    return -2;
	}
	ret = marg;
    } else
	inungetc(c);
    return ret;
}

/* do ?foo? search */

/**/
static int
hconsearch(char *str, int *marg)
{
    int t0, t1 = 0;
    char *s;
    Histent he;

    for (t0 = curhist - 1; (he = quietgethist(t0)); t0--)
	if ((s = strstr(he->text, str))) {
	    int pos = s - he->text;
	    while (t1 < he->nwords && he->words[2*t1] <= pos)
		t1++;
	    *marg = t1 - 1;
	    return t0;
	}
    return -1;
}

/* do !foo search */

/**/
int
hcomsearch(char *str)
{
    int t0;
    char *hs;

    for (t0 = curhist - 1; (hs = quietgetevent(t0)); t0--)
	if (!strncmp(hs, str, strlen(str)))
	    return t0;
    return -1;
}

/* various utilities for : modifiers */

/**/
int
remtpath(char **junkptr)
{
    char *str = *junkptr, *remcut;

    if ((remcut = strrchr(str, '/'))) {
	if (str != remcut)
	    *remcut = '\0';
	else
	    str[1] = '\0';
	return 1;
    }
    return 0;
}

/**/
int
remtext(char **junkptr)
{
    char *str = *junkptr, *remcut;

    if ((remcut = strrchr(str, '.')) && remcut != str) {
	*remcut = '\0';
	return 1;
    }
    return 0;
}

/**/
int
rembutext(char **junkptr)
{
    char *str = *junkptr, *remcut;

    if ((remcut = strrchr(str, '.')) && remcut != str) {
	*junkptr = dupstring(remcut + 1);	/* .xx or xx? */
	return 1;
    }
    return 0;
}

/**/
int
remlpaths(char **junkptr)
{
    char *str = *junkptr, *remcut;

    if ((remcut = strrchr(str, '/'))) {
	*remcut = '\0';
	*junkptr = dupstring(remcut + 1);
	return 1;
    }
    return 0;
}

/**/
int
makeuppercase(char **junkptr)
{
    char *str = *junkptr;

    for (; *str; str++)
	*str = tuupper(*str);
    return 1;
}

/**/
int
makelowercase(char **junkptr)
{
    char *str = *junkptr;

    for (; *str; str++)
	*str = tulower(*str);
    return 1;
}

/**/
int
makecapitals(char **junkptr)
{
    char *str = *junkptr;

    for (; *str;) {
	for (; *str && !ialnum(*str); str++);
	if (*str)
	    *str = tuupper(*str), str++;
	for (; *str && ialnum(*str); str++)
	    *str = tulower(*str);
    }
    return 1;
}

/**/
void
subst(char **strptr, char *in, char *out, int gbal)
{
    char *str = *strptr, *instr = *strptr, *substcut, *sptr, *oldstr;
    int off, inlen, outlen;

    if (!*in)
	in = str, gbal = 0;
    if (!(substcut = (char *)strstr(str, in)))
	return;
    inlen = strlen(in);
    sptr = convamps(out, in, inlen);
    outlen = strlen(sptr);

    do {
	*substcut = '\0';
	off = substcut - *strptr + outlen;
	substcut += inlen;
	*strptr = tricat(oldstr = *strptr, sptr, substcut);
	if (oldstr != instr)
	    zsfree(oldstr);
	str = (char *)*strptr + off;
    } while (gbal && (substcut = (char *)strstr(str, in)));
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
    ret = pp = (char *)alloc(slen + 1);
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
struct histent *
quietgethist(int ev)
{
    static struct histent storehist;

    if (ev < firsthist() || ev > curhist)
	return NULL;
    if (ev == curhist && (histactive & HA_ACTIVE)) {
	/* The current history line has not been stored.  Build it up
	 * from other variables.
	 */
	storehist.text = chline;
	storehist.nwords = chwordpos/2;
	storehist.words = chwords;

	return &storehist;
    } else
	return gethistent(ev);
}

/**/
char *
quietgetevent(int ev)
{
    Histent ent = quietgethist(ev);

    return ent ? ent->text : NULL;
}

/**/
static Histent
gethist(int ev)
{
    Histent ret;

    ret = quietgethist(ev);
    if (!ret) {
	herrflush();
	zerr("no such event: %d", NULL, ev);
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
	zerr("no such word in event", NULL, 0);
	return NULL;
    }

    pos1 = words[2*arg1];
    return dupstrpfx(elist->text + pos1, words[2*arg2+1] - pos1);
}

/**/
void
upcase(char **x)
{
    char *pp = *(char **)x;

    for (; *pp; pp++)
	*pp = tuupper(*pp);
}

/**/
void
downcase(char **x)
{
    char *pp = *(char **)x;

    for (; *pp; pp++)
	*pp = tulower(*pp);
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
    *str = rptr = (char *)alloc(len);
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
    *str = rptr = (char *)alloc(len);
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

#if 0
/* read an arbitrary amount of data into a buffer until stop is found */

/**/
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
	zerr("delimiter expected", NULL, 0);
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
    histentct = histsiz;
    histentarr = (Histent) zcalloc(histentct * sizeof *histentarr);
}

/**/
void
resizehistents(void)
{
    int newentct, t0, t1, firstlex;
    Histent newarr;

    newentct = histsiz;
    newarr = (Histent) zcalloc(newentct * sizeof *newarr);
    firstlex = curhist - histsiz + 1;
    t0 = firsthist();
    if (t0 < curhist - newentct)
	t0 = curhist - newentct;
    t1 = t0 % newentct;
    for (; t0 <= curhist; t0++) {
	newarr[t1] = *gethistent(t0);
	if (t0 < firstlex) {
	    zsfree(newarr[t1].text);
	    newarr[t1].text = NULL;
	}
	t1++;
	if (t1 == newentct)
	    t1 = 0;
    }
    free(histentarr);
    histentarr = newarr;
    histentct = newentct;
}

/**/
void
readhistfile(char *s, int err)
{
    char *buf;
    FILE *in;
    Histent ent;
    time_t tim = time(NULL);
    short *wordlist;
    int nwordpos, nwordlist, bufsiz;

    if (!s)
	return;
    if ((in = fopen(unmeta(s), "r"))) {
	nwordlist = 16;
	wordlist = (short *)zalloc(nwordlist*sizeof(short));
	bufsiz = 1024;
	buf = zalloc(bufsiz);

	while (fgets(buf, bufsiz, in)) {
	    int l = strlen(buf);
	    char *pt, *start;

	    while (l) {
		while (buf[l - 1] != '\n') {
		    buf = zrealloc(buf, 2 * bufsiz);
		    bufsiz = 2 * bufsiz;
		    if (!fgets(buf + l, bufsiz - l, in)) {
			l++;
			break;
		    }
		    l = strlen(buf);
		}
		buf[l - 1] = '\0';
		if (l > 1 && buf[l - 2] == '\\') {
		    buf[l - 2] = '\n';
		    fgets(buf + l - 1, bufsiz - (l - 1), in);
		    l = strlen(buf);
		} else
		    break;
	    }

	    ent = gethistent(++curhist);
	    pt = buf;
	    if (*pt == ':') {
		pt++;
		ent->stim = zstrtol(pt, NULL, 0);
		for (; *pt != ':' && *pt; pt++);
		if (*pt) {
		    pt++;
		    ent->ftim = zstrtol(pt, NULL, 0);
		    for (; *pt != ';' && *pt; pt++);
		    if (*pt)
			pt++;
		} else {
		    ent->ftim = tim;
		}
		if (ent->stim == 0)
		    ent->stim = tim;
		if (ent->ftim == 0)
		    ent->ftim = tim;
	    } else {
		ent->ftim = ent->stim = tim;
	    }

	    zsfree(ent->text);
	    ent->text = ztrdup(pt);
	    ent->flags = HIST_OLD|HIST_READ;
	    if (ent->nwords)
		zfree(ent->words, ent->nwords*2*sizeof(short));

	    /* Divide up the words.  We don't know how it lexes,
	       so just look for spaces.
	       */
	    nwordpos = 0;
	    start = pt;
	    do {
		while (*pt == ' ')
		    pt++;
		if (*pt) {
		    if (nwordpos >= nwordlist)
			wordlist = (short *) realloc(wordlist,
					(nwordlist += 16)*sizeof(short));
		    wordlist[nwordpos++] = pt - start;
		    while (*pt && *pt != ' ')
			pt++;
		    wordlist[nwordpos++] = pt - start;
		}
	    } while (*pt);

	    ent->nwords = nwordpos/2;
	    if (ent->nwords) {
		ent->words = (short *)zalloc(nwordpos*sizeof(short));
		memcpy(ent->words, wordlist, nwordpos*sizeof(short));
	    } else
		ent->words = (short *)NULL;
	}
	fclose(in);

	zfree(wordlist, nwordlist*sizeof(short));
	zfree(buf, bufsiz);
    } else if (err)
	zerr("can't read history file", s, 0);
}

/**/
void
savehistfile(char *s, int err, int app)
{
    char *t;
    FILE *out;
    int ev;
    Histent ent;
    int savehist = getiparam("SAVEHIST");

    if (!s || !interact || savehist <= 0)
	return;
    ev = curhist - savehist + 1;
    if (ev < firsthist())
	ev = firsthist();
    if (app & 1)
	out = fdopen(open(unmeta(s),
		     O_CREAT | O_WRONLY | O_APPEND | O_NOCTTY, 0600), "a");
    else
	out = fdopen(open(unmeta(s),
		     O_CREAT | O_WRONLY | O_TRUNC | O_NOCTTY, 0600), "w");
    if (out) {
	for (; ev <= curhist - !!(histactive & HA_ACTIVE); ev++) {
	    ent = gethistent(ev);
	    if (app & 2) {
		if (ent->flags & HIST_OLD)
		    continue;
		ent->flags |= HIST_OLD;
	    }
	    t = ent->text;
	    if (isset(EXTENDEDHISTORY)) {
		fprintf(out, ": %ld:%ld;",
			(long)ent->stim,
			(long)ent->ftim);
	    } else if (*t == ':')
		fputc('\\', out);

	    for (; *t; t++) {
		if (*t == '\n')
		    fputc('\\', out);
		fputc(*t, out);
	    }
	    fputc('\n', out);
	}
	fclose(out);

	if (app & 2 && (out = fopen(unmeta(s), "r"))) {
	    char **store, buf[1024], **ptr;
	    int i, l, histnum = 0;

	    store = (char **)zcalloc((savehist + 1) * sizeof *store);
	    while (fgets(buf, sizeof(buf), out)) {
		char *t;

		if (store[i = histnum % savehist])
		    free(store[i]);
		store[i] = ztrdup(buf);
		l = strlen(buf);
		if (l > 1) {
		    t = store[i] + l;
		    while ((t[-1] != '\n' ||
			    (t[-1] == '\n' && t[-2] == '\\')) &&
			   fgets(buf, sizeof(buf), out)) {
			l += strlen(buf);
			store[i] = zrealloc(store[i], l + 1);
			t = store[i] + l;
			strcat(store[i], buf);
		    }
		}
		histnum++;
	    }
	    fclose(out);
	    if ((out = fdopen(open(unmeta(s),
			    O_WRONLY | O_TRUNC | O_NOCTTY, 0600), "w"))) {
		if (histnum < savehist)
		    for (i = 0; i < histnum; i++)
			fprintf(out, "%s", store[i]);
		else
		    for (i = histnum; i < histnum + savehist; i++)
			fprintf(out, "%s", store[i % savehist]);
		fclose(out);
	    }
	    for (ptr = store; *ptr; ptr++)
		zsfree(*ptr);
	    free(store);
	}
    } else if (err)
	zerr("can't write history file %s", s, 0);
}

/**/
int
firsthist(void)
{
    int ev;
    Histent ent;

    ev = curhist - histentct + 1;
    if (ev < 1)
	ev = 1;
    do {
	ent = gethistent(ev);
	if (ent->text)
	    break;
	ev++;
    }
    while (ev < curhist);
    return ev;
}

