/*
 * zle_hist.c - history editing
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
#include "zle_hist.pro"

/* Column position of vi ideal cursor.  -1 if it is unknown -- most *
 * movements and changes do this.                                   */

/**/
int lastcol;

/* current history line number */

/**/
int histline;

/* Previous search string use in an incremental search */

/**/
char *previous_search = NULL;

/**/
int previous_search_len = 0;

#define ZLETEXT(X) ((X)->zle_text ? (X)->zle_text : (X)->text)

/**/
void
remember_edits(void)
{
    Histent ent = quietgethist(histline);
    if (ent && metadiffer(ZLETEXT(ent), (char *) line, ll)) {
	zsfree(ent->zle_text);
	ent->zle_text = metafy((char *) line, ll, META_DUP);
    }
}

/**/
void
forget_edits(void)
{
    Histent he;

    for (he = hist_ring; he; he = up_histent(he)) {
	zsfree(he->zle_text);
	he->zle_text = NULL;
    }
}

/**/
int
uphistory(char **args)
{
    int nodups = isset(HISTIGNOREDUPS);
    if (!zle_goto_hist(histline, -zmult, nodups) && isset(HISTBEEP))
	return 1;
    return 0;
}

/**/
static int
upline(void)
{
    int n = zmult;

    if (n < 0) {
	zmult = -zmult;
	n = downline();
	zmult = -zmult;
	return n;
    }
    if (lastcol == -1)
	lastcol = cs - findbol();
    cs = findbol();
    while (n) {
	if (!cs)
	    break;
	cs--;
	cs = findbol();
	n--;
    }
    if (!n) {
	int x = findeol();

	if ((cs += lastcol) >= x) {
	    cs = x;
	    if (cs > findbol() && invicmdmode())
		cs--;
	}
    }
    return n;
}

/**/
int
uplineorhistory(char **args)
{
    int ocs = cs;
    int n = upline();
    if (n) {
	int m = zmult, ret;

	cs = ocs;
	if (virangeflag || !(zlereadflags & ZLRF_HISTORY))
	    return 1;
	zmult = n;
	ret = uphistory(args);
	zmult = m;
	return ret;
    }
    return 0;
}

/**/
int
viuplineorhistory(char **args)
{
    int col = lastcol;
    uplineorhistory(args);
    lastcol = col;
    return vifirstnonblank(args);
}

/**/
int
uplineorsearch(char **args)
{
    int ocs = cs;
    int n = upline();
    if (n) {
	int m = zmult, ret;

	cs = ocs;
	if (virangeflag || !(zlereadflags & ZLRF_HISTORY))
	    return 1;
	zmult = n;
	ret = historysearchbackward(args);
	zmult = m;
	return ret;
    }
    return 0;
}

/**/
static int
downline(void)
{
    int n = zmult;

    if (n < 0) {
	zmult = -zmult;
	n = upline();
	zmult = -zmult;
	return n;
    }
    if (lastcol == -1)
	lastcol = cs - findbol();
    while (n) {
	int x = findeol();

	if (x == ll)
	    break;
	cs = x + 1;
	n--;
    }
    if (!n) {
	int x = findeol();

	if ((cs += lastcol) >= x) {
	    cs = x;
	    if (cs > findbol() && invicmdmode())
		cs--;
	}
    }
    return n;
}

/**/
int
downlineorhistory(char **args)
{
    int ocs = cs;
    int n = downline();
    if (n) {
	int m = zmult, ret;

	cs = ocs;
	if (virangeflag || !(zlereadflags & ZLRF_HISTORY))
	    return 1;
	zmult = n;
	ret = downhistory(args);
	zmult = m;
	return ret;
    }
    return 0;
}

/**/
int
vidownlineorhistory(char **args)
{
    int col = lastcol;
    downlineorhistory(args);
    lastcol = col;
    return vifirstnonblank(zlenoargs);
}

/**/
int
downlineorsearch(char **args)
{
    int ocs = cs;
    int n = downline();
    if (n) {
	int m = zmult, ret;

	cs = ocs;
	if (virangeflag || !(zlereadflags & ZLRF_HISTORY))
	    return 1;
	zmult = n;
	ret = historysearchforward(args);
	zmult = m;
	return ret;
    }
    return 0;
}

/**/
int
acceptlineanddownhistory(char **args)
{
    Histent he = quietgethist(histline);

    if (he && (he = movehistent(he, 1, HIST_FOREIGN))) {
	zpushnode(bufstack, ztrdup(he->text));
	stackhist = he->histnum;
    }
    done = 1;
    return 0;
}

/**/
int
downhistory(char **args)
{
    int nodups = isset(HISTIGNOREDUPS);
    if (!zle_goto_hist(histline, zmult, nodups) && isset(HISTBEEP))
	return 1;
    return 0;
}

static int histpos, srch_hl, srch_cs = -1;
static char *srch_str;

/**/
int
historysearchbackward(char **args)
{
    Histent he;
    int n = zmult, hp;
    char *s, *str;

    if (zmult < 0) {
	int ret;
	zmult = -n;
	ret = historysearchforward(args);
	zmult = n;
	return ret;
    }
    if ((str = *args))
	hp = strlen(str);
    else {
	if (histline == curhist || histline != srch_hl || cs != srch_cs ||
	    mark != 0 || memcmp(srch_str, line, histpos) != 0) {
	    zfree(srch_str, histpos);
	    for (histpos = 0; histpos < ll && !iblank(line[histpos]); histpos++) ;
	    if (histpos < ll)
		histpos++;
	    srch_str = zalloc(histpos);
	    memcpy(srch_str, line, histpos);
	}
	str = srch_str;
	hp = histpos;
    }
    if (!(he = quietgethist(histline)))
	return 1;
    while ((he = movehistent(he, -1, hist_skip_flags))) {
	if (isset(HISTFINDNODUPS) && he->flags & HIST_DUP)
	    continue;
	s = ZLETEXT(he);
	if (metadiffer(s, str, hp) < 0 &&
	    (*args || metadiffer(s, str, ll))) {
	    if (--n <= 0) {
		zle_setline(he);
		srch_hl = histline;
		srch_cs = cs;
		return 0;
	    }
	}
    }
    return 1;
}

/**/
int
historysearchforward(char **args)
{
    Histent he;
    int n = zmult, hp;
    char *s, *str;

    if (zmult < 0) {
	int ret;
	zmult = -n;
	ret = historysearchbackward(args);
	zmult = n;
	return ret;
    }
    if ((str = *args))
	hp = strlen(str);
    else {
	if (histline == curhist || histline != srch_hl || cs != srch_cs ||
	    mark != 0 || memcmp(srch_str, line, histpos) != 0) {
	    zfree(srch_str, histpos);
	    for (histpos = 0; histpos < ll && !iblank(line[histpos]); histpos++) ;
	    if (histpos < ll)
		histpos++;
	    srch_str = zalloc(histpos);
	    memcpy(srch_str, line, histpos);
	}
	str = srch_str;
	hp = histpos;
    }
    if (!(he = quietgethist(histline)))
	return 1;
    while ((he = movehistent(he, 1, hist_skip_flags))) {
	if (isset(HISTFINDNODUPS) && he->flags & HIST_DUP)
	    continue;
	s = ZLETEXT(he);
	if (metadiffer(s, str, hp) < (he->histnum == curhist) &&
	    (*args || metadiffer(s, str, ll))) {
	    if (--n <= 0) {
		zle_setline(he);
		srch_hl = histline;
		srch_cs = cs;
		return 0;
	    }
	}
    }
    return 1;
}

/**/
int
beginningofbufferorhistory(char **args)
{
    if (findbol())
	cs = 0;
    else
	return beginningofhistory(args);
    return 0;
}

/**/
int
beginningofhistory(char **args)
{
    if (!zle_goto_hist(firsthist(), 0, 0) && isset(HISTBEEP))
	return 1;
    return 0;
}

/**/
int
endofbufferorhistory(char **args)
{
    if (findeol() != ll)
	cs = ll;
    else
	return endofhistory(args);
    return 0;
}

/**/
int
endofhistory(char **args)
{
    zle_goto_hist(curhist, 0, 0);
    return 0;
}

/**/
int
insertlastword(char **args)
{
    int n, nwords, histstep = -1, wordpos = 0, deleteword = 0;
    char *s, *t;
    Histent he = NULL;
    LinkList l = NULL;
    LinkNode node;

    static char *lastinsert;
    static int lasthist, lastpos, lastlen;
    int evhist, save;

    /*
     * If we have at least one argument, the first is the history
     * step.  The default is -1 (go back).  Repeated calls take
     * a step in this direction.  A value of 0 is allowed and doesn't
     * move the line.
     *
     * If we have two arguments, the second is the position of
     * the word to extract, 1..N.  The default is to use the
     * numeric argument, or the last word if that is not set.
     *
     * If we have three arguments, we reset the history pointer to
     * the current history event before applying the history step.
     */
    if (*args)
    {
	histstep = (int)zstrtol(*args, NULL, 10);
	if (*++args)
	{
	    wordpos = (int)zstrtol(*args, NULL, 10);
	    if (*++args)
		lasthist = curhist;
	}
    }

    if (lastinsert && lastlen &&
	lastpos <= cs &&
	lastlen == cs - lastpos &&
	memcmp(lastinsert, (char *)&line[lastpos], lastlen) == 0)
	deleteword = 1;
    else
	lasthist = curhist;
    evhist = histstep ? addhistnum(lasthist, histstep, HIST_FOREIGN) :
	lasthist;

    if (evhist == curhist) {
	/*
	 * The line we are currently editing.  If we are going to
	 * replace an existing word, delete the old one now to avoid
	 * confusion.
	 */
	if (deleteword) {
	    int pos = cs;
	    cs = lastpos;
	    foredel(pos - cs);
	    /*
	     * Mark that this has been deleted.
	     * For consistency with history lines, we really ought to
	     * insert it back if the current command later fails. But
	     * - we can't be bothered
	     * - the problem that this can screw up going to other
	     *   lines in the history because we don't update
	     *   the history line isn't really relevant
	     * - you can see what you're copying, dammit, so you
	     *   shouldn't make errors.
	     * Of course, I could have implemented it in the time
	     * it took to say why I haven't.
	     */
	    deleteword = 0;
	}
	/*
	 * Can only happen fail if the line is empty, I hope.
	 * In that case, we don't need to worry about restoring
	 * a deleted word, because that can only have come
	 * from a non-empty line.  I think.
	 */
	if (!(l = bufferwords(NULL, NULL, NULL)))
	    return 1;
	nwords = countlinknodes(l);
    } else {
	/* Some stored line. */
	if (!(he = quietgethist(evhist)) || !he->nwords)
	    return 1;
	nwords = he->nwords;
    }
    if (wordpos) {
	n = (wordpos > 0) ? wordpos : nwords + wordpos + 1;
    } else if (zmult > 0) {
	n = nwords - (zmult - 1);
    } else {
	n = 1 - zmult;
    }
    if (n < 1 || n > nwords) {
	/*
	 * We can't put in the requested word, but we did find the
	 * history entry, so we remember the position in the history
	 * list.  This avoids getting stuck on a history line with
	 * fewer words than expected.  The cursor location cs
	 * has not changed, and lastinsert is still valid.
	 */
	lasthist = evhist;
	return 1;
    }
    /*
     * Only remove the old word from the command line if we have
     * successfully found a new one to insert.
     */
    if (deleteword > 0) {
	int pos = cs;
	cs = lastpos;
	foredel(pos - cs);
    }
    if (lastinsert) {
	zfree(lastinsert, lastlen);
	lastinsert = NULL;
    }
    if (l) {
	for (node = firstnode(l); --n; incnode(node))
	    ;
	s = (char *)getdata(node);
	t = s + strlen(s);
    } else {
	s = he->text + he->words[2*n-2];
	t = he->text + he->words[2*n-1];
    }

    save = *t;
    *t = '\0';			/* ignore trailing whitespace */
    lasthist = evhist;
    lastpos = cs;
    lastlen = t - s;
    lastinsert = zalloc(t - s);
    memcpy(lastinsert, s, lastlen);
    n = zmult;
    zmult = 1;
    doinsert(s);
    zmult = n;
    *t = save;
    return 0;
}

/**/
void
zle_setline(Histent he)
{
    remember_edits();
    mkundoent();
    histline = he->histnum;
    setline(ZLETEXT(he));
    setlastline();
    clearlist = 1;
}

/**/
int
setlocalhistory(char **args)
{
    if (zmod.flags & MOD_MULT) {
	hist_skip_flags = zmult? HIST_FOREIGN : 0;
    } else {
	hist_skip_flags ^= HIST_FOREIGN;
    }
    return 0;
}

/**/
int
zle_goto_hist(int ev, int n, int skipdups)
{
    Histent he = quietgethist(ev);
    if (!he || !(he = movehistent(he, n, hist_skip_flags)))
	return 1;
    if (skipdups && n) {
	n = n < 0? -1 : 1;
	while (he && !metadiffer(ZLETEXT(he), (char *) line, ll))
	    he = movehistent(he, n, hist_skip_flags);
    }
    if (!he)
	return 0;
    zle_setline(he);
    return 1;
}

/**/
int
pushline(char **args)
{
    int n = zmult;

    if (n < 0)
	return 1;
    zpushnode(bufstack, metafy((char *) line, ll, META_DUP));
    while (--n)
	zpushnode(bufstack, ztrdup(""));
    stackcs = cs;
    *line = '\0';
    ll = cs = 0;
    clearlist = 1;
    return 0;
}

/**/
int
pushlineoredit(char **args)
{
    int ics, ret;
    unsigned char *s;
    char *hline = hgetline();

    if (zmult < 0)
	return 1;
    if (hline && *hline) {
	ics = ztrlen(hline);
	sizeline(ics + ll + 1);
	for (s = line + ll; --s >= line; *(s + ics) = *s);
	for (s = line; *hline; hline++)
	    *s++ = *hline == Meta ? *++hline ^ 32 : *hline;
	ll += ics;
	cs += ics;
    }
    ret = pushline(args);
    if (!isfirstln)
	errflag = done = 1;
    clearlist = 1;
    return ret;
}

/**/
int
pushinput(char **args)
{
    int i, ret;

    if (zmult < 0)
	return 1;
    zmult += i = !isfirstln;
    ret = pushlineoredit(args);
    zmult -= i;
    return ret;
}

/**/
int
getline(char **args)
{
    char *s = (char *)getlinknode(bufstack);

    if (!s) {
	return 1;
    } else {
	int cc;

	unmetafy(s, &cc);
	spaceinline(cc);
	memcpy((char *)line + cs, s, cc);
	cs += cc;
	free(s);
	clearlist = 1;
    }
    return 0;
}

/**/
int
historyincrementalsearchbackward(char **args)
{
    doisearch(args, -1);
    return 0;
}

/**/
int
historyincrementalsearchforward(char **args)
{
    doisearch(args, 1);
    return 0;
}

static struct isrch_spot {
    int hl;			/* This spot's histline */
    unsigned short pos;		/* The search position in our metafied str */
    unsigned short cs;		/* The visible search position to the user */
    unsigned short len;		/* The search string's length */
    unsigned short flags;	/* This spot's flags */
#define ISS_FAILING	1
#define ISS_FORWARD	2
} *isrch_spots;

static int max_spot = 0;

/**/
void
free_isrch_spots(void)
{
    zfree(isrch_spots, max_spot * sizeof(*isrch_spots));
}

/**/
static void
set_isrch_spot(int num, int hl, int pos, int cs, int len, int dir, int nomatch)
{
    if (num >= max_spot) {
	if (!isrch_spots) {
	    isrch_spots = (struct isrch_spot*)
			    zalloc((max_spot = 64) * sizeof *isrch_spots);
	} else {
	    isrch_spots = (struct isrch_spot*)realloc((char*)isrch_spots,
			    (max_spot += 64) * sizeof *isrch_spots);
	}
    }

    isrch_spots[num].hl = hl;
    isrch_spots[num].pos = (unsigned short)pos;
    isrch_spots[num].cs = (unsigned short)cs;
    isrch_spots[num].len = (unsigned short)len;
    isrch_spots[num].flags = (dir > 0? ISS_FORWARD : 0)
			   + (nomatch? ISS_FAILING : 0);
}

/**/
static void
get_isrch_spot(int num, int *hlp, int *posp, int *csp, int *lenp, int *dirp, int *nomatch)
{
    *hlp = isrch_spots[num].hl;
    *posp = (int)isrch_spots[num].pos;
    *csp = (int)isrch_spots[num].cs;
    *lenp = (int)isrch_spots[num].len;
    *dirp = (isrch_spots[num].flags & ISS_FORWARD)? 1 : -1;
    *nomatch = (isrch_spots[num].flags & ISS_FAILING);
}

#define ISEARCH_PROMPT		"failing XXX-i-search: "
#define NORM_PROMPT_POS		8
#define FIRST_SEARCH_CHAR	(NORM_PROMPT_POS + 14)

/**/
static void
doisearch(char **args, int dir)
{
    char *s, *ibuf = zhalloc(80), *sbuf = ibuf + FIRST_SEARCH_CHAR;
    int sbptr = 0, top_spot = 0, pos, sibuf = 80;
    int nomatch = 0, skip_line = 0, skip_pos = 0;
    int odir = dir, sens = zmult == 1 ? 3 : 1;
    int hl = histline, savekeys = -1, feep = 0;
    Thingy cmd;
    char *okeymap;
    Histent he;

    if (!(he = quietgethist(hl)))
	return;

    clearlist = 1;

    if (*args) {
	int len;
	char *arg;
	savekeys = kungetct;
	arg = getkeystring(*args, &len, 2, NULL);
	ungetkeys(arg, len);
    }

    strcpy(ibuf, ISEARCH_PROMPT);
    memcpy(ibuf + NORM_PROMPT_POS, (dir == 1) ? "fwd" : "bck", 3);
    remember_edits();
    okeymap = ztrdup(curkeymapname);
    s = ZLETEXT(he);
    selectkeymap("main", 1);
    pos = metalen(s, cs);
    for (;;) {
	/* Remember the current values in case search fails (doesn't push). */
	set_isrch_spot(top_spot, hl, pos, cs, sbptr, dir, nomatch);
	if (sbptr == 1 && sbuf[0] == '^') {
	    cs = 0;
    	    nomatch = 0;
	    statusline = ibuf + NORM_PROMPT_POS;
	} else if (sbptr > 0) {
	    char *last_line = s;

	    for (;;) {
		char *t;

		if (skip_pos) {
		    if (dir < 0) {
			if (pos == 0)
			    skip_line = 1;
			else
			    pos -= 1 + (pos != 1 && s[pos-2] == Meta);
		    } else if (sbuf[0] != '^') {
			if (pos >= strlen(s+1))
			    skip_line = 1;
			else
			    pos += 1 + (s[pos] == Meta);
		    } else
			skip_line = 1;
		    skip_pos = 0;
		}
		if (!skip_line && ((sbuf[0] == '^') ?
		    (t = metadiffer(s, sbuf + 1, sbptr - 1) < sens ? s : NULL) :
		    (t = hstrnstr(s, pos, sbuf, sbptr, dir, sens)))) {
		    zle_setline(he);
		    pos = t - s;
		    cs = ztrsub(t, s) + (dir == 1? sbptr - (sbuf[0]=='^') : 0);
	    	    nomatch = 0;
		    statusline = ibuf + NORM_PROMPT_POS;
		    break;
		}
		if (!(zlereadflags & ZLRF_HISTORY)
		 || !(he = movehistent(he, dir, hist_skip_flags))) {
		    if (sbptr == (int)isrch_spots[top_spot-1].len
		     && (isrch_spots[top_spot-1].flags & ISS_FAILING))
			top_spot--;
		    get_isrch_spot(top_spot, &hl, &pos, &cs, &sbptr,
				   &dir, &nomatch);
		    if (!nomatch) {
			feep = 1;
			nomatch = 1;
		    }
		    he = quietgethist(hl);
		    s = ZLETEXT(he);
		    skip_line = 0;
		    statusline = ibuf;
		    break;
		}
		hl = he->histnum;
		s = ZLETEXT(he);
		pos = dir == 1? 0 : strlen(s);
		skip_line = isset(HISTFINDNODUPS)? !!(he->flags & HIST_DUP)
						 : !strcmp(last_line, s);
	    }
	} else {
	    top_spot = 0;
    	    nomatch = 0;
	    statusline = ibuf + NORM_PROMPT_POS;
	}
	sbuf[sbptr] = '_';
	statusll = sbuf - statusline + sbptr + 1;
    ref:
	zrefresh();
	if (!(cmd = getkeycmd()) || cmd == Th(z_sendbreak)) {
	    int i;
	    get_isrch_spot(0, &hl, &pos, &i, &sbptr, &dir, &nomatch);
	    he = quietgethist(hl);
	    zle_setline(he);
	    s = ZLETEXT(he);
	    cs = i;
	    break;
	}
	if(cmd == Th(z_clearscreen)) {
	    clearscreen(zlenoargs);
	    goto ref;
	} else if(cmd == Th(z_redisplay)) {
	    redisplay(zlenoargs);
	    goto ref;
	} else if(cmd == Th(z_vicmdmode)) {
	    if(selectkeymap(invicmdmode() ? "main" : "vicmd", 0))
		feep = 1;
	    goto ref;
	} else if(cmd == Th(z_vibackwarddeletechar) ||
	    	cmd == Th(z_backwarddeletechar)) {
	    if (top_spot)
		get_isrch_spot(--top_spot, &hl, &pos, &cs, &sbptr,
			       &dir, &nomatch);
	    else
		feep = 1;
	    if (nomatch) {
		statusline = ibuf;
		skip_pos = 1;
	    }
	    he = quietgethist(hl);
	    s = ZLETEXT(he);
	    if (nomatch || !sbptr || (sbptr == 1 && sbuf[0] == '^')) {
		int i = cs;
		zle_setline(he);
		cs = i;
	    }
	    memcpy(ibuf + NORM_PROMPT_POS, (dir == 1) ? "fwd" : "bck", 3);
	    continue;
	} else if(cmd == Th(z_acceptandhold)) {
	    acceptandhold(zlenoargs);
	    break;
	} else if(cmd == Th(z_acceptandinfernexthistory)) {
	    acceptandinfernexthistory(zlenoargs);
	    break;
	} else if(cmd == Th(z_acceptlineanddownhistory)) {
	    acceptlineanddownhistory(zlenoargs);
	    break;
	} else if(cmd == Th(z_acceptline)) {
	    acceptline(zlenoargs);
	    break;
	} else if(cmd == Th(z_historyincrementalsearchbackward)) {
	    set_isrch_spot(top_spot++, hl, pos, cs, sbptr, dir, nomatch);
	    if (dir != -1)
		dir = -1;
	    else
		skip_pos = 1;
	    goto rpt;
	} else if(cmd == Th(z_historyincrementalsearchforward)) {
	    set_isrch_spot(top_spot++, hl, pos, cs, sbptr, dir, nomatch);
	    if (dir != 1)
		dir = 1;
	    else
		skip_pos = 1;
	    goto rpt;
	} else if(cmd == Th(z_virevrepeatsearch)) {
	    set_isrch_spot(top_spot++, hl, pos, cs, sbptr, dir, nomatch);
	    dir = -odir;
	    skip_pos = 1;
	    goto rpt;
	} else if(cmd == Th(z_virepeatsearch)) {
	    set_isrch_spot(top_spot++, hl, pos, cs, sbptr, dir, nomatch);
	    dir = odir;
	    skip_pos = 1;
	rpt:
	    if (!sbptr && previous_search_len) {
		if (previous_search_len > sibuf - FIRST_SEARCH_CHAR - 2) {
		    ibuf = hrealloc(ibuf, sibuf, sibuf + previous_search_len);
		    sbuf = ibuf + FIRST_SEARCH_CHAR;
		    sibuf += previous_search_len;
		}
		memcpy(sbuf, previous_search, sbptr = previous_search_len);
	    }
	    memcpy(ibuf + NORM_PROMPT_POS, (dir == 1) ? "fwd" : "bck", 3);
	    continue;
	} else if(cmd == Th(z_viquotedinsert) ||
	    	cmd == Th(z_quotedinsert)) {
	    if(cmd == Th(z_viquotedinsert)) {
		sbuf[sbptr] = '^';
		zrefresh();
	    }
	    if ((c = getkey(0)) == EOF)
		feep = 1;
	    else
		goto ins;
	} else {
	    if(cmd == Th(z_selfinsertunmeta)) {
		c &= 0x7f;
		if(c == '\r')
		    c = '\n';
	    } else if (cmd == Th(z_magicspace))
		c = ' ';
	    else if (cmd != Th(z_selfinsert)) {
		ungetkeycmd();
		if (cmd == Th(z_sendbreak))
		    sbptr = 0;
		break;
	    }
	ins:
	    if (sbptr == PATH_MAX) {
		feep = 1;
		continue;
	    }
	    set_isrch_spot(top_spot++, hl, pos, cs, sbptr, dir, nomatch);
	    if (sbptr == sibuf - FIRST_SEARCH_CHAR - 2) {
		ibuf = hrealloc(ibuf, sibuf, sibuf * 2);
		sbuf = ibuf + FIRST_SEARCH_CHAR;
		sibuf *= 2;
	    }
	    sbuf[sbptr++] = c;
	}
	if (feep)
	    handlefeep(zlenoargs);
	feep = 0;
    }
    if (sbptr) {
	zfree(previous_search, previous_search_len);
	previous_search = zalloc(sbptr);
	memcpy(previous_search, sbuf, previous_search_len = sbptr);
    }
    statusline = NULL;
    selectkeymap(okeymap, 1);
    zsfree(okeymap);
    /*
     * Don't allow unused characters provided as a string to the
     * widget to overflow and be used as separated commands.
     */
    if (savekeys >= 0 && kungetct > savekeys)
	kungetct = savekeys;
}

static Histent
infernexthist(Histent he, char **args)
{
    for (he = movehistent(he, -2, HIST_FOREIGN);
	 he; he = movehistent(he, -1, HIST_FOREIGN)) {
	if (!metadiffer(he->text, (char *) line, ll))
	    return movehistent(he, 1, HIST_FOREIGN);
    }
    return NULL;
}

/**/
int
acceptandinfernexthistory(char **args)
{
    Histent he;

    if (!(he = infernexthist(hist_ring, args)))
	return 1;
    zpushnode(bufstack, ztrdup(he->text));
    done = 1;
    stackhist = he->histnum;
    return 0;
}

/**/
int
infernexthistory(char **args)
{
    Histent he = quietgethist(histline);

    if (!he || !(he = infernexthist(he, args)))
	return 1;
    zle_setline(he);
    return 0;
}

/**/
int
vifetchhistory(char **args)
{
    if (zmult < 0)
	return 1;
    if (histline == curhist) {
	if (!(zmod.flags & MOD_MULT)) {
	    cs = ll;
	    cs = findbol();
	    return 0;
	}
    }
    if (!zle_goto_hist((zmod.flags & MOD_MULT) ? zmult : curhist, 0, 0) &&
	isset(HISTBEEP)) {
	return 1;
    }
    return 0;
}

/* the last vi search */

static char *visrchstr, *vipenultsrchstr;
static int visrchsense;

/**/
static int
getvisrchstr(void)
{
    char *sbuf = zhalloc(80);
    int sptr = 1, ret = 0, ssbuf = 80, feep = 0;
    Thingy cmd;
    char *okeymap = ztrdup(curkeymapname);

    if (vipenultsrchstr) {
	zsfree(vipenultsrchstr);
    }

    if (visrchstr) {
	vipenultsrchstr = visrchstr;
	visrchstr = NULL;
    }
    clearlist = 1;
    statusline = sbuf;
    sbuf[0] = (visrchsense == -1) ? '?' : '/';
    selectkeymap("main", 1);
    while (sptr) {
	sbuf[sptr] = '_';
	statusll = sptr + 1;
	zrefresh();
	if (!(cmd = getkeycmd()) || cmd == Th(z_sendbreak)) {
	    ret = 0;
	    break;
	}
	if(cmd == Th(z_magicspace)) {
	    c = ' ';
	    cmd = Th(z_selfinsert);
	}
	if(cmd == Th(z_redisplay)) {
	    redisplay(zlenoargs);
	} else if(cmd == Th(z_clearscreen)) {
	    clearscreen(zlenoargs);
	} else if(cmd == Th(z_acceptline) ||
	    	cmd == Th(z_vicmdmode)) {
	    sbuf[sptr] = 0;
	    visrchstr = metafy(sbuf + 1, sptr - 1, META_DUP);
	    if (!strlen(visrchstr)) {
	        zsfree(visrchstr);
		visrchstr = ztrdup(vipenultsrchstr);
	    }
	    ret = 1;
	    sptr = 0;
	} else if(cmd == Th(z_backwarddeletechar) ||
	    	cmd == Th(z_vibackwarddeletechar)) {
	    sptr--;
	} else if(cmd == Th(z_backwardkillword) ||
	    	cmd == Th(z_vibackwardkillword)) {
	    while(sptr != 1 && iblank(sbuf[sptr - 1]))
		sptr--;
	    if(iident(sbuf[sptr - 1]))
		while(sptr != 1 && iident(sbuf[sptr - 1]))
		    sptr--;
	    else
		while(sptr != 1 && !iident(sbuf[sptr - 1]) && !iblank(sbuf[sptr - 1]))
		    sptr--;
	} else if(cmd == Th(z_viquotedinsert) || cmd == Th(z_quotedinsert)) {
	    if(cmd == Th(z_viquotedinsert)) {
		sbuf[sptr] = '^';
		zrefresh();
	    }
	    if ((c = getkey(0)) == EOF)
		feep = 1;
	    else
		goto ins;
	} else if(cmd == Th(z_selfinsertunmeta) || cmd == Th(z_selfinsert)) {
	    if(cmd == Th(z_selfinsertunmeta)) {
		c &= 0x7f;
		if(c == '\r')
		    c = '\n';
	    }
	  ins:
	    if(sptr == ssbuf - 1) {
		char *newbuf = zhalloc(ssbuf *= 2);
		strcpy(newbuf, sbuf);
		statusline = sbuf = newbuf;
	    }
	    sbuf[sptr++] = c;
	} else {
	    feep = 1;
	}
	if (feep)
	    handlefeep(zlenoargs);
	feep = 0;
    }
    statusline = NULL;
    selectkeymap(okeymap, 1);
    zsfree(okeymap);
    return ret;
}

/**/
int
vihistorysearchforward(char **args)
{
    if (*args) {
	int ose = visrchsense, ret;
	char *ost = visrchstr;

	visrchsense = 1;
	visrchstr = *args;
	ret = virepeatsearch(zlenoargs);
	visrchsense = ose;
	visrchstr = ost;
	return ret;
    }
    visrchsense = 1;
    if (getvisrchstr())
	return virepeatsearch(zlenoargs);
    return 1;
}

/**/
int
vihistorysearchbackward(char **args)
{
    if (*args) {
	int ose = visrchsense, ret;
	char *ost = visrchstr;

	visrchsense = -1;
	visrchstr = *args;
	ret = virepeatsearch(zlenoargs);
	visrchsense = ose;
	visrchstr = ost;
	return ret;
    }
    visrchsense = -1;
    if (getvisrchstr())
	return virepeatsearch(zlenoargs);
    return 1;
}

/**/
int
virepeatsearch(char **args)
{
    Histent he;
    int t0;
    int n = zmult;
    char *s;

    if (!visrchstr)
	return 1;
    if (zmult < 0) {
	n = -n;
	visrchsense = -visrchsense;
    }
    t0 = strlen(visrchstr);
    if (!(he = quietgethist(histline)))
	return 1;
    while ((he = movehistent(he, visrchsense, hist_skip_flags))) {
	if (isset(HISTFINDNODUPS) && he->flags & HIST_DUP)
	    continue;
	s = ZLETEXT(he);
	if (metadiffer(s, (char *) line, ll)
	 && (*visrchstr == '^'? strncmp(s, visrchstr + 1, t0 - 1) == 0
			      : hstrnstr(s, 0, visrchstr, t0, 1, 1) != 0)) {
	    if (--n <= 0) {
		zle_setline(he);
		return 0;
	    }
	}
    }
    return 1;
}

/**/
int
virevrepeatsearch(char **args)
{
    int ret;
    visrchsense = -visrchsense;
    ret = virepeatsearch(args);
    visrchsense = -visrchsense;
    return ret;
}

/* Extra function added by A.R. Iano-Fletcher.	*/
/*The extern variable "cs" is the position of the cursor. */
/* history-beginning-search-backward */

/**/
int
historybeginningsearchbackward(char **args)
{
    Histent he;
    int cpos = cs;		/* save cursor position */
    int n = zmult;
    char *s;

    if (zmult < 0) {
	int ret;
	zmult = -n;
	ret = historybeginningsearchforward(args);
	zmult = n;
	return ret;
    }
    if (!(he = quietgethist(histline)))
	return 1;
    while ((he = movehistent(he, -1, hist_skip_flags))) {
	if (isset(HISTFINDNODUPS) && he->flags & HIST_DUP)
	    continue;
	s = ZLETEXT(he);
	if (metadiffer(s, (char *)line, cs) < 0 &&
	    metadiffer(s, (char *)line, ll)) {
	    if (--n <= 0) {
		zle_setline(he);
		cs = cpos;
		return 0;
	    }
	}
    }
    return 1;
}

/* Extra function added by A.R. Iano-Fletcher.	*/

/* history-beginning-search-forward */
/**/
int
historybeginningsearchforward(char **args)
{
    Histent he;
    int cpos = cs;		/* save cursor position */
    int n = zmult;
    char *s;

    if (zmult < 0) {
	int ret;
	zmult = -n;
	ret = historybeginningsearchbackward(args);
	zmult = n;
	return ret;
    }
    if (!(he = quietgethist(histline)))
	return 1;
    while ((he = movehistent(he, 1, hist_skip_flags))) {
	if (isset(HISTFINDNODUPS) && he->flags & HIST_DUP)
	    continue;
	s = ZLETEXT(he);
	if (metadiffer(s, (char *)line, cs) < (he->histnum == curhist) &&
	    metadiffer(s, (char *)line, ll)) {
	    if (--n <= 0) {
		zle_setline(he);
		cs = cpos;
		return 0;
	    }
	}
    }
    return 1;
}
