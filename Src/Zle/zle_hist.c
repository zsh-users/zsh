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

/* Are references to earlier history lines permitted?  == 0 if       *
 * editing or reading a standalone line, such as in vared or select. */

/**/
int histallowed;

/* Column position of vi ideal cursor.  -1 if it is unknown -- most *
 * movements and changes do this.                                   */

/**/
int lastcol;

/* current history line number */

/**/
int histline;

#define ZLETEXT(X) ((X)->zle_text ? (X)->zle_text : (X)->text)

/**/
void
remember_edits(void)
{
    Histent ent = quietgethist(histline);
    if (metadiffer(ZLETEXT(ent), (char *) line, ll)) {
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
void
uphistory(void)
{
    int nodups = isset(HISTIGNOREDUPS);
    if (!zle_goto_hist(histline, -zmult, nodups) && isset(HISTBEEP))
	feep();
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
void
uplineorhistory(void)
{
    int ocs = cs;
    int n = upline();
    if (n) {
	int m = zmult;

	cs = ocs;
	if (virangeflag || !histallowed) {
	    feep();
	    return;
	}
	zmult = n;
	uphistory();
	zmult = m;
    }
}

/**/
void
viuplineorhistory(void)
{
    int col = lastcol;
    uplineorhistory();
    lastcol = col;
    vifirstnonblank();
}

/**/
void
uplineorsearch(void)
{
    int ocs = cs;
    int n = upline();
    if (n) {
	int m = zmult;

	cs = ocs;
	if (virangeflag || !histallowed) {
	    feep();
	    return;
	}
	zmult = n;
	historysearchbackward();
	zmult = m;
    }
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
void
downlineorhistory(void)
{
    int ocs = cs;
    int n = downline();
    if (n) {
	int m = zmult;

	cs = ocs;
	if (virangeflag || !histallowed) {
	    feep();
	    return;
	}
	zmult = n;
	downhistory();
	zmult = m;
    }
}

/**/
void
vidownlineorhistory(void)
{
    int col = lastcol;
    downlineorhistory();
    lastcol = col;
    vifirstnonblank();
}

/**/
void
downlineorsearch(void)
{
    int ocs = cs;
    int n = downline();
    if (n) {
	int m = zmult;

	cs = ocs;
	if (virangeflag || !histallowed) {
	    feep();
	    return;
	}
	zmult = n;
	historysearchforward();
	zmult = m;
    }
}

/**/
void
acceptlineanddownhistory(void)
{
    Histent he;

    if (!(he = movehistent(quietgethist(histline), 1, HIST_FOREIGN))) {
	feep();
	return;
    }
    pushnode(bufstack, ztrdup(ZLETEXT(he)));
    done = 1;
    stackhist = he->histnum;
}

/**/
void
downhistory(void)
{
    int nodups = isset(HISTIGNOREDUPS);
    if (!zle_goto_hist(histline, zmult, nodups) && isset(HISTBEEP))
	feep();
}

static int histpos, srch_hl, srch_cs = -1;
static char *srch_str;

/**/
void
historysearchbackward(void)
{
    Histent he;
    int n = zmult;
    char *s;

    if (zmult < 0) {
	zmult = -n;
	historysearchforward();
	zmult = n;
	return;
    }
    if (histline == curhist || histline != srch_hl || cs != srch_cs || mark != 0
     || memcmp(srch_str, line, histpos) != 0) {
	zfree(srch_str, histpos);
	for (histpos = 0; histpos < ll && !iblank(line[histpos]); histpos++) ;
	if (histpos < ll)
	    histpos++;
	srch_str = zalloc(histpos);
	memcpy(srch_str, line, histpos);
    }
    he = quietgethist(histline);
    while ((he = movehistent(he, -1, hist_skip_flags))) {
	if (isset(HISTFINDNODUPS) && he->flags & HIST_DUP)
	    continue;
	s = ZLETEXT(he);
	if (metadiffer(s, srch_str, histpos) < 0 &&
	    metadiffer(s, srch_str, ll)) {
	    if (--n <= 0) {
		zle_setline(he);
		srch_hl = histline;
		srch_cs = cs;
		return;
	    }
	}
    }
    feep();
}

/**/
void
historysearchforward(void)
{
    Histent he;
    int n = zmult;
    char *s;

    if (zmult < 0) {
	zmult = -n;
	historysearchbackward();
	zmult = n;
	return;
    }
    if (histline == curhist || histline != srch_hl || cs != srch_cs || mark != 0
     || memcmp(srch_str, line, histpos) != 0) {
	zfree(srch_str, histpos);
	for (histpos = 0; histpos < ll && !iblank(line[histpos]); histpos++) ;
	if (histpos < ll)
	    histpos++;
	srch_str = zalloc(histpos);
	memcpy(srch_str, line, histpos);
    }
    he = quietgethist(histline);
    while ((he = movehistent(he, 1, hist_skip_flags))) {
	if (isset(HISTFINDNODUPS) && he->flags & HIST_DUP)
	    continue;
	s = ZLETEXT(he);
	if (metadiffer(s, srch_str, histpos) < (he->histnum == curhist) &&
	    metadiffer(s, srch_str, ll)) {
	    if (--n <= 0) {
		zle_setline(he);
		srch_hl = histline;
		srch_cs = cs;
		return;
	    }
	}
    }
    feep();
}

/**/
void
beginningofbufferorhistory(void)
{
    if (findbol())
	cs = 0;
    else
	beginningofhistory();
}

/**/
void
beginningofhistory(void)
{
    if (!zle_goto_hist(firsthist(), 0, 0) && isset(HISTBEEP))
	feep();
}

/**/
void
endofbufferorhistory(void)
{
    if (findeol() != ll)
	cs = ll;
    else
	endofhistory();
}

/**/
void
endofhistory(void)
{
    zle_goto_hist(curhist, 0, 0);
}

/**/
void
insertlastword(void)
{
    int n;
    char *s, *t;
    Histent he;

/* multiple calls will now search back through the history, pem */
    static char *lastinsert;
    static int lasthist, lastpos;
    int evhist = addhistnum(curhist, -1, HIST_FOREIGN), save;

    if (lastinsert) {
	int lastlen = ztrlen(lastinsert);
	int pos = cs;

	if (lastpos <= pos &&
	    lastlen == pos - lastpos &&
	    memcmp(lastinsert, (char *)&line[lastpos], lastlen) == 0) {
	    evhist = addhistnum(lasthist, -1, HIST_FOREIGN);
	    cs = lastpos;
	    foredel(pos - cs);
	}
	zsfree(lastinsert);
	lastinsert = NULL;
    }
    if (!(he = quietgethist(evhist)) || !he->nwords) {
	feep();
	return;
    }
    if (zmult > 0) {
	n = he->nwords - (zmult - 1);
    } else {
	n = 1 - zmult;
    }
    if (n < 1 || n > he->nwords) {
	feep();
	return;
    }
    s = he->text + he->words[2*n-2];
    t = he->text + he->words[2*n-1];
    save = *t;
    *t = '\0';			/* ignore trailing whitespace */

    lasthist = evhist;
    lastpos = cs;
    lastinsert = ztrdup(s);
    n = zmult;
    zmult = 1;
    doinsert(s);
    zmult = n;
    *t = save;
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
void
setlocalhistory(void)
{
    if (zmod.flags & MOD_MULT) {
	hist_skip_flags = zmult? HIST_FOREIGN : 0;
    }
    else {
	hist_skip_flags ^= HIST_FOREIGN;
    }
}

/**/
int
zle_goto_hist(int ev, int n, int skipdups)
{
    Histent he = movehistent(quietgethist(ev), n, hist_skip_flags);
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
void
pushline(void)
{
    int n = zmult;

    if (n < 0)
	return;
    pushnode(bufstack, metafy((char *) line, ll, META_DUP));
    while (--n)
	pushnode(bufstack, ztrdup(""));
    stackcs = cs;
    *line = '\0';
    ll = cs = 0;
    clearlist = 1;
}

/**/
void
pushlineoredit(void)
{
    int ics;
    unsigned char *s;
    char *hline = hgetline();

    if (zmult < 0)
	return;
    if (hline && *hline) {
	ics = ztrlen(hline);
	sizeline(ics + ll + 1);
	for (s = line + ll; --s >= line; *(s + ics) = *s);
	for (s = line; *hline; hline++)
	    *s++ = *hline == Meta ? *++hline ^ 32 : *hline;
	ll += ics;
	cs += ics;
    }
    pushline();
    if (!isfirstln)
	errflag = done = 1;
    clearlist = 1;
}

/**/
void
pushinput(void)
{
    int i;

    if (zmult < 0)
	return;
    zmult += i = !isfirstln;
    pushlineoredit();
    zmult -= i;
}

/**/
void
getline(void)
{
    char *s = (char *)getlinknode(bufstack);

    if (!s)
	feep();
    else {
	int cc;

	unmetafy(s, &cc);
	spaceinline(cc);
	memcpy((char *)line + cs, s, cc);
	cs += cc;
	free(s);
	clearlist = 1;
    }
}

/**/
void
historyincrementalsearchbackward(void)
{
    doisearch(-1);
}

/**/
void
historyincrementalsearchforward(void)
{
    doisearch(1);
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
#ifdef MODULE

/**/
void
free_isrch_spots(void)
{
    zfree(isrch_spots, max_spot * sizeof(*isrch_spots));
}

/**/
#endif /* MODULE */

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
doisearch(int dir)
{
    char *s, *ibuf = zhalloc(80), *sbuf = ibuf + FIRST_SEARCH_CHAR;
    int sbptr = 0, top_spot = 0, pos, sibuf = 80;
    int nomatch = 0, skip_line = 0, skip_pos = 0;
    int odir = dir, sens = zmult == 1 ? 3 : 1;
    int hl = histline;
    Thingy cmd;
    char *okeymap = curkeymapname;
    static char *previous_search = NULL;
    static int previous_search_len = 0;
    Histent he;

    clearlist = 1;

    strcpy(ibuf, ISEARCH_PROMPT);
    memcpy(ibuf + NORM_PROMPT_POS, (dir == 1) ? "fwd" : "bck", 3);
    remember_edits();
    he = quietgethist(hl);
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
		if (!(he = movehistent(he, dir, hist_skip_flags))) {
		    if (sbptr == (int)isrch_spots[top_spot-1].len
		     && (isrch_spots[top_spot-1].flags & ISS_FAILING))
			top_spot--;
		    get_isrch_spot(top_spot, &hl, &pos, &cs, &sbptr,
				   &dir, &nomatch);
		    if (!nomatch) {
			feep();
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
	    clearscreen();
	    goto ref;
	} else if(cmd == Th(z_redisplay)) {
	    redisplay();
	    goto ref;
	} else if(cmd == Th(z_vicmdmode)) {
	    if(selectkeymap(invicmdmode() ? "main" : "vicmd", 0))
		feep();
	    goto ref;
	} else if(cmd == Th(z_vibackwarddeletechar) ||
	    	cmd == Th(z_backwarddeletechar)) {
	    if (top_spot)
		get_isrch_spot(--top_spot, &hl, &pos, &cs, &sbptr,
			       &dir, &nomatch);
	    else
		feep();
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
	    acceptandhold();
	    break;
	} else if(cmd == Th(z_acceptandinfernexthistory)) {
	    acceptandinfernexthistory();
	    break;
	} else if(cmd == Th(z_acceptlineanddownhistory)) {
	    acceptlineanddownhistory();
	    break;
	} else if(cmd == Th(z_acceptline)) {
	    acceptline();
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
		feep();
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
		feep();
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
	handlefeep();
    }
    if (sbptr) {
	zfree(previous_search, previous_search_len);
	previous_search = zalloc(sbptr);
	memcpy(previous_search, sbuf, previous_search_len = sbptr);
    }
    statusline = NULL;
    selectkeymap(okeymap, 1);
}

/**/
void
acceptandinfernexthistory(void)
{
    Histent he;

    done = 1;
    for (he = movehistent(quietgethist(histline), -2, HIST_FOREIGN);
	 he; he = movehistent(he, -1, HIST_FOREIGN)) {
	if (!metadiffer(ZLETEXT(he), (char *) line, ll)) {
	    he = movehistent(he, 1, HIST_FOREIGN);
	    pushnode(bufstack, ztrdup(ZLETEXT(he)));
	    stackhist = he->histnum;
	    return;
	}
    }
}

/**/
void
infernexthistory(void)
{
    Histent he;

    for (he = movehistent(quietgethist(histline), -2, HIST_FOREIGN);
	 he; he = movehistent(he, -1, HIST_FOREIGN)) {
	if (!metadiffer(ZLETEXT(he), (char *) line, ll)) {
	    he = movehistent(he, 1, HIST_FOREIGN);
	    zle_setline(he);
	    return;
	}
    }
    feep();
}

/**/
void
vifetchhistory(void)
{
    if (zmult < 0)
	return;
    if (histline == curhist) {
	if (!(zmod.flags & MOD_MULT)) {
	    cs = ll;
	    cs = findbol();
	    return;
	}
    }
    if (!zle_goto_hist((zmod.flags & MOD_MULT) ? zmult : curhist, 0, 0) &&
	isset(HISTBEEP))
	feep();
}

/* the last vi search */

static char *visrchstr;
static int visrchsense;

/**/
static int
getvisrchstr(void)
{
    char *sbuf = zhalloc(80);
    int sptr = 1, ret = 0, ssbuf = 80;
    Thingy cmd;
    char *okeymap = curkeymapname;

    if (visrchstr) {
	zsfree(visrchstr);
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
	    redisplay();
	} else if(cmd == Th(z_clearscreen)) {
	    clearscreen();
	} else if(cmd == Th(z_acceptline) ||
	    	cmd == Th(z_vicmdmode)) {
	    sbuf[sptr] = 0;
	    visrchstr = metafy(sbuf + 1, sptr - 1, META_DUP);
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
		feep();
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
	    feep();
	}
	handlefeep();
    }
    statusline = NULL;
    selectkeymap(okeymap, 1);
    return ret;
}

/**/
void
vihistorysearchforward(void)
{
    visrchsense = 1;
    if (getvisrchstr())
	virepeatsearch();
}

/**/
void
vihistorysearchbackward(void)
{
    visrchsense = -1;
    if (getvisrchstr())
	virepeatsearch();
}

/**/
void
virepeatsearch(void)
{
    Histent he;
    int t0;
    int n = zmult;
    char *s;

    if (!visrchstr) {
	feep();
	return;
    }
    if (zmult < 0) {
	n = -n;
	visrchsense = -visrchsense;
    }
    t0 = strlen(visrchstr);
    he = quietgethist(histline);
    while ((he = movehistent(he, visrchsense, hist_skip_flags))) {
	if (isset(HISTFINDNODUPS) && he->flags & HIST_DUP)
	    continue;
	s = ZLETEXT(he);
	if (metadiffer(s, (char *) line, ll)
	 && (*visrchstr == '^'? strncmp(s, visrchstr + 1, t0 - 1) == 0
			      : hstrnstr(s, 0, visrchstr, t0, 1, 1) != 0)) {
	    if (--n <= 0) {
		zle_setline(he);
		return;
	    }
	}
    }
    feep();
}

/**/
void
virevrepeatsearch(void)
{
    visrchsense = -visrchsense;
    virepeatsearch();
    visrchsense = -visrchsense;
}

/* Extra function added by A.R. Iano-Fletcher.	*/
/*The extern variable "cs" is the position of the cursor. */
/* history-beginning-search-backward */

/**/
void
historybeginningsearchbackward(void)
{
    Histent he;
    int cpos = cs;		/* save cursor position */
    int n = zmult;
    char *s;

    if (zmult < 0) {
	zmult = -n;
	historybeginningsearchforward();
	zmult = n;
	return;
    }
    he = quietgethist(histline);
    while ((he = movehistent(he, -1, hist_skip_flags))) {
	if (isset(HISTFINDNODUPS) && he->flags & HIST_DUP)
	    continue;
	s = ZLETEXT(he);
	if (metadiffer(s, (char *)line, cs) < 0 &&
	    metadiffer(s, (char *)line, ll)) {
	    if (--n <= 0) {
		zle_setline(he);
		cs = cpos;
		return;
	    }
	}
    }
    feep();
}

/* Extra function added by A.R. Iano-Fletcher.	*/

/* history-beginning-search-forward */
/**/
void
historybeginningsearchforward(void)
{
    Histent he;
    int cpos = cs;		/* save cursor position */
    int n = zmult;
    char *s;

    if (zmult < 0) {
	zmult = -n;
	historybeginningsearchbackward();
	zmult = n;
	return;
    }
    he = quietgethist(histline);
    while ((he = movehistent(he, 1, hist_skip_flags))) {
	if (isset(HISTFINDNODUPS) && he->flags & HIST_DUP)
	    continue;
	s = ZLETEXT(he);
	if (metadiffer(s, (char *)line, cs) < (he->histnum == curhist) &&
	    metadiffer(s, (char *)line, ll)) {
	    if (--n <= 0) {
		zle_setline(he);
		cs = cpos;
		return;
	    }
	}
    }
    feep();
}
