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
ZLE_STRING_T previous_search = NULL;

/**/
int previous_search_len = 0;


/*** History text manipulation utilities ***/


struct zle_text {
    /* Metafied, NULL-terminated string */
    char *text;
    /* 1 if we have allocated space for text */
    int alloced;
};

/*
 * Fetch the text of a history line in internal ZLE format.
 * If the line has been edited, returns that, else allocates
 * a converted line.
 *
 * Each use of this must have a matching zletextfree() in order
 * to free up the allocated line, if any.  (N.B.: each use *of
 * the function*, not just each use of a struct zle_text.)
 */

static void
zletext(Histent ent, struct zle_text *zt)
{
    if (ent->zle_text) {
	zt->text = ent->zle_text;
	zt->alloced = 0;
	return;
    }

    zt->text = ztrdup(ent->node.nam);
    zt->alloced = 1;
}

/* See above. */

static void
zletextfree(struct zle_text *zt)
{
    if (zt->alloced) {
	free(zt->text);
	zt->alloced = 0;
    }
}

/**/
void
remember_edits(void)
{
    Histent ent = quietgethist(histline);
    if (ent) {
	char *line = 
	    zlemetaline ? zlemetaline :
	    zlelineasstring(zleline, zlell, 0, NULL, NULL, 0);
	if (!ent->zle_text || strcmp(line, ent->zle_text) != 0) {
	    if (ent->zle_text)
		free(ent->zle_text);
	    ent->zle_text = zlemetaline ? ztrdup(line) : line;
	} else if (!zlemetaline)
	    free(line);
    }
}

/**/
void
forget_edits(void)
{
    Histent he;

    for (he = hist_ring; he; he = up_histent(he)) {
	if (he->zle_text) {
	    free(he->zle_text);
	    he->zle_text = NULL;
	}
    }
}


/*** Search utilities ***/


/*
 * Return zero if the ZLE string histp length histl and the ZLE string
 * inputp length inputl are the same.  Return -1 if inputp is a prefix
 * of histp.  Return 1 if inputp is the lowercase version of histp.
 * Return 2 if inputp is the lowercase prefix of histp and return 3
 * otherwise.
 */

static int
zlinecmp(const char *histp, const char *inputp)
{
    const char *hptr = histp, *iptr = inputp;
#ifdef MULTIBYTE_SUPPORT
    mbstate_t hstate, istate;
#endif

    while (*hptr == *iptr) {
	hptr++;
	iptr++;
    }

    if (!*iptr) {
	if (!*hptr) {
	    /* strings are the same */
	    return 0;
	} else {
	    /* inputp is a prefix */
	    return -1;
	}
    }

#ifdef MULTIBYTE_SUPPORT
    memset(&hstate, 0, sizeof(hstate));
    memset(&istate, 0, sizeof(istate));
#endif

    /* look for lower case versions */
    while (*histp && *inputp) {
#ifdef MULTIBYTE_SUPPORT
	wint_t hwc, iwc;
	int hlen, ilen;

	hlen = mb_metacharlenconv_r(histp, &hwc, &hstate);
	ilen = mb_metacharlenconv_r(inputp, &iwc, &istate);

	if (hwc == WEOF || iwc == WEOF) {
	    /* can't convert, compare input characters */
	    if (ilen != hlen || memcmp(histp, inputp, hlen) != 0)
		return 3;
	} else if (towlower(hwc) != iwc)
	    return 3;

	histp += hlen;
	inputp += ilen;
#else
    	if (tulower(*histp++) != *inputp++)
	    return 3;
#endif
    }
    if (!*inputp) {
	/* one string finished, if it's the input... */
	if (!*histp)
	    return 1;		/* ...same, else */
	else
	    return 2;		/* ...prefix */
    }
    /* Different */
    return 3;
}


/*
 * Search for needle in haystack.  Haystack and needle are metafied strings.
 * Start the search at position pos in haystack.
 * Search forward if dir > 0, otherwise search backward.
 * sens is used to test against the return value of linecmp.
 *
 * Return the pointer to the location in haystack found, else NULL.
 *
 * We assume we'll only find needle at some sensible position in a multibyte
 * string, so we don't bother calculating multibyte character lengths for
 * incrementing and decrementing the search position.
 */

static char *
zlinefind(char *haystack, int pos, char *needle, int dir, int sens)
{
    char *s = haystack + pos;

    if (dir > 0) {
	while (*s) {
	    if (zlinecmp(s, needle) < sens)
		return s;
	    s++;
	}
    } else {
	for (;;) {
	    if (zlinecmp(s, needle) < sens)
		return s;
	    if (s == haystack)
		break;
	    s--;
	}
    }

    return NULL;
}


/*** Widgets ***/


/**/
int
uphistory(UNUSED(char **args))
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
	n = -downline();
	zmult = -zmult;
	return n;
    }
    if (lastcol == -1)
	lastcol = zlecs - findbol();
    zlecs = findbol();
    while (n) {
	if (!zlecs)
	    break;
	zlecs--;
	zlecs = findbol();
	n--;
    }
    if (!n) {
	int x = findeol();

	if ((zlecs += lastcol) >= x) {
	    zlecs = x;
	    if (zlecs > findbol() && invicmdmode())
		DECCS();
	}
    }
    return n;
}

/**/
int
uplineorhistory(char **args)
{
    int ocs = zlecs;
    int n = upline();
    if (n) {
	int m = zmult, ret;

	zlecs = ocs;
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
    int ocs = zlecs;
    int n = upline();
    if (n) {
	int m = zmult, ret;

	zlecs = ocs;
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
	n = -upline();
	zmult = -zmult;
	return n;
    }
    if (lastcol == -1)
	lastcol = zlecs - findbol();
    while (n) {
	int x = findeol();

	if (x == zlell)
	    break;
	zlecs = x + 1;
	n--;
    }
    if (!n) {
	int x = findeol();

	if ((zlecs += lastcol) >= x) {
	    zlecs = x;
	    if (zlecs > findbol() && invicmdmode())
		DECCS();
	}
    }
    return n;
}

/**/
int
downlineorhistory(char **args)
{
    int ocs = zlecs;
    int n = downline();
    if (n) {
	int m = zmult, ret;

	zlecs = ocs;
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
    int ocs = zlecs;
    int n = downline();
    if (n) {
	int m = zmult, ret;

	zlecs = ocs;
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
acceptlineanddownhistory(UNUSED(char **args))
{
    Histent he = quietgethist(histline);

    if (he && (he = movehistent(he, 1, HIST_FOREIGN))) {
	zpushnode(bufstack, ztrdup(he->node.nam));
	stackhist = he->histnum;
    }
    done = 1;
    return 0;
}

/**/
int
downhistory(UNUSED(char **args))
{
    int nodups = isset(HISTIGNOREDUPS);
    if (!zle_goto_hist(histline, zmult, nodups) && isset(HISTBEEP))
	return 1;
    return 0;
}

/*
 * Values remembered for history searches to enable repetition.
 * srch_hl remembers the old value of histline, to see if it's changed
 *   since the last search.
 * srch_cs remembers the old value of zlecs for the same purpose (it is
 *   not use for any other purpose, i.e. does not need to be a valid
 *   index into anything).
 * srch_str is the metafied search string, as extracted from the start
 *   of zleline.
 */
static int srch_hl, srch_cs = -1;
static char *srch_str;

/**/
int
historysearchbackward(char **args)
{
    Histent he;
    int n = zmult, histpos;
    char *str;
    struct zle_text zt;

    if (zmult < 0) {
	int ret;
	zmult = -n;
	ret = historysearchforward(args);
	zmult = n;
	return ret;
    }
    if (*args) {
	str = *args;
    } else {
	char *line = zlelineasstring(zleline, zlell, 0, NULL, NULL, 0);
	if (histline == curhist || histline != srch_hl || zlecs != srch_cs ||
	    mark != 0 || strncmp(srch_str, line, histpos) != 0) {
	    free(srch_str);
	    for (histpos = 0; histpos < zlell && !ZC_iblank(zleline[histpos]);
		 histpos++)
		;
	    if (histpos < zlell)
		histpos++;
	    /* ensure we're not on a combining character */
	    CCRIGHTPOS(histpos);
	    /* histpos from now on on is an index into the metafied string */
	    srch_str = zlelineasstring(zleline, histpos, 0, NULL, NULL, 0);
	}
	free(line);
	str = srch_str;
    }
    if (!(he = quietgethist(histline)))
	return 1;

    while ((he = movehistent(he, -1, hist_skip_flags))) {
	if (isset(HISTFINDNODUPS) && he->node.flags & HIST_DUP)
	    continue;
	zletext(he, &zt);
	if (zlinecmp(zt.text, str) < 0 &&
	    (*args || strcmp(zt.text, str) != 0)) {
	    if (--n <= 0) {
		zle_setline(he);
		srch_hl = histline;
		srch_cs = zlecs;
		zletextfree(&zt);
		return 0;
	    }
	}
	zletextfree(&zt);
    }
    return 1;
}

/**/
int
historysearchforward(char **args)
{
    Histent he;
    int n = zmult, histpos;
    char *str;
    struct zle_text zt;

    if (zmult < 0) {
	int ret;
	zmult = -n;
	ret = historysearchbackward(args);
	zmult = n;
	return ret;
    }
    if (*args) {
	str = *args;
    } else {
	char *line = zlelineasstring(zleline, zlell, 0, NULL, NULL, 0);
	if (histline == curhist || histline != srch_hl || zlecs != srch_cs ||
	    mark != 0 || strncmp(srch_str, line, histpos) != 0) {
	    free(srch_str);
	    for (histpos = 0; histpos < zlell && !ZC_iblank(zleline[histpos]);
		 histpos++)
		;
	    if (histpos < zlell)
		histpos++;
	    CCRIGHT();
	    srch_str = zlelineasstring(zleline, histpos, 0, NULL, NULL, 0);
	}
	free(line);
	str = srch_str;
    }
    if (!(he = quietgethist(histline)))
	return 1;

    while ((he = movehistent(he, 1, hist_skip_flags))) {
	if (isset(HISTFINDNODUPS) && he->node.flags & HIST_DUP)
	    continue;
	zletext(he, &zt);
	if (zlinecmp(zt.text, str) < (he->histnum == curhist) &&
	    (*args || strcmp(zt.text, str) != 0)) {
	    if (--n <= 0) {
		zle_setline(he);
		srch_hl = histline;
		srch_cs = zlecs;
		zletextfree(&zt);
		return 0;
	    }
	}
	zletextfree(&zt);
    }
    return 1;
}

/**/
int
beginningofbufferorhistory(char **args)
{
    if (findbol())
	zlecs = 0;
    else
	return beginningofhistory(args);
    return 0;
}

/**/
int
beginningofhistory(UNUSED(char **args))
{
    if (!zle_goto_hist(firsthist(), 0, 0) && isset(HISTBEEP))
	return 1;
    return 0;
}

/**/
int
endofbufferorhistory(char **args)
{
    if (findeol() != zlell)
	zlecs = zlell;
    else
	return endofhistory(args);
    return 0;
}

/**/
int
endofhistory(UNUSED(char **args))
{
    zle_goto_hist(curhist, 0, 0);
    return 0;
}

/**/
int
insertlastword(char **args)
{
    int n, nwords, histstep = -1, wordpos = 0, deleteword = 0, len;
    char *s, *t;
    Histent he = NULL;
    LinkList l = NULL;
    LinkNode node;
    ZLE_STRING_T zs;

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

    metafy_line();
    if (lastinsert && lastlen &&
	lastpos <= zlemetacs &&
	lastlen == zlemetacs - lastpos &&
	memcmp(lastinsert, &zlemetaline[lastpos], lastlen) == 0)
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
	    int pos = zlemetacs;
	    zlemetacs = lastpos;
	    foredel(pos - zlemetacs, CUT_RAW);
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
	if (!(l = bufferwords(NULL, NULL, NULL))) {
	    unmetafy_line();
	    return 1;
	}
	nwords = countlinknodes(l);
    } else {
	/* Some stored line. */
	if (!(he = quietgethist(evhist)) || !he->nwords) {
	    unmetafy_line();
	    return 1;
	}
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
	unmetafy_line();
	return 1;
    }
    /*
     * Only remove the old word from the command line if we have
     * successfully found a new one to insert.
     */
    if (deleteword > 0) {
	int pos = zlemetacs;
	zlemetacs = lastpos;
	foredel(pos - zlemetacs, CUT_RAW);
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
	s = he->node.nam + he->words[2*n-2];
	t = he->node.nam + he->words[2*n-1];
    }

    save = *t;
    *t = '\0';			/* ignore trailing whitespace */
    lasthist = evhist;
    lastpos = zlemetacs;
    lastlen = t - s;
    lastinsert = zalloc(t - s);
    memcpy(lastinsert, s, lastlen);
    n = zmult;
    zmult = 1;

    unmetafy_line();

    zs = stringaszleline(s, 0, &len, NULL, NULL);
    doinsert(zs, len);
    free(zs);
    zmult = n;
    *t = save;
    return 0;
}

/**/
void
zle_setline(Histent he)
{
    int remetafy;
    if (zlemetaline) {
	unmetafy_line();
	remetafy = 1;
    } else
	remetafy = 0;
    remember_edits();
    mkundoent();
    histline = he->histnum;

    setline(he->zle_text ? he->zle_text : he->node.nam, ZSL_COPY|ZSL_TOEND);
    setlastline();
    clearlist = 1;
    if (remetafy)
	metafy_line();
}

/**/
int
setlocalhistory(UNUSED(char **args))
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
    char *line = zlelineasstring(zleline, zlell, 0, NULL, NULL, 1);

    if (!he || !(he = movehistent(he, n, hist_skip_flags)))
	return 1;
    if (skipdups && n) {
	struct zle_text zt;

	n = n < 0? -1 : 1;
	while (he) {
	    int ret;

	    zletext(he, &zt);
	    ret = zlinecmp(zt.text, line);
	    zletextfree(&zt);
	    if (ret)
		break;
	    he = movehistent(he, n, hist_skip_flags);
	}
    }
    if (!he)
	return 0;
    zle_setline(he);
    return 1;
}

/**/
int
pushline(UNUSED(char **args))
{
    int n = zmult;

    if (n < 0)
	return 1;
    zpushnode(bufstack, zlelineasstring(zleline, zlell, 0, NULL, NULL, 0));
    while (--n)
	zpushnode(bufstack, ztrdup(""));
    stackcs = zlecs;
    *zleline = ZWC('\0');
    zlell = zlecs = 0;
    clearlist = 1;
    return 0;
}

/**/
int
pushlineoredit(char **args)
{
    int ics, ret;
    ZLE_STRING_T s;
    char *hline = hgetline();

    if (zmult < 0)
	return 1;
    if (hline && *hline) {
	ZLE_STRING_T zhline = stringaszleline(hline, 0, &ics, NULL, NULL);

	sizeline(ics + zlell + 1);
	/* careful of overlapping copy */
	for (s = zleline + zlell; --s >= zleline; s[ics] = *s)
	    ;
	ZS_memcpy(zleline, zhline, ics);
	zlell += ics;
	zlecs += ics;
	free(zhline);
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

/* Renamed to avoid clash with library function */
/**/
int
zgetline(UNUSED(char **args))
{
    char *s = getlinknode(bufstack);

    if (!s) {
	return 1;
    } else {
	int cc;
	ZLE_STRING_T lineadd = stringaszleline(s, 0, &cc, NULL, NULL);

	spaceinline(cc);
	ZS_memcpy(zleline + zlecs, lineadd, cc);
	zlecs += cc;
	free(s);
	free(lineadd);
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
    max_spot = 0;
    isrch_spots = NULL;
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
    char *ibuf = zhalloc(80);
    char *sbuf = ibuf + FIRST_SEARCH_CHAR;
    char *last_line = NULL;
    struct zle_text zt;
    int sbptr = 0, top_spot = 0, pos, sibuf = 80;
    int nomatch = 0, skip_line = 0, skip_pos = 0;
    int odir = dir, sens = zmult == 1 ? 3 : 1;
    int hl = histline, savekeys = -1, feep = 0;
    Thingy cmd;
    char *okeymap;
    Histent he;
    ZleIntFunc exitfn = (ZleIntFunc)0;

    if (!(he = quietgethist(hl)))
	return;

    clearlist = 1;

    if (*args) {
	int len;
	char *arg;
	savekeys = kungetct;
	arg = getkeystring(*args, &len, GETKEYS_BINDKEY, NULL);
	ungetbytes(arg, len);
    }

    strcpy(ibuf, ISEARCH_PROMPT);
    /* careful with fwd/bck: we don't want the NULL copied */
    memcpy(ibuf + NORM_PROMPT_POS, (dir == 1) ? "fwd" : "bck", 3);
    okeymap = ztrdup(curkeymapname);
    selectkeymap("main", 1);

    metafy_line();
    remember_edits();
    zletext(he, &zt);
    pos = zlemetacs;
    for (;;) {
	/* Remember the current values in case search fails (doesn't push). */
	set_isrch_spot(top_spot, hl, pos, zlemetacs, sbptr, dir, nomatch);
	if (sbptr == 1 && sbuf[0] == '^') {
	    zlemetacs = 0;
    	    nomatch = 0;
	    statusline = ibuf + NORM_PROMPT_POS;
	} else if (sbptr > 0) {
	    /*
	     * As we may free zt.text as soon as we switch to a new
	     * line, we can't keep the pointer to it.  This is a bit
	     * ghastly.
	     */
	    if (last_line)
		free(last_line);
	    last_line = ztrdup(zt.text);

	    for (;;) {
		char *t;

		/*
		 * If instructed, move past a match position:
		 * backwards if searching backwards (skipping
		 * the line if we're at the start), forwards
		 * if searching forwards (skipping a line if we're
		 * at the end).
		 */
		if (skip_pos) {
		    if (dir < 0) {
			if (pos == 0)
			    skip_line = 1;
			else
			    pos = backwardmetafiedchar(zlemetaline,
						       zlemetaline + pos,
						       NULL) - zlemetaline;
		    } else if (sbuf[0] != '^') {
			if (pos >= strlen(zt.text) - 1)
			    skip_line = 1;
			else
			    pos += 1;
		    } else
			skip_line = 1;
		    skip_pos = 0;
		}
		/*
		 * First search for a(nother) match within the
		 * current line, unless we've been told to skip it.
		 */
		sbuf[sbptr] = '\0';
		if (!skip_line && ((sbuf[0] == '^') ?
				   (t = (zlinecmp(zt.text, sbuf + 1) < sens
					 ? zt.text : NULL)) :
		    (t = zlinefind(zt.text, pos, sbuf, dir, sens)))) {
		    zle_setline(he);
		    pos = t - zt.text;
		    zlemetacs = pos +
			(dir == 1 ? sbptr - (sbuf[0] == '^') : 0);
	    	    nomatch = 0;
		    statusline = ibuf + NORM_PROMPT_POS;
		    break;
		}
		/*
		 * If not found within that line, move through
		 * the history to try again.
		 */
		if (!(zlereadflags & ZLRF_HISTORY)
		 || !(he = movehistent(he, dir, hist_skip_flags))) {
		    if (sbptr == (int)isrch_spots[top_spot-1].len
		     && (isrch_spots[top_spot-1].flags & ISS_FAILING))
			top_spot--;
		    get_isrch_spot(top_spot, &hl, &pos, &zlemetacs, &sbptr,
				   &dir, &nomatch);
		    if (!nomatch) {
			feep = 1;
			nomatch = 1;
		    }
		    he = quietgethist(hl);
		    zletextfree(&zt);
		    zletext(he, &zt);
		    skip_line = 0;
		    statusline = ibuf;
		    break;
		}
		hl = he->histnum;
		zletextfree(&zt);
		zletext(he, &zt);
		pos = (dir == 1) ? 0 : strlen(zt.text);
		skip_line = isset(HISTFINDNODUPS)
		    ? !!(he->node.flags & HIST_DUP)
		    : !strcmp(zt.text, last_line);
	    }
	} else {
	    top_spot = 0;
    	    nomatch = 0;
	    statusline = ibuf + NORM_PROMPT_POS;
	}
	sbuf[sbptr] = '_';
	sbuf[sbptr+1] = '\0';
    ref:
	zrefresh();
	if (!(cmd = getkeycmd()) || cmd == Th(z_sendbreak)) {
	    int i;
	    get_isrch_spot(0, &hl, &pos, &i, &sbptr, &dir, &nomatch);
	    he = quietgethist(hl);
	    zle_setline(he);
	    zletextfree(&zt);
	    zletext(he, &zt);
	    zlemetacs = i;
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
		get_isrch_spot(--top_spot, &hl, &pos, &zlemetacs, &sbptr,
			       &dir, &nomatch);
	    else
		feep = 1;
	    if (nomatch) {
		statusline = ibuf;
		skip_pos = 1;
	    }
	    he = quietgethist(hl);
	    zletextfree(&zt);
	    zletext(he, &zt);
	    if (nomatch || !sbptr || (sbptr == 1 && sbuf[0] == '^')) {
		int i = zlemetacs;
		zle_setline(he);
		zlemetacs = i;
	    }
	    memcpy(ibuf + NORM_PROMPT_POS,
		   (dir == 1) ? "fwd" : "bck", 3);
	    continue;
	} else if(cmd == Th(z_acceptandhold)) {
	    exitfn = acceptandhold;
	    break;
	} else if(cmd == Th(z_acceptandinfernexthistory)) {
	    exitfn = acceptandinfernexthistory;
	    break;
	} else if(cmd == Th(z_acceptlineanddownhistory)) {
	    exitfn = acceptlineanddownhistory;
	    break;
	} else if(cmd == Th(z_acceptline)) {
	    exitfn = acceptline;
	    break;
	} else if(cmd == Th(z_historyincrementalsearchbackward)) {
	    set_isrch_spot(top_spot++, hl, pos, zlemetacs, sbptr, dir, nomatch);
	    if (dir != -1)
		dir = -1;
	    else
		skip_pos = 1;
	    goto rpt;
	} else if(cmd == Th(z_historyincrementalsearchforward)) {
	    set_isrch_spot(top_spot++, hl, pos, zlemetacs, sbptr, dir, nomatch);
	    if (dir != 1)
		dir = 1;
	    else
		skip_pos = 1;
	    goto rpt;
	} else if(cmd == Th(z_virevrepeatsearch)) {
	    set_isrch_spot(top_spot++, hl, pos, zlemetacs, sbptr, dir, nomatch);
	    dir = -odir;
	    skip_pos = 1;
	    goto rpt;
	} else if(cmd == Th(z_virepeatsearch)) {
	    set_isrch_spot(top_spot++, hl, pos, zlemetacs, sbptr, dir, nomatch);
	    dir = odir;
	    skip_pos = 1;
	rpt:
	    if (!sbptr && previous_search_len) {
		if (previous_search_len > sibuf - FIRST_SEARCH_CHAR - 2) {
		    ibuf = hrealloc((char *)ibuf, sibuf,
				    (sibuf + previous_search_len));
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
		sbuf[sbptr+1] = '\0';
		zrefresh();
	    }
	    if (getfullchar(0) == ZLEEOF)
		feep = 1;
	    else
		goto ins;
	} else {
	    if(cmd == Th(z_selfinsertunmeta)) {
		fixunmeta();
	    } else if (cmd == Th(z_magicspace)) {
		fixmagicspace();
	    } else if (cmd == Th(z_selfinsert)) {
#ifdef MULTIBYTE_SUPPORT
		if (!lastchar_wide_valid)
		    if (getrestchar(lastchar) == WEOF) {
			handlefeep(zlenoargs);
			continue;
		    }
#else
		;
#endif
	    } else {
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
	    set_isrch_spot(top_spot++, hl, pos, zlemetacs, sbptr, dir, nomatch);
	    if (sbptr >= sibuf - FIRST_SEARCH_CHAR - 2 
#ifdef MULTIBYTE_SUPPORT
		- 2 * MB_CUR_MAX
#endif
		) {
		ibuf = hrealloc(ibuf, sibuf, sibuf * 2);
		sbuf = ibuf + FIRST_SEARCH_CHAR;
		sibuf *= 2;
	    }
	    /*
	     * We've supposedly arranged above that lastchar_wide is
	     * always valid at this point.
	     */
	    sbptr += zlecharasstring(LASTFULLCHAR, sbuf + sbptr);
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
    unmetafy_line();
    if (exitfn)
	exitfn(zlenoargs);
    selectkeymap(okeymap, 1);
    zsfree(okeymap);
    /*
     * Don't allow unused characters provided as a string to the
     * widget to overflow and be used as separated commands.
     */
    if (savekeys >= 0 && kungetct > savekeys)
	kungetct = savekeys;
    if (last_line)
	free(last_line);
    zletextfree(&zt);
}

static Histent
infernexthist(Histent he, UNUSED(char **args))
{
    metafy_line();
    for (he = movehistent(he, -2, HIST_FOREIGN);
	 he; he = movehistent(he, -1, HIST_FOREIGN)) {
	struct zle_text zt;
	zletext(he, &zt);

	if (!zlinecmp(zt.text, zlemetaline)) {
	    unmetafy_line();
	    zletextfree(&zt);
	    return movehistent(he, 1, HIST_FOREIGN);
	}
	zletextfree(&zt);
    }
    unmetafy_line();
    return NULL;
}

/**/
int
acceptandinfernexthistory(char **args)
{
    Histent he;

    if (!(he = infernexthist(hist_ring, args)))
	return 1;
    zpushnode(bufstack, ztrdup(he->node.nam));
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
vifetchhistory(UNUSED(char **args))
{
    if (zmult < 0)
	return 1;
    if (histline == curhist) {
	if (!(zmod.flags & MOD_MULT)) {
	    zlecs = zlell;
	    zlecs = findbol();
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
	vipenultsrchstr = NULL;
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
	sbuf[sptr] = '\0';
	zrefresh();
	if (!(cmd = getkeycmd()) || cmd == Th(z_sendbreak)) {
	    ret = 0;
	    break;
	}
	if(cmd == Th(z_magicspace)) {
	    fixmagicspace();
	    cmd = Th(z_selfinsert);
	}
	if(cmd == Th(z_redisplay)) {
	    redisplay(zlenoargs);
	} else if(cmd == Th(z_clearscreen)) {
	    clearscreen(zlenoargs);
	} else if(cmd == Th(z_acceptline) ||
	    	cmd == Th(z_vicmdmode)) {
	    if (sptr) {
		sbuf[sptr] = ZWC('\0');
		visrchstr = ztrdup(sbuf+1);
	    } else {
		visrchstr = ztrdup(vipenultsrchstr);
	    }
	    ret = 1;
	    sptr = 0;
	} else if(cmd == Th(z_backwarddeletechar) ||
		  cmd == Th(z_vibackwarddeletechar)) {
	    sptr = backwardmetafiedchar(sbuf+1, sbuf+sptr, NULL) - sbuf;
	} else if(cmd == Th(z_backwardkillword) ||
		  cmd == Th(z_vibackwardkillword)) {
	    convchar_t cc;
	    char *newpos;
	    while (sptr != 1) {
		newpos = backwardmetafiedchar(sbuf+1, sbuf+sptr, &cc);
		if (!ZC_iblank(cc))
		    break;
		sptr = newpos - sbuf;
	    }
	    if (sptr > 1) {
		newpos = backwardmetafiedchar(sbuf+1, sbuf+sptr, &cc);
		if (ZC_iident(cc)) {
		    for (;;) {
			sptr = newpos - sbuf;
			if (sptr == 1)
			    break;
			newpos = backwardmetafiedchar(sbuf+1, sbuf+sptr, &cc);
			if (!ZC_iident(cc))
			    break;
		    }
		} else {
		    for (;;) {
			sptr = newpos - sbuf;
			if (sptr == 1)
			    break;
			newpos = backwardmetafiedchar(sbuf+1, sbuf+sptr, &cc);
			if (ZC_iident(cc) || ZC_iblank(cc))
			    break;
		    }
		}
	    }
	} else if(cmd == Th(z_viquotedinsert) || cmd == Th(z_quotedinsert)) {
	    if(cmd == Th(z_viquotedinsert)) {
		sbuf[sptr] = '^';
		zrefresh();
	    }
	    if (getfullchar(0) == ZLEEOF)
		feep = 1;
	    else
		goto ins;
	} else if(cmd == Th(z_selfinsertunmeta) || cmd == Th(z_selfinsert)) {
	    if(cmd == Th(z_selfinsertunmeta)) {
		fixunmeta();
	    } else {
#ifdef MULTIBYTE_SUPPORT
		if (!lastchar_wide_valid)
		    if (getrestchar(lastchar) == WEOF) {
			handlefeep(zlenoargs);
			continue;
		    }
#else
		;
#endif
	    }
	  ins:
	    if (sptr == ssbuf - 1) {
		char *newbuf = (char *)zhalloc((ssbuf *= 2));
		strcpy(newbuf, sbuf);
		statusline = sbuf = newbuf;
	    }
	    sptr += zlecharasstring(LASTFULLCHAR, sbuf + sptr);
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
virepeatsearch(UNUSED(char **args))
{
    Histent he;
    int n = zmult;
    struct zle_text zt;

    if (!visrchstr)
	return 1;
    if (zmult < 0) {
	n = -n;
	visrchsense = -visrchsense;
    }
    if (!(he = quietgethist(histline)))
	return 1;
    metafy_line();
    while ((he = movehistent(he, visrchsense, hist_skip_flags))) {
	if (isset(HISTFINDNODUPS) && he->node.flags & HIST_DUP)
	    continue;
	zletext(he, &zt);
	if (zlinecmp(zt.text, zlemetaline) &&
	    (*visrchstr == '^' ? strpfx(zt.text, visrchstr + 1) :
	     zlinefind(zt.text, 0, visrchstr, 1, 1) != 0)) {
	    if (--n <= 0) {
		unmetafy_line();
		zletextfree(&zt);
		zle_setline(he);
		return 0;
	    }
	}
	zletextfree(&zt);
    }
    unmetafy_line();
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
/*The extern variable "zlecs" is the position of the cursor. */
/* history-beginning-search-backward */

/**/
int
historybeginningsearchbackward(char **args)
{
    Histent he;
    int cpos = zlecs;		/* save cursor position */
    int n = zmult;
    struct zle_text zt;

    if (zmult < 0) {
	int ret;
	zmult = -n;
	ret = historybeginningsearchforward(args);
	zmult = n;
	return ret;
    }
    if (!(he = quietgethist(histline)))
	return 1;
    metafy_line();
    while ((he = movehistent(he, -1, hist_skip_flags))) {
	int tst;
	char sav;
	if (isset(HISTFINDNODUPS) && he->node.flags & HIST_DUP)
	    continue;
	zletext(he, &zt);
	sav = zlemetaline[zlemetacs];
	zlemetaline[zlemetacs] = '\0';
	tst = zlinecmp(zt.text, zlemetaline);
	zlemetaline[zlemetacs] = sav;
	if (tst < 0 && zlinecmp(zt.text, zlemetaline)) {
	    if (--n <= 0) {
		unmetafy_line();
		zletextfree(&zt);
		zle_setline(he);
		zlecs = cpos;
		CCRIGHT();
		return 0;
	    }
	}
	zletextfree(&zt);
    }
    unmetafy_line();
    return 1;
}

/* Extra function added by A.R. Iano-Fletcher.	*/

/* history-beginning-search-forward */
/**/
int
historybeginningsearchforward(char **args)
{
    Histent he;
    int cpos = zlecs;		/* save cursor position */
    int n = zmult;
    struct zle_text zt;

    if (zmult < 0) {
	int ret;
	zmult = -n;
	ret = historybeginningsearchbackward(args);
	zmult = n;
	return ret;
    }
    if (!(he = quietgethist(histline)))
	return 1;
    metafy_line();
    while ((he = movehistent(he, 1, hist_skip_flags))) {
	char sav;
	int tst;
	if (isset(HISTFINDNODUPS) && he->node.flags & HIST_DUP)
	    continue;
	zletext(he, &zt);
	sav = zlemetaline[zlemetacs];
	zlemetaline[zlemetacs] = '\0';
	tst = zlinecmp(zt.text, zlemetaline) < (he->histnum == curhist);
	zlemetaline[zlemetacs] = sav;
	if (tst && zlinecmp(zt.text, zlemetaline)) {
	    if (--n <= 0) {
		unmetafy_line();
		zletextfree(&zt);
		zle_setline(he);
		zlecs = cpos;
		CCRIGHT();
		return 0;
	    }
	}
	zletextfree(&zt);
    }
    unmetafy_line();
    return 1;
}
