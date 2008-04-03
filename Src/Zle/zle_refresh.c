/*
 * zle_refresh.c - screen update
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

#define ZR_equal(zr1, zr2) ((zr1).chr == (zr2).chr && (zr1).atr == (zr2).atr)

static void
ZR_memset(REFRESH_ELEMENT *dst, REFRESH_ELEMENT rc, int len)
{
    while (len--)
	*dst++ = rc;
}

#define ZR_memcpy(d, s, l)  memcpy((d), (s), (l)*sizeof(REFRESH_ELEMENT))

static void
ZR_strcpy(REFRESH_ELEMENT *dst, const REFRESH_ELEMENT *src)
{
    while ((*dst++ = *src++).chr != ZWC('\0'))
	;
}

static size_t
ZR_strlen(const REFRESH_ELEMENT *wstr)
{
    int len = 0;

    while (wstr++->chr != ZWC('\0'))
	len++;

    return len;
}

/*
 * Simplified strcmp: we don't need the sign, just whether
 * the strings and their attributes are equal.
 */
static int
ZR_strncmp(const REFRESH_ELEMENT *wstr1, const REFRESH_ELEMENT *wstr2, int len)
{
    while (len--) {
	if (!wstr1->chr || !wstr2->chr)
	    return !ZR_equal(*wstr1, *wstr2);
	if (!ZR_equal(*wstr1, *wstr2))
	    return 1;
	wstr1++;
	wstr2++;
    }

    return 0;
}

#include "zle_refresh.pro"

/*
 * Expanded prompts.
 *
 * These are always output from the start, except in the special
 * case where we are sure each character in the prompt corresponds
 * to a character on screen.
 */

/**/
char *lpromptbuf, *rpromptbuf;

/* Text attributes after displaying prompts */

/**/
unsigned pmpt_attr, rpmpt_attr;

/* number of lines displayed */

/**/
mod_export int nlnct;

/* Most lines of the buffer we've shown at once with the current list *
 * showing.  == 0 if there is no list.  == -1 if a new list has just  *
 * been put on the screen.  == -2 if zrefresh() needs to put up a new *
 * list.                                                              */

/**/
mod_export int showinglist;

/* > 0 if a completion list is displayed below the prompt,
 * < 0 if a list is displayed above the prompt. */

/**/
mod_export int listshown;

/* Length of last list displayed (if it is below the prompt). */

/**/
mod_export int lastlistlen;

/* Non-zero if ALWAYS_LAST_PROMPT has been used, meaning that the *
 * screen below the buffer display should not be cleared by       *
 * zrefresh(), but should be by trashzle().                       */

/**/
mod_export int clearflag;

/* Non-zero if zrefresh() should clear the list below the prompt. */

/**/
mod_export int clearlist;

/* Zle in trashed state - updates may be subtly altered */

/**/
int trashedzle;

/*
 * Information used by PREDISPLAY and POSTDISPLAY parameters which
 * add non-editable text to that being displayed.
 */
/**/
ZLE_STRING_T predisplay, postdisplay;
/**/
int predisplaylen, postdisplaylen;


/*
 * Attributes used for highlighting special (unprintable) characters
 * displayed on screen.
 */

static int special_atr_on, special_atr_off;

/* Flags for the region_highlight structure */
enum {
    /* Offsets include predisplay */
    ZRH_PREDISPLAY = 1
};

/*
 * Attributes used for highlighting regions.
 * and mark.
 */
struct region_highlight {
    /* Attributes turned on in the region */
    int atr;
    /* Start of the region */
    int start;
    /*
     * End of the region:  position of the first character not highlighted
     * (the same system as for point and mark).
     */
    int end;
    /*
     * Any of the flags defined above.
     */
    int flags;
};
/*
 * Array of region highlights, no special termination.
 * The first element (0) always describes the region between
 * point and mark.  Any other elements are set by the user
 * via the parameter region_highlight.
 */
struct region_highlight *region_highlights;
/*
 * Number of elements in region_highlights.
 * This includes the region between point and mark, element 0.
 */
int n_region_highlights;

/*
 * Flag that highlighting of the region is active.
 */
/**/
int region_active;

#ifdef HAVE_SELECT
/* cost of last update */
/**/
int cost;

# define SELECT_ADD_COST(X)	(cost += X)
# define zputc(a)		(zwcputc(a, NULL), cost++)
# define zwrite(a, b)		(zwcwrite((a), (b)), \
				 cost += ((b) * ZLE_CHAR_SIZE))
#else
# define SELECT_ADD_COST(X)
# define zputc(a)		zwcputc(a, NULL)
# define zwrite(a, b)		zwcwrite((a), (b))
#endif

static const REFRESH_ELEMENT zr_cr = { ZWC('\r'), 0 };
static const REFRESH_ELEMENT zr_dt = { ZWC('.'), 0 };
static const REFRESH_ELEMENT zr_nl = { ZWC('\n'), 0 };
static const REFRESH_ELEMENT zr_sp = { ZWC(' '), 0 };
static const REFRESH_ELEMENT zr_ht = { ZWC('\t'), 0 };
static const REFRESH_ELEMENT zr_zr = { ZWC('\0'), 0 };

/*
 * Constant arrays to be copied into place: these are memcpy'd,
 * so don't have terminating NULLs.
 */
static const REFRESH_ELEMENT zr_end_ellipsis[] = {
    { ZWC(' '), 0 },
    { ZWC('<'), 0 },
    { ZWC('.'), 0 },
    { ZWC('.'), 0 },
    { ZWC('.'), 0 },
    { ZWC('.'), 0 },
    { ZWC(' '), 0 },
};
#define ZR_END_ELLIPSIS_SIZE	\
    (sizeof(zr_end_ellipsis)/sizeof(zr_end_ellipsis[0]))

static const REFRESH_ELEMENT zr_mid_ellipsis1[] = {
    { ZWC(' '), 0 },
    { ZWC('<'), 0 },
    { ZWC('.'), 0 },
    { ZWC('.'), 0 },
    { ZWC('.'), 0 },
    { ZWC('.'), 0 },
};
#define ZR_MID_ELLIPSIS1_SIZE	\
    (sizeof(zr_mid_ellipsis1)/sizeof(zr_mid_ellipsis1[0]))

static const REFRESH_ELEMENT zr_mid_ellipsis2[] = {
    { ZWC('>'), 0 },
    { ZWC(' '), 0 },
};
#define ZR_MID_ELLIPSIS2_SIZE	\
    (sizeof(zr_mid_ellipsis2)/sizeof(zr_mid_ellipsis2[0]))

static const REFRESH_ELEMENT zr_start_ellipsis[] = {
    { ZWC('>'), 0 },
    { ZWC('.'), 0 },
    { ZWC('.'), 0 },
    { ZWC('.'), 0 },
    { ZWC('.'), 0 },
};
#define ZR_START_ELLIPSIS_SIZE	\
    (sizeof(zr_start_ellipsis)/sizeof(zr_start_ellipsis[0]))


/* Defines the available types of highlighting */
struct highlight {
    const char *name;
    int mask_on;
    int mask_off;
};

static const struct highlight highlights[] = {
    { "none", 0, TXT_ATTR_ON_MASK },
    { "bold", TXTBOLDFACE, 0 },
    { "standout", TXTSTANDOUT, 0 },
    { "underline", TXTUNDERLINE, 0 },
    { NULL, 0, 0 }
};

/*
 * Match a set of highlights in the given teststr.
 * Set *on_var to reflect the values found.
 */

static void
match_highlight(const char *teststr, int *on_var)
{
    int found = 1;

    *on_var = 0;
    while (found && *teststr) {
	const struct highlight *hl;

	found = 0;
	for (hl = highlights; hl->name; hl++) {
	    if (strpfx(hl->name, teststr)) {
		const char *val = teststr + strlen(hl->name);

		if (*val == ',')
		    val++;
		else if (*val)
		    break;

		*on_var |= hl->mask_on;
		*on_var &= ~hl->mask_off;
		teststr = val;
		found = 1;
	    }
	}
    }
}


/*
 * Parse the variable zle_highlight to decide how to highlight characters
 * and regions.  Set defaults for anything not explicitly covered.
 */

/**/
void zle_set_highlight(void)
{
    char **atrs = getaparam("zle_highlight");
    int special_atr_on_set = 0;
    int region_atr_on_set = 0;

    special_atr_on = 0;
    if (!region_highlights) {
	region_highlights = (struct region_highlight *)
	    zshcalloc(sizeof(struct region_highlight));
	n_region_highlights = 1;
    } else {
	region_highlights->atr = 0;
    }

    if (atrs) {
	for (; *atrs; atrs++) {
	    if (!strcmp(*atrs, "none")) {
		/* reset attributes for consistency... usually unnecessary */
		special_atr_on = region_highlights->atr = 0;
		special_atr_on_set = region_atr_on_set = 1;
	    } else if (strpfx("special:", *atrs)) {
		match_highlight(*atrs + 8, &special_atr_on);
		special_atr_on_set = 1;
	    } else if (strpfx("region:", *atrs)) {
		match_highlight(*atrs + 7, &region_highlights->atr);
		region_atr_on_set = 1;
	    }
	}
    }

    /* Defaults */
    if (!special_atr_on_set)
	special_atr_on = TXTSTANDOUT;
    if (!region_atr_on_set)
	region_highlights->atr = TXTSTANDOUT;
    special_atr_off = special_atr_on << TXT_ATTR_OFF_ON_SHIFT;
}


/*
 * Interface to the region_highlight ZLE parameter.
 * Converts betwen a format like "P32 42 underline,bold" to
 * the format in the region_highlights variable.  Note that
 * the region_highlights variable stores the internal (point/mark)
 * region in element zero.
 */

/**/
char **
get_region_highlight(UNUSED(Param pm))
{
    int arrsize = n_region_highlights;
    char **retarr, **arrp;

    /* region_highlights may not have been set yet */
    if (!arrsize)
	arrsize = 1;
    arrp = retarr = (char **)zhalloc(arrsize*sizeof(char *));
    /* ignore NULL termination */
    arrsize--;
    if (arrsize) {
	struct region_highlight *rhp;

	/* ignore point/mark at start */
	for (rhp = region_highlights+1; arrsize--; rhp++, arrp++) {
	    char digbuf1[DIGBUFSIZE], digbuf2[DIGBUFSIZE];
	    int atrlen = 0, alloclen, done1;
	    const struct highlight *hp;

	    sprintf(digbuf1, "%d", rhp->start);
	    sprintf(digbuf2, "%d", rhp->end);

	    for (hp = highlights; hp->name; hp++) {
		if (hp->mask_on & rhp->atr) {
		    if (atrlen)
			atrlen++; /* comma */
		    atrlen += strlen(hp->name);
		}
	    }
	    if (atrlen == 0)
		atrlen = 4; /* none */
	    alloclen = atrlen + strlen(digbuf1) + strlen(digbuf2) +
		3; /* 2 spaces, 1 0 */
	    if (rhp->flags & ZRH_PREDISPLAY)
		alloclen += 2; /* "P " */
	    *arrp = (char *)zhalloc(alloclen * sizeof(char));
	    /*
	     * On input we allow a space after the flags.
	     * I haven't put a space here because I think it's
	     * marginally easier to have the output always split
	     * into three words, and then check the first to
	     * see if there are flags.  However, it's arguable.
	     */
	    sprintf(*arrp, "%s%s %s ", 
		    (rhp->flags & ZRH_PREDISPLAY) ? "P" : "",
		    digbuf1, digbuf2);
	    if (atrlen) {
		for (hp = highlights, done1 = 0; hp->name; hp++) {
		    if (hp->mask_on & rhp->atr) {
			if (done1)
			    strcat(*arrp, ",");
			strcat(*arrp, hp->name);
			done1 = 1;
		    }
		}
	    } else
		strcat(*arrp, "none");
	}
    }
    *arrp = '\0';
    return retarr;
}


/**/
void
set_region_highlight(UNUSED(Param pm), char **aval)
{
    int len;
    struct region_highlight *rhp;

    len = aval ? arrlen(aval) : 0;
    if (n_region_highlights != len + 1) {
	/* no null termination, but include point/mark region at start */
	n_region_highlights = len + 1;
	region_highlights = (struct region_highlight *)
	    zrealloc(region_highlights,
		     sizeof(struct region_highlight) * n_region_highlights);
    }

    if (!aval)
	return;

    for (rhp = region_highlights + 1; *aval; rhp++, aval++) {
	char *strp, *oldstrp;

	oldstrp = *aval;
	if (*oldstrp == 'P') {
	    rhp->flags = ZRH_PREDISPLAY;
	    oldstrp++;
	}
	else
	    rhp->flags = 0;
	while (inblank(*oldstrp))
	    oldstrp++;

	rhp->start = (int)zstrtol(oldstrp, &strp, 10);
	if (strp == oldstrp)
	    rhp->start = -1;

	while (inblank(*strp))
	    strp++;

	oldstrp = strp;
	rhp->end = (int)zstrtol(strp, &strp, 10);
	if (strp == oldstrp)
	    rhp->end = -1;

	while (inblank(*strp))
	    strp++;

	match_highlight(strp, &rhp->atr);
    }
}


/**/
void
unset_region_highlight(Param pm, int exp)
{
    if (exp) {
	set_region_highlight(pm, NULL);
	stdunsetfn(pm, exp);
    }
}


/**/
void
zwcputc(const REFRESH_ELEMENT *c, REFRESH_CHAR *curatrp)
{
#ifdef MULTIBYTE_SUPPORT
    mbstate_t mbstate;
    int i;
    VARARR(char, mbtmp, MB_CUR_MAX + 1);
#endif

    /*
     * Don't output "on" attributes in a string of characters with
     * the same attributes.
     */
    if ((c->atr & TXT_ATTR_ON_MASK) &&
	(!curatrp ||
	 ((*curatrp & TXT_ATTR_ON_MASK) != (c->atr & TXT_ATTR_ON_MASK))))
	settextattributes(c->atr & TXT_ATTR_ON_MASK);

#ifdef MULTIBYTE_SUPPORT
    if (c->chr != WEOF) {
	memset(&mbstate, 0, sizeof(mbstate_t));
	if ((i = wcrtomb(mbtmp, (wchar_t)c->chr, &mbstate)) > 0)
	    fwrite(mbtmp, i, 1, shout);
    }
#else
    fputc(c->chr, shout);
#endif

    if (c->atr & TXT_ATTR_OFF_MASK)
	settextattributes(c->atr & TXT_ATTR_OFF_MASK);
    if (curatrp) {
	/*
	 * Remember the current attributes:  those that are turned
	 * on, less those that are turned off again.
	 */
	*curatrp = (c->atr & TXT_ATTR_ON_MASK) &
	    ~((c->atr & TXT_ATTR_OFF_MASK) >> TXT_ATTR_OFF_ON_SHIFT);
    }
}

static int
zwcwrite(const REFRESH_STRING s, size_t i)
{
    size_t j;
    REFRESH_CHAR curatr = 0;

    for (j = 0; j < i; j++)
	zwcputc(s + j, &curatr);
    return i; /* TODO something better for error indication */
}

/* Oct/Nov 94: <mason> some code savagely redesigned to fix several bugs -
   refreshline() & tc_rightcurs() majorly rewritten; zrefresh() fixed -
   I've put my fingers into just about every routine in here -
   any queries about updates to mason@primenet.com.au */

static REFRESH_STRING 
    *nbuf = NULL,		/* new video buffer line-by-line array */
    *obuf = NULL;		/* old video buffer line-by-line array */
static int more_start,		/* more text before start of screen?	    */
    more_end,			/* more stuff after end of screen?	    */
    olnct,			/* previous number of lines		    */
    ovln,			/* previous video cursor position line	    */
    lpromptw, rpromptw,		/* prompt widths on screen                  */
    lpromptwof,			/* left prompt width with real end position */
    lprompth,			/* lines taken up by the prompt		    */
    rprompth,			/* right prompt height                      */
    vcs, vln,			/* video cursor position column & line	    */
    vmaxln,			/* video maximum number of lines	    */
    winw, winh, rwinh,		/* window width & height		    */
    winpos,			/* singlelinezle: line's position in window */
    winprompt,			/* singlelinezle: part of lprompt showing   */
    winw_alloc = -1,		/* allocated window width */
    winh_alloc = -1;		/* allocates window height */

static void
freevideo(void)
{
    if (nbuf) {
	int ln;
	for (ln = 0; ln != winh_alloc; ln++) {
	    zfree(nbuf[ln], (winw_alloc + 2) * sizeof(**nbuf));
	    zfree(obuf[ln], (winw_alloc + 2) * sizeof(**obuf));
	}
	free(nbuf);
	free(obuf);
	nbuf = NULL;
	obuf = NULL;
    }
}

/**/
void
resetvideo(void)
{
    int ln;
 
    winw = columns;  /* terminal width */
    if (termflags & TERM_SHORT)
	winh = 1;
    else
	winh = (lines < 2) ? 24 : lines;
    rwinh = lines;		/* keep the real number of lines */
    vln = vmaxln = winprompt = 0;
    winpos = -1;
    if (winw_alloc != winw || winh_alloc != winh) {
	freevideo();
	nbuf = (REFRESH_STRING *)zshcalloc((winh + 1) * sizeof(*nbuf));
	obuf = (REFRESH_STRING *)zshcalloc((winh + 1) * sizeof(*obuf));
	nbuf[0] = (REFRESH_STRING)zalloc((winw + 2) * sizeof(**nbuf));
	obuf[0] = (REFRESH_STRING)zalloc((winw + 2) * sizeof(**obuf));

	winw_alloc = winw;
	winh_alloc = winh;
    }
    for (ln = 0; ln != winh + 1; ln++) {
	if (nbuf[ln]) {
	    nbuf[ln][0] = zr_nl;
	    nbuf[ln][1] = zr_zr;
	}
	if (obuf[ln]) {
	    obuf[ln][0] = zr_nl;
	    obuf[ln][1] = zr_zr;
	}
    }

    /*
     * countprompt() now correctly handles multibyte input.
     */
    countprompt(lpromptbuf, &lpromptwof, &lprompth, 1);
    countprompt(rpromptbuf, &rpromptw, &rprompth, 0);
    if (lpromptwof != winw)
	lpromptw = lpromptwof;
    else {
	lpromptw = 0;
	lprompth++;
    }

    if (lpromptw) {
    	ZR_memset(nbuf[0], zr_sp, lpromptw);
	ZR_memset(obuf[0], zr_sp, lpromptw);
	nbuf[0][lpromptw] = obuf[0][lpromptw] = zr_zr;
    }

    vcs = lpromptw;
    olnct = nlnct = 0;
    if (showinglist > 0)
	showinglist = -2;
    trashedzle = 0;
}

/*
 * Nov 96: <mason> changed to single line scroll
 */

/**/
static void
scrollwindow(int tline)
{
    int t0;
    REFRESH_STRING s;

    s = nbuf[tline];
    for (t0 = tline; t0 < winh - 1; t0++)
	nbuf[t0] = nbuf[t0 + 1];
    nbuf[winh - 1] = s;
    if (!tline)
	more_start = 1;
    return;
}

/*
 * Parameters in zrefresh used for communicating with next-line functions.
 */
struct rparams {
    int canscroll;		/* number of lines we are allowed to scroll */
    int ln;			/* current line we're working on */
    int more_status;		/* more stuff in status line */
    int nvcs;			/* video cursor column */
    int nvln;			/* video cursor line */
    int tosln;			/* tmp in statusline stuff */
    REFRESH_STRING s;		/* pointer into the video buffer */
    REFRESH_STRING sen;		/* pointer to end of the video buffer (eol) */
};
typedef struct rparams *Rparams;

static int cleareol,		/* clear to end-of-line (if can't cleareod) */
    clearf,			/* alwayslastprompt used immediately before */
    put_rpmpt,			/* whether we should display right-prompt   */
    oput_rpmpt,			/* whether displayed right-prompt last time */
    oxtabs,			/* oxtabs - tabs expand to spaces if set    */
    numscrolls, onumscrolls;

/*
 * Go to the next line in the main display area.  Return 1 if we should abort
 * processing the line loop at this point, else 0.
 *
 * If wrapped is non-zero, text wrapped, so output newline.
 * Otherwise, text not wrapped, so output null.
 */
static int
nextline(Rparams rpms, int wrapped)
{
    nbuf[rpms->ln][winw+1] = wrapped ? zr_nl : zr_zr;
    *rpms->s = zr_zr;
    if (rpms->ln != winh - 1)
	rpms->ln++;
    else {
	if (!rpms->canscroll)	{
	    if (rpms->nvln != -1 && rpms->nvln != winh - 1
		&& (numscrolls != onumscrolls - 1
		    || rpms->nvln <= winh / 2))
	        return 1;
	    numscrolls++;
	    rpms->canscroll = winh / 2;
	}
	rpms->canscroll--;
	scrollwindow(0);
	if (rpms->nvln != -1)
	    rpms->nvln--;
    }
    if (!nbuf[rpms->ln])
	nbuf[rpms->ln] = (REFRESH_STRING)zalloc((winw + 2) * sizeof(**nbuf));
    rpms->s = nbuf[rpms->ln];
    rpms->sen = rpms->s + winw;

    return 0;
}


/*
 * Go to the next line in the status area.
 */
static void
snextline(Rparams rpms)
{
    *rpms->s = zr_zr;
    if (rpms->ln != winh - 1)
	rpms->ln++;
    else
	if (rpms->tosln > rpms->ln) {
	    rpms->tosln--;
	    if (rpms->nvln > 1) {
		scrollwindow(0);
		rpms->nvln--;
	    } else
		more_end = 1;
	} else if (rpms->tosln > 2 && rpms->nvln > 1) {
	    rpms->tosln--;
	    if (rpms->tosln <= rpms->nvln) {
		scrollwindow(0);
		rpms->nvln--;
	    } else {
		scrollwindow(rpms->tosln);
		more_end = 1;
	    }
	} else {
	    rpms->more_status = 1;
	    scrollwindow(rpms->tosln + 1);
	}
    if (!nbuf[rpms->ln])
	nbuf[rpms->ln] = (REFRESH_STRING)zalloc((winw + 2) * sizeof(**nbuf));
    rpms->s = nbuf[rpms->ln];
    rpms->sen = rpms->s + winw;
}

/**/
static void
settextattributes(int atr)
{
    if (txtchangeisset(atr, TXTNOBOLDFACE))
	tsetcap(TCALLATTRSOFF, 0);
    if (txtchangeisset(atr, TXTNOSTANDOUT))
	tsetcap(TCSTANDOUTEND, 0);
    if (txtchangeisset(atr, TXTNOUNDERLINE))
	tsetcap(TCUNDERLINEEND, 0);
    if (txtchangeisset(atr, TXTBOLDFACE))
	tsetcap(TCBOLDFACEBEG, 0);
    if (txtchangeisset(atr, TXTSTANDOUT))
	tsetcap(TCSTANDOUTBEG, 0);
    if (txtchangeisset(atr, TXTUNDERLINE))
	tsetcap(TCUNDERLINEBEG, 0);
}

/**/
mod_export void
zrefresh(void)
{
    static int inlist;		/* avoiding recursion			     */
    int iln;			/* current line as index in loops	     */
    int t0 = -1;		/* tmp					     */
    ZLE_STRING_T tmpline,	/* line with added pre/post text	     */
	t,			/* pointer into the real buffer		     */
	scs,			/* pointer to cursor position in real buffer */
	u;			/* pointer for status line stuff	     */
    REFRESH_STRING	*qbuf;	/* tmp					     */
    int tmpcs, tmpll;		/* ditto cursor position and line length     */
    int tmppos;			/* t - tmpline				     */
    int tmpalloced;		/* flag to free tmpline when finished	     */
    int remetafy;		/* flag that zle line is metafied	     */
    struct rparams rpms;
#ifdef MULTIBYTE_SUPPORT
    int width;			/* width of wide character		     */
#endif

    
    /* If this is called from listmatches() (indirectly via trashzle()), and *
     * that was called from the end of zrefresh(), then we don't need to do  *
     * anything.  All this `inlist' code is actually unnecessary, but it     *
     * improves speed a little in a common case.                             */
    if (inlist)
	return;

    /*
     * zrefresh() is called from all over the place, so we can't
     * be sure if the line is metafied for completion or not.
     */
    if (zlemetaline != NULL) {
	remetafy = 1;
	unmetafy_line();
    }
    else
	remetafy = 0;

    if (predisplaylen || postdisplaylen) {
	/* There is extra text to display at the start or end of the line */
	tmpline = zalloc((zlell + predisplaylen + postdisplaylen)*sizeof(*tmpline));
	if (predisplaylen)
	    ZS_memcpy(tmpline, predisplay, predisplaylen);
	if (zlell)
	    ZS_memcpy(tmpline+predisplaylen, zleline, zlell);
	if (postdisplaylen)
	    ZS_memcpy(tmpline+predisplaylen+zlell, postdisplay,
		      postdisplaylen);

	tmpcs = zlecs + predisplaylen;
	tmpll = predisplaylen + zlell + postdisplaylen;
	tmpalloced = 1;
    } else {
	tmpline = zleline;
	tmpcs = zlecs;
	tmpll = zlell;
	tmpalloced = 0;
    }

    /* this will create region_highlights if it's still NULL */
    zle_set_highlight();

    if (region_active) {
	if (zlecs <= mark) {
	    region_highlights->start = zlecs;
	    region_highlights->end = mark;
	} else {
	    region_highlights->start = mark;
	    region_highlights->end = zlecs;
	}
    } else {
	region_highlights->start = region_highlights->end = -1;
    }

    if (clearlist && listshown > 0) {
	if (tccan(TCCLEAREOD)) {
	    int ovln = vln, ovcs = vcs;
	    REFRESH_STRING nb = nbuf[vln];

	    nbuf[vln] = obuf[vln];
	    moveto(nlnct, 0);
	    tcout(TCCLEAREOD);
	    moveto(ovln, ovcs);
	    nbuf[vln] = nb;
	} else {
	    invalidatelist();
	    moveto(0, 0);
	    clearflag = 0;
	    resetneeded = 1;
	}
	listshown = lastlistlen = 0;
	if (showinglist != -2)
	    showinglist = 0;
    }
    clearlist = 0;

#ifdef HAVE_SELECT
    cost = 0;			/* reset */
#endif

/* Nov 96: <mason>  I haven't checked how complete this is.  sgtty stuff may
   or may not work */
#if defined(SGTABTYPE)
    oxtabs = ((SGTTYFLAG & SGTABTYPE) == SGTABTYPE);
#else
    oxtabs = 0;
#endif

    cleareol = 0;		/* unset */
    more_start = more_end = 0;	/* unset */
    if (isset(SINGLELINEZLE) || lines < 3
	|| (termflags & (TERM_NOUP | TERM_BAD | TERM_UNKNOWN)))
	termflags |= TERM_SHORT;
    else
	termflags &= ~TERM_SHORT;
    if (resetneeded) {
	onumscrolls = 0;
	zsetterm();
#ifdef TIOCGWINSZ
	if (winchanged) {
	    moveto(0, 0);
	    t0 = olnct;		/* this is to clear extra lines even when */
	    winchanged = 0;	/* the terminal cannot TCCLEAREOD	  */
	    listshown = 0;
	}
#endif
	/* we probably should only have explicitly set attributes */
	tsetcap(TCALLATTRSOFF, 0);
	tsetcap(TCSTANDOUTEND, 0);
	tsetcap(TCUNDERLINEEND, 0);
	/* cheat on attribute unset */
	txtunset(TXTBOLDFACE|TXTSTANDOUT|TXTUNDERLINE|TXTDIRTY);

	if (trashedzle)
	    reexpandprompt(); 
	resetvideo();
	resetneeded = 0;	/* unset */
	oput_rpmpt = 0;		/* no right-prompt currently on screen */

        if (!clearflag) {
            if (tccan(TCCLEAREOD))
                tcout(TCCLEAREOD);
            else
                cleareol = 1;   /* request: clear to end of line */
	    if (listshown > 0)
		listshown = 0;
	}
        if (t0 > -1)
            olnct = (t0 < winh) ? t0 : winh;
        if (termflags & TERM_SHORT)
            vcs = 0;
	else if (!clearflag && lpromptbuf[0]) {
            zputs(lpromptbuf, shout);
	    if (lpromptwof == winw)
		zputs("\n", shout);	/* works with both hasam and !hasam */
	} else {
	    txtchange = pmpt_attr;
	    settextattributes(txtchange);
	}
	if (clearflag) {
	    zputc(&zr_cr);
	    vcs = 0;
	    moveto(0, lpromptw);
	}
	fflush(shout);
	clearf = clearflag;
    } else if (winw != columns || rwinh != lines)
	resetvideo();

/* now winw equals columns and winh equals lines 
   width comparisons can be made with winw, height comparisons with winh */

    if (termflags & TERM_SHORT) {
	singlerefresh(tmpline, tmpll, tmpcs);
	goto singlelineout;
    }

    if (tmpcs < 0) {
#ifdef DEBUG
	fprintf(stderr, "BUG: negative cursor position\n");
	fflush(stderr); 
#endif
	tmpcs = 0;
    }
    scs = tmpline + tmpcs;
    numscrolls = 0;

/* first, we generate the video line buffers so we know what to put on
   the screen - also determine final cursor position (nvln, nvcs) */

    /* Deemed necessary by PWS 1995/05/15 due to kill-line problems */
    if (!*nbuf)
	*nbuf = (REFRESH_STRING)zalloc((winw + 2) * sizeof(**nbuf));

    memset(&rpms, 0, sizeof(rpms));
    rpms.nvln = -1;

    rpms.s = nbuf[rpms.ln = 0] + lpromptw;
    rpms.sen = *nbuf + winw;
    for (t = tmpline, tmppos = 0; tmppos < tmpll; t++, tmppos++) {
	int base_atr_on = 0, base_atr_off = 0, ireg;
	struct region_highlight *rhp;
	/*
	 * Calculate attribute based on region.
	 * HERE: we may need to be smarter about turning
	 * attributes off if bailing out before the end of the
	 * region.
	 */
	for (ireg = 0, rhp = region_highlights;
	     ireg < n_region_highlights;
	     ireg++, rhp++) {
	    int offset;
	    if (rhp->flags & ZRH_PREDISPLAY)
		offset = 0;	/* include predisplay in start end */
	    else
		offset = predisplaylen; /* increment over it */
	    if (rhp->start + offset <= tmppos &&
		tmppos < rhp->end + offset) {
		base_atr_on |= rhp->atr;
		if (tmppos == rhp->end + offset - 1 ||
		    tmppos == tmpll - 1)
		    base_atr_off |= rhp->atr << TXT_ATTR_OFF_ON_SHIFT;
	    }
	}

	if (t == scs)			/* if cursor is here, remember it */
	    rpms.nvcs = rpms.s - nbuf[rpms.nvln = rpms.ln];

	if (*t == ZWC('\n')){		/* newline */
	    /* text not wrapped */
	    if (nextline(&rpms, 0))
		break;
	} else if (*t == ZWC('\t')) {		/* tab */
	    t0 = rpms.s - nbuf[rpms.ln];
	    if ((t0 | 7) + 1 >= winw) {
		/* text wrapped */
		if (nextline(&rpms, 1))
		    break;
	    } else {
		do {
		    rpms.s->chr = ZWC(' ');
		    rpms.s->atr = base_atr_on;
		    rpms.s++;
		} while ((++t0) & 7);
		rpms.s[-1].atr |= base_atr_off;
	    }
	}
#ifdef MULTIBYTE_SUPPORT
	else if (iswprint(*t) && (width = wcwidth(*t)) > 0) {
	    if (width > rpms.sen - rpms.s) {
		int started = 0;
		/*
		 * Too wide to fit.  Insert spaces to end of current line.
		 */
		do {
		    /* HERE highlight */
		    rpms.s->chr = ZWC(' ');
		    if (!started)
			started = 1;
		    rpms.s->atr = special_atr_on | base_atr_on;
		    rpms.s++;
		} while (rpms.s < rpms.sen);
		if (started)
		    rpms.s[-1].atr |= special_atr_off | base_atr_off;
		if (nextline(&rpms, 1))
		    break;
		if (t == scs) {
		    /* Update cursor to this point */
		    rpms.nvcs = rpms.s - nbuf[rpms.nvln = rpms.ln];
		}
	    }
	    if (width > rpms.sen - rpms.s || width == 0) {
		/*
		 * The screen width is too small to fit even one
		 * occurrence.
		 *
		 * HERE highlight
		 */
		rpms.s->chr = ZWC('?');
		rpms.s->atr = special_atr_on | special_atr_off |
		    base_atr_on | base_atr_off;
		rpms.s++;
	    } else {
		/* We can fit it without reaching the end of the line. */
		rpms.s->chr = *t;
		/*
		 * As we don't actually output the WEOF, we attach
		 * any off attributes to the character itself.
		 */
		rpms.s->atr = base_atr_on | base_atr_off;
		rpms.s++;
		while (--width > 0) {
		    rpms.s->chr = WEOF;
		    /* Not used, but be consistent... */
		    rpms.s->atr = base_atr_on | base_atr_off;
		    rpms.s++;
		}
	    }
	}
#endif
	else if (ZC_icntrl(*t)
#ifdef MULTIBYTE_SUPPORT
		 && (unsigned)*t <= 0xffU
#endif
	    ) {	/* other control character */
	    /* HERE highlight */
	    rpms.s->chr = ZWC('^');
	    rpms.s->atr = special_atr_on | base_atr_on;
	    rpms.s++;
	    if (rpms.s == rpms.sen) {
		/* text wrapped */
		rpms.s[-1].atr |= special_atr_off | base_atr_off;
		if (nextline(&rpms, 1))
		    break;
	    }
	    rpms.s->chr = (((unsigned int)*t & ~0x80u) > 31) ?
		ZWC('?') : (*t | ZWC('@'));
	    rpms.s->atr = special_atr_on | special_atr_off |
		base_atr_on | base_atr_off;
	    rpms.s++;
	}
#ifdef MULTIBYTE_SUPPORT
	else {
	    /*
	     * Not printable or zero width.
	     * Resort to hackery.
	     *
	     * HERE: highlight
	     */
	    char dispchars[11];
	    char *dispptr = dispchars;
	    wchar_t wc;
	    int started = 0;

	    if ((unsigned)*t > 0xffffU) {
		sprintf(dispchars, "<%.08x>", (unsigned)*t);
	    } else {
		sprintf(dispchars, "<%.04x>", (unsigned)*t);
	    }
	    while (*dispptr) {
		if (mbtowc(&wc, dispptr, 1) == 1 /* paranoia */)
		{
		    rpms.s->chr = wc;
		    if (!started)
			started = 1;
		    rpms.s->atr = special_atr_on | base_atr_on;
		    rpms.s++;
		    if (rpms.s == rpms.sen) {
			/* text wrapped */
			if (started) {
			    rpms.s[-1].atr |= special_atr_off | base_atr_off;
			    started = 0;
			}
			if (nextline(&rpms, 1))
			    break;
		    }
		}
		dispptr++;
	    }
	    if (started)
		rpms.s[-1].atr |= special_atr_off | base_atr_off;
	    if (*dispptr) /* nextline said stop processing */
		break;
	}
#else
	else {			/* normal character */
	    rpms.s->chr = *t;
	    rpms.s->atr = base_atr_on | base_atr_off;
	    rpms.s++;
	}
#endif
	if (rpms.s == rpms.sen) {
	    /* text wrapped */
	    if (nextline(&rpms, 1))
		break;
	}
    }

/* if we're really on the next line, don't fake it; do everything properly */
    if (t == scs &&
	(rpms.nvcs = rpms.s - (nbuf[rpms.nvln = rpms.ln])) == winw) {
	/* text wrapped */
	(void)nextline(&rpms, 1);
	*rpms.s = zr_zr;
	rpms.nvcs = 0;
	rpms.nvln++;
    }

    if (t != tmpline + tmpll)
	more_end = 1;

    if (statusline) {
	rpms.tosln = rpms.ln + 1;
	nbuf[rpms.ln][winw + 1] = zr_zr;	/* text not wrapped */
	snextline(&rpms);
	u = statusline;
	for (; u < statusline + statusll; u++) {
#ifdef MULTIBYTE_SUPPORT
	    if (iswprint(*u)) {
		int width = wcwidth(*u);
		/* Handle wide characters as above */
		if (width > rpms.sen - rpms.s) {
		    do {
			*rpms.s++ = zr_sp;
		    } while (rpms.s < rpms.sen);
		    nbuf[rpms.ln][winw + 1] = zr_nl;
		    snextline(&rpms);
		}
		if (width > rpms.sen - rpms.s) {
		    /* HERE: highlight */
		    rpms.s->chr = ZWC('?');
		    rpms.s->atr = special_atr_on | special_atr_off;
		    rpms.s++;
		} else {
		    rpms.s->chr = *u;
		    rpms.s->atr = 0;
		    rpms.s++;
		    while (--width > 0) {
			rpms.s->chr = WEOF;
			rpms.s->atr = 0;
			rpms.s++;
		    }
		}
	    }
	    else
#endif
	    if (ZC_icntrl(*u)) { /* simplified processing in the status line */
		/* HERE: highlight */
		rpms.s->chr = ZWC('^');
		rpms.s->atr = special_atr_on;
		rpms.s++;
		if (rpms.s == rpms.sen) {
		    nbuf[rpms.ln][winw + 1] = zr_nl;/* text wrapped */
		    snextline(&rpms);
		}
		rpms.s->chr = (((unsigned int)*u & ~0x80u) > 31)
		    ? ZWC('?') : (*u | ZWC('@'));
		rpms.s->atr = special_atr_on | special_atr_off;
		rpms.s++;
	    } else {
		rpms.s->chr = *u;
		rpms.s->atr = 0;
		rpms.s++;
	    }
	    if (rpms.s == rpms.sen) {
		nbuf[rpms.ln][winw + 1] = zr_nl;	/* text wrapped */
		snextline(&rpms);
	    }
	}
	if (rpms.s == rpms.sen) {
	    /*
	     * I suppose we don't modify nbuf[rpms.ln][winw+1] here
	     * since we're right at the end?
	     */
	    snextline(&rpms);
	}
    }
    *rpms.s = zr_zr;

/* insert <.... at end of last line if there is more text past end of screen */
    if (more_end) {
#ifdef MULTIBYTE_SUPPORT
	int extra_ellipsis = 0;
#endif
	if (!statusline)
	    rpms.tosln = winh;
	rpms.s = nbuf[rpms.tosln - 1];
	rpms.sen = rpms.s + winw - 7;
	for (; rpms.s < rpms.sen; rpms.s++) {
	    if (rpms.s->chr == ZWC('\0')) {
		ZR_memset(rpms.s, zr_sp, rpms.sen - rpms.s);
		/* make sure we don't trigger the WEOF test */
		rpms.sen->chr = ZWC('\0');
		break;
	    }
	}
	/* rpms.s is no longer needed */
#ifdef MULTIBYTE_SUPPORT
	/*
	 * Ensure we don't start overwriting in the middle of a wide
	 * character.
	 */
	while(rpms.sen > nbuf[rpms.tosln - 1] && rpms.sen->chr == WEOF) {
	    extra_ellipsis++;
	    rpms.sen--;
	}
#endif
	ZR_memcpy(rpms.sen, zr_end_ellipsis, ZR_END_ELLIPSIS_SIZE);
#ifdef MULTIBYTE_SUPPORT
	/* Extend to the end if we backed off for a wide character */
	if (extra_ellipsis) {
	    rpms.sen += ZR_END_ELLIPSIS_SIZE;
	    ZR_memset(rpms.sen, zr_dt, extra_ellipsis);
	}
#endif
	nbuf[rpms.tosln - 1][winw] = nbuf[rpms.tosln - 1][winw + 1] = zr_zr;
    }

/* insert <....> at end of first status line if status is too big */
    if (rpms.more_status) {
#ifdef MULTIBYTE_SUPPORT
	int extra_ellipsis = 0;
#endif
	rpms.s = nbuf[rpms.tosln];
	rpms.sen = rpms.s + winw - 8;
	for (; rpms.s < rpms.sen; rpms.s++) {
	    if (rpms.s->chr == ZWC('\0')) {
		ZR_memset(rpms.s, zr_sp, rpms.sen - rpms.s);
		break;
	    }
	}
	/* rpms.s is no longer needed */
#ifdef MULTIBYTE_SUPPORT
	/*
	 * Ensure we don't start overwriting in the middle of a wide
	 * character.
	 */
	while(rpms.sen > nbuf[rpms.tosln - 1] && rpms.sen->chr == WEOF) {
	    extra_ellipsis++;
	    rpms.sen--;
	}
#endif
	ZR_memcpy(rpms.sen, zr_mid_ellipsis1, ZR_MID_ELLIPSIS1_SIZE);
	rpms.sen += ZR_MID_ELLIPSIS1_SIZE;
#ifdef MULTIBYTE_SUPPORT
	/* Extend if we backed off for a wide character */
	if (extra_ellipsis) {
	    ZR_memset(rpms.sen, zr_dt, extra_ellipsis);
	    rpms.sen += extra_ellipsis;
	}
#endif
	ZR_memcpy(rpms.sen, zr_mid_ellipsis2, ZR_MID_ELLIPSIS2_SIZE);
	nbuf[rpms.tosln][winw] = nbuf[rpms.tosln][winw + 1] = zr_zr;
    }

    nlnct = rpms.ln + 1;
    for (iln = nlnct; iln < winh; iln++) {
	zfree(nbuf[iln], (winw + 2) * sizeof(**nbuf));
	nbuf[iln] = NULL;
    }

/* determine whether the right-prompt exists and can fit on the screen */
    if (!more_start) {
	if (trashedzle && opts[TRANSIENTRPROMPT])
	    put_rpmpt = 0;
	else
	    put_rpmpt = rprompth == 1 && rpromptbuf[0] &&
		!strchr(rpromptbuf, '\t') &&
		(int)ZR_strlen(nbuf[0]) + rpromptw < winw - 1;
    } else {
/* insert >.... on first line if there is more text before start of screen */
	ZR_memset(nbuf[0], zr_sp, lpromptw);
	t0 = winw - lpromptw;
	t0 = t0 > ZR_START_ELLIPSIS_SIZE ? ZR_START_ELLIPSIS_SIZE : t0;
	ZR_memcpy(nbuf[0] + lpromptw, zr_start_ellipsis, t0);
	ZR_memset(nbuf[0] + lpromptw + t0, zr_sp, winw - t0 - lpromptw);
	nbuf[0][winw] = nbuf[0][winw + 1] = zr_zr;
    }

    for (iln = 0; iln < nlnct; iln++) {
	/* if we have more lines than last time, clear the newly-used lines */
	if (iln >= olnct)
	    cleareol = 1;

    /* if old line and new line are different,
       see if we can insert/delete a line to speed up update */

	if (!clearf && iln > 0 && iln < olnct - 1 &&
	    !(hasam && vcs == winw) &&
	    nbuf[iln] && obuf[iln] &&
	    ZR_strncmp(nbuf[iln], obuf[iln], 16)) {
	    if (tccan(TCDELLINE) && obuf[iln + 1] &&
		obuf[iln + 1][0].chr && nbuf[iln] &&
		!ZR_strncmp(nbuf[iln], obuf[iln + 1], 16)) {
		moveto(iln, 0);
		tcout(TCDELLINE);
		zfree(obuf[iln], (winw + 2) * sizeof(**obuf));
		for (t0 = iln; t0 != olnct; t0++)
		    obuf[t0] = obuf[t0 + 1];
		obuf[--olnct] = NULL;
	    }
	/* don't try to insert a line if olnct = vmaxln (vmaxln is the number
	   of lines that have been displayed by this routine) so that we don't
	   go off the end of the screen. */

	    else if (tccan(TCINSLINE) && olnct < vmaxln && nbuf[iln + 1] &&
		     obuf[iln] && !ZR_strncmp(nbuf[iln + 1], 
					      obuf[iln], 16)) {
		moveto(iln, 0);
		tcout(TCINSLINE);
		for (t0 = olnct; t0 != iln; t0--)
		    obuf[t0] = obuf[t0 - 1];
		obuf[iln] = NULL;
		olnct++;
	    }
	}

    /* update the single line */
	refreshline(iln);

    /* output the right-prompt if appropriate */
	if (put_rpmpt && !iln && !oput_rpmpt) {
	    moveto(0, winw - 1 - rpromptw);
	    zputs(rpromptbuf, shout);
	    vcs = winw - 1;
	/* reset character attributes to that set by the main prompt */
	    txtchange = pmpt_attr;
	    if (txtchangeisset(txtchange, TXTNOBOLDFACE) &&
		(rpmpt_attr & TXTBOLDFACE))
		tsetcap(TCALLATTRSOFF, 0);
	    if (txtchangeisset(txtchange, TXTNOSTANDOUT) &&
		(rpmpt_attr & TXTSTANDOUT))
		tsetcap(TCSTANDOUTEND, 0);
	    if (txtchangeisset(txtchange, TXTNOUNDERLINE) &&
		(rpmpt_attr & TXTUNDERLINE))
		tsetcap(TCUNDERLINEEND, 0);
	    if (txtchangeisset(txtchange, TXTBOLDFACE) &&
		(rpmpt_attr & TXTNOBOLDFACE))
		tsetcap(TCBOLDFACEBEG, 0);
	    if (txtchangeisset(txtchange, TXTSTANDOUT) &&
		(rpmpt_attr & TXTNOSTANDOUT))
		tsetcap(TCSTANDOUTBEG, 0);
	    if (txtchangeisset(txtchange, TXTUNDERLINE) &&
		(rpmpt_attr & TXTNOUNDERLINE))
		tsetcap(TCUNDERLINEBEG, 0);
	}
    }

/* if old buffer had extra lines, set them to be cleared and refresh them
individually */

    if (olnct > nlnct) {
	cleareol = 1;
	for (iln = nlnct; iln < olnct; iln++)
	    refreshline(iln);
    }

/* reset character attributes */
    if (clearf && postedit) {
	if ((txtchange = pmpt_attr ? pmpt_attr : rpmpt_attr))
	    settextattributes(txtchange);
    }
    clearf = 0;
    oput_rpmpt = put_rpmpt;

/* move to the new cursor position */
    moveto(rpms.nvln, rpms.nvcs);

/* swap old and new buffers - better than freeing/allocating every time */
    qbuf = nbuf;
    nbuf = obuf;
    obuf = qbuf;
/* store current values so we can use them next time */
    ovln = rpms.nvln;
    olnct = nlnct;
    onumscrolls = numscrolls;
    if (nlnct > vmaxln)
	vmaxln = nlnct;
singlelineout:
    fflush(shout);		/* make sure everything is written out */

    if (tmpalloced)
	zfree(tmpline, tmpll * sizeof(*tmpline));

    /* if we have a new list showing, note it; if part of the list has been
    overwritten, redisplay it. We have to metafy line back before calling
    completion code */
    if (showinglist == -2 || (showinglist > 0 && showinglist < nlnct)) {
	if (remetafy) {
	    metafy_line();
	    remetafy = 0;
	}
	inlist = 1;
	listmatches();
	inlist = 0;
	zrefresh();
    }
    if (showinglist == -1)
	showinglist = nlnct;

    if (remetafy)
	metafy_line();
}

#define tcinscost(X)   (tccan(TCMULTINS) ? tclen[TCMULTINS] : (X)*tclen[TCINS])
#define tcdelcost(X)   (tccan(TCMULTDEL) ? tclen[TCMULTDEL] : (X)*tclen[TCDEL])
#define tc_delchars(X)	(void) tcmultout(TCDEL, TCMULTDEL, (X))
#define tc_inschars(X)	(void) tcmultout(TCINS, TCMULTINS, (X))
#define tc_upcurs(X)	(void) tcmultout(TCUP, TCMULTUP, (X))
#define tc_leftcurs(X)	(void) tcmultout(TCLEFT, TCMULTLEFT, (X))

static int
wpfxlen(const REFRESH_ELEMENT *s, const REFRESH_ELEMENT *t)
{
    int i = 0;

    while (s->chr && ZR_equal(*s, *t))
	s++, t++, i++;
    return i;
}

/* refresh one line, using whatever speed-up tricks are provided by the tty */

/**/
static void
refreshline(int ln)
{
    REFRESH_STRING nl, ol, p1;	/* line buffer pointers			 */
    int ccs = 0,		/* temporary count for cursor position	 */
	char_ins = 0,		/* number of characters inserted/deleted */
	col_cleareol,		/* clear to end-of-line from this column */
	i, j,			/* tmp					 */
	ins_last,		/* insert pushed last character off line */
	nllen, ollen,		/* new and old line buffer lengths	 */
	rnllen;			/* real new line buffer length		 */

/* 0: setup */
    nl = nbuf[ln];
    rnllen = nllen = nl ? ZR_strlen(nl) : 0;
    if (obuf[ln]) {
	ol = obuf[ln];
	ollen = ZR_strlen(ol);
    }
    else {
	static REFRESH_ELEMENT nullchr = { ZWC('\0'), 0 };
	ol = &nullchr;
	ollen = 0;
    }

/* optimisation: can easily happen for clearing old lines.  If the terminal has
   the capability, then this is the easiest way to skip unnecessary stuff */
    if (cleareol && !nllen && !(hasam && ln < nlnct - 1)
	&& tccan(TCCLEAREOL)) {
	moveto(ln, 0);
	tcout(TCCLEAREOL);
	return;	
    }

/* 1: pad out the new buffer with spaces to contain _all_ of the characters
      which need to be written. do this now to allow some pre-processing */

    if (cleareol 		/* request to clear to end of line */
	|| (!nllen && (ln != 0 || !put_rpmpt))	/* no line buffer given */
	|| (ln == 0 && (put_rpmpt != oput_rpmpt))) {	/* prompt changed */
	p1 = zhalloc((winw + 2) * sizeof(*p1));
	if (nllen)
	    ZR_memcpy(p1, nl, nllen);
	ZR_memset(p1 + nllen, zr_sp, winw - nllen);
	p1[winw] = zr_zr;
	p1[winw + 1] = (nllen < winw) ? zr_zr : nl[winw + 1];
	if (ln && nbuf[ln])
	    ZR_memcpy(nl, p1, winw + 2);	/* next time obuf will be up-to-date */
	else
	    nl = p1;		/* don't keep the padding for prompt line */
	nllen = winw;
    } else if (ollen > nllen) { /* make new line at least as long as old */
	p1 = zhalloc((ollen + 1) * sizeof(*p1));
	ZR_memcpy(p1, nl, nllen);
	ZR_memset(p1 + nllen, zr_sp, ollen - nllen);
	p1[ollen] = zr_zr;
	nl = p1;
	nllen = ollen;
    }

/* 2: see if we can clear to end-of-line, and if it's faster, work out where
   to do it from - we can normally only do so if there's no right-prompt.
   With automatic margins, we shouldn't do it if there is another line, in
   case it messes up cut and paste. */

    if (hasam && ln < nlnct - 1 && rnllen == winw)
	col_cleareol = -2;	/* clearing eol would be evil so don't */
    else {
	col_cleareol = -1;
	if (tccan(TCCLEAREOL) && (nllen == winw || put_rpmpt != oput_rpmpt)) {
	    /* HERE: watch for change of attributes */
	    for (i = nllen; i && ZR_equal(nl[i - 1], zr_sp); i--)
		;
	    for (j = ollen; j && ZR_equal(ol[j - 1], zr_sp); j--)
		;
	    if ((j > i + tclen[TCCLEAREOL])	/* new buf has enough spaces */
		|| (nllen == winw && ZR_equal(nl[winw - 1], zr_sp)))
		col_cleareol = i;
	}
    }

/* 2b: first a new trick for automargin niceness - good for cut and paste */

    if (hasam && vcs == winw) {
	if (nbuf[vln] && nbuf[vln][vcs + 1].chr == ZWC('\n')) {
	    vln++, vcs = 1;
            if (nbuf[vln]  && nbuf[vln]->chr) {
		zputc(nbuf[vln]);
	    } else
		zputc(&zr_sp);  /* I don't think this should happen */
	    if (ln == vln) {	/* better safe than sorry */
		nl++;
		if (ol->chr)
		    ol++;
		ccs = 1;
	    }			/* else  hmmm... I wonder what happened */
	} else {
	    vln++, vcs = 0;
	    zputc(&zr_nl);
	}
    }
    ins_last = 0;

/* 2c: if we're on the first line, start checking at the end of the prompt;
   we shouldn't be doing anything within the prompt */

    if (ln == 0 && lpromptw) {
	i = lpromptw - ccs;
	j = ZR_strlen(ol);
	nl += i;
	ol += (i > j ? j : i);	/* if ol is too short, point it to '\0' */
	ccs = lpromptw;
    }

#ifdef MULTIBYTE_SUPPORT
    /*
     * Realign to a real character after any jiggery pokery at
     * the start of the line.
     */
    while (nl->chr == WEOF) {
	nl++, ccs++, vcs++;
	if (ol->chr)
	    ol++;
    }
#endif

/* 3: main display loop - write out the buffer using whatever tricks we can */

    for (;;) {
#ifdef MULTIBYTE_SUPPORT
	if ((!nl->chr || nl->chr != WEOF) && (!ol->chr || ol->chr != WEOF)) {
#endif
	    if (nl->chr && ol->chr && ZR_equal(nl[1], ol[1])) {
		/* skip only if second chars match */
#ifdef MULTIBYTE_SUPPORT
		int ccs_was = ccs;
#endif
		/* skip past all matching characters */
		for (; nl->chr && ZR_equal(*nl, *ol); nl++, ol++, ccs++)
		    ;
#ifdef MULTIBYTE_SUPPORT
		/* Make sure ol and nl are pointing to real characters */
		while ((nl->chr == WEOF || ol->chr == WEOF) && ccs > ccs_was) {
		    nl--;
		    ol--;
		    ccs--;
		}
#endif
	    }

	    if (!nl->chr) {
		if (ccs == winw && hasam && char_ins > 0 && ins_last
		    && vcs != winw) {
		    nl--;           /* we can assume we can go back here */
		    moveto(ln, winw - 1);
		    zputc(nl);
		    vcs++;
		    return;         /* write last character in line */
		}
		if ((char_ins <= 0) || (ccs >= winw))    /* written everything */
		    return;
		if (tccan(TCCLEAREOL) && (char_ins >= tclen[TCCLEAREOL])
		    && col_cleareol != -2)
		    /* we've got junk on the right yet to clear */
		    col_cleareol = 0;	/* force a clear to end of line */
	    }

	    moveto(ln, ccs);	/* move to where we do all output from */

	    /* if we can finish quickly, do so */
	    if ((col_cleareol >= 0) && (ccs >= col_cleareol)) {
		tcout(TCCLEAREOL);
		return;
	    }

	    /* we've written out the new but yet to clear rubbish due to inserts */
	    if (!nl->chr) {
		i = (winw - ccs < char_ins) ? (winw - ccs) : char_ins;
		if (tccan(TCDEL) && (tcdelcost(i) <= i + 1))
		    tc_delchars(i);
		else {
		    vcs += i;
		    while (i-- > 0)
			zputc(&zr_sp);
		}
		return;
	    }

	    /* if we've reached the end of the old buffer, then there are few tricks
	       we can do, so we just dump out what we must and clear if we can */
	    if (!ol->chr) {
		i = (col_cleareol >= 0) ? col_cleareol : nllen;
		i -= vcs;
		if (i < 0) {
		    /*
		     * This shouldn't be necessary, but it's better
		     * than a crash if there's a bug somewhere else,
		     * so report in debug mode.
		     */
		    DPUTS(1, "BUG: badly calculated old line width in refresh");
		    i = 0;
		}
		zwrite(nl, i);
		vcs += i;
		if (col_cleareol >= 0)
		    tcout(TCCLEAREOL);
		return;
	    }

	    /* inserting & deleting chars: we can if there's no right-prompt */
	    if ((ln || !put_rpmpt || !oput_rpmpt) 
#ifdef MULTIBYTE_SUPPORT
		&& ol->chr != WEOF && nl->chr != WEOF
#endif
		&& nl[1].chr && ol[1].chr && !ZR_equal(nl[1], ol[1])) { 

		/* deleting characters - see if we can find a match series that
		   makes it cheaper to delete intermediate characters
		   eg. oldline: hifoobar \ hopefully cheaper here to delete two
		   newline: foobar	 / characters, then we have six matches */
		if (tccan(TCDEL)) {
		    for (i = 1; ol[i].chr; i++)
			if (tcdelcost(i) < wpfxlen(ol + i, nl)) {
			    tc_delchars(i);
			    ol += i;
			    char_ins -= i;
#ifdef MULTIBYTE_SUPPORT
			    while (ol->chr == WEOF) {
				ol++;
				char_ins--;
			    }
#endif
			    /*
			     * If the sequence we're deleting ended
			     * by turning off an attribute, make sure
			     * it stays turned off.  I don't think we
			     * should need this.
			     */
			    if (ol[-1].atr & TXT_ATTR_OFF_MASK)
				settextattributes(ol[-1].atr &
						  TXT_ATTR_OFF_MASK);
			    i = 0;
			    break;
			}
		    if (!i)
			continue;
		}
		/* inserting characters - characters pushed off the right should be
		   annihilated, but we don't do this if we're on the last line lest
		   undesired scrolling occurs due to `illegal' characters on screen */

		if (tccan(TCINS) && (vln != lines - 1)) {	/* not on last line */
		    for (i = 1; nl[i].chr; i++)
			if (tcinscost(i) < wpfxlen(nl + i, ol)) {
			    tc_inschars(i);
			    zwrite(nl, i);
			    nl += i;
#ifdef MULTIBYTE_SUPPORT
			    while (nl->chr == WEOF) {
				nl++;
				i++;
			    }
#endif
			    char_ins += i;
			    ccs = (vcs += i);
			    /* if we've pushed off the right, truncate oldline */
			    for (i = 0; ol[i].chr && i < winw - ccs; i++);
#ifdef MULTIBYTE_SUPPORT
			    while (ol[i].chr == WEOF)
				i++;
#endif
			    if (i >= winw - ccs) {
				ol[i] = zr_zr;
				ins_last = 1;
			    }
			    i = 0;
			    break;
			}
		    if (!i)
			continue;
		}
	    }
#ifdef MULTIBYTE_SUPPORT
	}
#endif
    /* we can't do any fancy tricks, so just dump the single character
       and keep on trying */
#ifdef MULTIBYTE_SUPPORT
	/*
	 * in case we were tidying up a funny-width character when we
	 * reached the end of the new line...
	 */
	if (!nl->chr)
	    break;
	do {
#endif
	    /*
	     * If an attribute was on here but isn't any more,
	     * output the sequence to turn it off.
	     */
	    int now_off = ol->atr & ~nl->atr & TXT_ATTR_ON_MASK;
	    if (now_off)
		settextattributes(now_off << TXT_ATTR_OFF_ON_SHIFT);

	    zputc(nl);
	    nl++, ol++;
	    ccs++, vcs++;
#ifdef MULTIBYTE_SUPPORT
	    /*
	     * Make sure we always overwrite the complete width of
	     * a character that was there before.
	     */
	} while ((ol->chr == WEOF && nl->chr) ||
		 (nl->chr == WEOF && ol->chr));
#endif
    }
}

/* move the cursor to line ln (relative to the prompt line),
   absolute column cl; update vln, vcs - video line and column */

/**/
void
moveto(int ln, int cl)
{
    const REFRESH_ELEMENT *rep;

    if (vcs == winw) {
	vln++, vcs = 0;
	if (!hasam) {
	    zputc(&zr_cr);
	    zputc(&zr_nl);
	} else {
	    if ((vln < nlnct) && nbuf[vln] && nbuf[vln]->chr)
		rep = nbuf[vln];
	    else
		rep = &zr_sp;
	    zputc(rep);
	    zputc(&zr_cr);
	    if ((vln < olnct) && obuf[vln] && obuf[vln]->chr)
		*obuf[vln] = *rep;
	}
    }

    if (ln == vln && cl == vcs)
	return;

/* move up */
    if (ln < vln) {
	tc_upcurs(vln - ln);
	vln = ln;
    }
/* move down; if we might go off the end of the screen, use newlines
   instead of TCDOWN */

    while (ln > vln) {
	if (vln < vmaxln - 1) {
	    if (ln > vmaxln - 1) {
		if (tc_downcurs(vmaxln - 1 - vln))
		    vcs = 0;
		vln = vmaxln - 1;
	    } else {
		if (tc_downcurs(ln - vln))
		    vcs = 0;
		vln = ln;
		continue;
	    }
	}
	zputc(&zr_cr), vcs = 0; /* safety precaution */
	while (ln > vln) {
	    zputc(&zr_nl);
	    vln++;
	}
    }

    if (cl != vcs)
        singmoveto(cl);
}

/**/
mod_export int
tcmultout(int cap, int multcap, int ct)
{
    if (tccan(multcap) && (!tccan(cap) || tclen[multcap] <= tclen[cap] * ct)) {
	tcoutarg(multcap, ct);
	return 1;
    } else if (tccan(cap)) {
	while (ct--)
	    tcout(cap);
	return 1;
    }
    return 0;
}

/* ct: number of characters to move across */
/**/
static void
tc_rightcurs(int ct)
{
    int cl,			/* ``desired'' absolute horizontal position */
	i = vcs,		/* cursor position after initial movements  */
	j;
    REFRESH_STRING t;

    cl = ct + vcs;

/* do a multright if we can - it's the most reliable */
    if (tccan(TCMULTRIGHT)) {
	tcoutarg(TCMULTRIGHT, ct);
	return;
    }

/* do an absolute horizontal position if we can */
    if (tccan(TCHORIZPOS)) {
	tcoutarg(TCHORIZPOS, cl);
	return;
    }

/* XXX: should really check "it" in termcap and use / and % */
/* try tabs if tabs are non destructive and multright is not possible */
    if (!oxtabs && tccan(TCNEXTTAB) && ((vcs | 7) < cl)) {
	i = (vcs | 7) + 1;
	tcout(TCNEXTTAB);
	for ( ; i + 8 <= cl; i += 8)
	    tcout(TCNEXTTAB);
	if ((ct = cl - i) == 0) /* number of chars still to move across */
	    return;
    }

/* otherwise _carefully_ write the contents of the video buffer.
   if we're anywhere in the prompt, goto the left column and write the whole
   prompt out.

   If strlen(lpromptbuf) == lpromptw, we can cheat and output
   the appropriate chunk of the string.  This test relies on the
   fact that any funny business will always make the length of
   the string larger than the printing width, so if they're the same
   we have only ASCII characters or a single-byte extension of ASCII.
   Unfortunately this trick won't work if there are potentially
   characters occupying more than one column.  We could flag that
   this has happened (since it's not that common to have characters
   wider than one column), but for now it's easier not to use the
   trick if we are using wcwidth() on the prompt.  It's not that
   common to be editing in the middle of the prompt anyway, I would
   think.
   */
    if (vln == 0 && i < lpromptw && !(termflags & TERM_SHORT)) {
#ifndef MULTIBYTE_SUPPORT
	if ((int)strlen(lpromptbuf) == lpromptw)
	    fputs(lpromptbuf + i, shout);
	else 
#endif
	if (tccan(TCRIGHT) && (tclen[TCRIGHT] * ct <= ztrlen(lpromptbuf)))
	    /* it is cheaper to send TCRIGHT than reprint the whole prompt */
	    for (ct = lpromptw - i; ct--; )
		tcout(TCRIGHT);
        else {
	    if (i != 0)
		zputc(&zr_cr);
	    tc_upcurs(lprompth - 1);
	    zputs(lpromptbuf, shout);
	    if (lpromptwof == winw)
		zputs("\n", shout);	/* works with both hasam and !hasam */
	}
	i = lpromptw;
	ct = cl - i;
    }

    if (nbuf[vln]) {
	for (j = 0, t = nbuf[vln]; t->chr && (j < i); j++, t++);
	if (j == i)
	    for ( ; t->chr && ct; ct--, t++)
		zputc(t);
    }
    while (ct--)
	zputc(&zr_sp);	/* not my fault your terminal can't go right */
}

/**/
mod_export int
tc_downcurs(int ct)
{
    int ret = 0;

    if (ct && !tcmultout(TCDOWN, TCMULTDOWN, ct)) {
	while (ct--)
	    zputc(&zr_nl);
	zputc(&zr_cr), ret = -1;
    }
    return ret;
}

/**/
mod_export void
tcout(int cap)
{
    tputs(tcstr[cap], 1, putshout);
    SELECT_ADD_COST(tclen[cap]);
}

/**/
static void
tcoutarg(int cap, int arg)
{
    char *result;

    result = tgoto(tcstr[cap], arg, arg);
    tputs(result, 1, putshout);
    SELECT_ADD_COST(strlen(result));
}

/**/
mod_export int
clearscreen(UNUSED(char **args))
{
    tcout(TCCLEARSCREEN);
    resetneeded = 1;
    clearflag = 0;
    return 0;
}

/**/
mod_export int
redisplay(UNUSED(char **args))
{
    moveto(0, 0);
    zputc(&zr_cr);		/* extra care */
    tc_upcurs(lprompth - 1);
    resetneeded = 1;
    clearflag = 0;
    return 0;
}

/*
 * Show as much of the line buffer as we can in single line mode.
 * TBD: all termcap effects are turned off in this mode, so
 * there's no point in using character attributes.  We should
 * decide what we're going to do and either remove the handling
 * from here or enable it in tsetcap().
 */

/**/
static void
singlerefresh(ZLE_STRING_T tmpline, int tmpll, int tmpcs)
{
    REFRESH_STRING vbuf, vp,	/* video buffer and pointer    */
	*qbuf,			/* tmp			       */
	refreshop;	        /* pointer to old video buffer */
    int t0,			/* tmp			       */
	vsiz,			/* size of new video buffer    */
	nvcs = 0,		/* new video cursor column     */
	owinpos = winpos,	/* previous window position    */
	owinprompt = winprompt;	/* previous winprompt          */

    nlnct = 1;
/* generate the new line buffer completely */
    for (vsiz = 1 + lpromptw, t0 = 0; t0 != tmpll; t0++, vsiz++)
	if (tmpline[t0] == ZWC('\t'))
	    vsiz = (vsiz | 7) + 1;
#ifdef MULTIBYTE_SUPPORT
	else if (iswprint(tmpline[t0]))
	    vsiz += wcwidth(tmpline[t0]);
#endif
	else if (ZC_icntrl(tmpline[t0]))
	    vsiz++;
    vbuf = (REFRESH_STRING)zalloc(vsiz * sizeof(*vbuf));

    if (tmpcs < 0) {
#ifdef DEBUG
	fprintf(stderr, "BUG: negative cursor position\n");
	fflush(stderr); 
#endif
	tmpcs = 0;
    }

    /* prompt is not directly copied into the video buffer */
    ZR_memset(vbuf, zr_sp, lpromptw);
    vp = vbuf + lpromptw;
    *vp = zr_zr;

    for (t0 = 0; t0 < tmpll; t0++) {
	int base_atr_on = 0, base_atr_off = 0, ireg;
	struct region_highlight *rhp;
	/*
	 * Calculate attribute based on region.
	 * HERE: we may need to be smarter about turning
	 * attributes off if bailing out before the end of the
	 * region.
	 */
	for (ireg = 0, rhp = region_highlights;
	     ireg < n_region_highlights;
	     ireg++, rhp++) {
	    int offset;
	    if (rhp->flags & ZRH_PREDISPLAY)
		offset = 0;	/* include predisplay in start end */
	    else
		offset = predisplaylen; /* increment over it */
	    if (rhp->start + offset <= t0 &&
		t0 < rhp->end + offset) {
		base_atr_on |= rhp->atr;
		if (t0 == rhp->end + offset - 1 ||
		    t0 == tmpll - 1)
		    base_atr_off |= rhp->atr << TXT_ATTR_OFF_ON_SHIFT;
	    }
	}

	if (tmpline[t0] == ZWC('\t')) {
	    REFRESH_ELEMENT sp = zr_sp;
	    sp.atr = base_atr_on;
	    for (*vp++ = zr_sp; (vp - vbuf) & 7; )
		*vp++ = zr_sp;
	    vp[-1].atr |= base_atr_off;
	} else if (tmpline[t0] == ZWC('\n')) {
	    /* HERE highlight */
	    vp->chr = ZWC('\\');
	    vp->atr = special_atr_on | base_atr_on;
	    vp++;
	    vp->chr = ZWC('n');
	    vp->atr = special_atr_on | special_atr_off |
		base_atr_on | base_atr_off;
	    vp++;
#ifdef MULTIBYTE_SUPPORT
	} else if (iswprint(tmpline[t0])) {
	    int width;
	    vp->chr = tmpline[t0];
	    vp->atr = base_atr_on;
	    vp++;
	    width = wcwidth(tmpline[t0]);
	    while (--width > 0) {
		vp->chr = WEOF;
		vp->atr = base_atr_on;
		vp++;
	    }
	    vp[-1].atr |= base_atr_off;
#endif
	} else if (ZC_icntrl(tmpline[t0])) {
	    /* HERE: highlight */
	    ZLE_INT_T t = tmpline[++t0];

	    vp->chr = ZWC('^');
	    vp->atr = special_atr_on | base_atr_on;
	    vp++;
	    vp->chr = (((unsigned int)t & ~0x80u) > 31) ?
		ZWC('?') : (t | ZWC('@'));
	    vp->atr = special_atr_on | special_atr_off | base_atr_on |
		base_atr_off;
	    vp++;
	} else {
	    vp->chr = tmpline[t0];
	    vp->atr = base_atr_on | base_atr_off;
	    vp++;
	}
	if (t0 == tmpcs)
	    nvcs = vp - vbuf - 1;
    }
    if (t0 == tmpcs)
	nvcs = vp - vbuf;
    *vp = zr_zr;

/* determine which part of the new line buffer we want for the display */
    if (winpos == -1)
	winpos = 0;
    if ((winpos && nvcs < winpos + 1) || (nvcs > winpos + winw - 2)) {
	if ((winpos = nvcs - ((winw - hasam) / 2)) < 0)
	    winpos = 0;
    }
    if (winpos) {
	vbuf[winpos].chr = ZWC('<');	/* line continues to the left */
	vbuf[winpos].atr = 0;
    }
    if ((int)ZR_strlen(vbuf + winpos) > (winw - hasam)) {
	vbuf[winpos + winw - hasam - 1].chr = ZWC('>');	/* line continues to right */
	vbuf[winpos + winw - hasam - 1].atr = 0;
	vbuf[winpos + winw - hasam] = zr_zr;
    }
    ZR_strcpy(nbuf[0], vbuf + winpos);
    zfree(vbuf, vsiz * sizeof(*vbuf));
    nvcs -= winpos;

    if (winpos < lpromptw) {
	/* skip start of buffer corresponding to prompt */
	winprompt = lpromptw - winpos;
    } else {
	/* don't */
	winprompt = 0;
    }
    if (winpos != owinpos && winprompt) {
	char *pptr;
	int skipping = 0, skipchars = winpos;
	/*
	 * Need to output such part of the left prompt as fits.
	 * Skip the first winpos characters, outputting
	 * any characters marked with %{...%}.
	 */
	singmoveto(0);
	MB_METACHARINIT();
	for (pptr = lpromptbuf; *pptr; ) {
	    if (*pptr == Inpar) {
		skipping = 1;
		pptr++;
	    } else if (*pptr == Outpar) {
		skipping = 0;
		pptr++;
	    } else {
		convchar_t cc;
		int mblen = MB_METACHARLENCONV(pptr, &cc);
		if (skipping || skipchars == 0)
		{
		    while (mblen) {
#ifdef MULTIBYTE_SUPPORT
			if (cc == WEOF)
			    fputc('?', shout);
			else
#endif
			    if (*pptr == Meta) {
				mblen--;
				fputc(*++pptr ^ 32, shout);
			    } else {
				fputc(*pptr, shout);
			    }
			pptr++;
			mblen--;
		    }
		} else {
		    skipchars--;
		    pptr += mblen;
		}
	    }
	}
	vcs = winprompt;
    }

/* display the `visible' portion of the line buffer */
    t0 = winprompt;
    vp = *nbuf + winprompt;
    refreshop = *obuf + winprompt;
    for (;;) {
	/*
	 * Skip past all matching characters, but if there used
	 * to be a prompt here be careful since all manner of
	 * nastiness may be around.
	 */
	if (vp - *nbuf >= owinprompt)
	    for (; vp->chr && ZR_equal(*vp, *refreshop);
		 t0++, vp++, refreshop++)
		;

	if (!vp->chr && !refreshop->chr)
	    break;

	singmoveto(t0);		/* move to where we do all output from */

	if (!refreshop->chr) {
	    if ((t0 = ZR_strlen(vp)))
		zwrite(vp, t0);
	    vcs += t0;
	    break;
	}
	if (!vp->chr) {
	    if (tccan(TCCLEAREOL))
		tcout(TCCLEAREOL);
	    else
		for (; refreshop++->chr; vcs++)
		    zputc(&zr_sp);
	    break;
	}
	zputc(vp);
	vcs++, t0++;
	vp++, refreshop++;
    }
/* move to the new cursor position */
    singmoveto(nvcs);

    qbuf = nbuf;
    nbuf = obuf;
    obuf = qbuf;
}

/**/
static void
singmoveto(int pos)
{
    if (pos == vcs)
	return;

/* choose cheapest movements for ttys without multiple movement capabilities -
   do this now because it's easier (to code) */

    if ((!tccan(TCMULTLEFT) || pos == 0) && (pos <= vcs / 2)) {
	zputc(&zr_cr);
	vcs = 0;
    }

    if (pos < vcs)
	tc_leftcurs(vcs - pos);
    else if (pos > vcs)
	tc_rightcurs(pos - vcs);

    vcs = pos;
}

/* Provided for unloading the module in a modular fashion */

/**/
void
zle_refresh_finish(void)
{
    freevideo();

    if (region_highlights)
	zfree(region_highlights,
	      sizeof(struct region_highlight) * n_region_highlights);
}
