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
#include "zle_refresh.pro"

/* Expanded prompts */

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
unsigned char *predisplay, *postdisplay;
/**/
int predisplaylen, postdisplaylen;


#ifdef HAVE_SELECT
/* cost of last update */
/**/
int cost;

# define SELECT_ADD_COST(X)	cost += X
# define zputc(a, b)		putc(a, b), cost++
# define zwrite(a, b, c, d)	fwrite(a, b, c, d), cost += (b * c)
#else
# define SELECT_ADD_COST(X)
# define zputc(a, b)		putc(a, b)
# define zwrite(a, b, c, d)	fwrite(a, b, c, d)
#endif

/* Oct/Nov 94: <mason> some code savagely redesigned to fix several bugs -
   refreshline() & tc_rightcurs() majorly rewritten; zrefresh() fixed -
   I've put my fingers into just about every routine in here -
   any queries about updates to mason@primenet.com.au */

static char **nbuf = NULL,	/* new video buffer line-by-line char array */
    **obuf = NULL;		/* old video buffer line-by-line char array */
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
    winpos;			/* singlelinezle: line's position in window */

/**/
void
resetvideo(void)
{
    int ln;
    static int lwinw = -1, lwinh = -1;	/* last window width & height */
 
    winw = columns;  /* terminal width */
    if (termflags & TERM_SHORT)
	winh = 1;
    else
	winh = (lines < 2) ? 24 : lines;
    rwinh = lines;		/* keep the real number of lines */
    winpos = vln = vmaxln = 0;
    if (lwinw != winw || lwinh != winh) {
	if (nbuf) {
	    for (ln = 0; ln != lwinh; ln++) {
		zfree(nbuf[ln], lwinw + 2);
		zfree(obuf[ln], lwinw + 2);
	    }
	    free(nbuf);
	    free(obuf);
	}
	nbuf = (char **)zcalloc((winh + 1) * sizeof(char *));
	obuf = (char **)zcalloc((winh + 1) * sizeof(char *));
	nbuf[0] = (char *)zalloc(winw + 2);
	obuf[0] = (char *)zalloc(winw + 2);

	lwinw = winw;
	lwinh = winh;
    }
    for (ln = 0; ln != winh + 1; ln++) {
	if (nbuf[ln])
	    *nbuf[ln] = '\0';
	if (obuf[ln])
	    *obuf[ln] = '\0';
    }

    countprompt(lpromptbuf, &lpromptwof, &lprompth, 1);
    countprompt(rpromptbuf, &rpromptw, &rprompth, 0);
    if (lpromptwof != winw)
	lpromptw = lpromptwof;
    else {
	lpromptw = 0;
	lprompth++;
    }

    if (lpromptw) {
    	memset(nbuf[0], ' ', lpromptw);
	memset(obuf[0], ' ', lpromptw);
	nbuf[0][lpromptw] = obuf[0][lpromptw] = '\0';
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
    char *s;

    s = nbuf[tline];
    for (t0 = tline; t0 < winh - 1; t0++)
	nbuf[t0] = nbuf[t0 + 1];
    nbuf[winh - 1] = s;
    if (!tline)
	more_start = 1;
    return;
}

/* this is the messy part. */
/* this define belongs where it's used!!! */

#define nextline					\
{							\
    *s = '\0';						\
    if (ln != winh - 1)					\
	ln++;						\
    else {						\
	if (!canscroll)	{				\
	    if (nvln != -1 && nvln != winh - 1		\
		&& (numscrolls != onumscrolls - 1	\
		    || nvln <= winh / 2))		\
	        break;					\
	    numscrolls++;				\
	    canscroll = winh / 2;			\
	}						\
	canscroll--;					\
	scrollwindow(0);				\
	if (nvln != -1)					\
	    nvln--;					\
    }							\
    if (!nbuf[ln])					\
	nbuf[ln] = (char *)zalloc(winw + 2);		\
    s = (unsigned char *)nbuf[ln];			\
    sen = s + winw;					\
}

#define snextline					\
{							\
    *s = '\0';						\
    if (ln != winh - 1)					\
	ln++;						\
    else						\
	if (tosln > ln) {				\
	    tosln--;					\
	    if (nvln > 1) {				\
		scrollwindow(0);			\
		nvln--;					\
	    } else					\
		more_end = 1;				\
	} else if (tosln > 2 && nvln > 1) {		\
	    tosln--;					\
	    if (tosln <= nvln) {			\
		scrollwindow(0);			\
		nvln--;					\
	    } else {					\
		scrollwindow(tosln);			\
		more_end = 1;				\
	    }						\
	} else {					\
	    more_status = 1;				\
	    scrollwindow(tosln + 1);			\
	}						\
    if (!nbuf[ln])					\
	nbuf[ln] = (char *)zalloc(winw + 2);		\
    s = (unsigned char *)nbuf[ln];			\
    sen = s + winw;					\
}

static int cleareol,		/* clear to end-of-line (if can't cleareod) */
    clearf,			/* alwayslastprompt used immediately before */
    put_rpmpt,			/* whether we should display right-prompt   */
    oput_rpmpt,			/* whether displayed right-prompt last time */
    oxtabs,			/* oxtabs - tabs expand to spaces if set    */
    numscrolls, onumscrolls;

/**/
mod_export void
zrefresh(void)
{
    static int inlist;		/* avoiding recursion                        */
    int canscroll = 0,		/* number of lines we are allowed to scroll  */
	ln = 0,			/* current line we're working on	     */
	more_status = 0,	/* more stuff in status line		     */
	nvcs = 0, nvln = -1,	/* video cursor column and line		     */
	t0 = -1,		/* tmp					     */
	tosln = 0;		/* tmp in statusline stuff		     */
    unsigned char *s,		/* pointer into the video buffer	     */
	*t,			/* pointer into the real buffer		     */
	*sen,			/* pointer to end of the video buffer (eol)  */
	*scs;			/* pointer to cursor position in real buffer */
    char **qbuf;		/* tmp					     */
    unsigned char *tmpline;	/* line with added pre/post text */
    int tmpcs, tmpll;		/* ditto cursor position and line length */
    int tmpalloced;		/* flag to free tmpline when finished */
	

    /* If this is called from listmatches() (indirectly via trashzle()), and *
     * that was called from the end of zrefresh(), then we don't need to do  *
     * anything.  All this `inlist' code is actually unnecessary, but it     *
     * improves speed a little in a common case.                             */
    if (inlist)
	return;

    if (predisplaylen || postdisplaylen) {
	/* There is extra text to display at the start or end of the line */
	tmpline = zalloc(ll + predisplaylen + postdisplaylen);
	if (predisplaylen)
	    memcpy(tmpline, predisplay, predisplaylen);
	if (ll)
	    memcpy(tmpline+predisplaylen, line, ll);
	if (postdisplaylen)
	    memcpy(tmpline+predisplaylen+ll, postdisplay, postdisplaylen);
	tmpcs = cs + predisplaylen;
	tmpll = predisplaylen + ll + postdisplaylen;
	tmpalloced = 1;
    } else {
	tmpline = line;
	tmpcs = cs;
	tmpll = ll;
	tmpalloced = 0;
    }

    if (clearlist && listshown > 0) {
	if (tccan(TCCLEAREOD)) {
	    int ovln = vln, ovcs = vcs;
	    char *nb = nbuf[vln];

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
	resetvideo();
	resetneeded = 0;	/* unset */
	oput_rpmpt = 0;		/* no right-prompt currently on screen */

	/* we probably should only have explicitly set attributes */
	tsetcap(TCALLATTRSOFF, 0);
	tsetcap(TCSTANDOUTEND, 0);
	tsetcap(TCUNDERLINEEND, 0);

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
	    if (txtchangeisset(TXTNOBOLDFACE))
		tsetcap(TCALLATTRSOFF, 0);
	    if (txtchangeisset(TXTNOSTANDOUT))
		tsetcap(TCSTANDOUTEND, 0);
	    if (txtchangeisset(TXTNOUNDERLINE))
		tsetcap(TCUNDERLINEEND, 0);
	    if (txtchangeisset(TXTBOLDFACE))
		tsetcap(TCBOLDFACEBEG, 0);
	    if (txtchangeisset(TXTSTANDOUT))
		tsetcap(TCSTANDOUTBEG, 0);
	    if (txtchangeisset(TXTUNDERLINE))
		tsetcap(TCUNDERLINEBEG, 0);
	}
	if (clearflag) {
	    zputc('\r', shout);
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
	*nbuf = (char *)zalloc(winw + 2);

    s = (unsigned char *)(nbuf[ln = 0] + lpromptw);
    t = tmpline;
    sen = (unsigned char *)(*nbuf + winw);
    for (; t < tmpline+tmpll; t++) {
	if (t == scs)			/* if cursor is here, remember it */
	    nvcs = s - (unsigned char *)(nbuf[nvln = ln]);

	if (*t == '\n')	{		/* newline */
	    nbuf[ln][winw + 1] = '\0';	/* text not wrapped */
	    nextline
	} else if (*t == '\t') {		/* tab */
	    t0 = (char *)s - nbuf[ln];
	    if ((t0 | 7) + 1 >= winw) {
		nbuf[ln][winw + 1] = '\n';	/* text wrapped */
		nextline
	    } else
		do
		    *s++ = ' ';
		while ((++t0) & 7);
	} else if (icntrl(*t)) {	/* other control character */
	    *s++ = '^';
	    if (s == sen) {
		nbuf[ln][winw + 1] = '\n';	/* text wrapped */
		nextline
	    }
	    *s++ = (*t == 127) ? '?' : (*t | '@');
	} else				/* normal character */
	    *s++ = *t;
	if (s == sen) {
	    nbuf[ln][winw + 1] = '\n';	/* text wrapped */
	    nextline
	}
    }

/* if we're really on the next line, don't fake it; do everything properly */
    if (t == scs && (nvcs = s - (unsigned char *)(nbuf[nvln = ln])) == winw) {
	nbuf[ln][winw + 1] = '\n';	/* text wrapped */
	switch ('\0') { 	/* a sad hack to make the break */
	case '\0':		/* in nextline work */
	    nextline
	}
	*s = '\0';
	nvcs = 0;
	nvln++;
    }

    if (t != tmpline + tmpll)
	more_end = 1;

    if (statusline) {
	tosln = ln + 1;
	nbuf[ln][winw + 1] = '\0';	/* text not wrapped */
	snextline
	t = (unsigned char *)statusline;
	for (; t < (unsigned char *)statusline + statusll; t++) {
	    if (icntrl(*t)) {	/* simplified processing in the status line */
		*s++ = '^';
		if (s == sen) {
		    nbuf[ln][winw + 1] = '\n';	/* text wrapped */
		    snextline
		}
		*s++ = (*t == 127) ? '?' : (*t | '@');
	    } else
		*s++ = *t;
	    if (s == sen) {
		nbuf[ln][winw + 1] = '\n';	/* text wrapped */
		snextline
	    }
	}
	if (s == sen)
	    snextline
    }
    *s = '\0';

/* insert <.... at end of last line if there is more text past end of screen */
    if (more_end) {
	if (!statusline)
	    tosln = winh;
	s = nbuf[tosln - 1];
	sen = s + winw - 7;
	for (; s < sen; s++) {
	    if (*s == '\0') {
		for (; s < sen; )
		    *s++ = ' ';
		break;
	    }
	}
	strncpy(sen, " <.... ", 7);
	nbuf[tosln - 1][winw] = nbuf[tosln - 1][winw + 1] = '\0';
    }

/* insert <....> at end of first status line if status is too big */
    if (more_status) {
	s = nbuf[tosln];
	sen = s + winw - 8;
	for (; s < sen; s++) {
	    if (*s == '\0') {
		for (; s < sen; )
		    *s++ = ' ';
		break;
	    }
	}
	strncpy(sen, " <....> ", 8);
	nbuf[tosln][winw] = nbuf[tosln][winw + 1] = '\0';
    }

    nlnct = ln + 1;
    for (ln = nlnct; ln < winh; ln++)
	zfree(nbuf[ln], winw + 2), nbuf[ln] = NULL;

/* determine whether the right-prompt exists and can fit on the screen */
    if (!more_start) {
	if (trashedzle && opts[TRANSIENTRPROMPT])
	    put_rpmpt = 0;
	else
	    put_rpmpt = rprompth == 1 && rpromptbuf[0] &&
		!strchr(rpromptbuf, '\t') &&
		(int)strlen(nbuf[0]) + rpromptw < winw - 1;
    } else {
/* insert >.... on first line if there is more text before start of screen */
	memset(nbuf[0], ' ', lpromptw);
	t0 = winw - lpromptw;
	t0 = t0 > 5 ? 5 : t0;
	strncpy(nbuf[0] + lpromptw, ">....", t0);
	memset(nbuf[0] + lpromptw + t0, ' ', winw - t0 - lpromptw);
	nbuf[0][winw] = nbuf[0][winw + 1] = '\0';
    }

    for (ln = 0; ln < nlnct; ln++) {
	/* if we have more lines than last time, clear the newly-used lines */
	if (ln >= olnct)
	    cleareol = 1;

    /* if old line and new line are different,
       see if we can insert/delete a line to speed up update */

	if (!clearf && ln > 0 && ln < olnct - 1 && !(hasam && vcs == winw) &&
	    nbuf[ln] && obuf[ln] &&
	    strncmp(nbuf[ln], obuf[ln], 16)) {
	    if (tccan(TCDELLINE) && obuf[ln + 1] && obuf[ln + 1][0] &&
		nbuf[ln] && !strncmp(nbuf[ln], obuf[ln + 1], 16)) {
		moveto(ln, 0);
		tcout(TCDELLINE);
		zfree(obuf[ln], winw + 2);
		for (t0 = ln; t0 != olnct; t0++)
		    obuf[t0] = obuf[t0 + 1];
		obuf[--olnct] = NULL;
	    }
	/* don't try to insert a line if olnct = vmaxln (vmaxln is the number
	   of lines that have been displayed by this routine) so that we don't
	   go off the end of the screen. */

	    else if (tccan(TCINSLINE) && olnct < vmaxln && nbuf[ln + 1] &&
		     obuf[ln] && !strncmp(nbuf[ln + 1], obuf[ln], 16)) {
		moveto(ln, 0);
		tcout(TCINSLINE);
		for (t0 = olnct; t0 != ln; t0--)
		    obuf[t0] = obuf[t0 - 1];
		obuf[ln] = NULL;
		olnct++;
	    }
	}

    /* update the single line */
	refreshline(ln);

    /* output the right-prompt if appropriate */
	if (put_rpmpt && !ln && !oput_rpmpt) {
	    moveto(0, winw - 1 - rpromptw);
	    zputs(rpromptbuf, shout);
	    vcs = winw - 1;
	/* reset character attributes to that set by the main prompt */
	    txtchange = pmpt_attr;
	    if (txtchangeisset(TXTNOBOLDFACE) && (rpmpt_attr & TXTBOLDFACE))
		tsetcap(TCALLATTRSOFF, 0);
	    if (txtchangeisset(TXTNOSTANDOUT) && (rpmpt_attr & TXTSTANDOUT))
		tsetcap(TCSTANDOUTEND, 0);
	    if (txtchangeisset(TXTNOUNDERLINE) && (rpmpt_attr & TXTUNDERLINE))
		tsetcap(TCUNDERLINEEND, 0);
	    if (txtchangeisset(TXTBOLDFACE) && (rpmpt_attr & TXTNOBOLDFACE))
		tsetcap(TCBOLDFACEBEG, 0);
	    if (txtchangeisset(TXTSTANDOUT) && (rpmpt_attr & TXTNOSTANDOUT))
		tsetcap(TCSTANDOUTBEG, 0);
	    if (txtchangeisset(TXTUNDERLINE) && (rpmpt_attr & TXTNOUNDERLINE))
		tsetcap(TCUNDERLINEBEG, 0);
	}
    }

/* if old buffer had extra lines, set them to be cleared and refresh them
individually */

    if (olnct > nlnct) {
	cleareol = 1;
	for (ln = nlnct; ln < olnct; ln++)
	    refreshline(ln);
    }

/* reset character attributes */
    if (clearf && postedit) {
	if ((txtchange = pmpt_attr ? pmpt_attr : rpmpt_attr)) {
	    if (txtchangeisset(TXTNOBOLDFACE))
		tsetcap(TCALLATTRSOFF, 0);
	    if (txtchangeisset(TXTNOSTANDOUT))
		tsetcap(TCSTANDOUTEND, 0);
	    if (txtchangeisset(TXTNOUNDERLINE))
		tsetcap(TCUNDERLINEEND, 0);
	    if (txtchangeisset(TXTBOLDFACE))
		tsetcap(TCBOLDFACEBEG, 0);
	    if (txtchangeisset(TXTSTANDOUT))
		tsetcap(TCSTANDOUTBEG, 0);
	    if (txtchangeisset(TXTUNDERLINE))
		tsetcap(TCUNDERLINEBEG, 0);
	}
    }
    clearf = 0;
    oput_rpmpt = put_rpmpt;

/* move to the new cursor position */
    moveto(nvln, nvcs);

/* swap old and new buffers - better than freeing/allocating every time */
    qbuf = nbuf;
    nbuf = obuf;
    obuf = qbuf;
/* store current values so we can use them next time */
    ovln = nvln;
    olnct = nlnct;
    onumscrolls = numscrolls;
    if (nlnct > vmaxln)
	vmaxln = nlnct;
singlelineout:
    fflush(shout);		/* make sure everything is written out */

    if (tmpalloced)
	zfree(tmpline, tmpll);

    /* if we have a new list showing, note it; if part of the list has been
    overwritten, redisplay it. */
    if (showinglist == -2 || (showinglist > 0 && showinglist < nlnct)) {
	inlist = 1;
	listmatches();
	inlist = 0;
	zrefresh();
    }
    if (showinglist == -1)
	showinglist = nlnct;
}

#define tcinscost(X)   (tccan(TCMULTINS) ? tclen[TCMULTINS] : (X)*tclen[TCINS])
#define tcdelcost(X)   (tccan(TCMULTDEL) ? tclen[TCMULTDEL] : (X)*tclen[TCDEL])
#define tc_delchars(X)	(void) tcmultout(TCDEL, TCMULTDEL, (X))
#define tc_inschars(X)	(void) tcmultout(TCINS, TCMULTINS, (X))
#define tc_upcurs(X)	(void) tcmultout(TCUP, TCMULTUP, (X))
#define tc_leftcurs(X)	(void) tcmultout(TCLEFT, TCMULTLEFT, (X))

/* refresh one line, using whatever speed-up tricks are provided by the tty */

/**/
static void
refreshline(int ln)
{
    char *nl, *ol, *p1;		/* line buffer pointers			 */
    int ccs = 0,		/* temporary count for cursor position	 */
	char_ins = 0,		/* number of characters inserted/deleted */
	col_cleareol,		/* clear to end-of-line from this column */
	i, j,			/* tmp					 */
	ins_last,		/* insert pushed last character off line */
	nllen, ollen,		/* new and old line buffer lengths	 */
	rnllen;			/* real new line buffer length		 */

/* 0: setup */
    nl = nbuf[ln];
    rnllen = nllen = nl ? strlen(nl) : 0;
    ol = obuf[ln] ? obuf[ln] : "";
    ollen = strlen(ol);

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
	p1 = zhalloc(winw + 2);
	if (nllen)
	    strncpy(p1, nl, nllen);
	memset(p1 + nllen, ' ', winw - nllen);
	p1[winw] = '\0';
	p1[winw + 1] = (nllen < winw) ? '\0' : nl[winw + 1];
	if (ln && nbuf[ln])
	    memcpy(nl, p1, winw + 2);	/* next time obuf will be up-to-date */
	else
	    nl = p1;		/* don't keep the padding for prompt line */
	nllen = winw;
    } else if (ollen > nllen) { /* make new line at least as long as old */
	p1 = zhalloc(ollen + 1);
	strncpy(p1, nl, nllen);
	memset(p1 + nllen, ' ', ollen - nllen);
	p1[ollen] = '\0';
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
	    for (i = nllen; i && nl[i - 1] == ' '; i--);
	    for (j = ollen; j && ol[j - 1] == ' '; j--);
	    if ((j > i + tclen[TCCLEAREOL])	/* new buf has enough spaces */
		|| (nllen == winw && nl[winw - 1] == ' '))
		col_cleareol = i;
	}
    }

/* 2b: first a new trick for automargin niceness - good for cut and paste */

    if (hasam && vcs == winw) {
	if (nbuf[vln] && nbuf[vln][vcs + 1] == '\n') {
	    vln++, vcs = 1;
            if (nbuf[vln]  && *nbuf[vln])
		zputc(*nbuf[vln], shout);
	    else
		zputc(' ', shout);  /* I don't think this should happen */
	    if (ln == vln) {	/* better safe than sorry */
		nl++;
		if (*ol)
		    ol++;
		ccs = 1;
	    }			/* else  hmmm... I wonder what happened */
	} else {
	    vln++, vcs = 0;
	    zputc('\n', shout);
	}
    }
    ins_last = 0;

/* 2c: if we're on the first line, start checking at the end of the prompt;
   we shouldn't be doing anything within the prompt */

    if (ln == 0 && lpromptw) {
	i = lpromptw - ccs;
	j = strlen(ol);
	nl += i;
	ol += (i > j ? j : i);	/* if ol is too short, point it to '\0' */
	ccs = lpromptw;
    }

/* 3: main display loop - write out the buffer using whatever tricks we can */

    for (;;) {
	if (*nl && *ol && nl[1] == ol[1]) /* skip only if second chars match */
	/* skip past all matching characters */
	    for (; *nl && (*nl == *ol); nl++, ol++, ccs++) ;

	if (!*nl) {
	    if (ccs == winw && hasam && char_ins > 0 && ins_last
		&& vcs != winw) {
		nl--;           /* we can assume we can go back here */
		moveto(ln, winw - 1);
		zputc(*nl, shout);
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
	if (!*nl) {
	    i = (winw - ccs < char_ins) ? (winw - ccs) : char_ins;
	    if (tccan(TCDEL) && (tcdelcost(i) <= i + 1))
		tc_delchars(i);
	    else {
		vcs += i;
		while (i-- > 0)
		    zputc(' ', shout);
	    }
	    return;
	}

    /* if we've reached the end of the old buffer, then there are few tricks
       we can do, so we just dump out what we must and clear if we can */
	if (!*ol) {
	    i = (col_cleareol >= 0) ? col_cleareol : nllen;
	    i -= vcs;
	    zwrite(nl, i, 1, shout);
	    vcs += i;
	    if (col_cleareol >= 0)
		tcout(TCCLEAREOL);
	    return;
	}

    /* inserting & deleting chars: we can if there's no right-prompt */
	if ((ln || !put_rpmpt || !oput_rpmpt) 
	    && (nl[1] && ol[1] && nl[1] != ol[1])) { 

	/* deleting characters - see if we can find a match series that
	   makes it cheaper to delete intermediate characters
	   eg. oldline: hifoobar \ hopefully cheaper here to delete two
	       newline: foobar	 / characters, then we have six matches */
	    if (tccan(TCDEL)) {
		for (i = 1; *(ol + i); i++)
		    if (tcdelcost(i) < pfxlen(ol + i, nl)) {
			tc_delchars(i);
			ol += i;
			char_ins -= i;
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
		for (i = 1; *(nl + i); i++)
		    if (tcinscost(i) < pfxlen(nl + i, ol)) {
			tc_inschars(i);
			zwrite(nl, i, 1, shout);
			nl += i;
			char_ins += i;
			ccs = (vcs += i);
		    /* if we've pushed off the right, truncate oldline */
			for (i = 0; *(ol + i) && i < winw - ccs; i++);
			if (i == winw - ccs) {
			    *(ol + i) = '\0';
			    ins_last = 1;
			}
			i = 0;
			break;
		    }
		if (!i)
		    continue;
	    }
	}
    /* we can't do any fancy tricks, so just dump the single character
       and keep on trying */
	zputc(*nl, shout);
	nl++, ol++;
	ccs++, vcs++;
    }
}

/* move the cursor to line ln (relative to the prompt line),
   absolute column cl; update vln, vcs - video line and column */

/**/
void
moveto(int ln, int cl)
{
    int c;

    if (vcs == winw) {
	vln++, vcs = 0;
	if (!hasam) {
	    zputc('\r', shout);
	    zputc('\n', shout);
	} else {
	    if ((vln < nlnct) && nbuf[vln] && *nbuf[vln])
		c = *nbuf[vln];
	    else
		c = ' ';
	    zputc(c, shout);
	    zputc('\r', shout);
	    if ((vln < olnct) && obuf[vln] && *obuf[vln])
		*obuf[vln] = c;
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
	zputc('\r', shout), vcs = 0; /* safety precaution */
	while (ln > vln) {
	    zputc('\n', shout);
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
    char *t;

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
   prompt out unless ztrlen(lpromptbuf) == lpromptw : we can cheat then */
    if (vln == 0 && i < lpromptw && !(termflags & TERM_SHORT)) {
	if (strlen(lpromptbuf) == lpromptw)
	    fputs(lpromptbuf + i, shout);
	else if (tccan(TCRIGHT) && (tclen[TCRIGHT] * ct <= ztrlen(lpromptbuf)))
	    /* it is cheaper to send TCRIGHT than reprint the whole prompt */
	    for (ct = lpromptw - i; ct--; )
		tcout(TCRIGHT);
        else {
	    if (i != 0)
		zputc('\r', shout);
	    tc_upcurs(lprompth - 1);
	    zputs(lpromptbuf, shout);
	    if (lpromptwof == winw)
		zputs("\n", shout);	/* works with both hasam and !hasam */
	}
	i = lpromptw;
	ct = cl - i;
    }

    if (nbuf[vln]) {
	for (j = 0, t = nbuf[vln]; *t && (j < i); j++, t++);
	if (j == i)
	    for ( ; *t && ct; ct--, t++)
		zputc(*t, shout);
    }
    while (ct--)
	zputc(' ', shout);	/* not my fault your terminal can't go right */
}

/**/
mod_export int
tc_downcurs(int ct)
{
    int ret = 0;

    if (ct && !tcmultout(TCDOWN, TCMULTDOWN, ct)) {
	while (ct--)
	    zputc('\n', shout);
	zputc('\r', shout), ret = -1;
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
clearscreen(char **args)
{
    tcout(TCCLEARSCREEN);
    resetneeded = 1;
    clearflag = 0;
    return 0;
}

/**/
mod_export int
redisplay(char **args)
{
    moveto(0, 0);
    zputc('\r', shout);		/* extra care */
    tc_upcurs(lprompth - 1);
    resetneeded = 1;
    clearflag = 0;
    return 0;
}

/**/
static void
singlerefresh(unsigned char *tmpline, int tmpll, int tmpcs)
{
    char *vbuf, *vp,		/* video buffer and pointer    */
	**qbuf,			/* tmp			       */
	*refreshop = *obuf;	/* pointer to old video buffer */
    int t0,			/* tmp			       */
	vsiz,			/* size of new video buffer    */
	nvcs = 0;		/* new video cursor column     */

    nlnct = 1;
/* generate the new line buffer completely */
    for (vsiz = 1 + lpromptw, t0 = 0; t0 != tmpll; t0++, vsiz++)
	if (tmpline[t0] == '\t')
	    vsiz = (vsiz | 7) + 1;
	else if (icntrl(tmpline[t0]))
	    vsiz++;
    vbuf = (char *)zalloc(vsiz);

    if (tmpcs < 0) {
#ifdef DEBUG
	fprintf(stderr, "BUG: negative cursor position\n");
	fflush(stderr); 
#endif
	tmpcs = 0;
    }

    /* only use last part of prompt */
    memcpy(vbuf, strchr(lpromptbuf, 0) - lpromptw, lpromptw);
    vbuf[lpromptw] = '\0';
    vp = vbuf + lpromptw;

    for (t0 = 0; t0 != tmpll; t0++) {
	if (tmpline[t0] == '\t')
	    for (*vp++ = ' '; (vp - vbuf) & 7; )
		*vp++ = ' ';
	else if (tmpline[t0] == '\n') {
	    *vp++ = '\\';
	    *vp++ = 'n';
	} else if (tmpline[t0] == 0x7f) {
	    *vp++ = '^';
	    *vp++ = '?';
	} else if (icntrl(tmpline[t0])) {
	    *vp++ = '^';
	    *vp++ = tmpline[t0] | '@';
	} else
	    *vp++ = tmpline[t0];
	if (t0 == tmpcs)
	    nvcs = vp - vbuf - 1;
    }
    if (t0 == tmpcs)
	nvcs = vp - vbuf;
    *vp = '\0';

/* determine which part of the new line buffer we want for the display */
    if ((winpos && nvcs < winpos + 1) || (nvcs > winpos + winw - 2)) {
	if ((winpos = nvcs - ((winw - hasam) / 2)) < 0)
	    winpos = 0;
    }
    if (winpos)
	vbuf[winpos] = '<';	/* line continues to the left */
    if ((int)strlen(vbuf + winpos) > (winw - hasam)) {
	vbuf[winpos + winw - hasam - 1] = '>';	/* line continues to right */
	vbuf[winpos + winw - hasam] = '\0';
    }
    strcpy(nbuf[0], vbuf + winpos);
    zfree(vbuf, vsiz);
    nvcs -= winpos;

/* display the `visable' portion of the line buffer */
    for (t0 = 0, vp = *nbuf;;) {
    /* skip past all matching characters */
	for (; *vp && *vp == *refreshop; t0++, vp++, refreshop++) ;

	if (!*vp && !*refreshop)
	    break;

	singmoveto(t0);		/* move to where we do all output from */

	if (!*refreshop) {
	    if ((t0 = strlen(vp)))
		zwrite(vp, t0, 1, shout);
	    vcs += t0;
	    break;
	}
	if (!*vp) {
	    if (tccan(TCCLEAREOL))
		tcout(TCCLEAREOL);
	    else
		for (; *refreshop++; vcs++)
		    zputc(' ', shout);
	    break;
	}
	zputc(*vp, shout);
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
	zputc('\r', shout);
	vcs = 0;
    }

    if (pos < vcs)
	tc_leftcurs(vcs - pos);
    else if (pos > vcs)
	tc_rightcurs(pos - vcs);

    vcs = pos;
}
