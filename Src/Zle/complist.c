/*
 * complist.c - completion listing enhancements
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 1999 Sven Wischnowsky
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Sven Wischnowsky or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Sven Wischnowsky and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Sven Wischnowsky and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Sven Wischnowsky and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "complist.mdh"
#include "complist.pro"


/* We use the parameters ZLS_COLORS and ZLS_COLOURS in the same way as
 * the color ls does. It's just that we don't support the `or' file
 * type. */


static Widget w_menuselect;
static Keymap mskeymap;

/* Indixes into the terminal string arrays. */

#define COL_NO  0
#define COL_FI  1
#define COL_DI  2
#define COL_LN  3
#define COL_PI  4
#define COL_SO  5
#define COL_BD  6
#define COL_CD  7
#define COL_EX  8
#define COL_MI  9
#define COL_LC 10
#define COL_RC 11
#define COL_EC 12
#define COL_MA 13

#define NUM_COLS 14

/* Names of the terminal strings. */

static char *colnames[] = {
    "no", "fi", "di", "ln", "pi", "so", "bd", "cd", "ex", "mi",
    "lc", "rc", "ec", "ma", NULL
};

/* Default values. */

static char *defcols[] = {
    "0", "0", "1;34", "1;36", "33", "1;35", "1;33", "1;33", "1;32", NULL,
    "\033[", "m", NULL, "7"
};

/* This describes a terminal string for a filename extension. */

typedef struct extcol *Extcol;

struct extcol {
    char *ext;			/* the extension */
    char *col;			/* the terminal color string */
    Extcol next;		/* the next one in the list */
};

/* This holds all terminal strings. */

typedef struct listcols *Listcols;

struct listcols {
    char *cols[NUM_COLS];	/* strings for file types */
    Extcol exts;		/* strings for extensions */
};

/* This parses the value of a definition (the part after the `=').
 * The return value is a pointer to the character after it. */

static char *
getcolval(char *s)
{
    char *p;

    for (p = s; *s && *s != ':'; p++, s++) {
	if (*s == '\\' && s[1]) {
	    switch (*++s) {
	    case 'a': *p = '\007'; break;
	    case 'n': *p = '\n'; break;
	    case 'b': *p = '\b'; break;
	    case 't': *p = '\t'; break;
	    case 'v': *p = '\v'; break;
	    case 'f': *p = '\f'; break;
	    case 'r': *p = '\r'; break;
	    case 'e': *p = '\033'; break;
	    case '_': *p = ' '; break;
	    case '?': *p = '\177'; break;
	    default:
		if (*s >= '0' && *s <= '7') {
		    int i = STOUC(*s);

		    if (*++s >= '0' && *s <= '7') {
			i = (i * 8) + STOUC(*s);
			if (*++s >= '0' && *s <= '7')
			    i = (i * 8) + STOUC(*s);
		    }
		    *p = (char) i;
		} else
		    *p = *s;
	    }
	} else if (*s == '^') {
	    if ((s[1] >= '@' && s[1] <= '_') ||
		(s[1] >= 'a' && s[1] <= 'z'))
		*p = (char) (STOUC(*s) & ~0x60);
	    else if (s[1] == '?')
		*p = '\177';
	    else {
		*p++ = *s;
		*p = s[1];
	    }
	    s++;
	} else
	    *p = *s;
    }
    if (p != s)
	*p = '\0';
    return s;
}

/* This parses one definition. Return value is a pointer to the
 * character after it. */

static char *
getcoldef(Listcols c, char *s)
{
    if (*s == '*') {
	Extcol ec;
	char *n, *p;

	/* This is for an extension. */

	n = ++s;
	while (*s && *s != '=')
	    s++;
	if (!*s)
	    return s;
	*s++ = '\0';
	p = getcolval(s);
	ec = (Extcol) zhalloc(sizeof(*ec));
	ec->ext = n;
	ec->col = s;
	ec->next = c->exts;
	c->exts = ec;
	if (*p)
	    *p++ = '\0';
	return p;
    } else {
	char *n = s, *p, **nn;
	int i;

	/* This is for a file type. */

	while (*s && *s != '=')
	    s++;
	if (!*s)
	    return s;
	*s++ = '\0';
	for (i = 0, nn = colnames; *nn; i++, nn++)
	    if (!strcmp(n ,*nn))
		break;
	p = getcolval(s);
	if (*nn)
	    c->cols[i] = s;
	if (*p)
	    *p++ = '\0';
	return p;
    }
}

/* Combined length of LC and RC, maximum length of capability strings. */

static int lr_caplen, max_caplen;

/* This initializes the given terminal color structure. */

static void
getcols(Listcols c)
{
    char *s;
    int i, l;

    if (!(s = getsparam("ZLS_COLORS")) &&
	!(s = getsparam("ZLS_COLOURS"))) {
	for (i = 0; i < NUM_COLS; i++)
	    c->cols[i] = "";
	c->exts = NULL;
	
	if (!(c->cols[COL_MA] = tcstr[TCSTANDOUTBEG]) ||
	    !c->cols[COL_MA][0])
	    c->cols[COL_MA] = "";
	else
	    c->cols[COL_EC] = tcstr[TCSTANDOUTEND];
	lr_caplen = 0;
	if ((max_caplen = strlen(c->cols[COL_MA])) <
	    (l = strlen(c->cols[COL_EC])))
	    max_caplen = l;
	return;
    }
    /* We have one of the parameters, use it. */
    memset(c, 0, sizeof(*c));
    s = dupstring(s);
    while (*s)
	s = getcoldef(c, s);

    /* Use default values for those that aren't set explicitly. */
    max_caplen = lr_caplen = 0;
    for (i = 0; i < NUM_COLS; i++) {
	if (!c->cols[i])
	    c->cols[i] = defcols[i];
	if (c->cols[i] && (l = strlen(c->cols[i])) > max_caplen)
	    max_caplen = l;
    }
    lr_caplen = strlen(c->cols[COL_LC]) + strlen(c->cols[COL_RC]);

    /* Default for missing files. */
    if (!c->cols[COL_MI])
	c->cols[COL_MI] = c->cols[COL_FI];

    return;
}

static int last_col = COL_NO;

static void
zlrputs(Listcols c, char *cap)
{
    VARARR(char, buf, lr_caplen + max_caplen + 1);

    strcpy(buf, c->cols[COL_LC]);
    strcat(buf, cap);
    strcat(buf, c->cols[COL_RC]);

    tputs(buf, 1, putshout);
}

static void
zcputs(Listcols c, int colour)
{
    if (colour != last_col
	&& (last_col < COL_NO
	    || strcmp(c->cols[last_col], c->cols[colour]))) {
	zlrputs(c, c->cols[colour]);
	last_col = colour;
    }
    return;
}

/* Get the terminal color string for the file with the given name and
 * file modes. */

static void
putcolstr(Listcols c, char *n, mode_t m)
{
    int colour;
    Extcol e;

    for (e = c->exts; e; e = e->next)
	if (strsfx(e->ext, n)) {	/* XXX: unoptimised if used */
	    if (last_col < COL_NO
		|| strcmp(c->cols[last_col], e->col))
		zlrputs(c, e->col);

	    last_col = COL_NO - 1;
	    return;
	}

    if (S_ISDIR(m))
	colour = COL_DI;
    else if (S_ISLNK(m))
	colour = COL_LN;
    else if (S_ISFIFO(m))
	colour = COL_PI;
    else if (S_ISSOCK(m))
	colour = COL_SO;
    else if (S_ISBLK(m))
	colour = COL_BD;
    else if (S_ISCHR(m))
	colour = COL_CD;
    else if (S_ISREG(m) && (m & S_IXUGO))
	colour = COL_EX;
    else
	colour = COL_FI;

    zcputs(c, colour);
    return;
}

/* Information about the list shown. */

static int noselect, mselect, inselect, mcol, mline, mcols, mlines, mmlen;
static Cmatch **mtab, **mmtabp;
static Cmgroup *mgtab, *mgtabp;
static struct listcols mcolors;


static void
clprintm(Cmgroup g, Cmatch *mp, int mc, int ml, int lastc, int width,
	 char *path, struct stat *buf)
{
    Cmatch m;
    int len, cc;

    if (!mp) {
	zcputs(&mcolors, COL_MI);
	len = width - 2;
	while (len-- > 0)
	    putc(' ', shout);
	if (mcolors.cols[COL_EC])
	    tputs(mcolors.cols[COL_EC], 1, putshout);
	else
	    zcputs(&mcolors, COL_NO);

	return;
    }
    m = *mp;
    if (m->disp && (m->flags & CMF_DISPLINE)) {
	if (mselect >= 0) {
	    int mm = (mcols * ml), i;

	    for (i = mcols; i--; ) {
		mtab[mm + i] = mp;
		mgtab[mm + i] = g;
	    }
	}
	if (m->gnum == mselect) {
	    int mm = (mcols * ml);
	    mline = ml;
	    mcol = 0;
	    mmtabp = mtab + mm;
	    mgtabp = mgtab + mm;
	    mmlen = mcols;
	    cc = COL_MA;
	} else
	    cc = COL_NO;
	zcputs(&mcolors, cc);
	printfmt(m->disp, 0, 1, 0);
	if (mcolors.cols[COL_EC])
	    tputs(mcolors.cols[COL_EC], 1, putshout);
	else
	    zcputs(&mcolors, COL_NO);
    } else {
	int mx;

	if (g->widths) {
	    int i;

	    for (i = mx = 0; i < mc; i++)
		mx += g->widths[i];
	} else
	    mx = mc * g->width;

	if (mselect >= 0) {
	    int mm = mcols * ml, i;

	    for (i = (width ? width : mcols); i--; ) {
		mtab[mx + mm + i] = mp;
		mgtab[mx + mm + i] = g;
	    }
	}
	if (m->gnum == mselect) {
	    int mm = mcols * ml;

	    mcol = mx;
	    mline = ml;
	    mmtabp = mtab + mx + mm;
	    mgtabp = mgtab + mx + mm;
	    mmlen = width;
	    zcputs(&mcolors, COL_MA);
	} else if (buf)
	    putcolstr(&mcolors, path, buf->st_mode);
	else
	    zcputs(&mcolors, COL_NO);

	nicezputs((m->disp ? m->disp : m->str), shout);
	len = niceztrlen(m->disp ? m->disp : m->str);

	if (isset(LISTTYPES)) {
	    if (buf)
		putc(file_type(buf->st_mode), shout);
	    else
		putc(' ', shout);
	    len++;
	}
	len = width - len - 2;

	while (len-- > 0)
	    putc(' ', shout);

	if (mcolors.cols[COL_EC])
	    tputs(mcolors.cols[COL_EC], 1, putshout);
	else
	    zcputs(&mcolors, COL_NO);
	if (!lastc) {
	    zcputs(&mcolors, COL_NO);
	    fputs("  ", shout);
	    if (mcolors.cols[COL_EC])
		tputs(mcolors.cols[COL_EC], 1, putshout);
	    else
		zcputs(&mcolors, COL_NO);
	}
    }
}

static int
complistmatches(Hookdef dummy, Chdata dat)
{
    Cmgroup oamatches = amatches;

    amatches = dat->matches;

    if (minfo.asked == 2) {
	showinglist = 0;
	amatches = oamatches;
	return (noselect = 1);
    }
    getcols(&mcolors);

    calclist();

    if (!listdat.nlines || (mselect >= 0 &&
			    (!(isset(USEZLE) && !termflags &&
			       complastprompt && *complastprompt) ||
			     (listdat.nlines + nlnct - 1) >= lines))) {
	showinglist = listshown = 0;
	noselect = 1;
	amatches = oamatches;
	return 1;
    }
    if (listdat.hidden) {
	noselect = 1;
	mselect = -1;
    }
    if (inselect)
	clearflag = 0;

    if (asklist()) {
	amatches = oamatches;
	return 1;
    }
    if (mselect >= 0) {
	int i;

	i = columns * listdat.nlines;
	free(mtab);
	mtab = (Cmatch **) zalloc(i * sizeof(Cmatch **));
	memset(mtab, 0, i * sizeof(Cmatch **));
	free(mgtab);
	mgtab = (Cmgroup *) zalloc(i * sizeof(Cmgroup));
	memset(mgtab, 0, i * sizeof(Cmgroup));
	mcols = columns;
	mlines = listdat.nlines;
    }
    last_col = COL_NO - 1;

    if (!printlist(1, clprintm) || listdat.nlines >= lines)
	noselect = 1;

    amatches = oamatches;

    return noselect;
}

static int
adjust_mcol(int wish, Cmatch ***tabp, Cmgroup **grp)
{
    Cmatch **tab = *tabp;
    int p, n, c;

    tab -= mcol;

    for (p = wish; p >= 0 && !tab[p]; p--);
    for (n = wish; n < mcols && !tab[n]; n++);
    if (n == mcols)
	n = -1;

    if (p < 0) {
	if (n < 0)
	    return 1;
	c = n;
    } else if (n < 0)
	c = p;
    else
	c = ((mcol - p) < (n - mcol) ? p : n);

    *tabp = tab + c;
    if (grp)
	*grp = *grp + c - mcol;

    mcol = c;
    
    return 0;
}

typedef struct menustack *Menustack;

struct menustack {
    Menustack prev;
    char *line;
    Brinfo brbeg;
    Brinfo brend;
    int nbrbeg, nbrend;
    int cs, acc, nmatches;
    struct menuinfo info;
    Cmgroup amatches, pmatches, lastmatches, lastlmatches;
};

static int
domenuselect(Hookdef dummy, Chdata dat)
{
    static Chdata fdat = NULL;
    Cmatch **p;
    Cmgroup *pg;
    Thingy cmd;
    Menustack u = NULL;
    int i = 0, acc = 0, wishcol = 0, setwish = 0, oe = onlyexpl;
    char *s;

    HEAPALLOC {
	if (fdat || (dummy && (!(s = getsparam("SELECTMIN")) ||
			       (dat && dat->num < atoi(s))))) {
	    if (fdat) {
		fdat->matches = dat->matches;
		fdat->num = dat->num;
	    }
	    LASTALLOC_RETURN 0;
	}
	fdat = dat;
	selectlocalmap(mskeymap);
	noselect = 0;
	mselect = (*(minfo.cur))->gnum;
	for (;;) {
	    onlyexpl = 0;
	    showinglist = -2;
	    zrefresh();
	    inselect = 1;
	    if (noselect)
		break;
	    if (!i) {
		i = mcols * mlines;
		while (i--)
		    if (mtab[i])
			break;
		if (!i)
		    break;
		i = 1;
	    }
	    p = mmtabp;
	    pg = mgtabp;
	    minfo.cur = *p;
	    minfo.group = *pg;
	    if (setwish)
		wishcol = mcol;
	    else if (mcol > wishcol) {
		while (mcol > 0 && p[-1] == minfo.cur)
		    mcol--, p--, pg--;
	    } else if (mcol < wishcol) {
		while (mcol < mcols - 1 && p[1] == minfo.cur)
		    mcol++, p++, pg++;
	    }
	    setwish = 0;

	getk:

	    if (!(cmd = getkeycmd()) || cmd == Th(z_sendbreak))
		break;
	    else if (cmd == Th(z_acceptline)) {
		acc = 1;
		break;
	    } else if (cmd == Th(z_acceptandinfernexthistory)) {
		Menustack s = (Menustack) zhalloc(sizeof(*s));

		s->prev = u;
		u = s;
		s->line = dupstring((char *) line);
		s->cs = cs;
		memcpy(&(s->info), &minfo, sizeof(struct menuinfo));
		s->amatches = amatches;
		s->pmatches = pmatches;
		s->lastmatches = lastmatches;
		s->lastlmatches = lastlmatches;
		s->acc = menuacc;
		s->brbeg = dupbrinfo(brbeg, NULL);
		s->brend = dupbrinfo(brend, NULL);
		s->nbrbeg = nbrbeg;
		s->nbrend = nbrend;
		s->nmatches = nmatches;
		menucmp = menuacc = 0;
		fixsuffix();
		validlist = 0;
		amatches = pmatches = lastmatches = NULL;
		invalidatelist();
		PERMALLOC {
		    menucomplete(zlenoargs);
		} LASTALLOC;
		if (dat->num < 2 || !minfo.cur || !*(minfo.cur)) {
		    noselect = clearlist = listshown = 1;
		    onlyexpl = 0;
		    zrefresh();
		    break;
		}
		clearlist = listshown = 1;
		mselect = (*(minfo.cur))->gnum;
		setwish = 1;
		continue;
	    } else if (cmd == Th(z_acceptandhold) ||
		       cmd == Th(z_acceptandmenucomplete)) {
		Menustack s = (Menustack) zhalloc(sizeof(*s));

		s->prev = u;
		u = s;
		s->line = dupstring((char *) line);
		s->cs = cs;
		memcpy(&(s->info), &minfo, sizeof(struct menuinfo));
		s->amatches = s->pmatches =
		    s->lastmatches = s->lastlmatches = NULL;
		s->acc = menuacc;
		s->brbeg = dupbrinfo(brbeg, NULL);
		s->brend = dupbrinfo(brend, NULL);
		s->nbrbeg = nbrbeg;
		s->nbrend = nbrend;
		s->nmatches = nmatches;
		acceptlast();
		do_menucmp(0);
		mselect = (*(minfo.cur))->gnum;
		setwish = 1;
		continue;
	    } else if (cmd == Th(z_undo)) {
		int l;

		if (!u)
		    goto getk;

		cs = 0;
		foredel(ll);
		spaceinline(l = strlen(u->line));
		strncpy((char *) line, u->line, l);
		cs = u->cs;
		menuacc = u->acc;
		memcpy(&minfo, &(u->info), sizeof(struct menuinfo));
		p = &(minfo.cur);
		if (u->lastmatches && lastmatches != u->lastmatches) {
		    if (lastmatches)
			freematches(lastmatches);
		    amatches = u->amatches;
		    pmatches = u->pmatches;
		    lastmatches = u->lastmatches;
		    lastlmatches = u->lastlmatches;
		    nmatches = u->nmatches;
		    hasoldlist = 1;
		}
		PERMALLOC {
		    freebrinfo(brbeg);
		    freebrinfo(brend);
		    brbeg = dupbrinfo(u->brbeg, &lastbrbeg);
		    brend = dupbrinfo(u->brend, &lastbrend);
		    nbrbeg = u->nbrbeg;
		    nbrend = u->nbrend;
		} LASTALLOC;
		u = u->prev;
		clearlist = 1;
		setwish = 1;
		listdat.valid = 0;
	    } else if (cmd == Th(z_redisplay)) {
		redisplay(zlenoargs);
		continue;
	    } else if (cmd == Th(z_clearscreen)) {
		clearscreen(zlenoargs);
		continue;
	    } else if (cmd == Th(z_downhistory) ||
		       cmd == Th(z_downlineorhistory) ||
		       cmd == Th(z_downlineorsearch) ||
		       cmd == Th(z_vidownlineorhistory)) {
		do {
		    if (mline == mlines - 1) {
			p -= mline * mcols;
			mline = 0;
		    } else {
			mline++;
			p += mcols;
		    }
		    if (adjust_mcol(wishcol, &p, NULL))
			continue;
		} while (!*p);
	    } else if (cmd == Th(z_uphistory) ||
		       cmd == Th(z_uplineorhistory) ||
		       cmd == Th(z_uplineorsearch) ||
		       cmd == Th(z_viuplineorhistory)) {
		do {
		    if (!mline) {
			mline = mlines - 1;
			p += mline * mcols;
		    } else {
			mline--;
			p -= mcols;
		    }
		    if (adjust_mcol(wishcol, &p, NULL))
			continue;
		} while (!*p);
	    } else if (cmd == Th(z_forwardchar) || cmd == Th(z_viforwardchar)) {
		int omcol = mcol;
		Cmatch *op = *p;

		do {
		    if (mcol == mcols - 1) {
			p -= mcol;
			mcol = 0;
		    } else {
			mcol++;
			p++;
		    }
		} while (!*p || (mcol != omcol && *p == op));
		wishcol = mcol;
	    } else if (cmd == Th(z_backwardchar) || cmd == Th(z_vibackwardchar)) {
		int omcol = mcol;
		Cmatch *op = *p;

		do {
		    if (!mcol) {
			mcol = mcols - 1;
			p += mcol;
		    } else {
			mcol--;
			p--;
		    }
		} while (!*p || (mcol != omcol && *p == op));
		wishcol = mcol;
	    } else if (cmd == Th(z_beginningofbufferorhistory) ||
		       cmd == Th(z_beginningofline) ||
		       cmd == Th(z_beginningoflinehist) ||
		       cmd == Th(z_vibeginningofline)) {
		p -= mcol;
		mcol = 0;
		while (!*p) {
		    mcol++;
		    p++;
		}
		wishcol = 0;
	    } else if (cmd == Th(z_endofbufferorhistory) ||
		       cmd == Th(z_endofline) ||
		       cmd == Th(z_endoflinehist) ||
		       cmd == Th(z_viendofline)) {
		p += mcols - mcol - 1;
		mcol = mcols - 1;
		while (!*p) {
		    mcol--;
		    p--;
		}
		wishcol = mcols - 1;
	    } else if (cmd == Th(z_forwardword) ||
		       cmd == Th(z_emacsforwardword) ||
		       cmd == Th(z_viforwardword) ||
		       cmd == Th(z_viforwardwordend)) {
		Cmgroup g = *pg;
		int ol = mline;

		do {
		    if (mline == mlines - 1) {
			p -= mline * mcols;
			pg -= mline * mcols;
			mline = 0;
		    } else {
			mline++;
			p += mcols;
			pg += mcols;
		    }
		    if (adjust_mcol(wishcol, &p, &pg))
			continue;
		} while (ol != mline && (*pg == g || !*pg));
	    } else if (cmd == Th(z_backwardword) ||
		       cmd == Th(z_emacsbackwardword) ||
		       cmd == Th(z_vibackwardword)) {
		Cmgroup g = *pg;
		int ol = mline;

		do {
		    if (!mline) {
			mline = mlines - 1;
			p += mline * mcols;
			pg += mline * mcols;
		    } else {
			mline--;
			p -= mcols;
			pg -= mcols;
		    }
		    if (adjust_mcol(wishcol, &p, &pg))
			continue;
		} while (ol != mline && (*pg == g || !*pg));
	    } else if (cmd == Th(z_completeword) ||
		       cmd == Th(z_expandorcomplete) ||
		       cmd == Th(z_expandorcompleteprefix) ||
		       cmd == Th(z_menucomplete) ||
		       cmd == Th(z_menuexpandorcomplete) ||
		       !strcmp(cmd->nam, "menu-select") ||
		       !strcmp(cmd->nam, "complete-word") ||
		       !strcmp(cmd->nam, "expand-or-complete") ||
		       !strcmp(cmd->nam, "expand-or-complete-prefix") ||
		       !strcmp(cmd->nam, "menu-complete") ||
		       !strcmp(cmd->nam, "menu-expand-or-complete")) {
		do_menucmp(0);
		mselect = (*(minfo.cur))->gnum;
		setwish = 1;
		continue;
	    } else if (cmd == Th(z_reversemenucomplete) ||
		       !strcmp(cmd->nam, "reverse-menu-complete")) {
		reversemenucomplete(zlenoargs);
		mselect = (*(minfo.cur))->gnum;
		setwish = 1;
		continue;
	    } else {
		ungetkeycmd();
		break;
	    }
	    do_single(**p);
	    mselect = (**p)->gnum;
	}
	if (u)
	    for (; u; u = u->prev)
		if (u->lastmatches != lastmatches)
		    freematches(u->lastmatches);

	selectlocalmap(NULL);
	mselect = -1;
	inselect = 0;
	if (acc) {
	    menucmp = 0;
	    lastambig = 0;
	    do_single(*(minfo.cur));
	}
	if (!noselect) {
	    showinglist = -2;
	    onlyexpl = oe;
	    zrefresh();
	}
	fdat = NULL;
    } LASTALLOC;
    return (!noselect ^ acc);
}

/* The widget function. */

static int
menuselect(char **args)
{
    int d = 0;

    if (!minfo.cur) {
	menucomplete(args);
	if ((minfo.cur && minfo.asked == 2) || getsparam("ZLS_SELECT"))
	    return 0;
	d = 1;
    }
    if (minfo.cur && (minfo.asked == 2 || domenuselect(NULL, NULL)) && !d)
	menucomplete(args);

    return 0;
}

/**/
int
setup_complist(Module m)
{
    return 0;
}

/**/
int
boot_complist(Module m)
{
    mtab = NULL;
    mgtab = NULL;
    mselect = -1;
    inselect = 0;

    w_menuselect = addzlefunction("menu-select", menuselect,
                                    ZLE_MENUCMP|ZLE_KEEPSUFFIX|ZLE_ISCOMP);
    if (!w_menuselect) {
	zwarnnam(m->nam, "name clash when adding ZLE function `menu-select'",
		 NULL, 0);
	return -1;
    }
    addhookfunc("list_matches", (Hookfn) complistmatches);
    addhookfunc("menu_start", (Hookfn) domenuselect);
    mskeymap = newkeymap(NULL, "menuselect");
    linkkeymap(mskeymap, "menuselect", 1);
    bindkey(mskeymap, "\t", refthingy(t_completeword), NULL);
    bindkey(mskeymap, "\n", refthingy(t_acceptline), NULL);
    bindkey(mskeymap, "\r", refthingy(t_acceptline), NULL);
    bindkey(mskeymap, "\33[A",  refthingy(t_uplineorhistory), NULL);
    bindkey(mskeymap, "\33[B",  refthingy(t_downlineorhistory), NULL);
    bindkey(mskeymap, "\33[C",  refthingy(t_forwardchar), NULL);
    bindkey(mskeymap, "\33[D",  refthingy(t_backwardchar), NULL);
    bindkey(mskeymap, "\33OA",  refthingy(t_uplineorhistory), NULL);
    bindkey(mskeymap, "\33OB",  refthingy(t_downlineorhistory), NULL);
    bindkey(mskeymap, "\33OC",  refthingy(t_forwardchar), NULL);
    bindkey(mskeymap, "\33OD",  refthingy(t_backwardchar), NULL);
    return 0;
}

#ifdef MODULE

/**/
int
cleanup_complist(Module m)
{
    free(mtab);
    free(mgtab);

    deletezlefunction(w_menuselect);
    deletehookfunc("list_matches", (Hookfn) complistmatches);
    deletehookfunc("menu_start", (Hookfn) domenuselect);
    unlinkkeymap("menuselect", 1);
    return 0;
}

/**/
int
finish_complist(Module m)
{
    return 0;
}

#endif
