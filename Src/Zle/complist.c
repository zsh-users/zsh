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
	if ((l = (c->cols[i] ? strlen(c->cols[i]) : 0)) > max_caplen)
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

static int noselect, mselect, inselect, mcol, mline, mcols, mlines;
static Cmatch *mmatch, **mtab;
static Cmgroup mgroup, *mgtab;

/* List the matches. Most of this is just taken from ilistmatches(),
 * of course. */

static int
complistmatches(Hookdef dummy, Chdata dat)
{
    Cmgroup amatches = dat->matches, g;
    Cmatch *p, m;
    Cexpl *e;
    int nlines = 0, ncols, nlist = 0, longest = 1, pnl = 0, opl = 0;
    int of = isset(LISTTYPES), cf;
    int mc, ml = 0, cc, hasm = 0, cl = -1;
    struct listcols col;

    if (minfo.asked == 2) {
	showinglist = 0;
	return (noselect = 1);
    }
    getcols(&col);

    for (g = amatches; g; g = g->next) {
	char **pp = g->ylist;
	int nl = 0, l;

	if (pp) {
	    /* We have an ylist, lets see, if it contains newlines. */
	    while (!nl && *pp)
		nl = !!strchr(*pp++, '\n');

	    pp = g->ylist;
	    if (nl || !pp[1]) {
		/* Yup, there are newlines, count lines. */
		char *nlptr, *sptr;

		g->flags |= CGF_LINES;
		noselect = 1;
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
		if (m->disp) {
		    if (m->flags & CMF_DISPLINE) {
			nlines += 1 + printfmt(m->disp, 0, 0, 0);
			g->flags |= CGF_HASDL;
		    } else if ((l = strlen(m->disp)) > longest)
			longest = l;
		    nlist++;
		} else if (!(m->flags & CMF_NOLIST)) {
		    if ((l = niceztrlen(m->str)) > longest)
			longest = l;
		    nlist++;
		} else
		    noselect = 1;
	    }
	}
	if ((e = g->expls)) {
	    while (*e) {
		if ((*e)->count)
		    nlines += 1 + printfmt((*e)->str, (*e)->count, 0, 1);
		e++;
	    }
	}
    }
    longest += 2 + of;
    if ((ncols = (columns + 1) / longest)) {
	for (g = amatches; g; g = g->next)
	    nlines += (g->lcount - g->llcount + ncols - 1) / ncols;
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
		    if (m->disp) {
			if (m->flags & CMF_DISPLINE)
			    nlines += 1 + printfmt(m->disp, 0, 0, 0);
			else
			    nlines += 1 + ((1 + niceztrlen(m->disp)) / columns);
		    } else if (!(m->flags & CMF_NOLIST))
			nlines += 1 + ((1 + niceztrlen(m->str)) / columns);
	}
    }
    cf = (isset(USEZLE) && !termflags && complastprompt && *complastprompt);
    if (!nlines || (mselect >= 0 && (!cf || (nlines + nlnct - 1) >= lines))) {
	showinglist = listshown = 0;
	noselect = 1;
	return 1;
    }
    /* Set the cursor below the prompt. */
    if (inselect)
	clearflag = 0;
    trashzle();
    showinglist = listshown = 0;

    clearflag = cf;

    /* Maybe we have to ask if the user wants to see the list. */
    if ((!minfo.cur || !minfo.asked) &&
	((complistmax && nlist > complistmax) ||
	 (!complistmax && nlines >= lines))) {
	int qup;
	zsetterm();
	qup = printfmt("zsh: do you wish to see all %n possibilities? ", nlist, 1, 1);
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
	    noselect = 1;
	    if (minfo.cur)
		minfo.asked = 2;
	    return 1;
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
    if (mselect >= 0) {
	int i;

	i = ncols * nlines;
	free(mtab);
	mtab = (Cmatch **) zalloc(i * sizeof(Cmatch **));
	memset(mtab, 0, i * sizeof(Cmatch **));
	free(mgtab);
	mgtab = (Cmgroup *) zalloc(i * sizeof(Cmgroup));
	memset(mgtab, 0, i * sizeof(Cmgroup));
	mcols = ncols;
	mlines = cl = nlines;
	if (cl < 2) {
	    cl = -1;
	    if (tccan(TCCLEAREOD))
		tcout(TCCLEAREOD);
	}
    }
    /* Now print the matches. */
    last_col = COL_NO - 1;
    g = amatches;
    while (g) {
	char **pp = g->ylist;

	if ((e = g->expls)) {
	    int l;

	    while (*e) {
		if ((*e)->count) {
		    if (pnl) {
			putc('\n', shout);
			pnl = 0;
			ml++;
			if (cl >= 0 && --cl <= 1) {
			    cl = -1;
			    if (tccan(TCCLEAREOD))
				tcout(TCCLEAREOD);
			}
		    }
		    l = printfmt((*e)->str, (*e)->count, 1, 1);
		    ml += l;
		    if (cl >= 0 && (cl -= l) <= 1) {
			cl = -1;
			if (tccan(TCCLEAREOD))
			    tcout(TCCLEAREOD);
		    }
		    pnl = 1;
		}
		e++;
	    }
	}
	if (pp && *pp) {
	    if (pnl) {
		putc('\n', shout);
		pnl = 0;
		ml++;
		if (cl >= 0 && --cl <= 1) {
		    cl = -1;
		    if (tccan(TCCLEAREOD))
			tcout(TCCLEAREOD);
		}
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
		    mc = 0;
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
		    if (n) {
			putc('\n', shout);
			ml++;
			if (cl >= 0 && --cl <= 1) {
			    cl = -1;
			    if (tccan(TCCLEAREOD))
				tcout(TCCLEAREOD);
			}
		    }
		    pp++;
		}
	    }
	} else if (g->lcount) {
	    int n = g->lcount - g->llcount, nl = (n + ncols - 1) / ncols;
	    int nc = nl, i, j, a = 0;
	    int zt;
	    Cmatch *q;

	    if (g->flags & CGF_HASDL) {
		for (p = g->matches; (m = *p); p++)
		    if (m->disp && (m->flags & CMF_DISPLINE)) {
			if (pnl) {
			    putc('\n', shout);
			    pnl = 0;
			    ml++;
			    if (cl >= 0 && --cl <= 1) {
				cl = -1;
				if (tccan(TCCLEAREOD))
				    tcout(TCCLEAREOD);
			    }
			}
			hasm = 1;
			if (mselect >= 0) {
			    for (i = 0; i < ncols; i++) {
				mtab[i + (ncols * ml)] = p;
				mgtab[i + (ncols * ml)] = g;
			    }
			}
			if (m->gnum == mselect) {
			    mline = ml;
			    mmatch = p;
			    mgroup = g;
			    cc = COL_MA;
			} else
			    cc = COL_NO;
			zcputs(&col, cc);
			printfmt(m->disp, 0, 1, 0);
			if (col.cols[COL_EC])
			    tputs(col.cols[COL_EC], 1, putshout);
			else
			    zcputs(&col, COL_NO);
			pnl = 1;
		    }
	    }
	    if (n && pnl) {
		putc('\n', shout);
		pnl = 0;
		ml++;
		if (cl >= 0 && --cl <= 1) {
		    cl = -1;
		    if (tccan(TCCLEAREOD))
			tcout(TCCLEAREOD);
		}
	    }
	    for (p = skipnolist(g->matches); n && nl--;) {
		i = ncols;
		mc = 0;
		q = p;
		while (n && i--) {
		    if (!(m = *q)) {
			zcputs(&col, COL_MI);
			a = longest - 2;
			while (a--)
			    putc(' ', shout);
			if (col.cols[COL_EC])
			    tputs(col.cols[COL_EC], 1, putshout);
			else
			    zcputs(&col, COL_NO);
			break;
		    }
		    hasm = 1;
		    if (mselect >= 0) {
			mtab[mc + (ncols * ml)] = q;
			mgtab[mc + (ncols * ml)] = g;
		    }
		    if (m->gnum == mselect) {
			mcol = mc;
			mline = ml;
			mmatch = q;
			mgroup = g;
			cc = COL_MA;
		    } else
			cc = -1;
		    if (!m->disp && m->flags & CMF_FILE) {
			struct stat buf;
			char *pb;

			pb = (char *) zhalloc((m->prpre ? strlen(m->prpre) : 0) +
					     3 + strlen(m->str));
			sprintf(pb, "%s%s", (m->prpre ? m->prpre : "./"),
				m->str);

			zt = ztat(pb, &buf, 1);
			if (cc >= 0)
			    zcputs(&col, cc);
			else if (zt)
			    zcputs(&col, COL_NO);
			else
			    putcolstr(&col, pb, buf.st_mode);
			nicezputs(m->str, shout);
			if (zt)
			    putc(' ', shout);
			else
			    putc(file_type(buf.st_mode), shout);
		    } else {
			zcputs(&col, cc >= 0 ? cc : COL_NO);
			nicezputs((m->disp ? m->disp : m->str), shout);
			if (of)
			    putc(' ', shout);
		    }
		    a = longest - niceztrlen(m->disp ? m->disp : m->str) - 2 - of;
		    while (a--)
			putc(' ', shout);
		    if (col.cols[COL_EC])
			tputs(col.cols[COL_EC], 1, putshout);
		    else
			zcputs(&col, COL_NO);
		    if (i) {
			zcputs(&col, COL_NO);
			fputs("  ", shout);
			if (col.cols[COL_EC])
			    tputs(col.cols[COL_EC], 1, putshout);
			else
			    zcputs(&col, COL_NO);
		    }
		    if (--n)
			for (j = nc; j && *q; j--)
			    q = skipnolist(q + 1);
		    mc++;
		}
		if (i > 0) {
		    zcputs(&col, COL_MI);
		    a = longest - 2;
		    while (a--)
			putc(' ', shout);
		    if (col.cols[COL_EC])
			tputs(col.cols[COL_EC], 1, putshout);
		    else
			zcputs(&col, COL_NO);
		}
		if (n) {
		    putc('\n', shout);
		    ml++;
		    if (cl >= 0 && --cl <= 1) {
			cl = -1;
			if (tccan(TCCLEAREOD))
			    tcout(TCCLEAREOD);
		    }
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
    if (!hasm || nlines >= lines)
	noselect = 1;

    return noselect;
}

typedef struct menustack *Menustack;

struct menustack {
    Menustack prev;
    char *line;
    int cs, acc;
    struct menuinfo info;
    Cmgroup amatches, pmatches, lmatches;
};

static int
domenuselect(Hookdef dummy, Chdata dat)
{
    static Chdata fdat = NULL;
    Cmatch **p;
    Cmgroup *pg;
    Thingy cmd;
    Menustack u = NULL;
    int i = 0, acc = 0;
    char *s;

    if (fdat || (dummy && (!(s = getsparam("SELECTMIN")) ||
			   (dat && dat->num < atoi(s))))) {
	if (fdat) {
	    fdat->matches = dat->matches;
	    fdat->num = dat->num;
	}
	return 0;
    }
    fdat = dat;
    selectlocalmap(mskeymap);
    noselect = 0;
    mselect = (*(minfo.cur))->gnum;
    for (;;) {
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
	p = mtab + mcol + (mline * mcols);
	pg = mgtab + mcol + (mline * mcols);
	minfo.cur = *p;
	minfo.group = *pg;

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
	    s->lmatches = lmatches;
	    s->acc = menuacc;
	    menucmp = menuacc = 0;
	    fixsuffix();
	    validlist = 0;
	    pmatches = NULL;
	    invalidatelist();
	    menucomplete(zlenoargs);
	    if (dat->num < 2 || !minfo.cur || !*(minfo.cur)) {
		noselect = clearlist = listshown = 1;
		zrefresh();
		break;
	    }
	    clearlist = listshown = 1;
	    mselect = (*(minfo.cur))->gnum;
	    continue;
	} else if (cmd == Th(z_acceptandhold) ||
		 cmd == Th(z_acceptandmenucomplete)) {
	    Menustack s = (Menustack) zhalloc(sizeof(*s));

	    s->prev = u;
	    u = s;
	    s->line = dupstring((char *) line);
	    s->cs = cs;
	    memcpy(&(s->info), &minfo, sizeof(struct menuinfo));
	    s->amatches = s->pmatches = s->lmatches = NULL;
	    s->acc = menuacc;
	    acceptlast();
	    do_menucmp(0);
	    mselect = (*(minfo.cur))->gnum;
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
	    if (u->pmatches && pmatches != u->pmatches) {
		freematches();
		amatches = u->amatches;
		pmatches = u->pmatches;
		lmatches = u->lmatches;
		hasperm = 1;
	    }
	    u = u->prev;
	    clearlist = 1;
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
	    } while (!*p);
	} else if (cmd == Th(z_forwardchar) || cmd == Th(z_viforwardchar)) {
	    do {
		if (mcol == mcols - 1) {
		    p -= mcol;
		    mcol = 0;
		} else {
		    mcol++;
		    p++;
		}
	    } while (!*p);
	} else if (cmd == Th(z_backwardchar) || cmd == Th(z_vibackwardchar)) {
	    do {
		if (!mcol) {
		    mcol = mcols - 1;
		    p += mcol;
		} else {
		    mcol--;
		    p--;
		}
	    } while (!*p);
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
	    continue;
	} else if (cmd == Th(z_reversemenucomplete) ||
		   !strcmp(cmd->nam, "reverse-menu-complete")) {
	    reversemenucomplete(zlenoargs);
	    mselect = (*(minfo.cur))->gnum;
	    continue;
	} else {
	    ungetkeycmd();
	    break;
	}
	do_single(**p);
	mselect = (**p)->gnum;
    }
    if (u) {
	int hp = hasperm;
	Cmgroup m = pmatches;

	for (; u; u = u->prev) {
	    if (u->pmatches != m) {
		pmatches = u->pmatches;
		freematches();
	    }
	}
	pmatches = m;
	hasperm = hp;
    }
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
	zrefresh();
    }
    fdat = NULL;
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
