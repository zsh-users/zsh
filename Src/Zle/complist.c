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
#define COL_TC 13
#define COL_SP 14
#define COL_MA 15
#define COL_HI 16
#define COL_DU 17

#define NUM_COLS 18

/* Maximum number of in-string colours supported. */

#define MAX_POS 11

/* Names of the terminal strings. */

static char *colnames[] = {
    "no", "fi", "di", "ln", "pi", "so", "bd", "cd", "ex", "mi",
    "lc", "rc", "ec", "tc", "sp", "ma", "hi", "du", NULL
};

/* Default values. */

static char *defcols[] = {
    "0", "0", "1;34", "1;36", "33", "1;35", "1;33", "1;33", "1;32", NULL,
    "\033[", "m", NULL, "0", "0", "7", "0", "0"
};

/* This describes a terminal string for a file type. */

typedef struct filecol *Filecol;

struct filecol {
    Patprog prog;		/* group pattern */
    char *col;			/* color string */
    Filecol next;		/* next one */
};

/* This describes a terminal string for a pattern. */

typedef struct patcol *Patcol;

struct patcol {
    Patprog prog;
    Patprog pat;		/* pattern for match */
    char *cols[MAX_POS + 1];
    Patcol next;
};

/* This describes a terminal string for a filename extension. */

typedef struct extcol *Extcol;

struct extcol {
    Patprog prog;		/* group pattern or NULL */
    char *ext;			/* the extension */
    char *col;			/* the terminal color string */
    Extcol next;		/* the next one in the list */
};

/* This holds all terminal strings. */

typedef struct listcols *Listcols;

struct listcols {
    Filecol files[NUM_COLS];	/* strings for file types */
    Patcol pats;		/* strings for patterns */
    Extcol exts;		/* strings for extensions */
};

/* This parses the value of a definition (the part after the `=').
 * The return value is a pointer to the character after it. */

static char *
getcolval(char *s, int multi)
{
    char *p;

    for (p = s; *s && *s != ':' && (!multi || *s != '='); p++, s++) {
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
    Patprog gprog = NULL;

    if (*s == '(') {
	char *p;
	int l = 0;

	for (p = s + 1, l = 0; *p && (*p != ')' || l); p++)
	    if (*p == '\\' && p[1])
		p++;
	    else if (*p == '(')
		l++;
	    else if (*p == ')')
		l--;

	if (*p == ')') {
	    char sav = p[1];

	    p[1] = '\0';
	    tokenize(s);
	    gprog = patcompile(s, 0, NULL);
	    p[1]  =sav;

	    s = p + 1;
	}
    }
    if (*s == '*') {
	Extcol ec, eo;
	char *n, *p;

	/* This is for an extension. */

	n = ++s;
	while (*s && *s != '=')
	    s++;
	if (!*s)
	    return s;
	*s++ = '\0';
	p = getcolval(s, 0);
	ec = (Extcol) zhalloc(sizeof(*ec));
	ec->prog = gprog;
	ec->ext = n;
	ec->col = s;
	ec->next = NULL;
	if ((eo = c->exts)) {
	    while (eo->next)
		eo = eo->next;
	    eo->next = ec;
	} else
	    c->exts = ec;
	if (*p)
	    *p++ = '\0';
	return p;
    } else if (*s == '=') {
	char *p = ++s, *t, *cols[MAX_POS];
	int ncols = 0;
	Patprog prog;

	/* This is for a pattern. */

	while (*s && *s != '=')
	    s++;
	if (!*s)
	    return s;
	*s++ = '\0';
	while (1) {
	    t = getcolval(s, 1);
	    if (ncols < MAX_POS)
		cols[ncols++] = s;
	    s = t;
	    if (*s != '=')
		break;
	    *s++ = '\0';
	}
	tokenize(p);
	if ((prog = patcompile(p, 0, NULL))) {
	    Patcol pc, po;
	    int i;

	    pc = (Patcol) zhalloc(sizeof(*pc));
	    pc->prog = gprog;
	    pc->pat = prog;
	    for (i = 0; i < ncols; i++)
		pc->cols[i] = cols[i];
	    pc->cols[i] = NULL;
	    pc->next = NULL;
	    if ((po = c->pats)) {
		while (po->next)
		    po = po->next;
		po->next = pc;
	    } else
		c->pats = pc;
	}
	if (*t)
	    *t++ = '\0';
	return t;
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
	    if (!strcmp(n, *nn))
		break;
	p = getcolval(s, 0);
	if (*nn) {
	    Filecol fc, fo;

	    fc = (Filecol) zhalloc(sizeof(*fc));
	    fc->prog = (i == COL_EC || i == COL_LC || i == COL_RC ?
			NULL : gprog);
	    fc->col = s;
	    fc->next = NULL;
	    if ((fo = c->files[i])) {
		while (fo->next)
		    fo = fo->next;
		fo->next = fc;
	    } else
		c->files[i] = fc;
	}
	if (*p)
	    *p++ = '\0';
	return p;
    }
}

static Filecol
filecol(char *col)
{
    Filecol fc;

    fc = (Filecol) zhalloc(sizeof(*fc));
    fc->prog = NULL;
    fc->col = col;
    fc->next = NULL;

    return fc;
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
	    c->files[i] = filecol("");
	c->pats = NULL;
	c->exts = NULL;
	
	if ((s = tcstr[TCSTANDOUTBEG]) && s[0]) {
	    c->files[COL_MA] = filecol(s);
	    c->files[COL_EC] = filecol(tcstr[TCSTANDOUTEND]);
	} else
	    c->files[COL_MA] = filecol("");
	lr_caplen = 0;
	if ((max_caplen = strlen(c->files[COL_MA]->col)) <
	    (l = strlen(c->files[COL_EC]->col)))
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
	if (!c->files[i] || !c->files[i]->col)
	    c->files[i] = filecol(defcols[i]);
	if (c->files[i] && c->files[i]->col &&
	    (l = strlen(c->files[i]->col)) > max_caplen)
	    max_caplen = l;
    }
    lr_caplen = strlen(c->files[COL_LC]->col) + strlen(c->files[COL_RC]->col);

    /* Default for missing files. */
    if (!c->files[COL_MI] || !c->files[COL_MI]->col)
	c->files[COL_MI] = c->files[COL_FI];

    return;
}

/* Information about the list shown. */

static int noselect, mselect, inselect, mcol, mline, mcols, mlines, mmlen;
static int selected;
static Cmatch **mtab, **mmtabp;
static Cmgroup *mgtab, *mgtabp;
static struct listcols mcolors;

/* Information for in-string colours. */

static int nrefs;
static int begpos[MAX_POS], curisbeg;
static int endpos[MAX_POS], curisend;
static char **patcols, *curiscols[MAX_POS];
static int curiscol;

/* The last color used. */

static char *last_cap;

static void
zlrputs(Listcols c, char *cap)
{
    if (strcmp(last_cap, cap)) {
	VARARR(char, buf, lr_caplen + max_caplen + 1);

	strcpy(buf, c->files[COL_LC]->col);
	strcat(buf, cap);
	strcat(buf, c->files[COL_RC]->col);

	tputs(buf, 1, putshout);

	strcpy(last_cap, cap);
    }
}

static void
zcputs(Listcols c, char *group, int colour)
{
    Filecol fc;

    for (fc = c->files[colour]; fc; fc = fc->next)
	if (fc->col &&
	    (!fc->prog || !group || pattry(fc->prog, group))) {
	    zlrputs(c, fc->col);

	    return;
	}
}

/* Turn off colouring. */

static void
zcoff(void)
{
    if (mcolors.files[COL_EC] && mcolors.files[COL_EC]->col)
	tputs(mcolors.files[COL_EC]->col, 1, putshout);
    else
	zcputs(&mcolors, NULL, COL_NO);
}


static void
initiscol(Listcols c)
{
    int i;

    zlrputs(c, patcols[0]);

    curiscols[curiscol = 0] = *patcols++;

    curisbeg = curisend = 0;

    for (i = nrefs;  i < MAX_POS; i++)
	begpos[i] = -1, endpos[i] = 0xfffffff;
}

static void
doiscol(Listcols c, int pos)
{
    if (pos > endpos[curisend]) {
	curisend++;
	if (curiscol) {
	    zcputs(c, NULL, COL_NO);
	    zlrputs(c, curiscols[--curiscol]);
	}
    }
    if (pos == begpos[curisbeg] && *patcols) {
	curisbeg++;

	zcputs(c, NULL, COL_NO);
	zlrputs(c, *patcols);

	curiscols[++curiscol] = *patcols++;
    }
}

/* Stripped-down version of printfmt(). But can do in-string colouring. */

static void
clprintfmt(Listcols c, char *p)
{
    int cc = 0, i = 0;

    initiscol(c);

    for (; *p; p++) {
	doiscol(c, i++);
	cc++;
	if (*p == '\n') {
	    if (tccan(TCCLEAREOL))
		tcout(TCCLEAREOL);
	    else {
		int s = columns - 1 - (cc % columns);

		while (s-- > 0)
		    putc(' ', shout);
	    }
	    cc = 0;
	}
	putc(*p, shout);
    }
    if (tccan(TCCLEAREOL))
	tcout(TCCLEAREOL);
    else {
	int s = columns - 1 - (cc % columns);

	while (s-- > 0)
	    putc(' ', shout);
    }
}

/* Local version of nicezputs() with in-string colouring. */

static void
clnicezputs(Listcols c, char *s)
{
    int cc, i = 0;

    initiscol(c);

    while ((cc = *s++)) {
	doiscol(c, i++);
	if (itok(cc)) {
	    if (cc <= Comma)
		cc = ztokens[cc - Pound];
	    else 
		continue;
	}
	if (cc == Meta)
	    cc = *s++ ^ 32;
	fputs(nicechar(cc), shout);
    }
}

/* Get the terminal color string for the given match. */

static int
putmatchcol(Listcols c, char *group, char *n)
{
    Patcol pc;

    nrefs = MAX_POS - 1;

    for (pc = c->pats; pc; pc = pc->next)
	if ((!pc->prog || !group || pattry(pc->prog, group)) &&
	    pattryrefs(pc->pat, n, &nrefs, begpos, endpos)) {
	    if (pc->cols[1]) {
		patcols = pc->cols;

		return 1;
	    }
	    zlrputs(c, pc->cols[0]);

	    return 0;
	}

    zcputs(c, group, COL_NO);

    return 0;
}

/* Get the terminal color string for the file with the given name and
 * file modes. */

static int
putfilecol(Listcols c, char *group, char *n, mode_t m)
{
    int colour;
    Extcol ec;
    Patcol pc;

    for (ec = c->exts; ec; ec = ec->next)
	if (strsfx(ec->ext, n) &&
	    (!ec->prog || !group || pattry(ec->prog, group))) {
	    zlrputs(c, ec->col);

	    return 0;
	}

    nrefs = MAX_POS - 1;

    for (pc = c->pats; pc; pc = pc->next)
	if ((!pc->prog || !group || pattry(pc->prog, group)) &&
	    pattryrefs(pc->pat, n, &nrefs, begpos, endpos)) {
	    if (pc->cols[1]) {
		patcols = pc->cols;

		return 1;
	    }
	    zlrputs(c, pc->cols[0]);

	    return 0;
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

    zcputs(c, group, colour);

    return 0;
}

static Cmgroup last_group;

static void
clprintm(Cmgroup g, Cmatch *mp, int mc, int ml, int lastc, int width,
	 char *path, struct stat *buf)
{
    Cmatch m;
    int len, subcols = 0;

    if (g != last_group)
        *last_cap = '\0';

    last_group = g;

    if (!mp) {
	zcputs(&mcolors, g->name, COL_SP);
	len = width - 2;
	while (len-- > 0)
	    putc(' ', shout);
	zcoff();
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
	    zcputs(&mcolors, g->name, COL_MA);
	} else if (m->flags & CMF_NOLIST)
	    zcputs(&mcolors, g->name, COL_HI);
	else if (mselect >= 0 && (m->flags & (CMF_MULT | CMF_FMULT)))
	    zcputs(&mcolors, g->name, COL_DU);
	else
	    subcols = putmatchcol(&mcolors, g->name, m->disp);
	if (subcols)
	    clprintfmt(&mcolors, m->disp);
	else
	    printfmt(m->disp, 0, 1, 0);
	zcoff();
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
	    zcputs(&mcolors, g->name, COL_MA);
	} else if (m->flags & CMF_NOLIST)
	    zcputs(&mcolors, g->name, COL_HI);
	else if (mselect >= 0 && (m->flags & (CMF_MULT | CMF_FMULT)))
	    zcputs(&mcolors, g->name, COL_DU);
	else if (buf)
	    subcols = putfilecol(&mcolors, g->name, path, buf->st_mode);
	else
	    subcols = putmatchcol(&mcolors, g->name, (m->disp ? m->disp : m->str));

	if (subcols)
	    clnicezputs(&mcolors, (m->disp ? m->disp : m->str));
	else
	    nicezputs((m->disp ? m->disp : m->str), shout);
	len = niceztrlen(m->disp ? m->disp : m->str);

	 if (isset(LISTTYPES) && buf) {
	    if (m->gnum != mselect) {
		zcoff();
		zcputs(&mcolors, g->name, COL_TC);
	    }
	    putc(file_type(buf->st_mode), shout);
	    len++;
        }
	if ((len = width - len - 2) > 0) {
	    if (m->gnum != mselect) {
		zcoff();
		zcputs(&mcolors, g->name, COL_SP);
	    }
	    while (len-- > 0)
		putc(' ', shout);
	}
	zcoff();
	if (!lastc) {
	    zcputs(&mcolors, g->name, COL_NO);
	    fputs("  ", shout);
	    zcoff();
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

    calclist(mselect >= 0);

    if (!listdat.nlines || (mselect >= 0 &&
			    (!(isset(USEZLE) && !termflags &&
			       complastprompt && *complastprompt) ||
			     (listdat.nlines + nlnct - 1) >= lines))) {
	showinglist = listshown = 0;
	noselect = 1;
	amatches = oamatches;
	return 1;
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
    last_cap = (char *) zhalloc(max_caplen + 1);
    *last_cap = '\0';

    if (!printlist(1, clprintm, (mselect >= 0)) || listdat.nlines >= lines)
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
    int i = 0, acc = 0, wishcol = 0, setwish = 0, oe = onlyexpl, wasnext = 0;
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
	    selected = 1;
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
	    setwish = wasnext = 0;

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
		invalidate_list();
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
		setwish = wasnext = 1;
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
		accept_last();
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
	    menucmp = lastambig = hasoldlist = 0;
	    do_single(*(minfo.cur));
	}
	if (wasnext) {
	    menucmp = 2;
	    showinglist = -2;
	    minfo.asked = 0;
	}
	if (!noselect) {
	    showinglist = -2;
	    onlyexpl = oe;
	    if (!smatches)
		clearlist = 1;
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
	selected = 0;
	menucomplete(args);
	if ((minfo.cur && minfo.asked == 2) || selected)
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
    addhookfunc("comp_list_matches", (Hookfn) complistmatches);
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

/**/
int
cleanup_complist(Module m)
{
    free(mtab);
    free(mgtab);

    deletezlefunction(w_menuselect);
    deletehookfunc("comp_list_matches", (Hookfn) complistmatches);
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
