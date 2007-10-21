/*
 * curses.c - curses windowing module for zsh
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 2007  Clint Adams
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Clint Adams or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Clint Adams and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Clint Adams and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Clint Adams and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#define _XOPEN_SOURCE_EXTENDED 1

#include <ncurses.h>
#ifndef MULTIBYTE_SUPPORT
# undef HAVE_SETCCHAR
# undef HAVE_WADDWSTR
#endif

#ifdef HAVE_SETCCHAR
# include <wchar.h>
#endif

#include <stdio.h>

#include "curses.mdh"
#include "curses.pro"

typedef struct zc_win {
    WINDOW *win;
    char *name;
} *ZCWin;

struct zcurses_namenumberpair {
    char *name;
    int number;
};

struct colorpairnode {
    struct hashnode node;
    short colorpair;
};
typedef struct colorpairnode *Colorpairnode;

struct zcurses_subcommand {
    struct zcurses_namenumberpair nn;
    int minargs;
    int maxargs;
};

static WINDOW *win_zero;
static struct ttyinfo saved_tty_state;
static struct ttyinfo curses_tty_state;
static LinkList zcurses_windows;
static HashTable zcurses_colorpairs = NULL;

#define ZCURSES_ERANGE 1
#define ZCURSES_EDEFINED 2
#define ZCURSES_EUNDEFINED 3

#define ZCURSES_UNUSED 1
#define ZCURSES_USED 2

#define ZCURSES_ATTRON 1
#define ZCURSES_ATTROFF 2

#define ZCURSES_SC_INIT 1
#define ZCURSES_SC_ADDWIN 2
#define ZCURSES_SC_DELWIN 3
#define ZCURSES_SC_REFRESH 4
#define ZCURSES_SC_MOVE 5
#define ZCURSES_SC_CHAR 6
#define ZCURSES_SC_STRING 7
#define ZCURSES_SC_BORDER 8
#define ZCURSES_SC_ENDWIN 9
#define ZCURSES_SC_ATTR 10
#define ZCURSES_SC_COLOR 11

static int zc_errno, zc_color_phase=0;
static short next_cp=0;

static const char *
zcurses_strerror(int err)
{
    static const char *errs[] = {
	"unknown error",
	"window number out of range",
	"window already defined",
	NULL };

    return errs[(err < 1 || err > 2) ? 0 : err];
}

static LinkNode
zcurses_getwindowbyname(char *name)
{
    LinkNode node;
    ZCWin w;

    for (node = firstnode(zcurses_windows); node; incnode(node))
	if (w = (ZCWin)getdata(node), !strcmp(w->name, name))
	    return node;

    return NULL;
}

static LinkNode
zcurses_validate_window(char *win, int criteria)
{
    LinkNode target;

    if (win==NULL || strlen(win) < 1) {
	zc_errno = ZCURSES_ERANGE;
	return NULL;
    }

    target = zcurses_getwindowbyname(win);

    if (target && (criteria & ZCURSES_UNUSED)) {
	zc_errno = ZCURSES_EDEFINED;
	return NULL;
    }

    if (!target && (criteria & ZCURSES_USED)) {
	zc_errno = ZCURSES_EUNDEFINED;
	return NULL;
    }

    zc_errno = 0;
    return target;
}

static int
zcurses_free_window(ZCWin w)
{
    if (delwin(w->win)!=OK)
	return 1;

    if (w->name)
	zsfree(w->name);

    zfree(w, sizeof(struct zc_win));

    return 0;
}

static int
zcurses_attribute(WINDOW *w, char *attr, int op)
{
    struct zcurses_namenumberpair *zca;

    static const struct zcurses_namenumberpair zcurses_attributes[] = {
	{"blink", A_BLINK},
	{"bold", A_BOLD},
	{"dim", A_DIM},
	{"reverse", A_REVERSE},
	{"standout", A_STANDOUT},
	{"underline", A_UNDERLINE},
	{NULL, 0}
    };

    if (!attr)
	return 1;

    for(zca=(struct zcurses_namenumberpair *)zcurses_attributes;zca->name;zca++)
	if (!strcmp(attr, zca->name)) {
	    switch(op) {
		case ZCURSES_ATTRON:
		    wattron(w, zca->number);
		    break;
		case ZCURSES_ATTROFF:
		    wattroff(w, zca->number);
		    break;
	    }

	    return 0;
	}

    return 1;
}

static short
zcurses_color(char *color)
{
    struct zcurses_namenumberpair *zc;

    static const struct zcurses_namenumberpair zcurses_colors[] = {
	{"black", COLOR_BLACK},
	{"red", COLOR_RED},
	{"green", COLOR_GREEN},
	{"yellow", COLOR_YELLOW},
	{"blue", COLOR_BLUE},
	{"magenta", COLOR_MAGENTA},
	{"cyan", COLOR_CYAN},
	{"white", COLOR_WHITE},
	{NULL, 0}
    };

    for(zc=(struct zcurses_namenumberpair *)zcurses_colors;zc->name;zc++)
	if (!strcmp(color, zc->name)) {
	    return (short)zc->number;
	}

    return (short)-1;
}

static int
zcurses_colorset(WINDOW *w, char *colorpair)
{
    char *fg, *bg, *cp;
    short f, b;
    Colorpairnode cpn;

    if (zc_color_phase==1 ||
	!(cpn = (Colorpairnode) gethashnode(zcurses_colorpairs, colorpair))) {
	zc_color_phase = 2;
	cp = ztrdup(colorpair);
	fg = strtok(cp, "/");
	bg = strtok(NULL, "/");

	if (bg==NULL) {
	    zsfree(cp);
	    return 1;
	}
        
	f = zcurses_color(fg);
	b = zcurses_color(bg);

	zsfree(cp);

	if (f==-1 || b==-1)
	    return 1;

	++next_cp;
	if (next_cp >= COLOR_PAIRS)
	    return 1;

	if (init_pair(next_cp, f, b) == ERR)
	    return 1;

	cpn = (Colorpairnode)zalloc(sizeof(struct colorpairnode));
	
	if (!cpn)
	    return 1;

	cpn->colorpair = next_cp;
	addhashnode(zcurses_colorpairs, ztrdup(colorpair), (void *)cpn);
    }

    return (wcolor_set(w, cpn->colorpair, NULL) == ERR);
}

static void
freecolorpairnode(HashNode hn)
{
    zsfree(hn->nam);
    zfree(hn, sizeof(struct colorpairnode));
}

/**/
static int
bin_zcurses(char *nam, char **args, Options ops, UNUSED(int func))
{
    char **saargs;
    struct zcurses_subcommand *zcsc;
    int sc, num_args;

    struct zcurses_subcommand scs[] = {
	{{"init", ZCURSES_SC_INIT}, 0, 0},
	{{"addwin", ZCURSES_SC_ADDWIN}, 5, 5},
	{{"delwin", ZCURSES_SC_DELWIN}, 1, 1},
	{{"refresh", ZCURSES_SC_REFRESH}, 0, 1},
	{{"move", ZCURSES_SC_MOVE}, 3, 3},
	{{"c", ZCURSES_SC_CHAR}, 2, 2},
	{{"s", ZCURSES_SC_STRING}, 2, 2},
	{{"border", ZCURSES_SC_BORDER}, 1, 5},
	{{"endwin", ZCURSES_SC_ENDWIN}, 1, 1},
	{{"attr", ZCURSES_SC_ATTR}, 2, 2},
	{{"color", ZCURSES_SC_COLOR}, 2, 2},
	{{NULL, -1}, 0, 0}
    };

    for(zcsc = scs; zcsc->nn.name; zcsc++) {
	if(!strcmp(args[0], zcsc->nn.name))
	    break;
    }

    sc = zcsc->nn.number;

    if (sc == -1) {
	zwarnnam(nam, "unknown subcommand: %s", args[0], 0);
	return 1;
    }

    saargs = args;
    while (*saargs++);
    num_args = saargs - (args + 2);

    if (num_args < zcsc->minargs || num_args > zcsc->maxargs)
	return 1;

    saargs = args + 1;

    /* Initialise curses */
    if (sc == ZCURSES_SC_INIT) {
	if (!win_zero) {
	    gettyinfo(&saved_tty_state);
	    win_zero = initscr();
	    if (start_color() != ERR) {
		if(!zc_color_phase)
		    zc_color_phase = 1;
		zcurses_colorpairs = newhashtable(8, "zc_colorpairs", NULL);

		zcurses_colorpairs->hash        = hasher;
		zcurses_colorpairs->emptytable  = emptyhashtable;
		zcurses_colorpairs->filltable   = NULL;
		zcurses_colorpairs->cmpnodes    = strcmp;
		zcurses_colorpairs->addnode     = addhashnode;
		zcurses_colorpairs->getnode     = gethashnode2;
		zcurses_colorpairs->getnode2    = gethashnode2;
		zcurses_colorpairs->removenode  = removehashnode;
		zcurses_colorpairs->disablenode = NULL;
		zcurses_colorpairs->enablenode  = NULL;
		zcurses_colorpairs->freenode    = freecolorpairnode;
		zcurses_colorpairs->printnode   = NULL;

	    }
	    gettyinfo(&curses_tty_state);
	} else {
	    settyinfo(&curses_tty_state);
	}
	return 0;
    } else
    if (sc == ZCURSES_SC_ADDWIN) {
	int nlines, ncols, begin_y, begin_x;
	ZCWin w;

	if (zcurses_validate_window(saargs[0], ZCURSES_UNUSED) == NULL && zc_errno) {
	    zerrnam(nam, "%s: %s", zcurses_strerror(zc_errno), saargs[0], 0);
	    return 1;
	}

	nlines = atoi(saargs[1]);
	ncols = atoi(saargs[2]);
	begin_y = atoi(saargs[3]);
	begin_x = atoi(saargs[4]);

	w = (ZCWin)zshcalloc(sizeof(struct zc_win));
	if (!w)
	    return 1;

	w->name = ztrdup(saargs[0]);
	w->win = newwin(nlines, ncols, begin_y, begin_x);

	if (w->win == NULL) {
	    zsfree(w->name);
	    free(w);
	    return 1;
	}

	zinsertlinknode(zcurses_windows, lastnode(zcurses_windows), (void *)w);

	return 0;
    } else
    if (sc == ZCURSES_SC_DELWIN) {
	LinkNode node;
	ZCWin w;

	node = zcurses_validate_window(saargs[0], ZCURSES_USED);
	if (node == NULL) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), saargs[0], 0);
	    return 1;
	}

	w = (ZCWin)getdata(node);

	if (w == NULL) {
	    zwarnnam(nam, "record for window `%s' is corrupt", saargs[0], 0);
	    return 1;
	}
	if (delwin(w->win)!=OK)
	    return 1;

	if (w->name)
	    zsfree(w->name);

	zfree((ZCWin)remnode(zcurses_windows, node), sizeof(struct zc_win));

	return 0;
    } else
    if (sc == ZCURSES_SC_REFRESH) {
	if (saargs[0]) {
	    LinkNode node;
	    ZCWin w;

	    node = zcurses_validate_window(saargs[0], ZCURSES_USED);
	    if (node == NULL) {
		zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), saargs[0],
			0);
		return 1;
	    }

	    w = (ZCWin)getdata(node);

	    return (wrefresh(w->win)!=OK) ? 1 : 0;
	}
	else
	{
	    return (refresh() != OK) ? 1 : 0;
	}
    } else
    if (sc == ZCURSES_SC_MOVE) {
	int y, x;
	LinkNode node;
	ZCWin w;

	node = zcurses_validate_window(saargs[0], ZCURSES_USED);
	if (node == NULL) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), saargs[0], 0);
	    return 1;
	}

	y = atoi(saargs[1]);
	x = atoi(saargs[2]);

	w = (ZCWin)getdata(node);

	if (wmove(w->win, y, x)!=OK)
	    return 1;

	return 0;
    } else
    if (sc == ZCURSES_SC_CHAR) {
	LinkNode node;
	ZCWin w;
#ifdef HAVE_SETCCHAR
	wchar_t c;
	cchar_t cc;
#endif

	node = zcurses_validate_window(saargs[0], ZCURSES_USED);
	if (node == NULL) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), saargs[0], 0);
	    return 1;
	}

	w = (ZCWin)getdata(node);

#ifdef HAVE_SETCCHAR
	if (mbrtowc(&c, saargs[1], MB_CUR_MAX, NULL) < 1)
	    return 1;

	if (setcchar(&cc, &c, A_NORMAL, 0, NULL)==ERR)
	    return 1;

	if (wadd_wch(w->win, &cc)!=OK)
	    return 1;
#else
	if (waddch(w->win, (chtype)saargs[1][0])!=OK)
	    return 1;
#endif

	return 0;
    } else
    if (sc == ZCURSES_SC_STRING) {
	LinkNode node;
	ZCWin w;

#ifdef HAVE_WADDWSTR
	int clen;
	wint_t wc;
	wchar_t *wstr, *wptr;
	char *str = saargs[1];
#endif

	node = zcurses_validate_window(saargs[0], ZCURSES_USED);
	if (node == NULL) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), args[0], 0);
	    return 1;
	}

	w = (ZCWin)getdata(node);

#ifdef HAVE_WADDWSTR
	mb_metacharinit();
	wptr = wstr = zhalloc((strlen(str)+1) * sizeof(cchar_t));

	while (*str && (clen = mb_metacharlenconv(str, &wc))) {
	    str += clen;
	    if (wc == WEOF) /* TODO: replace with space? nicen? */
		continue;
	    *wptr++ = wc;
	}
	*wptr++ = L'\0';
	if (waddwstr(w->win, wstr)!=OK) {
	    return 1;
	}
#else
	if (waddstr(w->win, saargs[1])!=OK)
	    return 1;
#endif
	return 0;
    } else
    if (sc == ZCURSES_SC_BORDER) {
	LinkNode node;
	ZCWin w;

	node = zcurses_validate_window(saargs[0], ZCURSES_USED);
	if (node == NULL) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), saargs[0], 0);
	    return 1;
	}

	w = (ZCWin)getdata(node);

	if (wborder(w->win, 0, 0, 0, 0, 0, 0, 0, 0)!=OK)
	    return 1;

	return 0;
    } else
    /* Finish using curses */
    if (sc == ZCURSES_SC_ENDWIN) {
	if (win_zero) {
	    endwin();
	    /* Restore TTY as it was before zcurses -i */
	    settyinfo(&saved_tty_state);
	    /*
	     * TODO: should I need the following?  Without it
	     * the screen stays messed up.  Presumably we are
	     * doing stuff with shttyinfo when we shouldn't really be.
	     */
	    gettyinfo(&shttyinfo);
	}
	return 0;
    } else
    if (sc == ZCURSES_SC_ATTR) {
	LinkNode node;
	ZCWin w;
	char **attrs;

	if (!saargs[0])
	    return 1;

	node = zcurses_validate_window(saargs[0], ZCURSES_USED);
	if (node == NULL) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), saargs[0], 0);
	    return 1;
	}

	w = (ZCWin)getdata(node);

	for(attrs = saargs+1; *attrs; attrs++) {
	    switch(*attrs[0]) {
		case '-':
		    zcurses_attribute(w->win, (*attrs)+1, ZCURSES_ATTROFF);
		    break;
		case '+':
		    zcurses_attribute(w->win, (*attrs)+1, ZCURSES_ATTRON);
		    break;
		default:
		    /* maybe a bad idea to spew warnings here */
		    break;
	    }
	}
	return 0;
    } else
    if (sc == ZCURSES_SC_COLOR) {
	LinkNode node;
	ZCWin w;

	if (!saargs[0] || !saargs[1] || !zc_color_phase)
	    return 1;

	node = zcurses_validate_window(saargs[0], ZCURSES_USED);
	if (node == NULL) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), saargs[0], 0);
	    return 1;
	}

	w = (ZCWin)getdata(node);

	return zcurses_colorset(w->win, saargs[1]);
    }

    return 0;
}

/*
 * boot_ is executed when the module is loaded.
 */

static struct builtin bintab[] = {
    BUILTIN("zcurses", 0, bin_zcurses, 1, 6, 0, "", NULL),
};

static struct features module_features = {
    bintab, sizeof(bintab)/sizeof(*bintab),
    NULL, 0,
    NULL, 0,
    NULL, 0,
    0
};

/**/
int
setup_(UNUSED(Module m))
{
    return 0;
}

/**/
int
features_(Module m, char ***features)
{
    *features = featuresarray(m, &module_features);
    return 0;
}

/**/
int
enables_(Module m, int **enables)
{
    return handlefeatures(m, &module_features, enables);
}

/**/
int
boot_(Module m)
{
    zcurses_windows = znewlinklist();

    return 0;
}

/**/
int
cleanup_(Module m)
{
    freelinklist(zcurses_windows, (FreeFunc) zcurses_free_window);
    if (zcurses_colorpairs)
	deletehashtable(zcurses_colorpairs);
    return setfeatureenables(m, &module_features, NULL);
}

/**/
int
finish_(UNUSED(Module m))
{
    return 0;
}
