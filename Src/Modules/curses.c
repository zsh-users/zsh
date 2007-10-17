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

struct zcurses_attribute {
    char *name;
    int number;
};

static WINDOW *win_zero;
static struct ttyinfo saved_tty_state;
static struct ttyinfo curses_tty_state;
static LinkList zcurses_windows;

#define ZCURSES_ERANGE 1
#define ZCURSES_EDEFINED 2
#define ZCURSES_EUNDEFINED 3

#define ZCURSES_UNUSED 1
#define ZCURSES_USED 2

#define ZCURSES_ATTRON 1
#define ZCURSES_ATTROFF 2

static int zc_errno;

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
    struct zcurses_attribute *zca;

    static const struct zcurses_attribute zcurses_attributes[] = {
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

    for(zca=(struct zcurses_attribute *)zcurses_attributes;zca->name;zca++)
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

/**/
static int
bin_zcurses(char *nam, char **args, Options ops, UNUSED(int func))
{
    /* Initialise curses */
    if (OPT_ISSET(ops,'i')) {
	if (!win_zero) {
	    gettyinfo(&saved_tty_state);
	    win_zero = initscr();
	    gettyinfo(&curses_tty_state);
	} else {
	    settyinfo(&curses_tty_state);
	}
	return 0;
    }

    if (OPT_ISSET(ops,'a')) {
	int nlines, ncols, begin_y, begin_x;
	ZCWin w;

	if (zcurses_validate_window(args[0], ZCURSES_UNUSED) == NULL && zc_errno) {
	    zerrnam(nam, "%s: %s", zcurses_strerror(zc_errno), args[0], 0);
	    return 1;
	}

	nlines = atoi(args[1]);
	ncols = atoi(args[2]);
	begin_y = atoi(args[3]);
	begin_x = atoi(args[4]);

	w = (ZCWin)zshcalloc(sizeof(struct zc_win));
	if (!w)
	    return 1;

	w->name = ztrdup(args[0]);
	w->win = newwin(nlines, ncols, begin_y, begin_x);

	if (w->win == NULL) {
	    zsfree(w->name);
	    free(w);
	    return 1;
	}

	zinsertlinknode(zcurses_windows, lastnode(zcurses_windows), (void *)w);

	return 0;
    }

    if (OPT_ISSET(ops,'d')) {
	LinkNode node;
	ZCWin w;

	node = zcurses_validate_window(OPT_ARG(ops,'d'), ZCURSES_USED);
	if (node == NULL) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), OPT_ARG(ops,'d'), 0);
	    return 1;
	}

	w = (ZCWin)getdata(node);

	if (w == NULL) {
	    zwarnnam(nam, "record for window `%s' is corrupt", OPT_ARG(ops, 'd'), 0);
	    return 1;
	}
	if (delwin(w->win)!=OK)
	    return 1;

	if (w->name)
	    zsfree(w->name);

	zfree((ZCWin)remnode(zcurses_windows, node), sizeof(struct zc_win));

	return 0;
    }

    if (OPT_ISSET(ops,'r')) {
	if (args[0]) {
	    LinkNode node;
	    ZCWin w;

	    node = zcurses_validate_window(args[0], ZCURSES_USED);
	    if (node == NULL) {
		zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), args[0],
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
    }

    if (OPT_ISSET(ops,'m')) {
	int y, x;
	LinkNode node;
	ZCWin w;

	node = zcurses_validate_window(args[0], ZCURSES_USED);
	if (node == NULL) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), args[0], 0);
	    return 1;
	}

	y = atoi(args[1]);
	x = atoi(args[2]);

	w = (ZCWin)getdata(node);

	if (wmove(w->win, y, x)!=OK)
	    return 1;

	return 0;
    }

    if (OPT_ISSET(ops,'c')) {
	LinkNode node;
	ZCWin w;
#ifdef HAVE_SETCCHAR
	wchar_t c;
	cchar_t cc;
#endif

	node = zcurses_validate_window(args[0], ZCURSES_USED);
	if (node == NULL) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), args[0], 0);
	    return 1;
	}

	w = (ZCWin)getdata(node);

#ifdef HAVE_SETCCHAR
	if (mbrtowc(&c, args[1], MB_CUR_MAX, NULL) < 1)
	    return 1;

	if (setcchar(&cc, &c, A_NORMAL, 0, NULL)==ERR)
	    return 1;

	if (wadd_wch(w->win, &cc)!=OK)
	    return 1;
#else
	if (waddch(w->win, (chtype)args[1][0])!=OK)
	    return 1;
#endif

	return 0;
    }

    if (OPT_ISSET(ops,'s')) {
	LinkNode node;
	ZCWin w;

#ifdef HAVE_SETCCHAR
	wchar_t *ws;
	cchar_t *wcc;
	size_t sl;
#endif

	node = zcurses_validate_window(args[0], ZCURSES_USED);
	if (node == NULL) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), args[0], 0);
	    return 1;
	}

	w = (ZCWin)getdata(node);

#ifdef HAVE_SETCCHAR
	sl = strlen(args[1]);

	if (sl == 0) {
	    return 0;
	}

	ws = malloc(sl * sizeof(wchar_t));

	if (mbstowcs(ws, args[1], sl) < 1) {
	    free(ws);
	    return 1;
	}

	wcc = malloc(wcslen(ws) * sizeof(cchar_t));

	if (setcchar(wcc, ws, A_NORMAL, 0, NULL)==ERR) {
	    return 1;
	}

	free(ws);

	if (wadd_wchstr(w->win, wcc)!=OK) {
	    free(wcc);
	    return 1;
	}

	free(wcc);
#else
	if (waddstr(w->win, args[1])!=OK)
	    return 1;
#endif
	return 0;
    }

    if (OPT_ISSET(ops,'b')) {
	LinkNode node;
	ZCWin w;

	node = zcurses_validate_window(OPT_ARG(ops,'b'), ZCURSES_USED);
	if (node == NULL) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), OPT_ARG(ops,'b'), 0);
	    return 1;
	}

	w = (ZCWin)getdata(node);

	if (wborder(w->win, 0, 0, 0, 0, 0, 0, 0, 0)!=OK)
	    return 1;

	return 0;
    }

    /* Finish using curses */
    if (OPT_ISSET(ops,'e')) {
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
    }
    if (OPT_ISSET(ops,'A')) {
	LinkNode node;
	ZCWin w;
	char **attrs;

	if (!args[0])
	    return 1;

	node = zcurses_validate_window(args[0], ZCURSES_USED);
	if (node == NULL) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), args[0], 0);
	    return 1;
	}

	w = (ZCWin)getdata(node);

	for(attrs = args+1; *attrs; attrs++) {
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
    }

    return 0;
}

/*
 * boot_ is executed when the module is loaded.
 */

static struct builtin bintab[] = {
    BUILTIN("zcurses", 0, bin_zcurses, 0, 5, 0, "Aab:cd:eimrs", NULL),
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
    return setfeatureenables(m, &module_features, NULL);
}

/**/
int
finish_(UNUSED(Module m))
{
    return 0;
}
