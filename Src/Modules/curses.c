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

#define ZCURSES_MAX_WINDOWS 9

static WINDOW *zcurses_WIN[ZCURSES_MAX_WINDOWS + 1];

#define ZCURSES_ERANGE 1
#define ZCURSES_EDEFINED 2
#define ZCURSES_EUNDEFINED 3

#define ZCURSES_UNUSED 1
#define ZCURSES_USED 2

static int zc_errno;

/**/
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

/**/
static unsigned
zcurses_validate_window(char *win, int criteria)
{
    unsigned target;

    if (win==NULL) {
	zc_errno = ZCURSES_ERANGE;
	return -1;
    }

    target = (unsigned)atoi(win);

    if (target > ZCURSES_MAX_WINDOWS) {
	zc_errno = ZCURSES_ERANGE;
	return -1;
    }

    if (zcurses_WIN[target]!=NULL && (criteria & ZCURSES_UNUSED)) {
	zc_errno = ZCURSES_EDEFINED;
	return -1;
    }

    if (zcurses_WIN[target]==NULL && (criteria & ZCURSES_USED)) {
	zc_errno = ZCURSES_EUNDEFINED;
	return -1;
    }

    return target;
}

/**/
static int
bin_zcurses(char *nam, char **args, Options ops, UNUSED(int func))
{
    unsigned targetwin;

    if (OPT_ISSET(ops,'a')) {
	int nlines, ncols, begin_y, begin_x;

	nlines = atoi(args[0]);
	ncols = atoi(args[1]);
	begin_y = atoi(args[2]);
	begin_x = atoi(args[3]);
	targetwin = zcurses_validate_window(args[4], ZCURSES_UNUSED);

	if (targetwin == -1) {
	    zerrnam(nam, "%s: %s", zcurses_strerror(zc_errno), args[4], 0);
	    return 1;
	}

	zcurses_WIN[targetwin]=newwin(nlines, ncols, begin_y, begin_x);

	if (zcurses_WIN[targetwin]==NULL)
	    return 1;

	return 0;
    }

    if (OPT_ISSET(ops,'d')) {
	targetwin = zcurses_validate_window(OPT_ARG(ops,'d'), ZCURSES_USED);
	if (targetwin == -1) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), OPT_ARG(ops,'d'), 0);
	    return 1;
	}

	if (delwin(zcurses_WIN[targetwin])!=OK)
		return 1;

	zcurses_WIN[targetwin]=NULL;
	return 0;
    }

    if (OPT_ISSET(ops,'r')) {
	targetwin = zcurses_validate_window(OPT_ARG(ops,'r'), ZCURSES_USED);
	if (targetwin == -1) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), OPT_ARG(ops,'r'), 0);
	    return 1;
	}

	return (wrefresh(zcurses_WIN[targetwin])!=OK) ? 1 : 0;
    }

    if (OPT_ISSET(ops,'m')) {
	int y, x;

	targetwin = zcurses_validate_window(args[0], ZCURSES_USED);
	if (targetwin == -1) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), args[0], 0);
	    return 1;
	}

	y = atoi(args[1]);
	x = atoi(args[2]);

	if (wmove(zcurses_WIN[targetwin], y, x)!=OK)
	    return 1;

	return 0;
    }

    if (OPT_ISSET(ops,'c')) {
#ifdef HAVE_SETCCHAR
	wchar_t c;
	cchar_t cc;
#endif

	targetwin = zcurses_validate_window(args[0], ZCURSES_USED);
	if (targetwin == -1) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), args[0], 0);
	    return 1;
	}

#ifdef HAVE_SETCCHAR
	if (mbrtowc(&c, args[1], MB_CUR_MAX, NULL) < 1)
	    return 1;

	if (setcchar(&cc, &c, A_NORMAL, 0, NULL)==ERR)
	    return 1;

	if (wadd_wch(zcurses_WIN[targetwin], &cc)!=OK)
	    return 1;
#else
	if (waddch(zcurses_WIN[targetwin], (chtype)args[1][0])!=OK)
	    return 1;
#endif

	return 0;
    }

    if (OPT_ISSET(ops,'s')) {
#ifdef HAVE_SETCCHAR
	wchar_t *ws;
	cchar_t *wcc;
	size_t sl;
#endif

	targetwin = zcurses_validate_window(args[0], ZCURSES_USED);
	if (targetwin == -1) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), args[0], 0);
	    return 1;
	}

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

	if (wadd_wchstr(zcurses_WIN[targetwin], wcc)!=OK) {
	    free(wcc);
	    return 1;
	}

	free(wcc);
#else
	if (waddstr(zcurses_WIN[targetwin], args[1])!=OK)
	    return 1;
#endif
	return 0;
    }

    if (OPT_ISSET(ops,'b')) {

	targetwin = zcurses_validate_window(OPT_ARG(ops,'b'), ZCURSES_USED);
	if (targetwin == -1) {
	    zwarnnam(nam, "%s: %s", zcurses_strerror(zc_errno), OPT_ARG(ops,'b'), 0);
	    return 1;
	}

	if (wborder(zcurses_WIN[targetwin], 0, 0, 0, 0, 0, 0, 0, 0)!=OK)
	    return 1;

	return 0;
    }

    return 0;
}

/*
 * boot_ is executed when the module is loaded.
 */

static struct builtin bintab[] = {
    BUILTIN("zcurses", 0, bin_zcurses, 0, 5, 0, "ab:cd:mr:rs", NULL),
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
    int i;
    for(i=1;i<=ZCURSES_MAX_WINDOWS;i++)
	zcurses_WIN[i]=NULL;

    zcurses_WIN[0]=initscr();

    return 0;
}

/**/
int
cleanup_(Module m)
{
    return setfeatureenables(m, &module_features, NULL);
}

/**/
int
finish_(UNUSED(Module m))
{
    return 0;
}
