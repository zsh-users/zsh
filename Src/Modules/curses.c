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
#include <wchar.h>

#include <stdio.h>

#include "curses.mdh"
#include "curses.pro"

static char *nam="curses";

#define ZCURSES_MAX_WINDOWS 9

static WINDOW *zcurses_WIN[ZCURSES_MAX_WINDOWS + 1];

/**/
static int
bin_zcurses_newwin(char *nam, char **args, Options ops, UNUSED(int func))
{
    int nlines, ncols, begin_y, begin_x;
    unsigned winnum;

    nlines = atoi(args[0]);
    ncols = atoi(args[1]);
    begin_y = atoi(args[2]);
    begin_x = atoi(args[3]);
    winnum = atoi(args[4]);

    if(winnum > ZCURSES_MAX_WINDOWS) {
        zerrnam(nam, "bad window number: %s", args[4], 0);
        return 1;
    }

    if(zcurses_WIN[winnum]!=NULL) {
        zwarnnam(nam, "window number %s already defined", args[4], 0);
        return 1;
    }

    zcurses_WIN[winnum]=newwin(nlines, ncols, begin_y, begin_x);

    if(zcurses_WIN[winnum]==NULL)
	return 1;

    return 0;
}

/**/
static int
bin_zcurses_delwin(char *nam, char **args, Options ops, UNUSED(int func))
{
    unsigned winnum;

    winnum = atoi(args[0]);

    if(winnum > ZCURSES_MAX_WINDOWS) {
        zerrnam(nam, "bad window number: %s", args[0], 0);
        return 1;
    }

    if(zcurses_WIN[winnum]==NULL) {
        zwarnnam(nam, "window number %s does not exist", args[0], 0);
        return 1;
    }

    if(delwin(zcurses_WIN[winnum])!=OK)
	return 1;

    zcurses_WIN[winnum]=NULL;
    return 0;
}

/**/
static int
bin_zcurses_wrefresh(char *nam, char **args, Options ops, UNUSED(int func))
{
    unsigned winnum;

    winnum = atoi(args[0]);

    if(winnum > ZCURSES_MAX_WINDOWS) {
        zerrnam(nam, "bad window number: %s", args[0], 0);
        return 1;
    }

    if(zcurses_WIN[winnum]==NULL) {
        zwarnnam(nam, "window number %s does not exist", args[0], 0);
        return 1;
    }

    if(wrefresh(zcurses_WIN[winnum])!=OK)
	return 1;

    return 0;
}

/**/
static int
bin_zcurses_wmove(char *nam, char **args, Options ops, UNUSED(int func))
{
    int y, x;
    unsigned winnum;

    winnum = atoi(args[0]);
    y = atoi(args[1]);
    x = atoi(args[2]);

    if(winnum > ZCURSES_MAX_WINDOWS) {
        zerrnam(nam, "bad window number: %s", args[0], 0);
        return 1;
    }

    if(zcurses_WIN[winnum]==NULL) {
        zwarnnam(nam, "window number %s is not defined", args[0], 0);
        return 1;
    }

    if(wmove(zcurses_WIN[winnum], y, x)!=OK)
	return 1;

    return 0;
}

/**/
static int
bin_zcurses_wadd_wch(char *nam, char **args, Options ops, UNUSED(int func))
{
    unsigned winnum;
    wchar_t c;
    cchar_t cc;

    winnum = atoi(args[0]);

    if(mbrtowc(&c, args[1], MB_CUR_MAX, NULL) < 1)
	return 1;

    if (setcchar(&cc, &c, A_NORMAL, 0, NULL)==ERR)
	return 1;

    if(winnum > ZCURSES_MAX_WINDOWS) {
        zerrnam(nam, "bad window number: %s", args[0], 0);
        return 1;
    }

    if(zcurses_WIN[winnum]==NULL) {
        zwarnnam(nam, "window number %s is not defined", args[0], 0);
        return 1;
    }

    if(wadd_wch(zcurses_WIN[winnum], &cc)!=OK)
	return 1;

    return 0;
}

/**/
static int
bin_zcurses_wadd_wchstr(char *nam, char **args, Options ops, UNUSED(int func))
{
    unsigned winnum;
    wchar_t *ws;
    cchar_t *wcc;
    size_t sl;

    winnum = atoi(args[0]);

    if(winnum > ZCURSES_MAX_WINDOWS) {
        zerrnam(nam, "bad window number: %s", args[0], 0);
        return 1;
    }

    if(zcurses_WIN[winnum]==NULL) {
        zwarnnam(nam, "window number %s is not defined", args[0], 0);
        return 1;
    }

    sl = strlen(args[1]);

    if(sl == 0) {
        return 0;
    }

    ws = malloc(sl * sizeof(wchar_t));

    if(mbstowcs(ws, args[1], sl) < 1) {
	free(ws);
	return 1;
    }

    wcc = malloc(wcslen(ws) * sizeof(cchar_t));

    if (setcchar(wcc, ws, A_NORMAL, 0, NULL)==ERR) {
	return 1;
    }

    free(ws);

    if(wadd_wchstr(zcurses_WIN[winnum], wcc)!=OK) {
        free(wcc);
	return 1;
    }

    free(wcc);
    return 0;
}

/**/
static int
bin_zcurses_wborder(char *nam, char **args, Options ops, UNUSED(int func))
{
    unsigned winnum;

    winnum = atoi(args[0]);

    if(winnum > ZCURSES_MAX_WINDOWS) {
        zerrnam(nam, "bad window number: %s", args[0], 0);
        return 1;
    }

    if(zcurses_WIN[winnum]==NULL) {
        zwarnnam(nam, "window number %s does not exist", args[0], 0);
        return 1;
    }

    if(wborder(zcurses_WIN[winnum], 0, 0, 0, 0, 0, 0, 0, 0)!=OK)
	return 1;

    return 0;
}



/*
 * boot_ is executed when the module is loaded.
 */

static struct builtin bintab[] = {
    BUILTIN("zcurses_newwin", 0, bin_zcurses_newwin, 5, 5, 0, NULL, NULL),
    BUILTIN("zcurses_delwin", 0, bin_zcurses_delwin, 1, 1, 0, NULL, NULL),
    BUILTIN("zcurses_wrefresh", 0, bin_zcurses_wrefresh, 1, 1, 0, NULL, NULL),
    BUILTIN("zcurses_wmove", 0, bin_zcurses_wmove, 3, 3, 0, NULL, NULL),
    BUILTIN("zcurses_wadd_wch", 0, bin_zcurses_wadd_wch, 2, 2, 0, NULL, NULL),
    BUILTIN("zcurses_wadd_wchstr", 0, bin_zcurses_wadd_wchstr, 2, 2, 0, NULL, NULL),
    BUILTIN("zcurses_wborder", 0, bin_zcurses_wborder, 1, 1, 0, NULL, NULL),
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
