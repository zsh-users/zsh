/*
 * deltochar.c - ZLE module implementing Emacs' zap-to-char
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 1996-1997 Peter Stephenson
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Peter Stephenson or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Peter Stephenson and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Peter Stephenson and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Peter Stephenson and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "deltochar.mdh"
#include "deltochar.pro"

static Widget w_deletetochar;
static Widget w_zaptochar;

/**/
static int
deltochar(char **args)
{
    int c = getkey(0), dest = cs, ok = 0, n = zmult;
    int zap = (bindk->widget == w_zaptochar);

    if (n > 0) {
	while (n-- && dest != ll) {
	    while (dest != ll && line[dest] != c)
		dest++;
	    if (dest != ll) {
		if (!zap || n > 0)
		    dest++;
		if (!n) {
		    forekill(dest - cs, 0);
		    ok++;
		}
	    }
	}
    } else {
	/* ignore character cursor is on when scanning backwards */
	if (dest)
	    dest--;
	while (n++ && dest != 0) {
	    while (dest != 0 && line[dest] != c)
		dest--;
	    if (line[dest] == c) {
		if (!n) {
		    backkill(cs - dest - zap, 1);
		    ok++;
		}
		if (dest)
		    dest--;
	    }
	}
    }
    return !ok;
}

/**/
int
setup_(Module m)
{
    return 0;
}

/**/
int
boot_(Module m)
{
    w_deletetochar = addzlefunction("delete-to-char", deltochar,
                                    ZLE_KILL | ZLE_KEEPSUFFIX);
    if (w_deletetochar) {
	w_zaptochar = addzlefunction("zap-to-char", deltochar,
				     ZLE_KILL | ZLE_KEEPSUFFIX);
	if (w_zaptochar)
	    return 0;
	deletezlefunction(w_deletetochar);
    }
    zwarnnam(m->nam, "deltochar: name clash when adding ZLE functions",
	     NULL, 0);
    return -1;
}

/**/
int
cleanup_(Module m)
{
    deletezlefunction(w_deletetochar);
    deletezlefunction(w_zaptochar);
    return 0;
}

/**/
int
finish_(Module m)
{
    return 0;
}
