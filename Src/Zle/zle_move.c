/*
 * zle_move.c - editor movement
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
#include "zle_move.pro"

static int vimarkcs[27], vimarkline[27];

/**/
int
beginningofline(char **args)
{
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = endofline(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	if (zlecs == 0)
	    return 0;
	if (zleline[zlecs - 1] == '\n')
	    if (!--zlecs)
		return 0;
	while (zlecs && zleline[zlecs - 1] != '\n')
	    zlecs--;
    }
    return 0;
}

/**/
int
endofline(char **args)
{
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = beginningofline(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	if (zlecs >= zlell) {
	    zlecs = zlell;
	    return 0;
	}
	if (zleline[zlecs] == '\n')
	    if (++zlecs == zlell)
		return 0;
	while (zlecs != zlell && zleline[zlecs] != '\n')
	    zlecs++;
    }
    return 0;
}

/**/
int
beginningoflinehist(char **args)
{
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = endoflinehist(args);
	zmult = n;
	return ret;
    }
    while (n) {
	if (zlecs == 0)
	    break;
	if (zleline[zlecs - 1] == '\n')
	    if (!--zlecs)
		break;
	while (zlecs && zleline[zlecs - 1] != '\n')
	    zlecs--;
	n--;
    }
    if (n) {
	int m = zmult, ret;

	zmult = n;
	ret = uphistory(args);
	zmult = m;
	zlecs = 0;
	return ret;
    }
    return 0;
}

/**/
int
endoflinehist(char **args)
{
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = beginningoflinehist(args);
	zmult = n;
	return ret;
    }
    while (n) {
	if (zlecs >= zlell) {
	    zlecs = zlell;
	    break;
	}
	if (zleline[zlecs] == '\n')
	    if (++zlecs == zlell)
		break;
	while (zlecs != zlell && zleline[zlecs] != '\n')
	    zlecs++;
	n--;
    }
    if (n) {
	int m = zmult, ret;

	zmult = n;
	ret = downhistory(args);
	zmult = m;
	return ret;
    }
    return 0;
}

/**/
int
forwardchar(UNUSED(char **args))
{
    zlecs += zmult;
    if (zlecs > zlell)
	zlecs = zlell;
    if (zlecs < 0)
	zlecs = 0;
    return 0;
}

/**/
int
backwardchar(UNUSED(char **args))
{
    zlecs -= zmult;
    if (zlecs > zlell)
	zlecs = zlell;
    if (zlecs < 0)
	zlecs = 0;
    return 0;
}

/**/
int
setmarkcommand(UNUSED(char **args))
{
    mark = zlecs;
    return 0;
}

/**/
int
exchangepointandmark(UNUSED(char **args))
{
    int x;

    x = mark;
    mark = zlecs;
    zlecs = x;
    if (zlecs > zlell)
	zlecs = zlell;
    return 0;
}

/**/
int
vigotocolumn(UNUSED(char **args))
{
    int x, y;

    findline(&x, &y);
    if (zmult >= 0)
	zlecs = x + zmult - (zmult > 0);
    else
	zlecs = y + zmult;
    if (zlecs > y)
	zlecs = y;
    if (zlecs < x)
	zlecs = x;
    return 0;
}

/**/
int
vimatchbracket(UNUSED(char **args))
{
    int ocs = zlecs, dir, ct;
    unsigned char oth, me;

  otog:
    if (zlecs == zlell || zleline[zlecs] == '\n') {
	zlecs = ocs;
	return 1;
    }
    switch (me = zleline[zlecs]) {
    case '{':
	dir = 1;
	oth = '}';
	break;
    case /*{*/ '}':
	virangeflag = -virangeflag;
	dir = -1;
	oth = '{'; /*}*/
	break;
    case '(':
	dir = 1;
	oth = ')';
	break;
    case ')':
	virangeflag = -virangeflag;
	dir = -1;
	oth = '(';
	break;
    case '[':
	dir = 1;
	oth = ']';
	break;
    case ']':
	virangeflag = -virangeflag;
	dir = -1;
	oth = '[';
	break;
    default:
	zlecs++;
	goto otog;
    }
    ct = 1;
    while (zlecs >= 0 && zlecs < zlell && ct) {
	zlecs += dir;
	if (zleline[zlecs] == oth)
	    ct--;
	else if (zleline[zlecs] == me)
	    ct++;
    }
    if (zlecs < 0 || zlecs >= zlell) {
	zlecs = ocs;
	return 1;
    } else if(dir > 0 && virangeflag)
	zlecs++;
    return 0;
}

/**/
int
viforwardchar(char **args)
{
    int lim = findeol() - invicmdmode();
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = vibackwardchar(args);
	zmult = n;
	return ret;
    }
    if (zlecs >= lim)
	return 1;
    while (n-- && zlecs < lim)
	zlecs++;
    return 0;
}

/**/
int
vibackwardchar(char **args)
{
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = viforwardchar(args);
	zmult = n;
	return ret;
    }
    if (zlecs == findbol())
	return 1;
    while (n--) {
	zlecs--;
	if (zlecs < 0 || zleline[zlecs] == '\n') {
	    zlecs++;
	    break;
	}
    }
    return 0;
}

/**/
int
viendofline(UNUSED(char **args))
{
    int oldcs = zlecs, n = zmult;

    if (n < 1)
	return 1;
    while(n--) {
	if (zlecs > zlell) {
	    zlecs = oldcs;
	    return 1;
	}
	zlecs = findeol() + 1;
    }
    zlecs--;
    lastcol = 1<<30;
    return 0;
}

/**/
int
vibeginningofline(UNUSED(char **args))
{
    zlecs = findbol();
    return 0;
}

static ZLE_INT_T vfindchar;
static int vfinddir, tailadd;

/**/
int
vifindnextchar(char **args)
{
    if ((vfindchar = vigetkey()) != ZLEEOF) {
	vfinddir = 1;
	tailadd = 0;
	return virepeatfind(args);
    }
    return 1;
}

/**/
int
vifindprevchar(char **args)
{
    if ((vfindchar = vigetkey()) != ZLEEOF) {
	vfinddir = -1;
	tailadd = 0;
	return virepeatfind(args);
    }
    return 1;
}

/**/
int
vifindnextcharskip(char **args)
{
    if ((vfindchar = vigetkey()) != ZLEEOF) {
	vfinddir = 1;
	tailadd = -1;
	return virepeatfind(args);
    }
    return 1;
}

/**/
int
vifindprevcharskip(char **args)
{
    if ((vfindchar = vigetkey()) != ZLEEOF) {
	vfinddir = -1;
	tailadd = 1;
	return virepeatfind(args);
    }
    return 1;
}

/**/
int
virepeatfind(char **args)
{
    int ocs = zlecs, n = zmult;

    if (!vfinddir)
	return 1;
    if (n < 0) {
	int ret;
	zmult = -n;
	ret = virevrepeatfind(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	do {
	    zlecs += vfinddir;
	} while (zlecs >= 0 && zlecs < zlell
	    && (ZLE_INT_T)zleline[zlecs] != vfindchar
	    && zleline[zlecs] != ZWC('\n'));
	if (zlecs < 0 || zlecs >= zlell || zleline[zlecs] == ZWC('\n')) {
	    zlecs = ocs;
	    return 1;
	}
    }
    zlecs += tailadd;
    if (vfinddir == 1 && virangeflag)
	zlecs++;
    return 0;
}

/**/
int
virevrepeatfind(char **args)
{
    int ret;

    if (zmult < 0) {
	zmult = -zmult;
	ret = virepeatfind(args);
	zmult = -zmult;
	return ret;
    }
    vfinddir = -vfinddir;
    ret = virepeatfind(args);
    vfinddir = -vfinddir;
    return ret;
}

/**/
int
vifirstnonblank(UNUSED(char **args))
{
    zlecs = findbol();
    while (zlecs != zlell && ZC_iblank(zleline[zlecs]))
	zlecs++;
    return 0;
}

/**/
int
visetmark(UNUSED(char **args))
{
    ZLE_INT_T ch;

    ch = getfullchar(0);
    if (ch < ZWC('a') || ch > ZWC('z'))
	return 1;
    ch -= ZWC('a');
    vimarkcs[ch] = zlecs;
    vimarkline[ch] = histline;
    return 0;
}

/**/
int
vigotomark(UNUSED(char **args))
{
    ZLE_INT_T ch;
    LASTFULLCHAR_T lfc = LASTFULLCHAR;

    ch = getfullchar(0);
    if (ch == lfc)
	ch = 26;
    else {
	if (ch < ZWC('a') || ch > ZWC('z'))
	    return 1;
	ch -= ZWC('a');
    }
    if (!vimarkline[ch])
	return 1;
    if (curhist != vimarkline[ch] && !zle_goto_hist(vimarkline[ch], 0, 0)) {
	vimarkline[ch] = 0;
	return 1;
    }
    zlecs = vimarkcs[ch];
    if (zlecs > zlell)
	zlecs = zlell;
    return 0;
}

/**/
int
vigotomarkline(char **args)
{
    vigotomark(args);
    return vifirstnonblank(zlenoargs);
}
