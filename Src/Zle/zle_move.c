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
	if (cs == 0)
	    return 0;
	if (line[cs - 1] == '\n')
	    if (!--cs)
		return 0;
	while (cs && line[cs - 1] != '\n')
	    cs--;
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
	if (cs >= ll) {
	    cs = ll;
	    return 0;
	}
	if (line[cs] == '\n')
	    if (++cs == ll)
		return 0;
	while (cs != ll && line[cs] != '\n')
	    cs++;
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
	if (cs == 0)
	    break;
	if (line[cs - 1] == '\n')
	    if (!--cs)
		break;
	while (cs && line[cs - 1] != '\n')
	    cs--;
	n--;
    }
    if (n) {
	int m = zmult, ret;

	zmult = n;
	ret = uphistory(args);
	zmult = m;
	cs = 0;
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
	if (cs >= ll) {
	    cs = ll;
	    break;
	}
	if (line[cs] == '\n')
	    if (++cs == ll)
		break;
	while (cs != ll && line[cs] != '\n')
	    cs++;
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
    cs += zmult;
    if (cs > ll)
	cs = ll;
    if (cs < 0)
	cs = 0;
    return 0;
}

/**/
int
backwardchar(UNUSED(char **args))
{
    cs -= zmult;
    if (cs > ll)
	cs = ll;
    if (cs < 0)
	cs = 0;
    return 0;
}

/**/
int
setmarkcommand(UNUSED(char **args))
{
    mark = cs;
    return 0;
}

/**/
int
exchangepointandmark(UNUSED(char **args))
{
    int x;

    x = mark;
    mark = cs;
    cs = x;
    if (cs > ll)
	cs = ll;
    return 0;
}

/**/
int
vigotocolumn(UNUSED(char **args))
{
    int x, y;

    findline(&x, &y);
    if (zmult >= 0)
	cs = x + zmult - (zmult > 0);
    else
	cs = y + zmult;
    if (cs > y)
	cs = y;
    if (cs < x)
	cs = x;
    return 0;
}

/**/
int
vimatchbracket(UNUSED(char **args))
{
    int ocs = cs, dir, ct;
    unsigned char oth, me;

  otog:
    if (cs == ll || line[cs] == '\n') {
	cs = ocs;
	return 1;
    }
    switch (me = line[cs]) {
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
	cs++;
	goto otog;
    }
    ct = 1;
    while (cs >= 0 && cs < ll && ct) {
	cs += dir;
	if (line[cs] == oth)
	    ct--;
	else if (line[cs] == me)
	    ct++;
    }
    if (cs < 0 || cs >= ll) {
	cs = ocs;
	return 1;
    } else if(dir > 0 && virangeflag)
	cs++;
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
    if (cs >= lim)
	return 1;
    while (n-- && cs < lim)
	cs++;
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
    if (cs == findbol())
	return 1;
    while (n--) {
	cs--;
	if (cs < 0 || line[cs] == '\n') {
	    cs++;
	    break;
	}
    }
    return 0;
}

/**/
int
viendofline(UNUSED(char **args))
{
    int oldcs = cs, n = zmult;

    if (n < 1)
	return 1;
    while(n--) {
	if (cs > ll) {
	    cs = oldcs;
	    return 1;
	}
	cs = findeol() + 1;
    }
    cs--;
    lastcol = 1<<30;
    return 0;
}

/**/
int
vibeginningofline(UNUSED(char **args))
{
    cs = findbol();
    return 0;
}

static int vfindchar, vfinddir, tailadd;

/**/
int
vifindnextchar(char **args)
{
    if ((vfindchar = vigetkey()) != -1) {
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
    if ((vfindchar = vigetkey()) != -1) {
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
    if ((vfindchar = vigetkey()) != -1) {
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
    if ((vfindchar = vigetkey()) != -1) {
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
    int ocs = cs, n = zmult;

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
	do
	    cs += vfinddir;
	while (cs >= 0 && cs < ll && line[cs] != vfindchar && line[cs] != '\n');
	if (cs < 0 || cs >= ll || line[cs] == '\n') {
	    cs = ocs;
	    return 1;
	}
    }
    cs += tailadd;
    if (vfinddir == 1 && virangeflag)
	cs++;
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
    cs = findbol();
    while (cs != ll && iblank(line[cs]))
	cs++;
    return 0;
}

/**/
int
visetmark(UNUSED(char **args))
{
    int ch;

    ch = getkey(0);
    if (ch < 'a' || ch > 'z')
	return 1;
    ch -= 'a';
    vimarkcs[ch] = cs;
    vimarkline[ch] = histline;
    return 0;
}

/**/
int
vigotomark(UNUSED(char **args))
{
    int ch;

    ch = getkey(0);
    if (ch == lastchar)
	ch = 26;
    else {
	if (ch < 'a' || ch > 'z')
	    return 1;
	ch -= 'a';
    }
    if (!vimarkline[ch])
	return 1;
    if (curhist != vimarkline[ch] && !zle_goto_hist(vimarkline[ch], 0, 0)) {
	vimarkline[ch] = 0;
	return 1;
    }
    cs = vimarkcs[ch];
    if (cs > ll)
	cs = ll;
    return 0;
}

/**/
int
vigotomarkline(char **args)
{
    vigotomark(args);
    return vifirstnonblank(zlenoargs);
}
