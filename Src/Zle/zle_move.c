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

static vimarkcs[27], vimarkline[27];

/**/
void
beginningofline(void)
{
    int n = zmult;

    if (n < 0) {
	zmult = -n;
	endofline();
	zmult = n;
	return;
    }
    while (n--) {
	if (cs == 0)
	    return;
	if (line[cs - 1] == '\n')
	    if (!--cs)
		return;
	while (cs && line[cs - 1] != '\n')
	    cs--;
    }
}

/**/
void
endofline(void)
{
    int n = zmult;

    if (n < 0) {
	zmult = -n;
	beginningofline();
	zmult = n;
	return;
    }
    while (n--) {
	if (cs >= ll) {
	    cs = ll;
	    return;
	}
	if (line[cs] == '\n')
	    if (++cs == ll)
		return;
	while (cs != ll && line[cs] != '\n')
	    cs++;
    }
}

/**/
void
beginningoflinehist(void)
{
    int n = zmult;

    if (n < 0) {
	zmult = -n;
	endoflinehist();
	zmult = n;
	return;
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
	int m = zmult;

	zmult = n;
	uphistory();
	zmult = m;
	cs = 0;
    }
}

/**/
void
endoflinehist(void)
{
    int n = zmult;

    if (n < 0) {
	zmult = -n;
	beginningoflinehist();
	zmult = n;
	return;
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
	int m = zmult;

	zmult = n;
	downhistory();
	zmult = m;
    }
}

/**/
void
forwardchar(void)
{
    cs += zmult;
    if (cs > ll)
	cs = ll;
    if (cs < 0)
	cs = 0;
}

/**/
void
backwardchar(void)
{
    cs -= zmult;
    if (cs > ll)
	cs = ll;
    if (cs < 0)
	cs = 0;
}

/**/
void
setmarkcommand(void)
{
    mark = cs;
}

/**/
void
exchangepointandmark(void)
{
    int x;

    x = mark;
    mark = cs;
    cs = x;
    if (cs > ll)
	cs = ll;
}

/**/
void
vigotocolumn(void)
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
}

/**/
void
vimatchbracket(void)
{
    int ocs = cs, dir, ct;
    unsigned char oth, me;

  otog:
    if (cs == ll || line[cs] == '\n') {
	feep();
	cs = ocs;
	return;
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
	feep();
	cs = ocs;
    } else if(dir > 0 && virangeflag)
	cs++;
}

/**/
void
viforwardchar(void)
{
    int lim = findeol() - invicmdmode();
    int n = zmult;

    if (n < 0) {
	zmult = -n;
	vibackwardchar();
	zmult = n;
	return;
    }
    if (cs >= lim) {
	feep();
	return;
    }
    while (n-- && cs < lim)
	cs++;
}

/**/
void
vibackwardchar(void)
{
    int n = zmult;

    if (n < 0) {
	zmult = -n;
	viforwardchar();
	zmult = n;
	return;
    }
    if (cs == findbol()) {
	feep();
	return;
    }
    while (n--) {
	cs--;
	if (cs < 0 || line[cs] == '\n') {
	    cs++;
	    break;
	}
    }
}

/**/
void
viendofline(void)
{
    int oldcs = cs, n = zmult;

    if (n < 1) {
	feep();
	return;
    }
    while(n--) {
	if (cs > ll) {
	    cs = oldcs;
	    feep();
	    return;
	}
	cs = findeol() + 1;
    }
    cs--;
    lastcol = 1<<30;
}

/**/
void
vibeginningofline(void)
{
    cs = findbol();
}

static int vfindchar, vfinddir, tailadd;

/**/
void
vifindnextchar(void)
{
    if ((vfindchar = vigetkey()) != -1) {
	vfinddir = 1;
	tailadd = 0;
	virepeatfind();
    }
}

/**/
void
vifindprevchar(void)
{
    if ((vfindchar = vigetkey()) != -1) {
	vfinddir = -1;
	tailadd = 0;
	virepeatfind();
    }
}

/**/
void
vifindnextcharskip(void)
{
    if ((vfindchar = vigetkey()) != -1) {
	vfinddir = 1;
	tailadd = -1;
	virepeatfind();
    }
}

/**/
void
vifindprevcharskip(void)
{
    if ((vfindchar = vigetkey()) != -1) {
	vfinddir = -1;
	tailadd = 1;
	virepeatfind();
    }
}

/**/
void
virepeatfind(void)
{
    int ocs = cs, n = zmult;

    if (!vfinddir) {
	feep();
	return;
    }
    if (n < 0) {
	zmult = -n;
	virevrepeatfind();
	zmult = n;
	return;
    }
    while (n--) {
	do
	    cs += vfinddir;
	while (cs >= 0 && cs < ll && line[cs] != vfindchar && line[cs] != '\n');
	if (cs < 0 || cs >= ll || line[cs] == '\n') {
	    feep();
	    cs = ocs;
	    return;
	}
    }
    cs += tailadd;
    if (vfinddir == 1 && virangeflag)
	cs++;
}

/**/
void
virevrepeatfind(void)
{
    if (zmult < 0) {
	zmult = -zmult;
	virepeatfind();
	zmult = -zmult;
	return;
    }
    vfinddir = -vfinddir;
    virepeatfind();
    vfinddir = -vfinddir;
}

/**/
void
vifirstnonblank(void)
{
    cs = findbol();
    while (cs != ll && iblank(line[cs]))
	cs++;
}

/**/
void
visetmark(void)
{
    int ch;

    ch = getkey(0);
    if (ch < 'a' || ch > 'z') {
	feep();
	return;
    }
    ch -= 'a';
    vimarkcs[ch] = cs;
    vimarkline[ch] = histline;
}

/**/
void
vigotomark(void)
{
    int ch;

    ch = getkey(0);
    if (ch == c)
	ch = 26;
    else {
	if (ch < 'a' || ch > 'z') {
	    feep();
	    return;
	}
	ch -= 'a';
    }
    if (!vimarkline[ch]) {
	feep();
	return;
    }
    if (curhist != vimarkline[ch]) {
	char *s;

	remember_edits();
	if (!(s = qgetevent(vimarkline[ch]))) {
	    vimarkline[ch] = 0;
	    feep();
	    return;
	}
	histline = vimarkline[ch];
	setline(s);
    }
    cs = vimarkcs[ch];
    if (cs > ll)
	cs = ll;
}

/**/
void
vigotomarkline(void)
{
    vigotomark();
    vifirstnonblank();
}
