/*
 * zle_word.c - word-related editor functions
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
#include "zle_word.pro"

/**/
int
forwardword(char **args)
{
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = backwardword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	while (cs != ll && iword(line[cs]))
	    cs++;
	if (wordflag && !n)
	    return 0;
	while (cs != ll && !iword(line[cs]))
	    cs++;
    }
    return 0;
}

/**/
int
viforwardword(char **args)
{
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = backwardword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	if (iident(line[cs]))
	    while (cs != ll && iident(line[cs]))
		cs++;
	else
	    while (cs != ll && !iident(line[cs]) && !iblank(line[cs]))
		cs++;
	if (wordflag && !n)
	    return 0;
	while (cs != ll && (iblank(line[cs]) || line[cs] == '\n'))
	    cs++;
    }
    return 0;
}

/**/
int
viforwardblankword(char **args)
{
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = vibackwardblankword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	while (cs != ll && !iblank(line[cs]))
	    cs++;
	if (wordflag && !n)
	    return 0;
	while (cs != ll && iblank(line[cs]))
	    cs++;
    }
    return 0;
}

/**/
int
emacsforwardword(char **args)
{
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = emacsbackwardword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	while (cs != ll && !iword(line[cs]))
	    cs++;
	if (wordflag && !n)
	    return 0;
	while (cs != ll && iword(line[cs]))
	    cs++;
    }
    return 0;
}

/**/
int
viforwardblankwordend(UNUSED(char **args))
{
    int n = zmult;

    if (n < 0)
	return 1;
    while (n--) {
	while (cs != ll && iblank(line[cs + 1]))
	    cs++;
	while (cs != ll && !iblank(line[cs + 1]))
	    cs++;
    }
    if (cs != ll && virangeflag)
	cs++;
    return 0;
}

/**/
int
viforwardwordend(char **args)
{
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = backwardword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	if (iblank(line[cs + 1]))
	    while (cs != ll && iblank(line[cs + 1]))
		cs++;
	if (iident(line[cs + 1]))
	    while (cs != ll && iident(line[cs + 1]))
		cs++;
	else
	    while (cs != ll && !iident(line[cs + 1]) && !iblank(line[cs + 1]))
		cs++;
    }
    if (cs != ll && virangeflag)
	cs++;
    return 0;
}

/**/
int
backwardword(char **args)
{
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = forwardword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	while (cs && !iword(line[cs - 1]))
	    cs--;
	while (cs && iword(line[cs - 1]))
	    cs--;
    }
    return 0;
}

/**/
int
vibackwardword(char **args)
{
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = backwardword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	while (cs && iblank(line[cs - 1]))
	    cs--;
	if (iident(line[cs - 1]))
	    while (cs && iident(line[cs - 1]))
		cs--;
	else
	    while (cs && !iident(line[cs - 1]) && !iblank(line[cs - 1]))
		cs--;
    }
    return 0;
}

/**/
int
vibackwardblankword(char **args)
{
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = viforwardblankword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	while (cs && iblank(line[cs - 1]))
	    cs--;
	while (cs && !iblank(line[cs - 1]))
	    cs--;
    }
    return 0;
}

/**/
int
emacsbackwardword(char **args)
{
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = emacsforwardword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	while (cs && !iword(line[cs - 1]))
	    cs--;
	while (cs && iword(line[cs - 1]))
	    cs--;
    }
    return 0;
}

/**/
int
backwarddeleteword(char **args)
{
    int x = cs, n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = deleteword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	while (x && !iword(line[x - 1]))
	    x--;
	while (x && iword(line[x - 1]))
	    x--;
    }
    backdel(cs - x);
    return 0;
}

/**/
int
vibackwardkillword(UNUSED(char **args))
{
    int x = cs, lim = (viinsbegin > findbol()) ? viinsbegin : findbol();
    int n = zmult;

    if (n < 0)
	return 1;
/* this taken from "vibackwardword" */
    while (n--) {
	while ((x > lim) && iblank(line[x - 1]))
	    x--;
	if (iident(line[x - 1]))
	    while ((x > lim) && iident(line[x - 1]))
		x--;
	else
	    while ((x > lim) && !iident(line[x - 1]) && !iblank(line[x - 1]))
		x--;
    }
    backkill(cs - x, 1);
    return 0;
}

/**/
int
backwardkillword(char **args)
{
    int x = cs;
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = killword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	while (x && !iword(line[x - 1]))
	    x--;
	while (x && iword(line[x - 1]))
	    x--;
    }
    backkill(cs - x, 1);
    return 0;
}

/**/
int
upcaseword(UNUSED(char **args))
{
    int n = zmult;
    int neg = n < 0, ocs = cs;

    if (neg)
	n = -n;
    while (n--) {
	while (cs != ll && !iword(line[cs]))
	    cs++;
	while (cs != ll && iword(line[cs])) {
	    line[cs] = tuupper(line[cs]);
	    cs++;
	}
    }
    if (neg)
	cs = ocs;
    return 0;
}

/**/
int
downcaseword(UNUSED(char **args))
{
    int n = zmult;
    int neg = n < 0, ocs = cs;

    if (neg)
	n = -n;
    while (n--) {
	while (cs != ll && !iword(line[cs]))
	    cs++;
	while (cs != ll && iword(line[cs])) {
	    line[cs] = tulower(line[cs]);
	    cs++;
	}
    }
    if (neg)
	cs = ocs;
    return 0;
}

/**/
int
capitalizeword(UNUSED(char **args))
{
    int first, n = zmult;
    int neg = n < 0, ocs = cs;

    if (neg)
	n = -n;
    while (n--) {
	first = 1;
	while (cs != ll && !iword(line[cs]))
	    cs++;
	while (cs != ll && iword(line[cs]) && !isalpha(line[cs]))
	    cs++;
	while (cs != ll && iword(line[cs])) {
	    line[cs] = (first) ? tuupper(line[cs]) : tulower(line[cs]);
	    first = 0;
	    cs++;
	}
    }
    if (neg)
	cs = ocs;
    return 0;
}

/**/
int
deleteword(char **args)
{
    int x = cs;
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = backwarddeleteword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	while (x != ll && !iword(line[x]))
	    x++;
	while (x != ll && iword(line[x]))
	    x++;
    }
    foredel(x - cs);
    return 0;
}

/**/
int
killword(char **args)
{
    int x = cs;
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = backwardkillword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	while (x != ll && !iword(line[x]))
	    x++;
	while (x != ll && iword(line[x]))
	    x++;
    }
    forekill(x - cs, 0);
    return 0;
}

/**/
int
transposewords(UNUSED(char **args))
{
    int p1, p2, p3, p4, x = cs;
    char *temp, *pp;
    int n = zmult;
    int neg = n < 0, ocs = cs;

    if (neg)
	n = -n;
    while (n--) {
	while (x != ll && line[x] != '\n' && !iword(line[x]))
	    x++;
	if (x == ll || line[x] == '\n') {
	    x = cs;
	    while (x && line[x - 1] != '\n' && !iword(line[x]))
		x--;
	    if (!x || line[x - 1] == '\n')
		return 1;
	}
	for (p4 = x; p4 != ll && iword(line[p4]); p4++);
	for (p3 = p4; p3 && iword(line[p3 - 1]); p3--);
	if (!p3)
	    return 1;
	for (p2 = p3; p2 && !iword(line[p2 - 1]); p2--);
	if (!p2)
	    return 1;
	for (p1 = p2; p1 && iword(line[p1 - 1]); p1--);
	pp = temp = (char *)zhalloc(p4 - p1 + 1);
	struncpy(&pp, (char *) line + p3, p4 - p3);
	struncpy(&pp, (char *) line + p2, p3 - p2);
	struncpy(&pp, (char *) line + p1, p2 - p1);
	strncpy((char *)line + p1, temp, p4 - p1);
	cs = p4;
    }
    if (neg)
	cs = ocs;
    return 0;
}
