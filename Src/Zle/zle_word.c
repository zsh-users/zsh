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

/*
 * TODO: use of iword needs completely rethinking for Unicode
 * since we can't base it on a table lookup.
 */

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
	while (zlecs != zlell && iword(zleline[zlecs]))
	    zlecs++;
	if (wordflag && !n)
	    return 0;
	while (zlecs != zlell && !iword(zleline[zlecs]))
	    zlecs++;
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
	if (iident(zleline[zlecs]))
	    while (zlecs != zlell && iident(zleline[zlecs]))
		zlecs++;
	else
	    while (zlecs != zlell && !iident(zleline[zlecs]) && !iblank(zleline[zlecs]))
		zlecs++;
	if (wordflag && !n)
	    return 0;
	while (zlecs != zlell && (iblank(zleline[zlecs]) || zleline[zlecs] == '\n'))
	    zlecs++;
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
	while (zlecs != zlell && !iblank(zleline[zlecs]))
	    zlecs++;
	if (wordflag && !n)
	    return 0;
	while (zlecs != zlell && iblank(zleline[zlecs]))
	    zlecs++;
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
	while (zlecs != zlell && !iword(zleline[zlecs]))
	    zlecs++;
	if (wordflag && !n)
	    return 0;
	while (zlecs != zlell && iword(zleline[zlecs]))
	    zlecs++;
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
	while (zlecs != zlell && iblank(zleline[zlecs + 1]))
	    zlecs++;
	while (zlecs != zlell && !iblank(zleline[zlecs + 1]))
	    zlecs++;
    }
    if (zlecs != zlell && virangeflag)
	zlecs++;
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
	if (iblank(zleline[zlecs + 1]))
	    while (zlecs != zlell && iblank(zleline[zlecs + 1]))
		zlecs++;
	if (iident(zleline[zlecs + 1]))
	    while (zlecs != zlell && iident(zleline[zlecs + 1]))
		zlecs++;
	else
	    while (zlecs != zlell && !iident(zleline[zlecs + 1]) && !iblank(zleline[zlecs + 1]))
		zlecs++;
    }
    if (zlecs != zlell && virangeflag)
	zlecs++;
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
	while (zlecs && !iword(zleline[zlecs - 1]))
	    zlecs--;
	while (zlecs && iword(zleline[zlecs - 1]))
	    zlecs--;
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
	while (zlecs && iblank(zleline[zlecs - 1]))
	    zlecs--;
	if (iident(zleline[zlecs - 1]))
	    while (zlecs && iident(zleline[zlecs - 1]))
		zlecs--;
	else
	    while (zlecs && !iident(zleline[zlecs - 1]) && !iblank(zleline[zlecs - 1]))
		zlecs--;
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
	while (zlecs && iblank(zleline[zlecs - 1]))
	    zlecs--;
	while (zlecs && !iblank(zleline[zlecs - 1]))
	    zlecs--;
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
	while (zlecs && !iword(zleline[zlecs - 1]))
	    zlecs--;
	while (zlecs && iword(zleline[zlecs - 1]))
	    zlecs--;
    }
    return 0;
}

/**/
int
backwarddeleteword(char **args)
{
    int x = zlecs, n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = deleteword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	while (x && !iword(zleline[x - 1]))
	    x--;
	while (x && iword(zleline[x - 1]))
	    x--;
    }
    backdel(zlecs - x);
    return 0;
}

/**/
int
vibackwardkillword(UNUSED(char **args))
{
    int x = zlecs, lim = (viinsbegin > findbol()) ? viinsbegin : findbol();
    int n = zmult;

    if (n < 0)
	return 1;
/* this taken from "vibackwardword" */
    while (n--) {
	while ((x > lim) && iblank(zleline[x - 1]))
	    x--;
	if (iident(zleline[x - 1]))
	    while ((x > lim) && iident(zleline[x - 1]))
		x--;
	else
	    while ((x > lim) && !iident(zleline[x - 1]) && !iblank(zleline[x - 1]))
		x--;
    }
    backkill(zlecs - x, 1);
    return 0;
}

/**/
int
backwardkillword(char **args)
{
    int x = zlecs;
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = killword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	while (x && !iword(zleline[x - 1]))
	    x--;
	while (x && iword(zleline[x - 1]))
	    x--;
    }
    backkill(zlecs - x, 1);
    return 0;
}

/**/
int
upcaseword(UNUSED(char **args))
{
    int n = zmult;
    int neg = n < 0, ocs = zlecs;

    if (neg)
	n = -n;
    while (n--) {
	while (zlecs != zlell && !iword(zleline[zlecs]))
	    zlecs++;
	while (zlecs != zlell && iword(zleline[zlecs])) {
	    zleline[zlecs] = ZS_toupper(zleline[zlecs]);
	    zlecs++;
	}
    }
    if (neg)
	zlecs = ocs;
    return 0;
}

/**/
int
downcaseword(UNUSED(char **args))
{
    int n = zmult;
    int neg = n < 0, ocs = zlecs;

    if (neg)
	n = -n;
    while (n--) {
	while (zlecs != zlell && !iword(zleline[zlecs]))
	    zlecs++;
	while (zlecs != zlell && iword(zleline[zlecs])) {
	    zleline[zlecs] = ZS_tolower(zleline[zlecs]);
	    zlecs++;
	}
    }
    if (neg)
	zlecs = ocs;
    return 0;
}

/**/
int
capitalizeword(UNUSED(char **args))
{
    int first, n = zmult;
    int neg = n < 0, ocs = zlecs;

    if (neg)
	n = -n;
    while (n--) {
	first = 1;
	while (zlecs != zlell && !iword(zleline[zlecs]))
	    zlecs++;
	while (zlecs != zlell && iword(zleline[zlecs]) && !isalpha(zleline[zlecs]))
	    zlecs++;
	while (zlecs != zlell && iword(zleline[zlecs])) {
	    zleline[zlecs] = (first) ? ZS_toupper(zleline[zlecs]) :
		ZS_tolower(zleline[zlecs]);
	    first = 0;
	    zlecs++;
	}
    }
    if (neg)
	zlecs = ocs;
    return 0;
}

/**/
int
deleteword(char **args)
{
    int x = zlecs;
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = backwarddeleteword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	while (x != zlell && !iword(zleline[x]))
	    x++;
	while (x != zlell && iword(zleline[x]))
	    x++;
    }
    foredel(x - zlecs);
    return 0;
}

/**/
int
killword(char **args)
{
    int x = zlecs;
    int n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = backwardkillword(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	while (x != zlell && !iword(zleline[x]))
	    x++;
	while (x != zlell && iword(zleline[x]))
	    x++;
    }
    forekill(x - zlecs, 0);
    return 0;
}

/**/
int
transposewords(UNUSED(char **args))
{
    int p1, p2, p3, p4, x = zlecs;
    char *temp, *pp;
    int n = zmult;
    int neg = n < 0, ocs = zlecs;

    if (neg)
	n = -n;
    while (n--) {
	while (x != zlell && zleline[x] != '\n' && !iword(zleline[x]))
	    x++;
	if (x == zlell || zleline[x] == '\n') {
	    x = zlecs;
	    while (x && zleline[x - 1] != '\n' && !iword(zleline[x]))
		x--;
	    if (!x || zleline[x - 1] == '\n')
		return 1;
	}
	for (p4 = x; p4 != zlell && iword(zleline[p4]); p4++);
	for (p3 = p4; p3 && iword(zleline[p3 - 1]); p3--);
	if (!p3)
	    return 1;
	for (p2 = p3; p2 && !iword(zleline[p2 - 1]); p2--);
	if (!p2)
	    return 1;
	for (p1 = p2; p1 && iword(zleline[p1 - 1]); p1--);
	pp = temp = (char *)zhalloc(p4 - p1 + 1);
	struncpy(&pp, (char *) zleline + p3, p4 - p3);
	struncpy(&pp, (char *) zleline + p2, p3 - p2);
	struncpy(&pp, (char *) zleline + p1, p2 - p1);
	strncpy((char *)zleline + p1, temp, p4 - p1);
	zlecs = p4;
    }
    if (neg)
	zlecs = ocs;
    return 0;
}
