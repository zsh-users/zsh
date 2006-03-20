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
	while (zlecs != zlell && ZC_iword(zleline[zlecs]))
	    zlecs++;
	if (wordflag && !n)
	    return 0;
	while (zlecs != zlell && !ZC_iword(zleline[zlecs]))
	    zlecs++;
    }
    return 0;
}

#define Z_vialnum(X) (ZC_ialnum(X) || (ZWC('_') == X))

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
	if (Z_vialnum(zleline[zlecs]))
	    while (zlecs != zlell && Z_vialnum(zleline[zlecs]))
		zlecs++;
	else
	    while (zlecs != zlell && !Z_vialnum(zleline[zlecs]) && !ZC_iblank(zleline[zlecs]))
		zlecs++;
	if (wordflag && !n)
	    return 0;
	while (zlecs != zlell && ZC_inblank(zleline[zlecs]))
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
	while (zlecs != zlell && !ZC_iblank(zleline[zlecs]))
	    zlecs++;
	if (wordflag && !n)
	    return 0;
	while (zlecs != zlell && ZC_iblank(zleline[zlecs]))
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
	while (zlecs != zlell && !ZC_iword(zleline[zlecs]))
	    zlecs++;
	if (wordflag && !n)
	    return 0;
	while (zlecs != zlell && ZC_iword(zleline[zlecs]))
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
	while (zlecs != zlell && ZC_iblank(zleline[zlecs + 1]))
	    zlecs++;
	while (zlecs != zlell && !ZC_iblank(zleline[zlecs + 1]))
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
	if (ZC_iblank(zleline[zlecs + 1]))
	    while (zlecs != zlell && ZC_iblank(zleline[zlecs + 1]))
		zlecs++;
	if (Z_vialnum(zleline[zlecs + 1]))
	    while (zlecs != zlell && Z_vialnum(zleline[zlecs + 1]))
		zlecs++;
	else
	    while (zlecs != zlell && !Z_vialnum(zleline[zlecs + 1]) && !ZC_iblank(zleline[zlecs + 1]))
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
	while (zlecs && !ZC_iword(zleline[zlecs - 1]))
	    zlecs--;
	while (zlecs && ZC_iword(zleline[zlecs - 1]))
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
	while (zlecs && ZC_iblank(zleline[zlecs - 1]))
	    zlecs--;
	if (Z_vialnum(zleline[zlecs - 1]))
	    while (zlecs && Z_vialnum(zleline[zlecs - 1]))
		zlecs--;
	else
	    while (zlecs && !Z_vialnum(zleline[zlecs - 1]) && !ZC_iblank(zleline[zlecs - 1]))
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
	while (zlecs && ZC_iblank(zleline[zlecs - 1]))
	    zlecs--;
	while (zlecs && !ZC_iblank(zleline[zlecs - 1]))
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
	while (zlecs && !ZC_iword(zleline[zlecs - 1]))
	    zlecs--;
	while (zlecs && ZC_iword(zleline[zlecs - 1]))
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
	while (x && !ZC_iword(zleline[x - 1]))
	    x--;
	while (x && ZC_iword(zleline[x - 1]))
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
	while ((x > lim) && ZC_iblank(zleline[x - 1]))
	    x--;
	if (Z_vialnum(zleline[x - 1]))
	    while ((x > lim) && Z_vialnum(zleline[x - 1]))
		x--;
	else
	    while ((x > lim) && !Z_vialnum(zleline[x - 1]) && !ZC_iblank(zleline[x - 1]))
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
	while (x && !ZC_iword(zleline[x - 1]))
	    x--;
	while (x && ZC_iword(zleline[x - 1]))
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
	while (zlecs != zlell && !ZC_iword(zleline[zlecs]))
	    zlecs++;
	while (zlecs != zlell && ZC_iword(zleline[zlecs])) {
	    zleline[zlecs] = ZC_toupper(zleline[zlecs]);
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
	while (zlecs != zlell && !ZC_iword(zleline[zlecs]))
	    zlecs++;
	while (zlecs != zlell && ZC_iword(zleline[zlecs])) {
	    zleline[zlecs] = ZC_tolower(zleline[zlecs]);
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
	while (zlecs != zlell && !ZC_iword(zleline[zlecs]))
	    zlecs++;
	while (zlecs != zlell && ZC_iword(zleline[zlecs]) && !ZC_ialpha(zleline[zlecs]))
	    zlecs++;
	while (zlecs != zlell && ZC_iword(zleline[zlecs])) {
	    zleline[zlecs] = (first) ? ZC_toupper(zleline[zlecs]) :
		ZC_tolower(zleline[zlecs]);
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
	while (x != zlell && !ZC_iword(zleline[x]))
	    x++;
	while (x != zlell && ZC_iword(zleline[x]))
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
	while (x != zlell && !ZC_iword(zleline[x]))
	    x++;
	while (x != zlell && ZC_iword(zleline[x]))
	    x++;
    }
    forekill(x - zlecs, 0);
    return 0;
}

/**/
int
transposewords(UNUSED(char **args))
{
    int p1, p2, p3, p4, len, x = zlecs;
    ZLE_STRING_T temp, pp;
    int n = zmult;
    int neg = n < 0, ocs = zlecs;

    if (neg)
	n = -n;
    while (n--) {
	while (x != zlell && zleline[x] != ZWC('\n') && !ZC_iword(zleline[x]))
	    x++;
	if (x == zlell || zleline[x] == ZWC('\n')) {
	    x = zlecs;
	    while (x && zleline[x - 1] != ZWC('\n') && !ZC_iword(zleline[x]))
		x--;
	    if (!x || zleline[x - 1] == ZWC('\n'))
		return 1;
	}
	for (p4 = x; p4 != zlell && ZC_iword(zleline[p4]); p4++);
	for (p3 = p4; p3 && ZC_iword(zleline[p3 - 1]); p3--);
	if (!p3)
	    return 1;
	for (p2 = p3; p2 && !ZC_iword(zleline[p2 - 1]); p2--);
	if (!p2)
	    return 1;
	for (p1 = p2; p1 && ZC_iword(zleline[p1 - 1]); p1--);

	pp = temp = (ZLE_STRING_T)zhalloc((p4 - p1)*ZLE_CHAR_SIZE);
	len = p4 - p3;
	ZS_memcpy(pp, zleline + p3, len);
	pp += len;
	len = p3 - p2;
	ZS_memcpy(pp, zleline + p2, len);
	pp += len;
	ZS_memcpy(pp, zleline + p1, p2 - p1);

	ZS_memcpy(zleline + p1, temp, p4 - p1);

	zlecs = p4;
    }
    if (neg)
	zlecs = ocs;
    return 0;
}
