/*
 * zle_params.c - ZLE special parameters
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

#include "zle_params.pro"

/*
 * ZLE SPECIAL PARAMETERS:
 *
 * These special parameters are created, with a local scope, when
 * running user-defined widget functions.  Reading and writing them
 * reads and writes bits of ZLE state.  The parameters are:
 *
 * BUFFER   (scalar)   entire buffer contents
 * CURSOR   (integer)  cursor position; 0 <= $CURSOR <= $#BUFFER
 * LBUFFER  (scalar)   portion of buffer to the left of the cursor
 * RBUFFER  (scalar)   portion of buffer to the right of the cursor
 */

#define FN(X) ( (void (*) _((void))) (X) )
static struct zleparam {
    char *name;
    int type;
    void (*setfn) _((void));
    void (*getfn) _((void));
    void (*unsetfn) _((Param, int));
    void *data;
} zleparams[] = {
    { "BUFFER",  PM_SCALAR,  FN(set_buffer),  FN(get_buffer),
	zleunsetfn, NULL },
    { "CURSOR",  PM_INTEGER, FN(set_cursor),  FN(get_cursor),
	zleunsetfn, NULL },
    { "MARK",  PM_INTEGER, FN(set_mark),  FN(get_mark),
	zleunsetfn, NULL },
    { "LBUFFER", PM_SCALAR,  FN(set_lbuffer), FN(get_lbuffer),
	zleunsetfn, NULL },
    { "RBUFFER", PM_SCALAR,  FN(set_rbuffer), FN(get_rbuffer),
	zleunsetfn, NULL },
    { "PREBUFFER",  PM_SCALAR | PM_READONLY,  NULL,  FN(get_prebuffer),
	zleunsetfn, NULL },
    { "WIDGET", PM_SCALAR | PM_READONLY, NULL, FN(get_widget),
        zleunsetfn, NULL },
    { "LASTWIDGET", PM_SCALAR | PM_READONLY, NULL, FN(get_lwidget),
        zleunsetfn, NULL },
    { "KEYMAP", PM_SCALAR | PM_READONLY, NULL, FN(get_keymap),
        zleunsetfn, NULL },
    { "KEYS", PM_SCALAR | PM_READONLY, NULL, FN(get_keys),
        zleunsetfn, NULL },
    { "NUMERIC", PM_INTEGER | PM_UNSET, FN(set_numeric), FN(get_numeric),
        unset_numeric, NULL },
    { "HISTNO", PM_INTEGER | PM_READONLY, NULL, FN(get_histno),
        zleunsetfn, NULL },
    { "BUFFERLINES", PM_INTEGER | PM_READONLY, NULL, FN(get_bufferlines),
        zleunsetfn, NULL },
    { "PENDING", PM_INTEGER | PM_READONLY, NULL, FN(get_pending),
        zleunsetfn, NULL },
    { "CUTBUFFER", PM_SCALAR, FN(set_cutbuffer), FN(get_cutbuffer),
	unset_cutbuffer, NULL },
    { "killring", PM_ARRAY, FN(set_killring), FN(get_killring),
	unset_killring, NULL },
    { NULL, 0, NULL, NULL, NULL, NULL }
};

/**/
mod_export void
makezleparams(int ro)
{
    struct zleparam *zp;

    for(zp = zleparams; zp->name; zp++) {
	Param pm = createparam(zp->name, (zp->type |PM_SPECIAL|PM_REMOVABLE|
					  PM_LOCAL|(ro ? PM_READONLY : 0)));
	if (!pm)
	    pm = (Param) paramtab->getnode(paramtab, zp->name);
	DPUTS(!pm, "param not set in makezleparams");

	pm->level = locallevel + 1;
	pm->u.data = zp->data;
	switch(PM_TYPE(zp->type)) {
	    case PM_SCALAR:
		pm->sets.cfn = (void (*) _((Param, char *))) zp->setfn;
		pm->gets.cfn = (char *(*) _((Param))) zp->getfn;
		break;
	    case PM_ARRAY:
		pm->sets.afn = (void (*) _((Param, char **))) zp->setfn;
		pm->gets.afn = (char **(*) _((Param))) zp->getfn;
		break;
	    case PM_INTEGER:
		pm->sets.ifn = (void (*) _((Param, zlong))) zp->setfn;
		pm->gets.ifn = (zlong (*) _((Param))) zp->getfn;
		pm->ct = 10;
		break;
	}
	pm->unsetfn = zp->unsetfn;
	if ((zp->type & PM_UNSET) && (zmod.flags & MOD_MULT))
	    pm->flags &= ~PM_UNSET;
    }
}

/* Special unset function for ZLE special parameters: act like the standard *
 * unset function if this is a user-initiated unset, but nothing is done if *
 * the parameter is merely going out of scope (which it will do).           */

/**/
static void
zleunsetfn(Param pm, int exp)
{
    if(exp)
	stdunsetfn(pm, exp);
}

/**/
static void
set_buffer(Param pm, char *x)
{
    if(x) {
	unmetafy(x, &ll);
	sizeline(ll);
	strcpy((char *)line, x);
	zsfree(x);
	if(cs > ll)
	    cs = ll;
    } else
	cs = ll = 0;
    fixsuffix();
    menucmp = 0;
}

/**/
static char *
get_buffer(Param pm)
{
    return metafy((char *)line, ll, META_HEAPDUP);
}

/**/
static void
set_cursor(Param pm, zlong x)
{
    if(x < 0)
	cs = 0;
    else if(x > ll)
	cs = ll;
    else
	cs = x;
    fixsuffix();
    menucmp = 0;
}

/**/
static zlong
get_cursor(Param pm)
{
    return cs;
}

/**/
static void
set_mark(Param pm, zlong x)
{
    if (x < 0)
	mark = 0;
    else if (x > ll)
	mark = ll;
    else
	mark = x;
}

/**/
static zlong
get_mark(Param pm)
{
    return mark;
}

/**/
static void
set_lbuffer(Param pm, char *x)
{
    char *y;
    int len;

    if(x)
	unmetafy(y = x, &len);
    else
	y = "", len = 0;
    sizeline(ll - cs + len);
    memmove(line + len, line + cs, ll - cs);
    memcpy(line, y, len);
    ll = ll - cs + len;
    cs = len;
    zsfree(x);
    fixsuffix();
    menucmp = 0;
}

/**/
static char *
get_lbuffer(Param pm)
{
    return metafy((char *)line, cs, META_HEAPDUP);
}

/**/
static void
set_rbuffer(Param pm, char *x)
{
    char *y;
    int len;

    if(x)
	unmetafy(y = x, &len);
    else
	y = "", len = 0;
    sizeline(ll = cs + len);
    memcpy(line + cs, y, len);
    zsfree(x);
    fixsuffix();
    menucmp = 0;
}

/**/
static char *
get_rbuffer(Param pm)
{
    return metafy((char *)line + cs, ll - cs, META_HEAPDUP);
}

/**/
static char *
get_prebuffer(Param pm)
{
    if (chline)
	return dupstrpfx(chline, hptr - chline);
    else
	return dupstring("");
}

/**/
static char *
get_widget(Param pm)
{
    return bindk->nam;
}

/**/
static char *
get_lwidget(Param pm)
{
    return (lbindk ? lbindk->nam : "");
}

/**/
static char *
get_keymap(Param pm)
{
    return dupstring(curkeymapname);
}

/**/
static char *
get_keys(Param pm)
{
    return keybuf;
}

/**/
static void
set_numeric(Param pm, zlong x)
{
    zmult = x;
    zmod.flags = MOD_MULT;
}

/**/
static zlong
get_numeric(Param pm)
{
    return zmult;
}

/**/
static void
unset_numeric(Param pm, int exp)
{
    if (exp) {
	stdunsetfn(pm, exp);
	zmod.flags = 0;
	zmult = 1;
    }
}

/**/
static zlong
get_histno(Param pm)
{
    return histline;
}

/**/
static zlong
get_bufferlines(Param pm)
{
    return nlnct;
}

/**/
static zlong
get_pending(Param pm)
{
    return noquery(0);
}

/**/
static char *
get_cutbuffer(Param pm)
{
    if (cutbuf.buf)
	return metafy(cutbuf.buf, cutbuf.len, META_HEAPDUP);
    else
	return "";
}


/**/
static void
set_cutbuffer(Param pm, char *x)
{
    if (cutbuf.buf)
	free(cutbuf.buf);
    cutbuf.flags = 0;
    if (x) {
	unmetafy(x, &cutbuf.len);
	cutbuf.buf = zalloc(cutbuf.len);
	strcpy((char *)cutbuf.buf, x);
	zsfree(x);
    } else {
	cutbuf.buf = NULL;
	cutbuf.len = 0;
    }
}

/**/
static void
unset_cutbuffer(Param pm, int exp)
{
    if (exp) {
	stdunsetfn(pm, exp);
	if (cutbuf.buf) {
	    free(cutbuf.buf);
	    cutbuf.buf = NULL;
	    cutbuf.len = 0;
	}
    }
}

/**/
static void
set_killring(Param pm, char **x)
{
    int kpos, kcnt;
    char **p;

    kcnt = 0;
    kpos = kringnum;

    if (x) {
	/*
	 * Insert the elements into the kill ring, up to a maximum
	 * of KRINGCT.  We always handle the ring in the order
	 * a series of yank-pops would show, i.e. starting with
	 * the most recently cut and going backwards.
	 */
	for (p = x; kcnt < KRINGCT && *p; kcnt++, p++) {
	    Cutbuffer kptr = kring + kpos;
	    if (kptr->buf)
		free(kptr->buf);
	    kptr->buf = (char *)zalloc(strlen(*p));
	    strcpy(kptr->buf, *p);
	    unmetafy(kptr->buf, &kptr->len);
	    kptr->flags = 0;
	    kpos = (kpos + KRINGCT - 1) % KRINGCT;
	}
	freearray(x);
    }
    /*
     * Any values unsupplied are to be unset.
     */
    for (; kcnt < KRINGCT; kcnt++) {
	Cutbuffer kptr = kring + kpos;
	if (kptr->buf) {
	    free(kptr->buf);
	    kptr->buf = NULL;
	    kptr->flags = kptr->len = 0;
	}
	kpos = (kpos + KRINGCT - 1) % KRINGCT;
    }
}

/**/
static char **
get_killring(Param pm)
{
    /*
     * Return the kill ring with the most recently killed first.
     * Stop as soon as we find something which isn't set, i.e.
     * don't fill in bogus entries.
     */
    int kpos, kcnt;
    char **ret, **p;

    for (kpos = kringnum, kcnt = 0; kcnt < KRINGCT; kcnt++) {
	if (!kring[kpos].buf)
	    break;
	kpos = (kpos + KRINGCT - 1) % KRINGCT;
    }

    p = ret = (char **)zhalloc((kcnt+1) * sizeof(char *));

    for (kpos = kringnum; kcnt; kcnt--) {
	*p++ = metafy((char *)kring[kpos].buf, kring[kpos].len, META_HEAPDUP);
	kpos = (kpos + KRINGCT - 1) % KRINGCT;
    }
    *p = NULL;

    return ret;
}

/**/
static void
unset_killring(Param pm, int exp)
{
    if (exp) {
	set_killring(pm, NULL);
	stdunsetfn(pm, exp);
    }
}
