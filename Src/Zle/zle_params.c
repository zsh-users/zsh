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

static const struct gsu_scalar buffer_gsu =
{ get_buffer, set_buffer, zleunsetfn };
static const struct gsu_scalar context_gsu =
{ get_context, nullstrsetfn, zleunsetfn };
static const struct gsu_scalar cutbuffer_gsu =
{ get_cutbuffer, set_cutbuffer, unset_cutbuffer };
static const struct gsu_scalar keymap_gsu =
{ get_keymap, nullstrsetfn, zleunsetfn };
static const struct gsu_scalar keys_gsu =
{ get_keys, nullstrsetfn, zleunsetfn };
static const struct gsu_scalar lastsearch_gsu =
{ get_lsearch, nullstrsetfn, zleunsetfn };
static const struct gsu_scalar lastwidget_gsu =
{ get_lwidget, nullstrsetfn, zleunsetfn };
static const struct gsu_scalar lbuffer_gsu =
{ get_lbuffer, set_lbuffer, zleunsetfn };
static const struct gsu_scalar postdisplay_gsu =
{ get_postdisplay, set_postdisplay, zleunsetfn };
static const struct gsu_scalar prebuffer_gsu =
{ get_prebuffer, nullstrsetfn, zleunsetfn };
static const struct gsu_scalar predisplay_gsu =
{ get_predisplay, set_predisplay, zleunsetfn };
static const struct gsu_scalar rbuffer_gsu =
{ get_rbuffer, set_rbuffer, zleunsetfn };
static const struct gsu_scalar widget_gsu =
{ get_widget, nullstrsetfn, zleunsetfn };
static const struct gsu_scalar widgetfunc_gsu =
{ get_widgetfunc, nullstrsetfn, zleunsetfn };
static const struct gsu_scalar widgetstyle_gsu =
{ get_widgetstyle, nullstrsetfn, zleunsetfn };

static const struct gsu_integer bufferlines_gsu =
{ get_bufferlines, NULL, zleunsetfn };
static const struct gsu_integer cursor_gsu =
{ get_cursor, set_cursor, zleunsetfn };
static const struct gsu_integer histno_gsu =
{ get_histno, set_histno, zleunsetfn };
static const struct gsu_integer mark_gsu =
{ get_mark, set_mark, zleunsetfn };
static const struct gsu_integer numeric_gsu =
{ get_numeric, set_numeric, unset_numeric };
static const struct gsu_integer pending_gsu =
{ get_pending, NULL, zleunsetfn };

static const struct gsu_array killring_gsu =
{ get_killring, set_killring, unset_killring };

#define GSU(X) ( (GsuScalar)(void*)(&(X)) )
static struct zleparam {
    char *name;
    int type;
    GsuScalar gsu;
    void *data;
} zleparams[] = {
    { "BUFFER",  PM_SCALAR,  GSU(buffer_gsu), NULL },
    { "BUFFERLINES", PM_INTEGER | PM_READONLY, GSU(bufferlines_gsu),
        NULL },
    { "CONTEXT", PM_SCALAR | PM_READONLY, GSU(context_gsu),
	NULL },
    { "CURSOR",  PM_INTEGER, GSU(cursor_gsu),
	NULL },
    { "CUTBUFFER", PM_SCALAR, GSU(cutbuffer_gsu), NULL },
    { "HISTNO", PM_INTEGER, GSU(histno_gsu), NULL },
    { "KEYMAP", PM_SCALAR | PM_READONLY, GSU(keymap_gsu), NULL },
    { "KEYS", PM_SCALAR | PM_READONLY, GSU(keys_gsu), NULL },
    { "killring", PM_ARRAY, GSU(killring_gsu), NULL },
    { "LASTSEARCH", PM_SCALAR | PM_READONLY, GSU(lastsearch_gsu), NULL },
    { "LASTWIDGET", PM_SCALAR | PM_READONLY, GSU(lastwidget_gsu), NULL },
    { "LBUFFER", PM_SCALAR,  GSU(lbuffer_gsu), NULL },
    { "MARK",  PM_INTEGER, GSU(mark_gsu), NULL },
    { "NUMERIC", PM_INTEGER | PM_UNSET, GSU(numeric_gsu), NULL },
    { "PENDING", PM_INTEGER | PM_READONLY, GSU(pending_gsu), NULL },
    { "POSTDISPLAY", PM_SCALAR, GSU(postdisplay_gsu), NULL },
    { "PREBUFFER",  PM_SCALAR | PM_READONLY,  GSU(prebuffer_gsu), NULL },
    { "PREDISPLAY", PM_SCALAR, GSU(predisplay_gsu), NULL },
    { "RBUFFER", PM_SCALAR,  GSU(rbuffer_gsu), NULL },
    { "WIDGET", PM_SCALAR | PM_READONLY, GSU(widget_gsu), NULL },
    { "WIDGETFUNC", PM_SCALAR | PM_READONLY, GSU(widgetfunc_gsu), NULL },
    { "WIDGETSTYLE", PM_SCALAR | PM_READONLY, GSU(widgetstyle_gsu), NULL },
    { NULL, 0, NULL, NULL }
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
		pm->gsu.s = zp->gsu;
		break;
	    case PM_ARRAY:
		pm->gsu.a = (GsuArray)zp->gsu;
		break;
	    case PM_INTEGER:
		pm->gsu.i = (GsuInteger)zp->gsu;
		pm->base = 10;
		break;
	}
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
set_buffer(UNUSED(Param pm), char *x)
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
get_buffer(UNUSED(Param pm))
{
    return metafy((char *)line, ll, META_HEAPDUP);
}

/**/
static void
set_cursor(UNUSED(Param pm), zlong x)
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
get_cursor(UNUSED(Param pm))
{
    return cs;
}

/**/
static void
set_mark(UNUSED(Param pm), zlong x)
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
get_mark(UNUSED(Param pm))
{
    return mark;
}

/**/
static void
set_lbuffer(UNUSED(Param pm), char *x)
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
get_lbuffer(UNUSED(Param pm))
{
    return metafy((char *)line, cs, META_HEAPDUP);
}

/**/
static void
set_rbuffer(UNUSED(Param pm), char *x)
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
get_rbuffer(UNUSED(Param pm))
{
    return metafy((char *)line + cs, ll - cs, META_HEAPDUP);
}

/**/
static char *
get_prebuffer(UNUSED(Param pm))
{
    if (chline)
	return dupstrpfx(chline, hptr - chline);
    else
	return dupstring("");
}

/**/
static char *
get_widget(UNUSED(Param pm))
{
    return bindk->nam;
}

/**/
static char *
get_widgetfunc(UNUSED(Param pm))
{
    Widget widget = bindk->widget;
    int flags = widget->flags;

    if (flags & WIDGET_INT)
	return ".internal";	/* Don't see how this can ever be returned... */
    else if (flags & WIDGET_NCOMP)
	return widget->u.comp.func;
    else
	return widget->u.fnnam;
}

/**/
static char *
get_widgetstyle(UNUSED(Param pm))
{
    Widget widget = bindk->widget;
    int flags = widget->flags;

    if (flags & WIDGET_INT)
	return ".internal";	/* Don't see how this can ever be returned... */
    else if (flags & WIDGET_NCOMP)
	return widget->u.comp.wid;
    else
	return "";
}

/**/
static char *
get_lwidget(UNUSED(Param pm))
{
    return (lbindk ? lbindk->nam : "");
}

/**/
static char *
get_keymap(UNUSED(Param pm))
{
    return dupstring(curkeymapname);
}

/**/
static char *
get_keys(UNUSED(Param pm))
{
    return keybuf;
}

/**/
static void
set_numeric(UNUSED(Param pm), zlong x)
{
    zmult = x;
    zmod.flags = MOD_MULT;
}

/**/
static zlong
get_numeric(UNUSED(Param pm))
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
static void
set_histno(UNUSED(Param pm), zlong x)
{
    Histent he;

    if (!(he = quietgethist((int)x)))
	return;
    zle_setline(he);
}

/**/
static zlong
get_histno(UNUSED(Param pm))
{
    return histline;
}

/**/
static zlong
get_bufferlines(UNUSED(Param pm))
{
    return nlnct;
}

/**/
static zlong
get_pending(UNUSED(Param pm))
{
    return noquery(0);
}

/**/
static char *
get_cutbuffer(UNUSED(Param pm))
{
    if (cutbuf.buf)
	return metafy(cutbuf.buf, cutbuf.len, META_HEAPDUP);
    else
	return "";
}


/**/
static void
set_cutbuffer(UNUSED(Param pm), char *x)
{
    if (cutbuf.buf)
	free(cutbuf.buf);
    cutbuf.flags = 0;
    if (x) {
	int n;
	unmetafy(x, &n);
	cutbuf.len = n;
	cutbuf.buf = zalloc(cutbuf.len);
	memcpy((char *)cutbuf.buf, x, cutbuf.len);
	free(x);
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
set_killring(UNUSED(Param pm), char **x)
{
    int kcnt;
    Cutbuffer kptr;
    char **p;

    if (kring) {
	for (kptr = kring, kcnt = 0; kcnt < kringsize; kcnt++, kptr++)
	    if (kptr->buf)
		zfree(kptr->buf, kptr->len);
	zfree(kring, kringsize * sizeof(struct cutbuffer));
	kring = NULL;
	kringsize = kringnum = 0;
    }
    if (x) {
	/*
	 * Insert the elements into the kill ring.
	 * Regardless of the old order, we number it with the current
	 * entry first.
	 *
	 * Be careful to add elements by looping backwards; this
	 * fits in with how we cycle the ring.
	 */
	int kpos = 0;
	kringsize = arrlen(x);
	kring = (Cutbuffer)zshcalloc(kringsize * sizeof(struct cutbuffer));
	for (p = x; *p; p++) {
	    int n, len = strlen(*p);
	    kptr = kring + kpos;
	    unmetafy(*p, &n);
	    kptr->len = n;
	    kptr->buf = (char *)zalloc(kptr->len);
	    memcpy(kptr->buf, *p, kptr->len);
	    zfree(*p, len+1);
	    kpos = (kpos + kringsize -1 ) % kringsize;
	}
	free(x);
    }
}

/**/
static char **
get_killring(UNUSED(Param pm))
{
    /*
     * Return the kill ring with the most recently killed first.
     * Since the kill ring is no longer a fixed length, we return
     * all entries even if empty.
     */
    int kpos, kcnt;
    char **ret, **p;

    /* Supposed to work even if kring is NULL */
    if (!kring) {
	kringsize = KRINGCTDEF;
	kring = (Cutbuffer)zshcalloc(kringsize * sizeof(struct cutbuffer));
    }

    p = ret = (char **)zhalloc((kringsize+1) * sizeof(char *));

    for (kpos = kringnum, kcnt = 0; kcnt < kringsize; kcnt++) {
	Cutbuffer kptr = kring + kpos;
	if (kptr->buf)
	{
	    /*
	     * Need to use HEAPDUP to make sure there's room for the
	     * terminating NULL.
	     */
	    *p++ = metafy((char *)kptr->buf, kptr->len, META_HEAPDUP);
	}
	else
	    *p++ = dupstring("");
	kpos = (kpos + kringsize - 1) % kringsize;
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

static void
set_prepost(unsigned char **textvar, int *lenvar, char *x)
{
    if (*lenvar) {
	zfree(*textvar, *lenvar);
	*textvar = NULL;
	*lenvar = 0;
    }
    if (x) {
	unmetafy(x, lenvar);
	if (*lenvar) {
	    *textvar = (unsigned char *)zalloc(*lenvar);
	    memcpy((char *)*textvar, x, *lenvar);
	}
	free(x);
    }
}

static char *
get_prepost(unsigned char *text, int len)
{
    return metafy((char *)text, len, META_HEAPDUP);
}

/**/
static void
set_predisplay(UNUSED(Param pm), char *x)
{
    set_prepost(&predisplay, &predisplaylen, x);
}

/**/
static char *
get_predisplay(UNUSED(Param pm))
{
    return get_prepost(predisplay, predisplaylen);
}

/**/
static void
set_postdisplay(UNUSED(Param pm), char *x)
{
    set_prepost(&postdisplay, &postdisplaylen, x);
}

/**/
static char *
get_postdisplay(UNUSED(Param pm))
{
    return get_prepost(postdisplay, postdisplaylen);
}

/**/
void
free_prepostdisplay(void)
{
    if (predisplaylen)
	set_prepost(&predisplay, &predisplaylen, NULL);
    if (postdisplaylen)
	set_prepost(&postdisplay, &postdisplaylen, NULL);
}

/**/
static char *
get_lsearch(UNUSED(Param pm))
{
    if (previous_search_len)
	return metafy(previous_search, previous_search_len, META_HEAPDUP);
    else
	return "";
}

/**/
static char *
get_context(UNUSED(Param pm))
{
    switch (zlecontext) {
    case ZLCON_LINE_CONT:
	return "cont";
	break;

    case ZLCON_SELECT:
	return "select";
	break;

    case ZLCON_VARED:
	return "vared";
	break;

    case ZLCON_LINE_START:
    default:
	return "start";
	break;
    }
}
