/*
 * zle_misc.c - miscellaneous editor routines
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
#include "zle_misc.pro"

/* insert a metafied string, with repetition and suffix removal */

/**/
void
doinsert(ZLE_STRING_T zstr, int len)
{
    ZLE_STRING_T s;
    ZLE_CHAR_T c1 = *zstr;	     /* first character */
    int neg = zmult < 0;             /* insert *after* the cursor? */
    int m = neg ? -zmult : zmult;    /* number of copies to insert */
    int count;

    iremovesuffix(c1, 0);
    invalidatelist();

    if(insmode)
	spaceinline(m * len);
    else if(zlecs + m * len > zlell)
	spaceinline(zlecs + m * len - zlell);
    while(m--)
	for(s = zstr, count = len; count; s++, count--)
	    zleline[zlecs++] = *s;
    if(neg)
	zlecs += zmult * len;
}

/**/
mod_export int
selfinsert(UNUSED(char **args))
{
#ifdef ZLE_UNICODE_SUPPORT
    /* wint_t and wchar_t not neccessarily the same size */
    wchar_t tmp;

    if (!lastchar_wide_valid)
	getrestchar(lastchar);
    tmp = lastchar_wide;
    doinsert(&tmp, 1);
#else
    char s = lastchar;
    doinsert(&s, 1);
#endif
    return 0;
}

/**/
mod_export void
fixunmeta(void)
{
    lastchar &= 0x7f;
    if (lastchar == '\r')
	lastchar = '\n';
#ifdef ZLE_UNICODE_SUPPORT
    /*
     * TODO: can we do this better?
     * We need a wide character to insert.
     * selfinsertunmeta is intrinsically problematic
     * with multibyte input.
     */
    lastchar_wide = (ZLE_CHAR_T)lastchar;
    lastchar_wide_valid = 1;
#endif
}

/**/
mod_export int
selfinsertunmeta(char **args)
{
    fixunmeta();
    return selfinsert(args);
}

/**/
int
deletechar(char **args)
{
    if (zmult < 0) {
	int ret;
	zmult = -zmult;
	ret = backwarddeletechar(args);
	zmult = -zmult;
	return ret;
    }
    if (zlecs + zmult <= zlell) {
	zlecs += zmult;
	backdel(zmult);
	return 0;
    }
    return 1;
}

/**/
int
backwarddeletechar(char **args)
{
    if (zmult < 0) {
	int ret;
	zmult = -zmult;
	ret = deletechar(args);
	zmult = -zmult;
	return ret;
    }
    backdel(zmult > zlecs ? zlecs : zmult);
    return 0;
}

/**/
int
killwholeline(UNUSED(char **args))
{
    int i, fg, n = zmult;

    if (n < 0)
	return 1;
    while (n--) {
	if ((fg = (zlecs && zlecs == zlell)))
	    zlecs--;
	while (zlecs && zleline[zlecs - 1] != '\n')
	    zlecs--;
	for (i = zlecs; i != zlell && zleline[i] != '\n'; i++);
	forekill(i - zlecs + (i != zlell), fg);
    }
    clearlist = 1;
    return 0;
}

/**/
int
killbuffer(UNUSED(char **args))
{
    zlecs = 0;
    forekill(zlell, 0);
    clearlist = 1;
    return 0;
}

/**/
int
backwardkillline(char **args)
{
    int i = 0, n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = killline(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	if (zlecs && zleline[zlecs - 1] == '\n')
	    zlecs--, i++;
	else
	    while (zlecs && zleline[zlecs - 1] != '\n')
		zlecs--, i++;
    }
    forekill(i, 1);
    clearlist = 1;
    return 0;
}

/**/
int
gosmacstransposechars(UNUSED(char **args))
{
    int cc;

    if (zlecs < 2 || zleline[zlecs - 1] == '\n' || zleline[zlecs - 2] == '\n') {
	if (zlecs == zlell || zleline[zlecs] == '\n' ||
	    ((zlecs + 1 == zlell || zleline[zlecs + 1] == '\n') &&
	     (!zlecs || zleline[zlecs - 1] == '\n'))) {
	    return 1;
	}
	zlecs += (zlecs == 0 || zleline[zlecs - 1] == '\n') ? 2 : 1;
    }
    cc = zleline[zlecs - 2];
    zleline[zlecs - 2] = zleline[zlecs - 1];
    zleline[zlecs - 1] = cc;
    return 0;
}

/**/
int
transposechars(UNUSED(char **args))
{
    int cc, ct;
    int n = zmult;
    int neg = n < 0;

    if (neg)
	n = -n;
    while (n--) {
	if (!(ct = zlecs) || zleline[zlecs - 1] == '\n') {
	    if (zlell == zlecs || zleline[zlecs] == '\n')
		return 1;
	    if (!neg)
		zlecs++;
	    ct++;
	}
	if (neg) {
	    if (zlecs && zleline[zlecs - 1] != '\n') {
		zlecs--;
		if (ct > 1 && zleline[ct - 2] != '\n')
		    ct--;
	    }
	} else {
	    if (zlecs != zlell && zleline[zlecs] != '\n')
		zlecs++;
	}
	if (ct == zlell || zleline[ct] == '\n')
	    ct--;
	if (ct < 1 || zleline[ct - 1] == '\n')
	    return 1;
	cc = zleline[ct - 1];
	zleline[ct - 1] = zleline[ct];
	zleline[ct] = cc;
    }
    return 0;
}

/**/
int
poundinsert(UNUSED(char **args))
{
    zlecs = 0;
    vifirstnonblank(zlenoargs);
    if (zleline[zlecs] != '#') {
	spaceinline(1);
	zleline[zlecs] = '#';
	zlecs = findeol();
	while(zlecs != zlell) {
	    zlecs++;
	    vifirstnonblank(zlenoargs);
	    spaceinline(1);
	    zleline[zlecs] = '#';
	    zlecs = findeol();
	}
    } else {
	foredel(1);
	zlecs = findeol();
	while(zlecs != zlell) {
	    zlecs++;
	    vifirstnonblank(zlenoargs);
	    if(zleline[zlecs] == '#')
		foredel(1);
	    zlecs = findeol();
	}
    }
    done = 1;
    return 0;
}

/**/
int
acceptline(UNUSED(char **args))
{
    done = 1;
    return 0;
}

/**/
int
acceptandhold(UNUSED(char **args))
{
    zpushnode(bufstack, zlelineasstring(zleline, zlell, 0, NULL, NULL, 0));
    stackcs = zlecs;
    done = 1;
    return 0;
}

/**/
int
killline(char **args)
{
    int i = 0, n = zmult;

    if (n < 0) {
	int ret;
	zmult = -n;
	ret = backwardkillline(args);
	zmult = n;
	return ret;
    }
    while (n--) {
	if (zleline[zlecs] == ZWC('\n'))
	    zlecs++, i++;
	else
	    while (zlecs != zlell && zleline[zlecs] != ZWC('\n'))
		zlecs++, i++;
    }
    backkill(i, 0);
    clearlist = 1;
    return 0;
}

/**/
int
killregion(UNUSED(char **args))
{
    if (mark > zlell)
	mark = zlell;
    if (mark > zlecs)
	forekill(mark - zlecs, 0);
    else
	backkill(zlecs - mark, 1);
    return 0;
}

/**/
int
copyregionaskill(UNUSED(char **args))
{
    if (mark > zlell)
	mark = zlell;
    if (mark > zlecs)
	cut(zlecs, mark - zlecs, 0);
    else
	cut(mark, zlecs - mark, 1);
    return 0;
}

/*
 * kct: index into kill ring, or -1 for original cutbuffer of yank.
 * yankb, yanke: mark the start and end of last yank in editing buffer.
 */
static int kct, yankb, yanke;
/* The original cutbuffer, either cutbuf or one of the vi buffers. */
static Cutbuffer kctbuf;

/**/
int
yank(UNUSED(char **args))
{
    int n = zmult;

    if (n < 0)
	return 1;
    if (zmod.flags & MOD_VIBUF)
	kctbuf = &vibuf[zmod.vibuf];
    else
	kctbuf = &cutbuf;
    if (!kctbuf->buf)
	return 1;
    mark = zlecs;
    yankb = zlecs;
    while (n--) {
	kct = -1;
	spaceinline(kctbuf->len);
	ZS_memcpy(zleline + zlecs, kctbuf->buf, kctbuf->len);
	zlecs += kctbuf->len;
	yanke = zlecs;
    }
    return 0;
}

/**/
int
yankpop(UNUSED(char **args))
{
    int cc, kctstart = kct;
    Cutbuffer buf;

    if (!(lastcmd & ZLE_YANK) || !kring || !kctbuf) {
	kctbuf = NULL;
	return 1;
    }
    do {
	/*
	 * This is supposed to make the yankpop loop
	 *   original buffer -> kill ring in order -> original buffer -> ...
	 * where the original buffer is -1 and the remainder are
	 * indices into the kill ring, remember that we need to start
	 * that at kringnum rather than zero.
	 */
	if (kct == -1)
	    kct = kringnum;
	else {
	    int kctnew = (kct + kringsize - 1) % kringsize;
	    if (kctnew == kringnum)
		kct = -1;
	    else
		kct = kctnew;
	}
	if (kct == -1)
	    buf = kctbuf;	/* Use original cutbuffer */
	else
	    buf = kring+kct;	/* Use somewhere in the kill ring */
	/* Careful to prevent infinite looping */
	if (kct == kctstart)
	    return 1;
	/*
	 * Skip unset buffers instead of stopping as we used to do.
	 * Also skip zero-length buffers.
	 * There are two reasons for this:
	 * 1. We now map the array $killring directly into the
	 *    killring, instead of via some extra size-checking logic.
	 *    When $killring has been set, a buffer will always have
	 *    at least a zero-length string in it.
	 * 2. The old logic was inconsistent; when the kill ring
	 *    was full, we could loop round and round it, otherwise
	 *    we just stopped when we hit the first empty buffer.
	 */
    } while (!buf->buf || *buf->buf == ZWC('\0'));

    zlecs = yankb;
    foredel(yanke - yankb);
    cc = buf->len;
    spaceinline(cc);
    ZS_memcpy(zleline + zlecs, buf->buf, cc);
    zlecs += cc;
    yanke = zlecs;
    return 0;
}

/**/
int
overwritemode(UNUSED(char **args))
{
    insmode ^= 1;
    return 0;
}

/**/
int
whatcursorposition(UNUSED(char **args))
{
    char msg[100];
    char *s = msg, *mbstr;
    int bol = findbol(), len;
    ZLE_CHAR_T c = zleline[zlecs];

    if (zlecs == zlell)
	strucpy(&s, "EOF");
    else {
	strucpy(&s, "Char: ");
	switch (c) {
	case ZWC(' '):
	    strucpy(&s, "SPC");
	    break;
	case ZWC('\t'):
	    strucpy(&s, "TAB");
	    break;
	case ZWC('\n'):
	    strucpy(&s, "LFD");
	    break;
	default:
	    /*
	     * convert a single character, remembering it may
	     * turn into a multibyte string or be metafied.
	     */
	    mbstr = zlelineasstring(zleline+zlecs, 1, 0, &len, NULL, 1);
	    strcpy(s, mbstr);
	    s += len;
	}
	sprintf(s, " (0%o, %u, 0x%x)", (unsigned int)c,
		(unsigned int)c, (unsigned int)c);
	s += strlen(s);
    }
    sprintf(s, "  point %d of %d(%d%%)  column %d", zlecs+1, zlell+1,
	    zlell ? 100 * zlecs / zlell : 0, zlecs - bol);
    showmsg(msg);
    return 0;
}

/**/
int
undefinedkey(UNUSED(char **args))
{
    return 1;
}

/**/
int
quotedinsert(char **args)
{
#ifndef HAS_TIO
    struct sgttyb sob;

    sob = shttyinfo.sgttyb;
    sob.sg_flags = (sob.sg_flags | RAW) & ~ECHO;
    ioctl(SHTTY, TIOCSETN, &sob);
#endif
    getfullchar(0);
#ifndef HAS_TIO
    zsetterm();
#endif
    if (LASTFULLCHAR == ZLEEOF)
	return 1;
    else
	return selfinsert(args);
}

/**/
int
digitargument(UNUSED(char **args))
{
    int sign = (zmult < 0) ? -1 : 1;

#ifdef ZLE_UNICODE_SUPPORT
    /*
     * It's too dangerous to allow metafied input.  See
     * universalargument for comments on (possibly suboptimal) handling
     * of digits.  We are assuming ASCII is a subset of the multibyte
     * encoding.
     */
    if (lastchar < '0' || lastchar > '9')
	return 1;
#else
    /* allow metafied as well as ordinary digits */
    if ((lastchar & 0x7f) < '0' || (lastchar & 0x7f) > '9')
	return 1;
#endif

    if (!(zmod.flags & MOD_TMULT))
	zmod.tmult = 0;
    if (zmod.flags & MOD_NEG) {
	/* If we just had a negative argument, this is the digit, *
	 * rather than the -1 assumed by negargument()            */
	zmod.tmult = sign * (lastchar & 0xf);
	zmod.flags &= ~MOD_NEG;
    } else
	zmod.tmult = zmod.tmult * 10 + sign * (lastchar & 0xf);
    zmod.flags |= MOD_TMULT;
    prefixflag = 1;
    return 0;
}

/**/
int
negargument(UNUSED(char **args))
{
    if (zmod.flags & MOD_TMULT)
	return 1;
    zmod.tmult = -1;
    zmod.flags |= MOD_TMULT|MOD_NEG;
    prefixflag = 1;
    return 0;
}

/**/
int
universalargument(char **args)
{
    int digcnt = 0, pref = 0, minus = 1, gotk;
    if (*args) {
	zmod.mult = atoi(*args);
	zmod.flags |= MOD_MULT;
	return 0;
    }
    /*
     * TODO: this is quite tricky to do when trying to maintain
     * compatibility between the old input system and Unicode.
     * We don't know what follows the digits, so if we try to
     * read wide characters we may fail (e.g. we may come across an old
     * \M-style binding).
     *
     * If we assume individual bytes are either explicitly ASCII or
     * not (a la UTF-8), we get away with it; we can back up individual
     * bytes and everything will work.  We may want to relax this
     * assumption later.  ("Much later" - (C) Steven Singer,
     * CSR BlueCore firmware, ca. 2000.)
     *
     * Hence for now this remains byte-by-byte.
     */
    while ((gotk = getbyte(0)) != EOF) {
	if (gotk == '-' && !digcnt) {
	    minus = -1;
	    digcnt++;
	} else if (gotk >= '0' && gotk <= '9') {
	    pref = pref * 10 + (gotk & 0xf);
	    digcnt++;
	} else {
	    ungetbyte(gotk);
	    break;
	}
    }
    if (digcnt)
	zmod.tmult = minus * (pref ? pref : 1);
    else
	zmod.tmult *= 4;
    zmod.flags |= MOD_TMULT;
    prefixflag = 1;
    return 0;
}

/**/
int
copyprevword(UNUSED(char **args))
{
    int len, t0;

    for (t0 = zlecs - 1; t0 >= 0; t0--)
	if (iword(zleline[t0]))
	    break;
    for (; t0 >= 0; t0--)
	if (!iword(zleline[t0]))
	    break;
    if (t0)
	t0++;
    len = zlecs - t0;
    spaceinline(len);
    ZS_memcpy(zleline + zlecs, zleline + t0, len);
    zlecs += len;
    return 0;
}

/**/
int
copyprevshellword(UNUSED(char **args))
{
    LinkList l;
    LinkNode n;
    int i;
    unsigned char *p = NULL;

    if ((l = bufferwords(NULL, NULL, &i)))
        for (n = firstnode(l); n; incnode(n))
            if (!i--) {
                p = (unsigned char *)getdata(n);
                break;
            }

    if (p) {
	int len;
	ZLE_STRING_T lineadd = stringaszleline(p, &len, NULL);

	spaceinline(len);
	ZS_memcpy(zleline + zlecs, lineadd, len);
	zlecs += len;

	free(lineadd);
    }
    return 0;
}

/**/
int
sendbreak(UNUSED(char **args))
{
    errflag = 1;
    return 1;
}

/**/
int
quoteregion(UNUSED(char **args))
{
    ZLE_STRING_T str;
    size_t len;

    if (mark > zlell)
	mark = zlell;
    if (mark < zlecs) {
	int tmp = mark;
	mark = zlecs;
	zlecs = tmp;
    }
    str = (ZLE_STRING_T)hcalloc((len = mark - zlecs) * ZLE_CHAR_SIZE);
    ZS_memcpy(str, zleline + zlecs, len);
    foredel(len);
    str = makequote(str, &len);
    spaceinline(len);
    ZS_memcpy(zleline + zlecs, str, len);
    mark = zlecs;
    zlecs += len;
    return 0;
}

/**/
int
quoteline(UNUSED(char **args))
{
    ZLE_STRING_T str;
    size_t len = zlell;

    str = makequote(zleline, &len);
    sizeline(len);
    ZS_memcpy(zleline, str, len);
    zlecs = zlell = len;
    return 0;
}

/**/
static ZLE_STRING_T
makequote(ZLE_STRING_T str, size_t *len)
{
    int qtct = 0;
    ZLE_STRING_T l, ol;
    ZLE_STRING_T end = str + *len;

    for (l = str; l < end; l++)
	if (*l == ZWC('\''))
	    qtct++;
    *len += 2 + qtct*3;
    l = ol = (char *)zhalloc(*len * ZLE_CHAR_SIZE);
    *l++ = ZWC('\'');
    for (; str < end; str++)
	if (*str == ZWC('\'')) {
	    *l++ = ZWC('\'');
	    *l++ = ZWC('\\');
	    *l++ = ZWC('\'');
	    *l++ = ZWC('\'');
	} else
	    *l++ = *str;
    *l++ = ZWC('\'');
    return ol;
}

/*
 * cmdstr is the buffer used for execute-named-command converted
 * to a metafied multibyte string.
 */
static char *namedcmdstr;
static LinkList namedcmdll;
static int namedcmdambig;

/**/
static void
scancompcmd(HashNode hn, UNUSED(int flags))
{
    int l;
    Thingy t = (Thingy) hn;

    if(strpfx(namedcmdstr, t->nam)) {
	addlinknode(namedcmdll, t->nam);
	l = pfxlen(peekfirst(namedcmdll), t->nam);
	if (l < namedcmdambig)
	    namedcmdambig = l;
    }

}

#define NAMLEN 60

/**/
Thingy
executenamedcommand(char *prmt)
{
    Thingy cmd;
    int l, len, feep = 0, listed = 0, curlist = 0;
    int ols = (listshown && validlist), olll = lastlistlen;
    ZLE_STRING_T cmdbuf, ptr, zprmt;
    char *okeymap = ztrdup(curkeymapname);

    clearlist = 1;
    /* prmt may be constant */
    prmt = ztrdup(prmt);
    zprmt = stringaszleline((unsigned char *)prmt, &l, NULL);
    cmdbuf = zhalloc((l + NAMLEN + 2) * ZLE_CHAR_SIZE);
    ZS_memcpy(cmdbuf, zprmt, l);
    free(zprmt);
    zsfree(prmt);
    statusline = cmdbuf;
    selectkeymap("main", 1);
    ptr = cmdbuf += l;
    len = 0;
    for (;;) {
	*ptr = ZWC('_');
	statusll = l + len + 1;
	zrefresh();
	if (!(cmd = getkeycmd()) || cmd == Th(z_sendbreak)) {
	    statusline = NULL;
	    selectkeymap(okeymap, 1);
	    zsfree(okeymap);
	    if ((listshown = ols)) {
		showinglist = -2;
		lastlistlen = olll;
	    } else if (listed)
		clearlist = listshown = 1;

	    return NULL;
	}
	if(cmd == Th(z_clearscreen)) {
	    clearscreen(zlenoargs);
	    if (curlist) {
		int zmultsav = zmult;

		zmult = 1;
		listlist(namedcmdll);
		showinglist = 0;
		zmult = zmultsav;
	    }
	} else if(cmd == Th(z_redisplay)) {
	    redisplay(zlenoargs);
	    if (curlist) {
		int zmultsav = zmult;

		zmult = 1;
		listlist(namedcmdll);
		showinglist = 0;
		zmult = zmultsav;
	    }
	} else if(cmd == Th(z_viquotedinsert)) {
	    *ptr = ZWC('^');
	    zrefresh();
	    getfullchar(0);
	    if(LASTFULLCHAR == ZLEEOF || !LASTFULLCHAR || len == NAMLEN)
		feep = 1;
	    else {
		*ptr++ = LASTFULLCHAR, len++, curlist = 0;
	    }
	} else if(cmd == Th(z_quotedinsert)) {
	    if(getfullchar(0) == ZLEEOF ||
	       !LASTFULLCHAR || len == NAMLEN)
		feep = 1;
	    else {
		*ptr++ = LASTFULLCHAR, len++, curlist = 0;
	    }
	} else if(cmd == Th(z_backwarddeletechar) ||
	    	cmd == Th(z_vibackwarddeletechar)) {
	    if (len) {
		len--, ptr--, curlist = 0;
	    }
	} else if(cmd == Th(z_killregion) || cmd == Th(z_backwardkillword) ||
		  cmd == Th(z_vibackwardkillword)) {
	    if (len)
		curlist = 0;
	    while (len && (len--, *--ptr != ZWC('-')));
	} else if(cmd == Th(z_killwholeline) || cmd == Th(z_vikillline) ||
	    	cmd == Th(z_backwardkillline)) {
	    len = 0;
	    ptr = cmdbuf;
	    if (listed)
		clearlist = listshown = 1;
	    curlist = 0;
	} else {
	    if(cmd == Th(z_acceptline) || cmd == Th(z_vicmdmode)) {
		Thingy r;
		unambiguous:
		*ptr = 0;
		namedcmdstr = zlelineasstring(cmdbuf, len, 0, NULL, NULL, 0);
		r = rthingy(namedcmdstr);
		free(namedcmdstr);
		namedcmdstr = NULL;
		if (!(r->flags & DISABLED)) {
		    unrefthingy(r);
		    statusline = NULL;
		    selectkeymap(okeymap, 1);
		    zsfree(okeymap);
		    if ((listshown = ols)) {
			showinglist = -2;
			lastlistlen = olll;
		    } else if (listed)
			clearlist = listshown = 1;
		    return r;
		}
		unrefthingy(r);
	    }
	    if(cmd == Th(z_selfinsertunmeta)) {
		fixunmeta();
		cmd = Th(z_selfinsert);
	    }
	    if (cmd == Th(z_listchoices) || cmd == Th(z_deletecharorlist) ||
		cmd == Th(z_expandorcomplete) || cmd == Th(z_completeword) ||
		cmd == Th(z_expandorcompleteprefix) || cmd == Th(z_vicmdmode) ||
		cmd == Th(z_acceptline) || lastchar == ' ' || lastchar == '\t') {
		namedcmdambig = 100;

		namedcmdll = newlinklist();

		namedcmdstr = zlelineasstring(cmdbuf, len, 0, NULL, NULL, 0);
		scanhashtable(thingytab, 1, 0, DISABLED, scancompcmd, 0);
		free(namedcmdstr);
		namedcmdstr = NULL;

		if (empty(namedcmdll)) {
		    feep = 1;
		    if (listed)
			clearlist = listshown = 1;
		    curlist = 0;
		} else if (cmd == Th(z_listchoices) ||
		    cmd == Th(z_deletecharorlist)) {
		    int zmultsav = zmult;
		    *ptr = ZWC('_');
		    statusll = l + len + 1;
		    zmult = 1;
		    listlist(namedcmdll);
		    listed = curlist = 1;
		    showinglist = 0;
		    zmult = zmultsav;
		} else if (!nextnode(firstnode(namedcmdll))) {
		    ZLE_STRING_T ztmp = stringaszleline(peekfirst(namedcmdll),
							&len, NULL);
		    ZS_memcpy(ptr = cmdbuf, ztmp, len);
		    ptr += len;
		    free(ztmp);
		    if(cmd == Th(z_acceptline) || cmd == Th(z_vicmdmode))
			goto unambiguous;
		} else {
		    int ltmp;
		    ZLE_STRING_T ztmp = stringaszleline(peekfirst(namedcmdll),
							&ltmp, NULL);
		    ZS_memcpy(cmdbuf, ztmp, ltmp);
		    free(ztmp);
		    ptr = cmdbuf + namedcmdambig;
		    *ptr = ZWC('_');
		    if (isset(AUTOLIST) &&
			!(isset(LISTAMBIGUOUS) && namedcmdambig > len)) {
			int zmultsav = zmult;
			if (isset(LISTBEEP))
			    feep = 1;
			statusll = l + namedcmdambig + 1;
			zmult = 1;
			listlist(namedcmdll);
			listed = curlist = 1;
			showinglist = 0;
			zmult = zmultsav;
		    }
		    len = namedcmdambig;
		}
	    } else {
		if (len == NAMLEN || cmd != Th(z_selfinsert))
		    feep = 1;
		else {
#ifdef ZLE_UNICODE_SUPPORT
		    if (!lastchar_wide_valid)
			getrestchar(0);
		    if (iswcntrl(lastchar_wide))
#else
		    if (icntrl(lastchar))
#endif
		    {
			feep = 1;
		    }
		    else {
			*ptr++ = LASTFULLCHAR, len++, curlist = 0;
		    }
		}
	    }
	}
	if (feep)
	    handlefeep(zlenoargs);
	feep = 0;
    }
}

/*****************/
/* Suffix system */
/*****************/

/*
 * The completion system sometimes tentatively adds a suffix to a word,
 * which can be removed depending on what is inserted next.  These
 * functions provide the capability to handle a removable suffix.
 *
 * Any removable suffix consists of characters immediately before the
 * cursor.  Whether it is removed depends on the next editing action.
 * There can be more than one suffix simultaneously present, with
 * different actions deleting different numbers of characters.
 *
 * If the next editing action changes the buffer other than by inserting
 * characters, normally the suffix should be removed so as to leave a
 * meaningful complete word.  The behaviour should be the same if the
 * next character inserted is a word separator.  If the next character
 * reasonably belongs where it is typed, or if the next editing action
 * is a deletion, the suffix should not be removed.  Other reasons for
 * suffix removal may have other behaviour.
 *
 * In order to maintain a consistent state, after a suffix has been added
 * the table *must* be zeroed, one way or another, before the buffer is
 * changed.  If the suffix is not being removed, call fixsuffix() to
 * indicate that it is being permanently fixed.
 */

/* Length of suffix to remove when inserting each possible character value.  *
 * suffixlen[256] is the length to remove for non-insertion editing actions. */

/*
 * TODO: Aargh, this is completely broken with wide characters.
 */
/**/
mod_export int suffixlen[257];

/* Shell function to call to remove the suffix. */

/**/
static char *suffixfunc;

/* Set up suffix: the last n characters are a suffix that should be *
 * removed in the usual word end conditions.                        */

/**/
mod_export void
makesuffix(int n)
{
    suffixlen[256] = suffixlen[' '] = suffixlen['\t'] = suffixlen['\n'] = 
	suffixlen[';'] = suffixlen['&'] = suffixlen['|'] = n;
}

/* Set up suffix for parameter names: the last n characters are a suffix *
 * that should be removed if the next character is one of the ones that  *
 * needs to go immediately after the parameter name.  br indicates that  *
 * the name is in braces (${PATH} instead of $PATH), so the extra        *
 * characters that can only be used in braces are included.              */

/**/
mod_export void
makeparamsuffix(int br, int n)
{
    if(br || unset(KSHARRAYS))
	suffixlen[':'] = suffixlen['['] = n;
    if(br) {
	suffixlen['#'] = suffixlen['%'] = suffixlen['?'] = n;
	suffixlen['-'] = suffixlen['+'] = suffixlen['='] = n;
	/*{*/ suffixlen['}'] = suffixlen['/'] = n;
    }
}

/* Set up suffix given a string containing the characters on which to   *
 * remove the suffix. */

/**/
mod_export void
makesuffixstr(char *f, char *s, int n)
{
    if (f) {
	zsfree(suffixfunc);
	suffixfunc = ztrdup(f);
	suffixlen[0] = n;
    } else if (s) {
	int inv, i, v, z = 0;

	if (*s == '^' || *s == '!') {
	    inv = 1;
	    s++;
	} else
	    inv = 0;
	s = getkeystring(s, &i, 5, &z);
	s = metafy(s, i, META_USEHEAP);

	if (inv) {
	    v = 0;
	    for (i = 0; i < 257; i++)
		 suffixlen[i] = n;
	} else
	    v = n;

	if (z)
	    suffixlen[256] = v;

	while (*s) {
	    if (s[1] == '-' && s[2]) {
		int b = (int) *s, e = (int) s[2];

		while (b <= e)
		    suffixlen[b++] = v;
		s += 2;
	    } else
		suffixlen[STOUC(*s)] = v;
	    s++;
	}
    } else
	makesuffix(n);
}

/* Remove suffix, if there is one, when inserting character c. */

/**/
mod_export void
iremovesuffix(ZLE_CHAR_T c, int keep)
{
    if (suffixfunc) {
	Eprog prog = getshfunc(suffixfunc);

	if (prog != &dummy_eprog) {
	    LinkList args = newlinklist();
	    char buf[20];
	    int osc = sfcontext;

	    sprintf(buf, "%d", suffixlen[0]);
	    addlinknode(args, suffixfunc);
	    addlinknode(args, buf);

	    startparamscope();
	    makezleparams(0);
	    sfcontext = SFC_COMPLETE;
	    doshfunc(suffixfunc, prog, args, 0, 1);
	    sfcontext = osc;
	    endparamscope();
	}
	zsfree(suffixfunc);
	suffixfunc = NULL;
    } else {
#ifdef ZLE_UNICODE_SUPPORT
	/* TODO: best I can think of for now... */
	int sl = (unsigned int)c < 256 ? suffixlen[c] : 0;
#else
	int sl = suffixlen[c];
#endif
	if(sl) {
	    backdel(sl);
	    if (!keep)
		invalidatelist();
	}
    }
    fixsuffix();
}

/* Fix the suffix in place, if there is one, making it non-removable. */

/**/
mod_export void
fixsuffix(void)
{
    memset(suffixlen, 0, sizeof(suffixlen));
}
