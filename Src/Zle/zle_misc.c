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
doinsert(char *str)
{
    char *s;
    int len = ztrlen(str);
    int c1 = *str == Meta ? STOUC(str[1])^32 : STOUC(*str);/* first character */
    int neg = zmult < 0;             /* insert *after* the cursor? */
    int m = neg ? -zmult : zmult;    /* number of copies to insert */

    iremovesuffix(c1, 0);
    invalidatelist();

    if(insmode)
	spaceinline(m * len);
    else if(cs + m * len > ll)
	spaceinline(cs + m * len - ll);
    while(m--)
	for(s = str; *s; s++)
	    line[cs++] = *s == Meta ? *++s ^ 32 : *s;
    if(neg)
	cs += zmult * len;
}

/**/
mod_export int
selfinsert(char **args)
{
    char s[3], *p = s;

    if(imeta(c)) {
	*p++ = Meta;
	c ^= 32;
    }
    *p++ = c;
    *p = 0;
    doinsert(s);
    return 0;
}

/**/
int
selfinsertunmeta(char **args)
{
    c &= 0x7f;
    if (c == '\r')
	c = '\n';
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
    if (cs + zmult <= ll) {
	cs += zmult;
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
    backdel(zmult > cs ? cs : zmult);
    return 0;
}

/**/
int
killwholeline(char **args)
{
    int i, fg, n = zmult;

    if (n < 0)
	return 1;
    while (n--) {
	if ((fg = (cs && cs == ll)))
	    cs--;
	while (cs && line[cs - 1] != '\n')
	    cs--;
	for (i = cs; i != ll && line[i] != '\n'; i++);
	forekill(i - cs + (i != ll), fg);
    }
    clearlist = 1;
    return 0;
}

/**/
int
killbuffer(char **args)
{
    cs = 0;
    forekill(ll, 0);
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
	if (cs && line[cs - 1] == '\n')
	    cs--, i++;
	else
	    while (cs && line[cs - 1] != '\n')
		cs--, i++;
    }
    forekill(i, 1);
    clearlist = 1;
    return 0;
}

/**/
int
gosmacstransposechars(char **args)
{
    int cc;

    if (cs < 2 || line[cs - 1] == '\n' || line[cs - 2] == '\n') {
	if (cs == ll || line[cs] == '\n' ||
	    ((cs + 1 == ll || line[cs + 1] == '\n') &&
	     (!cs || line[cs - 1] == '\n'))) {
	    return 1;
	}
	cs += (cs == 0 || line[cs - 1] == '\n') ? 2 : 1;
    }
    cc = line[cs - 2];
    line[cs - 2] = line[cs - 1];
    line[cs - 1] = cc;
    return 0;
}

/**/
int
transposechars(char **args)
{
    int cc, ct;
    int n = zmult;
    int neg = n < 0;

    if (neg)
	n = -n;
    while (n--) {
	if (!(ct = cs) || line[cs - 1] == '\n') {
	    if (ll == cs || line[cs] == '\n')
		return 1;
	    if (!neg)
		cs++;
	    ct++;
	}
	if (neg) {
	    if (cs && line[cs - 1] != '\n') {
		cs--;
		if (ct > 1 && line[ct - 2] != '\n')
		    ct--;
	    }
	} else {
	    if (cs != ll && line[cs] != '\n')
		cs++;
	}
	if (ct == ll || line[ct] == '\n')
	    ct--;
	if (ct < 1 || line[ct - 1] == '\n')
	    return 1;
	cc = line[ct - 1];
	line[ct - 1] = line[ct];
	line[ct] = cc;
    }
    return 0;
}

/**/
int
poundinsert(char **args)
{
    cs = 0;
    vifirstnonblank(zlenoargs);
    if (line[cs] != '#') {
	spaceinline(1);
	line[cs] = '#';
	cs = findeol();
	while(cs != ll) {
	    cs++;
	    vifirstnonblank(zlenoargs);
	    spaceinline(1);
	    line[cs] = '#';
	    cs = findeol();
	}
    } else {
	foredel(1);
	cs = findeol();
	while(cs != ll) {
	    cs++;
	    vifirstnonblank(zlenoargs);
	    if(line[cs] == '#')
		foredel(1);
	    cs = findeol();
	}
    }
    done = 1;
    return 0;
}

/**/
int
acceptline(char **args)
{
    done = 1;
    return 0;
}

/**/
int
acceptandhold(char **args)
{
    zpushnode(bufstack, metafy((char *)line, ll, META_DUP));
    stackcs = cs;
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
	if (line[cs] == '\n')
	    cs++, i++;
	else
	    while (cs != ll && line[cs] != '\n')
		cs++, i++;
    }
    backkill(i, 0);
    clearlist = 1;
    return 0;
}

/**/
int
killregion(char **args)
{
    if (mark > ll)
	mark = ll;
    if (mark > cs)
	forekill(mark - cs, 0);
    else
	backkill(cs - mark, 1);
    return 0;
}

/**/
int
copyregionaskill(char **args)
{
    if (mark > ll)
	mark = ll;
    if (mark > cs)
	cut(cs, mark - cs, 0);
    else
	cut(mark, cs - mark, 1);
    return 0;
}

static int kct, yankb, yanke;

/**/
int
yank(char **args)
{
    Cutbuffer buf = &cutbuf;
    int n = zmult;

    if (n < 0)
	return 1;
    if (zmod.flags & MOD_VIBUF)
	buf = &vibuf[zmod.vibuf];
    if (!buf->buf)
	return 1;
    mark = cs;
    yankb = cs;
    while (n--) {
	kct = kringnum;
	spaceinline(buf->len);
	memcpy((char *)line + cs, buf->buf, buf->len);
	cs += buf->len;
	yanke = cs;
    }
    return 0;
}

/**/
int
yankpop(char **args)
{
    int cc;

    if (!(lastcmd & ZLE_YANK) || !kring[kct].buf)
	return 1;
    cs = yankb;
    foredel(yanke - yankb);
    cc = kring[kct].len;
    spaceinline(cc);
    memcpy((char *)line + cs, kring[kct].buf, cc);
    cs += cc;
    yanke = cs;
    kct = (kct + KRINGCT - 1) % KRINGCT;
    return 0;
}

/**/
int
overwritemode(char **args)
{
    insmode ^= 1;
    return 0;
}

/**/
int
whatcursorposition(char **args)
{
    char msg[100];
    char *s = msg;
    int bol = findbol();
    int c = STOUC(line[cs]);

    if (cs == ll)
	strucpy(&s, "EOF");
    else {
	strucpy(&s, "Char: ");
	switch (c) {
	case ' ':
	    strucpy(&s, "SPC");
	    break;
	case '\t':
	    strucpy(&s, "TAB");
	    break;
	case '\n':
	    strucpy(&s, "LFD");
	    break;
	default:
	    if (imeta(c)) {
		*s++ = Meta;
		*s++ = c ^ 32;
	    } else
		*s++ = c;
	}
	sprintf(s, " (0%o, %d, 0x%x)", c, c, c);
	s += strlen(s);
    }
    sprintf(s, "  point %d of %d(%d%%)  column %d", cs+1, ll+1,
	    ll ? 100 * cs / ll : 0, cs - bol);
    showmsg(msg);
    return 0;
}

/**/
int
undefinedkey(char **args)
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
    c = getkey(0);
#ifndef HAS_TIO
    zsetterm();
#endif
    if (c < 0)
	return 1;
    else
	return selfinsert(args);
}

/**/
int
digitargument(char **args)
{
    int sign = (zmult < 0) ? -1 : 1;

    /* allow metafied as well as ordinary digits */
    if ((c & 0x7f) < '0' || (c & 0x7f) > '9')
	return 1;

    if (!(zmod.flags & MOD_TMULT))
	zmod.tmult = 0;
    if (zmod.flags & MOD_NEG) {
	/* If we just had a negative argument, this is the digit, *
	 * rather than the -1 assumed by negargument()            */
	zmod.tmult = sign * (c & 0xf);
	zmod.flags &= ~MOD_NEG;
    } else
	zmod.tmult = zmod.tmult * 10 + sign * (c & 0xf);
    zmod.flags |= MOD_TMULT;
    prefixflag = 1;
    return 0;
}

/**/
int
negargument(char **args)
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
    while ((gotk = getkey(0)) != EOF) {
	if (gotk == '-' && !digcnt) {
	    minus = -1;
	    digcnt++;
	} else if (gotk >= '0' && gotk <= '9') {
	    pref = pref * 10 + (gotk & 0xf);
	    digcnt++;
	} else {
	    ungetkey(gotk);
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
copyprevword(char **args)
{
    int len, t0;

    for (t0 = cs - 1; t0 >= 0; t0--)
	if (iword(line[t0]))
	    break;
    for (; t0 >= 0; t0--)
	if (!iword(line[t0]))
	    break;
    if (t0)
	t0++;
    len = cs - t0;
    spaceinline(len);
    memcpy((char *)&line[cs], (char *)&line[t0], len);
    cs += len;
    return 0;
}

/**/
int
copyprevshellword(char **args)
{
    LinkList l;
    LinkNode n;
    int i;
    char *p = NULL;

    if ((l = bufferwords(NULL, NULL, &i)))
        for (n = firstnode(l); n; incnode(n))
            if (!i--) {
                p = getdata(n);
                break;
            }

    if (p) {
	int len = strlen(p);

	spaceinline(len);
	memcpy(line + cs, p, len);
	cs += len;
    }
    return 0;
}

/**/
int
sendbreak(char **args)
{
    errflag = 1;
    return 1;
}

/**/
int
quoteregion(char **args)
{
    char *str;
    size_t len;

    if (mark > ll)
	mark = ll;
    if (mark < cs) {
	int tmp = mark;
	mark = cs;
	cs = tmp;
    }
    str = (char *)hcalloc(len = mark - cs);
    memcpy(str, (char *)&line[cs], len);
    foredel(len);
    str = makequote(str, &len);
    spaceinline(len);
    memcpy((char *)&line[cs], str, len);
    mark = cs;
    cs += len;
    return 0;
}

/**/
int
quoteline(char **args)
{
    char *str;
    size_t len = ll;

    str = makequote((char *)line, &len);
    sizeline(len);
    memcpy(line, str, len);
    cs = ll = len;
    return 0;
}

/**/
static char *
makequote(char *str, size_t *len)
{
    int qtct = 0;
    char *l, *ol;
    char *end = str + *len;

    for (l = str; l < end; l++)
	if (*l == '\'')
	    qtct++;
    *len += 2 + qtct*3;
    l = ol = (char *)zhalloc(*len);
    *l++ = '\'';
    for (; str < end; str++)
	if (*str == '\'') {
	    *l++ = '\'';
	    *l++ = '\\';
	    *l++ = '\'';
	    *l++ = '\'';
	} else
	    *l++ = *str;
    *l++ = '\'';
    return ol;
}

static char *cmdbuf;
static LinkList cmdll;
static int cmdambig;

/**/
static void
scancompcmd(HashNode hn, int flags)
{
    int l;
    Thingy t = (Thingy) hn;

    if(strpfx(cmdbuf, t->nam)) {
	addlinknode(cmdll, t->nam);
	l = pfxlen(peekfirst(cmdll), t->nam);
	if (l < cmdambig)
	    cmdambig = l;
    }

}

#define NAMLEN 60

/**/
Thingy
executenamedcommand(char *prmt)
{
    Thingy cmd;
    int len, l = strlen(prmt), feep = 0, listed = 0, curlist = 0;
    int ols = (listshown && validlist), olll = lastlistlen;
    char *ptr;
    char *okeymap = curkeymapname;

    clearlist = 1;
    cmdbuf = zhalloc(l + NAMLEN + 2);
    strcpy(cmdbuf, prmt);
    statusline = cmdbuf;
    selectkeymap("main", 1);
    ptr = cmdbuf += l;
    len = 0;
    for (;;) {
	*ptr = '_';
	statusll = l + len + 1;
	zrefresh();
	if (!(cmd = getkeycmd()) || cmd == Th(z_sendbreak)) {
	    statusline = NULL;
	    selectkeymap(okeymap, 1);
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
		listlist(cmdll);
		showinglist = 0;
		zmult = zmultsav;
	    }
	} else if(cmd == Th(z_redisplay)) {
	    redisplay(zlenoargs);
	    if (curlist) {
		int zmultsav = zmult;

		zmult = 1;
		listlist(cmdll);
		showinglist = 0;
		zmult = zmultsav;
	    }
	} else if(cmd == Th(z_viquotedinsert)) {
	    *ptr = '^';
	    zrefresh();
	    c = getkey(0);
	    if(c == EOF || !c || len == NAMLEN)
		feep = 1;
	    else
		*ptr++ = c, len++, curlist = 0;
	} else if(cmd == Th(z_quotedinsert)) {
	    if((c = getkey(0)) == EOF || !c || len == NAMLEN)
		feep = 1;
	    else
		*ptr++ = c, len++, curlist = 0;
	} else if(cmd == Th(z_backwarddeletechar) ||
	    	cmd == Th(z_vibackwarddeletechar)) {
	    if (len)
		len--, ptr--, curlist = 0;
	} else if(cmd == Th(z_killregion) || cmd == Th(z_backwardkillword) ||
		  cmd == Th(z_vibackwardkillword)) {
	    if (len)
		curlist = 0;
	    while (len && (len--, *--ptr != '-'));
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
		r = rthingy(cmdbuf);
		if (!(r->flags & DISABLED)) {
		    unrefthingy(r);
		    statusline = NULL;
		    selectkeymap(okeymap, 1);
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
		c &= 0x7f;
		if(c == '\r')
		    c = '\n';
		cmd = Th(z_selfinsert);
	    }
	    if (cmd == Th(z_listchoices) || cmd == Th(z_deletecharorlist) ||
		cmd == Th(z_expandorcomplete) || cmd == Th(z_completeword) ||
		cmd == Th(z_expandorcompleteprefix) || cmd == Th(z_vicmdmode) ||
		cmd == Th(z_acceptline) || c == ' ' || c == '\t') {
		cmdambig = 100;

		cmdll = newlinklist();
		*ptr = 0;

		scanhashtable(thingytab, 1, 0, DISABLED, scancompcmd, 0);

		if (empty(cmdll)) {
		    feep = 1;
		    if (listed)
			clearlist = listshown = 1;
		    curlist = 0;
		} else if (cmd == Th(z_listchoices) ||
		    cmd == Th(z_deletecharorlist)) {
		    int zmultsav = zmult;
		    *ptr = '_';
		    statusll = l + len + 1;
		    zmult = 1;
		    listlist(cmdll);
		    listed = curlist = 1;
		    showinglist = 0;
		    zmult = zmultsav;
		} else if (!nextnode(firstnode(cmdll))) {
		    strcpy(ptr = cmdbuf, peekfirst(cmdll));
		    ptr += (len = strlen(ptr));
		    if(cmd == Th(z_acceptline) || cmd == Th(z_vicmdmode))
			goto unambiguous;
		} else {
		    strcpy(cmdbuf, peekfirst(cmdll));
		    ptr = cmdbuf + cmdambig;
		    *ptr = '_';
		    if (isset(AUTOLIST) &&
			!(isset(LISTAMBIGUOUS) && cmdambig > len)) {
			int zmultsav = zmult;
			if (isset(LISTBEEP))
			    feep = 1;
			statusll = l + cmdambig + 1;
			zmult = 1;
			listlist(cmdll);
			listed = curlist = 1;
			showinglist = 0;
			zmult = zmultsav;
		    }
		    len = cmdambig;
		}
	    } else {
		if (len == NAMLEN || icntrl(c) || cmd != Th(z_selfinsert))
		    feep = 1;
		else
		    *ptr++ = c, len++, curlist = 0;
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
iremovesuffix(int c, int keep)
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
	int sl = suffixlen[c];
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
