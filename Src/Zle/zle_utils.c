/*
 * zle_utils.c - miscellaneous line editor utilities
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
#include "zle_utils.pro"

/* Primary cut buffer */

/**/
struct cutbuffer cutbuf;

/* Emacs-style kill buffer ring */

/**/
struct cutbuffer *kring;
/**/
int kringsize, kringnum;

/* Vi named cut buffers.  0-25 are the named buffers "a to "z, and *
 * 26-34 are the numbered buffer stack "1 to "9.                   */

/**/
struct cutbuffer vibuf[35];

/* the line before last mod (for undo purposes) */

/**/
ZLE_STRING_T lastline;
/**/
int lastlinesz, lastll, lastcs;

/* size of line buffer */

/**/
int linesz;

/* make sure that the line buffer has at least sz chars */

/**/
void
sizeline(int sz)
{
    while (sz > linesz)
    {
	if (linesz < 256)
	    linesz = 256;
	else
	    linesz *= 4;

	zleline =
	    (ZLE_STRING_T)realloc(zleline,
				  (linesz + 2) * ZLE_CHAR_SIZE);
    }
}

/*
 * Insert a character, called from main shell.
 */

/**/
mod_export void
zleaddtoline(ZLE_CHAR_T chr)
{
    spaceinline(1);
    zleline[zlecs++] = chr;
}

/*
 * Input a line in internal zle format, possibly using wide characters,
 * possibly not, together with its length and the cursor position.
 * The length must be accurate and includes all characters (no NULL
 * termination is expected).  The input cursor position is only
 * significant if outcs is non-NULL.
 *
 * Output an ordinary NULL-terminated string, using multibyte characters
 * instead of wide characters where appropriate and with the contents
 * metafied.
 *
 * If outll is non-NULL, assign the new length.  If outcs is non-NULL,
 * assign the new character position.  This is the conventional string
 * length, without the NULL byte.
 *
 * If useheap is 1, memory is returned from the heap, else is allocated
 * for later freeing.
 */

/**/
mod_export unsigned char *
zlelineasstring(ZLE_STRING_T instr, int inll, int incs, int *outll,
		int *outcs, int useheap)
{
#ifdef ZLE_UNICODE_SUPPORT
    char *s;
    int i, j;
    size_t mb_len = 0;

    s = zalloc(inll * MB_CUR_MAX + 1);

    for(i=0; i < inll; i++, incs--) {
	if (outcs != NULL && incs == 0)
	    *outcs = mb_len;
	j = wctomb(s + mb_len, instr[i]);
	if (j == -1) {
	    /* invalid char; what to do? */
	} else {
	    mb_len += j;
	}
    }
    s[mb_len] = '\0';

    if (outll != NULL)
	*outll = mb_len;
    if (useheap)
    {
	unsigned char *ret =
	    (unsigned char *) metafy((char *) s, mb_len, META_HEAPDUP);

	zfree((char *)s, inll * MB_CUR_MAX + 1);

	return ret;
    }
    else
    {
	return (unsigned char *) metafy((char *) s, mb_len, META_REALLOC);
    }
#else
    if (outll != NULL)
	*outll = inll;
    if (outcs != NULL)
	*outcs = incs;

    return (unsigned char *) metafy((char *) instr, inll,
				    useheap ? META_HEAPDUP : META_DUP);
#endif
}


/*
 * Input a NULL-terminated metafied string instr.
 * Output a line in internal zle format, together with its length
 * in the appropriate character units.  Note that outll may not be NULL.
 *
 * If outsz is non-NULL, the number of allocated characters in the
 * string is written there.  For compatibility with use of the linesz
 * variable (allocate size of zleline), at least two characters are
 * allocated more than needed for immediate use.  (The extra characters
 * may take a newline and a null at a later stage.)  These are not
 * included in *outsz.
 *
 * Note that instr is modified in place, hence should be copied
 * first if necessary;
 *
 * Memory for the returned string is permanently allocated.  *outsz may
 * be longer than the *outll returned.  Hence it should be freed with
 * zfree(outstr, *outsz) or free(outstr), not zfree(outstr, *outll).
 */

/**/
mod_export ZLE_STRING_T
stringaszleline(unsigned char *instr, int *outll, int *outsz)
{
    ZLE_STRING_T outstr;
    int ll, sz;
#ifdef ZLE_UNICODE_SUPPORT
    int cll;
    mbstate_t ps;
#endif

    unmetafy(instr, &ll);

    /*
     * ll is the maximum number of characters there can be in
     * the output string; the closer to ASCII the string, the
     * better the guess.  For the 2 see above.
     */
    sz = (ll + 2) * ZLE_CHAR_SIZE;
    if (outsz)
	*outsz = ll;
    outstr = (ZLE_STRING_T)zalloc(sz);

#ifdef ZLE_UNICODE_SUPPORT
    if (ll) {
	char *inptr = (char *)instr;
	wchar_t *outptr = outstr;

	/* Reset shift state to input complete string */
	memset(&ps, '\0', sizeof(ps));

	while (ll) {
	    size_t ret = mbrtowc(outptr, inptr, ll, &ps);

	    /*
	     * At this point we don't handle either incomplete (-2) or
	     * invalid (-1) multibyte sequences.  Use the current length
	     * and return.
	     */
	    if (ret == (size_t)-1 || ret == (size_t)-2)
		break;

	    /*
	     * Careful: converting a wide NUL returns zero, but we
	     * want to treat NULs as regular characters.
	     * The NUL does get converted, however, so test that.
	     * Assume it was represented by a single ASCII NUL;
	     * certainly true for Unicode and unlikely to be false
	     * in any non-pathological multibyte representation.
	     */
	    if (*outptr == L'\0' && ret == 0)
		ret = 1;

	    inptr += ret;
	    outptr++;
	    ll -= ret;
	}
	*outll = outptr - outstr;
    }
    else
	*outll = 0;
#else
    memcpy((char *)outstr, (char *)instr, ll);
    *outll = ll;
#endif

    return outstr;
}



/**/
mod_export unsigned char *
zlegetline(int *ll, int *cs)
{
    return zlelineasstring(zleline, zlell, zlecs, ll, cs, 0);
}


/* insert space for ct chars at cursor position */

/**/
mod_export void
spaceinline(int ct)
{
    int i;

    sizeline(ct + zlell);
    for (i = zlell; --i >= zlecs;)
	zleline[i + ct] = zleline[i];
    zlell += ct;
    zleline[zlell] = ZWC('\0');

    if (mark > zlecs)
	mark += ct;
}

/**/
static void
shiftchars(int to, int cnt)
{
    if (mark >= to + cnt)
	mark -= cnt;
    else if (mark > to)
	mark = to;

    while (to + cnt < zlell) {
	zleline[to] = zleline[to + cnt];
	to++;
    }
    zleline[zlell = to] = ZWC('\0');
}

/**/
mod_export void
backkill(int ct, int dir)
{
    int i = (zlecs -= ct);

    cut(i, ct, dir);
    shiftchars(i, ct);
}

/**/
mod_export void
forekill(int ct, int dir)
{
    int i = zlecs;

    cut(i, ct, dir);
    shiftchars(i, ct);
}

/**/
void
cut(int i, int ct, int dir)
{
    if (!ct)
	return;

    if (zmod.flags & MOD_VIBUF) {
	struct cutbuffer *b = &vibuf[zmod.vibuf];

	if (!(zmod.flags & MOD_VIAPP) || !b->buf) {
	    free(b->buf);
	    b->buf = (ZLE_STRING_T)zalloc(ct * ZLE_CHAR_SIZE);
	    ZS_memcpy(b->buf, zleline + i, ct);
	    b->len = ct;
	    b->flags = vilinerange ? CUTBUFFER_LINE : 0;
	} else {
	    int len = b->len;

	    if(vilinerange)
		b->flags |= CUTBUFFER_LINE;
	    b->buf = (ZLE_STRING_T)
		realloc((char *)b->buf,
			(ct + len + !!(b->flags & CUTBUFFER_LINE))
			* ZLE_CHAR_SIZE);
	    if (b->flags & CUTBUFFER_LINE)
		b->buf[len++] = ZWC('\n');
	    ZS_memcpy(b->buf + len, zleline + i, ct);
	    b->len = len + ct;
	}
	return;
    } else {
	/* Save in "1, shifting "1-"8 along to "2-"9 */
	int n;
	free(vibuf[34].buf);
	for(n=34; n>26; n--)
	    vibuf[n] = vibuf[n-1];
	vibuf[26].buf = (ZLE_STRING_T)zalloc(ct * ZLE_CHAR_SIZE);
	ZS_memcpy(vibuf[26].buf, zleline + i, ct);
	vibuf[26].len = ct;
	vibuf[26].flags = vilinerange ? CUTBUFFER_LINE : 0;
    }
    if (!cutbuf.buf) {
	cutbuf.buf = (ZLE_STRING_T)zalloc(ZLE_CHAR_SIZE);
	cutbuf.buf[0] = ZWC('\0');
	cutbuf.len = cutbuf.flags = 0;
    } else if (!(lastcmd & ZLE_KILL)) {
	Cutbuffer kptr;
	if (!kring) {
	    kringsize = KRINGCTDEF;
	    kring = (Cutbuffer)zshcalloc(kringsize * sizeof(struct cutbuffer));
	} else
	    kringnum = (kringnum + 1) % kringsize;
	kptr = kring + kringnum;
	if (kptr->buf)
	    free(kptr->buf);
	*kptr = cutbuf;
	cutbuf.buf = (ZLE_STRING_T)zalloc(ZLE_CHAR_SIZE);
	cutbuf.buf[0] = ZWC('\0');
	cutbuf.len = cutbuf.flags = 0;
    }
    if (dir) {
	ZLE_STRING_T s = (ZLE_STRING_T)zalloc((cutbuf.len + ct)*ZLE_CHAR_SIZE);

	ZS_memcpy(s, zleline + i, ct);
	ZS_memcpy(s + ct, cutbuf.buf, cutbuf.len);
	free(cutbuf.buf);
	cutbuf.buf = s;
	cutbuf.len += ct;
    } else {
	cutbuf.buf = realloc((char *)cutbuf.buf,
			     (cutbuf.len + ct) * ZLE_CHAR_SIZE);
	ZS_memcpy(cutbuf.buf + cutbuf.len, zleline + i, ct);
	cutbuf.len += ct;
    }
    if(vilinerange)
	cutbuf.flags |= CUTBUFFER_LINE;
    else
	cutbuf.flags &= ~CUTBUFFER_LINE;
}

/**/
mod_export void
backdel(int ct)
{
    shiftchars(zlecs -= ct, ct);
}

/**/
mod_export void
foredel(int ct)
{
    shiftchars(zlecs, ct);
}

/**/
void
setline(char *s, int flags)
{
    char *scp;

    if (flags & ZSL_COPY)
	scp = ztrdup(s);
    else
	scp = s;
    /*
     * TBD: we could make this more efficient by passing the existing
     * allocated line to stringaszleline.
     */
    free(zleline);

    zleline = stringaszleline(scp, &zlell, &linesz);

    if ((flags & ZSL_TOEND) && (zlecs = zlell) && invicmdmode())
	zlecs--;
    else if (zlecs > zlell)
	zlecs = zlell;

    if (flags & ZSL_COPY)
	free(scp);
}

/**/
int
findbol(void)
{
    int x = zlecs;

    while (x > 0 && zleline[x - 1] != ZWC('\n'))
	x--;
    return x;
}

/**/
int
findeol(void)
{
    int x = zlecs;

    while (x != zlell && zleline[x] != ZWC('\n'))
	x++;
    return x;
}

/**/
void
findline(int *a, int *b)
{
    *a = findbol();
    *b = findeol();
}

/*
 * Return zero if the ZLE string histp length histl and the ZLE string
 * inputp length inputl are the same.  Return -1 if inputp is a prefix
 * of histp.  Return 1 if inputp is the lowercase version of histp.
 * Return 2 if inputp is the lowercase prefix of histp and return 3
 * otherwise.
 */

/**/
int
zlinecmp(ZLE_STRING_T histp, int histl, ZLE_STRING_T inputp, int inputl)
{
    int cnt;

    if (histl < inputl) {
	/* Not identical, second string is not a prefix. */
	return 3;
    }

    if (!ZS_memcmp(histp, inputp, inputl)) {
	/* Common prefix is identical */
	/* If lines are identical return 0 */
	if (histl == inputl)
	    return 0;
	/* Second string is a prefix of the first */
	return -1;
    }

    for (cnt = inputl; cnt; cnt--) {
	if (*inputp++ != ZC_tolower(*histp++))
	    return 3;
    }
    /* Is second string is lowercase version of first? */
    if (histl == inputl)
	return 1;
    /* Second string is lowercase prefix of first */
    return 2;
}


/*
 * Search for needle in haystack.  Haystack and needle are ZLE strings
 * of the indicated length.  Start the search at position
 * pos in haystack.  Search forward if dir > 0, otherwise search
 * backward.  sens is used to test against the return value of linecmp.
 */

/**/
ZLE_STRING_T
zlinefind(ZLE_STRING_T haystack, int haylen, int pos,
	  ZLE_STRING_T needle, int needlen, int dir, int sens)
{
    ZLE_STRING_T s = haystack + pos;
    int slen = haylen - pos;

    if (dir > 0) {
	while (slen) {
	    if (zlinecmp(s, slen, needle, needlen) < sens)
		return s;
	    s++;
	    slen--;
	}
    } else {
	for (;;) {
	    if (zlinecmp(s, slen, needle, needlen) < sens)
		return s;
	    if (s == haystack)
		break;
	    s--;
	    slen++;
	}
    }

    return NULL;
}

/*
 * Query the user, and return a single character response.  The question
 * is assumed to have been printed already, and the cursor is left
 * immediately after the response echoed.  (Might cause a problem if
 * this takes it onto the next line.)  If yesno is non-zero: <Tab> is
 * interpreted as 'y'; any other control character is interpreted as
 * 'n'.  If there are any characters in the buffer, this is taken as a
 * negative response, and no characters are read.  Case is folded.
 *
 * TBD: this may need extending to return a wchar_t or possibly
 * a wint_t.
 */

/**/
mod_export int
getzlequery(int yesno)
{
    ZLE_INT_T c;
#ifdef FIONREAD
    int val;

    if (yesno) {
	/* check for typeahead, which is treated as a negative response */
	ioctl(SHTTY, FIONREAD, (char *)&val);
	if (val) {
	    putc('n', shout);
	    return 'n';
	}
    }
#endif

    /* get a character from the tty and interpret it */
    c = getfullchar(0);
    if (yesno) {
	if (c == ZWC('\t'))
	    c = ZWC('y');
	else if (ZS_icntrl(c) || c == ZLEEOF)
	    c = ZWC('n');
	else
	    c = ZS_tolower(c);
    }
    /* echo response and return */
    if (c != ZWC('\n'))
	zwcputc(c);
    return c;
}

/* Format a string, keybinding style. */

/**/
char *
bindztrdup(char *str)
{
    int c, len = 1;
    char *buf, *ptr, *ret;

    for(ptr = str; *ptr; ptr++) {
	c = *ptr == Meta ? STOUC(*++ptr) ^ 32 : STOUC(*ptr);
	if(c & 0x80) {
	    len += 3;
	    c &= 0x7f;
	}
	if(c < 32 || c == 0x7f) {
	    len++;
	    c ^= 64;
	}
	len += c == '\\' || c == '^';
	len++;
    }
    ptr = buf = zalloc(len);
    for(; *str; str++) {
	c = *str == Meta ? STOUC(*++str) ^ 32 : STOUC(*str);
	if(c & 0x80) {
	    *ptr++ = '\\';
	    *ptr++ = 'M';
	    *ptr++ = '-';
	    c &= 0x7f;
	}
	if(c < 32 || c == 0x7f) {
	    *ptr++ = '^';
	    c ^= 64;
	}
	if(c == '\\' || c == '^')
	    *ptr++ = '\\';
	*ptr++ = c;
    }
    *ptr = 0;
    ret = dquotedztrdup(buf);
    zsfree(buf);
    return ret;
}

/* Display a metafied string, keybinding-style. */

/**/
int
printbind(char *str, FILE *stream)
{
    char *b = bindztrdup(str);
    int ret = zputs(b, stream);

    zsfree(b);
    return ret;
}

/* Display a message where the completion list normally goes. *
 * The message must be metafied.                              */

/**/
mod_export void
showmsg(char const *msg)
{
    char const *p;
    int up = 0, cc = 0, c;

    trashzle();
    clearflag = isset(USEZLE) && !termflags && isset(ALWAYSLASTPROMPT);

    for(p = msg; (c = *p); p++) {
	if(c == Meta)
	    c = *++p ^ 32;
	if(c == '\n') {
	    putc('\n', shout);
	    up += 1 + cc / columns;
	    cc = 0;
	} else {
	    char const *n = nicechar(c);
	    fputs(n, shout);
	    cc += strlen(n);
	}
    }
    up += cc / columns;

    if (clearflag) {
	putc('\r', shout);
	tcmultout(TCUP, TCMULTUP, up + nlnct);
    } else
	putc('\n', shout);
    showinglist = 0;
}

/* handle the error flag */

/**/
int
handlefeep(UNUSED(char **args))
{
    zbeep();
    return 0;
}

/***************/
/* undo system */
/***************/

/* head of the undo list, and the current position */

static struct change *changes, *curchange;

/* list of pending changes, not yet in the undo system */

static struct change *nextchanges, *endnextchanges;

/**/
void
initundo(void)
{
    nextchanges = NULL;
    changes = curchange = zalloc(sizeof(*curchange));
    curchange->prev = curchange->next = NULL;
    curchange->del = curchange->ins = NULL;
    curchange->dell = curchange->insl = 0;
    lastline = zalloc((lastlinesz = linesz) * ZLE_CHAR_SIZE);
    ZS_memcpy(lastline, zleline, (lastll = zlell));
    lastcs = zlecs;
}

/**/
void
freeundo(void)
{
    freechanges(changes);
    freechanges(nextchanges);
    zfree(lastline, lastlinesz);
}

/**/
static void
freechanges(struct change *p)
{
    struct change *n;

    for(; p; p = n) {
	n = p->next;
	free(p->del);
	free(p->ins);
	zfree(p, sizeof(*p));
    }
}

/* register pending changes in the undo system */

/**/
mod_export void
handleundo(void)
{
    mkundoent();
    if(!nextchanges)
	return;
    setlastline();
    if(curchange->next) {
	freechanges(curchange->next);
	curchange->next = NULL;
	free(curchange->del);
	free(curchange->ins);
	curchange->del = curchange->ins = NULL;
	curchange->dell = curchange->insl = 0;
    }
    nextchanges->prev = curchange->prev;
    if(curchange->prev)
	curchange->prev->next = nextchanges;
    else
	changes = nextchanges;
    curchange->prev = endnextchanges;
    endnextchanges->next = curchange;
    nextchanges = endnextchanges = NULL;
}

/* add an entry to the undo system, if anything has changed */

/**/
void
mkundoent(void)
{
    int pre, suf;
    int sh = zlell < lastll ? zlell : lastll;
    struct change *ch;

    if(lastll == zlell && !memcmp(lastline, zleline, zlell * ZLE_CHAR_SIZE))
	return;
    for(pre = 0; pre < sh && zleline[pre] == lastline[pre]; )
	pre++;
    for(suf = 0; suf < sh - pre &&
	zleline[zlell - 1 - suf] == lastline[lastll - 1 - suf]; )
	suf++;
    ch = zalloc(sizeof(*ch));
    ch->next = NULL;
    ch->hist = histline;
    ch->off = pre;
    ch->old_cs = lastcs;
    ch->new_cs = zlecs;
    if(suf + pre == lastll) {
	ch->del = NULL;
	ch->dell = 0;
    } else {
	ch->dell = lastll - pre - suf;
	ch->del = (ZLE_STRING_T)zalloc(ch->dell * ZLE_CHAR_SIZE);
	ZS_memcpy(ch->del, lastline + pre, ch->dell);
    }
    if(suf + pre == zlell) {
	ch->ins = NULL;
	ch->insl = 0;
    } else {
	ch->insl = zlell - pre - suf;
	ch->ins = (ZLE_STRING_T)zalloc(ch->insl * ZLE_CHAR_SIZE);
	ZS_memcpy(ch->ins, zleline + pre, ch->insl);
    }
    if(nextchanges) {
	ch->flags = CH_PREV;
	ch->prev = endnextchanges;
	endnextchanges->flags |= CH_NEXT;
	endnextchanges->next = ch;
    } else {
	nextchanges = ch;
	ch->flags = 0;
	ch->prev = NULL;
    }
    endnextchanges = ch;
}

/* set lastline to match line */

/**/
void
setlastline(void)
{
    if(lastlinesz != linesz)
	lastline = realloc(lastline, (lastlinesz = linesz) * ZLE_CHAR_SIZE);
    ZS_memcpy(lastline, zleline, (lastll = zlell));
    lastcs = zlecs;
}

/* move backwards through the change list */

/**/
int
undo(UNUSED(char **args))
{
    handleundo();
    do {
	if(!curchange->prev)
	    return 1;
	if (unapplychange(curchange->prev))
	    curchange = curchange->prev;
	else
	    break;
    } while(curchange->flags & CH_PREV);
    setlastline();
    return 0;
}

/**/
static int
unapplychange(struct change *ch)
{
    if(ch->hist != histline) {
	zle_setline(quietgethist(ch->hist));
	zlecs = ch->new_cs;
	return 0;
    }
    zlecs = ch->off;
    if(ch->ins)
	foredel(ch->insl);
    if(ch->del) {
	spaceinline(ch->dell);
	ZS_memcpy(zleline + zlecs, ch->del, ch->dell);
	zlecs += ch->dell;
    }
    zlecs = ch->old_cs;
    return 1;
}

/* move forwards through the change list */

/**/
int
redo(UNUSED(char **args))
{
    handleundo();
    do {
	if(!curchange->next)
	    return 1;
	if (applychange(curchange))
	    curchange = curchange->next;
	else
	    break;
    } while(curchange->prev->flags & CH_NEXT);
    setlastline();
    return 0;
}

/**/
static int
applychange(struct change *ch)
{
    if(ch->hist != histline) {
	zle_setline(quietgethist(ch->hist));
	zlecs = ch->old_cs;
	return 0;
    }
    zlecs = ch->off;
    if(ch->del)
	foredel(ch->dell);
    if(ch->ins) {
	spaceinline(ch->insl);
	ZS_memcpy(zleline + zlecs, ch->ins, ch->insl);
	zlecs += ch->insl;
    }
    zlecs = ch->new_cs;
    return 1;
}

/* vi undo: toggle between the end of the undo list and the preceding point */

/**/
int
viundochange(char **args)
{
    handleundo();
    if(curchange->next) {
	do {
	    applychange(curchange);
	    curchange = curchange->next;
	} while(curchange->next);
	setlastline();
	return 0;
    } else
	return undo(args);
}
