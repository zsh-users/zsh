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
char *lastline;
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
	zleline = (ZLE_STRING_T)realloc(zleline, (linesz *= 4) + 2);
}

/*
 * Insert a character, called from main shell.
 *
 * WCHAR: type is wrong, should be a genuine wide character,
 * when available in the caller.
 */

/**/
mod_export void
zleaddtoline(int chr)
{
    spaceinline(1);
    zleline[zlecs++] = chr;
}

/**/
mod_export unsigned char *
zlegetline(int *ll, int *cs)
{
    *ll = zlell;
    *cs = zlecs;

    return zleline;
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
    zleline[zlell] = '\0';

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
    zleline[zlell = to] = '\0';
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
	    zfree(b->buf, b->len);
	    b->buf = (char *)zalloc(ct);
	    memcpy(b->buf, (char *) zleline + i, ct);
	    b->len = ct;
	    b->flags = vilinerange ? CUTBUFFER_LINE : 0;
	} else {
	    int len = b->len;

	    if(vilinerange)
		b->flags |= CUTBUFFER_LINE;
	    b->buf = realloc(b->buf, ct + len + !!(b->flags & CUTBUFFER_LINE));
	    if (b->flags & CUTBUFFER_LINE)
		b->buf[len++] = '\n';
	    memcpy(b->buf + len, (char *) zleline + i, ct);
	    b->len = len + ct;
	}
	return;
    } else {
	/* Save in "1, shifting "1-"8 along to "2-"9 */
	int n;
	zfree(vibuf[34].buf, vibuf[34].len);
	for(n=34; n>26; n--)
	    vibuf[n] = vibuf[n-1];
	vibuf[26].buf = (char *)zalloc(ct);
	memcpy(vibuf[26].buf, (char *) zleline + i, ct);
	vibuf[26].len = ct;
	vibuf[26].flags = vilinerange ? CUTBUFFER_LINE : 0;
    }
    if (!cutbuf.buf) {
	cutbuf.buf = ztrdup("");
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
	    zfree(kptr->buf, kptr->len);
	*kptr = cutbuf;
	cutbuf.buf = ztrdup("");
	cutbuf.len = cutbuf.flags = 0;
    }
    if (dir) {
	char *s = (char *)zalloc(cutbuf.len + ct);

	memcpy(s, (char *) zleline + i, ct);
	memcpy(s + ct, cutbuf.buf, cutbuf.len);
	free(cutbuf.buf);
	cutbuf.buf = s;
	cutbuf.len += ct;
    } else {
	cutbuf.buf = realloc(cutbuf.buf, cutbuf.len + ct);
	memcpy(cutbuf.buf + cutbuf.len, (char *) zleline + i, ct);
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
setline(char const *s)
{
    sizeline(strlen(s));
    strcpy((char *) zleline, s);
    unmetafy((char *) zleline, &zlell);
    if ((zlecs = zlell) && invicmdmode())
	zlecs--;
}

/**/
int
findbol(void)
{
    int x = zlecs;

    while (x > 0 && zleline[x - 1] != '\n')
	x--;
    return x;
}

/**/
int
findeol(void)
{
    int x = zlecs;

    while (x != zlell && zleline[x] != '\n')
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

/* Search for needle in haystack.  Haystack is a metafied string while *
 * needle is unmetafied and len-long.  Start the search at position    *
 * pos.  Search forward if dir > 0 otherwise search backward.          */

/**/
char *
hstrnstr(char *haystack, int pos, char *needle, int len, int dir, int sens)
{
    char *s = haystack + pos;

    if (dir > 0) {
	while (*s) {
	    if (metadiffer(s, needle, len) < sens)
		return s;
	    s += 1 + (*s == Meta);
	}
    } else {
	for (;;) {
	    if (metadiffer(s, needle, len) < sens)
		return s;
	    if (s == haystack)
		break;
	    s -= 1 + (s != haystack+1 && s[-2] == Meta);
	}
    }
    return NULL;
}

/* Query the user, and return a single character response.  The *
 * question is assumed to have been printed already, and the    *
 * cursor is left immediately after the response echoed.        *
 * (Might cause a problem if this takes it onto the next line.) *
 * If yesno is non-zero:                                        *
 * <Tab> is interpreted as 'y'; any other control character is  *
 * interpreted as 'n'.  If there are any characters in the      *
 * buffer, this is taken as a negative response, and no         *
 * characters are read.  Case is folded.                        */

/**/
mod_export int
getzlequery(int yesno)
{
    int c;
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
    c = getkey(0);
    if (yesno) {
	if (c == '\t')
	    c = 'y';
	else if (icntrl(c) || c == EOF)
	    c = 'n';
	else
	    c = tulower(c);
    }
    /* echo response and return */
    if (c != '\n')
	putc(c, shout);
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
    lastline = zalloc(lastlinesz = linesz);
    memcpy(lastline, zleline, lastll = zlell);
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
	zsfree(p->del);
	zsfree(p->ins);
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
	zsfree(curchange->del);
	zsfree(curchange->ins);
	curchange->del = curchange->ins = NULL;
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

    if(lastll == zlell && !memcmp(lastline, zleline, zlell))
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
    if(suf + pre == lastll)
	ch->del = NULL;
    else
	ch->del = metafy(lastline + pre, lastll - pre - suf, META_DUP);
    if(suf + pre == zlell)
	ch->ins = NULL;
    else
	ch->ins = metafy((char *)zleline + pre, zlell - pre - suf, META_DUP);
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
	lastline = realloc(lastline, lastlinesz = linesz);
    memcpy(lastline, zleline, lastll = zlell);
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
	foredel(ztrlen(ch->ins));
    if(ch->del) {
	char *c = ch->del;

	spaceinline(ztrlen(c));
	for(; *c; c++)
	    if(*c == Meta)
		zleline[zlecs++] = STOUC(*++c) ^ 32;
	    else
		zleline[zlecs++] = STOUC(*c);
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
	foredel(ztrlen(ch->del));
    if(ch->ins) {
	char *c = ch->ins;

	spaceinline(ztrlen(c));
	for(; *c; c++)
	    if(*c == Meta)
		zleline[zlecs++] = STOUC(*++c) ^ 32;
	    else
		zleline[zlecs++] = STOUC(*c);
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
