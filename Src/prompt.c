/*
 * prompt.c - construct zsh prompts
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

#include "zsh.mdh"
#include "prompt.pro"

/* text attribute mask */

/**/
unsigned txtattrmask;

/* text change - attribute change made by prompts */

/**/
mod_export unsigned txtchange;

/* the command stack for use with %_ in prompts */

/**/
unsigned char *cmdstack;
/**/
int cmdsp;

/* parser states, for %_ */

static char *cmdnames[CS_COUNT] = {
    "for",      "while",     "repeat",    "select",
    "until",    "if",        "then",      "else",
    "elif",     "math",      "cond",      "cmdor",
    "cmdand",   "pipe",      "errpipe",   "foreach",
    "case",     "function",  "subsh",     "cursh",
    "array",    "quote",     "dquote",    "bquote",
    "cmdsubst", "mathsubst", "elif-then", "heredoc",
    "heredocd", "brace",     "braceparam", "always",
};

/* The buffer into which an expanded and metafied prompt is being written, *
 * and its size.                                                           */

static char *buf;
static int bufspc;

/* bp is the pointer to the current position in the buffer, where the next *
 * character will be added.                                                */

static char *bp;

/* Position of the start of the current line in the buffer */

static char *bufline;

/* bp1 is an auxiliary pointer into the buffer, which when non-NULL is *
 * moved whenever the buffer is reallocated.  It is used when data is   *
 * being temporarily held in the buffer.                                */

static char *bp1;

/* The format string, for %-expansion. */

static char *fm;

/* Non-zero if truncating the current segment of the buffer. */

static int truncwidth;

/* Current level of nesting of %{ / %} sequences. */

static int dontcount;

/* Level of %{ / %} surrounding a truncation segment. */

static int trunccount;

/* Strings to use for %r and %R (for the spelling prompt). */

static char *rstring, *Rstring;

/*
 * Expand path p; maximum is npath segments where 0 means the whole path.
 * If tilde is 1, try and find a named directory to use.
 */

static void
promptpath(char *p, int npath, int tilde)
{
    char *modp = p;
    Nameddir nd;

    if (tilde && ((nd = finddir(p))))
	modp = tricat("~", nd->node.nam, p + strlen(nd->dir));

    if (npath) {
	char *sptr;
	if (npath > 0) {
	    for (sptr = modp + strlen(modp); sptr > modp; sptr--) {
		if (*sptr == '/' && !--npath) {
		    sptr++;
		    break;
		}
	    }
	    if (*sptr == '/' && sptr[1] && sptr != modp)
		sptr++;
	    stradd(sptr);
	} else {
	    char cbu;
	    for (sptr = modp+1; *sptr; sptr++)
		if (*sptr == '/' && !++npath)
		    break;
	    cbu = *sptr;
	    *sptr = 0;
	    stradd(modp);
	    *sptr = cbu;
	}
    } else
	stradd(modp);

    if (p != modp)
	zsfree(modp);
}

/* Perform prompt expansion on a string, putting the result in a *
 * permanently-allocated string.  If ns is non-zero, this string *
 * may have embedded Inpar and Outpar, which indicate a toggling *
 * between spacing and non-spacing parts of the prompt, and      *
 * Nularg, which (in a non-spacing sequence) indicates a         *
 * `glitch' space.                                               */

/**/
mod_export char *
promptexpand(char *s, int ns, char *rs, char *Rs)
{
    if(!s)
	return ztrdup("");

    if ((termflags & TERM_UNKNOWN) && (unset(INTERACTIVE)))
        init_term();

    if (isset(PROMPTSUBST)) {
	int olderr = errflag;
	int oldval = lastval;

	s = dupstring(s);
	if (!parsestr(s))
	    singsub(&s);

	/* Ignore errors and status change in prompt substitution */
	errflag = olderr;
	lastval = oldval;
    }

    rstring = rs;
    Rstring = Rs;
    fm = s;
    bp = bufline = buf = zshcalloc(bufspc = 256);
    bp1 = NULL;
    truncwidth = 0;
    putpromptchar(1, '\0');
    addbufspc(1);
    if(dontcount)
	*bp++ = Outpar;
    *bp = 0;
    if (!ns) {
	/* If zero, Inpar, Outpar and Nularg should be removed. */
	for (bp = buf; *bp; ) {
	    if (*bp == Meta)
		bp += 2;
	    else if (*bp == Inpar || *bp == Outpar || *bp == Nularg)
		chuck(bp);
	    else
		bp++;
	}
    }
    return buf;
}

/* Perform %- and !-expansion as required on a section of the prompt.  The *
 * section is ended by an instance of endchar.  If doprint is 0, the valid *
 * % sequences are merely skipped over, and nothing is stored.             */

/**/
static int
putpromptchar(int doprint, int endchar)
{
    char *ss, *hostnam;
    int t0, arg, test, sep, j, numjobs;
    struct tm *tm;
    time_t timet;
    Nameddir nd;

    for (; *fm && *fm != endchar; fm++) {
	arg = 0;
	if (*fm == '%' && isset(PROMPTPERCENT)) {
	    int minus = 0;
	    fm++;
	    if (*fm == '-') {
		minus = 1;
		fm++;
	    }
	    if (idigit(*fm)) {
		arg = zstrtol(fm, &fm, 10);
		if (minus)
		    arg *= -1;
	    } else if (minus)
		arg = -1;
	    if (*fm == '(') {
		int tc, otruncwidth;

		if (idigit(*++fm)) {
		    arg = zstrtol(fm, &fm, 10);
		} else if (arg < 0) {
		    /* negative numbers don't make sense here */
		    arg *= -1;
		}
		test = 0;
		ss = pwd;
		switch (tc = *fm) {
		case 'c':
		case '.':
		case '~':
		    if ((nd = finddir(ss))) {
			arg--;
			ss += strlen(nd->dir);
		    } /*FALLTHROUGH*/
		case '/':
		case 'C':
		    /* `/' gives 0, `/any' gives 1, etc. */
		    if (*ss++ == '/' && *ss)
			arg--;
		    for (; *ss; ss++)
			if (*ss == '/')
			    arg--;
		    if (arg <= 0)
			test = 1;
		    break;
		case 't':
		case 'T':
		case 'd':
		case 'D':
		case 'w':
		    timet = time(NULL);
		    tm = localtime(&timet);
		    switch (tc) {
		    case 't':
			test = (arg == tm->tm_min);
			break;
		    case 'T':
			test = (arg == tm->tm_hour);
			break;
		    case 'd':
			test = (arg == tm->tm_mday);
			break;
		    case 'D':
			test = (arg == tm->tm_mon);
			break;
		    case 'w':
			test = (arg == tm->tm_wday);
			break;
		    }
		    break;
		case '?':
		    if (lastval == arg)
			test = 1;
		    break;
		case '#':
		    if (geteuid() == (uid_t)arg)
			test = 1;
		    break;
		case 'g':
		    if (getegid() == (gid_t)arg)
			test = 1;
		    break;
		case 'j':
		    for (numjobs = 0, j = 1; j <= maxjob; j++)
			if (jobtab[j].stat && jobtab[j].procs &&
		    	    !(jobtab[j].stat & STAT_NOPRINT)) numjobs++;
		    if (numjobs >= arg)
		    	test = 1;
		    break;
		case 'l':
		    *bp = '\0';
		    countprompt(bufline, &t0, 0, 0);
		    if (t0 >= arg)
			test = 1;
		    break;
		case 'L':
		    if (shlvl >= arg)
			test = 1;
		    break;
		case 'S':
		    if (time(NULL) - shtimer.tv_sec >= arg)
			test = 1;
		    break;
		case 'v':
		    if (arrlen(psvar) >= arg)
			test = 1;
		    break;
		case '_':
		    test = (cmdsp >= arg);
		    break;
		case '!':
		    test = privasserted();
		    break;
		default:
		    test = -1;
		    break;
		}
		if (!*fm || !(sep = *++fm))
		    return 0;
		fm++;
		/* Don't do the current truncation until we get back */
		otruncwidth = truncwidth;
		truncwidth = 0;
		if (!putpromptchar(test == 1 && doprint, sep) || !*++fm ||
		    !putpromptchar(test == 0 && doprint, ')')) {
		    truncwidth = otruncwidth;
		    return 0;
		}
		truncwidth = otruncwidth;
		continue;
	    }
	    if (!doprint)
		switch(*fm) {
		  case '[':
		    while(idigit(*++fm));
		    while(*++fm != ']');
		    continue;
		  case '<':
		    while(*++fm != '<');
		    continue;
		  case '>':
		    while(*++fm != '>');
		    continue;
		  case 'D':
		    if(fm[1]=='{')
			while(*++fm != '}');
		    continue;
		  default:
		    continue;
		}
	    switch (*fm) {
	    case '~':
		promptpath(pwd, arg, 1);
		break;
	    case 'd':
	    case '/':
		promptpath(pwd, arg, 0);
		break;
	    case 'c':
	    case '.':
		promptpath(pwd, arg ? arg : 1, 1);
		break;
	    case 'C':
		promptpath(pwd, arg ? arg : 1, 0);
		break;
	    case 'N':
		promptpath(scriptname ? scriptname : argzero, arg, 0);
		break;
	    case 'h':
	    case '!':
		addbufspc(DIGBUFSIZE);
		convbase(bp, curhist, 10);
		bp += strlen(bp);
		break;
	    case 'j':
		for (numjobs = 0, j = 1; j <= maxjob; j++)
		    if (jobtab[j].stat && jobtab[j].procs &&
		    	!(jobtab[j].stat & STAT_NOPRINT)) numjobs++;
		addbufspc(DIGBUFSIZE);
		sprintf(bp, "%d", numjobs);
		bp += strlen(bp);
		break;
	    case 'M':
		queue_signals();
		if ((hostnam = getsparam("HOST")))
		    stradd(hostnam);
		unqueue_signals();
		break;
	    case 'm':
		if (!arg)
		    arg++;
		queue_signals();
		if (!(hostnam = getsparam("HOST")))
		    break;
		if (arg < 0) {
		    for (ss = hostnam + strlen(hostnam); ss > hostnam; ss--)
			if (ss[-1] == '.' && !++arg)
			    break;
		    stradd(ss);
		} else {
		    for (ss = hostnam; *ss; ss++)
			if (*ss == '.' && !--arg)
			    break;
		    stradd(*ss ? dupstrpfx(hostnam, ss - hostnam) : hostnam);
		}
		unqueue_signals();
		break;
	    case 'S':
		txtchangeset(TXTSTANDOUT, TXTNOSTANDOUT);
		txtset(TXTSTANDOUT);
		tsetcap(TCSTANDOUTBEG, 1);
		break;
	    case 's':
		txtchangeset(TXTNOSTANDOUT, TXTSTANDOUT);
		txtset(TXTDIRTY);
		txtunset(TXTSTANDOUT);
		tsetcap(TCSTANDOUTEND, 1);
		break;
	    case 'B':
		txtchangeset(TXTBOLDFACE, TXTNOBOLDFACE);
		txtset(TXTDIRTY);
		txtset(TXTBOLDFACE);
		tsetcap(TCBOLDFACEBEG, 1);
		break;
	    case 'b':
		txtchangeset(TXTNOBOLDFACE, TXTBOLDFACE);
		txtchangeset(TXTNOSTANDOUT, TXTSTANDOUT);
		txtchangeset(TXTNOUNDERLINE, TXTUNDERLINE);
		txtset(TXTDIRTY);
		txtunset(TXTBOLDFACE);
		tsetcap(TCALLATTRSOFF, 1);
		break;
	    case 'U':
		txtchangeset(TXTUNDERLINE, TXTNOUNDERLINE);
		txtset(TXTUNDERLINE);
		tsetcap(TCUNDERLINEBEG, 1);
		break;
	    case 'u':
		txtchangeset(TXTNOUNDERLINE, TXTUNDERLINE);
		txtset(TXTDIRTY);
		txtunset(TXTUNDERLINE);
		tsetcap(TCUNDERLINEEND, 1);
		break;
	    case '[':
		if (idigit(*++fm))
		    arg = zstrtol(fm, &fm, 10);
		if (!prompttrunc(arg, ']', doprint, endchar))
		    return *fm;
		break;
	    case '<':
	    case '>':
		if (!prompttrunc(arg, *fm, doprint, endchar))
		    return *fm;
		break;
	    case '{': /*}*/
		if (!dontcount++) {
		    addbufspc(1);
		    *bp++ = Inpar;
		}
		break;
	    case /*{*/ '}':
		if (trunccount && trunccount >= dontcount)
		    return *fm;
		if (dontcount && !--dontcount) {
		    addbufspc(1);
		    *bp++ = Outpar;
		}
		break;
	    case 't':
	    case '@':
	    case 'T':
	    case '*':
	    case 'w':
	    case 'W':
	    case 'D':
		{
		    char *tmfmt, *dd, *tmbuf = NULL;

		    switch (*fm) {
		    case 'T':
			tmfmt = "%K:%M";
			break;
		    case '*':
			tmfmt = "%K:%M:%S";
			break;
		    case 'w':
			tmfmt = "%a %f";
			break;
		    case 'W':
			tmfmt = "%m/%d/%y";
			break;
		    case 'D':
			if (fm[1] == '{' /*}*/) {
			    for (ss = fm + 2; *ss && *ss != /*{*/ '}'; ss++)
				if(*ss == '\\' && ss[1])
				    ss++;
			    dd = tmfmt = tmbuf = zalloc(ss - fm);
			    for (ss = fm + 2; *ss && *ss != /*{*/ '}';
				 ss++) {
				if(*ss == '\\' && ss[1])
				    ss++;
				*dd++ = *ss;
			    }
			    *dd = 0;
			    fm = ss - !*ss;
			    if (!*tmfmt) {
				free(tmbuf);
				continue;
			    }
			} else
			    tmfmt = "%y-%m-%d";
			break;
		    default:
			tmfmt = "%l:%M%p";
			break;
		    }
		    timet = time(NULL);
		    tm = localtime(&timet);
		    /*
		     * Hack because strftime won't say how
		     * much space it actually needs.  Try to add it
		     * a few times until it works.  Some formats don't
		     * actually have a length, so we could go on for
		     * ever.
		     */
		    for(j = 0, t0 = strlen(tmfmt)*8; j < 3; j++, t0*=2) {
			addbufspc(t0);
			if (ztrftime(bp, t0, tmfmt, tm) >= 0)
			    break;
		    }
		    /* There is enough room for this because addbufspc(t0)
		     * allocates room for t0 * 2 bytes. */
		    metafy(bp, -1, META_NOALLOC);
		    bp += strlen(bp);
		    zsfree(tmbuf);
		    break;
		}
	    case 'n':
		stradd(get_username());
		break;
	    case 'l':
		if (*ttystrname) {
                   ss = (strncmp(ttystrname, "/dev/tty", 8) ?
                           ttystrname + 5 : ttystrname + 8);
		    stradd(ss);
		} else
		    stradd("()");
		break;
	    case 'y':
		if (*ttystrname) {
		    ss = (strncmp(ttystrname, "/dev/", 5) ?
			    ttystrname : ttystrname + 5);
		    stradd(ss);
		} else
		    stradd("()");
		break;
	    case 'L':
		addbufspc(DIGBUFSIZE);
		sprintf(bp, "%ld", (long)shlvl);
		bp += strlen(bp);
		break;
	    case '?':
		addbufspc(DIGBUFSIZE);
		sprintf(bp, "%ld", (long)lastval);
		bp += strlen(bp);
		break;
	    case '%':
	    case ')':
		addbufspc(1);
		*bp++ = *fm;
		break;
	    case '#':
		addbufspc(1);
		*bp++ = privasserted() ? '#' : '%';
		break;
	    case 'v':
		if (!arg)
		    arg = 1;
		else if (arg < 0)
		    arg += arrlen(psvar) + 1;
		if (arg > 0 && arrlen(psvar) >= arg)
		    stradd(psvar[arg - 1]);
		break;
	    case 'E':
                tsetcap(TCCLEAREOL, 1);
		break;
	    case '^':
		if (cmdsp) {
		    if (arg >= 0) {
			if (arg > cmdsp || arg == 0)
			    arg = cmdsp;
			for (t0 = cmdsp - 1; arg--; t0--) {
			    stradd(cmdnames[cmdstack[t0]]);
			    if (arg) {
				addbufspc(1);
				*bp++=' ';
			    }
			}
		    } else {
			arg = -arg;
			if (arg > cmdsp)
			    arg = cmdsp;
			for (t0 = arg - 1; arg--; t0--) {
			    stradd(cmdnames[cmdstack[t0]]);
			    if (arg) {
				addbufspc(1);
				*bp++=' ';
			    }
			}
		    }
		}
		break;
	    case '_':
		if (cmdsp) {
		    if (arg >= 0) {
			if (arg > cmdsp || arg == 0)
			    arg = cmdsp;
			for (t0 = cmdsp - arg; arg--; t0++) {
			    stradd(cmdnames[cmdstack[t0]]);
			    if (arg) {
				addbufspc(1);
				*bp++=' ';
			    }
			}
		    } else {
			arg = -arg;
			if (arg > cmdsp)
			    arg = cmdsp;
			for (t0 = 0; arg--; t0++) {
			    stradd(cmdnames[cmdstack[t0]]);
			    if (arg) {
				addbufspc(1);
				*bp++=' ';
			    }
			}
		    }
		}
		break;
	    case 'r':
		if(rstring)
		    stradd(rstring);
		break;
	    case 'R':
		if(Rstring)
		    stradd(Rstring);
		break;
	    case 'i':
		addbufspc(DIGBUFSIZE);
		sprintf(bp, "%ld", (long)lineno);
		bp += strlen(bp);
		break;
	    case '\0':
		return 0;
	    case Meta:
		fm++;
		break;
	    }
	} else if(*fm == '!' && isset(PROMPTBANG)) {
	    if(doprint) {
		if(fm[1] == '!') {
		    fm++;
		    addbufspc(1);
		    pputc('!');
		} else {
		    addbufspc(DIGBUFSIZE);
		    convbase(bp, curhist, 10);
		    bp += strlen(bp);
		}
	    }
	} else {
	    char c = *fm == Meta ? *++fm ^ 32 : *fm;

	    if (doprint) {
		addbufspc(1);
		pputc(c);
	    }
	}
    }

    return *fm;
}

/* pputc adds a character to the buffer, metafying.  There must *
 * already be space.                                            */

/**/
static void
pputc(char c)
{
    if (imeta(c)) {
	*bp++ = Meta;
	c ^= 32;
    }
    *bp++ = c;
    if (c == '\n' && !dontcount)
	bufline = bp;
}

/* Make sure there is room for `need' more characters in the buffer. */

/**/
static void
addbufspc(int need)
{
    need *= 2;   /* for metafication */
    if((bp - buf) + need > bufspc) {
	int bo = bp - buf;
	int bo1 = bp1 ? bp1 - buf : -1;

	if(need & 255)
	    need = (need | 255) + 1;
	buf = realloc(buf, bufspc += need);
	bp = buf + bo;
	if(bo1 != -1)
	    bp1 = buf + bo1;
    }
}

/* stradd() adds a metafied string to the prompt, *
 * in a visible representation.                   */

/**/
void
stradd(char *d)
{
#ifdef MULTIBYTE_SUPPORT
    char *ums, *ups;
    int upslen, eol = 0;
    mbstate_t mbs;

    memset(&mbs, 0, sizeof mbs);
    ums = ztrdup(d);
    ups = unmetafy(ums, &upslen);

    /*
     * We now have a raw string of possibly multibyte characters.
     * Read each character one by one.
     */
    while (upslen > 0) {
	wchar_t cc;
	char *pc;
	size_t cnt = eol ? MB_INVALID : mbrtowc(&cc, ups, upslen, &mbs);

	switch (cnt) {
	case MB_INCOMPLETE:
	    eol = 1;
	    /* FALL THROUGH */
	case MB_INVALID:
	    /* Bad character.  Take the next byte on its own. */
	    pc = nicechar(*ups);
	    cnt = 1;
	    memset(&mbs, 0, sizeof mbs);
	    break;
	case 0:
	    cnt = 1;
	    /* FALL THROUGH */
	default:
	    /* Take full wide character in one go */
	    mb_metacharinit();
	    pc = wcs_nicechar(cc, NULL, NULL);
	    break;
	}
	/* Keep output as metafied string. */
	addbufspc(strlen(pc));

	upslen -= cnt;
	ups += cnt;

	/* Put printed representation into the buffer */
	while (*pc)
	    *bp++ = *pc++;
    }

    free(ums);
#else
    char *ps, *pc;
    addbufspc(niceztrlen(d));
    /* This loop puts the nice representation of the string into the
     * prompt buffer. */
    for (ps = d; *ps; ps++) {
	for (pc = nicechar(*ps == Meta ? *++ps^32 : *ps); *pc; pc++)
	    *bp++ = *pc;
    }
#endif
}

/* tsetcap(), among other things, can write a termcap string into the buffer. */

/**/
mod_export void
tsetcap(int cap, int flag)
{
    if (tccan(cap) && !isset(SINGLELINEZLE) &&
        !(termflags & (TERM_NOUP|TERM_BAD|TERM_UNKNOWN))) {
	switch(flag) {
	case -1:
	    tputs(tcstr[cap], 1, putraw);
	    break;
	case 0:
	    tputs(tcstr[cap], 1, putshout);
	    break;
	case 1:
	    if (!dontcount) {
		addbufspc(1);
		*bp++ = Inpar;
	    }
	    tputs(tcstr[cap], 1, putstr);
	    if (!dontcount) {
		int glitch = 0;

		if (cap == TCSTANDOUTBEG || cap == TCSTANDOUTEND)
		    glitch = tgetnum("sg");
		else if (cap == TCUNDERLINEBEG || cap == TCUNDERLINEEND)
		    glitch = tgetnum("ug");
		if(glitch < 0)
		    glitch = 0;
		addbufspc(glitch + 1);
		while(glitch--)
		    *bp++ = Nularg;
		*bp++ = Outpar;
	    }
	    break;
	}

	if (txtisset(TXTDIRTY)) {
	    txtunset(TXTDIRTY);
	    if (txtisset(TXTBOLDFACE) && cap != TCBOLDFACEBEG)
		tsetcap(TCBOLDFACEBEG, flag);
	    if (txtisset(TXTSTANDOUT))
		tsetcap(TCSTANDOUTBEG, flag);
	    if (txtisset(TXTUNDERLINE))
		tsetcap(TCUNDERLINEBEG, flag);
	}
    }
}

/**/
int
putstr(int d)
{
    addbufspc(1);
    pputc(d);
    return 0;
}

/*
 * Count height etc. of a prompt string returned by promptexpand().
 * This depends on the current terminal width, and tabs and
 * newlines require nontrivial processing.
 * Passing `overf' as -1 means to ignore columns (absolute width).
 *
 * If multibyte is enabled, take account of multibyte characters
 * by locating them and finding out their screen width.
 */

/**/
mod_export void
countprompt(char *str, int *wp, int *hp, int overf)
{
    int w = 0, h = 1;
    int s = 1;
#ifdef MULTIBYTE_SUPPORT
    int wcw, multi = 0;
    char inchar;
    mbstate_t mbs;
    wchar_t wc;

    memset(&mbs, 0, sizeof(mbs));
#endif

    for (; *str; str++) {
	if (w >= columns && overf >= 0) {
	    w = 0;
	    h++;
	}
	/*
	 * Input string should be metafied, so tokens in it should
	 * be real tokens, even if there are multibyte characters.
	 */
	if (*str == Inpar)
	    s = 0;
	else if (*str == Outpar)
	    s = 1;
	else if (*str == Nularg)
	    w++;
	else if (s) {
	    if (*str == Meta) {
#ifdef MULTIBYTE_SUPPORT
		inchar = *++str ^ 32;
#else
		str++;
#endif
	    } else {
#ifdef MULTIBYTE_SUPPORT
		/*
		 * Don't look for tab or newline in the middle
		 * of a multibyte character.  Otherwise, we are
		 * relying on the character set being an extension
		 * of ASCII so it's safe to test a single byte.
		 */
		if (!multi) {
#endif
		    if (*str == '\t') {
			w = (w | 7) + 1;
			continue;
		    } else if (*str == '\n') {
			w = 0;
			h++;
			continue;
		    }
#ifdef MULTIBYTE_SUPPORT
		}

		inchar = *str;
#endif
	    }

#ifdef MULTIBYTE_SUPPORT
	    switch (mbrtowc(&wc, &inchar, 1, &mbs)) {
	    case MB_INCOMPLETE:
		/* Character is incomplete -- keep looking. */
		multi = 1;
		break;
	    case MB_INVALID:
		memset(&mbs, 0, sizeof mbs);
		/* FALL THROUGH */
	    case 0:
		/* Invalid character or null: assume no output. */
		multi = 0;
		break;
	    default:
		/*
		 * If the character isn't printable, wcwidth() returns
		 * -1.  We assume width 1.
		 */
		wcw = wcwidth(wc);
		if (wcw >= 0)
		    w += wcw;
		else
		    w++;
		multi = 0;
		break;
	    }
#else
	    w++;
#endif
	}
    }
    /*
     * multi may still be set if we were in the middle of the character.
     * This isn't easy to handle generally; just assume there's no
     * output.
     */
    if(w >= columns && overf >= 0) {
	if (!overf || w > columns) {
	    w = 0;
	    h++;
	}
    }
    if(wp)
	*wp = w;
    if(hp)
	*hp = h;
}

/**/
static int
prompttrunc(int arg, int truncchar, int doprint, int endchar)
{
    if (arg > 0) {
	char ch = *fm, *ptr, *truncstr;
	int truncatleft = ch == '<';
	int w = bp - buf;

	/*
	 * If there is already a truncation active, return so that
	 * can be finished, backing up so that the new truncation
	 * can be started afterwards.
	 */
	if (truncwidth) {
	    while (*--fm != '%')
		;
	    fm--;
	    return 0;
	}

	truncwidth = arg;
	if (*fm != ']')
	    fm++;
	while (*fm && *fm != truncchar) {
	    if (*fm == '\\' && fm[1])
		++fm;
	    addbufspc(1);
	    *bp++ = *fm++;
	}
	if (!*fm)
	    return 0;
	if (bp - buf == w && truncchar == ']') {
	    addbufspc(1);
	    *bp++ = '<';
	}
	ptr = buf + w;		/* addbufspc() may have realloc()'d buf */
	/*
	 * Now:
	 *   buf is the start of the output prompt buffer
	 *   ptr is the start of the truncation string
	 *   bp is the end of the truncation string
	 */
	truncstr = ztrduppfx(ptr, bp - ptr);

	bp = ptr;
	w = bp - buf;
	fm++;
	trunccount = dontcount;
	putpromptchar(doprint, endchar);
	trunccount = 0;
	ptr = buf + w;		/* putpromptchar() may have realloc()'d */
	*bp = '\0';
	/*
	 * Now:
	 *   ptr is the start of the truncation string and also
	 *     where we need to start putting any truncated output
	 *   bp is the end of the string we have just added, which
	 *     may need truncating.
	 */

	/*
	 * w below is screen width if multibyte support is enabled
	 * (note that above it was a raw string pointer difference).
	 * It's the full width of the string we may need to truncate.
	 *
	 * truncwidth has come from the user, so we interpret this
	 * as a screen width, too.
	 */
	countprompt(ptr, &w, 0, -1);
	if (w > truncwidth) {
	    /*
	     * We need to truncate.  t points to the truncation string
	     * -- which is inserted literally, without nice
	     * representation.  twidth is its printing width, and maxwidth
	     * is the amount of the main string that we want to keep.
	     * Note that if the truncation string is longer than the
	     * truncation length (twidth > truncwidth), the truncation
	     * string is used in full.
	     */
	    char *t = truncstr;
	    int fullen = bp - ptr;
	    int twidth, maxwidth;
	    int ntrunc = strlen(t);

	    twidth = MB_METASTRWIDTH(t);
	    if (twidth < truncwidth) {
		maxwidth = truncwidth - twidth;
		/*
		 * It's not safe to assume there are no invisible substrings
		 * just because the width is less than the full string
		 * length since there may be multibyte characters.
		 */
		addbufspc(ntrunc+1);
		/* may have realloc'd */
		ptr = bp - fullen;

		if (truncatleft) {
		    /*
		     * To truncate at the left, selectively copy
		     * maxwidth bytes from the main prompt, preceeded
		     * by the truncation string in full.
		     *
		     * We're overwriting the string containing the
		     * text to be truncated, so copy it.  We've
		     * just ensured there's sufficient space at the
		     * end of the prompt string.
		     *
		     * Pointer into text to be truncated.
		     */
		    char *fulltextptr, *fulltext;
		    int remw;
#ifdef MULTIBYTE_SUPPORT
		    mbstate_t mbs;
		    memset(&mbs, 0, sizeof mbs);
#endif

		    fulltextptr = fulltext = ptr + ntrunc;
		    memmove(fulltext, ptr, fullen);
		    fulltext[fullen] = '\0';

		    /* Copy the truncstr into place. */
		    while (*t)
			*ptr++ = *t++;

		    /*
		     * Find the point in the text at which we should
		     * start copying, i.e. when the remaining width
		     * is less than or equal to the maximum width.
		     */
		    remw = w;
		    while (remw > maxwidth && *fulltextptr) {
			if (*fulltextptr == Inpar) {
			    /*
			     * Text marked as invisible: copy
			     * regardless, since we don't know what
			     * this does but it shouldn't affect
			     * the width.
			     */
			    for (;;) {
				*ptr++ = *fulltextptr;
				if (*fulltextptr == Outpar ||
				    *fulltextptr == '\0')
				    break;
				fulltextptr++;
			    }
			} else {
#ifdef MULTIBYTE_SUPPORT
			    /*
			     * Normal text: build up a multibyte character.
			     */
			    char inchar;
			    wchar_t cc;
			    int wcw;

			    /*
			     * careful: string is still metafied (we
			     * need that because we don't know a
			     * priori when to stop and the resulting
			     * string must be metafied).
			     */
			    if (*fulltextptr == Meta)
				inchar = *++fulltextptr ^ 32;
			    else
				inchar = *fulltextptr;
			    fulltextptr++;
			    switch (mbrtowc(&cc, &inchar, 1, &mbs)) {
			    case MB_INCOMPLETE:
				/* Incomplete multibyte character. */
				break;
			    case MB_INVALID:
				/* Reset invalid state. */
				memset(&mbs, 0, sizeof mbs);
				/* FALL THROUGH */
			    case 0:
				/* Assume a single-byte character. */
				remw--;
				break;
			    default:
				wcw = wcwidth(cc);
				if (wcw >= 0)
				    remw -= wcw;
				else
				    remw--;
				break;
			    }
#else
			    /* Single byte character */
			    if (*fulltextptr == Meta)
				fulltextptr++;
			    fulltextptr++;
			    remw--;
#endif
			}
		    }

		    /*
		     * Now simply copy the rest of the text.  Still
		     * metafied, so this is easy.
		     */
		    while (*fulltextptr)
			*ptr++ = *fulltextptr++;
		    /* Mark the end of copying */
		    bp = ptr;
		} else {
		    /*
		     * Truncating at the right is easier: just leave
		     * enough characters until we have reached the
		     * maximum width.
		     */
		    char *skiptext = ptr;
#ifdef MULTIBYTE_SUPPORT
		    mbstate_t mbs;
		    memset(&mbs, 0, sizeof mbs);
#endif

		    while (maxwidth > 0 && *skiptext) {
			if (*skiptext == Inpar) {
			    for (; *skiptext != Outpar && *skiptext;
				 skiptext++);
			} else {
#ifdef MULTIBYTE_SUPPORT
			    char inchar;
			    wchar_t cc;
			    int wcw;

			    if (*skiptext == Meta)
				inchar = *++skiptext ^ 32;
			    else
				inchar = *skiptext;
			    skiptext++;
			    switch (mbrtowc(&cc, &inchar, 1, &mbs)) {
			    case MB_INCOMPLETE:
				/* Incomplete character. */
				break;
			    case MB_INVALID:
				/* Reset invalid state. */
				memset(&mbs, 0, sizeof mbs);
				/* FALL THROUGH */
			    case 0:
				/* Assume a single-byte character. */
				maxwidth--;
				break;
			    default:
				wcw = wcwidth(cc);
				if (wcw >= 0)
				    maxwidth -= wcw;
				else
				    maxwidth--;
				break;
			    }
#else
			    if (*skiptext == Meta)
				skiptext++;
			    skiptext++;
			    maxwidth--;
#endif
			}
		    }
		    /*
		     * We don't need the visible text from now on,
		     * but we'd better copy any invisible bits.
		     * History dictates that these go after the
		     * truncation string.  This is sensible since
		     * they may, for example, turn off an effect which
		     * should apply to all text at this point.
		     *
		     * Copy the truncstr.
		     */
		    ptr = skiptext;
		    while (*t)
			*ptr++ = *t++;
		    bp = ptr;
		    if (*skiptext) {
			/* Move remaining text so we don't overwrite it */
			memmove(bp, skiptext, strlen(skiptext)+1);
			skiptext = bp;

			/*
			 * Copy anything we want, updating bp
			 */
			while (*skiptext) {
			    if (*skiptext == Inpar) {
				for (;;) {
				    *bp++ = *skiptext;
				    if (*skiptext == Outpar ||
					*skiptext == '\0')
					break;
				    skiptext++;
				}
			    }
			    else
				skiptext++;
			}
		    }
		}
	    } else {
		/* Just copy truncstr; no other text appears. */
		while (*t)
		    *ptr++ = *t++;
		bp = ptr;
	    }
	    *bp = '\0';
	}
	zsfree(truncstr);
	truncwidth = 0;
	/*
	 * We may have returned early from the previous putpromptchar *
	 * because we found another truncation following this one.    *
	 * In that case we need to do the rest now.                   *
	 */
	if (!*fm)
	    return 0;
	if (*fm != endchar) {
	    fm++;
	    /*
	     * With truncwidth set to zero, we always reach endchar *
	     * (or the terminating NULL) this time round.         *
	     */
	    if (!putpromptchar(doprint, endchar))
		return 0;
	}
	/* Now we have to trick it into matching endchar again */
	fm--;
    } else {
	if (*fm != ']')
	    fm++;
	while(*fm && *fm != truncchar) {
	    if (*fm == '\\' && fm[1])
		fm++;
	    fm++;
	}
	if (truncwidth || !*fm)
	    return 0;
    }
    return 1;
}

/**/
void
cmdpush(int cmdtok)
{
    if (cmdsp >= 0 && cmdsp < CMDSTACKSZ)
	cmdstack[cmdsp++] = (unsigned char)cmdtok;
}

/**/
void
cmdpop(void)
{
    if (cmdsp <= 0) {
	DPUTS(1, "BUG: cmdstack empty");
	fflush(stderr);
    } else
	cmdsp--;
}
