/*
 * zle_vi.c - vi-specific functions
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
#include "zle_vi.pro"

/* != 0 if we're getting a vi range */

/**/
int virangeflag;

/* kludge to get cw and dw to work right */

/**/
int wordflag;

/* != 0 if we're killing lines into a buffer, vi-style */

/**/
int vilinerange;

/* last vi change buffer, for vi change repetition */

/**/
int vichgbufsz, vichgbufptr, vichgflag;

/**/
char *vichgbuf;

/* point where vi insert mode was last entered */

/**/
int viinsbegin;

static struct modifier lastmod;
static int inrepeat, vichgrepeat;

/**/
static void
startvichange(int im)
{
    if (im != -1) {
	insmode = im;
	vichgflag = 1;
    }
    if (inrepeat) {
	zmod = lastmod;
	inrepeat = vichgflag = 0;
	vichgrepeat = 1;
    } else {
	lastmod = zmod;
	if (vichgbuf)
	    free(vichgbuf);
	vichgbuf = (char *)zalloc(vichgbufsz = 16);
	vichgbuf[0] = lastchar;
	vichgbufptr = 1;
	vichgrepeat = 0;
    }
}

/**/
static void
startvitext(int im)
{
    startvichange(im);
    selectkeymap("main", 1);
    undoing = 0;
    viinsbegin = cs;
}

/**/
int
vigetkey(void)
{
    Keymap mn = openkeymap("main");
    char m[3], *str;
    Thingy cmd;

    if((lastchar = getkey(0)) == EOF)
	return -1;

    m[0] = lastchar;
    metafy(m, 1, META_NOALLOC);
    if(mn)
	cmd = keybind(mn, m, &str);
    else
	cmd = t_undefinedkey;

    if (!cmd || cmd == Th(z_sendbreak)) {
	return -1;
    } else if (cmd == Th(z_quotedinsert)) {
	if ((lastchar = getkey(0)) == EOF)
	    return -1;
    } else if(cmd == Th(z_viquotedinsert)) {
	char sav = line[cs];

	line[cs] = '^';
	zrefresh();
	lastchar = getkey(0);
	line[cs] = sav;
	if(lastchar == EOF)
	    return -1;
    } else if (cmd == Th(z_vicmdmode))
	return -1;
    return lastchar;
}

/**/
static int
getvirange(int wf)
{
    int pos = cs, ret = 0;
    int mult1 = zmult, hist1 = histline;
    Thingy k2;

    virangeflag = 1;
    wordflag = wf;
    /* Now we need to execute the movement command, to see where it *
     * actually goes.  virangeflag here indicates to the movement   *
     * function that it should place the cursor at the end of the   *
     * range, rather than where the cursor would actually go if it  *
     * were executed normally.  This makes a difference to some     *
     * commands, but not all.  For example, if searching forward    *
     * for a character, under normal circumstances the cursor lands *
     * on the character.  For a range, the range must include the   *
     * character, so the cursor gets placed after the character if  *
     * virangeflag is set.  vi-match-bracket needs to change the    *
     * value of virangeflag under some circumstances, meaning that  *
     * we need to change the *starting* position.                   */
    zmod.flags &= ~MOD_TMULT;
    do {
	vilinerange = 0;
	prefixflag = 0;
	if (!(k2 = getkeycmd()) || (k2->flags & DISABLED) ||
		k2 == Th(z_sendbreak)) {
	    wordflag = 0;
	    virangeflag = 0;
	    return -1;
	}
	/*
	 * With k2 == bindk, the command key is repeated:
	 * a number of lines is used.  If the function used
	 * returns 1, we fail.
	 */
	if ((k2 == bindk) ? dovilinerange() : execzlefunc(k2, zlenoargs))
	    ret = -1;
	if(vichgrepeat)
	    zmult = mult1;
	else
	    zmult = mult1 * zmod.tmult;
    } while(prefixflag && !ret);
    wordflag = 0;
    virangeflag = 0;

    /* It is an error to use a non-movement command to delimit the *
     * range.  We here reject the case where the command modified  *
     * the line, or selected a different history line.             */
    if (histline != hist1 || ll != lastll || memcmp(line, lastline, ll)) {
	histline = hist1;
	memcpy(line, lastline, ll = lastll);
	cs = pos;
	return -1;
    }

    /* Can't handle an empty file.  Also, if the movement command *
     * failed, or didn't move, it is an error.                    */
    if (!ll || (cs == pos && virangeflag != 2) || ret == -1)
	return -1;

    /* vi-match-bracket changes the value of virangeflag when *
     * moving to the opening bracket, meaning that we need to *
     * change the *starting* position.                        */
    if(virangeflag == -1)
	pos++;

    /* Get the range the right way round.  cs is placed at the *
     * start of the range, and pos (the return value of this   *
     * function) is the end.                                   */
    if (cs > pos) {
	int tmp = cs;
	cs = pos;
	pos = tmp;
    }

    /* Was it a line-oriented move?  If so, the command will have set *
     * the vilinerange flag.  In this case, entire lines are taken,   *
     * rather than just the sequence of characters delimited by pos   *
     * and cs.  The terminating newline is left out of the range,     *
     * which the real command must deal with appropriately.  At this  *
     * point we just need to make the range encompass entire lines.   */
    if(vilinerange) {
	int newcs = findbol();
	cs = pos;
	pos = findeol();
	cs = newcs;
    }
    return pos;
}

/**/
static int
dovilinerange(void)
{
    int pos = cs, n = zmult;

    /* A number of lines is taken as the range.  The current line *
     * is included.  If the repeat count is positive the lines go *
     * downward, otherwise upward.  The repeat count gives the    *
     * number of lines.                                           */
    vilinerange = 1;
    if (!n)
	return 1;
    if (n > 0) {
	while(n-- && cs <= ll)
	    cs = findeol() + 1;
	if (n != -1) {
	    cs = pos;
	    return 1;
	}
	cs--;
    } else {
	while(n++ && cs >= 0)
	    cs = findbol() - 1;
	if (n != 1) {
	    cs = pos;
	    return 1;
	}
	cs++;
    }
    virangeflag = 2;
    return 0;
}

/**/
int
viaddnext(UNUSED(char **args))
{
    if (cs != findeol())
	cs++;
    startvitext(1);
    return 0;
}

/**/
int
viaddeol(UNUSED(char **args))
{
    cs = findeol();
    startvitext(1);
    return 0;
}

/**/
int
viinsert(UNUSED(char **args))
{
    startvitext(1);
    return 0;
}

/**/
int
viinsertbol(UNUSED(char **args))
{
    vifirstnonblank(zlenoargs);
    startvitext(1);
    return 0;
}

/**/
int
videlete(UNUSED(char **args))
{
    int c2, ret = 1;

    startvichange(1);
    if ((c2 = getvirange(0)) != -1) {
	forekill(c2 - cs, 0);
	ret = 0;
	if (vilinerange && ll) {
	    if (cs == ll)
		cs--;
	    foredel(1);
	    vifirstnonblank(zlenoargs);
	}
    }
    vichgflag = 0;
    return ret;
}

/**/
int
videletechar(char **args)
{
    int n = zmult;

    startvichange(-1);
    /* handle negative argument */
    if (n < 0) {
	int ret;
	zmult = -n;
	ret = vibackwarddeletechar(args);
	zmult = n;
	return ret;
    }
    /* it is an error to be on the end of line */
    if (cs == ll || line[cs] == '\n')
	return 1;
    /* Put argument into the acceptable range -- it is not an error to  *
     * specify a greater count than the number of available characters. */
    if (n > findeol() - cs)
	n = findeol() - cs;
    /* do the deletion */
    forekill(n, 0);
    return 0;
}

/**/
int
vichange(UNUSED(char **args))
{
    int c2, ret = 1;

    startvichange(1);
    if ((c2 = getvirange(1)) != -1) {
	ret = 0;
	forekill(c2 - cs, 0);
	selectkeymap("main", 1);
	viinsbegin = cs;
	undoing = 0;
    }
    return ret;
}

/**/
int
visubstitute(UNUSED(char **args))
{
    int n = zmult;

    startvichange(1);
    if (n < 0)
	return 1;
    /* it is an error to be on the end of line */
    if (cs == ll || line[cs] == '\n')
	return 1;
    /* Put argument into the acceptable range -- it is not an error to  *
     * specify a greater count than the number of available characters. */
    if (n > findeol() - cs)
	n = findeol() - cs;
    /* do the substitution */
    forekill(n, 0);
    startvitext(1);
    return 0;
}

/**/
int
vichangeeol(UNUSED(char **args))
{
    forekill(findeol() - cs, 0);
    startvitext(1);
    return 0;
}

/**/
int
vichangewholeline(char **args)
{
    vifirstnonblank(args);
    return vichangeeol(zlenoargs);
}

/**/
int
viyank(UNUSED(char **args))
{
    int oldcs = cs, c2, ret = 1;

    startvichange(1);
    if ((c2 = getvirange(0)) != -1) {
	cut(cs, c2 - cs, 0);
	ret = 0;
    }
    vichgflag = 0;
    cs = oldcs;
    return ret;
}

/**/
int
viyankeol(UNUSED(char **args))
{
    int x = findeol();

    startvichange(-1);
    if (x == cs)
	return 1;
    cut(cs, x - cs, 0);
    return 0;
}

/**/
int
viyankwholeline(UNUSED(char **args))
{
    int bol = findbol(), oldcs = cs;
    int n = zmult;

    startvichange(-1);
    if (n < 1)
	return 1;
    while(n--) {
     if (cs > ll) {
	cs = oldcs;
	return 1;
     }
     cs = findeol() + 1;
    }
    vilinerange = 1;
    cut(bol, cs - bol - 1, 0);
    cs = oldcs;
    return 0;
}

/**/
int
vireplace(UNUSED(char **args))
{
    startvitext(0);
    return 0;
}

/* vi-replace-chars has some oddities relating to vi-repeat-change.  In *
 * the real vi, if one does 3r at the end of a line, it feeps without   *
 * reading the argument, and won't repeat the action.  A successful rx  *
 * followed by 3. at the end of a line (or 3rx followed by . at the end *
 * of a line) will obviously feep after the ., even though it has the   *
 * argument available.  Here repeating is tied very closely to argument *
 * reading, so some trickery is needed to emulate this.  When repeating *
 * a change, we always read the argument normally, even if the count    *
 * was bad.  When recording a change for repeating, and a bad count is  *
 * given, we squash the repeat buffer to avoid repeating the partial    *
 * command; we've lost the previous change, but that can't be avoided   *
 * without a rewrite of the repeat code.                                */

/**/
int
vireplacechars(UNUSED(char **args))
{
    int ch, n = zmult;

    startvichange(1);
    /* check argument range */
    if (n < 1 || n + cs > findeol()) {
	if(vichgrepeat)
	    vigetkey();
	if(vichgflag) {
	    free(vichgbuf);
	    vichgbuf = NULL;
	    vichgflag = 0;
	}
	return 1;
    }
    /* get key */
    if((ch = vigetkey()) == -1) {
	vichgflag = 0;
	return 1;
    }
    /* do change */
    if (ch == '\r' || ch == '\n') {
	/* <return> handled specially */
	cs += n - 1;
	backkill(n - 1, 0);
	line[cs++] = '\n';
    } else {
	while (n--)
	    line[cs++] = ch;
	cs--;
    }
    vichgflag = 0;
    return 0;
}

/**/
int
vicmdmode(UNUSED(char **args))
{
    if (invicmdmode() || selectkeymap("vicmd", 0))
	return 1;
    undoing = 1;
    vichgflag = 0;
    if (cs != findbol())
	cs--;
    return 0;
}

/**/
int
viopenlinebelow(UNUSED(char **args))
{
    cs = findeol();
    spaceinline(1);
    line[cs++] = '\n';
    startvitext(1);
    clearlist = 1;
    return 0;
}

/**/
int
viopenlineabove(UNUSED(char **args))
{
    cs = findbol();
    spaceinline(1);
    line[cs] = '\n';
    startvitext(1);
    clearlist = 1;
    return 0;
}

/**/
int
vioperswapcase(UNUSED(char **args))
{
    int oldcs, c2, ret = 1;

    /* get the range */
    startvichange(1);
    if ((c2 = getvirange(0)) != -1) {
	oldcs = cs;
	/* swap the case of all letters within range */
	while (cs < c2) {
	    if (islower(line[cs]))
		line[cs] = tuupper(line[cs]);
	    else if (isupper(line[cs]))
		line[cs] = tulower(line[cs]);
	    cs++;
	}
	/* go back to the first line of the range */
	cs = oldcs;
	ret = 0;
#if 0
	vifirstnonblank();
#endif
    }
    vichgflag = 0;
    return ret;
}

/**/
int
virepeatchange(UNUSED(char **args))
{
    /* make sure we have a change to repeat */
    if (!vichgbuf || vichgflag)
	return 1;
    /* restore or update the saved count and buffer */
    if (zmod.flags & MOD_MULT) {
	lastmod.mult = zmod.mult;
	lastmod.flags |= MOD_MULT;
    }
    if (zmod.flags & MOD_VIBUF) {
	lastmod.vibuf = zmod.vibuf;
	lastmod.flags = (lastmod.flags & ~MOD_VIAPP) |
	    MOD_VIBUF | (zmod.flags & MOD_VIAPP);
    }
    /* repeat the command */
    inrepeat = 1;
    ungetkeys(vichgbuf, vichgbufptr);
    return 0;
}

/**/
int
viindent(UNUSED(char **args))
{
    int oldcs = cs, c2;

    /* get the range */
    startvichange(1);
    if ((c2 = getvirange(0)) == -1) {
	vichgflag = 0;
	return 1;
    }
    vichgflag = 0;
    /* must be a line range */
    if (!vilinerange) {
	cs = oldcs;
	return 1;
    }
    oldcs = cs;
    /* add a tab to the beginning of each line within range */
    while (cs < c2) {
	spaceinline(1);
	line[cs] = '\t';
	cs = findeol() + 1;
    }
    /* go back to the first line of the range */
    cs = oldcs;
    vifirstnonblank(zlenoargs);
    return 0;
}

/**/
int
viunindent(UNUSED(char **args))
{
    int oldcs = cs, c2;

    /* get the range */
    startvichange(1);
    if ((c2 = getvirange(0)) == -1) {
	vichgflag = 0;
	return 1;
    }
    vichgflag = 0;
    /* must be a line range */
    if (!vilinerange) {
	cs = oldcs;
	return 1;
    }
    oldcs = cs;
    /* remove a tab from the beginning of each line within range */
    while (cs < c2) {
	if (line[cs] == '\t')
	    foredel(1);
	cs = findeol() + 1;
    }
    /* go back to the first line of the range */
    cs = oldcs;
    vifirstnonblank(zlenoargs);
    return 0;
}

/**/
int
vibackwarddeletechar(char **args)
{
    int n = zmult;

    if (invicmdmode())
	startvichange(-1);
    /* handle negative argument */
    if (n < 0) {
	int ret;
	zmult = -n;
	ret = videletechar(args);
	zmult = n;
	return ret;
    }
    /* It is an error to be at the beginning of the line, or (in *
     * insert mode) to delete past the beginning of insertion.   */
    if ((!invicmdmode() && cs - n < viinsbegin) || cs == findbol()) {
	return 1;
    }
    /* Put argument into the acceptable range -- it is not an error to  *
     * specify a greater count than the number of available characters. */
    if (n > cs - findbol())
	n = cs - findbol();
    /* do the deletion */
    backkill(n, 1);
    return 0;
}

/**/
int
vikillline(UNUSED(char **args))
{
    if (viinsbegin > cs)
	return 1;
    backdel(cs - viinsbegin);
    return 0;
}

/**/
int
viputbefore(UNUSED(char **args))
{
    Cutbuffer buf = &cutbuf;
    int n = zmult;

    startvichange(-1);
    if (n < 0)
	return 1;
    if (zmod.flags & MOD_VIBUF)
	buf = &vibuf[zmod.vibuf];
    if (!buf->buf)
	return 1;
    if(buf->flags & CUTBUFFER_LINE) {
	cs = findbol();
	spaceinline(buf->len + 1);
	memcpy((char *)line + cs, buf->buf, buf->len);
	line[cs + buf->len] = '\n';
	vifirstnonblank(zlenoargs);
    } else {
	while (n--) {
	    spaceinline(buf->len);
	    memcpy((char *)line + cs, buf->buf, buf->len);
	    cs += buf->len;
	}
	if (cs)
	    cs--;
    }
    return 0;
}

/**/
int
viputafter(UNUSED(char **args))
{
    Cutbuffer buf = &cutbuf;
    int n = zmult;

    startvichange(-1);
    if (n < 0)
	return 1;
    if (zmod.flags & MOD_VIBUF)
	buf = &vibuf[zmod.vibuf];
    if (!buf->buf)
	return 1;
    if(buf->flags & CUTBUFFER_LINE) {
	cs = findeol();
	spaceinline(buf->len + 1);
	line[cs++] = '\n';
	memcpy((char *)line + cs, buf->buf, buf->len);
	vifirstnonblank(zlenoargs);
    } else {
	if (cs != findeol())
	    cs++;
	while (n--) {
	    spaceinline(buf->len);
	    memcpy((char *)line + cs, buf->buf, buf->len);
	    cs += buf->len;
	}
	if (cs)
	    cs--;
    }
    return 0;
}

/**/
int
vijoin(UNUSED(char **args))
{
    int x;

    startvichange(-1);
    if ((x = findeol()) == ll)
	return 1;
    cs = x + 1;
    for (x = 1; cs != ll && iblank(line[cs]); cs++, x++);
    backdel(x);
    if (cs && iblank(line[cs-1]))
	cs--;
    else {
	spaceinline(1);
	line[cs] = ' ';
    }
    return 0;
}

/**/
int
viswapcase(UNUSED(char **args))
{
    int eol, n = zmult;

    startvichange(-1);
    if (n < 1)
	return 1;
    eol = findeol();
    while (cs < eol && n--) {
	if (islower(line[cs]))
	    line[cs] = tuupper(line[cs]);
	else if (isupper(line[cs]))
	    line[cs] = tulower(line[cs]);
	cs++;
    }
    if (cs && cs == eol)
	cs--;
    return 0;
}

/**/
int
vicapslockpanic(UNUSED(char **args))
{
    clearlist = 1;
    zbeep();
    statusline = "press a lowercase key to continue";
    statusll = strlen(statusline);
    zrefresh();
    while (!islower(getkey(0)));
    statusline = NULL;
    return 0;
}

/**/
int
visetbuffer(UNUSED(char **args))
{
    int ch;

    if ((zmod.flags & MOD_VIBUF) ||
	(((ch = getkey(0)) < '1' || ch > '9') &&
	 (ch < 'a' || ch > 'z') && (ch < 'A' || ch > 'Z')))
	return 1;
    if (ch >= 'A' && ch <= 'Z')	/* needed in cut() */
	zmod.flags |= MOD_VIAPP;
    else
	zmod.flags &= ~MOD_VIAPP;
    zmod.vibuf = tulower(ch) + (idigit(ch) ? -'1' + 26 : -'a');
    zmod.flags |= MOD_VIBUF;
    prefixflag = 1;
    return 0;
}

/**/
int
vikilleol(UNUSED(char **args))
{
    int n = findeol() - cs;

    startvichange(-1);
    if (!n) {
	/* error -- line already empty */
	return 1;
    }
    /* delete to end of line */
    forekill(findeol() - cs, 0);
    return 0;
}

/**/
int
vipoundinsert(UNUSED(char **args))
{
    int oldcs = cs;

    startvichange(-1);
    vifirstnonblank(zlenoargs);
    if(line[cs] != '#') {
	spaceinline(1);
	line[cs] = '#';
	if(cs <= viinsbegin)
	    viinsbegin++;
	cs = oldcs + (cs <= oldcs);
    } else {
	foredel(1);
	if (cs < viinsbegin)
	    viinsbegin--;
	cs = oldcs - (cs < oldcs);
    }
    return 0;
}

/**/
int
viquotedinsert(char **args)
{
#ifndef HAS_TIO
    struct sgttyb sob;
#endif

    spaceinline(1);
    line[cs] = '^';
    zrefresh();
#ifndef HAS_TIO
    sob = shttyinfo.sgttyb;
    sob.sg_flags = (sob.sg_flags | RAW) & ~ECHO;
    ioctl(SHTTY, TIOCSETN, &sob);
#endif
    lastchar = getkey(0);
#ifndef HAS_TIO
    zsetterm();
#endif
    foredel(1);
    if(lastchar < 0)
	return 1;
    else
	return selfinsert(args);
}

/* the 0 key in vi: continue a repeat count in the manner of      *
 * digit-argument if possible, otherwise do vi-beginning-of-line. */

/**/
int
vidigitorbeginningofline(char **args)
{
    if(zmod.flags & MOD_TMULT)
	return digitargument(args);
    else {
	removesuffix();
	invalidatelist();
	return vibeginningofline(args);
    }
}
