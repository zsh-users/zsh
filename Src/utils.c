/*
 * utils.c - miscellaneous utilities
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
#include "utils.pro"

#if defined(HAVE_WCHAR_H) && defined(HAVE_WCTOMB) && defined (__STDC_ISO_10646__)
# include <wchar.h>
#else
# ifdef HAVE_LANGINFO_H 			       
#   include <langinfo.h>			       
#   if defined(HAVE_ICONV) || defined(HAVE_LIBICONV)   
#     include <iconv.h> 			       
#   endif					       
# endif 					       
#endif

/* name of script being sourced */

/**/
char *scriptname;

/* Print an error */
 
/**/
mod_export void
zerr(const char *fmt, const char *str, int num)
{
    if (errflag || noerrs) {
	if (noerrs < 2)
	    errflag = 1;
	return;
    }
    zwarn(fmt, str, num);
    errflag = 1;
}

/**/
mod_export void
zerrnam(const char *cmd, const char *fmt, const char *str, int num)
{
    if (errflag || noerrs)
	return;

    zwarnnam(cmd, fmt, str, num);
    errflag = 1;
}

/**/
mod_export void
zwarn(const char *fmt, const char *str, int num)
{
    if (errflag || noerrs)
	return;
    if (isatty(2))
	trashzle();
    /*
     * scriptname is set when sourcing scripts, so that we get the
     * correct name instead of the generic name of whatever
     * program/script is running.  It's also set in shell functions,
     * so test locallevel, too.
     */
    nicezputs((isset(SHINSTDIN) && !locallevel) ? "zsh" :
	      scriptname ? scriptname : argzero, stderr);
    fputc((unsigned char)':', stderr);
    zerrmsg(fmt, str, num);
}

/**/
mod_export void
zwarnnam(const char *cmd, const char *fmt, const char *str, int num)
{
    if (errflag || noerrs)
	return;
    trashzle();
    if (unset(SHINSTDIN) || locallevel) {
	nicezputs(scriptname ? scriptname : argzero, stderr);
	fputc((unsigned char)':', stderr);
    }
    if (cmd) {
	nicezputs(cmd, stderr);
	fputc((unsigned char)':', stderr);
    }
    zerrmsg(fmt, str, num);
}

/**/
void
zerrmsg(const char *fmt, const char *str, int num)
{
    if ((unset(SHINSTDIN) || locallevel) && lineno)
	fprintf(stderr, "%ld: ", (long)lineno);
    else
	fputc((unsigned char)' ', stderr);

    while (*fmt)
	if (*fmt == '%') {
	    fmt++;
	    switch (*fmt++) {
	    case 's':
		nicezputs(str, stderr);
		break;
	    case 'l': {
		char *s;
		num = metalen(str, num);
		s = zhalloc(num + 1);
		memcpy(s, str, num);
		s[num] = '\0';
		nicezputs(s, stderr);
		break;
	    }
	    case 'd':
		fprintf(stderr, "%d", num);
		break;
	    case '%':
		putc('%', stderr);
		break;
	    case 'c':
		fputs(nicechar(num), stderr);
		break;
	    case 'e':
		/* print the corresponding message for this errno */
		if (num == EINTR) {
		    fputs("interrupt\n", stderr);
		    errflag = 1;
		    return;
		}
		/* If the message is not about I/O problems, it looks better *
		 * if we uncapitalize the first letter of the message        */
		if (num == EIO)
		    fputs(strerror(num), stderr);
		else {
		    char *errmsg = strerror(num);
		    fputc(tulower(errmsg[0]), stderr);
		    fputs(errmsg + 1, stderr);
		}
		break;
	    }
	} else {
	    putc(*fmt == Meta ? *++fmt ^ 32 : *fmt, stderr);
	    fmt++;
	}
    putc('\n', stderr);
    fflush(stderr);
}

/* Output a single character, for the termcap routines.     *
 * This is used instead of putchar since it can be a macro. */

/**/
mod_export int
putraw(int c)
{
    putc(c, stdout);
    return 0;
}

/* Output a single character, for the termcap routines. */

/**/
mod_export int
putshout(int c)
{
    putc(c, shout);
    return 0;
}

/* Turn a character into a visible representation thereof.  The visible *
 * string is put together in a static buffer, and this function returns *
 * a pointer to it.  Printable characters stand for themselves, DEL is  *
 * represented as "^?", newline and tab are represented as "\n" and     *
 * "\t", and normal control characters are represented in "^C" form.    *
 * Characters with bit 7 set, if unprintable, are represented as "\M-"  *
 * followed by the visible representation of the character with bit 7   *
 * stripped off.  Tokens are interpreted, rather than being treated as  *
 * literal characters.                                                  */

/**/
mod_export char *
nicechar(int c)
{
    static char buf[6];
    char *s = buf;
    c &= 0xff;
    if (isprint(c))
	goto done;
    if (c & 0x80) {
	if (isset(PRINTEIGHTBIT))
	    goto done;
	*s++ = '\\';
	*s++ = 'M';
	*s++ = '-';
	c &= 0x7f;
	if(isprint(c))
	    goto done;
    }
    if (c == 0x7f) {
	*s++ = '^';
	c = '?';
    } else if (c == '\n') {
	*s++ = '\\';
	c = 'n';
    } else if (c == '\t') {
	*s++ = '\\';
	c = 't';
    } else if (c < 0x20) {
	*s++ = '^';
	c += 0x40;
    }
    done:
    *s++ = c;
    *s = 0;
    return buf;
}

/* Output a string's visible representation. */

#if 0 /**/
void
nicefputs(char *s, FILE *f)
{
    for (; *s; s++)
	fputs(nicechar(STOUC(*s)), f);
}
#endif

/* Return the length of the visible representation of a string. */

/**/
size_t
nicestrlen(char *s)
{
    size_t l = 0;

    for (; *s; s++)
	l += strlen(nicechar(STOUC(*s)));
    return l;
}

/* get a symlink-free pathname for s relative to PWD */

/**/
char *
findpwd(char *s)
{
    char *t;

    if (*s == '/')
	return xsymlink(s);
    s = tricat((pwd[1]) ? pwd : "", "/", s);
    t = xsymlink(s);
    zsfree(s);
    return t;
}

/* Check whether a string contains the *
 * name of the present directory.      */

/**/
int
ispwd(char *s)
{
    struct stat sbuf, tbuf;

    if (stat(unmeta(s), &sbuf) == 0 && stat(".", &tbuf) == 0)
	if (sbuf.st_dev == tbuf.st_dev && sbuf.st_ino == tbuf.st_ino)
	    return 1;
    return 0;
}

static char xbuf[PATH_MAX*2];

/**/
static char **
slashsplit(char *s)
{
    char *t, **r, **q;
    int t0;

    if (!*s)
	return (char **) zcalloc(sizeof(char **));

    for (t = s, t0 = 0; *t; t++)
	if (*t == '/')
	    t0++;
    q = r = (char **) zalloc(sizeof(char **) * (t0 + 2));

    while ((t = strchr(s, '/'))) {
	*q++ = ztrduppfx(s, t - s);
	while (*t == '/')
	    t++;
	if (!*t) {
	    *q = NULL;
	    return r;
	}
	s = t;
    }
    *q++ = ztrdup(s);
    *q = NULL;
    return r;
}

/* expands symlinks and .. or . expressions */
/* if flag = 0, only expand .. and . expressions */

/**/
static int
xsymlinks(char *s)
{
    char **pp, **opp;
    char xbuf2[PATH_MAX*2], xbuf3[PATH_MAX*2];
    int t0, ret = 0;

    opp = pp = slashsplit(s);
    for (; *pp; pp++) {
	if (!strcmp(*pp, ".")) {
	    zsfree(*pp);
	    continue;
	}
	if (!strcmp(*pp, "..")) {
	    char *p;

	    zsfree(*pp);
	    if (!strcmp(xbuf, "/"))
		continue;
	    p = xbuf + strlen(xbuf);
	    while (*--p != '/');
	    *p = '\0';
	    continue;
	}
	sprintf(xbuf2, "%s/%s", xbuf, *pp);
	t0 = readlink(unmeta(xbuf2), xbuf3, PATH_MAX);
	if (t0 == -1) {
	    strcat(xbuf, "/");
	    strcat(xbuf, *pp);
	    zsfree(*pp);
	} else {
	    ret = 1;
	    metafy(xbuf3, t0, META_NOALLOC);
	    if (*xbuf3 == '/') {
		strcpy(xbuf, "");
		xsymlinks(xbuf3 + 1);
	    } else
		xsymlinks(xbuf3);
	    zsfree(*pp);
	}
    }
    free(opp);
    return ret;
}

/*
 * expand symlinks in s, and remove other weird things:
 * note that this always expands symlinks.
 */

/**/
char *
xsymlink(char *s)
{
    if (*s != '/')
	return NULL;
    *xbuf = '\0';
    xsymlinks(s + 1);
    if (!*xbuf)
	return ztrdup("/");
    return ztrdup(xbuf);
}

/**/
void
print_if_link(char *s)
{
    if (*s == '/') {
	*xbuf = '\0';
	if (xsymlinks(s + 1))
	    printf(" -> "), zputs(*xbuf ? xbuf : "/", stdout);
    }
}

/* print a directory */

/**/
void
fprintdir(char *s, FILE *f)
{
    Nameddir d = finddir(s);

    if (!d)
	fputs(unmeta(s), f);
    else {
	putc('~', f);
	fputs(unmeta(d->nam), f);
	fputs(unmeta(s + strlen(d->dir)), f);
    }
}

/* Returns the current username.  It caches the username *
 * and uid to try to avoid requerying the password files *
 * or NIS/NIS+ database.                                 */

/**/
uid_t cached_uid;
/**/
char *cached_username;

/**/
char *
get_username(void)
{
#ifdef HAVE_GETPWUID
    struct passwd *pswd;
    uid_t current_uid;
 
    current_uid = getuid();
    if (current_uid != cached_uid) {
	cached_uid = current_uid;
	zsfree(cached_username);
	if ((pswd = getpwuid(current_uid)))
	    cached_username = ztrdup(pswd->pw_name);
	else
	    cached_username = ztrdup("");
    }
#else /* !HAVE_GETPWUID */
    cached_uid = getuid();
#endif /* !HAVE_GETPWUID */
    return cached_username;
}

/* static variables needed by finddir(). */

static char *finddir_full;
static Nameddir finddir_last;
static int finddir_best;

/* ScanFunc used by finddir(). */

/**/
static void
finddir_scan(HashNode hn, int flags)
{
    Nameddir nd = (Nameddir) hn;

    if(nd->diff > finddir_best && !dircmp(nd->dir, finddir_full)
       && !(nd->flags & ND_NOABBREV)) {
	finddir_last=nd;
	finddir_best=nd->diff;
    }
}

/* See if a path has a named directory as its prefix. *
 * If passed a NULL argument, it will invalidate any  *
 * cached information.                                */

/**/
Nameddir
finddir(char *s)
{
    static struct nameddir homenode = { NULL, "", 0, NULL, 0 };
    static int ffsz;

    /* Invalidate directory cache if argument is NULL.  This is called *
     * whenever a node is added to or removed from the hash table, and *
     * whenever the value of $HOME changes.  (On startup, too.)        */
    if (!s) {
	homenode.dir = home;
	homenode.diff = strlen(home);
	if(homenode.diff==1)
	    homenode.diff = 0;
	if(!finddir_full)
	    finddir_full = zalloc(ffsz = PATH_MAX);
	finddir_full[0] = 0;
	return finddir_last = NULL;
    }

    if(!strcmp(s, finddir_full) && *finddir_full)
	return finddir_last;

    if(strlen(s) >= ffsz) {
	free(finddir_full);
	finddir_full = zalloc(ffsz = strlen(s) * 2);
    }
    strcpy(finddir_full, s);
    finddir_best=0;
    finddir_last=NULL;
    finddir_scan((HashNode)&homenode, 0);
    scanhashtable(nameddirtab, 0, 0, 0, finddir_scan, 0);
    return finddir_last;
}

/* add a named directory */

/**/
mod_export void
adduserdir(char *s, char *t, int flags, int always)
{
    Nameddir nd;

    /* We don't maintain a hash table in non-interactive shells. */
    if (!interact)
	return;

    /* The ND_USERNAME flag means that this possible hash table *
     * entry is derived from a passwd entry.  Such entries are  *
     * subordinate to explicitly generated entries.             */
    if ((flags & ND_USERNAME) && nameddirtab->getnode2(nameddirtab, s))
	return;

    /* Normal parameter assignments generate calls to this function, *
     * with always==0.  Unless the AUTO_NAME_DIRS option is set, we  *
     * don't let such assignments actually create directory names.   *
     * Instead, a reference to the parameter as a directory name can *
     * cause the actual creation of the hash table entry.            */
    if (!always && unset(AUTONAMEDIRS) &&
	    !nameddirtab->getnode2(nameddirtab, s))
	return;

    if (!t || *t != '/' || strlen(t) >= PATH_MAX) {
	/* We can't use this value as a directory, so simply remove *
	 * the corresponding entry in the hash table, if any.       */
	HashNode hn = nameddirtab->removenode(nameddirtab, s);

	if(hn)
	    nameddirtab->freenode(hn);
	return;
    }

    /* add the name */
    nd = (Nameddir) zcalloc(sizeof *nd);
    nd->flags = flags;
    nd->dir = ztrdup(t);
    /* The variables PWD and OLDPWD are not to be displayed as ~PWD etc. */
    if (!strcmp(s, "PWD") || !strcmp(s, "OLDPWD"))
	nd->flags |= ND_NOABBREV;
    nameddirtab->addnode(nameddirtab, ztrdup(s), nd);
}

/* Get a named directory: this function can cause a directory name *
 * to be added to the hash table, if it isn't there already.       */

/**/
char *
getnameddir(char *name)
{
    Param pm;
    char *str;
    Nameddir nd;

    /* Check if it is already in the named directory table */
    if ((nd = (Nameddir) nameddirtab->getnode(nameddirtab, name)))
	return dupstring(nd->dir);

    /* Check if there is a scalar parameter with this name whose value *
     * begins with a `/'.  If there is, add it to the hash table and   *
     * return the new value.                                           */
    if ((pm = (Param) paramtab->getnode(paramtab, name)) &&
	    (PM_TYPE(pm->flags) == PM_SCALAR) &&
	    (str = getsparam(name)) && *str == '/') {
	pm->flags |= PM_NAMEDDIR;
	adduserdir(name, str, 0, 1);
	return str;
    }

#ifdef HAVE_GETPWNAM
    {
	/* Retrieve an entry from the password table/database for this user. */
	struct passwd *pw;
	if ((pw = getpwnam(name))) {
	    char *dir = isset(CHASELINKS) ? xsymlink(pw->pw_dir)
		: ztrdup(pw->pw_dir);
	    adduserdir(name, dir, ND_USERNAME, 1);
	    str = dupstring(dir);
	    zsfree(dir);
	    return str;
	}
    }
#endif /* HAVE_GETPWNAM */

    /* There are no more possible sources of directory names, so give up. */
    return NULL;
}

/**/
static int
dircmp(char *s, char *t)
{
    if (s) {
	for (; *s == *t; s++, t++)
	    if (!*s)
		return 0;
	if (!*s && *t == '/')
	    return 0;
    }
    return 1;
}

/* extra functions to call before displaying the prompt */

/**/
mod_export LinkList prepromptfns;

/* the last time we checked mail */
 
/**/
time_t lastmailcheck;
 
/* the last time we checked the people in the WATCH variable */
 
/**/
time_t lastwatch;

/**/
mod_export int
callhookfunc(char *name, LinkList lnklst)
{
    Eprog prog;

    if ((prog = getshfunc(name)) != &dummy_eprog) {
	/*
	 * Save stopmsg, since user doesn't get a chance to respond
	 * to a list of jobs generated in a hook.
	 */
	int osc = sfcontext, osm = stopmsg;

	sfcontext = SFC_HOOK;
	doshfunc(name, prog, lnklst, 0, 1);
	sfcontext = osc;
	stopmsg = osm;

	return 0;
    }

    return 1;
}

/* do pre-prompt stuff */

/**/
void
preprompt(void)
{
    static time_t lastperiodic;
    LinkNode ln;
    int period = getiparam("PERIOD");
    int mailcheck = getiparam("MAILCHECK");

    /* If NOTIFY is not set, then check for completed *
     * jobs before we print the prompt.               */
    if (unset(NOTIFY))
	scanjobs();
    if (errflag)
	return;

    /* If a shell function named "precmd" exists, *
     * then execute it.                           */
    callhookfunc("precmd", NULL);
    if (errflag)
	return;

    /* If 1) the parameter PERIOD exists, 2) the shell function     *
     * "periodic" exists, 3) it's been greater than PERIOD since we *
     * executed "periodic", then execute it now.                    */
    if (period && (time(NULL) > lastperiodic + period) &&
	!callhookfunc("periodic", NULL))
	lastperiodic = time(NULL);
    if (errflag)
	return;

    /* If WATCH is set, then check for the *
     * specified login/logout events.      */
    if (watch) {
	if ((int) difftime(time(NULL), lastwatch) > getiparam("LOGCHECK")) {
	    dowatch();
	    lastwatch = time(NULL);
	}
    }
    if (errflag)
	return;

    /* Check mail */
    if (mailcheck && (int) difftime(time(NULL), lastmailcheck) > mailcheck) {
	char *mailfile;

	if (mailpath && *mailpath && **mailpath)
	    checkmailpath(mailpath);
	else {
	    queue_signals();
	    if ((mailfile = getsparam("MAIL")) && *mailfile) {
		char *x[2];

		x[0] = mailfile;
		x[1] = NULL;
		checkmailpath(x);
	    }
	    unqueue_signals();
	}
	lastmailcheck = time(NULL);
    }

    /* Some people have claimed that C performs type    *
     * checking, but they were later found to be lying. */
    for(ln = firstnode(prepromptfns); ln; ln = nextnode(ln))
	(**(void (**) _((void)))getdata(ln))();
}

/**/
static void
checkmailpath(char **s)
{
    struct stat st;
    char *v, *u, c;

    while (*s) {
	for (v = *s; *v && *v != '?'; v++);
	c = *v;
	*v = '\0';
	if (c != '?')
	    u = NULL;
	else
	    u = v + 1;
	if (**s == 0) {
	    *v = c;
	    zerr("empty MAILPATH component: %s", *s, 0);
	} else if (mailstat(unmeta(*s), &st) == -1) {
	    if (errno != ENOENT)
		zerr("%e: %s", *s, errno);
	} else if (S_ISDIR(st.st_mode)) {
	    LinkList l;
	    DIR *lock = opendir(unmeta(*s));
	    char buf[PATH_MAX * 2], **arr, **ap;
	    int ct = 1;

	    if (lock) {
		char *fn;

		pushheap();
		l = newlinklist();
		while ((fn = zreaddir(lock, 1)) && !errflag) {
		    if (u)
			sprintf(buf, "%s/%s?%s", *s, fn, u);
		    else
			sprintf(buf, "%s/%s", *s, fn);
		    addlinknode(l, dupstring(buf));
		    ct++;
		}
		closedir(lock);
		ap = arr = (char **) zhalloc(ct * sizeof(char *));

		while ((*ap++ = (char *)ugetnode(l)));
		checkmailpath(arr);
		popheap();
	    }
	} else {
	    if (st.st_size && st.st_atime <= st.st_mtime &&
		st.st_mtime > lastmailcheck) {
		if (!u) {
		    fprintf(shout, "You have new mail.\n");
		    fflush(shout);
		} else {
		    VARARR(char, usav, underscoreused);

		    memcpy(usav, underscore, underscoreused);

		    setunderscore(*s);

		    u = dupstring(u);
		    if (! parsestr(u)) {
			singsub(&u);
			zputs(u, shout);
			fputc('\n', shout);
			fflush(shout);
		    }
		    setunderscore(usav);
		}
	    }
	    if (isset(MAILWARNING) && st.st_atime > st.st_mtime &&
		st.st_atime > lastmailcheck && st.st_size) {
		fprintf(shout, "The mail in %s has been read.\n", unmeta(*s));
		fflush(shout);
	    }
	}
	*v = c;
	s++;
    }
}

/* This prints the XTRACE prompt. */

/**/
FILE *xtrerr = 0;

/**/
void
printprompt4(void)
{
    if (!xtrerr)
	xtrerr = stderr;
    if (prompt4) {
	int l, t = opts[XTRACE];
	char *s = dupstring(prompt4);

	opts[XTRACE] = 0;
	unmetafy(s, &l);
	s = unmetafy(promptexpand(metafy(s, l, META_NOALLOC), 0, NULL, NULL), &l);
	opts[XTRACE] = t;

	fprintf(xtrerr, "%s", s);
	free(s);
    }
}

/**/
mod_export void
freestr(void *a)
{
    zsfree(a);
}

/**/
void
gettyinfo(struct ttyinfo *ti)
{
    if (SHTTY != -1) {
#ifdef HAVE_TERMIOS_H
# ifdef HAVE_TCGETATTR
	if (tcgetattr(SHTTY, &ti->tio) == -1)
# else
	if (ioctl(SHTTY, TCGETS, &ti->tio) == -1)
# endif
	    zerr("bad tcgets: %e", NULL, errno);
#else
# ifdef HAVE_TERMIO_H
	ioctl(SHTTY, TCGETA, &ti->tio);
# else
	ioctl(SHTTY, TIOCGETP, &ti->sgttyb);
	ioctl(SHTTY, TIOCLGET, &ti->lmodes);
	ioctl(SHTTY, TIOCGETC, &ti->tchars);
	ioctl(SHTTY, TIOCGLTC, &ti->ltchars);
# endif
#endif
    }
}

/**/
mod_export void
settyinfo(struct ttyinfo *ti)
{
    if (SHTTY != -1) {
#ifdef HAVE_TERMIOS_H
# ifdef HAVE_TCGETATTR
#  ifndef TCSADRAIN
#   define TCSADRAIN 1	/* XXX Princeton's include files are screwed up */
#  endif
	tcsetattr(SHTTY, TCSADRAIN, &ti->tio);
    /* if (tcsetattr(SHTTY, TCSADRAIN, &ti->tio) == -1) */
# else
	ioctl(SHTTY, TCSETS, &ti->tio);
    /* if (ioctl(SHTTY, TCSETS, &ti->tio) == -1) */
# endif
	/*	zerr("settyinfo: %e",NULL,errno)*/ ;
#else
# ifdef HAVE_TERMIO_H
	ioctl(SHTTY, TCSETA, &ti->tio);
# else
	ioctl(SHTTY, TIOCSETN, &ti->sgttyb);
	ioctl(SHTTY, TIOCLSET, &ti->lmodes);
	ioctl(SHTTY, TIOCSETC, &ti->tchars);
	ioctl(SHTTY, TIOCSLTC, &ti->ltchars);
# endif
#endif
    }
}

/* the default tty state */
 
/**/
mod_export struct ttyinfo shttyinfo;

/* != 0 if we need to call resetvideo() */

/**/
mod_export int resetneeded;

#ifdef TIOCGWINSZ
/* window size changed */

/**/
mod_export int winchanged;
#endif

static int
adjustlines(int signalled)
{
    int oldlines = lines;

#ifdef TIOCGWINSZ
    if (signalled || lines <= 0)
	lines = shttyinfo.winsize.ws_row;
    else
	shttyinfo.winsize.ws_row = lines;
#endif /* TIOCGWINSZ */
    if (lines <= 0) {
	DPUTS(signalled, "BUG: Impossible TIOCGWINSZ rows");
	lines = tclines > 0 ? tclines : 24;
    }

    if (lines > 2)
	termflags &= ~TERM_SHORT;
    else
	termflags |= TERM_SHORT;

    return (lines != oldlines);
}

static int
adjustcolumns(int signalled)
{
    int oldcolumns = columns;

#ifdef TIOCGWINSZ
    if (signalled || columns <= 0)
	columns = shttyinfo.winsize.ws_col;
    else
	shttyinfo.winsize.ws_col = columns;
#endif /* TIOCGWINSZ */
    if (columns <= 0) {
	DPUTS(signalled, "BUG: Impossible TIOCGWINSZ cols");
	columns = tccolumns > 0 ? tccolumns : 80;
    }

    if (columns > 2)
	termflags &= ~TERM_NARROW;
    else
	termflags |= TERM_NARROW;

    return (columns != oldcolumns);
}

/* check the size of the window and adjust if necessary. *
 * The value of from:					 *
 *   0: called from update_job or setupvals		 *
 *   1: called from the SIGWINCH handler		 *
 *   2: called from the LINES parameter callback	 *
 *   3: called from the COLUMNS parameter callback	 */

/**/
void
adjustwinsize(int from)
{
    static int getwinsz = 1;
    int ttyrows = shttyinfo.winsize.ws_row;
    int ttycols = shttyinfo.winsize.ws_col;
    int resetzle = 0;

    if (getwinsz || from == 1) {
#ifdef TIOCGWINSZ
	if (SHTTY == -1)
	    return;
	if (ioctl(SHTTY, TIOCGWINSZ, (char *)&shttyinfo.winsize) == 0) {
	    resetzle = (ttyrows != shttyinfo.winsize.ws_row ||
			ttycols != shttyinfo.winsize.ws_col);
	    if (from == 0 && resetzle && ttyrows && ttycols)
		from = 1; /* Signal missed while a job owned the tty? */
	    ttyrows = shttyinfo.winsize.ws_row;
	    ttycols = shttyinfo.winsize.ws_col;
	} else {
	    /* Set to value from environment on failure */
	    shttyinfo.winsize.ws_row = lines;
	    shttyinfo.winsize.ws_col = columns;
	    resetzle = (from == 1);
	}
#else
	resetzle = from == 1;
#endif /* TIOCGWINSZ */
    } /* else
	 return; */

    switch (from) {
    case 0:
    case 1:
	getwinsz = 0;
	/* Calling setiparam() here calls this function recursively, but  *
	 * because we've already called adjustlines() and adjustcolumns() *
	 * here, recursive calls are no-ops unless a signal intervenes.   *
	 * The commented "else return;" above might be a safe shortcut,   *
	 * but I'm concerned about what happens on race conditions; e.g., *
	 * suppose the user resizes his xterm during `eval $(resize)'?    */
	if (adjustlines(from) && zgetenv("LINES"))
	    setiparam("LINES", lines);
	if (adjustcolumns(from) && zgetenv("COLUMNS"))
	    setiparam("COLUMNS", columns);
	getwinsz = 1;
	break;
    case 2:
	resetzle = adjustlines(0);
	break;
    case 3:
	resetzle = adjustcolumns(0);
	break;
    }

#ifdef TIOCGWINSZ
    if (interact && from >= 2 &&
	(shttyinfo.winsize.ws_row != ttyrows ||
	 shttyinfo.winsize.ws_col != ttycols)) {
	/* shttyinfo.winsize is already set up correctly */
	ioctl(SHTTY, TIOCSWINSZ, (char *)&shttyinfo.winsize);
    }
#endif /* TIOCGWINSZ */

    if (zleactive && resetzle) {
#ifdef TIOCGWINSZ
	winchanged =
#endif /* TIOCGWINSZ */
	    resetneeded = 1;
	zrefresh();
    }
}

/* Move a fd to a place >= 10 and mark the new fd in fdtable.  If the fd *
 * is already >= 10, it is not moved.  If it is invalid, -1 is returned. */

/**/
mod_export int
movefd(int fd)
{
    if(fd != -1 && fd < 10) {
#ifdef F_DUPFD
	int fe = fcntl(fd, F_DUPFD, 10);
#else
	int fe = movefd(dup(fd));
#endif
	zclose(fd);
	fd = fe;
    }
    if(fd != -1) {
	if (fd > max_zsh_fd) {
	    while (fd >= fdtable_size)
		fdtable = zrealloc(fdtable, (fdtable_size *= 2));
	    max_zsh_fd = fd;
	}
	fdtable[fd] = 1;
    }
    return fd;
}

/* Move fd x to y.  If x == -1, fd y is closed. */

/**/
mod_export void
redup(int x, int y)
{
    if(x < 0)
	zclose(y);
    else if (x != y) {
	while (y >= fdtable_size)
	    fdtable = zrealloc(fdtable, (fdtable_size *= 2));
	dup2(x, y);
	if ((fdtable[y] = fdtable[x]) && y > max_zsh_fd)
	    max_zsh_fd = y;
	zclose(x);
    }
}

/* Close the given fd, and clear it from fdtable. */

/**/
mod_export int
zclose(int fd)
{
    if (fd >= 0) {
	fdtable[fd] = 0;
	while (max_zsh_fd > 0 && !fdtable[max_zsh_fd])
	    max_zsh_fd--;
	if (fd == coprocin)
	    coprocin = -1;
	if (fd == coprocout)
	    coprocout = -1;
    }
    return close(fd);
}

/* Get a file name relative to $TMPPREFIX which *
 * is unique, for use as a temporary file.      */
 
#ifdef HAVE__MKTEMP
extern char *_mktemp(char *);
#endif

/**/
mod_export char *
gettempname(void)
{
    char *s, *ret;
 
    queue_signals();
    if (!(s = getsparam("TMPPREFIX")))
	s = DEFAULT_TMPPREFIX;
 
#ifdef HAVE__MKTEMP
    /* Zsh uses mktemp() safely, so silence the warnings */
    ret = ((char *) _mktemp(dyncat(unmeta(s), "XXXXXX")));
#else
    ret = ((char *) mktemp(dyncat(unmeta(s), "XXXXXX")));
#endif
    unqueue_signals();

    return ret;
}

/* Check if a string contains a token */

/**/
mod_export int
has_token(const char *s)
{
    while(*s)
	if(itok(*s++))
	    return 1;
    return 0;
}

/* Delete a character in a string */
 
/**/
mod_export void
chuck(char *str)
{
    while ((str[0] = str[1]))
	str++;
}

/**/
mod_export int
tulower(int c)
{
    c &= 0xff;
    return (isupper(c) ? tolower(c) : c);
}

/**/
mod_export int
tuupper(int c)
{
    c &= 0xff;
    return (islower(c) ? toupper(c) : c);
}

/* copy len chars from t into s, and null terminate */

/**/
void
ztrncpy(char *s, char *t, int len)
{
    while (len--)
	*s++ = *t++;
    *s = '\0';
}

/* copy t into *s and update s */

/**/
mod_export void
strucpy(char **s, char *t)
{
    char *u = *s;

    while ((*u++ = *t++));
    *s = u - 1;
}

/**/
mod_export void
struncpy(char **s, char *t, int n)
{
    char *u = *s;

    while (n--)
	*u++ = *t++;
    *s = u;
    *u = '\0';
}

/* Return the number of elements in an array of pointers. *
 * It doesn't count the NULL pointer at the end.          */

/**/
mod_export int
arrlen(char **s)
{
    int count;

    for (count = 0; *s; s++, count++);
    return count;
}

/* Skip over a balanced pair of parenthesis. */

/**/
mod_export int
skipparens(char inpar, char outpar, char **s)
{
    int level;

    if (**s != inpar)
	return -1;

    for (level = 1; *++*s && level;)
	if (**s == inpar)
	   ++level;
	else if (**s == outpar)
	   --level;

   return level;
}

/* Convert string to zlong (see zsh.h).  This function (without the z) *
 * is contained in the ANSI standard C library, but a lot of them seem *
 * to be broken.                                                       */

/**/
mod_export zlong
zstrtol(const char *s, char **t, int base)
{
    zlong ret = 0;
    int neg;

    while (inblank(*s))
	s++;

    if ((neg = (*s == '-')))
	s++;
    else if (*s == '+')
	s++;

    if (!base) {
	if (*s != '0')
	    base = 10;
	else if (*++s == 'x' || *s == 'X')
	    base = 16, s++;
	else
	    base = 8;
    }
    if (base <= 10)
	for (; *s >= '0' && *s < ('0' + base); s++)
	    ret = ret * base + *s - '0';
    else
	for (; idigit(*s) || (*s >= 'a' && *s < ('a' + base - 10))
	     || (*s >= 'A' && *s < ('A' + base - 10)); s++)
	    ret = ret * base + (idigit(*s) ? (*s - '0') : (*s & 0x1f) + 9);
    if (t)
	*t = (char *)s;
    return neg ? -ret : ret;
}

/**/
int
setblock_fd(int turnonblocking, int fd, long *modep)
{
#ifdef O_NDELAY
# ifdef O_NONBLOCK
#  define NONBLOCK (O_NDELAY|O_NONBLOCK)
# else /* !O_NONBLOCK */
#  define NONBLOCK O_NDELAY
# endif /* !O_NONBLOCK */
#else /* !O_NDELAY */
# ifdef O_NONBLOCK
#  define NONBLOCK O_NONBLOCK
# else /* !O_NONBLOCK */
#  define NONBLOCK 0
# endif /* !O_NONBLOCK */
#endif /* !O_NDELAY */

#if NONBLOCK
    struct stat st;

    if (!fstat(fd, &st) && !S_ISREG(st.st_mode)) {
	*modep = fcntl(fd, F_GETFL, 0);
	if (*modep != -1) {
	    if (!turnonblocking) {
		/* We want to know if blocking was off */
		if ((*modep & NONBLOCK) ||
		    !fcntl(fd, F_SETFL, *modep | NONBLOCK))
		    return 1;
	    } else if ((*modep & NONBLOCK) &&
		       !fcntl(fd, F_SETFL, *modep & ~NONBLOCK)) {
		/* Here we want to know if the state changed */
		return 1;
	    }
	}
    } else
#endif /* NONBLOCK */
	*modep = -1;
    return 0;

#undef NONBLOCK
}

/**/
int
setblock_stdin(void)
{
    long mode;
    return setblock_fd(1, 0, &mode);
}

/*
 * Check for pending input on fd.  If polltty is set, we may need to
 * use termio to look for input.  As a final resort, go to non-blocking
 * input and try to read a character, which in this case will be
 * returned in *readchar.
 *
 * Note that apart from setting (and restoring) non-blocking input,
 * this function does not change the input mode.  The calling function
 * should have set cbreak mode if necessary.
 */

/**/
mod_export int
read_poll(int fd, int *readchar, int polltty, zlong microseconds)
{
    int ret = -1;
    long mode = -1;
    char c;
#ifdef HAVE_SELECT
    fd_set foofd;
    struct timeval expire_tv;
#else
#ifdef FIONREAD
    int val;
#endif
#endif
#ifdef HAS_TIO
    struct ttyinfo ti;
#endif


#if defined(HAS_TIO) && !defined(__CYGWIN__)
    /*
     * Under Solaris, at least, reading from the terminal in non-canonical
     * mode requires that we use the VMIN mechanism to poll.  Any attempt
     * to check any other way, or to set the terminal to non-blocking mode
     * and poll that way, fails; it will just for canonical mode input.
     * We should probably use this mechanism if the user has set non-canonical
     * mode, in which case testing here for isatty() and ~ICANON would be
     * better than testing whether bin_read() set it, but for now we've got
     * enough problems.
     *
     * Under Cygwin, you won't be surprised to here, this mechanism,
     * although present, doesn't work, and we *have* to use ordinary
     * non-blocking reads to find out if there is a character present
     * in non-canonical mode.
     *
     * I am assuming Solaris is nearer the UNIX norm.  This is not necessarily
     * as plausible as it sounds, but it seems the right way to guess.
     *		pws 2000/06/26
     */
    if (polltty) {
	gettyinfo(&ti);
	if ((polltty = ti.tio.c_cc[VMIN])) {
	    ti.tio.c_cc[VMIN] = 0;
	    /* termios timeout is 10ths of a second */
	    ti.tio.c_cc[VTIME] = (int) (microseconds / (zlong)100000);
	    settyinfo(&ti);
	}
    }
#else
    polltty = 0;
#endif
#ifdef HAVE_SELECT
    expire_tv.tv_sec = (int) (microseconds / (zlong)1000000);
    expire_tv.tv_usec = microseconds % (zlong)1000000;
    FD_ZERO(&foofd);
    FD_SET(fd, &foofd);
    ret = select(fd+1, (SELECT_ARG_2_T) &foofd, NULL, NULL, &expire_tv);
#else
#ifdef FIONREAD
    if (ioctl(fd, FIONREAD, (char *) &val) == 0)
	ret = (val > 0);
#endif
#endif

    if (ret < 0) {
	/*
	 * Final attempt: set non-blocking read and try to read a character.
	 * Praise Bill, this works under Cygwin (nothing else seems to).
	 */
	if ((polltty || setblock_fd(0, fd, &mode)) && read(fd, &c, 1) > 0) {
	    *readchar = STOUC(c);
	    ret = 1;
	}
	if (mode != -1)
	    fcntl(fd, F_SETFL, mode);
    }
#ifdef HAS_TIO
    if (polltty) {
	ti.tio.c_cc[VMIN] = 1;
	ti.tio.c_cc[VTIME] = 0;
	settyinfo(&ti);
    }
#endif
    return (ret > 0);
}

/**/
int
checkrmall(char *s)
{
    if (!shout)
	return 1;
    fprintf(shout, "zsh: sure you want to delete all the files in ");
    if (*s != '/') {
	nicezputs(pwd[1] ? unmeta(pwd) : "", shout);
	fputc('/', shout);
    }
    nicezputs(s, shout);
    if(isset(RMSTARWAIT)) {
	fputs("? (waiting ten seconds)", shout);
	fflush(shout);
	zbeep();
	sleep(10);
	fputc('\n', shout);
    }
    fputs(" [yn]? ", shout);
    fflush(shout);
    zbeep();
    return (getquery("ny", 1) == 'y');
}

/**/
int
read1char(void)
{
    char c;

    while (read(SHTTY, &c, 1) != 1) {
	if (errno != EINTR || errflag || retflag || breaks || contflag)
	    return -1;
    }
    return STOUC(c);
}

/**/
mod_export int
noquery(int purge)
{
    int val = 0;
    char c;

#ifdef FIONREAD
    ioctl(SHTTY, FIONREAD, (char *)&val);
    if (purge) {
	for (; val; val--)
	    read(SHTTY, &c, 1);
    }
#endif

    return val;
}

/**/
int
getquery(char *valid_chars, int purge)
{
    int c, d;
    int isem = !strcmp(term, "emacs");

    attachtty(mypgrp);
    if (!isem)
	setcbreak();

    if (noquery(purge)) {
	if (!isem)
	    settyinfo(&shttyinfo);
	write(SHTTY, "n\n", 2);
	return 'n';
    }

    while ((c = read1char()) >= 0) {
	if (c == 'Y')
	    c = 'y';
	else if (c == 'N')
	    c = 'n';
	if (!valid_chars)
	    break;
	if (c == '\n') {
	    c = *valid_chars;
	    break;
	}
	if (strchr(valid_chars, c)) {
	    write(SHTTY, "\n", 1);
	    break;
	}
	zbeep();
	if (icntrl(c))
	    write(SHTTY, "\b \b", 3);
	write(SHTTY, "\b \b", 3);
    }
    if (isem) {
	if (c != '\n')
	    while ((d = read1char()) >= 0 && d != '\n');
    } else {
	settyinfo(&shttyinfo);
	if (c != '\n' && !valid_chars)
	    write(SHTTY, "\n", 1);
    }
    return c;
}

static int d;
static char *guess, *best;

/**/
static void
spscan(HashNode hn, int scanflags)
{
    int nd;

    nd = spdist(hn->nam, guess, (int) strlen(guess) / 4 + 1);
    if (nd <= d) {
	best = hn->nam;
	d = nd;
    }
}

/* spellcheck a word */
/* fix s ; if hist is nonzero, fix the history list too */

/**/
mod_export void
spckword(char **s, int hist, int cmd, int ask)
{
    char *t, *u;
    int x;
    char ic = '\0';
    int ne;
    int preflen = 0;

    if ((histdone & HISTFLAG_NOEXEC) || **s == '-' || **s == '%')
	return;
    if (!strcmp(*s, "in"))
	return;
    if (!(*s)[0] || !(*s)[1])
	return;
    if (shfunctab->getnode(shfunctab, *s) ||
	builtintab->getnode(builtintab, *s) ||
	cmdnamtab->getnode(cmdnamtab, *s) ||
	aliastab->getnode(aliastab, *s)  ||
	reswdtab->getnode(reswdtab, *s))
	return;
    else if (isset(HASHLISTALL)) {
	cmdnamtab->filltable(cmdnamtab);
	if (cmdnamtab->getnode(cmdnamtab, *s))
	    return;
    }
    t = *s;
    if (*t == Tilde || *t == Equals || *t == String)
	t++;
    for (; *t; t++)
	if (itok(*t))
	    return;
    best = NULL;
    for (t = *s; *t; t++)
	if (*t == '/')
	    break;
    if (**s == Tilde && !*t)
	return;
    if (**s == String && !*t) {
	guess = *s + 1;
	if (*t || !ialpha(*guess))
	    return;
	ic = String;
	d = 100;
	scanhashtable(paramtab, 1, 0, 0, spscan, 0);
    } else if (**s == Equals) {
	if (*t)
	    return;
	if (hashcmd(guess = *s + 1, pathchecked))
	    return;
	d = 100;
	ic = Equals;
	scanhashtable(aliastab, 1, 0, 0, spscan, 0);
	scanhashtable(cmdnamtab, 1, 0, 0, spscan, 0);
    } else {
	guess = *s;
	if (*guess == Tilde || *guess == String) {
	    ic = *guess;
	    if (!*++t)
		return;
	    guess = dupstring(guess);
	    ne = noerrs;
	    noerrs = 2;
	    singsub(&guess);
	    noerrs = ne;
	    if (!guess)
		return;
	    preflen = strlen(guess) - strlen(t);
	}
	if (access(unmeta(guess), F_OK) == 0)
	    return;
	if ((u = spname(guess)) != guess)
	    best = u;
	if (!*t && cmd) {
	    if (hashcmd(guess, pathchecked))
		return;
	    d = 100;
	    scanhashtable(reswdtab, 1, 0, 0, spscan, 0);
	    scanhashtable(aliastab, 1, 0, 0, spscan, 0);
	    scanhashtable(shfunctab, 1, 0, 0, spscan, 0);
	    scanhashtable(builtintab, 1, 0, 0, spscan, 0);
	    scanhashtable(cmdnamtab, 1, 0, 0, spscan, 0);
	}
    }
    if (errflag)
	return;
    if (best && (int)strlen(best) > 1 && strcmp(best, guess)) {
	if (ic) {
	    if (preflen) {
		/* do not correct the result of an expansion */
		if (strncmp(guess, best, preflen))
		    return;
		/* replace the temporarily expanded prefix with the original */
		u = (char *) hcalloc(t - *s + strlen(best + preflen) + 1);
		strncpy(u, *s, t - *s);
		strcpy(u + (t - *s), best + preflen);
	    } else {
		u = (char *) hcalloc(strlen(best) + 2);
		strcpy(u + 1, best);
	    }
	    best = u;
	    guess = *s;
	    *guess = *best = ztokens[ic - Pound];
	}
	if (ask) {
	    if (noquery(0)) {
		x = 'n';
	    } else {
		char *pptbuf;
		pptbuf = promptexpand(sprompt, 0, best, guess);
		zputs(pptbuf, shout);
		free(pptbuf);
		fflush(shout);
		zbeep();
		x = getquery("nyae \t", 0);
	    }
	} else
	    x = 'y';
	if (x == 'y' || x == ' ' || x == '\t') {
	    *s = dupstring(best);
	    if (hist)
		hwrep(best);
	} else if (x == 'a') {
	    histdone |= HISTFLAG_NOEXEC;
	} else if (x == 'e') {
	    histdone |= HISTFLAG_NOEXEC | HISTFLAG_RECALL;
	}
	if (ic)
	    **s = ic;
    }
}

/**/
mod_export int
ztrftime(char *buf, int bufsize, char *fmt, struct tm *tm)
{
    int hr12;
#ifndef HAVE_STRFTIME
    static char *astr[] =
    {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
    static char *estr[] =
    {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
     "Aug", "Sep", "Oct", "Nov", "Dec"};
#else
    char *origbuf = buf;
#endif
    char tmp[3];


    tmp[0] = '%';
    tmp[2] = '\0';
    while (*fmt)
	if (*fmt == '%') {
	    fmt++;
	    switch (*fmt++) {
	    case 'd':
		*buf++ = '0' + tm->tm_mday / 10;
		*buf++ = '0' + tm->tm_mday % 10;
		break;
	    case 'e':
	    case 'f':
		if (tm->tm_mday > 9)
		    *buf++ = '0' + tm->tm_mday / 10;
		else if (fmt[-1] == 'e')
		    *buf++ = ' ';
		*buf++ = '0' + tm->tm_mday % 10;
		break;
	    case 'k':
	    case 'K':
		if (tm->tm_hour > 9)
		    *buf++ = '0' + tm->tm_hour / 10;
		else if (fmt[-1] == 'k')
		    *buf++ = ' ';
		*buf++ = '0' + tm->tm_hour % 10;
		break;
	    case 'l':
	    case 'L':
		hr12 = tm->tm_hour % 12;
		if (hr12 == 0)
		    hr12 = 12;
	        if (hr12 > 9)
		  *buf++ = '1';
		else if (fmt[-1] == 'l')
		  *buf++ = ' ';
		*buf++ = '0' + (hr12 % 10);
		break;
	    case 'm':
		*buf++ = '0' + (tm->tm_mon + 1) / 10;
		*buf++ = '0' + (tm->tm_mon + 1) % 10;
		break;
	    case 'M':
		*buf++ = '0' + tm->tm_min / 10;
		*buf++ = '0' + tm->tm_min % 10;
		break;
	    case 'S':
		*buf++ = '0' + tm->tm_sec / 10;
		*buf++ = '0' + tm->tm_sec % 10;
		break;
	    case 'y':
		*buf++ = '0' + (tm->tm_year / 10) % 10;
		*buf++ = '0' + tm->tm_year % 10;
		break;
#ifndef HAVE_STRFTIME
	    case 'a':
		strucpy(&buf, astr[tm->tm_wday]);
		break;
	    case 'b':
		strucpy(&buf, estr[tm->tm_mon]);
		break;
	    case 'p':
		*buf++ = (tm->tm_hour > 11) ? 'p' : 'a';
		*buf++ = 'm';
		break;
	    default:
		*buf++ = '%';
		if (fmt[-1] != '%')
		    *buf++ = fmt[-1];
#else
	    default:
		*buf = '\0';
		tmp[1] = fmt[-1];
		strftime(buf, bufsize - strlen(origbuf), tmp, tm);
		buf += strlen(buf);
#endif
		break;
	    }
	} else
	    *buf++ = *fmt++;
    *buf = '\0';
    return 0;
}

/**/
mod_export char *
zjoin(char **arr, int delim, int heap)
{
    int len = 0;
    char **s, *ret, *ptr;

    for (s = arr; *s; s++)
	len += strlen(*s) + 1;
    if (!len)
	return heap? "" : ztrdup("");
    ptr = ret = (heap ? (char *) hcalloc(len) : (char *) zcalloc(len));
    for (s = arr; *s; s++) {
	strucpy(&ptr, *s);
	if (delim)
	    *ptr++ = delim;
    }
    ptr[-1] = '\0';
    return ret;
}

/* Split a string containing a colon separated list *
 * of items into an array of strings.               */

/**/
char **
colonsplit(char *s, int uniq)
{
    int ct;
    char *t, **ret, **ptr, **p;

    for (t = s, ct = 0; *t; t++) /* count number of colons */
	if (*t == ':')
	    ct++;
    ptr = ret = (char **) zalloc(sizeof(char **) * (ct + 2));

    t = s;
    do {
	s = t;
        /* move t to point at next colon */
	for (; *t && *t != ':'; t++);
	if (uniq)
	    for (p = ret; p < ptr; p++)
		if (strlen(*p) == t - s && ! strncmp(*p, s, t - s))
		    goto cont;
	*ptr = (char *) zalloc((t - s) + 1);
	ztrncpy(*ptr++, s, t - s);
      cont: ;
    }
    while (*t++);
    *ptr = NULL;
    return ret;
}

/**/
static int
skipwsep(char **s)
{
    char *t = *s;
    int i = 0;

    while (*t && iwsep(*t == Meta ? t[1] ^ 32 : *t)) {
	if (*t == Meta)
	    t++;
	t++;
	i++;
    }
    *s = t;
    return i;
}

/* see findsep() below for handling of `quote' argument */

/**/
mod_export char **
spacesplit(char *s, int allownull, int heap, int quote)
{
    char *t, **ret, **ptr;
    int l = sizeof(*ret) * (wordcount(s, NULL, -!allownull) + 1);
    char *(*dup)(const char *) = (heap ? dupstring : ztrdup);

    ptr = ret = (heap ? (char **) hcalloc(l) : (char **) zcalloc(l));

    if (quote) {
	/*
	 * we will be stripping quoted separators by hacking string,
	 * so make sure it's hackable.
	 */
	s = dupstring(s);
    }

    t = s;
    skipwsep(&s);
    if (*s && isep(*s == Meta ? s[1] ^ 32 : *s))
	*ptr++ = dup(allownull ? "" : nulstring);
    else if (!allownull && t != s)
	*ptr++ = dup("");
    while (*s) {
	if (isep(*s == Meta ? s[1] ^ 32 : *s) || (quote && *s == '\\')) {
	    if (*s == Meta)
		s++;
	    s++;
	    skipwsep(&s);
	}
	t = s;
	findsep(&s, NULL, quote);
	if (s > t || allownull) {
	    *ptr = (heap ? (char *) hcalloc((s - t) + 1) :
		    (char *) zcalloc((s - t) + 1));
	    ztrncpy(*ptr++, t, s - t);
	} else
	    *ptr++ = dup(nulstring);
	t = s;
	skipwsep(&s);
    }
    if (!allownull && t != s)
	*ptr++ = dup("");
    *ptr = NULL;
    return ret;
}

/**/
static int
findsep(char **s, char *sep, int quote)
{
    /*
     * *s is the string we are looking along, which will be updated
     * to the point we have got to.
     *
     * sep is a possibly multicharacter separator to look for.  If NULL,
     * use normal separator characters.
     *
     * quote is a flag that '\<sep>' should not be treated as a separator.
     * in this case we need to be able to strip the backslash directly
     * in the string, so the calling function must have sent us something
     * modifiable.  currently this only works for sep == NULL.  also in
     * in this case only, we need to turn \\ into \.
     */
    int i;
    char *t, *tt;

    if (!sep) {
	for (t = *s; *t; t++) {
	    if (quote && *t == '\\' &&
		(isep(t[1] == Meta ? (t[2] ^ 32) : t[1]) || t[1] == '\\')) {
		chuck(t);
		if (*t == Meta)
		    t++;
		continue;
	    }
	    if (*t == Meta) {
		if (isep(t[1] ^ 32))
		    break;
		t++;
	    } else if (isep(*t))
		break;
	}
	i = t - *s;
	*s = t;
	return i;
    }
    if (!sep[0]) {
	if (**s) {
	    if (**s == Meta)
		*s += 2;
	    else
		++*s;
	    return 1;
	}
	return -1;
    }
    for (i = 0; **s; i++) {
	for (t = sep, tt = *s; *t && *tt && *t == *tt; t++, tt++);
	if (!*t)
	    return i;
	if (*(*s)++ == Meta) {
#ifdef DEBUG
	    if (! *(*s)++)
		fprintf(stderr, "BUG: unexpected end of string in findsep()\n");
#else
	    (*s)++;
#endif
	}
    }
    return -1;
}

/**/
char *
findword(char **s, char *sep)
{
    char *r, *t;
    int sl;

    if (!**s)
	return NULL;

    if (sep) {
	sl = strlen(sep);
	r = *s;
	while (! findsep(s, sep, 0)) {
	    r = *s += sl;
	}
	return r;
    }
    for (t = *s; *t; t++) {
	if (*t == Meta) {
	    if (! isep(t[1] ^ 32))
		break;
	    t++;
	} else if (! isep(*t))
	    break;
    }
    *s = t;
    findsep(s, sep, 0);
    return t;
}

/**/
int
wordcount(char *s, char *sep, int mul)
{
    int r, sl, c;

    if (sep) {
	r = 1;
	sl = strlen(sep);
	for (; (c = findsep(&s, sep, 0)) >= 0; s += sl)
	    if ((c && *(s + sl)) || mul)
		r++;
    } else {
	char *t = s;

	r = 0;
	if (mul <= 0)
	    skipwsep(&s);
	if ((*s && isep(*s == Meta ? s[1] ^ 32 : *s)) ||
	    (mul < 0 && t != s))
	    r++;
	for (; *s; r++) {
	    if (isep(*s == Meta ? s[1] ^ 32 : *s)) {
		if (*s == Meta)
		    s++;
		s++;
		if (mul <= 0)
		    skipwsep(&s);
	    }
	    findsep(&s, NULL, 0);
	    t = s;
	    if (mul <= 0)
		skipwsep(&s);
	}
	if (mul < 0 && t != s)
	    r++;
    }
    return r;
}

/**/
mod_export char *
sepjoin(char **s, char *sep, int heap)
{
    char *r, *p, **t;
    int l, sl;
    char sepbuf[3];

    if (!*s)
	return heap ? "" : ztrdup("");
    if (!sep) {
	sep = sepbuf;
	sepbuf[0] = *ifs;
	sepbuf[1] = *ifs == Meta ? ifs[1] ^ 32 : '\0';
	sepbuf[2] = '\0';
    }
    sl = strlen(sep);
    for (t = s, l = 1 - sl; *t; l += strlen(*t) + sl, t++);
    r = p = (heap ? (char *) hcalloc(l) : (char *) zcalloc(l));
    t = s;
    while (*t) {
	strucpy(&p, *t);
	if (*++t)
	    strucpy(&p, sep);
    }
    *p = '\0';
    return r;
}

/**/
char **
sepsplit(char *s, char *sep, int allownull, int heap)
{
    int n, sl;
    char *t, *tt, **r, **p;

    if (!sep)
	return spacesplit(s, allownull, heap, 0);

    sl = strlen(sep);
    n = wordcount(s, sep, 1);
    r = p = (heap ? (char **) hcalloc((n + 1) * sizeof(char *)) :
	     (char **) zcalloc((n + 1) * sizeof(char *)));

    for (t = s; n--;) {
	tt = t;
	findsep(&t, sep, 0);
	*p = (heap ? (char *) hcalloc(t - tt + 1) :
	      (char *) zcalloc(t - tt + 1));
	strncpy(*p, tt, t - tt);
	(*p)[t - tt] = '\0';
	p++;
	t += sl;
    }
    *p = NULL;

    return r;
}

/* Get the definition of a shell function */

/**/
mod_export Eprog
getshfunc(char *nam)
{
    Shfunc shf;

    if (!(shf = (Shfunc) shfunctab->getnode(shfunctab, nam)))
	return &dummy_eprog;

    return shf->funcdef;
}

/**/
char **
mkarray(char *s)
{
    char **t = (char **) zalloc((s) ? (2 * sizeof s) : (sizeof s));

    if ((*t = s))
	t[1] = NULL;
    return t;
}

/**/
mod_export void
zbeep(void)
{
    char *vb;
    queue_signals();
    if ((vb = getsparam("ZBEEP"))) {
	int len;
	vb = getkeystring(vb, &len, 2, NULL);
	write(SHTTY, vb, len);
    } else if (isset(BEEP))
	write(SHTTY, "\07", 1);
    unqueue_signals();
}

/**/
mod_export void
freearray(char **s)
{
    char **t = s;

    DPUTS(!s, "freearray() with zero argument");

    while (*s)
	zsfree(*s++);
    free(t);
}

/**/
int
equalsplit(char *s, char **t)
{
    for (; *s && *s != '='; s++);
    if (*s == '=') {
	*s++ = '\0';
	*t = s;
	return 1;
    }
    return 0;
}

/* the ztypes table */

/**/
mod_export short int typtab[256];

/* initialize the ztypes table */

/**/
void
inittyptab(void)
{
    int t0;
    char *s;

    for (t0 = 0; t0 != 256; t0++)
	typtab[t0] = 0;
    for (t0 = 0; t0 != 32; t0++)
	typtab[t0] = typtab[t0 + 128] = ICNTRL;
    typtab[127] = ICNTRL;
    for (t0 = '0'; t0 <= '9'; t0++)
	typtab[t0] = IDIGIT | IALNUM | IWORD | IIDENT | IUSER;
    for (t0 = 'a'; t0 <= 'z'; t0++)
	typtab[t0] = typtab[t0 - 'a' + 'A'] = IALPHA | IALNUM | IIDENT | IUSER | IWORD;
    for (t0 = 0240; t0 != 0400; t0++)
	typtab[t0] = IALPHA | IALNUM | IIDENT | IUSER | IWORD;
    typtab['_'] = IIDENT | IUSER;
    typtab['-'] = IUSER;
    typtab[' '] |= IBLANK | INBLANK;
    typtab['\t'] |= IBLANK | INBLANK;
    typtab['\n'] |= INBLANK;
    typtab['\0'] |= IMETA;
    typtab[STOUC(Meta)  ] |= IMETA;
    typtab[STOUC(Marker)] |= IMETA;
    for (t0 = (int)STOUC(Pound); t0 <= (int)STOUC(Nularg); t0++)
	typtab[t0] |= ITOK | IMETA;
    for (s = ifs ? ifs : DEFAULT_IFS; *s; s++) {
	if (inblank(*s)) {
	    if (s[1] == *s)
		s++;
	    else
		typtab[STOUC(*s)] |= IWSEP;
	}
	typtab[STOUC(*s == Meta ? *++s ^ 32 : *s)] |= ISEP;
    }
    for (s = wordchars ? wordchars : DEFAULT_WORDCHARS; *s; s++)
	typtab[STOUC(*s == Meta ? *++s ^ 32 : *s)] |= IWORD;
    for (s = SPECCHARS; *s; s++)
	typtab[STOUC(*s)] |= ISPECIAL;
    if (isset(BANGHIST) && bangchar && interact && isset(SHINSTDIN))
	typtab[bangchar] |= ISPECIAL;
}

/**/
mod_export char **
arrdup(char **s)
{
    char **x, **y;

    y = x = (char **) zhalloc(sizeof(char *) * (arrlen(s) + 1));

    while ((*x++ = dupstring(*s++)));

    return y;
}

/**/
mod_export char **
zarrdup(char **s)
{
    char **x, **y;

    y = x = (char **) zalloc(sizeof(char *) * (arrlen(s) + 1));

    while ((*x++ = ztrdup(*s++)));

    return y;
}

/**/
static char *
spname(char *oldname)
{
    char *p, spnameguess[PATH_MAX + 1], spnamebest[PATH_MAX + 1];
    static char newname[PATH_MAX + 1];
    char *new = newname, *old;
    int bestdist = 200, thisdist;

    old = oldname;
    for (;;) {
	while (*old == '/')
	    *new++ = *old++;
	*new = '\0';
	if (*old == '\0')
	    return newname;
	p = spnameguess;
	for (; *old != '/' && *old != '\0'; old++)
	    if (p < spnameguess + PATH_MAX)
		*p++ = *old;
	*p = '\0';
	if ((thisdist = mindist(newname, spnameguess, spnamebest)) >= 3) {
	    if (bestdist < 3) {
		strcpy(new, spnameguess);
		strcat(new, old);
		return newname;
	    } else
	    	return NULL;
	} else
	    bestdist = thisdist;
	for (p = spnamebest; (*new = *p++);)
	    new++;
    }
}

/**/
static int
mindist(char *dir, char *mindistguess, char *mindistbest)
{
    int mindistd, nd;
    DIR *dd;
    char *fn;
    char buf[PATH_MAX];

    if (dir[0] == '\0')
	dir = ".";
    mindistd = 100;
    sprintf(buf, "%s/%s", dir, mindistguess);
    if (access(unmeta(buf), F_OK) == 0) {
	strcpy(mindistbest, mindistguess);
	return 0;
    }
    if (!(dd = opendir(unmeta(dir))))
	return mindistd;
    while ((fn = zreaddir(dd, 0))) {
	nd = spdist(fn, mindistguess,
		    (int)strlen(mindistguess) / 4 + 1);
	if (nd <= mindistd) {
	    strcpy(mindistbest, fn);
	    mindistd = nd;
	    if (mindistd == 0)
		break;
	}
    }
    closedir(dd);
    return mindistd;
}

/**/
static int
spdist(char *s, char *t, int thresh)
{
    char *p, *q;
    const char qwertykeymap[] =
    "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\
\t1234567890-=\t\
\tqwertyuiop[]\t\
\tasdfghjkl;'\n\t\
\tzxcvbnm,./\t\t\t\
\n\n\n\n\n\n\n\n\n\n\n\n\n\n\
\t!@#$%^&*()_+\t\
\tQWERTYUIOP{}\t\
\tASDFGHJKL:\"\n\t\
\tZXCVBNM<>?\n\n\t\
\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
    const char dvorakkeymap[] =
    "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\
\t1234567890[]\t\
\t',.pyfgcrl/=\t\
\taoeuidhtns-\n\t\
\t;qjkxbmwvz\t\t\t\
\n\n\n\n\n\n\n\n\n\n\n\n\n\n\
\t!@#$%^&*(){}\t\
\t\"<>PYFGCRL?+\t\
\tAOEUIDHTNS_\n\t\
\t:QJKXBMWVZ\n\n\t\
\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
    const char *keymap;
    if ( isset( DVORAK ) )
      keymap = dvorakkeymap;
    else
      keymap = qwertykeymap;

    if (!strcmp(s, t))
	return 0;
/* any number of upper/lower mistakes allowed (dist = 1) */
    for (p = s, q = t; *p && tulower(*p) == tulower(*q); p++, q++);
    if (!*p && !*q)
	return 1;
    if (!thresh)
	return 200;
    for (p = s, q = t; *p && *q; p++, q++)
	if (*p == *q)
	    continue;		/* don't consider "aa" transposed, ash */
	else if (p[1] == q[0] && q[1] == p[0])	/* transpositions */
	    return spdist(p + 2, q + 2, thresh - 1) + 1;
	else if (p[1] == q[0])	/* missing letter */
	    return spdist(p + 1, q + 0, thresh - 1) + 2;
	else if (p[0] == q[1])	/* missing letter */
	    return spdist(p + 0, q + 1, thresh - 1) + 2;
	else if (*p != *q)
	    break;
    if ((!*p && strlen(q) == 1) || (!*q && strlen(p) == 1))
	return 2;
    for (p = s, q = t; *p && *q; p++, q++)
	if (p[0] != q[0] && p[1] == q[1]) {
	    int t0;
	    char *z;

	/* mistyped letter */

	    if (!(z = strchr(keymap, p[0])) || *z == '\n' || *z == '\t')
		return spdist(p + 1, q + 1, thresh - 1) + 1;
	    t0 = z - keymap;
	    if (*q == keymap[t0 - 15] || *q == keymap[t0 - 14] ||
		*q == keymap[t0 - 13] ||
		*q == keymap[t0 - 1] || *q == keymap[t0 + 1] ||
		*q == keymap[t0 + 13] || *q == keymap[t0 + 14] ||
		*q == keymap[t0 + 15])
		return spdist(p + 1, q + 1, thresh - 1) + 2;
	    return 200;
	} else if (*p != *q)
	    break;
    return 200;
}

/* set cbreak mode, or the equivalent */

/**/
void
setcbreak(void)
{
    struct ttyinfo ti;

    ti = shttyinfo;
#ifdef HAS_TIO
    ti.tio.c_lflag &= ~ICANON;
    ti.tio.c_cc[VMIN] = 1;
    ti.tio.c_cc[VTIME] = 0;
#else
    ti.sgttyb.sg_flags |= CBREAK;
#endif
    settyinfo(&ti);
}

/* give the tty to some process */

/**/
mod_export void
attachtty(pid_t pgrp)
{
    static int ep = 0;

    if (jobbing) {
#ifdef HAVE_TCSETPGRP
	if (SHTTY != -1 && tcsetpgrp(SHTTY, pgrp) == -1 && !ep)
#else
# if ardent
	if (SHTTY != -1 && setpgrp() == -1 && !ep)
# else
	int arg = pgrp;

	if (SHTTY != -1 && ioctl(SHTTY, TIOCSPGRP, &arg) == -1 && !ep)
# endif
#endif
	{
	    if (pgrp != mypgrp && kill(-pgrp, 0) == -1)
		attachtty(mypgrp);
	    else {
		if (errno != ENOTTY)
		{
		    zwarn("can't set tty pgrp: %e", NULL, errno);
		    fflush(stderr);
		}
		opts[MONITOR] = 0;
		ep = 1;
	    }
	}
    }
}

/* get the process group associated with the tty */

/**/
pid_t
gettygrp(void)
{
    pid_t arg;

    if (SHTTY == -1)
	return -1;

#ifdef HAVE_TCSETPGRP
    arg = tcgetpgrp(SHTTY);
#else
    ioctl(SHTTY, TIOCGPGRP, &arg);
#endif

    return arg;
}

/* Return the output baudrate */

#ifdef HAVE_SELECT
/**/
long
getbaudrate(struct ttyinfo *shttyinfo)
{
    long speedcode;

#ifdef HAS_TIO
# if defined(HAVE_TCGETATTR) && defined(HAVE_TERMIOS_H)
    speedcode = cfgetospeed(&shttyinfo->tio);
# else
    speedcode = shttyinfo->tio.c_cflag & CBAUD;
# endif
#else
    speedcode = shttyinfo->sgttyb.sg_ospeed;
#endif

    switch (speedcode) {
    case B0:
	return (0L);
    case B50:
	return (50L);
    case B75:
	return (75L);
    case B110:
	return (110L);
    case B134:
	return (134L);
    case B150:
	return (150L);
    case B200:
	return (200L);
    case B300:
	return (300L);
    case B600:
	return (600L);
#ifdef _B900
    case _B900:
	return (900L);
#endif
    case B1200:
	return (1200L);
    case B1800:
	return (1800L);
    case B2400:
	return (2400L);
#ifdef _B3600
    case _B3600:
	return (3600L);
#endif
    case B4800:
	return (4800L);
#ifdef _B7200
    case _B7200:
	return (7200L);
#endif
    case B9600:
	return (9600L);
#ifdef B19200
    case B19200:
	return (19200L);
#else
# ifdef EXTA
    case EXTA:
	return (19200L);
# endif
#endif
#ifdef B38400
    case B38400:
	return (38400L);
#else
# ifdef EXTB
    case EXTB:
	return (38400L);
# endif
#endif
#ifdef B57600
    case B57600:
	return (57600L);
#endif
#ifdef B115200
    case B115200:
	return (115200L);
#endif
#ifdef B230400
    case B230400:
	return (230400L);
#endif
#ifdef B460800
    case B460800:
	return (460800L);
#endif
    default:
	if (speedcode >= 100)
	    return speedcode;
	break;
    }
    return (0L);
}
#endif

/* Escape tokens and null characters.  Buf is the string which should be     *
 * escaped.  len is the length of the string.  If len is -1, buf should be   *
 * null terminated.  If len is non-negative and the third parameter is not   *
 * META_DUP, buf should point to an at least len+1 long memory area.  The    *
 * return value points to the quoted string.  If the given string does not   *
 * contain any special character which should be quoted and the third        *
 * parameter is not META_(HEAP|)DUP, buf is returned unchanged (a            *
 * terminating null character is appended to buf if necessary).  Otherwise   *
 * the third `heap' argument determines the method used to allocate space    *
 * for the result.  It can have the following values:                        *
 *   META_REALLOC:  use zrealloc on buf                                      *
 *   META_HREALLOC: use hrealloc on buf                                      *
 *   META_USEHEAP:  get memory from the heap.  This leaves buf unchanged.    *
 *   META_NOALLOC:  buf points to a memory area which is long enough to hold *
 *                  the quoted form, just quote it and return buf.           *
 *   META_STATIC:   store the quoted string in a static area.  The original  *
 *                  string should be at most PATH_MAX long.                   *
 *   META_ALLOC:    allocate memory for the new string with zalloc().        *
 *   META_DUP:      leave buf unchanged and allocate space for the return    *
 *                  value even if buf does not contains special characters   *
 *   META_HEAPDUP:  same as META_DUP, but uses the heap                      */

/**/
mod_export char *
metafy(char *buf, int len, int heap)
{
    int meta = 0;
    char *t, *p, *e;
    static char mbuf[PATH_MAX*2+1];

    if (len == -1) {
	for (e = buf, len = 0; *e; len++)
	    if (imeta(*e++))
		meta++;
    } else
	for (e = buf; e < buf + len;)
	    if (imeta(*e++))
		meta++;

    if (meta || heap == META_DUP || heap == META_HEAPDUP) {
	switch (heap) {
	case META_REALLOC:
	    buf = zrealloc(buf, len + meta + 1);
	    break;
	case META_HREALLOC:
	    buf = hrealloc(buf, len, len + meta + 1);
	    break;
	case META_ALLOC:
	case META_DUP:
	    buf = memcpy(zalloc(len + meta + 1), buf, len);
	    break;
	case META_USEHEAP:
	case META_HEAPDUP:
	    buf = memcpy(zhalloc(len + meta + 1), buf, len);
	    break;
	case META_STATIC:
#ifdef DEBUG
	    if (len > PATH_MAX) {
		fprintf(stderr, "BUG: len = %d > PATH_MAX in metafy\n", len);
		fflush(stderr);
	    }
#endif
	    buf = memcpy(mbuf, buf, len);
	    break;
#ifdef DEBUG
	case META_NOALLOC:
	    break;
	default:
	    fprintf(stderr, "BUG: metafy called with invalid heap value\n");
	    fflush(stderr);
	    break;
#endif
	}
	p = buf + len;
	e = t = buf + len + meta;
	while (meta) {
	    if (imeta(*--t = *--p)) {
		*t-- ^= 32;
		*t = Meta;
		meta--;
	    }
	}
    }
    *e = '\0';
    return buf;
}

/**/
mod_export char *
unmetafy(char *s, int *len)
{
    char *p, *t;

    for (p = s; *p && *p != Meta; p++);
    for (t = p; (*t = *p++);)
	if (*t++ == Meta)
	    t[-1] = *p++ ^ 32;
    if (len)
	*len = t - s;
    return s;
}

/* Return the character length of a metafied substring, given the      *
 * unmetafied substring length.                                        */

/**/
mod_export int
metalen(const char *s, int len)
{
    int mlen = len;

    while (len--) {
	if (*s++ == Meta) {
	    mlen++;
	    s++;
	}
    }
    return mlen;
}

/* This function converts a zsh internal string to a form which can be *
 * passed to a system call as a filename.  The result is stored in a   *
 * single static area.  NULL returned if the result is longer than     *
 * 4 * PATH_MAX.                                                       */

/**/
mod_export char *
unmeta(const char *file_name)
{
    static char fn[4 * PATH_MAX];
    char *p;
    const char *t;

    for (t = file_name, p = fn; *t && p < fn + 4 * PATH_MAX - 1; p++)
	if ((*p = *t++) == Meta)
	    *p = *t++ ^ 32;
    if (*t)
	return NULL;
    if (p - fn == t - file_name)
	return (char *) file_name;
    *p = '\0';
    return fn;
}

/* Unmetafy and compare two strings, with unsigned characters. *
 * "a\0" sorts after "a".                                      */

/**/
int
ztrcmp(unsigned char const *s1, unsigned char const *s2)
{
    int c1, c2;

    while(*s1 && *s1 == *s2) {
	s1++;
	s2++;
    }

    if(!(c1 = *s1))
	c1 = -1;
    else if(c1 == STOUC(Meta))
	c1 = *++s1 ^ 32;
    if(!(c2 = *s2))
	c2 = -1;
    else if(c2 == STOUC(Meta))
	c2 = *++s2 ^ 32;

    if(c1 == c2)
	return 0;
    else if(c1 < c2)
	return -1;
    else
	return 1;
}

/* Return zero if the metafied string s and the non-metafied,  *
 * len-long string r are the same.  Return -1 if r is a prefix *
 * of s.  Return 1 if r is the lowercase version of s.  Return *
 * 2 is r is the lowercase prefix of s and return 3 otherwise. */

/**/
mod_export int
metadiffer(char const *s, char const *r, int len)
{
    int l = len;

    while (l-- && *s && *r++ == (*s == Meta ? *++s ^ 32 : *s))
	s++;
    if (*s && l < 0)
	return -1;
    if (l < 0)
	return 0;
    if (!*s)
	return 3;
    s -= len - l - 1;
    r -= len - l;
    while (len-- && *s && *r++ == tulower(*s == Meta ? *++s ^ 32 : *s))
	s++;
    if (*s && len < 0)
	return 2;
    if (len < 0)
	return 1;
    return 3;
}

/* Return the unmetafied length of a metafied string. */

/**/
mod_export int
ztrlen(char const *s)
{
    int l;

    for (l = 0; *s; l++)
	if (*s++ == Meta) {
#ifdef DEBUG
	    if (! *s)
		fprintf(stderr, "BUG: unexpected end of string in ztrlen()\n");
	    else
#endif
	    s++;
	}
    return l;
}

/* Subtract two pointers in a metafied string. */

/**/
mod_export int
ztrsub(char const *t, char const *s)
{
    int l = t - s;

    while (s != t)
	if (*s++ == Meta) {
#ifdef DEBUG
	    if (! *s || s == t)
		fprintf(stderr, "BUG: substring ends in the middle of a metachar in ztrsub()\n");
	    else
#endif
	    s++;
	    l--;
	}
    return l;
}

/**/
mod_export char *
zreaddir(DIR *dir, int ignoredots)
{
    struct dirent *de;

    do {
	de = readdir(dir);
	if(!de)
	    return NULL;
    } while(ignoredots && de->d_name[0] == '.' &&
	(!de->d_name[1] || (de->d_name[1] == '.' && !de->d_name[2])));

    return metafy(de->d_name, -1, META_STATIC);
}

/* Unmetafy and output a string.  Tokens are skipped. */

/**/
mod_export int
zputs(char const *s, FILE *stream)
{
    int c;

    while (*s) {
	if (*s == Meta)
	    c = *++s ^ 32;
	else if(itok(*s)) {
	    s++;
	    continue;
	} else
	    c = *s;
	s++;
	if (fputc(c, stream) < 0)
	    return EOF;
    }
    return 0;
}

/* Create a visibly-represented duplicate of a string. */

/**/
static char *
nicedup(char const *s, int heap)
{
    int c, len = strlen(s) * 5;
    VARARR(char, buf, len);
    char *p = buf, *n;

    while ((c = *s++)) {
	if (itok(c)) {
	    if (c <= Comma)
		c = ztokens[c - Pound];
	    else 
		continue;
	}
	if (c == Meta)
	    c = *s++ ^ 32;
	n = nicechar(c);
	while(*n)
	    *p++ = *n++;
    }
    return metafy(buf, p - buf, (heap ? META_HEAPDUP : META_DUP));
}

/**/
mod_export char *
niceztrdup(char const *s)
{
    return nicedup(s, 0);
}

/**/
mod_export char *
nicedupstring(char const *s)
{
    return nicedup(s, 1);
}

/* Unmetafy and output a string, displaying special characters readably. */

/**/
mod_export int
nicezputs(char const *s, FILE *stream)
{
    int c;

    while ((c = *s++)) {
	if (itok(c)) {
	    if (c <= Comma)
		c = ztokens[c - Pound];
	    else 
		continue;
	}
	if (c == Meta)
	    c = *s++ ^ 32;
	if(fputs(nicechar(c), stream) < 0)
	    return EOF;
    }
    return 0;
}

/* Return the length of the visible representation of a metafied string. */

/**/
mod_export size_t
niceztrlen(char const *s)
{
    size_t l = 0;
    int c;

    while ((c = *s++)) {
	if (itok(c)) {
	    if (c <= Comma)
		c = ztokens[c - Pound];
	    else 
		continue;
	}
	if (c == Meta)
	    c = *s++ ^ 32;
	l += strlen(nicechar(STOUC(c)));
    }
    return l;
}

/* check for special characters in the string */

/**/
mod_export int
hasspecial(char const *s)
{
    for (; *s; s++)
	if (ispecial(*s == Meta ? *++s ^ 32 : *s))
	    return 1;
    return 0;
}

/* Quote the string s and return the result.  If e is non-zero, the         *
 * pointer it points to may point to a position in s and in e the position  *
 * of the corresponding character in the quoted string is returned.         *
 * The last argument should be zero if this is to be used outside a string, *
 * one if it is to be quoted for the inside of a single quoted string,      *
 * two if it is for the inside of a double quoted string, and               *
 * three if it is for the inside of a posix quoted string.                  *
 * The string may be metafied and contain tokens.                           */

/**/
mod_export char *
bslashquote(const char *s, char **e, int instring)
{
    const char *u, *tt;
    char *v;
    char *buf = hcalloc(4 * strlen(s) + 1);
    int sf = 0;

    tt = v = buf;
    u = s;
    for (; *u; u++) {
	if (e && *e == u)
	    *e = v, sf = 1;
	if (instring == 3) {
	  int c = *u;
	  if (c == Meta) {
	    c = *++u ^ 32;
	  }
	  c &= 0xff;
	  if(isprint(c)) {
	    switch (c) {
	    case '\\':
	    case '\'':
	      *v++ = '\\';
	      *v++ = c;
	      break;

	    default:
	      if(imeta(c)) {
		*v++ = Meta;
		*v++ = c ^ 32;
	      }
	      else {
		if (isset(BANGHIST) && c == bangchar) {
		  *v++ = '\\';
		}
		*v++ = c;
	      }
	      break;
	    }
	  }
	  else {
	    switch (c) {
	    case '\0':
	      *v++ = '\\';
	      *v++ = '0';
	      if ('0' <= u[1] && u[1] <= '7') {
		*v++ = '0';
		*v++ = '0';
	      }
	      break;

	    case '\007': *v++ = '\\'; *v++ = 'a'; break;
	    case '\b': *v++ = '\\'; *v++ = 'b'; break;
	    case '\f': *v++ = '\\'; *v++ = 'f'; break;
	    case '\n': *v++ = '\\'; *v++ = 'n'; break;
	    case '\r': *v++ = '\\'; *v++ = 'r'; break;
	    case '\t': *v++ = '\\'; *v++ = 't'; break;
	    case '\v': *v++ = '\\'; *v++ = 'v'; break;

	    default:
	      *v++ = '\\';
	      *v++ = '0' + ((c >> 6) & 7);
	      *v++ = '0' + ((c >> 3) & 7);
	      *v++ = '0' + (c & 7);
	      break;
	    }
	  }
	  continue;
	}
	else if (*u == Tick || *u == Qtick) {
	    char c = *u++;

	    *v++ = c;
	    while (*u && *u != c)
		*v++ = *u++;
	    *v++ = c;
	    if (!*u)
		u--;
	    continue;
	}
	else if ((*u == String || *u == Qstring) &&
		 (u[1] == Inpar || u[1] == Inbrack || u[1] == Inbrace)) {
	    char c = (u[1] == Inpar ? Outpar : (u[1] == Inbrace ?
						Outbrace : Outbrack));
	    char beg = *u;
	    int level = 0;

	    *v++ = *u++;
	    *v++ = *u++;
	    while (*u && (*u != c || level)) {
		if (*u == beg)
		    level++;
		else if (*u == c)
		    level--;
		*v++ = *u++;
	    }
	    if (*u)
		*v++ = *u;
	    else
		u--;
	    continue;
	}
	else if (ispecial(*u) &&
		 ((*u != '=' && *u != '~') ||
		  u == s ||
		  (isset(MAGICEQUALSUBST) && (u[-1] == '=' || u[-1] == ':')) ||
		  (*u == '~' && isset(EXTENDEDGLOB))) &&
	    (!instring ||
	     (isset(BANGHIST) && *u == (char)bangchar && instring != 1) ||
	     (instring == 2 &&
	      (*u == '$' || *u == '`' || *u == '\"' || *u == '\\')) ||
	     (instring == 1 && *u == '\''))) {
	    if (*u == '\n' || (instring == 1 && *u == '\'')) {
		if (unset(RCQUOTES)) {
		    *v++ = '\'';
		    if (*u == '\'')
			*v++ = '\\';
		    *v++ = *u;
		    *v++ = '\'';
		} else if (*u == '\n')
		    *v++ = '"', *v++ = '\n', *v++ = '"';
		else
		    *v++ = '\'', *v++ = '\'';
		continue;
	    } else
		*v++ = '\\';
	}
	if(*u == Meta)
	    *v++ = *u++;
	*v++ = *u;
    }
    *v = '\0';

    if (e && *e == u)
	*e = v, sf = 1;
    DPUTS(e && !sf, "BUG: Wild pointer *e in bslashquote()");

    return buf;
}

/* Unmetafy and output a string, quoted if it contains special characters. */

/**/
mod_export int
quotedzputs(char const *s, FILE *stream)
{
    int inquote = 0, c;

    /* check for empty string */
    if(!*s)
	return fputs("''", stream);

    if (!hasspecial(s))
	return zputs(s, stream);

    if (isset(RCQUOTES)) {
	/* use rc-style quotes-within-quotes for the whole string */
	if(fputc('\'', stream) < 0)
	    return EOF;
	while(*s) {
	    if (*s == Meta)
		c = *++s ^ 32;
	    else
		c = *s;
	    s++;
	    if (c == '\'') {
		if(fputc('\'', stream) < 0)
		    return EOF;
	    } else if(c == '\n' && isset(CSHJUNKIEQUOTES)) {
		if(fputc('\\', stream) < 0)
		    return EOF;
	    }
	    if(fputc(c, stream) < 0)
		return EOF;
	}
	if(fputc('\'', stream) < 0)
	    return EOF;
    } else {
	/* use Bourne-style quoting, avoiding empty quoted strings */
	while(*s) {
	    if (*s == Meta)
		c = *++s ^ 32;
	    else
		c = *s;
	    s++;
	    if (c == '\'') {
		if(inquote) {
		    if(fputc('\'', stream) < 0)
			return EOF;
		    inquote=0;
		}
		if(fputs("\\'", stream) < 0)
		    return EOF;
	    } else {
		if (!inquote) {
		    if(fputc('\'', stream) < 0)
			return EOF;
		    inquote=1;
		}
		if(c == '\n' && isset(CSHJUNKIEQUOTES)) {
		    if(fputc('\\', stream) < 0)
			return EOF;
		}
		if(fputc(c, stream) < 0)
		    return EOF;
	    }
	}
	if (inquote) {
	    if(fputc('\'', stream) < 0)
		return EOF;
	}
    }
    return 0;
}

/* Double-quote a metafied string. */

/**/
mod_export char *
dquotedztrdup(char const *s)
{
    int len = strlen(s) * 4 + 2;
    char *buf = zalloc(len);
    char *p = buf, *ret;

    if(isset(CSHJUNKIEQUOTES)) {
	int inquote = 0;

	while(*s) {
	    int c = *s++;

	    if (c == Meta)
		c = *s++ ^ 32;
	    switch(c) {
		case '"':
		case '$':
		case '`':
		    if(inquote) {
			*p++ = '"';
			inquote = 0;
		    }
		    *p++ = '\\';
		    *p++ = c;
		    break;
		default:
		    if(!inquote) {
			*p++ = '"';
			inquote = 1;
		    }
		    if(c == '\n')
			*p++ = '\\';
		    *p++ = c;
		    break;
	    }
	}
	if (inquote)
	    *p++ = '"';
    } else {
	int pending = 0;

	*p++ = '"';
	while(*s) {
	    int c = *s++;

	    if (c == Meta)
		c = *s++ ^ 32;
	    switch(c) {
		case '\\':
		    if(pending)
			*p++ = '\\';
		    *p++ = '\\';
		    pending = 1;
		    break;
		case '"':
		case '$':
		case '`':
		    if(pending)
			*p++ = '\\';
		    *p++ = '\\';
		    /* fall through */
		default:
		    *p++ = c;
		    pending = 0;
		    break;
	    }
	}
	if(pending)
	    *p++ = '\\';
	*p++ = '"';
    }
    ret = metafy(buf, p - buf, META_DUP);
    zfree(buf, len);
    return ret;
}

/* Unmetafy and output a string, double quoting it in its entirety. */

#if 0 /**/
int
dquotedzputs(char const *s, FILE *stream)
{
    char *d = dquotedztrdup(s);
    int ret = zputs(d, stream);

    zsfree(d);
    return ret;
}
#endif

# if defined(HAVE_NL_LANGINFO) && defined(CODESET) && !defined(__STDC_ISO_10646__)
/* Convert a character from UCS4 encoding to UTF-8 */

size_t
ucs4toutf8(char *dest, unsigned int wval)
{
    size_t len;

    if (wval < 0x80)
      len = 1;
    else if (wval < 0x800)
      len = 2;
    else if (wval < 0x10000)
      len = 3;
    else if (wval < 0x200000)
      len = 4;
    else if (wval < 0x4000000)
      len = 5;
    else
      len = 6;

    switch (len) { /* falls through except to the last case */
    case 6: dest[5] = (wval & 0x3f) | 0x80; wval >>= 6;
    case 5: dest[4] = (wval & 0x3f) | 0x80; wval >>= 6;
    case 4: dest[3] = (wval & 0x3f) | 0x80; wval >>= 6;
    case 3: dest[2] = (wval & 0x3f) | 0x80; wval >>= 6;
    case 2: dest[1] = (wval & 0x3f) | 0x80; wval >>= 6;
	*dest = wval | (0xfc << (6 - len)) & 0xfc;
	break;
    case 1: *dest = wval;
    }

    return len;
}
#endif

/*
 * Decode a key string, turning it into the literal characters.
 * The length is returned in len.
 * fromwhere determines how the processing works.
 *   0:  Don't handle keystring, just print-like escapes.
 *       Expects misc to be present.
 *   1:  Handle Emacs-like \C-X arguments etc., but not ^X
 *       Expects misc to be present.
 *   2:  Handle ^X as well as emacs-like keys; don't handle \c
 *       for no newlines.
 *   3:  As 1, but don't handle \c.
 *   4:  Do $'...' quoting.  Overwrites the existing string instead of
 *       zhalloc'ing. If \uNNNN ever generates multi-byte chars longer
 *       than 6 bytes, will need to adjust this to re-allocate memory.
 *   5:  As 2, but \- is special.  Expects misc to be defined.
 *   6:  As 2, but parses only one character and returns end-pointer
 *       and parsed character in *misc
 */

/**/
mod_export char *
getkeystring(char *s, int *len, int fromwhere, int *misc)
{
    char *buf, tmp[1];
    char *t, *u = NULL;
    char svchar = '\0';
    int meta = 0, control = 0;
    int i;
#if defined(HAVE_WCHAR_H) && defined(HAVE_WCTOMB) && defined(__STDC_ISO_10646__)
    wint_t wval;
    size_t count;
#else
    unsigned int wval;
# if defined(HAVE_NL_LANGINFO) && defined(CODESET) && (defined(HAVE_ICONV) || defined(HAVE_LIBICONV))
    iconv_t cd;
    char inbuf[4];
    size_t inbytes, outbytes;
    char *inptr;
    size_t count;
# endif
#endif

    if (fromwhere == 6)
	t = buf = tmp;
    else if (fromwhere != 4)
	t = buf = zhalloc(strlen(s) + 1);
    else {
	t = buf = s;
	s += 2;
    }
    for (; *s; s++) {
	if (*s == '\\' && s[1]) {
	    switch (*++s) {
	    case 'a':
#ifdef __STDC__
		*t++ = '\a';
#else
		*t++ = '\07';
#endif
		break;
	    case 'n':
		*t++ = '\n';
		break;
	    case 'b':
		*t++ = '\b';
		break;
	    case 't':
		*t++ = '\t';
		break;
	    case 'v':
		*t++ = '\v';
		break;
	    case 'f':
		*t++ = '\f';
		break;
	    case 'r':
		*t++ = '\r';
		break;
	    case 'E':
		if (!fromwhere) {
		    *t++ = '\\', s--;
		    continue;
		}
	    case 'e':
		*t++ = '\033';
		break;
	    case 'M':
		if (fromwhere) {
		    if (s[1] == '-')
			s++;
		    meta = 1 + control;	/* preserve the order of ^ and meta */
		} else
		    *t++ = '\\', s--;
		continue;
	    case 'C':
		if (fromwhere) {
		    if (s[1] == '-')
			s++;
		    control = 1;
		} else
		    *t++ = '\\', s--;
		continue;
	    case Meta:
		*t++ = '\\', s--;
		break;
	    case '-':
		if (fromwhere == 5) {
		    *misc  = 1;
		    break;
		}
		goto def;
	    case 'c':
		if (fromwhere < 2) {
		    *misc = 1;
		    break;
		}
	    case 'u':
	    case 'U':
	    	wval = 0;
		for (i=(*s == 'u' ? 4 : 8); i>0; i--) {
		    if (*++s && idigit(*s))
		        wval = wval * 16 + (*s - '0');
		    else if (*s && ((*s >= 'a' && *s <= 'f') ||
				    (*s >= 'A' && *s <= 'F')))
		        wval = wval * 16 + (*s & 0x1f) + 9;
		    else {
		    	s--;
		        break;
		    }
		}
    	    	if (fromwhere == 6) {
		    *misc = wval;
		    return s+1;
		}
#if defined(HAVE_WCHAR_H) && defined(HAVE_WCTOMB) && defined(__STDC_ISO_10646__)
		count = wctomb(t, (wchar_t)wval);
		if (count == (size_t)-1) {
		    zerr("character not in range", NULL, 0);
		    if (fromwhere == 4) {
			for (u = t; (*u++ = *++s););
			return t;
		    }
		    *t = '\0';
		    *len = t - buf;
		    return buf;
		}
		t += count;  
		continue;
# else
#  if defined(HAVE_NL_LANGINFO) && defined(CODESET)
		if (!strcmp(nl_langinfo(CODESET), "UTF-8")) {
		    t += ucs4toutf8(t, wval);
		    continue;
		} else {
#   if defined(HAVE_ICONV) || defined(HAVE_LIBICONV)
    	    	    inbytes = 4;
		    outbytes = 6;
    	    	    inptr = inbuf;
		    /* assume big endian convention for UCS-4 */
		    for (i=3;i>=0;i--) {
			inbuf[i] = wval & 0xff;
			wval >>= 8;
		    }

    	    	    cd = iconv_open(nl_langinfo(CODESET), "ISO-10646");
		    if (cd == (iconv_t)-1) {
			zerr("cannot do charset conversion", NULL, 0);
			if (fromwhere == 4) {
			    for (u = t; (*u++ = *++s););
			    return t;
			}
			*t = '\0';
			*len = t - buf;
			return buf;
		    }
                    count = iconv(cd, (char **)&inptr, &inbytes, &t, &outbytes);
		    iconv_close(cd);
		    if (count == (size_t)-1) {
                        zerr("cannot do charset conversion", NULL, 0);
		        *t = '\0';
			*len = t - buf;
			return buf;
		    }
		    continue;
#   else
                    zerr("cannot do charset conversion", NULL, 0);
		    *t = '\0';
		    *len = t - buf;
		    return buf;
#   endif
		}
#  else
                zerr("cannot do charset conversion", NULL, 0);
		*t = '\0';
		*len = t - buf;
		return buf;
#  endif
# endif
	    default:
	    def:
		if ((idigit(*s) && *s < '8') || *s == 'x') {
		    if (!fromwhere) {
			if (*s == '0')
			    s++;
			else if (*s != 'x') {
			    *t++ = '\\', s--;
			    continue;
			}
		    }
		    if (s[1] && s[2] && s[3]) {
			svchar = s[3];
			s[3] = '\0';
			u = s;
		    }
		    *t++ = zstrtol(s + (*s == 'x'), &s,
				   (*s == 'x') ? 16 : 8);
		    if (svchar) {
			u[3] = svchar;
			svchar = '\0';
		    }
		    s--;
		} else {
		    if (!fromwhere && *s != '\\')
			*t++ = '\\';
		    *t++ = *s;
		}
		break;
	    }
	} else if (fromwhere == 4 && *s == Snull) {
	    for (u = t; (*u++ = *s++););
	    return t + 1;
	} else if (*s == '^' && !control &&
		   (fromwhere == 2 || fromwhere == 5 || fromwhere == 6)) {
	    control = 1;
	    continue;
	} else if (*s == Meta)
	    *t++ = *++s ^ 32;
	else
	    *t++ = *s;
	if (meta == 2) {
	    t[-1] |= 0x80;
	    meta = 0;
	}
	if (control) {
	    if (t[-1] == '?')
		t[-1] = 0x7f;
	    else
		t[-1] &= 0x9f;
	    control = 0;
	}
	if (meta) {
	    t[-1] |= 0x80;
	    meta = 0;
	}
	if (fromwhere == 4 && imeta(t[-1])) {
	    *t = t[-1] ^ 32;
	    t[-1] = Meta;
	    t++;
	}
	if (fromwhere == 6 && t != tmp) {
	    *misc = STOUC(tmp[0]);
	    return s + 1;
	}
    }
    DPUTS(fromwhere == 4, "BUG: unterminated $' substitution");
    *t = '\0';
    *len = t - buf;
    return buf;
}

/* Return non-zero if s is a prefix of t. */

/**/
mod_export int
strpfx(char *s, char *t)
{
    while (*s && *s == *t)
	s++, t++;
    return !*s;
}

/* Return non-zero if s is a suffix of t. */

/**/
mod_export int
strsfx(char *s, char *t)
{
    int ls = strlen(s), lt = strlen(t);

    if (ls <= lt)
	return !strcmp(t + lt - ls, s);
    return 0;
}

/**/
static int
upchdir(int n)
{
    char buf[PATH_MAX];
    char *s;
    int err = -1;

    while (n > 0) {
	for (s = buf; s < buf + PATH_MAX - 4 && n--; )
	    *s++ = '.', *s++ = '.', *s++ = '/';
	s[-1] = '\0';
	if (chdir(buf))
	    return err;
	err = -2;
    }
    return 0;
}

/* Change directory, without following symlinks.  Returns 0 on success, -1 *
 * on failure.  Sets errno to ENOTDIR if any symlinks are encountered.  If *
 * fchdir() fails, or the current directory is unreadable, we might end up *
 * in an unwanted directory in case of failure.                            */

/**/
mod_export int
lchdir(char const *path, struct dirsav *d, int hard)
{
    char const *pptr;
    int level;
    struct stat st1;
    struct dirsav ds;
#ifdef HAVE_LSTAT
    char buf[PATH_MAX + 1], *ptr;
    int err;
    struct stat st2;
#endif

    if (!d) {
	ds.ino = ds.dev = 0;
	ds.dirname = NULL;
	ds.dirfd = -1;
	d = &ds;
    }
#ifdef HAVE_LSTAT
    if ((*path == '/' || !hard) &&
	(d != &ds || hard)){
#else
    if (*path == '/') {
#endif
	level = -1;
#ifdef HAVE_FCHDIR
	if (d->dirfd < 0 && (d->dirfd = open(".", O_RDONLY | O_NOCTTY)) < 0 &&
	    zgetdir(d) && *d->dirname != '/')
	    d->dirfd = open("..", O_RDONLY | O_NOCTTY);
#else
	if (!d->dirname)
	    zgetdir(d);
#endif
    } else {
	level = 0;
	if (!d->dev && !d->ino) {
	    stat(".", &st1);
	    d->dev = st1.st_dev;
	    d->ino = st1.st_ino;
	}
    }

#ifdef HAVE_LSTAT
    if (!hard)
#endif
    {
	if (d != &ds) {
	    for (pptr = path; *pptr; level++) {
		while (*pptr && *pptr++ != '/');
		while (*pptr == '/')
		    pptr++;
	    }
	    d->level = level;
	}
	return zchdir((char *) path);
    }
#ifdef HAVE_LSTAT
    if (*path == '/')
	chdir("/");
    for(;;) {
	while(*path == '/')
	    path++;
	if(!*path) {
	    if (d == &ds) {
		zsfree(ds.dirname);
		if (ds.dirfd >=0)
		    close(ds.dirfd);
	    } else
		d->level = level;
	    return 0;
	}
	for(pptr = path; *++pptr && *pptr != '/'; ) ;
	if(pptr - path > PATH_MAX) {
	    err = ENAMETOOLONG;
	    break;
	}
	for(ptr = buf; path != pptr; )
	    *ptr++ = *path++;
	*ptr = 0;
	if(lstat(buf, &st1)) {
	    err = errno;
	    break;
	}
	if(!S_ISDIR(st1.st_mode)) {
	    err = ENOTDIR;
	    break;
	}
	if(chdir(buf)) {
	    err = errno;
	    break;
	}
	if (level >= 0)
	    level++;
	if(lstat(".", &st2)) {
	    err = errno;
	    break;
	}
	if(st1.st_dev != st2.st_dev || st1.st_ino != st2.st_ino) {
	    err = ENOTDIR;
	    break;
	}
    }
    if (restoredir(d)) {
	if (d == &ds) {
	    zsfree(ds.dirname);
	    if (ds.dirfd >=0)
		close(ds.dirfd);
	}
	errno = err;
	return -2;
    }
    if (d == &ds) {
	zsfree(ds.dirname);
	if (ds.dirfd >=0)
	    close(ds.dirfd);
    }
    errno = err;
    return -1;
#endif /* HAVE_LSTAT */
}

/**/
mod_export int
restoredir(struct dirsav *d)
{
    int err = 0;
    struct stat sbuf;

    if (d->dirname && *d->dirname == '/')
	return chdir(d->dirname);
#ifdef HAVE_FCHDIR
    if (d->dirfd >= 0) {
	if (!fchdir(d->dirfd)) {
	    if (!d->dirname) {
		return 0;
	    } else if (chdir(d->dirname)) {
		close(d->dirfd);
		d->dirfd = -1;
		err = -2;
	    }
	} else {
	    close(d->dirfd);
	    d->dirfd = err = -1;
	}
    } else
#endif
    if (d->level > 0)
	err = upchdir(d->level);
    else if (d->level < 0)
	err = -1;
    if (d->dev || d->ino) {
	stat(".", &sbuf);
	if (sbuf.st_ino != d->ino || sbuf.st_dev != d->dev)
	    err = -2;
    }
    return err;
}

/* Get a signal number from a string */

/**/
mod_export int
getsignum(char *s)
{
    int x, i;

    /* check for a signal specified by number */
    x = atoi(s);
    if (idigit(*s) && x >= 0 && x < VSIGCOUNT)
	return x;

    /* search for signal by name */
    for (i = 0; i < VSIGCOUNT; i++)
	if (!strcmp(s, sigs[i]))
	    return i;

    /* no matching signal */
    return -1;
}

/* Check whether the shell is running with privileges in effect.  *
 * This is the case if EITHER the euid is zero, OR (if the system *
 * supports POSIX.1e (POSIX.6) capability sets) the process'      *
 * Effective or Inheritable capability sets are non-empty.        */

/**/
int
privasserted(void)
{
    if(!geteuid())
	return 1;
#ifdef HAVE_CAP_GET_PROC
    {
	cap_t caps = cap_get_proc();
	if(caps) {
	    /* POSIX doesn't define a way to test whether a capability set *
	     * is empty or not.  Typical.  I hope this is conforming...    */
	    cap_flag_value_t val;
	    cap_value_t n;
	    for(n = 0; !cap_get_flag(caps, n, CAP_EFFECTIVE, &val); n++)
		if(val) {
		    cap_free(caps);
		    return 1;
		}
	    cap_free(caps);
	}
    }
#endif /* HAVE_CAP_GET_PROC */
    return 0;
}

#ifdef DEBUG

/**/
mod_export void
dputs(char *message)
{
    fprintf(stderr, "%s\n", message);
    fflush(stderr);
}

#endif /* DEBUG */

/**/
mod_export int
mode_to_octal(mode_t mode)
{
    int m = 0;

    if(mode & S_ISUID)
	m |= 04000;
    if(mode & S_ISGID)
	m |= 02000;
    if(mode & S_ISVTX)
	m |= 01000;
    if(mode & S_IRUSR)
	m |= 00400;
    if(mode & S_IWUSR)
	m |= 00200;
    if(mode & S_IXUSR)
	m |= 00100;
    if(mode & S_IRGRP)
	m |= 00040;
    if(mode & S_IWGRP)
	m |= 00020;
    if(mode & S_IXGRP)
	m |= 00010;
    if(mode & S_IROTH)
	m |= 00004;
    if(mode & S_IWOTH)
	m |= 00002;
    if(mode & S_IXOTH)
	m |= 00001;
    return m;
}

#ifdef MAILDIR_SUPPORT
/*
 *     Stat a file. If it's a maildir, check all messages
 *     in the maildir and present the grand total as a file.
 *     The fields in the 'struct stat' are from the mail directory.
 *     The following fields are emulated:
 *
 *     st_nlink        always 1
 *     st_size         total number of bytes in all files
 *     st_blocks       total number of messages
 *     st_atime        access time of newest file in maildir
 *     st_mtime        modify time of newest file in maildir
 *     st_mode         S_IFDIR changed to S_IFREG
 *
 *     This is good enough for most mail-checking applications.
 */

/**/
int
mailstat(char *path, struct stat *st)
{
       DIR                     *dd;
       struct                  dirent *fn;
       struct stat             st_ret, st_tmp;
       static struct stat      st_ret_last;
       char                    *dir, *file = 0;
       int                     i;
       time_t                  atime = 0, mtime = 0;
       size_t                  plen = strlen(path), dlen;

       /* First see if it's a directory. */
       if ((i = stat(path, st)) != 0 || !S_ISDIR(st->st_mode))
               return i;

       st_ret = *st;
       st_ret.st_nlink = 1;
       st_ret.st_size  = 0;
       st_ret.st_blocks  = 0;
       st_ret.st_mode  &= ~S_IFDIR;
       st_ret.st_mode  |= S_IFREG;

       /* See if cur/ is present */
       dir = appstr(ztrdup(path), "/cur");
       if (stat(dir, &st_tmp) || !S_ISDIR(st_tmp.st_mode)) return 0;
       st_ret.st_atime = st_tmp.st_atime;

       /* See if tmp/ is present */
       dir[plen] = 0;
       dir = appstr(dir, "/tmp");
       if (stat(dir, &st_tmp) || !S_ISDIR(st_tmp.st_mode)) return 0;
       st_ret.st_mtime = st_tmp.st_mtime;

       /* And new/ */
       dir[plen] = 0;
       dir = appstr(dir, "/new");
       if (stat(dir, &st_tmp) || !S_ISDIR(st_tmp.st_mode)) return 0;
       st_ret.st_mtime = st_tmp.st_mtime;

#if THERE_IS_EXACTLY_ONE_MAILDIR_IN_MAILPATH
       {
       static struct stat      st_new_last;
       /* Optimization - if new/ didn't change, nothing else did. */
       if (st_tmp.st_dev == st_new_last.st_dev &&
           st_tmp.st_ino == st_new_last.st_ino &&
           st_tmp.st_atime == st_new_last.st_atime &&
           st_tmp.st_mtime == st_new_last.st_mtime) {
	   *st = st_ret_last;
	   return 0;
       }
       st_new_last = st_tmp;
       }
#endif

       /* Loop over new/ and cur/ */
       for (i = 0; i < 2; i++) {
	   dir[plen] = 0;
	   dir = appstr(dir, i ? "/cur" : "/new");
	   if ((dd = opendir(dir)) == NULL) {
	       zsfree(file);
	       zsfree(dir);
	       return 0;
	   }
	   dlen = strlen(dir) + 1; /* include the "/" */
	   while ((fn = readdir(dd)) != NULL) {
	       if (fn->d_name[0] == '.')
		   continue;
	       if (file) {
		   file[dlen] = 0;
		   file = appstr(file, fn->d_name);
	       } else {
		   file = tricat(dir, "/", fn->d_name);
	       }
	       if (stat(file, &st_tmp) != 0)
		   continue;
	       st_ret.st_size += st_tmp.st_size;
	       st_ret.st_blocks++;
	       if (st_tmp.st_atime != st_tmp.st_mtime &&
		   st_tmp.st_atime > atime)
		   atime = st_tmp.st_atime;
	       if (st_tmp.st_mtime > mtime)
		   mtime = st_tmp.st_mtime;
	   }
	   closedir(dd);
       }
       zsfree(file);
       zsfree(dir);

       if (atime) st_ret.st_atime = atime;
       if (mtime) st_ret.st_mtime = mtime;

       *st = st_ret_last = st_ret;
       return 0;
}
#endif
