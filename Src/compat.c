/*
 * compat.c - compatibility routines for the deprived
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
#include "compat.pro"

/* Return pointer to first occurence of string t *
 * in string s.  Return NULL if not present.     */

#ifndef HAVE_STRSTR
char *
strstr(const char *s, const char *t)
{
    char *p1, *p2;
 
    for (; *s; s++) {
        for (p1 = s, p2 = t; *p2; p1++, p2++)
            if (*p1 != *p2)
                break;
        if (!*p2)
            return (char *)s;
    }
    return NULL;
}
#endif


#ifndef HAVE_GETHOSTNAME
int
gethostname(char *name, size_t namelen)
{
    struct utsname uts;

    uname(&uts);
    if(strlen(uts.nodename) >= namelen) {
	errno = EINVAL;
	return -1;
    }
    strcpy(name, uts.nodename);
    return 0;
}
#endif


#ifndef HAVE_GETTIMEOFDAY
int
gettimeofday(struct timeval *tv, struct timezone *tz)
{
    tv->tv_usec = 0;
    tv->tv_sec = (long)time((time_t) 0);
    return 0;
}
#endif


/* compute the difference between two calendar times */

#ifndef HAVE_DIFFTIME
double
difftime(time_t t2, time_t t1)
{
    return ((double)t2 - (double)t1);
}
#endif


#ifndef HAVE_STRERROR
extern char *sys_errlist[];

/* Get error message string associated with a particular  *
 * error number, and returns a pointer to that string.    *
 * This is not a particularly robust version of strerror. */

char *
strerror(int errnum)
{
    return (sys_errlist[errnum]);
}
#endif


#if 0
/* pathconf(_PC_PATH_MAX) is not currently useful to zsh.  The value *
 * returned varies depending on a number of factors, e.g. the amount *
 * of memory available to the operating system at a given time; thus *
 * it can't be used for buffer allocation, or even as an indication  *
 * of whether an attempt to use or create a given pathname may fail  *
 * at any future time.                                               *
 *                                                                   *
 * The call is also permitted to fail if the argument path is not an *
 * existing directory, so even to make sense of that one must search *
 * for a valid directory somewhere in the path and adjust.  Even if  *
 * it succeeds, the return value is relative to the input directory, *
 * and therefore potentially relative to the length of the shortest  *
 * path either to that directory or to our working directory.        *
 *                                                                   *
 * Finally, see the note below for glibc; detection of pathconf() is *
 * not by itself an indication that it works reliably.               */

/* The documentation for pathconf() says something like:             *
 *     The limit is returned, if one exists.  If the system  does    *
 *     not  have  a  limit  for  the  requested  resource,  -1 is    *
 *     returned, and errno is unchanged.  If there is  an  error,    *
 *     -1  is returned, and errno is set to reflect the nature of    *
 *     the error.                                                    *
 *                                                                   *
 * System calls are not permitted to set errno to 0; but we must (or *
 * some other flag value) in order to determine that the resource is *
 * unlimited.  What use is leaving errno unchanged?  Instead, define *
 * a wrapper that resets errno to 0 and returns 0 for "the system    *
 * does not have a limit," so that -1 always means a real error.     */

/**/
mod_export long
zpathmax(char *dir)
{
#ifdef HAVE_PATHCONF
    long pathmax;

    errno = 0;
    if ((pathmax = pathconf(dir, _PC_PATH_MAX)) >= 0) {
	/* Some versions of glibc pathconf return a hardwired value! */
	return pathmax;
    } else if (errno == EINVAL || errno == ENOENT || errno == ENOTDIR) {
	/* Work backward to find a directory, until we run out of path. */
	char *tail = strrchr(dir, '/');
	while (tail > dir && tail[-1] == '/')
	    --tail;
	if (tail > dir) {
	    *tail = 0;
	    pathmax = zpathmax(dir);
	    *tail = '/';
	} else {
	    errno = 0;
	    if (tail)
		pathmax = pathconf("/", _PC_PATH_MAX);
	    else
		pathmax = pathconf(".", _PC_PATH_MAX);
	}
	if (pathmax > 0) {
	    long taillen = (tail ? strlen(tail) : (strlen(dir) + 1));
	    if (taillen < pathmax)
		return pathmax - taillen;
	    else
		errno = ENAMETOOLONG;
	}
    }
    if (errno)
	return -1;
    else
	return 0; /* pathmax should be considered unlimited */
#else
    long dirlen = strlen(dir);

    /* The following is wrong if dir is not an absolute path. */
    return ((long) ((dirlen >= PATH_MAX) ?
		    ((errno = ENAMETOOLONG), -1) :
		    ((errno = 0), PATH_MAX - dirlen)));
#endif
}
#endif /* 0 */

#ifdef HAVE_SYSCONF
/* This is replaced by a macro from system.h if not HAVE_SYSCONF.    *
 * 0 is returned by sysconf if _SC_OPEN_MAX is unavailable;          *
 * -1 is returned on error                                           *
 *                                                                   *
 * Neither of these should happen, but resort to OPEN_MAX rather     *
 * than return 0 or -1 just in case.                                 */

/**/
mod_export long
zopenmax(void)
{
    static long openmax = 0;

    if (openmax < 1) {
	if ((openmax = sysconf(_SC_OPEN_MAX)) < 1) {
	    openmax = OPEN_MAX;
	} else if (openmax > OPEN_MAX) {
	    /* On some systems, "limit descriptors unlimited" or the  *
	     * equivalent will set openmax to a huge number.  Unless  *
	     * there actually is a file descriptor > OPEN_MAX already *
	     * open, nothing in zsh requires the true maximum, and in *
	     * fact it causes inefficiency elsewhere if we report it. *
	     * So, report the maximum of OPEN_MAX or the largest open *
	     * descriptor (is there a better way to find that?).      */
	    long i, j = OPEN_MAX;
	    for (i = j; i < openmax; i += (errno != EINTR)) {
		errno = 0;
		if (fcntl(i, F_GETFL, 0) < 0 &&
		    (errno == EBADF || errno == EINTR))
		    continue;
		j = i;
	    }
	    openmax = j;
	}
    }

    return (max_zsh_fd > openmax) ? max_zsh_fd : openmax;
}
#endif

/**/
mod_export char *
zgetdir(struct dirsav *d)
{
    char nbuf[PATH_MAX+3];
    char *buf;
    int bufsiz, pos;
    struct stat sbuf;
    ino_t pino;
    dev_t pdev;
#if !defined(__CYGWIN__) && !defined(USE_GETCWD)
    struct dirent *de;
    DIR *dir;
    dev_t dev;
    ino_t ino;
    int len;
#endif

    buf = zhalloc(bufsiz = PATH_MAX);
    pos = bufsiz - 1;
    buf[pos] = '\0';
    strcpy(nbuf, "../");
    if (stat(".", &sbuf) < 0) {
	if (d)
	    return NULL;
	buf[0] = '.';
	buf[1] = '\0';
	return buf;
    }

    pino = sbuf.st_ino;
    pdev = sbuf.st_dev;
    if (d)
	d->ino = pino, d->dev = pdev;
#ifdef HAVE_FCHDIR
    else
#endif
#if !defined(__CYGWIN__) && !defined(USE_GETCWD)
	holdintr();

    for (;;) {
	if (stat("..", &sbuf) < 0)
	    break;

	ino = pino;
	dev = pdev;
	pino = sbuf.st_ino;
	pdev = sbuf.st_dev;

	if (ino == pino && dev == pdev) {
	    if (!buf[pos])
		buf[--pos] = '/';
	    if (d) {
#ifndef HAVE_FCHDIR
		zchdir(buf + pos);
		noholdintr();
#endif
		return d->dirname = ztrdup(buf + pos);
	    }
	    zchdir(buf + pos);
	    noholdintr();
	    return buf + pos;
	}

	if (!(dir = opendir("..")))
	    break;

	while ((de = readdir(dir))) {
	    char *fn = de->d_name;
	    /* Ignore `.' and `..'. */
	    if (fn[0] == '.' &&
		(fn[1] == '\0' ||
		 (fn[1] == '.' && fn[2] == '\0')))
		continue;
#ifdef HAVE_STRUCT_DIRENT_D_STAT
	    if(de->d_stat.st_dev == dev && de->d_stat.st_ino == ino) {
		strncpy(nbuf + 3, fn, PATH_MAX);
		break;
	    }
#else /* !HAVE_STRUCT_DIRENT_D_STAT */
# ifdef HAVE_STRUCT_DIRENT_D_INO
	    if (dev != pdev || (ino_t) de->d_ino == ino)
# endif /* HAVE_STRUCT_DIRENT_D_INO */
	    {
		strncpy(nbuf + 3, fn, PATH_MAX);
		lstat(nbuf, &sbuf);
		if (sbuf.st_dev == dev && sbuf.st_ino == ino)
		    break;
	    }
#endif /* !HAVE_STRUCT_DIRENT_D_STAT */
	}
	closedir(dir);
	if (!de)
	    break;
	len = strlen(nbuf + 2);
	pos -= len;
	while (pos <= 1) {
	    char *newbuf = zhalloc(2*bufsiz);
	    memcpy(newbuf + bufsiz, buf, bufsiz);
	    buf = newbuf;
	    pos += bufsiz;
	    bufsiz *= 2;
	}
	memcpy(buf + pos, nbuf + 2, len);
#ifdef HAVE_FCHDIR
	if (d)
	    return d->dirname = ztrdup(buf + pos + 1);
#endif
	if (chdir(".."))
	    break;
    }
    if (d) {
#ifndef HAVE_FCHDIR
	if (*buf)
	    zchdir(buf + pos + 1);
	noholdintr();
#endif
	return NULL;
    }
    if (*buf)
	zchdir(buf + pos + 1);
    noholdintr();

#else  /* __CYGWIN__, USE_GETCWD cases */

    if (!getcwd(buf, bufsiz)) {
	if (d) {
	    return NULL;
	}
    } else {
	if (d) {
	    return d->dirname = ztrdup(buf);
	}
	return buf;
    }
#endif

    buf[0] = '.';
    buf[1] = '\0';
    return buf;
}

/**/
char *
zgetcwd(void)
{
    return zgetdir(NULL);
}

/* chdir with arbitrary long pathname.  Returns 0 on success, 0 on normal *
 * failure and -2 when chdir failed and the current directory is lost.  */

/**/
mod_export int
zchdir(char *dir)
{
    char *s;
    int currdir = -2;

    for (;;) {
	if (!*dir)
	    return 0;
	if (!chdir(dir))
	    return 0;
	if ((errno != ENAMETOOLONG && errno != ENOMEM) ||
	    strlen(dir) < PATH_MAX)
	    break;
	for (s = dir + PATH_MAX - 1; s > dir && *s != '/'; s--);
	if (s == dir)
	    break;
#ifdef HAVE_FCHDIR
	if (currdir == -2)
	    currdir = open(".", O_RDONLY|O_NOCTTY);
#endif
	*s = '\0';
	if (chdir(dir)) {
	    *s = '/';
	    break;
	}
#ifndef HAVE_FCHDIR
	currdir = -1;
#endif
	*s = '/';
	while (*++s == '/');
	dir = s;
    }
#ifdef HAVE_FCHDIR
    if (currdir == -1 || (currdir >= 0 && fchdir(currdir))) {
	if (currdir >= 0)
	    close(currdir);
	return -2;
    }
    if (currdir >= 0)
	close(currdir);
    return -1;
#else
    return currdir == -2 ? -1 : -2;
#endif
}

/*
 * How to print out a 64 bit integer.  This isn't needed (1) if longs
 * are 64 bit, since ordinary %ld will work (2) if we couldn't find a
 * 64 bit type anyway.
 */
/**/
#ifdef ZSH_64_BIT_TYPE
/**/
mod_export char *
output64(zlong val)
{
    static char llbuf[DIGBUFSIZE];
    convbase(llbuf, val, 0);
    return llbuf;
}
/**/
#endif /* ZSH_64_BIT_TYPE */
