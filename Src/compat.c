/*
 * compat.c - compatibiltiy routines for the deprived
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


/**/
char *
zgetdir(struct dirsav *d)
{
    char nbuf[PATH_MAX+3];
    char *buf;
    int bufsiz, pos, len;
    struct stat sbuf;
    struct dirent *de;
    DIR *dir;
    ino_t ino, pino;
    dev_t dev, pdev;

    buf = halloc(bufsiz = PATH_MAX);
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
	    char *newbuf = halloc(2*bufsiz);
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
 * faliliure and -2 when chdir failed and the current directory is lost.  */

/**/
int
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
