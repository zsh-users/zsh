/*
 * files.c - file operation builtins
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 1996-1997 Andrew Main
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Andrew Main or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Andrew Main and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Andrew Main and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Andrew Main and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "files.mdh"

typedef int (*MoveFunc) _((char const *, char const *));

#ifndef STDC_HEADERS
extern int link _((const char *, const char *));
extern int symlink _((const char *, const char *));
extern int rename _((const char *, const char *));
#endif

#include "files.pro"

/**/
static int
ask(void)
{
    int a = getchar(), c;
    for(c = a; c != EOF && c != '\n'; )
	c = getchar();
    return a == 'y' || a == 'Y';
}

/* sync builtin */

/**/
static int
bin_sync(char *nam, char **args, char *ops, int func)
{
    sync();
    return 0;
}

/* mkdir builtin */

/**/
static int
bin_mkdir(char *nam, char **args, char *ops, int func)
{
    mode_t oumask = umask(0);
    mode_t mode = 0777 & ~oumask;
    int err = 0;

    umask(oumask);
    if(ops['m']) {
	char *str = *args++, *ptr;

	if(!*args) {
	    zwarnnam(nam, "not enough arguments", NULL, 0);
	    return 1;
	}
	mode = zstrtol(str, &ptr, 8);
	if(!*str || *ptr) {
	    zwarnnam(nam, "invalid mode `%s'", str, 0);
	    return 1;
	}
    }
    for(; *args; args++) {
	char *ptr = strchr(*args, 0);

	while(ptr > *args + (**args == '/') && *--ptr == '/')
	    *ptr = 0;
	if(ztrlen(*args) > PATH_MAX - 1) {
	    zwarnnam(nam, "%s: %e", *args, ENAMETOOLONG);
	    err = 1;
	    continue;
	}
	if(ops['p']) {
	    char *ptr = *args;

	    for(;;) {
		while(*ptr == '/')
		    ptr++;
		while(*ptr && *ptr != '/')
		    ptr++;
		if(!*ptr) {
		    err |= domkdir(nam, *args, mode, 1);
		    break;
		} else {
		    int e;

		    *ptr = 0;
		    e = domkdir(nam, *args, mode | 0300, 1);
		    if(e) {
			err = 1;
			break;
		    }
		    *ptr = '/';
		}
	    }
	} else
	    err |= domkdir(nam, *args, mode, 0);
    }
    return err;
}

/**/
static int
domkdir(char *nam, char *path, mode_t mode, int p)
{
    int err;
    mode_t oumask;
    char const *rpath = unmeta(path);

    if(p) {
	struct stat st;

	if(!lstat(rpath, &st) && S_ISDIR(st.st_mode))
	    return 0;
    }
    oumask = umask(0);
    err = mkdir(path, mode) ? errno : 0;
    umask(oumask);
    if(!err)
	return 0;
    zwarnnam(nam, "cannot make directory `%s': %e", path, err);
    return 1;
}

/* rmdir builtin */

/**/
static int
bin_rmdir(char *nam, char **args, char *ops, int func)
{
    int err = 0;

    for(; *args; args++) {
	char *rpath = unmeta(*args);

	if(!rpath) {
	    zwarnnam(nam, "%s: %e", *args, ENAMETOOLONG);
	    err = 1;
	} else if(rmdir(rpath)) {
	    zwarnnam(nam, "cannot remove directory `%s': %e", *args, errno);
	    err = 1;
	}
    }
    return err;
}

/* ln and mv builtins */

#define BIN_LN 0
#define BIN_MV 1

#define MV_NODIRS (1<<0)
#define MV_FORCE  (1<<1)
#define MV_INTER  (1<<2)
#define MV_ASKNW  (1<<3)
#define MV_ATOMIC (1<<4)

/* bin_ln actually does three related jobs: hard linking, symbolic *
 * linking, and renaming.  If called as mv it renames, otherwise   *
 * it looks at the -s option.  If hard linking, it will refuse to  *
 * attempt linking to a directory unless the -d option is given.   */

/**/
static int
bin_ln(char *nam, char **args, char *ops, int func)
{
    MoveFunc move;
    int flags, space, err = 0;
    char **a, *ptr, *rp;
    struct stat st;
    char buf[PATH_MAX * 2 + 1];


    if(func == BIN_MV) {
	move = rename;
	flags = ops['f'] ? 0 : MV_ASKNW;
	flags |= MV_ATOMIC;
    } else {
	flags = ops['f'] ? MV_FORCE : 0;
#ifdef HAVE_LSTAT
	if(ops['s'])
	    move = symlink;
	else
#endif
	     {
	    move = link;
	    if(!ops['d'])
		flags |= MV_NODIRS;
	}
    }
    if(ops['i'] && !ops['f'])
	flags |= MV_INTER;
    for(a = args; a[1]; a++) ;
    if(a != args) {
	rp = unmeta(*a);
	if(rp && !stat(rp, &st) && S_ISDIR(st.st_mode))
	    goto havedir;
    }
    if(a > args+1) {
	zwarnnam(nam, "last of many arguments must be a directory", NULL, 0);
	return 1;
    }
    if(!args[1]) {
	ptr = strrchr(args[0], '/');
	if(ptr)
	    args[1] = ptr+1;
	else
	    args[1] = args[0];
    }
    return domove(nam, move, args[0], args[1], flags);
    havedir:
    strcpy(buf, *a);
    *a = NULL;
    space = PATH_MAX - 1 - ztrlen(buf);
    rp = strchr(buf, 0);
    *rp++ = '/';
    for(; *args; args++) {
	if(ztrlen(*args) > PATH_MAX) {
	    zwarnnam(nam, "%s: %e", *args, ENAMETOOLONG);
	    err = 1;
	    continue;
	}
	ptr = strrchr(*args, '/');
	if(ptr)
	    ptr++;
	else
	    ptr = *args;
	if(ztrlen(ptr) > space) {
	    zwarnnam(nam, "%s: %e", ptr, ENAMETOOLONG);
	    err = 1;
	    continue;
	}
	strcpy(rp, ptr);
	err |= domove(nam, move, *args, buf, flags);
    }
    return err;
}

/**/
static int
domove(char *nam, MoveFunc move, char *p, char *q, int flags)
{
    struct stat st;
    char *qbuf;
    char pbuf[PATH_MAX + 1];
    strcpy(pbuf, unmeta(p));
    qbuf = unmeta(q);
    if(flags & MV_NODIRS) {
	errno = EISDIR;
	if(lstat(pbuf, &st) || S_ISDIR(st.st_mode)) {
	    zwarnnam(nam, "%s: %e", p, errno);
	    return 1;
	}
    }
    if(!lstat(qbuf, &st)) {
	int doit = flags & MV_FORCE;
	if(S_ISDIR(st.st_mode)) {
	    zwarnnam(nam, "%s: cannot overwrite directory", q, 0);
	    return 1;
	} else if(flags & MV_INTER) {
	    nicezputs(nam, stderr);
	    fputs(": replace `", stderr);
	    nicezputs(q, stderr);
	    fputs("'? ", stderr);
	    fflush(stderr);
	    if(!ask())
		return 0;
	    doit = 1;
	} else if((flags & MV_ASKNW) &&
		!S_ISLNK(st.st_mode) &&
		access(qbuf, W_OK)) {
	    nicezputs(nam, stderr);
	    fputs(": replace `", stderr);
	    nicezputs(q, stderr);
	    fprintf(stderr, "', overriding mode %04o? ",
		mode_to_octal(st.st_mode));
	    fflush(stderr);
	    if(!ask())
		return 0;
	    doit = 1;
	}
	if(doit && !(flags & MV_ATOMIC))
	    unlink(qbuf);
    }
    if(move(pbuf, qbuf)) {
	zwarnnam(nam, "%s: %e", p, errno);
	return 1;
    }
    return 0;
}

/* rm builtin */

/**/
static int
bin_rm(char *nam, char **args, char *ops, int func)
{
    int err = 0, len;
    char *rp, *s;
    struct dirsav ds;

    ds.ino = ds.dev = 0;
    ds.dirname = NULL;
    ds.dirfd = ds.level = -1;
    if (ops['r'] || ops['s']) {
	if ((ds.dirfd = open(".", O_RDONLY|O_NOCTTY)) < 0 &&
	    zgetdir(&ds) && *ds.dirname != '/')
	    ds.dirfd = open("..", O_RDONLY|O_NOCTTY);
    }
    for(; !errflag && !(err & 2) && *args; args++) {
	rp = ztrdup(*args);
	unmetafy(rp, &len);
	if (ops['s']) {
	    s = strrchr(rp, '/');
	    if (s && !s[1]) {
		while (*s == '/' && s > rp)
		    *s-- = '\0';
		while (*s != '/' && s > rp)
		    s--;
	    }
	    if (s && s[1]) {
		int e;

		*s = '\0';
		e = lchdir(s > rp ? rp : "/", &ds, 1);
		err |= -e;
		if (!e) {
		    struct dirsav d;

		    d.ino = d.dev = 0;
		    d.dirname = NULL;
		    d.dirfd = d.level = -1;
		    err |= dorm(nam, *args, s + 1, ops, &d, 0);
		    zsfree(d.dirname);
		    if (restoredir(&ds))
			err |= 2;
		} else
		    zwarnnam(nam, "%s: %e", *args, errno);
	    } else
		err |= dorm(nam, *args, rp, ops, &ds, 0);
	} else
	    err |= dorm(nam, *args, rp, ops, &ds, 1);
	zfree(rp, len + 1);
    }
    if ((err & 2) && ds.dirfd >= 0 && restoredir(&ds) && zchdir(pwd)) {
	zsfree(pwd);
	pwd = ztrdup("/");
	chdir(pwd);
    }
    if (ds.dirfd >= 0)
	close(ds.dirfd);
    zsfree(ds.dirname);
    return ops['f'] ? 0 : !!err;
}

/**/
static int
dorm(char *nam, char *arg, char *rp, char *ops, struct dirsav *ds, int first)
{
    struct stat st;

    if((!ops['d'] || !ops['f']) && !lstat(rp, &st)) {
	if(!ops['d'] && S_ISDIR(st.st_mode)) {
	    if(ops['r'])
		return dormr(nam, arg, rp, ops, ds, first);
	    if(!ops['f'])
		zwarnnam(nam, "%s: %e", arg, EISDIR);
	    return 1;
	}
	if(!ops['f'] && ops['i']) {
	    nicezputs(nam, stderr);
	    fputs(": remove `", stderr);
	    nicezputs(arg, stderr);
	    fputs("'? ", stderr);
	    fflush(stderr);
	    if(!ask())
		return 0;
	} else if(!ops['f'] &&
		!S_ISLNK(st.st_mode) &&
	    	access(rp, W_OK)) {
	    nicezputs(nam, stderr);
	    fputs(": remove `", stderr);
	    nicezputs(arg, stderr);
	    fprintf(stderr, "', overriding mode %04o? ",
		mode_to_octal(st.st_mode));
	    fflush(stderr);
	    if(!ask())
		return 0;
	}
    }
    if(!unlink(rp))
	return 0;
    if(!ops['f'])
	zwarnnam(nam, "%s: %e", arg, errno);
    return 1;
}

/**/
static int
dormr(char *nam, char *arg, char *rp, char *ops, struct dirsav *ds, int first)
{
    char *fn;
    DIR *d;
    int err;
    struct dirsav dsav;
    char *files = NULL;
    int fileslen = 0;

    err = -lchdir(rp, ds, !first);
    if (err) {
	if (!ops['f'])
	    zwarnnam(nam, "%s: %e", arg, errno);
	return err;
    }

    dsav.ino = dsav.dev = 0;
    dsav.dirname = NULL;
    dsav.dirfd = dsav.level = -1;
    d = opendir(".");
    if(!d) {
	if(!ops['f'])
	    zwarnnam(nam, "%s: %e", arg, errno);
	err = 1;
    } else {
	int arglen = strlen(arg) + 1;

	while (!errflag && (fn = zreaddir(d, 1))) {
	    int l = strlen(fn) + 1;
	    files = hrealloc(files, fileslen, fileslen + l);
	    strcpy(files + fileslen, fn);
	    fileslen += l;
	}
	closedir(d);
	for (fn = files; !errflag && !(err & 2) && fn < files + fileslen;) {
	    int l = strlen(fn) + 1;
	    VARARR(char, narg, arglen + l);

	    strcpy(narg,arg);
	    narg[arglen-1] = '/';
	    strcpy(narg + arglen, fn);
	    unmetafy(fn, NULL);
	    err |= dorm(nam, narg, fn, ops, &dsav, 0);
	    fn += l;
	}
	hrealloc(files, fileslen, 0);
    }
    zsfree(dsav.dirname);
    if (err & 2)
	return 2;
    if (restoredir(ds)) {
	if(!ops['f'])
	    zwarnnam(nam, "failed to return to previous directory: %e",
		     NULL, errno);
	return 2;
    }
    if(!ops['f'] && ops['i']) {
	nicezputs(nam, stderr);
	fputs(": remove `", stderr);
	nicezputs(arg, stderr);
	fputs("'? ", stderr);
	fflush(stderr);
	if(!ask())
	    return err;
    }
    if(!rmdir(rp))
	return err;
    if(!ops['f'])
	zwarnnam(nam, "%s: %e", arg, errno);
    return 1;
}

/* module paraphernalia */

#ifdef HAVE_LSTAT
# define LN_OPTS "dfis"
#else
# define LN_OPTS "dfi"
#endif

static struct builtin bintab[] = {
    BUILTIN("ln",    0, bin_ln,    1, -1, BIN_LN, LN_OPTS, NULL),
    BUILTIN("mkdir", 0, bin_mkdir, 1, -1, 0,      "pm",    NULL),
    BUILTIN("mv",    0, bin_ln,    2, -1, BIN_MV, "fi",    NULL),
    BUILTIN("rm",    0, bin_rm,    1, -1, 0,      "dfirs", NULL),
    BUILTIN("rmdir", 0, bin_rmdir, 1, -1, 0,      NULL,    NULL),
    BUILTIN("sync",  0, bin_sync,  0,  0, 0,      NULL,    NULL),
};

/**/
int
boot_files(Module m)
{
    return !addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
}

#ifdef MODULE

/**/
int
cleanup_files(Module m)
{
    deletebuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
    return 0;
}
#endif
