/*
 * zpty.c - sub-processes with pseudo terminals
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 2000 Sven Wischnowsky
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Sven Wischnowsky or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Sven Wischnowsky and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Sven Wischnowsky and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Sven Wischnowsky and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "zpty.mdh"
#include "zpty.pro"

typedef struct ptycmd *Ptycmd;

struct ptycmd {
    Ptycmd next;
    char *name;
    char **args;
    int fd;
    int pid;
    int echo;
    int block;
    int fin;
};

static Ptycmd ptycmds;

static int
ptynonblock(int fd)
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
    long mode;

    mode = fcntl(fd, F_GETFL, 0);
    if (mode != -1 && !(mode & NONBLOCK) &&
	!fcntl(fd, F_SETFL, mode | NONBLOCK))
	return 1;

#endif /* NONBLOCK */
    return 0;

#undef NONBLOCK
}

/**/
static int
ptygettyinfo(int fd, struct ttyinfo *ti)
{
    if (fd != -1) {
#ifdef HAVE_TERMIOS_H
# ifdef HAVE_TCGETATTR
	if (tcgetattr(fd, &ti->tio) == -1)
# else
	if (ioctl(fd, TCGETS, &ti->tio) == -1)
# endif
	    return 1;
#else
# ifdef HAVE_TERMIO_H
	ioctl(fd, TCGETA, &ti->tio);
# else
	ioctl(fd, TIOCGETP, &ti->sgttyb);
	ioctl(fd, TIOCLGET, &ti->lmodes);
	ioctl(fd, TIOCGETC, &ti->tchars);
	ioctl(fd, TIOCGLTC, &ti->ltchars);
# endif
#endif
	return 0;
    }
    return 1;
}

/**/
static void
ptysettyinfo(int fd, struct ttyinfo *ti)
{
    if (fd != -1) {
#ifdef HAVE_TERMIOS_H
# ifdef HAVE_TCGETATTR
#  ifndef TCSADRAIN
#   define TCSADRAIN 1	/* XXX Princeton's include files are screwed up */
#  endif
	tcsetattr(fd, TCSADRAIN, &ti->tio);
    /* if (tcsetattr(SHTTY, TCSADRAIN, &ti->tio) == -1) */
# else
	ioctl(fd, TCSETS, &ti->tio);
    /* if (ioctl(SHTTY, TCSETS, &ti->tio) == -1) */
# endif
	/*	zerr("settyinfo: %e",NULL,errno)*/ ;
#else
# ifdef HAVE_TERMIO_H
	ioctl(fd, TCSETA, &ti->tio);
# else
	ioctl(fd, TIOCSETN, &ti->sgttyb);
	ioctl(fd, TIOCLSET, &ti->lmodes);
	ioctl(fd, TIOCSETC, &ti->tchars);
	ioctl(fd, TIOCSLTC, &ti->ltchars);
# endif
#endif
    }
}

static Ptycmd
getptycmd(char *name)
{
    Ptycmd p;

    for (p = ptycmds; p; p = p->next)
	if (!strcmp(p->name, name))
	    return p;

    return NULL;
}

/**** maybe we should use configure here */
/**** and we certainly need more/better #if tests */

#ifdef __osf__

static int
get_pty(int *master, int *slave)
{
    return openpty(master, slave, NULL, NULL, NULL);
}

#else /* ! __osf__ */

#if defined(__SVR4) || defined(sinix)

#include <sys/stropts.h>

static int
get_pty(int *master, int *slave)
{
    int mfd, sfd;
    char *name;
    int ret;

    if ((mfd = open("/dev/ptmx", O_RDWR)) < 0)
	return 1;

    if (grantpt(mfd) || unlockpt(mfd) || !(name = ptsname(mfd))) {
	close(mfd);
	return 1;
    }
    if ((sfd = open(name, O_RDWR)) < 0) {
	close(mfd);
	return 1;
    }
    if ((ret = ioctl(sfd, I_FIND, "ptem")) != 1)
	if (ret == -1 || ioctl(sfd, I_PUSH, "ptem") == -1) {
	    close(mfd);
	    close(sfd);
	    return 1;
	}
    if ((ret = ioctl(sfd, I_FIND, "ldterm")) != 1)
	if (ret == -1 || ioctl(sfd, I_PUSH, "ldterm") == -1) {
	    close(mfd);
	    close(sfd);
	    return 1;
	}
    if ((ret = ioctl(sfd, I_FIND, "ttcompat")) != 1)
	if (ret == -1 || ioctl(sfd, I_PUSH, "ttcompat") == -1) {
	    close(mfd);
	    close(sfd);
	    return 1;
	}
    *master = mfd;
    *slave = sfd;

    return 0;
}

#else /* ! (defined(__SVR4) || defined(sinix))  */

static int
get_pty(int *master, int *slave)
{

#ifdef __linux
    static char char1[] = "abcdefghijklmnopqrstuvwxyz";
    static char char2[] = "0123456789abcdef";
#else /* ! __linux */
    static char char1[] = "pq";
    static char char2[] = "0123456789abcdef";
#endif /* __linux */

    char name[11], *p1, *p2;
    int mfd, sfd;

    strcpy(name, "/dev/ptyxx");

    for (p1 = char1; *p1; p1++) {
	name[8] = *p1;
	for (p2 = char2; *p2; p2++) {
	    name[9] = *p2;
	    if ((mfd = open(name, O_RDWR)) >= 0) {
		name[5] = 't';
		if ((sfd = open(name, O_RDWR)) >= 0) {
		    *master = mfd;
		    *slave = sfd;

		    return 0;
		}
		name[5] = 'p';
		close(mfd);
	    }
	}
    }
    return 1;
}

#endif /* __SVR4 */
#endif /* __osf__ */

static int
newptycmd(char *nam, char *pname, char **args, int echo, int block)
{
    Ptycmd p;
    int master, slave, pid;
    char *cmd;

    if (!(cmd = findcmd(*args, 1))) {
	zerrnam(nam, "unknown command: %s", *args, 0);
	return 1;
    }
    if (get_pty(&master, &slave)) {
	zerrnam(nam, "can't open pseudo terminal", NULL, 0);
	return 1;
    }
    if ((pid = fork()) == -1) {
	close(master);
	close(slave);
	zerrnam(nam, "couldn't create pty command: %s", pname, 0);
	return 1;
    } else if (!pid) {
	if (!echo) {
	    struct ttyinfo info;

	    if (!ptygettyinfo(slave, &info)) {
#ifdef HAVE_TERMIOS_H
		info.tio.c_lflag &= ~ECHO;
#else
#ifdef HAVE_TERMIO_H
		info.tio.c_lflag &= ~ECHO;
#else
		info.tio.lmodes &= ~ECHO; /**** dunno if this is right */
#endif
#endif
		ptysettyinfo(slave, &info);
	    }
	}

#ifdef TIOCGWINSZ
	if (interact) {
	    struct ttyinfo info;

	    if (ioctl(slave, TIOCGWINSZ, (char *) &info.winsize) == 0) {
		info.winsize.ws_row = lines;
		info.winsize.ws_col = columns;
		ioctl(slave, TIOCSWINSZ, (char *) &info.winsize);
	    }
	}
#endif /* TIOCGWINSZ */

	signal_default(SIGTERM);
	signal_default(SIGINT);
	signal_default(SIGQUIT);

	close(0);
	close(1);
	close(2);

	dup2(slave, 0);
	dup2(slave, 1);
	dup2(slave, 2);

	close(slave);

	execve(cmd, args, environ);
	exit(0);
    }
    master = movefd(master);
    close(slave);

    p = (Ptycmd) zalloc(sizeof(*p));

    p->name = ztrdup(pname);
    p->args = zarrdup(args);
    p->fd = master;
    p->pid = pid;
    p->echo = echo;
    p->block = block;
    p->fin = 0;

    p->next = ptycmds;
    ptycmds = p;

    if (!block)
	ptynonblock(master);

    return 0;
}

static void
deleteptycmd(Ptycmd cmd)
{
    Ptycmd p, q;

    for (q = NULL, p = ptycmds; p != cmd; q = p, p = p->next);

    if (p != cmd)
	return;

    if (q)
	q->next = p->next;
    else
	ptycmds = p->next;

    zsfree(p->name);
    freearray(p->args);

    kill(p->pid, SIGHUP);

    zclose(cmd->fd);

    zfree(p, sizeof(*p));
}

static void
deleteallptycmds(void)
{
    Ptycmd p, n;

    for (p = ptycmds; p; p = n) {
	n = p->next;
	deleteptycmd(p);
    }
}

/**** a better process handling would be nice */

static void
checkptycmd(Ptycmd cmd)
{
    if (kill(cmd->pid, 0) < 0) {
	cmd->fin = 1;
	zclose(cmd->fd);
    }
}

static int
ptyread(char *nam, Ptycmd cmd, char **args)
{
    int blen = 256, used = 0, ret;
    char *buf = (char *) zhalloc(blen + 1);
    Patprog prog = NULL;

    if (*args && args[1]) {
	char *p;

	if (args[2]) {
	    zerrnam(nam, "too many arguments", NULL, 0);
	    return 1;
	}
	p = dupstring(args[1]);
	tokenize(p);
	remnulargs(p);
	if (!(prog = patcompile(p, PAT_STATIC, NULL))) {
	    zerrnam(nam, "bad pattern: %s", args[1], 0);
	    return 1;
	}
    }
    do {
	while ((ret = read(cmd->fd, buf + used, 1)) == 1) {
	    if (++used == blen) {
		buf = hrealloc(buf, blen, blen << 1);
		blen <<= 1;
	    }
	}
	buf[used] = '\0';
    } while (prog && !pattry(prog, buf));

    if (*args)
	setsparam(*args, ztrdup(buf));
    else
	printf("%s", buf);

    return !used;
}

static int
ptywrite(Ptycmd cmd, char **args, int nonl)
{
    if (*args) {
	char sp = ' ';

	while (*args) {
	    write(cmd->fd, *args, strlen(*args));

	    if (*++args)
		write(cmd->fd, &sp, 1);
	}
	if (!nonl) {
	    sp = '\n';
	    write(cmd->fd, &sp, 1);
	}
    } else {
	int n;
	char buf[BUFSIZ];

	while ((n = read(0, buf, BUFSIZ)) > 0)
	    write(cmd->fd, buf, n);
    }
    return 0;
}

/**/
static int
bin_zpty(char *nam, char **args, char *ops, int func)
{
    if ((ops['r'] && ops['w']) ||
	((ops['r'] || ops['w']) && (ops['d'] || ops['e'] ||
				    ops['b'] || ops['L'])) ||
	(ops['n'] && (ops['b'] || ops['e'] || ops['r'] ||
		      ops['d'] || ops['L'])) ||
	(ops['d'] && (ops['b'] || ops['e'] || ops['L'])) ||
	(ops['L'] && (ops['b'] || ops['e']))) {
	zerrnam(nam, "illegal option combination", NULL, 0);
	return 1;
    }
    if (ops['r'] || ops['w']) {
	Ptycmd p;

	if (!*args) {
	    zerrnam(nam, "missing pty command name", NULL, 0);
	    return 1;
	} else if (!(p = getptycmd(*args))) {
	    zerrnam(nam, "no such pty command: %s", *args, 0);
	    return 1;
	}
	checkptycmd(p);
	if (p->fin)
	    return 1;
	return (ops['r'] ?
		ptyread(nam, p, args + 1) :
		ptywrite(p, args + 1, ops['n']));
    } else if (ops['d']) {
	Ptycmd p;
	int ret = 0;

	if (*args) {
	    while (*args)
		if ((p = getptycmd(*args++)))
		    deleteptycmd(p);
		else {
		    zwarnnam(nam, "no such pty command: %s", args[-1], 0);
		    ret = 1;
		}
	} else
	    deleteallptycmds();

	return ret;
    } else if (*args) {
	if (!args[1]) {
	    zerrnam(nam, "missing command", NULL, 0);
	    return 1;
	}
	if (getptycmd(*args)) {
	    zerrnam(nam, "pty command name already used: %s", *args, 0);
	    return 1;
	}
	return newptycmd(nam, *args, args + 1, ops['e'], ops['b']);
    } else {
	Ptycmd p;
	char **a;

	for (p = ptycmds; p; p = p->next) {
	    checkptycmd(p);
	    if (ops['L'])
		printf("%s %s%s%s ", nam, (p->echo ? "-e " : ""),
		       (p->block ? "-b " : ""), p->name);
	    else if (p->fin)
		printf("(finished) %s: ", p->name);
	    else
		printf("(%d) %s: ", p->pid, p->name);
	    for (a = p->args; *a; ) {
		quotedzputs(*a++, stdout);
		if (*a)
		    putchar(' ');
	    }
	    putchar('\n');
	}
	return 0;
    }
}

static int
ptyhook(Hookdef d, void *dummy)
{
    deleteallptycmds();
    return 0;
}

static struct builtin bintab[] = {
    BUILTIN("zpty", 0, bin_zpty, 0, -1, 0, "ebdrwLn", NULL),
};

/**/
int
setup_(Module m)
{
    return 0;
}

/**/
int
boot_(Module m)
{
    ptycmds = NULL;

    addhookfunc("exit", ptyhook);
    return !addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
}

/**/
int
cleanup_(Module m)
{
    deletehookfunc("exit", ptyhook);
    deleteallptycmds();
    deletebuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
    return 0;
}

/**/
int
finish_(Module m)
{
    return 0;
}
