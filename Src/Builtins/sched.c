/*
 * sched.c - execute commands at scheduled times
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

#include "sched.mdh"
#include "sched.pro"

/* node in sched list */

typedef struct schedcmd  *Schedcmd;

struct schedcmd {
    struct schedcmd *next;
    char *cmd;			/* command to run */
    time_t time;		/* when to run it */
};

/* the list of sched jobs pending */
 
static struct schedcmd *schedcmds;

/**/
static int
bin_sched(UNUSED(char *nam), char **argv, UNUSED(Options ops), UNUSED(int func))
{
    char *s = *argv++;
    time_t t;
    long h, m;
    struct tm *tm;
    struct schedcmd *sch, *sch2, *schl;
    int sn;

    /* If the argument begins with a -, remove the specified item from the
    schedule. */
    if (s && *s == '-') {
	sn = atoi(s + 1);

	if (!sn) {
	    zwarnnam("sched", "usage for delete: sched -<item#>.", NULL, 0);
	    return 1;
	}
	for (schl = (struct schedcmd *)&schedcmds, sch = schedcmds, sn--;
	     sch && sn; sch = (schl = sch)->next, sn--);
	if (!sch) {
	    zwarnnam("sched", "not that many entries", NULL, 0);
	    return 1;
	}
	schl->next = sch->next;
	zsfree(sch->cmd);
	zfree(sch, sizeof(struct schedcmd));

	return 0;
    }

    /* given no arguments, display the schedule list */
    if (!s) {
	char tbuf[40];

	for (sn = 1, sch = schedcmds; sch; sch = sch->next, sn++) {
	    t = sch->time;
	    tm = localtime(&t);
	    ztrftime(tbuf, 20, "%a %b %e %k:%M:%S", tm);
	    printf("%3d %s %s\n", sn, tbuf, sch->cmd);
	}
	return 0;
    } else if (!*argv) {
	/* other than the two cases above, sched *
	 *requires at least two arguments        */
	zwarnnam("sched", "not enough arguments", NULL, 0);
	return 1;
    }

    /* The first argument specifies the time to schedule the command for.  The
    remaining arguments form the command. */
    if (*s == '+') {
	/* + introduces a relative time.  The rest of the argument is an
	hour:minute offset from the current time.  Once the hour and minute
	numbers have been extracted, and the format verified, the resulting
	offset is simply added to the current time. */
	h = zstrtol(s + 1, &s, 10);
	if (*s != ':') {
	    zwarnnam("sched", "bad time specifier", NULL, 0);
	    return 1;
	}
	m = zstrtol(s + 1, &s, 10);
	if (*s) {
	    zwarnnam("sched", "bad time specifier", NULL, 0);
	    return 1;
	}
	t = time(NULL) + h * 3600 + m * 60;
    } else {
	/* If there is no +, an absolute time of day must have been given.
	This is in hour:minute format, optionally followed by a string starting
	with `a' or `p' (for a.m. or p.m.).  Characters after the `a' or `p'
	are ignored. */
	h = zstrtol(s, &s, 10);
	if (*s != ':') {
	    zwarnnam("sched", "bad time specifier", NULL, 0);
	    return 1;
	}
	m = zstrtol(s + 1, &s, 10);
	if (*s && *s != 'a' && *s != 'A' && *s != 'p' && *s != 'P') {
	    zwarnnam("sched", "bad time specifier", NULL, 0);
	    return 1;
	}
	t = time(NULL);
	tm = localtime(&t);
	t -= tm->tm_sec + tm->tm_min * 60 + tm->tm_hour * 3600;
	if (*s == 'p' || *s == 'P')
	    h += 12;
	t += h * 3600 + m * 60;
	/* If the specified time is before the current time, it must refer to
	tomorrow. */
	if (t < time(NULL))
	    t += 3600 * 24;
    }
    /* The time has been calculated; now add the new entry to the linked list
    of scheduled commands. */
    sch = (struct schedcmd *) zshcalloc(sizeof *sch);
    sch->time = t;
    sch->cmd = zjoin(argv, ' ', 0);
    sch->next = NULL;
    for (sch2 = (struct schedcmd *)&schedcmds; sch2->next; sch2 = sch2->next);
    sch2->next = sch;
    return 0;
}

/* Check scheduled commands; call this function from time to time. */

/**/
static void
checksched(void)
{
    time_t t;
    struct schedcmd *sch, *schl;

    if(!schedcmds)
	return;
    t = time(NULL);
    for (schl = (struct schedcmd *)&schedcmds, sch = schedcmds; sch;
	 sch = (schl = sch)->next) {
	if (sch->time <= t) {
	    execstring(sch->cmd, 0, 0);
	    schl->next = sch->next;
	    zsfree(sch->cmd);
	    zfree(sch, sizeof(struct schedcmd));
	    sch = schl;
	}
    }
}

static void (*p_checksched) _((void)) = checksched;
static struct linknode n_checksched = { NULL, NULL, &p_checksched };

static struct builtin bintab[] = {
    BUILTIN("sched", 0, bin_sched, 0, -1, 0, NULL, NULL),
};

/**/
int
setup_(UNUSED(Module m))
{
    return 0;
}

/**/
int
boot_(Module m)
{
    if(!addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab)))
	return 1;
    uaddlinknode(prepromptfns, &n_checksched);
    return 0;
}

/**/
int
cleanup_(Module m)
{
    struct schedcmd *sch, *schn;

    for (sch = schedcmds; sch; sch = schn) {
	schn = sch->next;
	zsfree(sch->cmd);
	zfree(sch, sizeof(*sch));
    }
    uremnode(prepromptfns, &n_checksched);
    deletebuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
    return 0;
}

/**/
int
finish_(UNUSED(Module m))
{
    return 0;
}
