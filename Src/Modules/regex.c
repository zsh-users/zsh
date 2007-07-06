/*
 * regex.c
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 2007 Phil Pennock
 * All Rights Reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Phil Pennock or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Phil Pennock and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Phil Pennock and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Phil Pennock and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "regex.mdh"
#include "regex.pro"

#include <regex.h>

/* we default to a vaguely modern syntax and set of capabilities */
#define ZREGEX_EXTENDED 0
/* if you want Basic syntax, make it an alternative options */

static void
zregex_regerrwarn(int r, regex_t *re, char *msg)
{
    char *errbuf;
    size_t errbufsz;

    errbufsz = regerror(r, re, NULL, 0);
    errbuf = zalloc(errbufsz*sizeof(char));
    regerror(r, re, errbuf, errbufsz);
    zwarn("%s: %s", msg, errbuf);
    zfree(errbuf, errbufsz);
}

/**/
static int
zcond_regex_match(char **a, int id)
{
    regex_t re;
    regmatch_t *m, *matches = NULL;
    size_t matchessz;
    char *lhstr, *rhre, *s, **arr, **x;
    int r, n, return_value, rcflags, reflags, nelem, start;

    lhstr = cond_str(a,0,0);
    rhre = cond_str(a,1,0);
    rcflags = reflags = 0;
    return_value = 0; /* 1 => matched successfully */

    switch(id) {
    case ZREGEX_EXTENDED:
	rcflags |= REG_EXTENDED;
	if (!isset(CASEMATCH))
	    rcflags |= REG_ICASE;
	r = regcomp(&re, rhre, rcflags);
	if (r) {
	    zregex_regerrwarn(r, &re, "failed to compile regex");
	    break;
	}
	/* re.re_nsub is number of parenthesized groups, we also need
	 * 1 for the 0 offset, which is the entire matched portion
	 */
	if (re.re_nsub < 0) {
	    zwarn("INTERNAL ERROR: regcomp() returned "
		    "negative subpattern count %d", re.re_nsub);
	    break;
	}
	matchessz = (re.re_nsub + 1) * sizeof(regmatch_t);
	matches = zalloc(matchessz);
	r = regexec(&re, lhstr, re.re_nsub+1, matches, reflags);
	if (r == REG_NOMATCH) /**/;
	else if (r == 0) {
	    return_value = 1;
	    if (isset(BASHREMATCH)) {
		start = 0;
		nelem = re.re_nsub + 1;
	    } else {
		start = 1;
		nelem = re.re_nsub;
	    }
	    arr = NULL; /* bogus gcc warning of used uninitialised */
	    /* entire matched portion + re_nsub substrings + NULL */
	    if (nelem) {
		arr = x = (char **) zalloc(sizeof(char *) * (nelem + 1));
		for (m = matches + start, n = start; n <= re.re_nsub; ++n, ++m, ++x) {
		    *x = ztrduppfx(lhstr + m->rm_so, m->rm_eo - m->rm_so);
		}
		*x = NULL;
	    }
	    if (isset(BASHREMATCH)) {
		setaparam("BASH_REMATCH", arr);
	    } else {
		m = matches;
		s = ztrduppfx(lhstr + m->rm_so, m->rm_eo - m->rm_so);
		setsparam("MATCH", s);
		if (nelem)
		    setaparam("match", arr);
	    }
	}
	else zregex_regerrwarn(r, &re, "regex matching error");
	break;
    default:
	DPUTS(1, "bad regex option");
	break;
    }

    if (matches)
	zfree(matches, matchessz);
    regfree(&re);
    return return_value;
}

static struct conddef cotab[] = {
    CONDDEF("regex-match", CONDF_INFIX, zcond_regex_match, 0, 0, ZREGEX_EXTENDED)
};


static struct features module_features = {
    NULL, 0,
    cotab, sizeof(cotab)/sizeof(*cotab),
    NULL, 0,
    NULL, 0,
    0
};


/**/
int
setup_(UNUSED(Module m))
{
    return 0;
}

/**/
int
features_(Module m, char ***features)
{
    *features = featuresarray(m, &module_features);
    return 0;
}

/**/
int
enables_(Module m, int **enables)
{
    return handlefeatures(m, &module_features, enables);
}

/**/
int
boot_(Module m)
{
    return 0;
}

/**/
int
cleanup_(Module m)
{
    return setfeatureenables(m, &module_features, NULL);
}

/**/
int
finish_(UNUSED(Module m))
{
    return 0;
}
