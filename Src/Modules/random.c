/*
 * random.c - module to access kernel random sources.
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 2022 Clinton Bunch
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Clinton Bunch or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Clinton Bunch and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Clinton Bunch and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Clinton Bunch and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "random.mdh"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>

#include <stdint.h>

#ifdef HAVE_SYS_RANDOM_H
#include <sys/random.h>
#endif

/* Simplify select URANDOM specific code */
#if !defined(HAVE_ARC4RANDOM_BUF) && !defined(HAVE_GETRANDOM)
#define USE_URANDOM
#endif

/* buffer to pre-load integers for SRANDOM to lessen the context switches */
static uint32_t rand_buff[8];
static int buf_cnt = -1;

#ifdef USE_URANDOM
/* File descriptor for /dev/urandom */
int randfd = -1;
#endif /* USE_URANDOM */

static zlong get_srandom(UNUSED(Param p));

/**/
ssize_t
getrandom_buffer(void *buf, size_t len)
{
    ssize_t ret;
    size_t  val     = 0;
    uint8_t *bufptr = buf;

    do {
	errno = 0;
#ifdef HAVE_ARC4RANDOM_BUF
	arc4random_buf(buf,len);
	ret = len;
#elif defined(HAVE_GETRANDOM)
	ret=getrandom(bufptr,(len - val),0);
#else
	ret=read(randfd,bufptr,(len - val));
#endif
	if (ret < 0) {
	    if (errno != EINTR || errflag || retflag || breaks || contflag) {
		zwarn("Unable to get random data: %e.", errno);
		return -1;
	    }
	}
	bufptr += ret;
	val    += ret;
    } while (ret < len);
    return ret;
}

/*
 * Generate count number of random 32-bit integers between 0 and max-1
 * Got this algorithm from
 *https://lemire.me/blog/2016/06/30/fast-random-shuffling/
 * adapting the public domain code.
 *
 */

/**/
void
get_bound_random_buffer(uint32_t *buffer, size_t count, uint32_t max)
{
    uint64_t multi_result;
    uint32_t threshold;
    uint32_t leftover;

    size_t i; /* loop counter */

    getrandom_buffer((void*) buffer, count*sizeof(uint32_t));
    if (max == UINT32_MAX)
	return;

    for(i=0;i<count;i++) {
	multi_result = ((uint64_t) buffer[i]) * (uint64_t) max;
	leftover = (uint32_t) multi_result;

	/*
	 * The following if statement should (according to Google's Gemini)
	 * only be executed with a probability of 1/2**32 or 2.33e-10
	 */
	if(leftover < max) {
	    threshold= -max % max;
	    while (leftover < threshold) {
		uint32_t j=get_srandom(NULL);
		multi_result=(uint64_t) j * (uint64_t) max;
		leftover= (uint32_t) multi_result;
	    }
	}
	buffer[i]=multi_result >> 32;
    }
}

/*
 * Provides for the SRANDOM parameter and returns an unsigned 32-bit random
 * integer.
 */

/**/
static zlong
get_srandom(UNUSED(Param pm)) {

    if(buf_cnt <= 0) {
	getrandom_buffer((void*) rand_buff,sizeof(rand_buff));
	buf_cnt=sizeof(rand_buff)/sizeof(rand_buff[0]);
    }
    return rand_buff[--buf_cnt];
}

/*
 * Implements math function zrand_int takes 0 to 3 arguments an upper bound,
 * a lower bound and a flag as to whether the range is inclusive or not.  The
 * default is exclusive.  If neither upper or lower is specified this is no
 * different than SRANDOM.
 */

/**/
static mnumber
math_zrand_int(UNUSED(char *name), int argc, mnumber *argv, UNUSED(int id))
{
    mnumber ret;
    uint32_t i;
    zlong lower=0, upper=UINT32_MAX,incl=0, diff;

    ret.type = MN_INTEGER;

    switch (argc) {
	case 0: ret.u.l=get_srandom(NULL);
		return ret;
		break;
	case 3: incl = (argv[2].u.l != 0)?1:0;
	case 2: lower = argv[1].u.l;
	case 1: upper = argv[0].u.l;
	default: diff = upper-lower+incl;
    }

    if (lower < 0 || lower >= UINT32_MAX) {
	zwarn("Lower bound (%z) out of range: 0-4294967295",lower);
    } else if (upper < lower) {
	zwarn("Upper bound (%z) must be greater than Lower Bound (%z)",upper,lower);
    } else if (upper < 0 || upper >= UINT32_MAX) {
	zwarn("Upper bound (%z) out of range: 0-4294967295",upper);
    }

    if ( diff == 0 ) {
	ret.u.l=upper; /* still not convinced this shouldn't be an error. */
    } else {
	get_bound_random_buffer(&i,1,(uint32_t) diff);
	ret.u.l=i+lower;
    }
    return ret;
}

/*
 * Implements the mathfunc zrand_float and returns a random floating-point
 * number between 0 and 1.
 *
 */

/**/
static mnumber
math_zrand_float(UNUSED(char *name), UNUSED(int argc), UNUSED(mnumber *argv),
	     UNUSED(int id))
{
    mnumber ret;
    double r;

    r = random_real();
    if (r < 0) {
	zwarnnam(name, "Failed to get sufficient random data.");
    }
    ret.type = MN_FLOAT;
    ret.u.d = r;

    return ret;
}

static const struct gsu_integer srandom_gsu =
{ get_srandom, nullintsetfn, stdunsetfn };

static struct paramdef patab[] = {
    {"SRANDOM", PM_INTEGER | PM_READONLY_SPECIAL | PM_HIDEVAL, NULL,
	    &srandom_gsu, NULL, NULL, NULL},
};

static struct mathfunc mftab[] = {
    NUMMATHFUNC("zrand_float", math_zrand_float, 0, 0, 0),
    NUMMATHFUNC("zrand_int", math_zrand_int, 0, 3, 0),
};

static struct features module_features = {
    NULL, 0,
    NULL, 0,
    mftab, sizeof(mftab)/sizeof(*mftab),
    patab, sizeof(patab)/sizeof(*patab),
    0
};

/**/
int
setup_(UNUSED(Module m))
{
#ifdef USE_URANDOM
    /* Check for the existence of /dev/urandom */

    struct stat st;

    errno=0;
    if (stat("/dev/urandom",&st) < 0) {
	zwarn("Error getting kernel random pool: %e.", errno);
	return 1;
    }

    errno=0;
    if (!(S_ISCHR(st.st_mode)) ) {
	zwarn("Error getting kernel random pool: %e.", errno);
	return 1;
    }
#endif /* USE_URANDOM */
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
boot_(UNUSED(Module m))
{
#ifdef USE_URANDOM
    /*
     * Open /dev/urandom.  Here because of a weird issue on HP-UX 11.31
     * When opening in setup_ open returned 0.  In boot_, it returns
     * an unused file descriptor. Decided against ifdef HPUX as it works
     * here just as well for other platforms.
     *
     */

    int tmpfd=-1;

    errno=0;
    if ((tmpfd = open("/dev/urandom", O_RDONLY)) < 0) {
	zwarn("Could not access kernel random pool: %e.",errno);
	return 1;
    }
    randfd = movefd(tmpfd);
    addmodulefd(randfd,FDT_MODULE);
    if (randfd < 0) {
	zwarn("Could not access kernel random pool.");
	return 1;
    }
#endif /* USE_URANDOM */
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
#ifdef USE_URANDOM
    if (randfd >= 0)
	zclose(randfd);
#endif /* USE_URANDOM */
    return 0;
}
