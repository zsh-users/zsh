/*  This file contains code under different copyrights separated by */
/* ====@@@@@=== */

/*
 * random_real.c - module to access kernel random sources.
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

#include <math.h>
#include <stdint.h>
#include <errno.h>


/* Count the number of leading zeros, hopefully in gcc/clang by HW
 * instruction */
#if defined(__GNUC__) || defined(__clang__)
#define clz64(x) __builtin_clzll(x)
#else
#define clz64(x) _zclz64(x)

/**/
int
_zclz64(uint64_t x) {
    int n = 0;

    if (x == 0)
	return 64;

    if (!(x & 0xFFFFFFFF00000000ull)) {
	n+=32;
	x<<=32;
    }
    if (!(x & 0xFFFF000000000000ull)) {
	n+=16;
	x<<=16;
    }
    if (!(x & 0xFF00000000000000ull)) {
	n+=8;
	x<<=8;
    }
    if (!(x & 0xF000000000000000ull)) {
	n+=4;
	x<<=4;
    }
    if (!(x & 0xC000000000000000ull)) {
	n+=2;
	x<<=1;
    }
    if (!(x & 0x8000000000000000ull)) {
	n+=1;
    }
    return n;
}
#endif /* __GNU_C__ or __clang__ */

/**/
uint64_t
random_64bit(void) {
    uint64_t r;

    if(getrandom_buffer(&r,sizeof(r)) < 0) {
	zwarn("zsh/random: Can't get sufficient random data.");
	return 1; /* 0 will cause loop */
    }

    return r;
}

/* ====@@@@@=== */
/* Following code is under the below copyright, despite changes for error
 * handling and removing GCCisms */

/*-
 * Copyright (c) 2014 Taylor R. Campbell
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 * Uniform random floats: How to generate a double-precision
 * floating-point numbers in [0, 1] uniformly at random given a uniform
 * random source of bits.
 *
 * See<http://mumble.net/~campbell/2014/04/28/uniform-random-float>
 * for explanation.
 *
 * Updated 2015-02-22 to replace ldexp(x, <constant>) by x * ldexp(1,
 * <constant>), since glibc and NetBSD libm both have slow software
 * bit-twiddling implementations of ldexp, but GCC can constant-fold
 * the latter.
 */

/*
 * random_real: Generate a stream of bits uniformly at random and
 * interpret it as the fractional part of the binary expansion of a
 * number in [0, 1], 0.00001010011111010100...; then round it.
 */

/**/
double
random_real(void)
{
	int exponent = 0;
	uint64_t significand = 0;
	uint64_t r = 0;
	unsigned shift;

	/*
	 * Read zeros into the exponent until we hit a one; the rest
	 * will go into the significand.
	 */
	while (significand == 0) {
		exponent -= 64;

		/* Get random_64bit and check for error */
		errno = 0;
		significand = random_64bit();
		if (errno)
		    return -1;
		/*
		 * If the exponent falls below -1074 = emin + 1 - p,
		 * the exponent of the smallest subnormal, we are
		 * guaranteed the result will be rounded to zero.  This
		 * case is so unlikely it will happen in realistic
		 * terms only if random_64bit is broken.
		 */
		if (exponent < -1074)
			return 0;
	}

	/*
	 * There is a 1 somewhere in significand, not necessarily in
	 * the most significant position.  If there are leading zeros,
	 * shift them into the exponent and refill the less-significant
	 * bits of the significand.  Can't predict one way or another
	 * whether there are leading zeros: there's a fifty-fifty
	 * chance, if random_64bit is uniformly distributed.
	 */
	shift = clz64(significand);
	if (shift != 0) {

		errno = 0;
		r = random_64bit();
		if (errno)
		    return -1;

		exponent -= shift;
		significand <<= shift;
		significand |= (r >> (64 - shift));
	}

	/*
	 * Set the sticky bit, since there is almost surely another 1
	 * in the bit stream.  Otherwise, we might round what looks
	 * like a tie to even when, almost surely, were we to look
	 * further in the bit stream, there would be a 1 breaking the
	 * tie.
	 */
	significand |= 1;

	/*
	 * Finally, convert to double (rounding) and scale by
	 * 2^exponent.
	 */
	return ldexp((double)significand, exponent);
}
/* ====@@@@@=== */
