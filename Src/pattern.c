/*
 * pattern.c - pattern matching
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 1999 Peter Stephenson
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Peter Stephenson or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Peter Stephenson and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Peter Stephenson and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Peter Stephenson and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 * Pattern matching code derived from the regexp library by Henry
 * Spencer, which has the following copyright.
 *
 *	Copyright (c) 1986 by University of Toronto.
 *	Written by Henry Spencer.  Not derived from licensed software.
 *
 *	Permission is granted to anyone to use this software for any
 *	purpose on any computer system, and to redistribute it freely,
 *	subject to the following restrictions:
 *
 *	1. The author is not responsible for the consequences of use of
 *		this software, no matter how awful, even if they arise
 *		from defects in it.
 *
 *	2. The origin of this software must not be misrepresented, either
 *		by explicit claim or by omission.
 *
 *	3. Altered versions must be plainly marked as such, and must not
 *		be misrepresented as being the original software.
 *
 * Eagle-eyed readers will notice this is an altered version.  Incredibly
 * sharp-eyed readers might even find bits that weren't altered.
 */

#include "zsh.mdh"

/*
 * The following union is used mostly for alignment purposes.
 * Normal nodes are longs, while certain nodes take a char * as an argument;
 * here we make sure that they both work out to the same length.
 * The compiled regexp we construct consists of upats stuck together;
 * anything else to be added (strings, numbers) is stuck after and
 * then aligned to a whole number of upat units.
 *
 * Note also that offsets are in terms of the sizes of these things.
 */
union upat {
    long l;
    unsigned char *p;
};

typedef union upat *Upat;

#include "pattern.pro"

/* Number of active parenthesised expressions allowed in backreferencing */
#define NSUBEXP  9

/* definition	number	opnd?	meaning */
#define	P_END	  0x00	/* no	End of program. */
#define P_EXCSYNC 0x01	/* no   Test if following exclude already failed */
#define P_EXCEND  0x02	/* no   Test if exclude matched orig branch */
#define	P_BACK	  0x03	/* no	Match "", "next" ptr points backward. */
#define	P_EXACTLY 0x04	/* str	Match this string. */
#define	P_NOTHING 0x05	/* no	Match empty string. */
#define	P_ONEHASH 0x06	/* node	Match this (simple) thing 0 or more times. */
#define	P_TWOHASH 0x07	/* node	Match this (simple) thing 1 or more times. */
#define P_GFLAGS  0x08	/* long Match nothing and set globbing flags */
#define P_ISSTART 0x09  /* no   Match start of string. */
#define P_ISEND   0x0a  /* no   Match end of string. */
/* numbered so we can test bit 5 for a branch */
#define	P_BRANCH  0x20	/* node	Match this alternative, or the next... */
#define	P_WBRANCH 0x21	/* uc* node P_BRANCH, but match at least 1 char */
/* excludes are also branches, but have bit 4 set, too */
#define P_EXCLUDE 0x30	/* uc* node Exclude this from previous branch */
#define P_EXCLUDP 0x31	/* uc* node Exclude, using full file path so far */
/* numbered so we can test bit 6 so as not to match initial '.' */
#define	P_ANY	  0x40	/* no	Match any one character. */
#define	P_ANYOF	  0x41	/* str  Match any character in this string. */
#define	P_ANYBUT  0x42	/* str  Match any character not in this string. */
#define P_STAR    0x43	/* no   Match any set of characters. */
#define P_NUMRNG  0x44	/* zr, zr Match a numeric range. */
#define P_NUMFROM 0x45	/* zr   Match a number >= X */
#define P_NUMTO   0x46	/* zr   Match a number <= X */
#define P_NUMANY  0x47	/* no   Match any set of decimal digits */
/* spaces left for P_OPEN+n,... for backreferences */
#define	P_OPEN	  0x80	/* no	Mark this point in input as start of n. */
#define	P_CLOSE	  0x90	/* no	Analogous to OPEN. */
/* zl    is the range type zrange_t:  may be zlong or unsigned long
 * char is a single char
 * uc*   is a pointer to unsigned char, used at run time and initialised
 *       to NULL.
 */

/*
 * Notes on usage:
 * P_WBRANCH:  This works like a branch and is used in complex closures,
 *    to ensure we don't succeed on a zero-length match of the pattern,
 *    since that would cause an infinite loop.  We do this by recording
 *    the positions where we have already tried to match.   See the
 *    P_WBRANCH test in patmatch().
 *
 *  P_ANY, P_ANYOF:  the operand is a null terminated
 *    string.  Normal characters match as expected.  Characters
 *    in the range Meta+PP_ALPHA..Meta+PP_UNKNWN do the approprate
 *    Posix range tests.  This relies on imeta returning true for these
 *    characters.  We treat unknown POSIX ranges as never matching.
 *    PP_RANGE means the next two (possibly metafied) characters form
 *    the limits of a range to test; it's too much like hard work to
 *    expand the range.
 *
 *  P_EXCLUDE, P_EXCSYNC, PEXCEND:  P_EXCLUDE appears in the pattern like
 *    P_BRANCH, but applies to the immediately preceding branch.  The code in
 *    the corresponding branch is followed by a P_EXCSYNC, which simply
 *    acts as a marker that a P_EXCLUDE comes next.  The P_EXCLUDE
 *    has a pointer to char embeded in it, which works
 *    like P_WBRANCH:  if we get to the P_EXCSYNC, and we already matched
 *    up to the same position, fail.  Thus we are forced to backtrack
 *    on closures in the P_BRANCH if the first attempt was excluded.
 *    Corresponding to P_EXCSYNC in the original branch, there is a
 *    P_EXCEND in the exclusion.  If we get to this point, and we did
 *    *not* match in the original branch, the exclusion itself fails,
 *    otherwise it succeeds since we know the tail already matches,
 *    so P_EXCEND is the end of the exclusion test.
 *    The whole sorry mess looks like this, where the upper lines
 *    show the linkage of the branches, and the lower shows the linkage
 *    of their pattern arguments.
 *
 *     	        ---------------------      ----------------------
 *              ^      	       	     v    ^      	         v
 *      ( <BRANCH>:apat-><EXCSYNC> <EXCLUDE>:excpat-><EXCEND> ) tail
 *                               	                         ^
 *		       	  |                                      |
 *			   --------------------------------------
 *
 * P_EXCLUDP: this behaves exactly like P_EXCLUDE, with the sole exception
 *   that we prepend the path so far to the exclude pattern.   This is
 *   for top level file globs, e.g. ** / *.c~*foo.c
 *                                    ^ I had to leave this space
 * P_NUM*: zl is a zlong if that is 64-bit, else an unsigned long.
 */
#define PP_ALPHA  1
#define PP_ALNUM  2
#define PP_BLANK  3
#define PP_CNTRL  4
#define PP_DIGIT  5
#define PP_GRAPH  6
#define PP_LOWER  7
#define PP_PRINT  8
#define PP_PUNCT  9
#define PP_SPACE  10
#define PP_UPPER  11
#define PP_XDIGIT 12
#define PP_UNKWN  13
#define PP_RANGE  14

#define	P_OP(p)		((p)->l & 0xff)
#define	P_NEXT(p)	((p)->l >> 8)
#define	P_OPERAND(p)	((p) + 1)
#define P_ISBRANCH(p)   ((p)->l & 0x20)
#define P_ISEXCLUDE(p)	(((p)->l & 0x30) == 0x30)
#define P_NOTDOT(p)	((p)->l & 0x40)

#define P_SIMPLE        0x01	/* Simple enough to be #/## operand. */
#define P_HSTART        0x02	/* Starts with # or ##'d pattern. */
#define P_PURESTR	0x04	/* Can be matched with a strcmp */

/* Next character after one which may be a Meta (x is any char *) */
#define METANEXT(x)	(*(x) == Meta ? (x)+2 : (x)+1)
/*
 * Increment pointer which may be on a Meta (x is a pointer variable),
 * returning the incremented value (i.e. like pre-increment).
 */
#define METAINC(x)	((x) += (*(x) == Meta) ? 2 : 1)
/*
 * Return unmetafied char from string (x is any char *)
 */
#define UNMETA(x)	(*(x) == Meta ? (x)[1] ^ 32 : *(x))

#if defined(ZSH_64_BIT_TYPE) || defined(LONG_IS_64_BIT)
typedef zlong zrange_t;
#else
typedef unsigned long zrange_t;
#endif

/*
 * Characters which terminate a pattern segment.  We actually use
 * a pointer patendseg which skips the first character if we are not
 * parsing a file pattern.
 * Note that the size of this and the next array are hard-wired
 * via the definitions.
 */

static char endseg[] = {
    '/',			/* file only */
    '\0', Bar, Outpar,		/* all patterns */
    Tilde			/* extended glob only */
};

#define PATENDSEGLEN_NORM 4
#define PATENDSEGLEN_EXT  5

/* Characters which terminate a simple string */

static char endstr[] = {
    '/',			/* file only */
    '\0', Bar, Outpar, Quest, Star, Inbrack, Inpar, Inang,
				/* all patterns */
    Tilde, Hat, Pound		/* extended glob only */
};

#define PATENDSTRLEN_NORM 9
#define PATENDSTRLEN_EXT  12


/* Default size for pattern buffer */
#define P_DEF_ALLOC 256

/* Flags used in compilation */
static char *patstart, *patparse;	/* input pointers */
static int patnpar;		/* () count */
static char *patcode;		/* point of code emission */
static long patsize;		/* size of code */
static char *patout;		/* start of code emission string */
static long patalloc;		/* size allocated for same */
static char *patendseg;		/* characters ending segment */
static int patendseglen;	/* length of same */
static char *patendstr;		/* characters ending plain string */
static int patendstrlen;	/* length of sameo */

/* Flags used in both compilation and execution */
static int patflags;		    /* flags passed down to patcompile */
static int patglobflags;  /* globbing flags & approx */

/* Add n more characters, ensuring there is enough space. */

/**/
static void
patadd(char *add, int ch, long n, int noalgn)
{
    /* Make sure everything gets aligned unless we get noalgn. */
    long newpatsize = patsize + n;
    if (!noalgn)
	newpatsize = (newpatsize + sizeof(union upat) - 1) &
		      ~(sizeof(union upat) - 1);
    if (patalloc < newpatsize) {
	long newpatalloc =
	    2*(newpatsize > patalloc ? newpatsize : patalloc);
	patout = (char *)zrealloc((char *)patout, newpatalloc);
	patcode = patout + patsize;
	patalloc = newpatalloc;
    }
    patsize = newpatsize;
    if (add) {
	while (n--)
	    *patcode++ = *add++;
    } else
	*patcode++ = ch;
    patcode = patout + patsize;
}

static long rn_offs;
/* operates on pointers to union upat, returns a pointer */
#define PATNEXT(p) ((rn_offs = P_NEXT(p)) ? \
		    (P_OP(p) == P_BACK) ? \
		    ((p)-rn_offs) : ((p)+rn_offs) : NULL)

/* Called before parsing a set of file matchs to initialize flags */

/**/
void
patcompstart(void)
{
    patglobflags = 0;
}

/* Top level pattern compilation subroutine */

/**/
mod_export Patprog
patcompile(char *exp, int inflags, char **endexp)
{
    int flags = 0, len = 0;
    long startoff;
    Upat pscan;
    char *lng, *strp = NULL;
    Patprog p;

    startoff = sizeof(struct patprog);
    /* Ensure alignment of start of program string */
    startoff = (startoff + sizeof(union upat) - 1) & ~(sizeof(union upat) - 1);

    /* Allocate reasonable sized chunk if none, reduce size if too big */
    if (patalloc != P_DEF_ALLOC)
	patout = (char *)zrealloc(patout, patalloc = P_DEF_ALLOC);
    patcode = patout + startoff;
    patsize = patcode - patout;
    patstart = patparse = exp;
    /*
     * Note global patnpar numbers parentheses 1..9, while patnpar
     * in struct is actual count of parentheses.
     */
    patnpar = 1;
    patflags = inflags & ~PAT_PURES;

    patendseg = endseg;
    patendseglen = isset(EXTENDEDGLOB) ? PATENDSEGLEN_EXT : PATENDSEGLEN_NORM;
    patendstr = endstr;
    patendstrlen = isset(EXTENDEDGLOB) ? PATENDSTRLEN_EXT : PATENDSTRLEN_NORM;

    if (!(patflags & PAT_FILE)) {
	patendseg++;
	patendstr++;
	patendseglen--;
	patendstrlen--;
	remnulargs(exp);
	patglobflags = 0;
    }
    /*
     * Have to be set now, since they get updated during compilation.
     */
    ((Patprog)patout)->globflags = patglobflags;

    if (!(patflags & PAT_ANY)) {
	/* Look for a really pure string, with no tokens at all. */
	if (!patglobflags)
	    for (strp = exp; *strp &&
		     (!(patflags & PAT_FILE) || *strp != '/') && !itok(*strp);
		 strp++)
		;
	if (!strp || (*strp && *strp != '/')) {
	    /* No, do normal compilation. */
	    strp = NULL;
	    if (patcompswitch(0, &flags) == 0)
		return NULL;
	} else {
	    /* Yes, copy the string and skip compilation altogether */
	    patparse = strp;
	    len = strp - exp;
	    patadd(exp, 0, len + 1, 0);
	    patout[startoff + len] = '\0';
	    patflags |= PAT_PURES;
	}
    }

    /* end of compilation: safe to use pointers */
    p = (Patprog)patout;
    p->startoff = startoff;
    p->patstartch = '\0';
    p->globend = patglobflags;
    p->flags = patflags;
    p->mustoff = 0;
    p->size = patsize;
    p->patmlen = len;
    p->patnpar = patnpar-1;

    if (!strp) {
	pscan = (Upat)(patout + startoff);

	if (!(patflags & PAT_ANY) && P_OP(PATNEXT(pscan)) == P_END) {
	    /* only one top level choice */
	    pscan = P_OPERAND(pscan);

	    if (flags & P_PURESTR) {
		/*
		 * The pattern can be matched with a simple strncmp/strcmp.
		 * Careful in case we've overwritten the node for the next ptr.
		 */
		char *dst = patout + startoff;
		Upat next;
		p->flags |= PAT_PURES;
		for (; pscan; pscan = next) {
		    next = PATNEXT(pscan);
		    if (P_OP(pscan) == P_EXACTLY) {
			char *opnd = (char *)P_OPERAND(pscan);
			while ((*dst = *opnd++))
			    dst++;
		    }
		}
		*dst++ = '\0';
		p->size = dst - patout;
		/* patmlen is really strlen, don't include null byte */
		p->patmlen = p->size - startoff - 1;
	    } else {
		/* starting point info */
		if (P_OP(pscan) == P_EXACTLY && !p->globflags)
		    p->patstartch = *(char *)P_OPERAND(pscan);
		/*
		 * Find the longest literal string in something expensive.
		 * This is itself not all that cheap if we have
		 * case-insensitive matching or approximation, so don't.
		 */
		if ((flags & P_HSTART) && !p->globflags) {
		    lng = NULL;
		    len = 0;
		    for (; pscan; pscan = PATNEXT(pscan))
			if (P_OP(pscan) == P_EXACTLY &&
			    strlen((char *)P_OPERAND(pscan)) >= len) {
			    lng = (char *)P_OPERAND(pscan);
			    len = strlen(lng);
			}
		    if (lng) {
			p->mustoff = lng - patout;
			p->patmlen = len;
		    }
		}
	    }
	}
    }

    /*
     * The pattern was compiled in a fixed buffer:  unless told otherwise,
     * we stick the compiled pattern on the heap.  This is necessary
     * for files where we will often be compiling multiple segments at once.
     * But if we get the ZDUP flag w always put it in zalloc()ed memory.
     */
    if (patflags & PAT_ZDUP) {
	Patprog newp = (Patprog)zalloc(patsize);
	memcpy((char *)newp, (char *)p, patsize);
	p = newp;
    } else if (!(patflags & PAT_STATIC)) {
	Patprog newp = (Patprog)zhalloc(patsize);
	memcpy((char *)newp, (char *)p, patsize);
	p = newp;
    }

    if (endexp)
	*endexp = patparse;
    return p;
}

/*
 * Main body or parenthesised subexpression in pattern
 * Parenthesis (and any ksh_glob gubbins) will have been removed.
 */

/**/
static long
patcompswitch(int paren, int *flagp)
{
    long starter, br, ender, excsync = 0;
    int parno = 0;
    int flags, gfchanged = 0, savglobflags = patglobflags;
    Upat ptr;

    *flagp = 0;

    if (paren && (patglobflags & GF_BACKREF) && patnpar <= NSUBEXP) {
	/*
	 * parenthesized:  make an open node.
	 * We can only refer to the first nine parentheses.
	 * For any others, we just use P_OPEN on its own; there's
	 * no gain in arbitrarily limiting the number of parentheses.
	 */
	parno = patnpar++;
	starter = patnode(P_OPEN + parno);
    } else
	starter = 0;

    br = patnode(P_BRANCH);
    if (!patcompbranch(&flags))
	return 0;
    if (patglobflags != savglobflags)
	gfchanged++;
    if (starter)
	pattail(starter, br);
    else
	starter = br;

    *flagp |= flags & (P_HSTART|P_PURESTR);

    while (*patparse == Bar ||
	   (isset(EXTENDEDGLOB) && *patparse == Tilde &&
	    (patparse[1] == '/' ||
	     !memchr(patendseg, patparse[1], patendseglen)))) {
	int tilde = *patparse++ == Tilde;
	long gfnode = 0, newbr;

	*flagp &= ~P_PURESTR;

	if (tilde) {
	    union upat up;
	    /* excsync remembers the P_EXCSYNC node before a chain of
	     * exclusions:  all point back to this.  only the
	     * original (non-excluded) branch gets a trailing P_EXCSYNC.
	     */
	    if (!excsync) {
		excsync = patnode(P_EXCSYNC);
		patoptail(br, excsync);
	    }
	    /*
	     * By default, approximations are turned off in exclusions:
	     * we need to do this here as otherwise the code compiling
	     * the exclusion doesn't know if the flags have really
	     * changed if the error count gets restored.
	     *
	     * At top level (paren == 0) in a file glob !(patflags &PAT_FILET)
	     * do the exclusion prepending the file path so far, else don't.
	     */
	    patglobflags &= ~0xff;
	    br = patnode(!(patflags & PAT_FILET) || paren ?
			 P_EXCLUDE : P_EXCLUDP);
	    up.p = NULL;
	    patadd((char *)&up, 0, sizeof(up), 0);
	    /* / is not treated as special if we are at top level */
	    if (!paren && *patendseg == '/') {
		tilde++;
		patendseg++;
		patendseglen--;
		patendstr++;
		patendstrlen--;
	    }
	} else {
	    excsync = 0;
	    br = patnode(P_BRANCH);
	    /*
	     * The position of the following statements means globflags
	     * set in the main branch carry over to the exclusion.
	     */
	    if (!paren) {
		patglobflags = 0;
		if (((Patprog)patout)->globflags) {
		    /*
		     * If at top level, we need to reinitialize flags to zero,
		     * since (#i)foo|bar only applies to foo and we stuck
		     * the #i into the global flags.
		     * We could have done it so that they only got set in the
		     * first branch, but it's quite convenient having any
		     * global flags set in the header and not buried in the
		     * pattern.  (Or maybe it isn't and we should
		     * forget this bit and always stick in an explicit GFLAGS
		     * statement instead of using the header.)
		     * Also, this can't happen for file globs where there are
		     * no top-level |'s.
		     *
		     * No gfchanged, as nothing to follow branch at top
		     * level. 
		     */
		    union upat up;
		    gfnode = patnode(P_GFLAGS);
		    up.l = patglobflags;
		    patadd((char *)&up, 0, sizeof(union upat), 0);
		}
	    } else {
		patglobflags = savglobflags;
	    }
	}
	newbr = patcompbranch(&flags);
	if (tilde == 2) {
	    /* restore special treatment of / */
	    patendseg--;
	    patendseglen++;
	    patendstr--;
	    patendstrlen++;
	}
	if (!newbr)
	    return 0;
	if (gfnode)
	    pattail(gfnode, newbr);
	if (!tilde && patglobflags != savglobflags)
	    gfchanged++;
	pattail(starter, br);
	if (excsync)
	    patoptail(br, patnode(P_EXCEND));
	*flagp |= flags & P_HSTART;
    }

    /*
     * Make a closing node, hooking it to the end.
     * Note that we can't optimize P_NOTHING out here, since another
     * branch at that point would indicate the current choices continue,
     * which they don't.
     */
    ender = patnode(paren ? parno ? P_CLOSE+parno : P_NOTHING : P_END);
    pattail(starter, ender);

    /*
     * Hook the tails of the branches to the closing node,
     * except for exclusions which terminate where they are.
     */
    for (ptr = (Upat)patout + starter; ptr; ptr = PATNEXT(ptr))
	if (!P_ISEXCLUDE(ptr))
	    patoptail(ptr-(Upat)patout, ender);

    /* check for proper termination */
    if ((paren && *patparse++ != Outpar) ||
	(!paren && *patparse &&
	 !((patflags & PAT_FILE) && *patparse == '/')))
	return 0;

    if (paren && gfchanged) {
	/*
	 * Restore old values of flags when leaving parentheses.
	 * gfchanged detects a change in any branch (except exclusions
	 * which are separate), since we need to emit this even if
	 * a later branch happened to put the flags back.
	 */
	pattail(ender, patnode(P_GFLAGS));
	patglobflags = savglobflags;
	patadd((char *)&savglobflags, 0, sizeof(long), 0);
    }

    return starter;
}

/*
 * Compile something ended by Bar, Outpar, Tilde, or end of string.
 * Note the BRANCH or EXCLUDE tag must already have been omitted:
 * this returns the position of the operand of that.
 */

/**/
static long
patcompbranch(int *flagp)
{
    long chain, latest, starter;
    int flags = 0;

    *flagp = P_PURESTR;

    starter = chain = 0;
    while (!memchr(patendseg, *patparse, patendseglen) ||
	   (*patparse == Tilde && patparse[1] != '/' &&
	    memchr(patendseg, patparse[1], patendseglen))) {
	if (isset(EXTENDEDGLOB) &&
	    ((!isset(SHGLOB) &&
	      (*patparse == Inpar && patparse[1] == Pound)) ||
	     (isset(KSHGLOB) && *patparse == '@' && patparse[1] == Inpar &&
	      patparse[2] == Pound))) {
	    /* Globbing flags. */
	    char *pp1 = patparse;
	    int oldglobflags = patglobflags;
	    long assert;
	    patparse += (*patparse == '@') ? 3 : 2;
	    if (!patgetglobflags(&patparse, &assert))
		return 0;
	    if (assert) {
		/*
		 * Start/end assertion looking like flags, but
		 * actually handled as a normal node
		 */
		latest = patnode(assert);
		flags = 0;
	    } else {
		if (pp1 == patstart) {
		    /* Right at start of pattern, the simplest case.
		     * Put them into the flags and don't emit anything.
		     */
		    ((Patprog)patout)->globflags = patglobflags;
		    continue;
		} else if (!*patparse) {
		    /* Right at the end, so just leave the flags for
		     * the next Patprog in the chain to pick up.
		     */
		    break;
		}
		/*
		 * Otherwise, we have to stick them in as a pattern
		 * matching nothing.
		 */
		if (oldglobflags != patglobflags) {
		    /* Flags changed */
		    union upat up;
		    latest = patnode(P_GFLAGS);
		    up.l = patglobflags;
		    patadd((char *)&up, 0, sizeof(union upat), 0);
		} else {
		    /* No effect. */
		    continue;
		}
	    }
	} else if (isset(EXTENDEDGLOB) && *patparse == Hat) {
	    /*
	     * ^pat:  anything but pat.  For proper backtracking,
	     * etc., we turn this into (*~pat), except without the
	     * parentheses.
	     */
	    patparse++;
	    latest = patcompnot(0, &flags);
	} else
	    latest = patcomppiece(&flags);
	if (!latest)
	    return 0;
	if (!starter)
	    starter = latest;
	if (!(flags & P_PURESTR))
	    *flagp &= ~P_PURESTR;
	if (!chain)
	    *flagp |= flags & P_HSTART;
	else
	    pattail(chain, latest);
	chain = latest;
    }
    /* check if there was nothing in the loop, i.e. () */
    if (!chain)
	starter = patnode(P_NOTHING);

    return starter;
}

/* get glob flags, return 1 for success, 0 for failure */

/**/
int
patgetglobflags(char **strp, long *assertp)
{
    char *nptr, *ptr = *strp;
    zlong ret;

    *assertp = 0;
    /* (#X): assumes we are still positioned on the first X */
    for (; *ptr && *ptr != Outpar; ptr++) {
	switch (*ptr) {
	case 'a':
	    /* Approximate matching, max no. of errors follows */
	    ret = zstrtol(++ptr, &nptr, 10);
	    /*
	     * We can't have more than 254, because we need 255 to
	     * mark 254 errors in wbranch and exclude sync strings
	     * (hypothetically --- hope no-one tries it).
	     */
	    if (ret < 0 || ret > 254 || ptr == nptr)
		return 0;
	    patglobflags = (patglobflags & ~0xff) | (ret & 0xff);
	    ptr = nptr-1;
	    break;

	case 'l':
	    /* Lowercase in pattern matches lower or upper in target */
	    patglobflags = (patglobflags & ~GF_IGNCASE) | GF_LCMATCHUC;
	    break;

	case 'i':
	    /* Fully case insensitive */
	    patglobflags = (patglobflags & ~GF_LCMATCHUC) | GF_IGNCASE;
	    break;

	case 'I':
	    /* Restore case sensitivity */
	    patglobflags &= ~(GF_LCMATCHUC|GF_IGNCASE);
	    break;

	case 'b':
	    /* Make backreferences */
	    patglobflags |= GF_BACKREF;
	    break;

	case 'B':
	    /* Don't make backreferences */
	    patglobflags &= ~GF_BACKREF;
	    break;

	case 'm':
	    /* Make references to complete match */
	    patglobflags |= GF_MATCHREF;
	    break;

	case 'M':
	    /* Don't */
	    patglobflags &= ~GF_MATCHREF;
	    break;

	case 's':
	    *assertp = P_ISSTART;
	    break;

	case 'e':
	    *assertp = P_ISEND;
	    break;

	default:
	    return 0;
	}
    }
    if (*ptr != Outpar)
	return 0;
    /* Start/end assertions must appear on their own. */
    if (*assertp && (*strp)[1] != Outpar)
	return 0;
    *strp = ptr + 1;
    return 1;
}

/*
 * compile a chunk such as a literal string or a [...] followed
 * by a possible hash operator
 */

/**/
static long
patcomppiece(int *flagp)
{
    long starter = 0, next, pound, op;
    int flags, flags2, kshchar, len, ch, patch;
    union upat up;
    char *nptr, *str0, cbuf[2];
    zrange_t from, to;

    flags = 0;
    str0 = patparse;
    for (;;) {
	/*
	 * Check if we have a string. First, we need to make sure
	 * the string doesn't introduce a ksh-like parenthesised expression.
	 */
	kshchar = '\0';
	if (isset(KSHGLOB) && *patparse && patparse[1] == Inpar) {
	    if (strchr("?*+!@", *patparse))
		kshchar = STOUC(*patparse);
	    else if (*patparse == Star || *patparse == Quest)
		kshchar = STOUC(ztokens[*patparse - Pound]);
	}

	/*
	 * End of string (or no string at all) if ksh-type parentheses,
	 * or special character, unless that character is a tilde and
	 * the character following is an end-of-segment character.  Thus
	 * tildes are not special if there is nothing following to
	 * be excluded.
	 */
	if (kshchar || (memchr(patendstr, *patparse, patendstrlen) &&
			(*patparse != Tilde ||
			 patparse[1] == '/' ||
			 !memchr(patendseg, patparse[1], patendseglen))))
	    break;

	METAINC(patparse);
    }

    if (patparse > str0) {
	/* Ordinary string: cancel kshchar lookahead */
	kshchar = '\0';
	/*
	 * Assume it matches a simple string until we find otherwise.
	 */
	flags |= P_PURESTR;
	DPUTS(patparse == str0, "BUG: matched nothing in patcomppiece.");
	/* more than one character matched? */
	len = str0 + (*str0 == Meta ? 2 : 1) < patparse;
	/*
	 * If we have more than one character, a following hash only
	 * applies to the last, so decrement.
	 */
	if (isset(EXTENDEDGLOB) && *patparse == Pound && len)
	    patparse -= (patparse > str0 + 1 && patparse[-2] == Meta) ? 2 : 1;
	/*
	 * If len is 1, we can't have an active # following, so doesn't
	 * matter that we don't make X in `XX#' simple.
	 */
	if (!len)
	    flags |= P_SIMPLE;
	starter = patnode(P_EXACTLY);
	/* add enough space including null byte */
	len = patparse - str0;
	patadd(str0, 0, len + 1, 0);
	nptr = (char *)P_OPERAND((Upat)patout + starter);
	nptr[len] = '\0';
	/*
	 * It's much simpler to turn off pure string mode for
	 * any case-insensitive or approximate matching; usually,
	 * that is correct, or they wouldn't have been turned on.
	 * However, we need to make sure we match a "." or ".."
	 * in a file name as a pure string.  There's a minor bug
	 * that this will also apply to something like
	 * ..(#a1).. (i.e. the (#a1) has no effect), but if you're
	 * going to write funny patterns, you get no sympathy from me.
	 */
	if (patglobflags &&
	    (!(patflags & PAT_FILE) || (strcmp(nptr, ".") &&
					strcmp(nptr, ".."))))
	    flags &= ~P_PURESTR;
	for (; *nptr; METAINC(nptr))
	    if (itok(*nptr))
		*nptr = ztokens[*nptr - Pound];
    } else {
	if (kshchar)
	    patparse++;

	patch = *patparse;
	METAINC(patparse);
	switch(patch) {
	case Quest:
	    flags |= P_SIMPLE;
	    starter = patnode(P_ANY);
	    break;
	case Star:
	    /* kshchar is used as a sign that we can't have #'s. */
	    kshchar = -1;
	    starter = patnode(P_STAR);
	    break;
	case Inbrack:
	    flags |= P_SIMPLE;
	    if (*patparse == Hat || *patparse == '^' || *patparse == '!') {
		patparse++;
		starter = patnode(P_ANYBUT);
	    } else
		starter = patnode(P_ANYOF);
	    if (*patparse == Outbrack) {
		patparse++;
		patadd(NULL, ']', 1, 1);
	    }
	    while (*patparse && *patparse != Outbrack) {
		/* Meta is not a token */
		if (*patparse == Inbrack && patparse[1] == ':' &&
			(nptr = strchr(patparse+2, ':')) &&
			nptr[1] == Outbrack) {
			/* Posix range. */
			patparse += 2;
			len = nptr - patparse;
			if (!strncmp(patparse, "alpha", len))
			    ch = PP_ALPHA;
			else if (!strncmp(patparse, "alnum", len))
			    ch = PP_ALNUM;
			else if (!strncmp(patparse, "blank", len))
			    ch = PP_BLANK;
			else if (!strncmp(patparse, "cntrl", len))
			    ch = PP_CNTRL;
			else if (!strncmp(patparse, "digit", len))
			    ch = PP_DIGIT;
			else if (!strncmp(patparse, "graph", len))
			    ch = PP_GRAPH;
			else if (!strncmp(patparse, "lower", len))
			    ch = PP_LOWER;
			else if (!strncmp(patparse, "print", len))
			    ch = PP_PRINT;
			else if (!strncmp(patparse, "punct", len))
			    ch = PP_PUNCT;
			else if (!strncmp(patparse, "space", len))
			    ch = PP_SPACE;
			else if (!strncmp(patparse, "upper", len))
			    ch = PP_UPPER;
			else if (!strncmp(patparse, "xdigit", len))
			    ch = PP_XDIGIT;
			else
			    ch = PP_UNKWN;
			patparse = nptr + 2;
			if (ch != PP_UNKWN)
			    patadd(NULL, STOUC(Meta+ch), 1, 1);
			continue;
		}
		if (itok(*patparse)) {
		    cbuf[0] = ztokens[*patparse - Pound];
		} else if (*patparse == Meta) {
		    cbuf[0] = Meta;
		    cbuf[1] = *++patparse;
		} else
		    cbuf[0] = *patparse;
		patparse++;

		if (*patparse == '-' && patparse[1] != Outbrack) {
		    patadd(NULL, STOUC(Meta+PP_RANGE), 1, 1);
		    patadd(cbuf, 0, (cbuf[0] == Meta) ? 2 : 1, 1);
		    if (itok(*++patparse)) {
			patadd(0, STOUC(ztokens[*patparse - Pound]), 1, 1);
		    } else
			patadd(patparse, 0, (*patparse == Meta) ? 2 : 1, 1);
		    METAINC(patparse);
		} else
		    patadd(cbuf, 0, (cbuf[0] == Meta) ? 2 : 1, 1);
	    }
	    if (*patparse != Outbrack)
		return 0;
	    patparse++;
	    /* terminate null string and fix alignment */
	    patadd(NULL, 0, 1, 0);
	    break;
	case Inpar:
	    /* is this how to treat parentheses in SHGLOB? */
	    if (isset(SHGLOB) && !kshchar)
		return 0;
	    if (kshchar == '!') {
		/* This is nasty, we should really either handle all
		 * kshglobbing below or here.  But most of the
		 * others look like non-ksh patterns, while this one
		 * doesn't, so we handle it here and leave the rest.
		 * We treat it like an extendedglob ^, except that
		 * it goes into parentheses.
		 *
		 * If we did do kshglob here, we could support
		 * the old behaviour that things like !(foo)##
		 * work, but it makes the code more complicated at
		 * the expense of allowing the user to do things
		 * they shouldn't.
		 */
		if (!(starter = patcompnot(1, &flags2)))
		    return 0;
	    } else if (!(starter = patcompswitch(1, &flags2)))
		return 0;
	    flags |= flags2 & P_HSTART;
	    break;
	case Inang:
	    /* Numeric glob */
	    len = 0;		/* beginning present 1, end present 2 */
	    if (idigit(*patparse)) {
		from = (zrange_t) zstrtol((char *)patparse,
					 (char **)&nptr, 10);
		patparse = nptr;
		len |= 1;
	    }
	    DPUTS(*patparse != '-', "BUG: - missing from numeric glob");
	    patparse++;
	    if (idigit(*patparse)) {
		to = (zrange_t) zstrtol((char *)patparse,
					  (char **)&nptr, 10);
		patparse = nptr;
		len |= 2;
	    }
	    if (*patparse != Outang)
		return 0;
	    patparse++;
	    switch(len) {
	    case 3:
		starter = patnode(P_NUMRNG);
		patadd((char *)&from, 0, sizeof(from), 0);
		patadd((char *)&to, 0, sizeof(to), 0);
		break;
	    case 2:
		starter = patnode(P_NUMTO);
		patadd((char *)&to, 0, sizeof(to), 0);
		break;
	    case 1:
		starter = patnode(P_NUMFROM);
		patadd((char *)&from, 0, sizeof(from), 0);
		break;
	    case 0:
		starter = patnode(P_NUMANY);
		break;
	    }
	    /* This can't be simple, because it isn't.
	     * Mention in manual that matching digits with [...]
	     * is more efficient.
	     */
	    break;
	case Pound:
	    DPUTS(!isset(EXTENDEDGLOB), "BUG: # not treated as string");
	    /*
	     * A hash here is an error; it should follow something
	     * repeatable.
	     */
	    return 0;
	    break;
#ifdef DEBUG
	default:
	    dputs("BUG: character not handled in patcomppiece");
	    return 0;
	    break;
#endif
	}
    }

    if (!(pound = (*patparse == Pound && isset(EXTENDEDGLOB))) &&
	(kshchar <= 0 || kshchar == '@' || kshchar == '!')) {
	*flagp = flags;
	return starter;
    }

    /* too much at once doesn't currently work */
    if (kshchar && pound)
	return 0;

    if (kshchar == '*') {
	op = P_ONEHASH;
	*flagp = P_HSTART;
    } else if (kshchar == '+') {
	op = P_TWOHASH;
	*flagp = P_HSTART;
    } else if (kshchar == '?') {
	op = 0;
	*flagp = 0;
    } else if (*++patparse == Pound) {
	op = P_TWOHASH;
	patparse++;
	*flagp = P_HSTART;
    } else {
	op = P_ONEHASH;
	*flagp = P_HSTART;
    }

    /*
     * Note optimizations with pointers into P_NOTHING branches:  some
     * should logically point to next node after current piece.
     *
     * Backtracking is also encoded in a slightly obscure way:  the
     * code emitted ensures we test the non-empty branch of complex
     * patterns before the empty branch on each repetition.  Hence
     * each time we fail on a non-empty branch, we try the empty branch,
     * which is equivalent to backtracking.
     */
    if ((flags & P_SIMPLE) && (op == P_ONEHASH || op == P_TWOHASH) &&
	P_OP((Upat)patout+starter) == P_ANY) {
	/* Optimize ?# to *.  Silly thing to do, since who would use
	 * use ?# ? But it makes the later code shorter.
	 */
	Upat uptr = (Upat)patout + starter;
	if (op == P_TWOHASH) {
	    /* ?## becomes ?* */
	    uptr->l = (uptr->l & ~0xff) | P_ANY;
	    pattail(starter, patnode(P_STAR));
	} else {
	    uptr->l = (uptr->l & ~0xff) | P_STAR;
	}
    } else if ((flags & P_SIMPLE) && op && !(patglobflags & 0xff)) {
	/* Don't simplify if we need to look for approximations. */
	patinsert(op, starter, NULL, 0);
    } else if (op == P_ONEHASH) {
	/* Emit x# as (x&|), where & means "self". */
	up.p = NULL;
	patinsert(P_WBRANCH, starter, (char *)&up, sizeof(up));
	                                      /* Either x */
	patoptail(starter, patnode(P_BACK));  /* and loop */
	patoptail(starter, starter);	      /* back */
	pattail(starter, patnode(P_BRANCH));  /* or */
	pattail(starter, patnode(P_NOTHING)); /* null. */
    } else if (op == P_TWOHASH) {
	/* Emit x## as x(&|) where & means "self". */
	next = patnode(P_WBRANCH);	      /* Either */
	up.p = NULL;
	patadd((char *)&up, 0, sizeof(up), 0);
	pattail(starter, next);
	pattail(patnode(P_BACK), starter);    /* loop back */
	pattail(next, patnode(P_BRANCH));     /* or */
	pattail(starter, patnode(P_NOTHING)); /* null. */
    } else if (kshchar == '?') {
	/* Emit ?(x) as (x|) */
	patinsert(P_BRANCH, starter, NULL, 0); /* Either x */
	pattail(starter, patnode(P_BRANCH));   /* or */
	next = patnode(P_NOTHING);	       /* null */
	pattail(starter, next);
	patoptail(starter, next);
    }
    if (*patparse == Pound)
	return 0;

    return starter;
}

/*
 * Turn a ^foo (paren = 0) or !(foo) (paren = 1) into *~foo with
 * parentheses if necessary.   As you see, that's really quite easy.
 */

/**/
static long
patcompnot(int paren, int *flagsp)
{
    union upat up;
    long excsync, br, excl, n, starter;
    int dummy;

    /* Here, we're matching a star at the start. */
    *flagsp = P_HSTART;

    starter = patnode(P_BRANCH);
    br = patnode(P_STAR);
    excsync = patnode(P_EXCSYNC);
    pattail(br, excsync);
    pattail(starter, excl = patnode(P_EXCLUDE));
    up.p = NULL;
    patadd((char *)&up, 0, sizeof(up), 0);
    if (!(br = (paren ? patcompswitch(1, &dummy) : patcompbranch(&dummy))))
	return 0;
    pattail(br, patnode(P_EXCEND));
    n = patnode(P_NOTHING); /* just so much easier */
    pattail(excsync, n);
    pattail(excl, n);

    return starter;
}

/* Emit a node */

/**/
static long
patnode(long op)
{
    long starter = (Upat)patcode - (Upat)patout;
    union upat up;

    up.l = op;
    patadd((char *)&up, 0, sizeof(union upat), 0);
    return starter;
}

/*
 * insert an operator in front of an already emitted operand:
 * we relocate the operand.  there had better be nothing else after.
 */

/**/
static void
patinsert(long op, int opnd, char *xtra, int sz)
{
    char *src, *dst, *opdst;
    union upat buf, *lptr;

    buf.l = 0;
    patadd((char *)&buf, 0, sizeof(buf), 0);
    if (sz)
	patadd(xtra, 0, sz, 0);
    src = patcode - sizeof(union upat) - sz;
    dst = patcode;
    opdst = patout + opnd * sizeof(union upat);
    while (src > opdst)
	*--dst = *--src;

    /* A cast can't be an lvalue */
    lptr = (Upat)opdst;
    lptr->l = op;
    opdst += sizeof(union upat);
    while (sz--)
	*opdst++ = *xtra++;
}

/* set the 'next' pointer at the end of a node chain */

/**/
static void
pattail(long p, long val)
{
    Upat scan, temp;
    long offset;

    scan = (Upat)patout + p;
    for (;;) {
	if (!(temp = PATNEXT(scan)))
	    break;
	scan = temp;
    }

    offset = (P_OP(scan) == P_BACK)
	? (scan - (Upat)patout) - val : val - (scan - (Upat)patout);

    scan->l |= offset << 8;
}

/* do pattail, but on operand of first argument; nop if operandless */

/**/
static void patoptail(long p, long val)
{
    Upat ptr = (Upat)patout + p;
    int op = P_OP(ptr);
    if (!p || !P_ISBRANCH(ptr))
	return;
    if (op == P_BRANCH)
	pattail(P_OPERAND(p), val);
    else
	pattail(P_OPERAND(p) + 1, val);
}


/*
 * Run a pattern.
 */
static char *patinstart;	/* Start of input string */

/**/
char *patinput;		/* String input pointer */

/* Length of input string, plus null byte, if needed */
static int patinlen;

/*
 * Offset of string at which we are trying to match.
 * This is added in to the positions recorded in patbeginp and patendp
 * when we are looking for substrings.  Currently this only happens
 * in the parameter substitution code.
 */
/**/
int patoffset;

static char *patbeginp[NSUBEXP];	/* Pointer to backref beginnings */
static char *patendp[NSUBEXP];		/* Pointer to backref ends */
static int parsfound;			/* parentheses (with backrefs) found */

static int globdots;			/* Glob initial dots? */

/*
 * The following need to be accessed in the globbing scanner for
 * a multi-component file path.  See horror story there.
 */
/**/
int errsfound;				/* Total error count so far */

/**/
int forceerrs;				/* Forced maximum error count */

/**/
void
pattrystart(void)
{
    forceerrs = -1;
    errsfound = 0;
}

/**/
mod_export int
pattry(Patprog prog, char *string)
{
    return pattryrefs(prog, string, NULL, NULL, NULL);
}

/* The last three arguments are used to report the positions for the
 * backreferences. On entry, *nump should contain the maximum number
 * positions to report. */

/**/
mod_export int
pattryrefs(Patprog prog, char *string, int *nump, int *begp, int *endp)
{
    int i, maxnpos = 0;
    char **sp, **ep;
    char *progstr = (char *)prog + prog->startoff;

    if (nump) {
	maxnpos = *nump;
	*nump = 0;
    }
    /* inherited from domatch, but why, exactly? */
    if (*string == Nularg)
	string++;

    patinstart = patinput = string;

    if (prog->flags & (PAT_PURES|PAT_ANY)) {
	if ((prog->flags & PAT_ANY) ||
	    ((prog->flags & PAT_NOANCH) ? 
	     !strncmp(progstr, string, prog->patmlen)
	     : !strcmp(progstr, string))) {
	    if ((prog->flags & PAT_NOGLD) && *string == '.')
		return 0;
	    /* in case used for ${..#..} etc. */
	    patinput = string + prog->patmlen;
	    /* if matching files, must update globbing flags */
	    patglobflags = prog->globend;
	    return 1;
	} else
	    return 0;
    } else {
	/*
	 * Test for a `must match' string, unless we're scanning for a match
	 * in which case we don't need to do this each time.
	 */
	if (!(prog->flags & PAT_SCAN) && prog->mustoff &&
	    !strstr(string, (char *)prog + prog->mustoff))
	    return 0;

	patinlen = 0;		/* don't calculate length unless needed */
	patflags = prog->flags;
	patglobflags = prog->globflags;
	if (!(patflags & PAT_FILE)) {
	    forceerrs = -1;
	    errsfound = 0;
	}
	globdots = !(patflags & PAT_NOGLD);
	parsfound = 0;

	if (patmatch((Upat)progstr)) {
	    /*
	     * we were lazy and didn't save the globflags if an exclusion
	     * failed, so set it now
	     */
	    patglobflags = prog->globend;
	    /*
	     * Should we clear backreferences and matches on a failed
	     * match?
	     */
	    if ((patglobflags & GF_MATCHREF) && !(patflags & PAT_FILE)) {
		/*
		 * m flag: for global match.  This carries no overhead
		 * in the pattern matching part.
		 */
		char *str;
		int mlen = ztrsub(patinput, patinstart);

		str = ztrduppfx(patinstart, patinput - patinstart);
		setsparam("MATCH", str);
		setiparam("MBEGIN", (zlong)(patoffset + !isset(KSHARRAYS)));
		setiparam("MEND",
			  (zlong)(mlen + patoffset + !isset(KSHARRAYS) - 1));
	    }
	    if (prog->patnpar && nump) {
		/*
		 * b flag: for backreferences using parentheses. Reported
		 * directly.
		 */
		*nump = prog->patnpar;

		sp = patbeginp;
		ep = patendp;

		for (i = 0; i < prog->patnpar && i < maxnpos; i++) {
		    if (parsfound & (1 << i)) {
			if (begp)
			    *begp++ = ztrsub(*sp, patinstart) + patoffset;
			if (endp)
			    *endp++ = ztrsub(*ep, patinstart) + patoffset - 1;
		    } else {
			if (begp)
			    *begp++ = -1;
			if (endp)
			    *endp++ = -1;
		    }

		    sp++;
		    ep++;
		}
	    } else if (prog->patnpar && !(patflags & PAT_FILE)) {
		/*
		 * b flag: for backreferences using parentheses.
		 */
		int palen = prog->patnpar+1;
		char **matcharr, **mbeginarr, **mendarr;
		char numbuf[DIGBUFSIZE];

		matcharr = zcalloc(palen*sizeof(char *));
		mbeginarr = zcalloc(palen*sizeof(char *));
		mendarr = zcalloc(palen*sizeof(char *));

		sp = patbeginp;
		ep = patendp;

		for (i = 0; i < prog->patnpar; i++) {
		    if (parsfound & (1 << i)) {
			matcharr[i] = ztrduppfx(*sp, *ep - *sp);
			/*
			 * mbegin and mend give indexes into the string
			 * in the standard notation, i.e. respecting
			 * KSHARRAYS, and with the end index giving
			 * the last character, not one beyond.
			 * For example, foo=foo; [[ $foo = (f)oo ]] gives
			 * (without KSHARRAYS) indexes 1 and 1, which
			 * corresponds to indexing as ${foo[1,1]}.
			 */
			sprintf(numbuf, "%ld",
				(long)(ztrsub(*sp, patinstart) + 
				       patoffset +
				       !isset(KSHARRAYS)));
			mbeginarr[i] = ztrdup(numbuf);
			sprintf(numbuf, "%ld",
				(long)(ztrsub(*ep, patinstart) + 
				       patoffset +
				       !isset(KSHARRAYS) - 1));
			mendarr[i] = ztrdup(numbuf);
		    } else {
			/* Pattern wasn't set: either it was in an
			 * unmatched branch, or a hashed parenthesis
			 * that didn't match at all.
			 */
			matcharr[i] = ztrdup("");
			mbeginarr[i] = ztrdup("-1");
			mendarr[i] = ztrdup("-1");
		    }
		    sp++;
		    ep++;
		}
		setaparam("match", matcharr);
		setaparam("mbegin", mbeginarr);
		setaparam("mend", mendarr);
	    }
	    return 1;
	} else
	    return 0;
    }
}

/*
 * Match literal characters with case insensitivity test:  the first
 * comes from the input string, the second the current pattern.
 */
#define CHARMATCH(chin, chpa) (chin == chpa || \
        ((patglobflags & GF_IGNCASE) ? \
	 ((isupper(chin) ? tolower(chin) : chin) == \
	  (isupper(chpa) ? tolower(chpa) : chpa)) : \
	 (patglobflags & GF_LCMATCHUC) ? \
	 (islower(chpa) && toupper(chpa) == chin) : 0))

/*
 * exactpos is used to remember how far down an exact string we have
 * matched, if we are doing approximation and can therefore redo from
 * the same point; we never need to otherwise.
 */
static char *exactpos;

/*
 * Main matching routine.
 *
 * Testing the tail end of a match is usually done by recursion, but
 * we try to eliminate that in favour of looping for simple cases.
 */

/**/
static int
patmatch(Upat prog)
{
    /* Current and next nodes */
    Upat scan = prog, next, opnd;
    char *start, *save, *chrop;
    int savglobflags, op, no, min, nextch, fail = 0, saverrsfound;
    zrange_t from, to, comp;

    while  (scan) {
	next = PATNEXT(scan);

	if (!globdots && P_NOTDOT(scan) && patinput == patinstart &&
	    *patinput == '.')
	    return 0;

	switch (P_OP(scan)) {
	case P_ANY:
	    if (!*patinput)
		fail = 1;
	    else
		METAINC(patinput);
	    break;
	case P_EXACTLY:
	    /*
	     * acts as nothing if *chrop is null:  this is used by
	     * approx code.
	     */
	    chrop = exactpos ? exactpos : (char *)P_OPERAND(scan);
	    exactpos = NULL;
	    while (*chrop && *patinput) {
		int chin = STOUC(UNMETA(patinput));
		int chpa = STOUC(UNMETA(chrop));
		if (!CHARMATCH(chin, chpa)) {
		    fail = 1;
		    break;
		}
		METAINC(chrop);
		METAINC(patinput);
	    }
	    if (*chrop) {
		exactpos = chrop;
		fail = 1;
	    }
	    break;
	case P_ANYOF:
	    if (!*patinput ||
		!patmatchrange((char *)P_OPERAND(scan),
			       STOUC(UNMETA(patinput))))
		fail = 1;
	    else
		METAINC(patinput);
	    break;
	case P_ANYBUT:
	    if (!*patinput ||
		patmatchrange((char *)P_OPERAND(scan),
			      STOUC(UNMETA(patinput))))
		fail = 1;
	    else
		METAINC(patinput);
	    break;
	case P_NUMRNG:
	case P_NUMFROM:
	case P_NUMTO:
	    /*
	     * To do this properly, we really have to treat numbers as
	     * closures:  that's so things like like <1-1000>33 will
	     * match 633 (they didn't up to 3.1.6).  To avoid making this
	     * too inefficient, we see if there's an exact match next:
	     * if there is, and it's not a digit, we return 1 after
	     * the first attempt.
	     */
	    op = P_OP(scan);
	    start = (char *)P_OPERAND(scan);
	    from = to = 0;
	    if (op != P_NUMTO) {
#ifdef ZSH_64_BIT_TYPE
		/* We can't rely on pointer alignment being good enough. */
		memcpy((char *)&from, start, sizeof(zrange_t));
#else
		from = *((zrange_t *) start);
#endif
		start += sizeof(zrange_t);
	    }
	    if (op != P_NUMFROM) {
#ifdef ZSH_64_BIT_TYPE
		memcpy((char *)&to, start, sizeof(zrange_t));
#else
		to = *((zrange_t *) start);
#endif
	    }
	    start = patinput;
	    comp = (zrange_t) zstrtol(patinput, &save, 10);
	    patinput = save;
	    no = 0;
	    while (patinput > start) {
		/* if already too small, no power on earth can save it */
		if (comp < from)
		    break;
		if ((op == P_NUMFROM || comp <= to) && patmatch(next)) {
		    return 1;
		}
		if (!no && P_OP(next) == P_EXACTLY &&
		    !idigit(STOUC(*(char *)P_OPERAND(next))) &&
		    !(patglobflags & 0xff))
		    return 0;
		patinput = --save;
		no++;
		comp /= 10;
	    }
	    patinput = start;
	    fail = 1;
	    break;
	case P_NUMANY:
	    /* This is <->: any old set of digits, don't bother comparing */
	    start = patinput;
	    while (idigit(STOUC(*patinput)))
		patinput++;
	    save = patinput;
	    no = 0;
	    while (patinput > start) {
		if (patmatch(next))
		    return 1;
		if (!no && P_OP(next) == P_EXACTLY &&
		    !idigit(STOUC(*(char *)P_OPERAND(next))) &&
		    !(patglobflags & 0xff))
		    return 0;
		patinput = --save;
		no++;
	    }
	    patinput = start;
	    fail = 1;
	    break;
	case P_NOTHING:
	    break;
	case P_BACK:
	    break;
	case P_GFLAGS:
	    patglobflags = P_OPERAND(scan)->l;
	    break;
	case P_OPEN:
	case P_OPEN+1:
	case P_OPEN+2:
	case P_OPEN+3:
	case P_OPEN+4:
	case P_OPEN+5:
	case P_OPEN+6:
	case P_OPEN+7:
	case P_OPEN+8:
	case P_OPEN+9:
	    no = P_OP(scan) - P_OPEN;
	    save = patinput;

	    if (patmatch(next)) {
		/*
		 * Don't set patbeginp if some later invocation of
		 * the same parentheses already has.
		 */
		if (no && !(parsfound & (1 << (no - 1)))) {
		    patbeginp[no-1] = save;
		    parsfound |= 1 << (no - 1);
		}
		return 1;
	    } else
		return 0;
	    break;
	case P_CLOSE:
	case P_CLOSE+1:
	case P_CLOSE+2:
	case P_CLOSE+3:
	case P_CLOSE+4:
	case P_CLOSE+5:
	case P_CLOSE+6:
	case P_CLOSE+7:
	case P_CLOSE+8:
	case P_CLOSE+9:
	    no = P_OP(scan) - P_CLOSE;
	    save = patinput;

	    if (patmatch(next)) {
		DPUTS(!patendp, "patendp not set for backreferencing");
		if (no && !(parsfound & (1 << (no + 15)))) {
		    patendp[no-1] = save;
		    parsfound |= 1 << (no + 15);
		}
		return 1;
	    } else
		return 0;
	    break;
	case P_EXCSYNC:
	    /* See the P_EXCLUDE code below for where syncptr comes from */
	    {
		unsigned char *syncptr;
		Upat after;
		after = P_OPERAND(scan);
		DPUTS(!P_ISEXCLUDE(after),
		      "BUG: EXCSYNC not followed by EXCLUDE.");
		DPUTS(!P_OPERAND(after)->p,
		      "BUG: EXCSYNC not handled by EXCLUDE");
		syncptr = P_OPERAND(after)->p + (patinput - patinstart);
		/*
		 * If we already matched from here, this time we fail.
		 * See WBRANCH code for story about error count.
		 */
		if (*syncptr && errsfound + 1 >= *syncptr)
		    return 0;
		/*
		 * Else record that we (possibly) matched this time.
		 * No harm if we don't:  then the previous test will just
		 * short cut the attempted match that is bound to fail.
		 * We never try to exclude something that has already
		 * failed anyway.
		 */
		*syncptr = errsfound + 1;
	    }
	    break;
	case P_EXCEND:
	    /*
	     * This is followed by a P_EXCSYNC, but only in the P_EXCLUDE
	     * branch.  Actually, we don't bother following it:  all we
	     * need to know is that we successfully matched so far up
	     * to the end of the asserted pattern; the endpoint
	     * in the target string is nulled out.
	     */
	    if (!(fail = (*patinput != '\0')))
		return 1;
	    break;
	case P_BRANCH:
	case P_WBRANCH:
	    /* P_EXCLUDE shouldn't occur without a P_BRANCH */
	    if (!P_ISBRANCH(next)) {
		/* no choice, avoid recursion */
		DPUTS(P_OP(scan) == P_WBRANCH,
		      "BUG: WBRANCH with no alternative.");
		next = P_OPERAND(scan);
	    } else {
		do {
		    save = patinput;
		    savglobflags = patglobflags;
		    saverrsfound = errsfound;
		    if (P_ISEXCLUDE(next)) {
			/*
			 * The strategy is to test the asserted pattern,
			 * recording via P_EXCSYNC how far the part to
			 * be excluded matched.  We then null out that
			 * point and see if the exclusion as far as
			 * P_EXCEND also matches that string.
			 * We need to keep testing the asserted pattern
			 * by backtracking, since the first attempt
			 * may be excluded while a later attempt may not.
			 * For this we keep a pointer just after
			 * the P_EXCLUDE which is tested by the P_EXCSYNC
			 * to see if we matched there last time, in which
			 * case we fail.  If there is nothing to backtrack
			 * over, that doesn't matter:  we should fail anyway.
			 * The pointer also tells us where the asserted
			 * pattern matched for use by the exclusion.
			 *
			 * P.S. in case you were wondering, this code
			 * is horrible.
			 */
			Upat syncstrp;
			unsigned char *oldsyncstr;
			char *matchpt = NULL;
			int ret, savglobdots, matchederrs = 0;
			int savparsfound = parsfound;
			DPUTS(P_OP(scan) == P_WBRANCH,
			      "BUG: excluded WBRANCH");
			syncstrp = P_OPERAND(next);
			/*
			 * Unlike WBRANCH, each test at the same exclude
			 * sync point (due to an external loop) is separate,
			 * i.e testing (foo~bar)# is no different from
			 * (foo~bar)(foo~bar)... from the exclusion point
			 * of view, so we use a different sync string.
			 */
			oldsyncstr = syncstrp->p;
			if (!patinlen)
			    patinlen = strlen(patinstart)+1;
			syncstrp->p = (unsigned char *)zcalloc(patinlen);
			while ((ret = patmatch(P_OPERAND(scan)))) {
			    unsigned char *syncpt;
			    char savchar, *testptr;
			    char *savpatinstart = patinstart;
			    int savforce = forceerrs, savpatinlen = patinlen;
			    int savpatflags = patflags;
			    forceerrs = -1;
			    savglobdots = globdots;
			    matchederrs = errsfound;
			    matchpt = patinput;    /* may not be end */
			    globdots = 1;	   /* OK to match . first */
			    /* Find the point where the scan
			     * matched the part to be excluded: because
			     * of backtracking, the one
			     * most recently matched will be the first.
			     * (Luckily, backtracking is done after all
			     * possibilities for approximation have been
			     * checked.)
			     */
			    for (syncpt = syncstrp->p; !*syncpt; syncpt++)
				;
			    testptr = patinstart + (syncpt - syncstrp->p);
			    DPUTS(testptr > matchpt, "BUG: EXCSYNC failed");
			    savchar = *testptr;
			    /*
			     * If this isn't really the end of the string,
			     * remember this for the (#e) assertion.
			     */
			    if (savchar)
				patflags |= PAT_NOTEND;
			    *testptr = '\0';
			    next = PATNEXT(scan);
			    while (next && P_ISEXCLUDE(next)) {
				char *buf = NULL;
				patinput = save;
				/*
				 * turn off approximations in exclusions:
				 * note we keep remaining patglobflags
				 * set by asserted branch (or previous
				 * excluded branches, for consistency).
				 */
				patglobflags &= ~0xff;
				errsfound = 0;
				opnd = P_OPERAND(next) + 1;
				if (P_OP(next) == P_EXCLUDP && pathpos) {
				    /*
				     * top level exclusion with a file,
				     * applies to whole path so add the
				     * segments already matched
				     */
				    DPUTS(patinput != patinstart,
					  "BUG: not at start excluding path");
				    buf = (char *)
					zalloc(pathpos + patinlen);
				    strcpy(buf, pathbuf);
				    strcpy(buf + pathpos, patinput);
				    patinput = patinstart = buf;
				    patinlen += pathpos;
				}
				if (patmatch(opnd)) {
				    ret = 0;
				    /*
				     * Another subtlety: if we exclude the
				     * match, any parentheses just found
				     * become invalidated.
				     */
				    parsfound = savparsfound;
				}
				if (buf) {
				    patinstart = savpatinstart;
				    patinlen = savpatinlen;
				    zfree(buf, pathpos + patinlen);
				}
				if (!ret)
				    break;
				next = PATNEXT(next);
			    }
			    *testptr = savchar;
			    patflags = savpatflags;
			    globdots = savglobdots;
			    forceerrs = savforce;
			    if (ret)
				break;
			    patinput = save;
			    patglobflags = savglobflags;
			    errsfound = saverrsfound;
			}
			zfree((char *)syncstrp->p, patinlen);
			syncstrp->p = oldsyncstr;
			if (ret) {
			    patinput = matchpt;
			    errsfound = matchederrs;
			    return 1;
			}
			while ((scan = PATNEXT(scan)) &&
			       P_ISEXCLUDE(scan))
			    ;
		    } else {
			int ret = 1, pfree = 0;
			Upat ptrp = NULL;
			unsigned char *ptr;
			if (P_OP(scan) == P_WBRANCH) {
			    /*
			     * This is where we make sure that we are not
			     * repeatedly matching zero-length strings in
			     * a closure, which would cause an infinite loop,
			     * and also remove exponential behaviour in
			     * backtracking nested closures.
			     * The P_WBRANCH operator leaves a space for a
			     * uchar *, initialized to NULL, which is
			     * turned into a string the same length as the
			     * target string.  Every time we match from a
			     * particular point in the target string, we
			     * stick a 1 at the corresponding point here.
			     * If we come round to the same branch again, and
			     * there is already a 1, then the test fails.
			     */
			    opnd = P_OPERAND(scan);
			    ptrp = opnd++;
			    if (!ptrp->p) {
				if (!patinlen)
				    patinlen = strlen((char *)patinstart)+1;
				ptrp->p = (unsigned char *)zcalloc(patinlen);
				pfree = 1;
			    }
			    ptr = ptrp->p + (patinput - patinstart);

			    /*
			     * Without approximation, this is just a
			     * single bit test.  With approximation, we
			     * need to know how many errors there were
			     * last time we made the test.  If errsfound
			     * is now smaller than it was, hence we can
			     * make more approximations in the remaining
			     * code, we continue with the test.
			     * (This is why the max number of errors is
			     * 254, not 255.)
			     */
			    if (*ptr && errsfound + 1 >= *ptr)
				ret = 0;
			    *ptr = errsfound + 1;
			} else
			    opnd = P_OPERAND(scan);
			if (ret)
			    ret = patmatch(opnd);
			if (pfree) {
			    zfree((char *)ptrp->p, patinlen);
			    ptrp->p = NULL;
			}
			if (ret)
			    return 1;
			scan = PATNEXT(scan);
		    }
		    patinput = save;
		    patglobflags = savglobflags;
		    errsfound = saverrsfound;
		    DPUTS(P_OP(scan) == P_WBRANCH,
			  "BUG: WBRANCH not first choice.");
		    next = PATNEXT(scan);
		} while (scan && P_ISBRANCH(scan));
		return 0;
	    }
	    break;
	case P_STAR:
	    /* Handle specially for speed, although really P_ONEHASH+P_ANY */
	case P_ONEHASH:
	case P_TWOHASH:
	    /*
	     * This is just simple cases, matching one character.
	     * With approximations, we still handle * this way, since
	     * no approximation is ever necessary, but other closures
	     * are handled by the more compicated branching method
	     */
	    op = P_OP(scan);
	    /* Note that no counts possibly metafied characters */
	    start = patinput;
	    if (op == P_STAR) {
		for (no = 0; *patinput; METAINC(patinput))
		    no++;
		/* simple optimization for reasonably common case */
		if (P_OP(next) == P_END)
		    return 1;
	    } else {
		DPUTS(patglobflags & 0xff,
		      "BUG: wrong backtracking with approximation.");
		if (!globdots && P_NOTDOT(P_OPERAND(scan)) &&
		    patinput == patinstart && *patinput == '.')
		    return 0;
		no = patrepeat(P_OPERAND(scan));
	    }
	    min = (op == P_TWOHASH) ? 1 : 0;
	    /*
	     * Lookahead to avoid useless matches. This is not possible
	     * with approximation.
	     */
	    if (P_OP(next) == P_EXACTLY && !(patglobflags & 0xff)) {
		char *nextop = (char *)P_OPERAND(next);
		/*
		 * If that P_EXACTLY is last (common in simple patterns,
		 * such as *.c), then it can be only be matched at one
		 * point in the test string, so record that.
		 */
		if (P_OP(PATNEXT(next)) == P_END &&
		    !(patflags & PAT_NOANCH)) {
		    int ptlen = strlen(patinput);
		    int oplen = strlen(nextop);
		    /* Are we in the right range? */
		    if (oplen > strlen(min ? METANEXT(start) : start) ||
			oplen < ptlen)
			return 0;
		    /* Yes, just position appropriately and test. */
		    patinput += ptlen - oplen;
		    if (patinput > start && patinput[-1] == Meta) {
			/* doesn't align properly, no go */
			return 0;
		    }
		    /* Continue loop with P_EXACTLY test. */
		    break;
		}
		nextch = STOUC(UNMETA(nextop));
	    } else
		nextch = -1;
	    save = patinput;
	    savglobflags = patglobflags;
	    saverrsfound = errsfound;
	    while (no >= min) {
		int chin;
		if ((nextch < 0 || (chin = STOUC(UNMETA(patinput)),
				    CHARMATCH(chin, nextch))) &&
		    patmatch(next))
		    return 1;
		no--;
		save--;
		if (save > start && save[-1] == Meta)
		    save--;
		patinput = save;
		patglobflags = savglobflags;
		errsfound = saverrsfound;
	    }
	    /*
	     * As with branches, the patmatch(next) stuff for *
	     * handles approximation, so we don't need to try
	     * anything here.
	     */
	    return 0;
	case P_ISSTART:
	    if (patinput != patinstart || (patflags & PAT_NOTSTART))
		fail = 1;
	    break;
	case P_ISEND:
	    if (*patinput || (patflags & PAT_NOTEND))
		fail = 1;
	    break;
	case P_END:
	    if (!(fail = (*patinput && !(patflags & PAT_NOANCH))))
		return 1;
	    break;
#ifdef DEBUG
	default:
	    dputs("BUG: bad operand in patmatch.");
	    return 0;
	    break;
#endif
	}

	if (fail) {
	    if (errsfound < (patglobflags & 0xff) &&
		(forceerrs == -1 || errsfound < forceerrs)) {
		/*
		 * Approximation code.  There are four possibilities
		 *
		 * 1. omit character from input string
		 * 2. transpose characters in input and pattern strings
		 * 3. omit character in both input and pattern strings
		 * 4. omit character from pattern string.
		 *
		 * which we try in that order.
		 *
		 * Of these, 2, 3 and 4 require an exact match string
		 * (P_EXACTLY) while 1, 2 and 3 require that we not
		 * have reached the end of the input string.
		 *
		 * Note in each case after making the approximation we
		 * need to retry the *same* pattern; this is what
		 * requires exactpos, a slightly doleful way of
		 * communicating with the exact character matcher.
		 */
		char *savexact = exactpos;
		save = patinput;
		savglobflags = patglobflags;
		saverrsfound = ++errsfound;
		fail = 0;

		DPUTS(P_OP(scan) != P_EXACTLY && exactpos,
		      "BUG: non-exact match has set exactpos");

		/* Try omitting a character from the input string */
		if (*patinput) {
		    METAINC(patinput);
		    /* If we are not on an exact match, then this is
		     * our last gasp effort, so we can optimize out
		     * the recursive call.
		     */
		    if (P_OP(scan) != P_EXACTLY)
			continue;
		    if (patmatch(scan))
			return 1;
		}

		if (P_OP(scan) == P_EXACTLY) {
		    char *nextexact = savexact;
		    DPUTS(!savexact || !*savexact,
			  "BUG: exact match has not set exactpos");
		    METAINC(nextexact);

		    if (*save) {
			char *nextin = save;
			METAINC(nextin);
			patglobflags = savglobflags;
			errsfound = saverrsfound;
			exactpos = savexact;

			/*
			 * Try swapping two characters in patinput and
			 * exactpos
			 */
			if (*save && *nextin && *nextexact) {
			    int cin0 = UNMETA(save);
			    int cpa0 = UNMETA(exactpos);
			    int cin1 = UNMETA(nextin);
			    int cpa1 = UNMETA(nextexact);

			    if (CHARMATCH(cin0, cpa1) &&
				CHARMATCH(cin1, cpa0)) {
				patinput = nextin;
				METAINC(patinput);
				exactpos = nextexact;
				METAINC(exactpos);
				if (patmatch(scan))
				    return 1;

				patglobflags = savglobflags;
				errsfound = saverrsfound;
			    }
			}

			/*
			 * Try moving up both strings.
			 */
			patinput = nextin;
			exactpos = nextexact;
			if (patmatch(scan))
			    return 1;

			patinput = save;
			patglobflags = savglobflags;
			errsfound = saverrsfound;
			exactpos = savexact;
		    }

		    /*
		     * Try moving up the exact match pattern.
		     * This must be the last attempt, so just loop
		     * instead of calling recursively.
		     */
		    METAINC(exactpos);
		    continue;
		}
	    }
	    exactpos = NULL;
	    return 0;
	}

	scan = next;
    }

    return 0;
}

/**/
static int
patmatchrange(char *range, int ch)
{
    int r1, r2;

    for (; *range; range++) {
	if (imeta(STOUC(*range))) {
	    switch (STOUC(*range)-STOUC(Meta)) {
	    case 0:
		if (STOUC(*++range ^ 32) == ch)
		    return 1;
		break;
	    case PP_ALPHA:
		if (isalpha(ch))
		    return 1;
		break;
	    case PP_ALNUM:
		if (isalnum(ch))
		    return 1;
		break;
	    case PP_BLANK:
		if (ch == ' ' || ch == '\t')
		    return 1;
		break;
	    case PP_CNTRL:
		if (iscntrl(ch))
		    return 1;
		break;
	    case PP_DIGIT:
		if (isdigit(ch))
		    return 1;
		break;
	    case PP_GRAPH:
		if (isgraph(ch))
		    return 1;
		break;
	    case PP_LOWER:
		if (islower(ch))
		    return 1;
		break;
	    case PP_PRINT:
		if (isprint(ch))
		    return 1;
		break;
	    case PP_PUNCT:
		if (ispunct(ch))
		    return 1;
		break;
	    case PP_SPACE:
		if (isspace(ch))
		    return 1;
		break;
	    case PP_UPPER:
		if (isupper(ch))
		    return 1;
		break;
	    case PP_XDIGIT:
		if (isxdigit(ch))
		    return 1;
	    case PP_RANGE:
		range++;
		r1 = STOUC(UNMETA(range));
		METAINC(range);
		r2 = STOUC(UNMETA(range));
		if (*range == Meta)
		    range++;
		if (r1 <= ch && ch <= r2)
		    return 1;
		break;
	    case PP_UNKWN:
		DPUTS(1, "BUG: unknown posix range passed through.\n");
		break;
	    default:
		DPUTS(1, "BUG: unknown metacharacter in range.");
		break;
	    }
	} else if (*range == ch)
	    return 1;
    }
    return 0;
}

/* repeatedly match something simple and say how many times */

/**/
static int patrepeat(Upat p)
{
    int count = 0, tch, inch;
    char *scan, *opnd;

    scan = patinput;
    opnd = (char *)P_OPERAND(p);

    switch(P_OP(p)) {
#ifdef DEBUG
    case P_ANY:
	dputs("BUG: ?# did not get optimized to *");
	return 0;
	break;
#endif
    case P_EXACTLY:
	tch = STOUC(UNMETA(opnd));
	while (*scan && (inch = STOUC(UNMETA(scan)), CHARMATCH(inch, tch))) {
	    count++;
	    METAINC(scan);
	}
	break;
    case P_ANYOF:
	while (*scan && patmatchrange(opnd, STOUC(UNMETA(scan)))) {
	    count++;
	    METAINC(scan);
    	}
	break;
    case P_ANYBUT:
	while (*scan && !patmatchrange(opnd, STOUC(UNMETA(scan)))) {
	    count++;
	    METAINC(scan);
    	}
	break;
#ifdef DEBUG
    default:
	dputs("BUG: something very strange is happening in patrepeat");
	return 0;
	break;
#endif
    }

    patinput = scan;
    return count;
}

/* Free a patprog. */

/**/
mod_export void
freepatprog(Patprog prog)
{
    if (prog && prog != dummy_patprog1 && prog != dummy_patprog2)
	zfree(prog, prog->size);
}

/**/
#ifdef ZSH_PAT_DEBUG

/* Debugging stuff: print and test a regular expression */

/* Dump a regexp onto stdout in vaguely comprehensible form */

/**/
static void
patdump(Patprog r)
{
    char *s, *base, op = P_EXACTLY;
    Upat up, codestart, next;

    base = (char *)r;
    s = base + r->startoff;

    if (r->flags & PAT_PURES) {
	printf("STRING:%s\n", (char *)s);
    } else {
	codestart = (Upat)s;
	while (op != P_END) {
	    up = (Upat)s;
	    op = P_OP(up);
	    printf("%2d%s", up-codestart, patprop(up));
	    next = PATNEXT(up);
	    printf("(%d)", next ? next-codestart : 0);
	    s += sizeof(union upat);
	    if (op == P_ANYOF || op == P_ANYBUT || op == P_EXACTLY) {
		while (*s != '\0') {
		    if (itok(*s)) {
			if (*s == Meta + PP_RANGE) {
			    s++;
			    printf("<RANGE:%c-", UNMETA(s));
			    METAINC(s);
			    printf("%c>", UNMETA(s));
			} else {
			    printf("<TYPE:%d>", *s - Meta);
			    s++;
			    continue;
			}
		    } else
			putchar(UNMETA(s));
		    METAINC(s);
		}
	    } else if (op == P_NUMRNG || op == P_NUMFROM || op == P_NUMTO) {
		printf("%lu", (unsigned long)*(zrange_t *)s);
		s += sizeof(zrange_t);
		if (op == P_NUMRNG) {
		    printf("-%lu", (unsigned long)*(zrange_t *)s);
		    s += sizeof(zrange_t);
		}
	    } else if (op == P_GFLAGS) {
		printf("%ld, %ld", (++up)->l & ~0xff, (++up)->l & 0xff);
		s += sizeof(union upat);
	    } else if (op == P_WBRANCH || op == P_EXCLUDE ||
		       op == P_EXCLUDP) {
		s += sizeof(union upat);
	    }
	    putchar('\n');
	    s = base + (((s - base) + sizeof(union upat) - 1) &
			~(sizeof(union upat) - 1));
	}
    }

    printf("Total size = %ld\n", r->size);
    if (r->patstartch)
	printf("start `%c' ", r->patstartch);
    if (!(r->flags & PAT_NOANCH))
	printf("EOL-anchor ");
    if (r->patnpar)
	printf("%d active backreferences ", r->patnpar);
    if (r->mustoff)
	printf("must have \"%s\"", (char *)r + r->mustoff);
    printf("\n");
    if (r->globflags) {
	printf("Globbing flags: ");
	if (r->globflags & GF_LCMATCHUC)
	    printf("LC matches UC ");
	if (r->globflags & GF_IGNCASE)
	    printf("Ignore case");
	printf("\n");
	if (r->globflags & 0xff)
	    printf("Max errors = %d\n", r->globflags & 0xff);
    }
}

/**/
static char *
patprop(Upat op)
{
    char *p = NULL;
    static char buf[50];

    strcpy(buf, ":");

    switch(P_OP(op)) {
    case P_ANY:
	p = "ANY";
	break;
    case P_ANYOF:
	p = "ANYOF";
	break;
    case P_ANYBUT:
	p = "ANYBUT";
	break;
    case P_BRANCH:
	p = "BRANCH";
	break;
    case P_WBRANCH:
	p = "WBRANCH";
	break;
    case P_EXCLUDE:
	p = "EXCLUDE";
	break;
    case P_EXCLUDP:
	p = "EXCLUDP";
	break;
    case P_EXCSYNC:
	p = "EXCSYNC";
	break;
    case P_EXCEND:
	p = "EXCEND";
	break;
    case P_EXACTLY:
	p = "EXACTLY";
	break;
    case P_GFLAGS:
	p = "GFLAGS";
	break;
    case P_ISSTART:
	p = "ISSTART";
	break;
    case P_ISEND:
	p = "ISEND";
	break;
    case P_NOTHING:
	p = "NOTHING";
	break;
    case P_BACK:
	p = "BACK";
	break;
    case P_END:
	p = "END";
	break;
    case P_OPEN:
    case P_OPEN+1:
    case P_OPEN+2:
    case P_OPEN+3:
    case P_OPEN+4:
    case P_OPEN+5:
    case P_OPEN+6:
    case P_OPEN+7:
    case P_OPEN+8:
    case P_OPEN+9:
	sprintf(buf+strlen(buf), "OPEN%ld", P_OP(op)-P_OPEN);
	p = NULL;
	break;
    case P_CLOSE:
    case P_CLOSE+1:
    case P_CLOSE+2:
    case P_CLOSE+3:
    case P_CLOSE+4:
    case P_CLOSE+5:
    case P_CLOSE+6:
    case P_CLOSE+7:
    case P_CLOSE+8:
    case P_CLOSE+9:
	sprintf(buf+strlen(buf), "CLOSE%ld", P_OP(op)-P_CLOSE);
	p = NULL;
	break;
    case P_STAR:
	p = "STAR";
	break;
    case P_ONEHASH:
	p = "ONEHASH";
	break;
    case P_TWOHASH:
	p = "TWOHASH";
	break;
    case P_NUMRNG:
	p = "NUMRNG";
	break;
    case P_NUMFROM:
	p = "NUMFROM";
	break;
    case P_NUMTO:
	p = "NUMTO";
	break;
    case P_NUMANY:
	p = "NUMANY";
	break;
    default:
	fprintf(stderr, "Bad opcode\n");
	p = NULL;
	break;
    }
    if (p)
	strcat(buf, p);
    return buf;
}

/**/
int
bin_patdebug(char *name, char **args, char *ops, int func)
{
    Patprog prog;
    int ret = 0;

    tokenize(*args);

    if (!(prog = patcompile((char *)*args, 0, 0)))
	return 1;
    if (ops['p'] || !args[1]) {
	patdump(prog);
    }

    while (*++args) {
	if (!pattry(prog, (char *)*args))
	    ret++;
    }
    return ret;
}

/**/
#endif /* ZSH_PAT_DEBUG */
