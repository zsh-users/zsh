/*
 * comp.h - header file for completion
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

#undef compctlread

typedef struct compctlp  *Compctlp;
typedef struct compctl   *Compctl;
typedef struct compcond  *Compcond;
typedef struct patcomp   *Patcomp;
typedef struct cmatcher  *Cmatcher;
typedef struct cmlist    *Cmlist;
typedef struct cpattern  *Cpattern;

/* node for compctl hash table (compctltab) */

struct compctlp {
    HashNode next;		/* next in hash chain               */
    char *nam;			/* command name                     */
    int flags;			/* CURRENTLY UNUSED                 */
    Compctl cc;			/* pointer to the compctl desc.     */
};

/* for the list of pattern compctls */

struct patcomp {
    Patcomp next;
    char *pat;
    Compctl cc;
};

/* compctl -x condition */

struct compcond {
    Compcond and, or;		/* the next or'ed/and'ed conditions    */
    int type;			/* the type (CCT_*)                    */
    int n;			/* the array length                    */
    union {			/* these structs hold the data used to */
	struct {		/* test this condition                 */
	    int *a, *b;		/* CCT_POS, CCT_NUMWORDS               */
	}
	r;
	struct {		/* CCT_CURSTR, CCT_CURPAT,... */
	    int *p;
	    char **s;
	}
	s;
	struct {		/* CCT_RANGESTR,... */
	    char **a, **b;
	}
	l;
    }
    u;
};

#define CCT_UNUSED     0
#define CCT_POS        1
#define CCT_CURSTR     2
#define CCT_CURPAT     3
#define CCT_WORDSTR    4
#define CCT_WORDPAT    5
#define CCT_CURSUF     6
#define CCT_CURPRE     7
#define CCT_CURSUB     8
#define CCT_CURSUBC    9
#define CCT_NUMWORDS  10
#define CCT_RANGESTR  11
#define CCT_RANGEPAT  12

/* Contains the real description for compctls */

struct compctl {
    int refc;			/* reference count                         */
    Compctl next;		/* next compctl for -x                     */
    unsigned long mask, mask2;	/* masks of things to complete (CC_*)      */
    char *keyvar;		/* for -k (variable)                       */
    char *glob;			/* for -g (globbing)                       */
    char *str;			/* for -s (expansion)                      */
    char *func;			/* for -K (function)                       */
    char *widget;		/* for -i (function)                       */
    char *explain;		/* for -X (explanation)                    */
    char *ylist;		/* for -y (user-defined desc. for listing) */
    char *prefix, *suffix;	/* for -P and -S (prefix, suffix)          */
    char *subcmd;		/* for -l (command name to use)            */
    char *withd;		/* for -w (with directory                  */
    char *hpat;			/* for -H (history pattern)                */
    int hnum;			/* for -H (number of events to search)     */
    char *gname;		/* for -J and -V (group name)              */
    Compctl ext;		/* for -x (first of the compctls after -x) */
    Compcond cond;		/* for -x (condition for this compctl)     */
    Compctl xor;		/* for + (next of the xor'ed compctls)     */
    Cmatcher matcher;		/* matcher control (-M) */
    char *mstr;			/* matcher string */
};

/* objects to complete (mask) */
#define CC_FILES	(1<<0)
#define CC_COMMPATH	(1<<1)
#define CC_REMOVE	(1<<2)
#define CC_OPTIONS	(1<<3)
#define CC_VARS		(1<<4)
#define CC_BINDINGS	(1<<5)
#define CC_ARRAYS	(1<<6)
#define CC_INTVARS	(1<<7)
#define CC_SHFUNCS	(1<<8)
#define CC_PARAMS	(1<<9)
#define CC_ENVVARS	(1<<10)
#define CC_JOBS		(1<<11)
#define CC_RUNNING	(1<<12)
#define CC_STOPPED	(1<<13)
#define CC_BUILTINS	(1<<14)
#define CC_ALREG	(1<<15)
#define CC_ALGLOB	(1<<16)
#define CC_USERS	(1<<17)
#define CC_DISCMDS	(1<<18)
#define CC_EXCMDS	(1<<19)
#define CC_SCALARS	(1<<20)
#define CC_READONLYS	(1<<21)
#define CC_SPECIALS	(1<<22)
#define CC_DELETE	(1<<23)
#define CC_NAMED	(1<<24)
#define CC_QUOTEFLAG	(1<<25)
#define CC_EXTCMDS	(1<<26)
#define CC_RESWDS	(1<<27)
#define CC_DIRS		(1<<28)

#define CC_EXPANDEXPL	(1<<30)
#define CC_RESERVED	(1<<31)

/* objects to complete (mask2) */
#define CC_NOSORT	(1<<0)
#define CC_XORCONT	(1<<1)
#define CC_CCCONT	(1<<2)
#define CC_PATCONT	(1<<3)
#define CC_DEFCONT	(1<<4)

typedef struct cexpl *Cexpl;
typedef struct cmgroup *Cmgroup;
typedef struct cmatch *Cmatch;

/* This is for explantion strings. */

struct cexpl {
    char *str;			/* the string */
    int count;			/* the number of matches */
    int fcount;			/* number of matches with fignore ignored */
};

/* This describes a group of matches. */

struct cmgroup {
    char *name;			/* the name of this group */
    Cmgroup prev;		/* previous on the list */
    Cmgroup next;		/* next one in list */
    int flags;			/* see CGF_* below */
    int mcount;			/* number of matches */
    Cmatch *matches;		/* the matches */
    int lcount;			/* number of things to list here */
    char **ylist;		/* things to list */
    int ecount;			/* number of explanation string */
    Cexpl *expls;		/* explanation strings */
    int ccount;			/* number of compctls used */
    Compctl *ccs;		/* the compctls used */
    LinkList lexpls;		/* list of explanation string while building */
    LinkList lmatches;		/* list of matches */
    LinkList lfmatches;		/* list of matches without fignore */
    LinkList lallccs;		/* list of used compctls */
    int num;			/* number of this group */
};


#define CGF_NOSORT  1		/* don't sort this group */
#define CGF_LINES   2		/* these are to be printed on different lines */

/* This is the struct used to hold matches. */

struct cmatch {
    char *str;			/* the match itself */
    char *ipre;			/* ignored prefix, has to be re-inserted */
    char *ripre;		/* ignored prefix, unquoted */
    char *isuf;			/* ignored suffix */
    char *ppre;			/* the path prefix */
    char *psuf;			/* the path suffix */
    char *prpre;		/* path prefix for opendir */
    char *pre;			/* prefix string from -P */
    char *suf;			/* suffix string from -S */
    int flags;			/* see CMF_* below */
    int brpl;			/* the place where to put the brace prefix */
    int brsl;			/* ...and the suffix */
    char *rems;			/* when to remove the suffix */
    char *remf;			/* shell function to call for suffix-removal */
    int rnum;			/* group relative number */
    int gnum;			/* global number */
};

#define CMF_FILE     1		/* this is a file */
#define CMF_REMOVE   2		/* remove the suffix */
#define CMF_PARBR    4		/* paramter expansion with a brace */
#define CMF_NOLIST   8		/* should not be listed */


/* Stuff for completion matcher control. */

struct cmlist {
    Cmlist next;		/* next one in the list of global matchers */
    Cmatcher matcher;		/* the matcher definition */
    char *str;			/* the string for it */
};

struct cmatcher {
    int refc;			/* reference counter */
    Cmatcher next;		/* next matcher */
    int flags;			/* see CMF_* below */
    Cpattern line;		/* what matches on the line */
    int llen;			/* length of line pattern */
    Cpattern word;		/* what matches in the word */
    int wlen;			/* length of word pattern */
    Cpattern left;		/* left anchor */
    int lalen;			/* length of left anchor */
    Cpattern right;		/* right anchor */
    int ralen;			/* length of right anchor */
};


#define CMF_LINE  1
#define CMF_LEFT  2
#define CMF_RIGHT 4


struct cpattern {
    Cpattern next;		/* next sub-pattern */
    unsigned char tab[256];	/* table of matched characters */
    int equiv;			/* if this is a {...} class */
};

/* Flags for makecomplist*(). Things not to do. */

#define CFN_FIRST   1
#define CFN_DEFAULT 2

/* Flags for compadd and addmatches(). */

#define CAF_QUOTE    1
#define CAF_NOSORT   2
#define CAF_ALT      4
#define CAF_MATCH    8

/* Flags for special parameters. */

#define CP_WORDS      (1 <<  0)
#define CP_CURRENT    (1 <<  1)
#define CP_PREFIX     (1 <<  2)
#define CP_SUFFIX     (1 <<  3)
#define CP_IPREFIX    (1 <<  4)
#define CP_ISUFFIX    (1 <<  5)
#define CP_COMPSTATE  (1 <<  6)

#define CP_REALPARAMS        7

#define CP_NMATCHES   (1 <<  7)
#define CP_MATCHER    (1 <<  8)
#define CP_MATCHERSTR (1 <<  9)
#define CP_MATCHERTOT (1 << 10)
#define CP_CONTEXT    (1 << 11)
#define CP_PARAMETER  (1 << 12)
#define CP_REDIRECT   (1 << 13)
#define CP_QUOTE      (1 << 14)
#define CP_QUOTING    (1 << 15)
#define CP_RESTORE    (1 << 16)
#define CP_LIST       (1 << 17)
#define CP_FORCELIST  (1 << 18)
#define CP_INSERT     (1 << 19)
#define CP_EXACT      (1 << 20)
#define CP_EXACTSTR   (1 << 21)
#define CP_PATMATCH   (1 << 22)
#define CP_PATINSERT  (1 << 23)
#define CP_UNAMBIG    (1 << 24)
#define CP_UNAMBIGC   (1 << 25)
#define CP_LISTMAX    (1 << 26)
#define CP_LASTPROMPT (1 << 27)
#define CP_TOEND      (1 << 28)
#define CP_OLDLIST    (1 << 29)
#define CP_OLDINS     (1 << 30)

#define CP_NUM              31

#define CP_ALLMASK    ((int) ((((unsigned int) 1) << CP_NUM) - 1))
