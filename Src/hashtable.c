/*
 * hashtable.c - hash tables
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

#include "../config.h"

#ifdef ZSH_HASH_DEBUG
# define HASHTABLE_DEBUG_MEMBERS \
    /* Members of struct hashtable used for debugging hash tables */ \
    HashTable next, last;	/* linked list of all hash tables           */ \
    char *tablename;		/* string containing name of the hash table */ \
    PrintTableStats printinfo;	/* pointer to function to print table stats */
#else /* !ZSH_HASH_DEBUG */
# define HASHTABLE_DEBUG_MEMBERS
#endif /* !ZSH_HASH_DEBUG */

#define HASHTABLE_INTERNAL_MEMBERS \
    ScanStatus scan;		/* status of a scan over this hashtable     */ \
    HASHTABLE_DEBUG_MEMBERS

typedef struct scanstatus *ScanStatus;

#include "zsh.mdh"
#include "hashtable.pro"

/* Structure for recording status of a hashtable scan in progress.  When a *
 * scan starts, the .scan member of the hashtable structure points to one  *
 * of these.  That member being non-NULL disables resizing of the          *
 * hashtable (when adding elements).  When elements are deleted, the       *
 * contents of this structure is used to make sure the scan won't stumble  *
 * into the deleted element.                                               */

struct scanstatus {
    int sorted;
    union {
	struct {
	    HashNode *tab;
	    int ct;
	} s;
	HashNode u;
    } u;
};

/********************************/
/* Generic Hash Table functions */
/********************************/

#ifdef ZSH_HASH_DEBUG
static HashTable firstht, lastht;
#endif /* ZSH_HASH_DEBUG */

/* Generic hash function */

/**/
unsigned
hasher(char *str)
{
    unsigned hashval = 0;

    while (*str)
	hashval += (hashval << 5) + ((unsigned) *str++);

    return hashval;
}

/* Get a new hash table */

/**/
HashTable
newhashtable(int size, char const *name, PrintTableStats printinfo)
{
    HashTable ht;

    ht = (HashTable) zcalloc(sizeof *ht);
#ifdef ZSH_HASH_DEBUG
    ht->next = NULL;
    if(!firstht)
	firstht = ht;
    ht->last = lastht;
    if(lastht)
	lastht->next = ht;
    lastht = ht;
    ht->printinfo = printinfo ? printinfo : printhashtabinfo;
    ht->tablename = ztrdup(name);
#endif /* ZSH_HASH_DEBUG */
    ht->nodes = (HashNode *) zcalloc(size * sizeof(HashNode));
    ht->hsize = size;
    ht->ct = 0;
    ht->scan = NULL;
    return ht;
}

/* Delete a hash table.  After this function has been used, any *
 * existing pointers to the hash table are invalid.             */

/**/
void
deletehashtable(HashTable ht)
{
    ht->emptytable(ht);
#ifdef ZSH_HASH_DEBUG
    if(ht->next)
	ht->next->last = ht->last;
    else
	lastht = ht->last;
    if(ht->last)
	ht->last->next = ht->next;
    else
	firstht = ht->next;
#endif /* ZSH_HASH_DEBUG */
    zfree(ht->nodes, ht->hsize * sizeof(HashNode));
    zfree(ht, sizeof(*ht));
}

/* Add a node to a hash table.                          *
 * nam is the key to use in hashing.  dat is a pointer  *
 * to the node to add.  If there is already a node in   *
 * the table with the same key, it is first freed, and  *
 * then the new node is added.  If the number of nodes  *
 * is now greater than twice the number of hash values, *
 * the table is then expanded.                          */

/**/
void
addhashnode(HashTable ht, char *nam, void *nodeptr)
{
    unsigned hashval;
    HashNode hn, hp, hq;

    hn = (HashNode) nodeptr;
    hn->nam = nam;

    hashval = ht->hash(hn->nam) % ht->hsize;
    hp = ht->nodes[hashval];

    /* check if this is the first node for this hash value */
    if (!hp) {
	hn->next = NULL;
	ht->nodes[hashval] = hn;
	if (++ht->ct >= ht->hsize * 2 && !ht->scan)
	    expandhashtable(ht);
	return;
    }

    /* else check if the first node contains the same key */
    if (!strcmp(hp->nam, hn->nam)) {
	ht->nodes[hashval] = hn;
	replacing:
	hn->next = hp->next;
	if(ht->scan)
	    if(ht->scan->sorted) {
		HashNode *tab = ht->scan->u.s.tab;
		int i;
		for(i = ht->scan->u.s.ct; i--; )
		    if(tab[i] == hp)
			tab[i] = hn;
	    } else if(ht->scan->u.u == hp)
		ht->scan->u.u = hn;
	ht->freenode(hp);
	return;
    }

    /* else run through the list and check all the keys */
    hq = hp;
    hp = hp->next;
    for (; hp; hq = hp, hp = hp->next) {
	if (!strcmp(hp->nam, hn->nam)) {
	    hq->next = hn;
	    goto replacing;
	}
    }

    /* else just add it at the front of the list */
    hn->next = ht->nodes[hashval];
    ht->nodes[hashval] = hn;
    if (++ht->ct >= ht->hsize * 2 && !ht->scan)
        expandhashtable(ht);
}

/* Get an enabled entry in a hash table.  *
 * If successful, it returns a pointer to *
 * the hashnode.  If the node is DISABLED *
 * or isn't found, it returns NULL        */

/**/
HashNode
gethashnode(HashTable ht, char *nam)
{
    unsigned hashval;
    HashNode hp;

    hashval = ht->hash(nam) % ht->hsize;
    for (hp = ht->nodes[hashval]; hp; hp = hp->next) {
	if (!strcmp(hp->nam, nam)) {
	    if (hp->flags & DISABLED)
		return NULL;
	    else
		return hp;
	}
    }
    return NULL;
}

/* Get an entry in a hash table.  It will *
 * ignore the DISABLED flag and return a  *
 * pointer to the hashnode if found, else *
 * it returns NULL.                       */

/**/
HashNode
gethashnode2(HashTable ht, char *nam)
{
    unsigned hashval;
    HashNode hp;

    hashval = ht->hash(nam) % ht->hsize;
    for (hp = ht->nodes[hashval]; hp; hp = hp->next) {
	if (!strcmp(hp->nam, nam))
	    return hp;
    }
    return NULL;
}

/* Remove an entry from a hash table.           *
 * If successful, it removes the node from the  *
 * table and returns a pointer to it.  If there *
 * is no such node, then it returns NULL        */

/**/
HashNode
removehashnode(HashTable ht, char *nam)
{
    unsigned hashval;
    HashNode hp, hq;

    hashval = ht->hash(nam) % ht->hsize;
    hp = ht->nodes[hashval];

    /* if no nodes at this hash value, return NULL */
    if (!hp)
	return NULL;

    /* else check if the key in the first one matches */
    if (!strcmp(hp->nam, nam)) {
	ht->nodes[hashval] = hp->next;
	gotit:
	ht->ct--;
	if(ht->scan)
	    if(ht->scan->sorted) {
		HashNode *tab = ht->scan->u.s.tab;
		int i;
		for(i = ht->scan->u.s.ct; i--; )
		    if(tab[i] == hp)
			tab[i] = NULL;
	    } else if(ht->scan->u.u == hp)
		ht->scan->u.u = hp->next;
	return hp;
    }

    /* else run through the list and check the rest of the keys */
    hq = hp;
    hp = hp->next;
    for (; hp; hq = hp, hp = hp->next) {
	if (!strcmp(hp->nam, nam)) {
	    hq->next = hp->next;
	    goto gotit;
	}
    }

    /* else it is not in the list, so return NULL */
    return NULL;
}

/* Disable a node in a hash table */

/**/
void
disablehashnode(HashNode hn, int flags)
{
    hn->flags |= DISABLED;
}

/* Enable a node in a hash table */

/**/
void
enablehashnode(HashNode hn, int flags)
{
    hn->flags &= ~DISABLED;
}

/* Compare two hash table entries */

/**/
static int
hnamcmp(const void *ap, const void *bp)
{
    HashNode a = *(HashNode *)ap;
    HashNode b = *(HashNode *)bp;
    return ztrcmp((unsigned char *) a->nam, (unsigned char *) b->nam);
}

/* Scan the nodes in a hash table and execute scanfunc on nodes based on
 * the flags that are set/unset.  scanflags is passed unchanged to
 * scanfunc (if executed).
 *
 * If sorted != 0, then sort entries of hash table before scanning.
 * If flags1 > 0, then execute scanfunc on a node only if at least one of
 *                these flags is set.
 * If flags2 > 0, then execute scanfunc on a node only if all of
 *                these flags are NOT set.
 * The conditions above for flags1/flags2 must both be true.
 *
 * It is safe to add, remove or replace hash table elements from within
 * the scanfunc.  Replaced elements will appear in the scan exactly once,
 * the new version if it was not scanned before the replacement was made.
 * Added elements might or might not appear in the scan.
 */

/**/
void
scanhashtable(HashTable ht, int sorted, int flags1, int flags2, ScanFunc scanfunc, int scanflags)
{
    struct scanstatus st;

    if (sorted) {
	int i, ct = ht->ct;
	VARARR(HashNode, hnsorttab, ct);
	HashNode *htp, hn;

	for (htp = hnsorttab, i = 0; i < ht->hsize; i++)
	    for (hn = ht->nodes[i]; hn; hn = hn->next)
		*htp++ = hn;
	qsort((void *)hnsorttab, ct, sizeof(HashNode), hnamcmp);

	st.sorted = 1;
	st.u.s.tab = hnsorttab;
	st.u.s.ct = ct;
	ht->scan = &st;

	for (htp = hnsorttab, i = 0; i < ct; i++, htp++)
	    if (*htp && ((*htp)->flags & flags1) + !flags1 &&
		!((*htp)->flags & flags2))
		scanfunc(*htp, scanflags);

	ht->scan = NULL;
    } else {
	int i, hsize = ht->hsize;
	HashNode *nodes = ht->nodes;

	st.sorted = 0;
	ht->scan = &st;

	for (i = 0; i < hsize; i++)
	    for (st.u.u = nodes[i]; st.u.u; ) {
		HashNode hn = st.u.u;
		st.u.u = st.u.u->next;
		if ((hn->flags & flags1) + !flags1 && !(hn->flags & flags2))
		    scanfunc(hn, scanflags);
	    }

	ht->scan = NULL;
    }
}

/* Scan all nodes in a hash table and executes scanfunc on the *
 * nodes which meet all the following criteria:                *
 * The hash key must match the glob pattern given by `com'.    *
 * If (flags1 > 0), then any flag in flags1 must be set.       *
 * If (flags2 > 0), then all flags in flags2 must NOT be set.  *
 *                                                             *
 * scanflags is passed unchanged to scanfunc (if executed).    *
 * The return value is the number of matches.                  */

/**/
int
scanmatchtable(HashTable ht, Comp com, int flags1, int flags2, ScanFunc scanfunc, int scanflags)
{
    int i, hsize = ht->hsize;
    HashNode *nodes = ht->nodes;
    int match = 0;
    struct scanstatus st;

    st.sorted = 0;
    ht->scan = &st;

    for (i = 0; i < hsize; i++)
	for (st.u.u = nodes[i]; st.u.u; ) {
	    HashNode hn = st.u.u;
	    st.u.u = st.u.u->next;
	    if ((hn->flags & flags1) + !flags1 && !(hn->flags & flags2) &&
		domatch(hn->nam, com, 0))
		scanfunc(hn, scanflags);
		match++;
	}

    ht->scan = NULL;

    return match;
}

/* Expand hash tables when they get too many entries. *
 * The new size is 4 times the previous size.         */

/**/
static void
expandhashtable(HashTable ht)
{
    struct hashnode **onodes, **ha, *hn, *hp;
    int i, osize;

    osize = ht->hsize;
    onodes = ht->nodes;

    ht->hsize = osize * 4;
    ht->nodes = (HashNode *) zcalloc(ht->hsize * sizeof(HashNode));
    ht->ct = 0;

    /* scan through the old list of nodes, and *
     * rehash them into the new list of nodes  */
    for (i = 0, ha = onodes; i < osize; i++, ha++) {
	for (hn = *ha; hn;) {
	    hp = hn->next;
	    ht->addnode(ht, hn->nam, hn);
	    hn = hp;
	}
    }
    zfree(onodes, osize * sizeof(HashNode));
}

/* Empty the hash table and resize it if necessary */

/**/
static void
resizehashtable(HashTable ht, int newsize)
{
    struct hashnode **ha, *hn, *hp;
    int i;

    /* free all the hash nodes */
    ha = ht->nodes;
    for (i = 0; i < ht->hsize; i++, ha++) {
	for (hn = *ha; hn;) {
	    hp = hn->next;
	    ht->freenode(hn);
	    hn = hp;
	}
    }

    /* If new size desired is different from current size, *
     * we free it and allocate a new nodes array.          */
    if (ht->hsize != newsize) {
	zfree(ht->nodes, ht->hsize * sizeof(HashNode));
	ht->nodes = (HashNode *) zcalloc(newsize * sizeof(HashNode));
	ht->hsize = newsize;
    } else {
	/* else we just re-zero the current nodes array */
	memset(ht->nodes, 0, newsize * sizeof(HashNode));
    }

    ht->ct = 0;
}

/* Generic method to empty a hash table */

/**/
void
emptyhashtable(HashTable ht)
{
    resizehashtable(ht, ht->hsize);
}

#ifdef ZSH_HASH_DEBUG

/* Print info about hash table */

#define MAXDEPTH 7

/**/
static void
printhashtabinfo(HashTable ht)
{
    HashNode hn;
    int chainlen[MAXDEPTH + 1];
    int i, tmpcount, total;

    printf("name of table   : %s\n",   ht->tablename);
    printf("size of nodes[] : %d\n",   ht->hsize);
    printf("number of nodes : %d\n\n", ht->ct);

    memset(chainlen, 0, sizeof(chainlen));

    /* count the number of nodes just to be sure */
    total = 0;
    for (i = 0; i < ht->hsize; i++) {
	tmpcount = 0;
	for (hn = ht->nodes[i]; hn; hn = hn->next)
	    tmpcount++;
	if (tmpcount >= MAXDEPTH)
	    chainlen[MAXDEPTH]++;
	else
	    chainlen[tmpcount]++;
	total += tmpcount;
    }

    for (i = 0; i < MAXDEPTH; i++)
	printf("number of hash values with chain of length %d  : %4d\n", i, chainlen[i]);
    printf("number of hash values with chain of length %d+ : %4d\n", MAXDEPTH, chainlen[MAXDEPTH]);
    printf("total number of nodes                         : %4d\n", total);
}

/**/
int
bin_hashinfo(char *nam, char **args, char *ops, int func)
{
    HashTable ht;
    printf("----------------------------------------------------\n");
    for(ht = firstht; ht; ht = ht->next) {
	ht->printinfo(ht);
	printf("----------------------------------------------------\n");
    }
    return 0;
}

#endif /* ZSH_HASH_DEBUG */

/********************************/
/* Command Hash Table Functions */
/********************************/

/* hash table containing external commands */
 
/**/
HashTable cmdnamtab;
 
/* how far we've hashed the PATH so far */
 
/**/
char **pathchecked;
 
/* Create a new command hash table */
 
/**/
void
createcmdnamtable(void)
{
    cmdnamtab = newhashtable(201, "cmdnamtab", NULL);

    cmdnamtab->hash        = hasher;
    cmdnamtab->emptytable  = emptycmdnamtable;
    cmdnamtab->filltable   = fillcmdnamtable;
    cmdnamtab->addnode     = addhashnode;
    cmdnamtab->getnode     = gethashnode2;
    cmdnamtab->getnode2    = gethashnode2;
    cmdnamtab->removenode  = removehashnode;
    cmdnamtab->disablenode = NULL;
    cmdnamtab->enablenode  = NULL;
    cmdnamtab->freenode    = freecmdnamnode;
    cmdnamtab->printnode   = printcmdnamnode;

    pathchecked = path;
}

/**/
static void
emptycmdnamtable(HashTable ht)
{
    emptyhashtable(ht);
    pathchecked = path;
}

/* Add all commands in a given directory *
 * to the command hashtable.             */

/**/
void
hashdir(char **dirp)
{
    Cmdnam cn;
    DIR *dir;
    char *fn;

    if (isrelative(*dirp) || !(dir = opendir(unmeta(*dirp))))
	return;

    while ((fn = zreaddir(dir, 1))) {
	if (!cmdnamtab->getnode(cmdnamtab, fn)) {
	    cn = (Cmdnam) zcalloc(sizeof *cn);
	    cn->flags = 0;
	    cn->u.name = dirp;
	    cmdnamtab->addnode(cmdnamtab, ztrdup(fn), cn);
	}
    }
    closedir(dir);
}

/* Go through user's PATH and add everything to *
 * the command hashtable.                       */

/**/
static void
fillcmdnamtable(HashTable ht)
{
    char **pq;
 
    for (pq = pathchecked; *pq; pq++)
	hashdir(pq);

    pathchecked = pq;
}

/**/
static void
freecmdnamnode(HashNode hn)
{
    Cmdnam cn = (Cmdnam) hn;
 
    zsfree(cn->nam);
    if (cn->flags & HASHED)
	zsfree(cn->u.cmd);
 
    zfree(cn, sizeof(struct cmdnam));
}

/* Print an element of the cmdnamtab hash table (external command) */
 
/**/
static void
printcmdnamnode(HashNode hn, int printflags)
{
    Cmdnam cn = (Cmdnam) hn;

    if (printflags & PRINT_WHENCE_WORD) {
	printf("%s: %s\n", cn->nam, (cn->flags & HASHED) ? 
	       "hashed" : "command");
	return;
    }

    if ((printflags & PRINT_WHENCE_CSH) || (printflags & PRINT_WHENCE_SIMPLE)) {
	if (cn->flags & HASHED) {
	    zputs(cn->u.cmd, stdout);
	    putchar('\n');
	} else {
	    zputs(*(cn->u.name), stdout);
	    putchar('/');
	    zputs(cn->nam, stdout);
	    putchar('\n');
	}
	return;
    }

    if (printflags & PRINT_WHENCE_VERBOSE) {
	if (cn->flags & HASHED) {
	    nicezputs(cn->nam, stdout);
	    printf(" is hashed to ");
	    nicezputs(cn->u.cmd, stdout);
	    putchar('\n');
	} else {
	    nicezputs(cn->nam, stdout);
	    printf(" is ");
	    nicezputs(*(cn->u.name), stdout);
	    putchar('/');
	    nicezputs(cn->nam, stdout);
	    putchar('\n');
	}
	return;
    }

    if (cn->flags & HASHED) {
	quotedzputs(cn->nam, stdout);
	putchar('=');
	quotedzputs(cn->u.cmd, stdout);
	putchar('\n');
    } else {
	quotedzputs(cn->nam, stdout);
	putchar('=');
	quotedzputs(*(cn->u.name), stdout);
	putchar('/');
	quotedzputs(cn->nam, stdout);
	putchar('\n');
    }
}

/***************************************/
/* Shell Function Hash Table Functions */
/***************************************/

/* hash table containing the shell functions */

/**/
HashTable shfunctab;

/**/
void
createshfunctable(void)
{
    shfunctab = newhashtable(7, "shfunctab", NULL);

    shfunctab->hash        = hasher;
    shfunctab->emptytable  = NULL;
    shfunctab->filltable   = NULL;
    shfunctab->addnode     = addhashnode;
    shfunctab->getnode     = gethashnode;
    shfunctab->getnode2    = gethashnode2;
    shfunctab->removenode  = removeshfuncnode;
    shfunctab->disablenode = disableshfuncnode;
    shfunctab->enablenode  = enableshfuncnode;
    shfunctab->freenode    = freeshfuncnode;
    shfunctab->printnode   = printshfuncnode;
}

/* Remove an entry from the shell function hash table.   *
 * It checks if the function is a signal trap and if so, *
 * it will disable the trapping of that signal.          */

/**/
static HashNode
removeshfuncnode(HashTable ht, char *nam)
{
    HashNode hn;

    if ((hn = removehashnode(shfunctab, nam))) {
	if (!strncmp(hn->nam, "TRAP", 4))
	    unsettrap(getsignum(hn->nam + 4));
	return hn;
    } else
	return NULL;
}

/* Disable an entry in the shell function hash table.    *
 * It checks if the function is a signal trap and if so, *
 * it will disable the trapping of that signal.          */

/**/
static void
disableshfuncnode(HashNode hn, int flags)
{
    hn->flags |= DISABLED;
    if (!strncmp(hn->nam, "TRAP", 4)) {
	int signum = getsignum(hn->nam + 4);
	sigtrapped[signum] &= ~ZSIG_FUNC;
	sigfuncs[signum] = NULL;
	unsettrap(signum);
    }
}

/* Re-enable an entry in the shell function hash table.  *
 * It checks if the function is a signal trap and if so, *
 * it will re-enable the trapping of that signal.        */

/**/
static void
enableshfuncnode(HashNode hn, int flags)
{
    Shfunc shf = (Shfunc) hn;
    int signum;

    shf->flags &= ~DISABLED;
    if (!strncmp(shf->nam, "TRAP", 4)) {
	signum = getsignum(shf->nam + 4);
	if (signum != -1) {
	    settrap(signum, shf->funcdef);
	    sigtrapped[signum] |= ZSIG_FUNC;
	}
    }
}

/**/
static void
freeshfuncnode(HashNode hn)
{
    Shfunc shf = (Shfunc) hn;

    zsfree(shf->nam);
    if (shf->funcdef)
	freestruct(shf->funcdef);
    zfree(shf, sizeof(struct shfunc));
}

/* Print a shell function */
 
/**/
static void
printshfuncnode(HashNode hn, int printflags)
{
    Shfunc f = (Shfunc) hn;
    char *t;
 
    if ((printflags & PRINT_NAMEONLY) ||
	((printflags & PRINT_WHENCE_SIMPLE) &&
	!(printflags & PRINT_WHENCE_FUNCDEF))) {
	zputs(f->nam, stdout);
	putchar('\n');
	return;
    }
 
    if ((printflags & (PRINT_WHENCE_VERBOSE|PRINT_WHENCE_WORD)) &&
	!(printflags & PRINT_WHENCE_FUNCDEF)) {
	nicezputs(f->nam, stdout);
	printf((printflags & PRINT_WHENCE_WORD) ? ": function\n" :
	       " is a shell function\n");
	return;
    }
 
    if (f->flags & PM_UNDEFINED)
	printf("undefined ");
    if (f->flags & PM_TAGGED)
	printf("traced ");
    if ((f->flags & PM_UNDEFINED) || !f->funcdef) {
	nicezputs(f->nam, stdout);
	printf(" () { }\n");
	return;
    }
 
    t = getpermtext((void *) dupstruct((void *) f->funcdef));
    quotedzputs(f->nam, stdout);
    printf(" () {\n\t");
    zputs(t, stdout);
    printf("\n}\n");
    zsfree(t);
}

/**************************************/
/* Reserved Word Hash Table Functions */
/**************************************/

/* Nodes for reserved word hash table */

static struct reswd reswds[] = {
    {NULL, "!", 0, BANG},
    {NULL, "[[", 0, DINBRACK},
    {NULL, "{", 0, INBRACE},
    {NULL, "}", 0, OUTBRACE},
    {NULL, "case", 0, CASE},
    {NULL, "coproc", 0, COPROC},
    {NULL, "do", 0, DO},
    {NULL, "done", 0, DONE},
    {NULL, "elif", 0, ELIF},
    {NULL, "else", 0, ELSE},
    {NULL, "end", 0, ZEND},
    {NULL, "esac", 0, ESAC},
    {NULL, "fi", 0, FI},
    {NULL, "for", 0, FOR},
    {NULL, "foreach", 0, FOREACH},
    {NULL, "function", 0, FUNC},
    {NULL, "if", 0, IF},
    {NULL, "nocorrect", 0, NOCORRECT},
    {NULL, "repeat", 0, REPEAT},
    {NULL, "select", 0, SELECT},
    {NULL, "then", 0, THEN},
    {NULL, "time", 0, TIME},
    {NULL, "until", 0, UNTIL},
    {NULL, "while", 0, WHILE},
    {NULL, NULL}
};

/* hash table containing the reserved words */

/**/
HashTable reswdtab;

/* Build the hash table containing zsh's reserved words. */

/**/
void
createreswdtable(void)
{
    Reswd rw;

    reswdtab = newhashtable(23, "reswdtab", NULL);

    reswdtab->hash        = hasher;
    reswdtab->emptytable  = NULL;
    reswdtab->filltable   = NULL;
    reswdtab->addnode     = addhashnode;
    reswdtab->getnode     = gethashnode;
    reswdtab->getnode2    = gethashnode2;
    reswdtab->removenode  = NULL;
    reswdtab->disablenode = disablehashnode;
    reswdtab->enablenode  = enablehashnode;
    reswdtab->freenode    = NULL;
    reswdtab->printnode   = printreswdnode;

    for (rw = reswds; rw->nam; rw++)
	reswdtab->addnode(reswdtab, rw->nam, rw);
}

/* Print a reserved word */

/**/
static void
printreswdnode(HashNode hn, int printflags)
{
    Reswd rw = (Reswd) hn;

    if (printflags & PRINT_WHENCE_WORD) {
	printf("%s: reserved\n", rw->nam);
	return;
    }

    if (printflags & PRINT_WHENCE_CSH) {
	printf("%s: shell reserved word\n", rw->nam);
	return;
    }

    if (printflags & PRINT_WHENCE_VERBOSE) {
	printf("%s is a reserved word\n", rw->nam);
	return;
    }

    /* default is name only */
    printf("%s\n", rw->nam);
}

/********************************/
/* Aliases Hash Table Functions */
/********************************/

/* hash table containing the aliases */
 
/**/
HashTable aliastab;
 
/* Create new hash table for aliases */

/**/
void
createaliastable(void)
{
    aliastab = newhashtable(23, "aliastab", NULL);

    aliastab->hash        = hasher;
    aliastab->emptytable  = NULL;
    aliastab->filltable   = NULL;
    aliastab->addnode     = addhashnode;
    aliastab->getnode     = gethashnode;
    aliastab->getnode2    = gethashnode2;
    aliastab->removenode  = removehashnode;
    aliastab->disablenode = disablehashnode;
    aliastab->enablenode  = enablehashnode;
    aliastab->freenode    = freealiasnode;
    aliastab->printnode   = printaliasnode;

    /* add the default aliases */
    aliastab->addnode(aliastab, ztrdup("run-help"), createaliasnode(ztrdup("man"), 0));
    aliastab->addnode(aliastab, ztrdup("which-command"), createaliasnode(ztrdup("whence"), 0));
}

/* Create a new alias node */

/**/
Alias
createaliasnode(char *txt, int flags)
{
    Alias al;

    al = (Alias) zcalloc(sizeof *al);
    al->flags = flags;
    al->text = txt;
    al->inuse = 0;
    return al;
}

/**/
static void
freealiasnode(HashNode hn)
{
    Alias al = (Alias) hn;
 
    zsfree(al->nam);
    zsfree(al->text);
    zfree(al, sizeof(struct alias));
}

/* Print an alias */

/**/
static void
printaliasnode(HashNode hn, int printflags)
{
    Alias a = (Alias) hn;

    if (printflags & PRINT_NAMEONLY) {
	zputs(a->nam, stdout);
	putchar('\n');
	return;
    }

    if (printflags & PRINT_WHENCE_WORD) {
	printf("%s: alias\n", a->nam);
	return;
    }

    if (printflags & PRINT_WHENCE_SIMPLE) {
	zputs(a->text, stdout);
	putchar('\n');
	return;
    }

    if (printflags & PRINT_WHENCE_CSH) {
	nicezputs(a->nam, stdout);
	if (a->flags & ALIAS_GLOBAL)
	    printf(": globally aliased to ");
	else
	    printf(": aliased to ");
	nicezputs(a->text, stdout);
	putchar('\n');
	return;
    }

    if (printflags & PRINT_WHENCE_VERBOSE) {
	nicezputs(a->nam, stdout);
	if (a->flags & ALIAS_GLOBAL)
	    printf(" is a global alias for ");
	else
	    printf(" is an alias for ");
	nicezputs(a->text, stdout);
	putchar('\n');
	return;
    }

    if (printflags & PRINT_LIST) {
	printf("alias ");
	if (a->flags & ALIAS_GLOBAL)
	    printf("-g ");

	/* If an alias begins with `-', then we must output `-- ' *
	 * first, so that it is not interpreted as an option.     */
	if(a->nam[0] == '-')
	    printf("-- ");
    }

    quotedzputs(a->nam, stdout);
    putchar('=');
    quotedzputs(a->text, stdout);
    putchar('\n');
}

/**********************************/
/* Parameter Hash Table Functions */
/**********************************/

/**/
void
freeparamnode(HashNode hn)
{
    Param pm = (Param) hn;
 
    zsfree(pm->nam);
    zfree(pm, sizeof(struct param));
}

/* Print a parameter */

/**/
void
printparamnode(HashNode hn, int printflags)
{
    Param p = (Param) hn;
    char *t, **u;

    if (p->flags & PM_UNSET)
	return;

    /* Print the attributes of the parameter */
    if (printflags & PRINT_TYPE) {
	if (p->flags & PM_INTEGER)
	    printf("integer ");
	if (p->flags & PM_ARRAY)
	    printf("array ");
	if (p->flags & PM_LEFT)
	    printf("left justified %d ", p->ct);
	if (p->flags & PM_RIGHT_B)
	    printf("right justified %d ", p->ct);
	if (p->flags & PM_RIGHT_Z)
	    printf("zero filled %d ", p->ct);
	if (p->flags & PM_LOWER)
	    printf("lowercase ");
	if (p->flags & PM_UPPER)
	    printf("uppercase ");
	if (p->flags & PM_READONLY)
	    printf("readonly ");
	if (p->flags & PM_TAGGED)
	    printf("tagged ");
	if (p->flags & PM_EXPORTED)
	    printf("exported ");
    }

    if (printflags & PRINT_NAMEONLY) {
	zputs(p->nam, stdout);
	putchar('\n');
	return;
    }

    /* How the value is displayed depends *
     * on the type of the parameter       */
    quotedzputs(p->nam, stdout);
    putchar('=');
    switch (PM_TYPE(p->flags)) {
    case PM_SCALAR:
	/* string: simple output */
	if (p->gets.cfn && (t = p->gets.cfn(p)))
	    quotedzputs(t, stdout);
	putchar('\n');
	break;
    case PM_INTEGER:
	/* integer */
	printf("%ld\n", p->gets.ifn(p));
	break;
    case PM_ARRAY:
	/* array */
	putchar('(');
	u = p->gets.afn(p);
	if(*u) {
	    quotedzputs(*u++, stdout);
	    while (*u) {
		putchar(' ');
		quotedzputs(*u++, stdout);
	    }
	}
	printf(")\n");
	break;
    }
}

/****************************************/
/* Named Directory Hash Table Functions */
/****************************************/

/* hash table containing named directories */

/**/
HashTable nameddirtab;
 
/* != 0 if all the usernames have already been *
 * added to the named directory hash table.    */

static int allusersadded;

/* Create new hash table for named directories */

/**/
void
createnameddirtable(void)
{
    nameddirtab = newhashtable(201, "nameddirtab", NULL);

    nameddirtab->hash        = hasher;
    nameddirtab->emptytable  = emptynameddirtable;
    nameddirtab->filltable   = fillnameddirtable;
    nameddirtab->addnode     = addnameddirnode;
    nameddirtab->getnode     = gethashnode;
    nameddirtab->getnode2    = gethashnode2;
    nameddirtab->removenode  = removenameddirnode;
    nameddirtab->disablenode = NULL;
    nameddirtab->enablenode  = NULL;
    nameddirtab->freenode    = freenameddirnode;
    nameddirtab->printnode   = printnameddirnode;

    allusersadded = 0;
    finddir(NULL);		/* clear the finddir cache */
}

/* Empty the named directories table */

/**/
static void
emptynameddirtable(HashTable ht)
{
    emptyhashtable(ht);
    allusersadded = 0;
    finddir(NULL);		/* clear the finddir cache */
}

/* Add all the usernames in the password file/database *
 * to the named directories table.                     */

/**/
static void
fillnameddirtable(HashTable ht)
{
#ifdef HAVE_GETPWENT
    if (!allusersadded) {
	struct passwd *pw;
 
	setpwent();
 
	/* loop through the password file/database *
	 * and add all entries returned.           */
	while ((pw = getpwent()) && !errflag)
	    adduserdir(ztrdup(pw->pw_name), pw->pw_dir, ND_USERNAME, 1);
 
	endpwent();
	allusersadded = 1;
    }
    return;
#endif /* HAVE_GETPWENT */
}

/* Add an entry to the named directory hash *
 * table, clearing the finddir() cache and  *
 * initialising the `diff' member.          */

/**/
static void
addnameddirnode(HashTable ht, char *nam, void *nodeptr)
{
    Nameddir nd = (Nameddir) nodeptr;

    nd->diff = strlen(nd->dir) - strlen(nam);
    finddir(NULL);		/* clear the finddir cache */
    addhashnode(ht, nam, nodeptr);
}

/* Remove an entry from the named directory  *
 * hash table, clearing the finddir() cache. */

/**/
static HashNode
removenameddirnode(HashTable ht, char *nam)
{
    HashNode hn = removehashnode(ht, nam);

    if(hn)
	finddir(NULL);		/* clear the finddir cache */
    return hn;
}

/* Free up the memory used by a named directory hash node. */

/**/
static void
freenameddirnode(HashNode hn)
{
    Nameddir nd = (Nameddir) hn;
 
    zsfree(nd->nam);
    zsfree(nd->dir);
    zfree(nd, sizeof(struct nameddir));
}

/* Print a named directory */

/**/
static void
printnameddirnode(HashNode hn, int printflags)
{
    Nameddir nd = (Nameddir) hn;

    if (printflags & PRINT_NAMEONLY) {
	zputs(nd->nam, stdout);
	putchar('\n');
	return;
    }

    quotedzputs(nd->nam, stdout);
    putchar('=');
    quotedzputs(nd->dir, stdout);
    putchar('\n');
}
