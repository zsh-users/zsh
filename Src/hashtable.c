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
mod_export unsigned
hasher(const char *str)
{
    unsigned hashval = 0, c;

    while ((c = *((unsigned char *) str++)))
	hashval += (hashval << 5) + c;

    return hashval;
}

/* Get a new hash table */

/**/
mod_export HashTable
newhashtable(int size, UNUSED(char const *name), UNUSED(PrintTableStats printinfo))
{
    HashTable ht;

    ht = (HashTable) zshcalloc(sizeof *ht);
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
    ht->nodes = (HashNode *) zshcalloc(size * sizeof(HashNode));
    ht->hsize = size;
    ht->ct = 0;
    ht->scan = NULL;
    ht->scantab = NULL;
    return ht;
}

/* Delete a hash table.  After this function has been used, any *
 * existing pointers to the hash table are invalid.             */

/**/
mod_export void
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
    zsfree(ht->tablename);
#endif /* ZSH_HASH_DEBUG */
    zfree(ht->nodes, ht->hsize * sizeof(HashNode));
    zfree(ht, sizeof(*ht));
}

/* Add a node to a hash table.                          *
 * nam is the key to use in hashing.  nodeptr points    *
 * to the node to add.  If there is already a node in   *
 * the table with the same key, it is first freed, and  *
 * then the new node is added.  If the number of nodes  *
 * is now greater than twice the number of hash values, *
 * the table is then expanded.                          */

/**/
mod_export void
addhashnode(HashTable ht, char *nam, void *nodeptr)
{
    HashNode oldnode = addhashnode2(ht, nam, nodeptr);
    if (oldnode)
	ht->freenode(oldnode);
}

/* Add a node to a hash table, returning the old node on replacement. */

/**/
HashNode
addhashnode2(HashTable ht, char *nam, void *nodeptr)
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
	return NULL;
    }

    /* else check if the first node contains the same key */
    if (ht->cmpnodes(hp->nam, hn->nam) == 0) {
	ht->nodes[hashval] = hn;
	replacing:
	hn->next = hp->next;
	if(ht->scan) {
	    if(ht->scan->sorted) {
		HashNode *tab = ht->scan->u.s.tab;
		int i;
		for(i = ht->scan->u.s.ct; i--; )
		    if(tab[i] == hp)
			tab[i] = hn;
	    } else if(ht->scan->u.u == hp)
		ht->scan->u.u = hn;
	}
	return hp;
    }

    /* else run through the list and check all the keys */
    hq = hp;
    hp = hp->next;
    for (; hp; hq = hp, hp = hp->next) {
	if (ht->cmpnodes(hp->nam, hn->nam) == 0) {
	    hq->next = hn;
	    goto replacing;
	}
    }

    /* else just add it at the front of the list */
    hn->next = ht->nodes[hashval];
    ht->nodes[hashval] = hn;
    if (++ht->ct >= ht->hsize * 2 && !ht->scan)
        expandhashtable(ht);
    return NULL;
}

/* Get an enabled entry in a hash table.  *
 * If successful, it returns a pointer to *
 * the hashnode.  If the node is DISABLED *
 * or isn't found, it returns NULL        */

/**/
mod_export HashNode
gethashnode(HashTable ht, const char *nam)
{
    unsigned hashval;
    HashNode hp;

    hashval = ht->hash(nam) % ht->hsize;
    for (hp = ht->nodes[hashval]; hp; hp = hp->next) {
	if (ht->cmpnodes(hp->nam, nam) == 0) {
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
mod_export HashNode
gethashnode2(HashTable ht, const char *nam)
{
    unsigned hashval;
    HashNode hp;

    hashval = ht->hash(nam) % ht->hsize;
    for (hp = ht->nodes[hashval]; hp; hp = hp->next) {
	if (ht->cmpnodes(hp->nam, nam) == 0)
	    return hp;
    }
    return NULL;
}

/* Remove an entry from a hash table.           *
 * If successful, it removes the node from the  *
 * table and returns a pointer to it.  If there *
 * is no such node, then it returns NULL        */

/**/
mod_export HashNode
removehashnode(HashTable ht, const char *nam)
{
    unsigned hashval;
    HashNode hp, hq;

    hashval = ht->hash(nam) % ht->hsize;
    hp = ht->nodes[hashval];

    /* if no nodes at this hash value, return NULL */
    if (!hp)
	return NULL;

    /* else check if the key in the first one matches */
    if (ht->cmpnodes(hp->nam, nam) == 0) {
	ht->nodes[hashval] = hp->next;
	gotit:
	ht->ct--;
	if(ht->scan) {
	    if(ht->scan->sorted) {
		HashNode *tab = ht->scan->u.s.tab;
		int i;
		for(i = ht->scan->u.s.ct; i--; )
		    if(tab[i] == hp)
			tab[i] = NULL;
	    } else if(ht->scan->u.u == hp)
		ht->scan->u.u = hp->next;
	}
	return hp;
    }

    /* else run through the list and check the rest of the keys */
    hq = hp;
    hp = hp->next;
    for (; hp; hq = hp, hp = hp->next) {
	if (ht->cmpnodes(hp->nam, nam) == 0) {
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
disablehashnode(HashNode hn, UNUSED(int flags))
{
    hn->flags |= DISABLED;
}

/* Enable a node in a hash table */

/**/
void
enablehashnode(HashNode hn, UNUSED(int flags))
{
    hn->flags &= ~DISABLED;
}

/* Compare two hash table entries by name */

/**/
static int
hnamcmp(const void *ap, const void *bp)
{
    HashNode a = *(HashNode *)ap;
    HashNode b = *(HashNode *)bp;
    return ztrcmp(a->nam, b->nam);
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
 *
 * pprog, if non-NULL, is a pattern that must match the name
 * of the node.
 *
 * The function returns the number of matches, as reduced by pprog, flags1
 * and flags2.
 */

/**/
mod_export int
scanmatchtable(HashTable ht, Patprog pprog, int sorted,
	       int flags1, int flags2, ScanFunc scanfunc, int scanflags)
{
    int match = 0;
    struct scanstatus st;

    /*
     * scantab is currently only used by modules to scan
     * tables where the contents are generated on the fly from
     * other objects.  Note the fact that in this case pprog,
     * sorted, flags1 and flags2 are ignore.
     */
    if (!pprog && ht->scantab) {
	ht->scantab(ht, scanfunc, scanflags);
	return ht->ct;
    }
    if (sorted) {
	int i, ct = ht->ct;
	VARARR(HashNode, hnsorttab, ct);
	HashNode *htp, hn;

	/*
	 * Because the structure might change under our feet,
	 * we can't apply the flags and the pattern before sorting,
	 * tempting though that is.
	 */
	for (htp = hnsorttab, i = 0; i < ht->hsize; i++)
	    for (hn = ht->nodes[i]; hn; hn = hn->next)
		*htp++ = hn;
	qsort((void *)hnsorttab, ct, sizeof(HashNode), hnamcmp);

	st.sorted = 1;
	st.u.s.tab = hnsorttab;
	st.u.s.ct = ct;
	ht->scan = &st;

	for (htp = hnsorttab, i = 0; i < ct; i++, htp++) {
	    if ((!flags1 || ((*htp)->flags & flags1)) &&
		!((*htp)->flags & flags2) &&
		(!pprog || pattry(pprog, (*htp)->nam))) {
		match++;
		scanfunc(*htp, scanflags);
	    }
	}

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
		if ((!flags1 || (hn->flags & flags1)) && !(hn->flags & flags2)
		    && (!pprog || pattry(pprog, hn->nam))) {
		    match++;
		    scanfunc(hn, scanflags);
		}
	    }

	ht->scan = NULL;
    }

    return match;
}


/**/
mod_export int
scanhashtable(HashTable ht, int sorted, int flags1, int flags2,
	      ScanFunc scanfunc, int scanflags)
{
    return scanmatchtable(ht, NULL, sorted, flags1, flags2,
			  scanfunc, scanflags);
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
    ht->nodes = (HashNode *) zshcalloc(ht->hsize * sizeof(HashNode));
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
	ht->nodes = (HashNode *) zshcalloc(newsize * sizeof(HashNode));
	ht->hsize = newsize;
    } else {
	/* else we just re-zero the current nodes array */
	memset(ht->nodes, 0, newsize * sizeof(HashNode));
    }

    ht->ct = 0;
}

/* Generic method to empty a hash table */

/**/
mod_export void
emptyhashtable(HashTable ht)
{
    resizehashtable(ht, ht->hsize);
}

/**/
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
bin_hashinfo(char *nam, char **args, Options ops, int func)
{
    HashTable ht;

    printf("----------------------------------------------------\n");
    queue_signals();
    for(ht = firstht; ht; ht = ht->next) {
	ht->printinfo(ht);
	printf("----------------------------------------------------\n");
    }
    unqueue_signals();
    return 0;
}

/**/
#endif /* ZSH_HASH_DEBUG */

/********************************/
/* Command Hash Table Functions */
/********************************/

/* hash table containing external commands */
 
/**/
mod_export HashTable cmdnamtab;
 
/* how far we've hashed the PATH so far */
 
/**/
mod_export char **pathchecked;

/* Create a new command hash table */
 
/**/
void
createcmdnamtable(void)
{
    cmdnamtab = newhashtable(201, "cmdnamtab", NULL);

    cmdnamtab->hash        = hasher;
    cmdnamtab->emptytable  = emptycmdnamtable;
    cmdnamtab->filltable   = fillcmdnamtable;
    cmdnamtab->cmpnodes    = strcmp;
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
    char *fn, *unmetadir, *pathbuf, *pathptr;
    int dirlen;
#if defined(_WIN32) || defined(__CYGWIN__)
    char *exe;
#endif /* _WIN32 || _CYGWIN__ */

    if (isrelative(*dirp))
	return;
    unmetadir = unmeta(*dirp);
    if (!(dir = opendir(unmetadir)))
	return;

    dirlen = strlen(unmetadir);
    pathbuf = (char *)zalloc(dirlen + PATH_MAX + 2);
    sprintf(pathbuf, "%s/", unmetadir);
    pathptr = pathbuf + dirlen + 1;

    while ((fn = zreaddir(dir, 1))) {
	if (!cmdnamtab->getnode(cmdnamtab, fn)) {
	    char *fname = ztrdup(fn);
	    struct stat statbuf;
	    int add = 0, dummylen;

	    unmetafy(fn, &dummylen);
	    if (strlen(fn) > PATH_MAX) {
		/* Too heavy to do all the allocation */
		add = 1;
	    } else {
		strcpy(pathptr, fn);
		/*
		 * This is the same test as for the glob qualifier for
		 * executable plain files.
		 */
		if (stat(pathbuf, &statbuf) == 0 &&
		    S_ISREG(statbuf.st_mode) && (statbuf.st_mode & S_IXUGO))
		    add = 1;
	    }
	    if (add) {
		cn = (Cmdnam) zshcalloc(sizeof *cn);
		cn->node.flags = 0;
		cn->u.name = dirp;
		cmdnamtab->addnode(cmdnamtab, fname, cn);
	    } else
		zsfree(fname);
	}
#if defined(_WIN32) || defined(__CYGWIN__)
	/* Hash foo.exe as foo, since when no real foo exists, foo.exe
	   will get executed by DOS automatically.  This quiets
	   spurious corrections when CORRECT or CORRECT_ALL is set. */
	if ((exe = strrchr(fn, '.')) &&
	    (exe[1] == 'E' || exe[1] == 'e') &&
	    (exe[2] == 'X' || exe[2] == 'x') &&
	    (exe[3] == 'E' || exe[3] == 'e') && exe[4] == 0) {
	    *exe = 0;
	    if (!cmdnamtab->getnode(cmdnamtab, fn)) {
		cn = (Cmdnam) zshcalloc(sizeof *cn);
		cn->node.flags = 0;
		cn->u.name = dirp;
		cmdnamtab->addnode(cmdnamtab, ztrdup(fn), cn);
	    }
	}
#endif /* _WIN32 || __CYGWIN__ */
    }
    closedir(dir);
    zfree(pathbuf, dirlen + PATH_MAX + 2);
}

/* Go through user's PATH and add everything to *
 * the command hashtable.                       */

/**/
static void
fillcmdnamtable(UNUSED(HashTable ht))
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
 
    zsfree(cn->node.nam);
    if (cn->node.flags & HASHED)
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
	printf("%s: %s\n", cn->node.nam, (cn->node.flags & HASHED) ? 
	       "hashed" : "command");
	return;
    }

    if ((printflags & PRINT_WHENCE_CSH) || (printflags & PRINT_WHENCE_SIMPLE)) {
	if (cn->node.flags & HASHED) {
	    zputs(cn->u.cmd, stdout);
	    putchar('\n');
	} else {
	    zputs(*(cn->u.name), stdout);
	    putchar('/');
	    zputs(cn->node.nam, stdout);
	    putchar('\n');
	}
	return;
    }

    if (printflags & PRINT_WHENCE_VERBOSE) {
	if (cn->node.flags & HASHED) {
	    nicezputs(cn->node.nam, stdout);
	    printf(" is hashed to ");
	    nicezputs(cn->u.cmd, stdout);
	    putchar('\n');
	} else {
	    nicezputs(cn->node.nam, stdout);
	    printf(" is ");
	    nicezputs(*(cn->u.name), stdout);
	    putchar('/');
	    nicezputs(cn->node.nam, stdout);
	    putchar('\n');
	}
	return;
    }

    if (printflags & PRINT_LIST) {
	printf("hash ");

	if(cn->node.nam[0] == '-')
	    printf("-- ");
    }

    if (cn->node.flags & HASHED) {
	quotedzputs(cn->node.nam, stdout);
	putchar('=');
	quotedzputs(cn->u.cmd, stdout);
	putchar('\n');
    } else {
	quotedzputs(cn->node.nam, stdout);
	putchar('=');
	quotedzputs(*(cn->u.name), stdout);
	putchar('/');
	quotedzputs(cn->node.nam, stdout);
	putchar('\n');
    }
}

/***************************************/
/* Shell Function Hash Table Functions */
/***************************************/

/* hash table containing the shell functions */

/**/
mod_export HashTable shfunctab;

/**/
void
createshfunctable(void)
{
    shfunctab = newhashtable(7, "shfunctab", NULL);

    shfunctab->hash        = hasher;
    shfunctab->emptytable  = NULL;
    shfunctab->filltable   = NULL;
    shfunctab->cmpnodes    = strcmp;
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
removeshfuncnode(UNUSED(HashTable ht), const char *nam)
{
    HashNode hn;
    int signum;

    if (!strncmp(nam, "TRAP", 4) && (signum = getsignum(nam + 4)) != -1)
	hn = removetrap(signum);
    else
	hn = removehashnode(shfunctab, nam);

    return hn;
}

/* Disable an entry in the shell function hash table.    *
 * It checks if the function is a signal trap and if so, *
 * it will disable the trapping of that signal.          */

/**/
static void
disableshfuncnode(HashNode hn, UNUSED(int flags))
{
    hn->flags |= DISABLED;
    if (!strncmp(hn->nam, "TRAP", 4)) {
	int signum = getsignum(hn->nam + 4);
	if (signum != -1) {
	    sigtrapped[signum] &= ~ZSIG_FUNC;
	    unsettrap(signum);
	}
    }
}

/* Re-enable an entry in the shell function hash table.  *
 * It checks if the function is a signal trap and if so, *
 * it will re-enable the trapping of that signal.        */

/**/
static void
enableshfuncnode(HashNode hn, UNUSED(int flags))
{
    Shfunc shf = (Shfunc) hn;

    shf->node.flags &= ~DISABLED;
    if (!strncmp(shf->node.nam, "TRAP", 4)) {
	int signum = getsignum(shf->node.nam + 4);
	if (signum != -1) {
	    settrap(signum, NULL, ZSIG_FUNC);
	}
    }
}

/**/
static void
freeshfuncnode(HashNode hn)
{
    Shfunc shf = (Shfunc) hn;

    zsfree(shf->node.nam);
    if (shf->funcdef)
	freeeprog(shf->funcdef);
    zsfree(shf->filename);
    zfree(shf, sizeof(struct shfunc));
}

/* Print a shell function */
 
/**/
static void
printshfuncnode(HashNode hn, int printflags)
{
    Shfunc f = (Shfunc) hn;
    char *t = 0;
 
    if ((printflags & PRINT_NAMEONLY) ||
	((printflags & PRINT_WHENCE_SIMPLE) &&
	!(printflags & PRINT_WHENCE_FUNCDEF))) {
	zputs(f->node.nam, stdout);
	putchar('\n');
	return;
    }
 
    if ((printflags & (PRINT_WHENCE_VERBOSE|PRINT_WHENCE_WORD)) &&
	!(printflags & PRINT_WHENCE_FUNCDEF)) {
	nicezputs(f->node.nam, stdout);
	printf((printflags & PRINT_WHENCE_WORD) ? ": function\n" :
	       " is a shell function\n");
	return;
    }
 
    quotedzputs(f->node.nam, stdout);
    if (f->funcdef || f->node.flags & PM_UNDEFINED) {
	printf(" () {\n\t");
	if (f->node.flags & PM_UNDEFINED)
	    printf("%c undefined\n\t", hashchar);
	else
	    t = getpermtext(f->funcdef, NULL, 1);
	if (f->node.flags & PM_TAGGED)
	    printf("%c traced\n\t", hashchar);
	if (!t) {
	    char *fopt = "Utkz";
	    int flgs[] = {
		PM_UNALIASED, PM_TAGGED, PM_KSHSTORED, PM_ZSHSTORED, 0
	    };
	    int fl;;

	    zputs("builtin autoload -X", stdout);
	    for (fl=0;fopt[fl];fl++)
		if (f->node.flags & flgs[fl]) putchar(fopt[fl]);
	} else {
	    zputs(t, stdout);
	    zsfree(t);
	    if (f->funcdef->flags & EF_RUN) {
		printf("\n\t");
		quotedzputs(f->node.nam, stdout);
		printf(" \"$@\"");
	    }
	}   
	printf("\n}\n");
    } else {
	printf(" () { }\n");
    }
}

/**************************************/
/* Reserved Word Hash Table Functions */
/**************************************/

/* Nodes for reserved word hash table */

static struct reswd reswds[] = {
    {{NULL, "!", 0}, BANG},
    {{NULL, "[[", 0}, DINBRACK},
    {{NULL, "{", 0}, INBRACE},
    {{NULL, "}", 0}, OUTBRACE},
    {{NULL, "case", 0}, CASE},
    {{NULL, "coproc", 0}, COPROC},
    {{NULL, "do", 0}, DOLOOP},
    {{NULL, "done", 0}, DONE},
    {{NULL, "elif", 0}, ELIF},
    {{NULL, "else", 0}, ELSE},
    {{NULL, "end", 0}, ZEND},
    {{NULL, "esac", 0}, ESAC},
    {{NULL, "fi", 0}, FI},
    {{NULL, "for", 0}, FOR},
    {{NULL, "foreach", 0}, FOREACH},
    {{NULL, "function", 0}, FUNC},
    {{NULL, "if", 0}, IF},
    {{NULL, "nocorrect", 0}, NOCORRECT},
    {{NULL, "repeat", 0}, REPEAT},
    {{NULL, "select", 0}, SELECT},
    {{NULL, "then", 0}, THEN},
    {{NULL, "time", 0}, TIME},
    {{NULL, "until", 0}, UNTIL},
    {{NULL, "while", 0}, WHILE},
    {{NULL, NULL, 0}, 0}
};

/* hash table containing the reserved words */

/**/
mod_export HashTable reswdtab;

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
    reswdtab->cmpnodes    = strcmp;
    reswdtab->addnode     = addhashnode;
    reswdtab->getnode     = gethashnode;
    reswdtab->getnode2    = gethashnode2;
    reswdtab->removenode  = NULL;
    reswdtab->disablenode = disablehashnode;
    reswdtab->enablenode  = enablehashnode;
    reswdtab->freenode    = NULL;
    reswdtab->printnode   = printreswdnode;

    for (rw = reswds; rw->node.nam; rw++)
	reswdtab->addnode(reswdtab, rw->node.nam, rw);
}

/* Print a reserved word */

/**/
static void
printreswdnode(HashNode hn, int printflags)
{
    Reswd rw = (Reswd) hn;

    if (printflags & PRINT_WHENCE_WORD) {
	printf("%s: reserved\n", rw->node.nam);
	return;
    }

    if (printflags & PRINT_WHENCE_CSH) {
	printf("%s: shell reserved word\n", rw->node.nam);
	return;
    }

    if (printflags & PRINT_WHENCE_VERBOSE) {
	printf("%s is a reserved word\n", rw->node.nam);
	return;
    }

    /* default is name only */
    printf("%s\n", rw->node.nam);
}

/********************************/
/* Aliases Hash Table Functions */
/********************************/

/* hash table containing the aliases */
 
/**/
mod_export HashTable aliastab;
 
/* has table containing suffix aliases */

/**/
mod_export HashTable sufaliastab;
 
/* Create new hash tables for aliases */

/**/
void
createaliastable(HashTable ht)
{
    ht->hash        = hasher;
    ht->emptytable  = NULL;
    ht->filltable   = NULL;
    ht->cmpnodes    = strcmp;
    ht->addnode     = addhashnode;
    ht->getnode     = gethashnode;
    ht->getnode2    = gethashnode2;
    ht->removenode  = removehashnode;
    ht->disablenode = disablehashnode;
    ht->enablenode  = enablehashnode;
    ht->freenode    = freealiasnode;
    ht->printnode   = printaliasnode;
}

/**/
void
createaliastables(void)
{
    /* Table for regular and global aliases */

    aliastab = newhashtable(23, "aliastab", NULL);

    createaliastable(aliastab);

    /* add the default aliases */
    aliastab->addnode(aliastab, ztrdup("run-help"), createaliasnode(ztrdup("man"), 0));
    aliastab->addnode(aliastab, ztrdup("which-command"), createaliasnode(ztrdup("whence"), 0));


    /* Table for suffix aliases --- make this smaller */

    sufaliastab = newhashtable(11, "sufaliastab", NULL);

    createaliastable(sufaliastab);
}

/* Create a new alias node */

/**/
mod_export Alias
createaliasnode(char *txt, int flags)
{
    Alias al;

    al = (Alias) zshcalloc(sizeof *al);
    al->node.flags = flags;
    al->text = txt;
    al->inuse = 0;
    return al;
}

/**/
static void
freealiasnode(HashNode hn)
{
    Alias al = (Alias) hn;
 
    zsfree(al->node.nam);
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
	zputs(a->node.nam, stdout);
	putchar('\n');
	return;
    }

    if (printflags & PRINT_WHENCE_WORD) {
	printf("%s: alias\n", a->node.nam);
	return;
    }

    if (printflags & PRINT_WHENCE_SIMPLE) {
	zputs(a->text, stdout);
	putchar('\n');
	return;
    }

    if (printflags & PRINT_WHENCE_CSH) {
	nicezputs(a->node.nam, stdout);
	printf(": ");
	if (a->node.flags & ALIAS_SUFFIX)
	    printf("suffix ");
	else if (a->node.flags & ALIAS_GLOBAL)
	    printf("globally ");
	printf ("aliased to ");
	nicezputs(a->text, stdout);
	putchar('\n');
	return;
    }

    if (printflags & PRINT_WHENCE_VERBOSE) {
	nicezputs(a->node.nam, stdout);
	printf(" is a");
	if (a->node.flags & ALIAS_SUFFIX)
	    printf(" suffix");
	else if (a->node.flags & ALIAS_GLOBAL)
	    printf(" global");
	else
	    printf("n");
	printf(" alias for ");
	nicezputs(a->text, stdout);
	putchar('\n');
	return;
    }

    if (printflags & PRINT_LIST) {
	printf("alias ");
	if (a->node.flags & ALIAS_SUFFIX)
	    printf("-s ");
	else if (a->node.flags & ALIAS_GLOBAL)
	    printf("-g ");

	/* If an alias begins with `-', then we must output `-- ' *
	 * first, so that it is not interpreted as an option.     */
	if(a->node.nam[0] == '-')
	    printf("-- ");
    }

    quotedzputs(a->node.nam, stdout);
    putchar('=');
    quotedzputs(a->text, stdout);

    putchar('\n');
}

/****************************************/
/* Named Directory Hash Table Functions */
/****************************************/

#ifdef HAVE_NIS_PLUS
# include <rpcsvc/nis.h>
#else
# ifdef HAVE_NIS
#  include	<rpc/types.h>
#  include	<rpc/rpc.h>
#  include	<rpcsvc/ypclnt.h>
#  include	<rpcsvc/yp_prot.h>
# endif
#endif

/* hash table containing named directories */

/**/
mod_export HashTable nameddirtab;
 
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
    nameddirtab->cmpnodes    = strcmp;
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

#ifdef HAVE_NIS_PLUS
static int
add_userdir(nis_name table, nis_object *object, void *userdata)
{
    if (object->zo_data.objdata_u.en_data.en_cols.en_cols_len >= 6) {
	static char name[40], dir[PATH_MAX + 1];
	register entry_col *ec =
	    object->zo_data.objdata_u.en_data.en_cols.en_cols_val;
	register int nl = minimum(ec[0].ec_value.ec_value_len, 39);
	register int dl = minimum(ec[5].ec_value.ec_value_len, PATH_MAX);

	memcpy(name, ec[0].ec_value.ec_value_val, nl);
	name[nl] = '\0';
	memcpy(dir, ec[5].ec_value.ec_value_val, dl);
	dir[dl] = '\0';

	adduserdir(name, dir, ND_USERNAME, 1);
    }
    return 0;
}
#else
# ifdef HAVE_NIS
static int
add_userdir(int status, char *key, int keylen, char *val, int vallen, char *dummy)
{
    char *p, *d, *de;

    if (status != YP_TRUE)
	return 1;

    if (vallen > keylen && *(p = val + keylen) == ':') {
	*p++ = '\0';
	for (de = val + vallen - 1; *de != ':' && de > val; de--);
	if (de > val) {
	    *de = '\0';
	    if ((d = strrchr(p, ':'))) {
		if (*++d && val[0])
		    adduserdir(val, d, ND_USERNAME, 1);
	    }
	}
    }
    return 0;
}
# endif /* HAVE_NIS */
#endif  /* HAVE_NIS_PLUS */

/**/
static void
fillnameddirtable(UNUSED(HashTable ht))
{
    if (!allusersadded) {
#if defined(HAVE_NIS) || defined(HAVE_NIS_PLUS)
	FILE *pwf;
	char buf[BUFSIZ], *p, *d, *de;
	int skipping, oldct = nameddirtab->ct, usepwf = 1;

# ifndef HAVE_NIS_PLUS
	char domain[YPMAXDOMAIN];
	struct ypall_callback cb;

	/* Get potential matches from NIS and cull those without local accounts */
	if (getdomainname(domain, YPMAXDOMAIN) == 0) {
	    cb.foreach = (int (*)()) add_userdir;
	    cb.data = NULL;
	    yp_all(domain, PASSWD_MAP, &cb);
    }
# else  /* HAVE_NIS_PLUS */
	/* Maybe we should turn this string into a #define'd constant...? */

	nis_list("passwd.org_dir", EXPAND_NAME|ALL_RESULTS|FOLLOW_LINKS|FOLLOW_PATH,
		 add_userdir, 0);
# endif
	if (nameddirtab->ct == oldct) {
	    /* Using NIS or NIS+ didn't add any user directories. This seems
	     * fishy, so we fall back to using getpwent(). If we don't have
	     * that, we only use the passwd file. */
#ifdef HAVE_GETPWENT
	    struct passwd *pw;
 
	    setpwent();
 
	    /* loop through the password file/database *
	     * and add all entries returned.           */
	    while ((pw = getpwent()) && !errflag)
		adduserdir(pw->pw_name, pw->pw_dir, ND_USERNAME, 1);
 
	    endpwent();
	    usepwf = 0;
#endif /* HAVE_GETPWENT */
	}
	if (usepwf) {
	    /* Don't forget the non-NIS matches from the flat passwd file */
	    if ((pwf = fopen(PASSWD_FILE, "r")) != NULL) {
		skipping = 0;
		while (fgets(buf, BUFSIZ, pwf) != NULL) {
		    if (strchr(buf, '\n') != NULL) {
			if (!skipping) {
			    if ((p = strchr(buf, ':')) != NULL) {
				*p++ = '\0';
				if ((de = strrchr(p, ':'))) {
				    *de = '\0';
				    if ((d = strrchr(p, ':'))) {
					if (*++d && buf[0])
					    adduserdir(buf, d, ND_USERNAME, 1);
				    }
				}
			    }
			} else
			    skipping = 0;
		    } else
			skipping = 1;
		}
		fclose(pwf);
	    }
	}
#else  /* no NIS or NIS_PLUS */
#ifdef USE_GETPWENT
	struct passwd *pw;
 
	setpwent();
 
	/* loop through the password file/database *
	 * and add all entries returned.           */
	while ((pw = getpwent()) && !errflag)
	    adduserdir(pw->pw_name, pw->pw_dir, ND_USERNAME, 1);
 
	endpwent();
#endif /* HAVE_GETPWENT */
#endif
	allusersadded = 1;
    }
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
removenameddirnode(HashTable ht, const char *nam)
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
 
    zsfree(nd->node.nam);
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
	zputs(nd->node.nam, stdout);
	putchar('\n');
	return;
    }
    
    if (printflags & PRINT_LIST) {
      printf("hash -d ");

      if(nd->node.nam[0] == '-')
	    printf("-- ");
    }

    quotedzputs(nd->node.nam, stdout);
    putchar('=');
    quotedzputs(nd->dir, stdout);
    putchar('\n');
}

/*************************************/
/* History Line Hash Table Functions */
/*************************************/

/**/
void
createhisttable(void)
{
    histtab = newhashtable(599, "histtab", NULL);

    histtab->hash        = histhasher;
    histtab->emptytable  = emptyhisttable;
    histtab->filltable   = NULL;
    histtab->cmpnodes    = histstrcmp;
    histtab->addnode     = addhistnode;
    histtab->getnode     = gethashnode2;
    histtab->getnode2    = gethashnode2;
    histtab->removenode  = removehashnode;
    histtab->disablenode = NULL;
    histtab->enablenode  = NULL;
    histtab->freenode    = freehistnode;
    histtab->printnode   = NULL;
}

/**/
unsigned
histhasher(const char *str)
{
    unsigned hashval = 0;

    while (inblank(*str)) str++;

    while (*str) {
	if (inblank(*str)) {
	    do str++; while (inblank(*str));
	    if (*str)
		hashval += (hashval << 5) + ' ';
	}
	else
	    hashval += (hashval << 5) + *(unsigned char *)str++;
    }
    return hashval;
}

/**/
void
emptyhisttable(HashTable ht)
{
    emptyhashtable(ht);
    if (hist_ring)
	histremovedups();
}

/* Compare two strings with normalized white-space */

/**/
int
histstrcmp(const char *str1, const char *str2)
{
    while (inblank(*str1)) str1++;
    while (inblank(*str2)) str2++;
    while (*str1 && *str2) {
	if (inblank(*str1)) {
	    if (!inblank(*str2))
		break;
	    do str1++; while (inblank(*str1));
	    do str2++; while (inblank(*str2));
	}
	else {
	    if (*str1 != *str2)
		break;
	    str1++;
	    str2++;
	}
    }
    return *str1 - *str2;
}

/**/
void
addhistnode(HashTable ht, char *nam, void *nodeptr)
{
    HashNode oldnode = addhashnode2(ht, nam, nodeptr);
    Histent he = (Histent)nodeptr;
    if (oldnode && oldnode != (HashNode)nodeptr) {
	if (he->node.flags & HIST_MAKEUNIQUE
	 || (he->node.flags & HIST_FOREIGN && (Histent)oldnode == he->up)) {
	    (void) addhashnode2(ht, oldnode->nam, oldnode); /* restore hash */
	    he->node.flags |= HIST_DUP;
	    he->node.flags &= ~HIST_MAKEUNIQUE;
	}
	else {
	    oldnode->flags |= HIST_DUP;
	    if (hist_ignore_all_dups)
		freehistnode(oldnode); /* Remove the old dup */
	}
    }
    else
	he->node.flags &= ~HIST_MAKEUNIQUE;
}

/**/
void
freehistnode(HashNode nodeptr)
{
    freehistdata((Histent)nodeptr, 1);
    zfree(nodeptr, sizeof (struct histent));
}

/**/
void
freehistdata(Histent he, int unlink)
{
    if (!he)
	return;

    if (!(he->node.flags & (HIST_DUP | HIST_TMPSTORE)))
	removehashnode(histtab, he->node.nam);

    zsfree(he->node.nam);
    if (he->nwords)
	zfree(he->words, he->nwords*2*sizeof(short));

    if (unlink) {
	if (!--histlinect)
	    hist_ring = NULL;
	else {
	    if (he == hist_ring)
		hist_ring = hist_ring->up;
	    he->up->down = he->down;
	    he->down->up = he->up;
	}
    }
}
