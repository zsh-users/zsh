/*
 * mapfile.c - associative array interface to external files
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 1999 Sven Wischnowsky, Peter Stephenson
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Sven Wischnowsky, Peter Stephenson or the Zsh Development
 * Group be liable to any party for direct, indirect, special, incidental, or
 * consequential damages arising out of the use of this software and its
 * documentation, even if Peter Stephenson, Sven Wischnowsky and the Zsh
 * Development Group have been advised of the possibility of such damage.
 *
 * Peter Stephenson, Sven Wischnowsky and the Zsh Development Group
 * specifically disclaim any warranties, including, but not limited to, the
 * implied warranties of merchantability and fitness for a particular purpose.
 * The softwareprovided hereunder is on an "as is" basis, and Peter
 * Stephenson, Sven Wischnowsky and the Zsh Development Group have no
 * obligation to provide maintenance, support, updates, enhancements, or
 * modifications. 
 *
 */

/*
 * To do:  worry about when keys of associative arrays get unmeta'd.
 */
#include "mapfile.mdh"
#include "mapfile.pro"

/*
 * Make sure we have all the bits I'm using for memory mapping, otherwise
 * I don't know what I'm doing.
 */
#if defined(HAVE_SYS_MMAN_H) && defined(HAVE_FTRUNCATE)
#if defined(HAVE_MMAP) && defined(HAVE_MUNMAP) && defined(HAVE_MSYNC)
#define USE_MMAP 1

#include <sys/mman.h>

#if !defined(MAP_VARIABLE)
#define MAP_VARIABLE 0
#endif
#if !defined(MAP_FILE)
#define MAP_FILE 0
#endif
#if !defined(MAP_NORESERVE)
#define MAP_NORESERVE 0
#endif
#define MMAP_ARGS (MAP_FILE | MAP_VARIABLE | MAP_SHARED | MAP_NORESERVE)

#endif /* HAVE_MMAP && HAVE_MUNMAP && HAVE_MSYNC */
#endif /* HAVE_SYS_MMAN_H &&  HAVE_FTRUNCATE */

/*
 * Name of the special parameter.  If zmodload took arguments,
 * we could make this selectable.
 */
static char mapfile_nam[] = "mapfile";

static Param mapfile_pm;

/* Empty dummy function for special hash parameters. */

/**/
static void
shempty(void)
{
}

static const struct gsu_hash mapfiles_gsu =
{ hashgetfn, setpmmapfiles, stdunsetfn };

/* Create the special hash parameter. */

/**/
static Param
createmapfilehash()
{
    Param pm;
    HashTable ht;

    unsetparam(mapfile_nam);
    mapfile_pm = NULL;

    if (!(pm = createparam(mapfile_nam, PM_SPECIAL|PM_HIDE|PM_HIDEVAL|
			   PM_REMOVABLE|PM_HASHED)))
	return NULL;

    pm->level = pm->old ? locallevel : 0;
    pm->gsu.h = &mapfiles_gsu;
    pm->u.hash = ht = newhashtable(7, mapfile_nam, NULL);

    ht->hash        = hasher;
    ht->emptytable  = (TableFunc) shempty;
    ht->filltable   = NULL;
    ht->addnode     = (AddNodeFunc) shempty;
    ht->getnode     = ht->getnode2 = getpmmapfile;
    ht->removenode  = (RemoveNodeFunc) shempty;
    ht->disablenode = NULL;
    ht->enablenode  = NULL;
    ht->freenode    = (FreeNodeFunc) shempty;
    ht->printnode   = printparamnode;
    ht->scantab     = scanpmmapfile;

    return (mapfile_pm = pm);
}

/* Functions for the options special parameter. */

/**/
static void
setpmmapfile(Param pm, char *value)
{
    int fd = -1, len;
    char *name = ztrdup(pm->node.nam);
#ifdef USE_MMAP
    caddr_t mmptr;
#else
    FILE *fout;
#endif

    /*
     * First unmetafy the value, and the name since we don't
     * where it's been.
     */
    unmetafy(name, &len);
    unmetafy(value, &len);

    /* Open the file for writing */
#ifdef USE_MMAP
    if (!(pm->node.flags & PM_READONLY) &&
	(fd = open(name, O_RDWR|O_CREAT|O_NOCTTY, 0666)) >= 0 &&
	(mmptr = (caddr_t)mmap((caddr_t)0, len, PROT_READ | PROT_WRITE,
			       MMAP_ARGS, fd, (off_t)0)) != (caddr_t)-1) {
	/*
	 * First we need to make sure the file is long enough for
	 * when we msync.  On AIX, at least, we just get zeroes otherwise.
	 */
	ftruncate(fd, len);
	memcpy(mmptr, value, len);
#ifndef MS_SYNC
#define MS_SYNC 0
#endif
	msync(mmptr, len, MS_SYNC);
	/*
	 * Then we need to truncate again, since mmap() always maps complete
	 * pages.  Honestly, I tried it without, and you need both.
	 */
	ftruncate(fd, len);
	munmap(mmptr, len);
    }
#else /* don't USE_MMAP */
    /* can't be bothered to do anything too clever here */
    if ((fout = fopen(name, "w"))) {
	while (len--)
	    putc(*value++, fout);
	fclose(fout);
    }
#endif /* USE_MMAP */
    if (fd >= 0)
	close(fd);
    free(name);
    free(value);
}

/**/
static void
unsetpmmapfile(Param pm, UNUSED(int exp))
{
    /* Unlink the file given by pm->nam */
    char *fname = ztrdup(pm->node.nam);
    int dummy;
    unmetafy(fname, &dummy);

    if (!(pm->node.flags & PM_READONLY))
	unlink(fname);

    free(fname);
}

/**/
static void
setpmmapfiles(Param pm, HashTable ht)
{
    int i;
    HashNode hn;

    /* just to see if I've understood what's happening */
    DPUTS(pm != mapfile_pm, "BUG: setpmmapfiles called for wrong param");

    if (!ht)
	return;

    if (!(pm->node.flags & PM_READONLY))
	for (i = 0; i < ht->hsize; i++)
	    for (hn = ht->nodes[i]; hn; hn = hn->next) {
		struct value v;

		v.isarr = v.inv = v.start = 0;
		v.end = -1;
		v.arr = NULL;
		v.pm = (Param) hn;

		setpmmapfile(v.pm, ztrdup(getstrvalue(&v)));
	    }
    deleteparamtable(ht);
}

/**/
static char *
get_contents(char *fname)
{
    int fd;
#ifdef USE_MMAP
    caddr_t mmptr;
    struct stat sbuf;
#endif
    char *val;
    unmetafy(fname = ztrdup(fname), &fd);

#ifdef USE_MMAP
    if ((fd = open(fname, O_RDONLY | O_NOCTTY)) < 0 ||
	fstat(fd, &sbuf) ||
	(mmptr = (caddr_t)mmap((caddr_t)0, sbuf.st_size, PROT_READ,
			       MMAP_ARGS, fd, (off_t)0)) == (caddr_t)-1) {
	if (fd >= 0)
	    close(fd);
	free(fname);
	return NULL;
    }

    /*
     * Sadly, we need to copy the thing even if metafying doesn't
     * change it.  We just don't know when we might get a chance to
     * munmap it, otherwise.
     */
    val = metafy((char *)mmptr, sbuf.st_size, META_HEAPDUP);

    munmap(mmptr, sbuf.st_size);
    close(fd);
#else /* don't USE_MMAP */
    val = NULL;
    if ((fd = open(fname, O_RDONLY | O_NOCTTY)) >= 0) {
	LinkList ll;

	if ((ll = readoutput(fd, 1)))
	    val = peekfirst(ll);
    }
#endif /* USE_MMAP */
    free(fname);
    return val;
}

static const struct gsu_scalar mapfile_gsu =
{ strgetfn, setpmmapfile, unsetpmmapfile };

/**/
static HashNode
getpmmapfile(UNUSED(HashTable ht), char *name)
{
    char *contents;
    Param pm = NULL;

    pm = (Param) hcalloc(sizeof(struct param));
    pm->node.nam = dupstring(name);
    pm->node.flags = PM_SCALAR;
    pm->gsu.s = &mapfile_gsu;
    pm->node.flags |= (mapfile_pm->node.flags & PM_READONLY);

    /* Set u.str to contents of file given by name */
    if ((contents = get_contents(pm->node.nam)))
	pm->u.str = contents;
    else {
	pm->u.str = "";
	pm->node.flags |= PM_UNSET;
    }
    return &pm->node;
}


/**/
static void
scanpmmapfile(UNUSED(HashTable ht), ScanFunc func, int flags)
{
    struct param pm;
    DIR *dir;

    if (!(dir = opendir(".")))
	return;

    memset((void *)&pm, 0, sizeof(struct param));
    pm.node.flags = PM_SCALAR;
    pm.gsu.s = &mapfile_gsu;
    pm.node.flags |= (mapfile_pm->node.flags & PM_READONLY);

    /* Here we scan the current directory, calling func() for each file */
    while ((pm.node.nam = zreaddir(dir, 1))) {
	/*
	 * Hmmm, it's rather wasteful always to read the contents.
	 * In fact, it's grotesequely wasteful, since that would mean
	 * we always read the entire contents of every single file
	 * in the directory into memory.  Hence just leave it empty.
	 */
	pm.node.nam = dupstring(pm.node.nam);
	pm.u.str = "";
	func(&pm.node, flags);
    }
    closedir(dir);
}

/**/
int
setup_(UNUSED(Module m))
{
    return 0;
}

/**/
int
boot_(UNUSED(Module m))
{
    /* Create the special associative array. */

    if (!createmapfilehash())
	return 1;

    return 0;
}

/**/
int
cleanup_(UNUSED(Module m))
{
    Param pm;

    /* Remove the special parameter if it is still the same. */

    if ((pm = (Param) paramtab->getnode(paramtab, mapfile_nam)) &&
	pm == mapfile_pm) {
	pm->node.flags &= ~PM_READONLY;
	unsetparam_pm(pm, 0, 1);
    }
    return 0;
}

/**/
int
finish_(UNUSED(Module m))
{
    return 0;
}
