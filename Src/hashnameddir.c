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

/*
 * On Solaris 8 there's a clash between "bool" in curses and RPC.
 * We don't need curses here, so ensure it doesn't get included.
 */
#define ZSH_NO_TERM_HANDLING

#include "zsh.mdh"
#include "hashnameddir.pro"

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

#include "../config.h"
