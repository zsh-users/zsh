/*
 * pcre.c - interface to the PCRE library
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 2001 Clint Adams
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Clint Adams or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Andrew Main and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Clint Adams and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Andrew Main and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */


#include "pcre.mdh"
#include "pcre.pro"

/**/
#if defined(HAVE_PCRE_COMPILE) && defined(HAVE_PCRE_EXEC)
#include <pcre.h>

static pcre *pcre_pattern;
static pcre_extra *pcre_hints;

/**/
static int
bin_pcre_compile(char *nam, char **args, Options ops, int func)
{
    int pcre_opts = 0, pcre_errptr;
    const char *pcre_error;
    
    if(OPT_ISSET(ops,'a')) pcre_opts |= PCRE_ANCHORED;
    if(OPT_ISSET(ops,'i')) pcre_opts |= PCRE_CASELESS;
    if(OPT_ISSET(ops,'m')) pcre_opts |= PCRE_MULTILINE;
    if(OPT_ISSET(ops,'x')) pcre_opts |= PCRE_EXTENDED;
    
    pcre_hints = NULL;  /* Is this necessary? */
    
    pcre_pattern = pcre_compile(*args, pcre_opts, &pcre_error, &pcre_errptr, NULL);
    
    if (pcre_pattern == NULL)
    {
	zwarnnam(nam, "error in regex: %s", pcre_error, 0);
	return 1;
    }
    
    return 0;
}

/**/
#ifdef HAVE_PCRE_STUDY

/**/
static int
bin_pcre_study(char *nam, char **args, Options ops, int func)
{
    const char *pcre_error;
    
    pcre_hints = pcre_study(pcre_pattern, 0, &pcre_error);
    if (pcre_error != NULL)
    {
	zwarnnam(nam, "error while studying regex: %s", pcre_error, 0);
	return 1;
    }
    
    return 0;
}

/**/
#else /* !HAVE_PCRE_STUDY */

# define bin_pcre_study bin_notavail

/**/
#endif /* !HAVE_PCRE_STUDY */

/**/
static int
bin_pcre_match(char *nam, char **args, Options ops, int func)
{
    int ret, capcount, *ovec, ovecsize;
    char **captures, **matches, *receptacle = NULL;
    
    if(OPT_ISSET(ops,'a')) {
	receptacle = *args++;
	if(!*args) {
	    zwarnnam(nam, "not enough arguments", NULL, 0);
	    return 1;
	}
    }
    
    if (pcre_fullinfo(pcre_pattern, pcre_hints, PCRE_INFO_CAPTURECOUNT, &capcount))
    {
	zwarnnam(nam, "error in fullinfo", NULL, 0);
	return 1;
    }
    
    ovecsize = (capcount+1)*3;
    ovec = zalloc(ovecsize*sizeof(int));
    
    ret = pcre_exec(pcre_pattern, pcre_hints, *args, strlen(*args), 0, 0, ovec, ovecsize);
    
    if (ret==0) return 0;
    else if (ret==PCRE_ERROR_NOMATCH) return 1; /* no match */
    else if (ret>0) {
	if(!pcre_get_substring_list(*args, ovec, ret, (const char ***)&captures)) {
	    
	    matches = zarrdup(&captures[1]); /* first one would be entire string */
	    if (receptacle == NULL)
		setaparam("match", matches);
	    else
		setaparam(receptacle, matches);
	    
	    pcre_free_substring_list((const char **)captures);
	    return 0;
      	}
    }
    else {
	zwarnnam(nam, "error in pcre_exec", NULL, 0);
	return 1;
    }
    
    return 1;
}

/**/
#else /* !(HAVE_PCRE_COMPILE && HAVE_PCRE_EXEC) */

# define bin_pcre_compile bin_notavail
# define bin_pcre_study bin_notavail
# define bin_pcre_match bin_notavail

/**/
#endif /* !(HAVE_PCRE_COMPILE && HAVE_PCRE_EXEC) */

static struct builtin bintab[] = {
    BUILTIN("pcre_compile", 0, bin_pcre_compile, 1, 1, 0, "aimx",  NULL),
    BUILTIN("pcre_study",   0, bin_pcre_study,   0, 0, 0, NULL,    NULL),
    BUILTIN("pcre_match",   0, bin_pcre_match,   1, 2, 0, "a",    NULL)
};

/**/
int
setup_(Module m)
{
    return 0;
}

/**/
int
boot_(Module m)
{
    return !addbuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
}

/**/
int
cleanup_(Module m)
{
    deletebuiltins(m->nam, bintab, sizeof(bintab)/sizeof(*bintab));
    return 0;
}

/**/
int
finish_(Module m)
{
    return 0;
}
