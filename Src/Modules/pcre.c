/*
 * pcre.c - interface to the PCRE library
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 2001, 2002, 2003, 2004, 2007 Clint Adams
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

#define CPCRE_PLAIN 0

/**/
#if defined(HAVE_PCRE_COMPILE) && defined(HAVE_PCRE_EXEC)
#include <pcre.h>

static pcre *pcre_pattern;
static pcre_extra *pcre_hints;

/**/
static int
zpcre_utf8_enabled(void)
{
#if defined(MULTIBYTE_SUPPORT) && defined(HAVE_NL_LANGINFO) && defined(CODESET)
    static int have_utf8_pcre = -1;

    /* value can toggle based on MULTIBYTE, so don't
     * be too eager with caching */
    if (have_utf8_pcre < -1)
	return 0;

    if (!isset(MULTIBYTE))
	return 0;

    if ((have_utf8_pcre == -1) &&
        (!strcmp(nl_langinfo(CODESET), "UTF-8"))) {

	if (pcre_config(PCRE_CONFIG_UTF8, &have_utf8_pcre))
	    have_utf8_pcre = -2; /* erk, failed to ask */
    }

    if (have_utf8_pcre < 0)
	return 0;
    return have_utf8_pcre;

#else
    return 0;
#endif
}

/**/
static int
bin_pcre_compile(char *nam, char **args, Options ops, UNUSED(int func))
{
    int pcre_opts = 0, pcre_errptr;
    const char *pcre_error;
    
    if(OPT_ISSET(ops,'a')) pcre_opts |= PCRE_ANCHORED;
    if(OPT_ISSET(ops,'i')) pcre_opts |= PCRE_CASELESS;
    if(OPT_ISSET(ops,'m')) pcre_opts |= PCRE_MULTILINE;
    if(OPT_ISSET(ops,'x')) pcre_opts |= PCRE_EXTENDED;
    if(OPT_ISSET(ops,'s')) pcre_opts |= PCRE_DOTALL;
    
    if (zpcre_utf8_enabled())
	pcre_opts |= PCRE_UTF8;

    pcre_hints = NULL;  /* Is this necessary? */
    
    if (pcre_pattern)
	pcre_free(pcre_pattern);

    pcre_pattern = pcre_compile(*args, pcre_opts, &pcre_error, &pcre_errptr, NULL);
    
    if (pcre_pattern == NULL)
    {
	zwarnnam(nam, "error in regex: %s", pcre_error);
	return 1;
    }
    
    return 0;
}

/**/
#ifdef HAVE_PCRE_STUDY

/**/
static int
bin_pcre_study(char *nam, UNUSED(char **args), UNUSED(Options ops), UNUSED(int func))
{
    const char *pcre_error;

    if (pcre_pattern == NULL)
    {
	zwarnnam(nam, "no pattern has been compiled for study");
	return 1;
    }
    
    pcre_hints = pcre_study(pcre_pattern, 0, &pcre_error);
    if (pcre_error != NULL)
    {
	zwarnnam(nam, "error while studying regex: %s", pcre_error);
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
zpcre_get_substrings(char *arg, int *ovec, int ret, char *matchvar,
		     char *substravar, int want_offset_pair, int matchedinarr,
		     int want_begin_end)
{
    char **captures, *match_all, **matches;
    char offset_all[50];
    int capture_start = 1;

    if (matchedinarr)
	capture_start = 0;
    if (matchvar == NULL)
	matchvar = "MATCH";
    if (substravar == NULL)
	substravar = "match";
    
    /* captures[0] will be entire matched string, [1] first substring */
    if (!pcre_get_substring_list(arg, ovec, ret, (const char ***)&captures)) {
	int nelem = arrlen(captures)-1;
	/* Set to the offsets of the complete match */
	if (want_offset_pair) {
	    sprintf(offset_all, "%d %d", ovec[0], ovec[1]);
	    setsparam("ZPCRE_OP", ztrdup(offset_all));
	}
	match_all = ztrdup(captures[0]);
	setsparam(matchvar, match_all);
	/*
	 * If we're setting match, mbegin, mend we only do
	 * so if there were parenthesised matches, for consistency
	 * (c.f. regex.c).
	 */
	if (!want_begin_end || nelem) {
	    matches = zarrdup(&captures[capture_start]);
	    setaparam(substravar, matches);
	}

	if (want_begin_end) {
	    char *ptr = arg;
	    zlong offs = 0;

	    /* Count the characters before the match */
	    MB_METACHARINIT();
	    while (ptr < arg + ovec[0]) {
		offs++;
		ptr += MB_METACHARLEN(ptr);
	    }
	    setiparam("MBEGIN", offs + !isset(KSHARRAYS));
	    /* Add on the characters in the match */
	    while (ptr < arg + ovec[1]) {
		offs++;
		ptr += MB_METACHARLEN(ptr);
	    }
	    setiparam("MEND", offs + !isset(KSHARRAYS) - 1);
	    if (nelem) {
		char **mbegin, **mend, **bptr, **eptr;
		int i, *ipair;

		bptr = mbegin = zalloc(sizeof(char*)*(nelem+1));
		eptr = mend = zalloc(sizeof(char*)*(nelem+1));

		for (ipair = ovec + 2, i = 0;
		     i < nelem;
		     ipair += 2, i++, bptr++, eptr++)
		{
		    char buf[DIGBUFSIZE];
		    ptr = arg;
		    offs = 0;
		    /* Find the start offset */
		    MB_METACHARINIT();
		    while (ptr < arg + ipair[0]) {
			offs++;
			ptr += MB_METACHARLEN(ptr);
		    }
		    convbase(buf, offs + !isset(KSHARRAYS), 10);
		    *bptr = ztrdup(buf);
		    /* Continue to the end offset */
		    while (ptr < arg + ipair[1]) {
			offs++;
			ptr += MB_METACHARLEN(ptr);
		    }
		    convbase(buf, offs + !isset(KSHARRAYS) - 1, 10);
		    *eptr = ztrdup(buf);
		}
		*bptr = *eptr = NULL;

		setaparam("mbegin", mbegin);
		setaparam("mend", mend);
	    }
	}

	pcre_free_substring_list((const char **)captures);
    }

    return 0;
}

/**/
static int
getposint(char *instr, char *nam)
{
    char *eptr;
    int ret;

    ret = (int)zstrtol(instr, &eptr, 10);
    if (*eptr || ret < 0) {
	zwarnnam(nam, "integer expected: %s", instr);
	return -1;
    }

    return ret;
}

/**/
static int
bin_pcre_match(char *nam, char **args, Options ops, UNUSED(int func))
{
    int ret, capcount, *ovec, ovecsize, c;
    char *matched_portion = NULL;
    char *receptacle = NULL;
    int return_value = 1;
    /* The subject length and offset start are both int values in pcre_exec */
    int subject_len;
    int offset_start = 0;
    int want_offset_pair = 0;

    if (pcre_pattern == NULL) {
	zwarnnam(nam, "no pattern has been compiled");
	return 1;
    }
    
    if(OPT_HASARG(ops,c='a')) {
	receptacle = OPT_ARG(ops,c);
    }
    if(OPT_HASARG(ops,c='v')) {
	matched_portion = OPT_ARG(ops,c);
    }
    if(OPT_HASARG(ops,c='n')) { /* The offset position to start the search, in bytes. */
	offset_start = getposint(OPT_ARG(ops,c), nam);
    }
    /* For the entire match, 'Return' the offset byte positions instead of the matched string */
    if(OPT_ISSET(ops,'b')) want_offset_pair = 1; 
    
    if(!*args) {
	zwarnnam(nam, "not enough arguments");
    }
    
    if ((ret = pcre_fullinfo(pcre_pattern, pcre_hints, PCRE_INFO_CAPTURECOUNT, &capcount)))
    {
	zwarnnam(nam, "error %d in fullinfo", ret);
	return 1;
    }
    
    ovecsize = (capcount+1)*3;
    ovec = zalloc(ovecsize*sizeof(int));
    
    subject_len = (int)strlen(*args);

    if (offset_start < 0 || offset_start >= subject_len)
	ret = PCRE_ERROR_NOMATCH;
    else
	ret = pcre_exec(pcre_pattern, pcre_hints, *args, subject_len, offset_start, 0, ovec, ovecsize);

    if (ret==0) return_value = 0;
    else if (ret==PCRE_ERROR_NOMATCH) /* no match */;
    else if (ret>0) {
	zpcre_get_substrings(*args, ovec, ret, matched_portion, receptacle,
			     want_offset_pair, 0, 0);
	return_value = 0;
    }
    else {
	zwarnnam(nam, "error in pcre_exec");
    }
    
    if (ovec)
	zfree(ovec, ovecsize*sizeof(int));

    return return_value;
}

/**/
static int
cond_pcre_match(char **a, int id)
{
    pcre *pcre_pat;
    const char *pcre_err;
    char *lhstr, *rhre, *avar=NULL;
    int r = 0, pcre_opts = 0, pcre_errptr, capcnt, *ov, ovsize;
    int return_value = 0;

    if (zpcre_utf8_enabled())
	pcre_opts |= PCRE_UTF8;

    lhstr = cond_str(a,0,0);
    rhre = cond_str(a,1,0);
    pcre_pat = NULL;
    ov = NULL;

    if (isset(BASHREMATCH))
	avar="BASH_REMATCH";

    switch(id) {
	 case CPCRE_PLAIN:
		pcre_pat = pcre_compile(rhre, pcre_opts, &pcre_err, &pcre_errptr, NULL);
		if (pcre_pat == NULL) {
		    zwarn("failed to compile regexp /%s/: %s", rhre, pcre_err);
		    break;
		}
                pcre_fullinfo(pcre_pat, NULL, PCRE_INFO_CAPTURECOUNT, &capcnt);
    		ovsize = (capcnt+1)*3;
		ov = zalloc(ovsize*sizeof(int));
    		r = pcre_exec(pcre_pat, NULL, lhstr, strlen(lhstr), 0, 0, ov, ovsize);
		/* r < 0 => error; r==0 match but not enough size in ov
		 * r > 0 => (r-1) substrings found; r==1 => no substrings
		 */
    		if (r==0) {
		    zwarn("reportable zsh problem: pcre_exec() returned 0");
		    return_value = 1;
		    break;
		}
	        else if (r==PCRE_ERROR_NOMATCH) return 0; /* no match */
		else if (r<0) {
		    zwarn("pcre_exec() error: %d", r);
		    break;
		}
                else if (r>0) {
		    zpcre_get_substrings(lhstr, ov, r, NULL, avar, 0,
					 isset(BASHREMATCH),
					 !isset(BASHREMATCH));
		    return_value = 1;
		    break;
		}
		break;
    }

    if (pcre_pat)
	pcre_free(pcre_pat);
    if (ov)
	zfree(ov, ovsize*sizeof(int));

    return return_value;
}

static struct conddef cotab[] = {
    CONDDEF("pcre-match", CONDF_INFIX, cond_pcre_match, 0, 0, CPCRE_PLAIN)
    /* CONDDEF can register =~ but it won't be found */
};

/**/
#else /* !(HAVE_PCRE_COMPILE && HAVE_PCRE_EXEC) */

# define bin_pcre_compile bin_notavail
# define bin_pcre_study bin_notavail
# define bin_pcre_match bin_notavail

/**/
#endif /* !(HAVE_PCRE_COMPILE && HAVE_PCRE_EXEC) */

static struct builtin bintab[] = {
    BUILTIN("pcre_compile", 0, bin_pcre_compile, 1, 1, 0, "aimxs",  NULL),
    BUILTIN("pcre_match",   0, bin_pcre_match,   1, 1, 0, "a:v:n:b",    NULL),
    BUILTIN("pcre_study",   0, bin_pcre_study,   0, 0, 0, NULL,    NULL)
};


static struct features module_features = {
    bintab, sizeof(bintab)/sizeof(*bintab),
#if defined(HAVE_PCRE_COMPILE) && defined(HAVE_PCRE_EXEC)
    cotab, sizeof(cotab)/sizeof(*cotab),
#else /* !(HAVE_PCRE_COMPILE && HAVE_PCRE_EXEC) */
    NULL, 0,
#endif /* !(HAVE_PCRE_COMPILE && HAVE_PCRE_EXEC) */
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
