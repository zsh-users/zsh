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
#if defined(HAVE_PCRE2_COMPILE_8) && defined(HAVE_PCRE2_H)
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

static pcre2_code *pcre_pattern;

/**/
static int
zpcre_utf8_enabled(void)
{
#if defined(MULTIBYTE_SUPPORT) && defined(HAVE_NL_LANGINFO) && defined(CODESET)
    static int have_utf8_pcre = -1;

    if (have_utf8_pcre < -1)
	return 0;

    if (!isset(MULTIBYTE))
	return 0;

    if ((have_utf8_pcre == -1) &&
       (pcre2_config(PCRE2_CONFIG_UNICODE, &have_utf8_pcre))) {
           have_utf8_pcre = -2; /* erk, failed to ask */
    }

    return (have_utf8_pcre == 1) && (!strcmp(nl_langinfo(CODESET), "UTF-8"));

#else
    return 0;
#endif
}

/**/
static int
bin_pcre_compile(char *nam, char **args, Options ops, UNUSED(int func))
{
    uint32_t pcre_opts = 0;
    int target_len;
    int pcre_error;
    PCRE2_SIZE pcre_offset;
    char *target;
    
    if (OPT_ISSET(ops, 'a')) pcre_opts |= PCRE2_ANCHORED;
    if (OPT_ISSET(ops, 'i')) pcre_opts |= PCRE2_CASELESS;
    if (OPT_ISSET(ops, 'm')) pcre_opts |= PCRE2_MULTILINE;
    if (OPT_ISSET(ops, 'x')) pcre_opts |= PCRE2_EXTENDED;
    if (OPT_ISSET(ops, 's')) pcre_opts |= PCRE2_DOTALL;
    
    if (zpcre_utf8_enabled())
	pcre_opts |= PCRE2_UTF;

    if (pcre_pattern)
	pcre2_code_free(pcre_pattern);
    pcre_pattern = NULL;

    target = ztrdup(*args);
    unmetafy(target, &target_len);

    pcre_pattern = pcre2_compile((PCRE2_SPTR) target, (PCRE2_SIZE) target_len,
	    pcre_opts, &pcre_error, &pcre_offset, NULL);

    free(target);

    if (pcre_pattern == NULL)
    {
	PCRE2_UCHAR buffer[256];
	pcre2_get_error_message(pcre_error, buffer, sizeof(buffer));
	zwarnnam(nam, "error in regex: %s", buffer);
	return 1;
    }
    
    return 0;
}

/**/
static int
bin_pcre_study(char *nam, UNUSED(char **args), UNUSED(Options ops), UNUSED(int func))
{
    if (pcre_pattern == NULL)
    {
	zwarnnam(nam, "no pattern has been compiled for study");
	return 1;
    }

    int jit = 0;
    if (!pcre2_config(PCRE2_CONFIG_JIT, &jit) && jit) {
	if (pcre2_jit_compile(pcre_pattern, PCRE2_JIT_COMPLETE) < 0) {
	    zwarnnam(nam, "error while studying regex");
	    return 1;
	}
    }
    
    return 0;
}

static int
pcre_callout(pcre2_callout_block_8 *block, UNUSED(void *callout_data))
{
    Eprog prog;
    int ret=0;

    if (!block->callout_number &&
	    ((prog = parse_string((char *) block->callout_string, 0))))
    {
	int ef = errflag, lv = lastval;

	setsparam(".pcre.subject",
		metafy((char *) block->subject, block->subject_length, META_DUP));
	setiparam(".pcre.pos", block->current_position + 1);
	execode(prog, 1, 0, "pcre");
	ret = lastval | errflag;

	/* Restore any user interrupt error status */
	errflag = ef | (errflag & ERRFLAG_INT);
	lastval = lv;
    }

    return ret;
}

static int
zpcre_get_substrings(pcre2_code *pat, char *arg, pcre2_match_data *mdata,
	int captured_count, char *matchvar, char *substravar, char *namedassoc,
	int want_offset_pair, int matchedinarr, int want_begin_end)
{
    PCRE2_SIZE *ovec;
    char *match_all, **matches;
    char offset_all[50];
    int capture_start = 1;
    int vec_off;
    PCRE2_SPTR ntable; /* table of named captures */
    uint32_t ncount, nsize;

    if (matchedinarr) {
	/* bash-style ovec[0] entire-matched string in the array */
	capture_start = 0;
    }

    /* ovec[0] will be entire matched string, [1] first substring */
    ovec = pcre2_get_ovector_pointer(mdata);
    if (ovec) {
	int nelem = captured_count - 1;
	/* Set to the offsets of the complete match */
	if (want_offset_pair) {
	    sprintf(offset_all, "%ld %ld", ovec[0], ovec[1]);
	    setsparam("ZPCRE_OP", ztrdup(offset_all));
	}
	/*
	 * Result strings can contain embedded NULs; the length of each is the
	 * difference between the two values in each paired entry in ovec.
	 * ovec is length 2*(1+capture_list_length)
	 */
	if (matchvar) {
	    match_all = metafy(arg + ovec[0], ovec[1] - ovec[0], META_DUP);
	    setsparam(matchvar, match_all);
	}
	/*
	 * If we're setting match, mbegin, mend we only do
	 * so if there were parenthesised matches, for consistency
	 * (c.f. regex.c).  That's the next block after this one.
	 * Here we handle the simpler case where we don't worry about
	 * Unicode lengths, etc.
	 * Either !want_begin_end (ie, this is bash) or nelem; if bash
	 * then we're invoked always, even without nelem results, to
	 * set the array variable with one element in it, the complete match.
	 */
	if (substravar &&
	    (!want_begin_end || nelem)) {
	    char **x;
	    int i;
	    matches = x = (char **) zalloc(sizeof(char *) * (captured_count+1-capture_start));
	    for (i = capture_start; i < captured_count; i++) {
		vec_off = 2*i;
		*x++ = metafy(arg + ovec[vec_off], ovec[vec_off+1]-ovec[vec_off], META_DUP);
	    }
	    *x = NULL;
	    setaparam(substravar, matches);
	}

	if (namedassoc
		&& !pcre2_pattern_info(pat, PCRE2_INFO_NAMECOUNT, &ncount) && ncount
		&& !pcre2_pattern_info(pat, PCRE2_INFO_NAMEENTRYSIZE, &nsize)
		&& !pcre2_pattern_info(pat, PCRE2_INFO_NAMETABLE, &ntable))
	{
	    char **hash, **hashptr;
	    uint32_t nidx;
	    hashptr = hash = (char **)zshcalloc((ncount+1)*2*sizeof(char *));
	    for (nidx = 0; nidx < ncount; nidx++) {
		vec_off = (ntable[nsize * nidx] << 9) + 2 * ntable[nsize * nidx + 1];
		/* would metafy the key but pcre limits characters in the name */
		*hashptr++ = ztrdup((char *) ntable + nsize * nidx + 2);
		*hashptr++ = metafy(arg + ovec[vec_off],
			ovec[vec_off+1]-ovec[vec_off], META_DUP);
	    }
	    sethparam(namedassoc, hash);
	}

	if (want_begin_end) {
	    /*
	     * cond-infix rather than builtin; also not bash; so we set a bunch
	     * of variables and arrays to values which require handling Unicode
	     * lengths
	     */
	    char *ptr = arg;
	    zlong offs = 0;
	    int clen, leftlen;

	    /* Count the characters before the match */
	    MB_CHARINIT();
	    leftlen = ovec[0];
	    while (leftlen) {
		offs++;
		clen = MB_CHARLEN(ptr, leftlen);
		ptr += clen;
		leftlen -= clen;
	    }
	    setiparam("MBEGIN", offs + !isset(KSHARRAYS));
	    /* Add on the characters in the match */
	    leftlen = ovec[1] - ovec[0];
	    while (leftlen) {
		offs++;
		clen = MB_CHARLEN(ptr, leftlen);
		ptr += clen;
		leftlen -= clen;
	    }
	    setiparam("MEND", offs + !isset(KSHARRAYS) - 1);
	    if (nelem) {
		char **mbegin, **mend, **bptr, **eptr;
		int i;
		size_t *ipair;

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
		    MB_CHARINIT();
		    leftlen = ipair[0];
		    while (leftlen > 0) {
			offs++;
			clen = MB_CHARLEN(ptr, leftlen);
			ptr += clen;
			leftlen -= clen;
		    }
		    convbase(buf, offs + !isset(KSHARRAYS), 10);
		    *bptr = ztrdup(buf);
		    /* Continue to the end offset */
		    leftlen = ipair[1] - ipair[0];
		    while (leftlen) {
			offs++;
			clen = MB_CHARLEN(ptr, leftlen);
			ptr += clen;
			leftlen -= clen;
		    }
		    convbase(buf, offs + !isset(KSHARRAYS) - 1, 10);
		    *eptr = ztrdup(buf);
		}
		*bptr = *eptr = NULL;

		setaparam("mbegin", mbegin);
		setaparam("mend", mend);
	    }
	}
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
    int ret, c;
    pcre2_match_data *pcre_mdata = NULL;
    char *matched_portion = NULL;
    char *plaintext = NULL;
    char *receptacle;
    char *named = NULL;
    int return_value = 1;
    /* The subject length and offset start are both int values in pcre_exec */
    int subject_len;
    int offset_start = 0;
    int want_offset_pair = 0;
    int use_dfa = 0;

    if (pcre_pattern == NULL) {
	zwarnnam(nam, "no pattern has been compiled");
	return 1;
    }

    if (!(use_dfa = OPT_ISSET(ops, 'd'))) {
	matched_portion = OPT_HASARG(ops, c='v') ? OPT_ARG(ops, c) : "MATCH";
	named = OPT_HASARG(ops, c='A') ? OPT_ARG(ops, c) : ".pcre.match";
    } else if (OPT_HASARG(ops, c='v') || OPT_HASARG(ops, c='A')) {
	zwarnnam(nam, "-d cannot be combined with -%c", c);
	return 1;
    }
    receptacle = OPT_HASARG(ops, 'a') ? OPT_ARG(ops, 'a') : "match";

    if(OPT_HASARG(ops,c='n')) { /* The offset position to start the search, in bytes. */
	if ((offset_start = getposint(OPT_ARG(ops,c), nam)) < 0)
	    return 1;
    }
    /* For the entire match, 'Return' the offset byte positions instead of the matched string */
    if(OPT_ISSET(ops,'b')) want_offset_pair = 1;

    plaintext = ztrdup(*args);
    unmetafy(plaintext, &subject_len);

    pcre2_match_context_8 *mcontext = pcre2_match_context_create(NULL);
    pcre2_set_callout(mcontext, &pcre_callout, 0);

    if (offset_start > 0 && offset_start >= subject_len)
	ret = PCRE2_ERROR_NOMATCH;
    else if (use_dfa) {
	PCRE2_SIZE old, wscount = 128, capcount = 128;
	void *workspace = zhalloc(sizeof(int) * wscount);
	pcre_mdata = pcre2_match_data_create(capcount, NULL);
	do {
	    ret = pcre2_dfa_match(pcre_pattern, (PCRE2_SPTR) plaintext, subject_len,
		offset_start, 0, pcre_mdata, mcontext, (int *) workspace, wscount);
	    if (ret == PCRE2_ERROR_DFA_WSSIZE) {
		old = wscount;
		wscount += wscount / 2;
		workspace = hrealloc(workspace, sizeof(int) * old, sizeof(int) * wscount);
	    } else if (ret == 0) {
		capcount += capcount / 2;
		pcre2_match_data_free(pcre_mdata);
		pcre_mdata = pcre2_match_data_create(capcount, NULL);
	    } else
		break;
	} while(1);
    } else {
	pcre_mdata = pcre2_match_data_create_from_pattern(pcre_pattern, NULL);
	ret = pcre2_match(pcre_pattern, (PCRE2_SPTR) plaintext, subject_len,
		offset_start, 0, pcre_mdata, mcontext);
	if (ret > 0)
	    ret = pcre2_get_ovector_count(pcre_mdata);
    }

    if (ret==0) return_value = 0;
    else if (ret == PCRE2_ERROR_NOMATCH) /* no match */;
    else if (ret>0) {
	zpcre_get_substrings(pcre_pattern, plaintext, pcre_mdata, ret,
		matched_portion, receptacle, named, want_offset_pair, use_dfa, 0);
	return_value = 0;
    }
    else {
	PCRE2_UCHAR buffer[256];
	pcre2_get_error_message(ret, buffer, sizeof(buffer));
	zwarnnam(nam, "error in pcre matching for /%s/: %s", plaintext, buffer);
    }
    
    if (pcre_mdata)
	pcre2_match_data_free(pcre_mdata);
    if (mcontext)
	pcre2_match_context_free(mcontext);
    zsfree(plaintext);

    return return_value;
}

/**/
static int
cond_pcre_match(char **a, int id)
{
    pcre2_code *pcre_pat = NULL;
    int pcre_err;
    PCRE2_SIZE pcre_erroff;
    char *lhstr, *rhre, *lhstr_plain, *rhre_plain, *avar, *svar;
    int r = 0, pcre_opts = 0;
    pcre2_match_data *pcre_mdata = NULL;
    int lhstr_plain_len, rhre_plain_len;
    int return_value = 0;

    if (zpcre_utf8_enabled())
	pcre_opts |= PCRE2_UTF;
    if (isset(REMATCHPCRE) && !isset(CASEMATCH))
	pcre_opts |= PCRE2_CASELESS;

    lhstr = cond_str(a,0,0);
    rhre = cond_str(a,1,0);
    lhstr_plain = ztrdup(lhstr);
    rhre_plain = ztrdup(rhre);
    unmetafy(lhstr_plain, &lhstr_plain_len);
    unmetafy(rhre_plain, &rhre_plain_len);

    if (isset(BASHREMATCH)) {
	svar = NULL;
	avar = "BASH_REMATCH";
    } else {
	svar = "MATCH";
	avar = "match";
    }

    switch(id) {
	 case CPCRE_PLAIN:
		if (!(pcre_pat = pcre2_compile((PCRE2_SPTR) rhre_plain,
			(PCRE2_SIZE) rhre_plain_len, pcre_opts,
			&pcre_err, &pcre_erroff, NULL)))
		{
		    PCRE2_UCHAR buffer[256];
		    pcre2_get_error_message(pcre_err, buffer, sizeof(buffer));
		    zwarn("failed to compile regexp /%s/: %s", rhre, buffer);
		    break;
		}
		pcre_mdata = pcre2_match_data_create_from_pattern(pcre_pat, NULL);
		r = pcre2_match(pcre_pat, (PCRE2_SPTR8) lhstr_plain, lhstr_plain_len,
			0, 0, pcre_mdata, NULL);
		/* r < 0 => error; r==0 match but not enough size in match data
		 * r > 0 => (r-1) substrings found; r==1 => no substrings
		 */
    		if (r==0) {
		    zwarn("reportable zsh problem: pcre2_match() returned 0");
		    return_value = 1;
		    break;
		}
		else if (r == PCRE2_ERROR_NOMATCH) {
		    return_value = 0; /* no match */
		    break;
		}
		else if (r<0) {
		    zwarn("pcre_exec() error [%d]", r);
		    break;
		}
                else if (r>0) {
		    uint32_t ovec_count = pcre2_get_ovector_count(pcre_mdata);
		    zpcre_get_substrings(pcre_pat, lhstr_plain, pcre_mdata, ovec_count, svar, avar,
			    ".pcre.match", 0, isset(BASHREMATCH), !isset(BASHREMATCH));
		    return_value = 1;
		    break;
		}
		break;
    }

    if (lhstr_plain)
	free(lhstr_plain);
    if(rhre_plain)
	free(rhre_plain);
    if (pcre_mdata)
	pcre2_match_data_free(pcre_mdata);
    if (pcre_pat)
	pcre2_code_free(pcre_pat);

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
    BUILTIN("pcre_match",   0, bin_pcre_match,   1, 1, 0, "A:a:v:n:bd",    NULL),
    BUILTIN("pcre_study",   0, bin_pcre_study,   0, 0, 0, NULL,    NULL)
};


static struct features module_features = {
    bintab, sizeof(bintab)/sizeof(*bintab),
#if defined(HAVE_PCRE2_COMPILE_8) && defined(HAVE_PCRE2_H)
    cotab, sizeof(cotab)/sizeof(*cotab),
#else /* !(HAVE_PCRE2_COMPILE_8 && HAVE_PCRE2_H) */
    NULL, 0,
#endif /* !(HAVE_PCRE2_COMPILE_8 && HAVE_PCRE2_H) */
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
boot_(UNUSED(Module m))
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
#if defined(HAVE_PCRE2_COMPILE_8) && defined(HAVE_PCRE2_H)
    if (pcre_pattern)
	pcre2_code_free(pcre_pattern);
    pcre_pattern = NULL;
#endif

    return 0;
}
