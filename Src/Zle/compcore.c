/*
 * compcore.c - the complete module, completion core code
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 1999 Sven Wischnowsky
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Sven Wischnowsky or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Sven Wischnowsky and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Sven Wischnowsky and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Sven Wischnowsky and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "complete.mdh"
#include "compcore.pro"

/* The last completion widget called. */

static Widget lastcompwidget;

/* Flags saying what we have to do with the result. */

/**/
int useexact, useline, uselist, forcelist, startauto;

/* Non-zero if we should go back to the last prompt. */

/**/
int dolastprompt;

/* Non-zero if we should keep an old list. */

/**/
int oldlist, oldins;

/* This is used to decide when the cursor should be moved to the end of    *
 * the inserted word: 0 - never, 1 - only when a single match is inserted, *
 * 2 - when a full match is inserted (single or menu), 3 - always.         */

/**/
int movetoend;

/* The match and group number to insert when starting menucompletion.   */

/**/
int insmnum, insgnum, insgroup, insspace;

/* Information about menucompletion. */

/**/
mod_export struct menuinfo minfo;

/* Number of matches accepted with accept-and-menu-complete */

/**/
mod_export int menuacc;

/* Brace insertion stuff. */

/**/
int hasunqu, useqbr, brpcs, brscs;

/* Flags saying in what kind of string we are. */

/**/
mod_export int ispar, linwhat;

/* A parameter expansion prefix (like ${). */

/**/
char *parpre;

/* Flags for parameter expansions for new style completion. */

/**/
int parflags;

/* Match flags for all matches in this group. */

/**/
mod_export int mflags;

/* Flags saying how the parameter expression we are in is quoted. */

/**/
int parq, eparq;

/* We store the following prefixes/suffixes:                               *
 * ipre,ripre  -- the ignored prefix (quoted and unquoted)                 *
 * isuf        -- the ignored suffix                                       */

/**/
mod_export char *ipre, *ripre, *isuf;

/* The list of matches.  fmatches contains the matches we first ignore *
 * because of fignore.                                                 */

/**/
mod_export LinkList matches;
/**/
LinkList fmatches;

/* This holds the list of matches-groups. lastmatches holds the last list of 
 * permanently allocated matches, pmatches is the same for the list
 * currently built, amatches is the heap allocated stuff during completion
 * (after all matches have been generated it is an alias for pmatches), and
 * lmatches/lastlmatches is a pointer to the last element in the lists. */

/**/
mod_export Cmgroup lastmatches, pmatches, amatches, lmatches, lastlmatches;

/* Non-zero if we have permanently allocated matches (old and new). */

/**/
mod_export int hasoldlist, hasperm;

/* Non-zero if we have newly added matches. */

/**/
int newmatches;

/* Number of permanently allocated matches and groups. */

/**/
int permmnum, permgnum, lastpermmnum, lastpermgnum;

/* The total number of matches and the number of matches to be listed. */

/**/
mod_export int nmatches;
/**/
mod_export int smatches;

/* != 0 if only explanation strings should be printed */

/**/
mod_export int onlyexpl;

/* Information about the matches for listing. */

/**/
mod_export struct cldata listdat;

/* This flag is non-zero if we are completing a pattern (with globcomplete) */

/**/
mod_export int ispattern, haspattern;

/* Non-zero if at least one match was added without -U. */

/**/
mod_export int hasmatched;

/* The current group of matches. */

/**/
Cmgroup mgroup;

/* Match counter: all matches. */

/**/
mod_export int mnum;

/* The match counter when unambig_data() was called. */

/**/
int unambig_mnum;

/* Length of longest/shortest match. */

/**/
int maxmlen, minmlen;

/* This holds the explanation strings we have to print in this group and *
 * a pointer to the current cexpl structure. */

/**/
LinkList expls;

/**/
mod_export Cexpl curexpl;

/* A stack of completion matchers to be used. */

/**/
mod_export Cmlist mstack;

/* The completion matchers used when building new stuff for the line. */

/**/
mod_export Cmlist bmatchers;

/* A list with references to all matchers we used. */

/**/
mod_export LinkList matchers;

/* A heap of free Cline structures. */

/**/
Cline freecl;

/* Ambiguous information. */

/**/
Aminfo ainfo, fainfo;

/* The memory heap to use for new style completion generation. */

/**/
mod_export Heap compheap;

/* A list of some data.
 *
 * Well, actually, it's the list of all compctls used so far, but since
 * conceptually we don't know anything about compctls here... */

/**/
mod_export LinkList allccs;

/* This says what of the state the line is in when completion is started *
 * came from a previous completion. If the FC_LINE bit is set, the       *
 * string was inserted. If FC_INWORD is set, the last completion moved   *
 * the cursor into the word although it was at the end of it when the    *
 * last completion was invoked.                                          *
 * This is used to detect if the string should be taken as an exact      *
 * match (see do_ambiguous()) and if the cursor has to be moved to the   *
 * end of the word before generating the completions.                    */

/**/
int fromcomp;

/* This holds the end-position of the last string inserted into the line. */

/**/
int lastend;

#define inststr(X) inststrlen((X),1,-1)

/* Main completion entry point, called from zle. */

/**/
int
do_completion(Hookdef dummy, Compldat dat)
{
    int ret = 0, lst = dat->lst, incmd = dat->incmd;
    char *s = dat->s;

    HEAPALLOC {
	char *opm;
	LinkNode n;

	pushheap();

	ainfo = fainfo = NULL;
	matchers = newlinklist();

	zsfree(compqstack);
	compqstack = ztrdup("\\");
	if (instring == 2)
	    compqstack[0] = '"';
	else if (instring)
	    compqstack[0] = '\'';

	hasunqu = 0;
	useline = (lst != COMP_LIST_COMPLETE);
	useexact = isset(RECEXACT);
	zsfree(compexactstr);
	compexactstr = ztrdup("");
	uselist = (useline ?
		   ((isset(AUTOLIST) && !isset(BASHAUTOLIST)) ? 
		    (isset(LISTAMBIGUOUS) ? 3 : 2) : 0) : 1);
	zsfree(comppatmatch);
	opm = comppatmatch = ztrdup(useglob ? "*" : "");
	zsfree(comppatinsert);
	comppatinsert = ztrdup("menu");
	forcelist = 0;
	haspattern = 0;
	complistmax = getiparam("LISTMAX");
	zsfree(complastprompt);
	complastprompt = ztrdup((dolastprompt =
				 ((isset(ALWAYSLASTPROMPT) && zmult == 1) ||
				  (unset(ALWAYSLASTPROMPT) && zmult != 1))) ?
				"yes" : "");
	zsfree(complist);
	complist = ztrdup(isset(LISTROWSFIRST) ?
			  (isset(LISTPACKED) ? "packed rows" : "rows") :
			  (isset(LISTPACKED) ? "packed" : ""));
	startauto = isset(AUTOMENU);
	movetoend = ((cs == we || isset(ALWAYSTOEND)) ? 2 : 1);
	showinglist = 0;
	hasmatched = 0;
	minmlen = 1000000;
	maxmlen = -1;

	/* Make sure we have the completion list and compctl. */
	if (makecomplist(s, incmd, lst)) {
	    /* Error condition: feeeeeeeeeeeeep(). */
	    cs = 0;
	    foredel(ll);
	    inststr(origline);
	    cs = origcs;
	    clearlist = 1;
	    ret = 1;
	    minfo.cur = NULL;
	    goto compend;
	}
	zsfree(lastprebr);
	zsfree(lastpostbr);
	lastprebr = lastpostbr = NULL;

	if (comppatmatch && *comppatmatch && comppatmatch != opm)
	    haspattern = 1;
	if (!useline && uselist) {
	    /* All this and the guy only wants to see the list, sigh. */
	    cs = 0;
	    foredel(ll);
	    inststr(origline);
	    cs = origcs;
	    showinglist = -2;
	} else if (useline == 2 && nmatches > 1) {
	    int first = 1, nm = nmatches;
	    Cmatch *mc;

	    menucmp = 1;
	    menuacc = 0;

	    for (minfo.group = amatches;
		 minfo.group && !(minfo.group)->mcount;
		 minfo.group = (minfo.group)->next);

	    mc = (minfo.group)->matches;

	    while (1) {
		if (!first)
		    accept_last();
		first = 0;

		if (!--nm)
		    menucmp = 0;

		do_single(*mc);
		minfo.cur = mc;

		if (!*++(minfo.cur)) {
		    do {
			if (!(minfo.group = (minfo.group)->next))
			    break;
		    } while (!(minfo.group)->mcount);
		    if (!minfo.group)
			break;
		    minfo.cur = minfo.group->matches;
		}
		mc = minfo.cur;
	    }
	    menucmp = 0;
	    minfo.cur = NULL;

	    if (forcelist)
		showinglist = -2;
	    else
		invalidatelist();
	} else if (useline) {
	    /* We have matches. */
	    if (nmatches > 1) {
		/* There is more than one match. */
		ret = do_ambiguous();
	    } else if (nmatches == 1) {
		/* Only one match. */
		Cmgroup m = amatches;

		while (!m->mcount)
		    m = m->next;
		minfo.cur = NULL;
		minfo.asked = 0;
		do_single(m->matches[0]);
		if (forcelist) {
		    if (uselist)
			showinglist = -2;
		    else
			clearlist = 1;
		} else
		    invalidatelist();
	    }
	} else {
	    invalidatelist();
	    if (forcelist)
		clearlist = 1;
	    cs = 0;
	    foredel(ll);
	    inststr(origline);
	    cs = origcs;
	}
	/* Print the explanation strings if needed. */
	if (!showinglist && validlist && usemenu != 2 && nmatches != 1 &&
	    useline != 2 && (!oldlist || !listshown)) {
	    onlyexpl = 1;
	    showinglist = -2;
	}
      compend:
	for (n = firstnode(matchers); n; incnode(n))
	    freecmatcher((Cmatcher) getdata(n));

	ll = strlen((char *)line);
	if (cs > ll)
	    cs = ll;
	popheap();
    } LASTALLOC;

    return ret;
}

/* Before and after hooks called by zle. */

static int oldmenucmp;

/**/
int
before_complete(Hookdef dummy, int *lst)
{
    oldmenucmp = menucmp;

    if (showagain && validlist)
	showinglist = -2;
    showagain = 0;

    /* If we are doing a menu-completion... */

    if (menucmp && *lst != COMP_LIST_EXPAND && 
	(menucmp != 1 || !compwidget || compwidget == lastcompwidget)) {
	do_menucmp(*lst);
	return 1;
    }
    if (menucmp && validlist && *lst == COMP_LIST_COMPLETE) {
	showinglist = -2;
	onlyexpl = listdat.valid = 0;
	return 1;
    }
    lastcompwidget = compwidget;

    /* We may have to reset the cursor to its position after the   *
     * string inserted by the last completion. */

    if ((fromcomp & FC_INWORD) && (cs = lastend) > ll)
	cs = ll;

    /* Check if we have to start a menu-completion (via automenu). */

    if (startauto && lastambig &&
	(!isset(BASHAUTOLIST) || lastambig == 2))
	usemenu = 2;

    return 0;
}

/**/
int
after_complete(Hookdef dummy, Compldat dat)
{
    if (menucmp && !oldmenucmp) {
	struct chdata dat;

	dat.matches = amatches;
	dat.num = nmatches;
	dat.cur = NULL;
	if (runhookdef(MENUSTARTHOOK, (void *) &dat))
	    menucmp = menuacc = 0;
    }
    return 0;
}

/* This calls the given completion widget function. */

/**/
static void
callcompfunc(char *s, char *fn)
{
    List list;
    int lv = lastval;
    char buf[20];

    if ((list = getshfunc(fn)) != &dummy_list) {
	char **p, *tmp;
	int aadd = 0, usea = 1, icf = incompfunc, osc = sfcontext;
	unsigned int rset, kset;
	Param *ocrpms = comprpms, *ockpms = compkpms;

	comprpms = (Param *) zalloc(CP_REALPARAMS * sizeof(Param));
	compkpms = (Param *) zalloc(CP_KEYPARAMS * sizeof(Param));

	rset = CP_ALLREALS;
	kset = CP_ALLKEYS &
	    ~(CP_PARAMETER | CP_REDIRECT | CP_QUOTE | CP_QUOTING |
	      CP_EXACTSTR | CP_OLDLIST | CP_OLDINS |
	      (useglob ? 0 : CP_PATMATCH));
	zsfree(compvared);
	if (varedarg) {
	    compvared = ztrdup(varedarg);
	    kset |= CP_VARED;
	} else
	    compvared = ztrdup("");
	if (!*complastprompt)
	    kset &= ~CP_LASTPROMPT;
	zsfree(compcontext);
	zsfree(compparameter);
	zsfree(compredirect);
	compparameter = compredirect = "";
	if (ispar)
	    compcontext = (ispar == 2 ? "brace_parameter" : "parameter");
	else if (linwhat == IN_MATH) {
	    if (insubscr) {
		compcontext = "subscript";
		if (varname) {
		    compparameter = varname;
		    kset |= CP_PARAMETER;
		}
	    } else
		compcontext = "math";
	    usea = 0;
	} else if (lincmd) {
	    if (insubscr) {
		compcontext = "subscript";
		kset |= CP_PARAMETER;
	    } else
		compcontext = "command";
	} else if (linredir) {
	    compcontext = "redirect";
	    if (rdstr)
		compredirect = rdstr;
	    kset |= CP_REDIRECT;
	} else
	    switch (linwhat) {
	    case IN_ENV:
		compcontext = (linarr ? "array_value" : "value");
		compparameter = varname;
		kset |= CP_PARAMETER;
		if (!clwpos) {
		    clwpos = 1;
		    clwnum = 2;
		    zsfree(clwords[1]);
		    clwords[1] = ztrdup(s);
		    zsfree(clwords[2]);
		    clwords[2] = NULL;
		}
		aadd = 1;
		break;
	    case IN_COND:
		compcontext = "condition";
		break;
	    default:
		if (cmdstr)
		    compcontext = "command";
		else {
		    compcontext = "value";
		    kset |= CP_PARAMETER;
		    if (clwords[0])
			compparameter = clwords[0];
		    aadd = 1;
		}
	    }
	compcontext = ztrdup(compcontext);
	if (compwords)
	    freearray(compwords);
	if (usea && (!aadd || clwords[0])) {
	    char **q;

	    PERMALLOC {
		q = compwords = (char **)
		    zalloc((clwnum + 1) * sizeof(char *));
		for (p = clwords + aadd; *p; p++, q++) {
		    tmp = dupstring(*p);
		    untokenize(tmp);
		    *q = ztrdup(tmp);
		}
		*q = NULL;
	    } LASTALLOC;
	} else
	    compwords = (char **) zcalloc(sizeof(char *));

	compparameter = ztrdup(compparameter);
	compredirect = ztrdup(compredirect);
	zsfree(compquote);
	zsfree(compquoting);
	if (instring) {
	    if (instring == 1) {
		compquote = ztrdup("\'");
		compquoting = ztrdup("single");
	    } else {
		compquote = ztrdup("\"");
		compquoting = ztrdup("double");
	    }
	    kset |= CP_QUOTE | CP_QUOTING;
	} else if (inbackt) {
	    compquote = ztrdup("`");
	    compquoting = ztrdup("backtick");
	    kset |= CP_QUOTE | CP_QUOTING;
	} else {
	    compquote = ztrdup("");
	    compquoting = ztrdup("");
	}
	zsfree(compprefix);
	zsfree(compsuffix);
	if (unset(COMPLETEINWORD)) {
	    tmp = multiquote(s, 0);
	    untokenize(tmp);
	    compprefix = ztrdup(tmp);
	    compsuffix = ztrdup("");
	} else {
	    char *ss, sav;
	    
	    ss = s + offs;

	    sav = *ss;
	    *ss = '\0';
	    tmp = multiquote(s, 0);
	    untokenize(tmp);
	    compprefix = ztrdup(tmp);
	    *ss = sav;
	    ss = multiquote(ss, 0);
	    untokenize(ss);
	    compsuffix = ztrdup(ss);
	}
	zsfree(compiprefix);
	compiprefix = ztrdup("");
	zsfree(compisuffix);
	compisuffix = ztrdup("");
	zsfree(compqiprefix);
	compqiprefix = ztrdup(qipre ? qipre : "");
	zsfree(compqisuffix);
	compqisuffix = ztrdup(qisuf ? qisuf : "");
	compcurrent = (usea ? (clwpos + 1 - aadd) : 0);

	zsfree(complist);
	switch (uselist) {
	case 0: complist = ""; kset &= ~CP_LIST; break;
	case 1: complist = "list"; break;
	case 2: complist = "autolist"; break;
	case 3: complist = "ambiguous"; break;
	}
	if (isset(LISTPACKED))
	    complist = dyncat(complist, " packed");
	if (isset(LISTROWSFIRST))
	    complist = dyncat(complist, " rows");

	complist = ztrdup(complist);
	zsfree(compinsert);
	if (useline) {
	    switch (usemenu) {
	    case 0:
		compinsert = (isset(AUTOMENU) ?
			      "automenu-unambiguous" :
			      "unambiguous");
		break;
	    case 1: compinsert = "menu"; break;
	    case 2: compinsert = "automenu"; break;
	    }
	} else {
	    compinsert = "";
	    kset &= ~CP_INSERT;
	}
	compinsert = ztrdup(compinsert);
	if (useexact)
	    compexact = ztrdup("accept");
	else {
	    compexact = ztrdup("");
	    kset &= ~CP_EXACT;
	}
	zsfree(comptoend);
	if (movetoend == 1)
	    comptoend = ztrdup("single");
	else
	    comptoend = ztrdup("match");
	zsfree(compoldlist);
	zsfree(compoldins);
	if (hasoldlist && lastpermmnum) {
	    if (listshown)
		compoldlist = "shown";
	    else
		compoldlist = "yes";
	    kset |= CP_OLDLIST;
	    if (minfo.cur) {
		sprintf(buf, "%d", (*(minfo.cur))->gnum);
		compoldins = buf;
		kset |= CP_OLDINS;
	    } else
		compoldins = "";
	} else
	    compoldlist = compoldins = "";
	compoldlist = ztrdup(compoldlist);
	compoldins = ztrdup(compoldins);

	incompfunc = 1;
	startparamscope();
	makecompparams();
	comp_setunset(rset, (~rset & CP_ALLREALS),
		      kset, (~kset & CP_ALLKEYS));
	makezleparams(1);
	sfcontext = SFC_CWIDGET;
	NEWHEAPS(compheap) {
	    LinkList largs = NULL;
	    int olv = lastval;

	    if (*cfargs) {
		char **p = cfargs;

		largs = newlinklist();
		addlinknode(largs, dupstring(fn));
		while (*p)
		    addlinknode(largs, dupstring(*p++));
	    }
	    doshfunc(fn, list, largs, 0, 0);
	    cfret = lastval;
	    lastval = olv;
	} OLDHEAPS;
	sfcontext = osc;
	endparamscope();
	lastcmd = 0;
	incompfunc = icf;

	if (!complist)
	    uselist = 0;
	else if (!strncmp(complist, "list", 4))
	    uselist = 1;
	else if (!strncmp(complist, "auto", 4))
	    uselist = 2;
	else if (!strncmp(complist, "ambig", 5))
	    uselist = 3;
	else
	    uselist = 0;
	forcelist = (complist && strstr(complist, "force"));
	onlyexpl = (complist && strstr(complist, "expl"));

	if (!compinsert)
	    useline = 0;
	else if (!strcmp(compinsert, "unambig") ||
		 !strcmp(compinsert, "unambiguous") ||
		 !strcmp(compinsert, "automenu-unambiguous"))
	    useline = 1, usemenu = 0;
	else if (!strcmp(compinsert, "menu"))
	    useline = 1, usemenu = 1;
	else if (!strcmp(compinsert, "auto") ||
		 !strcmp(compinsert, "automenu"))
	    useline = 1, usemenu = 2;
	else if (!strcmp(compinsert, "all"))
	    useline = 2, usemenu = 0;
	else if (idigit(*compinsert)) {
	    char *m;

	    useline = 1; usemenu = 3;
	    insmnum = atoi(compinsert);
	    if ((m = strchr(compinsert, ':'))) {
		insgroup = 1;
		insgnum = atoi(m + 1);
	    }
	    insspace = (compinsert[strlen(compinsert) - 1] == ' ');
	} else
	    useline = usemenu = 0;
	startauto = (compinsert &&
		     !strcmp(compinsert, "automenu-unambiguous"));
	useexact = (compexact && !strcmp(compexact, "accept"));

	if (!comptoend || !*comptoend)
	    movetoend = 0;
	else if (!strcmp(comptoend, "single"))
	    movetoend = 1;
	else if (!strcmp(comptoend, "always"))
	    movetoend = 3;
	else
	    movetoend = 2;

	oldlist = (hasoldlist && compoldlist && !strcmp(compoldlist, "keep"));
	oldins = (hasoldlist && minfo.cur &&
		  compoldins && !strcmp(compoldins, "keep"));

	zfree(comprpms, CP_REALPARAMS * sizeof(Param));
	zfree(compkpms, CP_KEYPARAMS * sizeof(Param));
	comprpms = ocrpms;
	compkpms = ockpms;
    }
    lastval = lv;
}

/* Create the completion list.  This is called whenever some bit of   *
 * completion code needs the list.                                    *
 * Along with the list is maintained the prefixes/suffixes etc.  When *
 * any of this becomes invalid -- e.g. if some text is changed on the *
 * command line -- invalidatelist() should be called, to set          *
 * validlist to zero and free up the memory used.  This function      *
 * returns non-zero on error.                                         */

/**/
static int
makecomplist(char *s, int incmd, int lst)
{
    struct cmlist ms;
    Cmlist m;
    char *p, *os = s;
    int onm = nmatches, osi = movefd(0);

    /* Inside $... ? */
    if (compfunc && (p = check_param(s, 0, 0)))
	os = s = p;

    /* We build a copy of the list of matchers to use to make sure that this
     * works even if a shell function called from the completion code changes
     * the global matchers. */

    if ((m = cmatcher)) {
	Cmlist mm, *mp = &mm;
	int n;

	for (n = 0; m; m = m->next, n++) {
	    *mp = (Cmlist) zhalloc(sizeof(struct cmlist));
	    (*mp)->matcher = m->matcher;
	    (*mp)->next = NULL;
	    (*mp)->str = dupstring(m->str);
	    mp = &((*mp)->next);
	    addlinknode(matchers, m->matcher);
	    if (m->matcher)
		m->matcher->refc++;
	}
	m = mm;
	compmatcher = 1;
	compmatchertot = n;
    } else
	compmatcher = 0;

    linwhat = inwhat;

    /* Walk through the global matchers. */
    for (;;) {
	bmatchers = NULL;
	zsfree(compmatcherstr);
	if (m) {
	    ms.next = NULL;
	    ms.matcher = m->matcher;
	    mstack = &ms;

	    /* Store the matchers used in the bmatchers list which is used
	     * when building new parts for the string to insert into the 
	     * line. */
	    add_bmatchers(m->matcher);
	    compmatcherstr = ztrdup(m->str);
	} else {
	    mstack = NULL;
	    compmatcherstr = ztrdup("");
	}
	ainfo = (Aminfo) hcalloc(sizeof(struct aminfo));
	fainfo = (Aminfo) hcalloc(sizeof(struct aminfo));

	freecl = NULL;

	if (!validlist)
	    lastambig = 0;
	amatches = NULL;
	mnum = 0;
	unambig_mnum = -1;
	isuf = NULL;
	insmnum = insgnum = 1;
	insgroup = oldlist = oldins = 0;
	begcmgroup("default", 0);
	menucmp = menuacc = newmatches = onlyexpl = 0;

	runhookdef(COMPCTLBEFOREHOOK, NULL);

	s = dupstring(os);
	if (compfunc)
	    callcompfunc(s, compfunc);
	else {
	    struct ccmakedat dat;

	    dat.str = s;
	    dat.incmd = incmd;
	    dat.lst = lst;
	    runhookdef(COMPCTLMAKEHOOK, (void *) &dat);
	}
	endcmgroup(NULL);

	runhookdef(COMPCTLAFTERHOOK,
		   (void *) ((amatches && !oldlist) ? 1L : 0L));

	if (oldlist) {
	    nmatches = onm;
	    validlist = 1;
	    amatches = lastmatches;
	    lmatches = lastlmatches;
	    if (pmatches) {
		freematches(pmatches);
		pmatches = NULL;
		hasperm = 0;
	    }
	    redup(osi, 0);

	    return 0;
	}
	PERMALLOC {
	    if (lastmatches) {
		freematches(lastmatches);
		lastmatches = NULL;
	    }
	    permmatches(1);
	    amatches = pmatches;
	    lastpermmnum = permmnum;
	    lastpermgnum = permgnum;
	} LASTALLOC;

	lastmatches = pmatches;
	lastlmatches = lmatches;
	pmatches = NULL;
	hasperm = 0;
	hasoldlist = 1;

	if (nmatches && !errflag) {
	    validlist = 1;

	    redup(osi, 0);

	    return 0;
	}
	if (!m || !(m = m->next))
	    break;

	errflag = 0;
	compmatcher++;
    }
    redup(osi, 0);
    return 1;
}

/**/
mod_export char *
multiquote(char *s, int ign)
{
    if (s) {
	char *os = s, *p = compqstack;

	if (p && *p && (ign < 1 || p[ign])) {
	    if (ign > 0)
		p += ign;
	    while (*p) {
		if (ign >= 0 || p[1])
		    s = bslashquote(s, NULL,
				    (*p == '\'' ? 1 : (*p == '"' ? 2 : 0)));
		p++;
	    }
	}
	return (s == os ? dupstring(s) : s);
    }
    DPUTS(1, "BUG: null pointer in multiquote()");
    return NULL;
}

/**/
mod_export char *
tildequote(char *s, int ign)
{
    if (s) {
	int tilde;

	if ((tilde = (*s == '~')))
	    *s = 'x';
	s = multiquote(s, ign);
	if (tilde)
	    *s = '~';

	return s;
    }
    DPUTS(1, "BUG: null pointer in tildequote()");
    return NULL;
}

/* Check if we have to complete a parameter name. */

/**/
mod_export char *
check_param(char *s, int set, int test)
{
    char *p;

    zsfree(parpre);
    parpre = NULL;

    if (!test)
	ispar = parq = eparq = 0;
    /* Try to find a `$'. */
    for (p = s + offs; p > s && *p != String && *p != Qstring; p--);
    if (*p == String || *p == Qstring) {
	/* Handle $$'s */
	while (p > s && (p[-1] == String || p[-1] == Qstring))
	    p--;
	while ((p[1] == String || p[1] == Qstring) &&
	       (p[2] == String || p[2] == Qstring))
	    p += 2;
    }
    if ((*p == String || *p == Qstring) && p[1] != Inpar && p[1] != Inbrack) {
	/* This is really a parameter expression (not $(...) or $[...]). */
	char *b = p + 1, *e = b;
	int n = 0, br = 1, nest = 0;

	if (*b == Inbrace) {
	    char *tb = b;

	    /* If this is a ${...}, see if we are before the '}'. */
	    if (!skipparens(Inbrace, Outbrace, &tb))
		return NULL;

	    /* Ignore the possible (...) flags. */
	    b++, br++;
	    n = skipparens(Inpar, Outpar, &b);

	    for (tb = p - 1; tb > s && *tb != Outbrace && *tb != Inbrace; tb--);
	    if (tb > s && *tb == Inbrace && (tb[-1] == String || *tb == Qstring))
		nest = 1;
	}

	/* Ignore the stuff before the parameter name. */
	for (; *b; b++)
	    if (*b != '^' && *b != Hat &&
		*b != '=' && *b != Equals &&
		*b != '~' && *b != Tilde)
		break;
	if (*b == '#' || *b == Pound || *b == '+')
	    b++;

	e = b;
	if (br) {
	    while (*e == (test ? Dnull : '"'))
		e++, parq++;
	    if (!test)
		b = e;
	}
	/* Find the end of the name. */
	if (*e == Quest || *e == Star || *e == String || *e == Qstring ||
	    *e == '?'   || *e == '*'  || *e == '$'    ||
	    *e == '-'   || *e == '!'  || *e == '@')
	    e++;
	else if (idigit(*e))
	    while (idigit(*e))
		e++;
	else if (iident(*e))
	    while (iident(*e) ||
		   (comppatmatch && *comppatmatch && (*e == Star || *e == Quest)))
		e++;

	/* Now make sure that the cursor is inside the name. */
	if (offs <= e - s && offs >= b - s && n <= 0) {
	    char sav;

	    if (br) {
		p = e;
		while (*p == (test ? Dnull : '"'))
		    p++, parq--, eparq++;
	    }
	    /* It is. */
	    if (test)
		return b;
	    /* If we were called from makecomplistflags(), we have to set the
	     * global variables. */

	    if (set) {
		if (br >= 2) {
		    mflags |= CMF_PARBR;
		    if (nest)
			mflags |= CMF_PARNEST;
		}
		/* Get the prefix (anything up to the character before the name). */
		isuf = dupstring(e);
		untokenize(isuf);
		sav = *b;
		*b = *e = '\0';
		ripre = dyncat((ripre ? ripre : ""), s);
		ipre = dyncat((ipre ? ipre : ""), s);
		*b = sav;

		untokenize(ipre);
	    }
	    /* Save the prefix. */
	    if (compfunc) {
		parflags = (br >= 2 ? CMF_PARBR : 0);
		sav = *b;
		*b = '\0';
		untokenize(parpre = ztrdup(s));
		*b = sav;
	    }
	    /* And adjust wb, we, and offs again. */
	    offs -= b - s;
	    wb = cs - offs;
	    we = wb + e - b;
	    ispar = (br >= 2 ? 2 : 1);
	    b[we-wb] = '\0';
	    return b;
	}
    }
    return NULL;
}

/* Copy the given string and remove backslashes from the copy and return it. */

/**/
mod_export char *
rembslash(char *s)
{
    char *t = s = dupstring(s);

    while (*s)
	if (*s == '\\') {
	    chuck(s);
	    if (*s)
		s++;
	} else
	    s++;

    return t;
}

/* This should probably be moved into tokenize(). */

/**/
mod_export char *
ctokenize(char *p)
{
    char *r = p;
    int bslash = 0;

    tokenize(p);

    for (p = r; *p; p++) {
	if (*p == '\\')
	    bslash = 1;
	else {
	    if (*p == '$' || *p == '{' || *p == '}') {
		if (bslash)
		    p[-1] = Bnull;
		else
		    *p = (*p == '$' ? String :
			  (*p == '{' ? Inbrace : Outbrace));
	    }
	    bslash = 0;
	}
    }
    return r;
}

/**/
mod_export char *
comp_str(int *ipl, int *pl, int untok)
{
    char *p = dupstring(compprefix);
    char *s = dupstring(compsuffix);
    char *ip = dupstring(compiprefix);
    char *str;
    int lp, ls, lip;

    if (!untok) {
	ctokenize(p);
	remnulargs(p);
	ctokenize(s);
	remnulargs(s);
    }
    lp = strlen(p);
    ls = strlen(s);
    lip = strlen(ip);
    str = zhalloc(lip + lp + ls + 1);
    strcpy(str, ip);
    strcat(str, p);
    strcat(str, s);

    if (ipl)
	*ipl = lip;
    if (pl)
	*pl = lp;

    return str;
}

/**/
int
set_comp_sep(void)
{
    int lip, lp;
    char *s = comp_str(&lip, &lp, 1);
    LinkList foo = newlinklist();
    LinkNode n;
    int owe = we, owb = wb, ocs = cs, swb, swe, scs, soffs, ne = noerrs;
    int tl, got = 0, i = 0, cur = -1, oll = ll, sl, remq;
    int ois = instring, oib = inbackt, noffs = lip + lp;
    char *tmp, *p, *ns, *ol = (char *) line, sav, *qp, *qs, *ts, qc = '\0';

    if (compisuffix)
	s = dyncat(s, compisuffix);
    untokenize(s);

    swb = swe = soffs = 0;
    ns = NULL;

    /* Put the string in the lexer buffer and call the lexer to *
     * get the words we have to expand.                        */
    zleparse = 1;
    addedx = 1;
    noerrs = 1;
    lexsave();
    tmp = (char *) zhalloc(tl = 3 + strlen(s));
    tmp[0] = ' ';
    memcpy(tmp + 1, s, noffs);
    tmp[(scs = cs = 1 + noffs)] = 'x';
    strcpy(tmp + 2 + noffs, s + noffs);
    if ((remq = (*compqstack == '\\')))
	tmp = rembslash(tmp);
    inpush(dupstrspace(tmp), 0, NULL);
    line = (unsigned char *) tmp;
    ll = tl - 1;
    strinbeg(0);
    noaliases = 1;
    do {
	ctxtlex();
	if (tok == LEXERR) {
	    int j;

	    if (!tokstr)
		break;
	    for (j = 0, p = tokstr; *p; p++)
		if (*p == Snull || *p == Dnull)
		    j++;
	    if (j & 1) {
		tok = STRING;
		if (p > tokstr && p[-1] == ' ')
		    p[-1] = '\0';
	    }
	}
	if (tok == ENDINPUT || tok == LEXERR)
	    break;
	if (tokstr && *tokstr)
	    addlinknode(foo, (p = ztrdup(tokstr)));
	else
	    p = NULL;
	if (!got && !zleparse) {
	    DPUTS(!p, "no current word in substr");
	    got = 1;
	    cur = i;
	    swb = wb - 1;
	    swe = we - 1;
	    soffs = cs - swb;
	    chuck(p + soffs);
	    ns = dupstring(p);
	}
	i++;
    } while (tok != ENDINPUT && tok != LEXERR);
    noaliases = 0;
    strinend();
    inpop();
    errflag = zleparse = 0;
    noerrs = ne;
    lexrestore();
    wb = owb;
    we = owe;
    cs = ocs;
    line = (unsigned char *) ol;
    ll = oll;
    if (cur < 0 || i < 1)
	return 1;
    owb = offs;
    offs = soffs;
    if ((p = check_param(ns, 0, 1))) {
	for (p = ns; *p; p++)
	    if (*p == Dnull)
		*p = '"';
	    else if (*p == Snull)
		*p = '\'';
    }
    offs = owb;

    untokenize(ts = dupstring(ns));

    if (*ns == Snull || *ns == Dnull) {
	instring = (*ns == Snull ? 1 : 2);
	inbackt = 0;
	swb++;
	if (ns[strlen(ns) - 1] == *ns && ns[1])
	    swe--;
	zsfree(autoq);
	autoq = ztrdup(compqstack[1] ? "" :
		       multiquote(*ns == Snull ? "'" : "\"", 1));
	qc = (*ns == Snull ? '\'' : '"');
	ts++;
    } else {
	instring = 0;
	zsfree(autoq);
	autoq = NULL;
    }
    for (p = ns, i = swb; *p; p++, i++) {
	if (INULL(*p)) {
	    if (i < scs) {
		if (remq && *p == Bnull && p[1])
		    swb -= 2;
	    }
	    if (p[1] || *p != Bnull) {
		if (*p == Bnull) {
		    if (scs == i + 1)
			scs++, soffs++;
		} else {
		    if (scs > i--)
			scs--;
		}
	    } else {
		if (scs == swe)
		    scs--;
	    }
	    chuck(p--);
	}
    }
    ns = ts;

    if (instring && strchr(compqstack, '\\')) {
	int rl = strlen(ns), ql = strlen(multiquote(ns, !!compqstack[1]));

	if (ql > rl)
	    swb -= ql - rl;
    }
    sav = s[(i = swb - 1)];
    s[i] = '\0';
    qp = rembslash(s);
    s[i] = sav;
    if (swe < swb)
	swe = swb;
    swe--;
    sl = strlen(s);
    if (swe > sl) {
	swe = sl;
	if (strlen(ns) > swe - swb + 1)
	    ns[swe - swb + 1] = '\0';
    }
    qs = rembslash(s + swe);
    sl = strlen(ns);
    if (soffs > sl)
	soffs = sl;

    {
	int set = CP_QUOTE | CP_QUOTING, unset = 0;

	p = tricat((instring ? (instring == 1 ? "'" : "\"") : "\\"),
		   compqstack, "");
	zsfree(compqstack);
	compqstack = p;

	zsfree(compquote);
	zsfree(compquoting);
	if (instring == 2) {
	    compquote = "\"";
	    compquoting = "double";
	} else if (instring == 1) {
	    compquote = "'";
	    compquoting = "single";
	} else {
	    compquote = compquoting = "";
	    unset = set;
	    set = 0;
	}
	compquote = ztrdup(compquote);
	compquoting = ztrdup(compquoting);
	comp_setunset(0, 0, set, unset);

	zsfree(compprefix);
	zsfree(compsuffix);
	if (unset(COMPLETEINWORD)) {
	    untokenize(ns);
	    compprefix = ztrdup(ns);
	    compsuffix = ztrdup("");
	} else {
	    char *ss, sav;
	    
	    ss = ns + soffs;

	    sav = *ss;
	    *ss = '\0';
	    untokenize(ns);
	    compprefix = ztrdup(ns);
	    *ss = sav;
	    untokenize(ss);
	    compsuffix = ztrdup(ss);
	}
	zsfree(compiprefix);
	compiprefix = ztrdup("");
	zsfree(compisuffix);
	compisuffix = ztrdup("");
	tmp = tricat(compqiprefix, "", multiquote(qp, 1));
	zsfree(compqiprefix);
	compqiprefix = tmp;
	tmp = tricat(multiquote(qs, 1), "", compqisuffix);
	zsfree(compqisuffix);
	compqisuffix = tmp;
	freearray(compwords);
	i = countlinknodes(foo);
	compwords = (char **) zalloc((i + 1) * sizeof(char *));
	for (n = firstnode(foo), i = 0; n; incnode(n), i++) {
	    p = compwords[i] = (char *) getdata(n);
	    untokenize(p);
	}
	compcurrent = cur + 1;
	compwords[i] = NULL;
    }
    instring = ois;
    inbackt = oib;

    return 0;
}

/* This stores the strings from the list in an array. */

/**/
mod_export void
set_list_array(char *name, LinkList l)
{
    char **a, **p;
    LinkNode n;

    a = (char **) zalloc((countlinknodes(l) + 1) * sizeof(char *));
    for (p = a, n = firstnode(l); n; incnode(n))
	*p++ = ztrdup((char *) getdata(n));
    *p = NULL;

    setaparam(name, a);
}

/* Get the words from a variable or a (list of words). */

/**/
mod_export char **
get_user_var(char *nam)
{
    if (!nam)
	return NULL;
    else if (*nam == '(') {
	/* It's a (...) list, not a parameter name. */
	char *ptr, *s, **uarr, **aptr;
	int count = 0, notempty = 0, brk = 0;
	LinkList arrlist = newlinklist();

	ptr = dupstring(nam);
	s = ptr + 1;
	while (*++ptr) {
	    if (*ptr == '\\' && ptr[1])
		chuck(ptr), notempty = 1;
	    else if (*ptr == ',' || inblank(*ptr) || *ptr == ')') {
		if (*ptr == ')')
		    brk++;
		if (notempty) {
		    *ptr = '\0';
		    count++;
		    if (*s == '\n')
			s++;
		    addlinknode(arrlist, s);
		}
		s = ptr + 1;
		notempty = 0;
	    } else {
		notempty = 1;
		if (*ptr == Meta)
		    ptr++;
	    }
	    if (brk)
		break;
	}
	if (!brk || !count)
	    return NULL;
	*ptr = '\0';
	aptr = uarr = (char **) zhalloc(sizeof(char *) * (count + 1));

	while ((*aptr++ = (char *)ugetnode(arrlist)));
	uarr[count] = NULL;
	return uarr;
    } else {
	/* Otherwise it should be a parameter name. */
	char **arr = NULL, *val;

	if ((arr = getaparam(nam)) || (arr = gethparam(nam)))
	    return (incompfunc ? arrdup(arr) : arr);

	if ((val = getsparam(nam))) {
	    arr = (char **) zhalloc(2*sizeof(char *));
	    arr[0] = (incompfunc ? dupstring(val) : val);
	    arr[1] = NULL;
	}
	return arr;
    }
}

/* This is used by compadd to add a couple of matches. The arguments are
 * the strings given via options. The last argument is the array with
 * the matches. */

/**/
int
addmatches(Cadata dat, char **argv)
{
    char *s, *ms, *lipre = NULL, *lisuf = NULL, *lpre = NULL, *lsuf = NULL;
    char **aign = NULL, **dparr = NULL, *oaq = autoq, *oppre = dat->ppre;
    char *oqp = qipre, *oqs = qisuf, qc, **disp = NULL;
    int lpl, lsl, pl, sl, bcp = 0, bcs = 0, bpadd = 0, bsadd = 0;
    int llpl = 0, llsl = 0, nm = mnum, gflags = 0, ohp = haspattern;
    int oisalt = 0, isalt, isexact, doadd, ois = instring, oib = inbackt;
    Cline lc = NULL, pline = NULL, sline = NULL;
    Cmatch cm;
    struct cmlist mst;
    Cmlist oms = mstack;
    Patprog cp = NULL;
    LinkList aparl = NULL, oparl = NULL, dparl = NULL;
    Brinfo bp, bpl = brbeg, obpl, bsl = brend, obsl;

    if (!*argv) {
	SWITCHHEAPS(compheap) {
	    HEAPALLOC {
		/* Select the group in which to store the matches. */
		gflags = (((dat->aflags & CAF_NOSORT ) ? CGF_NOSORT  : 0) |
			  ((dat->aflags & CAF_UNIQALL) ? CGF_UNIQALL : 0) |
			  ((dat->aflags & CAF_UNIQCON) ? CGF_UNIQCON : 0));
		if (dat->group) {
		    endcmgroup(NULL);
		    begcmgroup(dat->group, gflags);
		} else {
		    endcmgroup(NULL);
		    begcmgroup("default", 0);
		}
	    } LASTALLOC;
	} SWITCHBACKHEAPS;

	return 1;
    }
    for (bp = brbeg; bp; bp = bp->next)
	bp->curpos = ((dat->aflags & CAF_QUOTE) ? bp->pos : bp->qpos);
    for (bp = brend; bp; bp = bp->next)
	bp->curpos = ((dat->aflags & CAF_QUOTE) ? bp->pos : bp->qpos);

    if (dat->flags & CMF_ISPAR)
	dat->flags |= parflags;
    if (compquote && (qc = *compquote)) {
	if (qc == '`') {
	    instring = 0;
	    inbackt = 0;
	    autoq = "";
	} else {
	    char buf[2];

	    instring = (qc == '\'' ? 1 : 2);
	    inbackt = 0;
	    buf[0] = qc;
	    buf[1] = '\0';
	    autoq = multiquote(buf, 1);
	}
    } else {
	instring = inbackt = 0;
	autoq = NULL;
    }
    qipre = ztrdup(compqiprefix ? compqiprefix : "");
    qisuf = ztrdup(compqisuffix ? compqisuffix : "");

    useexact = (compexact && !strcmp(compexact, "accept"));

    /* Switch back to the heap that was used when the completion widget
     * was invoked. */
    SWITCHHEAPS(compheap) {
	HEAPALLOC {
	    if ((doadd = (!dat->apar && !dat->opar && !dat->dpar)) &&
		(dat->aflags & CAF_MATCH))
		hasmatched = 1;
	    if (dat->apar)
		aparl = newlinklist();
	    if (dat->opar)
		oparl = newlinklist();
	    if (dat->dpar) {
		if (*(dat->dpar) == '(')
		    dparr = NULL;
		else if ((dparr = get_user_var(dat->dpar)) && !*dparr)
		    dparr = NULL;
		dparl = newlinklist();
	    }
	    if (dat->exp) {
		curexpl = (Cexpl) zhalloc(sizeof(struct cexpl));
		curexpl->count = curexpl->fcount = 0;
		curexpl->str = dupstring(dat->exp);
	    } else
		curexpl = NULL;

	    /* Store the matcher in our stack of matchers. */
	    if (dat->match) {
		mst.next = mstack;
		mst.matcher = dat->match;
		mstack = &mst;

		if (!mnum)
		    add_bmatchers(dat->match);

		addlinknode(matchers, dat->match);
		dat->match->refc++;
	    }
	    if (mnum && (mstack || bmatchers))
		update_bmatchers();

	    /* Get the suffixes to ignore. */
	    if (dat->ign)
		aign = get_user_var(dat->ign);
	    /* Get the display strings. */
	    if (dat->disp)
		if ((disp = get_user_var(dat->disp)))
		    disp--;
	    /* Get the contents of the completion variables if we have
	     * to perform matching. */
	    if (dat->aflags & CAF_MATCH) {
		lipre = dupstring(compiprefix);
		lisuf = dupstring(compisuffix);
		lpre = dupstring(compprefix);
		lsuf = dupstring(compsuffix);
		llpl = strlen(lpre);
		llsl = strlen(lsuf);
		/* Test if there is an existing -P prefix. */
		if (dat->pre && *dat->pre) {
		    pl = pfxlen(dat->pre, lpre);
		    llpl -= pl;
		    lpre += pl;
		}
	    }
	    /* Now duplicate the strings we have from the command line. */
	    if (dat->ipre)
		dat->ipre = (lipre ? dyncat(lipre, dat->ipre) :
			     dupstring(dat->ipre));
	    else if (lipre)
		dat->ipre = lipre;
	    if (dat->isuf)
		dat->isuf = (lisuf ? dyncat(lisuf, dat->isuf) :
			     dupstring(dat->isuf));
	    else if (lisuf)
		dat->isuf = lisuf;
	    if (dat->ppre) {
		dat->ppre = ((dat->flags & CMF_FILE) ?
			     tildequote(dat->ppre,
					!!(dat->aflags & CAF_QUOTE)) :
			     multiquote(dat->ppre,
					!!(dat->aflags & CAF_QUOTE)));
		lpl = strlen(dat->ppre);
	    } else
		lpl = 0;
	    if (dat->psuf) {
		dat->psuf = multiquote(dat->psuf, !!(dat->aflags & CAF_QUOTE));
		lsl = strlen(dat->psuf);
	    } else
		lsl = 0;
	    if (dat->aflags & CAF_MATCH) {
		int ml, gfl = 0;
		char *globflag = NULL;

		if (comppatmatch && *comppatmatch &&
		    dat->ppre && lpre[0] == '(' && lpre[1] == '#') {
		    char *p;

		    for (p = lpre + 2; *p && *p != ')'; p++);

		    if (*p == ')') {
			char sav = p[1];

			p[1] = '\0';
			globflag = dupstring(lpre);
			gfl = p - lpre + 1;
			p[1] = sav;

			lpre = p + 1;
			llpl -= gfl;
		    }
		}
		s = dat->ppre ? dat->ppre : "";
		if ((ml = match_str(lpre, s, &bpl, 0, NULL, 0, 0, 1)) >= 0) {
		    if (matchsubs) {
			Cline tmp = get_cline(NULL, 0, NULL, 0, NULL, 0, 0);

			tmp->prefix = matchsubs;
			if (matchlastpart)
			    matchlastpart->next = tmp;
			else
			    matchparts = tmp;
		    }
		    pline = matchparts;
		    lpre += ml;
		    llpl -= ml;
		    bcp = ml;
		    bpadd = strlen(s) - ml;
		} else {
		    if (llpl <= lpl && strpfx(lpre, s))
			lpre = "";
		    else if (llpl > lpl && strpfx(s, lpre))
			lpre += lpl;
		    else
			*argv = NULL;
		    bcp = lpl;
		}
		s = dat->psuf ? dat->psuf : "";
		if ((ml = match_str(lsuf, s, &bsl, 0, NULL, 1, 0, 1)) >= 0) {
		    if (matchsubs) {
			Cline tmp = get_cline(NULL, 0, NULL, 0, NULL, 0,
					      CLF_SUF);

			tmp->suffix = matchsubs;
			if (matchlastpart)
			    matchlastpart->next = tmp;
			else
			    matchparts = tmp;
		    }
		    sline = revert_cline(matchparts);
		    lsuf[llsl - ml] = '\0';
		    llsl -= ml;
		    bcs = ml;
		    bsadd = strlen(s) - ml;
		} else {
		    if (llsl <= lsl && strsfx(lsuf, s))
			lsuf = "";
		    else if (llsl > lsl && strsfx(s, lsuf))
			lsuf[llsl - lsl] = '\0';
		    else
			*argv = NULL;
		    bcs = lsl;
		}
		if (comppatmatch && *comppatmatch) {
		    int is = (*comppatmatch == '*');
		    char *tmp = (char *) zhalloc(2 + llpl + llsl + gfl);

		    if (gfl) {
			strcpy(tmp, globflag);
			strcat(tmp, lpre);
		    } else
			strcpy(tmp, lpre);
		    tmp[llpl + gfl] = 'x';
		    strcpy(tmp + llpl + gfl + is, lsuf);

		    tokenize(tmp);
		    remnulargs(tmp);
		    if (haswilds(tmp)) {
			if (is)
			    tmp[llpl + gfl] = Star;
			if ((cp = patcompile(tmp, 0, NULL)))
			    haspattern = 1;
		    }
		}
	    }
	    /* Select the group in which to store the matches. */
	    gflags = (((dat->aflags & CAF_NOSORT ) ? CGF_NOSORT  : 0) |
		      ((dat->aflags & CAF_UNIQALL) ? CGF_UNIQALL : 0) |
		      ((dat->aflags & CAF_UNIQCON) ? CGF_UNIQCON : 0));
	    if (dat->group) {
		endcmgroup(NULL);
		begcmgroup(dat->group, gflags);
	    } else {
		endcmgroup(NULL);
		begcmgroup("default", 0);
	    }
	    if (*argv) {
		if (dat->pre)
		    dat->pre = dupstring(dat->pre);
		if (dat->suf)
		    dat->suf = dupstring(dat->suf);
		if (!dat->prpre && (dat->prpre = oppre)) {
		    singsub(&(dat->prpre));
		    untokenize(dat->prpre);
		} else
		    dat->prpre = dupstring(dat->prpre);
		/* Select the set of matches. */
		oisalt = (dat->aflags & CAF_ALT);

		if (dat->remf) {
		    dat->remf = dupstring(dat->remf);
		    dat->rems = NULL;
		} else if (dat->rems)
		    dat->rems = dupstring(dat->rems);

		if (lpre)
		    lpre = ((!(dat->aflags & CAF_QUOTE) &&
			     (!dat->ppre && (dat->flags & CMF_FILE))) ?
			    tildequote(lpre, 1) : multiquote(lpre, 1));
		if (lsuf)
		    lsuf = multiquote(lsuf, 1);
	    }
	    /* Walk through the matches given. */
	    obpl = bpl;
	    obsl = bsl;
	    for (; (s = *argv); argv++) {
		bpl = obpl;
		bsl = obsl;
		if (disp) {
		    if (!*++disp)
			disp = NULL;
		}
		sl = strlen(s);
		isalt = oisalt;
		if (doadd && (!dat->psuf || !*(dat->psuf)) && aign) {
		    /* Do the suffix-test. If the match has one of the
		     * suffixes from ign, we put it in the alternate set. */
		    char **pt = aign;
		    int filell;

		    for (isalt = 0; !isalt && *pt; pt++)
			if ((filell = strlen(*pt)) < sl
			    && !strcmp(*pt, s + sl - filell))
			    isalt = 1;
		}
		if (!(dat->aflags & CAF_MATCH)) {
		    if (dat->aflags & CAF_QUOTE)
			ms = dupstring(s);
		    else
			sl = strlen(ms = multiquote(s, 0));
		    lc = bld_parts(ms, sl, -1, NULL);
		    isexact = 0;
		} else if (!(ms = comp_match(lpre, lsuf, s, cp, &lc,
					     (!(dat->aflags & CAF_QUOTE) ?
					      (dat->ppre ||
					       !(dat->flags & CMF_FILE) ? 1 : 2) : 0),
					     &bpl, bcp, &bsl, bcs,
					     &isexact))) {
		    if (dparr && !*++dparr)
			dparr = NULL;
		    continue;
		}
		if (doadd) {
		    Brinfo bp;

		    for (bp = obpl; bp; bp = bp->next)
			bp->curpos += bpadd;
		    for (bp = obsl; bp; bp = bp->next)
			bp->curpos += bsadd;

		    if ((cm = add_match_data(isalt, ms, lc, dat->ipre, NULL,
					     dat->isuf, dat->pre, dat->prpre,
					     dat->ppre, pline,
					     dat->psuf, sline,
					     dat->suf, dat->flags, isexact))) {
			cm->rems = dat->rems;
			cm->remf = dat->remf;
			if (disp)
			    cm->disp = dupstring(*disp);
		    }
		} else {
		    if (dat->apar)
			addlinknode(aparl, ms);
		    if (dat->opar)
			addlinknode(oparl, s);
		    if (dat->dpar && dparr) {
			addlinknode(dparl, *dparr);
			if (!*++dparr)
			    dparr = NULL;
		    }
		    free_cline(lc);
		}
	    }
	    if (dat->apar)
		set_list_array(dat->apar, aparl);
	    if (dat->opar)
		set_list_array(dat->opar, oparl);
	    if (dat->dpar)
		set_list_array(dat->dpar, dparl);
	    if (dat->exp)
		addexpl();
	} LASTALLOC;
    } SWITCHBACKHEAPS;

    /* We switched back to the current heap, now restore the stack of
     * matchers. */
    mstack = oms;

    instring = ois;
    inbackt = oib;
    autoq = oaq;
    zsfree(qipre);
    zsfree(qisuf);
    qipre = oqp;
    qisuf = oqs;

    if (mnum == nm)
	haspattern = ohp;

    return (mnum == nm);
}

/* This adds all the data we have for a match. */

/**/
mod_export Cmatch
add_match_data(int alt, char *str, Cline line,
	       char *ipre, char *ripre, char *isuf,
	       char *pre, char *prpre,
	       char *ppre, Cline pline,
	       char *psuf, Cline sline,
	       char *suf, int flags, int exact)
{
    Cmatch cm;
    Aminfo ai = (alt ? fainfo : ainfo);
    int palen, salen, qipl, ipl, pl, ppl, qisl, isl, psl;
    int sl, lpl, lsl, ml;

    palen = salen = qipl = ipl = pl = ppl = qisl = isl = psl = 0;

    DPUTS(!line, "BUG: add_match_data() without cline");

    cline_matched(line);
    if (pline)
	cline_matched(pline);
    if (sline)
	cline_matched(sline);

    /* If there is a path suffix, we build a cline list for it and
     * append it to the list for the match itself. */
    if (!sline && psuf)
	salen = (psl = strlen(psuf));
    if (isuf)
	salen += (isl = strlen(isuf));
    if (qisuf)
	salen += (qisl = strlen(qisuf));

    if (salen) {
	char *asuf = (char *) zhalloc(salen);
	Cline pp, p, s, sl = NULL;
	

	if (psl)
	    memcpy(asuf, psuf, psl);
	if (isl)
	    memcpy(asuf + psl, isuf, isl);
	if (qisl)
	    memcpy(asuf + psl + isl, qisuf, qisl);

	for (pp = NULL, p = line; p->next; pp = p, p = p->next);

	if (salen > qisl) {
	    s = bld_parts(asuf, salen - qisl, salen - qisl, &sl);

	    if (sline) {
		Cline sp;

		sline = cp_cline(sline, 1);

		for (sp = sline; sp->next; sp = sp->next);
		sp->next = s;
		s = sline;
	    }
	    if (!(p->flags & (CLF_SUF | CLF_MID)) &&
		!p->llen && !p->wlen && !p->olen) {
		if (p->prefix) {
		    Cline q;

		    for (q = p->prefix; q->next; q = q->next);
		    q->next = s->prefix;
		    s->prefix = p->prefix;
		    p->prefix = NULL;
		}
		s->flags |= (p->flags & CLF_MATCHED);
		free_cline(p);
		if (pp)
		    pp->next = s;
		else
		    line = s;
	    } else
		p->next = s;
	}
	if (qisl) {
	    Cline qsl = bld_parts(asuf + psl + isl, qisl, qisl, NULL);

	    qsl->flags |= CLF_SUF;
	    qsl->suffix = qsl->prefix;
	    qsl->prefix = NULL;
	    if (sl)
		sl->next = qsl;
	    else if (sline) {
		Cline sp;

		sline = cp_cline(sline, 1);

		for (sp = sline; sp->next; sp = sp->next);
		sp->next = qsl;
		p->next = sline;
	    } else
		p->next = qsl;
	}
    } else if (sline) {
	Cline p;

	for (p = line; p->next; p = p->next);
	p->next = cp_cline(sline, 1);
    }
    /* The prefix is handled differently because the completion code
     * is much more eager to insert the -P prefix than it is to insert
     * the -S suffix. */
    if (qipre)
	palen = (qipl = strlen(qipre));
    if (ipre)
	palen += (ipl = strlen(ipre));
    if (pre)
	palen += (pl = strlen(pre));
    if (!pline && ppre)
	palen += (ppl = strlen(ppre));

    if (pl) {
	if (ppl || pline) {
	    Cline lp, p;

	    if (pline)
		for (p = cp_cline(pline, 1), lp = p; lp->next; lp = lp->next);
	    else
		p = bld_parts(ppre, ppl, ppl, &lp);

	    if (lp->prefix && !(line->flags & (CLF_SUF | CLF_MID)) &&
		!lp->llen && !lp->wlen && !lp->olen) {
		Cline lpp;

		for (lpp = lp->prefix; lpp->next; lpp = lpp->next);

		lpp->next = line->prefix;
		line->prefix = lp->prefix;
		lp->prefix = NULL;

		free_cline(lp);

		if (p != lp) {
		    Cline q;

		    for (q = p; q->next != lp; q = q->next);

		    q->next = line;
		    line = p;
		}
	    } else {
		lp->next = line;
		line = p;
	    }
	}
	if (pl) {
	    Cline lp, p = bld_parts(pre, pl, pl, &lp);

	    lp->next = line;
	    line = p;
	}
	if (ipl) {
	    Cline lp, p = bld_parts(ipre, ipl, ipl, &lp);

	    lp->next = line;
	    line = p;
	}
	if (qipl) {
	    Cline lp, p = bld_parts(qipre, qipl, qipl, &lp);

	    lp->next = line;
	    line = p;
	}
    } else if (palen || pline) {
	Cline p, lp;

	if (palen) {
	    char *apre = (char *) zhalloc(palen);

	    if (qipl)
		memcpy(apre, qipre, qipl);
	    if (ipl)
		memcpy(apre + qipl, ipre, ipl);
	    if (pl)
		memcpy(apre + qipl + ipl, pre, pl);
	    if (ppl)
		memcpy(apre + qipl + ipl + pl, ppre, ppl);

	    p = bld_parts(apre, palen, palen, &lp);

	    if (pline)
		for (lp->next = cp_cline(pline, 1); lp->next; lp = lp->next);
	} else
	    for (p = lp = cp_cline(pline, 1); lp->next; lp = lp->next);

	if (lp->prefix && !(line->flags & (CLF_SUF | CLF_MID)) &&
	    !lp->llen && !lp->wlen && !lp->olen) {
	    Cline lpp;

	    for (lpp = lp->prefix; lpp->next; lpp = lpp->next);

	    lpp->next = line->prefix;
	    line->prefix = lp->prefix;
	    lp->prefix = NULL;

	    free_cline(lp);

	    if (p != lp) {
		Cline q;

		for (q = p; q->next != lp; q = q->next);

		q->next = line;
		line = p;
	    }
	} else {
	    lp->next = line;
	    line = p;
	}
    }
    /* Allocate and fill the match structure. */
    cm = (Cmatch) zhalloc(sizeof(struct cmatch));
    cm->str = str;
    cm->ppre = (ppre && *ppre ? ppre : NULL);
    cm->psuf = (psuf && *psuf ? psuf : NULL);
    cm->prpre = ((flags & CMF_FILE) && prpre && *prpre ? prpre : NULL);
    if (qipre && *qipre)
	cm->ipre = (ipre && *ipre ? dyncat(qipre, ipre) : dupstring(qipre));
    else
	cm->ipre = (ipre && *ipre ? ipre : NULL);
    cm->ripre = (ripre && *ripre ? ripre : NULL);
    if (qisuf && *qisuf)
	cm->isuf = (isuf && *isuf ? dyncat(isuf, qisuf) : dupstring(qisuf));
    else
	cm->isuf = (isuf && *isuf ? isuf : NULL);
    cm->pre = pre;
    cm->suf = suf;
    cm->flags = (flags |
		 (complist ?
		  ((strstr(complist, "packed") ? CMF_PACKED : 0) |
		   (strstr(complist, "rows")   ? CMF_ROWS   : 0)) : 0));

    if ((*compqstack == '\\' && compqstack[1]) ||
	(autoq && *compqstack && compqstack[1] == '\\'))
	cm->flags |= CMF_NOSPACE;
    if (nbrbeg) {
	int *p;
	Brinfo bp;

	cm->brpl = (int *) zhalloc(nbrbeg * sizeof(int));

	for (p = cm->brpl, bp = brbeg; bp; p++, bp = bp->next)
	    *p = bp->curpos;
    } else
	cm->brpl = NULL;
    if (nbrend) {
	int *p;
	Brinfo bp;

	cm->brsl = (int *) zhalloc(nbrend * sizeof(int));

	for (p = cm->brsl, bp = brend; bp; p++, bp = bp->next)
	    *p = bp->curpos;
    } else
	cm->brsl = NULL;
    cm->qipl = qipl;
    cm->qisl = qisl;
    cm->autoq = dupstring(autoq ? autoq : (inbackt ? "`" : NULL));
    cm->rems = cm->remf = cm->disp = NULL;

    if ((lastprebr || lastpostbr) && !hasbrpsfx(cm, lastprebr, lastpostbr))
	return NULL;

    /* Then build the unambiguous cline list. */
    ai->line = join_clines(ai->line, line);

    mnum++;
    ai->count++;

    addlinknode((alt ? fmatches : matches), cm);

    newmatches = 1;

    if (!complastprompt || !*complastprompt)
	dolastprompt = 0;
    /* One more match for this explanation. */
    if (curexpl) {
	if (alt)
	    curexpl->fcount++;
	else
	    curexpl->count++;
    }
    if (!ai->firstm)
	ai->firstm = cm;

    sl = strlen(str);
    lpl = (cm->ppre ? strlen(cm->ppre) : 0);
    lsl = (cm->psuf ? strlen(cm->psuf) : 0);
    ml = sl + lpl + lsl;

    if (ml < minmlen)
	minmlen = ml;
    if (ml > maxmlen)
	maxmlen = ml;

    /* Do we have an exact match? More than one? */
    if (exact) {
	if (!ai->exact) {
	    ai->exact = useexact;
	    if (incompfunc && (!compexactstr || !*compexactstr)) {
		/* If a completion widget is active, we make the exact
		 * string available in `compstate'. */

		char *e;

		zsfree(compexactstr);
		compexactstr = e = (char *) zalloc(ml + 1);
		if (cm->ppre) {
		    strcpy(e, cm->ppre);
		    e += lpl;
		}
		strcpy(e, str);
		e += sl;
		if (cm->psuf)
		    strcpy(e, cm->psuf);
		comp_setunset(0, 0, CP_EXACTSTR, 0);
	    }
	    ai->exactm = cm;
	} else if (useexact) {
	    ai->exact = 2;
	    ai->exactm = NULL;
	    if (incompfunc)
		comp_setunset(0, 0, 0, CP_EXACTSTR);
	}
    }
    return cm;
}

/* This begins a new group of matches. */

/**/
mod_export void
begcmgroup(char *n, int flags)
{
    if (n) {
	Cmgroup p = amatches;

	while (p) {
	    if (p->name &&
		flags == (p->flags & (CGF_NOSORT|CGF_UNIQALL|CGF_UNIQCON)) &&
		!strcmp(n, p->name)) {
		mgroup = p;

		expls = p->lexpls;
		matches = p->lmatches;
		fmatches = p->lfmatches;
		allccs = p->lallccs;

		return;
	    }
	    p = p->next;
	}
    }
    mgroup = (Cmgroup) zhalloc(sizeof(struct cmgroup));
    mgroup->name = dupstring(n);
    mgroup->lcount = mgroup->llcount = mgroup->mcount = 0;
    mgroup->flags = flags;
    mgroup->matches = NULL;
    mgroup->ylist = NULL;
    mgroup->expls = NULL;

    mgroup->lexpls = expls = newlinklist();
    mgroup->lmatches = matches = newlinklist();
    mgroup->lfmatches = fmatches = newlinklist();

    mgroup->lallccs = allccs = ((flags & CGF_NOSORT) ? NULL : newlinklist());

    mgroup->next = amatches;
    amatches = mgroup;
}

/* End the current group for now. */

/**/
mod_export void
endcmgroup(char **ylist)
{
    mgroup->ylist = ylist;
}

/* Add an explanation string to the current group, joining duplicates. */

/**/
mod_export void
addexpl(void)
{
    LinkNode n;
    Cexpl e;

    for (n = firstnode(expls); n; incnode(n)) {
	e = (Cexpl) getdata(n);
	if (!strcmp(curexpl->str, e->str)) {
	    e->count += curexpl->count;
	    e->fcount += curexpl->fcount;

	    return;
	}
    }
    addlinknode(expls, curexpl);
    newmatches = 1;
}

/* The comparison function for matches (used for sorting). */

/**/
static int
matchcmp(Cmatch *a, Cmatch *b)
{
    if ((*a)->disp) {
	if ((*b)->disp) {
	    if ((*a)->flags & CMF_DISPLINE) {
		if ((*b)->flags & CMF_DISPLINE)
		    return strcmp((*a)->disp, (*b)->disp);
		else
		    return -1;
	    } else {
		if ((*b)->flags & CMF_DISPLINE)
		    return 1;
		else
		    return strcmp((*a)->disp, (*b)->disp);
	    }
	}
	return -1;
    }
    if ((*b)->disp)
	return 1;

    return strbpcmp(&((*a)->str), &((*b)->str));
}

/* This tests whether two matches are equal (would produce the same
 * strings on the command line). */

#define matchstreq(a, b) ((!(a) && !(b)) || ((a) && (b) && !strcmp((a), (b))))

/**/
static int
matcheq(Cmatch a, Cmatch b)
{
    return matchstreq(a->ipre, b->ipre) &&
	matchstreq(a->pre, b->pre) &&
	matchstreq(a->ppre, b->ppre) &&
	matchstreq(a->psuf, b->psuf) &&
	matchstreq(a->suf, b->suf) &&
	((!a->disp && !b->disp && matchstreq(a->str, b->str)) ||
	 (a->disp && b->disp && !strcmp(a->disp, b->disp) &&
	  matchstreq(a->str, b->str)));
}

/* Make an array from a linked list. The second argument says whether *
 * the array should be sorted. The third argument is used to return   *
 * the number of elements in the resulting array. The fourth argument *
 * is used to return the number of NOLIST elements. */

/**/
static Cmatch *
makearray(LinkList l, int type, int flags, int *np, int *nlp, int *llp)
{
    Cmatch *ap, *bp, *cp, *rp;
    LinkNode nod;
    int n, nl = 0, ll = 0;

    /* Build an array for the matches. */
    rp = ap = (Cmatch *) ncalloc(((n = countlinknodes(l)) + 1) *
				 sizeof(Cmatch));

    /* And copy them into it. */
    for (nod = firstnode(l); nod; incnode(nod))
	*ap++ = (Cmatch) getdata(nod);
    *ap = NULL;

    if (!type) {
	if (flags) {
	    char **ap, **bp, **cp;

	    /* Now sort the array (it contains strings). */
	    qsort((void *) rp, n, sizeof(char *),
		  (int (*) _((const void *, const void *)))strbpcmp);

	    /* And delete the ones that occur more than once. */
	    for (ap = cp = (char **) rp; *ap; ap++) {
		*cp++ = *ap;
		for (bp = ap; bp[1] && !strcmp(*ap, bp[1]); bp++, n--);
		ap = bp;
	    }
	    *cp = NULL;
	}
    } else {
	if (!(flags & CGF_NOSORT)) {
	    /* Now sort the array (it contains matches). */
	    qsort((void *) rp, n, sizeof(Cmatch),
		  (int (*) _((const void *, const void *)))matchcmp);

	    if (!(flags & CGF_UNIQCON)) {
		int dup;

		/* And delete the ones that occur more than once. */
		for (ap = cp = rp; *ap; ap++) {
		    *cp++ = *ap;
		    for (bp = ap; bp[1] && matcheq(*ap, bp[1]); bp++, n--);
		    ap = bp;
		    /* Mark those, that would show the same string in the list. */
		    for (dup = 0; bp[1] && !(*ap)->disp && !(bp[1])->disp &&
			     !strcmp((*ap)->str, (bp[1])->str); bp++) {
			(bp[1])->flags |= CMF_MULT;
			dup = 1;
		    }
		    if (dup)
			(*ap)->flags |= CMF_FMULT;
		}
		*cp = NULL;
	    }
	    for (ap = rp; *ap; ap++) {
		if ((*ap)->disp && ((*ap)->flags & CMF_DISPLINE))
		    ll++;
		if ((*ap)->flags & (CMF_NOLIST | CMF_MULT))
		    nl++;
	    }
	} else {
	    if (!(flags & CGF_UNIQALL) && !(flags & CGF_UNIQCON)) {
		for (ap = rp; *ap; ap++) {
		    for (bp = cp = ap + 1; *bp; bp++) {
			if (!matcheq(*ap, *bp))
			    *cp++ = *bp;
			else
			    n--;
		    }
		    *cp = NULL;
		}
	    } else if (!(flags & CGF_UNIQCON)) {
		int dup;

		for (ap = cp = rp; *ap; ap++) {
		    *cp++ = *ap;
		    for (bp = ap; bp[1] && matcheq(*ap, bp[1]); bp++, n--);
		    ap = bp;
		    for (dup = 0; bp[1] && !(*ap)->disp && !(bp[1])->disp &&
			     !strcmp((*ap)->str, (bp[1])->str); bp++) {
			(bp[1])->flags |= CMF_MULT;
			dup = 1;
		    }
		    if (dup)
			(*ap)->flags |= CMF_FMULT;
		}
		*cp = NULL;
	    }
	    for (ap = rp; *ap; ap++) {
		if ((*ap)->disp && ((*ap)->flags & CMF_DISPLINE))
		    ll++;
		if ((*ap)->flags & (CMF_NOLIST | CMF_MULT))
		    nl++;
	    }
	}
    }
    if (np)
	*np = n;
    if (nlp)
	*nlp = nl;
    if (llp)
	*llp = ll;
    return rp;
}

/* This duplicates one match. */

/**/
static Cmatch
dupmatch(Cmatch m, int nbeg, int nend)
{
    Cmatch r;

    r = (Cmatch) ncalloc(sizeof(struct cmatch));

    r->str = ztrdup(m->str);
    r->ipre = ztrdup(m->ipre);
    r->ripre = ztrdup(m->ripre);
    r->isuf = ztrdup(m->isuf);
    r->ppre = ztrdup(m->ppre);
    r->psuf = ztrdup(m->psuf);
    r->prpre = ztrdup(m->prpre);
    r->pre = ztrdup(m->pre);
    r->suf = ztrdup(m->suf);
    r->flags = m->flags;
    if (nbeg) {
	int *p, *q, i;

	r->brpl = (int *) zalloc(nbeg * sizeof(int));

	for (p = r->brpl, q = m->brpl, i = nbeg; i--; p++, q++)
	    *p = *q;
    } else
	r->brpl = NULL;
    if (nend) {
	int *p, *q, i;

	r->brsl = (int *) zalloc(nend * sizeof(int));

	for (p = r->brsl, q = m->brsl, i = nend; i--; p++, q++)
	    *p = *q;
    } else
	r->brsl = NULL;
    r->rems = ztrdup(m->rems);
    r->remf = ztrdup(m->remf);
    r->autoq = ztrdup(m->autoq);
    r->qipl = m->qipl;
    r->qisl = m->qisl;
    r->disp = dupstring(m->disp);

    return r;
}

/* This duplicates all groups of matches. */

/**/
int
permmatches(int last)
{
    Cmgroup g = amatches, n;
    Cmatch *p, *q;
    Cexpl *ep, *eq, e, o;
    LinkList mlist;
    static int fi = 0;
    int nn, nl, ll, gn = 1, mn = 1, rn;

    if (pmatches && !newmatches)
	return fi;

    newmatches = fi = 0;

    if (pmatches)
	freematches(pmatches);

    pmatches = lmatches = NULL;
    nmatches = smatches = 0;

    if (!ainfo->count) {
	if (last)
	    ainfo = fainfo;
	fi = 1;
    }
    while (g) {
	HEAPALLOC {
	    if (empty(g->lmatches))
		/* We have no matches, try ignoring fignore. */
		mlist = g->lfmatches;
	    else
		mlist = g->lmatches;

	    g->matches = makearray(mlist, 1, g->flags, &nn, &nl, &ll);
	    g->mcount = nn;
	    if ((g->lcount = nn - nl) < 0)
		g->lcount = 0;
	    g->llcount = ll;
	    if (g->ylist) {
		g->lcount = arrlen(g->ylist);
		smatches = 2;
	    }
	    g->expls = (Cexpl *) makearray(g->lexpls, 0, 0, &(g->ecount),
					   NULL, NULL);

	    g->ccount = 0;
	} LASTALLOC;

	nmatches += g->mcount;
	smatches += g->lcount;

	n = (Cmgroup) ncalloc(sizeof(struct cmgroup));

	if (!lmatches)
	    lmatches = n;
	if (pmatches)
	    pmatches->prev = n;
	n->next = pmatches;
	pmatches = n;
	n->prev = 0;
	n->num = gn++;

	n->flags = g->flags;
	n->mcount = g->mcount;
	n->matches = p = (Cmatch *) ncalloc((n->mcount + 1) *
					    sizeof(Cmatch));
	n->name = ztrdup(g->name);
	for (q = g->matches; *q; q++, p++)
	    *p = dupmatch(*q, nbrbeg, nbrend);
	*p = NULL;

	n->lcount = g->lcount;
	n->llcount = g->llcount;
	if (g->ylist)
	    n->ylist = arrdup(g->ylist);
	else
	    n->ylist = NULL;

	if ((n->ecount = g->ecount)) {
	    n->expls = ep = (Cexpl *) ncalloc((n->ecount + 1) *
					      sizeof(Cexpl));
	    for (eq = g->expls; (o = *eq); eq++, ep++) {
		*ep = e = (Cexpl) ncalloc(sizeof(struct cexpl));
		e->count = (fi ? o->fcount : o->count);
		e->str = ztrdup(o->str);
	    }
	    *ep = NULL;
	} else
	    n->expls = NULL;

	n->widths = NULL;

	g = g->next;
    }
    for (g = pmatches; g; g = g->next) {
	g->nbrbeg = nbrbeg;
	g->nbrend = nbrend;
	for (rn = 1, q = g->matches; *q; q++) {
	    (*q)->rnum = rn++;
	    (*q)->gnum = mn++;
	}
    }
    hasperm = 1;
    permmnum = mn - 1;
    permgnum = gn - 1;
    listdat.valid = 0;

    return fi;
}

/* This frees one match. */

/**/
static void
freematch(Cmatch m, int nbeg, int nend)
{
    if (!m) return;

    zsfree(m->str);
    zsfree(m->ipre);
    zsfree(m->ripre);
    zsfree(m->isuf);
    zsfree(m->ppre);
    zsfree(m->psuf);
    zsfree(m->pre);
    zsfree(m->suf);
    zsfree(m->prpre);
    zsfree(m->rems);
    zsfree(m->remf);
    zsfree(m->disp);
    zsfree(m->autoq);
    zfree(m->brpl, nbeg * sizeof(int));
    zfree(m->brsl, nend * sizeof(int));

    zfree(m, sizeof(m));
}

/* This frees the groups of matches. */

/**/
mod_export void
freematches(Cmgroup g)
{
    Cmgroup n;
    Cmatch *m;
    Cexpl *e;

    while (g) {
	n = g->next;
	
	for (m = g->matches; *m; m++)
	    freematch(*m, g->nbrbeg, g->nbrend);

	if (g->ylist)
	    freearray(g->ylist);

	if ((e = g->expls)) {
	    while (*e) {
		zsfree((*e)->str);
		free(*e);
		e++;
	    }
	    free(g->expls);
	}
	zsfree(g->name);
	free(g);

	g = n;
    }
}
