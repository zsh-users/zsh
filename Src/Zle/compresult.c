/*
 * compresult.c - the complete module, completion result handling
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
#include "compresult.pro"

/* This counts how often the list of completions was invalidated.
 * Can be used to detect if we have a new list.  */

/**/
mod_export int invcount;

#define inststr(X) inststrlen((X),1,-1)

/* This cuts the cline list before the stuff that isn't worth
 * inserting in the line. */

/**/
static Cline
cut_cline(Cline l)
{
    Cline q, p, e = NULL, maxp = NULL;
    int sum = 0, max = 0, tmp, ls = 0, miss = 0;

    /* If no match was added with matching, we don't really know
     * which parts of the unambiguous string are worth keeping,
     * so for now we keep everything (in the hope that this
     * produces a string containing at least everything that was 
     * originally on the line). */

    if (!hasmatched) {
	cline_setlens(l, 0);
	return l;
    }
    e = l = cp_cline(l, 0);

    /* First, search the last struct for which we have something on
     * the line. Anything before that is kept. */

    for (q = NULL, p = l; p; p = p->next) {
	if (p->orig || p->olen || !(p->flags & CLF_NEW))
	    e = p->next;
	if (!p->suffix && (p->wlen || p->llen || p->prefix))
	    q = p;
    }
    if (!e && q && !q->orig && !q->olen && (q->flags & CLF_MISS) &&
	(!(q->flags & CLF_MATCHED) || (!q->prefix && !q->suffix)) &&
	(q->word ? q->wlen : q->llen) < 3) {
	q->word = q->line = NULL;
	q->wlen = q->llen = 0;
    }
    /* Then keep all structs without missing characters. */

    while (e && !(e->flags & CLF_MISS))
	e = e->next;

    if (e) {
	/* Then we see if there is another struct with missing
	 * characters. If not, we keep the whole list. */

	for (p = e->next; p && !(p->flags & CLF_MISS); p = p->next);

	if (p) {
	    for (p = e; p; p = p->next) {
		if (!(p->flags & CLF_MISS))
		    sum += p->max;
		else {
		    tmp = cline_sublen(p);
		    if (tmp > 2 && tmp > ((p->max + p->min) >> 1))
			sum += tmp - (p->max - tmp);
		    else if (tmp < p->min)
			sum -= (((p->max + p->min) >> 1) - tmp) << (tmp < 2);
		}
		if (sum > max) {
		    max = sum;
		    maxp = p;
		}
	    }
	    if (max)
		e = maxp;
	    else {
		int len = 0;

		cline_setlens(l, 0);
		ls = 1;

		for (p = e; p; p = p->next)
		    len += p->min;

		if (len > ((minmlen << 1) / 3))
		    goto end;
	    }
	    e->line = e->word = NULL;
	    e->llen = e->wlen = e->olen = 0;
	    e->next = NULL;
	}
    }
 end:

    /* Sanity check. If there are no parts with missing characters but
     * parts with joined substrings, remove those. */

    for (p = l, e = 0, tmp = 0; p; p = p->next) {
	if (p->flags & (CLF_MISS|CLF_DIFF))
	    miss = 1;
	for (q = p->prefix; q; q = q->next)
	    if (q->flags & CLF_JOIN) {
		e = p;
		tmp = 0;
		break;
	    }
	for (q = p->suffix; q; q = q->next)
	    if (q->flags & CLF_JOIN) {
		e = p;
		tmp = 1;
		break;
	    }
    }
    if (e && (!miss || cline_sublen(e) == e->min)) {
	for (p = (tmp ? e->suffix : e->prefix);
	     p && p->next && !(p->next->flags & CLF_JOIN); p = p->next);
	if (p)
	    p->next = NULL;
    }
    if (!ls)
	cline_setlens(l, 0);

    return l;
}

/* This builds the unambiguous string. If ins is one, it is immediately
 * inserted into the line. Otherwise csp is used to return the relative
 * cursor position in the string returned and posl contains all 
 * positions with missing or ambiguous characters. If ins is two, csp
 * and posl contain real command line positions (including braces). */

/**/
static char *
cline_str(Cline l, int ins, int *csp, LinkList posl)
{
    Cline s;
    int ocs = cs, ncs, pcs, scs, opos = -1, npos;
    int pm, pmax, pmm, pma, sm, smax, smm, sma, d, dm, mid;
    int i, j, li = 0, cbr, padd = (ins ? wb - ocs : -ocs);
    Brinfo brp, brs;

    l = cut_cline(l);

    pmm = pma = smm = sma = dm = pcs = scs = 0;
    pm = pmax = sm = smax = d = mid = cbr = -1;
    brp = brs = NULL;

    /* Get the information about the brace beginning and end we have
     * to re-insert. */
    if (ins) {
	Brinfo bp;
	int olen = we - wb;

	if ((brp = brbeg)) {
	    for (bp = brbeg; bp; bp = bp->next) {
		bp->curpos = (hasunqu ? bp->pos : bp->qpos);
		olen -= strlen(bp->str);
	    }
	}
	if ((brs = lastbrend)) {
	    for (bp = brend; bp; bp = bp->next)
		olen -= strlen(bp->str);

	    for (bp = brend; bp; bp = bp->next)
		bp->curpos = olen - (hasunqu ? bp->pos : bp->qpos);
	}
	while (brp && !brp->curpos) {
	    inststrlen(brp->str, 1, -1);
	    brp = brp->next;
	}
	while (brs && !brs->curpos) {
	    if (cbr < 0)
		cbr = cs;
	    inststrlen(brs->str, 1, -1);
	    brs = brs->prev;
	}
    }
    /* Walk through the top-level cline list. */
    while (l) {
	/* Insert the original string if no prefix. */
	if (l->olen && !(l->flags & CLF_SUF) && !l->prefix) {
	    pcs = cs + l->olen;
	    inststrlen(l->orig, 1, l->olen);
	} else {
	    /* Otherwise insert the prefix. */
	    for (s = l->prefix; s; s = s->next) {
		pcs = cs + s->llen;
		if (s->flags & CLF_LINE)
		    inststrlen(s->line, 1, s->llen);
		else
		    inststrlen(s->word, 1, s->wlen);
		scs = cs;

		if ((s->flags & CLF_DIFF) && (!dm || (s->flags & CLF_MATCHED))) {
		    d = cs; dm = s->flags & CLF_MATCHED;
		    if (posl && (npos = cs + padd) != opos) {
			opos = npos;
			addlinknode(posl, (void *) ((long) npos));
		    }
		}
		li += s->llen;
	    }
	}
	if (ins) {
	    int ocs, bl;

	    while (brp && li >= brp->curpos) {
		ocs = cs;
		bl = strlen(brp->str);
		cs = pcs - (li - brp->curpos);
		inststrlen(brp->str, 1, bl);
		cs = ocs + bl;
		pcs += bl;
		scs += bl;
		brp = brp->next;
	    }
	}
	/* Remember the position if this is the first prefix with
	 * missing characters. */
	if ((l->flags & CLF_MISS) && !(l->flags & CLF_SUF)) {
	    if (posl && (npos = cs + padd) != opos) {
		opos = npos;
		addlinknode(posl, (void *) ((long) npos));
	    }
	    if (((pmax <= (l->max - l->min) || (pma && l->max != l->min)) &&
		 (!pmm || (l->flags & CLF_MATCHED))) ||
		((l->flags & CLF_MATCHED) && !pmm)) {
		pm = cs; pmax = l->max - l->min; pmm = l->flags & CLF_MATCHED;
		pma = ((l->prefix || l->suffix) && l->min == cline_sublen(l));
	    }
	}
	if (ins) {
	    int ocs, bl;

	    while (brs && li >= brs->curpos) {
		ocs = cs;
		bl = strlen(brs->str);
		cs = scs - (li - brs->curpos);
		if (cbr < 0)
		    cbr = cs;
		inststrlen(brs->str, 1, bl);
		cs = ocs + bl;
		pcs += bl;
		brs = brs->prev;
	    }
	}
	pcs = cs;
	/* Insert the anchor. */
	if (l->flags & CLF_LINE)
	    inststrlen(l->line, 1, l->llen);
	else
	    inststrlen(l->word, 1, l->wlen);
	scs = cs;
	if (ins) {
	    int ocs, bl;

	    li += l->llen;

	    while (brp && li >= brp->curpos) {
		ocs = cs;
		bl = strlen(brp->str);
		cs = pcs + l->llen - (li - brp->curpos);
		inststrlen(brp->str, 1, bl);
		cs = ocs + bl;
		pcs += bl;
		scs += bl;
		brp = brp->next;
	    }
	}
	/* Remember the cursor position for suffixes and mids. */
	if (l->flags & CLF_MISS) {
	    if (l->flags & CLF_MID)
		mid = cs;
	    else if (l->flags & CLF_SUF) {
		if (posl && (npos = cs + padd) != opos) {
		    opos = npos;
		    addlinknode(posl, (void *) ((long) npos));
		}
		if (((smax <= (l->min - l->max) || (sma && l->max != l->min)) &&
		     (!smm || (l->flags & CLF_MATCHED))) ||
		    ((l->flags & CLF_MATCHED) && !smm)) {
		    sm = cs; smax = l->min - l->max; smm = l->flags & CLF_MATCHED;
		    sma = ((l->prefix || l->suffix) && l->min == cline_sublen(l));
		}
	    }
	}
	if (ins) {
	    int ocs, bl;

	    while (brs && li >= brs->curpos) {
		ocs = cs;
		bl = strlen(brs->str);
		cs = scs - (li - brs->curpos);
		if (cbr < 0)
		    cbr = cs;
		inststrlen(brs->str, 1, bl);
		cs = ocs + bl;
		pcs += bl;
		brs = brs->prev;
	    }
	}
	/* And now insert the suffix or the original string. */
	if (l->olen && (l->flags & CLF_SUF) && !l->suffix) {
	    pcs = cs;
	    inststrlen(l->orig, 1, l->olen);
	    if (ins) {
		int ocs, bl;

		li += l->olen;

		while (brp && li >= brp->curpos) {
		    ocs = cs;
		    bl = strlen(brp->str);
		    cs = pcs + l->olen - (li - brp->curpos);
		    inststrlen(brp->str, 1, bl);
		    cs = ocs + bl;
		    pcs += bl;
		    brp = brp->next;
		}
		while (brs && li >= brs->curpos) {
		    ocs = cs;
		    bl = strlen(brs->str);
		    cs = pcs + l->olen - (li - brs->curpos);
		    if (cbr < 0)
			cbr = cs;
		    inststrlen(brs->str, 1, bl);
		    cs = ocs + bl;
		    pcs += bl;
		    brs = brs->prev;
		}
	    }
	} else {
	    Cline js = NULL;

	    for (j = -1, i = 0, s = l->suffix; s; s = s->next) {
		if (j < 0 && (s->flags & CLF_DIFF))
		    j = i, js = s;
		pcs = cs;
		if (s->flags & CLF_LINE) {
		    inststrlen(s->line, 0, s->llen);
		    i += s->llen; scs = cs + s->llen;
		} else {
		    inststrlen(s->word, 0, s->wlen);
		    i += s->wlen; scs = cs + s->wlen;
		}
		if (ins) {
		    int ocs, bl;

		    li += s->llen;

		    while (brp && li >= brp->curpos) {
			ocs = cs;
			bl = strlen(brp->str);
			cs = pcs + (li - brp->curpos);
			inststrlen(brp->str, 1, bl);
			cs = ocs + bl;
			pcs += bl;
			scs += bl;
			brp = brp->next;
		    }
		    while (brs && li >= brs->curpos) {
			ocs = cs;
			bl = strlen(brs->str);
			cs = scs - (li - brs->curpos);
			if (cbr < 0)
			    cbr = cs;
			inststrlen(brs->str, 1, bl);
			cs = ocs + bl;
			pcs += bl;
			brs = brs->prev;
		    }
		}
	    }
	    cs += i;
	    if (j >= 0 && (!dm || (js->flags & CLF_MATCHED))) {
		d = cs - j; dm = js->flags & CLF_MATCHED;
		if (posl && (npos = cs - j + padd) != opos) {
		    opos = npos;
		    addlinknode(posl, (void *) ((long) npos));
		}
	    }
	}
	l = l->next;
    }
    if (posl && (npos = cs + padd) != opos)
#if 0
	/* This could be used to put an extra colon before the end-of-word
	 * position if there is nothing missing. */
	addlinknode(posl, (void *) ((long) -npos));
#endif
	addlinknode(posl, (void *) ((long) npos));

    if (ins) {
	int ocs = cs;

	for (; brp; brp = brp->next)
	    inststrlen(brp->str, 1, -1);
	for (; brs; brs = brs->prev) {
	    if (cbr < 0)
		cbr = cs;
	    inststrlen(brs->str, 1, -1);
	}
	if (mid >= ocs)
	    mid += cs - ocs;
	if (pm >= ocs)
	    pm += cs - ocs;
	if (sm >= ocs)
	    sm += cs - ocs;
	if (d >= ocs)
	    d += cs - ocs;

	if (posl) {
	    LinkNode node;
	    long p;

	    for (node = firstnode(posl); node; incnode(node)) {
		p = (long) getdata(node);
		if (p >= ocs)
		    setdata(node, (void *) (p + cs - ocs));
	    }
	}
    }
    /* This calculates the new cursor position. If we had a mid cline
     * with missing characters, we take this, otherwise if we have a
     * prefix with missing characters, we take that, the same for a
     * suffix, and finally a place where the matches differ. */
    ncs = (mid >= 0 ? mid :
	   (cbr >= 0 ? cbr :
	    (pm >= 0 ? pm : (sm >= 0 ? sm : (d >= 0 ? d : cs)))));

    if (ins != 1) {
	/* We always inserted the string in the line. If that was not
	 * requested, we copy it and remove from the line. */
	char *r = zalloc((i = cs - ocs) + 1);

	memcpy(r, (char *) (line + ocs), i);
	r[i] = '\0';
	cs = ocs;
	foredel(i);

	if (csp)
	    *csp = ncs - ocs;

	return r;
    }
    lastend = cs;
    cs = ncs;

    return NULL;
}

/* Small utility function turning a list of positions into a colon
 * separated string. */

static char *
build_pos_string(LinkList list)
{
    LinkNode node;
    int l;
    char buf[40], *s;
    long p;

    for (node = firstnode(list), l = 0; node; incnode(node)) {
	p = (long) getdata(node);
#if 0
	/* This could be used to put an extra colon before the end-of-word
	 * position if there is nothing missing. */
	if (p < 0)
	    sprintf(buf, ":%ld", -p);
	else
#endif
	    sprintf(buf, "%ld", p);
	setdata(node, dupstring(buf));
	l += 1 + strlen(buf);
    }
    s = (char *) zalloc(l * sizeof(char));
    *s = 0;
    for (node = firstnode(list); node;) {
	strcat(s, (char *) getdata(node));
	incnode(node);
	if (node)
	    strcat(s, ":");
    }
    return s;
}

/* This is a utility function using the function above to allow access
 * to the unambiguous string and cursor position via compstate. */

/**/
char *
unambig_data(int *cp, char **pp, char **ip)
{
    static char *scache = NULL, *pcache = NULL, *icache = NULL;
    static int ccache;

    if (mnum && ainfo) {
	if (mnum != unambig_mnum) {
	    LinkList list = newlinklist();

	    zsfree(scache);
	    scache = cline_str((ainfo->count ? ainfo->line : fainfo->line),
			       0, &ccache, list);
	    zsfree(pcache);
	    if (empty(list))
		pcache = ztrdup("");
	    else
		pcache = build_pos_string(list);

	    zsfree(icache);

	    list = newlinklist();
	    zsfree(cline_str((ainfo->count ? ainfo->line : fainfo->line),
			     2, NULL, list));
	    if (empty(list))
		icache = ztrdup("");
	    else
		icache = build_pos_string(list);
	}
    } else if (mnum != unambig_mnum || !ainfo || !scache) {
	zsfree(scache);
	scache = ztrdup("");
	zsfree(pcache);
	pcache = ztrdup("");
	zsfree(icache);
	icache = ztrdup("");
	ccache = 0;
    }
    unambig_mnum = mnum;
    if (cp)
	*cp = ccache + 1;
    if (pp)
	*pp = pcache;
    if (ip)
	*ip = icache;
    return scache;
}

/* Insert the given match. This returns the number of characters inserted.
 * scs is used to return the position where a automatically created suffix
 * has to be inserted. */

/**/
static int
instmatch(Cmatch m, int *scs)
{
    int l, r = 0, ocs, a = cs, brb = 0, bradd, *brpos;
    Brinfo bp;

    zsfree(lastprebr);
    zsfree(lastpostbr);
    lastprebr = lastpostbr = NULL;

    /* Ignored prefix. */
    if (m->ipre) {
	char *p = m->ipre + (menuacc ? m->qipl : 0);

	inststrlen(p, 1, (l = strlen(p)));
	r += l;
    }
    /* -P prefix. */
    if (m->pre) {
	inststrlen(m->pre, 1, (l = strlen(m->pre)));
	r += l;
    }
    /* Path prefix. */
    if (m->ppre) {
	inststrlen(m->ppre, 1, (l = strlen(m->ppre)));
	r += l;
    }
    /* The string itself. */
    inststrlen(m->str, 1, (l = strlen(m->str)));
    r += l;
    ocs = cs;
    /* Re-insert the brace beginnings, if any. */
    if (brbeg) {
	int pcs = cs;

	l = 0;
	for (bp = brbeg, brpos = m->brpl,
		 bradd = (m->pre ? strlen(m->pre) : 0);
	     bp; bp = bp->next, brpos++) {
	    cs = a + *brpos + bradd;
	    pcs = cs;
	    l = strlen(bp->str);
	    bradd += l;
	    brpcs = cs;
	    inststrlen(bp->str, 1, l);
	    r += l;
	    ocs += l;
	}
	lastprebr = (char *) zalloc(pcs - a + 1);
	memcpy(lastprebr, (char *) line + a, pcs - a);
	lastprebr[pcs - a] = '\0';
	cs = ocs;
    }
    /* Path suffix. */
    if (m->psuf) {
	inststrlen(m->psuf, 1, (l = strlen(m->psuf)));
	r += l;
    }
    /* Re-insert the brace end. */
    if (brend) {
	a = cs;
	for (bp = brend, brpos = m->brsl, bradd = 0; bp; bp = bp->next, brpos++) {
	    cs = a - *brpos;
	    ocs = brscs = cs;
	    l = strlen(bp->str);
	    bradd += l;
	    inststrlen(bp->str, 1, l);
	    brb = cs;
	    r += l;
	}
	cs = a + bradd;
	if (scs)
	    *scs = ocs;
    } else {
	brscs = -1;

	if (scs)
	    *scs = cs;
    }
    /* -S suffix */
    if (m->suf) {
	inststrlen(m->suf, 1, (l = strlen(m->suf)));
	r += l;
    }
    /* ignored suffix */
    if (m->isuf) {
	inststrlen(m->isuf, 1, (l = strlen(m->isuf)));
	r += l;
    }
    if (brend) {
	lastpostbr = (char *) zalloc(cs - brb + 1);
	memcpy(lastpostbr, (char *) line + brb, cs - brb);
	lastpostbr[cs - brb] = '\0';
    }
    lastend = cs;
    cs = ocs;

    return r;
}

/* Check if the match has the given prefix/suffix before/after the
 * braces. */

/**/
mod_export int
hasbrpsfx(Cmatch m, char *pre, char *suf)
{
    if (m->flags & CMF_ALL)
	return 1;
    else {
	char *op = lastprebr, *os = lastpostbr;
	VARARR(char, oline, ll);
	int oll = ll, ocs = cs, ole = lastend, opcs = brpcs, oscs = brscs, ret;

	memcpy(oline, line, ll);

	lastprebr = lastpostbr = NULL;

	instmatch(m, NULL);

	cs = 0;
	foredel(ll);
	spaceinline(oll);
	memcpy(line, oline, oll);
	cs = ocs;
	lastend = ole;
	brpcs = opcs;
	brscs = oscs;

	ret = (((!pre && !lastprebr) ||
		(pre && lastprebr && !strcmp(pre, lastprebr))) &&
	       ((!suf && !lastpostbr) ||
		(suf && lastpostbr && !strcmp(suf, lastpostbr))));

	zsfree(lastprebr);
	zsfree(lastpostbr);
	lastprebr = op;
	lastpostbr = os;

	return ret;
    }
}

/* Handle the case were we found more than one match. */

/**/
int
do_ambiguous(void)
{
    int ret = 0;

    menucmp = menuacc = 0;

    /* If we have to insert the first match, call do_single().  This is *
     * how REC_EXACT takes effect.  We effectively turn the ambiguous   *
     * completion into an unambiguous one.                              */
    if (ainfo && ainfo->exact == 1 && !(fromcomp & FC_LINE)) {
	minfo.cur = NULL;
	do_single(ainfo->exactm);
	invalidatelist();
	return ret;
    }
    /* Setting lastambig here means that the completion is ambiguous and *
     * AUTO_MENU might want to start a menu completion next time round,  *
     * but this might be overridden below if we can complete an          *
     * unambiguous prefix.                                               */
    lastambig = 1;

    if (usemenu || (haspattern && comppatinsert &&
		    !strcmp(comppatinsert, "menu"))) {
	/* We are in a position to start using menu completion due to one  *
	 * of the menu completion options, or due to the menu-complete-    *
	 * word command, or due to using GLOB_COMPLETE which does menu-    *
	 * style completion regardless of the setting of the normal menu   *
	 * completion options.                                             */
	do_ambig_menu();
    } else if (ainfo) {
	int atend = (cs == we), la, eq, tcs;
	VARARR(char, old, we - wb);

	minfo.cur = NULL;
	minfo.asked = 0;

	fixsuffix();

	/* First remove the old string from the line. */
	tcs = cs;
	cs = wb;
	memcpy(old, (char *) line + wb, we - wb);
	foredel(we - wb);

	/* Now get the unambiguous string and insert it into the line. */
	cline_str(ainfo->line, 1, NULL, NULL);

	/* Sometimes the different match specs used may result in a cline
	 * that gives an empty string. If that happened, we re-insert the
         * old string. Unless there were matches added with -U, that is. */

	if (lastend < we && !lenchanged && !hasunmatched) {
	    cs = wb;
	    foredel(lastend - wb);
	    inststrlen(old, 0, we - wb);
	    lastend = we;
	    cs = tcs;
	}
	if (eparq) {
	    tcs = cs;
	    cs = lastend;
	    for (eq = eparq; eq; eq--)
		inststrlen("\"", 0, 1);
	    cs = tcs;
	}
	/* la is non-zero if listambiguous may be used. Copying and
	 * comparing the line looks like BFI but it is the easiest
	 * solution. Really. */
	la = (ll != origll || strncmp(origline, (char *) line, ll));

	/* If REC_EXACT and AUTO_MENU are set and what we inserted is an  *
	 * exact match, we want menu completion the next time round       *
	 * so we set fromcomp, to ensure that the word on the line is not *
	 * taken as an exact match. Also we remember if we just moved the *
	 * cursor into the word.                                          */
	fromcomp = ((isset(AUTOMENU) ? FC_LINE : 0) |
		    ((atend && cs != lastend) ? FC_INWORD : 0));

	/* Probably move the cursor to the end. */
	if (movetoend == 3)
	    cs = lastend;

	/* If the LIST_AMBIGUOUS option (meaning roughly `show a list only *
	 * if the completion is completely ambiguous') is set, and some    *
	 * prefix was inserted, return now, bypassing the list-displaying  *
	 * code.  On the way, invalidate the list and note that we don't   *
	 * want to enter an AUTO_MENU imediately.                          */
	if ((uselist == 3 ||
	     (!uselist && isset(BASHAUTOLIST) && isset(LISTAMBIGUOUS))) &&
	    la) {
	    int fc = fromcomp;

	    invalidatelist();
	    fromcomp = fc;
	    lastambig = 0;
	    clearlist = 1;
	    return ret;
	}
    } else
	return ret;

    /* At this point, we might want a completion listing.  Show the listing *
     * if it is needed.                                                     */
    if (isset(LISTBEEP) && !oldlist)
	ret = 1;

    if (uselist && (usemenu != 2 || (!listshown && !oldlist)) &&
	((!showinglist && (!listshown || !oldlist)) ||
	 (usemenu == 3 && !oldlist)) &&
	(smatches >= 2 || forcelist))
	showinglist = -2;

    return ret;
}

/* This is a stat that ignores backslashes in the filename.  The `ls' *
 * parameter says if we have to do lstat() or stat().  I think this   *
 * should instead be done by use of a general function to expand a    *
 * filename (stripping backslashes), combined with the actual         *
 * (l)stat().                                                         */

/**/
mod_export int
ztat(char *nam, struct stat *buf, int ls)
{
    if (!(ls ? lstat(nam, buf) : stat(nam, buf)))
	return 0;
    else {
	char *p;
	VARARR(char, b, strlen(nam) + 1);

	for (p = b; *nam; nam++)
	    if (*nam == '\\' && nam[1])
		*p++ = *++nam;
	    else
		*p++ = *nam;
	*p = '\0';
	
	return ls ? lstat(b, buf) : stat(b, buf);
    }
}

/* Insert all matches in the command line. */

/**/
void
do_allmatches(int end)
{
    int first = 1, nm = nmatches - 1, omc = menucmp, oma = menuacc, e;
    Cmatch *mc;
    struct menuinfo mi;
    char *p = (brbeg ? ztrdup(lastbrbeg->str) : NULL);

    memcpy(&mi, &minfo, sizeof(struct menuinfo));
    menucmp = 1;
    menuacc = 0;

    for (minfo.group = amatches;
	 minfo.group && !(minfo.group)->mcount;
	 minfo.group = (minfo.group)->next);

    mc = (minfo.group)->matches;

    while (1) {
	if (!((*mc)->flags & CMF_ALL)) {
	    if (!first)
		accept_last();
	    first = 0;

	    if (!omc && !--nm)
		menucmp = 0;

	    do_single(*mc);
	}
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
    menucmp = omc;
    menuacc = oma;

    e = minfo.end;
    memcpy(&minfo, &mi, sizeof(struct menuinfo));
    minfo.end = e;
    minfo.len = e - minfo.pos;

    if (p) {
	zsfree(lastbrbeg->str);
	lastbrbeg->str = p;
    }
}

/* Insert a single match in the command line. */

/**/
mod_export void
do_single(Cmatch m)
{
    int l, sr = 0, scs;
    int havesuff = 0;
    int partest = (m->ripre || ((m->flags & CMF_ISPAR) && parpre));
    char *str = m->str, *ppre = m->ppre, *psuf = m->psuf, *prpre = m->prpre;

    if (!prpre) prpre = "";
    if (!ppre) ppre = "";
    if (!psuf) psuf = "";

    fixsuffix();

    if (!minfo.cur) {
	/* We are currently not in a menu-completion, *
	 * so set the position variables.             */
	minfo.pos = wb;
	minfo.we = (movetoend >= 2 || (movetoend == 1 && !menucmp) ||
		    (!movetoend && cs == we));
	minfo.end = we;
    }
    /* If we are already in a menu-completion or if we have done a *
     * glob completion, we have to delete some of the stuff on the *
     * command line.                                               */
    if (minfo.cur)
	l = minfo.len + minfo.insc;
    else
	l = we - wb;

    minfo.insc = 0;
    cs = minfo.pos;
    foredel(l);

    if (m->flags & CMF_ALL)
	do_allmatches(0);
    else {

    /* And then we insert the new string. */
    minfo.len = instmatch(m, &scs);
    minfo.end = cs;
    cs = minfo.pos + minfo.len;

    if (m->suf) {
	havesuff = 1;
	minfo.insc = ztrlen(m->suf);
	minfo.len -= minfo.insc;
	if (minfo.we) {
	    minfo.end += minfo.insc;
	    if (m->flags & CMF_REMOVE) {
		makesuffixstr(m->remf, m->rems, minfo.insc);
		if (minfo.insc == 1)
		    suffixlen[STOUC(m->suf[0])] = 1;
	    }
	}
    } else {
	/* There is no user-specified suffix, *
	 * so generate one automagically.     */
	cs = scs;
	if (partest && (m->flags & CMF_PARBR)) {
	    int pq;

	    /*{{*/
	    /* Completing a parameter in braces.  Add a removable `}' suffix. */
	    cs += eparq;
	    for (pq = parq; pq; pq--)
		inststrlen("\"", 1, 1);
	    minfo.insc += parq;
	    inststrlen("}", 1, 1);
	    minfo.insc++;
	    if (minfo.we)
		minfo.end += minfo.insc;
	    if (m->flags & CMF_PARNEST)
		havesuff = 1;
	}
	if (((m->flags & CMF_FILE) || (partest && isset(AUTOPARAMSLASH))) &&
	    cs > 0 && line[cs - 1] != '/') {
	    /* If we have a filename or we completed a parameter name      *
	     * and AUTO_PARAM_SLASH is set, lets see if it is a directory. *
	     * If it is, we append a slash.                                */
	    struct stat buf;
	    char *p;
	    int t = 0;

	    if (m->ipre && m->ipre[0] == '~' && !m->ipre[1])
		t = 1;
	    else {
		/* Build the path name. */
		if (partest && !*psuf && !(m->flags & CMF_PARNEST)) {
		    int ne = noerrs, tryit = 1;

		    p = (char *) zhalloc(strlen((m->flags & CMF_ISPAR) ?
						parpre : m->ripre) +
					 strlen(str) + 2);
		    sprintf(p, "%s%s%c",
			    ((m->flags & CMF_ISPAR) ? parpre : m->ripre), str,
			    ((m->flags & CMF_PARBR) ? '}' : '\0'));
		    if (*p == '$') {
			char *n;
			Param pm;

			if (p[1] == '{') {
			    char *e;

			    n = dupstring(p + 2);
			    e = n + strlen(n) - 1;

			    if (*e == '}')
				*e = '\0';
			} else
			    n = p + 1;

			if ((pm = (Param) paramtab->getnode(paramtab, n)) &&
			    PM_TYPE(pm->flags) != PM_SCALAR)
			    tryit = 0;
		    }
		    if (tryit) {
			noerrs = 1;
			parsestr(p);
			singsub(&p);
			errflag = 0;
			noerrs = ne;
		    }
		} else {
		    p = (char *) zhalloc(strlen(prpre) + strlen(str) +
				 strlen(psuf) + 3);
		    sprintf(p, "%s%s%s", ((prpre && *prpre) ?
					  prpre : "./"), str, psuf);
		}
		/* And do the stat. */
		t = (!(sr = ztat(p, &buf, 0)) && S_ISDIR(buf.st_mode));
	    }
	    if (t) {
		/* It is a directory, so add the slash. */
		havesuff = 1;
		inststrlen("/", 1, 1);
		minfo.insc++;
		if (minfo.we)
		    minfo.end++;
		if (!menucmp || minfo.we) {
		    if (m->remf || m->rems)
			makesuffixstr(m->remf, m->rems, 1);
		    else if (isset(AUTOREMOVESLASH)) {
			makesuffix(1);
			suffixlen['/'] = 1;
		    }
		}
	    }
	}
	if (!minfo.insc)
	    cs = minfo.pos + minfo.len - m->qisl;
    }
    /* If completing in a brace expansion... */
    if (brbeg) {
	if (havesuff) {
	    /*{{*/
	    /* If a suffix was added, and is removable, let *
	     * `,' and `}' remove it.                       */
	    if (isset(AUTOPARAMKEYS))
		suffixlen[','] = suffixlen['}'] = suffixlen[256];
	} else if (!menucmp) {
	    /*{{*/
	    /* Otherwise, add a `,' suffix, and let `}' remove it. */
	    cs = scs;
	    havesuff = 1;
	    inststrlen(",", 1, 1);
	    minfo.insc++;
	    makesuffix(1);
	    if ((!menucmp || minfo.we) && isset(AUTOPARAMKEYS))
		suffixlen[','] = suffixlen['}'] = 1;
	}
    } else if (!havesuff && (!(m->flags & CMF_FILE) || !sr)) {
	/* If we didn't add a suffix, add a space, unless we are *
	 * doing menu completion or we are completing files and  *
	 * the string doesn't name an existing file.             */
	if (m->autoq && (!m->isuf || !strpfx(m->autoq, m->isuf))) {
	    int al = strlen(m->autoq);
	    inststrlen(m->autoq, 1, al);
	    minfo.insc += al;
	}
	if (!menucmp && !(m->flags & CMF_NOSPACE) &&
	    (usemenu != 3 || insspace)) {
	    inststrlen(" ", 1, 1);
	    minfo.insc++;
	    if (minfo.we)
		makesuffix(1);
	}
    }
    if (minfo.we && partest && isset(AUTOPARAMKEYS))
	makeparamsuffix(((m->flags & CMF_PARBR) ? 1 : 0), minfo.insc - parq);

    if ((menucmp && !minfo.we) || !movetoend) {
	cs = minfo.end;
	if (cs + m->qisl == lastend)
	    cs += minfo.insc;
    }
    {
	Cmatch *om = minfo.cur;
	struct chdata dat;

	dat.matches = amatches;
	dat.num = nmatches;
	dat.cur = m;

	if (menucmp)
	    minfo.cur = &m;
	runhookdef(INSERTMATCHHOOK, (void *) &dat);
	minfo.cur = om;
    }
    }
}

/* Do completion, given that we are in the middle of a menu completion.  We *
 * don't need to generate a list of matches, because that's already been    *
 * done by previous commands.  We will either list the completions, or      *
 * insert the next completion.                                              */

/**/
mod_export void
do_menucmp(int lst)
{
    /* Just list the matches if the list was requested. */
    if (lst == COMP_LIST_COMPLETE) {
	showinglist = -2;
	return;
    }
    /* Otherwise go to the next match in the array... */
    do {
	if (!*++(minfo.cur)) {
	    do {
		if (!(minfo.group = (minfo.group)->next))
		    minfo.group = amatches;
	    } while (!(minfo.group)->mcount);
	    minfo.cur = minfo.group->matches;
	}
    } while ((menuacc &&
	      !hasbrpsfx(*(minfo.cur), minfo.prebr, minfo.postbr)) ||
	     (((*minfo.cur)->flags & (CMF_NOLIST | CMF_MULT)) &&
	      (!(*minfo.cur)->str || !*(*minfo.cur)->str)));
    /* ... and insert it into the command line. */
    metafy_line();
    do_single(*(minfo.cur));
    unmetafy_line();
}

/**/
int
reverse_menu(Hookdef dummy, void *dummy2)
{
    do {
	if (minfo.cur == (minfo.group)->matches) {
	    do {
		if (!(minfo.group = (minfo.group)->prev))
		    minfo.group = lmatches;
	    } while (!(minfo.group)->mcount);
	    minfo.cur = (minfo.group)->matches + (minfo.group)->mcount - 1;
	} else
	    minfo.cur--;
    } while ((menuacc &&
	      !hasbrpsfx(*(minfo.cur), minfo.prebr, minfo.postbr)) ||
	     (((*minfo.cur)->flags & (CMF_NOLIST | CMF_MULT)) &&
	      (!(*minfo.cur)->str || !*(*minfo.cur)->str)));
    metafy_line();
    do_single(*(minfo.cur));
    unmetafy_line();

    return 0;
}

/* Accepts the current completion and starts a new arg, *
 * with the next completions. This gives you a way to   *
 * accept several selections from the list of matches.  */

/**/
mod_export int
accept_last(void)
{
    if (!menuacc) {
	zsfree(minfo.prebr);
	minfo.prebr = ztrdup(lastprebr);
	zsfree(minfo.postbr);
	minfo.postbr = ztrdup(lastpostbr);

	if (listshown && (lastprebr || lastpostbr)) {
	    Cmgroup g;
	    Cmatch *m;

	    for (g = amatches, m = NULL; g && (!m || !*m); g = g->next)
		for (m = g->matches; *m; m++)
		    if (!hasbrpsfx(*m, minfo.prebr, minfo.postbr)) {
			showinglist = -2;
			break;
		    }
	}
    }
    menuacc++;

    if (brbeg) {
	int l;

	iremovesuffix(',', 1);

	l = (brscs >= 0 ? brscs : cs) - brpcs;

	zsfree(lastbrbeg->str);
	lastbrbeg->str = (char *) zalloc(l + 2);
	memcpy(lastbrbeg->str, line + brpcs, l);
	lastbrbeg->str[l] = ',';
	lastbrbeg->str[l + 1] = '\0';
    } else {
	int l;

	cs = minfo.pos + minfo.len + minfo.insc;
	iremovesuffix(' ', 1);
	l = cs;
	cs = minfo.pos + minfo.len + minfo.insc - (*(minfo.cur))->qisl;
	if (cs < l)
	    foredel(l - cs);
	else if (cs > ll)
	    cs = ll;
	inststrlen(" ", 1, 1);
	minfo.insc = minfo.len = 0;
	minfo.pos = cs;
	minfo.we = 1;
    }
    return 0;
}

/* This maps the value in v into the range [0,m-1], decrementing v
 * if it is non-negative and making negative values count backwards. */

/**/
static int
comp_mod(int v, int m)
{
    if (v >= 0)
	v--;
    if (v >= 0)
	return v % m;
    else {
	while (v < 0)
	    v += m;
	return v;
    }
}

/* This handles the beginning of menu-completion. */

/**/
void
do_ambig_menu(void)
{
    Cmatch *mc;

    if (usemenu != 3) {
	menucmp = 1;
	menuacc = 0;
	minfo.cur = NULL;
    } else {
	if (oldlist) {
	    if (oldins && minfo.cur)
		accept_last();
	} else
	    minfo.cur = NULL;
    }
#if 0
    /* group-numbers in compstate[insert] */
    if (insgroup) {
	insgnum = comp_mod(insgnum, lastpermgnum);
	for (minfo.group = amatches;
	     minfo.group && (minfo.group)->num != insgnum + 1;
	     minfo.group = (minfo.group)->next);
	if (!minfo.group || !(minfo.group)->mcount) {
	    minfo.cur = NULL;
	    minfo.asked = 0;
	    return;
	}
	insmnum = comp_mod(insmnum, (minfo.group)->mcount);
    } else {
#endif
	insmnum = comp_mod(insmnum, lastpermmnum);
	for (minfo.group = amatches;
	     minfo.group && (minfo.group)->mcount <= insmnum;
	     minfo.group = (minfo.group)->next)
	    insmnum -= (minfo.group)->mcount;
	if (!minfo.group) {
	    minfo.cur = NULL;
	    minfo.asked = 0;
	    return;
	}
#if 0
	/* group-numbers in compstate[insert] */
    }
#endif
    mc = (minfo.group)->matches + insmnum;
    do_single(*mc);
    minfo.cur = mc;
}

/* Return the number of screen lines needed for the list. */

/**/
zlong
list_lines(void)
{
    Cmgroup oam;

    permmatches(0);

    oam = amatches;
    amatches = pmatches;
    listdat.valid = 0;
    calclist(0);
    listdat.valid = 0;
    amatches = oam;

    return listdat.nlines;
}

/**/
void
comp_list(char *v)
{
    zsfree(complist);
    complist = v;

    onlyexpl = (v ? ((strstr(v, "expl") ? 1 : 0) |
		     (strstr(v, "messages") ? 2 : 0)) : 0);
}

/* This skips over matches that are not to be listed. */

/**/
mod_export Cmatch *
skipnolist(Cmatch *p, int showall)
{
    int mask = (showall ? 0 : (CMF_NOLIST | CMF_MULT)) | CMF_HIDE;

    while (*p && (((*p)->flags & mask) ||
		  ((*p)->disp &&
		   ((*p)->flags & (CMF_DISPLINE | CMF_HIDE)))))
	p++;

    return p;
}

/**/
mod_export int
calclist(int showall)
{
    static int lastinvcount = -1;

    Cmgroup g;
    Cmatch *p, m;
    Cexpl *e;
    int hidden = 0, nlist = 0, nlines = 0, add = 2 + isset(LISTTYPES);
    int max = 0, i;
    VARARR(int, mlens, nmatches + 1);

    if (lastinvcount == invcount &&
	listdat.valid && onlyexpl == listdat.onlyexpl &&
	menuacc == listdat.menuacc && showall == listdat.showall &&
	lines == listdat.lines && columns == listdat.columns)
	return 0;
    lastinvcount = invcount;

    for (g = amatches; g; g = g->next) {
	char **pp = g->ylist;
	int nl = 0, l, glong = 1, gshort = columns, ndisp = 0, totl = 0;

	g->flags |= CGF_PACKED | CGF_ROWS;

	if (!onlyexpl && pp) {
	    /* We have an ylist, lets see, if it contains newlines. */
	    hidden = 1;
	    while (!nl && *pp) {
                if (ztrlen(*pp) >= columns)
                    nl = 1;
                else
                    nl = !!strchr(*pp++, '\n');
            }
	    pp = g->ylist;
	    if (nl || !pp[1]) {
		/* Yup, there are newlines, count lines. */
		char *nlptr, *sptr;

		g->flags |= CGF_LINES;
		hidden = 1;
		while ((sptr = *pp)) {
		    while (sptr && *sptr) {
			nlines += (nlptr = strchr(sptr, '\n'))
			    ? 1 + (nlptr - sptr - 1) / columns
			    : (ztrlen(sptr) - 1) / columns;
			sptr = nlptr ? nlptr+1 : NULL;
		    }
		    nlines++;
		    pp++;
		}
		/*** nlines--; */
	    } else {
		while (*pp) {
		    l = ztrlen(*pp);
		    ndisp++;
		    if (l > glong)
			glong = l;
		    if (l < gshort)
			gshort = l;
		    totl += l;
		    nlist++;
		    pp++;
		}
	    }
	} else if (!onlyexpl) {
	    for (p = g->matches; (m = *p); p++) {
		if (menuacc && !hasbrpsfx(m, minfo.prebr, minfo.postbr)) {
		    m->flags |= CMF_HIDE;
		    continue;
		}
		m->flags &= ~CMF_HIDE;

		if (m->disp) {
		    if (m->flags & CMF_DISPLINE) {
			nlines += 1 + printfmt(m->disp, 0, 0, 0);
			g->flags |= CGF_HASDL;
		    } else {
			l = niceztrlen(m->disp);
			ndisp++;
			if (l > glong)
			    glong = l;
			if (l < gshort)
			    gshort = l;
			totl += l;
			mlens[m->gnum] = l;
		    }
		    nlist++;
		    if (!(m->flags & CMF_PACKED))
			g->flags &= ~CGF_PACKED;
		    if (!(m->flags & CMF_ROWS))
			g->flags &= ~CGF_ROWS;
		} else if (showall || !(m->flags & (CMF_NOLIST | CMF_MULT))) {
		    if ((m->flags & (CMF_NOLIST | CMF_MULT)) &&
			(!m->str || !*m->str)) {
			m->flags |= CMF_HIDE;
			continue;
		    }
		    l = niceztrlen(m->str);
		    ndisp++;
		    if (l > glong)
			glong = l;
		    if (l < gshort)
			gshort = l;
		    totl += l;
		    mlens[m->gnum] = l;
		    nlist++;
		    if (!(m->flags & CMF_PACKED))
			g->flags &= ~CGF_PACKED;
		    if (!(m->flags & CMF_ROWS))
			g->flags &= ~CGF_ROWS;
		} else
		    hidden = 1;
	    }
	}
	if ((e = g->expls)) {
	    while (*e) {
		if ((*e)->count &&
		    (!onlyexpl ||
		     (onlyexpl & ((*e)->count > 0 ? 1 : 2))))
		    nlines += 1 + printfmt((*e)->str, (*e)->count, 0, 1);
		e++;
	    }
	}
	g->totl = totl + (ndisp * add);
	g->dcount = ndisp;
	g->width = glong + add;
	g->shortest = gshort + add;
	if ((g->cols = columns / g->width) > g->dcount)
	    g->cols = g->dcount;
	if (g->cols) {
	    i = g->cols * g->width - add;
	    if (i > max)
		max = i;
	}
    }
    if (!onlyexpl) {
	char **pp;
	int *ws, tlines, tline, tcols, maxlen, nth, width, glines;

	for (g = amatches; g; g = g->next) {
	    glines = 0;

	    zfree(g->widths, 0);
	    g->widths = NULL;

	    if ((pp = g->ylist)) {
		if (!(g->flags & CGF_LINES)) {
		    if (g->cols) {
			glines += (arrlen(pp) + g->cols - 1) / g->cols;
			if (g->cols > 1)
			    g->width += (max - (g->width * g->cols - add)) / g->cols;
		    } else {
			g->cols = 1;
			g->width = 1;
			
			while (*pp)
			    glines += 1 + (ztrlen(*pp++) / columns);
		    }
		}
	    } else {
		if (g->cols) {
		    glines += (g->dcount + g->cols - 1) / g->cols;
		    if (g->cols > 1)
			g->width += (max - (g->width * g->cols - add)) / g->cols;
		} else if (!(g->flags & CGF_LINES)) {
		    g->cols = 1;
		    g->width = 0;
		    
		    for (p = g->matches; (m = *p); p++)
			if (!(m->flags & CMF_HIDE)) {
			    if (m->disp) {
				if (!(m->flags & CMF_DISPLINE))
				    glines += 1 + (mlens[m->gnum] / columns);
			    } else if (showall ||
				       !(m->flags & (CMF_NOLIST | CMF_MULT)))
				glines += 1 + ((mlens[m->gnum]) / columns);
			}
		}
	    }
	    g->lins = glines;
	    nlines += glines;
	}
	for (g = amatches; g; g = g->next) {
	    if (!(g->flags & CGF_PACKED))
		continue;

	    ws = g->widths = (int *) zalloc(columns * sizeof(int));
	    memset(ws, 0, columns * sizeof(int));
	    tlines = g->lins;
	    tcols = g->cols;
	    width = 0;

	    if ((pp = g->ylist)) {
		if (!(g->flags & CGF_LINES)) {
		    int yl = arrlen(pp), i;
		    VARARR(int, ylens, yl);

		    for (i = 0; *pp; i++, pp++)
			ylens[i] = ztrlen(*pp) + add;

		    if (g->flags & CGF_ROWS) {
			int count, tcol, first, maxlines = 0, llines;
			int beg = columns / g->shortest, end = g->cols;

			while (1) {
			    tcols = (beg + end) >> 1;

			    for (nth = first = maxlen = width = maxlines =
				     llines = tcol = 0,
				     count = g->dcount;
				 count > 0; count--) {
				if (ylens[nth] > maxlen)
				    maxlen = ylens[nth];
				nth += tcols;
				tlines++;
				if (nth >= g->dcount) {
				    if ((width += maxlen) >= columns)
					break;
				    ws[tcol++] = maxlen;
				    maxlen = 0;
				    nth = ++first;
				    if (llines > maxlines)
					maxlines = llines;
				    llines = 0;
				}
			    }
			    if (nth < yl) {
				ws[tcol++] = maxlen;
				width += maxlen;
			    }
			    if (!count && width <= columns &&
				(tcols <= 0 || beg == end))
				break;

			    if (beg == end) {
				beg--;
				end--;
			    } else if (width < columns) {
				if ((end = tcols) == beg - 1)
				    end++;
			    } else {
				if ((beg = tcols) - 1 == end)
				    end++;
			    }
			}
			if (tcols > g->cols)
			    tlines = maxlines;
		    } else {
			int beg = ((g->totl + columns) / columns);
			int end = g->lins;

			while (1) {
			    tlines = (beg + end) >> 1;

			    for (pp = g->ylist, nth = tline = width =
				     maxlen = tcols = 0;
				 *pp; pp++) {
				if (ylens[nth] > maxlen)
				    maxlen = ylens[nth];
				if (++tline == tlines) {
				    if ((width += maxlen) >= columns)
					break;
				    ws[tcols++] = maxlen;
				    maxlen = tline = 0;
				}
				nth++;
			    }
			    if (tline) {
				ws[tcols++] = maxlen;
				width += maxlen;
			    }
			    if (nth == yl && width <= columns &&
				(beg == end || tlines >= g->lins))
				break;

			    if (beg == end) {
				beg++;
				end++;
			    } else if (width < columns) {
				if ((end = tlines) == beg + 1)
				    end--;
			    } else {
				if ((beg = tlines) + 1 == end)
				    end--;
			    }
			}
			if (tlines > g->lins)
			    tlines = g->lins;
		    }
		}
	    } else if (g->width) {
		if (g->flags & CGF_ROWS) {
		    int addlen, count, tcol, maxlines = 0, llines, i;
		    int beg = columns / g->shortest, end = g->cols;
		    Cmatch *first;

		    while (1) {
			tcols = (beg + end) >> 1;

			p = first = skipnolist(g->matches, showall);
			for (maxlen = width = maxlines = llines = tcol = 0,
				 count = g->dcount;
			     count > 0; count--) {
			    m = *p;
			    addlen = mlens[m->gnum] + add;
			    if (addlen > maxlen)
				maxlen = addlen;
			    for (i = tcols; i && *p; i--)
				p = skipnolist(p + 1, showall);

			    llines++;
			    if (!*p) {
				if (llines > maxlines)
				    maxlines = llines;
				llines = 0;

				if ((width += maxlen) >= columns)
				    break;
				ws[tcol++] = maxlen;
				maxlen = 0;

				p = first = skipnolist(first + 1, showall);
			    }
			}
			if (tlines) {
			    ws[tcol++] = maxlen;
			    width += maxlen;
			}
			if (!count && width <= columns &&
			    (tcols <= 0 || beg == end))
			    break;

			if (beg == end) {
			    beg--;
			    end--;
			} else if (width < columns) {
			    if ((end = tcols) == beg - 1)
				end++;
			} else {
			    if ((beg = tcols) - 1 == end)
				end++;
			}
		    }
		    if (tcols > g->cols)
			tlines = maxlines;
		} else {
		    int addlen;
		    int smask = ((showall ? 0 : (CMF_NOLIST | CMF_MULT)) |
				 CMF_HIDE);
		    int beg = ((g->totl + columns) / columns);
		    int end = g->lins;

		    while (1) {
			tlines = (beg + end) >> 1;

			for (p = g->matches, nth = tline = width =
				 maxlen = tcols = 0;
			     (m = *p); p++) {
			    if (!(m->flags &
				  (m->disp ? (CMF_DISPLINE | CMF_HIDE) :
				   smask))) {
				addlen = mlens[m->gnum] + add;
				if (addlen > maxlen)
				    maxlen = addlen;
				if (++tline == tlines) {
				    if ((width += maxlen) >= columns)
					break;
				    ws[tcols++] = maxlen;
				    maxlen = tline = 0;
				}
				nth++;
			    }
			}
			if (tline) {
			    ws[tcols++] = maxlen;
			    width += maxlen;
			}
			if (nth == g->dcount && width <= columns &&
			    (beg == end || tlines >= g->lins))
			    break;

			if (beg == end) {
			    beg++;
			    end++;
			} else if (width < columns) {
			    if ((end = tlines) == beg + 1)
				end--;
			} else {
			    if ((beg = tlines) + 1 == end)
				end--;
			}
		    }
		    if (tlines > g->lins)
			tlines = g->lins;
		}
	    }
	    if (tlines == g->lins) {
		zfree(ws, columns * sizeof(int));
		g->widths = NULL;
	    } else {
		nlines += tlines - g->lins;
		g->lins = tlines;
		g->cols = tcols;
		g->totl = width;
		width -= add;
		if (width > max)
		    max = width;
	    }
	}
	for (g = amatches; g; g = g->next) {
	    if (g->widths) {
		int *p, a = (max - g->totl + add) / g->cols;

		for (i = g->cols, p = g->widths; i; i--, p++)
		    *p += a;
	    } else if (g->width && g->cols > 1)
		g->width += (max - (g->width * g->cols - add)) / g->cols;
	}
    }
    listdat.valid = 1;
    listdat.hidden = hidden;
    listdat.nlist = nlist;
    listdat.nlines = nlines;
    listdat.menuacc = menuacc;
    listdat.onlyexpl = onlyexpl;
    listdat.columns = columns;
    listdat.lines = lines;
    listdat.showall = showall;

    return 1;
}

/**/
mod_export int
asklist(void)
{
    /* Set the cursor below the prompt. */
    trashzle();
    showinglist = listshown = 0;

    clearflag = (isset(USEZLE) && !termflags && dolastprompt);
    lastlistlen = 0;

    /* Maybe we have to ask if the user wants to see the list. */
    if ((!minfo.cur || !minfo.asked) &&
	((complistmax > 0 && listdat.nlist >= complistmax) ||
	 (complistmax < 0 && listdat.nlines <= -complistmax) ||
	 (!complistmax && listdat.nlines >= lines))) {
	int qup, l;

	zsetterm();
	l = (listdat.nlist > 0 ?
	     fprintf(shout, "zsh: do you wish to see all %d possibilities (%d lines)? ",
		     listdat.nlist, listdat.nlines) :
	     fprintf(shout, "zsh: do you wish to see all %d lines? ",
		     listdat.nlines));
	qup = ((l + columns - 1) / columns) - 1;
	fflush(shout);
	if (getzlequery(1) != 'y') {
	    if (clearflag) {
		putc('\r', shout);
		tcmultout(TCUP, TCMULTUP, qup);
		if (tccan(TCCLEAREOD))
		    tcout(TCCLEAREOD);
		tcmultout(TCUP, TCMULTUP, nlnct);
	    } else
		putc('\n', shout);
	    minfo.asked = 2;
	    return 1;
	}
	if (clearflag) {
	    putc('\r', shout);
	    tcmultout(TCUP, TCMULTUP, qup);
	    if (tccan(TCCLEAREOD))
		tcout(TCCLEAREOD);
	} else
	    putc('\n', shout);
	settyinfo(&shttyinfo);
	minfo.asked = 1;
    } else if (minfo.asked == 2)
	tcmultout(TCUP, TCMULTUP, nlnct);

    return (minfo.asked ? minfo.asked - 1 : 0);
}

/**/
mod_export int
printlist(int over, CLPrintFunc printm, int showall)
{
    Cmgroup g;
    Cmatch *p, m;
    Cexpl *e;
    int pnl = 0, cl = (over ? listdat.nlines : -1);
    int mc = 0, ml = 0, printed = 0;

    if (cl < 2) {
	cl = -1;
	if (tccan(TCCLEAREOD))
	    tcout(TCCLEAREOD);
    }
    g = amatches;
    while (g) {
	char **pp = g->ylist;

	if ((e = g->expls)) {
	    int l;

	    while (*e) {
		if ((*e)->count &&
		    (!listdat.onlyexpl ||
		     (listdat.onlyexpl & ((*e)->count > 0 ? 1 : 2)))) {
		    if (pnl) {
			putc('\n', shout);
			pnl = 0;
			ml++;
			if (cl >= 0 && --cl <= 1) {
			    cl = -1;
			    if (tccan(TCCLEAREOD))
				tcout(TCCLEAREOD);
			}
		    }
		    l = printfmt((*e)->str, (*e)->count, 1, 1);
		    ml += l;
		    if (cl >= 0 && (cl -= l) <= 1) {
			cl = -1;
			if (tccan(TCCLEAREOD))
			    tcout(TCCLEAREOD);
		    }
		    pnl = 1;
		}
		e++;
	    }
	}
	if (!listdat.onlyexpl && pp && *pp) {
	    if (pnl) {
		putc('\n', shout);
		pnl = 0;
		ml++;
		if (cl >= 0 && --cl <= 1) {
		    cl = -1;
		    if (tccan(TCCLEAREOD))
			tcout(TCCLEAREOD);
		}
	    }
	    if (g->flags & CGF_LINES) {
                char *p;

		while ((p = *pp++)) {
		    zputs(p, shout);
		    if (*pp) {
                        if (ztrlen(p) % columns)
                            putc('\n', shout);
                        else
                            fputs(" \010", shout);
                    }
		}
	    } else {
		int n = g->lcount, nl, nc, i, a;
		char **pq;

		nl = nc = g->lins;

		while (n && nl--) {
		    i = g->cols;
		    mc = 0;
		    pq = pp;
		    while (n && i--) {
			if (pq - g->ylist >= g->lcount)
			    break;
			zputs(*pq, shout);
			if (i) {
			    a = (g->widths ? g->widths[mc] : g->width) -
				strlen(*pq);
			    while (a--)
				putc(' ', shout);
			}
			pq += ((g->flags & CGF_ROWS) ? 1 : nc);
			mc++;
			n--;
		    }
		    if (n) {
			putc('\n', shout);
			ml++;
			if (cl >= 0 && --cl <= 1) {
			    cl = -1;
			    if (tccan(TCCLEAREOD))
				tcout(TCCLEAREOD);
			}
		    }
		    pp += ((g->flags & CGF_ROWS) ? g->cols : 1);
		}
	    }
	} else if (!listdat.onlyexpl &&
		   (g->lcount || (showall && g->mcount))) {
	    int n = g->dcount, nl, nc, i, j, wid;
	    Cmatch *q;

	    nl = nc = g->lins;

	    if (g->flags & CGF_HASDL) {
		for (p = g->matches; (m = *p); p++)
		    if (m->disp && (m->flags & CMF_DISPLINE)) {
			if (pnl) {
			    putc('\n', shout);
			    pnl = 0;
			    ml++;
			    if (cl >= 0 && --cl <= 1) {
				cl = -1;
				if (tccan(TCCLEAREOD))
				    tcout(TCCLEAREOD);
			    }
			}
			printed++;
			printm(g, p, 0, ml, 1, 0, NULL, NULL);
			pnl = 1;
		    }
	    }
	    if (n && pnl) {
		putc('\n', shout);
		pnl = 0;
		ml++;
		if (cl >= 0 && --cl <= 1) {
		    cl = -1;
		    if (tccan(TCCLEAREOD))
			tcout(TCCLEAREOD);
		}
	    }
	    for (p = skipnolist(g->matches, showall); n && nl--;) {
		i = g->cols;
		mc = 0;
		q = p;
		while (n && i--) {
		    wid = (g->widths ? g->widths[mc] : g->width);
		    if (!(m = *q)) {
			printm(g, NULL, mc, ml, (!i), wid, NULL, NULL);
			break;
		    }
		    if (!m->disp && (m->flags & CMF_FILE) &&
			m->str[0] && m->str[strlen(m->str) - 1] != '/') {
			struct stat buf;
			char *pb;

			pb = (char *) zhalloc((m->prpre ? strlen(m->prpre) : 0) +
					     3 + strlen(m->str));
			sprintf(pb, "%s%s", (m->prpre ? m->prpre : "./"),
				m->str);

			if (ztat(pb, &buf, 1))
			    printm(g, q, mc, ml, (!i), wid, NULL, NULL);
			else
			    printm(g, q, mc, ml, (!i), wid, pb, &buf);
		    } else
			printm(g, q, mc, ml, (!i), wid, NULL, NULL);

		    printed++;

		    if (--n)
			for (j = ((g->flags & CGF_ROWS) ? 1 : nc);
			     j && *q; j--)
			    q = skipnolist(q + 1, showall);
		    mc++;
		}
		while (i-- > 0) {
		    printm(g, NULL, mc, ml, (!i),
			   (g->widths ? g->widths[mc] : g->width), NULL, NULL);
		    mc++;
		}
		if (n) {
		    putc('\n', shout);
		    ml++;
		    if (cl >= 0 && --cl <= 1) {
			cl = -1;
			if (tccan(TCCLEAREOD))
			    tcout(TCCLEAREOD);
		    }
		    if (nl)
			for (j = ((g->flags & CGF_ROWS) ? g->cols : 1);
			     j && *p; j--)
			    p = skipnolist(p + 1, showall);
		}
	    }
	}
	if (g->lcount || (showall && g->mcount))
	    pnl = 1;
	g = g->next;
    }
    lastlistlen = 0;
    if (clearflag) {
	/* Move the cursor up to the prompt, if always_last_prompt *
	 * is set and all that...                                  */
	if ((ml = listdat.nlines + nlnct - 1) < lines) {
	    tcmultout(TCUP, TCMULTUP, ml);
	    showinglist = -1;

	    lastlistlen = listdat.nlines;
	} else
	    clearflag = 0, putc('\n', shout);
    } else
	putc('\n', shout);

    listshown = (clearflag ? 1 : -1);

    return printed;
}

/**/
mod_export void
bld_all_str(Cmatch all)
{
    Cmgroup g;
    Cmatch *mp, m;
    int len = columns - 5, t, add = 0;
    VARARR(char, buf, columns + 1);

    buf[0] = '\0';

    for (g = amatches; g && !g->mcount; g = g->next);

    mp = g->matches;
    while (1) {
	m = *mp;
	if (!(m->flags & (CMF_ALL | CMF_HIDE)) && m->str) {
	    t = strlen(m->str) + add;
	    if (len >= t) {
		if (add)
		    strcat(buf, " ");
		strcat(buf, m->str);
		len -= t;
		add = 1;
	    } else {
		if (len > add + 2) {
		    if (add)
			strcat(buf, " ");
		    strncat(buf, m->str, len);
		}
		strcat(buf, " ...");
		break;
	    }
	}
	if (!*++mp) {
	    do {
		if (!(g = g->next))
		    break;
	    } while (!g->mcount);
	    if (!g)
		break;
	    mp = g->matches;
	}
    }
    zsfree(all->disp);
    all->disp = ztrdup(buf);
}

/**/
static void
iprintm(Cmgroup g, Cmatch *mp, int mc, int ml, int lastc, int width,
	char *path, struct stat *buf)
{
    Cmatch m;
    int len = 0;

    if (!mp)
	return;

    m = *mp;
    if ((m->flags & CMF_ALL) && (!m->disp || !m->disp[0]))
	bld_all_str(m);
    if (m->disp) {
	if (m->flags & CMF_DISPLINE) {
	    printfmt(m->disp, 0, 1, 0);
	    return;
	}
	nicezputs(m->disp, shout);
	len = niceztrlen(m->disp);
    } else {
	nicezputs(m->str, shout);
	len = niceztrlen(m->str);

	if (isset(LISTTYPES) && buf) {
	    putc(file_type(buf->st_mode), shout);
	    len++;
	}
    }
    if (!lastc) {
	len = width - len;

	while (len-- > 0)
	    putc(' ', shout);
    }
}

/**/
int
ilistmatches(Hookdef dummy, Chdata dat)
{
    calclist(0);

    if (!listdat.nlines) {
	showinglist = listshown = 0;
	return 1;
    }
    if (asklist())
	return 0;

    printlist(0, iprintm, 0);

    return 0;
}

/* List the matches.  Note that the list entries are metafied. */

/**/
int
list_matches(Hookdef dummy, void *dummy2)
{
    struct chdata dat;
    int ret;

#ifdef DEBUG
    /* Sanity check */
    if (!validlist) {
	showmsg("BUG: listmatches called with bogus list");
	return 1;
    }
#endif

    dat.matches = amatches;
    dat.num = nmatches;
    dat.cur = NULL;
    ret = runhookdef(COMPLISTMATCHESHOOK, (void *) &dat);

    return ret;
}

/* Invalidate the completion list. */

/**/
mod_export int
invalidate_list(void)
{
    invcount++;
    if (validlist) {
	if (showinglist == -2)
	    zrefresh();
	freematches(lastmatches, 1);
	lastmatches = NULL;
	hasoldlist = 0;
    }
    lastambig = menucmp = menuacc = validlist = showinglist = fromcomp = 0;
    listdat.valid = 0;
    if (listshown < 0)
	listshown = 0;
    minfo.cur = NULL;
    minfo.asked = 0;
    zsfree(minfo.prebr);
    zsfree(minfo.postbr);
    minfo.postbr = minfo.prebr = NULL;
    compwidget = NULL;

    return 0;
}
