/*
 * hlgroup.c - Supporting parameters for highlight groups
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 2024 Oliver Kiddle
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Oliver Kiddle or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Oliver Kiddle and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Oliver Kiddle and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Oliver Kiddle and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "hlgroup.mdh"
#include "hlgroup.pro"

#define GROUPVAR ".zle.hlgroups"

static const struct gsu_scalar pmesc_gsu =
{ strgetfn, nullstrsetfn, nullunsetfn };

/**/
static char *
convertattr(char *attrstr, int sgr)
{
    zattr atr;
    char *r, *s;
    int len;

    match_highlight(attrstr, &atr, NULL, NULL);
    s = zattrescape(atr, sgr ? NULL : &len);

    if (sgr) {
	char *c = s, *t = s - 1;

	while (c[0] == '\033' && c[1] == '[') {
	    for (c += 2; ; c++) {
		if (isdigit(*c))
		    *++t = *c;
		else if (*c == ';' || *c == ':')
		    *++t = ';';
		else
		    break;
	    }
	    t++;
	    if (*c != 'm')
		break;
	    *t = ';';
	    c++;
	}
	if (t <= s) { /* always return at least "0" */
	    *s = '0';
	    t = s + 1;
	}
	*t = '\0';
	len = t - s;
    }

    r = dupstring_wlen(s, len);
    free(s);
    return r;
}

/**/
static HashNode
getgroup(const char *name, int sgr)
{
    Param pm = NULL;
    HashNode hn;
    HashTable hlg;
    Value v;
    struct value vbuf;
    char *var = GROUPVAR;

    pm = (Param) hcalloc(sizeof(struct param));
    pm->gsu.s = &pmesc_gsu;
    pm->node.nam = dupstring(name);
    pm->node.flags = PM_SCALAR|PM_SPECIAL;

    if (!(v = getvalue(&vbuf, &var, 0)) ||
	     PM_TYPE(v->pm->node.flags) != PM_HASHED ||
	     !(hlg = v->pm->gsu.h->getfn(v->pm)) ||
	     !(hn = gethashnode2(hlg, name)) ||
	     (((Param) hn)->node.flags & PM_UNSET))
    {
	pm->u.str = dupstring("");
	pm->node.flags |= PM_UNSET;
    } else {
	pm->u.str = convertattr(((Param) hn)->u.str, sgr);
    }

    return &pm->node;
}

/**/
static void
scangroup(ScanFunc func, int flags, int sgr)
{
    struct param pm;
    int i;
    HashNode hn;
    HashTable hlg;
    Value v;
    struct value vbuf;
    char *var = GROUPVAR;

    if (!(v = getvalue(&vbuf, &var, 0)) ||
	     PM_TYPE(v->pm->node.flags) != PM_HASHED)
	return;
    hlg = v->pm->gsu.h->getfn(v->pm);

    memset((void *)&pm, 0, sizeof(struct param));
    pm.node.flags = PM_SCALAR;
    pm.gsu.s = &pmesc_gsu;

    for (i = 0; i < hlg->hsize; i++)
	for (hn = hlg->nodes[i]; hn; hn = hn->next) {
	    pm.u.str = convertattr(((Param) hn)->u.str, sgr);
	    pm.node.nam = hn->nam;
	    func(&pm.node, flags);
	}
}
/**/
static HashNode
getpmesc(UNUSED(HashTable ht), const char *name)
{
    return getgroup(name, 0);
}

/**/
static void
scanpmesc(UNUSED(HashTable ht), ScanFunc func, int flags)
{
    scangroup(func, flags, 0);
}

/**/
static HashNode
getpmsgr(UNUSED(HashTable ht), const char *name)
{
    return getgroup(name, 1);
}

/**/
static void
scanpmsgr(UNUSED(HashTable ht), ScanFunc func, int flags)
{
    scangroup(func, flags, 1);
}

static struct paramdef partab[] = {
    SPECIALPMDEF(".zle.esc", PM_READONLY_SPECIAL, 0, getpmesc, scanpmesc),
    SPECIALPMDEF(".zle.sgr", PM_READONLY_SPECIAL, 0, getpmsgr, scanpmsgr)
};

static struct features module_features = {
    NULL, 0,
    NULL, 0,
    NULL, 0,
    partab, sizeof(partab)/sizeof(*partab),
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
    return 0;
}
