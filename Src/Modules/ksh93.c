/*
 * ksh93.c - support for more ksh93 features
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 2022 Barton E. Schaefer
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Barton E. Schaefer or the Zsh Development
 * Group be liable to any party for direct, indirect, special, incidental, or
 * consequential damages arising out of the use of this software and its
 * documentation, even if Barton E. Schaefer and the Zsh
 * Development Group have been advised of the possibility of such damage.
 *
 * Barton E. Schaefer and the Zsh Development Group
 * specifically disclaim any warranties, including, but not limited to, the
 * implied warranties of merchantability and fitness for a particular purpose.
 * The software provided hereunder is on an "as is" basis, and
 * Barton E. Schaefer and the Zsh Development Group have no
 * obligation to provide maintenance, support, updates, enhancements, or
 * modifications.
 *
 */

#include "ksh93.mdh"
#include "ksh93.pro"

/* Implementing "namespace" requires creating a new keword.  Hrm. */

/*
 * Standard module configuration/linkage
 */

static struct builtin bintab[] = {
    BUILTIN("nameref", BINF_ASSIGN, (HandlerFunc)bin_typeset, 0, -1, 0, "gpru", "n")
};

#include "zsh.mdh"

static void
edcharsetfn(Param pm, char *x)
{
    /*
     * To make this work like ksh, we must intercept $KEYS before the widget
     * is looked up, so that changing the key sequence causes a different
     * widget to be substituted.  Somewhat similar to "bindkey -s".
     *
     * Ksh93 adds SIGKEYBD to the trap list for this purpose.
     */
    ;
}

static char **
matchgetfn(Param pm)
{
    char **zsh_match = getaparam("match");

    /* For this to work accurately, ksh emulation should always imply
     * that the (#m) and (#b) extendedglob operators are enabled.
     *
     * When we have a 0th element (ksharrays), it is $MATCH.  Elements
     * 1st and larger mirror the $match array.
     */

    if (pm->u.arr)
	freearray(pm->u.arr);
    if (zsh_match && *zsh_match) {
	if (isset(KSHARRAYS)) {
	    char **ap =
		(char **) zalloc(sizeof(char *) * (arrlen(zsh_match)+1));
	    pm->u.arr = ap;
	    *ap++ = ztrdup(getsparam("MATCH"));
	    while (*zsh_match)
		*ap = ztrdup(*zsh_match++);
	} else
	    pm->u.arr = zarrdup(zsh_match);
    } else if (isset(KSHARRAYS)) {
	pm->u.arr = mkarray(ztrdup(getsparam("MATCH")));
    } else
	pm->u.arr = NULL;

    return arrgetfn(pm);
}

static const struct gsu_scalar constant_gsu =
    { strgetfn, NULL, nullunsetfn };

static const struct gsu_scalar sh_edchar_gsu =
    { strvargetfn, edcharsetfn, nullunsetfn };
static const struct gsu_scalar sh_edmode_gsu =
    { strgetfn, nullstrsetfn, nullunsetfn };
static const struct gsu_array sh_match_gsu =
    { matchgetfn, arrsetfn, stdunsetfn };
static const struct gsu_scalar sh_name_gsu =
    { strvargetfn, nullstrsetfn, nullunsetfn };
static const struct gsu_scalar sh_subscript_gsu =
    { strvargetfn, nullstrsetfn, nullunsetfn };

static char sh_unsetval[2];	/* Dummy to treat as NULL */
static char *sh_name = sh_unsetval;
static char *sh_subscript = sh_unsetval;
static char *sh_edchar = sh_unsetval;
static char sh_edmode[2];

/*
 * Some parameters listed here do not appear in ksh93.mdd autofeatures
 * because they are only instantiated by ksh93_wrapper() below.  This
 * obviously includes those commented out here.
 */
static struct paramdef partab[] = {
    PARAMDEF(".sh.edchar", PM_SCALAR|PM_SPECIAL,
	     &sh_edchar, &sh_edchar_gsu),
    PARAMDEF(".sh.edmode", PM_SCALAR|PM_READONLY|PM_SPECIAL,
	     &sh_edmode, &sh_edmode_gsu),
    PARAMDEF(".sh.file", PM_NAMEREF|PM_READONLY, "ZSH_SCRIPT", &constant_gsu),
    PARAMDEF(".sh.lineno", PM_NAMEREF|PM_READONLY, "LINENO", &constant_gsu),
    PARAMDEF(".sh.match", PM_ARRAY|PM_READONLY, NULL, &sh_match_gsu),
    PARAMDEF(".sh.name", PM_SCALAR|PM_READONLY|PM_SPECIAL,
	     &sh_name, &sh_name_gsu),
    PARAMDEF(".sh.subscript", PM_SCALAR|PM_READONLY|PM_SPECIAL,
	     &sh_subscript, &sh_subscript_gsu),
    PARAMDEF(".sh.subshell", PM_NAMEREF|PM_READONLY, "ZSH_SUBSHELL", &constant_gsu),
    /* SPECIALPMDEF(".sh.value", 0, NULL, NULL, NULL), */
    PARAMDEF(".sh.version", PM_NAMEREF|PM_READONLY, "ZSH_PATCHLEVEL", &constant_gsu)
};

static struct features module_features = {
    bintab, sizeof(bintab)/sizeof(*bintab),
    NULL, 0,
    NULL, 0,
    partab, sizeof(partab)/sizeof(*partab),
    0
};

/**/
static int
ksh93_wrapper(Eprog prog, FuncWrap w, char *name)
{
    Funcstack f;
    Param pm;
    zlong num = funcstack->prev ? getiparam(".sh.level") : 0;

    if (!EMULATION(EMULATE_KSH))
	return 1;

    if (num == 0)
	for (f = funcstack; f; f = f->prev, num++);
    else
	num++;

    queue_signals();
    ++locallevel;		/* Make these local */
#define LOCAL_NAMEREF (PM_LOCAL|PM_UNSET|PM_NAMEREF)
    if ((pm = createparam(".sh.command", LOCAL_NAMEREF))) {
	pm->level = locallevel;	/* Why is this necessary? */
	/* Force scoping by assignent hack */
	setloopvar(".sh.command", "ZSH_DEBUG_CMD");
	pm->node.flags |= PM_READONLY;
    }
    /* .sh.edchar is in partab and below */
    if (zleactive && (pm = createparam(".sh.edcol", LOCAL_NAMEREF))) {
	pm->level = locallevel;
	setloopvar(".sh.edcol", "CURSOR");
	pm->node.flags |= (PM_NAMEREF|PM_READONLY);
    }
    /* .sh.edmode is in partab and below */
    if (zleactive && (pm = createparam(".sh.edtext", LOCAL_NAMEREF))) {
	pm->level = locallevel;
	setloopvar(".sh.edtext", "BUFFER");
	pm->node.flags |= PM_READONLY;
    }

    if ((pm = createparam(".sh.fun", PM_LOCAL|PM_UNSET))) {
	pm->level = locallevel;
	setsparam(".sh.fun", ztrdup(name));
	pm->node.flags |= PM_READONLY;
    }
    if ((pm = createparam(".sh.level", PM_LOCAL|PM_UNSET))) {
	pm->level = locallevel;
	setiparam(".sh.level", num);
    }
    if (zleactive) {
	extern mod_import_variable char *curkeymapname;	/* XXX */
	extern mod_import_variable char *varedarg;	/* XXX */
	/* bindkey -v forces VIMODE so this test is as good as any */
	if (curkeymapname && isset(VIMODE) &&
	    strcmp(curkeymapname, "main") == 0)
	    strcpy(sh_edmode, "\033");
	else
	    strcpy(sh_edmode, "");
	if (sh_edchar == sh_unsetval)
	    sh_edchar = dupstring(getsparam("KEYS"));
	if (varedarg) {
	    char *ie = itype_end((sh_name = dupstring(varedarg)), INAMESPC, 0);
	    if (ie && *ie) {
		*ie++ = '\0';
		/* Assume bin_vared has validated subscript */
		sh_subscript = dupstring(ie);
		ie = sh_subscript + strlen(sh_subscript);
		*--ie = '\0';
	    } else
		sh_subscript = sh_unsetval;
	    if ((pm = createparam(".sh.value", LOCAL_NAMEREF))) {
		pm->level = locallevel;
		setloopvar(".sh.value", "BUFFER");	/* Hack */
		pm->node.flags |= PM_READONLY;
	    }
	} else
	    sh_name = sh_subscript = sh_unsetval;
    } else {
	sh_edchar = sh_name = sh_subscript = sh_unsetval;
	strcpy(sh_edmode, "");
	/* TODO:
	 * - disciplines
	 * - special handling of .sh.value in math
	 */
    }
    --locallevel;
    unqueue_signals();

    return 1;
}

static struct funcwrap wrapper[] = {
    WRAPDEF(ksh93_wrapper),
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
    return addwrapper(m, wrapper);
}

/**/
int
cleanup_(Module m)
{
    struct paramdef *p;

    deletewrapper(m, wrapper);

    /* Clean up namerefs, otherwise deleteparamdef() is confused */
    for (p = partab; p < partab + sizeof(partab)/sizeof(*partab); ++p) {
	if (p->flags & PM_NAMEREF) {
	    HashNode hn = gethashnode2(paramtab, p->name);
	    if (hn)
		((Param)hn)->node.flags &= ~PM_NAMEREF;
	}
    }
    return setfeatureenables(m, &module_features, NULL);
}

/**/
int
finish_(UNUSED(Module m))
{
    return 0;
}
