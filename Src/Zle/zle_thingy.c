/*
 * zle_thingy.c - thingies
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

#include "zle.mdh"
#include "zle_thingy.pro"

/*
 * Thingies:
 *
 * From the user's point of view, a thingy is just a string.  Internally,
 * the thingy is a struct thingy; these structures are in a hash table
 * indexed by the string the user sees.  This hash table contains all
 * thingies currently referenced anywhere; each has a reference count,
 * and is deleted when it becomes unused.  Being the name of a function
 * counts as a reference.
 *
 * The DISABLED flag on a thingy indicates that it is not the name of a
 * widget.  This makes it easy to generate completion lists;
 * looking only at the `enabled' nodes makes the thingy table look like
 * a table of widgets.
 */

/* Hashtable of thingies.  Enabled nodes are those that refer to widgets. */

/**/
HashTable thingytab;

/**********************************/
/* hashtable management functions */
/**********************************/

/**/
static void
createthingytab(void)
{
    thingytab = newhashtable(199, "thingytab", NULL);

    thingytab->hash        = hasher;
    thingytab->emptytable  = emptythingytab;
    thingytab->filltable   = NULL;
    thingytab->addnode     = addhashnode;
    thingytab->getnode     = gethashnode;
    thingytab->getnode2    = gethashnode2;
    thingytab->removenode  = removehashnode;
    thingytab->disablenode = NULL;
    thingytab->enablenode  = NULL;
    thingytab->freenode    = freethingynode;
    thingytab->printnode   = NULL;
}

/**/
static void
emptythingytab(HashTable ht)
{
    /* This will only be called when deleting the thingy table, which *
     * is only done to unload the zle module.  A normal emptytable()  *
     * function would free all the thingies, but we don't want to do  *
     * that because some of them are the known thingies in the fixed  *
     * `thingies' table.  As the module cleanup code deletes all the  *
     * keymaps and so on before deleting the thingy table, we can     *
     * just remove the user-defined widgets and then be sure that     *
     * *all* the thingies left are the fixed ones.  This has the side *
     * effect of freeing all resources used by user-defined widgets.  */
    scanhashtable(thingytab, 0, 0, DISABLED, scanemptythingies, 0);
}

/**/
static void
scanemptythingies(HashNode hn, int flags)
{
    Thingy t = (Thingy) hn;

    /* Mustn't unbind internal widgets -- we wouldn't want to free the *
     * memory they use.                                                */
    if(!(t->widget->flags & WIDGET_INT))
	unbindwidget(t, 1);
}

/**/
static Thingy
makethingynode(void)
{
    Thingy t = (Thingy) zcalloc(sizeof(*t));

    t->flags = DISABLED;
    return t;
}

/**/
static void
freethingynode(HashNode hn)
{
    Thingy th = (Thingy) hn;

    zsfree(th->nam);
    zfree(th, sizeof(*th));
}

/************************/
/* referencing thingies */
/************************/

/* It is important to maintain the reference counts on thingies.  When *
 * copying a reference to a thingy, wrap the copy in refthingy(), to   *
 * increase its reference count.  When removing a reference,           *
 * unrefthingy() it.  Both of these functions handle NULL arguments    *
 * correctly.                                                          */

/**/
Thingy
refthingy(Thingy th)
{
    if(th)
	th->rc++;
    return th;
}

/**/
void
unrefthingy(Thingy th)
{
    if(th && !--th->rc)
	thingytab->freenode(thingytab->removenode(thingytab, th->nam));
}

/* Use rthingy() to turn a string into a thingy.  It increases the reference *
 * count, after creating the thingy structure if necessary.                  */

/**/
Thingy
rthingy(char *nam)
{
    Thingy t = (Thingy) thingytab->getnode2(thingytab, nam);

    if(!t)
	thingytab->addnode(thingytab, ztrdup(nam), t = makethingynode());
    return refthingy(t);
}

/***********/
/* widgets */
/***********/

/*
 * Each widget is attached to one or more thingies.  Each thingy
 * names either zero or one widgets.  Thingies that name a widget
 * are treated as being referenced.  The widget type, flags and pointer
 * are stored in a separate structure pointed to by the thingies.  Each
 * thingy also has a pointer to the `next' thingy (in a circular list)
 * that references the same widget.  The DISABLED flag is unset in these
 * thingies.
 */

/* Bind a widget to a thingy.  The thingy's reference count must already *
 * have been incremented.  The widget may already be bound to other      *
 * thingies; if it is not, then its `first' member must be NULL.  Return *
 * is 0 on success, or -1 if the thingy has the TH_IMMORTAL flag set.    */

/**/
static int
bindwidget(Widget w, Thingy t)
{
    if(t->flags & TH_IMMORTAL) {
	unrefthingy(t);
	return -1;
    }
    if(!(t->flags & DISABLED)) {
	if(t->widget == w)
	    return 0;
	unbindwidget(t, 1);
    }
    if(w->first) {
	t->samew = w->first->samew;
	w->first->samew = t;
    } else {
	w->first = t;
	t->samew = t;
    }
    t->widget = w;
    t->flags &= ~DISABLED;
    return 0;
}

/* Unbind a widget from a thingy.  This decrements the thingy's reference *
 * count.  The widget will be destroyed if this is its last name.         *
 * TH_IMMORTAL thingies won't be touched, unless override is non-zero.    *
 * Returns 0 on success, or -1 if the thingy is protected.  If the thingy *
 * doesn't actually reference a widget, this is considered successful.    */

/**/
static int
unbindwidget(Thingy t, int override)
{
    Widget w;

    if(t->flags & DISABLED)
	return 0;
    if(!override && (t->flags & TH_IMMORTAL))
	return -1;
    w = t->widget;
    if(t->samew == t)
	freewidget(w);
    else {
	Thingy p;
	for(p = w->first; p->samew != t; p = p->samew) ;
	w->first = p;   /* optimised for deletezlefunction() */
	p->samew = t->samew;
    }
    t->flags &= ~TH_IMMORTAL;
    t->flags |= DISABLED;
    unrefthingy(t);
    return 0;
}

/* Free a widget. */

/**/
static void
freewidget(Widget w)
{
    if(!(w->flags & WIDGET_INT))
	zsfree(w->u.fnnam);
    zfree(w, sizeof(*w));
}

/* Add am internal widget provided by a module.  The name given is the  *
 * canonical one, which must not begin with a dot.  The widget is first *
 * bound to the dotted canonical name; if that name is already taken by *
 * an internal widget, failure is indicated.  The same widget is then   *
 * bound to the canonical name, and a pointer to the widget structure   *
 * returned.                                                            */

/**/
Widget
addzlefunction(char *name, ZleIntFunc ifunc, int flags)
{
    VARARR(char, dotn, strlen(name) + 2);
    Widget w;
    Thingy t;

    if(name[0] == '.')
	return NULL;
    dotn[0] = '.';
    strcpy(dotn + 1, name);
    t = (Thingy) thingytab->getnode(thingytab, dotn);
    if(t && (t->flags & TH_IMMORTAL))
	return NULL;
    w = zalloc(sizeof(*w));
    w->flags = WIDGET_INT | flags;
    w->first = NULL;
    w->u.fn = ifunc;
    t = rthingy(dotn);
    bindwidget(w, t);
    t->flags |= TH_IMMORTAL;
    bindwidget(w, rthingy(name));
    return w;
}

#ifdef DYNAMIC

/* Delete an internal widget provided by a module.  Don't try to delete *
 * a widget from the fixed table -- it would be bad.  (Thanks, Egon.)   */

/**/
void
deletezlefunction(Widget w)
{
    Thingy p, n;

    p = w->first;
    while(1) {
	n = p->samew;
	if(n == p) {
	    unbindwidget(p, 1);
	    return;
	}
	unbindwidget(p, 1);
	p = n;
    }
}

#endif /* DYNAMIC */

/***************/
/* zle builtin */
/***************/

/*
 * The available operations are:
 *
 *   -l   list user-defined widgets (no arguments)
 *   -D   delete widget names
 *   -A   link the two named widgets (2 arguments)
 *   -N   create new user-defined widget (1 or 2 arguments)
 *        invoke a widget (1 argument)
 */

/**/
int
bin_zle(char *name, char **args, char *ops, int func)
{
    static struct opn {
	char o;
	int (*func) _((char *, char **, char *, char));
	int min, max;
    } const opns[] = {
	{ 'l', bin_zle_list, 0,  0 },
	{ 'D', bin_zle_del,  1, -1 },
	{ 'A', bin_zle_link, 2,  2 },
	{ 'N', bin_zle_new,  1,  2 },
	{ 0,   bin_zle_call, 0, -1 },
    };
    struct opn const *op, *opp;
    int n;

    /* select operation and ensure no clashing arguments */
    for(op = opns; op->o && !ops[op->o]; op++) ;
    if(op->o)
	for(opp = op; (++opp)->o; )
	    if(ops[opp->o]) {
		zerrnam(name, "incompatible operation selection options",
		    NULL, 0);
		return 1;
	    }

    /* check number of arguments */
    for(n = 0; args[n]; n++) ;
    if(!op->o && n != 1) {
	zerrnam(name, "wrong number of arguments", NULL, 0);
	return 1;
    }
    if(n < op->min) {
	zerrnam(name, "not enough arguments for -%c", NULL, op->o);
	return 1;
    } else if(op->max != -1 && n > op->max) {
	zerrnam(name, "too many arguments for -%c", NULL, op->o);
	return 1;
    }

    /* pass on the work to the operation function */
    return op->func(name, args, ops, op->o);
}

/**/
static int
bin_zle_list(char *name, char **args, char *ops, char func)
{
    scanhashtable(thingytab, 1, 0, DISABLED, scanlistwidgets, ops['L']);
    return 0;
}

/**/
static void
scanlistwidgets(HashNode hn, int list)
{
    Thingy t = (Thingy) hn;
    Widget w = t->widget;

    if(w->flags & WIDGET_INT)
	return;
    if(list) {
	fputs("zle -N ", stdout);
	if(t->nam[0] == '-')
	    fputs("-- ", stdout);
	quotedzputs(t->nam, stdout);
	if(strcmp(t->nam, w->u.fnnam)) {
	    fputc(' ', stdout);
	    quotedzputs(w->u.fnnam, stdout);
	}
    } else {
	nicezputs(t->nam, stdout);
	if(strcmp(t->nam, w->u.fnnam)) {
	    fputs(" (", stdout);
	    nicezputs(w->u.fnnam, stdout);
	    fputc(')', stdout);
	}
    }
    putchar('\n');
}

/**/
static int
bin_zle_del(char *name, char **args, char *ops, char func)
{
    int ret = 0;

    do {
	Thingy t = (Thingy) thingytab->getnode(thingytab, *args);
	if(!t) {
	    zwarnnam(name, "no such widget `%s'", *args, 0);
	    ret = 1;
	} else if(unbindwidget(t, 0)) {
	    zwarnnam(name, "widget name `%s' is protected", *args, 0);
	    ret = 1;
	}
    } while(*++args);
    return ret;
}

/**/
static int
bin_zle_link(char *name, char **args, char *ops, char func)
{
    Thingy t = (Thingy) thingytab->getnode(thingytab, args[0]);

    if(!t) {
	zerrnam(name, "no such widget `%s'", args[0], 0);
	return 1;
    } else if(bindwidget(t->widget, rthingy(args[1]))) {
	zerrnam(name, "widget name `%s' is protected", args[1], 0);
	return 1;
    }
    return 0;

}

/**/
static int
bin_zle_new(char *name, char **args, char *ops, char func)
{
    Widget w = zalloc(sizeof(*w));

    w->flags = 0;
    w->first = NULL;
    w->u.fnnam = ztrdup(args[1] ? args[1] : args[0]);
    if(!bindwidget(w, rthingy(args[0])))
	return 0;
    freewidget(w);
    zerrnam(name, "widget name `%s' is protected", args[0], 0);
    return 1;
}

/**/
static int
bin_zle_call(char *name, char **args, char *ops, char func)
{
    Thingy t;

    if(!zleactive || incompctlfunc) {
	zerrnam(name, "widgets can only be called when ZLE is active",
	    NULL, 0);
	return 1;
    }
    t = rthingy(args[0]);
    PERMALLOC {
      execzlefunc(t);
    } LASTALLOC;
    unrefthingy(t);
    return 0;
}

/*******************/
/* initialiasation */
/*******************/

/**/
void
init_thingies(void)
{
    Thingy t;

    createthingytab();
    for(t = thingies; t->nam; t++)
	thingytab->addnode(thingytab, t->nam, t);
}
