/*
 * linklist.c - linked lists
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

#include "zsh.mdh"
#include "linklist.pro"

/* Get an empty linked list header */

/**/
LinkList
newlinklist(void)
{
    LinkList list;

    list = (LinkList) alloc(sizeof *list);
    list->first = NULL;
    list->last = (LinkNode) list;
    return list;
}

/* Insert a node in a linked list after a given node */

/**/
LinkNode
insertlinknode(LinkList list, LinkNode node, void *dat)
{
    LinkNode tmp, new;

    tmp = node->next;
    node->next = new = (LinkNode) alloc(sizeof *tmp);
    new->last = node;
    new->dat = dat;
    new->next = tmp;
    if (tmp)
	tmp->last = new;
    else
	list->last = new;
    return new;
}

/* Insert an already-existing node into a linked list after a given node */

/**/
LinkNode
uinsertlinknode(LinkList list, LinkNode node, LinkNode new)
{
    LinkNode tmp = node->next;
    node->next = new;
    new->last = node;
    new->next = tmp;
    if (tmp)
	tmp->last = new;
    else
	list->last = new;
    return new;
}

/* Insert a list in another list */

/**/
void
insertlinklist(LinkList l, LinkNode where, LinkList x)
{
    LinkNode nx;

    nx = where->next;
    if (!l->first)
	return;
    where->next = l->first;
    l->last->next = nx;
    l->first->last = where;
    if (nx)
	nx->last = l->last;
    else
	x->last = l->last;
}

/* Get top node in a linked list */

/**/
void *
getlinknode(LinkList list)
{
    void *dat;
    LinkNode node;

    if (!(node = list->first))
	return NULL;
    dat = node->dat;
    list->first = node->next;
    if (node->next)
	node->next->last = (LinkNode) list;
    else
	list->last = (LinkNode) list;
    zfree(node, sizeof(struct linknode));
    return dat;
}

/* Get top node in a linked list without freeing */

/**/
void *
ugetnode(LinkList list)
{
    void *dat;
    LinkNode node;

    if (!(node = list->first))
	return NULL;
    dat = node->dat;
    list->first = node->next;
    if (node->next)
	node->next->last = (LinkNode) list;
    else
	list->last = (LinkNode) list;
    return dat;
}

/* Remove a node from a linked list */

/**/
void *
remnode(LinkList list, LinkNode nd)
{
    void *dat;

    nd->last->next = nd->next;
    if (nd->next)
	nd->next->last = nd->last;
    else
	list->last = nd->last;
    dat = nd->dat;
    zfree(nd, sizeof(struct linknode));

    return dat;
}

/* Remove a node from a linked list without freeing */

/**/
void *
uremnode(LinkList list, LinkNode nd)
{
    void *dat;

    nd->last->next = nd->next;
    if (nd->next)
	nd->next->last = nd->last;
    else
	list->last = nd->last;
    dat = nd->dat;
    return dat;
}

/* Free a linked list */

/**/
void
freelinklist(LinkList list, FreeFunc freefunc)
{
    LinkNode node, next;

    for (node = list->first; node; node = next) {
	next = node->next;
	if (freefunc)
	    freefunc(node->dat);
	zfree(node, sizeof(struct linknode));
    }
    zfree(list, sizeof(struct linklist));
}

/* Count the number of nodes in a linked list */

/**/
int
countlinknodes(LinkList list)
{
    LinkNode nd;
    int ct = 0;

    for (nd = firstnode(list); nd; incnode(nd), ct++);
    return ct;
}

/**/
void
rolllist(LinkList l, LinkNode nd)
{
    l->last->next = l->first;
    l->first->last = l->last;
    l->first = nd;
    l->last = nd->last;
    nd->last = (LinkNode) l;
    l->last->next = 0;
}

