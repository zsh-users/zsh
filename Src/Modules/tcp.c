/*
 * tcp.c - builtin FTP client
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 1998-2001 Peter Stephenson
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Peter Stephenson or the Zsh Development
 * Group be liable to any party for direct, indirect, special, incidental,
 * or consequential damages arising out of the use of this software and
 * its documentation, even if Peter Stephenson, and the Zsh
 * Development Group have been advised of the possibility of such damage.
 *
 * Peter Stephenson and the Zsh Development Group specifically
 * disclaim any warranties, including, but not limited to, the implied
 * warranties of merchantability and fitness for a particular purpose.  The
 * software provided hereunder is on an "as is" basis, and Peter Stephenson
 * and the Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

/*
 * We need to include the zsh headers later to avoid clashes with
 * the definitions on some systems, however we need the configuration
 * file to decide whether we can include netinet/in_systm.h, which
 * doesn't exist on cygwin.
 */
#include "tcp.h"

/*
 * For some reason, configure doesn't always detect netinet/in_systm.h.
 * On some systems, including linux, this seems to be because gcc is
 * throwing up a warning message about the redefinition of
 * __USE_LARGEFILE.  This means the problem is somewhere in the
 * header files where we can't get at it.  For now, revert to
 * not including this file only on systems where we know it's missing.
 * Currently this is just cygwin.
 */
#ifndef __CYGWIN__
# include <netinet/in_systm.h>
#endif
#include <netinet/in.h>
#include <netinet/ip.h>
#include <arpa/inet.h>

/* it's a TELNET based protocol, but don't think I like doing this */
#include <arpa/telnet.h>

/*
 * We use poll() in preference to select because some subset of manuals says
 * that's the thing to do, plus it's a bit less fiddly.  I don't actually
 * have access to a system with poll but not select, however, though
 * both bits of the code have been tested on a machine with both.
 */
#ifdef HAVE_POLL_H
# include <poll.h>
#endif
#if defined(HAVE_POLL) && !defined(POLLIN) && !defined(POLLNORM)
# undef HAVE_POLL
#endif

#ifdef USE_LOCAL_H_ERRNO
int h_errno;
#endif

/* We use the RFC 2553 interfaces.  If the functions don't exist in the library,
   simulate them. */

#ifndef INET_ADDRSTRLEN
# define INET_ADDRSTRLEN 16
#endif

#ifndef INET6_ADDRSTRLEN
# define INET6_ADDRSTRLEN 46
#endif

/**/
#ifndef HAVE_INET_NTOP

/**/
mod_export char const *
zsh_inet_ntop(int af, void const *cp, char *buf, size_t len)
{       
        if(af != AF_INET) {
                errno = EAFNOSUPPORT;
                return NULL;
        } 
        if(len < INET_ADDRSTRLEN) {
                errno = ENOSPC;
                return NULL;
        }
        strcpy(buf, inet_ntoa(*(struct in_addr *)cp));
        return buf;
}

/**/
#else /* !HAVE_INET_NTOP */

/**/
# define zsh_inet_ntop inet_ntop

/**/
#endif /* !HAVE_INET_NTOP */

/**/
#ifndef HAVE_INET_PTON

/**/
# ifndef HAVE_INET_ATON

#  ifndef INADDR_NONE
#   define INADDR_NONE 0xffffffffUL
#  endif

/**/
mod_export int zsh_inet_aton(char const *src, struct in_addr *dst)
{
    return (dst->s_addr = inet_addr(src)) != INADDR_NONE;
}

/**/
#else /* !HAVE_INET_ATON */

/**/
# define zsh_inet_aton inet_aton

/**/
# endif /* !HAVE_INET_ATON */

/**/
mod_export int
zsh_inet_pton(int af, char const *src, void *dst)
{
        if(af != AF_INET) {
                errno = EAFNOSUPPORT;
                return -1;
        }
        return !!zsh_inet_aton(src, dst);
}

#else /* !HAVE_INET_PTON */

# define zsh_inet_pton inet_pton

/**/
#endif /* !HAVE_INET_PTON */

/**/
#ifndef HAVE_GETIPNODEBYNAME

/**/
# ifndef HAVE_GETHOSTBYNAME2

/**/
mod_export struct hostent *
zsh_gethostbyname2(char const *name, int af)
{
	if(af != AF_INET) {
		h_errno = NO_RECOVERY;
		return NULL;
	}
	return gethostbyname(name);
}

/**/
#else /* !HAVE_GETHOSTBYNAME2 */

/**/
# define zsh_gethostbyname2 gethostbyname2

/**/
# endif /* !HAVE_GETHOSTBYNAME2 */

/* note: this is not a complete implementation.  If ignores the flags,
   and does not provide the memory allocation of the standard interface.
   Each returned structure will overwrite the previous one. */

/**/
mod_export struct hostent *
zsh_getipnodebyname(char const *name, int af, int flags, int *errorp)
{
	static struct hostent ahe;
	static char nbuf[16];
	static char *addrlist[] = { nbuf, NULL };
# ifdef SUPPORT_IPV6
	static char pbuf[INET6_ADDRSTRLEN];
# else
	static char pbuf[INET_ADDRSTRLEN];
# endif
	struct hostent *he;
	if(zsh_inet_pton(af, name, nbuf) == 1) {
		zsh_inet_ntop(af, nbuf, pbuf, sizeof(pbuf));
		ahe.h_name = pbuf;
		ahe.h_aliases = addrlist+1;
		ahe.h_addrtype = af;
		ahe.h_length = (af == AF_INET) ? 4 : 16;
		ahe.h_addr_list = addrlist;
		return &ahe;
	}
	he = zsh_gethostbyname2(name, af);
	if(!he)
		*errorp = h_errno;
	return he;
}

/**/
mod_export void
freehostent(struct hostent *ptr)
{
}

/**/
#else /* !HAVE_GETIPNODEBYNAME */

/**/
# define zsh_getipnodebyname getipnodebyname

/**/
#endif /* !HAVE_GETIPNODEBYNAME */

/**/
mod_export int
tcp_socket(int domain, int type, int protocol, Tcp_session sess)
{
    sess->fd = socket(domain, type, protocol);
    return sess->fd;
}

static void
tcp_cleanup(void)
{
}

/**/
mod_export int
tcp_close(Tcp_session sess)
{
    if(!close(sess->fd))
    {
	sess->fd = -1;
	return 0;
    }
       else return -1;
}

/**/
mod_export int
tcp_connect(Tcp_session sess, char *addrp, struct hostent *zhost, int d_port)
{
    int salen;
#ifdef SUPPORT_IPV6
    if(zhost->h_addrtype==AF_INET6) {
	memcpy(&(sess->peer.in6.sin6_addr), addrp, zhost->h_length);
	sess->peer.in6.sin6_port = d_port;
	sess->peer.in6.sin6_flowinfo = 0;
# ifdef HAVE_STRUCT_SOCKADDR_IN6_SIN6_SCOPE_ID
	sess->peer.in6.sin6_scope_id = 0;
# endif
	salen = sizeof(struct sockaddr_in6);
    } else
#endif /* SUPPORT_IPV6 */
    {
	memcpy(&(sess->peer.in.sin_addr), addrp, zhost->h_length);
	sess->peer.in.sin_port = d_port;
	salen = sizeof(struct sockaddr_in);
    }

    return connect(sess->fd, (struct sockaddr *)&(sess->peer), salen);
}

/* The load/unload routines required by the zsh library interface */

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
    return 0;
}

/**/
int
cleanup_(Module m)
{
    tcp_cleanup();
    return 0;
}

/**/
int
finish_(Module m)
{
    return 0;
}
