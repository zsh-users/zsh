#
# Basic Makefile definitions
#
# Copyright (c) 1995-1997 Richard Coleman
# All rights reserved.
#
# Permission is hereby granted, without written agreement and without
# license or royalty fees, to use, copy, modify, and distribute this
# software and to distribute modified versions of this software for any
# purpose, provided that the above copyright notice and the following
# two paragraphs appear in all copies of this software.
#
# In no event shall Richard Coleman or the Zsh Development Group be liable
# to any party for direct, indirect, special, incidental, or consequential
# damages arising out of the use of this software and its documentation,
# even if Richard Coleman and the Zsh Development Group have been advised of
# the possibility of such damage.
#
# Richard Coleman and the Zsh Development Group specifically disclaim any
# warranties, including, but not limited to, the implied warranties of
# merchantability and fitness for a particular purpose.  The software
# provided hereunder is on an "as is" basis, and Richard Coleman and the
# Zsh Development Group have no obligation to provide maintenance,
# support, updates, enhancements, or modifications.
#

# fundamentals
SHELL = /bin/sh
@SET_MAKE@

# source/build directories
VPATH           = @srcdir@
sdir            = @srcdir@
sdir_top        = @top_srcdir@

# installation directories
prefix          = @prefix@
exec_prefix     = @exec_prefix@
bindir          = @bindir@
libdir          = @libdir@
MODDIR          = $(libdir)/zsh/$(VERSION)
infodir         = @infodir@
mandir          = @mandir@

# compilation
CC              = @CC@
CPPFLAGS        = @CPPFLAGS@
DEFS            = @DEFS@
CFLAGS          = @CFLAGS@
LDFLAGS         = @LDFLAGS@
EXTRA_LDFLAGS   = @EXTRA_LDFLAGS@
DLCFLAGS        = @DLCFLAGS@
DLLDFLAGS       = @DLLDFLAGS@
LIBLDFLAGS      = @LIBLDFLAGS@
EXELDFLAGS      = @EXELDFLAGS@
LIBS            = @LIBS@
DL_EXT          = @DL_EXT@
DLLD            = @DLLD@

# utilities
AWK             = @AWK@
YODL            = @YODL@
YODL2TXT        = $(YODL)2txt
YODL2HTML       = $(YODL)2html

# install utility
INSTALL         = @INSTALL@
INSTALL_PROGRAM = @INSTALL_PROGRAM@
INSTALL_DATA    = @INSTALL_DATA@

# flags passed to recursive makes in subdirectories
MAKEDEFS = \
prefix='$(prefix)' exec_prefix='$(exec_prefix)' bindir='$(bindir)' \
libdir='$(libdir)' MODDIR='$(MODDIR)' infodir='$(infodir)' mandir='$(mandir)' \
CC='$(CC)' CPPFLAGS='$(CPPFLAGS)' DEFS='$(DEFS)' CFLAGS='$(CFLAGS)' \
LDFLAGS='$(LDFLAGS)' EXTRA_LDFLAGS='$(EXTRA_LDFLAGS)' \
DLCFLAGS='$(DLCFLAGS)' DLLDFLAGS='$(DLLDFLAGS)' \
LIBLDFLAGS='$(LIBLDFLAGS)' EXELDFLAGS='$(EXELDFLAGS)' \
LIBS='$(LIBS)' DL_EXT='$(DL_EXT)' DLLD='$(DLLD)' \
AWK='$(AWK)' YODL='$(YODL)' YODL2TXT='$(YODL2TXT)' YODL2HTML='$(YODL2HTML)'

# override built-in suffix list
.SUFFIXES:
