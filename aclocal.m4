# Local additions to Autoconf macros.
# Copyright (C) 1992, 1994 Free Software Foundation, Inc.
# Francois Pinard <pinard@iro.umontreal.ca>, 1992.

# @defmac fp_PROG_CC_STDC
# @maindex PROG_CC_STDC
# @ovindex CC
# If the C compiler in not in ANSI C mode by default, try to add an option
# to output variable @code{CC} to make it so.  This macro tries various
# options that select ANSI C on some system or another.  It considers the
# compiler to be in ANSI C mode if it defines @code{__STDC__} to 1 and
# handles function prototypes correctly.
# 
# If you use this macro, you should check after calling it whether the C
# compiler has been set to accept ANSI C; if not, the shell variable
# @code{fp_cv_prog_cc_stdc} is set to @samp{no}.  If you wrote your source
# code in ANSI C, you can make an un-ANSIfied copy of it by using the
# program @code{ansi2knr}, which comes with Ghostscript.
# @end defmac

define(fp_PROG_CC_STDC,
[AC_CACHE_CHECK(for ${CC-cc} option to accept ANSI C,
fp_cv_prog_cc_stdc,
[fp_cv_prog_cc_stdc=no
ac_save_CFLAGS="$CFLAGS"
# Don't try gcc -ansi; that turns off useful extensions and
# breaks some systems' header files.
# AIX			-qlanglvl=ansi
# Ultrix and OSF/1	-std1
# HP-UX			-Aa -D_HPUX_SOURCE
# SVR4			-Xc
for ac_arg in "" -qlanglvl=ansi -std1 "-Aa -D_HPUX_SOURCE" -Xc
do
  CFLAGS="$ac_save_CFLAGS $ac_arg"
  AC_TRY_COMPILE(
[#ifndef __STDC__
choke me
#endif	
], [int test (int i, double x);
struct s1 {int (*f) (int a);};
struct s2 {int (*f) (double a);};],
[fp_cv_prog_cc_stdc="$ac_arg"; break])
done
CFLAGS="$ac_save_CFLAGS"
])
case "x$fp_cv_prog_cc_stdc" in
  x|xno) ;;
  *) CC="$CC $fp_cv_prog_cc_stdc" ;;
esac
])

builtin(include, aczsh.m4)
