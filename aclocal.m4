# Local additions to Autoconf macros.
# Copyright (C) 1992, 1994 Free Software Foundation, Inc.
# Francois Pinard <pinard@iro.umontreal.ca>, 1992.

AC_DEFUN(AC_PROG_LN,
[AC_MSG_CHECKING(whether ln works)
AC_CACHE_VAL(ac_cv_prog_LN,
[rm -f conftestdata conftestlink
echo > conftestdata
if ln conftestdata conftestlink 2>/dev/null
then
  rm -f conftestdata conftestlink
  ac_cv_prog_LN="ln"
else
  rm -f conftestdata
  ac_cv_prog_LN="cp"
fi])dnl
LN="$ac_cv_prog_LN"
if test "$ac_cv_prog_LN" = "ln"; then
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
fi
AC_SUBST(LN)dnl
])

builtin(include, aczsh.m4)
