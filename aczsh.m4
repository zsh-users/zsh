dnl
dnl  Autconf tests for zsh.
dnl
dnl  Copyright (c) 1995-1997 Richard Coleman
dnl  All rights reserved.
dnl
dnl  Permission is hereby granted, without written agreement and without
dnl  license or royalty fees, to use, copy, modify, and distribute this
dnl  software and to distribute modified versions of this software for any
dnl  purpose, provided that the above copyright notice and the following
dnl  two paragraphs appear in all copies of this software.
dnl
dnl  In no event shall Richard Coleman or the Zsh Development Group be liable
dnl  to any party for direct, indirect, special, incidental, or consequential
dnl  damages arising out of the use of this software and its documentation,
dnl  even if Richard Coleman and the Zsh Development Group have been advised of
dnl  the possibility of such damage.
dnl
dnl  Richard Coleman and the Zsh Development Group specifically disclaim any
dnl  warranties, including, but not limited to, the implied warranties of
dnl  merchantability and fitness for a particular purpose.  The software
dnl  provided hereunder is on an "as is" basis, and Richard Coleman and the
dnl  Zsh Development Group have no obligation to provide maintenance,
dnl  support, updates, enhancements, or modifications.
dnl

dnl
dnl zsh_SYS_DYNAMIC_BROKEN
dnl   Check whether static/shared library linking is broken.
dnl
dnl   On some systems, static modifiable library symbols (such as environ)
dnl   may appear only in statically linked libraries.  If this is the case,
dnl   then two shared libraries that reference the same symbol, each linked
dnl   with the static library, could be given distinct copies of the symbol.
dnl   If this is the case then dynamic linking is FUBAR.
dnl

AC_DEFUN(zsh_SYS_DYNAMIC_BROKEN,
[AC_CACHE_CHECK([if static/shared library linking is broken],
zsh_cv_sys_dynamic_broken,
[if test "$zsh_cv_func_dlsym_needs_underscore" = yes; then
    us=_
else
    us=
fi
echo '
	extern char **environ;
	void *symlist1[] = {
		(void *)&environ,
		(void *)0
	};
' > conftest1.c
sed 's/symlist1/symlist2/' < conftest1.c > conftest2.c
if $CC -c $CFLAGS $CPPFLAGS $DLCFLAGS conftest1.c 1>&5 2>&5 &&
$DLLD -o conftest1.$DL_EXT $LDFLAGS $DLLDFLAGS conftest1.o $LIBS 1>&5 2>&5 &&
$CC -c $CFLAGS $CPPFLAGS $DLCFLAGS conftest2.c 1>&5 2>&5 &&
$DLLD -o conftest2.$DL_EXT $LDFLAGS $DLLDFLAGS conftest2.o $LIBS 1>&5 2>&5; then
    AC_TRY_RUN([
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#else
#include <sys/types.h>
#include <nlist.h>
#include <link.h>
#endif
#ifndef RTLD_LAZY
#define RTLD_LAZY 1
#endif
#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL 0
#endif

main()
{
    void *handle1, *handle2;
    void **symlist1, **symlist2;
    handle1 = dlopen("./conftest1.$DL_EXT", RTLD_LAZY | RTLD_GLOBAL);
    if(!handle1) exit(1);
    handle2 = dlopen("./conftest2.$DL_EXT", RTLD_LAZY | RTLD_GLOBAL);
    if(!handle2) exit(1);
    symlist1 = (void **) dlsym(handle1, "${us}symlist1");
    symlist2 = (void **) dlsym(handle2, "${us}symlist2");
    if(!symlist1 || !symlist2) exit(1);
    for(; *symlist1; symlist1++, symlist2++)
	if(*symlist1 != *symlist2)
	    exit(1);
    exit(0);
}
], [zsh_cv_sys_dynamic_broken=no],
[zsh_cv_sys_dynamic_broken=yes],
[zsh_cv_sys_dynamic_broken=yes]
)
else
    zsh_cv_sys_dynamic_broken=yes
fi
])
])

dnl
dnl zsh_SYS_DYNAMIC_CLASH
dnl   Check whether symbol name clashes in shared libraries are acceptable.
dnl

AC_DEFUN(zsh_SYS_DYNAMIC_CLASH,
[AC_CACHE_CHECK([if name clashes in shared objects are OK],
zsh_cv_sys_dynamic_clash_ok,
[if test "$zsh_cv_func_dlsym_needs_underscore" = yes; then
    us=_
else
    us=
fi
echo 'int fred () { return 42; }' > conftest1.c
echo 'int fred () { return 69; }' > conftest2.c
if $CC -c $CFLAGS $CPPFLAGS $DLCFLAGS conftest1.c 1>&5 2>&5 &&
$DLLD -o conftest1.$DL_EXT $LDFLAGS $DLLDFLAGS conftest1.o $LIBS 1>&5 2>&5 &&
$CC -c $CFLAGS $CPPFLAGS $DLCFLAGS conftest2.c 1>&5 2>&5 &&
$DLLD -o conftest2.$DL_EXT $LDFLAGS $DLLDFLAGS conftest2.o $LIBS 1>&5 2>&5; then
    AC_TRY_RUN([
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#else
#include <sys/types.h>
#include <nlist.h>
#include <link.h>
#endif
#ifndef RTLD_LAZY
#define RTLD_LAZY 1
#endif
#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL 0
#endif

main()
{
    void *handle1, *handle2;
    int (*fred1)(), (*fred2)();
    handle1 = dlopen("./conftest1.$DL_EXT", RTLD_LAZY | RTLD_GLOBAL);
    if(!handle1) exit(1);
    handle2 = dlopen("./conftest2.$DL_EXT", RTLD_LAZY | RTLD_GLOBAL);
    if(!handle2) exit(1);
    fred1 = (int (*)()) dlsym(handle1, "${us}fred");
    fred2 = (int (*)()) dlsym(handle2, "${us}fred");
    if(!fred1 || !fred2) exit(1);
    exit((*fred1)() != 42 || (*fred2)() != 69);
}
], [zsh_cv_sys_dynamic_clash_ok=yes],
[zsh_cv_sys_dynamic_clash_ok=no],
[zsh_cv_sys_dynamic_clash_ok=no]
)
else
    zsh_cv_sys_dynamic_clash_ok=no
fi
])
if test "$zsh_cv_sys_dynamic_clash_ok" = yes; then
    AC_DEFINE(DYNAMIC_NAME_CLASH_OK)
fi
])

dnl
dnl zsh_SYS_DYNAMIC_GLOBAL
dnl   Check whether symbols in one dynamically loaded library are
dnl   available to another dynamically loaded library.
dnl

AC_DEFUN(zsh_SYS_DYNAMIC_GLOBAL,
[AC_CACHE_CHECK([for working RTLD_GLOBAL],
zsh_cv_sys_dynamic_rtld_global,
[if test "$zsh_cv_func_dlsym_needs_underscore" = yes; then
    us=_
else
    us=
fi
echo 'int fred () { return 42; }' > conftest1.c
echo 'extern int fred(); int barney () { return fred() + 27; }' > conftest2.c
if $CC -c $CFLAGS $CPPFLAGS $DLCFLAGS conftest1.c 1>&5 2>&5 &&
$DLLD -o conftest1.$DL_EXT $LDFLAGS $DLLDFLAGS conftest1.o $LIBS 1>&5 2>&5 &&
$CC -c $CFLAGS $CPPFLAGS $DLCFLAGS conftest2.c 1>&5 2>&5 &&
$DLLD -o conftest2.$DL_EXT $LDFLAGS $DLLDFLAGS conftest2.o $LIBS 1>&5 2>&5; then
    AC_TRY_RUN([
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#else
#include <sys/types.h>
#include <nlist.h>
#include <link.h>
#endif
#ifndef RTLD_LAZY
#define RTLD_LAZY 1
#endif
#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL 0
#endif

main()
{
    void *handle;
    int (*barneysym)();
    handle = dlopen("./conftest1.$DL_EXT", RTLD_LAZY | RTLD_GLOBAL);
    if(!handle) exit(1);
    handle = dlopen("./conftest2.$DL_EXT", RTLD_LAZY | RTLD_GLOBAL);
    if(!handle) exit(1);
    barneysym = (int (*)()) dlsym(handle, "${us}barney");
    if(!barneysym) exit(1);
    exit((*barneysym)() != 69);
}
], [zsh_cv_sys_dynamic_rtld_global=yes],
[zsh_cv_sys_dynamic_rtld_global=no],
[zsh_cv_sys_dynamic_rtld_global=no]
)
else
    zsh_cv_sys_dynamic_rtld_global=no
fi
])
])

dnl
dnl zsh_SYS_DYNAMIC_EXECSYMS
dnl   Check whether symbols in the executable are available to dynamically
dnl   loaded libraries.
dnl

AC_DEFUN(zsh_SYS_DYNAMIC_EXECSYMS,
[AC_CACHE_CHECK([whether symbols in the executable are available],
zsh_cv_sys_dynamic_execsyms,
[if test "$zsh_cv_func_dlsym_needs_underscore" = yes; then
    us=_
else
    us=
fi
echo 'extern int fred(); int barney () { return fred() + 27; }' > conftest1.c
if $CC -c $CFLAGS $CPPFLAGS $DLCFLAGS conftest1.c 1>&5 2>&5 &&
$DLLD -o conftest1.$DL_EXT $LDFLAGS $DLLDFLAGS conftest1.o $LIBS 1>&5 2>&5; then
    save_ldflags=$LDFLAGS
    LDFLAGS="$LDFLAGS $EXTRA_LDFLAGS"
    AC_TRY_RUN([
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#else
#include <sys/types.h>
#include <nlist.h>
#include <link.h>
#endif
#ifndef RTLD_LAZY
#define RTLD_LAZY 1
#endif
#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL 0
#endif

main()
{
    void *handle;
    int (*barneysym)();
    handle = dlopen("./conftest1.$DL_EXT", RTLD_LAZY | RTLD_GLOBAL);
    if(!handle) exit(1);
    barneysym = (int (*)()) dlsym(handle, "${us}barney");
    if(!barneysym) exit(1);
    exit((*barneysym)() != 69);
}

int fred () { return 42; }
], [zsh_cv_sys_dynamic_execsyms=yes],
[zsh_cv_sys_dynamic_execsyms=no],
[zsh_cv_sys_dynamic_execsyms=no]
)
    LDFLAGS=$save_ldflags
else
    zsh_cv_sys_dynamic_execsyms=no
fi
])
])

dnl
dnl zsh_SYS_DYNAMIC_STRIP_EXE
dnl   Check whether it is safe to strip executables.
dnl

AC_DEFUN(zsh_SYS_DYNAMIC_STRIP_EXE,
[AC_REQUIRE([zsh_SYS_DYNAMIC_EXECSYMS])
AC_CACHE_CHECK([whether executables can be stripped],
zsh_cv_sys_dynamic_strip_exe,
[if test "$zsh_cv_sys_dynamic_execsyms" != yes; then
    zsh_cv_sys_dynamic_strip_exe=yes
elif
    if test "$zsh_cv_func_dlsym_needs_underscore" = yes; then
	us=_
    else
	us=
    fi
    echo 'extern int fred(); int barney() { return fred() + 27; }' > conftest1.c
    $CC -c $CFLAGS $CPPFLAGS $DLCFLAGS conftest1.c 1>&5 2>&5 &&
    $DLLD -o conftest1.$DL_EXT $LDFLAGS $DLLDFLAGS conftest1.o $LIBS 1>&5 2>&5; then
    save_ldflags=$LDFLAGS
    LDFLAGS="$LDFLAGS $EXTRA_LDFLAGS -s"
    AC_TRY_RUN([
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#else
#include <sys/types.h>
#include <nlist.h>
#include <link.h>
#endif
#ifndef RTLD_LAZY
#define RTLD_LAZY 1
#endif
#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL 0
#endif

main()
{
    void *handle;
    int (*barneysym)();
    handle = dlopen("./conftest1.$DL_EXT", RTLD_LAZY | RTLD_GLOBAL);
    if(!handle) exit(1);
    barneysym = (int (*)()) dlsym(handle, "${us}barney");
    if(!barneysym) exit(1);
    exit((*barneysym)() != 69);
}

int fred () { return 42; }
], [zsh_cv_sys_dynamic_strip_exe=yes],
[zsh_cv_sys_dynamic_strip_exe=no],
[zsh_cv_sys_dynamic_strip_exe=no]
)
    LDFLAGS=$save_ldflags
else
    zsh_cv_sys_dynamic_strip_exe=no
fi
])
])

dnl
dnl zsh_SYS_DYNAMIC_STRIP_EXE
dnl   Check whether it is safe to strip dynamically loaded libraries.
dnl

AC_DEFUN(zsh_SYS_DYNAMIC_STRIP_LIB,
[AC_CACHE_CHECK([whether libraries can be stripped],
zsh_cv_sys_dynamic_strip_lib,
[if test "$zsh_cv_func_dlsym_needs_underscore" = yes; then
    us=_
else
    us=
fi
echo 'int fred () { return 42; }' > conftest1.c
if $CC -c $CFLAGS $CPPFLAGS $DLCFLAGS conftest1.c 1>&5 2>&5 &&
$DLLD -o conftest1.$DL_EXT $LDFLAGS $DLLDFLAGS -s conftest1.o $LIBS 1>&5 2>&5; then
    AC_TRY_RUN([
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#else
#include <sys/types.h>
#include <nlist.h>
#include <link.h>
#endif
#ifndef RTLD_LAZY
#define RTLD_LAZY 1
#endif
#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL 0
#endif

main()
{
    void *handle;
    int (*fredsym)();
    handle = dlopen("./conftest1.$DL_EXT", RTLD_LAZY | RTLD_GLOBAL);
    if(!handle) exit(1);
    fredsym = (int (*)()) dlsym(handle, "${us}fred");
    if(!fredsym) exit(1);
    exit((*fredsym)() != 42);
}
], [zsh_cv_sys_dynamic_strip_lib=yes],
[zsh_cv_sys_dynamic_strip_lib=no],
[zsh_cv_sys_dynamic_strip_lib=no]
)
else
    zsh_cv_sys_dynamic_strip_lib=no
fi
])
])

dnl
dnl zsh_PATH_UTMP(filename)
dnl   Search for a specified utmp-type file.
dnl

AC_DEFUN(zsh_PATH_UTMP,
[AC_CACHE_CHECK([for $1 file], [zsh_cv_path_$1],
[for dir in /etc /usr/etc /var/adm /usr/adm /var/run ./conftest; do
  zsh_cv_path_$1=${dir}/$1
  test -f $zsh_cv_path_$1 && break
  zsh_cv_path_$1=no
done
])
if test $zsh_cv_path_$1 != no; then
  AC_DEFINE_UNQUOTED(PATH_[]translit($1, [a-z], [A-Z])[]_FILE, "$zsh_cv_path_$1")
fi
])

dnl
dnl zsh_TYPE_EXISTS(#includes, type name)
dnl   Check whether a specified type exists.
dnl

AC_DEFUN(zsh_TYPE_EXISTS,
[AC_CACHE_CHECK([for $2], [zsh_cv_type_exists_[]translit($2, [ ], [_])],
[AC_TRY_COMPILE([$1], [$2 testvar;],
[zsh_cv_type_exists_[]translit($2, [ ], [_])=yes],
[zsh_cv_type_exists_[]translit($2, [ ], [_])=no])
])
if test $zsh_cv_type_exists_[]translit($2, [ ], [_]) = yes; then
  AC_DEFINE(HAVE_[]translit($2, [ a-z], [_A-Z]))
fi
])

dnl
dnl zsh_STRUCT_MEMBER(#includes, type name, member name)
dnl   Check whether a specified aggregate type exists and contains
dnl   a specified member.
dnl

AC_DEFUN(zsh_STRUCT_MEMBER,
[AC_CACHE_CHECK([for $3 in $2], [zsh_cv_struct_member_[]translit($2, [ ], [_])_$3],
[AC_TRY_COMPILE([$1], [$2 testvar; testvar.$3;],
[zsh_cv_struct_member_[]translit($2, [ ], [_])_$3=yes],
[zsh_cv_struct_member_[]translit($2, [ ], [_])_$3=no])
])
if test $zsh_cv_struct_member_[]translit($2, [ ], [_])_$3 = yes; then
  AC_DEFINE(HAVE_[]translit($2_$3, [ a-z], [_A-Z]))
fi
])
