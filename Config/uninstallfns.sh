#!/bin/sh

fndir=$DESTDIR$fndir

allfuncs="`grep ' functions=' ${dir_top}/config.modules |
  sed -e '/^#/d' -e '/ link=no/d' -e 's/^.* functions=//'`"

allfuncs="`cd ${sdir_top}; echo ${allfuncs}`"

case $fndir in
  *$VERSION*)
     # Version specific function directory, safe to remove completely.
     # However, we don't remove the top-level version directory since
     # it could have other things than functions in it.  We could
     # do that instead in the top-level Makefile on a full uninstall,
     # if we wanted.
     rm -rf $fndir
     ;;
  *) # The following will only apply with a custom install directory
     # with no version information.  This is rather undesirable.
     # But let's try and do the best we can.
     # We now have a list of files, but we need to use `test -f' to check
     # (1) the glob got expanded (2) we are not looking at directories.
     for file in $allfuncs; do
       if test -f $sdir_top/$file; then
	 if test x$FUNCTIONS_SUBDIRS != x -a x$FUNCTIONS_SUBDIRS != xno; then
	   file=`echo $file | sed -e 's%%^Functions/%'`
	   rm -f $fndir/$file;
	 else
	   bfile="`echo $file | sed -e 's%^.*/%%'`"
	   rm -f "$fndir/$bfile"; \
	 fi
       fi
     done
     ;;
esac

exit 0
