#!/bin/sh

# If the source directory is somewhere else, we need to force
# the shell to expand it in that directory, then strip it off.
install=
for file in $FUNCTIONS_INSTALL; do
  if test -f "$sdir/$file"; then
    install="$install $file"
  else
    install="$install `echo '' $sdir/$file | sed -e \"s% $sdir/% %g\"`"
  fi
done

fndir=$DESTDIR$fndir

for file in $install; do
  case $fndir in
    *$VERSION*)
       # Version specific function directory, safe to remove completely.
       # However, we don't remove the top-level version directory since
       # it could have other things than functions in it.  We could
       # do that instead in the top-level Makefile on a full uninstall,
       # if we wanted.
       rm -rf $fndir
       ;;
    *)
       if test -f $sdir/$file; then
	 if test x$FUNCTIONS_SUBDIRS != x -a x$FUNCTIONS_SUBDIRS != xno; then
	   rm -f $fndir/$file;
	 else
	   bfile="`echo $file | sed -e 's%^.*/%%'`"
	   rm -f "$fndir/$bfile"; \
	 fi
       fi
       ;;
  esac
done

exit 0
