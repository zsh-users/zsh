#!/bin/sh

fndir=$DESTDIR$fndir

$sdir_top/mkinstalldirs $fndir || exit 1;

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

for file in $install; do
  if test -f $sdir/$file; then
    if test x$FUNCTIONS_SUBDIRS != x -a x$FUNCTIONS_SUBDIRS != xno; then
      subfile="$file"
      subdir="`echo $file | sed -e 's%/[^/]*$%%'`"
      instdir="$fndir/$subdir"
    else
      subfile="`echo $file | sed -e 's%^.*/%%'`"
      instdir="$fndir"
    fi
    $sdir_top/mkinstalldirs $instdir || exit 1
    $INSTALL_DATA $sdir/$file $instdir || exit 1
  fi
done

exit 0
