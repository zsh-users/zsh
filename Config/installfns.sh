#!/bin/sh

if test -d $fndir.old; then
  add_old=1
fi

$sdir_top/mkinstalldirs $DESTDIR$fndir || exit 1;

# If the source directory is somewhere else, we need to force
# the shell to expand it in that directory, then strip it off.
install=
for file in $FUNCTIONS_INSTALL; do
  if test -f "$sdir/$file"; then
    install="$install $file"
  else
    install="$install `echo '' $sdir/$file | sed -e \"s% $sdir/%%g\"`"
  fi
done

for file in $install; do
  if test -f $sdir/$file; then
    if test x$FUNCTIONS_SUBDIRS != x -a x$FUNCTIONS_SUBDIRS != xno; then
      subfile="$file"
      subdir="`echo $file | sed -e 's%/[^/]*$%%'`"
      olddir="$fndir.old/$subdir"
      instdir="$fndir/$subdir"
    else
      subfile="`echo $file | sed -e 's%^.*/%%'`"
      olddir="$fndir.old"
      instdir="$fndir"
    fi
    if test -f $fndir/$subfile; then
      if cmp $fndir/$subfile $sdir/$file >/dev/null; then :; else
	$sdir_top/mkinstalldirs $olddir
        mv $fndir/$subfile $olddir
        : ${add_old:=1}
      fi
    fi
    $sdir_top/mkinstalldirs $DESTDIR$instdir || exit 1
    $INSTALL_DATA $sdir/$file $DESTDIR$instdir || exit 1
  fi
done

if test x$add_old != x1; then
  rm -rf $fndir.old
fi

exit 0
