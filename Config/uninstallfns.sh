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
  if test -f $sdir/$file; then
    if test x$FUNCTIONS_SUBDIRS != x -a x$FUNCTIONS_SUBDIRS != xno; then
      rm -f $fndir/$file;
      if test -f $fndir.old/$file; then
	mv $fndir.old/$file $fndir/$file
      fi
    else
      bfile="`echo $file | sed -e 's%^.*/%%'`"
      rm -f "$fndir/$bfile"; \
      if test -f $fndir.old/$bfile; then
        mv $fndir.old/$bfile $fndir/$bfile
      fi
    fi
  fi
done

exit 0
