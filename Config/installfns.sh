#!/bin/sh

fndir=$DESTDIR$fndir

/bin/sh $sdir_top/mkinstalldirs $fndir || exit 1;

allfuncs="`grep ' functions=.' ${dir_top}/config.modules |
  sed -e '/^#/d' -e '/ link=no/d' -e 's/^.* functions=//'`"

allfuncs="`cd $sdir_top; echo ${allfuncs}`"

# We now have a list of files, but we need to use `test -f' to check
# (1) the glob got expanded (2) we are not looking at directories.
for file in $allfuncs; do
  if test -f $sdir_top/$file; then
    case "$file" in
      */CVS/*) continue;;
    esac
    if test x$FUNCTIONS_SUBDIRS != x -a x$FUNCTIONS_SUBDIRS != xno; then
      case "$file" in
      Completion/*/*)
        subdir="`echo $file | sed -e 's%/[^/]*/[^/]*$%%'`"
        instdir="$fndir/$subdir"
        ;;
      Completion/*)
        instdir="$fndir/Completion"
        ;;
      *)
        subdir="`echo $file | sed -e 's%/[^/]*$%%' -e 's%^Functions/%%'`"
        instdir="$fndir/$subdir"
        ;;
      esac
    else
      instdir="$fndir"
    fi
    test -d $instdir || /bin/sh $sdir_top/mkinstalldirs $instdir || exit 1
    $INSTALL_DATA $sdir_top/$file $instdir || exit 1
  fi
done
