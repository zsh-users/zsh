#! /bin/sh
#
# mkbltnmlst.sh: generate boot code for linked-in modules
#
# Written by Andrew Main
#
srcdir=${srcdir-`echo $0|sed 's%/[^/][^/]*$%%'`}
test "x$srcdir" = "x$0" && srcdir=.
test "x$srcdir" = "x"   && srcdir=.
MODBINS=${MODBINS-modules-bltin}
XMODCF=${XMODCF-$srcdir/xmods.conf}

bin_mods=" zsh "`sed 's/^/ /;s/$/ /' $MODBINS`
x_mods=`cat $XMODCF`
. ./modules.index

trap "rm -f $1; exit 1" 1 2 15

exec > $1

echo "#ifdef DYNAMIC"
for x_mod in $x_mods; do
    case $bin_mods in
	*" $x_mod "*) ;;
	*)  echo "/* non-linked-in known module \`$x_mod' */"
	    eval "loc=\$loc_$x_mod"
	    unset moddeps autobins
	    . $srcdir/../$loc/${x_mod}.mdd
	    for bin in $autobins; do
		echo "    add_autobin(\"$bin\", \"$x_mod\");"
	    done
	    for dep in $moddeps; do
		case $bin_mods in
		    *" $dep "*)
			echo "    /* depends on \`$dep' */" ;;
		    *)	echo "    add_dep(\"$x_mod\", \"$dep\");" ;;
		esac
	    done ;;
    esac
done
echo "#endif /* DYNAMIC */"
echo
done_mods=" "
for bin_mod in $bin_mods; do
    echo "/* linked-in module \`$bin_mod' */"
    eval "loc=\$loc_$bin_mod"
    unset moddeps
    . $srcdir/../$loc/${bin_mod}.mdd
    for dep in $moddeps; do
	case $done_mods in
	    *" $dep "*)
		echo "    /* depends on \`$dep' */" ;;
	    *)	echo >&2 "ERROR: linked-in module \`$bin_mod' depends on \`$dep'"
		rm -f $1
		exit 1 ;;
	esac
    done
    echo "    mod.nam = \"$bin_mod\"; boot_$bin_mod(&mod);"
    done_mods="$done_mods$bin_mod "
done
