#! /bin/sh
#
# mkbltnmlst.sh: generate boot code for linked-in modules
#
# Written by Andrew Main
#

srcdir=${srcdir-`echo $0|sed 's%/[^/][^/]*$%%'`}
test "x$srcdir" = "x$0" && srcdir=.
test "x$srcdir" = "x"   && srcdir=.
CFMOD=${CFMOD-$srcdir/../config.modules}

bin_mods="`grep ' link=static' $CFMOD | sed -e '/^#/d' \
-e 's/ .*/ /' -e 's/^name=/ /'`"

x_mods="`grep ' load=yes' $CFMOD | sed -e '/^#/d' -e '/ link=no/d' \
-e 's/ .*/ /' -e 's/^name=/ /'`"

trap "rm -f $1; exit 1" 1 2 15

exec > $1

for x_mod in $x_mods; do
    modfile="`grep '^name='$x_mod' ' $CFMOD | sed -e 's/^.* modfile=//' \
      -e 's/ .*//'`"
    if test "x$modfile" = x; then
	echo >&2 "WARNING: no name for \`$x_mod' in $CFMOD (ignored)"
	continue
    fi
    case "$bin_mods" in
    *" $x_mod "*)
        echo "/* linked-in known module \`$x_mod' */"
	linked=yes
	;;
    *)
        echo "#ifdef DYNAMIC"
        echo "/* non-linked-in known module \`$x_mod' */"
	linked=no
    esac
    unset moddeps autobins autoinfixconds autoprefixconds autoparams
    unset automathfuncs
    . $srcdir/../$modfile
    echo "  if (emulation == EMULATE_ZSH) {"
    for bin in $autobins; do
	echo "    add_autobin(\"$bin\", \"$x_mod\");"
    done
    for cond in $autoinfixconds; do
	echo "    add_autocond(\"$cond\", 1, \"$x_mod\");"
    done
    for cond in $autoprefixconds; do
	echo "    add_autocond(\"$cond\", 0, \"$x_mod\");"
    done
    for param in $autoparams; do
	echo "    add_autoparam(\"$param\", \"$x_mod\");"
    done
    for mfunc in $automathfuncs; do
	echo "    add_automath(\"$mfunc\", \"$x_mod\");"
    done
    echo "  }"
    for dep in $moddeps; do
	echo "  add_dep(\"$x_mod\", \"$dep\");"
    done
    test "x$linked" = xno && echo "#endif"
done

echo
done_mods=" "
for bin_mod in $bin_mods; do
    q_bin_mod=`echo $bin_mod | sed 's,Q,Qq,g;s,_,Qu,g;s,/,Qs,g'`
    modfile="`grep '^name='$bin_mod' ' $CFMOD | sed -e 's/^.* modfile=//' \
      -e 's/ .*//'`"
    echo "/* linked-in module \`$bin_mod' */"
    unset moddeps
    . $srcdir/../$modfile
    for dep in $moddeps; do
	# This assumes there are no circular dependencies in the builtin
	# modules.  Better ordering of config.modules would be necessary
	# to enforce stricter dependency checking.
	case $bin_mods in
	    *" $dep "*)
		echo "    /* depends on \`$dep' */" ;;
	    *)	echo >&2 "ERROR: linked-in module \`$bin_mod' depends on \`$dep'"
		rm -f $1
		exit 1 ;;
	esac
    done
    echo "    {"
    echo "        extern int setup_${q_bin_mod} _((Module));"
    echo "        extern int boot_${q_bin_mod} _((Module));"
    echo "        extern int cleanup_${q_bin_mod} _((Module));"
    echo "        extern int finish_${q_bin_mod} _((Module));"
    echo
    echo "        register_module(\"$bin_mod\","
    echo "                        setup_${q_bin_mod}, boot_${q_bin_mod},"
    echo "                        cleanup_${q_bin_mod}, finish_${q_bin_mod});"
    echo "    }"
    done_mods="$done_mods$bin_mod "
done
