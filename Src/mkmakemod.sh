#!/bin/sh
#
# mkmakemod.sh: generate Makefile.in files for module building
#
# Options:
#   -m = file is already generated; only build the second stage
#   -i = do not build second stage
#
# Args:
#   $1 = subdirectory to look in, relative to $top_srcdir
#   $2 = final output filename, within the $1 directory
#
# This script must be run from the top-level build directory, and $top_srcdir
# must be set correctly in the environment.
#
# This looks in $1, and uses all the *.mdd files there.  Each .mdd file
# defines one module.  The .mdd file is actually a shell script, which will
# be sourced.  It may define the following shell variables:
#
#   moddeps       modules on which this module depends (default none)
#   nozshdep      non-empty indicates no dependence on the `zsh' pseudo-module
#   alwayslink    if non-empty, always link the module into the executable
#   autobins      builtins defined by the module, for autoloading
#   objects       .o files making up this module (*must* be defined)
#   proto         .pro files for this module (default generated from $objects)
#   headers       extra headers for this module (default none)
#   hdrdeps       extra headers on which the .mdh depends (default none)
#   otherincs     extra headers that are included indirectly (default none)
#
# The .mdd file may also include a Makefile.in fragment between lines
# `:<<\Make' and `Make' -- this will be copied into Makemod.in.
#
# The resulting Makemod.in knows how to build each module that is defined.
# For each module in also knows how to build a .mdh file.  Each source file
# should #include the .mdh file for the module it is a part of.  The .mdh
# file #includes the .mdh files for any module dependencies, then each of
# $headers, and then each of $proto (for global declarations).  It will
# be recreated if any of the dependency .mdh files changes, or if any of
# $headers or $hdrdeps changes.  When anything depends on it, all of $proto
# and $otherincs will be made up to date, but the .mdh file won't actually
# be rebuilt if those files change.
#
# The order of sections of the output file is thus:
#   simple generated macros
#   macros generated from *.mdd
#   included Makemod.in.in
#   rules generated from *.mdd
# The order dependencies are basically that the generated macros are required
# in Makemod.in.in, but some of the macros that it creates are needed in the
# later rules.
#

# sed script to normalise a pathname
sed_normalise='
    s,^,/,
    s,$,/,
    :1
    s,/\./,/,
    t1
    :2
    s,/[^/.][^/]*/\.\./,/,
    s,/\.[^/.][^/]*/\.\./,/,
    s,/\.\.[^/][^/]*/\.\./,/,
    t2
    s,^/$,.,
    s,^/,,
    s,\(.\)/$,\1,
'

# decide which stages to process
first_stage=true
second_stage=true
if test ."$1" = .-m; then
    shift
    first_stage=false
elif test ."$1" = .-i; then
    shift
    second_stage=false
fi

top_srcdir=`echo $top_srcdir | sed "$sed_normalise"`
the_subdir=$1
the_makefile=$2

if $first_stage; then

    trap "rm -f $the_subdir/${the_makefile}.in" 1 2 15
    echo "creating $the_subdir/${the_makefile}.in"
    exec 3>&1 >$the_subdir/${the_makefile}.in
    echo "##### ${the_makefile}.in generated automatically by mkmakemod.sh"
    echo "##### DO NOT EDIT!"
    echo
    echo "##### ===== DEFINITIONS ===== #####"
    echo
    echo "makefile = ${the_makefile}"
    echo "dir_top = "`echo $the_subdir | sed 's,[^/][^/]*,..,g'`
    echo "subdir = $the_subdir"
    echo

    . Src/modules.index
    bin_mods=" zsh "`sed 's/^/ /;s/$/ /' Src/modules-bltin`
    if grep '%@D@%D%' config.status >/dev/null; then
	is_dynamic=true
    else
	is_dynamic=false
    fi

    here_modules=
    all_subdirs=
    all_modobjs=
    all_modules=
    all_mdds=
    all_mdhs=
    all_proto=
    lastsub=//
    for module in $module_list; do
	eval "loc=\$loc_$module"
	case $loc in
	    $the_subdir)
		here_modules="$here_modules $module"
		build=$is_dynamic
		case $is_dynamic@$bin_mods in
		    *" $module "*)
			build=true
			all_modobjs="$all_modobjs modobjs.${module}" ;;
		    true@*)
			all_modules="$all_modules ${module}.\$(DL_EXT)" ;;
		esac
		all_mdds="$all_mdds ${module}.mdd"
		$build && all_mdhs="$all_mdhs ${module}.mdh"
		$build && all_proto="$all_proto proto.${module}"
		;;
	    $lastsub | $lastsub/*) ;;
	    $the_subdir/*)
		all_subdirs="$all_subdirs $loc"
		lastsub=$loc
		;;
	esac
    done
    all_subdirs=`echo "$all_subdirs" | sed "s' $the_subdir/' 'g"`
    echo "MODOBJS =$all_modobjs"
    echo "MODULES =$all_modules"
    echo "MDDS    =$all_mdds"
    echo "MDHS    =$all_mdhs"
    echo "PROTOS  =$all_proto"
    echo "SUBDIRS =$all_subdirs"
    echo

    echo "##### ===== INCLUDING Makemod.in.in ===== #####"
    echo
    cat $top_srcdir/Src/Makemod.in.in
    echo

    case $the_subdir in
	Src) modobjs_sed= ;;
	Src/*) modobjs_sed="| sed 's\" \" "`echo $the_subdir | sed 's,^Src/,,'`"/\"g' " ;;
	*) modobjs_sed="| sed 's\" \" ../$the_subdir/\"g' " ;;
    esac

    other_mdhs=
    remote_mdhs=
    for module in $here_modules; do

	unset moddeps nozshdep alwayslink
	unset autobins
	unset objects proto headers hdrdeps otherincs
	. $top_srcdir/$the_subdir/${module}.mdd
	test -n "${moddeps+set}" || moddeps=
	test -n "$nozshdep" || moddeps="$moddeps zsh"
	test -n "${proto+set}" ||
	    proto=`echo $objects '' | sed 's,\.o ,.pro ,g'`

	dobjects=`echo $objects '' | sed 's,\.o ,..o ,g'`
	modhdeps=
	for dep in $moddeps; do
	    eval "loc=\$loc_$dep"
	    case $the_subdir in
		$loc)
		    mdh="${dep}.mdh"
		    ;;
		$loc/*)
		    mdh="\$(dir_top)/$loc/${dep}.mdh"
		    case "$other_mdhs " in
			*" $mdh "*) ;;
			*) other_mdhs="$other_mdhs $mdh" ;;
		    esac
		    ;;
		*)
		    mdh="\$(dir_top)/$loc/${dep}.mdh"
		    case "$remote_mdhs " in
			*" $mdh "*) ;;
			*) remote_mdhs="$remote_mdhs $mdh" ;;
		    esac
		    ;;
	    esac
	    modhdeps="$modhdeps $mdh"
	done

	echo "##### ===== DEPENDENCIES GENERATED FROM ${module}.mdd ===== #####"
	echo
	echo "MODOBJS_${module} = $objects"
	echo "MODDOBJS_${module} = $dobjects"
	echo "PROTO_${module} = $proto"
	echo "INCS_${module} = \$(PROTO_${module}) $otherincs"
	echo
	echo "proto.${module}: \$(PROTO_${module})"
	echo "\$(PROTO_${module}): \$(PROTODEPS)"
	echo
	echo "modobjs.${module}: \$(MODOBJS_${module})"
	echo "	echo '' \$(MODOBJS_${module}) $modobjs_sed>> \$(dir_src)/stamp-modobjs.tmp"
	echo
	if test -z "$alwayslink"; then
	    echo "${module}.\$(DL_EXT): \$(MODDOBJS_${module})"
	    echo '	rm -f $@'
	    echo "	\$(DLLINK) \$(MODDOBJS_${module}) \$(LIBS)"
	    echo
	fi
	echo "${module}.mdhi: ${module}.mdhs \$(INCS_${module})"
	echo "	@test -f \$@ || echo 'do not delete this file' > \$@"
	echo
	echo "${module}.mdhs: ${module}.mdd"
	echo "	@\$(MAKE) -f \$(makefile) \$(MAKEDEFS) ${module}.mdh.tmp"
	echo "	@if cmp -s ${module}.mdh ${module}.mdh.tmp; then \\"
	echo "	    rm -f ${module}.mdh.tmp; \\"
	echo "	    echo \"\\\`${module}.mdh' is up to date.\"; \\"
	echo "	else \\"
	echo "	    mv -f ${module}.mdh.tmp ${module}.mdh; \\"
	echo "	    echo \"Updated \\\`${module}.mdh'.\"; \\"
	echo "	fi"
	echo "	echo 'timestamp for ${module}.mdh against ${module}.mdd' > \$@"
	echo
	echo "${module}.mdh: ${modhdeps} ${headers} ${hdrdeps} ${module}.mdhi"
	echo "	@\$(MAKE) -f \$(makefile) \$(MAKEDEFS) ${module}.mdh.tmp"
	echo "	@mv -f ${module}.mdh.tmp ${module}.mdh"
	echo "	@echo \"Updated \\\`${module}.mdh'.\""
	echo
	echo "${module}.mdh.tmp:"
	echo "	@( \\"
	echo "	    echo '#ifndef have_${module}_module'; \\"
	echo "	    echo '#define have_${module}_module'; \\"
	echo "	    echo; \\"
	if test -n "$moddeps"; then
	    echo "	    echo '/* Module dependencies */'; \\"
	    echo "	    for mod in $modhdeps; do \\"
	    echo "		echo '# define USING_MODULE'; \\"
	    echo "		echo '# include \"'\$\$mod'\"'; \\"
	    echo "	    done; \\"
	    echo "	    echo '# undef USING_MODULE'; \\"
	    echo "	    echo; \\"
	fi
	if test -n "$headers"; then
	    echo "	    echo '/* Extra headers for this module */'; \\"
	    echo "	    for hdr in $headers; do \\"
	    echo "		if test -f \$\$hdr; then \\"
	    echo "		    echo '# include \"'\$\$hdr'\"'; \\"
	    echo "		else \\"
	    echo "		    echo '# include \"\$(sdir)/'\$\$hdr'\"'; \\"
	    echo "		fi; \\"
	    echo "	    done; \\"
	    echo "	    echo; \\"
	fi
	if test -n "$proto"; then
	    echo "	    echo '# define GLOBAL_PROTOTYPES'; \\"
	    echo "	    for pro in \$(PROTO_${module}); do \\"
	    echo "		echo '# include \"'\$\$pro'\"'; \\"
	    echo "	    done; \\"
	    echo "	    echo '# undef GLOBAL_PROTOTYPES'; \\"
	    echo "	    echo; \\"
	fi
	echo "	    echo '#endif /* !have_${module}_module */'; \\"
	echo "	) > \$@"
	echo
	echo "\$(MODOBJS_${module}) \$(MODDOBJS_${module}): ${module}.mdh"
	sed -e '/^ *: *<< *\\Make *$/,/^Make$/!d' \
	    -e 's/^ *: *<< *\\Make *$//; /^Make$/d' \
	    < $top_srcdir/$the_subdir/${module}.mdd
	echo

    done

    if test -n "$remote_mdhs$other_mdhs"; then
	echo "##### ===== DEPENDENCIES FOR REMOTE MODULES ===== #####"
	echo
	for mdh in $remote_mdhs; do
	    echo "$mdh: FORCE"
	    echo "	@cd @%@ && \$(MAKE) \$(MAKEDEFS) @%@$mdh"
	    echo
	done | sed 's,^\(.*\)@%@\(.*\)@%@\(.*\)/\([^/]*\)$,\1\3\2\4,'
	if test -n "$other_mdhs"; then
	    echo "${other_mdhs}:"
	    echo "	false # should only happen with make -n"
	    echo
	fi
    fi

    echo "##### End of ${the_makefile}.in"

    exec >&3 3>&-

fi

if $second_stage; then

    trap "rm -f $the_subdir/${the_makefile}" 1 2 15

    # The standard config.status requires the pathname for the .in file to
    # be relative to the top of the source tree.  As we have it in the build
    # tree, this is a problem.  zsh's configure script edits config.status,
    # adding the feature that an input filename starting with "!" has the
    # "!" removed and is not mangled further.
    CONFIG_FILES=$the_subdir/${the_makefile}:\!$the_subdir/${the_makefile}.in CONFIG_HEADERS= ./config.status

fi

exit 0
