#
# makepro.awk - generate prototype lists
#

BEGIN {
    aborting = 0

    # arg 1 is the name of the file to process
    # arg 2 is the name of the subdirectory it is in
    if(ARGC != 3) {
	aborting = 1
	exit 1
    }
    name = ARGV[1]
    gsub(/^.*\//, "", name)
    gsub(/\.c$/, "", name)
    name = ARGV[2] "_" name
    gsub(/\//, "_", name)
    ARGC--

    # `locals' is a list of local declarations, built up while global
    # declarations are output.
    locals = ""

    printf "#ifndef have_%s_globals\n", name
    printf "#define have_%s_globals\n", name
    printf "\n"
}

# all relevant declarations are preceded by "/**/" on a line by itself

/^\/\*\*\/$/ {
    # The declaration is on following lines.  The interesting part might
    # be terminated by a `{' (`int foo(void) { }' or `int bar[] = {')
    # or `;' (`int x;').
    line = ""
    isfunc = 0
    while(1) {
	if(getline <= 0) {
	    aborting = 1
	    exit 1
	}
	gsub(/\t/, " ")
	line = line " " $0
	gsub(/\/\*([^*]|\*+[^*\/])*\*+\//, " ", line)
	if(line ~ /\/\*/)
	    continue
	# If it is a function definition, note so.
	if(line ~ /\) *[{].*$/) #}
	    isfunc = 1
	if(sub(/ *[{;].*$/, "", line)) #}
	    break
    }
    # Put spaces around each identifier.
    while(match(line, /[^_0-9A-Za-z ][_0-9A-Za-z]/) ||
	    match(line, /[_0-9A-Za-z][^_0-9A-Za-z ]/))
	line = substr(line, 1, RSTART) " " substr(line, RSTART+1)
    # Separate declarations into a type and a list of declarators.
    # In each declarator, "@{" and "@}" are used in place of parens to
    # mark function parameter lists, and "@!" is used in place of commas
    # in parameter lists.  "@<" and "@>" are used in place of
    # non-parameter list parens.
    gsub(/ _ +/, " _ ", line)
    while(1) {
	if(isfunc && match(line, /\([^()]*\)$/))
	    line = substr(line, 1, RSTART-1) " _ (" substr(line, RSTART) ")"
	else if(match(line, / _ \(\([^,()]*,/))
	    line = substr(line, 1, RSTART+RLENGTH-2) "@!" substr(line, RSTART+RLENGTH)
	else if(match(line, / _ \(\([^,()]*\)\)/))
	    line = substr(line, 1, RSTART-1) "@{" substr(line, RSTART+5, RLENGTH-7) "@}" substr(line, RSTART+RLENGTH)
	else if(match(line, /\([^,()]*\)/))
	    line = substr(line, 1, RSTART-1) "@<" substr(line, RSTART+1, RLENGTH-2) "@>" substr(line, RSTART+RLENGTH)
	else
	    break
    }
    sub(/^ */, "", line)
    match(line, /^((const|enum|static|struct|union) +)*([_0-9A-Za-z]+ +|((char|double|float|int|long|short|unsigned|void) +)+)((const|static) +)*/)
    dtype = substr(line, 1, RLENGTH)
    sub(/ *$/, "", dtype)
    islocal = " " dtype " " ~ / static /
    line = substr(line, RLENGTH+1) ","
    # Handle each declarator.
    output = ""
    while(match(line, /^[^,]*,/)) {
	# Separate out the name from the declarator.  Use "@+" and "@-"
	# to bracket the name within the declarator.  Strip off any
	# initialiser.
	dcltor = substr(line, 1, RLENGTH-1)
	line = substr(line, RLENGTH+1)
	sub(/\=.*$/, "", dcltor)
	match(dcltor, /^([^_0-9A-Za-z]| const )*/)
	dcltor = substr(dcltor, 1, RLENGTH) "@+" substr(dcltor, RLENGTH+1)
	match(dcltor, /^.*@\+[_0-9A-Za-z]+/)
	dcltor = substr(dcltor, 1, RLENGTH) "@-" substr(dcltor, RLENGTH+1)
	dnam = dcltor
	sub(/^.*@\+/, "", dnam)
	sub(/@-.*$/, "", dnam)

	# Put parens etc. back
	gsub(/@[{]/, " _((", dcltor)
	gsub(/@}/, "))", dcltor)
	gsub(/@</, "(", dcltor)
	gsub(/@>/, ")", dcltor)
	gsub(/@!/, ",", dcltor)

	# If this is a module boot/cleanup function, conditionally rename it.
	if(" " dtype " " ~ / int / && dcltor ~ / *@\+(boot|cleanup)_[_0-9A-Za-z]+@- *_\(\( *Module +[_0-9A-Za-z]+ *\)\) */) {
	    modtype = dnam
	    sub(/_.*$/, "", modtype)
	    output = output "# if defined(DYNAMIC_NAME_CLASH_OK) && defined(MODULE)\n"
	    output = output "#  define " dnam " " modtype "_\n"
	    output = output "# endif\n"
	}

	# Format the declaration for output
	dcl = dtype " " dcltor ";"
	if(!islocal)
	    dcl = "extern " dcl
	gsub(/@[+-]/, "", dcl)
	gsub(/ +/, " ", dcl)
	while(match(dcl, /[^_0-9A-Za-z] ./) || match(dcl, /. [^_0-9A-Za-z]/))
	    dcl = substr(dcl, 1, RSTART) substr(dcl, RSTART+2)
	output = output dcl "\n"
    }

    # Output global declarations now, but save up locals until the end.
    if(islocal)
	locals = locals output
    else
	printf "%s", output
}

END {
    if(aborting)
	exit 1
    printf "\n"
    printf "#endif /* !have_%s_globals */\n", name
    if(locals != "") {
	printf "\n"
	printf "#ifndef GLOBAL_PROTOTYPES\n"
	printf "\n"
	printf locals
	printf "\n"
	printf "#endif /* !GLOBAL_PROTOTYPES */\n"
    }
}
