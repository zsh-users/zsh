# Usage: zpgrep <perl5-compatible regex> <file1> <file2> ... <fileN>
#

zpgrep() {
local file pattern

pattern=$1
shift

if ((! ARGC)) then
	set -- -
fi

pcre_compile $pattern
pcre_study

for file
do
	if [[ "$file" == - ]] then
		while read -u0 buf; do pcre_match $buf && print $buf; done
	else
		while read -u0 buf; do pcre_match $buf && print $buf; done < "$file"
	fi
done
}
