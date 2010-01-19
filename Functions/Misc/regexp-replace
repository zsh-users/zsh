# Replace all occurrences of a regular expression in a variable.  The
# variable is modified directly.  Respects the setting of the
# option RE_MATCH_PCRE.
#
# First argument: *name* (not contents) of variable.
# Second argument: regular expression
# Third argument: replacement string.  This can contain all forms of
# $ and backtick substitutions; in particular, $MATCH will be replaced
# by the portion of the string matched by the regular expression.

integer pcre

[[ -o re_match_pcre ]] && pcre=1

emulate -L zsh
(( pcre )) && setopt re_match_pcre

# $4 is the string to be matched
4=${(P)1}
# $5 is the final string
5=
# 6 indicates if we made a change
6=
local MATCH MBEGIN MEND
local -a match mbegin mend

while [[ -n $4 ]]; do
  if [[ $4 =~ $2 ]]; then
    # append initial part and subsituted match
    5+=${4[1,MBEGIN-1]}${(e)3}
    # truncate remaining string
    4=${4[MEND+1,-1]}
    # indicate we did something
    6=1
  else
    break
  fi
done
5+=$4

eval ${1}=${(q)5}
# status 0 if we did something, else 1.
[[ -n $6 ]]
