#!/bin/zsh -f

emulate zsh

# Run all specified tests, keeping count of which succeeded.
# The reason for this extra layer above the test script is to
# protect from catastrophic failure of an individual test.
# We could probably do that with subshells instead.

integer success=0 failure=0 skipped=0 retval
typeset resln rule col
typeset -a ffiles res

# colour to use for ztst failure messages. also used as a signal that colours
# are available generally
[[ -t 1 ]] &&
(( ! $+ZTST_failcolour )) &&
[[ -z $NO_COLOR ]] &&
[[ $TERM == *color* || "$( tput colors 2> /dev/null )" == <8-> ]] &&
typeset -x ZTST_failcolour=red

for file in "${(f)ZTST_testlist}"; do
  file=${file#./}
  $ZTST_exe +Z -f $ZTST_srcdir/ztst.zsh $file
  retval=$?
  if (( $retval == 2 )); then
    (( skipped++ ))
  elif (( $retval )); then
    (( failure++ ))
    (( $retval > 128 )) && print -r - "$file: failed: SIG$signals[$retval - 127]."
    ffiles+=( $file )
  else
    (( success++ ))
  fi
  print
done

res=(
  "$success successful test script${${success:#1}:+s}"
  "$failure failure${${failure:#1}:+s}"
  "$skipped skipped"
)
resln=${(j<, >)res}
rule=${resln//?/\*}

[[ -n $ZTST_failcolour ]] &&
if (( failure )); then
  col=red
elif (( success )); then
  col=green
else
  col=yellow
fi

(( $#col )) && print -rnP - "%F{$col}"
print -rl - $rule $resln
(( $#ffiles )) &&
print -rl - '' "failing test script${${#ffiles:#1}:+s}:" ${(@)ffiles/#/  }
print -rl - $rule
(( $#col )) && print -rnP - %f

return $(( failure ? 1 : 0 ))
