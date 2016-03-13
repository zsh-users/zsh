updprompt()
{
  psvar[2]=""

  unset _trapchld_called
  local njobs jobstr
  njobs=$#jobstates
  [[ $njobs -gt 1 ]] && jobstr="s"
  [[ $njobs -ge 1 ]] && jobstr=" $njobs job$jobstr |"

  echo 1 > /dev/tty

  [[ -n $TTY && $TERM == (xterm*|dtterm|mlterm|rxvt*|screen*) ]] &&
    {
      [[ $TERM == screen* ]] || print -nP "\e]1;%m${ptsn:+[$ptsn]}:%.\x07"
      print -nP "\e]2;${jobstr}${WINTITLE:+ $WINTITLE |} %n@%m - %~ | %y\x07"
    } > /dev/tty

  echo 2 > /dev/tty
}

TRAPCHLD()
{
  echo SIGCHLD 1 > /dev/tty
  if [[ -o interactive && -n $TTY ]] then
    updprompt
    typeset -g _trapchld_called=1
  fi
  echo SIGCHLD 2 > /dev/tty
}
