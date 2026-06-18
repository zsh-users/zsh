#compdef update-rc.d

local curcontext="$curcontext" state line expl

_arguments -C \
  '-f[force removal of symlinks]' \
  '1:service:_services' \
  '2:command:(remove defaults defaults-disabled disable enable)' \
  '*::args:->args' && return

case $words[2] in
  disable|enable)
    _wanted runlevels expl runlevel S 2 3 4 5 && return
  ;;
  *)
    _message 'no more arguments'
  ;;
esac

return 1
