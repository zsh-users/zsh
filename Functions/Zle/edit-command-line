# Edit the command line using your usual editor.
# Binding this to '!' in the vi command mode map,
#   autoload -Uz edit-command-line
#   zle -N edit-command-line
#   bindkey -M vicmd '!' edit-command-line
# will give ksh-like behaviour for that key,
# except that it will handle multi-line buffers properly.

emulate -L zsh

() {
  exec </dev/tty

  # Compute the cursor's position in bytes, not characters.
  setopt localoptions nomultibyte noksharrays

  (( $+zle_bracketed_paste )) && print -r -n - $zle_bracketed_paste[2]

  # Open the editor, placing the cursor at the right place if we know how.
  local editor=( "${(@Q)${(z)${VISUAL:-${EDITOR:-vi}}}}" )
  case $editor in 
    (*vim*)
      integer byteoffset=$(( $#PREBUFFER + $#LBUFFER + 1 ))
      "${(@)editor}" -c "normal! ${byteoffset}go" -- $1;;
    (*emacs*)
      local lines=( "${(@f):-"$PREBUFFER$LBUFFER"}" )
      "${(@)editor}" +${#lines}:$((${#lines[-1]} + 1)) $1;;
    (*) "${(@)editor}" $1;;
  esac

  (( $+zle_bracketed_paste )) && print -r -n - $zle_bracketed_paste[1]

  # Replace the buffer with the editor output.
  print -Rz - "$(<$1)" 
} =(<<<"$PREBUFFER$BUFFER")

zle send-break		# Force reload from the buffer stack
