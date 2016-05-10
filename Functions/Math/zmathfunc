#autoload

zsh_math_func_min() {
  local result=$1
  shift
  local arg
  for arg ; do
    (( $arg < result )) && result=$arg
  done
  (( result )) # return
}
functions -M min 1 -1 zsh_math_func_min # at least one argument

zsh_math_func_max() {
  local result=$1
  shift
  local arg
  for arg ; do
    (( $arg > result )) && result=$arg
  done
  (( result )) # return
}
functions -M max 1 -1 zsh_math_func_max # at least one argument

zsh_math_func_sum() {
  local sum
  local arg
  for arg ; do
    (( sum += $arg ))
  done
  (( sum ))
}
functions -M sum 0 -1 zsh_math_func_sum

