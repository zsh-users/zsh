##
## zsh prompt themes extension
## by Adam Spiers <adam@spiers.net>
##
## Load with `autoload -Uz promptinit; promptinit'.
## Type `prompt -h' for help.
##

typeset -gaU prompt_themes
typeset -ga prompt_theme
typeset -g prompt_newline
prompt_themes=()

promptinit () {
  emulate -L zsh
  setopt extendedglob
  local ppath='' name theme
  local -a match mbegin mend

  # Autoload all prompt_*_setup functions in fpath
  for theme in $^fpath/prompt_*_setup(N); do
    if [[ $theme == */prompt_(#b)(*)_setup ]]; then
      name="$match[1]"
      if [[ -r "$theme" ]]; then
        prompt_themes=($prompt_themes $name)
        autoload -Uz prompt_${name}_setup
      else
        print "Couldn't read file $theme containing theme $name."
      fi
    else
      print "Eh?  Mismatch between glob patterns in promptinit."
    fi
  done

  # To manipulate precmd and preexec hooks...
  autoload -Uz add-zsh-hook

  # Variables common to all prompt styles
  prompt_newline=$'\n%{\r%}'
}

prompt_preview_safely() {
  emulate -L zsh
  print -P "%b%f%k"
  if [[ -z "$prompt_themes[(r)$1]" ]]; then
    print "Unknown theme: $1"
    return
  fi

  # This handles all the stuff from the default :prompt-theme cleanup
  local +h PS1=$PS1 PS2=$PS2 PS3=$PS3 PS4=$PS4 RPS1=$RPS1 RPS2=$RPS2
  local +h PROMPT=$PROMPT RPROMPT=$RPOMPT RPROMPT2=$RPROMPT2 PSVAR=$PSVAR
  local -a precmd_functions preexec_functions prompt_preview_cleanup
  local -aLl +h zle_highlight

  {
    # Save and clear current restore-point if any
    zstyle -g prompt_preview_cleanup :prompt-theme cleanup
    {
      zstyle -d :prompt-theme cleanup

      # The next line is a bit ugly.  It (perhaps unnecessarily)
      # runs the prompt theme setup function to ensure that if
      # the theme has a _preview function that it's been autoloaded.
      prompt_${1}_setup

      if typeset +f prompt_${1}_preview >&/dev/null; then
        prompt_${1}_preview "$@[2,-1]"
      else
        prompt_preview_theme "$@"
      fi
    } always {
      # Run any theme-specific cleanup, then reset restore point
      zstyle -t :prompt-theme cleanup
    }
  } always {
    (( $#prompt_preview_cleanup )) &&
      zstyle -e :prompt-theme cleanup "${prompt_preview_cleanup[@]}"
  }
}

set_prompt() {
  emulate -L zsh
  local opt preview theme usage old_theme

  usage='Usage: prompt <options>
Options:
    -c              Show currently selected theme and parameters
    -l              List currently available prompt themes
    -p [<themes>]   Preview given themes (defaults to all)
    -h [<theme>]    Display help (for given theme)
    -s <theme>      Set and save theme
    <theme>         Switch to new theme immediately (changes not saved)

Use prompt -h <theme> for help on specific themes.'

  getopts "chlps:" opt
  case "$opt" in
    (h|p)
      setopt localtraps
      if [[ -z "$prompt_theme[1]" ]]; then
        # Not using a prompt theme; save settings
        local +h PS1=$PS1 PS2=$PS2 PS3=$PS3 PS4=$PS4 RPS1=$RPS1 RPS2=$RPS2
        local +h PROMPT=$PROMPT RPROMPT=$RPOMPT RPROMPT2=$RPROMPT2 PSVAR=$PSVAR
        local -a precmd_functions preexec_functions
      else
        trap 'prompt_${prompt_theme[1]}_setup "${(@)prompt_theme[2,-1]}"' 0
      fi
      ;;
  esac
  case "$opt" in
    c) if [[ -n $prompt_theme ]]; then
         print -n "Current prompt theme"
         (( $#prompt_theme > 1 )) && print -n " with parameters"
         print " is:\n  $prompt_theme"
       else
         print "Current prompt is not a theme."
       fi
       return
       ;;
    h) if [[ -n "$2" && -n $prompt_themes[(r)$2] ]]; then
         if functions prompt_$2_setup >/dev/null; then
           # The next line is a bit ugly.  It (perhaps unnecessarily)
           # runs the prompt theme setup function to ensure that if
           # the theme has a _help function that it's been autoloaded.
           prompt_$2_setup
         fi
         if functions prompt_$2_help >/dev/null; then
           print "Help for $2 theme:\n"
           prompt_$2_help
         else
           print "No help available for $2 theme."
         fi
         print "\nType \`prompt -p $2' to preview the theme, \`prompt $2'"
         print "to try it out, and \`prompt -s $2' to use it in future sessions."
       else
         print "$usage"
       fi
       ;;
    l) print Currently available prompt themes:
       print $prompt_themes
       return
       ;;
    p) preview=( $prompt_themes )
       (( $#* > 1 )) && preview=( "$@[2,-1]" )
       for theme in $preview; do
         [[ "$theme" == "$prompt_theme[*]" ]] && continue
         prompt_preview_safely "$=theme"
       done
       print -P "%b%f%k"
       ;;
    s) print "Set and save not yet implemented.  Please ensure your ~/.zshrc"
       print "contains something similar to the following:\n"
       print "  autoload -Uz promptinit"
       print "  promptinit"
       print "  prompt $*[2,-1]"
       shift
       ;&
    *) if [[ "$1" == 'random' ]]; then
         local random_themes
         if (( $#* == 1 )); then
           random_themes=( $prompt_themes )
         else
           random_themes=( "$@[2,-1]" )
         fi
         local i=$(( ( $RANDOM % $#random_themes ) + 1 ))
         argv=( "${=random_themes[$i]}" )
       fi
       if [[ -z "$1" || -z $prompt_themes[(r)$1] ]]; then
         print "$usage"
         return
       fi

       # Reset some commonly altered bits to the default
       local hook
       for hook in chpwd precmd preexec periodic zshaddhistory zshexit; do
         add-zsh-hook -D "${hook}" "prompt_*_${hook}"
       done
       typeset -ga zle_highlight=( ${zle_highlight:#default:*} )
       (( ${#zle_highlight} )) || unset zle_highlight

       prompt_$1_setup "$@[2,-1]" && prompt_theme=( "$@" )
       ;;
  esac
}

prompt_cleanup () {
  local -a cleanup_hooks
  if zstyle -g cleanup_hooks :prompt-theme cleanup
  then
    cleanup_hooks+=(';' "$@")
    zstyle -e :prompt-theme cleanup "${cleanup_hooks[@]}"
  elif (( $+prompt_preview_cleanup == 0 ))
  then
    print -u2 "prompt_cleanup: no prompt theme active"
    return 1
  fi
}

prompt () {
  local -a prompt_opts theme_active

  zstyle -g theme_active :prompt-theme cleanup || {
    # This is done here rather than in set_prompt so that it
    # is safe and sane for set_prompt to setopt localoptions,
    # which will be cleared before we arrive back here again.
    # This is also why we pass around the prompt_opts array.
    [[ -o promptbang ]] && prompt_opts+=(bang)
    [[ -o promptcr ]] && prompt_opts+=(cr)
    [[ -o promptpercent ]] && prompt_opts+=(percent)
    [[ -o promptsp ]] && prompt_opts+=(sp)
    [[ -o promptsubst ]] && prompt_opts+=(subst)
    zstyle -e :prompt-theme cleanup \
        'zstyle -d :prompt-theme cleanup;' \
	'prompt_default_setup;' \
        ${PS1+PS1="${(q)PS1}"} \
        ${PS2+PS2="${(q)PS2}"} \
        ${PS3+PS3="${(q)PS3}"} \
        ${PS4+PS4="${(q)PS4}"} \
        ${RPS1+RPS1="${(q)RPS1}"} \
        ${RPS2+RPS2="${(q)RPS2}"} \
        ${RPROMPT+RPROMPT="${(q)RPROMPT}"} \
        ${RPROMPT2+RPROMPT2="${(q)RPROMPT2}"} \
        ${PSVAR+PSVAR="${(q)PSVAR}"} \
        "precmd_functions=(${(q)precmd_functions[@]})" \
        "preexec_functions=(${(q)preexec_functions[@]})" \
        "prompt_opts=( ${prompt_opts[*]} )" \
        'reply=(yes)'
  }
  set_prompt "$@"

  (( ${#prompt_opts} )) &&
      setopt noprompt{bang,cr,percent,sp,subst} "prompt${^prompt_opts[@]}"

  true
}

prompt_preview_theme () {
  emulate -L zsh

  # Check for proper state handling
  (( $+prompt_preview_cleanup )) || {
    prompt_preview_safely "$@"
    return
  }

  # Minimal preview for prompts that don't supply one
  local -a prompt_opts
  print -n "$1 theme"
  (( $#* > 1 )) && print -n " with parameters \`$*[2,-1]'"
  print ":"
  prompt_${1}_setup "$@[2,-1]"
  (( ${#prompt_opts} )) &&
      setopt noprompt{bang,cr,percent,sp,subst} "prompt${^prompt_opts[@]}"
  [[ -n ${precmd_functions[(r)prompt_${1}_precmd]} ]] &&
    prompt_${1}_precmd
  [[ -o promptcr ]] && print -n $'\r'; :
  print -P "${PS1}command arg1 arg2 ... argn"
  [[ -n ${preexec_functions[(r)prompt_${1}_preexec]} ]] &&
    prompt_${1}_preexec
}

[[ -o kshautoload ]] || promptinit "$@"
