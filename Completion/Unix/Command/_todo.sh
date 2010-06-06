#compdef todo.sh

# See http://todotxt.com for todo.sh.
#
# Featurettes:
#  - "replace" will complete the original text for editing
#  - completing priorities will cycle through A to Z (even without
#    menu completion)
#  - list and listall will complete p:<project> and @<where> from
#    values in existing entries
#  - will complete after p: and @ if typed in message text

setopt localoptions braceccl

local expl curcontext="$curcontext" state line pri nextstate
local -a cmdlist itemlist match mbegin mend
integer NORMARG

_arguments -s -n : \
  '-d[alternate config file]:config file:_files' \
  '-f[force, no confirmation]' \
  '-h[display help]' \
  '-p[plain mode, no colours]' \
  '-v[verbose mode, confirmation messages]' \
  '-V[display version etc.]' \
  '1:command:->commands' \
  '*:arguments:->arguments' && return 0

local projmsg="context or project"
local txtmsg="text with contexts or projects"

case $state in
  (commands)
  cmdlist=(
    "add:add TODO ITEM to todo.txt."
    "append:adds to item on line NUMBER the text TEXT."
    "archive:moves done items from todo.txt to done.txt."
    "del:deletes the item on line NUMBER in todo.txt."
    "do:marks item on line NUMBER as done in todo.txt."
    "list:displays all todo items containing TERM(s), sorted by priority."
    "listall:displays items including done ones containing TERM(s)"
    "listpri:displays all items prioritized at PRIORITY."
    "prepend:adds to the beginning of the item on line NUMBER text TEXT."
    "pri:adds or replace in NUMBER the priority PRIORITY (upper case letter)."
    "replace:replace in NUMBER the TEXT."
    "remdup:remove exact duplicates from todo.txt."
    "report:adds the number of open and done items to report.txt."
  )
  _describe -t todo-commands 'todo.sh command' cmdlist
  ;;

  (arguments)
  case $words[NORMARG] in
    (append|del|do|prepend|pri|replace)
    if (( NORMARG == CURRENT - 1 )); then
      itemlist=(${${(M)${(f)"$(todo.sh -p list)"}##<-> *}/(#b)(<->) (*)/${match[1]}:${match[2]}})
      _describe -t todo-items 'todo item' itemlist
    else
      case $words[NORMARG] in
	(pri)
	nextstate=pri
	;;
	(append|prepend)
	nextstate=proj
	;;
	(replace)
	compadd -Q -- "${(qq)$(todo.sh -p list "^0*${words[CURRENT-1]} ")##<-> }"
	;;
      esac
    fi
    ;;

    (add|list|listall)
    nextstate=proj
    ;;

    (listpri)
    nextstate=pri
    ;;

    (*)
    return 1
    ;;
  esac
  ;;
esac

case $nextstate in
  (pri)
  if [[ $words[CURRENT] = (|[A-Z]) ]]; then
    if [[ $words[CURRENT] = (|Z) ]]; then
      pri=A
    else
      # cycle priority
      pri=$words[CURRENT]
      pri=${(#)$(( #pri + 1 ))}
    fi
    _wanted priority expl 'priority' compadd -U -S '' -- $pri
  else
    _wanted priority expl 'priority' compadd {A-Z}
  fi
  ;;

  (proj)
  # This completes stuff beginning with p: (projects) or @ (contexts);
  # these are todo.sh conventions.
  if [[ ! -prefix p: && ! -prefix @ ]]; then
    projmsg=$txtmsg
  fi
  # In case there are quotes, ignore anything up to whitespace before
  # the p: or @ (which may not even be there yet).
  compset -P '*[[:space:]]'
  _wanted search expl $projmsg \
    compadd ${${=${${(M)${(f)"$(todo.sh -p list)"}##<-> *}##<-> }}:#^(p:*|@*)}
  ;;
esac
