#compdef todo.sh

# See http://todotxt.com for todo.sh.
#
# Featurettes:
#  - "replace" will complete the original text for editing.
#  - completing priorities will cycle through A to Z (even without
#    menu completion).

setopt localoptions braceccl

local expl curcontext="$curcontext" state line pri nextstate
local -a cmdlist itemlist
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

local txtmsg="text, can include p:<project> and @<where>"

case $state in
  (commands)
  cmdlist=(
    "add:Add TODO ITEM to todo.txt."
    "append:Adds to item on line NUMBER the text TEXT."
    "archive:Moves done items from todo.txt to done.txt."
    "del:Deletes the item on line NUMBER in todo.txt."
    "do:Marks item on line NUMBER as done in todo.txt."
    "list:Displays all todo items containing TERM(s), sorted by priority."
    "listall:Displays items including done ones containing TERM(s)"
    "listpri:Displays all items prioritized at PRIORITY."
    "prepend:Adds to the beginning of the item on line NUMBER text TEXT."
    "pri:Adds or replace in NUMBER the priority PRIORITY (upper case letter)."
    "replace:Replace in NUMBER the TEXT."
    "remdup:Remove exact duplicates from todo.txt."
    "report:Adds the number of open and done items to report.txt."
  )
  _describe -t todo-commands 'todo.sh command' cmdlist
  ;;

  (arguments)
  case $words[NORMARG] in
    (append|del|do|prepend|pri|replace)
    if (( NORMARG == CURRENT - 1 )); then
      itemlist=(${${(M)${(f)"$(todo.sh list)"}##<-> *}/(#b)(<->) (*)/${match[1]}:${match[2]}})
      _describe -t todo-items 'todo item' itemlist
    else
      case $words[NORMARG] in
	(pri)
	nextstate=pri
	;;
	(append|prepend)
	_message $txtmsg
	;;
	(replace)
	compadd -Q -- "${(qq)$(todo.sh list "^0*${words[CURRENT-1]} ")##<-> }"
	;;
      esac
    fi
    ;;

    (add)
    _message $txtmsg
    ;;

    (list|listall)
    # This completes stuff beginning with p: (projects) or @ (contexts);
    # these are todo.sh conventions.
    _wanted search expl 'context or project' \
      compadd ${${=${${(M)${(f)"$(todo.sh list)"}##<-> *}##<-> }}:#^(p:*|@*)}
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
esac
