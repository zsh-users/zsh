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

_arguments -s \
  '-d[alternate config file]:config file:_files' \
  '-f[force, no confirmation]' \
  '-h[display help]' \
  '-p[plain mode, no colours]' \
  '-v[verbose mode, confirmation messages]' \
  '-V[display version etc.]' \
  '1:command:->commands' \
  '2:first argument:->firstarg' \
  '3:second argument:->secondarg' && return 0

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

  (firstarg)
  case $words[CURRENT-1] in
    (append|del|do|prepend|pri|replace)
    itemlist=(${${(M)${(f)"$(todo.sh list)"}##<-> *}/(#b)(<->) (*)/${match[1]}:${match[2]}})
    _describe -t todo-items 'todo item' itemlist
    ;;

    (add)
    _message $txtmsg
    ;;

    (list|listall)
    # This completes stuff beginning with p: (projects) or @ (contexts);
    # these are todo.sh conventions.
    # We should do it for any argument after list or listall, but
    # _arguments doesn't make that easy.  We need it to tell us
    # the position of the first non-option argument.
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

  (secondarg)
  case $words[CURRENT-2] in
    (append|prepend)
    _message $txtmsg
    ;;
    (pri)
    nextstate=pri
    ;;
    (replace)
    compadd -Q -- "${(qq)$(todo.sh list "^0*${words[CURRENT-1]} ")##<-> }"
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
