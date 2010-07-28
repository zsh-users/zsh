emulate -L zsh

autoload -Uz read-from-minibuffer

zle -K zed-normal-keymap

local REPLY
read-from-minibuffer "File name: "
zed_file_name=$REPLY
