## vim:ft=zsh
## Written by Frank Terbeck <ft@bewatermyfriend.org>
## Distributed under the same BSD-ish license as zsh itself.

setopt localoptions NO_shwordsplit
local dirname=$1
local basedir="." file

basedir=${basedir:P}
while [[ ${basedir} != '/' ]]; do
    [[ -r ${basedir} ]] || return 1
    if [[ -n ${vcs_comm[detect_need_file]} ]] ; then
        [[ -d ${basedir}/${dirname} ]] && {
            for file in ${(s: :)${vcs_comm[detect_need_file]}}; do
                [[ -e ${basedir}/${dirname}/${file} ]] && break 2
            done
        }
    else
        [[ -d ${basedir}/${dirname} ]] && break
    fi

    basedir=${basedir:h}
done

[[ ${basedir} == "/" ]] && return 1
vcs_comm[basedir]=${basedir}
return 0
