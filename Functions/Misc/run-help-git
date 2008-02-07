if [ $# -eq 0 ]; then
    man git
else
    local al
    if al=$(git config --get "alias.$1"); then
        1=${al%% *}
    fi
    man git-$1
fi
