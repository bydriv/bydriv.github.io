perl -0 -pe 's/\s|\[(.*?)\]|//mg' "$@" | wc -m
