perl -0 -pe 's/\s|\[(.*?)\]|//mg' "$1" | wc -m
