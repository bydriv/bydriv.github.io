pandoc -f markdown+east_asian_line_breaks --template template.html -H "$1/header.html" "$1/index.md" > "$1/index.html"
