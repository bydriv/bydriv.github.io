pandoc -f markdown+ignore_line_breaks -s -c style.css -H header.html -B before.html -A after.html index.md > index.html
