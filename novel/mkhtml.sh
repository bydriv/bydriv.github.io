#!/bin/sh

printf "<!DOCTYPE html>\n"
printf "\n"
printf "<meta charset=\"utf-8\">\n"
printf "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
printf "<title>%s</title>\n" "$1"
printf "\n"

printf "<style>"
cat ./template.css
perl ./mkstyle.pl < "$2"
printf "</style>"

printf "<div id=\"container\">\n"

perl ./mkanchors.pl < "$2"

printf "  <div id=\"hd\"></div>\n"
printf "  <h1>%s</h1>\n" "$1"

perl ./mkindex.pl < "$2"
perl ./mkcontents.pl < "$2"

printf "</div>\n"
