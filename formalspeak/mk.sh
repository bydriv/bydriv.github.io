echo > common.l
echo '%{' >> common.l
echo '#include "y.tab.h"' >> common.l
echo '%}' >> common.l
echo '%%' >> common.l
awk -F'\t' '{print "\"" $2 "\" {return " ($1 == "," ? "COMMA" : $1 == "." ? "PERIOD" : $1 == ":" ? "COLON" : $1) ";}"}' common.tsv >> common.l
echo '%%' >> common.l

echo > formalspeak.y
awk -F'\t' '{print $1}' common.tsv | sort | uniq | perl -pe 's/,/%token COMMA/;s/\./%token PERIOD/;s/:/%token COLON/;s/PRP\$/%token PRPD/;s/WP\$/%token WPD/;s/^([^%].*)$/%token $1/' >> formalspeak.y
echo '%%' >> formalspeak.y
perl -pe 's/=/:/' formalspeak.ebnf >> formalspeak.y
echo '%%' >> formalspeak.y
