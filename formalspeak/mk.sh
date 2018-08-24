echo > common.l
echo '%{' >> common.l
echo '#include "y.tab.h"' >> common.l
echo '%}' >> common.l
echo '%%' >> common.l
awk -F, '{print "\"" $2 "\" {return " ($1 == ":" ? "COLON" : $1) ";}"}' common.csv >> common.l
echo '%%' >> common.l

echo > formalspeak.y
awk -F, '{print $1}' common.csv | sort | uniq | perl -pe 's/:/%token COLON/;s/PRP\$/%token PRPD/;s/WP\$/%token WPD/;s/^([^%].*)$/%token $1/' >> formalspeak.y
echo '%%' >> formalspeak.y
perl -pe 's/=/:/' formalspeak.ebnf >> formalspeak.y
echo '%%' >> formalspeak.y
