%{
#include "y.tab.h"

int
yywrap(void)
{
  return 1;
}
%}
%%
[\t\n\v\f\r]+ {
  return yylex();
}
"," {
  return COMMA;
}
"." {
  return DOT;
}
"I" {
  return I;
}
"am" {
  return AM;
}
[A-Za-z]+ {
  yylval.yytext = malloc(strlen(yytext));
  strcpy(yylval.yytext, yytext);
  return PN;
}
%%
