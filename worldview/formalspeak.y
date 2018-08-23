%{
#include <stdio.h>
#include <string.h>

const char *
translate(const char *origin)
{
  if (strcmp(origin, "Teiri") == 0)
    return "テーリ";
  else
    return origin;
}
%}
%union {
  char *yytext;
}
%token COMMA DOT
%token I AM
%token <yytext> PN /* Proper Noun */
%%
stmt: I AM PN DOT { printf("ぼくは%s。", translate($3)); }
%%
int
yyerror()
{
  return 0;
}

int
main(void)
{
  extern FILE *yyin;
  yyin = stdin;

  if (yyparse())
    return 1;
}
