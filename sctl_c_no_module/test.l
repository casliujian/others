%array
%option noyywrap
%option yylineno
%option outfile="Test.c"

%{
int lineno = 0;
#define YYLMAX 1024
%}

%%
