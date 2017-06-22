%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "parser_helper.h"
    #include "sctlparser.h"
    //%array
%}
%array
%option noyywrap
%option yylineno
%option outfile="sctllexer.c"

%x COMMENT MCOMMENT 

integer     [:digit:]+
id          [:alpha:][[:alpha:][:digit:]_-]*
%%

%{
    // struct modul_defs* moduls = yyextra;
    // struct modul0* main_modul = yyextra;
%}

"Module"  {return Module;}
"Model"   {return Model;}
"Var"     {return Var;}
"Define"  {return Define;}
"Init"    {return Init;}
"Transition"  {return Trans;}
"Atomic"  {return Atomic;}
"Spec"    {return Spec;}
"Int"     {return Int;}
"Bool"    {return Bool;}
"true"    {yylval.integer = 1; return B;}
"false"   {yylval.integer = 0; return B;}
"TRUE"    {return Top;}
"FALSE"   {return Bottom;}
"not"     {return Neg;}
"mod"     {return Mod;}
"AX"      {return AX;}
"EX"      {return EX;}
"AF"      {return AF;}
"EG"      {return EG;}
"AR"      {return AR;}
"EU"      {return EU;}
[a-zA-Z_][a-zA-Z0-9_]*    {yylval.str_ptr = strdup(yytext); return Id;}
[0-9]+   {yylval.integer = atoi(yytext); return I;}
"#"       {return Scalar;}
":"       {return Colon;}
";"       {return Semicolon;}
","       {return Comma;}
"."       {return Dot;}
".."      {return DotDot;}
"("      {return LB1;}
")"       {return RB1;}
"["      {return LB2;}
"]"      {return RB2;}
"{"      {return LB3;}
"}"      {return RB3;}
"/\\"       {return And;}
"\\/"       {return Or;}
"!"         {return Nego;}
"||"        {return Oro;}
"&&"        {return Ando;}
"+"         {return Add;}
"-"         {return Minus;}
"*"         {return Mult;}
"<"         {return LT;}
">"         {return GT;}
"<="        {return LE;}
">="        {return GE;}
":="        {return Assigno;}
"="         {return Equal;}
"!="        {return Non_equal;}
"\n"        {yylloc.first_line++;}
[ \t\r]+  
"//"        {BEGIN COMMENT;}    
"/*"        {BEGIN MCOMMENT;}
<<EOF>>     {return File_end;}
.           {printf("undefined token: %s in line %d.\n", 
                        yytext, 
                        yylloc.first_line);}

<COMMENT>{
    "\n"    {BEGIN INITIAL;}
    .       {}
}    

<MCOMMENT>{
    "*/"    {BEGIN INITIAL;}
    .       {}
}
