/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_SCTLPARSER_H_INCLUDED
# define YY_YY_SCTLPARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    Module = 258,
    Model = 259,
    Var = 260,
    Define = 261,
    Init = 262,
    Trans = 263,
    Atomic = 264,
    Spec = 265,
    Int = 266,
    Bool = 267,
    Top = 268,
    Bottom = 269,
    AX = 270,
    EX = 271,
    AF = 272,
    EG = 273,
    AR = 274,
    EU = 275,
    Neg = 276,
    Colon = 277,
    Semicolon = 278,
    Comma = 279,
    Dot = 280,
    DotDot = 281,
    LB1 = 282,
    RB1 = 283,
    LB2 = 284,
    RB2 = 285,
    LB3 = 286,
    RB3 = 287,
    Assigno = 288,
    Scalar = 289,
    Nego = 290,
    Ando = 291,
    Oro = 292,
    Non_equal = 293,
    Mod = 294,
    And = 295,
    Or = 296,
    Equal = 297,
    Add = 298,
    Minus = 299,
    Mult = 300,
    LT = 301,
    GT = 302,
    LE = 303,
    GE = 304,
    File_end = 305,
    Id = 306,
    I = 307,
    B = 308
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 24 "sctlparser.y" /* yacc.c:1909  */

    struct state* state_ptr;
    struct state_list* state_list_ptr;
    struct formula* fml_ptr;
    struct expr* expr_ptr;
    struct type* type_ptr;
    struct string_list* string_list_ptr;
    struct string_vars_expr_list* string_vars_expr_list_ptr;
    struct expr_pair_list* expr_pair_list_ptr;
    struct expr_list* expr_list_ptr;
    struct string_type_list* string_type_list_ptr;
    struct string_expr_list* string_expr_list_ptr;
    // struct expr_modul_instance_list* emi_list_ptr;
    struct string_emi_list* string_emi_list_ptr;
    struct transition* transition_ptr;
    struct transition_list* transition_list_ptr;
    struct string_fml_list* string_fml_list_ptr;
    struct modul0_list* module_list_ptr;
    // struct modul0* modul_ptr;
    char* str_ptr;
    int integer;

#line 131 "sctlparser.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE yylval;
extern YYLTYPE yylloc;
int yyparse (struct modul_defs* moduls);

#endif /* !YY_YY_SCTLPARSER_H_INCLUDED  */
