/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 2 "sctlparser.y" /* yacc.c:339  */

    // #define _GNU_SOURCE
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "term.h"
    #include "formula.h"
    #include "modul.h"
    // #include "sctllexer.h"
    #include "parser_helper.h"

#line 78 "sctlparser.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "sctlparser.h".  */
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
#line 24 "sctlparser.y" /* yacc.c:355  */

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

#line 195 "sctlparser.c" /* yacc.c:355  */
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

/* Copy the second part of user declarations.  */
#line 47 "sctlparser.y" /* yacc.c:358  */

    extern int yylex();
    void yyerror (struct modul_defs* moduls, const char* err_msg);

#line 230 "sctlparser.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   567

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  54
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  27
/* YYNRULES -- Number of rules.  */
#define YYNRULES  106
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  307

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   308

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    95,    95,   107,   120,   121,   128,   137,   138,   146,
     156,   159,   167,   177,   179,   180,   190,   193,   201,   209,
     217,   225,   233,   243,   245,   246,   258,   260,   261,   272,
     274,   275,   286,   287,   288,   289,   290,   291,   292,   293,
     294,   295,   296,   297,   298,   300,   301,   310,   314,   315,
     316,   317,   318,   321,   322,   329,   339,   346,   355,   356,
     366,   378,   379,   386,   396,   397,   398,   400,   401,   402,
     403,   404,   405,   406,   407,   408,   409,   410,   411,   412,
     413,   414,   415,   416,   417,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   438,   440,   442,   444,   446,   448
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "Module", "Model", "Var", "Define",
  "Init", "Trans", "Atomic", "Spec", "Int", "Bool", "Top", "Bottom", "AX",
  "EX", "AF", "EG", "AR", "EU", "Neg", "Colon", "Semicolon", "Comma",
  "Dot", "DotDot", "LB1", "RB1", "LB2", "RB2", "LB3", "RB3", "Assigno",
  "Scalar", "Nego", "Ando", "Oro", "Non_equal", "Mod", "And", "Or",
  "Equal", "Add", "Minus", "Mult", "LT", "GT", "LE", "GE", "File_end",
  "Id", "I", "B", "$accept", "program", "sub_modules", "parameters_decl",
  "var_decl", "var_decl_", "symbol_decl", "symbol_decl_", "init_decl",
  "init_decl_", "trans_decl", "trans_decl_", "atomic_decl", "atomic_decl_",
  "spec_decl", "spec_decl_", "fml", "state_list", "state", "expr_type",
  "bound_vars", "scalars", "expr_pair_list", "expr_list", "expr",
  "state_expr", "nested_var", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308
};
# endif

#define YYPACT_NINF -206

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-206)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  (!!((Yytable_value) == (-1)))

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -206,    13,    59,  -206,   -29,     5,    37,    63,    47,    47,
      78,    81,   114,    14,   116,   118,  -206,   153,   127,  -206,
     117,   143,   143,   153,   122,   153,   153,    50,  -206,  -206,
     242,  -206,   128,   169,    47,   153,   150,   101,   101,   264,
    -206,  -206,   260,   151,   153,   153,   153,   153,   153,   153,
     153,   153,   153,   153,   153,   153,   153,   153,   184,  -206,
    -206,   350,   159,   178,   198,   229,   255,   229,   255,  -206,
      57,  -206,   370,   286,   243,   259,   472,   243,   472,   492,
     260,   303,   444,   444,   444,   444,   127,  -206,   261,   262,
     265,   287,   255,   284,   305,   255,   332,   153,   268,  -206,
    -206,    14,  -206,   310,   317,   326,   328,   331,   153,  -206,
     332,   334,   361,   390,   333,   -12,   153,  -206,    43,  -206,
    -206,   349,    75,   361,   339,   351,   353,   268,   149,  -206,
     159,   120,   153,   -13,   147,  -206,   352,   359,   374,   371,
     354,  -206,   153,  -206,   265,   372,   228,   153,   287,   360,
    -206,   373,  -206,   369,   378,   410,  -206,   398,   153,   394,
    -206,   174,   391,   401,   402,    34,  -206,   268,   287,  -206,
     408,   153,   153,   418,   373,   409,  -206,  -206,   416,   417,
     423,   424,   434,   435,    34,    34,   436,    48,  -206,   287,
     430,   177,   153,  -206,   218,   413,   414,   419,   420,   433,
     447,  -206,   -21,   448,   354,    34,    34,  -206,   412,   360,
    -206,   218,   218,   218,   485,   195,   461,   489,   498,   499,
     506,   508,  -206,  -206,   505,   448,  -206,  -206,   494,   153,
    -206,   308,  -206,   282,   153,   339,   218,   218,   218,   218,
     218,   218,   218,   218,   218,   218,   218,   218,    34,    34,
      34,    34,   491,   493,  -206,  -206,   212,  -206,   330,  -206,
     501,   281,   482,   501,   482,   502,   282,   325,   458,   458,
     458,   458,    19,    52,    86,   104,   519,   524,   360,  -206,
     448,   448,   448,   448,    34,    34,  -206,   521,   522,   523,
     525,   136,   158,  -206,  -206,  -206,  -206,    34,    34,   187,
     206,   448,   448,   526,   527,  -206,  -206
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       4,     0,     0,     1,     0,     0,     0,     0,     7,     7,
       0,     0,     0,     0,     0,     0,    49,     0,     0,    52,
       8,     0,     0,     0,     0,     0,     0,    66,    64,    65,
       0,    68,     0,     0,     7,     0,     0,     0,     0,     0,
      69,    71,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    56,    50,
       9,     0,     0,     0,     0,     0,     0,     0,     0,    84,
     101,   105,     0,     0,    74,    75,    73,    79,    72,    76,
      77,    78,    80,    81,    82,    83,     0,    51,     0,     0,
      14,     0,     0,     0,     0,     0,     0,     0,    67,    48,
      57,     0,    10,     0,     0,     0,     0,     0,    24,     5,
       0,     0,     0,     0,     0,     0,     0,    13,     0,    16,
       6,     0,     0,     0,    27,     0,     0,   102,   103,   106,
      11,     0,    61,    66,     0,    23,     0,     0,     0,     0,
      30,     2,     0,    12,    14,     0,    62,    61,    17,    58,
       3,    53,    26,     0,     0,     0,    15,     0,    61,     0,
      18,     0,     0,    54,     0,     0,    29,   104,    19,    63,
       0,     0,     0,     0,    53,     0,    32,    33,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    20,    21,
       0,     0,    24,    55,     0,     0,     0,     0,     0,     0,
       0,    35,     0,    45,    30,     0,     0,    22,     0,    58,
      25,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    44,    47,     0,    45,    31,    36,    37,     0,
      59,     0,    87,    86,     0,    27,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    34,    46,     0,   100,     0,    28,
      90,    91,    89,    95,    88,    92,    93,    94,    96,    97,
      98,    99,     0,     0,     0,     0,     0,     0,    58,    85,
       0,     0,     0,     0,     0,     0,    60,     0,     0,     0,
       0,     0,     0,    38,    39,    40,    41,     0,     0,     0,
       0,     0,     0,     0,     0,    42,    43
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -206,  -206,  -206,    49,   530,   426,   520,   415,   -23,  -143,
     -11,   365,   450,   327,   438,   362,  -182,   338,  -130,   463,
     393,   479,  -205,  -137,   -17,  -107,   -42
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    11,    37,    89,    65,   104,    66,   106,
      94,   121,   112,   139,   126,   154,   187,   224,   225,    20,
     164,    33,   162,   145,   146,   215,    31
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      30,    71,   201,   202,   230,   160,    39,   222,    41,    42,
     159,   130,    43,     3,   147,    68,    44,    35,    61,   205,
     206,   169,     6,   227,   228,   188,    16,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    17,    92,   280,    95,    18,   207,   176,   177,   178,
     179,   180,   181,   182,   183,   184,     7,    96,    12,   205,
     206,   185,     4,     5,     8,    19,   272,   273,   274,   275,
      23,   204,   129,   286,   132,    43,   281,    24,    25,    44,
     113,   107,    43,    60,   110,   186,    97,    26,   205,   206,
       9,   122,   205,   206,   133,    28,    29,   136,    10,   131,
      13,   134,   291,   292,   231,   232,   233,    63,    64,    14,
     282,    46,    47,    48,    49,   299,   300,    50,    51,    52,
      53,    54,    55,    56,    57,   155,   205,   206,   283,   260,
     261,   262,   263,   264,   265,   266,   267,   268,   269,   270,
     271,    34,    15,   144,   205,   206,    35,    21,    36,    22,
     287,   288,   289,   290,   190,   191,    46,    47,    48,    49,
     297,    32,    50,    51,    52,    53,    54,    55,    56,    57,
     148,   303,   304,    40,    43,   122,   205,   206,   142,    58,
      23,    62,   298,    46,    47,    48,    49,    24,    25,    50,
      51,    52,    53,    54,    55,    56,    57,    26,   205,   206,
     209,    59,    70,   171,    27,    28,    29,   172,    86,    90,
      88,   301,   256,    46,    47,    48,    49,   258,   235,    50,
      51,    52,    53,    54,    55,    56,    57,   205,   206,    91,
     302,   236,   237,   238,   239,   278,    64,   240,   241,   242,
     243,   244,   245,   246,   247,   211,   205,   206,    46,    47,
      48,    49,   158,   212,    50,    51,    52,    53,    54,    55,
      56,    57,   213,    93,    46,    47,    48,    49,    45,   214,
      50,    51,    52,    53,    54,    55,    56,    57,    46,    47,
      48,    49,    49,   101,    50,    51,    52,    53,    54,    55,
      56,    57,    69,   114,   102,    46,    46,    47,    49,    49,
      46,    47,    48,    49,    -1,    53,    50,    51,    52,    53,
      54,    55,    56,    57,    99,   108,   103,   236,   236,   237,
     239,   239,    46,    47,    48,    49,    -1,   243,    50,    51,
      52,    53,    54,    55,    56,    57,   257,   109,   105,    46,
      47,   111,    49,   116,   236,   237,   238,   239,    -1,   117,
     240,   241,   242,   243,   244,   245,   246,   247,   279,   118,
     119,   236,   237,   120,   239,   124,    46,    47,    48,    49,
      -1,   125,    50,    51,    52,    53,    54,    55,    56,    57,
      87,   135,   140,   149,   128,   141,    46,    47,    48,    49,
     138,   150,    50,    51,    52,    53,    54,    55,    56,    57,
      98,   151,   165,   152,   157,   153,    46,    47,    48,    49,
     166,   161,    50,    51,    52,    53,    54,    55,    56,    57,
     127,   168,   170,   173,   163,   174,    46,    47,    48,    49,
     175,   189,    50,    51,    52,    53,    54,    55,    56,    57,
     167,   192,   194,   195,   196,   229,    46,    47,    48,    49,
     197,   198,    50,    51,    52,    53,    54,    55,    56,    57,
     208,   199,   200,   203,   216,   217,    46,    47,    48,    49,
     218,   219,    50,    51,    52,    53,    54,    55,    56,    57,
      46,    47,    48,    49,   220,   248,    50,    51,    52,    53,
      -1,    -1,    -1,    -1,   236,   237,   238,   239,   221,   223,
     240,   241,   242,   243,    -1,    -1,    -1,    -1,    46,    47,
      -1,    49,   234,   249,    -1,    51,    52,    53,   236,   237,
      -1,   239,   250,   251,    -1,   241,   242,   243,    46,    47,
     252,    49,   253,   254,   205,    -1,    52,    53,   236,   237,
     239,   239,   276,   284,   277,    -1,   242,   243,   285,   293,
     294,   295,    38,   296,   305,   306,   143,   210,    67,   156,
     123,   137,   259,   255,   115,   100,   226,   193
};

static const yytype_uint16 yycheck[] =
{
      17,    43,   184,   185,   209,   148,    23,    28,    25,    26,
     147,    23,    25,     0,    27,    38,    29,    29,    35,    40,
      41,   158,    51,   205,   206,   168,    12,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    27,    65,    24,    67,    31,   189,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    51,    68,     9,    40,
      41,    27,     3,     4,    27,    51,   248,   249,   250,   251,
      27,    23,   114,   278,    31,    25,    24,    34,    35,    29,
      97,    92,    25,    34,    95,    51,    29,    44,    40,    41,
      27,   108,    40,    41,    51,    52,    53,    22,    51,   116,
      22,   118,   284,   285,   211,   212,   213,     6,     7,    28,
      24,    36,    37,    38,    39,   297,   298,    42,    43,    44,
      45,    46,    47,    48,    49,   142,    40,    41,    24,   236,
     237,   238,   239,   240,   241,   242,   243,   244,   245,   246,
     247,    24,    28,    23,    40,    41,    29,    31,     5,    31,
     280,   281,   282,   283,   171,   172,    36,    37,    38,    39,
      24,    34,    42,    43,    44,    45,    46,    47,    48,    49,
      23,   301,   302,    51,    25,   192,    40,    41,    29,    51,
      27,    31,    24,    36,    37,    38,    39,    34,    35,    42,
      43,    44,    45,    46,    47,    48,    49,    44,    40,    41,
      23,    32,    51,    29,    51,    52,    53,    33,    24,    31,
      51,    24,   229,    36,    37,    38,    39,   234,    23,    42,
      43,    44,    45,    46,    47,    48,    49,    40,    41,    31,
      24,    36,    37,    38,    39,    23,     7,    42,    43,    44,
      45,    46,    47,    48,    49,    27,    40,    41,    36,    37,
      38,    39,    24,    35,    42,    43,    44,    45,    46,    47,
      48,    49,    44,     8,    36,    37,    38,    39,    26,    51,
      42,    43,    44,    45,    46,    47,    48,    49,    36,    37,
      38,    39,    39,    22,    42,    43,    44,    45,    46,    47,
      48,    49,    28,    25,    32,    36,    36,    37,    39,    39,
      36,    37,    38,    39,    44,    45,    42,    43,    44,    45,
      46,    47,    48,    49,    28,    31,    51,    36,    36,    37,
      39,    39,    36,    37,    38,    39,    44,    45,    42,    43,
      44,    45,    46,    47,    48,    49,    28,    32,    51,    36,
      37,     9,    39,    33,    36,    37,    38,    39,    45,    32,
      42,    43,    44,    45,    46,    47,    48,    49,    28,    33,
      32,    36,    37,    32,    39,    31,    36,    37,    38,    39,
      45,    10,    42,    43,    44,    45,    46,    47,    48,    49,
      30,    32,    31,    31,    51,    32,    36,    37,    38,    39,
      51,    32,    42,    43,    44,    45,    46,    47,    48,    49,
      30,    27,    33,    32,    32,    51,    36,    37,    38,    39,
      32,    51,    42,    43,    44,    45,    46,    47,    48,    49,
      30,    23,    28,    32,    51,    24,    36,    37,    38,    39,
      28,    23,    42,    43,    44,    45,    46,    47,    48,    49,
      30,    23,    33,    27,    27,    33,    36,    37,    38,    39,
      27,    27,    42,    43,    44,    45,    46,    47,    48,    49,
      30,    27,    27,    27,    51,    51,    36,    37,    38,    39,
      51,    51,    42,    43,    44,    45,    46,    47,    48,    49,
      36,    37,    38,    39,    51,    24,    42,    43,    44,    45,
      46,    47,    48,    49,    36,    37,    38,    39,    51,    51,
      42,    43,    44,    45,    46,    47,    48,    49,    36,    37,
      38,    39,    27,    24,    42,    43,    44,    45,    36,    37,
      38,    39,    24,    24,    42,    43,    44,    45,    36,    37,
      24,    39,    24,    28,    40,    43,    44,    45,    36,    37,
      39,    39,    51,    24,    51,    43,    44,    45,    24,    28,
      28,    28,    22,    28,    28,    28,   130,   192,    38,   144,
     110,   123,   235,   225,   101,    86,   204,   174
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    55,    56,     0,     3,     4,    51,    51,    27,    27,
      51,    57,    57,    22,    28,    28,    12,    27,    31,    51,
      73,    31,    31,    27,    34,    35,    44,    51,    52,    53,
      78,    80,    34,    75,    24,    29,     5,    58,    58,    78,
      51,    78,    78,    25,    29,    26,    36,    37,    38,    39,
      42,    43,    44,    45,    46,    47,    48,    49,    51,    32,
      57,    78,    31,     6,     7,    60,    62,    60,    62,    28,
      51,    80,    78,    78,    78,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    24,    30,    51,    59,
      31,    31,    62,     8,    64,    62,    64,    29,    30,    28,
      75,    22,    32,    51,    61,    51,    63,    64,    31,    32,
      64,     9,    66,    78,    25,    73,    33,    32,    33,    32,
      32,    65,    78,    66,    31,    10,    68,    30,    51,    80,
      23,    78,    31,    51,    78,    32,    22,    68,    51,    67,
      31,    32,    29,    59,    23,    77,    78,    27,    23,    31,
      32,    27,    32,    51,    69,    78,    61,    32,    24,    77,
      63,    51,    76,    51,    74,    33,    32,    30,    23,    77,
      28,    29,    33,    32,    24,    28,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    27,    51,    70,    63,    23,
      78,    78,    23,    74,    33,    27,    27,    27,    27,    27,
      27,    70,    70,    27,    23,    40,    41,    63,    30,    23,
      65,    27,    35,    44,    51,    79,    51,    51,    51,    51,
      51,    51,    28,    51,    71,    72,    69,    70,    70,    33,
      76,    79,    79,    79,    27,    23,    36,    37,    38,    39,
      42,    43,    44,    45,    46,    47,    48,    49,    24,    24,
      24,    24,    24,    24,    28,    71,    78,    28,    78,    67,
      79,    79,    79,    79,    79,    79,    79,    79,    79,    79,
      79,    79,    70,    70,    70,    70,    51,    51,    23,    28,
      24,    24,    24,    24,    24,    24,    76,    72,    72,    72,
      72,    70,    70,    28,    28,    28,    28,    24,    24,    70,
      70,    24,    24,    72,    72,    28,    28
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    54,    55,    55,    56,    56,    56,    57,    57,    57,
      58,    59,    59,    60,    61,    61,    62,    63,    63,    63,
      63,    63,    63,    64,    65,    65,    66,    67,    67,    68,
      69,    69,    70,    70,    70,    70,    70,    70,    70,    70,
      70,    70,    70,    70,    70,    71,    71,    72,    73,    73,
      73,    73,    73,    74,    74,    74,    75,    75,    76,    76,
      76,    77,    77,    77,    78,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    79,    79,    79,    79,    79,
      79,    79,    79,    79,    79,    79,    79,    79,    79,    79,
      79,    80,    80,    80,    80,    80,    80
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,    13,    14,     0,    11,    12,     0,     3,     5,
       4,     4,     5,     4,     0,     5,     4,     4,     5,     6,
       7,     7,     8,     4,     0,     7,     4,     0,     8,     4,
       0,     5,     1,     1,     4,     2,     3,     3,     8,     8,
       8,     8,    12,    12,     3,     0,     2,     1,     5,     1,
       3,     4,     1,     0,     1,     3,     2,     4,     0,     5,
       8,     0,     1,     3,     1,     1,     1,     4,     1,     2,
       2,     2,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     4,     2,     2,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     6,     6,     9,     3,     6
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (moduls, YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static unsigned
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  unsigned res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
 }

#  define YY_LOCATION_PRINT(File, Loc)          \
  yy_location_print_ (File, &(Loc))

# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, Location, moduls); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, struct modul_defs* moduls)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  YYUSE (yylocationp);
  YYUSE (moduls);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, struct modul_defs* moduls)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp, moduls);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule, struct modul_defs* moduls)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                       , &(yylsp[(yyi + 1) - (yynrhs)])                       , moduls);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, moduls); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, struct modul_defs* moduls)
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  YYUSE (moduls);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Location data for the lookahead symbol.  */
YYLTYPE yylloc
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (struct modul_defs* moduls)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.
       'yyls': related to locations.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    /* The location stack.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls;
    YYLTYPE *yylsp;

    /* The locations where the error started and ended.  */
    YYLTYPE yyerror_range[3];

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yylsp = yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

/* User initialization code.  */
#line 17 "sctlparser.y" /* yacc.c:1429  */
{
    printf("Starting to parse the model file.\n");
//    printf("initial model name: %s, model size: %d\n", moduls->main_modul->name, moduls->main_modul->size);
}

#line 1468 "sctlparser.c" /* yacc.c:1429  */
  yylsp[0] = yylloc;
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yyls1, yysize * sizeof (*yylsp),
                    &yystacksize);

        yyls = yyls1;
        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location.  */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 96 "sctlparser.y" /* yacc.c:1646  */
    {
            // Modul0_list* mdls = (Modul0_list*)malloc(sizeof(Modul0_list));
            // Modul0* m0 = mk_modul0($Id, sizeof_string_type_list($var_decl), $parameters_decl, $var_decl, NULL, $init_decl, $trans_decl, $atomic_decl, $spec_decl);
            // mdls->head = *m0;
            // mdls->tail = $sub_modules;
            // $$ = mdls;
            printf("founded a main module %s size %d, without symbols definition\n", (yyvsp[-10].str_ptr), len_string_type_list((yyvsp[-5].string_type_list_ptr)));
            moduls->main_modul = mk_modul0((yyvsp[-10].str_ptr), len_string_type_list((yyvsp[-5].string_type_list_ptr)), (yyvsp[-8].string_type_list_ptr), (yyvsp[-5].string_type_list_ptr), NULL, (yyvsp[-4].string_emi_list_ptr), (yyvsp[-3].transition_list_ptr), (yyvsp[-2].string_vars_expr_list_ptr), (yyvsp[-1].string_fml_list_ptr));
            moduls->sub_moduls = (yyvsp[-12].module_list_ptr);
            YYACCEPT;
        }
#line 1667 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 3:
#line 108 "sctlparser.y" /* yacc.c:1646  */
    {
            // Modul0_list* mdls = (Modul0_list*)malloc(sizeof(Modul0_list));
            // mdls->head = *(mk_modul0($Id, sizeof_string_type_list($var_decl), $parameters_decl, $var_decl, $symbol_decl, $init_decl, $trans_decl, $atomic_decl, $spec_decl));
            // mdls->tail = $sub_modules;
            // $$ = mdls;
            printf("founded a main module %s\n", (yyvsp[-11].str_ptr));
            moduls->main_modul = mk_modul0((yyvsp[-11].str_ptr), len_string_type_list((yyvsp[-6].string_type_list_ptr)), (yyvsp[-9].string_type_list_ptr), (yyvsp[-6].string_type_list_ptr), (yyvsp[-5].string_expr_list_ptr), (yyvsp[-4].string_emi_list_ptr), (yyvsp[-3].transition_list_ptr), (yyvsp[-2].string_vars_expr_list_ptr), (yyvsp[-1].string_fml_list_ptr));
            moduls->sub_moduls = (yyvsp[-13].module_list_ptr);
            YYACCEPT;
        }
#line 1682 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 4:
#line 120 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.module_list_ptr) = NULL;}
#line 1688 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 5:
#line 122 "sctlparser.y" /* yacc.c:1646  */
    {
            Modul0_list* mdls = (Modul0_list*)malloc(sizeof(Modul0_list));
            mdls->head = *(mk_modul0((yyvsp[-8].str_ptr), len_string_type_list((yyvsp[-3].string_type_list_ptr)), (yyvsp[-6].string_type_list_ptr), (yyvsp[-3].string_type_list_ptr), NULL, (yyvsp[-2].string_emi_list_ptr), (yyvsp[-1].transition_list_ptr), NULL, NULL));
            mdls->tail = (yyvsp[-10].module_list_ptr);
            (yyval.module_list_ptr) = mdls;           
        }
#line 1699 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 6:
#line 129 "sctlparser.y" /* yacc.c:1646  */
    {
            Modul0_list* mdls = (Modul0_list*)malloc(sizeof(Modul0_list));
            mdls->head = *(mk_modul0((yyvsp[-9].str_ptr), len_string_type_list((yyvsp[-4].string_type_list_ptr)), (yyvsp[-7].string_type_list_ptr), (yyvsp[-4].string_type_list_ptr), (yyvsp[-3].string_expr_list_ptr), (yyvsp[-2].string_emi_list_ptr), (yyvsp[-1].transition_list_ptr), NULL, NULL));
            mdls->tail = (yyvsp[-11].module_list_ptr);
            (yyval.module_list_ptr) = mdls;           
        }
#line 1710 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 7:
#line 137 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.string_type_list_ptr) = NULL;}
#line 1716 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 8:
#line 139 "sctlparser.y" /* yacc.c:1646  */
    {
            String_type_list* stl = (String_type_list*)malloc(sizeof(String_type_list));
            stl->name = (yyvsp[-2].str_ptr);
            stl->type = *(yyvsp[0].type_ptr);
            stl->next = NULL;
            (yyval.string_type_list_ptr) = stl;
        }
#line 1728 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 9:
#line 147 "sctlparser.y" /* yacc.c:1646  */
    {
            String_type_list* stl = (String_type_list*)malloc(sizeof(String_type_list));
            stl->name = (yyvsp[-4].str_ptr);
            stl->type = *(yyvsp[-2].type_ptr);
            stl->next = (yyvsp[0].string_type_list_ptr);
            (yyval.string_type_list_ptr) = stl;            
        }
#line 1740 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 10:
#line 156 "sctlparser.y" /* yacc.c:1646  */
    {printf("variable declaration.\n"); (yyval.string_type_list_ptr) = (yyvsp[-1].string_type_list_ptr);}
#line 1746 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 11:
#line 160 "sctlparser.y" /* yacc.c:1646  */
    {
            String_type_list* stl = (String_type_list*)malloc(sizeof(String_type_list));
            stl->name = (yyvsp[-3].str_ptr);
            stl->type = *(yyvsp[-1].type_ptr);
            stl->next = NULL;
            (yyval.string_type_list_ptr) = stl;
        }
#line 1758 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 12:
#line 168 "sctlparser.y" /* yacc.c:1646  */
    {
            String_type_list* stl = (String_type_list*)malloc(sizeof(String_type_list));
            stl->name = (yyvsp[-4].str_ptr);
            stl->type = *(yyvsp[-2].type_ptr);
            stl->next = (yyvsp[0].string_type_list_ptr);
            (yyval.string_type_list_ptr) = stl;            
        }
#line 1770 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 13:
#line 177 "sctlparser.y" /* yacc.c:1646  */
    {printf("symbol definition.\n"); (yyval.string_expr_list_ptr) = (yyvsp[-1].string_expr_list_ptr);}
#line 1776 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 14:
#line 179 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.string_expr_list_ptr) = NULL;}
#line 1782 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 15:
#line 181 "sctlparser.y" /* yacc.c:1646  */
    {
            String_expr_list* sel = (String_expr_list*)malloc(sizeof(String_expr_list));
            sel->name = (yyvsp[-4].str_ptr);
            sel->expr = *(yyvsp[-2].expr_ptr);
            sel->next = (yyvsp[0].string_expr_list_ptr);
            (yyval.string_expr_list_ptr) = sel;
        }
#line 1794 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 16:
#line 190 "sctlparser.y" /* yacc.c:1646  */
    {printf("initial state definition.\n"); (yyval.string_emi_list_ptr) = (yyvsp[-1].string_emi_list_ptr);}
#line 1800 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 17:
#line 194 "sctlparser.y" /* yacc.c:1646  */
    {
            String_emi_list* sel = (String_emi_list*)malloc(sizeof(String_emi_list));
            sel->name = (yyvsp[-3].str_ptr);
            sel->emi = mk_emi_expr((yyvsp[-1].expr_ptr));
            sel->next = NULL;
            (yyval.string_emi_list_ptr) = sel; 
        }
#line 1812 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 18:
#line 202 "sctlparser.y" /* yacc.c:1646  */
    {
            String_emi_list* sel = (String_emi_list*)malloc(sizeof(String_emi_list));
            sel->name = (yyvsp[-4].str_ptr);
            sel->emi = mk_emi_expr((yyvsp[-2].expr_ptr));
            sel->next = (yyvsp[0].string_emi_list_ptr);
            (yyval.string_emi_list_ptr) = sel; 
        }
#line 1824 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 19:
#line 210 "sctlparser.y" /* yacc.c:1646  */
    {
            String_emi_list* sel = (String_emi_list*)malloc(sizeof(String_emi_list));
            sel->name = (yyvsp[-5].str_ptr);
            sel->emi = mk_emi_expr(mk_expr_aray((yyvsp[-2].expr_list_ptr)));
            sel->next = NULL;
            (yyval.string_emi_list_ptr) = sel; 
        }
#line 1836 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 20:
#line 218 "sctlparser.y" /* yacc.c:1646  */
    {
            String_emi_list* sel = (String_emi_list*)malloc(sizeof(String_emi_list));
            sel->name = (yyvsp[-6].str_ptr);
            sel->emi = mk_emi_expr(mk_expr_aray((yyvsp[-3].expr_list_ptr)));
            sel->next = (yyvsp[0].string_emi_list_ptr);
            (yyval.string_emi_list_ptr) = sel; 
        }
#line 1848 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 21:
#line 226 "sctlparser.y" /* yacc.c:1646  */
    {
            String_emi_list* sel = (String_emi_list*)malloc(sizeof(String_emi_list));
            sel->name = (yyvsp[-6].str_ptr);
            sel->emi = mk_emi_modul_instance((yyvsp[-4].str_ptr), (yyvsp[-2].expr_list_ptr));
            sel->next = NULL;
            (yyval.string_emi_list_ptr) = sel; 
        }
#line 1860 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 22:
#line 234 "sctlparser.y" /* yacc.c:1646  */
    {
            String_emi_list* sel = (String_emi_list*)malloc(sizeof(String_emi_list));
            sel->name = (yyvsp[-7].str_ptr);
            sel->emi = mk_emi_modul_instance((yyvsp[-5].str_ptr), (yyvsp[-3].expr_list_ptr));
            sel->next = (yyvsp[0].string_emi_list_ptr);
            (yyval.string_emi_list_ptr) = sel; 
        }
#line 1872 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 23:
#line 243 "sctlparser.y" /* yacc.c:1646  */
    {printf("transition declaration.\n"); (yyval.transition_list_ptr) = (yyvsp[-1].transition_list_ptr);}
#line 1878 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 24:
#line 245 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.transition_list_ptr) = NULL;}
#line 1884 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 25:
#line 247 "sctlparser.y" /* yacc.c:1646  */
    {
            Transition_list* tl = (Transition_list*)malloc(sizeof(Transition_list));
            Transition* t = (Transition*)malloc(sizeof(Transition));
            t->guard = *(yyvsp[-6].expr_ptr);
            t->action = (yyvsp[-3].expr_pair_list_ptr);
            tl->head = *t;
            tl->tail = (yyvsp[0].transition_list_ptr);
            (yyval.transition_list_ptr) = tl;
        }
#line 1898 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 26:
#line 258 "sctlparser.y" /* yacc.c:1646  */
    {printf("atomic formulae declaration.\n"); (yyval.string_vars_expr_list_ptr) = (yyvsp[-1].string_vars_expr_list_ptr);}
#line 1904 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 27:
#line 260 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.string_vars_expr_list_ptr) = NULL;}
#line 1910 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 28:
#line 262 "sctlparser.y" /* yacc.c:1646  */
    {
            String_vars_expr_list* svel = (String_vars_expr_list*)malloc(sizeof(String_vars_expr_list));
            svel->name = (yyvsp[-7].str_ptr);
            svel->var_list = (yyvsp[-5].string_list_ptr);
            svel->expr = (yyvsp[-2].expr_ptr);
            svel->next = (yyvsp[0].string_vars_expr_list_ptr);
            (yyval.string_vars_expr_list_ptr) = svel;
        }
#line 1923 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 29:
#line 272 "sctlparser.y" /* yacc.c:1646  */
    {printf("specification declaration.\n"); (yyval.string_fml_list_ptr) = (yyvsp[-1].string_fml_list_ptr);}
#line 1929 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 30:
#line 274 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.string_fml_list_ptr) = NULL;}
#line 1935 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 31:
#line 276 "sctlparser.y" /* yacc.c:1646  */
    {
            String_fml_list* sfl = (String_fml_list*)malloc(sizeof(String_fml_list));
            sfl->name = (yyvsp[-4].str_ptr);
            sfl->fml = *(yyvsp[-2].fml_ptr);
            sfl->next = (yyvsp[0].string_fml_list_ptr);
            (yyval.string_fml_list_ptr) = sfl;
        }
#line 1947 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 32:
#line 286 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.fml_ptr) = mk_fml_top();}
#line 1953 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 33:
#line 287 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.fml_ptr) = mk_fml_bottom();}
#line 1959 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 34:
#line 288 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.fml_ptr) = mk_fml_atom((yyvsp[-3].str_ptr), (yyvsp[-1].state_list_ptr));}
#line 1965 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 35:
#line 289 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.fml_ptr) = mk_fml_neg((yyvsp[0].fml_ptr));}
#line 1971 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 36:
#line 290 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.fml_ptr) = mk_fml_and((yyvsp[-2].fml_ptr), (yyvsp[0].fml_ptr));}
#line 1977 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 37:
#line 291 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.fml_ptr) = mk_fml_or((yyvsp[-2].fml_ptr), (yyvsp[0].fml_ptr));}
#line 1983 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 38:
#line 292 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.fml_ptr) = mk_fml_ax((yyvsp[-5].str_ptr), (yyvsp[-3].fml_ptr), (yyvsp[-1].state_ptr));}
#line 1989 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 39:
#line 293 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.fml_ptr) = mk_fml_ex((yyvsp[-5].str_ptr), (yyvsp[-3].fml_ptr), (yyvsp[-1].state_ptr));}
#line 1995 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 40:
#line 294 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.fml_ptr) = mk_fml_af((yyvsp[-5].str_ptr), (yyvsp[-3].fml_ptr), (yyvsp[-1].state_ptr));}
#line 2001 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 41:
#line 295 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.fml_ptr) = mk_fml_eg((yyvsp[-5].str_ptr), (yyvsp[-3].fml_ptr), (yyvsp[-1].state_ptr));}
#line 2007 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 42:
#line 296 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.fml_ptr) = mk_fml_ar((yyvsp[-9].str_ptr), (yyvsp[-7].str_ptr), (yyvsp[-5].fml_ptr), (yyvsp[-3].fml_ptr), (yyvsp[-1].state_ptr));}
#line 2013 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 43:
#line 297 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.fml_ptr) = mk_fml_eu((yyvsp[-9].str_ptr), (yyvsp[-7].str_ptr), (yyvsp[-5].fml_ptr), (yyvsp[-3].fml_ptr), (yyvsp[-1].state_ptr));}
#line 2019 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 44:
#line 298 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.fml_ptr) = (yyvsp[-1].fml_ptr);}
#line 2025 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 45:
#line 300 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.state_list_ptr) = NULL;}
#line 2031 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 46:
#line 302 "sctlparser.y" /* yacc.c:1646  */
    {
            State_list* sl = (State_list*)malloc(sizeof(State_list));
            sl->head = (yyvsp[-1].state_ptr);
            sl->tail = (yyvsp[0].state_list_ptr);
            (yyval.state_list_ptr) = sl;
        }
#line 2042 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 47:
#line 310 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.state_ptr) = mk_state_iden((yyvsp[0].str_ptr));}
#line 2048 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 48:
#line 314 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.type_ptr) = mk_type_int((yyvsp[-3].expr_ptr), (yyvsp[-1].expr_ptr));}
#line 2054 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 49:
#line 315 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.type_ptr) = mk_type_bool();}
#line 2060 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 50:
#line 316 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.type_ptr) = mk_type_scalar(*(yyvsp[-1].string_list_ptr));}
#line 2066 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 51:
#line 317 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.type_ptr) = mk_type_array((yyvsp[-3].type_ptr), (yyvsp[-1].expr_ptr));}
#line 2072 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 52:
#line 318 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.type_ptr) = mk_type_modul((yyvsp[0].str_ptr));}
#line 2078 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 53:
#line 321 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.string_list_ptr) = NULL;}
#line 2084 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 54:
#line 323 "sctlparser.y" /* yacc.c:1646  */
    {
            String_list* sl = (String_list*)malloc(sizeof(String_list));
            sl->head = (yyvsp[0].str_ptr);
            sl->tail = NULL;
            (yyval.string_list_ptr) = sl;
        }
#line 2095 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 55:
#line 330 "sctlparser.y" /* yacc.c:1646  */
    {
            String_list* sl = (String_list*)malloc(sizeof(String_list));
            sl->head = (yyvsp[-2].str_ptr);
            sl->tail = (yyvsp[0].string_list_ptr);
            (yyval.string_list_ptr) = sl;            
        }
#line 2106 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 56:
#line 340 "sctlparser.y" /* yacc.c:1646  */
    {
            String_list* sl = (String_list*)malloc(sizeof(String_list));
            sl->head = (yyvsp[0].str_ptr);
            sl->tail = NULL;
            (yyval.string_list_ptr) = sl;
        }
#line 2117 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 57:
#line 347 "sctlparser.y" /* yacc.c:1646  */
    {
            String_list* sl = (String_list*)malloc(sizeof(String_list));
            sl->head = (yyvsp[-2].str_ptr);
            sl->tail = (yyvsp[0].string_list_ptr);
            (yyval.string_list_ptr) = sl;            
        }
#line 2128 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 58:
#line 355 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_pair_list_ptr) = NULL;}
#line 2134 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 59:
#line 357 "sctlparser.y" /* yacc.c:1646  */
    {
            Expr_pair_list* epl = (Expr_pair_list*)malloc(sizeof(Expr_pair_list));
            Expr_pair* ep = (Expr_pair*)malloc(sizeof(Expr_pair));
            ep->first = *(mk_expr_svar((yyvsp[-4].str_ptr)));
            ep->second = *(yyvsp[-2].expr_ptr);
            epl->head = *ep;
            epl->tail = (yyvsp[0].expr_pair_list_ptr);
            (yyval.expr_pair_list_ptr) = epl;
        }
#line 2148 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 60:
#line 367 "sctlparser.y" /* yacc.c:1646  */
    {
            Expr_pair_list* epl = (Expr_pair_list*)malloc(sizeof(Expr_pair_list));
            Expr_pair* ep = (Expr_pair*)malloc(sizeof(Expr_pair));
            ep->first = *(mk_expr_var_index((yyvsp[-7].str_ptr), (yyvsp[-5].expr_ptr)));
            ep->second = *(yyvsp[-2].expr_ptr);
            epl->head = *ep;
            epl->tail = (yyvsp[0].expr_pair_list_ptr);
            (yyval.expr_pair_list_ptr) = epl;
        }
#line 2162 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 61:
#line 378 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_list_ptr) = NULL;}
#line 2168 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 62:
#line 380 "sctlparser.y" /* yacc.c:1646  */
    {
            Expr_list* el = (Expr_list*)malloc(sizeof(Expr_list));
            el->head = (yyvsp[0].expr_ptr);
            el->tail = NULL;
            (yyval.expr_list_ptr) = el;
        }
#line 2179 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 63:
#line 387 "sctlparser.y" /* yacc.c:1646  */
    {
            Expr_list* el = (Expr_list*)malloc(sizeof(Expr_list));
            el->head = (yyvsp[-2].expr_ptr);
            el->tail = (yyvsp[0].expr_list_ptr);
            (yyval.expr_list_ptr) = el;
        }
#line 2190 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 64:
#line 396 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_cnst((yyvsp[0].integer));}
#line 2196 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 65:
#line 397 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_cnst((yyvsp[0].integer));}
#line 2202 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 66:
#line 398 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_svar((yyvsp[0].str_ptr));}
#line 2208 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 67:
#line 400 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_var_index((yyvsp[-3].str_ptr), (yyvsp[-1].expr_ptr));}
#line 2214 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 68:
#line 401 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = (yyvsp[0].expr_ptr);}
#line 2220 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 69:
#line 402 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_scalar((yyvsp[0].str_ptr));}
#line 2226 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 70:
#line 403 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_negi((yyvsp[0].expr_ptr));}
#line 2232 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 71:
#line 404 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_negb((yyvsp[0].expr_ptr));}
#line 2238 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 72:
#line 405 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_equl((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2244 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 73:
#line 406 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_negb(mk_expr_equl((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr)));}
#line 2250 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 74:
#line 407 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_ando((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2256 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 75:
#line 408 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_oro((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2262 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 76:
#line 409 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_add((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2268 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 77:
#line 410 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_minus((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2274 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 78:
#line 411 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_mult((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2280 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 79:
#line 412 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_mod((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2286 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 80:
#line 413 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_lt((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2292 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 81:
#line 414 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_gt((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2298 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 82:
#line 415 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_le((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2304 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 83:
#line 416 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_ge((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2310 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 84:
#line 417 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = (yyvsp[-1].expr_ptr);}
#line 2316 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 85:
#line 420 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_state_expr((yyvsp[-3].str_ptr), (yyvsp[-1].expr_ptr));}
#line 2322 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 86:
#line 421 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_negi((yyvsp[0].expr_ptr));}
#line 2328 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 87:
#line 422 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_negb((yyvsp[0].expr_ptr));}
#line 2334 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 88:
#line 423 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_equl((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2340 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 89:
#line 424 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_negb(mk_expr_equl((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr)));}
#line 2346 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 90:
#line 425 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_ando((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2352 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 91:
#line 426 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_oro((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2358 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 92:
#line 427 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_add((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2364 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 93:
#line 428 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_minus((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2370 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 94:
#line 429 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_mult((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2376 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 95:
#line 430 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_mod((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2382 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 96:
#line 431 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_lt((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2388 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 97:
#line 432 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_gt((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2394 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 98:
#line 433 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_le((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2400 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 99:
#line 434 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_ge((yyvsp[-2].expr_ptr), (yyvsp[0].expr_ptr));}
#line 2406 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 100:
#line 435 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = (yyvsp[-1].expr_ptr);}
#line 2412 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 101:
#line 439 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_nested_var(mk_expr_svar((yyvsp[-2].str_ptr)), mk_expr_svar((yyvsp[0].str_ptr)));}
#line 2418 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 102:
#line 441 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_nested_var(mk_expr_svar((yyvsp[-5].str_ptr)), mk_expr_var_index((yyvsp[-3].str_ptr), (yyvsp[-1].expr_ptr)));}
#line 2424 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 103:
#line 443 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_nested_var(mk_expr_var_index((yyvsp[-5].str_ptr), (yyvsp[-3].expr_ptr)), mk_expr_svar((yyvsp[0].str_ptr)));}
#line 2430 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 104:
#line 445 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_nested_var(mk_expr_var_index((yyvsp[-8].str_ptr), (yyvsp[-6].expr_ptr)), mk_expr_var_index((yyvsp[-3].str_ptr), (yyvsp[-1].expr_ptr)));}
#line 2436 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 105:
#line 447 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_nested_var(mk_expr_svar((yyvsp[-2].str_ptr)), (yyvsp[0].expr_ptr));}
#line 2442 "sctlparser.c" /* yacc.c:1646  */
    break;

  case 106:
#line 449 "sctlparser.y" /* yacc.c:1646  */
    {(yyval.expr_ptr) = mk_expr_nested_var(mk_expr_var_index((yyvsp[-5].str_ptr), (yyvsp[-3].expr_ptr)), (yyvsp[0].expr_ptr));}
#line 2448 "sctlparser.c" /* yacc.c:1646  */
    break;


#line 2452 "sctlparser.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (moduls, YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (moduls, yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }

  yyerror_range[1] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc, moduls);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  yyerror_range[1] = yylsp[1-yylen];
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, yylsp, moduls);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the lookahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, yyerror_range, 2);
  *++yylsp = yyloc;

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (moduls, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc, moduls);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, yylsp, moduls);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 455 "sctlparser.y" /* yacc.c:1906  */


void yyerror (struct modul_defs* moduls, const char* err_msg)
{
    // fprintf(stderr, "%s, in line %d: '%s'.\n", err_msg, yylineno, yytext);
    fprintf(stderr, "%s\n", err_msg);
}
