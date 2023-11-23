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
#line 1 "parser.yacc" /* yacc.c:339  */

#include <stdio.h>
#include "TeaplAst.h"

extern A_pos pos;
extern A_program root;
extern int line, col;
extern int yylex(void);
extern "C"{
extern void yyerror(char *s); 
extern int  yywrap();
}


#line 81 "y.tab.cpp" /* yacc.c:339  */

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
   by #include "y.tab.hpp".  */
#ifndef YY_YY_Y_TAB_HPP_INCLUDED
# define YY_YY_Y_TAB_HPP_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    ADD = 258,
    SUB = 259,
    MUL = 260,
    DIV = 261,
    SEMICOLON = 262,
    LPAR = 263,
    RPAR = 264,
    DOT = 265,
    COMMA = 266,
    AND = 267,
    GRE = 268,
    STRUCT = 269,
    LSQU = 270,
    RSQU = 271,
    LET = 272,
    ASSIGN = 273,
    INT = 274,
    LBRA = 275,
    RBRA = 276,
    OR = 277,
    NOT = 278,
    LES = 279,
    EGRE = 280,
    ELES = 281,
    EQUAL = 282,
    NEQUAL = 283,
    IF = 284,
    ELSE = 285,
    WHILE = 286,
    FUNC = 287,
    RETV = 288,
    CONTINUE = 289,
    BREAK = 290,
    RET = 291,
    COL = 292,
    ID = 293,
    NUM = 294
  };
#endif
/* Tokens.  */
#define ADD 258
#define SUB 259
#define MUL 260
#define DIV 261
#define SEMICOLON 262
#define LPAR 263
#define RPAR 264
#define DOT 265
#define COMMA 266
#define AND 267
#define GRE 268
#define STRUCT 269
#define LSQU 270
#define RSQU 271
#define LET 272
#define ASSIGN 273
#define INT 274
#define LBRA 275
#define RBRA 276
#define OR 277
#define NOT 278
#define LES 279
#define EGRE 280
#define ELES 281
#define EQUAL 282
#define NEQUAL 283
#define IF 284
#define ELSE 285
#define WHILE 286
#define FUNC 287
#define RETV 288
#define CONTINUE 289
#define BREAK 290
#define RET 291
#define COL 292
#define ID 293
#define NUM 294

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 16 "parser.yacc" /* yacc.c:355  */

  A_pos pos;
  A_tokenId tokenId;
  A_tokenNum tokenNum;
  A_program program;
  A_programElementList programElementList;
  A_programElement programElement;
  A_arithExpr arithExpr;
  A_exprUnit exprUnit;
  A_arrayExpr arrayExpr;
  A_structDef structDef;
  A_varDeclStmt varDeclStmt;
  A_fnDeclStmt fnDeclStmt;
  A_fnDef fnDef;
  A_fnCall fnCall;
  A_arithUExpr arithUExpr;
  A_arithBiOpExpr arithBiOpExpr;
  A_memberExpr memberExpr;
  A_varDecl varDecl;
  A_varDef varDef;
  A_varDeclScalar varDeclScalar;
  A_varDeclArray varDeclArray;
  A_varDefScalar varDefScalar; 
  A_type type;
  A_nativeType nativeType;//
  A_varDefArray varDefArray;
  A_rightVal rightVal;
  A_rightValList rightValList;
  A_indexExpr indexExpr;
  A_boolExpr boolExpr;
  A_boolUnit boolUnit;
  A_varDeclList varDeclList;
  A_fnDecl fnDecl;
  A_paramDecl paramDecl;
  A_codeBlockStmtList codeBlockStmtList;
  A_boolBiOp boolBiOp;//
  A_codeBlockStmt codeBlockStmt;
  A_assignStmt assignStmt;
  A_callStmt callStmt;
  A_ifStmt ifStmt;
  A_whileStmt whileStmt;
  A_returnStmt returnStmt;
  A_leftVal leftVal;

#line 244 "y.tab.cpp" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_HPP_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 261 "y.tab.cpp" /* yacc.c:358  */

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
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

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
#define YYFINAL  22
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   215

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  40
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  38
/* YYNRULES -- Number of rules.  */
#define YYNRULES  92
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  179

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   294

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
      35,    36,    37,    38,    39
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   148,   148,   156,   161,   167,   171,   175,   179,   183,
     190,   194,   199,   203,   210,   214,   221,   225,   232,   236,
     243,   247,   254,   258,   265,   269,   276,   280,   287,   294,
     298,   302,   306,   313,   317,   321,   325,   329,   333,   337,
     344,   348,   355,   359,   366,   373,   377,   384,   391,   395,
     399,   403,   410,   414,   418,   425,   429,   433,   437,   441,
     445,   449,   453,   460,   467,   471,   476,   482,   489,   496,
     500,   507,   514,   519,   525,   529,   533,   537,   541,   545,
     549,   553,   557,   564,   571,   575,   579,   586,   593,   597,
     604,   611,   615
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "ADD", "SUB", "MUL", "DIV", "SEMICOLON",
  "LPAR", "RPAR", "DOT", "COMMA", "AND", "GRE", "STRUCT", "LSQU", "RSQU",
  "LET", "ASSIGN", "INT", "LBRA", "RBRA", "OR", "NOT", "LES", "EGRE",
  "ELES", "EQUAL", "NEQUAL", "IF", "ELSE", "WHILE", "FUNC", "RETV",
  "CONTINUE", "BREAK", "RET", "COL", "ID", "NUM", "$accept", "Program",
  "ProgramElementList", "ProgramElement", "VarDeclStmt", "VarDecl",
  "VarDeclArray", "VarDeclScalar", "Type", "VarDef", "VarDefScalar",
  "RightVal", "ArithExpr", "ArithUExpr", "ArithBiOpExpr", "ExprUnit",
  "FnCall", "RightValList", "ArrayExpr", "IndexExpr", "MemberExpr",
  "VarDefArray", "BoolExpr", "BoolUnit", "StructDef", "VarDeclList",
  "FnDef", "FnDeclStmt", "FnDecl", "ParamDecl", "CodeBlockStmtList",
  "CodeBlockStmt", "AssignStmt", "LeftVal", "CallStmt", "IfStmt",
  "WhileStmt", "ReturnStmt", YY_NULLPTR
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
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294
};
# endif

#define YYPACT_NINF -89

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-89)))

#define YYTABLE_NINF -87

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      85,   -89,     1,    37,    57,    35,   -89,    85,   -89,   -89,
     -89,   -89,    16,    -5,    23,    97,   -89,   -89,   113,   -89,
     -89,   118,   -89,   -89,   -89,   112,    89,    91,    86,     2,
     -89,   -89,    89,   -89,   120,   134,   137,   138,     6,   144,
     -89,   151,   -89,   -89,   133,   112,   -89,    33,   -89,   -89,
     -89,   -89,    59,   159,   160,   152,    62,    86,    86,   157,
     -89,   -89,   180,   -89,   -89,   136,   -89,    15,   156,    -3,
     -89,   167,   -89,   -89,   162,   -89,   169,    86,    86,   -89,
     -89,   -89,   181,     8,   -89,   -89,   -89,   149,    25,    86,
     150,     2,    89,   -89,    79,    62,   -89,   170,   101,   136,
     -89,    62,    62,    62,    62,    62,    62,    62,    62,    62,
      62,    86,    86,    86,   158,   109,   147,   -89,   -89,   179,
     183,   -89,   -89,   -89,   177,   187,   182,   -89,   -89,   175,
       2,   -89,   -89,   -89,   106,   106,   -89,   -89,   -89,   -89,
     -89,   -89,   -89,   -89,   -89,   184,   -89,     2,   185,   186,
      86,   -89,   -89,   -89,   163,    29,   189,   -89,   112,   112,
     -89,     2,   -89,   176,   188,   178,   190,   -89,   -89,    68,
     171,   -89,   -89,   191,   193,   -89,   112,   194,   -89
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       4,     9,     0,     0,     0,     0,     2,     4,     5,     6,
       8,     7,     0,     0,    17,     0,    13,    12,     0,    20,
      21,     0,     1,     3,    68,    73,    66,     0,     0,     0,
      10,    11,    66,    82,     0,     0,     0,     0,     0,    84,
      74,     0,    85,    86,     0,    73,    75,     0,    76,    77,
      78,    79,    17,    65,     0,     0,     0,     0,     0,    34,
      33,    23,    24,    39,    26,    27,    36,    37,    38,    25,
      54,     0,    18,    19,    16,    71,     0,     0,     0,    80,
      81,    92,     0,     0,    87,    67,    72,     0,     0,     0,
       0,     0,    66,    63,    15,     0,    28,     0,     0,     0,
      62,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    69,     0,     0,    91,    41,    43,
       0,    47,    45,    46,     0,     0,     0,    16,    64,     0,
       0,    27,    35,    61,    29,    30,    31,    32,    57,    55,
      58,    56,    59,    60,    52,    53,    22,     0,     0,     0,
       0,    40,    44,    83,    15,     0,    14,    70,    73,    73,
      42,     0,    51,     0,     0,     0,     0,    14,    50,     0,
      88,    90,    49,     0,     0,    48,    73,     0,    89
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -89,   -89,   195,   -89,    98,   200,   -89,   -89,   -88,   -89,
     -89,   -20,   -46,   -89,   -89,   -24,   -25,   -47,   -23,   -89,
     -21,   -89,   -50,   146,   -89,   -27,   -89,   -89,   -89,   -89,
     -44,   -89,   -89,   -19,   -89,   -89,   -89,   -89
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     5,     6,     7,    40,    53,    16,    17,    74,    18,
      19,   119,    62,    63,    64,    65,    66,   120,    67,   124,
      68,    20,    69,    70,     9,    54,    10,    11,    12,    76,
      44,    45,    46,    71,    48,    49,    50,    51
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      41,    86,    42,   127,    43,    75,    47,    98,    61,   111,
      56,    97,    56,    81,    57,    26,    57,   118,    82,   112,
      41,    72,    42,    24,    43,   -85,    47,   115,   116,    58,
     -85,    58,    96,    56,    99,    22,    25,    57,    27,    13,
      73,    28,   156,    87,    59,    60,    59,    60,    88,    97,
     162,    89,    58,    99,    99,   134,   135,   136,   137,   157,
      29,   144,   145,   122,   123,   128,    56,    59,    60,   125,
      95,   131,    56,   167,    90,    14,    57,   131,   131,   131,
     131,   138,   139,   140,   141,   142,   143,    99,    99,   172,
      56,    58,     1,   146,    57,    21,    91,   129,     8,     2,
      59,    60,     3,   160,    30,     8,    59,    60,   163,    58,
     133,   103,   104,   111,   165,   166,   130,     4,   148,    33,
      31,   111,   173,   112,    59,    60,    32,    52,    77,     3,
      55,   112,   177,    41,    41,    42,    42,    43,    43,    47,
      47,    34,    78,    35,    79,    80,    36,    37,    38,   105,
      39,    41,    83,    42,    85,    43,   149,    47,    84,   111,
     106,   107,   108,   109,   110,    83,   -86,   -84,    94,   112,
      92,   -86,   -84,   101,   102,   103,   104,    87,   114,   132,
     113,    93,    88,   101,   102,   103,   104,   121,   117,   126,
     150,   147,   151,   152,   153,   155,   111,   168,   154,   170,
     161,   174,    23,    15,   100,   158,   159,   164,   169,     0,
       0,   171,   175,   176,     0,   178
};

static const yytype_int16 yycheck[] =
{
      25,    45,    25,    91,    25,    32,    25,    57,    28,    12,
       4,    57,     4,     7,     8,    20,     8,     9,    38,    22,
      45,    19,    45,     7,    45,    10,    45,    77,    78,    23,
      15,    23,    56,     4,    58,     0,    20,     8,    15,    38,
      38,    18,   130,    10,    38,    39,    38,    39,    15,    95,
      21,    18,    23,    77,    78,   101,   102,   103,   104,   147,
      37,   111,   112,    38,    39,    92,     4,    38,    39,    89,
       8,    95,     4,   161,    15,    38,     8,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    21,
       4,    23,     7,   113,     8,    38,    37,    18,     0,    14,
      38,    39,    17,   150,     7,     7,    38,    39,   155,    23,
       9,     5,     6,    12,   158,   159,    37,    32,     9,     7,
       7,    12,   169,    22,    38,    39,     8,    38,     8,    17,
      39,    22,   176,   158,   159,   158,   159,   158,   159,   158,
     159,    29,     8,    31,     7,     7,    34,    35,    36,    13,
      38,   176,     8,   176,    21,   176,     9,   176,     7,    12,
      24,    25,    26,    27,    28,     8,    10,    10,    16,    22,
      11,    15,    15,     3,     4,     5,     6,    10,     9,     9,
      18,    21,    15,     3,     4,     5,     6,    38,     7,    39,
      11,    33,     9,    16,     7,    20,    12,    21,    16,    21,
      37,    30,     7,     3,    58,    20,    20,    18,    20,    -1,
      -1,    21,    21,    20,    -1,    21
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     7,    14,    17,    32,    41,    42,    43,    44,    64,
      66,    67,    68,    38,    38,    45,    46,    47,    49,    50,
      61,    38,     0,    42,     7,    20,    20,    15,    18,    37,
       7,     7,     8,     7,    29,    31,    34,    35,    36,    38,
      44,    56,    58,    60,    70,    71,    72,    73,    74,    75,
      76,    77,    38,    45,    65,    39,     4,     8,    23,    38,
      39,    51,    52,    53,    54,    55,    56,    58,    60,    62,
      63,    73,    19,    38,    48,    65,    69,     8,     8,     7,
       7,     7,    51,     8,     7,    21,    70,    10,    15,    18,
      15,    37,    11,    21,    16,     8,    55,    52,    62,    55,
      63,     3,     4,     5,     6,    13,    24,    25,    26,    27,
      28,    12,    22,    18,     9,    62,    62,     7,     9,    51,
      57,    38,    38,    39,    59,    51,    39,    48,    65,    18,
      37,    55,     9,     9,    52,    52,    52,    52,    55,    55,
      55,    55,    55,    55,    62,    62,    51,    33,     9,     9,
      11,     9,    16,     7,    16,    20,    48,    48,    20,    20,
      57,    37,    21,    57,    18,    70,    70,    48,    21,    20,
      21,    21,    21,    57,    30,    21,    20,    70,    21
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    40,    41,    42,    42,    43,    43,    43,    43,    43,
      44,    44,    45,    45,    46,    46,    47,    47,    48,    48,
      49,    49,    50,    50,    51,    51,    52,    52,    53,    54,
      54,    54,    54,    55,    55,    55,    55,    55,    55,    55,
      56,    56,    57,    57,    58,    59,    59,    60,    61,    61,
      61,    61,    62,    62,    62,    63,    63,    63,    63,    63,
      63,    63,    63,    64,    65,    65,    65,    66,    67,    68,
      68,    69,    70,    70,    71,    71,    71,    71,    71,    71,
      71,    71,    71,    72,    73,    73,    73,    74,    75,    75,
      76,    77,    77
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     0,     1,     1,     1,     1,     1,
       3,     3,     1,     1,     6,     4,     3,     1,     1,     1,
       1,     1,     5,     3,     1,     1,     1,     1,     2,     3,
       3,     3,     3,     1,     1,     3,     1,     1,     1,     1,
       4,     3,     3,     1,     4,     1,     1,     3,    10,     9,
       8,     7,     3,     3,     1,     3,     3,     3,     3,     3,
       3,     3,     2,     5,     3,     1,     0,     4,     2,     5,
       7,     1,     2,     0,     1,     1,     1,     1,     1,     1,
       2,     2,     1,     4,     1,     1,     1,     2,     7,    11,
       7,     3,     2
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
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



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

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
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
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
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
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
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
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
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
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
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
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

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

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
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

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

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
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

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


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 149 "parser.yacc" /* yacc.c:1646  */
    {  
  root = A_Program((yyvsp[0].programElementList));
  (yyval.program) = A_Program((yyvsp[0].programElementList));
}
#line 1473 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 3:
#line 157 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.programElementList) = A_ProgramElementList((yyvsp[-1].programElement), (yyvsp[0].programElementList));
}
#line 1481 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 4:
#line 161 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.programElementList) = NULL;
}
#line 1489 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 5:
#line 168 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.programElement) = A_ProgramVarDeclStmt((yyvsp[0].varDeclStmt)->pos, (yyvsp[0].varDeclStmt));
}
#line 1497 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 6:
#line 172 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.programElement) = A_ProgramStructDef((yyvsp[0].structDef)->pos, (yyvsp[0].structDef));
}
#line 1505 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 7:
#line 176 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.programElement) = A_ProgramFnDeclStmt((yyvsp[0].fnDeclStmt)->pos, (yyvsp[0].fnDeclStmt));
}
#line 1513 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 8:
#line 180 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.programElement) = A_ProgramFnDef((yyvsp[0].fnDef)->pos, (yyvsp[0].fnDef));
}
#line 1521 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 9:
#line 184 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.programElement) = A_ProgramNullStmt((yyvsp[0].pos));
}
#line 1529 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 10:
#line 191 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDeclStmt) = A_VarDeclStmt((yyvsp[-2].pos), (yyvsp[-1].varDecl));
}
#line 1537 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 11:
#line 195 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDeclStmt) = A_VarDefStmt((yyvsp[-2].pos), (yyvsp[-1].varDef));
}
#line 1545 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 12:
#line 200 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDecl) = A_VarDecl_Scalar((yyvsp[0].varDeclScalar)->pos, (yyvsp[0].varDeclScalar));
}
#line 1553 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 13:
#line 204 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDecl) = A_VarDecl_Array((yyvsp[0].varDeclArray)->pos, (yyvsp[0].varDeclArray));
}
#line 1561 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 14:
#line 211 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDeclArray) = A_VarDeclArray((yyvsp[-5].tokenId)->pos, (yyvsp[-5].tokenId)->id, (yyvsp[-3].tokenNum)->num, (yyvsp[0].type));
}
#line 1569 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 15:
#line 215 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDeclArray) = A_VarDeclArray((yyvsp[-3].tokenId)->pos, (yyvsp[-3].tokenId)->id, (yyvsp[-1].tokenNum)->num, NULL);
}
#line 1577 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 16:
#line 222 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDeclScalar) = A_VarDeclScalar((yyvsp[-2].tokenId)->pos, (yyvsp[-2].tokenId)->id, (yyvsp[0].type));
}
#line 1585 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 17:
#line 226 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDeclScalar) = A_VarDeclScalar((yyvsp[0].tokenId)->pos, (yyvsp[0].tokenId)->id, NULL);
}
#line 1593 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 18:
#line 233 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.type) = A_NativeType((yyvsp[0].pos), A_intTypeKind);
}
#line 1601 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 19:
#line 237 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.type) = A_StructType((yyvsp[0].tokenId)->pos, (yyvsp[0].tokenId)->id);
}
#line 1609 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 20:
#line 244 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDef)=A_VarDef_Scalar((yyvsp[0].varDefScalar)->pos, (yyvsp[0].varDefScalar));
}
#line 1617 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 21:
#line 248 "parser.yacc" /* yacc.c:1646  */
    {   
  (yyval.varDef)=A_VarDef_Array((yyvsp[0].varDefArray)->pos, (yyvsp[0].varDefArray));
}
#line 1625 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 22:
#line 255 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDefScalar)=A_VarDefScalar((yyvsp[-4].tokenId)->pos, (yyvsp[-4].tokenId)->id, (yyvsp[-2].type), (yyvsp[0].rightVal));
}
#line 1633 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 23:
#line 259 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDefScalar)=A_VarDefScalar((yyvsp[-2].tokenId)->pos, (yyvsp[-2].tokenId)->id, NULL, (yyvsp[0].rightVal));
}
#line 1641 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 24:
#line 266 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.rightVal)=A_ArithExprRVal((yyvsp[0].arithExpr)->pos, (yyvsp[0].arithExpr));
}
#line 1649 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 25:
#line 270 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.rightVal)=A_BoolExprRVal((yyvsp[0].boolExpr)->pos, (yyvsp[0].boolExpr));
}
#line 1657 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 26:
#line 277 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.arithExpr) = A_ArithBiOp_Expr((yyvsp[0].arithBiOpExpr)->pos, (yyvsp[0].arithBiOpExpr));
}
#line 1665 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 27:
#line 281 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.arithExpr) = A_ExprUnit((yyvsp[0].exprUnit)->pos, (yyvsp[0].exprUnit));
}
#line 1673 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 28:
#line 288 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.arithUExpr) = A_ArithUExpr((yyvsp[-1].pos), A_neg, (yyvsp[0].exprUnit));
}
#line 1681 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 29:
#line 295 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.arithBiOpExpr) = A_ArithBiOpExpr((yyvsp[-2].arithExpr)->pos, A_add, (yyvsp[-2].arithExpr), (yyvsp[0].arithExpr));
}
#line 1689 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 30:
#line 299 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.arithBiOpExpr) = A_ArithBiOpExpr((yyvsp[-2].arithExpr)->pos, A_sub, (yyvsp[-2].arithExpr), (yyvsp[0].arithExpr));
}
#line 1697 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 31:
#line 303 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.arithBiOpExpr) = A_ArithBiOpExpr((yyvsp[-2].arithExpr)->pos, A_mul, (yyvsp[-2].arithExpr), (yyvsp[0].arithExpr));
}
#line 1705 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 32:
#line 307 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.arithBiOpExpr) = A_ArithBiOpExpr((yyvsp[-2].arithExpr)->pos, A_div, (yyvsp[-2].arithExpr), (yyvsp[0].arithExpr));
}
#line 1713 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 33:
#line 314 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.exprUnit) = A_NumExprUnit((yyvsp[0].tokenNum)->pos, (yyvsp[0].tokenNum)->num);
}
#line 1721 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 34:
#line 318 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.exprUnit) = A_IdExprUnit((yyvsp[0].tokenId)->pos, (yyvsp[0].tokenId)->id);
}
#line 1729 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 35:
#line 322 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.exprUnit) = A_ArithExprUnit((yyvsp[-2].pos), (yyvsp[-1].arithExpr));
}
#line 1737 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 36:
#line 326 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.exprUnit) = A_CallExprUnit((yyvsp[0].fnCall)->pos, (yyvsp[0].fnCall));
}
#line 1745 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 37:
#line 330 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.exprUnit) = A_ArrayExprUnit((yyvsp[0].arrayExpr)->pos, (yyvsp[0].arrayExpr));
}
#line 1753 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 38:
#line 334 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.exprUnit) = A_MemberExprUnit((yyvsp[0].memberExpr)->pos, (yyvsp[0].memberExpr));
}
#line 1761 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 39:
#line 338 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.exprUnit) = A_ArithUExprUnit((yyvsp[0].arithUExpr)->pos, (yyvsp[0].arithUExpr));
}
#line 1769 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 40:
#line 345 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.fnCall) = A_FnCall((yyvsp[-3].tokenId)->pos, (yyvsp[-3].tokenId)->id, (yyvsp[-1].rightValList));
}
#line 1777 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 41:
#line 349 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.fnCall) = A_FnCall((yyvsp[-2].tokenId)->pos, (yyvsp[-2].tokenId)->id, NULL);
}
#line 1785 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 42:
#line 356 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.rightValList) = A_RightValList((yyvsp[-2].rightVal), (yyvsp[0].rightValList));
}
#line 1793 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 43:
#line 360 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.rightValList) = A_RightValList((yyvsp[0].rightVal), NULL);
}
#line 1801 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 44:
#line 367 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.arrayExpr) = A_ArrayExpr((yyvsp[-3].leftVal)->pos, (yyvsp[-3].leftVal), (yyvsp[-1].indexExpr));
}
#line 1809 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 45:
#line 374 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.indexExpr) = A_IdIndexExpr((yyvsp[0].tokenId)->pos, (yyvsp[0].tokenId)->id);
}
#line 1817 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 46:
#line 378 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.indexExpr) = A_NumIndexExpr((yyvsp[0].tokenNum)->pos, (yyvsp[0].tokenNum)->num);
}
#line 1825 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 47:
#line 385 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.memberExpr) = A_MemberExpr((yyvsp[-2].leftVal)->pos, (yyvsp[-2].leftVal), (yyvsp[0].tokenId)->id);
}
#line 1833 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 48:
#line 392 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDefArray) = A_VarDefArray((yyvsp[-9].tokenId)->pos, (yyvsp[-9].tokenId)->id, (yyvsp[-7].tokenNum)->num, (yyvsp[-4].type), (yyvsp[-1].rightValList));
}
#line 1841 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 49:
#line 396 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDefArray) = A_VarDefArray((yyvsp[-8].tokenId)->pos, (yyvsp[-8].tokenId)->id, (yyvsp[-6].tokenNum)->num, (yyvsp[-3].type), NULL);
}
#line 1849 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 50:
#line 400 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDefArray) = A_VarDefArray((yyvsp[-7].tokenId)->pos, (yyvsp[-7].tokenId)->id, (yyvsp[-5].tokenNum)->num, NULL, (yyvsp[-1].rightValList));
}
#line 1857 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 51:
#line 404 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDefArray) = A_VarDefArray((yyvsp[-6].tokenId)->pos, (yyvsp[-6].tokenId)->id, (yyvsp[-4].tokenNum)->num, NULL, NULL);
}
#line 1865 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 52:
#line 411 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.boolExpr) = A_BoolBiOp_Expr((yyvsp[-2].boolExpr)->pos, A_BoolBiOpExpr((yyvsp[-2].boolExpr)->pos, A_and, (yyvsp[-2].boolExpr), (yyvsp[0].boolExpr)));
}
#line 1873 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 53:
#line 415 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.boolExpr) = A_BoolBiOp_Expr((yyvsp[-2].boolExpr)->pos, A_BoolBiOpExpr((yyvsp[-2].boolExpr)->pos, A_or, (yyvsp[-2].boolExpr), (yyvsp[0].boolExpr)));
}
#line 1881 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 54:
#line 419 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.boolExpr) = A_BoolExpr((yyvsp[0].boolUnit)->pos, (yyvsp[0].boolUnit));
}
#line 1889 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 55:
#line 426 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.boolUnit) = A_ComExprUnit((yyvsp[-2].exprUnit)->pos, A_ComExpr((yyvsp[-2].exprUnit)->pos, A_lt, (yyvsp[-2].exprUnit), (yyvsp[0].exprUnit)));
}
#line 1897 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 56:
#line 430 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.boolUnit) = A_ComExprUnit((yyvsp[-2].exprUnit)->pos, A_ComExpr((yyvsp[-2].exprUnit)->pos, A_le, (yyvsp[-2].exprUnit), (yyvsp[0].exprUnit)));
}
#line 1905 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 57:
#line 434 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.boolUnit) = A_ComExprUnit((yyvsp[-2].exprUnit)->pos, A_ComExpr((yyvsp[-2].exprUnit)->pos, A_gt, (yyvsp[-2].exprUnit), (yyvsp[0].exprUnit)));
}
#line 1913 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 58:
#line 438 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.boolUnit) = A_ComExprUnit((yyvsp[-2].exprUnit)->pos, A_ComExpr((yyvsp[-2].exprUnit)->pos, A_ge, (yyvsp[-2].exprUnit), (yyvsp[0].exprUnit)));
}
#line 1921 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 59:
#line 442 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.boolUnit) = A_ComExprUnit((yyvsp[-2].exprUnit)->pos, A_ComExpr((yyvsp[-2].exprUnit)->pos, A_eq, (yyvsp[-2].exprUnit), (yyvsp[0].exprUnit)));
}
#line 1929 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 60:
#line 446 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.boolUnit) = A_ComExprUnit((yyvsp[-2].exprUnit)->pos, A_ComExpr((yyvsp[-2].exprUnit)->pos, A_ne, (yyvsp[-2].exprUnit), (yyvsp[0].exprUnit)));
}
#line 1937 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 61:
#line 450 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.boolUnit) = A_BoolExprUnit((yyvsp[-2].pos), (yyvsp[-1].boolExpr));
}
#line 1945 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 62:
#line 454 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.boolUnit) = A_BoolUOpExprUnit((yyvsp[-1].pos), A_BoolUOpExpr((yyvsp[-1].pos), A_not, (yyvsp[0].boolUnit)));
}
#line 1953 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 63:
#line 461 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.structDef) = A_StructDef((yyvsp[-4].pos), (yyvsp[-3].tokenId)->id, (yyvsp[-1].varDeclList));
}
#line 1961 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 64:
#line 468 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDeclList) = A_VarDeclList((yyvsp[-2].varDecl), (yyvsp[0].varDeclList));
}
#line 1969 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 65:
#line 472 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDeclList) = A_VarDeclList((yyvsp[0].varDecl), NULL);
}
#line 1977 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 66:
#line 476 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.varDeclList) = NULL;
}
#line 1985 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 67:
#line 483 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.fnDef) = A_FnDef((yyvsp[-3].fnDecl)->pos, (yyvsp[-3].fnDecl), (yyvsp[-1].codeBlockStmtList));
}
#line 1993 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 68:
#line 490 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.fnDeclStmt) = A_FnDeclStmt((yyvsp[-1].fnDecl)->pos, (yyvsp[-1].fnDecl));
}
#line 2001 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 69:
#line 497 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.fnDecl) = A_FnDecl((yyvsp[-4].pos), (yyvsp[-3].tokenId)->id, (yyvsp[-1].paramDecl), NULL);
}
#line 2009 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 70:
#line 501 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.fnDecl) = A_FnDecl((yyvsp[-6].pos), (yyvsp[-5].tokenId)->id, (yyvsp[-3].paramDecl), (yyvsp[0].type));
}
#line 2017 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 71:
#line 508 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.paramDecl) = A_ParamDecl((yyvsp[0].varDeclList));
}
#line 2025 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 72:
#line 515 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.codeBlockStmtList) = A_CodeBlockStmtList((yyvsp[-1].codeBlockStmt), (yyvsp[0].codeBlockStmtList));
}
#line 2033 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 73:
#line 519 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.codeBlockStmtList) = NULL;
}
#line 2041 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 74:
#line 526 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.codeBlockStmt) = A_BlockVarDeclStmt((yyvsp[0].varDeclStmt)->pos, (yyvsp[0].varDeclStmt));
}
#line 2049 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 75:
#line 530 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.codeBlockStmt) = A_BlockAssignStmt((yyvsp[0].assignStmt)->pos, (yyvsp[0].assignStmt));
}
#line 2057 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 76:
#line 534 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.codeBlockStmt) = A_BlockCallStmt((yyvsp[0].callStmt)->pos, (yyvsp[0].callStmt));
}
#line 2065 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 77:
#line 538 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.codeBlockStmt) = A_BlockIfStmt((yyvsp[0].ifStmt)->pos, (yyvsp[0].ifStmt));
}
#line 2073 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 78:
#line 542 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.codeBlockStmt) = A_BlockWhileStmt((yyvsp[0].whileStmt)->pos, (yyvsp[0].whileStmt));
}
#line 2081 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 79:
#line 546 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.codeBlockStmt) = A_BlockReturnStmt((yyvsp[0].returnStmt)->pos, (yyvsp[0].returnStmt));
}
#line 2089 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 80:
#line 550 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.codeBlockStmt) = A_BlockContinueStmt((yyvsp[-1].pos));
}
#line 2097 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 81:
#line 554 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.codeBlockStmt) = A_BlockBreakStmt((yyvsp[-1].pos));
}
#line 2105 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 82:
#line 558 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.codeBlockStmt) = A_BlockNullStmt((yyvsp[0].pos));
}
#line 2113 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 83:
#line 565 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.assignStmt) = A_AssignStmt((yyvsp[-3].leftVal)->pos, (yyvsp[-3].leftVal), (yyvsp[-1].rightVal));
}
#line 2121 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 84:
#line 572 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.leftVal) = A_IdExprLVal((yyvsp[0].tokenId)->pos, (yyvsp[0].tokenId)->id);
}
#line 2129 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 85:
#line 576 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.leftVal) = A_ArrExprLVal((yyvsp[0].arrayExpr)->pos, (yyvsp[0].arrayExpr));
}
#line 2137 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 86:
#line 580 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.leftVal) = A_MemberExprLVal((yyvsp[0].memberExpr)->pos, (yyvsp[0].memberExpr));
}
#line 2145 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 87:
#line 587 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.callStmt) = A_CallStmt((yyvsp[-1].fnCall)->pos, (yyvsp[-1].fnCall));
}
#line 2153 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 88:
#line 594 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.ifStmt) = A_IfStmt((yyvsp[-6].pos), (yyvsp[-4].boolExpr), (yyvsp[-1].codeBlockStmtList), NULL);
}
#line 2161 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 89:
#line 598 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.ifStmt) = A_IfStmt((yyvsp[-10].pos), (yyvsp[-8].boolExpr), (yyvsp[-5].codeBlockStmtList), (yyvsp[-1].codeBlockStmtList));
}
#line 2169 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 90:
#line 605 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.whileStmt) = A_WhileStmt((yyvsp[-6].pos), (yyvsp[-4].boolExpr), (yyvsp[-1].codeBlockStmtList));
}
#line 2177 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 91:
#line 612 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.returnStmt) = A_ReturnStmt((yyvsp[-2].pos), (yyvsp[-1].rightVal));
}
#line 2185 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 92:
#line 616 "parser.yacc" /* yacc.c:1646  */
    {
  (yyval.returnStmt) = A_ReturnStmt((yyvsp[-1].pos), NULL);
}
#line 2193 "y.tab.cpp" /* yacc.c:1646  */
    break;


#line 2197 "y.tab.cpp" /* yacc.c:1646  */
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
      yyerror (YY_("syntax error"));
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
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



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
                      yytoken, &yylval);
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


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


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
  yyerror (YY_("memory exhausted"));
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
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
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
#line 620 "parser.yacc" /* yacc.c:1906  */


extern "C"{
void yyerror(char * s)
{
  fprintf(stderr,"Error: %s in line %d, col %d\n", s, line, col);
}
int yywrap()
{
  return(1);
}
}

