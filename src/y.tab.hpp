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
    FUNCTION = 287,
    RETV = 288,
    CONTINUE = 289,
    BREAK = 290,
    RETURN = 291,
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
#define FUNCTION 287
#define RETV 288
#define CONTINUE 289
#define BREAK 290
#define RETURN 291
#define COL 292
#define ID 293
#define NUM 294

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 16 "parser.yacc" /* yacc.c:1909  */

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

#line 177 "y.tab.hpp" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_HPP_INCLUDED  */
