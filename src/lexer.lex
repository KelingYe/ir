%{
#include <stdio.h>
#include <string.h>
#include "TeaplAst.h"
#include "y.tab.hpp"
extern int line, col;
%}
%x COMMENT1
%x COMMENT2

%%
"let"                {  yylval.pos = A_Pos(line, col); col += yyleng; return LET;}
"int"                {  yylval.pos = A_Pos(line, col); col += yyleng; return INT;}
"struct"             {  yylval.pos = A_Pos(line, col); col += yyleng; return STRUCT;}
"fn"                 {  yylval.pos = A_Pos(line, col); col += yyleng; return FUNCTION;}
"ret"                {  yylval.pos = A_Pos(line, col); col += yyleng; return RETURN;}
"continue"           {  yylval.pos = A_Pos(line, col); col += yyleng; return CONTINUE;}
"break"              {  yylval.pos = A_Pos(line, col); col += yyleng; return BREAK;}
"if"                 {  yylval.pos = A_Pos(line, col); col += yyleng; return IF;}
"else"               {  yylval.pos = A_Pos(line, col); col += yyleng; return ELSE;}
"while"              {  yylval.pos = A_Pos(line, col); col += yyleng; return WHILE;}
"+"	                 {  yylval.pos = A_Pos(line, col); col += yyleng; return ADD;}
"-"	                 {  yylval.pos = A_Pos(line, col); col += yyleng; return SUB;}
"*"                  {  yylval.pos = A_Pos(line, col); col += yyleng; return MUL;}
"/"                  {  yylval.pos = A_Pos(line, col); col += yyleng; return DIV;}
"("                  {  yylval.pos = A_Pos(line, col); col += yyleng; return LPAR;}
")"                  {  yylval.pos = A_Pos(line, col); col += yyleng; return RPAR;}
([1-9][0-9]*)|0      {  yylval.tokenNum = A_TokenNum(A_Pos(line, col), atoi(yytext)); col += yyleng; return NUM;}
[a-zA-Z_]+([a-zA-Z0-9_]*) {  int len = yyleng;
                        char* new_text = (char*)malloc((len+1)*sizeof(char));strcpy(new_text, yytext);
                        new_text[len]='\0';
                        yylval.tokenId = A_TokenId(A_Pos(line, col), new_text);
                        col += yyleng;
                        return ID;}
";"   { yylval.pos = A_Pos(line, col); col += yyleng; return SEMICOLON;}
"."   { yylval.pos = A_Pos(line, col); col += yyleng; return DOT;}
"["   { yylval.pos = A_Pos(line, col); col += yyleng; return LSQU;}
"]"   { yylval.pos = A_Pos(line, col); col += yyleng; return RSQU;}
","   { yylval.pos = A_Pos(line, col); col += yyleng; return COMMA;}
"&&"  { yylval.pos = A_Pos(line, col); col += yyleng; return AND;}
"||"  { yylval.pos = A_Pos(line, col); col += yyleng; return OR;}
"!"   { yylval.pos = A_Pos(line, col); col += yyleng; return NOT;}
">"   { yylval.pos = A_Pos(line, col); col += yyleng; return GRE;}
"<"   { yylval.pos = A_Pos(line, col); col += yyleng; return LES;}
">="  { yylval.pos = A_Pos(line, col); col += yyleng; return EGRE;}
"<="  { yylval.pos = A_Pos(line, col); col += yyleng; return ELES;}
"=="  { yylval.pos = A_Pos(line, col); col += yyleng; return EQUAL;}
"!="  { yylval.pos = A_Pos(line, col); col += yyleng; return NEQUAL;}
"="   { yylval.pos = A_Pos(line, col); col += yyleng; return ASSIGN;}
":"   { yylval.pos = A_Pos(line, col); col += yyleng; return COL;}
"{"   { yylval.pos = A_Pos(line, col); col += yyleng; return LBRA;}
"}"   { yylval.pos = A_Pos(line, col); col += yyleng; return RBRA;}
"->"  { yylval.pos = A_Pos(line, col); col += yyleng; return RETV;}
<INITIAL>"//" {  BEGIN COMMENT1;  }
<COMMENT1>"\n" {  BEGIN INITIAL;  }
<COMMENT1>.   
<INITIAL>"/*" {BEGIN COMMENT2;}
<COMMENT2>"*/" {BEGIN INITIAL;}
<COMMENT2>\n   
<COMMENT2>.
\n      { yylval.pos = A_Pos(line, col); col = 1; line += 1; }
" "      { yylval.pos = A_Pos(line, col); col += yyleng; }
%%
