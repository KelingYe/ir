%{
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

%}

%union {
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
}

%token <pos> ADD
%token <pos> SUB
%token <pos> MUL
%token <pos> DIV
%token <pos> SEMICOLON
%token <pos> LPAR
%token <pos> RPAR
%token <pos> DOT
%token <pos> COMMA
%token <pos> AND
%token <pos> GRE
%token <pos> STRUCT
%token <pos> LSQU
%token <pos> RSQU
%token <pos> LET
%token <pos> ASSIGN
%token <pos> INT
%token <pos> LBRA
%token <pos> RBRA
%token <pos> OR
%token <pos> NOT
%token <pos> LES
%token <pos> EGRE
%token <pos> ELES
%token <pos> EQUAL
%token <pos> NEQUAL
%token <pos> IF
%token <pos> ELSE
%token <pos> WHILE
%token <pos> FUNC
%token <pos> RETV
%token <pos> CONTINUE
%token <pos> BREAK
%token <pos> RET
%token <pos> COL
%token <tokenId> ID
%token <tokenNum> NUM

%type <program> Program
%type <arithExpr> ArithExpr
%type <programElementList> ProgramElementList
%type <programElement> ProgramElement
%type <exprUnit> ExprUnit
%type <structDef> StructDef//
%type <varDeclStmt> VarDeclStmt
%type <fnDeclStmt> FnDeclStmt//
%type <fnDef> FnDef//
%type <fnCall> FnCall
%type <arrayExpr> ArrayExpr
%type <arithUExpr> ArithUExpr
%type <memberExpr> MemberExpr
%type <varDecl> VarDecl
%type <varDef> VarDef
%type <varDeclScalar> VarDeclScalar 
%type <varDeclArray>  VarDeclArray 
%type <varDefScalar> VarDefScalar 
%type <varDefArray>   VarDefArray
%type <type>   Type
%type <rightVal> RightVal
%type <rightValList> RightValList
%type <indexExpr> IndexExpr
%type <boolExpr> BoolExpr
%type <boolUnit> BoolUnit
%type <varDeclList> VarDeclList//
%type <fnDecl> FnDecl//
%type <paramDecl> ParamDecl //
%type <codeBlockStmtList> CodeBlockStmtList//
%type <codeBlockStmt> CodeBlockStmt//
%type <assignStmt> AssignStmt//
%type <callStmt> CallStmt//
%type <ifStmt> IfStmt//
%type <whileStmt> WhileStmt//
%type <returnStmt> ReturnStmt//
%type <leftVal> LeftVal //
%type <arithBiOpExpr> ArithBiOpExpr

%left ASSIGN
%left OR
%left AND
%left GRE LES
%left ADD SUB
%left MUL DIV
%right NOT
%start Program
%%                   /* beginning of rules section */

Program
: ProgramElementList 
{  
  root = A_Program($1);
  $$ = A_Program($1);
}
;

ProgramElementList
: ProgramElement ProgramElementList
{
  $$ = A_ProgramElementList($1, $2);
}
|
{
  $$ = NULL;
}
;

ProgramElement
: VarDeclStmt
{
  $$ = A_ProgramVarDeclStmt($1->pos, $1);
}
| StructDef
{
  $$ = A_ProgramStructDef($1->pos, $1);
}
| FnDeclStmt
{
  $$ = A_ProgramFnDeclStmt($1->pos, $1);
}
| FnDef
{
  $$ = A_ProgramFnDef($1->pos, $1);
}
| SEMICOLON
{
  $$ = A_ProgramNullStmt($1);
}
;

VarDeclStmt
: LET VarDecl SEMICOLON 
{
  $$ = A_VarDeclStmt($1, $2);
}
| LET VarDef SEMICOLON 
{
  $$ = A_VarDefStmt($1, $2);
}

VarDecl: VarDeclScalar 
{
  $$ = A_VarDecl_Scalar($1->pos, $1);
}
| VarDeclArray 
{
  $$ = A_VarDecl_Array($1->pos, $1);
}
;

VarDeclArray
: ID LSQU NUM RSQU COL Type
{
  $$ = A_VarDeclArray($1->pos, $1->id, $3->num, $6);
}
| ID LSQU NUM RSQU
{
  $$ = A_VarDeclArray($1->pos, $1->id, $3->num, NULL);
}
; 

VarDeclScalar
: ID COL Type
{
  $$ = A_VarDeclScalar($1->pos, $1->id, $3);
}
| ID
{
  $$ = A_VarDeclScalar($1->pos, $1->id, NULL);
}
;

Type
: INT
{
  $$ = A_NativeType($1, A_intTypeKind);
}
| ID
{
  $$ = A_StructType($1->pos, $1->id);
}
;

VarDef
: VarDefScalar
{
  $$=A_VarDef_Scalar($1->pos, $1);
}
|   VarDefArray
{   
  $$=A_VarDef_Array($1->pos, $1);
}
;

VarDefScalar
: ID  COL Type ASSIGN RightVal
{
  $$=A_VarDefScalar($1->pos, $1->id, $3, $5);
}
|  ID ASSIGN RightVal
{
  $$=A_VarDefScalar($1->pos, $1->id, NULL, $3);
}
;

RightVal
: ArithExpr
{
  $$=A_ArithExprRVal($1->pos, $1);
}
| BoolExpr 
{
  $$=A_BoolExprRVal($1->pos, $1);
}
;

ArithExpr
: ArithBiOpExpr
{
  $$ = A_ArithBiOp_Expr($1->pos, $1);
}
| ExprUnit
{
  $$ = A_ExprUnit($1->pos, $1);
}
;

ArithUExpr
: SUB ExprUnit
{
  $$ = A_ArithUExpr($1, A_neg, $2);
}
;

ArithBiOpExpr
: ArithExpr ADD ArithExpr
{
  $$ = A_ArithBiOpExpr($1->pos, A_add, $1, $3);
}
| ArithExpr SUB ArithExpr
{
  $$ = A_ArithBiOpExpr($1->pos, A_sub, $1, $3);
}
| ArithExpr MUL ArithExpr
{
  $$ = A_ArithBiOpExpr($1->pos, A_mul, $1, $3);
}
| ArithExpr DIV ArithExpr
{
  $$ = A_ArithBiOpExpr($1->pos, A_div, $1, $3);
}
;

ExprUnit
: NUM 
{
  $$ = A_NumExprUnit($1->pos, $1->num);
}
| ID
{
  $$ = A_IdExprUnit($1->pos, $1->id);
}
| LPAR ArithExpr RPAR
{
  $$ = A_ArithExprUnit($1, $2);
}
| FnCall
{
  $$ = A_CallExprUnit($1->pos, $1);
}
| ArrayExpr
{
  $$ = A_ArrayExprUnit($1->pos, $1);
}
| MemberExpr
{
  $$ = A_MemberExprUnit($1->pos, $1);
}
| ArithUExpr 
{
  $$ = A_ArithUExprUnit($1->pos, $1);
}
;

FnCall
: ID LPAR RightValList RPAR
{
  $$ = A_FnCall($1->pos, $1->id, $3);
}
| ID LPAR RPAR
{
  $$ = A_FnCall($1->pos, $1->id, NULL);
}
;

RightValList
: RightVal COMMA RightValList
{
  $$ = A_RightValList($1, $3);
}
| RightVal 
{
  $$ = A_RightValList($1, NULL);
}
;

ArrayExpr
: ID LSQU IndexExpr RSQU
{
  $$ = A_ArrayExpr($1->pos, $1->id, $3);
}
;

IndexExpr
: ID
{
  $$ = A_IdIndexExpr($1->pos, $1->id);
}
| NUM
{
  $$ = A_NumIndexExpr($1->pos, $1->num);
}
;

MemberExpr
: ID DOT ID
{
  $$ = A_MemberExpr($1->pos, $1->id, $3->id);
}
;

VarDefArray
: ID LSQU NUM RSQU COL Type ASSIGN LBRA RightValList RBRA
{
  $$ = A_VarDefArray($1->pos, $1->id, $3->num, $6, $9);
}
| ID LSQU NUM RSQU COL Type ASSIGN LBRA RBRA
{
  $$ = A_VarDefArray($1->pos, $1->id, $3->num, $6, NULL);
}
| ID LSQU NUM RSQU ASSIGN LBRA RightValList RBRA
{
  $$ = A_VarDefArray($1->pos, $1->id, $3->num, NULL, $7);
}
| ID LSQU NUM RSQU ASSIGN LBRA RBRA
{
  $$ = A_VarDefArray($1->pos, $1->id, $3->num, NULL, NULL);
}
;

BoolExpr
: BoolExpr AND BoolExpr
{
  $$ = A_BoolBiOp_Expr($1->pos, A_BoolBiOpExpr($1->pos, A_and, $1, $3));
}
| BoolExpr OR BoolExpr
{
  $$ = A_BoolBiOp_Expr($1->pos, A_BoolBiOpExpr($1->pos, A_or, $1, $3));
}
| BoolUnit
{
  $$ = A_BoolExpr($1->pos, $1);
}
;

BoolUnit
: ExprUnit LES ExprUnit
{
  $$ = A_ComExprUnit($1->pos, A_ComExpr($1->pos, A_lt, $1, $3));
}
| ExprUnit ELES ExprUnit
{
  $$ = A_ComExprUnit($1->pos, A_ComExpr($1->pos, A_le, $1, $3));
}
| ExprUnit GRE ExprUnit
{
  $$ = A_ComExprUnit($1->pos, A_ComExpr($1->pos, A_gt, $1, $3));
}
| ExprUnit EGRE ExprUnit
{
  $$ = A_ComExprUnit($1->pos, A_ComExpr($1->pos, A_ge, $1, $3));
}
| ExprUnit EQUAL ExprUnit
{
  $$ = A_ComExprUnit($1->pos, A_ComExpr($1->pos, A_eq, $1, $3));
}
| ExprUnit NEQUAL ExprUnit
{
  $$ = A_ComExprUnit($1->pos, A_ComExpr($1->pos, A_ne, $1, $3));
}
| LPAR BoolExpr RPAR
{
  $$ = A_BoolExprUnit($1, $2);
}
| NOT BoolUnit
{
  $$ = A_BoolUOpExprUnit($1, A_BoolUOpExpr($1, A_not, $2));
}
;

StructDef
: STRUCT ID LBRA VarDeclList RBRA
{
  $$ = A_StructDef($1, $2->id, $4);
} 
;

VarDeclList
: VarDecl COMMA VarDeclList
{
  $$ = A_VarDeclList($1, $3);
}
| VarDecl
{
  $$ = A_VarDeclList($1, NULL);
}
|
{
  $$ = NULL;
}
;

FnDef
: FnDecl LBRA CodeBlockStmtList RBRA
{
  $$ = A_FnDef($1->pos, $1, $3);
}
;

FnDeclStmt
: FnDecl SEMICOLON
{
  $$ = A_FnDeclStmt($1->pos, $1);
}
;

FnDecl
: FUNC ID LPAR ParamDecl RPAR
{
  $$ = A_FnDecl($1, $2->id, $4, NULL);
}
| FUNC ID LPAR ParamDecl RPAR RETV Type
{
  $$ = A_FnDecl($1, $2->id, $4, $7);
}
;

ParamDecl
: VarDeclList
{
  $$ = A_ParamDecl($1);
}
;

CodeBlockStmtList
: CodeBlockStmt CodeBlockStmtList
{
  $$ = A_CodeBlockStmtList($1, $2);
}
| 
{
  $$ = NULL;
}
;

CodeBlockStmt
: VarDeclStmt
{
  $$ = A_BlockVarDeclStmt($1->pos, $1);
}
| AssignStmt
{
  $$ = A_BlockAssignStmt($1->pos, $1);
}
| CallStmt
{
  $$ = A_BlockCallStmt($1->pos, $1);
}
| IfStmt
{
  $$ = A_BlockIfStmt($1->pos, $1);
}
| WhileStmt
{
  $$ = A_BlockWhileStmt($1->pos, $1);
}
| ReturnStmt
{
  $$ = A_BlockReturnStmt($1->pos, $1);
}
| CONTINUE SEMICOLON
{
  $$ = A_BlockContinueStmt($1);
}
| BREAK SEMICOLON
{
  $$ = A_BlockBreakStmt($1);
}
| SEMICOLON
{
  $$ = A_BlockNullStmt($1);
}
;

AssignStmt
: LeftVal ASSIGN RightVal SEMICOLON
{
  $$ = A_AssignStmt($1->pos, $1, $3);
}
;

LeftVal
: ID
{
  $$ = A_IdExprLVal($1->pos, $1->id);
}
| ArrayExpr
{
  $$ = A_ArrExprLVal($1->pos, $1);
}
| MemberExpr
{
  $$ = A_MemberExprLVal($1->pos, $1);
}
;

CallStmt
: FnCall SEMICOLON
{
  $$ = A_CallStmt($1->pos, $1);
}
;

IfStmt
: IF LPAR BoolExpr RPAR LBRA CodeBlockStmtList RBRA
{
  $$ = A_IfStmt($1, $3, $6, NULL);
}
| IF LPAR BoolExpr RPAR LBRA CodeBlockStmtList RBRA ELSE LBRA CodeBlockStmtList RBRA
{
  $$ = A_IfStmt($1, $3, $6, $10);
}
;

WhileStmt
: WHILE LPAR BoolExpr RPAR LBRA CodeBlockStmtList RBRA
{
  $$ = A_WhileStmt($1, $3, $6);
}
;

ReturnStmt
: RET RightVal SEMICOLON
{
  $$ = A_ReturnStmt($1, $2);
}
;

%%

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

