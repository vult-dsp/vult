%{
(* START HEADER *)

type ('a,'sep) seq = 'a list
type 'a nseq = 'a * 'a list
type ('a,'sep) nsepseq = 'a * ('a,'sep) seq
type ('a,'sep) sepseq = ('a,'sep) nsepseq option

(* END HEADER *)
%}

(* TOKENS *)

(* Keywords *)

%token FUN AND EXTERNAL RETURN IF THEN ELSE WHILE MEM VAL TYPE
%token TRUE FALSE

(* Symbols *)

%token LPAR RPAR LBRACE RBRACE LBRACKET RBRACKET LSEQ RSEQ
%token ATTR COLON SEMI COMMA EQUAL WILD MINUS DOT

(* Operators *)
%token PLUS

(* Literals *)

%token <string> Ident
%token <string> String
%token <string> Tick
%token <string> Int
%token <string> Float

(* ENTRY *)

%start prog
%type <unit> prog

%%

(* RULES *)

(* Auxiliary parametric rules *)

opt(X): { None } | x=X { Some x }

nseq(X): h=X t=seq(X) { h,t }

seq(X): { [] } | l=nseq(X) { let h,t = l in h::t }

sepseq(X,Sep): s=opt(nsepseq(X,Sep)) { s }

nsepseq(X,Sep):
  x=X { x,[] }
| x=X s=Sep r=nsepseq(X,Sep)
    { let h,t = r in x, (s,h)::t }

nsepseq1(X,Sep):
| x=X s=Sep r=nsepseq(X,Sep)
    { let h,t = r in x, (s,h)::t }

(* Entry point *)

prog:
  seq(definition)
  {}

definition:
  function_definition
  {}
| type_definition
  {}
| external_definition
  {}

(* Functions *)

function_definition:
  FUN nsepseq(function_body,AND)
  {}

function_body:
  Ident LPAR sepseq(function_parameter,COMMA) RPAR
  opt(type_annotation) opt(attribute) statement
  {}

type_annotation:
  COLON type_
  {}

function_parameter:
  Ident opt(type_annotation)
  {}

attribute:
  ATTR Ident RBRACKET
  {}

(* Type definitions *)

type_definition:
  TYPE Ident
  {}


(* Types *)

type_:
  Tick
  {}
| WILD
  {}
| Ident opt(LPAR nsepseq(type_,COMMA) RPAR {})
  {}
| LPAR nsepseq(type_,COMMA) RPAR
  {}
| Int
  {}

(* Externals *)

external_definition:
  EXTERNAL Ident LPAR typed_parameter_list RPAR
  type_annotation String SEMI
  {}

typed_parameter_list:
  sepseq(typed_parameter,COMMA)
  {}

typed_parameter:
  Ident type_annotation
  {}

(* Statements *)

statement:
  compound_stmt
  {}
| return_stmt
  {}
| if_stmt
  {}
| while_stmt
  {}
| assignment
  {}
| declaration
  {}

compound_stmt:
  LBRACE sepseq(statement,SEMI) RBRACE
  {}

return_stmt:
  RETURN expression SEMI
  {}

if_stmt:
  IF LPAR expression RPAR statement opt(ELSE statement {})
  {}

while_stmt:
  WHILE LPAR expression RPAR statement
  {}

assignment:
  lvalue EQUAL expression SEMI
  {}

(* Expressions *)

lvalue:
  WILD
  {}
| Ident opt(type_annotation)
  {}
| LPAR nsepseq(lvalue,COMMA) RPAR
  {}

declaration:
  MEM assignment
  {}
| VAL assignment
  {}

expression:
  MINUS expression
  {}
| expression bin_op expression
  {}
| function_call
  {}
| tuple
  {}
| if_expression
  {}
| literal
  {}
| LPAR expression RPAR
  {}
| sequence
  {}
| array_expression
  {}
| Ident DOT Ident
  {}

function_call:
  opt(Ident COLON {}) name LPAR sepseq(expression,COMMA) RPAR
  {}

array_expression:
  LBRACKET sepseq(expression,COMMA) RBRACKET
  {}

name:
  nsepseq(Ident,DOT)
  {}

tuple:
  nsepseq1(expression,COMMA)
  {}

if_expression:
  IF expression THEN expression ELSE expression
  {}

literal:
  Int
  {}
| Float
  {}
| TRUE
  {}
| FALSE
  {}

sequence:
  LSEQ sepseq(statement,SEMI) RSEQ
  {}

(* Operators *)
bin_op:
   PLUS {}
