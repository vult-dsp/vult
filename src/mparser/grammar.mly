%{
(* Vult DSP Language - Menhir Parser
 * Converted from tree-sitter grammar by Leonardo Laguna Ruiz
 *)

open Bast (* Builder functions for AST nodes *)

%}

(* Token declarations *)
%token <string> ID
%token <string> INT
%token <string> REAL FIXED XINT STRING
%token WILDCARD (* _ *)
%token COLON SEMICOLON COMMA
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token DOT
%token LAND LOR
%token ASSIGN
%token IF THEN ELSE
%token WHILE ITER MATCH ARROW
%token TYPE VAL CONSTANT FUN AND EXTERNAL MEM RETURN ENUM EOF TAG
%token <string> OP_LEVEL_0 OP_LEVEL_1 OP_LEVEL_2 OP_LEVEL_3
%token MINUS
%token TRUE FALSE

(* Start symbol *)
%start <Bast.state -> Bast.state * Pparser.Syntax.stmts> program

(* Precedence and associativity - lowest precedence first *)
%left LOR
%left LAND
%left OP_LEVEL_3
%left OP_LEVEL_2
%left OP_LEVEL_1 MINUS
%left OP_LEVEL_0
%right UMINUS
%left DOT LBRACKET
%left LPAREN
%right COLON

%on_error_reduce exp
%on_error_reduce exp_or_list
%on_error_reduce exp_record_member
%on_error_reduce option(typed)
%on_error_reduce option(exp_or_list)
%on_error_reduce option(tag)
%on_error_reduce option(STRING)
%on_error_reduce option(tag_call_args)
%on_error_reduce option(preceded(ASSIGN, exp_or_list))
%on_error_reduce nonempty_list(type_member)
%on_error_reduce separated_nonempty_list(COMMA,type_)
%on_error_reduce separated_nonempty_list(DOT, id)
%on_error_reduce separated_nonempty_list(COMMA,fun_arg)
%on_error_reduce separated_nonempty_list(COMMA,tag_call)
%%

program:
  | top_stmt* EOF { program_Program $loc $1 }
  ;

%inline id:
  | ID { $1 }

top_stmt:
  | top_fun    { $1 }
  | top_ext    { $1 }
  | top_constant { $1 }
  | top_type   { $1 }
  | top_enum   { $1 }
  ;

top_fun:
  | FUN fun_def fun_def_cont* { top_stmt_Fun $loc $2 $3 }
  ;

fun_def_cont:
  | AND fun_def { $2 }
  ;

fun_def:
  | id=id args=fun_args ty=option(typed) tag=option(tag) stmt=stmt
    { fun_def_FunDef $loc id args ty tag stmt }
  ;

fun_args:
  | LPAREN args=separated_list(COMMA, fun_arg) RPAREN { args }
  ;

fun_arg:
  | id=id ty=option(typed) { fun_arg_FunArg $loc id ty }
  ;

top_ext:
  | EXTERNAL id=id args=ext_args ty=typed s=option(STRING) tag=option(tag) SEMICOLON
    { top_stmt_External $loc id args ty s tag }
  ;

ext_args:
  | LPAREN args=separated_list(COMMA, ext_arg) RPAREN { args }
  ;

ext_arg:
  | id=id ty=typed { ext_arg_ExtArg $loc id ty }
  ;

top_constant:
  | CONSTANT id=id ASSIGN e=exp_or_list SEMICOLON { top_stmt_Constant $loc id e }
  ;

top_type:
  | TYPE id=id LBRACE members=type_member+ RBRACE { top_stmt_TypeDef $loc id members }
  | TYPE id=id SEMICOLON { top_stmt_TypeDefEmpty $loc id }
  ;

top_enum:
  | ENUM id=id LBRACE uids=separated_nonempty_list(COMMA, id) RBRACE
    { top_stmt_Enum $loc id uids }
  ;

type_member:
  | VAL id=id COLON ty=type_ SEMICOLON { type_member_TypeMember $loc id ty }
  ;

path:
  | id=separated_nonempty_list(DOT, id) { path_Path $loc id }
  ;

type_:
  | size=INT { type_expr_TypeSize $loc size }
  | composed=type_composed { composed }
  | id=type_id { id }
  ;

type_composed:
  | path=path LPAREN ty=separated_nonempty_list(COMMA, type_) RPAREN { type_expr_TypeComposed $loc path ty }
  ;

type_id:
  | p=path { type_expr_TypeId $loc p }
  ;

typed:
  | COLON ty=type_ { ty }
  ;

dexp:
  | WILDCARD { dexp_DexpWild $loc }
  | id=id { dexp_DexpId $loc id }
  | id=id LBRACKET size=INT RBRACKET { dexp_DexpArray $loc id size }
  | e=dexp COMMA es=dexp { dexp_DexpCons $loc e es }
  | LPAREN e=dexp RPAREN { dexp_DexpGroup $loc e }
  | e=dexp ty=typed { dexp_DexpTyped $loc e ty }
  ;

lexp:
  | WILDCARD { lexp_LexpWild $loc }
  | id=id { lexp_LexpId $loc id }
  | e=lexp COMMA es=lexp { lexp_LexpCons $loc e es }
  | LPAREN e=lexp RPAREN {lexp_LexpGroup  $loc e }
  | e=lexp LBRACKET idx=exp RBRACKET { lexp_LexpIndex $loc e idx }
  | e=lexp DOT id=id { lexp_LexpMember $loc e id }
  ;

exp:
  | TRUE { exp_ExpBool $loc true }
  | FALSE { exp_ExpBool $loc false }
  | i=INT { exp_ExpInt $loc i }
  | x=XINT { exp_ExpXInt $loc x }
  | r=REAL { exp_ExpReal $loc r }
  | f=FIXED { exp_ExpFixed $loc f }
  | s=STRING { exp_ExpString $loc s }
  | id=id { exp_ExpId $loc id }
  | LPAREN e=exp_or_list RPAREN { exp_ExpGroup $loc e }
  | LBRACKET e=exp_or_list RBRACKET { exp_ExpArray $loc e }
  | e=exp LPAREN arg=option(exp_or_list) RPAREN { exp_ExpCall $loc e arg }
  | e=exp LBRACE arg=separated_list(COMMA, exp_record_member) RBRACE { exp_ExpRecord $loc e arg }
  | e1=exp LOR e2=exp { exp_ExpBop $loc e1 "||" e2 }
  | e1=exp LAND e2=exp { exp_ExpBop $loc e1 "&&" e2 }
  | e1=exp op=OP_LEVEL_3 e2=exp { exp_ExpBop $loc e1 op e2 }
  | e1=exp op=OP_LEVEL_2 e2=exp { exp_ExpBop $loc e1 op e2 }
  | e1=exp op=OP_LEVEL_1 e2=exp { exp_ExpBop $loc e1 op e2 }
  | e1=exp op=OP_LEVEL_0 e2=exp { exp_ExpBop $loc e1 op e2 }
  | e1=exp MINUS e2=exp { exp_ExpBop $loc e1 "-" e2 }
  | e1=exp COLON e2=exp { exp_ExpWithName $loc e1 e2 }
  | e1=exp DOT id=id { exp_ExpMember $loc e1 id }
  | e=exp LBRACKET idx=exp RBRACKET { exp_ExpIndex $loc e idx }
  | MINUS e=exp %prec UMINUS { exp_ExpUop $loc "-" e }
  | IF e1=exp THEN e2=exp_or_list ELSE e3=exp_or_list { exp_ExpIf $loc e1 e2 e3 }
  ;

exp_or_list:
  | e=exp COMMA es=exp_or_list { exp_ExpCons $loc e es }
  | e=exp { e }
  ;

exp_record_member:
  | id=id ASSIGN e=exp { (id, e) }

tag_exp:
  | TRUE { tag_exp_TagExpBool $loc true }
  | FALSE { tag_exp_TagExpBool $loc false }
  | i=INT { tag_exp_TagExpInt $loc i }
  | x=XINT { tag_exp_TagExpXInt $loc x }
  | r=REAL { tag_exp_TagExpReal $loc r }
  | f=FIXED { tag_exp_TagExpFixed $loc f }
  | s=STRING { tag_exp_TagExpString $loc s }
  | c=tag_call { tag_exp_TagExpCall $loc c }
  | MINUS e=tag_exp { tag_exp_TagExpUop $loc "-" e }
  ;

tag_arg:
  | id=id ASSIGN e=tag_exp { tag_arg_TagArg $loc id e }
  ;

tag_call:
  | id=id args=option(tag_call_args) { tag_call_TagCall $loc id args }
  ;

tag_call_args:
  | LPAREN args=separated_list(COMMA, tag_arg) RPAREN { args }
  ;

tag:
  | TAG calls=separated_nonempty_list(COMMA, tag_call) RBRACKET { tag_Tag $loc calls }
  ;

stmt:
  | stmt_val    { $1 }
  | stmt_mem    { $1 }
  | stmt_bind   { $1 }
  | stmt_block  { $1 }
  | stmt_return { $1 }
  | stmt_if     { $1 }
  | stmt_while  { $1 }
  | stmt_iter   { $1 }
  | stmt_call   { $1 }
  | stmt_match  { $1 }
  ;

%inline stmt_block:
  | LBRACE stmts=stmt* RBRACE { stmt_StmtBlock $loc stmts }
  ;

%inline stmt_val:
  | VAL d=dexp e=option(preceded(ASSIGN, exp_or_list)) SEMICOLON { stmt_StmtVal $loc d e }
  ;

%inline stmt_mem:
  | MEM d=dexp e=option(preceded(ASSIGN, exp_or_list)) t=option(tag) SEMICOLON { stmt_StmtMem $loc d e t }
  ;

%inline stmt_bind:
  | l=lexp ASSIGN e=exp_or_list SEMICOLON { stmt_StmtBind $loc l e }
  ;

%inline stmt_return:
  | RETURN e=exp_or_list SEMICOLON { stmt_StmtReturn $loc e }
  ;

%inline stmt_if:
  | IF LPAREN e=exp RPAREN s1=stmt s2=option(preceded(ELSE, stmt))
    { stmt_StmtIf $loc e s1 s2 }
  ;

%inline stmt_while:
  | WHILE LPAREN e=exp RPAREN s=stmt { stmt_StmtWhile $loc e s }
  ;

%inline stmt_iter:
  | ITER LPAREN id=id COMMA e=exp RPAREN s=stmt { stmt_StmtIter $loc id e s }
  ;

%inline stmt_call:
  | p=exp LPAREN arg=option(exp_or_list) RPAREN SEMICOLON { stmt_StmtCall $loc p arg }
  ;

patt:
  | WILDCARD { patt_PattWild $loc }
  | i=INT { patt_PattInt $loc i }
  | x=XINT { patt_PattXInt $loc x }
  | r=REAL { patt_PattReal $loc r }
  | f=FIXED { patt_PattFixed $loc f }
  | s=STRING { patt_PattString $loc s }
  | p=path { patt_PattId $loc p }
  | p=patt COMMA ps=patt { patt_PattCons $loc p ps }
  | LPAREN p=patt RPAREN { patt_PattGroup $loc p }
  ;

stmt_match_case:
  | p=patt ARROW s=stmt { match_case_MatchCase $loc p s }
  ;

%inline stmt_match:
  | MATCH LPAREN e=exp_or_list RPAREN LBRACE cases=stmt_match_case* RBRACE
    { stmt_StmtMatch $loc e cases }
  ;

%%