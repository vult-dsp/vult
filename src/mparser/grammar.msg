program: XINT
##
## Ends in an error in state: 0.
##
## program' -> . program [ # ]
##
## The known suffix of the stack is as follows:
##
##

Unexpected token at the beginning of the file. A Vult program should start with a top-level declaration like 'type', 'fun', 'enum', 'external', or 'constant'.

program: TYPE XINT
##
## Ends in an error in state: 1.
##
## top_type -> TYPE . ID LBRACE nonempty_list(type_member) RBRACE [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
## top_type -> TYPE . ID SEMICOLON [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## TYPE
##

Invalid type declaration. Expected an identifier after 'type', but found '%token'. Type declarations should be in the form 'type name { ... }' or 'type name;'.

program: TYPE ID XINT
##
## Ends in an error in state: 2.
##
## top_type -> TYPE ID . LBRACE nonempty_list(type_member) RBRACE [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
## top_type -> TYPE ID . SEMICOLON [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## TYPE ID
##

Incomplete type declaration. After the type name, expected either '{' (for a full type definition) or ';' (for an abstract type), but found '%token'.

program: TYPE ID LBRACE XINT
##
## Ends in an error in state: 4.
##
## top_type -> TYPE ID LBRACE . nonempty_list(type_member) RBRACE [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## TYPE ID LBRACE
##

Invalid type member declaration. Expected 'val' to begin a type member definition, but found '%token'. Type members should be in the form 'val name : type;'.

program: TYPE ID LBRACE VAL XINT
##
## Ends in an error in state: 5.
##
## type_member -> VAL . ID COLON type_ SEMICOLON [ VAL RBRACE ]
##
## The known suffix of the stack is as follows:
## VAL
##

Invalid type member declaration. Expected an identifier after 'val', but found '%token'. Type members should be in the form 'val name : type;'.

program: TYPE ID LBRACE VAL ID XINT
##
## Ends in an error in state: 6.
##
## type_member -> VAL ID . COLON type_ SEMICOLON [ VAL RBRACE ]
##
## The known suffix of the stack is as follows:
## VAL ID
##

Missing colon in type member declaration. Expected ':' after the member name, but found '%token'. Type members should be in the form 'val name : type;'.

program: TYPE ID LBRACE VAL ID COLON XINT
##
## Ends in an error in state: 7.
##
## type_member -> VAL ID COLON . type_ SEMICOLON [ VAL RBRACE ]
##
## The known suffix of the stack is as follows:
## VAL ID COLON
##

Invalid type specification in type member. '%token' cannot be used as type. Expected a type name (like 'int', 'real', etc.) or a user-defined type.

program: EXTERNAL ID LPAREN RPAREN COLON ID LPAREN XINT
##
## Ends in an error in state: 18.
##
## type_composed -> path LPAREN . separated_nonempty_list(COMMA,type_) RPAREN [ WILDCARD WHILE VAL TAG STRING SEMICOLON RPAREN RETURN MEM MATCH LPAREN LBRACE ITER IF ID COMMA COLON ASSIGN ]
##
## The known suffix of the stack is as follows:
## path LPAREN
##

Invalid type parameter in parameterized type. Expected a valid type name, but found '%token'. Parameterized types should be in the form 'typename(type1, type2, ...)'.

program: EXTERNAL ID LPAREN RPAREN COLON ID LPAREN ID COMMA XINT
##
## Ends in an error in state: 20.
##
## separated_nonempty_list(COMMA,type_) -> type_ COMMA . separated_nonempty_list(COMMA,type_) [ RPAREN ]
##
## The known suffix of the stack is as follows:
## type_ COMMA
##

Invalid type parameter list. Expected a valid type name after comma, but found '%token'. Type parameters should be valid type names separated by commas.

program: EXTERNAL ID LPAREN RPAREN COLON ID DOT XINT
##
## Ends in an error in state: 10.
##
## separated_nonempty_list(DOT,id) -> ID DOT . separated_nonempty_list(DOT,id) [ WILDCARD WHILE VAL TAG STRING SEMICOLON RPAREN RETURN MEM MATCH LPAREN LBRACE ITER IF ID COMMA COLON ASSIGN ARROW ]
##
## The known suffix of the stack is as follows:
## ID DOT
##

Invalid path. Expected an identifier after the dot, but found '%token'. Paths should consist of identifiers separated by dots, like 'module.function'.

program: EXTERNAL ID LPAREN RPAREN COLON ID LPAREN INT TYPE
##
## Ends in an error in state: 22.
##
## type_composed -> path LPAREN separated_nonempty_list(COMMA,type_) . RPAREN [ WILDCARD WHILE VAL TAG STRING SEMICOLON RPAREN RETURN MEM MATCH LPAREN LBRACE ITER IF ID COMMA COLON ASSIGN ]
##
## The known suffix of the stack is as follows:
## path LPAREN separated_nonempty_list(COMMA,type_)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 19, spurious reduction of production separated_nonempty_list(COMMA,type_) -> type_
##

Missing closing parenthesis in parameterized type. Expected ')' to close the type parameter list, but found '%token'. Parameterized types should be in the form 'typename(param1, param2, ...)'.

program: TYPE ID LBRACE VAL ID COLON ID SEMICOLON XINT
##
## Ends in an error in state: 26.
##
## top_type -> TYPE ID LBRACE nonempty_list(type_member) . RBRACE [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## TYPE ID LBRACE nonempty_list(type_member)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 24, spurious reduction of production nonempty_list(type_member) -> type_member
##

Missing closing brace in type definition. Expected '}' to close the type body, but found '%token'. Type definitions should have matching opening and closing braces.

program: FUN XINT
##
## Ends in an error in state: 28.
##
## top_fun -> FUN . fun_def list(fun_def_cont) [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## FUN
##

Invalid function declaration. Expected an identifier for the function name, but found '%token'. Function declarations should start with 'fun name(parameters) ...'.

program: FUN ID XINT
##
## Ends in an error in state: 29.
##
## fun_def -> ID . fun_args option(typed) option(tag) stmt [ TYPE FUN EXTERNAL EOF ENUM CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## ID
##

Missing function parameter list. Expected '(' to begin the parameter list, but found '%token'. Function declarations require a parameter list in the form 'fun name(param1, param2, ...)'.

program: FUN ID LPAREN XINT
##
## Ends in an error in state: 30.
##
## fun_args -> LPAREN . loption(separated_nonempty_list(COMMA,fun_arg)) RPAREN [ WILDCARD WHILE VAL TAG RETURN MEM MATCH LPAREN LBRACE ITER IF ID COLON ]
##
## The known suffix of the stack is as follows:
## LPAREN
##

Invalid function parameter. Expected a parameter name (identifier) or ')' for empty parameter list, but found '%token'. Function parameters should be identifiers, optionally followed by a type annotation.

program: FUN ID LPAREN ID TYPE
##
## Ends in an error in state: 37.
##
## fun_args -> LPAREN loption(separated_nonempty_list(COMMA,fun_arg)) . RPAREN [ WILDCARD WHILE VAL TAG RETURN MEM MATCH LPAREN LBRACE ITER IF ID COLON ]
##
## The known suffix of the stack is as follows:
## LPAREN loption(separated_nonempty_list(COMMA,fun_arg))
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 31, spurious reduction of production option(typed) ->
## In state 35, spurious reduction of production fun_arg -> ID option(typed)
## In state 39, spurious reduction of production separated_nonempty_list(COMMA,fun_arg) -> fun_arg
## In state 36, spurious reduction of production loption(separated_nonempty_list(COMMA,fun_arg)) -> separated_nonempty_list(COMMA,fun_arg)
##

Missing closing parenthesis in function parameter list. Expected ')' to close the parameter list, but found '%token'. Function declarations should have matching opening and closing parentheses around parameters.

program: EXTERNAL ID LPAREN RPAREN COLON XINT
##
## Ends in an error in state: 32.
##
## typed -> COLON . type_ [ WILDCARD WHILE VAL TAG STRING SEMICOLON RPAREN RETURN MEM MATCH LPAREN LBRACE ITER IF ID COMMA COLON ASSIGN ]
##
## The known suffix of the stack is as follows:
## COLON
##

Invalid type specification. Expected a valid type name after ':', but found '%token'. Types should be valid type names.

program: FUN ID LPAREN ID COMMA XINT
##
## Ends in an error in state: 40.
##
## separated_nonempty_list(COMMA,fun_arg) -> fun_arg COMMA . separated_nonempty_list(COMMA,fun_arg) [ RPAREN ]
##
## The known suffix of the stack is as follows:
## fun_arg COMMA
##

Invalid function parameter in parameter list. Expected a parameter name (identifier) after comma, but found '%token'. Function parameters should be comma-separated identifiers.

program: FUN ID LPAREN RPAREN TAG XINT
##
## Ends in an error in state: 44.
##
## tag -> TAG . separated_nonempty_list(COMMA,tag_call) RBRACKET [ WILDCARD WHILE VAL SEMICOLON RETURN MEM MATCH LPAREN LBRACE ITER IF ID ]
##
## The known suffix of the stack is as follows:
## TAG
##

Invalid tag in function declaration. Expected an identifier for the tag name, but found '%token'. Tags should be in the form '@[tagname(arg1, arg2, ...)]'.

program: FUN ID LPAREN RPAREN TAG ID COMMA XINT
##
## Ends in an error in state: 69.
##
## separated_nonempty_list(COMMA,tag_call) -> tag_call COMMA . separated_nonempty_list(COMMA,tag_call) [ RBRACKET ]
##
## The known suffix of the stack is as follows:
## tag_call COMMA
##

Invalid tag in comma-separated tag list. Expected an identifier for the next tag name, but found '%token'. Multiple tags should be comma-separated identifiers within '@[...]'.

program: FUN ID LPAREN RPAREN TAG ID LPAREN XINT
##
## Ends in an error in state: 46.
##
## tag_call_args -> LPAREN . loption(separated_nonempty_list(COMMA,tag_arg)) RPAREN [ RPAREN RBRACKET COMMA ]
##
## The known suffix of the stack is as follows:
## LPAREN
##

Invalid tag argument. Expected a parameter name (identifier) or ')' for empty argument list, but found '%token'. Tag arguments should be in the form 'name=value'.

program: FUN ID LPAREN RPAREN TAG ID LPAREN ID ASSIGN ID XINT
##
## Ends in an error in state: 60.
##
## separated_nonempty_list(COMMA,tag_arg) -> tag_arg . [ RPAREN ]
## separated_nonempty_list(COMMA,tag_arg) -> tag_arg . COMMA separated_nonempty_list(COMMA,tag_arg) [ RPAREN ]
##
## The known suffix of the stack is as follows:
## tag_arg
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 45, spurious reduction of production option(tag_call_args) ->
## In state 67, spurious reduction of production tag_call -> ID option(tag_call_args)
## In state 58, spurious reduction of production tag_exp -> tag_call
## In state 59, spurious reduction of production tag_arg -> ID ASSIGN tag_exp
##

Unexpected token in tag argument list. Expected ',' for another argument or ')' to close the argument list, but found '%token'. Tag argument lists should end with a closing parenthesis.

program: FUN ID LPAREN RPAREN TAG ID LPAREN ID ASSIGN ID COMMA XINT
##
## Ends in an error in state: 61.
##
## separated_nonempty_list(COMMA,tag_arg) -> tag_arg COMMA . separated_nonempty_list(COMMA,tag_arg) [ RPAREN ]
##
## The known suffix of the stack is as follows:
## tag_arg COMMA
##

Invalid tag argument in comma-separated list. Expected another tag argument (in the form 'name=value'), but found '%token'. Multiple tag arguments should be separated by commas.

program: FUN ID LPAREN RPAREN TAG ID LPAREN ID XINT
##
## Ends in an error in state: 47.
##
## tag_arg -> ID . ASSIGN tag_exp [ RPAREN COMMA ]
##
## The known suffix of the stack is as follows:
## ID
##

Missing assignment in tag argument. Expected '=' after parameter name, but found '%token'. Tag arguments should be in the form 'name=value'.

program: FUN ID LPAREN RPAREN TAG ID LPAREN ID ASSIGN WILDCARD
##
## Ends in an error in state: 48.
##
## tag_arg -> ID ASSIGN . tag_exp [ RPAREN COMMA ]
##
## The known suffix of the stack is as follows:
## ID ASSIGN
##

Invalid value in tag argument. '%token' cannot be used as a value in tag arguments. Use a valid literal or identifier instead.

program: FUN ID LPAREN RPAREN TAG ID LPAREN ID ASSIGN MINUS WILDCARD
##
## Ends in an error in state: 53.
##
## tag_exp -> MINUS . tag_exp [ RPAREN COMMA ]
##
## The known suffix of the stack is as follows:
## MINUS
##

Invalid negated expression in tag argument. '%token' cannot be used in a negated expression. Use a valid literal or identifier that can be negated.

program: FUN ID LPAREN RPAREN TAG ID XINT
##
## Ends in an error in state: 71.
##
## tag -> TAG separated_nonempty_list(COMMA,tag_call) . RBRACKET [ WILDCARD WHILE VAL SEMICOLON RETURN MEM MATCH LPAREN LBRACE ITER IF ID ]
##
## The known suffix of the stack is as follows:
## TAG separated_nonempty_list(COMMA,tag_call)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 45, spurious reduction of production option(tag_call_args) ->
## In state 67, spurious reduction of production tag_call -> ID option(tag_call_args)
## In state 68, spurious reduction of production separated_nonempty_list(COMMA,tag_call) -> tag_call
##

Missing closing bracket in tag declaration. Expected ']' to close the tag, but found '%token'. Tags should be enclosed in '@[...]'.

program: FUN ID LPAREN RPAREN TYPE
##
## Ends in an error in state: 74.
##
## fun_def -> ID fun_args option(typed) option(tag) . stmt [ TYPE FUN EXTERNAL EOF ENUM CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## ID fun_args option(typed) option(tag)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 42, spurious reduction of production option(typed) ->
## In state 43, spurious reduction of production option(tag) ->
##

Missing function body. Expected a statement or block to define the function body, but found '%token'. Function bodies should be blocks enclosed in curly braces or single statements.

program: FUN ID LPAREN RPAREN WHILE XINT
##
## Ends in an error in state: 76.
##
## stmt -> WHILE . LPAREN exp RPAREN stmt [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## WHILE
##

Invalid while loop. Expected '(' after 'while' to begin the condition, but found '%token'. While loops should be in the form 'while (condition) statement'.

program: FUN ID LPAREN RPAREN WHILE LPAREN WILDCARD
##
## Ends in an error in state: 77.
##
## stmt -> WHILE LPAREN . exp RPAREN stmt [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## WHILE LPAREN
##

Invalid while condition. '%token' cannot be used as a condition expression. While loop conditions should be valid expressions that evaluate to a boolean value.

program: CONSTANT ID ASSIGN MINUS WILDCARD
##
## Ends in an error in state: 82.
##
## exp -> MINUS . exp [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## MINUS
##

Invalid negated expression. '%token' cannot be used in a negated expression. Use a valid expression that can be negated.

program: CONSTANT ID ASSIGN LPAREN WILDCARD
##
## Ends in an error in state: 83.
##
## exp -> LPAREN . exp_or_list RPAREN [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## LPAREN
##

Invalid grouped expression. '%token' cannot be used as an expression in parentheses. Use a valid expression inside parentheses.

program: CONSTANT ID ASSIGN LBRACKET WILDCARD
##
## Ends in an error in state: 84.
##
## exp -> LBRACKET . exp_or_list RBRACKET [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## LBRACKET
##

Invalid array expression. '%token' cannot be used as an array element. Array elements should be valid expressions.

program: CONSTANT ID ASSIGN IF WILDCARD
##
## Ends in an error in state: 86.
##
## exp -> IF . exp THEN exp_or_list ELSE exp_or_list [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## IF
##

Invalid condition in if expression. '%token' cannot be used as a condition. If expressions should have the form 'if condition then expr1 else expr2' with a valid boolean condition.

program: CONSTANT ID ASSIGN ID LPAREN WILDCARD
##
## Ends in an error in state: 92.
##
## exp -> exp_path LPAREN option(exp_or_list) . RPAREN [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## exp_path LPAREN option(exp_or_list)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 91, spurious reduction of production option(exp_or_list) ->
##

Invalid function call argument.  Expected ')' to close the function call but found '%token'. Function call arguments should be enclosed in parentheses 'foo(...)'.

program: CONSTANT ID ASSIGN ID OP_LEVEL_3 WILDCARD
##
## Ends in an error in state: 96.
##
## exp -> exp OP_LEVEL_3 . exp [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## exp OP_LEVEL_3
##

Invalid right operand in comparison expression. '%token' cannot be used in a comparison operation. Both operands of a comparison should be valid expressions.

program: CONSTANT ID ASSIGN ID OP_LEVEL_2 WILDCARD
##
## Ends in an error in state: 98.
##
## exp -> exp OP_LEVEL_2 . exp [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## exp OP_LEVEL_2
##

Invalid right operand in bit shift expression. '%token' cannot be used in a bit shift operation. Both operands of a bit shift should be valid expressions.

program: CONSTANT ID ASSIGN ID OP_LEVEL_1 WILDCARD
##
## Ends in an error in state: 100.
##
## exp -> exp OP_LEVEL_1 . exp [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## exp OP_LEVEL_1
##

Invalid right operand in binary operation. '%token' cannot be used in a binary operation. Both operands of binary operators like '+', '|', '^' should be valid expressions.

program: CONSTANT ID ASSIGN ID OP_LEVEL_0 WILDCARD
##
## Ends in an error in state: 102.
##
## exp -> exp OP_LEVEL_0 . exp [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## exp OP_LEVEL_0
##

Invalid right operand in arithmetic or bitwise expression. '%token' cannot be used in an arithmetic or bitwise operation. Both operands of operators like '*', '/', '%', '&' should be valid expressions.

program: CONSTANT ID ASSIGN ID LBRACKET WILDCARD
##
## Ends in an error in state: 104.
##
## exp_path -> exp LBRACKET . exp RBRACKET [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## exp LBRACKET
##

Invalid array index. '%token' cannot be used as an array index. Array indices should be valid expressions that evaluate to integers.

program: CONSTANT ID ASSIGN ID LBRACKET ID XINT
##
## Ends in an error in state: 105.
##
## exp -> exp . LOR exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . LAND exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_3 exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_2 exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_1 exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_0 exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . MINUS exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp_path -> exp . COLON exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## exp_path -> exp . DOT ID [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## exp_path -> exp . LBRACKET exp RBRACKET [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## exp_path -> exp LBRACKET exp . RBRACKET [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## exp LBRACKET exp
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 90, spurious reduction of production exp -> exp_path
##

Missing closing bracket in array access. Expected ']' to close the array index, but found '%token'. Array indices should be enclosed in square brackets.

program: CONSTANT ID ASSIGN ID MINUS WILDCARD
##
## Ends in an error in state: 107.
##
## exp -> exp MINUS . exp [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## exp MINUS
##

Invalid right operand in subtraction. '%token' cannot be used as an operand in a subtraction operation. Both operands of a subtraction should be valid expressions.

program: CONSTANT ID ASSIGN ID DOT XINT
##
## Ends in an error in state: 109.
##
## exp_path -> exp DOT . ID [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## exp DOT
##

Invalid field access. Expected an identifier after the dot, but found '%token'. Field access should be in the form 'record.fieldname'.

program: CONSTANT ID ASSIGN ID COLON WILDCARD
##
## Ends in an error in state: 111.
##
## exp_path -> exp COLON . exp [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## exp COLON
##

Invalid named function call. '%token' cannot be used after a colon in a named function call. Named function calles should have the form 'name:function()' with a valid function call after the colon.

program: CONSTANT ID ASSIGN ID LOR WILDCARD
##
## Ends in an error in state: 113.
##
## exp -> exp LOR . exp [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## exp LOR
##

Invalid right operand in logical OR (||) operation. '%token' cannot be used in a logical OR expression. Both operands of '||' should be valid expressions that evaluate to boolean values.

program: CONSTANT ID ASSIGN ID LAND WILDCARD
##
## Ends in an error in state: 115.
##
## exp -> exp LAND . exp [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## exp LAND
##

Invalid right operand in logical AND (&&) operation. '%token' cannot be used in a logical AND expression. Both operands of '&&' should be valid expressions that evaluate to boolean values.

program: CONSTANT ID ASSIGN ID COMMA WILDCARD
##
## Ends in an error in state: 117.
##
## exp_or_list -> exp COMMA . exp_or_list [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## exp COMMA
##

Invalid element in comma-separated expression list. '%token' cannot be used in an expression list. Expression lists should contain valid expressions separated by commas.

program: CONSTANT ID ASSIGN ID LBRACE XINT
##
## Ends in an error in state: 119.
##
## exp -> exp_path LBRACE . loption(separated_nonempty_list(COMMA,exp_record_member)) RBRACE [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## exp_path LBRACE
##

Invalid record member. Expected member name (identifier) or '}' for empty record, but found '%token'. Record literals should have the form 'path {field1=value1, field2=value2, ...}'.

program: CONSTANT ID ASSIGN ID LBRACE ID XINT
##
## Ends in an error in state: 120.
##
## exp_record_member -> ID . ASSIGN exp [ RBRACE COMMA ]
##
## The known suffix of the stack is as follows:
## ID
##

Missing assignment in record field. Expected '=' after field name, but found '%token'. Record fields should be in the form 'fieldname=value'.

program: CONSTANT ID ASSIGN ID LBRACE ID ASSIGN WILDCARD
##
## Ends in an error in state: 121.
##
## exp_record_member -> ID ASSIGN . exp [ RBRACE COMMA ]
##
## The known suffix of the stack is as follows:
## ID ASSIGN
##

Invalid field value in record. '%token' cannot be used as a field value. Record field values should be valid expressions.

program: CONSTANT ID ASSIGN ID LBRACE ID ASSIGN ID XINT
##
## Ends in an error in state: 126.
##
## separated_nonempty_list(COMMA,exp_record_member) -> exp_record_member . [ RBRACE ]
## separated_nonempty_list(COMMA,exp_record_member) -> exp_record_member . COMMA separated_nonempty_list(COMMA,exp_record_member) [ RBRACE ]
##
## The known suffix of the stack is as follows:
## exp_record_member
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 90, spurious reduction of production exp -> exp_path
## In state 122, spurious reduction of production exp_record_member -> ID ASSIGN exp
##

Unexpected token in record literal. Expected ',' for another field or '}' to close the record, but found '%token'. Record literals should end with a closing brace.

program: CONSTANT ID ASSIGN ID LBRACE ID ASSIGN ID COMMA XINT
##
## Ends in an error in state: 127.
##
## separated_nonempty_list(COMMA,exp_record_member) -> exp_record_member COMMA . separated_nonempty_list(COMMA,exp_record_member) [ RBRACE ]
##
## The known suffix of the stack is as follows:
## exp_record_member COMMA
##

Invalid field in record literal. Expected field name (identifier) after comma, but found '%token'. Multiple record fields should be separated by commas with each field in the form 'name=value'.

program: CONSTANT ID ASSIGN IF ID XINT
##
## Ends in an error in state: 129.
##
## exp -> exp . LOR exp [ THEN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . LAND exp [ THEN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_3 exp [ THEN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_2 exp [ THEN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_1 exp [ THEN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_0 exp [ THEN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . MINUS exp [ THEN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> IF exp . THEN exp_or_list ELSE exp_or_list [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
## exp_path -> exp . COLON exp [ THEN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## exp_path -> exp . DOT ID [ THEN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## exp_path -> exp . LBRACKET exp RBRACKET [ THEN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
##
## The known suffix of the stack is as follows:
## IF exp
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 90, spurious reduction of production exp -> exp_path
##

Missing 'then' in if expression. Expected 'then' after condition, but found '%token'. If expressions should have the form 'if condition then expr1 else expr2'.

program: CONSTANT ID ASSIGN IF ID THEN WILDCARD
##
## Ends in an error in state: 130.
##
## exp -> IF exp THEN . exp_or_list ELSE exp_or_list [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## IF exp THEN
##

Invalid expression in if-then branch. '%token' cannot be used as an expression in the 'then' branch. If expressions should have valid expressions in both then and else branches.

program: CONSTANT ID ASSIGN IF ID THEN ID XINT
##
## Ends in an error in state: 131.
##
## exp -> IF exp THEN exp_or_list . ELSE exp_or_list [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## IF exp THEN exp_or_list
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 90, spurious reduction of production exp -> exp_path
## In state 95, spurious reduction of production exp_or_list -> exp
##

Missing 'else' branch in if expression. Expected 'else' after the then branch, but found '%token'. If expressions must have both then and else branches.

program: CONSTANT ID ASSIGN IF ID THEN ID ELSE WILDCARD
##
## Ends in an error in state: 132.
##
## exp -> IF exp THEN exp_or_list ELSE . exp_or_list [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## IF exp THEN exp_or_list ELSE
##

Invalid expression in if-else branch. '%token' cannot be used as an expression in the 'else' branch. If expressions should have valid expressions in both then and else branches.

program: CONSTANT ID ASSIGN LBRACKET ID XINT
##
## Ends in an error in state: 134.
##
## exp -> LBRACKET exp_or_list . RBRACKET [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## LBRACKET exp_or_list
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 90, spurious reduction of production exp -> exp_path
## In state 95, spurious reduction of production exp_or_list -> exp
##

Missing closing bracket in array literal. Expected ']' to close the array, but found '%token'. Array literals should be enclosed in square brackets.

program: CONSTANT ID ASSIGN LPAREN ID XINT
##
## Ends in an error in state: 136.
##
## exp -> LPAREN exp_or_list . RPAREN [ THEN TAG SEMICOLON RPAREN RBRACKET RBRACE OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND ELSE DOT COMMA COLON ]
##
## The known suffix of the stack is as follows:
## LPAREN exp_or_list
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 90, spurious reduction of production exp -> exp_path
## In state 95, spurious reduction of production exp_or_list -> exp
##

Missing closing parenthesis in grouped expression. Expected ')' to close the expression, but found '%token'. Grouped expressions should be enclosed in parentheses.

program: FUN ID LPAREN RPAREN WHILE LPAREN ID XINT
##
## Ends in an error in state: 139.
##
## exp -> exp . LOR exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . LAND exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_3 exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_2 exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_1 exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_0 exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . MINUS exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp_path -> exp . COLON exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## exp_path -> exp . DOT ID [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## exp_path -> exp . LBRACKET exp RBRACKET [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## stmt -> WHILE LPAREN exp . RPAREN stmt [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## WHILE LPAREN exp
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 90, spurious reduction of production exp -> exp_path
##

Missing closing parenthesis in while loop condition. Expected ')' after the condition, but found '%token'. While loop conditions must be enclosed in parentheses.

program: FUN ID LPAREN RPAREN WHILE LPAREN ID RPAREN TYPE
##
## Ends in an error in state: 140.
##
## stmt -> WHILE LPAREN exp RPAREN . stmt [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## WHILE LPAREN exp RPAREN
##

Missing body in while loop. Expected a statement or block after the loop condition, but found '%token'. While loops should have a body with at least one statement.

program: FUN ID LPAREN RPAREN VAL XINT
##
## Ends in an error in state: 141.
##
## stmt -> VAL . dexp option(preceded(ASSIGN,exp_or_list)) SEMICOLON [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## VAL
##

Invalid variable declaration. Expected an identifier after 'val', but found '%token'. Variable declarations should have the form 'val name;' or 'val name = value;'.

program: FUN ID LPAREN RPAREN MEM LPAREN XINT
##
## Ends in an error in state: 143.
##
## dexp -> LPAREN . dexp RPAREN [ TAG SEMICOLON RPAREN COMMA COLON ASSIGN ]
##
## The known suffix of the stack is as follows:
## LPAREN
##

Invalid declaration expression. Expected an identifier or another declaration expression in parentheses, but found '%token'.

program: FUN ID LPAREN RPAREN MEM ID XINT
##
## Ends in an error in state: 144.
##
## dexp -> ID . [ TAG SEMICOLON RPAREN COMMA COLON ASSIGN ]
## dexp -> ID . LBRACKET INT RBRACKET [ TAG SEMICOLON RPAREN COMMA COLON ASSIGN ]
##
## The known suffix of the stack is as follows:
## ID
##

Unexpected token in declaration. Expected '=', ':', '[', or ';' after variable name, but found '%token'. Declarations should be in the form 'val name;', 'val name: type;', or 'val name = value;'.

program: FUN ID LPAREN RPAREN MEM ID LBRACKET XINT
##
## Ends in an error in state: 145.
##
## dexp -> ID LBRACKET . INT RBRACKET [ TAG SEMICOLON RPAREN COMMA COLON ASSIGN ]
##
## The known suffix of the stack is as follows:
## ID LBRACKET
##

Invalid array size in declaration. Expected an integer literal for array size, but found '%token'. Array declarations should use decimal integers like 'val arr[10];'.

program: FUN ID LPAREN RPAREN MEM ID LBRACKET INT XINT
##
## Ends in an error in state: 146.
##
## dexp -> ID LBRACKET INT . RBRACKET [ TAG SEMICOLON RPAREN COMMA COLON ASSIGN ]
##
## The known suffix of the stack is as follows:
## ID LBRACKET INT
##

Missing closing bracket in array declaration. Expected ']' after array size, but found '%token'. Array declarations should have the form 'val name[size];'.

program: FUN ID LPAREN RPAREN MEM LPAREN WILDCARD TYPE
##
## Ends in an error in state: 148.
##
## dexp -> dexp . COMMA dexp [ RPAREN COMMA COLON ]
## dexp -> LPAREN dexp . RPAREN [ TAG SEMICOLON RPAREN COMMA COLON ASSIGN ]
## dexp -> dexp . typed [ RPAREN COMMA COLON ]
##
## The known suffix of the stack is as follows:
## LPAREN dexp
##

Missing closing parenthesis in grouped declaration. Expected ')' after the wildcard in a declaration expression, but found '%token'. Grouped declarations should have matching parentheses.

program: FUN ID LPAREN RPAREN MEM ID COMMA XINT
##
## Ends in an error in state: 150.
##
## dexp -> dexp COMMA . dexp [ TAG SEMICOLON RPAREN COMMA COLON ASSIGN ]
##
## The known suffix of the stack is as follows:
## dexp COMMA
##

Invalid pattern in multiple declaration. Expected an identifier or wildcard after comma, but found '%token'. Multiple declarations should have the form 'val a, b, c;'.

program: FUN ID LPAREN RPAREN MEM ID COMMA WILDCARD TYPE
##
## Ends in an error in state: 151.
##
## dexp -> dexp . COMMA dexp [ TAG SEMICOLON RPAREN COMMA COLON ASSIGN ]
## dexp -> dexp COMMA dexp . [ TAG SEMICOLON RPAREN COMMA COLON ASSIGN ]
## dexp -> dexp . typed [ TAG SEMICOLON RPAREN COMMA COLON ASSIGN ]
##
## The known suffix of the stack is as follows:
## dexp COMMA dexp
##

Unexpected '%token' in declaration. Expected a type annotation (preceded by ':'), another comma, or a semicolon to end the declaration.

program: FUN ID LPAREN RPAREN MEM ID ASSIGN WILDCARD
##
## Ends in an error in state: 154.
##
## option(preceded(ASSIGN,exp_or_list)) -> ASSIGN . exp_or_list [ TAG SEMICOLON ]
##
## The known suffix of the stack is as follows:
## ASSIGN
##

Invalid initializer in declaration. '%token' cannot be used as an initializer value. Initializer values should be valid expressions.

program: FUN ID LPAREN RPAREN VAL WILDCARD TYPE
##
## Ends in an error in state: 156.
##
## stmt -> VAL dexp option(preceded(ASSIGN,exp_or_list)) . SEMICOLON [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## VAL dexp option(preceded(ASSIGN,exp_or_list))
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 153, spurious reduction of production option(preceded(ASSIGN,exp_or_list)) ->
##

Missing semicolon in variable declaration. Expected ';' to end the declaration, but found '%token'. Variable declarations must end with a semicolon.

program: FUN ID LPAREN RPAREN RETURN WILDCARD
##
## Ends in an error in state: 158.
##
## stmt -> RETURN . exp_or_list SEMICOLON [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## RETURN
##

Invalid return value. '%token' cannot be used as a return value. Return statements should return a valid expression.

program: FUN ID LPAREN RPAREN RETURN ID XINT
##
## Ends in an error in state: 159.
##
## stmt -> RETURN exp_or_list . SEMICOLON [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## RETURN exp_or_list
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 90, spurious reduction of production exp -> exp_path
## In state 95, spurious reduction of production exp_or_list -> exp
##

Missing semicolon in return statement. Expected ';' after the return value, but found '%token'. Return statements must end with a semicolon.

program: FUN ID LPAREN RPAREN MEM XINT
##
## Ends in an error in state: 161.
##
## stmt -> MEM . dexp option(preceded(ASSIGN,exp_or_list)) option(tag) SEMICOLON [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## MEM
##

Invalid 'mem' declaration. Expected an identifier after 'mem', but found '%token'. Memory declarations should have the form 'mem name;' or 'mem name = value;'.

program: FUN ID LPAREN RPAREN MEM WILDCARD TYPE
##
## Ends in an error in state: 164.
##
## stmt -> MEM dexp option(preceded(ASSIGN,exp_or_list)) option(tag) . SEMICOLON [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## MEM dexp option(preceded(ASSIGN,exp_or_list)) option(tag)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 162, spurious reduction of production option(preceded(ASSIGN,exp_or_list)) ->
## In state 163, spurious reduction of production option(tag) ->
##

Missing semicolon in memory declaration. Expected ';' to end the declaration, but found '%token'. Memory declarations must end with a semicolon.

program: FUN ID LPAREN RPAREN MATCH XINT
##
## Ends in an error in state: 166.
##
## stmt -> MATCH . LPAREN exp_or_list RPAREN LBRACE list(stmt_match_case) RBRACE [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## MATCH
##

Invalid match statement. Expected '(' after 'match' to begin the match expression, but found '%token'. Match statements should have the form 'match (expression) { case1 -> stmt1, case2 -> stmt2, ... }'.

program: FUN ID LPAREN RPAREN MATCH LPAREN WILDCARD
##
## Ends in an error in state: 167.
##
## stmt -> MATCH LPAREN . exp_or_list RPAREN LBRACE list(stmt_match_case) RBRACE [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## MATCH LPAREN
##

Invalid match expression. '%token' cannot be used as a match expression. Match statements should match on a valid expression.

program: FUN ID LPAREN RPAREN MATCH LPAREN ID XINT
##
## Ends in an error in state: 168.
##
## stmt -> MATCH LPAREN exp_or_list . RPAREN LBRACE list(stmt_match_case) RBRACE [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## MATCH LPAREN exp_or_list
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 90, spurious reduction of production exp -> exp_path
## In state 95, spurious reduction of production exp_or_list -> exp
##

Missing closing parenthesis in match statement. Expected ')' after the match expression, but found '%token'. Match expressions must be enclosed in parentheses.

program: FUN ID LPAREN RPAREN MATCH LPAREN ID RPAREN XINT
##
## Ends in an error in state: 169.
##
## stmt -> MATCH LPAREN exp_or_list RPAREN . LBRACE list(stmt_match_case) RBRACE [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## MATCH LPAREN exp_or_list RPAREN
##

Missing opening brace in match statement. Expected '{' to begin the case list, but found '%token'. Match statements should be in the form 'match (expr) { case1 -> stmt1, ... }'.

program: FUN ID LPAREN RPAREN MATCH LPAREN ID RPAREN LBRACE WHILE
##
## Ends in an error in state: 170.
##
## stmt -> MATCH LPAREN exp_or_list RPAREN LBRACE . list(stmt_match_case) RBRACE [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## MATCH LPAREN exp_or_list RPAREN LBRACE
##

Invalid match pattern. Expected a pattern followed by '->' to define a case, but found '%token'. Match cases should be in the form 'pattern -> statement'.

program: FUN ID LPAREN RPAREN MATCH LPAREN ID RPAREN LBRACE LPAREN WHILE
##
## Ends in an error in state: 175.
##
## patt -> LPAREN . patt RPAREN [ RPAREN COMMA ARROW ]
##
## The known suffix of the stack is as follows:
## LPAREN
##

Invalid pattern in match statement. Expected a valid pattern (identifier, literal, or wildcard), but found '%token'. Match patterns should be valid identifiers, literals, wildcards, or grouped patterns.

program: FUN ID LPAREN RPAREN MATCH LPAREN ID RPAREN LBRACE LPAREN ID ARROW
##
## Ends in an error in state: 178.
##
## patt -> patt . COMMA patt [ RPAREN COMMA ]
## patt -> LPAREN patt . RPAREN [ RPAREN COMMA ARROW ]
##
## The known suffix of the stack is as follows:
## LPAREN patt
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 9, spurious reduction of production separated_nonempty_list(DOT,id) -> ID
## In state 16, spurious reduction of production path -> separated_nonempty_list(DOT,id)
## In state 182, spurious reduction of production patt -> path
##

Missing closing parenthesis in pattern. Expected ')' to close the pattern group, but found '%token'. Grouped patterns must have matching parentheses.

program: FUN ID LPAREN RPAREN MATCH LPAREN ID RPAREN LBRACE ID COMMA WHILE
##
## Ends in an error in state: 180.
##
## patt -> patt COMMA . patt [ RPAREN COMMA ARROW ]
##
## The known suffix of the stack is as follows:
## patt COMMA
##

Invalid pattern in comma-separated pattern list. Expected a valid pattern after comma, but found '%token'. Pattern lists should be valid patterns separated by commas.

program: FUN ID LPAREN RPAREN MATCH LPAREN ID RPAREN LBRACE ID COMMA ID XINT
##
## Ends in an error in state: 181.
##
## patt -> patt . COMMA patt [ RPAREN COMMA ARROW ]
## patt -> patt COMMA patt . [ RPAREN COMMA ARROW ]
##
## The known suffix of the stack is as follows:
## patt COMMA patt
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 9, spurious reduction of production separated_nonempty_list(DOT,id) -> ID
## In state 16, spurious reduction of production path -> separated_nonempty_list(DOT,id)
## In state 182, spurious reduction of production patt -> path
##

Unexpected token in pattern list. Expected '->' to start the match case body or another comma, but found '%token'. Match patterns should be followed by '->' and a statement.

program: FUN ID LPAREN RPAREN MATCH LPAREN ID RPAREN LBRACE ID ARROW LBRACE RBRACE WHILE
##
## Ends in an error in state: 183.
##
## list(stmt_match_case) -> stmt_match_case . list(stmt_match_case) [ RBRACE ]
##
## The known suffix of the stack is as follows:
## stmt_match_case
##

Unexpected '%token' in match cases. Expected another pattern followed by '->' or a closing brace '}'.

program: FUN ID LPAREN RPAREN MATCH LPAREN ID RPAREN LBRACE ID RPAREN
##
## Ends in an error in state: 184.
##
## patt -> patt . COMMA patt [ COMMA ARROW ]
## stmt_match_case -> patt . ARROW stmt [ XINT WILDCARD STRING REAL RBRACE LPAREN INT ID FIXED ]
##
## The known suffix of the stack is as follows:
## patt
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 9, spurious reduction of production separated_nonempty_list(DOT,id) -> ID
## In state 16, spurious reduction of production path -> separated_nonempty_list(DOT,id)
## In state 182, spurious reduction of production patt -> path
##

Missing arrow in match case. Expected '->' after pattern to separate the pattern from its handler code, but found ')'.

program: FUN ID LPAREN RPAREN MATCH LPAREN ID RPAREN LBRACE ID ARROW TYPE
##
## Ends in an error in state: 185.
##
## stmt_match_case -> patt ARROW . stmt [ XINT WILDCARD STRING REAL RBRACE LPAREN INT ID FIXED ]
##
## The known suffix of the stack is as follows:
## patt ARROW
##

Invalid statement in match case. Expected a valid statement after '->', but found '%token'. Match cases should have a pattern followed by '->' and a statement.

program: FUN ID LPAREN RPAREN LPAREN ID ASSIGN
##
## Ends in an error in state: 188.
##
## lexp -> lexp . COMMA lexp [ RPAREN LBRACKET DOT COMMA ]
## lexp -> LPAREN lexp . RPAREN [ RPAREN LBRACKET DOT COMMA ASSIGN ]
## lexp -> lexp . LBRACKET exp RBRACKET [ RPAREN LBRACKET DOT COMMA ]
## lexp -> lexp . DOT ID [ RPAREN LBRACKET DOT COMMA ]
##
## The known suffix of the stack is as follows:
## LPAREN lexp
##

Missing closing parenthesis in l-value. Expected ')' to close the grouped l-value, but found '='. Use parentheses to group l-values in complex assignments.

program: FUN ID LPAREN RPAREN ID LBRACKET WILDCARD
##
## Ends in an error in state: 190.
##
## lexp -> lexp LBRACKET . exp RBRACKET [ RPAREN LBRACKET DOT COMMA ASSIGN ]
##
## The known suffix of the stack is as follows:
## lexp LBRACKET
##

Invalid array index. '%token' cannot be used as an array index in an l-value. Array indices must be valid expressions.

program: FUN ID LPAREN RPAREN ID LBRACKET ID XINT
##
## Ends in an error in state: 191.
##
## exp -> exp . LOR exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . LAND exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_3 exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_2 exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_1 exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_0 exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . MINUS exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp_path -> exp . COLON exp [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## exp_path -> exp . DOT ID [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## exp_path -> exp . LBRACKET exp RBRACKET [ RBRACKET OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## lexp -> lexp LBRACKET exp . RBRACKET [ RPAREN LBRACKET DOT COMMA ASSIGN ]
##
## The known suffix of the stack is as follows:
## lexp LBRACKET exp
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 90, spurious reduction of production exp -> exp_path
##

Missing closing bracket in array access. Expected ']' to close the array index, but found '%token'. Array indices must be enclosed in square brackets.

program: FUN ID LPAREN RPAREN ID DOT XINT
##
## Ends in an error in state: 193.
##
## lexp -> lexp DOT . ID [ RPAREN LBRACKET DOT COMMA ASSIGN ]
##
## The known suffix of the stack is as follows:
## lexp DOT
##

Invalid field access. Expected a field name (identifier) after dot in l-value, but found '%token'. Field access should be in the form 'record.fieldname'.

program: FUN ID LPAREN RPAREN ID COMMA XINT
##
## Ends in an error in state: 195.
##
## lexp -> lexp COMMA . lexp [ RPAREN LBRACKET DOT COMMA ASSIGN ]
##
## The known suffix of the stack is as follows:
## lexp COMMA
##

Invalid l-value in comma-separated list. Expected a variable name or l-value after comma, but found '%token'. L-value lists should be variable names separated by commas.

program: FUN ID LPAREN RPAREN ID COMMA LPAREN XINT
##
## Ends in an error in state: 186.
##
## lexp -> LPAREN . lexp RPAREN [ RPAREN LBRACKET DOT COMMA ASSIGN ]
##
## The known suffix of the stack is as follows:
## LPAREN
##

Invalid l-value after opening parenthesis. Expected a variable name or another l-value, but found '%token'.

program: FUN ID LPAREN RPAREN ID COMMA ID XINT
##
## Ends in an error in state: 196.
##
## lexp -> lexp . COMMA lexp [ RPAREN LBRACKET DOT COMMA ASSIGN ]
## lexp -> lexp COMMA lexp . [ RPAREN LBRACKET DOT COMMA ASSIGN ]
## lexp -> lexp . LBRACKET exp RBRACKET [ RPAREN LBRACKET DOT COMMA ASSIGN ]
## lexp -> lexp . DOT ID [ RPAREN LBRACKET DOT COMMA ASSIGN ]
##
## The known suffix of the stack is as follows:
## lexp COMMA lexp
##

Unexpected token after l-value list. Expected an operator like ',', '=', or ';', but found '%token'.

program: FUN ID LPAREN RPAREN LBRACE TYPE
##
## Ends in an error in state: 197.
##
## stmt -> LBRACE . list(stmt) RBRACE [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## LBRACE
##

Invalid statement in block. '%token' cannot be used as a statement. Expected a valid statement like variable declaration, assignment, function call, etc.

program: FUN ID LPAREN RPAREN ITER XINT
##
## Ends in an error in state: 198.
##
## stmt -> ITER . LPAREN ID COMMA exp RPAREN stmt [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## ITER
##

Invalid iterator statement. Expected '(' after 'iter' to begin the iterator parameters, but found '%token'. Iterator statements should have the form 'iter (var, count) statement'.

program: FUN ID LPAREN RPAREN ITER LPAREN XINT
##
## Ends in an error in state: 199.
##
## stmt -> ITER LPAREN . ID COMMA exp RPAREN stmt [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## ITER LPAREN
##

Invalid iterator variable. Expected an identifier for the iteration variable, but found '%token'. Iterator variables should be valid identifiers.

program: FUN ID LPAREN RPAREN ITER LPAREN ID XINT
##
## Ends in an error in state: 200.
##
## stmt -> ITER LPAREN ID . COMMA exp RPAREN stmt [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## ITER LPAREN ID
##

Missing comma in iterator parameters. Expected ',' after the iteration variable, but found '%token'. Iterator statements should have the form 'iter (var, count) statement'.

program: FUN ID LPAREN RPAREN ITER LPAREN ID COMMA WILDCARD
##
## Ends in an error in state: 201.
##
## stmt -> ITER LPAREN ID COMMA . exp RPAREN stmt [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## ITER LPAREN ID COMMA
##

Invalid iteration count expression. '%token' cannot be used as an iteration count. The iteration count should be a valid expression that evaluates to a number.

program: FUN ID LPAREN RPAREN ITER LPAREN ID COMMA ID XINT
##
## Ends in an error in state: 202.
##
## exp -> exp . LOR exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . LAND exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_3 exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_2 exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_1 exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_0 exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . MINUS exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp_path -> exp . COLON exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## exp_path -> exp . DOT ID [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## exp_path -> exp . LBRACKET exp RBRACKET [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## stmt -> ITER LPAREN ID COMMA exp . RPAREN stmt [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## ITER LPAREN ID COMMA exp
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 90, spurious reduction of production exp -> exp_path
##

Missing closing parenthesis in iterator parameters. Expected ')' after the iteration count, but found '%token'. Iterator parameters must be enclosed in parentheses.

program: FUN ID LPAREN RPAREN ITER LPAREN ID COMMA ID RPAREN TYPE
##
## Ends in an error in state: 203.
##
## stmt -> ITER LPAREN ID COMMA exp RPAREN . stmt [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## ITER LPAREN ID COMMA exp RPAREN
##

Missing body in iterator statement. Expected a statement after the iterator parameters, but found '%token'. Iterator statements must have a body that defines what to execute in each iteration.

program: FUN ID LPAREN RPAREN IF WILDCARD
##
## Ends in an error in state: 204.
##
## stmt -> IF . LPAREN exp RPAREN stmt option(preceded(ELSE,stmt)) [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## IF
##

Invalid if statement or expression. Expected '(' to start a conditional statement or a valid expression for a conditional expression, but found a '%token'.

program: FUN ID LPAREN RPAREN IF LPAREN WILDCARD
##
## Ends in an error in state: 205.
##
## stmt -> IF LPAREN . exp RPAREN stmt option(preceded(ELSE,stmt)) [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## IF LPAREN
##

Invalid condition in if statement. '%token' cannot be used as a condition. If statement conditions should be valid expressions that evaluate to boolean values.

program: FUN ID LPAREN RPAREN IF LPAREN ID XINT
##
## Ends in an error in state: 206.
##
## exp -> exp . LOR exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . LAND exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_3 exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_2 exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_1 exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . OP_LEVEL_0 exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp -> exp . MINUS exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LOR LBRACKET LAND DOT COLON ]
## exp_path -> exp . COLON exp [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## exp_path -> exp . DOT ID [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## exp_path -> exp . LBRACKET exp RBRACKET [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COLON ]
## stmt -> IF LPAREN exp . RPAREN stmt option(preceded(ELSE,stmt)) [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## IF LPAREN exp
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 90, spurious reduction of production exp -> exp_path
##

Missing closing parenthesis in if statement condition. Expected ')' after the condition, but found '%token'. If statement conditions must be enclosed in parentheses.

program: FUN ID LPAREN RPAREN IF LPAREN ID RPAREN TYPE
##
## Ends in an error in state: 207.
##
## stmt -> IF LPAREN exp RPAREN . stmt option(preceded(ELSE,stmt)) [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## IF LPAREN exp RPAREN
##

Missing body in if statement. Expected a statement or block after the condition, but found '%token'. If statements require a body that executes when the condition is true.

program: FUN ID LPAREN RPAREN IF LPAREN ID RPAREN LBRACE RBRACE THEN
##
## Ends in an error in state: 222.
##
## stmt -> IF LPAREN exp RPAREN stmt . option(preceded(ELSE,stmt)) [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## IF LPAREN exp RPAREN stmt
##

Unexpected token '%token' after if statement body.

program: FUN ID LPAREN RPAREN IF LPAREN ID RPAREN LBRACE RBRACE ELSE TYPE
##
## Ends in an error in state: 223.
##
## option(preceded(ELSE,stmt)) -> ELSE . stmt [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## ELSE
##

Missing 'else' branch in if statement. Expected a statement or block after 'else', but found '%token'. The else branch should contain code to execute when the if condition is false.

program: FUN ID LPAREN RPAREN ID RPAREN
##
## Ends in an error in state: 208.
##
## lexp -> ID . [ LBRACKET DOT COMMA ASSIGN ]
## stmt_exp_path -> ID . [ LPAREN LBRACKET DOT COLON ]
##
## The known suffix of the stack is as follows:
## ID
##

Unexpected token '%token'. Expected an assignment ('=') after the identifier. If this is a function call, it requires a semicolon after the closing parenthesis.

program: FUN ID LPAREN RPAREN ID ASSIGN WILDCARD
##
## Ends in an error in state: 226.
##
## stmt -> lexp ASSIGN . exp_or_list SEMICOLON [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## lexp ASSIGN
##

Invalid assignment value. '%token' cannot be used as a value in an assignment. Assignments should have a valid expression on the right-hand side.

program: FUN ID LPAREN RPAREN ID ASSIGN ID XINT
##
## Ends in an error in state: 227.
##
## stmt -> lexp ASSIGN exp_or_list . SEMICOLON [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## lexp ASSIGN exp_or_list
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 90, spurious reduction of production exp -> exp_path
## In state 95, spurious reduction of production exp_or_list -> exp
##

Missing semicolon after assignment. Expected ';' to end the assignment statement, but found '%token'. All assignment statements must end with a semicolon.

program: FUN ID LPAREN RPAREN LBRACE LBRACE RBRACE TYPE
##
## Ends in an error in state: 231.
##
## list(stmt) -> stmt . list(stmt) [ RBRACE ]
##
## The known suffix of the stack is as follows:
## stmt
##

Invalid statement in block. Expected a valid statement or '}' to close the block, but found '%token'. Blocks should contain valid statements or be properly closed.

program: FUN ID LPAREN RPAREN LBRACE RBRACE XINT
##
## Ends in an error in state: 241.
##
## top_fun -> FUN fun_def . list(fun_def_cont) [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## FUN fun_def
##

Unexpected token after function definition. Expected 'and' for additional function definitions or another top-level declaration like 'type', 'fun', 'enum', etc., but found '%token'.

program: FUN ID LPAREN RPAREN LBRACE RBRACE AND XINT
##
## Ends in an error in state: 242.
##
## fun_def_cont -> AND . fun_def [ TYPE FUN EXTERNAL EOF ENUM CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## AND
##

Invalid function name in function continuation. Expected an identifier after 'and', but found '%token'. Function continuations should have the form 'and name(params) { ... }'.

program: FUN ID LPAREN RPAREN LBRACE RBRACE AND ID LPAREN RPAREN LBRACE RBRACE XINT
##
## Ends in an error in state: 245.
##
## list(fun_def_cont) -> fun_def_cont . list(fun_def_cont) [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## fun_def_cont
##

Unexpected token after function continuation. Expected 'and' for additional function definitions or another top-level declaration like 'type', 'fun', 'enum', etc., but found '%token'.

program: EXTERNAL XINT
##
## Ends in an error in state: 247.
##
## top_ext -> EXTERNAL . ID ext_args typed option(STRING) option(tag) SEMICOLON [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## EXTERNAL
##

Invalid external function name. Expected an identifier after 'external', but found '%token'. External declarations should start with 'external name(parameters) : type;'.

program: EXTERNAL ID XINT
##
## Ends in an error in state: 248.
##
## top_ext -> EXTERNAL ID . ext_args typed option(STRING) option(tag) SEMICOLON [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## EXTERNAL ID
##

Missing parameter list in external function declaration. Expected '(' to begin the parameter list, but found '%token'. External function declarations require a parameter list in the form 'external name(param1, param2, ...)'.

program: EXTERNAL ID LPAREN XINT
##
## Ends in an error in state: 249.
##
## ext_args -> LPAREN . loption(separated_nonempty_list(COMMA,ext_arg)) RPAREN [ COLON ]
##
## The known suffix of the stack is as follows:
## LPAREN
##

Invalid external function parameter. Expected a parameter name (identifier) or ')' for empty parameter list, but found '%token'. External function parameters should be identifiers followed by a type annotation.

program: EXTERNAL ID LPAREN ID XINT
##
## Ends in an error in state: 250.
##
## ext_arg -> ID . typed [ RPAREN COMMA ]
##
## The known suffix of the stack is as follows:
## ID
##

Unexpected token in external function parameter. Expected ':' for type annotation, or ')' to close the parameter list, but found '%token'. Parameter lists should be properly formatted.

program: EXTERNAL ID LPAREN ID COLON INT TYPE
##
## Ends in an error in state: 255.
##
## separated_nonempty_list(COMMA,ext_arg) -> ext_arg . [ RPAREN ]
## separated_nonempty_list(COMMA,ext_arg) -> ext_arg . COMMA separated_nonempty_list(COMMA,ext_arg) [ RPAREN ]
##
## The known suffix of the stack is as follows:
## ext_arg
##

Missing closing parenthesis in external function parameter list. Expected ')' to close the parameter list or ',' for another parameter, but found '%token'. External function declarations should have matching opening and closing parentheses around parameters.

program: EXTERNAL ID LPAREN ID COLON ID COMMA XINT
##
## Ends in an error in state: 256.
##
## separated_nonempty_list(COMMA,ext_arg) -> ext_arg COMMA . separated_nonempty_list(COMMA,ext_arg) [ RPAREN ]
##
## The known suffix of the stack is as follows:
## ext_arg COMMA
##

Invalid parameter in external function parameter list. Expected another parameter name (identifier) after comma, but found '%token'. Parameter lists should have properly formatted parameters separated by commas.

program: EXTERNAL ID LPAREN RPAREN XINT
##
## Ends in an error in state: 258.
##
## top_ext -> EXTERNAL ID ext_args . typed option(STRING) option(tag) SEMICOLON [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## EXTERNAL ID ext_args
##

Missing return type in external function declaration. Expected ':' followed by a type after the parameter list, but found '%token'. External function declarations must specify a return type.

program: EXTERNAL ID LPAREN RPAREN COLON INT TYPE
##
## Ends in an error in state: 262.
##
## top_ext -> EXTERNAL ID ext_args typed option(STRING) option(tag) . SEMICOLON [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## EXTERNAL ID ext_args typed option(STRING) option(tag)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 259, spurious reduction of production option(STRING) ->
## In state 261, spurious reduction of production option(tag) ->
##

Missing semicolon in external function declaration. Expected ';' to end the declaration, but found '%token'. External function declarations must end with a semicolon.

program: ENUM XINT
##
## Ends in an error in state: 264.
##
## top_enum -> ENUM . ID LBRACE separated_nonempty_list(COMMA,id) RBRACE [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## ENUM
##

Invalid enum name. Expected an identifier after 'enum', but found '%token'. Enum declarations should have the form 'enum name { Value1, Value2, ... }'.

program: ENUM ID XINT
##
## Ends in an error in state: 265.
##
## top_enum -> ENUM ID . LBRACE separated_nonempty_list(COMMA,id) RBRACE [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## ENUM ID
##

Missing opening brace in enum declaration. Expected '{' after enum name to begin listing the enum values, but found '%token'. Enum declarations should have the form 'enum name { Value1, Value2, ... }'.


program: CONSTANT XINT
##
## Ends in an error in state: 272.
##
## top_constant -> CONSTANT . ID ASSIGN exp_or_list SEMICOLON [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## CONSTANT
##

Invalid constant name. Expected an identifier after 'constant', but found '%token'. Constant declarations should have the form 'constant name = value;'.

program: CONSTANT ID XINT
##
## Ends in an error in state: 273.
##
## top_constant -> CONSTANT ID . ASSIGN exp_or_list SEMICOLON [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## CONSTANT ID
##

Missing assignment in constant declaration. Expected '=' after constant name, but found '%token'. Constant declarations must have an assignment in the form 'constant name = value;'.

program: CONSTANT ID ASSIGN WILDCARD
##
## Ends in an error in state: 274.
##
## top_constant -> CONSTANT ID ASSIGN . exp_or_list SEMICOLON [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## CONSTANT ID ASSIGN
##

Invalid constant value. '%token' cannot be used as a value in a constant declaration. Constants must be assigned a valid expression.

program: CONSTANT ID ASSIGN ID XINT
##
## Ends in an error in state: 275.
##
## top_constant -> CONSTANT ID ASSIGN exp_or_list . SEMICOLON [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## CONSTANT ID ASSIGN exp_or_list
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 90, spurious reduction of production exp -> exp_path
## In state 95, spurious reduction of production exp_or_list -> exp
##

Missing semicolon in constant declaration. Expected ';' to end the declaration, but found '%token'. Constant declarations must end with a semicolon.

program: TYPE ID SEMICOLON XINT
##
## Ends in an error in state: 278.
##
## list(top_stmt) -> top_stmt . list(top_stmt) [ EOF ]
##
## The known suffix of the stack is as follows:
## top_stmt
##

Unexpected token at top level. Expected a top-level declaration like 'type', 'fun', 'enum', 'external', or 'constant', but found '%token'.

program: FUN ID LPAREN RPAREN ID COLON ID XINT
##
## Ends in an error in state: 221.
##
## stmt_exp_path -> stmt_exp_path . COLON stmt_exp_path [ LPAREN LBRACKET DOT COLON ]
## stmt_exp_path -> stmt_exp_path COLON stmt_exp_path . [ LPAREN LBRACKET DOT COLON ]
## stmt_exp_path -> stmt_exp_path . DOT ID [ LPAREN LBRACKET DOT COLON ]
## stmt_exp_path -> stmt_exp_path . LBRACKET exp RBRACKET [ LPAREN LBRACKET DOT COLON ]
##
## The known suffix of the stack is as follows:
## stmt_exp_path COLON stmt_exp_path
##

Unexpected token in function call. Expected a semicolon, a dot for field access, another colon for naming the function, or a bracket for array indexing, but found '%token'.

program: FUN ID LPAREN RPAREN ID LPAREN WILDCARD
##
## Ends in an error in state: 211.
##
## stmt -> stmt_exp_path LPAREN option(exp_or_list) . RPAREN SEMICOLON [ XINT WILDCARD WHILE VAL TYPE STRING RETURN REAL RBRACE MEM MATCH LPAREN LBRACE ITER INT IF ID FUN FIXED EXTERNAL EOF ENUM ELSE CONSTANT AND ]
##
## The known suffix of the stack is as follows:
## stmt_exp_path LPAREN option(exp_or_list)
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 210, spurious reduction of production option(exp_or_list) ->
##

Invalid function argument. '%token' cannot be used as a function argument. Function arguments should be valid expressions. Expected a closing parenthesis if no arguments are needed.

program: TYPE ID LBRACE VAL ID COLON ID TAG
##
## Ends in an error in state: 14.
##
## type_member -> VAL ID COLON type_ . SEMICOLON [ VAL RBRACE ]
##
## The known suffix of the stack is as follows:
## VAL ID COLON type_
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 9, spurious reduction of production separated_nonempty_list(DOT,id) -> ID
## In state 16, spurious reduction of production path -> separated_nonempty_list(DOT,id)
## In state 17, spurious reduction of production type_id -> path
## In state 12, spurious reduction of production type_ -> type_id
##

Unexpected token in member declaration. Expected a semicolon ';' to end the declaration, but found '%token'.

program: ENUM ID LBRACE XINT
##
## Ends in an error in state: 266.
##
## top_enum -> ENUM ID LBRACE . separated_nonempty_list(COMMA,id) RBRACE [ TYPE FUN EXTERNAL EOF ENUM CONSTANT ]
##
## The known suffix of the stack is as follows:
## ENUM ID LBRACE
##

Expected a list of enumeration values but got '%token'. Enums are declared in the form 'enum name { Value1, Value2}'.

program: ENUM ID LBRACE ID XINT
##
## Ends in an error in state: 267.
##
## separated_nonempty_list(COMMA,id) -> ID . [ RBRACE ]
## separated_nonempty_list(COMMA,id) -> ID . COMMA separated_nonempty_list(COMMA,id) [ RBRACE ]
##
## The known suffix of the stack is as follows:
## ID
##

Unexpected token in enum list. Expected ',' for another argument or '}' to close the argument list, but found '%token'.

program: ENUM ID LBRACE ID COMMA XINT
##
## Ends in an error in state: 268.
##
## separated_nonempty_list(COMMA,id) -> ID COMMA . separated_nonempty_list(COMMA,id) [ RBRACE ]
##
## The known suffix of the stack is as follows:
## ID COMMA
##

Invalid token in enum declaration list. Expected an identifier for the next enum name, but found '%token'. Enums are declared in the form 'enum name { Value1, Value2}'.


program: EXTERNAL ID LPAREN RPAREN COLON ID TYPE
##
## Ends in an error in state: 17.
##
## type_composed -> path . LPAREN separated_nonempty_list(COMMA,type_) RPAREN [ XINT WILDCARD WHILE VAL TRUE TAG STRING SEMICOLON RPAREN RETURN REAL MINUS MEM MATCH LPAREN LBRACKET LBRACE ITER INT IF ID FIXED FALSE COMMA COLON ASSIGN ]
## type_id -> path . [ XINT WILDCARD WHILE VAL TRUE TAG STRING SEMICOLON RPAREN RETURN REAL MINUS MEM MATCH LPAREN LBRACKET LBRACE ITER INT IF ID FIXED FALSE COMMA COLON ASSIGN ]
##
## The known suffix of the stack is as follows:
## path
##
## WARNING: This example involves spurious reductions.
## This implies that, although the LR(1) items shown above provide an
## accurate view of the past (what has been recognized so far), they
## may provide an INCOMPLETE view of the future (what was expected next).
## In state 9, spurious reduction of production separated_nonempty_list(DOT,id) -> ID
## In state 16, spurious reduction of production path -> separated_nonempty_list(DOT,id)
##

<YOUR SYNTAX ERROR MESSAGE HERE>

program: FUN ID LPAREN RPAREN LPAREN WHILE
##
## Ends in an error in state: 185.
##
## exp -> LPAREN . exp_or_list RPAREN [ RPAREN OP_LEVEL_3 OP_LEVEL_2 OP_LEVEL_1 OP_LEVEL_0 MINUS LPAREN LOR LBRACKET LBRACE LAND DOT COMMA COLON ]
## lexp -> LPAREN . lexp RPAREN [ RPAREN LBRACKET DOT COMMA ASSIGN ]
##
## The known suffix of the stack is as follows:
## LPAREN
##

<YOUR SYNTAX ERROR MESSAGE HERE>