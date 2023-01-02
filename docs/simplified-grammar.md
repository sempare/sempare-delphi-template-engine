# ![](../images/sempare-logo-45px.png) SempareTemplate Engine

Copyright (c) 2019-2023 [Sempare Limited](http://www.sempare.ltd)

## Simplified Grammar definition

NOTE: normal precedence rules apply. expression definition below is simplified to keep it short.
```
main     : text 
         | stmts
         ;
text     : textblock
         ;
stmts    : stmt*
         ;
stmt     : if 
         | for
         | while
         | assign
         | include
         | template
         | require
         | with
         ;
loopstmts: loopstmt*
         ;
loopstmt : stmt
         | break
         | continue
         ;
include  : 'include' '(' expr [, expr ]')'
         ;
if       : 'if' expr stmts ('elif' expr stmts)* ('else' stmts) 'end'
         ;
for      : 'for' id 'in' expr loopstmts 'end'
         | 'for' id ':=' expr ('to'|'downto') loopstmts 'end'
         ;
while    : 'while' expr loopstmts 'end'
         ;
template : 'template'  expr  stmts 'end'
         ;
with     : 'with'  expr  stmts 'end'
         ;
assign   : id ':=' expr
         ;
require  : 'require' '(' exprlist ')'
         ;
exprlist : expr? (',' expr)*
         ;
expr     : id '(' exprlist ')'
         | id
         | id '.' id
         | id '[' expr ']'
         | '-' expr
         | 'not' expr
         | '(' expr ')'
         | expr ('+' | '-' | '/' | '*' | 'mod' | 'in' ) expr
         | expr ('and' | 'or' ) expr
         ;
literal  : 'true'
         | 'false'
         | string
         | number
         ;
```
The 'string' literal is anything contained within single quoted string.
A 'number' can be an integer (1,2,3...) or a float.
