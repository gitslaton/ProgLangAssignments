%{
  open Types
%}

%token <float> FLOAT
%token TRUE FALSE
%token IF THEN ELSE
%token DBLSEMI
%token AND OR NOT 
%nonassoc FLOAT
%nonassoc ELSE
%left OR AND
%nonassoc NOT

%start main
%type <Types.exprS> main
%%

main:
  | headEx DBLSEMI               { $1 }
;

headEx:
  | expr                         { $1 }
;

expr:
  | FLOAT                        { NumS $1 }
  | TRUE						 { BoolS true }
  | FALSE						 { BoolS false }
  | IF expr THEN expr ELSE expr  { IfS ($2, $4, $6) }
  | expr AND expr				 { AndS ($1, $3) }
  | expr OR expr 				 { OrS ($1, $3) }
  | NOT expr 					 { NotS ($2)}
;

