%{
  open Ast
%}

%token <int> INT;
%token <string> IDENT;
%token <bool> BOOL;
%token PLUS MINUS STAR DIV
%token NEQ GT LT GE LE EQPHY 
%token LP RP
%token LET EQ IN
%token AND OR
%token FUN ARROW SHOW
%token IF THEN ELSE
%token WHILE DO DONE
%token SEMI
%token COMMA
%token REF BANG SET
%token SPAWN
%token EOF
%token WAIT JOIN
       
(* %token TRY CATCH MATCH WITH *)
%nonassoc IN
%nonassoc ARROW
%left SEMI
%nonassoc ELSE
%nonassoc SET
%left OR
%left AND
%left EQ GT LT GE LE EQPHY
%left PLUS MINUS
%left STAR DIV
%nonassoc LP INT IDENT
%nonassoc BANG

%start main
%type <Ast.expr> main

%%

main:
| e=expr EOF { e }
;

expr:
| e=simple_expr                               { e                 }    
| e1=expr op=binop e2=expr                    { Binop(op, e1, e2) }
| LET id=IDENT EQ e1=expr IN e2=expr          { Letin(id, e1, e2) }
| FUN id=IDENT ARROW e=expr                   { Fun(id, e)        }
| e1=expr e2=simple_expr                      { Apply(e1, e2)     }
| IF c=expr THEN e1=expr ELSE e2=expr         { Cond(c, e1, e2)   }
| WHILE c=expr DO e=expr DONE                 { Loop(c, e)        }
| e1=expr SEMI e2=expr                        { Seq(e1, e2)       }
| REF e=simple_expr                           { Ref(e)            }
| d=expr SET e=expr                           { SetR(d, e)        }
| SPAWN e1=simple_expr e2=simple_expr         { Spawn(e1, e2)     }
(* Ajout *)
| op=unop e=simple_expr                       { Unop(op, e)       }
| SHOW el=separated_list(COMMA, simple_expr)  { Show(el)          }
| WAIT                                        { Wait}
| JOIN                                        { Join}
(* /!\ Work in Progress /!\ *)
(*/ ! \                / ! \  *)
(* | MATCH e1=simple_expr WITH e2=simple_expr {Match(e1, e2} *)
(* | TRY e1=expr CATCH e2=expr            { Exn(e1, e2)       } *)
;

simple_expr:
| n=INT              { Int(n)    }
| id=IDENT           { Ident(id) }
| LP e=expr RP       { e         }
| BANG e=simple_expr { GetR(e)   }
(* Ajout *)
| b=BOOL             { Bool(b)   }
;

%inline binop:
| PLUS  { Add   }
| MINUS { Sub   }
| STAR  { Mult  }
(* Ajout *)
| DIV   { Div   }
| EQ    { Eq    }
| NEQ   { Neq   }
| EQPHY { Eqphy }
| GT    { Gt    }
| LT    { Lt    }
| GE    { Ge    }
| LE    { Le    }
| OR    { Or    }
| AND   { And   }
(* Ajout *)
;

%inline unop:
| MINUS { Minus }
;
