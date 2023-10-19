%{
open Syntax
%}

%token <string> VAR     /* 変数名 */
%token <int> CONST_I    /* int型定数 */
%token <float> CONST_F  /* float型定数 */
%token EQUAL          /* = */
%token PLUS           /* + */
%token SUB            /* - */
%token MULT           /* * */
%token DIV            /* / */
%token EXC            /* % */
%token POW            /* ** */
%token WHILE          /* 予約語「while」 */
%token IF             /* 予約語「if」 */
%token LPAREN         /* ( */
%token RPAREN         /* ) */
%token GREATER        /* > */
%token GREATER_E      /* >= */
%token LESSER         /* < */
%token LESSER_E       /* <= */
%token DEQUAL         /* == */
%token LBRACE         /* { */
%token RBRACE         /* } */
%token SEMICOLON      /* ; */
%token PRINT          /* 予約語「print」 */
%token INPUT          /* 予約語「input」 */

%type <Syntax.statement> statement
%start statement

%%

statement: /* 一つの文を構文解析するルール */
| VAR EQUAL CONST_I
   /*「x = i」という形の文を解析する */
    { Const_I($1, $3) }
| VAR EQUAL CONST_F
   /*「x = f」という形の文を解析する */
    { Const_F($1, $3) }
| VAR EQUAL VAR PLUS VAR
   /* 「x = y + z」という形の文を解析する */
    { Add($1, $3, $5) }
| VAR EQUAL VAR SUB VAR
   /* 「x = y - z」という形の文を解析する */
    { Sub($1, $3, $5) }
| VAR EQUAL VAR MULT VAR
   /* 「x = y * z」という形の文を解析する */
    { Mul($1, $3, $5) }
| VAR EQUAL VAR DIV VAR
   /* 「x = y / z」という形の文を解析する */
    { Div($1, $3, $5) }
| VAR EQUAL VAR EXC VAR
   /* 「x = y % z」という形の文を解析する */
    { Exc($1, $3, $5) }
| VAR EQUAL VAR POW VAR
   /* 「x = y ** z」という形の文を解析する */
    { Pow($1, $3, $5) }
| WHILE LPAREN VAR GREATER VAR RPAREN statement
   /* 「while (x > y) 文」という形の文を解析する */
    { While_G($3, $5, $7) }
| WHILE LPAREN VAR GREATER_E VAR RPAREN statement
   /* 「while (x >= y) 文」という形の文を解析する */
    { While_GE($3, $5, $7) }
| WHILE LPAREN VAR LESSER VAR RPAREN statement
   /* 「while (x < y) 文」という形の文を解析する */
    { While_L($3, $5, $7) }
| WHILE LPAREN VAR LESSER_E VAR RPAREN statement
   /* 「while (x <= y) 文」という形の文を解析する */
    { While_LE($3, $5, $7) }
| WHILE LPAREN VAR DEQUAL VAR RPAREN statement
   /* 「while (x == y) 文」という形の文を解析する */
    { While_E($3, $5, $7) }
| IF LPAREN VAR GREATER VAR RPAREN statement
   /* 「if (x > y) 文」という形の文を解析する */
    { If_G($3, $5, $7) }
| IF LPAREN VAR GREATER_E VAR RPAREN statement
   /* 「if (x >= y) 文」という形の文を解析する */
    { If_GE($3, $5, $7) }
| IF LPAREN VAR LESSER VAR RPAREN statement
   /* 「if (x < y) 文」という形の文を解析する */
    { If_L($3, $5, $7) }
| IF LPAREN VAR LESSER_E VAR RPAREN statement
   /* 「if (x <= y) 文」という形の文を解析する */
    { If_LE($3, $5, $7) }
| IF LPAREN VAR DEQUAL VAR RPAREN statement
   /* 「if (x == y) 文」という形の文を解析する */
    { If_E($3, $5, $7) }
| LBRACE statement_list RBRACE
   /* 「{ 文1; 文2; …; 文n }」という形の文を解析する */
    { Seq($2) }
| PRINT LPAREN VAR RPAREN
   /*  「print(x)」という形の文を解析する */
    { Print($3) }
| VAR EQUAL INPUT LPAREN RPAREN
   /*  「x = input()」という形の文を解析する */
    { Input($1) }
| error /* 以上にマッチしない場合はエラーとして例外を発生(行番号も表示させる) */
    { (* print line and column numbers as well *)
      let start_pos = Parsing.symbol_start_pos () in
      let end_pos   = Parsing.symbol_end_pos () in
      failwith
        (Printf.sprintf
           "parse error near position (line %d, col %d)-(line %d, col %d)"
           (start_pos.Lexing.pos_lnum)
           (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol)
           (end_pos.Lexing.pos_lnum)
           (end_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol)) }

statement_list: /* 文の列を構文解析するルール */
| statement SEMICOLON statement_list
    /* 一つの文を，文のリストの先頭に追加 */
    { $1 :: $3 }
| /* 空列 */
    { [] } /* 空リストを返す */
