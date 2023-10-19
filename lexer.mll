{
open Parser
let new_line_is_read lexbuf =
  Lexing.new_line lexbuf;
  lexbuf (* 行番号情報を更新 *)
}

let space = [' ' '\t']                   (* 空白(タブ文字も含む) *)
let digit = ['0'-'9']                    (* 数字 *)
let alpha = ['A'-'Z' 'a'-'z' '_']        (* アルファベット, 下線 *)
let new_line = '\r' '\n' | [ '\r' '\n' ] (* 改行 *)

rule token = parse
| "while"      (* 2文字以上のトークンは二重引用符で囲む *)
    { WHILE }  (* トークンの照合は上から順に行われるため，*)
| "if"
    { IF }
| "print"
    { PRINT }
| "input"
    { INPUT }
| alpha (digit|alpha)*
    (* 以上の予約語にマッチしなければ変数名として処理 *)
    { VAR(Lexing.lexeme lexbuf) }
| '-'? digit+
    { CONST_I(int_of_string (Lexing.lexeme lexbuf)) }
| '-'? digit+ '.' digit+
    { CONST_F(float_of_string (Lexing.lexeme lexbuf)) }
| space+
    (* 空白をスキップして字句解析を続行 *)
    { token lexbuf }
| new_line
    { (ignore (new_line_is_read lexbuf)); token lexbuf } (* エラーメッセージ用の行番号を更新 *)
| '='
    (* 1文字のトークンは単一引用符で囲む *)
    { EQUAL }
| '+'
    { PLUS }
| '-'
    { SUB }
| '*'
    { MULT }
| '/'
    { DIV }
| '%'
    { EXC }
| "**"
    { POW }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '>'
    { GREATER }
| ">="
    { GREATER_E }
| '<'
    { LESSER }
| "<="
    { LESSER_E }
| "=="
    { DEQUAL }
| '{'
    { LBRACE }
| '}'
    { RBRACE }
| ';'
    { SEMICOLON }
| _
    (* 以上にマッチしない場合はエラーとして例外を発生 *)
    (* 行番号と，行内の先頭から数えた位置を表示  *)
    { failwith
	(let start_p = Lexing.lexeme_start_p lexbuf in
         let end_p   = Lexing.lexeme_end_p   lexbuf in
         Printf.sprintf
           "lexical analysis error: unknown token '%s' near line %d characters %d-%d"
           (Lexing.lexeme lexbuf)
	   start_p.Lexing.pos_lnum
	   (start_p.Lexing.pos_cnum - start_p.Lexing.pos_bol)
	   (end_p.Lexing.pos_cnum -   end_p.Lexing.pos_bol)) }
