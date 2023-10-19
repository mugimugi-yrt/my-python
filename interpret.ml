open Syntax
exception Break

let table = Hashtbl.create 10

let rec interpret (s:statement) = match s with
  | Const_I(x, i) ->
      (* 「x = i」という形の文を実行する *)
      Hashtbl.replace table x (Int i)
  | Const_F(x, f) ->
      (* 「x = f」という形の文を実行する *)
      Hashtbl.replace table x (Float f)
  | Add(x, y, z) ->
      (* 「x = y + z」という形の文を実行する *)
      ( match Hashtbl.find table y, Hashtbl.find table z with
        | Int(i), Int(j) -> Hashtbl.replace table x (Int (i + j))
        | Int(i), Float(j) -> Hashtbl.replace table x (Float (float_of_int i +. j))
        | Float(i), Int(j) -> Hashtbl.replace table x (Float (i +. float_of_int j))
        | Float(i), Float(j) -> Hashtbl.replace table x (Float (i +. j)) )
  | Sub(x, y, z) ->
      (* 「x = y - z」という形の文を実行する *)
      ( match Hashtbl.find table y, Hashtbl.find table z with
        | Int(i), Int(j) -> Hashtbl.replace table x (Int (i - j))
        | Int(i), Float(j) -> Hashtbl.replace table x (Float (float_of_int i -. j))
        | Float(i), Int(j) -> Hashtbl.replace table x (Float (i -. float_of_int j))
        | Float(i), Float(j) -> Hashtbl.replace table x (Float (i -. j)) )
  | Mul(x, y, z) ->
      (* 「x = y * z」という形の文を実行する *)
      ( match Hashtbl.find table y, Hashtbl.find table z with
        | Int(i), Int(j) -> Hashtbl.replace table x (Int (i * j))
        | Int(i), Float(j) -> Hashtbl.replace table x (Float (float_of_int i *. j))
        | Float(i), Int(j) -> Hashtbl.replace table x (Float (i *. float_of_int j))
        | Float(i), Float(j) -> Hashtbl.replace table x (Float (i *. j)) )
  | Div(x, y, z) ->
      (* 「x = y / z」という形の文を実行する *)
      ( match Hashtbl.find table y, Hashtbl.find table z with
        | Int(i), Int(j) -> Hashtbl.replace table x (Int (i / j))
        | Int(i), Float(j) -> Hashtbl.replace table x (Float (float_of_int i /. j))
        | Float(i), Int(j) -> Hashtbl.replace table x (Float (i /. float_of_int j))
        | Float(i), Float(j) -> Hashtbl.replace table x (Float (i /. j)) )
  | Exc(x, y, z) ->
      (* 「x = y % z」という形の文を実行する *)
      (* ただし、整数同士のみ有効とする *)
      ( match Hashtbl.find table y, Hashtbl.find table z with
        | Int(i), Int(j) -> Hashtbl.replace table x (Int (i mod j))
        | Int(i), Float(j) -> print_string "error: only int\n"
        | Float(i), Int(j) -> print_string "error: only int\n"
        | Float(i), Float(j) -> print_string "error: only int\n" )
  | Pow(x, y, z) ->
      (* 「x = y / z」という形の文を実行する *)
      ( match Hashtbl.find table y, Hashtbl.find table z with
        | Int(i), Int(j) -> Hashtbl.replace table x (Int (int_of_float (float_of_int i ** float_of_int j)))
        | Int(i), Float(j) -> Hashtbl.replace table x (Float (float_of_int i ** j))
        | Float(i), Int(j) -> Hashtbl.replace table x (Float (i ** float_of_int j))
        | Float(i), Float(j) -> Hashtbl.replace table x (Float (i ** j)) )
  | While_G(x, y, s2) ->
      (* 「while (x > y) 文」という形の文を実行する *)
      if Hashtbl.find table x >
        Hashtbl.find table y then
        (interpret s2; (* 文s2への再帰により文を実行する *)
         interpret s   (* 自身sへの再帰によりwhile文を再度実行する *)
	)
  | While_GE(x, y, s2) ->
      (* 「while (x >= y) 文」という形の文を実行する *)
      if Hashtbl.find table x >=
        Hashtbl.find table y then
        (interpret s2; (* 文s2への再帰により文を実行する *)
         interpret s   (* 自身sへの再帰によりwhile文を再度実行する *)
	)
  | While_L(x, y, s2) ->
      (* 「while (x < y) 文」という形の文を実行する *)
      if Hashtbl.find table x <
        Hashtbl.find table y then
        (interpret s2; (* 文s2への再帰により文を実行する *)
         interpret s   (* 自身sへの再帰によりwhile文を再度実行する *)
	)
  | While_LE(x, y, s2) ->
      (* 「while (x <= y) 文」という形の文を実行する *)
      if Hashtbl.find table x <=
        Hashtbl.find table y then
        (interpret s2; (* 文s2への再帰により文を実行する *)
         interpret s   (* 自身sへの再帰によりwhile文を再度実行する *)
	)
  | While_E(x, y, s2) ->
      (* 「while (x == y) 文」という形の文を実行する *)
      if Hashtbl.find table x =
        Hashtbl.find table y then
        (interpret s2; (* 文s2への再帰により文を実行する *)
         interpret s   (* 自身sへの再帰によりwhile文を再度実行する *)
	)
  | If_G(x, y, s2) ->
      (* 「if (x > y) 文」という形の文を実行する *)
      if Hashtbl.find table x >
        Hashtbl.find table y then
        (interpret s2; (* 文s2への再帰により文を実行する *))
  | If_GE(x, y, s2) ->
      (* 「if (x >= y) 文」という形の文を実行する *)
      if Hashtbl.find table x >=
        Hashtbl.find table y then
        (interpret s2; (* 文s2への再帰により文を実行する *))
  | If_L(x, y, s2) ->
      (* 「if (x < y) 文」という形の文を実行する *)
      if Hashtbl.find table x <
        Hashtbl.find table y then
        (interpret s2; (* 文s2への再帰により文を実行する *))
  | If_LE(x, y, s2) ->
      (* 「if (x <= y) 文」という形の文を実行する *)
      if Hashtbl.find table x <=
        Hashtbl.find table y then
        (interpret s2; (* 文s2への再帰により文を実行する *))
  | If_E(x, y, s2) ->
      (* 「if (x == y) 文」という形の文を実行する *)
      if Hashtbl.find table x =
        Hashtbl.find table y then
        (interpret s2; (* 文s2への再帰により文を実行する *))
  | Seq(ss) ->
     (* 「{ 文1; 文2; …; 文n }」という形の文を実行する *)
      List.iter interpret ss (* 文のリストssの各要素に順に再帰する *)
  | Print(x) ->
      (*  「print(x)」という形の文を実行する *)
      ( match Hashtbl.find table x with
        | Int(i) -> Printf.printf "%d\n" (i)
        | Float(f) -> Printf.printf "%f\n" (f) )
  | Input(x) ->
    (* 「x = input()」という形の文を実行する *)
    let i = read_line() in
    try
      let int_value = int_of_string i in
      Hashtbl.replace table x (Int int_value)
    with
    | Failure _ ->
      begin
        try
          let float_value = float_of_string i in
          Hashtbl.replace table x (Float float_value)
        with
        | Failure _ -> print_string "error: invalid input\n"
      end

let main =
  if Array.length Sys.argv > 1 then
    let input_channel = open_in Sys.argv.(1) in
    let s = Parser.statement Lexer.token (Lexing.from_channel input_channel) in
    interpret s;
    close_in input_channel
  else
    try
      print_endline "Welcome!";
      while true do
        try
          print_string "> ";
          let input = read_line () in
          if input = "exit()" || input = "exit();" then raise Break;
          let t = Parser.statement Lexer.token (Lexing.from_string input) in
          interpret t
        with
        | Break -> raise Break
        | End_of_file -> raise Break (* 緊急回避用 *)
        | exn -> print_endline ("Error: " ^ Printexc.to_string exn)
      done
    with
    | Break -> ()