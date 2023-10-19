type var = string (* 「変数名」を表す型 *)

type value = (* 異なる型の値を一つに束ねるデータ型 *)
    Int of int  (* integer type *)
  | Float of float (* float type *)

type statement = (* 「文」を表す代数データ型 *)
  | Const_I of var * int
      (* 「x = i」という形の文を表す *)
  | Const_F of var * float
      (* 「x = f」という形の文を表す *)
  | Add of var * var * var
      (* 「x = y + z」という形の文を表す *)
  | Sub of var * var * var
      (* 「x = y - z」という形の文を表す *)
  | Mul of var * var * var
      (* 「x = y * z」という形の文を表す *)
  | Div of var * var * var
      (* 「x = y / z」という形の文を表す *)
  | Exc of var * var * var
      (* 「x = y % z」という形の文を表す *)
  | Pow of var * var * var
      (* 「x = y ** z」という形の文を表す *)
  | While_G of var * var * statement
      (* 「while (x > y) 文」という形の文を表す *)
  | While_GE of var * var * statement
      (* 「while (x >= y) 文」という形の文を表す *)
  | While_L of var * var * statement
      (* 「while (x < y) 文」という形の文を表す *)
  | While_LE of var * var * statement
      (* 「while (x <= y) 文」という形の文を表す *)
  | While_E of var * var * statement
      (* 「while (x == y) 文」という形の文を表す *)
  | If_G of var * var * statement
      (* 「if (x > y) 文」という形の文を表す *)
  | If_GE of var * var * statement
      (* 「if (x >= y) 文」という形の文を表す *)
  | If_L of var * var * statement
      (* 「if (x < y) 文」という形の文を表す *)
  | If_LE of var * var * statement
      (* 「if (x <= y) 文」という形の文を表す *)
  | If_E of var * var * statement
      (* 「if (x == y) 文」という形の文を表す *)
  | Seq of statement list
      (* 「{ 文1; 文2; …; 文n }」という形の文を表す *)
  | Print of var
      (* 「print(x)」という形の文を表す *)
  | Input of var
      (* 「x = input()」という形の文を表す *)
