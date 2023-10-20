# my-python
author: mugi(Japanese)

## このプロジェクトの狙い
大学の講義の1つに研究室体験のような講義がある。このコードは3年春学期に、オリジナル言語処理系を作ってみることを目的とした講義の中で作成した言語である。この言語は、OCamlを使用したpythonのような言語である。  
このプログラムは、[住井英二郎先生の記事](https://xtech.nikkei.com/it/article/COLUMN/20060808/245369/)で作成されている言語処理系を発展させる形で作っていった。

## このプログラムの使用方法
使用するにはOCamlをダウンロードする必要がある。詳しくは[OCamlダウンロード公式ページ](https://ocaml.org/install)を参照。   
言語処理系プログラムは、以下の4つのOCamlファイルから作られている。  
- syntax.ml  
代数データ型を用いて構文木を表すデータ型が定義されている。
- parser.mly  
トークンと構文解析のルールが定義されている。ocamlyaccを使って構文解析器(parser.mliとparser.ml)を作るための材料。
- lexer.mll  
字句解析のルールが定義されている。ocamllexを使って字句解析器(lexer.ml)を作るための材料。
- interpret.ml  
処理系の本体。解析された構文の処理のルールが記載されている。

この4つのファイルを同じディレクトリに保存し、コマンドプロンプトを開いて、以下のコードを実行する。
```
ocamlyacc parser.mly
ocamllex.opt lexer.mll
ocamlc.opt syntax.ml parser.mli parser.ml lexer.ml interpret.ml -o interpret.exe
```
この3つがエラー無く実行されることで、言語処理系プログラムinterpret.exeが生成される。  
interpret.exeのみを実行したとき、

## サンプルコード
