# OCamlにおける遅延評価

stdlib/lazy.mlにまとまっている。

http://ocaml.jp/Lazy
http://ocaml.jp/Lazy%20Pattern

要は

    let x = lazy hogehoge

するとhogehoge部分が未評価のままxに束縛され、、、

    Lazy.force x

すると、xの中身が評価される。
もし二回目にLazy.forceしてもxの中が評価済みであれば、結果のみが返ってくる。

# Stream

stdlib/stream.mlで定義されているが、

http://ocaml.jp/Stream
http://www.geocities.jp/m_hiroi/func/ocaml23.html

とは内容が違う。ここではsmall_stream.mlをOCaml用に作ってみる。


# 疑問点のリスト

## OCamlに以下のシンタックスシュガーがあるか？

    fun lazy f p = e (* シンタックスシュガーは以下のような意味 *)
	fun f x = $case x of p => force e

ensure
equivalent
ordinary
