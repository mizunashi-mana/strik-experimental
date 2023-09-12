ユーザが自由に定義できる、コントロール演算子。演算子とハンドラーを指定すると、いい感じにブロックのコントロール解決をしてくれるものを目指す。
```
#handle (return) {
	#handle (break = return) {
		[1,2,3].each(#handle (cont_1 = return) {
			1 #> ().cont_1
		    2 #> ().break
			3 #> i.return
			_ #> // unreachable
		})
	}

	#if True #> 1.return

	2
}: Int // => 1: Int
```
みたいな感じのが書けるようにしたい。
