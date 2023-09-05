# Frontend Design
## Entity
### Keyword
* 基本 `#` を先頭につける
* 例外は以下で、いずれも一文字で一字句判定。ただし、字句解析は先頭から行われるため、モードによっては他の字句の一部となることがある
	* `[]`: implicit parameter に使用
	* `()`: tuple、named tuple に使用
	* `{}`: block、その他構文要素に使用
	* `|`: variant の区切りに使用
	* `,`: tuple、named tuple、record の区切りに使用
	* `.`: quantification の区切りに使用
	* `;`: 上記以外の区切りに使用
### Block
オブジェクト表現を持たない代わりにコントロールを埋め込めるサブルーチン。変数キャプチャ、引数渡しなどができない。
### Function
オブジェクト表現を持つサブルーチン。
### Module
### Visibility
要素 (in module, named tuple) の可視性
### Quantification
### Literal
### Data Type
### Named Tuple
複数の方による無名の名前付き順序あり直積。
### Suspension
block に対しての独自コントロール指定。
### Auto Layout
### Pattern
値への照合パターンを表現する式。パターン分岐で使用。
### View
変数を束縛しながら照合結果を計算する式。条件分岐で使用。
### Implicit Parameter
### Variant
複数の型による無名の名前付き順序なし直和。
### Record
複数の型による無名の名前付き順序なし直積。
## Idea
