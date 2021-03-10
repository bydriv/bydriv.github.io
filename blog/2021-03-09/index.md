---
url: https://bydriv.github.io/blog/2021-03-09/
title: bydriv.github.io
description: "多対多の関係を扱うデータ構造について"
thumbnail: https://bydriv.github.io/etc/site/thumbnail.png
---

# 多対多の関係を扱うデータ構造について

多くのプログラミング言語には `Map` と呼ばれるデータ構造があります。
これは **部分写像** を表すデータ構造です。
部分写像というと、いわゆる (プログラムの) 関数を想像するかもしれません。
部分写像を表現する方法はいろいろあって、そのうちのいくつかの方法として、
プログラムによる表現とデータ構造による表現があるわけですね。

```
-- 部分写像のデータ構造による表現
f :: Map Int Int
f = Map.fromList [(1, 2), (2, 3)]

-- 部分写像のプログラムによる表現
g :: Int -> Maybe Int
g 1 = Just 2
g 2 = Just 3
g _ = Nothing
```

さて部分写像というのは **関係** の一種です。
つまりある種の性質を満たす関係を部分写像と呼んでいます。

関係とは $a\in{}A$ と $b\in{}B$ のような集合の要素同士の関係性を定義するものです。
部分写像の場合は $f(a)=b$ という関係を定義しているのですね。
$A=B$ の場合もありますし $A\neq{}B$ の場合もあります。

同値関係 $a=b$ や順序関係 $a\leq{}b$ も関係です。
$c\in{}C$ のように $n$ 項関係に一般化することもでき
二項演算 $a+b=c$ や $a\times{}b=c$ のような関係もあります
(関係においては戻り値は引数との関係と考えるので、戻り値も含めると $a\in{}A,b\in{}B,c\in{}C$ のようなみっつの
集合の要素の関係を考えることになります。もちろん戻り値のない関係もあります) 。

より一般的には $aRb$ や $R\left(a,b\right)$のように記述します。

関係を直積集合として定義する場合には $G\subseteq{}A\times{}B$ として
$R=(A,B,G)$ と定め
$aRb\Leftrightarrow{}\left(a,b\right)\in{}G$ と定義します。
$G$ は **グラフ** と呼ばれます。
$A$ や $B$ という集合を考えることが関係とグラフの違いです。

さて **写像** とは **左全域的** かつ **右一意的** な関係を言います。
部分写像とは写像のうち、左全域的の条件を課さない関係です。
左全域的とはすべての $a\in{}A$ について $b\in{}B$ なる $\left(a,b\right)\in{}G$ が
（すくなくともひとつ）存在することを言います（複数あってもいい）。
右一意的とは $\left(a,b\right)\in{}G$ かつ $\left(a,c\right)\in{}G$ ならば
$b=c$ であること (つまり、それぞれの $a\in{}A$ にはたったひとつの $b\in{}B$ のみが関係すること)
を言います。

これはデータ構造の言葉で言えば、いわゆる **多対一** の関係です。
以下の例では $\left\{1,2\right\}\subseteq{}A$ が $\left\{5\right\}\subseteq{}B$ と関係していて
$\left\{3,4\right\}\subseteq{}A$ が $\left\{6\right\}\subseteq{}B$ と関係しています。

```
-- f(a) = b <=> a belongs to b
f 1 = Just 5
f 2 = Just 5
f 3 = Just 6
f 4 = Just 6
f _ = Nothing
```

(論理包含と部分集合の関係に似ていますね)

プログラムの開発には多対一だけでなくさまざまな関係を扱う必要があります。
その代表例が **グラフ** でしょう。
グラフは **頂点** 同士の関係と考えることができるわけです。
写像だけでなく一般の関係を扱うことはできるのでしょうか。

## 疎グラフと密グラフ

頂点の集合 $V$ と辺の集合 $E$ の数に着目したとき
$n=\left|V\right|,m=\left|E\right|$ とすると
$m$ の最大値は $n\times{}\left(n-1\right)$ つまり $\left|E\right|\leq{}\left|V\right|\left(\left|V\right|-1\right)$ となります。
$\left|E\right|=\left|V\right|\left(\left|V\right|-1\right)$ はすべての頂点がすべての頂点と辺でむすばれている、
つまり完全グラフの場合です。
この辺の数と頂点の数の量を比べて、辺の数が頂点の数に比して圧倒的に多いものを密グラフ、
そうでないものを疎グラフと言います。疎グラフは比較的一対一対応に近い状態で、
密グラフは多対多対応の状態です。

密グラフはほとんどすべての辺が存在するので、素朴な表のようなデータ構造で扱うのが簡単です。
つまり、以下のように $\left|V\right|\times{}\left|V\right|$ の正方行列を考えればいい：

$M=\left(\begin{array}{cccc}\bot{}&e_{12}^{}&\cdots{}&e_{1n}\\e_{21}^{}&\bot{}&\cdots{}&e_{2n}^{}\\\vdots&\vdots&\ddots&\vdots\\e_{n1}^{}&e_{n2}^{}&\cdots{}&\bot{}\end{array}\right)$

一方で疎グラフを表のようなデータ構造でもつと、
表のほとんどの部分に辺が存在しない状態になってしまい非効率的です。

ちょうど配列に対する連想配列のように、上記のような行列に対する連想行列（？）のようなデータ構造がほしいわけです。

素朴な方法として `Map (a, b) Bool` がありますが、これは
$\left(a,b\right)\in{}A\times{}B$ から検索することは高速なのですが、
$a\in{}A$ や $b\in{}A$ のみで表の行や列にまとめてアクセスすることが難しい。
`(Map a (Set b), Map b (Set a))` は計算量は申し分ないのですが、
二分木よっつの組み合わせでできており、あまり効率的に思えませんし
操作のインターフェースも猥雑です。
`type Relation a b = (Map a (Set b), Map b (Set a))` として操作を定義すれば
インターフェースは改善できますが、データ構造としてもうすこし最適なものを考えたいわけです。

関係をグラフと考える場合には $A=B=V$ として $v,u\in{}V$ について $vRu$ なることを
頂点同士が辺でむすばれていると考えます。無向グラフと有向グラフのように向きを区別する場合には
$vRu$ と $uRv$ を区別すればよいわけです。そこで多対多の関係を扱えるデータ構造を設計できれば、
自然とグラフも扱うことができるようになります。

## 二分探索に適した順序

配列に対する連想配列というアナロジーから
二分探索木を直積集合に応用すればよいことは自然な発想です。
しかしそう単純にはいかない。というのは、直積というのは線型ではないからです。

つまり整数や有理数のようなものを並べると直線になるのに対し、
直積は表 (平面) になり、 3 次元だと立体になります。
4 次元、 5 次元とどんどん増やすことはできますが、このようなものを単純に二分探索することは難しいわけです。

一方で直線状のものは二分探索で非常に効率的に扱えることから、
上記のような高次元の対象を直線状になおして扱えばよいのではないかというのが最初の直観です。

**線型代数** でうまくできないかといろいろと式をこねこねしていましたが、
そうしているうちにもっとシンプルな方法を思いつきました。

関係をある種の表としてとらえるのであれば、

$G=\left(\begin{array}{cccc}\left(a_1^{},b_1^{}\right)&\left(a_2^{},b_1^{}\right)&\cdots&\left(a_n^{},b_1^{}\right)\\\left(a_1^{},b_2^{}\right)&\left(a_2^{},b_2^{}\right)&\cdots{}&\left(a_n^{},b_2^{}\right)\\\vdots&\vdots&\ddots&\vdots\\\left(a_1^{},b_m^{}\right)&\left(a_2^{},b_m^{}\right)&\cdots{}&\left(a_n^{},b_m^{}\right)\\\end{array}\right)$

となるでしょう。

必要なことは $a\in{}A$ のみを与えられても $b\in{}B$ のみを与えられても効率的に
検索が可能なデータ構造です。たとえば $a_1^{}\in{}A$ の列のすべての関係を列挙したり、
$b_2^{}\in{}B$ の行のすべての関係を列挙したり、
あるいは $\left(a_2^{},b_2^{}\right)\in{}G$ という関係が存在するかを迅速に判定したいわけです。

さてここで $a_i^{}$ や $b_j^{}$ の **添え字** に着目します。

まず $G$ が長方形になっている場合には、正方形になる ($n=m$ になる) ように適当に拡大した表を考えます
(ほんとうにメモリ上にそういう表をつくる必要はなく、あくまで計算しやすくするため。
適当に `Nothing` とかで埋めればよい)。

$G=\left(\begin{array}{cccc}\left(a_1^{},b_1^{}\right)&\left(a_2^{},b_1^{}\right)&\cdots&\left(a_n^{},b_1^{}\right)\\\left(a_1^{},b_2^{}\right)&\left(a_2^{},b_2^{}\right)&\cdots{}&\left(a_n^{},b_2^{}\right)\\\vdots&\vdots&\ddots&\vdots\\\left(a_1^{},b_n^{}\right)&\left(a_2^{},b_n^{}\right)&\cdots{}&\left(a_n^{},b_n^{}\right)\\\end{array}\right)$

ここで $i=j$ となるような対角線の添え字に着目します。
すると

$G=\left(\begin{array}{cccc}1&&&\\&2&&\\&&\ddots&\\&&&n\\\end{array}\right)$

こうなっています。

ここに **直線** があるので、この直線を二分探索します。
この対角線のある点を $k$ とすると、その点に $ik$ 行と $kj$ 列の関係の集合
$row_k^{}=\left\{\left(a_1^{},b_k^{}\right),\left(a_2^{},b_k^{}\right),\cdots{},\left(a_i^{},b_k^{}\right)\right\}$ と
$col_k^{}=\left\{\left(a_k^{},b_1^{}\right),\left(a_k^{},b_2^{}\right),\cdots{},\left(a_k^{},b_j^{}\right)\right\}$ を
格納します。
ただし、関係をそのまま格納すると重複が生まれるので、実際には
$A_k^{}=\left\{a_1^{},a_2^{},\cdots{},a_i^{}\right\}$ と
$B_k^{}=\left\{b_1^{},b_2^{},\cdots{},b_j^{}\right\}$ を
格納すればじゅうぶんです。 たとえば $\left(a_k^{},b_j^{}\right)$ を格納する場合
$a_k^{}\in{}A_j^{}$ となり、 $a_k^{}$ と $b_j^{}$ が別々の場所に分解されて格納されることに注意してください。

つまり上述の $\left(n,n\right)$ 行列と同値なデータ表現として、
次のような $\left(n,3\right)$ 行列に変形します。

$G=\left(\begin{array}{ccc}1&A_1^{}&B_1^{}\\2&A_2^{}&B_2^{}\\\vdots&\vdots&\vdots&\\n&A_n^{}&B_n^{}\end{array}\right)$

このようにすればもはや長さ $n$ の配列と同じように扱え、
単純な二分木として実装が可能になるというわけです。

関係 $\left(a,b\right)$ を追加したとき、 $a$ と $b$ とが一ヶ所に格納されることが重要です。

ポインタであれば 8 byte のコピーで済むのですが、一ヶ所にあるということは単純にコピーしても問題ない
ということなので、たとえば Rust などの言語では Rc を使わずに済んだり、
データベースの永続化時などには有利です。

## より基礎的な話題

さて上記のようなデータ表現のより基礎的な話題について考えてみます。

まず $1,2,\cdots,n$ という添え字になっている対角線は $i=j$ となる部分で、 $i$ は $a_i^{}\in{}A$ の、
$j$ は $b_j^{}\in{}B$ の添え字にそれぞれなっています。
この **添字集合** を考えると $i\in{}I,j\in{}J$ となっています。
そこで $I=J$ であること、
$I$ は **全順序集合** であること、
そして $A,B,I$ のあいだに以下の関係が満たされることを公理と考えます。

- $\exists{}f:A\rightarrow{}I.\forall{}a_i^{},a_j^{}\in{}A.a_i^{}=a_j^{}\Leftrightarrow{}f\left(a_i^{}\right)=f\left(a_j^{}\right)$
- $\exists{}g:B\rightarrow{}I.\forall{}b_i^{},b_j^{}\in{}B.b_i^{}=b_j^{}\Leftrightarrow{}g\left(b_i^{}\right)=g\left(b_j^{}\right)$

印象的には、 $A,B$ と $I$ が一対一の関係にある、
つまり $A,B$ の要素と $I$ の要素を自然に同一視できる
(コンピュータの言葉で言えばバイナリにエンコーディングできる) ということです。

もし $A,B$ が **高々可算** の集合であれば $\mathbb{N}$ への単射が存在するので、
$I=\mathbb{N}$ と置けば上記の関係は自明に満たされます。

### $n$ 次元への拡張

$3$ 次元での表でも同様の表現が可能です。
(必要な集合は $AB_k^{}\subseteq{}A\times{}B$, $AC_k^{}\subseteq{}A\times{}C$, $BC_k^{}\subseteq{}B\times{}C$ のみっつ)。

$G=\left(\begin{array}{cccc}1&AB_1^{}&AC_1^{}&BC_1^{}\\2&AB_2^{}&AC_2^{}&BC_2^{}\\\vdots&\vdots&\vdots&\\n&AB_n^{}&AC_n^{}&BC_n^{}\end{array}\right)$

ここでさらに $AB_k^{}$ などを上述のような表現で再帰的に定義することもできます。

## ハッシュ値の場合

ハッシュ関数のような場合一対一の関係にはならないかもしれません。
その場合には添字集合をすこし工夫すればよいのです。
つまり

```
type A = String
type B = String
type I = (Hash, Either A B)
```

などとすればよろしい。
一意性はタプルの右の要素によって担保されていて、
また辞書順であれば、ほとんどの比較はハッシュ値の比較のみで済むので高速です。

もちろん実際に実装する場合には、添え字が重複を許可するように実装することも考えられます。

## 通常の Map での表現

```
type R i a b = Map i (Set a, Set b)
```

のようにすれば通常の `Map` でも同様の表現は可能です。

添字集合は自然数である必要はありません。
`R Integer` でも問題ありませんし、 `R (Either a b)` でもいいわけです。

(後者の表現はおそらくもっともナイーブ)。

うまく型をつけることで `Left x` から `(Set.empty, ys)` が返ってくること、
`Right y` から `(xs, Set.empty)` が返ってくることは表現できるかもしれません。

## 実装

集合を `[a]` や `[b]` ではなく二分木にしたり、
平衡二分木化も検討中ですがとりあえずのプロトタイプ。

```
module Relation
  ( R, Relation
  , empty
  , insert, selectLeft, selectRight, betweenLeft, betweenRight
  , insertBy, selectLeftBy, selectRightBy, betweenLeft, betweenRight ) where

import qualified Data.Char  as Char
import qualified Data.Int   as Int
import qualified Data.List  as List
import qualified Data.Ratio as Ratio
import qualified Data.Word  as Word

class Finite a where
  count :: a -> Integer

class Countable a where
  injection :: a -> Rational

type Relation = R Rational

data R i a b =
    Empty
  | Node i [a] [b] (R i a b) (R i a b)
  deriving (Read, Show)

--------------------------------------------------------------------------------

empty :: R i a b
empty =
  Empty

--------------------------------------------------------------------------------

insert :: (Countable a, Countable b) => a -> b -> Relation a b -> Relation a b
insert =
  insertBy injection injection

selectLeft :: (Countable a, Countable b) => a -> Relation a b -> [b]
selectLeft =
  selectLeftBy injection

selectRight :: (Countable a, Countable b) => b -> Relation a b -> [a]
selectRight =
  selectRightBy injection

betweenLeft :: (Countable a, Countable b) => a -> a -> Relation a b -> [b]
betweenLeft =
  betweenLeftBy injection

betweenRight :: (Countable a, Countable b) => b -> b -> Relation a b -> [a]
betweenRight =
  betweenRightBy injection

--------------------------------------------------------------------------------

insertBy :: Ord i => (a -> i) -> (b -> i) -> a -> b -> R i a b -> R i a b
insertBy f g x y r =
  insertAt f g (f x) [] [y] (insertAt f g (g y) [x] [] r)

selectLeftBy :: Ord i => (a -> i) -> a -> R i a b -> [b]
selectLeftBy f x =
  snd . lookupAt (f x)

selectRightBy :: Ord i => (b -> i) -> b -> R i a b -> [a]
selectRightBy g y =
  fst . lookupAt (g y)

betweenLeftBy :: Ord i => (a -> i) -> a -> a -> R i a b -> [b]
betweenLeftBy f x x' =
  concat . snd . between (f x) (f x')

betweenRightBy :: Ord i => (b -> i) -> b -> b -> R i a b -> [a]
betweenRightBy f x x' =
  concat . fst . between (f x) (f x')

--------------------------------------------------------------------------------

compareBy :: Ord i => (a -> i) -> a -> a -> Ordering
compareBy f x x' =
  compare (f x) (f x')

unionBy :: Ord i => (a -> i) -> [a] -> [a] -> [a]
unionBy =
  foldr . List.insertBy . compareBy

insertAt ::
  Ord i => (a -> i) -> (b -> i) -> i -> [a] -> [b] -> R i a b -> R i a b
insertAt f g i xs ys Empty =
  Node i xs ys Empty Empty
insertAt f g i xs ys (Node j xs' ys' lhs rhs) =
  case compare i j of
    LT ->
      Node j xs' ys' (insertAt f g i xs ys lhs) rhs
    EQ ->
      Node j (unionBy f xs' xs)  (unionBy g ys' ys) lhs rhs
    GT ->
      Node j xs' ys' lhs (insertAt f g i xs ys rhs)

lookupAt :: Ord i => i -> R i a b -> ([a], [b])
lookupAt _ Empty =
  ([], [])
lookupAt i (Node j xs ys lhs rhs) =
  case compare i j of
    LT ->
      lookupAt i lhs
    EQ ->
      (xs, ys)
    GT ->
      lookupAt i rhs

between :: Ord i => i -> i -> R i a b -> ([[a]], [[b]])
between _ _ Empty =
  ([], [])
between i j (Node k xs ys lhs rhs)
  | i > k && k >= j =
      ([], [])
  | i <= k && k < j =
      let
        (xss, yss) = between i j lhs
        (xss', yss') = between i j rhs
      in
        (xss ++ xs : xss', yss ++ ys : yss')
  | i <= k && k >= i =
      between i j lhs
  | i > k && k < i =
      between i j rhs

--------------------------------------------------------------------------------

instance Finite Char where
  count _ =
    toInteger (Char.ord maxBound)

instance Finite Int where
  count _ =
    toInteger (maxBound :: Int) - toInteger (minBound :: Int)

instance Finite Int.Int8 where
  count _ =
    toInteger (maxBound :: Int.Int8) - toInteger (minBound :: Int.Int8)

instance Finite Int.Int16 where
  count _ =
    toInteger (maxBound :: Int.Int16) - toInteger (minBound :: Int.Int16)

instance Finite Int.Int32 where
  count _ =
    toInteger (maxBound :: Int.Int32) - toInteger (minBound :: Int.Int32)

instance Finite Int.Int64 where
  count _ =
    toInteger (maxBound :: Int.Int64) - toInteger (minBound :: Int.Int64)

instance Finite Word.Word where
  count _ =
    toInteger (maxBound :: Word.Word)

instance Finite Word.Word8 where
  count _ =
    toInteger (maxBound :: Word.Word8)

instance Finite Word.Word16 where
  count _ =
    toInteger (maxBound :: Word.Word16)

instance Finite Word.Word32 where
  count _ =
    toInteger (maxBound :: Word.Word32)

instance Finite Word.Word64 where
  count _ =
    toInteger (maxBound :: Word.Word64)

--------------------------------------------------------------------------------

instance Countable Char where
  injection = toRational . Char.ord

instance Countable Int where
  injection = toRational

instance Countable Int.Int8 where
  injection = toRational

instance Countable Int.Int16 where
  injection = toRational

instance Countable Int.Int32 where
  injection = toRational

instance Countable Int.Int64 where
  injection = toRational

instance Countable Word.Word where
  injection = toRational

instance Countable Word.Word8 where
  injection = toRational

instance Countable Word.Word16 where
  injection = toRational

instance Countable Word.Word32 where
  injection = toRational

instance Countable Word.Word64 where
  injection = toRational

--------------------------------------------------------------------------------

instance Countable Integer where
  injection = toRational

instance Integral a => Countable (Ratio.Ratio a) where
  injection = toRational

instance (Finite a, Countable a) => Countable [a] where
  injection [] =
    0
  injection (x : xs) =
    injection x + injection xs * toRational (count x) + 1
```
