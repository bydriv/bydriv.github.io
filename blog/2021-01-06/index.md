---
url: https://bydriv.github.io/blog/2021-01-06/
title: bydriv.github.io
description: "プランク定数と光のエネルギーについて"
thumbnail: https://bydriv.github.io/etc/site/thumbnail.png
---

# プランク定数と光のエネルギーについて

- *[特殊相対性理論について](../2021-01-04)*
- *[プランク単位系における質量と速度の反比例関係について](../2021-01-05) の続き*

まず得られている式を整理します。
まず仮定しているのは $E^2=\left(mc^2\right)^2+\left(pc\right)^2$ という式だけです。
それぞれの変数の定義については *[特殊相対性理論について](../2021-01-04)* という記事をご覧ください。
この式は特殊相対性理論の結論として得られる式ですので、以下の式は特殊相対性理論を前提とすれば常に成り立ちます：

- $E^2=\left(mc^2\right)^2+\left(pc\right)^2\Leftrightarrow{}E=\frac{c^2}{E}m^2c^2+\frac{E}{c^2}\left|\vec{v}\right|^2$
- $E^2=\left(mc^2\right)^2+\left(pc\right)^2\Leftrightarrow{}E^2c^2=m^2c^6+\left|\vec{v}\right|^2E^2$
- $\epsilon=E^2=c^2\Rightarrow\left(E^2=\left(mc^2\right)^2+\left(pc\right)^2\Leftrightarrow\epsilon^2=m^2\epsilon^3+\left|\vec{v}\right|^2\epsilon\right)$

うえのふたつは単なる式変形を繰り返しただけの $E^2=\left(mc^2\right)^2+\left(pc\right)^2$ と完全に同値な命題で、
$E^2=\left(mc^2\right)^2+\left(pc\right)^2$ が成り立つかぎり常に成り立ちます。
いちばん下は二番目の式に $\epsilon=E^2=c^2$ と代入した結果ですので特殊相対性理論と矛盾なく成り立ちますが、
$E^2=\left(mc^2\right)^2+\left(pc\right)^2$ と同値であるとは限りません。
$E^2=\left(mc^2\right)^2+\left(pc\right)^2$ が成り立っても $\epsilon^2=m^2\epsilon^3+\left|\vec{v}\right|^2\epsilon$ が成り立たないような物質はいくらでも考えられます。
最終的には $\epsilon^2=m^2\epsilon^3+\left|\vec{v}\right|^2\epsilon$ という式で
$E^2=\left(mc^2\right)^2+\left(pc\right)^2$ が成り立つものをすべて説明しなければなりませんが、
それは今回はひとまず置いておきましょう。

ただ $E = c$ という等式はすこし考えればおかしいことは容易にわかります。
そもそもエネルギーと速度というのはまったく次元の違う物理量です。
ですがそもそも $c$ という定数は単なる速度の上限ではなくもっと根源的な
エネルギーの上限によって定められているのではないかという直観を話の出発点としていますので、
次元のことはこの際忘れて単なる数値として見てしまいましょう。
次元は最後に揃えてやればいいのです。
そして次元を揃えたことによって $E^2=\left(mc^2\right)^2+\left(pc\right)^2$ が成り立つものを
すべて説明できて初めて $E=c$ という等式の意味するところがわかるわけですが、
それが難しいことは明らかですので、それはまた今度にします。

$\epsilon^2=m^2\epsilon^3+\left|\vec{v}\right|^2\epsilon$ という式に関する明らかな事実として：

- $m=0$ ならば $\left|\vec{v}\right|=c$
- $\left|\vec{v}\right|=0$ ならば $m=c^{\text{-}1}$

が系として従います。ここで $c^{\text{-}1}=\frac{1}{c}$ です。
これは実際に代入してみれば確かめられます。なんとなれば左辺が定数になっていて、
動ける変数は $m$, $\left|\vec{v}\right|$ のみなので一方が固定されたならば他方もまた一意に決まってしまうからです。
実際前回の記事で示した以下の式

- $E=mc^2$
- $E=\frac{E}{c^2}\left|\vec{v}\right|^2=p\left|\vec{v}\right|$

これは $E=c$ を前提としていますので、結局

- $c=mc^2$
- $c=c^{\text{-}1}\left|\vec{v}\right|^2$

と同じことだからです。これを満たす $m$ は $m=c^{\text{-}1}$ しかありませんし、
$\left|\vec{v}\right|$ もまた $\left|\vec{v}\right|=c$ しかありません。

後者は要するに光子のことなのでいいのですが、前者は物理的にどう解釈すべきでしょうか？　素直に式をそのまま読むならば、
エネルギーの総和が $c$ でかつ $\left|\vec{v}\right|=0$ ならば $m=c^{\text{-}1}$ という
式は、静止している物体は $c^{\text{-}1}$ 以上に重くも軽くもなれないということです。
これはエネルギー保存則があるので自然なことです。なぜならそのエネルギーは相を変えても保存されなければ
なりませんが、静止している以上そのエネルギーは質量に変えて保存するしかありません。

またこの式は質量に上限があることを示しています。
つまり $m$ は $\left[0,c^{\text{-}1}\right]$ の区間しか動けないわけですね（質量も速度も $0$ 以上であると仮定するならば）。
また $\left|\vec{v}\right|$ は $\left[0,c\right]$ の区間を動けますが、やはり上限として $c$ というものは
あるわけで、これは光速度不変の原理と同じですね。ただ違いは質量にも上限があるという式になっているということです。
質量に上限などあるのでしょうか？　自然界においてどうであるかはいまは忘れて式のみに集中しましょう。

さて $c$ は定数なので計算できるのですが、 $c^{\text{-}1}$ を実際に計算してみれば極めて小さな値であることは明らかです。
質量は次元さえ揃えてやればいくらでも小さな値で表せる (たとえば $1000 g$ は $1 kg$ と表せる) ので
$c^{\text{-}1}$ という値の次元はいまは置いておきます。
どちらかというとこの値とほかの物理定数の関係が知りたくなります。たとえば $c^{\text{-}1}$ はだいたい $\frac{h\times10^25}{2}$ くらいです。
ここで $h$ はプランク定数で $h=62607015\times10^{\text{-}34}$ がその定義値です。

さてなぜ光にエネルギーがあるのか？　という疑問に関して特殊相対性理論は、
質量はなくとも光には速度があるからという解答を与えてくれます。一方でその具体的な値に関しては簡単に
計算して求められるものでもないようです。量子力学においては、光のエネルギーについて以下のように説明されます：

- $E=h\nu$

ここで

- $h$ はプランク定数
- $\nu=\frac{c}{\lambda}$ は振動数 *(周波数)*
- $c$ は光速度定数
- $\lambda=\frac{2\pi}{k}$ は波長
- $\pi$ は円周率
- $k$ は波数

です。ここで $\nu$ と $\left|\vec{v}\right|$ の違いに注意してください。
前者はギリシア文字の $\nu$, 後者はラテン文字の $v$ です。
前回までの記事では $\left|\vec{v}\right|$ を簡単のためラテン文字で $v$ とも表記しましたが
誤解を避けるため今後は常に $\left|\vec{v}\right|$ と表記することにします。

またこの式はほとんど定数で構成されていますから動ける変数は $k$ だけです。
また $m=1$ かつ $\left|\vec{v}\right|=0$ かつ $\nu=\frac{c^2}{h}$ であるとき、以下の等式が成り立ちます：

- $mc^2=h\nu$

このとき $\nu=\frac{c^2}{h}=\frac{c}{hc^{\text{-}1}}=\frac{c}{\lambda}$ ですから $\lambda=hc^{\text{-}1}$ です。
また $k=\frac{2\pi}{\lambda}$ の関係にあるので $k$ も計算で求められます。

このとき左辺は静止質量を表していて右辺は光のエネルギー、
つまり (光の質量が $0$ であると仮定するならば) $m=0$ かつ $\left|\vec{v}\right|=c$ であるような
物質のエネルギーを表しています。よって $\left|\vec{v}\right|=0$ を代入したとき

- $E=mc^2$

が得られ $\left|\vec{v}\right|=c$ を代入したとき

- $E=h\nu$

が得られるような式を得るのが第一歩の目標です。

*ところで一般相対性理論と量子力学というのは包含関係にあるような体系ではなく、
ちょうど相対論が古典力学と電磁気学をうまくとりまとめて説明したように、
相対論と量子力学をうまく橋渡しするような理論はいまだ研究途上のようです。
量子重力理論の候補はいろいろあるみたいですがいまのところは研究課題という扱いのようです。*

*今回は簡単のため特殊相対性理論で考えていますが、
要するにこういう式を統一的に扱える式を探求するというのは量子重力理論の研究にほかならないわけですね。
物理学の最重要課題とまで言われるのであれば素人が考えたところで容易に結論が出るとも思えませんし、
このふたつだけを説明したところで世のなかにはもっといろいろと難しい物理現象はあるわけです。
ただすくなくともいまだ結論の出ていない分野なのであれば、
そのものずばりという答えがどの本を読めばわかるというものでもありませんし趣味的な研究課題として
おもしろいのではないのでしょうか。*

まず定義により $E=c$ と定めたのですから $m=1$ と代入できないことは明らかです。
そこでまず特殊な場合として $m=c^{\text{-}1}$ と代入した場合に

- $mc^2=h\nu$

の等式が成り立つ関係を考えてみます。 $mc^2=c$ なので $h\nu=c$ でなければなりません。
これは明らかで $k=\frac{2\pi}{h}=\hbar^{\text{-}1}$ のときのみです。
ここで $\hbar=\frac{h}{2\pi}$ はディラック定数で $k$ はその逆数とわかります。
このとき $\lambda=h$ かつ $\nu=\frac{c}{h}$ です。

また $\left|\vec{v}\right|=c$ と代入した場合でも同様です。なんとなれば

- $mc^2=c^{\text{-}1}\left|\vec{v}\right|^2=h\nu$

だからです。

よって

- $mc^2=c^{\text{-}1}\left|\vec{v}\right|^2=h\nu=h\frac{c}{h}=c$

の等式が成り立ちます。

しかしこれは特殊な場合ですので $c=h\nu$ となるような $\nu$ が存在することは明らかです。
今度はこれを $E=c$ を前提とする一般項

- $\epsilon^2=m^2\epsilon^3+\left|\vec{v}\right|^2\epsilon$

で説明しなければなりません。

- $E=h\nu$

という式を見るとエネルギーは周波数に比例して際限なく上昇していきます。
そこでもしエネルギーに上限があるのであれば周波数にも上限があるのではないかとは
自然な推測になります。 $E=c$ をエネルギーの上限として定めたのですから、
$c=h\nu$ となるような $\nu$ が周波数の上限なのではないかと考えてみましょう。
そうなると自然と

- 周波数の上限 $\nu=\frac{c}{\lambda}=\frac{c}{h}$
- 波長の下限 $\lambda=\frac{2\pi}{k}=h$
- 波数の上限 $k=\frac{2\pi}{\lambda}=\frac{2\pi}{h}=\hbar^{\text{-}1}$

と考えることはできます。波長の場合はそれ以上小さくなると周波数が大きくなるので
下限になります。

さてエネルギーの単位として $\epsilon=E^2=c^2$ と定めたのでした。
そこでまずこの単位で揃えて表現できないか考えてみます。

- $E=h\nu=h\frac{c}{\lambda}=h\frac{1}{\lambda}c=h\frac{1}{\frac{2\pi}{k}}c=h\frac{k}{2\pi}c=k\frac{h}{2\pi}c=k\hbar{}c$

ここまではすんなり変形できました。しかもこの式は $E=c$ を仮定していないので $E=h\nu$ と完全に同値な命題です。
つまり

- $E=h\nu\Leftrightarrow{}E=k\hbar{}c$

だと言えます。

また前回はたまたま指数が $2$ の倍数だったので $\epsilon=c^2$ と定義しましたが今回は
指数ではありません。こうなると $\sqrt{\epsilon}=c$ のほうを基本単位と定義を変えたくなるものの、
ひとまず定義は変えずに単位を揃えてしまいます。すると

- $\epsilon=E^2=\left(k\hbar{}c\right)^2=k^2\hbar^2c^2=k^2\hbar^2\epsilon$
- $\epsilon=k^2\hbar^2\epsilon$

という等式が得られます。以前の式と並べてみるとよく似ていることがわかります。

- $\epsilon^2=m^2\epsilon^3+\left|\vec{v}\right|^2\epsilon$
- $\epsilon=k^2\hbar^2\epsilon$

この式をうまいこと合体させたいわけですが、そんなに単純な話ではありません。
$m$, $\left|\vec{v}\right|$ という変数の動ける範囲を変えたくないわけです。
さらに後者の式は $k$ が完全に固定されていて動けません。
ただ前者の式と合体させると動けるようになります。
この変数の動ける範囲についてきちんと考えたうえで合体させなければなりません。

しかしここまでくるとおもしろいことに気がつきます。
というのも、前回示した以下の式

- $E^2c^2=m^2c^6+\left|\vec{v}\right|^2E^2$

この $E^2$ の部分は実のところ速度にしかかかっていません。
そこで前回はとりあえず単位を揃えるために $c^2$ を代入したのですが、
ここにほかのなにかを代入できないと考えてしまいます。

しかしここに単純に $k\hbar{}c$ を代入してもやはりあまりうまくいかないことも
試してみるとすぐにわかります。ただここまできたらあともう一歩な感じはします。
とりあえず今日はこのへんで。

## 付録 A 公理、定理、定義の整理

### 公理

- $E^2=\left(mc^2\right)^2+\left(pc\right)^2$
- $E=h\nu$

### 定数

- $c$: 光速度定数 $c=299792458$
- $h$: プランク定数 $h=62607015\times10^{\text{-}34}$
- $\hbar$: ディラック定数 $\hbar=\frac{h}{2\pi}$
- $\pi$: 円周率

### 変数

- $E$: エネルギー
- $m$: 質量
- $p$: 運動量の大きさ $p=\frac{E}{c^2}\left|\vec{v}\right|=\left|\frac{E}{c^2}\vec{v}\right|$
- $\left|\vec{v}\right|$: 3 次元速度ベクトルの大きさ $\left|\vec{v}\right|=\left|\left(v_x,v_y,v_z\right)\right|$
- $\nu$: 振動数 *(周波数)* $\nu=\frac{c}{\lambda}$
- $\lambda$: 波長 $\lambda=\frac{2\pi}{k}$
- $k$: 波数 $k=\frac{2\pi}{\lambda}$

### 定義

- $\epsilon=c^2$

### 定理

- $E^2=\left(mc^2\right)^2+\left(pc\right)^2\Leftrightarrow{}E=\frac{c^2}{E}m^2c^2+\frac{E}{c^2}\left|\vec{v}\right|^2$
- $E^2=\left(mc^2\right)^2+\left(pc\right)^2\Leftrightarrow{}E^2c^2=m^2c^6+\left|\vec{v}\right|^2E^2$
- $E=h\nu\Leftrightarrow{}E=k\hbar{}c$
- $\epsilon=E^2=c^2\Rightarrow\left(E^2=\left(mc^2\right)^2+\left(pc\right)^2\Leftrightarrow\epsilon^2=m^2\epsilon^3+\left|\vec{v}\right|^2\epsilon\right)$
- $\epsilon=E^2=c^2\Rightarrow\left(E=h\nu\Leftrightarrow\epsilon=k^2\hbar^2\epsilon\right)$

## 付録 B 検算プログラム

基本的に文字式を考えるときは数式のほうが簡単ですが文字式ではなく実際の値を求める場合などには
プログラムで検算したほうが簡単です。 $\pi$ は無理数ですが近似値で計算すればいいので
今回は有理数で表しました。 $h=62607015\times10^{\text{-}34}=\frac{62607015}{10^34}$ に注意してください。
Haskell では `h = 62607015 * (10 ** (-34))` と表記することもできますが
これは `Floating` にしか使えないので逆数で定義しました。

```
module Lemma.Physics where

-- pi ~ pi'
pi' :: Rational
pi' = toRational pi

-- Speed of light
c :: Rational
c = 299792458

-- Planck constant
h :: Rational
h = 6.62607015 / (10 ^ 34)

-- Dirac's constant
h' :: Rational
h' = h / (2 * pi')

-- E = mc^2
emc2 :: Rational -> Rational
emc2 m = m * (c ^ 2)

-- E = pc
epc :: Rational -> Rational -> Rational
epc e v = (e / (c ^ 2)) * v * c

-- E = hv
ehv :: Rational -> Rational
ehv k = h * (c / ((2 * pi') / k))

-- E = kh'c
ekh'c :: Rational -> Rational
ekh'c k = k * h' * c
```