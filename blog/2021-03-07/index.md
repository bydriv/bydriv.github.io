---
url: https://bydriv.github.io/blog/2021-03-07/
title: bydriv.github.io
description: "行列演算ライブラリを実装してみる"
thumbnail: https://bydriv.github.io/etc/site/thumbnail.png
---

# 行列演算ライブラリを実装してみる

いろいろあって行列を真剣に頑張りたくなったので実装してみています。

## 動機

正方行列を対角化して爆速なアルゴリズムを実装したい（？）

## 今回の目標

正方行列を対角化するアルゴリズムを (とりあえず CPU で) 動くところまでを目指します。
行列演算ということで GPGPU でいろいろ頑張ることもできるかもしれませんが、ひとまず。

## 数学的な準備

今回実装するのに使う主な数学を軽く説明します。

### 可換環

スカラーの操作に使います。乗法と加法が定義されている体系です。

*集合と演算の存在*:

- $a,b,c,\cdots{}\in{}R$
- $+:R\times{}R\rightarrow{}R$
- $\times{}:R\times{}R\rightarrow{}R$

*加法*:

- $a+b=b+a$ (交換律)
- $\left(a+b\right)+c=a+\left(b+c\right)$ (結合律)
- $a+0=0+a=a$ (単位元の存在)
- $a+\left(\text{-}a\right)=\left(\text{-}a\right)+a=0$ (逆元の存在)

*乗法*:

- $a\times{}b=b\times{}a$ (交換律)
- $\left(a\times{}b\right)\times{}c=a\times{}\left(b\times{}c\right)$ (結合律)
- $a\times{}1=1\times{}a=a$ (単位元の存在)

*分配法則*:

- $a\times{}\left(b+c\right)=\left(a\times{}b\right)+\left(a\times{}c\right)$
- $\left(a+b\right)\times{}c=\left(a\times{}c\right)+\left(b\times{}c\right)$

### 可換体

可換環に加えて乗法逆元が存在します。

- $a,a_{}^{\text{-}1}\neq{}0\Rightarrow{}a\times{}a_{}^{\text{-}1}=a_{}^{\text{-}1}\times{}a=1$ (逆元の存在)

### ベクトル

ベクトルは

$\vec{v}=\left(a_1^{},a_2^{},\cdots{},a_n^{}\right)$

というように $n$ 個のなにかを並べたものです。
$n$ をベクトルの次元と言います。

今回は $a_1^{},a_2^{},\cdots{},a_n^{}\in{}R$ を上述の可換環・可換体の集合 $R$ の要素とします。

### ベクトルの内積

同じ次元のベクトルの内積は

$\vec{v}\vec{u}=\left(a_1^{},a_2^{},\cdots{},a_n^{}\right)\left(b_1^{},b_2^{},\cdots{},b_n^{}\right)=a_1^{}b_1^{}+a_2^{}b_2^{}+\cdots{}+a_n^{}b_n^{}$

と定義されます。

### 行列

行列は

$A=\left(\begin{array}{cccc}a_{11}^{}&a_{12}^{}&\cdots{}&a_{1n}^{}\\a_{21}^{}&a_{22}^{}&\cdots{}&a_{2n}^{}\\\vdots{}&\vdots{}&\ddots{}&\vdots{}\\a_{m1}^{}&a_{m2}^{}&\cdots{}&a_{mn}^{}\end{array}\right)$

というように $n\times{}m$ 個のなにかを矩形に並べたものです。
このような行列を $\left(m,n\right)$ 行列などと言います
(多くのプログラミング言語やライブラリとは座標系が異なりがちなことに注意)。

ここで並べたものを行列の成分と言います。行列の成分は数とは限りません。
今回は上述の可換環や可換体になるなにかを成分とします。

行列のうち横並びの部分を行、縦並びの部分を列と呼びます。
行や列をベクトルと見る場合

- $\vec{v}_i^{}=\left(a_{i1}^{},a_{i2}^{},\cdots{},a_{in}^{}\right)$
- $\vec{u}_j^{}=\left(a_{1j}^{},a_{2j}^{},\cdots{},a_{mj}^{}\right)$

などと置いて

$A=\left(\begin{array}{c}\vec{v}_1^{}\\\vec{v}_2^{}\\\vdots{}\\\vec{v}_m^{}\end{array}\right)$

$B=\left(\begin{array}{cccc}\vec{u}_1^{}&\vec{u}_2^{}&\cdots{}&\vec{u}_n^{}\end{array}\right)$

と行列を表現する場合もあります。行列には縦と横の区別がありますがベクトルにはありません。

### 行列の積

行列の積はベクトルの内積で定義されます。

$AB=\left(\begin{array}{c}\vec{v}_1^{}\\\vec{v}_2^{}\\\vdots{}\\\vec{v}_m^{}\end{array}\right)\left(\begin{array}{cccc}\vec{u}_1^{}&\vec{u}_2^{}&\cdots{}&\vec{u}_n^{}\end{array}\right)=\left(\begin{array}{cccc}\vec{v}_1\vec{u}_1&\vec{v}_1\vec{u}_2&\cdots{}&\vec{v}_1\vec{u}_n\\\vec{v}_2\vec{u}_1&\vec{v}_2\vec{u}_2&\cdots{}&\vec{v}_2\vec{u}_n\\\vdots{}&\vdots{}&\ddots{}&\vdots{}\\\vec{v}_m\vec{u}_1&\vec{v}_m\vec{u}_2&\cdots{}&\vec{v}_m\vec{u}_n\\\end{array}\right)$

ここで $A$ は $\left(m,k\right)$ 行列かつ $B$ は $\left(k,n\right)$ 行列でなければなりません。
$k$ は同じであれば具体的な数は問いません。

### 小行列

行列から特定の行や列を取りのぞいて得られる行列を小行列と言います。

### 正方行列

行列のうち $n=m$ となるようなものを $n$ 次元正方行列と言います。

### 行列式

正方行列に対して行列式 $det\left(A\right)$ は次のように帰納的に定義されます。

$det\left(a_{11}\right)=a_{11}$

$det\left(\begin{array}{cccc}a_{11}^{}&a_{12}^{}&\cdots{}&a_{1n}^{}\\a_{21}^{}&a_{22}^{}&\cdots{}&a_{2n}^{}\\\vdots{}&\vdots{}&\ddots{}&\vdots{}\\a_{n1}^{}&a_{n2}^{}&\cdots{}&a_{nn}^{}\end{array}\right)=a_{11}^{}\left(\text{-}1\right)^\left(1+1\right)det\left(\begin{array}{cccc}a_{22}^{}&a_{23}^{}&\cdots{}&a_{2n}^{}\\a_{32}^{}&a_{33}^{}&\cdots{}&a_{3n}^{}\\\vdots{}&\vdots{}&\ddots{}&\vdots{}\\a_{n2}^{}&a_{n3}^{}&\cdots{}&a_{nn}^{}\end{array}\right)+a_{21}^{}\left(\text{-}1\right)^\left(2+1\right)det\left(\begin{array}{cccc}a_{12}^{}&a_{13}^{}&\cdots{}&a_{1n}^{}\\a_{32}^{}&a_{33}^{}&\cdots{}&a_{3n}^{}\\\vdots{}&\vdots{}&\ddots{}&\vdots{}\\a_{n2}^{}&a_{n3}^{}&\cdots{}&a_{nn}^{}\end{array}\right)+\cdots+a_{n1}^{}\left(\text{-}1\right)^\left(n+1\right)det\left(\begin{array}{cccc}a_{12}^{}&a_{13}^{}&\cdots{}&a_{1n}^{}\\a_{22}^{}&a_{23}^{}&\cdots{}&a_{2n}^{}\\\vdots{}&\vdots{}&\ddots{}&\vdots{}\\a_{\left(n-1\right)2}^{}&a_{\left(n-1\right)3}^{}&\cdots{}&a_{\left(n-1\right)n}^{}\end{array}\right)$

後者の定義で $a_{ij}^{}$ の $i+j$ の偶奇によって $\left(\text{-}1\right)^\left(i+j\right)$ 符号が変化し、また最初の列と $i$ 行目を取りのぞいた小行列の行列式を再帰的に求めます。

(計算効率などの都合で行列式の定義にはいろいろな流派があります。これは余因子展開と呼ばれるものです)。

### 余因子

正方行列 $A$ のおのおのの $i,j$ に対して 余因子 $M_{i,j}^{}$ とは、
$A$ から $i$ 行と $j$ 列を取りのぞいて得られる小行列 $B$ の行列式に偶奇の符号をかけたもの $M_{i,j}^{}=\left(\text{-}1\right)^\left(i+j\right)det(B)$ と定義されます。

（今回は行列式の定義の都合で行列式の定義内に余因子に似た部分式が出現していますが、行列式自体はほかの定義もできます）

### 余因子行列

正方行列 $A$ のおのおのの $i,j$ に対して 余因子 $M_{i,j}^{}$ を $\left(i,j\right)$ 成分とする行列の転置行列を余因子行列と言い
$adj\left(A\right)$ と表します。

### 単位行列

正方行列のうち (同じ次元の) 任意の正方行列 $A$ について

$AI=IA=A$

となるような正方行列 (正方行列の積の単位元) $I$ を単位行列と言います。 $I$ は

$I=\left(\begin{array}{ccc}1&0&\cdots{}&0\\0&1&\cdots{}&0\\\vdots{}&\vdots{}&\ddots{}&\vdots{}\\0&0&\cdots{}&1\end{array}\right)$

というように対角線部分が $1$, それ以外が $0$ であるような正方行列として表せます。

$a_{11}^{},a_{22}^{},\cdots{},a_{nn}^{}=1$ というように添え字が $i=j$ となるような部分を $1$ で埋めたものです。

### 正則行列

正方行列のうち行列の積の逆元が存在するものを正則行列と言います。つまり

$AA^{\text{-}1}=A^{\text{-}1}A=I$

となるような $A^{\text{-}1}$ が存在するとき、かつそのときにかぎり、 $A$ は正則行列です。

$A^{\text{-}1}$ が存在しない正方行列はたくさんあります。

$A$ が正則行列であるのは、 $det\left(A\right)\neq{}0$ であるとき、かつそのときにかぎります。
ゆえに $det\left(A\right)\neq{}0$ か否かで $A$ が正則行列か否か ($A^{\text{-}1}$ が存在するか否か) を判定することができます。

$A$ から $A^{\text{-}1}$ を計算で求めるには乗法逆元が必要です。
$det\left(A\right)$ の乗法逆元 $det\left(A\right)^{\text{-}1}$ で表せば

$A^{\text{-}1}=det\left(A\right)^{\text{-}1}adj\left(A\right)$

で $A^{\text{-}1}$ を計算（プログラム）で求めることができます。

### 正方行列の対角化

正方行列 $A$ を対角化するには、正則行列 $P$ を *適当に* 選んで

$D=P^{\text{-}1}AP$

とします。

この $P$ を *適当に* 選ぶには固有値に関する方程式を解く必要があります。

ここまでをプログラムとして実装するのが今回の目標ですが、固有値と固有ベクトルについても軽く触れておきます。

### 固有値と固有ベクトル

$A$ の固有値 $\lambda\in{}R$ と固有ベクトル $\vec{v}$ とは

$A\vec{v}=\lambda\vec{v}$

の等式が成り立つものを言います。

### 固有方程式

ある $\lambda\in{}R$ が与えられたとき、それが $A$ の固有値かを判定するには

$det\left(A-\lambda{}I\right)=0$

を計算します。この式は固有方程式や特性方程式と呼ばれます。

## 実装

以上を実装してみます。

今回はあくまで行列について考えますが、前準備として環と体について軽く触れておきます。

まず環というのは簡単に言えば足し算と掛け算 (と引き算) ができる体系です。
体というのはそれに加えて割り算ができます。

プログラムとして実装するとき、行列の成分は float や double で決め打ちでもよいのですが、
ほかの型も使えるようになっているとあとあと都合がよいのでそうしてみます。

というわけでまず環 (ring) と体 (field) を定義します。

### signature RING

```
(* signature RING                                                             *)
(*                                                                            *)
(* ADDITIVE LAWS                                                              *)
(*                                                                            *)
(*   (a + b) + c = a + (b + c)                                                *)
(*   a + b       = b + a                                                      *)
(*   zero + a    = a                                                          *)
(*   a + zero    = a                                                          *)
(*   a + ~a      = zero                                                       *)
(*                                                                            *)
(* MULTIPLICATIVE LAWS                                                        *)
(*                                                                            *)
(*   (a * b) * c = a * (b * c)                                                *)
(*   one * a     = a                                                          *)
(*   a * one     = a                                                          *)
(*                                                                            *)
(* DISTRIBUTIVE LAWS                                                          *)
(*                                                                            *)
(*   a * (b + c) = a * b + a * c                                              *)
(*   (a + b) * c = a * c + b * c                                              *)

signature RING = sig
  type t
  val zero : t
  val one : t
  val + : t * t -> t
  val * : t * t -> t
  val ~ : t -> t
end
```

### signature FIELD

```
signature FIELD = sig
  include RING
  val inverse : t -> t option
end
```

ML の型システム (だけでなくおよそほとんどの言語の型システム)
では多くの公理を表現できませんのでコメントに記述しています。

ちなみに `a : t` かつ `b : t` ならば `a + b : t` という性質も
公理と考えることがありますがこれは型システムで表現できますので省略しています。

(なおこの性質がないと `a + b + c` とどんどん再帰的に組み合わせていけないので、
いろいろ組み合わせてなにかをつくるために重要な性質)

## signature MATRIX and signature FIELD_MATRIX

環のみを仮定した `MATRIX` と体 (乗法逆元) を仮定しないと実装できない
`FIELD_MATRIX` を分けてみます。

### signature MATRIX

```
signature MATRIX = sig
  exception Dimension

  type scalar
  type matrix

(* PRIMITIVE OPERATIONS                                                       *)

  val new : int * int * (int * int -> scalar) -> matrix

  val width : matrix -> int
  val height : matrix -> int

  val element : int * int * matrix -> scalar option
  val row : int * matrix -> scalar vector option
  val column : int * matrix -> scalar vector option

(* BASIC OPERATIONS                                                           *)
(*                                                                            *)
(*   A + B raises Dimension if width A <> width B or height A <> height B.    *)
(*   A - B raises Dimension if width A <> width B or height A <> height B.    *)
(*   A * B raises Dimension if width A <> height B.                           *)

  val submatrix : int * int * matrix -> matrix
  val transpose : matrix -> matrix
  val scale : scalar * matrix -> matrix

  val + : matrix * matrix -> matrix
  val - : matrix * matrix -> matrix
  val * : matrix * matrix -> matrix

(* SQUARE MATRIX OPERATIONS                                                   *)
(*                                                                            *)
(*   eigenequation(a, A) returns R.zero if a is an eigenvalue of A.           *)
(*                                                                            *)
(*   determinant A raises Dimension if not (isSquareMatrix A).                *)
(*   adjugate A raises Dimension if not (isSquareMatrix A).                   *)
(*   eigenequation(a, A) raises Dimension if not (isSquareMatrix A).          *)

  val isSquareMatrix : matrix -> bool

  val identityMatrix : int -> matrix

  val determinant : matrix -> scalar
  val adjugate : matrix -> matrix
  val eigenequation : scalar * matrix -> scalar

(* PROCEDURAL OPERATIONS                                                      *)

  val appiRow : matrix * (int * scalar vector -> unit) -> unit
  val appiCol : matrix * (int * scalar vector -> unit) -> unit
  val appRow : matrix * (scalar vector -> unit) -> unit
  val appCol : matrix * (scalar vector -> unit) -> unit

(* CONSTRUCTORS                                                               *)
(*                                                                            *)
(* fromRows rows raises Dimension if length of some rows is different.        *)

  val fromRows : scalar list list -> matrix
end
```

### signature FIELD_MATRIX

```
signature FIELD_MATRIX = sig
  include MATRIX

(* REGULAR MATRIX OPERATIONS                                                  *)
(*                                                                            *)
(*   inverse A raises Dimension if not (isRegularMatrix A).                   *)

  val isRegularMatrix : matrix -> bool

  val inverse : matrix -> matrix
end
```

###  functor Matrix

公理系を仮定したコードは `functor` で書くとすっきり書けたりします
(型クラスや trait でもいいけど)


```
functor Matrix(R : RING) :> MATRIX where type scalar = R.t = struct
  exception Dimension

  type scalar = R.t

  abstype matrix = MATRIX of {
    width : int,
    height : int,
    elements : scalar vector
  } with
    fun new(width, height, f) = MATRIX {
      width =
        width,
      height =
        height,
      elements =
        Vector.tabulate(width * height, fn i => let
          val (x, y) = (i mod width, i div width)
        in
          f(x, y)
        end)
    }

    fun width(MATRIX {width, ...}) =
      width

    fun height(MATRIX {height, ...}) =
      height

    fun element(x, y, MATRIX {width, height, elements}) = let
      val i = y * width + x
    in
      if not (0 <= i andalso i < width * height) then
        NONE
      else
        SOME(Vector.sub(elements, i))
    end

    fun column(x, MATRIX {width, height, elements}) =
      if not (0 <= x andalso x < width) then
        NONE
      else let
        val v = Vector.tabulate(height, fn i =>
          Vector.sub(elements, i * width + x))
      in
        SOME v
      end

    fun row(y, MATRIX {width, height, elements}) =
      if not (0 <= y andalso y < height) then
        NONE
      else let
        val v = Vector.tabulate(width, fn i =>
          Vector.sub(elements, y * width + i))
      in
        SOME v
      end
  end

  local
    fun sub(x, y, A) =
      case element(x, y, A) of
        NONE =>
          raise Subscript
      | SOME a =>
          a

    fun subColumn(x, A) =
      case column(x, A) of
        NONE =>
          raise Subscript
      | SOME v =>
          v

    fun subRow(y, A) =
      case row(y, A) of
        NONE =>
          raise Subscript
      | SOME v =>
          v
  in
    fun submatrix(x, y, A) =
      if width A = 0 orelse height A = 0 then
        A
      else if not (0 <= x andalso x < width A) then
        A
      else if not (0 <= y andalso y < height A) then
        A
      else
        new(width A - 1, height A - 1, fn (i, j) =>
          if i < x andalso j < y then
            sub(i, j, A)
          else if i < x then
            sub(i, j + 1, A)
          else if j < y then
            sub(i + 1, j, A)
          else
            sub(i + 1, j + 1, A))

    fun transpose A =
      new(height A, width A, fn (i, j) => sub(j, i, A))

    fun scale(a, A) =
      new(width A, height A, fn (i, j) => R.*(a, sub(i, j, A)))

    fun A + B =
      if not (width A = width B andalso height A = height B) then
        raise Dimension
      else let
        val n = width A (* = width B *)
        val m = height A (* = height B *)
      in
        new(n, m, fn (i, j) => R.+(sub(i, j, A), sub(i, j, B)))
      end

    fun A - B =
      if not (width A = width B andalso height A = height B) then
        raise Dimension
      else let
        val n = width A (* = width B *)
        val m = height A (* = height B *)
      in
        new(n, m, fn (i, j) => R.+(sub(i, j, A), R.~(sub(i, j, B))))
      end

    fun A * B =
      if not (width A = height B) then
        raise Dimension
      else let
        val n = width A (* = height B *)
      in
        new(width B, height A, fn (i, j) => let
          val (v, u) = (subRow(j, A), subColumn(i, B))
          val l = List.tabulate(n, fn k =>
            R.*(Vector.sub(v, k), Vector.sub(u, k)))
        in
          List.foldr R.+ R.zero l
        end)
      end

    fun isSquareMatrix A =
      width A = height A

    fun identityMatrix n =
      new(n, n, fn (i, j) => if i = j then R.one else R.zero)

    fun determinant A =
      if not (isSquareMatrix A) then
        raise Dimension
      else let
        val n = width A (* = height A *)
      in
        if n = 0 then
          R.zero
        else if n = 1 then
          sub(0, 0, A)
        else let
          val i = ref 0
          val a = ref R.zero
        in
          while !i < n do (
            if !i mod 2 = 0 then
              a := R.+(!a, R.*(sub(0, !i, A), determinant(submatrix(0, !i, A))))
            else
              a := R.+(!a, R.~(R.*(sub(0, !i, A), determinant(submatrix(0, !i, A)))));
            i := Int.+(!i, 1));
          !a
        end
      end

    fun adjugate A =
      if not (isSquareMatrix A) then
        raise Dimension
      else
        new(height A, width A, fn (i, j) =>
          if Int.+(i, j) mod 2 = 0 then
            determinant(submatrix(j, i, A))
          else
            R.~(determinant(submatrix(j, i, A))))

    fun eigenequation(a, A) =
      if not (isSquareMatrix A) then
        raise Dimension
      else let
        val n = width A (* = height A *)
        val I = identityMatrix n
      in
        determinant(A - scale(a, I))
      end

    fun appiRow(A, f) = let
      val i = ref 0
    in
      while !i < height A do (
        f(!i, subRow(!i, A));
        i := Int.+(!i, 1))
    end

    fun appiCol(A, f) = let
      val i = ref 0
    in
      while !i < width A do (
        f(!i, subColumn(!i, A));
        i := Int.+(!i, 1))
    end

    fun appRow(A, f) =
      appiRow(A, fn (_, v) => f v)

    fun appCol(A, f) =
      appiCol(A, fn (_, v) => f v)

    fun fromRows rows =
      case rows of
        nil =>
          new(0, 0, fn _ => R.zero)
      | row :: _ => let
          val width = List.length row
          val height = List.length rows
          val rows = Vector.fromList (List.map Vector.fromList rows)
        in
          if Vector.exists (fn row => Vector.length row <> width) rows then
            raise Dimension
          else
            new(width, height, fn (i, j) =>
              Vector.sub(Vector.sub(rows, j), i))
        end
  end
end
```

### functor FieldMatrix

乗法逆元が必要なコード。

```
functor FieldMatrix(F : FIELD) :> FIELD_MATRIX where type scalar = F.t = struct
  local structure M = Matrix(F) in
    open M

    fun isRegularMatrix A =
      if not (isSquareMatrix A) then
        false
      else
        Option.isSome (F.inverse (determinant A))

    fun inverse A =
      case F.inverse (determinant A) of
        NONE =>
          raise Dimension
      | SOME a =>
          scale (a, adjugate A)
  end
end
```

### instances

最後に instance を定義

```
structure IntRing : RING = struct
  type t = int
  val zero = 0
  val one = 1
  val op + = Int.+
  val op * = Int.*
  val ~ = Int.~
end

structure RealField : FIELD = struct
  type t = real
  val zero = 0.0
  val one = 1.0
  val op + = Real.+
  val op * = Real.*
  val ~ = Real.~

  fun inverse q =
    if Real.==(q, 0.0) then
      NONE
    else
      SOME (1.0 / q)
end

structure IntMatrix = Matrix(IntRing)
structure RealMatrix = FieldMatrix(RealField)
```

## 動かしてみる

実際に対角化できる正方行列を対角化してみる。

```
fun printMatrix A =
  RealMatrix.appRow(A, fn row =>
    let
      val s =
        String.concatWith "\t"
          (Vector.foldr (op ::) nil (Vector.map Real.toString row)) ^ "\n"
    in
      print s
    end)

val () = let
  open RealMatrix

  val A = RealMatrix.fromRows [
    [1.0, 2.0, 0.0],
    [0.0, 3.0, 0.0],
    [2.0, ~4.0, 2.0]
  ]

  val P = RealMatrix.fromRows [
    [~1.0, 0.0, ~1.0],
    [~1.0, 0.0, 0.0],
    [2.0, 1.0, 2.0]
  ]

  val P' = inverse P

  val hr = String.concat (List.tabulate(80, fn _ => "=")) ^ "\n"
in
  print hr;
  print "A =\n";
  print hr;
  printMatrix A;
  print hr;
  print "P =\n";
  print hr;
  printMatrix P;
  print hr;
  print "P^{-1}AP =\n";
  print hr;
  printMatrix (P' * A * P);
  print hr
end
```

するとこんな感じに動く。

```
$ mlton examples/diagonalize.mlb
$ examples/diagonalize
================================================================================
A =
================================================================================
1       2       0
0       3       0
2       ~4      2
================================================================================
P =
================================================================================
~1      0       ~1
~1      0       0
2       1       2
================================================================================
P^{-1}AP =
================================================================================
3       0       0
0       2       0
0       0       1
================================================================================
```

なんとなくよさそう！

## Appendix A. Full Code Listing

- [Matrix](/misc/matrix)
