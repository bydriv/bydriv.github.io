## チューリングマシン

Practical なプログラミング言語はいろいろやってきたけど、そういえば計算機科学的な素養があまりないな、とふと思ったので、
基礎（？）に立ちもどって、チューリングマシンの勉強をしてみる。

### 形式的定義

$$
M = (Q, \Gamma, a_0, \Sigma, \delta, q_0, H)
$$

ただし

|                                                                        |                                                         |
|------------------------------------------------------------------------|---------------------------------------------------------|
| $Q$                                                                    | **状態** (*state*) の集合                               |
| $\Gamma$                                                               | **記号** (*symbol*) の集合                              |
| $a_0 \in \Gamma$                                                       | **空白記号** (*blank symbol*)                           |
| $\Sigma \subseteq \Gamma \setminus \{a_0\}$                            | **入力記号** (*input symbol*) の集合                    |
| $\delta : Q \times \Gamma \rightarrow Q \times \Gamma \times \{L, R\}$ | **遷移関数** (*transition function*) と呼ばれる部分関数 |
| $q_0 \in Q$                                                            | **初期状態** (*initial state*)                          |
| $H \subseteq Q$                                                        | **受理状態** (*accept state*) の集合                    |

$n = |Q|$, $m = |\Gamma|$ として、 **$n$状態$m$記号チューリングマシン** (*$n$-state $m$-symbol Turing machine*) とか、 **($n$,$m$)チューリングマシン** (*($n$,$m$) Turing Machine*) とかいう。

$a_1,\cdots,a_n,b_1,\cdots,b_n$ を記号とし、 $q_i$ を状態とするとき、 $a_1\cdots a_nq_ib_1\cdots b_n$ を M の様相という。

**例**: 様相 $a_1\cdots a_nq_ib_1\cdots b_n$ で $\delta(q_i,b_1)=(q_{i+1},a_{n+1},R)$ ならば、そのつぎの様相は
$a_1\cdots a_n,a_{n+1}q_ib_2\cdots b_n$ となる。 $\delta$ は $q_i$ のすぐ右隣 (この場合は $b_1$) を第2引数としてとって、
2番めの戻り値の (この場合は $a_{n+1}$) で置きかえる。

- 様相 $a_1\cdots a_nq_ib_1\cdots b_n$ のとき、 $q_i\in H$ ならば **受理** (*accept*)
- $\delta(q_i,b_1)$ が定義されていない場合は **拒否** (*reject*)

$a_0$ を空白記号、 $a_1,\cdots,a_n$ を入力記号、 $q_0$ を初期状態とするとき、
TM は様相 $\cdots a_0a_0q_0a_1\cdots a_na_0a_0\cdots$ から開始して、受理または拒否して停止するまで $\delta$ に従った動作を続ける。

### チューリングマシンとは

チューリングマシンは、プログラマの言葉でいうところのプログラムにあたる。

*Note*: チューリング完全でないプログラムがあるように(例: `fn () => 42` のような定数関数)、
チューリング完全でないチューリングマシンもある。

### 万能チューリングマシンとは

万能チューリングマシンはチューリングマシンの1種であり、チューリング完全なチューリングマシンのこと。

プログラマの言葉でいえば、チューリング完全なプログラムのことを万能チューリングマシンという。

**例**: (チューリング完全な言語の) インタプリタ

### 複雑性クラスとか

言語 $L$ を文字列の集合とすると、(ある具体的なチューリングマシンが) $L$ に属する文字列をすべて受理するとき、
(そのある具体的なチューリングマシンは) $L$ を **認識** (*recognize*) するという。
言語クラス $\mathcal{L}$ は言語の集合である。
すべての言語 $L \in \mathcal{L}$ について、 $L$ を認識する具体的なチューリングマシンが存在するとき、
$\mathcal{L}$ を (一般的な意味での) チューリングマシンが認識する言語のクラスとかいう。

- 帰納的可算言語 (**RE**): チューリングマシンが受理して停止するか、無限ループする言語。 チューリングマシンはすべての帰納的可算言語を認識できる
  (チューリングマシンが認識する言語のクラス)。
- 帰納言語 (**R**): チューリングマシンが受理または拒否して停止する言語。

### 計算可能関数

数学の関数をコンピュータで実装しようとなると、どうしたってアルゴリズムが必要になる。
そこでアルゴリズムの存在する関数を計算可能関数と呼ぶことにしよう、というのがチャーチ＝チューリングのテーゼ。

ではアルゴリズムとはなにか？
というと、チューリングマシンで定義することができて、かつ、定義域の入力に対しては常に停止し、正しい結果を返すもの
(たとえば素数を引数にとる関数に素数でない数を渡した場合、停止しないかもしれないし、停止しても結果が正しいとは限らないが、
素数を渡した場合は常に停止するし、結果もやはり正しいもの)。

もうちょっと厳密な定義もあるらしいんだけど、理解するのも説明するのも難しいので、まあ大雑把にはそんな感じだと思う。

このへんは興味があるんだけど、まだあんまり理解できていない……。

### 多テープチューリングマシンとかRAMチューリングマシン

テープの数をひとつではなくふたつにしたり、ランダムアクセスできるようにしたり、
非決定計算をできるようにチューリングマシンを拡張することもできる。
ただし、このように拡張しても等価であることが示されている。
もちろん現実にはランダムアクセスできたほうが計算効率がよいとかはあるだろうが、計算可能性という意味では等価である、という意味。

### チューリングマシンの参考実装

```sml
signature TURING_MACHINE = sig
  datatype shift = L | R
  eqtype state
  eqtype symbol
  eqtype input_symbol
  val blank : symbol
  val transition : state * symbol -> (state * symbol * shift) option
  val initialState : state
  val acceptStates : state list

  (* utility functions *)
  val symbol : input_symbol -> symbol
end

signature TM = sig
  include TURING_MACHINE
  val accept : input_symbol list -> bool
end

functor TM(M: TURING_MACHINE) : TM = struct
  open M

  val CHUNK_SIZE = 256

  fun extend tape i = let
  in
    if !i - 1 < 0 then (
      tape := Array.tabulate(Array.length (!tape) + CHUNK_SIZE, fn j =>
        if j < CHUNK_SIZE then
          M.blank
        else
          Array.sub(!tape, j - CHUNK_SIZE))
    ; i := !i + CHUNK_SIZE
    ) else
      ()
  ; if !i + 1 >= Array.length (!tape) then (
      tape := Array.tabulate(Array.length (!tape) + CHUNK_SIZE, fn j =>
        if j < Array.length (!tape) then
          Array.sub(!tape, j)
        else
          M.blank)
    ) else
      ()
  end

  fun accept(input) = let
    val tape = ref (Array.fromList (map M.symbol input))
    val q = ref M.initialState
    val i = ref 0
    val reject = ref false
  in
    while not (!reject) andalso not (List.exists (fn q' => !q = q') M.acceptStates) do let
      val () = extend tape i
      val a = Array.sub(!tape, !i)
    in
      case M.transition(!q, a) of
        NONE =>
          reject := true
      | SOME (q', a', m) => let
        val offset = case m of M.L => ~1 | M.R => 1
      in
        Array.update(!tape, !i, a')
      ; q := q'
      ; i := !i + offset
      end
    end
  ; not (!reject)
  end
end
```

### チューリングマシンの例

[https://en.wikipedia.org/wiki/Turing_machine_examples](https://en.wikipedia.org/wiki/Turing_machine_examples) にいろいろある。

#### ウルフラムの2状態3記号チューリングマシン

チューリング完全性が証明されている、かなりシンプルなチューリングマシン。

参考: [https://en.wikipedia.org/wiki/Wolfram%27s_2-state_3-symbol_Turing_machine](https://en.wikipedia.org/wiki/Wolfram%27s_2-state_3-symbol_Turing_machine)

$$
M = (\{q_0, q_1\}, \{a_0,a_1,a_2\}, a_0, \{a_1, a_2\}, \delta, q_0, \emptyset)
$$

遷移関数 $\delta$ は状態遷移表で書くことが多いようだ。

|       |               |               |
|-------|---------------|---------------|
|       | $q_0$         | $q_1$         |
| $a_0$ | $(q_1,a_1,R)$ | $(q_0,a_2,L)$ |
| $a_1$ | $(q_0,a_2,L)$ | $(q_1,a_2,R)$ |
| $a_2$ | $(q_0,a_1,L)$ | $(q_0,a_0,R)$ |

あまり見ないが、プログラマには関数表記のほうがわかりやすいかもしれない。

|                                                                        |
|------------------------------------------------------------------------|
| $\delta : Q \times \Gamma \rightarrow Q \times \Gamma \times \{L, R\}$ |
| $\delta(q_0, a_0) = (q_1,a_1,R)$                                       |
| $\delta(q_0, a_1) = (q_0,a_2,L)$                                       |
| $\delta(q_0, a_2) = (q_0,a_1,L)$                                       |
| $\delta(q_1, a_0) = (q_0,a_2,L)$                                       |
| $\delta(q_1, a_1) = (q_1,a_2,R)$                                       |
| $\delta(q_1, a_2) = (q_0,a_0,R)$                                       |

##### 参考実装

```sml
signature WOLFRAM_2_3 = sig
  include TM
  val A1 : input_symbol
  val A2 : input_symbol
end

local
  structure Wolfram_2_3 = struct
    datatype shift = L | R
    datatype state = Q0 | Q1
    datatype symbol = A0 | A1' | A2'
    datatype input_symbol = A1 | A2

    val blank = A0

    fun transition(Q0, A0) = SOME (Q1, A1', R)
      | transition(Q0, A1') = SOME (Q0, A2', L)
      | transition(Q0, A2') = SOME (Q0, A1', L)
      | transition(Q1, A0) = SOME (Q0, A2', L)
      | transition(Q1, A1') = SOME (Q1, A2', R)
      | transition(Q1, A2') = SOME (Q0, A0, R)

    val initialState = Q0

    val acceptStates = nil

    (* utility functions *)
    fun symbol A1 = A1'
      | symbol A2 = A2'
  end

  structure TMWolfram_2_3 = TM(Wolfram_2_3)
in
  structure Wolfram_2_3 :> WOLFRAM_2_3 = struct
    open Wolfram_2_3
    val accept = TMWolfram_2_3.accept
  end
end
```

#### 0 を $2^n$ 個並べた文字列を受理するチューリングマシン

チューリングマシンのテキストでは、なんでかわからないけど最初にこれを例示することが多いらしい。
なのでここにも書いておく。

たとえば "0" (0 を $2^0$ 個並べた文字列) や "00" (0 を $2^1$ 個並べた文字列)、
"0000" (0 を $2^2$ 個並べた文字列) や "00000000" (0 を $2^3$ 個並べた文字列) だったら受理して停止し、
そうでなければ拒否して停止するかもしくは停止しないチューリングマシンを考える。

$$
M = (\{q_0, q_1, q_2, q_3, q_4, q_5, q_6\}, \{a_0, a_1, 0\}, a_0, \{0\}, \delta, q_0, \{q_1\})
$$

|       | start         | accept | reject |               |               |               |               |
|-------|---------------|--------|--------|---------------|---------------|---------------|---------------|
|       | $q_0$         | $q_1$  | $q_2$  | $q_3$         | $q_4$         | $q_5$         | $q_6$         |
| $a_0$ | $(q_2,a_0,R)$ |        |        | $(q_1,a_0,R)$ | $(q_6,a_0,L)$ | $(q_2,a_0,R)$ | $(q_3,a_0,R)$ |
| $a_1$ | $(q_2,a_1,R)$ |        |        | $(q_3,a_1,R)$ | $(q_4,a_1,R)$ | $(q_5,a_1,R)$ | $(q_6,a_1,L)$ |
| $0$   | $(q_3,a_0,R)$ |        |        | $(q_4,a_1,R)$ | $(q_5,0,R)$   | $(q_4,a_1,R)$ | $(q_6,0,L)$   |

##### 参考実装

```sml
signature EXAMPLE0 = sig
  include TM
  val O : input_symbol
  val parse : string -> input_symbol list option
end

local
  structure Example0 = struct
    datatype shift = L | R
    datatype state = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6
    datatype symbol = A0 | A1 | O'
    datatype input_symbol = O

    val blank = A0

    fun transition(Q0, A0) = SOME (Q2, A0, R)
      | transition(Q0, A1) = SOME (Q2, A1, R)
      | transition(Q0, O') = SOME (Q3, A0, R)
      | transition(Q1, _) = NONE
      | transition(Q2, _) = NONE
      | transition(Q3, A0) = SOME (Q1, A0, R)
      | transition(Q3, A1) = SOME (Q3, A1, R)
      | transition(Q3, O') = SOME (Q4, A1, R)
      | transition(Q4, A0) = SOME (Q6, A0, L)
      | transition(Q4, A1) = SOME (Q4, A1, R)
      | transition(Q4, O') = SOME (Q5, O', R)
      | transition(Q5, A0) = SOME (Q2, A0, R)
      | transition(Q5, A1) = SOME (Q5, A1, R)
      | transition(Q5, O') = SOME (Q4, A1, R)
      | transition(Q6, A0) = SOME (Q3, A0, R)
      | transition(Q6, A1) = SOME (Q6, A1, L)
      | transition(Q6, O') = SOME (Q6, O', L)

    val initialState = Q0

    val acceptStates = [Q1]

    (* utility functions *)
    fun symbol O = O'
  end

  structure TMExample0 = TM(Example0)
in
  structure Example0 :> EXAMPLE0 = struct
    open Example0
    val accept = TMExample0.accept

    fun parse s = let
      val zeros = String.explode s
    in
      if List.all (fn zero => zero = #"0") zeros then
        SOME (map (fn _ => O) zeros)
      else
        NONE
    end
  end
end

val () = let
  val SOME zero0 = Example0.parse ""
  val SOME zero1 = Example0.parse "0"
  val SOME zero2 = Example0.parse "00"
  val SOME zero3 = Example0.parse "000"
  val SOME zero4 = Example0.parse "0000"
  val SOME zero5 = Example0.parse "00000"
  val SOME zero6 = Example0.parse "000000"
  val SOME zero7 = Example0.parse "0000000"
  val SOME zero8 = Example0.parse "00000000"
in
  if Example0.accept zero0 then
    print "accept zero0\n"
  else
    print "reject zero0\n"
; if Example0.accept zero1 then
    print "accept zero1\n"
  else
    print "reject zero1\n"
; if Example0.accept zero2 then
    print "accept zero2\n"
  else
    print "reject zero2\n"
; if Example0.accept zero3 then
    print "accept zero3\n"
  else
    print "reject zero3\n"
; if Example0.accept zero4 then
    print "accept zero4\n"
  else
    print "reject zero4\n"
; if Example0.accept zero5 then
    print "accept zero5\n"
  else
    print "reject zero5\n"
; if Example0.accept zero6 then
    print "accept zero6\n"
  else
    print "reject zero6\n"
; if Example0.accept zero7 then
    print "accept zero7\n"
  else
    print "reject zero7\n"
; if Example0.accept zero8 then
    print "accept zero8\n"
  else
    print "reject zero8\n"
end
```

```
$ mlton turing-machine.sml
$ ./turing-machine
reject zero0
accept zero1
accept zero2
reject zero3
accept zero4
reject zero5
reject zero6
reject zero7
accept zero8
```
