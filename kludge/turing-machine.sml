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
