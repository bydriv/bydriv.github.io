---
url: https://bydriv.github.io/misc/matrix/
title: bydriv.github.io
description: Matrix
thumbnail: https://bydriv.github.io/etc/site/thumbnail.png
---

# Matrix

行列演算ライブラリ。

## Full Code Listing

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
          R.one
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

### structure IntRing

```
structure IntRing : RING = struct
  type t = int
  val zero = 0
  val one = 1
  val op + = Int.+
  val op * = Int.*
  val ~ = Int.~
end
```

### structure RealField


```
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
```

### structure IntMatrix

```
structure IntMatrix = Matrix(IntRing)
```

### structure RealMatrix

```
structure RealMatrix = FieldMatrix(RealField)
```
