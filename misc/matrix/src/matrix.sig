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
