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
