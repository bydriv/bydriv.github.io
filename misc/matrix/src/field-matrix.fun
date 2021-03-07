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
