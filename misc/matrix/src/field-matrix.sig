signature FIELD_MATRIX = sig
  include MATRIX

(* REGULAR MATRIX OPERATIONS                                                  *)
(*                                                                            *)
(*   inverse A raises Dimension if not (isRegularMatrix A).                   *)

  val isRegularMatrix : matrix -> bool

  val inverse : matrix -> matrix
end
