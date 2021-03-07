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
