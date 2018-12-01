type input = <
  x : float;
  y : float;
  buttons : bool array
> Js.t

type view = [
  `Image of int * int * string
]

module type Object = sig
  type t
  type param
  val intro : param -> t Js.Promise.t
  val step : input array -> t -> t Js.Promise.t
  val views : t -> view array Js.Promise.t
end

module Teiri : Object with type param = int * int

type object_t =
  Teiri of Teiri.t

type object_param =
  TeiriParam of Teiri.param

module Object : Object with type t = object_t and type param = object_param

module Map : sig
  include Object

  val test : param
end

(******************************************************************************)
(*                            GAME  AS  AN  OBJECT                            *)
(******************************************************************************)

include Object

(******************************************************************************)
(*                            GAME  CONFIGURATIONS                            *)
(******************************************************************************)

val scale : int
val width : int
val height : int

val assets : (string * (string * (int * int * int * int)) array) array
