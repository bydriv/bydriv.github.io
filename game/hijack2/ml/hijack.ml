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

module Teiri : Object with type param = int * int = struct
  type pose =
    Walk
  | Hijack

  type direction =
    Left
  | Back
  | Right
  | Front

  type t = {
    i : int;
    x : int;
    y : int;
    pose : pose;
    direction : direction
  }

  type param = int * int

  let intro(x, y) = Js.Promise.resolve {
    i = 0;
    x = x;
    y = y;
    pose = Walk;
    direction = Front
  }

  let step inputs teiri =
    let xshift =
      if Array.length inputs = 0 then
        0
      else if inputs.(0)##x < -0.25 then
        -1
      else if inputs.(0)##x > 0.25 then
        1
      else 0 in
    let yshift =
      if Array.length inputs = 0 then
        0
      else if inputs.(0)##y < -0.25 then
        -1
      else if inputs.(0)##y > 0.25 then
        1
      else
        0
    in
      Js.Promise.resolve {
        i = teiri.i + 1;
        x = teiri.x + xshift;
        y = teiri.y + yshift;

        pose =
          if Array.length inputs = 0 then
            Walk
          else if (inputs.(0)##buttons).(4) then
            Hijack
          else
            Walk;

        direction =
          if yshift < 0 then
            Back
          else if yshift > 0 then
            Front
          else if xshift < 0 then
            Left
          else if xshift > 0 then
            Right
          else
            teiri.direction
      }

  let x teiri =
    match teiri.pose with
      Walk -> teiri.x
    | Hijack -> teiri.x - 8

  let y teiri =
    match teiri.pose with
      Walk -> teiri.y
    | Hijack -> teiri.y - 8

  let string_of_pose = function
    Walk -> "walk"
  | Hijack -> "hijack"

  let string_of_direction = function
    Left -> "left"
  | Back -> "back"
  | Right -> "right"
  | Front -> "front"

  let views teiri =
    let views = [|
      `Image (
        x teiri,
        y teiri,
        "pixelart/teiri/" ^
          string_of_pose teiri.pose ^
          "/" ^
          string_of_direction teiri.direction ^
          "/" ^
          string_of_int (teiri.i / 8 mod 4) ^
          ".png"
      )
    |] in
      Js.Promise.resolve views
end

type object_t =
  Teiri of Teiri.t

type object_param =
  TeiriParam of Teiri.param

module Object : Object with type t = object_t and type param = object_param = struct
  type t = object_t
  type param = object_param

  let intro = function
    TeiriParam param ->
      Js.Promise.then_
        (fun teiri -> Js.Promise.resolve (Teiri teiri))
        (Teiri.intro param)

  let step inputs = function
    Teiri teiri ->
      Js.Promise.then_
        (fun teiri -> Js.Promise.resolve (Teiri teiri))
        (Teiri.step inputs teiri)

  let views = function
    Teiri teiri -> Teiri.views teiri
end

module Map : sig
  include Object

  val test : param
end = struct
  type t = {
    objects : Object.t array
  }

  type param = {
    params : Object.param array
  }

  let intro param =
    Js.Promise.then_
      (fun objects ->
        Js.Promise.resolve {
          objects = objects
        })
      (Js.Promise.all
        (Array.map
          (fun param -> Object.intro param)
          param.params))

  let step inputs map =
    Js.Promise.then_
      (fun objects -> Js.Promise.resolve {
        objects = objects
      })
     (Js.Promise.all
       (Array.map
         (fun obj -> Object.step inputs obj)
          map.objects))

  let views map =
    Js.Promise.then_
      (fun views -> Js.Promise.resolve (Array.concat (Array.to_list views)))
      (Js.Promise.all (Array.map (fun obj -> Object.views obj) map.objects))

  let test = {
    params = [|
      TeiriParam (32, 32)
    |]
  }
end

(******************************************************************************)
(*                            GAME  AS  AN  OBJECT                            *)
(******************************************************************************)

type t = Map.t
type param = unit

let intro() = Map.intro Map.test
let step = Map.step
let views = Map.views

(******************************************************************************)
(*                            GAME  CONFIGURATIONS                            *)
(******************************************************************************)

let scale = 2
let width = 640 / scale
let height = 480 / scale

let assets = [|
  ("pixelart/teiri/walk.png", [|
    ("pixelart/teiri/walk/left/0.png", ((0, 0, 16, 16)));
    ("pixelart/teiri/walk/left/1.png", ((16, 0, 16, 16)));
    ("pixelart/teiri/walk/left/2.png", ((32, 0, 16, 16)));
    ("pixelart/teiri/walk/left/3.png", ((48, 0, 16, 16)));
    ("pixelart/teiri/walk/back/0.png", ((0, 16, 16, 16)));
    ("pixelart/teiri/walk/back/1.png", ((16, 16, 16, 16)));
    ("pixelart/teiri/walk/back/2.png", ((32, 16, 16, 16)));
    ("pixelart/teiri/walk/back/3.png", ((48, 16, 16, 16)));
    ("pixelart/teiri/walk/right/0.png", ((0, 32, 16, 16)));
    ("pixelart/teiri/walk/right/1.png", ((16, 32, 16, 16)));
    ("pixelart/teiri/walk/right/2.png", ((32, 32, 16, 16)));
    ("pixelart/teiri/walk/right/3.png", ((48, 32, 16, 16)));
    ("pixelart/teiri/walk/front/0.png", ((0, 48, 16, 16)));
    ("pixelart/teiri/walk/front/1.png", ((16, 48, 16, 16)));
    ("pixelart/teiri/walk/front/2.png", ((32, 48, 16, 16)));
    ("pixelart/teiri/walk/front/3.png", ((48, 48, 16, 16)))
  |]);
  ("pixelart/teiri/hijack.png", [|
    ("pixelart/teiri/hijack/left/0.png", ((0, 0, 32, 32)));
    ("pixelart/teiri/hijack/left/1.png", ((32, 0, 32, 32)));
    ("pixelart/teiri/hijack/left/2.png", ((64, 0, 32, 32)));
    ("pixelart/teiri/hijack/left/3.png", ((96, 0, 32, 32)));
    ("pixelart/teiri/hijack/back/0.png", ((0, 32, 32, 32)));
    ("pixelart/teiri/hijack/back/1.png", ((32, 32, 32, 32)));
    ("pixelart/teiri/hijack/back/2.png", ((64, 32, 32, 32)));
    ("pixelart/teiri/hijack/back/3.png", ((96, 32, 32, 32)));
    ("pixelart/teiri/hijack/right/0.png", ((0, 64, 32, 32)));
    ("pixelart/teiri/hijack/right/1.png", ((32, 64, 32, 32)));
    ("pixelart/teiri/hijack/right/2.png", ((64, 64, 32, 32)));
    ("pixelart/teiri/hijack/right/3.png", ((96, 64, 32, 32)));
    ("pixelart/teiri/hijack/front/0.png", ((0, 96, 32, 32)));
    ("pixelart/teiri/hijack/front/1.png", ((32, 96, 32, 32)));
    ("pixelart/teiri/hijack/front/2.png", ((64, 96, 32, 32)));
    ("pixelart/teiri/hijack/front/3.png", ((96, 96, 32, 32)))
  |])
|]
