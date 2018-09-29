let Point = {
  x : Integer,
  y : Integer
} in
let Playable = {
  type : Text
} in
let Wave = {
  type : Text,
  incr : Double,
  bias : Double
} in
let TypeA = Wave in
let Control = <
  playable : Playable
| wave : Wave
| typeA : TypeA
> in
let playable = \(playable : Playable) -> <playable = playable | wave : Wave | typeA : TypeA> in
let wave = \(wave : Wave) -> <wave = wave | playable : Playable | typeA : TypeA> in
let typeA = \(typeA : TypeA) -> <typeA = typeA | playable : Playable | wave : Wave> in
let Teiri = {
  type : Text,
  id : Optional Text,
  x : Integer,
  y : Integer,
  team : Text,
  control : Control,
  pose : Text,
  direction : Text
} in
let Gray = {
  type : Text,
  id : Optional Text,
  x : Integer,
  y : Integer,
  team : Text
} in
let Object = <
  teiri : Teiri
| gray : Gray
> in
let teiri = \(teiri : Teiri) -> <teiri = teiri | gray : Gray> in
let gray = \(gray : Gray) -> <gray = gray | teiri : Teiri> in {
  width = 240,
  height = 160,
  lock = "teiri",
  objects = [
    teiri {
      type = "teiri",
      id = ["teiri"] : Optional Text,
      x = +0,
      y = +0,
      team = "player",
      control = playable {
        type = "playable"
      },
      pose = "default",
      direction = "front"
    }
  ] # ./Prelude/List/map Point Object (\(p : Point) ->
    teiri {
      type = "security-drone",
      id = [] : Optional Text,
      x = p.x,
      y = p.y,
      team = "enemy",
      control = typeA {
        type = "typeA",
        incr = 0.0245,
        bias = 0.252
      },
      pose = "default",
      direction = "front"
    }
  ) [
    {x = +64, y = +48},
    {x = -64, y = +48},
    {x = +64, y = -48},
    {x = -64, y = -48},
    {x = +64, y = +32},
    {x = -64, y = +32},
    {x = +64, y = -32},
    {x = -64, y = -32},
    {x = +80, y = +48},
    {x = -80, y = +48},
    {x = +80, y = -48},
    {x = -80, y = -48},
    {x = +80, y = +32},
    {x = -80, y = +32},
    {x = +80, y = -32},
    {x = -80, y = -32}
  ] # ./Prelude/List/map Point Object (\(p : Point) ->
    gray {
      type = "stone-tile",
      id = [] : Optional Text,
      x = p.x,
      y = p.y,
      team = "neutral"
    }
  ) [
    {x = -96, y = -64},
    {x = -96, y = -48},
    {x = -96, y = -32},
    {x = -96, y = -16},
    {x = -96, y = +0},
    {x = -96, y = +16},
    {x = -96, y = +32},
    {x = -96, y = +48},
    {x = -96, y = +64},
    {x = +96, y = -64},
    {x = +96, y = -48},
    {x = +96, y = -32},
    {x = +96, y = -16},
    {x = +96, y = +0},
    {x = +96, y = +16},
    {x = +96, y = +32},
    {x = +96, y = +48},
    {x = +96, y = +64},
    {x = -80, y = -64},
    {x = -64, y = -64},
    {x = -48, y = -64},
    {x = -32, y = -64},
    {x = -16, y = -64},
    {x = +0, y = -64},
    {x = +80, y = -64},
    {x = +64, y = -64},
    {x = +48, y = -64},
    {x = +32, y = -64},
    {x = +16, y = -64},
    {x = -80, y = +64},
    {x = -64, y = +64},
    {x = -48, y = +64},
    {x = -32, y = +64},
    {x = -16, y = +64},
    {x = +0, y = +64},
    {x = +80, y = +64},
    {x = +64, y = +64},
    {x = +48, y = +64},
    {x = +32, y = +64},
    {x = +16, y = +64}
  ]
}
