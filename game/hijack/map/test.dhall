let Point = {
  x : Integer,
  y : Integer
} in
let Player = {
  type : Text
} in
let Enemy = {
  type : Text
} in
let Team = <
  player : Player
| enemy : Enemy
> in
let player = \(player : Player) -> <player = player | enemy : Enemy> in
let enemy = \(enemy : Enemy) -> <enemy = enemy | player : Player> in
let Playable = {
  type : Text
} in
let Wave = {
  type : Text,
  incr : Double,
  bias : Double
} in
let Control = <
  playable : Playable
| wave : Wave
> in
let playable = \(playable : Playable) -> <playable = playable | wave : Wave> in
let wave = \(wave : Wave) -> <wave = wave | playable : Playable> in
let Teiri = {
  type : Text,
  id : Optional Text,
  x : Integer,
  y : Integer,
  team : Team,
  control : Control,
  pose : Text,
  direction : Text
} in
let Gray = {
  type : Text,
  id : Optional Text,
  x : Integer,
  y : Integer,
  team : Team
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
      team = player {
        type = "player"
      },
      control = playable {
        type = "playable"
      },
      pose = "walk",
      direction = "front"
    }
  ] # ./Prelude/List/map Point Object (\(p : Point) ->
    teiri {
      type = "teiri",
      id = [] : Optional Text,
      x = p.x,
      y = p.y,
      team = enemy {
        type = "enemy"
      },
      control = wave {
        type = "wave",
        incr = 0.0245,
        bias = 0.252
      },
      pose = "walk",
      direction = "front"
    }
  ) [
    {x = +60, y = +40},
    {x = -60, y = +40},
    {x = +60, y = -40},
    {x = -60, y = -40}
  ] # ./Prelude/List/map Point Object (\(p : Point) ->
    gray {
      type = "gray",
      id = [] : Optional Text,
      x = p.x,
      y = p.y,
      team = enemy {
        type = "enemy"
      }
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
